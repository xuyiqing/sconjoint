# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/dnn-train.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

# Bounded Gaussian trainer. Checkpoint selection is internal to each declared
# training bank; final refitting starts again from the original selected state.

.rum_dnn_train_clock <- function() unname(proc.time()["elapsed"])

.rum_dnn_train_guard <- function(deadline) {
  if (.rum_dnn_train_clock() >= deadline)
    stop(structure(list(message = "Declared elapsed-time budget exhausted.", call = NULL),
                   class = c("rum_dnn_budget", "error", "condition")))
}

.rum_dnn_train_pack <- function(state) {
  c(unlist(state$network$weights, use.names = FALSE),
    unlist(state$network$biases, use.names = FALSE), state$kappa,
    state$loading, log(state$omega))
}

.rum_dnn_train_unpack <- function(par, template) {
  result <- template
  cursor <- 0L
  for (j in seq_along(result$network$weights)) {
    count <- length(result$network$weights[[j]])
    result$network$weights[[j]][] <- par[cursor + seq_len(count)]
    cursor <- cursor + count
  }
  for (j in seq_along(result$network$biases)) {
    count <- length(result$network$biases[[j]])
    result$network$biases[[j]][] <- par[cursor + seq_len(count)]
    cursor <- cursor + count
  }
  result$kappa <- par[cursor + 1L]
  result$loading <- par[cursor + 1L + seq_len(result$network$p)]
  result$omega <- exp(tail(par, 1L))
  result
}

.rum_dnn_train_pack_gradient <- function(gradient, state) {
  c(unlist(gradient$weights, use.names = FALSE),
    unlist(gradient$biases, use.names = FALSE), gradient$kappa,
    gradient$loading, gradient$omega * state$omega)
}

.rum_dnn_adam_step <- function(par, gradient, m, v, iteration,
                               learning_rate, beta1, beta2, epsilon,
                               lower, upper) {
  m <- beta1 * m + (1 - beta1) * gradient
  v <- beta2 * v + (1 - beta2) * gradient^2
  proposal <- par - learning_rate * (m / (1 - beta1^iteration)) /
    (sqrt(v / (1 - beta2^iteration)) + epsilon)
  if (any(!is.finite(c(m, v, proposal)))) stop("Non-finite Adam update.")
  projected <- proposal < lower | proposal > upper
  list(par = pmax(lower, pmin(upper, proposal)), m = m, v = v,
       projected = projected)
}

.rum_dnn_train_control <- function(control, starts_count) {
  defaults <- list(max_epochs = 200L, validation_every = 5L, patience = 10L,
    min_delta = 0, learning_rate = .01, beta1 = .9, beta2 = .999,
    epsilon = 1e-8, network_l2 = 1e-4, max_seconds = 480,
    start_seconds = 90, refit_seconds = 150, evaluation_reserve = 60,
    train_order = 15L, check_orders = c(15L, 21L), block_size = 64L,
    kappa_bounds = c(-5, 5), loading_bounds = c(-3, 3),
    omega_bounds = c(1e-6, 4), loglik_tolerance = 1e-5,
    score_tolerance = 1e-4, network_gradient_tolerance = 1e-4)
  if (!is.list(control) || (length(control) &&
      (is.null(names(control)) || any(!nzchar(names(control))) ||
       anyDuplicated(names(control)) || any(!names(control) %in% names(defaults)))))
    stop("control must be a named list of supported, nonduplicated settings.")
  defaults[names(control)] <- control
  x <- defaults
  for (nm in c("max_epochs", "validation_every", "block_size", "train_order"))
    x[[nm]] <- .rum_dnn_integer(x[[nm]], nm)
  if (!(is.numeric(x$patience) && length(x$patience) == 1L &&
        !is.na(x$patience) && x$patience >= 1 &&
        (is.infinite(x$patience) || x$patience == floor(x$patience))))
    stop("patience must be a positive integer or Inf.")
  positive <- c("learning_rate", "epsilon", "max_seconds")
  nonnegative <- c("min_delta", "network_l2", "start_seconds", "refit_seconds",
    "evaluation_reserve", "loglik_tolerance", "score_tolerance", "network_gradient_tolerance")
  for (nm in c(positive, nonnegative)) {
    value <- x[[nm]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 0 || (nm %in% positive && value == 0))
      stop("Invalid nonnegative/positive control: ", nm, ".")
  }
  for (nm in c("beta1", "beta2"))
    if (!is.numeric(x[[nm]]) || length(x[[nm]]) != 1L || !is.finite(x[[nm]]) ||
        x[[nm]] < 0 || x[[nm]] >= 1) stop(nm, " must be in [0,1).")
  for (nm in c("kappa_bounds", "loading_bounds", "omega_bounds")) {
    value <- x[[nm]]
    if (!is.numeric(value) || length(value) != 2L || any(!is.finite(value)) ||
        value[1L] >= value[2L]) stop(nm, " must be two increasing finite numbers.")
  }
  if (x$omega_bounds[1L] <= 0) stop("The log-omega trainer requires positive omega bounds.")
  orders <- c(x$train_order, x$check_orders)
  if (!is.numeric(x$check_orders) || !length(x$check_orders) ||
      any(!is.finite(orders)) || any(orders != floor(orders)) ||
      any(orders < 2 | orders > 80) || any(x$check_orders < x$train_order) ||
      length(unique(orders)) < 2L)
    stop("Use increasing Gaussian orders in 2:80, including a finer check order.")
  x$check_orders <- sort(unique(as.integer(x$check_orders)))
  if (x$evaluation_reserve + x$refit_seconds > x$max_seconds)
    stop("The refit and evaluation reserves exceed the total time budget.")
  x
}

.rum_dnn_train_prepare <- function(data, Z, block_size) {
  blocks <- split(seq_len(data$n), ceiling(seq_len(data$n) / block_size))
  list(n = data$n, ids = data$ids, blocks = lapply(blocks, function(ii)
    list(data = rum_subset(data, ii), Z = Z[ii, , drop = FALSE])))
}

.rum_dnn_train_eval <- function(state, prepared, order, deadline,
                                retain = FALSE, need_gradient = TRUE) {
  rule <- rum_quadrature(order, policy_law = "normal")
  loss <- 0
  gradient <- if (need_gradient) numeric(length(.rum_dnn_train_pack(state))) else NULL
  saved <- vector("list", length(prepared$blocks))
  for (j in seq_along(prepared$blocks)) {
    .rum_dnn_train_guard(deadline)
    block <- prepared$blocks[[j]]
    ev <- if (need_gradient) rum_dnn_gaussian_loss(state$network, block$data,
      block$Z, state$kappa, state$loading, state$omega, rule) else
      rum_dnn_gaussian_evaluate(block$data,
        rum_dnn_gaussian_forward(state$network, block$Z),
        state$kappa, state$loading, state$omega, rule)
    .rum_dnn_train_guard(deadline)
    fraction <- block$data$n / prepared$n
    loss <- loss + fraction * ev$loss
    if (need_gradient)
      gradient <- gradient + fraction * .rum_dnn_train_pack_gradient(ev$loss_gradient, state)
    if (retain) saved[[j]] <- ev[c("mu", "loglik", "output_score", "common_score")]
  }
  if (!is.finite(loss) || (need_gradient && any(!is.finite(gradient))))
    stop("Non-finite full-batch likelihood or gradient.")
  result <- list(loss = loss, objective = -loss, gradient = gradient, order = order)
  if (retain) {
    result$mu <- do.call(rbind, lapply(saved, `[[`, "mu"))
    result$loglik <- unlist(lapply(saved, `[[`, "loglik"), use.names = FALSE)
    names(result$loglik) <- prepared$ids
    result$output_score <- do.call(rbind, lapply(saved, `[[`, "output_score"))
    result$common_score <- do.call(rbind, lapply(saved, `[[`, "common_score"))
    result$network_gradient <- gradient[seq_len(state$network$parameter_count)]
  }
  result
}

.rum_dnn_train_bounds <- function(state, control) {
  q <- state$network$parameter_count
  p <- state$network$p
  list(lower = c(rep(-Inf, q), control$kappa_bounds[1L],
      rep(control$loading_bounds[1L], p), log(control$omega_bounds[1L])),
    upper = c(rep(Inf, q), control$kappa_bounds[2L],
      rep(control$loading_bounds[2L], p), log(control$omega_bounds[2L])))
}

.rum_dnn_train_penalize <- function(loss, gradient, par, count, coefficient) {
  penalty <- if (coefficient == 0) 0 else coefficient * sum(par[seq_len(count)]^2) / 2
  gradient[seq_len(count)] <- gradient[seq_len(count)] + coefficient * par[seq_len(count)]
  if (!is.finite(penalty) || any(!is.finite(gradient)))
    stop("Non-finite network penalty or penalized gradient.")
  list(loss = loss, penalty = penalty, objective = loss + penalty, gradient = gradient)
}

.rum_dnn_train_stage <- function(initial, training, validation, control,
                                 deadline, epochs, selection = TRUE) {
  time0 <- proc.time()
  state <- initial
  par <- .rum_dnn_train_pack(state)
  m <- v <- numeric(length(par))
  bounds <- .rum_dnn_train_bounds(state, control)
  projected <- rep(FALSE, length(par))
  trace <- list(); checkpoints <- list(); best <- NULL
  stale <- 0L; completed_epochs <- 0L
  stop_reason <- if (selection) "max_epochs" else "selected_epochs_completed"
  error_message <- NULL
  add_checkpoint <- function(epoch, value, state) {
    list(epoch = as.integer(epoch), validation_loss = value, state = state,
         elapsed = .rum_dnn_train_clock() - unname(time0["elapsed"]))
  }
  tryCatch({
    .rum_dnn_train_guard(deadline)
    if (selection) {
      val <- .rum_dnn_train_eval(state, validation, control$train_order, deadline,
        need_gradient = FALSE)$loss
      best <- add_checkpoint(0L, val, state)
      checkpoints[[1L]] <- best
    }
    if (epochs > 0L) for (epoch in seq_len(epochs)) {
      .rum_dnn_train_guard(deadline)
      ev <- .rum_dnn_train_eval(state, training, control$train_order, deadline)
      pp <- .rum_dnn_train_penalize(ev$loss, ev$gradient, par,
        state$network$parameter_count, control$network_l2)
      update <- .rum_dnn_adam_step(par, pp$gradient, m, v, epoch,
        control$learning_rate, control$beta1, control$beta2, control$epsilon,
        bounds$lower, bounds$upper)
      par <- update$par; m <- update$m; v <- update$v
      projected <- projected | update$projected
      state <- .rum_dnn_train_unpack(par, initial)
      completed_epochs <- epoch
      val <- NA_real_
      # Save a completed update even if its subsequent validation call times out.
      trace_index <- length(trace) + 1L
      trace[[trace_index]] <- data.frame(epoch = epoch,
        training_loss_before_update = pp$loss, network_penalty_before_update = pp$penalty,
        penalized_objective_before_update = pp$objective, validation_loss = val,
        elapsed = .rum_dnn_train_clock() - unname(time0["elapsed"]))
      if (selection && (epoch == 1L || epoch %% control$validation_every == 0L || epoch == epochs)) {
        val <- .rum_dnn_train_eval(state, validation, control$train_order, deadline,
          need_gradient = FALSE)$loss
        checkpoint <- add_checkpoint(epoch, val, state)
        checkpoints[[length(checkpoints) + 1L]] <- checkpoint
        if (val < best$validation_loss - control$min_delta) {
          best <- checkpoint; stale <- 0L
        } else stale <- stale + 1L
      }
      trace[[trace_index]]$validation_loss <- val
      trace[[trace_index]]$elapsed <- .rum_dnn_train_clock() - unname(time0["elapsed"])
      if (!selection && epoch == 1L)
        checkpoints[[1L]] <- list(epoch = 1L, state = state, adam = list(m = m, v = v))
      .rum_dnn_train_guard(deadline)
      if (selection && stale >= control$patience) {
        stop_reason <- "validation_patience"; break
      }
    }
  }, rum_dnn_budget = function(e) {
    stop_reason <<- if (selection) "per_start_cap" else "refit_time_cap"
    error_message <<- conditionMessage(e)
  }, error = function(e) {
    stop_reason <<- "evaluation_error"; error_message <<- conditionMessage(e)
  })
  time_used <- proc.time() - time0
  list(initial_state = initial, initial_adam = list(m = numeric(length(par)), v = numeric(length(par))),
    best_checkpoint = best, checkpoints = checkpoints, last_state = state,
    trace = if (length(trace)) do.call(rbind, trace) else data.frame(),
    completed_epochs = completed_epochs, requested_epochs = epochs,
    completed = if (selection) !is.null(best) && stop_reason != "evaluation_error" else
      completed_epochs == epochs && stop_reason == "selected_epochs_completed",
    stop_reason = stop_reason, budget_limited = stop_reason %in% c("per_start_cap", "refit_time_cap"),
    error = error_message, adam = list(m = m, v = v, iteration = completed_epochs),
    projected_coordinates = which(projected),
    elapsed = unname(time_used["elapsed"]),
    cpu = unname(time_used["user.self"] + time_used["sys.self"]))
}

# Bounded Gaussian Structured-Network Training
#' @export
rum_dnn_gaussian_train <- function(data, Z, validation_ids,
    hidden = c(32L, 32L, 16L), seeds = c(42L, 7L, 1609L), control = list(), starts = NULL) {
  time0 <- proc.time()
  .rum_dnn_check_sequence_data(data)
  if (!is.matrix(Z) || !is.numeric(Z) || nrow(Z) != data$n || !ncol(Z) ||
      any(!is.finite(Z)) || !identical(rownames(Z), data$ids))
    stop("Z must be finite and its row names must match data$ids exactly.")
  if (!is.character(validation_ids) || !length(validation_ids) || anyNA(validation_ids) ||
      anyDuplicated(validation_ids) || any(!validation_ids %in% data$ids) ||
      length(validation_ids) >= data$n)
    stop("validation_ids must select a nonempty proper subset of respondent IDs.")
  if (!is.numeric(seeds) || !length(seeds) || any(!is.finite(seeds)) ||
      any(seeds < 0 | seeds > .Machine$integer.max | seeds != floor(seeds)) || anyDuplicated(seeds))
    stop("Supply distinct nonnegative integer initialization seeds.")
  control <- .rum_dnn_train_control(control, length(seeds))
  deadline <- unname(time0["elapsed"]) + control$max_seconds
  j <- seq_len(data$p)
  if (is.null(starts)) starts <- lapply(seq_along(seeds), function(s) {
    index <- (s - 1L) %% 3L + 1L
    list(network = rum_dnn_gaussian(ncol(Z), data$p, hidden, seeds[s]), kappa = 0,
      loading = .10 * switch(index, sin(j), cos(j), (-1)^j),
      omega = c(.25, .5, .75)[index])
  })
  if (!is.list(starts) || length(starts) != length(seeds))
    stop("starts must contain one original state for every initialization seed.")
  for (state in starts) {
    .rum_dnn_check_network(state$network)
    count <- sum(vapply(state$network$weights, length, integer(1))) +
      sum(vapply(state$network$biases, length, integer(1)))
    if (!is.numeric(state$network$parameter_count) ||
        length(state$network$parameter_count) != 1L ||
        !is.finite(state$network$parameter_count) || state$network$parameter_count != count)
      stop("Network parameter_count does not match its weights and biases.")
    if (state$network$p_z != ncol(Z) || state$network$p != data$p ||
        !identical(as.integer(state$network$hidden), as.integer(hidden)) ||
        !is.numeric(state$kappa) || length(state$kappa) != 1L || !is.finite(state$kappa) ||
        !is.numeric(state$loading) || length(state$loading) != data$p || any(!is.finite(state$loading)) ||
        !is.numeric(state$omega) || length(state$omega) != 1L || !is.finite(state$omega) || state$omega <= 0)
      stop("Initial state does not match the declared dimensions or parameters.")
    par <- .rum_dnn_train_pack(state); bounds <- .rum_dnn_train_bounds(state, control)
    if (any(par < bounds$lower | par > bounds$upper)) stop("Initial state is outside declared bounds.")
  }
  inner_val <- which(data$ids %in% validation_ids)
  inner_train <- which(!data$ids %in% validation_ids)
  training <- .rum_dnn_train_prepare(rum_subset(data, inner_train),
    Z[inner_train, , drop = FALSE], control$block_size)
  validation <- .rum_dnn_train_prepare(rum_subset(data, inner_val),
    Z[inner_val, , drop = FALSE], control$block_size)
  full <- .rum_dnn_train_prepare(data, Z, control$block_size)
  answer <- list(status = "incomplete", completed = FALSE, final = NULL,
    selection = NULL, starts = vector("list", length(seeds)), refit = NULL,
    evaluations = list(), numerical_checks = data.frame(), numerical_status = "unavailable",
    stage_cost = data.frame(), control = control, hidden = as.integer(hidden),
    seeds = as.integer(seeds), respondent_ids = data$ids,
    training_ids = data$ids[inner_train], validation_ids = data$ids[inner_val],
    errors = list(), inference_status = "unavailable", convergence_claim = FALSE,
    evaluation_block_size = control$block_size)
  costs <- list()
  finish <- function() {
    used <- proc.time() - time0
    answer$elapsed <- unname(used["elapsed"])
    answer$cpu <- unname(used["user.self"] + used["sys.self"])
    answer$budget_overrun_seconds <- max(0, answer$elapsed - control$max_seconds)
    if (answer$budget_overrun_seconds > 0) {
      answer$status <- "incomplete_total_time_cap"; answer$completed <- FALSE
      answer$final <- NULL
    }
    answer$stage_cost <- if (length(costs)) do.call(rbind, costs) else data.frame()
    class(answer) <- "rum_dnn_train"
    answer
  }
  for (s in seq_along(seeds)) {
    start_deadline <- min(.rum_dnn_train_clock() + control$start_seconds,
      deadline - control$refit_seconds - control$evaluation_reserve)
    result <- .rum_dnn_train_stage(starts[[s]], training, validation, control,
      start_deadline, control$max_epochs, selection = TRUE)
    answer$starts[[s]] <- result
    costs[[length(costs) + 1L]] <- data.frame(stage = paste0("start", s),
      elapsed = result$elapsed, cpu = result$cpu, stop_reason = result$stop_reason)
    if (!is.null(result$error)) answer$errors[[paste0("start", s)]] <- result$error
  }
  eligible <- which(vapply(answer$starts, function(x) isTRUE(x$completed), logical(1)))
  if (!length(eligible)) {answer$status <- "incomplete_no_valid_checkpoint"; return(finish())}
  values <- vapply(answer$starts[eligible], function(x) x$best_checkpoint$validation_loss, numeric(1))
  winner <- eligible[which.min(values)]
  best <- answer$starts[[winner]]$best_checkpoint
  limited <- any(vapply(answer$starts, function(x) isTRUE(x$budget_limited), logical(1)))
  answer$selection <- list(start_index = winner, seed = seeds[winner], epoch = best$epoch,
    validation_loss = best$validation_loss, zero_epoch = best$epoch == 0L,
    budget_limited = limited, tie_break = "first declared start, then earliest checkpoint")
  refit_deadline <- min(.rum_dnn_train_clock() + control$refit_seconds,
                       deadline - control$evaluation_reserve)
  answer$refit <- .rum_dnn_train_stage(starts[[winner]], full, NULL, control,
    refit_deadline, best$epoch, selection = FALSE)
  costs[[length(costs) + 1L]] <- data.frame(stage = "refit", elapsed = answer$refit$elapsed,
    cpu = answer$refit$cpu, stop_reason = answer$refit$stop_reason)
  if (!is.null(answer$refit$error)) answer$errors$refit <- answer$refit$error
  if (!answer$refit$completed) {answer$status <- "incomplete_refit"; return(finish())}
  state <- answer$refit$last_state
  orders <- sort(unique(c(control$train_order, control$check_orders)))
  for (order in orders) {
    eval_time <- proc.time()
    ev <- tryCatch(.rum_dnn_train_eval(state, full, order, deadline,
      retain = TRUE), error = function(e) e)
    used <- proc.time() - eval_time
    failed <- inherits(ev, "error")
    costs[[length(costs) + 1L]] <- data.frame(stage = paste0("evaluation_Q", order),
      elapsed = unname(used["elapsed"]), cpu = unname(used["user.self"] + used["sys.self"]),
      stop_reason = if (failed) conditionMessage(ev) else "finished")
    if (failed) {
      answer$errors[[paste0("evaluation_Q", order)]] <- conditionMessage(ev)
      answer$status <- "incomplete_final_evaluation"; return(finish())
    }
    answer$evaluations[[paste0("Q", order)]] <- ev
  }
  checks <- lapply(seq.int(2L, length(orders)), function(i) {
    a <- answer$evaluations[[i - 1L]]; b <- answer$evaluations[[i]]
    mu_difference <- b$output_score - a$output_score
    common_difference <- b$common_score - a$common_score
    ll <- abs(b$objective - a$objective)
    mu_max <- max(abs(mu_difference)); common_max <- max(abs(common_difference))
    network_max <- max(abs(b$network_gradient - a$network_gradient))
    data.frame(from = orders[i - 1L], to = orders[i], mean_loglik_difference = ll,
      max_output_score_difference = mu_max, rms_output_score_difference = sqrt(mean(mu_difference^2)),
      max_common_score_difference = common_max, rms_common_score_difference = sqrt(mean(common_difference^2)),
      max_network_gradient_difference = network_max,
      max_mean_common_gradient_difference = max(abs(colMeans(common_difference))),
      passed = ll <= control$loglik_tolerance && mu_max <= control$score_tolerance &&
        common_max <= control$score_tolerance && network_max <= control$network_gradient_tolerance)
  })
  answer$numerical_checks <- do.call(rbind, checks)
  answer$numerical_status <- if (all(answer$numerical_checks$passed)) "passed" else "qualified"
  par <- .rum_dnn_train_pack(state); bounds <- .rum_dnn_train_bounds(state, control)
  answer$active_bound_coordinates <- which((is.finite(bounds$lower) & par <= bounds$lower + 1e-8) |
    (is.finite(bounds$upper) & par >= bounds$upper - 1e-8))
  answer$ever_projected_coordinates <- sort(unique(c(unlist(lapply(answer$starts,
    `[[`, "projected_coordinates")), answer$refit$projected_coordinates)))
  answer$final <- state
  answer$completed <- TRUE
  answer$status <- if (limited) "completed_selection_budget_limited" else if (best$epoch == 0L)
    "completed_zero_epoch_selection" else "completed"
  if (any(vapply(answer$starts, function(x) x$stop_reason == "evaluation_error", logical(1))))
    answer$status <- paste0(answer$status, "_with_start_errors")
  finish()
}
