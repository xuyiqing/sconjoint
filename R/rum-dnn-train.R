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
  n_fac <- .rum_th_nfac(result)
  ix <- .rum_th_idx(result$network$p, n_fac, offset = cursor)   # theta follows the network parameters
  result$kappa <- par[ix$kappa]
  result$loading <- if (n_fac == 1L) par[ix$loading] else
    matrix(par[ix$loading], result$network$p, n_fac)
  result$omega <- exp(par[ix$omega])
  result
}

.rum_dnn_train_pack_gradient <- function(gradient, state) {
  c(unlist(gradient$weights, use.names = FALSE),
    unlist(gradient$biases, use.names = FALSE), gradient$kappa,
    gradient$loading, gradient$omega * state$omega)
}

.rum_dnn_adam_step <- function(par, gradient, m, v, iteration,
                               learning_rate, beta1, beta2, epsilon,
                               lower, upper, rows = NULL) {
  m <- beta1 * m + (1 - beta1) * gradient
  v <- beta2 * v + (1 - beta2) * gradient^2
  proposal <- par - learning_rate * (m / (1 - beta1^iteration)) /
    (sqrt(v / (1 - beta2^iteration)) + epsilon)
  if (any(!is.finite(c(m, v, proposal)))) stop("Non-finite Adam update.")
  projected <- proposal < lower | proposal > upper
  out <- pmax(lower, pmin(upper, proposal))
  if (!is.null(rows)) {
    pr <- .rum_project_rows(out, rows)
    out <- pr$par; projected <- projected | pr$projected
  }
  list(par = out, m = m, v = v, projected = projected)
}

# Row-length bound on a p x n_fac loading (several factors): rows$index is the p x n_fac
# matrix of the loading's positions in par and rows$radius the bound on each row's length
# sqrt(sum_k Lambda[j, k]^2). Rows outside the ball are rescaled onto it (the Euclidean
# projection onto the ball). With one factor the trainer keeps the elementwise box, which
# is the same set.
.rum_project_rows <- function(par, rows) {
  projected <- rep(FALSE, length(par))
  for (j in seq_len(nrow(rows$index))) {
    at <- rows$index[j, ]
    len <- sqrt(sum(par[at]^2))
    if (len > rows$radius) {
      par[at] <- par[at] * (rows$radius / len); projected[at] <- TRUE
    }
  }
  list(par = par, projected = projected)
}

# Row lengths of a loading (vector: absolute values).
.rum_loading_row_length <- function(loading)
  if (is.matrix(loading)) sqrt(rowSums(loading^2)) else abs(loading)

# Radius of the row-length bound from the elementwise loading bounds (-r, r).
.rum_loading_radius <- function(control) {
  b <- control$loading_bounds
  if (!isTRUE(all.equal(-b[1L], b[2L], tolerance = 0)))
    stop("With several taste factors loading_bounds must be symmetric, c(-r, r).")
  b[2L]
}

# Principal-axes rotation of a p x n_fac loading (reporting convention, build plan D3):
# Lambda V with V the eigenvectors of Lambda'Lambda in decreasing eigenvalue order, each
# column's sign set so that its largest-absolute entry is positive. Lambda Lambda' (and so
# the model) does not change. One factor (a vector or a p x 1 matrix) is returned as is,
# sign included, so the one-factor path is untouched.
# Tied and near-tied axes (Codex review of 829393d, finding 5). Within a set of (nearly) equal
# eigenvalues the eigenvectors are not unique: Lambda and Lambda A could get different axes, and a
# finite rule integrated on them would give different numbers. Consecutive eigenvalues whose gap
# is at most tie_tol times the largest eigenvalue form one cluster (default 1e-6: below it the
# eigenvectors' rounding error, about 1e-16 / gap, would exceed 1e-10). Within a cluster of m
# axes the basis is fixed by the feature coordinates: with the cluster's columns C = Lambda V_c
# (p x m), take the LQ factorisation C = R' Q' (QR of C', features in their order) and use C Q
# = R', lower trapezoidal with a positive diagonal (the first feature that loads on the cluster
# defines its first axis, and so on). R' does not depend on which orthonormal basis of the cluster
# eigen() returned, so tied axes are the same for every rotation of Lambda. Without ties the
# result is the one above, unchanged. .rum_canonical_axes() also returns the smallest relative
# gap between consecutive eigenvalues (min_gap_rel) and whether a cluster was resolved (near_tie).
.rum_axes_tie_tol <- 1e-6
.rum_canonical_axes <- function(L, tie_tol = .rum_axes_tie_tol) {
  if (!is.matrix(L) || ncol(L) == 1L)
    return(list(V = matrix(1, 1L, 1L), near_tie = FALSE, min_gap_rel = NA_real_, clusters = list()))
  e <- eigen(crossprod(L), symmetric = TRUE)
  o <- order(e$values, decreasing = TRUE); V <- e$vectors[, o, drop = FALSE]; ev <- pmax(e$values[o], 0)
  LV <- L %*% V
  for (k in seq_len(ncol(V))) {
    if (LV[which.max(abs(LV[, k])), k] < 0) V[, k] <- -V[, k]
  }
  q <- ncol(V); top <- ev[1L]
  gap <- if (top > 0) -diff(ev) / top else rep(0, q - 1L)
  cl <- split(seq_len(q), cumsum(c(TRUE, gap > tie_tol)))
  cl <- cl[lengths(cl) > 1L]
  for (ix in cl) {
    if (ev[ix[1L]] <= tie_tol * top) next          # a cluster of null axes: Lambda V_c = 0, nothing to fix
    C <- L %*% V[, ix, drop = FALSE]; m <- length(ix)
    qc <- qr(t(C)); Q <- qr.Q(qc, complete = TRUE); R <- qr.R(qc)   # t(C) = Q R: C Q = R' (lower trapezoidal)
    sg <- rep(1, m); dg <- diag(R)[seq_len(min(m, nrow(R), ncol(R)))]; sg[seq_along(dg)] <- sign(dg)
    sg[sg == 0] <- 1
    V[, ix] <- V[, ix, drop = FALSE] %*% (Q %*% diag(sg, m))
  }
  list(V = V, near_tie = length(cl) > 0L, min_gap_rel = if (q > 1L) min(gap) else NA_real_,
       clusters = unname(cl))
}
.rum_canonical_rotation <- function(L) .rum_canonical_axes(L)$V
.rum_canonical_loading <- function(L) {
  if (!is.matrix(L) || ncol(L) == 1L) return(L)
  out <- L %*% .rum_canonical_rotation(L)
  dimnames(out) <- dimnames(L)
  out
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
  # order: a Gauss-Hermite order (one factor, the original rule) or a rum_quadrature rule
  rule <- if (inherits(order, "rum_quadrature")) order else
    rum_quadrature(order, policy_law = "normal")
  if (inherits(order, "rum_quadrature")) order <- .rum_quad_label(order)
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
  n_net <- state$network$parameter_count
  p <- state$network$p
  n_fac <- .rum_th_nfac(state)
  if (n_fac == 1L)
    return(list(lower = c(rep(-Inf, n_net), control$kappa_bounds[1L],
      rep(control$loading_bounds[1L], p), log(control$omega_bounds[1L])),
    upper = c(rep(Inf, n_net), control$kappa_bounds[2L],
      rep(control$loading_bounds[2L], p), log(control$omega_bounds[2L]))))
  # several factors: no box on the loading; each row's length is bounded instead (D4)
  ix <- .rum_th_idx(p, n_fac, offset = n_net)
  list(lower = c(rep(-Inf, n_net), control$kappa_bounds[1L],
      rep(-Inf, p * n_fac), log(control$omega_bounds[1L])),
    upper = c(rep(Inf, n_net), control$kappa_bounds[2L],
      rep(Inf, p * n_fac), log(control$omega_bounds[2L])),
    rows = list(index = matrix(ix$loading, p, n_fac), radius = .rum_loading_radius(control)))
}

# Coordinates on a bound (box, or a loading row on the row-length bound).
.rum_dnn_active_bounds <- function(par, bounds, tol = 1e-8) {
  active <- (is.finite(bounds$lower) & par <= bounds$lower + tol) |
    (is.finite(bounds$upper) & par >= bounds$upper - tol)
  if (!is.null(bounds$rows)) for (j in seq_len(nrow(bounds$rows$index))) {
    at <- bounds$rows$index[j, ]
    if (sqrt(sum(par[at]^2)) >= bounds$rows$radius - tol) active[at] <- TRUE
  }
  which(active)
}

.rum_dnn_train_penalize <- function(loss, gradient, par, count, coefficient) {
  penalty <- if (coefficient == 0) 0 else coefficient * sum(par[seq_len(count)]^2) / 2
  gradient[seq_len(count)] <- gradient[seq_len(count)] + coefficient * par[seq_len(count)]
  if (!is.finite(penalty) || any(!is.finite(gradient)))
    stop("Non-finite network penalty or penalized gradient.")
  list(loss = loss, penalty = penalty, objective = loss + penalty, gradient = gradient)
}

.rum_dnn_train_stage <- function(initial, training, validation, control,
                                 deadline, epochs, selection = TRUE,
                                 rule = control$train_order) {
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
      val <- .rum_dnn_train_eval(state, validation, rule, deadline,
        need_gradient = FALSE)$loss
      best <- add_checkpoint(0L, val, state)
      checkpoints[[1L]] <- best
    }
    if (epochs > 0L) for (epoch in seq_len(epochs)) {
      .rum_dnn_train_guard(deadline)
      ev <- .rum_dnn_train_eval(state, training, rule, deadline)
      pp <- .rum_dnn_train_penalize(ev$loss, ev$gradient, par,
        state$network$parameter_count, control$network_l2)
      update <- .rum_dnn_adam_step(par, pp$gradient, m, v, epoch,
        control$learning_rate, control$beta1, control$beta2, control$epsilon,
        bounds$lower, bounds$upper, bounds$rows)
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
        val <- .rum_dnn_train_eval(state, validation, rule, deadline,
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

# Training and check rules. NULL at one factor keeps the integer Gauss-Hermite orders of
# the control (the original path). Several factors default to Sobol: 512 nodes for
# training, 2,048 for the check (the first 512 of the 2,048 are the training nodes).
.rum_dnn_train_rules <- function(quadrature, n_fac) {
  if (is.null(quadrature)) {
    if (n_fac == 1L) return(NULL)
    return(list(train = rum_quadrature(n_fac = n_fac, rule = "sobol", nodes = 512L),
                check = list(rum_quadrature(n_fac = n_fac, rule = "sobol", nodes = 2048L))))
  }
  if (inherits(quadrature, "rum_quadrature")) stop("quadrature must be list(train = , check = ).")
  if (!is.list(quadrature) || !inherits(quadrature$train, "rum_quadrature") ||
      !is.list(quadrature$check) || !length(quadrature$check) ||
      !all(vapply(quadrature$check, inherits, TRUE, "rum_quadrature")))
    stop("quadrature must be list(train = <rum_quadrature>, check = list(<rum_quadrature>, ...)).")
  for (r in c(list(quadrature$train), quadrature$check)) .rum_dnn_check_normal_rule(r, n_fac)
  labels <- vapply(c(list(quadrature$train), quadrature$check), .rum_quad_label, "")
  if (anyDuplicated(labels)) stop("The training and check rules must differ.")
  list(train = quadrature$train, check = quadrature$check)
}

# Bounded Gaussian Structured-Network Training
#' @export
# n_fac: number of taste factors (loading p x n_fac; a vector at n_fac = 1).
# quadrature: NULL (one factor: the Gauss-Hermite orders control$train_order and
#   control$check_orders, as before; several factors: Sobol, 512 training nodes and a 2,048
#   node check, seed 20261114), or list(train = <rum_quadrature>, check = list(<rum_quadrature>,
#   ...)) with rules that integrate n_fac factors.
rum_dnn_gaussian_train <- function(data, Z, validation_ids,
    hidden = c(32L, 32L, 16L), seeds = c(42L, 7L, 1609L), control = list(), starts = NULL,
    n_fac = 1L, quadrature = NULL) {
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
  n_fac <- .rum_dnn_integer(n_fac, "n_fac")
  rules <- .rum_dnn_train_rules(quadrature, n_fac)
  if (n_fac > 1L) .rum_loading_radius(control)
  deadline <- unname(time0["elapsed"]) + control$max_seconds
  j <- seq_len(data$p)
  pattern <- function(index) .10 * switch(index, sin(j), cos(j), (-1)^j)
  # Start s uses pattern (s - 1) mod 3 + 1 for the first factor; factor k moves k - 1
  # patterns on (the q-factor prototype's rule, rum_apps_twofactor_2026-11-05/K_common.R).
  if (is.null(starts)) starts <- lapply(seq_along(seeds), function(s) {
    index <- (s - 1L) %% 3L + 1L
    list(network = rum_dnn_gaussian(ncol(Z), data$p, hidden, seeds[s]), kappa = 0,
      # matrix(): vapply() simplifies to a vector when p = 1 (Codex review of 829393d, finding 3)
      loading = if (n_fac == 1L) pattern(index) else
        matrix(vapply(seq_len(n_fac), function(k) pattern((index + k - 2L) %% 3L + 1L), numeric(data$p)),
               data$p, n_fac),
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
        !is.numeric(state$loading) || length(state$loading) != data$p * n_fac || any(!is.finite(state$loading)) ||
        (n_fac > 1L && !identical(dim(state$loading), c(data$p, n_fac))) ||
        !is.numeric(state$omega) || length(state$omega) != 1L || !is.finite(state$omega) || state$omega <= 0)
      stop("Initial state does not match the declared dimensions or parameters.")
    par <- .rum_dnn_train_pack(state); bounds <- .rum_dnn_train_bounds(state, control)
    if (any(par < bounds$lower | par > bounds$upper) ||
        (!is.null(bounds$rows) && any(.rum_loading_row_length(state$loading) > bounds$rows$radius)))
      stop("Initial state is outside declared bounds.")
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
    result <- if (is.null(rules)) .rum_dnn_train_stage(starts[[s]], training, validation,
      control, start_deadline, control$max_epochs, selection = TRUE) else
      .rum_dnn_train_stage(starts[[s]], training, validation, control,
      start_deadline, control$max_epochs, selection = TRUE, rule = rules$train)
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
  answer$refit <- if (is.null(rules)) .rum_dnn_train_stage(starts[[winner]], full, NULL,
    control, refit_deadline, best$epoch, selection = FALSE) else
    .rum_dnn_train_stage(starts[[winner]], full, NULL, control,
    refit_deadline, best$epoch, selection = FALSE, rule = rules$train)
  costs[[length(costs) + 1L]] <- data.frame(stage = "refit", elapsed = answer$refit$elapsed,
    cpu = answer$refit$cpu, stop_reason = answer$refit$stop_reason)
  if (!is.null(answer$refit$error)) answer$errors$refit <- answer$refit$error
  if (!answer$refit$completed) {answer$status <- "incomplete_refit"; return(finish())}
  state <- answer$refit$last_state
  # several factors: report the loading on its principal axes (D3); the model is unchanged
  if (n_fac > 1L) state$loading <- .rum_canonical_loading(state$loading)
  orders <- if (is.null(rules)) sort(unique(c(control$train_order, control$check_orders))) else
    c(list(rules$train), rules$check)
  order_labels <- if (is.null(rules)) orders else vapply(orders, .rum_quad_label, "")
  for (order in orders) {
    if (inherits(order, "rum_quadrature")) {rule_q <- order; order <- .rum_quad_label(order)}
    else rule_q <- order
    eval_time <- proc.time()
    ev <- tryCatch(.rum_dnn_train_eval(state, full, rule_q, deadline,
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
    data.frame(from = order_labels[i - 1L], to = order_labels[i], mean_loglik_difference = ll,
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
  answer$active_bound_coordinates <- if (n_fac == 1L)
    which((is.finite(bounds$lower) & par <= bounds$lower + 1e-8) |
    (is.finite(bounds$upper) & par >= bounds$upper - 1e-8)) else
    .rum_dnn_active_bounds(par, bounds)
  answer$ever_projected_coordinates <- sort(unique(c(unlist(lapply(answer$starts,
    `[[`, "projected_coordinates")), answer$refit$projected_coordinates)))
  answer$final <- state
  if (n_fac > 1L) {
    answer$n_fac <- n_fac
    answer$quadrature <- list(train = .rum_quad_label(rules$train),
                              check = vapply(rules$check, .rum_quad_label, ""))
  }
  answer$completed <- TRUE
  answer$status <- if (limited) "completed_selection_budget_limited" else if (best$epoch == 0L)
    "completed_zero_epoch_selection" else "completed"
  if (any(vapply(answer$starts, function(x) x$stop_reason == "evaluation_error", logical(1))))
    answer$status <- paste0(answer$status, "_with_start_errors")
  finish()
}
