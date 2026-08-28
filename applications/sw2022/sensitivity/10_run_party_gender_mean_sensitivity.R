#!/usr/bin/env Rscript

## Post-hoc, application-only party-by-candidate-gender mean sensitivity.
##
## The maintained production DNN and every primary artifact are read-only.
## This runner fits two deliberately simple q=1 integrated mixed logits on the
## inherited respondent outer folds: (i) a common 13-coordinate mean, and
## (ii) that mean plus Republican- and Independent-by-candidate-Male
## deviations.  The comparison is diagnostic and never supplies formal
## inference or an outcome-blind model-selection claim.

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(profile = "smoke", force = FALSE)
  for (arg in x) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg,
                                       call. = FALSE)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", bits[[1L]], fixed = TRUE)
    if (!key %in% names(out)) stop("Unknown argument --", bits[[1L]],
                                   call. = FALSE)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out
}

.atomic_save <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.inverse_bound <- function(x, bound) {
  bound * atanh(pmin(pmax(x / bound, -0.98), 0.98))
}

.cluster_lm <- function(y, X, cluster) {
  X <- as.matrix(X); y <- as.numeric(y); cluster <- as.character(cluster)
  fit <- stats::lm.fit(X, y)
  if (fit$rank != ncol(X)) stop("AMCE projection is rank deficient.",
                                call. = FALSE)
  bread <- solve(crossprod(X))
  score <- rowsum(X * as.numeric(fit$residuals), cluster, reorder = FALSE)
  G <- nrow(score); n <- nrow(X); k <- ncol(X)
  correction <- (G / (G - 1)) * ((n - 1) / (n - k))
  list(coef = as.numeric(fit$coefficients),
       vcov = correction * bread %*% crossprod(score) %*% bread,
       n = n, G = G)
}

.amce_contrasts <- function(p, coordinate_names) {
  if (p != 13L) stop("The frozen application basis must have p=13.",
                      call. = FALSE)
  C <- matrix(0, 13L, p)
  rownames(C) <- c(
    "Female vs Male", "Previously ran: Yes vs No",
    "Collaborative vs Empathetic", "Determined vs Empathetic",
    "Assertive vs Empathetic", "Good Communicator vs Empathetic",
    "Hard-Working vs Empathetic", "Tough Negotiator vs Empathetic",
    "Moderate Changes vs Very Few", "Complete Overhaul vs Very Few",
    "1 child vs No children", "2 children vs No children",
    "3 children vs No children"
  )
  C[1L, 1L] <- -1
  C[2L, 2L] <- 1
  C[3L, c(3L, 5L)] <- c(1, -1)
  C[4L, c(4L, 5L)] <- c(1, -1)
  C[5L, 5L] <- -1
  C[6L, c(6L, 5L)] <- c(1, -1)
  C[7L, c(7L, 5L)] <- c(1, -1)
  C[8L, c(8L, 5L)] <- c(1, -1)
  C[9L, 9L] <- C[10L, 10L] <- C[11L, 11L] <-
    C[12L, 12L] <- C[13L, 13L] <- 1
  colnames(C) <- coordinate_names
  C
}

.sequence_from_task_probability <- function(probability, y, rid) {
  probability <- pmin(pmax(probability, 1e-10), 1 - 1e-10)
  task <- y * log(probability) + (1 - y) * log1p(-probability)
  aggregated <- rowsum(task, as.character(rid), reorder = FALSE)
  out <- aggregated[, 1L]
  names(out) <- rownames(aggregated)
  out
}

.paired_summary <- function(scores, group) {
  comparisons <- list(
    targeted_minus_primary = c("targeted_q1", "primary_dnn_q1"),
    targeted_minus_pooled = c("targeted_q1", "pooled_mean_q1"),
    pooled_minus_primary = c("pooled_mean_q1", "primary_dnn_q1")
  )
  strata <- c("Overall", sort(unique(group)))
  rows <- list(); z <- 0L
  for (nm in names(comparisons)) {
    pair <- comparisons[[nm]]
    difference <- scores[[pair[[1L]]]] - scores[[pair[[2L]]]]
    for (stratum in strata) {
      keep <- if (stratum == "Overall") rep(TRUE, length(group)) else
        group == stratum
      d <- difference[keep]
      z <- z + 1L
      rows[[z]] <- data.frame(
        comparison = nm, party = stratum, mean_difference = mean(d),
        respondent_se = stats::sd(d) / sqrt(length(d)),
        n_respondents = length(d),
        interpretation = paste(
          "Positive favors the first model. Descriptive paired outer-fold",
          "score comparison; the targeted diagnostic was chosen post hoc."
        ), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

.score_summary <- function(scores, group) {
  strata <- c("Overall", sort(unique(group)))
  rows <- list(); z <- 0L
  for (model in names(scores)) {
    for (stratum in strata) {
      keep <- if (stratum == "Overall") rep(TRUE, length(group)) else
        group == stratum
      x <- scores[[model]][keep]
      z <- z + 1L
      rows[[z]] <- data.frame(
        model = model, party = stratum,
        mean_complete_sequence_log_score = mean(x),
        respondent_se = stats::sd(x) / sqrt(length(x)),
        n_respondents = length(x), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

.q0_scope_check <- function(dx, y, rid, party, fold) {
  rep_flag <- as.numeric(party == "Republican")
  ind_flag <- as.numeric(party == "Independent")
  X <- list(
    q0_pooled = cbind(intercept = 1, dx),
    q0_targeted = cbind(intercept = 1, dx,
                        gender_x_republican = dx[, 1L] * rep_flag,
                        gender_x_independent = dx[, 1L] * ind_flag),
    q0_all_party_slopes = cbind(intercept = 1, dx,
                                dx * rep_flag, dx * ind_flag)
  )
  prediction <- lapply(X, function(x) rep(NA_real_, nrow(x)))
  for (k in sort(unique(fold))) {
    train <- fold != k; test <- !train
    for (nm in names(X)) {
      fit <- stats::glm.fit(X[[nm]][train, , drop = FALSE], y[train],
                            family = stats::binomial(),
                            control = list(maxit = 100L))
      if (fit$rank != ncol(X[[nm]]) || any(!is.finite(fit$coefficients))) {
        stop("q=0 scope-check fit failed for ", nm, " fold ", k, ".",
             call. = FALSE)
      }
      prediction[[nm]][test] <- stats::plogis(
        drop(X[[nm]][test, , drop = FALSE] %*% fit$coefficients))
    }
  }
  score <- lapply(prediction, .sequence_from_task_probability,
                  y = y, rid = rid)
  ids <- unique(as.character(rid))
  group <- party[match(ids, rid)]
  comparisons <- list(
    targeted_minus_pooled = score$q0_targeted - score$q0_pooled,
    all_slopes_minus_pooled = score$q0_all_party_slopes - score$q0_pooled,
    all_slopes_minus_targeted =
      score$q0_all_party_slopes - score$q0_targeted
  )
  rows <- lapply(names(comparisons), function(nm) {
    d <- comparisons[[nm]]
    data.frame(
      comparison = nm, mean_difference = mean(d),
      respondent_se = stats::sd(d) / sqrt(length(d)),
      n_respondents = length(d),
      role = paste(
        "Same-sample post-hoc scope diagnostic only; it motivated the narrow",
        "q=1 sensitivity and is not a maintained-model comparison."
      ), stringsAsFactors = FALSE
    )
  })
  list(prediction = prediction, score = score, comparison = do.call(rbind, rows),
       party = group)
}

## Custom fixed-dimensional mean module.  It is intentionally application-
## local and does not alter the package's maintained DNN implementation.
.party_mean_module <- torch::nn_module(
  "SWPartyGenderMeanMixedLogit",
  initialize = function(p, targeted, mu_bound, kappa_bound, a_init_sd = 0.15) {
    self$p_beta <- as.integer(p)
    self$q <- 1L
    self$targeted <- isTRUE(targeted)
    self$mu_bound <- mu_bound
    self$kappa_bound <- kappa_bound
    self$alpha_raw <- torch::nn_parameter(torch::torch_zeros(p))
    if (self$targeted) {
      self$gamma_raw <- torch::nn_parameter(torch::torch_zeros(2L))
    }
    self$kappa_raw <- torch::nn_parameter(torch::torch_zeros(1L))
    self$A <- torch::nn_parameter(torch::torch_randn(p, 1L) * a_init_sd)
    self$gender_basis <- torch::torch_tensor(
      c(1, rep(0, p - 1L)), dtype = torch::torch_float())
  },
  get_beta = function(z) {
    n <- z$shape[[1L]]
    raw <- self$alpha_raw$unsqueeze(1L)$expand(c(n, self$p_beta))
    if (self$targeted) {
      offset <- torch::torch_sum(
        z * self$gamma_raw$unsqueeze(1L), dim = 2L)
      raw <- raw + offset$unsqueeze(2L) * self$gender_basis$unsqueeze(1L)
    }
    self$mu_bound * torch::torch_tanh(raw / self$mu_bound)
  },
  get_kappa = function() {
    self$kappa_bound * torch::torch_tanh(
      self$kappa_raw / self$kappa_bound)
  }
)

.glm_initial_values <- function(dx, y, z, targeted, mu_bound, kappa_bound) {
  X <- cbind(intercept = 1, dx)
  if (targeted) {
    X <- cbind(X, gender_x_republican = dx[, 1L] * z[, 1L],
               gender_x_independent = dx[, 1L] * z[, 2L])
  }
  fit <- stats::glm.fit(X, y, family = stats::binomial(),
                        control = list(maxit = 100L))
  if (fit$rank != ncol(X) || any(!is.finite(fit$coefficients))) {
    stop("Training-only GLM initialization failed.", call. = FALSE)
  }
  b <- as.numeric(fit$coefficients)
  alpha <- .inverse_bound(b[seq.int(2L, ncol(dx) + 1L)], mu_bound)
  gamma <- numeric()
  if (targeted) {
    total_rep <- b[[2L]] + b[[ncol(dx) + 2L]]
    total_ind <- b[[2L]] + b[[ncol(dx) + 3L]]
    gamma <- c(.inverse_bound(total_rep, mu_bound) - alpha[[1L]],
               .inverse_bound(total_ind, mu_bound) - alpha[[1L]])
  }
  list(alpha_raw = alpha, gamma_raw = gamma,
       kappa_raw = .inverse_bound(b[[1L]], kappa_bound))
}

.fit_one_start <- function(dx, y, z, rid, grid, targeted, controls, bounds,
                           seed, start) {
  withr::local_preserve_seed()
  set.seed(seed)
  torch::torch_manual_seed(seed)
  dev <- torch::torch_device("cpu")
  rf <- factor(as.character(rid), levels = unique(as.character(rid)))
  tensors <- list(
    dx = torch::torch_tensor(dx, dtype = torch::torch_float(), device = dev),
    z = torch::torch_tensor(z, dtype = torch::torch_float(), device = dev),
    y = torch::torch_tensor(y, dtype = torch::torch_float(), device = dev),
    ri = torch::torch_tensor(as.integer(rf), dtype = torch::torch_long(),
                             device = dev),
    U = torch::torch_tensor(grid$U, dtype = torch::torch_float(), device = dev),
    logw = torch::torch_tensor(log(grid$w), dtype = torch::torch_float(),
                               device = dev)
  )
  init <- .glm_initial_values(dx, y, z, targeted, bounds$mu, bounds$kappa)
  net <- .party_mean_module(ncol(dx), targeted, bounds$mu, bounds$kappa)
  net$to(device = dev)
  torch::with_no_grad({
    net$alpha_raw$copy_(torch::torch_tensor(
      init$alpha_raw + stats::rnorm(length(init$alpha_raw), 0, 0.01),
      dtype = torch::torch_float(), device = dev))
    if (targeted) {
      net$gamma_raw$copy_(torch::torch_tensor(
        init$gamma_raw + stats::rnorm(2L, 0, 0.01),
        dtype = torch::torch_float(), device = dev))
    }
    net$kappa_raw$copy_(torch::torch_tensor(
      init$kappa_raw + stats::rnorm(1L, 0, 0.01),
      dtype = torch::torch_float(), device = dev))
  })
  .sc_mixed_project_parameters(
    net, coefficient_scale = rep(1, ncol(dx)),
    a_bound = bounds$loading, weight_bound = bounds$parameter)
  optimizer <- torch::optim_adam(net$parameters,
                                 lr = controls$learning_rate,
                                 weight_decay = 0)
  loss_trace <- grad_trace <- rep(NA_real_, controls$n_epochs)
  previous_check <- Inf; stable <- 0L
  stopped_at <- controls$n_epochs; stop_reason <- "maximum_epochs"
  for (epoch in seq_len(controls$n_epochs)) {
    net$train(); optimizer$zero_grad()
    nll <- .sc_mixed_nll(net, tensors$dx, tensors$z, tensors$y,
                         tensors$U, tensors$logw, tensors$ri, nlevels(rf))
    nll$backward()
    gradient <- .sc_mixed_gradient_norm(net)
    optimizer$step()
    .sc_mixed_project_parameters(
      net, coefficient_scale = rep(1, ncol(dx)),
      a_bound = bounds$loading, weight_bound = bounds$parameter)
    loss_trace[[epoch]] <- as.numeric(nll$item())
    grad_trace[[epoch]] <- gradient
    if (epoch %% 20L == 0L) {
      rel <- abs(loss_trace[[epoch]] - previous_check) /
        max(1, abs(previous_check))
      if (is.finite(rel) && rel <= controls$opt_tol &&
          gradient <= controls$grad_tol) stable <- stable + 1L else stable <- 0L
      previous_check <- loss_trace[[epoch]]
      if (stable >= 3L) {
        stopped_at <- epoch; stop_reason <- "criterion_and_gradient_tolerance"
        break
      }
    }
  }
  net$eval(); optimizer$zero_grad()
  final_nll_t <- .sc_mixed_nll(net, tensors$dx, tensors$z, tensors$y,
                               tensors$U, tensors$logw, tensors$ri,
                               nlevels(rf))
  final_nll_t$backward()
  gradient <- .sc_mixed_gradient_diagnostics(net)
  final_nll <- as.numeric(final_nll_t$item())
  mu <- .sc_predict_beta(net, z)
  kappa <- as.numeric(net$get_kappa()$detach()$cpu()$item())
  bound_state <- .sc_mixed_bound_diagnostics(
    net, mu = mu, kappa = kappa, coefficient_scale = rep(1, ncol(dx)),
    mu_bound = bounds$mu, kappa_bound = bounds$kappa,
    a_bound = bounds$loading, weight_bound = bounds$parameter)
  status <- .sc_mixed_optimization_status(
    final_loss = final_nll, final_nll = final_nll,
    previous_loss = loss_trace[[stopped_at]], gradient = gradient,
    opt_tol = controls$opt_tol, grad_tol = controls$grad_tol,
    bounds = bound_state, state_restored = FALSE)
  list(
    net = net, objective = -final_nll, final_nll = final_nll,
    mu = mu, A = as.matrix(torch::as_array(net$A$detach()$cpu())),
    kappa = kappa, optimization = status,
    final_gradient_norm = gradient$total,
    loss_trace = loss_trace[seq_len(stopped_at)],
    grad_trace = grad_trace[seq_len(stopped_at)],
    epochs = stopped_at, stop_reason = stop_reason, start = start
  )
}

.fit_multistart <- function(dx, y, z, rid, grid, targeted, controls, bounds,
                            seed) {
  fits <- lapply(seq_len(controls$n_starts), function(s) {
    .fit_one_start(dx, y, z, rid, grid, targeted, controls, bounds,
                   seed = seed + 10007L * s, start = s)
  })
  objective <- vapply(fits, `[[`, numeric(1L), "objective")
  gate <- vapply(fits, function(x)
    isTRUE(x$optimization$optimization_gate_pass), logical(1L))
  eligible <- is.finite(objective) & gate
  selected <- if (any(eligible)) {
    which.max(ifelse(eligible, objective, -Inf))
  } else which.max(ifelse(is.finite(objective), objective, -Inf))
  diagnostics <- do.call(rbind, lapply(seq_along(fits), function(s) {
    x <- fits[[s]]
    data.frame(
      start = s, selected = s == selected, objective = x$objective,
      gradient_norm = x$final_gradient_norm,
      relative_change = x$optimization$last_relative_change,
      criterion_tolerance_met = x$optimization$criterion_tolerance_met,
      stationarity_met = x$optimization$stationarity_met,
      bound_activity = x$optimization$bound_activity,
      optimization_gate_pass = x$optimization$optimization_gate_pass,
      epochs = x$epochs, stop_reason = x$stop_reason,
      failure_reasons = paste(x$optimization$failure_reasons, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))
  best <- fits[[selected]]
  best$start_diagnostics <- diagnostics
  best$selected_start <- selected
  best$any_eligible_start <- any(eligible)
  best
}

.predict_marginal <- function(fit, dx, z, grid) {
  mu <- .sc_predict_beta(fit$net, z)
  base <- fit$kappa + rowSums(dx * mu)
  factor_index <- dx %*% fit$A %*% t(grid$U)
  probability <- rowSums(stats::plogis(sweep(factor_index, 1L, base, `+`)) *
                           rep(grid$w, each = nrow(dx)))
  if (any(!is.finite(probability)) || any(probability <= 0) ||
      any(probability >= 1)) stop("Nonfinite marginal predictions.",
                                  call. = FALSE)
  probability
}

.compact_fit <- function(fit, training_ids, role, attr_names) {
  z_group <- rbind(Democrat = c(0, 0), Independent = c(0, 1),
                   Republican = c(1, 0))
  mean_by_party <- .sc_predict_beta(fit$net, z_group)
  colnames(mean_by_party) <- attr_names
  state_dict_cpu <- lapply(fit$net$state_dict(), function(tensor) {
    torch::as_array(tensor$detach()$cpu())
  })
  list(
    role = role, training_respondents = training_ids,
    training_respondent_count = length(training_ids),
    selected_start = fit$selected_start,
    any_eligible_start = fit$any_eligible_start,
    objective = fit$objective, A = fit$A, kappa = fit$kappa,
    optimization = fit$optimization,
    start_diagnostics = fit$start_diagnostics,
    epochs = fit$epochs, stop_reason = fit$stop_reason,
    mean_by_party = mean_by_party,
    state_dict_cpu = state_dict_cpu,
    portable_state_note = paste(
      "Pointer-free CPU arrays retained for audit only; no optimizer state is",
      "stored and this artifact is not represented as resumable training."
    )
  )
}

.run_simple_model <- function(label, targeted, dx, y, z, rid, fold, grid,
                              controls, bounds, seed) {
  ids <- unique(as.character(rid))
  K <- max(fold)
  task_probability <- rep(NA_real_, nrow(dx))
  sequence_score <- stats::setNames(rep(NA_real_, length(ids)), ids)
  compact_folds <- vector("list", K)
  selected_gate <- logical(K)
  for (k in seq_len(K)) {
    train <- fold != k; test <- !train
    train_ids <- unique(as.character(rid[train]))
    test_ids <- unique(as.character(rid[test]))
    if (length(intersect(train_ids, test_ids))) {
      stop("Respondent leakage in outer fold ", k, ".", call. = FALSE)
    }
    message(label, ": outer fold ", k, "/", K)
    fit <- .fit_multistart(
      dx[train, , drop = FALSE], y[train], z[train, , drop = FALSE], rid[train],
      grid, targeted, controls, bounds, seed + 100000L * k)
    task_probability[test] <- .predict_marginal(
      fit, dx[test, , drop = FALSE], z[test, , drop = FALSE], grid)
    ll <- .sc_comp_sequence_loglik(
      fit$net, dx[test, , drop = FALSE], y[test], z[test, , drop = FALSE],
      rid[test], grid, device = "cpu")
    sequence_score[names(ll)] <- ll
    selected_gate[[k]] <- isTRUE(fit$optimization$optimization_gate_pass)
    compact_folds[[k]] <- .compact_fit(
      fit, train_ids, paste0(label, "_outer_", k), colnames(dx))
    rm(fit); invisible(gc(FALSE))
  }
  message(label, ": full sample")
  full <- .fit_multistart(dx, y, z, rid, grid, targeted, controls, bounds,
                          seed + 900001L)
  full_gate <- isTRUE(full$optimization$optimization_gate_pass)
  if (anyNA(task_probability) || anyNA(sequence_score)) {
    stop("Outer-fold predictions are incomplete for ", label, ".",
         call. = FALSE)
  }
  list(
    label = label, targeted = targeted, probability = task_probability,
    sequence_score = sequence_score, folds = compact_folds,
    full = full,
    optimization_gate_by_fold = selected_gate,
    full_optimization_gate = full_gate,
    computational_gate_pass = all(selected_gate) && full_gate
  )
}

.amce_comparison <- function(dx, y, rid, party, predictions, C) {
  rows <- list(); z <- 0L
  for (g in c("Democrat", "Independent", "Republican")) {
    keep <- party == g
    X <- cbind(intercept = 1, dx[keep, , drop = FALSE])
    observed <- .cluster_lm(y[keep], X, rid[keep])
    obs <- as.numeric(C %*% observed$coef[-1L])
    se <- sqrt(pmax(diag(C %*% observed$vcov[-1L, -1L, drop = FALSE] %*%
                              t(C)), 0))
    model_projection <- lapply(predictions, function(p) {
      fit <- stats::lm.fit(X, p[keep])
      as.numeric(C %*% fit$coefficients[-1L])
    })
    for (j in seq_len(nrow(C))) {
      z <- z + 1L
      rows[[z]] <- data.frame(
        party = g, contrast = rownames(C)[[j]],
        observed_amce = obs[[j]], observed_cluster_se = se[[j]],
        primary_dnn_oof_projection =
          model_projection$primary_dnn_q1[[j]],
        pooled_mean_q1_oof_projection =
          model_projection$pooled_mean_q1[[j]],
        targeted_q1_oof_projection = model_projection$targeted_q1[[j]],
        primary_gap = obs[[j]] - model_projection$primary_dnn_q1[[j]],
        targeted_gap = obs[[j]] - model_projection$targeted_q1[[j]],
        comparison_status = paste(
          "Realized-design probability-scale projection; observed SE is",
          "descriptive and no discrepancy test is supplied."
        ), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

.calibration <- function(y, rid, party, predictions) {
  rows <- list(); z <- 0L
  for (model in names(predictions)) {
    p <- pmin(pmax(predictions[[model]], 1e-10), 1 - 1e-10)
    for (g in c("Democrat", "Independent", "Republican")) {
      keep <- party == g
      task_gap <- y[keep] - p[keep]
      respondent_gap <- rowsum(task_gap, rid[keep], reorder = FALSE)[, 1L] /
        as.numeric(table(factor(rid[keep], levels =
                                  unique(as.character(rid[keep])))))
      z <- z + 1L
      rows[[z]] <- data.frame(
        model = model, party = g, observed_rate = mean(y[keep]),
        predicted_rate = mean(p[keep]), calibration_gap = mean(task_gap),
        respondent_se_gap = stats::sd(respondent_gap) /
          sqrt(length(respondent_gap)),
        brier_score = mean((y[keep] - p[keep])^2),
        marginal_task_log_score =
          mean(y[keep] * log(p[keep]) + (1 - y[keep]) * log1p(-p[keep])),
        n_respondents = length(unique(as.character(rid[keep]))),
        n_tasks = sum(keep), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

.structural_gender <- function(model, party_levels, group_mu, A, grid) {
  do.call(rbind, lapply(seq_along(party_levels), function(j) {
    d <- numeric(ncol(group_mu)); d[[1L]] <- -1
    index <- sum(d * group_mu[j, ]) + as.numeric(crossprod(d, A)) *
      grid$U[, 1L]
    data.frame(
      model = model, party = party_levels[[j]],
      female_vs_male_latent_preference = sum(d * group_mu[j, ]),
      female_vs_male_position_neutral_choice_probability =
        sum(grid$w * stats::plogis(index)),
      position_neutral_probability_minus_half =
        sum(grid$w * stats::plogis(index)) - 0.5,
      formal_inference_available = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

.optimization_table <- function(x) {
  rows <- list(); z <- 0L
  for (model in c("pooled_mean_q1", "targeted_q1")) {
    object <- x[[model]]
    all_fits <- c(object$folds, list(object$full_compact))
    for (fit in all_fits) {
      d <- fit$start_diagnostics
      d$model <- model; d$fit_role <- fit$role
      z <- z + 1L; rows[[z]] <- d
    }
  }
  out <- do.call(rbind, rows)
  out[, c("model", "fit_role", setdiff(names(out), c("model", "fit_role"))),
      drop = FALSE]
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
if (!cli$profile %in% c("smoke", "production")) {
  stop("--profile must be smoke or production.", call. = FALSE)
}
root <- normalizePath(file.path(dirname(.script_file()), "..", "..", ".."),
                      mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
source(file.path(app, "sensitivity", "party_gender_mean_config.R"),
       local = FALSE)
controls <- sw_party_gender_mean_config$controls[[cli$profile]]
output_dir <- file.path(app, "results", "party_gender_mean_sensitivity",
                        cli$profile)
result_path <- file.path(output_dir, "party_gender_mean_sensitivity.rds")
if (file.exists(result_path) && !cli$force) {
  message("checkpoint: existing diagnostic retained: ", result_path)
  quit(save = "no", status = 0L)
}

if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

input_paths <- c(
  prepared = file.path(app, "results", "prep_analysis_data.rds"),
  primary_full = file.path(app, "results", "mixed_logit", cli$profile,
                           "fit_primary_full.rds"),
  primary_assembled = file.path(app, "results", "mixed_logit", cli$profile,
                                "fit_primary_assembled.rds"),
  config = file.path(app, "sensitivity", "party_gender_mean_config.R"),
  runner = .script_file(),
  mixed_likelihood_source = file.path(root, "R", "mixed-likelihood.R"),
  computation_source = file.path(root, "R", "paperps-computation.R"),
  prediction_source = file.path(root, "R",
                                "paperps-assessment-predictions.R")
)
if (any(!file.exists(input_paths))) {
  stop("Missing input(s): ", paste(names(input_paths)[!file.exists(input_paths)],
                                   collapse = ", "), call. = FALSE)
}
input_md5_before <- unname(tools::md5sum(input_paths))
names(input_md5_before) <- names(input_paths)
prepared <- readRDS(input_paths[["prepared"]])
primary_full <- readRDS(input_paths[["primary_full"]])
primary <- readRDS(input_paths[["primary_assembled"]])

dx <- as.matrix(prepared$deltaX); y <- as.numeric(prepared$y)
rid <- as.character(prepared$respondent_id); task <- as.integer(prepared$task)
if (!identical(dx, as.matrix(primary$deltaX)) || !identical(y, primary$y) ||
    !identical(rid, as.character(primary$respondent_id))) {
  stop("Primary assembled rows do not align with prepared data.", call. = FALSE)
}
meta <- prepared$respondent_meta
party <- as.character(meta$party[match(rid, meta$respondent_id)])
party <- ifelse(grepl("Republican", party), "Republican", party)
if (anyNA(party) || !setequal(unique(party),
                              c("Democrat", "Independent", "Republican"))) {
  stop("Party mapping failed.", call. = FALSE)
}
z <- cbind(Republican = as.numeric(party == "Republican"),
           Independent = as.numeric(party == "Independent"))
if (!identical(as.numeric(prepared$Z_primary[, "party_Republican"]), z[, 1L]) ||
    !identical(as.numeric(prepared$Z_primary[, "party_Independent"]), z[, 2L])) {
  stop("Party labels do not align with the frozen primary moderators.",
       call. = FALSE)
}
fold <- as.integer(primary$fold_id)
if (length(fold) != nrow(dx) || max(fold) < 2L ||
    any(vapply(split(fold, rid), function(x) length(unique(x)) != 1L,
               logical(1L)))) {
  stop("Inherited outer folds are malformed or split respondents.",
       call. = FALSE)
}

grid <- .sc_gh_grid(1L, n_nodes = controls$n_nodes)
primary_prediction <- scmix_heldout_predictions(
  primary, task_order = task, include_counts = FALSE,
  include_adjacent = FALSE, include_repeated = FALSE)
if (!identical(as.character(primary_prediction$task$respondent_id), rid) ||
    !identical(as.numeric(primary_prediction$task$observed), y)) {
  stop("Primary held-out predictions are not task-row aligned.", call. = FALSE)
}

q0_scope <- .q0_scope_check(dx, y, rid, party, fold)
pooled <- .run_simple_model(
  "pooled_mean_q1", FALSE, dx, y, z, rid, fold, grid, controls,
  sw_party_gender_mean_config$bounds,
  sw_party_gender_mean_config$seed + if (cli$profile == "smoke") 1L else 2L)
targeted <- .run_simple_model(
  "targeted_q1", TRUE, dx, y, z, rid, fold, grid, controls,
  sw_party_gender_mean_config$bounds,
  sw_party_gender_mean_config$seed + if (cli$profile == "smoke") 11L else 12L)

ids <- unique(rid)
party_respondent <- party[match(ids, rid)]
scores <- list(
  primary_dnn_q1 = as.numeric(primary_prediction$sequence_loglik[ids]),
  pooled_mean_q1 = as.numeric(pooled$sequence_score[ids]),
  targeted_q1 = as.numeric(targeted$sequence_score[ids])
)
predictions <- list(
  primary_dnn_q1 = primary_prediction$task$predicted,
  pooled_mean_q1 = pooled$probability,
  targeted_q1 = targeted$probability
)
C <- .amce_contrasts(ncol(dx), colnames(dx))
amce <- .amce_comparison(dx, y, rid, party, predictions, C)
calibration <- .calibration(y, rid, party, predictions)
score_summary <- .score_summary(scores, party_respondent)
score_difference <- .paired_summary(scores, party_respondent)

party_levels <- c("Democrat", "Independent", "Republican")
z_group <- rbind(Democrat = c(0, 0), Independent = c(0, 1),
                 Republican = c(1, 0))
pooled_mu <- .sc_predict_beta(pooled$full$net, z_group)
target_mu <- .sc_predict_beta(targeted$full$net, z_group)
primary_mu <- t(vapply(party_levels, function(g) {
  colMeans(primary_full$refit$mu[party == g, , drop = FALSE])
}, numeric(ncol(dx))))
primary_grid <- primary_full$refit$integration_grid
structural_gender <- rbind(
  .structural_gender("primary_dnn_q1", party_levels, primary_mu,
                     primary_full$refit$A, primary_grid),
  .structural_gender("pooled_mean_q1", party_levels, pooled_mu,
                     pooled$full$A, grid),
  .structural_gender("targeted_q1", party_levels, target_mu,
                     targeted$full$A, grid)
)

pooled$full_compact <- .compact_fit(
  pooled$full, ids, "pooled_mean_q1_full", colnames(dx))
targeted$full_compact <- .compact_fit(
  targeted$full, ids, "targeted_q1_full", colnames(dx))
optimization <- .optimization_table(list(pooled_mean_q1 = pooled,
                                         targeted_q1 = targeted))

primary_party_range <- apply(primary_mu, 2L, function(x) diff(range(x)))
primary_range_max <- max(primary_party_range)
diagnosis <- data.frame(
  check = c(
    "task/respondent ordering", "party-label alignment",
    "primary selected regularization", "primary conditional-mean variation",
    "narrow party-gender predictive increment", "all-party-slope scope check",
    "formal inference", "outcome-blind diagnostic selection"
  ),
  status = c(
    "pass", "pass", "observed", "observed", "run_descriptive",
    "run_descriptive", "withheld", "failed_by_design"
  ),
  value = c(
    "prepared and assembled rows identical; respondents intact by fold",
    "metadata party labels exactly reproduce primary Z party dummies",
    paste0(primary_full$selected$name, "; weight_decay=",
           primary_full$selected$weight_decay),
    paste0("largest full-fit party mean range=",
           signif(primary_range_max, 6)),
    paste0("mean target-minus-pooled q1 sequence score=",
           signif(score_difference$mean_difference[
             score_difference$comparison == "targeted_minus_pooled" &
               score_difference$party == "Overall"], 6)),
    paste0("q0 all-slopes-minus-targeted=",
           signif(q0_scope$comparison$mean_difference[
             q0_scope$comparison$comparison == "all_slopes_minus_targeted"],
             6)),
    "FALSE", "FALSE"
  ),
  interpretation = c(
    "No evidence of an application row-ordering error.",
    "No evidence of a party recoding or moderator-alignment error.",
    paste(
      "The maintained penalty is applied to the DNN mean-head parameters,",
      "including output biases; this can shrink common means and Z variation."
    ),
    if (primary_range_max < 0.01) {
      "The production primary mean head is nearly pooled across party."
    } else {
      paste(
        "The diagnostic records the observed party range without calling it",
        "near zero; smoke fits are not substantively interpreted."
      )
    },
    paste(
      "This isolates two party-by-candidate-gender deviations while holding",
      "the common q=1 normal residual structure fixed."
    ),
    "Letting every slope vary by party is a higher-dimensional post-hoc check.",
    sw_party_gender_mean_config$reporting$fail_closed,
    sw_party_gender_mean_config$provenance_note
  ), stringsAsFactors = FALSE
)

all_selected_gates <- all(optimization$optimization_gate_pass[optimization$selected])
fold_isolation <- all(vapply(seq_len(max(fold)), function(k) {
  heldout <- unique(rid[fold == k])
  all(!heldout %in% pooled$folds[[k]]$training_respondents) &&
    all(!heldout %in% targeted$folds[[k]]$training_respondents)
}, logical(1L)))
gates <- data.frame(
  gate = c(
    "input alignment", "party moderator alignment", "respondent fold isolation",
    "complete finite outer predictions", "selected optimization states",
    "primary artifacts unchanged", "formal inference enabled",
    "end-to-end outcome-blind model assessment"
  ),
  pass = c(
    TRUE, TRUE, fold_isolation,
    all(vapply(predictions, function(x) all(is.finite(x)), logical(1L))) &&
      all(vapply(scores, function(x) all(is.finite(x)), logical(1L))),
    if (isTRUE(controls$require_optimization_gate)) all_selected_gates else NA,
    NA, FALSE, FALSE
  ),
  required_for_descriptive_use = c(TRUE, TRUE, TRUE, TRUE,
                                   controls$require_optimization_gate,
                                   TRUE, FALSE, FALSE),
  status = c(
    "pass", "pass", if (fold_isolation) "pass" else "fail",
    "pass", if (!controls$require_optimization_gate) "smoke_not_required" else
      if (all_selected_gates) "pass" else "fail",
    "pending_final_hash_check", "withheld", "withheld_posthoc_selection"
  ), stringsAsFactors = FALSE
)

## Strip live torch modules from the persisted evidence object.  Derived
## parameters, predictions, start diagnostics, and scores are retained.
pooled_compact <- list(
  label = pooled$label, targeted = pooled$targeted,
  probability = pooled$probability, sequence_score = pooled$sequence_score,
  folds = pooled$folds, full = pooled$full_compact,
  optimization_gate_by_fold = pooled$optimization_gate_by_fold,
  full_optimization_gate = pooled$full_optimization_gate,
  computational_gate_pass = pooled$computational_gate_pass,
  full_group_mu = pooled_mu
)
targeted_compact <- list(
  label = targeted$label, targeted = targeted$targeted,
  probability = targeted$probability,
  sequence_score = targeted$sequence_score, folds = targeted$folds,
  full = targeted$full_compact,
  optimization_gate_by_fold = targeted$optimization_gate_by_fold,
  full_optimization_gate = targeted$full_optimization_gate,
  computational_gate_pass = targeted$computational_gate_pass,
  full_group_mu = target_mu
)
rownames(pooled_compact$full_group_mu) <-
  rownames(targeted_compact$full_group_mu) <- party_levels
colnames(pooled_compact$full_group_mu) <-
colnames(targeted_compact$full_group_mu) <- colnames(dx)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
input_md5_after <- unname(tools::md5sum(input_paths))
names(input_md5_after) <- names(input_paths)
unchanged <- identical(as.character(input_md5_before),
                       as.character(input_md5_after))
gates$pass[gates$gate == "primary artifacts unchanged"] <- unchanged
gates$status[gates$gate == "primary artifacts unchanged"] <-
  if (unchanged) "pass" else "fail"
if (!unchanged) stop("A frozen input changed while the diagnostic ran.",
                     call. = FALSE)
required_gate <- gates$required_for_descriptive_use
required_pass <- gates$pass[required_gate]
descriptive_use_gate <- length(required_pass) > 0L &&
  all(!is.na(required_pass) & required_pass)
if (!descriptive_use_gate) {
  warning("One or more required descriptive-use gates failed; results must be ",
          "treated as unusable diagnostics.", call. = FALSE)
}

tables <- list(
  sequence_score_summary = score_summary,
  sequence_score_paired_differences = score_difference,
  party_calibration = calibration,
  party_amce_projection = amce,
  party_gender_structural = structural_gender,
  optimization = optimization,
  diagnostic_cause_ledger = diagnosis,
  reporting_gates = gates,
  q0_scope_check = q0_scope$comparison
)
for (nm in names(tables)) {
  .write_csv(tables[[nm]], file.path(output_dir, paste0(nm, ".csv")))
}
result <- list(
  schema_version = sw_party_gender_mean_config$schema_version,
  profile = cli$profile, configuration = sw_party_gender_mean_config,
  input_paths = input_paths, input_md5 = input_md5_before,
  sample = list(n_respondents = length(ids), n_tasks = nrow(dx), p = ncol(dx),
                party_n = table(party_respondent)),
  inherited_primary_outer_folds = TRUE,
  fold_construction_verified = fold_isolation,
  diagnostic_selection_outcome_blind = FALSE,
  formal_inference_available = FALSE,
  maintained_model = FALSE, posterior_summaries_used = FALSE,
  primary_artifacts_modified = FALSE,
  integration_grid = grid,
  pooled = pooled_compact, targeted = targeted_compact,
  primary_sequence_score = scores$primary_dnn_q1,
  q0_scope_check = q0_scope$comparison,
  tables = tables,
  interpretation = paste(
    "This artifact separates three possibilities: exact input-alignment checks",
    "address an estimator/application bug; the unpenalized pooled q=1 comparator",
    "addresses shrinkage of common means; and the two-deviation targeted-minus-",
    "pooled score addresses predictive support for the narrow party signal.",
    "Because the diagnostic was selected after seeing the mismatch, every result",
    "remains descriptive and formal inference is withheld."
  ),
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  session_info = utils::capture.output(sessionInfo())
)
.atomic_save(result, result_path)

artifact_paths <- c(result_path,
                    file.path(output_dir, paste0(names(tables), ".csv")),
                    file.path(output_dir, "reporting_gates.csv"))
artifact_paths <- unique(artifact_paths[file.exists(artifact_paths)])
manifest <- list(
  schema_version = paste0(sw_party_gender_mean_config$schema_version,
                          "-manifest"),
  profile = cli$profile, input_paths = input_paths,
  input_md5 = input_md5_after,
  artifacts = stats::setNames(unname(tools::md5sum(artifact_paths)),
                              basename(artifact_paths)),
  primary_artifacts_unchanged = unchanged,
  descriptive_use_gate = descriptive_use_gate,
  formal_inference_available = FALSE,
  maintained_model = FALSE,
  outcome_blind = FALSE,
  primary_artifacts_modified = FALSE
)
.atomic_save(manifest, file.path(output_dir, "manifest.rds"))
message("Party-gender mean diagnostic complete: ", output_dir)
