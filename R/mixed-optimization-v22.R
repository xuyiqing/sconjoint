## Double-precision polishing and scale-free projected-KKT diagnostics for the
## frozen paperps inference protocol v2.2.

.scmix_v22_objective_id <- paste0(
  "mean_negative_complete_sequence_loglik_per_respondent_",
  "plus_declared_penalty_in_nats")
.scmix_v22_polish_method <- "full_batch_lbfgs_strong_wolfe"

.scmix_net_dtype <- function(net) {
  params <- net$parameters
  if (!length(params)) {
    stop("The network has no parameters from which to determine a dtype.",
         call. = FALSE)
  }
  params[[1L]]$dtype
}

.scmix_net_dtype_label <- function(net) {
  raw <- tolower(as.character(.scmix_net_dtype(net)))
  if (grepl("double|float64", raw, fixed = FALSE)) {
    "float64"
  } else if (grepl("float32|(^|[^a-z])float([^0-9]|$)", raw,
                   perl = TRUE)) {
    "float32"
  } else raw
}

.scmix_net_device <- function(net) {
  net$parameters[[1L]]$device
}

.scmix_parameter_array <- function(parameter, gradient = FALSE) {
  tensor <- if (isTRUE(gradient)) parameter$grad else parameter$detach()
  if (is.null(tensor)) return(array(0, dim = as.integer(parameter$shape)))
  value <- torch::as_array(tensor$detach()$cpu())
  storage.mode(value) <- "double"
  value
}

.scmix_network_all_finite <- function(net) {
  state <- net$state_dict()
  length(state) > 0L && all(vapply(state, function(x) {
    all(is.finite(torch::as_array(x$detach()$cpu())))
  }, logical(1L)))
}

.scmix_box_projected_residual <- function(theta, gradient, scale,
                                          lower = -Inf, upper = Inf,
                                          objective_scale) {
  theta <- as.numeric(theta)
  gradient <- as.numeric(gradient)
  scale <- rep(as.numeric(scale), length.out = length(theta))
  u <- theta / scale
  gradient_u <- gradient * scale
  proposal <- u - gradient_u / objective_scale
  projected <- pmin(pmax(proposal, lower), upper)
  u - projected
}

.scmix_ball_projected_residual <- function(theta, gradient, radius,
                                           objective_scale) {
  theta <- as.numeric(theta)
  gradient <- as.numeric(gradient)
  proposal <- theta - gradient / objective_scale
  proposal_norm <- sqrt(sum(proposal^2))
  projected <- if (proposal_norm > radius) {
    proposal * radius / proposal_norm
  } else proposal
  theta - projected
}

## Orthogonal projection of a loading gradient onto the horizontal tangent of
## the quotient A ~ A R, R'R = I. For q = 1 the rotation group has no
## continuous tangent and the original gradient is already horizontal.
.scmix_horizontal_loading_gradient <- function(A, gradient,
                                               eigen_tolerance = 1e-12) {
  A <- as.matrix(A)
  gradient <- as.matrix(gradient)
  q <- ncol(A)
  if (q <= 1L) {
    return(list(gradient = gradient,
                vertical = matrix(0, nrow(A), q),
                omega = matrix(0, q, q),
                continuous_rotation_dimension = 0L,
                rank = if (q == 0L) 0L else as.integer(qr(A)$rank)))
  }
  M <- crossprod(A)
  eig <- eigen(M, symmetric = TRUE)
  cutoff <- eigen_tolerance * max(1, max(abs(eig$values)))
  rank <- sum(eig$values > cutoff)
  B <- crossprod(A, gradient) - crossprod(gradient, A)
  Bt <- crossprod(eig$vectors, B %*% eig$vectors)
  denom <- outer(eig$values, eig$values, `+`)
  omega_t <- matrix(0, q, q)
  use <- abs(denom) > cutoff
  omega_t[use] <- Bt[use] / denom[use]
  omega_t <- (omega_t - t(omega_t)) / 2
  omega <- eig$vectors %*% omega_t %*% t(eig$vectors)
  vertical <- A %*% omega
  orbit_dimension <- sum(upper.tri(denom) & use)
  list(gradient = gradient - vertical, vertical = vertical, omega = omega,
       continuous_rotation_dimension = as.integer(orbit_dimension),
       rank = as.integer(rank))
}

.scmix_kkt_row <- function(block, residual, gradient_scaled,
                           dimension = length(residual), applicable = TRUE,
                           scale_rule, constraint, note = "") {
  dimension <- as.integer(dimension)
  if (!isTRUE(applicable)) {
    return(data.frame(
      block = block, applicable = FALSE, dimension = dimension,
      projected_kkt_rms = NA_real_, scaled_gradient_rms = NA_real_,
      scale_rule = scale_rule, constraint = constraint, note = note,
      stringsAsFactors = FALSE))
  }
  denom <- max(1, sqrt(dimension))
  data.frame(
    block = block, applicable = TRUE, dimension = dimension,
    projected_kkt_rms = sqrt(sum(as.numeric(residual)^2)) / denom,
    scaled_gradient_rms = sqrt(sum(as.numeric(gradient_scaled)^2)) / denom,
    scale_rule = scale_rule, constraint = constraint, note = note,
    stringsAsFactors = FALSE)
}

.sc_mixed_kkt_diagnostics <- function(net, objective,
                                      coefficient_scale,
                                      kappa_bound, alpha_bound,
                                      a_bound, weight_bound) {
  if (!is.numeric(objective) || length(objective) != 1L ||
      !is.finite(objective)) {
    stop("The projected-KKT diagnostic requires one finite mean objective.",
         call. = FALSE)
  }
  p <- net$p_beta
  q <- net$q
  coefficient_scale <- .scmix_loading_scale(
    coefficient_scale, p, "coefficient_scale")
  objective_scale <- max(1, abs(objective))
  params <- net$parameters
  pieces <- list()
  residuals <- list()

  ## The optimizer stores an unconstrained coordinate and maps it into the
  ## declared position-effect interval with tanh. Diagnose stationarity in
  ## the interpretable position-effect coordinate. Near saturation, invert the
  ## transform only down to a numerical floor so a vanishing chain-rule factor
  ## cannot make a large structural derivative look small.
  theta_k <- .scmix_parameter_array(params$kappa_raw)
  grad_k <- .scmix_parameter_array(params$kappa_raw, gradient = TRUE)
  kappa <- kappa_bound * tanh(theta_k / kappa_bound)
  derivative_kappa <- 1 / cosh(theta_k / kappa_bound)^2
  grad_kappa <- grad_k / pmax(derivative_kappa, sqrt(.Machine$double.eps))
  residuals$position_effect <- .scmix_box_projected_residual(
    kappa, grad_kappa, scale = kappa_bound, lower = -1, upper = 1,
    objective_scale = objective_scale)
  grad_scaled_k <- as.numeric(grad_kappa) * kappa_bound / objective_scale
  pieces$position_effect <- .scmix_kkt_row(
    "position_effect", residuals$position_effect, grad_scaled_k,
    scale_rule = "position effect divided by declared kappa_bound",
    constraint = "declared position-effect interval")

  if ("alpha_raw" %in% names(params)) {
    theta_alpha <- .scmix_parameter_array(params$alpha_raw)
    grad_alpha <- .scmix_parameter_array(params$alpha_raw, gradient = TRUE)
    residuals$unpenalized_mean_reference <- .scmix_box_projected_residual(
      theta_alpha, grad_alpha, scale = coefficient_scale * alpha_bound,
      lower = -1, upper = 1,
      objective_scale = objective_scale)
    pieces$unpenalized_mean_reference <- .scmix_kkt_row(
      "unpenalized_mean_reference",
      residuals$unpenalized_mean_reference,
      as.numeric(grad_alpha) * rep(coefficient_scale * alpha_bound,
                                   length.out = length(grad_alpha)) /
        objective_scale,
      scale_rule = paste(
        "optimization alpha divided by training contrast scale and",
        "declared raw alpha bound"),
      constraint = "unit box after raw-coordinate normalization")
  } else {
    residuals$unpenalized_mean_reference <- numeric()
    pieces$unpenalized_mean_reference <- .scmix_kkt_row(
      "unpenalized_mean_reference", numeric(), numeric(), dimension = 0L,
      applicable = FALSE, scale_rule = "not available in legacy mean family",
      constraint = "not applicable",
      note = "Legacy all-mean parameterization is not v2.2 paper-eligible.")
  }

  deviation_names <- setdiff(names(params), c("A", "kappa_raw", "alpha_raw"))
  deviation_residual <- numeric()
  deviation_gradient <- numeric()
  corrected_output <- !is.null(net$mean_family) &&
    !identical(net$mean_family, "legacy")
  for (nm in deviation_names) {
    theta <- .scmix_parameter_array(params[[nm]])
    gradient <- .scmix_parameter_array(params[[nm]], gradient = TRUE)
    parameter_scale <- if (corrected_output &&
        identical(nm, "param_layer.weight")) {
      matrix(coefficient_scale * weight_bound, nrow = p,
             ncol = ncol(as.matrix(theta)))
    } else {
      weight_bound
    }
    deviation_residual <- c(
      deviation_residual,
      .scmix_box_projected_residual(
        theta, gradient, scale = parameter_scale, lower = -1,
        upper = 1, objective_scale = objective_scale))
    deviation_gradient <- c(deviation_gradient,
                            as.numeric(gradient) *
                              rep(as.numeric(parameter_scale),
                                  length.out = length(gradient)) /
                              objective_scale)
  }
  residuals$centered_mean_deviation <- deviation_residual
  pieces$centered_mean_deviation <- .scmix_kkt_row(
    "centered_mean_deviation", deviation_residual, deviation_gradient,
    dimension = length(deviation_residual),
    applicable = length(deviation_names) > 0L,
    scale_rule = paste(
      "hidden parameters divided by the declared weight bound; corrected",
      "output-map rows additionally divided by their training contrast scale"),
    constraint = "unit box after optimizer-coordinate normalization",
    note = if (!length(deviation_names))
      "Constant mean family has no centered deviation coordinates." else "")

  horizontal <- NULL
  if (q > 0L) {
    A_optimization <- .scmix_parameter_array(params$A)
    dim(A_optimization) <- c(p, q)
    grad_optimization <- .scmix_parameter_array(params$A, gradient = TRUE)
    dim(grad_optimization) <- c(p, q)
    A_raw <- .scmix_loading_to_raw(A_optimization, coefficient_scale)
    grad_raw <- sweep(grad_optimization, 1L, coefficient_scale, `*`)
    horizontal <- .scmix_horizontal_loading_gradient(A_raw, grad_raw)
    residuals$horizontal_loadings <- .scmix_ball_projected_residual(
      A_raw / a_bound, horizontal$gradient * a_bound, radius = 1,
      objective_scale = objective_scale)
    horizontal_dimension <- p * q - horizontal$continuous_rotation_dimension
    pieces$horizontal_loadings <- .scmix_kkt_row(
      "horizontal_loadings", residuals$horizontal_loadings,
      as.numeric(horizontal$gradient) * a_bound / objective_scale,
      dimension = horizontal_dimension,
      scale_rule = paste(
        "loading and gradient mapped to raw coefficient units, divided by",
        "the declared Frobenius bound, and rotational tangent removed"),
      constraint = "unit Frobenius ball after raw-unit normalization",
      note = paste0("numerical loading rank ", horizontal$rank, " of ", q))
  } else {
    residuals$horizontal_loadings <- numeric()
    pieces$horizontal_loadings <- .scmix_kkt_row(
      "horizontal_loadings", numeric(), numeric(), dimension = 0L,
      applicable = FALSE, scale_rule = "q = 0", constraint = "not applicable")
  }

  structural_names <- c("position_effect", "unpenalized_mean_reference",
                        "centered_mean_deviation", "horizontal_loadings")
  structural_rows <- lapply(structural_names, function(nm) pieces[[nm]])
  structural_applicable <- vapply(structural_rows, `[[`, logical(1L),
                                  "applicable")
  structural_dimension <- sum(vapply(
    structural_rows[structural_applicable], `[[`, integer(1L), "dimension"))
  structural_residual <- unlist(
    residuals[structural_names[structural_applicable]], use.names = FALSE)
  structural_gradient_norm_sq <- sum(vapply(
    structural_rows[structural_applicable], function(x) {
      (x$scaled_gradient_rms * max(1, sqrt(x$dimension)))^2
    }, numeric(1L)))
  pieces$all_structural_coordinates <- data.frame(
    block = "all_structural_coordinates", applicable = TRUE,
    dimension = as.integer(structural_dimension),
    projected_kkt_rms = sqrt(sum(structural_residual^2)) /
      max(1, sqrt(structural_dimension)),
    scaled_gradient_rms = sqrt(structural_gradient_norm_sq) /
      max(1, sqrt(structural_dimension)),
    scale_rule = paste(
      "joint norm of position, reference-mean, centered-deviation, and",
      "horizontal-loading blocks"),
    constraint = "product of declared block constraints", note = "",
    stringsAsFactors = FALSE)

  block_order <- c("position_effect", "unpenalized_mean_reference",
                   "centered_mean_deviation", "horizontal_loadings",
                   "all_structural_coordinates")
  table <- do.call(rbind, pieces[block_order])
  applicable <- table$applicable
  list(
    metric = "block_scaled_projected_gradient_rms",
    objective = objective,
    objective_definition = paste(
      "mean negative complete-sequence log likelihood per respondent plus",
      "the declared mean-deviation penalty, in nats"),
    objective_scale = objective_scale,
    parameterization = "theta_block = S_block * u_block",
    blocks = table,
    maximum = max(table$projected_kkt_rms[applicable]),
    all_finite = all(is.finite(table$projected_kkt_rms[applicable])),
    loading_quotient = horizontal,
    raw_residual_vectors = residuals)
}

.scmix_clone_network_state <- function(net) {
  state <- lapply(net$state_dict(), function(x) {
    value <- torch::as_array(x$detach()$cpu())
    storage.mode(value) <- "double"
    value
  })
  state
}

.scmix_load_network_state <- function(net, state) {
  target <- net$state_dict()
  if (!identical(names(target), names(state))) {
    stop("Saved and target network states have different fields.",
         call. = FALSE)
  }
  restored <- lapply(names(target), function(nm) {
    value <- torch::torch_tensor(
      state[[nm]], dtype = target[[nm]]$dtype, device = target[[nm]]$device)
    if (!identical(as.integer(value$shape), as.integer(target[[nm]]$shape))) {
      stop("Saved and target tensor shapes differ for `", nm, "`.",
           call. = FALSE)
    }
    value
  })
  names(restored) <- names(target)
  torch::with_no_grad({
    for (nm in names(target)) target[[nm]]$copy_(restored[[nm]])
  })
  invisible(net)
}

.sc_mixed_evaluate_full_objective <- function(net, data, U_t, logw_t,
                                              weight_decay,
                                              coefficient_scale,
                                              kappa_bound, alpha_bound,
                                              a_bound, weight_bound,
                                              backward = TRUE) {
  net$eval()
  net$zero_grad()
  nll <- .sc_mixed_nll(net, data$dx, data$zt, data$yt, U_t, logw_t,
                       data$idx1, data$N)
  loss <- nll + .sc_mixed_penalty(net, weight_decay)
  if (isTRUE(backward)) loss$backward()
  objective <- as.numeric(loss$item())
  unpenalized <- as.numeric(nll$item())
  gradient <- if (isTRUE(backward)) .sc_mixed_gradient_diagnostics(net) else NULL
  kkt <- if (isTRUE(backward) && is.finite(objective)) {
    .sc_mixed_kkt_diagnostics(
      net, objective = objective, coefficient_scale = coefficient_scale,
      kappa_bound = kappa_bound, alpha_bound = alpha_bound,
      a_bound = a_bound, weight_bound = weight_bound)
  } else NULL
  list(loss = loss, objective = objective, unpenalized = unpenalized,
       penalty = objective - unpenalized,
       objective_definition_id = .scmix_v22_objective_id,
       gradient = gradient, kkt = kkt)
}

.sc_mixed_lbfgs_polish <- function(net, data, U_t, logw_t, weight_decay,
                                   coefficient_scale,
                                   kappa_bound, alpha_bound,
                                   a_bound, weight_bound,
                                   max_iter = 50L, history_size = 50L,
                                   tolerance_grad = 1e-10,
                                   tolerance_change = 1e-12,
                                   lr = 1,
                                   same_objective_all_stages = TRUE) {
  max_iter <- as.integer(max_iter)
  history_size <- as.integer(history_size)
  if (is.na(max_iter) || max_iter < 1L || is.na(history_size) ||
      history_size < 1L || !is.numeric(tolerance_grad) ||
      length(tolerance_grad) != 1L || !is.finite(tolerance_grad) ||
      tolerance_grad < 0 || !is.numeric(tolerance_change) ||
      length(tolerance_change) != 1L || !is.finite(tolerance_change) ||
      tolerance_change < 0 || !is.numeric(lr) || length(lr) != 1L ||
      !is.finite(lr) || lr <= 0 ||
      !is.logical(same_objective_all_stages) ||
      length(same_objective_all_stages) != 1L ||
      is.na(same_objective_all_stages)) {
    stop("Invalid full-batch L-BFGS polish controls.", call. = FALSE)
  }
  dtype <- .scmix_net_dtype_label(net)
  if (!identical(dtype, "float64")) {
    stop("Protocol v2.2 polishing requires float64 network parameters.",
         call. = FALSE)
  }
  before <- .sc_mixed_evaluate_full_objective(
    net, data, U_t, logw_t, weight_decay, coefficient_scale,
    kappa_bound, alpha_bound, a_bound, weight_bound, backward = TRUE)
  snapshot <- function(x) torch::as_array(x$detach()$cpu())
  rebuild <- function(value, template) {
    torch::torch_tensor(value, dtype = template$dtype,
                        device = template$device)
  }
  data_snapshot <- list(
    dx = snapshot(data$dx), zt = snapshot(data$zt),
    yt = snapshot(data$yt), idx1 = snapshot(data$idx1), N = data$N)
  U_snapshot <- snapshot(U_t)
  logw_snapshot <- snapshot(logw_t)
  ## Optimize a deep module clone. Some libtorch line-search failures can leave
  ## their parameter tensors nonfinite before raising. Isolation guarantees
  ## that the caller's feasible attained state remains recoverable.
  work <- torch::clone_module(net, deep = TRUE)
  work_data <- list(
    dx = rebuild(data_snapshot$dx, data$dx),
    zt = rebuild(data_snapshot$zt, data$zt),
    yt = rebuild(data_snapshot$yt, data$yt),
    idx1 = rebuild(data_snapshot$idx1, data$idx1),
    N = data$N)
  work_U_t <- rebuild(U_snapshot, U_t)
  work_logw_t <- rebuild(logw_snapshot, logw_t)

  optimizer <- torch::optim_lbfgs(
    work$parameters, lr = lr, max_iter = max_iter,
    tolerance_grad = tolerance_grad,
    tolerance_change = tolerance_change, history_size = history_size,
    line_search_fn = "strong_wolfe")
  closure_calls <- 0L
  closure <- function() {
    closure_calls <<- closure_calls + 1L
    optimizer$zero_grad()
    loss <- .sc_mixed_nll(
      work, work_data$dx, work_data$zt, work_data$yt,
      work_U_t, work_logw_t, work_data$idx1, work_data$N) +
      .sc_mixed_penalty(work, weight_decay)
    loss$backward()
    loss
  }
  error_message <- NULL
  completed <- tryCatch({
    optimizer$step(closure)
    TRUE
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    FALSE
  })

  iterations <- function_evaluations <- NA_integer_
  if (completed) {
    state <- tryCatch(optimizer$state$get(work$parameters[[1L]]),
                      error = function(e) NULL)
    if (!is.null(state)) {
      iterations <- as.integer(state$n_iter)
      function_evaluations <- as.integer(state$func_evals)
    }
  }
  ## Release the optimizer and closure before auditing its endpoint. The R
  ## torch binding can retain temporary strong-Wolfe trial state until these
  ## references are finalized; the production diagnostic must describe the
  ## settled module that will actually be returned.
  closure <- NULL
  optimizer <- NULL
  invisible(gc(verbose = FALSE))

  projection_changed <- NA
  attempted <- NULL
  if (completed) {
    pre_projection <- lapply(work$parameters, .scmix_parameter_array)
    .sc_mixed_project_parameters(
      work, coefficient_scale = coefficient_scale,
      alpha_bound = alpha_bound, a_bound = a_bound,
      weight_bound = weight_bound)
    post_projection <- lapply(work$parameters, .scmix_parameter_array)
    projection_changed <- any(vapply(seq_along(pre_projection), function(j) {
      !isTRUE(all.equal(pre_projection[[j]], post_projection[[j]],
                        tolerance = 100 * .Machine$double.eps,
                        check.attributes = FALSE))
    }, logical(1L)))
    attempted <- tryCatch(
      .sc_mixed_evaluate_full_objective(
        work, work_data, work_U_t, work_logw_t, weight_decay,
        coefficient_scale,
        kappa_bound, alpha_bound, a_bound, weight_bound, backward = TRUE),
      error = function(e) {
        error_message <<- paste(c(error_message, conditionMessage(e)),
                                collapse = "; ")
        NULL
      })
  }
  after_objective <- if (is.null(attempted)) NA_real_ else attempted$objective
  numerical_allowance <- 100 * .Machine$double.eps *
    max(1, abs(before$objective))
  nonworsening <- completed && is.finite(after_objective) &&
    after_objective <= before$objective + numerical_allowance
  ## A line-search endpoint that must be projected is not an L-BFGS endpoint
  ## for the declared constrained problem. Restore the feasible pre-polish
  ## state and fail the v2.2 gate rather than certifying the projected point.
  accepted <- isTRUE(completed) && isTRUE(nonworsening) && !is.null(attempted) &&
    identical(projection_changed, FALSE)
  ## Detach an accepted endpoint from the optimizer object before that object
  ## is finalized. This avoids any backend-specific lifetime coupling between
  ## the L-BFGS state and the module returned to the caller.
  returned_net <- if (accepted) {
    torch::clone_module(work, deep = TRUE)
  } else net
  returned_data <- list(
    dx = rebuild(data_snapshot$dx, data$dx),
    zt = rebuild(data_snapshot$zt, data$zt),
    yt = rebuild(data_snapshot$yt, data$yt),
    idx1 = rebuild(data_snapshot$idx1, data$idx1),
    N = data$N)
  returned_U_t <- rebuild(U_snapshot, U_t)
  returned_logw_t <- rebuild(logw_snapshot, logw_t)
  returned <- .sc_mixed_evaluate_full_objective(
    returned_net, returned_data, returned_U_t, returned_logw_t,
    weight_decay, coefficient_scale,
    kappa_bound, alpha_bound, a_bound, weight_bound, backward = TRUE)
  returned_matches_accepted <- !accepted ||
    (is.finite(returned$objective) &&
     abs(returned$objective - after_objective) <= numerical_allowance)
  pre_settlement_warning <- NULL
  if (!returned_matches_accepted) {
    accepted <- FALSE
    pre_settlement_warning <- paste(
      "L-BFGS endpoint changed before optimizer finalization;",
      "the caller must use the settled-state audit")
    returned_net <- net
    returned <- .sc_mixed_evaluate_full_objective(
      returned_net, returned_data, returned_U_t, returned_logw_t,
      weight_decay, coefficient_scale,
      kappa_bound, alpha_bound, a_bound, weight_bound, backward = TRUE)
  }

  list(
    ## The caller performs one final settled-state audit after this optimizer
    ## object leaves scope. Preserve the attempted module for that audit even
    ## when the preliminary reproduction check above fails.
    net = if (completed) work else net,
    fallback_net = net,
    protocol_id = "paperps-inference-2026-09-06-v2.3",
    dtype = dtype,
    objective_definition = paste(
      "mean negative complete-sequence log likelihood per respondent plus",
      "the declared mean-deviation penalty, in nats"),
    objective_definition_id = .scmix_v22_objective_id,
    same_objective_all_stages = isTRUE(same_objective_all_stages),
    method = "full_batch_lbfgs",
    line_search = "strong_wolfe",
    optimization_method_id = .scmix_v22_polish_method,
    line_search_status = if (!completed) {
      paste0("optimizer_error: ", error_message)
    } else if (!is.finite(after_objective)) {
      "optimizer_returned_nonfinite_objective"
    } else "optimizer_returned_strong_wolfe",
    completed = completed,
    accepted = accepted,
    objective_before_polish = before$objective,
    objective_after_polish = after_objective,
    pre_settlement_objective_after_polish = after_objective,
    pre_settlement_warning = pre_settlement_warning,
    objective_returned = returned$objective,
    returned_matches_accepted_objective = returned_matches_accepted,
    unpenalized_objective_before_polish = before$unpenalized,
    unpenalized_objective_after_polish = if (is.null(attempted)) NA_real_ else
      attempted$unpenalized,
    kkt_before_polish = before$kkt,
    kkt_after_polish = if (is.null(attempted)) NULL else attempted$kkt,
    kkt_returned = returned$kkt,
    absolute_gradient_returned = returned$gradient,
    lbfgs_nonworsening = nonworsening,
    nonworsening_numerical_allowance = numerical_allowance,
    projection_changed = projection_changed,
    iterations = iterations,
    function_evaluations = function_evaluations,
    closure_calls = closure_calls,
    max_iter = max_iter,
    history_size = history_size,
    tolerance_grad_optimizer_only = tolerance_grad,
    tolerance_change_optimizer_only = tolerance_change,
    lr = lr,
    error = error_message)
}

.sc_v22_protocol_mode <- function(protocol_mode = c("calibration",
                                                     "production")) {
  match.arg(protocol_mode)
}

.sc_v22_resolve_optimizer_calibration <- function(
    protocol_mode, calibration_artifact = NULL, analysis_contract = NULL,
    kkt_tol = NULL, nested_fit_relative_cutoff = NULL) {
  protocol_mode <- .sc_v22_protocol_mode(protocol_mode)
  if (identical(protocol_mode, "calibration")) {
    return(list(
      verified = FALSE, mode = protocol_mode,
      status = "calibration_only",
      failure_reason = "calibration_mode_cannot_certify",
      kkt_cutoff = kkt_tol,
      nested_fit_relative_cutoff = nested_fit_relative_cutoff,
      loading_probability_invariance_cutoff = NULL,
      calibration_hash = NA_character_))
  }
  ## Numeric production cutoffs are not an authorization path. They must come
  ## from the protocol-linked artifact validated by the inference contract.
  if (!is.null(kkt_tol) || !is.null(nested_fit_relative_cutoff)) {
    return(list(
      verified = FALSE, mode = protocol_mode,
      status = "direct_numeric_cutoff_rejected",
      failure_reason = "production_cutoff_not_from_calibration_artifact",
      kkt_cutoff = NULL, nested_fit_relative_cutoff = NULL,
      loading_probability_invariance_cutoff = NULL,
      calibration_hash = NA_character_))
  }
  validator <- get0(".scmix_v22_validate_calibration", mode = "function",
                    inherits = TRUE)
  if (is.null(validator) ||
      !inherits(calibration_artifact, "scmix_v22_calibration_artifact") ||
      is.null(analysis_contract)) {
    return(list(
      verified = FALSE, mode = protocol_mode,
      status = "calibration_artifact_unavailable",
      failure_reason = "production_calibration_artifact_not_verified",
      kkt_cutoff = NULL, nested_fit_relative_cutoff = NULL,
      loading_probability_invariance_cutoff = NULL,
      calibration_hash = NA_character_))
  }
  valid <- isTRUE(tryCatch(
    validator(calibration_artifact, analysis_contract,
              require_complete = TRUE),
    error = function(e) FALSE))
  constants <- calibration_artifact$constants
  required <- c("kkt_cutoff", "nested_fit_relative_cutoff",
                "loading_probability_invariance_cutoff")
  constants_ok <- is.numeric(constants) && !is.null(names(constants)) &&
    all(required %in% names(constants)) &&
    all(is.finite(constants[required])) && all(constants[required] >= 0)
  if (!valid || !constants_ok) {
    return(list(
      verified = FALSE, mode = protocol_mode,
      status = "calibration_artifact_invalid_or_incomplete",
      failure_reason = "production_calibration_artifact_not_verified",
      kkt_cutoff = NULL, nested_fit_relative_cutoff = NULL,
      loading_probability_invariance_cutoff = NULL,
      calibration_hash = NA_character_))
  }
  list(
    verified = TRUE, mode = protocol_mode, status = "verified",
    failure_reason = character(),
    kkt_cutoff = unname(constants[["kkt_cutoff"]]),
    nested_fit_relative_cutoff =
      unname(constants[["nested_fit_relative_cutoff"]]),
    loading_probability_invariance_cutoff =
      unname(constants[["loading_probability_invariance_cutoff"]]),
    calibration_hash = as.character(calibration_artifact$calibration_hash))
}

.sc_v22_require_verified_production_calibration <- function(x) {
  if (identical(x$mode, "production") && !isTRUE(x$verified)) {
    stop(
      "Protocol v2.2 production mode requires a complete protocol-linked ",
      "calibration artifact validated against the signed analysis contract ",
      "before fitting. Status: ", x$status %||% "unavailable", ".",
      call. = FALSE)
  }
  invisible(x)
}

.sc_mixed_v22_optimization_status <- function(
    polish, bounds, kkt_cutoff = NULL,
    protocol_mode = c("calibration", "production"),
    calibration_verified = FALSE,
    calibration_failure_reason = character()) {
  protocol_mode <- .sc_v22_protocol_mode(protocol_mode)
  calibration_verified <- isTRUE(calibration_verified)
  cutoff_available <- !is.null(kkt_cutoff)
  if (cutoff_available &&
      (!is.numeric(kkt_cutoff) || length(kkt_cutoff) != 1L ||
       !is.finite(kkt_cutoff) || kkt_cutoff < 0)) {
    stop("`kkt_cutoff` must be NULL or one finite nonnegative value.",
         call. = FALSE)
  }
  kkt <- polish$kkt_returned
  objective_finite <- is.finite(polish$objective_returned)
  kkt_finite <- is.list(kkt) && isTRUE(kkt$all_finite) &&
    is.finite(kkt$maximum)
  mean_reference_available <- kkt_finite && any(
    kkt$blocks$block == "unpenalized_mean_reference" &
      kkt$blocks$applicable)
  stationarity_met <- cutoff_available && kkt_finite &&
    kkt$maximum <= kkt_cutoff
  required_bound_flags <- c("mu_active", "kappa_active", "a_active",
                            "weight_active")
  if (isTRUE(bounds$alpha_diagnostics_applicable)) {
    required_bound_flags <- c(required_bound_flags, "alpha_active")
  }
  bound_complete <- is.list(bounds) &&
    all(required_bound_flags %in% names(bounds)) &&
    all(vapply(bounds[required_bound_flags], function(x) {
      is.logical(x) && length(x) == 1L && !is.na(x)
    }, logical(1L)))
  bound_activity <- !bound_complete ||
    any(unlist(bounds[required_bound_flags], use.names = FALSE))
  objective_contract_exact <-
    identical(polish$objective_definition_id, .scmix_v22_objective_id)
  polish_contract_exact <-
    identical(polish$optimization_method_id, .scmix_v22_polish_method) &&
    identical(polish$method, "full_batch_lbfgs") &&
    identical(polish$line_search, "strong_wolfe")
  required_artifacts_present <-
    identical(polish$protocol_id, "paperps-inference-2026-09-06-v2.3") &&
    objective_contract_exact && polish_contract_exact &&
    is.character(polish$dtype) && length(polish$dtype) == 1L &&
    is.numeric(polish$objective_before_polish) &&
    length(polish$objective_before_polish) == 1L &&
    is.numeric(polish$objective_after_polish) &&
    length(polish$objective_after_polish) == 1L &&
    is.list(polish$kkt_before_polish) && is.list(polish$kkt_after_polish) &&
    is.numeric(polish$iterations) && length(polish$iterations) == 1L &&
    !is.na(polish$iterations) &&
    is.character(polish$line_search_status) &&
    length(polish$line_search_status) == 1L &&
    !is.na(polish$line_search_status) && nzchar(polish$line_search_status) &&
    is.logical(polish$lbfgs_nonworsening) &&
    length(polish$lbfgs_nonworsening) == 1L &&
    !is.na(polish$lbfgs_nonworsening) &&
    is.logical(polish$returned_matches_accepted_objective) &&
    length(polish$returned_matches_accepted_objective) == 1L &&
    isTRUE(polish$returned_matches_accepted_objective) &&
    mean_reference_available
  absolute_gradient <- unlist(
    polish$absolute_gradient_returned$by_parameter, use.names = TRUE)
  derivatives_finite <- length(absolute_gradient) > 0L &&
    all(is.finite(absolute_gradient)) && kkt_finite
  polish_gate_pass <- required_artifacts_present &&
    isTRUE(polish$completed) && isTRUE(polish$accepted) &&
    identical(polish$projection_changed, FALSE) &&
    isTRUE(polish$lbfgs_nonworsening) &&
    isTRUE(polish$same_objective_all_stages) &&
    isTRUE(polish$returned_matches_accepted_objective)
  failure <- character()
  if (!required_artifacts_present) {
    failure <- c(failure, "missing_explicit_v22_optimizer_evidence")
  }
  if (!identical(polish$dtype, "float64")) failure <- c(failure, "dtype_not_float64")
  if (!isTRUE(polish$completed)) failure <- c(failure, "lbfgs_polish_not_completed")
  if (!isTRUE(polish$accepted)) failure <- c(failure, "lbfgs_polish_not_accepted")
  if (isTRUE(polish$projection_changed)) {
    failure <- c(failure, "lbfgs_endpoint_required_projection")
  }
  if (!isTRUE(polish$lbfgs_nonworsening)) failure <- c(failure, "lbfgs_polish_worsened_objective")
  if (!isTRUE(polish$same_objective_all_stages)) failure <- c(failure, "optimizer_objective_changed")
  if (!objective_contract_exact) failure <- c(failure, "optimizer_objective_definition_mismatch")
  if (!polish_contract_exact) failure <- c(failure, "optimizer_polish_method_mismatch")
  if (!isTRUE(polish$returned_matches_accepted_objective)) {
    failure <- c(failure, "returned_state_objective_mismatch")
  }
  if (!objective_finite) failure <- c(failure, "nonfinite_objective")
  if (!derivatives_finite) failure <- c(failure, "nonfinite_derivatives")
  if (!kkt_finite) failure <- c(failure, "nonfinite_or_missing_projected_kkt")
  if (!mean_reference_available) {
    failure <- c(failure, "missing_unpenalized_mean_reference_block")
  }
  if (!cutoff_available) {
    failure <- c(failure, "kkt_cutoff_calibration_pending")
  } else if (kkt_finite && !stationarity_met) {
    failure <- c(failure, "projected_kkt_cutoff_not_met")
  }
  if (!bound_complete) failure <- c(failure, "compact_bound_diagnostics_incomplete")
  else if (bound_activity) failure <- c(failure, "parameter_bound_active")
  if (!identical(protocol_mode, "production")) {
    failure <- c(failure, "calibration_artifact_not_production_eligible")
  } else if (!calibration_verified) {
    failure <- c(failure,
                 if (length(calibration_failure_reason)) {
                   as.character(calibration_failure_reason)
                 } else "production_calibration_artifact_not_verified")
  }
  base_failure <- setdiff(
    failure,
    c("kkt_cutoff_calibration_pending", "projected_kkt_cutoff_not_met",
      "calibration_artifact_not_production_eligible",
      ## Legacy means remain operational for reproducibility, but can never
      ## receive v2.2 paper eligibility without the unpenalized reference block.
      "missing_unpenalized_mean_reference_block"))
  precalibration_fit_valid <- !length(base_failure) && kkt_finite
  pass <- !length(failure)
  blocks <- if (kkt_finite) kkt$blocks else NULL
  block_value <- function(name) {
    if (is.null(blocks)) return(NA_real_)
    hit <- blocks$block == name & blocks$applicable
    if (!any(hit)) NA_real_ else blocks$projected_kkt_rms[which(hit)[1L]]
  }
  gate_evidence <- list(
    evidence_source = paste(
      "deterministically constructed from the optimizer artifact and compact",
      "bound diagnostics; no user-supplied pass flags"),
    F001 = list(
      statistic = "explicit_v2_2_evidence_present",
      observed = required_artifacts_present, threshold = TRUE,
      objective_definition_id = polish$objective_definition_id,
      required_objective_definition_id = .scmix_v22_objective_id,
      optimization_method_id = polish$optimization_method_id,
      required_optimization_method_id = .scmix_v22_polish_method,
      same_objective_all_stages =
        isTRUE(polish$same_objective_all_stages),
      pass = required_artifacts_present,
      state = if (required_artifacts_present) "pass" else "fail"),
    F002 = list(
      statistic = "dtype_is_float64",
      observed = identical(polish$dtype, "float64"), threshold = TRUE,
      pass = identical(polish$dtype, "float64"),
      state = if (identical(polish$dtype, "float64")) "pass" else "fail"),
    F003 = list(
      statistic = "objective_and_derivatives_finite",
      observed = objective_finite && derivatives_finite, threshold = TRUE,
      pass = objective_finite && derivatives_finite,
      state = if (objective_finite && derivatives_finite) "pass" else "fail"),
    F004 = list(
      statistic = "lbfgs_polish_completed_and_nonworsening",
      observed = polish_gate_pass,
      threshold = TRUE,
      objective_definition_id = polish$objective_definition_id,
      optimization_method_id = polish$optimization_method_id,
      completed = isTRUE(polish$completed),
      accepted = isTRUE(polish$accepted),
      projection_changed = polish$projection_changed,
      nonworsening = isTRUE(polish$lbfgs_nonworsening),
      same_objective_all_stages = isTRUE(polish$same_objective_all_stages),
      returned_matches_accepted_objective =
        isTRUE(polish$returned_matches_accepted_objective),
      pass = polish_gate_pass,
      state = if (polish_gate_pass) {
        "pass"
      } else "fail"),
    F005 = list(
      statistic = "maximum_block_scaled_projected_kkt",
      observed = if (kkt_finite) kkt$maximum else NA_real_,
      observed_by_block = blocks,
      threshold = if (cutoff_available) kkt_cutoff else
        "CALIBRATE_IN_STEP_3",
      pass = identical(protocol_mode, "production") &&
        calibration_verified && cutoff_available && stationarity_met,
      state = if (identical(protocol_mode, "calibration")) {
        "not_run"
      } else if (!calibration_verified) {
        "fail"
      } else if (cutoff_available) {
        if (stationarity_met) "pass" else "fail"
      } else "fail"),
    F006 = list(
      statistic = "any_artificial_bound_active",
      observed = if (bound_complete) bound_activity else NA,
      threshold = FALSE,
      pass = bound_complete && !bound_activity,
      state = if (bound_complete && !bound_activity) "pass" else "fail"))
  list(
    optimization_gate_pass = pass,
    protocol_mode = protocol_mode,
    calibration_only = identical(protocol_mode, "calibration"),
    calibration_verified = calibration_verified,
    optimizer_subset_pass = pass && identical(protocol_mode, "production"),
    precalibration_fit_valid = precalibration_fit_valid,
    precalibration_failure_reasons = unique(base_failure),
    objective_finite = objective_finite,
    dtype_is_float64 = identical(polish$dtype, "float64"),
    lbfgs_completed = isTRUE(polish$completed),
    lbfgs_accepted = isTRUE(polish$accepted),
    lbfgs_projection_changed = isTRUE(polish$projection_changed),
    lbfgs_nonworsening = isTRUE(polish$lbfgs_nonworsening),
    same_objective_all_stages = isTRUE(polish$same_objective_all_stages),
    kkt_cutoff = if (cutoff_available) kkt_cutoff else NA_real_,
    kkt_cutoff_available = cutoff_available,
    kkt_maximum = if (kkt_finite) kkt$maximum else NA_real_,
    required_mean_reference_block = mean_reference_available,
    stationarity_met = stationarity_met,
    structural_stationarity_met = is.finite(block_value(
      "all_structural_coordinates")) &&
      cutoff_available && block_value("all_structural_coordinates") <= kkt_cutoff,
    alpha_stationarity_met = is.na(block_value(
      "unpenalized_mean_reference")) ||
      (cutoff_available &&
       block_value("unpenalized_mean_reference") <= kkt_cutoff),
    sieve_stationarity_met = is.na(block_value("centered_mean_deviation")) ||
      (cutoff_available &&
       block_value("centered_mean_deviation") <= kkt_cutoff),
    bound_diagnostics_complete = bound_complete,
    bound_activity = bound_activity,
    gate_evidence = gate_evidence,
    failure_reasons = unique(failure))
}
