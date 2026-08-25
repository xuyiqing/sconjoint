## Computation and tuning orchestration for the paperps mixed-logit estimator.
##
## This file does not define another statistical model. It organizes fitting,
## validation, and numerical sensitivity for the low-rank normal mixed logit
## implemented in mixed-likelihood.R. Respondents are always the splitting and
## scoring units.

.sc_comp_seed <- function(seed, ...) {
  if (is.null(seed)) return(NULL)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be NULL or one finite number.", call. = FALSE)
  }
  tag <- paste(..., collapse = "|")
  code <- utf8ToInt(tag)
  offset <- if (length(code)) sum(as.double(code) * seq_along(code)) else 0
  as.integer((as.double(seed) + offset) %% (.Machine$integer.max - 1) + 1)
}

#' Respondent-weighted training scale for conjoint contrasts
#'
#' The scale of coordinate k is the square root of the mean, across
#' respondents, of the respondent-specific mean of deltaX_itk squared. It is
#' estimated on training respondents only and does not center randomized
#' contrasts, so rescaling coefficients preserves every utility index.
#'
#' @keywords internal
#' @noRd
.sc_comp_fit_dx_scale <- function(deltaX, respondent_id) {
  deltaX <- as.matrix(deltaX)
  if (!is.numeric(deltaX) || any(!is.finite(deltaX)) ||
      nrow(deltaX) != length(respondent_id) || nrow(deltaX) < 1L) {
    stop("`deltaX` must be a finite numeric matrix matching `respondent_id`.",
         call. = FALSE)
  }
  ids <- unique(as.character(respondent_id))
  ri <- match(as.character(respondent_id), ids)
  count <- tabulate(ri, nbins = length(ids))
  mean_sq_i <- rowsum(deltaX^2, ri, reorder = FALSE) / count
  scale <- sqrt(colMeans(mean_sq_i))
  constant <- !is.finite(scale) | scale < 1e-12
  scale[constant] <- 1
  names(scale) <- colnames(deltaX)
  list(scale = scale, constant = constant,
       n_respondents = length(ids),
       weighting = "equal respondent weight after within-respondent averaging",
       centering = "none")
}

.sc_comp_nonempty_signature <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

.sc_comp_signature_spec <- function(x) {
  ## Integration resolution and scramble are intentionally excluded: the
  ## refinement workflow changes those while holding the statistical analysis
  ## fixed. The artifact records each numerical setting separately.
  list(
    name = x$name,
    q = as.integer(x$q),
    hidden = as.integer(x$hidden),
    weight_decay = as.numeric(x$weight_decay),
    integration = as.character(x$integration)
  )
}

.sc_comp_bound_state <- function(bounds) {
  required <- c("mu_active", "kappa_active", "a_active", "weight_active")
  complete <- is.list(bounds) && all(required %in% names(bounds)) &&
    all(vapply(bounds[required], function(x) {
      is.logical(x) && length(x) == 1L && !is.na(x)
    }, logical(1L)))
  active <- if (complete) {
    any(unlist(bounds[required], use.names = FALSE))
  } else NA
  list(
    complete = complete,
    active = active,
    pass = complete && !active,
    mu_active = complete && isTRUE(bounds$mu_active),
    kappa_active = complete && isTRUE(bounds$kappa_active),
    a_active = complete && isTRUE(bounds$a_active),
    weight_active = complete && isTRUE(bounds$weight_active)
  )
}

.sc_comp_grid_rows <- function(grid) {
  if (is.data.frame(grid)) {
    return(lapply(seq_len(nrow(grid)), function(i) {
      out <- lapply(grid, function(x) {
        if (is.list(x)) x[[i]] else x[i]
      })
      names(out) <- names(grid)
      out
    }))
  }
  if (is.list(grid) && length(grid) > 0L &&
      all(vapply(grid, is.list, logical(1L)))) return(grid)
  stop("`grid` must be a nonempty data frame or a list of named candidate lists.",
       call. = FALSE)
}

.sc_comp_normalize_grid <- function(grid, q = NULL, p,
                                    allow_q_tuning = FALSE) {
  rows <- .sc_comp_grid_rows(grid)
  if (!is.null(q) && (!is.numeric(q) || length(q) != 1L || is.na(q) ||
                      q < 0L || q != as.integer(q))) {
    stop("Fixed `q` must be a nonnegative integer.", call. = FALSE)
  }
  out <- lapply(seq_along(rows), function(j) {
    x <- rows[[j]]
    qj <- if (is.null(x$q)) q else x$q
    if (is.null(qj)) {
      stop("Each candidate needs `q`, or one fixed `q` must be supplied.",
           call. = FALSE)
    }
    if (!is.null(q) && !is.null(x$q) && !identical(as.integer(x$q), as.integer(q))) {
      stop("A candidate `q` conflicts with the fixed `q` argument.", call. = FALSE)
    }
    if (!is.numeric(qj) || length(qj) != 1L || is.na(qj) || qj < 0L ||
        qj != as.integer(qj) || qj > p - 1L) {
      stop("Every candidate `q` must be an integer between zero and p - 1.",
           call. = FALSE)
    }
    hidden <- x$hidden
    if (is.null(hidden) || !is.numeric(hidden) || !length(hidden) ||
        any(!is.finite(hidden)) || any(hidden < 1L) ||
        any(hidden != as.integer(hidden))) {
      stop("Every candidate needs a positive-integer `hidden` architecture.",
           call. = FALSE)
    }
    wd <- if (is.null(x$weight_decay)) 0 else x$weight_decay
    if (!is.numeric(wd) || length(wd) != 1L || !is.finite(wd) || wd < 0) {
      stop("Every `weight_decay` must be one finite nonnegative number.",
           call. = FALSE)
    }
    integration <- if (is.null(x$integration)) "auto" else
      match.arg(as.character(x$integration), c("auto", "gh", "qmc"))
    n_nodes <- if (is.null(x$n_nodes)) 31L else x$n_nodes
    n_draws <- if (is.null(x$n_draws)) 4096L else x$n_draws
    if (!is.numeric(n_nodes) || length(n_nodes) != 1L || is.na(n_nodes) ||
        n_nodes < 3L || n_nodes != as.integer(n_nodes)) {
      stop("Every `n_nodes` must be an integer of at least three.", call. = FALSE)
    }
    if (!is.numeric(n_draws) || length(n_draws) != 1L || is.na(n_draws) ||
        n_draws < 16L || n_draws != as.integer(n_draws)) {
      stop("Every `n_draws` must be an integer of at least sixteen.", call. = FALSE)
    }
    effective_integration <- integration
    if (identical(effective_integration, "auto") && qj > 0L) {
      effective_integration <- if (qj <= 3L) "gh" else "qmc"
    }
    if (qj > 3L && identical(effective_integration, "gh")) {
      stop("Product Gauss-Hermite candidates require q <= 3.", call. = FALSE)
    }
    if (qj > 0L && identical(effective_integration, "qmc") &&
        n_draws %% 2L != 0L) {
      stop("Every QMC `n_draws` must be even because tuning uses antithetic draws.",
           call. = FALSE)
    }
    list(
      name = if (is.null(x$name)) paste0("candidate_", j) else as.character(x$name),
      hidden = as.integer(hidden), weight_decay = as.numeric(wd),
      integration = integration, n_nodes = as.integer(n_nodes),
      n_draws = as.integer(n_draws), q = as.integer(qj)
    )
  })
  qs <- unique(vapply(out, `[[`, integer(1L), "q"))
  if (length(qs) > 1L && !isTRUE(allow_q_tuning)) {
    stop("The tuning grid varies `q`. Prespecify one `q`, or set ",
         "`allow_q_tuning = TRUE` and do not use fixed-specification coverage ",
         "without selection-adjusted theory.", call. = FALSE)
  }
  out
}

.sc_comp_integration_policy <- function(specs,
                                        allow_integration_tuning = FALSE) {
  signatures <- vapply(specs, .sc_comp_effective_integration_signature,
                       character(1L), include_q = FALSE)
  q_values <- vapply(specs, `[[`, integer(1L), "q")
  varies <- any(vapply(split(signatures, q_values), function(x) {
    length(unique(x)) > 1L
  }, logical(1L)))
  if (varies && !isTRUE(allow_integration_tuning)) {
    stop(
      "The learner tuning grid varies numerical-integration resolution. ",
      "Choose one integration rule for learner CV and use ",
      "`scmix_integration_refinement()` for fresh-refit numerical refinement. ",
      "Set `allow_integration_tuning = TRUE` only for an explicitly diagnostic ",
      "comparison that is not used for ordinary fixed-specification inference.",
      call. = FALSE
    )
  }
  list(
    varies = varies,
    signatures = signatures,
    diagnostic_only = varies,
    primary_inference_eligible = !varies,
    interpretation = if (varies) {
      paste(
        "Integration settings were compared diagnostically inside learner CV;",
        "this does not replace fresh-refit integration refinement and is not",
        "eligible for ordinary fixed-specification inference."
      )
    } else {
      paste(
        "Numerical integration was held fixed across learner candidates;",
        "accuracy must be assessed separately by fresh-refit refinement."
      )
    }
  )
}

.sc_comp_validate_matrix_data <- function(deltaX, y, Z, respondent_id) {
  deltaX <- as.matrix(deltaX)
  Z <- as.matrix(Z)
  n <- nrow(deltaX)
  if (!is.numeric(deltaX) || !n || any(!is.finite(deltaX)) || ncol(deltaX) < 1L) {
    stop("`deltaX` must be a nonempty finite numeric matrix.", call. = FALSE)
  }
  if (!is.numeric(Z) || nrow(Z) != n || ncol(Z) < 1L || any(!is.finite(Z))) {
    stop("`Z` must be a finite numeric matrix with one row per task.", call. = FALSE)
  }
  if (length(y) != n || anyNA(y) || !all(y %in% c(0, 1))) {
    stop("`y` must be a binary vector with one value per task.", call. = FALSE)
  }
  if (length(respondent_id) != n || anyNA(respondent_id) ||
      length(unique(as.character(respondent_id))) < 2L) {
    stop("`respondent_id` must identify at least two respondents.", call. = FALSE)
  }
  ## This also verifies that moderators are constant within respondent.
  .sc_fit_z_transform(Z, respondent_id)
  list(deltaX = deltaX, y = as.numeric(y), Z = Z,
       respondent_id = respondent_id)
}

#' Unpenalized complete-sequence log likelihoods for a fitted network
#' @keywords internal
#' @noRd
.sc_comp_sequence_loglik <- function(net, deltaX, y, Z, respondent_id,
                                     integration_grid, device = "cpu") {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("Sequence evaluation requires the 'torch' package.", call. = FALSE)
  }
  dev <- torch::torch_device(device)
  rf <- factor(respondent_id, levels = unique(respondent_id))
  dx <- torch::torch_tensor(deltaX, dtype = torch::torch_float(), device = dev)
  zt <- torch::torch_tensor(Z, dtype = torch::torch_float(), device = dev)
  yt <- torch::torch_tensor(as.numeric(y), dtype = torch::torch_float(), device = dev)
  idx1 <- torch::torch_tensor(as.integer(rf), dtype = torch::torch_long(), device = dev)
  q <- ncol(integration_grid$U)
  U <- if (q == 0L) {
    torch::torch_zeros(1L, 1L, dtype = torch::torch_float(), device = dev)
  } else {
    torch::torch_tensor(integration_grid$U, dtype = torch::torch_float(), device = dev)
  }
  logw <- torch::torch_tensor(log(integration_grid$w),
                              dtype = torch::torch_float(), device = dev)
  net$eval()
  ll <- torch::with_no_grad({
    mu <- net$get_beta(zt)
    base <- net$get_kappa() + torch::torch_sum(dx * mu, dim = 2L)
    index <- if (net$q == 0L) {
      base$unsqueeze(2L)
    } else {
      base$unsqueeze(2L) + torch::torch_mm(torch::torch_mm(dx, net$A), U$t())
    }
    lp <- -torch::nnf_softplus(-index) * yt$unsqueeze(2L) -
      torch::nnf_softplus(index) * (1 - yt)$unsqueeze(2L)
    agg <- torch::torch_zeros(nlevels(rf), lp$shape[2], dtype = lp$dtype,
                              device = lp$device)
    agg <- agg$index_add(1L, idx1, lp)
    torch::torch_logsumexp(agg + logw$unsqueeze(1L), dim = 2L)
  })
  out <- as.numeric(torch::as_array(ll$detach()$cpu()))
  names(out) <- levels(rf)
  out
}

.sc_comp_integration_key <- function(spec) {
  .sc_comp_effective_integration_signature(spec, include_q = TRUE)
}

.sc_comp_effective_integration_signature <- function(spec, include_q = TRUE) {
  q <- as.integer(spec$q)
  method <- spec$integration
  if (q == 0L) {
    key <- "exact"
  } else {
    if (identical(method, "auto")) method <- if (q <= 3L) "gh" else "qmc"
    key <- if (identical(method, "gh")) {
      paste("gh", spec$n_nodes, sep = "|")
    } else {
      paste("qmc", spec$n_draws, sep = "|")
    }
  }
  if (isTRUE(include_q)) paste(q, key, sep = "|") else key
}

.sc_comp_validate_integration_grid <- function(grid, q,
                                               what = "integration grid") {
  if (!is.list(grid) || is.null(grid$U) || is.null(grid$w)) {
    stop("`", what, "` must contain nodes `U` and weights `w`.", call. = FALSE)
  }
  U <- as.matrix(grid$U)
  storage.mode(U) <- "double"
  w <- as.numeric(grid$w)
  if (ncol(U) != q || nrow(U) < 1L || length(w) != nrow(U) ||
      any(!is.finite(U)) || any(!is.finite(w)) || any(w <= 0)) {
    stop("`", what, "` is incompatible with q = ", q, ".", call. = FALSE)
  }
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0 || abs(sw - 1) > 1e-8) {
    stop("`", what, "` weights must be positive and sum to one.", call. = FALSE)
  }
  out <- grid
  out$U <- U
  out$w <- w / sw
  out
}

.sc_comp_same_integration_grid <- function(x, y, tolerance = 0) {
  isTRUE(all.equal(x$U, y$U, tolerance = tolerance,
                   check.attributes = FALSE)) &&
    isTRUE(all.equal(x$w, y$w, tolerance = tolerance,
                     check.attributes = FALSE))
}

.sc_comp_fit_summary <- function(fit) {
  list(
    best_start = fit$best_start,
    starts = fit$start_diagnostics,
    objective = fit$objective,
    gradient_norm = fit$final_gradient_norm,
    gradient_by_parameter = fit$final_gradient_by_parameter,
    structural_gradient_norm = fit$structural_gradient_norm,
    sieve_gradient_norm = fit$sieve_gradient_norm,
    converged = fit$converged,
    stationarity_met = fit$stationarity_met,
    structural_stationarity_met = fit$structural_stationarity_met,
    sieve_stationarity_met = fit$sieve_stationarity_met,
    criterion_tolerance_met = fit$criterion_tolerance_met,
    criterion_diagnostic_source = fit$criterion_diagnostic_source,
    state_restored = fit$state_restored,
    objective_components = fit$objective_components,
    objective_finite = fit$objective_finite,
    optimization_gate_pass = fit$optimization_gate_pass,
    optimization_failure_reasons = fit$optimization_failure_reasons,
    stop_reason = fit$stop_reason,
    bounds = fit$bounds,
    start_objective_range = fit$start_objective_range,
    best_minus_second = fit$best_minus_second,
    global_optimality_gap_known = FALSE
  )
}

.sc_comp_inner_fit_gate <- function(fit_summary) {
  if (!is.list(fit_summary)) {
    stop("Inner-fit diagnostics must be a list.", call. = FALSE)
  }
  bound_state <- .sc_comp_bound_state(fit_summary$bounds)
  pass <- isTRUE(fit_summary$optimization_gate_pass) && bound_state$pass
  failure_reasons <- fit_summary$optimization_failure_reasons
  if (!bound_state$complete) {
    failure_reasons <- c(
      failure_reasons, "compact_bound_diagnostics_incomplete")
  } else if (isTRUE(bound_state$active)) {
    failure_reasons <- c(failure_reasons, "parameter_bound_active")
  }
  if (!pass && !length(failure_reasons)) {
    failure_reasons <- "inner_computational_gate_failed_without_reason"
  }
  list(
    pass = pass,
    bound_state = bound_state,
    failure_reasons = unique(as.character(failure_reasons))
  )
}

#' Select a tuning candidate only from computationally valid inner fits
#'
#' Validation scores remain available for diagnosis, but a candidate enters the
#' maximization only if every inner-fold optimizer passes the fail-closed
#' computational/compact-bound gate and every fold score is finite.
#'
#' @keywords internal
#' @noRd
.sc_comp_select_candidate <- function(fold_score, fold_n,
                                      fold_computational_gate) {
  fold_score <- as.matrix(fold_score)
  fold_n <- as.matrix(fold_n)
  fold_computational_gate <- as.matrix(fold_computational_gate)
  if (!is.numeric(fold_score) || !is.numeric(fold_n) ||
      !is.logical(fold_computational_gate) ||
      !identical(dim(fold_score), dim(fold_n)) ||
      !identical(dim(fold_score), dim(fold_computational_gate)) ||
      nrow(fold_score) < 1L || ncol(fold_score) < 2L ||
      anyNA(fold_computational_gate)) {
    stop("Malformed inner-fold score or computational-gate matrices.",
         call. = FALSE)
  }
  computationally_eligible <- apply(
    fold_computational_gate, 1L, function(x) all(x))
  score_eligible <- apply(
    is.finite(fold_score) & is.finite(fold_n) & fold_n > 0,
    1L, function(x) all(x))
  cv_log_score <- rep(NA_real_, nrow(fold_score))
  if (any(score_eligible)) {
    cv_log_score[score_eligible] <-
      rowSums(fold_score[score_eligible, , drop = FALSE] *
                fold_n[score_eligible, , drop = FALSE]) /
      rowSums(fold_n[score_eligible, , drop = FALSE])
  }
  score_eligible <- score_eligible & is.finite(cv_log_score)
  selection_eligible <- computationally_eligible & score_eligible
  ineligible_reason <- vapply(seq_len(nrow(fold_score)), function(j) {
    why <- character()
    if (!computationally_eligible[j]) {
      why <- c(why, "inner_computational_gate_failed")
    }
    if (!score_eligible[j]) why <- c(why, "nonfinite_validation_score")
    if (!length(why)) "" else paste(why, collapse = ";")
  }, character(1L))
  if (!any(selection_eligible)) {
    details <- paste0(
      "candidate ", seq_len(nrow(fold_score)), " [", ineligible_reason, "]")
    stop(
      "No learner candidate passed every inner-fold computational gate and ",
      "returned finite complete-sequence validation scores. ",
      paste(details, collapse = "; "),
      ". Increase optimization effort or revise the prespecified grid; do not ",
      "select from failed fits.", call. = FALSE
    )
  }
  selection_score <- cv_log_score
  selection_score[!selection_eligible] <- -Inf
  list(
    selected = which.max(selection_score),
    cv_log_score = cv_log_score,
    selection_score = selection_score,
    computationally_eligible = computationally_eligible,
    score_eligible = score_eligible,
    selection_eligible = selection_eligible,
    ineligible_reason = ineligible_reason,
    pass = TRUE
  )
}

#' Respondent-level inner cross-validation for one outer training sample
#'
#' This matrix interface compares a finite, prespecified tuning grid using the
#' unpenalized complete-sequence log likelihood. Every row for a respondent is
#' assigned to the same validation fold. Moderator transforms and contrast
#' scales are learned on inner-training respondents only and frozen before
#' validation. For DML, the entire call must occur inside an outer training
#' sample; running it once on the full sample and reusing the selected nuisance
#' architecture in every outer fold leaks held-out information.
#' A candidate is eligible for selection only when every inner-fold fit passes
#' the fail-closed optimization and compact-bound gate and every validation
#' score is finite. Scores and failure diagnostics for excluded candidates are
#' retained, and tuning stops rather than selecting if no candidate is eligible.
#'
#' @param deltaX,y,Z,respondent_id Task-level matrix inputs.
#' @param grid Data frame or list of candidate lists. Each candidate specifies
#'   `hidden`, `weight_decay`, and optionally integration controls and `q`.
#'   QMC draw counts must be even because tuning uses antithetic draws.
#' @param q Optional fixed prespecified factor rank shared by every candidate.
#' @param K Number of respondent-level inner folds.
#' @param allow_q_tuning Permit data-dependent rank tuning. The paper's regular
#'   fixed-specification inference should leave this `FALSE`.
#' @param allow_integration_tuning Permit integration settings in learner CV to
#'   vary only as an explicitly diagnostic exercise. Numerical resolution
#'   should ordinarily be held fixed here and studied with fresh refits via
#'   [scmix_integration_refinement()].
#' @param refit_integration_grid Optional common integration grid for the
#'   selected refit. The nested outer wrapper uses this to ensure every fold
#'   nuisance is evaluated on exactly the same finite nodes.
#' @param refit Refit the selected specification on all supplied rows. These
#'   rows must themselves be one outer training sample when used for DML.
#' @param keep_cv_fits Retain all inner-fold network objects (memory intensive).
#' @param a_bound Positive raw-coefficient Frobenius bound for the loading.
#' @param weight_bound Positive coordinatewise network-parameter bound.
#' @param n_epochs,learning_rate,n_starts,mu_bound,kappa_bound,opt_tol,grad_tol,seed,device,verbose Optimization controls, passed to [scmix_tune_outer_matrix()] via its `...`.
#' @param ... Core optimization controls (`n_epochs`, `learning_rate`,
#'   `n_starts`, `mu_bound`, `kappa_bound`, `opt_tol`, `grad_tol`, `seed`,
#'   `device`, `verbose`) forwarded to [scmix_tune_matrix()] inside every
#'   outer training set.
#' @return An internal tuning object with respondent-sequence scores,
#'   training-only preprocessing, inner-fold computational and candidate
#'   eligibility diagnostics, selected specification, optional refit, and a
#'   data/specification/fold analysis signature.
#' @rdname scmix_tune_matrix
#' @export
scmix_tune_matrix <- function(deltaX, y, Z, respondent_id, grid, q = NULL,
                              K = 3L, allow_q_tuning = FALSE,
                              allow_integration_tuning = FALSE,
                              n_epochs = 400L, learning_rate = 0.01,
                              n_starts = 2L, mu_bound = 10,
                              kappa_bound = 10, a_bound = 10,
                              weight_bound = 10, opt_tol = 1e-7,
                              grad_tol = 1e-4, seed = NULL,
                              device = "cpu", refit = TRUE,
                              refit_integration_grid = NULL,
                              keep_cv_fits = FALSE, verbose = FALSE) {
  dat <- .sc_comp_validate_matrix_data(deltaX, y, Z, respondent_id)
  deltaX <- dat$deltaX; y <- dat$y; Z <- dat$Z
  respondent_id <- dat$respondent_id
  N <- length(unique(as.character(respondent_id)))
  compact <- .sc_mixed_validate_compact_bounds(
    p = ncol(deltaX), coefficient_scale = rep(1, ncol(deltaX)),
    a_bound = a_bound, weight_bound = weight_bound
  )
  if (!is.numeric(K) || length(K) != 1L || is.na(K) || K < 2L ||
      K != as.integer(K) || K > N) {
    stop("`K` must be an integer between two and the number of respondents.",
         call. = FALSE)
  }
  K <- as.integer(K)
  specs <- .sc_comp_normalize_grid(grid, q = q, p = ncol(deltaX),
                                   allow_q_tuning = allow_q_tuning)
  integration_policy <- .sc_comp_integration_policy(
    specs, allow_integration_tuning = allow_integration_tuning)
  fold_id <- .sc_make_folds(respondent_id, K = K, seed = seed)
  if (any(vapply(split(fold_id, as.character(respondent_id)),
                 function(x) length(unique(x)) != 1L, logical(1L)))) {
    stop("Internal error: a respondent was split across inner folds.", call. = FALSE)
  }

  prep <- vector("list", K)
  for (k in seq_len(K)) {
    train <- fold_id != k
    prep[[k]] <- list(
      z = .sc_fit_z_transform(Z[train, , drop = FALSE], respondent_id[train]),
      deltaX = .sc_comp_fit_dx_scale(deltaX[train, , drop = FALSE],
                                     respondent_id[train]),
      training_respondents = unique(as.character(respondent_id[train])),
      validation_respondents = unique(as.character(respondent_id[!train]))
    )
  }

  J <- length(specs)
  fold_score <- matrix(NA_real_, J, K)
  fold_n <- matrix(0L, J, K)
  fold_computational_gate <- matrix(FALSE, J, K)
  fold_computational_failure_reasons <- vector("list", J)
  sequence_scores <- vector("list", J)
  cv_optimization <- vector("list", J)
  cv_fits <- if (isTRUE(keep_cv_fits)) vector("list", J) else NULL
  integration_cache <- new.env(parent = emptyenv())

  for (j in seq_len(J)) {
    sequence_scores[[j]] <- vector("list", K)
    cv_optimization[[j]] <- vector("list", K)
    fold_computational_failure_reasons[[j]] <- vector("list", K)
    if (isTRUE(keep_cv_fits)) cv_fits[[j]] <- vector("list", K)
    spec <- specs[[j]]
    for (k in seq_len(K)) {
      train <- fold_id != k
      valid <- !train
      dx_scale <- prep[[k]]$deltaX$scale
      dx_train <- sweep(deltaX[train, , drop = FALSE], 2L, dx_scale, `/`)
      dx_valid <- sweep(deltaX[valid, , drop = FALSE], 2L, dx_scale, `/`)
      z_train <- .sc_apply_z_transform(Z[train, , drop = FALSE], prep[[k]]$z)
      z_valid <- .sc_apply_z_transform(Z[valid, , drop = FALSE], prep[[k]]$z)
      ikey <- .sc_comp_integration_key(spec)
      cache_key <- paste(k, ikey, sep = "::")
      if (exists(cache_key, envir = integration_cache, inherits = FALSE)) {
        integration_grid <- get(cache_key, envir = integration_cache,
                                inherits = FALSE)
      } else {
        grid_seed <- .sc_comp_seed(seed, "inner-grid", k, ikey)
        integration_grid <- .sc_mixed_grid(
          q = spec$q, integration = spec$integration,
          n_nodes = spec$n_nodes, n_draws = spec$n_draws,
          seed = grid_seed, antithetic = TRUE, scramble = TRUE)
        assign(cache_key, integration_grid, envir = integration_cache)
      }
      fit_jk <- .sc_train_mixed_multistart(
        deltaX = dx_train, y = y[train], Z = z_train,
        respondent_id = respondent_id[train], gh = integration_grid,
        hidden = spec$hidden, n_epochs = n_epochs,
        learning_rate = learning_rate, weight_decay = spec$weight_decay,
        n_starts = n_starts,
        seed = .sc_comp_seed(seed, "inner-fit", j, k),
        device = device, verbose = verbose, warm_state = NULL,
        early_stop = FALSE, opt_tol = opt_tol, grad_tol = grad_tol,
        mu_bound = mu_bound, kappa_bound = kappa_bound,
        a_bound = compact$a_bound, weight_bound = compact$weight_bound,
        coefficient_scale = dx_scale)
      ll <- .sc_comp_sequence_loglik(
        fit_jk$net, dx_valid, y[valid], z_valid, respondent_id[valid],
        integration_grid, device = device)
      sequence_scores[[j]][[k]] <- ll
      fold_score[j, k] <- mean(ll)
      fold_n[j, k] <- length(ll)
      fit_summary <- .sc_comp_fit_summary(fit_jk)
      inner_gate <- .sc_comp_inner_fit_gate(fit_summary)
      fold_computational_gate[j, k] <- inner_gate$pass
      fold_computational_failure_reasons[[j]][[k]] <-
        inner_gate$failure_reasons
      cv_optimization[[j]][[k]] <- fit_summary
      if (isTRUE(keep_cv_fits)) cv_fits[[j]][[k]] <- fit_jk$net
    }
  }
  selection <- .sc_comp_select_candidate(
    fold_score, fold_n, fold_computational_gate)
  cv_log_score <- selection$cv_log_score
  selected <- selection$selected
  candidate_table <- data.frame(
    candidate = vapply(specs, `[[`, character(1L), "name"),
    q = vapply(specs, `[[`, integer(1L), "q"),
    hidden = vapply(specs, function(x) paste(x$hidden, collapse = "-"), character(1L)),
    weight_decay = vapply(specs, `[[`, numeric(1L), "weight_decay"),
    integration = vapply(specs, `[[`, character(1L), "integration"),
    n_nodes = vapply(specs, `[[`, integer(1L), "n_nodes"),
    n_draws = vapply(specs, `[[`, integer(1L), "n_draws"),
    cv_sequence_log_score = cv_log_score,
    selection_score = selection$selection_score,
    all_inner_fits_computationally_valid =
      selection$computationally_eligible,
    all_inner_scores_finite = selection$score_eligible,
    selection_eligible = selection$selection_eligible,
    ineligible_reason = selection$ineligible_reason,
    selected = seq_len(J) == selected,
    stringsAsFactors = FALSE
  )
  analysis_signature <- .sc_analysis_signature(
    deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
    fold_id = fold_id,
    specification = list(
      workflow = "respondent-level-inner-tuning-and-selected-refit",
      candidates = lapply(specs, .sc_comp_signature_spec),
      selected_index = as.integer(selected),
      allow_q_tuning = isTRUE(allow_q_tuning),
      allow_integration_tuning = isTRUE(allow_integration_tuning),
      n_epochs = as.integer(n_epochs),
      learning_rate = as.numeric(learning_rate),
      n_starts = as.integer(n_starts),
      mu_bound = as.numeric(mu_bound),
      kappa_bound = as.numeric(kappa_bound),
      a_bound = compact$a_bound,
      weight_bound = compact$weight_bound,
      opt_tol = as.numeric(opt_tol),
      grad_tol = as.numeric(grad_tol)
    )
  )

  refitted <- NULL
  if (isTRUE(refit)) {
    spec <- specs[[selected]]
    z_full_transform <- .sc_fit_z_transform(Z, respondent_id)
    dx_full_transform <- .sc_comp_fit_dx_scale(deltaX, respondent_id)
    z_full <- .sc_apply_z_transform(Z, z_full_transform)
    dx_full <- sweep(deltaX, 2L, dx_full_transform$scale, `/`)
    integration_grid <- if (is.null(refit_integration_grid)) {
      .sc_mixed_grid(
        q = spec$q, integration = spec$integration,
        n_nodes = spec$n_nodes, n_draws = spec$n_draws,
        seed = .sc_comp_seed(seed, "selected-grid", .sc_comp_integration_key(spec)),
        antithetic = TRUE, scramble = TRUE)
    } else {
      .sc_comp_validate_integration_grid(
        refit_integration_grid, q = spec$q,
        what = "refit_integration_grid")
    }
    ff <- .sc_train_mixed_multistart(
      deltaX = dx_full, y = y, Z = z_full, respondent_id = respondent_id,
      gh = integration_grid, hidden = spec$hidden, n_epochs = n_epochs,
      learning_rate = learning_rate, weight_decay = spec$weight_decay,
      n_starts = n_starts, seed = .sc_comp_seed(seed, "selected-refit"),
      device = device, verbose = verbose, warm_state = NULL,
      early_stop = FALSE, opt_tol = opt_tol, grad_tol = grad_tol,
      mu_bound = mu_bound, kappa_bound = kappa_bound,
      a_bound = compact$a_bound, weight_bound = compact$weight_bound,
      coefficient_scale = dx_full_transform$scale)
    A <- ff$A / dx_full_transform$scale
    refitted <- list(
      net = ff$net,
      mu = sweep(.sc_predict_beta(ff$net, z_full), 2L,
                 dx_full_transform$scale, `/`),
      A = A, Sigma = tcrossprod(A), kappa = ff$kappa,
      specification = spec, integration_grid = integration_grid,
      preprocessing = list(Z = z_full_transform, deltaX = dx_full_transform),
      optimization = .sc_comp_fit_summary(ff),
      analysis_signature = analysis_signature,
      raw_data = list(deltaX = deltaX, y = y, Z = Z,
                      respondent_id = respondent_id),
      scope = "all rows supplied to this outer-training call"
    )
    class(refitted) <- c("scmix_tuned_matrix_fit", "list")
  }

  out <- list(
    candidates = candidate_table, specifications = specs,
    selected = specs[[selected]], selected_index = selected,
    fold_id = fold_id, preprocessing_folds = prep,
    fold_sequence_log_score = fold_score,
    fold_n_respondents = fold_n, sequence_scores = sequence_scores,
    fold_computational_gate = fold_computational_gate,
    fold_computational_failure_reasons =
      fold_computational_failure_reasons,
    candidate_selection_gate = list(
      pass = selection$pass,
      selected_index = selected,
      computationally_eligible = selection$computationally_eligible,
      score_eligible = selection$score_eligible,
      selection_eligible = selection$selection_eligible,
      ineligible_reason = selection$ineligible_reason),
    cv_optimization = cv_optimization, cv_fits = cv_fits,
    refit = refitted,
    scoring = "unpenalized complete respondent-sequence log likelihood",
    preprocessing = paste(
      "Z centering/scaling and respondent-weighted DeltaX scaling are fitted",
      "on inner-training respondents only and frozen for validation."),
    dml_scope = paste(
      "For DML this whole tuning operation must be rerun inside each outer",
      "training set; full-sample tuning cannot be reused across outer folds."),
    rank_selection_warning = if (length(unique(candidate_table$q)) > 1L)
      paste("q was tuned from the data; ordinary fixed-q confidence intervals",
            "do not automatically have selection-adjusted coverage.") else NULL,
    integration_policy = integration_policy,
    integration_tuning_diagnostic = integration_policy$diagnostic_only,
    primary_inference_eligible = integration_policy$primary_inference_eligible &&
      length(unique(candidate_table$q)) == 1L &&
      isTRUE(selection$selection_eligible[selected]),
    analysis_signature = analysis_signature
  )
  class(out) <- c("scmix_tuning", "list")
  out
}

#' Run tuning separately inside every DML outer training set
#'
#' @param outer_K,inner_K,outer_fold_id Respondent-level outer/inner fold
#'   controls, mirroring [scmix_tune_matrix()]'s `K` one level up: each outer
#'   fold runs its own complete inner tuning.
#' @rdname scmix_tune_matrix
#' @export
scmix_tune_outer_matrix <- function(deltaX, y, Z, respondent_id, grid,
                                    q = NULL, outer_K = 5L, inner_K = 3L,
                                    outer_fold_id = NULL, seed = NULL, ...) {
  dat <- .sc_comp_validate_matrix_data(deltaX, y, Z, respondent_id)
  N <- length(unique(as.character(respondent_id)))
  if (is.null(outer_fold_id)) {
    outer_fold_id <- .sc_make_folds(respondent_id, K = outer_K, seed = seed)
  }
  if (length(outer_fold_id) != nrow(dat$deltaX) || anyNA(outer_fold_id) ||
      any(vapply(split(outer_fold_id, as.character(respondent_id)),
                 function(x) length(unique(x)) != 1L, logical(1L)))) {
    stop("`outer_fold_id` must keep every respondent in one outer fold.",
         call. = FALSE)
  }
  outer_fold_character <- as.character(outer_fold_id)
  folds <- unique(outer_fold_character)
  if (length(folds) < 2L || anyNA(folds) || any(!nzchar(folds))) {
    stop("At least two valid outer folds are required.", call. = FALSE)
  }
  dots <- list(...)
  if (any(c("K", "seed", "refit", "refit_integration_grid") %in% names(dots))) {
    stop("Pass fold, seed, and refit controls through the outer wrapper, not `...`.",
         call. = FALSE)
  }
  specs <- .sc_comp_normalize_grid(
    grid, q = q, p = ncol(dat$deltaX),
    allow_q_tuning = isTRUE(dots$allow_q_tuning))
  integration_policy <- .sc_comp_integration_policy(
    specs,
    allow_integration_tuning = isTRUE(dots$allow_integration_tuning))
  full_integration_keys <- unique(vapply(
    specs, .sc_comp_integration_key, character(1L)))
  common_refit_grid <- NULL
  if (length(full_integration_keys) == 1L) {
    spec0 <- specs[[1L]]
    common_refit_grid <- .sc_mixed_grid(
      q = spec0$q, integration = spec0$integration,
      n_nodes = spec0$n_nodes, n_draws = spec0$n_draws,
      seed = .sc_comp_seed(seed, "outer-common-refit-grid",
                           full_integration_keys),
      antithetic = TRUE, scramble = TRUE)
  }
  fits <- lapply(folds, function(k) {
    train <- outer_fold_character != k
    do.call(scmix_tune_matrix, c(list(
      deltaX = dat$deltaX[train, , drop = FALSE], y = dat$y[train],
      Z = dat$Z[train, , drop = FALSE], respondent_id = respondent_id[train],
      grid = grid, q = q, K = inner_K,
      seed = .sc_comp_seed(seed, "outer", k), refit = TRUE,
      refit_integration_grid = common_refit_grid), dots))
  })
  names(fits) <- make.unique(paste0("outer_", folds))
  candidate_selection_gate_by_outer_fold <- vapply(fits, function(x) {
    gate <- x$candidate_selection_gate
    selected <- x$selected_index
    is.list(gate) && isTRUE(gate$pass) && length(selected) == 1L &&
      !is.na(selected) && isTRUE(gate$selection_eligible[selected])
  }, logical(1L))
  if (!all(candidate_selection_gate_by_outer_fold)) {
    stop("An outer training sample returned an invalid inner-tuning selection ",
         "gate. Do not assemble or use these nuisances.", call. = FALSE)
  }
  training_signatures <- vapply(fits, `[[`, character(1L),
                                "analysis_signature")
  if (any(!vapply(training_signatures, .sc_comp_nonempty_signature,
                  logical(1L)))) {
    stop("An outer tuning result is missing its analysis signature.",
         call. = FALSE)
  }
  outer_fold_index <- match(outer_fold_character, folds)
  analysis_signature <- .sc_analysis_signature(
    deltaX = dat$deltaX, y = dat$y, Z = dat$Z,
    respondent_id = respondent_id, fold_id = outer_fold_index,
    specification = list(
      workflow = "nested-respondent-level-tuning",
      candidates = lapply(specs, .sc_comp_signature_spec),
      inner_training_signatures = unname(training_signatures)
    )
  )
  out <- list(
    outer_fold_id = outer_fold_id,
    outer_fold_index = outer_fold_index,
    outer_folds = folds,
    tuning = fits,
    candidate_selection_gate_by_outer_fold =
      candidate_selection_gate_by_outer_fold,
    specifications = specs,
    integration_policy = integration_policy,
    common_refit_integration_grid = common_refit_grid,
    raw_data = list(deltaX = dat$deltaX, y = dat$y, Z = dat$Z,
                    respondent_id = respondent_id),
    analysis_signature = analysis_signature,
    n_respondents = N,
    nesting = "each tuning and selected refit used outer-training respondents only",
    assembly = paste(
      "Use scmix_assemble_nested() to construct the fold-nuisance object",
      "consumed by scmix_dml(); the assembler verifies folds, grids, and",
      "optimization diagnostics before handing off the object."))
  class(out) <- c("scmix_nested_tuning", "list")
  out
}

#' Assemble nested tuning refits for cross-fitted inference
#'
#' Converts one selected refit from each outer training set into the exact
#' fold-nuisance fields consumed by [scmix_dml()]. Every fold network is
#' evaluated on all respondents using only that outer-training fold's frozen
#' preprocessing. The current inference engine evaluates one common finite
#' integration grid, so the assembler requires identical nodes and weights
#' across folds and also retains them individually for audit.
#'
#' @param nested Result from [scmix_tune_outer_matrix()].
#' @param attr_names,z_names Optional labels for contrast and moderator columns.
#' @param require_optimization_gate Fail unless every selected outer refit has
#'   finite objective components, satisfies the fresh returned-state criterion
#'   and gradient tolerances, and has complete diagnostics showing that no
#'   artificial compact-parameter bound is active. It may be disabled only
#'   together with `diagnostic_only = TRUE`.
#' @param diagnostic_only Permit assembly after a failed computational or
#'   data-dependent-selection gate solely for diagnostics. Such an object is
#'   explicitly marked ineligible for the paper's ordinary inference.
#' @return A fold-nuisance object directly consumable by [scmix_dml()]. It is
#'   not a full-sample structural plug-in fit and retains the nested analysis
#'   signature.
#' @rdname scmix_tune_matrix
#' @export
scmix_assemble_nested <- function(nested, attr_names = NULL, z_names = NULL,
                                  require_optimization_gate = TRUE,
                                  diagnostic_only = FALSE) {
  if (!isTRUE(require_optimization_gate) && !isTRUE(diagnostic_only)) {
    stop("Disabling the optimization gate requires `diagnostic_only = TRUE`.",
         call. = FALSE)
  }
  if (!inherits(nested, "scmix_nested_tuning") ||
      is.null(nested$raw_data) || is.null(nested$tuning)) {
    stop("`nested` must be a result from `scmix_tune_outer_matrix()`.",
         call. = FALSE)
  }
  if (!.sc_comp_nonempty_signature(nested$analysis_signature)) {
    stop("Nested tuning is missing a valid analysis signature.", call. = FALSE)
  }
  dat <- .sc_comp_validate_matrix_data(
    nested$raw_data$deltaX, nested$raw_data$y, nested$raw_data$Z,
    nested$raw_data$respondent_id)
  rid <- as.character(dat$respondent_id)
  fold_id <- nested$outer_fold_index
  if (is.null(fold_id)) {
    fold_id <- match(as.character(nested$outer_fold_id), nested$outer_folds)
  }
  K <- length(nested$outer_folds)
  if (length(fold_id) != nrow(dat$deltaX) || anyNA(fold_id) ||
      !setequal(unique(fold_id), seq_len(K)) ||
      any(vapply(split(fold_id, rid), function(x) length(unique(x)) != 1L,
                 logical(1L)))) {
    stop("Nested outer folds are malformed or split a respondent.", call. = FALSE)
  }
  if (length(nested$tuning) != K) {
    stop("Nested tuning must contain one result per outer fold.", call. = FALSE)
  }
  refits <- lapply(seq_len(K), function(k) {
    fit <- nested$tuning[[k]]$refit
    if (!inherits(fit, "scmix_tuned_matrix_fit")) {
      stop("Outer fold ", k, " has no selected refit.", call. = FALSE)
    }
    expected_training <- setdiff(unique(rid), unique(rid[fold_id == k]))
    observed_training <- unique(as.character(fit$raw_data$respondent_id))
    if (!setequal(expected_training, observed_training)) {
      stop("Outer fold ", k,
           " refit does not contain exactly its outer-training respondents.",
           call. = FALSE)
    }
    fit
  })

  qs <- vapply(refits, function(x) x$specification$q, integer(1L))
  if (length(unique(qs)) != 1L) {
    stop("Selected outer refits have different q values and cannot define one ",
         "fixed-rank DML target.", call. = FALSE)
  }
  q <- qs[1L]
  p <- ncol(dat$deltaX)
  grids <- lapply(seq_len(K), function(k) {
    .sc_comp_validate_integration_grid(
      refits[[k]]$integration_grid, q = q,
      what = paste0("outer refit integration grid ", k))
  })
  same_grid <- vapply(grids[-1L], .sc_comp_same_integration_grid,
                      logical(1L), y = grids[[1L]], tolerance = 0)
  if (length(same_grid) && !all(same_grid)) {
    stop(
      "Selected outer refits use different finite integration nodes or weights. ",
      "Rerun `scmix_tune_outer_matrix()` with one fixed integration setting; ",
      "integration resolution belongs in `scmix_integration_refinement()`.",
      call. = FALSE
    )
  }

  candidate_selection_gate <- vapply(nested$tuning, function(x) {
    gate <- x$candidate_selection_gate
    selected <- x$selected_index
    is.list(gate) && isTRUE(gate$pass) && length(selected) == 1L &&
      !is.na(selected) && isTRUE(gate$selection_eligible[selected])
  }, logical(1L))
  selection_eligible <- vapply(
    nested$tuning,
    function(x) isTRUE(x$primary_inference_eligible), logical(1L)) &
    candidate_selection_gate
  if (!all(selection_eligible) && !isTRUE(diagnostic_only)) {
    stop(
      "At least one outer fold failed the inner candidate-selection gate, used ",
      "diagnostic integration, or used data-dependent q selection. Ordinary ",
      "fixed-specification inference is unavailable; ",
      "set `diagnostic_only = TRUE` only to assemble diagnostic predictions.",
      call. = FALSE
    )
  }
  optimization <- lapply(refits, `[[`, "optimization")
  bound_state <- lapply(optimization, function(x) .sc_comp_bound_state(x$bounds))
  bound_gate <- vapply(bound_state, `[[`, logical(1L), "pass")
  optimization_gate <- vapply(seq_along(optimization), function(k) {
    isTRUE(optimization[[k]]$optimization_gate_pass) && bound_gate[k]
  }, logical(1L))
  if (isTRUE(require_optimization_gate) && !all(optimization_gate) &&
      !isTRUE(diagnostic_only)) {
    failed <- which(!optimization_gate)
    reasons <- vapply(failed, function(k) {
      why <- optimization[[k]]$optimization_failure_reasons
      if (!bound_state[[k]]$complete) {
        why <- c(why, "compact_bound_diagnostics_incomplete")
      } else if (isTRUE(bound_state[[k]]$active)) {
        why <- c(why, "parameter_bound_active")
      }
      if (!length(why)) "missing fail-closed diagnostics" else
        paste(unique(why), collapse = ",")
    }, character(1L))
    stop("Selected outer-refit optimization gate failed in fold(s) ",
         paste(paste0(failed, " [", reasons, "]"), collapse = "; "),
         ". Increase optimization effort or inspect the audit; do not use ",
         "ordinary inference from this assembly.", call. = FALSE)
  }

  nets <- lapply(refits, `[[`, "net")
  z_transform_folds <- lapply(refits, function(x) x$preprocessing$Z)
  dx_transform_folds <- lapply(refits, function(x) x$preprocessing$deltaX)
  sd_dx_folds <- lapply(dx_transform_folds, `[[`, "scale")
  mu_all_folds <- vector("list", K)
  A_folds <- vector("list", K)
  kappa_folds <- numeric(K)
  for (k in seq_len(K)) {
    scale_k <- as.numeric(sd_dx_folds[[k]])
    if (length(scale_k) != p || any(!is.finite(scale_k)) || any(scale_k <= 0)) {
      stop("Outer fold ", k, " has an invalid DeltaX scale.", call. = FALSE)
    }
    Z_k <- .sc_apply_z_transform(dat$Z, z_transform_folds[[k]])
    mu_all_folds[[k]] <- sweep(
      .sc_predict_beta(nets[[k]], Z_k), 2L, scale_k, `/`)
    A_folds[[k]] <- as.matrix(refits[[k]]$A)
    if (!identical(dim(A_folds[[k]]), c(p, q)) ||
        any(!is.finite(A_folds[[k]]))) {
      stop("Outer fold ", k, " has an invalid computational loading.",
           call. = FALSE)
    }
    kappa_folds[k] <- refits[[k]]$kappa
  }
  if (any(!is.finite(kappa_folds))) {
    stop("Every outer fold must contain a finite kappa estimate.", call. = FALSE)
  }
  mu_hat <- matrix(NA_real_, nrow(dat$deltaX), p)
  for (k in seq_len(K)) {
    heldout <- fold_id == k
    mu_hat[heldout, ] <- mu_all_folds[[k]][heldout, , drop = FALSE]
  }
  if (any(!is.finite(mu_hat))) {
    stop("Could not construct complete out-of-fold mean predictions.", call. = FALSE)
  }

  attr_names <- if (is.null(attr_names)) colnames(dat$deltaX) else attr_names
  z_names <- if (is.null(z_names)) colnames(dat$Z) else z_names
  if (is.null(attr_names)) attr_names <- paste0("b", seq_len(p))
  if (is.null(z_names)) z_names <- paste0("z", seq_len(ncol(dat$Z)))
  if (length(attr_names) != p || length(z_names) != ncol(dat$Z)) {
    stop("`attr_names` or `z_names` has the wrong length.", call. = FALSE)
  }
  computational_gate_pass <- all(selection_eligible) && all(optimization_gate) &&
    !isTRUE(diagnostic_only)
  out <- list(
    deltaX = dat$deltaX, y = dat$y, Z = dat$Z,
    respondent_id = dat$respondent_id,
    fold_id = as.integer(fold_id), K = as.integer(K),
    N = length(unique(rid)), q = q,
    mu_hat = mu_hat, mu_all_folds = mu_all_folds,
    A_folds = A_folds, A_computational_folds = A_folds,
    kappa_folds = kappa_folds, nets = nets,
    z_transform_folds = z_transform_folds,
    dx_transform_folds = dx_transform_folds,
    sd_dx_folds = sd_dx_folds,
    gh = grids[[1L]], integration_grid = grids[[1L]],
    integration_grids_folds = grids,
    integration = grids[[1L]]$metadata,
    attr_names = as.character(attr_names), z_names = as.character(z_names),
    selected_specifications = lapply(refits, `[[`, "specification"),
    optimization = list(
      folds = optimization,
      gate_by_fold = optimization_gate,
      candidate_selection_gate_by_fold = candidate_selection_gate,
      compact_bound_gate_by_fold = bound_gate,
      diagnostics_are_certificates = FALSE),
    computational_gate_pass = computational_gate_pass,
    eligible_for_ordinary_inference = computational_gate_pass,
    diagnostic_only = !computational_gate_pass,
    analysis_signature = nested$analysis_signature,
    source = "selected refits from respondent-level nested tuning",
    scope = paste(
      "Fold nuisances for cross-fitted inference; not a full-sample structural",
      "plug-in fit. The computational gate is empirical and does not establish",
      "the paper's asymptotic numerical-error rate conditions."),
    nested_tuning = nested
  )
  class(out) <- c("scmix_nested_assembled", "list")
  out
}

#' Audit attained optimization results without asserting global optimality
#'
#' @param fit An `scmix`, `scmix_tuned_matrix_fit`, or assembled nested fit.
#' @return Start-by-fit diagnostics, compact-bound gates, the fit analysis
#'   signature, and scope summaries. Bound activity is available for the
#'   selected start because the core fit does not retain all nonselected
#'   networks.
#' @rdname scmix_tune_matrix
#' @export
scmix_optimization_audit <- function(fit) {
  opt <- fit$optimization
  if (is.null(opt)) stop("The fit does not contain optimization diagnostics.",
                         call. = FALSE)
  entries <- if (!is.null(opt$full)) {
    c(list(full = opt$full), stats::setNames(opt$folds,
                                             paste0("fold_", seq_along(opt$folds))))
  } else if (!is.null(opt$folds)) {
    stats::setNames(opt$folds, paste0("fold_", seq_along(opt$folds)))
  } else {
    list(refit = opt)
  }
  rows <- lapply(names(entries), function(scope) {
    x <- entries[[scope]]
    bound <- .sc_comp_bound_state(x$bounds)
    starts <- as.data.frame(x$starts)
    if (!nrow(starts) || !all(c("start", "objective", "gradient_norm",
                                "stop_reason") %in% names(starts))) {
      stop("Malformed start diagnostics in scope ", scope, ".", call. = FALSE)
    }
    if (!"optimization_gate_pass" %in% names(starts)) {
      starts$optimization_gate_pass <- FALSE
    }
    if (!"objective_finite" %in% names(starts)) {
      starts$objective_finite <- is.finite(starts$objective)
    }
    selected <- starts$start == x$best_start
    data.frame(
      scope = scope, starts,
      selected = selected,
      selected_mu_bound_active = ifelse(selected, isTRUE(x$bounds$mu_active), NA),
      selected_kappa_bound_active = ifelse(selected, isTRUE(x$bounds$kappa_active), NA),
      selected_a_bound_active = ifelse(selected, isTRUE(x$bounds$a_active), NA),
      selected_weight_bound_active = ifelse(
        selected, isTRUE(x$bounds$weight_active), NA),
      selected_bound_diagnostics_complete = ifelse(
        selected, bound$complete, NA),
      global_optimality_gap_known = FALSE,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  table <- do.call(rbind, rows)
  summary <- do.call(rbind, lapply(names(entries), function(scope) {
    x <- entries[[scope]]
    bound <- .sc_comp_bound_state(x$bounds)
    finite_start_objectives <- x$starts$objective[is.finite(x$starts$objective)]
    attained_range <- if (length(finite_start_objectives)) {
      diff(range(finite_start_objectives))
    } else {
      NA_real_
    }
    data.frame(
      scope = scope, selected_start = x$best_start,
      selected_objective = x$objective,
      attained_objective_range = attained_range,
      selected_gradient_norm = x$gradient_norm,
      selected_structural_gradient_norm = if (is.null(x$structural_gradient_norm))
        NA_real_ else x$structural_gradient_norm,
      selected_sieve_gradient_norm = if (is.null(x$sieve_gradient_norm))
        NA_real_ else x$sieve_gradient_norm,
      selected_stop_reason = x$stop_reason,
      selected_tolerance_status = isTRUE(x$converged),
      selected_criterion_source = if (is.null(x$criterion_diagnostic_source))
        "unavailable_legacy_diagnostic" else x$criterion_diagnostic_source,
      selected_state_restored = isTRUE(x$state_restored),
      selected_objective_finite = isTRUE(x$objective_finite),
      selected_optimization_gate_pass = isTRUE(x$optimization_gate_pass) &&
        bound$pass,
      selected_bound_diagnostics_complete = bound$complete,
      selected_mu_bound_active = bound$mu_active,
      selected_kappa_bound_active = bound$kappa_active,
      selected_a_bound_active = bound$a_active,
      selected_weight_bound_active = bound$weight_active,
      selected_any_bound_active = isTRUE(bound$active),
      stringsAsFactors = FALSE
    )
  }))
  analysis_signature <- if (.sc_comp_nonempty_signature(fit$analysis_signature)) {
    fit$analysis_signature
  } else NA_character_
  out <- list(starts = table, summary = summary,
              any_bound_activity = any(summary$selected_any_bound_active),
              all_bound_diagnostics_complete =
                all(summary$selected_bound_diagnostics_complete),
              all_selected_tolerances_met = all(summary$selected_tolerance_status),
              all_objectives_finite = all(summary$selected_objective_finite),
              all_computational_gates_pass =
                all(summary$selected_optimization_gate_pass),
              analysis_signature = analysis_signature,
              signature_match = .sc_comp_nonempty_signature(analysis_signature),
              global_optimality_gap_known = FALSE,
              disclaimer = paste(
                "Objective dispersion, gradient residuals, stopping reasons, and",
                "bound activity diagnose attained solutions. They do not bound the",
                "gap to the global optimum of the nonconvex criterion."))
  class(out) <- c("scmix_optimization_audit", "list")
  out
}

.sc_comp_extract_numeric <- function(fit, extractors) {
  if (!is.list(extractors) || !length(extractors) ||
      is.null(names(extractors)) || any(names(extractors) == "") ||
      any(!vapply(extractors, is.function, logical(1L)))) {
    stop("`extractors` must be a named nonempty list of functions.", call. = FALSE)
  }
  pieces <- lapply(names(extractors), function(nm) {
    value <- extractors[[nm]](fit)
    value <- unlist(value, recursive = TRUE, use.names = TRUE)
    if (!is.numeric(value) || !length(value) || any(!is.finite(value))) {
      stop("Extractor '", nm, "' must return finite numeric values.", call. = FALSE)
    }
    if (is.null(names(value)) || any(names(value) == "")) {
      names(value) <- paste0("value_", seq_along(value))
    }
    names(value) <- paste(nm, names(value), sep = ".")
    value
  })
  unlist(pieces, use.names = TRUE)
}

#' Refit-and-recompute integration refinement workflow
#'
#' Calls `refitter(setting)` once for every resolution-by-scramble setting,
#' then calls every requested extractor on every new fit. The resulting QOI,
#' standard-error, held-out-score, and information-eigenvalue summaries are
#' passed to `scmix_numerical_gate`; no cached baseline estimate is recycled.
#'
#' @param resolutions Positive integer node/draw resolutions.
#' @param scrambles Scramble seeds or labels. `NULL` requests one deterministic
#'   setting per resolution.
#' @param refitter Function taking one named setting list and returning a fit.
#' @param extractors Named functions such as `qoi`, `se`, `score`, and `eigen`.
#' @param tolerances Named tolerances for the flattened extractor columns.
#' @param reference Reference row for `scmix_numerical_gate`.
#' @param keep_fits Retain every refit.
#' @return Settings, recomputed checks, numerical gate, fit-linked analysis
#'   signature status, and optional fits.
#' @rdname scmix_tune_matrix
#' @export
scmix_integration_refinement <- function(resolutions, scrambles = NULL,
                                         refitter, extractors, tolerances,
                                         reference = NULL, keep_fits = FALSE) {
  if (!is.numeric(resolutions) || !length(resolutions) ||
      any(!is.finite(resolutions)) || any(resolutions < 1L) ||
      any(resolutions != as.integer(resolutions))) {
    stop("`resolutions` must be positive integers.", call. = FALSE)
  }
  if (!is.function(refitter)) stop("`refitter` must be a function.", call. = FALSE)
  if (is.null(scrambles)) scrambles <- NA_integer_
  if (!length(scrambles) || anyDuplicated(as.character(scrambles))) {
    stop("`scrambles` must contain distinct settings.", call. = FALSE)
  }
  settings <- expand.grid(
    resolution = as.integer(resolutions), scramble = scrambles,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  fits <- vector("list", nrow(settings))
  values <- vector("list", nrow(settings))
  for (i in seq_len(nrow(settings))) {
    setting <- as.list(settings[i, , drop = FALSE])
    setting$replication <- i
    fits[[i]] <- tryCatch(refitter(setting), error = function(e) {
      stop("Refit failed at resolution=", setting$resolution,
           ", scramble=", setting$scramble, ": ", conditionMessage(e),
           call. = FALSE)
    })
    values[[i]] <- .sc_comp_extract_numeric(fits[[i]], extractors)
  }
  template <- names(values[[1L]])
  if (any(!vapply(values, function(x) identical(names(x), template), logical(1L)))) {
    stop("Extractors returned inconsistent metrics across refits.", call. = FALSE)
  }
  checks_numeric <- as.data.frame(do.call(rbind, values), check.names = FALSE)
  checks <- cbind(settings, checks_numeric)
  if (is.null(reference)) reference <- nrow(checks)
  gate <- scmix_numerical_gate(checks, tolerances = tolerances,
                               reference = reference)
  refit_signatures <- vapply(fits, function(fit) {
    signature <- fit$analysis_signature
    if (.sc_comp_nonempty_signature(signature)) signature else NA_character_
  }, character(1L))
  reference_signature <- refit_signatures[gate$reference]
  signature_match <- .sc_comp_nonempty_signature(reference_signature) &&
    all(!is.na(refit_signatures)) &&
    all(refit_signatures == reference_signature)
  out <- list(settings = settings, checks = checks, gate = gate,
              refit_count = nrow(settings),
              analysis_signature = if (signature_match) {
                reference_signature
              } else NA_character_,
              refit_analysis_signatures = refit_signatures,
              signature_match = signature_match,
              fits = if (isTRUE(keep_fits)) fits else NULL,
              disclaimer = paste(
                "Every row is a fresh refit and fresh recomputation.",
                "Passing the empirical gate does not establish the numerical",
                "error rates assumed by the asymptotic theory."))
  class(out) <- c("scmix_integration_refinement", "list")
  out
}

#' Prespecified-rank sensitivity without data-dependent model selection
#'
#' @param primary_q Rank prespecified for the paper's primary specification.
#' @param alternatives Small set of sensitivity ranks.
#' @param refitter Function taking one integer q and returning a fit.
#' @param extractors Named QOI/SE/score/eigen extractor functions.
#' @param keep_fits Retain the refits.
#' @return Sensitivity table. The primary q is never replaced by the best-looking
#'   alternative and no selection-adjusted coverage is claimed.
#' @rdname scmix_tune_matrix
#' @export
scmix_q_sensitivity <- function(primary_q, alternatives, refitter, extractors,
                                keep_fits = FALSE) {
  qs <- c(primary_q, alternatives)
  if (!is.numeric(qs) || !length(qs) || anyNA(qs) || any(qs < 0L) ||
      any(qs != as.integer(qs))) {
    stop("Primary and alternative `q` values must be nonnegative integers.",
         call. = FALSE)
  }
  if (!is.function(refitter)) stop("`refitter` must be a function.", call. = FALSE)
  qs <- unique(as.integer(qs))
  primary_q <- as.integer(primary_q)
  fits <- lapply(qs, function(q) refitter(q))
  values <- lapply(fits, .sc_comp_extract_numeric, extractors = extractors)
  template <- names(values[[1L]])
  if (any(!vapply(values, function(x) identical(names(x), template), logical(1L)))) {
    stop("Extractors returned inconsistent metrics across q specifications.",
         call. = FALSE)
  }
  metrics <- as.data.frame(do.call(rbind, values), check.names = FALSE)
  table <- cbind(data.frame(q = qs, primary = qs == primary_q), metrics)
  primary_row <- which(table$primary)
  deviations <- sweep(as.matrix(metrics), 2L,
                      as.numeric(metrics[primary_row, , drop = TRUE]), `-`)
  colnames(deviations) <- paste0(names(metrics), ".difference_from_primary")
  table <- cbind(table, as.data.frame(deviations, check.names = FALSE))
  warning_text <- paste(
    "The primary q was prespecified and remains the reported specification.",
    "Alternative q fits are sensitivity analyses, not a selection procedure.",
    "If q is selected after inspecting these results, ordinary fixed-q",
    "confidence intervals do not automatically have selection-adjusted coverage.")
  out <- list(primary_q = primary_q, alternatives = setdiff(qs, primary_q),
              table = table, fits = if (isTRUE(keep_fits)) fits else NULL,
              selection_performed = FALSE,
              maintained_model = "low-rank normal mixed logit",
              coverage_warning = warning_text)
  class(out) <- c("scmix_q_sensitivity", "list")
  out
}
