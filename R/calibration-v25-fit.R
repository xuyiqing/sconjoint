## Numerical fitting adapters for the prospective v2.5 validation study.
##
## These functions deliberately contain no outcome-unlocking logic.  They are
## called only by the hash-bound evaluator after its approval record has been
## validated.  Keeping them separate makes the exact production fit and the
## two GH31 refinement branches auditable without inheriting any v2.3 release
## flag.

.scmix_v25_fit_grid <- function(nodes = 15L, weight_decay = 0.01) {
  if (!is.numeric(nodes) || length(nodes) != 1L || is.na(nodes) ||
      nodes != as.integer(nodes) || as.integer(nodes) != 15L) {
    stop("The frozen v2.5 reference fit uses exactly 15 GH nodes.",
         call. = FALSE)
  }
  if (!is.numeric(weight_decay) || length(weight_decay) != 1L ||
      !is.finite(weight_decay) || weight_decay != 0.01) {
    stop("The frozen v2.5 linear deviation penalty is exactly 0.01.",
         call. = FALSE)
  }
  nodes <- as.integer(nodes)
  list(
    list(name = "q1_constant", q = 1L, hidden = integer(),
         mean_family = "constant", weight_decay = 0,
         integration = "gh", n_nodes = nodes),
    list(name = "q1_linear", q = 1L, hidden = integer(),
         mean_family = "linear", weight_decay = as.numeric(weight_decay),
         integration = "gh", n_nodes = nodes)
  )
}

.scmix_v25_fit_seed <- function(outcome_seed) {
  if (!is.numeric(outcome_seed) || length(outcome_seed) != 1L ||
      is.na(outcome_seed) || !is.finite(outcome_seed) ||
      outcome_seed != as.integer(outcome_seed)) {
    stop("The v2.5 outcome seed must be one integer.", call. = FALSE)
  }
  seed <- as.double(outcome_seed) + 500000
  if (seed < 1 || seed > .Machine$integer.max) {
    stop("The v2.5 fit seed lies outside R's integer seed range.",
         call. = FALSE)
  }
  as.integer(seed)
}

.scmix_v25_fit_controls <- function(verbose = FALSE) {
  list(
    q = 1L, n_epochs = 400L, learning_rate = 0.01,
    n_starts = 2L, mu_bound = 10, kappa_bound = 10,
    alpha_bound = 5, a_bound = 10, weight_bound = 10,
    opt_tol = 0, grad_tol = 0, protocol_mode = "calibration",
    kkt_tol = NULL, polish_max_iter = 50L, polish_history_size = 50L,
    polish_tolerance_grad = 1e-10, polish_tolerance_change = 1e-12,
    polish_lr = 1, nested_objective_tol = 1e-6,
    nested_fit_relative_cutoff = NULL, keep_cv_fits = FALSE,
    device = "cpu", verbose = isTRUE(verbose)
  )
}

.scmix_v25_one_thread <- function(.local_envir = parent.frame()) {
  withr::local_envvar(c(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1"), .local_envir = .local_envir)
  suppressWarnings(torch::torch_set_num_threads(1L))
  if (!identical(as.integer(torch::torch_get_num_threads()), 1L)) {
    stop("Torch could not be restricted to one v2.5 worker thread.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.scmix_v25_fit_data <- function(template, outcome) {
  required_template <- c("deltaX", "Z", "deltaX_names", "Z_names")
  required_outcome <- c("source_row_index", "respondent_id", "y")
  if (!is.list(template) ||
      !all(required_template %in% names(template)) ||
      !is.list(outcome) ||
      !all(required_outcome %in% names(outcome))) {
    stop("The v2.5 fit requires a template and sealed row-level outcome.",
         call. = FALSE)
  }
  source_row_raw <- outcome$source_row_index
  if (!is.numeric(source_row_raw) || !length(source_row_raw) ||
      anyNA(source_row_raw) || any(!is.finite(source_row_raw)) ||
      any(source_row_raw != as.integer(source_row_raw))) {
    stop("The v2.5 source-row index is malformed.", call. = FALSE)
  }
  source_row_index <- as.integer(source_row_raw)
  if (anyNA(source_row_index) ||
      any(source_row_index < 1L) ||
      any(source_row_index > nrow(template$deltaX))) {
    stop("The v2.5 source-row index is malformed.", call. = FALSE)
  }
  if (!is.matrix(template$deltaX) || !is.numeric(template$deltaX) ||
      !is.matrix(template$Z) || !is.numeric(template$Z) ||
      nrow(template$Z) != nrow(template$deltaX) ||
      any(!is.finite(template$deltaX)) || any(!is.finite(template$Z)) ||
      length(template$deltaX_names) != ncol(template$deltaX) ||
      length(template$Z_names) != ncol(template$Z)) {
    stop("The response-free v2.5 design matrices are malformed.",
         call. = FALSE)
  }
  deltaX <- as.matrix(template$deltaX[source_row_index, , drop = FALSE])
  Z <- as.matrix(template$Z[source_row_index, , drop = FALSE])
  storage.mode(deltaX) <- "double"
  storage.mode(Z) <- "double"
  colnames(deltaX) <- template$deltaX_names
  colnames(Z) <- template$Z_names
  check_carried_copy <- function(carried, expected, label) {
    if (is.null(carried)) return(invisible(TRUE))
    carried <- as.matrix(carried)
    storage.mode(carried) <- "double"
    if (!identical(dim(carried), dim(expected)) ||
        !isTRUE(all.equal(carried, expected, tolerance = 0,
                          check.attributes = FALSE))) {
      stop("The sealed outcome's carried ", label,
           " differs from its response-free source rows.", call. = FALSE)
    }
    invisible(TRUE)
  }
  check_carried_copy(outcome$deltaX, deltaX, "deltaX")
  check_carried_copy(outcome$Z, Z, "Z")
  y <- as.numeric(outcome$y)
  respondent_id <- outcome$respondent_id
  if (length(y) != nrow(deltaX) || length(respondent_id) != nrow(deltaX) ||
      anyNA(y) || any(!y %in% c(0, 1)) || anyNA(respondent_id)) {
    stop("The sealed v2.5 outcomes do not match their source rows.",
         call. = FALSE)
  }
  list(deltaX = deltaX, Z = Z, y = y,
       respondent_id = respondent_id,
       source_row_index = source_row_index)
}

.scmix_v25_fit_metrics <- function(full_tuning, nested_tuning) {
  sets <- c(
    list(full = full_tuning$refit$multistart_artifacts),
    stats::setNames(lapply(nested_tuning$tuning, function(x)
      x$refit$multistart_artifacts),
      paste0("outer_", seq_along(nested_tuning$tuning))))
  if (length(sets) != 3L || any(vapply(sets, is.null, logical(1L))) ||
      any(vapply(sets, length, integer(1L)) != 2L)) {
    stop("The v2.5 fit did not retain two starts in every required scope.",
         call. = FALSE)
  }
  invisible(lapply(sets, .scmix_validate_multistart_artifacts))
  state_is_finite <- function(x) {
    arrays <- c(x$network_state$state_dict,
                list(A_optimization = x$A_optimization,
                     kappa = x$kappa))
    length(arrays) > 0L && all(vapply(arrays, function(value) {
      value <- suppressWarnings(as.numeric(value))
      length(value) > 0L && all(is.finite(value))
    }, logical(1L)))
  }
  surface <- do.call(rbind, lapply(names(sets), function(scope) {
    do.call(rbind, lapply(sets[[scope]], function(x) data.frame(
      fit_scope = scope, start_id = x$start_id,
      state_hash = x$state_hash, selected = isTRUE(x$selected),
      polished = isTRUE(x$polished),
      polish_accepted = isTRUE(x$optimizer_evidence$polish$accepted),
      polish_nonworsening = isTRUE(
        x$optimizer_evidence$polish$nonworsening),
      projection_unchanged = identical(
        x$optimizer_evidence$polish$projection_changed, FALSE),
      returned_matches_accepted_objective = isTRUE(
        x$optimizer_evidence$polish$returned_matches_accepted_objective),
      mean_objective = x$mean_objective,
      objective_relative_gap = x$relative_objective_from_best,
      block_kkt = x$optimizer_evidence$kkt$returned$maximum,
      dtype = x$dtype, state_finite = state_is_finite(x),
      stringsAsFactors = FALSE)))
  }))
  surface$near_optimum <- is.finite(surface$objective_relative_gap) &
    surface$objective_relative_gap <= 1e-6
  selected_surface <- surface[surface$selected, , drop = FALSE]
  near_surface <- surface[surface$near_optimum, , drop = FALSE]
  if (nrow(selected_surface) != length(sets) ||
      any(table(selected_surface$fit_scope) != 1L) ||
      nrow(near_surface) < length(sets) ||
      !all(selected_surface$near_optimum)) {
    stop("The v2.5 selected and near-optimum start sets are malformed.",
         call. = FALSE)
  }
  selected <- c(list(full = full_tuning$refit),
                stats::setNames(lapply(nested_tuning$tuning,
                                       function(x) x$refit),
                                paste0("outer_", seq_along(
                                  nested_tuning$tuning))))
  nested_gap <- vapply(selected, function(x) {
    gate <- x$optimization$nested_objective_gate
    value <- if (is.list(gate)) gate$relative_violation else NULL
    if (!is.list(gate) || !isTRUE(gate$applicable) ||
        !is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value < 0) {
      stop("A selected v2.5 fit lacks its applicable finite nesting gate.",
           call. = FALSE)
    }
    as.numeric(value)
  }, numeric(1L))
  loading_error <- vapply(selected, function(x) {
    evidence <- x$loading_contract$gate_evidence$L002$fit_scoped_evidence
    if (!is.list(evidence) || length(evidence) != 1L) {
      stop("A v2.5 fit lacks live loading-scale invariance evidence.",
           call. = FALSE)
    }
    .scmix_validate_loading_invariance_evidence(evidence[[1L]])
    evidence[[1L]]$maximum_absolute_probability_difference
  }, numeric(1L))
  list(
    start_surface = surface,
    selected_start_surface = selected_surface,
    near_optimum_start_surface = near_surface,
    maximum_block_kkt = max(selected_surface$block_kkt),
    maximum_nested_objective_gap = max(nested_gap),
    maximum_loading_probability_error = max(loading_error),
    every_polish_completed = all(near_surface$polished),
    every_polish_accepted = all(
      near_surface$polish_accepted & near_surface$polish_nonworsening &
        near_surface$projection_unchanged &
        near_surface$returned_matches_accepted_objective),
    every_state_float64 = all(near_surface$dtype == "float64"),
    all_finite = all(near_surface$state_finite) &&
      all(is.finite(c(selected_surface$mean_objective,
                      selected_surface$objective_relative_gap,
                      selected_surface$block_kkt, nested_gap,
                      loading_error))),
    fit_gate_start_scope = paste0(
      "selected_full_and_outer_states;near_optimum_integrity_only;",
      "discarded_far_starts_descriptive"))
}

.scmix_v25_fit_q1 <- function(template, outcome, verbose = FALSE) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("The v2.5 evaluator requires the torch package.", call. = FALSE)
  }
  .scmix_v25_one_thread()
  dat <- .scmix_v25_fit_data(template, outcome)
  deltaX <- dat$deltaX
  Z <- dat$Z
  rid <- dat$respondent_id
  fit_seed <- .scmix_v25_fit_seed(outcome$seed)
  outer_fold_id <- .sc_make_folds(
    rid, K = 2L, seed = .sc_comp_seed(fit_seed, "v25-outer-folds"))
  controls <- .scmix_v25_fit_controls(verbose)
  grid <- .scmix_v25_fit_grid(15L, 0.01)
  full <- do.call(scmix_tune_matrix, c(list(
    deltaX = deltaX, y = dat$y, Z = Z, respondent_id = rid,
    grid = grid, K = 2L, seed = fit_seed), controls))
  nested <- do.call(scmix_tune_outer_matrix, c(list(
    deltaX = deltaX, y = dat$y, Z = Z, respondent_id = rid,
    grid = grid, outer_K = 2L, inner_K = 2L,
    outer_fold_id = outer_fold_id, seed = fit_seed), controls))
  assembled <- scmix_assemble_nested(
    nested, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  metrics <- .scmix_v25_fit_metrics(full, nested)
  if (!identical(full$refit$specification$q, 1L) ||
      !identical(assembled$q, 1L) || !identical(assembled$K, 2L) ||
      !isTRUE(metrics$every_state_float64) || !isTRUE(metrics$all_finite)) {
    stop("The numerical fit is not the frozen v2.5 q=1 computation.",
         call. = FALSE)
  }
  list(full_tuning = full, nested_tuning = nested, assembled = assembled,
       fit_seed = fit_seed, outer_fold_id = outer_fold_id,
       metrics = metrics)
}

.scmix_v25_gh_grid <- function(nodes) {
  if (!is.numeric(nodes) || length(nodes) != 1L || is.na(nodes) ||
      nodes != as.integer(nodes) || as.integer(nodes) != 31L) {
    stop("The frozen v2.5 refinement uses exactly 31 GH nodes.",
         call. = FALSE)
  }
  nodes <- as.integer(nodes)
  .sc_mixed_grid(q = 1L, integration = "gh", n_nodes = as.integer(nodes),
                 n_draws = 1L, seed = 1L, antithetic = TRUE,
                 scramble = TRUE)
}

.scmix_v25_validate_selected_fit <- function(fit) {
  if (!inherits(fit, "scmix_tuned_matrix_fit") ||
      !inherits(fit$network_state, "scmix_network_state") ||
      !is.list(fit$raw_data) || !is.list(fit$preprocessing)) {
    stop("The v2.5 repolish requires one portable selected matrix fit.",
         call. = FALSE)
  }
  .scmix_validate_network_state(fit$network_state)
  .scmix_validate_loading_contract(fit)
  spec <- fit$specification
  if (!is.list(spec)) {
    stop("The selected fit is not an exact frozen v2.5 GH15 fit.",
         call. = FALSE)
  }
  architecture <- fit$network_state$architecture
  expected_decay <- if (identical(spec$mean_family, "constant")) {
    0
  } else 0.01
  valid <- is.list(spec) && identical(spec$q, 1L) &&
    spec$mean_family %in% c("constant", "linear") &&
    identical(spec$hidden, integer()) && identical(spec$integration, "gh") &&
    identical(spec$n_nodes, 15L) &&
    identical(as.numeric(spec$weight_decay), expected_decay) &&
    identical(fit$protocol_mode, "calibration") &&
    is.list(fit$integration_grid) &&
    identical(dim(as.matrix(fit$integration_grid$U)), c(15L, 1L)) &&
    length(fit$integration_grid$w) == 15L &&
    identical(architecture$q, 1L) &&
    identical(architecture$mean_family, spec$mean_family) &&
    identical(architecture$hidden, integer()) &&
    identical(architecture$compute_dtype, "float64") &&
    identical(unname(architecture$mu_bound), 10) &&
    identical(unname(architecture$kappa_bound), 10) &&
    identical(unname(architecture$alpha_bound), 5) &&
    identical(unname(architecture$a_bound), 10) &&
    identical(unname(architecture$weight_bound), 10)
  if (!isTRUE(valid)) {
    stop("The selected fit is not an exact frozen v2.5 GH15 fit.",
         call. = FALSE)
  }
  invisible(fit)
}

## Replace only the quadrature used to evaluate the selected GH15 states.
## Parameters, folds, preprocessing, and respondent order remain bitwise fixed.
.scmix_v25_fixed_state_gh <- function(assembled, nodes = 31L) {
  if (!is.list(assembled) || !identical(assembled$q, 1L) ||
      !identical(assembled$K, 2L) ||
      !is.list(assembled$integration_grids_folds) ||
      length(assembled$integration_grids_folds) != 2L ||
      !all(vapply(assembled$integration_grids_folds, function(grid) {
        is.list(grid) &&
          identical(dim(as.matrix(grid$U)), c(15L, 1L)) &&
          length(grid$w) == 15L
      }, logical(1L)))) {
    stop("The fixed-state v2.5 refinement requires an assembled q=1 fit.",
         call. = FALSE)
  }
  out <- assembled
  gh <- .scmix_v25_gh_grid(nodes)
  out$gh_folds <- replicate(out$K, gh, simplify = FALSE)
  out$gh <- gh
  out$integration_grid <- gh
  out$integration_grids_folds <- out$gh_folds
  out$v25_integration_branch <- "fixed_GH15_state_evaluated_at_GH31"
  out
}

.scmix_v25_repolish_selected <- function(fit, nodes = 31L,
                                          verbose = FALSE) {
  .scmix_v25_one_thread()
  .scmix_v25_validate_selected_fit(fit)
  spec <- fit$specification
  dat <- fit$raw_data
  z_fit <- .sc_apply_z_transform(dat$Z, fit$preprocessing$Z)
  scale <- as.numeric(fit$preprocessing$deltaX$scale)
  dx_fit <- sweep(dat$deltaX, 2L, scale, `/`)
  live <- scmix_restore_network(fit$network_state, device = "cpu")
  warm <- live$state_dict()
  gh <- .scmix_v25_gh_grid(nodes)
  trained <- .sc_train_mixed_multistart(
    deltaX = dx_fit, y = dat$y, Z = z_fit,
    respondent_id = dat$respondent_id, gh = gh,
    hidden = spec$hidden, mean_family = spec$mean_family,
    n_epochs = 0L, learning_rate = 0.01,
    weight_decay = spec$weight_decay, n_starts = 1L,
    seed = .sc_comp_seed(1L, "v25-gh31-repolish"),
    device = "cpu", verbose = isTRUE(verbose), warm_state = warm,
    warm_start_mode = "exact_state", early_stop = FALSE,
    opt_tol = 0, grad_tol = 0, kkt_tol = NULL,
    protocol_mode = "calibration",
    calibration_verification = fit$calibration_verification,
    polish_max_iter = 50L, polish_history_size = 50L,
    polish_tolerance_grad = 1e-10, polish_tolerance_change = 1e-12,
    polish_lr = 1, mu_bound = 10, kappa_bound = 10,
    alpha_bound = 5, a_bound = 10, weight_bound = 10,
    coefficient_scale = scale)
  A <- .scmix_loading_to_raw(trained$A, scale)
  scope <- paste0(fit$scope, "; GH31 repolish from exact GH15 state")
  loading <- .scmix_loading_invariance_evidence(
    trained$net, dat$deltaX, dat$y, z_fit, dat$respondent_id, gh, scale,
    analysis_signature = fit$analysis_signature, scope = scope,
    protocol_mode = "calibration", calibration_verified = FALSE,
    calibration_hash = NA_character_, probability_cutoff = NULL,
    device = "cpu")
  state <- .scmix_capture_network_state(
    trained$net, p = ncol(dat$deltaX), p_Z = ncol(dat$Z), q = 1L,
    hidden = spec$hidden, mean_family = spec$mean_family,
    mu_bound = 10, kappa_bound = 10, alpha_bound = 5,
    a_bound = 10, weight_bound = 10, coefficient_scale = scale,
    z_transform = fit$preprocessing$Z,
    dx_transform = fit$preprocessing$deltaX,
    coefficient_names = colnames(dat$deltaX),
    moderator_names = colnames(dat$Z), integration_grid = gh,
    analysis_signature = fit$analysis_signature, scope = scope)
  state <- .scmix_attach_network_loading_evidence(state, loading)
  out <- fit
  out$net <- trained$net
  out$network_state <- state
  out$multistart_artifacts <- .scmix_complete_multistart_artifacts(
    trained, state, scale)
  out$mu <- sweep(.sc_predict_beta(trained$net, z_fit), 2L, scale, `/`)
  out$A <- A
  out$Sigma <- tcrossprod(A)
  out$kappa <- trained$kappa
  out$integration_grid <- gh
  out$specification$n_nodes <- as.integer(nodes)
  out$optimization <- .sc_comp_fit_summary(trained)
  out$scope <- scope
  out$v25_integration_branch <- "GH31_repolish_from_exact_GH15_state"
  out <- .scmix_add_loading_contract(
    out, q = 1L, raw_full = A, optimization_full = trained$A,
    coefficient_scale_full = scale, invariance_evidence = loading,
    source = "v2.5 GH31 repolish from the exact selected GH15 state")
  class(out) <- c("scmix_tuned_matrix_fit", "list")
  out
}

.scmix_v25_repolish_bundle <- function(fit_bundle, verbose = FALSE) {
  if (!is.list(fit_bundle) ||
      !inherits(fit_bundle$full_tuning$refit, "scmix_tuned_matrix_fit") ||
      !inherits(fit_bundle$nested_tuning, "scmix_nested_tuning") ||
      length(fit_bundle$nested_tuning$tuning) != 2L) {
    stop("The v2.5 GH31 branch requires one complete q=1 fit bundle.",
         call. = FALSE)
  }
  nested <- fit_bundle$nested_tuning
  for (k in seq_along(nested$tuning)) {
    nested$tuning[[k]]$refit <- .scmix_v25_repolish_selected(
      nested$tuning[[k]]$refit, 31L, verbose)
  }
  assembled <- scmix_assemble_nested(
    nested, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  assembled$v25_integration_branch <-
    "GH31_repolish_from_exact_GH15_state"
  list(
    full = .scmix_v25_repolish_selected(
      fit_bundle$full_tuning$refit, 31L, verbose),
    nested_tuning = nested, assembled = assembled)
}

.scmix_v25_add_repolish_metrics <- function(metrics, repolished_bundle) {
  fits <- c(
    list(full = repolished_bundle$full),
    stats::setNames(lapply(repolished_bundle$nested_tuning$tuning,
                           function(x) x$refit),
                    paste0("outer_", seq_along(
                      repolished_bundle$nested_tuning$tuning))))
  if (!is.list(metrics) || length(fits) != 3L ||
      any(vapply(fits, is.null, logical(1L)))) {
    stop("The v2.5 GH31 repolish metrics are incomplete.", call. = FALSE)
  }
  surface <- do.call(rbind, lapply(names(fits), function(scope) {
    fit <- fits[[scope]]
    valid_grid <- function(grid) {
      is.list(grid) &&
        identical(dim(as.matrix(grid$U)), c(31L, 1L)) &&
        length(grid$w) == 31L &&
        all(is.finite(c(as.numeric(grid$U), as.numeric(grid$w)))) &&
        all(as.numeric(grid$w) > 0)
    }
    spec <- fit$specification
    if (!inherits(fit, "scmix_tuned_matrix_fit") || !is.list(spec) ||
        !identical(spec$q, 1L) ||
        !spec$mean_family %in% c("constant", "linear") ||
        !identical(spec$hidden, integer()) ||
        !identical(spec$integration, "gh") ||
        !identical(spec$n_nodes, 31L) ||
        !identical(fit$protocol_mode, "calibration") ||
        !identical(fit$v25_integration_branch,
                   "GH31_repolish_from_exact_GH15_state") ||
        !valid_grid(fit$integration_grid) ||
        !valid_grid(fit$network_state$integration_grid)) {
      stop("A v2.5 repolish metric is not bound to its GH31 fit scope.",
           call. = FALSE)
    }
    .scmix_validate_network_state(fit$network_state)
    .scmix_validate_loading_contract(fit)
    artifact <- fit$multistart_artifacts
    if (length(artifact) != 1L || !isTRUE(artifact[[1L]]$selected)) {
      stop("A v2.5 GH31 scope does not contain one selected state.",
           call. = FALSE)
    }
    .scmix_validate_multistart_artifacts(artifact)
    artifact <- artifact[[1L]]
    starts <- fit$optimization$starts
    loading <- fit$loading_contract$gate_evidence$L002$fit_scoped_evidence
    if (!is.list(loading) || length(loading) != 1L) {
      stop("A v2.5 GH31 fit lacks loading-scale evidence.", call. = FALSE)
    }
    .scmix_validate_loading_invariance_evidence(loading[[1L]])
    arrays <- c(artifact$network_state$state_dict,
                list(A_optimization = artifact$A_optimization,
                     kappa = artifact$kappa))
    state_finite <- length(arrays) > 0L && all(vapply(
      arrays, function(x) {
        x <- suppressWarnings(as.numeric(x))
        length(x) > 0L && all(is.finite(x))
      }, logical(1L)))
    data.frame(
      fit_scope = scope,
      exact_state_zero_adam = is.data.frame(starts) && nrow(starts) == 1L &&
        identical(as.integer(starts$epochs[[1L]]), 0L) &&
        identical(as.character(starts$warm_start_mode[[1L]]),
                  "exact_state") &&
        identical(as.numeric(starts$exact_warm_load_difference[[1L]]), 0),
      polished = isTRUE(artifact$polished),
      polish_accepted = isTRUE(
        artifact$optimizer_evidence$polish$accepted),
      polish_nonworsening = isTRUE(
        artifact$optimizer_evidence$polish$nonworsening),
      projection_unchanged = identical(
        artifact$optimizer_evidence$polish$projection_changed, FALSE),
      returned_matches_accepted_objective = isTRUE(
        artifact$optimizer_evidence$polish$
          returned_matches_accepted_objective),
      block_kkt = as.numeric(
        artifact$optimizer_evidence$kkt$returned$maximum),
      loading_probability_error = as.numeric(
        loading[[1L]]$maximum_absolute_probability_difference),
      dtype = artifact$dtype,
      state_finite = state_finite,
      objective_finite = is.finite(artifact$mean_objective),
      stringsAsFactors = FALSE)
  }))
  repolish_integrity <- with(surface,
    all(exact_state_zero_adam) && all(polished) &&
      all(polish_accepted) && all(polish_nonworsening) &&
      all(projection_unchanged) &&
      all(returned_matches_accepted_objective) &&
      all(dtype == "float64") && all(state_finite) &&
      all(objective_finite) && all(is.finite(block_kkt)) &&
      all(is.finite(loading_probability_error)))
  metrics$repolished_selected_surface <- surface
  metrics$maximum_block_kkt <- max(
    metrics$maximum_block_kkt, surface$block_kkt)
  metrics$maximum_loading_probability_error <- max(
    metrics$maximum_loading_probability_error,
    surface$loading_probability_error)
  metrics$every_polish_completed <-
    isTRUE(metrics$every_polish_completed) && all(surface$polished)
  metrics$every_polish_accepted <-
    isTRUE(metrics$every_polish_accepted) &&
    all(surface$polish_accepted & surface$polish_nonworsening &
          surface$projection_unchanged &
          surface$returned_matches_accepted_objective)
  metrics$every_state_float64 <-
    isTRUE(metrics$every_state_float64) && all(surface$dtype == "float64")
  metrics$all_finite <- isTRUE(metrics$all_finite) && repolish_integrity
  metrics$every_repolish_exact_state <- all(surface$exact_state_zero_adam)
  metrics
}
