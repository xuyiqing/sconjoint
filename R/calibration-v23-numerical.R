## Fail-closed numerical calibration runner for the prospective v2.3 study.
##
## This file intentionally separates three objects that are easy to conflate.
## A generated-outcome artifact binds synthetic choices to a signed,
## response-free design.  A base-fit artifact is produced only by the package's
## actual q = 1 nested/full fitting path.  A numerical-evidence bundle adds the
## target-, integration-, derivative-, and rank-specific evidence that cannot
## be inferred from a generic fit.  Only the last object contains all twelve
## numerical-primary inputs.  Missing evidence yields an explicitly incomplete
## object whose class is not accepted by the Step-3 threshold selector.

.scmix_v23_numerical_constants <- c(
  "kkt_cutoff",
  "integration_target_se_cutoff",
  "integration_if_l2_cutoff",
  "integration_objective_relative_cutoff",
  "integration_score_relative_cutoff",
  "integration_covariance_relative_cutoff",
  "near_optimum_objective_cutoff",
  "multistart_target_se_cutoff",
  "nested_fit_relative_cutoff",
  "loading_probability_invariance_cutoff",
  "derivative_cutoff",
  "numerical_rank_tolerance"
)

.scmix_v23_numerical_missing_primitives <- function() {
  primitive <- c(
      "target_recomputation_by_start_and_scope",
      "level_b_fixed_state_target_and_IF_evaluation",
      "level_b_repolish_from_each_level_a_state",
      "signed_derivative_coordinate_evaluator",
      "known_rank_information_benchmark",
      "analysis_linked_common_multiplier_bank",
      "fault_specific_actual_fit_scheduler",
      "q2_integration_sensitivity_adapter",
      "joint_near_optimum_and_target_stability_selection")
  expected_symbol <- c(
    ".scmix_v23_target_recomputation_adapter",
    ".scmix_v23_fixed_level_b_adapter",
    ".scmix_v23_repolish_level_b_adapter",
    ".scmix_v23_derivative_coordinate_adapter",
    ".scmix_v23_known_rank_benchmark_adapter",
    ".scmix_v23_multiplier_bank_adapter",
    ".scmix_v23_fault_fit_scheduler",
    ".scmix_v23_q2_sentinel_adapter",
    ".scmix_v23_joint_f008_selector")
  data.frame(
    primitive = primitive,
    expected_symbol = expected_symbol,
    available = vapply(expected_symbol, exists, logical(1L),
                       mode = "function", inherits = TRUE),
    package_status = c(
      "constructor exists but a concrete target evaluator is application specific",
      "artifact constructor exists but no generic target/Riesz callback exists",
      "callback contract exists but portable-state repolish is application specific",
      "audit constructor exists but the frozen coordinate map is target specific",
      "no design-independent truth label can be recovered from a fitted data set",
      "constructor requires the target-specific signed analysis contract",
      "base fitting is implemented but each registered numerical perturbation needs an explicit actual-fit adapter",
      "the validator is frozen but product-grid q2 evaluations remain application specific",
      "raw F008 surface is available but the two cutoffs must be selected jointly"
    ),
    required_resolution = c(
      "supply intact scmix_v22_target_recomputation artifacts for every registered start",
      "supply an intact scmix_v22_integration_refinement artifact",
      "supply the repolished branch inside that integration artifact",
      "supply an intact scmix_v22_derivative_audit artifact",
      "supply an intact scmix_v23_known_rank_evidence artifact",
      "supply an intact 9999-draw scmix_v22_multiplier_bank linked to the analysis",
      "fit every severity from the registry to the same sealed outcome and retain its signed evidence bundle",
      "supply signed q2 GH5x5/GH9x9/GH15x15 comparisons and a GH25x25 gold evaluation as sensitivity evidence only",
      "select the objective-gap and target-SE cutoffs on the frozen calibration stream"
    ),
    stringsAsFactors = FALSE
  )
}

.scmix_v23_numerical_readiness <- function(
    artifacts = list(), plan = scmix_v23_calibration_plan()) {
  if (!is.list(artifacts) || is.null(names(artifacts)) && length(artifacts)) {
    stop("`artifacts` must be an optionally named list.", call. = FALSE)
  }
  primitives <- .scmix_v23_numerical_missing_primitives()
  truth_manifest <- tryCatch(scmix_v23_truth_manifest(plan),
                             error = function(e) e)
  truth_ready <- !inherits(truth_manifest, "error") &&
    .scmix_v23_step3_intact(truth_manifest, "scmix_v23_truth_manifest") &&
    identical(truth_manifest$plan_fingerprint, plan$fingerprint) &&
    identical(truth_manifest$no_frozen_design_outcomes_before_freeze, TRUE)
  code_ready <- isTRUE(all(primitives$available))
  intact <- function(name, class_name, stream = NULL) {
    x <- artifacts[[name]]
    ok <- !is.null(x) && .scmix_v23_step3_intact(x, class_name) &&
      identical(x$plan_fingerprint, plan$fingerprint)
    if (!is.null(stream)) ok <- ok && identical(x$stream, stream)
    isTRUE(ok)
  }
  evidence <- c(
    numerical_calibration = intact(
      "numerical_calibration", "scmix_v23_numerical_calibration_run",
      "calibration"),
    ordinary_numerical_validation = intact(
      "ordinary_numerical_validation", "scmix_v23_numerical_layer_run",
      "validation"),
    untouched_transport = intact(
      "untouched_transport", "scmix_v23_transport_validation"),
    statistical_validation = intact(
      "statistical_validation", "scmix_v23_statistical_validation_run"))
  state <- data.frame(
    requirement = c(
      "frozen_response_free_truth_manifest",
      paste0("adapter:", primitives$primitive),
      paste0("evidence:", names(evidence)),
      "marginal_oracle_may_authorize_production"),
    satisfied = c(truth_ready, primitives$available, evidence, FALSE),
    stringsAsFactors = FALSE)
  ready_for_outcome_generation <- truth_ready && code_ready
  ready_for_threshold_selection <- ready_for_outcome_generation &&
    evidence[["numerical_calibration"]]
  ready_for_production_promotion <- ready_for_threshold_selection &&
    all(evidence[c("ordinary_numerical_validation", "untouched_transport",
                   "statistical_validation")])
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    state = state, primitive_status = primitives,
    expected_artifact_classes = c(
      numerical_calibration = "scmix_v23_numerical_calibration_run",
      ordinary_numerical_validation = "scmix_v23_numerical_layer_run",
      untouched_transport = "scmix_v23_transport_validation",
      statistical_validation = "scmix_v23_statistical_validation_run"),
    truth_manifest_fingerprint = if (truth_ready)
      truth_manifest$fingerprint else NA_character_,
    ready_for_outcome_generation = ready_for_outcome_generation,
    ready_for_threshold_selection = ready_for_threshold_selection,
    ready_for_production_promotion = ready_for_production_promotion,
    marginal_oracle_authorizes_production = FALSE,
    marginal_oracle_limitation = paste(
      "Current marginal-oracle evidence is a geometry and face-validity",
      "study. It cannot replace end-to-end statistical validation."),
    creates_production_manifest = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_readiness")
}

.scmix_v23_numerical_contract <- function(
    plan = scmix_v23_calibration_plan(),
    mode = c("base", "transport")) {
  mode <- match.arg(mode)
  if (!.scmix_v23_step3_intact(plan, "scmix_v23_calibration_plan")) {
    stop("`plan` must be the intact frozen v2.3 Step-3 plan.",
         call. = FALSE)
  }
  registered <- plan$constants$constant[
    plan$constants$primary_layer == "numerical"]
  if (!identical(registered, .scmix_v23_numerical_constants)) {
    stop("The frozen numerical-primary constant registry changed.",
         call. = FALSE)
  }
  reference <- plan$severity[
    plan$severity$regime_id == "well_informed_interior" &
      plan$severity$severity_id == "reference", , drop = FALSE]
  if (nrow(reference) != 1L) {
    stop("The frozen numerical reference schedule is absent.", call. = FALSE)
  }
  schedule <- if (identical(mode, "base")) list(
    mode = "base",
    outer_folds = plan$counts[["numerical_ordinary_outer_folds"]],
    inner_folds = plan$counts[["numerical_ordinary_inner_folds"]],
    optimizer_starts = plan$counts[["numerical_ordinary_optimizer_starts"]],
    level_a_nodes =
      plan$counts[["numerical_ordinary_reference_gh_nodes"]],
    level_b_nodes =
      plan$counts[["numerical_ordinary_refined_gh_nodes"]],
    multiplier_draws_required = NA_integer_,
    replications_per_design = c(
      calibration = plan$counts[["numerical_calibration_base_outcomes"]],
      validation = plan$counts[["numerical_validation_base_outcomes"]]),
    seed_layer = "numerical",
    role = "calibration_and_clustered_replay_base") else list(
      mode = "transport",
      outer_folds = plan$counts[["transport_outer_folds"]],
      inner_folds = plan$counts[["transport_inner_folds"]],
      optimizer_starts = plan$counts[["transport_optimizer_starts"]],
      level_a_nodes = plan$counts[["transport_reference_gh_nodes"]],
      level_b_nodes = plan$counts[["transport_refined_gh_nodes"]],
      multiplier_draws_required =
        plan$counts[["transport_common_respondent_multipliers"]],
      replications_per_design = c(
        validation = plan$counts[["transport_validation_reps"]]),
      seed_layer = "transport", role = "untouched_transport_validation")
  payload <- c(list(
    plan_id = plan$plan_id,
    protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    q = 1L,
    production_rank_scope = "q1_only",
    mean_candidates = c("constant", "linear"),
    mean_family_contract = "unpenalized_constant_plus_centered_deviation",
    dtype = "float64",
    device = "cpu",
    adam_epochs = as.integer(reference$error_epochs[[1L]]),
    learning_rate = 0.01,
    deviation_weight_decay = 0.01,
    adam_full_batch = TRUE,
    polish_method = "full_batch_lbfgs_strong_wolfe",
    polish_max_iterations =
      as.integer(reference$error_polish_iterations[[1L]]),
    torch_threads = 1L,
    blas_threads = 1L,
    numerical_constants = registered,
    replay_unit = "base_synthetic_outcome",
    replay_dependence = "clustered_not_independent_Wilson_trials",
    q2_role = "sensitivity_only_no_q1_manifest_authority",
    q2_sentinels = c("GH5x5_to_GH9x9", "GH9x9_to_GH15x15",
                     "gold_GH25x25"),
    creates_production_manifest = FALSE), schedule)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_contract")
}

.scmix_v23_numerical_generated_fingerprint <- function(x) {
  payload <- unclass(x)
  payload$fingerprint <- NULL
  .scmix_v23_step3_sha256(payload)
}

.scmix_v23_check_truth_manifest <- function(
    truth_manifest, plan, design_id = NULL) {
  frozen <- scmix_v23_truth_manifest(plan)
  valid <- .scmix_v23_step3_intact(
    truth_manifest, "scmix_v23_truth_manifest") &&
    identical(truth_manifest$fingerprint, frozen$fingerprint) &&
    identical(truth_manifest$plan_fingerprint, plan$fingerprint) &&
    identical(truth_manifest$application_outcomes_used, FALSE) &&
    identical(truth_manifest$no_frozen_design_outcomes_before_freeze, TRUE) &&
    identical(truth_manifest$toy_fixture_scope,
              "outside_design_matched_step3_study") &&
    !isTRUE(truth_manifest$production_manifest_created) &&
    is.data.frame(truth_manifest$target_truths)
  if (valid && !is.null(design_id)) {
    required <- c("design_id", "regime_id", "severity_id", "target_key",
                  "truth", "application_outcomes_used")
    outcome_flag <- truth_manifest$target_truths$application_outcomes_used
    outcome_free <- if (is.logical(outcome_flag)) {
      isTRUE(all(!outcome_flag))
    } else isTRUE(all(tolower(as.character(outcome_flag)) == "false"))
    valid <- all(required %in% names(truth_manifest$target_truths)) &&
      design_id %in% truth_manifest$target_truths$design_id &&
      outcome_free
  }
  if (!isTRUE(valid)) {
    stop(paste(
      "An intact outcome-free truth manifest frozen before any",
      "design-matched Step-3 outcome is required."), call. = FALSE)
  }
  invisible(truth_manifest)
}

## Seal, rather than merely label, a generated outcome.  The expected seed is
## determined by the prospective plan.  Application outcomes cannot enter this
## constructor because the only accepted design is an intact response-free
## template and the generator must affirm that it used no application outcome.
.scmix_v23_seal_generated_outcome <- function(
    template, generated, truth_manifest,
    stream = c("calibration", "validation", "transport"),
    base_replication, plan = scmix_v23_calibration_plan()) {
  stream <- match.arg(stream)
  .scmix_v23_step3_check_design(template, require_frozen = TRUE)
  if (!.scmix_v23_step3_intact(plan, "scmix_v23_calibration_plan") ||
      !identical(template$plan_id, plan$plan_id) ||
      !identical(template$protocol_id, plan$protocol_id)) {
    stop("The design and prospective plan are not exactly linked.",
         call. = FALSE)
  }
  readiness <- .scmix_v23_numerical_readiness(plan = plan)
  if (!isTRUE(readiness$ready_for_outcome_generation)) {
    missing <- readiness$state$requirement[!readiness$state$satisfied]
    stop(paste0(
      "Numerical outcome generation is disabled until the pre-outcome ",
      "implementation is complete. Missing: ", paste(missing, collapse = ", "),
      "."), call. = FALSE)
  }
  .scmix_v23_check_truth_manifest(
    truth_manifest, plan, template$design_id)
  base_replication <- as.integer(base_replication)
  run_mode <- if (identical(stream, "transport")) "transport" else "base"
  max_rep <- if (identical(run_mode, "transport")) {
    plan$counts[["transport_validation_reps"]]
  } else plan$counts[[paste0("numerical_", stream,
                              "_base_outcomes")]]
  if (length(base_replication) != 1L || is.na(base_replication) ||
      base_replication < 1L || base_replication > max_rep) {
    stop("`base_replication` is outside the frozen numerical stream.",
         call. = FALSE)
  }
  required <- c(
    "design_id", "regime_id", "severity_id", "certification_truth",
    "seed", "respondent_id", "source_respondent_id", "source_row_index",
    "task_id", "y", "observed", "synthetic_probability", "theta", "truth",
    "response_free_hash", "application_outcomes_used", "generator",
    "completion_mechanism")
  if (!is.list(generated) || !all(required %in% names(generated)) ||
      !identical(generated$design_id, template$design_id) ||
      !identical(generated$regime_id, "well_informed_interior") ||
      !identical(generated$response_free_hash,
                 template$hashes[["response_free_payload_hash"]]) ||
      !identical(generated$application_outcomes_used, FALSE) ||
      !is.character(generated$generator) || length(generated$generator) != 1L ||
      !grepl("^task_level_q1_", generated$generator)) {
    stop("Only a task-level q=1 synthetic outcome linked to this design can be sealed.",
         call. = FALSE)
  }
  .scmix_v23_check_truth_manifest(
    truth_manifest, plan, template$design_id)
  design_index <- match(template$design_id, plan$designs$design_id)
  regime_index <- match("well_informed_interior", plan$regimes$regime_id)
  seed_layer <- if (identical(run_mode, "transport"))
    "transport" else "numerical"
  seed_stream <- if (identical(run_mode, "transport"))
    "validation" else stream
  expected_seed <- .scmix_v23_step3_seed(
    plan, seed_layer, seed_stream, design_index, regime_index, 1L,
    base_replication)
  if (!identical(as.integer(generated$seed), expected_seed)) {
    stop("The generated outcome does not use its frozen numerical seed.",
         call. = FALSE)
  }
  n <- template$n_task_rows
  observed <- as.logical(generated$observed)
  y <- as.numeric(generated$y)
  source_row_index <- as.integer(generated$source_row_index)
  probability <- as.numeric(generated$synthetic_probability)
  truth_rows <- truth_manifest$target_truths[
    truth_manifest$target_truths$design_id == template$design_id &
      truth_manifest$target_truths$regime_id == "well_informed_interior" &
      truth_manifest$target_truths$severity_id == "reference", , drop = FALSE]
  recorded_truth <- if (nrow(truth_rows)) stats::setNames(
    as.numeric(truth_rows$truth), as.character(truth_rows$target_key)) else
      numeric()
  generated_truth <- generated$truth
  if (length(y) != n || length(observed) != n || anyNA(observed) ||
      any(!is.finite(y)) || any(!y %in% c(0, 1)) ||
      !all(observed) ||
      !identical(source_row_index, seq_len(n)) ||
      !identical(as.character(generated$source_respondent_id),
                 template$respondent_id) ||
      !identical(as.character(generated$task_id), template$task_id) ||
      length(probability) != n || any(!is.finite(probability)) ||
      any(probability < 0 | probability > 1) ||
      !is.numeric(generated$theta) || any(!is.finite(generated$theta)) ||
      !is.numeric(generated$truth) || any(!is.finite(generated$truth)) ||
      !identical(names(generated_truth), names(recorded_truth)) ||
      !identical(as.numeric(generated_truth), as.numeric(recorded_truth)) ||
      !identical(generated$severity_id, "reference") ||
      !identical(generated$certification_truth, "safe") ||
      !identical(generated$completion_mechanism,
                 "complete_frozen_design")) {
    stop(paste(
      "A numerical base outcome must retain every signed respondent-task row.",
      "Completion reweighting belongs to the statistical interpretation layer."),
      call. = FALSE)
  }
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint, stream = stream,
    run_mode = run_mode,
    truth_manifest_fingerprint = truth_manifest$fingerprint,
    design_id = template$design_id,
    design_template_fingerprint = template$fingerprint,
    response_free_payload_hash =
      template$hashes[["response_free_payload_hash"]],
    regime_id = "well_informed_interior",
    base_replication = base_replication, seed = expected_seed,
    respondent_id = as.character(generated$respondent_id),
    source_respondent_id = as.character(generated$source_respondent_id),
    source_row_index = source_row_index, task_id = template$task_id,
    y = y, observed = observed, synthetic_probability = probability,
    theta = generated$theta, truth = generated$truth,
    generator = generated$generator,
    generator_payload_hash = .scmix_v23_step3_sha256(generated),
    application_outcomes_used = FALSE,
    synthetic_outcome = TRUE
  )
  .scmix_v23_step3_sign(payload, "scmix_v23_generated_outcome")
}

.scmix_v23_validate_generated_outcome <- function(
    x, template, plan = scmix_v23_calibration_plan()) {
  .scmix_v23_step3_check_design(template, require_frozen = TRUE)
  truth_manifest <- scmix_v23_truth_manifest(plan)
  if (!.scmix_v23_step3_intact(x, "scmix_v23_generated_outcome") ||
      !identical(x$plan_fingerprint, plan$fingerprint) ||
      !identical(x$truth_manifest_fingerprint,
                 truth_manifest$fingerprint) ||
      !identical(x$design_template_fingerprint, template$fingerprint) ||
      !identical(x$response_free_payload_hash,
                 template$hashes[["response_free_payload_hash"]]) ||
      !identical(x$application_outcomes_used, FALSE) ||
      !identical(x$synthetic_outcome, TRUE) ||
      !x$stream %in% c("calibration", "validation", "transport") ||
      !identical(x$run_mode,
                 if (identical(x$stream, "transport"))
                   "transport" else "base") ||
      !identical(x$source_respondent_id, template$respondent_id) ||
      !identical(x$source_row_index, seq_len(template$n_task_rows)) ||
      !identical(x$task_id, template$task_id) ||
      length(x$respondent_id) != template$n_task_rows ||
      length(x$y) != template$n_task_rows || any(!x$y %in% c(0, 1)) ||
      length(x$synthetic_probability) != template$n_task_rows ||
      any(!is.finite(x$synthetic_probability)) ||
      any(x$synthetic_probability < 0 | x$synthetic_probability > 1) ||
      !is.numeric(x$theta) || any(!is.finite(x$theta)) ||
      !is.numeric(x$truth) || any(!is.finite(x$truth))) {
    stop("The generated-outcome artifact is absent, altered, or unlinked.",
         call. = FALSE)
  }
  invisible(x)
}

.scmix_v23_numerical_grid <- function(contract, weight_decay = 0.01) {
  list(
    list(name = "q1_constant", q = 1L, hidden = integer(),
         ## There is no deviation block in the constant family.  Its exact
         ## nested reference therefore has zero, rather than copied, decay.
         mean_family = "constant", weight_decay = 0,
         integration = "gh", n_nodes = contract$level_a_nodes),
    list(name = "q1_linear", q = 1L, hidden = integer(),
         mean_family = "linear", weight_decay = weight_decay,
         integration = "gh", n_nodes = contract$level_a_nodes)
  )
}

.scmix_v23_numerical_start_sets <- function(
    full_tuning, nested_tuning, contract) {
  sets <- c(list(full = full_tuning$refit$multistart_artifacts),
            stats::setNames(lapply(nested_tuning$tuning, function(x)
              x$refit$multistart_artifacts),
              paste0("outer_", seq_along(nested_tuning$tuning))))
  if (length(sets) != contract$outer_folds + 1L ||
      any(vapply(sets, is.null, logical(1L)))) {
    stop("The actual fit did not retain the contracted full and outer-fold start sets.",
         call. = FALSE)
  }
  invisible(lapply(sets, .scmix_validate_multistart_artifacts))
  sets
}

.scmix_v23_numerical_fit_metrics <- function(
    full_tuning, nested_tuning, contract) {
  sets <- .scmix_v23_numerical_start_sets(
    full_tuning, nested_tuning, contract)
  records <- unlist(sets, recursive = FALSE, use.names = FALSE)
  if (any(vapply(sets, length, integer(1L)) !=
          contract$optimizer_starts) ||
      any(vapply(records, function(x) !identical(x$dtype, "float64"),
                 logical(1L)))) {
    stop("Every full and outer fit must retain the contracted float64 starts.",
         call. = FALSE)
  }
  kkt <- vapply(records, function(x) {
    value <- x$optimizer_evidence$kkt$returned$maximum
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("A returned-state KKT diagnostic is missing or nonfinite.",
           call. = FALSE)
    }
    value
  }, numeric(1L))
  start_surface <- do.call(rbind, lapply(names(sets), function(scope) {
    do.call(rbind, lapply(sets[[scope]], function(x) data.frame(
      fit_scope = scope, start_id = x$start_id,
      state_hash = x$state_hash, selected = isTRUE(x$selected),
      polished = isTRUE(x$polished), mean_objective = x$mean_objective,
      objective_relative_gap = x$relative_objective_from_best,
      kkt = x$optimizer_evidence$kkt$returned$maximum,
      stringsAsFactors = FALSE)))
  }))
  fits <- c(list(full = full_tuning$refit),
            stats::setNames(lapply(nested_tuning$tuning, `[[`, "refit"),
                            paste0("outer_", seq_along(nested_tuning$tuning))))
  nested <- vapply(fits, function(x) {
    gate <- x$optimization$nested_objective_gate
    if (is.null(gate) || !isTRUE(gate$applicable)) 0 else
      as.numeric(gate$relative_violation)
  }, numeric(1L))
  loading <- vapply(fits, function(x) {
    evidence <- x$loading_contract$gate_evidence$L002$fit_scoped_evidence
    if (!is.list(evidence) || length(evidence) != 1L) {
      stop("A fit lacks its live loading-invariance evidence.",
           call. = FALSE)
    }
    .scmix_validate_loading_invariance_evidence(evidence[[1L]])
    evidence[[1L]]$maximum_absolute_probability_difference
  }, numeric(1L))
  list(
    kkt_cutoff = max(kkt),
    nested_fit_relative_cutoff = max(nested),
    loading_probability_invariance_cutoff = max(loading),
    start_surface = start_surface,
    all_finite = all(is.finite(c(kkt, nested, loading))),
    every_polish_completed = all(start_surface$polished),
    actual_fit_scopes = names(sets),
    actual_start_count = length(records)
  )
}

.scmix_v23_numerical_file_hash <- function(path) {
  if (!file.exists(path)) stop("Numerical artifact file is missing: ", path,
                               call. = FALSE)
  unname(as.character(tools::sha256sum(path)))
}

## Run the real estimator.  This function is deliberately expensive.  It is
## not invoked by package tests and it has no proxy or summary-statistic path.
.scmix_v23_fit_numerical_base <- function(
    template, outcome, artifact_dir,
    plan = scmix_v23_calibration_plan(), n_epochs = 400L,
    learning_rate = 0.01, weight_decay = 0.01,
    overwrite = FALSE, verbose = FALSE) {
  contract <- .scmix_v23_numerical_contract(plan, outcome$run_mode)
  .scmix_v23_validate_generated_outcome(outcome, template, plan)
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("The actual numerical runner requires the torch package.",
         call. = FALSE)
  }
  artifact_dir <- normalizePath(artifact_dir, mustWork = FALSE)
  if (!dir.exists(artifact_dir) && !dir.create(artifact_dir, recursive = TRUE)) {
    stop("Could not create `artifact_dir`.", call. = FALSE)
  }
  artifact_file <- file.path(artifact_dir, "actual-q1-fit.rds")
  if (file.exists(artifact_file) && !isTRUE(overwrite)) {
    stop("Refusing to overwrite an existing numerical fit artifact.",
         call. = FALSE)
  }
  n_epochs <- as.integer(n_epochs)
  if (!identical(n_epochs, contract$adam_epochs) ||
      !identical(as.numeric(learning_rate), contract$learning_rate) ||
      !identical(as.numeric(weight_decay),
                 contract$deviation_weight_decay)) {
    stop("The reference fit must use the frozen optimizer schedule.",
         call. = FALSE)
  }
  withr::local_envvar(c(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1"))
  suppressWarnings(torch::torch_set_num_threads(1L))
  if (!identical(as.integer(torch::torch_get_num_threads()), 1L)) {
    stop("Torch could not be restricted to one thread.", call. = FALSE)
  }
  deltaX <- template$deltaX
  Z <- template$Z
  colnames(deltaX) <- template$deltaX_names
  colnames(Z) <- template$Z_names
  rid <- outcome$respondent_id
  grid <- .scmix_v23_numerical_grid(contract, weight_decay)
  fit_seed <- .sc_comp_seed(outcome$seed, "v23-numerical-fit")
  outer_fold_id <- .sc_make_folds(
    rid, K = contract$outer_folds,
    seed = .sc_comp_seed(fit_seed, "outer-folds"))
  controls <- list(
    q = 1L, n_epochs = n_epochs, learning_rate = learning_rate,
    n_starts = contract$optimizer_starts,
    opt_tol = 0, grad_tol = 0, protocol_mode = "calibration",
    kkt_tol = NULL, polish_max_iter = contract$polish_max_iterations,
    polish_history_size = 50L, polish_tolerance_grad = 1e-10,
    polish_tolerance_change = 1e-12, polish_lr = 1,
    nested_objective_tol = 1e-6, nested_fit_relative_cutoff = NULL,
    keep_cv_fits = FALSE, device = "cpu", verbose = verbose)
  full_tuning <- do.call(scmix_tune_matrix, c(list(
    deltaX = deltaX, y = outcome$y, Z = Z, respondent_id = rid,
    grid = grid, K = contract$inner_folds, seed = fit_seed), controls))
  nested_tuning <- do.call(scmix_tune_outer_matrix, c(list(
    deltaX = deltaX, y = outcome$y, Z = Z, respondent_id = rid,
    grid = grid, outer_K = contract$outer_folds,
    inner_K = contract$inner_folds, outer_fold_id = outer_fold_id,
    seed = fit_seed), controls))
  assembled <- scmix_assemble_nested(
    nested_tuning, require_optimization_gate = FALSE,
    diagnostic_only = TRUE)
  metrics <- .scmix_v23_numerical_fit_metrics(
    full_tuning, nested_tuning, contract)
  if (!identical(full_tuning$refit$specification$q, 1L) ||
      !identical(assembled$q, 1L) ||
      !identical(assembled$K, contract$outer_folds) ||
      !identical(full_tuning$refit$protocol_mode, "calibration") ||
      !isTRUE(metrics$all_finite)) {
    stop("The actual q=1 fit did not satisfy the numerical calibration contract.",
         call. = FALSE)
  }
  bundle <- list(
    full_tuning = scmix_portable_copy(full_tuning),
    nested_tuning = scmix_portable_copy(nested_tuning),
    assembled = scmix_portable_copy(assembled))
  saveRDS(bundle, artifact_file, version = 3, compress = "xz")
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    numerical_contract_fingerprint = contract$fingerprint,
    design_id = template$design_id,
    design_template_fingerprint = template$fingerprint,
    outcome_fingerprint = outcome$fingerprint,
    truth_manifest_fingerprint = outcome$truth_manifest_fingerprint,
    outcome_seed = outcome$seed, stream = outcome$stream,
    run_mode = contract$mode,
    base_replication = outcome$base_replication,
    replay_cluster_id = paste(template$design_id, outcome$stream,
                              sprintf("base_%03d", outcome$base_replication),
                              sep = "::"),
    q = 1L, outer_folds = contract$outer_folds,
    inner_folds = contract$inner_folds,
    optimizer_starts = contract$optimizer_starts,
    level_a_nodes = contract$level_a_nodes,
    level_b_nodes = contract$level_b_nodes,
    multiplier_draws_required = contract$multiplier_draws_required,
    n_epochs = n_epochs, learning_rate = learning_rate,
    weight_decay = weight_decay, dtype = "float64",
    torch_threads = as.integer(torch::torch_get_num_threads()),
    blas_threads_requested = 1L,
    artifact_file = basename(artifact_file),
    artifact_sha256 = .scmix_v23_numerical_file_hash(artifact_file),
    metrics = metrics,
    application_outcomes_used = FALSE,
    synthetic_outcome_only = TRUE,
    production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_base_fit")
}

.scmix_v23_validate_numerical_base_fit <- function(
    x, artifact_dir, plan = scmix_v23_calibration_plan(), reload = TRUE) {
  run_mode <- if (is.character(x$run_mode) && length(x$run_mode) == 1L)
    x$run_mode else "invalid"
  if (!run_mode %in% c("base", "transport")) {
    stop("The numerical base-fit artifact is absent, altered, or off contract.",
         call. = FALSE)
  }
  contract <- .scmix_v23_numerical_contract(plan, run_mode)
  truth_manifest <- scmix_v23_truth_manifest(plan)
  if (!.scmix_v23_step3_intact(x, "scmix_v23_numerical_base_fit") ||
      !identical(x$plan_fingerprint, plan$fingerprint) ||
      !identical(x$numerical_contract_fingerprint, contract$fingerprint) ||
      !identical(x$run_mode, contract$mode) ||
      !identical(x$q, 1L) ||
      !identical(x$outer_folds, contract$outer_folds) ||
      !identical(x$inner_folds, contract$inner_folds) ||
      !identical(x$optimizer_starts, contract$optimizer_starts) ||
      !identical(x$level_a_nodes, contract$level_a_nodes) ||
      !identical(x$level_b_nodes, contract$level_b_nodes) ||
      !identical(x$multiplier_draws_required,
                 contract$multiplier_draws_required) ||
      !identical(x$n_epochs, contract$adam_epochs) ||
      !identical(x$learning_rate, contract$learning_rate) ||
      !identical(x$weight_decay,
                 contract$deviation_weight_decay) ||
      !identical(x$dtype, "float64") ||
      !is.character(x$truth_manifest_fingerprint) ||
      length(x$truth_manifest_fingerprint) != 1L ||
      is.na(x$truth_manifest_fingerprint) ||
      !nzchar(x$truth_manifest_fingerprint) ||
      !identical(x$truth_manifest_fingerprint,
                 truth_manifest$fingerprint) ||
      !identical(x$application_outcomes_used, FALSE) ||
      !identical(x$synthetic_outcome_only, TRUE) ||
      isTRUE(x$production_manifest_created)) {
    stop("The numerical base-fit artifact is absent, altered, or off contract.",
         call. = FALSE)
  }
  path <- file.path(artifact_dir, x$artifact_file)
  if (!identical(.scmix_v23_numerical_file_hash(path), x$artifact_sha256)) {
    stop("The recorded q=1 fit file hash does not match.", call. = FALSE)
  }
  if (isTRUE(reload)) {
    bundle <- readRDS(path)
    if (!is.list(bundle) ||
        !inherits(bundle$full_tuning, "scmix_tuning") ||
        !inherits(bundle$nested_tuning, "scmix_nested_tuning") ||
        !inherits(bundle$assembled, "scmix_nested_assembled") ||
        !identical(bundle$assembled$q, 1L) ||
        !identical(bundle$assembled$K, contract$outer_folds)) {
      stop("The recorded file is not the required actual q=1 fit bundle.",
           call. = FALSE)
    }
    observed <- .scmix_v23_numerical_fit_metrics(
      bundle$full_tuning, bundle$nested_tuning, contract)
    fields <- c("kkt_cutoff", "nested_fit_relative_cutoff",
                "loading_probability_invariance_cutoff", "all_finite",
                "every_polish_completed", "actual_start_count")
    if (!identical(observed[fields], x$metrics[fields]) ||
        !identical(observed$start_surface, x$metrics$start_surface)) {
      stop("Stored numerical summaries do not reproduce from the fit file.",
           call. = FALSE)
    }
  }
  invisible(x)
}

## Generate the one permitted reference outcome and immediately pass it to the
## actual estimator.  This is the entry point for a prospective numerical base
## run.  It consumes the numerical seed stream, never the oracle Monte Carlo
## stream, and writes only provenance and fit artifacts under `artifact_dir`.
.scmix_v23_run_numerical_base <- function(
    template, truth_manifest,
    stream = c("calibration", "validation", "transport"),
    base_replication, artifact_dir,
    plan = scmix_v23_calibration_plan(), n_epochs = 400L,
    learning_rate = 0.01, weight_decay = 0.01,
    overwrite = FALSE, verbose = FALSE) {
  stream <- match.arg(stream)
  .scmix_v23_step3_check_design(template, require_frozen = TRUE)
  if (!.scmix_v23_step3_intact(plan, "scmix_v23_calibration_plan")) {
    stop("`plan` must be the intact frozen v2.3 Step-3 plan.",
         call. = FALSE)
  }
  readiness <- .scmix_v23_numerical_readiness(plan = plan)
  if (!isTRUE(readiness$ready_for_outcome_generation)) {
    missing <- readiness$state$requirement[!readiness$state$satisfied]
    stop(paste0(
      "Numerical outcome generation is disabled until the pre-outcome ",
      "implementation is complete. Missing: ", paste(missing, collapse = ", "),
      "."), call. = FALSE)
  }
  .scmix_v23_check_truth_manifest(
    truth_manifest, plan, template$design_id)
  design_index <- match(template$design_id, plan$designs$design_id)
  regime_index <- match("well_informed_interior", plan$regimes$regime_id)
  if (is.na(design_index) || is.na(regime_index)) {
    stop("The template or reference regime is absent from the plan.",
         call. = FALSE)
  }
  base_replication <- as.integer(base_replication)
  run_mode <- if (identical(stream, "transport")) "transport" else "base"
  contract <- .scmix_v23_numerical_contract(plan, run_mode)
  maximum <- if (identical(run_mode, "transport")) {
    plan$counts[["transport_validation_reps"]]
  } else plan$counts[[paste0("numerical_", stream,
                              "_base_outcomes")]]
  if (length(base_replication) != 1L || is.na(base_replication) ||
      base_replication < 1L || base_replication > maximum) {
    stop("`base_replication` is outside the frozen numerical stream.",
         call. = FALSE)
  }
  artifact_dir <- normalizePath(artifact_dir, mustWork = FALSE)
  if (!dir.exists(artifact_dir) &&
      !dir.create(artifact_dir, recursive = TRUE)) {
    stop("Could not create `artifact_dir`.", call. = FALSE)
  }
  outcome_file <- file.path(artifact_dir, "generated-outcome.rds")
  base_file <- file.path(artifact_dir, "numerical-base-fit.rds")
  protected <- c(outcome_file, base_file,
                 file.path(artifact_dir, "actual-q1-fit.rds"))
  if (any(file.exists(protected)) && !isTRUE(overwrite)) {
    stop("Refusing to overwrite an existing numerical-run artifact.",
         call. = FALSE)
  }
  seed_layer <- if (identical(run_mode, "transport"))
    "transport" else "numerical"
  seed_stream <- if (identical(run_mode, "transport"))
    "validation" else stream
  seed <- .scmix_v23_step3_seed(
    plan, seed_layer, seed_stream, design_index, regime_index, 1L,
    base_replication)
  generated <- .scmix_v23_oracle_generate(
    template, "well_informed_interior", seed, plan,
    severity_id = "reference", require_frozen = TRUE)
  outcome <- .scmix_v23_seal_generated_outcome(
    template, generated, truth_manifest, stream, base_replication, plan)
  saveRDS(outcome, outcome_file, version = 3, compress = "xz")
  base_fit <- .scmix_v23_fit_numerical_base(
    template = template, outcome = outcome, artifact_dir = artifact_dir,
    plan = plan, n_epochs = n_epochs, learning_rate = learning_rate,
    weight_decay = weight_decay, overwrite = overwrite, verbose = verbose)
  saveRDS(base_fit, base_file, version = 3, compress = "xz")
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    design_id = template$design_id, stream = stream,
    run_mode = contract$mode,
    base_replication = base_replication, outcome_seed = seed,
    truth_manifest_fingerprint = truth_manifest$fingerprint,
    replay_cluster_id = base_fit$replay_cluster_id,
    outcome_fingerprint = outcome$fingerprint,
    base_fit_fingerprint = base_fit$fingerprint,
    artifact_files = stats::setNames(
      basename(c(outcome_file, base_file,
                 file.path(artifact_dir, base_fit$artifact_file))),
      c("generated_outcome", "base_fit_record", "actual_q1_fit")),
    artifact_sha256 = stats::setNames(vapply(
      c(outcome_file, base_file,
        file.path(artifact_dir, base_fit$artifact_file)),
      .scmix_v23_numerical_file_hash, character(1L)),
      c("generated_outcome", "base_fit_record", "actual_q1_fit")),
    actual_estimator = c("scmix_tune_matrix", "scmix_tune_outer_matrix",
                         "scmix_assemble_nested"),
    application_outcomes_used = FALSE,
    synthetic_outcome_only = TRUE,
    production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_base_run")
}

.scmix_v23_validate_numerical_base_run <- function(
    x, artifact_dir, plan = scmix_v23_calibration_plan(), reload = TRUE) {
  if (!.scmix_v23_step3_intact(x, "scmix_v23_numerical_base_run") ||
      !identical(x$plan_fingerprint, plan$fingerprint) ||
      !x$run_mode %in% c("base", "transport") ||
      !identical(x$run_mode,
                 if (identical(x$stream, "transport"))
                   "transport" else "base") ||
      !is.character(x$truth_manifest_fingerprint) ||
      length(x$truth_manifest_fingerprint) != 1L ||
      is.na(x$truth_manifest_fingerprint) ||
      !nzchar(x$truth_manifest_fingerprint) ||
      !identical(x$application_outcomes_used, FALSE) ||
      !identical(x$synthetic_outcome_only, TRUE) ||
      isTRUE(x$production_manifest_created) ||
      !identical(names(x$artifact_files),
                 c("generated_outcome", "base_fit_record",
                   "actual_q1_fit")) ||
      !identical(names(x$artifact_sha256), names(x$artifact_files))) {
    stop("The numerical base-run record is absent, altered, or off contract.",
         call. = FALSE)
  }
  paths <- file.path(artifact_dir, x$artifact_files)
  observed <- vapply(paths, .scmix_v23_numerical_file_hash, character(1L))
  names(observed) <- names(x$artifact_files)
  if (!identical(observed, x$artifact_sha256)) {
    stop("A numerical base-run file hash does not match.", call. = FALSE)
  }
  if (isTRUE(reload)) {
    outcome <- readRDS(paths[["generated_outcome"]])
    base_fit <- readRDS(paths[["base_fit_record"]])
    if (!.scmix_v23_step3_intact(outcome, "scmix_v23_generated_outcome") ||
        !identical(outcome$fingerprint, x$outcome_fingerprint) ||
        !identical(outcome$seed, x$outcome_seed) ||
        !identical(outcome$truth_manifest_fingerprint,
                   x$truth_manifest_fingerprint) ||
        !identical(base_fit$fingerprint, x$base_fit_fingerprint) ||
        !identical(base_fit$outcome_fingerprint, outcome$fingerprint)) {
      stop("The numerical outcome and base-fit records do not remain linked.",
           call. = FALSE)
    }
    .scmix_v23_validate_numerical_base_fit(
      base_fit, artifact_dir, plan, reload = TRUE)
  }
  invisible(x)
}

## Numerical-rank tolerance is calibrated first on matrices whose population
## rank is known by construction.  It is not estimated from the same weak
## application fit whose rank it later diagnoses.
.scmix_v23_known_rank_evidence <- function(matrices, truth_ranks,
                                           labels = names(matrices)) {
  if (!is.list(matrices) || !length(matrices) ||
      length(truth_ranks) != length(matrices)) {
    stop("Supply matrices and one known truth rank per matrix.",
         call. = FALSE)
  }
  if (is.null(labels)) labels <- paste0("matrix_", seq_along(matrices))
  if (length(labels) != length(matrices) || anyNA(labels) ||
      any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop("Known-rank benchmark labels must be unique and nonempty.",
         call. = FALSE)
  }
  rows <- lapply(seq_along(matrices), function(j) {
    x <- as.matrix(matrices[[j]])
    rank <- as.integer(truth_ranks[[j]])
    if (!is.numeric(x) || nrow(x) != ncol(x) || any(!is.finite(x)) ||
        is.na(rank) || rank < 0L || rank > nrow(x)) {
      stop("Every known-rank benchmark must be finite, square, and labeled.",
           call. = FALSE)
    }
    singular <- sort(svd(x, nu = 0L, nv = 0L)$d, decreasing = TRUE)
    scale <- max(singular, .Machine$double.xmin)
    ratio <- singular / scale
    data.frame(
      label = labels[[j]], dimension = nrow(x), truth_rank = rank,
      smallest_active_ratio = if (rank) ratio[[rank]] else Inf,
      largest_null_ratio = if (rank < length(ratio)) ratio[[rank + 1L]] else 0,
      matrix_hash = .scmix_v23_step3_sha256(unname(x)),
      stringsAsFactors = FALSE)
  })
  table <- do.call(rbind, rows)
  null_max <- max(table$largest_null_ratio)
  active_min <- min(table$smallest_active_ratio)
  separated <- is.finite(null_max) && is.finite(active_min) &&
    null_max < active_min
  tolerance <- if (separated) {
    if (null_max > 0) sqrt(null_max * active_min) else active_min / 2
  } else NA_real_
  payload <- list(
    protocol_id = .scmix_v23_step3_protocol_id,
    benchmark = table, null_ratio_maximum = null_max,
    active_ratio_minimum = active_min, separated = separated,
    numerical_rank_tolerance = tolerance,
    rule = "geometric_midpoint_between_maximum_known_null_and_minimum_known_active_singular_value_ratios",
    application_outcomes_used = FALSE,
    production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_known_rank_evidence")
}

.scmix_v23_validate_known_rank_evidence <- function(x) {
  if (!.scmix_v23_step3_intact(x, "scmix_v23_known_rank_evidence") ||
      !identical(x$protocol_id, .scmix_v23_step3_protocol_id) ||
      !isTRUE(x$separated) ||
      !is.numeric(x$numerical_rank_tolerance) ||
      length(x$numerical_rank_tolerance) != 1L ||
      !is.finite(x$numerical_rank_tolerance) ||
      x$numerical_rank_tolerance <= x$null_ratio_maximum ||
      x$numerical_rank_tolerance >= x$active_ratio_minimum ||
      !identical(x$application_outcomes_used, FALSE)) {
    stop("Known-rank numerical tolerance evidence is invalid or unseparated.",
         call. = FALSE)
  }
  invisible(x)
}

.scmix_v23_numerical_intact_v22 <- function(x, class_name) {
  validator <- get0(".scmix_v22_comp_intact", mode = "function",
                    inherits = TRUE)
  is.function(validator) && isTRUE(validator(x, class_name))
}

.scmix_v23_numerical_integration_metrics <- function(x, contract) {
  if (!.scmix_v23_numerical_intact_v22(
      x, "scmix_v22_integration_refinement") ||
      !identical(x$protocol_id, .scmix_v23_step3_protocol_id) ||
      !identical(x$protocol_mode, "calibration") ||
      !identical(x$q, 1L) ||
      !identical(x$level_a_rule$resolution, contract$level_a_nodes) ||
      !identical(x$level_b_rule$resolution, contract$level_b_nodes)) {
    stop("Integration evidence does not match the contracted q=1 A-to-B refinement.",
         call. = FALSE)
  }
  target <- unlist(x$worse_of_metrics$target, use.names = FALSE)
  influence <- unlist(x$worse_of_metrics$influence, use.names = FALSE)
  fit <- x$maximum_fit_metrics
  value <- c(
    integration_target_se_cutoff = max(target),
    integration_if_l2_cutoff = max(influence),
    integration_objective_relative_cutoff = unname(fit[["objective"]]),
    integration_score_relative_cutoff = unname(fit[["score"]]),
    integration_covariance_relative_cutoff = unname(fit[["covariance"]]))
  if (any(!is.finite(value)) || any(value < 0)) {
    stop("Integration evidence contains a missing or invalid primary metric.",
         call. = FALSE)
  }
  value
}

## Assemble all non-circular numerical evidence.  F008's two constants remain
## a raw two-dimensional surface until the calibration-stream joint selector
## freezes them.  Thus this constructor cannot masquerade as the
## scmix_v23_numerical_calibration_run consumed by the production selector.
.scmix_v23_numerical_evidence_bundle <- function(
    base_fit, artifact_dir, integration_artifact,
    multistart_artifact, derivative_artifact, rank_evidence,
    analysis_contract, multiplier_bank = NULL,
    plan = scmix_v23_calibration_plan()) {
  .scmix_v23_validate_numerical_base_fit(base_fit, artifact_dir, plan)
  contract <- .scmix_v23_numerical_contract(plan, base_fit$run_mode)
  fit_bundle <- readRDS(file.path(artifact_dir, base_fit$artifact_file))
  fit_respondents <- unique(as.character(
    fit_bundle$full_tuning$refit$raw_data$respondent_id))
  if (!.scmix_v22_intact(
      analysis_contract, "scmix_v22_analysis_contract") ||
      !identical(analysis_contract$analysis_signature,
                 fit_bundle$assembled$analysis_signature)) {
    stop("An intact analysis contract linked to the assembled fit is required.",
         call. = FALSE)
  }
  integration <- .scmix_v23_numerical_integration_metrics(
    integration_artifact, contract)
  if (!identical(integration_artifact$analysis_contract_fingerprint,
                 analysis_contract$fingerprint)) {
    stop("Integration evidence is not linked to the analysis contract.",
         call. = FALSE)
  }
  if (!.scmix_v23_numerical_intact_v22(
      multistart_artifact, "scmix_v22_multistart_stability") ||
      !identical(multistart_artifact$protocol_id, plan$protocol_id) ||
      !identical(multistart_artifact$analysis_contract_fingerprint,
                 analysis_contract$fingerprint) ||
      !identical(multistart_artifact$protocol_mode, "calibration") ||
      !is.data.frame(multistart_artifact$start_details) ||
      !is.data.frame(multistart_artifact$comparisons)) {
    stop("An intact calibration-mode F008 raw surface is required.",
         call. = FALSE)
  }
  if (!.scmix_v23_numerical_intact_v22(
      derivative_artifact, "scmix_v22_derivative_audit") ||
      !identical(derivative_artifact$protocol_id, plan$protocol_id) ||
      !identical(derivative_artifact$analysis_contract_fingerprint,
                 analysis_contract$fingerprint) ||
      !is.matrix(derivative_artifact$metric) ||
      any(!is.finite(derivative_artifact$metric))) {
    stop("An intact finite signed derivative audit is required.",
         call. = FALSE)
  }
  .scmix_v23_validate_known_rank_evidence(rank_evidence)
  multiplier_fingerprint <- NA_character_
  multiplier_draws <- 0L
  if (identical(contract$mode, "transport")) {
    multiplier_link <- multiplier_bank$analysis_contract_fingerprint
    multiplier_linked <- is.character(multiplier_link) &&
      length(multiplier_link) == 1L && !is.na(multiplier_link) &&
      nzchar(multiplier_link) &&
      identical(multiplier_link, analysis_contract$fingerprint)
    if (!inherits(multiplier_bank, "scmix_v22_multiplier_bank") ||
        !.scmix_v22_intact(multiplier_bank, "scmix_v22_multiplier_bank") ||
        !identical(multiplier_bank$protocol_id, plan$protocol_id) ||
        !identical(multiplier_bank$draws,
                   contract$multiplier_draws_required) ||
        !isTRUE(multiplier_bank$final_draw_count) ||
        !multiplier_linked) {
      stop("Transport requires a signed analysis-linked 9,999-draw respondent multiplier bank.",
           call. = FALSE)
    }
    .scmix_v22_validate_bank(
      multiplier_bank, fit_respondents, analysis_contract)
    multiplier_fingerprint <- multiplier_bank$fingerprint
    multiplier_draws <- multiplier_bank$draws
  } else if (!is.null(multiplier_bank)) {
    stop("Ordinary calibration/replay bases use respondent influence SEs, not a multiplier bank.",
         call. = FALSE)
  }
  derivative <- max(derivative_artifact$metric)
  metrics <- c(
    base_fit$metrics[c("kkt_cutoff", "nested_fit_relative_cutoff",
                       "loading_probability_invariance_cutoff")],
    as.list(integration), derivative_cutoff = derivative,
    numerical_rank_tolerance = rank_evidence$numerical_rank_tolerance)
  metrics <- unlist(metrics, use.names = TRUE)
  expected_non_f008 <- setdiff(.scmix_v23_numerical_constants,
                               c("near_optimum_objective_cutoff",
                                 "multistart_target_se_cutoff"))
  metrics <- metrics[expected_non_f008]
  if (!identical(names(metrics), expected_non_f008) ||
      any(!is.finite(metrics)) || any(metrics < 0)) {
    stop("The non-F008 numerical evidence is incomplete.", call. = FALSE)
  }
  f008 <- multistart_artifact$comparisons[, c(
    "fit_scope_id", "target_id", "start_id", "best_start_id",
    "objective_relative_gap", "target_change_over_se"), drop = FALSE]
  if (!nrow(f008) || any(!is.finite(f008$objective_relative_gap)) ||
      any(!is.finite(f008$target_change_over_se)) ||
      any(f008$objective_relative_gap < 0) ||
      any(f008$target_change_over_se < 0)) {
    stop("The raw joint F008 calibration surface is incomplete.",
         call. = FALSE)
  }
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    stream = base_fit$stream, run_mode = base_fit$run_mode,
    design_id = base_fit$design_id,
    base_replication = base_fit$base_replication,
    replay_cluster_id = base_fit$replay_cluster_id,
    base_fit_fingerprint = base_fit$fingerprint,
    outcome_fingerprint = base_fit$outcome_fingerprint,
    non_f008_metrics = metrics,
    f008_joint_surface = f008,
    source_fingerprints = c(
      integration = integration_artifact$fingerprint,
      multistart = multistart_artifact$fingerprint,
      derivative = derivative_artifact$fingerprint,
      rank = rank_evidence$fingerprint,
      multipliers = multiplier_fingerprint,
      analysis_contract = analysis_contract$fingerprint),
    multiplier_draws = multiplier_draws,
    replay_independence_claim = FALSE,
    complete_for_joint_f008_selection = TRUE,
    complete_for_scalar_threshold_selector = FALSE,
    scalar_selector_block_reason = paste(
      "near_optimum_objective_cutoff and multistart_target_se_cutoff",
      "must be selected jointly from f008_joint_surface"),
    application_outcomes_used = FALSE,
    production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_evidence_bundle")
}

.scmix_v23_numerical_bundle_metrics <- function(x) {
  if (!.scmix_v23_step3_intact(
      x, "scmix_v23_numerical_evidence_bundle")) {
    stop("An intact numerical-evidence bundle is required.", call. = FALSE)
  }
  surface <- x$f008_joint_surface
  f008 <- c(
    near_optimum_objective_cutoff = max(surface$objective_relative_gap),
    multistart_target_se_cutoff = max(surface$target_change_over_se))
  out <- c(x$non_f008_metrics, f008)
  out <- out[.scmix_v23_numerical_constants]
  if (!identical(names(out), .scmix_v23_numerical_constants) ||
      any(!is.finite(out)) || any(out < 0)) {
    stop("The numerical evidence does not cover all twelve constants.",
         call. = FALSE)
  }
  out
}

## Bind an algorithmic fault to the same synthetic outcome as its clean fit.
## The two F008 columns below are envelope diagnostics only.  The eventual
## cutoffs are still chosen jointly from each bundle's complete raw surface.
.scmix_v23_numerical_replay <- function(
    reference, fault, regime_id, severity_id, primary_constant_ids,
    plan = scmix_v23_calibration_plan(), mechanism) {
  bundles <- list(reference = reference, fault = fault)
  if (any(!vapply(bundles, .scmix_v23_step3_intact, logical(1L),
                  class_name = "scmix_v23_numerical_evidence_bundle")) ||
      !identical(reference$plan_fingerprint, plan$fingerprint) ||
      !identical(fault$plan_fingerprint, plan$fingerprint) ||
      !identical(reference$design_id, fault$design_id) ||
      !identical(reference$run_mode, fault$run_mode) ||
      !identical(reference$stream, fault$stream) ||
      !identical(reference$base_replication, fault$base_replication) ||
      !identical(reference$replay_cluster_id, fault$replay_cluster_id) ||
      !identical(reference$outcome_fingerprint, fault$outcome_fingerprint)) {
    stop("A numerical replay must reuse the exact signed base outcome.",
         call. = FALSE)
  }
  severity <- plan$severity[
    plan$severity$regime_id == regime_id &
      plan$severity$severity_id == severity_id, , drop = FALSE]
  if (nrow(severity) != 1L) {
    stop("The numerical replay is not in the frozen severity registry.",
         call. = FALSE)
  }
  primary_constant_ids <- as.character(primary_constant_ids)
  eligible <- plan$constants$constant[
    plan$constants$primary_layer == "numerical" &
      plan$constants$targeted_failure_regime == regime_id]
  if (!length(primary_constant_ids) || anyDuplicated(primary_constant_ids) ||
      any(!primary_constant_ids %in% eligible)) {
    stop("Replay primary constants must be prespecified for this failure regime.",
         call. = FALSE)
  }
  if (!is.character(mechanism) || length(mechanism) != 1L ||
      is.na(mechanism) || !nzchar(mechanism)) {
    stop("A nonempty replay mechanism is required.", call. = FALSE)
  }
  clean <- .scmix_v23_numerical_bundle_metrics(reference)
  bad <- .scmix_v23_numerical_bundle_metrics(fault)
  comparison <- data.frame(
    constant = primary_constant_ids,
    reference = unname(clean[primary_constant_ids]),
    fault = unname(bad[primary_constant_ids]),
    fault_to_reference_ratio = unname(
      bad[primary_constant_ids] /
        pmax(clean[primary_constant_ids], .Machine$double.eps)),
    fault_is_worse = unname(bad[primary_constant_ids] >
                              clean[primary_constant_ids]),
    stringsAsFactors = FALSE)
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    stream = reference$stream, run_mode = reference$run_mode,
    design_id = reference$design_id,
    base_replication = reference$base_replication,
    replay_cluster_id = reference$replay_cluster_id,
    outcome_fingerprint = reference$outcome_fingerprint,
    regime_id = regime_id, severity_id = severity_id,
    severity_role = severity$certification_truth[[1L]],
    mechanism = mechanism,
    reference_evidence_fingerprint = reference$fingerprint,
    fault_evidence_fingerprint = fault$fingerprint,
    comparison = comparison,
    every_primary_metric_worse = all(comparison$fault_is_worse),
    paired_replay = TRUE, independent_monte_carlo_trial = FALSE,
    Wilson_denominator_contribution = 0L,
    application_outcomes_used = FALSE,
    production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_replay")
}

## Reproduce the historical double-scaling error as an explicit deterministic
## fault.  The faulty branch divides the already raw loading by the contrast
## scale once more.  Nothing from this branch can become a reported estimate.
.scmix_v23_loading_double_scale_fault <- function(
    base_fit, artifact_dir, plan = scmix_v23_calibration_plan()) {
  .scmix_v23_validate_numerical_base_fit(base_fit, artifact_dir, plan)
  bundle <- readRDS(file.path(artifact_dir, base_fit$artifact_file))
  fit <- bundle$full_tuning$refit
  state <- fit$network_state
  .scmix_validate_network_state(state)
  net <- scmix_restore_network(state, device = "cpu")
  raw <- fit$raw_data
  deltaX <- as.matrix(raw$deltaX)
  Z <- .sc_apply_z_transform(as.matrix(raw$Z), fit$preprocessing$Z)
  scale <- as.numeric(fit$preprocessing$deltaX$scale)
  A_raw <- scmix_loadings(fit, scale = "raw", scope = "full")
  A_opt <- scmix_loadings(fit, scale = "optimization", scope = "full")
  mu_opt <- .sc_predict_beta(net, Z)
  kappa <- as.numeric(fit$kappa)
  U <- as.matrix(fit$integration_grid$U)
  dx_opt <- sweep(deltaX, 2L, scale, `/`)
  fixed <- kappa + rowSums(dx_opt * mu_opt)
  eta_reference <- sweep(dx_opt %*% A_opt %*% t(U), 1L, fixed, `+`)
  A_double_divided <- sweep(A_raw, 1L, scale, `/`)
  mu_raw <- sweep(mu_opt, 2L, scale, `/`)
  fixed_raw <- kappa + rowSums(deltaX * mu_raw)
  eta_fault <- sweep(deltaX %*% A_double_divided %*% t(U),
                     1L, fixed_raw, `+`)
  statistic <- max(abs(stats::plogis(eta_fault) -
                         stats::plogis(eta_reference)))
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    base_fit_fingerprint = base_fit$fingerprint,
    outcome_fingerprint = base_fit$outcome_fingerprint,
    replay_cluster_id = base_fit$replay_cluster_id,
    design_id = base_fit$design_id, stream = base_fit$stream,
    run_mode = base_fit$run_mode,
    base_replication = base_fit$base_replication,
    fault_id = "loading_raw_divided_by_scale_twice",
    constant = "loading_probability_invariance_cutoff",
    observed = statistic,
    comparator = "less_than_or_equal",
    correct_loading_hash = .scmix_v23_step3_sha256(A_raw),
    faulty_loading_hash = .scmix_v23_step3_sha256(A_double_divided),
    probability_rows = nrow(eta_reference),
    integration_points = ncol(eta_reference),
    paired_replay = TRUE, independent_monte_carlo_trial = FALSE,
    application_outcomes_used = FALSE,
    reporting_eligible = FALSE, production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_loading_fault")
}

## Collect the two ordinary base outcomes per design and all of their paired
## numerical replay cells.  Replay rows remain clustered and contribute zero
## independent Wilson trials.
.scmix_v23_numerical_layer_run <- function(
    base_bundles, replays, stream = c("calibration", "validation"),
    plan = scmix_v23_calibration_plan()) {
  stream <- match.arg(stream)
  if (!is.list(base_bundles) || !length(base_bundles) ||
      any(!vapply(base_bundles, .scmix_v23_step3_intact, logical(1L),
                  class_name = "scmix_v23_numerical_evidence_bundle"))) {
    stop("Ordinary numerical validation requires intact base evidence bundles.",
         call. = FALSE)
  }
  expected_designs <- plan$designs$design_id
  expected_reps <- plan$counts[[paste0(
    "numerical_", stream, "_base_outcomes")]]
  expected_cells <- expand.grid(
    design_id = expected_designs,
    base_replication = seq_len(expected_reps),
    stringsAsFactors = FALSE)
  observed_cells <- data.frame(
    design_id = vapply(base_bundles, `[[`, character(1L), "design_id"),
    base_replication = vapply(
      base_bundles, `[[`, integer(1L), "base_replication"),
    stringsAsFactors = FALSE)
  cell_key <- function(x) paste(x$design_id, x$base_replication, sep = "::")
  clusters <- vapply(
    base_bundles, `[[`, character(1L), "replay_cluster_id")
  bundle_ok <- setequal(cell_key(observed_cells), cell_key(expected_cells)) &&
    !anyDuplicated(cell_key(observed_cells)) && !anyDuplicated(clusters) &&
    all(vapply(base_bundles, `[[`, character(1L), "stream") == stream) &&
    all(vapply(base_bundles, `[[`, character(1L), "run_mode") == "base") &&
    all(vapply(base_bundles, `[[`, integer(1L), "multiplier_draws") == 0L) &&
    all(vapply(base_bundles, `[[`, character(1L), "plan_fingerprint") ==
          plan$fingerprint)
  if (!bundle_ok) {
    stop("The ordinary numerical layer does not contain its exact two-by-design base cells.",
         call. = FALSE)
  }
  if (!is.list(replays) || !length(replays) ||
      any(!vapply(replays, .scmix_v23_step3_intact, logical(1L),
                  class_name = "scmix_v23_numerical_replay")) ||
      any(vapply(replays, `[[`, logical(1L),
                 "independent_monte_carlo_trial")) ||
      any(vapply(replays, `[[`, integer(1L),
                 "Wilson_denominator_contribution") != 0L)) {
    stop("Ordinary numerical faults must be signed clustered replays.",
         call. = FALSE)
  }
  replay_clusters <- vapply(
    replays, `[[`, character(1L), "replay_cluster_id")
  replay_base_index <- match(replay_clusters, clusters)
  replay_design <- vapply(replays, `[[`, character(1L), "design_id")
  replay_replication <- vapply(
    replays, `[[`, integer(1L), "base_replication")
  if (any(!replay_clusters %in% clusters) ||
      any(vapply(replays, `[[`, character(1L), "stream") != stream) ||
      any(vapply(replays, `[[`, character(1L), "run_mode") != "base") ||
      any(vapply(replays, `[[`, character(1L), "plan_fingerprint") !=
            plan$fingerprint) ||
      !identical(replay_design,
                 observed_cells$design_id[replay_base_index]) ||
      !identical(replay_replication,
                 observed_cells$base_replication[replay_base_index])) {
    stop("Every ordinary replay must remain in an observed base-outcome cluster.",
         call. = FALSE)
  }
  regimes <- unique(plan$constants$targeted_failure_regime[
    plan$constants$primary_layer == "numerical"])
  severity <- plan$severity[plan$severity$regime_id %in% regimes,
                            c("regime_id", "severity_id"), drop = FALSE]
  expected_replay_cells <- merge(
    data.frame(replay_cluster_id = clusters, stringsAsFactors = FALSE),
    severity, by = NULL, sort = FALSE)
  observed_replay_cells <- data.frame(
    replay_cluster_id = replay_clusters,
    regime_id = vapply(replays, `[[`, character(1L), "regime_id"),
    severity_id = vapply(replays, `[[`, character(1L), "severity_id"),
    stringsAsFactors = FALSE)
  replay_key <- function(x) paste(
    x$replay_cluster_id, x$regime_id, x$severity_id, sep = "::")
  if (!setequal(replay_key(observed_replay_cells),
                replay_key(expected_replay_cells)) ||
      anyDuplicated(replay_key(observed_replay_cells))) {
    stop("Every base cluster must contain each frozen numerical replay cell exactly once.",
         call. = FALSE)
  }
  for (j in seq_along(replays)) {
    expected_constants <- plan$constants$constant[
      plan$constants$primary_layer == "numerical" &
        plan$constants$targeted_failure_regime == replays[[j]]$regime_id]
    if (!setequal(replays[[j]]$comparison$constant, expected_constants) ||
        anyDuplicated(replays[[j]]$comparison$constant)) {
      stop("A replay does not cover every prespecified primary constant in its regime.",
           call. = FALSE)
    }
  }
  metric <- do.call(rbind, lapply(base_bundles, function(x) data.frame(
    design_id = x$design_id, base_replication = x$base_replication,
    replay_cluster_id = x$replay_cluster_id,
    constant = names(.scmix_v23_numerical_bundle_metrics(x)),
    value = unname(.scmix_v23_numerical_bundle_metrics(x)),
    stringsAsFactors = FALSE)))
  comparison <- do.call(rbind, lapply(replays, function(x) {
    out <- x$comparison
    out$design_id <- x$design_id
    out$base_replication <- x$base_replication
    out$replay_cluster_id <- x$replay_cluster_id
    out$regime_id <- x$regime_id
    out$severity_id <- x$severity_id
    out$severity_role <- x$severity_role
    out
  }))
  every_targeted_replay_worse <- isTRUE(all(comparison$fault_is_worse))
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint, stream = stream,
    design_ids = expected_designs,
    independent_base_outcomes = nrow(expected_cells),
    base_bundle_fingerprints = vapply(
      base_bundles, `[[`, character(1L), "fingerprint"),
    replay_fingerprints = vapply(
      replays, `[[`, character(1L), "fingerprint"),
    base_metrics = metric, replay_comparisons = comparison,
    replay_cluster_count = length(clusters),
    replay_Wilson_denominator = 0L,
    every_targeted_replay_worse = every_targeted_replay_worse,
    numerical_completion_pass = every_targeted_replay_worse &&
      all(is.finite(metric$value)) && all(is.finite(
        comparison$fault_to_reference_ratio)),
    complete_numerical_primary_coverage =
      setequal(unique(metric$constant), .scmix_v23_numerical_constants),
    selector_eligible = FALSE,
    selector_block_reason = paste(
      "F008 requires joint cutoff selection and final promotion also requires",
      "the statistical validation and untouched transport layers."),
    application_outcomes_used = FALSE,
    creates_production_manifest = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_numerical_layer_run")
}

## Validate the separate one-per-design untouched transport layer.  Transport
## has no fault replays.  It demonstrates that the frozen constants can be
## evaluated on the exact production-scale path and cannot promote them alone.
.scmix_v23_transport_validation <- function(
    validation_bundles, q2_sentinels,
    plan = scmix_v23_calibration_plan()) {
  if (!is.list(validation_bundles) || !length(validation_bundles) ||
      any(!vapply(validation_bundles, .scmix_v23_step3_intact, logical(1L),
                  class_name = "scmix_v23_numerical_evidence_bundle"))) {
    stop("Transport validation requires intact numerical evidence bundles.",
         call. = FALSE)
  }
  expected_designs <- plan$designs$design_id
  expected_replications <- as.integer(
    plan$counts[["transport_validation_reps"]])
  design <- vapply(validation_bundles, `[[`, character(1L), "design_id")
  replication <- vapply(
    validation_bundles, `[[`, integer(1L), "base_replication")
  expected_cells <- expand.grid(
    design_id = expected_designs,
    base_replication = seq_len(expected_replications),
    stringsAsFactors = FALSE)
  observed_cells <- data.frame(
    design_id = design, base_replication = replication,
    stringsAsFactors = FALSE)
  cell_key <- function(x) paste(x$design_id, x$base_replication, sep = "::")
  if (!setequal(cell_key(observed_cells), cell_key(expected_cells)) ||
      anyDuplicated(cell_key(observed_cells)) ||
      any(vapply(validation_bundles, `[[`, character(1L), "stream") !=
            "transport") ||
      any(vapply(validation_bundles, `[[`, character(1L), "run_mode") !=
            "transport") ||
      any(vapply(validation_bundles, `[[`, character(1L),
                 "plan_fingerprint") != plan$fingerprint) ||
      any(vapply(validation_bundles, `[[`, integer(1L),
                 "multiplier_draws") !=
            plan$counts[["transport_common_respondent_multipliers"]])) {
    stop("Transport does not contain the exact untouched 5x2/three-start/GH31-to-GH61 validation bases in the plan.",
         call. = FALSE)
  }
  base_clusters <- vapply(
    validation_bundles, `[[`, character(1L), "replay_cluster_id")
  if (anyDuplicated(base_clusters)) {
    stop("Each untouched transport base must have a unique cluster identifier.",
         call. = FALSE)
  }
  if (!is.list(q2_sentinels) || length(q2_sentinels) != length(expected_designs)) {
    stop("One q=2 sensitivity sentinel is required per design.",
         call. = FALSE)
  }
  invisible(lapply(q2_sentinels, .scmix_v23_validate_q2_sentinel,
                   plan = plan))
  sentinel_design <- vapply(q2_sentinels, `[[`, character(1L), "design_id")
  if (!setequal(sentinel_design, expected_designs) ||
      anyDuplicated(sentinel_design)) {
    stop("The q=2 sentinel set does not cover every design exactly once.",
         call. = FALSE)
  }
  metric <- do.call(rbind, lapply(validation_bundles, function(x) data.frame(
    design_id = x$design_id,
    constant = names(.scmix_v23_numerical_bundle_metrics(x)),
    value = unname(.scmix_v23_numerical_bundle_metrics(x)),
    stringsAsFactors = FALSE)))
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    design_ids = expected_designs,
    validation_bundle_fingerprints = vapply(
      validation_bundles, `[[`, character(1L), "fingerprint"),
    q2_sentinel_fingerprints = vapply(
      q2_sentinels, `[[`, character(1L), "fingerprint"),
    full_evidence_metrics = metric,
    complete_numerical_primary_coverage =
      setequal(unique(metric$constant), .scmix_v23_numerical_constants),
    transport_completion_pass = all(is.finite(metric$value)) &&
      setequal(unique(metric$constant), .scmix_v23_numerical_constants),
    independent_validation_bases = nrow(expected_cells),
    q2_sensitivity_only = TRUE,
    selector_eligible = FALSE,
    selector_block_reason = paste(
      "Transport cannot promote values without successful statistical and",
      "ordinary numerical validation."),
    creates_production_manifest = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_transport_validation")
}

.scmix_v23_validate_q2_sentinel <- function(x, plan) {
  if (!.scmix_v23_step3_intact(x, "scmix_v23_q2_sentinel") ||
      !identical(x$plan_fingerprint, plan$fingerprint) ||
      !identical(x$q, 2L) ||
      !identical(x$comparisons,
                 c("GH5x5_to_GH9x9", "GH9x9_to_GH15x15")) ||
      !identical(x$gold, "GH25x25") ||
      !identical(x$sensitivity_only, TRUE) ||
      !identical(x$authorizes_q1_manifest, FALSE)) {
    stop("The q=2 integration sentinel is missing, altered, or claims authority.",
         call. = FALSE)
  }
  invisible(x)
}

## Public entry points.  Keeping the implementations private above makes the
## distinction between a signed constructor and its validation helpers easy to
## audit while preserving the full formal arguments on every exported alias.
scmix_v23_numerical_contract <- .scmix_v23_numerical_contract
scmix_v23_numerical_missing_primitives <-
  .scmix_v23_numerical_missing_primitives
scmix_v23_numerical_readiness <- .scmix_v23_numerical_readiness
scmix_v23_seal_generated_outcome <- .scmix_v23_seal_generated_outcome
scmix_v23_fit_numerical_base <- .scmix_v23_fit_numerical_base
scmix_v23_validate_numerical_base_fit <-
  .scmix_v23_validate_numerical_base_fit
scmix_v23_known_rank_evidence <- .scmix_v23_known_rank_evidence
scmix_v23_numerical_evidence_bundle <-
  .scmix_v23_numerical_evidence_bundle
scmix_v23_numerical_replay <- .scmix_v23_numerical_replay
scmix_v23_loading_double_scale_fault <-
  .scmix_v23_loading_double_scale_fault
scmix_v23_numerical_layer_run <- .scmix_v23_numerical_layer_run
scmix_v23_transport_validation <- .scmix_v23_transport_validation
scmix_v23_run_numerical_base <- .scmix_v23_run_numerical_base
scmix_v23_validate_numerical_base_run <-
  .scmix_v23_validate_numerical_base_run
