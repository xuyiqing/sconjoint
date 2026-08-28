## Pure fail-closed contract helpers for the Saha--Weeks v2.1 analysis.

.sw_v21_failed_pilot_valid <- function(manifest, manifest_path,
                                       current_runtime_signature,
                                       expected_configuration_version) {
  is.list(manifest) &&
    identical(
      manifest$schema_version,
      "sw2022-mixed-logit-v2-penalty-pilot-manifest-v1") &&
    identical(manifest$profile, "pilot") &&
    identical(manifest$configuration_version,
              expected_configuration_version) &&
    identical(manifest$pilot_success, FALSE) &&
    identical(manifest$all_generation_inputs_unchanged, TRUE) &&
    identical(manifest$frozen_v1_artifacts_unchanged, TRUE) &&
    identical(manifest$formal_inference_available, FALSE) &&
    identical(manifest$production_result, FALSE) &&
    identical(manifest$outcome_blind, FALSE) &&
    identical(manifest$runtime_signature, current_runtime_signature) &&
    .sc_manifest_artifacts_valid(manifest, manifest_path) &&
    is.character(manifest$input_paths) &&
    all(file.exists(manifest$input_paths)) &&
    .sc_identical_md5_vectors(
      manifest$generation_input_md5,
      .sc_md5_paths(manifest$input_paths))
}

.sw_v21_prepilot_spec_unchanged <- function(predecessor, current) {
  if (!is.list(predecessor) || !is.list(current) ||
      is.null(predecessor$profiles$production)) return(FALSE)
  old <- predecessor$profiles$production
  identical(current$predecessor, predecessor$version) &&
    identical(current$grid, old$grid) &&
    identical(current$folds$outer_K, old$outer_K) &&
    identical(current$folds$inner_K, old$inner_K) &&
    identical(current$optimizer$n_epochs, old$n_epochs) &&
    identical(current$optimizer$n_starts, old$n_starts) &&
    identical(current$optimizer$learning_rate, old$learning_rate) &&
    identical(current$model$n_nodes, old$n_nodes) &&
    identical(current$optimizer$opt_tol, old$opt_tol) &&
    identical(current$optimizer$grad_tol, old$grad_tol) &&
    identical(current$optimizer$nested_objective_tol,
              old$nested_objective_tol) &&
    identical(current$optimizer$selection_tie_tol, 1e-8) &&
    identical(current$model$q, predecessor$model$q) &&
    identical(current$bounds, predecessor$bounds) &&
    identical(current$optimizer$device, predecessor$optimizer$device) &&
    identical(current$optimizer$seed, predecessor$optimizer$seed) &&
    identical(current$optimizer$early_stop,
              predecessor$optimizer$early_stop)
}

.sw_v21_authorization_valid <- function(authorization, config, config_path,
                                        predecessor_config_path,
                                        manifest, manifest_path,
                                        generation_md5,
                                        runtime_signature) {
  is.list(authorization) &&
    identical(authorization$authorized, TRUE) &&
    identical(authorization$purpose,
              "sw2022-v2.1-postpilot-final-analysis") &&
    is.character(authorization$reviewed_by) &&
    length(authorization$reviewed_by) == 1L &&
    !is.na(authorization$reviewed_by) &&
    nzchar(authorization$reviewed_by) &&
    is.character(authorization$authorized_at_utc) &&
    length(authorization$authorized_at_utc) == 1L &&
    !is.na(authorization$authorized_at_utc) &&
    nzchar(authorization$authorized_at_utc) &&
    identical(authorization$acknowledged_postpilot_outcome_informed, TRUE) &&
    identical(authorization$acknowledged_formal_inference_unavailable, TRUE) &&
    identical(authorization$acknowledged_failed_pilot_not_rewritten, TRUE) &&
    identical(authorization$config_version, config$version) &&
    identical(as.character(authorization$config_md5),
              unname(tools::md5sum(config_path))) &&
    identical(as.character(authorization$predecessor_config_md5),
              unname(tools::md5sum(predecessor_config_path))) &&
    identical(as.character(
      authorization$reviewed_failed_pilot_manifest_md5),
      unname(tools::md5sum(manifest_path))) &&
    .sc_identical_md5_vectors(
      authorization$reviewed_failed_pilot_generation_input_md5,
      manifest$generation_input_md5) &&
    .sc_identical_md5_vectors(
      authorization$reviewed_failed_pilot_artifact_md5,
      manifest$artifacts) &&
    .sc_identical_md5_vectors(
      authorization$postpilot_generation_input_md5,
      generation_md5) &&
    identical(authorization$runtime_signature, runtime_signature) &&
    identical(as.numeric(authorization$noninferiority_margin),
              as.numeric(config$postpilot_guardrail$noninferiority_margin)) &&
    identical(authorization$formal_inference_available, FALSE) &&
    identical(authorization$outcome_blind, FALSE)
}

.sw_v21_guardrail_decision <- function(selected_scores, constant_scores,
                                       margin) {
  if (!is.numeric(selected_scores) || !is.numeric(constant_scores) ||
      length(selected_scores) < 2L ||
      length(selected_scores) != length(constant_scores) ||
      any(!is.finite(selected_scores)) || any(!is.finite(constant_scores)) ||
      !is.numeric(margin) || length(margin) != 1L || !is.finite(margin)) {
    stop("Malformed score vectors or post-pilot guardrail margin.",
         call. = FALSE)
  }
  difference <- selected_scores - constant_scores
  mean_difference <- mean(difference)
  pass <- mean_difference >= margin
  list(
    mean_difference = mean_difference,
    respondent_se = stats::sd(difference) / sqrt(length(difference)),
    margin = as.numeric(margin), pass = pass,
    fallback_applied = !pass,
    reported_primary = if (pass) "selected_procedure_q1" else
      "exact_constant_q1",
    descriptive_only = TRUE, formal_test = FALSE)
}
