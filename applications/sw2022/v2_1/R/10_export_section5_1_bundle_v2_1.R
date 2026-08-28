#!/usr/bin/env Rscript

## Add-only final evidence-bundle aggregator for Saha--Weeks Section 5.1.
##
## This is an export and provenance step, not manuscript drafting. It reads
## only completed, hash-valid v2.1 producers; preserves explicit `not_run`
## and `not_verified` states; and refuses to create formal-inference,
## majority, exact-design-HT, or maintained-assumption claims.

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

.sw51_script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.sw51_parse_cli <- function(args) {
  out <- list(output_name = "final")
  for (arg in args) {
    if (!grepl("^--output-name=[A-Za-z0-9][A-Za-z0-9._-]*$", arg)) {
      stop("Only --output-name=<safe-name> is accepted.", call. = FALSE)
    }
    out$output_name <- sub("^--output-name=", "", arg)
  }
  out
}

.sw51_md5 <- function(paths) {
  paths <- as.character(paths)
  out <- rep(NA_character_, length(paths))
  names(out) <- names(paths)
  ok <- !is.na(paths) & file.exists(paths) & !dir.exists(paths)
  if (any(ok)) out[ok] <- unname(tools::md5sum(paths[ok]))
  out
}

.sw51_same_named <- function(x, y) {
  is.character(x) && is.character(y) && identical(names(x), names(y)) &&
    identical(unname(x), unname(y))
}

.sw51_stable_read_rds <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("Required RDS artifact is absent: ", path, call. = FALSE)
  }
  before <- unname(tools::md5sum(path))
  value <- readRDS(path)
  after <- unname(tools::md5sum(path))
  if (!identical(before, after)) {
    stop("Artifact changed while being read: ", path, call. = FALSE)
  }
  list(value = value, path = normalizePath(path, mustWork = TRUE), md5 = before)
}

.sw51_stable_read_csv <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("Required CSV artifact is absent: ", path, call. = FALSE)
  }
  before <- unname(tools::md5sum(path))
  value <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
  after <- unname(tools::md5sum(path))
  if (!identical(before, after)) {
    stop("CSV changed while being read: ", path, call. = FALSE)
  }
  list(value = value, path = normalizePath(path, mustWork = TRUE), md5 = before)
}

.sw51_safe_relative <- function(x) {
  is.character(x) && length(x) > 0L && !anyNA(x) &&
    all(nzchar(x)) && !any(startsWith(x, "/")) &&
    !any(grepl("(^|[/\\\\])[.][.]($|[/\\\\])", x))
}

.sw51_audit_named_paths <- function(component, kind, paths, expected) {
  if (!is.character(paths) || !length(paths) || is.null(names(paths)) ||
      any(!nzchar(names(paths))) || anyDuplicated(names(paths)) ||
      !is.character(expected) || !.sw51_same_named(expected,
                                                    expected[names(paths)])) {
    stop(component, " has malformed named paths/hashes.", call. = FALSE)
  }
  if (!identical(names(paths), names(expected))) {
    stop(component, " path/hash names differ.", call. = FALSE)
  }
  observed <- .sw51_md5(paths)
  exists <- !is.na(observed)
  match <- exists & unname(observed) == unname(expected)
  audit <- data.frame(
    component = component, kind = kind, role = names(paths),
    path = unname(paths), expected_md5 = unname(expected),
    observed_md5 = unname(observed), exists = exists, match = match,
    bytes = ifelse(exists, as.numeric(file.info(paths)$size), NA_real_),
    stringsAsFactors = FALSE)
  if (!all(match)) {
    stop(component, " path/hash validation failed for: ",
         paste(audit$role[!match], collapse = ", "), call. = FALSE)
  }
  audit
}

.sw51_audit_relative_artifacts <- function(component, base_dir, hashes) {
  if (!is.character(hashes) || !length(hashes) || is.null(names(hashes)) ||
      any(!nzchar(names(hashes))) || anyDuplicated(names(hashes)) ||
      !.sw51_safe_relative(names(hashes))) {
    stop(component, " has malformed relative artifact hashes.",
         call. = FALSE)
  }
  paths <- stats::setNames(file.path(base_dir, names(hashes)), names(hashes))
  .sw51_audit_named_paths(component, "manifested_artifact", paths, hashes)
}

.sw51_manifest_hash_field <- function(manifest) {
  fields <- c("artifacts", "artifact_md5")
  found <- fields[vapply(fields, function(x) {
    z <- manifest[[x]]
    is.character(z) && length(z) > 0L && !is.null(names(z))
  }, logical(1L))]
  if (length(found) != 1L) {
    stop("Manifest must expose exactly one named artifact hash field.",
         call. = FALSE)
  }
  manifest[[found[[1L]]]]
}

.sw51_manifest_input_audit <- function(component, manifest) {
  alternatives <- list(
    c("generation_input_paths", "generation_input_md5"),
    c("input_paths", "generation_input_md5"),
    c("source_paths", "source_md5"))
  valid <- Filter(function(pair) {
    p <- manifest[[pair[[1L]]]]; h <- manifest[[pair[[2L]]]]
    is.character(p) && length(p) > 0L && !is.null(names(p)) &&
      is.character(h) && identical(names(p), names(h))
  }, alternatives)
  if (length(valid) != 1L) {
    stop(component, " must expose one unambiguous named input hash lock.",
         call. = FALSE)
  }
  pair <- valid[[1L]]
  .sw51_audit_named_paths(component, "generation_input",
                          manifest[[pair[[1L]]]], manifest[[pair[[2L]]]])
}

.sw51_validate_parent <- function(parent_dir) {
  manifest_path <- file.path(parent_dir, "manifest.rds")
  pointer_path <- file.path(parent_dir, "reported_primary_pointer.rds")
  mr <- .sw51_stable_read_rds(manifest_path)
  pr <- .sw51_stable_read_rds(pointer_path)
  m <- mr$value; p <- pr$value
  required <- identical(m$schema_version,
                        "sw2022-v2.1-postpilot-final-manifest-v1") &&
    identical(p$schema_version,
              "sw2022-v2.1-reported-primary-pointer-v1") &&
    identical(m$final_analysis_success, TRUE) &&
    identical(m$procedural_primary_available, TRUE) &&
    identical(m$reported_primary, p$reported_primary) &&
    identical(isTRUE(m$fallback_applied), isTRUE(p$fallback_applied)) &&
    identical(m$formal_inference_available, FALSE) &&
    identical(p$formal_inference_available, FALSE) &&
    identical(p$formal_test, FALSE) &&
    identical(m$outcome_blind, FALSE) && identical(p$outcome_blind, FALSE) &&
    identical(m$production_result, FALSE)
  if (!required) stop("Reported-primary parent fail-closed schema failed.",
                      call. = FALSE)
  input_audit <- .sw51_manifest_input_audit("parent", m)
  if (!.sw51_same_named(m$generation_input_md5,
                        m$completion_input_md5)) {
    stop("Parent generation/completion hashes differ.", call. = FALSE)
  }
  artifact_audit <- .sw51_audit_relative_artifacts(
    "parent", parent_dir, m$artifacts)
  pointer_row <- artifact_audit$role == basename(pointer_path)
  if (sum(pointer_row) != 1L ||
      !identical(artifact_audit$observed_md5[pointer_row], pr$md5)) {
    stop("Reported-primary pointer is not bound by the parent manifest.",
         call. = FALSE)
  }
  selected <- unlist(p$selected_procedure_paths, use.names = FALSE)
  constant <- unlist(p$exact_constant_paths, use.names = FALSE)
  chosen <- c(p$full_fit_path, p$nested_fit_path, p$assembled_fit_path)
  all_pointer <- c(selected, constant, chosen)
  if (length(selected) != 3L || length(constant) != 3L ||
      length(chosen) != 3L || any(!file.exists(all_pointer)) ||
      !all(basename(all_pointer) %in% names(m$artifacts)) ||
      !all(unname(tools::md5sum(all_pointer)) ==
             unname(m$artifacts[basename(all_pointer)]))) {
    stop("Pointer fit paths are incomplete or not parent-manifested.",
         call. = FALSE)
  }
  expected <- if (isTRUE(p$fallback_applied)) constant else selected
  if (!identical(normalizePath(chosen, mustWork = TRUE),
                 normalizePath(expected, mustWork = TRUE)) ||
      !.sw51_same_named(p$generation_input_md5, m$generation_input_md5)) {
    stop("Pointer does not resolve the manifest-declared primary.",
         call. = FALSE)
  }
  list(manifest = m, pointer = p, manifest_read = mr, pointer_read = pr,
       input_audit = input_audit, artifact_audit = artifact_audit)
}

.sw51_validate_postfit <- function(postfit_dir, parent) {
  path <- file.path(postfit_dir, "manifests", "evidence_manifest.rds")
  er <- .sw51_stable_read_rds(path); m <- er$value
  ok <- identical(m$schema_version,
                  "sw2022-v2.1-postfit-evidence-manifest-v1") &&
    identical(m$evidence_schema, "sw2022-v2.1-postfit-evidence-v1") &&
    identical(m$reported_primary, parent$pointer$reported_primary) &&
    identical(isTRUE(m$fallback_applied),
              isTRUE(parent$pointer$fallback_applied)) &&
    identical(m$producer_manifest_md5, parent$manifest_read$md5) &&
    identical(m$reported_primary_pointer_md5, parent$pointer_read$md5) &&
    identical(m$final_gates_pass, TRUE) &&
    identical(m$formal_inference_available, FALSE) &&
    identical(m$outcome_blind, FALSE) && identical(m$production_result, FALSE)
  if (!ok) stop("Post-fit evidence manifest failed its final schema/gates.",
                call. = FALSE)
  artifact_audit <- .sw51_audit_relative_artifacts(
    "postfit", postfit_dir, m$artifacts)
  provenance_path <- file.path(postfit_dir, "tables",
                               "provenance__input_manifest.csv")
  provenance <- .sw51_stable_read_csv(provenance_path)
  d <- provenance$value
  if (!all(c("role", "path", "bytes", "md5") %in% names(d)) ||
      anyDuplicated(d$role) || !identical(as.character(d$role),
                                         names(m$input_md5))) {
    stop("Post-fit input provenance table is malformed.", call. = FALSE)
  }
  paths <- stats::setNames(as.character(d$path), d$role)
  expected <- stats::setNames(as.character(d$md5), d$role)
  if (!.sw51_same_named(expected, m$input_md5)) {
    stop("Post-fit input provenance does not match its manifest.",
         call. = FALSE)
  }
  input_audit <- .sw51_audit_named_paths(
    "postfit", "generation_input", paths, expected)
  list(manifest = m, manifest_read = er, input_audit = input_audit,
       artifact_audit = artifact_audit)
}

.sw51_validate_rank <- function(rank_dir, parent) {
  manifest_path <- file.path(rank_dir, "manifest.rds")
  final_path <- file.path(rank_dir, "rank_numerical_final_result.rds")
  mr <- .sw51_stable_read_rds(manifest_path)
  fr <- .sw51_stable_read_rds(final_path)
  m <- mr$value; f <- fr$value
  ok <- identical(m$schema_version,
                  "sw2022-v2.1-rank-numerical-manifest-v1") &&
    identical(f$schema_version,
              "sw2022-v2.1-rank-numerical-final-v1") &&
    identical(m$completed, TRUE) &&
    identical(m$computational_and_provenance_gate_pass, TRUE) &&
    identical(f$computational_and_provenance_gate_pass, TRUE) &&
    identical(m$parent_reported_primary, parent$pointer$reported_primary) &&
    identical(f$parent_reported_primary, parent$pointer$reported_primary) &&
    identical(m$parent_manifest_md5, parent$manifest_read$md5) &&
    identical(m$parent_pointer_md5, parent$pointer_read$md5) &&
    identical(m$rank_selected, FALSE) && identical(f$rank_selected, FALSE) &&
    identical(f$primary_rank_changed, FALSE) &&
    identical(m$formal_inference_available, FALSE) &&
    identical(f$formal_inference_available, FALSE) &&
    identical(m$outcome_blind, FALSE) && identical(f$outcome_blind, FALSE) &&
    identical(f$empirical_gate_is_asymptotic_certificate, FALSE) &&
    identical(as.integer(m$ranks), 0:2)
  if (!ok) stop("Completed rank/numerical manifest failed closed.",
                call. = FALSE)
  input_audit <- .sw51_manifest_input_audit("rank_numerical", m)
  if (!.sw51_same_named(m$generation_input_md5,
                        m$completion_input_md5) ||
      !.sw51_same_named(f$generation_input_md5,
                        f$completion_input_md5)) {
    stop("Rank generation/completion hashes differ.", call. = FALSE)
  }
  artifact_audit <- .sw51_audit_relative_artifacts(
    "rank_numerical", rank_dir, m$artifacts)
  final_row <- artifact_audit$role == basename(final_path)
  if (sum(final_row) != 1L ||
      !identical(artifact_audit$observed_md5[final_row], fr$md5)) {
    stop("Rank final result is not bound by the rank manifest.",
         call. = FALSE)
  }
  list(manifest = m, final = f, manifest_read = mr, final_read = fr,
       input_audit = input_audit, artifact_audit = artifact_audit)
}

.sw51_validate_sensitivity_manifest <- function(path, component, profile,
                                                pointer_lock) {
  mr <- .sw51_stable_read_rds(path); m <- mr$value
  expected_schema <- switch(component,
    application = "sw2022-v2.1-application-sensitivity-manifest-v1",
    misspecification = "sw2022-v2.1-design-misspecification-manifest-v1")
  common <- identical(m$schema_version, expected_schema) &&
    identical(m$profile, profile) &&
    identical(m$primary_artifacts_modified, FALSE) &&
    identical(m$maintained_assumptions_verified, FALSE) &&
    identical(m$outcome_blind, FALSE) &&
    identical(m$formal_inference_available, FALSE) &&
    identical(m$empirical_alternative_likelihoods, "not_run") &&
    identical(m$profile_likelihoods, "not_run") &&
    .sw51_same_named(m$pointer_lock_md5, pointer_lock)
  component_ok <- if (component == "application") {
    identical(m$complete_battery, TRUE) &&
      setequal(m$completed_components,
               c("z19", "interaction", "process", "completion"))
  } else {
    identical(m$complete_scenario_battery, TRUE) &&
      identical(m$all_optimizer_gates_pass, TRUE) &&
      identical(m$simulation_validation_pass, TRUE) &&
      identical(m$coverage_evaluated, FALSE) &&
      identical(m$materiality_pass_issued, FALSE)
  }
  if (!common || !component_ok) {
    stop("Production sensitivity ", component,
         " manifest failed closed.", call. = FALSE)
  }
  input_audit <- .sw51_manifest_input_audit(
    paste0("sensitivity_", component), m)
  artifact_audit <- .sw51_audit_relative_artifacts(
    paste0("sensitivity_", component), dirname(path), m$artifact_md5)
  list(manifest = m, manifest_read = mr, input_audit = input_audit,
       artifact_audit = artifact_audit)
}

.sw51_application_component_common <- function(x, schema, component,
                                               pointer_lock, app_manifest) {
  s <- x$sw_v21_sensitivity_specification
  is.list(x) && identical(x$schema_version, schema) &&
    identical(x$outcome_blind, FALSE) &&
    identical(x$formal_inference_available, FALSE) &&
    identical(x$maintained_model, FALSE) &&
    .sw51_same_named(x$pointer_lock_md5, pointer_lock) && is.list(s) &&
    identical(s$profile, "production") &&
    identical(s$component, component) &&
    identical(s$reported_primary, app_manifest$reported_primary) &&
    .sw51_same_named(s$pointer_lock_md5, pointer_lock) &&
    .sw51_same_named(s$source_md5, app_manifest$source_md5) &&
    identical(s$outcome_blind, FALSE) &&
    identical(s$formal_inference_available, FALSE)
}

.sw51_application_table_disposition <- function(gates) {
  roles <- c(
    "tables/adjacent_transition_calibration.csv",
    "tables/application_status.csv",
    "tables/completion_amce_comparison.csv",
    "tables/completion_choice_comparison.csv",
    "tables/completion_early_assignment_response_balance.csv",
    "tables/completion_early_task_by_eventual_completion.csv",
    "tables/completion_sample.csv",
    "tables/completion_theta_comparison.csv",
    "tables/empirical_alternative_likelihood_status.csv",
    "tables/male_run_choice_probabilities.csv",
    "tables/male_run_conditional_effects.csv",
    "tables/male_run_design_audit.csv",
    "tables/male_run_heldout_score_difference.csv",
    "tables/position_diagnostics.csv",
    "tables/position_profile_swap.csv",
    "tables/profile_likelihood_status.csv",
    "tables/serial_residual_diagnostics.csv",
    "tables/task_order_calibration.csv",
    "tables/z19_choices.csv", "tables/z19_heldout_score_difference.csv",
    "tables/z19_theta.csv")
  component <- c(
    "task_position_serial", "application_status",
    "completion_design_descriptive", "completion_structural_refit",
    "completion_design_descriptive", "completion_design_descriptive",
    "completion_design_descriptive", "completion_structural_refit",
    "fail_closed_status", rep("male_x_prior_run", 4L),
    "task_position_serial", "position_profile_swap",
    "fail_closed_status", "task_position_serial", "task_position_serial",
    rep("postconjoint_19Z", 3L))
  estimand_type <- c(
    "heldout_process_diagnostic", "status_ledger",
    "respondent_clustered_marginal_amce", "structural_refit_comparison",
    "raw_assignment_response_descriptive", "raw_completion_descriptive",
    "sample_count", "structural_refit_comparison", "status_ledger",
    "structural_perturbation", "structural_perturbation",
    "design_rank_diagnostic", "heldout_score_diagnostic",
    "heldout_position_diagnostic", "optimization_equivariance_diagnostic",
    "status_ledger", "heldout_serial_diagnostic",
    "heldout_task_diagnostic", "structural_perturbation",
    "heldout_score_diagnostic", "structural_perturbation")
  gate <- c(
    gates$process, TRUE, gates$completion_design,
    gates$completion_structural, gates$completion_design,
    gates$completion_design, gates$completion_design,
    gates$completion_structural, TRUE,
    rep(gates$interaction, 4L), gates$process, gates$position_swap,
    TRUE, gates$process, gates$process, rep(gates$z19, 3L))
  status <- ifelse(gate, "reportable_descriptive", "unavailable_gate_fail")
  status[component == "completion_structural_refit" & !gate] <-
    "unavailable_nested_objective_gate_fail"
  data.frame(
    role = roles, component = component, estimand_type = estimand_type,
    reportable_descriptive = gate, status = status,
    formal_inference_available = FALSE,
    note = ifelse(component == "completion_structural_refit" & !gate,
      paste("For both no-Z fixed refits, pooled/continued gates pass but",
            "nested-objective, main, and overall gates fail; theta and",
            "structural choice comparisons are not exported."),
      ifelse(component == "completion_design_descriptive",
        paste("Computed directly from sample membership, assignments, choices,",
              "or respondent-clustered LPMs; does not rely on the failed refits."),
        "Component-specific hash and computational gate governs export.")),
    stringsAsFactors = FALSE)
}

.sw51_sensitivity_claim_state <- function(sensitivity) {
  components <- sensitivity$application_components$component_status
  required <- c("postconjoint_19Z", "male_x_prior_run",
                "task_position_serial", "position_profile_swap",
                "completion_structural_refit",
                "completion_design_descriptive")
  if (!is.data.frame(components) ||
      !identical(components$component, required)) {
    stop("Sensitivity component status is malformed.", call. = FALSE)
  }
  at <- function(component, field) {
    value <- components[components$component == component, field, drop = TRUE]
    if (length(value) != 1L || is.na(value)) {
      stop("Sensitivity component state is ambiguous: ", component,
           "/", field, call. = FALSE)
    }
    isTRUE(value)
  }
  gate_components <- required[1:4]
  list(
    gated_application_components = all(vapply(
      gate_components, at, logical(1L), field = "reportable_descriptive")),
    completion_structural = at(
      "completion_structural_refit", "reportable_descriptive"),
    completion_design = at(
      "completion_design_descriptive", "reportable_descriptive"))
}

.sw51_validate_application_components <- function(app, pointer_lock) {
  base <- dirname(app$manifest_read$path)
  roles <- c(
    z19 = "fit_z19_sensitivity.rds",
    interaction = "fit_male_run_interaction.rds",
    process = "task_process_diagnostics.rds",
    completion = "completion_sample_sensitivity.rds")
  reads <- lapply(roles, function(role) {
    row <- app$artifact_audit$role == role
    if (sum(row) != 1L) stop("Application component is not manifested: ", role,
                            call. = FALSE)
    out <- .sw51_stable_read_rds(file.path(base, role))
    if (!identical(out$md5, app$artifact_audit$expected_md5[row])) {
      stop("Application component hash differs from manifest: ", role,
           call. = FALSE)
    }
    out
  })
  objects <- lapply(reads, `[[`, "value")
  z19 <- objects$z19; interaction <- objects$interaction
  process <- objects$process; completion <- objects$completion
  common <- c(
    z19 = .sw51_application_component_common(
      z19, "sw2022-v2.1-z19-sensitivity-v1", "z19", pointer_lock,
      app$manifest),
    interaction = .sw51_application_component_common(
      interaction, "sw2022-v2.1-male-run-sensitivity-v1", "interaction",
      pointer_lock, app$manifest),
    process = .sw51_application_component_common(
      process, "sw2022-v2.1-task-process-sensitivity-v1", "process",
      pointer_lock, app$manifest),
    completion = .sw51_application_component_common(
      completion, "sw2022-v2.1-completion-sensitivity-v1", "completion",
      pointer_lock, app$manifest))
  if (!all(common)) {
    stop("An application sensitivity component failed its stamp/claim lock: ",
         paste(names(common)[!common], collapse = ", "), call. = FALSE)
  }
  z19_gate <- is.list(z19$fit) &&
    all(z19$fit$optimization_gate_by_fold) &&
    identical(z19$fit$full_optimization_gate, TRUE)
  interaction_gate <- is.list(interaction$fit) &&
    all(interaction$fit$optimization_gate_by_fold) &&
    identical(interaction$fit$full_optimization_gate, TRUE) &&
    identical(interaction$identification_established, FALSE)
  process_gate <- is.list(process$heldout_predictions) &&
    identical(process$heldout_predictions$out_of_fold, TRUE) &&
    identical(process$heldout_predictions$complete_sequence, TRUE) &&
    identical(process$heldout_predictions$shared_factor_within_sequence,
              TRUE) &&
    all(vapply(process$tables, is.data.frame, logical(1L))) &&
    identical(process$task_process_alternative_likelihood, "not_run") &&
    identical(process$serial_shock_alternative_likelihood, "not_run")
  position_swap_gate <- is.list(process$profile_swap_fit$gate) &&
    identical(process$profile_swap_fit$gate$pass, TRUE)
  completion_fits <- list(primary = completion$primary_noZ_fit,
                          expanded = completion$expanded_noZ_fit)
  completion_gate_shape <- vapply(completion_fits, function(fit) {
    g <- fit$gate
    is.list(g) && identical(fit$optimization_gate_pass, FALSE) &&
      identical(g$pass, FALSE) && identical(g$main, FALSE) &&
      identical(g$pooled_prefit, TRUE) &&
      identical(g$continued_constant, TRUE) &&
      identical(g$nested_objective, FALSE)
  }, logical(1L))
  completion_optimizer_gate_state <- identical(
    completion$optimizer_gate, c(primary = FALSE, expanded = FALSE))
  completion_structural_gate <- isTRUE(all(completion$optimizer_gate))
  completion_design_gate <-
    is.character(completion$source_path) &&
    length(completion$source_path) == 1L &&
    file.exists(completion$source_path) &&
    identical(unname(tools::md5sum(completion$source_path)),
              completion$source_md5) &&
    identical(completion$source_path, app$manifest$completion_raw_path) &&
    identical(completion$source_md5, app$manifest$completion_raw_md5) &&
    identical(completion$source_policy, "read-only") &&
    identical(completion$completion_independence_verified, FALSE) &&
    all(vapply(completion[c(
      "sample", "amce_comparison", "early_task_by_eventual_completion",
      "early_assignment_response_balance")], is.data.frame, logical(1L)))
  completion_table_roles <- c(
    sample = "tables/completion_sample.csv",
    amce_comparison = "tables/completion_amce_comparison.csv",
    early_task_by_eventual_completion =
      "tables/completion_early_task_by_eventual_completion.csv",
    early_assignment_response_balance =
      "tables/completion_early_assignment_response_balance.csv",
    theta_comparison = "tables/completion_theta_comparison.csv",
    choice_comparison = "tables/completion_choice_comparison.csv")
  completion_table_reads <- lapply(completion_table_roles, function(role) {
    row <- app$artifact_audit$role == role
    if (sum(row) != 1L) {
      stop("Completion table is not uniquely manifested: ", role,
           call. = FALSE)
    }
    out <- .sw51_stable_read_csv(file.path(base, role))
    if (!identical(out$md5, app$artifact_audit$expected_md5[row])) {
      stop("Completion table hash differs from manifest: ", role,
           call. = FALSE)
    }
    out
  })
  completion_table_identity <- vapply(names(completion_table_reads),
    function(field) isTRUE(all.equal(
      completion[[field]], completion_table_reads[[field]]$value,
      check.attributes = FALSE)), logical(1L))
  if (!z19_gate || !interaction_gate || !process_gate ||
      !position_swap_gate || completion_structural_gate ||
      !completion_optimizer_gate_state || !all(completion_gate_shape) ||
      !completion_design_gate || !all(completion_table_identity)) {
    stop(paste(
      "Application component gates differ from the validated fail-closed",
      "state (z19/interaction/process/swap pass; completion pooled/continued",
      "gates pass but nested-objective, main, and overall refit gates fail;",
      "raw completion descriptives remain valid and identity-checked)."),
      call. = FALSE)
  }
  gates <- list(
    z19 = z19_gate, interaction = interaction_gate,
    process = process_gate, position_swap = position_swap_gate,
    completion_structural = completion_structural_gate,
    completion_design = completion_design_gate)
  component_status <- data.frame(
    component = c("postconjoint_19Z", "male_x_prior_run",
      "task_position_serial", "position_profile_swap",
      "completion_structural_refit", "completion_design_descriptive"),
    completed = TRUE,
    computational_gate_pass = c(
      z19_gate, interaction_gate, process_gate, position_swap_gate,
      completion_structural_gate, completion_design_gate),
    reportable_descriptive = c(
      z19_gate, interaction_gate, process_gate, position_swap_gate,
      completion_structural_gate, completion_design_gate),
    status = c(
      rep("completed_component_gate_pass", 4L),
      "unavailable_nested_objective_gate_fail",
      "completed_raw_design_sample_descriptive"),
    formal_inference_available = FALSE,
    maintained_assumption_verified = FALSE,
    note = c(
      "Post-conjoint timing-sensitive perturbation; no formal inference.",
      "Interaction perturbation; identification remains unverified.",
      "Held-out task/order/serial diagnostics; no alternative process likelihood.",
      "A/B swap optimization/equivariance diagnostic; no formal test.",
      "For both fixed no-Z ReLU refits, pooled/continued gates pass but nested-objective, main, and overall gates fail; structural theta/choice comparisons are unavailable.",
      "Sample counts, early response/assignment summaries, and direct respondent-clustered LPM AMCE comparisons do not use the failed structural refits."),
    stringsAsFactors = FALSE)
  disposition <- .sw51_application_table_disposition(gates)
  csv_roles <- app$artifact_audit$role[
    grepl("[.]csv$", app$artifact_audit$role, ignore.case = TRUE)]
  if (anyDuplicated(disposition$role) || anyDuplicated(csv_roles) ||
      !setequal(disposition$role, csv_roles)) {
    stop("Application sensitivity CSVs are not exhaustively classified.",
         call. = FALSE)
  }
  list(reads = reads, objects = objects, gates = gates,
       component_status = component_status, table_disposition = disposition)
}

.sw51_validate_sensitivity <- function(sensitivity_dir, parent,
                                       profile = "production") {
  profile_dir <- file.path(sensitivity_dir, profile)
  validation_path <- file.path(profile_dir, "validation.rds")
  vr <- .sw51_stable_read_rds(validation_path); v <- vr$value
  ok <- identical(v$schema_version,
                  "sw2022-v2.1-sensitivity-validation-v1") &&
    identical(v$profile, profile) &&
    identical(v$requested_component, "all") &&
    is.data.frame(v$table) && nrow(v$table) == 2L &&
    setequal(v$table$component, c("application", "misspecification")) &&
    identical(v$pass, all(v$table$pass)) && identical(v$pass, FALSE) &&
    identical(v$reported_primary, parent$pointer$reported_primary) &&
    identical(v$outcome_blind, FALSE) &&
    identical(v$formal_inference_available, FALSE) &&
    identical(v$empirical_alternative_likelihoods, "not_run") &&
    identical(v$profile_likelihoods, "not_run")
  if (!ok) stop("Production sensitivity validator schema failed.",
                call. = FALSE)
  app <- .sw51_validate_sensitivity_manifest(
    file.path(profile_dir, "application", "manifest.rds"),
    "application", profile, v$pointer_lock_md5)
  misspec <- .sw51_validate_sensitivity_manifest(
    file.path(profile_dir, "misspecification", "manifest.rds"),
    "misspecification", profile, v$pointer_lock_md5)
  if (!identical(app$manifest$reported_primary,
                 parent$pointer$reported_primary) ||
      !identical(misspec$manifest$reported_primary,
                 parent$pointer$reported_primary)) {
    stop("Sensitivity manifests reference another reported primary.",
         call. = FALSE)
  }
  application_row <- v$table[v$table$component == "application", , drop = FALSE]
  misspec_row <- v$table[v$table$component == "misspecification", , drop = FALSE]
  validator_state <- nrow(application_row) == 1L &&
    identical(application_row$pass, FALSE) &&
    identical(application_row$reasons, "completion_optimizer_gates") &&
    nrow(misspec_row) == 1L && identical(misspec_row$pass, TRUE) &&
    (identical(misspec_row$reasons, "") || is.na(misspec_row$reasons))
  if (!validator_state) {
    stop("Sensitivity validator does not show the frozen componentwise state.",
         call. = FALSE)
  }
  components <- .sw51_validate_application_components(
    app, v$pointer_lock_md5)
  validation_csv <- file.path(profile_dir, "validation.csv")
  vcsv <- .sw51_stable_read_csv(validation_csv)
  if (!identical(vcsv$value, v$table)) {
    stop("Sensitivity validator RDS/CSV disagreement.", call. = FALSE)
  }
  list(validation = v, validation_read = vr, validation_csv = vcsv,
       application = app, misspecification = misspec,
       application_components = components,
       profile_dir = profile_dir)
}

.sw51_profile_directions <- function() {
  c("kappa", "female_vs_male_mean", "active_covariance_eigenvalue",
    "headline_contest_probability")
}

.sw51_profile_manifest_paths <- function(profile_root) {
  directions <- .sw51_profile_directions()
  stats::setNames(file.path(profile_root, directions, "manifest.rds"),
                  directions)
}

.sw51_validate_profile_authorization <- function(path, parent, sensitivity) {
  ar <- .sw51_stable_read_rds(path); a <- ar$value
  ok <- identical(a$schema_version,
                  "sw2022-v2.1-profile-sequence-authorization-v2") &&
    identical(a$authorized, TRUE) &&
    identical(a$purpose,
              "sw2022-v2.1-descriptive-penalized-criterion-profile-sequences") &&
    is.character(a$reviewed_by) && length(a$reviewed_by) == 1L &&
    !is.na(a$reviewed_by) && nzchar(a$reviewed_by) &&
    identical(a$reported_primary, parent$pointer$reported_primary) &&
    identical(a$reviewed_pointer_md5, parent$pointer_read$md5) &&
    identical(a$reviewed_manifest_md5, parent$manifest_read$md5) &&
    .sw51_same_named(a$reported_primary_lock_md5,
                     sensitivity$validation$pointer_lock_md5) &&
    identical(a$acknowledged_outcome_informed, TRUE) &&
    identical(a$acknowledged_descriptive_penalized_criterion_sequences,
              TRUE) &&
    identical(a$acknowledged_formal_inference_unavailable, TRUE) &&
    identical(a$acknowledged_no_lr_critical_values, TRUE) &&
    identical(a$acknowledged_fixed_learner_tuning_sieve, TRUE) &&
    identical(a$formal_inference_available, FALSE) &&
    identical(a$outcome_blind, FALSE)
  if (!ok) stop("Profile authorization failed its fail-closed contract.",
                call. = FALSE)
  input_audit <- .sw51_manifest_input_audit("profile_authorization", a)
  list(authorization = a, authorization_read = ar,
       input_audit = input_audit)
}

.sw51_profile_absence_audit <- function(profile_root, missing_directions) {
  if (!length(missing_directions)) {
    return(data.frame(
      direction = character(), direction_path = character(),
      direction_exists = logical(), manifest_path = character(),
      manifest_exists = logical(), unmanifested_file_count = integer(),
      state = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(missing_directions, function(direction) {
    direction_path <- file.path(profile_root, direction)
    manifest_path <- file.path(direction_path, "manifest.rds")
    files <- if (dir.exists(direction_path)) list.files(
      direction_path, recursive = TRUE, all.files = TRUE, no.. = TRUE,
      full.names = TRUE, include.dirs = FALSE) else character()
    data.frame(
      direction = direction, direction_path = direction_path,
      direction_exists = dir.exists(direction_path),
      manifest_path = manifest_path, manifest_exists = file.exists(manifest_path),
      unmanifested_file_count = length(files),
      state = "unavailable_no_manifest_or_artifact",
      stringsAsFactors = FALSE)
  }))
}

.sw51_revalidate_profile_absences <- function(profile) {
  if (!isTRUE(profile$root_present)) return(invisible(TRUE))
  unexpected_top <- setdiff(list.files(
    profile$base_dir, all.files = TRUE, no.. = TRUE), profile$allowed_top)
  if (length(unexpected_top)) {
    stop("The profile root acquired unmanifested entries during export: ",
         paste(unexpected_top, collapse = ", "), call. = FALSE)
  }
  if (!nrow(profile$absence_audit)) return(invisible(TRUE))
  current <- .sw51_profile_absence_audit(
    profile$base_dir, profile$absence_audit$direction)
  ok <- !current$manifest_exists & current$unmanifested_file_count == 0L
  if (!all(ok)) {
    stop("A previously unavailable profile direction acquired unmanifested output during export: ",
         paste(current$direction[!ok], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.sw51_validate_optional_profile <- function(profile_root, parent,
                                            sensitivity) {
  manifest_paths <- .sw51_profile_manifest_paths(profile_root)
  if (!dir.exists(profile_root)) {
    app_table_path <- file.path(
      sensitivity$profile_dir, "application", "tables",
      "profile_likelihood_status.csv")
    status_csv <- .sw51_stable_read_csv(app_table_path)
    z <- status_csv$value
    if (!all(c("status", "verified_profile") %in% names(z)) || !nrow(z) ||
        !all(z$status == "not_run") || any(z$verified_profile) ||
        !identical(sensitivity$application$manifest$profile_likelihoods,
                   "not_run") ||
        !identical(sensitivity$misspecification$manifest$profile_likelihoods,
                   "not_run")) {
      stop("An absent profile is not supported by explicit not_run evidence.",
           call. = FALSE)
    }
    return(list(
      root_present = FALSE, complete = FALSE,
      available = FALSE, reportable = FALSE, status = "not_run",
      reason = paste(
        "No profile-sequence output root is present; both validated",
        "production sensitivity manifests and their legacy status table",
        "explicitly record literal profile likelihoods as not_run."),
      direction_status = data.frame(
        direction = names(manifest_paths), manifest_available = FALSE,
        artifact_state = "not_run", reportable_descriptive = FALSE,
        literal_likelihood_profile = FALSE,
        formal_inference_available = FALSE,
        note = "No authorized profile-sequence output root is present.",
        stringsAsFactors = FALSE),
      absence_audit = data.frame(), status_csv = status_csv))
  }

  authorization_path <- file.path(
    profile_root, "PROFILE_SEQUENCE_AUTHORIZATION.rds")
  authorization <- .sw51_validate_profile_authorization(
    authorization_path, parent, sensitivity)
  allowed_top <- c("PROFILE_SEQUENCE_AUTHORIZATION.rds",
                   names(manifest_paths))
  top <- list.files(profile_root, all.files = TRUE, no.. = TRUE)
  unexpected_top <- setdiff(top, allowed_top)
  if (length(unexpected_top)) {
    stop("Unexpected unmanifested profile-root entries: ",
         paste(unexpected_top, collapse = ", "), call. = FALSE)
  }
  present <- stats::setNames(file.exists(manifest_paths),
                             names(manifest_paths))
  reads <- lapply(manifest_paths[present], .sw51_stable_read_rds)
  manifests <- lapply(reads, `[[`, "value")
  input_audits <- list(); artifact_audits <- list()
  for (direction in names(manifests)) {
    m <- manifests[[direction]]
    completed <- is.character(m$completed_at_utc) &&
      length(m$completed_at_utc) == 1L && !is.na(m$completed_at_utc) &&
      nzchar(m$completed_at_utc)
    fail_closed <-
      identical(m$schema_version,
                "sw2022-v2.1-profile-direction-manifest-v2") &&
      identical(m$direction, direction) && completed &&
      is.logical(m$verified_penalized_criterion_profile_sequence) &&
      length(m$verified_penalized_criterion_profile_sequence) == 1L &&
      !is.na(m$verified_penalized_criterion_profile_sequence) &&
      identical(m$literal_likelihood_profile, FALSE) &&
      identical(m$unpenalized_likelihood_overlay, TRUE) &&
      identical(m$learner_tuning_sieve_fixed, TRUE) &&
      identical(m$retuning_performed, FALSE) &&
      identical(m$penalized_nuisance_reoptimization, TRUE) &&
      identical(m$unpenalized_complete_sequence_likelihood_reported, TRUE) &&
      identical(m$formal_inference_available, FALSE) &&
      identical(m$outcome_blind, FALSE) &&
      identical(m$descriptive_only, TRUE) &&
      identical(m$formal_test, FALSE) &&
      identical(m$likelihood_ratio_critical_values, FALSE) &&
      identical(m$authorization_md5,
                authorization$authorization_read$md5) &&
      identical(m$config_version,
                authorization$authorization$config_version) &&
      identical(m$runtime_signature,
                authorization$authorization$runtime_signature) &&
      .sw51_same_named(m$reported_primary_lock_md5,
                       sensitivity$validation$pointer_lock_md5)
    if (!fail_closed) {
      stop("Profile direction manifest failed closed: ", direction,
           call. = FALSE)
    }
    input_audits[[direction]] <- .sw51_manifest_input_audit(
      paste0("profile_sequences_", direction), m)
    if (!identical(m$input_paths,
                   authorization$authorization$generation_input_paths) ||
        !.sw51_same_named(m$generation_input_md5,
                         authorization$authorization$generation_input_md5)) {
      stop("Profile direction does not share the authorized generation lock: ",
           direction, call. = FALSE)
    }
    hashes <- .sw51_manifest_hash_field(m)
    artifact_audits[[direction]] <- .sw51_audit_relative_artifacts(
      paste0("profile_sequences_", direction),
      dirname(manifest_paths[[direction]]), hashes)
    actual <- list.files(dirname(manifest_paths[[direction]]),
      recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = FALSE,
      include.dirs = FALSE)
    expected <- sort(c("manifest.rds", names(hashes)))
    if (!identical(sort(actual), expected)) {
      stop("Profile direction has unmanifested or missing files: ", direction,
           call. = FALSE)
    }
  }
  missing <- names(manifest_paths)[!present]
  absence_audit <- .sw51_profile_absence_audit(profile_root, missing)
  if (nrow(absence_audit) && (any(absence_audit$manifest_exists) ||
      any(absence_audit$unmanifested_file_count != 0L))) {
    stop("A profile direction lacks a manifest but has unmanifested output: ",
         paste(absence_audit$direction[
           absence_audit$manifest_exists |
             absence_audit$unmanifested_file_count != 0L],
           collapse = ", "), call. = FALSE)
  }
  direction_reportable <- vapply(manifests, function(m) {
    identical(m$verified_penalized_criterion_profile_sequence, TRUE) &&
      identical(m$all_nuisance_reoptimization_gates_pass, TRUE)
  }, logical(1L))
  direction_status <- do.call(rbind, lapply(names(manifest_paths),
    function(direction) {
      is_present <- present[[direction]]
      is_reportable <- is_present && direction_reportable[[direction]]
      data.frame(
        direction = direction, manifest_available = is_present,
        artifact_state = if (!is_present)
          "unavailable_no_manifest_or_artifact" else if (is_reportable)
          "completed_hash_valid_descriptive_diagnostic" else
          "completed_hash_valid_profile_gate_fail",
        reportable_descriptive = is_reportable,
        literal_likelihood_profile = if (is_present)
          manifests[[direction]]$literal_likelihood_profile else FALSE,
        formal_inference_available = FALSE,
        note = if (!is_present)
          "No manifest or artifact was emitted; this direction is unavailable."
        else if (is_reportable) paste(
          "Hash-valid penalized-criterion profile sequence with an",
          "unpenalized likelihood overlay; descriptive only.")
        else paste(
          "Hash-valid completed output, but a profile/reoptimization gate",
          "did not pass; no curvature claim is available."),
        stringsAsFactors = FALSE)
    }))
  complete <- all(present)
  reportable <- any(direction_status$reportable_descriptive)
  status <- if (!complete && any(present)) "incomplete_failed_closed" else if (
    !complete) "authorized_no_completed_directions" else if (
      all(direction_status$reportable_descriptive))
    "completed_descriptive_penalized_criterion_sequences" else
    "completed_profile_gate_fail"
  reason <- if (identical(status, "incomplete_failed_closed")) paste0(
    "The authorized fail-closed profile module produced completed, hash-valid ",
    "descriptive diagnostics for ",
    paste(names(manifest_paths)[present], collapse = ", "),
    "; ", paste(missing, collapse = ", "),
    " emitted no manifest or artifact and is unavailable. Completed ",
    "directions are penalized-criterion profile sequences with unpenalized ",
    "likelihood overlays, not literal likelihood profiles or LR inference.")
  else if (complete && all(direction_status$reportable_descriptive)) paste(
    "All four hash-valid descriptive penalized-criterion profile sequences",
    "passed their gates; the unpenalized likelihoods are overlays, not",
    "literal likelihood profiles or LR inference.")
  else paste(
    "The authorized profile module has no complete reportable four-direction",
    "battery; retain the direction statuses and draw no unsupported",
    "curvature conclusion.")
  combined_inputs <- c(list(authorization$input_audit), input_audits)
  combined_artifacts <- if (length(artifact_audits))
    do.call(rbind, artifact_audits) else authorization$input_audit[FALSE, ]
  list(
    root_present = TRUE, complete = complete,
    available = any(present), reportable = reportable,
    status = status, reason = reason, direction_status = direction_status,
    absence_audit = absence_audit,
    authorization = authorization$authorization,
    authorization_read = authorization$authorization_read,
    manifests = manifests, manifest_reads = reads,
    input_audit = do.call(rbind, combined_inputs),
    artifact_audit = combined_artifacts,
    allowed_top = allowed_top,
    base_dir = normalizePath(profile_root, mustWork = TRUE))
}

.sw51_bind_rows_fill <- function(xs) {
  xs <- xs[vapply(xs, is.data.frame, logical(1L))]
  if (!length(xs)) return(data.frame())
  cols <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(cols, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[cols]
  })
  do.call(rbind, xs)
}

.sw51_import_csvs <- function(component, audit, base_dir = NULL,
                              include_roles = NULL) {
  keep <- grepl("[.]csv$", audit$path, ignore.case = TRUE)
  if (!is.null(include_roles)) {
    if (!is.character(include_roles) || anyDuplicated(include_roles) ||
        !all(include_roles %in% audit$role)) {
      stop("CSV import allow-list is malformed or unmanifested.",
           call. = FALSE)
    }
    keep <- keep & audit$role %in% include_roles
  }
  rows <- audit[keep, , drop = FALSE]
  out <- list(); meta <- list()
  if (!nrow(rows)) return(list(tables = out, inventory = data.frame()))
  for (j in seq_len(nrow(rows))) {
    read <- .sw51_stable_read_csv(rows$path[[j]])
    if (!identical(read$md5, rows$expected_md5[[j]])) {
      stop("CSV hash changed after manifest validation: ", rows$path[[j]],
           call. = FALSE)
    }
    role <- rows$role[[j]]
    if (!is.null(base_dir)) {
      prefix <- paste0(normalizePath(base_dir, mustWork = TRUE), "/")
      np <- normalizePath(rows$path[[j]], mustWork = TRUE)
      if (startsWith(np, prefix)) role <- substring(np, nchar(prefix) + 1L)
    }
    key <- paste(component,
                 gsub("[^A-Za-z0-9._-]+", "__", sub("[.]csv$", "", role,
                                                     ignore.case = TRUE)),
                 sep = "__")
    if (key %in% names(out)) stop("Duplicate normalized table key: ", key,
                                 call. = FALSE)
    out[[key]] <- read$value
    meta[[key]] <- data.frame(
      table = key, component = component, source_role = role,
      source_path = read$path, source_md5 = read$md5,
      rows = nrow(read$value), columns = ncol(read$value),
      stringsAsFactors = FALSE)
  }
  list(tables = out, inventory = do.call(rbind, meta))
}

.sw51_get_table <- function(tables, key) {
  x <- tables[[key]]
  if (!is.data.frame(x)) stop("Required normalized table absent: ", key,
                             call. = FALSE)
  x
}

.sw51_fact_row <- function(domain, fact, value, unit, status, source, note) {
  data.frame(domain = domain, fact = fact, value = as.character(value),
             unit = unit, status = status, source_table = source, note = note,
             stringsAsFactors = FALSE)
}

.sw51_sample_design_completion_facts <- function(tables) {
  flow_key <- "postfit__tables__preparation__prep_sample_flow"
  mod_key <- "postfit__tables__preparation__prep_moderator_dictionary"
  coord_key <- "postfit__tables__preparation__prep_coordinate_dictionary"
  rank_key <- "postfit__tables__preparation__design_rank_summary"
  completion_key <- "postfit__tables__preparation__completion_key_facts"
  design_key <- "postfit__tables__design__assessment_ledger"
  flow <- .sw51_get_table(tables, flow_key)
  mods <- .sw51_get_table(tables, mod_key)
  coords <- .sw51_get_table(tables, coord_key)
  rank <- .sw51_get_table(tables, rank_key)
  completion <- .sw51_get_table(tables, completion_key)
  design <- .sw51_get_table(tables, design_key)
  at_flow <- function(stage, field) {
    z <- flow[flow$stage == stage, field, drop = TRUE]
    if (length(z) != 1L) stop("Sample-flow stage is ambiguous: ", stage,
                             call. = FALSE)
    z
  }
  at_rank <- function(label) {
    z <- rank[rank$diagnostic == label, "value", drop = TRUE]
    if (length(z) != 1L) stop("Design-rank fact is ambiguous: ", label,
                             call. = FALSE)
    z
  }
  at_completion <- function(label) {
    z <- completion[completion$fact == label, "value", drop = TRUE]
    if (length(z) != 1L) stop("Completion fact is ambiguous: ", label,
                             call. = FALSE)
    z
  }
  design_status <- function(label) {
    z <- design[design$component == label, "status", drop = TRUE]
    if (length(z) != 1L) stop("Design status is ambiguous: ", label,
                             call. = FALSE)
    z
  }
  rows <- list(
    .sw51_fact_row("sample", "raw respondents",
      at_flow("raw SSI", "respondents"), "respondents", "observed", flow_key,
      "Anonymous ResponseId is the respondent key."),
    .sw51_fact_row("sample", "raw tasks", at_flow("raw SSI", "tasks"),
      "candidate-pair tasks", "observed", flow_key, "Before exclusions."),
    .sw51_fact_row("sample", "primary respondents",
      at_flow("three tasks plus valid primary demographics", "respondents"),
      "respondents", "reported_primary_sample", flow_key,
      "Complete-case equal-respondent target; no survey weights."),
    .sw51_fact_row("sample", "primary tasks",
      at_flow("three tasks plus valid primary demographics", "tasks"),
      "candidate-pair tasks", "reported_primary_sample", flow_key,
      "Three observed tasks per included respondent."),
    .sw51_fact_row("specification", "primary moderator dimension",
      sum(mods$primary_15), "pre-conjoint moderator columns", "frozen", mod_key,
      "Post-conjoint moderators remain sensitivity-only."),
    .sw51_fact_row("design", "profile contrast dimension", nrow(coords),
      "reference-coded coordinates", "observed", coord_key, "p = 13."),
    .sw51_fact_row("design", "rank of intercept/mean map",
      at_rank("rank of [1,C*]"), "rank", "conditional_theoretical_result",
      rank_key, "Conditional on advertised full support; not fielding-certified."),
    .sw51_fact_row("design", "rank of symmetric quadratic map",
      at_rank("rank of symmetric quadratic map on C*"), "rank",
      "conditional_theoretical_result", rank_key,
      "Conditional on advertised full support; not fielding-certified."),
    .sw51_fact_row("design", "within-respondent repeated contrasts",
      at_rank("within-respondent repeated contrasts"), "respondents",
      "sparse_descriptive_only", rank_key,
      "Insufficient for general exact repeated-contrast calibration."),
    .sw51_fact_row("completion", "two-task unfinished respondents",
      at_completion("two-task unfinished respondents"), "respondents",
      "observed", completion_key, "Completion descriptives only."),
    .sw51_fact_row("completion", "final analysis respondents",
      at_completion("final analysis respondents"), "respondents",
      "reported_primary_sample", completion_key,
      "Noninformative completion is not verified."),
    .sw51_fact_row("design", "exact ordered-contrast HT benchmark", NA,
      "not available", design_status(
        "exact ordered-contrast Horvitz--Thompson benchmark"), design_key,
      "Fielded probabilities and cross-task restrictions are unavailable."))
  do.call(rbind, rows)
}

.sw51_component_status <- function(parent, postfit, rank, sensitivity,
                                   profile) {
  sensitivity_state <- .sw51_sensitivity_claim_state(sensitivity)
  data.frame(
    component = c("reported primary", "post-fit evidence",
                  "rank/numerical diagnostics",
                  "production application sensitivities",
                  "production misspecification simulations",
                  "descriptive profile sequences", "formal inference",
                  "exact design HT", "majority claims"),
    status = c(
      paste0("resolved:", parent$pointer$reported_primary),
      "completed_hash_valid",
      if (isTRUE(rank$manifest$empirical_numerical_stability_gate_pass))
        "completed_empirical_stability_pass" else
        "completed_empirical_stability_not_pass",
      if (sensitivity_state$gated_application_components &&
          sensitivity_state$completion_design &&
          !sensitivity_state$completion_structural)
        "completed_componentwise_completion_structural_refit_unavailable"
      else "failed_closed_component_state",
      "completed_simulated_data_diagnostic",
      profile$status, "withheld", "protocol_unavailable_not_run",
      "prohibited_without_valid_interval"),
    completed = c(TRUE, TRUE, TRUE, TRUE, TRUE, profile$complete,
                  FALSE, FALSE, FALSE),
    formal_inference_available = FALSE,
    outcome_blind = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                      FALSE, NA, NA),
    changes_reported_primary = FALSE,
    note = c(
      "Post-pilot/outcome-informed guardrail; descriptive comparison, not a formal test.",
      "All producer and estimator-independent inputs were hash-validated.",
      "Fixed q=0,1,2 specifications; q was not selected; empirical numerical gates are not asymptotic certificates.",
      paste("Global validator failed on completion_optimizer_gates; 19-Z,",
            "Male x prior-run, task/position/serial, and raw completion",
            "descriptives remain component-gated; structural completion",
            "refit comparisons are unavailable."),
      "Design-specific simulated-data diagnostics; not empirical alternative-family refits and no coverage claim.",
      profile$reason,
      "Diagnostic one-step calculations fail the formal verification gate.",
      "Exact fielded assignment probabilities and cross-task rules are unavailable.",
      "Sign-share point summaries do not authorize a regular majority claim."),
    stringsAsFactors = FALSE)
}

.sw51_claims_ledger <- function(profile, sensitivity) {
  sensitivity_state <- .sw51_sensitivity_claim_state(sensitivity)
  out <- data.frame(
    claim_id = c(
      "reported_primary", "heldout_prediction", "structural_plugin_points",
      "direct_amce", "rank_comparison", "numerical_stability",
      "application_sensitivity", "misspecification_simulation",
      "profile_sequences", "formal_inference", "majority",
      "exact_design_ht", "fielded_protocol", "noninformative_completion",
      "maintained_distribution", "off_support", "rank_selection"),
    claim = c(
      "reported primary procedure", "held-out predictive performance",
      "structural plug-in point summaries", "marginal AMCE-style benchmark",
      "fixed-rank descriptive comparison", "finite-GH numerical stability",
      "application perturbation sensitivity",
      "design-specific misspecification simulation",
      "descriptive penalized-criterion profile sequences",
      "formal structural inference",
      "population majority preference", "exact ordered-contrast HT benchmark",
      "fielded randomization probabilities verified",
      "noninformative completion verified",
      "maintained normal/common-covariance/independent-shock assumptions verified",
      "unconditional off-support counterfactual", "empirically selected rank"),
    evidence_state = c(
      "resolved_hash_valid_postpilot", "respondent_cross_fitted_diagnostic",
      "full_sample_point_estimates_only",
      "conditional_on_advertised_randomization_different_estimand",
      "run_q0_q1_q2_rank_not_selected",
      "empirical_gate_not_asymptotic_certificate",
      "run_outcome_informed_descriptive",
      "run_simulated_data_diagnostic_not_empirical_refit",
      profile$status, "withheld", "no_regular_majority_claim",
      "protocol_unavailable_not_run", "unavailable", "not_verified",
      "maintained_not_verified", "conditional_only", "false"),
    allowed_in_section_5_1 = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      profile$reportable, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE),
    formal_inference = FALSE,
    majority_claim = FALSE,
    exact_design_ht_claim = FALSE,
    maintained_assumption_verified = FALSE,
    required_qualifier = c(
      "post-pilot/outcome-informed; guardrail is descriptive",
      "respondent cross-fitted; no ordinary inference",
      "point estimate; no formal interval",
      "different estimand; advertised design only; clustered diagnostic SE",
      "fixed-rank sensitivity; rank not selected",
      "finite-grid empirical diagnostic; no asymptotic certificate",
      paste(
        "outcome-informed descriptive sensitivity; only component-gated",
        "19-Z, Male x prior-run, and task/position/serial diagnostics"),
      "simulated-data diagnostic; no empirical alternative likelihood or coverage",
      "only completed, direction-verified penalized-criterion sequences; unpenalized likelihoods are overlays, not literal profiles; no LR critical values",
      "do not report as formal inference",
      "do not translate sign-share points into majority language",
      "do not report; protocol probabilities unavailable",
      "do not call the fielded design machine-verified",
      "completion balance does not establish ignorability",
      "maintained assumptions remain unverified",
      "conditional on advertised support; fielded support not certified",
      "q=1 remains the reported primary; q panels are sensitivities"),
    stringsAsFactors = FALSE)
  out$evidence_state[out$claim_id == "application_sensitivity"] <-
    if (sensitivity_state$gated_application_components)
      "run_componentwise_global_validator_failed_completion_refits" else
      "unavailable_component_gate_fail"
  out$allowed_in_section_5_1[
    out$claim_id == "application_sensitivity"] <-
    sensitivity_state$gated_application_components
  rbind(out, data.frame(
    claim_id = c("completion_design_descriptives",
                 "completion_structural_refits"),
    claim = c(
      "completion sample/design/direct-AMCE descriptives",
      "completion-sample structural refit comparisons"),
    evidence_state = c(
      if (sensitivity_state$completion_design)
        "run_directly_from_raw_sample_assignment_choice_data" else
        "unavailable_component_gate_fail",
      if (sensitivity_state$completion_structural)
        "run_structural_refit_gate_pass" else
        "unavailable_nested_objective_gate_fail"),
    allowed_in_section_5_1 = c(
      sensitivity_state$completion_design,
      sensitivity_state$completion_structural),
    formal_inference = FALSE, majority_claim = FALSE,
    exact_design_ht_claim = FALSE, maintained_assumption_verified = FALSE,
    required_qualifier = c(
      paste("raw sample/assignment/choice or respondent-clustered LPM",
            "descriptive; completion ignorability remains unverified"),
      paste("do not report theta or structural choice comparisons unless",
            "both fixed no-Z refits pass the frozen optimizer gate")),
    stringsAsFactors = FALSE))
}

.sw51_reporting_ledger <- function(profile, sensitivity) {
  sensitivity_state <- .sw51_sensitivity_claim_state(sensitivity)
  out <- data.frame(
    quantity_family = c(
      "sample/design/completion facts", "heldout sequence scores",
      "structural mean/choice/heterogeneity points", "sign-share points",
      "diagnostic one-step quantities", "direct AMCE benchmarks",
      "rank/numerical diagnostics", "application sensitivities",
      "misspecification simulations", "profile sequences",
      "exact design HT", "majority statements"),
    point_estimate_allowed = c(
      TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
      sensitivity_state$gated_application_components, TRUE,
      profile$reportable, FALSE, FALSE),
    diagnostic_se_allowed = c(
      FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
      sensitivity_state$gated_application_components, TRUE,
      FALSE, FALSE, FALSE),
    formal_interval_allowed = FALSE,
    hypothesis_test_allowed = FALSE,
    majority_interpretation_allowed = FALSE,
    exact_design_ht_interpretation_allowed = FALSE,
    status = c(
      "report_with_source_qualifiers", "descriptive_only",
      "point_estimate_only", "point_estimate_only_no_majority_claim",
      "withheld_from_reporting", "different_estimand_descriptive",
      "descriptive_fixed_specifications", "descriptive_outcome_informed",
      "simulated_data_diagnostic", profile$status,
      "protocol_unavailable_not_run", "prohibited"),
    stringsAsFactors = FALSE)
  rbind(out, data.frame(
    quantity_family = c("completion design/sample descriptives",
                        "completion structural refit comparisons"),
    point_estimate_allowed = c(sensitivity_state$completion_design,
                               sensitivity_state$completion_structural),
    diagnostic_se_allowed = c(sensitivity_state$completion_design, FALSE),
    formal_interval_allowed = FALSE, hypothesis_test_allowed = FALSE,
    majority_interpretation_allowed = FALSE,
    exact_design_ht_interpretation_allowed = FALSE,
    status = c(
      if (sensitivity_state$completion_design)
        "raw_or_direct_lpm_descriptive_only" else
        "unavailable_component_gate_fail",
      if (sensitivity_state$completion_structural)
        "descriptive_structural_refit_gate_pass" else
        "unavailable_nested_objective_gate_fail"),
    stringsAsFactors = FALSE))
}

.sw51_later_writing_guardrails <- function(profile, sensitivity) {
  sensitivity_state <- .sw51_sensitivity_claim_state(sensitivity)
  out <- data.frame(
    topic = c(
      "workflow timing", "reported primary", "prediction",
      "structural quantities", "rank", "numerical integration",
      "formal inference", "sign shares", "design benchmark",
      "design support", "completion", "sensitivities",
      "misspecification simulations", "profiles", "applications scope"),
    required_wording = c(
      "post-pilot and outcome-informed",
      "v2.1 pointer-resolved procedure under a descriptive noninferiority guardrail",
      "respondent cross-fitted post-pilot diagnostic",
      "full-sample structural plug-in point estimate",
      "fixed q=0,1,2 sensitivity; q was not selected",
      "empirical finite-GH stability diagnostic",
      "formal inference unavailable; diagnostic calculations only",
      "point summary with no regular majority claim",
      "respondent-clustered marginal AMCE-style benchmark, a different estimand",
      "conditional on advertised full support; fielded protocol not certified",
      "complete-case target; noninformative completion not verified",
      "outcome-informed descriptive perturbations",
      "design-specific simulated-data diagnostics, not empirical alternative likelihoods",
      if (identical(profile$status, "incomplete_failed_closed")) paste(
        "profile battery incomplete and failed closed; report only the",
        "completed direction-verified penalized-criterion diagnostics; the",
        "missing direction is unavailable; unpenalized likelihood overlays",
        "are not literal profile likelihoods") else if (profile$reportable)
        paste("descriptive penalized-criterion profile sequences with",
              "unpenalized likelihood overlays; no LR inference") else if (
          profile$available)
        "profile computation completed but a verification gate failed; report status only"
      else paste("penalized-criterion profile sequences not run; literal",
                 "profile likelihoods not constructed"),
      "Saha--Weeks only; Section 5.1 prose remains unwritten"),
    prohibited_shortcut = c(
      "preregistered or outcome-blind", "formally selected best model",
      "confirmatory held-out test", "posterior estimate or confidence interval",
      "selected rank", "asymptotic numerical-error certificate",
      "valid confidence interval, p-value, or test", "a majority prefers",
      "structural preference coefficient or exact HT estimate",
      "verified fielded assignment probabilities", "missingness is ignorable",
      "robustness proves the model", "empirical nonnormal refit or coverage study",
      "likelihood-ratio test or global identification proof",
      "updated prose for all three applications"),
    stringsAsFactors = FALSE)
  out$gate_satisfied <- TRUE
  rbind(out, data.frame(
    topic = c("componentwise sensitivity validation",
              "completion design/sample descriptives",
              "completion structural refits"),
    required_wording = c(
      paste("global application validator failed only on completion refit",
            "optimizer gates; separately gated 19-Z, Male x prior-run, and",
            "task/position/serial diagnostics remain descriptive"),
      paste("raw sample/assignment/choice summaries and direct",
            "respondent-clustered LPM AMCE comparisons; no completion",
            "ignorability or structural-refit interpretation"),
      paste("unavailable: both fixed no-Z ReLU refits failed the frozen",
            "nested-objective gate, making main and overall gates fail",
            "while pooled/continued gates passed")),
    prohibited_shortcut = c(
      "the full application sensitivity battery passed",
      "completion-adjusted structural estimates",
      "completion structural theta or choice comparison"),
    gate_satisfied = c(
      sensitivity_state$gated_application_components,
      sensitivity_state$completion_design,
      !sensitivity_state$completion_structural),
    stringsAsFactors = FALSE))
}

.sw51_fail_closed_gate <- function(component_status, claims, reporting,
                                   tables, profile, sensitivity) {
  sign <- .sw51_get_table(
    tables, "postfit__tables__structural__sign_shares")
  design <- .sw51_get_table(
    tables, "postfit__tables__design__conditional_randomization_status")
  inference <- .sw51_get_table(
    tables, "postfit__tables__inference__status")
  sign_ok <- all(sign$formal_inference_available == FALSE) &&
    all(sign$majority_claim == "no regular majority claim")
  ht <- design[grepl("Horvitz", design$proposed_test), , drop = FALSE]
  ht_ok <- nrow(ht) == 1L &&
    identical(ht$status, "protocol_unavailable_not_run") &&
    !isTRUE(ht$protocol_verified) && is.na(ht$p_value)
  inference_ok <- any(inference$component == "formal inference" &
                        inference$status == "withheld")
  ledger_ok <- !any(claims$formal_inference) &&
    !any(claims$majority_claim) && !any(claims$exact_design_ht_claim) &&
    !any(claims$maintained_assumption_verified) &&
    !any(reporting$formal_interval_allowed) &&
    !any(reporting$hypothesis_test_allowed) &&
    !any(reporting$majority_interpretation_allowed) &&
    !any(reporting$exact_design_ht_interpretation_allowed) &&
    !any(component_status$formal_inference_available)
  profile_ok <-
    all(profile$direction_status$formal_inference_available == FALSE) &&
    !any(profile$direction_status$literal_likelihood_profile) &&
    !any(profile$direction_status$reportable_descriptive[
      !profile$direction_status$manifest_available]) &&
    (!identical(profile$status, "incomplete_failed_closed") ||
       (any(profile$direction_status$manifest_available) &&
          any(!profile$direction_status$manifest_available)))
  sensitivity_state <- .sw51_sensitivity_claim_state(sensitivity)
  validation <- sensitivity$validation
  application_row <- validation$table[
    validation$table$component == "application", , drop = FALSE]
  misspecification_row <- validation$table[
    validation$table$component == "misspecification", , drop = FALSE]
  sensitivity_validator_ok <-
    identical(validation$pass, FALSE) && nrow(application_row) == 1L &&
    identical(application_row$pass, FALSE) &&
    identical(application_row$reasons, "completion_optimizer_gates") &&
    nrow(misspecification_row) == 1L &&
    identical(misspecification_row$pass, TRUE)
  sensitivity_components_ok <-
    sensitivity_state$gated_application_components &&
    sensitivity_state$completion_design &&
    !sensitivity_state$completion_structural &&
    all(sensitivity$application_components$component_status$
          formal_inference_available == FALSE)
  disposition <- sensitivity$application_components$table_disposition
  allowed_roles <- disposition$role[disposition$reportable_descriptive]
  expected_application_keys <- paste(
    "sensitivity_application",
    gsub("[^A-Za-z0-9._-]+", "__", sub("[.]csv$", "", allowed_roles,
                                         ignore.case = TRUE)), sep = "__")
  actual_application_keys <- grep(
    "^sensitivity_application__", names(tables), value = TRUE)
  application_export_ok <-
    setequal(actual_application_keys, expected_application_keys) &&
    !any(grepl("completion_(theta|choice)_comparison$",
               actual_application_keys)) &&
    all(c(
      "sensitivity_application__tables__completion_sample",
      "sensitivity_application__tables__completion_amce_comparison",
      paste0("sensitivity_application__tables__completion_early_",
             c("task_by_eventual_completion",
               "assignment_response_balance"))) %in% names(tables))
  completion_claim_ok <-
    any(claims$claim_id == "completion_design_descriptives" &
          claims$allowed_in_section_5_1) &&
    any(claims$claim_id == "completion_structural_refits" &
          !claims$allowed_in_section_5_1 &
          claims$evidence_state ==
            "unavailable_nested_objective_gate_fail") &&
    any(reporting$quantity_family ==
          "completion structural refit comparisons" &
          !reporting$point_estimate_allowed &
          reporting$status == "unavailable_nested_objective_gate_fail")
  gates <- data.frame(
    gate = c("sign shares retain no-majority label",
             "exact design HT remains protocol-unavailable/not-run",
             "post-fit formal inference remains withheld",
             "new claims/reporting ledgers fail closed",
             "profile directions remain descriptive and fail closed",
             "sensitivity validator retains componentwise failed state",
             "sensitivity component gates retain frozen dispositions",
             "application CSV export suppresses failed structural refits",
             "completion claims/reporting remain componentwise fail closed"),
    pass = c(sign_ok, ht_ok, inference_ok, ledger_ok, profile_ok,
             sensitivity_validator_ok, sensitivity_components_ok,
             application_export_ok, completion_claim_ok),
    required = TRUE, stringsAsFactors = FALSE)
  if (!all(gates$pass)) {
    stop("Final Section 5.1 fail-closed claim gate failed: ",
         paste(gates$gate[!gates$pass], collapse = ", "), call. = FALSE)
  }
  gates
}

.sw51_plot_tables <- function(tables, profile) {
  exact <- c(
    plot__direct_amce = "postfit__tables__design__direct_amce",
    plot__heldout_calibration =
      "postfit__tables__prediction__calibration_marginal",
    plot__structural_latent_contrasts =
      "postfit__tables__structural__latent_preference_contrasts",
    plot__position_neutral_choices =
      "postfit__tables__structural__position_neutral_choice",
    plot__heterogeneity = "postfit__tables__structural__heterogeneity",
    plot__completion_flow = "postfit__tables__preparation__prep_sample_flow")
  out <- lapply(exact, function(key) .sw51_get_table(tables, key))
  rank_patterns <- list(
    plot__rank_sequence_scores = "rank_numerical__.*rank_sequence_scores$",
    plot__rank_qoi = "rank_numerical__.*rank_qoi$",
    plot__numerical_refinement =
      "rank_numerical__.*q[12]_refinement_summary$",
    plot__numerical_refinement_qoi =
      "rank_numerical__.*q[12]_refinement_qoi$",
    plot__rotation_stability =
      "rank_numerical__.*q2_refinement_rotation$")
  for (nm in names(rank_patterns)) {
    hit <- grep(rank_patterns[[nm]], names(tables), value = TRUE)
    if (!length(hit)) stop("Required rank plot source absent: ", nm,
                           call. = FALSE)
    out[[nm]] <- .sw51_bind_rows_fill(lapply(hit, function(key) {
      z <- tables[[key]]; z$source_table <- key; z
    }))
  }
  sensitivity_patterns <- c(
    "z19_theta$", "z19_choices$", "male_run_conditional_effects$",
    "male_run_choice_probabilities$", "position_profile_swap$",
    "completion_sample$", "completion_amce_comparison$",
    "completion_early_task_by_eventual_completion$",
    "completion_early_assignment_response_balance$",
    "qoi_bias_stability$", "dgp_calibration$")
  for (pattern in sensitivity_patterns) {
    hit <- grep(paste0("sensitivity_.*__", pattern), names(tables),
                value = TRUE)
    for (key in hit) out[[paste0("plot__", key)]] <- tables[[key]]
  }
  if (profile$available) {
    hit <- grep("^profile_sequences__", names(tables), value = TRUE)
    for (key in hit) {
      if (grepl("profile|sequence|curvature", key, ignore.case = TRUE)) {
        out[[paste0("plot__", key)]] <- tables[[key]]
      }
    }
  }
  out
}

.sw51_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.sw51_write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(x, path, useBytes = TRUE)
  invisible(path)
}

.sw51_safe_output_name <- function(x) {
  gsub("[^A-Za-z0-9._-]+", "__", x)
}

.sw51_source_snapshot <- function(audits, direct_reads, script_path) {
  audit <- do.call(rbind, audits)
  direct <- do.call(rbind, lapply(names(direct_reads), function(nm) {
    x <- direct_reads[[nm]]
    data.frame(component = nm, kind = "direct_stable_read",
      role = basename(x$path), path = x$path, expected_md5 = x$md5,
      observed_md5 = x$md5, exists = TRUE, match = TRUE,
      bytes = as.numeric(file.info(x$path)$size), stringsAsFactors = FALSE)
  }))
  script <- data.frame(
    component = "section5_1_aggregator", kind = "runner",
    role = basename(script_path), path = script_path,
    expected_md5 = unname(tools::md5sum(script_path)),
    observed_md5 = unname(tools::md5sum(script_path)), exists = TRUE,
    match = TRUE, bytes = as.numeric(file.info(script_path)$size),
    stringsAsFactors = FALSE)
  out <- rbind(audit, direct, script)
  if (anyDuplicated(paste(out$component, out$kind, out$role, out$path))) {
    stop("Source-input audit contains duplicate roles.", call. = FALSE)
  }
  out
}

.sw51_revalidate_snapshot <- function(snapshot) {
  observed <- .sw51_md5(snapshot$path)
  ok <- !is.na(observed) & unname(observed) == snapshot$expected_md5
  if (!all(ok)) {
    stop("A source changed during bundle construction: ",
         paste(unique(snapshot$path[!ok]), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.sw51_readme <- function(parent, rank, profile, sensitivity) {
  sensitivity_state <- .sw51_sensitivity_claim_state(sensitivity)
  reportable_profile <- profile$direction_status$direction[
    profile$direction_status$reportable_descriptive]
  unavailable_profile <- profile$direction_status$direction[
    !profile$direction_status$manifest_available]
  c(
    "# Saha--Weeks v2.1 Section 5.1 evidence bundle",
    "",
    "This directory is a machine-readable evidence export, not drafted manuscript prose.",
    "",
    paste0("- Reported primary: `", parent$pointer$reported_primary, "`."),
    "- Workflow status: post-pilot, outcome-informed, and descriptive.",
    "- Formal structural inference: unavailable.",
    "- Majority claims: unavailable; sign-share values are point summaries only.",
    "- Exact ordered-contrast Horvitz--Thompson benchmark: not run because the fielded protocol probabilities and cross-task rules are unavailable.",
    "- Rank panels: fixed q=0,1,2 sensitivities; rank was not selected.",
    paste0("- Rank/numerical empirical stability gate: `",
           rank$manifest$empirical_numerical_stability_gate_pass,
           "`; this is not an asymptotic certificate."),
    paste0("- Profile-sequence status: `", profile$status, "`."),
    paste0("- Reportable descriptive profile directions: `",
           if (length(reportable_profile))
             paste(reportable_profile, collapse = ", ") else "none", "`."),
    paste0("- Unavailable profile directions: `",
           if (length(unavailable_profile))
             paste(unavailable_profile, collapse = ", ") else "none", "`."),
    "- Profile outputs are penalized-criterion sequences with unpenalized likelihood overlays; they are not literal likelihood profiles or likelihood-ratio inference.",
    "- Production application sensitivity validator: globally failed closed on `completion_optimizer_gates`; do not relabel the full battery as passed.",
    paste0("- Separately reportable application components: 19-Z, Male x prior-run, task/position/serial, and position-swap diagnostics (`",
           sensitivity_state$gated_application_components, "`)."),
    paste0("- Raw completion sample/design/direct-LPM descriptives remain descriptive (`",
           sensitivity_state$completion_design,
           "`); they do not verify ignorable completion."),
    paste0("- Completion structural theta/choice refit comparisons are reportable: `",
           sensitivity_state$completion_structural,
           "`; for both fixed no-Z ReLU refits, pooled/continued gates passed but nested-objective, main, and overall gates failed."),
    "- Design-specific misspecification simulations passed their validation gates, but remain simulated-data diagnostics and do not verify maintained assumptions.",
    "",
    "Start with `tables/claims__claims_ledger.csv`, `tables/claims__reporting_ledger.csv`, `tables/guardrails__later_writing.csv`, and `tables/facts__sample_design_completion.csv`. The `plot__*.csv` files are plot-ready views; all other normalized CSVs preserve the producer tables with component prefixes.",
    "",
    "`objects/section5_1_evidence_bundle_v2_1.rds` contains the normalized tables and provenance snapshot. `manifests/source_input_manifest.csv` records every validated input. `manifests/bundle_artifact_manifest.csv` hashes every exported artifact other than itself.",
    "",
    "Do not use this bundle to claim formal confidence intervals, p-values, a majority preference, a verified fielded randomizer, exact design HT effects, ignorable completion, completion structural refit comparisons, selected rank, or verified maintained distributional assumptions."
  )
}

.sw51_main <- function() {
  cli <- .sw51_parse_cli(commandArgs(trailingOnly = TRUE))
  script <- .sw51_script_file()
  root <- normalizePath(file.path(dirname(script), "..", "..", "..", ".."),
                        mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  results <- file.path(app, "results")
  parent <- .sw51_validate_parent(file.path(
    results, "mixed_logit_v2_1_postpilot_final"))
  postfit <- .sw51_validate_postfit(file.path(
    results, "postfit_evidence_v2_1", "final"), parent)
  rank <- .sw51_validate_rank(file.path(
    results, "mixed_logit_v2_1_rank_numerical"), parent)
  sensitivity <- .sw51_validate_sensitivity(file.path(
    results, "mixed_logit_v2_1_sensitivity"), parent, "production")
  profile_root <- file.path(results, "mixed_logit_v2_1_profile_sequences")
  profile <- .sw51_validate_optional_profile(
    profile_root, parent, sensitivity)

  imported <- list(
    .sw51_import_csvs("postfit", postfit$artifact_audit,
                      file.path(results, "postfit_evidence_v2_1", "final")),
    .sw51_import_csvs("rank_numerical", rank$artifact_audit,
                      file.path(results, "mixed_logit_v2_1_rank_numerical")),
    .sw51_import_csvs("sensitivity_application",
                      sensitivity$application$artifact_audit,
                      file.path(sensitivity$profile_dir, "application"),
                      include_roles =
                        sensitivity$application_components$
                          table_disposition$role[
                            sensitivity$application_components$
                              table_disposition$reportable_descriptive]),
    .sw51_import_csvs("sensitivity_misspecification",
                      sensitivity$misspecification$artifact_audit,
                      file.path(sensitivity$profile_dir, "misspecification")))
  validation_key <- "sensitivity_validation__validation"
  imported[[5L]] <- list(
    tables = stats::setNames(list(sensitivity$validation_csv$value),
                             validation_key),
    inventory = data.frame(
      table = validation_key, component = "sensitivity_validation",
      source_role = "validation.csv",
      source_path = sensitivity$validation_csv$path,
      source_md5 = sensitivity$validation_csv$md5,
      rows = nrow(sensitivity$validation_csv$value),
      columns = ncol(sensitivity$validation_csv$value),
      stringsAsFactors = FALSE))
  if (profile$available && nrow(profile$artifact_audit)) {
    imported[[6L]] <- .sw51_import_csvs(
      "profile_sequences", profile$artifact_audit, profile$base_dir)
  } else if (!profile$root_present) {
    profile_key <- "profile_sequences__profile_likelihood_status"
    imported[[6L]] <- list(
      tables = stats::setNames(list(profile$status_csv$value), profile_key),
      inventory = data.frame(
        table = profile_key, component = "profile_sequences",
        source_role = "validated_production_not_run_status",
        source_path = profile$status_csv$path,
        source_md5 = profile$status_csv$md5,
        rows = nrow(profile$status_csv$value),
        columns = ncol(profile$status_csv$value),
        stringsAsFactors = FALSE))
  } else {
    imported[[6L]] <- list(tables = list(), inventory = data.frame())
  }
  tables <- unlist(lapply(imported, `[[`, "tables"), recursive = FALSE)
  if (anyDuplicated(names(tables))) stop("Normalized table names collide.",
                                        call. = FALSE)
  inventory <- do.call(rbind, lapply(imported, `[[`, "inventory"))
  facts <- .sw51_sample_design_completion_facts(tables)
  component_status <- .sw51_component_status(
    parent, postfit, rank, sensitivity, profile)
  claims <- .sw51_claims_ledger(profile, sensitivity)
  reporting <- .sw51_reporting_ledger(profile, sensitivity)
  guardrails <- .sw51_later_writing_guardrails(profile, sensitivity)
  final_gates <- .sw51_fail_closed_gate(
    component_status, claims, reporting, tables, profile, sensitivity)
  plot_tables <- .sw51_plot_tables(tables, profile)

  audits <- list(
    parent$input_audit, parent$artifact_audit,
    postfit$input_audit, postfit$artifact_audit,
    rank$input_audit, rank$artifact_audit,
    sensitivity$application$input_audit,
    sensitivity$application$artifact_audit,
    sensitivity$misspecification$input_audit,
    sensitivity$misspecification$artifact_audit)
  direct_reads <- list(
    parent_manifest = parent$manifest_read,
    parent_pointer = parent$pointer_read,
    postfit_manifest = postfit$manifest_read,
    rank_manifest = rank$manifest_read,
    rank_final = rank$final_read,
    sensitivity_validation = sensitivity$validation_read,
    sensitivity_validation_csv = sensitivity$validation_csv,
    sensitivity_application_manifest =
      sensitivity$application$manifest_read,
    sensitivity_misspecification_manifest =
      sensitivity$misspecification$manifest_read)
  for (component in names(sensitivity$application_components$reads)) {
    direct_reads[[paste0("sensitivity_application_component_", component)]] <-
      sensitivity$application_components$reads[[component]]
  }
  if (profile$root_present) {
    audits <- c(audits, list(profile$input_audit))
    if (nrow(profile$artifact_audit)) {
      audits <- c(audits, list(profile$artifact_audit))
    }
    direct_reads$profile_authorization <- profile$authorization_read
    for (direction in names(profile$manifest_reads)) {
      direct_reads[[paste0("profile_manifest_", direction)]] <-
        profile$manifest_reads[[direction]]
    }
  } else {
    direct_reads$profile_not_run_status <- profile$status_csv
  }
  source_snapshot <- .sw51_source_snapshot(audits, direct_reads, script)

  output_parent <- file.path(results, "section5_1_bundle_v2_1")
  target <- file.path(output_parent, cli$output_name)
  if (file.exists(target) || dir.exists(target)) {
    stop("Add-only target already exists: ", target, call. = FALSE)
  }
  dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(paste0(".", cli$output_name, "-"),
                    tmpdir = output_parent)
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create staging directory.", call. = FALSE)
  }
  published <- FALSE
  on.exit(if (!published && dir.exists(stage))
    unlink(stage, recursive = TRUE, force = FALSE), add = TRUE)
  dir.create(file.path(stage, "objects"))
  dir.create(file.path(stage, "tables"))
  dir.create(file.path(stage, "manifests"))

  all_tables <- c(
    tables,
    c(list(
      facts__sample_design_completion = facts,
      status__component_status = component_status,
      status__sensitivity_application_components =
        sensitivity$application_components$component_status,
      status__sensitivity_application_table_disposition =
        sensitivity$application_components$table_disposition,
      status__profile_directions = profile$direction_status,
      claims__claims_ledger = claims,
      claims__reporting_ledger = reporting,
      guardrails__later_writing = guardrails,
      status__final_fail_closed_gates = final_gates),
      if (nrow(profile$absence_audit)) list(
        status__profile_absence_audit = profile$absence_audit) else list()),
    plot_tables)
  if (anyDuplicated(names(all_tables))) {
    stop("Final normalized/export table names collide.", call. = FALSE)
  }
  table_paths <- stats::setNames(file.path(
    stage, "tables", paste0(.sw51_safe_output_name(names(all_tables)),
                            ".csv")), names(all_tables))
  for (nm in names(all_tables)) .sw51_write_csv(all_tables[[nm]],
                                                table_paths[[nm]])
  exported_inventory <- do.call(rbind, lapply(names(all_tables), function(nm) {
    data.frame(
      table = nm,
      bundle_path = file.path("tables", basename(table_paths[[nm]])),
      rows = nrow(as.data.frame(all_tables[[nm]])),
      columns = ncol(as.data.frame(all_tables[[nm]])),
      md5 = unname(tools::md5sum(table_paths[[nm]])),
      stringsAsFactors = FALSE)
  }))
  .sw51_write_csv(exported_inventory,
                  file.path(stage, "tables", "inventory__tables.csv"))
  .sw51_write_csv(source_snapshot,
                  file.path(stage, "manifests", "source_input_manifest.csv"))
  .sw51_write_lines(.sw51_readme(parent, rank, profile, sensitivity),
                    file.path(stage, "README.md"))

  bundle <- list(
    schema_version = "sw2022-v2.1-section5.1-evidence-bundle-v1",
    reported_primary = parent$pointer$reported_primary,
    fallback_applied = isTRUE(parent$pointer$fallback_applied),
    postpilot_outcome_informed = TRUE,
    formal_inference_available = FALSE,
    majority_claim_available = FALSE,
    exact_design_ht_available = FALSE,
    rank_selected = FALSE,
    profile_status = profile$status,
    component_status = component_status,
    sensitivity_application_component_status =
      sensitivity$application_components$component_status,
    sensitivity_application_table_disposition =
      sensitivity$application_components$table_disposition,
    claims_ledger = claims,
    reporting_ledger = reporting,
    later_writing_guardrails = guardrails,
    sample_design_completion_facts = facts,
    profile_direction_status = profile$direction_status,
    profile_absence_audit = profile$absence_audit,
    final_fail_closed_gates = final_gates,
    source_input_manifest = source_snapshot,
    source_table_inventory = inventory,
    exported_table_inventory = exported_inventory,
    normalized_tables = tables,
    plot_ready_tables = plot_tables,
    source_manifests = list(
      parent = parent$manifest, pointer = parent$pointer,
      postfit = postfit$manifest, rank = rank$manifest,
      rank_final = rank$final,
      sensitivity_validation = sensitivity$validation,
      sensitivity_application = sensitivity$application$manifest,
      sensitivity_misspecification = sensitivity$misspecification$manifest,
      sensitivity_application_component_gates =
        sensitivity$application_components$gates,
      profile = if (profile$root_present) list(
        authorization = profile$authorization,
        directions = profile$manifests,
        direction_status = profile$direction_status,
        status = profile$status, reason = profile$reason) else list(
          status = profile$status, reason = profile$reason)),
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    session_info = utils::capture.output(sessionInfo()))
  saveRDS(bundle,
          file.path(stage, "objects", "section5_1_evidence_bundle_v2_1.rds"),
          version = 3, compress = "xz")
  .sw51_write_lines(bundle$session_info,
                    file.path(stage, "manifests", "sessionInfo.txt"))

  .sw51_revalidate_snapshot(source_snapshot)
  .sw51_revalidate_profile_absences(profile)
  output_files <- list.files(stage, recursive = TRUE, full.names = TRUE)
  output_files <- output_files[!dir.exists(output_files)]
  manifest_path <- file.path(stage, "manifests",
                             "bundle_artifact_manifest.csv")
  output_files <- setdiff(output_files, manifest_path)
  rel <- substring(output_files, nchar(stage) + 2L)
  artifact_manifest <- data.frame(
    path = rel, bytes = as.numeric(file.info(output_files)$size),
    md5 = unname(tools::md5sum(output_files)),
    stringsAsFactors = FALSE)
  artifact_manifest <- artifact_manifest[order(artifact_manifest$path), ,
                                         drop = FALSE]
  .sw51_write_csv(artifact_manifest, manifest_path)
  if (!file.rename(stage, target)) {
    stop("Atomic publication of final Section 5.1 bundle failed.",
         call. = FALSE)
  }
  published <- TRUE
  message("Saha--Weeks v2.1 Section 5.1 evidence bundle published: ", target)
  invisible(bundle)
}

if (sys.nframe() == 0L) .sw51_main()
