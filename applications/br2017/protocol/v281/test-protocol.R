#!/usr/bin/env Rscript

## Outcome-free static checks for the Ballard--Rosa v2.8.1 fit protocol.
##
## During implementation, hash fields may contain PENDING_FINALIZATION.  This
## script then verifies the scientific contract, file coverage, and parseability
## and reports the pending count.  Outcome commands remain unavailable because
## the production validator accepts only exact SHA-256 values.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
protocol_dir <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(protocol_dir, "..", "..", "..", ".."),
                           mustWork = TRUE)

checks <- 0L
pending <- character()
assert <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}
read_registry <- function(name) {
  utils::read.csv(file.path(protocol_dir, name), stringsAsFactors = FALSE,
                  check.names = FALSE, na.strings = character())
}
sha256 <- function(path) {
  line <- system2(Sys.which("shasum"), c("-a", "256", shQuote(path)),
                  stdout = TRUE, stderr = TRUE)
  assert(length(line) == 1L && grepl("^[0-9a-f]{64}[[:space:]]", line),
         paste("Could not hash", path))
  sub("[[:space:]].*$", "", line)
}
check_hash <- function(path, expected, label) {
  assert(file.exists(path), paste("Missing", label))
  if (identical(expected, "PENDING_FINALIZATION")) {
    pending <<- c(pending, label)
    return(invisible(TRUE))
  }
  assert(grepl("^[0-9a-f]{64}$", expected) &&
           identical(sha256(path), expected),
         paste("Hash mismatch:", label))
}

expected_files <- c(
  "README.md", "application-contract.dcf", "carry-forward-manifest.csv",
  "implementation-manifest.csv", "input-manifest.csv",
  "prior-v26-manifest.csv", "seed-registry.csv", "stage-registry.csv",
  "state-registry.csv", "test-protocol.R", "CHECKSUMS.sha256")
assert(setequal(list.files(protocol_dir), expected_files),
       "The v2.8.1 fit protocol directory has unregistered files")

contract <- unlist(read.dcf(file.path(protocol_dir,
                                      "application-contract.dcf"),
                            all = TRUE)[1L, , drop = TRUE], use.names = TRUE)
exact <- c(
  `Protocol-Id` = "br2017-paperps-prospective-2026-09-07-v2.8.1-v1",
  `Method-Protocol-Id` = "paperps-draft-development-2026-09-07-v2.8.1-v1",
  `Prior-Diagnostic-Protocol` = "br2017-paperps-rapid-2026-09-07-v2.7-v1",
  `Prior-Diagnostic-Git-Tag` = "diagnostic/br2017-v2.7-2026-09-07",
  `Prior-Stopped-Protocol` =
    "br2017-paperps-prospective-2026-09-07-v2.8-v1",
  `Prior-Stopped-Git-Commit` =
    "9b2f33536aa8cd5e36bd0fd300f7bcb2f97f15fb",
  `Prior-Stopped-Artifact` = "pre_estimation_preflight_only",
  `Prior-Stopped-File-Count` = "2",
  `Prior-Stopped-Preflight-SHA256` =
    "b9af73f4dcef2272ae6ca68f78af9e79de73a1a6d2a7fef697e1c3ffe31e4ea4",
  `Prior-Stopped-Preflight-Sidecar-SHA256` =
    "ede6423f99a6ce69ad2f8bef4bfb6bdd914cd3de5d8a65e1d7545c60c04b37df",
  `Amendment-Scope` =
    "one_dimensional_parameter_shape_preservation_in_gradient_audit",
  `Preflight-Reuse` = "prohibited",
  `Failed-Optimization-Reuse` = "prohibited",
  `Defensive-Hardening` = paste0(
    "exact_parameter_name_length_dimension_and_finiteness_checks_on_",
    "gradient_audit_state"),
  `Carried-Forward-Model-Choices` =
    "v21_mean_specification_network_architecture_and_weight_decay",
  `Fresh-Optimization-Rule` =
    "all_trainable_parameters_reoptimized_from_initializer",
  `Q2-Timing-Decision` = paste0(
    "deterministic_completed_q0_q1_fit_seconds_plus_6.25_times_",
    "q2_sentinel_seconds"),
  `Q2-Timing-Cutoff-Seconds` = "50400",
  `Q2-Stop-Analysis-Rule` =
    "complete_q0_q1_comparison_and_record_q2_deferred",
  `Q2-Second-Column-Raw-Norm-Ratio` = "0.05",
  `Q2-Minimum-Q1-Raw-Loading-Norm` = "0.0000000001",
  `Optimizer` = "float64_full_batch_LBFGS_strong_wolfe",
  `Blockwise-KKT-Metric` = paste0(
    "maximum_preprocessing_normalized_projected_gradient_RMS_",
    "across_applicable_blocks"),
  `Blockwise-KKT-Cutoff` = "0.00001",
  `Near-Optimum-Relative-Cutoff` = "0.000001",
  `Start-Selection` = paste0(
    "minimum_penalized_mean_NLL_among_computationally_eligible_",
    "starts"),
  `Singleton-Near-Set` = paste0(
    "not_applicable_not_a_failure_and_not_affirmative_cross_start_",
    "evidence"),
  `Start-State-Retention` = "every_candidate_state_and_audit_retained_portably",
  `Projection-Diagnostic-Rule` =
    "missing_or_nonboolean_is_computationally_inadmissible",
  `Loading-Scale-Metadata-Rule` = paste0(
    "preprocessing_and_architecture_names_order_and_values_must_match_",
    "before_optimization"),
  `Q1-Riesz-Rule` = paste0(
    "target_specific_training_only_validation_on_outer_training_",
    "respondents"),
  `Q1-Riesz-Grid` = "0,Ntrain^-1,Ntrain^-3/4,Ntrain^-1/2",
  `Q1-Riesz-Tie-Rule` = paste0(
    "largest_ridge_when_absolute_risk_gap_le_1e-12_times_max_1_",
    "abs_minimum"),
  `Q1-Riesz-Preconditioning` = paste0(
    "structural_Gram_whitening_plus_diagonal_congruence_",
    "equilibration_of_full_penalized_system"),
  `Q1-Riesz-Rank-Assessment` =
    "after_diagonal_congruence_equilibration",
  `Q1-Zero-Ridge-Singular-Rule` =
    "retain_frozen_cutoff_numerical_pseudoinverse_representative",
  `Q1-Ill-Conditioned-Positive-Ridge-Rule` =
    "withhold_never_truncate",
  `Q1-Riesz-Fast-Status` = "diagnostic_when_outer_nuisance_is_held_fixed",
  `Q1-Riesz-Formal-Requirement` = "signed_inner_nuisance_refit_evidence",
  `Whole-Repository-Cleanliness` = "required_for_outcome_commands",
  `Untracked-File-Rule` = "reject_all_visible_untracked_files",
  `Loaded-Package-R-Coverage` =
    "every_loaded_R_file_exactly_hashed_and_tracked",
  `RNG-Kind` = "L'Ecuyer-CMRG,Inversion,Rejection",
  `Runtime-Wrapper` = "applications/bin/Rscript45_exact_hash",
  `Runtime-Binding` = paste0(
    "R_executable_sha256_R_version_platform_package_versions_",
    "native_torch_version_BLAS_and_single_thread_state"))
assert(all(names(exact) %in% names(contract)) &&
         identical(unname(contract[names(exact)]), unname(exact)),
       "A required prospective v2.8.1 fit rule changed")
assert(grepl("v21_and_v27_results_were_seen_and_v28_aborted_before_a_fit_checkpoint",
             contract[["Historical-Disclosure"]], fixed = TRUE) &&
         identical(contract[["Prospective-Boundary"]],
                   "no_v281_application_outcome_existed_when_this_protocol_was_written") &&
         identical(contract[["Prior-Results"]],
                   "v27_diagnostic_only_and_v28_produced_no_fit_gate_or_substantive_result"),
       "The stopped v2.8 history or v2.8.1 prospective boundary is not explicit")
assert(identical(contract[["Inferior-Start-Rule"]], paste0(
  "descriptive_only_and_excluded_from_target_stability_when_outside_",
  "near_optimum_cutoff")) &&
  identical(contract[["Lower-Inadmissible-Rule"]], paste0(
    "fail_closed_if_an_inadmissible_start_has_lower_objective_than_",
    "the_selected_eligible_start")),
"The multiple-start admissibility contract changed")
assert(identical(contract[["Gradient-Contract"]], paste0(
  "deterministic_blockwise_autodiff_vs_central_finite_difference_",
  "step_ladder")) &&
  identical(contract[["Gradient-Pass-Rule"]], paste0(
    "agreement_at_two_adjacent_prespecified_steps_for_every_",
    "applicable_block")) &&
  identical(contract[["Gradient-Steps"]], paste(
    c("0.001", "0.0003", "0.0001", "0.00003", "0.00001",
      "0.000003", "0.000001", "0.0000003", "0.0000001"),
    collapse = ",")),
"The deterministic adjacent-step gradient audit changed")

fixed_thresholds <- c(
  `Riesz-Defect-Cutoff` = "0.20",
  `Ridge-Neighborhood-Cutoff-SE` = "0.20",
  `Derivative-Cutoff` = "0.0001",
  `Null-Space-Cutoff` = "0.000001",
  `Nested-Objective-Relative-Cutoff` = "0.000001",
  `Near-Optimum-Target-Selected-Movement-Cutoff-SE` = "0.10",
  `Loading-Invariance-Cutoff` = "0.00000001")
assert(identical(unname(contract[names(fixed_thresholds)]),
                 unname(fixed_thresholds)) &&
         identical(contract[["Threshold-Change"]], "none"),
       "A fixed v2.8.1 diagnostic threshold changed")
assert(identical(contract[["Formal-Release"]], paste0(
  "withheld_without_complete_signed_fit_inference_and_target_",
  "stability_evidence")) &&
  identical(contract[["Manifest-Finalization"]],
            "completed_before_first_outcome_command"),
"The release or manifest-finalization rule changed")

inputs <- read_registry("input-manifest.csv")
assert(nrow(inputs) == 7L && !anyDuplicated(inputs$input_id) &&
         all(tolower(inputs$read_only) == "true"),
       "The input manifest is incomplete or not read-only")
initializers <- inputs$input_id[tolower(inputs$may_initialize) == "true"]
assert(identical(initializers, c("v21_primary_full", "v21_primary_nested")),
       "Only the two selected historical q=1 containers may initialize v2.8.1")

states <- read_registry("state-registry.csv")
assert(identical(states$scope, c("full", paste0("outer_", 1:5))) &&
         identical(states$n_respondents, c(2000L, rep(1600L, 5L))) &&
         all(states$mean_family == "relu") && all(states$weight_decay == 0.1),
       "The six frozen q=1 initializer specifications changed")

seeds <- read_registry("seed-registry.csv")
assert(identical(seeds$value[seeds$component == "perturbation_scale"],
                 "0.02") &&
         identical(seeds$rule[seeds$component == "perturbation_scale"],
                   "deterministic_optimizer_coordinate_scale"),
       "The v2.8.1 perturbation scale or coordinate system changed")
assert(identical(seeds$value[seeds$component == "q2_second_column_rule"],
                 "deterministic_trigonometric") &&
         identical(seeds$rule[seeds$component == "q2_second_column_rule"],
                   "no_rng_seed_consumed"),
       "The q=2 initializer incorrectly claims an unused RNG seed")
stages <- read_registry("stage-registry.csv")
assert("required_for_prospective_run" %in% names(stages) &&
         !any(grepl("rapid", names(stages), fixed = TRUE)) &&
         identical(stages$stage_index, seq_len(nrow(stages))) &&
         all(c("q1_assemble", "q0_assemble", "q2_outer_2",
               "q2_timing_decision", "q2_assemble", "q1_gh45",
               "q2_gh15") %in% stages$stage_id),
       "The prospective q=0,1,2 stage registry is incomplete")
dependency_tokens <- lapply(stages$depends_on, function(x) {
  if (identical(x, "none")) character() else strsplit(x, "+", fixed = TRUE)[[1L]]
})
assert(all(vapply(seq_along(dependency_tokens), function(i) {
  earlier <- if (i == 1L) character() else stages$stage_id[seq_len(i - 1L)]
  all(dependency_tokens[[i]] %in% earlier)
}, logical(1L))),
"Every dependency must name an earlier concrete stage")
expected_dependencies <- c(
  q1_assemble = paste0("q1_full+q1_outer_1+q1_outer_2+q1_outer_3+",
                       "q1_outer_4+q1_outer_5"),
  q0_assemble = paste0("q0_full+q0_outer_1+q0_outer_2+q0_outer_3+",
                       "q0_outer_4+q0_outer_5"),
  q2_timing_decision = "q2_outer_2+q0_assemble+q1_gh45",
  q2_assemble = paste0("q2_full+q2_outer_1+q2_outer_2+q2_outer_3+",
                       "q2_outer_4+q2_outer_5"),
  analysis = "q0_assemble+q1_gh45+q2_timing_decision")
assert(identical(unname(stages$depends_on[
                   match(names(expected_dependencies), stages$stage_id)]),
                 unname(expected_dependencies)),
       paste("The assembly, timing, or analysis prerequisites no longer",
             "match the runner's enforced dependency graph"))

carry <- read_registry("carry-forward-manifest.csv")
assert(nrow(carry) == 9L && all(carry$mode == "byte_identical"),
       "The v2.5 scientific registries are not exactly carried forward")
for (i in seq_len(nrow(carry))) {
  check_hash(file.path(repo_root, carry$relative_path[[i]]),
             carry$sha256[[i]], paste("carried registry", carry$registry[[i]]))
}
prior <- read_registry("prior-v26-manifest.csv")
assert(nrow(prior) == 13L && !anyDuplicated(prior$relative_path),
       "The sealed v2.6 lineage manifest is incomplete")
for (i in seq_len(nrow(prior))) {
  check_hash(file.path(repo_root, prior$relative_path[[i]]),
             prior$sha256[[i]], paste("sealed lineage", prior$relative_path[[i]]))
}

package_r <- sort(file.path(
  "R", list.files(file.path(repo_root, "R"), pattern = "[.]R$",
                  recursive = TRUE)))
required_implementation <- c(
  ".gitignore", "DESCRIPTION", "NAMESPACE", "applications/bin/Rscript45",
  package_r, "applications/br2017/R/br_v281_contract.R",
  "applications/br2017/R/09_run_fast_v281.R",
  "applications/br2017/R/test_br_v281_contract.R",
  "provenance/br2017-v28-first-fit-stop-2026-09-07.txt",
  "tests/testthat/test-calibration-v24-targets.R",
  "tests/testthat/test-mixed-optimization-v28.R",
  "tests/testthat/test-paperps-inference-v22.R",
  "tests/testthat/test-paperps-inference-v28.R")
implementation <- read_registry("implementation-manifest.csv")
assert(identical(implementation$relative_path, required_implementation) &&
         !anyDuplicated(implementation$relative_path),
       "The transitive v2.8.1 implementation manifest is incomplete")
for (i in seq_len(nrow(implementation))) {
  path <- file.path(repo_root, implementation$relative_path[[i]])
  check_hash(path, implementation$sha256[[i]],
             paste("implementation", implementation$relative_path[[i]]))
  if (grepl("[.]R$", path)) parse(path)
}

checksum_lines <- readLines(file.path(protocol_dir, "CHECKSUMS.sha256"),
                            warn = FALSE)
parts <- strsplit(checksum_lines, "  ", fixed = TRUE)
assert(length(checksum_lines) > 0L && all(lengths(parts) == 2L),
       "Malformed CHECKSUMS.sha256")
registered <- vapply(parts, `[[`, character(1L), 2L)
expected_hash <- vapply(parts, `[[`, character(1L), 1L)
assert(setequal(registered, setdiff(expected_files, "CHECKSUMS.sha256")),
       "Protocol checksums do not cover every other protocol file exactly once")
for (i in seq_along(registered)) {
  check_hash(file.path(protocol_dir, registered[[i]]), expected_hash[[i]],
             paste("protocol", registered[[i]]))
}

source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
if (nzchar(source_root)) {
  required <- inputs[tolower(inputs$required) == "true", , drop = FALSE]
  for (i in seq_len(nrow(required))) {
    check_hash(file.path(source_root,
                         required$relative_to_ConjointStructural[[i]]),
               required$sha256[[i]],
               paste("external input", required$input_id[[i]]))
  }
}

assert(!length(pending),
       "The finalized v2.8.1 protocol still contains pending hashes")
cat(sprintf("Ballard--Rosa v2.8.1 protocol checks passed: %d\n", checks))
