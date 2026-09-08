#!/usr/bin/env Rscript

## Outcome-free static checks for the Ballard--Rosa v2.8 target protocol.
## PENDING_FINALIZATION hashes are allowed only during construction. The
## production validator rejects them before any target outcome command.

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
read_csv <- function(name) {
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
  "README.md", "core-family-registry.csv", "core-primitive-registry.csv",
  "core-target-registry.csv", "frozen-registry-manifest.csv",
  "implementation-manifest.csv", "seed-registry.csv", "stage-registry.csv",
  "target-stage-contract.dcf", "target-type-registry.csv", "test-protocol.R",
  "CHECKSUMS.sha256")
assert(setequal(list.files(protocol_dir), expected_files),
       "The v2.8 target protocol directory has unregistered files")

contract <- unlist(read.dcf(file.path(protocol_dir,
                                      "target-stage-contract.dcf"),
                            all = TRUE)[1L, , drop = TRUE], use.names = TRUE)
exact <- c(
  `Protocol-Id` = "br2017-paperps-targets-2026-09-07-v2.8-v1",
  `Parent-Protocol-Id` = "br2017-paperps-prospective-2026-09-07-v2.8-v1",
  `Prior-Diagnostic-Target-Protocol` = paste0(
    "br2017-paperps-targets-2026-09-07-v2.7-v1"),
  `Prospective-Boundary` = paste0(
    "no_v28_target_outcome_existed_when_this_protocol_was_written"),
  `Target-Rank` = "1",
  `Core-Target-Selection` = paste0(
    "exactly_five_outcome_informed_post_diagnostic_manuscript_targets_",
    "in_core-target-registry.csv"),
  `Core-Target-Count` = "5",
  `Core-Family-Count` = "2",
  `Core-Families` = "context_preferences,heterogeneity_core",
  `Core-Heterogeneity-Primitive-Count` = "4",
  `Secondary-Target-Count` = "72",
  `Q2-Target-Selection-Role` = "none",
  `Riesz-Rule` = "target_specific_training_only_validation",
  `Riesz-Grid` = "0,Ntrain^-1,Ntrain^-3/4,Ntrain^-1/2",
  `Riesz-Validation-Fraction` = "0.20",
  `Riesz-Tie-Rule` = paste0(
    "largest_ridge_when_absolute_risk_gap_le_1e-12_times_max_1_",
    "abs_minimum"),
  `Riesz-Numerical-Rank-Rule` = "dimension_epsilon",
  `Riesz-Preconditioning` = paste0(
    "structural_Gram_whitening_plus_diagonal_congruence_",
    "equilibration_of_full_penalized_system"),
  `Riesz-Rank-Assessment` = "after_diagonal_congruence_equilibration",
  `Riesz-Zero-Ridge-Singular-Rule` =
    "retain_frozen_cutoff_numerical_pseudoinverse_representative",
  `Riesz-Ill-Conditioned-Positive-Ridge-Rule` =
    "withhold_never_truncate",
  `Fast-Nuisance-Validation-Status` = "diagnostic_only_shared_outer_nuisance",
  `Target-Execution` = "one_target_specific_DML_call_per_registered_target",
  `Target-Seed-Rule` = "family_base_seed_plus_original_target_index",
  `Family-Assembly` = paste0(
    "transform_target_influences_then_apply_one_common_multiplier_bank"),
  `Crossfit-HZ-Aggregation` =
    "global_crossfit_second_moment_minus_squared_global_crossfit_mean",
  `Crossfit-HZ-Riesz-Gradient-Center` = "outer_training_only",
  `Crossfit-HZ-Support-Moments` =
    "required_global_assembly_inputs",
  `Formal-Nuisance-Validation-Requirement` = "signed_inner_nuisance_refits",
  `Familywise-Level` = "0.95",
  `Choice-GH-Nodes` = "31",
  `Directional-Variance-Floor` = "0.000001",
  `Subgroup-Probability-Margin` = "0.05",
  `Directional-Heterogeneity-Total-Margin` = "0.0001",
  `Multiplier-Block-Size` = "250",
  `Formal-Release` =
    "withheld_post_diagnostic_same_sample_and_pending_complete_gates",
  `Confirmatory-Claim-Eligibility` = paste0(
    "independent_data_or_genuinely_pre_result_registry_or_",
    "valid_selective_inference"),
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
       "A required prospective v2.8 target rule changed")
assert(grepl("v21_and_v27_application_and_target_results_were_seen",
             contract[["Historical-Disclosure"]], fixed = TRUE) &&
         identical(contract[["Historical-Target-Estimate-Reuse"]],
                   "prohibited"),
       "The historical-result disclosure or reuse prohibition changed")

thresholds <- c(
  `Riesz-Defect-Cutoff` = "0.20",
  `Riesz-Correction-Relative-Cutoff` = "0.20",
  `Ridge-Movement-Cutoff` = "0.20",
  `Derivative-Cutoff` = "0.0001",
  `Null-Space-Cutoff` = "0.000001",
  `Fit-Blockwise-KKT-Cutoff` = "0.00001",
  `Fit-Near-Optimum-Relative-Cutoff` = "0.000001",
  `Target-Near-Optimum-Selected-Movement-Cutoff-SE` = "0.10",
  `Loading-Invariance-Cutoff` = "0.00000001",
  `Loading-Rank-Tolerance` = "0.00000001")
assert(identical(unname(contract[names(thresholds)]), unname(thresholds)) &&
         identical(contract[["Threshold-Change"]], "none"),
       "A fixed v2.8 fit or inference threshold changed")
assert(identical(contract[["Riesz-Preconditioning-Estimand"]], "unchanged") &&
         identical(contract[["Formal-Release-Rule"]], paste0(
           "withhold_if_any_required_fit_inference_or_target_stability_",
           "evidence_is_missing_or_fails")) &&
         identical(contract[["Current-Fast-Evidence"]], paste0(
           "descriptive_and_conditional_only_even_after_numerical_gates_on_",
           "same_data_because_core_selection_was_outcome_informed")),
       "The numerical-equivalence or missing-evidence release rule changed")

targets <- read_csv("core-target-registry.csv")
families <- read_csv("core-family-registry.csv")
primitives <- read_csv("core-primitive-registry.csv")
types <- read_csv("target-type-registry.csv")
expected_target_ids <- c(
  "mean_progressivity_rank_slope", "choice_progressive_vs_flat",
  "share_progressivity_rank_slope", "compensating_progressive_vs_flat",
  "heterogeneity_progressivity_rank_slope")
assert(nrow(targets) == 5L && !anyDuplicated(targets$target_id) &&
         identical(targets$target_id, expected_target_ids) &&
         all(tolower(targets$post_diagnostic_manuscript_core) == "true"),
       "The five-target core membership or order changed")
assert(nrow(families) == 2L &&
         identical(families$family_id,
                   c("context_preferences", "heterogeneity_core")) &&
         all(families$multiplier_draws == 9999L) &&
         all(families$release_rule ==
               "same_sample_formal_release_prohibited") &&
         identical(families$reporting_role,
                   c("post_diagnostic_context",
                     "post_diagnostic_heterogeneity_core")) &&
         setequal(unique(targets$multiplicity_family), families$family_id),
       "The two v2.8 core multiplicity families changed")
heterogeneity_primitives <- primitives[
  primitives$family_id == "heterogeneity_core", , drop = FALSE]
assert(nrow(heterogeneity_primitives) == 4L &&
         !anyDuplicated(heterogeneity_primitives$primitive_id) &&
         setequal(heterogeneity_primitives$target_id,
                  expected_target_ids[3:5]),
       "The four unique heterogeneity primitive coordinates changed")
mapped_status <- types$status[match(targets$target_type, types$target_type)]
assert(!anyNA(mapped_status) && all(mapped_status == "supported"),
       "A core target type is unsupported or unmapped")

secondary <- utils::read.csv(file.path(
  repo_root, "applications", "br2017", "protocol", "v25",
  "target-registry.csv"), stringsAsFactors = FALSE, check.names = FALSE,
  na.strings = character())
assert(nrow(secondary) == 72L && !anyDuplicated(secondary$target_id) &&
         identical(contract[["Secondary-Target-Role"]], paste0(
           "plug_in_and_diagnostic_only_not_manuscript_core_multiplicity_",
           "members")),
       "The complete 72-target secondary registry or its role changed")

seeds <- read_csv("seed-registry.csv")
assert(identical(seeds$family_id, families$family_id) &&
         all(seeds$multiplier_draws == 9999L) &&
         !anyDuplicated(c(seeds$dml_seed, seeds$multiplier_seed)),
       "The two-family seed or multiplier registry changed")
stages <- read_csv("stage-registry.csv")
assert(identical(stages$stage_index, seq_len(nrow(stages))) &&
         identical(stages$stage_id,
                   c("binding", "basis", "context_preferences",
                     "heterogeneity_core", "export")) &&
         identical(stages$reporting_role,
                   c("integrity", "integrity", "post_diagnostic_context",
                     "post_diagnostic_heterogeneity_core",
                     "post_diagnostic_export")) &&
         identical(tolower(stages$outcome_generating),
                   c("false", "false", "true", "true", "false")),
       "The v2.8 core target stage sequence changed")

frozen <- read_csv("frozen-registry-manifest.csv")
assert(!anyDuplicated(frozen$dependency_id) &&
         all(c("target_registry", "contrast_registry", "group_registry",
               "family_registry", "gate_registry", "parent_protocol_checksum",
               "parent_contract_helper", "parent_runner", "target_helper",
               "ame_helper") %in% frozen$dependency_id),
       "The frozen target dependency manifest is incomplete")
for (i in seq_len(nrow(frozen))) {
  check_hash(file.path(repo_root, frozen$relative_path[[i]]),
             frozen$sha256[[i]],
             paste("frozen dependency", frozen$dependency_id[[i]]))
}

package_r <- sort(file.path(
  "R", list.files(file.path(repo_root, "R"), pattern = "[.]R$",
                  recursive = TRUE)))
required_implementation <- c(
  ".gitignore", "DESCRIPTION", "NAMESPACE", "applications/bin/Rscript45",
  package_r, "applications/R/targeted_q1_inference.R",
  "applications/br2017/R/br_v28_contract.R",
  "applications/br2017/R/br_v28_targets_contract.R",
  "applications/br2017/R/08_run_targets_v28.R",
  "applications/br2017/R/test_br_v28_targets_contract.R",
  "tests/testthat/test-calibration-v24-targets.R",
  "tests/testthat/test-mixed-optimization-v28.R",
  "tests/testthat/test-paperps-inference-v22.R",
  "tests/testthat/test-paperps-inference-v28.R")
implementation <- read_csv("implementation-manifest.csv")
assert(identical(implementation$relative_path, required_implementation) &&
         !anyDuplicated(implementation$relative_path),
       "The transitive target implementation manifest is incomplete")
for (i in seq_len(nrow(implementation))) {
  path <- file.path(repo_root, implementation$relative_path[[i]])
  check_hash(path, implementation$sha256[[i]],
             paste("target implementation", implementation$relative_path[[i]]))
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
       "Target checksums do not cover every other protocol file exactly once")
for (i in seq_along(registered)) {
  check_hash(file.path(protocol_dir, registered[[i]]), expected_hash[[i]],
             paste("target protocol", registered[[i]]))
}

if (length(pending)) {
  cat(sprintf(paste0(
    "Ballard--Rosa v2.8 target substantive checks passed: %d; ",
    "%d hashes await finalization before any outcome command.\n"),
    checks, length(pending)))
} else {
  cat(sprintf("Ballard--Rosa v2.8 target protocol checks passed: %d\n",
              checks))
}
