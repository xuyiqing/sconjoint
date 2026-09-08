#!/usr/bin/env Rscript

## Outcome-free static checks for the Graham--Svolik v2.8.2 fit protocol.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
protocol_dir <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
} else normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(protocol_dir, "..", "..", "..", ".."),
                           mustWork = TRUE)

checks <- 0L
pending <- character()
assert <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}
read_registry <- function(name) utils::read.csv(
  file.path(protocol_dir, name), stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = character())
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
           identical(sha256(path), expected), paste("Hash mismatch:", label))
}

expected_files <- c(
  "README.md", "application-contract.dcf", "implementation-manifest.csv",
  "input-manifest.csv", "seed-registry.csv", "stage-registry.csv",
  "state-registry.csv", "test-protocol.R", "CHECKSUMS.sha256")
assert(setequal(list.files(protocol_dir), expected_files),
       "The fit protocol directory has unregistered files")

contract <- unlist(read.dcf(file.path(protocol_dir, "application-contract.dcf"),
                            all = TRUE)[1L, , drop = TRUE], use.names = TRUE)
exact <- c(
  `Protocol-Id` = "gs2020-paperps-prospective-2026-09-07-v2.8.2-v1",
  `Prior-GS-V282-Outcome` = "none",
  `Prior-GS-V281-Outcome` =
    "preflight_plus_q1_outer_1_and_outer_2_checkpoints_only",
  `V281-Failure-Mode` = paste0(
    "torch_shared_storage_mutated_caller_warm_tensors_before_later_",
    "candidate_starts"),
  `V281-Discovery-Point` =
    "q1_full_and_outer_3_execution_exposed_the_application_adapter_bug",
  `V281-Substantive-Inspection` =
    "no_printed_substantive_results_inspected_or_reused",
  `V281-Artifact-Reuse` = "prohibited",
  `Prior-GS-V28-Outcome` = "none",
  `Historical-Fit-Role` = "matching_outer_fold_q1_states_are_initializers_only",
  `Historical-Full-Fit` = "unavailable_not_invented",
  `Full-Initializer` = paste0(
    "deterministic_outer_1_state_affine_input_and_output_scale_remap_",
    "followed_by_full_reoptimization"),
  `Heldout-Leakage-Rule` =
    "each_outer_scope_uses_only_its_matching_outer_training_state",
  `Historical-Inner-Tuning-Audit` = paste0(
    "exact_outer_complements_respondent_clustered_inner_folds_training_only_",
    "preprocessing_validation_id_bound_scores_recomputed_selection_and_",
    "refit_specification"),
  `Historical-Inner-Tuning-Evidence-Limit` =
    "retained_metadata_internal_consistency_only_cv_networks_were_not_retained",
  `Prospective-Boundary` =
    "no_v282_application_outcome_existed_when_this_protocol_was_written",
  `Historical-Estimate-Reuse` = "prohibited",
  `Historical-Gate-Reuse` = "prohibited",
  `Historical-Target-Reuse` = "prohibited",
  `Checkpoint-Pair-Recovery` = paste0(
    "validate_installed_envelope_and_binding_then_recreate_only_",
    "missing_sha_sidecar"),
  `Amendment-Scope` = "caller_supplied_warm_tensor_ownership_isolation",
  `Inherited-Numerical-Amendment` = paste0(
    "v281_one_dimensional_parameter_shape_preservation_in_gradient_",
    "audit"),
  `Warm-Tensor-Ownership` = paste0(
    "clone_and_validate_every_caller_supplied_tensor_at_fit_one_start_",
    "entry_before_any_optimizer_call"),
  `Package-Warm-Tensor-Ownership` = paste0(
    "detach_and_clone_each_used_exact_warm_state_tensor_before_",
    "load_state_dict"),
  `Warm-Tensor-Fix-Scope` =
    "package_ownership_boundary_plus_application_level_defense_in_depth",
  `Primary-Input-SHA256` =
    "debd4a077330fb47b1c073cbc8a17e701d1c3f497fb10e188951b26110516878",
  `Primary-Sample` = "1605_respondents_20657_tasks_1_to_13_tasks_each",
  `Ranks` = "0,1,2", `Primary-Rank` = "1",
  `Fresh-Optimization-Rule` =
    "all_trainable_parameters_reoptimized_from_initializer",
  `Q1-Initializer-Alias-Rule` = paste0(
    "fit_preprocessing_identical_to_portable_state_preprocessing_and_names_",
    "with_architecture_scale_agreement_at_1e-12"),
  `Q1-Remap-Baseline-Shift` = paste0(
    "unbounded_corrected_relu_deviation_g_of_a_minus_g_of_zero_never_",
    "clipped_beta_difference"),
  `Q1-Remap-Validation-Scope` = "every_row_in_the_fresh_fitting_scope",
  `Analysis-Conditional-Dependency` =
    "require_q2_gh15_checkpoint_when_q2_timing_decision_is_continue",
  `Q2-Timing-Cutoff-Seconds` = "50400",
  `Optimizer` = "float64_full_batch_LBFGS_strong_wolfe",
  `Blockwise-KKT-Cutoff` = "0.00001",
  `Near-Optimum-Relative-Cutoff` = "0.000001",
  `Threshold-Change` = "none",
  `Manifest-Finalization` = "completed_before_first_outcome_command")
assert(all(names(exact) %in% names(contract)) &&
         identical(unname(contract[names(exact)]), unname(exact)),
       "A required fit-protocol rule changed")

fit_contract_text <- paste(readLines(file.path(
  repo_root, "applications", "gs2020", "R", "gs_v282_contract.R"),
  warn = FALSE), collapse = "\n")
assert(all(vapply(c(
  ".gs_v282_assert_historical_inner_tuning(prepared, nested, fold_index)",
  "!identical(fit$preprocessing, state$preprocessing)",
  ".gs_v282_unbounded_relu_deviation(before_net, mapped_new_zero)",
  "before_net, fitting_internal_old)",
  "target_net, repaired$internal)"
), grepl, logical(1L), x = fit_contract_text, fixed = TRUE)),
"The implementation no longer enforces the disclosed tuning/remap audit")
fit_contract_env <- new.env(parent = globalenv())
sys.source(file.path(repo_root, "applications", "gs2020", "R",
                     "gs_v282_contract.R"), envir = fit_contract_env)
fit_one_start_text <- paste(
  deparse(body(fit_contract_env$.gs_v282_fit_one_start)), collapse = "\n")
clone_helper_text <- paste(
  deparse(body(fit_contract_env$.gs_v282_clone_owned_warm)), collapse = "\n")
clone_at <- regexpr(
  "current_warm <- .gs_v282_clone_owned_warm(warm)", fit_one_start_text,
  fixed = TRUE)[[1L]]
optimizer_at <- regexpr(
  ".gs_v282_fit_one_stage(", fit_one_start_text, fixed = TRUE)[[1L]]
assert(clone_at > 0L && optimizer_at > clone_at &&
         grepl("owned <- lapply(warm, function(x) x$detach()$clone())",
               clone_helper_text, fixed = TRUE),
       paste("The application adapter must clone every caller warm tensor",
             "before its first optimizer call"))

inputs <- read_registry("input-manifest.csv")
assert(nrow(inputs) == 5L && !anyDuplicated(inputs$input_id) &&
         all(tolower(inputs$read_only) == "true") &&
         identical(inputs$input_id[tolower(inputs$may_initialize) == "true"],
                   "v21_primary_nested"),
       "The five read-only inputs or initializer permissions changed")

states <- read_registry("state-registry.csv")
assert(identical(states$scope, c("full", paste0("outer_", 1:5))) &&
         identical(states$source_position,
                   c("outer_1", paste0("outer_", 1:5))) &&
         identical(states$source_label,
                   c("outer_2", "outer_2", "outer_4", "outer_3",
                     "outer_1", "outer_5")) &&
         identical(states$n_respondents, rep(1284L, 6L)) &&
         identical(states$hidden, c(4L, 4L, 4L, 8L, 4L, 4L)) &&
         identical(states$initializer_role,
                   c("deterministic_full_transfer",
                     rep("matching_outer_training_initializer", 5L))),
       "The six disclosed q=1 initializer mappings changed")

seeds <- read_registry("seed-registry.csv")
assert(identical(seeds$value[seeds$component == "fit_master"], "202609176") &&
         identical(seeds$value[seeds$component == "perturbation_scale"],
                   "0.02") &&
         identical(seeds$value[seeds$component == "q2_second_column_rule"],
                   "deterministic_trigonometric"),
       "A fit seed or deterministic perturbation rule changed")

stages <- read_registry("stage-registry.csv")
assert(identical(stages$stage_index, seq_len(nrow(stages))) &&
         "conditional_depends_on" %in% names(stages) &&
         all(c("q1_outer_2", "q1_assemble", "q0_assemble", "q2_outer_2",
               "q2_timing_decision", "q2_assemble", "analysis") %in%
               stages$stage_id),
       "The q=0,1,2 checkpoint graph is incomplete")
assert(identical(stages$conditional_depends_on[
                   stages$stage_id == "analysis"],
                 "q2_gh15_if_timing_decision_equals_continue") &&
         all(stages$conditional_depends_on[
           !stages$stage_id %in% c(paste0("q2_", c(
             "full", "outer_1", "outer_3", "outer_4", "outer_5")),
             "q2_assemble", "q2_gh15", "analysis")] == "none"),
       "The conditional q=2 analysis dependency is not explicit")
dependencies <- lapply(stages$depends_on, function(x) {
  if (identical(x, "none")) character() else strsplit(x, "+", fixed = TRUE)[[1L]]
})
assert(all(vapply(seq_along(dependencies), function(i) {
  earlier <- if (i == 1L) character() else stages$stage_id[seq_len(i - 1L)]
  all(dependencies[[i]] %in% earlier)
}, logical(1L))), "A stage depends on a checkpoint not created earlier")

package_r <- sort(file.path(
  "R", list.files(file.path(repo_root, "R"), pattern = "[.]R$", recursive = TRUE)))
required_implementation <- c(
  ".gitignore", "DESCRIPTION", "NAMESPACE", "applications/bin/Rscript45",
  package_r, "tests/testthat/test-mixed-optimization-v28.R",
  "applications/gs2020/R/gs_v282_contract.R",
  "applications/gs2020/R/05_run_fast_v282.R",
  "applications/gs2020/R/test_gs_v282_contract.R")
implementation <- read_registry("implementation-manifest.csv")
assert(identical(implementation$relative_path, required_implementation) &&
         !anyDuplicated(implementation$relative_path),
       "The fit implementation manifest is incomplete")
for (i in seq_len(nrow(implementation))) {
  path <- file.path(repo_root, implementation$relative_path[[i]])
  check_hash(path, implementation$sha256[[i]],
             paste("implementation", implementation$relative_path[[i]]))
  if (grepl("[.]R$", path)) parse(path)
}

checksum_lines <- readLines(file.path(protocol_dir, "CHECKSUMS.sha256"),
                            warn = FALSE)
parts <- strsplit(checksum_lines, "  ", fixed = TRUE)
assert(length(parts) > 0L && all(lengths(parts) == 2L),
       "Malformed CHECKSUMS.sha256")
registered <- vapply(parts, `[[`, character(1L), 2L)
expected_hash <- vapply(parts, `[[`, character(1L), 1L)
assert(setequal(registered, setdiff(expected_files, "CHECKSUMS.sha256")),
       "Checksums do not cover every other fit-protocol file exactly once")
for (i in seq_along(registered)) {
  check_hash(file.path(protocol_dir, registered[[i]]), expected_hash[[i]],
             paste("protocol", registered[[i]]))
}

source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
if (nzchar(source_root)) {
  for (i in seq_len(nrow(inputs))) {
    check_hash(file.path(source_root,
                         inputs$relative_to_ConjointStructural[[i]]),
               inputs$sha256[[i]], paste("external input", inputs$input_id[[i]]))
  }
}

assert(!length(pending),
       "The finalized fit protocol still contains pending hashes")
cat(sprintf("Graham--Svolik v2.8.2 fit protocol checks passed: %d\n", checks))
