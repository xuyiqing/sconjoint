#!/usr/bin/env Rscript

## Outcome-free static checks for the Graham--Svolik v2.8.2 target protocol.

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
  "README.md", "contrast-registry.csv", "core-family-registry.csv",
  "core-primitive-registry.csv", "core-target-registry.csv",
  "frozen-registry-manifest.csv", "group-registry.csv",
  "implementation-manifest.csv", "seed-registry.csv", "stage-registry.csv",
  "target-stage-contract.dcf", "target-type-registry.csv", "test-protocol.R",
  "CHECKSUMS.sha256")
assert(setequal(list.files(protocol_dir), expected_files),
       "The target protocol directory has unregistered files")

contract <- unlist(read.dcf(file.path(protocol_dir, "target-stage-contract.dcf"),
                            all = TRUE)[1L, , drop = TRUE], use.names = TRUE)
exact <- c(
  `Protocol-Id` = "gs2020-paperps-targets-2026-09-07-v2.8.2-v1",
  `Parent-Protocol-Id` =
    "gs2020-paperps-prospective-2026-09-07-v2.8.2-v1",
  `Historical-Target-Protocol` = "gs2020-targeted-q1-2026-09-03",
  `Historical-Target-Role` = "diagnostic_comparator_only",
  `Historical-Disclosure` = paste0(
    "earlier_GS_q1_targets_and_BR_v281_results_were_seen_so_this_core_",
    "is_post_diagnostic_but_prospective_with_respect_to_its_own_",
    "GS_v282_outcomes"),
  `Prospective-Boundary` =
    "no_GS_v282_target_outcome_existed_when_this_protocol_was_written",
  `Upstream-Fit` = "exact_hash_bound_v282_q1_assembled_checkpoint",
  `Upstream-Preflight` =
    "exact_hash_bound_v282_pre_estimation_verified_input_checkpoint",
  `Fresh-Upstream-Requirement` =
    "v282_q1_assembly_and_preflight_generated_by_exact_parent_protocol",
  `Prior-Target-Checkpoint-Reuse` =
    "prohibited_including_v281_partial_or_complete_checkpoints",
  `Historical-Target-Estimate-Reuse` = "prohibited",
  `Checkpoint-Pair-Recovery` = paste0(
    "validate_installed_envelope_and_binding_then_recreate_only_",
    "missing_sha_sidecar"),
  `Core-Target-Count` = "5", `Core-Family-Count` = "2",
  `Core-Target-Substance` = "coparty_and_vote_restriction",
  `Q2-Target-Selection-Role` = "none",
  `Riesz-Defect-Cutoff` = "0.20",
  `Riesz-Correction-Relative-Cutoff` = "0.20",
  `Ridge-Movement-Cutoff` = "0.20",
  `Common-Family-Multiplier-Draws` = "9999",
  `Threshold-Change` = "none",
  `Formal-Release` =
    "withheld_post_diagnostic_same_sample_and_pending_complete_gates",
  `Manifest-Finalization` = "completed_before_first_outcome_command")
assert(all(names(exact) %in% names(contract)) &&
         identical(unname(contract[names(exact)]), unname(exact)),
       "A required target-protocol rule changed")

targets <- read_registry("core-target-registry.csv")
expected_ids <- c(
  "mean_coparty_utility", "choice_coparty_vote_restrictor",
  "share_oppose_vote_restriction",
  "compensating_coparty_vote_restriction",
  "heterogeneity_vote_restriction")
assert(identical(targets$target_index, 1:5) &&
         identical(targets$target_id, expected_ids) &&
         identical(targets$multiplicity_family,
                   c(rep("context_preferences", 2),
                     rep("heterogeneity_core", 3))) &&
         all(tolower(targets$post_diagnostic_manuscript_core) == "true"),
       "The five-target core or its two-family assignment changed")

coef_names <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote", "diff_dem_code_v_affair",
  "diff_dem_code_v_tax", "diff_sex_Female", "diff_race_Asian",
  "diff_race_Black", "diff_race_Hispanic", "diff_pro_Farmer",
  "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")
contrasts <- read_registry("contrast-registry.csv")
assert(identical(names(contrasts)[-1L], coef_names) &&
         identical(contrasts$contrast_id,
                   c("coparty", "vote_restriction",
                     "oppose_vote_restriction",
                     "coparty_plus_vote_restriction")),
       "The registered Graham--Svolik contrasts changed")
M <- as.matrix(contrasts[, coef_names, drop = FALSE])
party <- match("diff_respParty", coef_names)
vote <- match("diff_dem_code_u_limitVote", coef_names)
expected_M <- matrix(0, 4, length(coef_names))
expected_M[1, party] <- 1; expected_M[2, vote] <- 1
expected_M[3, vote] <- -1; expected_M[4, c(party, vote)] <- 1
assert(isTRUE(all.equal(unname(M), expected_M, check.attributes = FALSE)),
       "A contrast sign or coefficient coordinate changed")

families <- read_registry("core-family-registry.csv")
primitives <- read_registry("core-primitive-registry.csv")
types <- read_registry("target-type-registry.csv")
groups <- read_registry("group-registry.csv")
assert(identical(families$family_id,
                 c("context_preferences", "heterogeneity_core")) &&
         nrow(primitives) == 6L &&
         sum(primitives$target_id == "heterogeneity_vote_restriction") == 2L &&
         setequal(types$target_type, unique(targets$target_type)) &&
         all(types$status == "supported") &&
         identical(groups$group_id, "overall"),
       "The family, primitive, type, or group registry changed")

seeds <- read_registry("seed-registry.csv")
assert(identical(seeds$family_id, families$family_id) &&
         identical(seeds$dml_seed, c(202609410L, 202609420L)) &&
         identical(seeds$multiplier_seed, c(202619410L, 202619420L)) &&
         all(seeds$multiplier_draws == 9999L),
       "The target-family seeds changed")

stages <- read_registry("stage-registry.csv")
assert(identical(stages$stage_index, 1:5) &&
         identical(stages$stage_id,
                   c("binding", "basis", "context_preferences",
                     "heterogeneity_core", "export")),
       "The target checkpoint order changed")

frozen <- read_registry("frozen-registry-manifest.csv")
assert(!anyDuplicated(frozen$dependency_id) &&
         all(frozen$role != "") &&
         all(!grepl("^/|(^|/)\\.\\.(/|$)", frozen$relative_path)),
       "The frozen target dependency manifest is malformed")
for (i in seq_len(nrow(frozen))) {
  check_hash(file.path(repo_root, frozen$relative_path[[i]]),
             frozen$sha256[[i]], paste("frozen", frozen$dependency_id[[i]]))
}

package_r <- sort(file.path(
  "R", list.files(file.path(repo_root, "R"), pattern = "[.]R$", recursive = TRUE)))
required_implementation <- c(
  ".gitignore", "DESCRIPTION", "NAMESPACE", "applications/bin/Rscript45",
  package_r, "applications/R/targeted_q1_inference.R",
  "applications/gs2020/R/gs_v282_contract.R",
  "applications/gs2020/R/05_run_fast_v282.R",
  "applications/gs2020/R/test_gs_v282_contract.R",
  "applications/gs2020/R/gs_v282_targets_contract.R",
  "applications/gs2020/R/06_run_targets_v282.R",
  "applications/gs2020/R/test_gs_v282_targets_contract.R")
implementation <- read_registry("implementation-manifest.csv")
assert(identical(implementation$relative_path, required_implementation) &&
         !anyDuplicated(implementation$relative_path),
       "The target implementation manifest is incomplete")
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
       "Checksums do not cover every other target-protocol file exactly once")
for (i in seq_along(registered)) {
  check_hash(file.path(protocol_dir, registered[[i]]), expected_hash[[i]],
             paste("protocol", registered[[i]]))
}

assert(!length(pending),
       "The finalized target protocol still contains pending hashes")
cat(sprintf("Graham--Svolik v2.8.2 target protocol checks passed: %d\n", checks))
