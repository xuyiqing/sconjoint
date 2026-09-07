#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
protocol_dir <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

checks <- 0L
assert <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}
read_registry <- function(name) {
  read.csv(file.path(protocol_dir, name), check.names = FALSE,
           stringsAsFactors = FALSE)
}
is_true <- function(x) tolower(as.character(x)) == "true"
dcf_row <- function(name) {
  x <- read.dcf(file.path(protocol_dir, name), all = TRUE)
  assert(nrow(x) == 1L, paste(name, "must contain one DCF record"))
  x[1L, , drop = TRUE]
}

expected_files <- c(
  "README.md", "application-contract.dcf", "assessment-registry.csv",
  "contrast-registry.csv", "family-registry.csv", "group-registry.csv",
  "inference-gate-registry.csv", "input-manifest.csv", "mean-grid.csv",
  "model-specifications.csv", "restricted-mean-mask.csv",
  "review-decisions.md", "seed-registry.csv", "sensitivity-registry.csv",
  "showcase-rules.dcf", "target-registry.csv", "test-protocol.R",
  "CHECKSUMS.sha256"
)
assert(setequal(list.files(protocol_dir), expected_files),
       "The protocol directory does not contain exactly the registered files")

contract <- dcf_row("application-contract.dcf")
assert(identical(unname(contract[["Protocol-Id"]]),
                 "br2017-paperps-application-2026-09-07-v2.5-v1"),
       "Unexpected application protocol ID")
assert(identical(unname(contract[["Method-Protocol-Id"]]),
                 "paperps-inference-2026-09-07-v2.5"),
       "Unexpected method protocol ID")
assert(identical(unname(contract[["Fresh-Rerun-Outcome-Inspection"]]), "none") &&
         identical(unname(contract[["Application-Outcomes-Generated-Under-This-Protocol"]]),
                   "false"),
       "The prospective no-outcome status changed")
assert(identical(unname(contract[["Prior-V21-Warm-Starts-Permitted"]]), "false") &&
         identical(unname(contract[["Prior-V21-Selection-Or-Threshold-Use-Permitted"]]),
                   "false"),
       "Historical fits must remain comparator-only")
assert(identical(unname(contract[["Numeric-Precision"]]), "float64") &&
         identical(unname(contract[["Reference-GH-Nodes"]]), "15") &&
         identical(unname(contract[["Refined-GH-Nodes"]]), "31"),
       "The numerical contract changed")

inputs <- read_registry("input-manifest.csv")
assert(nrow(inputs) == 17L && !anyDuplicated(inputs$input_id),
       "Input manifest IDs must be unique and complete")
primary <- inputs[inputs$input_id == "prepared_primary", , drop = FALSE]
assert(nrow(primary) == 1L && identical(primary$sha256,
  "32964d49653a2cba335077f3f6170e5eb0aa7a2bb1c789343b91588f6a9cb24e"),
  "Authoritative prepared-input hash changed")
historical <- startsWith(inputs$input_id, "v21_")
assert(all(!is_true(inputs$required[historical])) &&
         all(!is_true(inputs$may_initialize_fit[historical])),
       "A v2.1 comparator was permitted to enter the fresh fit")

specs <- read_registry("model-specifications.csv")
assert(identical(specs$spec_id,
  c("flexible_q1", "restricted_q1", "flexible_q0", "flexible_q2")),
  "Unexpected specification registry or order")
assert(identical(specs$q, c(1L, 1L, 0L, 2L)), "Unexpected rank registry")
assert(identical(is_true(specs$formal_release),
  c(TRUE, FALSE, FALSE, FALSE)), "Only flexible q=1 may release formally")
assert(all(specs$precision == "float64") && all(specs$starts == 3L) &&
         all(specs$outer_folds == 5L) && all(specs$inner_folds == 2L),
       "Common fit settings changed")
assert(all(specs$polish == "LBFGS_strong_wolfe_50") &&
         all(specs$gh_reference[specs$q > 0L] == "15") &&
         all(specs$gh_refined[specs$q > 0L] == "31"),
       "Positive-rank GH or L-BFGS settings changed")

grid <- read_registry("mean-grid.csv")
grid_counts <- table(grid$grid_id)
assert(identical(as.integer(grid_counts[c("flexible_grid_v1", "restricted_grid_v1")]),
                 c(10L, 4L)), "Mean grids must contain 10 and 4 candidates")
assert(all(is_true(grid$constant_unpenalized)) &&
         all(is_true(grid$deviation_centered)) && all(is_true(grid$eligible)),
       "Mean-family correction must apply to every candidate")

mask <- read_registry("restricted-mean-mask.csv")
mask_inputs <- c("income_std", "pid7_std", "ineq_averse",
                 "work_vs_luck_hardwork", "taxes_harm_econ")
mask_outputs <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
                  "rate_175_375", "rate_375P", "revenue_score")
expected_mask <- expand.grid(input = mask_inputs, output = mask_outputs,
                             stringsAsFactors = FALSE)
assert(nrow(mask) == 35L && all(is_true(mask$allowed)) &&
         setequal(paste(mask$input, mask$output),
                  paste(expected_mask$input, expected_mask$output)),
       "Restricted mean must be the frozen 5-by-7 mask")

seeds <- read_registry("seed-registry.csv")
assert(!anyDuplicated(seeds$component) &&
         identical(seeds$value[seeds$component == "input_hash_binding"],
                   primary$sha256), "Fold/seed registry is not input-hash bound")
assert(all(c("outer_fold_salt", "inner_fold_salt", "fit_master", "dml_master",
             "multiplier_master") %in% seeds$component),
       "Required fold or random-stream seed is absent")

groups <- read_registry("group-registry.csv")
assert(identical(groups$group_id,
  c("overall", "democrat", "independent", "republican", "unclassified")),
  "Unexpected subgroup registry")

contrasts <- read_registry("contrast-registry.csv")
assert(identical(contrasts$contrast_index, seq_len(nrow(contrasts))) &&
         nrow(contrasts) == 23L && !anyDuplicated(contrasts$contrast_id),
       "Contrast registry must have 23 unique sequential entries")
coef_cols <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
               "rate_175_375", "rate_375P", "revenue_score")
contrast_vec <- function(id) {
  unlist(contrasts[contrasts$contrast_id == id, coef_cols, drop = FALSE],
         use.names = FALSE)
}
for (suffix in c("flat", "statusquo", "regressive")) {
  full <- contrast_vec(paste0("progressive_vs_", suffix))
  rate <- contrast_vec(paste0("progressive_vs_", suffix, "_rate"))
  assert(all(full[1:6] == rate[1:6]) && rate[7] == 0,
         paste("Rate-only and full policy contrasts disagree for", suffix))
}
assert(identical(unname(vapply(c("flat", "statusquo", "regressive"), function(suffix)
  contrast_vec(paste0("progressive_vs_", suffix))[7], numeric(1))), c(2, 2, 4)),
  "Frozen policy revenue-score differences must be 2, 2, and 4")

families <- read_registry("family-registry.csv")
assert(identical(families$family_index, seq_len(nrow(families))) &&
         nrow(families) == 10L && !anyDuplicated(families$family_id),
       "Multiplicity family registry must have 10 ordered families")
assert(all(families$simultaneous_confidence == 0.95) &&
         all(families$multiplier_draws == 9999L),
       "Family confidence level or multiplier count changed")

targets <- read_registry("target-registry.csv")
assert(identical(targets$target_index, seq_len(nrow(targets))) &&
         nrow(targets) == 72L && !anyDuplicated(targets$target_id),
       "Target registry must have 72 unique sequential entries")
assert(all(targets$multiplicity_family %in% families$family_id),
       "A target refers to an unknown multiplicity family")
assert(all(targets$group_id %in% groups$group_id),
       "A target refers to an unknown group")
contrast_refs <- targets$contrast_id[!is.na(targets$contrast_id) &
                                       targets$contrast_id != "none"]
benefit_refs <- targets$benefit_contrast_id[!is.na(targets$benefit_contrast_id) &
                                               targets$benefit_contrast_id != "none"]
assert(all(c(contrast_refs, benefit_refs) %in% contrasts$contrast_id),
       "A target refers to an unknown contrast")
assert(setequal(unique(targets$multiplicity_family), families$family_id),
       "Every registered family must contain at least one target")

directional <- grepl("directional_share", targets$target_type, fixed = TRUE)
compensating <- grepl("compensating_benefit_share", targets$target_type,
                      fixed = TRUE)
assert(all(grepl("directional_variance", targets$primitive_bundle[directional],
                 fixed = TRUE)),
       "Every directional share must carry its auxiliary variance primitive")
assert(all(targets$tie_rule[directional & !compensating] ==
             "strict_greater_than_zero") &&
         all(targets$tie_rule[compensating] == "greater_than_or_equal_zero"),
       "Directional-share tie rules changed")
assert(identical(sort(unique(targets$benefit_amount[compensating])), c(2L, 4L)),
       "Unexpected compensating-benefit amounts")

gates <- read_registry("inference-gate-registry.csv")
assert(identical(gates$rule_index, seq_len(nrow(gates))) && nrow(gates) == 14L &&
         !anyDuplicated(gates$rule_id), "Inference gate registry is incomplete")
gate_value <- function(id) as.numeric(gates$value[gates$rule_id == id])
assert(gate_value("regularity_z") == 1.96 && gate_value("riesz_defect") == 0.20 &&
         gate_value("ridge_movement") == 0.20 && gate_value("block_kkt") == 1e-5 &&
         gate_value("integration_target") == 0.05 &&
         gate_value("integration_if") == 0.05,
       "A core inference or fit threshold changed")

sens <- read_registry("sensitivity-registry.csv")
assert(identical(sens$sensitivity_index, seq_len(nrow(sens))) &&
         nrow(sens) == 15L && !anyDuplicated(sens$sensitivity_id) &&
         !any(is_true(sens$formal_release)), "Sensitivity registry is incomplete")

assess <- read_registry("assessment-registry.csv")
assert(identical(assess$assessment_index, seq_len(nrow(assess))) &&
         nrow(assess) == 35L && !anyDuplicated(assess$assessment_id),
       "Assessment registry must have 35 unique checks")
required_assessments <- c(
  "sequence_score_q0", "sequence_score_q2", "marginal_calibration_bins",
  "response_count_calibration", "transition_calibration",
  "response_pattern_calibration", "position_calibration",
  "task_order_calibration", "structural_ame_design_amce",
  "residual_shape_stability", "missingness_stability", "target_riesz",
  "target_ridge", "finite_standardization"
)
assert(all(required_assessments %in% assess$assessment_id),
       "The frozen Section 4 assessment battery is incomplete")

showcase <- dcf_row("showcase-rules.dcf")
assert(identical(unname(showcase[["No-Override"]]), "true"),
       "Showcase classification must not permit discretionary override")

checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
checksum_lines <- readLines(checksum_path, warn = FALSE)
parts <- strsplit(checksum_lines, "  ", fixed = TRUE)
assert(all(lengths(parts) == 2L), "Malformed checksum registry")
registered_names <- vapply(parts, `[[`, character(1L), 2L)
registered_hashes <- vapply(parts, `[[`, character(1L), 1L)
assert(setequal(registered_names, setdiff(expected_files, "CHECKSUMS.sha256")),
       "Checksum registry does not cover exactly every other protocol file")
shasum <- Sys.which("shasum")
assert(nzchar(shasum), "The shasum executable is required for this test")
actual_hashes <- vapply(registered_names, function(name) {
  line <- system2(shasum, c("-a", "256", shQuote(file.path(protocol_dir, name))),
                  stdout = TRUE, stderr = TRUE)
  assert(length(line) == 1L, paste("Could not hash", name))
  strsplit(line, "[[:space:]]+")[[1L]][1L]
}, character(1L))
assert(identical(unname(actual_hashes), registered_hashes),
       "One or more protocol files do not match CHECKSUMS.sha256")

source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
if (nzchar(source_root)) {
  required <- inputs[is_true(inputs$required), , drop = FALSE]
  for (i in seq_len(nrow(required))) {
    path <- file.path(source_root, required$relative_to_ConjointStructural[[i]])
    assert(file.exists(path), paste("Required external input is absent:", path))
    line <- system2(shasum, c("-a", "256", shQuote(path)), stdout = TRUE,
                    stderr = TRUE)
    observed <- strsplit(line, "[[:space:]]+")[[1L]][1L]
    assert(identical(observed, required$sha256[[i]]),
           paste("External input hash mismatch:", required$input_id[[i]]))
  }
}

cat(sprintf("Ballard--Rosa v2.5 protocol checks passed: %d\n", checks))
