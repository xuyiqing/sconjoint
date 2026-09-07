#!/usr/bin/env Rscript

## Outcome-free static and hash checks for the Ballard--Rosa v2.6 contract.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
protocol_dir <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
} else normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(protocol_dir, "..", "..", "..", ".."),
                           mustWork = TRUE)

checks <- 0L
assert <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}
read_registry <- function(name) {
  read.csv(file.path(protocol_dir, name), check.names = FALSE,
           stringsAsFactors = FALSE, na.strings = character())
}
sha256 <- function(path) {
  line <- system2(Sys.which("shasum"), c("-a", "256", shQuote(path)),
                  stdout = TRUE, stderr = TRUE)
  assert(length(line) == 1L, paste("Could not hash", path))
  strsplit(line, "[[:space:]]+")[[1L]][1L]
}
is_true <- function(x) identical(tolower(as.character(x)), "true")

expected_files <- c(
  "README.md", "application-contract.dcf", "carry-forward-manifest.csv",
  "implementation-manifest.csv", "input-manifest.csv", "seed-registry.csv",
  "stage-registry.csv", "state-registry.csv", "test-protocol.R",
  "CHECKSUMS.sha256")
assert(setequal(list.files(protocol_dir), expected_files),
       "The v2.6 protocol directory has unregistered files")

contract <- read.dcf(file.path(protocol_dir, "application-contract.dcf"),
                     all = TRUE)[1L, , drop = TRUE]
assert(identical(unname(contract[["Protocol-Id"]]),
                 "br2017-paperps-rapid-2026-09-07-v2.6-v1"),
       "Unexpected v2.6 application protocol ID")
assert(identical(unname(contract[["Ranks"]]), "0,1,2") &&
         identical(unname(contract[["Primary-Rank"]]), "1"),
       "The q=0,1,2 rank contract changed")
assert(grepl("draft_conditional_pending_confirmatory_validation",
             contract[["Q1-Inference"]], fixed = TRUE) &&
         identical(unname(contract[["Q1-Production-Calibration-Artifact"]]),
                   "absent_and_must_not_be_fabricated") &&
         identical(unname(contract[["Q1-Package-V22-Eligibility"]]),
                   "false_until_confirmatory_validation"),
       "The rapid q=1 inference qualification changed")
assert(identical(unname(contract[["Q1-Riesz-Rule"]]),
                 "fixed_n_train_minus_three_quarters") &&
         identical(unname(contract[["Q1-Riesz-Grid"]]), "none"),
       "The q=1 Riesz rule is not fixed prospectively")
assert(identical(unname(contract[["Q0-Inference"]]),
                 "no_regular_heterogeneity_share_inference") &&
         identical(unname(contract[["Q2-Inference"]]),
                   "no_regular_DML_in_rapid_track"),
       "A sensitivity rank was incorrectly granted regular inference")
assert(identical(unname(contract[["Q1-Primary-GH-Nodes"]]), "31") &&
         identical(unname(contract[["Q1-Fixed-State-GH-Nodes"]]), "45") &&
         identical(unname(contract[["Q2-Primary-GH-Nodes-Per-Dimension"]]), "9") &&
         identical(unname(contract[["Q2-Fixed-State-GH-Nodes-Per-Dimension"]]), "15"),
       "The rapid integration ladder changed")
assert(identical(unname(contract[["Q2-Timing-Sentinel"]]), "outer_2") &&
         identical(unname(contract[["Hard-Computation-Stop-Hours"]]), "16"),
       "The q=2 timing checkpoint changed")

inputs <- read_registry("input-manifest.csv")
assert(nrow(inputs) == 7L && !anyDuplicated(inputs$input_id) &&
         all(tolower(inputs$read_only) == "true"),
       "The input manifest is incomplete or not read-only")
initializers <- inputs$input_id[tolower(inputs$may_initialize) == "true"]
assert(identical(initializers, c("v21_primary_full", "v21_primary_nested")),
       "Only the two selected q=1 containers may initialize v2.6")
assert(!any(grepl("q0|q2", initializers)),
       "An old rank sensitivity was permitted as an initializer")

states <- read_registry("state-registry.csv")
assert(identical(states$scope, c("full", paste0("outer_", 1:5))) &&
         identical(states$n_respondents, c(2000L, rep(1600L, 5L))) &&
         identical(states$hidden, c(8L, 4L, 8L, 8L, 8L, 8L)) &&
         all(states$mean_family == "relu") && all(states$weight_decay == 0.1),
       "The six historically selected q=1 mean learners changed")
assert(identical(which(tolower(states$timing_sentinel) == "true"), 3L),
       "Outer fold 2 must be the timing sentinel")

stages <- read_registry("stage-registry.csv")
assert(identical(stages$stage_index, seq_len(nrow(stages))) &&
         all(c("q1_assemble", "q0_assemble", "q2_outer_2",
               "q2_timing_decision", "q2_assemble", "q1_gh45",
               "q2_gh15") %in% stages$stage_id),
       "The restartable q=0,1,2 stage registry is incomplete")

carry <- read_registry("carry-forward-manifest.csv")
assert(nrow(carry) == 9L && all(carry$mode == "byte_identical"),
       "The v2.5 substantive registries are not exactly carried forward")
for (i in seq_len(nrow(carry))) {
  path <- file.path(repo_root, carry$relative_path[[i]])
  assert(file.exists(path) && identical(sha256(path), carry$sha256[[i]]),
         paste("Carried-forward registry mismatch:", carry$registry[[i]]))
}

implementation <- read_registry("implementation-manifest.csv")
assert(identical(implementation$relative_path, c(
  "applications/br2017/R/br_v26_contract.R",
  "applications/br2017/R/06_run_fast_v26.R",
  "applications/br2017/R/test_br_v26_contract.R")),
  "Unexpected v2.6 implementation registry")
for (i in seq_len(nrow(implementation))) {
  path <- file.path(repo_root, implementation$relative_path[[i]])
  assert(file.exists(path) && identical(sha256(path), implementation$sha256[[i]]),
         paste("Implementation hash mismatch:", path))
  parse(path)
}
runner_text <- paste(readLines(file.path(repo_root,
  "applications/br2017/R/06_run_fast_v26.R"), warn = FALSE), collapse = "\n")
assert(!grepl("fit_q0_full|fit_q2_full|q_sensitivity[.]rds", runner_text),
       "The runner refers to a prohibited historical rank-sensitivity result")
assert(grepl("require_clean = outcome_command", runner_text, fixed = TRUE) &&
         grepl(".br_v26_save_atomic", runner_text, fixed = TRUE) &&
         grepl(".br_v26_analysis", runner_text, fixed = TRUE),
       "The runner lacks its Git or atomic-checkpoint guard")

checksum_lines <- readLines(file.path(protocol_dir, "CHECKSUMS.sha256"),
                            warn = FALSE)
parts <- strsplit(checksum_lines, "  ", fixed = TRUE)
assert(all(lengths(parts) == 2L), "Malformed CHECKSUMS.sha256")
registered <- vapply(parts, `[[`, character(1L), 2L)
expected_hash <- vapply(parts, `[[`, character(1L), 1L)
assert(setequal(registered, setdiff(expected_files, "CHECKSUMS.sha256")),
       "Protocol checksums do not cover exactly every other protocol file")
observed_hash <- vapply(registered, function(name) {
  sha256(file.path(protocol_dir, name))
}, character(1L))
assert(identical(unname(observed_hash), unname(expected_hash)),
       "A protocol file differs from CHECKSUMS.sha256")

source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
if (nzchar(source_root)) {
  required <- inputs[tolower(inputs$required) == "true", , drop = FALSE]
  for (i in seq_len(nrow(required))) {
    path <- file.path(source_root,
                      required$relative_to_ConjointStructural[[i]])
    assert(file.exists(path) && identical(sha256(path), required$sha256[[i]]),
           paste("External input mismatch:", required$input_id[[i]]))
  }
}

cat(sprintf("Ballard--Rosa v2.6 protocol checks passed: %d\n", checks))
