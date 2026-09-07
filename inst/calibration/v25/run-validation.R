#!/usr/bin/env Rscript

## Deterministic command-line driver for the prospective v2.5 validation.
## It never creates an approval record.  In particular, `pilot` returns only
## integrity and runtime metadata and does not expose sealed target results.

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1L]])
} else {
  "inst/calibration/v25/run-validation.R"
}
root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                      mustWork = TRUE)
Sys.setenv(SCONJOINT_SOURCE_ROOT = root)
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The v2.5 source driver requires the pkgload package.",
       call. = FALSE)
}
pkgload::load_all(root, quiet = TRUE)

usage <- function() {
  stop(paste(
    "Usage:",
    "  run-validation.R status",
    "  run-validation.R pilot",
    "  run-validation.R evidence-batch BATCH",
    "  run-validation.R evidence-manifest",
    "  run-validation.R evidence-assess",
    sep = "\n"), call. = FALSE)
}

if (!length(arguments)) usage()
command <- arguments[[1L]]
plan <- scmix_v25_validation_plan()
state_dir <- file.path(root, plan$canonical$state_relative)
run_dir <- file.path(root, plan$canonical$run_relative)

if (identical(command, "status")) {
  if (length(arguments) != 1L) usage()
  ready <- sconjoint:::.scmix_v25_evaluator_readiness(
    plan = plan, state_dir = state_dir, source_root = root)
  print(list(
    protocol_id = ready$protocol_id, status = ready$status,
    evaluator_approval_valid = ready$evaluator_approval_valid,
    integrity_pilot_completed = ready$integrity_pilot_completed,
    evidence_study_completed = ready$evidence_study_completed,
    family_approval_valid = ready$family_approval_valid))
} else if (identical(command, "pilot")) {
  if (length(arguments) != 1L) usage()
  result <- scmix_v25_run_pilot_wave(
    run_dir = run_dir, state_dir = state_dir, source_root = root)
  ## This object contains no target, gate, interval, or coverage result.
  print(result)
} else if (identical(command, "evidence-batch")) {
  if (length(arguments) != 2L) usage()
  result <- scmix_v25_run_evidence_batch(
    batch_index = as.integer(arguments[[2L]]),
    run_dir = run_dir, state_dir = state_dir, source_root = root)
  print(result)
} else if (identical(command, "evidence-manifest")) {
  if (length(arguments) != 1L) usage()
  print(scmix_v25_collect_evidence_manifest(
    run_dir = run_dir, source_root = root))
} else if (identical(command, "evidence-assess")) {
  if (length(arguments) != 1L) usage()
  print(scmix_v25_assess_evidence(
    run_dir = run_dir, state_dir = state_dir, source_root = root))
} else {
  usage()
}
