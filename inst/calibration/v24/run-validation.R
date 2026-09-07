#!/usr/bin/env Rscript

## Deterministic command-line driver for the prospective v2.4 validation.
## It never creates an approval record.  In particular, `pilot` returns only
## integrity and runtime metadata and does not expose sealed target results.

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1L]])
} else {
  "inst/calibration/v24/run-validation.R"
}
root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                      mustWork = TRUE)
Sys.setenv(SCONJOINT_SOURCE_ROOT = root)
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The v2.4 source driver requires the pkgload package.",
       call. = FALSE)
}
pkgload::load_all(root, quiet = TRUE)

usage <- function() {
  stop(paste(
    "Usage:",
    "  run-validation.R status [STATE_DIR]",
    "  run-validation.R pilot RUN_DIR [STATE_DIR]",
    "  run-validation.R evidence-batch BATCH RUN_DIR [STATE_DIR]",
    "  run-validation.R evidence-manifest RUN_DIR",
    "  run-validation.R evidence-assess RUN_DIR [STATE_DIR]",
    sep = "\n"), call. = FALSE)
}

if (!length(arguments)) usage()
command <- arguments[[1L]]
state_arg <- function(index) {
  if (length(arguments) >= index && nzchar(arguments[[index]]))
    arguments[[index]] else NULL
}
plan <- scmix_v24_validation_plan()

if (identical(command, "status")) {
  state_dir <- state_arg(2L)
  ready <- sconjoint:::.scmix_v24_evaluator_readiness(
    plan = plan, state_dir = state_dir, source_root = root)
  print(list(
    protocol_id = ready$protocol_id, status = ready$status,
    evaluator_approval_valid = ready$evaluator_approval_valid,
    integrity_pilot_completed = ready$integrity_pilot_completed,
    evidence_study_completed = ready$evidence_study_completed,
    family_approval_valid = ready$family_approval_valid))
} else if (identical(command, "pilot")) {
  if (length(arguments) < 2L) usage()
  result <- scmix_v24_run_pilot_wave(
    run_dir = arguments[[2L]], state_dir = state_arg(3L),
    source_root = root)
  ## This object contains no target, gate, interval, or coverage result.
  print(result)
} else if (identical(command, "evidence-batch")) {
  if (length(arguments) < 3L) usage()
  result <- scmix_v24_run_evidence_batch(
    batch_index = as.integer(arguments[[2L]]),
    run_dir = arguments[[3L]], state_dir = state_arg(4L),
    source_root = root)
  print(result)
} else if (identical(command, "evidence-manifest")) {
  if (length(arguments) != 2L) usage()
  print(scmix_v24_collect_evidence_manifest(
    run_dir = arguments[[2L]], source_root = root))
} else if (identical(command, "evidence-assess")) {
  if (length(arguments) < 2L) usage()
  print(scmix_v24_assess_evidence(
    run_dir = arguments[[2L]], state_dir = state_arg(3L),
    source_root = root))
} else {
  usage()
}
