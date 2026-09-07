#!/usr/bin/env Rscript

## Deterministic command-line driver for prospective validation v2.6.  It
## creates neither an approval nor a completion record.  Wave one returns
## only firewall-safe integrity, runtime, and hash metadata.

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1L]])
} else {
  "inst/calibration/v26/run-validation.R"
}
root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                      mustWork = TRUE)
Sys.setenv(SCONJOINT_SOURCE_ROOT = root)
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The v2.6 source driver requires the pkgload package.",
       call. = FALSE)
}
pkgload::load_all(root, quiet = TRUE)

usage <- function() {
  stop(paste(
    "Usage:",
    "  run-validation.R status",
    "  run-validation.R wave WAVE",
    "  run-validation.R collect preliminary|50|100",
    "  run-validation.R assess-preliminary",
    "  run-validation.R assess-50",
    sep = "\n"), call. = FALSE)
}

if (!length(arguments)) usage()
command <- arguments[[1L]]
plan <- scmix_v26_validation_plan()
state_dir <- file.path(root, plan$canonical$state_relative)
run_dir <- file.path(root, plan$canonical$run_relative)

if (identical(command, "status")) {
  if (length(arguments) != 1L) usage()
  ready <- sconjoint:::.scmix_v26_evaluator_readiness(
    plan = plan, state_dir = state_dir, source_root = root)
  print(list(
    protocol_id = ready$protocol_id, status = ready$status,
    evaluator_approval_valid = ready$evaluator_approval_valid,
    wave_one_completed = ready$wave_one_completed,
    preliminary_completed = ready$preliminary_completed,
    confirmatory_50_completed = ready$confirmatory_50_completed,
    confirmatory_100_completed = ready$confirmatory_100_completed,
    family_50_approval_valid = ready$family_50_approval_valid,
    outcome_generation_stage = ready$outcome_generation_stage))
} else if (identical(command, "wave")) {
  if (length(arguments) != 2L) usage()
  print(scmix_v26_run_wave(
    wave = as.integer(arguments[[2L]]), run_dir = run_dir,
    state_dir = state_dir, source_root = root))
} else if (identical(command, "collect")) {
  if (length(arguments) != 2L ||
      !arguments[[2L]] %in% c("preliminary", "50", "100")) usage()
  print(scmix_v26_collect_stage_manifest(
    endpoint = arguments[[2L]], run_dir = run_dir, source_root = root))
} else if (identical(command, "assess-preliminary")) {
  if (length(arguments) != 1L) usage()
  print(scmix_v26_assess_preliminary(
    run_dir = run_dir, state_dir = state_dir, source_root = root))
} else if (identical(command, "assess-50")) {
  if (length(arguments) != 1L) usage()
  print(scmix_v26_assess_confirmatory_50(
    run_dir = run_dir, state_dir = state_dir, source_root = root))
} else {
  usage()
}
