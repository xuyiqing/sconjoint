#!/usr/bin/env Rscript

## Read-only end-to-end dependency and structural-interface test for the
## completed v2.1 post-fit runner. No output directory is created.

options(stringsAsFactors = FALSE)

.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(.script_arg)) stop("Run this file with Rscript.", call. = FALSE)
.script <- normalizePath(sub("^--file=", "", .script_arg[[1L]]),
                         mustWork = TRUE)
.app <- normalizePath(file.path(dirname(.script), "..", ".."),
                      mustWork = TRUE)
.project <- normalizePath(file.path(.app, "..", ".."), mustWork = TRUE)
.runner <- file.path(
  .app, "v2_1", "R", "05_postfit_evidence_v2_1.R")
source(.runner, local = globalenv())

loaded <- .load_runner_sources(.app, envir = globalenv())
stopifnot(
  all(loaded$available),
  identical(environment(.structural_tables), globalenv()),
  exists(".swv21_contrast_labels", envir = globalenv(), inherits = FALSE),
  exists(".sw_exact_joint_predictions", envir = globalenv(),
         inherits = FALSE),
  exists("sw_v21_postfit_config", envir = globalenv(), inherits = FALSE),
  exists("sw_v21_config", envir = globalenv(), inherits = FALSE))

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The project-local pkgload package is required.", call. = FALSE)
}
suppressPackageStartupMessages(pkgload::load_all(.project, quiet = TRUE))
snapshot <- .swv21_resolve_reported_primary(
  sw_v21_config$output_root, sw_v21_config, sw_v21_postfit_config)
full <- .swv21_stable_read_rds(snapshot$fit_paths[["full"]])$value
prepared <- .swv21_stable_read_rds(file.path(
  .app, "results", "prep_analysis_data.rds"))$value
meta <- .swv21_meta(prepared)
contrasts <- .swv21_contrasts(colnames(prepared$deltaX))
structural <- .structural_tables(
  full, prepared, meta, contrasts, sw_v21_postfit_config)
stopifnot(
  nrow(structural$coordinates) == 78L,
  nrow(structural$contrasts) == 78L,
  nrow(structural$choice) == 18L,
  nrow(structural$heterogeneity) == 12L,
  nrow(structural$sign) == 4L,
  identical(snapshot$formal_inference_available, FALSE),
  identical(snapshot$outcome_blind, FALSE))
.swv21_assert_resolution_unchanged(snapshot)

cat("v2.1 post-fit runner dependency/interface test passed\n")
