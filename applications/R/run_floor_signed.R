#!/usr/bin/env Rscript
## Driver: rerun the zero-heterogeneity calibration RETAINING the signed
## raw-scale loading matrix A~ from every (replication, fold) refit
## (audit work package 5).
##
## Usage: Rscript applications/R/run_floor_signed.R <app> <profile> <hidden> <wd> [R]
##
## Why a second artifact rather than a rewrite: the existing
## `share_bound_floor.rds` is the calibration every current number rests
## on. This driver writes `share_bound_floor_signed.rds` beside it and
## VERIFIES that the per-coordinate norms reproduce the original exactly.
## If they do, the signed matrices describe the same calibration and the
## composite ceiling can be computed exactly without changing any
## per-coordinate floor. If they do not, the run stops rather than
## silently substituting a different calibration.

options(stringsAsFactors = FALSE, warn = 1)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 4L) {
  stop("Usage: run_floor_signed.R <app> <profile> <hidden> <wd> [R]")
}
app <- a[[1]]; profile <- a[[2]]
hidden <- as.integer(strsplit(a[[3]], "-")[[1]])
wd <- as.numeric(a[[4]])
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/share_bounds.R"))
source(file.path(root, "applications/R/provenance.R"))

dir <- file.path(root, "applications", app, "results/mixed_logit", profile)
asm <- readRDS(file.path(dir, "fit_primary_assembled.rds"))
old <- readRDS(file.path(dir, "share_bound_floor.rds"))
R <- if (length(a) >= 5L) as.integer(a[[5]]) else as.integer(old$R)
log <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
                       flush.console() }
log("recalibrating", app, profile, "| hidden", paste(hidden, collapse = "-"),
    "| wd", wd, "| R", R, "| folds", paste(old$folds_use, collapse = ","))

t0 <- Sys.time()
fl <- sb_zero_floor(asm, hidden = hidden, weight_decay = wd, R = R,
                    folds_use = old$folds_use, gamma = old$gamma,
                    n_epochs = as.integer(old$n_epochs),
                    keep_loadings = TRUE)
log(sprintf("done in %.1f min", as.numeric(difftime(Sys.time(), t0,
                                                    units = "mins"))))

## Identity check against the calibration of record.
if (identical(dim(fl$draws), dim(old$draws))) {
  dev <- max(abs(fl$draws - old$draws))
  log(sprintf("norm reproduction vs share_bound_floor.rds: max|dev| = %.3e",
              dev))
  if (dev > 1e-10) {
    stop("The signed rerun does NOT reproduce the calibration of record ",
         "(max|dev| = ", format(dev, digits = 3), "). Refusing to write a ",
         "different calibration under a name that implies the same one. ",
         "Check the learner spec (hidden/weight_decay) and n_epochs.")
  }
} else {
  stop("Shape mismatch against the calibration of record: rerun ",
       paste(dim(fl$draws), collapse = "x"), " vs stored ",
       paste(dim(old$draws), collapse = "x"), ".")
}
stopifnot(sb_calibration_has_loadings(fl))
fl$reproduces_calibration_of_record <- TRUE
fl$learner <- list(hidden = hidden, weight_decay = wd)

out <- file.path(dir, "share_bound_floor_signed.rds")
saveRDS(fl, out, version = 3)
log("written:", out)
log("signed loadings:", length(fl$A_raw_draws), "matrices of",
    paste(dim(fl$A_raw_draws[[1]]), collapse = "x"))
log("DONE")
