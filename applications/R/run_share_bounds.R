#!/usr/bin/env Rscript
## Driver: bounded sign shares for one application.
## Usage: Rscript applications/R/run_share_bounds.R <app> <profile>
## Needs: fit_primary_assembled.rds, share_bound_floor.rds, and (for
## orientation) inference_diagnostic.rds in the profile's fit directory.

options(stringsAsFactors = FALSE, warn = 1)
a <- commandArgs(trailingOnly = TRUE)
app <- a[[1]]; profile <- a[[2]]
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/share_bounds.R"))

dir <- file.path(root, "applications", app, "results/mixed_logit", profile)
asm <- readRDS(file.path(dir, "fit_primary_assembled.rds"))
fl <- readRDS(file.path(dir, "share_bound_floor.rds"))
if (!identical(fl$analysis_signature, asm$analysis_signature)) {
  stop("Floor calibration does not match the assembled fit.")
}

orient <- NULL
inf_path <- file.path(dir, "inference_diagnostic.rds")
if (file.exists(inf_path)) {
  inf <- readRDS(inf_path)
  th <- inf$main$estimate
  th_lab <- grep("^theta:", names(th), value = TRUE)
  orient <- stats::setNames(as.numeric(th[th_lab]),
                            sub("^theta:", "", th_lab))
}

tb <- sb_bounds_table(asm, fl$floor, orient = orient)
inf_cfg <- list(riesz_validation_fraction = 0.2,
                riesz_equation_tolerance = 0.05,
                ridge_sensitivity_tolerance = 0.10,
                active_eigenvalue_min = 1e-6,
                information_eigenvalue_min = 1e-8,
                rank_tolerance = 1e-8)
tb <- sb_confidence_bounds(asm, tb, inf_cfg, seed = 20260826L)
tb$floor_R <- fl$R
tb$floor_gamma <- fl$gamma
## Coordinates whose fitted spread clears the framework's ratio-two
## detection rule are back in the point-identified regime; the bound is
## reported for reference but the ordinary sign-share machinery governs.
tb$regime <- ifelse(tb$fitted_s >= 2 * tb$floor, "point_identified", "floored")
out_csv <- file.path(dir, "share_bounds.csv")
utils::write.csv(tb, out_csv, row.names = FALSE)
message("share bounds written: ", out_csv)
print(tb[, c("coordinate", "modal_side", "s_bar", "lower_bound_gauss",
             "gauss_lcb95", "lower_bound_cantelli", "cant_lcb95")],
      row.names = FALSE, digits = 3)
