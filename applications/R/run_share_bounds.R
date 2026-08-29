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
source(file.path(root, "applications/R/provenance.R"))
source(file.path(root, "applications/R/orientation_spec.R"))

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

## Matched ceiling (audit finding P2): average within replication, then
## quantile across replications. Recomputed from the calibration's saved
## draws, so no refit is needed; `fl$floor` on a pre-audit artifact is the
## pooled fold-level quantile, which is a different statistic.
floors <- sb_matched_floor(fl, attr_names = asm$attr_names)
floor_pooled <- fl$floor[asm$attr_names]
message("ceiling statistic: matched (within-replication fold average, ",
        "quantile across replications)")
## Orientation comes from the application's PRESPECIFIED table. `orient`
## (one-step thetas) is still passed so unspecified rows keep their
## diagnostic side, but only a prespecified row can be displayed.
ospec <- orient_spec_for(app, "coordinate")
message("prespecified orientations on file for ", app, ": ", nrow(ospec))
tb <- sb_bounds_table(asm, floors, orient = orient, orientation = ospec)
tb$floor_pooled_preaudit <- as.numeric(floor_pooled)
inf_cfg <- list(riesz_validation_fraction = 0.2,
                riesz_equation_tolerance = 0.05,
                ridge_sensitivity_tolerance = 0.10,
                active_eigenvalue_min = 1e-6,
                information_eigenvalue_min = 1e-8,
                rank_tolerance = 1e-8)
tb <- sb_confidence_bounds(asm, tb, inf_cfg, seed = 20260826L)
## Pointwise by default: the coordinate table is a row-by-row description,
## not a joint claim. A runner that wants a familywise caption must set a
## claim family explicitly (audit work package 4).
tb <- sb_attach_multiplicity(tb, claim_family = NULL)
tb$floor_R <- fl$R
tb$floor_gamma <- fl$gamma
## `regime` is set inside sb_bounds_table() now, together with the release
## gate: the intermediate window (floor <= fitted_s < 2 floor) has no valid
## ceiling and is withheld, and the point-identified regime is not reported
## as a bound because the ordinary sign-share machinery governs there.
tb <- sb_stamp_provenance(tb, app = app, profile = profile, fit = asm,
                          calibration = fl, seed = 20260826L,
                          producer = "applications/R/run_share_bounds.R")
## Hard gate before anything manuscript-facing is written.
orient_require_prespecified(tb, what = "share_bounds.csv")
out_csv <- file.path(dir, "share_bounds.csv")
utils::write.csv(tb, out_csv, row.names = FALSE)
message("share bounds written: ", out_csv)
print(tb[, c("coordinate", "orientation_side", "orientation_source",
             "regime", "released_lower_bound_gauss", "bound_release")],
      row.names = FALSE, digits = 3)
message("released rows: ", sum(sb_is_released(tb$bound_release)), " of ",
        nrow(tb))
