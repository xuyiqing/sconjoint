#!/usr/bin/env Rscript
## Driver: bounded sign shares along LINEAR CONTRASTS for one application.
## Usage: Rscript applications/R/run_contrast_bounds.R <app> <profile> [floor_mode]
## Structural twin of applications/R/run_share_bounds.R --- same signature
## check against the fit, same inf_cfg, same seed, same output location.
##
## Needs: fit_primary_assembled.rds, share_bound_floor.rds, and (for
## orientation) inference_diagnostic.rds in the profile's fit directory.
##
## floor_mode (default "draws"):
##   draws           project the calibration draws through the contrast by
##                   the row-wise triangle envelope. Conservative (the
##                   ceiling can only be too large, which can only shrink
##                   the bound). Needs share_bound_floor.rds to carry its
##                   `draws` matrix.
##   coordinate_sum  sum_j |c_j| * floor_j. Approximation; warns.
##   supplied        read contrast_bound_floor.rds from the same directory,
##                   a NAMED numeric vector with one exact floor per
##                   contrast, produced by re-running the zero-heterogeneity
##                   calibration with ||A~' c|| recorded in place of
##                   |A~_j|. This is the publication path.

options(stringsAsFactors = FALSE, warn = 1)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 2L) {
  stop("Usage: run_contrast_bounds.R <app> <profile> [floor_mode]")
}
app <- a[[1]]; profile <- a[[2]]
floor_mode <- if (length(a) >= 3L) a[[3]] else "draws"
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
## contrast_bounds.R loads the audited share_bounds.R itself, preferring
## <root>/applications/R/share_bounds.R (its production location) and
## falling back to the Dropbox copy on machines where it has not been
## vendored into the repo yet. run_share_bounds.R sources the production
## path directly; this is the same file, located rather than assumed.
source(file.path(root, "applications/R/contrast_bounds.R"))
source(file.path(root, "applications/R/br_progressivity_contrasts.R"))
message("audited bound math: ", sb_share_bounds_path(root))

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

## Contrast set. br2017 gets the tax-progressivity family; any other
## application must ship its own constructor before it can be run here.
contrasts <- if (identical(app, "br2017")) {
  br_progressivity_contrasts(asm$attr_names)
} else {
  stop("No contrast set defined for application '", app, "'. Add a ",
       "constructor beside br_progressivity_contrasts() and wire it in ",
       "here; do not fall back to positional coordinates.")
}

floors_arg <- fl
if (identical(floor_mode, "supplied")) {
  sup_path <- file.path(dir, "contrast_bound_floor.rds")
  if (!file.exists(sup_path)) {
    stop("floor_mode='supplied' needs ", sup_path,
         " (a named numeric vector, one exact floor per contrast).")
  }
  sup <- readRDS(sup_path)
  if (is.list(sup) && !is.null(sup$analysis_signature) &&
      !identical(sup$analysis_signature, asm$analysis_signature)) {
    stop("Contrast floor calibration does not match the assembled fit.")
  }
  floors_arg <- if (is.list(sup) && !is.null(sup$floor)) sup$floor else sup
}

tb <- sb_contrast_bounds(asm, contrasts, floors_arg, orient = orient,
                         floor_mode = floor_mode, gamma = fl$gamma)
inf_cfg <- list(riesz_validation_fraction = 0.2,
                riesz_equation_tolerance = 0.05,
                ridge_sensitivity_tolerance = 0.10,
                active_eigenvalue_min = 1e-6,
                information_eigenvalue_min = 1e-8,
                rank_tolerance = 1e-8)
tb <- sb_contrast_confidence_bounds(asm, tb, inf_cfg, seed = 20260826L)
tb$floor_R <- fl$R
tb$floor_gamma <- fl$gamma
tb$floor_mode <- floor_mode
## Contrasts whose fitted spread clears the framework's ratio-two
## detection rule are back in the point-identified regime; the bound is
## reported for reference but the ordinary sign-share machinery governs.
tb$regime <- ifelse(tb$fitted_s >= 2 * tb$floor, "point_identified", "floored")
out_csv <- file.path(dir, "contrast_bounds.csv")
utils::write.csv(tb, out_csv, row.names = FALSE)
message("contrast bounds written: ", out_csv)
print(tb[, c("contrast", "modal_side", "s_bar", "lower_bound_gauss",
             "gauss_lcb95", "lower_bound_cantelli", "cant_lcb95")],
      row.names = FALSE, digits = 3)
