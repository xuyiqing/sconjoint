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
##                   |A~_j|. It removes the projection approximation only;
##                   the ceiling stays a null detection threshold, so the
##                   output stays a conditional sensitivity bound.

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
source(file.path(root, "applications/R/provenance.R"))
source(file.path(root, "applications/R/orientation_spec.R"))
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
## Matched ceiling (audit finding P2). For the 'draws' and
## 'coordinate_sum' modes sb_contrast_floor() now averages each
## replication over its folds before quantiling; for 'coordinate_sum' the
## per-coordinate input must be the matched floor as well, so replace the
## calibration's pre-audit pooled `floor` before it is read.
if (!identical(floor_mode, "supplied") && !is.null(fl$draws)) {
  fl$floor <- sb_matched_floor(fl, attr_names = asm$attr_names)
  floors_arg <- fl
  message("ceiling statistic: matched (within-replication fold average, ",
          "quantile across replications)")
}
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

## Prespecified orientation, by contrast name. br_progressivity_contrasts()
## already defines its contrasts so that positive means progressive, so the
## side is fixed by the definition rather than by the fit.
ospec <- orient_spec_for(app, "contrast")
message("prespecified orientations on file for ", app, ": ", nrow(ospec))
tb <- sb_contrast_bounds(asm, contrasts, floors_arg, orient = orient,
                         floor_mode = floor_mode, gamma = fl$gamma,
                         orientation = ospec)
inf_cfg <- list(riesz_validation_fraction = 0.2,
                riesz_equation_tolerance = 0.05,
                ridge_sensitivity_tolerance = 0.10,
                active_eigenvalue_min = 1e-6,
                information_eigenvalue_min = 1e-8,
                rank_tolerance = 1e-8)
tb <- sb_contrast_confidence_bounds(asm, tb, inf_cfg, seed = 20260826L)
## The progressivity contrasts ARE read jointly in the report ("both
## submitted claims survive"), so they form one claim family and carry a
## Bonferroni-adjusted endpoint alongside the pointwise one.
claim_fam <- ifelse(sb_is_released(tb$bound_release) &
                      tb$contrast %in% c("top_minus_bottom", "slope",
                                         "slope_unit"),
                    "br_progressivity", NA_character_)
tb <- sb_attach_multiplicity(tb, claim_family = claim_fam,
                             alpha_family = 0.05)
tb$floor_R <- fl$R
tb$floor_gamma <- fl$gamma
tb$floor_mode <- floor_mode
## `regime` and the release gate are set inside sb_bounds_table(): the
## intermediate window (floor <= fitted_s < 2 floor) has no valid ceiling
## and is withheld; the point-identified regime is not reported as a bound.
tb <- sb_stamp_provenance(tb, app = app, profile = profile, fit = asm,
                          calibration = fl, seed = 20260826L,
                          producer = "applications/R/run_contrast_bounds.R",
                          target_label = "contrast_directional_share_bound")
orient_require_prespecified(tb, what = "contrast_bounds.csv")
## A familywise caption is only writable if the family carries its
## adjustment metadata; check it here so the artifact cannot ship claiming
## a family it has not adjusted for.
if (any(!is.na(tb$claim_family))) {
  for (f in unique(stats::na.omit(tb$claim_family))) {
    sb_require_family_adjustment(tb, f, what = paste0("'", f, "' caption"))
  }
  message("claim families adjusted: ",
          paste(unique(stats::na.omit(tb$claim_family)), collapse = ", "))
}
out_csv <- file.path(dir, "contrast_bounds.csv")
utils::write.csv(tb, out_csv, row.names = FALSE)
message("contrast bounds written: ", out_csv)
print(tb[, c("contrast", "orientation_side", "orientation_source",
             "regime", "released_lower_bound_gauss",
             "released_gauss_cond_ladj", "bound_release")],
      row.names = FALSE, digits = 3)
message("released rows: ", sum(sb_is_released(tb$bound_release)), " of ",
        nrow(tb))
