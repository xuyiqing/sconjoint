#!/usr/bin/env Rscript
## Driver: the maintained-ceiling sensitivity display (audit work package 2).
## Usage: Rscript applications/R/run_sensitivity_profile.R <app> <profile>
##
## WHY THIS EXISTS. The share bound is a deterministic consequence of a
## MAINTAINED ceiling on residual dispersion. The calibration supplies one
## candidate ceiling, and that candidate is a null detection threshold, not
## an upper confidence limit. Attaching a confidence statement to it
## overstates what is established. Showing the bound as a function of the
## ceiling a reader is willing to maintain does not: the calibration value
## becomes one marked point on a curve instead of the whole claim.
##
## Only PRESPECIFIED directions appear. An unprespecified direction has no
## defined reported side, so it has no curve either.

options(stringsAsFactors = FALSE, warn = 1)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 2L) stop("Usage: run_sensitivity_profile.R <app> <profile>")
app <- a[[1]]; profile <- a[[2]]
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/contrast_bounds.R"))
source(file.path(root, "applications/R/br_progressivity_contrasts.R"))
source(file.path(root, "applications/R/orientation_spec.R"))
source(file.path(root, "applications/R/provenance.R"))

dir <- file.path(root, "applications", app, "results/mixed_logit", profile)
asm <- readRDS(file.path(dir, "fit_primary_assembled.rds"))
fl <- readRDS(file.path(dir, "share_bound_floor.rds"))
if (!identical(fl$analysis_signature, asm$analysis_signature)) {
  stop("Floor calibration does not match the assembled fit.")
}

contrasts <- if (identical(app, "br2017")) {
  br_progressivity_contrasts(asm$attr_names)
} else {
  stop("No contrast set defined for application '", app, "'.")
}
C <- sb_as_contrast_matrix(contrasts, as.character(asm$attr_names))
ospec <- orient_spec_for(app, "contrast")

## The calibration's own ceiling, for the marked point, and a geometric
## grid around it. The grid is supplied here, not read off the fit.
fl$floor <- sb_matched_floor(fl, attr_names = asm$attr_names)
cal_ceiling <- sb_contrast_floor(C, fl, floor_mode = "draws",
                                 gamma = fl$gamma)
mult <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 12, 16)
prof <- do.call(rbind, lapply(colnames(C), function(nm) {
  spec_j <- orient_lookup(ospec, nm)
  if (!identical(spec_j$orientation_source, "prespecified")) return(NULL)
  grid <- as.numeric(cal_ceiling[[nm]]) * mult
  p1 <- sb_sensitivity_table(sb_project_assembled(asm, C[, nm, drop = FALSE]),
                             matrix(1, 1L, 1L, dimnames = list(nm, nm)),
                             ospec, grid)
  p1$ceiling_multiple <- mult
  p1$calibration_ceiling <- as.numeric(cal_ceiling[[nm]])
  p1
}))
if (is.null(prof) || !nrow(prof)) {
  stop("No prespecified contrast to profile; nothing to display.")
}
prof$ceiling_source <- "supplied externally (grid); calibration value marked"
prof <- sb_stamp_provenance(prof, app = app, profile = profile, fit = asm,
                            calibration = fl, seed = NA_integer_,
                            producer = "applications/R/run_sensitivity_profile.R",
                            target_label = "maintained_ceiling_sensitivity")
out_csv <- file.path(dir, "sensitivity_profile.csv")
utils::write.csv(prof, out_csv, row.names = FALSE)
message("sensitivity profile written: ", out_csv)
print(prof[prof$ceiling_multiple %in% c(0.5, 1, 2, 4),
           c("contrast", "ceiling_multiple", "s_bar", "lower_bound_gauss",
             "lower_bound_cantelli")], row.names = FALSE, digits = 3)
