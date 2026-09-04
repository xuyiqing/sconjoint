#!/usr/bin/env Rscript
## Three-way comparison of the gs2020 share-bound ceilings and release
## decisions under three null calibrations that differ ONLY in how the
## refit learner is chosen:
##
##   A  record   relu8 fixed, 4000 epochs  (the calibration of record)
##   B  fixed    relu4 fixed, 4000 epochs  (the memo's recommended default:
##                                          the learner folds 1-2 selected,
##                                          but still fixed)
##   C  matched  the analysis's own inner-CV selection over the declared
##               grid, 6000 epochs         (procedure-matched)
##
## Arm C also matches the analysis on epochs; the calibration of record used
## 4000 against the profile's 6000, so A differs from the analysis on two
## axes, not one. B isolates the learner swap alone.
options(stringsAsFactors = FALSE, warn = 1)
root <- normalizePath(Sys.getenv("SCONJOINT_ROOT", unset = path.expand("~/GitHub/sconjoint")), mustWork = TRUE)
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/share_bounds.R"))
source(file.path(root, "applications/R/share_bounds_matched.R"))
source(file.path(root, "applications/R/provenance.R"))
source(file.path(root, "applications/R/orientation_spec.R"))

app <- "gs2020"; profile <- "v21_corrected"
dir <- file.path(root, "applications", app, "results/mixed_logit", profile)
mc <- file.path(dir, "matched_calibration")
asm <- readRDS(file.path(dir, "fit_primary_assembled.rds"))

arms <- list()
arms$record <- readRDS(file.path(dir, "share_bound_floor_signed.rds"))
if (file.exists(file.path(mc, "armB_fixed_relu4.rds")))
  arms$fixed_relu4 <- readRDS(file.path(mc, "armB_fixed_relu4.rds"))
if (file.exists(file.path(mc, "armC_matched.rds")))
  arms$matched <- readRDS(file.path(mc, "armC_matched.rds"))
message("arms available: ", paste(names(arms), collapse = ", "))

orient <- NULL
inf_path <- file.path(dir, "inference_diagnostic.rds")
if (file.exists(inf_path)) {
  inf <- readRDS(inf_path)
  th <- inf$main$estimate
  th_lab <- grep("^theta:", names(th), value = TRUE)
  orient <- stats::setNames(as.numeric(th[th_lab]), sub("^theta:", "", th_lab))
}
ospec <- orient_spec_for(app, "coordinate")
inf_cfg <- list(riesz_validation_fraction = 0.2,
                riesz_equation_tolerance = 0.05,
                ridge_sensitivity_tolerance = 0.10,
                active_eigenvalue_min = 1e-6,
                information_eigenvalue_min = 1e-8,
                rank_tolerance = 1e-8)

build_arm <- function(fl, nm) {
  message("-- arm ", nm, " (R = ", fl$R, ", epochs = ", fl$n_epochs, ")")
  floors <- sb_matched_floor(fl, attr_names = asm$attr_names)
  tb <- sb_bounds_table(asm, floors, orient = orient, orientation = ospec)
  ## SB_FAST=1 skips the confidence-bound step, which wraps scmix_dml and is
  ## the expensive part. Regimes, release decisions and the L_G lower bounds
  ## do not depend on it, so the structural comparison is available without
  ## it; the reported one-sided limits are not.
  if (!nzchar(Sys.getenv("SB_FAST"))) {
    tb <- sb_confidence_bounds(asm, tb, inf_cfg, seed = 20260826L)
    tb <- sb_attach_multiplicity(tb, claim_family = NULL)
  } else {
    message("   (SB_FAST: confidence bounds skipped)")
  }
  tb$arm <- nm
  tb$floor_value <- as.numeric(floors[tb$coordinate])
  tb
}

tabs <- lapply(names(arms), function(nm) build_arm(arms[[nm]], nm))
names(tabs) <- names(arms)
all_tb <- do.call(rbind, tabs)
utils::write.csv(all_tb, file.path(mc, paste0("share_bounds_by_arm", ifelse(nzchar(Sys.getenv("SB_FAST")), "_fast", ""), ".csv")),
                 row.names = FALSE)

## Wide comparison on the decision-relevant columns.
key <- c("coordinate", "regime", "bound_release", "floor_value",
         "fitted_s", "released_lower_bound_gauss", "released_gauss_cond_l95",
         "released_lower_bound_cantelli", "released_gauss_cond_ladj")
key <- intersect(key, names(tabs[[1]]))
wide <- tabs[[1]][, "coordinate", drop = FALSE]
for (nm in names(tabs)) {
  t <- tabs[[nm]][, key]
  names(t)[-1] <- paste0(names(t)[-1], "_", nm)
  wide <- merge(wide, t, by = "coordinate", all = TRUE)
}
utils::write.csv(wide, file.path(mc, paste0("share_bounds_comparison", ifelse(nzchar(Sys.getenv("SB_FAST")), "_fast", ""), ".csv")),
                 row.names = FALSE)

cat("\n=== RELEASED ROWS BY ARM ===\n")
for (nm in names(tabs)) {
  rel <- sb_is_released(tabs[[nm]]$bound_release)
  cat(sprintf("%-14s %2d of %2d released: %s\n", nm, sum(rel), nrow(tabs[[nm]]),
              paste(tabs[[nm]]$coordinate[rel], collapse = ", ")))
}
cat("\n=== REGIME CHANGES vs record ===\n")
if (length(tabs) > 1L) {
  base <- tabs[["record"]]
  for (nm in setdiff(names(tabs), "record")) {
    t <- tabs[[nm]]
    m <- merge(base[, c("coordinate", "regime", "bound_release")],
               t[, c("coordinate", "regime", "bound_release")],
               by = "coordinate", suffixes = c("_record", paste0("_", nm)))
    chg <- m[m[[2]] != m[[4]] | m[[3]] != m[[5]], ]
    cat("--", nm, ":", nrow(chg), "of", nrow(m), "rows changed\n")
    if (nrow(chg)) print(chg, row.names = FALSE)
  }
}
cat("\n=== FLOOR RATIOS (arm / record) ===\n")
fr <- sb_matched_floor(arms$record, attr_names = asm$attr_names)
for (nm in setdiff(names(arms), "record")) {
  fa <- sb_matched_floor(arms[[nm]], attr_names = asm$attr_names)
  r <- fa / fr
  cat(sprintf("%-14s median %.3f  min %.3f  max %.3f\n", nm,
              stats::median(r), min(r), max(r)))
}
cat("\nCOMPARISON_DONE\n")
