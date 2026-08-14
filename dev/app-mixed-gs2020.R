# E1: gs2020 (democracy) under the integrated mixed logit.
# scmix fit -> zero-floor calibration -> design check -> app bundle.
#
# Every artifact is cache-gated on file.exists() and saved the moment it is
# computed, so a crash or rerun never repeats finished work. BEFORE side
# reuses the frozen v3 canonical fit (replicate fit_holdout_seed42.rds,
# stage2 = map_c5) when its attribute frame matches; falls back to a fresh
# scfit with the tutorial 03 recipe otherwise.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
stopifnot(dir.exists(OUT_DIR))
dir.create(file.path(OUT_DIR, "logs"), showWarnings = FALSE)

ts <- function() format(Sys.time(), "[%H:%M:%S]")
say <- function(...) cat(ts(), sprintf(...), "\n")

bt <- function(x) paste0("`", x, "`")
mk_formula <- function(attrs, zvars) {
  stats::as.formula(paste("choice ~", paste(bt(attrs), collapse = " + "),
                          "|", paste(bt(zvars), collapse = " + ")))
}

# Paper main spec: 30 contrasts | held-out 16-covariate moderator set
# (tutorial/03-example-gs.qmd; drops the six direct democracy-attitude items).
gs_attrs <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote", "diff_dem_code_v_affair",
  "diff_dem_code_v_tax", "diff_sex_Female", "diff_race_Asian",
  "diff_race_Black", "diff_race_Hispanic", "diff_pro_Farmer",
  "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")
gs_z <- c("z_ideo", "z_pid7", "z_trump", "z_age", "z_educ", "z_hhi",
          "z_auth", "z_knowl", "z_female", "z_race_black", "z_race_asian",
          "z_race_other", "E_ideal", "I_ideal", "M_ideal", "T_ideal")
gs_formula <- mk_formula(gs_attrs, gs_z)

data(gs2020, package = "sconjoint")

## STEP 1: scmix fit ---------------------------------------------------------
fit_path <- file.path(OUT_DIR, "scmix_fit_gs2020.rds")
if (file.exists(fit_path)) {
  mx <- readRDS(fit_path)
  say("scmix fit loaded from cache")
} else {
  say("scmix fit starting (N=1605, p=30, q=1, K=5, 600 epochs)")
  t0 <- Sys.time()
  mx <- scmix(gs_formula, gs2020, respondent = "respondent", task = "task",
              profile = "profile", q = 1L, K = 5L, n_epochs = 600L,
              seed = 42L)
  say("scmix fit done: %.1f min",
      as.numeric(difftime(Sys.time(), t0, units = "mins")))
  saveRDS(mx, fit_path)
  say("scmix fit saved")
}

## STEP 2: zero-floor calibration -------------------------------------------
if (is.null(mx$zero_floor)) {
  say("zero-floor calibration starting (R = 2 full retrains)")
  t0 <- Sys.time()
  mx$zero_floor <- scmix_calibrate_zero(mx, R = 2L)
  say("zero floor done: %.1f min | ratio = %.2f",
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      mx$zero_floor$ratio)
  saveRDS(mx, fit_path)
  say("fit re-saved with zero_floor")
} else {
  say("zero floor cached: ratio = %.2f", mx$zero_floor$ratio)
}

## STEP 3: design check ------------------------------------------------------
dc_path <- file.path(OUT_DIR, "design_check_gs2020.rds")
if (file.exists(dc_path)) {
  dc <- readRDS(dc_path)
  say("design check loaded from cache")
} else {
  say("design check starting (p=30 information sim)")
  t0 <- Sys.time()
  dc <- scmix_design_check(mx, n_bins = 50L, seed = 7L)
  say("design check done: %.1f min | weak directions: %d | identified: %d/%d",
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      dc$weak_directions, length(dc$identified), length(mx$attr_names))
  saveRDS(dc, dc_path)
}

## STEP 4: BEFORE side (two-stage comparator) --------------------------------
before_path <- file.path(OUT_DIR, "scfit_fit_gs2020.rds")
canon_path <- path.expand(file.path(
  "~/Dropbox/Research_Hub/Projects/ConjointStructural",
  "replicate/data/derived/gs2020/fits/fit_holdout_seed42.rds"))
if (file.exists(before_path)) {
  before <- readRDS(before_path)
  say("two-stage fit loaded from OUT_DIR cache")
} else {
  before <- NULL
  if (file.exists(canon_path)) {
    cand <- readRDS(canon_path)
    if (is.list(cand) && !inherits(cand, "sc_fit") && !is.null(cand$fit))
      cand <- cand$fit
    if (inherits(cand, "sc_fit") &&
        identical(names(cand$theta), mx$attr_names) &&
        length(unique(cand$respondent_id)) == mx$N) {
      before <- cand
      say("two-stage: reusing frozen v3 canonical fit (fit_holdout_seed42)")
      saveRDS(before, before_path)
    } else {
      say("two-stage: canonical fit incompatible, will refit fresh")
    }
  }
  if (is.null(before)) {
    say("scfit (two-stage, map_c5, K=10) starting")
    t0 <- Sys.time()
    before <- scfit(gs_formula, gs2020, respondent = "respondent",
                    task = "task", profile = "profile", K = 10L, seed = 42L,
                    stage2 = "map_c5", parallel = TRUE, n_cores = 10L)
    say("scfit done: %.1f min",
        as.numeric(difftime(Sys.time(), t0, units = "mins")))
    saveRDS(before, before_path)
  }
}

## STEP 5: quantities + app bundle (run_app schema) --------------------------
app_path <- file.path(OUT_DIR, "app_gs2020.rds")
if (file.exists(app_path)) {
  say("app bundle cached; nothing to do")
} else {
  contrast_name <- "diff_respParty"
  say("quantities starting (theta, pi, V(c) at n_bins=50, seed=7)")
  t0 <- Sys.time()
  th <- scmix_theta(mx, n_bins = 50L, seed = 7L)
  say("theta done: %.1f min",
      as.numeric(difftime(Sys.time(), t0, units = "mins")))
  t0 <- Sys.time()
  pol <- scmix_polarization(mx, n_bins = 50L, seed = 7L)
  say("polarization done: %.1f min",
      as.numeric(difftime(Sys.time(), t0, units = "mins")))
  cv <- stats::setNames(numeric(length(mx$attr_names)), mx$attr_names)
  cv[contrast_name] <- 1
  t0 <- Sys.time()
  vc <- scmix_counterfactual(mx, contrast = cv, n_bins = 50L, seed = 7L)
  say("counterfactual done: %.1f min",
      as.numeric(difftime(Sys.time(), t0, units = "mins")))

  stopifnot(identical(names(before$theta), mx$attr_names))
  before_se <- sqrt(pmax(diag(before$vcov), 0))
  first_bh <- !duplicated(before$respondent_id)
  before_sign <- colMeans(before$beta_hat[first_bh, , drop = FALSE] > 0)
  cvb <- stats::setNames(numeric(length(before$theta)), names(before$theta))
  cvb[contrast_name] <- 1
  before_share <- mean(stats::plogis(before$beta_hat %*% cvb))

  sd_mix <- sqrt(diag(Reduce(`+`, lapply(mx$A_folds, tcrossprod)) /
                        length(mx$A_folds)))
  res <- list(
    name = "gs2020",
    attr_names = mx$attr_names,
    before = list(theta = before$theta, se = before_se,
                  sign_share = before_sign, share = before_share,
                  sigma_prior = before$sigma_prior,
                  stage2_method = before$stage2_method),
    after = list(theta = unname(th$estimate), se = unname(th$se),
                 pi = unname(pol$estimate), pi_se = unname(pol$se),
                 pi_floored = pol$extra$floored,
                 share = unname(vc$estimate), share_se = unname(vc$se),
                 sd_resid = sd_mix,
                 A_folds = mx$A_folds,
                 loss_final = vapply(mx$loss_traces, function(l) l[length(l)],
                                     numeric(1))),
    contrast_name = contrast_name,
    n_respondents = mx$N, T_range = range(table(mx$respondent_id))
  )
  saveRDS(res, app_path)
  say("app bundle saved")
  cat("theta (before | after):\n")
  print(round(cbind(before = before$theta, after = th$estimate), 3))
}

say("floor ratio: %.2f (threshold 2; br 3.4 / sw 1.7 for reference)",
    mx$zero_floor$ratio)
cat("\nDONE E1 gs2020\n")
