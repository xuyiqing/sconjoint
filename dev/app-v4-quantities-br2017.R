# E2 quantity menu: br2017 (tax) under the integrated estimator.
# Reads the cached final-code fit; every quantity is a cached slot.
# Conventions: n_bins = 50, seed = 7 (the app-mixed-comparison settings).
source("dev/app-v4-common.R")

mx <- readRDS(file.path(OUT_DIR, "scmix_fit_br2017.rds"))
slots_open("br2017")

## production constants (replicate/code/empirical/br2017/compare_results.R)
RATE_COLS <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
               "rate_175_375", "rate_375P")
log_mid_c <- log(c(5, 22.5, 60, 130, 275, 500))
log_mid_c <- log_mid_c - mean(log_mid_c)
slope_w <- log_mid_c / sum(log_mid_c^2)
LEVELS_FIXED <- list(rate_L10 = c(0, 5, 15, 25),
                     rate_10_35 = c(5, 15, 25, 35),
                     rate_35_85 = c(5, 15, 25, 35),
                     rate_85_175 = c(5, 15, 25, 35),
                     rate_175_375 = c(5, 15, 25, 35, 45),
                     rate_375P = c(5, 15, 25, 35, 45, 55),
                     revenue_score = c(-2, -1, 0, 1, 2))
## plan schedules + revenue score, attr order = c(RATE_COLS, revenue_score)
plans <- list(progressive = c(0, 5, 15, 25, 35, 45, 2),
              statusquo = c(5, 15, 25, 25, 35, 35, 0),
              flat = c(15, 15, 15, 15, 15, 15, 0),
              regressive = c(25, 25, 15, 15, 5, 5, -2))
stopifnot(identical(mx$attr_names, c(RATE_COLS, "revenue_score")))
plan_D <- rbind(
  prog_vs_flat = plans$progressive - plans$flat,
  prog_vs_statusquo = plans$progressive - plans$statusquo,
  prog_vs_regressive = plans$progressive - plans$regressive)

meta <- resp_meta_for("br2017", mx)
party <- party3_from_pid7(meta$pid7)
stopifnot(!anyNA(party))

slope_delta <- stats::setNames(slope_w, RATE_COLS)
tmb_delta <- c(rate_375P = 1, rate_L10 = -1)

## zero floor: ensure it is stored on the fit (banked 08-10: ratio 3.4)
if (is.null(mx$zero_floor)) {
  say("zero floor missing on cached fit; computing (R = 2)")
  mx$zero_floor <- scmix_calibrate_zero(mx, R = 2L)
  saveRDS(mx, file.path(OUT_DIR, "scmix_fit_br2017.rds"))
}
slot("zero_floor", mx$zero_floor)

slot("design_check", scmix_design_check(mx, n_bins = 50L, seed = 7L))
slot("theta", scmix_theta(mx, n_bins = 50L, seed = 7L))
slot("pi", scmix_polarization(mx, n_bins = 50L, seed = 7L))
slot("theta_by_party", scmix_average(mx, by = party, n_bins = 50L, seed = 7L))

slot("slope_share",
     scmix_signshare(mx, list(progressivity_slope = slope_delta),
                     n_bins = 50L, seed = 7L))
slot("slope_share_by_party",
     scmix_signshare(mx, list(progressivity_slope = slope_delta),
                     by = party, n_bins = 50L, seed = 7L))
slot("tmb_share",
     scmix_signshare(mx, list(top_minus_bottom = tmb_delta),
                     n_bins = 50L, seed = 7L))

## mean slope by party: slope weights applied to the corrected theta signal
slot("party_slopes", {
  th <- slot_get("theta")
  v <- as.numeric(th$psi %*% stats::setNames(
    ifelse(mx$attr_names %in% RATE_COLS,
           slope_w[match(mx$attr_names, RATE_COLS)], 0), mx$attr_names))
  list(overall = data.frame(group = "overall", n = length(v),
                            estimate = mean(v),
                            se = stats::sd(v) / sqrt(length(v))),
       by_party = group_stats(v, party))
})

slot("importance",
     scmix_importance(mx, design = "levels", levels = LEVELS_FIXED,
                      n_bins = 50L, seed = 7L))
slot("importance_by_party",
     scmix_importance(mx, design = "levels", levels = LEVELS_FIXED,
                      by = party, n_bins = 50L, seed = 7L))

slot("plans_V",
     scmix_counterfactual(mx, contrast = plan_D, n_bins = 50L, seed = 7L))
slot("plans_du_share",
     scmix_signshare(mx, plan_D, n_bins = 50L, seed = 7L))
slot("plans_du_share_by_party",
     scmix_signshare(mx, plan_D, by = party, n_bins = 50L, seed = 7L))

## per-party V(c) for the plan panels: group means of the batch psi
slot("plans_V_by_party", {
  vc <- slot_get("plans_V")
  do.call(rbind, lapply(seq_len(ncol(vc$psi)), function(j) {
    gs <- group_stats(vc$psi[, j], party)
    gs$contrast <- rownames(plan_D)[j]
    gs
  }))
})

slot("posterior", scmix_posterior(mx, what = c("mean", "sd")))
slot("party", party)

## headline print for the log
Q <- .slot_env$Q
say("floor ratio: %.2f", Q$zero_floor$ratio)
say("theta revenue_score: %.4f (banked 0.1311)",
    Q$theta$estimate["revenue_score"])
say("pi revenue_score: %.3f [%.3f, %.3f] (banked 0.68 [0.59, 0.77])",
    Q$pi$estimate["revenue_score"],
    Q$pi$ci_lower[match("revenue_score", names(Q$pi$estimate))],
    Q$pi$ci_upper[match("revenue_score", names(Q$pi$estimate))])
say("progressivity slope share: %.3f [%.3f, %.3f] (v3 MAP fraction 0.93)",
    Q$slope_share$estimate, Q$slope_share$ci_lower, Q$slope_share$ci_upper)
say("top-minus-bottom share: %.3f (v3 0.91)", Q$tmb_share$estimate)
print(Q$design_check)
cat("\nDONE E2 br2017\n")
