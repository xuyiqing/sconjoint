# E2 addendum: descriptive conditional-mean sign shares.
#
# Where the population sign share is gated NA (residual dispersion below
# the floor or unidentified loadings), the package's own guidance is
# that "the conditional-mean sign is the defensible directional
# summary." This computes those shares -- mean over respondents of
# 1{delta' mu(Z_i) > 0} -- as clearly-labeled DESCRIPTIVE quantities
# (Z-explained; no residual-law claim; no debiased CI, since the
# indicator is not a smooth functional).
source("dev/app-v4-common.R")

mu_first <- function(fit) {
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  mu <- fit$mu_hat[!duplicated(as.integer(resp_f)), , drop = FALSE]
  colnames(mu) <- fit$attr_names
  mu
}

## --- br2017 -----------------------------------------------------------------
mx <- readRDS(file.path(OUT_DIR, "scmix_fit_br2017.rds"))
slots_open("br2017")
mu <- mu_first(mx)
RATE_COLS <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
               "rate_175_375", "rate_375P")
log_mid_c <- log(c(5, 22.5, 60, 130, 275, 500))
log_mid_c <- log_mid_c - mean(log_mid_c)
slope_mu <- as.numeric(mu[, RATE_COLS] %*% log_mid_c) / sum(log_mid_c^2)
tmb_mu <- mu[, "rate_375P"] - mu[, "rate_L10"]
party <- slot_get("party")
slot("muZ_shares", list(
  note = "descriptive Z-explained shares; population sign shares are gated NA",
  slope_positive = mean(slope_mu > 0),
  slope_positive_by_party = tapply(slope_mu > 0, party, mean),
  top_gt_bottom = mean(tmb_mu > 0),
  top_gt_bottom_by_party = tapply(tmb_mu > 0, party, mean)))
say("br muZ: slope>0 %.3f (v3 MAP fraction 0.93), tmb>0 %.3f (v3 0.91)",
    mean(slope_mu > 0), mean(tmb_mu > 0))

## --- gs2020 -----------------------------------------------------------------
mx <- readRDS(file.path(OUT_DIR, "scmix_fit_gs2020.rds"))
slots_open("gs2020")
mu <- mu_first(mx)
UNDEM <- c("diff_dem_code_u_banProtest", "diff_dem_code_u_court",
           "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
           "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
           "diff_dem_code_u_limitVote")
share_opposed <- vapply(UNDEM, function(k) mean(mu[, k] < 0), numeric(1L))
share_positive <- vapply(UNDEM, function(k) mean(mu[, k] > 0), numeric(1L))
slot("muZ_shares", list(
  note = "descriptive Z-explained shares; population sign shares are gated NA",
  opposed_by_action = share_opposed,
  min_opposed = min(share_opposed),
  positive_by_action = share_positive,
  max_positive = max(share_positive)))
say("gs muZ: min opposed %.3f (v3 '>93%%'), max positive %.3f (v3 ~0.061)",
    min(share_opposed), max(share_positive))

## --- sw2022 -----------------------------------------------------------------
mx <- readRDS(file.path(OUT_DIR, "scmix_fit_sw2022.rds"))
slots_open("sw2022")
mu <- mu_first(mx)
party <- slot_get("party")
male_mu <- mu[, "cand_genderMale"]
slot("muZ_shares", list(
  note = "descriptive Z-explained shares; population sign shares are gated NA",
  prefer_male = mean(male_mu > 0),
  prefer_male_by_party = tapply(male_mu > 0, party, mean)))
say("sw muZ: prefer-male by party %s (v3 within-party MAP shares 66.5/65.7)",
    paste(sprintf("%.3f", tapply(male_mu > 0, party, mean)), collapse = " / "))

cat("\nDONE muZ shares\n")
