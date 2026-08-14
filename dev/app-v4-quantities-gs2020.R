# E2 quantity menu: gs2020 (democracy) under the integrated estimator.
# Uses the E1 q = 1 fit; the q = 2/3 sensitivity sweep is separate
# evidence and does not change which fit this menu is computed on.
source("dev/app-v4-common.R")

mx <- readRDS(file.path(OUT_DIR, "scmix_fit_gs2020.rds"))
slots_open("gs2020")

PARTY <- "diff_respParty"
UNDEM <- c("diff_dem_code_u_banProtest", "diff_dem_code_u_court",
           "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
           "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
           "diff_dem_code_u_limitVote")
CD_ACTIONS <- c(journalists = "diff_dem_code_u_journalists",
                limitVote = "diff_dem_code_u_limitVote",
                court = "diff_dem_code_u_court")
MRS_ACTIONS <- c(journalists = "diff_dem_code_u_journalists",
                 court = "diff_dem_code_u_court",
                 gerry10 = "diff_dem_code_u_gerry10")

meta <- resp_meta_for("gs2020", mx)
stopifnot(!anyNA(meta$ideo7))
terc <- tercile_from_ideo7(meta$ideo7)
stopifnot(!anyNA(terc))

slot("zero_floor", mx$zero_floor)
slot("design_check", readRDS(file.path(OUT_DIR, "design_check_gs2020.rds")))
slot("theta", scmix_theta(mx, n_bins = 50L, seed = 7L))
slot("pi", scmix_polarization(mx, n_bins = 50L, seed = 7L))
slot("theta_by_tercile", scmix_average(mx, by = terc, n_bins = 50L, seed = 7L))

## population MRS with Fieller (v3: journalists 1.29 [1.04, 1.53],
## courts 1.04, gerry10 0.95, all vs the co-partisan bonus)
for (a in names(MRS_ACTIONS)) {
  slot(paste0("mrs_", a),
       scmix_mrs(mx, MRS_ACTIONS[[a]], PARTY, n_bins = 50L, seed = 7L))
}

## compensating differentials: P(beta_action + beta_party >= 0) by
## ideology tercile (v3 cells 38/66, 47/72, 46/65), now gated sign shares
cd_list <- lapply(CD_ACTIONS, function(d) {
  v <- c(1, 1)
  names(v) <- c(d, PARTY)
  v
})
names(cd_list) <- paste0("compdiff_", names(CD_ACTIONS))
slot("compdiff", scmix_signshare(mx, cd_list, n_bins = 50L, seed = 7L))
slot("compdiff_by_tercile",
     scmix_signshare(mx, cd_list, by = terc, n_bins = 50L, seed = 7L))

## majority-preference contests: co-partisan candidate takes action u
## against a clean opposing-party candidate (delta = e_party + e_u) --
## the sweep-endpoint versions of the v3 moderation panels
contest_list <- lapply(UNDEM, function(d) {
  v <- c(1, 1)
  names(v) <- c(d, PARTY)
  v
})
names(contest_list) <- paste0("contest_", sub("diff_dem_code_u_", "", UNDEM))
slot("contest_share", scmix_signshare(mx, contest_list,
                                      n_bins = 50L, seed = 7L))
slot("contest_V", scmix_counterfactual(mx, contrast = do.call(
  rbind, lapply(contest_list, function(v) {
    cv <- stats::setNames(numeric(length(mx$attr_names)), mx$attr_names)
    cv[names(v)] <- v
    cv
  })), n_bins = 50L, seed = 7L))

## per-tercile V(c): group means of the batch psi (the panel points)
slot("contest_V_by_tercile", {
  vc <- slot_get("contest_V")
  do.call(rbind, lapply(seq_len(ncol(vc$psi)), function(j) {
    gs <- group_stats(vc$psi[, j], terc)
    gs$contrast <- names(contest_list)[j]
    gs
  }))
})

slot("importance_by_tercile",
     scmix_importance(mx, design = "uniform", by = terc,
                      n_bins = 50L, seed = 7L))
slot("importance", scmix_importance(mx, design = "uniform",
                                    n_bins = 50L, seed = 7L))

## ridgeline inputs: conditional means and the model residual scale
slot("density_inputs", {
  resp_f <- factor(mx$respondent_id, levels = unique(mx$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  Sig <- Reduce(`+`, lapply(mx$A_folds, tcrossprod)) / length(mx$A_folds)
  list(mu_resp = mx$mu_hat[first, , drop = FALSE],
       sigma_k = sqrt(pmax(diag(Sig), 0)),
       attr_names = mx$attr_names)
})

slot("posterior", scmix_posterior(mx, what = c("mean", "sd")))
slot("tercile", terc)

Q <- .slot_env$Q
say("floor ratio: %.2f (threshold 2)", Q$zero_floor$ratio)
say("theta party: %.3f (two-stage canon 0.722)", Q$theta$estimate[PARTY])
undem_pi <- Q$pi$estimate[UNDEM]
say("undem pi: %d of 7 reported (rest NA)", sum(!is.na(undem_pi)))
for (a in names(MRS_ACTIONS)) {
  m <- slot_get(paste0("mrs_", a))
  say("MRS %s: %.3f [%.3f, %.3f] fieller [%.3f, %.3f]", a,
      m$estimate, m$ci_lower, m$ci_upper,
      m$extra$fieller_lo, m$extra$fieller_hi)
}
cd <- slot_get("compdiff_by_tercile")
lab <- names(cd$estimate)
lib_con <- grep("^(Liberal|Conservative): compdiff_", lab, value = TRUE)
say("compdiff cells (v3 38/66, 47/72, 46/65):")
for (l in lib_con) say("  %-42s %.3f", l, cd$estimate[l])
cat("\nDONE E2 gs2020\n")
