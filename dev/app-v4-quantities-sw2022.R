# E2 quantity menu: sw2022 (candidate, T = 3) under the integrated
# estimator. The design gates are the story here: the memo expects the
# within-party gender majority shares to come out NA.
source("dev/app-v4-common.R")

mx <- readRDS(file.path(OUT_DIR, "scmix_fit_sw2022.rds"))
slots_open("sw2022")

## party from the moderator dummies (fig_partisan_gender.R convention)
resp_f <- factor(mx$respondent_id, levels = unique(mx$respondent_id))
first <- !duplicated(as.integer(resp_f))
Zr <- mx$Z[first, , drop = FALSE]
party <- factor(ifelse(Zr[, "party_Republican"] == 1, "Republican",
                ifelse(Zr[, "party_Independent"] == 1, "Independent",
                       "Democrat")),
                levels = c("Democrat", "Independent", "Republican"))
stopifnot(!anyNA(party))

if (is.null(mx$zero_floor)) {
  say("zero floor missing on cached fit; computing (R = 2)")
  mx$zero_floor <- scmix_calibrate_zero(mx, R = 2L)
  saveRDS(mx, file.path(OUT_DIR, "scmix_fit_sw2022.rds"))
}
slot("zero_floor", mx$zero_floor)

slot("design_check", scmix_design_check(mx, n_bins = 50L, seed = 7L))
slot("theta", scmix_theta(mx, n_bins = 50L, seed = 7L))
slot("pi", scmix_polarization(mx, n_bins = 50L, seed = 7L))
slot("theta_by_party", scmix_average(mx, by = party, n_bins = 50L, seed = 7L))

## the headline gate: within-party gender majority shares (v3: 66.5/65.7
## or 68/69 depending on the fit) must clear the identification gates or
## report NA
slot("gender_share_by_party",
     scmix_signshare(mx, list(prefer_male = c(cand_genderMale = 1)),
                     by = party, n_bins = 50L, seed = 7L))
slot("gender_share_overall",
     scmix_signshare(mx, list(prefer_male = c(cand_genderMale = 1)),
                     n_bins = 50L, seed = 7L))

## agenda contrasts sit on the two identified coordinates (t = 9.7 / 3.9)
slot("agenda_share",
     scmix_signshare(mx, list(
       moderate_vs_none = c(cand_agendaModerate.Changes = 1),
       overhaul_vs_none = c(cand_agendaComplete.Overhaul = 1),
       overhaul_vs_moderate = c(cand_agendaComplete.Overhaul = 1,
                                cand_agendaModerate.Changes = -1)),
       n_bins = 50L, seed = 7L))
slot("agenda_share_by_party",
     scmix_signshare(mx, list(
       moderate_vs_none = c(cand_agendaModerate.Changes = 1),
       overhaul_vs_none = c(cand_agendaComplete.Overhaul = 1)),
       by = party, n_bins = 50L, seed = 7L))

slot("importance",
     scmix_importance(mx, design = "uniform", n_bins = 50L, seed = 7L))
slot("importance_by_party",
     scmix_importance(mx, design = "uniform", by = party,
                      n_bins = 50L, seed = 7L))

slot("fold_spread", fold_index_sd(mx))
slot("posterior", scmix_posterior(mx, what = c("mean", "sd")))
slot("party", party)

Q <- .slot_env$Q
say("floor ratio: %.2f (banked 1.7; threshold 2)", Q$zero_floor$ratio)
say("fold index-SD spread: %.2f - %.2f (banked 1.02-1.26)",
    min(Q$fold_spread), max(Q$fold_spread))
say("gender theta by party (Dem/Ind/Rep): %s",
    paste(sprintf("%.3f", Q$theta_by_party$estimate[
      paste(levels(party), "cand_genderMale", sep = ": ")]), collapse = " / "))
say("gender share by party: %s (memo expectation: NA under the gates)",
    paste(sprintf("%s=%.3f",
                  levels(party),
                  Q$gender_share_by_party$estimate[
                    paste(levels(party), "prefer_male", sep = ": ")]),
          collapse = " "))
print(Q$design_check)
cat("\nDONE E2 sw2022\n")
