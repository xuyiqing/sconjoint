#!/usr/bin/env Rscript
## Runner: structural AME_0 for Saha--Weeks (paperps Section 2 definition),
## on the v2.1 reproduction fit in the worktree. Independent-uniform design
## per the application's protocol record. PROVISIONAL pending audit.

options(stringsAsFactors = FALSE, warn = 1)
root <- path.expand("~/GitHub/sconjoint")
wt <- path.expand("~/GitHub/sconjoint-v21-repro")
suppressPackageStartupMessages(pkgload::load_all(wt, quiet = TRUE))
source(file.path(root, "applications/R/estimands_v21.R"))

dir <- file.path(wt, "applications/sw2022/results/mixed_logit_v2_1_postpilot_final")
out <- file.path(dir, "estimands")
dir.create(out, showWarnings = FALSE)

fit <- est_fit(file.path(dir, "fit_selected_full.rds"),
               file.path(wt, "applications/sw2022/results/prep_analysis_data.rds"))

attrs <- list(
  gender = "cand_genderMale",
  run = "cand_runYes",
  talent = c("cand_talentCollaborative", "cand_talentDetermined.to.Succeed",
             "cand_talentEmpathetic", "cand_talentGood.Communicator",
             "cand_talentHard.Working", "cand_talentTough.Negotiator"),
  agenda = c("cand_agendaModerate.Changes", "cand_agendaComplete.Overhaul"),
  children = c("cand_child1.child", "cand_child2.children",
               "cand_child3.children"))
stopifnot(setequal(unlist(attrs), fit$coord))

## End-to-end validation (memo Sec. 6 check i): the Table-4 agenda
## contests routed through est_V0 must reproduce the evidence chain's
## banked position-neutral probabilities on the same fit.
tab4 <- read.csv(file.path(dirname(dir), "postfit_evidence_v2_1",
                           "final", "tables",
                           "structural__position_neutral_choice.csv"))
tab4 <- tab4[tab4$group == "Overall", c("contest", "estimate")]
dvec <- function(her_agenda) {
  d <- setNames(numeric(length(fit$coord)), fit$coord)
  d["cand_genderMale"] <- -1
  d["cand_talentEmpathetic"] <- 1
  d["cand_talentTough.Negotiator"] <- -1
  d["cand_agendaComplete.Overhaul"] <- d["cand_agendaComplete.Overhaul"] - 1
  if (her_agenda == "complete") d["cand_agendaComplete.Overhaul"] <-
      d["cand_agendaComplete.Overhaul"] + 1
  if (her_agenda == "moderate") d["cand_agendaModerate.Changes"] <- 1
  d
}
t4_ok <- TRUE
for (ct in c("complete", "moderate", "very_few")) {
  got <- est_V0(fit, dvec(ct), neutral = TRUE, n_nodes = 45L)$value[["Overall"]]
  want <- tab4$estimate[tab4$contest == ct][1]
  dev <- abs(got - want)
  cat("TABLE4 CHECK", ct, ": est_V0", format(got, digits = 9),
      "banked", format(want, digits = 9), "dev", format(dev, digits = 3), "\n")
  if (!(dev < 1e-6)) t4_ok <- FALSE
}
cat("TABLE4 REPRODUCTION:", if (t4_ok) "PASS" else "FAIL", "\n")

res <- est_AME(fit, attrs, n_nodes = 31L, M_D = 20000L, seed = 20260827L)
res2 <- est_AME(fit, attrs, n_nodes = 31L, M_D = 40000L, seed = 20260828L)
stab <- merge(res$value, res2$value, by = "coordinate",
              suffixes = c("", "_2x"))
## Independent-seed replicates: the difference has variance
## mc_se^2 + mc_se_2x^2; gate at the Bonferroni 5% two-sided threshold
## across the 13 simultaneous coordinate comparisons.
stab$z <- abs(stab$ame - stab$ame_2x) /
  sqrt(stab$mc_se^2 + stab$mc_se_2x^2)
zcrit <- qnorm(1 - 0.025 / nrow(stab))
stab$stable <- stab$z <= zcrit

## Cross-check against the design AMCEs already banked by the v2.1
## post-fit (same contrasts where they align; near-pooled fit implies
## near-equality per the algorithm memo's check ii).
par_csv <- file.path(dir, "..", "postfit_evidence_v2_1", "final", "tables",
                     "design__amce_structural_parallel.csv")
par_csv <- normalizePath(file.path(dirname(dir), "postfit_evidence_v2_1",
                                   "final", "tables",
                                   "design__amce_structural_parallel.csv"))
amce <- read.csv(par_csv)
amce <- amce[amce$group == "Overall", c("contrast", "estimate_amce")]
map <- c(cand_agendaComplete.Overhaul = "agenda_complete_vs_very_few",
         cand_genderMale = "female_vs_male")
stab$design_amce <- NA_real_
for (k in names(map)) {
  row <- amce$contrast == map[[k]]
  if (any(row)) {
    v <- amce$estimate_amce[row][1]
    ## female_vs_male AMCE is on the female side; our coordinate is Male.
    if (k == "cand_genderMale") v <- -v
    stab$design_amce[stab$coordinate == k] <- v
  }
}
ok <- all(res$checks$pass) && all(stab$stable) && t4_ok
if (!ok) cat("CHECK FAILED: sw AME -- value withheld\n")
write.csv(stab, file.path(out, "ame_sw.csv"), row.names = FALSE)
write.csv(rbind(transform(res$checks, run = "M20k"),
                data.frame(check = "independent_replicates_bonferroni",
                           pass = all(stab$stable), run = "M40k")),
          file.path(out, "ame_checks.csv"), row.names = FALSE)
cat("SW AME DONE | all checks pass:", ok, "| max z:",
    round(max(stab$z), 2), "(crit", round(zcrit, 2), ")\n")
print(stab[, c("coordinate", "ame", "mc_se", "ame_2x", "design_amce")],
      digits = 3)
