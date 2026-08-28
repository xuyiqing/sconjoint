#!/usr/bin/env Rscript
## Runner: main-text estimand ports on the v2.1 fits.
## Usage: Rscript applications/R/run_estimands_v21.R <br2017|gs2020>
## Writes results/mixed_logit/v21_corrected/estimands/*.csv plus a
## checks.csv per application; a failed check withholds the number.
## Status: PROVISIONAL --- algorithm memo pending author verification.

options(stringsAsFactors = FALSE, warn = 1)
app <- commandArgs(trailingOnly = TRUE)[[1]]
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/estimands_v21.R"))

dir <- file.path(root, "applications", app, "results/mixed_logit/v21_corrected")
out <- file.path(dir, "estimands")
## Stale-artifact guard (code audit): wipe prior outputs so an aborted
## run can never leave a ledger asserting checks that no longer pass.
unlink(file.path(out, "*"))
dir.create(out, showWarnings = FALSE)
fit <- est_fit(file.path(dir, "fit_primary_full.rds"),
               file.path(root, "applications", app, "results/prep_analysis_data.rds"))
## Cross-check mu column identity against the banked plugin thetas.
inf_path <- file.path(dir, "inference_summary.csv")
if (file.exists(inf_path)) {
  inf0 <- read.csv(inf_path)
  plug <- setNames(inf0$plugin, sub("^theta:", "", inf0$label))
  common <- intersect(names(plug), fit$coord)
  ## Loose identity guard: the banked plugin thetas are CROSS-FITTED
  ## means, the loaded mu is the full-sample refit, so they agree to
  ## ~5e-3, not machine precision (measured: br 6e-4, gs 5e-3). A
  ## permuted or wrong-fit mu gives near-zero correlation and O(0.5)
  ## deviations, which is what this catches.
  stopifnot(length(common) == length(fit$coord),
            max(abs(colMeans(fit$mu)[common] - plug[common])) < 0.05,
            cor(colMeans(fit$mu)[common], plug[common]) > 0.99)
}
stopifnot(est_importance_selftest())
all_checks <- list()
keep <- function(res, nm) {
  all_checks[[nm]] <<- transform(res$checks, estimand = nm)
  if (!all(res$checks$pass)) cat("CHECK FAILED:", nm, "-- value withheld\n")
  all(res$checks$pass)
}
## The checks ledger is written even if a later block errors out.
on.exit({
  led <- do.call(rbind, all_checks)
  if (!is.null(led))
    write.csv(led, file.path(out, "checks.csv"), row.names = FALSE)
}, add = TRUE)
write_rows <- function(rows, path) {
  df <- do.call(rbind, rows)
  if (is.null(df)) cat("NO ROWS SURVIVED for", basename(path), "\n")
  else write.csv(df, path, row.names = FALSE)
}

if (app == "br2017") {
  meta <- fit$meta[match(fit$resp_id, as.character(fit$meta$respondent_id)), ]
  stopifnot(!anyNA(meta$pid7))
  ## pid7 code 8 = "not sure / something else" (75 respondents): excluded
  ## from party subgroups, as in the submitted draft's three-party splits.
  party <- cut(meta$pid7, c(0, 3, 4, 7),
               labels = c("Democrat", "Independent", "Republican"))
  classified <- !is.na(party)
  cat("party-unclassified respondents excluded from subgroups:",
      sum(!classified), "\n")

  ## fig:br_schedule_by_party successor: subgroup structural means,
  ## computed on the classified subset so the aggregation check is exact.
  fitc <- est_subset(fit, classified)
  tb <- est_theta_B(fitc, droplevels(party[classified]))
  if (keep(tb, "theta_by_party")) {
    df <- do.call(rbind, lapply(names(tb$value), function(g)
      data.frame(party = g, coordinate = fit$coord, mean = tb$value[[g]])))
    df <- rbind(df, data.frame(party = "Overall (classified)",
                               coordinate = fit$coord, mean = tb$overall))
    write.csv(df, file.path(out, "theta_by_party.csv"), row.names = FALSE)
  }

  ## fig:br_plans_counterfactual: the four plans, submitted definitions,
  ## raw-rate + revenue-score units (verified against prepared ranges).
  plans <- list(`Steeply progressive` = c(0, 5, 15, 25, 35, 45),
                `Status-quo analog`   = c(5, 15, 25, 25, 35, 35),
                `Flat 15%`            = c(15, 15, 15, 15, 15, 15),
                `Regressive`          = c(25, 25, 15, 15, 5, 5))
  plan_rev <- c(`Steeply progressive` = 2, `Status-quo analog` = 0,
                `Flat 15%` = 0, `Regressive` = -2)
  stopifnot(identical(fit$coord,
    c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
      "rate_175_375", "rate_375P", "revenue_score")))
  ## Raw-unit assertion (code audit): rate contrasts must span raw
  ## percentage points and the revenue score its -2..2 scale; a future
  ## prep rescaling fails here instead of silently rescaling the plans.
  prep0 <- readRDS(file.path(root, "applications", app,
                             "results/prep_analysis_data.rds"))
  rng <- apply(abs(prep0$deltaX), 2, max)
  stopifnot(all(rng[1:6] >= 20 & rng[1:6] <= 55), rng[7] <= 4 + 1e-9)
  plan_vec <- function(p) c(plans[[p]], plan_rev[[p]])
  rows <- list(); srows <- list()
  for (opp in c("Flat 15%", "Status-quo analog", "Regressive")) {
    d <- plan_vec("Steeply progressive") - plan_vec(opp)
    v <- est_V0(fit, d, neutral = TRUE, n_nodes = 45L, groups = party)
    s <- est_S0(fit, d, groups = party)
    nm <- paste0("plan_vs_", gsub("[^A-Za-z]", "", opp))
    if (keep(v, paste0(nm, "_V0n")))
      rows[[opp]] <- data.frame(opponent = opp, group = names(v$value),
                                win_prob = as.numeric(v$value))
    if (keep(s, paste0(nm, "_S0")))
      srows[[opp]] <- data.frame(opponent = opp, group = names(s$value),
                                 share_prefer = as.numeric(s$value),
                                 floored = s$floored)
  }
  write_rows(rows, file.path(out, "plans_V0n.csv"))
  write_rows(srows, file.path(out, "plans_S0.csv"))

  ## Appendix-only in the submitted draft but cheap here: importance port.
  ## Level-set variances mean((l-mean(l))^2), sets per the archived code;
  ## computed from the sets (single source of truth).
  lsets <- list(rate_L10 = c(0,5,15,25), rate_10_35 = c(5,15,25,35),
    rate_35_85 = c(5,15,25,35), rate_85_175 = c(5,15,25,35),
    rate_175_375 = c(5,15,25,35,45), rate_375P = c(5,15,25,35,45,55),
    revenue_score = c(-2,-1,0,1,2))
  lv <- vapply(lsets, function(l) mean((l - mean(l))^2), 0)
  im <- est_importance(fitc, spec = lv[fit$coord], mode = "numeric",
                       groups = droplevels(party[classified]))
  if (keep(im, "importance_by_party")) {
    df <- do.call(rbind, lapply(names(im$value), function(g)
      data.frame(group = g, coordinate = names(im$value[[g]]),
                 share = as.numeric(im$value[[g]]))))
    write.csv(df, file.path(out, "importance_by_party.csv"), row.names = FALSE)
  }
}

if (app == "gs2020") {
  meta <- fit$meta[match(fit$resp_id, as.character(fit$meta$respondent_id)), ]
  stopifnot(!anyNA(meta$ideo7))
  ideo <- cut(meta$ideo7, c(0, 3, 4, 7),
              labels = c("Liberal", "Moderate", "Conservative"))

  U_COLS <- grep("^diff_dem_code_u_", fit$coord, value = TRUE)
  PARTY <- "diff_respParty"

  ## fig:gs_compdiff: compensating-benefit shares C_0(e_u, e_party; 1),
  ## the manuscript's construct; old code: frac(beta_u + beta_party >= 0).
  cd <- list()
  for (u in U_COLS) {
    cp <- as.numeric(fit$coord == u)
    cb <- as.numeric(fit$coord == PARTY)
    r <- est_C0(fit, cp, cb, a = 1, groups = ideo)
    if (keep(r, paste0("compdiff_", u)))
      cd[[u]] <- data.frame(action = sub("diff_dem_code_u_", "", u),
                            group = names(r$value),
                            share = as.numeric(r$value),
                            s2 = r$s2, floored = r$floored)
  }
  write_rows(cd, file.path(out, "compdiff_C0.csv"))

  ## fig:gs_importance: the production-35 grouped construct, by ideology.
  G_COLS <- grep("^diff_dem_code_g_", fit$coord, value = TRUE)
  V_COLS <- grep("^diff_dem_code_v_", fit$coord, value = TRUE)
  P_COLS <- c("diff_p1_num", "diff_p2_num")
  ATTR_GROUPS <- list(party = PARTY, policy = P_COLS, dem_good = G_COLS,
    dem_undem = U_COLS, dem_val = V_COLS, sex = "diff_sex_Female",
    race = grep("^diff_race_", fit$coord, value = TRUE),
    profession = grep("^diff_pro_", fit$coord, value = TRUE))
  im <- est_importance(fit, spec = ATTR_GROUPS, mode = "categorical",
                       groups = ideo)
  write.csv(data.frame(respondent = fit$resp_id, im$respondent_shares),
            file.path(out, "importance_respondent_shares.csv"),
            row.names = FALSE)
  if (keep(im, "importance_groups")) {
    df <- do.call(rbind, lapply(names(im$value), function(g)
      data.frame(group = g, bucket = names(im$value[[g]]),
                 share = as.numeric(im$value[[g]]))))
    write.csv(df, file.path(out, "importance_groups.csv"), row.names = FALSE)
  }

  ## Subgroup means by ideology (for the update's exhibits).
  tb <- est_theta_B(fit, ideo)
  if (keep(tb, "theta_by_ideology")) {
    df <- do.call(rbind, lapply(names(tb$value), function(g)
      data.frame(ideology = g, coordinate = fit$coord,
                 mean = tb$value[[g]])))
    write.csv(df, file.path(out, "theta_by_ideology.csv"), row.names = FALSE)
  }

  ## MRS points from one-step thetas (delta-method interval deferred:
  ## the runner has per-coordinate diagnostic SEs, not the joint block).
  inf <- read.csv(file.path(dir, "inference_summary.csv"))
  th <- setNames(inf$estimate, sub("^theta:", "", inf$label))
  se <- setNames(inf$diagnostic_se, sub("^theta:", "", inf$label))
  mr <- list()
  for (u in c("diff_dem_code_u_journalists", "diff_dem_code_u_court",
              "diff_dem_code_u_gerry10")) {
    r <- est_mrs(as.list(th), u, "diff_respParty",
                 den_se = se[["diff_respParty"]])
    if (keep(r, paste0("mrs_", u)))
      mr[[u]] <- data.frame(action = sub("diff_dem_code_u_", "", u),
                            mrs_signed = r$value[["mrs"]],
                            mrs_abs = r$value[["abs"]],
                            denominator_t = r$value[["den_t"]])
  }
  write_rows(mr, file.path(out, "mrs_points.csv"))

  ## fig:gs_moderation: the three counterfactual panels, exact archived
  ## contrast grids (fig_revisions.R): co-partisan A vs clean B, with
  ## (A) a social-policy sweep diff_p2 = pos-3, (B) one undemocratic
  ## action each, (C) journalists plus the policy sweep. All linear
  ## contrasts -> the audited V_0^n path.
  dx0 <- setNames(numeric(length(fit$coord)), fit$coord)
  co_dx <- function() { d <- dx0; d["diff_respParty"] <- 1; d }
  mod <- list()
  add_mod <- function(panel, xval, d, nm) {
    r <- est_V0(fit, d, neutral = TRUE, n_nodes = 45L, groups = ideo)
    if (keep(r, nm))
      mod[[nm]] <<- data.frame(panel = panel, x = xval,
                               group = names(r$value),
                               p_win = as.numeric(r$value))
  }
  for (pos in 0:4) {
    d <- co_dx(); d["diff_p2_num"] <- pos - 3
    add_mod("A", pos, d, paste0("modA_pos", pos))
    d["diff_dem_code_u_journalists"] <- 1
    add_mod("C", pos, d, paste0("modC_pos", pos))
  }
  for (u in U_COLS) {
    d <- co_dx(); d[u] <- 1
    add_mod("B", sub("diff_dem_code_u_", "", u), d, paste0("modB_", u))
  }
  write_rows(mod, file.path(out, "moderation_V0n.csv"))
}

cat("ESTIMANDS DONE", app, "| checks:",
    sum(vapply(all_checks, function(x) all(x$pass), TRUE)), "of",
    length(all_checks), "estimand blocks fully passing\n")
