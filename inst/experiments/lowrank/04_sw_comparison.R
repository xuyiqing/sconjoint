## 04_sw_comparison.R -- Headline-stability comparison for the Saha-Weeks
## (2022) application: does the attribute-interaction extension change the
## paper's main SW findings?
##
## Branch: feat/lowrank-interaction.  Run from the worktree root:
##   Rscript inst/experiments/lowrank/04_sw_comparison.R
##
## Arms (same data, same seed 42, same config; only `interactions` differs):
##   arm0_s42 : interactions = "none"      -- baseline (production-equivalent)
##   arm0_s7  : interactions = "none", seed 7 -- two-seed ensemble noise scale
##              (doubles as the reproducibility check)
##   arm1_exp : interactions = "explicit"  (identified pairwise features, ridge)
##   arm2_lr2 : interactions = "lowrank", interaction_rank = 2, default lambda_V
##
## Configuration mirrors the production SW run
## (code/30_setup_saha_weeks.R + 32_run_pipeline_saha_weeks.R) as closely as
## the scfit() API allows:
##   K = 10 folds, hidden auto -> c(32,32,16), lr = 0.01, n_epochs = 1000 (v13),
##   weight_decay = "adaptive" (v13 rule), ridge_lambda = 1e-4, seed = 42,
##   stage2 = "map_c5" (paper EnsC5), normalize_deltaX = FALSE.
## Differences from production (unavoidable with the shipped sw2022 data):
##   * Z = (resp_female, std age, pid dummies) -- the shipped moderators;
##     production Z also had income/education/region/employment/ideology/
##     2016 vote/gender attitudes.  So arm0_s42 is the *within-experiment*
##     baseline; paper canon is quoted for orientation only.
##   * Shipped cand_gender reference is Male, so the dummy is
##     cand_genderFemale = the female-candidate coefficient directly
##     (production used cand_genderMale; canon party means of beta_Male were
##     Dem -0.334 / Ind -0.116 / Rep +0.233, i.e. Female ~ +0.33/+0.12/-0.23).
##
## Output:
##   04_cache_<arm>.rds          per-arm extracted quantities (gitignored)
##   results_sw_comparison.rds   comparison table + interaction summaries
##                               (committed)

suppressMessages(devtools::load_all(".", quiet = TRUE))

exp_dir <- "inst/experiments/lowrank"
stopifnot(dir.exists(exp_dir))

## --- Data ---------------------------------------------------------------
load("data/sw2022.rda")
sw <- sw2022
sw$age_std <- as.numeric(scale(sw$age))            # production standardized age
ATTRS <- c("agenda", "talent", "children", "cand_gender", "prior_office")
stopifnot(all(vapply(sw[ATTRS], is.factor, logical(1))))   # factor-only design:
## interactions != "none" requires all-factor attributes -- verified here.
cat("sw2022:", nrow(sw), "rows,", length(unique(sw$respondent)), "respondents\n")

## Respondent-level party lookup (Democrat / Independent / Republican)
resp_tab <- unique(sw[, c("respondent", "pid")])
party_of <- setNames(
  ifelse(resp_tab$pid == "Republican (GOP)", "Republican",
         ifelse(resp_tab$pid == "Independent", "Independent", "Democrat")),
  resp_tab$respondent)

GENDER_DUMMY <- "cand_genderFemale"

## --- Quantity extraction -------------------------------------------------
heldout_logloss <- function(fit) {
  ## Cross-fitted: Stage-1 out-of-fold beta_hat_dnn + Stage-1 held-out
  ## interaction offset (g_offset).  The diagonal of VV' is already absorbed
  ## into beta_hat, so index = deltaX'beta + g_offset is the full index.
  idx <- rowSums(fit$deltaX * fit$beta_hat_dnn)
  if (!is.null(fit$interaction)) idx <- idx + fit$interaction$g_offset
  pr <- pmin(pmax(stats::plogis(idx), 1e-12), 1 - 1e-12)
  -mean(fit$y * log(pr) + (1 - fit$y) * log(1 - pr))
}

extract_arm <- function(fit, label, runtime_min) {
  k <- GENDER_DUMMY
  th  <- fit$theta[k]
  se  <- sqrt(diag(fit$vcov))[k]

  ## (1) Importance shares, production formula for factor designs
  ##     (design = "uniform": SW canon agenda 52 / talent 21 / gender 17)
  imp <- sc_importance(fit, design = "uniform")$estimate
  gender_share <- imp$share[imp$attribute == "cand_gender"]

  ## (2) Party means of respondent-level beta_i (female-candidate coef)
  first <- !duplicated(fit$respondent_id)
  b_resp <- fit$beta_hat[first, k]
  party  <- factor(party_of[fit$respondent_id[first]],
                   levels = c("Democrat", "Independent", "Republican"))
  stopifnot(!anyNA(party), length(b_resp) == 1191L)
  pm <- tapply(b_resp, party, mean)

  ## (4) Fraction preferring a female candidate (beta_iF > 0)
  fr <- sc_fraction_preferring(fit)$estimate
  frow <- fr[fr$dummy_name == k, ]

  ## (5) Counterfactual contest: Ambitious woman vs Ambitious man
  ##     (production matchup, 32_run_pipeline 8.2)
  A <- list(cand_gender = "Female", prior_office = "Yes",
            talent = "Hard-Working", agenda = "Complete Overhaul",
            children = "No children")
  B <- modifyList(A, list(cand_gender = "Male"))
  cf_orth <- withCallingHandlers(
    sc_counterfactual(fit, A, B),                  # orthogonal (debiased)
    warning = function(w) { message("  [cf warning] ", conditionMessage(w))
                            invokeRestart("muffleWarning") })
  cf_plug <- sc_counterfactual(fit, A, B, vartype = "plugin")

  ## Interaction-coefficient summary (arms 1-2 only)
  int_sum <- NULL
  if (!is.null(fit$interaction)) {
    it <- data.frame(term  = fit$interaction$feature_names,
                     theta = unname(fit$interaction$theta),
                     se    = unname(fit$interaction$se))
    it$z <- it$theta / it$se
    int_sum <- list(
      table        = it,
      n_features   = fit$interaction$n_features,
      n_dropped    = fit$interaction$n_dropped_nosupport,
      n_z_gt2      = sum(abs(it$z) > 2),
      expected_5pct = round(0.05 * nrow(it), 1),
      max_abs_theta = it[which.max(abs(it$theta)), ],
      max_abs_z     = it[which.max(abs(it$z)), ]
    )
  }

  list(
    label   = label,
    runtime_min = runtime_min,
    quantities = c(
      gender_importance_pct = 100 * gender_share,
      beta_F_mean_Dem  = unname(pm["Democrat"]),
      beta_F_mean_Ind  = unname(pm["Independent"]),
      beta_F_mean_Rep  = unname(pm["Republican"]),
      theta_F          = unname(th),
      theta_F_se       = unname(se),
      theta_F_ci_lo    = unname(th - 1.96 * se),
      theta_F_ci_hi    = unname(th + 1.96 * se),
      frac_prefer_female_pct = 100 * frow$frac_positive,
      cf_AmbW_vs_AmbM_orth   = cf_orth$estimate,
      cf_AmbW_vs_AmbM_orth_lo = cf_orth$ci_lo,
      cf_AmbW_vs_AmbM_orth_hi = cf_orth$ci_hi,
      cf_AmbW_vs_AmbM_plugin  = cf_plug$estimate,
      heldout_logloss  = heldout_logloss(fit)
    ),
    importance_table = imp,
    party_n     = table(party),
    interaction = int_sum
  )
}

## --- Arm runner with cache ------------------------------------------------
run_arm <- function(label, interactions, seed, interaction_rank = 2L) {
  cache <- file.path(exp_dir, paste0("04_cache_", label, ".rds"))
  if (file.exists(cache)) {
    cat("[", label, "] cached -- skipping refit\n")
    return(readRDS(cache))
  }
  cat("[", label, "] fitting: interactions =", interactions,
      " seed =", seed, "\n")
  t0 <- Sys.time()
  fit <- scfit(
    choice ~ agenda + talent + children + cand_gender + prior_office |
      resp_female + age_std + pid,
    data = sw,
    respondent = "respondent", task = "task", profile = "profile",
    K = 10L,                       # production K_FOLDS
    n_epochs = 1000L,              # production v13 (memo 40)
    learning_rate = 0.01,          # production
    hidden = "auto",               # -> c(32,32,16) = production at this N
    weight_decay = "adaptive",     # production v13 rule
    ridge_lambda = 1e-4,           # production RIDGE_PENALTY
    seed = seed,
    stage2 = "map_c5",             # paper EnsC5 (default stage2_seed 12345)
    interactions = interactions,
    interaction_rank = interaction_rank,
    lambda_V = 1e-2,               # default ridge on the interaction head
    keep_modules = FALSE,
    verbose = FALSE
  )
  rt <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat("[", label, "] done in", round(rt, 1), "min\n")
  stopifnot(identical(fit$hidden, c(32L, 32L, 16L)),
            length(fit$theta) == 13L,
            isFALSE(fit$normalize_deltaX))
  out <- extract_arm(fit, label, rt)
  saveRDS(out, cache)
  out
}

arms <- list(
  arm0_s42 = run_arm("arm0_s42", "none",     42L),
  arm0_s7  = run_arm("arm0_s7",  "none",      7L),
  arm1_exp = run_arm("arm1_exp", "explicit", 42L),
  arm2_lr2 = run_arm("arm2_lr2", "lowrank",  42L, interaction_rank = 2L)
)

## --- Comparison table ------------------------------------------------------
qn   <- names(arms$arm0_s42$quantities)
tab  <- sapply(arms, function(a) a$quantities[qn])
rownames(tab) <- qn

base  <- tab[, "arm0_s42"]
noise <- abs(tab[, "arm0_s7"] - base)              # two-seed ensemble noise
diffs <- abs(tab[, c("arm1_exp", "arm2_lr2"), drop = FALSE] - base)
within <- sweep(diffs, 1, noise, "<=")

cat("\n================ SW headline comparison (arm x quantity) ===============\n")
print(round(tab, 4))
cat("\n--- |arm - arm0_s42| vs two-seed noise |arm0_s7 - arm0_s42| ---\n")
cmp <- data.frame(round(diffs, 4), noise = round(noise, 4),
                  within_noise_exp = within[, "arm1_exp"],
                  within_noise_lr2 = within[, "arm2_lr2"])
print(cmp)

cat("\n--- Interaction-coefficient summaries ---\n")
for (lab in c("arm1_exp", "arm2_lr2")) {
  s <- arms[[lab]]$interaction
  cat(sprintf("%s: %d identified terms (%d dropped, no support); |z|>2: %d (expect ~%.1f by chance);\n",
              lab, s$n_features, s$n_dropped, s$n_z_gt2, s$expected_5pct))
  with(s$max_abs_theta, cat(sprintf(
    "  largest |theta|: %s = %+.3f [%+.3f, %+.3f] (z = %+.2f)\n",
    term, theta, theta - 1.96 * se, theta + 1.96 * se, z)))
  with(s$max_abs_z, cat(sprintf(
    "  largest |z|:     %s = %+.3f [%+.3f, %+.3f] (z = %+.2f)\n",
    term, theta, theta - 1.96 * se, theta + 1.96 * se, z)))
}

cat("\nRuntime (min):",
    paste(sprintf("%s %.1f", names(arms),
                  vapply(arms, `[[`, numeric(1), "runtime_min")),
          collapse = " | "), "\n")

saveRDS(list(table = tab, noise = noise, diffs = diffs, within = within,
             arms = arms,
             config = list(K = 10L, n_epochs = 1000L, learning_rate = 0.01,
                           hidden = c(32L, 32L, 16L), weight_decay = "adaptive",
                           ridge_lambda = 1e-4, stage2 = "map_c5",
                           seeds = c(42L, 7L), lambda_V = 1e-2,
                           interaction_rank = 2L)),
        file.path(exp_dir, "results_sw_comparison.rds"))
cat("Saved", file.path(exp_dir, "results_sw_comparison.rds"), "\n")
