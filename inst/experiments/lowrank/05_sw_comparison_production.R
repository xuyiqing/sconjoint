## 05_sw_comparison_production.R -- Headline-stability comparison for the
## Saha-Weeks (2022) application on the PRODUCTION analysis data: does the
## attribute-interaction extension change the paper's main SW findings?
##
## Branch: feat/lowrank-interaction.  Run from the worktree root:
##   Rscript inst/experiments/lowrank/05_sw_comparison_production.R
##
## Why this script exists: 04_sw_comparison.R used the package-bundled
## data/sw2022.rda, whose Z has only 3 of the 19 production respondent
## moderators -- its baseline does NOT reproduce the paper (gender importance
## 2.6% vs canon 16.5%; party means of beta_gender far from canon).  This
## script rebuilds the data exactly as production does and verifies the
## baseline against the paper's canonical numbers before running the arms.
##
## Data (production, loaded -- not re-derived -- per the saved-prep-first
## rule):
##   ConjointStructural/code/yiqing/sw2022/out/prep_long.rds      (scfit input)
##   ConjointStructural/code/yiqing/sw2022/out/prep_matrices.rds  (DeltaX/Y/Z
##     + resp_meta; itself bit-gated against the saved production run by
##     04_data_prep.R: rowSums(DeltaX * saved beta_hat) == saved logit_index)
## prep_long carries the 13 production attribute dummies as NUMERIC columns;
## the interaction arms need FACTOR attributes, so we reconstruct the five
## factors (production reference levels: Female / No / Assertive / Very Few
## Changes / No children) and gate bit-level:
##   gate 1: model.matrix on the reconstructed factors == the 13 dummy columns
##   gate 2: DeltaX / Y / Z recomputed from prep_long == prep_matrices.rds
## Z = all 19 production moderators (gender, age, income, education, party,
## region, employment, ideology, 2016 vote, gender attitudes).
##
## Arms (same data, same config; only `interactions` and the seed differ):
##   arm0_s42 : interactions = "none", seed 42 -- production-equivalent baseline
##   arm0_s7  : interactions = "none", seed 7  -- two-seed ensemble noise scale
##   arm1_exp : interactions = "explicit", seed 42
##   arm2_lr2 : interactions = "lowrank", interaction_rank = 2, seed 42
##
## Configuration mirrors the production SW run (code/30_setup_saha_weeks.R +
## 32_run_pipeline_saha_weeks.R, package runner code/yiqing/05_fit.R):
##   K = 10 folds, hidden auto -> c(32,32,16), lr = 0.01, n_epochs = 1000 (v13),
##   weight_decay = "adaptive" (v13 rule), ridge_lambda = 1e-4,
##   stage2 = "map_c5" (paper EnsC5, stage2_seed 12345),
##   normalize_deltaX = FALSE, parallel over folds (seed-stream reproducible).
##
## Sign convention: production codes gender as cand_genderMale (reference =
## Female), so canon party means of beta_Male are Dem -0.334 / Ind -0.116 /
## Rep +0.233 and canon theta_Male = -0.11 [-0.24, 0.02].  The comparison
## table reports the FEMALE-coefficient convention (beta_F = -beta_Male,
## theta_F = -theta_Male) for continuity with the 04 bundled-data report;
## the canon gate runs on the raw male-coded values.
##
## Baseline gate (arm0_s42 must qualitatively reproduce paper canon):
##   gender importance ~16.5% (saved 16.47); beta_Male party means near
##   -0.334 / -0.116 / +0.233; debiased theta_Male CI covering zero.
##   Fresh fits won't match the saved run exactly (the yiqing/sw2022
##   seed-42 package rerun landed at importance 16.05, Dem -0.270,
##   Ind -0.161, Rep +0.227); the gate stops the arms if arm0 strays
##   beyond |importance| 1.5pp / |party mean| 0.10 / |theta| 0.08 / CI
##   excluding zero.
##
## Output:
##   05_cache_<arm>.rds                     per-arm quantities (gitignored)
##   results_sw_comparison_production.rds   comparison table + interaction
##                                          summaries (committed)

suppressMessages(devtools::load_all(".", quiet = TRUE))

exp_dir <- "inst/experiments/lowrank"
stopifnot(dir.exists(exp_dir))

## --- Data: load the production prep ---------------------------------------
PROD_OUT <- "/Users/xyq/Dropbox/Projects/ConjointStructural/code/yiqing/sw2022/out"
stopifnot(dir.exists(PROD_OUT))
long <- readRDS(file.path(PROD_OUT, "prep_long.rds"))
mats <- readRDS(file.path(PROD_OUT, "prep_matrices.rds"))
Z_COLS <- colnames(mats$Z)
stopifnot(length(Z_COLS) == 19L, all(Z_COLS %in% names(long)),
          nrow(long) == 7146L)

## Production factor levels (reference first; code/30_setup_saha_weeks.R)
FACTOR_LEVELS <- list(
  cand_gender = c("Female", "Male"),
  cand_run    = c("No", "Yes"),
  cand_talent = c("Assertive", "Collaborative", "Determined to Succeed",
                  "Empathetic", "Good Communicator", "Hard-Working",
                  "Tough Negotiator"),
  cand_agenda = c("Very Few Changes", "Moderate Changes", "Complete Overhaul"),
  cand_child  = c("No children", "1 child", "2 children", "3 children")
)
DUMMY_COLS <- list(
  cand_gender = "cand_genderMale",
  cand_run    = "cand_runYes",
  cand_talent = c("cand_talentCollaborative", "cand_talentDetermined.to.Succeed",
                  "cand_talentEmpathetic", "cand_talentGood.Communicator",
                  "cand_talentHard.Working", "cand_talentTough.Negotiator"),
  cand_agenda = c("cand_agendaModerate.Changes", "cand_agendaComplete.Overhaul"),
  cand_child  = c("cand_child1.child", "cand_child2.children", "cand_child3.children")
)
ATTRS <- names(FACTOR_LEVELS)

## Reconstruct the factor attributes from the production dummy columns
recon_factor <- function(lev, cols) {
  M <- as.matrix(long[, cols, drop = FALSE])
  stopifnot(all(M %in% c(0, 1)), all(rowSums(M) <= 1))
  factor(lev[as.integer(M %*% seq_along(cols)) + 1L], levels = lev)
}
sw <- long[, c("respondent", "task", "profile", "choice", Z_COLS)]
for (a in ATTRS) sw[[a]] <- recon_factor(FACTOR_LEVELS[[a]], DUMMY_COLS[[a]])
stopifnot(!anyNA(sw[ATTRS]))

## Gate 1: re-encoding the factors reproduces the production dummies bit-level
X_chk <- model.matrix(
  ~ cand_gender + cand_run + cand_talent + cand_agenda + cand_child, sw)[, -1]
colnames(X_chk) <- make.names(colnames(X_chk))
stopifnot(identical(colnames(X_chk), mats$dummy_names),
          max(abs(X_chk - as.matrix(long[, mats$dummy_names]))) == 0)
cat("gate 1 OK: factor re-encoding == production dummy columns (bit-level)\n")

## Gate 2: DeltaX / Y / Z recomputed from prep_long == prep_matrices.rds
pa <- long[long$profile == 1L, ]; pb <- long[long$profile == 2L, ]
ka <- paste(pa$respondent, pa$task)
stopifnot(identical(ka, paste(pb$respondent, pb$task)))
ii <- match(paste(mats$respondent, mats$task), ka)
stopifnot(!anyNA(ii))
dX <- as.matrix(pa[, mats$dummy_names]) - as.matrix(pb[, mats$dummy_names])
stopifnot(max(abs(dX[ii, ] - mats$DeltaX)) == 0,
          max(abs(pa$choice[ii] - mats$Y)) == 0,
          max(abs(as.matrix(pa[ii, Z_COLS]) - mats$Z)) == 0)
cat("gate 2 OK: DeltaX / Y / Z bit-match prep_matrices.rds\n")
cat("production data:", nrow(sw), "rows,",
    length(unique(sw$respondent)), "respondents, |Z| =", length(Z_COLS), "\n")

## Respondent-level party lookup (resp_meta from the production prep)
party_of <- setNames(
  ifelse(mats$resp_meta$party == "Republican (GOP)", "Republican",
         ifelse(mats$resp_meta$party == "Independent", "Independent",
                "Democrat")),
  mats$resp_meta$respondent)

GENDER_DUMMY <- "cand_genderMale"   # production coding: reference = Female

## --- Canonical numbers (paper section 4.3 / CONTRACT, saved production run)
CANON <- list(
  importance_gender_pct = 16.47,                 # paper text: 16.5%
  beta_male_party = c(Democrat = -0.334, Independent = -0.116,
                      Republican = 0.233),       # paper: -0.33 / -0.12 / +0.23
  theta_male = -0.108, theta_male_ci = c(-0.2366, 0.0206),  # paper: -0.11 [-0.24, 0.02]
  frac_dem_prefer_female = 0.68, frac_rep_prefer_male = 0.69,
  frac_ind_prefer_female = 0.57,
  party_n = c(Democrat = 403, Independent = 397, Republican = 391)
)

## --- Quantity extraction ---------------------------------------------------
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
  th_m <- fit$theta[k]                       # male-coded (production)
  se   <- sqrt(diag(fit$vcov))[k]

  ## (1) Importance shares, production formula for factor designs
  ##     (design = "uniform": SW canon agenda 52 / talent 21 / gender 16.5)
  imp <- sc_importance(fit, design = "uniform")$estimate
  gender_share <- imp$share[imp$attribute == "cand_gender"]

  ## (2) Party means of respondent-level beta_i,Male (production grouping)
  first  <- !duplicated(fit$respondent_id)
  b_male <- fit$beta_hat[first, k]
  party  <- factor(party_of[fit$respondent_id[first]],
                   levels = c("Democrat", "Independent", "Republican"))
  stopifnot(!anyNA(party), length(b_male) == 1191L)
  pm_male <- tapply(b_male, party, mean)

  ## (3) Fractions preferring a female candidate (beta_i,Male < 0)
  fr <- sc_fraction_preferring(fit)$estimate
  frac_male_pos <- fr$frac_positive[fr$dummy_name == k]
  frac_female   <- 1 - frac_male_pos
  f_dem <- mean(b_male[party == "Democrat"] < 0)
  f_rep <- mean(b_male[party == "Republican"] > 0)
  f_ind <- mean(b_male[party == "Independent"] < 0)

  ## (4) Counterfactual contest: Ambitious woman vs Ambitious man
  ##     (production matchup, 32_run_pipeline 8.2)
  A <- list(cand_gender = "Female", cand_run = "Yes",
            cand_talent = "Hard-Working", cand_agenda = "Complete Overhaul",
            cand_child = "No children")
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
    ## Female-coefficient convention (beta_F = -beta_Male), matching the
    ## 04 bundled-data report rows; canon gate uses canon_check below.
    quantities = c(
      gender_importance_pct = 100 * gender_share,
      beta_F_mean_Dem  = -unname(pm_male["Democrat"]),
      beta_F_mean_Ind  = -unname(pm_male["Independent"]),
      beta_F_mean_Rep  = -unname(pm_male["Republican"]),
      theta_F          = -unname(th_m),
      theta_F_se       = unname(se),
      theta_F_ci_lo    = -unname(th_m + 1.96 * se),
      theta_F_ci_hi    = -unname(th_m - 1.96 * se),
      frac_prefer_female_pct = 100 * frac_female,
      frac_Dem_prefer_F_pct  = 100 * f_dem,
      frac_Rep_prefer_M_pct  = 100 * f_rep,
      frac_Ind_prefer_F_pct  = 100 * f_ind,
      cf_AmbW_vs_AmbM_orth   = cf_orth$estimate,
      cf_AmbW_vs_AmbM_orth_lo = cf_orth$ci_lo,
      cf_AmbW_vs_AmbM_orth_hi = cf_orth$ci_hi,
      cf_AmbW_vs_AmbM_plugin  = cf_plug$estimate,
      heldout_logloss  = heldout_logloss(fit)
    ),
    ## Raw production (male-coded) values for the canon gate
    canon_check = list(
      gender_importance_pct = 100 * gender_share,
      beta_male_party = pm_male,
      theta_male = unname(th_m), theta_male_se = unname(se),
      theta_male_ci = unname(c(th_m - 1.96 * se, th_m + 1.96 * se)),
      party_n = table(party)
    ),
    importance_table = imp,
    interaction = int_sum
  )
}

## --- Baseline canon gate ----------------------------------------------------
gate_baseline <- function(arm) {
  cc <- arm$canon_check
  d_imp <- cc$gender_importance_pct - CANON$importance_gender_pct
  d_pm  <- cc$beta_male_party - CANON$beta_male_party[names(cc$beta_male_party)]
  d_th  <- cc$theta_male - CANON$theta_male
  covers0 <- cc$theta_male_ci[1] < 0 && cc$theta_male_ci[2] > 0
  cat(sprintf(
    "\n--- baseline canon gate (arm0_s42 vs saved production run) ---\n"))
  cat(sprintf("  party n: %s (canon 403/397/391)\n",
              paste(cc$party_n, collapse = "/")))
  cat(sprintf("  gender importance: %.2f%% (canon 16.47; diff %+.2fpp)\n",
              cc$gender_importance_pct, d_imp))
  for (p in names(d_pm))
    cat(sprintf("  beta_Male mean %-11s: %+.3f (canon %+.3f; diff %+.3f)\n",
                p, cc$beta_male_party[p], CANON$beta_male_party[p], d_pm[p]))
  cat(sprintf("  theta_Male: %+.3f [%+.3f, %+.3f] (canon %+.3f [%+.3f, %+.3f]; diff %+.3f; CI covers 0: %s)\n",
              cc$theta_male, cc$theta_male_ci[1], cc$theta_male_ci[2],
              CANON$theta_male, CANON$theta_male_ci[1], CANON$theta_male_ci[2],
              d_th, covers0))
  ok <- all(cc$party_n == CANON$party_n) &&
    abs(d_imp) <= 1.5 && all(abs(d_pm) <= 0.10) &&
    abs(d_th) <= 0.08 && covers0
  if (!ok) stop("Baseline canon gate FAILED -- arm0_s42 does not reproduce ",
                "the paper's SW canon; not running the interaction arms. ",
                "See diffs above for the prep mismatch.")
  cat("  GATE PASSED\n")
}

## --- Arm runner with cache ---------------------------------------------------
fml <- as.formula(paste(
  "choice ~", paste(ATTRS, collapse = " + "),
  "|", paste(Z_COLS, collapse = " + ")))

run_arm <- function(label, interactions, seed, interaction_rank = 2L) {
  cache <- file.path(exp_dir, paste0("05_cache_", label, ".rds"))
  if (file.exists(cache)) {
    cat("[", label, "] cached -- skipping refit\n")
    return(readRDS(cache))
  }
  cat("[", label, "] fitting: interactions =", interactions,
      " seed =", seed, "\n")
  t0 <- Sys.time()
  fit <- scfit(
    fml,
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
    parallel = TRUE, n_cores = 10L,  # production runner (05_fit.R); RNG
                                     # streams make this == serial bit-level
    keep_modules = FALSE,
    verbose = FALSE
  )
  rt <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat("[", label, "] done in", round(rt, 1), "min\n")
  stopifnot(identical(fit$hidden, c(32L, 32L, 16L)),
            length(fit$theta) == 13L,
            nrow(fit$deltaX) == 3573L,
            isFALSE(fit$normalize_deltaX),
            identical(make.names(colnames(fit$deltaX)), mats$dummy_names))
  out <- extract_arm(fit, label, rt)
  saveRDS(out, cache)
  out
}

## --- Run: baseline first, gate, then the remaining arms ---------------------
arms <- list(arm0_s42 = run_arm("arm0_s42", "none", 42L))
gate_baseline(arms$arm0_s42)

arms$arm0_s7  <- run_arm("arm0_s7",  "none",      7L)
arms$arm1_exp <- run_arm("arm1_exp", "explicit", 42L)
arms$arm2_lr2 <- run_arm("arm2_lr2", "lowrank",  42L, interaction_rank = 2L)

## --- Comparison table ---------------------------------------------------------
qn   <- names(arms$arm0_s42$quantities)
tab  <- sapply(arms, function(a) a$quantities[qn])
rownames(tab) <- qn

base  <- tab[, "arm0_s42"]
noise <- abs(tab[, "arm0_s7"] - base)              # two-seed ensemble noise
diffs <- abs(tab[, c("arm1_exp", "arm2_lr2"), drop = FALSE] - base)
within <- sweep(diffs, 1, noise, "<=")

cat("\n============== SW production headline comparison (arm x quantity) ==============\n")
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
             arms = arms, canon = CANON,
             config = list(K = 10L, n_epochs = 1000L, learning_rate = 0.01,
                           hidden = c(32L, 32L, 16L), weight_decay = "adaptive",
                           ridge_lambda = 1e-4, stage2 = "map_c5",
                           seeds = c(42L, 7L), lambda_V = 1e-2,
                           interaction_rank = 2L, parallel = TRUE,
                           n_cores = 10L,
                           data = "production prep_long.rds (19-moderator Z)")),
        file.path(exp_dir, "results_sw_comparison_production.rds"))
cat("Saved", file.path(exp_dir, "results_sw_comparison_production.rds"), "\n")
