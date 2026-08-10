# Application comparison: two-stage (scfit, book/paper production recipe)
# vs integrated-likelihood mixed logit (scmix) on bundled br2017 + sw2022.
# feat/mixed-logit overnight prototype.
#
# BEFORE: scfit with the tutorial chapters' exact current-recipe formulas
# (the May regression-cache fits predate the v0.2.1 full-data schema, so
# both sides are fit fresh here on the same data + moderators).
# AFTER: scmix with the identical formula, q = 1.
# Estimation only; figures come from dev/app-mixed-figures.R.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/sconjoint/mixedlogit_prototype")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

bt <- function(x) paste0("`", x, "`")
mk_formula <- function(attrs, zvars) {
  stats::as.formula(paste("choice ~", paste(bt(attrs), collapse = " + "),
                          "|", paste(bt(zvars), collapse = " + ")))
}

run_app <- function(name, formula, data, scfit_args, n_epochs, contrast_name) {
  cat(sprintf("\n===== %s =====\n", name))

  before_path <- file.path(OUT_DIR, paste0("scfit_fit_", name, ".rds"))
  if (file.exists(before_path)) {
    before <- readRDS(before_path)
    cat("two-stage fit loaded from cache\n")
  } else {
    t0 <- Sys.time()
    before <- do.call(scfit, c(list(formula = formula, data = data,
                                    respondent = "respondent", task = "task",
                                    profile = "profile", K = 10L, seed = 42L,
                                    parallel = TRUE, n_cores = 10L),
                               scfit_args))
    cat(sprintf("scfit (two-stage): %.1f min\n",
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    saveRDS(before, before_path)
  }

  t0 <- Sys.time()
  mx <- scmix(formula, data, respondent = "respondent", task = "task",
              profile = "profile", q = 1L, K = 5L, n_epochs = n_epochs,
              seed = 42L)
  cat(sprintf("scmix fit: %.1f min\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  th <- scmix_theta(mx, n_bins = 50L, seed = 7L)
  pol <- scmix_polarization(mx, n_bins = 50L, seed = 7L)
  cv <- stats::setNames(numeric(length(mx$attr_names)), mx$attr_names)
  cv[contrast_name] <- 1
  vc <- scmix_counterfactual(mx, contrast = cv, n_bins = 50L, seed = 7L)

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
    name = name,
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
  saveRDS(res, file.path(OUT_DIR, paste0("app_", name, ".rds")))
  saveRDS(mx, file.path(OUT_DIR, paste0("scmix_fit_", name, ".rds")))
  cat("theta (before | after):\n")
  print(round(cbind(before = before$theta, after = th$estimate), 3))
  res
}

## --- BR 2017 (tax; T = 8; 7 continuous attributes; varref recipe) ----------
data(br2017, package = "sconjoint")
br_attrs <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
              "rate_175_375", "rate_375P", "revenue_score")
br_z <- c("age_std", "female", "pid7_std", "educ_std", "race_white",
          "income_std", "ineq_averse", "work_vs_luck", "taxes_harm_econ",
          "hardwork", "high_econ_know", "employed_ft", "conserv_ideo",
          "govt_serv", "newsint", "numeracy", "gen_mobile", "future_mobile",
          "gov_assist", "risk_averse", "hardship", "children", "trust")
br_res <- run_app("br2017", mk_formula(br_attrs, br_z), br2017,
                  scfit_args = list(stage2 = "varref", varref_floor = 1e-3),
                  n_epochs = 600L, contrast_name = "revenue_score")

## --- SW 2022 (candidate; T = 3; the fragile small-T case; map_c5) ----------
data(sw2022, package = "sconjoint")
sw_attrs <- c("cand_genderMale", "cand_runYes",
              "cand_talentCollaborative", "cand_talentDetermined.to.Succeed",
              "cand_talentEmpathetic", "cand_talentGood.Communicator",
              "cand_talentHard.Working", "cand_talentTough.Negotiator",
              "cand_agendaModerate.Changes", "cand_agendaComplete.Overhaul",
              "cand_child1.child", "cand_child2.children", "cand_child3.children")
sw_z <- c("gender_num", "age", "income", "educ_Middle", "educ_High",
          "party_Republican", "party_Independent", "region_NORTHEAST",
          "region_SOUTH", "region_WEST", "employ_parttime",
          "employ_homemaker", "employ_not_working", "employ_retired",
          "employ_student", "ideo_conservative", "vote_trump",
          "vote_clinton", "gender_att")
sw_res <- run_app("sw2022", mk_formula(sw_attrs, sw_z), sw2022,
                  scfit_args = list(stage2 = "map_c5"),
                  n_epochs = 600L, contrast_name = "cand_genderMale")

cat("\nDONE app-mixed-comparison\n")
