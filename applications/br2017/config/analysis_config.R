## Frozen analysis choices for the rebuilt Ballard-Rosa application.
##
## Declared 2026-08-26 BEFORE any structural fit of the rebuilt estimator to
## these data. Grid and effort copy the sw2022 amended production profile (see
## the sw2022 config note on the original grid's infeasibility), so for this
## application the grid is outcome-blind. Primary q = 1 mirrors the sw2022
## convention; provisional pending the coauthor's outcome-blind sign-off
## (memo ask 4). q = 0 and q = 2 are sensitivity specifications.

br_application_root <- getOption("sconjoint.br_application_root", NULL)
if (is.null(br_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "br2017")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.br_application_root' before sourcing this file.")
  }
  br_application_root <- candidate
}
br_application_root <- normalizePath(br_application_root, mustWork = TRUE)

br_coefficient_order <- c(
  "rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
  "rate_175_375", "rate_375P", "revenue_score")

br_analysis_config <- list(
  version = "br2017-paperps-2026-08-26-v1",
  application = "br2017",
  input = list(
    prepared = file.path(br_application_root, "results",
                         "prep_analysis_data.rds"),
    primary_Z = "Z_primary",
    primary_sample = "2,000 respondents, 8 tasks each, 16,000 tasks",
    task_outcome = "Y = 1 when the displayed-left tax plan is chosen",
    contrast_orientation = "DeltaX = X_left - X_right (DeltaX_fixed)",
    respondent_weighting = "equal respondent weight",
    no_survey_weights = TRUE
  ),
  output_root = file.path(br_application_root, "results", "mixed_logit"),
  coefficients = list(order = br_coefficient_order),
  expected_sample = list(n_respondents = 2000L, n_tasks = 16000L,
                         constant_task_count = TRUE),
  primary = list(
    q = 1L,
    provenance = paste(
      "Mirrors the sw2022 primary-rank convention; set before fitting the",
      "rebuilt estimator to these data; provisional pending coauthor",
      "outcome-blind sign-off (memo ask 4)."),
    rank_selected_from_current_data = FALSE,
    alternative_q = c(0L, 2L)
  ),
  profiles = list(
    smoke = list(
      label = "interface smoke test; never report substantively",
      outer_K = 2L, inner_K = 2L, n_epochs = 3L, n_starts = 1L,
      learning_rate = 0.01, n_nodes = 5L,
      grid = list(
        list(name = "smoke_8", hidden = c(8L), weight_decay = 1e-4,
             integration = "gh", n_nodes = 5L)
      ),
      opt_tol = 1e6, grad_tol = 1e6,
      diagnostic_only = TRUE
    ),
    production_amended = list(
      label = "2026-08-26 production: sw2022-amended grid, declared pre-fit",
      outer_K = 5L, inner_K = 2L, n_epochs = 3000L, n_starts = 3L,
      learning_rate = 0.01, n_nodes = 31L,
      grid = list(
        list(name = "memo_4_wd1e1", hidden = c(4L),
             weight_decay = 0.1, integration = "gh", n_nodes = 31L),
        list(name = "narrow_4_wd1e2", hidden = c(4L),
             weight_decay = 0.01, integration = "gh", n_nodes = 31L),
        list(name = "narrow_8_wd1e2", hidden = c(8L),
             weight_decay = 0.01, integration = "gh", n_nodes = 31L)
      ),
      opt_tol = 1e-6, grad_tol = 1e-3,
      diagnostic_only = FALSE
    ),
    ## Added 2026-08-26 after the production round: NO grid candidate ---
    ## including the memo 4-unit/decay-0.1 learner --- passes the production
    ## computational gates on this application (probes to 10k epochs plateau
    ## at sieve gradient ~5e-3--1e-2 vs grad_tol 1e-3, no bound activity).
    ## The per-respondent-sequence loss grows with tasks per respondent, so
    ## the absolute gate calibrated on sw2022 (T=3) does not transfer to this
    ## data scale. Until the gate is recalibrated (memo ask), fits run under
    ## this DIAGNOSTIC profile: same declared grid, generous computational
    ## gates, diagnostic_only --- never reported as production-grade.
    diagnostic_amended = list(
      label = "2026-08-26 diagnostic: declared grid under generous gates",
      outer_K = 5L, inner_K = 2L, n_epochs = 6000L, n_starts = 3L,
      learning_rate = 0.01, n_nodes = 31L,
      grid = list(
        list(name = "memo_4_wd1e1", hidden = c(4L),
             weight_decay = 0.1, integration = "gh", n_nodes = 31L),
        list(name = "narrow_4_wd1e2", hidden = c(4L),
             weight_decay = 0.01, integration = "gh", n_nodes = 31L),
        list(name = "narrow_8_wd1e2", hidden = c(8L),
             weight_decay = 0.01, integration = "gh", n_nodes = 31L)
      ),
      opt_tol = 1e-4, grad_tol = 2e-2,
      diagnostic_only = TRUE
    ),
    ## Added 2026-08-26 (v2.1 rerun): the paperps corrected mean family ---
    ## unpenalized reference alpha + penalized centered deviation --- with
    ## the exact unpenalized constant nested, mirroring the sw2022 v2.1
    ## grid (constant + linear/relu4/relu8 x decay 1e-3/1e-2/1e-1, GH-31).
    ## Computational gates stay at the diagnostic_amended settings: the
    ## sw2022-calibrated production gate still does not transfer to this
    ## data scale (open resolution), so this profile stays diagnostic_only.
    v21_corrected = list(
      label = "2026-08-26 v2.1: corrected mean-family grid, diagnostic gates",
      outer_K = 5L, inner_K = 2L, n_epochs = 6000L, n_starts = 3L,
      learning_rate = 0.01, n_nodes = 31L,
      grid = local({
        out <- list(list(name = "constant", mean_family = "constant",
                         hidden = integer(), weight_decay = 0,
                         integration = "gh", n_nodes = 31L))
        for (family in c("linear", "relu4", "relu8")) {
          for (wd in c(1e-3, 1e-2, 1e-1)) {
            out[[length(out) + 1L]] <- list(
              name = paste0(family, "_wd", format(wd, scientific = TRUE)),
              mean_family = if (identical(family, "linear")) "linear"
                            else "relu",
              hidden = if (identical(family, "linear")) integer()
                       else if (identical(family, "relu4")) 4L else 8L,
              weight_decay = wd, integration = "gh", n_nodes = 31L)
          }
        }
        out
      }),
      opt_tol = 1e-4, grad_tol = 2e-2,
      diagnostic_only = TRUE
    )
  ),
  optimizer = list(
    mu_bound = 10, kappa_bound = 10, a_bound = 10, weight_bound = 20,
    device = "cpu", early_stop = FALSE,
    seed = 20260826L,
    checkpoint_after_each_fit = TRUE,
    require_all_inner_and_selected_refit_gates = TRUE,
    global_optimum_certified = FALSE
  ),
  rank_sensitivity = list(
    q2_nodes = c(smoke = 5L, production_amended = 15L,
                 diagnostic_amended = 15L, v21_corrected = 15L)
  ),
  inference = list(
    multiplier_draws = 1999L,
    multiplier = "normal",
    level = 0.95,
    active_eigenvalue_min = 1e-6,
    information_eigenvalue_min = 1e-8,
    rank_tolerance = 1e-8,
    riesz_validation_fraction = 0.2,
    riesz_equation_tolerance = 0.05,
    ridge_sensitivity_tolerance = 0.10,
    variance_floor = 1e-6,
    choice_nodes = 45L,
    enable_conditional_formal_inference = FALSE
  ),
  qoi = list(
    headline_coordinate = "revenue_score",
    subgroups = list(),
    subgroup_contrast = NULL
  ),
  fail_closed = list(
    no_formal_interval_without_classed_verification = TRUE,
    no_majority_claim_unless_interval_excludes_one_half = TRUE,
    no_sign_share_inference_below_variance_floor = TRUE,
    no_rank_selected_fixed_q_interval = TRUE,
    posterior_modes_prohibited = TRUE
  )
)

rm(br_coefficient_order)
