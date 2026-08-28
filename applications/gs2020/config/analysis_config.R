## Frozen analysis choices for the rebuilt Graham--Svolik application.
##
## Declared 2026-08-26 BEFORE any structural fit of the rebuilt estimator to
## these data. The learner grid and optimization effort copy the sw2022
## amended production profile (the original sw2022 frozen grid is
## unsatisfiable under its own computational gates; see the sw2022 config
## note), so for this application the grid is outcome-blind. The primary
## residual rank q = 1 mirrors the sw2022 convention and the exploratory
## prototype; it is provisional pending the coauthor's outcome-blind sign-off
## (memo ask 4). q = 0 and q = 2 are sensitivity specifications.

gs_application_root <- getOption("sconjoint.gs_application_root", NULL)
if (is.null(gs_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "gs2020")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.gs_application_root' before sourcing this file.")
  }
  gs_application_root <- candidate
}
gs_application_root <- normalizePath(gs_application_root, mustWork = TRUE)

gs_coefficient_order <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote",
  "diff_dem_code_v_affair", "diff_dem_code_v_tax",
  "diff_sex_Female",
  "diff_race_Asian", "diff_race_Black", "diff_race_Hispanic",
  "diff_pro_Farmer", "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")

gs_analysis_config <- list(
  version = "gs2020-paperps-2026-08-26-v1",
  application = "gs2020",
  input = list(
    prepared = file.path(gs_application_root, "results",
                         "prep_analysis_data.rds"),
    primary_Z = "Z_primary",
    primary_sample = paste(
      "1,605 complete-case respondents, 20,657 tasks with observed outcomes;",
      "variable task counts (1-13) retained"),
    task_outcome = "Y = 1 when the displayed-left candidate is chosen",
    contrast_orientation = "DeltaX = X_left - X_right (c_onLeft rebuild)",
    respondent_weighting = "equal respondent weight",
    no_survey_weights = TRUE
  ),
  output_root = file.path(gs_application_root, "results", "mixed_logit"),
  coefficients = list(order = gs_coefficient_order),
  expected_sample = list(n_respondents = 1605L, n_tasks = 20657L,
                         constant_task_count = FALSE),
  restrictions = paste(
    "Candidate age and experience are excluded from the structural utility;",
    "their coefficients are restricted to zero (decision 2026-08-24)."),
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
      ## 2026-08-26 diagnosis (outer fold 5, exact driver seeds): the
      ## shared continued-constant inner fit attained an objective
      ## 1.431e-6 above the shared pooled prefit (gradient 4.6e-4,
      ## converged) --- optimizer float noise at this objective scale
      ## (~8 nats), not an optimization failure. At the package default
      ## nested_objective_tol = 1e-6 that one shared fit disqualified
      ## every candidate. Set the tolerance one decade above the noise
      ## floor. Upstream ask: scale the default with objective
      ## magnitude, or restore the prefit state instead of failing.
      nested_objective_tol = 1e-5,
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
    headline_coordinate = "diff_respParty",
    subgroups = list(
      liberal = list(meta_column = "ideo7", op = "<=", value = 3),
      conservative = list(meta_column = "ideo7", op = ">=", value = 5)
    ),
    subgroup_contrast = "diff_respParty"
  ),
  fail_closed = list(
    no_formal_interval_without_classed_verification = TRUE,
    no_majority_claim_unless_interval_excludes_one_half = TRUE,
    no_sign_share_inference_below_variance_floor = TRUE,
    no_rank_selected_fixed_q_interval = TRUE,
    posterior_modes_prohibited = TRUE
  )
)

rm(gs_coefficient_order)
