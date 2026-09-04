## Frozen analysis choices for the rebuilt Saha--Weeks application.
##
## This file contains specifications and gates, not fitted results.  The
## primary residual rank q = 1 is inherited from the exploratory prototype
## and was fixed before this production reanalysis.  It was not preregistered
## or chosen outcome-blind.  q = 0 and q = 2 are sensitivity specifications;
## they never replace the q = 1 headline fit.

sw_application_root <- getOption("sconjoint.sw_application_root", NULL)
if (is.null(sw_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "sw2022")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.sw_application_root' before sourcing this file.")
  }
  sw_application_root <- candidate
}
sw_application_root <- normalizePath(sw_application_root, mustWork = TRUE)

sw_coefficient_order <- c(
  "cand_genderMale",
  "cand_runYes",
  "cand_talentCollaborative",
  "cand_talentDetermined.to.Succeed",
  "cand_talentEmpathetic",
  "cand_talentGood.Communicator",
  "cand_talentHard.Working",
  "cand_talentTough.Negotiator",
  "cand_agendaModerate.Changes",
  "cand_agendaComplete.Overhaul",
  "cand_child1.child",
  "cand_child2.children",
  "cand_child3.children"
)

sw_analysis_config <- list(
  version = "sw2022-paperps-2026-08-26-v2-amended",
  input = list(
    prepared = file.path(sw_application_root, "results",
                         "prep_analysis_data.rds"),
    primary_Z = "Z_primary",
    sensitivity_Z = "Z_sensitivity19_raw",
    primary_sample = paste(
      "1,191 SSI respondents with three valid completed conjoint tasks and",
      "complete prespecified pre-conjoint moderator data"
    ),
    task_outcome = "Y = 1 when candidate A is chosen",
    contrast_orientation = "DeltaX = X_A - X_B",
    respondent_weighting = "equal respondent weight",
    no_survey_weights = TRUE
  ),
  output_root = file.path(sw_application_root, "results", "mixed_logit"),
  coefficients = list(
    order = sw_coefficient_order,
    references = c(
      candidate_gender = "Female", candidate_run = "No",
      candidate_talent = "Assertive", candidate_agenda = "Very Few Changes",
      candidate_children = "No children"
    )
  ),
  primary = list(
    q = 1L,
    provenance = paste(
      "Inherited from the exploratory low-rank prototype before the revised",
      "production analysis; not preregistered and not outcome-blind."
    ),
    rank_selected_from_current_data = FALSE,
    alternative_q = c(0L, 2L)
  ),
  network = list(
    activation = "ReLU (fixed by the scmix implementation)",
    output = "coordinatewise bounded tanh conditional-mean head",
    tuning_criterion = "held-out complete respondent-sequence log likelihood",
    regularization = "finite prespecified squared-weight penalty grid",
    stopping = paste(
      "maximum epochs or joint returned-state criterion/gradient tolerance;",
      "early validation stopping disabled"
    )
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
    pilot = list(
      label = "staged computational pilot; not the paper result",
      outer_K = 2L, inner_K = 2L, n_epochs = 200L, n_starts = 2L,
      learning_rate = 0.01, n_nodes = 9L,
      grid = list(
        list(name = "pilot_16_wd1e4", hidden = c(16L, 16L),
             weight_decay = 1e-4, integration = "gh", n_nodes = 9L),
        list(name = "pilot_32_wd5e4", hidden = c(32L, 32L, 16L),
             weight_decay = 5e-4, integration = "gh", n_nodes = 9L)
      ),
      opt_tol = 1e-5, grad_tol = 5e-3,
      diagnostic_only = TRUE
    ),
    production = list(
      label = "paper production specification",
      outer_K = 5L, inner_K = 2L, n_epochs = 800L, n_starts = 3L,
      learning_rate = 0.005, n_nodes = 31L,
      grid = list(
        list(name = "compact_16_wd1e4", hidden = c(16L, 16L),
             weight_decay = 1e-4, integration = "gh", n_nodes = 31L),
        list(name = "wide_32_wd1e4", hidden = c(32L, 32L, 16L),
             weight_decay = 1e-4, integration = "gh", n_nodes = 31L),
        list(name = "wide_32_wd5e4", hidden = c(32L, 32L, 16L),
             weight_decay = 5e-4, integration = "gh", n_nodes = 31L)
      ),
      opt_tol = 1e-6, grad_tol = 1e-3,
      diagnostic_only = FALSE
    ),
    ## Added 2026-08-26 (Yiqing's rerun). The frozen production grid is
    ## unsatisfiable under its own computational gates on these data: every
    ## candidate either leaves the sieve gradient far above grad_tol at any
    ## epoch budget or drives the conditional-mean output onto its +-10 compact
    ## bound (mu bound active), which is a terminal gate failure. This profile
    ## keeps the production fold structure, starts, node count, and tolerances,
    ## and revises only (a) the learner grid, to the memo's outcome-informed
    ## 4-unit/decay-0.1 candidate plus two less-penalized small candidates that
    ## probes show approach stationarity without bound activity, and (b) the
    ## optimization effort (epochs 800 -> 12000, learning rate 0.005 -> 0.01),
    ## so that failure to converge reflects the candidate, not the budget. The
    ## revisions were fixed before any substantive quantity of the new
    ## candidates was computed; the only outcome-adjacent number seen first was
    ## one half-sample validation log score for the memo candidate.
    production_amended = list(
      label = "2026-08-26 amended production: feasible grid, evidence-sized effort",
      ## n_epochs 12000: the moderate-decay candidates plateau at sieve
      ## gradient ~5e-3--1e-2 (probes to 20k epochs at two learning rates) and
      ## fail at any budget; the memo candidate passes full-sample inner CV
      ## from 3000 epochs up but kept failing one nested outer fold at 3000
      ## and 6000, so the budget is raised once more for fold-level margin.
      outer_K = 5L, inner_K = 2L, n_epochs = 12000L, n_starts = 3L,
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
    ## Diagnostic-only forced-non-degenerate-mean fit (work-list step 2:
    ## "refit with a mean forced to be non-degenerate"). One moderate-decay
    ## candidate under deliberately generous computational gates so the fit
    ## assembles; NEVER report substantively. Its purpose is to test the
    ## mean-collapse hypothesis: does a non-degenerate mean (a) keep
    ## tr(Omega_Z) away from zero, (b) recover party-varying gender means,
    ## (c) change the Riesz-equation and ridge-sensitivity diagnostics?
    diagnostic_forced_mean = list(
      label = "diagnostic forced non-degenerate mean; never report substantively",
      outer_K = 5L, inner_K = 2L, n_epochs = 6000L, n_starts = 3L,
      learning_rate = 0.01, n_nodes = 31L,
      grid = list(
        list(name = "forced_4_wd1e2", hidden = c(4L),
             weight_decay = 0.01, integration = "gh", n_nodes = 31L)
      ),
      opt_tol = 1e-4, grad_tol = 2e-2,
      diagnostic_only = TRUE
    )
  ),
  optimizer = list(
    mu_bound = 10, kappa_bound = 10, a_bound = 10, weight_bound = 20,
    device = "cpu", early_stop = FALSE,
    seed = 20260824L,
    checkpoint_after_each_fit = TRUE,
    require_all_inner_and_selected_refit_gates = TRUE,
    global_optimum_certified = FALSE
  ),
  numerical_refinement = list(
    method = "fresh nested refits using common deterministic GH nodes",
    production_nodes = c(15L, 31L, 45L),
    pilot_nodes = c(7L, 9L, 13L),
    tolerances = list(
      qoi = 0.005, likelihood = 0.002, score = 0.005,
      riesz = 0.01, if_l2 = 0.05, se = 0.005
    )
  ),
  rank_sensitivity = list(
    q2_nodes = c(smoke = 5L, pilot = 9L, production = 15L,
                 production_amended = 15L, diagnostic_forced_mean = 15L),
    note = paste(
      "The q=2 tensor-product GH sensitivity uses its separately declared",
      "resolution and must receive its own node-refinement check before any",
      "small numerical difference is interpreted substantively."
    )
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
    total_heterogeneity_margin = 1e-4,
    subgroup_probability_margin = 0.10,
    choice_nodes = 45L,
    ## Leave FALSE unless a paper-specific approximation/product-rate argument
    ## for the declared fitted sieve has been written and approved.  Numerical
    ## and optimization diagnostics alone cannot switch this flag.
    enable_conditional_formal_inference = FALSE,
    tangent = list(
      type = "fitted_sieve", prespecified = TRUE,
      identified_directions = TRUE, training_only = TRUE,
      outer_fold_specific = TRUE,
      approximation_argument = "",
      product_rate_argument = "",
      provenance = ""
    )
  ),
  qoi = list(
    reporting_reference = paste(
      "Male and Empathetic for continuity with Saha--Weeks displays; the",
      "likelihood is fit in the frozen Female/No/Assertive/Very-Few/No-child basis"
    ),
    primary_subgroups = "party",
    secondary_subgroups = "respondent gender",
    mrs = "not applicable: this design contains no natural common-unit denominator",
    contests = list(
      complete = c(-1, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0),
      moderate = c(-1, 0, 0, 0, 1, 0, 0, -1, 1, -1, 0, 0, 0),
      very_few = c(-1, 0, 0, 0, 1, 0, 0, -1, 0, -1, 0, 0, 0)
    ),
    contests_are_position_neutral = TRUE,
    contests_on_support = FALSE
  ),
  fail_closed = list(
    no_formal_interval_without_classed_verification = TRUE,
    no_majority_claim_unless_interval_excludes_one_half = TRUE,
    no_sign_share_inference_below_variance_floor = TRUE,
    no_rank_selected_fixed_q_interval = TRUE,
    posterior_modes_prohibited = TRUE
  )
)

rm(sw_coefficient_order)
