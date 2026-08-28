## Frozen, application-only specification for the Saha--Weeks party-by-
## candidate-gender conditional-mean diagnostic.
##
## This diagnostic was designed after inspecting the production mismatch
## between party-specific AMCE-style estimates and the primary DNN plug-in.
## It is therefore post hoc, not preregistered or outcome-blind.  The purpose
## is narrow: distinguish shrinkage of the DNN mean head from predictive
## support for two party-by-candidate-gender mean deviations.  Nothing here
## changes the maintained estimator, primary fit, or formal-inference status.

sw_party_gender_mean_config <- list(
  schema_version = "sw2022-party-gender-mean-diagnostic-v1",
  created_after_primary_mismatch_was_observed = TRUE,
  outcome_blind = FALSE,
  primary_artifacts_modified = FALSE,
  formal_inference_available = FALSE,
  maintained_model = FALSE,
  posterior_summaries_used = FALSE,
  model = list(
    rank = 1L,
    integration = "gauss-hermite",
    production_nodes = 31L,
    pooled_mean = paste(
      "Thirteen unrestricted common mean coefficients, a common rank-one",
      "normal residual loading, and a common candidate-A intercept."
    ),
    targeted_mean = paste(
      "The pooled-mean model plus exactly two conditional-mean deviations:",
      "Republican-by-candidate-Male and Independent-by-candidate-Male, with",
      "Democrat as the reference. All other mean slopes remain common."
    ),
    penalty = paste(
      "No mean-coefficient penalty. The fixed 15-parameter targeted mean is",
      "used to isolate the primary DNN penalty/architecture from the narrow",
      "party-by-candidate-gender signal; there is no tuning grid."
    ),
    folds = paste(
      "Reuse the primary respondent-level outer folds. Each comparator is",
      "refit using only the corresponding outer-training respondents."
    )
  ),
  controls = list(
    smoke = list(
      n_nodes = 7L, n_epochs = 40L, learning_rate = 0.01,
      n_starts = 1L, opt_tol = 1, grad_tol = 1,
      require_optimization_gate = FALSE, diagnostic_only = TRUE
    ),
    production = list(
      n_nodes = 31L, n_epochs = 1400L, learning_rate = 0.005,
      n_starts = 3L, opt_tol = 1e-4, grad_tol = 1e-2,
      require_optimization_gate = TRUE, diagnostic_only = TRUE
    )
  ),
  bounds = list(mu = 10, kappa = 10, loading = 10, parameter = 20),
  seed = 20260824L + 730000L,
  reporting = list(
    primary_target = "Female versus Male candidate preference, by party",
    sequence_score = "complete respondent-sequence log likelihood",
    calibration = paste(
      "Party-specific observed and predicted choice rates plus realized-design",
      "AMCE-style projections of out-of-fold predicted probabilities."
    ),
    fail_closed = paste(
      "Report as a cross-fitted post-hoc diagnostic only. Withhold formal",
      "inference, model-selection claims, maintained-assumption verification,",
      "and any pass/fail materiality conclusion."
    )
  ),
  provenance_note = paste(
    "A same-sample q=0 scope check was used to choose the narrow two-parameter",
    "extension instead of all party-specific slopes. Consequently, outer-fold",
    "scores verify respondent isolation for the frozen refits but are not an",
    "outcome-blind assessment of the process that selected this diagnostic."
  )
)
