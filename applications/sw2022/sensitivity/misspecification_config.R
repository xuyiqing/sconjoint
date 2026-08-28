## Frozen design-specific misspecification experiment for Saha--Weeks.
##
## This configuration is application-only.  It was written after the observed
## application and its primary fit had been inspected, but before any outcomes
## from 08_run_misspecification_experiments.R were generated.  It is therefore
## a disclosed analysis plan, not a preregistration.  Quantitative materiality
## thresholds were not approved in advance, so the runner may describe bias
## and instability but must never convert them into a substantive pass.

sw_misspecification_config <- list(
  schema_version = "sw2022-design-misspecification-v1",
  prespecified_before_simulation_results = TRUE,
  preregistered = FALSE,
  primary_model_unchanged = TRUE,
  estimand_distribution = paste(
    "The empirical distribution of the 1,191 primary respondents' fitted",
    "conditional means and the fielded three-task contrast sequences."
  ),
  factor_orientation = paste(
    "For q=1, orient the fitted loading so that its largest-absolute",
    "coordinate is positive.  Run both signs of the skewed factor."
  ),
  scenarios = c(
    "normal_benchmark",
    "shape_skewed_positive", "shape_skewed_negative",
    "shape_bimodal", "shape_heavy_tail",
    "covariance_by_party", "random_scale", "serial_shock"
  ),
  dgp = list(
    skewed = list(
      family = "centered and variance-standardized chi-square",
      df = 3,
      analytic_mean = 0,
      analytic_variance = 1
    ),
    bimodal = list(
      family = "equal-weight symmetric two-normal mixture",
      location = 0.90,
      component_sd = sqrt(1 - 0.90^2),
      analytic_mean = 0,
      analytic_variance = 1
    ),
    heavy_tail = list(
      family = "variance-standardized Student t",
      df = 5,
      multiplier = sqrt((5 - 2) / 5),
      analytic_mean = 0,
      analytic_variance = 1,
      finite_covariance = TRUE
    ),
    covariance_by_party = list(
      raw_sd_multiplier = c(
        Democrat = 0.65, Independent = 1.00, Republican = 1.35
      ),
      normalization = paste(
        "Divide by the respondent-weighted RMS multiplier so aggregate",
        "residual covariance equals the primary fitted covariance."
      )
    ),
    random_scale = list(
      family = "mean-one lognormal respondent scale",
      log_sd = 0.35,
      normalization = paste(
        "Theoretical mean one, followed by exact sample-mean normalization",
        "within each simulated respondent sample."
      ),
      comparable_quantities = c(
        "position-neutral choice probabilities",
        "preference sign shares (positive scale invariance)"
      )
    ),
    serial_shock = list(
      family = paste(
        "stationary Gaussian AR(1) additive index component plus the",
        "maintained independent logistic choice shock"
      ),
      rho = 0.50,
      stationary_sd = 0.50,
      innovation_sd = 0.50 * sqrt(1 - 0.50^2)
    )
  ),
  profiles = list(
    smoke = list(replications = 1L, minimum_defensible_replications = 1L,
                 truth_draws = 2000L),
    pilot = list(replications = 5L, minimum_defensible_replications = 5L,
                 truth_draws = 10000L),
    production = list(replications = 30L,
                      minimum_defensible_replications = 20L,
                      truth_draws = 50000L)
  ),
  refit = list(
    rule = paste(
      "Condition on the architecture, penalty, rank, and integration rule",
      "selected by the corresponding primary full-sample analysis, then",
      "refit all normal mixed-logit parameters to each simulated data set."
    ),
    tuning_repeated = FALSE,
    inference_repeated = FALSE,
    rationale = paste(
      "This isolates distributional misspecification conditional on the",
      "reported primary tuning decision and keeps the experiment feasible."
    )
  ),
  coverage = list(
    evaluated = FALSE,
    reason = paste(
      "The application deliberately withholds formal intervals until its",
      "fitted-sieve approximation and product-rate argument is approved.",
      "No oracle or same-sample Monte Carlo interval is relabeled as coverage."
    )
  ),
  materiality_tolerances = NULL,
  fail_closed_note = paste(
    "Simulation summaries report bias, RMSE, quantiles, and optimization",
    "success.  With no approved materiality margin and no formal interval",
    "procedure, every substantive pass and coverage entry remains unavailable."
  ),
  seed = 20269824L
)
