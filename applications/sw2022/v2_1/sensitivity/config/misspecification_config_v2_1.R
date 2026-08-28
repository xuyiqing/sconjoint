## Design-specific simulation plan for the v2.1 reported primary.  The DGP
## definitions, scenario ordering, and seed are imported byte-for-byte from
## the existing v1 plan; only execution profiles and pointer binding change.

if (!exists("sw_v21_application_root", inherits = TRUE)) {
  stop("Source analysis_config_v2_1.R before this config.", call. = FALSE)
}

.sw_v21_v1_misspec_path <- file.path(
  sw_v21_application_root, "sensitivity", "misspecification_config.R")
if (!file.exists(.sw_v21_v1_misspec_path)) {
  stop("The frozen v1 misspecification config is missing.", call. = FALSE)
}
.sw_v21_v1_env <- new.env(parent = baseenv())
sys.source(.sw_v21_v1_misspec_path, envir = .sw_v21_v1_env)
.sw_v21_v1_misspec <- .sw_v21_v1_env$sw_misspecification_config

sw_v21_misspecification_config <- list(
  schema_version = "sw2022-design-misspecification-v2.1",
  version = "sw2022-paperps-2026-08-24-v2.1-misspecification-v1",
  outcome_blind = FALSE,
  formal_inference_available = FALSE,
  maintained_model = FALSE,
  primary_artifacts_overwritten = FALSE,
  dgp_source_schema_version = .sw_v21_v1_misspec$schema_version,
  dgp_source_path = .sw_v21_v1_misspec_path,
  dgp_source_md5 = unname(tools::md5sum(.sw_v21_v1_misspec_path)),
  dgp_definitions_reused_exactly = TRUE,
  scenario_order_reused_exactly = TRUE,
  seed_reused_exactly = TRUE,
  scenarios = .sw_v21_v1_misspec$scenarios,
  dgp = .sw_v21_v1_misspec$dgp,
  seed = .sw_v21_v1_misspec$seed,
  factor_orientation = .sw_v21_v1_misspec$factor_orientation,
  estimand_distribution = paste(
    "The empirical distribution of the 1,191 primary respondents'",
    "pointer-reported v2.1 conditional means and the fielded three-task",
    "contrast sequences."),
  profiles = list(
    production = list(
      replications = 30L, minimum_defensible_replications = 20L,
      truth_draws = 50000L,
      n_epochs = as.integer(sw_v21_config$optimizer$n_epochs),
      n_starts = as.integer(sw_v21_config$optimizer$n_starts),
      validated_fallback = FALSE,
      label = "production"),
    validated_fallback = list(
      replications = 5L, minimum_defensible_replications = 5L,
      truth_draws = 10000L, n_epochs = 800L, n_starts = 2L,
      validated_fallback = TRUE,
      label = "validated_fallback")
  ),
  refit = list(
    rule = paste(
      "Condition on the mean family, architecture, penalty, q=1 rank, and",
      "integration grid in the validated reported-primary full fit; refit",
      "that normal common-covariance likelihood to each simulated outcome."),
    tuning_repeated = FALSE, inference_repeated = FALSE,
    pointer_required = TRUE),
  coverage = .sw_v21_v1_misspec$coverage,
  materiality_tolerances = NULL,
  empirical_alternative_likelihoods = "not_run",
  profile_likelihoods = "not_run",
  fail_closed_note = paste(
    "These are design-specific simulated-data stress tests, not empirical",
    "alternative-family likelihood fits. Bias and stability are descriptive;",
    "coverage, materiality passes, identification, and formal inference remain",
    "unavailable."))

if (!identical(sw_v21_misspecification_config$scenarios,
               .sw_v21_v1_misspec$scenarios) ||
    !identical(sw_v21_misspecification_config$dgp,
               .sw_v21_v1_misspec$dgp) ||
    !identical(sw_v21_misspecification_config$seed,
               .sw_v21_v1_misspec$seed)) {
  stop("The v2.1 simulation plan drifted from the frozen v1 DGP or seed.",
       call. = FALSE)
}

rm(.sw_v21_v1_env, .sw_v21_v1_misspec)
