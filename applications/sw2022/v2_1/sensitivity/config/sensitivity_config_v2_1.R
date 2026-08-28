## Versioned application-sensitivity plan for the Saha--Weeks v2.1 reported
## primary.  This is a disclosed, outcome-informed plan and not a
## preregistration.  No quantitative materiality rule is introduced here.

if (!exists("sw_v21_config", inherits = TRUE)) {
  stop("Source analysis_config_v2_1.R before this sensitivity config.",
       call. = FALSE)
}

sw_v21_sensitivity_config <- list(
  schema_version = "sw2022-paperps-sensitivity-v2.1",
  version = "sw2022-paperps-2026-08-24-v2.1-sensitivity-v1",
  reported_primary_pointer = file.path(
    sw_v21_config$output_root, "reported_primary_pointer.rds"),
  output_root = file.path(sw_v21_application_root, "results",
                          "mixed_logit_v2_1_sensitivity"),
  outcome_blind = FALSE,
  formal_inference_available = FALSE,
  maintained_model = FALSE,
  primary_artifacts_overwritten = FALSE,
  disclosure = paste(
    "Created after inspection of the Saha--Weeks outcomes and v2 pilot.",
    "Every result is descriptive and outcome-informed; ordinary formal",
    "inference and maintained-model claims are unavailable."),
  pointer_policy = paste(
    "Every fitted sensitivity must first validate the canonical",
    "reported_primary_pointer, its result and manifest, every parent",
    "artifact, all frozen generation inputs, runtime, authorization, and",
    "the chosen full/nested/assembled fit stamps. The complete hash lock is",
    "then embedded in each checkpoint and downstream manifest."),
  components = c(
    "postconjoint_19Z", "male_x_prior_run", "position_profile_swap",
    "completion_1191_vs_1249_noZ", "task_order_serial_diagnostics"),
  postconjoint = list(
    variables = c("ideo_conservative", "vote_trump", "vote_clinton",
                  "gender_att"),
    imputation = "outer-training respondent median, frozen for held-out rows",
    interpretation = paste(
      "The four post-conjoint variables stay excluded from the reported",
      "primary and enter only this timing-sensitive descriptive perturbation.")),
  male_run = list(
    feature = "I(candidate is Male) * I(candidate previously ran)",
    contrast_orientation = "feature(candidate A) - feature(candidate B)"),
  completion = list(
    comparison = paste(
      "The 1,191 complete-case respondents and all 1,249 respondents with",
      "two or three valid tasks are each fit with the same pointer-selected",
      "q=1 learner and a one-column zero moderator matrix.")),
  process = list(
    position_swap = paste(
      "Refit after A/B relabeling: deltaX becomes -deltaX and y becomes 1-y."),
    heldout_diagnostics = paste(
      "Task-order calibration, adjacent response-pattern calibration, and",
      "serial residual summaries use pointer-validated out-of-fold predictions."),
    task_process_likelihood = "not_run",
    serial_shock_likelihood = "not_run"),
  profiles = list(
    production = list(
      label = "production",
      validated_fallback = FALSE,
      n_epochs = as.integer(sw_v21_config$optimizer$n_epochs),
      n_starts = as.integer(sw_v21_config$optimizer$n_starts),
      learning_rate = sw_v21_config$optimizer$learning_rate,
      opt_tol = sw_v21_config$optimizer$opt_tol,
      grad_tol = sw_v21_config$optimizer$grad_tol,
      nested_objective_tol = sw_v21_config$optimizer$nested_objective_tol,
      note = paste(
        "Uses the frozen v2.1 production optimization budget. Components may",
        "be run separately and are combined only after every checkpoint",
        "revalidates the same reported-primary hash lock.")),
    validated_fallback = list(
      label = "validated_fallback",
      validated_fallback = TRUE,
      n_epochs = 800L, n_starts = 2L,
      learning_rate = sw_v21_config$optimizer$learning_rate,
      opt_tol = sw_v21_config$optimizer$opt_tol,
      grad_tol = sw_v21_config$optimizer$grad_tol,
      nested_objective_tol = sw_v21_config$optimizer$nested_objective_tol,
      note = paste(
        "Smaller, separately labeled computational fallback. It receives the",
        "same provenance and optimizer gates but is never relabeled as the",
        "production sensitivity battery."))
  ),
  bounds = sw_v21_config$bounds,
  device = sw_v21_config$optimizer$device,
  seed = as.integer(sw_v21_config$optimizer$seed + 700000L),
  materiality_tolerances = NULL,
  empirical_alternative_likelihoods = list(
    skewed_factor = "not_run", bimodal_factor = "not_run",
    student_t5_factor = "not_run", covariance_by_party = "not_run",
    random_response_scale = "not_run", serial_AR1_shock = "not_run",
    task_varying_coefficients = "not_run"),
  profile_likelihoods = "not_run",
  fail_closed_note = paste(
    "No quantitative materiality margin was approved. A completed optimizer",
    "or simulation gate is not a substantive pass, does not verify a",
    "maintained assumption, and does not supply formal inference."))
