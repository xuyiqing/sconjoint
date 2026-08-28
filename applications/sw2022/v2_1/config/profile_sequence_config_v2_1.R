## Saha--Weeks v2.1 descriptive penalized-criterion profile-sequence
## specification with unpenalized likelihood overlays.
##
## This downstream specification was written after all application outcomes
## and the reported-primary fit had been inspected.  It supplies curvature
## diagnostics only.  It does not authorize likelihood-ratio inference.

sw_v21_profile_application_root <- getOption(
  "sconjoint.sw_application_root", NULL)
if (is.null(sw_v21_profile_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "sw2022")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.sw_application_root' before sourcing the profile config.")
  }
  sw_v21_profile_application_root <- candidate
}
sw_v21_profile_application_root <- normalizePath(
  sw_v21_profile_application_root, mustWork = TRUE)

sw_v21_profile_config <- list(
  schema_version = "sw2022-v2.1-profile-sequence-config-v2",
  version = "sw2022-paperps-2026-08-24-v2.1-profile-sequence-v2",
  parent_config_version =
    "sw2022-paperps-2026-08-24-v2.1-postpilot-final",
  parent_manifest_schema = "sw2022-v2.1-postpilot-final-manifest-v1",
  parent_pointer_schema = "sw2022-v2.1-reported-primary-pointer-v1",
  outcome_blind = FALSE,
  descriptive_only = TRUE,
  formal_inference_available = FALSE,
  formal_test = FALSE,
  likelihood_ratio_critical_values = FALSE,
  rank_selected = FALSE,
  input = list(
    prepared = file.path(sw_v21_profile_application_root, "results",
                         "prep_analysis_data.rds"),
    primary_Z = "Z_primary",
    reported_primary_pointer = file.path(
      sw_v21_profile_application_root, "results",
      "mixed_logit_v2_1_postpilot_final",
      "reported_primary_pointer.rds")
  ),
  output_root = file.path(
    sw_v21_profile_application_root, "results",
    "mixed_logit_v2_1_profile_sequences"),
  authorization_file = file.path(
    sw_v21_profile_application_root, "results",
    "mixed_logit_v2_1_profile_sequences",
    "PROFILE_SEQUENCE_AUTHORIZATION.rds"),
  fixed_fit = list(
    q = 1L,
    require_reported_primary = "selected_procedure_q1",
    mean_family = "relu",
    hidden = 4L,
    weight_decay = 0.1,
    integration = "gh",
    n_nodes = 31L,
    preprocessing = "reported-primary full-sample preprocessing",
    learner_and_tuning_fixed = TRUE,
    retuning_per_grid_point = FALSE
  ),
  optimizer = list(
    device = "cpu",
    seed = 20260824L + 960000L,
    n_epochs = 1400L,
    n_starts = 3L,
    learning_rate = 0.005,
    check_every = 25L,
    opt_tol = 1e-4,
    projected_gradient_tol = 1e-2,
    target_tol = 2e-5,
    jitter_fraction = 0.01
  ),
  grids = list(
    kappa = list(
      construction = "additive offsets from reported-primary full-fit value",
      offsets = c(-0.08, -0.04, 0, 0.04, 0.08),
      label = "position/alternative constant kappa",
      rank_boundary = FALSE),
    female_vs_male_mean = list(
      construction = "additive offsets from reported-primary respondent-average value",
      offsets = c(-0.08, -0.04, 0, 0.04, 0.08),
      contrast = c(-1, rep(0, 12L)),
      label = "respondent-average Female-vs-Male conditional mean",
      rank_boundary = FALSE),
    active_covariance_eigenvalue = list(
      construction = "multipliers of the reported-primary rank-one eigenvalue",
      multipliers = c(0.50, 0.75, 1.00, 1.25, 1.50),
      label = "active rank-one residual covariance eigenvalue",
      rank_boundary = FALSE),
    headline_contest_probability = list(
      construction = "additive offsets from reported-primary probability",
      offsets = c(-0.04, -0.02, 0, 0.02, 0.04),
      contest_name = "very_few",
      contrast = c(-1, 0, 0, 0, 1, 0, 0, -1, 0, -1, 0, 0, 0),
      position_neutral = TRUE,
      label = paste(
        "population position-neutral choice probability for the",
        "prespecified very-few-changes contest"),
      support_status = "conditional on advertised support; fielded support not certified",
      rank_boundary = FALSE)
  ),
  reporting = list(
    artifact_kind = paste(
      "descriptive penalized-criterion profile sequence with an",
      "unpenalized complete-sequence likelihood overlay"),
    literal_likelihood_profile = FALSE,
    nuisance_reoptimized = TRUE,
    penalized_reoptimization = TRUE,
    reported_likelihood = "unpenalized complete respondent-sequence likelihood",
    penalty_role = paste(
      "The selected mean-deviation penalty remains fixed and enters nuisance",
      "reoptimization; the table separately reports the unpenalized sequence likelihood."),
    interpretation = paste(
      "Descriptive penalized-criterion profile sequence only; flatness diagnoses weak",
      "application curvature but is neither likelihood-ratio inference nor",
      "a global nonidentification result.")
  )
)
