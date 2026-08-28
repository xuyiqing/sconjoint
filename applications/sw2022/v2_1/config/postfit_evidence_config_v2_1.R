## Saha--Weeks v2.1 post-fit evidence specification.
##
## This configuration is downstream of the outcome-informed v2.1 model fit.
## It does not change model selection, the reported-primary decision, or the
## unavailable status of formal inference.

sw_v21_postfit_config <- list(
  schema_version = "sw2022-v2.1-postfit-evidence-config-v1",
  version = "sw2022-paperps-2026-08-24-v2.1-postfit-evidence-v1",
  producer_config_version =
    "sw2022-paperps-2026-08-24-v2.1-postpilot-final",
  producer_manifest_schema =
    "sw2022-v2.1-postpilot-final-manifest-v1",
  producer_pointer_schema =
    "sw2022-v2.1-reported-primary-pointer-v1",
  evidence_schema = "sw2022-v2.1-postfit-evidence-v1",
  output_directory_name = "postfit_evidence_v2_1",
  formal_inference_available = FALSE,
  outcome_blind = FALSE,
  assessment_role = paste(
    "Descriptive post-pilot evidence. Fold construction is respondent",
    "cross-fitted, but the final grid and guardrail were specified after",
    "same-sample pilot results were observed."
  ),
  inference = list(
    multiplier_draws = 0L,
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
    choice_nodes = 45L
  ),
  qoi = list(
    reporting_reference = paste(
      "Male and Empathetic for continuity with Saha--Weeks displays; the",
      "likelihood basis is the frozen 13-coordinate preparation basis."
    ),
    contests = list(
      complete = c(-1, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0),
      moderate = c(-1, 0, 0, 0, 1, 0, 0, -1, 1, -1, 0, 0, 0),
      very_few = c(-1, 0, 0, 0, 1, 0, 0, -1, 0, -1, 0, 0, 0)
    ),
    support_note = paste(
      "Realizable under the advertised unrestricted full-profile design;",
      "the fielded randomizer, assignment probabilities, and cross-task",
      "restrictions are not document-certified."
    )
  ),
  fail_closed = list(
    require_complete_producer_manifest = TRUE,
    require_reported_primary_pointer = TRUE,
    hash_validate_all_producer_artifacts = TRUE,
    prohibit_formal_intervals = TRUE,
    prohibit_outcome_blind_label = TRUE,
    prohibit_reuse_of_v1_assessment_or_qoi_artifacts = TRUE,
    publish_only_from_complete_staging_directory = TRUE
  )
)
