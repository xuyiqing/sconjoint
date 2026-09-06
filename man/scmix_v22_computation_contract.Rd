\name{scmix_v22_computation_contract}
\alias{scmix_v22_structural_parameter_contract}
\alias{scmix_v22_integration_rule}
\alias{scmix_v22_integration_contract}
\alias{scmix_v22_fit_scope_registry}
\alias{scmix_v22_start_optimizer_evidence}
\alias{scmix_v22_target_recomputation}
\alias{scmix_v22_multistart_stability}
\alias{scmix_v22_nested_specification_contract}
\alias{scmix_v22_nested_fit_stability}
\alias{scmix_v22_integration_riesz_solution}
\alias{scmix_v22_integration_target_evidence}
\alias{scmix_v22_integration_evaluation}
\alias{scmix_v22_integration_compare}
\alias{scmix_v22_integration_refinement}
\alias{scmix_v22_validate_computation_artifact}
\title{Signed Protocol-v2.2 Computation Evidence}
\usage{
scmix_v22_structural_parameter_contract(
  application_id, specification_id, q, covariance_coordinate_ids,
  score_coordinate_ids
)

scmix_v22_integration_rule(
  rule_id, method, level = c("a", "b"), nodes, weights = NULL,
  nesting_supported
)

scmix_v22_integration_contract(
  structural_parameter_contract, registry, uses_numerical_integration,
  level_a_rule, level_b_rule, integrated_target_ids,
  integrated_primitive_ids, nonnesting_reason = NULL
)

scmix_v22_fit_scope_registry(
  application_id, specification_id, fit_id, outer_fold_ids, starts,
  integration_contract
)

scmix_v22_start_optimizer_evidence(
  analysis_contract, registry, integration_contract,
  structural_parameter_contract, integration_rule, fit_scope_id, seed,
  optimizer_artifact,
  role = c("level_a_production_start", "level_b_repolish"),
  initial_state_hash = NULL, calibration_artifact = NULL
)

scmix_v22_target_recomputation(
  optimizer_evidence, analysis_contract, registry, integration_contract,
  structural_parameter_contract, reported_estimates, standard_errors
)

scmix_v22_multistart_stability(
  evaluations, optimizer_evidence, analysis_contract, registry,
  integration_contract, structural_parameter_contract,
  calibration_artifact = NULL,
  protocol_mode = c("calibration", "production")
)

scmix_v22_nested_specification_contract(
  restricted_analysis_contract, full_analysis_contract,
  restricted_structural_contract, full_structural_contract,
  embedding_matrix, restriction_offset, verification_artifact_hash
)

scmix_v22_nested_fit_stability(
  nested_contract, restricted_analysis_contract, full_analysis_contract,
  restricted_registry, full_registry, restricted_integration_contract,
  full_integration_contract, restricted_structural_contract,
  full_structural_contract, restricted_optimizer_evidence,
  full_optimizer_evidence, restricted_calibration_artifact = NULL,
  full_calibration_artifact = NULL,
  protocol_mode = c("calibration", "production")
)

scmix_v22_integration_riesz_solution(
  analysis_contract, registry, integration_contract,
  structural_parameter_contract, primitive_id, parameter_state_hashes,
  solution_by_scope, integration_rule, code_hash, provenance
)

scmix_v22_integration_target_evidence(
  analysis_contract, registry, integration_contract,
  structural_parameter_contract, integration_rule,
  parameter_state_hashes, targets, standard_errors, influence,
  respondent_id, riesz_solution_artifacts, code_hash, provenance
)

scmix_v22_integration_evaluation(
  analysis_contract, registry, integration_contract,
  structural_parameter_contract,
  role = c("level_a_production", "level_b_fixed_parameters",
    "level_b_repolished"),
  objective, score, covariance, target_evidence,
  optimizer_evidence = NULL, calibration_artifact = NULL,
  protocol_mode = c("calibration", "production")
)

scmix_v22_integration_compare(
  level_a, fixed_b, repolished_b, analysis_contract, registry,
  integration_contract, structural_parameter_contract,
  calibration_artifact = NULL,
  protocol_mode = c("calibration", "production")
)

scmix_v22_integration_refinement(
  level_a, level_b_rule, fixed_parameter_evaluator, repolisher,
  repolished_evaluator, ...
)

scmix_v22_validate_computation_artifact(
  artifact, analysis_contract, registry, calibration_artifact
)
}
\arguments{
  \item{application_id, specification_id}{Frozen application and specification
  identifiers.}
  \item{q}{Prespecified nonnegative factor rank.}
  \item{covariance_coordinate_ids}{Ordered raw-scale utility-coefficient IDs.
  These define the structural covariance dimension \code{p}.}
  \item{score_coordinate_ids}{Ordered structural-orthonormal score IDs.}
  \item{rule_id, method, level, nodes, weights, nesting_supported}{The exact
  integration rule, including its ordered nodes or draws and optional weights.}
  \item{structural_parameter_contract}{An intact signed structural-coordinate
  contract.}
  \item{registry}{The complete list of intact signed v2.2 target
  specifications.}
  \item{uses_numerical_integration}{Whether the fit uses numerical
  integration.}
  \item{level_a_rule, level_b_rule}{Intact signed production and
  higher-resolution rules.}
  \item{integrated_target_ids, integrated_primitive_ids}{Prespecified target
  and primitive IDs that use integration.}
  \item{nonnesting_reason}{Prespecified reason when the method cannot nest.}
  \item{fit_id, outer_fold_ids, starts}{The exact fit, fold, and optimizer-start
  registry.}
  \item{analysis_contract}{An intact signed v2.2 analysis contract whose
  integration and optimizer fields contain the exact computation-contract
  fingerprints.}
  \item{integration_contract}{An intact signed integration contract.}
  \item{integration_rule}{The exact signed rule used for the evidence.}
  \item{fit_scope_id, seed}{Registered optimizer scope and seed.}
  \item{optimizer_artifact}{The portable per-start optimizer record. The
  constructor derives the start, state, objective, precision, polish, KKT,
  calibration, and network-state evidence from this object.}
  \item{role, initial_state_hash}{Branch role and the exact level-A origin for
  a level-B repolish.}
  \item{objective}{Named full-and-fold values of the frozen lower-is-better
  mean complete-sequence objective.}
  \item{optimizer_evidence}{Intact per-start evidence. For an integration
  evaluation this is exactly one selected object per full/fold scope.}
  \item{reported_estimates, targets, standard_errors}{Registered target values
  and respondent-influence standard errors in exact registry order.}
  \item{evaluations}{One intact target recomputation for every registered
  fit-scope and start pair.}
  \item{calibration_artifact}{The intact exact Step-3 calibration artifact.
  It is required for production and omitted during calibration.}
  \item{protocol_mode}{Either \code{"calibration"} or \code{"production"}.}
  \item{nested_contract}{An intact exact restricted-to-full nesting contract.}
  \item{restricted_analysis_contract, full_analysis_contract,
  restricted_structural_contract, full_structural_contract}{The exact paired
  analysis and structural contracts.}
  \item{embedding_matrix, restriction_offset, verification_artifact_hash}{The
  named structural embedding, its offset, and independent exact-embedding
  verification hash.}
  \item{restricted_registry, full_registry, restricted_integration_contract,
  full_integration_contract}{The paired specification registries and signed
  integration contracts.}
  \item{restricted_optimizer_evidence, full_optimizer_evidence}{One selected
  optimizer artifact for every paired full and outer-fold fit scope.}
  \item{restricted_calibration_artifact, full_calibration_artifact}{Exact
  analysis-linked Step-3 calibration artifacts for the pair.}
  \item{primitive_id, parameter_state_hashes, solution_by_scope}{A primitive
  ID, all full/fold state hashes, and all full/fold Riesz solutions.}
  \item{code_hash, provenance}{Immutable recomputation code and provenance.}
  \item{influence, respondent_id, riesz_solution_artifacts}{Centered
  respondent-by-primitive raw one-step influences, respondent order, and one
  rule-bound Riesz artifact per primitive.}
  \item{target_evidence}{Intact rule-bound target, influence, and Riesz
  recomputation evidence.}
  \item{score, covariance}{Named full/fold score vectors and raw-scale
  structural covariance matrices. Covariance matrices must be \code{p} by
  \code{p}, never \code{q} by \code{q}.}
  \item{level_a, fixed_b, repolished_b}{Intact signed evaluation artifacts for
  the production rule, fixed-parameter higher-resolution branch, and
  repolished higher-resolution branch.}
  \item{fixed_parameter_evaluator, repolisher, repolished_evaluator}{Callbacks
  that execute the two required level-B branches.}
  \item{...}{Arguments passed to \code{scmix_v22_integration_compare}.}
  \item{artifact}{An intact F007, F008, or I001--I005 computation artifact.}
}
\value{
Constructors return fingerprinted protocol-v2.2 evidence artifacts.
\code{scmix_v22_multistart_stability()} returns F008 rows,
\code{scmix_v22_nested_fit_stability()} returns F007 evidence, and
\code{scmix_v22_integration_compare()} returns I001--I005 rows and the
worse-of-two-branches metrics. A field named
\code{selected_gate_subset_passed} describes only the gates calculated by the
returned object. It is not an application-level eligibility claim.
}
\description{
These functions implement the fail-closed computation evidence layer of the
frozen inference protocol v2.2. Production constructors require the exact
analysis-linked calibration artifact. They validate evidence supplied by the
application runner and do not refit models or choose Step-3 thresholds.
}
