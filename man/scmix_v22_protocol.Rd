\name{scmix_v22_protocol}
\alias{scmix_v22_protocol}
\alias{scmix_v22_domain_amendment}
\alias{scmix_v22_target_spec}
\alias{scmix_v22_analysis_contract}
\alias{scmix_v22_calibration_hash}
\alias{scmix_v22_calibration_artifact}
\alias{scmix_v22_full_plugin_artifact}
\alias{scmix_v22_multiplier_bank}
\alias{scmix_v22_run_inner_refits}
\alias{scmix_v22_riesz_evidence}
\alias{scmix_v22_derivative_audit}
\alias{scmix_v22_gate_record}
\alias{scmix_v22_gate_template}
\alias{scmix_v22_validate_gate_record}
\alias{scmix_v22_gate_artifact}
\alias{scmix_v22_status}
\alias{scmix_v22_collect}
\alias{scmix_v22_finalize_collections}
\alias{scmix_v22_finalize}
\alias{print.scmix_v22_inference}
\title{Frozen protocol-v2.2 inference artifacts and finalization}
\usage{
scmix_v22_domain_amendment(target_type, domain, reason, provenance)

scmix_v22_target_spec(
  target_id, family_id,
  target_type = c(
    "unrestricted_scalar", "positive_scalar", "probability",
    "preference_direction_share", "compensating_benefit_share",
    "subgroup_share", "directional_heterogeneity",
    "mean_tradeoff_mrs_ratio"
  ),
  supporting_names, primitive_registry_ids, orientation, units,
  parameter_dependencies,
  family_primitive_registry_ids = primitive_registry_ids,
  family_target_ids = target_id,
  confirmatory_status = c("confirmatory", "sensitivity", "outcome_informed"),
  uses_numerical_integration = TRUE,
  domain = NULL, reference_rule = "none",
  influence_definition = "centered_raw_one_step_respondent_sequence",
  primitive_estimands = NULL, primitive_orientations = NULL,
  primitive_units = NULL, primitive_reference_rules = NULL,
  primitive_influence_definitions = NULL
)

scmix_v22_analysis_contract(
  analysis_signature, application_id, specification_id, specification_role,
  respondent_sample_rule, completion_rule, missing_data_rule,
  outer_folds, inner_folds, preprocessing_rule, integration_rule,
  optimizer_starts, learner_grid, ridge_grid, seeds,
  code_commit, data_hash, design_hash, calibration_hash,
  q, fit_uses_numerical_integration, exact_nested_pair_registered,
  derivative_coordinate_registry, derivative_coordinate_subset,
  derivative_subset_rule
)

scmix_v22_calibration_hash(values, source_hash)

scmix_v22_calibration_artifact(
  values, analysis_contract, source_hash, provenance
)

scmix_v22_full_plugin_artifact(
  inference, values, analysis_contract, final_fit_hash, code_hash, provenance
)

scmix_v22_multiplier_bank(
  respondent_id, draws = 9999L, seed = 1L,
  distribution = "normal", analysis_contract = NULL
)

scmix_v22_run_inner_refits(
  fit, primitive_registry_ids, inner_folds = 3L, refit,
  seed = 1L, code_hash, provenance,
  analysis_contract = fit$v22_analysis_contract
)

scmix_v22_riesz_evidence(
  fit, nested_refit, ridge_sequence_provenance,
  analysis_contract = fit$v22_analysis_contract
)

scmix_v22_derivative_audit(
  fit, fold_coordinates, primitive_registry_ids, evaluate,
  analytic_gradient = NULL,
  coordinate_subset = names(fold_coordinates[[1L]]),
  subset_rule = "all registered structural coordinates",
  code_hash, provenance,
  analysis_contract = fit$v22_analysis_contract
)

scmix_v22_gate_record(
  gate_id, category, scope, severity, state, statistic, threshold,
  comparator, units, provenance, message, scope_id = NA_character_
)

scmix_v22_gate_template()

scmix_v22_validate_gate_record(gates, require_all_ids = TRUE)

scmix_v22_gate_artifact(gates, registry, analysis_contract, provenance)

scmix_v22_status(
  gates, target_id, family_id, primitive_ids = character(),
  parameter_dependencies = character(), release_class = "none",
  interval_method = "none", multiplicity_status = "none",
  complete_external_record = FALSE
)

scmix_v22_collect(
  inference, registry, full_sample_plugin = NULL,
  analysis_contract = inference$v22_analysis_contract,
  derivative_audit = inference$v22_derivative_audit
)

scmix_v22_finalize_collections(
  collections, multiplier_bank = NULL, calibration = NULL,
  external_gates = NULL, complete_external_record = FALSE, level = 0.95
)

scmix_v22_finalize(
  inference, registry, full_sample_plugin = NULL,
  analysis_contract = inference$v22_analysis_contract,
  derivative_audit = inference$v22_derivative_audit,
  multiplier_bank = NULL, calibration = NULL, external_gates = NULL,
  complete_external_record = FALSE, level = 0.95
)

\method{print}{scmix_v22_inference}(x, ...)
}
\arguments{
\item{target_type}{A frozen target type.}
\item{domain}{A replacement domain carried by a signed amendment.}
\item{reason, provenance}{Nonempty audit documentation.}
\item{target_id, family_id}{Stable target and substantive-family IDs.}
\item{supporting_names}{Source columns used to construct the target.}
\item{primitive_registry_ids}{IDs for the minimal scalar primitives.}
\item{orientation, units, parameter_dependencies}{The declared orientation,
units, and structural-block dependencies.}
\item{family_primitive_registry_ids, family_target_ids}{The complete frozen
primitive and target membership of the family.}
\item{confirmatory_status}{The target's prespecified reporting role.}
\item{uses_numerical_integration}{Whether the target requires the frozen
numerical-integration refinement checks.}
\item{reference_rule, influence_definition, primitive_estimands,
primitive_orientations, primitive_units, primitive_reference_rules,
primitive_influence_definitions}{Target and primitive metadata used to reject
incompatible duplicate IDs.}
\item{analysis_signature, application_id, specification_id,
specification_role}{Fit-linked analysis identifiers.}
\item{respondent_sample_rule, completion_rule, missing_data_rule,
preprocessing_rule, integration_rule}{Frozen analysis rules.}
\item{outer_folds, inner_folds, optimizer_starts, learner_grid, ridge_grid,
seeds}{Frozen folds, grids, starts, and named seeds.}
\item{code_commit, data_hash, design_hash}{Immutable analysis hashes.}
\item{calibration_hash}{Hash of the exact calibrated constants and source.}
\item{q}{The frozen nonnegative latent-factor dimension.}
\item{fit_uses_numerical_integration, exact_nested_pair_registered}{Frozen
logical declarations governing fit-level integration and nested-fit evidence.}
\item{derivative_coordinate_registry, derivative_coordinate_subset,
derivative_subset_rule}{The pre-fit structural-coordinate registry, audited
subset in registry order, and deterministic selection rule.}
\item{values}{Named numeric calibration constants or plug-in values.}
\item{source_hash}{Immutable hash of the calibration source file.}
\item{inference}{A computed \code{scmix_dml} object.}
\item{analysis_contract}{An intact \code{scmix_v22_analysis_contract}. For a
derivative audit it freezes the coordinate registry, subset, rule, folds, and
code hash.}
\item{final_fit_hash, code_hash}{Immutable final-fit and code identifiers.}
\item{respondent_id}{Unique respondent identifiers.}
\item{draws, seed, distribution}{Multiplier count, signed seed, and Normal
multiplier law. Final simultaneous status requires exactly 9,999 draws.}
\item{fit}{A fit carrying respondent-level outer folds and the signed analysis
signature.}
\item{refit}{A callback that refits all outcome-dependent inner-training
nuisances and returns exact validation risks and linked hashes.}
\item{nested_refit}{An intact trace from \code{scmix_v22_run_inner_refits}.}
\item{ridge_sequence_provenance}{Documentation for the frozen vanishing grid.}
\item{fold_coordinates}{Named structural-orthonormal coordinates by outer
fold.}
\item{evaluate}{A primitive evaluator in the declared coordinate order.}
\item{analytic_gradient}{Optional analytic primitive derivatives by fold.}
\item{coordinate_subset, subset_rule}{A deterministic signed coordinate
subset and its rule.}
\item{gate_id, category, scope, severity, state, statistic, threshold,
comparator, message}{Fields of one frozen gate record.}
\item{scope_id}{The explicit analysis, fit, target, primitive, family, or block
identifier to which a gate row applies.}
\item{gates}{A v2.2 gate data frame.}
\item{require_all_ids}{Whether every frozen gate ID must be present.}
\item{registry}{A list of intact \code{scmix_v22_target_spec} objects.}
\item{primitive_ids}{Primitive IDs relevant to one target.}
\item{release_class, interval_method, multiplicity_status}{Proposed status
labels, which hard failures may withhold or demote.}
\item{complete_external_record}{Whether the complete signed external gate
record has validated.}
\item{full_sample_plugin}{An intact full-fit plug-in artifact.}
\item{derivative_audit}{An intact structural-coordinate derivative artifact.}
\item{collections}{One or more target-job collections.}
\item{multiplier_bank}{One analysis-linked common respondent multiplier bank.}
\item{calibration}{An intact, exact-values calibration artifact.}
\item{external_gates}{A complete scoped gate artifact.}
\item{level}{Confidence level.}
\item{x}{A finalized \code{scmix_v22_inference} object.}
\item{...}{Unused print arguments.}
}
\description{
Constructs fingerprinted artifacts and performs fail-closed finalization under
the frozen paperps inference protocol v2.2. The finalizer retains the
full-sample plug-in, cross-fit plug-in, raw one-step value, and range-respecting
reported value as distinct quantities. It maps primitive confidence rectangles
through the declared constraints and never treats pending calibration constants
or unsigned evidence as production certification.
}
\details{
Riesz tuning is respondent-nested and separate by minimal scalar primitive.
The derivative audit uses centered differences at structural-orthonormal steps
\eqn{h_j=\epsilon^{1/3}\max(1,|u_j|)} and \eqn{h_j/2}. Multi-primitive
families reuse one analysis-linked bank of respondent multipliers. Separate job
collections must be assembled before the family critical value is computed.
}
\value{
Constructors return fingerprinted list artifacts. Gate validation returns its
input invisibly. Finalizers return a \code{scmix_v22_inference} object with
target results, constrained confidence sets, full gate records, and distinct
fit, target, multiplicity, and interpretation statuses.
}
