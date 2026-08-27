\name{scmix_assess}
\alias{scmix_assess}
\alias{scmix_design_benchmark}
\alias{scmix_design_benchmark_neutral}
\alias{scmix_structural_design_discrepancy}
\alias{scmix_design_audit}
\alias{scmix_heldout_sequence_score}
\alias{scmix_heldout_calibration}
\alias{scmix_heldout_predictions}
\alias{scmix_prediction_assessment}
\alias{scmix_completion_diagnostics}
\alias{scmix_rank_gate}
\alias{scmix_numerical_gate}
\alias{scmix_local_information}
\alias{scmix_profile_sequence_likelihood}
\alias{scmix_discrepancy_multiplier}
\alias{scmix_structural_sensitivity}
\alias{scmix_run_structural_sensitivity}
\alias{scmix_reporting_gates}
\alias{print.scmix_design_audit}
\alias{as.data.frame.scmix_design_audit}
\alias{print.scmix_reporting_gates}
\alias{as.data.frame.scmix_reporting_gates}
\alias{print.scmix_assessment}
\alias{as.data.frame.scmix_assessment}
\title{Specification assessment for the low-rank normal mixed logit}
\description{
Tools for auditing theorem applicability, comparing structural estimates with
a protocol-weighted on-support benchmark, assessing held-out sequence fit and
calibration, diagnosing local information and numerical stability, recording
structural sensitivities, and enforcing quantity-specific reporting gates.
}
\usage{
scmix_design_benchmark(deltaX, y, respondent_id, contrast,
  protocol_probability, tol = 1e-8)

scmix_design_benchmark_neutral(positive, negative)

scmix_structural_design_discrepancy(structural_estimate, design,
  structural_influence = NULL, target_label = NULL,
  structural_contrast = NULL, require_verified = TRUE)

scmix_design_audit(contrasts, q, protocol_support = NULL,
  protocol_strata = NULL, deltaX = NULL, respondent_id = NULL,
  task_order = NULL, covariance_injective = NA,
  injectivity_certificate = NULL, tol = 1e-8)

scmix_heldout_sequence_score(loglik, respondent_id = NULL,
  out_of_fold = FALSE, training_only_tuning = FALSE, provenance = NULL,
  analysis_signature = NULL)

scmix_heldout_calibration(y, predicted, respondent_id,
  probability_breaks = seq(0, 1, by = 0.1), design_cell = NULL,
  respondent_group = NULL, task_order = NULL, joint = NULL,
  out_of_fold = FALSE, training_only_tuning = FALSE, provenance = NULL,
  analysis_signature = NULL)

scmix_heldout_predictions(fit, task_order = NULL, include_counts = TRUE,
  include_adjacent = TRUE, include_repeated = TRUE, repeat_tol = 1e-8)

scmix_prediction_assessment(predictions, design_cell = NULL,
  respondent_group = NULL)

scmix_completion_diagnostics(completed_tasks, predictors = NULL,
  early_response = NULL, completion_pattern = NULL, respondent_id = NULL)

scmix_rank_gate(Sigma, q, eigenvalue_margin = NULL, rank_tol = 1e-8,
  structural_scale = NULL, absolute_floor = sqrt(.Machine$double.eps))

scmix_numerical_gate(checks, tolerances, reference = nrow(checks))

scmix_local_information(score, structural_norm, respondent_id = NULL,
  direction_labels = colnames(score), identified_directions = FALSE,
  provenance = NULL, norm_tol = 1e-10)

scmix_profile_sequence_likelihood(grid, loglik, respondent_id = NULL,
  direction = "structural direction", nuisance_reoptimized = FALSE,
  rank_boundary = FALSE, sieve_tuning_fixed = FALSE, provenance = NULL)

scmix_discrepancy_multiplier(discrepancies, R = 999L, level = 0.95,
  seed = NULL, multipliers = NULL)

scmix_structural_sensitivity(results = list(), q_values = NULL,
  materiality_tolerances = NULL, prespecified = FALSE)

scmix_run_structural_sensitivity(runners, provenance, context = list(),
  q_values = NULL, materiality_tolerances = NULL, prespecified = FALSE)

scmix_reporting_gates(mrs_denominator = NULL, mrs_margin = NULL,
  total_heterogeneity = NULL, total_margin = NULL,
  residual_variance = NULL, residual_variance_margin = NULL,
  confidence_interval = NULL, on_support = NA, rank_gate = NULL,
  numerical_gate = NULL, weak_information = NULL,
  local_information = NULL, information_eigenvalue_margin = NULL,
  target = c("generic_regular", "mrs", "variance_share",
    "threshold_share", "structural_choice", "descriptive"),
  inference = NULL, target_label = NULL, optimization_gate = NULL,
  design_audit = NULL)

scmix_assess(fit = NULL, design_audit = NULL, design_benchmarks = NULL,
  discrepancies = NULL, heldout_scores = NULL, calibration = NULL,
  completion = NULL, local_information = NULL, profiles = NULL,
  numerical = NULL, sensitivity = NULL, reporting = NULL,
  inference = NULL, optimization = NULL, q = NULL,
  eigenvalue_margin = NULL, information_eigenvalue_margin = NULL,
  calibration_margins = NULL, required_reporting = NULL,
  required_discrepancies = NULL, discrepancy_tolerances = NULL)
}
\arguments{
\item{deltaX, y, respondent_id}{Task-level design, choices, and respondent
identifiers.}
\item{contrast}{Prespecified ordered contrast.}
\item{protocol_probability, protocol_support}{Known exposure probabilities
from the fielded randomization and completion protocol.}
\item{positive, negative}{Design benchmarks for opposite orientations.}
\item{structural_estimate, design, structural_influence, target_label,
structural_contrast, require_verified}{Structural-design comparison inputs.}
\item{contrasts, q, protocol_strata, covariance_injective,
injectivity_certificate, task_order, tol}{Identification-audit inputs.}
\item{loglik, predicted, probability_breaks, design_cell, respondent_group,
joint, out_of_fold, training_only_tuning, provenance}{Held-out fit,
calibration, and provenance inputs.}
\item{analysis_signature}{Optional nonempty signature linking a fit-aware score
or calibration object to the fitted analysis. The official prediction pipeline
supplies it.}
\item{include_counts, include_adjacent, include_repeated, repeat_tol,
predictions}{Fit-aware marginal, sequence, and joint prediction controls.}
\item{completed_tasks, predictors, early_response, completion_pattern}{Observed
completion diagnostics.}
\item{Sigma, eigenvalue_margin, rank_tol, structural_scale,
absolute_floor}{Rank-interiority gate.}
\item{checks, tolerances, reference}{Numerical-stability gate.}
\item{score, structural_norm, direction_labels, identified_directions,
norm_tol}{Complete-sequence local-information inputs in identified structural
directions.}
\item{grid, direction, nuisance_reoptimized, rank_boundary,
sieve_tuning_fixed}{Descriptive profile sequence-likelihood inputs.}
\item{discrepancies, R, level, seed, multipliers}{Common respondent multiplier
inputs.}
\item{results, q_values, materiality_tolerances, prespecified}{Sensitivity-plan
and result metadata. Rank stability requires at least two distinct values.
Every implemented component needs a named tolerance and result fields recording
that the matching tolerance was applied to a finite nonnegative materiality
value.}
\item{runners, context}{Application-specific sensitivity callbacks and their
read-only inputs.}
\item{mrs_denominator, mrs_margin, total_heterogeneity, total_margin,
residual_variance, residual_variance_margin, confidence_interval, on_support,
rank_gate, numerical_gate, weak_information, target, inference,
local_information, information_eigenvalue_margin,
optimization_gate}{Quantity-specific reporting-gate inputs. A legacy
\code{weak_information = FALSE} assertion cannot substitute for a verified
local-information object and positive prespecified eigenvalue margin.}
\item{fit, design_audit, design_benchmarks, heldout_scores, calibration,
completion, local_information, profiles, numerical, sensitivity, reporting,
inference, optimization}{
Precomputed components collected by \code{scmix_assess()}.}
\item{calibration_margins}{Named, strictly positive marginal and joint held-out
calibration-gap thresholds.}
\item{required_reporting}{Prespecified nonempty names of every quantity for
which regular structural reporting is requested.}
\item{required_discrepancies}{Prespecified names of every applicable on-support
structural--design comparison. \code{character(0)} explicitly declares that
there are none; \code{NULL} leaves scope undeclared and fails closed.}
\item{discrepancy_tolerances}{Named, strictly positive materiality thresholds
for all required discrepancies.}
}
\value{
An auditable assessment object appropriate to the requested component.
}
\details{
These exercises can reveal conflict with observable implications, weak local
information, numerical instability, or substantive sensitivity. They cannot
establish that normality, common covariance, noninformative completion, or
independent logit shocks is true. Failure of a sufficient design condition
leaves the paper's theorem silent; it is not proof of nonidentification.

The position-neutral benchmark accepts only objects whose ordered contrasts are
exact negatives. A supplied protocol-support table is never treated as its own
universe: \code{protocol_strata} must explicitly enumerate every required
design/completion stratum. The optional covariance-injectivity artifact must
inherit from \code{scmix_covariance_injectivity_certificate}; an unclassed list,
logical assertion, or prose certificate cannot establish injectivity. The
artifact must record its method, domain, provenance, and finite positive numeric
tolerance, together with the rank and contrast matrix to which the verification
applies.

For a \code{scmix_dml} structural estimate, joint discrepancy inference is
available only when the selected target is explicitly inference-available and
has a finite, strictly positive standard error. Both \code{"available"} and
\code{"conditional_available"} statuses are recognized; the latter is always
reported as conditional on documented high-level assumptions and never as
verification of those assumptions. Named contrasts are aligned by coefficient
name when the benchmark carries coefficient names; a fully specified named
contrast can also be compared, in benchmark order, with a legacy unnamed
benchmark. The assessment collector reports
\code{protocol_complete} separately from \code{structural_reporting_ready}.
Protocol completion means only that every prespecified exercise was executed
with the required provenance. Structural reporting also requires passing the
prespecified local-information, calibration-materiality, sensitivity,
quantity-specific reporting, and structural--design discrepancy gates. The
legacy \code{complete} field is an alias for \code{protocol_complete}; neither
field says that a maintained model assumption has been verified.

A duplicated rank list such as \code{c(1, 1)} is not a rank-stability exercise.
Sensitivity execution and substantive passage are separate: substantive passage
requires every applicable component to carry an explicit prespecified tolerance
and auditable result evidence that the tolerance was applied and passed.

When a fit is supplied, its nonempty \code{analysis_signature} must match the
inference result, optimization audit, and numerical or integration-refinement
artifact. Every supplied held-out score and calibration object must carry the
same nonempty signature. The official fit-aware prediction pipeline propagates
the fit signature through predictions into both assessment objects. Missing or
cross-analysis signatures fail the explicit artifact-signature gate and
withhold structural-reporting readiness.
}
