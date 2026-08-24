\name{scmix_tune_matrix}
\alias{scmix_tune_matrix}
\alias{scmix_tune_outer_matrix}
\alias{scmix_assemble_nested}
\alias{scmix_optimization_audit}
\alias{scmix_integration_refinement}
\alias{scmix_q_sensitivity}
\title{Respondent-level computation and tuning tools for scmix}
\usage{
scmix_tune_matrix(deltaX, y, Z, respondent_id, grid, q = NULL, K = 3L,
  allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
  n_epochs = 400L, learning_rate = 0.01,
  n_starts = 2L, mu_bound = 10, kappa_bound = 10,
  a_bound = 10, weight_bound = 10,
  opt_tol = 1e-7, grad_tol = 1e-4, seed = NULL, device = "cpu",
  refit = TRUE, refit_integration_grid = NULL,
  keep_cv_fits = FALSE, verbose = FALSE)

scmix_tune_outer_matrix(deltaX, y, Z, respondent_id, grid, q = NULL,
  outer_K = 5L, inner_K = 3L, outer_fold_id = NULL, seed = NULL, ...)

scmix_assemble_nested(nested, attr_names = NULL, z_names = NULL,
  require_optimization_gate = TRUE, diagnostic_only = FALSE)

scmix_optimization_audit(fit)

scmix_integration_refinement(resolutions, scrambles = NULL, refitter,
  extractors, tolerances, reference = NULL, keep_fits = FALSE)

scmix_q_sensitivity(primary_q, alternatives, refitter, extractors,
  keep_fits = FALSE)
}
\arguments{
\item{deltaX, y, Z, respondent_id}{Task-level contrasts, choices, moderators,
and respondent identifiers.}
\item{grid}{Finite prespecified tuning grid. QMC draw counts must be even
because the tuning workflow uses antithetic draws.}
\item{q}{Fixed maximum factor rank.}
\item{K, outer_K, inner_K, outer_fold_id}{Respondent-level fold controls.}
\item{allow_q_tuning}{Permit data-dependent rank comparison. Fixed-rank
inference is not selection-adjusted.}
\item{allow_integration_tuning}{Permit integration resolution to vary in
learner CV only as an explicitly diagnostic exercise.}
\item{n_epochs, learning_rate, n_starts, mu_bound, kappa_bound, opt_tol,
grad_tol, seed, device, verbose}{Optimization controls.}
\item{a_bound}{Positive raw-coefficient Frobenius bound for the loading.}
\item{weight_bound}{Positive coordinatewise bound for network weights and
biases.}
\item{refit, refit_integration_grid, keep_cv_fits, keep_fits}{Selected-refit,
common finite-grid, and object-retention controls.}
\item{nested, attr_names, z_names, require_optimization_gate,
diagnostic_only}{Nested-tuning assembly controls. Diagnostic assembly never
becomes eligible for ordinary fixed-specification inference. The ordinary
gate requires complete diagnostics showing that none of the artificial
compact-parameter bounds is active.}
\item{fit}{An \code{scmix}, tuned matrix fit, or assembled nested fit with
optimization diagnostics.}
\item{resolutions, scrambles}{Integration refinement settings.}
\item{refitter}{Function that performs a fresh fit at a supplied setting or
rank.}
\item{extractors}{Named functions recomputing quantities, standard errors,
held-out scores, or information diagnostics from every fresh fit.}
\item{tolerances, reference}{Prespecified numerical-stability rule.}
\item{primary_q, alternatives}{Prespecified primary and sensitivity ranks.}
\item{...}{Further tuning arguments passed inside each outer training set.}
}
\value{
Auditable tuning, optimization, integration-refinement, assembled-nuisance, or
rank-sensitivity objects. Fits and the two inference-facing audit artifacts
carry an \code{analysis_signature} when they can be linked to the same data,
model/learner specification, and respondent folds.
}
\description{
Implements complete-sequence respondent-level cross-validation, nested tuning,
multiple-start diagnostics, fresh-refit numerical refinement, and
prespecified-rank sensitivity. A tuning candidate enters selection only when
every inner-fold fit passes the fail-closed computational and compact-bound
gate and every complete-sequence validation score is finite. Diagnostics and
raw scores for excluded candidates are retained; tuning stops if none is
eligible.
}
\details{
All preprocessing and outcome-adaptive tuning for DML must occur inside the
relevant outer training respondents. Diagnostics do not certify the global
nonconvex optimum or the asymptotic numerical-error rates. A fixed number of
simulation draws is not treated as exact likelihood evaluation.
The analysis signature excludes fitted parameters and integration resolution;
the latter is recorded separately by every refinement row. A refinement
artifact is fit-linked only when all fresh refits carry the same signature.
}
