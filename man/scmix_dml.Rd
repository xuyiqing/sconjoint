\name{scmix_dml}
\alias{scmix_dml}
\alias{print.scmix_dml}
\alias{scmix_inference_verification}
\alias{scmix_inference_target}
\alias{scmix_delta_transform}
\title{Respondent-level orthogonal inference for regular mixed-logit quantities}
\usage{
scmix_dml(
  fit,
  targets = "theta",
  plugin_targets = NULL,
  mu_by_fold = NULL,
  kappa_folds = NULL,
  mu_basis = NULL,
  nu_grid = NULL,
  riesz_penalty = "identity",
  riesz_validation_fraction = 0.2,
  active_eigenvalue_min = NULL,
  rank_tolerance = 1e-8,
  information_eigenvalue_min = NULL,
  derivative_eps = 1e-5,
  allow_numeric_derivatives = FALSE,
  derivative_refinement_factor = 2,
  derivative_refinement_tolerance = 1e-3,
  riesz_equation_tolerance = 5e-2,
  ridge_sensitivity_tolerance = 1e-1,
  verification = NULL,
  multiplier_draws = 999L,
  multiplier = c("normal", "rademacher"),
  level = 0.95,
  seed = 1L
)

scmix_inference_verification(
  fit,
  mu_basis,
  tangent,
  numerical,
  optimization,
  ridge = NULL
)

scmix_inference_target(
  type = c(
    "tau", "choice", "sign", "compensating",
    "subgroup_tau_primitives", "mrs_primitives",
    "heterogeneity_primitives", "covariance_primitives"
  ),
  contrast = NULL,
  numerator = NULL,
  denominator = NULL,
  subgroup = NULL,
  penalty = NULL,
  benefit = NULL,
  amount = NULL,
  position_neutral = FALSE,
  n_nodes = 31L,
  variance_floor = NULL,
  label = NULL
)

scmix_delta_transform(
  inference,
  type = c(
    "subgroup_ratio", "mrs", "directional_heterogeneity",
    "covariance_decomposition"
  ),
  primitives,
  denominator_margin = NULL,
  total_margin = NULL,
  level = 0.95
)
}
\arguments{
\item{fit}{For \code{scmix_dml()}, an \code{scmix}-like fit containing complete task sequences,
respondent-level outer folds, fold-specific mean and kappa estimates, loading
matrices, and common or fold-specific integration grids. For
\code{scmix_inference_verification()}, the exact fit whose nonempty
\code{analysis_signature} must match every audit artifact. An explicit
\code{diagnostic_only = TRUE}, an explicit
\code{eligible_for_ordinary_inference = FALSE}, or a malformed value in either
field fails closed; absence of both fields on a legacy fit is allowed.}

\item{targets}{Built-in regular targets; currently \code{"theta"}, the
respondent-weighted expectation of the conditional mean preference vector.}

\item{plugin_targets}{Optional named list of smooth callback functions for
rowwise-expectation primitives. A callback receives \code{mu}, \code{kappa},
\code{Sigma}, \code{Z}, \code{respondent_id}, \code{fold}, and
\code{attr_names}. It must return a list containing
\code{target_type = "rowwise_expectation"}, an N-row \code{value}, and
analytic \code{d_mu} and \code{d_kappa}; when relevant it supplies
\code{d_Sigma}, or declares \code{sigma_invariant = TRUE}. Raw loading factors
and raw-loading derivatives are not part of the public target interface.
Generic nonlinear functionals of the empirical distribution of Z require a
separately derived influence function and are rejected by this interface.}

\item{mu_by_fold, kappa_folds}{Optional fold-specific nuisance overrides.
Mean predictions must have one row per respondent or be constant within
respondent when supplied at task level.}

\item{mu_basis}{Finite local basis for conditional-mean tangent directions.
\code{NULL} uses an intercept and training-standardized linear moderator
terms. This default is diagnostic. A verification record must instead be tied
to an explicit growing or outer-fold-fitted sieve, and the current constructor
does not treat prose rate arguments as machine verification.}

\item{nu_grid}{\code{NULL} for the fold-size-dependent grid
\code{c(0, 1/N_train, 1/sqrt(N_train))}; a common finite nonnegative vector;
or a list with one grid per outer fold.}

\item{riesz_penalty}{A positive-semidefinite penalty in the declared
structural-orthonormal coordinates: \code{"identity"}, a diagonal vector, or a
square matrix.}

\item{riesz_validation_fraction}{Training-respondent fraction used to choose
among multiple Riesz penalties.}

\item{active_eigenvalue_min}{Explicit prespecified positive minimum active
eigenvalue of the loading covariance in raw coded coefficient units. \code{NULL} permits
a diagnostic calculation but cannot support conditional inference when
\code{q > 0}.}

\item{rank_tolerance}{Strictly positive relative numerical-rank tolerance.}

\item{information_eigenvalue_min}{Explicit prespecified strictly positive
minimum generalized complete-sequence information eigenvalue under the
declared structural norm. \code{NULL} permits a diagnostic calculation but
cannot support conditional inference.}

\item{derivative_eps}{Initial central-difference step for an explicitly
requested numerical callback derivative.}

\item{allow_numeric_derivatives}{Whether a callback may omit analytic
derivatives and use refined central differences. The default is \code{FALSE}.}

\item{derivative_refinement_factor, derivative_refinement_tolerance}{Step-size
refinement factor and maximum relative derivative discrepancy.}

\item{riesz_equation_tolerance}{Maximum relative unpenalized Riesz-equation
residual for conditionally supported inference.}

\item{ridge_sensitivity_tolerance}{Maximum relative change in the score
correction under the selected ridge versus zero ridge.}

\item{verification}{A record returned by
\code{scmix_inference_verification()}. An arbitrary list of booleans is not
accepted as verification. If this record is absent or incomplete, estimates
and diagnostic variance calculations may be returned with status
\code{"conditional_unverified"}, but ordinary intervals and multiplier inference
are withheld.}

\item{multiplier_draws, multiplier, level, seed}{Respondent-level multiplier
draw count, multiplier distribution, confidence level, and reproducibility
seed. Targets with zero or nonfinite standard errors are excluded from
studentized multiplier inference.}

\item{tangent}{For \code{scmix_inference_verification()}, a list with
\code{type = "growing_sieve"} or \code{"fitted_sieve"},
\code{prespecified = TRUE}, \code{identified_directions = TRUE}, and nonempty
\code{approximation_argument}, \code{product_rate_argument}, and
\code{provenance}. A fitted sieve must also be training-only and specific to
each outer fold; a growing sieve records increasing sample sizes and
dimensions. The constructor records but does not certify the asserted
approximation and product rates.}

\item{numerical}{A list containing an \code{artifact} returned by
\code{scmix_integration_refinement()}, a nonempty root-N-scale
\code{rate_argument}, and \code{provenance}. At least two fresh-refit settings
and a passing numerical gate are required. Its analysis signature must equal
the fit's, every retained refit signature must agree, and its checks must cover quantities of interest, likelihood,
score or derivative, Riesz solution, influence-function L2 error, and standard
errors.}

\item{optimization}{A list containing an \code{artifact} returned by
\code{scmix_optimization_audit()}, a nonempty approximate-maximization
\code{gap_argument}, and \code{provenance}. Attained-solution gates must pass
and no output bound may be active.}

\item{ridge}{Optional evidence for a caller-supplied positive ridge sequence:
a list with \code{vanishing_sequence = TRUE}, a nonempty
\code{rate_argument}, and \code{provenance}.}

\item{type}{For \code{scmix_inference_target()}, a supported paper primitive.
For \code{scmix_delta_transform()}, a supported nonlinear map of such
primitives. Arbitrary functionals of the empirical distribution of moderators
are outside this interface.}

\item{contrast}{A finite coefficient contrast for average utility, choice,
sign, subgroup, or directional-heterogeneity primitives. Named contrasts may
omit coefficients that are zero.}

\item{numerator, denominator}{Coefficient contrasts for the numerator and
denominator of mean-preference MRS primitives.}

\item{subgroup}{A respondent-level binary subgroup indicator, optionally named
by respondent identifier.}

\item{penalty, benefit, amount}{Contrasts and scalar amount defining a
compensating-benefit share.}

\item{position_neutral}{Whether a choice primitive averages the two display
positions.}

\item{n_nodes}{Number of one-dimensional normal quadrature nodes for a choice
primitive.}

\item{variance_floor}{Explicit prespecified positive
directional-residual-variance reporting gate required for regular sign and
compensating-share inference; unused types may leave it \code{NULL}.}

\item{label}{Optional label for a scalar primitive.}

\item{inference}{A computed \code{scmix_dml} object containing the named
primitive estimates and diagnostic covariance.}

\item{primitives}{The exact names, in documented order, of the primitive
targets to transform. Covariance decomposition uses all mean, lower-triangular
second-moment, and lower-triangular residual-covariance primitives returned by
\code{type = "covariance_primitives"}.}

\item{denominator_margin}{Positive prespecified reporting margin for a subgroup
probability or an MRS denominator.}

\item{total_margin}{Positive prespecified total-directional-heterogeneity
margin for reporting its explained share.}
}
\value{
\code{scmix_dml()} returns an object of class \code{scmix_dml}. Its status is
\code{"conditional_available"} when the rank, generalized-information,
derivative, Riesz, fit-linked numerical/optimization, and structured
high-level-assumption gates pass. Its intervals and multiplier results are
explicitly conditional on the documented growing/fitted-sieve approximation
and product-rate assumptions; this is not a claim that those assumptions were
verified by the data. A fit explicitly marked \code{diagnostic_only = TRUE} or
\code{eligible_for_ordinary_inference = FALSE} cannot receive this status;
legacy fits on which both fields are absent remain eligible for the other
checks. It is
\code{"conditional_unverified"} when diagnostic calculations exist but the
high-level record or automatic diagnostic gates are incomplete, and
\code{"withheld"} at a rank or information boundary.

\code{scmix_inference_verification()} returns a classed audit record. It
checks exact fit and basis linkage and the empirical status of supplied
artifacts. It rejects a fit carrying either adverse eligibility field and
rechecks those fields when the record is consumed. Because the current
implementation has no machine artifact that
establishes the growing-sieve approximation and product rates, the constructor
records typed arguments and provenance for those high-level assumptions. A
complete record may support \code{"conditional_available"}, never an
empirically \code{"verified"} status.

\code{scmix_inference_target()} returns a typed, rotation-invariant rowwise
callback. \code{scmix_delta_transform()} returns a gated delta-method object
for subgroup ratios, MRS, directional heterogeneity and its share, or the
lower-triangular entries of \eqn{\Omega_Z}, \eqn{\Omega_R}, and
\eqn{\Omega_T}. Formal covariance entries and intervals are withheld target by
target whenever primitive inference or a required reporting gate fails; a
\code{"conditional_available"} input propagates that conditional status.
}
\description{
Uses complete respondent-sequence Fisher scores, outer-fold evaluation, and
training-only Riesz fits to construct a cross-fitted one-step estimator. The
influence contribution includes the direct empirical-distribution-of-Z term.
The public target bridge implements the paper's prespecified rowwise
expectation primitives and named delta transformations; it does not claim a
generic arbitrary-\eqn{P_Z} functional API.
}
\details{
Information and Riesz calculations use the declared structural norm
\deqn{(\delta\kappa)^2 + E_{train}\|\delta\mu(Z)\|^2
      + \|\delta A_{horizontal}\|_F^2,}
so rescaling a finite mean basis does not change the result. Loading directions
that only rotate a factorization are removed, while callbacks operate through
\code{Sigma} and therefore cannot define factor-orientation-dependent targets.

Respondents, not tasks, are the sampling, folding, Riesz-validation, and
multiplier units. If \code{phi_i} denotes the centered respondent influence
contribution, the reported influence variance is
\code{crossprod(phi)/N}, and the estimator covariance is that matrix divided
by N.

The routine does not recover realized respondent preferences and does not
provide boundary-valid inference when the true covariance rank is below the
maintained rank. Passing empirical refinement and optimization audits does not
establish the paper's high-level asymptotic conditions; the resulting
inference remains conditional on the documented arguments in the verification
record.
}
