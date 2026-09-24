\name{rum_fit}
\alias{rum_fit}
\title{Fit the Finite One-Policy-Factor Random Utility Model}
\description{Use bounded likelihood optimization, multiple declared starts, explicit boundary candidates and quadrature refinement with refitting.}
\usage{
rum_fit(data, starts = NULL, orders = c(9L, 15L, 21L),
        control = list(), policy_law = "normal", policy_rank = 1L)
}
\arguments{
\item{data}{A \code{rum_data} object.}
\item{starts}{Optional parameter vector, list of vectors, or matrix with starts in rows. Defaults are deterministic and do not use a simulation truth. Zero-policy and zero-position optimizations are also performed.}
\item{orders}{Strictly increasing sequence of integration orders. Each stage uses this order in both latent dimensions under the normal model.}
\item{control}{Named list overriding numerical defaults. See details.}
\item{policy_law}{\code{"normal"} or \code{"three_point"}.}
\item{policy_rank}{One for the full policy model, or zero for a nested comparator. Full-model regular target inference is not supplied for rank zero.}
}
\details{
Default controls: maxit=300, factr=1e5, pgtol=1e-8, max_seconds=120,
max_evaluations=8000, coefficient_bound=12, loading_bound=6,
position_bound=16, score_tolerance=2e-5, score_se_tolerance=.01,
integration_se_tolerance=.03, boundary_tolerance=1e-8, keep_starts=2,
hessian_step=1e-4, hessian_curvature_tolerance=1e-6.
Bounds are numerical bounds on the unpenalized criterion. Active artificial bounds qualify the result. Score/refinement tolerances concern numerical error in sampling-scale units; they are not statistical model checks.

Every likelihood call, including final Hessian differences, is counted and checked against the declared budget before dispatch. A single native call cannot be interrupted by this R-level timer. A budget-exhausted result retains the best finite evaluated points and withholds unresolved regular inference. If no finite point was evaluated, a \code{rum_fit_error} condition retains candidates and counts.

The fitted loading sign is anchored at its largest absolute coordinate; invariant covariance targets are unaffected. Candidate records retain initial vectors, initial objectives, returned best evaluated points, modes, messages and runtime. Finite evaluated points can survive failed optimizers; their numerical qualification is retained. This is not proof of global optimization.

A \code{regular_candidate} flag concerns numerical completion and an interior fitted parameter only. It does not verify identification, true interiority, correct specification or coverage. The interpretation of regular intervals remains conditional on those assumptions.}
\value{A \code{rum_fit} object containing data, parameters, respondent scores/log likelihoods, information matrices, candidate and refinement records, numerical issues, status, controls and computation counts.}
\seealso{\code{\link{rum_targets}}, \code{\link{rum_assess}}}
