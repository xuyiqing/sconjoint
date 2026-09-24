\name{rum_targets}
\alias{rum_targets}
\title{Utility-Contrast Means and Heterogeneity Targets}
\description{Calculate fixed finite-model structural targets, full-information derivatives and conditional regular intervals.}
\usage{
rum_targets(fit, contrasts, reference = c("population", "fixed"),
            alpha = .05, infer = TRUE,
            hz_interval = c("wald", "fixed_rank_one_projection"))
}
\arguments{
\item{fit}{A fitted \code{rum_fit} object. Point-only evaluation needs data and parameters.}
\item{contrasts}{Named list of length-p vectors or N-by-p contrast matrices. Specify before selecting outcomes.}
\item{reference}{Population standardization includes the respondent-composition influence term; fixed standardization conditions on the supplied covariate reference bank.}
\item{alpha}{Marginal interval error probability.}
\item{infer}{Whether to request inference or only point values and derivatives.}
\item{hz_interval}{Default marginal Wald intervals for HZ, or projection of a regular scalar mean-parameter interval when the known fixed-reference HZ quadratic form has rank one. The latter requires reference fixed and rejects higher-rank forms rather than dropping directions.}
}
\details{For contrast c_i, let m_i=c_i' B_i alpha and e_i=c_i' loading. Targets include mean(m), HZ=variance(m) using denominator N, HR=mean(e squared), HT=HZ+HR, and corresponding pairwise KZ/KR/KT covariance decompositions. Position variance is a nuisance in inference and is excluded from policy heterogeneity targets.

The influence function adds the appropriate direct composition term to S'I-inverse times the target derivative. Fixed-contrast HR intervals map the entire signed-loading interval through squaring, including zero. Fixed-contrast KR intervals map a Bonferroni joint two-loading rectangle through multiplication. Other nondegenerate targets use marginal regular Wald intervals. These are not jointly simultaneous across all reported targets.

Literal full-factor or position-variance boundaries, unresolved numerical candidates, singular information and degenerate influence coordinates withhold the corresponding regular inference. No variance floor or ridge inverse is applied. Estimated interiority does not establish true interiority.}
\value{A list with a target table, analytic gradient, direct composition, influence and covariance matrices when available, and explicit inference context/assumptions.}
\seealso{\code{\link{rum_fit}}}
