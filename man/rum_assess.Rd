\name{rum_assess}
\alias{rum_assess}
\alias{rum_moment_bank}
\alias{rum_holdout_assess}
\alias{rum_score_compare}
\title{Respondent Moment Assessment and Independent Prediction Comparisons}
\description{Assess a fixed bank of observable respondent moments, distinguishing known parameters, same-sample fitting and independent prediction.}
\usage{
rum_moment_bank(data, marginal_weights = NULL, pair_weights = NULL,
                count_weights = NULL, names = NULL)
rum_assess(data, par = NULL, bank, quadrature = NULL, estimated = FALSE,
           fit = NULL, alpha = .05, nsim = 1999L, seed = 1L)
rum_holdout_assess(data, par, bank, quadrature, alpha = .05,
                   nsim = 1999L, seed = 1L,
                   method = c("gaussian", "hoeffding"),
                   tolerances = NULL, residual_bounds = NULL)
rum_score_compare(data, par_a, par_b, quadrature, alpha = .05,
                  quadrature_b = quadrature)
}
\arguments{
\item{data}{Complete respondent data. Independent test data for holdout methods.}
\item{marginal_weights,pair_weights,count_weights}{Numeric matrices with a common positive column count J. Rows concatenate tasks, pairs ordered by t then s greater than t, and counts k=0 through T, respectively, within each respondent. At least one matrix is required; missing components are zero. Must be chosen without outcome selection.}
\item{names}{Unique names for the J fixed moments.}
\item{par,par_a,par_b}{Parameter vectors. A and B use the same feature/mean parameter interface.}
\item{bank}{A moment bank matching the complete design, basis and respondent IDs. Observed responses are refreshed from data on each assessment call.}
\item{quadrature,quadrature_b}{Integration rules. Separate B rule permits a fixed distribution sensitivity comparison.}
\item{estimated}{Use same-sample composite-null influence adjustment. Requires the exact fitted data, parameters and rule.}
\item{fit}{Same-sample fitted object, required when estimated is true.}
\item{alpha}{Error probability for a simultaneous bank or a paired score interval.}
\item{nsim}{Number of Gaussian multiplier draws.}
\item{seed}{Private integer multiplier seed.}
\item{method}{Gaussian multiplier or bounded Hoeffding holdout intervals.}
\item{tolerances}{Optional J fixed nonnegative discrepancy tolerances.}
\item{residual_bounds}{Required for Hoeffding: J-by-2 declared lower/upper residual ranges valid over the target population, not merely empirical ranges.}
}
\details{For residual r=f-Ef and average moment derivative A, known-parameter assessment uses the covariance of r. Regular same-fit assessment uses the covariance of r-A I-inverse S. The plug-in expression C-A I-inverse A' is retained as an identity diagnostic; empirical cross moments need not equal their model expectations in finite samples. Degenerate coordinates do not acquire a variance floor; any untested nontrivial coordinates qualify the family result. These bands are model-null compatibility approximations, not robust discrepancy confidence intervals under misspecification.

Independent holdout assessment targets a fixed trained predictor's observable discrepancy, conditional on independent training. Its Gaussian mode assumes nondegenerate iid test-respondent influence; its Hoeffding mode uses externally justified finite ranges. Neither establishes latent-target bias control. Paired log scores use full respondent sequences, with a conditional iid-test CLT; overlapping-training cross-validation is not equivalent.}
\value{Moment banks store weights, matching design and observed summaries. Assessments return tables, expectations, residuals, derivatives, covariance/influence details when applicable, calibration results and scope qualifications. Score comparison returns the paired mean log-score difference, standard error, interval and respondent differences.}
\seealso{\code{\link{rum_loglik}}, \code{\link{rum_targets}}}
