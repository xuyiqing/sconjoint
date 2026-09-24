\name{rum_data}
\alias{rum_data}
\alias{rum_mean_basis}
\alias{rum_parameters}
\alias{rum_subset}
\alias{rum_scale_features}
\alias{rum_scale_parameters}
\alias{rum_simulate}
\title{Complete Respondent Sequences and Finite Utility Bases}
\description{Construct and transform complete repeated binary-choice records without dropping tasks. Simulate a shared policy factor and a separate persistent position effect.}
\usage{
rum_data(y, d, mean_basis = NULL, ids = NULL, feature_names = NULL,
         mean_names = NULL, metadata = list())
rum_mean_basis(z, feature_names)
rum_parameters(data, kappa = 0, alpha = rep(0, data$r),
               loading = rep(0, data$p), position_variance = .25)
rum_subset(data, respondents)
rum_scale_features(data, scales)
rum_scale_parameters(data, par, scales, inverse = FALSE)
rum_simulate(data, par, seed = NULL, policy_law = "normal")
}
\arguments{
\item{y}{List of numeric binary response vectors, one per respondent.}
\item{d}{List of finite task-by-feature matrices, one per respondent.}
\item{mean_basis}{A common p-by-r matrix, or one such matrix per respondent. The mean is B_i alpha. Defaults to the identity.}
\item{ids}{Unique respondent identifiers.}
\item{feature_names,mean_names}{Unique nonempty feature and mean-coefficient names.}
\item{metadata}{Provenance list. Must include a nonempty string \code{orientation} documenting what y=1 means. Arbitrary metadata are preserved without reindexing; keep row-level source data keyed by respondent ID.}
\item{z}{Finite moderator-basis matrix; include an intercept explicitly.}
\item{data}{A \code{rum_data} object.}
\item{kappa}{Common physical-position utility constant.}
\item{alpha}{Finite mean-coefficient vector.}
\item{loading}{Policy-factor loading vector.}
\item{position_variance}{Nonnegative persistent position variance, omega.}
\item{respondents}{Unique respondent indices or IDs to retain.}
\item{scales}{Positive feature scales. Transforms d to d/scales, B to scales*B, and loading to scales*loading. Alpha is unchanged. Divide contrast coordinates by scales to preserve substantive targets.}
\item{par}{Parameter vector in order kappa, alpha, loading, omega.}
\item{inverse}{Reverse the loading-parameter scale transformation.}
\item{seed}{Optional private RNG seed, restoring the caller's RNG state on exit.}
\item{policy_law}{\code{"normal"} or the fixed symmetric \code{"three_point"} policy distribution. Position effects remain normal.}
}
\details{Each simulated respondent shares the same latent draws across tasks. Nonlinear profile features and moderator bases are allowed, but this mean is linear in its coefficients. The numerical records do not establish assignment exogeneity, physical orientation, or completion assumptions.}
\value{\code{rum_data}, \code{rum_subset}, \code{rum_scale_features} and \code{rum_simulate} return packed \code{rum_data} objects. \code{rum_mean_basis} returns a list of matrices. Parameter functions return numeric vectors.}
\seealso{\code{\link{rum_fit}}}
