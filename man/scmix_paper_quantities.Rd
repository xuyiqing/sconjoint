\name{scmix_paper_quantities}
\alias{scmix_paper_quantities}
\alias{scmix_paper_theta}
\alias{scmix_paper_choice}
\alias{scmix_paper_mrs}
\alias{scmix_paper_signshare}
\alias{scmix_paper_compensating}
\alias{scmix_paper_heterogeneity}
\title{Structural plug-in quantities under the fitted mixed-logit model}
\usage{
scmix_paper_theta(fit, subgroup = NULL, mu = NULL)

scmix_paper_choice(fit, contrast, position_neutral = FALSE,
  kappa = NULL, mu = NULL, Sigma = NULL, n_nodes = 31L,
  on_support = NA)

scmix_paper_mrs(fit, numerator, denominator,
  denominator_margin = NULL, mu = NULL)

scmix_paper_signshare(fit, contrast, ties = c("exclude", "include"),
  variance_margin = NULL, ci = NULL, mu = NULL, Sigma = NULL)

scmix_paper_compensating(fit, penalty, benefit, amount,
  variance_margin = NULL, ci = NULL, mu = NULL, Sigma = NULL)

scmix_paper_heterogeneity(fit, direction = NULL, total_margin = NULL,
  mu = NULL, Sigma = NULL)

scmix_paper_quantities(fit, subgroup = NULL, choice = NULL, mrs = NULL,
  sign = NULL, compensating = NULL, heterogeneity = NULL)
}
\arguments{
\item{fit}{An \code{scmix} fit. Full-sample structural estimates are used when
available.}
\item{subgroup}{Optional prespecified subgroup indicator.}
\item{contrast, numerator, denominator, penalty, benefit, direction}{Coefficient
contrasts defining the requested quantity.}
\item{position_neutral}{Average across both display orientations.}
\item{kappa, mu, Sigma}{Optional explicit structural estimates.}
\item{n_nodes}{One-dimensional normal quadrature resolution for a projected
normal index.}
\item{on_support}{Result of the application design audit.}
\item{denominator_margin, variance_margin, total_margin}{Prespecified reporting
gates.}
\item{ties}{Whether exact threshold ties are excluded or included.}
\item{ci}{Optional interval used only for a majority-reporting gate.}
\item{amount}{Compensating benefit amount.}
\item{choice, mrs, sign, compensating, heterogeneity}{Optional named argument
lists passed by the collector to the corresponding routine.}
}
\value{
Auditable plug-in objects containing the estimate, structural source, and
applicable reporting gates.
}
\description{
Computes respondent-weighted population and subgroup quantities by integrating
the fitted conditional normal preference distribution. These functions never
form empirical distributions of respondent posterior means or modes.
}
\details{
The marginal rate of substitution is the ratio evaluated at average
preferences, not the mean of individual ratios. Sign and compensating shares
use the maintained normal shape and explicit zero-variance tie conventions.
Regular intervals require \code{scmix_dml()} or an appropriate smooth
transformation of its primitive estimates.
}
