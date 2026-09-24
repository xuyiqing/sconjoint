\name{rum_loglik}
\alias{rum_loglik}
\alias{rum_quadrature}
\alias{rum_information}
\title{Joint Respondent Likelihood, Scores and Information}
\description{Integrate a complete task product over shared policy and position latent effects using a stable compiled core.}
\usage{
rum_quadrature(order = c(11L, 11L), policy_law = c("normal", "three_point"))
rum_loglik(data, par, quadrature = rum_quadrature())
rum_information(data, par, quadrature = rum_quadrature(), step = 1e-4)
}
\arguments{
\item{order}{One or two standard-normal quadrature orders from 2 to 80, for policy and position. Under the separate three-point policy law the first order does not change its three fixed support points.}
\item{policy_law}{Normal policy factor or fixed symmetric three-point sensitivity distribution.}
\item{data}{A \code{rum_data} object.}
\item{par}{Parameter vector kappa, alpha, loading, omega.}
\item{quadrature}{A \code{rum_quadrature} rule.}
\item{step}{Positive relative finite-difference step for differentiating the analytic score.}
}
\details{Scores include every nuisance parameter. At zero position variance, the right derivative uses one half of the second derivative with respect to a common index shift. This handles a numerical derivative; it does not establish regular boundary inference. Information uses average respondent score outer products and a separately calculated negative average Hessian. Their sample difference is a diagnostic, not a specification test.}
\value{Quadrature returns nodes, weights, orders and distribution. Likelihood returns a respondent log-likelihood vector, respondent score matrix, mean objective and mean gradient. Information returns OPG and Hessian matrices, eigenvalues and their sample difference.}
\seealso{\code{\link{rum_fit}}, \code{\link{rum_assess}}}
