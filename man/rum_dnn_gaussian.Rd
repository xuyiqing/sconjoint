\name{rum_dnn_gaussian}
\alias{rum_dnn_gaussian}
\alias{rum_dnn_gaussian_forward}
\alias{rum_dnn_gaussian_evaluate}
\alias{rum_dnn_gaussian_loss}
\title{Gaussian Structured-Network Sequence Likelihood and Gradients}
\description{
A dependency-free numeric reference network and differentiable likelihood for
one persistent Gaussian policy factor and an independent persistent Gaussian
recorded-label effect. Computes exact derivatives of the declared finite-node
likelihood and backpropagates them through a ReLU network. Does not train a
model, choose tuning parameters, compute intervals, or calibrate a test.
}
\usage{
rum_dnn_gaussian(p_z = 16L, p = 16L,
  hidden = c(32L, 32L, 16L), seed = 1L)

rum_dnn_gaussian_forward(network, Z)

rum_dnn_gaussian_evaluate(data, mu, kappa, loading, omega,
  quadrature = rum_quadrature())

rum_dnn_gaussian_loss(network, data, Z, kappa, loading, omega,
  quadrature = rum_quadrature())
}
\arguments{
\item{p_z}{Positive number of encoded respondent inputs.}
\item{p}{Positive number of utility-feature mean outputs.}
\item{hidden}{Positive integer hidden-layer widths. An empty integer vector
  gives an affine network for numerical comparisons.}
\item{seed}{Nonnegative integer base-R initialization seed. The caller's RNG
  state is restored, including absence of a previously initialized state.}
\item{network}{Numeric network from \code{rum_dnn_gaussian()}. Each
  \code{weights} matrix is input-by-output; each \code{biases} entry is a vector.}
\item{Z}{Finite numeric respondent-by-input matrix. Rows must follow
  \code{data$ids} for the loss wrapper. Supplied row names must match exactly;
  an unnamed matrix is interpreted positionally. Scaling is not automatic.}
\item{data}{Complete respondent sequences constructed by \code{rum_data()}.
  Unbalanced positive task counts are supported.}
\item{mu}{Finite numeric respondent-by-\code{p} conditional mean matrix,
  aligned with \code{data$ids} and \code{data$feature_names}. Supplied row names
  must match the respondent IDs exactly; unnamed rows are interpreted
  positionally. These outputs replace the finite mean basis stored in data.}
\item{kappa}{Finite common intercept for the recorded choice orientation.}
\item{loading}{Finite length-\code{p} policy-factor loading vector.}
\item{omega}{Nonnegative variance of the independent Gaussian label effect.}
\item{quadrature}{A positive normalized Gaussian rule from
  \code{rum_quadrature(policy_law = "normal")}. No non-Gaussian rule is accepted.}
}
\details{
The network has ReLU hidden layers and a linear output layer. Initialization
uses independent uniform weights and biases in
\eqn{[-1/\sqrt{d},1/\sqrt{d}]} for input width \eqn{d}. This is a numeric R
implementation; it is not claimed to reproduce a historical torch RNG stream.
The default 16-input, 16-output network has 2,400 weights and biases.

For respondent \eqn{i}, each quadrature pair \eqn{(u_q,v_q)} determines
\deqn{\eta_{itq}=\kappa+d_{it}'\mu_i+(d_{it}'\lambda)u_q+\sqrt{\omega}v_q.}
The likelihood sums the product of task Bernoulli probabilities over these
shared latent nodes. It is not the product of marginal mixture probabilities.
The existing native sequence engine uses stable softplus and log-sum-exp
arithmetic. One native call per respondent supplies scores for its mean outputs
and all common parameters, without constructing a large respondent-specific
finite-basis parameterization. Network derivatives then use matrix
backpropagation in R double precision.

\code{objective} is the average respondent log likelihood and \code{loss}
is its negative. Sequences are not divided by their task counts. All returned
scores are derivatives of the individual log likelihood before averaging.
\code{objective_gradient} differentiates the positive objective;
\code{loss_gradient} has the opposite sign. No penalty is included.

The ReLU derivative is taken as zero at an exactly zero preactivation. Such a
point can be a nondifferentiable kink; this convention is not a differentiability
claim. At \code{omega = 0}, the native heat-identity calculation supplies the
right derivative in the variance coordinate, marked
\code{gradient_convention = "omega_right_derivative"}. It is not an interior
gradient or a boundary optimization/inference guarantee. Zero policy loadings
are also recorded. The functions do not assert integration accuracy for every
parameter state: compare increasing node orders before future estimation use.

No optimizer, posterior updating, inference over network weights, structural
confidence interval, or specification p-value is implemented. Parameter counts
are bookkeeping, not a claim that all neural coordinates are identified. No
global thread settings are changed. Respondent subsets can be evaluated using
\code{rum_subset()} and the identically ordered rows of \code{Z} or \code{mu}.
}
\value{
The constructor returns a \code{rum_dnn_gaussian} list containing dimensions,
\code{weights}, \code{biases}, initialization metadata, and parameter count.
The forward function returns a respondent-by-\code{p} numeric matrix.

The output evaluator returns \code{loglik}, \code{objective}, \code{loss},
\code{mu}, \code{output_score} (respondent-by-\code{p}), and
\code{common_score} (respondent-by-\code{p+2}, in the order kappa, loadings,
omega). Its \code{objective_gradient} contains \code{mu = output_score/n}
and the averaged common derivatives \code{kappa}, \code{loading}, \code{omega}.
\code{loss_gradient} negates these entries. Boundary convention, task counts,
the quadrature rule, and \code{inference_status = "unavailable"} are retained.

The loss wrapper returns the same likelihood and scores. Its two gradient
lists instead contain network \code{weights} and \code{biases} lists together
with \code{kappa}, \code{loading}, and \code{omega}. Network gradient matrices
have the same shapes as their corresponding weights. It also records the
network parameter count and the ReLU kink convention.
}
\seealso{\code{\link{rum_data}}, \code{\link{rum_quadrature}},
  \code{\link{rum_loglik}}, \code{\link{rum_subset}}}
