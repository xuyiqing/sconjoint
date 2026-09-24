\name{rum_dnn_gaussian_train}
\alias{rum_dnn_gaussian_train}
\title{Bounded Gaussian DNN Checkpoint Selection and Refit}
\description{Joint full-batch Adam training of the conditional mean network and
common Gaussian latent parameters, with an explicit inner respondent validation
set, a fixed initialization menu, and bounded selection/refit stages. This is
an optimization procedure, not an inferential or model-adequacy certificate.}
\usage{
rum_dnn_gaussian_train(data, Z, validation_ids,
  hidden = c(32L, 32L, 16L), seeds = c(42L, 7L, 1609L),
  control = list(), starts = NULL)
}
\arguments{
\item{data}{Complete respondent records from \code{rum_data()}.}
\item{Z}{Finite encoded input matrix with row names exactly equal to
\code{data$ids}, in order. All preprocessing is supplied by the caller.}
\item{validation_ids}{A nonempty proper subset of respondent IDs, chosen
before fitting. Their responses are used only for checkpoint selection until
the selected full-training-bank refit. The function never creates a split.}
\item{hidden}{Hidden-layer widths, default 32, 32, 16.}
\item{seeds}{Distinct initialization seeds, default 42, 7, 1609.}
\item{control}{Named overrides for the explicit controls described below.}
\item{starts}{Optional list of original states, one per seed. Each state has
\code{network}, \code{kappa}, \code{loading}, and \code{omega}. Dimension and
bound checks are applied. Supplied states must not use held-out outcomes or
simulation truth.}
}
\details{
The raw objective is respondent-average negative sequence log likelihood plus
\code{network_l2/2} times the sum of squared network weights and biases.
Common parameters are not penalized. Adam updates the network, kappa, loadings,
and log(omega) jointly; the log-variance gradient applies the chain factor
omega. Finite common-parameter bounds are enforced by projection after each
step. These are computational restrictions and bound contacts are recorded.

Each start is trained on the respondents outside \code{validation_ids}.
Validation uses unpenalized complete-sequence log loss. Epoch zero and scheduled
checkpoints are candidates; ties favor the earliest checkpoint and then the
first declared start. After choosing one start/checkpoint, its original state
and zero Adam moments are restored. It is refit on every supplied respondent
for exactly the selected epoch count. There is no ensemble or second selection
after refitting. A zero-epoch winner is explicitly labeled, not called learning
or convergence.

Defaults are \code{max_epochs=200}, \code{validation_every=5},
\code{patience=10} validation checks, \code{min_delta=0},
\code{learning_rate=.01}, \code{beta1=.9}, \code{beta2=.999},
\code{epsilon=1e-8}, and \code{network_l2=1e-4}. Epoch one and the last epoch
are also checked. \code{patience=Inf} disables patience stopping.
The three default loading starts have coordinates \eqn{.1\sin(j)},
\eqn{.1\cos(j)}, and \eqn{.1(-1)^j}; kappa starts at zero and omega at
.25, .5, and .75. No response or truth-based initializer is used.

Time controls are \code{max_seconds=480}, \code{start_seconds=90} per start,
\code{refit_seconds=150}, and \code{evaluation_reserve=60}. Reserves may
shorten selection stages. A time-limited selection start can supply a saved
valid checkpoint, but the budget-limited selection status remains explicit.
An unfinished required refit or final evaluation leaves \code{final=NULL}.
Time is checked between respondent blocks and operations; a single atomic
native evaluation cannot be interrupted by these checks. Any measured total
overrun is reported and cannot be marked completed. No extra run or larger
budget is launched automatically.

\code{train_order=15}, \code{check_orders=c(15,21)}, and
\code{block_size=64} control numerical evaluation. Blocks accumulate the full
batch gradient before an Adam update; they are not stochastic minibatches.
Final checks compare increasing node orders at the same selected parameter
state. Thresholds are \code{loglik_tolerance=1e-5},
\code{score_tolerance=1e-4} for unscaled respondent output/common scores, and
\code{network_gradient_tolerance=1e-4}. These checks measure integration
sensitivity only. \code{kappa_bounds=c(-5,5)},
\code{loading_bounds=c(-3,3)}, and \code{omega_bounds=c(1e-6,4)} are the
default artificial bounds. The log-variance trainer cannot attain omega zero;
the separate likelihood evaluator supports its right derivative.
}
\value{
A \code{rum_dnn_train} list containing \code{status}, \code{completed},
\code{final}, \code{selection}, every start's original/checkpoint/last state,
traces and costs, the reset full-bank \code{refit}, final per-order
\code{evaluations}, \code{numerical_checks}, \code{numerical_status},
\code{stage_cost}, explicit controls and input IDs, errors, bound flags, and
elapsed/CPU cost. \code{final} is a state containing network, kappa, loading,
and omega, not a confidence statement. Every result has
\code{inference_status="unavailable"} and \code{convergence_claim=FALSE}.
Numerically qualified final states and start errors are retained.
}
\seealso{\code{\link{rum_dnn_gaussian}}, \code{\link{rum_dnn_gaussian_loss}},
\code{\link{rum_subset}}}
