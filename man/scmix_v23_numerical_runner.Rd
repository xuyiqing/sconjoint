\name{scmix_v23_numerical_runner}
\alias{scmix_v23_numerical_contract}
\alias{scmix_v23_numerical_missing_primitives}
\alias{scmix_v23_numerical_readiness}
\alias{scmix_v23_seal_generated_outcome}
\alias{scmix_v23_fit_numerical_base}
\alias{scmix_v23_validate_numerical_base_fit}
\alias{scmix_v23_known_rank_evidence}
\alias{scmix_v23_numerical_evidence_bundle}
\alias{scmix_v23_numerical_replay}
\alias{scmix_v23_loading_double_scale_fault}
\alias{scmix_v23_numerical_layer_run}
\alias{scmix_v23_transport_validation}
\alias{scmix_v23_run_numerical_base}
\alias{scmix_v23_validate_numerical_base_run}
\title{Prospective protocol-v2.3 numerical calibration runner}
\description{
Fail-closed constructors and validators for the protocol-v2.3 numerical
calibration and untouched transport study. The base runner generates choices
only after a truth manifest has been frozen, and only from a signed
response-free design and the frozen numerical seed. It then
uses the package's actual q=1 full and nested estimators. Evidence bundles
require signed target, integration, derivative, rank, and respondent-multiplier
artifacts. They cannot create a production calibration manifest.
}
\details{
The ordinary-base contract fixes two outer folds, two inner folds, two starts,
double precision, GH15-to-GH31 refinement, and full-batch L-BFGS polishing.
The separate transport contract fixes five outer folds, two inner folds, three
starts, GH31-to-GH61 refinement, and 9,999 common respondent multipliers.
Replays remain clustered by their base synthetic outcome. The q=2 checks are
sensitivity sentinels and have no authority over the q=1 production manifest.

\code{scmix_v23_numerical_missing_primitives()} reports the application- and
target-specific adapters that must exist before the prospective study can
select constants. An incomplete evidence object has no selector-eligible
class.

\code{scmix_v23_numerical_readiness()} returns the signed pre-outcome and
promotion state. Missing adapters disable design-matched outcome generation.
Marginal oracle evidence alone cannot satisfy the end-to-end statistical
validation requirement or authorize production values.
}
\value{
Depending on the function, a signed contract, generated outcome, actual-fit
record, evidence bundle, replay record, transport-validation record, or an
invisible validated input. The missing-primitives function returns a data
frame.
}
\keyword{internal}
