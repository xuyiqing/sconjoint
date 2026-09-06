\name{scmix_v23_calibration}
\alias{scmix_v23_calibration_plan}
\alias{scmix_v23_design_template}
\alias{scmix_v23_load_frozen_design}
\alias{scmix_v23_truth_manifest}
\alias{scmix_v23_oracle_truths}
\alias{scmix_v23_run_oracle_calibration}
\alias{scmix_v23_oracle_operating_characteristics}
\alias{scmix_v23_select_calibration_constants}
\title{Prospective protocol-v2.3 calibration records}
\description{
Constructors and validators for the response-free design record, frozen truth
manifest, and diagnostic statistical oracle used while preparing the
protocol-v2.3 calibration study.
}
\details{
The design templates contain profile contrasts, moderators, respondent and
task identifiers, and optional response-free positions. They do not contain
observed choices. The truth manifest verifies the frozen tables, registries,
plan, and immutable audit copies of the truth-generation source files.

The local-Gaussian oracle is a geometry and face-validity diagnostic. It does
not reproduce the fitted respondent-sequence estimator, nested cross-fitting,
target-specific empirical Riesz solves, one-step corrections, or common
respondent multiplier procedure. Oracle output alone is therefore ineligible
for threshold selection and cannot authorize production calibration values.

The selector remains fail closed unless it receives complete, signed
end-to-end statistical and numerical evidence under the frozen plan. These
functions do not create a production approval manifest.
}
\value{
Depending on the function, a signed calibration plan, response-free design,
truth manifest, diagnostic oracle record, operating-characteristic summary,
or incomplete calibration proposal.
}
\keyword{internal}
