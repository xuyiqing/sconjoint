\name{scmix_v22_fit_loading_gate_artifact}
\alias{scmix_v22_fit_loading_gate_artifact}
\alias{scmix_v22_validate_fit_loading_gate_artifact}
\title{Construct and validate v2.2 fit and loading gate evidence}
\usage{
scmix_v22_fit_loading_gate_artifact(
  fit, registry, analysis_contract, calibration_artifact, provenance
)

scmix_v22_validate_fit_loading_gate_artifact(
  artifact, analysis_contract, registry, calibration_artifact
)
}
\arguments{
\item{fit}{A production \code{scmix} fit containing full-sample and outer-fold portable optimizer records, an explicit raw-loading contract, and live loading-invariance evidence.}
\item{registry}{The complete frozen v2.2 target registry.}
\item{analysis_contract}{The exact signed v2.2 analysis contract.}
\item{calibration_artifact}{The complete production calibration artifact linked to the analysis contract.}
\item{provenance}{Nonempty immutable audit provenance.}
\item{artifact}{A \code{scmix_v22_fit_loading_gate_artifact}.}
}
\value{The constructor returns a fingerprinted \code{scmix_v22_fit_loading_gate_artifact}. The validator returns the artifact invisibly and raises an error when evidence is missing, altered, or contract-mismatched.}
\description{Derives F001--F006 and L001--L002 from a fitted model rather than caller-supplied gate states. The artifact covers the full fit and every registered outer fold and binds its evidence to the analysis signature, target registry, fit-scope registry, production calibration artifact, and immutable code, data, design, and calibration hashes.}
