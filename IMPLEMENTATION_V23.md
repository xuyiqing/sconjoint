# Inference protocol v2.3 implementation

This branch implements the prospective software contract in
`INFERENCE_PROTOCOL_V23.md` and `inst/protocol/inference-v2.3.yaml` under
protocol ID `paperps-inference-2026-09-06-v2.3`. The v2.2 protocol files and
checksum remain unchanged as historical evidence.

The exported function names retain their `scmix_v22_` prefix for source
compatibility with the Step 2 development branch. Every newly constructed
artifact records the v2.3 Protocol ID, so a v2.2 artifact cannot be mixed with
this implementation. The package-approved calibration also has the explicit
`scmix_v23_package_calibration()` accessor. The historical
`scmix_v22_package_calibration()` entry point delegates to the same v2.3
contract for source compatibility.

## Production calibration contract

The future canonical values file is
`inst/protocol/inference-v2.3-calibration-values.dcf`. Its separate approval
manifest is `inst/protocol/inference-v2.3-calibration.dcf`. Neither file is
present before the simulation validation succeeds.

The loader computes a true SHA-256 digest of the values file. The manifest must
bind that digest, the v2.3 Protocol ID, production status, `Rank-Scope: 1`, the
derived calibration hash, and the exact complete set of 26 constants. Any
missing, extra, altered, or nonfinite value fails closed. All G006 component
cutoffs must be strictly positive.

G004 and G005 are bound exactly to the approved active-eigenvalue and
generalized-information constants. An externally supplied threshold cannot
weaken either gate.

G006 uses the v2.3 normalized compound statistic. Each applicable raw or
standardized component is divided by its own independently calibrated cutoff.
The gate statistic is the minimum component ratio and its fixed threshold is
one. The output retains the full component table. No constants are equated or
silently collapsed.

Production calibration authorizes only a signed analysis contract with
`q = 1`. Rank-zero and rank-two runs remain diagnostic sensitivities. A
rank-two integration sentinel cannot authorize rank-two inference.

## Verification

The focused contract tests cover exact 26-constant completeness, byte-level
source tampering, manifest tampering, the absent-manifest fail-closed path,
rank-scope mismatches, wrong external G004 and G005 thresholds, all constant
consumers, and the target-specific G006 components.

The prospective calibration harness and its operating-characteristic results
are separate from this production loader. A pilot or incomplete validation
study must not create the two production files.
