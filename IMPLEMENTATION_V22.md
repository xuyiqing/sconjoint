# Inference protocol v2.2 implementation

This branch implements the software contract frozen in
`INFERENCE_PROTOCOL_V22.md` and `inst/protocol/inference-v2.2.yaml` under
protocol ID `paperps-inference-2026-09-05-v2.2`.

## What Step 2 implements

- Four distinct target estimates: the full-sample plug-in, cross-fitted
  plug-in, raw one-step estimate, and chart-stabilized reported estimate.
- Support-respecting scalar intervals and admissible-set mappings for subgroup
  shares, directional heterogeneity shares, and mean-tradeoff ratios.
- Respondent-level influence functions, a deterministic bank of 9,999 common
  normal respondent multipliers, and assembly of simultaneous families across
  separately computed target jobs.
- A fail-closed, machine-readable gate record with separate calculation, fit,
  target, release, multiplicity, and interpretation statuses.
- Inner-respondent ridge selection followed by untouched-outer-fold Riesz
  diagnostics and perturbations over the frozen ridge neighborhood.
- Double-precision mixed-logit fitting, a full-batch L-BFGS polish, and
  dimensionless blockwise projected-KKT diagnostics.
- An explicit accessor for raw and optimization-scale loadings and a tested
  round-trip identity between the two representations. A live fitted-network
  test also verifies probability and respondent-sequence likelihood
  invariance after an equivalent preprocessing rescaling.
- Typed, fingerprinted evidence for the full fit and every outer-fold fit,
  including F001--F006 and L001--L002. Typed computation artifacts separately
  cover nested-fit stability, multistart target stability, and both required
  integration-refinement branches. The finalizer rejects caller-entered rows
  for these protected gates.
- An exact target-registry check, contract-derived gate applicability, and a
  gate schema whose scope, severity, comparator, and units are frozen. A
  target-domain change requires a new protocol ID rather than a same-version
  amendment.
- A fail-closed hook for a package-approved Step-3 calibration manifest. No
  such production manifest ships in Step 2, so an application cannot mint its
  own cutoffs and receive v2.2 status.

## Step 3 calibration contract

The Step 3 branch adds the production loader without pre-populating any
simulation result. It expects a canonical values file named
`inst/protocol/inference-v2.2-calibration-values.dcf` and a separate approval
manifest named `inst/protocol/inference-v2.2-calibration.dcf`. The loader
computes a true SHA-256 digest of the values file. The manifest must bind that
digest, the protocol ID, production status, the derived calibration hash, and
the exact complete set of 26 constants. Any missing, extra, altered, or
nonfinite value fails closed.

The manifest also enforces the equality aliases needed by the frozen one-row
G006 gate. The directional-heterogeneity release margin equals its directional
variance and total-heterogeneity aliases. The mean-tradeoff margin equals its
denominator alias. G004 and G005 are checked against the approved active-
eigenvalue and generalized-information constants rather than trusting an
external gate row. G006 selects its approved cutoff by target type and the
directional-heterogeneity boundary statistic includes the standardized total
heterogeneity as well as both variance components.

## Deliberately pending after Step 2

Constants marked `CALIBRATE_IN_STEP_3` remain unset. Applicable gates that
depend on them fail closed. The Step 2 code therefore cannot certify an
application as v2.2-eligible. Step 3 will calibrate those constants with
design-matched simulations without using application pass or fail outcomes.
The approved calibration file will be added only after that study is frozen.

No existing Ballard--Rosa, Graham--Svolik, or Saha--Weeks artifact is
grandfathered into v2.2. Each application will require a new signed registry,
fully nested nuisance evidence, fresh fits, integration checks, target-specific
inference, and the complete specification-assessment battery after calibration.

## Audit commands

From the repository root, verify the frozen files and run the focused tests:

```sh
shasum -a 256 -c provenance/inference_protocol_v2.2.sha256
applications/bin/Rscript45 -e 'pkgload::load_all(quiet = TRUE); testthat::test_local(filter = "paperps-inference-v22|paperps-computation|mixed-fit-loading-gate-v22|mixed-network-state|mixed-optimization-v22|mixed-loading-contract-v22", reporter = "summary")'
env SCONJOINT_ROOT="$PWD" applications/bin/Rscript45 applications/R/test-targeted-q1-inference.R
```

The exact test and package-check results for this implementation are recorded
in `provenance/step2_verification_2026-09-06.txt` and the running coauthor
memo. The expensive refit callback still requires an auditable application
runner. The package verifies its respondent splits, ridge grids, risks, and
fingerprints, but software alone cannot prove that arbitrary callback code
honored its declared training-only semantics. Each production run must retain
the callback code and generated artifacts. External pushing remains disabled
in the shared working copy.
