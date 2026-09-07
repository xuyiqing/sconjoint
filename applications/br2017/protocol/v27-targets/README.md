# Ballard--Rosa v2.7 q=1 target stage

This is a separate, prospective target-stage protocol. It does not change any
v2.7 fit, assembly, refinement, or rank-comparison checkpoint. Before any
target calculation it binds the exact SHA-256 of the immutable v2.7 q=1
assembly and repaired-input preflight. It never reads q=2. The predictive
rank comparison therefore cannot select a target or alter a target family.

The target, contrast, subgroup, multiplicity-family, and inference-gate
registries are the byte-identical v2.5 files listed in
`frozen-registry-manifest.csv`. There are 72 registered targets in ten
families. Sixty-six targets have analytic rowwise callbacks. They cover the
position effect, coefficient and progressivity means, subgroup means, choice
probabilities, progressivity direction shares, compensating-benefit shares,
directional heterogeneity and polarization, and the mean MRS.

The six structural AMEs are explicitly unavailable. Their frozen definition
averages over the fielded profile and revenue distribution. The repository's
generic fixed-draw AME helper instead draws attributes independently and
uniformly. Substituting it would change the estimand. A later, newly versioned
protocol may add the AMEs after an exact Ballard--Rosa fielded-standardization
constructor and frozen draw artifact are reviewed.

Each supported family is passed in one complete call to the v2.7 target DML
adapter. The adapter fixes the ridge at training-sample size to the power minus
three quarters, uses the dimension-epsilon numerical-rank rule, and rejects
numerical target derivatives. Directional heterogeneity uses the direct
fold-specific empirical-P_Z construction for the smallest H_Z and H_R
primitive vector. Other smooth ratios are constructed from their joint
analytic primitives. A separate deterministic bank of 9,999 normal respondent
multipliers is then applied to the transformed influence columns jointly
within the full frozen family. Its seed, critical value, and draw-matrix hash
are retained. The draws themselves are reproducible but are not duplicated in
the checkpoint.

All reported intervals are diagnostic delta-method ranges. Formal release is
always false. Primitive-level Riesz and zero-ridge sensitivity numbers are
retained only as nuisance diagnostics. They are not substituted for the frozen
target-level transformed-Riesz, ridge-neighborhood, or regularity-z gates. The
package production-calibration artifact is absent. Target-level GH45
integration, near-optimum target spread, and range-respecting nonlinear
confidence-set records are also absent from this rapid stage. These missing
records are disclosed and never treated as passes.

The runner writes immutable SHA-sidecar checkpoints under
`runs/br2017/v27/rapid-2026-09-07-v1/targets`. One checkpoint is written per
complete multiplicity family. `export.rds` is both the machine-readable export
and its artifact manifest. It must contain all 72 frozen target IDs in order,
including explicit unavailable rows. Existing outputs are never overwritten.

Before target execution, the protocol directory and three implementation
files must be committed and clean. The intended order is `status`, `bind`,
`basis`, one or more `family --family=ID` calls, and `export`. Independent
families may run in separate processes after the basis checkpoint exists.
