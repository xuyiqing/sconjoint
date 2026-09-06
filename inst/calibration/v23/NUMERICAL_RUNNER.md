# Protocol v2.3 numerical runner

`R/calibration-v23-numerical.R` is the Step-3 actual-fit numerical path. It has
no proxy mode. It refuses to seal any synthetic outcome until an intact
outcome-free truth manifest has been frozen. It then binds the outcome to that
manifest, an intact response-free design, and the prospective numerical seed,
and calls
`scmix_tune_matrix()`, `scmix_tune_outer_matrix()`, and
`scmix_assemble_nested()` to produce the actual full and nested q=1 fits.

The ordinary numerical calibration and replay-base contract fixes two
respondent-level outer folds, two inner folds, two starts, GH15-to-GH31
refinement, double precision, one Torch and BLAS thread per worker, a
full-batch strong-Wolfe L-BFGS polish, and the constant-plus-linear corrected
mean family. Those settings apply to the two independent base outcomes per
design and stream. Computational-error variants are clustered replays of one
of those outcomes.

The separate transport contract uses one untouched validation outcome per
design. It fixes five outer folds, two inner folds, three starts, GH31-to-GH61
refinement, and 9,999 common respondent multipliers. The portable RDS in both
paths contains all full and outer start states. Its companion artifact records
and verifies the RDS SHA-256 hash. The runner derives KKT, nested-fit, and
loading-scale metrics from fitted objects. It never accepts those metrics as
caller-supplied numbers.

Five integration metrics require the package's signed two-branch artifact at
the resolution fixed by the relevant contract. The derivative metric requires
the signed finite-difference audit.
The numerical-rank tolerance is selected first from matrices with known rank.
In transport, the common multiplier bank must contain exactly 9,999
respondent-level draws. The runner regenerates and validates its values and
checks its respondent set. Ordinary bases use valid respondent-level influence
standard errors for SE-scaled metrics but do not use multiplier draws. Both
paths require the derivative audit, integration evidence, and multistart
surface to be linked to the same signed analysis contract as the assembled
fit.

The near-optimum objective cutoff and the multistart target-stability cutoff
are intrinsically joint. The F008 artifact therefore remains a raw surface of
objective gaps and target changes until the calibration-stream joint selector
freezes both constants. The evidence bundle deliberately is not assigned the
class `scmix_v23_numerical_calibration_run`. This prevents the existing scalar
selector from silently treating either half of the joint rule as independent.

Fault variants retain the same base-outcome fingerprint and carry one replay
cluster identifier. They are paired replays, not independent Monte Carlo
trials. Wilson intervals must never use replay count as their denominator.
The q=2 GH5-squared to GH9-squared and GH9-squared to GH15-squared checks, with
GH25-squared as gold, are sensitivity sentinels only. They cannot authorize or
inherit a q=1 production manifest.

The current package deliberately requires application-specific builders for
target recomputation, level-B Riesz and influence evaluation, repolishing from
every level-A state, derivative coordinates, registered fault fits, and q=2
product-grid sentinels. Their exact required outputs are listed by
`scmix_v23_numerical_missing_primitives()`. The F008 cutoffs also require a
joint selector after the calibration stream is run. An incomplete or unsigned
object fails before threshold selection. No function in this file creates
either production calibration DCF file.

`scmix_v23_numerical_readiness()` exposes this pre-outcome state as a signed,
machine-readable artifact. Design-matched outcome generation stays disabled
until every named actual-fit adapter is present. Threshold selection then
requires a complete ordinary calibration artifact. Production promotion also
requires untouched ordinary numerical validation, the exact transport layer,
and a distinct end-to-end statistical validation artifact. The current
marginal composite-likelihood oracle is useful for geometry and face-validity
checks, but the readiness record explicitly prevents it from authorizing
production values on its own.
