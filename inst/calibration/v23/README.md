# Step 3 calibration harness for protocol v2.3

This directory implements the prospective calibration plan in
`inst/protocol/inference-v2.3-step3-plan.dcf`. The plan is outcome locked.
Only the whitelisted `deltaX`, `Z_primary`, respondent identifier, and task
identifier fields enter a design payload. The three source files, the three
response-free payloads, their compressed wrappers, and this extractor are
pinned by SHA-256. Each wrapper carries its `design_id`. The manifest also
freezes each design's exact respondent task-count histogram.

The calibration has three layers.

1. The task-level oracle layer generates choices from the frozen rank-one
   mixed-logit DGP and computes respondent-clustered local Gaussian evidence.
   It uses 2,000 calibration and 2,000 untouched validation replications per
   design, regime, and frozen severity. It never supplies numerical-fit
   metrics.
2. The numerical layer uses two independent synthetic base outcomes per
   design and stream. Every severity and computational-error arm is a replay
   clustered within its base outcome. A replay is not an independent Wilson
   trial. Each replay must execute the full synthetic-data fit and evidence
   path. A callback that returns invented or perturbed metrics without the
   required fit artifacts fails closed.
3. Transport validation sends one untouched validation base per design
   through five outer folds, two inner folds, three starts, GH31-to-GH61
   integration refinement, and 9,999 common respondent multipliers.

Cutoffs are selected independently. The complete 26-cutoff vector is then
evaluated jointly without retuning. G006 divides every applicable boundary
component by its own cutoff and passes exactly when the minimum ratio is at
least one. There are no cross-constant aliases.

Each high- or low-moderator subgroup share is the composite `a / b`, where
`a` is the respondent-equal mean of the subgroup indicator times that
respondent's integrated task-mean choice probability and `b` is the
respondent-equal subgroup probability. Its maintained coordinates obey
`0 < a < b < 1` and use the chart `logit(a / b), logit(b)`. The subgroup G006
diagnostic is `min(a, 1 - b, b - a)`, minimized again over the two variants.
It is measured on the raw probability scale without standard-error
standardization before division by its independently calibrated cutoff.

Every adverse severity row is labeled `safe`, `transition`, or `unsafe`.
Only unsafe rows enter the false-certification denominator. Selection and
validation enforce benign passage and unsafe false certification separately
within each design. Benign calibration includes both the well-informed
reference and the safe row of the constant's own targeted regime.
Interpretation-sensitivity regimes do not enter certification calibration.
Serial-dependence choices use a stationary Gaussian AR(1)
copula that preserves each task's marginal logit probability. The completion
sensitivity uses duplicated finite-population PPS respondent selection within
exact task-count strata, which preserves the original task-count histogram.
The residual-shape grid spans a standardized Student t with five degrees of
freedom, a centered standardized chi-square with three degrees of freedom,
and a symmetric unit-variance Gaussian mixture with component means plus and
minus 0.85.

The production scope is rank one. Rank-two results remain sensitivity
analyses and must run the frozen 5-squared to 9-squared, 9-squared to
15-squared, and gold 25-squared integration sentinels. They cannot inherit a
rank-one calibration artifact.

No function in this directory writes the production values or approval DCF.
Those files may be created only in a separate approval step after every
frozen per-design validation criterion passes. Until then, production remains
fail closed.

The no-outcome truth builder writes the seven target truths for all design,
regime, and severity cells and records the whole-file SHA-256 before the first
synthetic choice is generated. The truth manifest is therefore a prerequisite
for either calibration runner, not an output that can be revised after seeing
operating characteristics.

The frozen output registry covers unconditional and release-conditional
coverage, unsafe false certification, natural- and standard-error-scale
interval-length medians and 90th percentiles, empty and uninformative sets,
every gate-state frequency, and first-failure frequency. Numerical severity
replays stay clustered by their independent synthetic base outcome and never
enter a Wilson calculation as independent trials.

Conditional coverage requires at least 1,000 released replications per design
and target. In each well-informed oracle cell, interval efficiency compares
the reported natural-scale length with the ideal unconstrained linear-Wald
length `2 * z_0.975 * SE`, or the exact oracle Fieller benchmark for a ratio.
The median reported-to-benchmark ratio may not exceed 1.25 and its type-8 90th
percentile may not exceed 2.0.
