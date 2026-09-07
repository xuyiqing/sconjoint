# Prospective inference-validation protocol v2.5

**Protocol ID:** `paperps-inference-2026-09-07-v2.5`

**Plan ID:** `paperps-lean-validation-2026-09-07-v2.5-v1`

**Frozen:** September 7, 2026, before any v2.5 synthetic outcome.

**Status:** Protocol, response-free inputs, DGPs, truths, targets, gates,
settings, counts, seeds, schedule, and decision rules are frozen. The v2.5
evaluator implementation and approval are absent. Outcome generation is
locked.

## 1. Reason for v2.5

Version 2.4 could not satisfy its prospective eight-hour runtime rule. The
v2.5 change uses only information that the v2.4 pilot firewall permitted. It
changes computation and Monte Carlo replication counts. It does not change or
reselect an empirical template, DGP geometry, target, target truth,
family-to-cell approval assignment, or per-replication gate.

The exact v2.4 response-free design manifest, selection salts and hashes,
six DGP geometries, target registry, 78 target truths, approval matrix, and
fit-regularity primitive are copied to new v2.5 protocol paths. The new master
seeds do not overlap the v2.4 streams.

## 2. Cells and estimands

The six cells remain:

1. `br_regular`
2. `gs_regular`
3. `sw_regular`
4. `br_probability_zero_denominator_boundary`
5. `br_q0_rank_boundary`
6. `gs_weak_inverse`

Each cell uses the same 400-response-free-respondent empirical template and
the same rank-one fitted procedure as v2.4. Whole respondent sequences are
sampled with replacement. Truth remains defined under the original frozen
400-respondent empirical distribution.

The six target families and all reported members remain:

- smooth means: direction, numerator, and denominator means
- choice probabilities: the integrated choice probability
- directional shares: preference direction and compensating benefit shares
- subgroup ratios: high- and low-subgroup choice shares
- directional heterogeneity: explained variance, residual variance, and
  explained share
- MRS: the ratio of numerator and denominator means

The active rank-one covariance eigenvalue remains a separate fit-regularity
primitive. Exact definitions, contrasts, weights, primitives, tie rules, and
truths are in the v2.5 registries.

## 3. Per-replication procedure and gates

Every replication retains the v2.4 estimator and per-replication gates. It
uses two outer folds, two inner folds, two starts, GH15 integration, the paired
GH31 refinement, fixed target-specific Riesz correction, constrained
confidence-set mapping, and 9,999 common respondent multipliers.

The unchanged per-replication gates are:

| Rule | Frozen comparator |
|---|---|
| regularity margin | at least 1.96 standard errors |
| chart or nonlinear discrepancy | at most 0.10 target standard errors |
| held-out Riesz defect | at most 0.20 |
| neighboring-ridge movement | at most 0.20 target standard errors |
| blockwise KKT residual | at most 0.00001 |
| nested objective gap | at most 0.000001 |
| near-optimum objective gap | at most 0.000001 |
| near-optimum target spread | at most 0.10 target standard errors |
| GH15-to-GH31 target movement | at most 0.05 target standard errors |
| GH15-to-GH31 influence movement | at most 0.05 empirical standard deviations |
| loading-scale probability difference | at most 0.00000001 |
| derivative relative error | at most 0.0001 |
| certified null-space component | at most 0.000001 |
| numerical rank | dimension-adjusted machine-epsilon SVD rule |

The target families, exact-boundary assignments, weak-or-bounded assignments,
and six descriptive-only family-cell pairs are unchanged. Thirty of 36
family-cell rows enter approval.

## 4. Replication counts and translated rules

There are 50 evidence replications per cell and 300 total. Evidence is
scheduled as five batches. Each batch has ten replications per cell.

The v2.4 proportions are translated to exact integer rules for 50 planned
replications:

- A cell must complete at least 48 of 50 replications.
- A regular family-cell pair must release at least 38 of 50 replications.
- Conditional simultaneous coverage must be at least 0.90 among complete
  family releases. Equivalently, covered releases must be at least the ceiling
  of 0.90 times the number released.
- At an exact boundary, the invalid regular-release proportion must be at most
  0.05 and its one-sided exact 95 percent Clopper-Pearson upper limit must be at
  most 0.10. With any permitted 48 to 50 completed replications, this compound
  rule allows at most one invalid release. For 50 completed replications the
  upper limits are 0.091398 for one invalid release and 0.120614 for two.
- A weak-or-bounded pair succeeds by withholding when it has at most two
  releases. It can instead be informative with at least 15 releases if the
  regular coverage rule holds. Three through 14 releases are inconclusive.

Early success and early stopping are prohibited. All five batches and all 300
evidence attempts are required. Coverage Monte Carlo standard errors and
two-sided 90 percent Wilson intervals remain descriptive.

## 5. Twelve-worker deterministic schedule

The only permitted backend is 12 spawned base R PSOCK processes with one Torch
thread per worker. Each cell has two deterministic lanes. For cell index
`c` and lane `l` in `{1,2}`, the worker ID is
`2 * (c - 1) + l`.

The integrity pilot is one 12-job wave. Lane 1 runs pilot replication 1 and
lane 2 runs pilot replication 2 for its cell, so every worker runs one job.
Within each ten-replication evidence batch, lane 1 runs within-batch positions
1, 3, 5, 7, and 9. Lane 2 runs positions 2, 4, 6, 8, and 10. Each lane
therefore processes 25 evidence replications across five batches.

The pilot master seed is 1910000000. The evidence master seed is 2010000000.
For cell index `c` and replication `r`, the seed is
`master + (c - 1) * 1000000 + r`. The resampling, latent, choice, and
multiplier substreams use offsets 0, 200000, 400000, and 600000.

All production state records have one frozen location relative to the approved
source root, `inst/protocol`. The pilot and all five evidence batches likewise
share one frozen run directory,
`runs/v25/step5c_integrity_and_evidence_2026-09-07`. No alternate state or run
directory is permitted. This makes the first valid pilot ledger unique and
prevents selecting among otherwise valid pilot waves created in different
directories.

## 6. Pilot firewall and runtime decision

The pilot is excluded from evidence. Public inspection is limited to execution
integrity, elapsed time, and resource use. Synthetic choices, fits, targets,
gates, intervals, coverage, and operating characteristics remain sealed and
uninspected. Artifact existence and hashes may be verified without opening
their statistical contents.

Complete pilot wall time is measured monotonically from immediately before
the 12-worker cluster is created through worker shutdown and verification of
all 12 artifacts and signed manifests. The runtime projection is

`1.25 * 25 * complete pilot wall time`.

It must be no more than eight hours. This is equivalent to a complete pilot
wall time no greater than 921.6 seconds.

Every attempted pilot receives a signed private ledger. The chronologically
first complete integrity-valid pilot wave is the only valid pilot and is
binding. An invalid infrastructure or integrity attempt may be retried only
before a valid wave exists and only after a documented correction based on
firewall-permitted information. Once a valid pilot exists, it may not be rerun
or replaced, even if its runtime is unfavorable. A valid pilot over 921.6
seconds stops v2.5 and requires a new protocol ID. Selecting the fastest or
most favorable run is prohibited.

## 7. Approval and lock

No v2.5 evaluator implementation manifest, evaluator approval, pilot approval,
public pilot manifest, evidence manifest, family approval, or synthetic
outcome exists at freeze. A future evaluator must use `calibration-v25-*`
source names and `scmix_v25_*` interfaces, match the frozen settings, and be
hash-bound in a separate pre-outcome implementation approval.

That approval may open only the pilot. Evidence remains locked until the first
valid pilot passes the integrity and runtime rule and a separate signed pilot
approval records the mechanical result using firewall-permitted fields. No
failed target or family can be dropped, no threshold can be retuned, and no
discretionary override can approve a family.

The study validates an implementation on three finite empirical design
templates. It does not certify an application.
