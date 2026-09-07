# Ballard--Rosa rapid continuation protocol v2.6

**Protocol ID:** `br2017-paperps-rapid-2026-09-07-v2.6-v1`

This directory fixes the outcome-generation contract for the same-day
Ballard--Rosa rerun authorized on 2026-09-07.  It implements the application
track in `proposals/v26/application-plan.md`.  The authors have seen the older
v2.1 results, so this is prospective only with respect to v2.6 outputs.

The analysis fits ranks zero, one, and two on the same respondent folds and
with the same historically selected conditional-mean learner in each training
sample.  Rank one is primary.  Its six v2.1 selected states are hash-bound
initial values only.  Each is reconstructed after the `work_vs_luck` repair,
converted to float64, and optimized again by full-batch L-BFGS from both an
exact and a deterministic perturbed start.  No prior estimate, interval,
target, gate, or assessment is a v2.6 result.

Rank zero and rank two start only from their corresponding completed v2.6
rank-one state.  They retain its mean architecture and weight decay.  Rank
zero drops the loading and uses exact integration.  Rank two retains the first
loading column and adds a deterministic small raw-scale orthogonal column.
It uses GH9 for fitting and GH15 for a fixed-state integration check.  Rank
one uses GH31 for fitting and GH45 for its fixed-state check.

All six scopes are computed for each rank.  The scopes are the full sample and
the five historical outer-training samples.  Outer fold 2 is the rank-two
timing sentinel.  Only elapsed time, integrity fields, memory, and hashes may
be inspected there.  Remaining rank-two scopes are dispatched only if the
prespecified timing rule permits them.

Target-specific DML is computed only for rank one.  Its rapid intervals are
draft-conditional and remain pending the confirmatory validation extension.
The software continues to enforce every fit, Riesz, ridge, derivative,
regularity, integration, and family gate.  It does not fabricate a production
calibration approval.  Rank zero and rank two provide held-out sequence
scores, calibration checks, numerical gates, and plug-in target sensitivities.
A zero-rank preference-share calculation is not regular heterogeneity
inference.  Rank-two outer-cross-fitted regular inference is deferred to the
confirmatory extension even though the rapid run preserves all outer states
for fair predictive and plug-in comparisons.

The response-free repair affects one respondent and therefore eight task
rows.  Both units are recorded in preflight provenance.

The v2.5 target, contrast, group, family, inference-gate, assessment,
sensitivity, restricted-mean, and showcase registries are carried forward
byte for byte through `carry-forward-manifest.csv`.  This protocol changes the
computational route and reporting status, not the registered substantive
targets or gate thresholds.

The runner writes only below
`runs/br2017/v26/rapid-2026-09-07-v1`.  Every checkpoint is written atomically,
has a SHA-256 sidecar, and is immutable after completion.  A restart validates
and reuses a completed checkpoint.  Source artifacts are opened read-only and
are never copied over or modified.

Run the outcome-free contract test from the repository root with

```sh
CONJOINTSTRUCTURAL_ROOT=/path/to/ConjointStructural \
  applications/bin/Rscript45 applications/br2017/protocol/v26/test-protocol.R
```

After the protocol and implementation are committed, the executable entry
point is `applications/br2017/R/06_run_fast_v26.R`.  Its `status` command is
read-only.  Fit commands generate outcomes and must not be run from an
uncommitted or modified implementation.
