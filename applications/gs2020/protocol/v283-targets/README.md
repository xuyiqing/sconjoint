# Graham--Svolik q=1 target protocol v2.8.3

**Protocol ID:** `gs2020-paperps-targets-2026-09-07-v2.8.3-v1`

This target stage is prospective with respect to the new v2.8.3 Graham--Svolik
fit, but not confirmatory on this sample. Earlier Graham--Svolik target results,
including the completed but now invalidated v2.8.2 target stage, and the
Ballard--Rosa v2.8.1 results were already known. No v2.8.2 target estimate or
checkpoint is reused. The five targets below were selected after those
diagnostics. The stage therefore reports conditional, descriptive evidence and
never converts a passed numerical gate into a confirmatory claim.

The stage reads only the fresh immutable, hash-bound q=1 assembly and verified
input checkpoint from the exact parent v2.8.3 run. It does not alter any fit,
assembly, refinement, or rank-comparison checkpoint. It never uses q=2 to
select targets. It does not read, copy, resume, or otherwise reuse any v2.8.1
or v2.8.2 target checkpoint. This prohibition includes an incomplete binding,
basis, family, or export checkpoint left by a partial earlier target run.

## Five-target core

The two context targets are the mean co-party utility contrast and the
position-neutral probability of choosing a co-party candidate who restricts
voting over an otherwise matched out-party democratic candidate. The three
heterogeneity targets concern voting restrictions. They are the share who
oppose the restriction, the share for whom a co-party benefit compensates the
restriction, and the share of directional variance explained by observed
respondent moderators. The exact signs and coefficient coordinates are frozen
in `contrast-registry.csv` and `core-target-registry.csv`.

These choices keep the rapid stage focused on one central democratic violation.
The wider seven-action summary from the earlier analysis remains a descriptive
plug-in table. It is not part of either core multiplicity family and is not run
silently by this protocol.

For directional heterogeneity, cross-fit aggregation forms global `H_Z` from
the global cross-fit second moment minus the squared global cross-fit mean.
Each fold's Riesz gradient is centered only at its outer-training mean. This
preserves the declared finite-population variance without feeding held-out
information into nuisance or ridge fitting.

## Inference and release rules

Each target receives its own DML call. Riesz ridge selection uses only the
corresponding outer-training respondents and the fixed grid `0`, `Ntrain^-1`,
`Ntrain^-3/4`, and `Ntrain^-1/2`. The fixed defect, correction, derivative,
null-space, fit KKT, loading-invariance, and target-stability thresholds are not
relaxed. The fast route shares the already fitted outer nuisance and is
diagnostic. Formal inference still requires signed inner nuisance refits.

Within each family, one bank of 9,999 respondent-level standard-normal
multipliers supplies simultaneous intervals. Missing or failed fit, Riesz,
derivative, ridge-neighborhood, or target-stability evidence withholds release.
Even a numerically complete result remains same-sample descriptive evidence
because the target core was selected after earlier outcomes were examined.

## Integrity and order

Every outcome command requires a clean repository and exact protocol,
implementation, runtime, input, and upstream-checkpoint hashes. Outputs are
immutable. The shared R runtime link is created once and then verified without
replacement so simultaneous family processes cannot interrupt one another's
base-package resolution. No external push is authorized. After finalization and commit, run
`status`, `bind`, `basis`, both family commands, and `export`. The two family
commands may run independently after the basis checkpoint exists.

If a hard kill occurs between installing a complete checkpoint envelope and
its SHA sidecar, the reader verifies the envelope and full binding before
recreating only the missing sidecar. It never repairs a foreign or malformed
checkpoint.

During construction, the manifests used `PENDING_FINALIZATION` for
implementation, dependency, and protocol hashes. The production validators
reject that value. The finalized target protocol contains exact SHA-256 values
after the parent v2.8.3 fit protocol was frozen. Any later code change
invalidates those hashes and therefore fails closed.

The outcome-free checks are

```sh
applications/bin/Rscript45 applications/gs2020/protocol/v283-targets/test-protocol.R
applications/bin/Rscript45 applications/gs2020/R/test_gs_v283_targets_contract.R
```
