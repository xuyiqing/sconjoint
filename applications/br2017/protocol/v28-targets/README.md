# Ballard--Rosa prospective q=1 target protocol v2.8

**Protocol ID:** `br2017-paperps-targets-2026-09-07-v2.8-v1`

This target stage is prospective with respect to v2.8 outcomes. It produces no
result merely by being validated. The authors had already seen the v2.1 and
v2.7 application and target diagnostics when this protocol was written. The
v2.7 target protocol and outputs remain historical diagnostics. They are not
renamed, reused as estimates, or presented as prospective v2.8 evidence.

The stage reads only the immutable, hash-bound v2.8 q=1 assembly and repaired
input checkpoint. It does not modify fit, assembly, refinement, or rank
comparison checkpoints. It never uses q=2 to choose a target or family.

## Core and secondary targets

The post-diagnostic manuscript core has exactly five frozen targets in two
multiplicity families. The targets were chosen after earlier results had been
seen. They are prospective only with respect to the v2.8 numerical run and are
not confirmatory on the same Ballard--Rosa sample. `context_preferences`
contains the mean progressivity slope and the
position-neutral choice probability for progressive versus flat taxation.
`heterogeneity_core` contains the direction share for the progressivity slope,
the compensating-benefit share for progressive versus flat taxation, and the
directional-heterogeneity ratio for the progressivity slope. The latter family
is represented by four unique primitive coordinates. The exact targets,
families, and primitive mapping are frozen in `core-target-registry.csv`,
`core-family-registry.csv`, and `core-primitive-registry.csv`.
For the directional-heterogeneity ratio, cross-fit aggregation first combines
the raw mean and second-moment support values across all held-out respondents.
It then computes global `H_Z` as the global cross-fit second moment minus the
squared global cross-fit mean. Each fold's Riesz gradient remains centered only
at its outer-training mean. This prevents a pooled cross-fit center from
feeding back into nuisance or ridge fitting while preserving the declared
finite-population variance, including between-fold differences.

All 72 targets from the byte-identical v2.5 registry remain available as
secondary plug-ins and diagnostics. They are not members of the v2.8
manuscript-core multiplicity families. This smaller core reduces reporting and
multiplicity burden. It does not change an estimand and cannot turn weak
target-specific information into valid inference. The six v2.5 structural
AMEs remain explicitly unavailable until an exact Ballard--Rosa fielded-profile
and revenue-standardization constructor is frozen. The generic independent
uniform-draw helper is not an acceptable substitute.

## Riesz and inference contract

Each registered core target is run in its own DML call. This prevents one
target's bookkeeping or primitive geometry from being substituted for
another's. Each primitive uses the ridge grid `0`, `Ntrain^-1`,
`Ntrain^-3/4`, and `Ntrain^-1/2`. Selection uses respondents contained
entirely in the relevant outer-training fold. A ridge is tied when its absolute
risk gap is at most `1e-12` times the larger of one and the absolute minimum
risk. Ties are resolved in favor of the larger ridge. The target-specific seed is
the frozen family seed plus the target's original v2.5 index. After each
target is transformed, the influence columns are recombined for the common
familywise multiplier. The Riesz system first uses structural-Gram whitening
and then a congruent diagonal equilibration of the complete penalized system.
This is a numerical re-expression of the same full-rank ridge problem. A
scale-free singular zero-ridge system retains the original numerical
pseudoinverse representative defined by the frozen eigenvalue cutoff rather
than changing it through the congruence transform. Every selected and grid
solve retains an audit of the route used and its backward error.
A positive-ridge system below the frozen scale-free rank threshold is withheld
rather than truncated. A small backward error cannot certify forward accuracy
for such an ill-conditioned system.

The fast validation route shares the already fitted outer nuisance and is
diagnostic only. Formal inference requires signed inner nuisance refits. The
Riesz defect cutoff remains `0.20`. The separate relative Riesz-correction
sensitivity cutoff also remains `0.20`. This latter diagnostic is not the
ridge-neighborhood target-movement gate. That gate remains `0.20` standard
errors and is recorded as unavailable until the full target and interval
endpoint mapping has been run. The analytic derivative, null-space, fit KKT,
near-objective, selected-reference target-movement, and loading-invariance
cutoffs also remain
fixed. Nothing in v2.8 relaxes an inferential threshold.

The `0.10` target-movement diagnostic retains the statistic calibrated in
v2.6. It takes the largest selected-reference movement over the full-fit
surface and over one-outer-fold-at-a-time substitution surfaces. It is not an
uncalibrated Cartesian-product spread across simultaneous fold alternatives.

Within each of the two complete core families, the stage uses one common bank
of 9,999 respondent-level standard-normal multiplier draws. Missing or failed
fit, Riesz, signed-inner-refit, integration, derivative, ridge-neighborhood,
or near-optimum target-stability evidence withholds any numerically qualified
release. A missing record is never counted as a pass. Even if every numerical
gate later passes, same-sample intervals for this outcome-informed core remain
descriptive and conditional. Confirmatory claims require independent data, a
genuinely pre-result registry with suitable multiplicity control, or valid
selective inference.

## Integrity and execution boundary

Every outcome command requires the whole tracked repository to be clean. The
implementation manifest covers the parent protocol, target wrappers, and the
transitive package sources used by the target path. It hashes every package R
file that `pkgload` can load and the exact R 4.5 wrapper. Every checkpoint also
binds the R executable hash, R and package versions, native torch version, and
frozen RNG kinds. Visible untracked files are rejected, and every
manifest and protocol file must already be tracked. During code construction,
`PENDING_FINALIZATION` marks hashes that must be replaced after all v2.8 code
is complete. Production validators reject those placeholders, so no outcome
can be produced under an unfinished manifest. The protocol checksum registry
must likewise be finalized before any outcome command.

After finalization and commit, the intended order is `status`, `bind`, `basis`,
one `family` command for each core family, and `export`. The stage registry
contains only these core steps. Secondary plug-ins require a separately
recorded diagnostic action and cannot be silently folded into the core export.
Existing outputs are never overwritten. No external push is authorized.

Run the outcome-free static check from the repository root with

```sh
applications/bin/Rscript45 \
  applications/br2017/protocol/v28-targets/test-protocol.R
applications/bin/Rscript45 \
  applications/br2017/R/test_br_v28_targets_contract.R
```

The first command can report pending hashes while implementation is underway.
That message is a construction status, not permission to execute outcomes.
