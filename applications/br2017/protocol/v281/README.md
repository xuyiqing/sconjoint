# Ballard--Rosa prospective numerical protocol v2.8.1

**Protocol ID:** `br2017-paperps-prospective-2026-09-07-v2.8.1-v1`

This directory specifies the code contract for the next Ballard--Rosa run. It
does not contain or generate application outcomes. The authors have already
seen v2.1 and v2.7 results. The first v2.8 fit attempt stopped before a fit
checkpoint because the gradient audit dropped the shape of a one-dimensional
ReLU bias parameter. It produced only a pre-estimation data checkpoint. That
checkpoint contains the prepared response along with the other fitted-data
inputs, but it contains no fitted state, gate result, or substantive estimate. Version
2.8.1 changes only that implementation shape handling. It carries forward the
v2.8 model, estimator, estimands, seeds, optimization schedule, gates, and
thresholds without change. The v2.8 preflight and failed in-memory
optimization state may not be reused. The v2.7 results remain diagnostic and
are never relabelled as v2.8.1.
The same amendment adds defensive name, length, dimension, and finiteness
checks when gradient-audit directions are loaded. These checks fail closed on
a malformed state. They do not change a valid fitted objective or parameter.

The inherited v2.8 protocol fixes the meaning of multiple starts. The estimator selects the
lowest penalized respondent-sequence mean NLL among computationally admissible
starts. A worse start is useful evidence about the surface but is not a reason
to fail an otherwise valid fit. Only admissible starts within relative
objective gap `1e-6` are near-optimal and enter target-stability comparisons.
If the near set contains only the selected start, the comparison is recorded
as not applicable. It is neither a failure nor evidence of observed
cross-start stability. Every candidate state and its numerical evidence is
retained in portable form. A lower-objective but inadmissible state triggers an
explicit fail-closed record. A missing or nonboolean post-polish projection
diagnostic makes a start computationally inadmissible.

The target-stability statistic keeps its v2.6 calibration. It is the largest
selected-reference movement over the full-fit surface and over the surfaces
formed by replacing one outer fold at a time. It is not the range over a
Cartesian product that changes several outer folds simultaneously.

The existing float64, full-batch, strong-Wolfe L-BFGS route and its settled
endpoint protections are retained. Absolute optimizer stopping tolerances
remain disabled. The release diagnostic is the preprocessing-normalized
blockwise projected KKT RMS, with the unchanged cutoff `1e-5`. Maximum
coordinate residuals are also recorded descriptively so that RMS aggregation
cannot conceal an isolated coordinate. A separate deterministic audit
compares autodiff with central finite differences by parameter block over a
fixed step ladder. A block passes only if two adjacent step sizes agree. This
rule is needed because the flexible ReLU mean can have a kink close to a fitted
endpoint. Failure may indicate local nonsmoothness or a gradient contract
problem. Either case withholds release. It does not authorize a larger KKT
cutoff.

The Riesz solve retains structural-Gram whitening and adds diagonal
equilibration through a congruence transformation of the complete penalized
system. This is numerical preconditioning, not a change in the ridge estimand.
Numerical rank is assessed after equilibration so coordinate scale alone
cannot cause a positive-definite direction to be discarded. When the
scale-free zero-ridge system is rank deficient, the code retains the original
numerical pseudoinverse representative defined by the frozen eigenvalue cutoff
because a congruence transform need not preserve that representative. Every
selected and grid solve records whether equilibration or this fallback was
used and records its backward error. A positive-ridge system whose
equilibrated condition is below the frozen scale-free rank threshold is
withheld. It is never replaced by a truncated ridge solution. Backward error
alone cannot certify forward accuracy in that case.
Ridge candidates are `0`, `Ntrain^-1`, `Ntrain^-3/4`, and `Ntrain^-1/2`.
Each primitive is tuned using respondents contained entirely inside the
corresponding outer-training sample. The fast held-nuisance split is
diagnostic. Formal release requires the existing signed inner-nuisance-refit
evidence. No Riesz, ridge, derivative, information, KKT, integration, loading,
or target-stability threshold is relaxed.

The separate `v281-targets` protocol freezes a smaller core family. All v2.5
targets remain available as secondary plug-ins or diagnostics, but they are
not silently promoted into the post-diagnostic manuscript core. This reduces
reporting and multiplicity burden. It cannot by itself repair a weak
target-specific Riesz problem.

The q=2 sentinel and timing rule inspect computation only. They never inspect
a substantive estimate or select a q=1 target. The rule uses completed q=0 and
q=1 fit runtimes plus a fixed projection from the q=2 sentinel, so operator
pauses cannot change the decision. If it stops q=2, the analysis checkpoint
still reports q=0 versus q=1 and records q=2 as deferred. It does not require a
nonexistent q=2 assembly after a stop decision.

Every outcome command requires the whole tracked repository to be clean. The
implementation manifest hashes the transitive package and application files
used by the numerical and inference paths. It also hashes every package R file
that `pkgload` can load and the exact R 4.5 wrapper. Checkpoints bind the R
executable hash, R and package versions, native torch version, and frozen RNG
kinds. Visible untracked files are rejected, and every manifest and protocol
file must already be tracked. Outputs, when authorized in a later step, will be
immutable below
`runs/br2017/v281/prospective-2026-09-07-v1`. Neither the v2.7 tree nor the
stopped v2.8 tree is modified or relabelled. The v2.8.1 preflight records and
rechecks exact hashes for the two files in the stopped v2.8 tree.

During construction, the manifests used the explicit value
`PENDING_FINALIZATION`. The production validator does not accept that value.
The committed v2.8.1 protocol contains exact SHA-256 values and no placeholder.
Any later code change invalidates those hashes and therefore fails closed.

Run the outcome-free contract checks from the repository root with

```sh
CONJOINTSTRUCTURAL_ROOT=/path/to/ConjointStructural \
  applications/bin/Rscript45 applications/br2017/protocol/v281/test-protocol.R
applications/bin/Rscript45 applications/br2017/R/test_br_v281_contract.R
```

The `status` command is read-only. All other runner commands generate v2.8.1
application outcomes and must not be used until the later rerun step.
