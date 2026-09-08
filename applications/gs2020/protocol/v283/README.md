# Graham--Svolik prospective numerical protocol v2.8.3

**Protocol ID:** `gs2020-paperps-prospective-2026-09-07-v2.8.3-v1`

This directory specifies the code contract for the fresh Graham--Svolik run.
It does not contain or generate application outcomes. The authors have seen
the earlier corrected q=1 analysis, Ballard--Rosa v2.8.1 results, and the
complete Graham--Svolik v2.8.2 numerical and target diagnostics. A post-run
audit reconstructed 43 of 54 stage-one start hashes. Every exact start and
every q=0 and q=2 start matched. Eleven of the twelve q=1 perturbed or
constant inputs did not. The complete v2.8.2 tree is therefore invalidated
for substantive use and no estimate, fit, gate, score, target, or interval is
reused. It remains untouched for audit. No Graham--Svolik v2.8.3 outcome
existed when this candidate was written.

The root cause was the R-to-Torch construction boundary in the application
initializer. `torch_tensor()` could retain storage borrowed from a temporary
R array on this build. Later collection or allocation could then overwrite a
long-lived tensor even when a subsequent tensor-level detach and clone had
appeared to separate its storage. Only the rank-one loading `A` changed in the
reproduction. An immediate clone at tensorization, backed by a private host
copy, kept it unchanged.

The run keeps the v2.8 model, estimator, numerical schedule, seeds, gates, and
thresholds unchanged. This includes the scalar-shape correction and defensive
gradient-state checks. Version 2.8.3 changes only storage ownership. Canonical
start values are private ordinary-R arrays. Exact, perturbed, and constant
templates are all derived before the first optimizer call. Each template is
then independently tensorized with an immediate clone, and its hash must equal
the first staged-optimizer ledger hash. The package loader separately performs
a private host-array round trip followed by another immediate tensor clone
before `load_state_dict`. Per-stage checks verify that optimizer work never
changes the caller input. The two layers are deliberate defense in depth.

The five historical outer-training q=1 states may initialize only their
matching folds. No historical full-sample fit exists. The state from
outer-fold position one therefore receives a deterministic affine remap to
the full-sample moderator and coefficient scales and initializes the fresh
full-sample optimization. Every trainable parameter is reoptimized. No old
estimate, gate, target result, standard error, or interval is reused.

Before accepting those states, preflight mechanically audits every retained
historical inner-tuning record. It verifies the exact outer-training
complement, respondent-clustered inner folds, disjoint and exhaustive inner
partitions, training-only moderator and contrast preprocessing, respondent-ID
bindings and means of the sequence scores, candidate specifications, gate and
selection recomputation, and agreement between the selected and refitted
specification. This is an internal-consistency audit of the exact-hash-bound
retained metadata. The historical CV networks were not retained, so the audit
does not reconstruct their likelihoods or independently prove the execution
that originally produced the numeric scores.

The q=1 remap also requires the fit-level preprocessing aliases and column
names to agree with the portable state, and checks the preprocessing and
architecture coefficient scales under the frozen `1e-12` rule. Its ReLU
baseline correction computes the unbounded `g(a)-g(0)` deviation directly;
it never differences clipped coefficient predictions. Raw prediction
invariance and the moderator affine identity are then checked over every row
of the fresh fitting scope, including the full-sample rows that were absent
from the historical transfer source.

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

The separate `v283-targets` protocol freezes a smaller core family. It covers
the co-party mean and four quantities centered on restricting voting. The
choice of targets is outcome-informed and remains descriptive even if every
gate passes. The wider seven-action table remains secondary. Reducing the
reporting and multiplicity burden cannot repair a weak target-specific Riesz
problem.

The q=2 sentinel and timing rule inspect computation only. They never inspect
a substantive estimate or select a q=1 target. The rule uses completed q=0 and
q=1 fit runtimes plus a fixed projection from the q=2 sentinel, so operator
pauses cannot change the decision. If it stops q=2, the analysis checkpoint
still reports q=0 versus q=1 and records q=2 as deferred. It does not require a
nonexistent q=2 assembly after a stop decision.
If the timing decision continues q=2, the analysis stage instead requires the
completed q=2 assembly and GH15 fixed-state refinement. This conditional
dependency is explicit in the stage registry.

Every outcome command requires the whole tracked repository to be clean. The
implementation manifest hashes the transitive package and application files
used by the numerical and inference paths. It also hashes every package R file
that `pkgload` can load and the exact R 4.5 wrapper. Checkpoints bind the R
executable hash, R and package versions, native torch version, and frozen RNG
kinds. The wrapper creates its common runtime link once and thereafter verifies
the exact target without replacing the link during concurrent starts. Visible
untracked files are rejected, and every manifest and protocol
file must already be tracked. Outputs, when authorized in a later step, will be
immutable below
`runs/gs2020/v283/prospective-2026-09-07-v1`. The historical q=1 artifacts are
read-only and remain bound by exact input hashes. They are never relabelled as
v2.8.3 results.

A hard kill after the checkpoint data rename but before the SHA sidecar rename
is recoverable. The reader validates the installed envelope and its complete
binding before recreating only the missing sidecar. A sidecar without data, a
foreign binding, or a malformed envelope remains a hard failure. Temporary
files that were never installed are not counted as prior outcomes.

During construction, the manifests used the explicit value
`PENDING_FINALIZATION`. The production validator does not accept that value.
The committed v2.8.3 protocol contains exact SHA-256 values and no placeholder.
Any later code change invalidates those hashes and therefore fails closed.

Run the outcome-free contract checks from the repository root with

```sh
CONJOINTSTRUCTURAL_ROOT=/path/to/ConjointStructural \
  applications/bin/Rscript45 applications/gs2020/protocol/v283/test-protocol.R
applications/bin/Rscript45 applications/gs2020/R/test_gs_v283_contract.R
```

The `status` command is read-only. All other runner commands generate v2.8.3
application outcomes and must not be used until the later rerun step.
