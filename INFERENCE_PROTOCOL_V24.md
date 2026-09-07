# Lean prospective validation protocol v2.4

**Protocol ID:** `paperps-inference-2026-09-06-v2.4`

**Plan ID:** `paperps-lean-validation-2026-09-06-v2.4-v1`

**Frozen on:** 2026-09-06

**Status:** validation design, truths, fixed rules, evaluator, and PSOCK
launcher requirement frozen; evaluator approval v1 superseded after an
outcome-free startup failure; all outcomes locked pending approval v2

**Pre-outcome amendment on 2026-09-07:** The first freeze mislabeled the
directional mean and second-moment columns as inferential primitives. They are
supporting audit columns. The minimal inferential vector is `(H_Z, H_R)`, as
defined in the paper and implemented by the existing target-specific
inference code. The target registry and its hashes were corrected before any
pilot or evidence outcome was generated. The target definitions, target
truths, and numerical decision rules did not change. The original freeze
remains available in Git history.

The final pre-outcome amendment on the same date completes the concrete
end-to-end evaluator and its hash contract. It also makes explicit details
already required by the decision registry. These include the
dimension-adjusted machine-epsilon rank rule, both GH31 checks, the fixed ridge
neighborhood, target constraint maps, and the pilot information firewall. The
implementation adds literal zero-Adam exact-state GH31 polishing. It also
repairs a general R/Torch storage problem found during toy testing by cloning
fit tensors into owned storage before optimization. No v2.4 outcome existed
when these changes were made. The immutable design, DGP, truth, target, seed,
and decision values did not change. The target registry now states the
zero-variance tie conventions already used by the paper's estimands.

**Pre-outcome launcher amendment on 2026-09-07:** The first integrity-pilot
launch under evaluator approval v1 used the frozen six-worker fork schedule.
The macOS fork workers failed during process startup. The signed private ledger
has attempt count zero and no records. There is no public manifest, persisted
replication artifact, or target or gate result available for inspection. Thus
the recorded count of completed or persisted outcomes is zero. A doomed child
could in principle have instantiated an ephemeral in-memory outcome before its
abort, so the record makes no stronger claim. The response-free plan, DGP,
truths, targets, thresholds, seeds, statistical evaluator, and family decision
rules are unchanged. The fixed seeds also make the retry outcome-independent.
Evaluator settings v2 replace only the parallel launcher contract with six
spawned base R PSOCK processes. Fork workers are no longer permitted. Approval
v1 remains in the repository as superseded audit history. Approval v2 is
absent, so the state machine remains fail closed until the amended
implementation is separately committed, hash-bound, and approved.

Protocol v2.4 supersedes v2.3 prospectively. It does not alter or delete any
v2.3 file. The v2.3 calibration attempt is retained as audit history. It did
not produce an approved production calibration.

## 1. Fixed rules rather than calibrated thresholds

Version 2.4 has no calibration stream and selects no reporting threshold by
simulation. Numerical tolerances, approximation budgets, boundary rules, and
validation criteria are fixed before any version 2.4 simulation outcome or
application rerun. Earlier application audits helped motivate the redesign,
so this is not a claim that the method was written before those studies were
seen. No observed application choice enters the validation study or changes a
rule after this freeze. Simulation evaluates the fixed rules. It cannot tune
them. The frozen family-by-cell matrix determines approval mechanically after
the untouched evidence study is complete. A later approval file records that
result and cannot override it.

The machine-readable rules are in
`inst/protocol/inference-v2.4-decision-registry.csv`. In brief, regularity uses
the minimum 1.96 studentized distance over the target's primitive constraints
and the active covariance eigenvalue. For rank one, the latter is
`trace(Sigma)`. It is a fit-regularity primitive rather than a reported target
family. Chart and nonlinear discrepancies may
use at most 0.10 standard errors. The honest information-scaled held-out Riesz
defect may not exceed 0.20. Movement over the fixed ridge neighborhood may not
exceed 0.20 standard errors. The ridge is `N_train^(-3/4)` and its fixed
sensitivities are `N_train^(-1)` and `N_train^(-1/2)`. These are checks, not a
tuning grid.

The numerical rules use a blockwise scale-free KKT tolerance of `1e-5`,
nested and near-optimum objective tolerances of `1e-6`, a near-optimum target
spread budget of 0.10 standard errors, target and influence integration
budgets of 0.05 standard deviations, probability-scale loading invariance of
`1e-8`, derivative error of `1e-4`, null-space relative materiality of `1e-6`,
and a dimension-adjusted machine-epsilon SVD tolerance.

The KKT, loading, and nesting gates apply to the selected full-sample state
and the two selected outer-fold states. Every state within `1e-6` of the best
objective in its scope must also be finite, double precision, and successfully
polished. Its target movement enters the near-optimum stability gate. A
discarded start outside that set is retained as a diagnostic but does not
invalidate the selected estimator merely because its own KKT residual is
large. The three exact-state GH31 repolishes must independently be finite,
double precision, nonworsening, successfully polished, and pass the same KKT
and loading checks.

Directional shares are evaluated by the exact normal-CDF formula at positive
directional variance and by its declared tie rule at zero variance. Preference
direction excludes a tie, while compensating benefit includes it. No positive
numerical floor changes the target. Each share nevertheless requires
its own directional variance to have a studentized distance of at least 1.96
from zero. Derivative, Riesz, and null-space diagnostics include that auxiliary
variance primitive. Thus the limit keeps evaluation stable while the
regularity gate prevents ordinary inference at the boundary.

All 78 response-free truths for the six cells, twelve target variants, and
the fit-regularity primitive are
frozen in `inst/protocol/inference-v2.4-target-truths.csv`. Choice probabilities
use a deterministic 257-node normal Gauss-Hermite rule. This truth table and
its hash were fixed before any design-matched choice was generated.
The target registry also fixes every contrast, averaging rule, subgroup split,
and primitive decomposition. This prevents an evaluator from silently changing
the estimand after outcomes exist.

## 2. Lean design

The design has three deterministic response-free templates. Each contains
400 respondents selected from the v2.3 response-free BR2017, GS2020, and
SW2022 payloads. Selection is stratified jointly by exact task count and the
above-mean split of the first respondent-varying moderator. Every nonempty
stratum receives one respondent. Remaining places use Hamilton proportional
allocation. A salted deterministic hash orders respondents within strata.
The source wrapper, selected respondent vector, and derived payload all have
frozen SHA-256 hashes. In every replication, the generator samples 400 complete
respondent records with replacement from that template, retains each selected
respondent's whole task sequence, and assigns new replication-specific IDs.
It then draws the persistent latent utility shock and choices. The frozen truth
is the target under the original 400-respondent empirical distribution. Thus
the study includes the respondent-composition variation that enters the
paper's superpopulation influence function while preserving the observed
sequence structures.

There are six cells. Three use an interior regular DGP, one combines a
near-boundary probability with an exactly zero MRS denominator, one generates
rank zero while fitting rank one, and one puts the target mean and loading in
the weakest design direction. Two integrity replications per cell do not
enter evidence. The evidence study requires exactly 100 replications per
cell. It runs in batches of ten. All 600 attempts are required. There is no
early success or futility stop.

The twelve integrity pilots are also a fixed runtime gate. They may be
inspected only for execution integrity, elapsed time, and resource use. The
parallel backend is exactly six spawned base R PSOCK processes. A fork backend
is not permitted. Evidence proceeds only if the complete six-cell study
is projected to finish within eight wall-clock hours on those workers. If it
does not, work stops and any redesign receives a new protocol ID. Pilot target
results and operating characteristics cannot alter this protocol.
The result firewall is a procedural and hash-audited restriction rather than
an encryption claim. Pilot artifacts exist so failures can be audited, but
the checkpoint interface exposes only the prespecified integrity and runtime
metadata before the go or no-go decision. A signed private ledger binds every
sealed pilot artifact by path and file hash. An interrupted wave may resume
only by revalidating an exact on-contract artifact, and the runtime projection
retains the original wave start so interruption time cannot make it optimistic.

Every fit uses rank one, two respondent outer folds, two inner folds, two
starts, GH15 integration checked against GH31, and 9,999 common respondent
multipliers. One fit is reused across the six prespecified target families.
Target-specific Riesz problems are still solved separately.

Every released family uses a 95 percent simultaneous confidence set. For each
multiplier draw, respondent influence columns are centered and studentized by
the square root of their centered sum of squares. The draw statistic is the
largest absolute value over all unique registered family primitives. The
critical value is the type-1 empirical 0.95 quantile of the 9,999 draw
statistics, with no interpolation. These choices are fixed rather than left to
the future evaluator.

## 3. Exact DGP scale contract

Let `S` be the diagonal respondent-weighted RMS scale of the raw profile
contrasts. It has no centering term. The estimator sees `X* = X S^(-1)`.
Regular design directions use raw coefficient vectors
`S^(-1) v_j lambda_j^(-1/2)` and exact dual target contrasts
`S v_j lambda_j^(1/2)`. Their respondent-weighted score RMS is one.

The weak cell instead uses `S^(-1) v_min` for both the target mean direction
and rank-one loading. It is not divided by the square root of the smallest
eigenvalue. Its exact dual is `S v_min`, and its score RMS remains
`sqrt(lambda_min)`. This preserves rather than normalizes away the weak
inverse problem. Raw- and estimator-scale utility indices must agree to
machine precision.

## 4. Evidence decisions

Every cell must complete at least 95 percent of its 100 evidence replications.
For each regular cell and target family, at least 75 percent must release.
Conditional simultaneous coverage among complete family releases must be at
least 90 percent. We report its binomial Monte Carlo standard error and a
two-sided 90 percent Wilson interval using `qnorm(0.95)` without a continuity
correction. The interval is descriptive rather than a second approval gate.
This keeps the 100-replication study informative without giving a nominally
correct procedure a large chance of failure from Monte Carlo noise alone.

Choice probabilities and directional shares are separate families because a
rank-zero boundary affects their regularity differently. The combined BR cell
has an exact boundary only for MRS. Its choice-probability and subgroup-ratio
families are near-boundary rather than exact-boundary. In the GS weak cell,
only smooth means, directional shares, and directional heterogeneity load on
the deliberately weakest direction. Choice probabilities, subgroup ratios,
and MRS retain their regular classifications there.

The rank-zero BR cell places the active rank-one covariance eigenvalue at an
exact global fit boundary. Most substantive targets in that cell remain
defined and may be interior. The question is whether the full rank-one
procedure nevertheless makes an invalid regular family release when an
applicable global regularity primitive is exactly on its boundary. This is not
a claim that every substantive truth in the cell is itself undefined.

For an affected family at an exact boundary, both the observed invalid regular
release rate must not exceed 0.05 and its one-sided exact 95 percent upper
Clopper-Pearson confidence limit must not exceed 0.10. With 100 completed replications, the
exact-upper-limit rule is the binding one and permits at most four invalid
regular releases. In a weak or bounded affected family, at most five releases
establishes successful withholding. Alternatively, at least 30 releases can
be informative if the same coverage rule used for regular cells holds. Six
through 29 releases is inconclusive.

The approval matrix is
`inst/protocol/inference-v2.4-approval-matrix.csv`. A family release means a
complete simultaneous interval for every registered family member, computed
with common respondent multipliers, after every applicable fit and target
rule passes. An incomplete family result and a Bonferroni fallback do not count
as a release. A family is approved only if every required matrix row passes.
The file contains all 36 family-by-cell pairs. Thirty are required. The six
unaffected pairs in the two special target cells are retained as descriptive
controls but do not determine approval. This avoids adding redundant regular
gates after every family has already been tested in all three regular design
cells.

## 5. Outcome lock

No v2.4 design-matched synthetic outcome had been generated at this final
pre-outcome amendment. The response-free design, truths, target definitions,
seeds, decision rules, and one concrete end-to-end evaluator are frozen. That
evaluator executes the respondent-sequence likelihood, nested respondent
splitting, target-specific Riesz correction, one-step inference, fixed
reporting rules, and common respondent multipliers. Before any outcome can be
generated, its implementation manifest, settings, and interface must be bound
by SHA-256 in `inst/protocol/inference-v2.4-evaluator-approval.dcf`.

At the instant of the evaluator amendment, that approval file was absent and
`scmix_v24_generate_study_outcome()` failed closed. The live state machine,
rather than this historical statement, determines whether pilot or evidence
generation is currently permitted. There is no calibration-values file, and a
family approval can exist only after all 600 evidence attempts are recorded.

Evaluator approval v1 was later recorded, but its macOS fork launch produced no
completed or persisted replication and exposed no target or gate result. The
launcher amendment supersedes that approval and requires settings and approval
v2 with the six-process base R PSOCK backend. Approval v2 is absent. The
recorded outcome count therefore remains zero and both pilot and evidence
generation are locked.

Even a successful study supplies implementation evidence only for these fixed
rules on the three finite empirical design templates. It does not certify an
application or all possible conjoint designs. Every application must still
pass the paper's own design, fit, numerical, approximation, and target-specific
information gates before a result is released.
