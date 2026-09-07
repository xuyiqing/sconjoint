# Prospective inference-validation protocol v2.6

**Protocol ID:** `paperps-inference-2026-09-07-v2.6`

**Plan ID:** `paperps-rapid-validation-2026-09-07-v2.6-v1`

**Frozen:** September 7, 2026, before any v2.6 synthetic outcome.

**Status:** The design, evaluator, truths, targets, gates, seeds, schedule,
and continuation endpoints are frozen. Outcome generation remains locked
until the complete implementation is hash-approved in a later commit.

## 1. Purpose and interpretation

Version 2.5 stopped under its prospective runtime rule before a complete
replication artifact existed. No statistical content was inspected. Version
2.6 uses only the permitted runtime information to shorten the same-day lane
depth. It does not weaken an estimator, target, interval, or gate.

Version 2.6 separates three claims. Software-integrity tests establish that
the implemented algebra and state machine obey their contracts. Eight
replications in each of six cells provide provisional end-to-end,
design-matched evidence. They can reveal gross failures but cannot establish
nominal coverage. The precommitted continuation to 50 replications per cell
is the minimum endpoint at which the frozen family rules can produce an
approval. A continuation to 100 per cell is the preferred precision endpoint.

Application eligibility is separate. An application result must pass its own
fit, local-rank, Riesz, ridge, integration, domain, and specification checks.
A small Monte Carlo prefix neither certifies nor disqualifies an application.

## 2. Exact carry-forward from v2.5

The following are carried forward exactly from v2.5:

- the three response-free empirical templates and their subset hashes
- the six DGP cells and all 78 target truths
- the six target families and complete target registry
- the family-by-cell approval matrix
- rank-one fitting, constant-versus-linear mean tuning, two respondent outer
  folds, two respondent inner folds, and two optimizer starts
- GH15 fitting and both GH31 integration-refinement branches
- the fused ordinary-target DML computation and the separate registered
  heterogeneity adapter
- the fixed Riesz ridge and its two sensitivity ridges
- 9,999 common respondent multipliers and simultaneous family intervals
- all fourteen fit and target gates and their thresholds
- fail-closed target and family handling

The v2.6 tests compare the frozen tables byte for byte and compare normalized
v2.5 and v2.6 statistical source. The only substantive code differences are
the new identifiers, seed namespace, state paths, scheduler, and endpoint
interpretation.

## 3. Same-day preliminary stage

The preliminary stage has eight replications in each of six cells. It has 48
attempts in total. It runs in four waves. Each wave contains two replications
from every cell and therefore has 12 jobs. Twelve base-R PSOCK processes are
used, with one Torch thread per process and two deterministic lanes per cell.
In wave `w`, lane 1 runs replication `2w - 1` and lane 2 runs replication
`2w`.

Wave 1 is both the sole sealed integrity and runtime checkpoint and the first
fixed evidence prefix. It counts as replications 1 and 2 in each cell. This
choice is frozen before any outcome is generated. Before the go/no-go
decision, the public record may expose only assignments, hashes, completion,
process identity, worker thread counts, resource use, and elapsed wall time.
It may not expose choices, fits, estimates, standard errors, gates, intervals,
release decisions, or coverage.

The binding runtime projection is

```
1.25 * 4 * complete integrity-valid wave-1 wall time.
```

It must not exceed five hours. The equivalent wave-1 cutoff is 3,600 seconds.
An integrity-valid wave 1 cannot be rerun, even if it is slow. A documented
infrastructure interruption may resume only the same assignments, seeds, and
evaluator bytes. A numerical or statistical gate failure is an outcome and is
not a reason to rerun a job.

If wave 1 passes, waves 2 through 4 run without intermediate statistical
inspection. Every job writes an atomic artifact or guarded-failure record.
Every wave writes a hash-bound manifest. All 48 attempts are required. No
target, family, or cell may be removed after the artifacts are opened.

After all four waves, the program reports completion, release, simultaneous
coverage, false-release, and withholding counts with exact binomial
intervals. The only permissible overall statuses are
`preliminary_end_to_end_evidence_complete`,
`preliminary_end_to_end_evidence_incomplete`, `stopped_on_integrity`, and
`stopped_on_runtime`. There is no eight-replication family approval and no
status called validated, calibrated, approved, or coverage-passed.

## 4. Confirmatory continuation

The same evaluator and seed stream continue unconditionally through
replication 50 in every cell if their bytes do not change. Replications 1
through 8 count toward this endpoint. The frozen v2.5 rules apply only after
all 300 attempts are present:

- at least 48 of 50 replications complete in each cell
- a regular family-cell pair releases at least 38 times
- conditional simultaneous coverage is at least 0.90
- an exact-boundary pair has at most one invalid release under the joint
  point-rate and one-sided Clopper--Pearson rule
- a weak or bounded pair succeeds by withholding with at most two releases,
  or is informative with at least 15 releases and adequate coverage; three
  through fourteen releases are inconclusive

The preferred endpoint continues the same stream to 100 replications per
cell. The 50-replication result remains a prespecified minimum endpoint and is
not discarded. The 100-replication result improves Monte Carlo precision and
must be reported whether it strengthens or weakens the earlier conclusion.
Any change to the evaluator, target, DGP, threshold, or seed rule after the
preliminary results are opened requires a new protocol ID and fresh affected
seeds.

## 5. Seeds and canonical paths

The master validation seed is 2020000000. For cell index `c` and replication
`r`, the base seed is `2020000000 + (c - 1) * 1000000 + r`. Resampling,
latent, choice, fit, and multiplier substreams use offsets 0, 200000, 400000,
500000, and 600000. This namespace does not overlap v2.4 or v2.5 and remains
below R's integer maximum through replication 100.

The sole state directory is `inst/protocol`. The sole run directory is
`runs/v26/rapid_validation_2026-09-07`. Alternate and symbolic-link state or
run paths are rejected.

## 6. Application rerun boundary

The same-day application contract is separate from the simulation evaluator.
For Ballard--Rosa, rank zero and rank two condition on the mean architecture
selected by rank one on the same training sample. Rank zero is exact. Rank
one uses GH31 as primary and GH45 as a fixed-state integration check. Rank two
uses GH9 as primary and GH15 as a fixed-state check. Its second loading starts
from a deterministic, small direction orthogonal to the rank-one loading. One
prespecified outer fold is the timing sentinel before the remaining states
run in parallel.

Within each fitted specification, optimizer starts are selected by the
declared penalized training criterion. The unpenalized respondent-sequence
log score is used only for held-out rank and specification comparison. These
two decisions must not be conflated.

Formal target-specific DML inference is primary only at rank one. Rank zero
and rank two are predictive and plug-in sensitivity analyses. In particular,
rank zero does not support regular directional-share inference. Saved v2.1
states may serve only as hash-bound initial values and all reported v2.6
quantities and gates must be recomputed.

## 7. Lock

No v2.6 synthetic outcome exists at freeze. A separate approval must bind the
implementation manifest, exact source commit, evaluator settings, and
canonical paths before the first outcome can be generated. Application
outcomes may not alter the simulation cells, targets, gates, or extension
rules. The preliminary stage is development evidence. It is not a substitute
for the 50-per-cell confirmatory endpoint.
