# Prototype inference-validation protocol v2.6

Status: prospective design draft. No v2.6 synthetic outcome may be generated
until this document, the machine-readable protocol, the evaluator
implementation, and their hashes have been reviewed and frozen under a new
v2.6 identifier.

## Purpose

Version 2.6 separates a same-day development check from confirmatory Monte
Carlo validation. The same-day stage is designed to finish in no more than
about five hours on the project Mac and to leave enough time to rerun the
Ballard--Rosa and Graham--Svolik applications. It can support transparent
preliminary results in a draft. It cannot certify nominal coverage or create a
method-family approval.

The confirmatory stage extends the same prospectively frozen replication
stream. It is not redesigned after the same-day results are opened. The
minimum endpoint is 50 replications per cell. A 100-replication endpoint is
preferred for stronger publication evidence.

## What v2.6 leaves unchanged

The v2.6 evaluator should copy and hash-bind the v2.5 statistical evaluator.
Except for a new protocol identifier, new nonoverlapping seeds, new canonical
state and run directories, and the schedule described below, it should retain
all of the following without modification.

- The three response-free empirical design templates and their subset hashes
- The six data-generating cells
- The empirical-superpopulation truth definition and all 78 target truths
- The six target families and the complete target registry
- The family-by-cell approval matrix
- Rank-one fitting, two respondent outer folds, two respondent inner folds,
  and two optimizer starts
- The constant and linear mean candidates and respondent-sequence tuning score
- GH15 fitting and both GH31 integration-refinement branches
- The fused ordinary-target DML calculation and separate registered
  heterogeneity adapter
- The fixed Riesz ridge and its two sensitivity ridges
- The common 9,999 respondent multipliers and simultaneous family intervals
- Every fit, rank, regularity, Riesz, ridge, derivative, loading-scale,
  integration, near-optimum, and nonlinear-map gate
- Fail-closed target and family handling

No application outcome may be used to change a v2.6 DGP, truth, target, gate,
threshold, or replication rule. Existing application results are historically
known, so a later application rerun should be described as a prospectively
frozen rerun of an already studied application, not as an outcome-blind
application study.

## Why one v2.5 replication is expensive

The v2.5 runtime stop does not show a software crash. It shows that none of the
12 complete end-to-end jobs had reached its final atomic artifact after
1,067.752 seconds. The evaluator did not save public stage timings, so that
record cannot identify one empirical bottleneck.

The code nevertheless explains the scale of the job. One replication runs a
full tuning problem and two outer-fold tuning problems. With two candidates,
two inner folds, a shared pooled prefit, a continued constant fit, a linear
fit, and two starts, these amount to approximately 48 to 54 optimizer-start
schedules. Each schedule is forced through 400 Adam epochs and an L-BFGS
polish. This is approximately 19,200 to 21,600 Adam epochs before counting the
three selected-state GH31 repolishes. The evaluator then runs six DML
calculations. These are the baseline, fixed-GH31, and repolished-GH31 branches,
each with an ordinary-target and heterogeneity calculation. One replication
artifact is written only after all these operations and the full gate engine
finish.

The principal v2.5 time burden was therefore its lane depth of 25 complete
replications, not the number of target labels or the act of comparing a gate
to its threshold. Weakening or deleting gates would buy little computational
time and would reduce credibility.

## Same-day stage

### Fixed job count and schedule

The same-day stage has eight replications in each of the six frozen cells. It
therefore has 48 attempted replications. It uses four waves with two
replications per cell and 12 jobs per wave.

The backend is the already tested 12-process base R PSOCK launcher. Every
worker uses one Torch thread. Each cell retains two deterministic lanes, and
each lane runs one job in each wave. Seeds are fixed by cell and replication,
not by completion order or worker timing.

Every job writes an atomic, hash-bound result or a hash-bound guarded-failure
record. Each wave writes a manifest after verifying all 12 assigned records.
An infrastructure interruption may resume only the same cell, replication,
seed, and evaluator bytes. A numerical or statistical failure is an outcome
and may not be silently rerun.

All 48 attempts are required. There is no statistical early success, early
failure, family dropping, target dropping, or threshold revision.

### Sealed first-wave checkpoint

The first 12-job wave is both the first fixed evidence prefix and the sole
runtime and integrity checkpoint. Its statistical contents remain sealed.
Before deciding whether to continue, the public record may expose only
assignment integrity, process identity, source and artifact hashes, worker
thread counts, completion status, elapsed wall time, and resource use. It may
not expose choices, fits, estimates, standard errors, gates, intervals,
release decisions, or coverage.

The chronologically first complete integrity-valid wave is binding. A valid
runtime wave requires 12 complete replication artifacts. An artifact remains
complete when an ordinary numerical or statistical gate records a fail. Those
gate fields stay sealed and do not affect the runtime decision. An evaluator
exception, truncated artifact, hash mismatch, or missing assignment is an
integrity failure. It stops the same-day continuation, is retained, and may
not be replaced as though the attempt never occurred. There is no search over
pilot waves or worker configurations. The runtime projection is

```
1.25 * 4 * complete first-wave wall time.
```

It must not exceed five hours. The corresponding first-wave cutoff is 3,600
seconds. If the wave fails integrity, the signed failure is retained and only
the documented infrastructure defect may be corrected under a newly
hash-bound implementation approval. If an integrity-valid wave exceeds 3,600
seconds, v2.6 stops without opening its statistical contents.

The first wave counts toward the fixed eight replications per cell because
the protocol and sample size were fixed before it ran and no statistical
content is used for the runtime decision. If reviewers prefer complete
separation, the same six-cell wave can instead be declared
evidence-ineligible before any outcome is generated. That conservative option
adds one wave and changes the five-hour cutoff to 2,880 seconds under the same
1.25 safety factor. The choice between these two designs must be frozen. It
cannot be made after the first wave.

### Runtime expectation

The v2.5 audit gives a lower bound rather than a completed-wave estimate. If
its 1,067.752 seconds is inserted into the new four-wave rule, the projection
is 1.483 hours. A completed first wave of 30 minutes projects to 2.5 hours. A
45-minute wave projects to 3.75 hours. A 60-minute wave reaches the five-hour
ceiling.

This schedule is consequently plausible under the evidence presently
available. The estimate includes the 1.25 safety factor but does not include
time needed to implement and audit v2.6 or to run either application.

## Same-day reporting and decision status

Once all four waves and manifests are complete, the statistical artifacts may
be opened exactly once and summarized mechanically. Every cell and every
registered family is reported. For each rate, the report gives its numerator,
denominator, point estimate, and a two-sided exact 95 percent binomial
interval. Targets within a replication are correlated and must not be pooled
or treated as independent Monte Carlo trials.

The only permissible overall status is one of the following.

- `preliminary_end_to_end_evidence_complete`
- `preliminary_end_to_end_evidence_incomplete`
- `stopped_on_integrity`
- `stopped_on_runtime`

There is no same-day status called `validated`, `calibrated`, `approved`, or
`coverage_passed`. The original per-replication gates still decide whether a
particular simulated or application family is released. Eight Monte Carlo
replications per cell do not establish the repeated-sampling performance of
those gates.

The same-day table may identify conspicuous problems in plain language. These
include pervasive noncompletion, no releases in a regular cell, obvious
undercoverage among the few releases, or repeated regular release at an exact
boundary. Such labels are diagnostics. They are not a substitute for the
confirmatory approval rules. A small count that looks favorable must be
described with equal caution.

Application results produced after this stage may be included in a working
draft as provisional results. A family that passes all of its application
gates may be described as passing the prespecified target-specific checks in
that application. It may not be described as having a simulation-validated
95 percent procedure. A candidate showcase remains a candidate pending the
confirmatory stage.

## Confirmatory extension

### Minimum endpoint of 50 replications per cell

Before the same-day results are opened, v2.6 should commit to continuing the
same evaluator and seed stream through replication 50 in every cell. This adds
42 replications per cell and 252 jobs. With two lanes per cell, it adds 21
12-job waves. Continuation is unconditional on whether the first eight results
look favorable.

At 50 replications per cell, the mechanical v2.5 rules can be applied without
translation.

- At least 48 of 50 replications must complete in each cell.
- A regular family-cell pair must release at least 38 times.
- Conditional simultaneous coverage must be at least 0.90.
- An exact-boundary pair may have no more than one invalid regular release
  under the compound point-rate and one-sided Clopper--Pearson rule.
- A weak pair succeeds by withholding with at most two releases. It can be
  informative with at least 15 releases if its conditional coverage rule also
  holds. Counts from 3 through 14 are inconclusive.

Only the completed 50-per-cell endpoint can create the mechanical family
approval record. If it fails, the failure is reported. Any methodological
repair then requires a new protocol and fresh confirmatory seeds.

### Preferred precision endpoint of 100 replications per cell

Fifty replications are a practical minimum, not high-precision coverage
evidence. If true coverage is 0.90, its Monte Carlo standard error is about
0.042 with 50 replications and 0.030 with 100. At true coverage 0.95, the
corresponding values are about 0.031 and 0.022.

A preferred publication check therefore continues through 100 replications
per cell. Starting after the same-day stage, this entails 92 additional
replications per cell and 552 jobs, or 46 more 12-job waves. The 50-replication
approval remains a prespecified interim endpoint. The 100-replication result
adds precision and must be reported whether it strengthens or weakens the
earlier conclusion.

Boundary evidence also improves materially. With zero invalid releases, the
one-sided exact 95 percent upper limit is 0.312 at eight replications, 0.058
at 50, and 0.030 at 100. At 100 replications, four invalid releases have an
upper limit of about 0.089 and five have an upper limit of about 0.102. This
is why the same-day stage cannot certify boundary behavior.

## Necessary safeguards and demands that can be relaxed

| Requirement | v2.6 treatment | Reason |
|---|---|---|
| Prospective targets, truths, seeds, gates, and hashes | Keep | Prevents result-dependent specification and coding changes |
| Whole-respondent resampling and respondent-level folds | Keep | Matches the dependence structure and inferential unit |
| Exact end-to-end estimator, tuning, and DML procedure | Keep | A surrogate or oracle calculation cannot validate the implemented estimator |
| Two starts and numerical, rank, Riesz, and regularity gates | Keep | These protect against precisely the failures seen in the applications |
| Regular, weak, and exact-boundary cells | Keep | Each evaluates a different promised release or withholding behavior |
| All target families | Keep | Fused DML makes their marginal cost modest, and the difficult heterogeneity families are central claims |
| Fifty replications per cell before any draft result | Relax | It is too costly for a same-day checkpoint and is not itself high-precision coverage evidence |
| Discarding a statistically sealed first wave | Relax prospectively | A fixed evidence prefix can also supply runtime information when no statistical content is inspected |
| Twenty-five jobs per deterministic lane | Relax | Four jobs per lane are enough for the preliminary stage |
| Withholding all application output until final Monte Carlo approval | Relax with explicit labels | Draft results can be useful if they are called provisional and all application gates remain binding |
| Gate removal or threshold weakening to increase releases | Do not relax | This would change the inferential claim rather than solve the runtime problem |
| Replacing validation with a respondent bootstrap | Do not use as a shortcut | A refit bootstrap would generally cost more and would not remove the need to validate boundary and weak-identification behavior |

## Implementation checklist before any v2.6 outcome

1. Create new v2.6 protocol, seed, state, run, and approval identifiers. Never
   reuse a v2.5 approval or artifact.
2. Copy the v2.5 design, truth, target, gate, and evaluator contracts and prove
   their byte or parsed-object equivalence in tests.
3. Implement the four-wave schedule and atomic per-job and per-wave manifests.
4. Decide prospectively whether the sealed first wave is evidence-eligible.
   The lean design above makes it eligible.
5. Ensure the first-wave public view cannot reveal statistical content.
6. Add tests for the 3,600-second rule, first-wave uniqueness, fixed 48-job
   count, resume behavior, guarded failures, and refusal to issue family
   approval after only eight replications per cell.
7. Hash-bind the complete implementation and record zero v2.6 outcomes before
   opening the first wave.
8. Freeze the Ballard--Rosa and Graham--Svolik application contracts before
   opening either new application result.
9. After four complete waves, generate a mechanical same-day report that uses
   only the preliminary status vocabulary above.
10. Continue the exact same replication stream to 50 per cell next week,
    irrespective of the preliminary findings. Prefer a later extension to 100
    per cell.

## Suggested draft language

The draft should say that the authors ran a prospectively specified,
design-matched end-to-end development study with eight replications in each of
six cells. It should report the observed release and simultaneous-coverage
counts with exact Monte Carlo uncertainty. It should state that this small run
is intended to expose gross implementation or gate failures, not to establish
nominal coverage. Application intervals should be called provisional until
the precommitted 50-per-cell confirmation is complete. The final paper should
replace this language and table with the confirmatory results rather than
silently treating the development run as definitive.
