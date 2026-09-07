# Inference and application protocol v2.6 prototype

**Status:** Draft for author review. It is not frozen or approved and it
authorizes no outcome generation.

**Proposed protocol ID:**
`paperps-draft-development-2026-09-07-v2.6-v1`

## The central change

Versions 2.4 and 2.5 made a complete Monte Carlo validation study a condition
for starting an application. That dependency was stronger than the evidence
could support. Even a successful 50-replication-per-cell study would have
evaluated a reduced fitting problem on finite empirical templates. It would
not have certified Ballard--Rosa or Graham--Svolik. At the same time, its
all-or-nothing lock prevented the application-specific fit, information, and
specification checks that actually determine whether an empirical target can
be reported.

The v2.6 prototype separates three questions.

1. **Software integrity.** Does the exact implementation reproduce its
   algebra, honor the loading-scale contract, use respondent-level folds,
   isolate target failures, and create complete auditable artifacts?
2. **Operating characteristics.** In repeated design-matched samples, how
   often does the fixed procedure release, cover, or correctly withhold a
   target? The same-day run gives preliminary evidence only. A precommitted
   extension supplies the sample size needed for a serious assessment.
3. **Application eligibility.** Does a particular fitted specification pass
   its own optimization, integration, local-rank, Riesz, ridge, domain, and
   specification checks? This decision remains fail-closed and does not
   depend on calling a small Monte Carlo exercise a certification.

No estimand, influence function, target family, target constraint, Riesz or
ridge comparator, multiplier construction, or application gate is weakened
in this prototype.

## Same-day development evidence

The proposed same-day study carries forward all six v2.5 response-free
templates and DGP cells, the 78 truths, all registered targets, and all 14
per-replication gates. It uses the exact v2.5 end-to-end evaluator. Only the
execution schedule, replication count, seed namespace, and interpretation of
the small study change.

- Run eight replications per cell, 48 in total.
- Use twelve one-thread PSOCK workers and two fixed lanes per cell.
- Run four waves. Each wave contains two replications from every cell.
- Wave 1 is both the sealed integrity/runtime pilot and the first evidence
  prefix. It is not discarded.
- Before the route is fixed, inspect only hashes, completion, process
  integrity, resource use, and wall time. Keep every statistical field sealed.
- The projected wall time is `1.25 * 4 * complete wave-1 wall time` and must
  not exceed five hours. The corresponding wave-1 cutoff is 3,600 seconds.
- If wave 1 has an infrastructure or integrity failure, stop. A correction
  needs a new implementation approval before another attempt. If the runtime
  rule fails, do not run waves 2--4.
- If wave 1 passes, run the remaining three waves without inspecting
  intermediate statistical output. Checkpoint and hash every replication and
  wave manifest.
- After all 48 attempts, report completion, release, simultaneous-coverage,
  false-release, and withholding counts with exact binomial intervals. Do not
  issue a family approval or claim nominal coverage from eight replications.
  No family or target may be dropped after opening the results.

The same-day output is called **provisional end-to-end design-matched
evidence**. Its purpose is to find gross implementation failures and show that
the complete estimator can run across regular, boundary, and weak-information
settings. It cannot establish a 95 percent coverage claim.

## Application results for the draft

Application work may begin after the v2.6 code, contracts, seeds, input hashes,
and integrity tests are approved. It is not locked behind the preliminary
Monte Carlo rates. Ballard--Rosa and Graham--Svolik still have to pass their
own registered checks target by target.

The same-day application path is a continuation-and-repolish analysis. The
saved v2.1 rank-one full and outer-fold states may be used only as hash-bound
initial values. This is justified because the loading-scale bug was
downstream. It did not change the respondent-sequence likelihood, selected
mean learner, conditional means, position parameter, stored raw loading,
folds, or fitted probabilities. New v2.6 states must be rebuilt in float64,
evaluated on the exact training sample, and polished with full-batch L-BFGS.
They are accepted only if the objective, scale-free blockwise KKT, loading
invariance, nesting, integration, and near-optimum checks pass. Historical
selection and the use of saved starting values must be disclosed.

Ballard--Rosa first reconstructs the `work_vs_luck` moderator from the raw
input, treats codes 8 and 9 as missing, and applies training-fold imputation
and scaling. Its old state is therefore only an initializer on the repaired
input. Graham--Svolik uses the hash-compatible prepared input. Neither
application may copy an old target estimate, standard error, interval, gate
status, figure, or table into the v2.6 results.

For both applications the same-day priorities are:

1. the primary fixed rank-one fit and every prespecified headline target
   through the fused, algebraically equivalent target-specific DML path
2. 9,999 common respondent multipliers within each complete family
3. fixed-state GH45 evaluation from the primary GH31 states, loading and
   optimization checks, local injectivity at every final state, and the core
   Section 4 assessment battery
4. a fresh rank-zero predictive benchmark
5. a theory-guided restricted rank-one mean if it is completed under its
   predeclared mask and time checkpoint

Fresh rank-two outer-crossfit inference, the complete flexible tuning grid,
all refitted residual-shape and dependence sensitivities, and publication-level
simulation evidence are part of the confirmatory extension. They are not
silently replaced by historical artifacts.

The draft uses four reporting states. A target can be `draft_eligible`,
`conditional_on_preserved_selection`, `descriptive_only`, or `withheld`.
Passing local rank does not override a failed fit or target gate. A
range-respecting interval does not turn a nonregular one-step calculation into
regular inference. A failed target cannot be removed from a simultaneous
family.

## Time budget and checkpoints

The target is ten hours from final approval and the hard ceiling is sixteen.
The detailed sequence is in `application-plan.md` and
`same-day-timeline.csv`.

- Hours 0--1 finalize and hash-approve the evaluator and both application
  contracts.
- Hours 1--4 run the four-wave development study if its sealed first wave
  projects no more than five hours. The application preflights can run during
  non-compute-bound portions, but heavy fit jobs must not compete with the
  twelve-worker wave.
- Hours 4--8 continue and repolish both primary rank-one fits, run fresh
  rank-zero benchmarks, compute all headline targets and inference, and apply
  the numerical gates.
- Hours 8--10 run the core assessment battery and export tables, figures, and
  a concise coauthor results note.
- Hours 10--14 are contingency time for restartable fit checkpoints. No
  optional analysis may be started after hour 10.
- Hours 14--16 are reserved for validation, compilation, and delivery. The
  results already available at hour 14 are frozen for the same-day bundle.

If an application fit is not complete by its checkpoint, the bundle reports
the last complete status and does not relax a tolerance. The other application
continues. This prevents one hard fit from consuming the delivery window.

## Confirmatory extension over the following week

The extension is fixed before the same-day statistical output is opened. If
the implementation is unchanged, the first eight replications per cell count
toward a mandatory total of 50 per cell. All 300 attempts are then run and the
v2.5 integer decision rules are applied. A preferred publication endpoint is
100 per cell because 50 replications still give a Monte Carlo standard error
of about 0.031 at 95 percent coverage and about 0.042 at 90 percent coverage.
At 100 replications the corresponding values are about 0.022 and 0.030.

The application extension reruns the complete candidate grid from fresh
starts, compares those fits with the continuation states, completes rank-two
outer-crossfit inference, runs the restricted mean for both applications, and
executes every prespecified residual-shape, covariance, state-dependence,
position, task-order, completion, missingness, and weighting sensitivity. It
also resolves the Ballard--Rosa fielding-instrument qualification if the
original assignment materials can be obtained.

If any procedure, threshold, target, DGP, or code path changes after opening
the same-day results, the affected extension must receive a new protocol ID
and fresh seeds. The preliminary results then remain an explicitly separate
development record.

## What requires author approval

Before implementation, the authors should approve four points.

1. The same-day 48-replication exercise is preliminary and cannot be cited as
   nominal-coverage validation.
2. The application reruns condition on the historically selected mean
   architecture and use saved states only as initial values. A fresh-start
   comparison follows in the confirmatory extension.
3. Rank two and the exhaustive refitted sensitivity battery are deferred, not
   treated as passed.
4. Ballard--Rosa can be called a provisional showcase only if its own central
   fit, target, family, and core assessment checks pass. Graham--Svolik can be
   reported as a stress test if those checks withhold heterogeneity inference.

After approval, this proposal should be copied to immutable `inst/protocol`
files, assigned final seeds and paths, hash-bound to the implementation, and
committed before any v2.6 synthetic or application outcome is generated.
