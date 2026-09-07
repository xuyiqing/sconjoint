# Version 2.6 rapid Ballard--Rosa and Graham--Svolik application plan

## Purpose and status

This document specifies a rapid application track that can produce auditable
Ballard--Rosa and Graham--Svolik results for a draft within one working day. It
also fixes the additional work needed before those results can be described as
the final production analysis.

The plan is prospective with respect to the version 2.6 reruns. It is not
outcome-blind in the broader historical sense. The authors have already seen
the version 2.1 application results, and those results informed the target
registries, the theory-guided Ballard--Rosa mean restriction, and the decision
to use the saved version 2.1 states as computational starting values. That
history must be disclosed. No version 2.6 estimate, interval, gate result, or
specification-assessment result may be inspected before this plan and the
corresponding executable contract are committed and hash-bound.

The rapid track does not weaken any inferential or numerical gate. It saves
time by continuing an already fitted version of the same likelihood instead of
repeating the full architecture search from random initializations, and by
deferring computationally expensive sensitivity fits that are not necessary
to produce the primary results. A fresh-start reproduction and the complete
sensitivity battery remain required during the following week.

## Why continuation is legitimate, and what it cannot establish

The loading-scale error was in downstream use of the stored loading. It did
not alter the likelihood optimized in version 2.1, the selected mean learner,
the fitted conditional means, the position effect, the stored loading, or the
fitted choice probabilities. The saved full and outer-fold network states can
therefore serve as initial values for the same structural likelihood under the
corrected implementation.

An initial value is not a final estimate. Every continued state must be
reconstructed in double precision, evaluated on its correct training sample,
and polished with the current full-batch optimizer. It becomes a version 2.6
state only if it passes the unchanged fit gates. A deterministic perturbed
start provides a local competing solution. If the resulting surface does not
meet the KKT, objective, loading-invariance, nesting, and near-optimum-stability
requirements, the fit is diagnostic and its regular intervals are withheld.

This procedure can establish that the historical solution lies in a stable,
well-optimized local basin under the corrected code. It cannot replace a
fresh-start search for other basins or a fresh comparison of every tuning
candidate. Those checks belong to the week-long extension.

## Immutable inputs and reuse rules

All source artifacts remain read-only. Version 2.6 writes to a new run
directory and records SHA-256 hashes for every input, source file, protocol
file, checkpoint, and exported result.

The following existing material may be reused.

| Material | Permitted version 2.6 role |
|---|---|
| Raw application files and prepared design objects | Input after exact hash and schema verification |
| Response-free templates, target truths, and design metadata from version 2.5 | Direct carry-forward with a new protocol identifier and checksum manifest |
| Version 2.5 target, contrast, group, family, gate, sensitivity, and assessment registries | Direct carry-forward where the version 2.6 contract does not expressly supersede a computational item |
| Version 2.1 selected full and outer-fold rank-one network states | Initial values only for the rapid continuation and repolish |
| Version 2.1 fold membership and within-fold selected architecture | Fixed rapid-track nuisance specification; historical selection must be disclosed |
| Existing design, completion, and design-based AMCE summaries | Reusable only when their source hashes and analysis populations are unchanged; otherwise recompute |
| Prior estimates, intervals, and gate summaries | Historical comparators only, after the version 2.6 run is sealed |

The principal supplied hashes are as follows.

| Application and artifact | SHA-256 |
|---|---|
| Ballard--Rosa prepared object | `32964d49653a2cba335077f3f6170e5eb0aa7a2bb1c789343b91588f6a9cb24e` |
| Ballard--Rosa nested fit | `91dc8b6da7174b5d4923fd1963424b8008252627a7336713ad0173be73abc194` |
| Ballard--Rosa assembled fit | `a3e2ea1430c3c9f5dbc3f974ef586f74d3edfe1933d226982172c34952a5a106` |
| Graham--Svolik prepared object | `debd4a077330fb47b1c073cbc8a17e701d1c3f497fb10e188951b26110516878` |
| Graham--Svolik nested fit | `af27477673ee95a55bb258ad3c48a3f32057e8227054b6b401f197441d5f8ff0` |
| Graham--Svolik assembled fit | `9036fb12fe2e79831824580e4cb709192e893e78def169ce056e1767f23d1a5a` |

The copies of the three Graham--Svolik artifacts in `inputs/gs2020` match the
supplied Yiqing artifacts byte for byte. The Ballard--Rosa artifacts remain in
Yiqing's shared read-only artifact directory and must be imported by hash into
the new run rather than modified in place.

The following items must not be reused as version 2.6 results.

- No old fit object may be relabeled as a version 2.6 fit.
- No old state may supply a final result without double-precision repolishing
  and the current fit gates.
- No old target correction, covariance-derived quantity, interval, or release
  decision may be copied into the new output.
- The old rank-zero and rank-two fits are historical comparisons. Even though
  rank zero has no loading-scale issue, it was fitted under the older
  precision and optimization pipeline.
- An old artifact may not fill a failed or missing version 2.6 checkpoint.

## Ballard--Rosa response-free repair

Before restoring a Ballard--Rosa state, rebuild `work_vs_luck` from the
hash-bound raw file. Codes 1, 2, and 3 are substantive. Codes 8 and 9 are
missing. Imputation and standardization are recomputed within the relevant
training sample. The source object is not changed.

Where the old and new moderator transformations differ, analytically remap the
first network layer so that the same raw-scale conditional-mean function is
used as the initializer. For an old transform `(z-c_old)/s_old` and a new
transform `(z-c_new)/s_new`, the new first-layer weight and bias must implement
the identical affine input map before the intentionally changed raw value is
applied. A unit test must compare predictions before and after this
reparameterization on rows whose raw inputs did not change. The repaired row is
allowed to change because its old numeric treatment was incorrect.

## Rapid rank-one fit contract

The primary model remains the flexible rank-one normal mixed logit. The rapid
track preserves the five historical respondent outer folds and the learner
selected inside each outer training sample. Ballard--Rosa has one four-unit
ReLU outer state and four eight-unit ReLU outer states, all with weight decay
0.1. Graham--Svolik has four four-unit ReLU outer states and one eight-unit
ReLU outer state, also with weight decay 0.1. The Ballard--Rosa full-sample
selected state is treated in the same way. The supplied Graham--Svolik bundle
has five outer states but no full-sample state, so its rapid primary report
uses cross-fitted plug-ins and one-step quantities. A new Graham--Svolik
full-sample fit is optional and cannot delay the cross-fitted results. These
architecture choices were
learned from the application outcomes in the earlier nested analysis and are
therefore labeled historically selected.

For each full or outer-fold state, the runner performs these operations.

1. Verify the application input, fold membership, training-row membership,
   architecture, coefficient order, moderator order, preprocessing record,
   and state hash.
2. Restore the portable state and reconstruct the same fitted probabilities
   under its recorded float implementation. Failure of this reproduction check
   stops that state.
3. Rebuild the network and all data tensors in float64. Map the stored state to
   the double-precision network without changing its raw-scale utility index.
4. Optimize the same training likelihood at 31 Gauss--Hermite nodes. Use an
   exact continued state and one deterministically perturbed competing state.
   Full-batch L-BFGS is the required final optimizer. A short Adam continuation
   may be used only under a prespecified rule triggered by a failed initial KKT
   check. It is followed by another full-batch L-BFGS polish.
5. Select the finite state with the best unpenalized respondent-sequence
   objective. Save both starts and their objective, blockwise scale-free KKT,
   bound-activity, polish, and loading-invariance records.
6. Accept the selected state only when all unchanged fit gates pass. A failed
   state is retained as a diagnostic checkpoint and cannot support regular
   inference.

The rapid runner must checkpoint each state atomically. Restarting reads and
rehashes a completed state rather than recomputing or overwriting it.

The primary integration rule is GH31 because the historical states were fitted
at GH31. Evaluate each final state at fixed-state GH45 without changing its
parameters. Recompute the target and influence views at GH45. If their movement
exceeds the existing integration tolerance, the affected family is withheld.
The rapid track does not repolish at GH45. That additional numerical check is
part of the week-long fresh-fit extension. The tolerance itself is never
changed to meet the deadline.

## Targets and inference

Run all registered headline quantities rather than selecting targets after the
fit. In particular, the rapid analysis retains the quantities that motivated
the reanalysis.

- Ballard--Rosa includes average rate and revenue preferences, the registered
  progressivity directions, plan-choice probabilities, directional preference
  shares, compensating-benefit shares, and directional heterogeneity and
  polarization.
- Graham--Svolik includes all prespecified preference directions, all overall
  and ideology-group compensating-benefit shares, and all directional
  heterogeneity decompositions.

Ordinary targets that share a fold, score, Gram matrix, and Riesz penalty use
one algebraically equivalent multiple-right-hand-side solve. Each scalar target
retains its own column, defect, null-space check, ridge-neighborhood movement,
influence contribution, and pass or fail decision. Heterogeneity primitives
remain a separate joint calculation. Unit tests must establish numerical and
gate equivalence to separate target calls.

The ridge rules, derivative checks, target-space constraints, active-eigenvalue
screen, null-space check, and blockwise optimization checks are unchanged.
Generate 9,999 common respondent multipliers for each prespecified substantive
family. A family is released only when the common fit gates and every member's
target gates pass. No failed member is dropped. Plug-ins remain available as
structural descriptions, but a failed family receives no regular confidence
interval.

Recompute the local-injectivity certificate at every final fitted state. Local
rank is necessary for the corresponding structural interpretation. It is not
evidence of global injectivity or finite-sample precision.

## Same-day specification assessment

The following checks are mandatory before the rapid results are delivered.

1. Input hashes, coefficient orientation, the Ballard--Rosa contrast repair,
   the `work_vs_luck` repair, sequence completion, and finite standardization.
2. Available protocol-support evidence and an explicit qualification when the
   original assignment instrument remains unavailable.
3. Held-out respondent-sequence score and marginal calibration for the
   continued rank-one fit.
4. Within-respondent response counts, adjacent-response transitions, and
   prespecified response patterns.
5. Display position and task-order checks.
6. Structural probability-scale AMEs compared with design-based AMCEs on
   common randomized support.
7. The final local-injectivity certificate, target-specific information,
   Riesz and ridge records, parameter-space checks, and simultaneous-family
   release record.
8. Completion and missingness facts for the analysis population. An
   unavailable sensitivity is reported as unavailable rather than passed.

A fresh rank-zero comparison is the first sensitivity after both primary
rank-one analyses and their inference are complete. It uses the current input,
folds, precision, likelihood, and optimizer. It is required for a same-day
claim that persistent residual heterogeneity improves prediction. It is not
replaced by the old rank-zero artifact. Under the amber runtime route it may be
deferred to protect the primary outputs. The draft must then state that the
comparison is pending and may not make the predictive-superiority claim.

The restricted rank-one mean is second in the optional queue. A fresh rank-two
fit is third and, for the rapid draft, may be limited to a clearly labeled
full-sample predictive and plug-in sensitivity. Outer-cross-fitted rank-two
inference is part of the week-long extension. No optional fit starts until the
two primary rank-one outputs and their gate records have been saved.

## Checkpointed time budget

All worker processes use one Torch, BLAS, and OpenMP thread. At most ten
workers run simultaneously, normally five per application, leaving two cores
for the operating system and orchestration. Seeds are functions of the
application, fold, state, start, and protocol ID, so completion order cannot
alter a result.

The target schedule is eight to ten hours. Sixteen hours is a hard delivery
limit.

| Time from approval | Required checkpoint | Decision |
|---:|---|---|
| 0:00--0:45 | Commit and hash the application contracts, input manifests, state-conversion code, runner, and focused tests | Do not unlock a version 2.6 application outcome until this passes |
| 0:45--1:30 | Restore and repolish Ballard--Rosa outer fold 2 and Graham--Svolik outer fold 3, the prespecified eight-unit timing states; inspect timing and integrity fields only | Apply the binding green, amber, or red projection below |
| 1:30--4:00 | Run the remaining Ballard--Rosa and Graham--Svolik full and outer states in two five-worker lanes | Atomic state checkpoints make the run restartable; no threshold changes are permitted |
| 4:00 | Primary-fit checkpoint | If a fit is incomplete, devote all workers to unfinished primary states. Do not start rank or mean sensitivities |
| 4:00--5:30 | Assemble both cross-fitted fits; run fused target DML, multipliers, fit and target gates, higher-resolution fixed-state checks, and injectivity certificates | A failed gate withholds the relevant fit or family; it does not trigger a new tolerance |
| 5:30--7:00 | Run the mandatory specification-assessment battery and export primary tables and figures | Missing checks are labeled unavailable and prevent a clean-showcase label when applicable |
| 7:00--8:30 | Run fresh rank-zero comparisons in parallel if the hour-4 checkpoint passed with adequate margin | Stop this branch if it threatens primary export |
| 8:30--10:00 | Build application summaries, update the running memo, and compile draft-ready outputs | Start no new optional fit after hour 10 |
| 10:00--14:00 | Reserved continuation window for interrupted primary checkpoints, export repair, or a prespecified numerical escalation | No new sensitivity family may enter this window |
| 14:00--16:00 | Hard computation stop; audit manifests, compile documents, and prepare coauthor handoff | Deliver both applications even if one is correctly labeled diagnostic or incomplete |

The timing checkpoint uses elapsed time, completion, worker, memory, and hash
fields only. It does not expose coefficients, targets, gate results, or other
substantive output. Runtime may determine whether a predeclared optional branch
runs. It cannot alter a model, target, threshold, or reporting rule.

Let `t_max` be the larger wall time of the two completed timing states. After
those states, nine supplied full or outer states remain and can occupy at most
nine workers in one wave. The conservative primary-fit projection is the
elapsed time at the checkpoint plus `1.5 * t_max`.

- Green means a projected primary-fit completion no later than hour 6. Continue
  the primary fits and retain the optional queue.
- Amber means a projection after hour 6 but no later than hour 12. Continue only
  the primary fits, inference, mandatory assessments, and exports. Do not start
  rank-zero, restricted-mean, or rank-two work.
- Red means a projection after hour 12, or either timing state failing its
  integrity reconstruction. Stop before dispatching the remaining states and
  report that the same-day continuation cannot meet the hard delivery rule.

The expected workload for the primary continuation is eleven supplied
selected states, six for Ballard--Rosa and five for Graham--Svolik, with two
starts per state and no repeated architecture search. A new Graham--Svolik
full-sample selected-architecture fit is optional. The old post-fit target
calculations took only minutes once the
assembled states were available. The rapid track should therefore fit in
roughly six to ten hours, including implementation, assessment, and document
production. This is an engineering estimate, not an assurance. The 90-minute
checkpoint supplies the binding projection.

## Reporting labels and stop rules

The draft must call these results the **version 2.6 rapid continuation
analysis**. It must not call them a fully fresh production rerun.

- If every fit and target gate passes and the mandatory assessment record is
  complete, the associated regular interval may be reported under the stated
  structural assumptions.
- If a common fit gate fails, no regular family from that fit is released.
- If a target or family gate fails, that family is withheld without changing
  the threshold or deleting the failed target.
- A well-defined plug-in can be shown as a model-based description even when
  its interval is withheld.
- Ballard--Rosa may be called a provisional showcase candidate if its core
  gates and assessments pass. It is not a clean showcase until the fresh-start
  and full-sensitivity extension confirms it.
- Graham--Svolik is described according to its version 2.6 evidence. Historical
  difficulty is not concealed, and a repeated failure is presented as a mixed
  application or stress test.
- Share-detection thresholds and diagnostic ranges are not confidence bounds.

## Week-long confirmation and expansion

Freeze the extension settings and seeds before inspecting the rapid version
2.6 results. The extension should then proceed from raw inputs rather than from
the rapid selected states.

1. Rerun the full flexible rank-one grid with fresh starts in every full,
   outer, and inner training sample. Use at least three starts for selected
   refits, float64 throughout, full-batch L-BFGS polishing, and the same
   blockwise gates. Compare the fresh states with the rapid continuation states
   in objective value, predictions, and every headline target.
2. Complete a fresh theory-guided restricted rank-one mean analysis on the
   common respondent folds.
3. Fit fresh rank-zero and rank-two models through the same nested pipeline.
   Compare held-out sequence scores, marginal calibration, response counts,
   transitions, and headline targets.
4. Complete the registered residual-shape fits, common-covariance sensitivity,
   lagged-choice and task-order specifications, position interactions, logit
   scale, completion and missingness analyses, and survey-weighted population
   sensitivity.
5. Recompute every target family, multiplier set, integration comparison, and
   injectivity certificate from the fresh states.
6. Expand the version 2.6 simulation stream to its predeclared replication
   counts and additional design cells. Report Monte Carlo uncertainty. Do not
   use the simulation to tune gates after the application results are known.
7. Replace rapid-draft values only through a hash-checked export comparison.
   Explain all material changes in the running method and code memo.

Until these steps are complete, the rapid analysis supplies reproducible,
deadline-appropriate draft evidence. It does not supply the final robustness
claim that the expanded protocol is designed to support.
