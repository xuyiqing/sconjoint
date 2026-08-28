# Saha--Weeks application reanalysis

This directory contains the local, reproducible reanalysis of the Saha--Weeks
SSI conjoint for the model in `paperps.tex`. The replication project named by
`SCONJOINT_APPLICATION_ROOT` is read-only. All prepared data, fits,
checkpoints, assessments, and manuscript-handoff artifacts are written below
this directory.

The primary estimand is the equal-weight distribution of the 1,191 SSI
respondents who completed all three tasks and have valid pre-conjoint primary
demographics. The analysis has 3,573 task observations, a 13-coordinate
profile basis, and 15 primary pre-conjoint moderators. The four post-conjoint
survey variables in the 19-moderator legacy object are excluded from the
primary specification and may enter only a disclosed sensitivity.

## Run order

Run commands from the package root with the project-local R 4.5 launcher:

```sh
applications/bin/Rscript45 applications/sw2022/R/00_verify_environment.R
applications/bin/Rscript45 applications/sw2022/R/01_prepare_data.R
applications/bin/Rscript45 applications/sw2022/R/02_design_completion_audit.R
applications/bin/Rscript45 applications/sw2022/R/03_fit_models.R --profile=pilot --stage=all
applications/bin/Rscript45 applications/sw2022/R/03b_rank_assessment.R --profile=pilot --stage=all
applications/bin/Rscript45 applications/sw2022/R/04_inference_qoi.R --profile=pilot --stage=all
applications/bin/Rscript45 applications/sw2022/R/05_assessment.R --profile=pilot
applications/bin/Rscript45 applications/sw2022/R/06_export_bundle.R --profile=pilot
applications/bin/Rscript45 applications/sw2022/tests/smoke_assessment_extensions.R --profile=pilot
applications/bin/Rscript45 applications/sw2022/tests/smoke_sensitivity_stamp.R
```

Use `--profile=production` for the manuscript run. Pilot outputs are for
pipeline validation and may not be quoted as application results. The fit
script checkpoints existing artifacts unless explicitly forced; see its
header and `config/analysis_config.R` for the profile-specific controls.

The stages have distinct roles:

1. `00_verify_environment.R` checks the native project-local R, dependency,
   package-build, and CPU Torch environment before an expensive fit starts.
2. `01_prepare_data.R` reconstructs candidate-A choices and
   `DeltaX = X(A) - X(B)` from raw replication data, verifies equivalence to
   the frozen legacy matrices, and leaves moderator scaling to training folds.
3. `02_design_completion_audit.R` records sample flow, realized support,
   theoretical rank witnesses, repeated contrasts, and the 1,249-respondent
   completion universe. It also freezes a local paired-task audit for
   completion/exclusion comparisons. It does not call the fielding protocol
   verified.
4. `03_fit_models.R` estimates the fixed-primary `q=1` normal low-rank mixed
   logit and the prespecified `q=0` and `q=2` sensitivities. Respondents remain
   intact in every fold and tuning uses complete-sequence log scores.
5. `04_inference_qoi.R` constructs full-sample structural plug-ins and
   respondent-level diagnostic/regular-inference artifacts. Posterior modes or
   posterior means are never used as recovered individual preferences.
   Before that step, `03b_rank_assessment.R` reruns q=0 and q=2 on the
   primary q=1 outer folds, constructs paired respondent-sequence score
   comparisons, and checks q=2 product-GH resolution and loading orientation.
6. `05_assessment.R` builds held-out complete-sequence scores, marginal and
   joint calibration (including all eight three-task response patterns and the
   prespecified task pairs 1--2, 2--3, and 1--3), respondent-clustered
   AMCE-style checks, information and optimization summaries,
   completion/order/serial diagnostics, sensitivity status, and fail-closed
   reporting ledgers. The sole exact repeated contrast stays a separately
   labeled sparse diagnostic.
7. `06_export_bundle.R` freezes the tables, figures, manifests, and
   machine-readable evidence needed to draft Section 5.1 later. It does not
   draft the section.

After every expensive fit process has exited, reinstall and validate the exact
current package source with:

```sh
applications/sw2022/bin/postfit_validate.sh
```

The driver refuses to run while `03_fit_models.R` is active. It refreshes the
pinned environment record and writes critical R/Torch/launcher SHA-256 hashes,
the complete project-library package manifest, R parse and source-text checks,
and machine-readable results for the focused paperps and CPU-Torch tests below
`manifests/postfit_validation/`. This focused validation is not represented as
a clean run of every legacy test in the package.

On restricted hosts where the operating-system process service is unavailable,
the driver fails closed. After the fit session itself reports completion, use
`applications/sw2022/bin/postfit_validate.sh --fit-confirmed-stopped` to record
that explicit confirmation and proceed.

## Output layout

- `results/prep_analysis_data.rds`: local prepared analysis matrices.
- `results/design_completion_audit.rds`: design and raw-universe completion
  audit.
- `results/completion_task_audit.rds`: local paired outcomes/contrasts and
  completion flags for the 1,249-respondent raw universe.
- `results/mixed_logit/<profile>/`: fits, QOIs, inference, and computation
  checkpoints.
- `results/rank_assessment/<profile>/`: common-outer-fold rank comparisons and
  q=2 integration/orientation checks.
- `results/assessment/<profile>/`: application assessment objects and tables.
- `results/section5_1_bundle/<profile>/`: future Section 5.1 handoff.
- `tables/`: preparation and design/completion audit tables.
- `manifests/`: source, artifact, and environment provenance.

Generated results and the project-local R/package libraries are ignored by
git where appropriate; analysis code, configuration, and provenance manifests
remain reviewable.

## Interpretation rules

Assessment status is deliberately fail-closed. `not_run` means no qualifying
refit or diagnostic artifact was found. `maintained_assumption` means the
primary model imposes the condition; it is not evidence that the condition is
true. `protocol_unavailable` means exact fielding probabilities or rules were
not recovered and must never be relabeled as a passed design check.

In particular:

- the article describes five profile attributes as independently randomized
  without combination restrictions, but the fielded QSF/randomizer,
  assignment probabilities, display-order metadata, and cross-task rules are
  unavailable;
- the 91-contrast algebraic witness gives the required affine and symmetric
  quadratic ranks conditional on advertised full support, but realized cells
  alone do not verify the repeated-support condition;
- exact ordered-contrast Horvitz--Thompson benchmarks are therefore withheld;
  the respondent-clustered LPM tables are marginal AMCE-style estimands and
  are not structural mixed-logit preference coefficients;
- conditional-randomization tests of early assignment versus completion are
  also withheld because the fielded randomizer, restrictions, and exact
  completion-conditioned exposure probabilities were not recovered; no
  empirical or illustrative-uniform permutation law is substituted;
- the primary residual distribution is normal with a common covariance and a
  fixed logit scale. Skewed, bimodal, heavy-tailed, covariance-by-party, and
  random-scale refits remain sensitivities until actually run;
- held-out full-sequence, task-pair, task-order, adjacent-pattern, and residual
  checks can reveal lack of fit but cannot establish independent shocks,
  absence of fatigue/learning, or noninformative completion;
- the final network grid was adapted after a same-sample computational pilot;
  subsequent folds keep respondents out of fitting and tuning under that grid,
  but their predictive summaries are diagnostic rather than a clean
  end-to-end held-out evaluation of a fully outcome-blind workflow;
- conventional regular inference requires the exact-rank/interiority,
  identified-direction information, optimization, numerical-refinement, and
  quantity-specific gates. Missing gates withhold intervals and majority
  language;
- the complete, moderate, and very-few contest quantities are position-neutral
  and realizable under the advertised unrestricted profile support, but that
  support is not document-certified from the fielded protocol. Any use beyond
  verified support must be labeled structural extrapolation;
- the targeted Male-by-prior-run interaction sensitivity is needed before the
  updated application can make the original paper's gender-by-ambition claim;
  its absence remains visible in the assessment ledger;
- no individual MAP ridgelines, individual sign counts, or posterior-mode
  preference clusters should be recreated.

The exported `README_FOR_SECTION5_1.md`, inventory, claims ledger, and
quantity-gate table are the authoritative starting point for the eventual
manuscript write-up.
