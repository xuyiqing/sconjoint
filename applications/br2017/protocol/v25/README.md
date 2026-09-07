# Ballard--Rosa fresh application protocol v2.5

**Protocol ID:** `br2017-paperps-application-2026-09-07-v2.5-v1`

**Status:** Drafted before the fresh v2.5 application rerun. No v2.5
Ballard--Rosa fit, target, gate, or assessment result existed when this
directory was written.

## Scope and provenance

This protocol fixes the Ballard--Rosa application choices that must not be
changed after the fresh rerun begins. It is prospective with respect to that
rerun, not outcome-blind in the broader historical sense. The authors and the
code audit had already examined the v2.1 Ballard--Rosa results. Those results
motivated the corrected scale contract, target-specific inference, stronger
optimization checks, the restricted-mean sensitivity, and the complete
specification-assessment battery. Any later revision must receive a new
protocol ID and say which fresh results had been inspected.

The authoritative structural-analysis input is the prepared object whose
SHA-256 is
`32964d49653a2cba335077f3f6170e5eb0aa7a2bb1c789343b91588f6a9cb24e`.
It contains the corrected `DeltaX_fixed` contrasts for 2,000 respondents and
16,000 tasks. The original profile-level data and corrected long object are
separate inputs for the design benchmark and missingness audit. Their roles
and hashes are in `input-manifest.csv`.

The regenerated v2.1 files in Yiqing's `artifacts/br2017` directory are audit
comparators only. They must never initialize an optimizer, select a model,
set a threshold, define a target, or fill a missing fresh artifact.

## Population and weighting

The primary population is the 2,000 respondents assigned to the
revenue-information version of the conjoint. Every respondent receives equal
weight and all eight completed tasks enter the respondent-sequence
likelihood. The analysis does not claim transport to respondents outside that
fielded arm. The supplied survey weights define a prespecified sensitivity,
not the primary estimand.

## Common folds and seeds

All four fitted specifications use the same five respondent outer folds and
the same two inner folds within each outer training sample. Fold assignment
is a deterministic function of respondent ID, the input hash, and the salts
in `seed-registry.csv`. It never uses a response. For each fold, respondents
are sorted by the full lowercase SHA-256 hex digest of

`salt || "\n" || input_sha256 || "\n" || UTF-8 respondent_id`

with respondent ID as a tie breaker, then assigned cyclically to folds
`1,...,K`. Inner folds use only the relevant outer-training respondents and
the outer-fold-specific salt. The eventual runner must save the resulting
assignment and its SHA-256 before fitting. Preprocessing that uses responses
is training-fold specific. The already frozen response-free scaling in the
prepared object may be reused exactly.

Optimizer and inferential seeds follow `seed-registry.csv`. The same logical
fit cell uses the same seed across specifications. Specifications may have
different parameter dimensions, so this is paired reproducibility rather
than identical initialization vectors.

## Models

The primary model is the flexible rank-one normal mixed logit. Its conditional
mean candidate set contains the unpenalized constant, linear deviations, and
four- and eight-unit ReLU deviations at three fixed penalties. Every deviation
is centered over the training respondents. The constant is never penalized.

The restricted rank-one sensitivity is linear. It retains an unpenalized
constant for every utility coefficient and permits only the 35 slopes in
`restricted-mean-mask.csv`. The selected moderators represent material
self-interest, partisan alignment, redistributive values, deservingness
beliefs, and perceived efficiency costs. Each of these five moderators can
affect all seven utility coefficients. Unlisted input-output links are exactly
zero. This model is a sensitivity and cannot replace the flexible primary
model based on the fresh results.

The raw `work_vs_luck` item has substantive codes 1, 2, and 3, plus skipped
codes 8 and 9. The earlier preparation treated the single revenue-arm code 8
as numeric before standardizing it. The v2.5 preflight must reconstruct this
column from the hash-bound raw file, set 8 and 9 to missing, impute the
training-fold median, and standardize over the training respondents for the
flexible model. The restricted model instead uses the published-faithful
binary indicator `raw == 1` versus `raw in {2,3}`. This response-free repair
must be written to a new derived-input artifact. It does not modify the
authoritative source object.

Fresh rank-zero and rank-two sensitivities use the full flexible mean grid,
the common folds, and the same numerical pipeline. Old q-sweep fits are not
comparable. Formal release is assessed for the fixed primary q=1 model.
Sensitivity estimates remain labeled by their maintained specification.

All fits use float64 on CPU. GH15 is the production quadrature for positive
rank. Every selected full and outer fit uses three starts, full-batch Adam,
and an accepted full-batch L-BFGS polish. A fixed-state GH31 evaluation and a
zero-Adam GH31 L-BFGS repolish are required approximation branches. Fit-level
KKT, nesting, and loading checks refer to the selected GH15 state. The GH31
repolish has its own exact-state and optimization checks.

## Targets and inference

`contrast-registry.csv`, `group-registry.csv`, and `target-registry.csv`
jointly define every reportable target. The rank slope is the primary
progressivity direction. Top-minus-bottom is a transparent companion. The
log-midpoint slope is an appendix sensitivity because the open-ended top
bracket requires a fixed $500,000 midpoint. The three policy comparisons use
the schedules recorded in the contrast registry.

The outcome-informed compensating-benefit targets decompose each progressive-plan comparison
into its rate-schedule component and its stated revenue-score benefit. Thus
the amounts are 2, 2, and 4 revenue-score units against the flat,
status-quo, and regressive schedules. Exact ties are excluded from ordinary
preference-direction shares and included for compensating-benefit shares, as
declared in the paper.

Each scalar target, or the smallest joint primitive vector needed for a smooth
transformation, receives its own cross-fitted Riesz calculation. The primary
ridge is `Ntrain^(-3/4)` and the fixed sensitivity rates are `Ntrain^(-1)` and
`Ntrain^(-1/2)`. All members of a multiplicity family use the same 9,999
respondent multipliers. A family releases only as a complete simultaneous
family. A failed target withholds that family but does not contaminate an
unrelated family. The directional variance supporting a share is mapped to
that share and can withhold only its directional-share family.

Every final specification receives a fresh local-injectivity certificate.
Passing local rank is necessary for the corresponding structural claim. It
does not establish global injectivity, optimization success, or precise
finite-sample inference.

## Specification assessment

`assessment-registry.csv` freezes the complete application battery and its
materiality thresholds. It covers protocol support, structural AME versus
design AMCE, held-out sequence scores, marginal and joint calibration,
response counts and transitions, position and task order, local information,
rank, residual shape, common covariance, state dependence, logit scale,
completion, missingness, and weighting. A check that cannot be run is
reported as unavailable and never counted as a pass.

The assessment thresholds are reporting rules, not tests that prove the
structural model. Central target families may be reported as a clean
heterogeneity showcase only if the applicable v2.5 calibration family is
approved, the primary fit and target gates pass, the family releases, the
core design and predictive checks pass, and the conclusion is stable under
the prespecified sensitivities. Otherwise the application is labeled mixed or
a stress test. Plug-ins may still be shown as structural descriptions when
well defined, but diagnostic ranges are not confidence intervals.

## Required preflight resolution

The current repository protocol summarizes the randomization but does not
contain the original fielding instrument or generator that proves independent
assignment across brackets and rules out rejection of implausible plans. The
raw data establish realized exposure, not the protocol probabilities required
by the identification theorem. Before calling Ballard--Rosa a clean showcase,
the authors must either supply and hash the assignment instrument or retain an
explicit identification qualification. This does not prevent a diagnostic
rerun.

## Files

- `application-contract.dcf` fixes the core analysis contract.
- `input-manifest.csv` records required inputs and historical comparators.
- `model-specifications.csv` fixes fitted specifications and tuning grids.
- `restricted-mean-mask.csv` fixes the restricted linear mean.
- `seed-registry.csv` fixes folds and random-number streams.
- `group-registry.csv` fixes subgroup definitions.
- `contrast-registry.csv` fixes all utility contrasts.
- `target-registry.csv` fixes targets, primitive bundles, and families.
- `family-registry.csv` fixes family order, simultaneous coverage, and release.
- `inference-gate-registry.csv` copies the application inference gates.
- `sensitivity-registry.csv` fixes structural sensitivities.
- `assessment-registry.csv` fixes assessment summaries and tolerances.
- `review-decisions.md` identifies substantive judgments for author review.
- `test-protocol.R` checks internal consistency and `CHECKSUMS.sha256`.

No application runner is part of this directory. Implementation begins only
after author review and after the v2.5 calibration procedure is approved.
