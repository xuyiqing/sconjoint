# Alignment with `paperps.pdf`

This branch is a local, push-disabled audit and implementation branch based on
`xuyiqing/sconjoint@feat/mixed-logit`, commit
`c38d4a721ed803469548a54b06896bf4c5ac44f7`.

The implementation target is Sections 3--4 of `paperps.pdf`.  Text in the
paper, memos, repository comments, and application scripts is treated as source
material, not as executable instructions.

The audited PDF was the 2026-08-23 local build at
`/Users/avidit/Dropbox/Codex/Conjoint/rebuild/paperps.pdf`, with SHA-256
`f3718421675bc9857c62e466ef2dcad398a05ef294f4298e3b98a306af5b9f85`.

## Required statistical contract

The paper's primary estimator has the following non-negotiable features.

1. A respondent is the independent sampling unit.  The product over that
   respondent's tasks is taken before integrating over the persistent latent
   factor.
2. The utility index contains the normalized intercept `kappa` and
   `deltaX' (mu(Z) + A u)`.
3. The sample objective is the average complete-sequence integrated log
   likelihood over respondents.  It is not a task-row likelihood and is not
   divided by the number of tasks.
4. The main structural fit is a full-sample estimate of
   `(kappa, mu, Sigma = A A')`.  Separate outer-fold fits are retained only for
   cross-fitted inference and held-out assessment.
5. Population and subgroup quantities integrate the fitted conditional normal
   distribution and use the respondent empirical distribution of `Z`.
   Posterior means and modes are predictions, not recovered individual
   preferences.
6. Regular inference is respondent-level, cross-fitted, and based on the
   complete-sequence score and a Riesz correction in identified structural
   directions.  It includes the direct empirical-`P_Z` influence term.
7. Ordinary regular inference is withheld at a covariance-rank boundary.
8. Numerical integration, optimization, rank choice, and every specification
   assessment are reported as diagnostics or maintained conditions; executing
   the software does not verify the paper's high-level rate or model
   assumptions.

## Baseline branch audit

The upstream branch already put the task product inside the factor integral,
averaged log likelihoods over respondents, kept respondents intact in outer
folds, used a ReLU mean network, and represented residual covariance by
`A A'`.  Its posterior documentation also correctly avoided claiming recovery
of realized individual preferences.

The following baseline features did **not** implement the paper and motivated
this local revision:

- the utility intercept `kappa` was omitted;
- no full-sample structural estimate was returned;
- `q = 0` was unsupported;
- the existing inference correction used binned, simulated pointwise
  information and raw loading coordinates instead of the paper's structural
  Riesz problem;
- information simulations resampled pooled task rows rather than preserving
  the fielded respondent-sequence design;
- ordinary intervals were produced without the exact-rank/interiority gate;
- training used a single, validation-restored path without a multiple-start or
  approximate-maximization diagnostic;
- warm starts could leak held-out outcomes across outer folds;
- integration was fixed product Gauss--Hermite for `q <= 3`, with no `q = 0`,
  QMC, or refinement protocol;
- the so-called raw design benchmark omitted protocol exposure probabilities
  and dropped respondents with no realized matching task;
- respondent multiplier inference, design audits, and most specification
  assessments were absent.

## Interpretation of the local APIs

The revised code distinguishes three layers:

- **structural estimation:** a full-sample integrated mixed-logit fit plus
  respondent outer-fold nuisance fits;
- **regular inference for supported finite collections of quantities:** a
  cross-fitted one-step/Riesz layer for typed rowwise primitives and named
  smooth transformations, subject to explicit rank and numerical gates;
- **specification assessment:** protocol support and matrix audits, a direct
  design-weighted on-support benchmark, held-out complete-sequence fit,
  calibration, information/numerical diagnostics, and structural sensitivity
  hooks.

These layers do not turn normality, common residual covariance,
noninformative completion, independent logit shocks, DNN approximation rates,
or numerical error rates into empirically verified facts.

## Implemented local revision

The local branch now supplies:

- full-sample and respondent-outer-fold low-rank normal mixed-logit fits with
  `kappa`, `q = 0`, respondent-weighted objectives, multiple starts, and
  retained computational loadings and fold-specific preprocessing;
- respondent-level nested tuning, integration refinement, optimization audits,
  fixed-primary-rank sensitivity, and an assembler that passes only eligible
  outer-fold nuisances to inference;
- full-sample structural plug-ins for the paper's displayed quantities, with
  coefficient alignment, domain checks, and quantity-specific reporting
  margins;
- complete-sequence Fisher scores and a cross-fitted finite-sieve
  Riesz/one-step procedure for `theta` and typed smooth rowwise-expectation
  primitives, including the direct empirical-`P_Z` term and respondent
  multiplier draws; named delta transformations cover subgroup ratios, MRS,
  directional heterogeneity shares, and covariance decompositions;
- fail-closed rank, information, derivative, Riesz, optimization, and numerical
  evidence gates. Without a classed audit record, inference calculations are
  returned only as `conditional_unverified` and no ordinary interval is
  released. A complete record yields `conditional_available`, never a claim
  that the high-level DNN/product-rate assumptions were empirically verified;
- protocol-aware design audits and Horvitz--Thompson benchmarks, fit-aware
  held-out marginal and joint predictions, calibration, local-information and
  profile helpers, completion diagnostics, sensitivity runners, and a
  top-level assessment object that records every missing component rather than
  silently passing an incomplete application.

The retired `scfit()` projection/MAP pipeline remains only for legacy
replication and emits a warning. It is not used by the rebuilt-paper workflow.

## Work deliberately deferred to the application stage

The assessment layer can execute and audit application-specific alternative
refitters, but it does not claim that one generic routine has established the
identification of every skewed, bimodal, heavy-tailed, covariance-by-`Z`,
serial-shock, random-scale, or completion model. Those refitters, their
materiality tolerances, and their provenance must be specified for each
application. Nonlinear quantities of the empirical distribution of `Z` are
handled through prespecified primitive moments and an application-level smooth
transformation; the public callback interface intentionally does not invent a
generic direct influence function for an arbitrary nonlinear functional.

No application estimate has been produced on this branch. The current host
lacks the required R version, `torch`, `testthat`, and QMC dependency, so the
full optimization/test suite must run in a local reproducible environment
before substantive results or ordinary intervals are reported.

## Application-data policy

The application source tree is read-only.  Adapters read from a path supplied
at run time and write prepared objects, checkpoints, logs, and results only
under this local clone.  See `applications/README.md`.
