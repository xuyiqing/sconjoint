# sconjoint 0.2.0.9005 (2026-06-13)

Paper catch-up (2026-06-12 revision):

* `sc_average(scale = "logit", subgroup = ...)` now returns the
  influence-function subgroup estimate (subgroup mean of the task-level
  orthogonal-score contributions, respondent-clustered SE) instead of
  silently returning the full-population `theta`. This is the construction
  behind the party-level estimates in the paper's candidate application.
* `sc_counterfactual(vartype = "orthogonal")` warns when the one-step
  estimate falls outside [0, 1]: the linearized correction is unreliable for
  extreme contrasts (observed on lopsided tax-plan bundles), and the plugin
  aggregate should be compared.

# sconjoint 0.2.0.9004 (development version)

## Bundled `br2017` data corrected

The Ballard-Rosa, Martin and Scheve (2017) source file stores the
45% level of the $175-375k bracket as the number 5 in its derived
rate column (the underlying coded variable and its value label are
correct). The bundled `br2017` fixture inherited this, affecting
19.9% of profile rows. `data-raw/build_br2017.R` now rebuilds all six
bracket rates from the coded variables and their value labels, and the
bundled data has been regenerated. Analyses involving the $175-375k
bracket (and, through correlation, the revenue coefficient) change
under the corrected data.

## Importance shares: reproduce the paper's reported numbers

`sc_importance()` gains `design = "levels"` plus a `levels` argument:
attributes listed in `levels` contribute `beta^2 * Var(level set)`
(the production pipeline's convention for continuous attributes), all
others use the `"uniform"` within-attribute variance over levels. The
documentation now states plainly which option reproduces which set of
published numbers: the paper's *reported* importance shares come from
`"uniform"` (factor designs: Saha-Weeks, Graham-Svolik) and `"levels"`
(continuous designs: Ballard-Rosa), while `"design_variance"` remains
the formula displayed in the paper's text. The two do not agree in
general.

# sconjoint 0.2.0.9003

## Debiased inference for the additional quantities of interest

The orthogonal-score debiasing now extends beyond the average parameter
`theta` to the additional quantities of the paper's Appendix C, through
the per-row score `psi^H = H(f_hat) + grad_f H' u`, where `u` is the
universal correction already formed by `scfit()` and stored on the fit.
With `H = f_k` this reproduces the average-parameter estimate and its
clustered standard error exactly.

* `sc_counterfactual()` gains `vartype = c("orthogonal", "plugin")` with
  `"orthogonal"` as the default: the counterfactual choice probability
  now carries a respondent-clustered standard error that propagates
  first-stage uncertainty. The v0.1 plug-in, which did not, remains
  available via `vartype = "plugin"`.
* New `sc_ame(object, attr)`: the debiased structural average marginal
  effect on the probability scale. `scfit()` now stores a single-profile
  pool (`object$profile_pool`) used for the design-law integral.
* `sc_mrs()` / `sc_wtp()` gain `estimand = "population"`: the debiased
  population ratio `theta_j / theta_k` (delta-method plus Fieller
  interval), distinct from the default per-respondent trimmed ratio.
* `sc_importance()` gains `vartype = "orthogonal"`: the debiased
  Appendix-C shares (a ratio of orthogonal-score numerators with a
  clustered simplex-Jacobian standard error) for
  `design = "design_variance"`. This is opt-in -- the always-in-`[0, 1]`
  plug-in shares stay the default, because the debiased shares are a
  ratio and can fall outside `[0, 1]` when the first stage is noisy.


# sconjoint 0.2.0.9002 (development version)

## Pluggable first-stage learners

- `scfit(learner = ...)` adds two alternatives to the default deep
  network (`"dnn"`): `"enet"`, a cross-fitted elastic-net logit with an
  automatically spline-expanded moderator basis (needs `glmnet`), and
  `"grf"`, a generalized-random-forest local logit (needs `grf`).  All
  three feed the identical DML inference; only the first stage changes.
  `stage2` is forced to `"none"` for the alternative learners.
- New `scfit()` arguments `enet_alpha`, `enet_df`, `enet_interactions`
  control the elastic-net mixing parameter and the spline basis expansion
  (`enet_df = 1` with `enet_interactions = FALSE` recovers a
  linear-in-moderators first stage).
- `glmnet` and `grf` added to Suggests.  New tutorial section
  (Simulated Example) compares learner recovery against known truth.


# sconjoint 0.2.0.9001 (development version)

Paper v13 alignment.  Three changes to bring `scfit()` defaults in
line with the May v13 production estimator that generated every
current paper number (memo 42, `ConjointStructural/output/memos/42_sconjoint_upgrades/`).

## Plotting

- Ridgeline plots (`plot(fit, "beta_ridgelines")` and
  `plot_importance()`) now use `ggridges` `scale = 1.0` (was `2.0`
  and `1.8` respectively).  At the old scale each density spanned up
  to two row-heights and overlapped its neighbours; `1.0` makes the
  per-respondent distributions legible, especially on many-level
  designs.  Pair with a taller `fig.height` for dense panels.

## Continuous-attribute MAP prior

- New `scfit()` argument **`varref_floor`** (default `1e-3`,
  ignored unless `stage2 = "varref"`).  Replaces the hardcoded
  `0.01` floor on the diagonal prior variance.  The `0.01` default
  over-shrank continuous-attribute coefficients --- the BR top-bracket
  validation `r` against self-reported ideal rates collapsed from the
  paper's `0.39` to `0.13`.  At `1e-3` (production setting in
  `code/60_setup_ballard_rosa.R`) the natural varref value passes
  through unclipped on continuous designs; factor-dummy designs are
  unaffected (the floor is rarely binding there).

## Training config: NT-adaptive L2 + paper-faithful defaults

- Renamed `scfit(lambda = ...)` --> **`scfit(weight_decay = ...)`**.
  Matches the production code (`code/04_training.R`) and torch's
  optimizer convention.  Also distinguishes the DNN L2 coefficient
  from the unrelated `ridge_lambda` used in Lambda(Z) estimation.
- Default `weight_decay = "adaptive"` (new sentinel).  Resolves
  per-fit to the v13 NT-adaptive rule `K_adaptive / NT` where
  `K_adaptive = 15` if `NT/p < 300` else `25`.  Numeric values
  (e.g. `1e-4`) still pass through unchanged.  The resolved value
  is exposed on the returned `sc_fit` as `weight_decay_used` for
  diagnostics.
- Default `n_epochs = 1000L` (was `2000L`).  Matches paper v13.
- **Behavioral**: L2 regularization now applied via
  `optim_adam(..., weight_decay = X)` instead of adding
  `X * sum(p^2)` to the loss.  For Adam these are not equivalent
  (loss-side L2 is rescaled by Adam's adaptive per-parameter rates
  whereas `weight_decay` is applied to the parameter update
  directly).  The production code uses the optimizer channel; the
  package now matches it bit-for-bit.

## Auto-hidden architecture

- `.sc_auto_hidden()` (internal; used when `hidden = "auto"`) now
  returns the paper v13 base `c(32L, 32L, 16L)` for any `NT >= 2000`
  instead of scaling up to `c(64L, 64L, 32L)` at `NT >= 10000`.
  This is the architecture the paper uses for all three showcase
  apps (SW NT=3573, GS NT=20657, BR NT=16000).  A new
  large-design override returns `c(128L, 64L, 64L)` only when
  `p >= 40 AND NT >= 80000` (no showcase app triggers it; logic
  included for safety).

## Importance formula (paper-consistency fix)

- `sc_importance(..., design = "design_variance")` is the new default.
  This implements the paper's reported formula
  \eqn{\mathrm{Imp}_{i,g} = \sum_{k \in g} \hat\beta_{ik}^2 \cdot
  \mathrm{Var}(\Delta X_k)}, where `Var(ΔX_k)` is the empirical
  variance of dummy column k.  Reproduces the paper's sw2022 agenda
  share (~0.65 in the paper; ~0.62 in the package on the same data
  with K=5 / n_epochs=200).
- The previous `"uniform"` and `"empirical"` branches remain
  available but implement different functionals (variance of beta
  over a level distribution, not sum of beta^2 weighted by per-dummy
  Var(ΔX)).  Neither reproduces the paper's reported numbers;
  `?sc_importance` explains the relationship.

## Documentation

- `?sc_design_diagnostic` now reports build-time validation results
  against a controlled 18-cell sim grid (Pearson rho 0.76 with truth,
  mean |bias| 0.23, bias concentrated at low true R^2_Z).  The print
  banner makes the failure mode explicit: do not treat tier hints as
  a pass/fail gate.  `experimental = TRUE` remains the default.

# sconjoint 0.2.0

This release brings `sconjoint` up to the algorithmic defaults in the
2026-04 paper revision of Acharya, Hainmueller, and Xu (2026), plus a
prior-calibration fix and a new design diagnostic.

## Major default-behavior change

- `scfit()` now runs the paper's empirical-Bayes MAP update (EnsC5) as
  the default Stage 2. The returned `beta_hat` is the hybrid (Stage-2-
  refined), task-expanded matrix that all `sc_*` quantity functions read.
  DML point estimates and clustered standard errors are unchanged on
  the same `seed` --- they continue to use the Stage-1 DNN only.
  Set `stage2 = "none"` to recover 0.1 behavior exactly.

## New `scfit()` arguments

- `stage2`: one of `"map_c5"` (default), `"none"`, `"varref"`,
  `"mixed_logit"`. See `?scfit`.
- `stage2_seed`: integer seed for the 2nd DNN in the Stage-2 ensemble
  (default `12345L`). Independent of `seed`, so the master-seed
  bit-exact determinism guarantee extends through Stage 2.
- `normalize_deltaX` (default `FALSE`). When `TRUE`, the internal
  pipeline divides each `deltaX` column by its sample SD before
  training / lambda / DML / MAP, and un-standardizes user-facing slots
  at return. Use on designs with continuous attributes on very
  different scales --- the score-based MAP prior assumes
  `Var(deltaX_k) ~ 1`, which is violated by e.g. percentage-point tax
  rates and was producing per-respondent betas with extreme tails.

## New design diagnostic

- New export **`sc_design_diagnostic()`**: estimates per-coefficient
  R^2_Z from the MAP posterior and reports recovery-tier hints
  (mean / distributional / individual / ratio) per paper §6. Flagged
  `experimental = TRUE` until validated against the paper's
  simulation grid.

## New `sc_fit` slots

- `beta_hat_dnn`: the Stage-1 single-DNN task-level matrix that DML used.
- `beta_hat_dnn2`: the 2nd-DNN task-level matrix (only when Stage 2 ran).
- `beta_hat_ens`: the ensemble average (only when Stage 2 ran).
- `beta_hat_resp`: the respondent-level Stage-2-refined matrix.
- `sigma_prior`: diagonal prior variance used in MAP (`NULL` for
  `stage2 = "none"` / `"mixed_logit"`).
- `sigma_post_diag`: diagonal posterior variance from the MAP Hessian,
  averaged across respondents (`NULL` for `stage2 = "none"` /
  `"mixed_logit"`).
- `stage2_method`: one of `"none"`, `"map_c5"`, `"varref"`,
  `"mixed_logit"`, or `"mixed_logit_failed"`.

## New quantity-function argument

- Every `sc_*` quantity function now accepts
  `which_beta = c("hybrid", "dnn")`. Default `"hybrid"` reads the
  Stage-2-refined betas; `"dnn"` reads the Stage-1 single-DNN matrix
  on `object$beta_hat_dnn`. When `stage2 = "none"`, the two are
  numerically identical.

## Bug fixes

- `sc_baseline_logit` / `sc_baseline_lpm` now include an intercept;
  the previous no-intercept fits biased LPM AMCE coefficients on
  unbalanced designs (could flip signs).
- `sc_average(scale = "probability")` SE now via delta-method on
  theta's vcov (was empirical clustering on per-task contributions,
  10--20× too small).
- `sc_average(scale = "probability")` gprime computation now uses
  theta_hat (was per-respondent MAP betas; pushed G' → 0 on
  continuous-attribute designs, shrinking AME by ~58×).

## Bug guards

- Added an explicit regression test against the prototype's
  2026-04-26 prior-indexing bug (passing the MAP prior at task level
  rather than respondent level).
- Added orthogonality and determinism test suites that pin DML θ̂ and
  Vcov as invariant across Stage-2 choices on the same seed.

## Compatibility

- The user-facing surface (`coef`, `vcov`, `beta_hat`, `sigma_*`) is
  unchanged for `normalize_deltaX = FALSE` (the default), so existing
  scripts and the v0.2 paper-catchup behavior are preserved bit-exactly.

## New dependencies

- `lme4` added as `Suggests` (required only for
  `stage2 = "mixed_logit"`).

# sconjoint 0.1.0

Initial release.

## Estimator
- `scfit()`: structural deep-learning estimator with DML inference,
  respondent-clustered cross-fitting, and bit-exact determinism.
- `predict.sc_fit()`: forward-pass on new moderator data via `newdata`.
- `keep_modules` argument to persist per-fold torch modules.

## Structural quantities (21 functions)
- **Tier A**: `sc_mrs`, `sc_counterfactual`, `sc_wtp`, `sc_importance`,
  `sc_polarization`, `sc_fraction_preferring`, `sc_optimal_profile`,
  `sc_direction_intensity`, `sc_heterogeneity_test`.
- **Tier B**: `sc_subgroup`, `sc_compensating`, `sc_clusters`.
- **Tier C**: `sc_surplus`, `sc_welfare_change`, `sc_average`,
  `sc_indifference`, `sc_demand_curve`, `sc_decisiveness`, `sc_inequality`.

## Baselines
- `sc_baseline_logit`, `sc_baseline_lpm` for comparison with
  the structural model.

## Plots (7 functions)
- `plot_amce`, `plot_fraction`, `plot_hetero`, `plot_subgroup`,
  `plot_importance`: publication-quality diagnostic plots with
  `dummies`, `labels`, `groups` customization.
- `plot.sc_fit`: beta ridgelines and training loss trace.

## Bundled datasets
- `sw2022` (Saha & Weeks 2022), `gs2020` (Graham & Svolik 2020),
  `br2017` (Ballard-Rosa, Martin & Scheve 2017),
  `bs2013` (Bechtel & Scheve 2013) from published replication materials.
- `simdata`: synthetic DGP with known ground truth for validation.
