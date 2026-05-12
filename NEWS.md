# sconjoint 0.2.0 (development version)

## Paper-catchup release

This release brings `sconjoint` up to the algorithmic defaults in the
2026-04 paper revision of Acharya, Hainmueller, and Xu (2026).

## Major default-behavior change

- `scfit()` now runs the paper's empirical-Bayes MAP update (EnsC5) as
  the default Stage 2. The returned `beta_hat` is the hybrid (Stage-2-
  refined), task-expanded matrix that all `sc_*` quantity functions read.
  DML point estimates and clustered standard errors are unchanged on
  the same `seed` — they continue to use the Stage-1 DNN only.
  Set `stage2 = "none"` to recover 0.1 behavior exactly.

## New `scfit()` arguments

- `stage2`: one of `"map_c5"` (default), `"none"`, `"varref"`,
  `"mixed_logit"`. See `?scfit`.
- `stage2_seed`: integer seed for the 2nd DNN in the Stage-2 ensemble
  (default `12345L`). Independent of `seed`, so the master-seed
  bit-exact determinism guarantee extends through Stage 2.

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

## Bug guards

- Added an explicit regression test against the prototype's
  2026-04-26 prior-indexing bug (passing the MAP prior at task level
  rather than respondent level).
- Added orthogonality and determinism test suites that pin DML θ̂ and
  Vcov as invariant across Stage-2 choices on the same seed.

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
