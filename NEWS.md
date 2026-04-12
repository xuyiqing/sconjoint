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
