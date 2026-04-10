# sconjoint 0.0.0.9000

## M5.a — predict(newdata) forward-pass
- `scfit()` gains `keep_modules` argument to persist per-fold torch modules.
- `predict.sc_fit()` now supports `newdata` (forward-pass on new moderator data) and `type` argument.

## M5 — Tier B quantities + case-study data
- New: `sc_subgroup()`, `sc_compensating()`, `sc_clusters()`.
- Bundled datasets: `sw2022`, `gs2020`, `bs2013` (synthetic fixtures).
- Quarto tutorial book with 3 case-study chapters.

## M4 — Tier A quantities + S3 methods
- 9 structural quantity functions: `sc_mrs()`, `sc_counterfactual()`, `sc_wtp()`, `sc_importance()`, `sc_polarization()`, `sc_fraction_preferring()`, `sc_optimal_profile()`, `sc_direction_intensity()`, `sc_heterogeneity_test()`.
- Full S3 method set: `summary()`, `predict()`, `plot()`, `autoplot()`.
- Profile helper: `sc_profile()`.

## M3 — DML inference + cross-fitting
- `scfit()` main estimator function.
- Respondent-clustered K-fold cross-fitting with bit-exact determinism across core counts.
- DML debiased inference with respondent-clustered standard errors.

## M2 — DNN architecture + training
- Torch-based DNN with Adam + BCE + L2 training.
- L'Ecuyer-CMRG parallel-safe RNG streams.

## M1 — Package skeleton
- Initial DESCRIPTION, NAMESPACE, CI workflows.
