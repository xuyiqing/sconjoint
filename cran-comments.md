# CRAN submission comments

## Submission

This is a new submission. `sconjoint` implements the structural
deep-learning conjoint estimator of Acharya, Hainmueller, and Xu (2026).

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.3 -- `R CMD check --as-cran`
* TODO before submitting: win-builder (R-devel) and the macbuilder
  service (macos-arm64), per CRAN policy.

## R CMD check results

`R CMD check --as-cran`: 0 errors | 0 warnings | 1 note.

The single NOTE is the standard new-submission note:

```
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Yiqing Xu <yiqingxu@stanford.edu>'
  New submission
```

## Notes for the reviewer

* The package depends on 'torch'. Every example that fits a model is
  wrapped in `\donttest{}` and additionally guarded by
  `torch::torch_is_installed()`, so the examples are skipped (not errored)
  on machines where the libtorch backend is not installed. The package
  itself downloads nothing at install or run time.
* Suggested packages used as optional first-stage learners ('glmnet',
  'grf') and the optional mixed-logit Stage 2 ('lme4') are used
  conditionally via `requireNamespace()`.
* The bundled datasets (`sw2022`, `gs2020`, `br2017`, `bs2013`) are small
  extracts from the published replication materials of the cited studies,
  included for illustration and documented with their `@source`. <!-- TODO
  (maintainer): state the redistribution license / permission for each
  bundled dataset before submitting. -->
