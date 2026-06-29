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

On win-builder the same NOTE also lists possibly-misspelled words in the
Description: `Acharya`, `Hainmueller`, and `Xu` (the cited authors'
surnames) and `debiased` (a standard term from the double/debiased
machine-learning literature of Chernozhukov et al., 2018). All are spelled
correctly.

## Notes for the reviewer

* The package depends on 'torch'. Every example that fits a model is
  wrapped in `\donttest{}` and additionally guarded by
  `torch::torch_is_installed()`, so the examples are skipped (not errored)
  on machines where the libtorch backend is not installed. The package
  itself downloads nothing at install or run time.
* Suggested packages used as optional first-stage learners ('glmnet',
  'grf') and the optional mixed-logit Stage 2 ('lme4') are used
  conditionally via `requireNamespace()`.
* The bundled datasets are small extracts from public replication archives
  on the Harvard Dataverse, each under a license that permits
  redistribution and commercial use: `sw2022` (CC0 1.0,
  doi:10.7910/DVN/KVTPVX), `gs2020` (CC BY 4.0, doi:10.7910/DVN/EEARKA),
  `br2017` (CC0 1.0, doi:10.7910/DVN/NGRGS5), and `bs2013` (CC0 1.0,
  doi:10.7910/DVN/UGZ2BY). Each dataset's `@source` records its citation,
  Dataverse DOI, and license; `gs2020` (CC BY 4.0) is attributed
  accordingly.
