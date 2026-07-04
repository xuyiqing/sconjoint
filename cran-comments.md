# CRAN submission comments

## Resubmission

This is a resubmission of a new package (previous submission: 0.2.1,
reviewed 2026-07). `sconjoint` implements the structural deep-learning
conjoint estimator of Acharya, Hainmueller, and Xu (2026). All three
review comments have been addressed:

* **Missing \value in sconjoint-reexports.Rd**: added. The topic now
  documents that it re-exports the `ggplot2::autoplot()` generic and
  what calling it on an `sc_fit` object returns.

* **\dontrun{} examples**: all five topics that used `\dontrun{}` with
  schematic code (`sc_ame`, `sc_mrs`, `sc_profile`, `sc_validate_amce`,
  `sc_design_diagnostic`) now carry complete, executable examples that
  simulate a small conjoint dataset, fit the model, and call the
  documented function. They are wrapped in `\donttest{}` (not unwrapped)
  only because they require the 'torch' backend, which is not available
  on all check machines; each is additionally guarded by
  `torch::torch_is_installed()`. No `\dontrun{}` remains in the package.

* **Modifying the .GlobalEnv**: the package no longer touches the global
  environment. The previous version contained hand-rolled save/restore
  blocks for `.Random.seed` (assign/rm into `globalenv()`), intended to
  keep seeded fits from perturbing the caller's RNG state. These have
  all been replaced with `withr::local_seed()` /
  `withr::local_preserve_seed()`. No `assign()`, `rm()`, or `<<-` into
  the global environment remains.

## Test environments

* local macOS (aarch64-apple-darwin20 / arm64), R 4.5.3 -- `R CMD check --as-cran`
* win-builder (R-devel) -- `R CMD check --as-cran`

## R CMD check results

On both environments, `R CMD check --as-cran` gives 0 errors | 0 warnings |
1 note. The single NOTE is the standard new-submission note:

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
