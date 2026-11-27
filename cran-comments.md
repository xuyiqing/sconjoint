# CRAN submission comments

## Submission

This is a resubmission of a new package (previous submission: 0.2.1, reviewed 2026-07).
`sconjoint` implements the structural random-utility estimator for conjoint experiments of
Acharya, Hainmueller, and Xu (2026).

Since the previous submission the package has been rebuilt around a single model. The functions
the earlier review commented on no longer exist, and neither does the dependence on 'torch'. The
review's three general points apply to the new code as follows:

* **\value**: every exported function's help page documents its return value.

* **\dontrun{}**: no example uses `\dontrun{}`. Every example runs in under a second, on a small
  bundled fit (`inst/extdata/toy_fit.rds`) or simulated data.

* **Modifying the .GlobalEnv**: the package does not write to the global environment and never
  calls `set.seed()`. Seeded draws run inside `withr::with_seed()`, which restores the caller's
  random-number state afterwards.

## Test environments

* local macOS 27.2 (arm64), R 4.5.3: `R CMD check --as-cran`
* win-builder (R-devel): to be run before submission
* macOS builder (R-release): to be run before submission

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewer

* The package downloads nothing at install or run time.
* The tests shipped with the package take about two seconds. The full test suite stays in the
  package's GitHub repository.
* The bundled datasets are extracts from public replication archives on the Harvard Dataverse,
  each under a license that permits redistribution: `gs2020` (CC BY 4.0, doi:10.7910/DVN/EEARKA)
  and `br2017` (CC0 1.0, doi:10.7910/DVN/NGRGS5). Each dataset's `@source` records its citation,
  Dataverse DOI, and license; `gs2020` (CC BY 4.0) is attributed accordingly. `simconjoint` is
  simulated.
