# CRAN submission comments

## Submission

This is a resubmission of a new package (previous submission: 0.2.1, reviewed 2026-07).
`sconjoint` implements the structural random-utility estimator for conjoint experiments of
Acharya, Hainmueller, and Xu (2026).

Since the previous submission the package has been rebuilt around a single model. The functions
the earlier review commented on no longer exist, and neither does the dependence on 'torch'. The
review's three general points apply to the new code as follows:

* **\value**: every exported function's help page documents its return value.

* **\dontrun{}**: no example uses `\dontrun{}`. Slow examples are wrapped in `\donttest{}` and run
  on a small bundled fit (`inst/extdata/toy_fit.rds`).

* **Modifying the .GlobalEnv**: seeded code uses `withr::local_seed()` / `withr::with_seed()`.
  <!-- Before submitting: R/rum-data.R (lines 138-142) still saves and restores .Random.seed in
  .GlobalEnv with assign()/rm(), which the earlier review asked us to remove. Fix it, then state
  here that the package does not touch the global environment. -->

## Test environments

To be filled in after the release checks (local macOS arm64 and win-builder, `R CMD check --as-cran`).

## R CMD check results

To be filled in after the release checks.

## Notes for the reviewer

* The package downloads nothing at install or run time.
* The bundled datasets are extracts from public replication archives on the Harvard Dataverse,
  each under a license that permits redistribution: `gs2020` (CC BY 4.0, doi:10.7910/DVN/EEARKA)
  and `br2017` (CC0 1.0, doi:10.7910/DVN/NGRGS5). Each dataset's `@source` records its citation,
  Dataverse DOI, and license; `gs2020` (CC BY 4.0) is attributed accordingly.
