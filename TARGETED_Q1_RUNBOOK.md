# Corrected targeted q = 1 application runbook

This checkout is a local-only continuation of `xuyiqing/sconjoint` at commit
`0ac7e0fbe2817c7c057afc15b7f1256bc896e55c`, with Yiqing's 2026-08-31
application snapshot preserved at the baseline tag. Nothing in this repository
should be pushed without explicit author approval.

## Correction

The package converts each selected refit's loading matrix to raw contrast units
before storing it as `refit$A` and `assembled$A_folds`. Several application
helpers divided those already-raw matrices by `sd_dx_folds` again. That affected
the post-fit directional-share, contrast-bound, and Graham--Svolik compensator
calculations. It did not affect the fitted likelihood, conditional means,
position effect, core respondent-sequence DML score, or the loading matrices
stored by the package.

The corrected contract is enforced by
`applications/R/test-loading-scale-contract.R`. In the saved Graham--Svolik
fit, every assembled loading is identical to its selected nested refit. The
co-party plug-in share is 0.832103 under the correct scale and 0.746202 under
the deleted double scaling.

The matched null-calibration code now distinguishes two cases. Direct calls to
the internal trainer still convert its standardized loading to raw units.
Objects returned by `scmix_tune_matrix(..., refit = TRUE)` are already raw and
are not converted again.

## Targeted analyses

`applications/R/targeted_q1_inference.R` reconstructs the fitted-sieve basis
from portable network states, checks it against saved all-fold predictions,
runs typed rotation-invariant targets through `scmix_dml`, records per-target
Riesz and ridge diagnostics, and writes fingerprinted atomic checkpoints.

The Graham--Svolik runner estimates prespecified opposition shares for seven
undemocratic actions, opposition shares for two valence violations, the
co-party preference share, co-party compensating shares overall and by
ideology, and the directional heterogeneity decomposition for the same ten
directions. The nonlinear best-policy and best-governance menus are excluded
because they are not the paper's fixed-contrast compensating estimand.

The Ballard--Rosa runner estimates two progressivity-direction shares,
directional heterogeneity for all native coordinates and three prespecified
tax-schedule contrasts, and position-neutral choice probabilities for the
steeply progressive plan against three alternatives overall and by party.

Both runners retain diagnostic estimates and diagnostic standard errors. They
never relabel them as formal confidence intervals when the fit is
diagnostic-only or the paper's verification record is absent.

## Execution and expected time on the 12-core MacBook Pro

Use the preserved R 4.5.3 and torch 0.16.3 environment. Set `SCONJOINT_ROOT` to
this checkout and `R_LIBS_USER` to the preserved 4.5 library before running.

The saved Graham--Svolik fit is sufficient. Basis reconstruction takes seconds.
The three targeted DML families are expected to take about 3 to 6 hours. Run
them with:

```sh
Rscript applications/gs2020/R/04b_targeted_q1_inference.R \
  --family=direction,compensating,heterogeneity
```

No corrected Ballard--Rosa assembled fit is present in the shared snapshot.
Unless the coauthor supplies `fit_primary_full.rds`,
`fit_primary_nested.rds`, and `fit_primary_assembled.rds`, a new v2.1 fit is
required. The fit is expected to take about 8 to 16 hours. Its targeted DML
stage should add roughly 0.5 to 1.5 hours. The existing Ballard--Rosa
application summaries must not be presented as results from this corrected
runner.

Every application writes to a fresh directory under `runs/`. The source data
and Yiqing's original folders remain unchanged.
