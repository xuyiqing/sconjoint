# Design-specific misspecification experiments

`08_run_misspecification_experiments.R` implements the simulation route in
Section 4 of `paperps.tex`.  It uses the observed 1,191-respondent, three-task
design and the fitted primary conditional means as the calibration population.
It generates from eight data-generating processes and refits the selected
normal primary estimator to each simulated data set:

1. a correctly specified normal benchmark;
2. positive and negative orientations of a standardized skewed factor;
3. a standardized symmetric bimodal factor;
4. a variance-standardized finite-covariance Student-t factor;
5. party-varying factor scales normalized to preserve aggregate covariance;
6. a mean-one lognormal respondent response scale; and
7. an additive stationary Gaussian AR(1) index component.

For the nonnormal q=1 experiments, the loading is oriented by making its
largest-absolute coordinate positive.  Both signs of the skewed factor are
run.  The alternative distributions have mean zero and unit variance, and the
party multipliers have respondent-weighted mean square one.  Consequently,
differences are not induced merely by changing the fitted mean or aggregate
residual covariance.  Random-scale comparisons are restricted to choice
probabilities and positive-scale-invariant sign shares.

This is a misspecification experiment, not an empirical alternative-family
fit and not an identification result.  The primary tuning decision is held
fixed and all normal-model parameters are refit.  Because the application
currently withholds formal inference, the runner does not manufacture an
oracle Monte Carlo interval or call it coverage.  It records bias, empirical
standard deviation, RMSE, quantiles, truth-integration refinement, and every
optimization gate; formal coverage and materiality passage remain explicitly
unavailable.

From the package root, run and validate the smoke battery with:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/08_run_misspecification_experiments.R \
  --profile=smoke --scenarios=all --force=true

applications/bin/Rscript45 \
  applications/sw2022/sensitivity/09_validate_misspecification_artifacts.R \
  --profile=smoke
```

The production battery is resumable at one checkpoint per scenario and
replication:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/08_run_misspecification_experiments.R \
  --profile=production --scenarios=all --force=false

applications/bin/Rscript45 \
  applications/sw2022/sensitivity/09_validate_misspecification_artifacts.R \
  --profile=production
```

The frozen production plan has 30 replications for each of eight scenarios
(240 full-sample refits, each with the production epoch and multistart rules)
and 50,000 deterministic truth-integration draws.  This is deliberately a
long final-analysis job.  A partial run can be requested with a comma-separated
`--scenarios=` list or a disclosed `--replications=` override; the output then
remains visibly incomplete.  Re-running without `--force=true` loads completed
checkpoints.

If 240 refits proves prohibitively slow, the frozen fail-closed minimum is 20
replications per scenario (160 refits):

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/08_run_misspecification_experiments.R \
  --profile=production --scenarios=all --replications=20 --force=false
```

The tables report the Monte Carlo standard error of estimated bias, and the
bundle records both the 30-replication frozen target and the 20-replication
override.  The production validator rejects fewer than 20 replications per
scenario.  Twenty is adequate for a bounded, explicitly Monte-Carlo-uncertain
sensitivity diagnostic, but the 30-replication target remains preferable.  At
either setting this is an hours-to-overnight job rather than an interactive
run; exact wall time depends on the selected network and CPU.  Per-scenario,
per-replication checkpoints make interruption and resumption safe.

All artifacts are written below
`applications/sw2022/results/mixed_logit/<profile>/sensitivity_analysis/misspecification/`.
No source data, primary fit, or file in `ConjointStructural` is modified.
