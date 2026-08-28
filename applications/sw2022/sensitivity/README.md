# Saha--Weeks application sensitivities

This directory contains application-only sensitivity code.  It does not
change the maintained estimator or the production tuning configuration, and
it never writes to the read-only `ConjointStructural` source tree.

Run from the package root after the corresponding primary fit exists:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/07_run_sensitivities.R \
  --profile=smoke --force=true
```

The production command is identical except for `--profile=production`.  It
writes only below
`applications/sw2022/results/mixed_logit/<profile>/sensitivity_analysis/`.
Each component can be checkpointed separately with `--stage=z19`,
`--stage=interaction`, `--stage=process`, or `--stage=completion`.  After all
components finish, validate their hashes, fold isolation, ranks, sample counts,
and fail-closed labels with:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/08_validate_sensitivity_artifacts.R \
  --profile=production
```

The main runner performs four application-specific checks:

1. It refits the fixed-rank mixed logit with the four post-conjoint
   moderators added to the primary 15 moderators.  Missing post-conjoint
   values are median-imputed using the relevant training respondents only;
   outer-held-out values never determine an imputation median or scale.
2. It refits the model after adding the profile-level Male-by-prior-run
   interaction and reports the interaction, conditional effects, structural
   choice probabilities, and held-out sequence-score comparison.
3. It constructs task-order, transition, residual-serial, and position
   diagnostics from out-of-fold sequence predictions, and it performs a
   full-sample A/B profile-swap optimization check.
4. It reconstructs all valid two- or three-task respondents from the raw
   replication file, fits the same intercept-only-moderator mixed-logit model
   to the primary and expanded samples, and compares structural quantities
   and respondent-clustered AMCE-style benchmarks.

The separate design-specific simulated-data battery is run and validated with:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/08_run_misspecification_experiments.R \
  --profile=production --scenarios=all --replications=0 --force=true
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/09_validate_misspecification_artifacts.R \
  --profile=production
```

Here `--replications=0` selects the frozen profile default (30 in production).
After that validator writes its artifact, rerun the parent manifest and
validator without refitting completed sensitivity components:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/07_run_sensitivities.R \
  --profile=production --stage=all --force=false
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/08_validate_sensitivity_artifacts.R \
  --profile=production
```

This order makes the independently validated nested simulation artifacts part
of the parent manifest.  `05_assessment.R` then verifies both provenance
layers, copies the tables byte-for-byte, and `06_export_bundle.R` rechecks the
source, input/config, and copy hashes before export:

```sh
applications/bin/Rscript45 applications/sw2022/R/05_assessment.R \
  --profile=production
applications/bin/Rscript45 applications/sw2022/R/06_export_bundle.R \
  --profile=production
```

These are sensitivities and diagnostics.  They do not establish normality,
common covariance, independent shocks, or noninformative completion.  The
post-conjoint and interaction models are not promoted to the maintained
primary specification.  No materiality threshold was preregistered for this
reanalysis, so the script deliberately records results without declaring a
substantive pass.  The simulated-data battery evaluates the maintained normal
estimator under calibrated shape, party-varying covariance, serial-shock, and
random-scale DGPs.  It is not an empirical fit or identification argument for
those alternative families: empirical residual-shape, covariance-by-Z,
serial-shock, task-process, and random-scale alternative models remain visibly
`not_run`, and formal coverage remains withheld.

## Post-hoc party-by-candidate-gender mean diagnostic

The primary production DNN shrinks its conditional mean nearly to a pooled
mean across party, while the design-based Female-versus-Male AMCE-style check
differs visibly by party.  The isolated runner below diagnoses that mismatch
without changing or refitting the maintained primary model:

```sh
applications/bin/Rscript45 \
  applications/sw2022/sensitivity/10_run_party_gender_mean_sensitivity.R \
  --profile=smoke --force=true
```

Use `--profile=production` only after the production primary artifacts are
final.  The runner compares the inherited outer-fold primary predictions with
two fixed-rank, fixed-GH mixed-logit diagnostics: an unpenalized pooled mean
and that same mean plus only Republican- and Independent-by-candidate-Male
deviations.  It also records a q=0 scope check showing why all party-specific
slopes were not used.  The specification was designed after observing the
mismatch, so its scores are cross-fitted with respect to each refit but are not
an outcome-blind assessment of the diagnostic-selection process.  Formal
inference, maintained-model status, and pass/fail materiality claims remain
withheld.  Outputs are written separately under
`results/party_gender_mean_sensitivity/<profile>/`.
