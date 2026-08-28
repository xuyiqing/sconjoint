# Saha--Weeks v2.1 sensitivity pipeline

This directory contains a downstream, versioned sensitivity battery for the
v2.1 post-pilot analysis. It is intentionally separate from both the v1
`sensitivity/` directory and the v2.1 primary-fit directory. It does not modify
the reported primary or the external `ConjointStructural` replication files.

Every runner fails before fitting unless
`results/mixed_logit_v2_1_postpilot_final/reported_primary_pointer.rds` is
present and validates against the completed parent result, parent manifest,
all parent artifacts, every frozen generation input, the runtime and
authorization hashes, and the chosen full/nested/assembled fit stamps. The
complete named hash lock is embedded in every checkpoint and manifest.

The battery is outcome-informed and descriptive. Formal inference,
maintained-model status, assumption verification, quantitative materiality
passes, and outcome-blind model-selection claims remain unavailable.

## Components

`R/07_run_sensitivities_v2_1.R` supplies:

- the primary 15-Z versus post-conjoint 19-Z sensitivity, with fold-specific
  training-respondent median imputation;
- the targeted Male × prior-run augmented-basis fit;
- held-out task-order, adjacent-pattern, and serial-residual diagnostics;
- an A/B profile-position relabeling refit; and
- the 1,191 versus 1,249 respondent no-Z completion sensitivity.

The 19-Z and Male × Run fits inherit the learner specification and integration
grid from each pointer-reported outer-training fit. Full-sample sensitivities
inherit the pointer-reported full specification and grid. No sensitivity
retunes the model family.

`R/08_run_misspecification_v2_1.R` reuses the exact v1 scenario definitions,
scenario order, and seed. It generates choices on the fielded three-task
design under a normal benchmark, positive and negative standardized skew,
symmetric bimodality, variance-standardized Student t5, party-varying
covariance scale, random response scale, and a Gaussian AR(1) additive index
shock. Each simulated outcome is refit with the pointer-reported normal q=1
specification. These are simulated-data stress tests—not empirical fits of
alternative likelihoods.

Empirical skewed, bimodal, t5, covariance-scale, random-scale, AR(1), and
task-varying likelihoods remain `not_run`. Nuisance-reoptimized profile
likelihoods also remain `not_run`; a likelihood slice is never relabeled as a
profile.

## Execution profiles

- `production` uses 1,400 epochs, three starts, 30 simulation replications per
  scenario, and 50,000 deterministic truth draws.
- `validated_fallback` is explicitly labeled as a smaller computational
  fallback: 800 epochs, two starts, five replications per scenario, and 10,000
  truth draws. It receives the same optimizer, provenance, pointer, and output
  validation gates and is never relabeled as production.

Long production runs can be checkpointed by component or scenario. Use a
common profile and configured replication count for every batch, then invoke
the `all` stage to assemble the complete manifest.

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/sensitivity/R/07_run_sensitivities_v2_1.R \
  --profile=validated_fallback --stage=all --force=false

applications/bin/Rscript45 \
  applications/sw2022/v2_1/sensitivity/R/08_run_misspecification_v2_1.R \
  --profile=validated_fallback --scenarios=all --replications=0 --force=false

applications/bin/Rscript45 \
  applications/sw2022/v2_1/sensitivity/R/09_validate_sensitivities_v2_1.R \
  --profile=validated_fallback --component=all
```

Run the validator only after both complete batteries finish. A simulation
validation pass means that the configured scenarios, minimum replication
count, artifact hashes, and optimizer gates passed. It is not a substantive
model-assessment pass and provides no formal coverage claim.
