# Saha--Weeks v2.1 descriptive penalized-criterion profile sequences

This add-only module implements the local-curvature assessment requested in
`paperps.tex`. It has not been launched. Its outputs are descriptive
penalized-criterion profile sequences with unpenalized complete-sequence
likelihood overlays; they are not literal profile likelihoods. The module does
not alter package `R/` sources, the reported-primary analysis, rank/numerical
artifacts, or any prior result.

## What is fixed

Every grid point inherits the completed reported-primary pointer and fixes its
full-sample q=1 learner and tuning choice: width-4 centered ReLU conditional
mean, deviation penalty 0.1, the reported preprocessing, rank-one normal
factor, and the exact reported 31-node Gauss--Hermite grid. There is no
retuning, rank selection, learner selection, integration selection, or sieve
change within a profile.

At each target value, the remaining nuisance components are reoptimized under
the same penalized criterion. The target constraint is reimposed after every
optimizer step. Each point starts from the reported state plus two
deterministic local perturbations; all three continuations use the reported
1,400-step and learning-rate controls. A point passes only when its target
error is at most `2e-5`, no compact bound is active, the penalized objective is
stable, and the equality-constrained projected gradient is at most `1e-2`.
Each point is checkpointed independently.

The output table reports the unpenalized integrated log likelihood of each
respondent's complete response sequence as an overlay. It separately records
the fixed mean-deviation penalty and penalized objective used for nuisance
reoptimization. Because nuisance components are optimized under the penalized
criterion rather than the unpenalized likelihood, the overlay must not be
called a profile likelihood.

## Frozen five-point grids

- kappa: reported value plus `-0.08, -0.04, 0, 0.04, 0.08`;
- respondent-average Female-vs-Male conditional mean: reported value plus
  `-0.08, -0.04, 0, 0.04, 0.08`;
- active rank-one covariance eigenvalue: reported value times
  `0.50, 0.75, 1.00, 1.25, 1.50` (strictly inside q=1, so this grid does not
  touch the rank boundary); and
- headline contest: the population position-neutral probability for the
  prespecified `very_few` contest, reported value plus
  `-0.04, -0.02, 0, 0.02, 0.04`.

The contest constraint and unpenalized likelihood overlay use the same frozen
GH31 grid, so the target and overlay do not mix integration rules.

The contest remains conditional on the advertised unrestricted support; the
fielded randomizer and exact support were not document-certified.

## Interpretation

These are descriptive penalized-criterion profile sequences, not likelihood
profiles or likelihood-ratio inference. No LR cutoff, confidence set, p-value,
or formal standard error is constructed. Flatness is evidence of weak
application-level curvature, not a proof of global nonidentification. Formal
inference remains unavailable for this application because the separate
inference gates did not pass.

No point is checkpointable unless at least one start passes every target,
objective-stability, compact-bound, and equality-constrained projected-gradient
gate. If all starts fail, the runner exits nonzero. A direction result and
manifest are emitted only after all five checkpoints independently pass their
stamp, target, objective, gradient, bound, and respondent-log-likelihood
consistency checks. The checkpoint stores the selected start's final relative
penalized-objective change, verifies that it is no larger than `opt_tol`, and
requires exact agreement with the retained selected-start diagnostic.

## Review and execution

The runner refuses to start unless all reported-primary pointer, manifest,
artifact, input, authorization, package-source, launcher, and runtime hashes
remain exact. It also requires a separate reviewer-named authorization and an
explicit launch flag. After code review, create the one-time authorization
without starting a fit:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/00_create_profile_sequence_authorization_v2_1.R \
  --reviewed-by="REVIEWER NAME"
```

Then run all directions, resumably:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/06_profile_sequence_likelihoods_v2_1.R \
  --direction=all --force=false --reviewed-launch=true
```

A single direction can be requested with `--direction=kappa`,
`female_vs_male_mean`, `active_covariance_eigenvalue`, or
`headline_contest_probability`. A stale checkpoint fails closed;
`--force=true` is intended only after reviewing the mismatch.

Based on the completed q=1 full-fit timings and a no-fit benchmark of the
nonlinear contest projector, the expected CPU runtime for all 20 points is
approximately 30--60 minutes on the project Mac. This is an estimate; each
checkpoint and direction manifest records elapsed seconds, and checkpointing
limits the cost of an interruption. The contest direction is expected to be
the slowest.

No authorization or penalized-criterion profile output existed when this
module was added.
