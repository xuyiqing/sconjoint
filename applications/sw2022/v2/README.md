# Saha--Weeks mixed-logit v2 development branch

This directory isolates the post-hoc correction prompted by the version-1
party diagnostic. Version 1 remains unchanged under
`applications/sw2022/results/mixed_logit/`. Version 2 writes only beneath
`applications/sw2022/results/mixed_logit_v2/`.

The corrected mean parameterization is

```text
mu(z) = clip{alpha + g_omega(z_dagger) - g_omega(0)}.
```

`alpha` is a compact, unpenalized reference level at the training fold's coded
moderator origin. The squared-weight penalty applies only to the centered
moderator deviation. The eligible grid contains an exact constant model, a
linear deviation, and ReLU deviations of widths 4 and 8. Within each training
fold, all corrected candidates share one stage-1 constant prefit. The constant
candidate and every flexible candidate then receive the same stage-2 epoch and
start budget. Flexible starts inherit the fitted structural coordinates, keep
randomized hidden layers, and zero only the deviation output map, so they begin
at the pooled function without creating an all-zero ReLU saddle. A flexible
candidate is rejected by the computational gate if its attained penalized
training objective is worse than the separately continued constant reference
beyond the declared tolerance. The continued constant is itself rejected if
its stage-2 objective worsens the shared stage-1 prefit beyond tolerance.
Stage 1 supplies multistart exploration of the pooled structural coordinates;
the stage-2 constant starts are intentionally identical continuations, retained
to make the declared candidate-stage budget auditable and comparable.

Run from the package root with the project-local R 4.5 launcher:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2/R/03_fit_penalty_pilot_v2.R \
  --profile=smoke --force=true

applications/bin/Rscript45 \
  applications/sw2022/v2/R/03_fit_penalty_pilot_v2.R \
  --profile=pilot --force=true
```

The smoke profile only checks interfaces and numerical nesting. The pilot
reuses the exact five version-1 respondent outer folds and reports paired
complete-sequence scores for the selected v2 model, an independently refitted
exact constant v2 comparator, and the frozen v1 primary, pooled, and targeted
diagnostics. It also reports calibration, AMCE projections, party variation in
the fitted conditional mean, compact-bound activity, optimization gates, and
input/artifact hashes.

Both profiles are outcome-informed model-development exercises. Checkpoint
stamps include generation-time hashes for the prepared data, frozen v1
artifacts, runner, configuration, `DESCRIPTION`, `NAMESPACE`, and every sorted
package source file under `R/`, as well as both project R 4.5 launchers. Stamps
also bind the R version/platform and the Torch, pkgload, and source-package
versions. A resume fails if any named hash or runtime field differs. The same
complete hash vector must remain unchanged through completion. They do not
produce formal inference or a paper production result. The production profile
is fail-closed until the pilot audit has been reviewed. It requires a separate
positive authorization RDS whose config-version, config-file hash, and reviewed
pilot-manifest hash match current files. The authorization must also contain the
reviewed pilot's complete named generation-input hash vector, which must exactly
match both the pilot manifest and the current execution, along with the reviewed
runtime signature; absence or any source/runtime change never authorizes
execution. The lock also requires the expected successful-pilot manifest schema
and re-hashes every listed pilot artifact, so a missing or modified reviewed
artifact invalidates authorization. If it is later authorized, the entire v2
chain—not merely the pilot—must be rerun, and the complete configuration for
subsequent applications should be frozen before their outcomes are inspected.
