# Saha--Weeks v2.1 rank and numerical diagnostics

This downstream pipeline is outcome-informed and descriptive. It never
changes the v2.1 reported primary, selects a rank, or enables formal inference.
It first validates the completed v2.1 reported-primary pointer, final manifest,
every parent artifact and input hash, the runtime, and a separate reviewed
rank/numerical authorization.

The common-fold rank panel fits `q=0,1,2` on the exact locked outer respondent
folds and recreates the same deterministic inner assignments. The
selected-procedure panel uses the complete frozen ten-candidate v2.1 grid. If
the v2.1 guardrail invoked the exact-constant fallback, a second panel uses the
exact-constant candidate at every rank. Rank is not selected from these
results.

The GH checks rerun the complete frozen within-rank fitting procedure at
`15,31,45` nodes for `q=1` and at `9,15,21` product nodes for `q=2`. The `q=2`
checks also rotate each fitted loading through four orthogonal orientations
while preserving `AA'`, which isolates finite-product-grid orientation error.
An empirical stability failure is retained and reported; it is never converted
to a formal-inference result.

After the completed parent v2.1 run and after reviewing the staged patch,
create the execution authorization without starting a fit:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/00_create_rank_numerical_authorization_v2_1.R \
  --reviewer=REVIEWER \
  --acknowledge-outcome-informed=true \
  --acknowledge-no-formal-inference=true \
  --acknowledge-no-rank-selection=true
```

Then run all stages, or run `rank`, `q1_refinement`, and `q2_refinement`
separately. The final manifest is withheld until all required stages exist and
validate.

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/05_rank_numerical_v2_1.R \
  --stage=all --force=false
```

All new results are written under
`applications/sw2022/results/mixed_logit_v2_1_rank_numerical`; parent v1, v2,
and v2.1 artifacts are read-only.
