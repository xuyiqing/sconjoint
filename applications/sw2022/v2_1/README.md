# Saha--Weeks v2.1 post-pilot final analysis

This branch is separate from the failed v2 developmental pilot and never
rewrites it. It was specified after that pilot was observed, is explicitly
outcome-informed, and does not provide formal inference.

The candidate grid and computation schedule are the previously declared v2
production settings without alteration: exact constant; linear, width-4 ReLU,
and width-8 ReLU deviations at penalties 0.001, 0.01, and 0.1; five locked v1
outer folds; three respondent-level inner folds; 1,400 epochs; three starts;
and 31-node Gauss--Hermite integration at q=1.

The post-pilot descriptive guardrail is

```text
mean held-out sequence score(selected - exact constant) >= -0.001.
```

This is a materiality/noninferiority rule introduced after the failed pilot,
not a test and not a retroactive pilot-success criterion. The paired respondent
standard error is reported separately. If the selected procedure misses the
margin, its CV selections and diagnostic results remain recorded unchanged,
but the final reported primary model falls back to the independently fitted
exact constant q=1 mixed logit.

Fallback is never used to cure a failed optimizer, nesting, bound, constant-
eligibility, or provenance gate. If any such required gate fails, diagnostic
checkpoints and tables may be retained, but the reported primary is marked
unavailable and no consumable primary-pointer artifact is emitted.

Execution is fail-closed. A separate authorization must bind the byte-identical
failed-pilot manifest and its live artifacts, every failed-pilot generation
input at its reviewed hash, the complete current v2.1 input/source hash vector,
the runtime signature, configuration hash/version, and the -0.001 margin.
It must name the reviewer, carry an authorization timestamp, and explicitly
acknowledge the post-pilot/outcome-informed status, unavailable formal
inference, and preservation of the failed pilot. Absence or staleness stops
before any output directory or fit is created. Its own file hash is embedded in
every checkpoint and must remain unchanged through completion.

After independent review creates the authorization, the exact launch command is:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/04_fit_postpilot_final_v2_1.R \
  --force=true
```

Expected CPU runtime on the project Mac is approximately 60--90 minutes. Do
not run the old `--profile=production` command and do not call this result
outcome-blind or formal-inference eligible.

The reviewed one-time authorization is created (without launching a fit) by:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/00_create_final_analysis_authorization_v2_1.R
```

The creator refuses to overwrite an existing authorization, hashes its own
source as part of the runner generation context, writes atomically, and
immediately validates the on-disk authorization and every frozen input again.
