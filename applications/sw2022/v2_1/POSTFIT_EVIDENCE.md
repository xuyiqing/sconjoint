# Saha--Weeks v2.1 post-fit evidence

This stage runs only after `04_fit_postpilot_final_v2_1.R` has completed and
published both `manifest.rds` and `reported_primary_pointer.rds`. It is an
evidence-generation stage, not a Section 5.1 draft.

The runner resolves the reported primary once from the pointer, validates the
pointer and every producer artifact against the completed manifest, validates
all live producer-generation inputs, and then uses only the chosen full,
nested, and assembled fits. It never chooses again between the selected
procedure and the exact-constant fallback.

The output includes full-sample structural plug-ins, diagnostic-only DML,
respondent-cross-fitted scores and calibration, full and pair response-pattern
checks, order/serial and position diagnostics, direct AMCE-style benchmarks,
and completion/design ledgers. The cross-fitted results retain their
post-pilot/outcome-informed label. Formal inference is unavailable.

Prior v1 assessment, QOI, rank, sensitivity, and post-hoc diagnostic artifacts
are not ingested. The only reused non-v2.1 artifacts are the hash-validated,
estimator-independent preparation, design, and completion artifacts.

From the package root, after the final-fit manifest is present, run:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/R/05_postfit_evidence_v2_1.R \
  --output-name=final
```

The runner constructs a staging directory and publishes it by a single rename
only after all gates pass. It refuses to overwrite an existing output. A
failed staging directory is intentionally retained for audit rather than being
silently deleted.

Helper tests are read-only with respect to application artifacts:

```sh
applications/bin/Rscript45 \
  applications/sw2022/v2_1/tests/test_postfit_helpers_v2_1.R

applications/bin/Rscript45 \
  applications/sw2022/v2_1/tests/test_postfit_runner_symbols_v2_1.R
```
