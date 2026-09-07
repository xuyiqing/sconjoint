# Inference validation v2.5

This directory contains the command-line driver and audit source mirrors for
the prospective v2.5 inference-validation study. The protocol is frozen in
`INFERENCE_PROTOCOL_V25.md` and `inst/protocol/inference-v2.5-*`.

Production execution has one state directory and one run directory relative
to the hash-approved source root:

- state: `inst/protocol`
- run: `runs/v25/step5c_integrity_and_evidence_2026-09-07`

The driver therefore accepts no state- or run-directory override. Its commands
are:

```text
run-validation.R status
run-validation.R pilot
run-validation.R evidence-batch BATCH
run-validation.R evidence-manifest
run-validation.R evidence-assess
```

The files in `source/` are exact byte-for-byte mirrors of
`R/calibration-v25.R` and `R/calibration-v25-evaluator.R`. The implementation
inventory rejects a missing or mismatched mirror. These copies exist only to
make the outcome lock, state machine, and prospective design easy to audit
from an installed package.

The driver never creates an approval. Before any v2.5 outcome can be generated,
the complete implementation inventory must be committed and hash-approved.
The pilot exposes only the frozen integrity, timing, and resource metadata.
