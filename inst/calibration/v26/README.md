# Inference validation v2.6

This directory contains the driver and audit mirrors for the prospective v2.6
validation study. The protocol is frozen in `INFERENCE_PROTOCOL_V26.md` and
`inst/protocol/inference-v2.6-*`.

There is exactly one state directory and one run directory relative to the
hash-approved source root:

- state: `inst/protocol`
- run: `runs/v26/rapid_validation_2026-09-07`

The driver accepts no directory override. Its commands are:

```text
run-validation.R status
run-validation.R wave WAVE
run-validation.R collect preliminary|50|100
run-validation.R assess-preliminary
run-validation.R assess-50
```

Waves 1 through 4 form the eight-replication-per-cell preliminary stage.
Wave 1 is the sealed, evidence-eligible runtime prefix. Waves 5 through 25
continue the identical stream to 50 replications per cell. Waves 26 through
50 are the preferred precision continuation to 100 per cell.

The files in `source/` must be byte-for-byte mirrors of
`R/calibration-v26.R` and `R/calibration-v26-evaluator.R`. The implementation
inventory rejects a mismatch. The driver never creates an evaluator approval,
wave-one approval, endpoint-completion record, or family approval.

No statistical result may be inspected after Wave 1 until all four
preliminary waves and their manifests are complete. Eight replications per
cell can produce only a descriptive preliminary report. Mechanical family
approval is prohibited until the complete 50-per-cell endpoint.
