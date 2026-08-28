# Saha--Weeks post-fit evidence audit

Run the extractor only after the fit, QOI/inference, numerical-refinement, and
assessment stages have finished for a profile:

```sh
applications/bin/Rscript45 applications/sw2022/R/07_extract_evidence.R \
  --profile=production --require-complete=true
```

The extractor does not edit any upstream artifact. It checks each RDS checksum
before and after reading and then writes only to
`applications/sw2022/results/evidence_audit/<profile>/`. A changed or missing
source, mismatched profile, incompatible class, mismatched recorded checksum,
mixed analysis signature, task-level fold split, or irreconcilable nested-CV
selection causes a fail-closed error.

The principal handoff is
`tables/section5_1_evidence_ledger.csv`. Supporting tables retain:

- every full-sample and outer-fold inner-CV candidate and selection;
- all starts, returned-state optimization gates, and bound diagnostics for the
  full and nested fits;
- rank-interiority diagnostics, identified-direction information eigenvalues,
  and the fixed-q sensitivity results;
- respondent-sequence scores by outer fold and the complete marginal and joint
  calibration tables;
- every structural plug-in row, primitive one-step target, transformed target,
  and quantity-specific reporting gate;
- every fresh-refit integration check and tolerance comparison; and
- component/claim states, source checksums, and within-family analysis-signature
  checks.

`tables/extraction_validation.csv` separates artifact-integrity checks from
evidence and reporting gates. Evidence-gate failures are recorded rather than
concealed. In particular, a passing optimization or numerical diagnostic is
not treated as a global-optimum certificate or proof of an asymptotic
numerical-error rate. Formal intervals and majority claims remain unavailable
whenever the inference artifact says so. Smoke and pilot audits are explicitly
marked nonreportable.
