# mlcodex provenance

This working repository is an isolated, local-only continuation of
`xuyiqing/sconjoint` for the paper applications. It must not modify or overwrite
the source folders in `ConjointStructural`, and it must not be pushed without
explicit authorization from the authors.

## Baseline

- Upstream repository: `https://github.com/xuyiqing/sconjoint.git`
- Upstream branch: `feat/mixed-logit`
- Upstream commit: `0ac7e0fbe2817c7c057afc15b7f1256bc896e55c`
- Application overlay:
  `/Users/avidit/Dropbox/Research - Active/Conjoint/ConjointStructural/2608_issues/Yiqing/applications_code_2026-08-31/applications`
- Saved Graham--Svolik inputs:
  `/Users/avidit/Dropbox/Research - Active/Conjoint/ConjointStructural/2608_issues/Yiqing/applications_code_2026-08-31/artifacts/gs2020`

The application overlay contains Yiqing's uncommitted work after the upstream
commit, including the matched null-calibration scripts. The first mlcodex commit
records this composite state before any correction.

Large RDS inputs and generated runs are excluded from Git. Their SHA-256 hashes
are recorded under `provenance/`. New runs must write to a fresh directory under
`runs/` and record the source commit, R session, seeds, input hashes, and output
hashes.

## Frozen inference protocols

The historical v2.2 inference contract is
`paperps-inference-2026-09-05-v2.2`. Its normative documents are
`INFERENCE_PROTOCOL_V22.md` and `inst/protocol/inference-v2.2.yaml`.
Its files and checksum remain unchanged as historical audit evidence.

Before simulation calibration, an independent prospective-design review found
that v2.2 did not give its separately named boundary constants distinct
operational roles. The narrow v2.3 correction is
`paperps-inference-2026-09-06-v2.3`. Its normative documents are
`INFERENCE_PROTOCOL_V23.md` and `inst/protocol/inference-v2.3.yaml`. V2.3 uses
one normalized compound G006 statistic with independently calibrated component
cutoffs and limits production calibration to `Rank-Scope: 1`. No production
values file or approval manifest exists yet. Rank-zero and rank-two results
remain diagnostic sensitivities, and existing v2.1 or v2.2 artifacts are not
grandfathered.

## Required reproduction check

Before an expensive rerun, the untouched baseline must pass Yiqing's one-cell
reproduction gate. It must select `relu4_wd1e-01` and reproduce the saved loading
matrix within the declared `1e-5` absolute tolerance. On the preserved R 4.5.3
and torch 0.16.3 CPU environment, the observed maximum deviation was
`7.12e-6`. A larger difference or a changed selected learner blocks substantive
reporting.

## conjointRUM 0.1.3 port (2026-09-24)

- Source: `conjointRUM_0.1.3.tar.gz` in `ConjointStructural/2608_issues/Avi/mlcodex/conjointRUM/`,
  SHA-256 `0af0dc558427f52e3c7449206a556a63f4181c53b0b1bb176ebcb3160b896c12`. Its `R/`, `src/`
  and `tests/` match Avi's source tree on 2026-09-24. Every RUM result in the 09-23 memo and the
  09-24 three-applications brief used this version.
- Destination: branch `feat/rum`, from `feat/mixed-logit` at `62125e6`. Files: `R/rum-*.R`,
  `src/rum_core.cpp`, `man/rum_*.Rd`, `tests/testthat/rum/*.R`.
- Code changes: none. The R files turn Avi's roxygen-style comments into plain comments (his Rd
  files in `man/` are the help pages) and tag each export with `@export`. `Makevars`
  (`CXX_STD = CXX11`) is dropped. `RcppExports` is regenerated for `sconjoint`. The test scripts
  change only the package name and the R file names.
- Parity on the Ballard-Rosa data of the 09-23 memo (1,991 respondents): 15 checks, all
  bit-for-bit identical to conjointRUM 0.1.3 (largest difference 0). They cover the quadrature,
  subsetting, likelihood, information, simulation, the full-sample formula fit, the network training
  under Avi's frozen settings, targets (fixed and population), a moment bank, the same-fit
  assessment, the held-out assessment and the score comparison. Script and log:
  `2608_issues/Yiqing/rum_absorb_2026-09-24/`. Avi's nine test scripts pass, and his synthetic
  example prints the same output.
- The branch is local-only. Pushing it publishes Avi's unreleased code in a public repository,
  which needs the authors' agreement.
