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

## Required reproduction check

Before an expensive rerun, the untouched baseline must pass Yiqing's one-cell
reproduction gate. It must select `relu4_wd1e-01` and reproduce the saved loading
matrix with maximum absolute deviation zero. A failure indicates an environment
or source mismatch and blocks substantive reporting.
