# Read-only application workflow

The three application data sources may be read from the existing
`ConjointStructural` project without changing that project.  All new prepared
objects, checkpoints, diagnostics, and results should be written below this
local `applications/` directory.

## Configuration

Copy `config.example.R` to an untracked local configuration file or set the
environment variable `SCONJOINT_APPLICATION_ROOT`.  Do not hard-code a
coauthor's home directory in analysis scripts.

The intended source root is currently:

`/Users/avidit/Dropbox/Research - Active/Conjoint/ConjointStructural`

The application adapters must open that tree read-only.  The local output root
defaults to `applications/output/` in this clone.

## Frozen prepared inputs found in the source project

- Saha--Weeks: `code/analysis/sw2022/out/prep_matrices.rds`
- Graham--Svolik: `code/analysis/gs2020/out/prep_matrices.rds`
- Ballard-Rosa et al.: `code/analysis/br2017/out/prep_matrices.rds`

Those objects are useful for initial adapter tests, but each adapter must also
record the coding decisions required by the rebuilt structural model.

In particular:

- use `code/analysis/br2017/04_data_prep_fixed.R`, not the legacy
  `code/61_data_preparation_ballard_rosa.R`; the latter contains a corrupted
  derived tax-rate contrast for code-5 rows;
- in Graham--Svolik, use `c_onLeft` to orient both the outcome and profile
  contrast consistently with the modeled alternative/position intercept;
- decide explicitly whether the manipulated candidate-age and experience
  attributes enter the Graham--Svolik structural utility.  Omitting them
  imposes zero utility coefficients;
- preserve variable respondent task counts and completion patterns rather than
  converting observations to an independent task sample.

## What can be done without modifying `ConjointStructural`

The mixed-logit fits, outer-fold checkpoints, design audits, held-out sequence
scores, calibration, task-order checks, completion diagnostics, rank and shape
sensitivities, tables, and figures can all be generated locally.  A separate
`ConjointStructural/mlcodex` directory is therefore unnecessary.  If one is
later desired for a handoff artifact, it should contain only copied final
outputs, never modified source data or legacy scripts.

## Design metadata still needed

Realized contrasts do not encode the full randomization protocol.  Each
application needs machine-readable metadata for its fielded blocking,
attribute restrictions, task order, and protocol exposure probabilities.
These must be checked against the original instrument or replication
documentation.  Realized support alone cannot establish the paper's
protocol-level repeated-contrast condition.

Exact full contrast cells are extremely sparse in all three applications, so
the exact-contrast design benchmark can be unbiased yet practically
uninformative.  A coarsened or marginal benchmark is possible only after
defining a different estimand and updating the paper accordingly.

See `DATA_FEASIBILITY.md` for the frozen-object inventory, preliminary matrix
rank checks, completion patterns, and application-specific coding repairs found
during the read-only audit.
