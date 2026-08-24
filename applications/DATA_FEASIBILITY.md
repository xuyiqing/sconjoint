# Feasibility audit for the three applications

Audit date: 2026-08-23.

The source project was inspected read-only at:

`/Users/avidit/Dropbox/Research - Active/Conjoint/ConjointStructural`

No files or directories were created or modified there. Future preparation,
fits, checkpoints, diagnostics, tables, and figures can be written under this
clone's `applications/output/`; a `ConjointStructural/mlcodex` directory is not
needed.

## Frozen prepared objects

| Application | Tasks | Respondents | Preference columns | Moderator columns | Source object |
|---|---:|---:|---:|---:|---|
| Saha--Weeks 2022 | 3,573 | 1,191 | 13 | 19 | `code/analysis/sw2022/out/prep_matrices.rds` |
| Graham--Svolik 2020 | 20,657 | 1,605 | 30 | 22 | `code/analysis/gs2020/out/prep_matrices.rds` |
| Ballard-Rosa et al. 2017 | 16,000 | 2,000 | 7 | 23 | `code/analysis/br2017/out/prep_matrices.rds` |

These objects establish data availability, not final coding correctness.

## Coding repairs required before fitting

- Ballard-Rosa must use `code/analysis/br2017/04_data_prep_fixed.R` and the
  frozen object's `DeltaX_fixed`. The legacy preparation has a corrupted
  code-5 tax-rate recode; 5,044 of 16,000 task contrasts change when corrected.
- Graham--Svolik must use `c_onLeft` to orient both `DeltaX` and `Y` consistently
  with the modeled alternative/position intercept. The legacy object conditions
  on candidate 1 even though candidate 1 is not always displayed on the same
  side.
- Graham--Svolik's legacy 30-column utility omits manipulated candidate age and
  experience. The rebuilt application must include them or explicitly maintain
  zero utility effects.
- Variable task counts and observed completion patterns must be retained. They
  cannot be converted to an independent task-row sample.

## Completion information available

- Saha--Weeks: the preparation drops 7 partial completers and then 51
  respondents with invalid demographics; those exclusions should be
  reconstructed and assessed.
- Graham--Svolik: 262 task choices are missing among 76 respondents. Observed
  task counts range from 1 to 13; 1,538 of 1,605 respondents have 13 tasks.
- Ballard-Rosa: the current sample has all 8 tasks for each of 2,000
  respondents.

## Preliminary realized-design audit

The affine contrast matrix has full empirical column rank in all three frozen
objects: 14/14 for Saha--Weeks, 31/31 for Graham--Svolik, and 8/8 for
Ballard-Rosa. The transparent unrestricted covariance-measurement rank is
91/91, 429/465, and 28/28, respectively. Thus the stronger unrestricted
covariance condition succeeds for Saha--Weeks and Ballard-Rosa but not for
Graham--Svolik. A certified rank-restricted injectivity check is needed there;
failure of the stronger check is not proof of nonidentification.

Exact repeated full ordered contrasts are nearly absent in the realized data:
one respondent in Saha--Weeks and none in the other two applications. Realized
counts do not determine protocol support, so application of the paper's global
identification theorem requires machine-readable randomization and completion
metadata verified against the fielded instruments. Those protocol
probabilities are not completely encoded in the current frozen objects.

Exact full-contrast cells are also sparse: 3,087 distinct cells among 3,573
Saha--Weeks tasks (largest cell 15), 20,620 among 20,657 Graham--Svolik tasks
(largest cell 2), and 15,444 among 16,000 corrected Ballard-Rosa tasks (largest
cell 4). The exact-contrast design benchmark is therefore implementable but
likely very imprecise. Pooling or marginalizing contrasts would define a
different estimand and must not be done silently.

## Execution environment

The current system R is 4.0.5 and lacks `torch` and `testthat`. The legacy
replication lockfile targets R 4.5.3 and `torch` 0.16.3 but is not a complete
environment for this branch. Before producing application estimates, create a
local reproducible environment under this clone, install the package and its
QMC/testing dependencies, and run the full Torch and `testthat` suites. This is
an execution prerequisite, not a reason to write into the source project.
