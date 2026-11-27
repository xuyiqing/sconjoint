# data-raw/

Scripts that build the bundled data from the published replication files and
the paper's saved inputs. Run each from the package root with `Rscript`. This
folder is excluded from the package build (`.Rbuildignore`).

| Script | Builds | Reads (read only) |
|---|---|---|
| `build_gs2020.R` | `data/gs2020.rda` | Graham and Svolik (2020) `data_experiment.csv`, `data_covariates.csv`, `data_howDemocratic.csv` (`SCONJOINT_GS_RAW`) |
| `build_br2017.R` | `data/br2017.rda` | Ballard-Rosa, Martin and Scheve (2017) `us_fullsurvey_taxplan_level.dta` (`SCONJOINT_BR_RAW`) |
| `build_sim.R` | `data/simconjoint.rda`, `data/simconjoint_truth.rda` | nothing: simulated from the package's model with a fixed seed; computes the exact population values |
| `build_sysdata.R` | `R/sysdata.rda` | the paper's `G_inputs.rds` (folds) and fold fit files (validation respondents) (`SCONJOINT_PAPER_ROOT`) |

The defaults point at the paper project's folder
(`~/Dropbox/Research_Hub/Projects/ConjointStructural`). Each data script
checks that every previously bundled column is reproduced exactly before it
saves.

`R/sysdata.rda` holds the paper's five cross-fitting folds and, for each
fold, the respondents held out of training for early stopping. Neither can be
derived from the data (they were drawn once with seeds whose code is not part
of the rebuild), so they are shipped as vectors. `sc_example_data()` reads
them.
