#!/usr/bin/env Rscript
## Arm B: the memo's recommended default --- swap the calibration learner to
## the one the analysis folds actually selected (relu4, decay 0.1) but keep it
## FIXED, i.e. still no selection inside the loop.  Same shape as the
## calibration of record (R = 10, folds 1-2, 4000 epochs, gamma 0.05) so the
## only thing that moves is the learner.
options(stringsAsFactors = FALSE, warn = 1)
root <- normalizePath(Sys.getenv("SCONJOINT_ROOT", unset = path.expand("~/GitHub/sconjoint")), mustWork = TRUE)
Sys.setenv(OMP_NUM_THREADS = Sys.getenv("SB_THREADS", "2"),
           MKL_NUM_THREADS = Sys.getenv("SB_THREADS", "2"))
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
try(torch::torch_set_num_threads(as.integer(Sys.getenv("SB_THREADS", "2"))), silent = TRUE)
source(file.path(root, "applications/R/share_bounds.R"))
resdir <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected")
assembled <- readRDS(file.path(resdir, "fit_primary_assembled.rds"))
out <- file.path(resdir, "matched_calibration", "armE_fixed_relu8_6000.rds")
if (file.exists(out)) { cat("SKIP (exists)\n"); quit(save = "no", status = 0L) }
cal <- sb_zero_floor(assembled, hidden = 8L, weight_decay = 0.1,
                     n_epochs = 6000L, R = 10L, folds_use = c(1L, 2L),
                     gamma = 0.05, seed = 20260826L, keep_loadings = TRUE)
cal$learner <- list(mode = "fixed", hidden = 8L, weight_decay = 0.1,
                    note = "epoch control: record learner at the analysis epoch budget")
saveRDS(cal, out, version = 3)
cat("ARMB_DONE\n")
