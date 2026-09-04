#!/usr/bin/env Rscript
## Arm H, one cell. Decisive test for the mean-family hypothesis.
##
## sb_zero_floor() calls .sc_train_mixed_one() WITHOUT a `mean_family`
## argument, and that argument defaults to "legacy" -- the v1 mean family.
## The gs2020 analysis of record runs the `v21_corrected` profile, whose
## grid entries carry mean_family = "relu". So the null benchmark has been
## calibrating a DIFFERENT MODEL FAMILY from the analysis it certifies.
##
## This cell is sb_zero_floor()'s exact refit, with one change: the corrected
## family is passed through. Everything else -- learner, epochs, single cold
## start, fold transforms, null draws -- is held at the record's settings.
## If this reproduces the tune path's ceiling, the family is the whole story.
options(stringsAsFactors = FALSE, warn = 1)
a <- commandArgs(trailingOnly = TRUE)
r <- as.integer(a[[1]]); k <- as.integer(a[[2]]); outdir <- a[[3]]
fam <- if (length(a) >= 4L) a[[4]] else "relu"
root <- path.expand("~/GitHub/sconjoint")
Sys.setenv(OMP_NUM_THREADS = "2", MKL_NUM_THREADS = "2")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
try(torch::torch_set_num_threads(2L), silent = TRUE)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(outdir, sprintf("cell_r%03d_k%02d.rds", r, k))
if (file.exists(out)) { cat("SKIP\n"); quit(save = "no", status = 0L) }

d <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected")
asm <- readRDS(file.path(d, "fit_primary_assembled.rds"))
deltaX <- as.matrix(asm$deltaX); n <- nrow(deltaX)
fold_id <- asm$fold_id
pr <- stats::plogis(asm$kappa_folds[fold_id] + rowSums(deltaX * as.matrix(asm$mu_hat)))
## Same RNG stream as sb_zero_floor(): one set.seed, one runif per replication.
set.seed(20260826L)
ysim <- NULL
for (i in seq_len(r)) ysim <- as.numeric(stats::runif(n) < pr)

in_k <- fold_id != k
sd_dx_k <- as.numeric(asm$sd_dx_folds[[k]])
Z_train <- sconjoint:::.sc_apply_z_transform(
  as.matrix(asm$Z)[in_k, , drop = FALSE], asm$z_transform_folds[[k]])
gh_k <- if (!is.null(asm$integration_grids_folds)) asm$integration_grids_folds[[k]] else asm$gh
t0 <- proc.time()[["elapsed"]]
fk <- sconjoint:::.sc_train_mixed_one(
  deltaX = sweep(deltaX[in_k, , drop = FALSE], 2L, sd_dx_k, `/`),
  y = ysim[in_k], Z = Z_train, respondent_id = asm$respondent_id[in_k],
  gh = gh_k, hidden = 4L, mean_family = fam,
  n_epochs = 6000L, learning_rate = 0.01, weight_decay = 0.1,
  early_stop = FALSE, mu_bound = 10, kappa_bound = 10,
  seed = 20260826L + 1000L * r + k)
A_raw <- as.matrix(fk$A) / sd_dx_k
dimnames(A_raw) <- list(asm$attr_names, NULL)
cell <- list(r = r, k_index = k, fold_label = as.character(k),
             selected_name = paste0("fixed_relu4_wd1e-01_family_", fam),
             selected_index = 1L, selection_gate_pass = TRUE,
             draw = sqrt(rowSums(A_raw^2)), A_raw = A_raw, kappa = fk$kappa,
             seed = 20260826L + 1000L * r + k,
             elapsed_sec = proc.time()[["elapsed"]] - t0, is_gate_cell = FALSE)
saveRDS(cell, out, version = 3)
cat("H cell r", r, "k", k, "family", fam, "done\n")
