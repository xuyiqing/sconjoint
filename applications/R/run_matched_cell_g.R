#!/usr/bin/env Rscript
## One cell of the matched null calibration, as its own process.
##
## Usage: run_matched_cell.R <r> <k_index> <outdir> [gate]
##   r        replication index (1..R); ignored when `gate` is passed
##   k_index  assembled fold id (1..outer_K)
##   outdir   directory for the per-cell .rds checkpoint
##   gate     literal "gate" runs the cell on the REAL outcome with the
##            analysis's own seed, so its selection and loading must
##            reproduce the stored analysis refit for that fold
##
## Separate processes rather than forked workers: torch does not survive
## forking cleanly, and a per-cell checkpoint makes a partial overnight run
## usable instead of wasted.
options(stringsAsFactors = FALSE, warn = 1)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 3L) stop("Usage: run_matched_cell.R <r> <k_index> <outdir> [gate]")
r <- as.integer(a[[1]]); k_index <- as.integer(a[[2]]); outdir <- a[[3]]
is_gate <- length(a) >= 4L && identical(a[[4]], "gate")

root <- normalizePath(Sys.getenv("SCONJOINT_ROOT", unset = path.expand("~/GitHub/sconjoint")), mustWork = TRUE)
Sys.setenv(OMP_NUM_THREADS = Sys.getenv("SB_THREADS", "2"),
           MKL_NUM_THREADS = Sys.getenv("SB_THREADS", "2"))
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
try(torch::torch_set_num_threads(as.integer(Sys.getenv("SB_THREADS", "2"))),
    silent = TRUE)

options(sconjoint.gs_application_root = file.path(root, "applications/gs2020"))
source(file.path(root, "applications/gs2020/config/analysis_config.R"))
cfg <- gs_analysis_config
source(file.path(root, "applications/R/share_bounds.R"))
source(file.path(root, "applications/R/share_bounds_matched.R"))

PROFILE <- "v21_corrected"
GRID_KEEP <- c("constant", "relu4_wd1e-01")
## Arm G: as arm F but a SINGLE optimizer start, matching what
## .sc_train_mixed_one does in sb_zero_floor(). Holding the grid, epochs and
## the refit path fixed, this isolates MULTI-START from the pooled-prefit path.
STARTS_OVERRIDE <- 1L
Q <- 1L
resdir <- file.path(root, "applications/gs2020/results/mixed_logit", PROFILE)
assembled <- readRDS(file.path(resdir, "fit_primary_assembled.rds"))
nested <- readRDS(file.path(resdir, "fit_primary_nested.rds"))
profile <- cfg$profiles[[PROFILE]]
## Arm F: minimum legal grid. A corrected mean-family candidate requires
## exactly one exact constant candidate at the same q and integration, so two
## entries is the floor. Holding starts, epochs and the pooled prefit fixed,
## this isolates SELECTION BREADTH from the rest of the fitting procedure.
profile$grid <- Filter(function(g) g$name %in% GRID_KEEP, profile$grid)
stopifnot(length(profile$grid) == 2L)
profile$n_starts <- STARTS_OVERRIDE

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
tag <- if (is_gate) sprintf("gate_k%02d", k_index) else sprintf("r%03d_k%02d", r, k_index)
outfile <- file.path(outdir, paste0("cell_", tag, ".rds"))
if (file.exists(outfile)) {
  cat("SKIP (exists):", outfile, "\n"); quit(save = "no", status = 0L)
}

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

if (is_gate) {
  ## Reproduction gate: real outcome, the analysis's own per-fold seed.
  label <- nested$outer_folds[[k_index]]
  gseed <- sconjoint:::.sc_comp_seed(cfg$optimizer$seed + 1000L * Q,
                                     "outer", label)
  msg("GATE cell: k_index", k_index, "label", label, "seed", gseed)
  cell <- sb_matched_cell(assembled, nested, profile, cfg$optimizer,
                          r = 0L, k_index = k_index, q = Q,
                          y_override = as.numeric(nested$raw_data$y),
                          gate_seed = gseed)
  ref <- nested$tuning[[k_index]]
  ref_A <- as.matrix(ref$refit$A) /
    as.numeric(ref$refit$preprocessing$deltaX$scale)
  cell$gate <- list(
    selected_match = identical(cell$selected_name, ref$selected$name),
    reference_selected = ref$selected$name,
    max_abs_dev_A = max(abs(cell$A_raw - ref_A)),
    max_abs_dev_draw = max(abs(cell$draw - sqrt(rowSums(ref_A^2)))))
  msg("GATE selected:", cell$selected_name, "| reference:", ref$selected$name,
      "| match:", cell$gate$selected_match,
      "| max|dev A|:", format(cell$gate$max_abs_dev_A, digits = 3))
} else {
  R_TOTAL <- as.integer(Sys.getenv("SB_R", "10"))
  draws <- sb_matched_null_draws(assembled, R = R_TOTAL,
                                 seed = as.integer(Sys.getenv("SB_SEED", "20260826")))
  msg("cell r", r, "k_index", k_index, "of R =", R_TOTAL)
  cell <- sb_matched_cell(assembled, nested, profile, cfg$optimizer,
                          r = r, k_index = k_index, ysim = draws$ysim[[r]],
                          q = Q,
                          seed_base = as.integer(Sys.getenv("SB_SEED", "20260826")))
  msg("selected:", cell$selected_name, "| gate:", cell$selection_gate_pass,
      "| elapsed(s):", round(cell$elapsed_sec))
}

tmp <- paste0(outfile, ".tmp")
saveRDS(cell, tmp, version = 3)
if (!file.rename(tmp, outfile)) stop("Could not write ", outfile)
msg("WROTE", basename(outfile))
cat("CELL_DONE", tag, "\n")
