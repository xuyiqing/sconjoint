#!/usr/bin/env Rscript
## Assemble finished cells into armC_matched.rds, and report the gate.
## Safe to run repeatedly: it uses whatever complete replications exist.
options(stringsAsFactors = FALSE, warn = 1)
root <- normalizePath(Sys.getenv("SCONJOINT_ROOT", unset = path.expand("~/GitHub/sconjoint")), mustWork = TRUE)
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/share_bounds.R"))
source(file.path(root, "applications/R/share_bounds_matched.R"))
dir <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected")
mc <- file.path(dir, "matched_calibration")
asm <- readRDS(file.path(dir, "fit_primary_assembled.rds"))

files <- list.files(file.path(mc, "cells"), pattern = "^cell_.*\\.rds$",
                    full.names = TRUE)
cells <- lapply(files, readRDS)
cat("cells on disk:", length(cells), "\n")

## --- Reproduction gate -------------------------------------------------
## The matched cell machinery, handed the REAL outcome and the analysis's own
## per-fold seed, must return the analysis's selected candidate and loading.
## If it does not, the calibration is not replicating the analysis and the
## whole run is discarded.
gates <- Filter(function(x) isTRUE(x$is_gate_cell), cells)
if (length(gates)) {
  cat("\n=== REPRODUCTION GATE ===\n")
  ok <- TRUE
  for (g in gates) {
    cat(sprintf("k_index %d (label %s): selected %s | reference %s | match %s | max|dev A| %.3e | max|dev draw| %.3e\n",
                g$k_index, g$fold_label, g$selected_name,
                g$gate$reference_selected, g$gate$selected_match,
                g$gate$max_abs_dev_A, g$gate$max_abs_dev_draw))
    if (!isTRUE(g$gate$selected_match)) ok <- FALSE
  }
  cat("GATE VERDICT:", if (ok) "PASS (selection reproduced)" else
      "FAIL (selection not reproduced)", "\n")
} else {
  cat("\n(no gate cells finished yet)\n")
}

## --- Selection frequencies --------------------------------------------
main <- Filter(function(x) !isTRUE(x$is_gate_cell), cells)
if (length(main)) {
  sel <- vapply(main, `[[`, character(1L), "selected_name")
  cat("\n=== LEARNER SELECTED UNDER THE NULL (", length(sel), " cells) ===\n", sep = "")
  print(sort(table(sel), decreasing = TRUE))
  el <- vapply(main, `[[`, numeric(1L), "elapsed_sec")
  cat(sprintf("cell minutes: median %.1f  min %.1f  max %.1f\n",
              median(el)/60, min(el)/60, max(el)/60))
}

cal <- sb_assemble_matched_calibration(
  cells, asm, folds_use = c(1L, 2L), gamma = 0.05, n_epochs = 6000L,
  profile_name = "v21_corrected", keep_loadings = TRUE)
saveRDS(cal, file.path(mc, "armC_matched.rds"), version = 3)
cat("\nassembled R =", cal$R, "| dropped incomplete:",
    paste(cal$replications_dropped_incomplete, collapse = ","), "\n")

rec <- readRDS(file.path(dir, "share_bound_floor_signed.rds"))
fr <- sb_matched_floor(rec, attr_names = asm$attr_names)
fc <- cal$floor[asm$attr_names]
cat("\n=== CEILING: matched vs record ===\n")
cmp <- data.frame(coordinate = asm$attr_names,
                  record = round(as.numeric(fr), 4),
                  matched = round(as.numeric(fc), 4),
                  ratio = round(as.numeric(fc / fr), 3))
print(cmp, row.names = FALSE)
cat(sprintf("\nratio: median %.3f  min %.3f  max %.3f\n",
            median(cmp$ratio), min(cmp$ratio), max(cmp$ratio)))
cat("ASSEMBLE_DONE\n")
