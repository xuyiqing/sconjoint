#!/usr/bin/env Rscript

## Design-based benchmark for Saha--Weeks (decision 2, 2026-08-24): the
## old-draft comparison --- female-vs-male AMCE by party from a linear
## probability model on the task contrasts with respondent-clustered SEs ---
## NOT paperps section 4.1's exact-contrast U_i(d) estimator (largest exact
## cell is 15, so that estimator is unbiased but nearly uninformative).
##
## Reproduces Avidit's memo values exactly: +7.49 (1.91) Democrats,
## +2.73 (2.01) Independents, -3.57 (2.04) Republicans.

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}
app_root <- normalizePath(file.path(dirname(script_path()), ".."),
                          mustWork = TRUE)
prepared <- readRDS(file.path(app_root, "results", "prep_analysis_data.rds"))

DX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
party <- prepared$respondent_meta$party[
  match(prepared$respondent_id, prepared$respondent_meta$respondent_id)]
rid <- as.character(prepared$respondent_id)

rows <- lapply(c("Democrat", "Independent", "Republican (GOP)"), function(pt) {
  keep <- party == pt
  X <- cbind(1, DX[keep, , drop = FALSE])
  yy <- y[keep]; rr <- rid[keep]
  XtXi <- solve(crossprod(X))
  b <- XtXi %*% crossprod(X, yy)
  u <- as.numeric(yy - X %*% b)
  cl <- lapply(split(seq_along(rr), rr), function(ii) {
    tcrossprod(colSums(X[ii, , drop = FALSE] * u[ii]))
  })
  V <- XtXi %*% Reduce(`+`, cl) %*% XtXi
  j <- 1 + which(colnames(DX) == "cand_genderMale")
  data.frame(party = pt,
             n_respondents = length(unique(rr)),
             female_vs_male_amce_pp = round(-100 * b[j], 2),
             se_pp = round(100 * sqrt(V[j, j]), 2))
})
out <- do.call(rbind, rows)
dir.create(file.path(app_root, "tables"), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(out, file.path(app_root, "tables", "amce_by_party.csv"),
                 row.names = FALSE)
print(out, row.names = FALSE)
