#!/usr/bin/env Rscript
## Regression test for the assembled-loading scale contract.
##
## The package converts selected-refit loadings back to raw contrast units
## before storing them in both nested refits and assembled A_folds. Application
## code must not divide either object by the saved training scale a second time.

options(stringsAsFactors = FALSE, warn = 1)

root <- normalizePath(Sys.getenv(
  "SCONJOINT_ROOT", unset = path.expand("~/GitHub/sconjoint")),
  mustWork = TRUE)
resdir <- Sys.getenv("GS_V21_RESULTS", unset = file.path(
  root, "applications/gs2020/results/mixed_logit/v21_corrected"))
assembled_path <- file.path(resdir, "fit_primary_assembled.rds")
nested_path <- file.path(resdir, "fit_primary_nested.rds")
if (!file.exists(assembled_path) || !file.exists(nested_path)) {
  stop("The GS loading-scale test requires the saved assembled and nested fits ",
       "in ", resdir, ".", call. = FALSE)
}

assembled <- readRDS(assembled_path)
nested <- readRDS(nested_path)
stopifnot(assembled$K == length(nested$tuning))

deviation <- vapply(seq_len(assembled$K), function(k) {
  max(abs(as.matrix(assembled$A_folds[[k]]) -
          as.matrix(nested$tuning[[k]]$refit$A)))
}, numeric(1L))
if (max(deviation) > 1e-12) {
  stop("Assembled and selected-refit loadings disagree. Maximum deviation: ",
       signif(max(deviation), 6), ".", call. = FALSE)
}

coordinate <- "diff_respParty"
j <- match(coordinate, assembled$attr_names)
if (is.na(j)) stop("Cannot find ", coordinate, " in the saved fit.")
keep <- !duplicated(as.character(assembled$respondent_id))
m <- as.matrix(assembled$mu_hat)[keep, j]
fold <- as.integer(assembled$fold_id)[keep]
sdirection <- vapply(fold, function(k)
  abs(as.matrix(assembled$A_folds[[k]])[j, 1L]), numeric(1L))
share <- mean(ifelse(sdirection > 1e-12, stats::pnorm(m / sdirection),
                     as.numeric(m >= 0)))
double_scaled <- vapply(fold, function(k)
  abs(as.matrix(assembled$A_folds[[k]])[j, 1L] /
        as.numeric(assembled$sd_dx_folds[[k]])[j]), numeric(1L))
share_wrong <- mean(stats::pnorm(m / double_scaled))

if (abs(share - 0.8321) > 5e-4) {
  stop("Corrected co-party plug-in share is ", signif(share, 7),
       ", not approximately 0.8321.", call. = FALSE)
}
if (abs(share_wrong - 0.7462) > 5e-4 || abs(share - share_wrong) < 0.05) {
  stop("The regression fixture no longer distinguishes the corrected and ",
       "double-scaled calculations.", call. = FALSE)
}

cat("PASS  assembled A_folds equal selected-refit raw loadings\n")
cat(sprintf("PASS  corrected co-party share %.6f; double-scaled value %.6f\n",
            share, share_wrong))
