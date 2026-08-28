#!/usr/bin/env Rscript
## Local injectivity certificate for gs2020 at the fitted v2.1 loading
## (queue item 2). Construction, stated so it is reproducible this time:
##
## At rank one, Sigma = a a' and a loading tangent direction b moves the
## directional variance of realized contrast d by 2 (d'a)(d'b). Local
## injectivity of the loading block over the fielded design therefore
## requires the weighted contrast matrix
##     M = diag(w) D,   w_l = d_l' a_hat,   D = unique realized contrasts
## to have full column rank p; the certificate reports rank(M), its
## condition number sigma_1/sigma_p, the design-only rank(D) and cond(D),
## and whether any realized contrast is orthogonal to the loading
## (w_l = 0, which deletes that contrast's loading information).
##
## The 08-26 certificate at the v1-family loading (rank 30/30 over
## 20,620 unique contrasts, condition 15.3) did not preserve its script;
## the unique-contrast count anchors that the same design-side set is
## used, and the comparison of condition numbers is qualitative.

options(stringsAsFactors = FALSE, warn = 1)
root <- path.expand("~/GitHub/sconjoint")
dir <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected")

assembled <- readRDS(file.path(dir, "fit_primary_assembled.rds"))
fullw <- readRDS(file.path(dir, "fit_primary_full.rds"))
full <- if (inherits(fullw, "scmix_tuning")) fullw$refit else fullw
stopifnot(!is.null(full$A), ncol(as.matrix(full$A)) == 1L)

D <- unique(as.matrix(assembled$deltaX))
p <- ncol(D)
cat("unique realized contrasts:", nrow(D), " p:", p, "\n")

a_model <- as.numeric(as.matrix(full$A)[, 1L])
certify <- function(D, a, label) {
  w <- as.numeric(D %*% a)
  M <- w * D
  sv <- svd(M, nu = 0, nv = 0)$d
  tol <- max(dim(M)) * .Machine$double.eps * sv[1L]
  r <- sum(sv > tol)
  svD <- svd(D, nu = 0, nv = 0)$d
  rD <- sum(svD > max(dim(D)) * .Machine$double.eps * svD[1L])
  data.frame(
    scale = label, n_contrasts = nrow(D), p = p,
    rank_weighted = r, cond_weighted = sv[1L] / sv[p],
    rank_design = rD, cond_design = svD[1L] / svD[p],
    n_orthogonal_exact = sum(w == 0),
    min_abs_w = min(abs(w)), median_abs_w = stats::median(abs(w)),
    frac_below_1em3_maxw = mean(abs(w) < 1e-3 * max(abs(w))))
}

res <- certify(D, a_model, "model_scale")
## Raw-scale variant when the standardization vector is recoverable:
## columns rescale by sd_dx, the loading by 1/sd_dx, so w is IDENTICAL;
## only the column geometry (hence the condition number) changes.
sdv <- full$sd_dx
if (is.null(sdv) && !is.null(assembled$sd_dx_folds)) {
  sdv <- Reduce(`+`, lapply(assembled$sd_dx_folds, as.numeric)) /
    length(assembled$sd_dx_folds)
}
if (!is.null(sdv)) {
  sdv <- as.numeric(sdv)
  stopifnot(length(sdv) == p, all(sdv > 0))
  D_raw <- sweep(D, 2L, sdv, `*`)
  a_raw <- a_model / sdv
  res <- rbind(res, certify(D_raw, a_raw, "raw_scale_foldmean_sd"))
}
print(res, digits = 4)

out <- "/Users/xyq/Dropbox/Research_Hub/Projects/ConjointStructural/2608_issues/Yiqing/applications"
write.csv(res, file.path(out, "gs_injectivity_certificate_v21_2026-08-28.csv"),
          row.names = FALSE)
cat("written:", file.path(out, "gs_injectivity_certificate_v21_2026-08-28.csv"), "\n")
cat("DONE\n")
