## Reproduction + regression test for sb_fitted_dispersion() shape handling.
SRC <- local({ a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", grep("^--file=", a, value = TRUE)); d <- if (length(f)) dirname(f) else "applications/R"; file.path(d, "share_bounds.R") })
source(SRC)

ok <- TRUE
chk <- function(label, pass, detail) {
  cat(if (pass) "PASS  " else "FAIL  ", label, " -- ", detail, "\n", sep = "")
  if (!pass) ok <<- FALSE
}

mk <- function(A_list) list(K = length(A_list), A_folds = A_list,
                            sd_dx_folds = rep(list(1), length(A_list)))

## --- Case 1: ONE coordinate, K = 3 folds (the defect) ---------------
f1 <- mk(list(matrix(0.5, 1, 1), matrix(0.7, 1, 1), matrix(0.9, 1, 1)))
d1 <- sb_fitted_dispersion(f1)
chk("p=1, K=3 returns one dispersion",
    length(d1) == 1L, paste0("length = ", length(d1), " (expected 1)"))
chk("p=1, K=3 value is the fold mean",
    length(d1) == 1L && isTRUE(all.equal(d1[[1]], 0.7)),
    paste0("got ", paste(signif(d1, 6), collapse = ", "), " (expected 0.7)"))

## --- Case 2: one coordinate, q = 2 (norm across columns) -------------
f2 <- mk(list(matrix(c(3, 4), 1, 2), matrix(c(6, 8), 1, 2)))
d2 <- sb_fitted_dispersion(f2)
chk("p=1, q=2, K=2 returns one dispersion",
    length(d2) == 1L && isTRUE(all.equal(d2[[1]], 7.5)),
    paste0("got ", paste(signif(d2, 6), collapse = ", "), " (expected 7.5)"))

## --- Case 3: K = 1 single fold, one coordinate ----------------------
f3 <- mk(list(matrix(0.6, 1, 1)))
d3 <- sb_fitted_dispersion(f3)
chk("p=1, K=1 returns one dispersion",
    length(d3) == 1L && isTRUE(all.equal(d3[[1]], 0.6)),
    paste0("got ", paste(signif(d3, 6), collapse = ", ")))

## --- Case 4: REGRESSION, multi-coordinate must be untouched ---------
set.seed(11)
A_list <- lapply(1:3, function(k) matrix(rnorm(4 * 2), 4, 2))
f4 <- mk(A_list)
d4 <- sb_fitted_dispersion(f4)
expect4 <- rowMeans(vapply(A_list, function(A) sqrt(rowSums(A^2)), numeric(4)))
chk("p=4 unchanged (length)", length(d4) == 4L, paste0("length = ", length(d4)))
chk("p=4 unchanged (values)", isTRUE(all.equal(unname(d4), unname(expect4))),
    paste0("max dev = ", signif(max(abs(d4 - expect4)), 3)))

## --- Case 5: per-coordinate sd_dx scaling still applied -------------
f5 <- list(K = 2, A_folds = list(matrix(c(2, 4), 2, 1), matrix(c(2, 4), 2, 1)),
           sd_dx_folds = list(c(2, 4), c(2, 4)))
d5 <- sb_fitted_dispersion(f5)
chk("raw-scale division preserved", isTRUE(all.equal(unname(d5), c(1, 1))),
    paste0("got ", paste(signif(d5, 6), collapse = ", "), " (expected 1, 1)"))

cat("\n", if (ok) "ALL PASS" else "FAILURES PRESENT", "\n", sep = "")
quit(status = if (ok) 0L else 1L)
