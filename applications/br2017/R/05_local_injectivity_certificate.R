#!/usr/bin/env Rscript

## Fitted-point local covariance-injectivity certificates for br2017.
##
## For a p by q full-column-rank loading A, the rank-q covariance is
## Sigma = A A'.  A loading perturbation B has covariance tangent
##
##                 d Sigma[B] = A B' + B A'.
##
## For a realized contrast d, the derivative of d' Sigma d is
##
##              2 (d' A) (d' B)'.
##
## Stacking these derivatives over the unique realized contrasts gives a
## Jacobian with p*q columns.  Its unavoidable kernel consists of loading
## rotations B = A Omega, with Omega skew-symmetric, and has dimension
## q*(q-1)/2.  Thus the covariance is locally injective over the rank-q PSD
## manifold at A if this Jacobian has rank
##
##                    p*q - q*(q-1)/2.
##
## This generalizes the rank-one gs2020 certificate.  It diagnoses local
## identification of the covariance at the supplied fitted loading.  It does
## not establish global injectivity, estimator convergence, or finite-sample
## precision.
##
## The saved v2.1 fit loadings and assembled contrasts are already in raw
## contrast units.  Do not divide A by a saved preprocessing scale.
##
## Usage from any directory:
##   SCONJOINT_BR_ARTIFACT_DIR=<directory containing the 14 v2.1 files> \
##   SCONJOINT_CERTIFICATE_OUTPUT_DIR=<optional output directory> \
##     Rscript applications/br2017/R/05_local_injectivity_certificate.R
##
## By default q = 1,2 and the relative SVD tolerance is 1e-10.  Override with
## SCONJOINT_CERTIFICATE_Q (comma-separated) and SCONJOINT_RANK_TOL.

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
default_artifact_dir <- file.path(
  root, "applications", "br2017", "results", "mixed_logit", "v21_corrected")
artifact_dir <- Sys.getenv("SCONJOINT_BR_ARTIFACT_DIR",
                           unset = default_artifact_dir)
artifact_dir <- normalizePath(artifact_dir, mustWork = TRUE)

q_text <- Sys.getenv("SCONJOINT_CERTIFICATE_Q", unset = "1,2")
q_values <- suppressWarnings(as.integer(strsplit(q_text, ",", fixed = TRUE)[[1L]]))
q_values <- unique(q_values)
if (!length(q_values) || anyNA(q_values) || any(q_values < 1L)) {
  stop("SCONJOINT_CERTIFICATE_Q must contain positive comma-separated integers.",
       call. = FALSE)
}

rank_tol <- suppressWarnings(as.numeric(Sys.getenv(
  "SCONJOINT_RANK_TOL", unset = "1e-10")))
if (length(rank_tol) != 1L || is.na(rank_tol) || !is.finite(rank_tol) ||
    rank_tol <= 0 || rank_tol >= 1) {
  stop("SCONJOINT_RANK_TOL must be one finite number strictly between 0 and 1.",
       call. = FALSE)
}

matrix_spectrum <- function(M, relative_tolerance) {
  M <- as.matrix(M)
  singular <- svd(M, nu = 0L, nv = 0L)$d
  largest <- if (length(singular)) singular[[1L]] else 0
  threshold <- relative_tolerance * largest
  rank <- if (largest == 0) 0L else sum(singular > threshold)
  smallest_nonzero <- if (rank > 0L) singular[[rank]] else NA_real_
  list(
    singular_values = singular,
    threshold = threshold,
    rank = as.integer(rank),
    condition_nonzero = if (rank > 0L) largest / smallest_nonzero else Inf,
    smallest_nonzero = smallest_nonzero,
    largest = largest,
    first_excluded = if (rank < length(singular)) singular[[rank + 1L]] else NA_real_
  )
}

quadratic_design <- function(D) {
  p <- ncol(D)
  pairs <- which(upper.tri(matrix(FALSE, p, p), diag = TRUE), arr.ind = TRUE)
  out <- vapply(seq_len(nrow(pairs)), function(h) {
    j <- pairs[h, 1L]
    k <- pairs[h, 2L]
    if (j == k) D[, j]^2 else 2 * D[, j] * D[, k]
  }, numeric(nrow(D)))
  dim(out) <- c(nrow(D), nrow(pairs))
  colnames(out) <- vapply(seq_len(nrow(pairs)), function(h) {
    paste0(colnames(D)[pairs[h, 1L]], "__", colnames(D)[pairs[h, 2L]])
  }, character(1L))
  out
}

rotation_null_residual <- function(J, A) {
  q <- ncol(A)
  if (q < 2L) return(0)
  residual <- numeric(q * (q - 1L) / 2L)
  h <- 0L
  for (r in seq_len(q - 1L)) {
    for (s in (r + 1L):q) {
      h <- h + 1L
      omega <- matrix(0, q, q)
      omega[r, s] <- 1
      omega[s, r] <- -1
      residual[h] <- max(abs(as.numeric(J %*% as.numeric(A %*% omega))))
    }
  }
  max(residual)
}

assembled_path <- file.path(artifact_dir, "fit_primary_assembled.rds")
if (!file.exists(assembled_path)) {
  stop("Missing assembled fit: ", assembled_path, call. = FALSE)
}
assembled <- readRDS(assembled_path)
D_all <- as.matrix(assembled$deltaX)
if (!is.numeric(D_all) || any(!is.finite(D_all)) || nrow(D_all) < 1L ||
    ncol(D_all) < 1L) {
  stop("The assembled fit does not contain a finite nonempty contrast matrix.",
       call. = FALSE)
}
if (is.null(colnames(D_all))) {
  colnames(D_all) <- paste0("x", seq_len(ncol(D_all)))
}
D <- unique(D_all)
p <- ncol(D)
if (any(q_values > p)) {
  stop("Every requested q must be no larger than p = ", p, ".", call. = FALSE)
}

linear_spec <- matrix_spectrum(D, rank_tol)
Q <- quadratic_design(D)
quadratic_spec <- matrix_spectrum(Q, rank_tol)

certificates <- vector("list", length(q_values))
names(certificates) <- paste0("q", q_values)
summary_rows <- vector("list", length(q_values))

for (h in seq_along(q_values)) {
  q <- q_values[[h]]
  fit_path <- file.path(artifact_dir, sprintf("fit_q%d_full.rds", q))
  if (!file.exists(fit_path)) stop("Missing q = ", q, " fit: ", fit_path,
                                   call. = FALSE)
  wrapper <- readRDS(fit_path)
  fit <- if (inherits(wrapper, "scmix_tuning")) wrapper$refit else wrapper
  A <- as.matrix(fit$A)
  if (!is.numeric(A) || any(!is.finite(A)) ||
      !identical(dim(A), c(p, q))) {
    stop("The q = ", q, " loading must be a finite ", p, " by ", q,
         " matrix; got ", paste(dim(A), collapse = " by "), ".",
         call. = FALSE)
  }
  if (!is.null(fit$raw_data$deltaX) &&
      !isTRUE(all.equal(as.matrix(fit$raw_data$deltaX), D_all,
                        tolerance = 0, check.attributes = FALSE))) {
    stop("The q = ", q, " fit and assembled artifact use different contrasts.",
         call. = FALSE)
  }
  if (!is.null(fit$Sigma) &&
      max(abs(as.matrix(fit$Sigma) - tcrossprod(A))) > 1e-12) {
    stop("The q = ", q, " stored covariance is not A %*% t(A).",
         call. = FALSE)
  }
  rownames(A) <- colnames(D)

  A_spec <- matrix_spectrum(A, rank_tol)
  projected <- D %*% A
  J <- do.call(cbind, lapply(seq_len(q), function(k) {
    2 * projected[, k] * D
  }))
  colnames(J) <- unlist(lapply(seq_len(q), function(k) {
    paste0("B[", colnames(D), ",", k, "]")
  }), use.names = FALSE)
  tangent_spec <- matrix_spectrum(J, rank_tol)

  loading_parameter_dim <- p * q
  rotation_dim <- q * (q - 1L) / 2L
  covariance_tangent_dim <- loading_parameter_dim - rotation_dim
  jacobian_nullity <- ncol(J) - tangent_spec$rank
  established <- A_spec$rank == q &&
    tangent_spec$rank == covariance_tangent_dim &&
    jacobian_nullity == rotation_dim
  projected_norm <- sqrt(rowSums(projected^2))
  max_projected_norm <- max(projected_norm)
  rotation_residual <- rotation_null_residual(J, A)

  provenance <- paste0(
    "applications/br2017/R/05_local_injectivity_certificate.R; assembled=",
    basename(assembled_path), " (md5 ", unname(tools::md5sum(assembled_path)),
    "); fit=", basename(fit_path), " (md5 ",
    unname(tools::md5sum(fit_path)), "); raw contrast scale")
  certificate <- list(
    established = established,
    verified = TRUE,
    method = paste0(
      "SVD rank of the fitted rank-", q,
      " covariance-tangent Jacobian, quotienting loading rotations"),
    domain = paste0(
      "local rank-", q,
      " positive-semidefinite covariance manifold at the fitted raw-scale loading"),
    provenance = provenance,
    tolerance = rank_tol,
    q = as.integer(q),
    contrasts = D,
    loading = A,
    dimensions = list(
      n_unique_realized_contrasts = nrow(D),
      p = p,
      loading_parameter_dim = loading_parameter_dim,
      rotation_dim = rotation_dim,
      covariance_tangent_dim = covariance_tangent_dim,
      jacobian_rows = nrow(J),
      jacobian_columns = ncol(J)),
    ranks = list(
      loading = A_spec$rank,
      linear_design = linear_spec$rank,
      unrestricted_quadratic_design = quadratic_spec$rank,
      covariance_tangent_jacobian = tangent_spec$rank,
      jacobian_nullity = jacobian_nullity,
      extra_nullity_beyond_rotations = jacobian_nullity - rotation_dim),
    diagnostics = list(
      tangent_condition_nonzero = tangent_spec$condition_nonzero,
      tangent_largest_singular_value = tangent_spec$largest,
      tangent_smallest_nonzero_singular_value = tangent_spec$smallest_nonzero,
      tangent_first_excluded_singular_value = tangent_spec$first_excluded,
      rotation_null_max_absolute_residual = rotation_residual,
      n_exactly_orthogonal_contrasts = sum(projected_norm == 0),
      min_projected_loading_norm = min(projected_norm),
      median_projected_loading_norm = stats::median(projected_norm),
      fraction_below_1e3_of_max = if (max_projected_norm > 0)
        mean(projected_norm < 1e-3 * max_projected_norm) else 1,
      unrestricted_quadratic_condition = quadratic_spec$condition_nonzero),
    disclaimer = paste(
      "This fitted-point derivative-rank certificate establishes only local",
      "injectivity of the rank-constrained covariance measurement map.",
      "It does not establish global injectivity, numerical convergence,",
      "or precise finite-sample inference."))
  class(certificate) <- c("scmix_covariance_injectivity_certificate", "list")
  certificates[[h]] <- certificate

  summary_rows[[h]] <- data.frame(
    q = q,
    n_unique_contrasts = nrow(D),
    p = p,
    loading_rank = A_spec$rank,
    loading_parameter_dim = loading_parameter_dim,
    rotation_dim = rotation_dim,
    covariance_tangent_dim = covariance_tangent_dim,
    jacobian_rows = nrow(J),
    jacobian_columns = ncol(J),
    jacobian_rank = tangent_spec$rank,
    jacobian_nullity = jacobian_nullity,
    extra_nullity_beyond_rotations = jacobian_nullity - rotation_dim,
    tangent_condition_nonzero = tangent_spec$condition_nonzero,
    tangent_smallest_nonzero_singular_value = tangent_spec$smallest_nonzero,
    tangent_first_excluded_singular_value = tangent_spec$first_excluded,
    rotation_null_max_absolute_residual = rotation_residual,
    n_exactly_orthogonal_contrasts = sum(projected_norm == 0),
    min_projected_loading_norm = min(projected_norm),
    median_projected_loading_norm = stats::median(projected_norm),
    established = established,
    stringsAsFactors = FALSE)
}

summary <- do.call(rbind, summary_rows)
rownames(summary) <- NULL

cat("Ballard-Rosa fitted-point local covariance-injectivity certificate\n")
cat("raw-scale unique realized contrast matrix:", nrow(D), "by", p, "\n")
cat("linear design rank:", linear_spec$rank, "/", p, "\n")
cat("unrestricted quadratic design rank:", quadratic_spec$rank, "/",
    ncol(Q), "\n")
print(summary, digits = 8, row.names = FALSE)

output_dir <- Sys.getenv("SCONJOINT_CERTIFICATE_OUTPUT_DIR", unset = "")
if (nzchar(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  for (h in seq_along(q_values)) {
    saveRDS(
      certificates[[h]],
      file.path(output_dir, sprintf("br2017_local_injectivity_q%d.rds",
                                    q_values[[h]])),
      version = 3)
  }
  summary_path <- file.path(output_dir,
                            "br2017_local_injectivity_summary.csv")
  write.csv(summary, summary_path, row.names = FALSE)
  cat("written:", normalizePath(output_dir, mustWork = TRUE), "\n")
} else {
  cat("No files written. Set SCONJOINT_CERTIFICATE_OUTPUT_DIR to save the",
      "classed RDS certificates and CSV summary.\n")
}

if (!all(summary$established)) {
  stop("At least one requested fitted-point local-injectivity certificate failed.",
       call. = FALSE)
}
cat("DONE\n")
