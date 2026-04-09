## Lambda(Z) estimation for sconjoint (M3).
##
## Ports `05_lambda_estimation.R` from the prototype.  For each
## upper-triangle (j, k) pair with j <= k, we regress the product
## g'(DeltaX' beta(Z)) * DeltaX_j * DeltaX_k on a Z design matrix
## (with intercept and ridge penalty), and store the fitted values
## per observation.  Per-observation Lambda(Z_i) is reconstructed
## on demand and inverted via ridge-SVD in `.sc_reconstruct_lambda_inv()`.

#' Reconstruct Lambda^{-1}(Z_i) from an upper-triangle vector
#'
#' Given an upper-triangle vector of length `p*(p+1)/2`, rebuild the
#' symmetric `p x p` matrix, add a ridge penalty on the diagonal, and
#' invert via SVD (truncating singular values below `svd_tol`).
#'
#' @param lambda_vec Numeric vector of length `p*(p+1)/2`.
#' @param p Integer, matrix dimension.
#' @param ridge Non-negative numeric, ridge penalty on the diagonal.
#' @param svd_tol Non-negative numeric, singular-value truncation
#'   threshold.  Mirrors the prototype's `1e-8`.
#' @return A numeric `p x p` matrix.
#' @keywords internal
#' @noRd
.sc_reconstruct_lambda_inv <- function(lambda_vec, p,
                                       ridge = 1e-4,
                                       svd_tol = 1e-8) {
  Lambda <- matrix(0, p, p)
  Lambda[upper.tri(Lambda, diag = TRUE)] <- lambda_vec
  Lambda <- Lambda + t(Lambda) - diag(diag(Lambda))

  Lambda_reg <- Lambda + ridge * diag(p)
  sv <- svd(Lambda_reg)
  d_inv <- ifelse(sv$d > svd_tol, 1 / sv$d, 0)
  sv$v %*% (d_inv * t(sv$u))
}

#' Estimate Lambda(Z) via element-wise ridge regression on Z
#'
#' For each upper-triangle pair (j, k) with `j <= k`, regress
#' `g' * DeltaX_j * DeltaX_k` on `cbind(1, Z)` with ridge penalty
#' `ridge_lambda`.  The fitted values are stored so that per-
#' observation Lambda matrices can be reconstructed downstream.
#'
#' @param beta_hat Numeric N x p matrix of out-of-sample beta(Z)
#'   predictions from cross-fitting.
#' @param deltaX Numeric N x p matrix of per-task attribute differences.
#' @param Z Numeric N x p_Z matrix of respondent moderators.
#' @param ridge_lambda Non-negative numeric, ridge penalty used both in
#'   the Z regression and in the per-observation Lambda inversion.
#' @return A list with:
#'   * `fitted` -- N x `p*(p+1)/2` matrix of fitted Lambda values
#'     (upper triangle in column-major `which(upper.tri(..., diag=T))`
#'     order);
#'   * `jk` -- `p*(p+1)/2` x 2 integer matrix of (j, k) indices;
#'   * `p` -- integer dimension of the Lambda matrix;
#'   * `ridge` -- ridge penalty used;
#'   * `prob_hat` -- N-vector of plug-in predicted probabilities
#'     `plogis(rowSums(deltaX * beta_hat))` (stored so downstream
#'     inference can reuse it without re-computation);
#'   * `g_prime` -- N-vector of `prob_hat * (1 - prob_hat)`.
#' @keywords internal
#' @noRd
.sc_estimate_lambda <- function(beta_hat, deltaX, Z, ridge_lambda = 1e-4) {
  if (!is.matrix(beta_hat) || !is.numeric(beta_hat)) {
    stop(".sc_estimate_lambda(): `beta_hat` must be a numeric matrix.")
  }
  if (!is.matrix(deltaX) || !is.numeric(deltaX)) {
    stop(".sc_estimate_lambda(): `deltaX` must be a numeric matrix.")
  }
  if (!is.matrix(Z) || !is.numeric(Z)) {
    stop(".sc_estimate_lambda(): `Z` must be a numeric matrix.")
  }
  n <- nrow(deltaX)
  p <- ncol(deltaX)
  if (nrow(beta_hat) != n || ncol(beta_hat) != p || nrow(Z) != n) {
    stop(".sc_estimate_lambda(): row/column dimensions disagree.")
  }
  if (!is.numeric(ridge_lambda) || length(ridge_lambda) != 1L || ridge_lambda < 0) {
    stop(".sc_estimate_lambda(): `ridge_lambda` must be a non-negative scalar.")
  }

  ## --- Plug-in prob / logistic density ---
  logit_index <- rowSums(deltaX * beta_hat)
  prob_hat    <- stats::plogis(logit_index)
  g_prime     <- prob_hat * (1 - prob_hat)

  ## --- Upper-triangle (j, k) pairs ---
  jk <- which(upper.tri(diag(p), diag = TRUE), arr.ind = TRUE)
  n_pairs <- nrow(jk)

  ## --- Raw products g' * DeltaX_j * DeltaX_k (N x n_pairs) ---
  lambda_products <- matrix(NA_real_, nrow = n, ncol = n_pairs)
  for (idx in seq_len(n_pairs)) {
    j <- jk[idx, 1L]
    k <- jk[idx, 2L]
    lambda_products[, idx] <- g_prime * deltaX[, j] * deltaX[, k]
  }

  ## --- Ridge regression on Z with intercept ---
  Z_int <- cbind(1, Z)
  p_z1  <- ncol(Z_int)
  ZtZ_inv <- solve(crossprod(Z_int) + ridge_lambda * diag(p_z1))
  ## Hat projection: fitted = Z_int %*% (ZtZ_inv %*% Z_int' %*% y).
  ## Vectorize across all n_pairs outcomes simultaneously.
  gamma_mat    <- ZtZ_inv %*% crossprod(Z_int, lambda_products)  # p_z1 x n_pairs
  lambda_fit   <- Z_int %*% gamma_mat                             # N x n_pairs

  list(
    fitted   = lambda_fit,
    jk       = jk,
    p        = p,
    ridge    = ridge_lambda,
    prob_hat = prob_hat,
    g_prime  = g_prime
  )
}
