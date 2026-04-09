## DML inference for sconjoint (M3).
##
## Ports `06_inference.R` from the prototype, with one upgrade: the
## prototype stored only the diagonal of the clustered variance; here
## we form the full `p x p` clustered variance-covariance matrix so
## that `vcov.sc_fit()` works and downstream subgroup / joint-test
## quantities can be computed.
##
## Notation mirrors spec.md section 6.5:
##
##   psi_i^raw = beta_hat(Z_i) + Lambda^{-1}(Z_i) * DeltaX_i * (Y_i - G_i)
##   theta_hat = colMeans(psi_raw)
##   psi_c     = psi_raw - theta_hat  (centered)
##   V_cluster = (M/(M-1)) * crossprod(cluster_sums) / N^2
##
## where `cluster_sums[m, ]` is the within-respondent sum of `psi_c`.

#' Compute the DML influence function and point estimates
#'
#' @param beta_hat Numeric N x p matrix of out-of-sample beta(Z).
#' @param lambda_obj List returned by `.sc_estimate_lambda()`.
#' @param deltaX Numeric N x p matrix of per-task attribute differences.
#' @param y Numeric vector of length N with 0/1 choice outcomes.
#' @return A list with:
#'   * `theta_hat` -- numeric p-vector of DML point estimates;
#'   * `plugin` -- numeric p-vector of plug-in estimates
#'     `colMeans(beta_hat)`;
#'   * `correction` -- numeric N x p matrix of DML correction terms;
#'   * `influence_raw` -- numeric N x p matrix `beta_hat + correction`.
#' @keywords internal
#' @noRd
.sc_influence_function <- function(beta_hat, lambda_obj, deltaX, y) {
  if (!is.list(lambda_obj) ||
      !all(c("fitted", "p", "ridge", "prob_hat") %in% names(lambda_obj))) {
    stop(".sc_influence_function(): `lambda_obj` missing required fields.")
  }
  n <- nrow(beta_hat)
  p <- ncol(beta_hat)
  if (nrow(deltaX) != n || ncol(deltaX) != p || length(y) != n) {
    stop(".sc_influence_function(): dimension mismatch.")
  }
  if (lambda_obj$p != p || nrow(lambda_obj$fitted) != n) {
    stop(".sc_influence_function(): Lambda object shape mismatches (N, p).")
  }

  prob_hat <- lambda_obj$prob_hat
  resid    <- as.numeric(y) - prob_hat
  dx_resid <- deltaX * resid

  ridge <- lambda_obj$ridge
  lambda_fit <- lambda_obj$fitted

  correction <- matrix(0, nrow = n, ncol = p)

  ## Chunked loop over observations for manageable peak memory on
  ## large N.  Chunk size of 1000 mirrors the prototype.
  chunk_size <- 1000L
  n_chunks   <- ceiling(n / chunk_size)
  for (ch in seq_len(n_chunks)) {
    i0 <- (ch - 1L) * chunk_size + 1L
    i1 <- min(ch * chunk_size, n)
    for (i in i0:i1) {
      Lambda_inv_i  <- .sc_reconstruct_lambda_inv(lambda_fit[i, ],
                                                  p = p,
                                                  ridge = ridge)
      correction[i, ] <- Lambda_inv_i %*% dx_resid[i, ]
    }
  }

  influence_raw <- beta_hat + correction
  theta_hat     <- colMeans(influence_raw)
  plugin        <- colMeans(beta_hat)

  list(
    theta_hat     = theta_hat,
    plugin        = plugin,
    correction    = correction,
    influence_raw = influence_raw
  )
}

#' Respondent-clustered variance-covariance of theta_hat
#'
#' Computes the full `p x p` clustered variance of the DML estimator
#' using the cluster-crossproduct formula from spec.md section 6.5:
#'
#'   V_cluster = (M / (M-1)) * crossprod(cluster_sums) / N^2
#'
#' where `cluster_sums[m, ]` is the sum of the centered influence
#' contributions of respondent `m` across all of that respondent's
#' tasks.
#'
#' @param influence_raw Numeric N x p matrix of raw influence values.
#' @param theta_hat Numeric p-vector of point estimates.
#' @param respondent_id Vector of length N giving the respondent of
#'   each row.
#' @return A list with:
#'   * `vcov` -- numeric p x p clustered variance-covariance matrix;
#'   * `se` -- numeric p-vector of clustered standard errors
#'     (sqrt of the diagonal);
#'   * `M` -- integer number of unique respondents (clusters).
#' @keywords internal
#' @noRd
.sc_cluster_vcov <- function(influence_raw, theta_hat, respondent_id) {
  if (!is.matrix(influence_raw) || !is.numeric(influence_raw)) {
    stop(".sc_cluster_vcov(): `influence_raw` must be a numeric matrix.")
  }
  n <- nrow(influence_raw)
  p <- ncol(influence_raw)
  if (length(theta_hat) != p) {
    stop(".sc_cluster_vcov(): `theta_hat` length disagrees with ncol(influence_raw).")
  }
  if (length(respondent_id) != n) {
    stop(".sc_cluster_vcov(): `respondent_id` length disagrees with nrow(influence_raw).")
  }

  infl_c <- sweep(influence_raw, 2L, theta_hat, check.margin = FALSE)
  ## Sort respondent ids for deterministic ordering
  key <- as.character(respondent_id)
  uniq <- sort(unique(key))
  M <- length(uniq)
  if (M < 2L) {
    stop(".sc_cluster_vcov(): at least 2 clusters are required.")
  }

  cluster_sums <- matrix(0, nrow = M, ncol = p)
  idx_by_resp  <- split(seq_len(n), factor(key, levels = uniq))
  for (m in seq_len(M)) {
    rows <- idx_by_resp[[m]]
    cluster_sums[m, ] <- colSums(infl_c[rows, , drop = FALSE])
  }

  dfc <- M / (M - 1)
  vcov_mat <- dfc * crossprod(cluster_sums) / (n * n)
  se <- sqrt(pmax(diag(vcov_mat), 0))

  list(vcov = vcov_mat, se = se, M = M)
}

#' Un-clustered ("iid") variance of theta_hat, for the DML/iid diagnostic
#'
#' Treats every observation as an independent draw.  Returns the full
#' `p x p` matrix (not just the diagonal) so that `vcov(fit, cluster =
#' "iid")` can be served in M4.
#'
#' @param influence_raw Numeric N x p matrix of raw influence values.
#' @param theta_hat Numeric p-vector of point estimates.
#' @return A list with `vcov` (p x p) and `se` (p-vector).
#' @keywords internal
#' @noRd
.sc_iid_vcov <- function(influence_raw, theta_hat) {
  n <- nrow(influence_raw)
  infl_c <- sweep(influence_raw, 2L, theta_hat, check.margin = FALSE)
  vcov_mat <- crossprod(infl_c) / (n * n)
  se <- sqrt(pmax(diag(vcov_mat), 0))
  list(vcov = vcov_mat, se = se)
}

#' Ratio of DML clustered SE to iid SE
#'
#' A scalar diagnostic used by `summary.sc_fit()` (in M4) to highlight
#' the inflation due to within-respondent correlation.  Values >> 1
#' indicate that treating the panel as iid would understate uncertainty.
#'
#' @param vcov_cluster,vcov_iid Numeric p x p matrices.
#' @return A list with `per_param` (p-vector of ratios) and `mean`
#'   (scalar mean ratio).
#' @keywords internal
#' @noRd
.sc_dml_iid_ratio <- function(vcov_cluster, vcov_iid) {
  se_c <- sqrt(pmax(diag(vcov_cluster), 0))
  se_i <- sqrt(pmax(diag(vcov_iid), 0))
  ratio <- ifelse(se_i > 0, se_c / se_i, NA_real_)
  list(per_param = ratio, mean = mean(ratio, na.rm = TRUE))
}
