## DML inference for sconjoint (M3).
##
## Ports `06_inference.R` from the prototype, with one upgrade: the
## prototype stored only the diagonal of the clustered variance; here
## we form the full `p x p` clustered variance-covariance matrix so
## that `vcov.sc_fit()` works and downstream subgroup / joint-test
## quantities can be computed.
##
## Notation mirrors the paper's Appendix B (Proposition "Average-parameter
## asymptotic normality"). The estimator and its variance are
## RESPONDENT-weighted -- the respondent is the i.i.d. sampling unit:
##
##   psi_it^raw = beta_hat(Z_i) + Lambda^{-1}(Z_i) * DeltaX_it * (Y_it - G_it)
##   phi_bar_i  = (1 / T_i) * sum_t psi_it^raw          (within-respondent mean)
##   theta_hat  = (1 / M)   * sum_i phi_bar_i           (respondent-weighted)
##   phi_i      = phi_bar_i - theta_hat                 (centered respondent contrib.)
##   V_cluster  = (1 / (M (M-1))) * sum_i phi_i phi_i^T
##            ( = (M/(M-1)) * crossprod(phi_centered) / M^2 )
##
## Optional survey weights replace the uniform respondent weights 1/M by
## normalized respondent weights a_i = w_i / sum_j w_j. They affect only the
## respondent-level target aggregation and clustered variance, not first-stage
## learner training.
##
## Under balanced T_i = T these reduce exactly to the task-weighted forms
## colMeans(psi_raw) and (M/(M-1)) * crossprod(cluster_sums) / n^2 (with
## n = sum_i T_i). Under unbalanced T_i (e.g. the Graham 2020 application,
## T in {10,...,15}) they differ, and the respondent-weighted form is the
## one the paper states and proves.

#' Compute the DML influence function and point estimates
#'
#' @param beta_hat Numeric N x p matrix of out-of-sample beta(Z).
#' @param lambda_obj List returned by `.sc_estimate_lambda()`.
#' @param deltaX Numeric N x p matrix of per-task attribute differences.
#' @param y Numeric vector of length N with 0/1 choice outcomes.
#' @param respondent_id Optional length-N vector giving each row's
#'   respondent. When supplied, `theta_hat` and `plugin` are
#'   RESPONDENT-weighted, `(1/M) sum_i (1/T_i) sum_t .`, matching the
#'   paper's Appendix B estimator. When `NULL` (legacy / balanced-only)
#'   they fall back to the task-weighted `colMeans()`; the two coincide
#'   when every respondent has the same number of tasks.
#' @return A list with:
#'   * `theta_hat` -- numeric p-vector of (respondent-weighted) DML point
#'     estimates;
#'   * `plugin` -- numeric p-vector of plug-in estimates (respondent-mean
#'     of `beta_hat`);
#'   * `correction` -- numeric N x p matrix of DML correction terms;
#'   * `influence_raw` -- numeric N x p matrix `beta_hat + correction`;
#'   * `phi_bar` -- numeric M x p matrix of within-respondent means of
#'     `influence_raw` (rows ordered by sorted respondent id), or `NULL`
#'     when `respondent_id` is not supplied.
#' @keywords internal
#' @noRd
.sc_influence_function <- function(beta_hat, lambda_obj, deltaX, y,
                                   respondent_id = NULL,
                                   respondent_weights = NULL) {
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

  if (is.null(respondent_id)) {
    ## Legacy / balanced-only: task-weighted average (== respondent-weighted
    ## when every respondent has the same number of tasks).
    theta_hat <- colMeans(influence_raw)
    plugin    <- colMeans(beta_hat)
    phi_bar   <- NULL
  } else {
    if (length(respondent_id) != n) {
      stop(".sc_influence_function(): `respondent_id` length disagrees with nrow(beta_hat).")
    }
    ## Respondent-weighted: theta_hat = (1/M) sum_i phi_bar_i, with
    ## phi_bar_i = (1/T_i) sum_t psi_it. rowsum(reorder=TRUE) groups by
    ## sorted respondent id; the unit-vector rowsum yields the matching T_i.
    key     <- as.character(respondent_id)
    w_obj   <- .sc_respondent_weight_object(respondent_id, respondent_weights)
    cnt     <- w_obj$count
    phi_bar <- rowsum(influence_raw, group = key, reorder = TRUE) / cnt
    bbar    <- rowsum(beta_hat,      group = key, reorder = TRUE) / cnt
    theta_hat <- as.numeric(crossprod(w_obj$a, phi_bar))
    plugin    <- as.numeric(crossprod(w_obj$a, bbar))
  }

  list(
    theta_hat     = theta_hat,
    plugin        = plugin,
    correction    = correction,
    influence_raw = influence_raw,
    phi_bar       = phi_bar
  )
}

#' Respondent-clustered variance-covariance of theta_hat
#'
#' Computes the full `p x p` RESPONDENT-weighted clustered variance of the
#' DML estimator, matching the paper's Appendix B:
#'
#'   V_cluster = (1 / (M (M-1))) * sum_i phi_i phi_i^T
#'             = (M/(M-1)) * crossprod(phi_centered) / M^2
#'
#' where `phi_i = phi_bar_i - theta_hat` and `phi_bar_i` is respondent
#' `i`'s within-respondent mean of the raw influence contributions. This
#' is the textbook clustered variance of the respondent-weighted mean
#' `theta_hat = (1/M) sum_i phi_bar_i`. Under balanced `T_i` it equals the
#' task-weighted `(M/(M-1)) crossprod(cluster_sums)/n^2`; `theta_hat`
#' passed in must therefore be the respondent-weighted point estimate.
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
.sc_cluster_vcov <- function(influence_raw, theta_hat, respondent_id,
                             respondent_weights = NULL) {
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

  w_obj <- .sc_respondent_weight_object(respondent_id, respondent_weights)
  key <- as.character(respondent_id)
  M   <- length(w_obj$w)
  if (M < 2L) {
    stop(".sc_cluster_vcov(): at least 2 clusters are required.")
  }

  ## Within-respondent means phi_bar_i (rows ordered by sorted id),
  ## centered at the respondent-weighted theta_hat.
  cnt     <- w_obj$count
  phi_bar <- rowsum(influence_raw, group = key, reorder = TRUE) / cnt  # M x p
  phi_c   <- sweep(phi_bar, 2L, theta_hat, check.margin = FALSE)

  ## V_cluster = (1 / (M (M-1))) sum_i phi_i phi_i^T
  ##           = (M/(M-1)) * crossprod(phi_c) / M^2
  phi_w <- phi_c * w_obj$a
  vcov_mat <- (M / (M - 1)) * crossprod(phi_w)
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
#' @param respondent_id Optional length-N vector of respondent ids. When
#'   supplied, the iid variance uses the respondent-weighted estimator's
#'   per-row weights `w_it = 1 / (M T_i)` -- i.e. the variance the
#'   respondent-weighted `theta_hat` would have if tasks were independent.
#'   When `NULL` it falls back to the equal-weight `crossprod(infl_c)/n^2`.
#'   The two coincide under balanced `T_i`.
#' @return A list with `vcov` (p x p) and `se` (p-vector).
#' @keywords internal
#' @noRd
.sc_iid_vcov <- function(influence_raw, theta_hat, respondent_id = NULL,
                         respondent_weights = NULL) {
  n <- nrow(influence_raw)
  infl_c <- sweep(influence_raw, 2L, theta_hat, check.margin = FALSE)
  if (is.null(respondent_id)) {
    vcov_mat <- crossprod(infl_c) / (n * n)
  } else {
    w_obj <- .sc_respondent_weight_object(respondent_id, respondent_weights)
    key <- as.character(respondent_id)
    Ti  <- stats::ave(rep.int(1, n), key, FUN = length)  # T_i per row
    row_a <- w_obj$a[match(key, w_obj$key)] / Ti
    Wc  <- infl_c * row_a
    ## crossprod(Wc) = sum_it w_it^2 (psi_it - theta)(psi_it - theta)^T
    vcov_mat <- crossprod(Wc)
  }
  se <- sqrt(pmax(diag(vcov_mat), 0))
  list(vcov = vcov_mat, se = se)
}

#' Collapse and normalize optional respondent survey weights
#'
#' @param respondent_id Length-N respondent ids, in task-row order.
#' @param respondent_weights Optional length-N numeric vector. Must be
#'   constant within respondent. `NULL` gives equal respondent weights.
#' @return list with sorted respondent keys, raw weights, normalized weights,
#'   and task counts.
#' @keywords internal
#' @noRd
.sc_respondent_weight_object <- function(respondent_id, respondent_weights = NULL) {
  key <- as.character(respondent_id)
  n <- length(key)
  cnt <- as.numeric(rowsum(rep.int(1, n), group = key, reorder = TRUE))
  keys <- rownames(rowsum(rep.int(1, n), group = key, reorder = TRUE))
  M <- length(keys)
  if (is.null(respondent_weights)) {
    w <- rep.int(1, M)
  } else {
    if (!is.numeric(respondent_weights) || length(respondent_weights) != n ||
        any(!is.finite(respondent_weights)) || any(respondent_weights < 0)) {
      stop("respondent_weights must be a finite non-negative numeric vector matching respondent_id.")
    }
    if (sum(respondent_weights) <= 0) {
      stop("respondent_weights must contain at least one positive weight.")
    }
    w_sum <- as.numeric(rowsum(respondent_weights, group = key, reorder = TRUE))
    w <- w_sum / cnt
    max_dev <- tapply(respondent_weights, key, function(z) max(abs(z - z[1L])), simplify = TRUE)
    if (any(max_dev > 1e-8)) {
      stop("respondent_weights must be constant within respondent.")
    }
  }
  a <- w / sum(w)
  list(key = keys, w = w, a = a, count = cnt)
}

#' Weighted mean and clustered variance of respondent-level contributions
#' @keywords internal
#' @noRd
.sc_weighted_cluster_stats <- function(phi, respondent_weights = NULL, level = 0.95) {
  phi <- as.matrix(phi)
  M <- nrow(phi)
  if (M < 2L) {
    stop(".sc_weighted_cluster_stats(): at least 2 respondents are required.")
  }
  if (is.null(respondent_weights)) {
    w <- rep.int(1, M)
  } else {
    w <- as.numeric(respondent_weights)
    if (length(w) != M || any(!is.finite(w)) || any(w < 0) || sum(w) <= 0) {
      stop(".sc_weighted_cluster_stats(): invalid respondent weights.")
    }
  }
  a <- w / sum(w)
  est <- as.numeric(crossprod(a, phi))
  phi_c <- sweep(phi, 2L, est, check.margin = FALSE)
  V <- (M / (M - 1)) * crossprod(phi_c * a)
  se <- sqrt(pmax(diag(V), 0))
  z <- stats::qnorm(1 - (1 - level) / 2)
  list(estimate = est, vcov = V, se = se,
       ci_lo = est - z * se, ci_hi = est + z * se,
       weights = w, norm_weights = a)
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
