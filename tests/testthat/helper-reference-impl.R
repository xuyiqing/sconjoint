## Pure-R reference implementations of each Tier A quantity, written
## directly from the spec.md §3.3 / §6A formulas (not from the
## prototype or the package R/ code).  These are the reference
## against which the package functions are checked at 1e-6 parity.
##
## Every helper takes a plain numeric matrix `B` (rows = observations,
## cols = dummies), a `resp` cluster vector, and optional `subset_idx`.
## None of them touches torch or `sc_fit`.

.ref_cluster_se <- function(q, resp) {
  n <- length(q)
  if (n == 0L) return(NA_real_)
  q_bar <- mean(q)
  dev <- q - q_bar
  resp_f <- as.factor(resp)
  M_S <- nlevels(resp_f)
  if (M_S < 2L) return(NA_real_)
  c_m <- as.numeric(tapply(dev, resp_f, sum))
  c_m[is.na(c_m)] <- 0
  sqrt((M_S / (M_S - 1)) * sum(c_m^2) / n^2)
}

.ref_mrs <- function(B, resp, num_idx, den_idx,
                     trim = c(0.01, 0.99), subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  r <- B[subset_idx, num_idx] / B[subset_idx, den_idx]
  r[!is.finite(r)] <- NA_real_
  ok <- !is.na(r)
  r <- r[ok]
  rs <- resp[subset_idx][ok]
  q_lo <- stats::quantile(r, trim[1], names = FALSE)
  q_hi <- stats::quantile(r, trim[2], names = FALSE)
  rt <- pmin(pmax(r, q_lo), q_hi)
  list(estimate = mean(rt),
       se = .ref_cluster_se(rt, rs),
       n = length(rt),
       trim_thresh = c(q_lo, q_hi))
}

.ref_counterfactual <- function(B, resp, delta_x, subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  lin <- as.numeric(B[subset_idx, , drop = FALSE] %*% delta_x)
  p_i <- stats::plogis(lin)
  list(estimate = mean(p_i),
       se = .ref_cluster_se(p_i, resp[subset_idx]),
       per_row_prob = p_i)
}

.ref_wtp <- function(B, resp, attr_idx, cost_idx,
                     trim = c(0.01, 0.99), subset_idx = NULL) {
  m <- .ref_mrs(B, resp, attr_idx, cost_idx, trim, subset_idx)
  list(estimate = -m$estimate, se = m$se)
}

.ref_importance <- function(B, resp, attr_map,
                            design = "uniform", subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  n_S <- length(subset_idx)
  K <- length(attr_map)
  V_mat <- matrix(0, n_S, K)
  for (a in seq_len(K)) {
    cols <- attr_map[[a]]
    L_a <- length(cols) + 1L
    bsub <- B[subset_idx, cols, drop = FALSE]
    m1 <- rowSums(bsub) / L_a
    m2 <- rowSums(bsub^2) / L_a
    V_mat[, a] <- m2 - m1^2
  }
  rs <- rowSums(V_mat)
  rs[rs == 0] <- NA_real_
  share <- V_mat / rs
  share[is.na(share)] <- 0
  est <- colMeans(share)
  se <- vapply(seq_len(K), function(a) {
    .ref_cluster_se(share[, a], resp[subset_idx])
  }, numeric(1L))
  data.frame(attribute = names(attr_map),
             share = est, se = se,
             stringsAsFactors = FALSE, row.names = NULL)
}

.ref_polarization <- function(B, subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  Bs <- B[subset_idx, , drop = FALSE]
  fp <- colMeans(Bs > 0)
  fn <- colMeans(Bs < 0)
  data.frame(dummy = seq_len(ncol(B)),
             frac_positive = fp,
             frac_negative = fn,
             polarization_idx = 1 - abs(fp - fn),
             stringsAsFactors = FALSE, row.names = NULL)
}

.ref_fraction_preferring <- function(B, resp, threshold = 0, subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  Bs <- B[subset_idx, , drop = FALSE]
  rs <- resp[subset_idx]
  p <- ncol(B)
  fp <- colMeans(Bs > threshold)
  fn <- colMeans(Bs < -threshold)
  se_p <- numeric(p); se_n <- numeric(p)
  for (j in seq_len(p)) {
    se_p[j] <- .ref_cluster_se(as.numeric(Bs[, j] > threshold), rs)
    se_n[j] <- .ref_cluster_se(as.numeric(Bs[, j] < -threshold), rs)
  }
  data.frame(dummy = seq_len(p),
             frac_positive = fp, frac_negative = fn,
             se_positive = se_p, se_negative = se_n,
             stringsAsFactors = FALSE, row.names = NULL)
}

.ref_optimal_profile <- function(B, resp, attr_map, subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  Bs <- B[subset_idx, , drop = FALSE]
  col_means <- colMeans(Bs)
  p <- ncol(B)
  x_star <- numeric(p)
  for (cols in attr_map) {
    cand <- c(0, col_means[cols])
    sel <- which.max(cand) - 1L
    if (sel > 0L) x_star[cols[sel]] <- 1
  }
  lin <- as.numeric(Bs %*% x_star)
  p_i <- stats::plogis(lin)
  list(estimate = mean(p_i),
       se = .ref_cluster_se(p_i, resp[subset_idx]),
       dummy_vector = x_star)
}

.ref_direction_intensity <- function(B, resp, subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  Bs <- B[subset_idx, , drop = FALSE]
  rs <- resp[subset_idx]
  p <- ncol(B)
  sig <- sign(Bs)
  mag <- abs(Bs)
  d <- colMeans(sig); u <- colMeans(mag)
  se_d <- numeric(p); se_u <- numeric(p)
  for (j in seq_len(p)) {
    se_d[j] <- .ref_cluster_se(sig[, j], rs)
    se_u[j] <- .ref_cluster_se(mag[, j], rs)
  }
  list(
    direction = data.frame(dummy = seq_len(p), d = d, se = se_d,
                           stringsAsFactors = FALSE, row.names = NULL),
    intensity = data.frame(dummy = seq_len(p), u = u, se = se_u,
                           stringsAsFactors = FALSE, row.names = NULL),
    separation = unname(u * (1 - abs(d)))
  )
}

.ref_heterogeneity_test <- function(B, resp, adjust = "none") {
  N <- nrow(B); p <- ncol(B)
  M <- length(unique(resp))
  resp_f <- as.factor(resp)
  var_beta <- numeric(p); se_var <- numeric(p)
  for (j in seq_len(p)) {
    bj <- B[, j]
    bbar <- mean(bj)
    dev2 <- (bj - bbar)^2
    var_beta[j] <- sum(dev2) / (N - 1L)
    cm <- as.numeric(tapply(dev2 - var_beta[j], resp_f, sum))
    cm[is.na(cm)] <- 0
    se_var[j] <- sqrt((M / (M - 1)) * sum(cm^2) / N^2)
  }
  t_stat <- ifelse(se_var > 0, var_beta / se_var, NA_real_)
  p_value <- stats::pnorm(-t_stat)
  method <- switch(adjust, none = "none", holm = "holm", bh = "BH")
  p_adj <- if (method == "none") p_value else stats::p.adjust(p_value, method)
  data.frame(dummy = seq_len(p),
             var_beta = var_beta, se_var = se_var,
             t_stat = t_stat, p_value = p_value,
             p_adjusted = p_adj,
             stringsAsFactors = FALSE, row.names = NULL)
}

## ---- Tier B reference implementations (M5) --------------------------------

.ref_subgroup <- function(B, resp, subset_idx) {
  Bs <- B[subset_idx, , drop = FALSE]
  rs <- resp[subset_idx]
  p <- ncol(B)
  theta <- colMeans(Bs)
  se <- numeric(p)
  for (j in seq_len(p)) {
    se[j] <- .ref_cluster_se(Bs[, j], rs)
  }
  data.frame(dummy = seq_len(p), theta = unname(theta), se = se,
             stringsAsFactors = FALSE, row.names = NULL)
}

.ref_compensating <- function(B, resp, benefit_idx, cost_idx,
                              trim = c(0.01, 0.99), subset_idx = NULL) {
  if (is.null(subset_idx)) subset_idx <- seq_len(nrow(B))
  b_ben <- B[subset_idx, benefit_idx]
  b_cost <- B[subset_idx, cost_idx]
  rs <- resp[subset_idx]
  ratio <- -b_ben / b_cost
  ratio[!is.finite(ratio)] <- NA_real_
  ok <- !is.na(ratio)
  r <- ratio[ok]
  rsub <- rs[ok]
  q_lo <- stats::quantile(r, trim[1], names = FALSE)
  q_hi <- stats::quantile(r, trim[2], names = FALSE)
  rt <- pmin(pmax(r, q_lo), q_hi)
  list(estimate = mean(rt),
       se = .ref_cluster_se(rt, rsub),
       frac_compensated = mean((b_ben + b_cost) >= 0),
       n = length(rt))
}

.ref_clusters <- function(B, k, scale = TRUE, nstart = 25L, seed = NULL) {
  if (isTRUE(scale)) {
    col_sd <- apply(B, 2L, stats::sd)
    col_sd[col_sd == 0] <- 1
    col_mean <- colMeans(B)
    X <- sweep(sweep(B, 2L, col_mean, "-"), 2L, col_sd, "/")
  } else {
    X <- B
  }
  old_rng <- if (exists(".Random.seed", envir = .GlobalEnv))
    .GlobalEnv$.Random.seed else NULL
  on.exit({
    if (is.null(old_rng)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_rng, envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (!is.null(seed)) set.seed(as.integer(seed))
  km <- stats::kmeans(X, centers = k, nstart = nstart, iter.max = 50L)
  list(assignment = as.integer(km$cluster),
       sizes = as.integer(table(factor(km$cluster, levels = seq_len(k)))),
       within_ss = as.numeric(km$withinss),
       total_within_ss = sum(km$withinss))
}

## Partition equivalence: two integer vectors represent the same
## partition if there exists a permutation of labels mapping one to
## the other.  We check by comparing the sorted table of co-membership
## counts.
.partition_equal <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)
  tab <- table(a, b)
  ## Each row (each a-label) must have exactly one nonzero column.
  rows_ok <- all(apply(tab, 1L, function(r) sum(r > 0) == 1L))
  cols_ok <- all(apply(tab, 2L, function(cc) sum(cc > 0) == 1L))
  rows_ok && cols_ok
}

## Build a cached sc_fit on small synthetic data for all quantity
## tests.  Skips tests when torch is unavailable.  The cache lives
## in a testthat-local environment to avoid refitting.
.sc_test_env <- new.env(parent = emptyenv())

.get_toy_fit <- function() {
  if (!is.null(.sc_test_env$fit)) return(.sc_test_env$fit)
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) {
    testthat::skip("libtorch not installed")
  }
  toy <- .make_toy_long(M = 50L, T_i = 3L, p = 3L, p_Z = 2L, seed = 1L)
  fit <- sconjoint::scfit(
    y ~ a1 + a2 + a3 | z1 + z2,
    data = toy$data,
    respondent = "rid", task = "tid", profile = "pos",
    K = 2L, n_epochs = 30L, seed = 1L
  )
  .sc_test_env$fit <- fit
  fit
}
