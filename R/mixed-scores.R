## Fisher-identity scores and model-implied information for scmix fits.
##
## Everything here is evaluated at each respondent's OUT-OF-FOLD
## nuisances: mu_hat(Z_i) predicted by the fold model that never saw
## respondent i, and that same fold model's loading matrix A.  These
## are the ingredients the orthogonal scores in R/mixed-inference.R
## consume.
##
## The location score of the integrated likelihood obeys the Fisher
## identity: the derivative of log L_i with respect to mu(Z_i) equals
## the posterior-expected complete-data score,
##
##   S_i = E[ sum_t deltaX_it (Y_it - G(deltaX_it' beta)) | Y_i, X_i, Z_i ],
##
## with the posterior over beta_i = mu + A u taken on the quadrature
## grid.  `.scmix_check_fisher()` verifies this numerically against a
## finite difference of log L_i; the test suite runs it.

#' Per-respondent posterior weights, scores, and log-likelihoods
#'
#' Computes, at the out-of-fold nuisances stored on the fit: the
#' posterior weights over the quadrature nodes, the Fisher-identity
#' location score `S` (N x p), the per-respondent marginal
#' log-likelihood (length N), and the posterior mean of beta
#' (N x p, useful for descriptive comparison with the two-stage MAP).
#'
#' @param fit An `scmix` object.
#' @param mu_override Optional n x p matrix replacing `fit$mu_hat`
#'   (used by the Fisher-identity self-check).
#' @return A list with `resp` (respondent ids, one per respondent, in
#'   first-appearance order), `S`, `loglik`, `post_mean`, `T_i`.
#' @keywords internal
#' @noRd
.scmix_scores <- function(fit, mu_override = NULL) {
  deltaX <- fit$deltaX
  y <- fit$y
  mu <- if (is.null(mu_override)) fit$mu_hat else mu_override
  gh <- fit$gh
  G <- length(gh$w)
  p <- ncol(deltaX)

  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  ridx <- as.integer(resp_f)
  N <- nlevels(resp_f)

  ## respondent-level fold: constant within respondent by construction
  fold_resp <- tapply(fit$fold_id, ridx, function(v) v[1L])

  S <- matrix(0, N, p)
  loglik <- numeric(N)
  post_mean <- matrix(0, N, p)
  T_i <- as.integer(table(ridx))

  sigm <- function(x) 1 / (1 + exp(-x))

  for (k in seq_len(fit$K)) {
    rows <- fit$fold_id == k
    if (!any(rows)) next
    A <- fit$A_folds[[k]]
    dxk <- deltaX[rows, , drop = FALSE]
    idx <- rowSums(dxk * mu[rows, , drop = FALSE]) +
      (dxk %*% A) %*% t(gh$U)                       # n_k x G
    yk <- y[rows]
    ## log per-task choice prob at each node, numerically stable
    lp <- ifelse(rep(yk, G) == 1, -log1p(exp(-idx)), -log1p(exp(idx)))
    dim(lp) <- dim(idx)

    rk <- ridx[rows]
    rk_f <- factor(rk, levels = unique(rk))
    agg <- rowsum(lp, rk_f, reorder = FALSE)         # N_k x G
    lw <- sweep(agg, 2L, log(gh$w), `+`)
    m <- apply(lw, 1L, max)
    ll <- m + log(rowSums(exp(lw - m)))
    postw <- exp(lw - ll)                            # N_k x G, rows sum to 1

    resp_ids_k <- as.integer(levels(factor(rk)))     # unique respondent indices
    ## map each task row to its within-fold respondent position
    pos <- match(rk, as.integer(levels(rk_f)))

    ## residual r_t = y_t - sum_g postw_{i(t),g} sigma(idx_tg)
    sig <- sigm(idx)
    rbar <- yk - rowSums(postw[pos, , drop = FALSE] * sig)
    Sk <- rowsum(dxk * rbar, rk_f, reorder = FALSE)  # N_k x p

    ## posterior mean of beta = mu + A E[u | data]
    Eu <- postw %*% gh$U                             # N_k x q
    mu_resp_k <- mu[rows, , drop = FALSE][!duplicated(rk), , drop = FALSE]
    pm <- mu_resp_k + Eu %*% t(A)

    tgt <- as.integer(levels(rk_f))
    S[tgt, ] <- Sk
    loglik[tgt] <- ll
    post_mean[tgt, ] <- pm
  }

  list(resp = levels(resp_f), S = S, loglik = loglik,
       post_mean = post_mean, T_i = T_i,
       fold_resp = as.integer(fold_resp))
}

#' Numerical Fisher-identity self-check
#'
#' Perturbs one coordinate of mu for a few respondents and compares
#' the finite-difference derivative of the marginal log-likelihood to
#' the Fisher-identity score.  Returns the maximum absolute
#' discrepancy; the unit tests assert it is ~1e-6.
#'
#' @keywords internal
#' @noRd
.scmix_check_fisher <- function(fit, n_check = 3L, coord = 1L, eps = 1e-5) {
  base <- .scmix_scores(fit)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  ridx <- as.integer(resp_f)
  worst <- 0
  for (i in seq_len(min(n_check, fit$N))) {
    up <- fit$mu_hat; dn <- fit$mu_hat
    up[ridx == i, coord] <- up[ridx == i, coord] + eps
    dn[ridx == i, coord] <- dn[ridx == i, coord] - eps
    ll_up <- .scmix_scores(fit, mu_override = up)$loglik[i]
    ll_dn <- .scmix_scores(fit, mu_override = dn)$loglik[i]
    fd <- (ll_up - ll_dn) / (2 * eps)
    worst <- max(worst, abs(fd - base$S[i, coord]))
  }
  worst
}

#' Model-implied respondent-level information, binned over mu
#'
#' Estimates I(Z) = E[S S' | Z] by simulation from the fitted model
#' over the KNOWN design law -- the design-law plug-in pattern: draw a
#' respondent's tasks by resampling observed deltaX rows, draw
#' u ~ N(0, I_q), draw choices from the model, and average the outer
#' product of the Fisher-identity score.  Because the design is
#' independent of Z, I(Z) varies only through (mu(Z), T); respondents
#' are therefore grouped into k-means bins over mu_hat (crossed with
#' distinct task counts), one simulation per bin.
#'
#' The residual covariance uses the rotation-invariant fold average of
#' AA', refactored to p x q.  An eigenvalue floor guards the inverse.
#'
#' @param fit An `scmix` object.
#' @param n_bins Target number of mu bins (default 40).
#' @param M Simulated respondents per bin (default 300).
#' @param seed RNG seed for the simulation.
#' @param eig_floor Relative eigenvalue floor (default 1e-3 of the
#'   mean diagonal).
#' @return A list with `I_inv` (list of p x p inverses, one per bin)
#'   and `bin_of` (length-N bin index per respondent).
#' @keywords internal
#' @noRd
.scmix_information <- function(fit, n_bins = 40L, M = 300L, seed = 1L,
                               eig_floor = 1e-3) {
  withr::local_preserve_seed()
  set.seed(seed)

  sc <- .scmix_scores(fit)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  mu_resp <- fit$mu_hat[first, , drop = FALSE]
  N <- nrow(mu_resp)
  p <- ncol(mu_resp)

  ## rotation-invariant residual covariance, refactored to rank q
  Sig <- Reduce(`+`, lapply(fit$A_folds, tcrossprod)) / length(fit$A_folds)
  eS <- eigen(Sig, symmetric = TRUE)
  qq <- fit$q
  A_sim <- eS$vectors[, seq_len(qq), drop = FALSE] %*%
    diag(sqrt(pmax(eS$values[seq_len(qq)], 0)), qq)

  n_bins <- min(as.integer(n_bins), N)
  km <- suppressWarnings(
    stats::kmeans(mu_resp, centers = n_bins, iter.max = 100L, nstart = 3L,
                  algorithm = "Lloyd")
  )
  bin_of <- km$cluster
  ## split bins further by distinct task count when unbalanced
  T_i <- sc$T_i
  key <- paste(bin_of, T_i, sep = ":")
  ukey <- unique(key)
  bin_of <- match(key, ukey)

  gh <- fit$gh
  sigm <- function(x) 1 / (1 + exp(-x))
  pool <- fit$deltaX
  I_inv <- vector("list", length(ukey))

  for (b in seq_along(ukey)) {
    members <- which(bin_of == b)
    mu_c <- colMeans(mu_resp[members, , drop = FALSE])
    T_b <- as.integer(round(mean(T_i[members])))
    ## simulate M respondents with T_b tasks each
    rows <- sample.int(nrow(pool), M * T_b, replace = TRUE)
    dx <- pool[rows, , drop = FALSE]
    u <- matrix(stats::rnorm(M * qq), M, qq)
    beta <- matrix(mu_c, M, p, byrow = TRUE) + u %*% t(A_sim)
    rid <- rep(seq_len(M), each = T_b)
    pr <- sigm(rowSums(dx * beta[rid, , drop = FALSE]))
    ysim <- as.numeric(stats::runif(length(pr)) < pr)

    ## Fisher-identity score for each simulated respondent under (mu_c, A_sim)
    idx <- rowSums(dx * matrix(mu_c, length(rid), p, byrow = TRUE)) +
      (dx %*% A_sim) %*% t(gh$U)
    lp <- ifelse(rep(ysim, length(gh$w)) == 1, -log1p(exp(-idx)), -log1p(exp(idx)))
    dim(lp) <- dim(idx)
    agg <- rowsum(lp, rid, reorder = TRUE)
    lw <- sweep(agg, 2L, log(gh$w), `+`)
    mrow <- apply(lw, 1L, max)
    ll <- mrow + log(rowSums(exp(lw - mrow)))
    postw <- exp(lw - ll)
    sig <- sigm(idx)
    rbar <- ysim - rowSums(postw[rid, , drop = FALSE] * sig)
    Ssim <- rowsum(dx * rbar, rid, reorder = TRUE)

    I_b <- crossprod(Ssim) / M
    ## eigenvalue floor
    eI <- eigen(I_b, symmetric = TRUE)
    floor_val <- eig_floor * mean(diag(I_b))
    vals <- pmax(eI$values, floor_val)
    I_inv[[b]] <- eI$vectors %*% diag(1 / vals, p) %*% t(eI$vectors)
  }

  list(I_inv = I_inv, bin_of = bin_of)
}
