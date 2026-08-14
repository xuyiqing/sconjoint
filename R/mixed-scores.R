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
#' Per-column contrast SDs with the degenerate-column guard
#' @keywords internal
#' @noRd
.sc_sd_dx_cols <- function(m) {
  s <- apply(m, 2L, stats::sd)
  s[!is.finite(s) | s < 1e-12] <- 1
  s
}

#' Contrast SDs stored on the fit, recomputed when absent
#' @keywords internal
#' @noRd
.scmix_sd_dx <- function(fit) {
  if (is.null(fit$sd_dx)) .sc_sd_dx_cols(fit$deltaX) else fit$sd_dx
}

#' Canonical orientation for the fold loadings
#'
#' A is identified only up to right-rotation; the fit stores one
#' arbitrary orientation.  Inference must not depend on it, so every
#' entry point re-orients a copy of `A_folds` deterministically within
#' the equivalence class: column r of every fold is flipped so that the
#' largest-magnitude standardized component of fold 1's column r is
#' positive.
#' @keywords internal
#' @noRd
.scmix_canon <- function(fit) {
  sdx <- .scmix_sd_dx(fit)
  A1 <- fit$A_folds[[1L]] * sdx
  for (r in seq_len(ncol(A1))) {
    j <- which.max(abs(A1[, r]))
    if (A1[j, r] < 0) {
      for (k in seq_along(fit$A_folds)) {
        fit$A_folds[[k]][, r] <- -fit$A_folds[[k]][, r]
      }
    }
  }
  fit
}

.scmix_scores <- function(fit, mu_override = NULL, post_sd = FALSE) {
  fit <- .scmix_canon(fit)
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

  q <- ncol(fit$A_folds[[1L]])
  S <- matrix(0, N, p)
  S_A <- matrix(0, N, p * q)
  loglik <- numeric(N)
  post_mean <- matrix(0, N, p)
  post_sd_m <- if (post_sd) matrix(0, N, p) else NULL
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
    lp <- ifelse(rep(yk, G) == 1,
                 stats::plogis(idx, log.p = TRUE),
                 stats::plogis(-idx, log.p = TRUE))
    dim(lp) <- dim(idx)

    rk <- ridx[rows]
    rk_f <- factor(rk, levels = unique(rk))
    agg <- rowsum(lp, rk_f, reorder = FALSE)         # N_k x G
    lw <- sweep(agg, 2L, log(gh$w), `+`)
    m <- apply(lw, 1L, max)
    ll <- m + log(rowSums(exp(lw - m)))
    postw <- exp(lw - ll)                            # N_k x G, rows sum to 1

    ## map each task row to its within-fold respondent position
    pos <- match(rk, as.integer(levels(rk_f)))

    ## residual r_t = y_t - sum_g postw_{i(t),g} sigma(idx_tg)
    sig <- sigm(idx)
    resid_g <- yk - sig                              # n_k x G residual at each node
    rbar <- rowSums(postw[pos, , drop = FALSE] * resid_g)
    Sk <- rowsum(dxk * rbar, rk_f, reorder = FALSE)  # N_k x p

    ## loading score: d logL_i / d A_{kr} =
    ## E_post[ sum_t deltaX_tk * u_r * (y_t - G(idx)) ]
    SAk <- matrix(0, nlevels(rk_f), p * q)
    for (r in seq_len(q)) {
      s_r <- rowSums(postw[pos, , drop = FALSE] *
                       sweep(resid_g, 2L, gh$U[, r], `*`))
      SAk[, (r - 1L) * p + seq_len(p)] <-
        rowsum(dxk * s_r, rk_f, reorder = FALSE)
    }

    ## posterior mean of beta = mu + A E[u | data]
    Eu <- postw %*% gh$U                             # N_k x q
    mu_resp_k <- mu[rows, , drop = FALSE][!duplicated(rk), , drop = FALSE]
    pm <- mu_resp_k + Eu %*% t(A)

    tgt <- as.integer(levels(rk_f))
    S[tgt, ] <- Sk
    S_A[tgt, ] <- SAk
    loglik[tgt] <- ll
    post_mean[tgt, ] <- pm

    if (post_sd) {
      ## posterior variance of beta_k = A[k, ] Cov(u | data) A[k, ]',
      ## accumulated pair by pair on the quadrature grid (opt-in: the
      ## hot inference path never pays for this)
      pv <- matrix(0, nlevels(rk_f), p)
      for (r in seq_len(q)) {
        for (s in r:q) {
          Euu_rs <- postw %*% (gh$U[, r] * gh$U[, s])
          cov_rs <- as.numeric(Euu_rs) - Eu[, r] * Eu[, s]
          fac <- if (r == s) 1 else 2
          pv <- pv + fac * (cov_rs %o% (A[, r] * A[, s]))
        }
      }
      post_sd_m[tgt, ] <- sqrt(pmax(pv, 0))
    }
  }

  list(resp = levels(resp_f), S = S, S_A = S_A, loglik = loglik,
       post_mean = post_mean, post_sd = post_sd_m, T_i = T_i,
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
#' @param M Simulated respondents per bin (default 2000).
#' @param seed RNG seed for the simulation.
#' @param eig_floor Relative eigenvalue floor (default 1e-3 of the
#'   mean diagonal).
#' @return A list with `I_inv` (list of p x p inverses, one per bin)
#'   and `bin_of` (length-N bin index per respondent).
#' @keywords internal
#' @noRd
.scmix_information <- function(fit, n_bins = 40L, M = 2000L, seed = 1L,
                               eig_floor = 1e-3) {
  fit <- .scmix_canon(fit)
  sc <- .scmix_scores(fit)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  mu_resp <- fit$mu_hat[first, , drop = FALSE]

  ## rotation-invariant residual covariance, refactored to rank q on the
  ## STANDARDIZED scale (raw-units eigen-truncation makes the retained
  ## subspace, and with it every downstream correction, unit-dependent)
  sd_dxS <- .scmix_sd_dx(fit)
  Sig <- Reduce(`+`, lapply(fit$A_folds, tcrossprod)) / length(fit$A_folds)
  D_S <- diag(sd_dxS, ncol(Sig))
  Sig_std <- D_S %*% Sig %*% D_S
  eS <- eigen(Sig_std, symmetric = TRUE)
  qq <- fit$q
  A_sim <- diag(1 / sd_dxS, ncol(Sig)) %*%
    (eS$vectors[, seq_len(qq), drop = FALSE] %*%
       diag(sqrt(pmax(eS$values[seq_len(qq)], 0)), qq))
  ## orient the simulation loading to the (Procrustes-aligned) fold
  ## loadings: the eigenvector sign is arbitrary, and a sim-vs-data
  ## orientation mismatch corrupts the cross-information blocks
  M_p <- crossprod(A_sim, fit$A_folds[[1L]])
  sv <- svd(M_p)
  A_sim <- A_sim %*% (sv$u %*% t(sv$v))

  .sc_mixed_info_core(mu_resp = mu_resp, T_i = sc$T_i, A_sim = A_sim,
                      pool = fit$deltaX, sd_dx = sd_dxS, gh = fit$gh,
                      n_bins = n_bins, M = M, seed = seed,
                      eig_floor = eig_floor)
}

#' Simulation core for the model-implied information
#'
#' The fit-free half of [.scmix_information()]: bins respondents on the
#' standardized mean scale (crossed with distinct task counts), then
#' simulates the per-bin information blocks at (mu_c, A_sim) over the
#' design pool.  Taking (mu_resp, T_i, A_sim, pool) directly makes the
#' same code path serve both the fit-based check and the
#' hypothesized-value design precheck.
#' @keywords internal
#' @noRd
.sc_mixed_info_core <- function(mu_resp, T_i, A_sim, pool, sd_dx, gh,
                                n_bins = 40L, M = 2000L, seed = 1L,
                                eig_floor = 1e-3) {
  withr::local_preserve_seed()
  set.seed(seed)
  N <- nrow(mu_resp)
  p <- ncol(mu_resp)
  qq <- ncol(A_sim)

  ## bin on the standardized scale so the partition (and with it every
  ## downstream correction) is invariant to attribute units
  mu_std <- sweep(mu_resp, 2L, sd_dx, `*`)
  n_distinct <- nrow(unique(mu_std))
  n_bins <- min(as.integer(n_bins), N, n_distinct)
  if (n_distinct <= n_bins) {
    ## coarse Z: every distinct mu row is its own bin, no kmeans needed
    key_mu <- apply(round(mu_std, 10L), 1L, paste, collapse = ":")
    bin_of <- match(key_mu, unique(key_mu))
  } else {
    km <- suppressWarnings(
      stats::kmeans(mu_std, centers = n_bins, iter.max = 100L, nstart = 3L,
                    algorithm = "Lloyd")
    )
    bin_of <- km$cluster
  }
  ## split bins further by distinct task count when unbalanced
  key <- paste(bin_of, T_i, sep = ":")
  ukey <- unique(key)
  bin_of <- match(key, ukey)

  sigm <- function(x) 1 / (1 + exp(-x))
  ## The eigenvalue floor must not compare eigenvalues across contrast
  ## columns with very different scales (a small-variance column's true
  ## information can sit far below the floor set by large-variance
  ## columns, silently degrading the correction to the plug-in there).
  ## Floor on the standardized scale instead: I_std = D I D with
  ## D = diag(sd_dx), floor I_std's eigenvalues, map back.
  D_inv <- diag(1 / sd_dx, p)
  floor_hits <- 0L
  I_inv <- vector("list", length(ukey))
  I_muA <- vector("list", length(ukey))
  I_AA <- vector("list", length(ukey))

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

    ## Fisher-identity scores (location and loading) for each simulated
    ## respondent under (mu_c, A_sim)
    idx <- rowSums(dx * matrix(mu_c, length(rid), p, byrow = TRUE)) +
      (dx %*% A_sim) %*% t(gh$U)
    lp <- ifelse(rep(ysim, length(gh$w)) == 1,
                 stats::plogis(idx, log.p = TRUE),
                 stats::plogis(-idx, log.p = TRUE))
    dim(lp) <- dim(idx)
    agg <- rowsum(lp, rid, reorder = TRUE)
    lw <- sweep(agg, 2L, log(gh$w), `+`)
    mrow <- apply(lw, 1L, max)
    ll <- mrow + log(rowSums(exp(lw - mrow)))
    postw <- exp(lw - ll)
    sig <- sigm(idx)
    resid_g <- ysim - sig
    rbar <- rowSums(postw[rid, , drop = FALSE] * resid_g)
    Ssim <- rowsum(dx * rbar, rid, reorder = TRUE)
    SAsim <- matrix(0, M, p * qq)
    for (r in seq_len(qq)) {
      s_r <- rowSums(postw[rid, , drop = FALSE] *
                       sweep(resid_g, 2L, gh$U[, r], `*`))
      SAsim[, (r - 1L) * p + seq_len(p)] <- rowsum(dx * s_r, rid,
                                                   reorder = TRUE)
    }

    I_muA[[b]] <- crossprod(Ssim, SAsim) / M
    I_AA[[b]] <- crossprod(SAsim) / M
    I_b <- crossprod(Ssim) / M
    ## eigenvalue floor on the standardized information
    I_std <- D_inv %*% I_b %*% D_inv
    eI <- eigen(I_std, symmetric = TRUE)
    floor_val <- eig_floor * mean(diag(I_std))
    floor_hits <- floor_hits + sum(eI$values < floor_val)
    vals <- pmax(eI$values, floor_val)
    Istd_inv <- eI$vectors %*% diag(1 / vals, p) %*% t(eI$vectors)
    I_inv[[b]] <- D_inv %*% Istd_inv %*% D_inv
  }
  if (floor_hits > 0L) {
    warning(".scmix_information(): the eigenvalue floor bound ", floor_hits,
            " eigenvalue(s); the orthogonal correction is damped in those",
            " directions. Inspect the design scaling before trusting the",
            " affected coordinates.")
  }

  list(I_inv = I_inv, I_muA = I_muA, I_AA = I_AA, bin_of = bin_of)
}
