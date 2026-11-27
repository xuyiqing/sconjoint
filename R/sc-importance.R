# Attribute importance as quadratic forms over level menus (paper eq. attribute_importance).
#
# Ported from the authors' code of the paper's rebuild (replication/2026-11_v4/sources/
# importance_qfactor_author.R, itself analysis/2026-11-18_rum_applications_memo/scripts/
# Q_imp_closures.R): Qlev, impQ_block, impQ_sums_rqmc, imp_Q_sums1 and make_imp_q, arithmetic unchanged.
#
# For respondent i with taste b = mu_i + Lambda u, u ~ N(0, I_q), the importance share of attribute
# group g is m_ig = E[ b' Q_g b / sum_h b' Q_h b ], where Q_g is the variance matrix of the group's
# utility over its menu of levels: the menu A (L levels x |g| coded columns, e.g. the reference level 0
# plus one dummy per column, or a four-point scale matrix(1:4)) with level weights pi gives
# Q_g = A' diag(pi) A - (A' pi)(A' pi)' on the group's columns (Qlev). This is the paper's definition
# "both" (four-point policy scales, the democracy codes merged into one group).
# Integration. q > 1: randomised QMC with convergence control on the canonical axes (as the package's
# .rum_imp_sums_rqmc): 8 Owen scramblings of a Sobol set (spacefillr, seeds 20261114 + 1..8), each point
# with its mirror, doubling from 256 to at most 16,384 points per scrambling, respondent by respondent,
# until every share's standard error across the scramblings is at most 1e-4. One factor (a loading
# vector): the one-dimensional rule of the October code (the u-marginal of rum_quadrature(c(80, 41))).
# Gradients are exact on the same draws, mapped to Lambda with the rotation part removed (.rum_imp_gl).

# Level-variance matrix of one group (p x p, zero outside the group's columns).
.sc_Qlev <- function(p, idx, A, pi) {
  pi <- pi / sum(pi); a <- colSums(pi * A); Qs <- crossprod(A * sqrt(pi)) - tcrossprod(a)
  Q <- matrix(0, p, p); Q[idx, idx] <- Qs; Q
}
# u-marginal of a product rule (the one-factor rule).
.sc_umarg <- function(Q) {
  uu <- sort(unique(round(Q$u, 12)))
  list(u = uu, w = vapply(uu, function(x) sum(Q$weight[abs(Q$u - x) < 1e-9]), 0))
}
# One factor (October, verbatim).
.sc_impQ_sums1 <- function(mu, lam, QL, uq) {
  n <- nrow(mu); p <- ncol(mu); ng <- length(QL); M <- matrix(0, n, ng); GM <- GL <- array(0, c(n, p, ng))
  for (x in seq_along(uq$u)) {
    B <- mu + matrix(lam * uq$u[x], n, p, byrow = TRUE)
    QB <- lapply(QL, function(Q) B %*% Q); v <- sapply(QB, function(z) rowSums(z * B)); if (!is.matrix(v)) v <- matrix(v, n)
    Tt <- rowSums(v); sh <- v / Tt; SQB <- Reduce(`+`, QB)
    for (g in seq_len(ng)) {
      dr <- 2 * (QB[[g]] - sh[, g] * SQB) / Tt
      GM[, , g] <- GM[, , g] + uq$w[x] * dr; GL[, , g] <- GL[, , g] + uq$w[x] * uq$u[x] * dr
    }
    M <- M + uq$w[x] * sh
  }
  list(m = M, gm = GM, gl = GL)
}
# Shares and gradients at the draws OFF (K x p offsets C z) for means MU (n x p): sums over the K draws.
.sc_impQ_block <- function(MU, OFF, Zr, QL, grad = TRUE) {
  n <- nrow(MU); pp <- ncol(MU); K <- nrow(OFF); ng <- length(QL); r <- if (is.null(Zr)) 0L else ncol(Zr)
  ii <- rep(seq_len(n), K); kk <- rep(seq_len(K), each = n)
  B <- MU[ii, , drop = FALSE] + OFF[kk, , drop = FALSE]
  QB <- lapply(QL, function(Q) {
    ix <- which(rowSums(Q != 0) > 0); z <- matrix(0, nrow(B), pp)         # Q_g is zero outside its block
    z[, ix] <- B[, ix, drop = FALSE] %*% Q[ix, ix, drop = FALSE]; z })
  v <- matrix(vapply(QB, function(z) rowSums(z * B), numeric(n * K)), n * K)
  Tt <- rowSums(v); sh <- v / Tt
  sumk <- function(x) matrix(x, n, K) %*% rep(1, K)
  out <- list(sh = matrix(vapply(seq_len(ng), function(g) drop(sumk(sh[, g])), numeric(n)), n))
  if (!grad) return(out)
  SQB <- Reduce(`+`, QB); gm <- array(0, c(n, pp, ng)); gz <- array(0, c(n, pp, max(r, 1L), ng))
  for (g in seq_len(ng)) {
    dr <- 2 * (QB[[g]] - sh[, g] * SQB) / Tt
    for (j in seq_len(pp)) { Gj <- matrix(dr[, j], n, K); gm[, j, g] <- Gj %*% rep(1, K); if (r) gz[, j, , g] <- Gj %*% Zr }
  }
  out$gm <- gm; out$gz <- gz; out
}
# Randomised QMC with convergence control (q > 1).
.sc_impQ_sums_rqmc <- function(mu, loading, QL, control = .rum_imp_control()) {
  n <- nrow(mu); pp <- ncol(mu); nf <- ncol(loading); ng <- length(QL)
  ax <- .rum_imp_axes(loading); r <- length(ax$keep)
  ev <- function(MU, OFF, Zr) .sc_impQ_block(MU, OFF, Zr, QL)
  if (!r) {
    e <- ev(mu, matrix(0, 1L, pp), NULL)
    return(list(m = e$sh, gm = e$gm, gl = array(0, c(n, pp * nf, ng)), se = matrix(0, n, ng), n_points = rep(0L, n)))
  }
  R <- control$n_rep; tol <- control$tol
  M <- matrix(0, n, ng); GM <- array(0, c(n, pp, ng)); GZ <- array(0, c(n, pp, r, ng))
  SE <- matrix(NA_real_, n, ng); NP <- integer(n)
  Sr <- array(0, c(n, ng, R)); GMs <- array(0, c(n, pp, ng)); GZs <- array(0, c(n, pp, r, ng))
  active <- seq_len(n); Ncur <- 0L; Nt <- control$n_start
  chunk <- max(1L, floor(2e5 / max(1L, n)))
  repeat {
    for (rp in seq_len(R)) {
      z <- stats::qnorm(matrix(spacefillr::generate_sobol_owen_set(Nt, r, seed = control$seed + rp), Nt, r))
      z <- z[(Ncur + 1L):Nt, , drop = FALSE]; z <- rbind(z, -z)
      for (cs in split(seq_len(nrow(z)), ceiling(seq_len(nrow(z)) / chunk))) {
        Zc <- z[cs, , drop = FALSE]; e <- ev(mu[active, , drop = FALSE], Zc %*% t(ax$C), Zc)
        Sr[active, , rp] <- Sr[active, , rp] + e$sh
        GMs[active, , ] <- GMs[active, , , drop = FALSE] + e$gm
        GZs[active, , , ] <- GZs[active, , , , drop = FALSE] + e$gz
      }
    }
    Ncur <- Nt; nd <- 2 * Ncur
    est <- Sr[active, , , drop = FALSE] / nd
    se <- matrix(apply(est, c(1L, 2L), stats::sd) / sqrt(R), length(active))
    done <- apply(se, 1L, max) <= tol
    if (Nt >= control$n_max) done[] <- TRUE
    fin <- active[done]
    if (length(fin)) {
      M[fin, ] <- matrix(apply(est[done, , , drop = FALSE], c(1L, 2L), mean), length(fin))
      GM[fin, , ] <- GMs[fin, , , drop = FALSE] / (nd * R); GZ[fin, , , ] <- GZs[fin, , , , drop = FALSE] / (nd * R)
      SE[fin, ] <- se[done, , drop = FALSE]; NP[fin] <- Ncur
    }
    active <- active[!done]
    if (!length(active)) break
    Nt <- min(2L * Nt, control$n_max)
  }
  bad <- which(apply(SE, 1L, max) > tol)
  if (length(bad))
    warning(sprintf("importance (quadratic forms): %d respondent(s) did not reach SE %g at %d points (largest %.2g)",
                    length(bad), tol, control$n_max, max(SE)), call. = FALSE)
  GL <- array(0, c(n, pp * nf, ng))
  for (g in seq_len(ng)) GL[, , g] <- .rum_imp_gl(array(GZ[, , , g], c(n, pp, r)), ax$V, ax$keep, loading)
  list(m = M, gm = GM, gl = GL, se = SE, n_points = NP)
}
.sc_impQ_sums <- function(mu, loading, QL, uq = NULL, control = .rum_imp_control()) {
  if (!is.matrix(loading)) return(.sc_impQ_sums1(mu, loading, QL, if (is.null(uq)) .sc_umarg(rum_quadrature(c(80L, 41L))) else uq))
  .sc_impQ_sums_rqmc(mu, loading, QL, control)
}

# Internal importance targets of one definition (make_imp_q): one integration per fitted state, kept in
# a small cache keyed on (mu, loading) (most recent first, at most `keep` states: a joint update cycles
# through the five fold states and the moved states of its path). p: the coefficients the forms use.
.sc_imp_targets <- function(QL, names, p, w = NULL, control = .rum_imp_control(), keep = 16L) {
  force(QL); force(names); force(p); force(w); force(control)
  cache <- new.env(parent = emptyenv()); cache$entries <- list()
  sums <- function(mu, th) {
    hit <- which(vapply(cache$entries, function(x) identical(x$mu, mu) && identical(x$loading, th$loading), logical(1)))
    if (length(hit)) {
      old <- cache$entries[[hit[1L]]]; cache$entries <- c(list(old), cache$entries[-hit[1L]]); return(old$value)
    }
    L <- th$loading; Lp <- if (is.matrix(L)) L[seq_len(p), , drop = FALSE] else L[seq_len(p)]
    value <- .sc_impQ_sums(mu, Lp, QL, control = control)
    cache$entries <- utils::head(c(list(list(mu = mu, loading = th$loading, value = value)), cache$entries), keep)
    value
  }
  lapply(seq_along(QL), function(g) {
    force(g)
    gf <- function(mu, th) {
      s <- sums(mu, th); n <- nrow(mu)
      list(m = s$m[, g], gm = matrix(s$gm[, , g], n), gt = cbind(0, matrix(s$gl[, , g], n), 0))
    }
    list(name = names[g], kind = "importance", w = w, m = function(mu, th) gf(mu, th)$m, grad = gf)
  })
}
