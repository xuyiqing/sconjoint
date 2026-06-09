## Debiased (orthogonal-score) inference for the additional quantities of
## interest -- the package implementation of the paper's Appendix C.
##
## The DML correction debiases not only the average parameter theta but any
## smooth functional H(f(Z)) of the per-respondent mean preference vector,
## via the orthogonal score
##
##   psi^H_it = H(f_hat(Z_i)) + grad_f H(f_hat(Z_i))' u_it,
##   u_it     = Lambda_hat^{-1}(Z_i) DeltaX_it (Y_it - G_hat_it),
##
## where u_it is the SAME "residual transport" vector for every estimand --
## it is exactly `object$correction`, formed once by `scfit()`.  Each
## quantity supplies its own H and grad_f H; the respondent-clustered
## variance of the respondent-weighted mean of psi^H is the debiased SE.
## With H = f_k this reproduces `object$theta` and its clustered SE exactly.
##
## These internals back the `vartype = "orthogonal"` path of the additional
## QoI functions.  They reuse `object$beta_hat_dnn` (the Stage-1 cross-fit
## f_hat that the orthogonal score is built at) and `object$correction`.

#' Debiased estimate + clustered SE of a scalar functional H(f)
#'
#' @param object An `sc_fit`.
#' @param Hfun A function of the `N x p` first-stage matrix `f` returning a
#'   list with `H` (length-N value) and `grad` (`N x p` gradient).
#' @return Named numeric: `estimate`, `se`, `ci_lo`, `ci_hi`.
#' @keywords internal
#' @noRd
.sc_debiased_scalar <- function(object, Hfun, level = 0.95) {
  comp <- .sc_debiased_phi(object, Hfun)            # respondent-level phi_bar
  phi  <- comp$phi; M <- length(phi)
  est  <- mean(phi)
  V    <- sum((phi - est)^2) / (M * (M - 1))
  se   <- sqrt(max(V, 0))
  z    <- stats::qnorm(1 - (1 - level) / 2)
  c(estimate = est, se = se, ci_lo = est - z * se, ci_hi = est + z * se)
}

#' Per-respondent influence means phi_bar_i for a functional H(f)
#'
#' Shared core: forms the per-row orthogonal score `psi = H + rowSums(grad *
#' correction)` and collapses to respondent-level means (rows ordered by
#' sorted respondent id, matching `.sc_cluster_vcov()`).
#' @return list(phi = M-vector of phi_bar_i, psi = N-vector raw score).
#' @keywords internal
#' @noRd
.sc_debiased_phi <- function(object, Hfun) {
  f <- object$beta_hat_dnn
  U <- object$correction
  if (is.null(f) || is.null(U)) {
    stop("debiased inference requires `object$beta_hat_dnn` and ",
         "`object$correction`; refit with a current version of scfit().",
         call. = FALSE)
  }
  hb  <- Hfun(f)
  psi <- as.numeric(hb$H) + rowSums(hb$grad * U)
  key <- as.character(object$respondent_id)
  cnt <- as.numeric(rowsum(rep.int(1, length(psi)), group = key, reorder = TRUE))
  phi <- as.numeric(rowsum(psi, group = key, reorder = TRUE) / cnt)
  list(phi = phi, psi = psi)
}

## ---------------------------------------------------------------------------
## H-value and gradient builders (pure functions of the N x p matrix `f`)
## ---------------------------------------------------------------------------

#' H = f_k, grad = e_k (the average-parameter anchor).
#' @keywords internal
#' @noRd
.sc_dH_thetak <- function(k) {
  force(k)
  function(f) list(H = f[, k],
                   grad = { g <- matrix(0, nrow(f), ncol(f)); g[, k] <- 1; g })
}

#' Counterfactual vote share / win probability: H = G(c'f), grad = G'(c'f) c.
#' `cvec` is an analyst-chosen exogenous contrast (length p).
#' @keywords internal
#' @noRd
.sc_dH_voteshare <- function(cvec) {
  force(cvec)
  function(f) {
    idx <- as.numeric(f %*% cvec)
    g   <- stats::plogis(idx)
    list(H = g, grad = outer(g * (1 - g), cvec))
  }
}

#' Attribute-importance numerator for block `cols`: H = f_a' S_a f_a,
#' grad = 2 S_a^{(p)} f (zero outside the block).
#' @keywords internal
#' @noRd
.sc_dH_importance_num <- function(cols, Sa) {
  force(cols); force(Sa)
  function(f) {
    fa <- f[, cols, drop = FALSE]
    grad <- matrix(0, nrow(f), ncol(f))
    grad[, cols] <- 2 * (fa %*% Sa)
    list(H = rowSums((fa %*% Sa) * fa), grad = grad)
  }
}

#' AME of attribute `k` on the probability scale, integrated over a
#' single-profile pool `Xpool` (n_pool x p) drawn from the design law:
#'   H(f) = E_X[G(f_k + X_{-k}'f_{-k}) - G(X_{-k}'f_{-k})].
#' @keywords internal
#' @noRd
.sc_dH_ame <- function(k, Xpool) {
  force(k); force(Xpool)
  function(f) {
    n <- nrow(f); p <- ncol(f)
    off <- setdiff(seq_len(p), k)
    Xoff <- Xpool[, off, drop = FALSE]
    H <- numeric(n); grad <- matrix(0, n, p)
    Gp <- function(x) { g <- stats::plogis(x); g * (1 - g) }
    for (i in seq_len(n)) {
      lin_off <- as.numeric(Xoff %*% f[i, off])
      lin_on  <- f[i, k] + lin_off
      gon <- Gp(lin_on); goff <- Gp(lin_off)
      H[i] <- mean(stats::plogis(lin_on) - stats::plogis(lin_off))
      grad[i, k] <- mean(gon)
      if (length(off) > 0L) grad[i, off] <- colMeans(Xpool[, off, drop = FALSE] * (gon - goff))
    }
    list(H = H, grad = grad)
  }
}

## ---------------------------------------------------------------------------
## design-law plug-ins (Z-measurable, exogenous) from stored slots
## ---------------------------------------------------------------------------

#' Per-attribute block covariances S_a = Cov(DeltaX[, block_a]) from the
#' realized design (the empirical "design_variance" weighting).
#' @keywords internal
#' @noRd
.sc_design_S_blocks <- function(object, attr_map) {
  dX <- object$deltaX
  lapply(attr_map, function(cols) stats::cov(dX[, cols, drop = FALSE]))
}

## ---------------------------------------------------------------------------
## ratio estimands (MRS / WTP): delta-method + Fieller
## ---------------------------------------------------------------------------

#' Debiased population ratio of average parameters (MRS = theta_j/theta_k,
#' WTP = -theta_j/theta_money) with delta-method SE and a Fieller interval.
#' @keywords internal
#' @noRd
.sc_debiased_ratio <- function(object, j, k, transform = c("mrs", "wtp"),
                               level = 0.95) {
  transform <- match.arg(transform)
  phi_a <- .sc_debiased_phi(object, .sc_dH_thetak(j))$phi
  phi_b <- .sc_debiased_phi(object, .sc_dH_thetak(k))$phi
  M <- length(phi_a)
  th_a <- mean(phi_a); th_b <- mean(phi_b)
  da <- phi_a - th_a; db <- phi_b - th_b
  Vaa <- sum(da * da) / (M * (M - 1))
  Vbb <- sum(db * db) / (M * (M - 1))
  Vab <- sum(da * db) / (M * (M - 1))
  z <- stats::qnorm(1 - (1 - level) / 2); z2 <- z^2

  if (transform == "mrs") {
    est  <- th_a / th_b
    Vest <- (Vaa - 2 * est * Vab + est^2 * Vbb) / th_b^2
    af <- th_b^2 - z2 * Vbb; bf <- -2 * (th_a * th_b - z2 * Vab); cf <- th_a^2 - z2 * Vaa
  } else {
    est  <- -th_a / th_b
    Vest <- Vaa / th_b^2 - 2 * th_a * Vab / th_b^3 + th_a^2 * Vbb / th_b^4
    af <- th_b^2 - z2 * Vbb; bf <- +2 * (th_a * th_b - z2 * Vab); cf <- th_a^2 - z2 * Vaa
  }
  se <- sqrt(max(Vest, 0))
  fie <- .sc_fieller(af, bf, cf)
  list(estimate = est, se = se,
       ci_lo = est - z * se, ci_hi = est + z * se,
       fieller_lo = fie$lo, fieller_hi = fie$hi, fieller_type = fie$type)
}

#' Fieller interval solver for `a w^2 + b w + c <= 0`.
#' @keywords internal
#' @noRd
.sc_fieller <- function(a, b, c) {
  disc <- b^2 - 4 * a * c
  if (a > 0) {
    if (disc < 0) return(list(lo = NA_real_, hi = NA_real_, type = "empty"))
    r <- sqrt(disc)
    list(lo = (-b - r) / (2 * a), hi = (-b + r) / (2 * a), type = "bounded")
  } else if (disc < 0) {
    list(lo = -Inf, hi = Inf, type = "all_real")
  } else {
    r <- sqrt(disc)
    list(lo = min((-b - r) / (2 * a), (-b + r) / (2 * a)),
         hi = max((-b - r) / (2 * a), (-b + r) / (2 * a)), type = "exclusive")
  }
}

## ---------------------------------------------------------------------------
## importance shares (vector estimand: simplex Jacobian on the numerators)
## ---------------------------------------------------------------------------

#' Debiased attribute-importance shares with a clustered simplex-Jacobian SE.
#' Returns a data frame: attribute, share, se, ci_lo, ci_hi.
#' @keywords internal
#' @noRd
.sc_debiased_importance <- function(object, attr_map, level = 0.95) {
  Kattr <- length(attr_map)
  S_blocks <- .sc_design_S_blocks(object, attr_map)
  ## respondent-level numerator means phi_N (M x Kattr)
  phiN <- vapply(seq_len(Kattr), function(a)
    .sc_debiased_phi(object, .sc_dH_importance_num(attr_map[[a]], S_blocks[[a]]))$phi,
    numeric(length(unique(object$respondent_id))))
  if (is.null(dim(phiN))) phiN <- matrix(phiN, ncol = Kattr)
  M <- nrow(phiN)
  N_hat <- colMeans(phiN); D_hat <- sum(N_hat); share <- N_hat / D_hat
  Jmat <- (diag(Kattr) - matrix(share, Kattr, Kattr, byrow = TRUE)) / D_hat
  phiN_c <- sweep(phiN, 2L, N_hat); V_N <- crossprod(phiN_c) / (M * (M - 1))
  V_share <- Jmat %*% V_N %*% t(Jmat)
  se <- sqrt(pmax(diag(V_share), 0))
  z  <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(attribute = names(attr_map), share = share, se = se,
             ci_lo = share - z * se, ci_hi = share + z * se,
             row.names = NULL)
}
