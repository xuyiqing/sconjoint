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
  w <- comp$weights
  st <- .sc_weighted_cluster_stats(matrix(comp$phi, ncol = 1L), w, level = level)
  c(estimate = st$estimate[1L], se = st$se[1L],
    ci_lo = st$ci_lo[1L], ci_hi = st$ci_hi[1L])
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
  w <- NULL
  if (!is.null(object$respondent_weights)) {
    w <- .sc_respondent_weight_object(object$respondent_id,
                                      object$respondent_weights)$w
  }
  list(phi = phi, psi = psi, weights = w)
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

#' Debiased counterfactual vote share for an exogenous contrast
#'
#' Computes the debiased (orthogonal-score) win probability
#' \eqn{E[G(c^\top f(Z))]} for an analyst-supplied contrast vector
#' \eqn{c} in the attribute-dummy space, with a respondent-clustered
#' standard error. This is the raw-contrast counterpart of
#' [sc_counterfactual()]: that function derives the contrast from two
#' profile lists, whereas this one takes the contrast directly in dummy
#' space (useful when the contrast is not a single A-vs-B profile swap,
#' e.g. a coded difference variable or a multi-attribute plan).
#'
#' @param object An `sc_fit`.
#' @param contrast Numeric contrast vector \eqn{c}. Either length
#'   `ncol(object$beta_hat)` (assumed in dummy order) or a named vector
#'   whose names match the fit's attribute dummies.
#' @param level Confidence level (default 0.95).
#' @return An `sc_quantity` with the debiased estimate, clustered SE, and
#'   normal-approximation confidence bounds.
#' @seealso [sc_counterfactual()]
#' @export
sc_voteshare_contrast <- function(object, contrast, level = 0.95) {
  stopifnot(inherits(object, "sc_fit"))
  nm <- colnames(object$beta_hat)
  cvec <- numeric(length(nm))
  names(cvec) <- nm
  if (is.null(names(contrast))) {
    if (length(contrast) != length(nm)) {
      stop("`contrast` must have length ", length(nm),
           " (the number of attribute dummies) or be a named vector ",
           "matching the fit's attribute dummies.", call. = FALSE)
    }
    cvec[] <- contrast
  } else {
    bad <- setdiff(names(contrast), nm)
    if (length(bad)) {
      stop("`contrast` names not in the fit: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
    cvec[names(contrast)] <- contrast
  }
  d <- .sc_debiased_scalar(object, .sc_dH_voteshare(cvec), level = level)
  if (!is.na(d["estimate"]) && (d["estimate"] < 0 || d["estimate"] > 1)) {
    warning("sc_voteshare_contrast(): the orthogonal one-step estimate (",
            sprintf("%.3f", d["estimate"]), ") lies outside [0, 1]; the ",
            "linearized correction is unreliable for extreme contrasts.",
            call. = FALSE)
  }
  .sc_quantity(
    name = "voteshare_contrast",
    estimate = unname(d["estimate"]), se = unname(d["se"]),
    ci_lo = unname(d["ci_lo"]), ci_hi = unname(d["ci_hi"]),
    level = level,
    details = list(contrast = cvec, vartype = "orthogonal",
                   se_method = "debiased orthogonal score, respondent-clustered"),
    call = match.call())
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
#'   `H(f) = E_X[G(f_k + X_{-k}'f_{-k}) - G(X_{-k}'f_{-k})]`.
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

#' Per-attribute block covariances S_a = `Cov(DeltaX[, block_a])` from the
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
  comp_b <- .sc_debiased_phi(object, .sc_dH_thetak(k))
  phi_b <- comp_b$phi
  st <- .sc_weighted_cluster_stats(cbind(phi_a, phi_b), comp_b$weights, level = level)
  th_a <- st$estimate[1L]; th_b <- st$estimate[2L]
  Vaa <- st$vcov[1L, 1L]
  Vbb <- st$vcov[2L, 2L]
  Vab <- st$vcov[1L, 2L]
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
  w <- NULL
  if (!is.null(object$respondent_weights)) {
    w <- .sc_respondent_weight_object(object$respondent_id,
                                      object$respondent_weights)$w
  }
  stN <- .sc_weighted_cluster_stats(phiN, w, level = level)
  N_hat <- stN$estimate; D_hat <- sum(N_hat); share <- N_hat / D_hat
  Jmat <- (diag(Kattr) - matrix(share, Kattr, Kattr, byrow = TRUE)) / D_hat
  V_N <- stN$vcov
  V_share <- Jmat %*% V_N %*% t(Jmat)
  se <- sqrt(pmax(diag(V_share), 0))
  z  <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(attribute = names(attr_map), share = share, se = se,
             ci_lo = share - z * se, ci_hi = share + z * se,
             row.names = NULL)
}
