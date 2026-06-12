## Attribute-interaction helpers for sconjoint.
##
## Implements the population-level interaction extension of the profile
## utility:  u(X) = X' beta_i + g(X), with either
##
##   * "lowrank"  : g(X) = ||V' X||^2, V a p x r parameter trained inside
##     the torch mean stage (weight-decayed by `lambda_V`); or
##   * "explicit" : g(X_A) - g(X_B) = (q_A - q_B)' w over the IDENTIFIED
##     interaction features only, with w a ridge-penalized linear head.
##
## The choice index is always the difference of profile utilities,
## deltaX' beta_i + g(X_A) - g(X_B).  The quadratic-in-the-DIFFERENCE form
## ||V' deltaX||^2 is deliberately NOT used: it is invariant under an
## A <-> B profile swap (so the implied P(choose A) after a swap is not
## 1 - P(choose A)) and corresponds to no profile-separable random-utility
## model.  See the 2026-06-12 feasibility memo and the regression test in
## tests/testthat/test-interactions.R.
##
## Identification accounting under one-hot dummy coding (all attributes
## factors):  of the p(p+1)/2 raw quadratic terms,
##   * diagonal terms X_k^2 = X_k are collinear with the main effects
##     (absorbed into beta);
##   * within-attribute pairs X_k X_l = 0 (mutually exclusive levels) are
##     structurally zero;
##   * only the CROSS-ATTRIBUTE pairs remain identified.
## Consequently only the cross-attribute blocks of W = V V' are identified
## (never V itself), and the diagonal of W is absorbed into the main
## effects at extraction time so that `beta_hat` is the coefficient on
## deltaX in the identified linear-in-parameters representation.
##
## All functions in this file are internal.

#' Map dummy columns to attribute ids
#'
#' @param attr_map Named list mapping attribute name -> integer column
#'   indices (as stored on `sc_fit$attr_map` / returned by `.sc_encode()`).
#' @param p Integer, total number of dummy columns.
#' @return Integer vector of length `p`; entry k is the attribute id of
#'   dummy column k.
#' @keywords internal
#' @noRd
.sc_int_attr_assign <- function(attr_map, p) {
  assign_vec <- integer(p)
  for (a in seq_along(attr_map)) {
    assign_vec[attr_map[[a]]] <- a
  }
  if (any(assign_vec == 0L)) {
    stop(".sc_int_attr_assign(): `attr_map` does not cover all columns.")
  }
  assign_vec
}

#' Identified (cross-attribute) interaction pairs
#'
#' Enumerates all pairs (k, l), k < l, of dummy columns belonging to
#' DIFFERENT attributes.  Within-attribute pairs are structurally zero
#' under one-hot coding and diagonal terms are collinear with the main
#' effects, so neither is included.
#'
#' @param attr_assign Integer vector mapping dummy column -> attribute id
#'   (from `.sc_int_attr_assign()`).
#' @param x_names Optional character vector of dummy column names, used to
#'   label the pairs.
#' @return A list with `pairs` (q x 2 integer matrix of column indices)
#'   and `names` (length-q character, `"colk:coll"`).
#' @keywords internal
#' @noRd
.sc_int_pairs <- function(attr_assign, x_names = NULL) {
  p <- length(attr_assign)
  if (p < 2L) {
    return(list(pairs = matrix(integer(0), 0L, 2L), names = character(0)))
  }
  pairs_all <- t(utils::combn(p, 2L))
  cross <- attr_assign[pairs_all[, 1L]] != attr_assign[pairs_all[, 2L]]
  pairs <- pairs_all[cross, , drop = FALSE]
  nms <- if (!is.null(x_names)) {
    paste0(x_names[pairs[, 1L]], ":", x_names[pairs[, 2L]])
  } else {
    paste0("x", pairs[, 1L], ":x", pairs[, 2L])
  }
  list(pairs = pairs, names = nms)
}

#' Interaction features of single profiles
#'
#' For each row X of `Xmat` and each identified pair (k, l), the feature
#' is the product `X_k * X_l`.
#'
#' @param Xmat Numeric matrix (rows = profiles, p columns) or a length-p
#'   vector (treated as one profile).
#' @param pairs q x 2 integer matrix of column-index pairs.
#' @return Numeric matrix `nrow(Xmat) x q`.
#' @keywords internal
#' @noRd
.sc_int_features_profile <- function(Xmat, pairs) {
  if (is.null(dim(Xmat))) {
    Xmat <- matrix(Xmat, nrow = 1L)
  }
  q <- nrow(pairs)
  out <- matrix(0, nrow(Xmat), q)
  for (j in seq_len(q)) {
    out[, j] <- Xmat[, pairs[j, 1L]] * Xmat[, pairs[j, 2L]]
  }
  out
}

#' Difference-of-quadratics interaction features of a profile pair
#'
#' Feature j is `XA_k XA_l - XB_k XB_l` for the j-th identified pair
#' (k, l).  This is the regressor that multiplies the interaction
#' coefficient in the identified linear-in-parameters representation of
#' the choice index.
#'
#' @param XA,XB Numeric matrices (rows = tasks, p columns).
#' @param pairs q x 2 integer matrix.
#' @return Numeric matrix `nrow(XA) x q`.
#' @keywords internal
#' @noRd
.sc_int_features <- function(XA, XB, pairs) {
  .sc_int_features_profile(XA, pairs) - .sc_int_features_profile(XB, pairs)
}

#' Extract interaction parameters from a trained network
#'
#' Returns the coefficients of the identified linear-in-parameters
#' representation implied by the trained head:
#'
#' * `"lowrank"`: with `W = V V'`, the index satisfies (on the one-hot
#'   design support)  `g(X_A) - g(X_B) = diag(W)' deltaX + sum_{(k,l)}
#'   2 W_kl (XA_k XA_l - XB_k XB_l)` over the identified cross-attribute
#'   pairs.  So `w = 2 * W[pairs]` and `beta_shift = diag(W)` (the
#'   diagonal mass absorbed into the main effects).
#' * `"explicit"`: `w` is the trained linear head itself; `beta_shift = 0`.
#'
#' @param net Trained `nn_module` from `.sc_build_network()`.
#' @param interactions `"lowrank"` or `"explicit"`.
#' @param pairs q x 2 integer matrix of identified pairs.
#' @param p Integer number of dummy columns.
#' @return List with `w` (length-q), `beta_shift` (length-p), `V`
#'   (p x r matrix or NULL), `W` (p x p matrix or NULL).
#' @keywords internal
#' @noRd
.sc_int_extract <- function(net, interactions, pairs, p) {
  if (identical(interactions, "lowrank")) {
    V <- as.matrix(torch::as_array(net$V$detach()))
    W <- V %*% t(V)
    w <- 2 * W[pairs]
    list(w = as.numeric(w), beta_shift = diag(W), V = V, W = W)
  } else if (identical(interactions, "explicit")) {
    w <- as.numeric(torch::as_array(net$w_int$detach()))
    list(w = w, beta_shift = rep(0, p), V = NULL, W = NULL)
  } else {
    stop(".sc_int_extract(): no interaction head on this network.")
  }
}

#' Interaction offset for analyst-supplied profile pairs
#'
#' Computes `g(x_a) - g(x_b)` for a quantity function evaluating the
#' choice index at a profile pair, using the fit's population-level
#' interaction coefficients on the identified features.  Returns 0 when
#' the fit carries no interaction term, so callers can add it
#' unconditionally.
#'
#' @param object An `sc_fit`.
#' @param xa,xb Length-p numeric dummy vectors (e.g. from
#'   `.sc_profile_to_dummies()`).  `xb` may be NULL for "versus the
#'   all-reference profile" evaluations (g(reference) = 0).
#' @return A scalar offset.
#' @keywords internal
#' @noRd
.sc_int_pair_offset <- function(object, xa, xb = NULL) {
  int <- object$interaction
  if (is.null(int)) return(0)
  fa <- .sc_int_features_profile(xa, int$pairs)
  ga <- as.numeric(fa %*% int$w_hat)
  if (is.null(xb)) return(ga)
  fb <- .sc_int_features_profile(xb, int$pairs)
  ga - as.numeric(fb %*% int$w_hat)
}
