#' Debiased structural average marginal effect (probability scale)
#'
#' The structural AME of an attribute level is the average change in choice
#' probability from turning that level on, integrated over the design law of
#' the other attributes,
#' \deqn{\mathrm{AME}_k = \mathrm{E}_Z\, \mathrm{E}_X\big[G(f_k(Z) + X_{-k}^\top f_{-k}(Z))
#'   - G(X_{-k}^\top f_{-k}(Z))\big],}
#' where the inner \eqn{\mathrm{E}_X} is taken over a pool of single profiles drawn
#' from the design (stored on the fit as `object$profile_pool`).  This is the
#' debiased (orthogonal-score) estimand of the paper's Appendix C: it is
#' computed on the Stage-1 first stage with a respondent-clustered standard
#' error that propagates first-stage estimation uncertainty.  Under correct
#' specification it agrees with the nonparametric AMCE (see
#' [sc_validate_amce()]), so a discrepancy beyond the combined uncertainty is
#' a misspecification signal.
#'
#' @param object An `sc_fit`.
#' @param attr Attribute level, specified as in [sc_mrs()] (an
#'   `"attribute:level"` string, a bare dummy column name, or an integer
#'   column index).
#' @return An `sc_quantity` with a scalar `estimate`, respondent-clustered
#'   `se`, and a normal-approximation confidence interval.
#' @seealso [sc_validate_amce()] for the structural-vs-reduced-form check.
#' @examples
#' \donttest{
#' if (requireNamespace("torch", quietly = TRUE) &&
#'     torch::torch_is_installed()) {
#'   ## Tiny synthetic conjoint: 40 respondents, 2 tasks, 2 attributes,
#'   ## 1 respondent covariate (see ?scfit for a larger example).
#'   set.seed(1)
#'   M <- 40; T_i <- 2; p <- 2
#'   Z_mat <- matrix(stats::rnorm(M), M, 1)
#'   rid <- rep(seq_len(M), each = T_i)
#'   dX  <- matrix(sample(c(-1, 0, 1), M * T_i * p, replace = TRUE),
#'                 M * T_i, p)
#'   logit <- 0.5 * dX[, 1] * (1 + Z_mat[rid, 1]) - 0.4 * dX[, 2]
#'   y <- stats::rbinom(M * T_i, 1, stats::plogis(logit))
#'   long <- data.frame(
#'     rid = rep(rid, each = 2),
#'     tid = rep(rep(seq_len(T_i), M), each = 2),
#'     pos = rep(c(1L, 2L), M * T_i),
#'     a1  = as.vector(rbind(dX[, 1], 0)),
#'     a2  = as.vector(rbind(dX[, 2], 0)),
#'     z1  = rep(Z_mat[rid, 1], each = 2),
#'     y   = as.vector(rbind(y, 1 - y))
#'   )
#'   fit <- scfit(y ~ a1 + a2 | z1, data = long,
#'                respondent = "rid", task = "tid", profile = "pos",
#'                K = 2, n_epochs = 20, seed = 1)
#'   sc_ame(fit, "a1")
#' }
#' }
#' @export
sc_ame <- function(object, attr) {
  stopifnot(inherits(object, "sc_fit"))
  k <- .sc_parse_dummy_name(object, attr)
  pool <- object$profile_pool
  if (is.null(pool)) {
    stop("sc_ame(): `object$profile_pool` is not stored; refit with a ",
         "current version of scfit().", call. = FALSE)
  }
  d <- .sc_debiased_scalar(object, .sc_dH_ame(k, pool))
  .sc_quantity(
    name = "ame",
    estimate = unname(d["estimate"]),
    se = unname(d["se"]),
    ci_lo = unname(d["ci_lo"]),
    ci_hi = unname(d["ci_hi"]),
    details = list(
      attr      = attr,
      vartype   = "orthogonal",
      n_pool    = nrow(pool),
      se_method = "debiased orthogonal score, respondent-clustered"
    ),
    call = match.call()
  )
}
