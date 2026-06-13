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
#' @details
#' When the fit carries an attribute-interaction term
#' (`scfit(..., interactions != "none")`), the on/off index evaluations
#' include the interaction contribution of the on/off profiles,
#' \eqn{g(X_{on}) - g(X_{off})}, marginalized over the same design pool,
#' and the orthogonal score runs on the expanded coefficient vector
#' (main effects plus identified interaction coefficients) with
#' cross-fitted nuisances.
#' @return An `sc_quantity` with a scalar `estimate`, respondent-clustered
#'   `se`, and a normal-approximation confidence interval.
#' @seealso [sc_validate_amce()] for the structural-vs-reduced-form check.
#' @examples
#' \dontrun{
#' fit <- scfit(y ~ a1 + a2 + a3 | z1 + z2, data = dat,
#'              respondent = "rid", task = "tid", profile = "pos", seed = 1L)
#' sc_ame(fit, "a1")
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
  int_spec <- if (is.null(object$interaction)) NULL else
    list(pairs = object$interaction$pairs)
  d <- .sc_debiased_scalar(object, .sc_dH_ame(k, pool, int = int_spec))
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
