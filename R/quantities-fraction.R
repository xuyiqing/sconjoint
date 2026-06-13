#' Fraction of respondents with beta above / below a threshold, per dummy
#'
#' For every dummy column \eqn{j} and row subgroup \eqn{S}, computes
#' \eqn{\mathrm{frac}^+_j(\tau) = \Pr_{i\in S}\{\hat\beta_j(Z_i) > \tau\}}
#' and the symmetric lower-tail fraction
#' \eqn{\mathrm{frac}^-_j(\tau) = \Pr_{i\in S}\{\hat\beta_j(Z_i) < -\tau\}}.
#' The default threshold is \eqn{\tau = 0}, giving the fraction of
#' respondents preferring each non-reference level over the reference.
#'
#' Point estimate is a direct port of
#' `07b_structural_quantities.R` lines 478--486 (`frac_positive` /
#' `frac_negative` columns).  Clustered SEs on the Bernoulli
#' indicators are new in sconjoint v0.1.
#'
#'
#' When the fit carries an attribute-interaction term
#' (`scfit(..., interactions != "none")`), this quantity is computed from
#' the per-respondent `beta_i` exactly as before, but `beta_i` is then
#' the MAIN-EFFECT part of the utility (the coefficient on `deltaX` at
#' the no-interaction baseline), not the all-else-equal effect.
#' @param object An `sc_fit`.
#' @param threshold Non-negative scalar `tau`.
#' @param subgroup Row selector.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @return An `sc_quantity` with `estimate` a data.frame.
#' @export
sc_fraction_preferring <- function(object, threshold = 0, subgroup = NULL,
                                   which_beta = c("hybrid", "dnn")) {
  stopifnot(inherits(object, "sc_fit"))
  which_beta <- match.arg(which_beta)
  if (!is.numeric(threshold) || length(threshold) != 1L || threshold < 0) {
    stop("sc_fraction_preferring(): `threshold` must be a non-negative scalar.")
  }
  B <- .sc_pick_beta(object, which_beta)
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- B[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  p <- ncol(B)
  fp <- colMeans(Bs > threshold)
  fn <- colMeans(Bs < -threshold)
  se_p <- numeric(p)
  se_n <- numeric(p)
  for (j in seq_len(p)) {
    se_p[j] <- .sc_cluster_se(as.numeric(Bs[, j] > threshold), resp_s)
    se_n[j] <- .sc_cluster_se(as.numeric(Bs[, j] < -threshold), resp_s)
  }
  ci_q <- stats::qnorm(0.975)
  df <- data.frame(
    dummy_name       = object$attr_names,
    frac_positive    = fp,
    frac_negative    = fn,
    se_positive      = se_p,
    se_negative      = se_n,
    ci_lo_positive   = fp - ci_q * se_p,
    ci_hi_positive   = fp + ci_q * se_p,
    ci_lo_negative   = fn - ci_q * se_n,
    ci_hi_negative   = fn + ci_q * se_n,
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
  .sc_quantity(
    name = "fraction_preferring",
    estimate = df,
    se = NA_real_,
    details = list(threshold = threshold, subgroup_size = length(S)),
    call = match.call()
  )
}
