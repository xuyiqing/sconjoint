#' Choice decisiveness between two profiles
#'
#' For each respondent \eqn{i}, computes the logit choice probability
#' \eqn{p_i = G((X_A - X_B)^\top \hat\beta(Z_i))} and the
#' decisiveness index \eqn{d_i = |2p_i - 1|}.  A decisiveness of 1
#' means the respondent is certain; 0 means indifferent.
#'
#' The aggregate decisiveness is the respondent average with clustered
#' SE.  The function also reports the fraction of respondents who are
#' strongly decisive (\eqn{p_i > 0.9} or \eqn{p_i < 0.1}).
#'
#' @param object An `sc_fit`.
#' @param A,B Named lists describing the two profiles (as in
#'   [sc_counterfactual()]).
#' @param subgroup Optional row selector.
#' @return An `sc_quantity` with scalar estimate (mean decisiveness),
#'   clustered SE, normal-approx CI, and details including the
#'   fraction strongly decisive.
#' @export
sc_decisiveness <- function(object, A, B, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  XA <- .sc_profile_to_dummies(object, A)
  XB <- .sc_profile_to_dummies(object, B)
  dx <- XA - XB
  Bm <- object$beta_hat
  S  <- .sc_resolve_subgroup(object, subgroup)
  Bs <- Bm[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  lin <- as.numeric(Bs %*% dx)
  p_i <- stats::plogis(lin)
  d_i <- abs(2 * p_i - 1)
  est <- mean(d_i)
  se  <- .sc_cluster_se(d_i, resp_s)
  ci  <- .sc_ci_normal(est, se)
  strongly_decisive <- mean(p_i > 0.9 | p_i < 0.1)
  .sc_quantity(
    name = "decisiveness",
    estimate = est,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      per_row_prob         = p_i,
      per_row_decisiveness = d_i,
      frac_strongly_decisive = strongly_decisive,
      median_decisiveness  = stats::median(d_i),
      profile_A            = A,
      profile_B            = B,
      subgroup_size        = length(S),
      se_method            = "respondent-clustered"
    ),
    call = match.call()
  )
}
