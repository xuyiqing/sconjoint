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
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @details
#' When the fit carries an attribute-interaction term, the per-respondent
#' index includes the population-level interaction offset
#' \eqn{g(X_A) - g(X_B)} (see [sc_counterfactual()]).  The interaction
#' term enters via the regularized mean-stage estimate (`w_hat`); see
#' `?scfit` for the attenuation caveat.
#' @return An `sc_quantity` with scalar estimate (mean decisiveness),
#'   clustered SE, normal-approx CI, and details including the
#'   fraction strongly decisive.
#' @section Population claims:
#' This function describes the fitted respondent-level (MAP) estimates.
#' Under the estimand-estimator correspondence it is limited to
#' descriptive use of those fitted values: population sign shares and
#' heterogeneity magnitudes require the integrated-likelihood route
#' ([scmix_polarization()] and its design checks), and MAP fractions
#' are biased for population shares (9--13 percentage points in the
#' head-to-head simulations, with no standard errors).
#' @export
sc_decisiveness <- function(object, A, B, subgroup = NULL,
                            which_beta = c("hybrid", "dnn")) {
  stopifnot(inherits(object, "sc_fit"))
  .sc_population_claim_note("sc_decisiveness")
  which_beta <- match.arg(which_beta)
  XA <- .sc_profile_to_dummies(object, A)
  XB <- .sc_profile_to_dummies(object, B)
  dx <- XA - XB
  Bm <- .sc_pick_beta(object, which_beta)
  S  <- .sc_resolve_subgroup(object, subgroup)
  Bs <- Bm[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  lin <- as.numeric(Bs %*% dx) + .sc_int_pair_offset(object, XA, XB)
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
