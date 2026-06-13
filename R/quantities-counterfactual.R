#' Counterfactual choice probability between two profiles
#'
#' For each row \eqn{i} in the (optional) subgroup, computes
#' \eqn{\hat p_i = G((X_A - X_B)^\top \hat\beta(Z_i))}, then averages
#' over \eqn{i}.  The SE is the respondent-clustered empirical SD of
#' the per-row probabilities (not an influence-function SE on the DML
#' \eqn{\hat\theta}); v0.1 does not propagate nuisance uncertainty
#' from \eqn{\hat\beta(Z)} into \eqn{\hat p}.
#'
#' `A`, `B` are named lists in human-readable form, e.g.
#' `A = list(gender = "female", talent = "hard_working")`; attributes
#' not mentioned default to the reference level.  The conversion from
#' `(A, B)` to encoded dummy vectors follows the prototype
#' `07b_structural_quantities.R` lines 153--175 (`profile_to_dummies`).
#'
#' @param object An `sc_fit`.
#' @param A,B Named lists describing the two profiles.
#' @param vartype Either `"orthogonal"` (default) for the debiased
#'   counterfactual probability \eqn{E[G((X_A - X_B)^\top f(Z))]} -- the
#'   orthogonal score on the Stage-1 first stage, with a respondent-clustered
#'   standard error that propagates first-stage uncertainty -- or `"plugin"`
#'   for the v0.1 average of per-respondent probabilities (which does not).
#'   The `"orthogonal"` estimand is a population average and ignores
#'   `subgroup` and `which_beta`.
#' @param subgroup Optional row selector, see `sc_mrs`.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @details
#' When the fit carries an attribute-interaction term
#' (`scfit(..., interactions != "none")`), the choice index includes the
#' interaction offset: \eqn{(X_A - X_B)^\top \beta + g(X_A) - g(X_B)}.
#' The `"plugin"` path adds the population-level offset built from
#' `object$interaction$w_hat`; the `"orthogonal"` path extends the
#' contrast with the identified interaction features of the pair and
#' scores the expanded coefficient vector with cross-fitted nuisances.
#' On the `"plugin"` path the interaction term enters via the
#' regularized mean-stage estimate (`w_hat`); see `?scfit` for the
#' attenuation caveat.
#' @return An `sc_quantity` with scalar estimate, clustered SE,
#'   normal-approx CI.
#' @export
sc_counterfactual <- function(object, A, B,
                              vartype = c("orthogonal", "plugin"),
                              subgroup = NULL,
                              which_beta = c("hybrid", "dnn")) {
  stopifnot(inherits(object, "sc_fit"))
  vartype <- match.arg(vartype)
  which_beta <- match.arg(which_beta)
  XA <- .sc_profile_to_dummies(object, A)
  XB <- .sc_profile_to_dummies(object, B)
  dx <- XA - XB

  if (vartype == "orthogonal") {
    cvec <- dx
    if (!is.null(object$interaction)) {
      f_pair <- .sc_int_features_profile(XA, object$interaction$pairs) -
        .sc_int_features_profile(XB, object$interaction$pairs)
      cvec <- c(dx, as.numeric(f_pair))
    }
    d <- .sc_debiased_scalar(object, .sc_dH_voteshare(cvec))
    if (!is.na(d["estimate"]) && (d["estimate"] < 0 || d["estimate"] > 1)) {
      warning("sc_counterfactual(): the orthogonal one-step estimate (",
              sprintf("%.3f", d["estimate"]), ") lies outside [0, 1]; the ",
              "linearized correction is unreliable for extreme contrasts. ",
              "Compare `vartype = \"plugin\"` and interpret with caution.",
              call. = FALSE)
    }
    return(.sc_quantity(
      name = "counterfactual", estimate = unname(d["estimate"]),
      se = unname(d["se"]), ci_lo = unname(d["ci_lo"]), ci_hi = unname(d["ci_hi"]),
      details = list(delta_x = dx, profile_A = A, profile_B = B,
                     vartype = "orthogonal",
                     se_method = "debiased orthogonal score, respondent-clustered"),
      call = match.call()))
  }

  Bm <- .sc_pick_beta(object, which_beta)
  S  <- .sc_resolve_subgroup(object, subgroup)
  lin <- as.numeric(Bm[S, , drop = FALSE] %*% dx) +
    .sc_int_pair_offset(object, XA, XB)
  p_i <- stats::plogis(lin)
  est <- mean(p_i)
  se  <- .sc_cluster_se(p_i, object$respondent_id[S])
  ci  <- .sc_ci_normal(est, se)
  .sc_quantity(
    name = "counterfactual",
    estimate = est,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      per_row_prob = p_i,
      delta_x      = dx,
      profile_A    = A,
      profile_B    = B,
      subgroup_size = length(S),
      se_method    = "respondent-clustered"
    ),
    call = match.call()
  )
}
