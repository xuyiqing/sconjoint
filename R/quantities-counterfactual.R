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
#' @param subgroup Optional row selector, see `sc_mrs`.
#' @return An `sc_quantity` with scalar estimate, clustered SE,
#'   normal-approx CI.
#' @export
sc_counterfactual <- function(object, A, B, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  XA <- .sc_profile_to_dummies(object, A)
  XB <- .sc_profile_to_dummies(object, B)
  dx <- XA - XB
  Bm <- object$beta_hat
  S  <- .sc_resolve_subgroup(object, subgroup)
  lin <- as.numeric(Bm[S, , drop = FALSE] %*% dx)
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
