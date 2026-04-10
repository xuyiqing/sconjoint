#' Log-sum consumer surplus over a set of profiles
#'
#' For each respondent \eqn{i}, computes the inclusive-value
#' \eqn{\mathrm{CS}_i = \log\bigl(\sum_j \exp(V_{ij})\bigr)} where
#' \eqn{V_{ij} = X_j^\top \hat\beta(Z_i)} for each profile \eqn{j} in
#' the supplied choice set.
#' The log-sum-exp is computed with numerical stabilisation.
#' The aggregate surplus is the respondent average with clustered SE.
#'
#' @param object An `sc_fit`.
#' @param profiles A list of profile specs (each a named list as
#'   accepted by [sc_counterfactual()]).
#' @param subgroup Optional row selector.
#' @return An `sc_quantity` with scalar estimate, clustered SE, and
#'   normal-approx CI.
#' @export
sc_surplus <- function(object, profiles, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  if (!is.list(profiles) || length(profiles) < 1L) {
    stop("sc_surplus(): `profiles` must be a non-empty list of profile specs.")
  }
  ## Convert each profile to dummy vector
  dummies <- lapply(profiles, function(pr) .sc_profile_to_dummies(object, pr))
  Bm <- object$beta_hat
  S  <- .sc_resolve_subgroup(object, subgroup)
  Bs <- Bm[S, , drop = FALSE]
  ## For each respondent, compute V_j for every profile, then logsumexp
  J <- length(dummies)
  ## V matrix: |S| x J
  V <- matrix(NA_real_, nrow = length(S), ncol = J)
  for (j in seq_len(J)) {
    V[, j] <- as.numeric(Bs %*% dummies[[j]])
  }
  cs_i <- apply(V, 1L, .sc_logsumexp)
  est <- mean(cs_i)
  se  <- .sc_cluster_se(cs_i, object$respondent_id[S])
  ci  <- .sc_ci_normal(est, se)
  .sc_quantity(
    name = "surplus",
    estimate = est,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      per_row_surplus = cs_i,
      n_profiles      = J,
      subgroup_size   = length(S),
      se_method       = "respondent-clustered"
    ),
    call = match.call()
  )
}

#' Numerically stable log-sum-exp
#' @param v Numeric vector.
#' @return Scalar `log(sum(exp(v)))`.
#' @keywords internal
#' @noRd
.sc_logsumexp <- function(v) {
  mx <- max(v)
  mx + log(sum(exp(v - mx)))
}
