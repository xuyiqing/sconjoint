#' Welfare change between two choice sets
#'
#' For each respondent \eqn{i}, computes
#' \eqn{\Delta\mathrm{CS}_i = \mathrm{CS}_i(\mathrm{new}) -
#' \mathrm{CS}_i(\mathrm{old})} where the consumer surplus in each
#' choice set is the log-sum-exp inclusive value (see [sc_surplus()]).
#' The aggregate welfare change is the respondent average with
#' clustered SE.
#'
#' @param object An `sc_fit`.
#' @param old_set,new_set Lists of profile specs (each element a named
#'   list as accepted by [sc_counterfactual()]).
#' @param subgroup Optional row selector.
#' @return An `sc_quantity` with scalar estimate, clustered SE, and
#'   normal-approx CI.
#' @export
sc_welfare_change <- function(object, old_set, new_set, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  if (!is.list(old_set) || length(old_set) < 1L) {
    stop("sc_welfare_change(): `old_set` must be a non-empty list of profile specs.")
  }
  if (!is.list(new_set) || length(new_set) < 1L) {
    stop("sc_welfare_change(): `new_set` must be a non-empty list of profile specs.")
  }
  ## Helper: compute per-respondent logsumexp for a choice set
  .cs_vec <- function(profiles, Bs) {
    dummies <- lapply(profiles, function(pr) .sc_profile_to_dummies(object, pr))
    V <- matrix(NA_real_, nrow = nrow(Bs), ncol = length(dummies))
    for (j in seq_along(dummies)) {
      V[, j] <- as.numeric(Bs %*% dummies[[j]])
    }
    apply(V, 1L, .sc_logsumexp)
  }
  Bm <- object$beta_hat
  S  <- .sc_resolve_subgroup(object, subgroup)
  Bs <- Bm[S, , drop = FALSE]
  cs_old <- .cs_vec(old_set, Bs)
  cs_new <- .cs_vec(new_set, Bs)
  dcs_i  <- cs_new - cs_old
  est <- mean(dcs_i)
  se  <- .sc_cluster_se(dcs_i, object$respondent_id[S])
  ci  <- .sc_ci_normal(est, se)
  .sc_quantity(
    name = "welfare_change",
    estimate = est,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      per_row_delta   = dcs_i,
      n_old           = length(old_set),
      n_new           = length(new_set),
      subgroup_size   = length(S),
      se_method       = "respondent-clustered"
    ),
    call = match.call()
  )
}
