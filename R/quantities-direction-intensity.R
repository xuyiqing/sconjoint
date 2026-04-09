#' Direction-vs-intensity decomposition of the preference vector
#'
#' For each dummy column \eqn{j}, reports:
#'  * **direction** \eqn{d_j = \overline{\mathrm{sign}(\hat\beta_j(Z_i))}
#'     \in [-1, 1]} — positive when most respondents favor the level,
#'     negative when most oppose, zero when evenly split.
#'  * **intensity** \eqn{u_j = \overline{|\hat\beta_j(Z_i)|} \ge 0} —
#'     how strongly the population cares about the level on average.
#'  * a scalar **separation** diagnostic \eqn{s_j = u_j(1 - |d_j|)},
#'     large when respondents care a lot but disagree on direction.
#'
#' Both components carry respondent-clustered SEs.  This is the
#' headline conceptual contribution described in the paper (§2.3,
#' §3.1); it is new in sconjoint v0.1 with no prototype counterpart.
#'
#' @param object An `sc_fit`.
#' @param subgroup Row selector.
#' @return An `sc_quantity_bivariate` carrying two `sc_quantity`
#'   sub-objects (`direction` and `intensity`) and a `separation`
#'   data.frame in `$details`.
#' @export
sc_direction_intensity <- function(object, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  B <- object$beta_hat
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- B[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  p <- ncol(B)
  sig <- sign(Bs)
  mag <- abs(Bs)
  d <- colMeans(sig)
  u <- colMeans(mag)
  se_d <- numeric(p); se_u <- numeric(p)
  for (j in seq_len(p)) {
    se_d[j] <- .sc_cluster_se(sig[, j], resp_s)
    se_u[j] <- .sc_cluster_se(mag[, j], resp_s)
  }
  ci_q <- stats::qnorm(0.975)
  df_d <- data.frame(
    dummy_name = object$attr_names,
    d = d, se_d = se_d,
    ci_lo_d = d - ci_q * se_d, ci_hi_d = d + ci_q * se_d,
    stringsAsFactors = FALSE, row.names = NULL
  )
  df_u <- data.frame(
    dummy_name = object$attr_names,
    u = u, se_u = se_u,
    ci_lo_u = u - ci_q * se_u, ci_hi_u = u + ci_q * se_u,
    stringsAsFactors = FALSE, row.names = NULL
  )
  s_sep <- u * (1 - abs(d))
  direction_q <- .sc_quantity(
    name = "direction",
    estimate = df_d,
    details = list(subgroup_size = length(S),
                   se_method = "respondent-clustered")
  )
  intensity_q <- .sc_quantity(
    name = "intensity",
    estimate = df_u,
    details = list(subgroup_size = length(S),
                   se_method = "respondent-clustered")
  )
  .sc_quantity_bivariate(
    name = "direction_intensity",
    direction = direction_q,
    intensity = intensity_q,
    details = list(
      separation = data.frame(
        dummy_name = object$attr_names,
        s_j = s_sep,
        stringsAsFactors = FALSE,
        row.names = NULL
      ),
      subgroup_size = length(S),
      se_method     = "respondent-clustered"
    ),
    call = match.call()
  )
}
