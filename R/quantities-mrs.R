#' Marginal rate of substitution between two attribute dummies
#'
#' Computes the respondent-averaged trimmed ratio
#' \eqn{r_i = \beta_j(Z_i) / \beta_k(Z_i)} between a `numerator` and
#' `denominator` attribute level, trimming the raw ratio at the
#' user-specified quantiles.  The SE is clustered at the respondent
#' level.
#'
#' Point estimate and trimming follow the prototype
#' `07b_structural_quantities.R` (lines 54--112).  The clustered SE
#' is new in sconjoint v0.1 (the prototype reports a plain delta-method
#' SE which is retained inside `$details$delta_se` as a diagnostic).
#'
#' @param object An `sc_fit`.
#' @param numerator,denominator Character of form `"attribute:level"`,
#'   or a bare dummy column name matching `object$attr_names`, or an
#'   integer column index.
#' @param trim Length-2 numeric with the lower and upper quantiles at
#'   which the raw ratio is clipped.  Default `c(0.01, 0.99)`.
#' @param subgroup Optional logical / integer / Z column-name vector
#'   selecting a subset of rows.  See Details.
#' @return An `sc_quantity` with scalar `estimate`, clustered `se`,
#'   normal-approx CI, and `details` including the prototype delta SE.
#' @examples
#' \dontrun{
#' fit <- scfit(y ~ a1 + a2 + a3 | z1 + z2, data = dat,
#'              respondent = "rid", task = "tid", profile = "pos",
#'              seed = 1L)
#' sc_mrs(fit, numerator = "a1", denominator = "a2")
#' }
#' @export
sc_mrs <- function(object,
                   numerator,
                   denominator,
                   trim = c(0.01, 0.99),
                   subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  if (!is.numeric(trim) || length(trim) != 2L ||
      trim[1L] < 0 || trim[2L] > 1 || trim[1L] >= trim[2L]) {
    stop("sc_mrs(): `trim` must be c(q_lo, q_hi) with 0 <= q_lo < q_hi <= 1.")
  }
  j <- .sc_parse_dummy_name(object, numerator)
  k <- .sc_parse_dummy_name(object, denominator)
  B <- object$beta_hat
  resp <- object$respondent_id
  S <- .sc_resolve_subgroup(object, subgroup)
  r_raw <- B[S, j] / B[S, k]
  r_raw[!is.finite(r_raw)] <- NA_real_
  ok <- !is.na(r_raw)
  r_raw <- r_raw[ok]
  resp_s <- resp[S][ok]
  if (length(r_raw) < 2L) {
    stop("sc_mrs(): fewer than 2 finite ratios in subgroup.")
  }
  q_lo <- stats::quantile(r_raw, trim[1L], names = FALSE)
  q_hi <- stats::quantile(r_raw, trim[2L], names = FALSE)
  r_trim <- pmin(pmax(r_raw, q_lo), q_hi)
  est <- mean(r_trim)
  se  <- .sc_cluster_se(r_trim, resp_s)
  ci  <- .sc_ci_normal(est, se)
  ## prototype "delta SE" diagnostic: plain plug-in se of the ratio mean
  delta_se <- if (length(r_trim) > 1L) {
    stats::sd(r_trim) / sqrt(length(r_trim))
  } else NA_real_
  .sc_quantity(
    name = "mrs",
    estimate = est,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      median         = stats::median(r_trim),
      q25            = stats::quantile(r_trim, 0.25, names = FALSE),
      q75            = stats::quantile(r_trim, 0.75, names = FALSE),
      n_trimmed      = sum(r_raw < q_lo | r_raw > q_hi),
      trim_thresh    = c(q_lo, q_hi),
      delta_se       = delta_se,
      subgroup_size  = length(r_trim),
      numerator      = numerator,
      denominator    = denominator,
      se_method      = "respondent-clustered"
    ),
    call = match.call()
  )
}
