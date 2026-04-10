#' Indifference sets: per-respondent MRS between two attributes
#'
#' Computes the marginal rate of substitution between `attr1` and
#' `attr2` for each respondent in the (optional) subgroup:
#' \deqn{\mathrm{MRS}_i = -\frac{\hat\beta_{\mathrm{attr1}}(Z_i)}
#' {\hat\beta_{\mathrm{attr2}}(Z_i)}}
#'
#' This is a generalisation of [sc_mrs()] that returns the full
#' per-respondent distribution rather than only the trimmed mean,
#' enabling users to plot indifference curves and study trade-off
#' heterogeneity.
#'
#' @param object An `sc_fit`.
#' @param attr1,attr2 Attribute identifiers accepted by
#'   `.sc_parse_dummy_name()` (`"attribute:level"`, bare dummy name,
#'   or integer column index).
#' @param trim Length-2 numeric with lower/upper quantiles for
#'   trimming the MRS distribution.  Default `c(0.01, 0.99)`.
#' @param subgroup Optional row selector.
#' @return An `sc_quantity` whose `estimate` is a data.frame with
#'   columns `respondent`, `mrs`, `beta_attr1`, `beta_attr2`, and
#'   whose `details` contain summary statistics.
#' @export
sc_indifference <- function(object, attr1, attr2,
                            trim = c(0.01, 0.99),
                            subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  if (!is.numeric(trim) || length(trim) != 2L ||
      trim[1L] < 0 || trim[2L] > 1 || trim[1L] >= trim[2L]) {
    stop("sc_indifference(): `trim` must be c(q_lo, q_hi) with 0 <= q_lo < q_hi <= 1.")
  }
  j1 <- .sc_parse_dummy_name(object, attr1)
  j2 <- .sc_parse_dummy_name(object, attr2)
  B <- object$beta_hat
  S <- .sc_resolve_subgroup(object, subgroup)
  b1 <- B[S, j1]
  b2 <- B[S, j2]
  mrs_raw <- -b1 / b2
  mrs_raw[!is.finite(mrs_raw)] <- NA_real_
  ok <- !is.na(mrs_raw)
  ## Build full per-respondent data.frame (including NAs for context)
  df <- data.frame(
    respondent  = object$respondent_id[S],
    mrs         = mrs_raw,
    beta_attr1  = b1,
    beta_attr2  = b2,
    stringsAsFactors = FALSE,
    row.names   = NULL
  )
  ## Trimmed summary
  mrs_ok   <- mrs_raw[ok]
  resp_ok  <- object$respondent_id[S][ok]
  if (length(mrs_ok) < 2L) {
    stop("sc_indifference(): fewer than 2 finite MRS values in subgroup.")
  }
  q_lo <- stats::quantile(mrs_ok, trim[1L], names = FALSE)
  q_hi <- stats::quantile(mrs_ok, trim[2L], names = FALSE)
  mrs_trim <- pmin(pmax(mrs_ok, q_lo), q_hi)
  est <- mean(mrs_trim)
  se  <- .sc_cluster_se(mrs_trim, resp_ok)
  ci  <- .sc_ci_normal(est, se)
  .sc_quantity(
    name = "indifference",
    estimate = df,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      trimmed_mean    = est,
      median          = stats::median(mrs_ok),
      q25             = stats::quantile(mrs_ok, 0.25, names = FALSE),
      q75             = stats::quantile(mrs_ok, 0.75, names = FALSE),
      sd              = stats::sd(mrs_ok),
      n_finite        = length(mrs_ok),
      n_trimmed       = sum(mrs_ok < q_lo | mrs_ok > q_hi),
      trim_thresh     = c(q_lo, q_hi),
      attr1           = attr1,
      attr2           = attr2,
      subgroup_size   = length(S),
      se_method       = "respondent-clustered"
    ),
    call = match.call()
  )
}
