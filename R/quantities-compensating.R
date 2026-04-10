## Compensating differential.
##
## For a "benefit" dummy j and a "cost" dummy k, `sc_compensating()`
## answers: by how much of the cost attribute would each respondent
## be willing to trade off to gain one unit of the benefit?  On the
## linear utility scale this is the per-respondent ratio
## `c_i = -beta_benefit(Z_i) / beta_cost(Z_i)`.  We trim the ratio at
## the (0.01, 0.99) empirical quantiles (same rule as `sc_mrs`) to
## guard against division by small `beta_cost` values, and report the
## trimmed mean plus a respondent-clustered SE.  A companion scalar
## `frac_compensated` reports the share of respondents for whom
## benefit + cost is a net gain (beta_benefit + beta_cost >= 0) --
## this is the "fraction offset" quantity from spec §3.3 B2.

#' Compensating differential between two attribute levels
#'
#' For a "benefit" dummy and a "cost" dummy (identified by name or
#' index), compute the per-respondent compensating differential
#' \eqn{c_i = -\hat\beta_{\mathrm{benefit}}(Z_i)/\hat\beta_{\mathrm{cost}}(Z_i)}
#' -- the (signed) quantity of the cost attribute that offsets a
#' one-unit gain in the benefit attribute on the utility scale.  The
#' returned object carries three summaries:
#' \itemize{
#'   \item `estimate` -- the respondent-average compensating
#'     differential, trimmed at the user-supplied quantiles;
#'   \item `se` -- respondent-clustered empirical SE of the trimmed
#'     per-row ratio;
#'   \item `frac_compensated` -- the fraction of respondents for whom
#'     `beta_benefit + beta_cost >= 0` (the "fraction offset" from
#'     spec §3.3 B2).
#' }
#'
#' Dummy identifiers follow the same rules as `sc_mrs`: either
#' `"attribute:level"`, a bare dummy name matching `object$attr_names`,
#' or an integer column index.
#'
#' @param object An `sc_fit`.
#' @param benefit Dummy identifier for the benefit attribute level.
#' @param cost Dummy identifier for the cost attribute level.
#' @param trim Length-2 numeric, lower and upper quantile cut-offs
#'   for trimming the per-row ratio.  Defaults to `c(0.01, 0.99)`.
#' @param subgroup Optional subgroup selector (see
#'   `sc_subgroup`).
#' @return An `sc_quantity`.  `estimate` is a scalar; `details`
#'   contains `frac_compensated`, `n`, and the trimming thresholds.
#' @export
sc_compensating <- function(object,
                            benefit,
                            cost,
                            trim = c(0.01, 0.99),
                            subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  if (!is.numeric(trim) || length(trim) != 2L ||
      trim[1] < 0 || trim[2] > 1 || trim[1] >= trim[2]) {
    stop("sc_compensating(): `trim` must be a length-2 numeric in (0,1) with trim[1] < trim[2].")
  }
  b_idx <- .sc_parse_dummy_name(object, benefit)
  c_idx <- .sc_parse_dummy_name(object, cost)
  if (b_idx == c_idx) {
    stop("sc_compensating(): `benefit` and `cost` must refer to different dummies.")
  }
  B <- object$beta_hat
  S <- .sc_resolve_subgroup_ext(object, subgroup)
  if (length(S) == 0L) {
    stop("sc_compensating(): selected subgroup is empty.")
  }
  b_ben <- B[S, b_idx]
  b_cost <- B[S, c_idx]
  resp_s <- object$respondent_id[S]

  ratio <- -b_ben / b_cost
  ratio[!is.finite(ratio)] <- NA_real_
  ok <- !is.na(ratio)
  r <- ratio[ok]
  rs <- resp_s[ok]
  if (length(r) < 2L) {
    stop("sc_compensating(): fewer than 2 finite ratios after removing division-by-zero rows.")
  }
  q_lo <- stats::quantile(r, trim[1], names = FALSE)
  q_hi <- stats::quantile(r, trim[2], names = FALSE)
  rt <- pmin(pmax(r, q_lo), q_hi)
  est <- mean(rt)
  se <- .sc_cluster_se(rt, rs)

  frac_compensated <- mean((b_ben + b_cost) >= 0)

  ci <- .sc_ci_normal(est, se)
  .sc_quantity(
    name = "compensating",
    estimate = est,
    se = se,
    ci_lo = ci[1],
    ci_hi = ci[2],
    details = list(
      benefit = benefit,
      cost = cost,
      benefit_index = b_idx,
      cost_index = c_idx,
      frac_compensated = frac_compensated,
      trim = trim,
      trim_thresholds = c(q_lo, q_hi),
      n = length(rt),
      subgroup_size = length(S)
    ),
    call = match.call()
  )
}
