#' Willingness-to-pay: signed MRS against a cost attribute
#'
#' Thin wrapper around [sc_mrs()]: `sc_wtp(object, attr, cost_attr)`
#' returns \eqn{-\widehat{\mathrm{MRS}}_{j,k}} where \eqn{j} is the
#' `attr` column and \eqn{k} is the `cost_attr` column.  The minus
#' sign converts the MRS "units of cost to maintain indifference
#' when `attr` increases by 1" into a positive willingness-to-pay
#' when the cost attribute has a negative utility coefficient.
#'
#' `cost_attr` must resolve to a single dummy column (either a
#' numeric attribute or a single non-reference level of a binary
#' factor).  Multi-level cost attributes require calling `sc_mrs()`
#' directly with the user-chosen level pair.
#'
#' @param object An `sc_fit`.
#' @param attr Numerator attribute level.
#' @param cost_attr Single-column cost attribute.
#' @param trim,subgroup Forwarded to `sc_mrs()`.
#' @return An `sc_quantity` with the sign-flipped MRS.
#' @export
sc_wtp <- function(object, attr, cost_attr,
                   trim = c(0.01, 0.99), subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  j <- .sc_parse_dummy_name(object, attr)
  k <- .sc_parse_dummy_name(object, cost_attr)
  ## Validate that cost_attr does not refer to a multi-level attribute
  ## beyond a single dummy unless it was explicitly passed as a single
  ## "attr:level" or bare dummy name.  We only check when the user
  ## passed the bare attribute variable name (e.g. "price") without a
  ## level qualifier: if that attribute has more than one dummy
  ## column, error out with a helpful message.
  if (is.character(cost_attr) && length(cost_attr) == 1L &&
      !grepl(":", cost_attr, fixed = TRUE)) {
    map <- .sc_attr_map(object)
    if (cost_attr %in% names(map) && length(map[[cost_attr]]) > 1L) {
      stop(sprintf(
        "sc_wtp: cost_attr must resolve to a single dummy column -- for multi-level cost attributes use sc_mrs directly. (Attribute '%s' has %d dummy columns.)",
        cost_attr, length(map[[cost_attr]])
      ))
    }
  }
  mrs_obj <- sc_mrs(object,
                    numerator   = attr,
                    denominator = cost_attr,
                    trim        = trim,
                    subgroup    = subgroup)
  est <- -mrs_obj$estimate
  ci_lo <- -mrs_obj$ci_hi
  ci_hi <- -mrs_obj$ci_lo
  sign_cost <- sign(mean(object$beta_hat[, k]))
  .sc_quantity(
    name = "wtp",
    estimate = est,
    se = mrs_obj$se,
    ci_lo = ci_lo,
    ci_hi = ci_hi,
    details = c(mrs_obj$details,
                list(attr = attr,
                     cost_attr = cost_attr,
                     sign_cost = sign_cost)),
    call = match.call()
  )
}
