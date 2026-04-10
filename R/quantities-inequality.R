#' Preference inequality across respondents
#'
#' Quantifies how heterogeneous preferences are across respondents
#' for each attribute dummy.
#'
#' With `measure = "variance"` (default), reports
#' \eqn{\mathrm{Var}(\hat\beta_k(Z_i))} for each attribute dummy
#' \eqn{k} over respondents in the subgroup.
#'
#' With `measure = "gini"`, reports the Gini coefficient of the
#' absolute values \eqn{|\hat\beta_k(Z_i)|} for each dummy.
#'
#' @param object An `sc_fit`.
#' @param measure One of `"variance"` or `"gini"`.
#' @param subgroup Optional row selector.
#' @return An `sc_quantity` with a data.frame estimate containing
#'   per-attribute inequality measures.
#' @export
sc_inequality <- function(object, measure = c("variance", "gini"),
                          subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  measure <- match.arg(measure)
  S  <- .sc_resolve_subgroup(object, subgroup)
  Bs <- object$beta_hat[S, , drop = FALSE]
  p  <- ncol(Bs)
  vals <- numeric(p)
  if (measure == "variance") {
    for (j in seq_len(p)) {
      vals[j] <- stats::var(Bs[, j])
    }
  } else {
    for (j in seq_len(p)) {
      vals[j] <- .sc_gini(Bs[, j])
    }
  }
  df <- data.frame(
    dummy_name = object$attr_names,
    measure    = measure,
    value      = vals,
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
  .sc_quantity(
    name = paste0("inequality_", measure),
    estimate = df,
    se = NA_real_,
    details = list(
      measure       = measure,
      subgroup_size = length(S)
    ),
    call = match.call()
  )
}

#' Gini coefficient of absolute values
#' @param x Numeric vector.
#' @return Scalar Gini coefficient.
#' @keywords internal
#' @noRd
.sc_gini <- function(x) {
  x <- sort(abs(x))
  n <- length(x)
  s <- sum(x)
  if (n < 2L || s == 0) return(0)
  2 * sum(seq_len(n) * x) / (n * s) - (n + 1) / n
}
