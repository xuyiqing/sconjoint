## S3 classes for structural quantities returned by `sc_*` functions.
##
## `sc_quantity` is a lightweight list carrying the point estimate, a
## (possibly NA) standard error, normal-approx 95% confidence bounds,
## a named `details` list for per-function diagnostics, and the call
## that produced it.  `sc_quantity_bivariate` is a subclass used by
## `sc_direction_intensity()` to carry two `sc_quantity` sub-objects
## (direction and intensity) side-by-side while still dispatching
## through shared `sc_quantity` methods.

#' Construct an `sc_quantity` object
#'
#' Internal constructor used by every Tier A quantity function.
#' `estimate` may be a scalar numeric or a `data.frame`.  `se`,
#' `ci_lo`, `ci_hi` may be `NA_real_` when an SE rule is not defined
#' (e.g. `sc_polarization`).
#'
#' @param name Character, name of the quantity (used by `print`).
#' @param estimate Numeric scalar or `data.frame`.
#' @param se Numeric scalar, vector, or `NA_real_`.
#' @param ci_lo,ci_hi Numeric scalar, vector, or `NA_real_`.
#' @param level Confidence level (default 0.95).
#' @param details Named list of per-function diagnostics.
#' @param call The matched call from the caller.
#' @return An object of class `c("sc_quantity", "list")`.
#' @keywords internal
#' @noRd
.sc_quantity <- function(name,
                         estimate,
                         se = NA_real_,
                         ci_lo = NA_real_,
                         ci_hi = NA_real_,
                         level = 0.95,
                         details = list(),
                         call = NULL) {
  structure(
    list(
      name     = name,
      estimate = estimate,
      se       = se,
      ci_lo    = ci_lo,
      ci_hi    = ci_hi,
      level    = level,
      details  = details,
      call     = call
    ),
    class = c("sc_quantity", "list")
  )
}

#' Construct an `sc_quantity_bivariate` object
#'
#' Used by `sc_direction_intensity()`.  Carries two `sc_quantity`
#' sub-objects (one for the direction component, one for the
#' intensity component) on a single top-level object.
#' @keywords internal
#' @noRd
.sc_quantity_bivariate <- function(name, direction, intensity,
                                   details = list(), call = NULL) {
  structure(
    list(
      name      = name,
      direction = direction,
      intensity = intensity,
      details   = details,
      call      = call
    ),
    class = c("sc_quantity_bivariate", "sc_quantity", "list")
  )
}

#' Print method for `sc_quantity`
#'
#' @param x An object of class `sc_quantity`.
#' @param digits Number of significant digits (default 4).
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.sc_quantity <- function(x, digits = 4L, ...) {
  cat(sprintf("sc_quantity: %s\n", x$name))
  est <- x$estimate
  if (is.data.frame(est)) {
    cat(sprintf("  estimate: data.frame with %d rows\n", nrow(est)))
    print(utils::head(est, 10L), digits = digits, row.names = FALSE)
    if (nrow(est) > 10L) {
      cat(sprintf("  ... %d more rows\n", nrow(est) - 10L))
    }
  } else if (length(est) == 1L) {
    se_str <- if (is.na(x$se)) "NA" else format(x$se, digits = digits)
    ci_str <- if (is.na(x$ci_lo) || is.na(x$ci_hi)) {
      "[NA, NA]"
    } else {
      sprintf("[%s, %s]",
              format(x$ci_lo, digits = digits),
              format(x$ci_hi, digits = digits))
    }
    cat(sprintf("  estimate = %s   se = %s   %g%% CI = %s\n",
                format(est, digits = digits), se_str,
                100 * x$level, ci_str))
  } else {
    print(est, digits = digits)
  }
  invisible(x)
}

#' Format method for `sc_quantity`
#'
#' Returns a one-line summary string suitable for embedding in other
#' output (e.g. `summary.sc_fit`).
#' @param x An object of class `sc_quantity`.
#' @param digits Significant digits.
#' @param ... Unused.
#' @return A character string.
#' @export
format.sc_quantity <- function(x, digits = 4L, ...) {
  est <- x$estimate
  if (is.data.frame(est)) {
    sprintf("sc_quantity '%s' (%d rows)", x$name, nrow(est))
  } else if (length(est) == 1L) {
    sprintf("sc_quantity '%s': %s (se = %s)",
            x$name, format(est, digits = digits),
            if (is.na(x$se)) "NA" else format(x$se, digits = digits))
  } else {
    sprintf("sc_quantity '%s' (length %d)", x$name, length(est))
  }
}

#' Print method for `sc_quantity_bivariate`
#' @param x An `sc_quantity_bivariate`.
#' @param digits Significant digits.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.sc_quantity_bivariate <- function(x, digits = 4L, ...) {
  cat(sprintf("sc_quantity_bivariate: %s\n", x$name))
  cat("-- direction --\n")
  print(x$direction, digits = digits)
  cat("-- intensity --\n")
  print(x$intensity, digits = digits)
  invisible(x)
}

#' Format method for `sc_quantity_bivariate`
#' @param x Bivariate quantity.
#' @param digits Significant digits.
#' @param ... Unused.
#' @return A character string.
#' @export
format.sc_quantity_bivariate <- function(x, digits = 4L, ...) {
  sprintf("sc_quantity_bivariate '%s'", x$name)
}
