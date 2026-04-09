#' Build and validate a profile specification
#'
#' Small user-facing helper that wraps the internal
#' `.sc_profile_to_dummies()` port of
#' `07b_structural_quantities.R` lines 153--175.  Returns an object of
#' class `sc_profile` — a named list with the validated profile plus
#' the encoded dummy vector — ready to pass into `sc_counterfactual()`
#' or `sc_mrs()`-driven workflows.
#'
#' @param object An `sc_fit`.
#' @param ... Named `attribute = level` pairs.  Attributes not
#'   mentioned default to the reference level.  Unknown attributes or
#'   levels error out.
#' @return An object of class `sc_profile` carrying `$spec` (the named
#'   list) and `$dummies` (the numeric dummy vector).
#' @examples
#' \dontrun{
#' sc_profile(fit, gender = "female", talent = "hard_working")
#' }
#' @export
sc_profile <- function(object, ...) {
  stopifnot(inherits(object, "sc_fit"))
  spec <- list(...)
  dumm <- .sc_profile_to_dummies(object, spec)
  structure(
    list(spec = spec, dummies = dumm),
    class = c("sc_profile", "list")
  )
}

#' Print method for `sc_profile`
#' @param x An `sc_profile`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.sc_profile <- function(x, ...) {
  cat("sc_profile:\n")
  if (length(x$spec) == 0L) {
    cat("  (all attributes at reference level)\n")
  } else {
    for (nm in names(x$spec)) {
      cat(sprintf("  %s = %s\n", nm, format(x$spec[[nm]])))
    }
  }
  invisible(x)
}
