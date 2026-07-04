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
#' \donttest{
#' if (requireNamespace("torch", quietly = TRUE) &&
#'     torch::torch_is_installed()) {
#'   ## Tiny synthetic conjoint: 40 respondents, 2 tasks, 2 attributes,
#'   ## 1 respondent covariate (see ?scfit for a larger example).
#'   set.seed(1)
#'   M <- 40; T_i <- 2; p <- 2
#'   Z_mat <- matrix(stats::rnorm(M), M, 1)
#'   rid <- rep(seq_len(M), each = T_i)
#'   dX  <- matrix(sample(c(-1, 0, 1), M * T_i * p, replace = TRUE),
#'                 M * T_i, p)
#'   logit <- 0.5 * dX[, 1] * (1 + Z_mat[rid, 1]) - 0.4 * dX[, 2]
#'   y <- stats::rbinom(M * T_i, 1, stats::plogis(logit))
#'   long <- data.frame(
#'     rid = rep(rid, each = 2),
#'     tid = rep(rep(seq_len(T_i), M), each = 2),
#'     pos = rep(c(1L, 2L), M * T_i),
#'     a1  = as.vector(rbind(dX[, 1], 0)),
#'     a2  = as.vector(rbind(dX[, 2], 0)),
#'     z1  = rep(Z_mat[rid, 1], each = 2),
#'     y   = as.vector(rbind(y, 1 - y))
#'   )
#'   fit <- scfit(y ~ a1 + a2 | z1, data = long,
#'                respondent = "rid", task = "tid", profile = "pos",
#'                K = 2, n_epochs = 20, seed = 1)
#'   ## Profile with a1 set to 1, a2 left at its reference value.
#'   sc_profile(fit, a1 = 1)
#' }
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
