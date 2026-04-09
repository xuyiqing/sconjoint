## Minimal S3 methods for `sc_fit` (M3).
##
## M3 ships only the bare minimum needed to validate the pipeline end-
## to-end: `print`, `coef`, `vcov`.  The full method set (`summary`,
## `predict`, `plot`, `confint`, `fitted`, `residuals`, `nobs`,
## `logLik`, `format`) lands in M4.

#' Print method for `sc_fit`
#'
#' One-line description: call, K, number of respondents, number of
#' attribute dummies, number of Z covariates, and seed.
#'
#' @param x An object of class `sc_fit`.
#' @param ... Unused, present for method compatibility.
#' @return `x`, invisibly.
#' @export
print.sc_fit <- function(x, ...) {
  cat("sc_fit (structural conjoint DML estimator)\n")
  cat("Call: ")
  print(x$call)
  M <- length(unique(x$respondent_id))
  p <- length(x$theta)
  p_Z <- ncol(x$Z)
  seed_str <- if (is.null(x$seed)) "NULL" else format(x$seed)
  cat(sprintf(
    "K = %d folds | M = %d respondents | p = %d attrs | p_Z = %d covariates | seed = %s\n",
    x$K, M, p, p_Z, seed_str
  ))
  ## Show the last few training losses of fold 1 as a pipeline health hint.
  if (length(x$loss_traces) >= 1L && length(x$loss_traces[[1L]]) > 0L) {
    tail_losses <- utils::tail(x$loss_traces[[1L]], n = 3L)
    cat("Fold 1 final losses: ",
        paste(sprintf("%.4f", tail_losses), collapse = " "), "\n", sep = "")
  }
  cat("Use coef() for point estimates, vcov() for clustered variance.\n")
  invisible(x)
}

#' Extract DML point estimates from `sc_fit`
#'
#' @param object An object of class `sc_fit`.
#' @param ... Unused.
#' @return A named numeric vector of DML point estimates `theta_hat`.
#' @export
coef.sc_fit <- function(object, ...) {
  object$theta
}

#' Extract the clustered variance-covariance matrix from `sc_fit`
#'
#' Returns the full `p x p` respondent-clustered variance-covariance
#' computed via the DML influence function.  Use `sqrt(diag(vcov(fit)))`
#' for clustered standard errors.
#'
#' @param object An object of class `sc_fit`.
#' @param ... Unused.
#' @return A numeric `p x p` matrix.
#' @export
vcov.sc_fit <- function(object, ...) {
  object$vcov
}
