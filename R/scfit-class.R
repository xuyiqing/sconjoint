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

#' Summary method for `sc_fit`
#'
#' Returns a coefficient table with clustered SE, z-statistic,
#' two-sided p-value, and normal-approx 95% CI, plus pipeline
#' metadata and the DML/iid SE ratio diagnostic.
#'
#' @param object An `sc_fit`.
#' @param ... Unused.
#' @return An object of class `sc_fit_summary`.
#' @export
summary.sc_fit <- function(object, ...) {
  theta <- object$theta
  V <- object$vcov
  se <- sqrt(diag(V))
  z  <- theta / se
  p_val <- 2 * stats::pnorm(-abs(z))
  q <- stats::qnorm(0.975)
  coef_tbl <- data.frame(
    estimate  = as.numeric(theta),
    std_error = se,
    z_value   = z,
    p_value   = p_val,
    ci_lo     = theta - q * se,
    ci_hi     = theta + q * se,
    row.names = names(theta),
    stringsAsFactors = FALSE
  )
  out <- list(
    call             = object$call,
    coefficients     = coef_tbl,
    n_resp           = length(unique(object$respondent_id)),
    n_obs            = nrow(object$beta_hat),
    K                = object$K,
    hidden           = object$hidden,
    n_epochs         = object$n_epochs,
    seed             = object$seed,
    device           = object$device,
    parallel         = object$parallel,
    n_cores          = object$n_cores,
    se_ratio_dml_iid = object$se_ratio_dml_iid
  )
  class(out) <- c("sc_fit_summary", "list")
  out
}

#' Print method for `sc_fit_summary`
#' @param x An `sc_fit_summary`.
#' @param digits Significant digits.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.sc_fit_summary <- function(x, digits = 4L, ...) {
  cat("sc_fit summary\n")
  cat("Call: "); print(x$call); cat("\n")
  cat(sprintf("%d respondents | %d observations | K = %d folds\n",
              x$n_resp, x$n_obs, x$K))
  cat(sprintf("hidden = %s | epochs = %d | seed = %s | device = %s",
              paste(x$hidden, collapse = "-"),
              x$n_epochs,
              if (is.null(x$seed)) "NULL" else format(x$seed),
              x$device))
  if (isTRUE(x$parallel)) {
    cat(sprintf(" | parallel (%s cores)",
                if (is.null(x$n_cores)) "?" else format(x$n_cores)))
  }
  cat("\n\nCoefficients (DML, respondent-clustered SE):\n")
  print(x$coefficients, digits = digits)
  if (!is.null(x$se_ratio_dml_iid)) {
    r <- x$se_ratio_dml_iid
    r_val <- if (is.list(r) && !is.null(r$mean)) r$mean
             else if (is.numeric(r)) mean(r) else NA_real_
    if (!is.na(r_val)) {
      cat(sprintf("\nDML/iid SE ratio (mean): %s\n",
                  format(r_val, digits = digits)))
    }
  }
  invisible(x)
}

#' Predict method for \code{sc_fit}
#'
#' When \code{newdata = NULL}, returns stored held-out predictions from
#' the cross-fit.  When \code{newdata} is supplied (a data frame or
#' numeric matrix of respondent moderators \eqn{Z}), performs a
#' forward-pass through the stored per-fold neural networks and averages
#' across folds.
#'
#' @param object An \code{sc_fit} object.
#' @param newdata Either \code{NULL} (default — use stored predictions)
#'   or a data frame / numeric matrix of new respondent moderators.
#'   For a data frame, columns matching \code{object$z_names} are
#'   extracted and converted to a numeric matrix.  For a matrix,
#'   \code{ncol} must equal \code{length(object$z_names)}.
#' @param type Character; one of \code{"beta"} (default), \code{"logit"},
#'   or \code{"prob"}.  When \code{newdata} is non-NULL, only
#'   \code{"beta"} is supported (logit and prob require task-level
#'   deltaX which is unavailable for new respondents).
#' @param ... Unused.
#' @return When \code{type = "beta"}: an \eqn{N \times p} numeric matrix
#'   of preference parameters.
#'   When \code{type = "logit"} (only \code{newdata = NULL}): a numeric
#'   vector of length \eqn{N}.
#'   When \code{type = "prob"} (only \code{newdata = NULL}): a numeric
#'   vector of probabilities of length \eqn{N}.
#' @export
predict.sc_fit <- function(object, newdata = NULL, type = c("beta", "logit", "prob"), ...) {
  type <- match.arg(type)

  if (is.null(newdata)) {
    ## ---- Stored predictions (backward-compatible) ----
    switch(type,
      beta  = object$beta_hat,
      logit = as.numeric(rowSums(object$deltaX * object$beta_hat)),
      prob  = as.numeric(stats::plogis(rowSums(object$deltaX * object$beta_hat)))
    )
  } else {
    ## ---- Forward-pass on new Z ----
    if (type != "beta") {
      stop("predict.sc_fit: type = \"", type, "\" requires task-level deltaX, ",
           "which is not available for new respondents. ",
           "Use type = \"beta\" with newdata.", call. = FALSE)
    }
    if (is.null(object$nets)) {
      stop("predict.sc_fit: per-fold modules were not stored. ",
           "Refit with scfit(..., keep_modules = TRUE).", call. = FALSE)
    }

    ## Coerce newdata to numeric matrix
    if (is.data.frame(newdata)) {
      missing_z <- setdiff(object$z_names, names(newdata))
      if (length(missing_z) > 0L) {
        stop("predict.sc_fit: newdata is missing Z columns: ",
             paste(missing_z, collapse = ", "), call. = FALSE)
      }
      Z_new <- as.matrix(newdata[, object$z_names, drop = FALSE])
      storage.mode(Z_new) <- "double"
    } else if (is.matrix(newdata)) {
      if (ncol(newdata) != length(object$z_names)) {
        stop("predict.sc_fit: newdata matrix has ", ncol(newdata),
             " columns but expected ", length(object$z_names),
             " (length of z_names).", call. = FALSE)
      }
      Z_new <- newdata
      if (!is.numeric(Z_new)) {
        stop("predict.sc_fit: newdata matrix must be numeric.", call. = FALSE)
      }
    } else {
      stop("predict.sc_fit: newdata must be a data.frame or numeric matrix.",
           call. = FALSE)
    }

    ## Forward-pass: average across K folds
    K <- length(object$nets)
    beta_sum <- .sc_predict_beta(object$nets[[1L]], Z_new)
    if (K > 1L) {
      for (k in 2L:K) {
        beta_sum <- beta_sum + .sc_predict_beta(object$nets[[k]], Z_new)
      }
    }
    beta_avg <- beta_sum / K
    colnames(beta_avg) <- object$attr_names
    beta_avg
  }
}

#' Plot method for `sc_fit`
#'
#' Two variants: `"beta_ridgelines"` shows the per-respondent
#' distributions of \eqn{\hat\beta_j(Z_i)} (one ridgeline per
#' attribute dummy); `"loss_trace"` shows the per-fold training loss
#' curves.
#'
#' @param x An `sc_fit`.
#' @param which Either `"beta_ridgelines"` (default) or `"loss_trace"`.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.sc_fit <- function(x, which = c("beta_ridgelines", "loss_trace"), ...) {
  which <- match.arg(which)
  switch(
    which,
    beta_ridgelines = .sc_plot_ridgelines(x$beta_hat, x$attr_names),
    loss_trace      = .sc_plot_loss_traces(x$loss_traces)
  )
}

#' Autoplot method for `sc_fit`
#'
#' Default ggplot2 entry point; returns the ridgeline plot of
#' `beta(Z)`.
#'
#' @param object An `sc_fit`.
#' @param ... Passed to `plot.sc_fit()`.
#' @return A `ggplot` object.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.sc_fit <- function(object, ...) {
  plot.sc_fit(object, which = "beta_ridgelines", ...)
}

