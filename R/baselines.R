#' Baseline logit model for comparison with `sc_fit`
#'
#' Fits a standard logistic regression on the same `deltaX` and `y`
#' stored in an `sc_fit` object.  This produces homogeneous
#' coefficients (no heterogeneity in \eqn{\beta}) for comparison
#' against the DNN-estimated heterogeneous \eqn{\hat\beta(Z_i)}.
#' Standard errors are respondent-clustered.
#'
#' @param object An `sc_fit` (must have `deltaX` and `y` stored).
#' @return An object of class `"sc_baseline"` with components
#'   `coefficients`, `vcov`, `fitted.values`, `residuals`,
#'   `model` (the underlying `glm` or `lm` object), and metadata.
#' @export
sc_baseline_logit <- function(object) {
  stopifnot(inherits(object, "sc_fit"))
  if (is.null(object$deltaX) || is.null(object$y)) {
    stop("sc_baseline_logit(): object must have `deltaX` and `y` stored.")
  }
  dX <- object$deltaX
  y  <- object$y
  df <- as.data.frame(dX)
  ## Make column names syntactically valid for formula
  safe_names <- make.names(names(df))
  names(df) <- safe_names
  df$.y <- y
  fml <- stats::as.formula(paste(".y ~", paste(safe_names,
                                               collapse = " + "), "- 1"))
  fit <- stats::glm(fml, data = df, family = stats::binomial())
  ## Restore original names on coefficients
  names(fit$coefficients) <- colnames(dX)
  vcov_cl <- .sc_baseline_vcov(fit, object$respondent_id)
  rownames(vcov_cl) <- colnames(vcov_cl) <- colnames(dX)
  .sc_baseline(
    coefficients = stats::coef(fit),
    vcov         = vcov_cl,
    fitted.values = stats::fitted(fit),
    residuals    = stats::residuals(fit, type = "response"),
    model        = fit,
    type         = "logit",
    attr_names   = object$attr_names,
    n_obs        = length(y),
    n_resp       = length(unique(object$respondent_id))
  )
}

#' Baseline linear probability model for comparison with `sc_fit`
#'
#' Fits an OLS linear probability model on the same `deltaX` and `y`
#' stored in an `sc_fit` object.  Standard errors are
#' respondent-clustered.
#'
#' @param object An `sc_fit` (must have `deltaX` and `y` stored).
#' @return An object of class `"sc_baseline"`.
#' @export
sc_baseline_lpm <- function(object) {
  stopifnot(inherits(object, "sc_fit"))
  if (is.null(object$deltaX) || is.null(object$y)) {
    stop("sc_baseline_lpm(): object must have `deltaX` and `y` stored.")
  }
  dX <- object$deltaX
  y  <- object$y
  df <- as.data.frame(dX)
  safe_names <- make.names(names(df))
  names(df) <- safe_names
  df$.y <- y
  fml <- stats::as.formula(paste(".y ~", paste(safe_names,
                                               collapse = " + "), "- 1"))
  fit <- stats::lm(fml, data = df)
  names(fit$coefficients) <- colnames(dX)
  vcov_cl <- .sc_baseline_vcov(fit, object$respondent_id)
  .sc_baseline(
    coefficients = stats::coef(fit),
    vcov         = vcov_cl,
    fitted.values = stats::fitted(fit),
    residuals    = stats::residuals(fit),
    model        = fit,
    type         = "lpm",
    attr_names   = object$attr_names,
    n_obs        = length(y),
    n_resp       = length(unique(object$respondent_id))
  )
}

# ---- Internal constructor and methods ----

#' Construct an `sc_baseline` object
#' @keywords internal
#' @noRd
.sc_baseline <- function(coefficients, vcov, fitted.values, residuals,
                         model, type, attr_names, n_obs, n_resp) {
  structure(
    list(
      coefficients  = coefficients,
      vcov          = vcov,
      fitted.values = fitted.values,
      residuals     = residuals,
      model         = model,
      type          = type,
      attr_names    = attr_names,
      n_obs         = n_obs,
      n_resp        = n_resp
    ),
    class = c("sc_baseline", "list")
  )
}

#' @export
coef.sc_baseline <- function(object, ...) object$coefficients

#' @export
vcov.sc_baseline <- function(object, ...) object$vcov

#' @export
print.sc_baseline <- function(x, digits = 4L, ...) {
  cat(sprintf("Baseline %s model  (%d obs, %d respondents)\n\n",
              toupper(x$type), x$n_obs, x$n_resp))
  se <- sqrt(diag(x$vcov))
  df <- data.frame(
    Estimate = x$coefficients,
    `Std.Err` = se,
    `z value` = x$coefficients / se,
    check.names = FALSE,
    row.names = x$attr_names
  )
  print(round(df, digits))
  invisible(x)
}

#' @export
summary.sc_baseline <- function(object, ...) {
  se <- sqrt(diag(object$vcov))
  z  <- object$coefficients / se
  pval <- 2 * stats::pnorm(-abs(z))
  df <- data.frame(
    Estimate  = object$coefficients,
    Std.Error = se,
    z.value   = z,
    Pr.z      = pval,
    row.names = object$attr_names
  )
  cat(sprintf("Baseline %s model  (%d obs, %d respondents)\n",
              toupper(object$type), object$n_obs, object$n_resp))
  cat("Respondent-clustered standard errors\n\n")
  stats::printCoefmat(df, digits = 4L, P.values = TRUE, has.Pvalue = TRUE)
  invisible(df)
}

# ---- Clustered variance-covariance ----

#' Respondent-clustered variance-covariance for glm/lm
#'
#' Computes the HC1-style clustered sandwich estimator
#' \eqn{(X'X)^{-1} B (X'X)^{-1}} where
#' \eqn{B = \frac{M}{M-1}\sum_m u_m u_m'} and
#' \eqn{u_m = \sum_{i \in m} X_i e_i}.
#'
#' @param fit A `glm` or `lm` object.
#' @param cluster Integer/character vector of cluster IDs.
#' @return A p x p variance-covariance matrix.
#' @keywords internal
#' @noRd
.sc_baseline_vcov <- function(fit, cluster) {
  X <- stats::model.matrix(fit)
  e <- stats::residuals(fit, type = "response")
  n <- nrow(X)
  p <- ncol(X)
  cluster_f <- as.factor(cluster)
  M <- nlevels(cluster_f)
  ## Bread: (X'X)^{-1}
  bread <- solve(crossprod(X))
  ## Meat: sum of u_m u_m'
  meat <- matrix(0, p, p)
  for (m in levels(cluster_f)) {
    idx <- which(cluster_f == m)
    u_m <- colSums(X[idx, , drop = FALSE] * e[idx])
    meat <- meat + tcrossprod(u_m)
  }
  meat <- meat * (M / (M - 1))
  vcov_cl <- bread %*% meat %*% bread
  rownames(vcov_cl) <- colnames(vcov_cl) <- colnames(X)
  vcov_cl
}
