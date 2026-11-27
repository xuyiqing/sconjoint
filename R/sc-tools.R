# Helpers around estimate tables and fold states: comparison with known true values
# (sc_compare()) and the spread of tastes among respondents with the same characteristics
# (sc_spread()).

#' Compare estimates with known true values
#'
#' For checks on simulated data: lines up an estimate table from
#' [sc_estimate()] with the true value of each target and reports the error
#' and whether the 95% interval covers the truth. Printing gives the table and
#' a line on coverage. The result can be passed as `reference` to
#' [sc_plot_estimates()] and [sc_plot_shares()], which then draw the true
#' values.
#'
#' @param estimates An estimate table from [sc_estimate()].
#' @param truth The true values: a numeric vector named by target name, or a
#'   data frame with columns `name` and `value` (such as
#'   `simconjoint_truth$population`). Targets without a true value get `NA`.
#' @param format `"data.frame"` (the default) or `"markdown"`, a pipe table
#'   that prints as a table in a knitr or Quarto chunk with `results: asis`.
#' @param digits Decimal places (printing and `"markdown"`).
#' @param labels Optional row labels for `"markdown"`: named by target name,
#'   or one per row in order.
#' @return For `"data.frame"`, a data frame of class `sc_compare` with one row
#'   per target: `name`, `truth`, `estimate`, `lower`, `upper`, `error`
#'   (estimate minus truth) and `covered` (`NA` when the target has no
#'   interval or no true value). For `"markdown"`, a character string of class
#'   `sc_table_text`.
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   est <- sc_estimate(fit, list(sc_mean("top", "tax6_linear"), sc_mean("bottom", "tax1_linear")))
#'   cmp <- sc_compare(est, c(top = 5, bottom = -10))
#'   cmp
#'   sc_plot_estimates(est, reference = cmp, reference_label = "Truth")
#' }
#' @export
sc_compare <- function(estimates, truth, format = c("data.frame", "markdown"), digits = 3, labels = NULL) {
  fun <- "sc_compare"
  format <- match.arg(format)
  est <- .scp_tables(list(estimates), fun)[[1L]]
  if (is.data.frame(truth)) {
    if (!all(c("name", "value") %in% names(truth)))
      stop("sc_compare(): a truth data frame needs columns name and value", call. = FALSE)
    truth <- stats::setNames(as.numeric(truth$value), as.character(truth$name))
  }
  if (!is.numeric(truth) || is.null(names(truth)) || any(!nzchar(names(truth))))
    stop("sc_compare(): truth must be a numeric vector named by target name, or a data frame with name and value",
         call. = FALSE)
  if (anyDuplicated(names(truth))) stop("sc_compare(): truth names a target twice", call. = FALSE)
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0 || digits != round(digits))
    stop("sc_compare(): digits must be one whole number >= 0", call. = FALSE)
  tv <- unname(truth[match(est$name, names(truth))])
  if (all(is.na(tv))) stop("sc_compare(): no name in truth matches a target of the estimates", call. = FALSE)
  has_ci <- is.finite(est$lower) & is.finite(est$upper)
  out <- data.frame(name = est$name, truth = tv, estimate = est$estimate, lower = est$lower, upper = est$upper,
                    error = est$estimate - tv,
                    covered = ifelse(has_ci & !is.na(tv), tv >= est$lower & tv <= est$upper, NA),
                    stringsAsFactors = FALSE)
  attr(out, "digits") <- digits
  class(out) <- c("sc_compare", "data.frame")
  if (format == "data.frame") return(out)
  labs <- as.character(.scp_labels(labels, out$name, fun))
  num <- function(v) ifelse(is.na(v), "", formatC(round(v, digits), format = "f", digits = digits))
  cov <- ifelse(is.na(out$covered), "", ifelse(out$covered, "yes", "no"))
  esc <- function(s) gsub("|", "\\|", s, fixed = TRUE)
  line <- function(v) paste0("| ", paste(esc(v), collapse = " | "), " |")
  body <- vapply(seq_len(nrow(out)), function(i)
    line(c(labs[i], num(out$truth[i]), num(out$estimate[i]), num(out$lower[i]), num(out$upper[i]), cov[i])), "")
  txt <- c(line(c("Target", "Truth", "Estimate", "95% lower", "95% upper", "Covered")),
           "|:---|---:|---:|---:|---:|:---:|", body)
  structure(paste(txt, collapse = "\n"), class = "sc_table_text")
}

#' @export
print.sc_compare <- function(x, digits = attr(x, "digits"), ...) {
  if (is.null(digits)) digits <- 3L
  y <- as.data.frame(unclass(x), stringsAsFactors = FALSE); attr(y, "digits") <- NULL
  num <- c("truth", "estimate", "lower", "upper", "error")
  y[num] <- lapply(y[num], round, digits)
  print(y, row.names = FALSE)
  k <- sum(!is.na(x$covered))
  if (k) {
    miss <- x$name[!is.na(x$covered) & !x$covered]
    cat(sprintf("\n%d of %d intervals cover the truth%s\n", sum(x$covered, na.rm = TRUE), k,
                if (length(miss)) paste0("; missed: ", paste(miss, collapse = ", ")) else ""))
  }
  invisible(x)
}

#' Spread of tastes among respondents with the same characteristics
#'
#' The model's tastes are `beta_i = mu(Z_i) + Lambda u_i`; respondents with
#' the same characteristics differ by `Lambda u_i`. The loading `Lambda` is
#' identified only up to a rotation of its columns, but the conditional
#' covariance `Lambda Lambda'` is not, and the square roots of its diagonal
#' are each feature's conditional standard deviation. This function reports
#' them for each fold and averaged over the folds.
#'
#' @param object Fold states from [sc_states()] or [sc_load()].
#' @param features Optional feature names; default all coded features.
#' @return A data frame with one row per feature: `feature`, `sd` (the
#'   average over folds) and one column per fold (`fold_1`, ...).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) sc_spread(fit, c("tax1_linear", "tax6_linear"))
#' @export
sc_spread <- function(object, features = NULL) {
  object <- .sc_states_check(object)
  fn <- object$data$data$feature_names
  if (is.null(features)) features <- fn
  bad <- setdiff(features, fn)
  if (length(bad)) stop("sc_spread(): not coded features: ", paste(bad, collapse = ", "), call. = FALSE)
  per_fold <- vapply(object$folds, function(f) {
    L <- as.matrix(f$th$loading)
    sqrt(rowSums(L^2))
  }, numeric(length(fn)))
  per_fold <- matrix(per_fold, nrow = length(fn))
  i <- match(features, fn)
  out <- data.frame(feature = features, sd = rowMeans(per_fold[i, , drop = FALSE]), stringsAsFactors = FALSE)
  folds <- per_fold[i, , drop = FALSE]; colnames(folds) <- paste0("fold_", seq_len(ncol(folds)))
  out <- cbind(out, as.data.frame(folds))
  rownames(out) <- NULL
  out
}
