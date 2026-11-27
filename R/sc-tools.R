# Helpers around estimate tables and fold states: comparison with true values or another model
# (sc_compare()) and the spread of tastes among respondents with the same characteristics
# (sc_spread()).

#' Compare estimates with true values or with another model
#'
#' Lines an estimate table from [sc_estimate()] up with a reference, target
#' by target. The reference is either
#'
#' * **another estimate table**, for example the same targets from a model
#'   with a different number of taste factors, different covariates or a
#'   different network. The result gives each target's difference between
#'   the two models with its standard error and 95% interval. When both
#'   tables come from fits to the same respondents, the standard error is
#'   paired: it is the standard deviation, over respondents, of the
#'   difference between the two estimates' influence values, divided by the
#'   square root of n. The pairing removes the variation the two estimates
#'   share, so it detects much smaller differences than the two separate
#'   standard errors would. When the respondents differ, or a table has no
#'   influence values, the two estimates are treated as independent. Targets without intervals (importance shares)
#'   get a difference but no standard error. A difference whose interval
#'   excludes zero says the two models disagree about that target; it does
#'   not say which model is right.
#' * **true values**, for checks on simulated data: a numeric vector named by
#'   target, or a data frame with columns `name` and `value` (such as
#'   `simconjoint_truth$population`). The result gives each target's error
#'   and whether its 95% interval covers the truth.
#'
#' Printing gives the table and a summary line. The result can be passed as
#' `reference` to [sc_plot_estimates()] and [sc_plot_shares()], which then
#' draw the reference values.
#'
#' @param estimates An estimate table from [sc_estimate()].
#' @param reference Another estimate table, or the true values (see
#'   Details). Targets the reference lacks get `NA`.
#' @param format `"data.frame"` (the default) or `"markdown"`, a pipe table
#'   that prints as a table in a knitr or Quarto chunk with `results: asis`.
#' @param digits Decimal places (printing and `"markdown"`).
#' @param labels Optional row labels for `"markdown"`: named by target name,
#'   or one per row in order.
#' @param z Critical value of the difference intervals (default 1.96).
#' @return For `"data.frame"`, a data frame of class `sc_compare` with one row
#'   per target. Against another model: `name`, `estimate`, `reference`,
#'   `difference` (estimate minus reference), `se`, `lower`, `upper` (the
#'   difference's interval) and `paired` (whether the standard error is
#'   paired). Against true values: `name`, `truth`, `estimate`, `lower`,
#'   `upper` (the estimate's interval), `error` and `covered`. For
#'   `"markdown"`, a character string of class `sc_table_text`.
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   tg <- list(sc_mean("top", "tax6_linear"), sc_mean("bottom", "tax1_linear"))
#'   est <- sc_estimate(fit, tg)
#'   # against true values (a simulation)
#'   sc_compare(est, c(top = 5, bottom = -10))
#'   # against another estimate of the same targets (here a copy shifted by 1)
#'   other <- est; other$estimate <- other$estimate + 1
#'   sc_compare(est, other)
#' }
#' @export
sc_compare <- function(estimates, reference, format = c("data.frame", "markdown"), digits = 3, labels = NULL,
                       z = 1.96) {
  fun <- "sc_compare"
  format <- match.arg(format)
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0 || digits != round(digits))
    stop("sc_compare(): digits must be one whole number >= 0", call. = FALSE)
  infl1 <- attr(estimates, "influence")
  est <- .scp_tables(list(estimates), fun)[[1L]]
  if (inherits(reference, "sc_estimates") || (is.data.frame(reference) && all(c("name", "estimate") %in% names(reference)))) {
    out <- .sc_compare_models(est, infl1, reference, z)
  } else {
    out <- .sc_compare_truth(est, reference)
  }
  attr(out, "digits") <- digits
  class(out) <- c("sc_compare", "data.frame")
  if (format == "data.frame") return(out)
  labs <- as.character(.scp_labels(labels, out$name, fun))
  num <- function(v) ifelse(is.na(v), "", formatC(round(v, digits), format = "f", digits = digits))
  esc <- function(s) gsub("|", "\\|", s, fixed = TRUE)
  line <- function(v) paste0("| ", paste(esc(v), collapse = " | "), " |")
  if (identical(attr(out, "type"), "truth")) {
    cov <- ifelse(is.na(out$covered), "", ifelse(out$covered, "yes", "no"))
    body <- vapply(seq_len(nrow(out)), function(i)
      line(c(labs[i], num(out$truth[i]), num(out$estimate[i]), num(out$lower[i]), num(out$upper[i]), cov[i])), "")
    txt <- c(line(c("Target", "Truth", "Estimate", "95% lower", "95% upper", "Covered")),
             "|:---|---:|---:|---:|---:|:---:|", body)
  } else {
    ci <- ifelse(is.finite(out$lower) & is.finite(out$upper), paste0("[", num(out$lower), ", ", num(out$upper), "]"), "")
    body <- vapply(seq_len(nrow(out)), function(i)
      line(c(labs[i], num(out$estimate[i]), num(out$reference[i]), num(out$difference[i]), ci[i])), "")
    txt <- c(line(c("Target", "Estimate", "Reference", "Difference", "95% interval")),
             "|:---|---:|---:|---:|---:|", body)
  }
  structure(paste(txt, collapse = "\n"), class = "sc_table_text")
}

.sc_compare_truth <- function(est, truth) {
  if (is.data.frame(truth)) {
    if (!all(c("name", "value") %in% names(truth)))
      stop("sc_compare(): a truth data frame needs columns name and value", call. = FALSE)
    truth <- stats::setNames(as.numeric(truth$value), as.character(truth$name))
  }
  if (!is.numeric(truth) || is.null(names(truth)) || any(!nzchar(names(truth))))
    stop("sc_compare(): reference must be another estimate table, a numeric vector named by target name, ",
         "or a data frame with name and value", call. = FALSE)
  if (anyDuplicated(names(truth))) stop("sc_compare(): the reference names a target twice", call. = FALSE)
  tv <- unname(truth[match(est$name, names(truth))])
  if (all(is.na(tv))) stop("sc_compare(): no name in the reference matches a target of the estimates", call. = FALSE)
  has_ci <- is.finite(est$lower) & is.finite(est$upper)
  out <- data.frame(name = est$name, truth = tv, estimate = est$estimate, lower = est$lower, upper = est$upper,
                    error = est$estimate - tv,
                    covered = ifelse(has_ci & !is.na(tv), tv >= est$lower & tv <= est$upper, NA),
                    stringsAsFactors = FALSE)
  attr(out, "type") <- "truth"
  out
}

.sc_compare_models <- function(est, infl1, reference, z) {
  infl2 <- attr(reference, "influence")
  ref <- .scp_tables(list(reference), "sc_compare")[[1L]]
  j <- match(est$name, ref$name)
  if (all(is.na(j))) stop("sc_compare(): no target of the reference matches a target of the estimates", call. = FALSE)
  r <- ref[j, , drop = FALSE]
  d <- est$estimate - r$estimate
  no_ci <- function(x) {
    iv <- if (is.null(x$interval)) rep(NA_character_, nrow(x)) else x$interval
    (!is.na(iv) & iv == "none") | !is.finite(x$se)
  }
  skip <- no_ci(est) | no_ci(r) | is.na(j)
  # paired when both tables carry influence values for the same respondents
  ids1 <- rownames(infl1); ids2 <- rownames(infl2)
  paired <- !is.null(infl1) && !is.null(infl2) && !is.null(ids1) && !is.null(ids2) &&
    length(ids1) == length(ids2) && setequal(ids1, ids2)
  se <- rep(NA_real_, nrow(est)); pr <- rep(FALSE, nrow(est))
  for (i in which(!skip)) {
    nm <- est$name[i]
    if (paired && nm %in% colnames(infl1) && nm %in% colnames(infl2)) {
      dv <- infl1[, nm] - infl2[match(ids1, ids2), nm]
      se[i] <- stats::sd(dv) / sqrt(length(dv)); pr[i] <- TRUE
    } else se[i] <- sqrt(est$se[i]^2 + r$se[i]^2)
  }
  out <- data.frame(name = est$name, estimate = est$estimate, reference = r$estimate, difference = d, se = se,
                    lower = d - z * se, upper = d + z * se, paired = pr, stringsAsFactors = FALSE)
  attr(out, "type") <- "model"
  out
}

#' @export
print.sc_compare <- function(x, digits = attr(x, "digits"), ...) {
  if (is.null(digits)) digits <- 3L
  y <- as.data.frame(unclass(x), stringsAsFactors = FALSE); attr(y, "digits") <- NULL; attr(y, "type") <- NULL
  num <- intersect(c("truth", "estimate", "reference", "difference", "se", "lower", "upper", "error"), names(y))
  y[num] <- lapply(y[num], round, digits)
  print(y, row.names = FALSE)
  if (identical(attr(x, "type"), "truth")) {
    k <- sum(!is.na(x$covered))
    if (k) {
      miss <- x$name[!is.na(x$covered) & !x$covered]
      cat(sprintf("\n%d of %d intervals cover the truth%s\n", sum(x$covered, na.rm = TRUE), k,
                  if (length(miss)) paste0("; missed: ", paste(miss, collapse = ", ")) else ""))
    }
  } else {
    ok <- is.finite(x$lower) & is.finite(x$upper)
    k <- sum(ok)
    if (k) {
      apart <- x$name[ok & (x$lower > 0 | x$upper < 0)]
      cat(sprintf("\n%d of %d differences have a 95%% interval that excludes zero%s\n", length(apart), k,
                  if (length(apart)) paste0(": ", paste(apart, collapse = ", ")) else ""))
      if (any(ok & !x$paired))
        cat("Standard errors treat the two estimates as independent for:",
            paste(x$name[ok & !x$paired], collapse = ", "), "\n")
    }
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
