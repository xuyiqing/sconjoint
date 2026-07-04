## sc_validate_amce(): pooled (and optionally subgroup) homogeneous-logit
## comparison against the DML theta_hat.  Mirrors paper sectionAppendix D /
## prototype code/70_validation_amce.R.

#' Validate the structural model against reduced-form AMCE benchmarks
#'
#' Compares the structural DML point estimates `coef(object)` against
#' the coefficients of a *pooled homogeneous logit* fit on the same
#' (deltaX, y) -- i.e. an ordinary `glm(family = binomial)` that
#' assumes no heterogeneity in \eqn{\beta}.  Under correct logit
#' specification, \eqn{\theta_k = \mathbb{E}[\beta_k(Z)]} equals the
#' pooled coefficient on attribute \eqn{k}.  In practice the paper
#' shows \eqn{r \approx 0.998} across 28 attribute levels on its main
#' application (Bansak, Hainmueller & Hangartner 2023).
#'
#' Optionally, supply `subgroup` (a column name in the original data,
#' or a vector of values) to repeat the comparison within each level
#' of `subgroup`: fit a pooled logit on the subgroup's rows, take the
#' DML's `sc_subgroup()` estimate on the same rows, and report
#' per-subgroup correlation.
#'
#' @param object An `sc_fit`.
#' @param subgroup Optional: a column name (character) for a moderator
#'   stored in `object$Z`, or a vector of length `nrow(object$Z)` with
#'   subgroup labels.  When `NULL` (default), only the pooled
#'   comparison is reported.
#' @return An `sc_quantity_validate_amce` whose `estimate` is a list
#'   with components:
#'   * `pooled`: data.frame with one row per attribute level and
#'     columns `attribute`, `dml_theta`, `dml_se`, `homog_logit_coef`,
#'     `homog_logit_se`, `diff`, `abs_diff`.
#'   * `pooled_correlation`: scalar Pearson r between `dml_theta` and
#'     `homog_logit_coef`.
#'   * `subgroup` (if `subgroup` was supplied): data.frame with columns
#'     `subgroup`, `n_rows`, `correlation`, `mean_abs_diff`.
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
#'   v <- sc_validate_amce(fit)
#'   print(v)
#' }
#' }
#' @export
sc_validate_amce <- function(object, subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  if (is.null(object$deltaX) || is.null(object$y)) {
    stop("sc_validate_amce(): object must have `deltaX` and `y` stored.")
  }

  ## --- Pooled comparison ---
  baseline <- sc_baseline_logit(object)
  homog_coef <- stats::coef(baseline)
  homog_se   <- sqrt(diag(stats::vcov(baseline)))

  dml_theta <- as.numeric(object$theta)
  dml_se    <- sqrt(diag(object$vcov))
  attr_names <- object$attr_names

  pooled <- data.frame(
    attribute        = attr_names,
    dml_theta        = dml_theta,
    dml_se           = dml_se,
    homog_logit_coef = unname(homog_coef),
    homog_logit_se   = unname(homog_se),
    diff             = dml_theta - unname(homog_coef),
    abs_diff         = abs(dml_theta - unname(homog_coef)),
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
  pooled_cor <- stats::cor(pooled$dml_theta, pooled$homog_logit_coef)

  ## --- Subgroup comparison ---
  subgroup_df <- NULL
  if (!is.null(subgroup)) {
    subgroup_df <- .sc_validate_amce_subgroup(object, subgroup)
  }

  estimate <- list(
    pooled             = pooled,
    pooled_correlation = pooled_cor,
    subgroup           = subgroup_df
  )

  out <- .sc_quantity(
    name     = "validate_amce",
    estimate = estimate,
    se       = NA_real_,
    details  = list(
      n_obs              = length(object$y),
      n_respondents      = length(unique(object$respondent_id)),
      n_attributes       = length(attr_names),
      pooled_correlation = pooled_cor,
      stage2_method      = object$stage2_method
    ),
    call = match.call()
  )
  class(out) <- c("sc_quantity_validate_amce", class(out))
  out
}

#' Per-subgroup validation: pooled logit within each subgroup level
#' @keywords internal
#' @noRd
.sc_validate_amce_subgroup <- function(object, subgroup) {
  Z <- object$Z
  if (is.character(subgroup) && length(subgroup) == 1L) {
    if (!subgroup %in% colnames(Z)) {
      stop(sprintf("sc_validate_amce(): subgroup column '%s' not found in object$Z.",
                   subgroup))
    }
    sub_vals <- Z[, subgroup]
    sub_label <- subgroup
  } else {
    if (length(subgroup) != nrow(Z)) {
      stop("sc_validate_amce(): `subgroup` vector must have length nrow(object$Z).")
    }
    sub_vals <- subgroup
    sub_label <- "subgroup"
  }

  unique_levels <- sort(unique(sub_vals))
  dX <- object$deltaX
  y  <- object$y
  attr_names <- object$attr_names

  rows <- list()
  for (lev in unique_levels) {
    idx <- which(sub_vals == lev)
    if (length(idx) < length(attr_names) + 5L) {
      ## Too few observations -- skip
      next
    }
    dX_s <- dX[idx, , drop = FALSE]
    y_s  <- y[idx]
    df_s <- as.data.frame(dX_s)
    safe_names <- make.names(names(df_s))
    names(df_s) <- safe_names
    df_s$.y <- y_s
    fml <- stats::as.formula(
      paste(".y ~", paste(safe_names, collapse = " + "), "- 1")
    )
    fit_s <- tryCatch(
      suppressWarnings(stats::glm(fml, data = df_s, family = stats::binomial())),
      error = function(e) NULL
    )
    if (is.null(fit_s)) next
    homog_s <- unname(stats::coef(fit_s))

    ## DML within subgroup: use sc_subgroup() on object
    ## For continuity with how Z is stored at the task level, we pass a
    ## logical vector selecting the subgroup rows.
    dml_sub <- tryCatch(
      sc_subgroup(object, idx, which_beta = "dnn"),
      error = function(e) NULL
    )
    if (is.null(dml_sub)) next
    dml_theta_s <- dml_sub$estimate$theta
    if (length(dml_theta_s) != length(homog_s)) next
    r_s <- stats::cor(dml_theta_s, homog_s)
    rows[[length(rows) + 1]] <- data.frame(
      subgroup       = as.character(lev),
      n_rows         = length(idx),
      correlation    = r_s,
      mean_abs_diff  = mean(abs(dml_theta_s - homog_s)),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  do.call(rbind, rows)
}

#' Print method for `sc_quantity_validate_amce`
#' @param x An `sc_quantity_validate_amce`.
#' @param digits Significant digits.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.sc_quantity_validate_amce <- function(x, digits = 3L, ...) {
  cat("sc_validate_amce -- pooled and (optionally) subgroup comparison\n")
  cat(sprintf("Stage 2: %s\n",
              if (is.null(x$details$stage2_method)) "(unknown)"
              else x$details$stage2_method))
  cat(sprintf("N obs: %d, N respondents: %d, P attributes: %d\n",
              x$details$n_obs,
              x$details$n_respondents,
              x$details$n_attributes))
  cat(sprintf("Pooled correlation (DML theta vs homogeneous logit coef): %s\n",
              format(x$estimate$pooled_correlation, digits = digits)))
  cat("\nPooled comparison (first 10 rows):\n")
  pooled <- x$estimate$pooled
  show <- utils::head(pooled, 10L)
  print(show, digits = digits, row.names = FALSE)
  if (!is.null(x$estimate$subgroup)) {
    cat("\nSubgroup correlations:\n")
    print(x$estimate$subgroup, digits = digits, row.names = FALSE)
  }
  invisible(x)
}
