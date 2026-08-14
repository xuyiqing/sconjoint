## Respondent-level posterior summaries from an scmix fit (P10).
##
## The fitted model implies a posterior for each respondent's beta_i
## given that respondent's own choices, at the out-of-fold nuisances.
## Its mean is an empirical-Bayes shrinkage summary; under the
## one-estimator revision these summaries are DESCRIPTIVE objects --
## for ranking respondents, drawing fitted schedules, and external
## validation through scale-free correlations -- never aggregated into
## population claims (population quantities come from (mu, A) through
## the orthogonal estimands).

#' Respondent-level posterior means and SDs from an scmix fit
#'
#' Extracts `E[beta_i | data]` (and, on request, the posterior SD of
#' each coordinate) for every respondent, evaluated at the out-of-fold
#' `mu_hat(Z_i)` and the fold loading matrix on the quadrature grid.
#'
#' The posterior mean combines `mu_hat(Z_i)` with the respondent's own
#' choices and is a shrinkage estimate by construction; its dispersion
#' understates the population's.  Use these summaries for descriptive
#' displays (labeled as such) and for external-validation correlations,
#' which are scale- and shrinkage-tolerant.  Population distributional
#' claims go through [scmix_polarization()] / [scmix_signshare()]; the
#' posterior SDs condition on the estimated `(mu, A)` and do not
#' propagate nuisance uncertainty.
#'
#' @param fit An `scmix` object.
#' @param what Which summaries to compute: `"mean"`, `"sd"`, or both.
#'   The SD pass costs one extra sweep over the quadrature weights and
#'   is skipped unless requested.
#' @return An object of class `scmix_posterior`: a list with
#'   `respondent` (ids in first-appearance order), `mean` (N x p, one
#'   row per respondent, columns named by `fit$attr_names`), `sd` (same
#'   shape, or `NULL` when not requested), `T_i`, `fold`, `attr_names`,
#'   and `n_respondents`.  `as.data.frame()` returns the wide form
#'   (`respondent`, `T_i`, `fold`, `mean_*`, `sd_*`) for plotting and
#'   validation joins.
#' @export
scmix_posterior <- function(fit, what = c("mean", "sd")) {
  stopifnot(inherits(fit, "scmix"))
  what <- match.arg(what, several.ok = TRUE)
  sc <- .scmix_scores(fit, post_sd = "sd" %in% what)
  mean_m <- sc$post_mean
  colnames(mean_m) <- fit$attr_names
  sd_m <- sc$post_sd
  if (!is.null(sd_m)) colnames(sd_m) <- fit$attr_names
  out <- list(respondent = sc$resp,
              mean = mean_m,
              sd = sd_m,
              T_i = sc$T_i,
              fold = sc$fold_resp,
              attr_names = fit$attr_names,
              n_respondents = length(sc$resp))
  class(out) <- c("scmix_posterior", "list")
  out
}

#' @export
as.data.frame.scmix_posterior <- function(x, ...) {
  df <- data.frame(respondent = x$respondent, T_i = x$T_i, fold = x$fold,
                   stringsAsFactors = FALSE)
  mean_df <- as.data.frame(x$mean)
  names(mean_df) <- paste0("mean_", x$attr_names)
  df <- cbind(df, mean_df)
  if (!is.null(x$sd)) {
    sd_df <- as.data.frame(x$sd)
    names(sd_df) <- paste0("sd_", x$attr_names)
    df <- cbind(df, sd_df)
  }
  rownames(df) <- NULL
  df
}

#' @export
print.scmix_posterior <- function(x, ...) {
  cat(sprintf(
    "scmix posterior summaries: %d respondents x %d coordinates (%s)\n",
    x$n_respondents, length(x$attr_names),
    if (is.null(x$sd)) "means" else "means + SDs"))
  cat("descriptive shrinkage summaries -- not for population claims\n")
  cat("posterior-mean ranges:\n")
  rng <- t(apply(x$mean, 2L, range))
  colnames(rng) <- c("min", "max")
  print(round(rng, 3))
  invisible(x)
}
