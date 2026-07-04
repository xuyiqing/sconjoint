## Respondent-cluster (wild) bootstrap for distributional / threshold
## quantities.
##
## `sc_polarization()` and `sc_fraction_preferring()` summarize the
## *distribution* of the recovered per-respondent slopes beta_i(Z_i):
## the fraction of respondents who prefer a level (Pr{beta_ij > tau}),
## and the direction-split polarization index. These are plug-in
## functionals of the empirical distribution of beta_i over respondents.
## The sampling unit is the respondent, so their sampling uncertainty is
## a respondent-level cluster phenomenon: with M respondents drawn i.i.d.
## from the population, a fraction-of-respondents estimate has the
## standard error of a clustered proportion.
##
## Two facts shape the implementation:
##
##   1. The recovered beta_i is CONSTANT within a respondent (Z is a
##      respondent-level covariate, and every quantity reads the same
##      beta(Z_i) on each of respondent i's task rows). So the
##      task-level p x p beta matrix collapses, with no loss, to one
##      row per respondent. We bootstrap that respondent-level object.
##
##   2. The expensive part of `scfit()` -- the cross-fitted DNN that
##      recovers beta_i -- is NOT re-run inside the bootstrap. We
##      bootstrap the respondent-level AGGREGATION step (which fraction
##      of the recovered betas clears the threshold), because that is
##      what carries the sampling uncertainty of these fractions.
##      Re-fitting the network per resample would also fold in
##      first-stage estimation noise, which these descriptive summaries
##      do not claim to debias.
##
## IMPORTANT (documented to the user): fixed-T empirical-Bayes shrinkage
## pulls each beta_i toward the population consensus, which biases the
## plug-in fraction toward agreement. The bootstrap interval below
## reflects SAMPLING variability of the fraction; it does not correct
## that shrinkage bias.

#' Rademacher draws (+1 / -1 with equal probability)
#' @param n Integer length.
#' @return Numeric vector of +1 / -1.
#' @keywords internal
#' @noRd
.sc_rademacher <- function(n) {
  2 * stats::rbinom(n, size = 1L, prob = 0.5) - 1
}

#' Respondent-cluster bootstrap of a distributional functional
#'
#' Generic engine shared by `sc_polarization()` and
#' `sc_fraction_preferring()`. Given a respondent-level numeric matrix
#' `G` (M respondents x q "contribution" columns, each column already a
#' per-respondent quantity whose column mean is the point estimate of
#' interest) and a function `fun()` mapping a set of column means to the
#' reported quantities, it returns bootstrap standard errors and
#' percentile confidence intervals.
#'
#' Two resampling schemes are supported:
#'
#' * `"cluster"` -- nonparametric respondent (cluster) bootstrap: draw
#'   `M` respondents with replacement and recompute the column means on
#'   the drawn set. Exact and robust for bounded fractions.
#' * `"wild"` -- Rademacher wild bootstrap on the centered respondent
#'   contributions: the b-th resampled column mean is
#'   `theta_q + (1/M) sum_i w_i (G_iq - theta_q)` with `w_i` i.i.d.
#'   Rademacher. This is the wild bootstrap of a sample mean and matches
#'   the respondent-clustered analytic variance to first order, while
#'   propagating through the nonlinear `fun()` by recomputation.
#'
#' @param G Numeric `M x q` matrix of per-respondent contributions.
#' @param fun Function taking a length-`q` numeric vector of column
#'   means and returning a numeric vector (the reported quantities; same
#'   length for every call). Applied to the observed means for the point
#'   estimate and to each resample's means for the bootstrap draws.
#' @param n_boot Integer number of bootstrap resamples.
#' @param boot_type `"wild"` (default) or `"cluster"`.
#' @param level Confidence level for the percentile interval.
#' @param seed Optional integer seed; the RNG state is saved and
#'   restored so the caller's stream is untouched.
#' @return A list with:
#'   * `est` -- numeric vector, the observed quantities `fun(colMeans(G))`;
#'   * `se` -- numeric vector of bootstrap standard errors
#'     (`apply(draws, 1, sd)`);
#'   * `ci_lo`, `ci_hi` -- numeric vectors of percentile CI bounds;
#'   * `n_boot`, `boot_type`, `M`, `n_valid` (resamples that produced a
#'     finite value for at least one quantity).
#' @keywords internal
#' @noRd
.sc_resp_cluster_boot <- function(G, fun,
                                  n_boot = 200L,
                                  boot_type = c("wild", "cluster"),
                                  level = 0.95,
                                  seed = NULL,
                                  weights = NULL) {
  boot_type <- match.arg(boot_type)
  if (!is.matrix(G) || !is.numeric(G)) {
    stop(".sc_resp_cluster_boot(): `G` must be a numeric matrix.")
  }
  M <- nrow(G)
  if (M < 2L) {
    stop(".sc_resp_cluster_boot(): at least 2 respondents (clusters) are required.")
  }
  if (is.null(weights)) {
    survey_w <- rep.int(1, M)
  } else {
    survey_w <- as.numeric(weights)
    if (length(survey_w) != M || any(!is.finite(survey_w)) ||
        any(survey_w < 0) || sum(survey_w) <= 0) {
      stop(".sc_resp_cluster_boot(): invalid respondent weights.")
    }
  }
  a <- survey_w / sum(survey_w)
  n_boot <- as.integer(n_boot)
  if (is.na(n_boot) || n_boot < 1L) {
    stop(".sc_resp_cluster_boot(): `n_boot` must be a positive integer.")
  }

  theta_obs <- as.numeric(crossprod(a, G))
  est <- fun(theta_obs)
  q <- length(est)

  ## Preserve the caller's RNG state so the bootstrap does not perturb
  ## reproducibility (mirrors the .sc_* RNG-hygiene helpers).
  withr::local_preserve_seed()
  if (!is.null(seed)) set.seed(as.integer(seed))

  draws <- matrix(NA_real_, nrow = q, ncol = n_boot)
  for (b in seq_len(n_boot)) {
    if (boot_type == "cluster") {
      idx <- sample.int(M, M, replace = TRUE)
      ab <- survey_w[idx] / sum(survey_w[idx])
      theta_b <- as.numeric(crossprod(ab, G[idx, , drop = FALSE]))
    } else {
      ## Wild: theta_q + sum_i a_i xi_i (G_iq - theta_q)
      w <- .sc_rademacher(M)
      theta_b <- theta_obs + as.numeric(crossprod(a * w, sweep(G, 2L, theta_obs,
                                                               check.margin = FALSE)))
    }
    draws[, b] <- fun(theta_b)
  }

  alpha <- 1 - level
  se <- apply(draws, 1L, stats::sd, na.rm = TRUE)
  ci <- t(apply(draws, 1L, function(z) {
    stats::quantile(z, probs = c(alpha / 2, 1 - alpha / 2),
                    names = FALSE, na.rm = TRUE, type = 7L)
  }))
  n_valid <- sum(apply(draws, 2L, function(z) any(is.finite(z))))

  list(
    est       = est,
    se        = as.numeric(se),
    ci_lo     = ci[, 1L],
    ci_hi     = ci[, 2L],
    n_boot    = n_boot,
    boot_type = boot_type,
    M         = M,
    weights   = survey_w,
    n_valid   = n_valid
  )
}

#' Collapse a task-level per-row matrix to one row per respondent
#'
#' `beta(Z_i)` is constant within a respondent, so every quantity that
#' reads the task-level beta matrix can be reduced, exactly, to the
#' respondent level by taking the first row seen for each respondent (in
#' first-appearance order). Returns the M x p collapsed matrix and the
#' vector of respondent ids in that order.
#'
#' @param Bs Numeric `n_task x p` matrix (already subset to the
#'   subgroup rows).
#' @param resp Vector of length `nrow(Bs)` giving each row's respondent.
#' @return A list with `B_resp` (M x p) and `resp` (length-M id vector).
#' @keywords internal
#' @noRd
.sc_collapse_beta_to_resp <- function(Bs, resp) {
  if (length(resp) != nrow(Bs)) {
    stop(".sc_collapse_beta_to_resp(): `resp` length disagrees with nrow(Bs).")
  }
  first <- !duplicated(resp)
  list(B_resp = Bs[first, , drop = FALSE],
       resp   = resp[first])
}
