#' Fraction of respondents with beta above / below a threshold, per dummy
#'
#' For every dummy column \eqn{j} and row subgroup \eqn{S}, computes
#' \eqn{\mathrm{frac}^+_j(\tau) = \Pr_{i\in S}\{\hat\beta_j(Z_i) > \tau\}}
#' and the symmetric lower-tail fraction
#' \eqn{\mathrm{frac}^-_j(\tau) = \Pr_{i\in S}\{\hat\beta_j(Z_i) < -\tau\}}.
#' The default threshold is \eqn{\tau = 0}, giving the fraction of
#' respondents preferring each non-reference level over the reference.
#'
#' This is a \emph{descriptive} summary of the distribution of the
#' predicted/shrunken per-respondent slopes, not a debiased estimator (see the
#' *Legacy inferential status* section of `?scfit`). Two standard
#' error rules are offered via `se_method`:
#' \itemize{
#'   \item `"clustered"` (default, the v0.1 behavior): respondent-clustered
#'     standard errors on the Bernoulli indicators
#'     \eqn{1\{\hat\beta_j(Z_i) > \tau\}}, treating each indicator as an
#'     observed 0/1 outcome. These are correct for the variance of a
#'     clustered proportion but ignore that the indicators are taken of
#'     \emph{shrunken} coefficients.
#'   \item `"wild_bootstrap"`: a respondent-cluster (wild) bootstrap. The
#'     predicted/shrunken \eqn{\hat\beta_i} is resampled at the respondent level
#'     (`boot_type = "wild"` applies Rademacher weights to the centered
#'     per-respondent indicator contributions; `boot_type = "cluster"`
#'     draws respondents with replacement) and the fraction is recomputed
#'     on each of `n_boot` resamples. The deep network is \emph{not}
#'     refit inside the bootstrap: only the respondent-level aggregation
#'     step is resampled, which is what carries the sampling uncertainty
#'     of these fractions. The returned interval is a percentile interval
#'     and reflects \strong{sampling variability of the fraction}, not the
#'     finite-\eqn{T} shrinkage bias that pulls each \eqn{\hat\beta_i}
#'     toward consensus and so biases the fraction toward agreement.
#' }
#'
#'
#' When the fit carries an attribute-interaction term
#' (`scfit(..., interactions != "none")`), this quantity is computed from
#' the per-respondent `beta_i` exactly as before, but `beta_i` is then
#' the MAIN-EFFECT part of the utility (the coefficient on `deltaX` at
#' the no-interaction baseline), not the all-else-equal effect.
#' @param object An `sc_fit`.
#' @param threshold Non-negative scalar `tau`.
#' @param subgroup Row selector.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @param se_method One of `"clustered"` (default) or `"wild_bootstrap"`.
#'   See Details.
#' @param n_boot Integer number of bootstrap resamples when
#'   `se_method = "wild_bootstrap"`. Default `200L`.
#' @param boot_type Bootstrap scheme when `se_method = "wild_bootstrap"`:
#'   `"wild"` (default; Rademacher weights) or `"cluster"` (nonparametric
#'   respondent resampling).
#' @param boot_seed Optional integer seed for the bootstrap. The RNG
#'   state is saved and restored, so the caller's stream is unaffected.
#' @return An `sc_quantity` with `estimate` a data.frame. The SE / CI
#'   columns (`se_positive`, `se_negative`, `ci_*`) are filled by the
#'   chosen `se_method`. When `se_method = "wild_bootstrap"`,
#'   `details$se_method` is `"wild_bootstrap"` and `details$n_boot`,
#'   `details$boot_type` record the bootstrap settings.
#' @section Population claims:
#' This function describes the fitted respondent-level (MAP) estimates.
#' Under the estimand-estimator correspondence it is limited to
#' descriptive use of those fitted values: population sign shares and
#' heterogeneity magnitudes require the integrated-likelihood route
#' ([scmix_polarization()] and its design checks), and MAP fractions
#' are biased for population shares (9--13 percentage points in the
#' head-to-head simulations, with no standard errors).
#' @export
sc_fraction_preferring <- function(object, threshold = 0, subgroup = NULL,
                                   which_beta = c("hybrid", "dnn"),
                                   se_method = c("clustered", "wild_bootstrap"),
                                   n_boot = 200L,
                                   boot_type = c("wild", "cluster"),
                                   boot_seed = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  which_beta <- match.arg(which_beta)
  se_method  <- match.arg(se_method)
  boot_type  <- match.arg(boot_type)
  if (!is.numeric(threshold) || length(threshold) != 1L || threshold < 0) {
    stop("sc_fraction_preferring(): `threshold` must be a non-negative scalar.")
  }
  B <- .sc_pick_beta(object, which_beta)
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- B[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  w_s <- .sc_weights_for_rows(object, S)
  p <- ncol(B)
  if (is.null(w_s)) {
    fp <- colMeans(Bs > threshold)
    fn <- colMeans(Bs < -threshold)
  } else {
    fp <- vapply(seq_len(p), function(j)
      .sc_weighted_task_mean(as.numeric(Bs[, j] > threshold), resp_s, w_s),
      numeric(1L))
    fn <- vapply(seq_len(p), function(j)
      .sc_weighted_task_mean(as.numeric(Bs[, j] < -threshold), resp_s, w_s),
      numeric(1L))
  }
  ci_q <- stats::qnorm(0.975)

  if (se_method == "clustered") {
    se_p <- numeric(p)
    se_n <- numeric(p)
    for (j in seq_len(p)) {
      se_p[j] <- .sc_cluster_se(as.numeric(Bs[, j] > threshold), resp_s, w_s)
      se_n[j] <- .sc_cluster_se(as.numeric(Bs[, j] < -threshold), resp_s, w_s)
    }
    ci_lo_p <- fp - ci_q * se_p
    ci_hi_p <- fp + ci_q * se_p
    ci_lo_n <- fn - ci_q * se_n
    ci_hi_n <- fn + ci_q * se_n
    details <- list(threshold = threshold, subgroup_size = length(S),
                    se_method = "clustered")
  } else {
    ## Respondent-cluster (wild) bootstrap. Collapse to one beta per
    ## respondent (beta is constant within respondent), build the
    ## per-respondent indicator contributions for both tails, and
    ## resample respondents.
    col <- .sc_collapse_beta_to_resp(Bs, resp_s)
    Br  <- col$B_resp                                  # M x p
    w_resp <- if (is.null(w_s)) NULL else .sc_respondent_weight_object(resp_s, w_s)$w
    ind_pos <- (Br > threshold) * 1                    # M x p
    ind_neg <- (Br < -threshold) * 1                   # M x p
    G <- cbind(ind_pos, ind_neg)                       # M x 2p, colMeans = c(fp, fn)
    bt <- .sc_resp_cluster_boot(
      G, fun = function(m) m,                          # identity: report the fractions
      n_boot = n_boot, boot_type = boot_type,
      level = 0.95, seed = boot_seed, weights = w_resp
    )
    se_p <- bt$se[seq_len(p)]
    se_n <- bt$se[p + seq_len(p)]
    ci_lo_p <- bt$ci_lo[seq_len(p)]
    ci_hi_p <- bt$ci_hi[seq_len(p)]
    ci_lo_n <- bt$ci_lo[p + seq_len(p)]
    ci_hi_n <- bt$ci_hi[p + seq_len(p)]
    details <- list(threshold = threshold, subgroup_size = length(S),
                    se_method = "wild_bootstrap", n_boot = bt$n_boot,
                    boot_type = bt$boot_type, n_respondents = bt$M)
  }

  df <- data.frame(
    dummy_name       = object$attr_names,
    frac_positive    = fp,
    frac_negative    = fn,
    se_positive      = se_p,
    se_negative      = se_n,
    ci_lo_positive   = ci_lo_p,
    ci_hi_positive   = ci_hi_p,
    ci_lo_negative   = ci_lo_n,
    ci_hi_negative   = ci_hi_n,
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
  .sc_quantity(
    name = "fraction_preferring",
    estimate = df,
    se = NA_real_,
    details = details,
    call = match.call()
  )
}
