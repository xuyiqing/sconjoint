#' Preference polarization index per dummy
#'
#' For every dummy column \eqn{j}, computes
#' \eqn{1 - |\mathrm{frac}^+_j - \mathrm{frac}^-_j|} where
#' \eqn{\mathrm{frac}^\pm_j} is the subgroup-averaged indicator of
#' \eqn{\hat\beta_j(Z_i) > 0} (resp. `< 0`).  Values near 1 indicate
#' a population that is evenly split in direction (highly polarized);
#' values near 0 indicate near-uniform agreement.
#'
#' Point estimate is a direct port of `07b_structural_quantities.R`
#' lines 478--497. The polarization index is a bounded nonlinear
#' transform of two proportions and has no clean clustered-sd
#' expression, so by default (`se_method = "none"`) no standard error is
#' returned. A respondent-cluster (wild) bootstrap is available via
#' `se_method = "wild_bootstrap"`: the recovered per-respondent
#' \eqn{\hat\beta_i} are resampled at the respondent level and
#' \eqn{\mathrm{frac}^+_j}, \eqn{\mathrm{frac}^-_j}, and the
#' polarization index are recomputed on each resample, giving bootstrap
#' standard errors and percentile confidence intervals for all three
#' (including the nonlinear index). The deep network is \emph{not} refit
#' inside the bootstrap: only the respondent-level aggregation is
#' resampled. As with `sc_fraction_preferring()`, fixed-\eqn{T}
#' shrinkage biases the plug-in fractions toward consensus, so the
#' interval reflects \strong{sampling variability}, not the shrinkage
#' bias. See the *Inference validity by quantity* section of `?scfit`.
#'
#' @param object An `sc_fit`.
#' @param subgroup Row selector.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @param se_method One of `"none"` (default; `NA` SEs, the v0.1
#'   behavior) or `"wild_bootstrap"` (respondent-cluster bootstrap SEs
#'   and percentile CIs). See Details.
#' @param n_boot Integer number of bootstrap resamples when
#'   `se_method = "wild_bootstrap"`. Default `200L`.
#' @param boot_type Bootstrap scheme when `se_method = "wild_bootstrap"`:
#'   `"wild"` (default; Rademacher weights) or `"cluster"` (nonparametric
#'   respondent resampling).
#' @param boot_seed Optional integer seed for the bootstrap. The RNG
#'   state is saved and restored, so the caller's stream is unaffected.
#' @return An `sc_quantity` with `estimate` a data.frame of one row
#'   per dummy (`dummy_name`, `frac_positive`, `frac_negative`,
#'   `polarization_idx`, and SE/CI columns `se`, `ci_lo`, `ci_hi` for the
#'   polarization index). Under `se_method = "none"` the SE/CI columns
#'   are `NA`; under `"wild_bootstrap"` they are the bootstrap SE and
#'   percentile bounds for `polarization_idx`, and the extra columns
#'   `se_positive`, `se_negative` carry the bootstrap SEs of the two
#'   fractions.
#' @export
sc_polarization <- function(object, subgroup = NULL,
                            which_beta = c("hybrid", "dnn"),
                            se_method = c("none", "wild_bootstrap"),
                            n_boot = 200L,
                            boot_type = c("wild", "cluster"),
                            boot_seed = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  which_beta <- match.arg(which_beta)
  se_method  <- match.arg(se_method)
  boot_type  <- match.arg(boot_type)
  B <- .sc_pick_beta(object, which_beta)
  S <- .sc_resolve_subgroup(object, subgroup)
  Bs <- B[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  w_s <- .sc_weights_for_rows(object, S)
  p <- ncol(B)
  if (is.null(w_s)) {
    fp <- colMeans(Bs > 0)
    fn <- colMeans(Bs < 0)
  } else {
    fp <- vapply(seq_len(p), function(j)
      .sc_weighted_task_mean(as.numeric(Bs[, j] > 0), resp_s, w_s),
      numeric(1L))
    fn <- vapply(seq_len(p), function(j)
      .sc_weighted_task_mean(as.numeric(Bs[, j] < 0), resp_s, w_s),
      numeric(1L))
  }
  poli <- 1 - abs(fp - fn)

  se_poli <- rep(NA_real_, p)
  ci_lo   <- rep(NA_real_, p)
  ci_hi   <- rep(NA_real_, p)
  se_p    <- rep(NA_real_, p)
  se_n    <- rep(NA_real_, p)

  if (se_method == "wild_bootstrap") {
    col <- .sc_collapse_beta_to_resp(Bs, resp_s)
    Br  <- col$B_resp                                  # M x p
    w_resp <- if (is.null(w_s)) NULL else .sc_respondent_weight_object(resp_s, w_s)$w
    ind_pos <- (Br > 0) * 1                            # M x p
    ind_neg <- (Br < 0) * 1                            # M x p
    G <- cbind(ind_pos, ind_neg)                       # M x 2p; colMeans = c(fp, fn)
    ## fun maps column means c(fp, fn) -> c(fp, fn, poli)
    fun <- function(m) {
      fpj <- m[seq_len(p)]
      fnj <- m[p + seq_len(p)]
      c(fpj, fnj, 1 - abs(fpj - fnj))
    }
    bt <- .sc_resp_cluster_boot(
      G, fun = fun, n_boot = n_boot, boot_type = boot_type,
      level = 0.95, seed = boot_seed, weights = w_resp
    )
    se_p    <- bt$se[seq_len(p)]
    se_n    <- bt$se[p + seq_len(p)]
    se_poli <- bt$se[2L * p + seq_len(p)]
    ci_lo   <- bt$ci_lo[2L * p + seq_len(p)]
    ci_hi   <- bt$ci_hi[2L * p + seq_len(p)]
    details <- list(subgroup_size = length(S), se_method = "wild_bootstrap",
                    n_boot = bt$n_boot, boot_type = bt$boot_type,
                    n_respondents = bt$M)
  } else {
    details <- list(subgroup_size = length(S), se_method = "none")
  }

  df <- data.frame(
    dummy_name       = object$attr_names,
    frac_positive    = fp,
    frac_negative    = fn,
    polarization_idx = poli,
    se               = se_poli,
    ci_lo            = ci_lo,
    ci_hi            = ci_hi,
    se_positive      = se_p,
    se_negative      = se_n,
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
  .sc_quantity(
    name = "polarization",
    estimate = df,
    se = NA_real_,
    details = details,
    call = match.call()
  )
}
