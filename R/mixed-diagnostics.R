## Post-fit and pre-fielding diagnostics for the integrated estimator:
## the second profile-Newton linearity check (estimand-memo T3) and the
## hypothesized-value design precheck (T5 extension).

#' Second profile-Newton linearity diagnostic for the loading estimate
#'
#' The shipped loading-influence correction is algebraically a one-step
#' profile-likelihood Newton step on vec(A); treating `A_hat` as
#' asymptotically linear rests on that one step being enough.  This
#' diagnostic takes the free first increment
#' `delta_1 = mean_i IF_A[i, ]`, applies it to every fold loading, and
#' computes the SECOND increment `delta_2` from the effective scores at
#' the incremented loadings, holding the information blocks at their
#' base values (their dependence on the perturbation is second order,
#' which keeps the diagnostic at roughly one preparation cost).  A
#' small second increment -- `ratio` well below one and `max_rel_se`
#' well below one -- supports the linearity assumption behind the
#' correction; reporting only, no hard threshold.
#'
#' @inheritParams scmix_theta
#' @return An object of class `scmix_linearity_check`: `delta1` and
#'   `delta2` (p x q, raw units), their standardized norms `norm1_std`
#'   and `norm2_std`, `ratio = norm2_std / norm1_std`, `max_rel_se`
#'   (largest standardized second-increment coordinate over its loading
#'   SE), and `n_truncated` (loading directions the correction projects
#'   out; increments live in the kept subspace).
#' @export
scmix_linearity_check <- function(fit, n_bins = 40L, M = 2000L, seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  fit <- .scmix_canon(fit)
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  p <- pr$p
  q <- pr$pq / p
  sd_dx <- pr$sd_dxA

  delta1 <- colMeans(pr$IF_A)
  A2_folds <- lapply(fit$A_folds, function(A) A + matrix(delta1, p, q))
  sc2 <- .scmix_scores(fit, A_override = A2_folds)

  ## effective scores at the incremented loadings, base information
  S_Aeff2 <- matrix(0, pr$N, pr$pq)
  for (i in seq_len(pr$N)) {
    b <- pr$info$bin_of[i]
    S_Aeff2[i, ] <- sc2$S_A[i, ] -
      crossprod(pr$info$I_muA[[b]], pr$info$I_inv[[b]] %*% sc2$S[i, ])
  }
  delta2 <- as.numeric(pr$I_AAeff_inv %*% colMeans(S_Aeff2))

  std <- rep(sd_dx, q)
  d1_std <- delta1 * std
  d2_std <- delta2 * std
  norm1 <- sqrt(sum(d1_std^2))
  norm2 <- sqrt(sum(d2_std^2))

  ## standardized loading SEs from the plain floored inverse (the
  ## design-check convention -- flagging, not correction)
  eigA <- pr$eigA
  I_std_inv <- eigA$vectors %*%
    diag(1 / pmax(eigA$values, 1e-12), pr$pq) %*% t(eigA$vectors)
  se_std <- sqrt(pmax(diag(I_std_inv), 0) / pr$N)
  max_rel_se <- max(abs(d2_std) / se_std)

  out <- list(delta1 = matrix(delta1, p, q), delta2 = matrix(delta2, p, q),
              norm1_std = norm1, norm2_std = norm2,
              ratio = norm2 / max(norm1, 1e-300),
              max_rel_se = max_rel_se,
              n_truncated = sum(!eigA$keep))
  class(out) <- c("scmix_linearity_check", "list")
  out
}

#' @export
print.scmix_linearity_check <- function(x, ...) {
  cat("scmix linearity check (second profile-Newton increment)\n")
  cat(sprintf("  ||delta1||_std = %.4g   ||delta2||_std = %.4g   ratio = %.3f\n",
              x$norm1_std, x$norm2_std, x$ratio))
  cat(sprintf("  max |delta2_std| / loading SE = %.3f\n", x$max_rel_se))
  if (x$n_truncated > 0L) {
    cat(sprintf("  %d loading direction(s) projected out (increments live in the kept subspace)\n",
                x$n_truncated))
  }
  cat(if (x$ratio < 1 && x$max_rel_se < 1) {
    "  Reading: the second increment is small; the one-step linearity\n  assumption behind the loading correction is supported on this fit.\n"
  } else {
    "  Reading: the second increment is NOT small relative to the first\n  (or to the loading SEs); interpret loading-dependent inference with\n  caution -- the one-step linearity assumption is strained here.\n"
  })
  invisible(x)
}

#' Hypothesized-value design check (pre-fielding)
#'
#' The design-side counterpart of [scmix_design_check()]: instead of a
#' fitted model, it takes hypothesized values -- a mean vector (or
#' scenario matrix) `mu`, a loading matrix `A`, the planned contrast
#' pool `deltaX`, task counts `T_i`, and the planned number of
#' respondents `N` -- and reports which loading coordinates that design
#' would identify.  Run it before fielding: a design that cannot
#' identify the loadings it needs cannot support distributional claims,
#' whatever the data later say.  `N` scales the loading t-ratios, so
#' doubling the planned sample raises every t by roughly sqrt(2).
#'
#' @param deltaX Numeric matrix of planned attribute contrasts (the
#'   pool tasks are resampled from), one column per attribute dummy;
#'   column names become the coordinate labels.
#' @param mu Hypothesized mean coefficient vector of length p, or an
#'   N x p matrix of scenario means.
#' @param A Hypothesized p x q loading matrix (`q <= 3`).
#' @param T_i Tasks per respondent: a scalar or a length-N vector
#'   (unbalanced designs split simulation bins by distinct counts).
#' @param N Planned number of respondents.
#' @param n_nodes Gauss-Hermite nodes per residual dimension.
#' @inheritParams scmix_design_check
#' @return An `scmix_design_check` object with `source =
#'   "hypothesized"`.
#' @export
scmix_design_precheck <- function(deltaX, mu, A, T_i, N,
                                  n_nodes = 31L, n_bins = 40L, M = 2000L,
                                  seed = 1L, eig_tol = 0.05, t_min = 2) {
  deltaX <- as.matrix(deltaX)
  storage.mode(deltaX) <- "double"
  if (!all(is.finite(deltaX))) {
    stop("scmix_design_precheck(): `deltaX` must be finite numeric.")
  }
  p <- ncol(deltaX)
  A <- as.matrix(A)
  if (nrow(A) != p) {
    stop("scmix_design_precheck(): `A` must have ", p,
         " rows (one per attribute dummy).")
  }
  q <- ncol(A)
  if (q > 3L) stop("scmix_design_precheck(): q = ncol(A) must be <= 3.")
  if (length(N) != 1L || !is.finite(N) || N < 2L) {
    stop("scmix_design_precheck(): `N` must be a single count >= 2.")
  }
  N <- as.integer(N)
  if (is.matrix(mu)) {
    if (ncol(mu) != p || nrow(mu) != N) {
      stop("scmix_design_precheck(): a matrix `mu` must be N x p.")
    }
    mu_resp <- mu
  } else {
    if (length(mu) != p) {
      stop("scmix_design_precheck(): `mu` must have length ", p, ".")
    }
    mu_resp <- matrix(as.numeric(mu), N, p, byrow = TRUE)
  }
  if (length(T_i) == 1L) T_i <- rep(as.integer(T_i), N)
  if (length(T_i) != N || any(!is.finite(T_i)) || any(T_i < 1L)) {
    stop("scmix_design_precheck(): `T_i` must be a positive scalar or a",
         " length-N vector of task counts.")
  }
  T_i <- as.integer(T_i)

  attr_names <- colnames(deltaX)
  if (is.null(attr_names)) attr_names <- paste0("X", seq_len(p))
  sd_dx <- .sc_sd_dx_cols(deltaX)
  gh <- .sc_gh_grid(q, n_nodes = n_nodes)

  info <- .sc_mixed_info_core(mu_resp = mu_resp, T_i = T_i, A_sim = A,
                              pool = deltaX, sd_dx = sd_dx, gh = gh,
                              n_bins = n_bins, M = M, seed = seed)
  eff <- .scmix_eff_info_bar(info, N)
  .scmix_design_check_core(
    I_AAeff_bar = eff$I_AAeff_bar, A_bar = A, sd_dx = sd_dx,
    attr_names = attr_names, p = p, q = q, N = N,
    median_T = stats::median(T_i), eig_tol = eig_tol, t_min = t_min,
    source = "hypothesized")
}
