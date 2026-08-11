## Derived orthogonal estimands for scmix fits: subgroup means (P1),
## MRS/WTP ratios with joint influence-function covariance (P2), and the
## design-rank preflight for distributional claims (P4).
##
## All three consume the corrected theta-signal built by scmix_theta():
## psi_ik = mu_hat_k(Z_i) + [I^{-1}(Z_i) S_i]_k - loading-influence term.
## Subgroups re-average that psi; ratios delta-method it; the design
## check inspects the effective loading information that the loading
## term inverts.

#' Corrected theta-signal matrix (one row per respondent)
#' @keywords internal
#' @noRd
.scmix_theta_psi <- function(fit, n_bins = 40L, M = 2000L, seed = 1L) {
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  psi <- pr$mu_resp + pr$C
  for (k in seq_len(pr$p)) {
    e_k <- matrix(0, pr$N, pr$p); e_k[, k] <- 1
    psi[, k] <- psi[, k] - .scmix_A_adjust(pr, e_k)
  }
  list(psi = psi, pr = pr)
}

#' Warn when a ratio denominator is weakly separated from zero
#'
#' The regime behind the paper's MRS coverage loss: with
#' |theta_k| / SE(theta_k) below about 4, the delta-method normal
#' interval for theta_j / theta_k undercovers badly. The memo's
#' reporting rule is to take ratios against the largest-|theta|
#' denominator; this guard enforces the diagnostic side of that rule.
#' @keywords internal
#' @noRd
.sc_ratio_denominator_guard <- function(fn, th_k, se_k, label) {
  t_k <- abs(th_k) / se_k
  if (is.finite(t_k) && t_k < 4) {
    warning(fn, "(): weak denominator ", label, " (|theta|/SE = ",
            sprintf("%.1f", t_k), " < 4). The delta-method normal CI for ",
            "the ratio is unreliable in this regime; report the Fieller ",
            "interval, and prefer the largest-|theta| attribute as the ",
            "denominator.", call. = FALSE)
  }
  invisible(t_k)
}

#' Debiased subgroup means from an scmix fit
#'
#' The integrated-likelihood analogue of `sc_average(scale = "logit",
#' subgroup = ...)`: the estimand is `theta_g = E[mu_k(Z_i) | G_i = g]`
#' for an observed respondent grouping G. The point estimate is the
#' within-group mean of the same corrected respondent-level signal that
#' [scmix_theta()] averages (location correction plus loading-influence
#' term), and the SE is the within-group empirical standard error of
#' that signal. Groups must be observed respondent attributes (they are
#' not estimated), so no group-probability term enters the SE.
#'
#' @inheritParams scmix_theta
#' @param by The respondent grouping: a vector of length `N`
#'   (respondents, in first-appearance order) or of length
#'   `nrow(fit$deltaX)` (task rows; the first row per respondent is
#'   used), or a length-1 character naming a moderator column in
#'   `fit$Z`, in which case respondents are split at the median value
#'   (groups `"above"` / `"at_or_below"`).
#' @return An `scmix_quantity` with one entry per group x attribute
#'   dummy, named `"<group>: <dummy>"`; `$extra$groups` records group
#'   sizes.
#' @export
scmix_average <- function(fit, by, n_bins = 40L, M = 2000L, seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  ts <- .scmix_theta_psi(fit, n_bins = n_bins, M = M, seed = seed)
  psi <- ts$psi
  N <- nrow(psi)

  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  if (is.character(by) && length(by) == 1L) {
    if (!(by %in% colnames(fit$Z))) {
      stop("scmix_average(): '", by, "' is not a moderator column in fit$Z.")
    }
    zv <- fit$Z[first, by]
    g <- factor(ifelse(zv > stats::median(zv), "above", "at_or_below"),
                levels = c("at_or_below", "above"))
  } else if (length(by) == nrow(fit$deltaX)) {
    g <- factor(by[first])
  } else if (length(by) == N) {
    g <- factor(by)
  } else {
    stop("scmix_average(): `by` must have length N (respondents), length ",
         "nrow(fit$deltaX) (task rows), or name a moderator column.")
  }
  if (anyNA(g)) stop("scmix_average(): `by` contains NA.")

  levs <- levels(g)
  p <- ncol(psi)
  est <- se <- matrix(NA_real_, length(levs), p)
  for (l in seq_along(levs)) {
    rows <- which(g == levs[l])
    if (length(rows) < 2L) {
      stop("scmix_average(): group '", levs[l], "' has fewer than 2 respondents.")
    }
    est[l, ] <- colMeans(psi[rows, , drop = FALSE])
    se[l, ] <- sqrt(pmax(apply(psi[rows, , drop = FALSE], 2L, stats::var), 0) /
                      length(rows))
  }
  labels <- as.vector(t(outer(levs, fit$attr_names, paste, sep = ": ")))
  est_v <- as.vector(t(est)); se_v <- as.vector(t(se))
  out <- list(
    quantity = "theta by subgroup (population mean, latent scale)",
    estimate = stats::setNames(est_v, labels),
    se = stats::setNames(se_v, labels),
    ci_lower = est_v - stats::qnorm(0.975) * se_v,
    ci_upper = est_v + stats::qnorm(0.975) * se_v,
    n_respondents = N,
    psi = psi,
    extra = list(groups = table(g)),
    call_fit = fit$call
  )
  class(out) <- c("scmix_quantity", "list")
  out
}

#' Debiased population MRS from an scmix fit
#'
#' Estimates `MRS_jk = theta_j / theta_k` on the integrated-likelihood
#' route: both coordinates come from the corrected [scmix_theta()]
#' signal, the SE is the delta-method standard error using the joint
#' respondent-level influence (the cross-covariance between coordinates
#' is free, since the signal is stored as an N x p matrix), and a
#' Fieller interval is returned in `$extra`. A warning fires when the
#' denominator is weakly separated from zero (|theta_k|/SE < 4), the
#' regime in which the delta-method interval undercovers; the memo's
#' reporting rule is to use the largest-|theta| attribute as the
#' denominator.
#'
#' @inheritParams scmix_theta
#' @param numerator,denominator Character of form `"attribute:level"`,
#'   a bare dummy column name, or an integer column index (as in
#'   [sc_mrs()]).
#' @return An `scmix_quantity` with the scalar ratio; `$extra` carries
#'   the Fieller interval and the component estimates.
#' @export
scmix_mrs <- function(fit, numerator, denominator,
                      n_bins = 40L, M = 2000L, seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  j <- .sc_parse_dummy_name(fit, numerator)
  k <- .sc_parse_dummy_name(fit, denominator)
  .scmix_ratio(fit, j, k, transform = "mrs", fn = "scmix_mrs",
               labels = c(numerator, denominator),
               n_bins = n_bins, M = M, seed = seed)
}

#' Debiased population WTP from an scmix fit
#'
#' `-theta_attr / theta_cost` on the integrated-likelihood route; see
#' [scmix_mrs()] for the influence-function covariance, the Fieller
#' interval, and the weak-denominator warning.
#'
#' @inheritParams scmix_mrs
#' @param attr Numerator attribute level.
#' @param cost_attr Single-dummy cost attribute (the denominator).
#' @export
scmix_wtp <- function(fit, attr, cost_attr,
                      n_bins = 40L, M = 2000L, seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  j <- .sc_parse_dummy_name(fit, attr)
  k <- .sc_parse_dummy_name(fit, cost_attr)
  .scmix_ratio(fit, j, k, transform = "wtp", fn = "scmix_wtp",
               labels = c(attr, cost_attr),
               n_bins = n_bins, M = M, seed = seed)
}

#' Shared ratio machinery for scmix_mrs / scmix_wtp
#' @keywords internal
#' @noRd
.scmix_ratio <- function(fit, j, k, transform, fn, labels,
                         n_bins = 40L, M = 2000L, seed = 1L) {
  ts <- .scmix_theta_psi(fit, n_bins = n_bins, M = M, seed = seed)
  psi <- ts$psi
  N <- nrow(psi)
  th <- colMeans(psi)
  th_j <- th[j]; th_k <- th[k]
  V <- stats::var(psi[, c(j, k), drop = FALSE]) / N
  se_k <- sqrt(V[2L, 2L])
  .sc_ratio_denominator_guard(fn, th_k, se_k, labels[2L])

  sgn <- if (transform == "wtp") -1 else 1
  est <- sgn * th_j / th_k
  ## delta-method via the joint respondent-level influence
  IF <- sgn * (psi[, j] * th_k - th_j * psi[, k]) / th_k^2
  se <- stats::sd(IF) / sqrt(N)

  z <- stats::qnorm(0.975); z2 <- z^2
  Vaa <- V[1L, 1L] ; Vbb <- V[2L, 2L]; Vab <- V[1L, 2L]
  if (transform == "mrs") {
    af <- th_k^2 - z2 * Vbb; bf <- -2 * (th_j * th_k - z2 * Vab)
  } else {
    af <- th_k^2 - z2 * Vbb; bf <- +2 * (th_j * th_k - z2 * Vab)
  }
  cf <- th_j^2 - z2 * Vaa
  fie <- .sc_fieller(af, bf, cf)

  qty <- if (transform == "wtp") "WTP (population, integrated route)" else
    "MRS (population, integrated route)"
  out <- list(
    quantity = qty,
    estimate = stats::setNames(est, paste(labels, collapse = " / ")),
    se = stats::setNames(se, paste(labels, collapse = " / ")),
    ci_lower = est - z * se,
    ci_upper = est + z * se,
    n_respondents = N,
    psi = matrix(IF, ncol = 1L),
    extra = list(theta_num = th_j, theta_den = th_k,
                 se_den = se_k, t_den = abs(th_k) / se_k,
                 fieller_lo = fie$lo, fieller_hi = fie$hi,
                 fieller_type = fie$type),
    call_fit = fit$call
  )
  class(out) <- c("scmix_quantity", "list")
  out
}

#' Design-rank preflight for distributional claims
#'
#' Computes the model-implied EFFECTIVE loading information at the
#' design in hand --- the average of
#' `I_AA(Z) - I_Amu(Z) I_mumu(Z)^{-1} I_muA(Z)` over respondents, the
#' matrix whose inverse scales the loading-influence correction --- and
#' reports (i) its eigen-spectrum on the standardized (index) scale and
#' (ii) per-loading standard errors and t-ratios. Distributional
#' estimands (sign shares `pi_k`, residual SDs) are identified only in
#' the loading directions this design actually pins down; coordinates
#' whose loading t-ratio is small cannot support population
#' distributional claims at this design, whatever the point estimates
#' say. Run this before reporting [scmix_polarization()] or residual-SD
#' magnitudes.
#'
#' The finite-design rank analysis behind this check (task T5 of the
#' estimand memo): on the bundled applications it identifies the tax
#' design (T = 8) up to one weak direction, while the candidate design
#' (T = 3) pins down only the agenda coordinates' loadings.
#'
#' @inheritParams scmix_theta
#' @param eig_tol Relative eigenvalue below which a loading direction
#'   is reported as weakly identified (default 0.05).
#' @param t_min Loading t-ratio below which a coordinate is reported as
#'   not supporting distributional claims (default 2).
#' @return An object of class `scmix_design_check`: a list with
#'   `spectrum` (standardized eigenvalues, relative to the largest),
#'   `loadings` (data frame: coordinate, standardized loading, SE, t),
#'   `weak_directions`, `identified`, and the thresholds used.
#' @export
scmix_design_check <- function(fit, n_bins = 40L, M = 2000L, seed = 1L,
                               eig_tol = 0.05, t_min = 2) {
  stopifnot(inherits(fit, "scmix"))
  fit <- .scmix_canon(fit)
  sc <- .scmix_scores(fit)
  info <- .scmix_information(fit, n_bins = n_bins, M = M, seed = seed)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  N <- sum(first); p <- ncol(fit$deltaX); pq <- ncol(sc$S_A)
  q <- pq / p

  I_AAeff_bar <- matrix(0, pq, pq)
  for (i in seq_len(N)) {
    b <- info$bin_of[i]
    B_b <- info$I_inv[[b]] %*% info$I_muA[[b]]
    I_AAeff_bar <- I_AAeff_bar +
      (info$I_AA[[b]] - crossprod(info$I_muA[[b]], B_b)) / N
  }
  sdx <- fit$sd_dx
  if (is.null(sdx)) {
    sdx <- apply(fit$deltaX, 2L, stats::sd)
    sdx[!is.finite(sdx) | sdx < 1e-12] <- 1
  }
  D_A <- diag(rep(1 / sdx, q), pq)
  I_std <- D_A %*% I_AAeff_bar %*% D_A
  eA <- eigen(I_std, symmetric = TRUE)
  rel <- eA$values / max(eA$values, 1e-12)
  I_std_inv <- eA$vectors %*% diag(1 / pmax(eA$values, 1e-12), pq) %*%
    t(eA$vectors)
  se_std <- sqrt(pmax(diag(I_std_inv), 0) / N)
  A_bar <- Reduce(`+`, fit$A_folds) / length(fit$A_folds)
  A_std <- as.numeric(A_bar * sdx)
  tstat <- abs(A_std) / se_std

  loadings <- data.frame(
    coord = rep(fit$attr_names, q),
    factor = rep(seq_len(q), each = p),
    loading_std = A_std, se_std = se_std, t = tstat,
    row.names = NULL)
  out <- list(
    spectrum = rel,
    eigenvalues = eA$values,
    loadings = loadings,
    weak_directions = sum(rel < eig_tol),
    identified = fit$attr_names[tstat[seq_len(p)] >= t_min],
    not_identified = fit$attr_names[tstat[seq_len(p)] < t_min],
    eig_tol = eig_tol, t_min = t_min,
    n_respondents = N, median_T = stats::median(sc$T_i), q = q)
  class(out) <- c("scmix_design_check", "list")
  out
}

#' @export
print.scmix_design_check <- function(x, ...) {
  cat("scmix design-rank check (loading identification at this design)\n")
  cat(sprintf("  respondents: %d   median tasks per respondent: %d   q = %d\n",
              x$n_respondents, x$median_T, x$q))
  cat("  standardized effective-information eigenvalues (rel. to max):\n   ",
      paste(sprintf("%.3f", x$spectrum), collapse = " "), "\n")
  if (x$weak_directions > 0L) {
    cat(sprintf("  %d direction(s) below eig_tol = %.2f: weakly identified.\n",
                x$weak_directions, x$eig_tol))
  }
  df <- x$loadings
  df$loading_std <- round(df$loading_std, 3)
  df$se_std <- round(df$se_std, 3)
  df$t <- round(df$t, 2)
  print(df)
  if (length(x$not_identified) > 0L) {
    cat("  Coordinates NOT supporting distributional claims (t < ",
        x$t_min, "): ", paste(x$not_identified, collapse = ", "), "\n", sep = "")
  } else {
    cat("  All coordinates pass the loading t-ratio threshold.\n")
  }
  invisible(x)
}
