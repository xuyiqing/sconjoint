## Orthogonal-score inference for scmix fits.
##
## Implements the mixed-logit analogue of the paper's (corrected)
## master proposition.  For a population functional
##
##   theta_H = E_Z[ H(mu(Z), Sigma; Z) ],
##
## the orthogonal signal is
##
##   psi_i = H(mu_hat(Z_i), Sigma_hat)
##           + a(Z_i)' I_hat(Z_i)^{-1} S_i,
##   a(Z)  = d H / d mu  (Z-measurable by construction),
##
## with S_i the Fisher-identity location score (R/mixed-scores.R) and
## I_hat the model-implied respondent-level information simulated over
## the known design law.  Cross-fitting: every ingredient for
## respondent i comes from the fold model that did not train on i.
##
## The loading matrix / residual covariance Sigma = AA' is a
## finite-dimensional nuisance estimated by the same likelihood.  Its
## estimation error is NOT yet propagated into the standard errors
## (the influence component for Sigma_hat is a documented follow-up);
## point estimates are unaffected because Sigma_hat is root-N
## consistent, and a fold-spread sensitivity is reported instead.
## theta_k = E[mu_k(Z)] does not involve Sigma at all, so its
## inference is complete as implemented.

#' Shared setup for scmix orthogonal estimands
#' @keywords internal
#' @noRd
.scmix_prep <- function(fit, n_bins = 40L, M = 300L, seed = 1L) {
  sc <- .scmix_scores(fit)
  info <- .scmix_information(fit, n_bins = n_bins, M = M, seed = seed)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  mu_resp <- fit$mu_hat[first, , drop = FALSE]
  ## correction matrix C[i, ] = I^{-1}(Z_i) S_i, one row per respondent
  N <- nrow(mu_resp)
  p <- ncol(mu_resp)
  C <- matrix(0, N, p)
  for (i in seq_len(N)) {
    C[i, ] <- info$I_inv[[info$bin_of[i]]] %*% sc$S[i, ]
  }
  list(sc = sc, info = info, mu_resp = mu_resp, C = C, N = N, p = p)
}

#' Summarize an influence-style estimate
#' @keywords internal
#' @noRd
.scmix_wrap <- function(psi, labels, quantity, fit, extra = NULL) {
  est <- colMeans(psi)
  N <- nrow(psi)
  se <- sqrt(pmax(apply(psi, 2L, stats::var), 0) / N)
  out <- list(
    quantity = quantity,
    estimate = stats::setNames(est, labels),
    se = stats::setNames(se, labels),
    ci_lower = est - stats::qnorm(0.975) * se,
    ci_upper = est + stats::qnorm(0.975) * se,
    n_respondents = N,
    psi = psi,
    extra = extra,
    call_fit = fit$call
  )
  class(out) <- c("scmix_quantity", "list")
  out
}

#' @export
print.scmix_quantity <- function(x, ...) {
  cat(sprintf("scmix orthogonal estimate: %s\n", x$quantity))
  df <- data.frame(estimate = round(x$estimate, 4),
                   se = round(x$se, 4),
                   ci95 = sprintf("[%.3f, %.3f]", x$ci_lower, x$ci_upper))
  print(df)
  invisible(x)
}

#' Debiased population-average preferences from an scmix fit
#'
#' The mixed-logit counterpart of the two-stage `theta`: the target is
#' `theta_k = E[beta_ik] = E[mu_k(Z_i)]`, now genuinely the latent mean
#' because the integrated likelihood removes the projection wedge.
#' The orthogonal score is
#' `psi_ik = mu_hat_k(Z_i) + [I_hat^{-1}(Z_i) S_i]_k`; standard errors
#' are respondent-level (respondents are the independent clusters).
#'
#' @param fit An `scmix` object.
#' @param n_bins,M,seed Controls for the model-implied information
#'   simulation (see `.scmix_information()`).
#' @return An `scmix_quantity` with one row per attribute dummy.
#' @export
scmix_theta <- function(fit, n_bins = 40L, M = 300L, seed = 1L) {
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  psi <- pr$mu_resp + pr$C
  .scmix_wrap(psi, fit$attr_names, "theta (population mean, latent scale)", fit)
}

#' Debiased population sign shares (polarization) from an scmix fit
#'
#' Estimates `pi_k = E_Z[ Pr(beta_ik > 0 | Z_i) ]` under the maintained
#' mixing law: conditional on Z, `beta_k ~ N(mu_k(Z), sigma_k^2)` with
#' `sigma_k^2 = (AA')_kk`, so `H = Phi(mu_k(Z)/sigma_k)` and the
#' mu-gradient is `phi(mu_k/sigma_k)/sigma_k * e_k`.  In the two-stage
#' framework this quantity had no orthogonal score and was reported as
#' a shrinkage-biased MAP sign fraction; under the maintained mixing
#' law it is an identified smooth functional with debiased inference.
#'
#' `sigma_k` uses each respondent's fold-specific loading matrix
#' (cross-fitted); its estimation error is not yet in the SEs (see the
#' file header).  A fold-spread range for `sigma_k` is returned in
#' `$extra` as a sensitivity check.
#'
#' @inheritParams scmix_theta
#' @param sd_floor Lower bound on the conditional residual SD used in
#'   `H = Phi(mu_k/sigma_k)`.  When `(AA')_kk` is at or near zero the
#'   sign-share functional degenerates to the step `1{mu_k(Z) > 0}` and
#'   is no longer smooth in `mu` --- the gradient `phi/sigma_k` blows up
#'   and the orthogonal correction can push the estimate outside
#'   `[0, 1]`.  Coordinates floored here are reported in `$extra$floored`
#'   and should be read as "residual variance indistinguishable from
#'   zero; the share is effectively the sign of the conditional mean."
#' @export
scmix_polarization <- function(fit, n_bins = 40L, M = 300L, seed = 1L,
                               sd_floor = 0.05) {
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  sd_folds <- vapply(fit$A_folds,
                     function(A) sqrt(pmax(diag(tcrossprod(A)), 1e-12)),
                     numeric(pr$p))
  if (is.null(dim(sd_folds))) sd_folds <- matrix(sd_folds, nrow = pr$p)
  floored <- rowMeans(sd_folds) < sd_floor
  if (any(floored)) {
    warning("scmix_polarization(): residual SD floored at ", sd_floor,
            " for: ", paste(fit$attr_names[floored], collapse = ", "),
            " (residual variance ~ 0; the sign share degenerates to the",
            " sign of the conditional mean there).")
  }
  sd_folds <- pmax(sd_folds, sd_floor)
  fold_resp <- pr$sc$fold_resp
  psi <- matrix(0, pr$N, pr$p)
  for (i in seq_len(pr$N)) {
    s <- sd_folds[, fold_resp[i]]
    zsc <- pr$mu_resp[i, ] / s
    h <- stats::pnorm(zsc)
    a <- stats::dnorm(zsc) / s
    psi[i, ] <- h + a * pr$C[i, ]
  }
  .scmix_wrap(psi, fit$attr_names, "pi (share with beta_k > 0)", fit,
              extra = list(sigma_k_by_fold = sd_folds,
                           sigma_k_range = t(apply(sd_folds, 1L, range)),
                           floored = fit$attr_names[floored]))
}

#' Debiased eta-integrated counterfactual choice share
#'
#' Estimates `V(c) = E_Z[ E_u G(c' (mu(Z) + A u)) ]` -- the
#' heterogeneity-integrated share choosing profile bundle A over B for
#' a contrast vector `c`.  This is the "Jensen gap" object the
#' two-stage paper explicitly could not point-identify at fixed T; it
#' is identified under the maintained mixing law.  `H` integrates over
#' the quadrature grid; the mu-gradient is
#' `sum_g w_g G'(c' beta_g) c`.
#'
#' @inheritParams scmix_theta
#' @param contrast Numeric vector of length p (attribute-dummy scale),
#'   or a named subset that is expanded against `fit$attr_names`.
#' @export
scmix_counterfactual <- function(fit, contrast, n_bins = 40L, M = 300L,
                                 seed = 1L) {
  p <- ncol(fit$deltaX)
  if (!is.null(names(contrast))) {
    cv <- stats::setNames(numeric(p), fit$attr_names)
    bad <- setdiff(names(contrast), fit$attr_names)
    if (length(bad) > 0L) {
      stop("scmix_counterfactual(): unknown contrast names: ",
           paste(bad, collapse = ", "))
    }
    cv[names(contrast)] <- as.numeric(contrast)
  } else {
    if (length(contrast) != p) {
      stop("scmix_counterfactual(): `contrast` must have length ", p,
           " or be a named vector.")
    }
    cv <- as.numeric(contrast)
  }

  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  gh <- fit$gh
  sigm <- function(x) 1 / (1 + exp(-x))
  fold_resp <- pr$sc$fold_resp

  psi <- matrix(0, pr$N, 1L)
  for (i in seq_len(pr$N)) {
    A <- fit$A_folds[[fold_resp[i]]]
    idx <- sum(cv * pr$mu_resp[i, ]) + as.numeric((t(cv) %*% A) %*% t(gh$U))
    pg <- sigm(idx)
    h <- sum(gh$w * pg)
    aZ <- sum(gh$w * pg * (1 - pg)) * cv
    psi[i, 1L] <- h + sum(aZ * pr$C[i, ])
  }
  .scmix_wrap(psi, "V(c)", "eta-integrated counterfactual share", fit,
              extra = list(contrast = stats::setNames(cv, fit$attr_names)))
}

#' Orthogonality self-check for the scmix construction
#'
#' Perturbs the location nuisance by `delta` in a fixed direction and
#' verifies that the orthogonal estimate moves O(delta^2) while the
#' plug-in moves O(delta).  Returns a data frame with one row per
#' delta; used by the test suite and the memo.
#'
#' @keywords internal
#' @noRd
.scmix_check_orthogonality <- function(fit, deltas = c(0.1, 0.05, 0.025),
                                       coord = 1L, n_bins = 40L, M = 300L,
                                       seed = 1L) {
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  base_est <- colMeans(pr$mu_resp + pr$C)[coord]
  out <- data.frame(delta = deltas, plugin_shift = NA_real_,
                    orth_shift = NA_real_)
  for (j in seq_along(deltas)) {
    d <- deltas[j]
    mu2 <- fit$mu_hat
    mu2[, coord] <- mu2[, coord] + d
    sc2 <- .scmix_scores(fit, mu_override = mu2)
    resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
    first <- !duplicated(as.integer(resp_f))
    mu_resp2 <- mu2[first, , drop = FALSE]
    C2 <- matrix(0, pr$N, pr$p)
    for (i in seq_len(pr$N)) {
      C2[i, ] <- pr$info$I_inv[[pr$info$bin_of[i]]] %*% sc2$S[i, ]
    }
    out$plugin_shift[j] <- mean(mu_resp2[, coord]) - mean(pr$mu_resp[, coord])
    out$orth_shift[j] <- colMeans(mu_resp2 + C2)[coord] - base_est
  }
  out
}
