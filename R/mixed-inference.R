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
## The loading matrix is a finite-dimensional nuisance estimated by the
## same likelihood, and the location score is NOT orthogonal to it (the
## cross-information I_muA is nonzero, so a loading error transmits
## into the pseudo-true location).  Every estimand therefore adds a
## loading-influence term -Gamma_A' IF_A built from the loading scores
## S_A, the per-bin cross-information blocks, and the ridge-guarded
## effective loading information; the nonzero mean of IF_A at the
## fitted loadings doubles as a correction for early-stopped training.
## This treats A-hat as an asymptotically linear estimator; the
## root-N/profile-likelihood conditions behind that are stated in the
## memo as assumptions, not established here.

#' Shared setup for scmix orthogonal estimands
#' @keywords internal
#' @noRd
.scmix_prep <- function(fit, n_bins = 40L, M = 2000L, seed = 1L) {
  sc <- .scmix_scores(fit)
  info <- .scmix_information(fit, n_bins = n_bins, M = M, seed = seed)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  mu_resp <- fit$mu_hat[first, , drop = FALSE]
  ## correction matrix C[i, ] = I^{-1}(Z_i) S_i, one row per respondent
  N <- nrow(mu_resp)
  p <- ncol(mu_resp)
  pq <- ncol(sc$S_A)
  C <- matrix(0, N, p)
  ## effective loading score S_Aeff,i = S_A,i - I_Amu I_mumu^{-1} S_mu,i,
  ## its (respondent-averaged) effective information, and the per-bin
  ## sensitivity block B(Z) = I_mumu(Z)^{-1} I_muA(Z), which measures how
  ## a loading error transmits into the pseudo-true location
  S_Aeff <- matrix(0, N, pq)
  B_bin <- lapply(seq_along(info$I_inv), function(b)
    info$I_inv[[b]] %*% info$I_muA[[b]])
  I_AAeff_bar <- matrix(0, pq, pq)
  for (i in seq_len(N)) {
    b <- info$bin_of[i]
    C[i, ] <- info$I_inv[[b]] %*% sc$S[i, ]
    S_Aeff[i, ] <- sc$S_A[i, ] - crossprod(info$I_muA[[b]], C[i, ])
    I_AAeff_bar <- I_AAeff_bar +
      (info$I_AA[[b]] - crossprod(info$I_muA[[b]], B_bin[[b]])) / N
  }
  ## ridge-guarded inverse of the effective loading information
  eA <- eigen(I_AAeff_bar, symmetric = TRUE)
  vals <- pmax(eA$values, 1e-8 * max(eA$values, 1e-12))
  I_AAeff_inv <- eA$vectors %*% diag(1 / vals, pq) %*% t(eA$vectors)
  ## influence of the loading estimate per respondent
  IF_A <- S_Aeff %*% I_AAeff_inv
  list(sc = sc, info = info, mu_resp = mu_resp, C = C, N = N, p = p,
       pq = pq, B_bin = B_bin, IF_A = IF_A)
}

#' Loading-influence adjustment for an estimand with gradient rows a_i
#'
#' For theta_H with mu-gradient a(Z_i), the sensitivity of the corrected
#' plug-in to a loading error is Gamma_A = mean_i a_i' B(Z_i) with
#' B(Z) = I_mumu(Z)^{-1} I_muA(Z); the missing influence term is
#' Gamma_A applied to the loading influence IF_A,i.  `a_rows` is N x p.
#' Returns the N-vector adjustment to ADD to psi (sign validated by the
#' loading-perturbation transmission check in the test suite).
#' @keywords internal
#' @noRd
.scmix_A_adjust <- function(pr, a_rows) {
  N <- pr$N
  GammaA <- matrix(0, 1L, pr$pq)
  for (i in seq_len(N)) {
    GammaA <- GammaA + (a_rows[i, , drop = FALSE] %*%
                          pr$B_bin[[pr$info$bin_of[i]]]) / N
  }
  as.numeric(pr$IF_A %*% t(GammaA))
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
scmix_theta <- function(fit, n_bins = 40L, M = 2000L, seed = 1L) {
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  psi <- pr$mu_resp + pr$C
  ## loading-influence term, coordinate by coordinate (a_i = e_k)
  for (k in seq_len(pr$p)) {
    e_k <- matrix(0, pr$N, pr$p); e_k[, k] <- 1
    psi[, k] <- psi[, k] - .scmix_A_adjust(pr, e_k)
  }
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
#' @param sd_floor Lower bound on the INDEX-SCALE residual SD (the
#'   per-unit SD times the contrast SD, so the threshold is invariant to
#'   attribute units) used in `H = Phi(mu_k/sigma_k)`.  When `(AA')_kk` is at or near zero the
#'   sign-share functional degenerates to the step `1{mu_k(Z) > 0}` and
#'   is no longer smooth in `mu` --- the gradient `phi/sigma_k` blows up
#'   and the orthogonal correction can push the estimate outside
#'   `[0, 1]`.  Coordinates floored here are reported in `$extra$floored`
#'   and should be read as "residual variance indistinguishable from
#'   zero; the share is effectively the sign of the conditional mean."
#' @export
scmix_polarization <- function(fit, n_bins = 40L, M = 2000L, seed = 1L,
                               sd_floor = 0.05) {
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  sd_folds <- vapply(fit$A_folds,
                     function(A) sqrt(pmax(diag(tcrossprod(A)), 1e-12)),
                     numeric(pr$p))
  if (is.null(dim(sd_folds))) sd_folds <- matrix(sd_folds, nrow = pr$p)
  ## A coordinate is floored if ANY fold's residual SD sits below the
  ## floor. Floored coordinates get NA rather than a number: the sign
  ## share there is a function of the arbitrary floor constant, not of
  ## the data (sweeping the floor moves the estimate by far more than
  ## its SE), so no number is the honest report.
  ## compare on the index scale (sigma_k times the contrast SD) so the
  ## floor decision is invariant to attribute units
  sd_dx0 <- fit$sd_dx
  if (is.null(sd_dx0)) {
    sd_dx0 <- apply(fit$deltaX, 2L, stats::sd)
    sd_dx0[!is.finite(sd_dx0) | sd_dx0 < 1e-12] <- 1
  }
  sd_folds_idx <- sd_folds * sd_dx0
  floored <- apply(sd_folds_idx < sd_floor, 1L, any)
  if (any(floored)) {
    warning("scmix_polarization(): residual SD below the floor (", sd_floor,
            ") for: ", paste(fit$attr_names[floored], collapse = ", "),
            ". Their sign shares are not identified separately from the",
            " floor constant and are reported as NA; the conditional-mean",
            " sign is the defensible directional summary there.")
  }
  sd_folds <- pmax(sd_folds, sd_floor / sd_dx0)
  fold_resp <- pr$sc$fold_resp
  psi <- matrix(0, pr$N, pr$p)
  a_all <- matrix(0, pr$N, pr$p)
  for (i in seq_len(pr$N)) {
    s <- sd_folds[, fold_resp[i]]
    zsc <- pr$mu_resp[i, ] / s
    h <- stats::pnorm(zsc)
    a <- stats::dnorm(zsc) / s
    a_all[i, ] <- a
    psi[i, ] <- h + a * pr$C[i, ]
  }
  for (k in seq_len(pr$p)) {
    ak <- matrix(0, pr$N, pr$p); ak[, k] <- a_all[, k]
    psi[, k] <- psi[, k] - .scmix_A_adjust(pr, ak)
  }
  psi[, floored] <- NA_real_
  out <- .scmix_wrap(psi, fit$attr_names, "pi (share with beta_k > 0)", fit,
                     extra = list(sigma_k_by_fold = sd_folds,
                                  sigma_k_range = t(apply(sd_folds, 1L, range)),
                                  floored = fit$attr_names[floored]))
  oob <- !is.na(out$estimate) & (out$estimate < 0 | out$estimate > 1)
  if (any(oob)) {
    warning("scmix_polarization(): estimate outside [0, 1] for: ",
            paste(fit$attr_names[oob], collapse = ", "),
            ". The additive correction is unreliable for shares this",
            " close to the boundary (small T); interpret with caution.")
    out$extra$out_of_range <- fit$attr_names[oob]
  }
  out
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
scmix_counterfactual <- function(fit, contrast, n_bins = 40L, M = 2000L,
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
  a_all <- matrix(0, pr$N, pr$p)
  for (i in seq_len(pr$N)) {
    A <- fit$A_folds[[fold_resp[i]]]
    idx <- sum(cv * pr$mu_resp[i, ]) + as.numeric((t(cv) %*% A) %*% t(gh$U))
    pg <- sigm(idx)
    h <- sum(gh$w * pg)
    aZ <- sum(gh$w * pg * (1 - pg)) * cv
    a_all[i, ] <- aZ
    psi[i, 1L] <- h + sum(aZ * pr$C[i, ])
  }
  psi[, 1L] <- psi[, 1L] - .scmix_A_adjust(pr, a_all)
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

#' Truth-zero calibration for the estimated residual heterogeneity
#'
#' At small T the marginal likelihood can attribute part of the mean
#' network's misfit to the residual factor, so the estimated loading
#' carries an upward floor even when no residual heterogeneity exists.
#' This diagnostic measures that floor for the design at hand: it
#' simulates choice data from the fitted conditional means with the
#' loading matrix set to zero, refits `scmix()` on the simulated data
#' with the original settings, and reports the index-scale residual SD
#' the pipeline manufactures from nothing. Estimated heterogeneity that
#' does not clearly exceed this floor should not be interpreted.
#'
#' @param fit An `scmix` object.
#' @param R Number of truth-zero replications (default 2; each costs
#'   one full `scmix()` fit).
#' @param seed RNG seed.
#' @return A list with `floor_index_sd` (length-R vector of spurious
#'   index-scale SDs), `fitted_index_sd` (the fit's own per-fold
#'   index-scale SDs), and `ratio` (fitted mean over floor mean; values
#'   near 1 mean the fitted heterogeneity is indistinguishable from the
#'   small-T artifact).
#' @export
scmix_calibrate_zero <- function(fit, R = 2L, seed = 1L) {
  withr::local_preserve_seed()
  set.seed(seed)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  ridx <- as.integer(resp_f)
  first <- !duplicated(ridx)
  mu_resp <- fit$mu_hat[first, , drop = FALSE]
  n <- nrow(fit$deltaX)

  floor_sd <- numeric(R)
  for (r in seq_len(R)) {
    pr <- stats::plogis(rowSums(fit$deltaX * mu_resp[ridx, , drop = FALSE]))
    ysim <- as.numeric(stats::runif(n) < pr)
    dat <- data.frame(
      respondent = fit$respondent_id,
      task = stats::ave(seq_len(n), fit$respondent_id, FUN = seq_along),
      y = ysim)
    ## rebuild a long-format frame around the existing contrasts: reuse
    ## the internal training path directly instead of round-tripping
    ## through the formula interface
    gh <- fit$gh
    fold_id <- fit$fold_id
    A_zero_folds <- vector("list", fit$K)
    for (k in seq_len(fit$K)) {
      in_k <- fold_id != k
      fk <- .sc_train_mixed_one(
        deltaX = sweep(fit$deltaX[in_k, , drop = FALSE], 2L, fit$sd_dx, `/`),
        y = ysim[in_k],
        Z = fit$Z[in_k, , drop = FALSE],
        respondent_id = fit$respondent_id[in_k],
        gh = gh, hidden = fit$hidden,
        n_epochs = fit$n_epochs, weight_decay = fit$weight_decay_used,
        seed = if (is.null(fit$seed)) NULL else
          .sc_fold_seed(fit$seed + 1000L * r, k))
      A_zero_folds[[k]] <- fk$A / fit$sd_dx
    }
    idx_sd_k <- vapply(A_zero_folds, function(A)
      sqrt(mean((fit$deltaX %*% A)^2)), numeric(1L))
    floor_sd[r] <- mean(idx_sd_k)
  }
  fitted_sd <- vapply(fit$A_folds, function(A)
    sqrt(mean((fit$deltaX %*% A)^2)), numeric(1L))
  list(floor_index_sd = floor_sd,
       fitted_index_sd = fitted_sd,
       ratio = mean(fitted_sd) / mean(floor_sd))
}
