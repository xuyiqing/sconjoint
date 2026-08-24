## Legacy exploratory score corrections for early scmix fits.
##
## IMPORTANT: this file predates the structural-sieve Riesz construction in
## paperps.tex.  It is retained for backwards compatibility and diagnostics,
## but it is not the regular inference procedure justified by that paper.
## The paper-aligned public entry point is scmix_dml() in
## R/paperps-inference.R.  In particular, the binned design simulation below
## must not be used when claiming paperps inference.
##
## For a population functional
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

#' Per-bin sensitivity blocks and averaged effective loading information
#'
#' `B(Z) = I_mumu(Z)^{-1} I_muA(Z)` measures how a loading error
#' transmits into the pseudo-true location; the effective loading
#' information averages `I_AA(Z) - I_Amu(Z) I_mumu(Z)^{-1} I_muA(Z)`
#' respondent by respondent.  Shared by [.scmix_prep()] and
#' [scmix_design_check()] (which previously duplicated the loop).
#' @keywords internal
#' @noRd
.scmix_eff_info_bar <- function(info, N) {
  B_bin <- lapply(seq_along(info$I_inv), function(b)
    info$I_inv[[b]] %*% info$I_muA[[b]])
  pq <- ncol(info$I_AA[[1L]])
  I_AAeff_bar <- matrix(0, pq, pq)
  for (i in seq_len(N)) {
    b <- info$bin_of[i]
    I_AAeff_bar <- I_AAeff_bar +
      (info$I_AA[[b]] - crossprod(info$I_muA[[b]], B_bin[[b]])) / N
  }
  list(I_AAeff_bar = I_AAeff_bar, B_bin = B_bin)
}

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
  ## effective loading score S_Aeff,i = S_A,i - I_Amu I_mumu^{-1} S_mu,i
  S_Aeff <- matrix(0, N, pq)
  eff <- .scmix_eff_info_bar(info, N)
  B_bin <- eff$B_bin
  I_AAeff_bar <- eff$I_AAeff_bar
  for (i in seq_len(N)) {
    b <- info$bin_of[i]
    C[i, ] <- info$I_inv[[b]] %*% sc$S[i, ]
    S_Aeff[i, ] <- sc$S_A[i, ] - crossprod(info$I_muA[[b]], C[i, ])
  }
  ## Truncating pseudo-inverse of the effective loading information,
  ## compared on the standardized scale (mixed attribute units must not
  ## decide which directions count as identified).  Directions with
  ## near-zero effective information correspond to unidentified loading
  ## components (for example a collapsed fold loading); their influence
  ## is projected OUT rather than amplified --- a ridge here turns a
  ## rank deficiency into a silent explosion of the correction.
  sd_dxA <- .scmix_sd_dx(fit)
  q_A <- pq / p
  D_A <- diag(rep(1 / sd_dxA, q_A), pq)
  I_std_A <- D_A %*% I_AAeff_bar %*% D_A
  eA <- eigen(I_std_A, symmetric = TRUE)
  keep_A <- eA$values > 1e-6 * max(eA$values, 1e-12)
  if (any(!keep_A)) {
    warning(".scmix_prep(): ", sum(!keep_A), " loading direction(s) have",
            " near-zero effective information (unidentified, e.g. a",
            " collapsed fold loading); their influence is projected out",
            " of the correction.")
  }
  inv_vals <- ifelse(keep_A, 1 / eA$values, 0)
  I_AAeff_inv <- D_A %*% (eA$vectors %*% diag(inv_vals, pq) %*%
                            t(eA$vectors)) %*% D_A
  ## influence of the loading estimate per respondent
  IF_A <- S_Aeff %*% I_AAeff_inv
  list(sc = sc, info = info, mu_resp = mu_resp, C = C, N = N, p = p,
       pq = pq, B_bin = B_bin, IF_A = IF_A,
       I_AAeff_inv = I_AAeff_inv, sd_dxA = sd_dxA,
       eigA = list(vectors = eA$vectors, values = eA$values,
                   keep = keep_A))
}

#' Loading-influence adjustment for an estimand with gradient rows a_i
#'
#' For theta_H with mu-gradient a(Z_i), the sensitivity of the corrected
#' plug-in to a loading error is Gamma_A = mean_i a_i' B(Z_i) with
#' B(Z) = I_mumu(Z)^{-1} I_muA(Z); the missing influence term is
#' Gamma_A applied to the loading influence IF_A,i.  `a_rows` is N x p.
#' Returns the N-vector Gamma_A IF_A term, which every caller
#' SUBTRACTS from psi (sign validated by the loading-perturbation
#' transmission check in the test suite and by the 2026-08-11 audit's
#' numerical flatness check: subtracting cancels the transmission,
#' adding doubles it).
#' @keywords internal
#' @noRd
.scmix_A_adjust <- function(pr, a_rows, dA_rows = NULL) {
  N <- pr$N
  GammaA <- matrix(0, 1L, pr$pq)
  for (i in seq_len(N)) {
    GammaA <- GammaA + (a_rows[i, , drop = FALSE] %*%
                          pr$B_bin[[pr$info$bin_of[i]]]) / N
  }
  ## the complete first-order term is (Gamma_A - D_A) IF_A, where D_A is
  ## the mean DIRECT gradient of H in vec(A); estimands whose H does not
  ## involve the loading (theta) pass dA_rows = NULL
  if (!is.null(dA_rows)) {
    GammaA <- GammaA - matrix(colMeans(dA_rows), nrow = 1L)
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
  out$paperps_regular_inference <- FALSE
  out$inference_scope <- paste(
    "Legacy binned-information approximation; not the paperps structural",
    "Riesz/DML procedure. Use scmix_dml() for paper-aligned inference."
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
  stopifnot(inherits(fit, "scmix"))
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
  stopifnot(inherits(fit, "scmix"))
  fit <- .scmix_canon(fit)
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
  sd_dx0 <- .scmix_sd_dx(fit)
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
  q <- ncol(fit$A_folds[[1L]])
  for (k in seq_len(pr$p)) {
    ak <- matrix(0, pr$N, pr$p); ak[, k] <- a_all[, k]
    ## direct channel: dH/dA_{kr} = -phi(z) z A_{kr} / sigma_k^2
    dA <- matrix(0, pr$N, pr$p * q)
    for (i in seq_len(pr$N)) {
      Af <- fit$A_folds[[fold_resp[i]]]
      s_i <- sd_folds[k, fold_resp[i]]
      z_i <- pr$mu_resp[i, k] / s_i
      dA[i, (seq_len(q) - 1L) * pr$p + k] <-
        -stats::dnorm(z_i) * z_i * Af[k, ] / s_i^2
    }
    psi[, k] <- psi[, k] - .scmix_A_adjust(pr, ak, dA_rows = dA)
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
#' @param contrast One contrast or many: a numeric vector of length p
#'   (attribute-dummy scale), a named subset expanded against
#'   `fit$attr_names`, a numeric matrix with one contrast per row
#'   (rownames become labels), or a list of such vectors.  All
#'   contrasts in a call share one information simulation -- pass a
#'   sweep as a batch rather than looping.  Each contrast carries its
#'   own raw-share benchmark (`$extra$raw`, one row per contrast).
#' @export
scmix_counterfactual <- function(fit, contrast, n_bins = 40L, M = 2000L,
                                 seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  pc <- tryCatch(.scmix_parse_contrasts(fit, contrast),
                 error = function(e) {
                   stop("scmix_counterfactual(): ", conditionMessage(e),
                        call. = FALSE)
                 })
  D <- pc$D
  J <- nrow(D)
  single <- J == 1L

  fit <- .scmix_canon(fit)
  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  gh <- fit$gh
  sigm <- function(x) 1 / (1 + exp(-x))
  fold_resp <- pr$sc$fold_resp
  qq <- ncol(fit$A_folds[[1L]])

  psi <- matrix(0, pr$N, J)
  raw_rows <- vector("list", J)
  for (j in seq_len(J)) {
    cv <- D[j, ]
    a_all <- matrix(0, pr$N, pr$p)
    dA_all <- matrix(0, pr$N, pr$p * qq)
    for (i in seq_len(pr$N)) {
      A <- fit$A_folds[[fold_resp[i]]]
      idx <- sum(cv * pr$mu_resp[i, ]) + as.numeric((t(cv) %*% A) %*% t(gh$U))
      pg <- sigm(idx)
      h <- sum(gh$w * pg)
      gprime <- gh$w * pg * (1 - pg)
      aZ <- sum(gprime) * cv
      a_all[i, ] <- aZ
      for (r in seq_len(qq)) {
        dA_all[i, (r - 1L) * pr$p + seq_len(pr$p)] <-
          sum(gprime * gh$U[, r]) * cv
      }
      psi[i, j] <- h + sum(aZ * pr$C[i, ])
    }
    psi[, j] <- psi[, j] - .scmix_A_adjust(pr, a_all, dA_rows = dA_all)
    raw_rows[[j]] <- .sc_raw_share(fit$deltaX, fit$y, fit$respondent_id, cv)
  }

  labels <- if (single) "V(c)" else pc$labels
  extra <- if (single) {
    list(contrast = stats::setNames(D[1L, ], fit$attr_names))
  } else {
    list(contrasts = D, contrast_labels = pc$labels)
  }
  out <- .scmix_wrap(psi, labels, "eta-integrated counterfactual share", fit,
                     extra = extra)
  if (single) {
    ## backward-compatible flat raw-share fields for one contrast
    out$extra <- c(out$extra, raw_rows[[1L]])
  } else {
    out$extra$raw <- data.frame(
      label = pc$labels,
      raw_share = vapply(raw_rows, `[[`, numeric(1L), "raw_share"),
      raw_share_se = vapply(raw_rows, `[[`, numeric(1L), "raw_share_se"),
      raw_n_tasks = vapply(raw_rows, `[[`, integer(1L), "raw_n_tasks"),
      raw_n_respondents = vapply(raw_rows, `[[`, integer(1L),
                                 "raw_n_respondents"),
      stringsAsFactors = FALSE)
  }
  for (j in seq_len(J)) {
    raw <- raw_rows[[j]]
    if (!is.na(raw$raw_share)) {
      gap <- abs(out$estimate[j] - raw$raw_share)
      gap_se <- sqrt(out$se[j]^2 + raw$raw_share_se^2)
      if (is.finite(gap_se) && gap > 2 * gap_se) {
        warning("scmix_counterfactual(): the model-based share differs from ",
                "the raw design-based share by more than 2 SEs (",
                sprintf("%.3f vs %.3f", out$estimate[j], raw$raw_share),
                if (single) "" else sprintf(" for %s", labels[j]),
                "). Treat as a specification warning for this contrast.",
                call. = FALSE)
      }
    }
  }
  out
}

#' Raw design-based share for an in-design contrast
#'
#' Proposition 1(b) of the estimand memo: when the design assigns
#' contrast `cv` (or its negation) with positive probability, the raw
#' share of choices among matching tasks is a model-free estimator of
#' `V(cv)`, unbiased conditional on the realized design. Tasks whose
#' contrast equals `-cv` contribute `1 - y`. Respondents are averaged
#' FIRST (task-pooling weights respondents by task count, which biases
#' the share when `T_i` correlates with `beta_i`); the SE is the
#' between-respondent empirical standard error.  Returns `NA` when no
#' task matches (off-design contrast).
#' @keywords internal
#' @noRd
.sc_raw_share <- function(deltaX, y, respondent_id, cv, tol = 1e-8) {
  d_pos <- rowSums(abs(sweep(deltaX, 2L, cv, `-`))) < tol
  d_neg <- rowSums(abs(sweep(deltaX, 2L, -cv, `-`))) < tol
  hit <- d_pos | d_neg
  if (!any(hit)) {
    return(list(raw_share = NA_real_, raw_share_se = NA_real_,
                raw_n_tasks = 0L, raw_n_respondents = 0L))
  }
  contrib <- ifelse(d_pos[hit], y[hit], 1 - y[hit])
  resp <- respondent_id[hit]
  m_i <- tapply(contrib, resp, mean)
  list(raw_share = mean(m_i),
       raw_share_se = if (length(m_i) > 1L)
         stats::sd(m_i) / sqrt(length(m_i)) else NA_real_,
       raw_n_tasks = sum(hit),
       raw_n_respondents = length(m_i))
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
#' network's misfit to the residual factor. This legacy exploratory diagnostic
#' asks whether the fitted low-rank covariance carries an upward floor even
#' when no residual heterogeneity exists.
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
#'   small-T artifact). Store the result on the fit,
#'   `fit$zero_floor <- scmix_calibrate_zero(fit)`. The ratio is descriptive;
#'   it is not a paperps reporting gate and does
#'   not replace rank, information, shape-sensitivity, or numerical checks.
#' @export
scmix_calibrate_zero <- function(fit, R = 2L, seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  if (isTRUE(fit$q == 0L)) {
    return(list(floor_index_sd = 0, fitted_index_sd = 0, ratio = NA_real_,
                applicable = FALSE,
                note = "q = 0 was fixed, so residual heterogeneity is absent by specification."))
  }
  withr::local_preserve_seed()
  set.seed(seed)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  ridx <- as.integer(resp_f)
  first <- !duplicated(ridx)
  mu_resp <- fit$mu_hat[first, , drop = FALSE]
  n <- nrow(fit$deltaX)

  floor_sd <- numeric(R)
  for (r in seq_len(R)) {
    kappa_task <- if (is.null(fit$kappa_folds)) 0 else
      fit$kappa_folds[fit$fold_id]
    pr <- stats::plogis(kappa_task +
                          rowSums(fit$deltaX * mu_resp[ridx, , drop = FALSE]))
    ysim <- as.numeric(stats::runif(n) < pr)
    ## reuse the internal training path directly on the existing
    ## contrasts instead of round-tripping through the formula interface
    gh <- fit$gh
    fold_id <- fit$fold_id
    A_zero_folds <- vector("list", fit$K)
    for (k in seq_len(fit$K)) {
      in_k <- fold_id != k
      sd_dx_k <- if (!is.null(fit$sd_dx_folds)) {
        fit$sd_dx_folds[[k]]
      } else fit$sd_dx
      if (is.null(sd_dx_k) || length(sd_dx_k) != ncol(fit$deltaX) ||
          any(!is.finite(sd_dx_k)) || any(sd_dx_k <= 0)) {
        stop("scmix_calibrate_zero(): invalid fold-specific DeltaX scale in fold ",
             k, ".", call. = FALSE)
      }
      Z_train <- fit$Z[in_k, , drop = FALSE]
      if (!is.null(fit$z_transform_folds)) {
        Z_train <- .sc_apply_z_transform(
          Z_train, fit$z_transform_folds[[k]])
      }
      fk <- .sc_train_mixed_one(
        deltaX = sweep(fit$deltaX[in_k, , drop = FALSE], 2L, sd_dx_k, `/`),
        y = ysim[in_k],
        Z = Z_train,
        respondent_id = fit$respondent_id[in_k],
        gh = gh, hidden = fit$hidden,
        n_epochs = fit$n_epochs,
        learning_rate = if (is.null(fit$learning_rate)) 0.01 else
          fit$learning_rate,
        weight_decay = fit$weight_decay_used,
        early_stop = FALSE,
        mu_bound = if (!is.null(fit$bounds$mu_internal)) {
          fit$bounds$mu_internal
        } else if (!is.null(fit$bounds$mu)) fit$bounds$mu else 10,
        kappa_bound = if (is.null(fit$bounds$kappa)) 10 else fit$bounds$kappa,
        seed = if (is.null(fit$seed)) NULL else
          .sc_fold_seed(fit$seed + 1000L * r, k))
      A_zero_folds[[k]] <- fk$A / sd_dx_k
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
