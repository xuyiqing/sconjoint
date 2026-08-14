## Model-implied attribute-importance decomposition for scmix fits (P9).
##
## The two-stage sc_importance() builds per-respondent utility-variance
## contributions from MAP point estimates -- exactly the respondent-level
## object the one-estimator revision retires.  The model-implied
## decomposition works from (mu, A) instead: for attribute block g with
## level-weighting matrix S_g, the population numerator is
##
##   N_g = E_Z[ mu_g(Z)' S_g mu_g(Z) ] + tr(S_g (A A')_{gg}),
##
## the model-implied mean over respondents of the within-respondent
## attribute-utility variance -- a second-moment functional of beta_i,
## so the residual factor contributes the trace term the point-estimate
## version has no way to see.  Shares are ratio-of-means,
## share_g = N_g / sum_h N_h, with the simplex-Jacobian influence.

#' Level-weighting matrices S_g per attribute
#'
#' Encodes the four weightings of [sc_importance()] as one quadratic
#' form per attribute block: `"uniform"` is `I/L - 11'/L^2` over the
#' dummy block (L levels including the reference); `"levels"` is the
#' scalar variance of the supplied design levels for single-column
#' continuous attributes (others fall back to uniform); `"design_variance"`
#' is the full block covariance of the realized contrasts (the
#' covariance-aware form the orthogonal two-stage route uses -- not the
#' diagonal of the plug-in); `"empirical"` is
#' `diag(w) - w w'` over the empirical level frequencies.
#' @keywords internal
#' @noRd
.sc_importance_S_blocks <- function(attr_map, deltaX, design, levels, fn) {
  attrs <- names(attr_map)
  if (identical(design, "levels")) {
    if (!is.null(levels)) {
      if (is.null(names(levels)) || !all(nzchar(names(levels))))
        stop(fn, "(design = \"levels\"): `levels` must be a named list.",
             call. = FALSE)
      bad <- setdiff(names(levels), attrs)
      if (length(bad))
        stop(fn, "(): unknown attribute(s) in `levels`: ",
             paste(bad, collapse = ", "), call. = FALSE)
      for (nm in names(levels)) {
        if (length(attr_map[[nm]]) != 1L)
          stop(fn, "(): `levels` applies to single-column ",
               "(continuous) attributes; '", nm, "' has ",
               length(attr_map[[nm]]), " columns.", call. = FALSE)
        if (!is.numeric(levels[[nm]]) || length(levels[[nm]]) < 2L)
          stop(fn, "(): `levels$", nm,
               "` must be a numeric vector of at least 2 design levels.",
               call. = FALSE)
      }
    }
  } else if (!is.null(levels)) {
    warning(fn, "(): `levels` is ignored unless design = \"levels\".",
            call. = FALSE)
  }

  out <- lapply(attrs, function(a) {
    cols <- attr_map[[a]]
    n_c <- length(cols)
    if (identical(design, "design_variance")) {
      stats::cov(deltaX[, cols, drop = FALSE])
    } else if (identical(design, "levels") && a %in% names(levels)) {
      l <- levels[[a]]
      matrix(mean((l - mean(l))^2), 1L, 1L)
    } else if (identical(design, "empirical")) {
      w <- colMeans(abs(deltaX[, cols, drop = FALSE]))
      tot <- max(1 - sum(w), 0) + sum(w)
      if (tot == 0) {
        matrix(0, n_c, n_c)
      } else {
        w_norm <- w / tot
        diag(w_norm, n_c) - w_norm %o% w_norm
      }
    } else {
      ## uniform (also the "levels" fallback for factor attributes)
      L <- n_c + 1L
      diag(1 / L, n_c) - matrix(1 / L^2, n_c, n_c)
    }
  })
  stats::setNames(out, attrs)
}

#' Simplex share influence rows from numerator signals
#'
#' Maps an N x K matrix of numerator influence signals to the N x K
#' matrix of share influence rows `s' + (psiN_i - N_hat)' J'` with
#' `J = (I - 1 s') / D`; column means equal the shares and the
#' column-wise SEs equal the simplex-Jacobian standard errors.  Warns
#' (never clamps) when a numerator is non-positive or a share leaves
#' [0, 1].
#' @keywords internal
#' @noRd
.sc_simplex_share_psi <- function(psiN, labels, fn) {
  N_hat <- colMeans(psiN)
  if (any(N_hat <= 0)) {
    warning(fn, "(): non-positive importance numerator for: ",
            paste(labels[N_hat <= 0], collapse = ", "),
            ". The corrected numerator signal overshoots zero there;",
            " shares are reported uncorrected of that and can leave",
            " [0, 1].", call. = FALSE)
  }
  D_hat <- sum(N_hat)
  share <- N_hat / D_hat
  K <- length(N_hat)
  Jmat <- (diag(K) - matrix(share, K, K, byrow = TRUE)) / D_hat
  centered <- sweep(psiN, 2L, N_hat, `-`)
  psi_share <- matrix(share, nrow(psiN), K, byrow = TRUE) +
    centered %*% t(Jmat)
  if (any(share < 0 | share > 1)) {
    warning(fn, "(): importance share outside [0, 1] for: ",
            paste(labels[share < 0 | share > 1], collapse = ", "),
            ". Interpret with caution.", call. = FALSE)
  }
  list(psi_share = psi_share, share = share, N_hat = N_hat)
}

#' Model-implied attribute-importance shares from an scmix fit
#'
#' Decomposes utility variance by attribute from the fitted `(mu, A)`
#' rather than from respondent-level point estimates: the numerator for
#' attribute g is `E_Z[mu_g(Z)' S_g mu_g(Z)] + tr(S_g (AA')_{gg})` --
#' the model-implied average of the within-respondent attribute-utility
#' variance, whose residual-factor term a point-estimate decomposition
#' cannot see -- and the share is the ratio-of-means normalization
#' `N_g / sum_h N_h` with simplex-Jacobian influence SEs.  `$extra`
#' reports the `between_Z` (moderator-explained) and `residual`
#' (factor) components per attribute so applications can state which
#' totals they quote.
#'
#' Two named deviations from [sc_importance()]: shares are
#' ratio-of-means (one normalization of population numerators), not the
#' plug-in's per-respondent share-then-average; and the default
#' weighting is `"uniform"` (the paper's reported convention for factor
#' designs; use `"levels"` with bracket midpoints for continuous
#' attributes, as in the tax application).
#'
#' @inheritParams scmix_theta
#' @param design Level-weighting for the variance decomposition:
#'   `"uniform"` (default; uniform draw over levels with the reference
#'   at zero), `"levels"` (explicit numeric design levels for
#'   single-column continuous attributes, others uniform),
#'   `"design_variance"` (full block covariance of the realized
#'   contrasts), or `"empirical"` (empirical level frequencies).
#' @param levels Named list of numeric design levels, used only with
#'   `design = "levels"`.
#' @param by Optional respondent grouping as in [scmix_average()];
#'   group shares renormalize within group (their own simplex
#'   Jacobian), so a group's shares always sum to one.
#' @return An `scmix_quantity` with one entry per attribute (times
#'   groups when `by` is given).  `$extra` carries the numerators, the
#'   between-Z / residual split, the per-fold residual-term range, the
#'   design tag, and the S blocks.
#' @export
scmix_importance <- function(fit,
                             design = c("uniform", "levels",
                                        "design_variance", "empirical"),
                             levels = NULL, by = NULL,
                             n_bins = 40L, M = 2000L, seed = 1L) {
  stopifnot(inherits(fit, "scmix"))
  design <- match.arg(design)
  fit <- .scmix_canon(fit)
  attr_map <- fit$attr_map
  if (is.null(attr_map)) {
    stop("scmix_importance(): fit$attr_map is missing.", call. = FALSE)
  }
  attrs <- names(attr_map)
  Kattr <- length(attrs)
  S_blocks <- .sc_importance_S_blocks(attr_map, fit$deltaX, design, levels,
                                      fn = "scmix_importance")

  pr <- .scmix_prep(fit, n_bins = n_bins, M = M, seed = seed)
  fold_resp <- pr$sc$fold_resp
  Kf <- length(fit$A_folds)
  q <- ncol(fit$A_folds[[1L]])

  ## fold-specific residual terms tr(S_g (A_f A_f')_{gg}) and direct
  ## channels 2 (S_g A_f[g, ])
  resid_by_fold <- matrix(0, Kf, Kattr)
  dA_by_fold <- vector("list", Kf)
  for (f in seq_len(Kf)) {
    A_f <- fit$A_folds[[f]]
    dA_f <- matrix(0, Kattr, pr$pq)
    for (a in seq_len(Kattr)) {
      cols <- attr_map[[a]]
      S_g <- S_blocks[[a]]
      A_g <- A_f[cols, , drop = FALSE]
      resid_by_fold[f, a] <- sum(S_g * tcrossprod(A_g))
      SA <- 2 * (S_g %*% A_g)                       # |g| x q
      for (r in seq_len(q)) {
        dA_f[a, (r - 1L) * pr$p + cols] <- SA[, r]
      }
    }
    dA_by_fold[[f]] <- dA_f
  }

  psiN <- matrix(0, pr$N, Kattr)
  between_Z <- numeric(Kattr)
  for (a in seq_len(Kattr)) {
    cols <- attr_map[[a]]
    S_g <- S_blocks[[a]]
    mu_g <- pr$mu_resp[, cols, drop = FALSE]
    h_mu <- rowSums((mu_g %*% S_g) * mu_g)
    h <- h_mu + resid_by_fold[fold_resp, a]
    between_Z[a] <- mean(h_mu)
    a_rows <- matrix(0, pr$N, pr$p)
    a_rows[, cols] <- 2 * (mu_g %*% S_g)
    dA_rows <- do.call(rbind,
      lapply(fold_resp, function(f) dA_by_fold[[f]][a, , drop = FALSE]))
    psiN[, a] <- h + rowSums(a_rows * pr$C) -
      .scmix_A_adjust(pr, a_rows, dA_rows = dA_rows)
  }

  extra <- list(design = design, S_blocks = S_blocks,
                between_Z = stats::setNames(between_Z, attrs),
                ## respondent-weighted so between_Z + residual equals the
                ## uncorrected numerator mean under unequal fold sizes
                residual = stats::setNames(
                  colMeans(resid_by_fold[fold_resp, , drop = FALSE]), attrs),
                residual_by_fold = resid_by_fold,
                numerator_signals = psiN)

  if (is.null(by)) {
    sh <- .sc_simplex_share_psi(psiN, attrs, fn = "scmix_importance")
    extra$numerators <- stats::setNames(sh$N_hat, attrs)
    .scmix_wrap(sh$psi_share, attrs,
                sprintf("attribute importance shares (model-implied, %s)",
                        design),
                fit, extra = extra)
  } else {
    g <- .scmix_resolve_by(fit, by, fn = "scmix_importance")
    ## each group gets its own normalization: build a psi matrix whose
    ## rows hold the OWN group's share-influence rows, so the shared
    ## group-mean wrapper returns exactly the per-group shares and SEs
    psi_sub <- matrix(NA_real_, pr$N, Kattr)
    for (lev in base::levels(g)) {
      rows <- which(g == lev)
      if (length(rows) < 2L) {
        stop("scmix_importance(): group '", lev,
             "' has fewer than 2 respondents.", call. = FALSE)
      }
      sh_g <- .sc_simplex_share_psi(psiN[rows, , drop = FALSE],
                                    paste(lev, attrs, sep = ": "),
                                    fn = "scmix_importance")
      psi_sub[rows, ] <- sh_g$psi_share
    }
    .scmix_wrap_by(psi_sub, g, col_labels = attrs,
                   quantity = sprintf(
                     "attribute importance shares by subgroup (%s)", design),
                   fit = fit, fn = "scmix_importance", extra = extra)
  }
}
