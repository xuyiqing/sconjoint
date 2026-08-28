## Bounded sign shares under a dispersion detection floor.
## Companion code to 2608_issues/Yiqing/share_bounds_memo_2026-08-26.tex.
## Prototype quality: functions take an assembled nested fit
## (scmix_nested_assembled) plus per-coordinate floors and produce
## worst-case share bounds with one-sided confidence bounds through the
## existing scmix_dml machinery. Source AFTER pkgload::load_all(<pkg root>).

## Loadings on the raw contrast scale. A is optimized on standardized
## contrasts; dividing rows by the per-fold DeltaX scale recovers raw units
## (same convention as scmix_calibrate_zero()).
sb_raw_A_folds <- function(assembled) {
  lapply(seq_len(assembled$K), function(k) {
    A <- as.matrix(assembled$A_folds[[k]])
    sc <- as.numeric(assembled$sd_dx_folds[[k]])
    A / sc
  })
}

sb_respondent_means <- function(assembled) {
  rid <- as.character(assembled$respondent_id)
  keep <- !duplicated(rid)
  list(m = as.matrix(assembled$mu_hat)[keep, , drop = FALSE],
       respondents = rid[keep])
}

## Per-coordinate fitted dispersion sqrt(c' A A' c), fold-averaged, raw scale.
sb_fitted_dispersion <- function(assembled) {
  A_raw <- sb_raw_A_folds(assembled)
  p <- nrow(A_raw[[1L]])
  draws <- vapply(A_raw, function(A) sqrt(rowSums(A^2)), numeric(p))
  ## vapply drops to a plain length-K vector when p == 1, and as.matrix()
  ## then makes it K x 1 -- rowMeans would average over coordinates instead
  ## of folds and return K dispersions. Never fires on native coordinates
  ## (p > 1); does fire on a single contrast. Fix the shape explicitly.
  dim(draws) <- c(p, length(A_raw))
  rowMeans(draws)
}

## Zero-heterogeneity calibration adapted to assembled fits: simulate
## outcomes from the fitted mean and position effect with A = 0, refit the
## selected learner on the retained folds' training sets, and record the
## apparent raw-scale dispersion |c' A~| each refit manufactures from noise.
sb_zero_floor <- function(assembled, hidden, weight_decay,
                          n_epochs = 4000L, learning_rate = 0.01,
                          mu_bound = 10, kappa_bound = 10,
                          R = 10L, folds_use = c(1L, 2L),
                          gamma = 0.05, seed = 20260826L) {
  stopifnot(inherits(assembled, "scmix_nested_assembled"))
  set.seed(seed)
  deltaX <- as.matrix(assembled$deltaX)
  n <- nrow(deltaX)
  p <- ncol(deltaX)
  fold_id <- assembled$fold_id
  if (length(fold_id) != n) stop("fold_id must be task-level.")
  mu_task <- as.matrix(assembled$mu_hat)
  kappa_task <- assembled$kappa_folds[fold_id]
  pr <- stats::plogis(kappa_task + rowSums(deltaX * mu_task))

  draws <- matrix(NA_real_, nrow = R * length(folds_use), ncol = p)
  row <- 0L
  for (r in seq_len(R)) {
    ysim <- as.numeric(stats::runif(n) < pr)
    for (k in folds_use) {
      in_k <- fold_id != k
      sd_dx_k <- as.numeric(assembled$sd_dx_folds[[k]])
      Z_train <- sconjoint:::.sc_apply_z_transform(
        as.matrix(assembled$Z)[in_k, , drop = FALSE],
        assembled$z_transform_folds[[k]])
      gh_k <- if (!is.null(assembled$integration_grids_folds)) {
        assembled$integration_grids_folds[[k]]
      } else assembled$gh
      fk <- sconjoint:::.sc_train_mixed_one(
        deltaX = sweep(deltaX[in_k, , drop = FALSE], 2L, sd_dx_k, `/`),
        y = ysim[in_k],
        Z = Z_train,
        respondent_id = assembled$respondent_id[in_k],
        gh = gh_k, hidden = hidden,
        n_epochs = n_epochs, learning_rate = learning_rate,
        weight_decay = weight_decay,
        early_stop = FALSE,
        mu_bound = mu_bound, kappa_bound = kappa_bound,
        seed = seed + 1000L * r + k)
      A_raw <- as.matrix(fk$A) / sd_dx_k
      row <- row + 1L
      draws[row, ] <- sqrt(rowSums(A_raw^2))
      message(sprintf("floor calibration: rep %d fold %d done", r, k))
    }
  }
  floors <- apply(draws, 2L, stats::quantile, probs = 1 - gamma,
                  names = FALSE, type = 1L)
  names(floors) <- assembled$attr_names
  list(floor = floors, draws = draws, R = R, folds_use = folds_use,
       gamma = gamma, n_epochs = n_epochs,
       analysis_signature = assembled$analysis_signature)
}

## Worst-case share bounds per coordinate. Orientation: by default toward
## the modal sign of the fitted means; pass `orient` (a named vector of
## one-step theta estimates from the application's 04 output) to orient by
## the CORRECTED mean instead --- under a collapsed plugin mean the plugin
## sign can be wrong, as Ballard-Rosa's revenue coordinate shows.
sb_bounds_table <- function(assembled, floors, grid_n = 60L, orient = NULL) {
  rm <- sb_respondent_means(assembled)
  fitted_s <- sb_fitted_dispersion(assembled)
  p <- ncol(rm$m)
  nm <- assembled$attr_names
  rows <- vector("list", p)
  for (j in seq_len(p)) {
    m <- rm$m[, j]
    modal_negative <- if (!is.null(orient) && nm[j] %in% names(orient)) {
      orient[[nm[j]]] < 0
    } else mean(m < 0) >= 0.5
    mo <- if (modal_negative) m else -m       # oriented: modal side is mo < 0
    sbar <- max(fitted_s[j], floors[j])
    eta <- function(s) mean(stats::pnorm(-mo / s))
    grid <- sbar * exp(seq(log(1e-4), 0, length.out = grid_n))
    eta0 <- mean(mo < 0) + 0.5 * mean(mo == 0)
    LG <- min(min(vapply(grid, eta, numeric(1L))), eta0)
    LC <- mean(ifelse(mo < 0, mo^2 / (sbar^2 + mo^2), 0))
    rows[[j]] <- data.frame(
      coordinate = nm[j],
      modal_side = if (modal_negative) "negative" else "positive",
      all_one_sign = all(mo < 0),
      mean_abs = mean(abs(m)), mean_oriented = mean(mo),
      fitted_s = fitted_s[j], floor = floors[j], s_bar = sbar,
      share_limit_s0 = eta0,
      lower_bound_gauss = LG,
      lower_bound_cantelli = LC)
  }
  do.call(rbind, rows)
}

## Typed plugin targets at FIXED s_bar for the one-step machinery, each a
## rowwise expectation, sigma-invariant, with analytic d_mu.
##
## One-sign coordinates get a single Gaussian target at s_bar (the minimum
## sits there; Proposition 3, one-sign case). Mixed-sign coordinates get
## the monotone bracketing envelope: one target per grid cell, evaluating
## negative-mean terms at the cell's right endpoint and positive-mean terms
## at the left, plus the boundary cell [0, s_1] with the trivial zero for
## positive terms; the confidence bound is the minimum of the cell LCBs
## (the cell containing the true s covers on its own). The envelope's
## per-respondent sign split makes the target value continuous but kinks
## its derivative at m = 0 --- a prototype-level compromise, noted in the
## memo. Cantelli targets need no envelope: the two-moment bound is
## per-respondent and involves no minimization over s.
sb_gauss_env_target <- function(co, s_neg, s_pos, label) {
  force(co); force(s_neg); force(s_pos); force(label)
  function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    N <- nrow(mu)
    m <- as.numeric(mu %*% co)
    neg <- m < 0
    value <- numeric(N)
    slope <- numeric(N)
    value[neg] <- stats::pnorm(-m[neg] / s_neg)
    slope[neg] <- -stats::dnorm(-m[neg] / s_neg) / s_neg
    if (is.finite(s_pos)) {
      value[!neg] <- stats::pnorm(-m[!neg] / s_pos)
      slope[!neg] <- -stats::dnorm(-m[!neg] / s_pos) / s_pos
    }                                  # s_pos = Inf encodes the trivial 0
    dm <- array(0, c(N, 1L, length(co)))
    for (i in seq_len(N)) dm[i, 1L, ] <- slope[i] * co
    list(target_type = "rowwise_expectation",
         value = matrix(value, ncol = 1L),
         d_mu = dm, d_kappa = matrix(0, N, 1L),
         labels = label, sigma_invariant = TRUE)
  }
}

sb_make_targets <- function(bounds_table, attr_names, env_cells = 8L) {
  targets <- list()
  for (j in seq_len(nrow(bounds_table))) {
    nm <- bounds_table$coordinate[j]
    sbar <- bounds_table$s_bar[j]
    sgn <- if (bounds_table$modal_side[j] == "negative") 1 else -1
    co <- sgn * as.numeric(attr_names == nm)
    if (isTRUE(bounds_table$all_one_sign[j])) {
      targets[[paste0("gauss_", nm)]] <-
        sb_gauss_env_target(co, sbar, sbar, paste0("gauss_", nm))
    } else {
      pts <- sbar * exp(seq(log(1 / 50), 0, length.out = env_cells + 1L))
      targets[[paste0("gaussenv_", nm, "_c0")]] <-
        sb_gauss_env_target(co, pts[1L], Inf,
                            paste0("gaussenv_", nm, "_c0"))
      for (g in seq_len(env_cells)) {
        targets[[paste0("gaussenv_", nm, "_c", g)]] <-
          sb_gauss_env_target(co, pts[g + 1L], pts[g],
                              paste0("gaussenv_", nm, "_c", g))
      }
    }
    local({
      co_l <- co; sbar_l <- sbar; nm_l <- nm
      targets[[paste0("cant_", nm_l)]] <<- function(mu, kappa, Sigma, Z,
                                                    respondent_id, fold,
                                                    attr_names) {
        N <- nrow(mu)
        m <- as.numeric(mu %*% co_l)
        neg <- m < 0
        value <- matrix(ifelse(neg, m^2 / (sbar_l^2 + m^2), 0), ncol = 1L)
        dm <- array(0, c(N, 1L, length(co_l)))
        slope <- ifelse(neg, 2 * m * sbar_l^2 / (sbar_l^2 + m^2)^2, 0)
        for (i in seq_len(N)) dm[i, 1L, ] <- slope[i] * co_l
        list(target_type = "rowwise_expectation", value = value,
             d_mu = dm, d_kappa = matrix(0, N, 1L),
             labels = paste0("cant_", nm_l), sigma_invariant = TRUE)
      }
    })
  }
  targets
}

## One-sided 95 percent lower confidence bounds via the existing dml
## machinery (diagnostic wherever the application's Riesz gates are).
sb_confidence_bounds <- function(assembled, bounds_table, inf_cfg, seed) {
  targets <- sb_make_targets(bounds_table, assembled$attr_names)
  out <- scmix_dml(
    fit = assembled,
    targets = character(),
    plugin_targets = targets,
    mu_basis = NULL,
    riesz_validation_fraction = inf_cfg$riesz_validation_fraction,
    riesz_equation_tolerance = inf_cfg$riesz_equation_tolerance,
    ridge_sensitivity_tolerance = inf_cfg$ridge_sensitivity_tolerance,
    active_eigenvalue_min = inf_cfg$active_eigenvalue_min,
    information_eigenvalue_min = inf_cfg$information_eigenvalue_min,
    rank_tolerance = inf_cfg$rank_tolerance,
    multiplier_draws = 0L,
    level = 0.95, seed = seed)
  z <- stats::qnorm(0.95)
  est <- out$estimate
  se <- out$diagnostic_se
  gauss_onestep <- gauss_lcb <- numeric(nrow(bounds_table))
  for (j in seq_len(nrow(bounds_table))) {
    nm <- bounds_table$coordinate[j]
    if (isTRUE(bounds_table$all_one_sign[j])) {
      lab <- paste0("gauss_", nm)
      gauss_onestep[j] <- est[[lab]]
      gauss_lcb[j] <- est[[lab]] - z * se[[lab]]
    } else {
      labs <- grep(paste0("^gaussenv_", nm, "_c"), names(est), value = TRUE)
      lcbs <- as.numeric(est[labs]) - z * as.numeric(se[labs])
      gauss_onestep[j] <- min(as.numeric(est[labs]))
      gauss_lcb[j] <- min(lcbs)
    }
  }
  lab_c <- paste0("cant_", bounds_table$coordinate)
  bounds_table$gauss_onestep <- gauss_onestep
  bounds_table$gauss_lcb95 <- pmax(0, gauss_lcb)
  bounds_table$cant_onestep <- as.numeric(est[lab_c])
  bounds_table$cant_lcb95 <- pmax(0,
    as.numeric(est[lab_c]) - z * as.numeric(se[lab_c]))
  bounds_table$dml_status <- out$status
  attr(bounds_table, "dml") <- out
  bounds_table
}
