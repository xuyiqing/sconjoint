## Bounded sign shares under a dispersion detection floor.
## Companion code to 2608_issues/Yiqing/share_bounds_memo_2026-08-26.tex.
##
## STATUS (2026-08-28, after the estimand/bounds audit). Everything this
## file computes is a CONDITIONAL SENSITIVITY CALCULATION, not a
## confidence bound. The Gaussian and Cantelli algebra is correct GIVEN a
## genuine upper bound s_bar on the true directional dispersion. The
## calibration that currently supplies s_bar (`sb_zero_floor()`) is a
## zero-heterogeneity DETECTION THRESHOLD: it answers "how much apparent
## dispersion does a refit manufacture from noise when the truth is
## zero?", which is not an upper confidence limit for positive true
## dispersion. Every table this file returns therefore carries
## `ceiling_source`, `ceiling_status`, and the literal maintained
## condition, and every number is released only through
## `bound_release`, which fails closed.
##
## Prototype quality: functions take an assembled nested fit
## (scmix_nested_assembled) plus per-coordinate ceilings and produce
## worst-case share bounds with one-step quantities through the existing
## scmix_dml machinery. Source AFTER pkgload::load_all(<pkg root>).

## The literal maintained condition, stamped into every artifact.
SB_MAINTAINED_CONDITION <- paste0(
  "s_true <= s_bar. The ceiling s_bar is NOT established by the data: it ",
  "comes from a zero-heterogeneity detection calibration, which is a null ",
  "critical value, not an upper confidence limit for positive true ",
  "directional dispersion. All bounds below are conditional sensitivity ",
  "calculations under this maintained condition.")

## The inferential population the one-step standard errors actually
## describe. scmix_dml() carries the direct empirical-P_Z influence term,
## which is Avi's superpopulation target; the bound memo's earlier
## finite-population conditional-Z description did not match the code.
SB_INFERENCE_POPULATION <- paste0(
  "superpopulation (scmix_dml retains the direct empirical-P_Z influence ",
  "term); NOT the finite realized-N target conditional on observed Z")

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

## ---------------------------------------------------------------------
## Matched calibration statistic (audit finding P2)
## ---------------------------------------------------------------------
##
## The OBSERVED dispersion is a fold AVERAGE (sb_fitted_dispersion above
## averages sqrt(rowSums(A_raw^2)) over folds). The calibration must use
## the SAME functional of each replication, so a replication's fold-level
## draws are averaged FIRST and the quantile is taken across replications.
## Pooling fold-level draws and quantiling the pool -- the pre-audit
## behaviour -- compares a fold average with a fold-level quantile, which
## is a different statistic and can sit on either side of the matched one.
##
## `sb_calibration_reps()` reshapes a calibration's raw draws (one row per
## (replication, fold), in the order sb_zero_floor() writes them) into an
## R x p matrix of per-replication fold averages. It works on calibration
## objects saved before this change, which carry `R` and `folds_use` but
## no explicit replication index.
sb_calibration_reps <- function(cal) {
  draws <- as.matrix(cal$draws)
  rid <- cal$rep_id
  if (is.null(rid)) {
    n_fold <- length(cal$folds_use)
    R <- cal$R
    if (is.null(n_fold) || is.null(R) || nrow(draws) != R * n_fold) {
      stop("Cannot reconstruct the replication index for this calibration: ",
           "nrow(draws) = ", nrow(draws), " but R * length(folds_use) = ",
           if (is.null(R)) "NA" else R, " * ",
           if (is.null(n_fold)) "NA" else n_fold, ".", call. = FALSE)
    }
    rid <- rep(seq_len(R), each = n_fold)
  }
  out <- t(vapply(split(seq_len(nrow(draws)), rid), function(ii)
    colMeans(draws[ii, , drop = FALSE]), numeric(ncol(draws))))
  dim(out) <- c(length(unique(rid)), ncol(draws))
  colnames(out) <- colnames(draws)
  out
}

## The matched ceiling: quantile ACROSS replications of the within-
## replication fold average. `gamma` defaults to the calibration's own.
sb_matched_floor <- function(cal, gamma = NULL, attr_names = NULL) {
  if (is.null(gamma)) gamma <- cal$gamma
  if (is.null(gamma)) stop("A calibration tail level `gamma` is required.",
                           call. = FALSE)
  reps <- sb_calibration_reps(cal)
  out <- apply(reps, 2L, stats::quantile, probs = 1 - gamma, names = FALSE,
               type = 1L)
  nm <- attr_names %||% cal$attr_names %||% colnames(cal$draws)
  if (!is.null(nm) && length(nm) == length(out)) names(out) <- nm
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## Zero-heterogeneity calibration adapted to assembled fits: simulate
## outcomes from the fitted mean and position effect with A = 0, refit the
## selected learner on the retained folds' training sets, and record the
## apparent raw-scale dispersion |c' A~| each refit manufactures from noise.
##
## This is a NULL DETECTION THRESHOLD, not an upper confidence limit. The
## returned object says so in `ceiling_status`; see SB_MAINTAINED_CONDITION.
## `keep_loadings = TRUE` additionally retains the SIGNED raw-scale
## loading matrix A~ from every (replication, fold) refit. The norms alone
## cannot reconstruct ||A~' c|| for a composite contrast, because that
## depends on the signs (for q > 1, the directions) of A~'s rows --- which
## is why the composite floor had to fall back on a triangle envelope.
## With the matrices retained the exact matched composite ceiling is
## computable; see `sb_contrast_floor_matched()`.
sb_zero_floor <- function(assembled, hidden, weight_decay,
                          n_epochs = 4000L, learning_rate = 0.01,
                          mu_bound = 10, kappa_bound = 10,
                          R = 10L, folds_use = c(1L, 2L),
                          gamma = 0.05, seed = 20260826L,
                          keep_loadings = FALSE) {
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
  rep_id <- integer(R * length(folds_use))
  fold_of_draw <- integer(R * length(folds_use))
  A_raw_draws <- if (isTRUE(keep_loadings))
    vector("list", R * length(folds_use)) else NULL
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
      if (isTRUE(keep_loadings)) {
        dimnames(A_raw) <- list(assembled$attr_names, NULL)
        A_raw_draws[[row]] <- A_raw
      }
      rep_id[row] <- r
      fold_of_draw[row] <- k
      message(sprintf("floor calibration: rep %d fold %d done", r, k))
    }
  }
  colnames(draws) <- assembled$attr_names
  cal <- list(draws = draws, rep_id = rep_id, fold_of_draw = fold_of_draw,
              R = R, folds_use = folds_use, gamma = gamma,
              n_epochs = n_epochs, attr_names = assembled$attr_names,
              analysis_signature = assembled$analysis_signature,
              ceiling_source = "zero_heterogeneity_null_calibration",
              ceiling_status = "conditional_unverified",
              maintained_condition = SB_MAINTAINED_CONDITION,
              statistic = "within-replication fold average of |A~_j|",
              A_raw_draws = A_raw_draws,
              keep_loadings = isTRUE(keep_loadings))
  ## Matched statistic (audit P2). `floor_pooled` is the pre-audit number,
  ## kept only so a rerun can show what changed.
  cal$floor <- sb_matched_floor(cal, gamma = gamma,
                                attr_names = assembled$attr_names)
  cal$floor_pooled <- stats::setNames(
    apply(draws, 2L, stats::quantile, probs = 1 - gamma, names = FALSE,
          type = 1L), assembled$attr_names)
  cal
}

## ---------------------------------------------------------------------
## Matched composite contrast ceilings from signed calibration loadings
## ---------------------------------------------------------------------
##
## The per-coordinate calibration answers "how large is |A~_j| under the
## null?". A composite contrast needs ||A~' c||_2, which the norms alone
## cannot give: it depends on the SIGNS of A~'s rows. Two routes:
##
##   matched_composite   the exact statistic, from the retained signed A~
##                       (needs `keep_loadings = TRUE` at calibration).
##   triangle_fallback   ||A~' c|| <= sum_j |c_j| ||A~_j.||, row by row.
##                       Conservative, and all the stored norms support.
##
## Both are aggregated the SAME way, so they are comparable and the
## inequality survives aggregation: average over the replication's folds
## (matching the observed fold-averaged dispersion), then --- when a whole
## COLUMN is wanted --- take the maximum over the column's support
## contrasts WITHIN the replication, and only then the (1-gamma) quantile
## ACROSS replications. Maximising before quantiling is the conservative
## order (the quantile of a maximum dominates the maximum of quantiles)
## and is what a gate covering "whichever line binds" requires.

sb_calibration_has_loadings <- function(cal) {
  isTRUE(cal$keep_loadings) && is.list(cal$A_raw_draws) &&
    length(cal$A_raw_draws) == nrow(as.matrix(cal$draws)) &&
    !any(vapply(cal$A_raw_draws, is.null, logical(1L)))
}

## Per-replication matched statistic for each contrast: a reps x m matrix
## of within-replication fold averages of ||A~' c||_2.
sb_matched_contrast_reps <- function(cal, C) {
  if (!sb_calibration_has_loadings(cal)) {
    stop("This calibration did not retain signed loadings; rerun ",
         "sb_zero_floor(..., keep_loadings = TRUE) or use the triangle ",
         "fallback.", call. = FALSE)
  }
  C <- as.matrix(C)
  rid <- cal$rep_id
  if (is.null(rid)) {
    rid <- rep(seq_len(cal$R), each = length(cal$folds_use))
  }
  per_draw <- t(vapply(cal$A_raw_draws, function(A) {
    A <- as.matrix(A)
    if (nrow(A) != nrow(C)) {
      stop("Retained loading has ", nrow(A), " coordinates; the contrast ",
           "matrix expects ", nrow(C), ".", call. = FALSE)
    }
    sqrt(colSums(crossprod(A, C)^2))          # ||A~' c||_2 per contrast
  }, numeric(ncol(C))))
  dim(per_draw) <- c(length(cal$A_raw_draws), ncol(C))
  out <- t(vapply(split(seq_len(nrow(per_draw)), rid), function(ii)
    colMeans(per_draw[ii, , drop = FALSE]), numeric(ncol(C))))
  dim(out) <- c(length(unique(rid)), ncol(C))
  colnames(out) <- colnames(C)
  out
}

## The triangle envelope's per-replication statistic, aggregated the same
## way, so the two are directly comparable.
sb_triangle_contrast_reps <- function(cal, C) {
  reps <- sb_calibration_reps(cal)
  out <- reps %*% abs(as.matrix(C))
  colnames(out) <- colnames(C)
  out
}

## One ceiling per contrast (no column maximum): quantile across
## replications of the per-replication statistic.
sb_contrast_floor_matched <- function(cal, C, gamma = NULL,
                                      method = c("auto", "matched_composite",
                                                 "triangle_fallback")) {
  method <- match.arg(method)
  if (is.null(gamma)) gamma <- cal$gamma
  if (is.null(gamma)) stop("A calibration tail level `gamma` is required.",
                           call. = FALSE)
  if (method == "auto") {
    method <- if (sb_calibration_has_loadings(cal)) "matched_composite" else
      "triangle_fallback"
  }
  reps <- if (method == "matched_composite")
    sb_matched_contrast_reps(cal, C) else sb_triangle_contrast_reps(cal, C)
  out <- apply(reps, 2L, stats::quantile, probs = 1 - gamma, names = FALSE,
               type = 1L)
  list(floor = stats::setNames(as.numeric(out), colnames(as.matrix(C))),
       method = method, reps = reps, n_reps = nrow(reps), gamma = gamma)
}

## One ceiling for a whole COLUMN whose support contrasts are the columns
## of `C`: maximum over contrasts within each replication, then the
## quantile across replications.
sb_column_floor_matched <- function(cal, C, gamma = NULL,
                                    method = c("auto", "matched_composite",
                                               "triangle_fallback")) {
  m <- sb_contrast_floor_matched(cal, C, gamma = gamma, method = method)
  per_rep_max <- apply(m$reps, 1L, max)
  list(column = unname(stats::quantile(per_rep_max, probs = 1 - m$gamma,
                                       names = FALSE, type = 1L)),
       per_line = m$floor, per_rep_max = per_rep_max, method = m$method,
       n_reps = m$n_reps, gamma = m$gamma)
}

## ---------------------------------------------------------------------
## Certified lower envelope for the Gaussian bound
## ---------------------------------------------------------------------
##
## The estimand is  L_G = inf_{s in (0, s_bar]} eta(s),
##   eta(s) = mean_i Phi(-mo_i / s).
## A GRID MINIMUM min_g eta(s_g) is an upper bound for that infimum, so it
## cannot be reported as a lower bound: on the 60-point grid a three-mean
## counterexample overstates the infimum by 7.7e-4 (audit finding P1).
##
## The certified replacement partitions (0, s_bar] into cells and bounds
## eta from below on each cell using per-respondent monotonicity:
##   mo_i <  0: Phi(-mo_i/s) is DECREASING in s -> minimised at the cell's
##              RIGHT endpoint;
##   mo_i >  0: Phi(-mo_i/s) is INCREASING in s -> minimised at the cell's
##              LEFT endpoint (0 on the boundary cell, where s -> 0+);
##   mo_i == 0: constant 1/2.
## The cell minimum of the mean is at least the mean of the per-respondent
## cell minima, so `min over cells` is a genuine lower bound for every
## s in (0, s_bar]. This is exactly the envelope sb_gauss_env_target()
## already uses for the one-step targets, so point bound and target now
## share one function -- no observed/simulated statistic mismatch.
##
## Returns the certified lower bound plus the raw grid minimum and their
## gap, so the numerical cost of certification is visible.
sb_gauss_lower_envelope <- function(mo, sbar, env_cells = 8L,
                                    grid_n = 60L, cell_min_ratio = 1 / 50) {
  stopifnot(is.numeric(mo), length(mo) >= 1L, is.finite(sbar), sbar > 0)
  eta <- function(s) mean(stats::pnorm(-mo / s))
  eta0 <- mean(mo < 0) + 0.5 * mean(mo == 0)     # lim_{s -> 0+} eta(s)
  ## Cell endpoints, geometric, matching sb_make_targets()'s grid.
  pts <- sbar * exp(seq(log(cell_min_ratio), 0, length.out = env_cells + 1L))
  cell_lb <- function(s_neg, s_pos) {
    v <- numeric(length(mo))
    neg <- mo < 0
    v[neg] <- stats::pnorm(-mo[neg] / s_neg)
    if (is.finite(s_pos)) v[!neg] <- stats::pnorm(-mo[!neg] / s_pos)
    ## s_pos = Inf encodes the boundary cell's trivial 0 for mo >= 0.
    mean(v)
  }
  lbs <- c(cell_lb(pts[1L], Inf),
           vapply(seq_len(env_cells),
                  function(g) cell_lb(pts[g + 1L], pts[g]), numeric(1L)))
  certified <- min(lbs, eta0)
  grid <- sbar * exp(seq(log(1e-4), 0, length.out = grid_n))
  grid_min <- min(min(vapply(grid, eta, numeric(1L))), eta0)
  list(certified = certified, grid_min = grid_min,
       gap = grid_min - certified, cells = as.integer(env_cells),
       cell_lower_bounds = lbs, eta0 = eta0)
}

## ---------------------------------------------------------------------
## Sensitivity profile over externally supplied ceilings
## ---------------------------------------------------------------------
##
## The honest display for a bound whose ceiling is maintained rather than
## estimated (audit work package 2). Instead of attaching a confidence
## statement to one null-calibrated threshold, show the lower share as a
## function of the ceiling a reader is willing to maintain, and let them
## pick. The calibration threshold is one vertical line on that picture,
## not the picture.
##
## `mo` is the ORIENTED respondent-mean vector: the reported side is
## `mo < 0`, exactly as in `sb_bounds_table()`. `s_bar_grid` is supplied
## externally --- that is the point --- and nothing here reads it off the
## fit.
sb_sensitivity_profile <- function(mo, s_bar_grid, env_cells = 8L,
                                   one_sign = NULL) {
  mo <- as.numeric(mo)
  s_bar_grid <- sort(unique(as.numeric(s_bar_grid)))
  if (!length(s_bar_grid) || any(!is.finite(s_bar_grid)) ||
      any(s_bar_grid <= 0)) {
    stop("`s_bar_grid` must be finite and strictly positive.", call. = FALSE)
  }
  if (is.null(one_sign)) one_sign <- all(mo < 0)
  do.call(rbind, lapply(s_bar_grid, function(sb) {
    LG <- if (isTRUE(one_sign)) {
      mean(stats::pnorm(-mo / sb))
    } else {
      sb_gauss_lower_envelope(mo, sb, env_cells = env_cells)$certified
    }
    LC <- mean(ifelse(mo < 0, mo^2 / (sb^2 + mo^2), 0))
    data.frame(s_bar = sb, lower_bound_gauss = LG,
               lower_bound_cantelli = LC,
               all_one_sign = isTRUE(one_sign),
               stringsAsFactors = FALSE)
  }))
}

## The same, for every contrast of a projected fit, with the orientation
## taken from the prespecified table. Returns one long data frame with the
## ceiling on every row, so no number can be read without its ceiling.
sb_sensitivity_table <- function(assembled, C, orientation,
                                 s_bar_grid, env_cells = 8L) {
  C <- as.matrix(C)
  rm <- sb_respondent_means(assembled)
  do.call(rbind, lapply(colnames(C), function(nm) {
    spec_j <- orient_lookup(orientation, nm)
    if (!identical(spec_j$orientation_source, "prespecified")) {
      return(NULL)          # never display an unprespecified direction
    }
    m <- as.numeric(rm$m %*% C[, nm])
    mo <- if (identical(spec_j$orientation_side, "negative")) m else -m
    prof <- sb_sensitivity_profile(mo, s_bar_grid, env_cells = env_cells)
    cbind(data.frame(contrast = nm,
                     orientation_side = spec_j$orientation_side,
                     orientation_source = spec_j$orientation_source,
                     stringsAsFactors = FALSE),
          prof,
          data.frame(maintained_condition = SB_MAINTAINED_CONDITION,
                     stringsAsFactors = FALSE))
  }))
}

## ---------------------------------------------------------------------
## Regime and release policy
## ---------------------------------------------------------------------
##
## Three regimes, by the fitted spread against the calibration ceiling:
##   fitted_s <  floor          "floored"      s_bar = floor
##   floor <= fitted_s < 2 floor "intermediate_ceiling_window"
##                                             s_bar = fitted_s
##   fitted_s >= 2 floor        "point_identified"  s_bar = fitted_s
##
## The intermediate window is WITHHELD. The bound memo says the raw fitted
## spread must not be used as a ceiling there, but the pre-audit runners
## classified the window as floored and set s_bar = fitted_s anyway
## (audit finding P2, "intermediate floor window").
##
## The point-identified regime is NOT REPORTED as a bound: the framework's
## own ratio-two detection rule says the ordinary sign-share machinery
## governs, and s_bar = fitted_s is again the raw fitted spread.
##
## Mixed-sign coordinates are WITHHELD. Two independent defects (audit
## findings P1): the boundary-cell target is discontinuous at m = 0 (left
## limit 1/2, value 0) so the ordinary smooth one-step theorem does not
## cover it, and the branch itself is selected from the same fitted means
## the bound is computed from.
sb_regime <- function(fitted_s, floor) {
  ## A missing or non-finite ceiling is its own regime, and it fails closed
  ## below. Letting it fall through to `ifelse` would return NA, which the
  ## release rule would then have to treat as "not the withheld cases" ---
  ## fail-open by accident.
  ifelse(!is.finite(fitted_s) | !is.finite(floor), "no_ceiling",
         ifelse(fitted_s < floor, "floored",
                ifelse(fitted_s < 2 * floor, "intermediate_ceiling_window",
                       "point_identified")))
}

## `orientation_source` gates release as hard as the regime does. A side
## chosen from the same fit the interval covers is a selection the theory
## does not account for, so such a row is a diagnostic, never a display
## (audit work package 3).
##
## A row can fail more than one gate at once, and which one you call "the"
## reason would be arbitrary. Every failing reason is reported, in a fixed
## order, joined by "; ". `sb_is_released()` still tests equality with the
## single released status, so a row that fails anything is withheld.
sb_release_status <- function(regime, all_one_sign,
                              orientation_source = NULL) {
  if (is.null(orientation_source)) {
    orientation_source <- rep("unspecified", length(regime))
  }
  out <- character(length(regime))
  for (i in seq_along(regime)) {
    if (!isTRUE(regime[i] %in% c("floored", "intermediate_ceiling_window",
                                 "point_identified"))) {
      out[i] <- "withheld (no ceiling available; the gate cannot be evaluated)"
      next
    }
    if (identical(regime[i], "point_identified")) {
      out[i] <- paste0("not_reported (point-identified regime; ordinary ",
                       "share machinery governs)")
      next
    }
    reasons <- character(0)
    if (identical(regime[i], "intermediate_ceiling_window")) {
      reasons <- c(reasons, "no valid ceiling in the intermediate window")
    }
    if (!identical(orientation_source[i], "prespecified")) {
      reasons <- c(reasons, paste0("orientation not prespecified: ",
                                   orientation_source[i]))
    }
    if (!isTRUE(all_one_sign[i])) {
      reasons <- c(reasons,
                   "mixed-sign inference disabled pending valid theory")
    }
    out[i] <- if (!length(reasons)) "conditional_sensitivity" else
      paste0("withheld (", paste(reasons, collapse = "; "), ")")
  }
  out
}

sb_is_released <- function(bound_release) {
  rel <- bound_release == "conditional_sensitivity"
  rel[is.na(rel)] <- FALSE
  rel
}

## ---------------------------------------------------------------------
## Worst-case share bounds per coordinate
## ---------------------------------------------------------------------
##
## Orientation. The pre-audit column was called `modal_side`, which was
## wrong twice over: when `orient` is supplied the side is the SIGN OF AN
## AVERAGE one-step estimate, not the respondent mode, and when it is not
## supplied the side is the mode of the FITTED means. The column is now
## `orientation_side` and the rule that produced it is recorded in
## `orientation_rule`. Neither rule is prespecified, so both are
## data-selected and the table says so in `branch_status`.
##
## `sign_margin`: a PRESPECIFIED population sign margin for the one-sign
## branch. When supplied, `sign_margin_ok` records whether every oriented
## fitted mean clears it. When NA (the default) the one-sign branch is
## data-selected and `sign_margin_ok` is NA.
sb_bounds_table <- function(assembled, floors, grid_n = 60L, orient = NULL,
                            env_cells = 8L, sign_margin = NA_real_,
                            orientation = NULL,
                            ceiling_source = "zero_heterogeneity_null_calibration",
                            ceiling_status = "conditional_unverified") {
  rm <- sb_respondent_means(assembled)
  fitted_s <- sb_fitted_dispersion(assembled)
  p <- ncol(rm$m)
  nm <- assembled$attr_names
  rows <- vector("list", p)
  for (j in seq_len(p)) {
    m <- rm$m[, j]
    ## Orientation, in strict precedence: a prespecified side beats
    ## anything read off the fit. The legacy data-selected rules still run
    ## when no prespecification exists, because their columns remain
    ## useful as diagnostics --- but they cannot release (see
    ## sb_release_status()).
    spec_j <- if (exists("orient_lookup", mode = "function"))
      orient_lookup(orientation, nm[j]) else
        list(orientation_side = NA_character_,
             orientation_source = "unspecified",
             sign_margin = NA_real_, rationale = NA_character_)
    oriented_by_onestep <- !is.null(orient) && nm[j] %in% names(orient)
    if (identical(spec_j$orientation_source, "prespecified")) {
      negative_side <- identical(spec_j$orientation_side, "negative")
      orientation_source <- "prespecified"
      orientation_rule <- paste0("prespecified: ", spec_j$rationale)
    } else if (oriented_by_onestep) {
      negative_side <- orient[[nm[j]]] < 0
      orientation_source <- "onestep_theta_sign"
      orientation_rule <- "sign of the one-step theta (data-selected)"
    } else {
      negative_side <- mean(m < 0) >= 0.5
      orientation_source <- "fitted_mean_mode"
      orientation_rule <- "modal sign of the fitted means (data-selected)"
    }
    margin_j <- if (is.finite(spec_j$sign_margin)) spec_j$sign_margin else
      as.numeric(sign_margin)
    mo <- if (negative_side) m else -m       # oriented: reported side is mo < 0
    sbar <- max(fitted_s[j], floors[j])
    one_sign <- all(mo < 0)
    min_abs <- min(abs(m))
    env <- sb_gauss_lower_envelope(mo, sbar, env_cells = env_cells,
                                   grid_n = grid_n)
    LG <- if (one_sign) {
      ## One-sign case: eta is decreasing in s, so the infimum over
      ## (0, s_bar] is attained exactly at s_bar. No envelope needed.
      mean(stats::pnorm(-mo / sbar))
    } else env$certified
    LC <- mean(ifelse(mo < 0, mo^2 / (sbar^2 + mo^2), 0))
    reg <- sb_regime(fitted_s[j], floors[j])
    rows[[j]] <- data.frame(
      coordinate = nm[j],
      orientation_side = if (negative_side) "negative" else "positive",
      orientation_source = orientation_source,
      orientation_rule = orientation_rule,
      orientation_rationale = if (is.na(spec_j$rationale)) NA_character_
        else spec_j$rationale,
      all_one_sign = one_sign,
      min_abs_mean = min_abs,
      sign_margin = margin_j,
      sign_margin_ok = if (is.na(margin_j)) NA else min_abs >= margin_j,
      branch_status = if (is.na(margin_j))
        "sign margin not checked (diagnostic gate absent)" else
        "sign margin checked (diagnostic only; no uniform theorem supplied)",
      mean_abs = mean(abs(m)), mean_oriented = mean(mo),
      fitted_s = fitted_s[j], floor = floors[j], s_bar = sbar,
      ceiling_source = ceiling_source,
      ceiling_status = ceiling_status,
      maintained_condition = SB_MAINTAINED_CONDITION,
      regime = reg,
      share_limit_s0 = env$eta0,
      lower_bound_gauss = LG,
      envelope_cells = if (one_sign) NA_integer_ else env$cells,
      envelope_gap_vs_grid = if (one_sign) NA_real_ else env$gap,
      lower_bound_cantelli = LC,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  out$bound_release <- sb_release_status(out$regime, out$all_one_sign,
                                         out$orientation_source)
  out
}

## Typed plugin targets at FIXED s_bar for the one-step machinery, each a
## rowwise expectation, sigma-invariant, with analytic d_mu.
##
## One-sign coordinates get a single Gaussian target at s_bar (the minimum
## sits there; Proposition 3, one-sign case). Mixed-sign coordinates get
## the monotone bracketing envelope: one target per grid cell, evaluating
## negative-mean terms at the cell's right endpoint and positive-mean terms
## at the left, plus the boundary cell [0, s_1] with the trivial zero for
## positive terms.
##
## AUDIT NOTE. The envelope's per-respondent sign split makes the boundary
## cell's rowwise target DISCONTINUOUS at m = 0: its left limit is 1/2 and
## its value is 0, and the analytic derivative omits the crossing term. The
## smooth one-step theorem does not cover it. The mixed-sign branch is
## therefore built for diagnostics only and its one-step numbers are never
## released (see sb_release_status()).
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
    sgn <- if (bounds_table$orientation_side[j] == "negative") 1 else -1
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

## ---------------------------------------------------------------------
## One-step quantities and the release gate
## ---------------------------------------------------------------------
##
## `*_cond_l95` is a one-sided 95 percent limit CONDITIONAL on the
## maintained ceiling. It is not a confidence bound for the share: the
## ceiling is unverified, and orientation and branch are selected on the
## same data. The released columns are NA unless `bound_release` says
## `conditional_sensitivity`.
## ---------------------------------------------------------------------
## Table-level multiplicity (audit work package 4)
## ---------------------------------------------------------------------
##
## Needed only when the PROSE makes a joint claim across several rows
## ("every one of these contrasts is at least ..."). A table of row-by-row
## descriptions stays pointwise, and says so.
##
## Bonferroni, deliberately: alpha_row = alpha_family / K with K the
## prespecified number of rows in the family. It is conservative, needs no
## dependence model, and cannot be gamed by reordering, because K is a
## count. Adding a row to a family can only lower every adjusted endpoint
## in it.
##
## WHAT THIS DOES NOT DO. It controls the SAMPLING-error component,
## conditional on the maintained dispersion ceiling. It does not convert a
## conditional sensitivity calculation into unconditional inference, and
## no combination of the two makes the ceiling verified.
SB_MULTIPLICITY_SCOPE <- paste0(
  "Bonferroni over the family's prespecified rows; controls the sampling ",
  "error component ONLY, conditional on the maintained ceiling ",
  "s_true <= s_bar. It does not make the ceiling verified.")

#' Attach family-adjusted one-sided endpoints.
#'
#' @param bounds_table output of `sb_confidence_bounds()`.
#' @param claim_family `NULL` for a pointwise table (the default); a single
#'   string to put every RELEASED row in one family; or a character vector
#'   of length `nrow()` with `NA` for rows in no family.
#' @param alpha_family familywise one-sided level (default 0.05).
sb_attach_multiplicity <- function(bounds_table, claim_family = NULL,
                                   alpha_family = 0.05) {
  n <- nrow(bounds_table)
  if (is.null(claim_family)) {
    fam <- rep(NA_character_, n)
  } else if (length(claim_family) == 1L) {
    rel <- sb_is_released(bounds_table$bound_release)
    fam <- ifelse(rel, as.character(claim_family), NA_character_)
  } else if (length(claim_family) == n) {
    fam <- as.character(claim_family)
  } else {
    stop("`claim_family` must be NULL, one string, or one entry per row.",
         call. = FALSE)
  }
  K <- rep(NA_integer_, n)
  a_row <- rep(NA_real_, n)
  for (f in unique(stats::na.omit(fam))) {
    ii <- which(fam == f)
    K[ii] <- length(ii)
    a_row[ii] <- alpha_family / length(ii)
  }
  z_adj <- ifelse(is.na(a_row), NA_real_, stats::qnorm(1 - a_row))
  bounds_table$claim_family <- fam
  bounds_table$alpha_family <- ifelse(is.na(fam), NA_real_, alpha_family)
  bounds_table$claim_family_K <- K
  bounds_table$alpha_row <- a_row
  bounds_table$multiplicity_method <- ifelse(
    is.na(fam), "none (pointwise row description)", "bonferroni")
  bounds_table$multiplicity_scope <- ifelse(is.na(fam), NA_character_,
                                            SB_MULTIPLICITY_SCOPE)
  ## Adjusted endpoints, from the SAME one-step estimate and standard
  ## error, with the family-adjusted quantile.
  se_g <- (bounds_table$gauss_onestep - bounds_table$gauss_cond_l95) /
    stats::qnorm(0.95)
  se_c <- (bounds_table$cant_onestep - bounds_table$cant_cond_l95) /
    stats::qnorm(0.95)
  bounds_table$gauss_cond_ladj <- ifelse(
    is.na(z_adj), NA_real_,
    pmax(0, bounds_table$gauss_onestep - z_adj * se_g))
  bounds_table$cant_cond_ladj <- ifelse(
    is.na(z_adj), NA_real_,
    pmax(0, bounds_table$cant_onestep - z_adj * se_c))
  rel <- sb_is_released(bounds_table$bound_release)
  bounds_table$released_gauss_cond_ladj <-
    ifelse(rel, bounds_table$gauss_cond_ladj, NA_real_)
  bounds_table$released_cant_cond_ladj <-
    ifelse(rel, bounds_table$cant_cond_ladj, NA_real_)
  bounds_table
}

#' Gate a familywise caption. A joint claim may be written only when every
#' row it covers shares one family and carries the adjustment metadata.
sb_require_family_adjustment <- function(bounds_table, family,
                                         what = "familywise claim") {
  need <- c("claim_family", "alpha_family", "claim_family_K", "alpha_row",
            "multiplicity_method", "gauss_cond_ladj")
  miss <- setdiff(need, names(bounds_table))
  if (length(miss)) {
    stop("Cannot make a ", what, ": missing ", paste(miss, collapse = ", "),
         ". Run sb_attach_multiplicity() first.", call. = FALSE)
  }
  ii <- which(!is.na(bounds_table$claim_family) &
                bounds_table$claim_family == family)
  if (!length(ii)) {
    stop("Cannot make a ", what, ": no rows carry claim_family '", family,
         "'.", call. = FALSE)
  }
  bad <- ii[is.na(bounds_table$alpha_row[ii]) |
              is.na(bounds_table$claim_family_K[ii]) |
              bounds_table$multiplicity_method[ii] != "bonferroni" |
              is.na(bounds_table$gauss_cond_ladj[ii])]
  if (length(bad)) {
    stop("Cannot make a ", what, ": ", length(bad),
         " row(s) in family '", family, "' lack adjustment metadata.",
         call. = FALSE)
  }
  invisible(TRUE)
}

sb_attach_release <- function(bounds_table) {
  rel <- sb_is_released(bounds_table$bound_release)
  na_if <- function(v) ifelse(rel, v, NA_real_)
  bounds_table$released_lower_bound_gauss <- na_if(bounds_table$lower_bound_gauss)
  bounds_table$released_gauss_cond_l95 <- na_if(bounds_table$gauss_cond_l95)
  bounds_table$released_lower_bound_cantelli <-
    na_if(bounds_table$lower_bound_cantelli)
  bounds_table$released_cant_cond_l95 <- na_if(bounds_table$cant_cond_l95)
  bounds_table$inference_population <- SB_INFERENCE_POPULATION
  bounds_table$selection_status <- paste0(
    "orientation and one-sign/mixed-sign branch are selected on the same ",
    "data; no sample split, uniform, or selective-inference adjustment")
  bounds_table
}

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
  n <- nrow(bounds_table)
  gauss_onestep <- gauss_l <- rep(NA_real_, n)
  for (j in seq_len(n)) {
    nm <- bounds_table$coordinate[j]
    if (isTRUE(bounds_table$all_one_sign[j])) {
      lab <- paste0("gauss_", nm)
      gauss_onestep[j] <- est[[lab]]
      gauss_l[j] <- est[[lab]] - z * se[[lab]]
    } else {
      ## Mixed-sign: the envelope's boundary cell has no valid influence
      ## function, so no one-step number is formed. The cell minimum of
      ## the PLUG-IN values is kept as a diagnostic only.
      labs <- grep(paste0("^gaussenv_", nm, "_c"), names(est), value = TRUE)
      gauss_onestep[j] <- if (length(labs)) min(as.numeric(est[labs])) else
        NA_real_
      gauss_l[j] <- NA_real_
    }
  }
  lab_c <- paste0("cant_", bounds_table$coordinate)
  bounds_table$gauss_onestep <- gauss_onestep
  bounds_table$gauss_cond_l95 <- pmax(0, gauss_l)
  bounds_table$cant_onestep <- as.numeric(est[lab_c])
  bounds_table$cant_cond_l95 <- pmax(0,
    as.numeric(est[lab_c]) - z * as.numeric(se[lab_c]))
  bounds_table$dml_status <- out$status
  bounds_table <- sb_attach_release(bounds_table)
  attr(bounds_table, "dml") <- out
  bounds_table
}
