## Bounded sign shares along arbitrary LINEAR CONTRASTS.
##
## Generalizes applications/R/share_bounds.R from the p native coordinates
## to any contrast c (length p, raw coordinate scale). Companion to
## 2608_issues/Yiqing/share_bounds_memo_2026-08-26.tex, whose Section 2
## already states the estimand for a general reporting contrast c; this
## file supplies the code path the memo's algorithm assumes.
##
## DESIGN RULE: this file contains NO copy of the audited bound math. It
## reaches the contrast case by PROJECTION --- it builds a pseudo-assembled
## fit whose m "coordinates" are the m contrasts, and then calls the
## audited sb_bounds_table() / sb_make_targets() on that object. For
## c = e_j the projection is the identity, so the audited row is
## reproduced exactly (see test-contrast-bounds.R, ORACLE check).
##
## Base R + stats only. Source AFTER pkgload::load_all(<pkg root>) if the
## confidence-bound step is wanted; the point bounds need no package.

## ---------------------------------------------------------------------
## 0. Locate and load the audited source of record.
## ---------------------------------------------------------------------

## Production location is applications/R/share_bounds.R inside the package
## root (the machine that holds the fits). The Dropbox path is the
## documented fallback for local development on machines where the file
## has not been copied into the repo yet.
sb_share_bounds_path <- function(root = NULL) {
  cand <- character()
  if (!is.null(root)) {
    cand <- c(cand, file.path(root, "applications/R/share_bounds.R"))
  }
  cand <- c(
    cand,
    file.path(path.expand("~/GitHub/sconjoint"),
              "applications/R/share_bounds.R"),
    path.expand(file.path(
      "~/Dropbox/Research_Hub/Projects/ConjointStructural/2608_issues",
      "Yiqing/bound_for_share/code/share_bounds.R")))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) {
    stop("Cannot find the audited share_bounds.R. Looked in:\n  ",
         paste(cand, collapse = "\n  "), call. = FALSE)
  }
  hit[[1L]]
}

## Idempotent: if the driver already sourced share_bounds.R, leave it be.
if (!exists("sb_bounds_table", mode = "function")) {
  source(sb_share_bounds_path())
}

## ---------------------------------------------------------------------
## 1. Contrast bookkeeping. Every contrast is resolved by NAME against
##    the fit's attr_names; a missing name is a hard error.
## ---------------------------------------------------------------------

## `contrasts` is either a named list of named numeric vectors (sparse is
## fine --- only the coordinates the contrast touches need appear), or a
## p x m matrix whose columns are named and whose rows are named by (a
## superset of) attr_names, or an unnamed-row matrix with exactly p rows
## assumed already in attr_names order.
##
## Returns a p x m matrix with rownames = attr_names, colnames = contrast
## names.
sb_as_contrast_matrix <- function(contrasts, attr_names) {
  attr_names <- as.character(attr_names)
  p <- length(attr_names)
  if (anyDuplicated(attr_names)) {
    stop("attr_names contains duplicates; name lookup is unsafe.",
         call. = FALSE)
  }

  if (is.matrix(contrasts) || is.data.frame(contrasts)) {
    C <- as.matrix(contrasts)
    storage.mode(C) <- "double"
    if (is.null(colnames(C))) {
      stop("A contrast MATRIX must have column names (the contrast ids).",
           call. = FALSE)
    }
    if (!is.null(rownames(C))) {
      missing_nm <- setdiff(attr_names, rownames(C))
      if (length(missing_nm)) {
        stop("Contrast matrix rownames do not cover attr_names; missing: ",
             paste(missing_nm, collapse = ", "), call. = FALSE)
      }
      C <- C[attr_names, , drop = FALSE]
    } else {
      if (nrow(C) != p) {
        stop("Contrast matrix has ", nrow(C), " rows but the fit has ", p,
             " coordinates, and no rownames to match on.", call. = FALSE)
      }
      rownames(C) <- attr_names
    }
  } else {
    if (!is.list(contrasts) || !length(contrasts)) {
      stop("`contrasts` must be a non-empty named list or a p x m matrix.",
           call. = FALSE)
    }
    if (is.null(names(contrasts)) || any(!nzchar(names(contrasts)))) {
      stop("Every element of `contrasts` must be named.", call. = FALSE)
    }
    C <- matrix(0, nrow = p, ncol = length(contrasts),
                dimnames = list(attr_names, names(contrasts)))
    for (nm in names(contrasts)) {
      v <- contrasts[[nm]]
      if (!is.numeric(v) || !length(v)) {
        stop("Contrast '", nm, "' is not a non-empty numeric vector.",
             call. = FALSE)
      }
      if (is.null(names(v))) {
        if (length(v) != p) {
          stop("Contrast '", nm, "' is unnamed and has length ", length(v),
               ", not ", p, ". Name it, or give all p entries.",
               call. = FALSE)
        }
        names(v) <- attr_names
      }
      if (anyDuplicated(names(v))) {
        stop("Contrast '", nm, "' names a coordinate twice: ",
             paste(unique(names(v)[duplicated(names(v))]), collapse = ", "),
             call. = FALSE)
      }
      ## Hard name lookup. Never positional.
      unknown <- setdiff(names(v), attr_names)
      stopifnot(
        `contrast names must all be coordinates of the fit` =
          length(unknown) == 0L)
      C[names(v), nm] <- as.numeric(v)
    }
  }

  if (anyDuplicated(colnames(C))) {
    stop("Duplicate contrast names: ",
         paste(unique(colnames(C)[duplicated(colnames(C))]),
               collapse = ", "), call. = FALSE)
  }
  if (any(!is.finite(C))) {
    stop("Contrast weights must be finite.", call. = FALSE)
  }
  zero <- colSums(abs(C)) == 0
  if (any(zero)) {
    stop("All-zero contrast(s): ",
         paste(colnames(C)[zero], collapse = ", "), call. = FALSE)
  }
  C
}

## ---------------------------------------------------------------------
## 2. Projection: turn the p-coordinate assembled fit into an
##    m-"coordinate" pseudo fit whose coordinates ARE the contrasts.
## ---------------------------------------------------------------------
##
## After this, sb_respondent_means() returns m_c(i) = c' beta_i for every
## contrast, and sb_fitted_dispersion() returns ||A_raw' c||_2 fold-averaged
## --- both computed by the audited functions, unmodified.
##
## sd_dx_folds is set to 1 because A_folds is replaced by the ALREADY-raw
## projected loadings, and sb_raw_A_folds() divides by sd_dx_folds.
sb_project_assembled <- function(assembled, C) {
  A_raw <- sb_raw_A_folds(assembled)
  p <- nrow(A_raw[[1L]])
  if (nrow(C) != p) {
    stop("Contrast matrix has ", nrow(C), " rows; the fit's loadings have ",
         p, ".", call. = FALSE)
  }
  mu <- as.matrix(assembled$mu_hat)
  if (ncol(mu) != p) {
    stop("mu_hat has ", ncol(mu), " columns; the fit's loadings have ", p,
         ".", call. = FALSE)
  }
  ## sb_bounds_table() assumes mu_hat's columns are in attr_names order
  ## (it pairs rm$m[, j] with nm[j]). The contrast path depends on the
  ## same alignment and can check it whenever the names are carried.
  if (!is.null(colnames(mu)) && !is.null(rownames(C)) &&
      !identical(colnames(mu), rownames(C))) {
    stop("mu_hat's column names are not the fit's attr_names in order; ",
         "contrast projection would silently mis-align.", call. = FALSE)
  }
  m <- ncol(C)
  proj <- assembled
  proj$A_folds <- lapply(A_raw, function(A) crossprod(C, A))  # m x q
  proj$sd_dx_folds <- rep(list(rep(1, m)), length(A_raw))
  proj$mu_hat <- mu %*% C                                     # n x m
  proj$attr_names <- colnames(C)
  proj$K <- length(A_raw)
  ## Fields that no longer describe this object are dropped rather than
  ## left stale; nothing downstream of sb_bounds_table() reads them.
  proj$deltaX <- NULL
  proj$mu_all_folds <- NULL
  proj$A_computational_folds <- NULL
  proj$A_folds_aligned <- NULL
  proj$dx_transform_folds <- NULL
  proj
}

## ---------------------------------------------------------------------
## 3. The dispersion floor along a contrast direction.
## ---------------------------------------------------------------------
##
## WHAT sb_zero_floor() DOES, AND WHAT IT DOES NOT GIVE US.
##
## sb_zero_floor() simulates R zero-heterogeneity datasets, refits the
## selected learner on each retained fold, and records the apparent raw
## dispersion the refit manufactures from noise. It records it as
## sqrt(rowSums(A_raw^2)) --- the PER-COORDINATE norms |A~_j| --- and
## returns the (1-gamma) quantile of each column. The refit's loading
## MATRIX A~ itself is not retained.
##
## The calibration LOGIC generalizes verbatim: the exact contrast floor is
## the (1-gamma) quantile of ||A~' c||_2 over the same draws. But that
## number cannot be recovered from the saved object, because ||A~' c||
## depends on the SIGNS (for q > 1, the directions) of A~'s rows, and only
## their norms were stored. This is a limitation of the stored artifact,
## not of the construction.
##
## Three modes, in decreasing order of preference:
##
##  "supplied"  A numeric floor per contrast, computed on the machine that
##              holds the fit by re-running the zero-heterogeneity
##              calibration and recording ||A~' c|| instead of |A~_j|.
##              This is the exact object. USE THIS FOR PUBLICATION.
##
##  "draws"     From fl$draws (the R x p matrix of per-coordinate norms):
##              floor_c = q_{1-gamma}( sum_j |c_j| * draws[, j] ), the
##              row-wise triangle-inequality envelope. Since
##              ||A~' c|| = || sum_j c_j A~_j. || <= sum_j |c_j| ||A~_j.||
##              holds row by row, its (1-gamma) quantile dominates the
##              exact contrast floor. A LARGER ceiling can only LOWER
##              both bounds --- L_G is a minimum of eta over (0, s_bar],
##              so widening the interval can only lower it, and each
##              Cantelli term m^2/(s_bar^2 + m^2) is decreasing in s_bar
##              --- so this errs conservative on both branches. Reduces
##              to the exact per-coordinate floor when c = e_j.
##
##  "coordinate_sum"  floor_c = sum_j |c_j| * floor_j, from fl$floor alone.
##              An approximation, not a proven bound: a quantile of a sum
##              is not the sum of quantiles in general. It coincides with
##              "draws" under comonotone draws, which the rank-one (q = 1)
##              fits approach because every coordinate's apparent
##              dispersion comes from the same refit. Reduces to the
##              exact per-coordinate floor when c = e_j. Warns.
sb_contrast_floor <- function(C, floors_or_calibration,
                              floor_mode = c("auto", "supplied", "draws",
                                             "coordinate_sum"),
                              gamma = NULL, attr_names = rownames(C)) {
  floor_mode <- match.arg(floor_mode)
  cn <- colnames(C)
  fl <- floors_or_calibration

  is_cal <- is.list(fl) && !is.data.frame(fl) && !is.null(fl$floor)

  if (floor_mode == "auto") {
    floor_mode <- if (is_cal) {
      if (!is.null(fl$draws)) "draws" else "coordinate_sum"
    } else if (is.numeric(fl) && !is.null(names(fl)) &&
               all(cn %in% names(fl))) {
      "supplied"
    } else {
      "coordinate_sum"
    }
  }

  if (floor_mode == "supplied") {
    v <- if (is_cal) fl$floor else fl
    if (!is.numeric(v) || is.null(names(v))) {
      stop("floor_mode='supplied' needs a NAMED numeric vector of floors, ",
           "one per contrast.", call. = FALSE)
    }
    missing_nm <- setdiff(cn, names(v))
    stopifnot(`a floor must be supplied for every contrast` =
                length(missing_nm) == 0L)
    out <- as.numeric(v[cn])
  } else {
    ## Both remaining modes need per-coordinate material aligned to
    ## attr_names, resolved BY NAME.
    if (is_cal) {
      per_coord <- fl$floor
      draws <- fl$draws
      if (is.null(gamma)) gamma <- fl$gamma
    } else {
      per_coord <- fl
      draws <- NULL
    }
    if (!is.numeric(per_coord)) {
      stop("Per-coordinate floors must be numeric.", call. = FALSE)
    }
    if (is.null(names(per_coord))) {
      if (length(per_coord) != nrow(C)) {
        stop("Unnamed per-coordinate floors have length ", length(per_coord),
             ", not ", nrow(C), ".", call. = FALSE)
      }
      names(per_coord) <- attr_names
    }
    missing_nm <- setdiff(attr_names, names(per_coord))
    stopifnot(`per-coordinate floors must cover every attr_name` =
                length(missing_nm) == 0L)
    per_coord <- per_coord[attr_names]

    if (floor_mode == "draws") {
      if (is.null(draws)) {
        stop("floor_mode='draws' needs the calibration's `draws` matrix ",
             "(the full sb_zero_floor() return value).", call. = FALSE)
      }
      draws <- as.matrix(draws)
      if (ncol(draws) != nrow(C)) {
        stop("Calibration draws have ", ncol(draws), " columns; the fit has ",
             nrow(C), " coordinates.", call. = FALSE)
      }
      if (is.null(gamma)) {
        stop("floor_mode='draws' needs gamma (the calibration's tail level).",
             call. = FALSE)
      }
      env <- draws %*% abs(C)                       # R x m, row-wise envelope
      out <- apply(env, 2L, stats::quantile, probs = 1 - gamma,
                   names = FALSE, type = 1L)
    } else {
      warning("floor_mode='coordinate_sum' is an APPROXIMATION for ",
              "multi-coordinate contrasts (a quantile of a sum is not the ",
              "sum of quantiles). Exact only for single-coordinate ",
              "contrasts. Prefer 'draws', or supply a recalibrated floor.",
              call. = FALSE)
      out <- as.numeric(abs(t(C)) %*% per_coord)
    }
  }

  if (any(!is.finite(out)) || any(out < 0)) {
    stop("Contrast floors must be finite and non-negative.", call. = FALSE)
  }
  stats::setNames(out, cn)
}

## ---------------------------------------------------------------------
## 4. The bounds table for arbitrary contrasts.
## ---------------------------------------------------------------------
##
## Orientation. sb_bounds_table() orients coordinate j by orient[[nm_j]]
## when that name is present, else by the modal sign of the fitted means.
## The contrast analogue orients by c' theta, and is used only when EVERY
## coordinate the contrast touches has a one-step theta; otherwise the
## contrast falls back to the modal rule, exactly as an unlisted
## coordinate does. For c = e_j the two rules coincide.
##
## Returns sb_bounds_table()'s columns verbatim, with a leading `contrast`
## identifier column (`coordinate` is retained and equals `contrast`).
sb_contrast_bounds <- function(assembled, contrasts, floors_or_calibration,
                               orient = NULL, grid_n = 60L,
                               floor_mode = c("auto", "supplied", "draws",
                                              "coordinate_sum"),
                               gamma = NULL, return_projection = FALSE) {
  floor_mode <- match.arg(floor_mode)
  attr_names <- as.character(assembled$attr_names)
  if (!length(attr_names)) {
    stop("The assembled fit has no attr_names; contrasts cannot be ",
         "resolved by name.", call. = FALSE)
  }
  C <- sb_as_contrast_matrix(contrasts, attr_names)
  m <- ncol(C)

  floors <- sb_contrast_floor(C, floors_or_calibration,
                              floor_mode = floor_mode, gamma = gamma,
                              attr_names = attr_names)

  ## Orientation, projected. c' theta over the contrast's support.
  orient_c <- NULL
  if (!is.null(orient)) {
    if (is.null(names(orient))) {
      stop("`orient` must be a NAMED vector of one-step thetas.",
           call. = FALSE)
    }
    th <- stats::setNames(numeric(length(attr_names)), attr_names)
    shared <- intersect(attr_names, names(orient))
    th[shared] <- as.numeric(orient[shared])
    covered <- vapply(seq_len(m), function(j) {
      all(attr_names[C[, j] != 0] %in% names(orient))
    }, logical(1L))
    if (any(covered)) {
      vals <- as.numeric(crossprod(C[, covered, drop = FALSE], th))
      orient_c <- stats::setNames(vals, colnames(C)[covered])
    }
  }

  ## sb_fitted_dispersion() collapses to a length-K vector when there is
  ## exactly one coordinate (vapply drops the p = 1 matrix dimension), so
  ## pad to two columns and drop the pad afterwards. The pad is a copy of
  ## the first contrast, so it cannot change the other rows --- every
  ## column of sb_bounds_table() is computed independently per coordinate.
  padded <- m < 2L
  Cw <- C
  floors_w <- floors
  if (padded) {
    pad_nm <- ".__sb_pad__"
    Cw <- cbind(C, C[, 1L, drop = FALSE])
    colnames(Cw) <- c(colnames(C), pad_nm)
    floors_w <- c(floors, stats::setNames(floors[[1L]], pad_nm))
  }

  proj <- sb_project_assembled(assembled, Cw)
  tb <- sb_bounds_table(proj, floors_w, grid_n = grid_n, orient = orient_c)
  tb <- tb[seq_len(m), , drop = FALSE]
  rownames(tb) <- NULL

  out <- cbind(data.frame(contrast = colnames(C),
                          stringsAsFactors = FALSE),
               tb)
  rownames(out) <- NULL
  attr(out, "contrast_matrix") <- C
  if (return_projection) attr(out, "projected_fit") <- proj
  out
}

## ---------------------------------------------------------------------
## 5. Typed plugin targets for arbitrary contrasts.
## ---------------------------------------------------------------------
##
## sb_make_targets() builds its targets from a unit vector in the space it
## is handed. Handing it the PROJECTED space gives targets that are
## correct in projected mu; sb_wrap_target_to_original() then pulls each
## one back to the fit's own p-dimensional mu by the chain rule
##   d/d mu_orig = C %*% d/d mu_proj,
## leaving value, d_kappa, labels and sigma_invariant untouched. No bound
## math is re-implemented here.
sb_wrap_target_to_original <- function(f, C) {
  force(f); force(C)
  function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    mu <- as.matrix(mu)
    if (ncol(mu) != nrow(C)) {
      stop("Target received mu with ", ncol(mu), " columns; the contrast ",
           "matrix expects ", nrow(C), ".", call. = FALSE)
    }
    res <- f(mu %*% C, kappa, Sigma, Z, respondent_id, fold, colnames(C))
    dm_p <- res$d_mu
    N <- dim(dm_p)[1L]
    L <- dim(dm_p)[2L]
    dm <- array(0, c(N, L, nrow(C)))
    for (l in seq_len(L)) {
      ## (N x m) %*% (m x p) -> N x p
      slice <- matrix(dm_p[, l, ], nrow = N, ncol = ncol(C))
      dm[, l, ] <- slice %*% t(C)
    }
    res$d_mu <- dm
    res
  }
}

## Mirrors sb_confidence_bounds()'s target step, but contrast-aware.
## `bounds_table` must be the sb_contrast_bounds() output (its
## `coordinate` column carries the contrast names, which is what
## sb_make_targets() keys on).
sb_contrast_targets <- function(bounds_table, C, env_cells = 8L) {
  if (!all(bounds_table$coordinate == colnames(C))) {
    stop("bounds_table rows and contrast matrix columns are out of step.",
         call. = FALSE)
  }
  proj_targets <- sb_make_targets(bounds_table, colnames(C),
                                  env_cells = env_cells)
  lapply(proj_targets, sb_wrap_target_to_original, C = C)
}

## ---------------------------------------------------------------------
## 6. One-sided 95 percent lower confidence bounds.
## ---------------------------------------------------------------------
##
## REQUIRES A REAL FIT. scmix_dml() is package one-step machinery; it
## needs the assembled fit's nuisances, folds and integration grids, and
## cannot be exercised against a synthetic object. The label bookkeeping
## below mirrors sb_confidence_bounds() one-for-one (it is the only part
## of that function that is not already contrast-general) and is pinned to
## it by a regression check in test-contrast-bounds.R that stubs
## scmix_dml().
sb_contrast_confidence_bounds <- function(assembled, bounds_table, inf_cfg,
                                          seed, env_cells = 8L) {
  C <- attr(bounds_table, "contrast_matrix")
  if (is.null(C)) {
    stop("bounds_table is missing its `contrast_matrix` attribute; pass the ",
         "sb_contrast_bounds() output unmodified.", call. = FALSE)
  }
  if (!exists("scmix_dml", mode = "function")) {
    stop("scmix_dml() not found. Run pkgload::load_all(<sconjoint root>) ",
         "first; the confidence-bound step needs the real package and a ",
         "real assembled fit.", call. = FALSE)
  }
  targets <- sb_contrast_targets(bounds_table, C, env_cells = env_cells)
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
  gauss_onestep <- gauss_lcb <- numeric(n)
  for (j in seq_len(n)) {
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
