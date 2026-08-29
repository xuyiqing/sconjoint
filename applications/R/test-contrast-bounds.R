#!/usr/bin/env Rscript
## Self-contained tests for applications/R/contrast_bounds.R.
##   Rscript applications/R/test-contrast-bounds.R
## Prints PASS/FAIL per check; exits non-zero if anything fails.
##
## No package load, no real fit: everything runs against SYNTHETIC
## assembled fits carrying the field names the real ones use (A_folds,
## sd_dx_folds, mu_hat, respondent_id, K, attr_names, analysis_signature).
## The confidence-bound step is exercised with a stubbed scmix_dml(); the
## real one needs the real fit and is left to the driver.

options(stringsAsFactors = FALSE, warn = 1)
set.seed(20260827L)

## --------------------------------------------------------------------
## Harness
## --------------------------------------------------------------------
.n_pass <- 0L; .n_fail <- 0L
ok <- function(label, passed, detail = "") {
  if (isTRUE(passed)) {
    .n_pass <<- .n_pass + 1L
    cat(sprintf("PASS  %-58s %s\n", label, detail))
  } else {
    .n_fail <<- .n_fail + 1L
    cat(sprintf("FAIL  %-58s %s\n", label, detail))
  }
  invisible(passed)
}
## Section guard. tryCatch evaluates its expression in THIS frame
## (globalenv), so assignments inside a guarded block still persist; an
## error becomes a FAIL instead of aborting the suite and hiding the rest.
guard <- function(label) function(e) ok(paste(label, "[aborted]"), FALSE,
                                        conditionMessage(e))
maxdev <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  if (length(a) != length(b)) return(Inf)
  bad <- is.na(a) != is.na(b)
  if (any(bad)) return(Inf)
  keep <- !is.na(a)
  if (!any(keep)) return(0)
  max(abs(a[keep] - b[keep]))
}

## --------------------------------------------------------------------
## Load the audited source of record, then the code under test.
## --------------------------------------------------------------------
here <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[[1L]])) else getwd()
}, error = function(e) getwd())

## PRODUCTION FIRST. contrast_bounds.R's own sb_share_bounds_path() prefers
## the in-repo copy and treats the Dropbox tree as the fallback; this file
## used to prefer the Dropbox copy, which meant the tests could silently
## exercise a stale mirror while the driver used the repo file. Same order
## as the code under test now.
repo_sb <- file.path(here, "share_bounds.R")
dropbox_sb <- path.expand(file.path(
  "~/Dropbox/Research_Hub/Projects/ConjointStructural/2608_issues",
  "Yiqing/bound_for_share/code/share_bounds.R"))
share_bounds_path <- if (file.exists(repo_sb)) repo_sb else dropbox_sb
if (!file.exists(share_bounds_path)) {
  stop("Cannot locate the audited share_bounds.R (looked at ", dropbox_sb,
       " and ", repo_sb, ").")
}
cat("audited source : ", share_bounds_path, "\n", sep = "")
source(share_bounds_path)
source(file.path(here, "orientation_spec.R"))
source(file.path(here, "contrast_bounds.R"))
source(file.path(here, "br_progressivity_contrasts.R"))
cat("under test     : ", file.path(here, "contrast_bounds.R"), "\n\n", sep = "")

## --------------------------------------------------------------------
## Synthetic assembled fits
## --------------------------------------------------------------------
## Task-level rows, several tasks per respondent, mu_hat constant within
## respondent (as the real out-of-fold construction makes it).
make_fit <- function(p = 7L, q = 2L, K = 3L, n_resp = 51L, tasks = 4L,
                     mu_scale = 0.05, a_scale = 0.10, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  attr_names <- paste0("b", seq_len(p))
  rid <- rep(sprintf("r%03d", seq_len(n_resp)), each = tasks)
  mu_resp <- matrix(stats::rnorm(n_resp * p, sd = mu_scale), n_resp, p)
  ## Give a few coordinates a clear modal side and one a mixed side.
  mu_resp[, 1L] <- mu_resp[, 1L] - 3 * mu_scale
  mu_resp[, 2L] <- mu_resp[, 2L] + 3 * mu_scale
  mu_hat <- mu_resp[rep(seq_len(n_resp), each = tasks), , drop = FALSE]
  colnames(mu_hat) <- attr_names
  A_folds <- lapply(seq_len(K), function(k)
    matrix(stats::rnorm(p * q, sd = a_scale), p, q))
  sd_dx_folds <- lapply(seq_len(K), function(k)
    stats::runif(p, 0.5, 2))
  out <- list(
    A_folds = A_folds, sd_dx_folds = sd_dx_folds,
    mu_hat = mu_hat, respondent_id = rid,
    K = as.integer(K), q = as.integer(q),
    attr_names = attr_names,
    analysis_signature = "synthetic-test-signature")
  class(out) <- c("scmix_nested_assembled", "list")
  out
}

## A calibration object shaped like sb_zero_floor()'s return value:
## per-coordinate apparent dispersions, plus the quantile it derives.
make_calibration <- function(fit, R_rows = 20L, gamma = 0.05, scale = 0.02,
                             n_folds = 2L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  p <- length(fit$attr_names)
  ## Row count must equal R * length(folds_use): the replication index is
  ## reconstructed from those two on artifacts saved before the matching
  ## fix, and a mismatch is a hard error rather than a silent regroup.
  R_rows <- as.integer(ceiling(R_rows / n_folds) * n_folds)
  R <- as.integer(R_rows / n_folds)
  draws <- matrix(abs(stats::rnorm(R_rows * p, sd = scale)), R_rows, p)
  colnames(draws) <- fit$attr_names
  cal <- list(draws = draws, R = R, folds_use = seq_len(n_folds),
              gamma = gamma, n_epochs = 4000L, attr_names = fit$attr_names,
              analysis_signature = fit$analysis_signature)
  ## Matched statistic: average within replication, then quantile across.
  cal$floor <- sb_matched_floor(cal, gamma = gamma,
                                attr_names = fit$attr_names)
  cal$floor_pooled <- stats::setNames(
    apply(draws, 2L, stats::quantile, probs = 1 - gamma, names = FALSE,
          type = 1L), fit$attr_names)
  cal
}

identity_contrasts <- function(fit) {
  p <- length(fit$attr_names)
  C <- diag(1, p)
  dimnames(C) <- list(fit$attr_names, fit$attr_names)
  C
}

NUMCOL <- c("mean_abs", "mean_oriented", "fitted_s", "floor", "s_bar",
            "share_limit_s0", "lower_bound_gauss", "lower_bound_cantelli")

compare_tables <- function(ct, bt) {
  ## ct = sb_contrast_bounds() output, bt = sb_bounds_table() output.
  devs <- vapply(NUMCOL, function(cl) maxdev(ct[[cl]], bt[[cl]]), numeric(1L))
  chr_ok <- identical(as.character(ct$coordinate), as.character(bt$coordinate)) &&
    identical(as.character(ct$contrast), as.character(bt$coordinate)) &&
    identical(as.character(ct$orientation_side), as.character(bt$orientation_side)) &&
    identical(as.logical(ct$all_one_sign), as.logical(bt$all_one_sign))
  list(max_dev = max(devs), devs = devs, chr_ok = chr_ok)
}

## ====================================================================
## CHECK 1 (ORACLE). c = e_j for every j must reproduce sb_bounds_table
## exactly, over several random fits, both floor modes, with and without
## an `orient` vector, and via the m = 1 single-contrast path.
## ====================================================================
cat("--- ORACLE: c = e_j reproduces sb_bounds_table ---\n")
tryCatch({
  oracle_worst <- 0
  oracle_chr_ok <- TRUE
  for (rep_i in 1:5) {
    fit <- make_fit(p = 7L, q = if (rep_i %% 2L) 1L else 3L, K = 2L + rep_i %% 3L,
                    seed = 1000L + rep_i)
    cal <- make_calibration(fit, seed = 2000L + rep_i)
    C <- identity_contrasts(fit)
    ## orientation vector covering only some coordinates, to exercise both
    ## the theta branch and the modal fallback
    th <- stats::rnorm(4L, sd = 0.05)
    names(th) <- fit$attr_names[c(1L, 3L, 5L, 7L)]

    for (om in c("none", "orient")) {
      or <- if (om == "orient") th else NULL
      bt <- sb_bounds_table(fit, cal$floor, orient = or)
      for (fm in c("draws", "coordinate_sum")) {
        ct <- suppressWarnings(
          sb_contrast_bounds(fit, C, cal, orient = or, floor_mode = fm,
                             gamma = cal$gamma))
        cmp <- compare_tables(ct, bt)
        oracle_worst <- max(oracle_worst, cmp$max_dev)
        oracle_chr_ok <- oracle_chr_ok && cmp$chr_ok
      }
      ## Same thing through the named-list interface, one contrast at a
      ## time (m = 1, exercising the padding path).
      for (j in seq_along(fit$attr_names)) {
        nm <- fit$attr_names[[j]]
        one <- stats::setNames(list(stats::setNames(1, nm)), nm)
        ct1 <- suppressWarnings(
          sb_contrast_bounds(fit, one, cal, orient = or,
                             floor_mode = "coordinate_sum"))
        cmp1 <- compare_tables(ct1, bt[j, , drop = FALSE])
        oracle_worst <- max(oracle_worst, cmp1$max_dev)
        oracle_chr_ok <- oracle_chr_ok && cmp1$chr_ok
      }
    }
  }
  ok("oracle: e_j numeric columns match to 1e-12", oracle_worst <= 1e-12,
     sprintf("max deviation = %.3e", oracle_worst))
  ok("oracle: e_j identifier / side / one-sign columns match", oracle_chr_ok)
}, error = guard("CHECK 1 oracle"))

## ====================================================================
## CHECK 2. Independent recomputation of the projection for a genuine
## multi-coordinate contrast (the e_j oracle cannot see a transposed C).
## ====================================================================
cat("\n--- projection against an independent reference ---\n")
tryCatch({
  ## Independent re-implementation of the CERTIFIED lower bound: the
  ## one-sign case is the exact value at s_bar (eta is decreasing there);
  ## the mixed-sign case is the per-respondent monotone envelope over the
  ## same geometric cells. A grid minimum would be an UPPER bound for the
  ## infimum, which is the defect the certified version replaces.
  ref_bound <- function(m, s_c, floor_c, orient_val = NULL, grid_n = 60L,
                        env_cells = 8L) {
    oriented_negative <- if (!is.null(orient_val)) orient_val < 0 else
      mean(m < 0) >= 0.5
    mo <- if (oriented_negative) m else -m
    sbar <- max(s_c, floor_c)
    eta0 <- mean(mo < 0) + 0.5 * mean(mo == 0)
    LG <- if (all(mo < 0)) {
      mean(stats::pnorm(-mo / sbar))
    } else {
      pts <- sbar * exp(seq(log(1 / 50), 0, length.out = env_cells + 1L))
      cell <- function(s_neg, s_pos) {
        v <- numeric(length(mo))
        neg <- mo < 0
        v[neg] <- stats::pnorm(-mo[neg] / s_neg)
        if (is.finite(s_pos)) v[!neg] <- stats::pnorm(-mo[!neg] / s_pos)
        mean(v)
      }
      min(c(cell(pts[1L], Inf),
            vapply(seq_len(env_cells),
                   function(g) cell(pts[g + 1L], pts[g]), numeric(1L)),
            eta0))
    }
    LC <- mean(ifelse(mo < 0, mo^2 / (sbar^2 + mo^2), 0))
    list(orientation_side = if (oriented_negative) "negative" else "positive",
         all_one_sign = all(mo < 0),
         mean_abs = mean(abs(m)), mean_oriented = mean(mo),
         fitted_s = s_c, floor = floor_c, s_bar = sbar,
         share_limit_s0 = eta0, lower_bound_gauss = LG,
         lower_bound_cantelli = LC)
  }
  proj_worst <- 0
  proj_chr_ok <- TRUE
  for (rep_i in 1:4) {
    fit <- make_fit(p = 6L, q = if (rep_i %% 2L) 1L else 2L, K = 3L,
                    seed = 3000L + rep_i)
    cal <- make_calibration(fit, seed = 4000L + rep_i)
    cvec <- stats::setNames(stats::rnorm(6L), fit$attr_names)
    cvec2 <- stats::setNames(c(1, -1), fit$attr_names[c(6L, 1L)])
    cl <- list(cA = cvec, cB = cvec2)
    ct <- suppressWarnings(
      sb_contrast_bounds(fit, cl, cal, floor_mode = "coordinate_sum"))

    ## hand projection
    rid <- as.character(fit$respondent_id)
    keep <- !duplicated(rid)
    M <- as.matrix(fit$mu_hat)[keep, , drop = FALSE]
    Araw <- lapply(seq_len(fit$K), function(k)
      as.matrix(fit$A_folds[[k]]) / as.numeric(fit$sd_dx_folds[[k]]))
    for (jj in seq_along(cl)) {
      cfull <- stats::setNames(numeric(6L), fit$attr_names)
      v <- cl[[jj]]; cfull[names(v)] <- v
      m_ref <- as.numeric(M %*% cfull)
      s_ref <- mean(vapply(Araw, function(A)
        sqrt(sum(crossprod(A, cfull)^2)), numeric(1L)))
      fl_ref <- sum(abs(cfull) * cal$floor[fit$attr_names])
      r <- ref_bound(m_ref, s_ref, fl_ref)
      proj_worst <- max(proj_worst,
        maxdev(unlist(r[NUMCOL]), unlist(ct[jj, NUMCOL])))
      proj_chr_ok <- proj_chr_ok &&
        identical(r$orientation_side, ct$orientation_side[[jj]]) &&
        identical(r$all_one_sign, ct$all_one_sign[[jj]])
    }
  }
  ok("projection matches hand-computed m_c and ||A' c||", proj_worst <= 1e-12,
     sprintf("max deviation = %.3e", proj_worst))
  ok("projection: side / one-sign columns match reference", proj_chr_ok)
}, error = guard("CHECK 2 projection"))

## ====================================================================
## CHECK 3. Scale equivariance. The SHARE is invariant under c -> t c for
## t > 0; the mean and dispersion columns scale linearly by t.
## ====================================================================
cat("\n--- scale equivariance (c vs 2c) ---\n")
tryCatch({
  fit <- make_fit(p = 6L, q = 2L, K = 3L, seed = 5001L)
  cal <- make_calibration(fit, seed = 5002L)
  cvec <- stats::setNames(c(1, -0.5, 0.25, 0, 0.75, -1), fit$attr_names)
  tb_s <- suppressWarnings(sb_contrast_bounds(
    fit, list(c1 = cvec, c2 = 2 * cvec, c10 = 10 * cvec), cal,
    floor_mode = "coordinate_sum"))
  INVAR <- c("share_limit_s0", "lower_bound_gauss", "lower_bound_cantelli")
  LINEAR <- c("mean_abs", "mean_oriented", "fitted_s", "floor", "s_bar")
  inv_dev <- max(
    maxdev(tb_s[1L, INVAR], tb_s[2L, INVAR]),
    maxdev(tb_s[1L, INVAR], tb_s[3L, INVAR]))
  lin_dev <- max(
    maxdev(2 * unlist(tb_s[1L, LINEAR]), unlist(tb_s[2L, LINEAR])),
    maxdev(10 * unlist(tb_s[1L, LINEAR]), unlist(tb_s[3L, LINEAR])))
  ok("scale: bound and share limit invariant under c -> t c", inv_dev <= 1e-12,
     sprintf("max deviation = %.3e", inv_dev))
  ok("scale: mean / dispersion / floor / ceiling scale by t", lin_dev <= 1e-12,
     sprintf("max deviation = %.3e", lin_dev))
  ok("scale: modal side unchanged",
     length(unique(tb_s$orientation_side)) == 1L &&
       length(unique(tb_s$all_one_sign)) == 1L,
     paste(tb_s$orientation_side, collapse = "/"))
}, error = guard("CHECK 3 scale"))

## ====================================================================
## CHECK 4. Sign flip. c -> -c flips orientation_side and leaves every bound
## and every magnitude column unchanged (the oriented series is the same
## series). Tested under both the modal rule and an `orient` vector.
## ====================================================================
cat("\n--- sign flip (c vs -c) ---\n")
tryCatch({
  flip_dev <- 0
  flip_side_ok <- TRUE
  for (rep_i in 1:4) {
    f2 <- make_fit(p = 6L, q = 2L, K = 3L, seed = 6000L + rep_i)
    c2 <- make_calibration(f2, seed = 6500L + rep_i)
    cv <- stats::setNames(stats::rnorm(6L), f2$attr_names)
    th <- stats::setNames(stats::rnorm(6L, sd = 0.05), f2$attr_names)
    for (om in c("none", "orient")) {
      or <- if (om == "orient") th else NULL
      tt <- suppressWarnings(sb_contrast_bounds(
        f2, list(pos = cv, neg = -cv), c2, orient = or,
        floor_mode = "coordinate_sum"))
      flip_dev <- max(flip_dev, maxdev(tt[1L, NUMCOL], tt[2L, NUMCOL]))
      flip_side_ok <- flip_side_ok &&
        tt$orientation_side[[1L]] != tt$orientation_side[[2L]] &&
        identical(tt$all_one_sign[[1L]], tt$all_one_sign[[2L]])
    }
  }
  ok("sign flip: all numeric columns unchanged", flip_dev <= 1e-12,
     sprintf("max deviation = %.3e", flip_dev))
  ok("sign flip: orientation_side flips, all_one_sign unchanged", flip_side_ok)

  ## A PRESPECIFIED side survives the flip: reversing the contrast reverses
  ## the quantity, so an explicitly reversed contrast must be declared
  ## explicitly too. This is the invariance that still has to hold once
  ## orientation stops being read off the fit (work package 3).
  spec_pn <- orient_spec(
    orient_row("pos", "negative", "test: declared for the contrast"),
    orient_row("neg", "positive", "test: declared for the reversed contrast"))
  f3 <- make_fit(p = 6L, q = 2L, K = 3L, seed = 6600L)
  c3 <- make_calibration(f3, seed = 6700L)
  cv3 <- stats::setNames(stats::rnorm(6L), f3$attr_names)
  tp <- suppressWarnings(sb_contrast_bounds(
    f3, list(pos = cv3, neg = -cv3), c3, floor_mode = "coordinate_sum",
    orientation = spec_pn))
  ok("prespecified sides survive an explicit contrast reversal",
     tp$orientation_side[[1L]] == "negative" &&
       tp$orientation_side[[2L]] == "positive" &&
       all(tp$orientation_source == "prespecified"),
     paste(tp$orientation_side, collapse = "/"))
  ok("the reversed pair still shares every numeric bound column",
     maxdev(tp[1L, NUMCOL], tp[2L, NUMCOL]) <= 1e-12,
     sprintf("max deviation = %.3e", maxdev(tp[1L, NUMCOL], tp[2L, NUMCOL])))
}, error = guard("CHECK 4 sign flip"))

## ====================================================================
## CHECK 5. Degenerate direction: A_raw' c = 0 exactly. The floor branch
## must take over and the bound must be finite.
## ====================================================================
cat("\n--- degenerate direction (A' c = 0) ---\n")
tryCatch({
  fd <- make_fit(p = 5L, q = 2L, K = 3L, seed = 7001L)
  ## make raw loadings identical on coordinates 1 and 2 in every fold
  for (k in seq_len(fd$K)) {
    fd$sd_dx_folds[[k]] <- rep(1, 5L)
    fd$A_folds[[k]][2L, ] <- fd$A_folds[[k]][1L, ]
  }
  cd <- make_calibration(fd, seed = 7002L)
  cdeg <- stats::setNames(c(1, -1), fd$attr_names[1:2])
  td <- suppressWarnings(sb_contrast_bounds(fd, list(dead = cdeg), cd,
                                            floor_mode = "coordinate_sum"))
  deg_floor <- sum(abs(c(1, 1)) * cd$floor[fd$attr_names[1:2]])
  ok("degenerate: fitted_s is exactly zero", td$fitted_s[[1L]] == 0,
     sprintf("fitted_s = %.3e", td$fitted_s[[1L]]))
  ok("degenerate: s_bar falls back to the floor",
     td$floor[[1L]] > 0 && abs(td$s_bar[[1L]] - deg_floor) <= 1e-15,
     sprintf("s_bar = %.6f, floor = %.6f", td$s_bar[[1L]], deg_floor))
  ok("degenerate: both bounds finite, in [0, 1], not NA",
     all(is.finite(c(td$lower_bound_gauss[[1L]], td$lower_bound_cantelli[[1L]],
                     td$share_limit_s0[[1L]]))) &&
       td$lower_bound_gauss[[1L]] >= 0 && td$lower_bound_gauss[[1L]] <= 1 &&
       td$lower_bound_cantelli[[1L]] >= 0 && td$lower_bound_cantelli[[1L]] <= 1,
     sprintf("L_G = %.4f, L_C = %.4f", td$lower_bound_gauss[[1L]],
             td$lower_bound_cantelli[[1L]]))
}, error = guard("CHECK 5 degenerate"))

## ====================================================================
## CHECK 6. Name-lookup failures raise clear errors, everywhere.
## ====================================================================
cat("\n--- name-lookup discipline ---\n")
tryCatch({
  err_of <- function(expr) tryCatch({ force(expr); NA_character_ },
                                    error = function(e) conditionMessage(e))
  fn <- make_fit(p = 5L, K = 2L, seed = 8001L)
  cn <- make_calibration(fn, seed = 8002L)

  e1 <- err_of(sb_contrast_bounds(
    fn, list(bad = stats::setNames(c(1, -1), c("b1", "not_a_coord"))), cn,
    floor_mode = "coordinate_sum"))
  ok("unknown coordinate name is rejected",
     !is.na(e1) && grepl("contrast names must all be coordinates", e1,
                         fixed = TRUE),
     substr(e1, 1L, 60L))

  e2 <- err_of(sb_contrast_bounds(fn, list(z = stats::setNames(numeric(5L),
                                                              fn$attr_names)),
                                  cn, floor_mode = "coordinate_sum"))
  ok("all-zero contrast is rejected",
     !is.na(e2) && grepl("All-zero contrast", e2, fixed = TRUE),
     substr(e2, 1L, 60L))

  e3 <- err_of(sb_contrast_bounds(fn, list(u = c(1, 2, 3)), cn,
                                  floor_mode = "coordinate_sum"))
  ok("unnamed short contrast is rejected",
     !is.na(e3) && grepl("unnamed", e3, fixed = TRUE), substr(e3, 1L, 60L))

  e4 <- err_of(sb_contrast_bounds(fn, matrix(1, 5L, 2L), cn,
                                  floor_mode = "coordinate_sum"))
  ok("contrast matrix without column names is rejected",
     !is.na(e4) && grepl("column names", e4, fixed = TRUE),
     substr(e4, 1L, 60L))

  e5 <- err_of(sb_contrast_bounds(
    fn, list(a = stats::setNames(1, "b1")),
    stats::setNames(c(0.1, 0.2), c("b1", "b2")), floor_mode = "coordinate_sum"))
  ok("incomplete per-coordinate floors are rejected",
     !is.na(e5) && grepl("per-coordinate floors must cover", e5, fixed = TRUE),
     substr(e5, 1L, 60L))

  e6 <- err_of(br_progressivity_contrasts(c("rate_L10", "revenue_score")))
  ok("br constructor rejects a fit missing rate brackets",
     !is.na(e6) && grepl("not found in the fit's attr_names", e6, fixed = TRUE),
     substr(e6, 1L, 60L))

  e7 <- err_of(sb_contrast_bounds(
    fn, list(a = stats::setNames(1, "b1")), cn$floor, floor_mode = "draws"))
  ok("floor_mode='draws' without draws is rejected",
     !is.na(e7) && grepl("`draws` matrix", e7, fixed = TRUE),
     substr(e7, 1L, 60L))

  wmsg <- tryCatch({
    sb_contrast_bounds(fn, list(a = stats::setNames(c(1, -1), c("b1", "b2"))),
                       cn, floor_mode = "coordinate_sum")
    NA_character_
  }, warning = function(w) conditionMessage(w))
  ok("floor_mode='coordinate_sum' warns on a multi-coordinate contrast",
     !is.na(wmsg) && grepl("APPROXIMATION", wmsg, fixed = TRUE),
     substr(wmsg, 1L, 60L))
}, error = guard("CHECK 6 name lookup"))

## ====================================================================
## CHECK 7. Floor projection. The draws envelope must dominate the exact
## contrast floor computed from a known loading matrix, and must reduce
## to the per-coordinate floor on e_j.
## ====================================================================
cat("\n--- floor projection ---\n")
tryCatch({
  ff <- make_fit(p = 6L, K = 2L, seed = 9001L)
  cf <- make_calibration(ff, R_rows = 40L, seed = 9002L)
  Cf <- identity_contrasts(ff)
  fl_ej <- sb_contrast_floor(Cf, cf, floor_mode = "draws", gamma = cf$gamma)
  ok("draws mode reproduces the MATCHED per-coordinate floor on e_j",
     maxdev(fl_ej, cf$floor) <= 1e-15,
     sprintf("max deviation = %.3e", maxdev(fl_ej, cf$floor)))
  ok("the matched floor differs from the pre-audit pooled quantile",
     maxdev(cf$floor, cf$floor_pooled) > 0,
     sprintf("matched vs pooled max gap = %.3e",
             maxdev(cf$floor, cf$floor_pooled)))
  ## dominance: simulate signed loading rows whose norms are the recorded
  ## draws, and check the envelope quantile is >= the exact one.
  set.seed(9003L)
  cvecF <- stats::setNames(stats::rnorm(6L), ff$attr_names)
  Cf2 <- sb_as_contrast_matrix(list(cx = cvecF), ff$attr_names)
  env_floor <- sb_contrast_floor(Cf2, cf, floor_mode = "draws",
                                 gamma = cf$gamma)
  ## Matched exact statistic: the signed contrast norm per (rep, fold),
  ## averaged within replication, then quantiled across replications ---
  ## the same functional the envelope dominates.
  exact_row <- apply(cf$draws, 1L, function(nrm) {
    sgn <- sample(c(-1, 1), length(nrm), replace = TRUE)
    abs(sum(cvecF * sgn * nrm))
  })
  rep_idx <- rep(seq_len(cf$R), each = length(cf$folds_use))
  exact_draw <- vapply(split(exact_row, rep_idx), mean, numeric(1L))
  exact_floor <- stats::quantile(exact_draw, probs = 1 - cf$gamma,
                                 names = FALSE, type = 1L)
  ok("draws envelope dominates an exact signed contrast floor",
     env_floor[[1L]] >= exact_floor,
     sprintf("envelope = %.5f >= exact = %.5f", env_floor[[1L]], exact_floor))
}, error = guard("CHECK 7 floor projection"))

## ====================================================================
## CHECK 8. Plugin targets. For c = e_j the wrapped targets must be
## bit-identical to sb_make_targets()'s, in labels, value, d_mu and
## d_kappa --- this pins the chain-rule wrapper to the audited targets.
## ====================================================================
cat("\n--- plugin targets vs sb_make_targets ---\n")
tryCatch({
  ft <- make_fit(p = 5L, q = 2L, K = 3L, seed = 11001L)
  ct_cal <- make_calibration(ft, seed = 11002L)
  Ct <- identity_contrasts(ft)
  bt_t <- sb_bounds_table(ft, ct_cal$floor)
  ctb_t <- suppressWarnings(sb_contrast_bounds(ft, Ct, ct_cal,
                                               floor_mode = "draws",
                                               gamma = ct_cal$gamma))
  tg_ref <- sb_make_targets(bt_t, ft$attr_names)
  tg_new <- sb_contrast_targets(ctb_t, attr(ctb_t, "contrast_matrix"))
  lab_ok <- identical(names(tg_ref), names(tg_new))
  mu_eval <- as.matrix(ft$mu_hat)[!duplicated(as.character(ft$respondent_id)), ,
                                  drop = FALSE]
  tgt_dev <- 0
  for (nm in names(tg_ref)) {
    r1 <- tg_ref[[nm]](mu_eval, NULL, NULL, NULL, NULL, NULL, ft$attr_names)
    r2 <- tg_new[[nm]](mu_eval, NULL, NULL, NULL, NULL, NULL, ft$attr_names)
    tgt_dev <- max(tgt_dev,
                   maxdev(r1$value, r2$value),
                   maxdev(r1$d_mu, r2$d_mu),
                   maxdev(r1$d_kappa, r2$d_kappa))
    lab_ok <- lab_ok && identical(r1$labels, r2$labels) &&
      identical(r1$target_type, r2$target_type) &&
      identical(r1$sigma_invariant, r2$sigma_invariant) &&
      identical(dim(r1$d_mu), dim(r2$d_mu))
  }
  ok("targets: labels, types and gradient shapes match", lab_ok,
     sprintf("%d targets", length(tg_ref)))
  ok("targets: value / d_mu / d_kappa match to 1e-12", tgt_dev <= 1e-12,
     sprintf("max deviation = %.3e", tgt_dev))

  ## Chain rule against a numerical derivative, for a genuine contrast.
  cnum <- stats::setNames(stats::rnorm(5L), ft$attr_names)
  ctb_n <- suppressWarnings(sb_contrast_bounds(ft, list(g = cnum), ct_cal,
                                               floor_mode = "coordinate_sum"))
  tg_n <- sb_contrast_targets(ctb_n, attr(ctb_n, "contrast_matrix"))
  num_dev <- 0
  for (nm in names(tg_n)) {
    f <- tg_n[[nm]]
    base <- f(mu_eval, NULL, NULL, NULL, NULL, NULL, ft$attr_names)
    h <- 1e-6
    for (l in seq_len(ncol(mu_eval))) {
      mp <- mu_eval; mp[, l] <- mp[, l] + h
      mm <- mu_eval; mm[, l] <- mm[, l] - h
      num <- (f(mp, NULL, NULL, NULL, NULL, NULL, ft$attr_names)$value -
              f(mm, NULL, NULL, NULL, NULL, NULL, ft$attr_names)$value) / (2 * h)
      num_dev <- max(num_dev, maxdev(num, base$d_mu[, 1L, l]))
    }
  }
  ok("targets: analytic d_mu matches a central difference", num_dev <= 1e-5,
     sprintf("max deviation = %.3e", num_dev))
}, error = guard("CHECK 8 targets"))

## ====================================================================
## CHECK 9. Confidence-bound bookkeeping, against a stubbed scmix_dml.
## The real scmix_dml needs the real fit; this pins only the label
## extraction and the column assembly, which is the part that had to be
## re-expressed for contrasts.
## ====================================================================
cat("\n--- confidence-bound bookkeeping (stubbed scmix_dml) ---\n")
tryCatch({
  scmix_dml <- function(fit, targets, plugin_targets, mu_basis, ...) {
    labs <- names(plugin_targets)
    est <- se <- stats::setNames(numeric(length(labs)), labs)
    for (nm in labs) {
      r <- plugin_targets[[nm]](mu_eval, NULL, NULL, NULL, NULL, NULL,
                                fit$attr_names)
      est[[nm]] <- mean(r$value)
      se[[nm]] <- sqrt(mean(r$d_mu^2)) / sqrt(nrow(mu_eval))
    }
    list(estimate = est, diagnostic_se = se, status = "stubbed")
  }
  inf_cfg <- list(riesz_validation_fraction = 0.2,
                  riesz_equation_tolerance = 0.05,
                  ridge_sensitivity_tolerance = 0.10,
                  active_eigenvalue_min = 1e-6,
                  information_eigenvalue_min = 1e-8,
                  rank_tolerance = 1e-8)
  cb_ref <- sb_confidence_bounds(ft, bt_t, inf_cfg, seed = 20260826L)
  cb_new <- sb_contrast_confidence_bounds(ft, ctb_t, inf_cfg, seed = 20260826L)
  CBCOL <- c("gauss_onestep", "gauss_cond_l95", "cant_onestep",
             "cant_cond_l95")
  cb_dev <- max(vapply(CBCOL, function(cl) maxdev(cb_ref[[cl]], cb_new[[cl]]),
                       numeric(1L)))
  ok("confidence columns match sb_confidence_bounds on e_j", cb_dev <= 1e-12,
     sprintf("max deviation = %.3e", cb_dev))
  ok("contrast_matrix attribute survives the confidence step",
     !is.null(attr(cb_new, "contrast_matrix")) &&
       identical(dim(attr(cb_new, "contrast_matrix")), c(5L, 5L)))
  ok("confidence step errors clearly without a contrast_matrix",
     grepl("missing its `contrast_matrix`",
           err_of(sb_contrast_confidence_bounds(
             ft, as.data.frame(as.list(ctb_t[1L, ])), inf_cfg, seed = 1L)),
           fixed = TRUE))
  rm(scmix_dml)
}, error = guard("CHECK 9 confidence"))

## ====================================================================
## CHECK 10. The br2017 progressivity contrasts.
## ====================================================================
cat("\n--- br2017 progressivity contrasts ---\n")
tryCatch({
  br_names <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
                "rate_175_375", "rate_375P", "revenue_score")
  brc <- br_progressivity_contrasts(br_names)
  ok("br: four contrasts built by name",
     identical(names(brc), c("top_minus_bottom", "slope", "slope_unit",
                             "mean_rate")))
  ok("br: top_minus_bottom is rate_375P minus rate_L10",
     identical(sort(names(brc$top_minus_bottom)),
               sort(c("rate_L10", "rate_375P"))) &&
       brc$top_minus_bottom[["rate_375P"]] == 1 &&
       brc$top_minus_bottom[["rate_L10"]] == -1)
  ok("br: slope weights sum to zero and are increasing in bracket",
     abs(sum(brc$slope)) < 1e-14 && !is.unsorted(brc$slope) &&
       identical(names(brc$slope), BR_RATE_BRACKETS),
     sprintf("w = %s", paste(sprintf("%+.4f", brc$slope), collapse = " ")))
  ok("br: slope reads as an OLS slope on bracket rank",
     abs(sum(brc$slope * seq_along(brc$slope)) - 1) < 1e-12,
     sprintf("sum(w * x) = %.12f", sum(brc$slope * seq_along(brc$slope))))
  ok("br: slope_unit has unit L2 norm and the same direction",
     abs(sqrt(sum(brc$slope_unit^2)) - 1) < 1e-12 &&
       maxdev(brc$slope_unit / sqrt(sum(brc$slope_unit^2)),
              brc$slope / sqrt(sum(brc$slope^2))) < 1e-12)
  ok("br: mean_rate is the equal-weight level contrast",
     abs(sum(brc$mean_rate) - 1) < 1e-14 &&
       length(unique(round(brc$mean_rate, 12))) == 1L)
  brc_log <- br_progressivity_contrasts(br_names, spacing = "log_midpoint")
  ok("br: log_midpoint spacing also yields an increasing zero-sum slope",
     abs(sum(brc_log$slope)) < 1e-14 && !is.unsorted(brc_log$slope))
  ok("br: extra contrasts are name-checked",
     grepl("not found in the fit's attr_names",
           err_of(br_progressivity_contrasts(
             br_names, extra = list(bad = stats::setNames(1, "nope")))),
           fixed = TRUE))

  ## End-to-end on a synthetic fit carrying the real br coordinate names,
  ## with the v2.1 theta pattern as the orientation vector.
  fbr <- make_fit(p = 7L, q = 1L, K = 3L, seed = 12001L)
  fbr$attr_names <- br_names
  colnames(fbr$mu_hat) <- br_names
  ## give the rate coordinates the v2.1 progressivity pattern in the mean
  th21 <- c(rate_L10 = -0.049, rate_10_35 = -0.035, rate_35_85 = -0.021,
            rate_85_175 = -0.001, rate_175_375 = 0.006, rate_375P = 0.011,
            revenue_score = 0.126)
  fbr$mu_hat <- fbr$mu_hat * 0.3 +
    matrix(th21, nrow(fbr$mu_hat), 7L, byrow = TRUE)
  cbr <- make_calibration(fbr, seed = 12002L)
  tbr <- suppressWarnings(sb_contrast_bounds(fbr, brc, cbr, orient = th21,
                                             floor_mode = "draws",
                                             gamma = cbr$gamma))
  ok("br: end-to-end table has one row per contrast, all finite",
     nrow(tbr) == 4L && identical(tbr$contrast, names(brc)) &&
       all(is.finite(tbr$lower_bound_gauss)) &&
       all(is.finite(tbr$lower_bound_cantelli)),
     sprintf("L_G = %s", paste(sprintf("%.3f", tbr$lower_bound_gauss),
                               collapse = " ")))
  ok("br: slope and slope_unit give identical bounds (scale invariance)",
     maxdev(tbr[tbr$contrast == "slope", INVAR],
            tbr[tbr$contrast == "slope_unit", INVAR]) <= 1e-12,
     sprintf("max deviation = %.3e",
             maxdev(tbr[tbr$contrast == "slope", INVAR],
                    tbr[tbr$contrast == "slope_unit", INVAR])))
  ok("br: progressivity contrasts orient positive under the v2.1 thetas",
     all(tbr$orientation_side[tbr$contrast %in%
                          c("top_minus_bottom", "slope")] == "positive"),
     paste(tbr$contrast, tbr$orientation_side, sep = "=", collapse = " "))
}, error = guard("CHECK 10 br2017"))

## --------------------------------------------------------------------
cat(sprintf("\n%d passed, %d failed\n", .n_pass, .n_fail))
quit(status = if (.n_fail > 0L) 1L else 0L)
