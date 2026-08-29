#!/usr/bin/env Rscript
## Regression tests for the fail-closed share-bound layer.
##   Rscript applications/R/test-share-bounds-release.R
## Prints PASS/FAIL per check; exits non-zero if anything fails.
##
## Written against the 2026-08-28 estimand/bounds audit. What each block
## pins:
##
##   A  matched calibration statistic -- average within replication, then
##      quantile across replications (audit P2), including reconstruction
##      from artifacts saved before the fix
##   B  certified lower envelope for the Gaussian bound -- a grid minimum
##      is an UPPER bound for the infimum and cannot be reported as a
##      lower bound (audit P1)
##   C  regime classification at both endpoints and the interior of the
##      intermediate ceiling window (audit P2)
##   D  the release gate: intermediate window withheld, mixed-sign
##      withheld, point-identified not reported, released columns NA
##   E  the mixed-sign boundary cell's discontinuity at m = 0 and the
##      reporting behaviour it forces (audit P1)
##   F  ceiling provenance on every row (audit P0)
##   G  an illustration that a zero-heterogeneity detection threshold is
##      not an upper confidence limit at nonzero true dispersion
##   H  prespecified orientation: it beats the fit, and an unprespecified
##      row cannot be displayed (work package 3)
##   I  table-level multiplicity: Bonferroni, monotone in K, invariant to
##      row order, and gated (work package 4)
##
## No package load and no real fit: synthetic assembled objects only.

options(stringsAsFactors = FALSE, warn = 1)
set.seed(20260828L)

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
section <- function(s) cat("\n--- ", s, " ---\n", sep = "")
err_of <- function(expr) tryCatch({ force(expr); NA_character_ },
                                  error = function(e) conditionMessage(e))

here <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) dirname(normalizePath(f[[1L]])) else getwd()
}, error = function(e) getwd())
source(file.path(here, "share_bounds.R"))
source(file.path(here, "orientation_spec.R"))

## --------------------------------------------------------------------
## Synthetic fixtures
## --------------------------------------------------------------------

## An assembled-shaped object whose per-coordinate fitted dispersion is
## exactly `s_target` and whose respondent means are exactly `M`.
make_fit <- function(M, s_target) {
  p <- ncol(M)
  n <- nrow(M)
  attr_names <- colnames(M)
  if (is.null(attr_names)) attr_names <- paste0("b", seq_len(p))
  colnames(M) <- attr_names
  A <- diag(s_target, nrow = p)          # rowSums(A^2) = s_target^2
  out <- list(A_folds = list(A, A), sd_dx_folds = list(rep(1, p), rep(1, p)),
              mu_hat = M, respondent_id = sprintf("r%03d", seq_len(n)),
              K = 2L, q = as.integer(p), attr_names = attr_names,
              analysis_signature = "synthetic")
  class(out) <- c("scmix_nested_assembled", "list")
  out
}

make_cal <- function(attr_names, per_coord_floor, R = 10L, n_folds = 2L,
                     gamma = 0.05) {
  p <- length(attr_names)
  ## Constant draws so the matched and pooled quantiles are both exactly
  ## the target floor: the regime tests need an exact ceiling.
  draws <- matrix(rep(per_coord_floor, each = R * n_folds), R * n_folds, p)
  colnames(draws) <- attr_names
  cal <- list(draws = draws, R = R, folds_use = seq_len(n_folds),
              gamma = gamma, attr_names = attr_names,
              analysis_signature = "synthetic")
  cal$floor <- sb_matched_floor(cal)
  cal
}

## ====================================================================
section("A. matched calibration statistic")
## ====================================================================

## Fold-level draws with a deliberately skewed second fold: pooling the
## fold-level values and quantiling the pool is NOT the same statistic as
## averaging within replication and quantiling across replications.
R <- 8L; n_folds <- 2L; p <- 3L
d <- matrix(0, R * n_folds, p)
for (r in seq_len(R)) {
  d[2L * r - 1L, ] <- c(0.10, 0.20, 0.30) * r / R
  d[2L * r, ]      <- c(0.90, 0.80, 0.70) * r / R
}
colnames(d) <- c("b1", "b2", "b3")
cal_a <- list(draws = d, R = R, folds_use = c(1L, 2L), gamma = 0.05,
              attr_names = colnames(d))

reps <- sb_calibration_reps(cal_a)
hand <- t(vapply(seq_len(R), function(r)
  colMeans(d[c(2L * r - 1L, 2L * r), , drop = FALSE]), numeric(p)))
ok("reps: rows are per-replication fold averages",
   identical(dim(reps), c(R, p)) && max(abs(reps - hand)) == 0,
   sprintf("%d x %d, max deviation = 0", nrow(reps), ncol(reps)))

matched <- sb_matched_floor(cal_a)
hand_matched <- apply(hand, 2L, stats::quantile, probs = 0.95, names = FALSE,
                      type = 1L)
pooled <- apply(d, 2L, stats::quantile, probs = 0.95, names = FALSE,
                type = 1L)
ok("matched floor = quantile across replications of the fold average",
   max(abs(as.numeric(matched) - hand_matched)) == 0,
   sprintf("matched = %s", paste(signif(matched, 4), collapse = ", ")))
ok("matched floor differs from the pre-audit pooled quantile",
   max(abs(as.numeric(matched) - pooled)) > 1e-6,
   sprintf("pooled = %s", paste(signif(pooled, 4), collapse = ", ")))
ok("matched floor carries the coordinate names",
   identical(names(matched), colnames(d)))

bad <- cal_a; bad$R <- 5L
ok("an inconsistent replication count is a hard error, not a regroup",
   grepl("Cannot reconstruct the replication index",
         err_of(sb_calibration_reps(bad)), fixed = TRUE),
   substr(err_of(sb_calibration_reps(bad)), 1L, 52L))

## ====================================================================
section("B. certified lower envelope vs the grid minimum")
## ====================================================================

## Dense reference for inf_{s in (0, sbar]} eta(s).
dense_inf <- function(mo, sbar, n = 200001L) {
  grid <- sbar * exp(seq(log(1e-6), 0, length.out = n))
  min(vapply(grid, function(s) mean(stats::pnorm(-mo / s)), numeric(1L)))
}

## A three-mean mixed-sign configuration whose interior dip of eta sits
## between the coarse grid's points: the 60-point grid minimum overstates
## the infimum by 6.7e-4, so it is not a lower bound.
mo3 <- c(0.459, -0.164, 0.651)
sbar3 <- 0.627
env3 <- sb_gauss_lower_envelope(mo3, sbar3)
inf3 <- dense_inf(mo3, sbar3)
ok("certified envelope is a genuine LOWER bound for the infimum",
   env3$certified <= inf3 + 1e-12,
   sprintf("certified %.8f <= dense infimum %.8f", env3$certified, inf3))
ok("the grid minimum sits ABOVE the infimum (cannot be a lower bound)",
   env3$grid_min >= inf3 - 1e-12 && env3$grid_min > env3$certified,
   sprintf("grid min %.8f > certified %.8f (gap %.2e)",
           env3$grid_min, env3$certified, env3$gap))

## Adversarial sweep: over many random mixed-sign configurations the
## certified value must never exceed the dense infimum, and the grid
## minimum must overstate it somewhere by a visible margin.
set.seed(99L)
worst_violation <- -Inf
worst_grid_excess <- 0
for (i in 1:200) {
  m <- stats::rnorm(3L, sd = 0.4)
  if (all(m < 0) || all(m > 0)) next
  sb <- stats::runif(1L, 0.05, 1.5)
  e <- sb_gauss_lower_envelope(m, sb)
  di <- dense_inf(m, sb, n = 20001L)
  worst_violation <- max(worst_violation, e$certified - di)
  worst_grid_excess <- max(worst_grid_excess, e$grid_min - di)
}
ok("sweep: certified never exceeds the dense infimum",
   worst_violation <= 1e-10,
   sprintf("worst (certified - infimum) = %.3e over 200 configs",
           worst_violation))
ok("sweep: the grid minimum does overstate the infimum",
   worst_grid_excess > 1e-6,
   sprintf("worst (grid min - infimum) = %.3e", worst_grid_excess))

## One-sign case: eta is decreasing in s, so the infimum is exactly at
## s_bar and no envelope is needed.
mo1 <- c(-0.9, -0.4, -0.2)
ok("one-sign case: infimum is attained exactly at s_bar",
   abs(mean(stats::pnorm(-mo1 / sbar3)) - dense_inf(mo1, sbar3)) < 1e-12,
   sprintf("eta(s_bar) = %.10f", mean(stats::pnorm(-mo1 / sbar3))))

## ====================================================================
section("C. regime classification")
## ====================================================================

fl <- 0.10
ok("regime: fitted_s below the floor is 'floored'",
   sb_regime(0.099, fl) == "floored")
ok("regime: fitted_s exactly at the floor enters the intermediate window",
   sb_regime(fl, fl) == "intermediate_ceiling_window")
ok("regime: interior of the window is the intermediate window",
   sb_regime(1.5 * fl, fl) == "intermediate_ceiling_window")
ok("regime: fitted_s just below twice the floor is still the window",
   sb_regime(2 * fl - 1e-12, fl) == "intermediate_ceiling_window")
ok("regime: fitted_s at twice the floor is point-identified",
   sb_regime(2 * fl, fl) == "point_identified")
ok("regime: a missing ceiling is its own regime, not NA",
   sb_regime(0.05, NA_real_) == "no_ceiling" &&
     sb_regime(NA_real_, fl) == "no_ceiling")
ok("release: a missing ceiling fails closed",
   grepl("^withheld \\(no ceiling available",
         sb_release_status("no_ceiling", TRUE)),
   sb_release_status("no_ceiling", TRUE))

## ====================================================================
section("D. release gate")
## ====================================================================

nm <- c("floored_one_sign", "window_one_sign", "pointid_one_sign",
        "floored_mixed")
## Means: three one-sign coordinates and one mixed-sign coordinate.
n <- 60L
M <- cbind(floored_one_sign = -abs(stats::rnorm(n, 0.5, 0.05)),
           window_one_sign  = -abs(stats::rnorm(n, 0.5, 0.05)),
           pointid_one_sign = -abs(stats::rnorm(n, 0.5, 0.05)),
           floored_mixed    = stats::rnorm(n, 0, 0.4))
## Fitted dispersion is common across coordinates in this fixture, so the
## regimes are separated through the per-coordinate floor instead.
fit_d <- make_fit(M, s_target = 0.20)
floors_d <- c(floored_one_sign = 0.40,   # fitted 0.20 < floor
              window_one_sign  = 0.15,   # 0.15 <= 0.20 < 0.30
              pointid_one_sign = 0.05,   # 0.20 >= 0.10
              floored_mixed    = 0.40)
## Every row is prespecified here, so section D isolates the REGIME logic;
## the orientation gate has its own section below.
spec_d <- do.call(orient_spec, lapply(nm, function(x)
  orient_row(x, "negative", "test fixture: declared by design")))
tb <- sb_bounds_table(fit_d, floors_d[nm], orientation = spec_d)
rel <- stats::setNames(tb$bound_release, tb$coordinate)
ok("floored one-sign coordinate is a conditional sensitivity bound",
   rel[["floored_one_sign"]] == "conditional_sensitivity", rel[["floored_one_sign"]])
ok("intermediate-window coordinate is withheld",
   grepl("no valid ceiling", rel[["window_one_sign"]], fixed = TRUE),
   rel[["window_one_sign"]])
ok("point-identified coordinate is not reported as a bound",
   grepl("^not_reported", rel[["pointid_one_sign"]]), rel[["pointid_one_sign"]])
ok("mixed-sign coordinate is withheld",
   grepl("mixed-sign", rel[["floored_mixed"]], fixed = TRUE),
   rel[["floored_mixed"]])
## A row failing two gates reports both, rather than picking one.
two <- sb_release_status("intermediate_ceiling_window", FALSE,
                         "fitted_mean_mode")
ok("a row failing several gates reports every reason",
   grepl("no valid ceiling", two, fixed = TRUE) &&
     grepl("orientation not prespecified", two, fixed = TRUE) &&
     grepl("mixed-sign", two, fixed = TRUE), two)

## Release columns, through the confidence step with a stubbed scmix_dml.
mu_eval <- as.matrix(fit_d$mu_hat)
scmix_dml <- function(fit, targets, plugin_targets, ...) {
  labs <- names(plugin_targets)
  est <- se <- stats::setNames(numeric(length(labs)), labs)
  for (lb in labs) {
    r <- plugin_targets[[lb]](mu_eval, NULL, NULL, NULL, NULL, NULL,
                              fit$attr_names)
    est[[lb]] <- mean(r$value)
    se[[lb]] <- sqrt(mean(r$d_mu^2)) / sqrt(nrow(mu_eval))
  }
  list(estimate = est, diagnostic_se = se, status = "stubbed")
}
inf_cfg <- list(riesz_validation_fraction = 0.2,
                riesz_equation_tolerance = 0.05,
                ridge_sensitivity_tolerance = 0.10,
                active_eigenvalue_min = 1e-6,
                information_eigenvalue_min = 1e-8,
                rank_tolerance = 1e-8)
cb <- sb_confidence_bounds(fit_d, tb, inf_cfg, seed = 1L)
released <- sb_is_released(cb$bound_release)
ok("released columns are NA wherever the gate did not release",
   all(is.na(cb$released_lower_bound_gauss[!released])) &&
     all(is.na(cb$released_gauss_cond_l95[!released])) &&
     all(is.na(cb$released_lower_bound_cantelli[!released])) &&
     all(is.na(cb$released_cant_cond_l95[!released])),
   sprintf("%d of %d rows released", sum(released), nrow(cb)))
ok("released columns carry the value wherever the gate released",
   all(cb$released_lower_bound_gauss[released] ==
         cb$lower_bound_gauss[released]),
   sprintf("released rows: %s",
           paste(cb$coordinate[released], collapse = ", ")))
ok("no one-step interval is emitted for a mixed-sign coordinate",
   all(is.na(cb$gauss_cond_l95[!cb$all_one_sign])),
   sprintf("mixed-sign rows: %d", sum(!cb$all_one_sign)))
ok("the diagnostic mixed-sign plug-in minimum is still recorded",
   all(is.finite(cb$gauss_onestep[!cb$all_one_sign])),
   sprintf("gauss_onestep = %s",
           paste(signif(cb$gauss_onestep[!cb$all_one_sign], 4),
                 collapse = ", ")))
rm(scmix_dml)

## ====================================================================
section("E. the mixed-sign boundary cell at m = 0")
## ====================================================================

## The boundary cell evaluates negative means at the cell's right endpoint
## and positive means at s -> 0+, i.e. at the trivial zero. That target
## jumps at m = 0: the left limit is 1/2 and the value is 0.
tgt <- sb_gauss_env_target(co = 1, s_neg = 0.1, s_pos = Inf, label = "c0")
val_at <- function(m) as.numeric(tgt(matrix(m, ncol = 1L), 0, NULL, NULL,
                                     NULL, NULL, "b1")$value)
eps <- 1e-9
v_minus <- val_at(-eps); v_zero <- val_at(0); v_plus <- val_at(eps)
ok("boundary cell: left limit at m -> 0- is 1/2",
   abs(v_minus - 0.5) < 1e-6, sprintf("value = %.10f", v_minus))
ok("boundary cell: value at m = 0 is 0 (a jump of 1/2)",
   v_zero == 0, sprintf("value = %.10f", v_zero))
ok("boundary cell: value at m -> 0+ is 0",
   v_plus == 0, sprintf("value = %.10f", v_plus))
d_minus <- tgt(matrix(-eps, ncol = 1L), 0, NULL, NULL, NULL, NULL,
               "b1")$d_mu[1L, 1L, 1L]
ok("boundary cell: the analytic derivative omits the crossing term",
   is.finite(d_minus),
   sprintf("d_mu at m -> 0- is the smooth branch (%.3e), not the jump",
           d_minus))
ok("the smooth one-step theorem therefore cannot cover the boundary cell",
   abs(v_minus - v_zero) > 0.49,
   "jump 0.5 at an interior point of the mu domain")

## ====================================================================
section("F. ceiling provenance")
## ====================================================================

need <- c("ceiling_source", "ceiling_status", "maintained_condition",
          "regime", "bound_release", "orientation_side", "orientation_rule",
          "branch_status")
ok("every provenance column is present on the bounds table",
   all(need %in% names(tb)),
   paste(setdiff(need, names(tb)), collapse = ", "))
ok("the maintained condition is stated literally on every row",
   all(nzchar(tb$maintained_condition)) &&
     all(grepl("s_true <= s_bar", tb$maintained_condition, fixed = TRUE)),
   substr(tb$maintained_condition[[1L]], 1L, 46L))
ok("the ceiling status is conditional_unverified",
   all(tb$ceiling_status == "conditional_unverified"))
ok("orientation_source and orientation_rule agree on every row",
   all((tb$orientation_source == "prespecified") ==
         grepl("^prespecified:", tb$orientation_rule)) &&
     all((tb$orientation_source != "prespecified") ==
           grepl("data-selected", tb$orientation_rule, fixed = TRUE)),
   paste(unique(tb$orientation_source), collapse = "/"))
ok("a prespecified row records the rationale it came from",
   all(!is.na(tb$orientation_rationale[tb$orientation_source ==
                                         "prespecified"])))
ok("the confidence step stamps the inferential population",
   all(grepl("superpopulation", cb$inference_population, fixed = TRUE)),
   substr(cb$inference_population[[1L]], 1L, 46L))
ok("the confidence step stamps the selection status",
   all(grepl("selected on the same", cb$selection_status, fixed = TRUE)))

## ====================================================================
section("G. a null threshold is not an upper confidence limit")
## ====================================================================

## ILLUSTRATION, deliberately simple: the estimator of the directional
## dispersion is modelled as s_hat = |N(s_true, tau)|, and the calibration
## takes the 95th percentile of s_hat under s_true = 0. An upper
## CONFIDENCE limit would cover s_true at least 95 percent of the time.
## A null detection threshold does not, and its coverage collapses as
## s_true grows -- which is why the bounds built on it are labelled
## conditional sensitivity calculations rather than confidence bounds.
tau <- 0.05
set.seed(7L)
null_draws <- abs(stats::rnorm(200000L, 0, tau))
thr <- stats::quantile(null_draws, 0.95, names = FALSE)
cov_at <- function(s_true) {
  s_hat <- abs(stats::rnorm(50000L, s_true, tau))
  mean(s_true <= pmax(s_hat, thr))     # the runner's s_bar = max(s_hat, thr)
}
grid_s <- c(0, 0.5, 1.0, 1.2, 1.5, 2.0) * thr
cov <- vapply(grid_s, cov_at, numeric(1L))
cat(sprintf("      s_true / threshold: %s\n",
            paste(sprintf("%.1f", grid_s / thr), collapse = "  ")))
cat(sprintf("      coverage of s_true: %s\n",
            paste(sprintf("%.3f", cov), collapse = "  ")))
ok("the null threshold covers a zero true dispersion by construction",
   cov[[1L]] >= 0.99, sprintf("coverage at s_true = 0 is %.3f", cov[[1L]]))
ok("coverage collapses as the true dispersion grows",
   cov[[length(cov)]] < cov[[1L]] - 0.4,
   sprintf("%.3f at s_true = 0 down to %.3f at 2x the threshold",
           cov[[1L]], cov[[length(cov)]]))
ok("s_bar = max(fitted, threshold) is NOT a 95 percent upper limit",
   any(cov < 0.95),
   sprintf("coverage at s_true = 1.2 * threshold is %.3f (< 0.95)",
           cov[[4L]]))

## ====================================================================
section("H. prespecified orientation (work package 3)")
## ====================================================================

nm_h <- c("declared_negative", "declared_positive", "undeclared")
n_h <- 40L
M_h <- cbind(declared_negative = -abs(stats::rnorm(n_h, 0.5, 0.05)),
             declared_positive =  abs(stats::rnorm(n_h, 0.5, 0.05)),
             undeclared        = -abs(stats::rnorm(n_h, 0.5, 0.05)))
fit_h <- make_fit(M_h, s_target = 0.20)
floors_h <- stats::setNames(rep(0.40, 3L), nm_h)   # all floored
spec_h <- orient_spec(
  orient_row("declared_negative", "negative", "test: declared by design"),
  orient_row("declared_positive", "positive", "test: declared by design"))

tb_h <- sb_bounds_table(fit_h, floors_h[nm_h], orientation = spec_h)
side_h <- stats::setNames(tb_h$orientation_side, tb_h$coordinate)
src_h <- stats::setNames(tb_h$orientation_source, tb_h$coordinate)
ok("prespecified rows carry orientation_source = 'prespecified'",
   src_h[["declared_negative"]] == "prespecified" &&
     src_h[["declared_positive"]] == "prespecified",
   paste(src_h, collapse = " / "))
ok("an undeclared row falls back to a data-selected source",
   src_h[["undeclared"]] %in% c("fitted_mean_mode", "onestep_theta_sign"),
   src_h[["undeclared"]])
ok("an undeclared row cannot be released",
   grepl("orientation not prespecified",
         tb_h$bound_release[tb_h$coordinate == "undeclared"]),
   tb_h$bound_release[tb_h$coordinate == "undeclared"])

## THE LOAD-BEARING CHECK. Flip every fitted mean and the declared sides
## must not move. A data-selected rule would flip both.
fit_flip <- make_fit(-M_h, s_target = 0.20)
tb_flip <- sb_bounds_table(fit_flip, floors_h[nm_h], orientation = spec_h)
side_flip <- stats::setNames(tb_flip$orientation_side, tb_flip$coordinate)
ok("flipping the fitted estimates cannot flip a prespecified side",
   side_flip[["declared_negative"]] == side_h[["declared_negative"]] &&
     side_flip[["declared_positive"]] == side_h[["declared_positive"]],
   sprintf("declared: %s -> %s", side_h[["declared_negative"]],
           side_flip[["declared_negative"]]))
ok("the data-selected side DOES flip, which is the defect being removed",
   side_flip[["undeclared"]] != side_h[["undeclared"]],
   sprintf("undeclared: %s -> %s", side_h[["undeclared"]],
           side_flip[["undeclared"]]))

## A one-step theta pointing the other way loses to the prespecification.
orient_wrong <- c(declared_negative = 5, declared_positive = -5,
                  undeclared = 1)
tb_w <- sb_bounds_table(fit_h, floors_h[nm_h], orient = orient_wrong,
                        orientation = spec_h)
ok("a conflicting one-step theta does not move a prespecified side",
   tb_w$orientation_side[tb_w$coordinate == "declared_negative"] ==
     "negative" &&
     tb_w$orientation_side[tb_w$coordinate == "declared_positive"] ==
     "positive")

## The runner-level gate.
err_h <- err_of(orient_require_prespecified(
  data.frame(coordinate = "x", orientation_source = "fitted_mean_mode",
             bound_release = "conditional_sensitivity",
             stringsAsFactors = FALSE), what = "test artifact"))
ok("the runner gate refuses a displayed row without prespecification",
   grepl("lack a prespecified orientation", err_h, fixed = TRUE),
   substr(err_h, 1L, 56L))
ok("the runner gate passes when nothing displayed is unprespecified",
   is.na(err_of(orient_require_prespecified(tb_h, what = "test artifact"))))
ok("a prespecified orientation needs a rationale",
   grepl("needs a rationale",
         err_of(orient_row("x", "negative", "")), fixed = TRUE))

## ====================================================================
section("I. table-level multiplicity (work package 4)")
## ====================================================================

mk_tab <- function(k) {
  data.frame(coordinate = paste0("c", seq_len(k)),
             bound_release = "conditional_sensitivity",
             gauss_onestep = 0.90, gauss_cond_l95 = 0.90 - 1.6449 * 0.02,
             cant_onestep = 0.70, cant_cond_l95 = 0.70 - 1.6449 * 0.03,
             stringsAsFactors = FALSE)
}
t3 <- sb_attach_multiplicity(mk_tab(3L), claim_family = "f")
t4 <- sb_attach_multiplicity(mk_tab(4L), claim_family = "f")
ok("Bonferroni: alpha_row = alpha_family / K",
   all(abs(t3$alpha_row - 0.05 / 3) < 1e-12) && all(t3$claim_family_K == 3L),
   sprintf("K = 3, alpha_row = %.5f", t3$alpha_row[[1L]]))
ok("adding a row to a family weakly lowers every adjusted endpoint",
   all(t4$gauss_cond_ladj <= t3$gauss_cond_ladj[[1L]] + 1e-12) &&
     t4$gauss_cond_ladj[[1L]] < t3$gauss_cond_ladj[[1L]],
   sprintf("K=3 -> %.5f, K=4 -> %.5f", t3$gauss_cond_ladj[[1L]],
           t4$gauss_cond_ladj[[1L]]))
ok("the adjusted endpoint is never above the pointwise one",
   all(t3$gauss_cond_ladj <= t3$gauss_cond_l95 + 1e-12) &&
     all(t3$cant_cond_ladj <= t3$cant_cond_l95 + 1e-12))
t3r <- sb_attach_multiplicity(mk_tab(3L)[c(3L, 1L, 2L), ],
                              claim_family = "f")
ok("reordering rows does not change adjusted endpoints",
   max(abs(sort(t3r$gauss_cond_ladj) - sort(t3$gauss_cond_ladj))) < 1e-12)
t_none <- sb_attach_multiplicity(mk_tab(3L))
ok("the default is pointwise, with no adjusted endpoint",
   all(t_none$multiplicity_method == "none (pointwise row description)") &&
     all(is.na(t_none$gauss_cond_ladj)) && all(is.na(t_none$claim_family)))
ok("a familywise claim is refused without adjustment metadata",
   grepl("Run sb_attach_multiplicity",
         err_of(sb_require_family_adjustment(mk_tab(3L), "f")), fixed = TRUE))
ok("a familywise claim is refused for an absent family",
   grepl("no rows carry claim_family",
         err_of(sb_require_family_adjustment(t3, "other")), fixed = TRUE))
ok("a familywise claim passes once the family is adjusted",
   is.na(err_of(sb_require_family_adjustment(t3, "f"))))

cat(sprintf("\n%d passed, %d failed\n", .n_pass, .n_fail))
if (.n_fail > 0L) quit(status = 1L)
