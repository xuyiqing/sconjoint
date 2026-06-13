## Tests for the population-level attribute-interaction extension
## (scfit(interactions = c("none", "lowrank", "explicit"))).
##
## Covers:
##   1. swap antisymmetry of the difference-of-quadratics head (and a
##      regression test that the paper-as-written ||V' deltaX||^2 form
##      is swap-INVARIANT, i.e. structurally broken for forced choice);
##   2. exact reduction: interactions = "none" is byte-identical to the
##      pre-extension code path (golden fixture), and the expanded
##      inference objects with q = 0 reduce exactly to the current ones;
##   3. recovery of a strong cross-attribute interaction (explicit sign
##      + magnitude, held-out log-loss improvement, low-rank cross-block
##      sign);
##   4. null safety: no contamination of main-effect theta / SEs under a
##      no-interaction DGP;
##   5. validity of the expanded orthogonal score (mean-zero at the
##      estimate; CI coverage in a Monte Carlo with emulated cross-fitted
##      nuisances -- no torch needed).

## --------------------------------------------------------------------------
## Local DGP helper: M respondents x T_i tasks, three binary factor
## attributes (levels "l0" < "l1", so l0 is the reference and the dummy is
## the l1 indicator), heterogeneous mains beta(Z), and a single
## cross-attribute interaction w_true * (XA1 XA2 - XB1 XB2) on the (a1, a2)
## dummy pair.  Returns the long data frame plus the simulation internals
## needed for held-out evaluation.
.make_int_dgp <- function(M, T_i, w_true = 0, seed = 1L) {
  set.seed(seed)
  p_Z <- 2L
  Z <- matrix(stats::rnorm(M * p_Z), M, p_Z)
  beta_true <- cbind(0.6 + 0.3 * Z[, 1], -0.5 + 0.4 * Z[, 2], 0.3)
  rid <- rep(seq_len(M), each = T_i)
  n <- M * T_i
  lvl <- function(n) sample(c("l0", "l1"), n, replace = TRUE)
  a1A <- lvl(n); a1B <- lvl(n)
  a2A <- lvl(n); a2B <- lvl(n)
  a3A <- lvl(n); a3B <- lvl(n)
  d <- function(x) as.numeric(x == "l1")
  XA <- cbind(d(a1A), d(a2A), d(a3A))
  XB <- cbind(d(a1B), d(a2B), d(a3B))
  idx <- rowSums((XA - XB) * beta_true[rid, ]) +
    w_true * (XA[, 1] * XA[, 2] - XB[, 1] * XB[, 2])
  y1 <- stats::rbinom(n, 1, stats::plogis(idx))
  long <- data.frame(
    rid = rep(rid, each = 2L),
    tid = rep(rep(seq_len(T_i), M), each = 2L),
    pos = rep(1:2, n),
    a1 = as.vector(rbind(a1A, a1B)),
    a2 = as.vector(rbind(a2A, a2B)),
    a3 = as.vector(rbind(a3A, a3B)),
    z1 = rep(Z[rid, 1], each = 2L),
    z2 = rep(Z[rid, 2], each = 2L),
    y = as.vector(rbind(y1, 1 - y1)),
    stringsAsFactors = FALSE
  )
  list(long = long, Z = Z, beta_true = beta_true,
       XA = XA, XB = XB, rid = rid, y = y1)
}

## Held-out log-loss of a fit on a fresh draw from the same DGP family:
## beta via the K-fold-averaged forward pass on the new Z, plus the fit's
## population-level interaction term when present.
.heldout_logloss <- function(fit, dgp_test) {
  Zdf <- as.data.frame(dgp_test$Z)
  names(Zdf) <- c("z1", "z2")
  beta_pred <- predict(fit, newdata = Zdf)          # M_test x p
  dX <- dgp_test$XA - dgp_test$XB
  lin <- rowSums(dX * beta_pred[dgp_test$rid, , drop = FALSE])
  if (!is.null(fit$interaction)) {
    Fte <- .sc_int_features(dgp_test$XA, dgp_test$XB, fit$interaction$pairs)
    lin <- lin + as.numeric(Fte %*% fit$interaction$w_hat)
  }
  pr <- stats::plogis(lin)
  pr <- pmin(pmax(pr, 1e-12), 1 - 1e-12)
  -mean(dgp_test$y * log(pr) + (1 - dgp_test$y) * log(1 - pr))
}

## ==========================================================================
## 1. Swap antisymmetry
## ==========================================================================

test_that("difference-of-quadratics flips sign under A<->B swap; ||V'deltaX||^2 does not", {
  set.seed(101)
  p <- 6L; r <- 2L; n <- 50L
  V <- matrix(rnorm(p * r), p, r)
  XA <- matrix(rbinom(n * p, 1L, 0.5), n, p)
  XB <- matrix(rbinom(n * p, 1L, 0.5), n, p)

  g <- function(X) rowSums((X %*% V)^2)
  contrib      <- g(XA) - g(XB)
  contrib_swap <- g(XB) - g(XA)
  expect_equal(contrib_swap, -contrib, tolerance = 1e-12)
  expect_gt(stats::sd(contrib), 0)  # non-degenerate

  ## Regression test for the broken paper-as-written form: the quadratic
  ## in the DIFFERENCE is invariant under the swap (deltaX -> -deltaX),
  ## so the implied P(choose A) after a swap is not 1 - P(choose A).  No
  ## profile-separable random-utility model generates it; this is why
  ## the package uses the difference of quadratics.
  broken      <- rowSums(((XA - XB) %*% V)^2)
  broken_swap <- rowSums(((XB - XA) %*% V)^2)
  expect_equal(broken_swap, broken, tolerance = 1e-12)   # swap-INVARIANT
  ## ... and therefore NOT antisymmetric (it is nonnegative, not odd):
  expect_gt(max(abs(broken + broken_swap)), 1e-6)
})

test_that("lowrank torch head is swap-antisymmetric end to end", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  set.seed(102)
  p <- 4L; p_Z <- 2L; n <- 30L
  torch::torch_manual_seed(7)
  net <- .sc_build_network(p = p, p_Z = p_Z, hidden = c(8L, 8L),
                           interactions = "lowrank", interaction_rank = 2L)
  XA <- matrix(rbinom(n * p, 1L, 0.5), n, p)
  XB <- matrix(rbinom(n * p, 1L, 0.5), n, p)
  Z  <- matrix(rnorm(n * p_Z), n, p_Z)
  tt <- function(m) torch::torch_tensor(m, dtype = torch::torch_float())
  idx_ab <- as.numeric(net$forward(tt(XA - XB), tt(Z), x_a = tt(XA), x_b = tt(XB)))
  idx_ba <- as.numeric(net$forward(tt(XB - XA), tt(Z), x_a = tt(XB), x_b = tt(XA)))
  ## Full index flips sign => P(A) + P(A after swap) = 1.
  expect_equal(idx_ba, -idx_ab, tolerance = 1e-5)
})

## ==========================================================================
## 2. Exact reduction
## ==========================================================================

test_that("interactions = 'none' reproduces the pre-extension fit byte-for-byte", {
  skip_on_cran()
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  toy <- .make_toy_long(M = 80L, T_i = 4L, p = 3L, p_Z = 2L, seed = 7L)
  fit <- scfit(y ~ a1 + a2 + a3 | z1 + z2, data = toy$data,
               respondent = "rid", task = "tid", profile = "pos",
               K = 2L, n_epochs = 120L, seed = 7L,
               interactions = "none")
  ## Golden fixture captured from the unmodified code path (commit
  ## 64a4024) on the same seed: the default must be a byte-identical
  ## no-op, not merely statistically equivalent.
  golden <- readRDS(test_path("fixtures", "golden-none-fit.rds"))
  expect_equal(fit$theta,        golden$theta,        tolerance = 1e-12)
  expect_equal(fit$vcov,         golden$vcov,         tolerance = 1e-12)
  expect_equal(fit$vcov_iid,     golden$vcov_iid,     tolerance = 1e-12)
  expect_equal(fit$beta_hat,     golden$beta_hat,     tolerance = 1e-12)
  expect_equal(fit$beta_hat_dnn, golden$beta_hat_dnn, tolerance = 1e-12)
  expect_equal(fit$sigma_prior,  golden$sigma_prior,  tolerance = 1e-12)
  expect_equal(fit$plugin,       golden$plugin,       tolerance = 1e-12)
  expect_equal(colSums(fit$correction),    golden$correction_colsums, tolerance = 1e-12)
  expect_equal(colSums(fit$influence_raw), golden$influence_colsums,  tolerance = 1e-12)
  expect_equal(predict(fit, type = "prob"), golden$pred_prob, tolerance = 1e-12)
  expect_null(fit$interaction)

  ## Omitting the argument entirely (the historical call signature) is
  ## the same code path.
  fit_default <- scfit(y ~ a1 + a2 + a3 | z1 + z2, data = toy$data,
                       respondent = "rid", task = "tid", profile = "pos",
                       K = 2L, n_epochs = 120L, seed = 7L)
  expect_identical(fit_default$theta, fit$theta)
  expect_identical(fit_default$beta_hat, fit$beta_hat)
})

test_that("expanded inference objects with q = 0 reduce exactly to the current ones", {
  ## The interaction inference path calls the SAME functions on
  ## [deltaX, F] and [beta_hat, w]; with zero interaction features these
  ## are the same matrices, so Lambda(Z), the influence function, theta,
  ## and the clustered vcov must agree exactly.  No torch needed.
  set.seed(103)
  n <- 200L; p <- 3L; p_Z <- 2L
  dX <- matrix(sample(c(-1, 0, 1), n * p, replace = TRUE), n, p)
  Z  <- matrix(rnorm(n * p_Z), n, p_Z)
  bh <- matrix(rnorm(n * p, sd = 0.3), n, p) + 0.4
  y  <- rbinom(n, 1, stats::plogis(rowSums(dX * bh)))
  rid <- rep(seq_len(n / 4), each = 4L)

  F0 <- matrix(numeric(0), n, 0L)            # q = 0 feature matrix
  W_exp     <- cbind(dX, F0)
  gamma_hat <- cbind(bh, matrix(numeric(0), n, 0L))

  lam  <- .sc_estimate_lambda(bh, dX, Z, ridge_lambda = 1e-4)
  lamE <- .sc_estimate_lambda(gamma_hat, W_exp, Z, ridge_lambda = 1e-4)
  expect_identical(lamE$fitted, lam$fitted)
  expect_identical(lamE$prob_hat, lam$prob_hat)

  inf  <- .sc_influence_function(bh, lam, dX, y, respondent_id = rid)
  infE <- .sc_influence_function(gamma_hat, lamE, W_exp, y, respondent_id = rid)
  expect_identical(infE$theta_hat, inf$theta_hat)
  expect_identical(infE$influence_raw, inf$influence_raw)

  vc  <- .sc_cluster_vcov(inf$influence_raw,  inf$theta_hat,  rid)
  vcE <- .sc_cluster_vcov(infE$influence_raw, infE$theta_hat, rid)
  expect_identical(vcE$vcov, vc$vcov)
})

## ==========================================================================
## 3. Recovery of a strong cross-attribute interaction
## ==========================================================================

test_that("explicit and lowrank heads recover a strong interaction; held-out log-loss improves", {
  skip_on_cran()
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  w_true <- 0.8
  dgp  <- .make_int_dgp(M = 600L, T_i = 8L, w_true = w_true, seed = 31L)
  dgp_test <- .make_int_dgp(M = 400L, T_i = 8L, w_true = w_true, seed = 32L)

  fit_args <- list(
    formula = y ~ a1 + a2 + a3 | z1 + z2, data = dgp$long,
    respondent = "rid", task = "tid", profile = "pos",
    K = 2L, n_epochs = 400L, seed = 5L, stage2 = "none"
  )
  f_none <- do.call(scfit, c(fit_args, list(interactions = "none")))
  f_exp  <- do.call(scfit, c(fit_args, list(interactions = "explicit",
                                            lambda_V = 1e-3)))
  f_lr   <- do.call(scfit, c(fit_args, list(interactions = "lowrank",
                                            interaction_rank = 2L,
                                            lambda_V = 1e-3)))

  ## --- explicit: sign + magnitude of the DML interaction coefficient ---
  j <- which(f_exp$interaction$feature_names == "a1l1:a2l1")
  expect_length(j, 1L)
  th_int <- f_exp$interaction$theta
  expect_gt(th_int[j], 0)                          # sign
  expect_lt(abs(th_int[j] - w_true), 0.35)         # magnitude
  ## the two null pairs stay comparatively small
  expect_lt(max(abs(th_int[-j])), abs(th_int[j]))

  ## --- held-out log-loss: both heads beat the main-effects fit ---------
  ll_none <- .heldout_logloss(f_none, dgp_test)
  ll_exp  <- .heldout_logloss(f_exp,  dgp_test)
  ll_lr   <- .heldout_logloss(f_lr,   dgp_test)
  expect_lt(ll_exp, ll_none)
  expect_lt(ll_lr,  ll_none)

  ## --- lowrank r = 2 captures it: VV' cross-block sign + DML estimate --
  W_avg <- f_lr$interaction$W_avg
  expect_gt(W_avg["a1l1", "a2l1"], 0)              # cross-block sign
  jl <- which(f_lr$interaction$feature_names == "a1l1:a2l1")
  expect_gt(f_lr$interaction$theta[jl], 0)
  expect_lt(abs(f_lr$interaction$theta[jl] - w_true), 0.35)

  ## --- the offset actually enters stored predictions -------------------
  base <- rowSums(f_exp$deltaX * f_exp$beta_hat)
  expect_equal(predict(f_exp, type = "logit"),
               as.numeric(base + f_exp$interaction$g_offset_task),
               tolerance = 1e-12)
})

## ==========================================================================
## 4. Null safety
## ==========================================================================

test_that("null DGP: interaction coefficients near zero, mains uncontaminated", {
  skip_on_cran()
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  dgp <- .make_int_dgp(M = 600L, T_i = 8L, w_true = 0, seed = 42L)
  fit_args <- list(
    formula = y ~ a1 + a2 + a3 | z1 + z2, data = dgp$long,
    respondent = "rid", task = "tid", profile = "pos",
    K = 2L, n_epochs = 300L, seed = 9L, stage2 = "none"
  )
  f_none <- do.call(scfit, c(fit_args, list(interactions = "none")))
  f_exp  <- do.call(scfit, c(fit_args, list(interactions = "explicit")))

  ## interaction coefficients ~ 0 (point estimates and z-scores)
  th_int <- f_exp$interaction$theta
  se_int <- f_exp$interaction$se
  expect_lt(max(abs(th_int)), 0.2)
  expect_lt(max(abs(th_int / se_int)), 3)

  ## Main-effect point estimates agree with the interactions = "none"
  ## fit within the sampling noise of the estimator DIFFERENCE.
  ## Calibration note (2026-06-12, seeds 41-44): the two estimators on
  ## the same data differ by sd ~ sqrt(se_exp^2 - se_none^2) ~ 0.06;
  ## observed max diffs across typical draws were 0.05-0.11.  A genuine
  ## contamination bug (e.g. mis-aligned w_fold or feature columns)
  ## moves theta by O(w-feature correlation), an order of magnitude
  ## more.
  expect_lt(max(abs(f_exp$theta - f_none$theta)), 0.15)

  ## Main-effect SEs inflate by the joint-estimation price, NOT
  ## arbitrarily.  Under this design the identified features are
  ## correlated with the mains (cor(F_kl, deltaX_k) = 0.577 for
  ## p = 1/2 binary dummies), so jointly estimating w widens the
  ## main-effect SEs by a VIF-like factor; observed ratios across
  ## seeds were a stable 1.6-1.75.  This is a property of the expanded
  ## estimand, not contamination -- a Monte Carlo with emulated first
  ## stages shows identical second-order bias for the expanded and
  ## non-expanded scores and correct coverage for both (see the score
  ## validity test below).  The band guards against both directions:
  ## a ratio near 1 would mean the interaction block is silently
  ## ignored; a blow-up would flag a Gram-conditioning bug.
  se_none <- sqrt(diag(f_none$vcov))
  se_exp  <- sqrt(diag(f_exp$vcov))
  expect_true(all(se_exp / se_none > 1.0 & se_exp / se_none < 2.2))

  ## true mains are recovered by both (E[beta] = (0.6, -0.5, 0.3))
  truth <- c(0.6, -0.5, 0.3)
  expect_lt(max(abs(f_exp$theta - truth)), 0.25)
  expect_lt(max(abs(f_none$theta - truth)), 0.25)
})

## ==========================================================================
## 5. Score validity of the expanded orthogonal score
## ==========================================================================

test_that("expanded orthogonal score: mean zero at the estimate; MC coverage of mains", {
  skip_on_cran()

  ## Monte Carlo over the expanded DML layer with emulated cross-fitted
  ## nuisances (first-stage estimates = truth + independent noise).  This
  ## isolates the new inference layer (expanded Lambda, expanded score)
  ## from torch training noise and keeps the runtime in seconds.
  ## Design: two binary attributes (p = 2 mains), q = 1 identified
  ## cross-attribute feature, true w = 0.5.
  n_rep <- 100L
  M <- 150L; T_i <- 5L
  beta_fun <- function(z) cbind(0.5 + 0.3 * z, -0.4 + 0 * z)
  w_true <- 0.5
  theta_true <- c(0.5, -0.4, w_true)   # E[beta1], E[beta2], w

  cover <- matrix(NA, n_rep, 3L)
  est   <- matrix(NA_real_, n_rep, 3L)
  set.seed(20260612)
  for (rep_i in seq_len(n_rep)) {
    z   <- rnorm(M)
    bt  <- beta_fun(z)
    rid <- rep(seq_len(M), each = T_i)
    n   <- M * T_i
    XA <- cbind(rbinom(n, 1L, 0.5), rbinom(n, 1L, 0.5))
    XB <- cbind(rbinom(n, 1L, 0.5), rbinom(n, 1L, 0.5))
    dX <- XA - XB
    Fq <- matrix(XA[, 1] * XA[, 2] - XB[, 1] * XB[, 2], n, 1L)
    idx <- rowSums(dX * bt[rid, ]) + w_true * Fq[, 1]
    y   <- rbinom(n, 1, stats::plogis(idx))

    ## Emulated cross-fitted first stage: independent estimation noise.
    bh <- bt[rid, ] + matrix(rnorm(M * 2, sd = 0.15), M, 2)[rid, ]
    wh <- w_true + rnorm(1, sd = 0.10)
    gamma_hat <- cbind(bh, wh)
    W_exp <- cbind(dX, Fq)
    Zmat  <- matrix(z[rid], n, 1L)

    lam <- .sc_estimate_lambda(gamma_hat, W_exp, Zmat, ridge_lambda = 1e-4)
    inf <- .sc_influence_function(gamma_hat, lam, W_exp, y, respondent_id = rid)
    vc  <- .sc_cluster_vcov(inf$influence_raw, inf$theta_hat, rid)
    th  <- inf$theta_hat
    se  <- vc$se
    est[rep_i, ]   <- th
    cover[rep_i, ] <- abs(th - theta_true) <= stats::qnorm(0.975) * se

    if (rep_i == 1L) {
      ## Score mean-zero at the estimate: with balanced T_i the
      ## respondent-weighted theta equals the raw score mean, so the
      ## centered expanded score has mean exactly 0 in every coordinate.
      expect_equal(unname(colMeans(inf$influence_raw)), unname(th),
                   tolerance = 1e-10)
    }
  }

  ## Estimator centered at the truth (MC error ~ se/sqrt(n_rep)).
  bias <- colMeans(est) - theta_true
  mc_se <- apply(est, 2L, stats::sd) / sqrt(n_rep)
  expect_true(all(abs(bias) < 4 * mc_se + 0.02))

  ## 95% CI coverage for the main-effect theta AND the interaction
  ## coefficient from the expanded score (binomial noise at n_rep = 100).
  cov_rate <- colMeans(cover)
  expect_true(all(cov_rate >= 0.87))
  expect_true(all(cov_rate <= 1.00))
})
