## Tests for the integrated-likelihood mixed-logit estimator (scmix).
##
## Ground-truth strategy: simulate from the maintained model
## beta_i = mu(Z_i) + A u_i and check (a) the quadrature grid,
## (b) the Fisher identity, (c) orthogonality of the theta score,
## (d) recovery of the loading scale and location at loose tolerances,
## (e) polarization boundary handling.  The heavier recovery /
## coverage evidence lives in dev/sim-mixed-facevalidity.R.

test_that("Gauss-Hermite grid integrates Gaussian moments exactly", {
  gh <- sconjoint:::.sc_gh_grid(q = 1L, n_nodes = 15L)
  expect_equal(sum(gh$w), 1, tolerance = 1e-12)
  expect_equal(sum(gh$w * gh$U[, 1]), 0, tolerance = 1e-10)
  expect_equal(sum(gh$w * gh$U[, 1]^2), 1, tolerance = 1e-10)
  expect_equal(sum(gh$w * gh$U[, 1]^4), 3, tolerance = 1e-8)

  gh2 <- sconjoint:::.sc_gh_grid(q = 2L, n_nodes = 9L)
  expect_equal(sum(gh2$w), 1, tolerance = 1e-12)
  expect_equal(colSums(gh2$w * gh2$U^2), c(1, 1), tolerance = 1e-8)
  expect_equal(sum(gh2$w * gh2$U[, 1] * gh2$U[, 2]), 0, tolerance = 1e-10)
})

## The shared fixture (.mk_mixed_fixture / .fit_mixed_fixture) lives in
## helper-mixed-fixture.R so test-mixed-quantities.R can reuse it.

test_that("scmix runs, returns sane structure, and A is scale-recovered", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  expect_s3_class(fit, "scmix")
  expect_equal(ncol(fit$mu_hat), 2L)
  expect_equal(fit$N, 250L)
  ## rotation-invariant loading scale within +-60% at this tiny size
  sd_hat <- sqrt(diag(Reduce(`+`, lapply(fit$A_folds, tcrossprod)) /
                        length(fit$A_folds)))
  sd_true <- sqrt(diag(tcrossprod(fw$fx$A_true)))
  expect_true(all(abs(sd_hat - sd_true) < 0.6 * pmax(sd_true, 0.3)))
})

test_that("Fisher identity: score equals numeric d logL / d mu", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  worst <- sconjoint:::.scmix_check_fisher(fw$fit, n_check = 3L, coord = 1L)
  expect_lt(worst, 1e-6)
})

test_that("theta score is orthogonal: O(delta^2) vs O(delta) plug-in", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  chk <- sconjoint:::.scmix_check_orthogonality(
    fw$fit, deltas = c(0.2, 0.1), coord = 1L, n_bins = 15L, M = 200L, seed = 2L)
  ## plug-in moves one-for-one; orthogonal estimate moves by far less
  expect_equal(chk$plugin_shift, c(0.2, 0.1), tolerance = 1e-10)
  expect_true(all(abs(chk$orth_shift) < 0.25 * abs(chk$plugin_shift)))
})

test_that("scmix_theta is near the truth with sane inference output", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  truth <- colMeans(fw$fx$mu_fun(matrix(stats::runif(2e5, -1, 1), ncol = 2)))
  th <- scmix_theta(fit, n_bins = 15L, M = 200L, seed = 2L)
  ## loose per-draw check at this tiny size; the systematic
  ## de-attenuation-vs-pooled evidence is the replicated simulation's
  ## job (dev/sim-mixed-facevalidity.R), not a single-draw unit test's
  expect_true(all(abs(th$estimate - truth) < 4 * th$se))
  expect_true(all(th$se > 0))
  expect_true(all(is.finite(th$psi)))
  expect_gt(stats::sd(th$psi[, 1]), 0)
})

test_that("polarization respects [0,1] and floors zero-variance coords", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  pol <- suppressWarnings(
    scmix_polarization(fw$fit, n_bins = 15L, M = 200L, seed = 2L))
  ok <- !is.na(pol$estimate)
  expect_true(all(pol$estimate[ok] > -0.02 & pol$estimate[ok] < 1.02))
  ## forcing an absurd floor: every coordinate floored -> all NA + warning
  expect_warning(
    pol10 <- scmix_polarization(fw$fit, n_bins = 15L, M = 200L, seed = 2L,
                                sd_floor = 10),
    "floor"
  )
  expect_true(all(is.na(pol10$estimate)))
})

test_that("counterfactual share accepts named contrasts and stays in [0,1]", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  vc <- scmix_counterfactual(fw$fit, contrast = c(a1yes = 1, a2yes = -1),
                             n_bins = 15L, M = 200L, seed = 2L)
  expect_true(vc$estimate > 0 && vc$estimate < 1)
  expect_error(
    scmix_counterfactual(fw$fit, contrast = c(bogus = 1)),
    "unknown contrast"
  )
})

test_that("print methods run quietly", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  expect_output(print(fw$fit), "Integrated-likelihood")
  th <- scmix_theta(fw$fit, n_bins = 10L, M = 100L, seed = 2L)
  expect_output(print(th), "orthogonal estimate")
})

test_that("loading score matches finite-difference d logL / dA", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  sc <- sconjoint:::.scmix_scores(fit)
  eps <- 1e-5
  worst <- 0
  for (i in 1:2) {
    for (kr in 1:2) {
      f_up <- fit; f_dn <- fit
      k_fold <- sc$fold_resp[i]
      dA <- matrix(0, 2, 1); dA[kr, 1] <- eps
      f_up$A_folds[[k_fold]] <- fit$A_folds[[k_fold]] + dA
      f_dn$A_folds[[k_fold]] <- fit$A_folds[[k_fold]] - dA
      ll_up <- sconjoint:::.scmix_scores(f_up)$loglik[i]
      ll_dn <- sconjoint:::.scmix_scores(f_dn)$loglik[i]
      fd <- (ll_up - ll_dn) / (2 * eps)
      worst <- max(worst, abs(fd - sc$S_A[i, kr]))
    }
  }
  expect_lt(worst, 1e-6)
})

test_that("estimates are invariant to a global loading sign flip", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  f2 <- fit
  f2$A_folds <- lapply(fit$A_folds, function(A) -A)
  th1 <- scmix_theta(fit, n_bins = 15L, M = 400L, seed = 2L)
  th2 <- scmix_theta(f2, n_bins = 15L, M = 400L, seed = 2L)
  expect_equal(unname(th1$estimate), unname(th2$estimate), tolerance = 1e-10)
  expect_equal(unname(th1$se), unname(th2$se), tolerance = 1e-10)
})
