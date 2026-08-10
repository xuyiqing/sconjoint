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

## Small shared fixture: N x T mixed-logit conjoint in long format.
.mk_mixed_fixture <- function(N = 250L, T_i = 6L, seed = 5L) {
  withr::local_preserve_seed()
  set.seed(seed)
  z <- matrix(stats::runif(N * 2, -1, 1), N, 2)
  mu_fun <- function(z) cbind(0.7 + 0.4 * z[, 1], -0.8 + 0.5 * z[, 2])
  A_true <- matrix(c(0.8, 0.5), 2, 1)
  beta <- mu_fun(z) + stats::rnorm(N) %*% t(A_true)
  a1 <- sample(c("no", "yes"), 2 * N * T_i, TRUE)
  a2 <- sample(c("no", "yes"), 2 * N * T_i, TRUE)
  odd <- seq(1L, 2 * N * T_i, by = 2L)
  dxm <- cbind((a1[odd] == "yes") - (a1[odd + 1L] == "yes"),
               (a2[odd] == "yes") - (a2[odd + 1L] == "yes"))
  rid <- rep(seq_len(N), each = T_i)
  pr <- stats::plogis(rowSums(dxm * beta[rid, , drop = FALSE]))
  yA <- stats::rbinom(N * T_i, 1, pr)
  list(
    data = data.frame(
      resp_id = rep(rid, each = 2L),
      task_id = rep(rep(seq_len(T_i), N), each = 2L),
      profile_id = rep(1:2, N * T_i),
      a1 = a1, a2 = a2,
      z1 = rep(z[rid, 1], each = 2L), z2 = rep(z[rid, 2], each = 2L),
      choice = as.vector(rbind(yA, 1L - yA))
    ),
    mu_fun = mu_fun, A_true = A_true, z = z
  )
}

.fit_mixed_fixture <- function() {
  fx <- .mk_mixed_fixture()
  fit <- scmix(choice ~ a1 + a2 | z1 + z2, fx$data,
               respondent = "resp_id", task = "task_id",
               profile = "profile_id",
               q = 1L, K = 2L, n_epochs = 150L, seed = 11L)
  list(fit = fit, fx = fx)
}

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
  pol <- scmix_polarization(fw$fit, n_bins = 15L, M = 200L, seed = 2L)
  expect_true(all(pol$estimate > -0.02 & pol$estimate < 1.02))
  ## forcing an absurd floor triggers the warning path
  expect_warning(
    scmix_polarization(fw$fit, n_bins = 15L, M = 200L, seed = 2L,
                       sd_floor = 10),
    "floored"
  )
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
