test_that(".sc_estimate_lambda returns finite, right-shape output", {
  set.seed(1)
  n <- 50
  p <- 3
  p_Z <- 2
  deltaX <- matrix(sample(c(-1, 0, 1), n * p, replace = TRUE), n, p)
  Z <- matrix(stats::rnorm(n * p_Z), n, p_Z)
  beta_hat <- matrix(stats::rnorm(n * p, sd = 0.3), n, p)

  obj <- sconjoint:::.sc_estimate_lambda(beta_hat, deltaX, Z, ridge_lambda = 1e-4)

  n_pairs <- p * (p + 1) / 2
  expect_equal(dim(obj$fitted), c(n, n_pairs))
  expect_true(all(is.finite(obj$fitted)))
  expect_equal(dim(obj$jk), c(n_pairs, 2L))
  expect_equal(obj$p, p)
  expect_true(all(is.finite(obj$prob_hat)))
  expect_true(all(obj$prob_hat >= 0 & obj$prob_hat <= 1))
})

test_that(".sc_reconstruct_lambda_inv is finite and invertible-looking", {
  set.seed(2)
  p <- 3
  n_pairs <- p * (p + 1) / 2
  v <- stats::rnorm(n_pairs)
  inv <- sconjoint:::.sc_reconstruct_lambda_inv(v, p = p, ridge = 1e-3)
  expect_equal(dim(inv), c(p, p))
  expect_true(all(is.finite(inv)))
})
