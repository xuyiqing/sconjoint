test_that("sc_clusters is reproducible given a seed", {
  fit <- .get_toy_fit()
  q1 <- sc_clusters(fit, k = 3L, seed = 42L)
  q2 <- sc_clusters(fit, k = 3L, seed = 42L)
  expect_s3_class(q1, "sc_quantity")
  expect_equal(q1$estimate$cluster_assignment, q2$estimate$cluster_assignment)
  expect_equal(q1$estimate$sizes, q2$estimate$sizes)
  expect_equal(q1$estimate$total_within_ss, q2$estimate$total_within_ss,
               tolerance = 1e-10)
})

test_that("sc_clusters assignment matches pure-R reference up to label permutation", {
  fit <- .get_toy_fit()
  q <- sc_clusters(fit, k = 3L, seed = 1L, nstart = 25L)
  ref <- .ref_clusters(fit$beta_hat, k = 3L, seed = 1L, nstart = 25L)
  expect_true(.partition_equal(q$estimate$cluster_assignment, ref$assignment))
  expect_equal(sort(q$estimate$sizes), sort(ref$sizes))
  expect_equal(q$estimate$total_within_ss, ref$total_within_ss,
               tolerance = 1e-10)
})

test_that("sc_clusters centers are on the beta scale", {
  fit <- .get_toy_fit()
  q <- sc_clusters(fit, k = 2L, seed = 1L)
  expect_equal(dim(q$estimate$centers), c(2L, ncol(fit$beta_hat)))
  expect_equal(sum(q$estimate$sizes), nrow(fit$beta_hat))
})

test_that("sc_clusters errors on bad k", {
  fit <- .get_toy_fit()
  expect_error(sc_clusters(fit, k = 1L), "k")
  expect_error(sc_clusters(fit, k = nrow(fit$beta_hat) + 1L), "exceeds")
})
