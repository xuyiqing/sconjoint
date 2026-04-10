test_that("sc_subgroup matches reference on a logical mask", {
  fit <- .get_toy_fit()
  N <- nrow(fit$beta_hat)
  mask <- seq_len(N) %% 2L == 0L
  q <- sc_subgroup(fit, mask)
  ref <- .ref_subgroup(fit$beta_hat, fit$respondent_id, which(mask))
  expect_s3_class(q, "sc_quantity")
  expect_equal(q$estimate$theta, ref$theta, tolerance = 1e-12)
  expect_equal(q$estimate$se, ref$se, tolerance = 1e-6)
  expect_equal(q$details$subgroup_size, sum(mask))
})

test_that("sc_subgroup accepts a function of Z and a named list", {
  fit <- .get_toy_fit()
  f <- function(Z) Z[, 1] > 0
  q1 <- sc_subgroup(fit, f)
  expect_s3_class(q1, "sc_quantity")
  expect_true(q1$details$subgroup_size > 0L)

  q_list <- sc_subgroup(fit, list(lo = function(Z) Z[, 1] <= 0,
                                   hi = function(Z) Z[, 1] > 0))
  expect_type(q_list, "list")
  expect_named(q_list, c("lo", "hi"))
  expect_s3_class(q_list$lo, "sc_quantity")
  expect_s3_class(q_list$hi, "sc_quantity")
  N <- nrow(fit$beta_hat)
  expect_equal(q_list$lo$details$subgroup_size + q_list$hi$details$subgroup_size, N)
})

test_that("sc_subgroup errors on empty subgroup", {
  fit <- .get_toy_fit()
  N <- nrow(fit$beta_hat)
  expect_error(sc_subgroup(fit, rep(FALSE, N)), "empty")
})
