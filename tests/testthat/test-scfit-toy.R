test_that("scfit() runs end-to-end on a tiny toy DGP", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  toy <- .make_toy_long(M = 150L, T_i = 5L, p = 3L, p_Z = 2L, seed = 1L)
  fit <- scfit(
    y ~ a1 + a2 + a3 | z1 + z2,
    data = toy$data,
    respondent = "rid", task = "tid", profile = "pos",
    K = 2L, n_epochs = 200L, seed = 1L,
    parallel = FALSE, verbose = FALSE
  )

  expect_s3_class(fit, "sc_fit")
  expect_named(fit$theta, c("a1", "a2", "a3"))
  expect_equal(dim(fit$vcov), c(3L, 3L))
  expect_equal(dim(fit$vcov_iid), c(3L, 3L))
  expect_equal(ncol(fit$beta_hat), 3L)
  expect_equal(nrow(fit$beta_hat), 150L * 5L)
  expect_length(fit$fold_id, 150L * 5L)
  expect_equal(length(fit$loss_traces), 2L)
  expect_true(is.finite(fit$se_ratio_dml_iid$mean))

  # S3 methods
  expect_output(print(fit), "sc_fit")
  cf <- coef(fit)
  expect_identical(cf, fit$theta)
  vv <- vcov(fit)
  expect_identical(vv, fit$vcov)
  # symmetric
  expect_true(max(abs(vv - t(vv))) < 1e-10)
})
