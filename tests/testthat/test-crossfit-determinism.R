test_that("scfit() gives bit-exact coef() across 1, 2, 4 cores", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_on_cran()

  toy <- .make_toy_long(M = 80L, T_i = 3L, p = 3L, p_Z = 2L, seed = 2L)
  args <- list(
    formula = y ~ a1 + a2 + a3 | z1 + z2,
    data = toy$data,
    respondent = "rid", task = "tid", profile = "pos",
    K = 4L, n_epochs = 50L, seed = 42L
  )

  fit1 <- do.call(scfit, c(args, list(parallel = FALSE)))
  fit2 <- do.call(scfit, c(args, list(parallel = TRUE,  n_cores = 2L)))
  fit4 <- do.call(scfit, c(args, list(parallel = TRUE,  n_cores = 4L)))

  expect_identical(coef(fit1), coef(fit2))
  expect_identical(coef(fit1), coef(fit4))
  expect_identical(fit1$beta_hat, fit2$beta_hat)
  expect_identical(fit1$beta_hat, fit4$beta_hat)
  expect_identical(fit1$vcov, fit2$vcov)
  expect_identical(fit1$vcov, fit4$vcov)
})
