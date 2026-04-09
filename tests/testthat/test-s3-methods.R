test_that("summary.sc_fit produces a coefficient table with clustered SE", {
  fit <- .get_toy_fit()
  s <- summary(fit)
  expect_s3_class(s, "sc_fit_summary")
  expect_true(is.data.frame(s$coefficients))
  expect_setequal(colnames(s$coefficients),
                  c("estimate", "std_error", "z_value", "p_value", "ci_lo", "ci_hi"))
  expect_equal(nrow(s$coefficients), length(fit$theta))
  expect_output(print(s), "sc_fit summary")
})

test_that("predict.sc_fit(newdata = NULL) returns beta_hat", {
  fit <- .get_toy_fit()
  p0 <- predict(fit)
  expect_identical(p0, fit$beta_hat)
})

test_that("predict.sc_fit errors cleanly on non-NULL newdata", {
  fit <- .get_toy_fit()
  expect_error(
    predict(fit, newdata = data.frame()),
    "M5\\.a"
  )
})

test_that("plot.sc_fit and autoplot.sc_fit return ggplot objects", {
  skip_if_not_installed("ggplot2")
  fit <- .get_toy_fit()
  g1 <- plot(fit, which = "beta_ridgelines")
  expect_s3_class(g1, "ggplot")
  g2 <- plot(fit, which = "loss_trace")
  expect_s3_class(g2, "ggplot")
  g3 <- ggplot2::autoplot(fit)
  expect_s3_class(g3, "ggplot")
})
