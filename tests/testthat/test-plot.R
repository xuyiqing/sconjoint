test_that("ridgeline plot has expected structure", {
  skip_if_not_installed("ggplot2")
  fit <- .get_toy_fit()
  g <- plot(fit, which = "beta_ridgelines")
  expect_s3_class(g, "ggplot")
  expect_true(length(g$layers) >= 1L)
})

test_that("loss trace plot has expected structure", {
  skip_if_not_installed("ggplot2")
  fit <- .get_toy_fit()
  g <- plot(fit, which = "loss_trace")
  expect_s3_class(g, "ggplot")
  expect_true(length(g$layers) >= 1L)
})
