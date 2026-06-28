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

test_that("plot_importance applies xlim as a coord_cartesian zoom", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggridges")
  fit <- .get_toy_fit()
  g0 <- plot_importance(fit)
  expect_s3_class(g0, "ggplot")
  ## xlim is applied as a coord zoom (preserving the densities and the
  ## mean-percent labels past the cap), not a clipping scale limit.
  g1 <- plot_importance(fit, xlim = c(0, 50))
  expect_s3_class(g1, "ggplot")
  expect_s3_class(g1$coordinates, "CoordCartesian")
  expect_equal(g1$coordinates$limits$x, c(0, 50))
  expect_no_error(suppressMessages(ggplot2::ggplot_build(g1)))
})
