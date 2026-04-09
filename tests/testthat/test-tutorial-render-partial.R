test_that("partial Quarto book renders (skipped on CRAN / no quarto)", {
  skip_on_cran()
  skip_if_not_installed("quarto")
  qbin <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
  if (is.null(qbin) || !nzchar(qbin)) testthat::skip("quarto CLI not available")
  root <- tryCatch(testthat::test_path("..", ".."), error = function(e) NULL)
  if (is.null(root)) testthat::skip("cannot locate package root")
  tut <- file.path(root, "tutorial")
  if (!dir.exists(tut)) testthat::skip("tutorial/ not present in this checkout")
  ## Render only the tiny quickstart chapter to keep runtime low.
  res <- tryCatch(
    quarto::quarto_render(
      input = file.path(tut, "02-quickstart.qmd"),
      quiet = TRUE
    ),
    error = function(e) e
  )
  expect_false(inherits(res, "error"))
})
