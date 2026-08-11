test_that("Quarto book includes all chapters", {
  skip_on_cran()
  root <- tryCatch(testthat::test_path("..", ".."), error = function(e) NULL)
  if (is.null(root)) testthat::skip("cannot locate package root")
  tut <- file.path(root, "tutorial")
  if (!dir.exists(tut)) testthat::skip("tutorial/ not present in this checkout")
  yml <- file.path(tut, "_quarto.yml")
  if (!file.exists(yml)) testthat::skip("_quarto.yml missing")
  lines <- readLines(yml, warn = FALSE)
  chapters <- grep("\\.qmd\\s*$", lines, value = TRUE)
  ## 13 entries: index + 01-installation + 02-sanity + 03-gs + 04-br +
  ##             05-sw + 06-bs + 07-estimands + 08-plot-options +
  ##             09-advanced-options + 10-mixed-logit + references +
  ##             changelog
  expect_equal(length(chapters), 13L,
               info = paste(chapters, collapse = "\n"))
  expect_true(any(grepl("03-example-gs", chapters)))
  expect_true(any(grepl("04-example-br", chapters)))
  expect_true(any(grepl("05-example-sw", chapters)))
  expect_true(any(grepl("06-example-bs", chapters)))
  expect_true(any(grepl("07-estimands", chapters)))
  expect_true(any(grepl("09-advanced-options", chapters)))
  expect_true(any(grepl("10-mixed-logit", chapters)))
})

test_that("full book renders (skipped without quarto)", {
  skip_on_cran()
  skip_if_not_installed("quarto")
  qbin <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
  if (is.null(qbin) || !nzchar(qbin)) testthat::skip("quarto CLI not available")
  root <- tryCatch(testthat::test_path("..", ".."), error = function(e) NULL)
  if (is.null(root)) testthat::skip("cannot locate package root")
  tut <- file.path(root, "tutorial")
  if (!dir.exists(tut)) testthat::skip("tutorial/ not present in this checkout")
  res <- tryCatch(
    quarto::quarto_render(input = tut, quiet = TRUE),
    error = function(e) e
  )
  expect_false(inherits(res, "error"))
  bookdir <- file.path(tut, "_book")
  if (dir.exists(bookdir)) {
    htmls <- list.files(bookdir, pattern = "\\.html$")
    expect_true(length(htmls) >= 9L)
  }
})
