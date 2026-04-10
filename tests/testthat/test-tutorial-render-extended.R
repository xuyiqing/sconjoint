test_that("extended Quarto book includes 13 chapters (stub 12 excluded)", {
  skip_on_cran()
  root <- tryCatch(testthat::test_path("..", ".."), error = function(e) NULL)
  if (is.null(root)) testthat::skip("cannot locate package root")
  tut <- file.path(root, "tutorial")
  if (!dir.exists(tut)) testthat::skip("tutorial/ not present in this checkout")
  yml <- file.path(tut, "_quarto.yml")
  if (!file.exists(yml)) testthat::skip("_quarto.yml missing")
  lines <- readLines(yml, warn = FALSE)
  chapters <- grep("\\.qmd\\s*$", lines, value = TRUE)
  ## Every listed chapter (index + 01..11 + 13 = 13 entries; 12 stub excluded)
  expect_equal(length(chapters), 13L,
               info = paste(chapters, collapse = "\n"))
  expect_true(any(grepl("08-case-saha-weeks", chapters)))
  expect_true(any(grepl("09-case-graham-svolik", chapters)))
  expect_true(any(grepl("10-case-bechtel-scheve", chapters)))
  expect_false(any(grepl("12-advanced", chapters)))
})

test_that("full extended book renders (skipped without quarto)", {
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
    expect_true(length(htmls) >= 13L)
  }
})
