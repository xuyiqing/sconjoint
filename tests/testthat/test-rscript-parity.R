test_that("vignettes/rscript/*.R mirrors match tutorial/*.qmd code blocks", {
  skip_on_cran()
  root <- tryCatch(testthat::test_path("..", ".."), error = function(e) NULL)
  if (is.null(root)) testthat::skip("cannot locate package root")
  check_script <- file.path(root, "inst", "dev", "check-rscript-parity.R")
  if (!file.exists(check_script)) {
    testthat::skip("check-rscript-parity.R not installed")
  }
  rs_dir <- file.path(root, "vignettes", "rscript")
  if (!dir.exists(rs_dir)) {
    testthat::skip("vignettes/rscript/ not present")
  }
  out <- system2("Rscript", c(shQuote(check_script), shQuote(root)),
                 stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  expect_equal(as.integer(status), 0L, info = paste(out, collapse = "\n"))
})
