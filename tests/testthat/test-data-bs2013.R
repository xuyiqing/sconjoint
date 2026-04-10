test_that("bs2013 loads with expected columns and size", {
  data(bs2013, package = "sconjoint")
  expect_s3_class(bs2013, "data.frame")
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "distribution", "enforcement", "monitoring",
                    "participation", "sanctions", "cost_usd",
                    "resp_female", "age", "resp_ideo")
                  %in% names(bs2013)))
  expect_equal(nrow(bs2013), 200L * 6L * 2L)
  expect_true(is.numeric(bs2013$cost_usd))
  expect_identical(attr(bs2013, "synthetic"), TRUE)
  f <- system.file("data", "bs2013.rda", package = "sconjoint")
  if (nzchar(f)) {
    expect_lt(file.info(f)$size, 500L * 1024L)
  }
})
