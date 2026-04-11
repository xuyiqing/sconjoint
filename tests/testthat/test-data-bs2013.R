test_that("bs2013 loads with expected columns and size", {
  data(bs2013, package = "sconjoint")
  expect_s3_class(bs2013, "data.frame")
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "cost_usd", "distribution", "participation",
                    "emissions", "sanctions", "monitoring",
                    "resp_female", "resp_age", "resp_ideo")
                  %in% names(bs2013)))
  expect_equal(nrow(bs2013), 20000L)
  expect_true(is.numeric(bs2013$cost_usd))
  f <- system.file("data", "bs2013.rda", package = "sconjoint")
  if (nzchar(f)) {
    expect_lt(file.info(f)$size, 500L * 1024L)
  }
})
