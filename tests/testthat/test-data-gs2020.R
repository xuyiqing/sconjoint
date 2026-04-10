test_that("gs2020 loads with expected columns and size", {
  data(gs2020, package = "sconjoint")
  expect_s3_class(gs2020, "data.frame")
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "federal", "immigration", "tax", "abortion",
                    "undem", "cand_party", "resp_pid", "resp_ideo",
                    "age") %in% names(gs2020)))
  expect_equal(nrow(gs2020), 200L * 8L * 2L)
  expect_identical(attr(gs2020, "synthetic"), TRUE)
  f <- system.file("data", "gs2020.rda", package = "sconjoint")
  if (nzchar(f)) {
    expect_lt(file.info(f)$size, 500L * 1024L)
  }
})
