test_that("gs2020 loads with expected columns and size", {
  data(gs2020, package = "sconjoint")
  expect_s3_class(gs2020, "data.frame")
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "cand_party", "dem_code", "cand_sex", "cand_race",
                    "policy_prox", "resp_ideo", "resp_pid",
                    "resp_age") %in% names(gs2020)))
  expect_equal(nrow(gs2020), 20440L)
  f <- system.file("data", "gs2020.rda", package = "sconjoint")
  if (nzchar(f)) {
    expect_lt(file.info(f)$size, 500L * 1024L)
  }
})
