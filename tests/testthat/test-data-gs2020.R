test_that("gs2020 loads with expected columns and size", {
  data(gs2020, package = "sconjoint")
  expect_s3_class(gs2020, "data.frame")
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "copartisan", "p1", "p2", "dem_code", "cand_sex",
                    "cand_race", "cand_pro",
                    "resp_ideo", "resp_pid", "resp_trump", "resp_age",
                    "resp_female", "resp_race_black", "resp_race_asian",
                    "resp_race_other", "resp_educ", "resp_income",
                    "resp_auth", "resp_knowledge") %in% names(gs2020)))
  expect_equal(nrow(gs2020), 41314L)
  expect_equal(length(unique(gs2020$respondent)), 1605L)
})
