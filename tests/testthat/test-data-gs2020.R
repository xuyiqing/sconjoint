test_that("gs2020 loads as the full-Z analysis frame", {
  data(gs2020, package = "sconjoint")
  expect_s3_class(gs2020, "data.frame")
  ## differenced attribute contrasts + the paper's 22 moderators + convenience
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "diff_respParty", "diff_p1_num",
                    "diff_dem_code_u_journalists", "diff_sex_Female",
                    "diff_pro_Lawyer",
                    "z_ideo", "z_pid7", "z_female", "E_ideal",
                    "dem_treat_journalists",
                    "ideo7", "pid7", "weight") %in% names(gs2020)))
  expect_equal(nrow(gs2020), 41314L)
  expect_equal(length(unique(gs2020$respondent)), 1605L)
  expect_equal(length(grep("^diff_", names(gs2020))), 30L)
})
