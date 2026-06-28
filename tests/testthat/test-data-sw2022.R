test_that("sw2022 loads as the full-Z analysis frame", {
  data(sw2022, package = "sconjoint")
  expect_s3_class(sw2022, "data.frame")
  ## attribute dummies + the paper's 19 moderators + convenience factor
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "cand_genderMale", "cand_runYes",
                    "cand_agendaModerate.Changes", "cand_talentEmpathetic",
                    "cand_child1.child",
                    "gender_num", "age", "income", "party_Republican",
                    "party_Independent", "ideo_conservative", "gender_att",
                    "resp_party")
                  %in% names(sw2022)))
  expect_equal(nrow(sw2022), 7146L)
  expect_equal(ncol(sw2022), 37L)
  ## 13 attribute dummies
  expect_equal(length(grep("^cand_", names(sw2022))), 13L)
  ## two profiles per respondent-task
  expect_true(all(table(sw2022$respondent, sw2022$task) == 2L))
})
