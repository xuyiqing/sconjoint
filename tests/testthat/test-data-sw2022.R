test_that("sw2022 loads with expected columns and size", {
  data(sw2022, package = "sconjoint")
  expect_s3_class(sw2022, "data.frame")
  expect_true(all(c("respondent", "task", "profile", "choice",
                    "agenda", "talent", "children", "cand_gender",
                    "prior_office", "resp_female", "age", "pid")
                  %in% names(sw2022)))
  expect_equal(nrow(sw2022), 200L * 3L * 2L)
  expect_true(all(table(sw2022$respondent, sw2022$task) == 2L))
  expect_identical(attr(sw2022, "synthetic"), TRUE)
  f <- system.file("data", "sw2022.rda", package = "sconjoint")
  if (nzchar(f)) {
    expect_lt(file.info(f)$size, 500L * 1024L)
  }
})
