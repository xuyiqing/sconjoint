# Bundled datasets: shape, identifiers and the columns added for the paper's inputs.

test_that("only the paper's two datasets are bundled", {
  ds <- ls(getNamespaceInfo(asNamespace("sconjoint"), "lazydata"))
  expect_setequal(ds, c("br2017", "gs2020"))
})

test_that("gs2020 has the paper's sample and two rows per matchup", {
  gs <- sconjoint::gs2020
  expect_identical(dim(gs), c(41314L, 60L))
  expect_identical(length(unique(gs$respondent)), 1605L)
  expect_identical(sum(gs$profile == 1L), 20657L)
  expect_identical(gs$profile, rep(1:2, nrow(gs) / 2))
  p1 <- gs[gs$profile == 1L, ]; p2 <- gs[gs$profile == 2L, ]
  expect_identical(p1$respondent, p2$respondent)
  expect_identical(p1$task, p2$task)
  expect_identical(p1$choice + p2$choice, rep(1L, nrow(p1)))
  diff_cols <- grep("^diff_", names(gs), value = TRUE)
  expect_length(diff_cols, 30L)
  expect_true(all(as.matrix(p2[, diff_cols]) == 0))
})

test_that("gs2020 on_left marks one left candidate per matchup", {
  gs <- sconjoint::gs2020
  expect_true(is.integer(gs$on_left))
  expect_true(all(gs$on_left %in% 0:1))
  p1 <- gs[gs$profile == 1L, ]; p2 <- gs[gs$profile == 2L, ]
  expect_identical(p1$on_left + p2$on_left, rep(1L, nrow(p1)))
  expect_identical(sum(p1$on_left == 0L), 10275L)
})

test_that("br2017 keeps raw covariate codes and the income-validity flag", {
  br <- sconjoint::br2017
  expect_identical(dim(br), c(32000L, 39L))
  expect_identical(length(unique(br$respondent)), 2000L)
  expect_true(is.logical(br$valid_income))
  bad <- sort(unique(br$respondent[!br$valid_income]))
  expect_identical(bad, c("206770863", "206819471", "206979199", "207003707",
                          "207124543", "207155412", "207177378", "207185872",
                          "207186054"))
  expect_identical(is.na(br$resp_income), !br$valid_income)
  expect_true(all(br$resp_income[br$valid_income] %in% 1:13))
  expect_true(all(br$resp_educ %in% 1:6))
  expect_true(all(br$resp_pid7 %in% 1:8))
  expect_true(all(br$resp_age >= 18 & br$resp_age <= 100))
  # Respondent-level columns are constant within respondent.
  for (v in c("resp_age", "resp_educ", "resp_pid7", "valid_income")) {
    expect_true(all(tapply(br[[v]], br$respondent, function(x) length(unique(x)) == 1L)))
  }
  # The standardized columns are recodings of the raw codes.
  ok <- br$valid_income
  expect_equal(stats::cor(br$age_std, br$resp_age), 1)
  expect_equal(stats::cor(br$income_std[ok], br$resp_income[ok]), 1)
})
