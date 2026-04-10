skip_on_cran()
skip_if_not_installed("torch")
if (!torch::torch_is_installed()) {
  testthat::skip("libtorch not installed")
}

test_that("sw2022 runs scfit() + a few quantities", {
  data(sw2022, package = "sconjoint")
  fit <- sconjoint::scfit(
    choice ~ agenda + talent + children + cand_gender + prior_office |
             resp_female + age + pid,
    data = sw2022, respondent = "respondent",
    task = "task", profile = "profile",
    K = 2L, n_epochs = 40L, seed = 1L
  )
  expect_s3_class(fit, "sc_fit")
  expect_no_error(sc_importance(fit))
  expect_no_error(sc_direction_intensity(fit))
  expect_no_error(sc_subgroup(fit, fit$Z[, "resp_female"] > 0.5))
})

test_that("gs2020 runs scfit() + a few quantities", {
  data(gs2020, package = "sconjoint")
  fit <- sconjoint::scfit(
    choice ~ federal + immigration + tax + abortion + undem + cand_party |
             resp_pid + resp_ideo + age,
    data = gs2020, respondent = "respondent",
    task = "task", profile = "profile",
    K = 2L, n_epochs = 40L, seed = 1L
  )
  expect_s3_class(fit, "sc_fit")
  expect_no_error(sc_heterogeneity_test(fit))
  expect_no_error(sc_clusters(fit, k = 3L, seed = 1L))
})

test_that("bs2013 runs scfit() + a WTP + compensating", {
  data(bs2013, package = "sconjoint")
  fit <- sconjoint::scfit(
    choice ~ distribution + enforcement + monitoring +
             participation + sanctions + cost_usd |
             resp_female + age + resp_ideo,
    data = bs2013, respondent = "respondent",
    task = "task", profile = "profile",
    K = 2L, n_epochs = 40L, seed = 1L
  )
  expect_s3_class(fit, "sc_fit")
  expect_no_error(sc_importance(fit))
  ## Dollar-scale WTP against the numeric cost attribute.
  expect_no_error(sc_wtp(fit, attr = "participation90pct", cost_attr = "cost_usd"))
})
