test_that("sc_importance matches reference impl at 1e-6 (uniform)", {
  fit <- .get_toy_fit()
  q <- sc_importance(fit, design = "uniform")
  ref <- .ref_importance(fit$beta_hat, fit$respondent_id, fit$attr_map,
                         design = "uniform")
  expect_equal(q$estimate$share, ref$share, tolerance = 1e-6)
  expect_equal(q$estimate$se, ref$se, tolerance = 1e-6)
  expect_equal(sum(q$estimate$share), 1, tolerance = 1e-6)
})
