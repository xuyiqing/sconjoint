test_that("sc_fraction_preferring matches reference impl", {
  fit <- .get_toy_fit()
  q <- sc_fraction_preferring(fit, threshold = 0)
  ref <- .ref_fraction_preferring(fit$beta_hat, fit$respondent_id, 0)
  expect_equal(q$estimate$frac_positive, ref$frac_positive, tolerance = 1e-12)
  expect_equal(q$estimate$frac_negative, ref$frac_negative, tolerance = 1e-12)
  expect_equal(q$estimate$se_positive, ref$se_positive, tolerance = 1e-6)
  expect_equal(q$estimate$se_negative, ref$se_negative, tolerance = 1e-6)
})
