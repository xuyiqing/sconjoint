test_that("sc_polarization matches reference impl at 1e-12", {
  fit <- .get_toy_fit()
  q <- sc_polarization(fit)
  ref <- .ref_polarization(fit$beta_hat)
  expect_equal(q$estimate$frac_positive, ref$frac_positive, tolerance = 1e-12)
  expect_equal(q$estimate$frac_negative, ref$frac_negative, tolerance = 1e-12)
  expect_equal(q$estimate$polarization_idx, ref$polarization_idx, tolerance = 1e-12)
  expect_true(all(is.na(q$estimate$se)))
})
