test_that("sc_wtp is sign-flipped sc_mrs", {
  fit <- .get_toy_fit()
  m <- sc_mrs(fit, "a1", "a2")
  w <- sc_wtp(fit, attr = "a1", cost_attr = "a2")
  expect_equal(w$estimate, -m$estimate, tolerance = 1e-12)
  expect_equal(w$se, m$se, tolerance = 1e-12)
  ref <- .ref_wtp(fit$beta_hat, fit$respondent_id, 1L, 2L)
  expect_equal(w$estimate, ref$estimate, tolerance = 1e-6)
})
