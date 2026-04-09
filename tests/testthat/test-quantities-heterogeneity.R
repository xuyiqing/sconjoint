test_that("sc_heterogeneity_test matches reference impl", {
  fit <- .get_toy_fit()
  q <- sc_heterogeneity_test(fit, adjust = "none")
  ref <- .ref_heterogeneity_test(fit$beta_hat, fit$respondent_id, adjust = "none")
  expect_equal(q$estimate$var_beta, ref$var_beta, tolerance = 1e-6)
  expect_equal(q$estimate$se_var, ref$se_var, tolerance = 1e-6)
  expect_equal(q$estimate$t_stat, ref$t_stat, tolerance = 1e-6)
  expect_equal(q$estimate$p_value, ref$p_value, tolerance = 1e-6)
  q_bh <- sc_heterogeneity_test(fit, adjust = "bh")
  expect_equal(q_bh$estimate$p_adjusted,
               stats::p.adjust(ref$p_value, method = "BH"),
               tolerance = 1e-6)
})
