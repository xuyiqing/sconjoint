test_that("sc_optimal_profile greedy matches reference impl", {
  fit <- .get_toy_fit()
  q <- sc_optimal_profile(fit, search = "greedy")
  ref <- .ref_optimal_profile(fit$beta_hat, fit$respondent_id, fit$attr_map)
  expect_equal(q$estimate, ref$estimate, tolerance = 1e-6)
  expect_equal(q$se, ref$se, tolerance = 1e-6)
  expect_equal(as.numeric(q$details$optimal_dummy_vector), ref$dummy_vector,
               tolerance = 1e-12)
})

test_that("sc_optimal_profile exhaustive works for small grids", {
  fit <- .get_toy_fit()
  q_g <- sc_optimal_profile(fit, search = "greedy")
  q_e <- sc_optimal_profile(fit, search = "exhaustive")
  ## For purely additive utility with one-hot dummies, greedy = exhaustive.
  expect_equal(q_e$estimate, q_g$estimate, tolerance = 1e-12)
})
