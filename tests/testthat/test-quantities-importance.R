test_that("sc_importance matches reference impl at 1e-6 (uniform)", {
  fit <- .get_toy_fit()
  q <- sc_importance(fit, design = "uniform")
  ref <- .ref_importance(fit$beta_hat, fit$respondent_id, fit$attr_map,
                         design = "uniform")
  expect_equal(q$estimate$share, ref$share, tolerance = 1e-6)
  expect_equal(q$estimate$se, ref$se, tolerance = 1e-6)
  expect_equal(sum(q$estimate$share), 1, tolerance = 1e-6)
})

test_that("sc_importance empirical branch returns valid shares", {
  fit <- .get_toy_fit()
  q_emp <- sc_importance(fit, design = "empirical")
  expect_equal(sum(q_emp$estimate$share), 1, tolerance = 1e-6)
  expect_true(all(q_emp$estimate$share >= 0))
  ## Empirical and uniform should differ in general
  q_unif <- sc_importance(fit, design = "uniform")
  expect_false(isTRUE(all.equal(q_emp$estimate$share, q_unif$estimate$share)))
})
