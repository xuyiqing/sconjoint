test_that("sc_compensating matches reference on two dummies", {
  fit <- .get_toy_fit()
  q <- sc_compensating(fit, benefit = 1L, cost = 2L)
  ref <- .ref_compensating(fit$beta_hat, fit$respondent_id,
                           benefit_idx = 1L, cost_idx = 2L)
  expect_s3_class(q, "sc_quantity")
  expect_equal(q$estimate, ref$estimate, tolerance = 1e-12)
  expect_equal(q$se, ref$se, tolerance = 1e-6)
  expect_equal(q$details$frac_compensated, ref$frac_compensated,
               tolerance = 1e-12)
  expect_equal(q$details$n, ref$n)
})

test_that("sc_compensating errors when benefit and cost coincide", {
  fit <- .get_toy_fit()
  expect_error(sc_compensating(fit, benefit = 1L, cost = 1L), "different")
})

test_that("sc_compensating errors on malformed trim", {
  fit <- .get_toy_fit()
  expect_error(sc_compensating(fit, 1L, 2L, trim = c(0.99, 0.01)), "trim")
  expect_error(sc_compensating(fit, 1L, 2L, trim = c(-0.1, 0.9)), "trim")
})
