test_that("sc_counterfactual matches reference impl at 1e-6", {
  fit <- .get_toy_fit()
  A <- list(a1 = 1, a2 = 0, a3 = 0)
  B <- list(a1 = 0, a2 = 1, a3 = 0)
  q <- sc_counterfactual(fit, A = A, B = B)
  delta_x <- c(1, -1, 0)
  ref <- .ref_counterfactual(fit$beta_hat, fit$respondent_id, delta_x)
  expect_equal(q$estimate, ref$estimate, tolerance = 1e-12)
  expect_equal(q$se, ref$se, tolerance = 1e-6)
})

test_that("sc_counterfactual rejects unknown attributes", {
  fit <- .get_toy_fit()
  expect_error(
    sc_counterfactual(fit,
                      A = list(nonsense = 1),
                      B = list(a1 = 0)),
    "unknown attribute"
  )
})
