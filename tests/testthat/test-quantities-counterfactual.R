test_that("sc_counterfactual matches reference impl at 1e-6", {
  fit <- .get_toy_fit()
  A <- list(a1 = 1, a2 = 0, a3 = 0)
  B <- list(a1 = 0, a2 = 1, a3 = 0)
  q <- sc_counterfactual(fit, A = A, B = B, vartype = "plugin")
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

test_that("sc_counterfactual warns when the orthogonal estimate leaves [0,1]", {
  fit <- .get_toy_fit()
  local_mocked_bindings(
    .sc_debiased_scalar = function(object, Hfun) {
      c(estimate = 1.2, se = 0.1, ci_lo = 1.0, ci_hi = 1.4)
    },
    .package = "sconjoint"
  )
  ## two warnings by construction: the [0,1] boundary warning, and the
  ## raw-share specification warning (the mocked 1.2 sits far from the
  ## design share)
  w <- testthat::capture_warnings(
    sc_counterfactual(fit, A = list(a1 = 1), B = list(a2 = 1)))
  expect_true(any(grepl("outside \\[0, 1\\]", w)))
  expect_true(any(grepl("raw design-based share", w)))
})
