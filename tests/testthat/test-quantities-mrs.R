test_that("sc_mrs matches the reference implementation at 1e-6", {
  fit <- .get_toy_fit()
  q <- sc_mrs(fit, numerator = "a1", denominator = "a2")
  ref <- .ref_mrs(fit$beta_hat, fit$respondent_id, 1L, 2L)
  expect_equal(q$estimate, ref$estimate, tolerance = 1e-6)
  expect_equal(q$se, ref$se, tolerance = 1e-6)
})

test_that("sc_mrs resolves bare dummy names and accepts subgroup", {
  fit <- .get_toy_fit()
  q1 <- sc_mrs(fit, "a1", "a2")
  q2 <- sc_mrs(fit, 1L, 2L)
  expect_equal(q1$estimate, q2$estimate, tolerance = 1e-12)
  N <- nrow(fit$beta_hat)
  sub <- seq_len(N) <= (N %/% 2L)
  q3 <- sc_mrs(fit, "a1", "a2", subgroup = sub)
  ref3 <- .ref_mrs(fit$beta_hat, fit$respondent_id, 1L, 2L, subset_idx = which(sub))
  expect_equal(q3$estimate, ref3$estimate, tolerance = 1e-6)
})
