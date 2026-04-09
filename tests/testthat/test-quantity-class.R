test_that("sc_quantity print and format don't error", {
  q <- sconjoint:::.sc_quantity(
    name = "toy", estimate = 1.5, se = 0.1,
    ci_lo = 1.3, ci_hi = 1.7,
    details = list(subgroup_size = 10L))
  expect_output(print(q), "toy")
  expect_type(format(q), "character")
})

test_that("sc_quantity_bivariate dispatch works", {
  d <- sconjoint:::.sc_quantity("direction", estimate = 0.2)
  u <- sconjoint:::.sc_quantity("intensity", estimate = 0.4)
  b <- sconjoint:::.sc_quantity_bivariate("di", direction = d, intensity = u)
  expect_s3_class(b, "sc_quantity_bivariate")
  expect_s3_class(b, "sc_quantity")
  expect_output(print(b), "direction")
})
