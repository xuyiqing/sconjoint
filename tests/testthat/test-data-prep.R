test_that(".sc_parse_formula splits two-sided formulas", {
  parsed <- sconjoint:::.sc_parse_formula(choice ~ a + b + c | z1 + z2)
  expect_equal(parsed$response, "choice")
  expect_equal(parsed$attr_vars, c("a", "b", "c"))
  expect_equal(parsed$z_vars, c("z1", "z2"))

  parsed2 <- sconjoint:::.sc_parse_formula(y ~ x1 + x2)
  expect_equal(parsed2$response, "y")
  expect_equal(parsed2$attr_vars, c("x1", "x2"))
  expect_length(parsed2$z_vars, 0L)

  expect_error(sconjoint:::.sc_parse_formula(~ x), "two-sided")
})

test_that(".sc_to_long validates the long-format structure", {
  df <- data.frame(
    r = rep(1:20, each = 6),
    t = rep(rep(1:3, each = 2), 20),
    p = rep(1:2, 60),
    choice = rbinom(120, 1, 0.5),
    a = sample(letters[1:3], 120, replace = TRUE),
    z = rep(rnorm(20), each = 6)
  )
  out <- sconjoint:::.sc_to_long(df, "r", "t", "p")
  expect_equal(nrow(out), 120)

  bad <- df[-1L, , drop = FALSE]
  expect_error(sconjoint:::.sc_to_long(bad, "r", "t", "p"), "exactly 2 profiles")
})

test_that(".sc_encode dummy-encodes attributes and builds DeltaX", {
  set.seed(1)
  df <- data.frame(
    r = rep(1:20, each = 6),
    t = rep(rep(1:3, each = 2), 20),
    p = rep(1:2, 60),
    a1 = sample(c("low", "med", "high"), 120, replace = TRUE),
    a2 = sample(c("A", "B"), 120, replace = TRUE),
    a3 = sample(c("x", "y", "z", "w"), 120, replace = TRUE),
    a4 = rnorm(120),
    z1 = rep(rnorm(20), each = 6),
    z2 = rep(sample(c("M", "F"), 20, replace = TRUE), each = 6),
    stringsAsFactors = FALSE
  )

  enc <- sconjoint:::.sc_encode(df, c("a1", "a2", "a3", "a4"), c("z1", "z2"))
  # a1: 2 dummies, a2: 1, a3: 3, a4: 1  -> 7 columns
  expect_equal(ncol(enc$X), 7L)
  expect_equal(nrow(enc$X), 120L)
  # z1: 1 (numeric), z2: 1 dummy -> 2 columns
  expect_equal(ncol(enc$Z), 2L)

  long <- sconjoint:::.sc_to_long(df, "r", "t", "p")
  enc2 <- sconjoint:::.sc_encode(long, c("a1", "a2", "a3", "a4"), c("z1", "z2"))
  dx <- sconjoint:::.sc_build_deltax(enc2$X, enc2$Z, long$t, long$p, long$r)
  expect_equal(nrow(dx$deltaX), 60L)
  expect_equal(ncol(dx$deltaX), 7L)
  expect_equal(nrow(dx$Z_task), 60L)
  expect_equal(length(dx$respondent_task), 60L)
})
