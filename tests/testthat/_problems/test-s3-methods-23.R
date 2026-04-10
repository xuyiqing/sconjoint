# Extracted from test-s3-methods.R:23

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "sconjoint", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
fit <- .get_toy_fit()
expect_error(
    predict(fit, newdata = data.frame()),
    "M5\\.a"
  )
