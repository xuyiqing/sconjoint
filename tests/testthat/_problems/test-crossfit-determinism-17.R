# Extracted from test-crossfit-determinism.R:17

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "sconjoint", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())
skip_if_not_installed("future")
skip_if_not_installed("future.apply")
skip_on_cran()
toy <- .make_toy_long(M = 80L, T_i = 3L, p = 3L, p_Z = 2L, seed = 2L)
args <- list(
    formula = y ~ a1 + a2 + a3 | z1 + z2,
    data = toy$data,
    respondent = "rid", task = "tid", profile = "pos",
    K = 4L, n_epochs = 50L, seed = 42L
  )
fit1 <- do.call(scfit, c(args, list(parallel = FALSE)))
fit2 <- do.call(scfit, c(args, list(parallel = TRUE,  n_cores = 2L)))
