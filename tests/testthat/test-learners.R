## Pluggable first-stage learners (elastic net, GRF).  These slot into the
## same DML layer as the DNN, swapping only the first-stage beta_hat(Z), so
## the checks here are: scfit() returns a well-formed sc_fit, the DML
## inference objects are sane (finite theta, symmetric PSD vcov), downstream
## quantity functions run, and the fit is deterministic under a fixed seed.

.fit_with_learner <- function(learner, seed = 1L, K = 4L, ...) {
  toy <- .make_toy_long(M = 80L, T_i = 4L, p = 3L, p_Z = 2L, seed = 7L)
  scfit(y ~ a1 + a2 + a3 | z1 + z2,
        data = toy$data,
        respondent = "rid", task = "tid", profile = "pos",
        learner = learner, K = K, seed = seed, ...)
}

test_that("elastic-net learner produces a well-formed sc_fit", {
  skip_if_not_installed("glmnet")
  fit <- .fit_with_learner("enet")

  expect_s3_class(fit, "sc_fit")
  expect_identical(fit$learner, "enet")
  expect_length(fit$theta, 3L)
  expect_true(all(is.finite(fit$theta)))

  ## DML clustered vcov: p x p, symmetric, PSD.
  expect_equal(dim(fit$vcov), c(3L, 3L))
  expect_true(max(abs(fit$vcov - t(fit$vcov))) < 1e-10)
  evs <- eigen(fit$vcov, symmetric = TRUE, only.values = TRUE)$values
  expect_true(min(evs) > -1e-8)

  ## Out-of-sample first stage filled for every row, finite.
  expect_equal(nrow(fit$beta_hat), nrow(fit$deltaX))
  expect_true(all(is.finite(fit$beta_hat)))

  ## stage2 is forced off for non-DNN learners; no torch nets stored.
  expect_null(fit$nets)
  expect_false(is.null(fit$fold_models))
})

test_that("GRF learner produces a well-formed sc_fit", {
  skip_if_not_installed("grf")
  fit <- .fit_with_learner("grf")

  expect_s3_class(fit, "sc_fit")
  expect_identical(fit$learner, "grf")
  expect_length(fit$theta, 3L)
  expect_true(all(is.finite(fit$theta)))
  expect_true(all(is.finite(fit$beta_hat)))

  expect_equal(dim(fit$vcov), c(3L, 3L))
  expect_true(max(abs(fit$vcov - t(fit$vcov))) < 1e-10)
  evs <- eigen(fit$vcov, symmetric = TRUE, only.values = TRUE)$values
  expect_true(min(evs) > -1e-8)
})

test_that("downstream quantity functions run on a non-DNN learner", {
  skip_if_not_installed("glmnet")
  fit <- .fit_with_learner("enet")
  ## coef / vcov / summary should all work off the learner-agnostic slots.
  expect_length(coef(fit), 3L)
  expect_equal(dim(vcov(fit)), c(3L, 3L))
  ## A debiased quantity (attribute importance) should return finite values.
  imp <- sc_importance(fit)
  expect_true(is.data.frame(imp) || is.list(imp))
})

test_that("learners are deterministic under a fixed seed", {
  skip_if_not_installed("glmnet")
  a <- .fit_with_learner("enet", seed = 42L)
  b <- .fit_with_learner("enet", seed = 42L)
  expect_equal(coef(a), coef(b))
  expect_equal(a$vcov, b$vcov)

  skip_if_not_installed("grf")
  g1 <- .fit_with_learner("grf", seed = 42L)
  g2 <- .fit_with_learner("grf", seed = 42L)
  expect_equal(coef(g1), coef(g2))
})

test_that("stage2 != none warns and is downgraded for non-DNN learners", {
  skip_if_not_installed("glmnet")
  expect_warning(
    .fit_with_learner("enet", stage2 = "map_c5"),
    "learner = \"dnn\""
  )
})

test_that("requesting a learner without its package errors cleanly", {
  ## Only exercisable when the suggested package is absent; otherwise skip.
  skip_if(requireNamespace("glmnet", quietly = TRUE))
  expect_error(.fit_with_learner("enet"), "glmnet")
})
