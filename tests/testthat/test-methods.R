## Tests for predict.sc_fit() — M5.a forward-pass and newdata support
## Spec: test-spec.md §2

## ---- 2.1 predict(newdata = NULL) backward compatibility --------------------

test_that("T1: predict(fit) returns beta_hat (default)", {
  fit <- .get_toy_fit()
  p0 <- predict(fit)
  expect_identical(p0, fit$beta_hat)
})

test_that("T2: predict(fit, type = 'beta') same as default", {
  fit <- .get_toy_fit()
  p_beta <- predict(fit, type = "beta")
  expect_identical(p_beta, fit$beta_hat)
})

test_that("T3: predict(fit, type = 'logit')", {
  fit <- .get_toy_fit()
  p_logit <- predict(fit, type = "logit")
  expected <- as.numeric(rowSums(fit$deltaX * fit$beta_hat))
  expect_type(p_logit, "double")
  expect_length(p_logit, nrow(fit$beta_hat))
  expect_equal(p_logit, expected)
})

test_that("T4: predict(fit, type = 'prob')", {
  fit <- .get_toy_fit()
  p_prob <- predict(fit, type = "prob")
  expected <- as.numeric(plogis(rowSums(fit$deltaX * fit$beta_hat)))
  expect_type(p_prob, "double")
  expect_length(p_prob, nrow(fit$beta_hat))
  expect_true(all(p_prob >= 0 & p_prob <= 1))
  expect_equal(p_prob, expected)
})

## ---- 2.2 predict(newdata = ...) forward-pass -------------------------------

test_that("T5: predict(fit, newdata = <data.frame>, type = 'beta')", {
  fit <- .get_toy_fit()
  set.seed(42)
  new_df <- as.data.frame(matrix(rnorm(5 * length(fit$z_names)),
                                  nrow = 5))
  names(new_df) <- fit$z_names
  result <- predict(fit, newdata = new_df, type = "beta")
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 5L)
  expect_equal(ncol(result), length(fit$attr_names))
  expect_equal(colnames(result), fit$attr_names)
  expect_true(all(is.finite(result)))
})

test_that("T6: predict(fit, newdata = <matrix>, type = 'beta')", {
  fit <- .get_toy_fit()
  set.seed(43)
  new_mat <- matrix(rnorm(3 * length(fit$z_names)),
                    nrow = 3, ncol = length(fit$z_names))
  result <- predict(fit, newdata = new_mat, type = "beta")
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 3L)
  expect_equal(ncol(result), length(fit$attr_names))
  expect_true(all(is.finite(result)))
})

test_that("T7: forward-pass reproduces held-out predictions (approximate)", {
  fit <- .get_toy_fit()
  pred_new <- predict(fit, newdata = fit$Z, type = "beta")
  r <- cor(as.numeric(pred_new), as.numeric(fit$beta_hat))
  expect_gt(r, 0.5)
})

## ---- Edge cases ------------------------------------------------------------

test_that("T8: newdata with wrong number of columns errors", {
  fit <- .get_toy_fit()
  bad_mat <- matrix(1, nrow = 2, ncol = 1)
  expect_error(predict(fit, newdata = bad_mat), "columns")
})

test_that("T9: newdata data.frame missing Z columns errors", {
  fit <- .get_toy_fit()
  bad_df <- data.frame(x = 1:3)
  expect_error(predict(fit, newdata = bad_df), "missing")
})

test_that("T10: newdata non-numeric matrix errors", {
  fit <- .get_toy_fit()
  bad_mat <- matrix("a", nrow = 2, ncol = length(fit$z_names))
  expect_error(predict(fit, newdata = bad_mat), "numeric")
})

test_that("T11: newdata that is neither data.frame nor matrix errors", {
  fit <- .get_toy_fit()
  expect_error(predict(fit, newdata = list(a = 1)), "data.frame|matrix")
})

## ---- 2.3 type restrictions with newdata ------------------------------------

test_that("T12: type = 'logit' with newdata errors", {
  fit <- .get_toy_fit()
  set.seed(44)
  new_df <- as.data.frame(matrix(rnorm(3 * length(fit$z_names)), nrow = 3))
  names(new_df) <- fit$z_names
  expect_error(predict(fit, newdata = new_df, type = "logit"),
               "deltaX|not available")
})

test_that("T13: type = 'prob' with newdata errors", {
  fit <- .get_toy_fit()
  set.seed(45)
  new_df <- as.data.frame(matrix(rnorm(3 * length(fit$z_names)), nrow = 3))
  names(new_df) <- fit$z_names
  expect_error(predict(fit, newdata = new_df, type = "prob"),
               "deltaX|not available")
})

## ---- 2.4 keep_modules = FALSE ----------------------------------------------

test_that("T14: scfit with keep_modules = FALSE", {
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) skip("libtorch not installed")
  toy <- .make_toy_long(M = 30L, T_i = 3L, p = 3L, p_Z = 2L, seed = 2L)
  fit_no_mod <- sconjoint::scfit(
    y ~ a1 + a2 + a3 | z1 + z2,
    data = toy$data,
    respondent = "rid", task = "tid", profile = "pos",
    K = 2L, n_epochs = 10L, seed = 2L,
    keep_modules = FALSE
  )
  expect_null(fit_no_mod$nets)
  p0 <- predict(fit_no_mod)
  expect_identical(p0, fit_no_mod$beta_hat)
})

test_that("T15: predict(newdata) with keep_modules = FALSE errors", {
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) skip("libtorch not installed")
  toy <- .make_toy_long(M = 30L, T_i = 3L, p = 3L, p_Z = 2L, seed = 2L)
  fit_no_mod <- sconjoint::scfit(
    y ~ a1 + a2 + a3 | z1 + z2,
    data = toy$data,
    respondent = "rid", task = "tid", profile = "pos",
    K = 2L, n_epochs = 10L, seed = 2L,
    keep_modules = FALSE
  )
  new_df <- data.frame(z1 = 1, z2 = 2)
  expect_error(predict(fit_no_mod, newdata = new_df),
               "keep_modules|modules were not stored")
})

## ---- 2.5 keep_modules = TRUE stores nets -----------------------------------

test_that("T16: fit$nets is a list of length K", {
  fit <- .get_toy_fit()
  expect_true(is.list(fit$nets))
  expect_equal(length(fit$nets), fit$K)
})
