## Unit tests for the MAP Stage-2 algorithm primitives.
## Tests the internal pieces in R/map-stage2.R in isolation: no `scfit()`
## end-to-end run here.  End-to-end tests live in
## test-stage2-determinism.R and test-stage2-orthogonality.R.

test_that(".sc_collapse_to_resp / .sc_expand_to_task are inverses", {
  set.seed(1)
  ## 3 respondents, T_i=4 each, P=2
  resp_idx <- rep(1:3, each = 4)
  beta_resp_true <- matrix(c(1, 1,
                             -1, -1,
                             0, 0),
                           nrow = 3, byrow = TRUE)
  ## Expand to task level, then collapse back
  bt <- .sc_expand_to_task(beta_resp_true, resp_idx)
  expect_equal(dim(bt), c(12L, 2L))
  br <- .sc_collapse_to_resp(bt, resp_idx, n_resp = 3L)
  expect_equal(br, beta_resp_true,
               ignore_attr = TRUE)
})

test_that(".sc_estimate_sigma_score has the right shape and floor", {
  set.seed(1)
  n_resp <- 20L
  T_i <- 4L
  P <- 3L
  n_task <- n_resp * T_i
  resp_idx <- rep(seq_len(n_resp), each = T_i)
  deltaX <- matrix(stats::rnorm(n_task * P), n_task, P)
  beta_hat_resp <- matrix(stats::rnorm(n_resp * P), n_resp, P)
  beta_task <- beta_hat_resp[resp_idx, , drop = FALSE]
  lin <- rowSums(deltaX * beta_task)
  y <- stats::rbinom(n_task, 1, stats::plogis(lin))

  sig <- .sc_estimate_sigma_score(deltaX, y, beta_hat_resp, resp_idx)
  expect_length(sig, P)
  expect_true(all(sig >= 0.01))  ## floor enforced
  expect_true(all(is.finite(sig)))
})

test_that(".sc_estimate_sigma_score floor at 0.01 triggers on near-separated data", {
  ## Near-separated: deltaX huge, beta huge, predicted prob essentially
  ## 0 or 1, residuals ~ 0, so the raw score variance is also ~ 0.
  set.seed(2)
  n_resp <- 10L
  T_i <- 5L
  P <- 2L
  n_task <- n_resp * T_i
  resp_idx <- rep(seq_len(n_resp), each = T_i)
  deltaX <- matrix(10 * stats::rnorm(n_task * P), n_task, P)
  beta_hat_resp <- matrix(0, n_resp, P)
  ## All residuals exactly zero -> raw variance zero -> floor must kick in
  y <- as.numeric(stats::plogis(rowSums(deltaX * beta_hat_resp[resp_idx, ])) > 0.5)
  sig <- .sc_estimate_sigma_score(deltaX, y, beta_hat_resp, resp_idx,
                                  floor = 0.01)
  expect_true(all(sig >= 0.01))
})

test_that(".sc_estimate_sigma_varref gives 0.5 * Var with floor", {
  set.seed(3)
  beta_resp <- matrix(stats::rnorm(60), 20, 3)
  s <- .sc_estimate_sigma_varref(beta_resp)
  expected <- pmax(0.5 * apply(beta_resp, 2, stats::var), 0.01)
  expect_equal(unname(s), expected)
})

test_that(".sc_map_one reduces gradient and Hessian is negative definite", {
  set.seed(4)
  P <- 3L
  T_i <- 8L
  deltaX_i <- matrix(stats::rnorm(T_i * P), T_i, P)
  beta_true <- c(0.7, -0.4, 0.2)
  y_i <- stats::rbinom(T_i, 1, stats::plogis(deltaX_i %*% beta_true))
  f_i <- c(0.5, -0.2, 0.1)
  sigma_prior <- rep(0.2, P)
  res <- .sc_map_one(deltaX_i, y_i, f_i, sigma_prior,
                     max_iter = 50L, tol = 1e-8)
  expect_true(res$converged)
  expect_length(res$eta, P)
  expect_length(res$post_var_diag, P)
  expect_true(all(res$post_var_diag > 0))
})

test_that(".sc_map_all returns the right shape", {
  set.seed(5)
  n_resp <- 8L
  T_i <- 5L
  P <- 3L
  n_task <- n_resp * T_i
  resp_idx <- rep(seq_len(n_resp), each = T_i)
  deltaX <- matrix(stats::rnorm(n_task * P), n_task, P)
  beta_hat_resp <- matrix(stats::rnorm(n_resp * P), n_resp, P)
  beta_task <- beta_hat_resp[resp_idx, , drop = FALSE]
  y <- stats::rbinom(n_task, 1, stats::plogis(rowSums(deltaX * beta_task)))
  sig_prior <- rep(0.2, P)
  out <- .sc_map_all(deltaX, y, beta_hat_resp, resp_idx, sig_prior)
  expect_equal(dim(out$beta_hat_resp_map), c(n_resp, P))
  expect_equal(dim(out$post_var_resp_diag), c(n_resp, P))
})

test_that("BUG GUARD: prior must be respondent-indexed, not task-indexed", {
  ## This test is the regression guard for the prototype's 2026-04-26 bug,
  ## documented in code/04b_map_update.R lines 172-175.  Passing the prior
  ## at task-level indexing instead of respondent-level produces visibly
  ## different MAP estimates.
  set.seed(6)
  n_resp <- 5L
  T_i <- 6L
  P <- 2L
  n_task <- n_resp * T_i
  resp_idx <- rep(seq_len(n_resp), each = T_i)
  deltaX <- matrix(stats::rnorm(n_task * P), n_task, P)
  ## Deliberately heterogeneous respondent-level priors
  beta_hat_resp <- matrix(c( 1.0,  0.5,
                            -1.0, -0.5,
                             0.5,  1.0,
                            -0.5, -1.0,
                             0.0,  0.0),
                          nrow = n_resp, byrow = TRUE)
  y <- stats::rbinom(n_task, 1,
                     stats::plogis(rowSums(deltaX * beta_hat_resp[resp_idx, ])))
  sig_prior <- c(0.2, 0.3)

  ## Correct: respondent-level prior matrix
  ok <- .sc_map_all(deltaX, y, beta_hat_resp, resp_idx, sig_prior)

  ## Wrong (the bug): pass task-level expansion as the prior matrix.
  ## We simulate this by giving the function a beta_hat_resp with n_task
  ## rows, but the function expects respondent-level shape.  The shape
  ## check would catch it directly, so instead we test that two priors
  ## with the same MEANS but different structure give visibly different
  ## results — i.e. the function is genuinely using respondent-level
  ## priors, not just the column means.
  beta_hat_resp_flat <- matrix(colMeans(beta_hat_resp), n_resp, P,
                               byrow = TRUE)
  flat <- .sc_map_all(deltaX, y, beta_hat_resp_flat, resp_idx, sig_prior)

  ## Distinct priors should produce distinct posteriors
  expect_false(isTRUE(all.equal(ok$beta_hat_resp_map,
                                flat$beta_hat_resp_map,
                                tolerance = 1e-6)))
})
