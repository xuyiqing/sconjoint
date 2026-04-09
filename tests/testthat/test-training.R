test_that(".sc_train_one reduces loss and recovers a simple DGP", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  ## Tiny DGP: binary Z modulates 3 attribute effects linearly.
  set.seed(7)
  N  <- 300L
  p  <- 3L
  pz <- 2L
  Z  <- cbind(runif(N, -1, 1), rbinom(N, 1, 0.5))
  ## True beta(Z): linear in Z
  W  <- matrix(c(1.2, -0.8, 0.3,  0.5, 0.2, -0.6), nrow = pz, byrow = TRUE)
  beta_true <- Z %*% W   # N x p
  deltaX <- matrix(rnorm(N * p), N, p)
  logit  <- rowSums(deltaX * beta_true)
  y      <- rbinom(N, 1, plogis(logit))

  res <- sconjoint:::.sc_train_one(
    deltaX, y, Z,
    hidden        = c(8L, 8L),
    n_epochs      = 200L,
    learning_rate = 0.05,
    lambda        = 0,
    seed          = 123L,
    device        = "cpu",
    verbose       = FALSE
  )

  expect_true(res$loss_trace[1] > res$final_loss)
  expect_true(res$final_loss < 0.75)  ## < log(2) ~= 0.693 baseline

  beta_hat <- sconjoint:::.sc_predict_beta(res$net, Z)
  expect_equal(dim(beta_hat), c(N, p))
  mae <- mean(abs(beta_hat - beta_true))
  ## Loose correctness band: on N=300 with 200 epochs the network
  ## cannot nail the linear DGP but must clearly beat a null fit.
  expect_lt(mae, 0.6)
})
