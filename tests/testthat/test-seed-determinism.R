test_that(".sc_train_one with fixed seed is bit-exact on CPU (single core)", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  set.seed(1)
  N  <- 120L
  p  <- 3L
  pz <- 2L
  Z  <- matrix(rnorm(N * pz), N, pz)
  deltaX <- matrix(rnorm(N * p), N, p)
  beta_true <- c(0.5, -0.3, 0.8)
  y <- rbinom(N, 1, plogis(deltaX %*% beta_true))

  r1 <- sconjoint:::.sc_train_one(deltaX, y, Z,
                                  hidden = c(8L),
                                  n_epochs = 30L,
                                  learning_rate = 0.05,
                                  lambda = 0,
                                  seed = 42L,
                                  device = "cpu")
  r2 <- sconjoint:::.sc_train_one(deltaX, y, Z,
                                  hidden = c(8L),
                                  n_epochs = 30L,
                                  learning_rate = 0.05,
                                  lambda = 0,
                                  seed = 42L,
                                  device = "cpu")

  b1 <- sconjoint:::.sc_predict_beta(r1$net, Z)
  b2 <- sconjoint:::.sc_predict_beta(r2$net, Z)
  expect_identical(b1, b2)
  expect_identical(r1$loss_trace, r2$loss_trace)
})
