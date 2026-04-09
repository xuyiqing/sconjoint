test_that(".sc_auto_hidden returns correct bands", {
  expect_equal(sconjoint:::.sc_auto_hidden(500),    c(32L, 16L))
  expect_equal(sconjoint:::.sc_auto_hidden(1999),   c(32L, 16L))
  expect_equal(sconjoint:::.sc_auto_hidden(2000),   c(32L, 32L, 16L))
  expect_equal(sconjoint:::.sc_auto_hidden(9999),   c(32L, 32L, 16L))
  expect_equal(sconjoint:::.sc_auto_hidden(10000),  c(64L, 64L, 32L))
  expect_equal(sconjoint:::.sc_auto_hidden(1e6),    c(64L, 64L, 32L))
  expect_error(sconjoint:::.sc_auto_hidden(-1))
})

test_that(".sc_build_network returns a torch nn_module with correct shapes", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())

  net <- sconjoint:::.sc_build_network(p = 4L, p_Z = 2L, hidden = c(8L))
  expect_s3_class(net, "nn_module")

  batch <- 5L
  Z  <- torch::torch_tensor(matrix(rnorm(batch * 2), batch, 2), dtype = torch::torch_float())
  dX <- torch::torch_tensor(matrix(rnorm(batch * 4), batch, 4), dtype = torch::torch_float())

  logit <- net$forward(dX, Z)
  expect_equal(as.integer(logit$shape), batch)

  beta <- net$get_beta(Z)
  expect_equal(as.integer(beta$shape), c(batch, 4L))

  prob <- sconjoint:::.sc_forward(net, Z, dX)
  pr <- as.numeric(torch::as_array(prob))
  expect_true(all(pr >= 0 & pr <= 1))
})
