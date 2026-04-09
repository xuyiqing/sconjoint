test_that("DML inference on tiny DGP recovers true theta reasonably", {
  set.seed(3)
  M <- 80; T_i <- 4; p <- 3; p_Z <- 2
  Z_mat <- matrix(stats::rnorm(M * p_Z), M, p_Z)
  beta_true <- cbind(0.5, -0.3, 0.2)[rep(1, M), ]  # homogeneous for this test
  rid <- rep(seq_len(M), each = T_i)
  dX <- matrix(sample(c(-1, 0, 1), M * T_i * p, replace = TRUE), M * T_i, p)
  logit <- rowSums(dX * beta_true[rid, ])
  y <- stats::rbinom(M * T_i, 1, stats::plogis(logit))
  Z_task <- Z_mat[rid, , drop = FALSE]

  # Use beta_hat == truth as the "oracle" first stage to isolate the
  # inference layer from DNN training noise.
  beta_hat <- beta_true[rid, , drop = FALSE]

  lam <- sconjoint:::.sc_estimate_lambda(beta_hat, dX, Z_task, ridge_lambda = 1e-4)
  infl <- sconjoint:::.sc_influence_function(beta_hat, lam, dX, y)
  expect_length(infl$theta_hat, p)
  # Close to truth (oracle beta_hat, so only the DML correction noise matters)
  expect_true(max(abs(infl$theta_hat - c(0.5, -0.3, 0.2))) < 0.3)

  vc <- sconjoint:::.sc_cluster_vcov(infl$influence_raw, infl$theta_hat, rid)
  expect_equal(dim(vc$vcov), c(p, p))
  # Symmetric
  expect_true(max(abs(vc$vcov - t(vc$vcov))) < 1e-10)
  # PSD: all eigenvalues >= -tiny tolerance
  evs <- eigen(vc$vcov, symmetric = TRUE, only.values = TRUE)$values
  expect_true(min(evs) > -1e-8)

  iid <- sconjoint:::.sc_iid_vcov(infl$influence_raw, infl$theta_hat)
  ratio <- sconjoint:::.sc_dml_iid_ratio(vc$vcov, iid$vcov)
  # Clustering should inflate SE >= iid because tasks within a
  # respondent share beta; expect ratio at least near 1.
  expect_true(ratio$mean > 0.8)
})
