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

test_that("respondent-weighted theta/vcov match the explicit phi_i closed form (unbalanced T)", {
  set.seed(11)
  p <- 3L; p_Z <- 2L
  ## deliberately unbalanced, with T_i correlated with beta so the
  ## task- vs respondent-weighting gap is first-order (not just noise).
  T_vec <- rep(c(2L, 3L, 5L, 4L, 2L, 6L, 3L, 5L, 2L, 4L), 8L)  # M = 80
  M <- length(T_vec)
  rid <- rep(seq_len(M), times = T_vec)
  n <- length(rid)
  ## coordinate 1 of the truth depends on T_i -> weighting matters
  beta_true <- cbind(0.5 + 0.05 * (T_vec - mean(T_vec)), -0.3, 0.2)
  Z_mat  <- matrix(stats::rnorm(M * p_Z), M, p_Z)
  Z_task <- Z_mat[rid, , drop = FALSE]
  dX <- matrix(sample(c(-1, 0, 1), n * p, replace = TRUE), n, p)
  y  <- stats::rbinom(n, 1, stats::plogis(rowSums(dX * beta_true[rid, ])))
  beta_hat <- beta_true[rid, , drop = FALSE]            # oracle first stage

  lam  <- sconjoint:::.sc_estimate_lambda(beta_hat, dX, Z_task, ridge_lambda = 1e-4)
  infl <- sconjoint:::.sc_influence_function(beta_hat, lam, dX, y, respondent_id = rid)
  vc   <- sconjoint:::.sc_cluster_vcov(infl$influence_raw, infl$theta_hat, rid)

  ## explicit respondent-weighted closed form (independent recompute)
  IR <- infl$influence_raw
  phi_bar  <- t(vapply(sort(unique(rid)),
                       function(m) colMeans(IR[rid == m, , drop = FALSE]),
                       numeric(p)))
  theta_cf <- colMeans(phi_bar)                          # (1/M) sum_i phi_bar_i
  phi_c    <- sweep(phi_bar, 2L, theta_cf)
  vcov_cf  <- crossprod(phi_c) / (M * (M - 1))

  expect_equal(unname(infl$theta_hat), unname(theta_cf), tolerance = 1e-10)
  expect_equal(unname(vc$vcov),        unname(vcov_cf),  tolerance = 1e-10)

  ## the fixture must actually exercise the gap: task-weighted differs
  theta_task <- colMeans(IR)
  expect_true(max(abs(theta_task - theta_cf)) > 1e-4)
})

test_that("under constant T, respondent-weighted == task-weighted (conventions agree)", {
  set.seed(12)
  M <- 60L; T_i <- 4L; p <- 3L; p_Z <- 2L
  rid <- rep(seq_len(M), each = T_i); n <- M * T_i
  beta_true <- cbind(0.4, -0.2, 0.3)[rep(1, M), ]
  Z_task <- matrix(stats::rnorm(M * p_Z), M, p_Z)[rid, , drop = FALSE]
  dX <- matrix(sample(c(-1, 0, 1), n * p, replace = TRUE), n, p)
  y  <- stats::rbinom(n, 1, stats::plogis(rowSums(dX * beta_true[rid, ])))
  beta_hat <- beta_true[rid, , drop = FALSE]
  lam <- sconjoint:::.sc_estimate_lambda(beta_hat, dX, Z_task, ridge_lambda = 1e-4)

  ## respondent-weighted (with id) vs legacy task-weighted (no id)
  infl_rw <- sconjoint:::.sc_influence_function(beta_hat, lam, dX, y, respondent_id = rid)
  infl_tw <- sconjoint:::.sc_influence_function(beta_hat, lam, dX, y)
  expect_equal(infl_rw$theta_hat, infl_tw$theta_hat, tolerance = 1e-12)
  expect_equal(unname(infl_rw$theta_hat), unname(colMeans(infl_rw$influence_raw)),
               tolerance = 1e-12)

  ## respondent-weighted clustered vcov == legacy task-weighted closed form
  vc_rw <- sconjoint:::.sc_cluster_vcov(infl_rw$influence_raw, infl_rw$theta_hat, rid)
  ic <- sweep(infl_rw$influence_raw, 2L, infl_rw$theta_hat)
  cs <- rowsum(ic, group = rid)                          # within-respondent SUMS
  vc_tw <- (M / (M - 1)) * crossprod(cs) / (n * n)
  expect_equal(unname(vc_rw$vcov), unname(vc_tw), tolerance = 1e-12)

  ## iid diagnostic also coincides under balanced T
  iid_rw <- sconjoint:::.sc_iid_vcov(infl_rw$influence_raw, infl_rw$theta_hat, respondent_id = rid)
  iid_tw <- sconjoint:::.sc_iid_vcov(infl_rw$influence_raw, infl_rw$theta_hat)
  expect_equal(unname(iid_rw$vcov), unname(iid_tw$vcov), tolerance = 1e-12)
})
