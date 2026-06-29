test_that("survey-weighted DML theta/vcov match respondent-level closed form", {
  set.seed(131)
  p <- 3L; p_Z <- 2L
  T_vec <- c(2L, 4L, 3L, 5L, 2L, 6L, 3L, 4L)
  M <- length(T_vec)
  rid <- rep(seq_len(M), times = T_vec)
  n <- length(rid)
  survey_w <- rep(seq(0.5, 2.25, length.out = M), times = T_vec)
  beta_true <- cbind(0.2 + 0.08 * seq_len(M), -0.4, 0.3)
  Z_task <- matrix(stats::rnorm(M * p_Z), M, p_Z)[rid, , drop = FALSE]
  dX <- matrix(sample(c(-1, 0, 1), n * p, replace = TRUE), n, p)
  y <- stats::rbinom(n, 1, stats::plogis(rowSums(dX * beta_true[rid, ])))
  beta_hat <- beta_true[rid, , drop = FALSE]

  lam <- sconjoint:::.sc_estimate_lambda(beta_hat, dX, Z_task, ridge_lambda = 1e-4)
  infl <- sconjoint:::.sc_influence_function(
    beta_hat, lam, dX, y, respondent_id = rid,
    respondent_weights = survey_w
  )
  vc <- sconjoint:::.sc_cluster_vcov(
    infl$influence_raw, infl$theta_hat, rid,
    respondent_weights = survey_w
  )

  IR <- infl$influence_raw
  phi_bar <- t(vapply(sort(unique(rid)),
                      function(m) colMeans(IR[rid == m, , drop = FALSE]),
                      numeric(p)))
  w_resp <- survey_w[match(sort(unique(rid)), rid)]
  a <- w_resp / sum(w_resp)
  theta_cf <- as.numeric(crossprod(a, phi_bar))
  phi_c <- sweep(phi_bar, 2L, theta_cf)
  vcov_cf <- (M / (M - 1)) * crossprod(phi_c * a)

  expect_equal(unname(infl$theta_hat), unname(theta_cf), tolerance = 1e-10)
  expect_equal(unname(vc$vcov), unname(vcov_cf), tolerance = 1e-10)
  expect_true(max(abs(colMeans(phi_bar) - theta_cf)) > 1e-4)
})

test_that("survey weights feed production-facing plugin quantities", {
  B_resp <- rbind(
    c(a =  0.5, b = -0.4),
    c(a =  1.0, b = -0.4),
    c(a = -0.2, b = -0.4),
    c(a =  0.3, b = -0.4)
  )
  resp <- rep(seq_len(nrow(B_resp)), each = 2L)
  B <- B_resp[resp, , drop = FALSE]
  w <- rep(c(1, 2, 3, 4), each = 2L)
  fit <- structure(list(
    beta_hat = B,
    beta_hat_dnn = B,
    attr_names = c("a", "b"),
    attr_vars = c("a", "b"),
    attr_map = list(a = 1L, b = 2L),
    factor_levels = list(),
    z_names = "z",
    Z = matrix(0, nrow(B), 1L, dimnames = list(NULL, "z")),
    respondent_id = resp,
    respondent_weights = w,
    deltaX = matrix(0, nrow(B), 2L, dimnames = list(NULL, c("a", "b"))),
    theta = c(a = stats::weighted.mean(B_resp[, "a"], c(1, 2, 3, 4)),
              b = stats::weighted.mean(B_resp[, "b"], c(1, 2, 3, 4))),
    vcov = diag(2) * 0.01
  ), class = c("sc_fit", "list"))

  q_frac <- sc_fraction_preferring(fit, threshold = 0)
  expect_equal(q_frac$estimate$frac_positive[1L], 0.7, tolerance = 1e-12)
  expect_equal(q_frac$estimate$frac_negative[1L], 0.3, tolerance = 1e-12)

  q_cf <- sc_counterfactual(fit, A = list(a = 1), B = list(a = 0),
                            vartype = "plugin")
  expect_equal(q_cf$estimate,
               stats::weighted.mean(stats::plogis(B_resp[, "a"]), c(1, 2, 3, 4)),
               tolerance = 1e-12)

  q_comp <- sc_compensating(fit, benefit = "a", cost = "b")
  ratio <- -B_resp[, "a"] / B_resp[, "b"]
  ratio_task <- ratio[resp]
  q_lo <- stats::quantile(ratio_task, 0.01, names = FALSE)
  q_hi <- stats::quantile(ratio_task, 0.99, names = FALSE)
  ratio <- pmin(pmax(ratio, q_lo), q_hi)
  expect_equal(q_comp$estimate,
               stats::weighted.mean(ratio, c(1, 2, 3, 4)),
               tolerance = 1e-12)
  expect_equal(q_comp$details$frac_compensated, 0.3, tolerance = 1e-12)
})
