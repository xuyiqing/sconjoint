## Tests for the integrated-likelihood mixed-logit estimator (scmix).
##
## Ground-truth strategy: simulate from the maintained model
## beta_i = mu(Z_i) + A u_i and check (a) the quadrature grid,
## (b) the Fisher identity, (c) orthogonality of the theta score,
## (d) recovery of the loading scale and location at loose tolerances,
## (e) polarization boundary handling.  The heavier recovery /
## coverage evidence lives in dev/sim-mixed-facevalidity.R.

test_that("Gauss-Hermite grid integrates Gaussian moments exactly", {
  gh0 <- sconjoint:::.sc_gh_grid(q = 0L)
  expect_equal(dim(gh0$U), c(1L, 0L))
  expect_identical(gh0$w, 1)
  expect_identical(gh0$metadata$method, "exact")

  gh <- sconjoint:::.sc_gh_grid(q = 1L, n_nodes = 15L)
  expect_equal(sum(gh$w), 1, tolerance = 1e-12)
  expect_equal(sum(gh$w * gh$U[, 1]), 0, tolerance = 1e-10)
  expect_equal(sum(gh$w * gh$U[, 1]^2), 1, tolerance = 1e-10)
  expect_equal(sum(gh$w * gh$U[, 1]^4), 3, tolerance = 1e-8)

  gh2 <- sconjoint:::.sc_gh_grid(q = 2L, n_nodes = 9L)
  expect_equal(sum(gh2$w), 1, tolerance = 1e-12)
  expect_equal(colSums(gh2$w * gh2$U^2), c(1, 1), tolerance = 1e-8)
  expect_equal(sum(gh2$w * gh2$U[, 1] * gh2$U[, 2]), 0, tolerance = 1e-10)
  expect_identical(gh2$metadata$method, "gauss-hermite")

  expect_error(
    sconjoint:::.sc_qmc_grid(q = 1L, n_draws = 17L, antithetic = TRUE),
    "must be even"
  )
  expect_error(
    sconjoint:::.sc_mixed_grid(
      q = 4L, integration = "qmc", n_draws = 17L,
      antithetic = TRUE),
    "must be even"
  )
  expect_error(
    sconjoint:::.sc_mixed_grid(q = 0L, antithetic = NA),
    "logical scalars"
  )
})

test_that("optimization status is fresh-state and fail-closed", {
  gradient <- list(total = 1e-6, structural = 5e-7, sieve = 1e-6)
  bounds <- list(mu_active = FALSE, kappa_active = FALSE,
                 a_active = FALSE, weight_active = FALSE)
  fresh <- sconjoint:::.sc_mixed_optimization_status(
    final_loss = 1, final_nll = 0.9, previous_loss = 1 + 1e-8,
    gradient = gradient, opt_tol = 1e-6, grad_tol = 1e-4,
    bounds = bounds, state_restored = FALSE)
  expect_true(fresh$optimization_gate_pass)
  expect_true(fresh$objective_finite)
  expect_true(fresh$structural_stationarity_met)
  expect_identical(
    fresh$criterion_diagnostic_source,
    "returned_state_vs_immediately_preceding_attained_state")

  restored <- sconjoint:::.sc_mixed_optimization_status(
    final_loss = 1, final_nll = 0.9, previous_loss = 1,
    gradient = gradient, opt_tol = 1e-6, grad_tol = 1e-4,
    bounds = bounds, state_restored = TRUE)
  expect_false(restored$criterion_tolerance_met)
  expect_false(restored$optimization_gate_pass)
  expect_true(is.na(restored$last_relative_change))
  expect_match(restored$criterion_diagnostic_source, "state_restoration")

  on_bound <- sconjoint:::.sc_mixed_optimization_status(
    final_loss = 1, final_nll = 0.9, previous_loss = 1,
    gradient = gradient, opt_tol = 1e-6, grad_tol = 1e-4,
    bounds = list(mu_active = FALSE, kappa_active = FALSE,
                  a_active = TRUE, weight_active = FALSE))
  expect_false(on_bound$optimization_gate_pass)
  expect_true("parameter_bound_active" %in% on_bound$failure_reasons)

  missing_compact <- sconjoint:::.sc_mixed_optimization_status(
    final_loss = 1, final_nll = 0.9, previous_loss = 1 + 1e-8,
    gradient = gradient, opt_tol = 1e-6, grad_tol = 1e-4,
    bounds = list(mu_active = FALSE, kappa_active = FALSE))
  expect_false(missing_compact$optimization_gate_pass)
  expect_true("compact_bound_diagnostics_incomplete" %in%
                missing_compact$failure_reasons)

  nonfinite <- sconjoint:::.sc_mixed_optimization_status(
    final_loss = Inf, final_nll = 0.9, previous_loss = 1,
    gradient = gradient, opt_tol = 1e-6, grad_tol = 1e-4,
    bounds = bounds)
  expect_false(nonfinite$optimization_gate_pass)
  expect_true("nonfinite_objective" %in% nonfinite$failure_reasons)
})

test_that("analysis signatures are stable and exclude fitted parameters", {
  dx <- matrix(c(-1, 1, 0, 1), ncol = 1L,
               dimnames = list(NULL, "x"))
  Z <- matrix(c(0, 0, 1, 1), ncol = 1L,
              dimnames = list(NULL, "z"))
  args <- list(deltaX = dx, y = c(0, 1, 1, 0), Z = Z,
               respondent_id = c("a", "a", "b", "b"),
               fold_id = c(1L, 1L, 2L, 2L),
               specification = list(q = 0L, hidden = 4L))
  s1 <- do.call(sconjoint:::.sc_analysis_signature, args)
  s2 <- do.call(sconjoint:::.sc_analysis_signature, args)
  expect_identical(s1, s2)
  expect_match(s1, "^scmix-v1-[0-9a-f]{16}$")
  changed <- args
  changed$fold_id <- rev(changed$fold_id)
  expect_false(identical(
    s1, do.call(sconjoint:::.sc_analysis_signature, changed)))
})

test_that("compact projections use raw loading units and bound sieve weights", {
  skip_if_not_installed("torch")
  scale <- c(2, 0.5)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 1L, hidden = 3L, a_init_sd = 100,
    a_bound = 0.25, weight_bound = 0.05,
    coefficient_scale = scale
  )
  A_internal <- as.matrix(torch::as_array(net$A))
  A_raw <- sweep(A_internal, 1L, scale, `/`)
  expect_lte(sqrt(sum(A_raw^2)), 0.25 + 1e-6)
  sieve <- setdiff(names(net$parameters), c("A", "kappa_raw"))
  weight_max <- max(vapply(sieve, function(nm) {
    max(abs(torch::as_array(net$parameters[[nm]]$detach())))
  }, numeric(1L)))
  expect_lte(weight_max, 0.05 + 1e-7)
})

test_that("moderator preprocessing is respondent weighted", {
  Z <- matrix(c(0, 10, 10, 10), ncol = 1L,
              dimnames = list(NULL, "z"))
  rid <- c("one", "two", "two", "two")
  tr <- sconjoint:::.sc_fit_z_transform(Z, rid)
  expect_equal(unname(tr$center), 5)
  expect_equal(unname(tr$scale), stats::sd(c(0, 10)))
  expect_identical(tr$n_respondents, 2L)
  expect_identical(tr$weighting, "respondent")
  Zs <- sconjoint:::.sc_apply_z_transform(Z, tr)
  expect_equal(mean(Zs[!duplicated(rid), 1L]), 0, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(tr$center, colMeans(Z))))
})

test_that("mixed likelihood averages respondent sequence log likelihoods", {
  skip_if_not_installed("torch")
  dev <- torch::torch_device("cpu")
  zt <- torch::torch_zeros(4L, 1L, dtype = torch::torch_float(), device = dev)
  dx <- torch::torch_zeros(4L, 1L, dtype = torch::torch_float(), device = dev)
  yt <- torch::torch_tensor(c(1, 0, 0, 0), dtype = torch::torch_float(),
                            device = dev)
  idx1 <- torch::torch_tensor(c(1L, 2L, 2L, 2L), dtype = torch::torch_long(),
                              device = dev)
  ## U is unused on the exact q = 0 path; use a nonempty placeholder for
  ## compatibility with torch builds that reject zero-column tensors.
  U <- torch::torch_zeros(1L, 1L, dtype = torch::torch_float(), device = dev)
  logw <- torch::torch_zeros(1L, dtype = torch::torch_float(), device = dev)
  p <- 0.8
  net <- list(
    q = 0L,
    get_beta = function(z) z,
    get_kappa = function() torch::torch_tensor(stats::qlogis(p),
                                                dtype = torch::torch_float(),
                                                device = dev)
  )
  got <- as.numeric(sconjoint:::.sc_mixed_nll(
    net, dx, zt, yt, U, logw, idx1, N = 2L)$item())
  respondent_mean <- -mean(c(log(p), 3 * log1p(-p)))
  task_mean <- -mean(c(log(p), rep(log1p(-p), 3L)))
  expect_equal(got, respondent_mean, tolerance = 1e-6)
  expect_gt(abs(got - task_mean), 0.1)
})

.mk_q0_kappa_data <- function(N = 30L, T = 5L) {
  rid <- rep(seq_len(N), each = 2L * T)
  task <- rep(rep(seq_len(T), each = 2L), N)
  profile <- rep(1:2, N * T)
  y1 <- rep(c(1, 1, 1, 1, 0), length.out = N * T)
  choice <- as.numeric(c(rbind(y1, 1 - y1)))
  data.frame(
    resp_id = rid,
    task_id = task,
    profile_id = profile,
    choice = choice,
    a = factor(rep("no", 2L * N * T), levels = c("no", "yes")),
    z = rep(rep(seq(-2, 2, length.out = N), each = T), each = 2L)
  )
}

test_that("q=0 fit estimates kappa and retains full and fold fits", {
  skip_if_not_installed("torch")
  dat <- .mk_q0_kappa_data()
  fit <- scmix(
    choice ~ a | z, dat, q = 0L, K = 2L, hidden = 4L,
    n_epochs = 200L, learning_rate = 0.05, weight_decay = 0,
    n_starts = 2L, mu_bound = 2, kappa_bound = 3,
    opt_tol = 1e-6, grad_tol = 1e-3, seed = 20260823L
  )
  expect_equal(fit$kappa_hat, stats::qlogis(0.8), tolerance = 0.2)
  expect_length(fit$kappa_folds, 2L)
  expect_true(all(is.finite(fit$kappa_folds)))
  expect_equal(dim(fit$A_hat), c(1L, 0L))
  expect_equal(fit$Sigma_hat, matrix(0, 1L, 1L))
  expect_identical(fit$integration$method, "exact")
  expect_identical(fit$integration$n_points, 1L)
  expect_true(inherits(fit$full_net, "nn_module"))
  expect_s3_class(fit$network_state_full, "scmix_network_state")
  expect_length(fit$network_state_folds, fit$K)
  expect_equal(fit$network_state_full$integration_grid$U, fit$gh$U)
  expect_equal(fit$network_state_full$integration_grid$w, fit$gh$w)
  expect_equal(
    unname(scmix_predict_network(fit$network_state_full, fit$Z)),
    unname(fit$mu_full), tolerance = 1e-6)
  expect_equal(dim(fit$mu_full), dim(fit$mu_hat))
  expect_length(fit$mu_all_folds, 2L)
  expect_true(all(vapply(fit$mu_all_folds,
                         function(x) identical(dim(x), dim(fit$mu_hat)),
                         logical(1L))))
  expect_equal(nrow(fit$optimization$full$starts), 2L)
  expect_equal(fit$optimization$full$objective,
               max(fit$optimization$full$starts$objective))
  expect_false(fit$optimization$full$global_optimality_gap_known)
  expect_identical(
    fit$computational_optimization_gate_pass,
    isTRUE(fit$optimization$full$optimization_gate_pass) &&
      all(fit$optimization$gate_by_fold))
  expect_true(all(abs(fit$mu_full) <= fit$bounds$mu_raw_by_coordinate + 1e-6))
  expect_true(is.character(fit$analysis_signature) &&
                nzchar(fit$analysis_signature))
  expect_equal(fit$bounds$a, 10)
  expect_equal(fit$bounds$weight, 10)

  ## Raw Z is retained, while every fold transform is learned from unique
  ## held-in respondents and the accessor reconstructs each network input.
  expect_equal(fit$Z[, 1L], rep(seq(-2, 2, length.out = 30L), each = 5L))
  for (k in seq_len(fit$K)) {
    in_k <- fit$fold_id != k
    first <- !duplicated(fit$respondent_id[in_k])
    expected <- mean(fit$Z[in_k, 1L][first])
    expect_equal(unname(fit$z_transform_folds[[k]]$center), expected)
  }
  Z_oof <- sconjoint:::.scmix_z_for_rows(fit, source = "crossfit")
  Z_full <- sconjoint:::.scmix_z_for_rows(fit, source = "full")
  expect_equal(dim(Z_oof), dim(fit$Z))
  expect_equal(dim(Z_full), dim(fit$Z))
  first <- !duplicated(fit$respondent_id)
  expect_equal(colMeans(Z_full[first, , drop = FALSE]), 0, tolerance = 1e-12)

  expect_error(
    scmix(choice ~ a | z, dat, q = 0.5, K = 2L, n_epochs = 1L),
    "integer between 0 and p - 1"
  )
})

## The shared fixture (.mk_mixed_fixture / .fit_mixed_fixture) lives in
## helper-mixed-fixture.R so test-mixed-quantities.R can reuse it.

test_that("scmix runs, returns sane structure, and A is scale-recovered", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  expect_s3_class(fit, "scmix")
  expect_equal(ncol(fit$mu_hat), 2L)
  expect_equal(dim(fit$mu_full), dim(fit$mu_hat))
  expect_length(fit$mu_all_folds, fit$K)
  expect_length(fit$kappa_folds, fit$K)
  expect_length(fit$sd_dx_folds, fit$K)
  expect_equal(fit$sd_dx_full, fit$sd_dx)
  expect_equal(
    unname(fit$sd_dx),
    unname(sconjoint:::.sc_comp_fit_dx_scale(
      fit$deltaX, fit$respondent_id)$scale)
  )
  for (k in seq_len(fit$K)) {
    train <- fit$fold_id != k
    expected_scale <- sconjoint:::.sc_comp_fit_dx_scale(
      fit$deltaX[train, , drop = FALSE], fit$respondent_id[train])$scale
    expect_equal(unname(fit$sd_dx_folds[[k]]), unname(expected_scale))
    A_internal <- as.matrix(torch::as_array(fit$nets[[k]]$A))
    expect_equal(sweep(fit$A_folds[[k]], 1L, fit$sd_dx_folds[[k]], `*`),
                 A_internal, tolerance = 1e-6)
  }
  expect_true(is.finite(fit$kappa_hat))
  expect_equal(fit$Sigma_hat, tcrossprod(fit$A_hat))
  expect_lte(sqrt(sum(fit$A_hat^2)), fit$bounds$a + 1e-6)
  expect_true(all(vapply(fit$A_folds, function(A) {
    sqrt(sum(A^2)) <= fit$bounds$a + 1e-6
  }, logical(1L))))
  expect_false(fit$optimization$diagnostics_are_certificates)
  expect_equal(fit$N, 250L)
  ## rotation-invariant loading scale within +-60% at this tiny size
  sd_hat <- sqrt(diag(Reduce(`+`, lapply(fit$A_folds, tcrossprod)) /
                        length(fit$A_folds)))
  sd_true <- sqrt(diag(tcrossprod(fw$fx$A_true)))
  expect_true(all(abs(sd_hat - sd_true) < 0.6 * pmax(sd_true, 0.3)))
})

test_that("Fisher identity: score equals numeric d logL / d mu", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  worst <- sconjoint:::.scmix_check_fisher(fw$fit, n_check = 3L, coord = 1L)
  expect_lt(worst, 1e-6)
})

test_that("theta score is orthogonal: O(delta^2) vs O(delta) plug-in", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  chk <- sconjoint:::.scmix_check_orthogonality(
    fw$fit, deltas = c(0.2, 0.1), coord = 1L, n_bins = 15L, M = 200L, seed = 2L)
  ## plug-in moves one-for-one; orthogonal estimate moves by far less
  expect_equal(chk$plugin_shift, c(0.2, 0.1), tolerance = 1e-10)
  expect_true(all(abs(chk$orth_shift) < 0.25 * abs(chk$plugin_shift)))
})

test_that("scmix_theta is near the truth with sane inference output", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  truth <- colMeans(fw$fx$mu_fun(matrix(stats::runif(2e5, -1, 1), ncol = 2)))
  th <- scmix_theta(fit, n_bins = 15L, M = 200L, seed = 2L)
  ## loose per-draw check at this tiny size; the systematic
  ## de-attenuation-vs-pooled evidence is the replicated simulation's
  ## job (dev/sim-mixed-facevalidity.R), not a single-draw unit test's
  expect_true(all(abs(th$estimate - truth) < 4 * th$se))
  expect_true(all(th$se > 0))
  expect_true(all(is.finite(th$psi)))
  expect_gt(stats::sd(th$psi[, 1]), 0)
})

test_that("polarization respects [0,1] and floors zero-variance coords", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  pol <- suppressWarnings(
    scmix_polarization(fw$fit, n_bins = 15L, M = 200L, seed = 2L))
  ok <- !is.na(pol$estimate)
  expect_true(all(pol$estimate[ok] > -0.02 & pol$estimate[ok] < 1.02))
  ## forcing an absurd floor: every coordinate floored -> all NA + warning
  expect_warning(
    pol10 <- scmix_polarization(fw$fit, n_bins = 15L, M = 200L, seed = 2L,
                                sd_floor = 10),
    "floor"
  )
  expect_true(all(is.na(pol10$estimate)))
})

test_that("counterfactual share accepts named contrasts and stays in [0,1]", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  vc <- scmix_counterfactual(fw$fit, contrast = c(a1yes = 1, a2yes = -1),
                             n_bins = 15L, M = 200L, seed = 2L)
  expect_true(vc$estimate > 0 && vc$estimate < 1)
  expect_error(
    scmix_counterfactual(fw$fit, contrast = c(bogus = 1)),
    "unknown contrast"
  )
})

test_that("print methods run quietly", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  expect_output(print(fw$fit), "Integrated-likelihood")
  th <- scmix_theta(fw$fit, n_bins = 10L, M = 100L, seed = 2L)
  expect_output(print(th), "orthogonal estimate")
})

test_that("loading score matches finite-difference d logL / dA", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  sc <- sconjoint:::.scmix_scores(fit)
  eps <- 1e-5
  worst <- 0
  for (i in 1:2) {
    for (kr in 1:2) {
      f_up <- fit; f_dn <- fit
      k_fold <- sc$fold_resp[i]
      dA <- matrix(0, 2, 1); dA[kr, 1] <- eps
      f_up$A_folds[[k_fold]] <- fit$A_folds[[k_fold]] + dA
      f_dn$A_folds[[k_fold]] <- fit$A_folds[[k_fold]] - dA
      ll_up <- sconjoint:::.scmix_scores(f_up)$loglik[i]
      ll_dn <- sconjoint:::.scmix_scores(f_dn)$loglik[i]
      fd <- (ll_up - ll_dn) / (2 * eps)
      worst <- max(worst, abs(fd - sc$S_A[i, kr]))
    }
  }
  expect_lt(worst, 1e-6)
})

test_that("estimates are invariant to a global loading sign flip", {
  skip_if_not_installed("torch")
  fw <- .fit_mixed_fixture()
  fit <- fw$fit
  f2 <- fit
  f2$A_folds <- lapply(fit$A_folds, function(A) -A)
  th1 <- scmix_theta(fit, n_bins = 15L, M = 400L, seed = 2L)
  th2 <- scmix_theta(f2, n_bins = 15L, M = 400L, seed = 2L)
  expect_equal(unname(th1$estimate), unname(th2$estimate), tolerance = 1e-10)
  expect_equal(unname(th1$se), unname(th2$se), tolerance = 1e-10)
})
