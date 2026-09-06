test_that("horizontal loading projection removes only rotation tangents", {
  set.seed(9502)
  A <- matrix(rnorm(12), 4L, 3L)
  G <- matrix(rnorm(12), 4L, 3L)
  out <- sconjoint:::.scmix_horizontal_loading_gradient(A, G)
  expect_equal(out$gradient + out$vertical, G, tolerance = 1e-12)
  cross <- crossprod(A, out$gradient)
  expect_equal(cross, t(cross), tolerance = 1e-10)
  expect_equal(out$continuous_rotation_dimension, 3L)
})

test_that("joint KKT block contains every applicable component", {
  skip_if_not_installed("torch")
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 1L, hidden = integer(), mean_family = "linear",
    coefficient_scale = c(2, 0.5), a_bound = 3, weight_bound = 4)
  z <- torch::torch_tensor(matrix(c(-1, 0, 1, 2), ncol = 1L),
                           dtype = torch::torch_float64())
  dx <- torch::torch_tensor(matrix(seq(-0.8, 0.7, length.out = 8), 4L, 2L),
                            dtype = torch::torch_float64())
  y <- torch::torch_tensor(c(0, 1, 0, 1), dtype = torch::torch_float64())
  idx <- torch::torch_tensor(1:4, dtype = torch::torch_long())
  gh <- sconjoint:::.sc_gh_grid(1L, 7L)
  U <- torch::torch_tensor(gh$U, dtype = torch::torch_float64())
  logw <- torch::torch_tensor(log(gh$w), dtype = torch::torch_float64())
  data <- list(dx = dx, zt = z, yt = y, idx1 = idx, N = 4L)
  evaluated <- sconjoint:::.sc_mixed_evaluate_full_objective(
    net, data, U, logw, weight_decay = 1e-4,
    coefficient_scale = c(2, 0.5), kappa_bound = 10,
    alpha_bound = 5, a_bound = 3, weight_bound = 4, backward = TRUE)
  blocks <- evaluated$kkt$blocks
  expect_identical(blocks$block,
                   c("position_effect", "unpenalized_mean_reference",
                     "centered_mean_deviation", "horizontal_loadings",
                     "all_structural_coordinates"))
  component <- blocks[blocks$block != "all_structural_coordinates" &
                        blocks$applicable, ]
  joint <- blocks[blocks$block == "all_structural_coordinates", ]
  expect_equal(joint$dimension, sum(component$dimension))
  expected <- sqrt(sum((component$projected_kkt_rms *
                          sqrt(component$dimension))^2)) /
    sqrt(sum(component$dimension))
  expect_equal(joint$projected_kkt_rms, expected, tolerance = 1e-13)
})

test_that("v2.2 optimization gate fails closed before cutoff calibration", {
  blocks <- data.frame(
    block = c("unpenalized_mean_reference", "all_structural_coordinates"),
    applicable = TRUE, dimension = c(1L, 4L),
    projected_kkt_rms = c(1e-5, 2e-5),
    scaled_gradient_rms = c(1e-5, 2e-5))
  polish <- list(
    protocol_id = "paperps-inference-2026-09-06-v2.3",
    objective_definition_id = sconjoint:::.scmix_v22_objective_id,
    optimization_method_id = sconjoint:::.scmix_v22_polish_method,
    method = "full_batch_lbfgs", line_search = "strong_wolfe",
    dtype = "float64", completed = TRUE, accepted = TRUE,
    projection_changed = FALSE, lbfgs_nonworsening = TRUE,
    same_objective_all_stages = TRUE,
    objective_before_polish = 1.1, objective_after_polish = 1,
    objective_returned = 1, iterations = 2L,
    line_search_status = "optimizer_returned_strong_wolfe",
    returned_matches_accepted_objective = TRUE,
    kkt_before_polish = list(all_finite = TRUE),
    kkt_after_polish = list(all_finite = TRUE),
    kkt_returned = list(all_finite = TRUE, maximum = 2e-5,
                        blocks = blocks),
    absolute_gradient_returned = list(by_parameter = c(alpha_raw = 1e-5)))
  bounds <- list(mu_active = FALSE, kappa_active = FALSE,
                 a_active = FALSE, weight_active = FALSE,
                 alpha_active = FALSE,
                 alpha_diagnostics_applicable = TRUE)
  pending <- sconjoint:::.sc_mixed_v22_optimization_status(
    polish, bounds, kkt_cutoff = NULL)
  expect_true(pending$precalibration_fit_valid)
  expect_false(pending$optimization_gate_pass)
  expect_true("kkt_cutoff_calibration_pending" %in%
                pending$failure_reasons)
  expect_identical(names(pending$gate_evidence)[-1L],
                   paste0("F00", 1:6))
  expect_true(pending$gate_evidence$F001$pass)
  expect_identical(pending$gate_evidence$F005$state, "not_run")
  expect_false(pending$gate_evidence$F005$pass)
  expect_identical(pending$optimizer_subset_pass, FALSE)
  calibrated <- sconjoint:::.sc_mixed_v22_optimization_status(
    polish, bounds, kkt_cutoff = 3e-5, protocol_mode = "production",
    calibration_verified = TRUE)
  expect_true(calibrated$optimization_gate_pass)
  expect_true(calibrated$optimizer_subset_pass)
  unverified <- sconjoint:::.sc_mixed_v22_optimization_status(
    polish, bounds, kkt_cutoff = 3e-5, protocol_mode = "production",
    calibration_verified = FALSE)
  expect_false(unverified$optimization_gate_pass)
  expect_identical(unverified$gate_evidence$F005$state, "fail")
  expect_true("production_calibration_artifact_not_verified" %in%
                unverified$failure_reasons)
  expect_false(sconjoint:::.sc_mixed_v22_optimization_status(
    polish, bounds, kkt_cutoff = 3e-5,
    protocol_mode = "calibration",
    calibration_verified = TRUE)$optimization_gate_pass)

  polish$accepted <- FALSE
  polish$projection_changed <- TRUE
  projected <- sconjoint:::.sc_mixed_v22_optimization_status(
    polish, bounds, kkt_cutoff = 3e-5, protocol_mode = "production",
    calibration_verified = TRUE)
  expect_false(projected$optimization_gate_pass)
  expect_true("lbfgs_endpoint_required_projection" %in%
                projected$failure_reasons)
})

test_that("F001-F006 cannot all pass when a top-level optimizer gate fails", {
  blocks <- data.frame(
    block = c("unpenalized_mean_reference", "all_structural_coordinates"),
    applicable = TRUE, dimension = c(1L, 2L),
    projected_kkt_rms = c(1e-6, 2e-6),
    scaled_gradient_rms = c(1e-6, 2e-6))
  base <- list(
    protocol_id = "paperps-inference-2026-09-06-v2.3",
    objective_definition_id = sconjoint:::.scmix_v22_objective_id,
    optimization_method_id = sconjoint:::.scmix_v22_polish_method,
    method = "full_batch_lbfgs", line_search = "strong_wolfe",
    dtype = "float64", completed = TRUE, accepted = TRUE,
    projection_changed = FALSE, lbfgs_nonworsening = TRUE,
    same_objective_all_stages = TRUE,
    returned_matches_accepted_objective = TRUE,
    objective_before_polish = 1.1, objective_after_polish = 1,
    objective_returned = 1, iterations = 2L,
    line_search_status = "optimizer_returned_strong_wolfe",
    kkt_before_polish = list(all_finite = TRUE),
    kkt_after_polish = list(all_finite = TRUE),
    kkt_returned = list(all_finite = TRUE, maximum = 2e-6,
                        blocks = blocks),
    absolute_gradient_returned = list(by_parameter = c(alpha_raw = 1e-6)))
  bounds <- list(mu_active = FALSE, kappa_active = FALSE,
                 a_active = FALSE, weight_active = FALSE,
                 alpha_active = FALSE,
                 alpha_diagnostics_applicable = TRUE)
  tamper <- list(
    objective_definition_id = "wrong_objective",
    optimization_method_id = "wrong_method",
    same_objective_all_stages = FALSE,
    projection_changed = TRUE,
    returned_matches_accepted_objective = FALSE)
  for (field in names(tamper)) {
    bad <- base
    bad[[field]] <- tamper[[field]]
    status <- sconjoint:::.sc_mixed_v22_optimization_status(
      bad, bounds, kkt_cutoff = 1e-4, protocol_mode = "production",
      calibration_verified = TRUE)
    states <- vapply(status$gate_evidence[paste0("F00", 1:6)], `[[`,
                     character(1L), "state")
    expect_false(status$optimization_gate_pass, info = field)
    expect_true(any(states == "fail"), info = field)
  }
})

test_that("corrected output scaling preserves objective, KKT, and sequences", {
  skip_if_not_installed("torch")
  p <- 2L
  raw_dx <- matrix(c(-1, .2, .5, -.3, .8, .7, -.4, 1.1), 4L, p)
  Z <- matrix(c(-1, -.2, .4, 1), ncol = 1L)
  y <- c(1, 0, 1, 0)
  rid <- c(1, 1, 2, 2)
  grid <- sconjoint:::.sc_gh_grid(1L, 7L)
  alpha_raw <- c(.2, -.1)
  deviation_raw <- matrix(c(.35, -.25), p, 1L)
  A_raw <- matrix(c(.15, -.2), p, 1L)
  make_case <- function(scale) {
    net <- sconjoint:::.sc_build_mixed_network(
      p = p, p_Z = 1L, q = 1L, hidden = integer(), mean_family = "linear",
      coefficient_scale = scale, a_bound = 3, weight_bound = 2)
    state <- sconjoint:::.scmix_clone_network_state(net)
    state$alpha_raw <- alpha_raw * scale
    state$param_layer.weight <- deviation_raw * scale
    state$A <- A_raw * scale
    state$kappa_raw <- array(0.1, dim = 1L)
    sconjoint:::.scmix_load_network_state(net, state)
    dx <- sweep(raw_dx, 2L, scale, `/`)
    data <- list(
      dx = torch::torch_tensor(dx, dtype = torch::torch_float64()),
      zt = torch::torch_tensor(Z, dtype = torch::torch_float64()),
      yt = torch::torch_tensor(y, dtype = torch::torch_float64()),
      idx1 = torch::torch_tensor(rid, dtype = torch::torch_long()), N = 2L)
    U <- torch::torch_tensor(grid$U, dtype = torch::torch_float64())
    logw <- torch::torch_tensor(log(grid$w), dtype = torch::torch_float64())
    evaluated <- sconjoint:::.sc_mixed_evaluate_full_objective(
      net, data, U, logw, weight_decay = .03,
      coefficient_scale = scale, kappa_bound = 10, alpha_bound = 5,
      a_bound = 3, weight_bound = 2, backward = TRUE)
    evidence <- sconjoint:::.scmix_loading_invariance_evidence(
      net, raw_dx, y, Z, rid, grid, scale,
      analysis_signature = "scale-invariance-test", scope = "unit-fit",
      protocol_mode = "calibration")
    list(net = net, evaluated = evaluated, evidence = evidence)
  }
  one <- make_case(c(1, 1))
  two <- make_case(c(.25, 4))
  expect_equal(one$evaluated$objective, two$evaluated$objective,
               tolerance = 1e-12)
  expect_equal(one$evaluated$unpenalized, two$evaluated$unpenalized,
               tolerance = 1e-12)
  expect_equal(one$evaluated$kkt$blocks$projected_kkt_rms,
               two$evaluated$kkt$blocks$projected_kkt_rms,
               tolerance = 1e-11)
  expect_lte(one$evidence$maximum_absolute_probability_difference, 1e-14)
  expect_lte(two$evidence$maximum_absolute_probability_difference, 1e-14)
  expect_lte(one$evidence$maximum_absolute_sequence_loglik_difference, 1e-12)
  expect_lte(two$evidence$maximum_absolute_sequence_loglik_difference, 1e-12)
  expect_identical(one$evidence$state, "not_run")

  bounded <- two$net
  torch::with_no_grad({
    scale <- torch::torch_tensor(c(.25, 4), dtype = torch::torch_float64())$
      unsqueeze(2L)
    bounded$param_layer$weight$copy_(
      torch::torch_tensor(matrix(c(3, -4), p, 1L),
                          dtype = torch::torch_float64()) * scale)
  })
  sconjoint:::.sc_mixed_project_parameters(
    bounded, coefficient_scale = c(.25, 4), alpha_bound = 5,
    a_bound = 3, weight_bound = 2)
  output_raw <- sweep(as.matrix(torch::as_array(
    bounded$param_layer$weight$detach()$cpu())), 1L, c(.25, 4), `/`)
  expect_equal(as.numeric(output_raw), c(2, -2), tolerance = 1e-12)
})

test_that("float64 L-BFGS polish is settled, nonworsening, and audited", {
  skip_if_not_installed("torch")
  N <- 16L
  TT <- 4L
  rid <- rep(seq_len(N), each = TT)
  dx <- matrix(0, N * TT, 2L)
  Z <- matrix(rep(seq(-1, 1, length.out = N), each = TT), ncol = 1L)
  y <- rep(c(0, 1, 0, 1), N)
  fit <- sconjoint:::.sc_train_mixed_multistart(
    dx, y, Z, rid, sconjoint:::.sc_gh_grid(0L),
    hidden = integer(), mean_family = "constant", n_epochs = 2L,
    learning_rate = 0.01, weight_decay = 1e-4, seed = 1L,
    kkt_tol = NULL, polish_max_iter = 6L, mu_bound = 10,
    alpha_bound = 5, a_bound = 10, weight_bound = 10, n_starts = 1L)
  expect_identical(fit$dtype, "float64")
  expect_true(fit$lbfgs_completed)
  expect_true(fit$polish$accepted)
  expect_false(fit$polish$projection_changed)
  expect_lte(fit$objective_after_polish,
             fit$objective_before_polish +
               fit$polish$nonworsening_numerical_allowance)
  expect_equal(fit$penalized_loss, fit$polish$objective_returned,
               tolerance = 1e-12)
  expect_true(fit$optimization_gate_pass)
  expect_false(fit$v22_optimization_gate_pass)
  expect_identical(fit$kkt_cutoff_source, "calibration_only")
  expect_true("kkt_cutoff_calibration_pending" %in%
                fit$v22_optimization_failure_reasons)
  expect_true(all(is.finite(fit$kkt_returned$blocks$projected_kkt_rms[
    fit$kkt_returned$blocks$applicable])))
  expect_length(fit$start_artifacts, 1L)
  expect_true(is.list(fit$start_artifacts[[1L]]$state_dict))
  expect_identical(fit$start_artifacts[[1L]]$start_id, "start_001")
  expect_match(fit$start_artifacts[[1L]]$state_hash, "^scmix-state-v1-")
  expect_true(fit$start_artifacts[[1L]]$polished)
  expect_identical(fit$start_artifacts[[1L]]$dtype, "float64")
})

test_that("production cutoffs cannot bypass the calibration artifact", {
  calibration <- sconjoint:::.sc_v22_resolve_optimizer_calibration(
    protocol_mode = "calibration", kkt_tol = 1e-4,
    nested_fit_relative_cutoff = 2e-4)
  expect_false(calibration$verified)
  expect_identical(calibration$status, "calibration_only")
  expect_equal(calibration$kkt_cutoff, 1e-4)

  direct <- sconjoint:::.sc_v22_resolve_optimizer_calibration(
    protocol_mode = "production", kkt_tol = 1e-4)
  expect_false(direct$verified)
  expect_identical(direct$status, "direct_numeric_cutoff_rejected")
  expect_null(direct$kkt_cutoff)
  expect_error(
    sconjoint:::.sc_v22_require_verified_production_calibration(direct),
    "protocol-linked calibration artifact")

  arbitrary <- sconjoint:::.sc_v22_resolve_optimizer_calibration(
    protocol_mode = "production",
    calibration_artifact = list(constants = c(kkt_cutoff = 1e-4,
      nested_fit_relative_cutoff = 2e-4)),
    analysis_contract = list())
  expect_false(arbitrary$verified)
  expect_identical(arbitrary$status, "calibration_artifact_unavailable")
  expect_null(arbitrary$nested_fit_relative_cutoff)
  expect_invisible(
    sconjoint:::.sc_v22_require_verified_production_calibration(calibration))
})

test_that("F007 uses the frozen relative mean-objective violation", {
  gate <- sconjoint:::.sc_comp_nested_objective_gate(
    candidate_penalized_nll = 12, pooled_penalized_nll = 10,
    tolerance = 0.1, applicable = TRUE, v22_cutoff = 0.25,
    v22_applicable = TRUE, protocol_mode = "production",
    calibration_verified = TRUE, calibration_hash = "unit-test")
  expect_equal(gate$relative_violation, 0.2)
  expect_false(gate$pass)
  expect_true(gate$v22_pass)
  expect_identical(gate$gate_evidence$F007$statistic,
                   "nested_fit_relative_violation")
})
