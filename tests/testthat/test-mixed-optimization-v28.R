test_that("v2.8 treats inferior starts as diagnostics", {
  starts <- data.frame(
    start_id = c("exact", "perturbed"),
    penalized_mean_nll = c(4.7, 4.8),
    objective_finite = c(TRUE, TRUE),
    dtype = c("float64", "float64"),
    lbfgs_completed = c(TRUE, TRUE),
    lbfgs_accepted = c(TRUE, TRUE),
    lbfgs_nonworsening = c(TRUE, TRUE),
    projection_diagnostic_complete = c(TRUE, TRUE),
    projection_changed = c(FALSE, FALSE),
    bound_activity = c(FALSE, FALSE),
    gradient_contract_pass = c(TRUE, TRUE),
    kkt_maximum = c(9e-6, 2e-4), stringsAsFactors = FALSE)
  out <- sconjoint:::.scmix_v28_start_surface(starts)
  expect_true(out$fit_gate_pass)
  expect_identical(out$selected_start_id, "exact")
  expect_identical(out$n_near, 1L)
  expect_identical(out$near_optimum_status,
                   "not_applicable_singleton_near_set")
  expect_true(out$surface$inferior_start_diagnostic[[2L]])
  expect_false(out$surface$target_comparison_eligible[[2L]])
})

test_that("v2.8 includes only eligible near starts in target comparisons", {
  starts <- data.frame(
    start_id = c("best", "near", "far", "invalid_lower"),
    penalized_mean_nll = c(2, 2 + 5e-7, 2.01, 1.9),
    objective_finite = rep(TRUE, 4L), dtype = rep("float64", 4L),
    lbfgs_completed = rep(TRUE, 4L),
    lbfgs_accepted = rep(TRUE, 4L),
    lbfgs_nonworsening = rep(TRUE, 4L),
    projection_diagnostic_complete = rep(TRUE, 4L),
    projection_changed = rep(FALSE, 4L),
    bound_activity = c(FALSE, FALSE, FALSE, TRUE),
    gradient_contract_pass = rep(TRUE, 4L),
    kkt_maximum = rep(1e-6, 4L), stringsAsFactors = FALSE)
  out <- sconjoint:::.scmix_v28_start_surface(starts)
  expect_identical(out$selected_start_id, "best")
  expect_equal(out$n_near, 2L)
  expect_identical(which(out$surface$target_comparison_eligible), 1:2)
  expect_true(out$lower_objective_inadmissible)
  expect_false(out$fit_gate_pass)
})

test_that("v2.8 KKT contract exposes RMS gate and max-coordinate diagnostic", {
  blocks <- c("position_effect", "unpenalized_mean_reference",
              "centered_mean_deviation", "horizontal_loadings",
              "all_structural_coordinates")
  kkt <- list(
    all_finite = TRUE,
    blocks = data.frame(
      block = blocks, applicable = c(TRUE, TRUE, TRUE, FALSE, TRUE),
      dimension = c(1L, 2L, 3L, 0L, 6L),
      projected_kkt_rms = c(1e-6, 2e-6, 3e-6, NA, 1),
      stringsAsFactors = FALSE),
    raw_residual_vectors = list(
      position_effect = 1e-6,
      unpenalized_mean_reference = c(1e-6, 2e-6),
      centered_mean_deviation = c(1e-6, 2e-6, 4e-6),
      horizontal_loadings = numeric()))
  out <- sconjoint:::.scmix_v28_kkt_contract(kkt, cutoff = 1e-5)
  expect_true(out$pass)
  expect_equal(out$maximum_coordinate_diagnostic, 4e-6)
  expect_match(out$scale_claim, "DeltaX preprocessing")
  expect_false(out$blocks$gate_metric[
    out$blocks$block == "all_structural_coordinates"])
})

test_that("v2.8 rejects an incomplete dtype audit", {
  starts <- data.frame(
    start_id = c("a", "b"), penalized_mean_nll = c(1, 2),
    objective_finite = c(TRUE, TRUE), dtype = c("float64", NA_character_),
    lbfgs_completed = c(TRUE, TRUE), lbfgs_accepted = c(TRUE, TRUE),
    lbfgs_nonworsening = c(TRUE, TRUE),
    projection_diagnostic_complete = c(TRUE, TRUE),
    projection_changed = c(FALSE, FALSE),
    bound_activity = c(FALSE, FALSE),
    gradient_contract_pass = c(TRUE, TRUE),
    kkt_maximum = c(1e-6, 1e-6), stringsAsFactors = FALSE)
  expect_error(sconjoint:::.scmix_v28_start_surface(starts),
               "invalid types")
})

test_that("v2.8.1 directional gradient ladder certifies a smooth linear fit", {
  skip_if_not_installed("torch")
  set.seed(11)
  respondents <- rep(sprintf("r%02d", 1:8), each = 3)
  deltaX <- matrix(rnorm(length(respondents) * 2), ncol = 2)
  Z <- matrix(rnorm(length(respondents) * 2), ncol = 2)
  y <- rep(c(0, 1, 0), 8)
  scale <- c(1.3, 0.8)
  grid <- sconjoint:::.sc_mixed_grid(q = 1L, integration = "gh", n_nodes = 7L,
                         n_draws = 1L, seed = 1L,
                         antithetic = TRUE, scramble = FALSE)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 2L, q = 1L, hidden = integer(),
    mean_family = "linear", coefficient_scale = scale,
    compute_dtype = "float64")
  out <- sconjoint:::.scmix_v28_gradient_contract_audit(
    net, deltaX, y, Z, respondents, grid, weight_decay = 0.01,
    coefficient_scale = scale, kappa_bound = 10, alpha_bound = 5,
    a_bound = 10, weight_bound = 10,
    steps = c(1e-4, 3e-5, 1e-5, 3e-6, 1e-6), seed = 22L,
    relative_tolerance = 5e-4, absolute_tolerance = 1e-7)
  expect_true(out$state_restored_exactly)
  expect_true(out$overall_pass)
  expect_identical(out$protocol, "paperps-gradient-contract-v2.8.1")
  expect_true(all(c("position_effect", "unpenalized_mean_reference",
                    "centered_mean_deviation", "horizontal_loadings") %in%
                  names(out$pass_by_block)))
})

test_that("v2.8.1 gradient directions preserve rank-one ReLU shapes", {
  skip_if_not_installed("torch")
  net <- sconjoint:::.sc_build_mixed_network(
    p = 3L, p_Z = 2L, q = 1L, hidden = 4L,
    mean_family = "relu", coefficient_scale = c(1.2, 0.8, 1.5),
    compute_dtype = "float64")
  bank <- sconjoint:::.scmix_v28_direction_bank(
    net, coefficient_scale = c(1.2, 0.8, 1.5),
    kappa_bound = 10, alpha_bound = 5, a_bound = 10,
    weight_bound = 10, seed = 912L)
  expect_true("hidden.0.bias" %in%
                names(bank$centered_mean_deviation))
  for (block in bank) {
    for (nm in names(block)) {
      template <- sconjoint:::.scmix_parameter_array(net$parameters[[nm]])
      expect_length(block[[nm]], length(template))
      expect_identical(dim(block[[nm]]), dim(template))
      expect_true(all(is.finite(block[[nm]])))
    }
  }

  original <- sconjoint:::.scmix_v28_copy_parameter_arrays(net)
  for (block in bank) {
    candidate <- original
    for (nm in names(block)) {
      candidate[[nm]] <- candidate[[nm]] + 1e-4 * block[[nm]]
    }
    expect_silent(sconjoint:::.scmix_v28_load_parameter_arrays(net, candidate))
    observed <- sconjoint:::.scmix_v28_copy_parameter_arrays(net)
    expect_identical(lapply(observed, dim), lapply(candidate, dim))
  }
  expect_silent(sconjoint:::.scmix_v28_load_parameter_arrays(net, original))

  malformed <- original
  malformed$hidden.0.bias <- matrix(malformed$hidden.0.bias, nrow = 1L)
  expect_error(
    sconjoint:::.scmix_v28_load_parameter_arrays(net, malformed),
    "state shape does not match")

  set.seed(11)
  respondent_id <- rep(sprintf("r%02d", 1:8), each = 3)
  deltaX <- matrix(rnorm(length(respondent_id) * 3), ncol = 3)
  Z <- matrix(rnorm(length(respondent_id) * 2), ncol = 2)
  y <- rep(c(0, 1, 0), 8)
  grid <- sconjoint:::.sc_mixed_grid(
    q = 1L, integration = "gh", n_nodes = 7L, n_draws = 1L,
    seed = 1L, antithetic = TRUE, scramble = FALSE)
  audit <- sconjoint:::.scmix_v28_gradient_contract_audit(
    net, deltaX, y, Z, respondent_id, grid, weight_decay = 0.01,
    coefficient_scale = c(1.2, 0.8, 1.5), kappa_bound = 10,
    alpha_bound = 5, a_bound = 10, weight_bound = 10,
    steps = c(1e-4, 3e-5, 1e-5, 3e-6, 1e-6), seed = 22L,
    relative_tolerance = 5e-4, absolute_tolerance = 1e-7)
  expect_true(is.list(audit))
  expect_setequal(names(audit$pass_by_block),
                  c("position_effect", "unpenalized_mean_reference",
                    "centered_mean_deviation", "horizontal_loadings"))
  expect_true(audit$state_restored_exactly)
  expect_true(all(is.finite(audit$table$autodiff)))
  expect_true(all(is.finite(audit$table$finite_difference)))
})

test_that("an exact warm start retains caller-owned tensor values", {
  skip_if_not_installed("torch")
  set.seed(57)
  respondent_id <- rep(sprintf("r%02d", 1:6), each = 3)
  deltaX <- matrix(rnorm(length(respondent_id) * 2), ncol = 2)
  Z <- matrix(rnorm(length(respondent_id) * 2), ncol = 2)
  y <- rep(c(0, 1, 0), 6)
  scale <- c(1.2, 0.9)
  grid <- sconjoint:::.sc_mixed_grid(
    q = 1L, integration = "gh", n_nodes = 5L, n_draws = 1L,
    seed = 1L, antithetic = TRUE, scramble = FALSE)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 2L, q = 1L, hidden = 2L,
    mean_family = "relu", coefficient_scale = scale,
    compute_dtype = "float64")
  warm <- lapply(net$state_dict(), function(x) x$detach()$cpu()$clone())
  before <- lapply(warm, function(x) torch::as_array(x$detach()$cpu()))

  expect_silent(sconjoint:::.sc_train_mixed_multistart(
    deltaX = sweep(deltaX, 2L, scale, `/`), y = y, Z = Z,
    respondent_id = respondent_id, gh = grid, hidden = 2L,
    mean_family = "relu", n_epochs = 0L, learning_rate = 0.01,
    weight_decay = 0.01, n_starts = 1L, seed = 411L,
    device = "cpu", verbose = FALSE, warm_state = warm,
    warm_start_mode = "exact_state", early_stop = FALSE,
    opt_tol = 0, grad_tol = 0, kkt_tol = NULL,
    protocol_mode = "calibration", calibration_verification = NULL,
    polish_max_iter = 2L, polish_history_size = 5L,
    polish_tolerance_grad = 0, polish_tolerance_change = 0,
    polish_lr = 0.25, coefficient_scale = scale))

  after <- lapply(warm, function(x) torch::as_array(x$detach()$cpu()))
  expect_true(all(vapply(after, function(x) all(is.finite(x)), logical(1L))))
  expect_identical(after, before)
})
