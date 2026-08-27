## Corrected conditional-mean parameterization: an unpenalized compact
## baseline plus a penalized moderator deviation centered at training Z = 0.

test_that("mean-family grids make the pooled benchmark explicit", {
  grid <- list(
    list(name = "pooled", mean_family = "constant", weight_decay = 0),
    list(name = "linear", mean_family = "linear", weight_decay = 1e-2),
    list(name = "h4", mean_family = "relu", hidden = 4L,
         weight_decay = 1e-2),
    list(name = "h8", mean_family = "relu", hidden = 8L,
         weight_decay = 1e-1)
  )
  got <- sconjoint:::.sc_comp_normalize_grid(grid, q = 1L, p = 3L)
  expect_identical(vapply(got, `[[`, character(1L), "mean_family"),
                   c("constant", "linear", "relu", "relu"))
  expect_length(got[[1L]]$hidden, 0L)
  expect_length(got[[2L]]$hidden, 0L)
  expect_identical(got[[3L]]$hidden, 4L)
  expect_identical(got[[4L]]$hidden, 8L)
  expect_identical(got[[1L]]$weight_decay, 0)

  expect_error(
    sconjoint:::.sc_comp_normalize_grid(
      list(list(mean_family = "constant", weight_decay = 0.1)),
      q = 1L, p = 3L),
    "constant.*weight_decay.*zero"
  )
  expect_error(
    sconjoint:::.sc_comp_normalize_grid(
      list(list(mean_family = "relu", hidden = integer(),
                weight_decay = 0.1)),
      q = 1L, p = 3L),
    "positive-integer.*hidden"
  )
})

test_that("constant, linear, and ReLU means share an exact unpenalized baseline", {
  skip_if_not_installed("torch")
  build <- function(family, hidden = integer()) {
    sconjoint:::.sc_build_mixed_network(
      p = 2L, p_Z = 2L, q = 0L, hidden = hidden,
      mean_family = family, mu_bound = 10, kappa_bound = 10,
      alpha_bound = 3, a_bound = 10, weight_bound = 20,
      coefficient_scale = c(1, 2)
    )
  }
  Z <- torch::torch_tensor(
    matrix(c(-1, 0, 1, 2, 0, 0), ncol = 2L, byrow = TRUE),
    dtype = torch::torch_float())

  for (spec in list(c("constant"), c("linear"), c("relu", "3"))) {
    family <- spec[[1L]]
    hidden <- if (length(spec) == 1L) integer() else as.integer(spec[[2L]])
    net <- build(family, hidden)
    torch::with_no_grad({
      net$alpha_raw$copy_(torch::torch_tensor(c(0.4, -0.8)))
    })
    zero <- torch::torch_zeros(1L, 2L, dtype = torch::torch_float())
    baseline <- as.numeric(torch::as_array(net$get_beta(zero)))
    expect_equal(baseline, c(0.4, -0.8), tolerance = 1e-6)
    if (identical(family, "constant")) {
      prediction <- as.matrix(torch::as_array(net$get_beta(Z)))
      expect_equal(prediction,
                   matrix(baseline, nrow(prediction), 2L, byrow = TRUE),
                   tolerance = 1e-6)
    }
  }
})

test_that("corrected means use one common raw-coordinate output bound", {
  skip_if_not_installed("torch")
  scale <- c(2, 0.5)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 0L, hidden = integer(),
    mean_family = "linear", mu_bound = 1, kappa_bound = 10,
    alpha_bound = 0.25, a_bound = 10, weight_bound = 200,
    coefficient_scale = scale)
  expect_equal(as.numeric(torch::as_array(net$mu_bound_internal)),
               c(2, 0.5), tolerance = 0)
  torch::with_no_grad({
    net$param_layer$weight$copy_(torch::torch_tensor(
      matrix(c(100, -100), nrow = 2L, ncol = 1L)))
  })
  internal <- sconjoint:::.sc_predict_beta(net, matrix(c(1, -1), ncol = 1L))
  raw <- sweep(internal, 2L, scale, `/`)
  expect_equal(raw[1L, ], c(1, -1), tolerance = 1e-7)
  expect_equal(raw[2L, ], c(-1, 1), tolerance = 1e-7)

  bounds <- sconjoint:::.sc_mixed_bound_diagnostics(
    net, mu = matrix(c(1.6, 0.4), nrow = 1L), kappa = 0,
    coefficient_scale = scale, mu_bound = 1, kappa_bound = 10,
    alpha_bound = 0.25, a_bound = 10, weight_bound = 200)
  expect_false(bounds$mu_active)
  expect_equal(bounds$mu_max_internal, 1.6, tolerance = 0)
  expect_equal(bounds$mu_max_abs_raw, 0.8, tolerance = 1e-12)
  expect_identical(bounds$mu_units, "raw coefficient")

  expect_error(
    sconjoint:::.sc_build_mixed_network(
      p = 1L, p_Z = 1L, q = 0L, hidden = integer(),
      mean_family = "constant", mu_bound = 1, kappa_bound = 10,
      alpha_bound = 1, a_bound = 10, weight_bound = 20,
      coefficient_scale = 0.1),
    "alpha_bound < mu_bound"
  )
})

test_that("old live modules without a family field retain legacy bound units", {
  skip_if_not_installed("torch")
  scale <- c(2, 0.5)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 0L, hidden = 2L,
    mean_family = "legacy", mu_bound = 1, kappa_bound = 10,
    alpha_bound = 0.25, a_bound = 10, weight_bound = 20,
    coefficient_scale = scale)
  net$mean_family <- NULL
  bounds <- sconjoint:::.sc_mixed_bound_diagnostics(
    net, mu = matrix(c(1.6, 0.4), nrow = 1L), kappa = 0,
    coefficient_scale = scale, mu_bound = 1, kappa_bound = 10,
    alpha_bound = 0.25, a_bound = 10, weight_bound = 20)
  expect_true(bounds$mu_active)
  expect_identical(bounds$mu_units,
                   "legacy standardized-contrast coefficient")
})

test_that("the corrected penalty excludes alpha and includes deviations", {
  skip_if_not_installed("torch")
  lambda <- 0.2
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 2L, q = 1L, hidden = 3L,
    mean_family = "relu", mu_bound = 10, kappa_bound = 10,
    alpha_bound = 3, a_bound = 10, weight_bound = 20,
    coefficient_scale = c(1, 1)
  )
  torch::with_no_grad({
    for (nm in names(net$parameters)) net$parameters[[nm]]$fill_(1)
  })
  excluded <- c("A", "kappa_raw", "alpha_raw")
  deviation <- setdiff(names(net$parameters), excluded)
  n_deviation <- sum(vapply(
    deviation, function(nm) net$parameters[[nm]]$numel(), numeric(1L)))
  penalty <- sconjoint:::.sc_mixed_penalty(net, lambda)
  expect_equal(as.numeric(penalty$item()), lambda * n_deviation,
               tolerance = 1e-6)
  penalty$backward()
  alpha_grad <- net$alpha_raw$grad
  expect_true(is.null(alpha_grad) || alpha_grad$numel() == 0L ||
                max(abs(torch::as_array(alpha_grad$detach()))) == 0)
  for (nm in deviation) {
    expect_equal(
      as.numeric(torch::as_array(net$parameters[[nm]]$grad$detach())),
      rep(2 * lambda, net$parameters[[nm]]$numel()), tolerance = 1e-6)
  }
})

test_that("alpha is projected on a compact raw-coordinate set", {
  skip_if_not_installed("torch")
  scale <- c(2, 0.5)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 0L, hidden = integer(),
    mean_family = "constant", mu_bound = 10, kappa_bound = 10,
    alpha_bound = 0.25, a_bound = 10, weight_bound = 20,
    coefficient_scale = scale
  )
  torch::with_no_grad({ net$alpha_raw$fill_(100) })
  sconjoint:::.sc_mixed_project_parameters(
    net, coefficient_scale = scale, alpha_bound = 0.25,
    a_bound = 10, weight_bound = 20)
  alpha_raw_units <- as.numeric(torch::as_array(net$alpha_raw)) / scale
  expect_lte(max(abs(alpha_raw_units)), 0.25 + 1e-7)
  bounds <- sconjoint:::.sc_mixed_bound_diagnostics(
    net, mu = matrix(0, 1L, 2L), kappa = 0,
    coefficient_scale = scale, mu_bound = 10, kappa_bound = 10,
    alpha_bound = 0.25, a_bound = 10, weight_bound = 20)
  expect_true(bounds$alpha_active)
  expect_equal(bounds$alpha_max_abs_raw, 0.25, tolerance = 1e-7)
})

test_that("nested objective gate fails closed above the pooled feasible value", {
  pass <- sconjoint:::.sc_comp_nested_objective_gate(
    candidate_penalized_nll = 1.9, pooled_penalized_nll = 2,
    tolerance = 1e-6, applicable = TRUE)
  expect_true(pass$pass)
  expect_lte(pass$gap, 0)

  fail <- sconjoint:::.sc_comp_nested_objective_gate(
    candidate_penalized_nll = 2.01, pooled_penalized_nll = 2,
    tolerance = 1e-4, applicable = TRUE)
  expect_false(fail$pass)
  expect_identical(fail$failure_reason,
                   "nested_pooled_objective_not_attained")

  legacy <- sconjoint:::.sc_comp_nested_objective_gate(
    candidate_penalized_nll = 3, pooled_penalized_nll = 2,
    tolerance = 0, applicable = FALSE)
  expect_true(legacy$pass)
  expect_false(legacy$applicable)
})

test_that("candidate-score ties prefer the simpler eligible family", {
  score <- matrix(c(-2, -2, -2 + 5e-9, -2), nrow = 2L, byrow = TRUE)
  count <- matrix(10, nrow = 2L, ncol = 2L)
  gate <- matrix(TRUE, nrow = 2L, ncol = 2L)
  selected <- sconjoint:::.sc_comp_select_candidate(
    score, count, gate, complexity_rank = c(1, 3), tie_tolerance = 1e-8)
  expect_identical(selected$selected, 1L)
  expect_identical(selected$within_selection_tie, c(TRUE, TRUE))
})

test_that("matrix tuning carries the pooled nesting gate across families", {
  skip_if_not_installed("torch")
  N <- 10L; T <- 3L
  rid <- rep(seq_len(N), each = T)
  dx <- matrix(rep(c(-1, 0, 1), N), ncol = 1L,
               dimnames = list(NULL, "x"))
  Z <- matrix(rep(seq(-1, 1, length.out = N), each = T), ncol = 1L,
              dimnames = list(NULL, "z"))
  y <- unlist(lapply(seq_len(N), function(i) {
    if (i <= N / 2L) c(1, 0, 0) else c(0, 0, 1)
  }))
  grid <- list(
    list(name = "pooled", mean_family = "constant", weight_decay = 0),
    list(name = "linear", mean_family = "linear", weight_decay = 0.1),
    list(name = "h2", mean_family = "relu", hidden = 2L,
         weight_decay = 0.1)
  )
  out <- sconjoint:::scmix_tune_matrix(
    dx, y, Z, rid, grid = grid, q = 0L, K = 2L,
    n_epochs = 20L, learning_rate = 0.03, n_starts = 2L,
    alpha_bound = 3, opt_tol = 1e6, grad_tol = 1e6,
    seed = 20260824L)
  expect_identical(out$candidates$mean_family,
                   c("constant", "linear", "relu"))
  expect_true(out$candidates$all_inner_nesting_gates_pass[[1L]])
  expect_false(anyNA(out$fold_nesting_gate))
  expect_true(all(!out$fold_computational_gate |
                    out$fold_nesting_gate))
  expect_true(all(out$fold_pooled_prefit_gate))
  expect_true(all(out$fold_continued_constant_gate))
  expect_true(out$refit$optimization$nested_objective_gate$pass)
  expect_true(out$refit$optimization$nested_objective_gate$applicable)
  expect_true(out$refit$optimization$alpha_bound_diagnostics_complete)
  for (k in seq_len(2L)) {
    constant_starts <- out$cv_optimization[[1L]][[k]]$starts
    relu_starts <- out$cv_optimization[[3L]][[k]]$starts
    expect_identical(
      relu_starts$warm_start_mode,
      c("pooled_structural", "pooled_structural"))
    expect_true(all(constant_starts$epochs == 20L))
    expect_true(all(relu_starts$epochs == 20L))
    expect_equal(
      out$cv_pooled_prefit_optimization[[1L]][[k]]$objective,
      out$cv_pooled_prefit_optimization[[3L]][[k]]$objective,
      tolerance = 0)
    expect_true(any(relu_starts$deviation_from_origin_max > 1e-6))
  }
})
