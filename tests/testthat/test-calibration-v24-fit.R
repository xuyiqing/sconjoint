test_that("v2.4 fit data are reconstructed from sealed source rows", {
  template <- list(
    deltaX = matrix(seq_len(12L), nrow = 4L),
    Z = matrix(c(-1, -1, 1, 1, 2, 2, 3, 3), nrow = 4L),
    deltaX_names = c("x1", "x2", "x3"),
    Z_names = c("z1", "z2"))
  source <- c(3L, 4L, 1L, 2L)
  outcome <- list(
    source_row_index = source,
    respondent_id = c("draw1", "draw1", "draw2", "draw2"),
    y = c(1, 0, 0, 1))
  got <- sconjoint:::.scmix_v24_fit_data(template, outcome)
  expect_equal(got$deltaX, template$deltaX[source, , drop = FALSE],
               ignore_attr = TRUE)
  expect_equal(got$Z, template$Z[source, , drop = FALSE],
               ignore_attr = TRUE)
  expect_identical(colnames(got$deltaX), template$deltaX_names)
  expect_identical(colnames(got$Z), template$Z_names)
  expect_identical(got$respondent_id, outcome$respondent_id)

  carried <- outcome
  carried$deltaX <- got$deltaX
  carried$Z <- got$Z
  expect_silent(sconjoint:::.scmix_v24_fit_data(template, carried))
  carried$deltaX[1L, 1L] <- carried$deltaX[1L, 1L] + 1
  expect_error(
    sconjoint:::.scmix_v24_fit_data(template, carried),
    "differs from its response-free source rows")
  bad_index <- outcome
  bad_index$source_row_index[1L] <- 1.5
  expect_error(
    sconjoint:::.scmix_v24_fit_data(template, bad_index),
    "source-row index is malformed")
})

test_that("v2.4 fit controls and GH refinement are frozen", {
  grid <- sconjoint:::.scmix_v24_fit_grid()
  expect_identical(vapply(grid, `[[`, character(1L), "name"),
                   c("q1_constant", "q1_linear"))
  expect_identical(vapply(grid, `[[`, integer(1L), "q"), c(1L, 1L))
  expect_identical(vapply(grid, `[[`, integer(1L), "n_nodes"),
                   c(15L, 15L))
  expect_equal(vapply(grid, `[[`, numeric(1L), "weight_decay"),
               c(0, 0.01))
  controls <- sconjoint:::.scmix_v24_fit_controls()
  expect_identical(controls$n_epochs, 400L)
  expect_identical(controls$n_starts, 2L)
  expect_identical(controls$polish_max_iter, 50L)
  expect_identical(controls$device, "cpu")
  expect_identical(sconjoint:::.scmix_v24_fit_seed(1900000000),
                   1900500000L)
  expect_error(sconjoint:::.scmix_v24_fit_seed(1.5), "must be one integer")
  expect_error(sconjoint:::.scmix_v24_fit_grid(15.5),
               "exactly 15 GH nodes")
  expect_error(sconjoint:::.scmix_v24_fit_grid(weight_decay = 0.02),
               "penalty is exactly 0.01")

  reference <- sconjoint:::.sc_mixed_grid(
    q = 1L, integration = "gh", n_nodes = 15L)
  assembled <- list(
    q = 1L, K = 2L,
    integration_grids_folds = list(reference, reference))
  refined <- sconjoint:::.scmix_v24_fixed_state_gh(assembled)
  expect_equal(nrow(refined$gh$U), 31L)
  expect_length(refined$integration_grids_folds, 2L)
  expect_identical(refined$v24_integration_branch,
                   "fixed_GH15_state_evaluated_at_GH31")
  expect_error(
    sconjoint:::.scmix_v24_fixed_state_gh(
      list(q = 0L, K = 2L,
           integration_grids_folds = list(reference, reference))),
    "assembled q=1 fit")
  expect_error(sconjoint:::.scmix_v24_gh_grid(15L),
               "exactly 31 GH nodes")

  before <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  inside <- local({
    sconjoint:::.scmix_v24_one_thread()
    Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  })
  expect_identical(inside, "1")
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_),
                   before)
})

test_that("exact-state zero-Adam repolish is literal and fail closed", {
  skip_if_not_installed("torch")
  N <- 8L
  TT <- 3L
  respondent_id <- rep(seq_len(N), each = TT)
  deltaX <- matrix(rep(c(-1, 1), length.out = N * TT * 2L), ncol = 2L)
  Z <- matrix(rep(seq(-1, 1, length.out = N), each = TT), ncol = 1L)
  y <- rep(c(0, 1, 1), N)
  gh <- sconjoint:::.sc_gh_grid(1L, 7L)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 1L, hidden = integer(),
    mean_family = "linear", coefficient_scale = c(1, 1))
  warm <- net$state_dict()

  rf <- factor(respondent_id, levels = unique(respondent_id))
  objective_data <- list(
    dx = torch::torch_tensor(deltaX, dtype = torch::torch_float64()),
    zt = torch::torch_tensor(Z, dtype = torch::torch_float64()),
    yt = torch::torch_tensor(y, dtype = torch::torch_float64()),
    idx1 = torch::torch_tensor(as.integer(rf), dtype = torch::torch_long()),
    N = nlevels(rf))
  expected <- sconjoint:::.sc_mixed_evaluate_full_objective(
    net, objective_data,
    torch::torch_tensor(gh$U, dtype = torch::torch_float64()),
    torch::torch_tensor(log(gh$w), dtype = torch::torch_float64()),
    weight_decay = 0.01, coefficient_scale = c(1, 1),
    kappa_bound = 10, alpha_bound = 5, a_bound = 10,
    weight_bound = 10, backward = TRUE)$objective
  warm_arrays <- lapply(warm, function(x) torch::as_array(x$detach()$cpu()))

  fit <- sconjoint:::.sc_train_mixed_multistart(
    deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
    gh = gh, hidden = integer(), mean_family = "linear",
    n_epochs = 0L, learning_rate = 0.01, weight_decay = 0.01,
    n_starts = 1L, seed = 876L, warm_state = warm,
    warm_start_mode = "exact_state", protocol_mode = "calibration",
    polish_max_iter = 2L, coefficient_scale = c(1, 1))
  expect_identical(fit$stopped_at, 0L)
  expect_length(fit$loss_trace, 0L)
  expect_length(fit$nll_trace, 0L)
  expect_length(fit$grad_trace, 0L)
  expect_equal(fit$objective_before_polish, expected, tolerance = 1e-12)
  expect_identical(fit$exact_warm_load_difference, 0)
  expect_equal(
    warm_arrays,
    lapply(warm, function(x) torch::as_array(x$detach()$cpu())),
    tolerance = 0)
  expect_identical(fit$warm_start_mode, "exact_state")
  expect_match(fit$stop_reason, "^zero_adam_exact_state\\+lbfgs_polish")
  expect_true(is.finite(fit$objective))

  incomplete <- warm[-1L]
  expect_error(
    sconjoint:::.sc_train_mixed_one(
      deltaX, y, Z, respondent_id, gh, hidden = integer(),
      mean_family = "linear", n_epochs = 0L, warm_state = incomplete,
      warm_start_mode = "exact_state", coefficient_scale = c(1, 1)),
    "exact warm state could not be loaded")
  expect_error(
    sconjoint:::.sc_train_mixed_one(
      deltaX, y, Z, respondent_id, gh, hidden = integer(),
      mean_family = "linear", n_epochs = 1L, warm_state = warm,
      warm_start_mode = "exact_state", coefficient_scale = c(1, 1)),
    "reserved for a zero-Adam")
  expect_error(
    sconjoint:::.sc_train_mixed_one(
      deltaX, y, Z, respondent_id, gh, hidden = integer(),
      mean_family = "linear", n_epochs = 0L,
      coefficient_scale = c(1, 1)),
    "zero Adam epochs are reserved")
  expect_error(
    sconjoint:::.sc_train_mixed_multistart(
      deltaX = deltaX, y = y, Z = Z, respondent_id = respondent_id,
      gh = gh, hidden = integer(), mean_family = "linear",
      n_epochs = 0L, n_starts = 2L, warm_state = warm,
      warm_start_mode = "exact_state", coefficient_scale = c(1, 1)),
    "requires `n_starts = 1`")
})

test_that("v2.4 GH31 repolish preserves the selected toy-fit contract", {
  skip_if_not_installed("torch")
  set.seed(41L)
  N <- 10L
  TT <- 3L
  respondent_id <- rep(seq_len(N), each = TT)
  deltaX <- matrix(stats::rnorm(N * TT * 2L), ncol = 2L)
  z_respondent <- seq(-1, 1, length.out = N)
  Z <- matrix(rep(z_respondent, each = TT), ncol = 1L)
  eta <- 0.2 + deltaX[, 1L] * (0.3 + 0.1 * Z[, 1L]) -
    deltaX[, 2L] * 0.2
  y <- stats::rbinom(length(eta), 1L, stats::plogis(eta))
  colnames(deltaX) <- c("x1", "x2")
  colnames(Z) <- "z1"
  grid <- list(list(
    name = "q1_constant", q = 1L, hidden = integer(),
    mean_family = "constant", weight_decay = 0,
    integration = "gh", n_nodes = 15L))
  tuned <- scmix_tune_matrix(
    deltaX, y, Z, respondent_id, grid = grid, q = 1L, K = 2L,
    n_epochs = 1L, n_starts = 1L, protocol_mode = "calibration",
    polish_max_iter = 2L, seed = 12L)
  refined <- sconjoint:::.scmix_v24_repolish_selected(tuned$refit)

  expect_identical(tuned$refit$specification$n_nodes, 15L)
  expect_identical(refined$specification$n_nodes, 31L)
  expect_equal(nrow(refined$integration_grid$U), 31L)
  expect_equal(nrow(refined$network_state$integration_grid$U), 31L)
  expect_identical(refined$v24_integration_branch,
                   "GH31_repolish_from_exact_GH15_state")
  expect_identical(refined$optimization$starts$epochs, 0L)
  expect_identical(refined$optimization$starts$warm_start_mode,
                   "exact_state")
  expect_identical(refined$optimization$starts$exact_warm_load_difference,
                   0)
  expect_match(refined$optimization$stop_reason,
               "^zero_adam_exact_state\\+lbfgs_polish")
  expect_invisible(sconjoint:::.scmix_validate_loading_contract(refined))
  expect_invisible(sconjoint:::.scmix_validate_multistart_artifacts(
    refined$multistart_artifacts))

  repolished_bundle <- list(
    full = refined,
    nested_tuning = list(tuning = list(
      list(refit = refined), list(refit = refined))))
  base_metrics <- list(
    maximum_block_kkt = 0,
    maximum_loading_probability_error = 0,
    every_polish_completed = TRUE,
    every_polish_accepted = TRUE,
    every_state_float64 = TRUE,
    all_finite = TRUE)
  metrics <- sconjoint:::.scmix_v24_add_repolish_metrics(
    base_metrics, repolished_bundle)
  expect_equal(nrow(metrics$repolished_selected_surface), 3L)
  expect_true(metrics$every_repolish_exact_state)

  wrong_nodes <- repolished_bundle
  wrong_nodes$nested_tuning$tuning[[2L]]$refit$specification$n_nodes <- 15L
  expect_error(
    sconjoint:::.scmix_v24_add_repolish_metrics(
      base_metrics, wrong_nodes),
    "not bound to its GH31 fit scope")
})

test_that("v2.4 selected fits require applicable finite nesting evidence", {
  skip_if_not_installed("torch")
  set.seed(42L)
  N <- 10L
  TT <- 3L
  respondent_id <- rep(seq_len(N), each = TT)
  deltaX <- matrix(stats::rnorm(N * TT * 2L), ncol = 2L)
  Z <- matrix(rep(seq(-1, 1, length.out = N), each = TT), ncol = 1L)
  y <- stats::rbinom(N * TT, 1L, 0.5)
  colnames(deltaX) <- c("x1", "x2")
  colnames(Z) <- "z1"
  grid <- list(list(
    name = "q1_constant", q = 1L, hidden = integer(),
    mean_family = "constant", weight_decay = 0,
    integration = "gh", n_nodes = 15L))
  tuned <- scmix_tune_matrix(
    deltaX, y, Z, respondent_id, grid = grid, q = 1L, K = 2L,
    n_epochs = 1L, n_starts = 2L, protocol_mode = "calibration",
    polish_max_iter = 2L, seed = 13L)
  nested <- list(tuning = list(tuned, tuned))
  metrics <- sconjoint:::.scmix_v24_fit_metrics(tuned, nested)
  expect_equal(nrow(metrics$selected_start_surface), 3L)
  expect_equal(metrics$maximum_block_kkt,
               max(metrics$selected_start_surface$block_kkt))

  missing <- tuned
  missing$refit$optimization$nested_objective_gate <- NULL
  expect_error(
    sconjoint:::.scmix_v24_fit_metrics(missing, nested),
    "lacks its applicable finite nesting gate")

  selected <- which(vapply(
    tuned$refit$multistart_artifacts, `[[`, logical(1L), "selected"))
  discarded <- setdiff(seq_along(tuned$refit$multistart_artifacts), selected)
  far <- tuned
  far_record <- far$refit$multistart_artifacts[[discarded]]
  far_record$relative_objective_from_best <- 1e-2
  far_record$polished <- FALSE
  far_record$optimizer_evidence$polish$accepted <- FALSE
  far_record$optimizer_evidence$kkt$returned$maximum <- 1
  far$refit$multistart_artifacts[[discarded]] <- far_record
  local_mocked_bindings(
    .scmix_validate_multistart_artifacts = function(x) invisible(x),
    .package = "sconjoint")
  far_metrics <- sconjoint:::.scmix_v24_fit_metrics(
    far, list(tuning = list(far, far)))
  expect_equal(far_metrics$maximum_block_kkt,
               max(far_metrics$selected_start_surface$block_kkt))
  expect_true(far_metrics$every_polish_completed)
  expect_true(far_metrics$every_polish_accepted)

  near <- far
  near$refit$multistart_artifacts[[discarded]]$
    relative_objective_from_best <- 5e-7
  near_metrics <- sconjoint:::.scmix_v24_fit_metrics(
    near, list(tuning = list(near, near)))
  expect_equal(near_metrics$maximum_block_kkt,
               max(near_metrics$selected_start_surface$block_kkt))
  expect_false(near_metrics$every_polish_completed)
  expect_false(near_metrics$every_polish_accepted)
})
