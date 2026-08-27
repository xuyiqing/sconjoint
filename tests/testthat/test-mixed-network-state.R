test_that("portable mixed-network states survive an RDS round trip", {
  skip_if_not_installed("torch")
  torch::torch_manual_seed(90210L)
  scale <- c(a = 0.5, b = 2)
  Z <- cbind(z1 = c(-2, -0.5, 1, 2), z2 = c(0, 1, -1, 0.5))
  rid <- paste0("r", seq_len(nrow(Z)))
  z_transform <- sconjoint:::.sc_fit_z_transform(Z, rid)
  dx_transform <- list(scale = scale)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 2L, q = 1L, hidden = c(3L, 2L),
    mu_bound = 4, kappa_bound = 3, a_bound = 5, weight_bound = 6,
    coefficient_scale = scale)
  Z_std <- sconjoint:::.sc_apply_z_transform(Z, z_transform)
  expected_network <- sconjoint:::.sc_predict_beta(net, Z_std)
  expected_raw <- sweep(expected_network, 2L, scale, `/`)
  expected_A_internal <- as.matrix(torch::as_array(net$A$detach()$cpu()))
  expected_kappa <- as.numeric(torch::as_array(
    net$get_kappa()$detach()$cpu()))
  integration_grid <- sconjoint:::.sc_mixed_grid(
    q = 1L, integration = "gh", n_nodes = 7L, seed = 19L)

  state <- sconjoint:::.scmix_capture_network_state(
    net, p = 2L, p_Z = 2L, q = 1L, hidden = c(3L, 2L),
    mu_bound = 4, kappa_bound = 3, a_bound = 5, weight_bound = 6,
    coefficient_scale = scale, z_transform = z_transform,
    dx_transform = dx_transform, coefficient_names = c("a", "b"),
    moderator_names = c("z1", "z2"),
    integration_grid = integration_grid,
    analysis_signature = "scmix-v1-0123456789abcdef", scope = "unit test")
  expect_s3_class(state, "scmix_network_state")
  expect_true(all(vapply(state$state_dict, is.numeric, logical(1L))))
  expect_identical(names(state$state_dtypes), names(state$state_dict))
  expect_false(any(vapply(state$state_dict, inherits, logical(1L),
                          what = "torch_tensor")))
  portable_fit <- scmix_portable_copy(list(net = net, network_state = state,
                                           stored_mu = expected_raw))
  expect_null(portable_fit$net)
  expect_s3_class(portable_fit$network_state, "scmix_network_state")
  expect_equal(portable_fit$stored_mu, expected_raw)

  path <- tempfile(fileext = ".rds")
  saveRDS(state, path, version = 3)
  reloaded <- readRDS(path)
  torch::torch_manual_seed(771L)
  expected_after_reload <- torch::as_array(torch::torch_randn(5L))
  torch::torch_manual_seed(771L)
  restored <- scmix_restore_network(reloaded)
  observed_after_reload <- torch::as_array(torch::torch_randn(5L))
  got_network <- sconjoint:::.sc_predict_beta(restored, Z_std)
  got_raw <- scmix_predict_network(reloaded, Z[, c("z2", "z1")])
  got_A_internal <- as.matrix(torch::as_array(restored$A$detach()$cpu()))
  got_kappa <- as.numeric(torch::as_array(
    restored$get_kappa()$detach()$cpu()))
  expect_equal(observed_after_reload, expected_after_reload, tolerance = 0)
  expect_equal(got_network, expected_network, tolerance = 1e-7)
  expect_equal(unname(got_raw), unname(expected_raw), tolerance = 1e-7)
  expect_equal(got_A_internal, expected_A_internal, tolerance = 0)
  expect_equal(
    sweep(got_A_internal, 1L, scale, `/`),
    sweep(expected_A_internal, 1L, scale, `/`), tolerance = 0)
  expect_equal(got_kappa, expected_kappa, tolerance = 0)
  expect_identical(colnames(got_raw), c("a", "b"))
  expect_equal(reloaded$integration_grid$U, integration_grid$U, tolerance = 0)
  expect_equal(reloaded$integration_grid$w, integration_grid$w, tolerance = 0)

  ## Version-1 bundles remain reloadable as the original legacy architecture.
  legacy_v1 <- reloaded
  legacy_v1$format_version <- 1L
  legacy_v1$architecture_id <- "mixed-conjoint-dnn-relu-tanh-v1"
  legacy_v1$architecture$mean_family <- NULL
  legacy_v1$architecture$alpha_bound <- NULL
  expect_equal(unname(scmix_predict_network(legacy_v1, Z)),
               unname(expected_raw), tolerance = 1e-7)

  dx_sequence_raw <- cbind(a = c(-1, 0.5, 2, -0.25),
                           b = c(0.2, -3, 1, 4))
  dx_sequence_internal <- sweep(dx_sequence_raw, 2L, scale, `/`)
  y_sequence <- c(0, 1, 1, 0)
  expected_ll <- sconjoint:::.sc_comp_sequence_loglik(
    net, dx_sequence_internal, y_sequence, Z_std, rid, integration_grid)
  restored_ll <- sconjoint:::.sc_comp_sequence_loglik(
    restored, dx_sequence_internal, y_sequence, Z_std, rid,
    reloaded$integration_grid)
  expect_equal(restored_ll, expected_ll, tolerance = 1e-7)

  dx_raw <- dx_sequence_raw
  u <- 0.7
  raw_index <- rowSums(dx_raw * expected_raw) +
    as.numeric(dx_raw %*% sweep(expected_A_internal, 1L, scale, `/`)) * u
  internal_index <- rowSums(sweep(dx_raw, 2L, scale, `/`) *
                              expected_network) +
    as.numeric(sweep(dx_raw, 2L, scale, `/`) %*% expected_A_internal) * u
  expect_equal(raw_index, internal_index, tolerance = 1e-7)

  ## Restoration is independent of later mutations to the live fitting module.
  torch::with_no_grad(net$param_layer$bias$add_(10))
  expect_equal(unname(scmix_predict_network(reloaded, Z)),
               unname(expected_raw),
               tolerance = 1e-7)
})

test_that("version-2 states restore every corrected conditional-mean family", {
  skip_if_not_installed("torch")
  Z <- cbind(z1 = c(-1, 0, 2), z2 = c(0.5, -0.5, 1))
  rid <- seq_len(nrow(Z))
  z_transform <- sconjoint:::.sc_fit_z_transform(Z, rid)
  Z_std <- sconjoint:::.sc_apply_z_transform(Z, z_transform)
  coefficient_scale <- c(x1 = 0.5, x2 = 2)
  for (family in c("constant", "linear", "relu")) {
    hidden <- if (family == "relu") 3L else integer()
    torch::torch_manual_seed(match(family,
                                   c("constant", "linear", "relu")) + 440L)
    net <- sconjoint:::.sc_build_mixed_network(
      p = 2L, p_Z = 2L, q = 1L, hidden = hidden,
      mean_family = family, mu_bound = 10, kappa_bound = 3,
      alpha_bound = 2, a_bound = 5, weight_bound = 6,
      coefficient_scale = coefficient_scale)
    expected <- sconjoint:::.sc_predict_beta(net, Z_std)
    state <- sconjoint:::.scmix_capture_network_state(
      net, p = 2L, p_Z = 2L, q = 1L, hidden = hidden,
      mean_family = family, mu_bound = 10, kappa_bound = 3,
      alpha_bound = 2, a_bound = 5, weight_bound = 6,
      coefficient_scale = coefficient_scale,
      z_transform = z_transform,
      dx_transform = list(scale = coefficient_scale),
      coefficient_names = names(coefficient_scale),
      moderator_names = colnames(Z))
    path <- tempfile(fileext = ".rds")
    saveRDS(state, path, version = 3)
    reloaded <- readRDS(path)
    restored <- scmix_restore_network(reloaded)
    expect_identical(reloaded$format_version, 2L)
    expect_identical(reloaded$architecture$mean_family, family)
    expect_equal(reloaded$architecture$alpha_bound, 2)
    expect_equal(reloaded$state_dict$mu_bound_internal,
                 unname(10 * coefficient_scale), tolerance = 0)
    expect_equal(sconjoint:::.sc_predict_beta(restored, Z_std),
                 expected, tolerance = 1e-7)
    expect_equal(torch::as_array(restored$mu_bound_internal),
                 unname(10 * coefficient_scale), tolerance = 0)
    expected_state <- net$state_dict()
    restored_state <- restored$state_dict()
    expect_identical(names(restored_state), names(expected_state))
    for (nm in names(expected_state)) {
      expect_equal(torch::as_array(restored_state[[nm]]$detach()$cpu()),
                   torch::as_array(expected_state[[nm]]$detach()$cpu()),
                   tolerance = 0)
    }
  }
})

test_that("portable mixed-network states fail closed on malformed state", {
  skip_if_not_installed("torch")
  net <- sconjoint:::.sc_build_mixed_network(
    p = 1L, p_Z = 1L, q = 0L, hidden = 2L,
    coefficient_scale = 1)
  state <- sconjoint:::.scmix_capture_network_state(
    net, p = 1L, p_Z = 1L, q = 0L, hidden = 2L,
    mu_bound = 10, kappa_bound = 10, a_bound = 10, weight_bound = 10,
    coefficient_scale = 1,
    z_transform = list(center = c(z = 0), scale = c(z = 1),
                       constant = c(z = FALSE)),
    dx_transform = list(scale = c(x = 1)),
    coefficient_names = "x", moderator_names = "z")
  expected_q0 <- sconjoint:::.sc_predict_beta(net, matrix(c(-1, 0, 1), ncol = 1L))
  got_q0 <- scmix_predict_network(
    state, matrix(c(-1, 0, 1), ncol = 1L, dimnames = list(NULL, "z")),
    input = "standardized", output = "network")
  restored_q0 <- scmix_restore_network(state)
  expect_equal(unname(got_q0), unname(expected_q0), tolerance = 1e-7)
  expect_identical(restored_q0$q, 0L)
  expect_null(restored_q0$A)
  bad <- state
  bad$state_shapes[[1L]] <- c(999L)
  expect_error(scmix_restore_network(bad), "Malformed serialized tensor|shape")
  bad_scale <- state
  bad_scale$preprocessing$deltaX$scale[] <- 2
  expect_error(scmix_restore_network(bad_scale), "differs from the network")
  bad_architecture <- state
  bad_architecture$architecture_id <- "unknown"
  expect_error(scmix_restore_network(bad_architecture), "not a supported")
  expect_error(scmix_predict_network(state, cbind(wrong = 1)),
               "stored moderator names")
})

test_that("q=2 portable states preserve every named tensor and its shape", {
  skip_if_not_installed("torch")
  torch::torch_manual_seed(1948L)
  net <- sconjoint:::.sc_build_mixed_network(
    p = 3L, p_Z = 2L, q = 2L, hidden = c(4L, 3L),
    coefficient_scale = c(0.25, 2, 5))
  state <- sconjoint:::.scmix_capture_network_state(
    net, p = 3L, p_Z = 2L, q = 2L, hidden = c(4L, 3L),
    mu_bound = 10, kappa_bound = 10, a_bound = 10, weight_bound = 10,
    coefficient_scale = c(0.25, 2, 5),
    z_transform = list(center = c(z1 = 0, z2 = 1),
                       scale = c(z1 = 2, z2 = 0.5),
                       constant = c(z1 = FALSE, z2 = FALSE)),
    dx_transform = list(scale = c(x1 = 0.25, x2 = 2, x3 = 5)),
    coefficient_names = c("x1", "x2", "x3"),
    moderator_names = c("z1", "z2"))
  restored <- scmix_restore_network(state)
  got <- restored$state_dict()
  expect_identical(names(got), names(state$state_dict))
  for (nm in names(got)) {
    expect_identical(as.integer(got[[nm]]$shape), state$state_shapes[[nm]])
    expect_equal(torch::as_array(got[[nm]]$detach()$cpu()),
                 state$state_dict[[nm]], tolerance = 0)
  }
})
