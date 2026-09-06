test_that("explicit loading accessor round-trips full and fold scales", {
  raw_full <- matrix(c(0.2, -0.4, 0.1, 0.3), 2L, 2L,
                     dimnames = list(c("x1", "x2"), NULL))
  scale_full <- c(x1 = 2, x2 = 0.5)
  raw_folds <- list(raw_full + 0.1, raw_full - 0.05)
  fold_scales <- list(c(x1 = 1.5, x2 = 0.75),
                      c(x1 = 0.8, x2 = 3))
  fit <- sconjoint:::.scmix_add_loading_contract(
    list(), q = 2L, raw_full = raw_full,
    coefficient_scale_full = scale_full,
    raw_folds = raw_folds, coefficient_scale_folds = fold_scales,
    source = "unit test")

  expect_equal(scmix_loadings(fit, "raw", "full"), raw_full,
               ignore_attr = TRUE)
  expect_equal(
    scmix_loadings(fit, "optimization", "full"),
    sweep(raw_full, 1L, scale_full, `*`), ignore_attr = TRUE)
  for (k in 1:2) {
    expect_equal(scmix_loadings(fit, "raw", "fold", k), raw_folds[[k]],
                 ignore_attr = TRUE)
    expect_equal(
      scmix_loadings(fit, "optimization", "fold", k),
      sweep(raw_folds[[k]], 1L, fold_scales[[k]], `*`),
      ignore_attr = TRUE)
  }
  expect_identical(attr(scmix_loadings(fit, "raw", "full"),
                        "loading_scale"), "raw")
  expect_error(scmix_loadings(list(A = raw_full), "raw", "full"),
               "lacks an explicit")

  corrupted <- fit
  corrupted$A_optimization_full[1L, 1L] <-
    corrupted$A_optimization_full[1L, 1L] + 0.01
  expect_error(scmix_loadings(corrupted, "raw", "full"), "round-trip")

  q0 <- sconjoint:::.scmix_add_loading_contract(
    list(), q = 0L, raw_full = matrix(numeric(), 2L, 0L),
    coefficient_scale_full = scale_full, source = "q0 unit test")
  expect_identical(dim(scmix_loadings(q0, "raw", "full")), c(2L, 0L))
  expect_identical(dim(scmix_loadings(q0, "optimization", "full")),
                   c(2L, 0L))
})

test_that("loading-scale contract preserves fitted indices and probabilities", {
  set.seed(9501)
  dx_raw <- matrix(rnorm(30), 10L, 3L)
  A_raw <- matrix(rnorm(6), 3L, 2L)
  coefficient_scale <- c(0.25, 2, 5)
  U <- matrix(rnorm(14), 7L, 2L)
  dx_optimization <- sweep(dx_raw, 2L, coefficient_scale, `/`)
  A_optimization <- sweep(A_raw, 1L, coefficient_scale, `*`)
  raw_index <- dx_raw %*% A_raw %*% t(U)
  optimization_index <- dx_optimization %*% A_optimization %*% t(U)
  expect_equal(optimization_index, raw_index, tolerance = 1e-13)
  expect_equal(stats::plogis(optimization_index), stats::plogis(raw_index),
               tolerance = 1e-13)
})

test_that("portable states carry dtype and loading metadata", {
  skip_if_not_installed("torch")
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 1L, hidden = integer(), mean_family = "linear",
    coefficient_scale = c(2, 0.5))
  state <- sconjoint:::.scmix_capture_network_state(
    net, p = 2L, p_Z = 1L, q = 1L, hidden = integer(),
    mean_family = "linear", mu_bound = 10, kappa_bound = 10,
    alpha_bound = 5, a_bound = 10, weight_bound = 10,
    coefficient_scale = c(x1 = 2, x2 = 0.5),
    z_transform = list(center = c(z = 0), scale = c(z = 1),
                       constant = c(z = FALSE)),
    dx_transform = list(scale = c(x1 = 2, x2 = 0.5)),
    coefficient_names = c("x1", "x2"), moderator_names = "z")
  expect_identical(state$architecture$compute_dtype, "float64")
  expect_identical(state$loading_contract$contract_id,
                   "scmix-loading-scale-v1")
  expect_equal(
    scmix_loadings(state, "optimization", "full"),
    state$state_dict$A, ignore_attr = TRUE)
  expect_equal(
    scmix_loadings(state, "raw", "full"),
    sweep(state$state_dict$A, 1L, c(2, 0.5), `/`), ignore_attr = TRUE)
  restored <- scmix_restore_network(state)
  expect_true(all(vapply(restored$parameters, function(x) {
    identical(as.character(x$dtype), "Double")
  }, logical(1L))))
})
