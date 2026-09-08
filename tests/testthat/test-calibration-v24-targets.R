test_that("v2.4 frozen choice callback is respondent- and task-equal", {
  X <- rbind(c(1, 0), c(0, 1), c(1, 1), c(-1, 0))
  colnames(X) <- c("a", "b")
  task_id <- c("r1", "r2", "r2", "r2")
  callback <- sconjoint:::.scmix_v24_frozen_choice_target(
    X, task_id, n_nodes = 31L)
  mu <- rbind(r1 = c(0.3, -0.2), r2 = c(-0.1, 0.4))
  Sigma <- matrix(c(0.25, 0.04, 0.04, 0.16), 2L)
  answer <- callback(
    mu, 0.15, Sigma, matrix(0, 2L, 1L), rownames(mu), 1L,
    colnames(X))

  expect_identical(dim(answer$value), c(2L, 1L))
  expect_true(all(answer$value > 0 & answer$value < 1))
  expect_identical(dim(answer$d_mu), c(2L, 1L, 2L))
  expect_identical(dim(answer$d_Sigma), c(2L, 1L, 2L, 2L))

  ## With zero covariance the target reduces to the exact task average.
  zero <- callback(
    mu, 0.15, matrix(0, 2L, 2L), matrix(0, 2L, 1L), rownames(mu),
    1L, colnames(X))
  manual <- c(
    stats::plogis(0.15 + sum(X[1L, ] * mu[1L, ])),
    mean(stats::plogis(0.15 + drop(X[2:4, ] %*% mu[2L, ]))))
  expect_equal(drop(zero$value), manual, tolerance = 1e-13)
  expect_false(isTRUE(all.equal(
    mean(zero$value), mean(stats::plogis(c(
      0.15 + sum(X[1L, ] * mu[1L, ]),
      0.15 + drop(X[2:4, ] %*% mu[2L, ])))))))
})

test_that("v2.4 frozen choice analytic derivatives match perturbations", {
  X <- rbind(c(1, 0.5), c(-0.2, 1), c(0.7, -0.3), c(0.4, 0.8))
  colnames(X) <- c("a", "b")
  ids <- c("r1", "r1", "r2", "r2")
  callback <- sconjoint:::.scmix_v24_frozen_choice_target(
    X, ids, kappa_sign = c(1, -1, 1, 1), n_nodes = 41L)
  mu <- rbind(r1 = c(0.2, -0.1), r2 = c(-0.3, 0.25))
  Sigma <- matrix(c(0.35, 0.06, 0.06, 0.22), 2L)
  evaluate <- function(mu_value = mu, kappa_value = -0.12,
                       Sigma_value = Sigma) callback(
    mu_value, kappa_value, Sigma_value, matrix(0, 2L, 1L),
    rownames(mu), 1L, colnames(X))
  answer <- evaluate()
  h <- 1e-6
  for (i in seq_len(2L)) for (j in seq_len(2L)) {
    up <- down <- mu
    up[i, j] <- up[i, j] + h
    down[i, j] <- down[i, j] - h
    numerical <- (evaluate(up)$value[i, 1L] -
                    evaluate(down)$value[i, 1L]) / (2 * h)
    expect_equal(answer$d_mu[i, 1L, j], numerical, tolerance = 2e-7)
  }
  numerical_kappa <- (evaluate(kappa_value = -0.12 + h)$value -
                        evaluate(kappa_value = -0.12 - h)$value) / (2 * h)
  expect_equal(drop(answer$d_kappa), drop(numerical_kappa), tolerance = 2e-7)

  for (j in seq_len(2L)) {
    up <- down <- Sigma
    up[j, j] <- up[j, j] + h
    down[j, j] <- down[j, j] - h
    numerical <- (evaluate(Sigma_value = up)$value -
                    evaluate(Sigma_value = down)$value) / (2 * h)
    expect_equal(drop(answer$d_Sigma[, 1L, j, j]), drop(numerical),
                 tolerance = 2e-7)
  }
  up <- down <- Sigma
  up[1L, 2L] <- up[1L, 2L] + h
  up[2L, 1L] <- up[2L, 1L] + h
  down[1L, 2L] <- down[1L, 2L] - h
  down[2L, 1L] <- down[2L, 1L] - h
  numerical <- (evaluate(Sigma_value = up)$value -
                  evaluate(Sigma_value = down)$value) / (2 * h)
  analytic <- answer$d_Sigma[, 1L, 1L, 2L] +
    answer$d_Sigma[, 1L, 2L, 1L]
  expect_equal(drop(analytic), drop(numerical), tolerance = 2e-7)
})

test_that("v2.4 directional share carries its exact variance primitive", {
  direction <- c(a = 1, b = -0.5)
  callback <- sconjoint:::.scmix_v24_directional_share_target(
    direction, label = "preference_direction_share")
  mu <- rbind(r1 = c(0.3, -0.2), r2 = c(-0.1, 0.4))
  colnames(mu) <- c("a", "b")
  Sigma <- matrix(c(0.25, 0.04, 0.04, 0.16), 2L)
  answer <- callback(
    mu, 0, Sigma, matrix(0, 2L, 1L), rownames(mu), 1L,
    colnames(mu))
  variance <- as.numeric(crossprod(direction, Sigma %*% direction))
  directional_mean <- drop(mu %*% direction)
  expect_identical(
    answer$labels,
    c("preference_direction_share",
      "preference_direction_share:directional_variance"))
  expect_equal(unname(answer$value[, 1L]),
               unname(stats::pnorm(directional_mean / sqrt(variance))))
  expect_equal(answer$value[, 2L], rep(variance, nrow(mu)))
  expect_equal(answer$d_Sigma[1L, 2L, , ], tcrossprod(direction))
  expect_equal(answer$d_mu[, 2L, ], matrix(0, nrow(mu), ncol(mu)))
  expect_equal(answer$d_kappa, matrix(0, nrow(mu), 2L))
})

test_that("v2.4 directional share is defined at zero and tiny variance", {
  callback <- sconjoint:::.scmix_v24_directional_share_target(
    c(a = 1, b = 0), label = "direction_share")
  mu <- rbind(positive = c(0.2, 0), negative = c(-0.3, 0),
              tie = c(0, 0))
  colnames(mu) <- c("a", "b")
  zero <- callback(
    mu, 0, matrix(0, 2L, 2L), matrix(0, 3L, 1L), rownames(mu),
    1L, colnames(mu))
  expect_equal(zero$value[, 1L], c(1, 0, 0))
  expect_equal(zero$value[, 2L], rep(0, 3L))
  expect_equal(zero$d_mu[, 1L, ], array(0, c(3L, 2L)))
  expect_equal(zero$d_Sigma[, 1L, , ], array(0, c(3L, 2L, 2L)))
  for (i in seq_len(3L)) {
    expect_equal(zero$d_Sigma[i, 2L, , ], matrix(c(1, 0, 0, 0), 2L))
  }
  inclusive <- sconjoint:::.scmix_v24_directional_share_target(
    c(a = 1, b = 0), label = "compensating_share", include_ties = TRUE)
  inclusive_zero <- inclusive(
    mu, 0, matrix(0, 2L, 2L), matrix(0, 3L, 1L), rownames(mu),
    1L, colnames(mu))
  expect_equal(inclusive_zero$value[, 1L], c(1, 0, 1))

  tiny_mu <- rbind(r1 = c(1e-10, 0), r2 = c(-2e-10, 0))
  colnames(tiny_mu) <- c("a", "b")
  tiny_sigma <- diag(c(1e-20, 0))
  expect_lt(as.numeric(tiny_sigma[1L, 1L]), .Machine$double.eps)
  expect_silent(tiny <- callback(
    tiny_mu, 0, tiny_sigma, matrix(0, 2L, 1L), rownames(tiny_mu),
    1L, colnames(tiny_mu)))
  expect_true(all(is.finite(c(tiny$value, tiny$d_mu, tiny$d_Sigma))))
  expect_equal(tiny$value[, 1L], stats::pnorm(c(1, -2)))
})

test_that("v2.4 directional-share analytic derivatives are consistent", {
  direction <- c(a = 0.8, b = -0.4)
  callback <- sconjoint:::.scmix_v24_directional_share_target(
    direction, label = "direction_share")
  mu <- rbind(r1 = c(0.2, -0.1), r2 = c(-0.3, 0.25))
  colnames(mu) <- c("a", "b")
  Sigma <- matrix(c(0.35, 0.06, 0.06, 0.22), 2L)
  evaluate <- function(mu_value = mu, Sigma_value = Sigma) callback(
    mu_value, 0.12, Sigma_value, matrix(0, 2L, 1L), rownames(mu),
    1L, colnames(mu))
  answer <- evaluate()
  h <- 1e-6
  for (i in seq_len(nrow(mu))) for (j in seq_len(ncol(mu))) {
    up <- down <- mu
    up[i, j] <- up[i, j] + h
    down[i, j] <- down[i, j] - h
    numerical <- (evaluate(mu_value = up)$value[i, ] -
                    evaluate(mu_value = down)$value[i, ]) / (2 * h)
    expect_equal(unname(answer$d_mu[i, , j]), unname(numerical),
                 tolerance = 2e-7)
  }
  for (j in seq_len(2L)) {
    up <- down <- Sigma
    up[j, j] <- up[j, j] + h
    down[j, j] <- down[j, j] - h
    numerical <- (evaluate(Sigma_value = up)$value -
                    evaluate(Sigma_value = down)$value) / (2 * h)
    expect_equal(unname(answer$d_Sigma[, , j, j]), unname(numerical),
                 tolerance = 2e-7)
  }
  up <- down <- Sigma
  up[1L, 2L] <- up[1L, 2L] + h
  up[2L, 1L] <- up[2L, 1L] + h
  down[1L, 2L] <- down[1L, 2L] - h
  down[2L, 1L] <- down[2L, 1L] - h
  numerical <- (evaluate(Sigma_value = up)$value -
                  evaluate(Sigma_value = down)$value) / (2 * h)
  analytic <- answer$d_Sigma[, , 1L, 2L] +
    answer$d_Sigma[, , 2L, 1L]
  expect_equal(unname(analytic), unname(numerical), tolerance = 2e-7)
})

test_that("v2.4 subgroup and active-covariance callbacks are analytic", {
  X <- rbind(c(1, 0), c(0, 1), c(1, -1), c(0.5, 0.5))
  colnames(X) <- c("a", "b")
  ids <- c("r1", "r1", "r2", "r2")
  base <- sconjoint:::.scmix_v24_frozen_choice_target(X, ids)
  subgroup <- c(r2 = 0, r1 = 1)
  callback <- sconjoint:::.scmix_v24_subgroup_primitives(
    base, subgroup, "high")
  mu <- rbind(r1 = c(0.1, 0.2), r2 = c(-0.1, 0.3))
  Sigma <- diag(c(0.2, 0.1))
  result <- callback(mu, 0, Sigma, matrix(0, 2L, 1L), rownames(mu),
                     1L, colnames(X))
  expect_equal(result$value[, 2L], c(1, 0))
  expect_equal(unname(result$value[2L, 1L]), 0)
  expect_equal(result$d_mu[2L, 1L, ], c(0, 0))
  expect_equal(result$d_Sigma[2L, 1L, , ], matrix(0, 2L, 2L))

  trace_target <- sconjoint:::.scmix_v24_trace_sigma_target()
  trace_result <- trace_target(
    mu, 0, Sigma, matrix(0, 2L, 1L), rownames(mu), 1L, colnames(X))
  expect_equal(drop(trace_result$value), rep(sum(diag(Sigma)), 2L))
  expect_equal(trace_result$d_Sigma[1L, 1L, , ], diag(2L))
  expect_equal(trace_result$d_mu, array(0, c(2L, 1L, 2L)))
})

test_that("v2.4 heterogeneity adapter requests direct H_Z and H_R solves", {
  adapter <- sconjoint:::.scmix_v24_heterogeneity_dml_adapter(
    c(a = 1, b = 0), primitive_ids = c("hz", "hr"))
  expect_identical(names(adapter$plugin_targets), "heterogeneity_support")
  expect_s3_class(adapter$primitive_registry[[1L]],
                  "scmix_v24_primitive_adapter")
  expect_identical(adapter$primitive_registry[[1L]]$protocol_id,
                   "paperps-inference-2026-09-06-v2.4")
  expect_null(adapter$primitive_registry[[1L]]$fingerprint)
  expect_identical(adapter$release_finalizer, "scmix_v24_only")

  ## Exercise the exact minimal-primitive hook used inside scmix_dml().
  m <- c(-0.4, 0.2, 0.1, 0.7)
  N <- length(m)
  labels <- adapter$supporting_names
  value <- cbind(m, m^2, rep(0.3, N))
  colnames(value) <- labels
  gradient <- list(matrix(1, N, 3L), matrix(99, N, 3L),
                   matrix(2, N, 3L))
  tg <- list(
    value = value, labels = labels, gradient = gradient,
    gradient_h = gradient, gradient_h2 = gradient,
    derivative_source = rep("analytic", 3L),
    derivative_refinement_error = rep(0, 3L),
    derivative_refinement_protocol_exact = rep(TRUE, 3L))
  direct <- sconjoint:::.scmix_dml_v22_primitive_targets(
    tg, adapter$primitive_registry, train = 1:2, evaluation = 3:4)
  expect_identical(direct$labels, c("hz", "hr"))
  expect_length(direct$gradient, 2L)
  expect_equal(direct$gradient[[2L]], gradient[[3L]])
  expect_false(isTRUE(all.equal(direct$gradient[[1L]], gradient[[2L]])))
  expect_equal(direct$value[1:2, 1L], (m[1:2] - mean(m[1:2]))^2)
  ## Foldwise Riesz gradients must use only the outer-training center.  The
  ## held rows therefore retain that training center.  The reported H_Z
  ## estimand is assembled later from the global cross-fit first and second
  ## moments, rather than by averaging these fold-centered rows.
  expect_equal(direct$value[3:4, 1L], (m[3:4] - mean(m[1:2]))^2)
  assembled <- sconjoint:::.scmix_dml_global_hz_assembly(
    mean_direction = m,
    second_moment = m^2)
  expect_equal(assembled$estimate, mean(m^2) - mean(m)^2)
})

test_that("v2.4 adapter runs the full scmix_dml minimal-primitive path", {
  set.seed(88)
  N <- 48L
  T <- 3L
  K <- 2L
  p <- 2L
  z <- seq(-1, 1, length.out = N)
  respondent_id <- rep(sprintf("r%03d", seq_len(N)), each = T)
  respondent_index <- rep(seq_len(N), each = T)
  fold_respondent <- rep(seq_len(K), length.out = N)
  X <- matrix(stats::rnorm(N * T * p), ncol = p,
              dimnames = list(NULL, c("x1", "x2")))
  mu <- cbind(0.2 + 0.25 * z, -0.1 + 0.15 * z)
  A <- matrix(c(0.3, -0.2), p, 1L)
  latent <- stats::rnorm(N)
  loading_rows <- matrix(rep(A[, 1L], each = N * T), ncol = p)
  index <- 0.1 + rowSums(X *
    (mu[respondent_index, ] + latent[respondent_index] * loading_rows))
  gh <- list(U = matrix(c(-1, 0, 1), ncol = 1L),
             w = c(0.25, 0.5, 0.25))
  fit <- list(
    deltaX = X, y = stats::rbinom(N * T, 1L, stats::plogis(index)),
    respondent_id = respondent_id,
    fold_id = rep(fold_respondent, each = T), K = K,
    Z = matrix(z[respondent_index], ncol = 1L,
               dimnames = list(NULL, "z")),
    q = 1L, A_folds = replicate(K, A, simplify = FALSE), A_hat = A,
    gh = gh, kappa_folds = rep(0.1, K),
    mu_all_folds = replicate(K, mu, simplify = FALSE),
    attr_names = colnames(X), analysis_signature = "v24-target-test")
  basis <- replicate(K, cbind(one = 1, z = z), simplify = FALSE)
  adapter <- sconjoint:::.scmix_v24_heterogeneity_dml_adapter(
    c(x1 = 1, x2 = 0))
  inference <- sconjoint:::scmix_dml(
    fit, targets = character(), plugin_targets = adapter$plugin_targets,
    mu_basis = basis, nu_grid = NULL,
    riesz_rule = "fixed_n_train_minus_three_quarters",
    riesz_numerical_rank_rule = "dimension_epsilon",
    v22_registry = adapter$primitive_registry,
    active_eigenvalue_min = 1e-10,
    information_eigenvalue_min = 1e-12,
    multiplier_draws = 0L, seed = 12L)
  extracted <- adapter$extract(inference)
  expect_identical(names(inference$estimate), c("H_Z", "H_R"))
  expect_identical(colnames(inference$influence), c("H_Z", "H_R"))
  expect_identical(extracted$primitive_ids, c("H_Z", "H_R"))
  expect_identical(inference$v22_primitive_mode, TRUE)
  expect_length(inference$fold_details, 2L)
})

test_that("v2.4 heterogeneity transform retains only minimal influence", {
  estimate <- c(mean = 0.2, second_moment = 0.13,
                residual_variance = 0.25)
  influence <- cbind(
    mean = c(-1, 0, 1), second_moment = c(0.2, -0.1, -0.1),
    residual_variance = c(-0.2, 0, 0.2))
  rownames(influence) <- paste0("r", 1:3)
  result <- sconjoint:::.scmix_v24_heterogeneity_transform(
    estimate, influence)
  expect_equal(result$estimate, c(H_Z = 0.09, H_R = 0.25))
  expect_identical(colnames(result$influence), c("H_Z", "H_R"))
  expect_equal(result$influence[, "H_Z"],
               influence[, "second_moment"] - 0.4 * influence[, "mean"])
  expect_equal(result$share, 0.09 / 0.34)
  expect_false(result$production_release_eligible)
})

test_that("v2.4 common normal multiplier uses exact type-1 quantile", {
  ids <- paste0("r", 1:5)
  bank <- structure(list(
    protocol_id = "paperps-inference-2026-09-06-v2.4",
    respondent_id = ids, draws = 5L, seed = 19L,
    multipliers = matrix(c(
      -1, 0, 1, 0, 0,
      0, -1, 0, 1, 0,
      1, 1, -1, -1, 0,
      2, -1, 0, 0, -1,
      -2, 0, 1, 0, 1), nrow = 5L,
      dimnames = list(ids, NULL))),
    class = c("scmix_v24_multiplier_bank", "list"))
  influence <- cbind(a = c(-2, -1, 0, 1, 2),
                     b = c(1, -1, 1, -1, 0))
  rownames(influence) <- ids
  result <- sconjoint:::.scmix_v24_family_multiplier(
    c(a = 0.2, b = 0.4), influence, ids, bank)
  centered <- sweep(influence, 2L, colMeans(influence), `-`)
  statistic <- crossprod(bank$multipliers, centered)
  statistic <- sweep(statistic, 2L, sqrt(colSums(centered^2)), `/`)
  maximum <- apply(abs(statistic), 1L, max)
  expect_equal(result$critical, sort(maximum)[ceiling(0.95 * 5)])
  expect_identical(result$method, "common_normal_max_t_type1")
  expect_false(result$release_eligible)

  set.seed(991)
  state <- .Random.seed
  kind <- RNGkind()
  generated_a <- sconjoint:::.scmix_v24_multiplier_bank(ids, 20L, 71L)
  expect_identical(.Random.seed, state)
  expect_identical(RNGkind(), kind)
  generated_b <- sconjoint:::.scmix_v24_multiplier_bank(rev(ids), 20L, 71L)
  expect_identical(generated_a$multipliers, generated_b$multipliers)
})

test_that("v2.4 rectangle maps respect each target range", {
  probability <- sconjoint:::.scmix_v24_map_rectangle(
    "probability", -0.2, 1.3)
  expect_equal(c(probability$lower, probability$upper), c(0, 1))

  subgroup <- sconjoint:::.scmix_v24_map_rectangle(
    "subgroup_ratio", c(-0.1, 0.2), c(0.3, 0.8))
  expect_true(subgroup$nonempty)
  expect_equal(c(subgroup$lower, subgroup$upper), c(0, 1))

  heterogeneity <- sconjoint:::.scmix_v24_map_rectangle(
    "heterogeneity_share", c(-0.2, -0.1), c(0.4, 0.5))
  expect_true(heterogeneity$uninformative)
  expect_equal(c(heterogeneity$lower, heterogeneity$upper), c(0, 1))

  mrs <- sconjoint:::.scmix_v24_map_rectangle(
    "mrs", c(-1, -0.2), c(2, 0.3))
  expect_true(mrs$uninformative)
  expect_true(all(is.na(c(mrs$lower, mrs$upper))))

  chart <- sconjoint:::.scmix_v24_chart_point(
    "probability", plugin = 0.9, raw = 1.2)
  expect_true(chart$interior)
  expect_true(chart$target > 0 && chart$target < 1)
})

test_that("v2.4 family finalizer uses all and only registered primitives", {
  ids <- paste0("r", seq_len(20L))
  influence <- cbind(H_Z = seq(-1, 1, length.out = 20L),
                     H_R = sin(seq_len(20L)))
  rownames(influence) <- ids
  bank <- sconjoint:::.scmix_v24_multiplier_bank(ids, 9999L, 73L)
  result <- sconjoint:::.scmix_v24_finalize_family(
    "directional_heterogeneity", c(H_Z = 0.2, H_R = 0.4), influence,
    ids, bank, plugin = c(H_Z = 0.19, H_R = 0.39))
  expect_true(result$complete)
  expect_identical(result$multiplier$primitive_id, c("H_Z", "H_R"))
  expect_true(all(c(
    "heterogeneity_explained_variance",
    "heterogeneity_residual_variance",
    "heterogeneity_explained_share") %in% names(result$members)))
  expect_true(result$members$heterogeneity_explained_share$estimate >= 0)
  expect_true(result$members$heterogeneity_explained_share$estimate <= 1)
})

test_that("v2.4 Monte Carlo summaries match frozen formulas", {
  interval <- sconjoint:::.scmix_v24_wilson90(90L, 100L)
  z <- stats::qnorm(0.95)
  center <- (0.9 + z^2 / 200) / (1 + z^2 / 100)
  half <- z * sqrt(0.9 * 0.1 / 100 + z^2 / 40000) /
    (1 + z^2 / 100)
  expect_equal(interval, c(lower = center - half, upper = center + half))
  expect_equal(
    sconjoint:::.scmix_v24_clopper_pearson_upper95(4L, 100L),
    stats::qbeta(0.95, 5, 96))
  expect_equal(
    sconjoint:::.scmix_v24_clopper_pearson_upper95(100L, 100L), 1)
})

test_that("v2.4 family-cell approval is fully mechanical", {
  regular <- sconjoint:::.scmix_v24_family_approval_rule(
    "regular", rep(TRUE, 100L), seq_len(100L) <= 80L,
    seq_len(100L) <= 72L)
  expect_true(regular$passed)

  boundary <- sconjoint:::.scmix_v24_family_approval_rule(
    "exact_boundary", rep(TRUE, 100L), seq_len(100L) <= 4L,
    rep(FALSE, 100L))
  expect_true(boundary$passed)

  weak_low <- sconjoint:::.scmix_v24_family_approval_rule(
    "weak_or_bounded", rep(TRUE, 100L), seq_len(100L) <= 5L,
    rep(FALSE, 100L))
  expect_true(weak_low$passed)
  weak_middle <- sconjoint:::.scmix_v24_family_approval_rule(
    "weak_or_bounded", rep(TRUE, 100L), seq_len(100L) <= 10L,
    rep(FALSE, 100L))
  expect_identical(weak_middle$state, "inconclusive")
  weak_high <- sconjoint:::.scmix_v24_family_approval_rule(
    "weak_or_bounded", rep(TRUE, 100L), seq_len(100L) <= 30L,
    seq_len(100L) <= 27L)
  expect_true(weak_high$passed)

  approval <- data.frame(
    family_id = c("regular_family", "boundary_family", "weak_family",
                  "control_family"),
    cell_id = "cell", required = c(TRUE, TRUE, TRUE, FALSE),
    criterion = c("regular", "exact_boundary", "weak_or_bounded",
                  "descriptive_only"), stringsAsFactors = FALSE)
  make_rows <- function(family, released, covered) data.frame(
    cell_id = "cell", family_id = family, replication = seq_len(100L),
    completed = TRUE, released = released, covered = covered,
    stringsAsFactors = FALSE)
  replications <- rbind(
    make_rows("regular_family", seq_len(100L) <= 80L,
              seq_len(100L) <= 72L),
    make_rows("boundary_family", seq_len(100L) <= 4L,
              rep(FALSE, 100L)),
    make_rows("weak_family", seq_len(100L) <= 5L,
              rep(FALSE, 100L)),
    make_rows("control_family", rep(FALSE, 100L),
              rep(FALSE, 100L)))
  assessment <- sconjoint:::.scmix_v24_assess_validation(
    replications, approval)
  expect_true(assessment$approved)
  expect_true(assessment$all_cell_completion_pass)
  expect_identical(
    assessment$family_cell$state[
      assessment$family_cell$family_id == "control_family"],
    "descriptive_only")
})
