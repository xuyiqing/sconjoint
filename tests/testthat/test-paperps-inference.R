.paperps_dml_mock <- function(N = 120L, T = 4L, K = 3L, q = 0L,
                              seed = 481L) {
  set.seed(seed)
  p <- 2L
  Z_resp <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
                   dimnames = list(NULL, "z"))
  respondent_id <- rep(sprintf("r%03d", seq_len(N)), each = T)
  respondent_index <- rep(seq_len(N), each = T)
  fold_resp <- rep(seq_len(K), length.out = N)
  fold_id <- rep(fold_resp, each = T)
  deltaX <- matrix(stats::rnorm(N * T * p), ncol = p,
                   dimnames = list(NULL, c("x1", "x2")))
  mu <- cbind(0.2 + 0.3 * Z_resp[, 1L],
              -0.1 + 0.2 * Z_resp[, 1L])
  kappa <- 0.15
  index <- kappa + rowSums(deltaX * mu[respondent_index, , drop = FALSE])
  y <- stats::rbinom(length(index), 1L, stats::plogis(index))
  if (q == 0L) {
    A_folds <- replicate(K, matrix(numeric(0), p, 0L), simplify = FALSE)
    A_hat <- matrix(numeric(0), p, 0L)
    gh <- list(U = matrix(numeric(0), 1L, 0L), w = 1)
  } else {
    A_folds <- replicate(K, matrix(c(0.35, -0.2), p, 1L),
                         simplify = FALSE)
    A_hat <- matrix(c(0.35, -0.2), p, 1L)
    gh <- list(U = matrix(c(-1, 0, 1), ncol = 1L),
               w = c(0.25, 0.5, 0.25))
  }
  list(
    deltaX = deltaX, y = y, respondent_id = respondent_id,
    fold_id = fold_id, K = K,
    Z = Z_resp[respondent_index, , drop = FALSE],
    q = q, A_folds = A_folds, A_hat = A_hat, gh = gh,
    kappa_folds = rep(kappa, K),
    mu_all_folds = replicate(K, mu, simplify = FALSE),
    attr_names = colnames(deltaX),
    analysis_signature = paste("mock", N, T, K, q, seed, sep = "-")
  )
}

.paperps_dml_basis <- function(fit) {
  layout <- sconjoint:::.scmix_dml_layout(fit)
  B <- cbind(one = 1, z = layout$Z_resp[, 1L])
  replicate(fit$K, B, simplify = FALSE)
}

.paperps_dml_verification <- function(fit, mu_basis) {
  checks <- data.frame(
    resolution = c(8L, 16L), qoi.theta = c(0, 0),
    likelihood.mean = c(0, 0), score.max = c(0, 0),
    riesz.residual = c(0, 0), if_l2.norm = c(0, 0), se.theta = c(0, 0))
  numerical_artifact <- structure(
    list(gate = list(pass = TRUE),
         checks = checks, refit_count = 2L,
         refit_analysis_signatures = rep(fit$analysis_signature, 2L),
         analysis_signature = fit$analysis_signature,
         signature_match = TRUE),
    class = c("scmix_integration_refinement", "list"))
  optimization_artifact <- structure(
    list(all_selected_tolerances_met = TRUE, all_objectives_finite = TRUE,
         all_computational_gates_pass = TRUE, any_bound_activity = FALSE,
         analysis_signature = fit$analysis_signature, signature_match = TRUE),
    class = c("scmix_optimization_audit", "list"))
  sconjoint::scmix_inference_verification(
    fit = fit, mu_basis = mu_basis,
    tangent = list(
      type = "fitted_sieve", prespecified = TRUE,
      identified_directions = TRUE, training_only = TRUE,
      outer_fold_specific = TRUE,
      approximation_argument = "prespecified sieve approximation audit",
      product_rate_argument = "cross-fit nuisance product-rate argument",
      provenance = "prespecified tangent audit"),
    numerical = list(
      artifact = numerical_artifact,
      rate_argument = "refinement error negligible at the root-N scale",
      provenance = "quadrature refinement audit"),
    optimization = list(
      artifact = optimization_artifact,
      gap_argument = "attained criterion gap negligible at the root-N scale",
      provenance = "optimization-gap audit")
  )
}

test_that("complete-sequence scores are sums over observed tasks", {
  dx <- matrix(c(1, -0.5, 2, 1, -1, 0.25), ncol = 2L, byrow = TRUE)
  y <- c(1, 0, 1)
  mu <- matrix(c(0.2, -0.1), nrow = 1L)
  kappa <- 0.3
  basis <- matrix(1, 1L, 1L, dimnames = list(NULL, "(Intercept)"))
  got <- sconjoint:::.scmix_sequence_scores_sieve(
    dx, y, rep(1L, 3L), mu, kappa,
    matrix(numeric(0), 2L, 0L),
    list(U = matrix(numeric(0), 1L, 0L), w = 1),
    basis, matrix(numeric(0), 0L, 0L)
  )
  residual <- y - stats::plogis(as.numeric(kappa + dx %*% mu[1L, ]))
  expected <- c(sum(residual), colSums(dx * residual))
  expect_equal(as.numeric(got$score[1L, ]), expected, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(got$score[1L, 1L], mean(residual))))
})

test_that("mixed complete-sequence scores satisfy the Fisher identity", {
  dx <- matrix(c(1, -0.5, 0.25, 1, -1, 0.4), ncol = 2L, byrow = TRUE)
  y <- c(1, 0, 1)
  mu <- matrix(c(0.15, -0.2), 1L, 2L)
  A <- matrix(c(0.3, -0.25), 2L, 1L)
  gh <- list(U = matrix(c(-1, 0, 1), ncol = 1L),
             w = c(0.25, 0.5, 0.25))
  B <- matrix(1, 1L, 1L, dimnames = list(NULL, "one"))
  H <- diag(2L)
  evaluate <- function(mu_value = mu, kappa_value = 0.1, A_value = A) {
    sconjoint:::.scmix_sequence_scores_sieve(
      dx, y, rep(1L, 3L), mu_value, kappa_value, A_value, gh, B, H
    )
  }
  base <- evaluate()
  eps <- 1e-6
  numeric_score <- numeric(5L)
  numeric_score[1L] <- (evaluate(kappa_value = 0.1 + eps)$loglik -
                         evaluate(kappa_value = 0.1 - eps)$loglik) / (2 * eps)
  for (j in 1:2) {
    up <- down <- mu
    up[1L, j] <- up[1L, j] + eps
    down[1L, j] <- down[1L, j] - eps
    numeric_score[1L + j] <- (evaluate(mu_value = up)$loglik -
                              evaluate(mu_value = down)$loglik) / (2 * eps)
    Au <- Ad <- A
    Au[j, 1L] <- Au[j, 1L] + eps
    Ad[j, 1L] <- Ad[j, 1L] - eps
    numeric_score[3L + j] <- (evaluate(A_value = Au)$loglik -
                              evaluate(A_value = Ad)$loglik) / (2 * eps)
  }
  expect_equal(as.numeric(base$score[1L, ]), numeric_score, tolerance = 2e-7)
})

test_that("loading tangent removes rotations and retains structural directions", {
  A <- matrix(c(0.8, 0.1, -0.2, 0.4, 0.3, -0.7), 3L, 2L)
  H <- sconjoint:::.scmix_horizontal_basis(A)
  Omega <- matrix(c(0, -1, 1, 0), 2L, 2L)
  vertical <- as.vector(A %*% Omega)
  expect_equal(dim(H), c(6L, 5L))
  expect_equal(crossprod(H), diag(5L), tolerance = 1e-10)
  expect_equal(as.numeric(crossprod(H, vertical)), rep(0, 5L),
               tolerance = 1e-10)
})

test_that("scmix_dml returns foldwise direct-PZ and respondent inference", {
  fit <- .paperps_dml_mock()
  B <- .paperps_dml_basis(fit)
  square_target <- function(mu, kappa, Sigma, Z, respondent_id,
                            fold, attr_names) {
    list(target_type = "rowwise_expectation",
         value = mu[, 1L]^2 + kappa,
         d_mu = cbind(2 * mu[, 1L], 0),
         d_kappa = rep(1, nrow(mu)), labels = "square")
  }
  set.seed(2026)
  rng_before <- .Random.seed
  out <- sconjoint:::scmix_dml(
    fit, plugin_targets = list(square = square_target),
    mu_basis = B, nu_grid = 0,
    verification = .paperps_dml_verification(fit, B),
    information_eigenvalue_min = 1e-12,
    multiplier_draws = 49L, seed = 31L
  )
  expect_identical(.Random.seed, rng_before)
  expect_true(out$inference_available)
  expect_identical(out$status, "conditional_available")
  expect_identical(out$inference_claim,
                   "conditional_on_documented_high_level_assumptions")
  expect_identical(out$analysis_signature, fit$analysis_signature)
  expect_equal(dim(out$influence), c(120L, 3L))
  expect_equal(unname(colMeans(out$influence)), rep(0, 3L), tolerance = 1e-12)
  expect_equal(out$diagnostic_covariance,
               crossprod(out$influence) / 120^2, tolerance = 1e-12)
  expect_equal(out$variance_of_influence,
               crossprod(out$influence) / 120, tolerance = 1e-12)
  expect_equal(out$se, out$diagnostic_se, tolerance = 1e-12)
  expect_equal(out$diagnostic_se,
               sqrt(diag(out$diagnostic_covariance)), tolerance = 1e-12)
  expect_equal(out$covariance, out$diagnostic_covariance, tolerance = 1e-12)
  expect_equal(out$uncentered_influence,
               out$direct_empirical_PZ + out$score_correction,
               tolerance = 1e-12)
  expect_identical(out$multiplier$status, "available")
  expect_true(all(out$derivative_source[, "square"] == "analytic"))
  expect_match(out$theory_scope, "Finite-sieve/high-level")

  layout <- sconjoint:::.scmix_dml_layout(fit)
  for (k in seq_len(fit$K)) {
    held <- which(layout$fold_resp == k)
    expected <- sweep(out$plugin_contribution[held, , drop = FALSE], 2L,
                      out$fold_details[[k]]$training_plugin_mean, `-`)
    expect_equal(out$direct_empirical_PZ[held, , drop = FALSE], expected,
                 tolerance = 1e-12)
    expect_equal(out$fold_details[[k]]$selected_nu, 0)
    expect_true(out$fold_details[[k]]$information_structural_min > 0)
  }
})

test_that("numeric derivatives require opt-in and refinement", {
  fit <- .paperps_dml_mock(N = 60L)
  B <- .paperps_dml_basis(fit)
  smooth <- function(mu, kappa, Sigma, Z, respondent_id,
                     fold, attr_names) {
    list(target_type = "rowwise_expectation",
         value = mu[, 1L]^2 + 0.5 * kappa)
  }
  expect_error(
    sconjoint:::scmix_dml(
      fit, targets = character(), plugin_targets = list(smooth = smooth),
      nu_grid = 0, multiplier_draws = 0L),
    "allow_numeric_derivatives"
  )
  out <- sconjoint:::scmix_dml(
    fit, targets = character(), plugin_targets = list(smooth = smooth),
    mu_basis = B, nu_grid = 0, allow_numeric_derivatives = TRUE,
    verification = .paperps_dml_verification(fit, B),
    information_eigenvalue_min = 1e-12,
    multiplier_draws = 0L
  )
  expect_true(out$inference_available)
  expect_identical(out$status, "conditional_available")
  expect_identical(colnames(out$influence), "smooth")
  expect_true(all(out$derivative_source == "numeric_refined"))
  expect_true(all(out$derivative_refinement_error < 1e-3))
})

test_that("rank and active-eigenvalue boundaries withhold ordinary inference", {
  fit <- .paperps_dml_mock(q = 1L)
  fit$A_folds[[2L]][, ] <- 0
  expect_warning(
    out <- sconjoint:::scmix_dml(fit, multiplier_draws = 0L),
    "rank boundary"
  )
  expect_false(out$inference_available)
  expect_identical(out$status, "withheld")
  expect_false(out$rank_gate$pass[out$rank_gate$component == "fold 2"])

  full_boundary <- .paperps_dml_mock(q = 1L)
  full_boundary$A_hat[,] <- 0
  expect_warning(
    full_out <- sconjoint:::scmix_dml(full_boundary,
                                      multiplier_draws = 0L),
    "rank boundary"
  )
  expect_false(full_out$rank_gate$pass[full_out$rank_gate$component == "full"])
})

test_that("unverified high-level conditions never yield available inference", {
  fit <- .paperps_dml_mock(N = 60L)
  out <- sconjoint:::scmix_dml(fit, nu_grid = 0, multiplier_draws = 0L)
  expect_identical(out$status, "conditional_unverified")
  expect_false(out$inference_available)
  expect_true(all(is.na(out$se)))
  expect_true(all(is.finite(out$diagnostic_se)))
  expect_match(out$reason, "tangent evidence")
  expect_match(out$reason, "numerical evidence")
  expect_match(out$reason, "optimization evidence")

  raw_assertions <- list(
    tangent = list(pass = TRUE, provenance = "asserted"),
    numerical = list(pass = TRUE, provenance = "asserted"),
    optimization = list(pass = TRUE, provenance = "asserted"))
  raw <- sconjoint:::scmix_dml(
    fit, nu_grid = 0, verification = raw_assertions, multiplier_draws = 0L)
  expect_identical(raw$status, "conditional_unverified")
  expect_false(raw$inference_available)
})

test_that("verification constructor validates classed audit artifacts", {
  fit <- .paperps_dml_mock()
  B <- .paperps_dml_basis(fit)
  verified <- .paperps_dml_verification(fit, B)
  expect_s3_class(verified, "scmix_inference_verification")
  verified$numerical$artifact$gate$pass <- FALSE
  rechecked <- sconjoint:::.scmix_dml_verification(fit, verified, B)
  expect_false(all(rechecked$pass))
  bad_numerical <- structure(
    list(gate = list(pass = FALSE),
         checks = data.frame(resolution = c(8L, 16L))),
    class = c("scmix_integration_refinement", "list"))
  good_optimization <- structure(
    list(all_selected_tolerances_met = TRUE, all_objectives_finite = TRUE,
         all_computational_gates_pass = TRUE, any_bound_activity = FALSE,
         analysis_signature = fit$analysis_signature, signature_match = TRUE),
    class = c("scmix_optimization_audit", "list"))
  expect_error(
    sconjoint::scmix_inference_verification(
      fit = fit, mu_basis = B,
      tangent = list(
        type = "fitted_sieve", prespecified = TRUE,
        identified_directions = TRUE, training_only = TRUE,
        outer_fold_specific = TRUE,
        approximation_argument = "argument",
        product_rate_argument = "argument", provenance = "record"),
      numerical = list(artifact = bad_numerical,
                       rate_argument = "argument", provenance = "record"),
      optimization = list(artifact = good_optimization,
                          gap_argument = "argument", provenance = "record")),
    "fit-linked scmix_integration_refinement"
  )
})

test_that("fold identifiers and task-row means are validated before collapse", {
  fit <- .paperps_dml_mock(N = 30L)
  fit$fold_id[1L] <- 1.5
  expect_error(sconjoint:::.scmix_dml_layout(fit), "positive integer")

  fit <- .paperps_dml_mock(N = 30L)
  task_mu <- fit$mu_all_folds[[1L]][rep(seq_len(30L), each = 4L), ]
  task_mu[2L, 1L] <- task_mu[2L, 1L] + 0.1
  fit$mu_all_folds[[1L]] <- task_mu
  layout <- sconjoint:::.scmix_dml_layout(fit)
  expect_error(sconjoint:::.scmix_dml_resolve_mu(fit, layout),
               "not constant within respondent")
})

test_that("fold-specific quadrature grids are preserved", {
  fit <- .paperps_dml_mock(q = 1L)
  fit$gh_folds <- lapply(seq_len(fit$K), function(k) {
    list(U = matrix(c(-k, 0, k), ncol = 1L), w = c(1, 2, 1))
  })
  fit$gh <- NULL
  got <- sconjoint:::.scmix_dml_resolve_A_gh(fit, 2L, fit$K)
  expect_length(got$gh_folds, fit$K)
  expect_equal(got$gh_folds[[2L]]$U[, 1L], c(-2, 0, 2))
  expect_equal(sum(got$gh_folds[[3L]]$w), 1)
  fit$integration_grids_folds <- fit$gh_folds
  fit$gh_folds <- NULL
  got_assembled <- sconjoint:::.scmix_dml_resolve_A_gh(fit, 2L, fit$K)
  expect_equal(got_assembled$gh_folds[[3L]]$U[, 1L], c(-3, 0, 3))
})

test_that("plugin contract is rowwise and rotation invariant", {
  fit <- .paperps_dml_mock(q = 1L)
  generic <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    mean(mu[, 1L])
  }
  expect_error(
    sconjoint:::scmix_dml(fit, targets = character(),
                          plugin_targets = list(generic = generic),
                          nu_grid = 0, multiplier_draws = 0L),
    "rowwise_expectation"
  )
  raw_loading_derivative <- function(mu, kappa, Sigma, Z, respondent_id,
                                     fold, attr_names) {
    list(target_type = "rowwise_expectation", value = mu[, 1L],
         d_mu = cbind(1, 0), d_kappa = rep(0, nrow(mu)),
         d_A = array(0, c(nrow(mu), 1L, 2L, 1L)))
  }
  expect_error(
    sconjoint:::scmix_dml(fit, targets = character(),
                          plugin_targets = list(bad = raw_loading_derivative),
                          nu_grid = 0, multiplier_draws = 0L),
    "d_Sigma but not d_A"
  )

  sigma_trace <- function(mu, kappa, Sigma, Z, respondent_id,
                          fold, attr_names) {
    ds <- array(0, c(nrow(mu), 1L, ncol(mu), ncol(mu)))
    for (i in seq_len(nrow(mu))) ds[i, 1L, , ] <- diag(ncol(mu))
    list(target_type = "rowwise_expectation",
         value = rep(sum(diag(Sigma)), nrow(mu)),
         d_mu = matrix(0, nrow(mu), ncol(mu)),
         d_kappa = rep(0, nrow(mu)), d_Sigma = ds)
  }
  out <- sconjoint:::scmix_dml(
    fit, targets = character(), plugin_targets = list(trace = sigma_trace),
    mu_basis = (B <- .paperps_dml_basis(fit)), nu_grid = 0,
    verification = .paperps_dml_verification(fit, B),
    active_eigenvalue_min = 1e-8,
    information_eigenvalue_min = 1e-12,
    multiplier_draws = 0L)
  expect_identical(out$status, "conditional_available")
  expect_true(out$inference_available)
  expect_true(all(out$derivative_source == "analytic"))
})

test_that("typed paper quantities expose rotation-invariant analytic primitives", {
  mu <- matrix(c(0.2, -0.1, 0.4, 0.3), 2L, 2L, byrow = TRUE)
  Sigma <- matrix(c(0.25, 0.05, 0.05, 0.16), 2L, 2L)
  args <- list(mu = mu, kappa = 0.15, Sigma = Sigma,
               Z = matrix(0, 2L, 0L), respondent_id = c("a", "b"),
               fold = 1L, attr_names = c("x1", "x2"))

  tau <- sconjoint:::scmix_inference_target(
    "tau", contrast = c(x1 = 1, x2 = -2))
  got_tau <- do.call(tau, args)
  expect_identical(got_tau$target_type, "rowwise_expectation")
  expect_equal(as.numeric(got_tau$value), as.numeric(mu %*% c(1, -2)))
  expect_true(isTRUE(got_tau$sigma_invariant))

  choice_zero <- sconjoint:::scmix_inference_target(
    "choice", contrast = c(1, 0), n_nodes = 9L)
  zero_args <- args
  zero_args$Sigma <- matrix(0, 2L, 2L)
  got_choice <- do.call(choice_zero, zero_args)
  index <- args$kappa + mu[, 1L]
  expect_equal(as.numeric(got_choice$value), stats::plogis(index),
               tolerance = 1e-12)
  expect_true(all(is.finite(got_choice$d_Sigma)))

  sign_target <- sconjoint:::scmix_inference_target(
    "sign", contrast = c(1, -1), variance_floor = 1e-7)
  got_sign <- do.call(sign_target, args)
  expect_true(all(is.finite(got_sign$value)))
  expect_true(all(is.finite(got_sign$d_mu)))
  expect_true(all(is.finite(got_sign$d_Sigma)))
  expect_error(
    sconjoint:::scmix_inference_target("sign", contrast = c(1, -1)),
    "explicit prespecified positive variance_floor"
  )

  covariance_target <- sconjoint:::scmix_inference_target(
    "covariance_primitives")
  got_covariance <- do.call(covariance_target, args)
  expect_equal(dim(got_covariance$value), c(2L, 8L))
  expect_identical(
    got_covariance$labels,
    c("mean[x1]", "mean[x2]", "second[x1,x1]", "second[x2,x1]",
      "second[x2,x2]", "residual[x1,x1]", "residual[x2,x1]",
      "residual[x2,x2]"))
  expect_equal(got_covariance$value[, "residual[x2,x1]"], rep(0.05, 2L))
})

test_that("delta bridge implements gated subgroup, MRS, H, and Omega objects", {
  make_inference <- function(estimate, covariance = diag(length(estimate)) / 100) {
    dimnames(covariance) <- list(names(estimate), names(estimate))
    structure(list(
      status = "conditional_available", inference_available = TRUE,
      inference_claim = "conditional_on_documented_high_level_assumptions",
      estimate = estimate,
      diagnostic_covariance = covariance,
      target_inference_available = stats::setNames(
        rep(TRUE, length(estimate)), names(estimate))),
      class = c("scmix_dml", "list"))
  }

  subgroup_est <- c(weighted = 0.6, probability = 0.4)
  subgroup <- sconjoint:::scmix_delta_transform(
    make_inference(subgroup_est), "subgroup_ratio", names(subgroup_est),
    denominator_margin = 0.1)
  expect_equal(unname(subgroup$estimate), 1.5)
  expect_identical(subgroup$status, "conditional_available")

  mrs_est <- c(numerator = 0.3, denominator = -0.2)
  mrs <- sconjoint:::scmix_delta_transform(
    make_inference(mrs_est), "mrs", names(mrs_est),
    denominator_margin = 0.05)
  expect_equal(unname(mrs$estimate), 1.5)
  expect_equal(as.numeric(mrs$jacobian), c(5, 7.5))

  known_denominator <- make_inference(mrs_est, diag(c(0.01, 0)))
  known_denominator$target_inference_available[["denominator"]] <- FALSE
  known_mrs <- sconjoint:::scmix_delta_transform(
    known_denominator, "mrs", names(mrs_est), denominator_margin = 0.05)
  expect_identical(known_mrs$status, "conditional_available")
  expect_true(known_mrs$target_inference_available[["mrs"]])

  h_est <- c(mean = 1, second = 5, residual = 2)
  h <- sconjoint:::scmix_delta_transform(
    make_inference(h_est), "directional_heterogeneity", names(h_est),
    total_margin = 0.1)
  expect_equal(h$estimate, c(H_Z = 4, H_R = 2, H_T = 6,
                             share_Z = 2 / 3))
  expect_true(all(h$target_inference_available))

  cov_est <- c(
    `mean[x1]` = 1, `mean[x2]` = 2,
    `second[x1,x1]` = 3, `second[x2,x1]` = 3,
    `second[x2,x2]` = 8, `residual[x1,x1]` = 0.5,
    `residual[x2,x1]` = 0.1, `residual[x2,x2]` = 0.7)
  omega <- sconjoint:::scmix_delta_transform(
    make_inference(cov_est), "covariance_decomposition", names(cov_est))
  expect_equal(unname(omega$estimate[c("Omega_Z[1,1]", "Omega_Z[2,1]",
                                       "Omega_Z[2,2]")]), c(2, 1, 4))
  expect_equal(unname(omega$estimate[c("Omega_T[1,1]", "Omega_T[2,1]",
                                       "Omega_T[2,2]")]), c(2.5, 1.1, 4.7))

  failed <- sconjoint:::scmix_delta_transform(
    make_inference(mrs_est), "mrs", names(mrs_est),
    denominator_margin = 0.5)
  expect_identical(failed$status, "conditional_unverified")
  expect_true(is.na(failed$se[["mrs"]]))
  expect_false(failed$target_inference_available[["mrs"]])
})

test_that("verification artifacts must match the live fit and basis", {
  fit <- .paperps_dml_mock()
  B <- .paperps_dml_basis(fit)
  verified <- .paperps_dml_verification(fit, B)
  mismatch <- verified
  mismatch$numerical$artifact$analysis_signature <- "different-analysis"
  evidence <- sconjoint:::.scmix_dml_verification(fit, mismatch, B)
  expect_false(any(evidence$pass))

  changed_basis <- lapply(B, function(x) x[, 1L, drop = FALSE])
  evidence_basis <- sconjoint:::.scmix_dml_verification(
    fit, verified, changed_basis)
  expect_false(any(evidence_basis$pass))

  expect_error(
    sconjoint:::scmix_inference_verification(
      fit = fit, mu_basis = NULL, tangent = list(), numerical = list(),
      optimization = list()),
    "default fixed linear tangent"
  )
})

test_that("diagnostic-only and inference-ineligible fits fail closed", {
  fit <- .paperps_dml_mock()
  B <- .paperps_dml_basis(fit)
  verification <- .paperps_dml_verification(fit, B)

  diagnostic_fit <- fit
  diagnostic_fit$diagnostic_only <- TRUE
  expect_error(.paperps_dml_verification(diagnostic_fit, B),
               "not eligible.*diagnostic_only")
  diagnostic_out <- sconjoint:::scmix_dml(
    diagnostic_fit, mu_basis = B, nu_grid = 0,
    verification = verification, multiplier_draws = 0L)
  expect_identical(diagnostic_out$status, "conditional_unverified")
  expect_false(diagnostic_out$inference_available)
  expect_match(diagnostic_out$reason, "diagnostic-only.*diagnostic_only")

  ineligible_fit <- fit
  ineligible_fit$eligible_for_ordinary_inference <- FALSE
  expect_error(.paperps_dml_verification(ineligible_fit, B),
               "not eligible.*eligible_for_ordinary_inference")
  ineligible_out <- sconjoint:::scmix_dml(
    ineligible_fit, mu_basis = B, nu_grid = 0,
    verification = verification, multiplier_draws = 0L)
  expect_identical(ineligible_out$status, "conditional_unverified")
  expect_false(ineligible_out$inference_available)
  expect_match(ineligible_out$reason,
               "ineligible.*eligible_for_ordinary_inference")

  explicit_ok <- fit
  explicit_ok$diagnostic_only <- FALSE
  explicit_ok$eligible_for_ordinary_inference <- TRUE
  explicit_verification <- .paperps_dml_verification(explicit_ok, B)
  expect_true(explicit_verification$fit_eligibility$pass)
})

test_that("structural-norm whitening removes basis-scale dependence", {
  fit <- .paperps_dml_mock()
  B <- .paperps_dml_basis(fit)
  Bs <- lapply(B, function(x) {
    sweep(x, MARGIN = 2L, STATS = c(7, 0.2), FUN = `*`)
  })
  a <- sconjoint:::scmix_dml(
    fit, mu_basis = B, nu_grid = 0,
    verification = .paperps_dml_verification(fit, B), multiplier_draws = 0L)
  b <- sconjoint:::scmix_dml(
    fit, mu_basis = Bs, nu_grid = 0,
    verification = .paperps_dml_verification(fit, Bs),
                             multiplier_draws = 0L)
  expect_equal(a$estimate, b$estimate, tolerance = 1e-10)
  expect_equal(a$influence, b$influence, tolerance = 1e-9)
  expect_equal(a$diagnostic_covariance, b$diagnostic_covariance,
               tolerance = 1e-10)
})

test_that("margins and penalties fail closed", {
  fit <- .paperps_dml_mock(N = 30L)
  expect_error(sconjoint:::scmix_dml(fit, active_eigenvalue_min = 0),
               "strictly positive scalar when supplied")
  expect_error(sconjoint:::scmix_dml(fit, information_eigenvalue_min = 0),
               "strictly positive scalar when supplied")
  B <- .paperps_dml_basis(fit)
  missing_margin <- sconjoint:::scmix_dml(
    fit, mu_basis = B, nu_grid = 0,
    verification = .paperps_dml_verification(fit, B),
    multiplier_draws = 0L)
  expect_identical(missing_margin$status, "conditional_unverified")
  expect_match(missing_margin$reason,
               "missing explicit prespecified generalized-information margin")
  tiny_full_rank <- sconjoint:::.scmix_dml_rank_gate(
    A_folds = list(matrix(c(3e-5, -2e-5), 2L, 1L)), q = 1L,
    active_eigenvalue_min = 1e-12, rank_tolerance = 1e-8)
  expect_true(tiny_full_rank$rank_pass)
  expect_true(tiny_full_rank$margin_pass)
  expect_error(sconjoint:::scmix_dml(fit, riesz_penalty = "not-a-penalty"),
               "identity.*numeric")
  expect_error(sconjoint:::scmix_dml(fit, riesz_penalty = c(-1, 1, 1, 1, 1)),
               "positive semidefinite")
})

test_that("multiplier excludes zero-standard-error targets", {
  fit <- .paperps_dml_mock()
  constant <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    list(target_type = "rowwise_expectation", value = rep(1, nrow(mu)),
         d_mu = matrix(0, nrow(mu), ncol(mu)),
         d_kappa = rep(0, nrow(mu)), labels = "constant")
  }
  B <- .paperps_dml_basis(fit)
  out <- sconjoint:::scmix_dml(
    fit, targets = character(), plugin_targets = list(constant = constant),
    mu_basis = B, nu_grid = 0,
    verification = .paperps_dml_verification(fit, B), multiplier_draws = 19L)
  expect_identical(out$status, "conditional_unverified")
  expect_match(out$reason, "no target has a positive finite standard error")
  expect_false(out$target_inference_available[["constant"]])

  mixed <- sconjoint:::scmix_dml(
    fit, targets = "theta", plugin_targets = list(constant = constant),
    mu_basis = B, nu_grid = 0,
    verification = .paperps_dml_verification(fit, B),
    information_eigenvalue_min = 1e-12, multiplier_draws = 19L)
  expect_identical(mixed$status, "conditional_available")
  expect_false(mixed$target_inference_available[["constant"]])
  expect_true(all(mixed$target_inference_available[grep("^theta", names(
    mixed$target_inference_available))]))
  expect_true(is.na(mixed$se[["constant"]]))
  expect_true(all(is.na(mixed$covariance["constant", ])))
  expect_identical(mixed$multiplier$status, "partially_withheld")
  expect_true("constant" %in% mixed$multiplier$withheld_targets)

  IF <- cbind(varying = seq(-1, 1, length.out = 20), constant = 0)
  got <- sconjoint:::.scmix_dml_multiplier(
    IF, estimate = c(varying = 0, constant = 1),
    se = c(varying = 0.1, constant = 0), R = 19L, level = 0.95,
    seed = 4L, multiplier = "normal")
  expect_identical(got$status, "partially_withheld")
  expect_true("constant" %in% got$withheld_targets)
  expect_true(is.na(got$simultaneous_lower[["constant"]]))
})

test_that("missing core fields fail clearly and fold preprocessing is honored", {
  fit <- .paperps_dml_mock(N = 30L)
  fit$kappa_folds <- NULL
  expect_error(sconjoint:::scmix_dml(fit), "fold-specific kappa")

  no_mu <- .paperps_dml_mock(N = 30L)
  no_mu$mu_all_folds <- NULL
  expect_error(sconjoint:::scmix_dml(no_mu), "all-fold mean predictions")

  Z <- cbind(z = c(1, 3))
  prep_fit <- list(z_preprocess_folds = list(
    list(center = 1, scale = 2), list(center = 2, scale = 4)
  ))
  got1 <- sconjoint:::.scmix_dml_fold_network_Z(prep_fit, Z, 1L, 2L)
  got2 <- sconjoint:::.scmix_dml_fold_network_Z(prep_fit, Z, 2L, 2L)
  expect_equal(as.numeric(got1), c(0, 1))
  expect_equal(as.numeric(got2), c(-0.25, 0.25))
})
