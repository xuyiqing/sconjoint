test_that("v2.8 equilibrated solve preserves the positive-definite ridge system", {
  set.seed(19)
  X <- matrix(rnorm(36), 12, 3)
  I <- crossprod(X) / nrow(X)
  P <- diag(c(1, 2, 4))
  g <- matrix(rnorm(6), 3, 2)
  nu <- 0.07
  expected <- solve(I + nu * P, g)
  actual <- sconjoint:::.scmix_dml_solve(I, g, nu, P)
  audit <- sconjoint:::.scmix_dml_solve_v28_audit(I, g, nu, P)
  expect_equal(actual, expected, tolerance = 1e-10)
  expect_equal(audit$alpha, expected, tolerance = 1e-10)
  expect_lt(max(audit$original_coordinate_relative_residual), 1e-10)
  expect_false(audit$changes_statistical_ridge_problem)
  expect_true(audit$congruent_equilibration_used)
})

test_that("v2.8 equilibration handles highly unequal coordinate scales", {
  ## The first eigenvalue is below the legacy cutoff only because the
  ## coordinates differ by more than ten orders of magnitude.  Equilibration
  ## must recover the strictly positive-definite zero-ridge solution rather
  ## than silently truncating that coordinate.
  I <- diag(c(1e-12, 1, 1e12))
  P <- diag(3)
  g <- matrix(c(1e-12, 2, 3e12), 3, 1)
  audit <- sconjoint:::.scmix_dml_solve_v28_audit(I, g, 0, P)
  expect_true(all(is.finite(audit$alpha)))
  expect_equal(as.numeric(audit$alpha), c(1, 2, 3), tolerance = 1e-10)
  expect_lt(audit$original_coordinate_relative_residual, 1e-9)
  expect_false(audit$original_coordinate_numerical_full_rank)
  expect_true(audit$scale_free_full_rank)
  expect_true(audit$congruent_equilibration_used)
  expect_lt(audit$componentwise_backward_error, 1e-10)
})

test_that("v2.8 never truncates a positive ridge with a PD penalty", {
  I <- matrix(c(1, 1 - 1e-12, 1 - 1e-12, 1), 2L, 2L)
  P <- diag(2)
  g <- matrix(c(1, -1), 2L, 1L)
  nu <- 1e-6
  expected <- solve(I + nu * P, g)
  audit <- sconjoint:::.scmix_dml_solve_v28_audit(I, g, nu, P)
  expect_true(audit$positive_ridge_pd_penalty)
  expect_true(audit$congruent_equilibration_used)
  expect_equal(audit$alpha, expected, tolerance = 1e-4)
  expect_lt(audit$componentwise_backward_error, 1e-10)
})

test_that("v2.8 withholds an uncertifiable ill-conditioned positive ridge", {
  Q <- matrix(c(1, 1, -1, 1), 2L, 2L) / sqrt(2)
  I <- Q %*% diag(c(1e12, 0)) %*% t(Q)
  P <- diag(2)
  nu <- 0.01
  g <- matrix(Q[, 2L], 2L, 1L)
  expect_error(
    sconjoint:::.scmix_dml_solve_v28_audit(I, g, nu, P),
    "scale-free ill-conditioned")
})

test_that("v2.8 preserves the reference representative at a singular zero ridge", {
  I <- diag(c(2, 0, 5))
  P <- diag(3)
  g <- matrix(c(4, 0, 10), 3, 1)
  expected <- c(2, 0, 2)
  actual <- sconjoint:::.scmix_dml_solve(I, g, 0, P)
  expect_equal(as.numeric(actual), expected, tolerance = 1e-14)
  audit <- sconjoint:::.scmix_dml_solve_v28_audit(I, g, 0, P)
  expect_lt(audit$original_coordinate_relative_residual, 1e-14)
  expect_false(audit$scale_free_full_rank)
  expect_false(audit$congruent_equilibration_used)
  expect_identical(
    audit$fallback_reason,
    paste0("scale_free_singular_system_uses_frozen_cutoff_numerical_",
           "pseudoinverse_representative"))
})

test_that("v2.8 Riesz artifacts retain the solver audit for every solve", {
  set.seed(91)
  score <- matrix(rnorm(160), 80L, 2L)
  gradient <- list(matrix(rnorm(160), 80L, 2L))
  out <- sconjoint:::.scmix_dml_riesz(
    score = score, gradient = gradient, train = 1:60,
    evaluation = 61:80, nu_grid = c(0, 60^(-3 / 4)),
    penalty = diag(2), structural_gram = diag(2),
    validation_fraction = 0.2, seed = 9L, solve_tolerance = 1e-10,
    numerical_rank_rule = "dimension_epsilon", N_total = 80L)
  audit <- out$v28_solver_audit
  expect_s3_class(audit, "data.frame")
  expect_true(all(c("training_validation_grid_1",
                    "training_validation_grid_2",
                    "selected_full_training",
                    "zero_reference_full_training") %in% audit$purpose))
  used <- audit$congruent_equilibration_used
  expect_true(all(is.finite(audit$componentwise_backward_error[used])))
  expect_true(all(audit$componentwise_backward_error[used] <= 1e-10))
})
