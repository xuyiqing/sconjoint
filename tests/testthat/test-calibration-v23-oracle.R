.v23_oracle_fixture <- function(N = 48L, T = 4L, seed = 20260906L) {
  set.seed(seed)
  respondent <- rep(sprintf("r%03d", seq_len(N)), each = T)
  z <- rep(seq(-1.5, 1.5, length.out = N), each = T)
  X <- matrix(stats::rnorm(N * T * 5L), ncol = 5L,
              dimnames = list(NULL, paste0("x", seq_len(5L))))
  Z <- cbind(z = z, z2 = rep(c(-1, 1), length.out = N * T))
  list(
    design_id = "pilot", deltaX = X, Z = Z,
    respondent_id = respondent,
    task_id = rep(seq_len(T), N))
}

.v23_oracle_plan <- function() {
  constants <- data.frame(
    constant = sconjoint:::.scmix_v23_oracle_names,
    comparator = ifelse(
      sconjoint:::.scmix_v23_oracle_names %in% c(
        "chart_discrepancy_cutoff", "nonlinear_remainder_cutoff",
        "null_space_materiality_cutoff", "defect_cutoff", "ridge_cutoff"),
      "less_than_or_equal", "greater_than_or_equal"),
    primary_layer = "oracle",
    targeted_failure_regime = "well_informed_interior",
    stringsAsFactors = FALSE)
  dgp <- data.frame(
    regime_id = "well_informed_interior", intercept = 0, loading_sd = .55,
    mrs_denominator_intercept = .60, residual_distribution = "normal",
    serial_rho = 0, informative_completion = FALSE,
    completion_intercept = 0, completion_latent_coefficient = 0,
    completion_difficulty_coefficient = 0, weak_target_direction = FALSE,
    direction_intercept = .12, direction_moderator_slope = .22,
    numerator_intercept = .75, numerator_moderator_slope = .10,
    denominator_moderator_slope = .08, serial_innovation_sd = .35,
    oracle_model_nodes = 9, truth_normal_nodes = 31,
    truth_nonnormal_quantiles = 129, stringsAsFactors = FALSE)
  list(
    plan_id = "pilot-plan", protocol_id = "pilot-protocol", dgp = dgp,
    regimes = data.frame(
      regime_id = "well_informed_interior", targeted_failure = FALSE,
      stringsAsFactors = FALSE),
    severity = data.frame(
      regime_id = "well_informed_interior", severity_index = 1L,
      severity_id = "reference", certification_truth = "safe",
      intercept = 0, loading_sd = .55, mrs_denominator_intercept = .60,
      d_projection_weakest_weight = 0, residual_distribution = "normal",
      serial_rho = 0, serial_innovation_sd = 0,
      completion_intercept = 0, completion_latent_coefficient = 0,
      completion_difficulty_coefficient = 0, stringsAsFactors = FALSE),
    constants = constants,
    designs = data.frame(design_id = "pilot", stringsAsFactors = FALSE),
    operating_characteristics = data.frame(characteristic = c(
      "unconditional_coverage", "coverage_conditional_on_release",
      "false_certification_rate",
      "interval_length_natural_median", "interval_length_natural_p90",
      "interval_length_se_units_median", "interval_length_se_units_p90",
      "empty_constraint_set_rate", "uninformative_constraint_set_rate",
      "gate_passage_frequency", "gate_state_frequency",
      "first_failure_frequency"), stringsAsFactors = FALSE),
    counts = list(
      oracle_calibration_reps = 2000L, oracle_validation_reps = 2000L,
      oracle_calibration_seed = 100000000L,
      oracle_validation_seed = 500000000L, maximum_workers = 2L),
    rates = list(nominal_coverage = .95),
    raw = list(
      `Oracle-Calibration-Master-Seed` = "100000000",
      `Oracle-Validation-Master-Seed` = "500000000",
      `Maximum-Workers` = "2"))
}

test_that("v2.3 truths use only the response-free design and frozen DGP", {
  template <- .v23_oracle_fixture()
  plan <- .v23_oracle_plan()
  truth1 <- scmix_v23_oracle_truths(
    template, plan = plan, require_frozen = FALSE)
  truth2 <- scmix_v23_oracle_truths(
    template, plan = plan, require_frozen = FALSE)
  expect_identical(truth1, truth2)
  expect_setequal(unique(truth1$target_id),
                  sconjoint:::.scmix_v23_oracle_targets)
  expect_equal(nrow(truth1), 8L)
  expect_true(all(truth1$application_outcomes_used == FALSE))
  expect_true(all(truth1$truth[1:4] > 0 & truth1$truth[1:4] < 1))
})

test_that("v2.3 task generator creates persistent synthetic sequences", {
  template <- .v23_oracle_fixture()
  plan <- .v23_oracle_plan()
  first <- sconjoint:::.scmix_v23_oracle_generate(
    template, "well_informed_interior", 971L, plan,
    require_frozen = FALSE)
  second <- sconjoint:::.scmix_v23_oracle_generate(
    template, "well_informed_interior", 971L, plan,
    require_frozen = FALSE)
  expect_identical(first$y, second$y)
  expect_identical(first$latent_u, second$latent_u)
  expect_length(first$y, nrow(template$deltaX))
  expect_true(all(first$y %in% 0:1))
  expect_true(all(first$observed))
  expect_false(first$application_outcomes_used)
  expect_identical(first$generator, "task_level_q1_mixed_logit_v2.3")
})

test_that("frozen nonnormal grids are centered with unit variance", {
  base <- list(truth_nonnormal_quantiles = 4097L)
  for (residual in c(
      "student_t5_unit_variance", "centered_chisq3_unit_variance",
      "symmetric_bimodal_mu0p85_unit_variance")) {
    config <- c(base, list(residual = residual))
    grid <- sconjoint:::.scmix_v23_oracle_grid(config, truth = TRUE)
    expect_equal(sum(grid$w), 1, tolerance = 1e-12)
    expect_lt(abs(sum(grid$w * grid$u)), .02)
    expect_lt(abs(sum(grid$w * grid$u^2) - 1), .04)
  }
  bad <- c(base, list(residual = "unregistered_residual"))
  expect_error(sconjoint:::.scmix_v23_oracle_grid(bad, truth = TRUE),
               "No frozen truth grid")
})

test_that("serial copula and PPS completion preserve frozen invariants", {
  template <- .v23_oracle_fixture(N = 80L)
  plan <- .v23_oracle_plan()
  serial_row <- plan$dgp
  serial_row$regime_id <- "serial_dependence"
  completion_row <- plan$dgp
  completion_row$regime_id <- "informative_completion"
  completion_row$informative_completion <- TRUE
  plan$dgp <- rbind(plan$dgp, serial_row, completion_row)
  plan$severity <- rbind(
    plan$severity,
    transform(plan$severity, regime_id = "serial_dependence",
              severity_id = "severe", certification_truth = "unsafe",
              serial_rho = .7),
    transform(plan$severity, regime_id = "informative_completion",
              severity_id = "severe", certification_truth = "unsafe",
              completion_latent_coefficient = -1,
              completion_difficulty_coefficient = -.45))
  independent <- sconjoint:::.scmix_v23_oracle_generate(
    template, "well_informed_interior", 19L, plan,
    severity_id = "reference", require_frozen = FALSE)
  serial <- sconjoint:::.scmix_v23_oracle_generate(
    template, "serial_dependence", 19L, plan,
    severity_id = "severe", require_frozen = FALSE)
  expect_identical(independent$latent_u, serial$latent_u)
  expect_identical(independent$synthetic_probability,
                   serial$synthetic_probability)
  expect_true(all(is.finite(serial$serial_copula_normal)))
  expect_match(serial$serial_mechanism, "Gaussian_AR1_copula")

  completion <- sconjoint:::.scmix_v23_oracle_generate(
    template, "informative_completion", 23L, plan,
    severity_id = "severe", require_frozen = FALSE)
  original_histogram <- table(table(template$respondent_id))
  selected_histogram <- table(table(completion$respondent_id))
  expect_identical(unname(original_histogram), unname(selected_histogram))
  expect_match(completion$completion_mechanism, "PPS_with_replacement")
})

test_that("local Gaussian moments are design-derived and sequence-auditable", {
  template <- .v23_oracle_fixture(N = 64L)
  plan <- .v23_oracle_plan()
  problem <- sconjoint:::.scmix_v23_oracle_problem(
    template, "well_informed_interior", plan, require_frozen = FALSE)
  expect_true(all(is.finite(problem$H)))
  expect_true(all(is.finite(problem$B)))
  expect_equal(problem$H, t(problem$H), tolerance = 1e-10)
  expect_equal(problem$B, t(problem$B), tolerance = 1e-10)
  expect_gte(min(eigen(problem$B, symmetric = TRUE)$values), -1e-9)
  expect_identical(
    problem$engine,
    "respondent_clustered_marginal_composite_Godambe")

  generated <- sconjoint:::.scmix_v23_oracle_generate(
    template, "well_informed_interior", 222L, plan,
    require_frozen = FALSE)
  audit <- sconjoint:::.scmix_v23_oracle_sequence_step(problem, generated)
  expect_true(all(is.finite(audit$theta_hat)))
  expect_identical(audit$respondents_used, 64L)
  expect_identical(audit$task_rows_used, 64L * 4L)
  expect_false(audit$application_outcomes_used)
})

test_that("pilot oracle emits real metrics and all seven registered OCs", {
  template <- .v23_oracle_fixture(N = 56L)
  plan <- .v23_oracle_plan()
  run <- scmix_v23_run_oracle_calibration(
    template, regime_ids = "well_informed_interior", replications = 5L,
    plan = plan, workers = 1L, pilot = TRUE)
  expect_false(run$eligible_for_threshold_selection)
  expect_false(run$numerical_proxy_metrics_used)
  expect_true(all(sconjoint:::.scmix_v23_oracle_names %in%
                    names(run$replications)))
  expect_equal(nrow(run$targets), 5L * 8L)
  expect_true(all(run$targets$target_id %in%
                    sconjoint:::.scmix_v23_oracle_targets))
  expect_true(all(is.finite(run$gates$value)))

  upper <- plan$constants$comparator == "less_than_or_equal"
  threshold <- stats::setNames(rep(.Machine$double.eps, nrow(plan$constants)),
                              plan$constants$constant)
  threshold[upper] <- .Machine$double.xmax^0.1
  oc <- scmix_v23_oracle_operating_characteristics(run, threshold, plan)
  expect_setequal(unique(oc$summary$characteristic),
                  plan$operating_characteristics$characteristic)
  expect_true(all(oc$targets$released))
  expect_false(oc$thresholds_selected_here)
})
