test_that("v2.3 mean MRS and target gate bindings are exact", {
  plan <- scmix_v23_calibration_plan()
  mrs <- plan$targets[plan$targets$target_id == "mean_tradeoff_mrs_ratio", ]
  expect_identical(mrs$variant_id, "negative_s_over_d")
  expect_match(mrs$truth_definition, "^-E_i\\(.*\\)/E_i\\(.*\\)$")
  expect_identical(
    sconjoint:::.scmix_v23_oracle_variants[["mean_tradeoff_mrs_ratio"]],
    "negative_s_over_d")

  map <- sconjoint:::.scmix_v23_oracle_gate_map
  expect_true(all(vapply(map, function(x)
    "active_eigenvalue_standardized_cutoff" %in% x, logical(1L))))
  expect_true("chart_discrepancy_cutoff" %in% map$probability)
  expect_false("nonlinear_remainder_cutoff" %in% map$probability)
  expect_true("nonlinear_remainder_cutoff" %in% map$subgroup_share)
  expect_false("chart_discrepancy_cutoff" %in% map$subgroup_share)
  for (id in c("preference_direction_share", "compensating_benefit_share")) {
    expect_true("interior_distance_cutoff" %in% map[[id]])
    expect_false(any(c("directional_variance_standardized_cutoff",
                       "total_heterogeneity_standardized_cutoff") %in%
                     map[[id]]))
  }
  expect_true(all(c(
    "directional_heterogeneity_regularity_margin",
    "directional_variance_standardized_cutoff",
    "total_heterogeneity_standardized_cutoff") %in%
      map$directional_heterogeneity))
})

test_that("active eigenvalue and directional residual statistics are distinct", {
  active <- sconjoint:::.scmix_v23_oracle_active_eigen_statistic(.05, .04)
  first_order_residual <- .05^2 / (2 * abs(.05) * sqrt(.04))
  expect_gt(active[["standard_error"]], 0)
  expect_false(isTRUE(all.equal(
    active[["standardized"]], first_order_residual)))
  expect_equal(active[["eigenvalue"]], .05^2)
})

test_that("unsafe labels are target-specific and unresolved DGPs fail closed", {
  plan <- scmix_v23_calibration_plan()
  classify <- sconjoint:::.scmix_v23_oracle_gate_certification
  expect_identical(classify(
    "chart_discrepancy_cutoff", "probability",
    "probability_near_boundary", "unsafe", plan$constants), "unsafe")
  expect_identical(classify(
    "chart_discrepancy_cutoff", "preference_direction_share",
    "probability_near_boundary", "unsafe", plan$constants), "not_targeted")
  expect_identical(classify(
    "total_heterogeneity_standardized_cutoff", "directional_heterogeneity",
    "directional_variance_near_zero", "unsafe", plan$constants),
    "not_targeted")
  expect_identical(classify(
    "active_eigenvalue_standardized_cutoff", "probability",
    "directional_variance_near_zero", "unsafe", plan$constants), "unsafe")

  id <- "total_heterogeneity_standardized_cutoff"
  evidence <- list()
  evidence[[id]] <- data.frame(
    value = c(2, 1, .5), design_id = "br2017",
    regime_id = c("well_informed_interior",
                  "directional_variance_near_zero",
                  "directional_variance_near_zero"),
    severity_id = c("reference", "mild", "severe"),
    certification_truth = c("safe", "not_targeted", "not_targeted"),
    calibration_unit_id = "directional_heterogeneity",
    stringsAsFactors = FALSE)
  out <- sconjoint:::.scmix_v23_step3_select_group(id, evidence, plan)
  expect_false(out$feasible)
  expect_identical(out$reason, "no_target_specific_unsafe_evidence")
})

test_that("calibration requires every safe cell in every design", {
  plan <- scmix_v23_calibration_plan()
  id <- "chart_discrepancy_cutoff"
  evidence <- list()
  evidence[[id]] <- data.frame(
    value = c(rep(1, 80), rep(2, 20), rep(1, 100), rep(2, 100)),
    design_id = "br2017",
    regime_id = c(rep("well_informed_interior", 100),
                  rep("probability_near_boundary", 200)),
    severity_id = c(rep("reference", 100), rep("mild", 100),
                    rep("severe", 100)),
    certification_truth = c(rep("safe", 200), rep("unsafe", 100)),
    calibration_unit_id = "probability", stringsAsFactors = FALSE)
  out <- sconjoint:::.scmix_v23_step3_select_group(id, evidence, plan)
  expect_false(out$feasible)
  expect_equal(out$benign_reference_passage, .8)
  expect_equal(out$benign_targeted_safe_passage, 1)

  expect_identical(
    sconjoint:::.scmix_v23_step3_candidate_grid(
      c(-2, 0, 1, Inf), strictly_positive = TRUE), 1)
})

.v23_statistical_threshold_fixture <- function(plan) {
  comparator <- plan$constants$comparator[
    match(sconjoint:::.scmix_v23_oracle_names, plan$constants$constant)]
  stats::setNames(ifelse(comparator == "less_than_or_equal", 1, .1),
                  sconjoint:::.scmix_v23_oracle_names)
}

test_that("subgroup high and low variants release and cover jointly", {
  plan <- scmix_v23_calibration_plan()
  target_keys <- c("subgroup_share__z_high", "subgroup_share__z_low")
  variants <- c("z_positive_after_standardization",
                "z_nonpositive_after_standardization")
  targets <- data.frame(
    target_key = target_keys, target_id = "subgroup_share",
    variant_id = variants, truth = c(.6, .4), estimate = c(.6, .4),
    se = .05, lower = c(.5, .3), upper = c(.7, .5),
    covered = c(TRUE, FALSE), empty_constraint_set = FALSE,
    uninformative_constraint_set = FALSE,
    reported_interval_length = .2, oracle_benchmark_length = .196,
    interval_length_ratio_to_oracle = .2 / .196,
    design_id = "br2017", regime_id = "well_informed_interior",
    severity_id = "reference", certification_truth = "safe",
    target_certification_truth = "safe", cell_certification_truth = "safe",
    severity_role = "safe", replication = 1L, stringsAsFactors = FALSE)
  constants <- sconjoint:::.scmix_v23_oracle_gate_map$subgroup_share
  gates <- do.call(rbind, lapply(seq_along(target_keys), function(j) {
    comparator <- plan$constants$comparator[match(
      constants, plan$constants$constant)]
    value <- ifelse(comparator == "less_than_or_equal", .1, 1)
    if (j == 2L) value[constants == "nonlinear_remainder_cutoff"] <- 2
    data.frame(
      target_key = target_keys[[j]], target_id = "subgroup_share",
      variant_id = variants[[j]], constant = constants, value = value,
      calibration_unit_id = "subgroup_share_joint", metric_scope = "target",
      statistical_authority = "marginal_local_Gaussian_diagnostic_only",
      design_id = "br2017", regime_id = "well_informed_interior",
      severity_id = "reference", certification_truth = "safe",
      target_certification_truth = "safe", cell_certification_truth = "safe",
      severity_role = "safe", replication = 1L, stringsAsFactors = FALSE)
  }))
  run <- sconjoint:::.scmix_v23_step3_sign(list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    stream = "calibration", layer = "oracle_local_gaussian", pilot = TRUE,
    targets = targets, gates = gates, production_manifest_created = FALSE,
    statistical_engine_production_authority = FALSE),
    "scmix_v23_oracle_calibration_run")
  oc <- scmix_v23_oracle_operating_characteristics(
    run, .v23_statistical_threshold_fixture(plan), plan)
  expect_false(any(oc$targets$released))
  expect_false(any(oc$targets$coverage_assessment_event))
  expect_false(oc$production_promotion_permitted)
})

test_that("validation enforces release, Wilson, length, and empty-set rules", {
  plan <- scmix_v23_calibration_plan()
  n <- 2000L
  targets <- data.frame(
    design_id = "br2017", regime_id = "well_informed_interior",
    severity_id = "reference", target_id = "probability",
    variant_id = "all_respondents", target_key = "probability",
    replication = seq_len(n), target_certification_truth = "safe",
    coverage_assessment_event = seq_len(n) <= 1900L,
    released = TRUE, empty_assessment_event = FALSE,
    uninformative_assessment_event = FALSE, empty_constraint_set = FALSE,
    interval_length_ratio_to_oracle = 1, stringsAsFactors = FALSE)
  run <- list(stream = "validation", pilot = FALSE)
  assessed <- sconjoint:::.scmix_v23_oracle_validation_assessment(
    targets, run, plan)
  expect_true(assessed$all_statistical_criteria_pass)
  expect_true(all(assessed$criteria$passed))

  targets$released[1000:n] <- FALSE
  failed <- sconjoint:::.scmix_v23_oracle_validation_assessment(
    targets, run, plan)
  expect_false(failed$all_statistical_criteria_pass)
  expect_false(all(failed$criteria$passed))
  expect_true(any(failed$criteria$criterion ==
                    "well_informed_conditional_coverage" &
                  !failed$criteria$passed))
})
