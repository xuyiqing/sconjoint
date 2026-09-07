test_that("v2.4 freezes one lean validation design without calibration", {
  plan <- scmix_v24_validation_plan()
  expect_s3_class(plan, "scmix_v24_validation_plan")
  expect_true(sconjoint:::.scmix_v24_intact(
    plan, "scmix_v24_validation_plan"))
  expect_identical(plan$protocol_id,
                   "paperps-inference-2026-09-06-v2.4")
  expect_identical(plan$plan_id,
                   "paperps-lean-validation-2026-09-06-v2.4-v1")
  expect_identical(plan$status,
                   "prospective_validation_design_frozen_outcomes_locked")
  expect_identical(plan$threshold_selection, "none")
  expect_identical(plan$calibration_stream, "none")
  expect_false(plan$application_outcomes_permitted)
  expect_identical(plan$raw[["Pre-Outcome-Amended-On"]], "2026-09-07")
  expect_match(plan$raw[["Pre-Outcome-Amendment"]],
               "directional_heterogeneity_primitive_metadata", fixed = TRUE)
  expect_identical(plan$freeze$fields[["Integrity-Pilot-Status"]],
                   "startup_failed_zero_completed_or_persisted_outcomes")
  expect_identical(plan$freeze$fields[["Evaluator-Approval-Status"]],
                   "v1_superseded_v2_absent")
  expect_identical(plan$freeze$fields[["Outcome-Count"]], "0")
  expect_identical(unname(unlist(plan$counts)),
                   c(400L, 2L, 100L, 10L, 2L, 2L, 2L, 15L, 31L, 9999L,
                     6L, 8L))
  expect_identical(plan$cells$cell_id, c(
    "br_regular", "gs_regular", "sw_regular",
    "br_probability_zero_denominator_boundary", "br_q0_rank_boundary",
    "gs_weak_inverse"))
  expect_true(all(plan$cells$replications == 100L))
  expect_true(all(plan$cells$fit_rank == 1L))
  expect_identical(plan$targets$family_id, c(
    "smooth_means", "choice_probabilities", "directional_shares",
    "subgroup_ratios", "directional_heterogeneity", "mrs"))
  expect_identical(
    plan$cells$exact_boundary_families[
      plan$cells$cell_id ==
        "br_probability_zero_denominator_boundary"], "mrs")
  expect_identical(
    plan$cells$weak_or_bounded_families[
      plan$cells$cell_id ==
        "br_probability_zero_denominator_boundary"],
    "choice_probabilities|subgroup_ratios")
  expect_identical(
    plan$cells$exact_boundary_families[
      plan$cells$cell_id == "br_q0_rank_boundary"], "all")
  expect_identical(
    plan$cells$weak_or_bounded_families[
      plan$cells$cell_id == "gs_weak_inverse"],
    "smooth_means|directional_shares|directional_heterogeneity")
  expect_equal(nrow(plan$approval), 36L)
  expect_equal(sum(plan$approval$required), 30L)
  expect_equal(sum(!plan$approval$required), 6L)
  expect_true(all(plan$approval$criterion[!plan$approval$required] ==
                    "descriptive_only"))
  expect_true(all(plan$approval$release_definition ==
                    plan$raw[["Family-Release-Definition"]]))
  expect_true(all(grepl("no_incomplete_or_bonferroni_fallback",
                        plan$approval$release_definition, fixed = TRUE)))
  expect_equal(nrow(plan$truths), 78L)
  expect_true(all(plan$truths$application_outcomes_used == FALSE))
  expect_identical(plan$fit_regularity$primitive_id,
                   "active_q1_covariance_eigenvalue")
  expect_true(all(c("yaml", "readme", "source_mirror",
                    "evaluator_source_mirror") %in%
                    names(plan$freeze$companion_sha256)))
  expect_true(setequal(
    names(plan$freeze$companion_sha256),
    c("yaml", "readme", "source_mirror", "evaluator_source_mirror")) ||
      setequal(
        names(plan$freeze$companion_sha256),
        c("protocol_markdown", "yaml", "readme", "source_mirror",
          "evaluator_source_mirror")))
  expect_identical(plan$raw[["Target-Truths-SHA256"]],
                   "f97dfca2a47e6b1d8f2b4c232ce61c5417776a7684bd70d819ceb92c522800a8")
})

test_that("v2.4 fixed rules have their prespecified values", {
  plan <- scmix_v24_validation_plan()
  value <- setNames(plan$decisions$value, plan$decisions$rule_id)
  expect_identical(value[["regularity_z"]], "1.96")
  expect_identical(value[["chart_nonlinear_discrepancy"]], "0.10")
  expect_identical(value[["riesz_defect"]], "0.20")
  expect_identical(value[["ridge_movement"]], "0.20")
  expect_identical(value[["block_kkt"]], "0.00001")
  expect_identical(value[["nested_objective"]], "0.000001")
  expect_identical(value[["near_optimum_target_spread"]], "0.10")
  expect_identical(value[["integration_target"]], "0.05")
  expect_identical(value[["integration_if"]], "0.05")
  expect_identical(value[["loading_invariance"]], "0.00000001")
  expect_identical(value[["derivative"]], "0.0001")
  expect_identical(value[["null_space"]], "0.000001")
  expect_identical(value[["all_cell_completion"]], "0.95")
  expect_identical(value[["regular_release"]], "0.75")
  expect_identical(value[["regular_coverage"]], "0.90")
  expect_identical(value[["boundary_false_release"]],
                   "point_at_most_0.05_and_upper_at_most_0.10")
  expect_identical(value[["weak_release_low"]], "5")
  expect_identical(value[["weak_release_high"]], "30")
  expect_identical(value[["pilot_runtime"]], "8")
  expect_identical(value[["coverage_mc_uncertainty"]],
                   "z_equals_qnorm_0.95_no_continuity_correction")
  expect_match(
    plan$decisions$metric[plan$decisions$rule_id == "regularity_z"],
    "target_primitive_constraints_and_active_q1_covariance_eigenvalue",
    fixed = TRUE)
  expect_identical(plan$raw[["Ridge-Rule"]], "Ntrain^(-3/4)")
  expect_identical(plan$raw[["Ridge-Sensitivity"]],
                   "Ntrain^(-1),Ntrain^(-1/2)")
  expect_identical(plan$raw[["Ridge-Selection"]], "none")
  expect_identical(plan$raw[["Familywise-Confidence-Level"]], "0.95")
  expect_match(plan$raw[["Multiplier-Critical-Value"]],
               "type1_quantile_at_0.95", fixed = TRUE)
  expect_match(plan$raw[["Boundary-Exact-Upper-Rule"]],
               "Clopper_Pearson_qbeta_0.95", fixed = TRUE)
  expect_match(plan$raw[["Respondent-Superpopulation-Resampling"]],
               "complete_respondent_records_with_replacement", fixed = TRUE)
  expect_match(plan$raw[["Family-Approval-Rule"]],
               "no_discretionary_override", fixed = TRUE)
  expect_match(plan$raw[["Family-Release-Definition"]],
               "complete_family_simultaneous_interval", fixed = TRUE)
  expect_match(plan$raw[["Incomplete-Family-Result"]],
               "not_a_family_release", fixed = TRUE)
})

test_that("v2.4 freezes exact target definitions and reference weights", {
  plan <- scmix_v24_validation_plan()
  expect_true(all(c("target_definition", "contrast_rule",
                    "reference_weight_rule", "primitive_rule") %in%
                  names(plan$targets)))
  expect_true(all(nzchar(plan$targets$target_definition)))
  expect_true(all(nzchar(plan$targets$contrast_rule)))
  expect_true(all(grepl("frozen_original_template",
                        plan$targets$reference_weight_rule, fixed = TRUE)))
  expect_match(
    plan$targets$target_definition[
      plan$targets$family_id == "directional_heterogeneity"],
    "denominator_N", fixed = TRUE)
  expect_identical(
    plan$targets$primitive_rule[
      plan$targets$family_id == "directional_heterogeneity"],
    paste0("joint_H_Z_and_H_R_with_directional_mean_and_second_moment_",
           "retained_as_supporting_audit_columns"))
  expect_match(
    plan$targets$contrast_rule[plan$targets$family_id == "subgroup_ratios"],
    "z_i_greater_than_0", fixed = TRUE)
})

test_that("v2.4 subsets are exact hashed response-free templates", {
  plan <- scmix_v24_validation_plan()
  expected_rows <- c(br2017 = 3200L, gs2020 = 5080L, sw2022 = 1200L)
  for (design_id in plan$designs$design_id) {
    template <- scmix_v24_load_design(design_id, plan)
    expect_s3_class(template, "scmix_v24_design_template")
    expect_true(sconjoint:::.scmix_v24_intact(
      template, "scmix_v24_design_template"))
    expect_true(template$response_free)
    expect_true(template$manifest_verified)
    expect_length(template$outcome_fields_stored, 0L)
    expect_equal(length(unique(template$respondent_id)), 400L)
    expect_equal(nrow(template$deltaX), expected_rows[[design_id]])
    expect_equal(template$selection$high_count +
                   template$selection$low_count, 400L)
    expect_true(all(template$selection$stratum_allocation >= 1L))
    expect_false(any(c("y", "choice", "outcome", "response") %in%
                       names(template)))
    again <- scmix_v24_load_design(design_id, plan)
    expect_identical(template$selection$selected_ids,
                     again$selection$selected_ids)
    expect_identical(template$subset_payload_hash,
                     again$subset_payload_hash)
  }
})

test_that("v2.4 DGP lies exactly in the uncentered fitted model", {
  plan <- scmix_v24_validation_plan()
  for (cell_id in plan$cells$cell_id) {
    geometry <- scmix_v24_dgp_geometry(cell_id, plan)
    expect_s3_class(geometry, "scmix_v24_dgp_geometry")
    expect_true(sconjoint:::.scmix_v24_intact(
      geometry, "scmix_v24_dgp_geometry"))
    expect_identical(geometry$centering, "none")
    expect_false(geometry$application_outcomes_used)
    expect_lt(geometry$scale_invariance_error, 1e-12)
    expect_equal(unname(geometry$duality), diag(3L), tolerance = 1e-10)
    expect_true(all(is.finite(geometry$fixed_index)))
    expect_true(all(is.finite(geometry$random_score)))
    if (!identical(cell_id, "gs_weak_inverse")) {
      expect_equal(geometry$direction_score_rms, 1, tolerance = 1e-10)
    }
  }
  boundary <- scmix_v24_dgp_geometry("br_q0_rank_boundary", plan)
  expect_identical(boundary$dgp_rank, 0L)
  expect_equal(unname(boundary$loading_raw),
               rep(0, length(boundary$loading_raw)))
  denominator <- scmix_v24_dgp_geometry(
    "br_probability_zero_denominator_boundary", plan)
  expect_equal(drop(denominator$target_dual_raw[, "denominator"] %*%
                      denominator$beta0_raw), 0, tolerance = 1e-12)
})

test_that("weak v2.4 score retains the smallest design eigenvalue", {
  weak <- scmix_v24_dgp_geometry("gs_weak_inverse")
  j <- weak$pc_index[["direction"]]
  expect_identical(j, length(weak$eigenvalues))
  expect_equal(weak$direction_score_rms,
               sqrt(weak$eigenvalues[[j]]), tolerance = 1e-10)
  expect_equal(weak$direction_score_rms,
               weak$expected_direction_score_rms, tolerance = 1e-12)
  expect_gt(abs(weak$direction_score_rms - 1), 1e-3)
  expected_raw <- weak$eigenvectors[, j] /
    weak$respondent_weighted_rms
  expect_equal(unname(weak$coefficient_basis_raw[, "direction"]),
               unname(expected_raw), tolerance = 1e-12)
  expect_equal(drop(weak$target_dual_raw[, "direction"] %*%
                      weak$coefficient_basis_raw[, "direction"]),
               1, tolerance = 1e-12)
})

test_that("v2.4 response-free truths reproduce their frozen table", {
  plan <- scmix_v24_validation_plan()
  observed <- scmix_v24_target_truths(plan)
  frozen <- plan$truths
  expect_identical(observed$cell_id, frozen$cell_id)
  expect_identical(observed$target_id, frozen$target_id)
  expect_identical(observed$status, frozen$status)
  expect_identical(observed$validation_class, frozen$validation_class)
  expect_identical(observed$application_outcomes_used,
                   frozen$application_outcomes_used)
  expect_equal(observed$truth, frozen$truth, tolerance = 5e-15)
  expect_identical(
    frozen$status[frozen$cell_id ==
      "br_probability_zero_denominator_boundary" &
        frozen$target_id == "mrs"], "undefined_exact_boundary")
  expect_equal(
    frozen$truth[frozen$cell_id == "br_q0_rank_boundary" &
                   frozen$target_id ==
                     "active_q1_covariance_eigenvalue"], 0)
})

test_that("v2.4 is fail closed after the v1 launcher is superseded", {
  plan <- scmix_v24_validation_plan()
  readiness <- scmix_v24_readiness(plan)
  expect_s3_class(readiness, "scmix_v24_readiness")
  expect_true(readiness$single_end_to_end_evaluator_required)
  expect_true(readiness$respondent_superpopulation_resampling_frozen)
  expect_true(readiness$deterministic_family_cell_approval_frozen)
  expect_true(readiness$evaluator_approval_present)
  expect_false(readiness$evaluator_approval_valid)
  expect_identical(readiness$status, "invalid_evaluator_state")
  expect_match(readiness$state_errors[["evaluator"]],
               "malformed or not pre-outcome", fixed = TRUE)
  expect_false(readiness$outcome_generation_permitted)
  expect_false(readiness$integrity_pilot_completed)
  expect_false(readiness$evidence_study_completed)
  expect_false(readiness$family_approval_present)
  expect_error(scmix_v24_generate_study_outcome(), "outcome locked")
  protocol_dir <- dirname(plan$files[["plan"]])
  superseded <- file.path(
    protocol_dir, "inference-v2.4-evaluator-approval.dcf")
  expect_true(file.exists(superseded))
  expect_identical(
    unname(read.dcf(superseded)[1L, "Approval-Id"]),
    "paperps-v2.4-evaluator-approval-v1")
  expect_false(file.exists(file.path(
    protocol_dir, "inference-v2.4-calibration-values.dcf")))
  expect_false(file.exists(file.path(
    protocol_dir, "inference-v2.4-family-approval.dcf")))
})

test_that("v2.3 protocol files remain immutable audit history", {
  plan <- scmix_v24_validation_plan()
  source_root <- normalizePath(file.path(
    dirname(plan$files[["plan"]]), "..", ".."), mustWork = TRUE)
  md <- file.path(source_root, "INFERENCE_PROTOCOL_V23.md")
  yaml <- file.path(source_root, "inst", "protocol",
                    "inference-v2.3.yaml")
  skip_if_not(file.exists(md) && file.exists(yaml),
              "top-level source protocol files are not installed")
  expect_identical(
    unname(as.character(tools::sha256sum(md))),
    "be66f6b126b537a39a199e7cd8f3bec9b2732ae2b105d1a1fd22a03d71c69b99")
  expect_identical(
    unname(as.character(tools::sha256sum(yaml))),
    "4e19dad898a84920b6dee4c670457edeece86ec51e2ea7d214a1b54d1f5836d2")
})
