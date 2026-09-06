test_that("v2.3 Step-3 plan is complete, response-free, and prospective", {
  plan <- scmix_v23_calibration_plan()
  expect_s3_class(plan, "scmix_v23_calibration_plan")
  expect_identical(plan$protocol_id,
                   "paperps-inference-2026-09-06-v2.3")
  expect_identical(plan$plan_id,
                   "paperps-step3-calibration-2026-09-06-v2.3-v1")
  expect_false(plan$application_outcomes_permitted)
  expect_false(plan$creates_production_manifest)
  expect_identical(nrow(plan$constants), 26L)
  expect_identical(nrow(plan$regimes), 9L)
  expect_identical(nrow(plan$targets), 7L)
  expect_identical(nrow(plan$operating_characteristics), 12L)
  expect_true(all(plan$regimes$regime_id %in% plan$severity$regime_id))
  expect_identical(
    plan$operating_characteristics$characteristic,
    c("unconditional_coverage", "coverage_conditional_on_release",
      "false_certification_rate", "interval_length_natural_median",
      "interval_length_natural_p90", "interval_length_se_units_median",
      "interval_length_se_units_p90", "empty_constraint_set_rate",
      "uninformative_constraint_set_rate", "gate_passage_frequency",
      "gate_state_frequency", "first_failure_frequency"))
  expect_match(
    plan$operating_characteristics$denominator[
      plan$operating_characteristics$characteristic ==
        "false_certification_rate"], "unsafe_severity", fixed = TRUE)
  expect_identical(plan$counts[["maximum_workers"]], 6L)
  expect_identical(plan$counts[["torch_threads_per_worker"]], 1L)
  expect_identical(plan$counts[["blas_threads_per_worker"]], 1L)
  expect_identical(plan$counts[["transport_validation_reps"]], 1L)
  expect_identical(
    plan$counts[["minimum_released_for_conditional_coverage"]], 1000L)
  expect_equal(plan$rates[["maximum_interval_median_ratio_to_oracle"]],
               1.25)
  expect_equal(plan$rates[["maximum_interval_p90_ratio_to_oracle"]], 2)
  expect_match(plan$raw[["G006-Compound-Rule"]],
               "own_independently_calibrated_cutoff", fixed = TRUE)
  expect_match(plan$raw[["Boundary-Alias-Policy"]],
               "no_cross_constant_aliases", fixed = TRUE)
  expect_identical(plan$raw[["Production-Rank-Scope"]], "q1_only")
  expect_match(plan$raw[["Q2-Integration-Sentinels"]],
               "GH5_squared_to_GH9_squared", fixed = TRUE)
  expect_true(all(grepl("^[[:xdigit:]]{64}$",
                        plan$designs$response_free_payload_hash)))
  expect_true(all(grepl("^[[:xdigit:]]{64}$",
                        plan$designs$source_file_sha256)))
  expect_true(all(grepl("^[[:xdigit:]]{64}$",
                        plan$designs$template_file_sha256)))
  expect_identical(plan$designs$ti_histogram,
                   c("8:2000",
                     paste0("1:1;2:1;4:2;5:1;6:2;7:3;8:4;9:10;",
                            "10:6;11:12;12:25;13:1538"),
                     "3:1191"))
  expect_identical(plan$raw[["Candidate-Grid"]],
                   paste0("observed_finite_metric_values_only_with_the_",
                          "frozen_inclusive_comparator"))
  expect_match(plan$raw[["Validation-Interval-Efficiency-Criterion"]],
               "ideal_unconstrained_linear_Wald_benchmark", fixed = TRUE)
})

test_that("subgroup target retains the frozen a-over-b composite contract", {
  plan <- scmix_v23_calibration_plan()
  row <- plan$targets[plan$targets$target_id == "subgroup_share", ,
                      drop = FALSE]
  expect_identical(row$target_family_id, "subgroup_ratio_family")
  expect_identical(
    row$primitive_registry_ids,
    paste(c("subgroup_z_high_numerator_a",
            "subgroup_z_high_denominator_b",
            "subgroup_z_low_numerator_a",
            "subgroup_z_low_denominator_b"), collapse = ";"))
  expect_match(row$truth_definition, "target=a_B/b_B", fixed = TRUE)
  expect_identical(row$primitive_constraints,
                   "for_each_variant_0<a_B<b_B<1")
  expect_identical(row$chart_coordinates,
                   "for_each_variant_[logit(a_B/b_B);logit(b_B)]")
  expect_match(row$g006_boundary_components_and_standardization,
               "D_B=min(a_B;1-b_B;b_B-a_B)", fixed = TRUE)
  expect_match(row$g006_boundary_components_and_standardization,
               "raw_probability_scale_with_no_SE_standardization",
               fixed = TRUE)
})

test_that("committed design wrappers are hashed response-free templates", {
  plan <- scmix_v23_calibration_plan()
  for (i in seq_len(nrow(plan$designs))) {
    template <- scmix_v23_load_frozen_design(plan$designs$design_id[[i]])
    expect_s3_class(template, "scmix_v23_design_template")
    expect_true(template$response_free)
    expect_true(template$manifest_verified)
    expect_length(template$outcome_fields_stored, 0L)
    expect_identical(template$ti_histogram,
                     plan$designs$ti_histogram[[i]])
    expect_false(any(c("y", "choice", "outcome", "response") %in%
                       names(template)))
  }
})

test_that("severity grid and mechanisms are frozen and executable", {
  plan <- scmix_v23_calibration_plan()
  targeted <- plan$regimes$regime_id[
    tolower(plan$regimes$targeted_failure) == "true"]
  for (id in targeted) {
    expect_identical(
      plan$severity$certification_truth[plan$severity$regime_id == id],
      c("safe", "transition", "unsafe"))
  }
  expect_identical(
    plan$severity$residual_distribution[
      plan$severity$regime_id == "residual_shape_misspecification"],
    c("student_t5_unit_variance", "centered_chisq3_unit_variance",
      "symmetric_bimodal_mu0p85_unit_variance"))
  expect_true(all(plan$severity$serial_dependence_mechanism[
    plan$severity$regime_id == "serial_dependence"] ==
      "stationary_Gaussian_AR1_copula_task_choices"))
  expect_true(all(plan$severity$completion_sampling_mechanism[
    plan$severity$regime_id == "informative_completion"] ==
      "duplicated_finite_population_PPS_with_replacement_within_exact_Ti_strata"))
  expect_identical(
    plan$constants$targeted_failure_regime[
      plan$constants$constant == "subgroup_interior_margin"],
    "probability_near_boundary")
})

test_that("severity-aware seed streams do not collide", {
  plan <- scmix_v23_calibration_plan()
  seeds <- unlist(lapply(seq_len(3L), function(d)
    lapply(seq_len(9L), function(r)
      vapply(seq_len(3L), function(s)
        sconjoint:::.scmix_v23_step3_seed(
          plan, "oracle", "calibration", d, r, s, 2000L),
        integer(1L)))), use.names = FALSE)
  expect_identical(anyDuplicated(seeds), 0L)
  expect_true(all(seeds > 0L & seeds <= .Machine$integer.max))
})

test_that("response-free template rejects outcome-like input and tampering", {
  deltaX <- cbind(attribute_a = c(-1, 1, 0, 1),
                  attribute_b = c(0, 1, -1, 0))
  Z <- cbind(moderator = c(-1, -1, 1, 1))
  id <- c("a", "a", "b", "b")
  task <- c("1", "2", "1", "2")
  template <- scmix_v23_design_template(
    deltaX, Z, id, task, design_id = "test_fixture",
    verify_manifest = FALSE)
  expect_s3_class(template, "scmix_v23_design_template")
  expect_true(template$response_free)
  expect_length(template$outcome_fields_stored, 0L)
  expect_false(any(c("y", "choice", "outcome") %in% names(template)))
  expect_error(scmix_v23_design_template(
    cbind(y = 0:3, deltaX), Z, id, task,
    design_id = "test_fixture", verify_manifest = FALSE),
    "Outcome-like")
  damaged <- template
  damaged$deltaX[1, 1] <- damaged$deltaX[1, 1] + 1
  expect_error(sconjoint:::.scmix_v23_step3_check_design(
    damaged, require_frozen = FALSE), "intact response-free")
})

test_that("frozen importer excludes source outcomes by construction", {
  extractor <- sconjoint:::.scmix_v23_step3_asset_file(
    "calibration", "v23", "extract_response_free_design.R")
  env <- new.env(parent = baseenv())
  sys.source(extractor, envir = env)
  source <- list(
    schema_version = "toy-v1",
    deltaX = cbind(a = c(-1, 1)), Z_primary = cbind(z = c(0, 0)),
    respondent_id = c("r", "r"), task = 1:2, y = c(0, 1))
  source_path <- tempfile(fileext = ".rds")
  whitelist_path <- tempfile(fileext = ".dcf")
  saveRDS(source, source_path, version = 3)
  payload <- list(
    deltaX = unname(as.matrix(source$deltaX)),
    deltaX_names = colnames(source$deltaX),
    Z = unname(as.matrix(source$Z_primary)),
    Z_names = colnames(source$Z_primary),
    respondent_id = as.character(source$respondent_id),
    task_id = as.character(source$task), position = NULL)
  payload_path <- tempfile(fileext = ".rds")
  saveRDS(payload, payload_path, version = 3, compress = FALSE)
  fields <- c(
    "Design-Id: toy", "Protocol-Id: paperps-inference-2026-09-06-v2.3",
    "Source-Schema: toy-v1",
    paste0("Source-SHA256: ", tools::sha256sum(source_path)),
    "Allowed-Source-Fields: deltaX,Z_primary,respondent_id,task",
    "Forbidden-Output-Fields: y,choice,chosen,outcome,response,selected,vote,candidate_vote,c_win",
    "DeltaX-Field: deltaX", "Z-Field: Z_primary",
    "Respondent-Field: respondent_id", "Task-Field: task",
    "Position-Field: none",
    paste0("Response-Free-Payload-SHA256: ",
           tools::sha256sum(payload_path)))
  writeLines(fields, whitelist_path)
  on.exit(unlink(c(source_path, whitelist_path, payload_path)), add = TRUE)
  out <- env$scmix_v23_extract_response_free_design(
    source_path, whitelist_path)
  expect_true(out$response_free)
  expect_length(out$outcome_fields_stored, 0L)
  expect_false("y" %in% names(out$payload))
})

test_that("G006 uses independent normalized components", {
  out <- sconjoint:::.scmix_v23_step3_g006_compound(
    c(subgroup = 0.20, denominator = 0.10),
    c(subgroup = 0.10, denominator = 0.20))
  expect_equal(out$ratio, c(subgroup = 2, denominator = 0.5))
  expect_equal(out$statistic, 0.5)
  expect_equal(out$threshold, 1)
  expect_false(out$pass)
})

test_that("candidate grid uses only observed finite inclusive cutoffs", {
  expect_identical(
    sconjoint:::.scmix_v23_step3_candidate_grid(
      c(2, NA_real_, 1, Inf, 2)), c(1, 2))
  expect_true(sconjoint:::.scmix_v23_step3_pass(
    2, 2, "less_than_or_equal"))
  expect_true(sconjoint:::.scmix_v23_step3_pass(
    2, 2, "greater_than_or_equal"))
})

test_that("threshold selection excludes transition rows from false certification", {
  plan <- scmix_v23_calibration_plan()
  id <- "chart_discrepancy_cutoff"
  designs <- plan$designs$design_id
  evidence <- list()
  evidence[[id]] <- do.call(rbind, lapply(designs, function(design_id)
    data.frame(
      value = c(1, 1, 0, 2), design_id = design_id,
      regime_id = c("well_informed_interior",
                    "probability_near_boundary",
                    "probability_near_boundary",
                    "probability_near_boundary"),
      severity_id = c("reference", "mild", "moderate", "severe"),
      certification_truth = c("safe", "safe", "transition", "unsafe"),
      stringsAsFactors = FALSE)))
  out <- sconjoint:::.scmix_v23_step3_select_group(id, evidence, plan)
  expect_true(out$feasible)
  expect_equal(out$threshold, 1)
  expect_equal(out$benign_passage, 1)
  expect_equal(out$false_certification, 0)
})

test_that("threshold constraints are enforced separately by design", {
  plan <- scmix_v23_calibration_plan()
  id <- "chart_discrepancy_cutoff"
  rows <- lapply(seq_along(plan$designs$design_id), function(i) {
    design_id <- plan$designs$design_id[[i]]
    unsafe <- rep(2, 100)
    if (i == 1L) unsafe[1:2] <- 0.5
    data.frame(
      value = c(1, 1, unsafe), design_id = design_id,
      regime_id = c("well_informed_interior",
                    "probability_near_boundary",
                    rep("probability_near_boundary", 100)),
      severity_id = c("reference", "mild", rep("severe", 100)),
      certification_truth = c("safe", "safe", rep("unsafe", 100)),
      stringsAsFactors = FALSE)
  })
  evidence <- list()
  evidence[[id]] <- do.call(rbind, rows)
  out <- sconjoint:::.scmix_v23_step3_select_group(id, evidence, plan)
  expect_false(out$feasible)
})

test_that("preliminary pseudo-metric simulator is fail-closed", {
  expect_error(
    sconjoint:::.scmix_v23_step3_metrics(NULL, NULL, NULL, NULL, NULL),
    "Disabled preliminary pseudo-metric generator")
  expect_error(
    sconjoint:::.scmix_v23_step3_deprecated_oracle_calibration(NULL),
    "preliminary summary-statistic oracle simulator is disabled")
})
