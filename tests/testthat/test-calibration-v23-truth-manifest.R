test_that("v2.3 target truths were frozen before synthetic outcomes", {
  plan <- scmix_v23_calibration_plan()
  truth <- scmix_v23_truth_manifest(plan)
  expect_s3_class(truth, "scmix_v23_truth_manifest")
  expect_true(sconjoint:::.scmix_v23_step3_intact(
    truth, "scmix_v23_truth_manifest"))
  expect_identical(truth$plan_id, plan$plan_id)
  expect_identical(truth$protocol_id, plan$protocol_id)
  expect_identical(plan$raw[["Mean-Tradeoff-Sign-Convention"]],
                   paste0("negative_mean_prespecified_numerator_divided_",
                          "by_mean_prespecified_denominator"))
  expect_false(truth$application_outcomes_used)
  expect_true(truth$no_frozen_design_outcomes_before_freeze)
  expect_identical(truth$toy_fixture_scope,
                   "outside_design_matched_step3_study")
  expect_false(truth$production_manifest_created)
  expect_identical(nrow(truth$target_truths), 600L)
  expect_identical(nrow(truth$design_diagnostics), 3L)
  expect_identical(truth$design_diagnostics$design_id,
                   c("br2017", "gs2020", "sw2022"))
  expect_true(all(grepl("^[[:xdigit:]]{64}$", truth$source_hashes)))
  expect_identical(
    unique(truth$target_truths$variant_id[
      truth$target_truths$target_id == "mean_tradeoff_mrs_ratio"]),
    "negative_s_over_d")
  expect_true(all(truth$target_truths$truth[
    truth$target_truths$target_id == "mean_tradeoff_mrs_ratio"] < 0))
})

test_that("frozen subgroup truths obey the respondent-equal a-over-b contract", {
  truth <- scmix_v23_truth_manifest()
  subgroup <- truth$target_truths[
    truth$target_truths$target_id == "subgroup_share", , drop = FALSE]
  expect_setequal(unique(subgroup$variant_id),
                  c("z_positive_after_standardization",
                    "z_nonpositive_after_standardization"))
  expect_true(all(subgroup$primitive_a_truth > 0))
  expect_true(all(subgroup$primitive_a_truth <
                    subgroup$primitive_b_truth))
  expect_true(all(subgroup$primitive_b_truth < 1))
  expect_equal(subgroup$truth,
               subgroup$primitive_a_truth / subgroup$primitive_b_truth,
               tolerance = 2e-15)
  split_sum <- aggregate(
    primitive_b_truth ~ design_id + regime_id + severity_id,
    data = subgroup, FUN = sum)
  expect_equal(split_sum$primitive_b_truth, rep(1, nrow(split_sum)),
               tolerance = 2e-15)
})

test_that("frozen design diagnostics retain the first varying moderator", {
  diagnostic <- scmix_v23_truth_manifest()$design_diagnostics
  expect_identical(diagnostic$subgroup_rule,
                   rep("z_positive_after_standardization_vs_z_nonpositive",
                       3L))
  positive <- as.integer(diagnostic$n_z_positive)
  nonpositive <- as.integer(diagnostic$n_z_nonpositive)
  respondents <- as.integer(diagnostic$n_respondents)
  expect_true(all(positive > 0L & nonpositive > 0L))
  expect_identical(positive + nonpositive, respondents)
  ## The first Saha-Weeks moderator is binary gender. The above-mean rule
  ## retains its two observed groups rather than substituting age.
  expect_identical(positive[diagnostic$design_id == "sw2022"], 597L)
  expect_identical(nonpositive[diagnostic$design_id == "sw2022"], 594L)
})

test_that("truth-manifest signatures detect in-memory tampering", {
  truth <- scmix_v23_truth_manifest()
  damaged <- truth
  damaged$target_truths$truth[[1L]] <-
    damaged$target_truths$truth[[1L]] + 0.01
  expect_false(sconjoint:::.scmix_v23_step3_intact(
    damaged, "scmix_v23_truth_manifest"))
})

test_that("installed audit copies match the frozen source files", {
  source_root <- Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = "")
  if (!nzchar(source_root)) {
    source_root <- normalizePath(
      file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  }
  source_names <- c(
    "calibration-v23-oracle.R",
    "calibration-v23-truth-manifest.R",
    "calibration-v23.R")
  for (source_name in source_names) {
    live <- file.path(source_root, "R", source_name)
    audit <- file.path(
      source_root, "inst", "calibration", "v23", "source", source_name)
    if (!file.exists(live) || !file.exists(audit)) {
      skip("Source-copy equality is checked in the package source tree.")
    }
    expect_identical(
      unname(as.character(tools::sha256sum(live))),
      unname(as.character(tools::sha256sum(audit))))
  }
})
