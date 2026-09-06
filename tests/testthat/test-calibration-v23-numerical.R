v23_test_numerical_bundle <- function(
    plan, design_id, stream, base_replication, mode = "base") {
  non_f008 <- setdiff(
    sconjoint:::.scmix_v23_numerical_constants,
    c("near_optimum_objective_cutoff", "multistart_target_se_cutoff"))
  x <- list(
    plan_fingerprint = plan$fingerprint, design_id = design_id,
    stream = stream, run_mode = mode,
    base_replication = as.integer(base_replication),
    replay_cluster_id = paste(design_id, stream,
                              sprintf("base_%03d", base_replication),
                              sep = "::"),
    outcome_fingerprint = paste0("outcome_", design_id, "_", stream, "_",
                                 base_replication),
    non_f008_metrics = stats::setNames(rep(1, length(non_f008)),
                                       non_f008),
    f008_joint_surface = data.frame(
      objective_relative_gap = c(0, 1),
      target_change_over_se = c(0, 1)),
    multiplier_draws = if (identical(mode, "transport")) 9999L else 0L)
  sconjoint:::.scmix_v23_step3_sign(
    x, "scmix_v23_numerical_evidence_bundle")
}

test_that("v2.3 numerical contracts separate replay bases from transport", {
  plan <- scmix_v23_calibration_plan()
  x <- scmix_v23_numerical_contract(plan, "base")
  expect_true(sconjoint:::.scmix_v23_step3_intact(
    x, "scmix_v23_numerical_contract"))
  expect_identical(x$q, 1L)
  expect_identical(x$mode, "base")
  expect_identical(x$outer_folds, 2L)
  expect_identical(x$inner_folds, 2L)
  expect_identical(x$optimizer_starts, 2L)
  expect_identical(c(x$level_a_nodes, x$level_b_nodes), c(15L, 31L))
  expect_true(is.na(x$multiplier_draws_required))
  expect_identical(x$dtype, "float64")
  expect_identical(x$torch_threads, 1L)
  transport <- scmix_v23_numerical_contract(plan, "transport")
  expect_identical(transport$mode, "transport")
  expect_identical(transport$outer_folds, 5L)
  expect_identical(transport$inner_folds, 2L)
  expect_identical(transport$optimizer_starts, 3L)
  expect_identical(
    c(transport$level_a_nodes, transport$level_b_nodes), c(31L, 61L))
  expect_identical(transport$multiplier_draws_required, 9999L)
  expect_identical(transport$replications_per_design,
                   c(validation = 1L))
  expect_identical(x$q2_role,
                   "sensitivity_only_no_q1_manifest_authority")
  expect_false(x$creates_production_manifest)
  expect_identical(x$numerical_constants,
                   sconjoint:::.scmix_v23_numerical_constants)
})

test_that("generated outcomes and base fits fail closed when unsigned", {
  plan <- scmix_v23_calibration_plan()
  fake_design <- structure(list(response_free = TRUE),
                           class = c("scmix_v23_design_template", "list"))
  expect_error(
    sconjoint:::.scmix_v23_seal_generated_outcome(
      template = fake_design, generated = list(), truth_manifest = list(),
      stream = "calibration", base_replication = 1L, plan = plan),
    "intact response-free")
  expect_error(
    sconjoint:::.scmix_v23_validate_numerical_base_fit(
      list(q = 1L), tempdir(), plan, reload = FALSE),
    "absent, altered, or off contract")
})

test_that("missing adapters block design-matched outcome generation", {
  plan <- scmix_v23_calibration_plan()
  template <- scmix_v23_load_frozen_design("br2017")
  readiness <- scmix_v23_numerical_readiness(plan = plan)
  expect_true(sconjoint:::.scmix_v23_step3_intact(
    readiness, "scmix_v23_numerical_readiness"))
  expect_false(readiness$ready_for_outcome_generation)
  expect_false(readiness$ready_for_threshold_selection)
  expect_false(readiness$ready_for_production_promotion)
  expect_false(readiness$marginal_oracle_authorizes_production)
  expect_true(any(!readiness$primitive_status$available))
  expect_error(
    scmix_v23_seal_generated_outcome(
      template = template, generated = list(), truth_manifest = list(),
      stream = "calibration", base_replication = 1L, plan = plan),
    "outcome generation is disabled")
  artifact_dir <- tempfile("v23-numerical-not-ready-")
  expect_error(
    scmix_v23_run_numerical_base(
      template = template, truth_manifest = list(),
      stream = "calibration", base_replication = 1L,
      artifact_dir = artifact_dir, plan = plan),
    "outcome generation is disabled")
  expect_false(dir.exists(artifact_dir))
})

test_that("known-rank tolerance is bracketed by truth-labeled spectra", {
  matrices <- list(
    exact_rank_two = diag(c(4, 1, 0)),
    noisy_rank_one = diag(c(3, 1e-14, 0)))
  evidence <- sconjoint:::.scmix_v23_known_rank_evidence(
    matrices, truth_ranks = c(2L, 1L))
  expect_true(sconjoint:::.scmix_v23_step3_intact(
    evidence, "scmix_v23_known_rank_evidence"))
  expect_lt(evidence$null_ratio_maximum,
            evidence$numerical_rank_tolerance)
  expect_lt(evidence$numerical_rank_tolerance,
            evidence$active_ratio_minimum)
  expect_invisible(
    sconjoint:::.scmix_v23_validate_known_rank_evidence(evidence))
  tampered <- evidence
  tampered$numerical_rank_tolerance <- 1
  expect_error(
    sconjoint:::.scmix_v23_validate_known_rank_evidence(tampered),
    "invalid or unseparated")
})

test_that("incomplete numerical inputs cannot acquire selector class", {
  missing <- sconjoint:::.scmix_v23_numerical_missing_primitives()
  expect_identical(nrow(missing), 9L)
  expect_true(all(c("primitive", "expected_symbol", "available",
                    "package_status", "required_resolution") %in%
                  names(missing)))
  expect_false(inherits(missing, "scmix_v23_numerical_calibration_run"))
  expect_false(any(grepl("proxy", missing$required_resolution,
                         ignore.case = TRUE)))
})

test_that("numerical replay cannot change the base outcome", {
  plan <- scmix_v23_calibration_plan()
  make_bundle <- function(outcome, fingerprint) {
    x <- list(
      plan_fingerprint = plan$fingerprint, design_id = "br2017",
      stream = "validation", run_mode = "base", base_replication = 1L,
      replay_cluster_id = "br2017::validation::base_001",
      outcome_fingerprint = outcome,
      non_f008_metrics = stats::setNames(
        rep(1, 10L), setdiff(sconjoint:::.scmix_v23_numerical_constants,
                            c("near_optimum_objective_cutoff",
                              "multistart_target_se_cutoff"))),
      f008_joint_surface = data.frame(
        objective_relative_gap = c(0, 1),
        target_change_over_se = c(0, 1)))
    sconjoint:::.scmix_v23_step3_sign(
      x, "scmix_v23_numerical_evidence_bundle")
  }
  a <- make_bundle("one", "a")
  b <- make_bundle("two", "b")
  expect_error(
    sconjoint:::.scmix_v23_numerical_replay(
      a, b, "optimization_error", "severe", "kkt_cutoff", plan,
      mechanism = "truncated optimizer"),
    "exact signed base outcome")
})

test_that("ordinary layer keeps 2+2 bases and fault evidence clustered", {
  plan <- scmix_v23_calibration_plan()
  bundles <- unlist(lapply(plan$designs$design_id, function(design_id) {
    lapply(seq_len(2L), function(replication) {
      v23_test_numerical_bundle(
        plan, design_id, "validation", replication, "base")
    })
  }), recursive = FALSE)
  regimes <- unique(plan$constants$targeted_failure_regime[
    plan$constants$primary_layer == "numerical"])
  severity <- plan$severity[plan$severity$regime_id %in% regimes,
                            c("regime_id", "severity_id",
                              "certification_truth"), drop = FALSE]
  replays <- unlist(lapply(bundles, function(bundle) {
    lapply(seq_len(nrow(severity)), function(j) {
      constants <- plan$constants$constant[
        plan$constants$primary_layer == "numerical" &
          plan$constants$targeted_failure_regime == severity$regime_id[[j]]]
      x <- list(
        plan_fingerprint = plan$fingerprint,
        design_id = bundle$design_id, stream = bundle$stream,
        run_mode = "base", base_replication = bundle$base_replication,
        replay_cluster_id = bundle$replay_cluster_id,
        regime_id = severity$regime_id[[j]],
        severity_id = severity$severity_id[[j]],
        severity_role = severity$certification_truth[[j]],
        comparison = data.frame(
          constant = constants, reference = 1, fault = 2,
          fault_to_reference_ratio = 2, fault_is_worse = TRUE,
          stringsAsFactors = FALSE),
        independent_monte_carlo_trial = FALSE,
        Wilson_denominator_contribution = 0L)
      sconjoint:::.scmix_v23_step3_sign(
        x, "scmix_v23_numerical_replay")
    })
  }), recursive = FALSE)
  out <- scmix_v23_numerical_layer_run(
    bundles, replays, "validation", plan)
  expect_true(sconjoint:::.scmix_v23_step3_intact(
    out, "scmix_v23_numerical_layer_run"))
  expect_identical(out$independent_base_outcomes, 6L)
  expect_identical(out$replay_cluster_count, 6L)
  expect_identical(out$replay_Wilson_denominator, 0L)
  expect_true(out$complete_numerical_primary_coverage)
  expect_true(out$every_targeted_replay_worse)
  expect_true(out$numerical_completion_pass)
  expect_false(out$selector_eligible)
})

test_that("transport has one untouched exact-path bundle per design", {
  plan <- scmix_v23_calibration_plan()
  bundles <- lapply(plan$designs$design_id, function(design_id) {
    v23_test_numerical_bundle(plan, design_id, "transport", 1L,
                              "transport")
  })
  sentinels <- lapply(plan$designs$design_id, function(design_id) {
    sconjoint:::.scmix_v23_step3_sign(list(
      plan_fingerprint = plan$fingerprint, design_id = design_id, q = 2L,
      comparisons = c("GH5x5_to_GH9x9", "GH9x9_to_GH15x15"),
      gold = "GH25x25", sensitivity_only = TRUE,
      authorizes_q1_manifest = FALSE), "scmix_v23_q2_sentinel")
  })
  out <- scmix_v23_transport_validation(bundles, sentinels, plan)
  expect_true(sconjoint:::.scmix_v23_step3_intact(
    out, "scmix_v23_transport_validation"))
  expect_identical(out$independent_validation_bases, 3L)
  expect_true(out$complete_numerical_primary_coverage)
  expect_true(out$transport_completion_pass)
  expect_true(out$q2_sensitivity_only)
  expect_false(out$selector_eligible)
  expect_false(out$creates_production_manifest)
})
