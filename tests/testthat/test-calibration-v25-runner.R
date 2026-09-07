.v25_runner_plan <- function() {
  families <- c("smooth_means", "choice_probabilities",
                "directional_shares", "subgroup_ratios",
                "directional_heterogeneity", "mrs")
  cells <- paste0("cell", seq_len(6L))
  list(
    protocol_id = "paperps-inference-2026-09-07-v2.5",
    plan_id = "paperps-lean-validation-2026-09-07-v2.5-v1",
    canonical = list(
      state_relative = "inst/protocol",
      run_relative =
        "runs/v25/step5c_integrity_and_evidence_2026-09-07"),
    cells = data.frame(cell_id = cells, design_id = "toy",
                       stringsAsFactors = FALSE),
    targets = data.frame(family_id = families,
                         stringsAsFactors = FALSE),
    counts = list(pilot_reps = 2L, evidence_reps = 50L,
                  batch_size = 10L, multipliers = 9999L,
                  maximum_projected_hours = 8L),
    approval = expand.grid(
      family_id = families, cell_id = cells,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
}

.v25_runner_hash <- function(letter) paste(rep(letter, 64L), collapse = "")

.v25_runner_evaluator <- function() list(
  sha256 = .v25_runner_hash("a"),
  settings = list(fingerprint = .v25_runner_hash("b")),
  raw = list(`Implementation-Manifest-SHA256` = .v25_runner_hash("c")))

.v25_dml_mock <- function(N = 24L, T = 2L, K = 2L, q = 1L) {
  p <- 2L
  respondent <- sprintf("r%03d", seq_len(N))
  respondent_id <- rep(respondent, each = T)
  respondent_index <- rep(seq_len(N), each = T)
  z <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
              dimnames = list(respondent, "z"))
  deltaX <- cbind(
    x1 = sin(seq_len(N * T) / 3),
    x2 = cos(seq_len(N * T) / 5))
  mu <- cbind(x1 = 0.2 + 0.15 * z[, 1L],
              x2 = -0.1 + 0.1 * z[, 1L])
  index <- 0.05 + rowSums(deltaX * mu[respondent_index, , drop = FALSE])
  y <- as.integer(stats::plogis(index) > rep(c(0.35, 0.65), length.out = N * T))
  fold_resp <- rep(seq_len(K), length.out = N)
  if (q == 1L) {
    A_hat <- matrix(c(0.3, -0.2), p, 1L)
    A_folds <- replicate(K, A_hat, simplify = FALSE)
    gh <- list(U = matrix(c(-1.22474487139159, 0, 1.22474487139159),
                          ncol = 1L),
               w = c(1 / 6, 2 / 3, 1 / 6))
  } else if (q == 0L) {
    A_hat <- matrix(numeric(), p, 0L)
    A_folds <- replicate(K, A_hat, simplify = FALSE)
    gh <- list(U = matrix(numeric(), 1L, 0L), w = 1)
  } else stop("The v2.5 DML mock supports q=0 or q=1.")
  list(
    deltaX = deltaX, y = y, respondent_id = respondent_id,
    fold_id = rep(fold_resp, each = T), K = K,
    Z = z[respondent_index, , drop = FALSE], q = as.integer(q),
    A_folds = A_folds, A_hat = A_hat, gh = gh,
    kappa_folds = rep(0.05, K),
    mu_all_folds = replicate(K, mu, simplify = FALSE),
    attr_names = colnames(deltaX), analysis_signature = "v25-fusion-mock")
}

.v25_runner_pilot_artifact <- function(
    run_dir, plan, evaluator, cell, replication, elapsed = 1) {
  payload <- sconjoint:::.scmix_v25_sign(list(
    protocol_id = plan$protocol_id, plan_id = plan$plan_id,
    stream = "pilot", cell_id = cell, replication = as.integer(replication),
    design_id = plan$cells$design_id[match(cell, plan$cells$cell_id)],
    evaluator_approval_sha256 = evaluator$sha256,
    evaluator_settings_fingerprint = evaluator$settings$fingerprint,
    implementation_manifest_sha256 =
      evaluator$raw[["Implementation-Manifest-SHA256"]],
    evaluation_elapsed_seconds = as.numeric(elapsed),
    synthetic_outcome = TRUE, application_outcomes_used = FALSE,
    completed = TRUE), "scmix_v25_replication_artifact")
  path <- sconjoint:::.scmix_v25_artifact_file(
    run_dir, list(stream = "pilot", cell_id = cell,
                  replication = replication))
  path <- sconjoint:::.scmix_v25_save_atomic(payload, path)
  list(
    artifact_file = path,
    artifact_sha256 = unname(as.character(tools::sha256sum(path))),
    elapsed_seconds = as.numeric(elapsed))
}

test_that("v2.5 raw-to-canonical primitive labels are not reversed", {
  labels <- c(
    "mean_direction", "mean_numerator", "mean_denominator",
    "choice_probability",
    "preference_direction_share:preference_direction_share",
    paste0("preference_direction_share:preference_direction_share:",
           "directional_variance"),
    "compensating_benefit_share:compensating_benefit_share",
    paste0("compensating_benefit_share:compensating_benefit_share:",
           "directional_variance"),
    "subgroup_high:choice_probability:numerator",
    "subgroup_high:choice_probability:denominator",
    "subgroup_low:choice_probability:numerator",
    "subgroup_low:choice_probability:denominator",
    "mrs:numerator", "mrs:denominator",
    "active_q1_covariance_eigenvalue")
  map <- sconjoint:::.scmix_v25_nonheterogeneity_map(labels)
  expect_identical(unname(map[[
    "subgroup_high:choice_probability:numerator"]]),
    "subgroup_high_numerator")
  expect_identical(unname(map[["mrs:denominator"]]), "mrs_denominator")
  expect_identical(unname(map[[
    "preference_direction_share:preference_direction_share"]]),
    "preference_direction_share")
  expect_identical(unname(map[[paste0(
    "preference_direction_share:preference_direction_share:",
    "directional_variance")]]), "preference_direction_variance")
  expect_true(all(names(map) %in% labels))
  expect_true(all(c("mean_direction", "active_q1_covariance_eigenvalue",
                    "subgroup_low_denominator",
                    "preference_direction_variance",
                    "compensating_benefit_variance") %in% unname(map)))
})

test_that("v2.5 heterogeneity Riesz primitives are exactly H_Z and H_R", {
  outcome <- list(
    deltaX = rbind(c(1, 0), c(0, 1)),
    deltaX_names = c("a", "b"), respondent_id = c("r1", "r2"),
    source_row_index = 1:2)
  geometry <- list(
    target_dual_raw = matrix(
      c(1, 0, 0, 1, 1, 1), nrow = 2L,
      dimnames = list(c("a", "b"),
                      c("direction", "numerator", "denominator"))),
    z_task = c(-1, 1))
  bundle <- sconjoint:::.scmix_v25_target_callbacks(
    outcome, geometry, n_nodes = 15L)
  expect_identical(bundle$heterogeneity$primitive_ids, c("H_Z", "H_R"))
  mu <- rbind(r1 = c(0.2, -0.1), r2 = c(-0.3, 0.4))
  colnames(mu) <- c("a", "b")
  share <- bundle$callbacks$preference_direction_share(
    mu, 0, diag(c(0.2, 0.1)), matrix(0, 2L, 1L), rownames(mu),
    1L, colnames(mu))
  expect_identical(
    share$labels,
    c("preference_direction_share",
      "preference_direction_share:directional_variance"))
  expect_identical(ncol(share$value), 2L)
})

test_that("v2.5 DML assembly maps raw labels to canonical result labels", {
  raw_by_analysis <- list(
    active_q1_covariance_eigenvalue = "active_q1_covariance_eigenvalue",
    mean_direction = "mean_direction",
    mean_numerator = "mean_numerator",
    mean_denominator = "mean_denominator",
    choice_probability = "choice_probability",
    preference_direction_share = c(
      "preference_direction_share:preference_direction_share",
      paste0("preference_direction_share:preference_direction_share:",
             "directional_variance")),
    compensating_benefit_share = c(
      "compensating_benefit_share:compensating_benefit_share",
      paste0("compensating_benefit_share:compensating_benefit_share:",
             "directional_variance")),
    subgroup_high_choice_share = c(
      "subgroup_high:x:numerator", "subgroup_high:x:denominator"),
    subgroup_low_choice_share = c(
      "subgroup_low:x:numerator", "subgroup_low:x:denominator"),
    mrs = c("mrs:numerator", "mrs:denominator"),
    directional_heterogeneity = c("H_Z", "H_R"))
  make_dml <- function(raw, estimate = seq_along(raw)) {
    fold <- list(
      derivative_refinement_error = stats::setNames(rep(0, length(raw)), raw),
      derivative_refinement_protocol_exact = stats::setNames(
        rep(TRUE, length(raw)), raw),
      honest_information_scaled_riesz_defect = stats::setNames(
        rep(0, length(raw)), raw),
      honest_riesz_null_space_defect = stats::setNames(
        rep(0, length(raw)), raw),
      ridge_neighborhood_artifact = stats::setNames(
        lapply(raw, function(label) list(label = label)), raw))
    structure(list(
      estimate = stats::setNames(estimate, raw),
      crossfit_plugin = stats::setNames(estimate / 10, raw),
      influence = matrix(
        seq_len(2L * length(raw)), 2L,
        dimnames = list(c("r1", "r2"), raw)),
      respondent_id = c("r1", "r2"), fold_details = list(fold)),
      class = c("scmix_dml", "list"))
  }
  calls <- list()
  local_mocked_bindings(
    scmix_dml = function(..., v22_registry = NULL) {
      marker <- unname(unlist(list(...)$plugin_targets, use.names = FALSE))
      calls[[length(calls) + 1L]] <<- marker
      raw <- unlist(raw_by_analysis[marker], use.names = FALSE)
      if (identical(marker, "directional_heterogeneity")) {
        make_dml(raw, c(2, 3))
      } else {
        make_dml(raw)
      }
    },
    .package = "sconjoint")
  callbacks <- stats::setNames(
    as.list(c(
      "active_q1_covariance_eigenvalue", "mean_direction",
      "mean_numerator", "mean_denominator", "choice_probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_high_choice_share", "subgroup_low_choice_share", "mrs")),
    c("active_q1_covariance_eigenvalue", "mean_direction",
      "mean_numerator", "mean_denominator", "choice_probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_high", "subgroup_low", "mrs"))
  targets <- list(
    callbacks = callbacks,
    heterogeneity = list(
      plugin_targets = list("directional_heterogeneity"),
      primitive_registry = list(list()),
      primitive_ids = c("H_Z", "H_R")))
  result <- sconjoint:::.scmix_v25_dml_once(
    list(deltaX = matrix(0, 2L, 2L)), targets, seed = 1L)
  expect_true(all(c(
    "subgroup_high_numerator", "mrs_denominator",
    "preference_direction_share", "preference_direction_variance",
    "compensating_benefit_share", "compensating_benefit_variance",
    "heterogeneity_explained_variance",
    "heterogeneity_residual_variance") %in% names(result$estimate)))
  expect_identical(result$estimate[["heterogeneity_explained_variance"]], 2)
  expect_identical(result$heterogeneity_label_map,
                   c(H_Z = "heterogeneity_explained_variance",
                     H_R = "heterogeneity_residual_variance"))
  expect_identical(length(calls), 2L)
  expect_identical(length(calls[[1L]]), 10L)
  expect_identical(calls[[2L]], "directional_heterogeneity")
  expect_identical(result$dml_execution$base, "single_fused_call")
  expect_identical(result$dml_execution$dml_call_count, 2L)
})

test_that("v2.5 q=1 fused ordinary DML equals separate loading-score solves", {
  fit <- .v25_dml_mock(q = 1L)
  first <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    list(
      target_type = "rowwise_expectation", value = mu[, 1L] + 0.2 * kappa,
      d_mu = cbind(rep(1, nrow(mu)), rep(0, nrow(mu))),
      d_kappa = rep(0.2, nrow(mu)), labels = "first",
      sigma_invariant = TRUE)
  }
  second <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    list(
      target_type = "rowwise_expectation", value = mu[, 2L]^2,
      d_mu = cbind(rep(0, nrow(mu)), 2 * mu[, 2L]),
      d_kappa = rep(0, nrow(mu)),
      labels = "second", sigma_invariant = TRUE)
  }
  fused <- sconjoint:::.scmix_v25_dml_call(
    fit, list(first = first, second = second), seed = 314L)
  one <- sconjoint:::.scmix_v25_dml_call(
    fit, list(first = first), seed = 314L)
  two <- sconjoint:::.scmix_v25_dml_call(
    fit, list(second = second), seed = 314L)
  expect_equal(fused$estimate["first"], one$estimate["first"],
               tolerance = 64 * .Machine$double.eps)
  expect_equal(fused$estimate["second"], two$estimate["second"],
               tolerance = 64 * .Machine$double.eps)
  expect_equal(fused$crossfit_plugin["first"],
               one$crossfit_plugin["first"],
               tolerance = 64 * .Machine$double.eps)
  expect_equal(fused$crossfit_plugin["second"],
               two$crossfit_plugin["second"],
               tolerance = 64 * .Machine$double.eps)
  expect_equal(fused$influence[, "first"], one$influence[, "first"],
               tolerance = 64 * .Machine$double.eps)
  expect_equal(fused$influence[, "second"], two$influence[, "second"],
               tolerance = 64 * .Machine$double.eps)
  for (k in seq_along(fused$fold_details)) {
    expect_identical(fused$fold_details[[k]]$loading_horizontal_dimension, 2L)
    for (field in c(
      "derivative_refinement_error",
      "honest_information_scaled_riesz_defect",
      "honest_riesz_null_space_defect")) {
      expect_equal(fused$fold_details[[k]][[field]]["first"],
                   one$fold_details[[k]][[field]]["first"],
                   tolerance = 64 * .Machine$double.eps)
      expect_equal(fused$fold_details[[k]][[field]]["second"],
                   two$fold_details[[k]][[field]]["second"],
                   tolerance = 64 * .Machine$double.eps)
    }
  }
})

test_that("v2.5 fused q=1 inference matches isolated v2.4 target analyses", {
  fit <- .v25_dml_mock(N = 24L, T = 2L, K = 2L, q = 1L)
  outcome <- list(
    deltaX = fit$deltaX, deltaX_names = colnames(fit$deltaX),
    respondent_id = fit$respondent_id,
    source_row_index = seq_len(nrow(fit$deltaX)))
  geometry <- list(
    target_dual_raw = matrix(
      c(1, 0, 0, 1, 1, 1), nrow = 2L,
      dimnames = list(colnames(fit$deltaX),
                      c("direction", "numerator", "denominator"))),
    z_task = fit$Z[, 1L])
  old_targets <- sconjoint:::.scmix_v24_target_callbacks(
    outcome, geometry, n_nodes = 15L)
  new_targets <- sconjoint:::.scmix_v25_target_callbacks(
    outcome, geometry, n_nodes = 15L)
  old <- sconjoint:::.scmix_v24_dml_once(fit, old_targets, seed = 887L)
  new <- sconjoint:::.scmix_v25_dml_once(fit, new_targets, seed = 887L)
  expect_identical(new$dml_execution$base, "single_fused_call")
  expect_identical(new$dml_execution$dml_call_count, 2L)
  expect_identical(names(new$estimate), names(old$estimate))
  expect_equal(new$estimate, old$estimate,
               tolerance = 256 * .Machine$double.eps)
  expect_equal(new$plugin, old$plugin,
               tolerance = 256 * .Machine$double.eps)
  expect_equal(new$influence, old$influence,
               tolerance = 256 * .Machine$double.eps)
  expect_identical(new$failed_targets, old$failed_targets)
  expect_identical(new$failed_families, old$failed_families)
  new_view <- sconjoint:::.scmix_v25_inference_view(new)
  old_view <- sconjoint:::.scmix_v24_inference_view(old)
  expect_equal(new_view$reported_estimate, old_view$reported_estimate,
               tolerance = 256 * .Machine$double.eps)
  expect_equal(new_view$reported_se, old_view$reported_se,
               tolerance = 256 * .Machine$double.eps)
  expect_equal(new_view$reported_influence, old_view$reported_influence,
               tolerance = 256 * .Machine$double.eps)

  ## The v2.4 per-target analyses are the numerical reference.  Its historical
  ## composite builder retained unrelated diagnostic labels from each
  ## one-target result, however, whereas v2.5 subsets each fold field to the
  ## primitive map before combining it.  Normalize that container-only detail
  ## with the v2.5 combiner before comparing diagnostics or asking the
  ## unchanged v2.4 gate engine to evaluate the isolated target analyses.
  old_base_analyses <- old$analyses[vapply(
    old$analyses,
    function(x) identical(x$status, "success") &&
      !identical(x$analysis_id, "directional_heterogeneity"),
    logical(1L))]
  old_base_normalized <- sconjoint:::.scmix_v25_merge_base_dml(
    old_base_analyses)
  old$base <- old_base_normalized$dml
  old$base_label_map <- old_base_normalized$map

  for (k in seq_along(new$base$fold_details)) {
    for (field in c(
      "derivative_refinement_error",
      "honest_information_scaled_riesz_defect",
      "honest_riesz_null_space_defect")) {
      expect_equal(new$base$fold_details[[k]][[field]],
                   old$base$fold_details[[k]][[field]],
                   tolerance = 256 * .Machine$double.eps)
    }
  }

  fit_metrics <- list(
    start_surface = data.frame(objective_relative_gap = c(0, 5e-7)),
    maximum_block_kkt = 1e-6,
    maximum_nested_objective_gap = 1e-7,
    maximum_loading_probability_error = 1e-10,
    every_polish_completed = TRUE, every_polish_accepted = TRUE,
    every_repolish_exact_state = TRUE, every_state_float64 = TRUE,
    all_finite = TRUE)
  old_near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  new_near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v25_gate_target_specs())),
    names(sconjoint:::.scmix_v25_gate_target_specs()))
  old_bank <- sconjoint:::.scmix_v24_multiplier_bank(
    old$respondent_id, draws = 9999L, seed = 713L)
  new_bank <- sconjoint:::.scmix_v25_multiplier_bank(
    new$respondent_id, draws = 9999L, seed = 713L)
  old_bundle <- list(inference = old, view = old_view)
  new_bundle <- list(inference = new, view = new_view)
  root <- testthat::test_path("..", "..")
  old_registry <- utils::read.csv(file.path(
    root, "inst", "protocol", "inference-v2.4-decision-registry.csv"),
    stringsAsFactors = FALSE, check.names = FALSE)
  new_registry <- utils::read.csv(file.path(
    root, "inst", "protocol", "inference-v2.5-decision-registry.csv"),
    stringsAsFactors = FALSE, check.names = FALSE)
  old_gates <- sconjoint:::.scmix_v24_gate_engine(
    old_bundle, old_bundle, old_bundle, fit_metrics, old_near, old_bank,
    old_registry)
  new_gates <- sconjoint:::.scmix_v25_gate_engine(
    new_bundle, new_bundle, new_bundle, fit_metrics, new_near, new_bank,
    new_registry)
  gate_columns <- c("target_id", "family_id", "rule_id", "observed",
                    "comparator", "threshold", "pass")
  expect_equal(new_gates$target_gates[, gate_columns],
               old_gates$target_gates[, gate_columns],
               tolerance = 512 * .Machine$double.eps)
  expect_equal(new_gates$fit_gates, old_gates$fit_gates,
               tolerance = 512 * .Machine$double.eps)
  family_columns <- c("family_id", "released", "failed_gates")
  expect_identical(new_gates$family_gates[, family_columns],
                   old_gates$family_gates[, family_columns])
  expect_equal(new_gates$target_metrics, old_gates$target_metrics,
               tolerance = 512 * .Machine$double.eps)
  expect_equal(new_gates$primitive_metrics, old_gates$primitive_metrics,
               tolerance = 512 * .Machine$double.eps)
})

test_that("v2.5 target-specific DML failure is isolated to its family", {
  raw_by_analysis <- list(
    active = "active_q1_covariance_eigenvalue",
    mean_direction = "mean_direction",
    mean_numerator = "mean_numerator",
    mean_denominator = "mean_denominator",
    choice_probability = "choice_probability",
    preference_direction_share = c(
      "preference_direction_share:preference_direction_share",
      "preference_direction_share:x:directional_variance"),
    compensating_benefit_share = c(
      "compensating_benefit_share:compensating_benefit_share",
      "compensating_benefit_share:x:directional_variance"),
    subgroup_high = c("subgroup_high:x:numerator",
                      "subgroup_high:x:denominator"),
    subgroup_low = c("subgroup_low:x:numerator",
                     "subgroup_low:x:denominator"),
    mrs = c("mrs:numerator", "mrs:denominator"),
    heterogeneity = c("H_Z", "H_R"))
  make_dml <- function(raw) {
    metric <- function(value) stats::setNames(rep(value, length(raw)), raw)
    fold <- list(
      derivative_refinement_error = metric(0),
      derivative_refinement_protocol_exact = metric(TRUE),
      honest_information_scaled_riesz_defect = metric(0),
      honest_riesz_null_space_defect = metric(0),
      ridge_neighborhood_artifact = stats::setNames(
        lapply(raw, function(x) list(id = x)), raw),
      honest_evaluation_information = diag(2),
      riesz_numerical_rank_rule = "dimension_epsilon",
      riesz_numerical_rank_cutoff = 2 * .Machine$double.eps,
      honest_evaluation_numerical_rank = 2L)
    value <- seq_along(raw) + 0.5
    structure(list(
      estimate = stats::setNames(value, raw),
      crossfit_plugin = stats::setNames(value - 0.1, raw),
      influence = matrix(
        seq_len(4L * length(raw)), 4L,
        dimnames = list(paste0("r", 1:4), raw)),
      respondent_id = paste0("r", 1:4), fold_details = list(fold)),
      class = c("scmix_dml", "list"))
  }
  callbacks <- stats::setNames(
    as.list(c("active", "mean_direction", "mean_numerator",
              "mean_denominator", "choice_probability",
              "preference_direction_share", "compensating_benefit_share",
              "subgroup_high", "subgroup_low", "mrs")),
    c("active_q1_covariance_eigenvalue", "mean_direction",
      "mean_numerator", "mean_denominator", "choice_probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_high", "subgroup_low", "mrs"))
  targets <- list(
    callbacks = callbacks,
    heterogeneity = list(
      plugin_targets = list("heterogeneity"),
      primitive_registry = list(list()), primitive_ids = c("H_Z", "H_R")))
  calls <- character()
  local_mocked_bindings(
    scmix_dml = function(..., v22_registry = NULL) {
      marker <- unname(list(...)$plugin_targets[[1L]])
      calls <<- c(calls, marker)
      if (identical(marker, "mrs")) stop("target-specific MRS failure")
      make_dml(raw_by_analysis[[marker]])
    },
    .package = "sconjoint")
  result <- sconjoint:::.scmix_v25_dml_once(
    list(deltaX = matrix(0, 4L, 2L)), targets, seed = 2L)
  expect_identical(calls, c(
    "active", "active", "mean_direction", "mean_numerator", "mean_denominator",
    "choice_probability", "preference_direction_share",
    "compensating_benefit_share", "subgroup_high", "subgroup_low", "mrs",
    "heterogeneity"))
  expect_identical(result$failed_targets, "mrs")
  expect_identical(result$failed_families, "mrs")
  expect_false(any(c("mrs_numerator", "mrs_denominator") %in%
                     names(result$estimate)))
  expect_true(all(c("mean_direction", "choice_probability",
                    "heterogeneity_explained_variance") %in%
                  names(result$estimate)))
  expect_true(result$base$target_specific_composite)
  expect_true(all(c("mean_direction", "choice_probability") %in%
                    names(result$base$estimate)))
  view <- sconjoint:::.scmix_v25_inference_view(result)
  expect_true(is.na(view$reported_estimate[["mrs"]]))
  expect_identical(view$reported_se[["mrs"]], Inf)
  expect_true(is.finite(view$reported_estimate[["mean_direction"]]))
  expect_identical(view$failed_families, "mrs")
})

test_that("v2.5 global fit-regularity DML failure fails the replication", {
  targets <- list(
    callbacks = list(active_q1_covariance_eigenvalue = "active"),
    heterogeneity = list(plugin_targets = list("heterogeneity"),
                         primitive_registry = list(list())))
  local_mocked_bindings(
    scmix_dml = function(...) stop("common score failure"),
    .package = "sconjoint")
  expect_error(sconjoint:::.scmix_v25_dml_once(
    list(deltaX = matrix(0, 4L, 2L)), targets, seed = 2L),
    "global v2.5 fit-regularity DML preflight failed")
})

test_that("v2.5 truth coverage is simultaneous within a released family", {
  family <- structure(list(
    members = list(
      a = list(interval = c(lower = 0, upper = 2)),
      b = list(interval = c(lower = -1, upper = 1)))),
    class = c("scmix_v25_family_result", "list"))
  truth <- data.frame(
    family_id = c("smooth_means", "smooth_means"),
    target_id = c("a", "b"), truth = c(1, 0.5),
    status = "defined", stringsAsFactors = FALSE)
  result <- sconjoint:::.scmix_v25_family_truth_result(
    "smooth_means", family, TRUE, truth)
  expect_true(result$released)
  expect_true(result$covered)
  family$members$b$interval <- c(-1, 0)
  result <- sconjoint:::.scmix_v25_family_truth_result(
    "smooth_means", family, TRUE, truth)
  expect_false(result$covered)
  truth$status[[2L]] <- "undefined_exact_boundary"
  result <- sconjoint:::.scmix_v25_family_truth_result(
    "smooth_means", family, TRUE, truth)
  expect_true(is.na(result$covered))
})

test_that("v2.5 artifact writes are signed, private, and no-overwrite", {
  root <- tempfile("v25-artifact-")
  dir.create(root)
  payload <- sconjoint:::.scmix_v25_sign(list(
    cell_id = "cell1", replication = 1L, synthetic_outcome = TRUE,
    application_outcomes_used = FALSE, completed = TRUE),
    "scmix_v25_replication_artifact")
  path <- file.path(root, "one.rds")
  saved <- sconjoint:::.scmix_v25_save_atomic(payload, path)
  expect_identical(
    sconjoint:::.scmix_v25_read_artifact(
      saved, data.frame(cell_id = "cell1", replication = 1L)),
    payload)
  expect_error(sconjoint:::.scmix_v25_save_atomic(payload, path),
               "Refusing to overwrite")
})

test_that("v2.5 artifact reuse requires every frozen evaluator binding", {
  root <- tempfile("v25-reuse-")
  dir.create(root)
  run_dir <- file.path(root, "run")
  dir.create(run_dir)
  plan <- .v25_runner_plan()
  evaluator <- .v25_runner_evaluator()
  written <- .v25_runner_pilot_artifact(
    run_dir, plan, evaluator, "cell1", 1L, elapsed = 2.5)
  verified <- sconjoint:::.scmix_v25_validate_reusable_artifact(
    written$artifact_file,
    list(stream = "pilot", cell_id = "cell1", replication = 1L),
    plan, evaluator, expected_sha256 = written$artifact_sha256)
  expect_identical(verified$artifact_sha256, written$artifact_sha256)
  expect_identical(verified$elapsed_seconds, 2.5)
  expect_error(sconjoint:::.scmix_v25_validate_reusable_artifact(
    written$artifact_file,
    list(stream = "pilot", cell_id = "cell1", replication = 1L),
    plan, evaluator, expected_sha256 = .v25_runner_hash("d")),
    "recorded file hash")
  changed <- evaluator
  changed$settings$fingerprint <- .v25_runner_hash("e")
  expect_error(sconjoint:::.scmix_v25_validate_reusable_artifact(
    written$artifact_file,
    list(stream = "pilot", cell_id = "cell1", replication = 1L),
    plan, changed), "exact current schedule and evaluator binding")
})

test_that("v2.5 worker allocation is deterministic by cell and lane", {
  plan <- .v25_runner_plan()
  pilot <- expand.grid(
    cell_id = plan$cells$cell_id, replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result <- sconjoint:::.scmix_v25_run_worker_schedule(
    pilot, 12L, function(cell, replication, worker) list(
      cell_id = cell, replication = replication, worker_id = worker),
    parallel = FALSE)
  by_worker <- split(result, vapply(result, `[[`, integer(1L), "worker_id"))
  expect_length(by_worker, 12L)
  expect_true(all(lengths(by_worker) == 1L))
  expect_true(all(vapply(by_worker, function(x)
    length(unique(vapply(x, `[[`, character(1L), "cell_id"))) == 1L,
    logical(1L))))
  expected_worker <- vapply(result, function(x) {
    cell <- match(x$cell_id, plan$cells$cell_id)
    2L * (cell - 1L) + as.integer(x$replication)
  }, integer(1L))
  expect_identical(vapply(result, `[[`, integer(1L), "worker_id"),
                   expected_worker)

  evidence <- sconjoint:::.scmix_v25_evidence_jobs(plan, 3L)
  expect_equal(nrow(evidence), 60L)
  expect_identical(sort(unique(evidence$replication)), 21:30)
})

test_that("v2.5 PSOCK backend maps twelve workers and always stops them", {
  plan <- .v25_runner_plan()
  pilot <- expand.grid(
    cell_id = plan$cells$cell_id, replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  source_root <- normalizePath(testthat::test_path("..", ".."),
                               winslash = "/", mustWork = TRUE)
  withr::local_envvar(SCONJOINT_SOURCE_ROOT = source_root)

  factory_calls <- 0L
  methods_seen <- NULL
  bootstrap_calls <- 0L
  stop_calls <- 0L
  fake_cluster <- structure(as.list(seq_len(12L)),
                            class = "v25_fake_cluster")
  factory <- function(workers, methods) {
    factory_calls <<- factory_calls + 1L
    methods_seen <<- methods
    expect_identical(workers, 12L)
    fake_cluster
  }
  initialize <- function(cluster, bootstrap, root) {
    bootstrap_calls <<- bootstrap_calls + 1L
    expect_identical(cluster, fake_cluster)
    expect_true(is.function(bootstrap))
    expect_identical(environment(bootstrap), baseenv())
    expect_identical(root, source_root)
    lapply(seq_len(12L), function(worker) list(
      source_root = root, pid = as.integer(1000L + worker),
      torch_threads = 1L))
  }
  apply <- function(cluster, worker_ids, work) {
    expect_identical(cluster, fake_cluster)
    expect_identical(worker_ids, seq_len(12L))
    lapply(worker_ids, work)
  }
  stop <- function(cluster) {
    expect_identical(cluster, fake_cluster)
    stop_calls <<- stop_calls + 1L
    invisible(NULL)
  }
  result <- sconjoint:::.scmix_v25_run_worker_schedule(
    pilot, 12L, function(cell, replication, worker) list(
      cell_id = cell, replication = replication, worker_id = worker),
    parallel = TRUE, .cluster_factory = factory,
    .cluster_call = initialize, .cluster_apply = apply,
    .cluster_stop = stop)

  expect_identical(factory_calls, 1L)
  expect_identical(methods_seen, FALSE)
  expect_identical(bootstrap_calls, 1L)
  expect_identical(stop_calls, 1L)
  expect_length(result, 12L)
  expect_true(all(vapply(result, function(x)
    identical(x$worker_id,
              2L * (match(x$cell_id, plan$cells$cell_id) - 1L) +
                as.integer(x$replication)),
    logical(1L))))
  expect_false(any(c("source_root", "pid", "torch_threads") %in%
                     unique(unlist(lapply(result, names)))))

  failing_apply <- function(cluster, worker_ids, work) {
    stop("private worker detail", call. = FALSE)
  }
  expect_error(sconjoint:::.scmix_v25_run_worker_schedule(
    pilot, 12L, function(...) NULL, parallel = TRUE,
    .cluster_factory = factory, .cluster_call = initialize,
    .cluster_apply = failing_apply, .cluster_stop = stop),
    "One or more v2.5 worker processes failed outside a job guard",
    fixed = TRUE)
  expect_identical(factory_calls, 2L)
  expect_identical(bootstrap_calls, 2L)
  expect_identical(stop_calls, 2L)
})

test_that("v2.5 PSOCK dispatch forces an outer job-function promise", {
  plan <- .v25_runner_plan()
  jobs <- expand.grid(
    cell_id = plan$cells$cell_id, replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  source_root <- normalizePath(testthat::test_path("..", ".."),
                               winslash = "/", mustWork = TRUE)
  withr::local_envvar(SCONJOINT_SOURCE_ROOT = source_root)
  fake_cluster <- structure(as.list(seq_len(12L)),
                            class = "v25_fake_cluster")
  factory <- function(workers, methods) fake_cluster
  initialize <- function(cluster, bootstrap, root) {
    lapply(seq_len(12L), function(worker) list(
      source_root = root, pid = as.integer(2000L + worker),
      torch_threads = 1L))
  }
  stopped <- FALSE
  stop <- function(cluster) {
    stopped <<- TRUE
    invisible(NULL)
  }

  caller <- new.env(parent = environment())
  caller$fun <- function(cell, replication, worker) list(
    cell_id = cell, replication = replication, worker_id = worker)
  caller$invoke <- function(jobs, factory, initialize, apply, stop) {
    sconjoint:::.scmix_v25_run_worker_schedule(
      jobs, 12L, job_fun = fun, parallel = TRUE,
      .cluster_factory = factory, .cluster_call = initialize,
      .cluster_apply = apply, .cluster_stop = stop)
  }
  environment(caller$invoke) <- caller
  apply <- function(cluster, worker_ids, work) {
    rm("fun", envir = caller)
    remote_work <- unserialize(serialize(work, NULL))
    lapply(worker_ids, remote_work)
  }
  result <- caller$invoke(jobs, factory, initialize, apply, stop)
  expect_true(stopped)
  expect_length(result, 12L)
  expect_true(all(vapply(result, function(x)
    identical(x$worker_id,
              2L * (match(x$cell_id, plan$cells$cell_id) - 1L) +
                as.integer(x$replication)),
    logical(1L))))
})

test_that("v2.5 production waves expose and use only fixed dispatch", {
  expect_identical(names(formals(scmix_v25_run_pilot_wave)), c(
    "run_dir", "plan", "state_dir", "source_root", "verbose"))
  expect_identical(names(formals(scmix_v25_run_evidence_batch)), c(
    "batch_index", "run_dir", "plan", "state_dir", "source_root",
    "verbose"))
  expect_false(any(c(".parallel", ".job_fun") %in%
                     names(formals(scmix_v25_run_pilot_wave))))
  expect_false(any(c(".parallel", ".job_fun") %in%
                     names(formals(scmix_v25_run_evidence_batch))))
  expect_identical(names(formals(sconjoint:::.scmix_v25_pilot_wave_core)),
                   c("run_dir", "plan", "state_dir", "source_root",
                     "verbose"))
  expect_identical(
    names(formals(sconjoint:::.scmix_v25_evidence_batch_core)),
    c("batch_index", "run_dir", "plan", "state_dir", "source_root",
      "verbose"))
  expect_false(exists(".scmix_v25_test_run_pilot_wave",
                      envir = asNamespace("sconjoint"), inherits = FALSE))
  expect_false(exists(".scmix_v25_test_run_evidence_batch",
                      envir = asNamespace("sconjoint"), inherits = FALSE))
  expect_false(any(grepl("TESTTHAT", readLines(
    testthat::test_path("..", "..", "R", "calibration-v25-runner.R"),
    warn = FALSE), fixed = TRUE)))

  scheduled <- NULL
  local_mocked_bindings(
    .scmix_v25_run_worker_schedule = function(
        jobs, workers, job_fun, parallel, ...) {
      scheduled <<- list(jobs = jobs, workers = workers,
                         job_fun = job_fun, parallel = parallel)
      "scheduled"
    },
    .package = "sconjoint")
  fixed_dispatch <- sconjoint:::.scmix_v25_fixed_psock_dispatch

  jobs <- expand.grid(
    cell_id = paste0("cell", seq_len(6L)), replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  job_fun <- function(...) NULL
  expect_identical(fixed_dispatch(jobs, job_fun), "scheduled")
  expect_identical(scheduled$jobs, jobs)
  expect_identical(scheduled$workers, 12L)
  expect_identical(scheduled$job_fun, job_fun)
  expect_identical(scheduled$parallel, TRUE)
})

test_that("v2.5 evidence orchestration can be isolated only by test mocks", {
  root <- tempfile("v25-evidence-test-")
  dir.create(root)
  plan <- .v25_runner_plan()
  run_dir <- .v25_test_run_dir(root, plan)
  local_mocked_bindings(
    .scmix_v25_evaluator_readiness = function(...)
      list(evidence_outcome_generation_permitted = TRUE),
    .package = "sconjoint")
  job <- function(cell, replication, worker) list(
    cell_id = cell, replication = replication, completed = TRUE,
    artifact_file = file.path(run_dir, paste0(cell, "-", replication)),
    artifact_sha256 = .v25_runner_hash("a"), worker_id = worker,
    elapsed_seconds = 1, resumed = FALSE, error = "")
  .v25_local_serial_dispatch(job)
  result <- scmix_v25_run_evidence_batch(
    1L, run_dir, plan = plan, source_root = root)
  manifest <- utils::read.csv(result$manifest_file,
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  expect_identical(result$attempted_jobs, 60L)
  expect_identical(result$completed_jobs, 60L)
  expect_identical(result$workers, 12L)
  expect_equal(nrow(manifest), 60L)
  expect_identical(
    manifest$worker_id,
    sconjoint:::.scmix_v25_worker_ids(
      manifest, as.character(plan$cells$cell_id)))
})

test_that("v2.5 pilot public manifest contains metadata only", {
  root <- tempfile("v25-pilot-")
  dir.create(root)
  plan <- .v25_runner_plan()
  evaluator <- .v25_runner_evaluator()
  local_mocked_bindings(
    .scmix_v25_evaluator_readiness = function(...)
      list(pilot_outcome_generation_permitted = TRUE,
           evaluator = evaluator),
    scmix_v25_runtime_projection = function(...)
      list(projected_evidence_wall_seconds = 120,
           projected_evidence_wall_hours = 1 / 30, pass = TRUE),
    scmix_v25_generate_study_outcome = function(...)
      stop("a valid pilot artifact should have been resumed"),
    .package = "sconjoint")
  run_dir <- .v25_test_run_dir(root, plan)
  job <- function(cell, replication, worker) {
    written <- .v25_runner_pilot_artifact(
      run_dir, plan, evaluator, cell, replication, elapsed = worker)
    c(list(
      cell_id = cell, replication = replication, worker_id = worker,
      completed = TRUE, resumed = FALSE, error = ""), written)
  }
  .v25_local_serial_dispatch(job)
  alternate <- file.path(normalizePath(root, mustWork = TRUE),
                         "runs", "v25", "alternate-pilot")
  expect_error(scmix_v25_run_pilot_wave(
    alternate, plan = plan, source_root = root),
    "one frozen canonical run directory", fixed = TRUE)
  expect_false(dir.exists(alternate))
  result <- scmix_v25_run_pilot_wave(
    run_dir, plan = plan, source_root = root)
  manifest <- utils::read.csv(result$pilot_run_manifest,
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  expect_identical(names(manifest), c(
    "cell_id", "replication", "completed", "worker_id",
    "elapsed_seconds"))
  expect_equal(nrow(manifest), 12L)
  expect_false(any(grepl(
    "estimate|interval|coverage|artifact|released|failed_gates",
    names(manifest), ignore.case = TRUE)))
  expect_false(any(c("estimates", "gates", "coverage") %in%
                     names(result)))
  expect_false(result$target_or_gate_results_inspected)
  expect_true(file.exists(result$private_artifact_manifest))
  expect_identical(
    unname(as.character(tools::sha256sum(result$private_artifact_manifest))),
    result$private_artifact_manifest_sha256)
  private <- readRDS(result$private_artifact_manifest)
  expect_true(sconjoint:::.scmix_v25_intact(
    private, "scmix_v25_pilot_artifact_manifest"))
  expect_true(private$complete)
  expect_equal(nrow(private$records), 12L)
  expect_true(all(grepl(
    "^[[:xdigit:]]{64}$", private$records$artifact_sha256)))

  expect_gt(result$wave_finished_unix, result$wave_started_unix)
  expect_error(scmix_v25_run_pilot_wave(
    alternate, plan = plan, source_root = root),
    "one frozen canonical run directory", fixed = TRUE)
  expect_false(dir.exists(alternate))
  alias <- file.path(normalizePath(root, mustWork = TRUE),
                     "runs", "v25", "pilot-alias")
  if (isTRUE(file.symlink(run_dir, alias))) {
    expect_error(scmix_v25_run_pilot_wave(
      alias, plan = plan, source_root = root),
      "one frozen canonical run directory", fixed = TRUE)
  }
  expect_error(scmix_v25_run_pilot_wave(
    run_dir, plan = plan, source_root = root),
    "first valid v2.5 pilot wave is binding", fixed = TRUE)
})

test_that("v2.5 resumed partial pilot cannot shorten its runtime window", {
  root <- tempfile("v25-pilot-resume-")
  dir.create(root)
  plan <- .v25_runner_plan()
  evaluator <- .v25_runner_evaluator()
  local_mocked_bindings(
    .scmix_v25_evaluator_readiness = function(...)
      list(pilot_outcome_generation_permitted = TRUE, evaluator = evaluator),
    .package = "sconjoint")
  run_dir <- .v25_test_run_dir(root, plan)
  fail_job <- function(cell, replication, worker) list(
    cell_id = cell, replication = replication, worker_id = worker,
    completed = FALSE, elapsed_seconds = 0.01, resumed = FALSE,
    artifact_file = "", artifact_sha256 = "", error = "infrastructure")
  .v25_local_serial_dispatch(fail_job)
  first <- scmix_v25_run_pilot_wave(
    run_dir, plan = plan, source_root = root)
  expect_identical(first$completed_jobs, 0L)

  private_path <- first$private_artifact_manifest
  private <- unclass(readRDS(private_path))
  private$fingerprint <- NULL
  private$wave_started_unix <- private$wave_started_unix - 100
  private <- sconjoint:::.scmix_v25_sign(
    private, "scmix_v25_pilot_artifact_manifest")
  saveRDS(private, private_path, version = 3L)
  pass_job <- function(cell, replication, worker) {
    written <- .v25_runner_pilot_artifact(
      run_dir, plan, evaluator, cell, replication, elapsed = 0.02)
    c(list(
      cell_id = cell, replication = replication, worker_id = worker,
      completed = TRUE, resumed = FALSE, error = ""), written)
  }
  .v25_local_serial_dispatch(pass_job)
  second <- scmix_v25_run_pilot_wave(
    run_dir, plan = plan, source_root = root)
  expect_true(second$integrity_status == "pass")
  expect_gte(second$wave_finished_unix - second$wave_started_unix, 100)
  expect_gte(second$projected_evidence_wall_seconds, 1.25 * 25 * 100)
})
