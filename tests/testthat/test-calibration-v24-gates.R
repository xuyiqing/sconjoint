.v24_gate_ids <- function() sprintf("r%02d", seq_len(20L))

.v24_gate_estimate <- function(
    active = 0.4, preference_variance = 0.25,
    compensating_variance = 0.20) {
  c(
    mean_direction = 0.20,
    mean_numerator = 0.25,
    mean_denominator = 0.50,
    choice_probability = 0.55,
    preference_direction_share = 0.60,
    preference_direction_variance = preference_variance,
    compensating_benefit_share = 0.58,
    compensating_benefit_variance = compensating_variance,
    subgroup_high_numerator = 0.30,
    subgroup_high_denominator = 0.50,
    subgroup_low_numerator = 0.20,
    subgroup_low_denominator = 0.50,
    mrs_numerator = 0.25,
    mrs_denominator = 0.50,
    active_q1_covariance_eigenvalue = active,
    heterogeneity_explained_variance = 0.30,
    heterogeneity_residual_variance = 0.40)
}

.v24_gate_influence <- function(ids, primitive) {
  i <- seq_along(ids)
  value <- vapply(seq_along(primitive), function(j) {
    x <- sin((i + 0.37 * j) * (j + 1) / 7) +
      0.4 * cos((i + j) / (j + 2))
    0.06 * (x - mean(x)) / stats::sd(x)
  }, numeric(length(ids)))
  dimnames(value) <- list(ids, primitive)
  value
}

.v24_gate_fold <- function(raw_estimate, raw_if, fold, derivative = 1e-5) {
  n <- nrow(raw_if)
  training_n <- n / 2
  nu <- c(training_n^(-1), training_n^(-3 / 4),
          training_n^(-1 / 2))
  artifacts <- lapply(seq_along(raw_estimate), function(j) {
    change <- 0.0005 * sin(seq_len(n) * (j + fold) / 5)
    change <- change - mean(change)
    influence <- cbind(
      raw_if[, j] + change,
      raw_if[, j],
      raw_if[, j] + 2 * change)
    rownames(influence) <- rownames(raw_if)
    list(
      nu = nu,
      raw_one_step = c(raw_estimate[[j]] + 0.0002,
                       raw_estimate[[j]],
                       raw_estimate[[j]] + 0.0004),
      standard_error = rep(0.01, 3L),
      influence = influence)
  })
  names(artifacts) <- names(raw_estimate)
  information <- diag(c(1, 0.7, 0.4))
  cutoff <- nrow(information) * .Machine$double.eps
  list(
    training_n = training_n,
    derivative_refinement_error = stats::setNames(
      rep(derivative, length(raw_estimate)), names(raw_estimate)),
    derivative_refinement_protocol_exact = stats::setNames(
      rep(TRUE, length(raw_estimate)), names(raw_estimate)),
    honest_information_scaled_riesz_defect = stats::setNames(
      rep(0.05, length(raw_estimate)), names(raw_estimate)),
    honest_riesz_null_space_defect = stats::setNames(
      rep(1e-7, length(raw_estimate)), names(raw_estimate)),
    ridge_neighborhood_artifact = artifacts,
    honest_evaluation_information = information,
    honest_evaluation_numerical_rank = 3L,
    riesz_numerical_rank_rule = "dimension_epsilon",
    riesz_numerical_rank_cutoff = cutoff)
}

.v24_gate_dml <- function(estimate, influence, raw_names) {
  raw_estimate <- estimate
  names(raw_estimate) <- raw_names
  raw_if <- influence
  colnames(raw_if) <- raw_names
  structure(list(
    estimate = raw_estimate,
    influence = raw_if,
    respondent_id = rownames(raw_if),
    fold_details = lapply(1:2, function(k)
      .v24_gate_fold(raw_estimate, raw_if, k))),
    class = c("scmix_dml", "list"))
}

.v24_gate_bundle <- function(
    active = 0.4, shift = 0, influence_shift = 0,
    preference_variance = 0.25, compensating_variance = 0.20) {
  ids <- .v24_gate_ids()
  estimate <- .v24_gate_estimate(
    active, preference_variance, compensating_variance)
  IF <- .v24_gate_influence(ids, names(estimate))
  base_ids <- setdiff(names(estimate), c(
    "heterogeneity_explained_variance",
    "heterogeneity_residual_variance"))
  raw_base <- base_ids
  raw_base[raw_base == "preference_direction_share"] <-
    "preference_direction_share:preference_direction_share"
  raw_base[raw_base == "preference_direction_variance"] <-
    paste0("preference_direction_share:preference_direction_share:",
           "directional_variance")
  raw_base[raw_base == "compensating_benefit_share"] <-
    "compensating_benefit_share:compensating_benefit_share"
  raw_base[raw_base == "compensating_benefit_variance"] <-
    paste0("compensating_benefit_share:compensating_benefit_share:",
           "directional_variance")
  raw_base[raw_base == "subgroup_high_numerator"] <-
    "subgroup_high:choice:numerator"
  raw_base[raw_base == "subgroup_high_denominator"] <-
    "subgroup_high:choice:denominator"
  raw_base[raw_base == "subgroup_low_numerator"] <-
    "subgroup_low:choice:numerator"
  raw_base[raw_base == "subgroup_low_denominator"] <-
    "subgroup_low:choice:denominator"
  raw_base[raw_base == "mrs_numerator"] <- "mrs:numerator"
  raw_base[raw_base == "mrs_denominator"] <- "mrs:denominator"
  heterogeneity_ids <- c("heterogeneity_explained_variance",
                         "heterogeneity_residual_variance")
  base <- .v24_gate_dml(
    estimate[base_ids], IF[, base_ids, drop = FALSE], raw_base)
  heterogeneity <- .v24_gate_dml(
    estimate[heterogeneity_ids], IF[, heterogeneity_ids, drop = FALSE],
    c("H_Z", "H_R"))
  branch_estimate <- estimate + shift
  branch_if <- IF
  if (influence_shift != 0) {
    perturbation <- influence_shift * sin(seq_len(nrow(IF)) / 3)
    branch_if <- sweep(branch_if, 1L, perturbation, `+`)
    branch_if <- sweep(branch_if, 2L, colMeans(branch_if), `-`)
  }
  list(
    estimate = branch_estimate,
    plugin = branch_estimate,
    influence = branch_if,
    respondent_id = ids,
    base = base,
    heterogeneity = heterogeneity,
    base_label_map = stats::setNames(raw_base, base_ids),
    heterogeneity_label_map = c(
      H_Z = "heterogeneity_explained_variance",
      H_R = "heterogeneity_residual_variance"),
    protocol_id = "paperps-inference-2026-09-06-v2.4")
}

.v24_gate_failed_bundle <- function(family = c("mrs", "heterogeneity")) {
  family <- match.arg(family)
  out <- .v24_gate_bundle()
  if (identical(family, "mrs")) {
    target <- "mrs"
    canonical <- c("mrs_numerator", "mrs_denominator")
    raw <- unname(out$base_label_map[canonical])
    out$estimate <- out$estimate[setdiff(names(out$estimate), canonical)]
    out$plugin <- out$plugin[setdiff(names(out$plugin), canonical)]
    out$influence <- out$influence[,
      setdiff(colnames(out$influence), canonical), drop = FALSE]
    out$base$estimate <- out$base$estimate[
      setdiff(names(out$base$estimate), raw)]
    out$base$influence <- out$base$influence[,
      setdiff(colnames(out$base$influence), raw), drop = FALSE]
    for (k in seq_along(out$base$fold_details)) {
      for (field in c(
        "derivative_refinement_error",
        "derivative_refinement_protocol_exact",
        "honest_information_scaled_riesz_defect",
        "honest_riesz_null_space_defect",
        "ridge_neighborhood_artifact")) {
        value <- out$base$fold_details[[k]][[field]]
        out$base$fold_details[[k]][[field]] <-
          value[setdiff(names(value), raw)]
      }
    }
    out$base_label_map <- out$base_label_map[
      setdiff(names(out$base_label_map), canonical)]
    out$failed_targets <- target
    out$failed_families <- "mrs"
  } else {
    target <- c("heterogeneity_explained_variance",
                "heterogeneity_residual_variance",
                "heterogeneity_explained_share")
    canonical <- c("heterogeneity_explained_variance",
                   "heterogeneity_residual_variance")
    out$estimate <- out$estimate[setdiff(names(out$estimate), canonical)]
    out$plugin <- out$plugin[setdiff(names(out$plugin), canonical)]
    out$influence <- out$influence[,
      setdiff(colnames(out$influence), canonical), drop = FALSE]
    out$heterogeneity <- NULL
    out$failed_targets <- target
    out$failed_families <- "directional_heterogeneity"
  }
  out$failure_records <- data.frame(
    analysis_id = family, family_id = out$failed_families,
    target_ids = paste(target, collapse = "|"), error = "isolated toy failure",
    stringsAsFactors = FALSE)
  out$target_specific <- TRUE
  out$complete_target_registry <- FALSE
  out
}

.v24_gate_fit_metrics <- function() {
  list(
    start_surface = data.frame(objective_relative_gap = c(0, 5e-7)),
    maximum_block_kkt = 1e-6,
    maximum_nested_objective_gap = 1e-7,
    maximum_loading_probability_error = 1e-10,
    every_polish_completed = TRUE,
    every_polish_accepted = TRUE,
    every_repolish_exact_state = TRUE,
    every_state_float64 = TRUE,
    all_finite = TRUE)
}

.v24_gate_registry <- function() {
  utils::read.csv(
    file.path(testthat::test_path("..", ".."), "inst", "protocol",
              "inference-v2.4-decision-registry.csv"),
    stringsAsFactors = FALSE, check.names = FALSE)
}

test_that("v2.4 reconstructs fixed ridge alternatives over all folds", {
  baseline <- .v24_gate_bundle()
  alternative <- sconjoint:::.scmix_v24_reconstruct_ridge(
    baseline, exponent = -1)
  expect_equal(alternative$estimate[["mean_direction"]], 0.2004,
               tolerance = 1e-14)
  expect_equal(alternative$estimate[["mrs_denominator"]], 0.5004,
               tolerance = 1e-14)
  expect_identical(rownames(alternative$influence), .v24_gate_ids())
  expect_identical(names(alternative$estimate), names(baseline$estimate))
})

test_that("v2.4 fixed gate engine releases a regular toy replication", {
  baseline <- .v24_gate_bundle()
  for (component in c("base", "heterogeneity")) {
    for (k in seq_along(baseline[[component]]$fold_details)) {
      baseline[[component]]$fold_details[[k]]$
        derivative_refinement_protocol_exact[] <- FALSE
    }
  }
  fixed <- .v24_gate_bundle(shift = 1e-4, influence_shift = 1e-5)
  repolished <- .v24_gate_bundle(shift = 3e-4,
                                 influence_shift = 3e-5)
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600001L)
  near <- stats::setNames(
    rep(0.01, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  result <- sconjoint:::.scmix_v24_gate_engine(
    baseline, fixed, repolished, .v24_gate_fit_metrics(), near, bank,
    .v24_gate_registry())
  expect_true(result$complete)
  expect_equal(nrow(result$target_gates), 12L * 9L)
  expect_identical(result$family_gates$family_id,
                   names(sconjoint:::.scmix_v24_gate_family_specs()))
  expect_true(all(result$fit_gates$pass))
  expect_true(all(result$target_gates$pass))
  expect_true(all(result$family_gates$released))
  expected <- 3e-4 / sconjoint:::.scmix_v24_inference_view(
    baseline)$reported_se[["mean_direction"]]
  expect_equal(result$gh_movement$target[["mean_direction"]], expected,
               tolerance = 1e-10)
  expect_true(all(c("Ntrain^(-1)", "Ntrain^(-1/2)") %in%
                    names(result$ridge_sensitivities$alternatives)))
})

test_that("v2.4 diagnostics fail only the affected target family", {
  baseline <- .v24_gate_bundle()
  baseline$base$fold_details[[1L]]$derivative_refinement_error[[
    "mrs:numerator"]] <- 0.01
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600002L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  result <- sconjoint:::.scmix_v24_gate_engine(
    baseline, .v24_gate_bundle(), .v24_gate_bundle(),
    .v24_gate_fit_metrics(), near, bank, .v24_gate_registry())
  derivative <- result$target_gates[
    result$target_gates$rule_id == "derivative", ]
  expect_false(derivative$pass[derivative$target_id == "mrs"])
  expect_true(all(derivative$pass[derivative$target_id != "mrs"]))
  expect_false(result$family_gates$released[
    result$family_gates$family_id == "mrs"])
  expect_true(result$family_gates$released[
    result$family_gates$family_id == "smooth_means"])
})

test_that("target-specific DML failure withholds only the affected family", {
  failed <- .v24_gate_failed_bundle("mrs")
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600007L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  near[["mrs"]] <- Inf
  result <- sconjoint:::.scmix_v24_gate_engine(
    failed, failed, failed, .v24_gate_fit_metrics(), near, bank,
    .v24_gate_registry())
  expect_true(result$complete)
  expect_identical(result$failed_targets, "mrs")
  expect_identical(result$failed_families, "mrs")
  expect_true(all(result$fit_gates$pass))
  expect_true(is.infinite(result$target_metrics$riesz_defect[["mrs"]]))
  expect_false(any(result$target_gates$pass[
    result$target_gates$target_id == "mrs"]))
  expect_false(result$family_gates$released[
    result$family_gates$family_id == "mrs"])
  expect_true(all(result$family_gates$released[
    result$family_gates$family_id != "mrs"]))
  expect_match(result$family_gates$failed_gates[
    result$family_gates$family_id == "mrs"],
    "target_specific_dml_failure:mrs", fixed = TRUE)
  expect_identical(result$family_results$mrs$reason,
                   "target_specific_dml_failure")
})

test_that("target failures are unioned over all three integration branches", {
  baseline <- .v24_gate_bundle()
  fixed <- .v24_gate_failed_bundle("mrs")
  repolished <- .v24_gate_bundle()
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600010L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  result <- sconjoint:::.scmix_v24_gate_engine(
    baseline, fixed, repolished, .v24_gate_fit_metrics(), near, bank,
    .v24_gate_registry())
  expect_identical(result$failed_targets, "mrs")
  expect_false(result$branch_family_results$fixed31$mrs$complete)
  expect_true(result$branch_family_results$baseline$mrs$complete)
  expect_true(is.infinite(result$target_metrics$integration_target[["mrs"]]))
  expect_true(all(vapply(result$target_metrics, function(metric)
    is.infinite(metric[["mrs"]]), logical(1L))))
  expect_false(any(result$target_gates$pass[
    result$target_gates$target_id == "mrs"]))
  expect_false(result$family_gates$released[
    result$family_gates$family_id == "mrs"])
  expect_true(all(result$family_gates$released[
    result$family_gates$family_id != "mrs"]))
})

test_that("absent failed heterogeneity does not become a common SVD failure", {
  failed <- .v24_gate_failed_bundle("heterogeneity")
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600008L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  near[failed$failed_targets] <- Inf
  result <- sconjoint:::.scmix_v24_gate_engine(
    failed, failed, failed, .v24_gate_fit_metrics(), near, bank,
    .v24_gate_registry())
  expect_true(result$fit_gates$pass[
    result$fit_gates$rule_id == "svd_rank"])
  expect_false(result$family_gates$released[
    result$family_gates$family_id == "directional_heterogeneity"])
  expect_true(result$family_gates$released[
    result$family_gates$family_id == "smooth_means"])
  expect_error(sconjoint:::.scmix_v24_gate_engine(
    failed, failed, failed, .v24_gate_fit_metrics(),
    stats::setNames(rep(Inf, length(near)), names(near)), bank,
    .v24_gate_registry()), "infinity allowed only")
})

test_that("a finalizer error is isolated to its family", {
  baseline <- .v24_gate_bundle()
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600009L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  original <- sconjoint:::.scmix_v24_finalize_family
  local_mocked_bindings(
    .scmix_v24_finalize_family = function(family_id, ...) {
      if (identical(family_id, "mrs")) stop("toy finalizer error")
      original(family_id, ...)
    },
    .package = "sconjoint")
  result <- sconjoint:::.scmix_v24_gate_engine(
    baseline, baseline, baseline, .v24_gate_fit_metrics(), near, bank,
    .v24_gate_registry())
  expect_false(result$family_gates$released[
    result$family_gates$family_id == "mrs"])
  expect_true(result$family_gates$released[
    result$family_gates$family_id == "smooth_means"])
  expect_match(result$family_results$mrs$reason,
               "family_finalizer_failure:toy finalizer error", fixed = TRUE)
  expect_match(result$family_gates$failed_gates[
    result$family_gates$family_id == "mrs"],
    "family_finalizer_failure:toy finalizer error", fixed = TRUE)
})

test_that("share diagnostics include their auxiliary variance primitive", {
  baseline <- .v24_gate_bundle()
  variance_raw <- unname(baseline$base_label_map[[
    "preference_direction_variance"]])
  baseline$base$fold_details[[1L]]$derivative_refinement_error[[
    variance_raw]] <- 0.01
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600003L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  result <- sconjoint:::.scmix_v24_gate_engine(
    baseline, .v24_gate_bundle(), .v24_gate_bundle(),
    .v24_gate_fit_metrics(), near, bank, .v24_gate_registry())
  derivative <- result$target_gates[
    result$target_gates$rule_id == "derivative", , drop = FALSE]
  expect_false(derivative$pass[
    derivative$target_id == "preference_direction_share"])
  expect_true(derivative$pass[
    derivative$target_id == "compensating_benefit_share"])
  expect_identical(
    result$family_gates$family_id[!result$family_gates$released],
    "directional_shares")
})

test_that("active q1 covariance regularity is a common release gate", {
  baseline <- .v24_gate_bundle(active = 0.001)
  fixed <- .v24_gate_bundle(active = 0.001)
  repolished <- .v24_gate_bundle(active = 0.001)
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600004L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  result <- sconjoint:::.scmix_v24_gate_engine(
    baseline, fixed, repolished, .v24_gate_fit_metrics(), near, bank,
    .v24_gate_registry())
  regularity <- result$target_gates[
    result$target_gates$rule_id == "regularity_z", ]
  expect_true(result$active_eigenvalue_regularity_z < 1.96)
  expect_false(any(regularity$pass))
  expect_false(any(result$family_gates$released))
})

test_that("a zero or tiny share variance withholds only its target family", {
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600005L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  for (variance in c(0, 1e-6)) {
    baseline <- .v24_gate_bundle(preference_variance = variance)
    result <- sconjoint:::.scmix_v24_gate_engine(
      baseline, baseline, baseline, .v24_gate_fit_metrics(), near, bank,
      .v24_gate_registry())
    regularity <- result$target_gates[
      result$target_gates$rule_id == "regularity_z", , drop = FALSE]
    expect_false(regularity$pass[
      regularity$target_id == "preference_direction_share"])
    expect_true(regularity$pass[
      regularity$target_id == "compensating_benefit_share"])
    expect_identical(
      result$family_gates$family_id[!result$family_gates$released],
      "directional_shares")
  }
})

test_that("fit integrity uses accepted exact-state polishes only in scope", {
  baseline <- .v24_gate_bundle()
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600006L)
  near <- stats::setNames(
    rep(0, length(sconjoint:::.scmix_v24_gate_target_specs())),
    names(sconjoint:::.scmix_v24_gate_target_specs()))
  metrics <- .v24_gate_fit_metrics()
  metrics$start_surface <- data.frame(
    objective_relative_gap = c(0, 5e-7, 1e-2),
    block_kkt = c(1e-6, 1e-6, 1),
    polished = c(TRUE, TRUE, FALSE),
    polish_accepted = c(TRUE, TRUE, FALSE))
  regular <- sconjoint:::.scmix_v24_gate_engine(
    baseline, baseline, baseline, metrics, near, bank,
    .v24_gate_registry())
  expect_true(regular$fit_integrity)
  expect_true(all(regular$family_gates$released))

  for (flag in c("every_polish_accepted", "every_repolish_exact_state")) {
    invalid <- metrics
    invalid[[flag]] <- FALSE
    withheld <- sconjoint:::.scmix_v24_gate_engine(
      baseline, baseline, baseline, invalid, near, bank,
      .v24_gate_registry())
    expect_false(withheld$fit_integrity)
    expect_false(any(withheld$family_gates$released))
    expect_true(all(grepl("fit:integrity",
                          withheld$family_gates$failed_gates,
                          fixed = TRUE)))
  }
})

test_that("v2.4 gate inputs fail closed when near-optimum targets are absent", {
  baseline <- .v24_gate_bundle()
  bank <- sconjoint:::.scmix_v24_multiplier_bank(
    .v24_gate_ids(), draws = 9999L, seed = 1900600007L)
  expect_error(sconjoint:::.scmix_v24_gate_engine(
    baseline, baseline, baseline, .v24_gate_fit_metrics(),
    c(mean_direction = 0), bank, .v24_gate_registry()),
    "complete named nonnegative")
})
