.v22_protocol <- "paperps-inference-2026-09-05-v2.2"

.v22_calibration_names <- c(
  "subgroup_interior_margin",
  "directional_heterogeneity_regularity_margin",
  "mean_tradeoff_denominator_margin",
  "chart_discrepancy_cutoff", "nonlinear_remainder_cutoff",
  "interior_distance_cutoff", "kkt_cutoff",
  "integration_target_se_cutoff", "integration_if_l2_cutoff",
  "integration_objective_relative_cutoff",
  "integration_score_relative_cutoff",
  "integration_covariance_relative_cutoff",
  "near_optimum_objective_cutoff", "multistart_target_se_cutoff",
  "nested_fit_relative_cutoff", "loading_probability_invariance_cutoff",
  "active_eigenvalue_standardized_cutoff",
  "generalized_information_margin",
  "directional_variance_standardized_cutoff",
  "total_heterogeneity_standardized_cutoff",
  "denominator_standardized_cutoff", "derivative_cutoff",
  "null_space_materiality_cutoff", "numerical_rank_tolerance",
  "defect_cutoff", "ridge_cutoff")

.v22_calibration_source_hash <- "cal-source"
.v22_calibration_values <- function() {
  constants <- stats::setNames(rep(10, length(.v22_calibration_names)),
                               .v22_calibration_names)
  constants[c("near_optimum_objective_cutoff",
              "nested_fit_relative_cutoff")] <- .1
  constants[["multistart_target_se_cutoff"]] <- 1
  constants
}
.v22_approved_calibration <- list(
  protocol_id = .v22_protocol,
  status = "frozen_for_production",
  calibration_hash = scmix_v22_calibration_hash(
    .v22_calibration_values(), .v22_calibration_source_hash),
  source_hash = .v22_calibration_source_hash,
  manifest_fingerprint = "synthetic-package-manifest-fingerprint")
testthat::local_mocked_bindings(
  .scmix_v22_approved_calibration_manifest = function()
    .v22_approved_calibration,
  .package = "sconjoint", .env = testthat:::teardown_env())

.v22_sign <- function(x, class_name) {
  sconjoint:::.scmix_v22_comp_sign(x, class_name)
}

.v22_spec <- function(id, primitives, family_targets = c("t1", "t2"),
                      family_primitives = c("p1", "p2")) {
  .v22_sign(list(
    protocol_id = .v22_protocol, target_id = id,
    target_family_id = "family", primitive_registry_ids = primitives,
    family_target_ids = family_targets,
    family_primitive_registry_ids = family_primitives),
    "scmix_v22_target_spec")
}

.v22_analysis <- function(signature, specification_id, integration,
                          fit_registry, constants, source_hash = "cal-source") {
  constants <- as.list(constants[sort(names(constants), method = "radix")])
  cal_hash <- sconjoint:::.scmix_v22_comp_fingerprint(list(
    protocol_id = .v22_protocol, source_hash = source_hash,
    constants = constants))
  .v22_sign(list(
    protocol_id = .v22_protocol, analysis_signature = signature,
    application_id = "app", specification_id = specification_id,
    specification_role = "primary", respondent_sample_rule = "all",
    completion_rule = "complete", missing_data_rule = "prespecified",
    outer_folds = list(fold_1 = c("r1", "r2")),
    inner_folds = list(fold_1 = list(c("r1"), c("r2"))),
    preprocessing_rule = "training-only",
    integration_rule = integration$fingerprint,
    optimizer_starts = fit_registry, learner_grid = list(hidden = 4L),
    ridge_grid = c("0", "n_k^-1", "n_k^-3/4", "n_k^-1/2"),
    seeds = list(folds = 1L), code_commit = "code",
    data_hash = "data", design_hash = "design",
    calibration_hash = cal_hash), "scmix_v22_analysis_contract")
}

.v22_calibration <- function(analysis, constants,
                             source_hash = .v22_calibration_source_hash) {
  constants <- as.list(constants[sort(names(constants), method = "radix")])
  .v22_sign(list(
    protocol_id = .v22_protocol,
    analysis_signature = analysis$analysis_signature,
    analysis_contract_fingerprint = analysis$fingerprint,
    calibration_hash = analysis$calibration_hash,
    source_hash = source_hash, constants = constants, complete = TRUE,
    approval_manifest = .v22_approved_calibration,
    provenance = "design-matched simulation fixture"),
    "scmix_v22_calibration_artifact")
}

.v22_bundle <- function(specification_id = "full_q1", q = 1L,
                        p = 3L, score_p = 5L,
                        integrated_targets = "t1",
                        integrated_primitives = "p2",
                        n_starts = 2L) {
  registry <- list(.v22_spec("t1", "p1"),
                   .v22_spec("t2", c("p1", "p2")))
  covariance_ids <- paste0("beta", seq_len(p))
  score_ids <- paste0("u", seq_len(score_p))
  structural <- scmix_v22_structural_parameter_contract(
    "app", specification_id, q, covariance_ids, score_ids)
  nodes_a <- matrix(c(-1, 1), ncol = q)
  nodes_b <- rbind(nodes_a, matrix(c(0, 2), ncol = q))
  weights_a <- rep(1 / nrow(nodes_a), nrow(nodes_a))
  weights_b <- rep(1 / nrow(nodes_b), nrow(nodes_b))
  rule_a <- scmix_v22_integration_rule(
    paste0(specification_id, "-a"), "nested_qmc", "a", nodes_a,
    weights_a, nesting_supported = TRUE)
  rule_b <- scmix_v22_integration_rule(
    paste0(specification_id, "-b"), "nested_qmc", "b", nodes_b,
    weights_b, nesting_supported = TRUE)
  integration <- scmix_v22_integration_contract(
    structural, registry, uses_numerical_integration = TRUE,
    rule_a, rule_b, integrated_targets, integrated_primitives)
  scopes <- c(paste(specification_id, "full", sep = "::"),
              paste(specification_id, "outer", "fold_1", sep = "::"))
  starts <- expand.grid(
    fit_scope_id = scopes,
    start_id = sprintf("start_%03d", seq_len(n_starts)),
    stringsAsFactors = FALSE)
  starts$seed <- seq_len(nrow(starts))
  fit_registry <- scmix_v22_fit_scope_registry(
    "app", specification_id, paste0(specification_id, "-fit"), "fold_1",
    starts, integration)
  constants <- .v22_calibration_values()
  analysis <- .v22_analysis(
    paste0("analysis-", specification_id), specification_id,
    integration, fit_registry, constants)
  calibration <- .v22_calibration(analysis, constants)
  list(registry = registry, structural = structural, rule_a = rule_a,
       rule_b = rule_b, integration = integration,
       fit_registry = fit_registry, analysis = analysis,
       calibration = calibration, constants = constants, scopes = scopes,
       covariance_ids = covariance_ids, score_ids = score_ids,
       nodes_a = nodes_a, nodes_b = nodes_b,
       weights_a = weights_a, weights_b = weights_b)
}

.v22_kkt <- function() data.frame(
  block = c("position_effect", "unpenalized_mean_reference",
            "centered_mean_deviation", "horizontal_loadings",
            "all_structural_coordinates"),
  applicable = TRUE, dimension = c(1L, 3L, 4L, 3L, 11L),
  projected_kkt_rms = rep(1e-5, 5L),
  stringsAsFactors = FALSE)

.v22_start <- function(bundle, scope, start_id, selected,
                       objective, role = "level_a_production_start",
                       initial_state_hash = NULL,
                       optimizer_protocol_mode = "production") {
  row <- bundle$fit_registry$starts[
    bundle$fit_registry$starts$fit_scope_id == scope &
      bundle$fit_registry$starts$start_id == start_id, , drop = FALSE]
  A <- matrix(seq_len(length(bundle$covariance_ids) *
                        max(1L, bundle$structural$q)) / 100,
              nrow = length(bundle$covariance_ids),
              ncol = max(1L, bundle$structural$q))
  if (bundle$structural$q == 0L) A <- A[, FALSE, drop = FALSE]
  network_state <- list(
    analysis_signature = bundle$analysis$analysis_signature,
    architecture = list(p = length(bundle$covariance_ids),
                        q = bundle$structural$q),
    coefficient_names = bundle$covariance_ids,
    integration_grid = if (role == "level_a_production_start") {
      list(U = bundle$nodes_a, w = bundle$weights_a)
    } else list(U = bundle$nodes_b, w = bundle$weights_b),
    state_dict = list(weight = matrix(match(start_id,
                                             unique(bundle$fit_registry$starts$start_id)),
                                      1L, 1L) +
                        if (role == "level_b_repolish") 100 else 0))
  kappa <- 0
  state_hash <- sconjoint:::.scmix_v22_comp_state_hash(
    network_state$state_dict, A, kappa)
  kkt <- .v22_kkt()
  gate_states <- if (optimizer_protocol_mode == "production") {
    rep("pass", 6L)
  } else c(rep("pass", 4L), "not_run", "pass")
  gate_evidence <- stats::setNames(lapply(seq_len(6L), function(j)
    list(state = gate_states[j], observed = if (j == 5L) 1e-5 else
      if (j == 6L) FALSE else TRUE,
      threshold = if (j == 5L) bundle$constants[["kkt_cutoff"]] else TRUE)),
    paste0("F00", seq_len(6L)))
  artifact <- list(
    start_id = start_id, state_hash = state_hash, selected = selected,
    mean_objective = objective,
    objective_definition_id = paste0(
      "mean_negative_complete_sequence_loglik_per_respondent_",
      "plus_declared_penalty_in_nats"),
    dtype = "float64", polished = TRUE,
    protocol_mode = optimizer_protocol_mode,
    calibration_hash = bundle$analysis$calibration_hash,
    optimizer_evidence = list(
      protocol_id = .v22_protocol,
      objective = list(before_polish = objective + .05,
                       after_polish = objective, returned = objective,
                       nonworsening_numerical_allowance = 1e-12,
                       definition_id = paste0(
                         "mean_negative_complete_sequence_loglik_per_respondent_",
                         "plus_declared_penalty_in_nats")),
      polish = list(method = "full_batch_lbfgs", line_search = "strong_wolfe",
                    method_id = "full_batch_lbfgs_strong_wolfe",
                    line_search_status = "optimizer_returned_strong_wolfe",
                    completed = TRUE, accepted = TRUE,
                    projection_changed = FALSE, nonworsening = TRUE,
                    same_objective_all_stages = TRUE,
                    returned_matches_accepted_objective = TRUE),
      kkt = list(returned = list(all_finite = TRUE, maximum = 1e-5,
                                 blocks = kkt))),
    v22_optimization_status = list(
      optimization_gate_pass = optimizer_protocol_mode == "production",
      precalibration_fit_valid = TRUE, gate_evidence = gate_evidence),
    network_state = network_state,
    A_optimization = A, kappa = kappa)
  scmix_v22_start_optimizer_evidence(
    bundle$analysis, bundle$registry, bundle$integration,
    bundle$structural,
    if (role == "level_a_production_start") bundle$rule_a else bundle$rule_b,
    scope, row$seed, artifact, role = role,
    initial_state_hash = initial_state_hash,
    calibration_artifact = if (optimizer_protocol_mode == "production")
      bundle$calibration else NULL)
}

.v22_all_starts <- function(bundle, base_objective = 1,
                            protocol_mode = "production") {
  unlist(lapply(seq_along(bundle$scopes), function(j) {
    lapply(seq_len(nrow(bundle$fit_registry$starts) /
                     length(bundle$scopes)), function(s) {
      .v22_start(bundle, bundle$scopes[j], sprintf("start_%03d", s),
                 selected = s == 1L,
                 objective = base_objective + .01 * (j - 1L) + .02 * (s - 1L),
                 optimizer_protocol_mode = protocol_mode)
    })
  }), recursive = FALSE)
}

.v22_riesz <- function(bundle, rule, states) {
  lapply(c("p1", "p2"), function(id) {
    solution <- lapply(seq_along(bundle$scopes), function(j)
      stats::setNames(seq_along(bundle$score_ids) /
                        (10 + j + match(id, c("p1", "p2"))),
                      bundle$score_ids))
    names(solution) <- bundle$scopes
    scmix_v22_integration_riesz_solution(
      bundle$analysis, bundle$registry, bundle$integration,
      bundle$structural, id, states, solution, rule,
      code_hash = "riesz-code", provenance = "fixture recomputation")
  })
}

.v22_target_evidence <- function(bundle, rule, states,
                                 targets = c(t1 = .2, t2 = .5),
                                 influence_scale = 1) {
  influence <- cbind(p1 = c(-1, 1, -1, 1),
                     p2 = c(-2, 2, -2, 2)) * influence_scale
  scmix_v22_integration_target_evidence(
    bundle$analysis, bundle$registry, bundle$integration,
    bundle$structural, rule, states, targets,
    c(t1 = .1, t2 = .2), influence, paste0("r", 1:4),
    .v22_riesz(bundle, rule, states), "target-code", "fixture target run")
}

.v22_eval <- function(bundle, role, states, targets,
                      objective_shift = 0, score_scale = 1,
                      covariance_scale = 1, influence_scale = 1,
                      optimizer_evidence = NULL,
                      protocol_mode = "production") {
  rule <- if (role == "level_a_production") bundle$rule_a else bundle$rule_b
  objective <- stats::setNames(c(2, 1) + objective_shift, bundle$scopes)
  if (!is.null(optimizer_evidence)) {
    optimizer_evidence <- optimizer_evidence[match(
      bundle$scopes,
      vapply(optimizer_evidence, `[[`, character(1L), "fit_scope_id"))]
    objective <- stats::setNames(vapply(
      optimizer_evidence, `[[`, numeric(1L), "objective"), bundle$scopes)
  }
  score <- lapply(seq_along(bundle$scopes), function(j)
    stats::setNames(score_scale * seq_along(bundle$score_ids),
                    bundle$score_ids))
  names(score) <- bundle$scopes
  covariance <- lapply(bundle$scopes, function(x) {
    z <- diag(covariance_scale, length(bundle$covariance_ids))
    dimnames(z) <- list(bundle$covariance_ids, bundle$covariance_ids)
    z
  })
  names(covariance) <- bundle$scopes
  target_evidence <- .v22_target_evidence(
    bundle, rule, states, targets, influence_scale)
  scmix_v22_integration_evaluation(
    bundle$analysis, bundle$registry, bundle$integration,
    bundle$structural, role, objective, score, covariance,
    target_evidence, optimizer_evidence,
    if (protocol_mode == "production") bundle$calibration else NULL,
    protocol_mode)
}

.v22_three_branches <- function(bundle = .v22_bundle(),
                                protocol_mode = "production") {
  starts <- .v22_all_starts(bundle, protocol_mode = protocol_mode)
  selected <- starts[vapply(starts, `[[`, logical(1L), "selected")]
  states_a <- stats::setNames(vapply(
    selected, `[[`, character(1L), "state_hash"), bundle$scopes)
  a <- .v22_eval(bundle, "level_a_production", states_a,
                 c(t1 = .2, t2 = .5), optimizer_evidence = selected,
                 protocol_mode = protocol_mode)
  fixed <- .v22_eval(bundle, "level_b_fixed_parameters", states_a,
                     c(t1 = .25, t2 = .48), objective_shift = c(.1, .2),
                     score_scale = 2, covariance_scale = 2,
                     influence_scale = 1.2, protocol_mode = protocol_mode)
  b_selected <- lapply(seq_along(bundle$scopes), function(j) {
    .v22_start(
      bundle, bundle$scopes[j], selected[[j]]$start_id, TRUE,
      objective = c(2.3, 1.1)[j], role = "level_b_repolish",
      initial_state_hash = states_a[j],
      optimizer_protocol_mode = protocol_mode)
  })
  states_b <- stats::setNames(vapply(
    b_selected, `[[`, character(1L), "state_hash"), bundle$scopes)
  repolished <- .v22_eval(
    bundle, "level_b_repolished", states_b, c(t1 = .3, t2 = .56),
    objective_shift = c(.3, .1), score_scale = 1.1,
    covariance_scale = 1.5, influence_scale = .8,
    optimizer_evidence = b_selected, protocol_mode = protocol_mode)
  list(bundle = bundle, starts = starts, a = a, fixed = fixed,
       repolished = repolished)
}

test_that("exact signed integration rules enforce higher resolution and nesting", {
  b <- .v22_bundle()
  expect_true(b$integration$exact_nesting_verified)
  expect_gt(b$rule_b$resolution, b$rule_a$resolution)

  bad_b <- scmix_v22_integration_rule(
    "bad-b", "nested_qmc", "b", matrix(c(3, 4, 5), ncol = 1),
    rep(1 / 3, 3), nesting_supported = TRUE)
  expect_error(scmix_v22_integration_contract(
    b$structural, b$registry, TRUE, b$rule_a, bad_b, "t1", "p2"),
    "exactly contain")

  tampered <- b$rule_b
  tampered$node_keys[1] <- "changed"
  expect_error(scmix_v22_integration_contract(
    b$structural, b$registry, TRUE, b$rule_a, tampered, "t1", "p2"),
    "intact signed")
})

test_that("integration evaluation binds all scopes and uses p by p covariance", {
  x <- .v22_three_branches()
  expect_equal(nrow(x$a$covariance[[1]]), 3L)
  expect_equal(x$a$q, 1L)
  expect_true(sconjoint:::.scmix_v22_comp_intact(
    x$a, "scmix_v22_integration_evaluation"))

  bad_cov <- x$a$covariance
  bad_cov[[1]] <- matrix(1, 1, 1, dimnames = list("u", "u"))
  expect_error(scmix_v22_integration_evaluation(
    x$bundle$analysis, x$bundle$registry, x$bundle$integration,
    x$bundle$structural, "level_a_production", x$a$objective,
    x$a$score, bad_cov, x$a$target_evidence,
    x$starts[vapply(x$starts, `[[`, logical(1L), "selected")],
    x$bundle$calibration, "production"), "p-by-p")

  missing_scope <- x$a$objective[-1]
  expect_error(scmix_v22_integration_evaluation(
    x$bundle$analysis, x$bundle$registry, x$bundle$integration,
    x$bundle$structural, "level_a_production", missing_scope,
    x$a$score, x$a$covariance, x$a$target_evidence,
    x$starts[vapply(x$starts, `[[`, logical(1L), "selected")],
    x$bundle$calibration, "production"), "full fit and every")

  wrong_objective <- x$a$objective
  wrong_objective[[1]] <- wrong_objective[[1]] + 1
  expect_error(scmix_v22_integration_evaluation(
    x$bundle$analysis, x$bundle$registry, x$bundle$integration,
    x$bundle$structural, "level_a_production", wrong_objective,
    x$a$score, x$a$covariance, x$a$target_evidence,
    x$starts[vapply(x$starts, `[[`, logical(1L), "selected")],
    x$bundle$calibration, "production"), "must equal")
})

test_that("integration target evidence rejects zero-variance IF and stale Riesz", {
  b <- .v22_bundle()
  starts <- .v22_all_starts(b)
  selected <- starts[vapply(starts, `[[`, logical(1L), "selected")]
  states <- stats::setNames(vapply(selected, `[[`, character(1L),
                                   "state_hash"), b$scopes)
  zero_if <- matrix(0, 4, 2,
                    dimnames = list(NULL, c("p1", "p2")))
  expect_error(scmix_v22_integration_target_evidence(
    b$analysis, b$registry, b$integration, b$structural, b$rule_a,
    states, c(t1 = .2, t2 = .5), c(t1 = .1, t2 = .2), zero_if,
    paste0("r", 1:4), .v22_riesz(b, b$rule_a, states),
    "code", "provenance"), "positive respondent influence SD")

  stale <- .v22_riesz(b, b$rule_a, states)
  expect_error(scmix_v22_integration_target_evidence(
    b$analysis, b$registry, b$integration, b$structural, b$rule_b,
    states, c(t1 = .2, t2 = .5), c(t1 = .1, t2 = .2),
    cbind(p1 = c(-1, 1, -1, 1), p2 = c(-2, 2, -2, 2)),
    paste0("r", 1:4), stale, "code", "provenance"),
    "not bound")
})

test_that("two-branch refinement uses the worse branch and OR applicability", {
  x <- .v22_three_branches()
  out <- scmix_v22_integration_compare(
    x$a, x$fixed, x$repolished,
    x$bundle$analysis, x$bundle$registry, x$bundle$integration,
    x$bundle$structural, x$bundle$calibration, "production")
  expect_equal(unname(out$worse_of_metrics$target), c(1, .3),
               tolerance = 1e-12)
  expect_equal(out$influence_statistic_by_target[["t1"]], sqrt(.03),
               tolerance = 1e-12)
  expect_equal(out$influence_statistic_by_target[["t2"]], sqrt(.03),
               tolerance = 1e-12)
  expect_true(all(out$gates$state == "pass"))
  expect_true(out$selected_gate_subset_passed)
  expect_equal(sum(out$gates$gate_id == "I001"), 2L)
  expect_equal(sum(out$gates$gate_id == "I002"), 2L)
  authenticated <- scmix_v22_validate_computation_artifact(
    out, x$bundle$analysis, x$bundle$registry, x$bundle$calibration)
  expect_setequal(unique(authenticated$gate_id), sprintf("I%03d", 1:5))
  expect_error(scmix_v22_validate_computation_artifact(
    out$gates, x$bundle$analysis, x$bundle$registry,
    x$bundle$calibration), "Unknown, damaged, or unsigned")
})

test_that("production integration fails closed without exact calibration", {
  b <- .v22_bundle()
  starts <- .v22_all_starts(b)
  selected <- starts[vapply(starts, `[[`, logical(1L), "selected")]
  states <- stats::setNames(vapply(selected, `[[`, character(1L),
                                   "state_hash"), b$scopes)
  target <- .v22_target_evidence(b, b$rule_a, states)
  score <- stats::setNames(lapply(b$scopes, function(z)
    stats::setNames(rep(0, length(b$score_ids)), b$score_ids)), b$scopes)
  covariance <- stats::setNames(lapply(b$scopes, function(z) {
    m <- diag(length(b$covariance_ids));
    dimnames(m) <- list(b$covariance_ids, b$covariance_ids); m
  }), b$scopes)
  expect_error(scmix_v22_integration_evaluation(
    b$analysis, b$registry, b$integration, b$structural,
    "level_a_production", stats::setNames(c(1, 1), b$scopes),
    score, covariance, target, selected,
    calibration_artifact = NULL, protocol_mode = "production"),
    "requires the intact Step-3")

  forged <- b$calibration
  forged$constants$integration_target_se_cutoff <- 1e6
  expect_error(scmix_v22_integration_evaluation(
    b$analysis, b$registry, b$integration, b$structural,
    "level_a_production", stats::setNames(c(1, 1), b$scopes),
    score, covariance, target, selected, forged, "production"),
    "not intact")
})

test_that("F008 consumes every intact optimizer state and recomputation", {
  b <- .v22_bundle()
  starts <- .v22_all_starts(b)
  evaluations <- lapply(starts, function(x) {
    scmix_v22_target_recomputation(
      x, b$analysis, b$registry, b$integration, b$structural,
      c(t1 = .2 + ifelse(x$start_id == "start_002", .05, 0),
        t2 = .4 + ifelse(x$start_id == "start_002", .01, 0)),
      c(t1 = .1, t2 = .2))
  })
  out <- scmix_v22_multistart_stability(
    evaluations, starts, b$analysis, b$registry, b$integration,
    b$structural, b$calibration, "production")
  expect_equal(unname(out$statistic_by_target), c(.5, .05),
               tolerance = 1e-12)
  expect_true(all(out$gates$state == "pass"))
  expect_true(out$selected_gate_subset_passed)
  expect_equal(nrow(scmix_v22_validate_computation_artifact(
    out, b$analysis, b$registry, b$calibration)), 2L)

  expect_error(scmix_v22_multistart_stability(
    evaluations[-1], starts, b$analysis, b$registry, b$integration,
    b$structural, b$calibration, "production"), "cover every")
  damaged <- starts
  damaged[[1]]$objective <- -99
  expect_error(scmix_v22_multistart_stability(
    evaluations, damaged, b$analysis, b$registry, b$integration,
    b$structural, b$calibration, "production"), "intact")

  source <- starts[[1]]$source_optimizer_artifact
  source$state_hash <- "self-asserted-state"
  expect_error(scmix_v22_start_optimizer_evidence(
    b$analysis, b$registry, b$integration, b$structural, b$rule_a,
    starts[[1]]$fit_scope_id, starts[[1]]$seed, source),
    "source optimizer state")
  source <- starts[[1]]$source_optimizer_artifact
  source$calibration_hash <- "wrong-calibration"
  expect_error(scmix_v22_start_optimizer_evidence(
    b$analysis, b$registry, b$integration, b$structural, b$rule_a,
    starts[[1]]$fit_scope_id, starts[[1]]$seed, source),
    "source optimizer state")

  source <- starts[[1]]$source_optimizer_artifact
  expect_error(scmix_v22_start_optimizer_evidence(
    b$analysis, b$registry, b$integration, b$structural, b$rule_a,
    starts[[1]]$fit_scope_id, starts[[1]]$seed, source),
    "valid v2.2")
  source$v22_optimization_status$gate_evidence$F006$observed <- TRUE
  expect_error(scmix_v22_start_optimizer_evidence(
    b$analysis, b$registry, b$integration, b$structural, b$rule_a,
    starts[[1]]$fit_scope_id, starts[[1]]$seed, source,
    calibration_artifact = b$calibration), "valid v2.2")
})

test_that("F008 is not applicable only for the signed one-start registry", {
  b <- .v22_bundle(n_starts = 1L)
  starts <- .v22_all_starts(b)
  evaluations <- lapply(starts, function(x)
    scmix_v22_target_recomputation(
      x, b$analysis, b$registry, b$integration, b$structural,
      c(t1 = .2, t2 = .4), c(t1 = .1, t2 = .2)))
  out <- scmix_v22_multistart_stability(
    evaluations, starts, b$analysis, b$registry, b$integration,
    b$structural, b$calibration, "production")
  expect_true(all(out$gates$state == "not_applicable"))
  expect_true(out$selected_gate_subset_passed)
})

test_that("F007 covers the full fit and every exact paired outer fold", {
  restricted <- .v22_bundle("restricted_q1", score_p = 3L)
  full <- .v22_bundle("full_q1", score_p = 5L)
  E <- matrix(0, length(full$score_ids), length(restricted$score_ids),
              dimnames = list(full$score_ids, restricted$score_ids))
  E[seq_along(restricted$score_ids), ] <- diag(
    length(restricted$score_ids))
  nested <- scmix_v22_nested_specification_contract(
    restricted$analysis, full$analysis, restricted$structural,
    full$structural, E, stats::setNames(rep(0, length(full$score_ids)),
                                        full$score_ids),
    "verified-identical-probability-and-penalty-test")
  re <- .v22_all_starts(restricted, 1.1)
  re <- re[vapply(re, `[[`, logical(1L), "selected")]
  fe <- .v22_all_starts(full, 1.0)
  fe <- fe[vapply(fe, `[[`, logical(1L), "selected")]
  out <- scmix_v22_nested_fit_stability(
    nested, restricted$analysis, full$analysis,
    restricted$registry, full$registry,
    restricted$integration, full$integration,
    restricted$structural, full$structural,
    re, fe, restricted$calibration, full$calibration, "production")
  expect_equal(nrow(out$objective_by_scope), 2L)
  expect_equal(out$statistic, 0)
  expect_identical(out$gate$state, "pass")
  expect_identical(
    scmix_v22_validate_computation_artifact(
      out, full$analysis, full$registry, full$calibration)$gate_id,
    "F007")
  alternate_registry <- list(
    .v22_spec("t1", "p1", family_primitives = "p1"),
    .v22_spec("t2", "p1", family_primitives = "p1"))
  expect_error(scmix_v22_validate_computation_artifact(
    out, full$analysis, alternate_registry, full$calibration),
    "not linked")

  expect_error(scmix_v22_nested_fit_stability(
    nested, restricted$analysis, full$analysis,
    restricted$registry, full$registry,
    restricted$integration, full$integration,
    restricted$structural, full$structural,
    re[-1], fe, restricted$calibration, full$calibration, "production"),
    "per nested fit scope")
})

test_that("calibration mode records pending gates without certifying", {
  x <- .v22_three_branches(protocol_mode = "calibration")
  out <- scmix_v22_integration_compare(
    x$a, x$fixed, x$repolished,
    x$bundle$analysis, x$bundle$registry, x$bundle$integration,
    x$bundle$structural, calibration_artifact = NULL,
    protocol_mode = "calibration")
  expect_true(out$calibration_only)
  expect_true(all(out$gates$state == "not_run"))
  expect_false(out$selected_gate_subset_passed)
})

test_that("integration callback wrapper preserves its final fingerprint", {
  x <- .v22_three_branches(protocol_mode = "calibration")
  out <- scmix_v22_integration_refinement(
    x$a, x$bundle$rule_b,
    function(level_a, level_b_rule) x$fixed,
    function(level_a, level_b_rule) list(state = "polished"),
    function(repolished_fit, level_b_rule) x$repolished,
    analysis_contract = x$bundle$analysis,
    registry = x$bundle$registry,
    integration_contract = x$bundle$integration,
    structural_parameter_contract = x$bundle$structural,
    protocol_mode = "calibration")
  expect_true(sconjoint:::.scmix_v22_comp_intact(
    out, "scmix_v22_integration_refinement"))
  expect_equal(out$callback_contract$repolisher_calls, 1L)
})
