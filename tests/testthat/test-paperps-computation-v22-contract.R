make_multistart_evaluations <- function(starts = c("s1", "s2", "s3")) {
  targets <- c("t1", "t2")
  grid <- expand.grid(
    fit_scope_id = "full", start_id = starts, target_id = targets,
    stringsAsFactors = FALSE)
  q <- c(s1 = 1, s2 = 1.01, s3 = 1.5)[grid$start_id]
  theta1 <- c(s1 = .2, s2 = .25, s3 = .9)
  theta2 <- c(s1 = .4, s2 = .41, s3 = .8)
  grid$state_hash <- paste0("hash-", grid$start_id)
  grid$objective <- unname(q)
  grid$objective_definition_id <- "mean-respondent-nll-plus-penalty"
  grid$polished <- TRUE
  grid$dtype <- "float64"
  grid$reported_estimate <- ifelse(
    grid$target_id == "t1", theta1[grid$start_id], theta2[grid$start_id])
  grid$standard_error <- ifelse(grid$target_id == "t1", .1, .2)
  grid$standard_error_definition_id <- "raw_one_step_respondent_influence"
  grid$analysis_signature <- "analysis-hash"
  grid$registry_hash <- "registry-hash"
  grid
}

test_that("F008 uses only polished near-optimum starts and raw-IF SE units", {
  ev <- make_multistart_evaluations()
  out <- scmix_v22_multistart_stability(
    ev, registered_targets = c("t1", "t2"),
    registered_starts = c("s1", "s2", "s3"),
    near_optimum_objective_cutoff = .02,
    multistart_target_se_cutoff = .6,
    protocol_mode = "production")

  expect_equal(unname(out$statistic_by_target[c("t1", "t2")]), c(.5, .05))
  expect_false(out$start_details$near_optimum[
    out$start_details$start_id == "s3"])
  expect_true(out$production_ready)
  expect_true(all(out$gates$state == "pass"))
  if (exists("scmix_v22_validate_gate_record", mode = "function")) {
    expect_silent(scmix_v22_validate_gate_record(
      out$gates, require_all_ids = FALSE))
  }
  expect_equal(
    out$start_details$objective_relative_gap[
      out$start_details$start_id == "s2"], .01)
})

test_that("F008 is fail closed while Step-3 cutoffs are pending", {
  ev <- make_multistart_evaluations()
  calibration <- scmix_v22_multistart_stability(
    ev, c("t1", "t2"), c("s1", "s2", "s3"),
    protocol_mode = "calibration")
  expect_true(calibration$calibration_only)
  expect_true(all(calibration$gates$state == "not_run"))
  expect_true(all(is.na(calibration$statistic_by_target)))

  production <- scmix_v22_multistart_stability(
    ev, c("t1", "t2"), c("s1", "s2", "s3"),
    protocol_mode = "production")
  expect_false(production$production_ready)
  expect_true(all(production$gates$state == "fail"))
})

test_that("F008 validates the complete frozen start and target registry", {
  ev <- make_multistart_evaluations()
  expect_error(
    scmix_v22_multistart_stability(
      ev[ev$start_id != "s3", ], c("t1", "t2"),
      c("s1", "s2", "s3")),
    "do not equal the frozen registry")
  bad <- ev
  bad$standard_error[bad$target_id == "t1"] <- 0
  expect_error(
    scmix_v22_multistart_stability(
      bad, c("t1", "t2"), c("s1", "s2", "s3")),
    "positive standard errors")
  bad <- ev
  bad$polished[1L] <- FALSE
  expect_error(
    scmix_v22_multistart_stability(
      bad, c("t1", "t2"), c("s1", "s2", "s3")),
    "completed polished state")

  bad <- ev
  bad$analysis_signature[1L] <- "different-analysis"
  expect_error(
    scmix_v22_multistart_stability(
      bad, c("t1", "t2"), c("s1", "s2", "s3")),
    "identifiers must be constant")

  bad <- ev
  bad$reported_estimate[1L] <- Inf
  expect_error(
    scmix_v22_multistart_stability(
      bad, c("t1", "t2"), c("s1", "s2", "s3")),
    "must be finite")

  bad <- ev
  bad$standard_error_definition_id[1L] <- "plugin_standard_error"
  expect_error(
    scmix_v22_multistart_stability(
      bad, c("t1", "t2"), c("s1", "s2", "s3")),
    "identifiers must be constant")
})

test_that("F008 is invariant to evaluation-row order", {
  ev <- make_multistart_evaluations()
  a <- scmix_v22_multistart_stability(
    ev, c("t1", "t2"), c("s1", "s2", "s3"), .02, .6,
    protocol_mode = "production")
  b <- scmix_v22_multistart_stability(
    ev[c(6, 1, 4, 2, 5, 3), ], c("t1", "t2"),
    c("s1", "s2", "s3"), .02, .6, protocol_mode = "production")
  expect_equal(a$statistic_by_target, b$statistic_by_target)
})

test_that("F008 is not applicable only for a one-start registry", {
  ev <- make_multistart_evaluations("s1")
  out <- scmix_v22_multistart_stability(
    ev, c("t1", "t2"), "s1",
    near_optimum_objective_cutoff = 0,
    multistart_target_se_cutoff = 0,
    protocol_mode = "production")
  expect_true(all(out$gates$state == "not_applicable"))
  expect_true(out$production_ready)
})

make_integration_evaluations <- function() {
  covariance <- list(
    full = matrix(c(1, 0, 0, 1), 2L, dimnames = list(c("a", "b"),
                                                       c("a", "b"))),
    fold_1 = matrix(c(1, 0, 0, 1), 2L,
                    dimnames = list(c("a", "b"), c("a", "b"))))
  base <- list(
    protocol_id = "paperps-inference-2026-09-05-v2.2",
    analysis_signature = "analysis-hash",
    registry_hash = "registry-hash", fold_hash = "fold-hash",
    nuisance_selection_hash = "nuisance-hash",
    objective_definition_id = "mean-respondent-nll-plus-penalty",
    score_definition_id = "scaled-full-batch-score",
    covariance_definition_id = "structural-covariance",
    influence_definition_id = "raw_one_step_respondent_influence",
    integration_rule = list(
      rule_id = "a", rule_hash = "rule-a", method = "qmc",
      nesting_supported = TRUE),
    parameter_state_hashes = c(full = "state-full", fold_1 = "state-fold"),
    objective = c(full = 2, fold_1 = 1),
    score = list(full = c(a = 3, b = 4), fold_1 = c(a = 1, b = 0)),
    covariance = covariance,
    targets = c(t1 = .2, t2 = .5), standard_errors = c(t1 = .1, t2 = .2),
    influence = cbind(p1 = c(-1, 1, -1, 1),
                      p2 = c(-2, 2, -2, 2)),
    riesz_solution_hashes = c(p1 = "riesz-a-p1", p2 = "riesz-a-p2"),
    riesz_integration_rule_hash = "rule-a",
    uses_numerical_integration = TRUE,
    integrated_target_ids = c("t1", "t2"),
    integrated_primitive_ids = c("p1", "p2"),
    respondent_id = paste0("r", 1:4), role = "level_a_production", q = 1L)
  fixed <- base
  fixed$role <- "level_b_fixed_parameters"
  fixed$integration_rule <- list(
    rule_id = "b", rule_hash = "rule-b", method = "qmc",
    nesting_supported = TRUE, nested_level_a_hash = "rule-a")
  fixed$objective <- c(full = 2.1, fold_1 = 1.2)
  fixed$score <- list(full = c(a = 6, b = 8), fold_1 = c(a = 1.2, b = 0))
  fixed$covariance <- lapply(covariance, function(x) 2 * x)
  fixed$targets <- c(t1 = .25, t2 = .48)
  fixed$standard_errors <- NULL
  fixed$influence <- sweep(base$influence, 2L, c(.5, 1), `+`)
  fixed$riesz_solution_hashes <- c(p1 = "riesz-fixed-p1",
                                   p2 = "riesz-fixed-p2")
  fixed$riesz_integration_rule_hash <- "rule-b"

  repolished <- base
  repolished$role <- "level_b_repolished"
  repolished$integration_rule <- fixed$integration_rule
  repolished$initial_parameter_state_hashes <- base$parameter_state_hashes
  repolished$parameter_state_hashes <- c(full = "state-full-b",
                                         fold_1 = "state-fold-b")
  repolished$repolish_completed <- TRUE
  repolished$objective <- c(full = 2.3, fold_1 = 1.1)
  repolished$score <- list(full = c(a = 3, b = 4.5),
                           fold_1 = c(a = 1.1, b = 0))
  repolished$covariance <- lapply(covariance, function(x) 1.5 * x)
  repolished$targets <- c(t1 = .3, t2 = .56)
  repolished$standard_errors <- NULL
  repolished$influence <- sweep(base$influence, 2L, c(.2, 2), `+`)
  repolished$riesz_solution_hashes <- c(p1 = "riesz-polished-p1",
                                        p2 = "riesz-polished-p2")
  repolished$riesz_integration_rule_hash <- "rule-b"
  list(a = base, fixed = fixed, repolished = repolished)
}

test_that("two-branch integration refinement uses the worse discrepancy", {
  x <- make_integration_evaluations()
  calibration <- list(
    integration_target_se_cutoff = 2,
    integration_if_l2_cutoff = 2,
    integration_objective_relative_cutoff = .25,
    integration_score_relative_cutoff = 2,
    integration_covariance_relative_cutoff = 2)
  out <- scmix_v22_integration_compare(
    x$a, x$fixed, x$repolished,
    target_primitives = list(t1 = "p1", t2 = c("p1", "p2")),
    calibration = calibration, protocol_mode = "production")

  expect_equal(unname(out$worse_of_metrics$target[c("t1", "t2")]),
               c(1, .3))
  expect_equal(unname(out$worse_of_metrics$influence[c("p1", "p2")]),
               c(.5, 1))
  expect_equal(out$influence_statistic_by_target, c(t1 = .5, t2 = 1))
  expect_equal(out$maximum_fit_metrics[["objective"]], .2)
  expect_equal(out$maximum_fit_metrics[["score"]], 1)
  expect_equal(out$maximum_fit_metrics[["covariance"]], 1)
  expect_true(out$production_ready)
  expect_true(all(out$gates$state == "pass"))
  if (exists("scmix_v22_validate_gate_record", mode = "function")) {
    expect_silent(scmix_v22_validate_gate_record(
      out$gates, require_all_ids = FALSE))
  }
  expect_equal(sum(out$gates$gate_id == "I001"), 2L)
  expect_equal(sum(out$gates$gate_id == "I002"), 2L)
})

test_that("two-branch integration evidence fails closed before calibration", {
  x <- make_integration_evaluations()
  cal <- scmix_v22_integration_compare(
    x$a, x$fixed, x$repolished,
    list(t1 = "p1", t2 = c("p1", "p2")),
    protocol_mode = "calibration")
  expect_true(cal$calibration_only)
  expect_true(all(cal$gates$state == "not_run"))
  prod <- scmix_v22_integration_compare(
    x$a, x$fixed, x$repolished,
    list(t1 = "p1", t2 = c("p1", "p2")),
    protocol_mode = "production")
  expect_false(prod$production_ready)
  expect_true(all(prod$gates$state == "fail"))
})

test_that("integration refinement rejects missing frozen branch provenance", {
  x <- make_integration_evaluations()
  bad_fixed <- x$fixed
  bad_fixed$parameter_state_hashes[["full"]] <- "other-state"
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_fixed, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "fixed-parameter branch")

  bad_repolished <- x$repolished
  bad_repolished$initial_parameter_state_hashes[["fold_1"]] <- "other-state"
  expect_error(
    scmix_v22_integration_compare(
      x$a, x$fixed, bad_repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "did not start")

  bad_nested <- x$fixed
  bad_nested$integration_rule$nested_level_a_hash <- "wrong-rule"
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_nested, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "required nesting")

  bad_signature <- x$fixed
  bad_signature$analysis_signature <- "different-analysis"
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_signature, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "analysis_signature")

  bad_order <- x$fixed
  bad_order$respondent_id <- rev(bad_order$respondent_id)
  bad_order$influence <- bad_order$influence[4:1, , drop = FALSE]
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_order, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "preserve scopes, targets, primitives, or respondents")

  bad_finite <- x$fixed
  bad_finite$score$full[1L] <- Inf
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_finite, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "score vector must be finite")

  bad_influence <- x$fixed
  bad_influence$influence_definition_id <- "plugin_influence"
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_influence, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "raw one-step respondent influence")

  bad_riesz <- x$fixed
  bad_riesz$riesz_integration_rule_hash <- "rule-a"
  expect_error(
    scmix_v22_integration_compare(
      x$a, bad_riesz, x$repolished,
      list(t1 = "p1", t2 = c("p1", "p2"))),
    "Riesz evidence was not recomputed")

  expect_error(
    scmix_v22_integration_compare(
      x$a, x$fixed, x$repolished,
      list(t1 = "p1", t2 = "p1")),
    "use every influence primitive")
})

test_that("integration gates honor conditional applicability", {
  x <- make_integration_evaluations()
  for (nm in names(x)) {
    x[[nm]]$q <- 0L
    x[[nm]]$uses_numerical_integration <- FALSE
    x[[nm]]$integrated_target_ids <- character()
    x[[nm]]$integrated_primitive_ids <- character()
  }
  out <- scmix_v22_integration_compare(
    x$a, x$fixed, x$repolished,
    list(t1 = "p1", t2 = c("p1", "p2")),
    calibration = list(
      integration_target_se_cutoff = 0,
      integration_if_l2_cutoff = 0,
      integration_objective_relative_cutoff = 0,
      integration_score_relative_cutoff = 0,
      integration_covariance_relative_cutoff = 0),
    protocol_mode = "production")
  expect_true(all(out$gates$state == "not_applicable"))
  expect_true(out$production_ready)
})

test_that("integration callback wrapper executes each required branch once", {
  x <- make_integration_evaluations()
  calls <- new.env(parent = emptyenv())
  calls$fixed <- calls$polish <- calls$evaluate <- 0L
  out <- scmix_v22_integration_refinement(
    level_a = x$a, level_b_rule = x$fixed$integration_rule,
    fixed_parameter_evaluator = function(level_a, level_b_rule) {
      calls$fixed <- calls$fixed + 1L
      x$fixed
    },
    repolisher = function(level_a, level_b_rule) {
      calls$polish <- calls$polish + 1L
      list(fit = "polished")
    },
    repolished_evaluator = function(repolished_fit, level_b_rule) {
      calls$evaluate <- calls$evaluate + 1L
      x$repolished
    },
    target_primitives = list(t1 = "p1", t2 = c("p1", "p2")),
    protocol_mode = "calibration")
  expect_equal(c(calls$fixed, calls$polish, calls$evaluate), c(1L, 1L, 1L))
  expect_equal(out$callback_contract$repolisher_calls, 1L)
})
