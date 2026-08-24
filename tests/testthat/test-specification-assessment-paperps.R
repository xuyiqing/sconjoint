test_that("protocol benchmark retains respondents with no realized match", {
  dx <- matrix(c(1, 1, 1, 1, 0), ncol = 1,
               dimnames = list(NULL, "x"))
  y <- c(1, 1, 1, 1, 0)
  rid <- c("A", "A", "A", "A", "B")
  out <- sconjoint:::scmix_design_benchmark(
    dx, y, rid, contrast = c(x = 1),
    protocol_probability = c(A = 1, B = 0.5)
  )
  expect_s3_class(out, "scmix_design_benchmark")
  expect_equal(out$estimate, 0.5)
  expect_equal(out$respondent$matches, c(4L, 0L))
  expect_equal(out$respondent$U, c(1, 0))
  expect_equal(out$se, 0.5)
  expect_equal(out$n_respondents, 2L)
})

test_that("position-neutral benchmark preserves respondent covariance", {
  rid <- rep(c("A", "B"), each = 2)
  pos <- sconjoint:::scmix_design_benchmark(
    matrix(c(1, 1, 0, 0), ncol = 1), c(1, 1, 0, 0), rid, 1,
    protocol_probability = c(A = 1, B = 0.5)
  )
  neg <- sconjoint:::scmix_design_benchmark(
    matrix(c(0, 0, -1, -1), ncol = 1), c(0, 0, 1, 1), rid, -1,
    protocol_probability = c(A = 0.5, B = 1)
  )
  out <- sconjoint:::scmix_design_benchmark_neutral(pos, neg)
  expect_equal(out$estimate,
               0.5 * (pos$estimate + 1 - neg$estimate), tolerance = 1e-12)
  expect_equal(out$se, sd(out$respondent$U) / sqrt(2), tolerance = 1e-12)
  wrong <- neg
  wrong$contrast <- -2
  expect_error(
    sconjoint:::scmix_design_benchmark_neutral(pos, wrong),
    "exactly the negative"
  )
})

test_that("structural-design discrepancy uses joint respondent influence", {
  dx <- matrix(c(1, 1, 1, 1, 0), ncol = 1)
  des <- sconjoint:::scmix_design_benchmark(
    dx, c(1, 1, 1, 1, 0), c("A", "A", "A", "A", "B"), 1,
    protocol_probability = c(A = 1, B = 0.5)
  )
  out <- sconjoint:::scmix_structural_design_discrepancy(
    0.6, des, structural_influence = c(A = 0.2, B = -0.2),
    structural_contrast = 1
  )
  expect_equal(out$estimate, 0.1)
  expect_identical(out$inference, "joint respondent influence")
  expect_equal(unname(out$influence), c(-0.3, 0.3), tolerance = 1e-12)
  descriptive <- sconjoint:::scmix_structural_design_discrepancy(0.6, des)
  expect_true(is.na(descriptive$se))
  expect_identical(descriptive$inference, "descriptive only")
})

test_that("DML discrepancy inference is target-specific and studentized", {
  des <- sconjoint:::scmix_design_benchmark(
    matrix(c(1, 1), ncol = 1), c(1, 0), c("A", "B"), 1,
    protocol_probability = 1
  )
  dml <- list(
    estimate = c(choice = 0.6), se = c(choice = 0.1),
    influence = matrix(c(-0.2, 0.2), 2, 1,
                       dimnames = list(c("A", "B"), "choice")),
    target_inference_available = c(choice = FALSE),
    inference_available = TRUE, status = "conditional_available",
    inference_claim = "conditional_on_documented_high_level_assumptions"
  )
  class(dml) <- c("scmix_dml", "list")
  withheld <- sconjoint:::scmix_structural_design_discrepancy(
    dml, des, target_label = "choice", structural_contrast = c(x = 1)
  )
  expect_identical(withheld$inference, "descriptive only")
  expect_false(withheld$structural_target_inference_available)

  dml$target_inference_available[] <- TRUE
  dml$se[] <- 0
  zero_se <- sconjoint:::scmix_structural_design_discrepancy(
    dml, des, target_label = "choice", structural_contrast = c(x = 1)
  )
  expect_identical(zero_se$inference, "descriptive only")

  dml$se[] <- 0.1
  available <- sconjoint:::scmix_structural_design_discrepancy(
    dml, des, target_label = "choice", structural_contrast = c(x = 1)
  )
  expect_identical(available$inference, "joint respondent influence")
  expect_identical(available$structural_inference_status,
                   "conditional_available")
  expect_identical(available$structural_inference_claim,
                   "conditional_on_documented_high_level_assumptions")
})

test_that("design audit separates protocol support, rank, and realized counts", {
  D <- rbind(d1 = c(1, 0), d2 = c(0, 1), d3 = c(1, 1))
  dx <- D[rep(seq_len(3), each = 2), , drop = FALSE]
  rid <- rep(paste0("r", 1:3), each = 2)
  order <- rep(1:2, 3)
  support <- data.frame(
    contrast_id = rep(rownames(D), each = 2),
    stratum = rep(c("s1", "s2"), 3),
    event = "repeated_ordered_contrast", probability = 0.1
  )
  out <- sconjoint:::scmix_design_audit(
    D, q = 1L, protocol_support = support,
    protocol_strata = c("s1", "s2"),
    deltaX = dx, respondent_id = rid, task_order = order
  )
  expect_s3_class(out, "scmix_design_audit")
  expect_true(out$established)
  expect_equal(out$affine_rank, 3L)
  expect_equal(out$vech_rank, 3L)
  expect_equal(unname(out$realized_repeat_counts), rep(1L, 3))

  support$probability[1] <- 0
  expect_error(
    sconjoint:::scmix_design_audit(D, q = 1L,
                                    protocol_support = support,
                                    protocol_strata = c("s1", "s2")),
    "lie in"
  )
  support$probability[1] <- 0.1
  expect_error(
    sconjoint:::scmix_design_audit(D, q = 1L, protocol_support = support),
    "explicitly enumerate"
  )
  expect_equal(sconjoint:::.pps_matrix_rank(diag(c(1, 1e-9)), tol = 1e-8),
               1L)
  expect_equal(sconjoint:::.pps_matrix_rank(diag(c(1, 1e-9)), tol = 1e-10),
               2L)
})

test_that("rank-constrained injectivity requires a described certificate", {
  D <- rbind(d1 = c(1, 0), d2 = c(0, 1), d3 = c(1, 1))
  out <- sconjoint:::scmix_design_audit(
    D[1:2, , drop = FALSE], q = 1L,
    covariance_injective = TRUE
  )
  expect_false(out$established)
  expect_true(out$covariance_user_asserted)
  expect_match(out$covariance_method, "user asserted")

  unclassed <- list(established = TRUE, verified = TRUE, method = "analytic",
                    domain = "rank one", tolerance = 1e-8,
                    provenance = "unit test",
                    contrasts = D[1:2, , drop = FALSE], q = 1L)
  ignored <- sconjoint:::scmix_design_audit(
    D[1:2, , drop = FALSE], q = 1L,
    covariance_injective = unclassed
  )
  expect_match(ignored$covariance_method, "user asserted")
  certificate <- structure(
    unclassed,
    class = c("scmix_covariance_injectivity_certificate", "list")
  )
  accepted <- sconjoint:::scmix_design_audit(
    D[1:2, , drop = FALSE], q = 1L,
    covariance_injective = certificate
  )
  expect_match(accepted$covariance_method, "verified external certificate")
  certificate$q <- 0L
  mismatched <- sconjoint:::scmix_design_audit(
    D[1:2, , drop = FALSE], q = 1L,
    covariance_injective = certificate
  )
  expect_match(mismatched$covariance_method, "user asserted")
  certificate$q <- NA_real_
  malformed <- sconjoint:::scmix_design_audit(
    D[1:2, , drop = FALSE], q = 1L,
    covariance_injective = certificate
  )
  expect_false(unname(malformed$conditions[["covariance_injectivity"]]))
  expect_match(malformed$covariance_method, "user asserted")
  expect_error(
    sconjoint:::scmix_design_audit(D[1:2, , drop = FALSE], q = NA_real_),
    "q.*integer"
  )
})

test_that("held-out scores require one complete sequence per respondent", {
  ll <- cbind(normal = c(-1, -2, -3), alt = c(-1.2, -1.8, -3.1))
  out <- sconjoint:::scmix_heldout_sequence_score(
    ll, c("a", "b", "c"), out_of_fold = TRUE,
    training_only_tuning = TRUE, provenance = "unit-test fold constructor")
  expect_equal(out$estimate, colMeans(ll))
  expect_equal(nrow(out$paired_differences), 1L)
  expect_equal(out$paired_differences$difference,
               mean(ll[, 1] - ll[, 2]))
  expect_error(
    sconjoint:::scmix_heldout_sequence_score(ll, c("a", "a", "c")),
    "one complete-sequence"
  )
})

test_that("calibration averages within respondent before across respondents", {
  y <- c(1, 1, 1, 1, 0)
  p <- c(0.9, 0.9, 0.9, 0.9, 0.1)
  rid <- c("A", "A", "A", "A", "B")
  joint <- data.frame(
    respondent_id = c("A", "B"), type = "response_pattern",
    stratum = "all", observed = c(1, 0), predicted = c(0.8, 0.2)
  )
  out <- sconjoint:::scmix_heldout_calibration(
    y, p, rid, design_cell = rep("all", 5), task_order = c(1:4, 1),
    joint = joint, out_of_fold = TRUE, training_only_tuning = TRUE,
    provenance = "unit-test fold constructor"
  )
  cell <- out$marginal[out$marginal$type == "design_cell", ]
  expect_equal(cell$observed, 0.5)
  expect_equal(cell$predicted, 0.5)
  expect_true(out$joint_checks_supplied)
  expect_match(out$disclaimer, "Marginal calibration alone")
})

test_that("fit-aware held-out predictions integrate complete respondent sequences", {
  fit <- list(
    deltaX = matrix(1, 4, 1), y = c(1, 0, 1, 1),
    respondent_id = rep(c("A", "B"), each = 2),
    fold_id = rep(1:2, each = 2), q = 0L,
    mu_all_folds = list(matrix(0, 4, 1), matrix(0, 4, 1)),
    kappa_folds = c(0, 0),
    gh = list(U = matrix(numeric(0), 1, 0), w = 1),
    eligible_for_ordinary_inference = TRUE,
    analysis_signature = "analysis-A"
  )
  pred <- sconjoint:::scmix_heldout_predictions(fit)
  expect_identical(pred$analysis_signature, "analysis-A")
  expect_equal(pred$task$predicted, rep(0.5, 4))
  expect_equal(unname(pred$sequence_loglik), rep(-2 * log(2), 2))
  count_A <- pred$joint[pred$joint$type == "response_count" &
                          pred$joint$respondent_id == "A", ]
  expect_equal(count_A$predicted, c(0.25, 0.5, 0.25))
  assessed <- sconjoint:::scmix_prediction_assessment(pred)
  expect_identical(assessed$score$analysis_signature, "analysis-A")
  expect_identical(assessed$calibration$analysis_signature, "analysis-A")
  expect_true(assessed$score$verified_heldout)
  expect_true(assessed$calibration$verified_heldout)
  expect_true(assessed$calibration$joint_checks_supplied)

  unsigned_fit <- fit
  unsigned_fit$analysis_signature <- NULL
  expect_error(sconjoint:::scmix_heldout_predictions(unsigned_fit),
               "analysis_signature")
})

test_that("completion diagnostics are descriptive, not verification", {
  out <- sconjoint:::scmix_completion_diagnostics(
    completed_tasks = c(2, 3, 4, 5, 6),
    predictors = data.frame(z = 1:5, group = c("a", "a", "b", "b", "b")),
    early_response = c(0, 0, 1, 1, 1),
    completion_pattern = c("early", "early", "full", "full", "full")
  )
  expect_equal(nrow(out$associations), 2L)
  expect_true(all(c("slope", "se", "p_value") %in% names(out$early_response)))
  expect_match(out$disclaimer, "cannot verify")
})

test_that("rank and numerical gates fail safely", {
  rank_ok <- sconjoint:::scmix_rank_gate(diag(c(0.4, 0)), q = 1L,
                                         eigenvalue_margin = 0.1,
                                         structural_scale = c(1, 1))
  expect_true(rank_ok$regular_inference)
  rank_bad <- sconjoint:::scmix_rank_gate(diag(c(0.01, 0)), q = 1L,
                                          eigenvalue_margin = 0.1,
                                          structural_scale = c(1, 1))
  expect_false(rank_bad$regular_inference)
  expect_match(rank_bad$status, "withheld")

  stable <- sconjoint:::scmix_numerical_gate(
    data.frame(estimate = c(1, 1.001), se = c(0.2, 0.201),
               log_score = c(-2, -2.001), min_eigenvalue = c(0.4, 0.399)),
    tolerances = c(estimate = 0.01, se = 0.01,
                   log_score = 0.01, min_eigenvalue = 0.01)
  )
  expect_true(stable$pass)
  unstable <- sconjoint:::scmix_numerical_gate(
    data.frame(estimate = c(1, 1.2), se = c(0.2, 0.2)),
    tolerances = c(estimate = 0.01, se = 0.01)
  )
  expect_false(unstable$pass)
})

test_that("prespecified q zero passes only for a numerically zero covariance", {
  zero <- sconjoint:::scmix_rank_gate(matrix(0, 2, 2), q = 0L)
  expect_true(zero$regular_inference)
  nonzero <- sconjoint:::scmix_rank_gate(diag(c(0.1, 0)), q = 0L)
  expect_false(nonzero$regular_inference)
  expect_error(sconjoint:::scmix_rank_gate(diag(2), q = 2L), "p - 1")
})

test_that("local information uses structural-norm generalized eigenvalues", {
  S <- cbind(mean_direction = c(-1, 0, 1, 0),
             covariance_direction = c(0, -2, 0, 2))
  G <- diag(c(1, 4))
  expect_error(
    sconjoint:::scmix_local_information(S, G),
    "identified_directions"
  )
  out <- sconjoint:::scmix_local_information(
    S, G, respondent_id = letters[1:4], identified_directions = TRUE,
    provenance = "unit-test structural tangent"
  )
  manual <- diag(c(1, 1 / 2)) %*% (crossprod(S) / 4) %*%
    diag(c(1, 1 / 2))
  expect_equal(out$generalized_eigenvalues,
               eigen(manual, symmetric = TRUE)$values, tolerance = 1e-12)
  expect_false(out$raw_parameter_hessian_used)
  expect_match(out$disclaimer, "not prove global")
})

test_that("profile helper remains descriptive at a rank boundary", {
  ll <- cbind(c(-2, -1.5, -2.5), c(-1, -1.2, -1.1), c(-1.5, -1.4, -1.6))
  out <- sconjoint:::scmix_profile_sequence_likelihood(
    grid = c(0, 0.2, 0.4), loglik = ll,
    respondent_id = c("a", "b", "c"),
    nuisance_reoptimized = TRUE, rank_boundary = TRUE,
    sieve_tuning_fixed = TRUE, provenance = "unit-test profile"
  )
  expect_identical(out$kind, "profile sequence likelihood")
  expect_false(out$likelihood_ratio_critical_values)
  expect_equal(max(out$table$total_loglik_difference), 0)
  expect_match(out$disclaimer, "No regular likelihood-ratio")
})

test_that("common multipliers preserve respondent and contrast dependence", {
  make_discrepancy <- function(estimate, influence) {
    x <- list(estimate = estimate, influence = influence)
    class(x) <- c("scmix_structural_design_discrepancy", "list")
    x
  }
  ds <- list(
    d1 = make_discrepancy(0.1, c(a = -1, b = 1, c = -2, d = 2)),
    d2 = make_discrepancy(-0.2, c(a = -2, b = 2, c = -4, d = 4))
  )
  zeta <- rbind(c(1, -1, 1, -1), c(-1, 1, -1, 1),
                c(1, 1, -1, -1), c(-1, -1, 1, 1))
  out <- sconjoint:::scmix_discrepancy_multiplier(ds, multipliers = zeta)
  expect_equal(out$draws[, "d2"], 2 * out$draws[, "d1"], tolerance = 1e-12)
  expect_identical(out$unit,
                   "one common multiplier per respondent across contrasts")
  expect_equal(nrow(out$intervals), 2L)
})

test_that("sensitivity collector exposes every omission and claims no identification", {
  out <- sconjoint:::scmix_structural_sensitivity(
    results = list(
      rank_q_stability = list(status = "run_pass", provenance = "test",
                              result = list(stable = TRUE)),
      shape_skewed_simulation = list(
        status = "run_pass", provenance = "test",
        result = list(mean_zero = TRUE, unit_covariance = TRUE,
                      finite_covariance = TRUE,
                      factor_orientation_prespecified = TRUE)
      )
    ),
    q_values = c(1L, 2L),
    materiality_tolerances = c(rank_q_stability = 0.05,
                               shape_skewed_simulation = 0.05),
    prespecified = TRUE
  )
  expect_equal(nrow(out$status), 13L)
  expect_false(out$complete)
  expect_true(all(!out$status$separately_identified))
  expect_true(any(out$status$status == "not_run"))
  expect_match(out$disclaimer, "not coequal identified models")
  expect_true(out$shape_standardization$skewed$mean_zero)
})

test_that("sensitivity runner records failures rather than silently omitting them", {
  out <- sconjoint:::scmix_run_structural_sensitivity(
    runners = list(
      rank_q_stability = function(context) list(stable = context$stable),
      covariance_by_Z = function(context) stop("deliberate failure")
    ),
    provenance = c(rank_q_stability = "unit test",
                   covariance_by_Z = "unit test"),
    context = list(stable = TRUE), q_values = c(1L, 2L),
    prespecified = TRUE
  )
  expect_identical(out$status$status[out$status$component == "rank_q_stability"],
                   "run_pass")
  expect_identical(out$status$status[out$status$component == "covariance_by_Z"],
                   "run_fail")
  expect_false(out$complete)
})

test_that("reporting gates enforce majority and extrapolation labels", {
  rg <- sconjoint:::scmix_rank_gate(diag(c(0.4, 0)), 1L, 0.1,
                                    structural_scale = c(1, 1))
  ng <- sconjoint:::scmix_numerical_gate(
    data.frame(estimate = c(1, 1), se = c(0.2, 0.2)),
    c(estimate = 0.01, se = 0.01)
  )
  inf <- list(status = "conditional_available", inference_available = TRUE,
              estimate = c(share = 0.62), se = c(share = 0.04),
              target_inference_available = c(share = TRUE),
              ci_lower = c(share = 0.55),
              ci_upper = c(share = 0.70))
  class(inf) <- c("scmix_dml", "list")
  opt <- list(all_selected_tolerances_met = TRUE,
              all_computational_gates_pass = TRUE,
              any_bound_activity = FALSE)
  audit <- list(established = TRUE, status = "sufficient conditions established")
  class(audit) <- c("scmix_design_audit", "list")
  info <- sconjoint:::scmix_local_information(
    matrix(c(-1, 1), ncol = 1), matrix(1),
    respondent_id = c("a", "b"), identified_directions = TRUE,
    provenance = "unit-test identified tangent"
  )
  out <- sconjoint:::scmix_reporting_gates(
    residual_variance = 0.4, residual_variance_margin = 0.1,
    on_support = FALSE, rank_gate = rg, numerical_gate = ng,
    local_information = info, information_eigenvalue_margin = 0.1,
    target = "threshold_share",
    inference = inf, target_label = "share", optimization_gate = opt,
    design_audit = audit
  )
  expect_true(out$regular_reporting_approved)
  expect_identical(out$majority_claim, "above one-half")
  expect_identical(out$support, "structural extrapolation")
  expect_identical(out$inference_claim,
                   "conditional_on_documented_high_level_assumptions")

  legacy <- sconjoint:::scmix_reporting_gates(
    residual_variance = 0.4, residual_variance_margin = 0.1,
    on_support = FALSE, rank_gate = rg, numerical_gate = ng,
    weak_information = FALSE, target = "threshold_share",
    inference = inf, target_label = "share", optimization_gate = opt,
    design_audit = audit
  )
  expect_false(legacy$regular_reporting_approved)
  expect_match(legacy$gates$reason[legacy$gates$gate ==
                                   "local information evidence"],
               "insufficient")
})

test_that("high-level assessment never says assumptions were verified", {
  fit <- list(
    respondent_id = c("a", "a", "b"),
    mu_full = matrix(c(1, 0, 1, 0, 2, 1), 3, 2, byrow = TRUE),
    Sigma_hat = tcrossprod(matrix(c(0.5, 0.2), 2, 1)), q = 1L,
    sd_dx_full = c(1, 1),
    attr_names = c("x1", "x2")
  )
  class(fit) <- c("scmix", "list")
  sens <- sconjoint:::scmix_structural_sensitivity()
  out <- sconjoint:::scmix_assess(fit, eigenvalue_margin = 0.1,
                                  sensitivity = sens)
  expect_s3_class(out, "scmix_assessment")
  expect_false(out$maintained_assumptions_verified)
  expect_match(out$disclaimer, "cannot verify normality")
  expect_true(out$rank$regular_inference)
  expect_identical(out$sensitivity, sens)
})

test_that("protocol completion cannot by itself authorize structural reporting", {
  fit <- list(
    respondent_id = c("a", "a", "b"),
    mu_full = matrix(c(1, 0, 1, 0, 2, 1), 3, 2, byrow = TRUE),
    Sigma_hat = tcrossprod(matrix(c(0.5, 0.2), 2, 1)), q = 1L,
    sd_dx_full = c(1, 1), attr_names = c("x1", "x2"),
    analysis_signature = "analysis-A"
  )
  class(fit) <- c("scmix", "list")
  components <- c(
    "rank_q_stability",
    "shape_skewed_simulation", "shape_skewed_refit",
    "shape_bimodal_simulation", "shape_bimodal_refit",
    "shape_heavy_tail_simulation", "shape_heavy_tail_refit",
    "covariance_by_Z", "task_order_fatigue_learning", "serial_shocks",
    "position", "scale", "completion"
  )
  applicable_components <- components[!grepl("_refit$", components)]
  sensitivity_tolerances <- stats::setNames(
    rep(0.05, length(applicable_components)), applicable_components)
  results <- stats::setNames(lapply(components, function(nm) {
    if (grepl("_refit$", nm)) {
      list(status = "not_applicable", provenance = "prespecified plan",
           justification = "simulation is the prespecified shape check")
    } else if (grepl("^shape_", nm)) {
      list(status = "run_pass", provenance = "prespecified plan",
           result = list(mean_zero = TRUE, unit_covariance = TRUE,
                         finite_covariance = TRUE,
                         factor_orientation_prespecified = TRUE,
                         tolerance_applied = TRUE,
                         materiality_tolerance = 0.05,
                         materiality_value = 0.01, passed = TRUE))
    } else {
      list(status = "run_pass", provenance = "prespecified plan",
           result = list(tolerance_applied = TRUE,
                         materiality_tolerance = 0.05,
                         materiality_value = 0.01, passed = TRUE))
    }
  }), components)
  sensitivity_pass <- sconjoint:::scmix_structural_sensitivity(
    results, q_values = c(1L, 2L),
    materiality_tolerances = sensitivity_tolerances,
    prespecified = TRUE
  )
  expect_true(sensitivity_pass$substantive_pass)
  duplicated_q <- sconjoint:::scmix_structural_sensitivity(
    results, q_values = c(1L, 1L),
    materiality_tolerances = sensitivity_tolerances,
    prespecified = TRUE
  )
  expect_false(duplicated_q$complete)
  expect_false(duplicated_q$distinct_q_values)
  no_materiality <- sconjoint:::scmix_structural_sensitivity(
    results, q_values = c(1L, 2L), prespecified = TRUE
  )
  expect_true(no_materiality$complete)
  expect_false(no_materiality$materiality_complete)
  expect_false(no_materiality$substantive_pass)
  sensitive_results <- results
  sensitive_results$position$status <- "run_sensitive"
  sensitive_results$position$result$materiality_value <- 0.10
  sensitive_results$position$result$passed <- FALSE
  sensitivity_bad <- sconjoint:::scmix_structural_sensitivity(
    sensitive_results, q_values = c(1L, 2L),
    materiality_tolerances = sensitivity_tolerances,
    prespecified = TRUE
  )
  expect_true(sensitivity_bad$complete)
  expect_false(sensitivity_bad$substantive_pass)

  audit <- structure(
    list(established = TRUE, status = "sufficient conditions established"),
    class = c("scmix_design_audit", "list"))
  score <- structure(list(verified_heldout = TRUE,
                          analysis_signature = "analysis-A"),
                     class = c("scmix_heldout_score", "list"))
  calibration <- structure(
    list(verified_heldout = TRUE, joint_checks_supplied = TRUE,
         marginal = data.frame(gap = 0.01), joint = data.frame(gap = -0.02),
         analysis_signature = "analysis-A"),
    class = c("scmix_calibration_assessment", "list"))
  completion <- structure(list(), class = c("scmix_completion_assessment", "list"))
  information <- structure(
    list(verified_source = TRUE, smallest = 0.5),
    class = c("scmix_local_information", "list"))
  profile <- structure(list(verified_profile = TRUE),
                       class = c("scmix_profile_sequence_likelihood", "list"))
  numerical <- structure(
    list(pass = TRUE, status = "numerical-stability gate passed",
         analysis_signature = "analysis-A"),
    class = c("scmix_numerical_gate", "list"))
  reporting <- structure(
    list(regular_reporting_approved = TRUE),
    class = c("scmix_reporting_gates", "list"))
  inference <- structure(
    list(inference_available = TRUE, status = "conditional_available",
         analysis_signature = "analysis-A",
         inference_claim =
           "conditional_on_documented_high_level_assumptions"),
    class = c("scmix_dml", "list"))
  optimization <- structure(
    list(all_selected_tolerances_met = TRUE,
         all_computational_gates_pass = TRUE, any_bound_activity = FALSE,
         analysis_signature = "analysis-A", signature_match = TRUE),
    class = c("scmix_optimization_audit", "list"))
  assess <- function(sensitivity, calibration_margins = c(marginal = 0.05,
                                                           joint = 0.05),
                     inference_use = inference, score_use = score,
                     calibration_use = calibration) {
    sconjoint:::scmix_assess(
      fit, design_audit = audit, heldout_scores = score_use,
      calibration = calibration_use, completion = completion,
      local_information = information, profiles = profile,
      numerical = numerical, sensitivity = sensitivity,
      reporting = list(theta = reporting), inference = inference_use,
      optimization = optimization, eigenvalue_margin = 0.1,
      information_eigenvalue_margin = 0.1,
      calibration_margins = calibration_margins,
      required_reporting = "theta", required_discrepancies = character(0)
    )
  }
  bad <- assess(sensitivity_bad)
  expect_true(bad$protocol_complete)
  expect_false(bad$structural_reporting_ready)
  expect_false(bad$assessment_gates$pass[
    bad$assessment_gates$gate == "prespecified structural sensitivity"])

  calibrated_bad <- assess(sensitivity_pass,
                           c(marginal = 0.005, joint = 0.005))
  expect_false(calibrated_bad$structural_reporting_ready)
  expect_false(calibrated_bad$assessment_gates$pass[
    calibrated_bad$assessment_gates$gate ==
      "held-out calibration materiality"])

  good <- assess(sensitivity_pass)
  expect_true(good$protocol_complete)
  expect_true(good$structural_reporting_ready)
  expect_true(good$signature_match)
  expect_identical(good$inference_status, "conditional_available")
  expect_identical(good$inference_claim,
                   "conditional_on_documented_high_level_assumptions")
  expect_false(good$maintained_assumptions_verified)

  cross_analysis <- inference
  cross_analysis$analysis_signature <- "analysis-B"
  mismatched <- assess(sensitivity_pass, inference_use = cross_analysis)
  expect_false(mismatched$structural_reporting_ready)
  expect_false(mismatched$signature_match)
  expect_match(mismatched$assessment_gates$reason[
    mismatched$assessment_gates$gate == "analysis artifact signatures"],
    "mismatch")

  cross_calibration <- calibration
  cross_calibration$analysis_signature <- "analysis-B"
  mismatched_calibration <- assess(
    sensitivity_pass, calibration_use = cross_calibration)
  expect_false(mismatched_calibration$structural_reporting_ready)
  expect_false(mismatched_calibration$signature_match)

  unsigned_score <- score
  unsigned_score$analysis_signature <- NULL
  missing_score_signature <- assess(sensitivity_pass,
                                    score_use = unsigned_score)
  expect_false(missing_score_signature$structural_reporting_ready)
  expect_false(missing_score_signature$signature_match)
  expect_match(missing_score_signature$assessment_gates$reason[
    missing_score_signature$assessment_gates$gate ==
      "analysis artifact signatures"], "missing nonempty")

  unsigned_calibration <- calibration
  unsigned_calibration$analysis_signature <- NULL
  missing_calibration_signature <- assess(
    sensitivity_pass, calibration_use = unsigned_calibration)
  expect_false(missing_calibration_signature$structural_reporting_ready)
  expect_false(missing_calibration_signature$signature_match)
})
