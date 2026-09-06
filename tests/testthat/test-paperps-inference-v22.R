## Focused tests for the frozen v2.2 target/reporting layer. These tests use a
## synthetic scmix_dml object so they do not require torch or model fitting.

.v22_calibration_values <- stats::setNames(
  as.list(rep(1, length(sconjoint:::.scmix_v22_required_calibration_names))),
  sconjoint:::.scmix_v22_required_calibration_names)
.v22_calibration_values$chart_discrepancy_cutoff <- 1e6
.v22_calibration_values$nonlinear_remainder_cutoff <- 1e6
.v22_calibration_values$interior_distance_cutoff <- 0
.v22_calibration_values$numerical_rank_tolerance <- 1e-12
.v22_calibration_source_hash <- "synthetic-calibration-source-sha256"

.v22_contract <- function(signature = "v22-synthetic",
                          code_commit = "test-code",
                          riesz_inner_seed = 17L,
                          coordinate_registry = c("u1", "u2"), q = 0L,
                          coordinate_subset = coordinate_registry,
                          derivative_subset_rule =
                            "all registered structural coordinates",
                          fit_uses_numerical_integration = FALSE,
                          exact_nested_pair_registered = FALSE) {
  scmix_v22_analysis_contract(
    analysis_signature = signature, application_id = "synthetic-app",
    specification_id = "synthetic-q1", specification_role = "primary",
    respondent_sample_rule = "all synthetic respondents",
    completion_rule = "complete sequences", missing_data_rule = "none",
    outer_folds = list(fold1 = "signed", fold2 = "signed"), inner_folds = 2L,
    preprocessing_rule = "training only", integration_rule = "fixed nodes",
    optimizer_starts = "start_001", learner_grid = "fixed",
    ridge_grid = c("0", "n_k^-1", "n_k^-3/4", "n_k^-1/2"),
    seeds = list(multiplier = 73L, riesz_inner = riesz_inner_seed),
    code_commit = code_commit,
    data_hash = "test-data", design_hash = "test-design",
    calibration_hash = scmix_v22_calibration_hash(
      .v22_calibration_values, .v22_calibration_source_hash),
    q = q,
    fit_uses_numerical_integration = fit_uses_numerical_integration,
    exact_nested_pair_registered = exact_nested_pair_registered,
    derivative_coordinate_registry = coordinate_registry,
    derivative_coordinate_subset = coordinate_subset,
    derivative_subset_rule = derivative_subset_rule)
}

## Most inference-unit fixtures do not exercise numerical integration. Make
## that declaration explicit so the typed I001--I005 path is fail-closed only
## in the dedicated computation-artifact tests below.
.v22_signed_target_spec <- sconjoint::scmix_v22_target_spec
scmix_v22_target_spec <- function(..., uses_numerical_integration = FALSE) {
  .v22_signed_target_spec(
    ..., uses_numerical_integration = uses_numerical_integration)
}

.v22_fake_dml <- function(crossfit, raw, influence,
                          primitive_mode = FALSE) {
  stopifnot(identical(names(crossfit), names(raw)))
  influence <- as.matrix(influence)
  colnames(influence) <- names(crossfit)
  if (is.null(rownames(influence))) {
    rownames(influence) <- sprintf("r%03d", seq_len(nrow(influence)))
  }
  influence <- sweep(influence, 2L, colMeans(influence), `-`)
  J <- ncol(influence); labels <- colnames(influence)
  fold_id <- rep(1:2, length.out = nrow(influence))
  one_fold <- function(k) {
    training_n <- sum(fold_id != k)
    neighborhood_grid <- c(0, training_n^(-1))
    I <- diag(J); dimnames(I) <- list(NULL, labels)
    neighborhoods <- stats::setNames(lapply(seq_len(J), function(j) list(
      nu = neighborhood_grid, raw_one_step = rep(raw[j], 2L),
      standard_error = rep(stats::sd(influence[, j]) /
                             sqrt(nrow(influence)), 2L),
      raw_wald_lower = rep(raw[j], 2L),
      raw_wald_upper = rep(raw[j], 2L),
      influence = cbind(influence[, j], influence[, j]))), labels)
    list(
      training_n = training_n,
      honest_evaluation_information = diag(J),
      honest_evaluation_information_pseudoinverse = diag(J),
      honest_evaluation_gradient = I,
      riesz_numerical_rank_tolerance = 1e-12,
      riesz_coefficients_whitened = I,
      selected_nu_by_target = stats::setNames(rep(0, J), labels),
      derivative_refinement_error = stats::setNames(rep(0, J), labels),
      derivative_refinement_protocol_exact = stats::setNames(
        rep(TRUE, J), labels),
      ridge_neighborhood_artifact = neighborhoods)
  }
  out <- structure(list(
    status = "conditional_available", inference_available = TRUE,
    plugin_estimate = crossfit, estimate = raw,
    crossfit_plugin = crossfit, raw_one_step = raw,
    influence = influence, respondent_id = rownames(influence),
    fold_id = fold_id,
    fold_details = stats::setNames(lapply(1:2, one_fold),
                                   c("fold1", "fold2")),
    v22_primitive_mode = primitive_mode,
    analysis_signature = "v22-synthetic"),
    class = c("scmix_dml", "list"))
  out$v22_analysis_contract <- .v22_contract(out$analysis_signature)
  refit <- function(fit, outer_fold, inner_fold,
                    training_respondent_id, validation_respondent_id,
                    ridge_grid, primitive_registry_ids) {
    risk <- matrix(rep(seq_along(ridge_grid) - 1,
                       length(primitive_registry_ids)),
                   nrow = length(ridge_grid),
                   ncol = length(primitive_registry_ids),
                   dimnames = list(NULL, primitive_registry_ids))
    list(
      success = TRUE, validation_risk = risk, ridge_grid = ridge_grid,
      preprocessing_hash = paste0("prep-", outer_fold, "-", inner_fold),
      nuisance_fit_hash = paste0("nuisance-", outer_fold, "-", inner_fold),
      score_hash = paste0("score-", outer_fold, "-", inner_fold))
  }
  trace <- scmix_v22_run_inner_refits(
    out, labels, inner_folds = 2L, refit = refit, seed = 17L,
    code_hash = "test-code", provenance = "executed synthetic inner refits")
  evidence <- scmix_v22_riesz_evidence(
    out, trace, "signed n_k-dependent sequence")
  out$v22_riesz_status <- "signed_inner_nuisance_refit_evidence_used"
  out$v22_riesz_evidence_fingerprint <- evidence$fingerprint
  out$v22_riesz_evidence <- evidence
  out$v22_riesz_analysis_contract_fingerprint <-
    evidence$analysis_contract_fingerprint
  out$v22_ridge_sequence_provenance <-
    evidence$ridge_sequence_provenance
  out
}

.v22_fake_direct_heterogeneity <- function(crossfit, raw, influence,
                                           primitive_ids = c(
                                             "primitive:HZ", "primitive:HR")) {
  m <- unname(crossfit[[1L]])
  cf <- c(crossfit[[2L]] - m^2, crossfit[[3L]])
  hz_raw <- cf[[1L]] + sum(c(-2 * m, 1, 0) * (raw - crossfit))
  raw_primitive <- c(hz_raw, raw[[3L]])
  names(cf) <- names(raw_primitive) <- primitive_ids
  primitive_if <- cbind(
    influence[, 2L] - 2 * m * influence[, 1L], influence[, 3L])
  colnames(primitive_if) <- primitive_ids
  out <- .v22_fake_dml(cf, raw_primitive, primitive_if,
                       primitive_mode = TRUE)
  out
}

.v22_full_plugin <- function(dml, values) {
  scmix_v22_full_plugin_artifact(
    dml, values, dml$v22_analysis_contract,
    final_fit_hash = "synthetic-final-fit", code_hash = "test-code",
    provenance = "synthetic full fit")
}

.v22_derivative <- function(dml, registry) {
  primitive_ids <- sort(unique(unlist(lapply(
    registry, `[[`, "primitive_registry_ids"))), method = "radix")
  coordinates <- stats::setNames(
    rep(0, length(dml$v22_analysis_contract$derivative_coordinate_registry)),
    dml$v22_analysis_contract$derivative_coordinate_registry)
  folds <- list(fold1 = coordinates, fold2 = coordinates)
  analytic <- matrix(0, length(coordinates), length(primitive_ids))
  analytic[cbind(seq_along(primitive_ids), seq_along(primitive_ids))] <- 1
  dimnames(analytic) <- list(names(coordinates), primitive_ids)
  scmix_v22_derivative_audit(
    dml, folds, primitive_ids,
    evaluate = function(fold_id, coordinates, primitive_registry_ids) {
      stats::setNames(
        as.numeric(coordinates[seq_along(primitive_registry_ids)]),
        primitive_registry_ids)
    },
    analytic_gradient = list(fold1 = analytic, fold2 = analytic),
    code_hash = "test-code", provenance = "synthetic exact derivative audit")
}

.v22_approved_calibration <- list(
  protocol_id = sconjoint:::.scmix_v22_protocol_id,
  status = "frozen_for_production",
  calibration_hash = scmix_v22_calibration_hash(
    .v22_calibration_values, .v22_calibration_source_hash),
  source_hash = .v22_calibration_source_hash,
  manifest_fingerprint = "synthetic-package-manifest-fingerprint")
testthat::local_mocked_bindings(
  .scmix_v22_approved_calibration_manifest = function()
    .v22_approved_calibration,
  .package = "sconjoint", .env = testthat:::teardown_env())

.v22_external_gates <- function(registry, contract) {
  local_ids <- c(sconjoint:::.scmix_v22_local_gate_ids,
                 sconjoint:::.scmix_v22_computation_gate_ids,
                 sconjoint:::.scmix_v22_fit_loading_gate_ids)
  out <- scmix_v22_gate_template()
  out <- out[!out$gate_id %in% local_ids, , drop = FALSE]
  out <- do.call(rbind, lapply(seq_len(nrow(out)), function(i) {
    row <- out[i, , drop = FALSE]
    expected <- sconjoint:::.scmix_v22_expected_scope_ids(
      row$scope, registry, contract)
    if (!length(expected)) expected <- paste0("synthetic:", row$scope)
    do.call(rbind, lapply(expected, function(scope_id) {
      z <- row; z$scope_id <- scope_id; z
    }))
  }))
  values <- lapply(out$comparator, function(comparator) switch(
    comparator,
    equal = list(statistic = TRUE, threshold = TRUE),
    less_than_or_equal = list(statistic = 0, threshold = 1),
    greater_than_or_equal = list(statistic = 1, threshold = 0),
    contains = list(statistic = "supported", threshold = "supported"),
    consistent_with = list(statistic = "consistent", threshold = "consistent")))
  out$state <- "pass"
  out$statistic <- I(lapply(values, `[[`, "statistic"))
  out$threshold <- I(lapply(values, `[[`, "threshold"))
  out$provenance <- "signed synthetic test artifact"
  out$message <- paste("Synthetic passing evidence for", out$gate_id)
  applicable <- mapply(
    sconjoint:::.scmix_v22_external_gate_applicable,
    out$gate_id, out$scope_id,
    MoreArgs = list(registry = registry, contract = contract),
    USE.NAMES = FALSE)
  stopifnot(!anyNA(applicable))
  out$state[!applicable] <- "not_applicable"
  out$message[!applicable] <- paste(
    "Not applicable under the signed synthetic contract for",
    out$gate_id[!applicable])
  rownames(out) <- NULL
  scmix_v22_gate_artifact(
    out, registry, contract, "synthetic signed gate artifact")
}

.v22_calibration <- scmix_v22_calibration_artifact(
  .v22_calibration_values, .v22_contract(),
  source_hash = .v22_calibration_source_hash,
  provenance = "synthetic Step-3 calibration artifact")

test_that("target specs carry explicit domains and minimal primitive IDs", {
  p <- scmix_v22_target_spec(
    "vote", "headline", "probability", "p", "primitive:p",
    "profile A rather than B", "probability", "mean")
  expect_identical(p$target_domain$kind, "bounded")
  expect_equal(c(p$target_domain$lower, p$target_domain$upper), c(0, 1))

  h <- scmix_v22_target_spec(
    "heterogeneity", "headline", "directional_heterogeneity",
    c("m", "second", "hr"), c("primitive:HZ", "primitive:HR"),
    "declared contrast direction", "share", c("mean", "loading"))
  expect_identical(h$primitive_registry_ids,
                   c("primitive:HZ", "primitive:HR"))
  expect_length(h$supporting_names, 3L)
  expect_error(scmix_v22_target_spec(
    "bad", "headline", "directional_heterogeneity", c("m", "second"),
    c("hz", "hr"), "direction", "share", "mean"),
    "requires 3 supporting")
  expect_error(scmix_v22_target_spec(
    "override", "headline", "probability", "p", "primitive:p",
    "positive", "probability", "mean",
    domain = list(kind = "bounded", lower = -1, upper = 1,
                  chart = "scaled_logit")),
    "requires a new protocol ID")
  expect_false(exists(
    "scmix_v22_domain_amendment", envir = asNamespace("sconjoint"),
    inherits = FALSE))
})

test_that("heterogeneity Riesz targets are direct minimal HZ and HR primitives", {
  N <- 12L; d <- 3L
  m <- seq(-0.5, 0.6, length.out = N)
  second <- m^2
  hr <- rep(0.2, N)
  gm <- matrix(rep(c(1, 2, 3), each = N), N, d)
  tg <- list(
    value = cbind(m = m, second = second, hr = hr),
    gradient = list(gm, 2 * m * gm, matrix(1, N, d)),
    gradient_h = list(gm, 2 * m * gm, matrix(1, N, d)),
    gradient_h2 = list(gm, 2 * m * gm, matrix(1, N, d)),
    labels = c("m", "second", "hr"),
    derivative_source = rep("analytic", 3L),
    derivative_refinement_error = rep(0, 3L),
    derivative_refinement_protocol_exact = rep(TRUE, 3L))
  spec <- scmix_v22_target_spec(
    "heterogeneity", "family", "directional_heterogeneity",
    c("m", "second", "hr"), c("HZ", "HR"), "direction", "share",
    c("mean", "loading"))
  train <- 1:8; held <- 9:12
  out <- sconjoint:::.scmix_dml_v22_primitive_targets(
    tg, list(spec), train, held)
  mbar_train <- mean(m[train])
  mbar_held <- mean(m[held])
  expected_hz <- numeric(N)
  expected_hz[train] <- (m[train] - mbar_train)^2
  expected_hz[held] <- (m[held] - mbar_held)^2
  expected_gradient <- gm
  expected_gradient[train, ] <-
    2 * (m[train] - mbar_train) * gm[train, ]
  expected_gradient[held, ] <-
    2 * (m[held] - mbar_held) * gm[held, ]
  expect_identical(out$labels, c("HZ", "HR"))
  expect_equal(out$value[, "HZ"], expected_hz)
  expect_equal(out$gradient[[1L]], expected_gradient)
  expect_equal(out$direct_pz_center[["HZ"]], mean(expected_hz[held]))
  expect_equal(
    out$supporting_audit$heterogeneity$outer_training_mean_direction,
    mbar_train)
  expect_equal(
    out$supporting_audit$heterogeneity$outer_evaluation_mean_direction,
    mbar_held)
  expect_equal(out$supporting_audit$heterogeneity$training_mean,
               colMeans(tg$value[train, , drop = FALSE]))
})

test_that("honest outer-fold Riesz defects and ridge neighborhoods are stored", {
  set.seed(918)
  N <- 90L; d <- 3L
  score <- matrix(stats::rnorm(N * d), N, d)
  gradient <- list(
    matrix(rep(c(1, 0.2, -0.1), each = N), N, d),
    matrix(rep(c(-0.3, 0.4, 0.8), each = N), N, d))
  train <- seq_len(60L); held <- 61:90
  grid <- c(0, 60^(-1), 60^(-3 / 4), 60^(-1 / 2))
  risk <- cbind(c(2, 0, 1, 3), c(2, 1, 0, 3))
  rz <- sconjoint:::.scmix_dml_riesz(
    score, gradient, train, held, grid, diag(d), diag(d),
    validation_fraction = 0.2, seed = 8L, solve_tolerance = 1e-12,
    selected_nu_override = c(grid[2L], grid[3L]),
    inner_validation_override = risk,
    numerical_rank_tolerance = 1e-12, N_total = N)
  expect_identical(rz$selection_source,
                   "signed_inner_nuisance_refit_evidence")
  expect_true(all(is.finite(rz$honest_riesz_null_space_defect)))
  expect_true(all(is.finite(rz$honest_information_scaled_defect_numerator)))
  expect_length(rz$ridge_neighborhood, 2L)
  expect_true(all(vapply(rz$ridge_neighborhood, function(x) 0 %in% x,
                         logical(1L))))
  expect_equal(nrow(rz$correction_neighborhood[[1L]]), length(held))
  expect_equal(rz$selected_nu_by_target, c(grid[2L], grid[3L]))

  rid <- sprintf("i%03d", seq_len(60L))
  fit <- list(K = 2L, analysis_signature = "signed-fit",
              respondent_id = rid, fold_id = rep(1:2, each = 30L))
  fit$v22_analysis_contract <- .v22_contract(
    "signed-fit", code_commit = "code-hash", riesz_inner_seed = 17L,
    coordinate_registry = c("u1", "u2"))
  callback <- function(fit, outer_fold, inner_fold,
                       training_respondent_id, validation_respondent_id,
                       ridge_grid, primitive_registry_ids) {
    risk <- if (outer_fold == 1L) {
      cbind(a = c(2, 0, 1, 3), b = c(2, 1, 0, 3))
    } else cbind(a = c(2, 1, 0, 3), b = c(2, 0, 1, 3))
    list(success = TRUE, validation_risk = risk, ridge_grid = ridge_grid,
         preprocessing_hash = paste0("prep-", outer_fold, "-", inner_fold),
         nuisance_fit_hash = paste0("fit-", outer_fold, "-", inner_fold),
         score_hash = paste0("score-", outer_fold, "-", inner_fold))
  }
  nested <- scmix_v22_run_inner_refits(
    fit, c("a", "b"), inner_folds = 2L, refit = callback, seed = 17L,
    code_hash = "code-hash", provenance = "executed nested-refit manifest")
  expect_s3_class(nested, "scmix_v22_inner_refit_trace")
  expect_equal(nrow(nested$trace), 4L)
  evidence <- scmix_v22_riesz_evidence(
    fit, nested, "n_k-dependent vanishing grid")
  expect_s3_class(evidence, "scmix_v22_riesz_evidence")
  expect_true(nzchar(evidence$fingerprint))
  arbitrary <- nested
  arbitrary$selected_nu[1L, 1L] <- max(c(
    0, nested$training_n[1L]^(-1), nested$training_n[1L]^(-3 / 4),
    nested$training_n[1L]^(-1 / 2)))
  resign <- unclass(arbitrary); resign$fingerprint <- NULL
  arbitrary$fingerprint <- sconjoint:::.scmix_v22_fingerprint(resign)
  expect_error(scmix_v22_riesz_evidence(
    fit, arbitrary, "sequence"), "intact signed inner-refit trace")
  relabeled <- nested
  rownames(relabeled$inner_validation[[1L]]) <-
    rev(rownames(relabeled$inner_validation[[1L]]))
  resign <- unclass(relabeled); resign$fingerprint <- NULL
  relabeled$fingerprint <- sconjoint:::.scmix_v22_fingerprint(resign)
  expect_error(scmix_v22_riesz_evidence(
    fit, relabeled, "sequence"), "intact signed inner-refit trace")
  wrong_grid <- callback
  body(wrong_grid) <- substitute({
    ans <- BODY
    ans$ridge_grid <- rev(ans$ridge_grid)
    ans
  }, list(BODY = body(callback)))
  expect_error(scmix_v22_run_inner_refits(
    fit, c("a", "b"), 2L, wrong_grid, 17L, "code-hash", "manifest"),
    "malformed ridge/risk artifact")
})

test_that("bounded targets retain four estimates and chart the raw update", {
  N <- 80L
  IF <- matrix(seq(-2, 2, length.out = N), ncol = 1L,
               dimnames = list(sprintf("r%03d", seq_len(N)), "p"))
  dml <- .v22_fake_dml(c(p = 0.80), c(p = 1.20), IF)
  spec <- scmix_v22_target_spec(
    "vote", "headline", "probability", "p", "primitive:p",
    "profile A rather than B", "probability", "mean")
  out <- scmix_v22_finalize(
    dml, list(spec), full_sample_plugin = .v22_full_plugin(dml, c(p = 0.82)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  z <- out$targets$vote
  expect_equal(z$full_sample_plugin, 0.82)
  expect_equal(z$crossfit_plugin, 0.80)
  expect_equal(z$raw_one_step, 1.20)
  expect_gt(z$reported_estimate, 0)
  expect_lt(z$reported_estimate, 1)
  expect_true(all(c("full_sample_plugin", "crossfit_plugin", "raw_one_step",
                    "reported_estimate") %in% out$retained_estimates))
  expect_true(all(is.na(z$pointwise_interval)))
  expect_true("C004" %in% z$status$warning_codes)
  expect_identical(z$status$target_status, "withheld")
  expect_identical(z$confidence_center, z$reported_estimate)
  expect_true(z$honest_riesz$available)
  honest_gates <- z$status$gate_record[
    z$status$gate_record$gate_id %in% c("R002", "R003", "R004"), ,
    drop = FALSE]
  expect_true(all(honest_gates$state == "pass"))
  expect_identical(out$source_inference[[1L]]$influence, dml$influence)

  unsigned_status <- scmix_v22_finalize(
    dml, list(spec), full_sample_plugin = .v22_full_plugin(dml, c(p = 0.82)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = FALSE)
  expect_identical(unsigned_status$targets$vote$status$fit_status, "withheld")
  expect_true("P001" %in%
                unsigned_status$targets$vote$status$hard_failure_codes)
})

test_that("missing calibration or gate evidence fails closed but keeps diagnostics", {
  N <- 40L
  IF <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
               dimnames = list(sprintf("r%03d", seq_len(N)), "p"))
  dml <- .v22_fake_dml(c(p = 0.6), c(p = 0.65), IF)
  spec <- scmix_v22_target_spec(
    "vote", "headline", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean")
  out <- scmix_v22_finalize(dml, list(spec))
  z <- out$targets$vote
  expect_identical(z$status$fit_status, "withheld")
  expect_identical(z$status$target_status, "withheld")
  expect_true(all(is.na(z$pointwise_interval)))
  expect_true(is.finite(z$reported_estimate))
  expect_true(all(c("P001", "C003", "G006") %in%
                    z$status$hard_failure_codes))
})

test_that("typed computation gates cannot be supplied through external_gates", {
  spec <- scmix_v22_target_spec(
    "vote", "single", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean")
  contract <- .v22_contract()
  forbidden <- scmix_v22_gate_record(
    "F008", "fit_computation", "target", "hard_target", "pass",
    0, 1, "less_than_or_equal", "standard_errors",
    "generic synthetic artifact", "Synthetic passing F008 evidence.",
    "vote")
  expect_error(scmix_v22_gate_artifact(
    forbidden, list(spec), contract, "generic artifact"),
    "may not supply typed computation")
  forbidden_fit <- scmix_v22_gate_record(
    "F001", "fit_computation", "fit", "hard_fit", "pass",
    TRUE, TRUE, "equal", "boolean", "generic synthetic artifact",
    "Synthetic passing F001 evidence.", contract$specification_id)
  expect_error(scmix_v22_gate_artifact(
    forbidden_fit, list(spec), contract, "generic artifact"),
    "may not supply typed fit/loading")

  N <- 30L
  influence <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
                      dimnames = list(sprintf("r%03d", seq_len(N)), "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .52), influence)
  legacy <- .v22_external_gates(list(spec), dml$v22_analysis_contract)
  legacy_payload <- unclass(legacy); legacy_payload$fingerprint <- NULL
  legacy_payload$gates <- rbind(legacy_payload$gates, forbidden)
  legacy_payload$fingerprint <- sconjoint:::.scmix_v22_fingerprint(
    legacy_payload)
  class(legacy_payload) <- c("scmix_v22_gate_artifact", "list")
  expect_error(scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .51)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration, external_gates = legacy_payload,
    complete_external_record = TRUE), "may not supply typed computation")
})

test_that("missing typed fit/loading evidence is explicit and fail closed", {
  N <- 30L
  influence <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
                      dimnames = list(sprintf("r%03d", seq_len(N)), "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .52), influence)
  spec <- scmix_v22_target_spec(
    "vote", "single", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean")
  out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .51)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_true(all(out$fit_loading_gates$state %in%
                    c("not_run", "not_applicable")))
  expect_true(all(paste0("F00", 1:6) %in%
                    out$targets$vote$status$hard_failure_codes))
  expect_identical(out$fit_loading_gate_artifact_fingerprint, NA_character_)
})

test_that("missing applicable typed computation evidence remains not_run", {
  spec <- scmix_v22_target_spec(
    "vote", "single", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean",
    uses_numerical_integration = TRUE)
  contract <- .v22_contract()
  payload <- unclass(contract); payload$fingerprint <- NULL
  payload$q <- 1L
  payload$fit_uses_numerical_integration <- TRUE
  payload$exact_nested_pair_registered <- TRUE
  payload$optimizer_starts <- c("start_001", "start_002")
  payload$fingerprint <- sconjoint:::.scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_analysis_contract", "list")
  calibration <- scmix_v22_calibration_artifact(
    .v22_calibration_values, payload, .v22_calibration_source_hash,
    "applicable computation fixture")
  computation <- sconjoint:::.scmix_v22_computation_gate_rows(
    NULL, payload, list(spec), calibration)
  expect_setequal(unique(computation$rows$gate_id),
                  sconjoint:::.scmix_v22_computation_gate_ids)
  expect_true(all(computation$rows$state == "not_run"))

  ## The public finalizer must retain the same fail-closed target rows rather
  ## than silently treating absent typed evidence as a pass.
  n <- 30L
  influence <- matrix(seq(-1, 1, length.out = n), ncol = 1L,
                      dimnames = list(sprintf("r%03d", seq_len(n)), "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .52), influence)
  finalized <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .51)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  target_i <- finalized$computation_gates[
    finalized$computation_gates$gate_id %in% c("I001", "I002"), ,
    drop = FALSE]
  expect_true(all(target_i$state == "not_run"))
  expect_identical(finalized$targets$vote$status$target_status, "withheld")
})

test_that("typed computation artifacts are authenticated and unique by family", {
  spec <- scmix_v22_target_spec(
    "vote", "single", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean")
  contract <- .v22_contract()
  payload <- unclass(contract); payload$fingerprint <- NULL
  payload$optimizer_starts <- c("start_001", "start_002")
  payload$fingerprint <- sconjoint:::.scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_analysis_contract", "list")
  calibration <- scmix_v22_calibration_artifact(
    .v22_calibration_values, payload, .v22_calibration_source_hash,
    "typed computation fixture")
  f008_gate <- scmix_v22_gate_record(
    "F008", "fit_computation", "target", "hard_target", "pass",
    0, 1, "less_than_or_equal", "standard_errors",
    "typed F008 fixture", "Targets are stable across registered starts.",
    "vote")
  artifact <- sconjoint:::.scmix_v22_comp_sign(list(
    protocol_id = sconjoint:::.scmix_v22_protocol_id,
    protocol_mode = "production",
    analysis_contract_fingerprint = payload$fingerprint,
    registry_fingerprint = sconjoint:::.scmix_v22_fingerprint(list(spec)),
    calibration_artifact_fingerprint = calibration$fingerprint,
    gates = f008_gate), "scmix_v22_multistart_stability")
  computation <- sconjoint:::.scmix_v22_computation_gate_rows(
    list(artifact), payload, list(spec), calibration)
  observed_f008 <- computation$rows[
    computation$rows$gate_id == "F008", , drop = FALSE]
  expect_identical(observed_f008$state, "pass")
  expect_identical(unname(computation$fingerprints[["F008"]]),
                   artifact$fingerprint)

  ## Exercise the public integration point, including audit output that binds
  ## the exact authenticated artifact fingerprint.
  n <- 30L
  influence <- matrix(seq(-1, 1, length.out = n), ncol = 1L,
                      dimnames = list(sprintf("r%03d", seq_len(n)), "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .52), influence)
  dml$v22_analysis_contract <- payload
  finalized <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .51)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = calibration,
    external_gates = .v22_external_gates(list(spec), payload),
    computation_artifacts = list(artifact),
    complete_external_record = TRUE)
  finalized_f008 <- finalized$computation_gates[
    finalized$computation_gates$gate_id == "F008", , drop = FALSE]
  expect_identical(finalized_f008$state, "pass")
  expect_identical(
    unname(finalized$computation_artifact_fingerprints[["F008"]]),
    artifact$fingerprint)
  expect_error(sconjoint:::.scmix_v22_computation_gate_rows(
    list(artifact, artifact), payload, list(spec), calibration),
    "at most one exact artifact")

  inconsistent <- unclass(artifact); inconsistent$fingerprint <- NULL
  inconsistent$gates$state <- "not_applicable"
  inconsistent$gates$message <- "F008 does not apply to a single start."
  inconsistent <- sconjoint:::.scmix_v22_comp_sign(
    inconsistent, "scmix_v22_multistart_stability")
  expect_error(sconjoint:::.scmix_v22_computation_gate_rows(
    list(inconsistent), payload, list(spec), calibration),
    "disagree with applicability")
})

test_that("missing honest outer-fold information stays NA without -Inf warnings", {
  N <- 30L
  IF <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
               dimnames = list(sprintf("r%03d", seq_len(N)), "p"))
  dml <- .v22_fake_dml(c(p = 0.6), c(p = 0.62), IF)
  dml$fold_details <- lapply(dml$fold_details, function(x) {
    x$honest_evaluation_information <- NULL
    x
  })
  spec <- scmix_v22_target_spec(
    "vote", "single", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean")
  expect_silent(out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = 0.61)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE))
  expect_true(is.na(out$targets$vote$honest_riesz$maximum_null))
  expect_true("R002" %in% out$targets$vote$status$hard_failure_codes)
})

test_that("C003 uses distinct scalar-chart and nonlinear-map calibrations", {
  N <- 50L
  ids <- sprintf("r%03d", seq_len(N))
  x <- seq(-1, 1, length.out = N)
  scalar_dml <- .v22_fake_dml(c(p = 0.6), c(p = 0.65),
                              matrix(x, ncol = 1L,
                                     dimnames = list(ids, "p")))
  scalar <- scmix_v22_target_spec(
    "vote", "single", "probability", "p", "primitive:p",
    "A rather than B", "probability", "mean")
  scalar_calibration <- .v22_calibration
  scalar_calibration$constants$chart_discrepancy_cutoff <- NULL
  scalar_out <- scmix_v22_finalize(
    scalar_dml, list(scalar),
    full_sample_plugin = .v22_full_plugin(scalar_dml, c(p = 0.61)),
    derivative_audit = .v22_derivative(scalar_dml, list(scalar)),
    calibration = scalar_calibration,
    external_gates = .v22_external_gates(
      list(scalar), scalar_dml$v22_analysis_contract),
    complete_external_record = TRUE)
  scalar_c3 <- scalar_out$targets$vote$status$gate_record
  scalar_c3 <- scalar_c3[scalar_c3$gate_id == "C003", , drop = FALSE]
  expect_identical(scalar_c3$state, "not_run")
  expect_identical(scalar_out$targets$vote$status$target_status, "withheld")

  IF <- cbind(m = x, second = x^2 - mean(x^2), hr = sin(seq_len(N)))
  rownames(IF) <- ids
  composite_dml <- .v22_fake_direct_heterogeneity(
    c(m = 0.2, second = 0.14, hr = 0.20),
    c(m = 0.21, second = 0.15, hr = 0.19), IF)
  composite <- scmix_v22_target_spec(
    "heterogeneity", "heterogeneity", "directional_heterogeneity",
    c("m", "second", "hr"), c("primitive:HZ", "primitive:HR"),
    "declared direction", "share", c("mean", "loading"))
  composite_calibration <- .v22_calibration
  composite_calibration$constants$nonlinear_remainder_cutoff <- NULL
  composite_out <- scmix_v22_finalize(
    composite_dml, list(composite),
    full_sample_plugin = .v22_full_plugin(
      composite_dml, c(`primitive:HZ` = 0.10, `primitive:HR` = 0.20)),
    derivative_audit = .v22_derivative(composite_dml, list(composite)),
    calibration = composite_calibration,
    external_gates = .v22_external_gates(
      list(composite), composite_dml$v22_analysis_contract),
    complete_external_record = TRUE)
  composite_c3 <- composite_out$targets$heterogeneity$status$gate_record
  composite_c3 <- composite_c3[composite_c3$gate_id == "C003", , drop = FALSE]
  expect_identical(composite_c3$state, "not_run")
  expect_identical(composite_out$targets$heterogeneity$status$target_status,
                   "withheld")
})

test_that("directional heterogeneity uses the minimal HZ and HR vector", {
  N <- 100L
  r <- seq(-1, 1, length.out = N)
  IF <- cbind(m = r, second = r^2 - mean(r^2), hr = sin(seq_len(N)))
  rownames(IF) <- sprintf("r%03d", seq_len(N))
  dml <- .v22_fake_direct_heterogeneity(
    c(m = 0.2, second = 0.14, hr = 0.20),
    c(m = 0.2, second = -0.06, hr = 0.15), IF)
  spec <- scmix_v22_target_spec(
    "heterogeneity", "heterogeneity_family", "directional_heterogeneity",
    c("m", "second", "hr"), c("primitive:HZ", "primitive:HR"),
    "positive declared contrast", "explained share", c("mean", "loading"))
  out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(
      dml, c(`primitive:HZ` = 0.12, `primitive:HR` = 0.18)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  z <- out$targets$heterogeneity
  expect_equal(z$crossfit_primitive[["primitive:HZ"]], 0.10,
               tolerance = 1e-12)
  expect_equal(z$raw_one_step_primitive[["primitive:HZ"]], -0.10,
               tolerance = 1e-12)
  expect_true(all(z$reported_primitive > 0))
  expect_true(z$reported_estimate >= 0 && z$reported_estimate <= 1)
  expect_identical(colnames(z$primitive_influence),
                   c("primitive:HZ", "primitive:HR"))

  origin <- sconjoint:::.scmix_v22_map_rectangle(
    "directional_heterogeneity", c(-0.2, -0.3), c(0.4, 0.5))
  expect_true(origin$nonempty)
  expect_true(origin$boundary_touch)
  expect_true(origin$uninformative)
  expect_equal(c(origin$lower, origin$upper), c(0, 1))
})

test_that("subgroup and ratio rectangles are mapped through constraints", {
  subgroup <- sconjoint:::.scmix_v22_map_rectangle(
    "subgroup_share", c(-0.1, 0.2), c(0.3, 0.8))
  expect_true(subgroup$nonempty)
  expect_equal(subgroup$lower, 0)
  expect_equal(subgroup$upper, 1)
  expect_true(subgroup$boundary_touch)

  mrs <- sconjoint:::.scmix_v22_map_rectangle(
    "mean_tradeoff_mrs_ratio", c(-1, -0.2), c(2, 0.3))
  expect_true(mrs$nonempty)
  expect_true(mrs$uninformative)
  expect_true(is.na(mrs$lower))
  expect_match(mrs$reason, "contains zero")
})

test_that("common multiplier banks are order and job-partition invariant", {
  ids <- sprintf("r%03d", seq_len(60L))
  contract <- .v22_contract()
  bank_a <- scmix_v22_multiplier_bank(
    ids, draws = 9999L, seed = 73L, analysis_contract = contract)
  bank_b <- scmix_v22_multiplier_bank(
    rev(ids), draws = 9999L, seed = 73L, analysis_contract = contract)
  expect_identical(bank_a$fingerprint, bank_b$fingerprint)
  expect_equal(bank_a$multipliers, bank_b$multipliers)

  IF <- cbind(a = seq(-1, 1, length.out = length(ids)),
              b = cos(seq_along(ids)))
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = 0.4, b = 0.7),
                       c(a = 0.45, b = 0.65), IF)
  family_ids <- c("primitive:a", "primitive:b")
  make <- function(id, column, primitive) scmix_v22_target_spec(
    id, "joint_family", "probability", column, primitive,
    paste(column, "positive"), "probability", "mean",
    family_primitive_registry_ids = family_ids,
    family_target_ids = c("target_a", "target_b"))
  a <- make("target_a", "a", "primitive:a")
  b <- make("target_b", "b", "primitive:b")
  full <- .v22_full_plugin(dml, c(a = 0.42, b = 0.68))
  gates <- .v22_external_gates(list(a, b), dml$v22_analysis_contract)
  args <- list(
    inference = dml, full_sample_plugin = full,
    derivative_audit = .v22_derivative(dml, list(a, b)),
    multiplier_bank = bank_a, calibration = .v22_calibration,
    external_gates = gates,
    complete_external_record = TRUE)
  out_ab <- do.call(scmix_v22_finalize, c(args, list(registry = list(a, b))))
  out_ba <- do.call(scmix_v22_finalize, c(args, list(registry = list(b, a))))
  expect_equal(out_ab$family_resampling$joint_family$critical,
               out_ba$family_resampling$joint_family$critical)
  expect_equal(out_ab$targets$target_a$diagnostic_family_interval,
               out_ba$targets$target_a$diagnostic_family_interval)
  expect_identical(out_ab$multiplier_fingerprint,
                   out_ba$multiplier_fingerprint)

  dml_a <- .v22_fake_dml(c(a = 0.4), c(a = 0.45), IF[, "a", drop = FALSE])
  dml_b <- .v22_fake_dml(c(b = 0.7), c(b = 0.65), IF[, "b", drop = FALSE])
  job_a <- scmix_v22_collect(
    dml_a, list(a), .v22_full_plugin(dml_a, c(a = 0.42)),
    derivative_audit = .v22_derivative(dml_a, list(a)))
  job_b <- scmix_v22_collect(
    dml_b, list(b), .v22_full_plugin(dml_b, c(b = 0.68)),
    derivative_audit = .v22_derivative(dml_b, list(b)))
  out_split <- scmix_v22_finalize_collections(
    list(job_a, job_b), multiplier_bank = bank_a,
    calibration = .v22_calibration,
    external_gates = gates,
    complete_external_record = TRUE)
  expect_equal(out_ab$family_resampling$joint_family$critical,
               out_split$family_resampling$joint_family$critical)
  expect_equal(out_ab$targets$target_a$diagnostic_family_interval,
               out_split$targets$target_a$diagnostic_family_interval)
  expect_identical(out_split$targets$target_a$status$multiplicity_status,
                   "none")

  incomplete <- scmix_v22_finalize_collections(
    list(job_a), multiplier_bank = bank_a,
    calibration = .v22_calibration,
    external_gates = gates,
    complete_external_record = TRUE)
  expect_false(incomplete$family_resampling$joint_family$membership_complete)
  expect_equal(incomplete$family_resampling$joint_family$critical,
               stats::qnorm(1 - 0.05 / (2 * length(family_ids))))
  expect_identical(incomplete$targets$target_a$status$multiplicity_status,
                   "none")
  expect_true(all(is.na(
    incomplete$targets$target_a$simultaneous_interval)))
  expect_true("P001" %in%
                incomplete$targets$target_a$status$hard_failure_codes)
})

test_that("duplicate primitive IDs cannot hide different influence values", {
  ids <- sprintf("r%03d", seq_len(40L))
  IF <- cbind(a = seq(-1, 1, length.out = 40L),
              b = seq(-1, 1, length.out = 40L)^3)
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = 0.4, b = 0.4), c(a = 0.45, b = 0.45), IF)
  a <- scmix_v22_target_spec(
    "a", "family", "probability", "a", "same-id", "positive",
    "probability", "mean", family_target_ids = c("a", "b"))
  b <- scmix_v22_target_spec(
    "b", "family", "probability", "b", "same-id", "positive",
    "probability", "mean", family_target_ids = c("a", "b"))
  out <- scmix_v22_finalize(
    dml, list(a, b),
    full_sample_plugin = .v22_full_plugin(dml, c(a = 0.4, b = 0.4)),
    derivative_audit = .v22_derivative(dml, list(a, b)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out$family_resampling$family$method, "none")
  expect_identical(out$family_resampling$family$conflict, "same-id")
  expect_identical(out$targets$a$status$multiplicity_status, "none")
  m2 <- out$targets$a$status$gate_record
  m2 <- m2[m2$gate_id == "M002", , drop = FALSE]
  expect_identical(m2$state, "fail")
})

test_that("an exactly shared primitive is deduplicated without losing status", {
  ids <- sprintf("r%03d", seq_len(28L))
  x <- seq(-1, 1, length.out = length(ids))
  IF <- cbind(a = x, b = x); rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = .4, b = .4), c(a = .41, b = .41), IF)
  make <- function(target, column) scmix_v22_target_spec(
    target, "f", "probability", column, "shared", "positive",
    "probability", "mean", family_target_ids = c("a", "b"),
    primitive_estimands = "same probability")
  a <- make("a", "a"); b <- make("b", "b")
  out <- scmix_v22_finalize(
    dml, list(a, b),
    full_sample_plugin = .v22_full_plugin(dml, c(a = .4, b = .4)),
    derivative_audit = .v22_derivative(dml, list(a, b)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out$family_resampling$f$method, "pointwise")
  expect_identical(out$targets$a$status$target_status, "withheld")
  expect_identical(out$targets$b$status$target_status, "withheld")
})

test_that("split jobs require one signed analysis and full-fit provenance", {
  ids <- sprintf("r%03d", seq_len(30L))
  IF <- cbind(a = seq(-1, 1, length.out = length(ids)),
              b = cos(seq_along(ids)))
  rownames(IF) <- ids
  dml_a <- .v22_fake_dml(c(a = .4), c(a = .41), IF[, "a", drop = FALSE])
  dml_b <- .v22_fake_dml(c(b = .5), c(b = .51), IF[, "b", drop = FALSE])
  family_ids <- c("pa", "pb")
  a <- scmix_v22_target_spec(
    "a", "f", "probability", "a", "pa", "positive", "probability",
    "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  b <- scmix_v22_target_spec(
    "b", "f", "probability", "b", "pb", "positive", "probability",
    "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  job_a <- scmix_v22_collect(
    dml_a, list(a), .v22_full_plugin(dml_a, c(a = .4)),
    derivative_audit = .v22_derivative(dml_a, list(a)))
  tampered_job <- job_a
  tampered_job$source_inference$fold_details[[1L]][[
    "honest_evaluation_gradient"]][1L, 1L] <- 2
  expect_error(scmix_v22_finalize_collections(list(tampered_job)),
               "failed fingerprint validation")
  other_plugin <- scmix_v22_full_plugin_artifact(
    dml_b, c(b = .5), dml_b$v22_analysis_contract,
    final_fit_hash = "different-final-fit", code_hash = "test-code",
    provenance = "deliberately different full fit")
  job_b_other_fit <- scmix_v22_collect(
    dml_b, list(b), other_plugin,
    derivative_audit = .v22_derivative(dml_b, list(b)))
  gates <- .v22_external_gates(list(a, b), dml_a$v22_analysis_contract)
  bank <- scmix_v22_multiplier_bank(
    ids, 9999L, 73L, analysis_contract = dml_a$v22_analysis_contract)
  out <- scmix_v22_finalize_collections(
    list(job_a, job_b_other_fit), multiplier_bank = bank,
    calibration = .v22_calibration, external_gates = gates,
    complete_external_record = TRUE)
  expect_true("P002" %in% out$targets$a$status$hard_failure_codes)

  altered_contract <- dml_b$v22_analysis_contract
  altered_contract$data_hash <- "different-data"
  payload <- unclass(altered_contract); payload$fingerprint <- NULL
  altered_contract$fingerprint <-
    sconjoint:::.scmix_v22_fingerprint(payload)
  dml_b$v22_analysis_contract <- altered_contract
  dml_b$v22_riesz_analysis_contract_fingerprint <- altered_contract$fingerprint
  job_b_other_analysis <- scmix_v22_collect(
    dml_b, list(b), .v22_full_plugin(dml_b, c(b = .5)),
    derivative_audit = .v22_derivative(dml_b, list(b)))
  expect_error(scmix_v22_finalize_collections(
    list(job_a, job_b_other_analysis), multiplier_bank = bank,
    calibration = .v22_calibration, external_gates = gates,
    complete_external_record = TRUE), "same signed folds, data, design")
})

test_that("diagnostic multiplier counts cannot certify final simultaneity", {
  ids <- sprintf("r%03d", seq_len(40L))
  IF <- cbind(a = seq(-1, 1, length.out = 40L),
              b = cos(seq_len(40L)))
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = 0.4, b = 0.5), c(a = 0.42, b = 0.52), IF)
  family_ids <- c("primitive:a", "primitive:b")
  a <- scmix_v22_target_spec(
    "a", "family", "probability", "a", "primitive:a", "positive",
    "probability", "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  b <- scmix_v22_target_spec(
    "b", "family", "probability", "b", "primitive:b", "positive",
    "probability", "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  out <- scmix_v22_finalize(
    dml, list(a, b),
    full_sample_plugin = .v22_full_plugin(dml, c(a = 0.4, b = 0.5)),
    derivative_audit = .v22_derivative(dml, list(a, b)),
    multiplier_bank = scmix_v22_multiplier_bank(
      ids, 199L, 73L, analysis_contract = dml$v22_analysis_contract),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out$family_resampling$family$method,
                   "fixed_family_bonferroni")
  expect_identical(out$family_resampling$family$diagnostic_method,
                   "common_draw_max_t")
  expect_false(out$family_resampling$family$final_draw_count)
  expect_identical(out$targets$a$status$multiplicity_status, "none")
  expect_true(all(is.na(out$targets$a$simultaneous_interval)))
  m3 <- out$targets$a$status$gate_record
  m3 <- m3[m3$gate_id == "M003", , drop = FALSE]
  expect_identical(m3$state, "fail")
})

test_that("the gate template is exhaustive and malformed records are rejected", {
  g <- scmix_v22_gate_template()
  expect_setequal(g$gate_id, sconjoint:::.scmix_v22_required_gate_ids)
  expect_silent(scmix_v22_validate_gate_record(g))
  expect_error(scmix_v22_validate_gate_record(g[-1L, ]), "incomplete")

  duplicate <- rbind(g, g[1L, ])
  expect_error(scmix_v22_validate_gate_record(duplicate), "duplicated")

  weakened <- g
  weakened$severity[weakened$gate_id == "R003"] <- "warning"
  expect_error(scmix_v22_validate_gate_record(weakened),
               "changes frozen")
  relabeled <- g
  relabeled$scope[relabeled$gate_id == "F005"] <- "fit"
  expect_error(scmix_v22_validate_gate_record(relabeled),
               "changes frozen")
  unscoped <- g
  unscoped$scope_id[unscoped$gate_id == "R001"] <- NA_character_
  expect_error(scmix_v22_validate_gate_record(unscoped),
               "explicit scope_id")
})

test_that("only exact contract-linked calibration artifacts can certify", {
  contract <- .v22_contract()
  expect_true(sconjoint:::.scmix_v22_validate_calibration(
    .v22_calibration, contract))
  changed <- .v22_calibration_values
  changed$ridge_cutoff <- changed$ridge_cutoff + 0.01
  expect_error(scmix_v22_calibration_artifact(
    changed, contract, .v22_calibration_source_hash, "tampered values"),
    "do not match the signed analysis contract")

  ids <- sprintf("r%03d", seq_len(25L))
  IF <- matrix(seq(-1, 1, length.out = 25L), ncol = 1L,
               dimnames = list(ids, "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .51), IF)
  spec <- scmix_v22_target_spec(
    "p", "single", "probability", "p", "primitive:p", "positive",
    "probability", "mean")
  out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .5)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration_values,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out$protocol_status,
                   "implementation_complete_calibration_or_gates_pending")
  expect_true("P002" %in% out$targets$p$status$hard_failure_codes)
  c3 <- out$targets$p$status$gate_record
  c3 <- c3[c3$gate_id == "C003", , drop = FALSE]
  expect_identical(c3$state, "not_run")
})

test_that("a failed family primitive prevents simultaneous-complete status", {
  ids <- sprintf("r%03d", seq_len(45L))
  IF <- cbind(a = seq(-1, 1, length.out = length(ids)),
              b = cos(seq_along(ids)))
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = .4, b = .55), c(a = .41, b = .56), IF)
  family_ids <- c("primitive:a", "primitive:b")
  make <- function(id, column, primitive) scmix_v22_target_spec(
    id, "f", "probability", column, primitive, "positive",
    "probability", "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  a <- make("a", "a", "primitive:a")
  b <- make("b", "b", "primitive:b")
  coordinates <- c(u1 = 0, u2 = 0)
  analytic <- diag(2L)
  dimnames(analytic) <- list(names(coordinates), family_ids)
  analytic[, 2L] <- 0
  derivative <- scmix_v22_derivative_audit(
    dml, list(fold1 = coordinates, fold2 = coordinates), family_ids,
    evaluate = function(fold_id, coordinates, primitive_registry_ids) {
      stats::setNames(as.numeric(coordinates), primitive_registry_ids)
    }, analytic_gradient = list(fold1 = analytic, fold2 = analytic),
    code_hash = "test-code", provenance = "one deliberately failed primitive")
  bank <- scmix_v22_multiplier_bank(
    ids, 9999L, 73L, analysis_contract = dml$v22_analysis_contract)
  out <- scmix_v22_finalize(
    dml, list(a, b),
    full_sample_plugin = .v22_full_plugin(dml, c(a = .4, b = .55)),
    derivative_audit = derivative, multiplier_bank = bank,
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  fam <- out$family_resampling$f
  expect_identical(fam$method, "fixed_family_bonferroni")
  expect_identical(fam$diagnostic_method, "common_draw_max_t")
  expect_false(fam$primitive_gate_pass[["primitive:b"]])
  expect_identical(out$targets$a$status$target_status, "withheld")
  expect_identical(out$targets$a$status$multiplicity_status, "none")
  expect_identical(out$targets$b$status$target_status, "withheld")
})

test_that("multiplier values and analysis linkage are revalidated", {
  ids <- sprintf("r%03d", seq_len(30L))
  contract <- .v22_contract()
  bank <- scmix_v22_multiplier_bank(
    ids, 9999L, 73L, analysis_contract = contract)
  changed <- bank
  changed$multipliers[1L, 1L] <- changed$multipliers[1L, 1L] + 1
  expect_error(sconjoint:::.scmix_v22_validate_bank(
    changed, ids, contract), "integrity check")
  expect_error(scmix_v22_multiplier_bank(
    ids, 9999L, 72L, analysis_contract = contract),
    "signed analysis contract")

  IF <- cbind(a = seq(-1, 1, length.out = 30L),
              b = cos(seq_len(30L)))
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = 0.4, b = 0.5), c(a = 0.41, b = 0.51), IF)
  other <- .v22_contract()
  other$data_hash <- "different-data"
  payload <- unclass(other); payload$fingerprint <- NULL
  other$fingerprint <- sconjoint:::.scmix_v22_fingerprint(payload)
  family_ids <- c("pa", "pb")
  a <- scmix_v22_target_spec(
    "a", "f", "probability", "a", "pa", "positive", "probability",
    "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  b <- scmix_v22_target_spec(
    "b", "f", "probability", "b", "pb", "positive", "probability",
    "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  foreign_bank <- scmix_v22_multiplier_bank(
    ids, 9999L, 73L, analysis_contract = other)
  out <- scmix_v22_finalize(
    dml, list(a, b),
    full_sample_plugin = .v22_full_plugin(dml, c(a = .4, b = .5)),
    derivative_audit = .v22_derivative(dml, list(a, b)),
    multiplier_bank = foreign_bank, calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out$family_resampling$f$method,
                   "fixed_family_bonferroni")
  expect_false(out$family_resampling$f$analysis_contract_linked)
})

test_that("full-fit plugin coverage and primitive metadata fail closed", {
  ids <- sprintf("r%03d", seq_len(35L))
  IF <- cbind(a = seq(-1, 1, length.out = 35L),
              b = seq(-1, 1, length.out = 35L))
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = .4, b = .4), c(a = .41, b = .41), IF)
  family_targets <- c("a", "b")
  a <- scmix_v22_target_spec(
    "a", "f", "probability", "a", "pa", "positive", "probability",
    "mean", family_primitive_registry_ids = c("pa", "pb"),
    family_target_ids = family_targets,
    primitive_estimands = "same probability")
  b <- scmix_v22_target_spec(
    "b", "f", "probability", "b", "pb", "positive", "probability",
    "mean", family_primitive_registry_ids = c("pa", "pb"),
    family_target_ids = family_targets,
    primitive_estimands = "same probability")
  missing_plugin <- .v22_full_plugin(dml, c(a = .4))
  out_missing <- scmix_v22_finalize(
    dml, list(a, b), full_sample_plugin = missing_plugin,
    derivative_audit = .v22_derivative(dml, list(a, b)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out_missing$targets$a$status$fit_status, "withheld")
  expect_true("P002" %in% out_missing$targets$a$status$hard_failure_codes)

  a_bad <- scmix_v22_target_spec(
    "a", "f", "probability", "a", "same", "positive", "probability",
    "mean", family_target_ids = family_targets,
    primitive_estimands = "same probability")
  b_bad <- scmix_v22_target_spec(
    "b", "f", "probability", "b", "same", "negative", "probability",
    "mean", family_target_ids = family_targets,
    primitive_estimands = "same probability")
  expect_error(scmix_v22_finalize(
    dml, list(a_bad, b_bad),
    full_sample_plugin = .v22_full_plugin(dml, c(a = .4, b = .4)),
    derivative_audit = .v22_derivative(dml, list(a_bad, b_bad)),
    calibration = .v22_calibration,
    external_gates = NULL, complete_external_record = TRUE),
    "Duplicate primitive IDs have incompatible")
})

test_that("a supplied bank never changes a single-primitive family to max-t", {
  ids <- sprintf("r%03d", seq_len(25L))
  IF <- matrix(seq(-1, 1, length.out = 25L), ncol = 1L,
               dimnames = list(ids, "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .51), IF)
  spec <- scmix_v22_target_spec(
    "p", "single", "probability", "p", "primitive:p", "positive",
    "probability", "mean")
  out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .5)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    multiplier_bank = scmix_v22_multiplier_bank(
      ids, 9999L, 73L, analysis_contract = dml$v22_analysis_contract),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(out$family_resampling$single$method, "pointwise")
  expect_identical(out$targets$p$status$multiplicity_status, "none")
})

test_that("exact derivative audit uses signed structural h and h/2 geometry", {
  fit <- list(analysis_signature = "derivative-fit")
  fit$v22_analysis_contract <- .v22_contract(
    "derivative-fit", coordinate_registry = c("u1", "u2"),
    derivative_subset_rule = "all coordinates")
  u <- c(u1 = 2, u2 = 0.25)
  folds <- list(fold1 = u, fold2 = u + c(0.1, -0.05))
  evaluator <- function(fold_id, coordinates, primitive_registry_ids) {
    stats::setNames(c(
      coordinates[["u1"]]^2 + 3 * coordinates[["u2"]],
      exp(coordinates[["u1"]]) - coordinates[["u2"]]),
      primitive_registry_ids)
  }
  analytic_one <- function(x) {
    out <- rbind(c(2 * x[["u1"]], exp(x[["u1"]])), c(3, -1))
    dimnames(out) <- list(c("u1", "u2"), c("p1", "p2"))
    out
  }
  audit <- scmix_v22_derivative_audit(
    fit, folds, c("p1", "p2"), evaluator,
    analytic_gradient = list(fold1 = analytic_one(folds$fold1),
                             fold2 = analytic_one(folds$fold2)),
    coordinate_subset = c("u1", "u2"), subset_rule = "all coordinates",
    code_hash = "test-code", provenance = "executed derivative audit")
  expect_s3_class(audit, "scmix_v22_derivative_audit")
  expected_h <- .Machine$double.eps^(1 / 3) * pmax(1, abs(u))
  names(expected_h) <- names(u)
  expect_equal(audit$fold_artifacts$fold1$steps_h, expected_h,
               tolerance = 0)
  expect_equal(
    vapply(audit$fold_artifacts$fold1$evaluations, `[[`, numeric(1L),
           "h_over_2"), expected_h / 2, tolerance = 0)
  expect_true(all(is.finite(audit$metric)))
  expect_lt(max(audit$metric), 1e-6)
  expect_true(audit$analytic_checked)

  expect_error(scmix_v22_derivative_audit(
    fit, folds, c("p1", "p2"), evaluator,
    coordinate_subset = c("u2", "u1"), subset_rule = "reversed",
    code_hash = "test-code", provenance = "test"),
    "signed structural-coordinate order")
  expect_error(scmix_v22_derivative_audit(
    fit, folds, c("p1", "p2"), evaluator,
    coordinate_subset = "u1", subset_rule = "",
    code_hash = "test-code", provenance = "test"),
    "subset_rule")
  wrong_order <- function(fold_id, coordinates, primitive_registry_ids) {
    stats::setNames(c(1, 2), rev(primitive_registry_ids))
  }
  expect_error(scmix_v22_derivative_audit(
    fit, folds, c("p1", "p2"), wrong_order,
    subset_rule = "all coordinates",
    code_hash = "test-code", provenance = "test"),
    "exact signed registry order")
})

test_that("legacy raw-coordinate refinement cannot pass R001", {
  ids <- sprintf("r%03d", seq_len(25L))
  IF <- matrix(seq(-1, 1, length.out = 25L), ncol = 1L,
               dimnames = list(ids, "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .51), IF)
  spec <- scmix_v22_target_spec(
    "p", "single", "probability", "p", "primitive:p", "positive",
    "probability", "mean")
  out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .5)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  r1 <- out$targets$p$status$gate_record
  r1 <- r1[r1$gate_id == "R001", , drop = FALSE]
  expect_identical(r1$state, "not_run")
  expect_identical(out$targets$p$status$target_status, "withheld")
})

test_that("Riesz evidence cannot be self-certified by flags or resigned traces", {
  ids <- sprintf("r%03d", seq_len(32L))
  IF <- matrix(seq(-1, 1, length.out = length(ids)), ncol = 1L,
               dimnames = list(ids, "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .51), IF)
  spec <- scmix_v22_target_spec(
    "p", "single", "probability", "p", "p", "positive",
    "probability", "mean")
  finish <- function(x) scmix_v22_finalize(
    x, list(spec),
    full_sample_plugin = .v22_full_plugin(x, c(p = .5)),
    derivative_audit = .v22_derivative(x, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), x$v22_analysis_contract),
    complete_external_record = TRUE)

  flags_only <- dml
  flags_only$v22_riesz_evidence <- NULL
  out_flags <- finish(flags_only)
  expect_true(all(c("P002", "N002") %in%
                    out_flags$targets$p$status$hard_failure_codes))

  resigned <- dml
  evidence <- resigned$v22_riesz_evidence
  trace <- evidence$inner_refit_trace
  trace$trace$validation_risk[[1L]][1L, 1L] <-
    trace$trace$validation_risk[[1L]][1L, 1L] + 1
  payload <- unclass(trace); payload$fingerprint <- NULL
  trace$fingerprint <- sconjoint:::.scmix_v22_fingerprint(payload)
  evidence$inner_refit_trace <- trace
  evidence$inner_refit_trace_fingerprint <- trace$fingerprint
  payload <- unclass(evidence); payload$fingerprint <- NULL
  evidence$fingerprint <- sconjoint:::.scmix_v22_fingerprint(payload)
  resigned$v22_riesz_evidence <- evidence
  resigned$v22_riesz_evidence_fingerprint <- evidence$fingerprint
  out_resigned <- finish(resigned)
  expect_true(all(c("P002", "N002") %in%
                    out_resigned$targets$p$status$hard_failure_codes))
})

test_that("R004 requires zero and every adjacent selected-ridge value", {
  ids <- sprintf("r%03d", seq_len(32L))
  IF <- matrix(seq(-1, 1, length.out = length(ids)), ncol = 1L,
               dimnames = list(ids, "p"))
  dml <- .v22_fake_dml(c(p = .5), c(p = .51), IF)
  artifact <- dml$fold_details[[1L]]$ridge_neighborhood_artifact$p
  artifact$nu <- artifact$nu[1L]
  artifact$raw_one_step <- artifact$raw_one_step[1L]
  artifact$standard_error <- artifact$standard_error[1L]
  artifact$raw_wald_lower <- artifact$raw_wald_lower[1L]
  artifact$raw_wald_upper <- artifact$raw_wald_upper[1L]
  artifact$influence <- artifact$influence[, 1L, drop = FALSE]
  dml$fold_details[[1L]]$ridge_neighborhood_artifact$p <- artifact
  spec <- scmix_v22_target_spec(
    "p", "single", "probability", "p", "p", "positive",
    "probability", "mean")
  out <- scmix_v22_finalize(
    dml, list(spec),
    full_sample_plugin = .v22_full_plugin(dml, c(p = .5)),
    derivative_audit = .v22_derivative(dml, list(spec)),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(spec), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  r4 <- out$targets$p$status$gate_record
  r4 <- r4[r4$gate_id == "R004", , drop = FALSE]
  expect_identical(r4$state, "not_run")
  expect_true("R004" %in% out$targets$p$status$hard_failure_codes)
})

test_that("unconditional gates and derived applicability cannot be waived", {
  contract <- .v22_contract(q = 0L,
                            fit_uses_numerical_integration = FALSE)
  spec <- scmix_v22_target_spec(
    "p", "single", "probability", "p", "primitive:p", "positive",
    "probability", "mean", uses_numerical_integration = FALSE)
  unconditional <- scmix_v22_gate_record(
    "F001", "fit_computation", "fit", "hard_fit", "not_applicable",
    FALSE, TRUE, "equal", "boolean", "audit",
    "Not applicable by unsupported assertion", contract$specification_id)
  expect_error(scmix_v22_validate_gate_record(
    unconditional, require_all_ids = FALSE), "default-always")

  wrong_conditional <- scmix_v22_gate_record(
    "I001", "numerical_integration", "target", "hard_target", "pass",
    0, 1, "less_than_or_equal", "standard_errors", "audit",
    "unsupported pass", "p")
  expect_error(scmix_v22_gate_artifact(
    wrong_conditional, list(spec), contract, "audit"),
    "may not supply typed computation")
})

test_that("gate comparator, units, and mechanically evaluable state are frozen", {
  valid <- scmix_v22_gate_record(
    "F005", "fit_computation", "fit_block", "hard_fit", "pass",
    .1, .2, "less_than_or_equal", "dimensionless", "audit",
    "valid synthetic KKT row", "position_effect")
  expect_silent(scmix_v22_validate_gate_record(
    valid, require_all_ids = FALSE))

  wrong_comparator <- valid
  wrong_comparator$comparator <- "greater_than_or_equal"
  expect_error(scmix_v22_validate_gate_record(
    wrong_comparator, require_all_ids = FALSE), "comparator, or units")
  wrong_units <- valid
  wrong_units$units <- "boolean"
  expect_error(scmix_v22_validate_gate_record(
    wrong_units, require_all_ids = FALSE), "comparator, or units")

  inconsistent <- valid
  inconsistent$statistic[[1L]] <- .3
  expect_error(scmix_v22_validate_gate_record(
    inconsistent, require_all_ids = FALSE), "state disagrees")
  silently_skipped <- valid
  silently_skipped$state <- "not_run"
  expect_error(scmix_v22_validate_gate_record(
    silently_skipped, require_all_ids = FALSE), "state disagrees")

  warning_row <- scmix_v22_gate_record(
    "C004", "target_constraint", "target", "warning", "warn",
    TRUE, FALSE, "equal", "boolean", "audit",
    "raw point outside support", "p")
  expect_silent(scmix_v22_validate_gate_record(
    warning_row, require_all_ids = FALSE))
  expect_error(scmix_v22_gate_record(
    "F005", "fit_computation", "fit_block", "hard_fit", "pass",
    TRUE, TRUE, "equal", "boolean", "audit", "wrong schema",
    "position_effect"), "frozen comparator")
})

test_that("calibration cannot certify without the package-shipped Step-3 manifest", {
  contract <- .v22_contract()
  expect_error(testthat::with_mocked_bindings(
    scmix_v22_calibration_artifact(
      .v22_calibration_values, contract, .v22_calibration_source_hash,
      "audit"),
    .scmix_v22_approved_calibration_manifest = function() NULL,
    .package = "sconjoint"), "package-shipped Step-3")

  changed <- .v22_calibration_values
  changed$chart_discrepancy_cutoff <- 1e99
  changed_source <- "posthoc-source"
  changed_contract <- .v22_contract()
  changed_contract$calibration_hash <- scmix_v22_calibration_hash(
    changed, changed_source)
  payload <- unclass(changed_contract); payload$fingerprint <- NULL
  changed_contract$fingerprint <- sconjoint:::.scmix_v22_fingerprint(payload)
  expect_error(scmix_v22_calibration_artifact(
    changed, changed_contract, changed_source, "posthoc"),
    "package-shipped Step-3")
})

test_that("the derivative subset and rule are frozen in the analysis contract", {
  fit <- list(analysis_signature = "derivative-prefit")
  fit$v22_analysis_contract <- .v22_contract(
    "derivative-prefit", coordinate_registry = c("u1", "u2"),
    coordinate_subset = "u1", derivative_subset_rule = "first coordinate")
  folds <- list(fold1 = c(u1 = 0, u2 = 0),
                fold2 = c(u1 = 0, u2 = 0))
  evaluator <- function(fold_id, coordinates, primitive_registry_ids) {
    stats::setNames(coordinates[["u1"]], primitive_registry_ids)
  }
  expect_s3_class(scmix_v22_derivative_audit(
    fit, folds, "p", evaluator, coordinate_subset = "u1",
    subset_rule = "first coordinate", code_hash = "test-code",
    provenance = "prefit subset"), "scmix_v22_derivative_audit")
  expect_error(scmix_v22_derivative_audit(
    fit, folds, "p", evaluator, coordinate_subset = c("u1", "u2"),
    subset_rule = "all coordinates", code_hash = "test-code",
    provenance = "posthoc expansion"), "must pre-freeze")
})

test_that("every signed target must be collected even when primitives overlap", {
  N <- 50L; ids <- sprintf("r%03d", seq_len(N))
  IF <- cbind(HZ = seq(-1, 1, length.out = N),
              HR = sin(seq_len(N)))
  rownames(IF) <- ids
  dml <- .v22_fake_dml(c(HZ = .1, HR = .2), c(HZ = .11, HR = .19),
                       IF, primitive_mode = TRUE)
  common <- list(
    family_primitive_registry_ids = c("HZ", "HR"),
    family_target_ids = c("a", "b"),
    primitive_estimands = c("explained variance", "residual variance"),
    primitive_orientations = c("direction", "direction"),
    primitive_units = c("variance", "variance"),
    primitive_reference_rules = c("none", "none"),
    primitive_influence_definitions = rep(
      "centered_raw_one_step_respondent_sequence", 2L))
  make <- function(id) do.call(scmix_v22_target_spec, c(list(
    target_id = id, family_id = "f",
    target_type = "directional_heterogeneity",
    supporting_names = c("m", "second", "hr"),
    primitive_registry_ids = c("HZ", "HR"), orientation = "direction",
    units = "share", parameter_dependencies = c("mean", "loading")),
    common))
  a <- make("a"); b <- make("b")
  out <- scmix_v22_finalize(
    dml, list(a),
    full_sample_plugin = .v22_full_plugin(dml, c(HZ = .1, HR = .2)),
    derivative_audit = .v22_derivative(dml, list(a)),
    multiplier_bank = scmix_v22_multiplier_bank(
      ids, 9999L, 73L, analysis_contract = dml$v22_analysis_contract),
    calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  expect_identical(names(out$targets), "a")
  expect_identical(out$targets$a$status$fit_status, "withheld")
  expect_true("P001" %in% out$targets$a$status$hard_failure_codes)
  expect_identical(out$targets$a$status$multiplicity_status, "none")
})

test_that("ridge-neighborhood max-t endpoints reuse draws but recompute critical values", {
  N <- 80L; ids <- sprintf("r%03d", seq_len(N))
  a_if <- seq(-1, 1, length.out = N)
  b_if <- a_if + .01 * sin(seq_len(N))
  alt_if <- a_if + .1 * rep(c(-1, 1), length.out = N)
  IF <- cbind(a = a_if, b = b_if); rownames(IF) <- ids
  dml <- .v22_fake_dml(c(a = .4, b = .6), c(a = .41, b = .61), IF)
  for (k in seq_along(dml$fold_details)) {
    fd <- dml$fold_details[[k]]
    grid <- c(0, fd$training_n^(-1))
    art <- fd$ridge_neighborhood_artifact$a
    art$nu <- grid
    art$raw_one_step <- c(.41, .41)
    art$standard_error <- c(stats::sd(a_if), stats::sd(alt_if)) / sqrt(N)
    art$influence <- cbind(a_if, alt_if)
    rownames(art$influence) <- ids
    fd$ridge_neighborhood_artifact$a <- art
    dml$fold_details[[k]] <- fd
  }
  family_ids <- c("pa", "pb")
  make <- function(id, supporting, primitive) scmix_v22_target_spec(
    id, "f", "probability", supporting, primitive, "positive",
    "probability", "mean", family_primitive_registry_ids = family_ids,
    family_target_ids = c("a", "b"))
  a <- make("a", "a", "pa"); b <- make("b", "b", "pb")
  bank <- scmix_v22_multiplier_bank(
    ids, 9999L, 73L, analysis_contract = dml$v22_analysis_contract)
  out <- scmix_v22_finalize(
    dml, list(a, b),
    full_sample_plugin = .v22_full_plugin(dml, c(a = .4, b = .6)),
    derivative_audit = .v22_derivative(dml, list(a, b)),
    multiplier_bank = bank, calibration = .v22_calibration,
    external_gates = .v22_external_gates(
      list(a, b), dml$v22_analysis_contract),
    complete_external_record = TRUE)
  zeta <- sconjoint:::.scmix_v22_validate_bank(
    bank, ids, dml$v22_analysis_contract)
  alt_family <- cbind(pa = alt_if, pb = dml$influence[, "b"])
  se <- apply(alt_family, 2L, stats::sd) / sqrt(N)
  manual <- unname(stats::quantile(apply(abs(sweep(
    crossprod(zeta, alt_family) / N, 2L, se, `/`)), 1L, max),
    .95, names = FALSE, type = 8))
  ev <- out$targets$a$ridge_neighborhood$evaluations
  perturbed <- ev[ev$nu > 0, , drop = FALSE]
  expect_true(nrow(perturbed) > 0L)
  expect_equal(unique(perturbed$critical_value), manual)
  baseline <- if (!is.null(out$family_resampling$f$diagnostic_critical)) {
    out$family_resampling$f$diagnostic_critical
  } else out$family_resampling$f$critical
  expect_false(isTRUE(all.equal(manual, baseline)))
})
