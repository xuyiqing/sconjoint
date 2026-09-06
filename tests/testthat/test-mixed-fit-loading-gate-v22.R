.fit_gate_fixture <- function() {
  protocol <- sconjoint:::.scmix_v22_protocol_id
  registry <- list(scmix_v22_target_spec(
    "vote", "headline", "probability", "p_vote", "primitive:vote",
    "A over B", "probability", "mean"))
  specification_id <- "q1-primary"
  fold_ids <- c("fold_1", "fold_2")
  scope_ids <- c(paste(specification_id, "full", sep = "::"),
                 paste(specification_id, "outer", fold_ids, sep = "::"))
  starts <- expand.grid(fit_scope_id = scope_ids,
                        start_id = c("start_001", "start_002"),
                        stringsAsFactors = FALSE)
  starts$seed <- seq_len(nrow(starts)) + 100L
  fit_registry <- sconjoint:::.scmix_v22_comp_sign(list(
    protocol_id = protocol, application_id = "app",
    specification_id = specification_id, fit_id = "fit", q = 1L,
    integration_contract_fingerprint = "integration",
    structural_parameter_contract_fingerprint = "structural",
    uses_numerical_integration = TRUE, outer_fold_ids = fold_ids,
    fit_scope_ids = sort(scope_ids, method = "radix"),
    starts = starts[order(starts$fit_scope_id, starts$start_id), ]),
    "scmix_v22_fit_scope_registry")
  values <- stats::setNames(
    as.list(rep(1, length(sconjoint:::.scmix_v22_required_calibration_names))),
    sconjoint:::.scmix_v22_required_calibration_names)
  source_hash <- paste(rep("c", 64L), collapse = "")
  calibration_hash <- scmix_v22_calibration_hash(values, source_hash)
  contract <- scmix_v22_analysis_contract(
    "fit-gate-analysis", "app", specification_id, "primary", "all",
    "complete", "prespecified", stats::setNames(as.list(fold_ids), fold_ids),
    2L, "training only", "integration", fit_registry, "fixed",
    c("0", "n_k^-1", "n_k^-3/4", "n_k^-1/2"), list(multiplier = 1L),
    "code-hash", "data-hash", "design-hash", calibration_hash,
    1L, TRUE, TRUE, "u1", "u1", "all")
  approval <- list(protocol_id = protocol, status = "frozen_for_production",
                   rank_scope = 1L,
                   calibration_hash = calibration_hash,
                   source_hash = source_hash,
                   values_file =
                     sconjoint:::.scmix_v22_calibration_values_file,
                   constant_count = length(
                     sconjoint:::.scmix_v22_required_calibration_names),
                   constants = sconjoint:::.scmix_v22_normalize_calibration(
                     values, require_complete = TRUE),
                   source_verified = TRUE,
                   manifest_fingerprint = "test-manifest")
  testthat::local_mocked_bindings(
    .scmix_v22_approved_calibration_manifest = function() approval,
    .package = "sconjoint")
  calibration <- scmix_v22_calibration_artifact(
    values, contract, source_hash, "design-matched fixture")
  raw_scopes <- c("full-sample structural plug-in fit",
                  "outer-fold nuisance fit 1", "outer-fold nuisance fit 2")
  make_set <- function(j) {
    records <- lapply(seq_len(2L), function(s) {
      state_dict <- list(weight = matrix(j + s, 1L, 1L))
      f <- stats::setNames(lapply(seq_len(6L), function(k) list(
        state = "pass", pass = TRUE,
        threshold = if (k == 5L) values$kkt_cutoff else TRUE)),
        paste0("F00", 1:6))
      f$F005$observed_by_block <- data.frame(
        block = c("position_effect", "unpenalized_mean_reference",
                  "centered_mean_deviation", "horizontal_loadings",
                  "all_structural_coordinates"),
        applicable = TRUE, projected_kkt_rms = rep(0.01, 5L))
      list(start_id = sprintf("start_%03d", s),
           seed = starts$seed[starts$fit_scope_id == scope_ids[j] &
                                starts$start_id == sprintf("start_%03d", s)],
           selected = s == 1L, state_hash = paste0("state-", j, "-", s),
           protocol_mode = "production", calibration_hash = calibration_hash,
           scope = raw_scopes[j], artifact_fingerprint = paste0("artifact-", j, "-", s),
           network_state = list(state_dict = state_dict),
           v22_optimization_status = list(optimization_gate_pass = TRUE,
                                          gate_evidence = f))
    })
    structure(records, class = c("scmix_multistart_artifacts", "list"),
              artifact_set_fingerprint = paste0("set-", j))
  }
  evidence <- lapply(seq_along(raw_scopes), function(j) {
    x <- list(protocol_id = protocol, analysis_signature = "fit-gate-analysis",
      scope = raw_scopes[j], protocol_mode = "production",
      calibration_hash = calibration_hash,
      network_state_hash = sconjoint:::.scmix_stable_state_hash(
        list(weight = matrix(j + 1L, 1L, 1L))),
      evaluation_data_hash = paste0("data-", j),
      statistic = "fitted_probability_scale_invariance",
      maximum_absolute_probability_difference = 0,
      maximum_absolute_sequence_loglik_difference = 0,
      threshold = values$loading_probability_invariance_cutoff,
      state = "pass", pass = TRUE)
    class(x) <- c("scmix_loading_invariance_evidence", "list")
    x$evidence_fingerprint <- sconjoint:::.scmix_loading_invariance_fingerprint(x)
    x
  })
  fit <- structure(list(
    analysis_signature = "fit-gate-analysis", protocol_mode = "production",
    q = 1L, A_raw_full = matrix(.2), A_optimization_full = matrix(.4),
    A_raw_folds = list(matrix(.3), matrix(.4)),
    A_optimization_folds = list(matrix(.6), matrix(.8)),
    multistart_artifacts = list(full = make_set(1L),
                                folds = list(make_set(2L), make_set(3L))),
    loading_contract = list(
      contract_id = "scmix-loading-scale-v1",
      full = list(raw_field = "A_raw_full",
                  optimization_field = "A_optimization_full",
                  coefficient_scale = 2),
      folds = list(raw_field = "A_raw_folds",
                   optimization_field = "A_optimization_folds",
                   coefficient_scale = list(2, 2)),
      gate_evidence = list(L002 = list(fit_scoped_evidence = evidence)))),
    class = c("scmix", "list"))
  list(fit = fit, registry = registry, contract = contract,
       calibration = calibration, approval = approval)
}

test_that("fit/loading artifact covers full and every outer-fold fit", {
  fixture <- .fit_gate_fixture()
  testthat::local_mocked_bindings(
    .scmix_v22_approved_calibration_manifest = function() fixture$approval,
    .scmix_validate_multistart_artifacts = function(x) invisible(x),
    .scmix_validate_loading_contract = function(x) invisible(x$loading_contract),
    .package = "sconjoint")
  artifact <- scmix_v22_fit_loading_gate_artifact(
    fixture$fit, fixture$registry, fixture$contract, fixture$calibration,
    "unit-test fit audit")
  expect_s3_class(artifact, "scmix_v22_fit_loading_gate_artifact")
  expect_identical(names(artifact$fit_scopes), artifact$fit_scope_ids)
  expect_true(all(vapply(artifact$fit_scopes, function(x)
    all(vapply(x$gates, `[[`, logical(1L), "pass")), logical(1L))))
  expect_invisible(scmix_v22_validate_fit_loading_gate_artifact(
    artifact, fixture$contract, fixture$registry, fixture$calibration))

  failed <- fixture$fit
  failed$multistart_artifacts$full[[1L]]$v22_optimization_status$
    gate_evidence$F005$state <- "fail"
  failed$multistart_artifacts$full[[1L]]$v22_optimization_status$
    gate_evidence$F005$pass <- FALSE
  failed$multistart_artifacts$full[[1L]]$v22_optimization_status$
    optimization_gate_pass <- FALSE
  failed_artifact <- scmix_v22_fit_loading_gate_artifact(
    failed, fixture$registry, fixture$contract, fixture$calibration,
    "typed failing gate")
  expect_identical(failed_artifact$fit_scopes[[1L]]$gates$F005$state,
                   "fail")

  altered <- artifact
  altered$fit_scopes[[1L]]$gates$L002$observed <- 1
  expect_error(scmix_v22_validate_fit_loading_gate_artifact(
    altered, fixture$contract, fixture$registry, fixture$calibration),
    "tampered")

  missing <- fixture$fit
  missing$multistart_artifacts$folds <- missing$multistart_artifacts$folds[1]
  expect_error(scmix_v22_fit_loading_gate_artifact(
    missing, fixture$registry, fixture$contract, fixture$calibration, "missing"),
    "every outer-fold")
})

test_that("finalizer adapter binds only authenticated fit/loading rows", {
  fixture <- .fit_gate_fixture()
  testthat::local_mocked_bindings(
    .scmix_v22_approved_calibration_manifest = function() fixture$approval,
    .scmix_validate_multistart_artifacts = function(x) invisible(x),
    .scmix_validate_loading_contract = function(x) invisible(x$loading_contract),
    .package = "sconjoint")
  artifact <- scmix_v22_fit_loading_gate_artifact(
    fixture$fit, fixture$registry, fixture$contract, fixture$calibration,
    "adapter fixture")
  rows <- sconjoint:::.scmix_v22_fit_loading_gate_rows(
    artifact, fixture$contract, fixture$registry, fixture$calibration)
  expect_identical(sort(unique(rows$gate_id)),
                   sort(c(paste0("F00", 1:6), "L001", "L002")))
  expect_true(all(rows$state == "pass"))
  expect_true(all(grepl(artifact$fingerprint, rows$message, fixed = TRUE) |
                    rows$gate_id == "F005"))
  tampered <- artifact
  tampered$fit_scopes[[1L]]$selected_state_hash <- "tampered"
  expect_error(sconjoint:::.scmix_v22_fit_loading_gate_rows(
    tampered, fixture$contract, fixture$registry, fixture$calibration),
    "tampered")
})

test_that("fit/loading artifact rejects start and live-L002 tampering", {
  fixture <- .fit_gate_fixture()
  testthat::local_mocked_bindings(
    .scmix_v22_approved_calibration_manifest = function() fixture$approval,
    .scmix_validate_multistart_artifacts = function(x) invisible(x),
    .scmix_validate_loading_contract = function(x) invisible(x$loading_contract),
    .package = "sconjoint")
  fixture$fit$multistart_artifacts$full[[1L]]$seed <- 999L
  expect_error(scmix_v22_fit_loading_gate_artifact(
    fixture$fit, fixture$registry, fixture$contract, fixture$calibration, "bad"),
    "IDs and seeds")
  fixture <- .fit_gate_fixture()
  fixture$fit$loading_contract$gate_evidence$L002$
    fit_scoped_evidence[[1L]]$network_state_hash <- "wrong"
  fixture$fit$loading_contract$gate_evidence$L002$
    fit_scoped_evidence[[1L]]$evidence_fingerprint <-
      sconjoint:::.scmix_loading_invariance_fingerprint(
        fixture$fit$loading_contract$gate_evidence$L002$
          fit_scoped_evidence[[1L]])
  expect_error(scmix_v22_fit_loading_gate_artifact(
    fixture$fit, fixture$registry, fixture$contract, fixture$calibration, "bad"),
    "L001-L002")
})
