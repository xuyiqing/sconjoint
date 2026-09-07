#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

.v27a_test_script <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(hit)) {
    return(normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/",
                         mustWork = TRUE))
  }
  normalizePath("applications/br2017/R/test_br_v27_assessment.R",
                winslash = "/", mustWork = TRUE)
}

.v27a_root <- normalizePath(file.path(dirname(.v27a_test_script()), "..", "..",
                                      ".."), winslash = "/", mustWork = TRUE)

if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("testthat", quietly = TRUE)) {
  stop("pkgload and testthat are required.", call. = FALSE)
}
pkgload::load_all(.v27a_root, quiet = TRUE, export_all = FALSE)
sys.source(file.path(.v27a_root, "applications", "br2017", "R",
                     "br_v27_contract.R"), envir = .GlobalEnv)
sys.source(file.path(.v27a_root, "applications", "br2017", "R",
                     "br_v27_assessment_contract.R"), envir = .GlobalEnv)

testthat::test_that("assessment paths and protocol are disjoint from fit v2.7", {
  testthat::expect_identical(
    .br_v27a_protocol_id,
    "br2017-paperps-assessment-2026-09-07-v2.7-a1")
  testthat::expect_identical(
    gsub("\\\\", "/", .br_v27a_run_relative),
    "runs/br2017/v27/assessment-2026-09-07-v1")
  testthat::expect_false(identical(.br_v27a_run_relative,
                                   .br_v27a_upstream_relative))
  protocol <- .br_v27a_validate_protocol(.v27a_root, require_clean = FALSE)
  testthat::expect_identical(protocol$protocol_id, .br_v27a_protocol_id)
  testthat::expect_identical(
    protocol$contract[["Structural-AME-Versus-Design-AMCE"]],
    "unavailable_without_new_target_construction")
  upstream <- .br_v27_validate_protocol(
    .v27a_root, require_external = FALSE, require_clean = FALSE)
  binding <- .br_v27a_upstream_binding(upstream, 1L)
  testthat::expect_identical(binding$kind, "assembled")
  testthat::expect_identical(binding$q, 1L)
  testthat::expect_identical(binding$scope, "all")
  testthat::expect_identical(binding$input_hashes,
                             stats::setNames(upstream$inputs$sha256,
                                             upstream$inputs$input_id))
})

testthat::test_that("dimension-times-epsilon numerical rank is exact", {
  M <- diag(c(4, 2, 1))
  spectrum <- .br_v27a_matrix_spectrum(M)
  testthat::expect_identical(spectrum$dimension_factor, 3L)
  testthat::expect_equal(spectrum$threshold,
                         3 * .Machine$double.eps * 4, tolerance = 0)
  testthat::expect_identical(spectrum$rank, 3L)

  almost <- diag(c(1, .Machine$double.eps))
  almost_spectrum <- .br_v27a_matrix_spectrum(almost)
  testthat::expect_identical(almost_spectrum$rank, 1L)
})

testthat::test_that("q1 and q2 covariance-tangent certificates use rotations", {
  D <- rbind(
    c(1, 0, 0), c(0, 1, 0), c(0, 0, 1),
    c(1, 1, 0), c(1, 0, 1), c(0, 1, 1),
    c(1, -1, 0), c(1, 0, -1), c(0, 1, -1),
    c(1, 1, 1), c(1, -1, 1), c(-1, 1, 1))
  colnames(D) <- c("x1", "x2", "x3")
  q1 <- .br_v27a_local_certificate(D, matrix(c(.7, -.4, 1.1), 3L, 1L),
                                     1L, paste(rep("a", 64), collapse = ""))
  A2 <- matrix(c(.7, -.4, 1.1, .2, .9, -.5), nrow = 3L, ncol = 2L)
  q2 <- .br_v27a_local_certificate(D, A2, 2L,
                                     paste(rep("b", 64), collapse = ""))
  testthat::expect_true(q1$established)
  testthat::expect_true(q2$established)
  testthat::expect_identical(q1$dimensions$covariance_tangent_dim, 3)
  testthat::expect_identical(q2$dimensions$covariance_tangent_dim, 5)
  testthat::expect_identical(q2$ranks$jacobian_nullity, 1L)
  testthat::expect_lte(
    q2$information$rotation_null_max_absolute_residual,
    100 * .Machine$double.eps)
  testthat::expect_match(q2$disclaimer, "does not establish global injectivity",
                         fixed = TRUE)
  testthat::expect_match(q2$disclaimer, "finite-sample inference", fixed = TRUE)
})

testthat::test_that("design diagnostics include linear and quadratic ranks", {
  D <- rbind(diag(3), -diag(3), c(1, 1, 0), c(1, 0, 1), c(0, 1, 1),
             c(1, -1, 0), c(1, 0, -1), c(0, 1, -1))
  colnames(D) <- c("x1", "x2", "x3")
  out <- .br_v27a_design_diagnostics(D)
  testthat::expect_identical(out$linear$rank, 3L)
  testthat::expect_identical(out$unrestricted_quadratic$rank, 6L)
  testthat::expect_identical(out$summary$design,
                             c("linear", "unrestricted_quadratic"))
})

testthat::test_that("calibration and behavioral checks use existing rows", {
  task <- data.frame(
    respondent_id = rep(c("r1", "r2", "r3"), each = 4L),
    task = rep(1:4, 3L), fold = rep(1:3, each = 4L),
    y = c(0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1),
    probability = c(.1, .2, .15, .1, .9, .8, .85, .75,
                    .4, .6, .45, .55))
  calibration <- lapply(0:2, function(q) {
    shifted <- task
    shifted$probability <- pmin(.99, pmax(.01,
      shifted$probability + (q - 1) * .01))
    .br_v27a_calibration_summary(shifted, bins = 3L)
  })
  names(calibration) <- paste0("q", 0:2)
  behavior <- .br_v27a_response_diagnostics(task, calibration)
  testthat::expect_identical(behavior$n_respondents, 3L)
  testthat::expect_identical(behavior$response_count_distribution$n_tasks, 4L)
  testthat::expect_equal(sum(behavior$transition_table$count), 9)
  testthat::expect_equal(sum(behavior$switch_pattern$n_respondents), 3)
  testthat::expect_equal(nrow(behavior$task_order_calibration), 12)
  testthat::expect_match(behavior$analytic_sample_limitation,
                         "retained in the assembled analytic sample",
                         fixed = TRUE)
  testthat::expect_match(behavior$transition_limitation,
                         "descriptive observed-sequence", fixed = TRUE)
})

testthat::test_that("unsupported checks are explicit and no fit call exists", {
  registry <- .br_v27a_read_csv(file.path(
    .br_v27a_protocol_dir(.v27a_root), "assessment-registry.csv"))
  availability <- .br_v27a_availability(registry)
  structural <- availability[
    availability$assessment_id == "structural_ame_vs_design_amce", ]
  completion <- availability[
    availability$assessment_id ==
      "preassembly_completion_and_missingness_sensitivity", ]
  target_information <- availability[
    availability$assessment_id == "target_specific_information_diagnostics", ]
  testthat::expect_identical(structural$status, "unavailable")
  testthat::expect_identical(completion$status, "unavailable")
  testthat::expect_identical(target_information$status, "unavailable")
  runner <- paste(readLines(file.path(
    .v27a_root, "applications", "br2017", "R", "08_assess_fast_v27.R"),
    warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl("\\.br_v27_fit_scope\\(", runner))
  testthat::expect_false(grepl("\\.br_v27_assemble\\(", runner))
  testthat::expect_false(grepl("scmix_dml\\(", runner))
})

testthat::test_that("assessment output is atomic bound and non-overwriting", {
  temporary <- tempfile("br-v27-assessment-")
  dir.create(temporary)
  output <- file.path(temporary, "assessment.rds")
  binding <- list(protocol_id = .br_v27a_protocol_id,
                  protocol_hash = paste(rep("a", 64), collapse = ""))
  .br_v27a_save_atomic(list(synthetic_test = TRUE), output, binding)
  read <- .br_v27a_read_output(output, binding)
  testthat::expect_true(read$payload$synthetic_test)
  testthat::expect_true(file.exists(paste0(output, ".sha256")))
  testthat::expect_error(
    .br_v27a_save_atomic(list(synthetic_test = TRUE), output, binding),
    "Refusing to overwrite")
  unlink(temporary, recursive = TRUE, force = TRUE)
})

if (sys.nframe() == 0L) cat("Ballard--Rosa v2.7 assessment checks passed.\n")
