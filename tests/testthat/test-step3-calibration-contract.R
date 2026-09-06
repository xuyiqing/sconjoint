.step3_values <- function() {
  stats::setNames(
    as.list(rep(1, length(
      sconjoint:::.scmix_v22_required_calibration_names))),
    sconjoint:::.scmix_v22_required_calibration_names)
}

.step3_write_dcf <- function(path, fields) {
  writeLines(paste0(names(fields), ": ", unlist(fields, use.names = FALSE)),
             path, useBytes = TRUE)
}

.step3_write_values <- function(directory, values = .step3_values(),
                                rank_scope = "1") {
  path <- file.path(
    directory, sconjoint:::.scmix_v22_calibration_values_file)
  fields <- c(
    list(`Protocol-ID` = sconjoint:::.scmix_v22_protocol_id,
         Status = "frozen_for_production", `Rank-Scope` = rank_scope),
    values)
  .step3_write_dcf(path, fields)
  path
}

.step3_write_manifest <- function(directory, values_path,
                                  values = .step3_values(),
                                  source_hash = NULL,
                                  calibration_hash = NULL,
                                  rank_scope = "1") {
  if (is.null(source_hash)) {
    source_hash <- sconjoint:::.scmix_v22_sha256_file(values_path)
  }
  if (is.null(calibration_hash)) {
    calibration_hash <- scmix_v22_calibration_hash(values, source_hash)
  }
  path <- file.path(
    directory, sconjoint:::.scmix_v22_calibration_manifest_file)
  constant_fields <- values
  names(constant_fields) <- paste0("Constant-", names(values))
  fields <- c(list(
    `Protocol-ID` = sconjoint:::.scmix_v22_protocol_id,
    Status = "frozen_for_production",
    `Calibration-Hash` = calibration_hash,
    `Source-Hash` = source_hash,
    `Values-File` = sconjoint:::.scmix_v22_calibration_values_file,
    `Constant-Count` = as.character(length(values)),
    `Rank-Scope` = rank_scope), constant_fields)
  .step3_write_dcf(path, fields)
  path
}

test_that("package calibration manifest binds the exact SHA-256 source", {
  directory <- tempfile("step3-calibration-")
  dir.create(directory)
  values <- .step3_values()
  values_path <- .step3_write_values(directory, values)
  manifest_path <- .step3_write_manifest(directory, values_path, values)

  out <- sconjoint:::.scmix_v22_read_calibration_manifest(
    manifest_path, values_path)
  expect_identical(out$source_hash,
                   sconjoint:::.scmix_v22_sha256_file(values_path))
  expect_match(out$source_hash, "^[0-9a-f]{64}$")
  expect_identical(out$constant_count, 26L)
  expect_identical(out$constants,
                   sconjoint:::.scmix_v22_normalize_calibration(
                     values, require_complete = TRUE))

  ## A byte-level change is detected even when the DCF remains parseable.
  writeLines(c(readLines(values_path, warn = FALSE), ""), values_path,
             useBytes = TRUE)
  expect_error(sconjoint:::.scmix_v22_read_calibration_manifest(
    manifest_path, values_path), "SHA-256")
})

test_that("manifest and values readers reject incomplete or altered constants", {
  directory <- tempfile("step3-calibration-")
  dir.create(directory)
  values <- .step3_values()
  values_path <- .step3_write_values(directory, values)
  manifest_path <- .step3_write_manifest(directory, values_path, values)

  wrong <- values
  wrong$ridge_cutoff <- 2
  wrong_manifest <- .step3_write_manifest(
    directory, values_path, wrong,
    source_hash = sconjoint:::.scmix_v22_sha256_file(values_path),
    calibration_hash = scmix_v22_calibration_hash(
      wrong, sconjoint:::.scmix_v22_sha256_file(values_path)))
  expect_error(sconjoint:::.scmix_v22_read_calibration_manifest(
    wrong_manifest, values_path), "manifest and values file differ")

  incomplete <- values[-1L]
  incomplete_path <- .step3_write_values(directory, incomplete)
  expect_error(sconjoint:::.scmix_v22_read_calibration_values(
    incomplete_path), "protocol/status/rank-scope row and all 26")

  independent <- values
  independent$directional_heterogeneity_regularity_margin <- 2
  independent$directional_variance_standardized_cutoff <- 3
  independent$total_heterogeneity_standardized_cutoff <- 4
  independent$mean_tradeoff_denominator_margin <- 5
  independent$denominator_standardized_cutoff <- 6
  independent_path <- .step3_write_values(directory, independent)
  loaded <- sconjoint:::.scmix_v22_read_calibration_values(independent_path)
  expect_identical(
    loaded$constants,
    sconjoint:::.scmix_v22_normalize_calibration(
      independent, require_complete = TRUE))
})

test_that("package accessor fails closed without approved shipped files", {
  expect_error(testthat::with_mocked_bindings(
    scmix_v22_package_calibration(),
    .scmix_v22_approved_calibration_manifest = function() NULL,
    .package = "sconjoint"), "No intact package-approved")

  values <- sconjoint:::.scmix_v22_normalize_calibration(
    .step3_values(), require_complete = TRUE)
  source_hash <- paste(rep("d", 64L), collapse = "")
  approval <- list(
    protocol_id = sconjoint:::.scmix_v22_protocol_id,
    status = "frozen_for_production",
    rank_scope = 1L,
    calibration_hash = scmix_v22_calibration_hash(values, source_hash),
    source_hash = source_hash,
    values_file = sconjoint:::.scmix_v22_calibration_values_file,
    constant_count = 26L, constants = values, source_verified = TRUE,
    manifest_fingerprint = "tested-package-manifest")
  out <- testthat::with_mocked_bindings(
    scmix_v22_package_calibration(),
    .scmix_v22_approved_calibration_manifest = function() approval,
    .package = "sconjoint")
  expect_s3_class(out, "scmix_v22_package_calibration")
  out_v23 <- testthat::with_mocked_bindings(
    scmix_v23_package_calibration(),
    .scmix_v22_approved_calibration_manifest = function() approval,
    .package = "sconjoint")
  expect_identical(out_v23, out)

  bad <- approval
  bad$constants$interior_distance_cutoff <- 0
  bad$calibration_hash <- scmix_v22_calibration_hash(
    bad$constants, source_hash)
  expect_error(testthat::with_mocked_bindings(
    scmix_v22_package_calibration(),
    .scmix_v22_approved_calibration_manifest = function() bad,
    .package = "sconjoint"), "No intact package-approved")
})

test_that("all 26 calibration constants have a declared consumer", {
  consumers <- sconjoint:::.scmix_v22_calibration_consumers
  expect_identical(sort(names(consumers), method = "radix"),
                   sort(sconjoint:::.scmix_v22_required_calibration_names,
                        method = "radix"))
  expect_true(all(nzchar(unname(consumers))))
  expect_false(anyDuplicated(unname(consumers)) > 0L)

  duplicated <- consumers
  duplicated[[2L]] <- duplicated[[1L]]
  expect_error(testthat::with_mocked_bindings(
    sconjoint:::.scmix_v22_require_operational_calibration(.step3_values()),
    .scmix_v22_calibration_consumers = duplicated,
    .package = "sconjoint"), "own operational consumer")
})

test_that("G004 and G005 thresholds are calibration-bound and fail closed", {
  rows <- rbind(
    scmix_v22_gate_record(
      "G004", "identification_regularity", "fit", "hard_fit", "pass",
      3, 1, "greater_than_or_equal", "standard_errors", "test", "test",
      "q1"),
    scmix_v22_gate_record(
      "G005", "identification_regularity", "target", "hard_target", "pass",
      3, 1, "greater_than_or_equal", "declared_structural_norm", "test",
      "test", "target"))
  values <- .step3_values()
  expect_silent(sconjoint:::.scmix_v22_bind_external_calibration(
    rows, values, TRUE))

  wrong <- rows
  wrong$threshold[[1L]] <- .5
  expect_error(sconjoint:::.scmix_v22_bind_external_calibration(
    wrong, values, TRUE), "G004 threshold must equal")
  wrong <- rows
  wrong$threshold[[2L]] <- .5
  expect_error(sconjoint:::.scmix_v22_bind_external_calibration(
    wrong, values, TRUE), "G005 threshold must equal")

  pending <- sconjoint:::.scmix_v22_bind_external_calibration(
    rows, list(), FALSE)
  expect_identical(pending$state, c("not_run", "not_run"))
  expect_true(all(vapply(pending$threshold, identical, logical(1L),
                         "CALIBRATE_IN_STEP_3")))
})

test_that("G006 is a normalized compound of independent component cutoffs", {
  values <- .step3_values()
  values$interior_distance_cutoff <- .5
  values$subgroup_interior_margin <- .05
  values$directional_heterogeneity_regularity_margin <- .1
  values$directional_variance_standardized_cutoff <- 2
  values$total_heterogeneity_standardized_cutoff <- 3
  values$mean_tradeoff_denominator_margin <- .25
  values$denominator_standardized_cutoff <- 4

  scalar <- sconjoint:::.scmix_v22_g006_component_statistics(
    "probability", .4, .1, matrix(seq_len(20), ncol = 1L))
  expect_identical(scalar, c(interior_distance_cutoff = 4))

  subgroup <- sconjoint:::.scmix_v22_g006_component_statistics(
    "subgroup_share", c(.2, .7), c(.1, .1), matrix(0, 20, 2))
  expect_identical(subgroup, c(subgroup_interior_margin = .2))

  influence <- cbind(
    explained = seq(-2, 2, length.out = 40),
    residual = cos(seq_len(40)))
  se <- apply(influence, 2L, stats::sd) / sqrt(nrow(influence))
  distance <- sconjoint:::.scmix_v22_boundary_distance(
    "directional_heterogeneity", c(.2, .3), se)
  expect_identical(as.numeric(distance), min(c(.2, .3) / se))
  diagnostics <- sconjoint:::.scmix_v22_g006_component_statistics(
    "directional_heterogeneity", c(.2, .3), se, influence)
  expect_equal(diagnostics[[
    "directional_heterogeneity_regularity_margin"]], .2)
  expect_equal(diagnostics[[
    "directional_variance_standardized_cutoff"]], .3 / se[[2L]])
  expect_equal(diagnostics[[
    "total_heterogeneity_standardized_cutoff"]],
    .5 / (stats::sd(rowSums(influence)) / sqrt(nrow(influence))))

  compound <- sconjoint:::.scmix_v22_g006_compound(
    list(g006_component_statistics = diagnostics), values)
  expected_ratios <- diagnostics / unlist(values[names(diagnostics)])
  expect_equal(compound$statistic, min(expected_ratios))
  expect_identical(compound$threshold, 1)
  expect_identical(compound$state,
                   if (min(expected_ratios) >= 1) "pass" else "fail")
  expect_identical(compound$components$constant_name, names(diagnostics))
  expect_equal(compound$components$normalized_ratio,
               unname(expected_ratios))

  mrs <- sconjoint:::.scmix_v22_g006_component_statistics(
    "mean_tradeoff_mrs_ratio", c(.3, -.5), c(.1, .1),
    matrix(0, 20, 2))
  expect_identical(mrs, c(
    mean_tradeoff_denominator_margin = .5,
    denominator_standardized_cutoff = 5))

  missing <- values
  missing$total_heterogeneity_standardized_cutoff <- NULL
  pending <- sconjoint:::.scmix_v22_g006_compound(
    list(g006_component_statistics = diagnostics), missing)
  expect_identical(pending$state, "not_run")
  expect_true(is.na(pending$statistic))
})

test_that("rank scope is exact in both package calibration files", {
  directory <- tempfile("step3-rank-scope-")
  dir.create(directory)
  values <- .step3_values()
  wrong_values <- .step3_write_values(directory, values, rank_scope = "2")
  expect_error(
    sconjoint:::.scmix_v22_read_calibration_values(wrong_values),
    "q=1 rank scope")

  values_path <- .step3_write_values(directory, values)
  wrong_manifest <- .step3_write_manifest(
    directory, values_path, values, rank_scope = "2")
  expect_error(
    sconjoint:::.scmix_v22_read_calibration_manifest(
      wrong_manifest, values_path),
    "Rank-Scope: 1")
})

test_that("production calibration is scoped to q=1", {
  values <- .step3_values()
  source_hash <- paste(rep("e", 64L), collapse = "")
  approval <- list(
    protocol_id = sconjoint:::.scmix_v22_protocol_id,
    status = "frozen_for_production", rank_scope = 1L,
    calibration_hash = scmix_v22_calibration_hash(values, source_hash),
    source_hash = source_hash,
    values_file = sconjoint:::.scmix_v22_calibration_values_file,
    constant_count = 26L,
    constants = sconjoint:::.scmix_v22_normalize_calibration(
      values, require_complete = TRUE),
    source_verified = TRUE, manifest_fingerprint = "rank-scope-test")
  make_contract <- function(q) {
    out <- sconjoint::scmix_v22_analysis_contract(
      "rank-scope", "app", paste0("q", q), "sensitivity", "all",
      "complete", "none", list(fold1 = "signed"), 2L, "training only",
      "fixed", "start", "grid",
      c("0", "n_k^-1", "n_k^-3/4", "n_k^-1/2"), list(seed = 1L),
      "code", "data", "design",
      scmix_v22_calibration_hash(values, source_hash), q, FALSE, FALSE,
      "u1", "u1", "all")
    out
  }
  for (q in c(0L, 2L)) {
    expect_error(testthat::with_mocked_bindings(
      scmix_v22_calibration_artifact(
        values, make_contract(q), source_hash, "rank test"),
      .scmix_v22_approved_calibration_manifest = function() approval,
      .package = "sconjoint"), "Rank-Scope: 1")
  }
  expect_s3_class(testthat::with_mocked_bindings(
    scmix_v22_calibration_artifact(
      values, make_contract(1L), source_hash, "rank test"),
    .scmix_v22_approved_calibration_manifest = function() approval,
    .package = "sconjoint"), "scmix_v22_calibration_artifact")
})
