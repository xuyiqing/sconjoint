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

.step3_write_values <- function(directory, values = .step3_values()) {
  path <- file.path(
    directory, sconjoint:::.scmix_v22_calibration_values_file)
  fields <- c(
    list(`Protocol-ID` = sconjoint:::.scmix_v22_protocol_id,
         Status = "frozen_for_production"),
    values)
  .step3_write_dcf(path, fields)
  path
}

.step3_write_manifest <- function(directory, values_path,
                                  values = .step3_values(),
                                  source_hash = NULL,
                                  calibration_hash = NULL) {
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
    `Constant-Count` = as.character(length(values))), constant_fields)
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
    incomplete_path), "exactly one protocol/status row and all 26")

  aliases <- values
  aliases$total_heterogeneity_standardized_cutoff <- 2
  alias_path <- .step3_write_values(directory, aliases)
  expect_error(sconjoint:::.scmix_v22_read_calibration_values(alias_path),
               "aliases must be exactly equal")
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
})

test_that("all 26 calibration constants have a declared consumer", {
  consumers <- sconjoint:::.scmix_v22_calibration_consumers
  expect_identical(sort(names(consumers), method = "radix"),
                   sort(sconjoint:::.scmix_v22_required_calibration_names,
                        method = "radix"))
  expect_true(all(nzchar(unname(consumers))))
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

test_that("G006 maps target-specific aliases and includes total heterogeneity", {
  values <- .step3_values()
  values$interior_distance_cutoff <- 2
  values$subgroup_interior_margin <- 3
  values$directional_heterogeneity_regularity_margin <- 4
  values$directional_variance_standardized_cutoff <- 4
  values$total_heterogeneity_standardized_cutoff <- 4
  values$mean_tradeoff_denominator_margin <- 5
  values$denominator_standardized_cutoff <- 5
  expect_identical(sconjoint:::.scmix_v22_g006_cutoff(
    "probability", values), 2)
  expect_identical(sconjoint:::.scmix_v22_g006_cutoff(
    "subgroup_share", values), 3)
  expect_identical(sconjoint:::.scmix_v22_g006_cutoff(
    "directional_heterogeneity", values), 4)
  expect_identical(sconjoint:::.scmix_v22_g006_cutoff(
    "mean_tradeoff_mrs_ratio", values), 5)

  influence <- cbind(
    explained = seq(-2, 2, length.out = 40),
    residual = cos(seq_len(40)))
  se <- apply(influence, 2L, stats::sd) / sqrt(nrow(influence))
  distance <- sconjoint:::.scmix_v22_boundary_distance(
    "directional_heterogeneity", c(.2, .3), se, influence)
  components <- attr(distance, "components")
  expect_named(components, c("explained_variance", "residual_variance",
                             "total_heterogeneity"))
  expect_equal(components[["total_heterogeneity"]],
               .5 / (stats::sd(rowSums(influence)) /
                 sqrt(nrow(influence))))
  expect_identical(as.numeric(distance), min(components))
})
