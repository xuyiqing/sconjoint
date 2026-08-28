#!/usr/bin/env Rscript

## Fail-closed validator for 08_run_misspecification_experiments.R outputs.

options(stringsAsFactors = FALSE)

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.profile <- function(args) {
  hit <- grep("^--profile=", args, value = TRUE)
  if (!length(hit)) return("smoke")
  if (length(hit) != 1L) stop("Supply --profile once.", call. = FALSE)
  sub("^--profile=", "", hit)
}

.assert <- function(x, message) {
  if (!isTRUE(x)) stop(message, call. = FALSE)
}

.atomic_save <- function(x, path) {
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not write ", path, call. = FALSE)
  invisible(path)
}

.main <- function() {
  profile <- .profile(commandArgs(trailingOnly = TRUE))
  root <- normalizePath(file.path(dirname(.script_file()), "../../.."),
                        mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  options(sconjoint.sw_application_root = app)
  source(file.path(app, "config", "analysis_config.R"), local = FALSE)
  path <- file.path(sw_analysis_config$output_root, profile,
                    "sensitivity_analysis", "misspecification",
                    "misspecification_results.rds")
  if (!file.exists(path)) stop("Missing results bundle: ", path,
                               call. = FALSE)
  x <- readRDS(path)
  out_dir <- dirname(path)
  manifest_path <- file.path(out_dir, "manifest.rds")
  if (!file.exists(manifest_path)) stop("Missing manifest: ", manifest_path,
                                        call. = FALSE)
  manifest <- readRDS(manifest_path)
  .assert(identical(manifest$schema_version,
                    "sw2022-design-misspecification-manifest-v1") &&
            identical(manifest$profile, profile) &&
            identical(manifest$primary_artifacts_modified, FALSE) &&
            identical(manifest$coverage_evaluated, FALSE) &&
            identical(manifest$materiality_pass_issued, FALSE) &&
            identical(manifest$maintained_assumptions_verified, FALSE),
          "The misspecification manifest is malformed or not fail closed.")
  artifact_path <- file.path(out_dir, names(manifest$artifact_md5))
  artifact_exists <- file.exists(artifact_path)
  artifact_observed <- rep(NA_character_, length(artifact_path))
  artifact_observed[artifact_exists] <-
    unname(tools::md5sum(artifact_path[artifact_exists]))
  artifact_match <- artifact_exists &
    artifact_observed == as.character(manifest$artifact_md5)
  .assert(length(artifact_match) > 0L && all(artifact_match),
          "A manifested misspecification artifact is missing or stale.")
  expected <- c(
    "normal_benchmark", "shape_skewed_positive", "shape_skewed_negative",
    "shape_bimodal", "shape_heavy_tail", "covariance_by_party",
    "random_scale", "serial_shock"
  )
  .assert(all(expected %in% names(x$truths)),
          "The complete eight-scenario truth battery was not run.")
  experiment_profile <- x$config$profiles[[profile]]
  .assert(is.list(experiment_profile) &&
            x$requested_replications >=
              experiment_profile$minimum_defensible_replications,
          paste(
            "Too few replications for the profile's minimum defensible",
            "misspecification summary."
          ))
  .assert(identical(x$primary_fit_md5,
                    unname(tools::md5sum(x$primary_fit_path))),
          "The primary fit changed after the sensitivity run.")
  .assert(identical(x$prepared_md5,
                    unname(tools::md5sum(x$prepared_path))),
          "The frozen prepared data changed after the sensitivity run.")
  .assert(!isTRUE(x$posterior_summaries_used),
          "Respondent posterior summaries must not enter the experiment.")
  .assert(!isTRUE(x$formal_inference_available) &&
            !isTRUE(x$coverage$coverage_evaluated) &&
            !isTRUE(x$coverage$oracle_interval_substituted),
          "Coverage or formal inference was incorrectly promoted.")
  cal <- x$calibration
  get <- function(s) cal[cal$scenario == s, , drop = FALSE]
  .assert(abs(get("shape_skewed_positive")$factor_skewness) > 1,
          "Positive-skew calibration is too weak or has the wrong sign.")
  .assert(get("shape_skewed_positive")$factor_skewness > 0 &&
            get("shape_skewed_negative")$factor_skewness < 0,
          "Both prespecified skew orientations were not retained.")
  .assert(get("shape_bimodal")$factor_excess_kurtosis < -0.5,
          "The symmetric-mixture calibration is not visibly bimodal.")
  .assert(get("shape_heavy_tail")$factor_excess_kurtosis > 1,
          "The heavy-tail calibration is not visibly heavy tailed.")
  .assert(abs(get("covariance_by_party")$party_multiplier_mean_square - 1) <
            1e-12,
          "Party covariance scales do not preserve aggregate covariance.")
  .assert(abs(get("random_scale")$random_scale_mean - 1) < 1e-12,
          "Random response scale is not normalized to mean one.")
  rs <- x$qoi[x$qoi$scenario == "random_scale", , drop = FALSE]
  .assert(all(rs$comparable == grepl("^(choice|sign):", rs$quantity)),
          "Random-scale comparability labels are not fail closed.")
  .assert(all(x$truth_summary$max_truth_refinement_difference < 0.005),
          "Deterministic truth integration differs by more than 0.005 on refinement.")
  .assert(all(vapply(x$replications, function(z)
    !isTRUE(z$formal_inference_computed), logical(1L))),
    "A replication incorrectly claims formal inference.")
  status <- x$structural_sensitivity$status
  implemented <- c("shape_skewed_simulation", "shape_bimodal_simulation",
                   "shape_heavy_tail_simulation", "covariance_by_Z",
                   "serial_shocks", "scale")
  .assert(all(status$implemented[match(implemented, status$component)]),
          "One or more design-specific sensitivity components are absent.")
  .assert(!isTRUE(x$structural_sensitivity$complete) &&
            !isTRUE(x$structural_sensitivity$substantive_pass),
          "An incomplete, non-preregistered battery was promoted to a pass.")
  config_match <- file.exists(x$config_path) && identical(
    as.character(x$config_md5),
    unname(as.character(tools::md5sum(x$config_path)))
  )
  .assert(config_match, "The frozen misspecification config changed.")
  checks <- data.frame(
    check = c(
      "manifest schema/profile and fail-closed flags",
      "manifested artifact hashes",
      "primary fit input hash", "prepared-data input hash",
      "frozen misspecification config hash",
      "complete eight-scenario DGP battery",
      "minimum defensible replication count",
      "shape/covariance/scale calibration",
      "truth-integration refinement",
      "random-scale quantity comparability labels",
      "posterior summaries/formal inference/coverage prohibited",
      "design-specific components recorded without substantive pass"
    ),
    pass = TRUE,
    detail = c(
      paste(manifest$schema_version, profile),
      paste(length(artifact_path), "artifact hashes matched"),
      x$primary_fit_md5, x$prepared_md5, x$config_md5,
      paste(length(expected), "scenarios present"),
      paste(x$requested_replications, "per scenario; minimum",
            experiment_profile$minimum_defensible_replications,
            "and frozen target", experiment_profile$replications),
      "mean/covariance normalization and alternative-shape checks passed",
      paste("maximum coarse/fine difference",
            signif(max(x$truth_summary$max_truth_refinement_difference), 4)),
      "only choice and positive-scale-invariant sign quantities comparable",
      "all fail-closed flags retained",
      "simulation execution is distinct from empirical refit/identification"
    ), stringsAsFactors = FALSE
  )
  validation_path <- file.path(out_dir, "misspecification_validation.rds")
  validation <- list(
    schema_version = "sw2022-misspecification-validation-v1",
    profile = profile, passed = TRUE, checks = checks,
    manifest_path = manifest_path,
    manifest_md5 = unname(tools::md5sum(manifest_path)),
    results_path = path, results_md5 = unname(tools::md5sum(path)),
    primary_fit_path = x$primary_fit_path,
    primary_fit_md5 = x$primary_fit_md5,
    prepared_path = x$prepared_path, prepared_md5 = x$prepared_md5,
    config_path = x$config_path, config_md5 = x$config_md5,
    formal_inference_available = FALSE,
    coverage_evaluated = FALSE, materiality_pass_issued = FALSE,
    maintained_assumptions_verified = FALSE,
    distinction = paste(
      "Validated artifacts are design-specific simulated-data diagnostics;",
      "empirical alternative-family refits and their identification remain not_run."
    ),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  .atomic_save(validation, validation_path)
  cat("validated:", normalizePath(path), "\n")
  cat("validation:", normalizePath(validation_path), "\n")
  cat("scenarios:", length(expected), " replications:",
      length(x$replications), " QOI rows:", nrow(x$qoi), "\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) .main()
