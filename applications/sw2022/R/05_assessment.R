#!/usr/bin/env Rscript

## Saha--Weeks (2022): application-specific specification assessment.
##
## This script consumes only local prepared, fit, and inference artifacts.  It
## never reads or writes the ConjointStructural source tree.  A diagnostic is
## recorded as executed only when its required artifact and provenance are
## present; a maintained assumption or unavailable fielding probability is
## never converted into a passing assessment.

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(arg)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

.parse_args <- function(x) {
  out <- list()
  for (arg in x) {
    if (!startsWith(arg, "--") || !grepl("=", arg, fixed = TRUE)) {
      stop("Arguments must have the form --name=value: ", arg,
           call. = FALSE)
    }
    pair <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    out[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <-
      paste(pair[-1L], collapse = "=")
  }
  out
}

.read_optional <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  readRDS(path)
}

.validate_rank_artifact <- function(x, expected_class, expected_role,
                                    profile, prep_path) {
  if (is.null(x)) return(invisible(NULL))
  p <- x$provenance
  paths <- p$artifact_paths
  hashes <- p$artifact_md5
  path_ok <- is.character(paths) && length(paths) > 0L &&
    length(paths) == length(hashes) && all(file.exists(paths))
  hash_ok <- path_ok && identical(
    unname(as.character(tools::md5sum(paths))),
    unname(as.character(hashes))
  )
  prep_ok <- file.exists(prep_path) && identical(
    as.character(p$prepared_md5),
    unname(as.character(tools::md5sum(prep_path)))
  )
  schema_ok <- inherits(x, expected_class) && is.list(p) &&
    identical(p$schema_version, "sw2022-rank-assessment-provenance-v1") &&
    identical(p$role, expected_role) && identical(p$profile, profile) &&
    is.character(p$rank_config_version) && nzchar(p$rank_config_version) &&
    is.character(p$main_config_version) && nzchar(p$main_config_version)
  if (!schema_ok || !prep_ok || !hash_ok) {
    stop(
      "Rank-assessment provenance is stale or malformed for ", expected_role,
      ". Rerun 03b_rank_assessment.R for profile ", profile,
      " before 05_assessment.R.", call. = FALSE
    )
  }
  invisible(x)
}

.write_csv <- function(x, path) {
  if (is.null(x)) return(invisible(FALSE))
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(TRUE)
}

.first_existing <- function(directory, stems) {
  paths <- file.path(directory, stems)
  hit <- paths[file.exists(paths)]
  if (length(hit)) hit[[1L]] else NA_character_
}

.sensitivity_artifact_audit <- function(directory, expected) {
  if (is.null(names(expected)) || any(!nzchar(names(expected)))) {
    stop("Sensitivity manifest artifacts must be a named hash vector.",
         call. = FALSE)
  }
  path <- file.path(directory, names(expected))
  exists <- file.exists(path)
  observed <- rep(NA_character_, length(path))
  observed[exists] <- unname(tools::md5sum(path[exists]))
  data.frame(
    artifact = names(expected), path = path, exists = exists,
    expected_md5 = as.character(expected), observed_md5 = observed,
    hash_match = exists & !is.na(observed) & observed == as.character(expected),
    stringsAsFactors = FALSE
  )
}

.load_misspecification_bridge <- function(sensitivity_directory, profile) {
  directory <- file.path(sensitivity_directory, "misspecification")
  if (!dir.exists(directory)) {
    return(list(
      available = FALSE, validated = FALSE, directory = directory,
      status = data.frame(
        check = "design-specific misspecification simulation",
        status = "not_run",
        detail = paste(
          "No validated simulated-data experiment is present; empirical",
          "alternative-family refits also remain not_run."
        ), stringsAsFactors = FALSE
      ),
      compact = NULL
    ))
  }
  source_paths <- c(
    manifest = file.path(directory, "manifest.rds"),
    results = file.path(directory, "misspecification_results.rds"),
    validation = file.path(directory, "misspecification_validation.rds")
  )
  missing <- names(source_paths)[!file.exists(source_paths)]
  if (length(missing)) {
    stop(
      "The misspecification directory exists but is not independently ",
      "validated: ", paste(missing, collapse = ", "),
      ". Run 08_run_misspecification_experiments.R and ",
      "09_validate_misspecification_artifacts.R before assessment.",
      call. = FALSE
    )
  }
  manifest <- readRDS(source_paths[["manifest"]])
  results <- readRDS(source_paths[["results"]])
  validation <- readRDS(source_paths[["validation"]])
  current_md5 <- unname(tools::md5sum(source_paths))
  names(current_md5) <- names(source_paths)
  validation_ok <- is.list(validation) &&
    identical(validation$schema_version,
              "sw2022-misspecification-validation-v1") &&
    identical(validation$profile, profile) && isTRUE(validation$passed) &&
    is.data.frame(validation$checks) && nrow(validation$checks) > 0L &&
    all(validation$checks$pass) &&
    identical(as.character(validation$manifest_md5),
              as.character(current_md5[["manifest"]])) &&
    identical(as.character(validation$results_md5),
              as.character(current_md5[["results"]])) &&
    identical(validation$formal_inference_available, FALSE) &&
    identical(validation$coverage_evaluated, FALSE) &&
    identical(validation$materiality_pass_issued, FALSE) &&
    identical(validation$maintained_assumptions_verified, FALSE)
  manifest_ok <- is.list(manifest) &&
    identical(manifest$schema_version,
              "sw2022-design-misspecification-manifest-v1") &&
    identical(manifest$profile, profile) &&
    identical(manifest$primary_artifacts_modified, FALSE) &&
    identical(manifest$coverage_evaluated, FALSE) &&
    identical(manifest$materiality_pass_issued, FALSE) &&
    identical(manifest$maintained_assumptions_verified, FALSE) &&
    is.character(manifest$artifact_md5) &&
    length(manifest$artifact_md5) > 0L
  results_ok <- is.list(results) &&
    identical(results$schema_version,
              "sw2022-design-misspecification-results-v1") &&
    identical(results$profile, profile) &&
    identical(results$primary_artifacts_modified, FALSE) &&
    identical(results$posterior_summaries_used, FALSE) &&
    identical(results$formal_inference_available, FALSE) &&
    identical(results$maintained_assumptions_verified, FALSE) &&
    inherits(results$structural_sensitivity,
             "scmix_structural_sensitivity") &&
    identical(results$structural_sensitivity$complete, FALSE) &&
    identical(results$structural_sensitivity$substantive_pass, FALSE) &&
    is.data.frame(results$qoi_summary) &&
    is.data.frame(results$optimization) &&
    is.data.frame(results$calibration) &&
    is.data.frame(results$truth_summary) &&
    is.data.frame(results$coverage)
  if (!validation_ok || !manifest_ok || !results_ok) {
    stop(
      "Misspecification simulation validation is stale, malformed, or not ",
      "fail closed. Rerun its runner and validator; the results are not ",
      "ingested.", call. = FALSE
    )
  }
  artifact_audit <- .sensitivity_artifact_audit(
    directory, manifest$artifact_md5)
  input_paths <- c(
    primary_fit = validation$primary_fit_path,
    prepared = validation$prepared_path,
    config = validation$config_path
  )
  expected_input <- c(
    primary_fit = validation$primary_fit_md5,
    prepared = validation$prepared_md5,
    config = validation$config_md5
  )
  input_exists <- file.exists(input_paths)
  input_md5 <- rep(NA_character_, length(input_paths))
  input_md5[input_exists] <- unname(tools::md5sum(input_paths[input_exists]))
  input_audit <- data.frame(
    artifact = names(input_paths), path = unname(input_paths),
    exists = input_exists, expected_md5 = unname(expected_input),
    observed_md5 = input_md5,
    hash_match = input_exists & input_md5 == unname(expected_input),
    stringsAsFactors = FALSE
  )
  if (!all(artifact_audit$hash_match) || !all(input_audit$hash_match)) {
    stop(
      "Misspecification simulation artifacts or inputs changed after ",
      "validation; rerun 08/09 and the parent 07/08 manifest pipeline.",
      call. = FALSE
    )
  }
  status <- data.frame(
    check = c(
      "independent misspecification validator",
      "misspecification artifact hashes",
      "misspecification input/config hashes",
      "simulation execution versus empirical alternatives",
      "coverage/materiality/formal inference withheld"
    ),
    status = c("pass", "pass", "pass", "run_simulated_data_diagnostic",
               "withheld"),
    detail = c(
      paste(nrow(validation$checks), "checks tied to manifest/results hashes"),
      paste(nrow(artifact_audit), "nested artifact hashes matched"),
      paste(nrow(input_audit), "input/config hashes matched"),
      validation$distinction,
      paste(
        "No formal coverage, approved materiality threshold, or maintained",
        "assumption verification was issued."
      )
    ), stringsAsFactors = FALSE
  )
  out <- list(
    available = TRUE, validated = TRUE, directory = directory,
    profile = profile, source_paths = source_paths,
    source_md5 = unname(current_md5), manifest = manifest,
    validation = validation, artifact_audit = artifact_audit,
    input_audit = input_audit, status = status,
    compact = list(
      qoi_summary = results$qoi_summary,
      optimization = results$optimization,
      calibration = results$calibration,
      truth_summary = results$truth_summary,
      coverage = results$coverage,
      structural_sensitivity = results$structural_sensitivity,
      config = results$config,
      distinction = validation$distinction,
      formal_inference_available = FALSE,
      coverage_evaluated = FALSE,
      materiality_pass_issued = FALSE,
      maintained_assumptions_verified = FALSE,
      empirical_alternative_refits = "not_run"
    ),
    formal_inference_available = FALSE,
    maintained_assumptions_verified = FALSE
  )
  class(out) <- c("sw2022_misspecification_bridge", "list")
  out
}

.compact_sensitivity_components <- function(z19, interaction, process,
                                            completion) {
  list(
    z19 = list(
      theta_comparison = z19$theta_comparison,
      choice_comparison = z19$choice_comparison,
      score_comparison = z19$score_comparison,
      missing_task_rows = z19$missing_task_rows,
      imputation_verified_training_only =
        z19$imputation_verified_training_only,
      interpretation = z19$interpretation,
      formal_inference_available = z19$formal_inference_available,
      identification_established = z19$identification_established
    ),
    male_run_interaction = list(
      conditional_effects = interaction$conditional_effects,
      choice_probabilities = interaction$choice_probabilities,
      score_comparison = interaction$score_comparison,
      design_audit = interaction$design_audit,
      feature_definition = interaction$feature_definition,
      interpretation = interaction$interpretation,
      formal_inference_available = interaction$formal_inference_available,
      identification_established = interaction$identification_established
    ),
    task_process = list(
      tables = process$tables,
      profile_swap_summary = process$profile_swap_summary,
      task_process_alternative_refit = process$task_process_alternative_refit,
      serial_shock_alternative_refit = process$serial_shock_alternative_refit,
      interpretation = process$interpretation
    ),
    completion = list(
      sample = completion$sample,
      theta_comparison = completion$theta_comparison,
      choice_comparison = completion$choice_comparison,
      amce_comparison = completion$amce_comparison,
      early_task_by_eventual_completion =
        completion$early_task_by_eventual_completion,
      early_assignment_response_balance =
        completion$early_assignment_response_balance,
      optimizer_gate = completion$optimizer_gate,
      interpretation = completion$interpretation,
      formal_inference_available = completion$formal_inference_available
    )
  )
}

.load_sensitivity_bridge <- function(fit_dir, profile) {
  directory <- file.path(fit_dir, "sensitivity_analysis")
  if (!dir.exists(directory)) {
    return(list(
      available = FALSE, validated = FALSE, directory = directory,
      status = data.frame(
        check = "validated sensitivity directory", status = "not_run",
        detail = "sensitivity_analysis directory is absent",
        stringsAsFactors = FALSE
      )
    ))
  }
  source_paths <- c(
    manifest = file.path(directory, "sensitivity_manifest.rds"),
    validation = file.path(directory, "sensitivity_validation.rds"),
    structural = file.path(directory, "structural_sensitivity.rds"),
    z19 = file.path(directory, "fit_z19_sensitivity.rds"),
    interaction = file.path(directory, "fit_male_run_interaction.rds"),
    process = file.path(directory, "task_process_diagnostics.rds"),
    completion = file.path(directory, "completion_sample_sensitivity.rds"),
    application_status = file.path(directory, "tables",
                                   "application_sensitivity_status.csv")
  )
  missing <- names(source_paths)[!file.exists(source_paths)]
  if (length(missing)) {
    stop(
      "The sensitivity directory exists but is incomplete: ",
      paste(missing, collapse = ", "),
      ". Run 07_run_sensitivities.R and 08_validate_sensitivity_artifacts.R ",
      "before assessment.", call. = FALSE
    )
  }
  manifest <- readRDS(source_paths[["manifest"]])
  validation <- readRDS(source_paths[["validation"]])
  current_manifest_md5 <- unname(tools::md5sum(source_paths[["manifest"]]))
  validation_ok <- is.list(validation) &&
    identical(validation$schema_version,
              "sw2022-sensitivity-validation-v1") &&
    identical(validation$profile, profile) && isTRUE(validation$passed) &&
    is.data.frame(validation$checks) && nrow(validation$checks) > 0L &&
    all(validation$checks$pass) &&
    identical(as.character(validation$manifest_md5),
              as.character(current_manifest_md5))
  manifest_ok <- is.list(manifest) &&
    identical(manifest$schema_version, "sw2022-sensitivity-manifest-v1") &&
    identical(manifest$profile, profile) &&
    identical(manifest$primary_artifacts_modified, FALSE) &&
    identical(manifest$formal_inference_available, FALSE) &&
    identical(manifest$maintained_assumptions_verified, FALSE) &&
    is.character(manifest$artifacts) && length(manifest$artifacts) > 0L
  if (!validation_ok || !manifest_ok) {
    stop(
      "Sensitivity validation or manifest provenance is stale/invalid. ",
      "Rerun 07_run_sensitivities.R and 08_validate_sensitivity_artifacts.R; ",
      "unvalidated results are not ingested.", call. = FALSE
    )
  }
  artifact_audit <- .sensitivity_artifact_audit(
    directory, manifest$artifacts)
  input_paths <- unlist(manifest$input_paths, use.names = TRUE)
  expected_input <- as.character(manifest$input_md5)
  if (length(input_paths) != length(expected_input)) {
    stop("Sensitivity manifest input hashes are malformed.", call. = FALSE)
  }
  input_exists <- file.exists(input_paths)
  input_md5 <- rep(NA_character_, length(input_paths))
  input_md5[input_exists] <- unname(tools::md5sum(input_paths[input_exists]))
  input_audit <- data.frame(
    artifact = names(input_paths), path = as.character(input_paths),
    exists = input_exists, expected_md5 = expected_input,
    observed_md5 = input_md5,
    hash_match = input_exists & input_md5 == expected_input,
    stringsAsFactors = FALSE
  )
  if (!all(artifact_audit$hash_match) || !all(input_audit$hash_match)) {
    bad <- artifact_audit$artifact[!artifact_audit$hash_match]
    bad_input <- input_audit$artifact[!input_audit$hash_match]
    if (length(bad_input)) bad <- c(bad, paste0("input:", bad_input))
    stop(
      "Sensitivity bridge hash verification failed for: ",
      paste(bad, collapse = ", "),
      ". Primary or sensitivity artifacts changed after validation; rerun the ",
      "sensitivity pipeline.", call. = FALSE
    )
  }
  misspecification <- .load_misspecification_bridge(directory, profile)
  if (isTRUE(misspecification$validated)) {
    misspec_paths <- misspecification$source_paths
    names(misspec_paths) <- paste0("misspecification_", names(misspec_paths))
    source_paths <- c(source_paths, misspec_paths)
  }
  structural <- readRDS(source_paths[["structural"]])
  z19 <- readRDS(source_paths[["z19"]])
  interaction <- readRDS(source_paths[["interaction"]])
  process <- readRDS(source_paths[["process"]])
  completion <- readRDS(source_paths[["completion"]])
  application_status <- utils::read.csv(
    source_paths[["application_status"]], check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!inherits(structural, "scmix_structural_sensitivity") ||
      !all(c("component", "status", "note",
             "maintained_assumption_verified", "formal_inference") %in%
           names(application_status)) ||
      any(application_status$maintained_assumption_verified) ||
      any(application_status$formal_inference)) {
    stop("Validated sensitivity objects have an unexpected fail-closed schema.",
         call. = FALSE)
  }
  status <- data.frame(
    check = c(
      "sensitivity manifest schema/profile",
      "independent validator tied to manifest hash",
      "manifested sensitivity artifact hashes",
      "primary input artifact hashes",
      "maintained assumptions/formal inference withheld"
    ),
    status = "pass",
    detail = c(
      paste(manifest$schema_version, profile),
      paste(nrow(validation$checks), "validator checks passed"),
      paste(nrow(artifact_audit), "artifact hashes matched"),
      paste(nrow(input_audit), "input hashes matched"),
      "all application component rows retain FALSE verification/inference flags"
    ), stringsAsFactors = FALSE
  )
  status <- rbind(status, misspecification$status)
  components <- .compact_sensitivity_components(
    z19, interaction, process, completion)
  components$misspecification <- misspecification$compact
  out <- list(
    available = TRUE, validated = TRUE, directory = directory,
    profile = profile, source_paths = source_paths,
    source_md5 = unname(tools::md5sum(source_paths)),
    manifest = manifest, manifest_md5 = current_manifest_md5,
    validation = validation,
    artifact_audit = artifact_audit, input_audit = input_audit,
    structural = structural, application_status = application_status,
    components = components,
    misspecification = misspecification,
    status = status,
    formal_inference_available = FALSE,
    maintained_assumptions_verified = FALSE,
    disclaimer = paste(
      "The bridge verifies provenance and execution status only. It does not",
      "convert diagnostics into alternative-model refits, verify maintained",
      "assumptions, or supply formal inference."
    )
  )
  class(out) <- c("sw2022_sensitivity_bridge", "list")
  out
}

.load_party_gender_mean_bridge <- function(app, profile) {
  directory <- file.path(
    app, "results", "party_gender_mean_sensitivity", profile
  )
  if (!dir.exists(directory)) {
    return(list(
      available = FALSE, validated = FALSE, directory = directory,
      status = data.frame(
        check = "party-by-candidate-gender mean diagnostic",
        status = "not_run",
        detail = paste(
          "The post-hoc party-gender diagnostic directory is absent; no",
          "diagnostic or inferential claim is supplied."
        ), stringsAsFactors = FALSE
      )
    ))
  }

  source_paths <- c(
    result = file.path(directory, "party_gender_mean_sensitivity.rds"),
    manifest = file.path(directory, "manifest.rds")
  )
  missing <- names(source_paths)[!file.exists(source_paths)]
  if (length(missing)) {
    stop(
      "The party-gender diagnostic directory exists but is incomplete: ",
      paste(missing, collapse = ", "),
      ". Rerun 10_run_party_gender_mean_sensitivity.R; partial results are ",
      "not ingested.", call. = FALSE
    )
  }

  result <- readRDS(source_paths[["result"]])
  manifest <- readRDS(source_paths[["manifest"]])
  result_schema <- "sw2022-party-gender-mean-diagnostic-v1"
  required_tables <- c(
    "sequence_score_summary", "sequence_score_paired_differences",
    "party_calibration", "party_amce_projection",
    "party_gender_structural", "optimization",
    "diagnostic_cause_ledger", "reporting_gates", "q0_scope_check"
  )
  result_ok <- is.list(result) &&
    identical(result$schema_version, result_schema) &&
    identical(result$profile, profile) &&
    identical(result$formal_inference_available, FALSE) &&
    identical(result$maintained_model, FALSE) &&
    identical(result$diagnostic_selection_outcome_blind, FALSE) &&
    identical(result$primary_artifacts_modified, FALSE) &&
    identical(result$posterior_summaries_used, FALSE) &&
    isTRUE(result$fold_construction_verified) &&
    isTRUE(result$inherited_primary_outer_folds) &&
    is.list(result$configuration) &&
    identical(
      result$configuration$created_after_primary_mismatch_was_observed, TRUE
    ) &&
    identical(result$configuration$outcome_blind, FALSE) &&
    identical(result$configuration$formal_inference_available, FALSE) &&
    identical(result$configuration$maintained_model, FALSE) &&
    identical(result$configuration$primary_artifacts_modified, FALSE) &&
    identical(as.integer(result$sample$n_respondents), 1191L) &&
    identical(as.integer(result$sample$n_tasks), 3573L) &&
    is.list(result$tables) &&
    all(required_tables %in% names(result$tables)) &&
    all(vapply(result$tables[required_tables], is.data.frame, logical(1L))) &&
    all(vapply(result$tables[required_tables], nrow, integer(1L)) > 0L)
  manifest_ok <- is.list(manifest) &&
    identical(manifest$schema_version, paste0(result_schema, "-manifest")) &&
    identical(manifest$profile, profile) &&
    isTRUE(manifest$primary_artifacts_unchanged) &&
    isTRUE(manifest$descriptive_use_gate) &&
    identical(manifest$formal_inference_available, FALSE) &&
    identical(manifest$maintained_model, FALSE) &&
    identical(manifest$outcome_blind, FALSE) &&
    identical(manifest$primary_artifacts_modified, FALSE) &&
    is.character(manifest$artifacts) && length(manifest$artifacts) > 0L
  if (!result_ok || !manifest_ok) {
    stop(
      "The party-gender diagnostic result or manifest is malformed or does ",
      "not retain its fail-closed post-hoc labels. It is not ingested.",
      call. = FALSE
    )
  }

  gates <- result$tables$reporting_gates
  gate_schema <- all(c(
    "gate", "pass", "required_for_descriptive_use", "status"
  ) %in% names(gates))
  required_gate <- gate_schema & gates$required_for_descriptive_use %in% TRUE
  required_gate_ok <- gate_schema && any(required_gate) &&
    all(gates$pass[required_gate] %in% TRUE)
  formal_row <- if (gate_schema) {
    which(gates$gate == "formal inference enabled")
  } else integer()
  outcome_row <- if (gate_schema) {
    which(gates$gate == "end-to-end outcome-blind model assessment")
  } else integer()
  withheld_ok <- length(formal_row) == 1L && length(outcome_row) == 1L &&
    identical(gates$pass[[formal_row]], FALSE) &&
    identical(gates$pass[[outcome_row]], FALSE) &&
    grepl("withheld", gates$status[[formal_row]], fixed = TRUE) &&
    grepl("withheld", gates$status[[outcome_row]], fixed = TRUE)
  if (!required_gate_ok || !withheld_ok) {
    stop(
      "The party-gender diagnostic reporting gates do not authorize even ",
      "descriptive use or fail to withhold formal/outcome-blind claims.",
      call. = FALSE
    )
  }

  artifact_audit <- .sensitivity_artifact_audit(
    directory, manifest$artifacts
  )
  manifest_input_paths <- unlist(manifest$input_paths, use.names = TRUE)
  manifest_input_md5 <- as.character(manifest$input_md5)
  result_input_paths <- unlist(result$input_paths, use.names = TRUE)
  result_input_md5 <- as.character(result$input_md5)
  inputs_agree <- identical(names(manifest_input_paths),
                            names(result_input_paths)) &&
    identical(unname(as.character(manifest_input_paths)),
              unname(as.character(result_input_paths))) &&
    identical(names(manifest$input_md5), names(result$input_md5)) &&
    identical(unname(manifest_input_md5), unname(result_input_md5)) &&
    length(manifest_input_paths) == length(manifest_input_md5)
  if (!inputs_agree || !length(manifest_input_paths)) {
    stop(
      "The party-gender result and manifest input provenance disagree.",
      call. = FALSE
    )
  }
  input_exists <- file.exists(manifest_input_paths)
  input_observed <- rep(NA_character_, length(manifest_input_paths))
  input_observed[input_exists] <- unname(tools::md5sum(
    manifest_input_paths[input_exists]
  ))
  input_audit <- data.frame(
    artifact = names(manifest_input_paths),
    path = unname(as.character(manifest_input_paths)),
    exists = input_exists,
    expected_md5 = unname(manifest_input_md5),
    observed_md5 = input_observed,
    hash_match = input_exists & input_observed == unname(manifest_input_md5),
    stringsAsFactors = FALSE
  )
  result_manifested <- any(
    artifact_audit$artifact == basename(source_paths[["result"]]) &
      artifact_audit$path == source_paths[["result"]]
  )
  if (!result_manifested || !all(artifact_audit$hash_match) ||
      !all(input_audit$hash_match)) {
    bad <- artifact_audit$artifact[!artifact_audit$hash_match]
    bad_input <- input_audit$artifact[!input_audit$hash_match]
    if (!result_manifested) bad <- c(bad, "unmanifested result object")
    if (length(bad_input)) bad <- c(bad, paste0("input:", bad_input))
    stop(
      "Party-gender diagnostic provenance verification failed for: ",
      paste(bad, collapse = ", "),
      ". Rerun the diagnostic before assessment.", call. = FALSE
    )
  }

  source_md5 <- unname(tools::md5sum(source_paths))
  names(source_md5) <- names(source_paths)
  status <- data.frame(
    check = c(
      "party-gender diagnostic schema/profile",
      "manifested diagnostic artifact hashes",
      "diagnostic input/source hashes",
      "respondent-level outer-fold isolation",
      "post-hoc selection disclosure",
      "formal inference and maintained-model status"
    ),
    status = c("pass", "pass", "pass", "pass",
               "failed_by_design_disclosed", "withheld"),
    detail = c(
      paste(result_schema, profile),
      paste(nrow(artifact_audit), "artifact hashes matched"),
      paste(nrow(input_audit), "input/source hashes matched"),
      paste(
        "The frozen q=1 pooled and two-slope comparators reuse primary",
        "respondent-level outer folds."
      ),
      paste(
        "The narrow party-gender extension was chosen after observing the",
        "primary mismatch; it is not an outcome-blind model assessment."
      ),
      paste(
        "Descriptive diagnostic only: no formal inference, maintained-model",
        "claim, model-selection claim, or materiality pass is supplied."
      )
    ), stringsAsFactors = FALSE
  )
  out <- list(
    available = TRUE, validated = TRUE, directory = directory,
    profile = profile, source_paths = source_paths, source_md5 = source_md5,
    result = result, manifest = manifest,
    artifact_audit = artifact_audit, input_audit = input_audit,
    status = status,
    formal_inference_available = FALSE,
    maintained_model = FALSE,
    outcome_blind = FALSE,
    primary_artifacts_modified = FALSE,
    disclaimer = paste(
      "This is a respondent-cross-fitted but post-hoc diagnostic selected",
      "after the party-by-candidate-gender mismatch was observed. It supplies",
      "no formal inference, outcome-blind selection claim, maintained-model",
      "status, or materiality conclusion."
    )
  )
  class(out) <- c("sw2022_party_gender_mean_bridge", "list")
  out
}

.copy_party_gender_mean_tables <- function(bridge, table_dir) {
  if (!isTRUE(bridge$validated)) return(data.frame())
  audit <- bridge$artifact_audit
  keep <- grepl("\\.csv$", audit$artifact, ignore.case = TRUE)
  source <- audit$path[keep]
  if (!length(source)) {
    stop("The validated party-gender diagnostic has no manifested CSV tables.",
         call. = FALSE)
  }
  label <- gsub("[/\\\\]", "__", audit$artifact[keep])
  target <- file.path(table_dir, paste0("party_gender__", label))
  if (anyDuplicated(target)) {
    stop("Party-gender diagnostic table names do not map uniquely.",
         call. = FALSE)
  }
  copied <- file.copy(source, target, overwrite = TRUE, copy.mode = TRUE)
  source_md5 <- unname(tools::md5sum(source))
  expected_md5 <- as.character(audit$expected_md5[keep])
  target_md5 <- rep(NA_character_, length(target))
  target_md5[copied] <- unname(tools::md5sum(target[copied]))
  out <- data.frame(
    source = source, target = target, copied = copied,
    expected_md5 = expected_md5, source_md5 = source_md5,
    target_md5 = target_md5,
    hash_match = copied & source_md5 == expected_md5 &
      source_md5 == target_md5,
    stringsAsFactors = FALSE
  )
  if (!all(out$hash_match)) {
    stop("A validated party-gender table could not be copied byte-for-byte.",
         call. = FALSE)
  }
  out
}

.clear_party_gender_mean_tables <- function(table_dir) {
  if (!dir.exists(table_dir)) return(invisible(character()))
  files <- list.files(table_dir, full.names = TRUE)
  stale <- files[grepl(
    "^party_gender(__|_bridge_).*\\.csv$", basename(files)
  )]
  if (length(stale) && !all(file.remove(stale))) {
    stop("Could not clear stale party-gender diagnostic bridge tables.",
         call. = FALSE)
  }
  invisible(stale)
}

.copy_sensitivity_bridge_tables <- function(bridge, table_dir) {
  if (!isTRUE(bridge$validated)) return(data.frame())
  files <- list.files(
    bridge$directory, pattern = "\\.csv$", recursive = TRUE,
    full.names = TRUE
  )
  relative <- substring(files, nchar(bridge$directory) + 2L)
  is_table <- grepl("(^|/)tables/", relative)
  files <- files[is_table]
  relative <- relative[is_table]
  if (!length(files)) stop("Validated sensitivity table directory is empty.",
                           call. = FALSE)
  label <- sub("^tables/", "", relative)
  label <- sub("/tables/", "/", label, fixed = TRUE)
  label <- gsub("/", "__", label, fixed = TRUE)
  target <- file.path(table_dir, paste0("sensitivity__", label))
  if (anyDuplicated(target)) {
    stop("Recursive sensitivity table names do not map uniquely.",
         call. = FALSE)
  }
  copied <- file.copy(files, target, overwrite = TRUE, copy.mode = TRUE)
  source_md5 <- unname(tools::md5sum(files))
  target_md5 <- rep(NA_character_, length(target))
  target_md5[copied] <- unname(tools::md5sum(target[copied]))
  out <- data.frame(
    source = files, target = target, copied = copied,
    source_md5 = source_md5, target_md5 = target_md5,
    hash_match = copied & source_md5 == target_md5,
    stringsAsFactors = FALSE
  )
  if (!all(out$hash_match)) {
    stop("A validated sensitivity table could not be copied byte-for-byte.",
         call. = FALSE)
  }
  out
}

.clear_sensitivity_bridge_tables <- function(table_dir) {
  if (!dir.exists(table_dir)) return(invisible(character()))
  files <- list.files(table_dir, full.names = TRUE)
  base <- basename(files)
  stale <- files[
    grepl("^sensitivity__.*\\.csv$", base) |
      grepl("^sensitivity_bridge_.*\\.csv$", base) |
      grepl("^sensitivity_misspecification_.*\\.csv$", base)
  ]
  if (length(stale) && !all(file.remove(stale))) {
    stop("Could not clear stale generated sensitivity bridge tables.",
         call. = FALSE)
  }
  invisible(stale)
}

.finite_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

.fit_core <- function(x) {
  if (is.null(x)) return(NULL)
  x$refit %||% x$full_fit %||% x
}

.fit_summary_row <- function(x, label, expected_q = NA_integer_) {
  core <- .fit_core(x)
  if (is.null(core)) {
    return(data.frame(
      specification = label, q = expected_q, artifact_available = FALSE,
      objective = NA_real_, kappa = NA_real_, smallest_active_eigenvalue = NA_real_,
      integration = NA_character_, integration_points = NA_integer_,
      note = "refit artifact not available", stringsAsFactors = FALSE
    ))
  }
  q <- core$specification$q %||% core$q %||% expected_q
  Sigma <- core$Sigma
  eig <- if (is.matrix(Sigma) && nrow(Sigma) == ncol(Sigma)) {
    sort(eigen((Sigma + t(Sigma)) / 2, symmetric = TRUE,
               only.values = TRUE)$values, decreasing = TRUE)
  } else numeric()
  q_int <- suppressWarnings(as.integer(q))
  active <- if (length(eig) && is.finite(q_int) && q_int > 0L &&
                q_int <= length(eig)) eig[[q_int]] else NA_real_
  grid <- core$integration_grid %||% core$gh
  metadata <- grid$metadata %||% list()
  method <- metadata$method %||% metadata$integration %||%
    core$specification$integration %||% NA_character_
  points <- if (is.list(grid) && !is.null(grid$U)) nrow(as.matrix(grid$U)) else
    NA_integer_
  objective <- core$optimization$objective %||% core$objective %||% NA_real_
  data.frame(
    specification = label, q = q_int, artifact_available = TRUE,
    objective = if (.finite_scalar(objective)) objective else NA_real_,
    kappa = if (.finite_scalar(core$kappa)) core$kappa else NA_real_,
    smallest_active_eigenvalue = active,
    integration = as.character(method)[1L], integration_points = points,
    note = paste(
      "Full-sample attained solution; objective values across q need not be",
      "comparable if integration/tuning differs. This is sensitivity, not rank selection."
    ), stringsAsFactors = FALSE
  )
}

.cluster_lm <- function(y, X, cluster) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  cluster <- as.character(cluster)
  if (nrow(X) != length(y) || length(cluster) != length(y) ||
      any(!is.finite(X)) || any(!is.finite(y)) || anyNA(cluster)) {
    stop("Malformed inputs to respondent-clustered linear model.",
         call. = FALSE)
  }
  fit <- stats::lm.fit(X, y)
  if (fit$rank != ncol(X)) {
    stop("The AMCE benchmark design matrix is rank deficient.", call. = FALSE)
  }
  bread <- solve(crossprod(X))
  cluster_score <- rowsum(X * as.numeric(fit$residuals), cluster,
                          reorder = FALSE)
  G <- nrow(cluster_score)
  n <- nrow(X)
  k <- ncol(X)
  correction <- if (G > 1L && n > k) (G / (G - 1)) * ((n - 1) / (n - k)) else
    NA_real_
  vcov <- correction * bread %*% crossprod(cluster_score) %*% bread
  list(coef = as.numeric(fit$coefficients), vcov = vcov,
       residual = as.numeric(fit$residuals), rank = fit$rank,
       clusters = G, n = n)
}

.amce_contrasts <- function(p, coordinate_names) {
  if (p != 13L) {
    C <- diag(p)
    rownames(C) <- paste(coordinate_names, "vs preparation reference")
    return(C)
  }
  C <- matrix(0, 13L, p)
  rownames(C) <- c(
    "Female vs Male", "Previously ran: Yes vs No",
    "Collaborative vs Empathetic", "Determined vs Empathetic",
    "Assertive vs Empathetic", "Good Communicator vs Empathetic",
    "Hard-Working vs Empathetic", "Tough Negotiator vs Empathetic",
    "Moderate Changes vs Very Few", "Complete Overhaul vs Very Few",
    "1 child vs No children", "2 children vs No children",
    "3 children vs No children"
  )
  C[1L, 1L] <- -1
  C[2L, 2L] <- 1
  C[3L, c(3L, 5L)] <- c(1, -1)
  C[4L, c(4L, 5L)] <- c(1, -1)
  C[5L, 5L] <- -1
  C[6L, c(6L, 5L)] <- c(1, -1)
  C[7L, c(7L, 5L)] <- c(1, -1)
  C[8L, c(8L, 5L)] <- c(1, -1)
  C[9L, 9L] <- 1
  C[10L, 10L] <- 1
  C[11L, 11L] <- 1
  C[12L, 12L] <- 1
  C[13L, 13L] <- 1
  colnames(C) <- coordinate_names
  C
}

.run_amce <- function(prepared) {
  dx <- as.matrix(prepared$deltaX)
  p <- ncol(dx)
  coordinate_names <- colnames(dx) %||%
    prepared$coordinate_dictionary$name %||% paste0("b", seq_len(p))
  X <- cbind(`(Candidate A intercept)` = 1, dx)
  fit <- .cluster_lm(prepared$y, X, prepared$respondent_id)
  C <- .amce_contrasts(p, coordinate_names)
  b <- fit$coef[-1L]
  V <- fit$vcov[-1L, -1L, drop = FALSE]
  estimate <- as.numeric(C %*% b)
  variance <- diag(C %*% V %*% t(C))
  se <- sqrt(pmax(variance, 0))
  data.frame(
    contrast = rownames(C), estimate = estimate, se = se,
    conf_low = estimate - stats::qnorm(0.975) * se,
    conf_high = estimate + stats::qnorm(0.975) * se,
    n_tasks = fit$n, n_respondents = fit$clusters,
    estimand = paste(
      "Respondent-clustered, difference-coded linear-probability AMCE-style",
      "marginal choice effect; this is not a mixed-logit preference coefficient."
    ),
    design_condition = paste(
      "Conditional on the advertised independent randomized profile assignment;",
      "the fielded protocol probabilities were not machine-verified."
    ), stringsAsFactors = FALSE
  )
}

.amce_structural_parallel <- function(amce, qoi) {
  theta <- qoi$theta_reporting_basis
  if (inherits(theta, "scmix_paper_quantity")) theta <- theta$estimate
  if (!is.numeric(theta) || is.null(names(theta))) return(NULL)
  map <- c(
    "Female vs Male" = "female_vs_male",
    "Previously ran: Yes vs No" = "run_yes_vs_no",
    "Collaborative vs Empathetic" = "talent_collaborative_vs_empathetic",
    "Determined vs Empathetic" = "talent_determined_vs_empathetic",
    "Assertive vs Empathetic" = "talent_assertive_vs_empathetic",
    "Good Communicator vs Empathetic" =
      "talent_good_communicator_vs_empathetic",
    "Hard-Working vs Empathetic" = "talent_hard_working_vs_empathetic",
    "Tough Negotiator vs Empathetic" = "talent_tough_negotiator_vs_empathetic",
    "Moderate Changes vs Very Few" = "agenda_moderate_vs_very_few",
    "Complete Overhaul vs Very Few" = "agenda_complete_vs_very_few",
    "1 child vs No children" = "one_child_vs_none",
    "2 children vs No children" = "two_children_vs_none",
    "3 children vs No children" = "three_children_vs_none"
  )
  target <- unname(map[amce$contrast])
  if (anyNA(target) || !all(target %in% names(theta))) return(NULL)
  structural <- as.numeric(theta[target])
  data.frame(
    contrast = amce$contrast,
    amce_probability_effect = amce$estimate,
    amce_cluster_se = amce$se,
    structural_logit_preference = structural,
    structural_component = target,
    sign_agreement = sign(amce$estimate) == sign(structural),
    comparison_status = paste(
      "parallel qualitative check only: different estimands and scales;",
      "no equality test or discrepancy interval is reported"
    ), stringsAsFactors = FALSE
  )
}

.party_by_task <- function(prepared) {
  meta <- prepared$respondent_meta
  if (!is.data.frame(meta) ||
      !all(c("respondent_id", "party") %in% names(meta))) return(NULL)
  as.character(meta$party[match(as.character(prepared$respondent_id),
                                as.character(meta$respondent_id))])
}

.respondent_field_by_task <- function(prepared, field) {
  meta <- prepared$respondent_meta
  if (!is.data.frame(meta) ||
      !all(c("respondent_id", field) %in% names(meta))) return(NULL)
  as.character(meta[[field]][match(as.character(prepared$respondent_id),
                                   as.character(meta$respondent_id))])
}

.serial_summary <- function(task_predictions) {
  d <- task_predictions
  d$residual <- d$observed - d$predicted
  d <- d[order(d$respondent_id, d$task_order, method = "radix"), , drop = FALSE]
  parts <- split(seq_len(nrow(d)), d$respondent_id)
  pairs <- do.call(rbind, lapply(parts, function(ii) {
    if (length(ii) < 2L) return(NULL)
    data.frame(
      respondent_id = d$respondent_id[ii[-1L]],
      task_order = d$task_order[ii[-1L]],
      lag_observed = d$observed[ii[-length(ii)]],
      observed = d$observed[ii[-1L]],
      lag_residual = d$residual[ii[-length(ii)]],
      residual = d$residual[ii[-1L]], stringsAsFactors = FALSE
    )
  }))
  if (is.null(pairs) || nrow(pairs) < 2L) return(NULL)
  serial_fit <- .cluster_lm(
    pairs$residual, cbind(intercept = 1, lag_residual = pairs$lag_residual),
    pairs$respondent_id
  )
  order_fit <- .cluster_lm(
    d$residual, cbind(intercept = 1, task_order = d$task_order),
    d$respondent_id
  )
  data.frame(
    diagnostic = c("adjacent residual correlation",
                   "adjacent residual slope", "task-order residual slope"),
    estimate = c(stats::cor(pairs$lag_residual, pairs$residual),
                 serial_fit$coef[[2L]], order_fit$coef[[2L]]),
    se = c(NA_real_, sqrt(serial_fit$vcov[2L, 2L]),
           sqrt(order_fit$vcov[2L, 2L])),
    n_rows = c(nrow(pairs), nrow(pairs), nrow(d)),
    interpretation = paste(
      "Descriptive held-out residual diagnostic; it can reveal dependence or",
      "order-related lack of fit but cannot establish shock independence."
    ), stringsAsFactors = FALSE
  )
}

.completion_object <- function(prepared, completion_fun, design_audit = NULL) {
  meta <- design_audit$completion$status %||% prepared$respondent_meta
  if (!is.data.frame(meta) || !"respondent_id" %in% names(meta)) return(NULL)
  completed_name <- intersect(
    c("completed_tasks", "T_i", "n_tasks", "tasks"), names(meta)
  )
  if (!length(completed_name)) return(NULL)
  completed <- as.numeric(meta[[completed_name[[1L]]]])
  if (any(!is.finite(completed)) || length(unique(completed)) < 2L) return(NULL)
  predictor_names <- intersect(
    c("party", "respondent_gender", "age", "finished", "progress",
      "all_primary_demographics_missing", "primary_demographics_valid",
      "final_analysis_sample"), names(meta)
  )
  pattern <- if (all(c("finished", "progress") %in% names(meta))) {
    paste0(ifelse(meta$finished, "finished", "unfinished"),
           "_progress", meta$progress, "_T", completed)
  } else paste0("T", completed)
  completion_fun(
    completed_tasks = completed,
    predictors = meta[, predictor_names, drop = FALSE],
    completion_pattern = pattern, respondent_id = meta$respondent_id
  )
}

.run_amce_by_group <- function(prepared, group, group_name) {
  if (is.null(group) || length(group) != length(prepared$y) || anyNA(group)) {
    return(NULL)
  }
  out <- lapply(unique(as.character(group)), function(level) {
    keep <- as.character(group) == level
    sub <- prepared
    sub$deltaX <- prepared$deltaX[keep, , drop = FALSE]
    sub$y <- prepared$y[keep]
    sub$respondent_id <- prepared$respondent_id[keep]
    tab <- .run_amce(sub)
    tab$group_variable <- group_name
    tab$group <- level
    tab
  })
  do.call(rbind, out)
}

.flatten_information <- function(inference) {
  details <- inference$fold_details
  if (!is.list(details) || !length(details)) return(NULL)
  do.call(rbind, lapply(seq_along(details), function(k) {
    eig <- details[[k]]$information_eigenvalues
    if (!is.numeric(eig) || !length(eig)) return(NULL)
    data.frame(
      fold = k, eigen_index = seq_along(eig), eigenvalue = eig,
      structural_min = details[[k]]$information_structural_min %||% min(eig),
      structural_norm = details[[k]]$structural_norm %||% NA_character_,
      source = paste(
        "Complete respondent-sequence scores in the finite-sieve structural",
        "norm used by the diagnostic inference routine."
      ), stringsAsFactors = FALSE
    )
  }))
}

.profile_status <- function(profiles) {
  required <- c(
    "kappa", "gender average preference",
    "Democrat-Republican gender difference",
    "smallest active covariance eigenvalue", "complete-overhaul contest",
    "moderate-changes contest", "very-few-changes contest"
  )
  objects <- if (inherits(profiles, "scmix_profile_sequence_likelihood")) {
    list(`unnamed supplied profile` = profiles)
  } else if (is.list(profiles)) profiles else list()
  data.frame(
    direction = required,
    status = vapply(required, function(nm) {
      hit <- objects[[nm]]
      if (inherits(hit, "scmix_profile_sequence_likelihood") &&
          isTRUE(hit$verified_profile)) "run_descriptive_profile" else "not_run"
    }, character(1L)),
    note = paste(
      "A profile requires nuisance reoptimization at every grid point with",
      "sieve tuning fixed; a likelihood slice is not relabeled as a profile."
    ), stringsAsFactors = FALSE
  )
}

.collect_quantities <- function(x, path = "qoi", depth = 0L) {
  if (inherits(x, "scmix_paper_quantity")) {
    return(stats::setNames(list(x), path))
  }
  if (!is.list(x) || depth >= 6L || !length(x)) return(list())
  nm <- names(x)
  if (is.null(nm)) nm <- paste0("item", seq_along(x))
  out <- list()
  for (j in seq_along(x)) {
    if (is.environment(x[[j]]) || is.function(x[[j]])) next
    child <- .collect_quantities(x[[j]], paste(path, nm[[j]], sep = "/"),
                                 depth + 1L)
    out <- c(out, child)
  }
  out
}

.quantity_tables <- function(qoi, inference) {
  objects <- .collect_quantities(qoi)
  if (!length(objects)) {
    return(list(
      estimates = data.frame(
        quantity_id = character(), quantity = character(),
        component = character(), estimate = numeric(), source = character(),
        regular_interval = character(), stringsAsFactors = FALSE
      ),
      gates = data.frame(
        quantity_id = character(), quantity = character(),
        plugin_gate_status = character(), gate_value = numeric(),
        gate_margin = numeric(), gate_reason = character(), support = character(),
        inference_status = character(), regular_reporting = character(),
        majority_claim = character(), stringsAsFactors = FALSE
      )
    ))
  }
  estimates <- do.call(rbind, lapply(names(objects), function(nm) {
    value <- unlist(objects[[nm]]$estimate, recursive = TRUE, use.names = TRUE)
    value <- value[is.numeric(value)]
    if (!length(value)) return(NULL)
    if (is.null(names(value)) || any(!nzchar(names(value)))) {
      names(value) <- paste0("value_", seq_along(value))
    }
    data.frame(
      quantity_id = nm, quantity = objects[[nm]]$quantity,
      component = names(value), estimate = as.numeric(value),
      source = paste0("full-sample structural plug-in; ",
                      "posterior respondent summaries used: no"),
      regular_interval = "not linked by this generic exporter",
      stringsAsFactors = FALSE
    )
  }))
  inference_status <- inference$status %||% "not supplied"
  gates <- do.call(rbind, lapply(names(objects), function(nm) {
    object <- objects[[nm]]
    gate <- object$gate
    gate_state <- if (is.null(gate)) "not_applicable" else
      if (isTRUE(gate$pass)) "plugin_margin_met" else
        if (identical(gate$pass, FALSE)) "plugin_margin_not_met" else
          "margin_not_prespecified"
    support <- object$details$conditional_protocol_support %||%
      object$details$support %||% "support not audited"
    data.frame(
      quantity_id = nm, quantity = object$quantity,
      plugin_gate_status = gate_state,
      gate_value = gate$value %||% NA_real_, gate_margin = gate$margin %||% NA_real_,
      gate_reason = gate$reason %||% "no quantity-specific plug-in gate",
      support = support, inference_status = inference_status,
      regular_reporting = if (inference_status %in%
        c("available", "conditional_available"))
        "requires exact target linkage and all global gates" else "withheld",
      majority_claim = "withheld unless a linked regular interval excludes 1/2",
      stringsAsFactors = FALSE
    )
  }))
  list(estimates = estimates, gates = gates)
}

.application_qoi_tables <- function(qoi) {
  empty_headline <- data.frame(
    quantity = character(), component = character(), estimate = numeric(),
    source = character(), posterior_summaries_used = logical(),
    stringsAsFactors = FALSE
  )
  empty_subgroup <- data.frame(
    grouping = character(), group = character(), component = character(),
    estimate = numeric(), stringsAsFactors = FALSE
  )
  if (!is.list(qoi)) {
    return(list(headline = empty_headline, subgroup_raw = empty_subgroup,
                mrs = data.frame()))
  }
  headline <- list()
  add_headline <- function(quantity, value, source) {
    component <- names(value)
    value <- as.numeric(value)
    if (is.null(component)) component <- paste0("value_", seq_along(value))
    headline[[length(headline) + 1L]] <<- data.frame(
      quantity = quantity, component = component, estimate = value,
      source = source, posterior_summaries_used = FALSE,
      stringsAsFactors = FALSE
    )
  }
  kappa_value <- if (inherits(qoi$kappa, "scmix_paper_quantity"))
    qoi$kappa$estimate else qoi$kappa
  if (.finite_scalar(kappa_value)) {
    value <- kappa_value
    names(value) <- names(kappa_value) %||% "kappa"
    add_headline("position/alternative intercept", value,
                 "full-sample integrated sequence likelihood")
  }
  theta_reporting <- if (inherits(qoi$theta_reporting_basis,
                                  "scmix_paper_quantity")) {
    qoi$theta_reporting_basis$estimate
  } else qoi$theta_reporting_basis
  if (is.numeric(theta_reporting)) {
    add_headline("average preference in reporting basis",
                 theta_reporting,
                 "linear transform of the full-sample plug-in mean")
  }
  subgroup_rows <- list()
  add_subgroup <- function(x, grouping, add_headline_fallback = TRUE) {
    if (!is.matrix(x) || !is.numeric(x)) return(NULL)
    rn <- rownames(x) %||% paste0("group_", seq_len(nrow(x)))
    cn <- colnames(x) %||% paste0("b", seq_len(ncol(x)))
    raw <- do.call(rbind, lapply(seq_len(nrow(x)), function(j) {
      data.frame(
        grouping = grouping, group = rn[[j]], component = cn,
        estimate = as.numeric(x[j, ]), stringsAsFactors = FALSE
      )
    }))
    subgroup_rows[[length(subgroup_rows) + 1L]] <<- raw
    if (ncol(x) >= 1L && isTRUE(add_headline_fallback)) {
      value <- -as.numeric(x[, 1L])
      names(value) <- paste0(grouping, ":", rn)
      add_headline(
        paste0("female-vs-male average preference by ", grouping), value,
        "linear transform (-Male coordinate) of subgroup plug-in means"
      )
    }
    invisible(NULL)
  }
  has_party_reporting <- inherits(qoi$subgroup_reporting_theta_party,
                                  "scmix_paper_quantity")
  has_gender_reporting <- inherits(
    qoi$subgroup_reporting_theta_respondent_gender, "scmix_paper_quantity"
  )
  add_subgroup(qoi$subgroup_raw_theta_party, "party",
               add_headline_fallback = !has_party_reporting)
  add_subgroup(qoi$subgroup_raw_theta_respondent_gender,
               "respondent_gender",
               add_headline_fallback = !has_gender_reporting)
  add_reporting_subgroup <- function(x, grouping) {
    if (inherits(x, "scmix_paper_quantity")) x <- x$estimate
    if (is.numeric(x) && !is.null(names(x))) {
      keep <- endsWith(names(x), ":female_vs_male")
      if (!any(keep)) return(NULL)
      value <- x[keep]
      names(value) <- sub(":female_vs_male$", "", names(value))
    } else if (is.matrix(x) && is.numeric(x) && !is.null(colnames(x)) &&
               "female_vs_male" %in% colnames(x)) {
      value <- x[, "female_vs_male"]
      names(value) <- rownames(x) %||% seq_len(nrow(x))
    } else return(NULL)
    names(value) <- paste0(grouping, ":", names(value))
    add_headline(
      paste0("female-vs-male average preference by ", grouping),
      value, "classed full-sample reporting-basis subgroup plug-in"
    )
    invisible(NULL)
  }
  add_reporting_subgroup(qoi$subgroup_reporting_theta_party, "party")
  add_reporting_subgroup(qoi$subgroup_reporting_theta_respondent_gender,
                         "respondent_gender")
  mrs <- if (is.list(qoi$mrs)) data.frame(
    status = qoi$mrs$status %||% "not_run",
    reason = qoi$mrs$reason %||% "MRS status not documented",
    stringsAsFactors = FALSE
  ) else data.frame()
  list(
    headline = if (length(headline)) do.call(rbind, headline) else empty_headline,
    subgroup_raw = if (length(subgroup_rows)) do.call(rbind, subgroup_rows) else
      empty_subgroup,
    mrs = mrs
  )
}

.named_value <- function(x, labels, default = NA_real_) {
  if (is.null(x) || is.null(names(x))) return(rep(default, length(labels)))
  as.numeric(x[labels])
}

.inference_tables <- function(inference, transforms = NULL) {
  labels <- names(inference$estimate)
  targets <- if (is.character(labels) && length(labels)) data.frame(
    target = labels,
    one_step_estimate = as.numeric(inference$estimate),
    plugin_estimate = .named_value(inference$plugin_estimate, labels),
    one_step_adjustment = .named_value(inference$one_step_adjustment, labels),
    diagnostic_se = .named_value(inference$diagnostic_se, labels),
    formal_se = .named_value(inference$se, labels),
    formal_ci_low = .named_value(inference$ci_lower, labels),
    formal_ci_high = .named_value(inference$ci_upper, labels),
    target_inference_available = as.logical(.named_value(
      inference$target_inference_available, labels, default = FALSE
    )),
    inference_status = inference$status %||% "not_run",
    diagnostic_only = !isTRUE(inference$inference_available),
    stringsAsFactors = FALSE
  ) else data.frame(
    target = character(), one_step_estimate = numeric(),
    plugin_estimate = numeric(), one_step_adjustment = numeric(),
    diagnostic_se = numeric(), formal_se = numeric(), formal_ci_low = numeric(),
    formal_ci_high = numeric(), target_inference_available = logical(),
    inference_status = character(), diagnostic_only = logical(),
    stringsAsFactors = FALSE
  )
  transform_rows <- list()
  if (is.list(transforms)) {
    for (nm in names(transforms)) {
      x <- transforms[[nm]]
      if (!inherits(x, "scmix_delta_transform")) next
      value <- as.numeric(x$estimate)
      component <- names(x$estimate) %||% paste0("value_", seq_along(value))
      transform_rows[[length(transform_rows) + 1L]] <- data.frame(
        transform = nm, component = component, estimate = value,
        diagnostic_se = .named_value(x$diagnostic_se, component),
        formal_se = .named_value(x$se, component),
        formal_ci_low = .named_value(x$ci_lower, component),
        formal_ci_high = .named_value(x$ci_upper, component),
        target_inference_available = as.logical(.named_value(
          x$target_inference_available, component, default = FALSE
        )),
        status = x$status %||% "not_run",
        diagnostic_only = !isTRUE(x$inference_available),
        gate_reason = x$reason %||% "",
        stringsAsFactors = FALSE
      )
    }
  }
  transformed <- if (length(transform_rows)) do.call(rbind, transform_rows) else
    data.frame(
      transform = character(), component = character(), estimate = numeric(),
      diagnostic_se = numeric(), formal_se = numeric(), formal_ci_low = numeric(),
      formal_ci_high = numeric(), target_inference_available = logical(),
      status = character(), diagnostic_only = logical(), gate_reason = character(),
      stringsAsFactors = FALSE
    )
  summary <- data.frame(
    status = inference$status %||% "not_run",
    inference_available = isTRUE(inference$inference_available),
    inference_claim = inference$inference_claim %||% "not_available",
    reason = inference$reason %||% "",
    n_respondents = inference$n_respondents %||% NA_integer_,
    outer_folds = inference$outer_folds %||% NA_integer_,
    q = inference$q %||% NA_integer_,
    riesz_equation_max_relative_residual =
      inference$riesz_equation_max_relative_residual %||% NA_real_,
    ridge_max_relative_sensitivity =
      inference$ridge_max_relative_sensitivity %||% NA_real_,
    analysis_signature = inference$analysis_signature %||% NA_character_,
    stringsAsFactors = FALSE
  )
  list(targets = targets, transforms = transformed, summary = summary)
}

.status_table <- function(q_summary, heldout_state, optimization, numerical,
                          completion, information, inference, interaction_file,
                          z19_file, sensitivity_file,
                          common_rank = NULL, q2_numerical = NULL,
                          sensitivity_bridge = NULL) {
  q_available <- inherits(common_rank, "sw_common_outer_rank_assessment") ||
    sum(q_summary$artifact_available) >= 2L
  opt_available <- inherits(optimization, "scmix_optimization_audit")
  num_available <- inherits(numerical, "scmix_integration_refinement") ||
    inherits(numerical, "scmix_numerical_gate")
  numerical_state <- if (num_available) "run_descriptive" else
    if (!is.null(numerical)) "run_failed_or_unverified" else "not_run"
  bridge_status <- function(component, fallback) {
    if (!isTRUE(sensitivity_bridge$validated)) return(fallback)
    tab <- sensitivity_bridge$application_status
    hit <- tab$status[tab$component == component]
    if (length(hit) == 1L) as.character(hit) else fallback
  }
  misspecification_validated <-
    isTRUE(sensitivity_bridge$misspecification$validated)
  simulated_status <- function() {
    if (misspecification_validated) {
      paste0("run_design_specific_simulated_data_diagnostic;",
             " empirical_alternative_refit_not_run")
    } else "maintained_assumption; alternative_refit_not_run"
  }
  rows <- list(
    c("rank q=0,1,2 stability", if (inherits(
      common_rank, "sw_common_outer_rank_assessment"))
      "run_common_outer_crossfitted_diagnostic" else if (q_available)
        "run_full_sample_descriptive_only" else "not_run",
      paste(
        "Alternative ranks are sensitivities; primary q=1 is not reselected.",
        "Common-outer status requires the separate rank-assessment artifact."
      )),
    c("q=2 integration and loading-orientation stability", if (inherits(
      q2_numerical, "sw_q2_numerical_orientation_assessment")) {
        if (isTRUE(q2_numerical$gate$pass)) "run_gate_pass" else "run_gate_fail"
      } else "not_run",
      paste(
        "Fresh fixed-learner q=2 node refits and nontrivial loading rotations;",
        "an empirical pass does not establish an asymptotic numerical rate."
      )),
    c("integration resolution and replication", numerical_state,
      "A fixed finite grid is a maintained approximation until fresh-refit refinement is run."),
    c("multiple starts and attained optimization", if (opt_available)
      "run_diagnostic" else "not_run",
      "Attained gradients/objective dispersion do not bound the global optimum gap."),
    c("skewed residual distribution", simulated_status(),
      paste("Positive/negative-skew simulated-data diagnostics were run when",
            "validated; the empirical fit remains normal and no separately",
            "identified skew-family likelihood was fitted.")),
    c("bimodal residual distribution", simulated_status(),
      paste("A bimodal-factor simulated-data diagnostic was run when validated;",
            "the empirical fit remains normal and no separately identified",
            "mixture likelihood was fitted.")),
    c("heavy-tailed residual distribution", simulated_status(),
      paste("A heavy-tail simulated-data diagnostic was run when validated;",
            "the empirical fit remains normal and no separately identified",
            "heavy-tail likelihood was fitted.")),
    c("covariance varying by party/Z", simulated_status(),
      paste("A party-varying covariance DGP diagnostic was run when validated;",
            "the empirical fit retains common covariance and no covariance-by-Z",
            "alternative was fitted.")),
    c("task order, fatigue, and learning", bridge_status(
      "task-order/fatigue/learning diagnostics", heldout_state),
      paste("Held-out task-order diagnostics were implemented when validated;",
            "the task-varying structural alternative remains not_run.")),
    c("serial shock dependence", bridge_status(
      "serial dependence diagnostics", heldout_state),
      paste("Adjacent calibration/residual diagnostics and a design-specific",
            "serial-shock DGP simulation were implemented when validated;",
            "the empirical serial-shock likelihood remains not_run.")),
    c("position/profile swap", bridge_status(
      "position/profile swap", if (heldout_state == "not_run") "not_run" else
        "primary_kappa_run; swap_refit_not_run"),
      paste("The primary utility includes kappa. A validated A/B swap is an",
            "optimization-equivariance diagnostic, not assumption verification.")),
    c("random response scale", if (misspecification_validated) paste0(
      "run_design_specific_simulated_data_diagnostic; ",
      "maintained_fixed_logit_scale; empirical_refit_not_run"
    ) else "maintained_fixed_logit_scale; refit_not_run",
      paste("A random-scale DGP diagnostic was run when validated; scale",
            "heterogeneity is not identified by merely rescaling fitted",
            "coefficients and no empirical alternative was fitted.")),
    c("completion/attrition", bridge_status(
      "completion/sample sensitivity", if (inherits(completion,
        "scmix_completion_assessment")) "run_descriptive" else
        "not_run_for_raw_completion_universe"),
      paste("Raw completion diagnostics and, when validated, 1,191-versus-1,249",
            "sample refits do not establish noninformative completion.")),
    c("post-conjoint 19-Z moderators", bridge_status(
      "primary 15-Z vs post-conjoint 19-Z",
      if (file.exists(z19_file)) "artifact_supplied; review_before_use" else
        "not_run"),
      "Four post-conjoint fields are excluded from the primary moderator set."),
    c("Male x prior-run interaction", bridge_status(
      "Male x prior-run interaction",
      if (file.exists(interaction_file)) "artifact_supplied; review_before_use" else
        "not_run"),
      paste("Targeted augmented-basis sensitivity; formal inference and",
            "document-verified fielded support remain unavailable.")),
    c("application structural-sensitivity battery", bridge_status(
      "structural sensitivity protocol completeness",
      if (file.exists(sensitivity_file))
        "artifact_supplied; review_component_status" else "not_run"),
      paste("The validated battery includes selected descriptive empirical",
            "sensitivities and design-specific simulations, but is incomplete;",
            "unfitted alternative families remain not_run and inherit no",
            "normal-model identification result.")),
    c("design-specific misspecification simulations",
      if (misspecification_validated) paste0(
        "run_simulated_data_diagnostic_incomplete; ",
        "empirical_alternative_refits_not_run"
      ) else "not_run",
      if (misspecification_validated) {
        sensitivity_bridge$misspecification$validation$distinction
      } else paste(
        "No validated shape/covariance/serial/scale simulated-data battery",
        "was ingested; empirical alternative-family refits remain not_run."
      )),
    c("finite-sieve local information", if (is.data.frame(information) &&
      nrow(information)) "run_diagnostic" else "not_run",
      "Small eigenvalues diagnose weak local information; no global identification claim."),
    c("regular respondent-level inference", inference$status %||% "not_run",
      inference$reason %||%
        "Any available interval remains conditional on documented high-level assumptions.")
  )
  data.frame(
    component = vapply(rows, `[[`, character(1L), 1L),
    status = vapply(rows, `[[`, character(1L), 2L),
    note = vapply(rows, `[[`, character(1L), 3L),
    maintained_assumption_verified = FALSE,
    stringsAsFactors = FALSE
  )
}

.main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- .parse_args(args)
  script <- .script_file()
  project <- normalizePath(file.path(dirname(script), "../../.."),
                           mustWork = TRUE)
  app <- file.path(project, "applications", "sw2022")
  extension_source <- file.path(app, "R", "assessment_extensions.R")
  if (!file.exists(extension_source)) {
    stop("Application assessment extension source is missing: ",
         extension_source, call. = FALSE)
  }
  source(extension_source, local = environment())
  profile <- opt$profile %||% Sys.getenv("SCONJOINT_SW_PROFILE", "production")
  prep_path <- opt$prep %||% file.path(app, "results", "prep_analysis_data.rds")
  fit_dir <- opt$fit_dir %||%
    file.path(app, "results", "mixed_logit", profile)
  out_dir <- opt$out_dir %||%
    file.path(app, "results", "assessment", profile)
  table_dir <- file.path(out_dir, "tables")
  object_dir <- file.path(out_dir, "objects")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_dir, recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(prep_path)) {
    stop("Prepared input not found: ", prep_path,
         ". Run 01_prepare_data.R first.", call. = FALSE)
  }
  if (!requireNamespace("sconjoint", quietly = TRUE)) {
    stop("Install this checkout into the project-local R library before running assessment.",
         call. = FALSE)
  }
  fun <- function(name) getExportedValue("sconjoint", name)
  prepared <- readRDS(prep_path)
  design_audit_path <- file.path(app, "results", "design_completion_audit.rds")
  design_task_path <- file.path(app, "results", "design_task_metadata.rds")
  design_audit <- .read_optional(design_audit_path)
  design_task <- .read_optional(design_task_path)
  completion_task_path <- design_audit$completion$task_audit_path %||%
    file.path(app, "results", "completion_task_audit.rds")
  completion_task <- .read_optional(completion_task_path)
  expected_completion_task_md5 <- design_audit$completion$task_audit_md5
  if (!is.null(expected_completion_task_md5) &&
      (!file.exists(completion_task_path) || !identical(
        as.character(expected_completion_task_md5),
        unname(as.character(tools::md5sum(completion_task_path)))))) {
    stop(
      "The raw-universe completion task audit changed after the design audit. ",
      "Rerun 02_design_completion_audit.R.", call. = FALSE
    )
  }

  paths <- c(
    primary_full = .first_existing(fit_dir, "fit_primary_full.rds"),
    primary_assembled = .first_existing(fit_dir, "fit_primary_assembled.rds"),
    q0_full = .first_existing(fit_dir, "fit_q0_full.rds"),
    q2_full = .first_existing(fit_dir, "fit_q2_full.rds"),
    q_sensitivity = .first_existing(fit_dir, "q_sensitivity.rds"),
    qoi = .first_existing(fit_dir, "qoi_plugin.rds"),
    inference = .first_existing(
      fit_dir, c("inference_verified.rds", "inference_diagnostic.rds")),
    inference_transforms = .first_existing(
      fit_dir, c("inference_verified_transforms.rds",
                 "inference_transforms.rds")),
    inference_manifest = .first_existing(fit_dir, "inference_manifest.rds"),
    inference_verification_status = .first_existing(
      fit_dir, "inference_verification_status.rds"),
    optimization = .first_existing(
      fit_dir, c("optimization_primary_nested.rds",
                 "optimization_primary_full.rds")),
    numerical = .first_existing(
      fit_dir, c("integration_refinement.rds", "numerical_refinement.rds")),
    profiles = .first_existing(
      fit_dir, c("profile_likelihoods.rds", "profiles.rds")),
    sensitivity = .first_existing(
      fit_dir, c("structural_sensitivity.rds", "sensitivity.rds")),
    interaction = .first_existing(
      fit_dir, c("fit_male_run_interaction.rds", "sensitivity_male_run.rds")),
    z19 = .first_existing(
      fit_dir, c("fit_z19_sensitivity.rds", "sensitivity_z19.rds"))
  )
  rank_dir <- file.path(app, "results", "rank_assessment", profile)
  rank_common_path <- file.path(rank_dir, "common_outer_rank_assessment.rds")
  rank_q2_path <- file.path(
    rank_dir, "q2_numerical_orientation_assessment.rds")
  sensitivity_bridge <- .load_sensitivity_bridge(fit_dir, profile)
  party_gender_mean_bridge <- .load_party_gender_mean_bridge(app, profile)
  if (isTRUE(sensitivity_bridge$validated)) {
    paths[["sensitivity"]] <-
      sensitivity_bridge$source_paths[["structural"]]
    paths[["interaction"]] <-
      sensitivity_bridge$source_paths[["interaction"]]
    paths[["z19"]] <- sensitivity_bridge$source_paths[["z19"]]
  }
  path_or_missing <- function(nm) if (is.na(paths[[nm]])) "" else paths[[nm]]
  primary_full <- .read_optional(path_or_missing("primary_full"))
  assembled <- .read_optional(path_or_missing("primary_assembled"))
  q0 <- .read_optional(path_or_missing("q0_full"))
  q2 <- .read_optional(path_or_missing("q2_full"))
  q_sensitivity <- .read_optional(path_or_missing("q_sensitivity"))
  qoi <- .read_optional(path_or_missing("qoi"))
  inference <- .read_optional(path_or_missing("inference")) %||% list(
    status = "not_run", reason = "inference artifact not available",
    inference_available = FALSE
  )
  inference_transforms <- .read_optional(path_or_missing("inference_transforms"))
  inference_manifest <- .read_optional(path_or_missing("inference_manifest"))
  inference_verification_status <-
    .read_optional(path_or_missing("inference_verification_status"))
  optimization <- .read_optional(path_or_missing("optimization"))
  numerical <- .read_optional(path_or_missing("numerical"))
  profiles <- .read_optional(path_or_missing("profiles"))
  sensitivity <- .read_optional(path_or_missing("sensitivity"))
  common_rank <- .read_optional(rank_common_path)
  q2_numerical <- .read_optional(rank_q2_path)
  .validate_rank_artifact(
    common_rank, "sw_common_outer_rank_assessment",
    "common_outer_rank_assessment", profile, prep_path
  )
  .validate_rank_artifact(
    q2_numerical, "sw_q2_numerical_orientation_assessment",
    "q2_numerical_orientation_assessment", profile, prep_path
  )
  .clear_sensitivity_bridge_tables(table_dir)
  sensitivity_table_copies <- .copy_sensitivity_bridge_tables(
    sensitivity_bridge, table_dir)
  .clear_party_gender_mean_tables(table_dir)
  party_gender_mean_table_copies <- .copy_party_gender_mean_tables(
    party_gender_mean_bridge, table_dir
  )
  .write_csv(sensitivity_bridge$status,
             file.path(table_dir, "sensitivity_bridge_status.csv"))
  .write_csv(sensitivity_bridge$artifact_audit,
             file.path(table_dir, "sensitivity_bridge_artifact_hashes.csv"))
  .write_csv(sensitivity_bridge$input_audit,
             file.path(table_dir, "sensitivity_bridge_input_hashes.csv"))
  .write_csv(sensitivity_table_copies,
             file.path(table_dir, "sensitivity_bridge_table_copies.csv"))
  .write_csv(
    party_gender_mean_bridge$status,
    file.path(table_dir, "party_gender_bridge_status.csv")
  )
  .write_csv(
    party_gender_mean_bridge$artifact_audit,
    file.path(table_dir, "party_gender_bridge_artifact_hashes.csv")
  )
  .write_csv(
    party_gender_mean_bridge$input_audit,
    file.path(table_dir, "party_gender_bridge_input_hashes.csv")
  )
  .write_csv(
    party_gender_mean_table_copies,
    file.path(table_dir, "party_gender_bridge_table_copies.csv")
  )
  if (isTRUE(sensitivity_bridge$misspecification$validated)) {
    .write_csv(
      sensitivity_bridge$misspecification$status,
      file.path(table_dir, "sensitivity_misspecification_bridge_status.csv")
    )
    .write_csv(
      sensitivity_bridge$misspecification$validation$checks,
      file.path(table_dir, "sensitivity_misspecification_validation.csv")
    )
    .write_csv(
      sensitivity_bridge$misspecification$artifact_audit,
      file.path(table_dir,
                "sensitivity_misspecification_artifact_hashes.csv")
    )
    .write_csv(
      sensitivity_bridge$misspecification$input_audit,
      file.path(table_dir,
                "sensitivity_misspecification_input_hashes.csv")
    )
  }

  ## The fold constructors below establish that the final grid was tuned and
  ## fitted without each outer validation respondent.  They cannot undo an
  ## earlier, outcome-informed choice of the candidate grid itself.  This
  ## application revised that grid after a same-sample computational pilot, so
  ## its cross-fitted predictions are useful diagnostics but are not a clean
  ## end-to-end held-out assessment of a fully prespecified workflow.
  grid_provenance <- assembled$sw_application_specification$
    learner_grid_provenance %||%
    primary_full$sw_application_specification$learner_grid_provenance %||% ""
  grid_same_sample_adapted <- nzchar(grid_provenance) &&
    grepl("same-sample", grid_provenance, ignore.case = TRUE)

  q_summary <- rbind(
    .fit_summary_row(q0, "q=0 sensitivity", 0L),
    .fit_summary_row(primary_full, "q=1 primary", 1L),
    .fit_summary_row(q2, "q=2 sensitivity", 2L)
  )
  q_summary$integration_refinement_status <- vapply(q_summary$q, function(q) {
    if (is.na(q)) return("fit_unavailable")
    if (q == 0L) return("exact_no_integration")
    if (q == 1L && inherits(numerical, "scmix_integration_refinement")) {
      return("fresh_nested_refinement_artifact")
    }
    if (q == 1L) return("not_run")
    if (q == 2L && inherits(
      q2_numerical, "sw_q2_numerical_orientation_assessment")) {
      return("fresh_fixed_learner_refinement_and_rotation_artifact")
    }
    "not_run_separately_for_rank_sensitivity"
  }, character(1L))
  .write_csv(q_summary, file.path(table_dir, "rank_q_stability.csv"))
  if (inherits(q_sensitivity, "scmix_q_sensitivity")) {
    q_sensitivity_table <- q_sensitivity$table
    old_score <- "score.heldout_sequence"
    old_difference <- paste0(old_score, ".difference_from_primary")
    names(q_sensitivity_table)[names(q_sensitivity_table) == old_score] <-
      "score.selected_inner_cv_sequence_log_score"
    names(q_sensitivity_table)[names(q_sensitivity_table) == old_difference] <-
      paste0("score.selected_inner_cv_sequence_log_score",
             ".difference_from_primary")
    q_sensitivity_table$score_role <- paste(
      "Selected inner-CV tuning score from each full-sample rank fit;",
      "not a common-outer-fold predictive assessment. Rank-specific fold",
      "assignments and winner selection make the comparison descriptive."
    )
    .write_csv(q_sensitivity_table,
               file.path(table_dir, "rank_q_qoi_sensitivity.csv"))
  }

  optimization_state <- "not_run"
  if (inherits(optimization, "scmix_optimization_audit")) {
    .write_csv(optimization$summary,
               file.path(table_dir, "optimization_summary.csv"))
    .write_csv(optimization$starts,
               file.path(table_dir, "optimization_starts.csv"))
    optimization_state <- "run_diagnostic"
  }

  integration_table <- .fit_summary_row(primary_full, "primary finite grid", 1L)
  integration_table$refinement_artifact <- !is.null(numerical)
  integration_table$interpretation <- paste(
    "A finite number of nodes/draws is not treated as exact likelihood evaluation;",
    "fresh-refit refinement is required for an empirical numerical-stability claim."
  )
  .write_csv(integration_table,
             file.path(table_dir, "integration_primary_grid.csv"))
  if (inherits(numerical, "scmix_integration_refinement")) {
    .write_csv(numerical$checks,
               file.path(table_dir, "integration_refinement_checks.csv"))
    if (inherits(numerical$gate, "scmix_numerical_gate")) {
      .write_csv(numerical$gate$comparison,
                 file.path(table_dir, "integration_refinement_gate.csv"))
    }
  } else if (inherits(numerical, "scmix_numerical_gate")) {
    .write_csv(numerical$comparison,
               file.path(table_dir, "integration_refinement_gate.csv"))
  }

  prediction_objects <- NULL
  prediction_error <- NULL
  joint_extensions <- NULL
  joint_extension_error <- NULL
  if (!is.null(assembled)) {
    prediction_objects <- tryCatch({
      predictions <- fun("scmix_heldout_predictions")(
        assembled, task_order = prepared$task,
        include_counts = TRUE, include_adjacent = TRUE,
        include_repeated = TRUE
      )
      party <- .party_by_task(prepared)
      design_cell <- if (is.data.frame(design_task) &&
                         "design_cell" %in% names(design_task) &&
                         nrow(design_task) == nrow(prepared$deltaX)) {
        design_task$design_cell
      } else {
        paste0("nonzero_coordinates_",
               rowSums(abs(prepared$deltaX) > 1e-12))
      }
      fun("scmix_prediction_assessment")(
        predictions, design_cell = design_cell,
        respondent_group = party
      )
    }, error = function(e) {
      prediction_error <<- conditionMessage(e)
      NULL
    })
    joint_extensions <- tryCatch(
      .sw_exact_joint_predictions(assembled, prepared),
      error = function(e) {
        joint_extension_error <<- conditionMessage(e)
        NULL
      }
    )
  }
  heldout_state <- "not_run"
  serial <- NULL
  if (!is.null(prediction_objects)) {
    fold_construction_verified <-
      isTRUE(prediction_objects$score$verified_heldout) &&
      isTRUE(prediction_objects$calibration$verified_heldout)
    end_to_end_heldout <- fold_construction_verified &&
      !isTRUE(grid_same_sample_adapted)
    heldout_state <- if (end_to_end_heldout) {
      "run_verified_heldout"
    } else if (fold_construction_verified && isTRUE(grid_same_sample_adapted)) {
      "run_crossfitted_diagnostic_same_sample_grid_adaptation"
    } else "run_diagnostic_unverified"
    .write_csv(data.frame(
      model = names(prediction_objects$score$estimate),
      mean_complete_sequence_log_score =
        as.numeric(prediction_objects$score$estimate),
      respondent_se = as.numeric(prediction_objects$score$se),
      verified_heldout = end_to_end_heldout,
      fold_construction_verified = fold_construction_verified,
      candidate_grid_outcome_blind = !isTRUE(grid_same_sample_adapted),
      grid_provenance = grid_provenance,
      provenance = prediction_objects$score$provenance,
      stringsAsFactors = FALSE
    ), file.path(table_dir, "heldout_sequence_score.csv"))
    .write_csv(prediction_objects$calibration$marginal,
               file.path(table_dir, "calibration_marginal.csv"))
    .write_csv(prediction_objects$calibration$joint,
               file.path(table_dir, "calibration_joint.csv"))
    .write_csv(prediction_objects$calibration$response_count,
               file.path(table_dir, "calibration_response_count.csv"))
    .write_csv(prediction_objects$predictions$task,
               file.path(table_dir, "heldout_task_predictions.csv"))
    serial <- .serial_summary(prediction_objects$predictions$task)
    .write_csv(serial, file.path(table_dir, "order_serial_diagnostics.csv"))
    position <- data.frame(
      diagnostic = c("candidate-A observed choice rate",
                     "candidate-A held-out predicted rate",
                     "candidate-A calibration gap", "full-fit kappa"),
      estimate = c(
        mean(prediction_objects$predictions$task$observed),
        mean(prediction_objects$predictions$task$predicted),
        mean(prediction_objects$predictions$task$observed -
               prediction_objects$predictions$task$predicted),
        .fit_core(primary_full)$kappa %||% NA_real_
      ),
      interpretation = c(
        rep("Descriptive position/alternative-side assessment.", 3L),
        "Estimated position/alternative intercept; not a profile-swap sensitivity refit."
      ), stringsAsFactors = FALSE
    )
    .write_csv(position, file.path(table_dir, "position_diagnostics.csv"))
  } else if (!is.null(assembled)) {
    heldout_state <- "run_failed"
  }

  joint_extension_status <- if (is.null(joint_extensions)) {
    data.frame(
      component = c(
        "full eight-pattern three-task calibration",
        "prespecified task-pair joint calibration",
        "exact repeated-contrast joint calibration"
      ),
      status = if (is.null(assembled)) "not_run" else "run_failed",
      note = joint_extension_error %||%
        "The assembled held-out nuisance fit was unavailable.",
      maintained_assumption_verified = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    full_calibration <- .sw_joint_calibration_table(joint_extensions$full)
    pair_calibration <- .sw_joint_calibration_table(joint_extensions$pair)
    repeated_calibration <-
      .sw_joint_calibration_table(joint_extensions$repeated)
    .write_csv(full_calibration,
               file.path(table_dir, "calibration_full_response_pattern.csv"))
    .write_csv(pair_calibration,
               file.path(table_dir, "calibration_prespecified_task_pairs.csv"))
    .write_csv(repeated_calibration,
               file.path(table_dir, "calibration_exact_repeated_contrast.csv"))
    .write_csv(data.frame(
      diagnostic = names(joint_extensions$probability_sum_error),
      maximum_probability_sum_error =
        as.numeric(joint_extensions$probability_sum_error),
      tolerance = 1e-10,
      gate_pass = is.na(joint_extensions$probability_sum_error) |
        joint_extensions$probability_sum_error <= 1e-10,
      stringsAsFactors = FALSE
    ), file.path(table_dir, "calibration_joint_probability_checks.csv"))
    .sw_joint_extension_status(joint_extensions, heldout_state)
  }
  .write_csv(joint_extension_status,
             file.path(table_dir, "joint_calibration_extension_status.csv"))

  amce <- .run_amce(prepared)
  .write_csv(amce, file.path(table_dir, "design_amce_lpm.csv"))
  amce_structural_parallel <- .amce_structural_parallel(amce, qoi)
  .write_csv(amce_structural_parallel,
             file.path(table_dir, "amce_structural_parallel_check.csv"))
  amce_party <- .run_amce_by_group(
    prepared, .respondent_field_by_task(prepared, "party"), "party"
  )
  amce_gender <- .run_amce_by_group(
    prepared, .respondent_field_by_task(prepared, "respondent_gender"),
    "respondent_gender"
  )
  .write_csv(rbind(amce_party, amce_gender),
             file.path(table_dir, "design_amce_lpm_by_group.csv"))
  design_status <- data.frame(
    component = c(
      "realized support/rank audit", "exact ordered-contrast HT benchmark",
      "marginal AMCE-style benchmark", "structural-design discrepancy",
      "conditional-randomization completion/assignment test"
    ),
    status = c(
      if (file.exists(file.path(app, "tables", "design_rank_summary.csv")))
        "run_realized_design_algebra" else "not_run",
      "protocol_unavailable", "run_conditional_on_advertised_randomization",
      "not_run", "protocol_unavailable_not_run"
    ),
    reason = c(
      paste(
        "Realized/design-algebra ranks do not substitute for the unavailable",
        "machine-readable fielding protocol."
      ),
      paste(
        "The respondent exposure probabilities bar(pi)_i(d), assignment",
        "restrictions, and completion strata were not recovered from a fielded instrument."
      ),
      paste(
        "Respondent-clustered marginal probability benchmark; a different",
        "estimand and scale from structural mixed-logit preference coefficients."
      ),
      "No exact structural-versus-HT discrepancy is formed without protocol probabilities.",
      paste(
        "No randomization p-value is formed without the fielded randomizer,",
        "assignment restrictions, and completion-conditioned exposure probabilities."
      )
    ), maintained_assumption_verified = FALSE,
    stringsAsFactors = FALSE
  )
  .write_csv(design_status, file.path(table_dir, "design_assessment_status.csv"))
  conditional_randomization <-
    .sw_conditional_randomization_status(design_audit)
  .write_csv(conditional_randomization,
             file.path(table_dir, "conditional_randomization_test_status.csv"))

  completion_comparisons <- .sw_completion_comparisons(completion_task)
  .write_csv(completion_comparisons$summary,
             file.path(table_dir,
                       "completion_early_response_assignment_by_task.csv"))
  .write_csv(completion_comparisons$status,
             file.path(table_dir,
                       "completion_early_response_assignment_status.csv"))

  completion <- tryCatch(
    .completion_object(prepared, fun("scmix_completion_diagnostics"),
                       design_audit),
    error = function(e) NULL
  )
  completion_note <- if (inherits(completion, "scmix_completion_assessment")) {
    .write_csv(as.data.frame(as.list(completion$summary)),
               file.path(table_dir, "completion_summary.csv"))
    .write_csv(completion$associations,
               file.path(table_dir, "completion_associations.csv"))
    .write_csv(completion$completion_patterns,
               file.path(table_dir, "completion_patterns.csv"))
    "Raw completion universe contained variation and was assessed descriptively."
  } else {
    paste(
      "The prepared structural sample contains only the 1,191 three-task",
      "complete cases; raw-universe completion diagnostics were not run here."
    )
  }
  .write_csv(data.frame(
    estimand = prepared$estimand,
    completion_assessment = if (inherits(completion,
      "scmix_completion_assessment")) "run_descriptive" else
      "not_run_for_raw_completion_universe",
    note = completion_note,
    noninformative_completion_verified = FALSE,
    stringsAsFactors = FALSE
  ), file.path(table_dir, "completion_status.csv"))

  information <- .flatten_information(inference)
  .write_csv(information, file.path(table_dir, "information_eigenvalues.csv"))
  profile_status <- .profile_status(profiles)
  .write_csv(profile_status,
             file.path(table_dir, "profile_likelihood_status.csv"))
  if (inherits(sensitivity, "scmix_structural_sensitivity")) {
    .write_csv(sensitivity$status,
               file.path(table_dir, "structural_sensitivity_status.csv"))
  }

  quantity_tables <- .quantity_tables(qoi, inference)
  .write_csv(quantity_tables$estimates,
             file.path(table_dir, "structural_plugin_quantities.csv"))
  .write_csv(quantity_tables$gates,
             file.path(table_dir, "quantity_reporting_gates.csv"))
  application_qoi <- .application_qoi_tables(qoi)
  .write_csv(application_qoi$headline,
             file.path(table_dir, "headline_plugin_quantities.csv"))
  .write_csv(application_qoi$subgroup_raw,
             file.path(table_dir, "subgroup_raw_plugin_quantities.csv"))
  .write_csv(application_qoi$mrs,
             file.path(table_dir, "mrs_status.csv"))
  inference_tables <- .inference_tables(inference, inference_transforms)
  .write_csv(inference_tables$targets,
             file.path(table_dir, "inference_target_diagnostics.csv"))
  .write_csv(inference_tables$transforms,
             file.path(table_dir, "inference_transform_diagnostics.csv"))
  .write_csv(inference_tables$summary,
             file.path(table_dir, "inference_summary.csv"))
  .write_csv(inference$verification_evidence,
             file.path(table_dir, "inference_verification_evidence.csv"))
  .write_csv(inference$rank_gate,
             file.path(table_dir, "rank_interiority_diagnostic.csv"))

  interaction_path <- path_or_missing("interaction")
  z19_path <- path_or_missing("z19")
  sensitivity_path <- path_or_missing("sensitivity")
  status <- .status_table(
    q_summary = q_summary, heldout_state = heldout_state,
    optimization = optimization, numerical = numerical,
    completion = completion, information = information,
    inference = inference, interaction_file = interaction_path,
    z19_file = z19_path, sensitivity_file = sensitivity_path,
    common_rank = common_rank, q2_numerical = q2_numerical,
    sensitivity_bridge = sensitivity_bridge
  )
  status <- rbind(status, joint_extension_status,
                  completion_comparisons$status,
                  data.frame(
                    component =
                      "conditional-randomization completion/assignment test",
                    status = "protocol_unavailable_not_run",
                    note = conditional_randomization$reason[[1L]],
                    maintained_assumption_verified = FALSE,
                    stringsAsFactors = FALSE
                  ),
                  data.frame(
                    component =
                      "post-hoc party-by-candidate-gender mean diagnostic",
                    status = if (isTRUE(party_gender_mean_bridge$validated))
                      paste0(
                        "run_crossfitted_posthoc_diagnostic; ",
                        "formal_inference_withheld; ",
                        "outcome_blind_selection_false"
                      ) else "not_run",
                    note = if (isTRUE(party_gender_mean_bridge$validated))
                      paste(
                        "Frozen q=1 pooled and two-slope comparators reuse",
                        "respondent outer folds. The diagnostic was selected",
                        "after the mismatch was observed; it is not a maintained",
                        "model and does not modify the primary fit."
                      ) else paste(
                        "No validated party-gender diagnostic was supplied;",
                        "no descriptive or inferential claim is made."
                      ),
                    maintained_assumption_verified = FALSE,
                    stringsAsFactors = FALSE
                  ))
  .write_csv(status, file.path(table_dir, "assessment_component_status.csv"))

  claims <- data.frame(
    claim = c(
      "primary sample", "primary factor rank", "held-out predictive fit",
      "normal residual shape", "common residual covariance",
      "independent logit shocks", "noninformative completion",
      "exact design-based ordered-contrast benchmark", "regular intervals",
      "majority/sign-share statements", "off-support contests",
      "Male x prior-run interaction", "post-conjoint 19-Z sensitivity",
      "task-process alternative", "serial-shock alternative",
      "completion/sample sensitivity",
      "design-specific misspecification simulations",
      "full three-task response-pattern calibration",
      "prespecified task-pair joint calibration",
      "exact repeated-contrast calibration",
      "conditional-randomization completion/assignment test"
    ),
    manuscript_state = c(
      "report with sample-flow caveat", "q=1 fixed primary; q=0/q=2 sensitivity",
      heldout_state,
      if (isTRUE(sensitivity_bridge$misspecification$validated)) paste(
        "maintained; not verified; design-specific shape simulations run;",
        "empirical alternative-family refits not run"
      ) else "maintained; not verified",
      if (isTRUE(sensitivity_bridge$misspecification$validated)) paste(
        "maintained; not verified; party-varying covariance DGP simulation",
        "run; empirical covariance-by-Z refit not run"
      ) else "maintained; not verified",
      if (isTRUE(sensitivity_bridge$misspecification$validated)) paste(
        "maintained; not verified; serial-shock DGP simulation run;",
        "empirical serial-shock likelihood not run"
      ) else "maintained; not verified",
      "maintained; not verified",
      "withheld: protocol probabilities unavailable",
      if (isTRUE(inference$inference_available))
        "conditional only; review target-specific gates" else "withheld",
      "withheld unless linked regular CI excludes 1/2",
      paste(
        "conditional on advertised unrestricted support; fielded protocol not",
        "document-certified, and any off-support use must be labeled structural extrapolation"
      ),
      if (isTRUE(sensitivity_bridge$validated)) paste(
        "validated descriptive sensitivity supplied; formal inference withheld;",
        "fielded support for the augmented basis not document-verified"
      ) else if (file.exists(interaction_path)) "sensitivity artifact supplied" else
        "not run",
      if (isTRUE(sensitivity_bridge$validated)) paste(
        "validated descriptive sensitivity supplied; four post-conjoint",
        "variables remain excluded from the primary specification"
      ) else "not run",
      if (isTRUE(sensitivity_bridge$validated))
        "held-out order diagnostics run; task-varying structural refit not run" else
        "not run",
      if (isTRUE(sensitivity_bridge$validated))
        "held-out serial diagnostics run; explicit serial-shock likelihood not run" else
        "not run",
      if (isTRUE(sensitivity_bridge$validated)) paste(
        "validated 1,191-versus-1,249 descriptive sample sensitivity;",
        "noninformative completion not verified"
      ) else "not run",
      if (isTRUE(sensitivity_bridge$misspecification$validated)) paste(
        "validated design-specific simulated-data diagnostics supplied;",
        "formal coverage/materiality pass withheld and empirical",
        "alternative-family refits remain not run"
      ) else "not run",
      joint_extension_status$status[[1L]],
      joint_extension_status$status[[2L]],
      paste(joint_extension_status$status[[3L]],
            "(one exact repeated-contrast respondent in the realized data)"),
      "withheld: fielded randomization protocol and probabilities unavailable"
    ),
    evidence_does_not_establish_assumption = TRUE,
    stringsAsFactors = FALSE
  )
  claims <- rbind(
    claims,
    data.frame(
      claim = "party-by-candidate-gender mean diagnostic",
      manuscript_state = if (isTRUE(party_gender_mean_bridge$validated))
        paste(
          "validated respondent-cross-fitted post-hoc diagnostic supplied;",
          "formal inference, outcome-blind model-selection claims,",
          "maintained-model status, and materiality conclusions withheld"
        ) else "not run",
      evidence_does_not_establish_assumption = TRUE,
      stringsAsFactors = FALSE
    )
  )
  .write_csv(claims, file.path(table_dir, "manuscript_claims_ledger.csv"))

  artifact_paths <- c(prep = prep_path,
                      design_completion_audit = design_audit_path,
                      design_task_metadata = design_task_path,
                      completion_task_audit = completion_task_path,
                      assessment_extension_source = extension_source,
                      common_outer_rank_assessment = rank_common_path,
                      q2_numerical_orientation_assessment = rank_q2_path,
                      paths)
  if (isTRUE(sensitivity_bridge$validated)) {
    bridge_extra <- sensitivity_bridge$source_paths[
      setdiff(names(sensitivity_bridge$source_paths),
              c("structural", "interaction", "z19"))
    ]
    names(bridge_extra) <- paste0("sensitivity_bridge_", names(bridge_extra))
    artifact_paths <- c(artifact_paths, bridge_extra)
  }
  if (isTRUE(party_gender_mean_bridge$validated)) {
    party_bridge_paths <- party_gender_mean_bridge$source_paths
    names(party_bridge_paths) <- paste0(
      "party_gender_bridge_", names(party_bridge_paths)
    )
    artifact_paths <- c(artifact_paths, party_bridge_paths)
  }
  artifact_exists <- !is.na(artifact_paths) & file.exists(artifact_paths)
  input_manifest <- data.frame(
    artifact = names(artifact_paths), path = unname(artifact_paths),
    exists = artifact_exists,
    bytes = ifelse(artifact_exists, file.info(artifact_paths)$size, NA_real_),
    md5 = NA_character_, stringsAsFactors = FALSE
  )
  input_manifest$md5[artifact_exists] <-
    unname(tools::md5sum(artifact_paths[artifact_exists]))
  .write_csv(input_manifest, file.path(out_dir, "input_manifest.csv"))

  assessment <- list(
    schema_version = "sw2022-assessment-v1",
    application = prepared$application,
    profile = profile,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    estimand = prepared$estimand,
    sample = prepared$sample,
    coordinate_dictionary = prepared$coordinate_dictionary,
    design = list(status = design_status, amce = amce,
                  amce_by_group = rbind(amce_party, amce_gender),
                  amce_structural_parallel = amce_structural_parallel,
                  audit = design_audit,
                  exact_protocol_benchmark = NULL,
                  conditional_randomization = conditional_randomization),
    prediction = prediction_objects,
    prediction_joint_extensions = joint_extensions,
    prediction_joint_extension_status = joint_extension_status,
    prediction_joint_extension_error = joint_extension_error,
    prediction_assessment_provenance = list(
      fold_construction_verified = !is.null(prediction_objects) &&
        isTRUE(prediction_objects$score$verified_heldout) &&
        isTRUE(prediction_objects$calibration$verified_heldout),
      candidate_grid_outcome_blind = !isTRUE(grid_same_sample_adapted),
      grid_provenance = grid_provenance,
      interpretation = if (isTRUE(grid_same_sample_adapted)) paste(
        "Predictions are cross-fitted under the final grid, but the grid was",
        "adapted after a same-sample pilot; treat predictive summaries as",
        "diagnostic rather than clean end-to-end held-out evaluation."
      ) else "End-to-end held-out provenance was not downgraded by grid adaptation."
    ),
    prediction_error = prediction_error,
    serial_order = serial,
    completion = completion,
    completion_early_response_assignment = completion_comparisons,
    q_stability = q_summary,
    q_sensitivity = q_sensitivity,
    common_outer_rank_assessment = common_rank,
    q2_numerical_orientation_assessment = q2_numerical,
    optimization = optimization,
    numerical = numerical,
    local_information = information,
    profiles = profiles,
    profile_status = profile_status,
    sensitivity = sensitivity,
    sensitivity_bridge = sensitivity_bridge,
    sensitivity_table_copies = sensitivity_table_copies,
    party_gender_mean_bridge = party_gender_mean_bridge,
    party_gender_mean_table_copies = party_gender_mean_table_copies,
    qoi = qoi,
    inference = inference,
    inference_transforms = inference_transforms,
    inference_manifest = inference_manifest,
    inference_verification_status = inference_verification_status,
    quantity_tables = quantity_tables,
    application_qoi_tables = application_qoi,
    inference_tables = inference_tables,
    component_status = status,
    claims_ledger = claims,
    input_manifest = input_manifest,
    maintained_assumptions_verified = FALSE,
    disclaimer = paste(
      "These assessments can reveal poor fit or sensitivity. They cannot",
      "establish normality, common covariance, noninformative completion,",
      "or independent logit shocks. Missing artifacts never pass by default."
    )
  )
  final_party_gender_bridge <- .load_party_gender_mean_bridge(app, profile)
  same_party_gender_state <-
    identical(isTRUE(final_party_gender_bridge$validated),
              isTRUE(party_gender_mean_bridge$validated)) &&
    identical(isTRUE(final_party_gender_bridge$available),
              isTRUE(party_gender_mean_bridge$available))
  if (same_party_gender_state && isTRUE(party_gender_mean_bridge$validated)) {
    same_party_gender_state <-
      identical(final_party_gender_bridge$source_md5,
                party_gender_mean_bridge$source_md5) &&
      identical(final_party_gender_bridge$manifest$artifacts,
                party_gender_mean_bridge$manifest$artifacts) &&
      identical(final_party_gender_bridge$result$input_md5,
                party_gender_mean_bridge$result$input_md5) &&
      all(file.exists(party_gender_mean_table_copies$target)) &&
      identical(
        unname(tools::md5sum(party_gender_mean_table_copies$target)),
        unname(as.character(party_gender_mean_table_copies$target_md5))
      )
  }
  if (!same_party_gender_state) {
    stop(
      "Party-gender diagnostic state or provenance changed during ",
      "assessment. Discard this assessment and rerun after its producer ",
      "finishes.", call. = FALSE
    )
  }
  class(assessment) <- c("sw2022_application_assessment", "list")
  saveRDS(assessment, file.path(object_dir, "assessment_bundle.rds"), version = 3)
  capture.output(utils::sessionInfo(),
                 file = file.path(out_dir, "sessionInfo.txt"))

  cat(sprintf(
    paste0("Saha--Weeks assessment (%s): %s; AMCE rows=%d; ",
           "formal inference=%s.\n"),
    profile, heldout_state, nrow(amce), inference$status %||% "not_run"
  ))
  invisible(assessment)
}

if (sys.nframe() == 0L) .main()
