#!/usr/bin/env Rscript

## Read-only post-fit evidence extraction for the Saha--Weeks application.
##
## Run this only after 03_fit_models.R, 04_inference_qoi.R, and
## 05_assessment.R have completed for the requested profile.  The script never
## edits a fit or assessment artifact.  It reads each RDS through a before/after
## checksum gate and writes a normalized audit beneath
## results/evidence_audit/<profile>/.
##
## Examples from the package root:
##   applications/bin/Rscript45 applications/sw2022/R/07_extract_evidence.R \
##     --profile=smoke --require-complete=true
##   applications/bin/Rscript45 applications/sw2022/R/07_extract_evidence.R \
##     --profile=production --require-complete=true

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(arg)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

.parse_args <- function(x) {
  out <- list(profile = "production", require_complete = TRUE)
  for (arg in x) {
    if (!startsWith(arg, "--") || !grepl("=", arg, fixed = TRUE)) {
      stop("Arguments must have the form --name=value: ", arg,
           call. = FALSE)
    }
    pair <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", pair[[1L]], fixed = TRUE)
    if (!key %in% c("profile", "require_complete", "fit_dir",
                    "assessment_dir", "out_dir")) {
      stop("Unknown argument --", pair[[1L]], call. = FALSE)
    }
    out[[key]] <- paste(pair[-1L], collapse = "=")
  }
  out$require_complete <- tolower(as.character(out$require_complete)) %in%
    c("1", "true", "yes")
  out
}

.md5 <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

.read_stable_rds <- function(path, required = FALSE) {
  if (is.na(path) || !file.exists(path)) {
    if (required) stop("Required artifact is missing: ", path,
                       call. = FALSE)
    return(NULL)
  }
  before <- .md5(path)
  value <- readRDS(path)
  after <- .md5(path)
  if (!identical(before, after)) {
    stop("Artifact changed while it was being read: ", path,
         ". Wait for the producing stage to finish and rerun.", call. = FALSE)
  }
  attr(value, "sw_evidence_source_md5") <- before
  value
}

.load_misspecification_evidence <- function(fit_dir, profile) {
  directory <- file.path(fit_dir, "sensitivity_analysis", "misspecification")
  empty_status <- data.frame(
    check = "validated design-specific misspecification experiment",
    pass = FALSE, state = "not_run",
    detail = "misspecification artifact directory is absent",
    stringsAsFactors = FALSE
  )
  if (!dir.exists(directory)) {
    return(list(available = FALSE, validated = FALSE,
                status = empty_status, tables = list()))
  }
  paths <- c(
    manifest = file.path(directory, "manifest.rds"),
    results = file.path(directory, "misspecification_results.rds"),
    validation = file.path(directory, "misspecification_validation.rds")
  )
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Misspecification directory exists but lacks validated artifact(s): ",
      paste(missing, collapse = ", "),
      ". Rerun 08_run_misspecification_experiments.R and ",
      "09_validate_misspecification_artifacts.R.", call. = FALSE
    )
  }
  manifest <- .read_stable_rds(paths[["manifest"]], required = TRUE)
  results <- .read_stable_rds(paths[["results"]], required = TRUE)
  validation <- .read_stable_rds(paths[["validation"]], required = TRUE)
  schema_ok <-
    identical(manifest$schema_version,
              "sw2022-design-misspecification-manifest-v1") &&
    identical(results$schema_version,
              "sw2022-design-misspecification-results-v1") &&
    identical(validation$schema_version,
              "sw2022-misspecification-validation-v1") &&
    identical(manifest$profile, profile) &&
    identical(results$profile, profile) &&
    identical(validation$profile, profile) &&
    isTRUE(validation$passed) && is.data.frame(validation$checks) &&
    nrow(validation$checks) > 0L && all(validation$checks$pass)
  fail_closed_ok <-
    identical(manifest$primary_artifacts_modified, FALSE) &&
    identical(manifest$coverage_evaluated, FALSE) &&
    identical(manifest$materiality_pass_issued, FALSE) &&
    identical(manifest$maintained_assumptions_verified, FALSE) &&
    identical(results$primary_artifacts_modified, FALSE) &&
    identical(results$formal_inference_available, FALSE) &&
    identical(results$maintained_assumptions_verified, FALSE) &&
    identical(validation$formal_inference_available, FALSE) &&
    identical(validation$coverage_evaluated, FALSE) &&
    identical(validation$materiality_pass_issued, FALSE) &&
    identical(validation$maintained_assumptions_verified, FALSE)
  validation_hash_ok <- identical(
    as.character(validation$manifest_md5), as.character(.md5(paths[["manifest"]]))
  ) && identical(
    as.character(validation$results_md5), as.character(.md5(paths[["results"]]))
  )
  input_paths <- c(
    primary = validation$primary_fit_path,
    prepared = validation$prepared_path,
    config = validation$config_path
  )
  input_expected <- c(
    validation$primary_fit_md5, validation$prepared_md5,
    validation$config_md5
  )
  input_exists <- file.exists(input_paths)
  input_observed <- rep(NA_character_, length(input_paths))
  input_observed[input_exists] <- unname(tools::md5sum(input_paths[input_exists]))
  input_ok <- all(input_exists & input_observed == input_expected)
  artifact_paths <- file.path(directory, names(manifest$artifact_md5))
  artifact_exists <- file.exists(artifact_paths)
  artifact_observed <- rep(NA_character_, length(artifact_paths))
  artifact_observed[artifact_exists] <-
    unname(tools::md5sum(artifact_paths[artifact_exists]))
  artifact_ok <- length(artifact_paths) > 0L &&
    all(artifact_exists &
          artifact_observed == as.character(manifest$artifact_md5))
  structural <- results$structural_sensitivity
  structure_ok <- inherits(structural, "scmix_structural_sensitivity") &&
    !isTRUE(structural$complete) && !isTRUE(structural$substantive_pass) &&
    identical(results$coverage$coverage_evaluated, FALSE) &&
    identical(results$coverage$oracle_interval_substituted, FALSE)
  if (!all(c(schema_ok, fail_closed_ok, validation_hash_ok, input_ok,
             artifact_ok, structure_ok))) {
    stop(
      "Validated misspecification evidence failed a schema, hash, input, or ",
      "fail-closed gate. Rerun its runner and validator before extraction.",
      call. = FALSE
    )
  }
  status <- data.frame(
    check = c(
      "schema/profile and independent validation",
      "manifest and results hashes tied to validator",
      "manifested artifact hashes", "primary/prepared/config input hashes",
      "formal inference, coverage, and materiality withheld",
      "alternative-family empirical refits",
      "maintained assumptions"
    ),
    pass = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    state = c(
      rep("validated", 5L), "not_run", "not_verified"
    ),
    detail = c(
      paste(nrow(validation$checks), "validator checks passed for", profile),
      "current manifest and compact results match validator hashes",
      paste(length(artifact_paths), "manifested artifacts match"),
      "primary fit, prepared data, and frozen experiment config match",
      "design-specific simulation supplies no formal inferential or substantive pass",
      paste(
        "No empirical alternative likelihood was fitted or separately",
        "identified; simulated-data primary-model refits are not relabeled."
      ),
      paste(
        "Simulation can reveal sensitivity but cannot establish normality,",
        "common covariance, fixed scale, or shock independence."
      )
    ), stringsAsFactors = FALSE
  )
  empirical_refits <- data.frame(
    alternative = c("skewed", "symmetric bimodal", "heavy-tailed",
                    "party-varying covariance", "random scale",
                    "serial shock"),
    design_specific_simulation = TRUE,
    primary_normal_estimator_refit_to_simulated_data = TRUE,
    empirical_alternative_family_refit = FALSE,
    alternative_identification_established = FALSE,
    state = "not_run_empirical_alternative_refit",
    stringsAsFactors = FALSE
  )
  tables <- list(
    misspecification_ingestion_status = status,
    misspecification_structural_component_status = structural$status,
    misspecification_empirical_refit_distinction = empirical_refits,
    misspecification_qoi_bias_stability = results$qoi_summary,
    misspecification_replication_optimization = results$optimization,
    misspecification_dgp_calibration = results$calibration,
    misspecification_truth_resolution = results$truth_summary,
    misspecification_coverage_status = results$coverage
  )
  tables <- tables[vapply(tables, is.data.frame, logical(1L))]
  list(
    available = TRUE, validated = TRUE, directory = directory,
    paths = paths, source_md5 = vapply(paths, .md5, character(1L)),
    status = status, tables = tables,
    profile = profile,
    frozen_replications_per_scenario =
      results$config$profiles[[profile]]$replications,
    frozen_total_refits = results$config$profiles[[profile]]$replications *
      length(results$config$scenarios),
    minimum_defensible_replications_per_scenario =
      results$config$profiles[[profile]]$minimum_defensible_replications,
    requested_replications_per_scenario = results$requested_replications,
    requested_total_refits = results$requested_replications *
      length(results$requested_scenarios),
    completed_total_refits = nrow(results$optimization),
    scenarios = unique(as.character(results$optimization$scenario)),
    distinction = validation$distinction,
    formal_inference_available = FALSE,
    coverage_evaluated = FALSE,
    maintained_assumptions_verified = FALSE
  )
}

.party_gender_mean_table_schemas <- function() {
  list(
    sequence_score_summary = c(
      "model", "party", "mean_complete_sequence_log_score",
      "respondent_se", "n_respondents"
    ),
    sequence_score_paired_differences = c(
      "comparison", "party", "mean_difference", "respondent_se",
      "n_respondents", "interpretation"
    ),
    party_calibration = c(
      "model", "party", "observed_rate", "predicted_rate",
      "calibration_gap", "respondent_se_gap", "brier_score",
      "marginal_task_log_score", "n_respondents", "n_tasks"
    ),
    party_amce_projection = c(
      "party", "contrast", "observed_amce", "observed_cluster_se",
      "primary_dnn_oof_projection", "pooled_mean_q1_oof_projection",
      "targeted_q1_oof_projection", "primary_gap", "targeted_gap",
      "comparison_status"
    ),
    party_gender_structural = c(
      "model", "party", "female_vs_male_latent_preference",
      "female_vs_male_position_neutral_choice_probability",
      "position_neutral_probability_minus_half",
      "formal_inference_available"
    ),
    optimization = c(
      "model", "fit_role", "start", "selected", "objective",
      "gradient_norm", "relative_change", "criterion_tolerance_met",
      "stationarity_met", "bound_activity", "optimization_gate_pass",
      "epochs", "stop_reason", "failure_reasons"
    ),
    diagnostic_cause_ledger = c(
      "check", "status", "value", "interpretation"
    ),
    reporting_gates = c(
      "gate", "pass", "required_for_descriptive_use", "status"
    ),
    q0_scope_check = c(
      "comparison", "mean_difference", "respondent_se", "n_respondents",
      "role"
    )
  )
}

.table_csv_md5 <- function(x) {
  path <- tempfile("sw-party-gender-table-", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE),
    path, row.names = FALSE, na = ""
  )
  .md5(path)
}

.load_party_gender_mean_evidence <- function(app, profile) {
  directory <- file.path(
    app, "results", "party_gender_mean_sensitivity", profile
  )
  empty_status <- data.frame(
    check = "party-by-candidate-gender mean diagnostic",
    pass = FALSE,
    state = "not_run",
    detail = paste(
      "Post-hoc diagnostic directory is absent; no formal inference,",
      "outcome-blind selection claim, or maintained-model claim is available."
    ),
    stringsAsFactors = FALSE
  )
  if (!dir.exists(directory)) {
    return(list(
      available = FALSE, validated = FALSE, directory = directory,
      directory_was_absent = TRUE, status = empty_status,
      artifact_audit = data.frame(), input_audit = data.frame(),
      table_csv_agreement = data.frame(),
      tables = list(party_gender_mean_ingestion_status = empty_status),
      formal_inference_available = FALSE,
      diagnostic_selection_outcome_blind = FALSE,
      outcome_blind = FALSE, maintained_model = FALSE,
      primary_artifacts_modified = FALSE
    ))
  }

  paths <- c(
    result = file.path(directory, "party_gender_mean_sensitivity.rds"),
    manifest = file.path(directory, "manifest.rds")
  )
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Party-gender diagnostic directory exists but lacks artifact(s): ",
      paste(missing, collapse = ", "),
      ". Rerun 10_run_party_gender_mean_sensitivity.R for this profile.",
      call. = FALSE
    )
  }
  result <- .read_stable_rds(paths[["result"]], required = TRUE)
  manifest <- .read_stable_rds(paths[["manifest"]], required = TRUE)
  expected_schema <- "sw2022-party-gender-mean-diagnostic-v1"
  schemas <- .party_gender_mean_table_schemas()
  expected_table_names <- names(schemas)
  expected_artifact_names <- c(
    "party_gender_mean_sensitivity.rds",
    paste0(expected_table_names, ".csv")
  )
  expected_input_names <- c(
    "prepared", "primary_full", "primary_assembled", "config", "runner",
    "mixed_likelihood_source", "computation_source", "prediction_source"
  )
  project <- normalizePath(file.path(app, "../.."), mustWork = TRUE)
  expected_input_paths <- c(
    prepared = file.path(app, "results", "prep_analysis_data.rds"),
    primary_full = file.path(
      app, "results", "mixed_logit", profile, "fit_primary_full.rds"
    ),
    primary_assembled = file.path(
      app, "results", "mixed_logit", profile, "fit_primary_assembled.rds"
    ),
    config = file.path(
      app, "sensitivity", "party_gender_mean_config.R"
    ),
    runner = file.path(
      app, "sensitivity", "10_run_party_gender_mean_sensitivity.R"
    ),
    mixed_likelihood_source = file.path(project, "R", "mixed-likelihood.R"),
    computation_source = file.path(project, "R", "paperps-computation.R"),
    prediction_source = file.path(
      project, "R", "paperps-assessment-predictions.R"
    )
  )

  result_inputs <- result$input_paths
  manifest_inputs <- manifest$input_paths
  result_input_md5 <- result$input_md5
  manifest_input_md5 <- manifest$input_md5
  input_contract_ok <-
    is.character(result_inputs) && is.character(manifest_inputs) &&
    is.character(result_input_md5) && is.character(manifest_input_md5) &&
    identical(names(result_inputs), expected_input_names) &&
    identical(names(manifest_inputs), expected_input_names) &&
    identical(names(result_input_md5), expected_input_names) &&
    identical(names(manifest_input_md5), expected_input_names) &&
    identical(as.character(result_inputs), as.character(manifest_inputs)) &&
    identical(as.character(result_input_md5),
              as.character(manifest_input_md5)) &&
    identical(
      normalizePath(result_inputs, mustWork = FALSE),
      normalizePath(expected_input_paths, mustWork = FALSE)
    ) &&
    all(grepl("^[0-9a-f]{32}$", result_input_md5))

  artifact_hashes <- manifest$artifacts
  artifact_contract_ok <-
    is.character(artifact_hashes) &&
    identical(names(artifact_hashes), expected_artifact_names) &&
    length(unique(names(artifact_hashes))) == length(artifact_hashes) &&
    all(basename(names(artifact_hashes)) == names(artifact_hashes)) &&
    all(grepl("^[0-9a-f]{32}$", artifact_hashes))
  if (!input_contract_ok || !artifact_contract_ok) {
    stop(
      "Party-gender diagnostic failed its named input/artifact contract. ",
      "Rerun the frozen diagnostic before evidence extraction.",
      call. = FALSE
    )
  }

  input_exists <- file.exists(result_inputs)
  input_observed <- rep(NA_character_, length(result_inputs))
  input_observed[input_exists] <-
    unname(tools::md5sum(result_inputs[input_exists]))
  input_audit <- data.frame(
    input = names(result_inputs), path = unname(result_inputs),
    expected_md5 = unname(result_input_md5),
    current_md5 = input_observed, exists = input_exists,
    match = input_exists & input_observed == unname(result_input_md5),
    stringsAsFactors = FALSE
  )
  artifact_paths <- file.path(directory, names(artifact_hashes))
  artifact_exists <- file.exists(artifact_paths)
  artifact_observed <- rep(NA_character_, length(artifact_paths))
  artifact_observed[artifact_exists] <-
    unname(tools::md5sum(artifact_paths[artifact_exists]))
  artifact_audit <- data.frame(
    artifact = names(artifact_hashes), path = artifact_paths,
    expected_md5 = unname(artifact_hashes),
    current_md5 = artifact_observed, exists = artifact_exists,
    match = artifact_exists & artifact_observed == unname(artifact_hashes),
    stringsAsFactors = FALSE
  )

  table_contract_ok <-
    is.list(result$tables) &&
    identical(names(result$tables), expected_table_names) &&
    all(vapply(expected_table_names, function(nm) {
      is.data.frame(result$tables[[nm]]) &&
        nrow(result$tables[[nm]]) > 0L &&
        identical(names(result$tables[[nm]]), schemas[[nm]])
    }, logical(1L)))
  if (!table_contract_ok) {
    stop(
      "Party-gender diagnostic result has a missing or stale table schema.",
      call. = FALSE
    )
  }
  table_csv_agreement <- data.frame(
    table = expected_table_names,
    rds_table_csv_md5 = vapply(
      result$tables[expected_table_names], .table_csv_md5, character(1L)
    ),
    manifested_csv_md5 = unname(
      artifact_hashes[paste0(expected_table_names, ".csv")]
    ),
    stringsAsFactors = FALSE
  )
  table_csv_agreement$match <-
    table_csv_agreement$rds_table_csv_md5 ==
    table_csv_agreement$manifested_csv_md5

  gates <- result$tables$reporting_gates
  required_gate <- as.logical(gates$required_for_descriptive_use)
  required_gate_ok <- any(required_gate) &&
    all(!is.na(gates$pass[required_gate]) & gates$pass[required_gate])
  formal_gate <- gates[gates$gate == "formal inference enabled", , drop = FALSE]
  outcome_gate <- gates[
    gates$gate == "end-to-end outcome-blind model assessment", , drop = FALSE
  ]
  withheld_gate_ok <-
    nrow(formal_gate) == 1L && identical(formal_gate$pass[[1L]], FALSE) &&
    !isTRUE(formal_gate$required_for_descriptive_use[[1L]]) &&
    identical(as.character(formal_gate$status[[1L]]), "withheld") &&
    nrow(outcome_gate) == 1L && identical(outcome_gate$pass[[1L]], FALSE) &&
    !isTRUE(outcome_gate$required_for_descriptive_use[[1L]]) &&
    identical(
      as.character(outcome_gate$status[[1L]]),
      "withheld_posthoc_selection"
    )
  cause <- result$tables$diagnostic_cause_ledger
  formal_cause <- cause[cause$check == "formal inference", , drop = FALSE]
  outcome_cause <- cause[
    cause$check == "outcome-blind diagnostic selection", , drop = FALSE
  ]
  cause_ledger_ok <-
    nrow(formal_cause) == 1L &&
    identical(as.character(formal_cause$status[[1L]]), "withheld") &&
    identical(toupper(as.character(formal_cause$value[[1L]])), "FALSE") &&
    nrow(outcome_cause) == 1L &&
    identical(
      as.character(outcome_cause$status[[1L]]), "failed_by_design"
    ) &&
    identical(toupper(as.character(outcome_cause$value[[1L]])), "FALSE")

  schema_flag_ok <-
    identical(result$schema_version, expected_schema) &&
    identical(manifest$schema_version, paste0(expected_schema, "-manifest")) &&
    identical(result$profile, profile) && identical(manifest$profile, profile) &&
    is.list(result$configuration) &&
    identical(result$configuration$schema_version, expected_schema) &&
    identical(
      result$configuration$created_after_primary_mismatch_was_observed, TRUE
    ) &&
    identical(result$configuration$outcome_blind, FALSE) &&
    identical(result$configuration$formal_inference_available, FALSE) &&
    identical(result$configuration$maintained_model, FALSE) &&
    identical(result$configuration$posterior_summaries_used, FALSE) &&
    identical(result$configuration$primary_artifacts_modified, FALSE) &&
    identical(result$diagnostic_selection_outcome_blind, FALSE) &&
    identical(result$formal_inference_available, FALSE) &&
    identical(result$maintained_model, FALSE) &&
    identical(result$posterior_summaries_used, FALSE) &&
    identical(result$primary_artifacts_modified, FALSE) &&
    identical(result$inherited_primary_outer_folds, TRUE) &&
    identical(result$fold_construction_verified, TRUE) &&
    identical(manifest$primary_artifacts_unchanged, TRUE) &&
    identical(manifest$descriptive_use_gate, TRUE) &&
    identical(manifest$formal_inference_available, FALSE) &&
    identical(manifest$maintained_model, FALSE) &&
    identical(manifest$outcome_blind, FALSE) &&
    identical(manifest$primary_artifacts_modified, FALSE) &&
    identical(as.integer(result$sample$n_respondents), 1191L) &&
    identical(as.integer(result$sample$n_tasks), 3573L) &&
    identical(as.integer(result$sample$p), 13L) &&
    is.logical(
      result$tables$party_gender_structural$formal_inference_available
    ) &&
    length(result$tables$party_gender_structural$formal_inference_available) >
      0L &&
    !anyNA(result$tables$party_gender_structural$formal_inference_available) &&
    all(!result$tables$party_gender_structural$formal_inference_available)
  all_ok <- schema_flag_ok && required_gate_ok && withheld_gate_ok &&
    cause_ledger_ok && all(input_audit$match) && all(artifact_audit$match) &&
    all(table_csv_agreement$match)
  if (!isTRUE(all_ok)) {
    stop(
      "Party-gender evidence failed a schema, post-hoc labeling, hash, ",
      "descriptive-use, or RDS/CSV agreement gate. Rerun the diagnostic ",
      "before extraction.", call. = FALSE
    )
  }

  status <- data.frame(
    check = c(
      "result and manifest schemas/profile",
      "named input hashes", "manifested artifact hashes",
      "RDS tables and manifested CSVs agree",
      "respondent-fold isolation and descriptive-use gates",
      "formal inference", "outcome-blind diagnostic selection",
      "maintained model"
    ),
    pass = c(rep(TRUE, 5L), FALSE, FALSE, FALSE),
    state = c(
      rep("validated", 5L), "withheld", "failed_by_design",
      "not_maintained"
    ),
    detail = c(
      paste("validated", expected_schema, "for", profile),
      paste(nrow(input_audit), "current inputs match result and manifest"),
      paste(nrow(artifact_audit), "manifested artifacts match"),
      paste(nrow(table_csv_agreement), "table exports match RDS payloads"),
      "outer-fold isolation verified; descriptive-use gate passed",
      "post-hoc descriptive diagnostic supplies no formal inference",
      "diagnostic scope was selected after observing the primary mismatch",
      "targeted q=1 comparator does not replace the maintained estimator"
    ), stringsAsFactors = FALSE
  )
  surfaced <- stats::setNames(
    result$tables,
    paste0("party_gender_mean_", names(result$tables))
  )
  surfaced <- c(
    list(
      party_gender_mean_ingestion_status = status,
      party_gender_mean_input_hashes = input_audit,
      party_gender_mean_artifact_hashes = artifact_audit,
      party_gender_mean_rds_csv_agreement = table_csv_agreement
    ),
    surfaced
  )
  source_md5 <- vapply(paths, .md5, character(1L))
  list(
    available = TRUE, validated = TRUE, directory = directory,
    directory_was_absent = FALSE, paths = paths, source_md5 = source_md5,
    status = status, artifact_audit = artifact_audit,
    input_audit = input_audit, table_csv_agreement = table_csv_agreement,
    tables = surfaced, profile = profile,
    descriptive_use_gate = TRUE, post_hoc = TRUE,
    formal_inference_available = FALSE,
    diagnostic_selection_outcome_blind = FALSE,
    outcome_blind = FALSE, maintained_model = FALSE,
    primary_artifacts_modified = FALSE
  )
}

.atomic_save_rds <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.atomic_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.atomic_write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(x, tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.relative <- function(path, root) {
  if (is.na(path)) return(NA_character_)
  root <- paste0(normalizePath(root, mustWork = TRUE), "/")
  normalized <- normalizePath(path, mustWork = FALSE)
  if (startsWith(normalized, root)) {
    substring(normalized, nchar(root) + 1L)
  } else normalized
}

.git_revision <- function(root) {
  ans <- tryCatch(
    system2("git", c("-C", root, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(ans)) ans[[1L]] else NA_character_
}

.empty <- function(...) {
  specification <- list(...)
  as.data.frame(lapply(specification, function(x) x[FALSE]),
                stringsAsFactors = FALSE, check.names = FALSE)
}

.csv_safe <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  list_columns <- vapply(x, is.list, logical(1L))
  for (nm in names(x)[list_columns]) {
    x[[nm]] <- vapply(x[[nm]], function(z) paste(z, collapse = ":"),
                      character(1L))
  }
  x
}

.candidate_rows <- function(tuning, scope, outer_fold = NA_integer_,
                            selection_gate = NA) {
  if (is.null(tuning) || !is.data.frame(tuning$candidates)) return(data.frame())
  if (is.list(selection_gate) && !is.null(selection_gate$pass)) {
    selection_gate <- selection_gate$pass
  }
  if (length(selection_gate) != 1L) selection_gate <- NA
  out <- .csv_safe(tuning$candidates)
  out <- cbind(
    data.frame(scope = scope, outer_fold = outer_fold,
               selection_gate = as.logical(selection_gate),
               stringsAsFactors = FALSE),
    out
  )
  selected <- suppressWarnings(as.integer(tuning$selected_index %||% NA_integer_))
  out$selected_index_recorded <- seq_len(nrow(out)) == selected
  out
}

.nested_cv_tables <- function(full, nested, assembled) {
  candidate <- list(.candidate_rows(
    full, "full_sample_inner_cv", NA_integer_,
    full$candidate_selection_gate %||% NA
  ))
  tuning <- nested$tuning %||% list()
  outer_labels <- nested$outer_folds %||% as.character(seq_along(tuning))
  selection_gate <- nested$candidate_selection_gate_by_outer_fold %||%
    rep(NA, length(tuning))
  for (j in seq_along(tuning)) {
    candidate[[length(candidate) + 1L]] <- .candidate_rows(
      tuning[[j]], "outer_training_inner_cv",
      suppressWarnings(as.integer(outer_labels[[j]])), selection_gate[[j]]
    )
  }
  candidate <- candidate[vapply(candidate, nrow, integer(1L)) > 0L]
  candidate <- if (length(candidate)) do.call(rbind, candidate) else data.frame()
  rownames(candidate) <- NULL

  selected_specs <- assembled$selected_specifications %||% list()
  fold_rows <- lapply(seq_along(tuning), function(j) {
    z <- tuning[[j]]
    spec <- selected_specs[[j]] %||% z$selected %||% list()
    selected_index <- suppressWarnings(as.integer(z$selected_index %||% NA_integer_))
    selected_score <- if (is.data.frame(z$candidates) &&
                          is.finite(selected_index) && selected_index >= 1L &&
                          selected_index <= nrow(z$candidates)) {
      z$candidates$cv_sequence_log_score[[selected_index]] %||% NA_real_
    } else NA_real_
    data.frame(
      outer_fold = suppressWarnings(as.integer(outer_labels[[j]])),
      candidate = as.character(spec$name %||% NA_character_),
      hidden = paste(spec$hidden %||% NA, collapse = ":"),
      weight_decay = as.numeric(spec$weight_decay %||% NA_real_),
      q = as.integer(spec$q %||% NA_integer_),
      integration = as.character(spec$integration %||% NA_character_),
      n_nodes = as.integer(spec$n_nodes %||% NA_integer_),
      inner_cv_sequence_log_score = as.numeric(selected_score),
      candidate_selection_gate_pass = as.logical(selection_gate[[j]]),
      selected_refit_optimization_gate_pass = as.logical(
        assembled$optimization$gate_by_fold[[j]] %||% NA
      ),
      compact_bound_gate_pass = as.logical(
        assembled$optimization$compact_bound_gate_by_fold[[j]] %||% NA
      ),
      stringsAsFactors = FALSE
    )
  })
  selected <- if (length(fold_rows)) do.call(rbind, fold_rows) else data.frame()
  list(candidates = candidate, selected = selected)
}

.optimization_tables <- function(full_audit, nested_audit) {
  audits <- list(full_sample = full_audit, nested_outer_folds = nested_audit)
  summaries <- starts <- gates <- list()
  for (nm in names(audits)) {
    x <- audits[[nm]]
    if (!inherits(x, "scmix_optimization_audit")) next
    if (is.data.frame(x$summary)) {
      summaries[[length(summaries) + 1L]] <- cbind(
        audit_scope = nm, .csv_safe(x$summary), stringsAsFactors = FALSE
      )
    }
    if (is.data.frame(x$starts)) {
      starts[[length(starts) + 1L]] <- cbind(
        audit_scope = nm, .csv_safe(x$starts), stringsAsFactors = FALSE
      )
    }
    gates[[length(gates) + 1L]] <- data.frame(
      audit_scope = nm,
      all_selected_tolerances_met = isTRUE(x$all_selected_tolerances_met),
      all_objectives_finite = isTRUE(x$all_objectives_finite),
      all_computational_gates_pass = isTRUE(x$all_computational_gates_pass),
      all_bound_diagnostics_complete = isTRUE(x$all_bound_diagnostics_complete),
      any_bound_activity = isTRUE(x$any_bound_activity),
      signature_match = isTRUE(x$signature_match),
      global_optimality_gap_known = isTRUE(x$global_optimality_gap_known),
      disclaimer = as.character(x$disclaimer %||% ""),
      stringsAsFactors = FALSE
    )
  }
  list(
    summary = if (length(summaries)) do.call(rbind, summaries) else data.frame(),
    starts = if (length(starts)) do.call(rbind, starts) else data.frame(),
    gates = if (length(gates)) do.call(rbind, gates) else data.frame()
  )
}

.heldout_tables <- function(assessment, assembled) {
  prediction <- assessment$prediction
  if (!is.list(prediction) || !is.list(prediction$score)) {
    return(list(score = data.frame(), by_fold = data.frame(),
                marginal = data.frame(), joint = data.frame(),
                calibration_summary = data.frame()))
  }
  score <- prediction$score
  score_table <- data.frame(
    model = names(score$estimate) %||% paste0("model_", seq_along(score$estimate)),
    mean_complete_sequence_log_score = as.numeric(score$estimate),
    respondent_se = as.numeric(score$se),
    unit = as.character(score$unit %||% "complete respondent sequence"),
    out_of_fold = isTRUE(score$out_of_fold),
    training_only_tuning_verified = isTRUE(score$training_only_tuning),
    verified_heldout = isTRUE(score$verified_heldout),
    status = as.character(score$status %||% "undocumented"),
    provenance = as.character(score$provenance %||% ""),
    stringsAsFactors = FALSE
  )

  by_fold <- data.frame()
  loglik <- score$loglik
  score_rid <- as.character(score$respondent_id %||% character())
  if (!is.null(loglik) && length(score_rid) && !is.null(assembled)) {
    loglik <- as.matrix(loglik)
    rid <- unique(as.character(assembled$respondent_id))
    first <- match(rid, as.character(assembled$respondent_id))
    fold <- as.integer(assembled$fold_id[first])
    fold <- fold[match(score_rid, rid)]
    if (nrow(loglik) == length(score_rid) && !anyNA(fold)) {
      fold_rows <- list()
      for (m in seq_len(ncol(loglik))) {
        for (k in sort(unique(fold))) {
          values <- loglik[fold == k, m]
          fold_rows[[length(fold_rows) + 1L]] <- data.frame(
            model = colnames(loglik)[[m]] %||% paste0("model_", m),
            outer_fold = k, n_respondents = length(values),
            mean_complete_sequence_log_score = mean(values),
            respondent_se = stats::sd(values) / sqrt(length(values)),
            verified_heldout = isTRUE(score$verified_heldout),
            stringsAsFactors = FALSE
          )
        }
      }
      by_fold <- do.call(rbind, fold_rows)
    }
  }

  calibration <- prediction$calibration %||% list()
  marginal <- calibration$marginal %||% data.frame()
  joint <- calibration$joint %||% data.frame()
  summarize_calibration <- function(x, family) {
    if (!is.data.frame(x) || !nrow(x) ||
        !all(c("type", "gap") %in% names(x))) return(data.frame())
    rows <- lapply(split(x, x$type, drop = TRUE), function(z) {
      ok <- is.finite(z$gap)
      weight <- if ("n_rows" %in% names(z)) as.numeric(z$n_rows) else
        rep(1, nrow(z))
      ok_weight <- ok & is.finite(weight) & weight > 0
      data.frame(
        family = family, type = as.character(z$type[[1L]]),
        n_strata = nrow(z), finite_gap_strata = sum(ok),
        sum_stratum_rows = sum(weight[is.finite(weight)], na.rm = TRUE),
        weighted_mean_gap = if (any(ok_weight))
          stats::weighted.mean(z$gap[ok_weight], weight[ok_weight]) else NA_real_,
        weighted_rmse_gap = if (any(ok_weight))
          sqrt(stats::weighted.mean(z$gap[ok_weight]^2, weight[ok_weight])) else
          NA_real_,
        max_absolute_gap = if (any(ok)) max(abs(z$gap[ok])) else NA_real_,
        verified_heldout = isTRUE(calibration$verified_heldout),
        status = as.character(calibration$status %||% "undocumented"),
        interpretation = paste(
          "Descriptive aggregation across reported strata; strata can overlap,",
          "so summed row counts are not a sample size."
        ), stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
  calibration_summary <- rbind(
    summarize_calibration(marginal, "marginal"),
    summarize_calibration(joint, "joint")
  )
  list(score = score_table, by_fold = by_fold, marginal = marginal,
       joint = joint, calibration_summary = calibration_summary)
}

.qoi_tables <- function(assessment) {
  plugin <- assessment$quantity_tables$estimates %||% data.frame()
  gates <- assessment$quantity_tables$gates %||% data.frame()
  if (is.data.frame(plugin) && nrow(plugin) && is.data.frame(gates) &&
      nrow(gates) && "quantity_id" %in% names(plugin) &&
      "quantity_id" %in% names(gates)) {
    keep <- setdiff(names(gates), c("quantity", "quantity_id"))
    plugin <- merge(plugin, gates[c("quantity_id", keep)],
                    by = "quantity_id", all.x = TRUE, sort = FALSE)
  }
  targets <- assessment$inference_tables$targets %||% data.frame()
  transforms <- assessment$inference_tables$transforms %||% data.frame()
  catalog <- data.frame(
    evidence = c("structural plug-in rows", "primitive one-step targets",
                 "transformed one-step targets", "quantity-level gates"),
    rows = c(nrow(plugin), nrow(targets), nrow(transforms), nrow(gates)),
    point_estimates_available = c(
      is.data.frame(plugin) && nrow(plugin) > 0L && all(is.finite(plugin$estimate)),
      is.data.frame(targets) && nrow(targets) > 0L &&
        all(is.finite(targets$one_step_estimate)),
      is.data.frame(transforms) && nrow(transforms) > 0L &&
        all(is.finite(transforms$estimate)),
      NA
    ),
    formal_inference_available = c(
      isTRUE(assessment$inference$inference_available),
      isTRUE(assessment$inference$inference_available) &&
        all(targets$target_inference_available),
      isTRUE(assessment$inference$inference_available) &&
        all(transforms$target_inference_available),
      NA
    ), stringsAsFactors = FALSE
  )
  list(plugin = plugin, targets = targets, transforms = transforms,
       gates = gates, catalog = catalog)
}

.numerical_tables <- function(x) {
  if (inherits(x, "scmix_integration_refinement")) {
    return(list(
      status = data.frame(
        artifact_class = class(x)[[1L]], refit_count = x$refit_count %||% NA,
        gate_pass = isTRUE(x$gate$pass),
        signature_match = isTRUE(x$signature_match),
        state = as.character(x$gate$status %||% "undocumented"),
        disclaimer = as.character(x$disclaimer %||% ""),
        stringsAsFactors = FALSE
      ),
      settings = x$settings %||% data.frame(),
      checks = x$checks %||% data.frame(),
      comparison = x$gate$comparison %||% data.frame()
    ))
  }
  if (is.null(x)) {
    state <- "not_run"
    class_name <- NA_character_
  } else {
    state <- as.character(x$message %||% x$reason %||% "failed_or_unverified")
    class_name <- class(x)[[1L]]
  }
  list(
    status = data.frame(
      artifact_class = class_name, refit_count = NA_integer_,
      gate_pass = FALSE, signature_match = FALSE, state = state,
      disclaimer = "No passing fresh-refit numerical artifact is available.",
      stringsAsFactors = FALSE
    ),
    settings = data.frame(), checks = data.frame(), comparison = data.frame()
  )
}

.signature_rows <- function(full, nested, assembled, qoi, inference,
                            numerical, assessment) {
  value <- function(x) as.character(x$analysis_signature %||% NA_character_)
  rows <- data.frame(
    family = c("full_sample", "full_sample", "nested_fold",
               "nested_fold", "nested_fold", "nested_fold",
               "nested_fold", "nested_fold"),
    artifact = c("fit_primary_full", "qoi_plugin", "fit_primary_nested",
                 "fit_primary_assembled", "inference_diagnostic",
                 "integration_refinement", "prediction_score",
                 "prediction_calibration"),
    analysis_signature = c(
      value(full), value(qoi), value(nested), value(assembled), value(inference),
      value(numerical), value(assessment$prediction$score),
      value(assessment$prediction$calibration)
    ),
    signature_required = c(
      TRUE, TRUE, TRUE, TRUE, TRUE,
      inherits(numerical, "scmix_integration_refinement"), FALSE, FALSE
    ), stringsAsFactors = FALSE
  )
  rows$signature_present <- !is.na(rows$analysis_signature) &
    nzchar(rows$analysis_signature)
  rows$family_signature_match <- ave(
    seq_len(nrow(rows)), rows$family,
    FUN = function(ii) {
      observed <- unique(rows$analysis_signature[ii][rows$signature_present[ii]])
      length(observed) == 1L
    }
  ) > 0
  rows
}

.provenance_checks <- function(paths, fit_manifest, assessment) {
  observed <- data.frame(
    artifact = names(paths), path = unname(paths),
    exists = !is.na(paths) & file.exists(paths),
    bytes = NA_real_, current_md5 = NA_character_,
    fit_manifest_md5 = NA_character_, assessment_manifest_md5 = NA_character_,
    stringsAsFactors = FALSE
  )
  observed$bytes[observed$exists] <- file.info(paths[observed$exists])$size
  observed$current_md5[observed$exists] <-
    unname(tools::md5sum(paths[observed$exists]))

  fit_hash <- fit_manifest$artifacts %||% character()
  if (length(fit_hash)) {
    ## 03_fit_models.R records every RDS present in its output directory, but
    ## only fit-stage artifacts are authoritative entries in that manifest.
    ## In particular, the manifest can contain its own previous checksum or a
    ## previous 04_inference_qoi.R artifact when a stage is resumed.
    authoritative <- grepl(
      "^(fit_primary_|fit_q[0-9]+_|optimization_|q_sensitivity\\.rds$)",
      names(fit_hash)
    )
    fit_hash <- fit_hash[authoritative]
    hit <- match(basename(observed$path), names(fit_hash))
    observed$fit_manifest_md5[!is.na(hit)] <- unname(fit_hash[hit[!is.na(hit)]])
  }
  assessment_input <- assessment$input_manifest
  if (is.data.frame(assessment_input) &&
      all(c("path", "md5") %in% names(assessment_input))) {
    hit <- match(normalizePath(observed$path, mustWork = FALSE),
                 normalizePath(assessment_input$path, mustWork = FALSE))
    observed$assessment_manifest_md5[!is.na(hit)] <-
      assessment_input$md5[hit[!is.na(hit)]]
  }
  observed$fit_manifest_match <- ifelse(
    is.na(observed$fit_manifest_md5), NA,
    observed$current_md5 == observed$fit_manifest_md5
  )
  observed$assessment_manifest_match <- ifelse(
    is.na(observed$assessment_manifest_md5), NA,
    observed$current_md5 == observed$assessment_manifest_md5
  )
  observed$fit_manifest_check <- ifelse(
    is.na(observed$fit_manifest_match), "not_recorded",
    ifelse(observed$fit_manifest_match, "match", "mismatch")
  )
  observed$assessment_manifest_check <- ifelse(
    is.na(observed$assessment_manifest_match), "not_recorded",
    ifelse(observed$assessment_manifest_match, "match", "mismatch")
  )
  observed
}

.evidence_ledger <- function(profile, paths, assessment, nested_tables,
                             optimization, heldout, qoi, numerical,
                             signatures) {
  exists <- function(nm) !is.na(paths[[nm]]) && file.exists(paths[[nm]])
  opt_pass <- nrow(optimization$gates) >= 2L &&
    all(optimization$gates$all_computational_gates_pass) &&
    all(optimization$gates$all_bound_diagnostics_complete) &&
    all(optimization$gates$all_objectives_finite)
  nested_pass <- nrow(nested_tables$selected) > 0L &&
    all(nested_tables$selected$candidate_selection_gate_pass) &&
    all(nested_tables$selected$selected_refit_optimization_gate_pass) &&
    all(nested_tables$selected$compact_bound_gate_pass)
  rank_gate <- assessment$inference$rank_gate %||% data.frame()
  rank_pass <- is.data.frame(rank_gate) && nrow(rank_gate) > 0L &&
    "pass" %in% names(rank_gate) && all(rank_gate$pass)
  point_ready <- exists("qoi_plugin") && nrow(qoi$plugin) > 0L &&
    all(is.finite(qoi$plugin$estimate))
  heldout_run <- nrow(heldout$score) > 0L
  heldout_verified <- heldout_run && all(heldout$score$verified_heldout)
  numerical_pass <- nrow(numerical$status) == 1L &&
    isTRUE(numerical$status$gate_pass) &&
    isTRUE(numerical$status$signature_match)
  inference_ready <- isTRUE(assessment$inference$inference_available)
  signature_pass <- nrow(signatures) > 0L &&
    all(signatures$signature_present | !signatures$signature_required) &&
    all(signatures$family_signature_match)

  component <- assessment$component_status %||% data.frame()
  component_state <- function(pattern, default = "not_run") {
    if (!is.data.frame(component) || !nrow(component)) return(default)
    hit <- grep(pattern, component$component, ignore.case = TRUE)
    if (length(hit)) paste(unique(component$status[hit]), collapse = "; ") else
      default
  }
  rows <- list(
    c("profile_scope", "analysis profile",
      if (identical(profile, "production")) "production" else
        "diagnostic_profile_not_reportable",
      if (identical(profile, "production"))
        "Eligible to supply application evidence subject to every downstream gate." else
        "Smoke and pilot values test the workflow only and must never enter Section 5.1."),
    c("sample", "analysis sample and estimand", "ready_descriptive",
      "Report the frozen 1,191-respondent complete-case estimand and caveat completion."),
    c("tuning", "nested respondent-level tuning",
      if (nested_pass) "executed_computational_gates_pass" else
        "withheld_gate_failed_or_missing",
      "Candidate selection is inside outer training folds; do not call the architecture prespecified outcome-blind."),
    c("optimization", "multiple starts, bounds, and returned-state gates",
      if (opt_pass) "executed_computational_gates_pass" else
        "withheld_gate_failed_or_missing",
      "Passing attained-solution diagnostics does not certify a global optimum or a global approximation gap."),
    c("rank", "rank interiority and q sensitivity",
      if (rank_pass) "interiority_diagnostic_pass" else
        "interiority_gate_failed_or_missing",
      "q=1 is the fixed primary specification; q=0 and q=2 are sensitivities, never a data-selected rank."),
    c("point_qoi", "structural plug-in quantities",
      if (!identical(profile, "production"))
        "diagnostic_profile_not_reportable" else if (point_ready)
          "point_estimates_ready" else "not_ready",
      "Report as integrated full-sample plug-ins; no respondent posterior modes were used."),
    c("regular_inference", "respondent-level regular inference",
      if (inference_ready) "conditional_inference_available" else
        as.character(assessment$inference$status %||% "withheld"),
      "Intervals and majority claims remain withheld unless the classed verification record and quantity-specific gates pass."),
    c("heldout_fit", "held-out respondent-sequence scoring",
      if (heldout_verified) "run_verified_heldout" else if (heldout_run)
        "run_diagnostic_unverified" else "not_run",
      "Scores and calibration use whole respondent folds; retain the artifact's verification state verbatim."),
    c("calibration", "marginal and joint calibration",
      if (nrow(heldout$calibration_summary)) {
        if (heldout_verified) "run_verified_heldout" else
          "run_diagnostic_unverified"
      } else "not_run",
      "Calibration can reveal lack of fit but cannot establish shock independence or the maintained distribution."),
    c("numerical", "fresh-refit integration refinement",
      if (numerical_pass) "empirical_refinement_gate_pass" else
        "gate_failed_or_not_run",
      "Passing finite-grid checks does not prove the o_p(N^{-1/2}) numerical condition."),
    c("design_layer", "marginal AMCE-style empirical layer",
      "run_conditional_on_advertised_randomization",
      "This is a distinct marginal probability estimand, not the structural preference coefficient."),
    c("exact_ht", "exact ordered-contrast HT benchmark",
      "protocol_unavailable",
      "Withhold: fielded exposure probabilities and randomizer restrictions were not recovered."),
    c("shape_covariance", "shape and covariance sensitivities",
      paste(component_state("skewed|bimodal|heavy-tailed"),
            component_state("covariance varying"), sep = "; "),
      "Alternative residual-shape/covariance exercises are sensitivities and cannot verify maintained assumptions."),
    c("completion", "completion and attrition",
      component_state("completion/attrition"),
      "Diagnostics do not establish noninformative completion; the estimand remains the complete-case population."),
    c("targeted_sensitivity", "Male by prior-run interaction",
      component_state("Male x prior-run"),
      "Do not restore the original application claim unless the targeted sensitivity artifact has been reviewed."),
    c("provenance", "artifact signatures and checksums",
      if (signature_pass) "signature_families_match" else
        "signature_missing_or_mismatch",
      "Full-sample and nested-fold signatures are checked within, not across, their distinct fit families.")
  )
  ledger <- data.frame(
    evidence_id = vapply(rows, `[[`, character(1L), 1L),
    topic = vapply(rows, `[[`, character(1L), 2L),
    state = vapply(rows, `[[`, character(1L), 3L),
    manuscript_constraint = vapply(rows, `[[`, character(1L), 4L),
    profile = profile, stringsAsFactors = FALSE
  )
  ledger
}

.validation_table <- function(profile, paths, full, nested, assembled,
                              full_audit, nested_audit, qoi_plugin,
                              assessment, nested_tables, optimization,
                              numerical, signatures, provenance) {
  add <- function(check, pass, severity, consequence) {
    data.frame(check = check, pass = isTRUE(pass), severity = severity,
               consequence_if_failed = consequence,
               stringsAsFactors = FALSE)
  }
  rows <- list()
  rows[[length(rows) + 1L]] <- add(
    "all declared core source artifacts exist",
    all(!is.na(paths) & file.exists(paths)), "integrity",
    "The extraction is incomplete and must not be used."
  )
  rows[[length(rows) + 1L]] <- add(
    "core RDS classes match the application pipeline",
    inherits(full, "scmix_tuning") &&
      inherits(nested, "scmix_nested_tuning") &&
      inherits(assembled, "scmix_nested_assembled") &&
      inherits(full_audit, "scmix_optimization_audit") &&
      inherits(nested_audit, "scmix_optimization_audit") &&
      inherits(assessment, "sw2022_application_assessment"),
    "integrity", "The artifact schema is stale or malformed."
  )
  stamps <- vapply(
    list(full, nested, assembled),
    function(x) as.character(x$sw_application_specification$profile %||%
                               NA_character_), character(1L)
  )
  rows[[length(rows) + 1L]] <- add(
    "fit and assessment profiles match the requested profile",
    all(stamps == profile) && identical(assessment$profile, profile),
    "integrity", "Artifacts from different analysis profiles were mixed."
  )
  recorded_hashes_match <-
    all(provenance$fit_manifest_match[!is.na(provenance$fit_manifest_match)]) &&
    all(provenance$assessment_manifest_match[
      !is.na(provenance$assessment_manifest_match)
    ])
  rows[[length(rows) + 1L]] <- add(
    "all recorded upstream checksums match current source artifacts",
    recorded_hashes_match, "integrity",
    "At least one source artifact changed after its provenance record."
  )
  rows[[length(rows) + 1L]] <- add(
    "analysis signatures match within full-sample and nested-fold families",
    nrow(signatures) > 0L &&
      all(signatures$signature_present | !signatures$signature_required) &&
      all(signatures$family_signature_match), "integrity",
    "Fit-linked evidence cannot be joined safely."
  )

  candidate <- nested_tables$candidates
  candidate_key <- if (nrow(candidate)) {
    paste(candidate$scope,
          ifelse(is.na(candidate$outer_fold), "full", candidate$outer_fold),
          sep = ":")
  } else character()
  one_selected <- nrow(candidate) > 0L && all(vapply(
    split(candidate$selected_index_recorded, candidate_key),
    function(z) sum(z) == 1L, logical(1L)
  ))
  rows[[length(rows) + 1L]] <- add(
    "exactly one tuning candidate is selected in every selection problem",
    one_selected, "integrity",
    "Nested-CV selection cannot be reconstructed unambiguously."
  )
  selected_match <- nrow(nested_tables$selected) > 0L && all(vapply(
    seq_len(nrow(nested_tables$selected)), function(j) {
      fold <- nested_tables$selected$outer_fold[[j]]
      observed <- candidate$candidate[
        candidate$scope == "outer_training_inner_cv" &
          candidate$outer_fold == fold & candidate$selected_index_recorded
      ]
      length(observed) == 1L &&
        identical(as.character(observed),
                  as.character(nested_tables$selected$candidate[[j]]))
    }, logical(1L)
  ))
  rows[[length(rows) + 1L]] <- add(
    "assembled fold specifications equal inner-CV selections",
    selected_match, "integrity",
    "The assembled nuisance fit is not linked to the recorded selection."
  )

  rid <- as.character(assembled$respondent_id %||% character())
  fold <- assembled$fold_id %||% integer()
  respondent_fold_ok <- length(rid) > 0L && length(rid) == length(fold) &&
    all(vapply(split(fold, rid), function(z) length(unique(z)) == 1L,
               logical(1L)))
  rows[[length(rows) + 1L]] <- add(
    "every respondent belongs to exactly one outer fold",
    respondent_fold_ok, "integrity",
    "Task-level leakage may be present; held-out results are invalid."
  )
  expected_K <- full$sw_application_specification$profile_specification$outer_K %||%
    NA_integer_
  observed_K <- length(unique(fold))
  rows[[length(rows) + 1L]] <- add(
    "observed outer-fold count equals the profile specification",
    is.finite(expected_K) && observed_K == expected_K, "integrity",
    "The nested fit does not implement the declared profile."
  )
  rows[[length(rows) + 1L]] <- add(
    "frozen Saha--Weeks analysis dimensions are preserved",
    identical(as.integer(assembled$N), 1191L) && length(rid) == 3573L,
    "integrity", "The results do not target the frozen application sample."
  )

  rows[[length(rows) + 1L]] <- add(
    "no respondent posterior summaries enter the QOI artifact",
    identical(qoi_plugin$posterior_summaries_used, FALSE), "reporting_gate",
    "Structural plug-ins must be rebuilt by integrating the fitted distribution."
  )
  rows[[length(rows) + 1L]] <- add(
    "full and nested optimization computational gates pass",
    nrow(optimization$gates) == 2L &&
      all(optimization$gates$all_computational_gates_pass) &&
      all(optimization$gates$all_bound_diagnostics_complete) &&
      all(optimization$gates$all_objectives_finite), "evidence_gate",
    "Do not report the attained fit as the application estimate."
  )
  rows[[length(rows) + 1L]] <- add(
    "rank-interiority diagnostic passes in every outer fold",
    is.data.frame(assessment$inference$rank_gate) &&
      nrow(assessment$inference$rank_gate) > 0L &&
      all(assessment$inference$rank_gate$pass), "evidence_gate",
    "Ordinary regular inference is unavailable at a rank boundary."
  )
  rows[[length(rows) + 1L]] <- add(
    "fresh-refit numerical refinement and signature gates pass",
    nrow(numerical$status) == 1L && numerical$status$gate_pass &&
      numerical$status$signature_match, "evidence_gate",
    "Treat the finite integration rule as an unresolved numerical approximation."
  )
  rows[[length(rows) + 1L]] <- add(
    "regular respondent-level inference is available",
    isTRUE(assessment$inference$inference_available), "reporting_gate",
    "Withhold formal intervals and majority/sign-share claims."
  )
  rows[[length(rows) + 1L]] <- add(
    "maintained assumptions are not mislabeled as verified",
    identical(assessment$maintained_assumptions_verified, FALSE) &&
      all(!assessment$component_status$maintained_assumption_verified),
    "reporting_gate",
    "Rewrite the assessment state before using it in the manuscript."
  )
  do.call(rbind, rows)
}

.main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- .parse_args(args)
  project <- normalizePath(file.path(dirname(.script_file()), "../../.."),
                           mustWork = TRUE)
  app <- file.path(project, "applications", "sw2022")
  profile <- opt$profile
  fit_dir <- opt$fit_dir %||%
    file.path(app, "results", "mixed_logit", profile)
  assessment_dir <- opt$assessment_dir %||%
    file.path(app, "results", "assessment", profile)
  out_dir <- opt$out_dir %||%
    file.path(app, "results", "evidence_audit", profile)

  paths <- c(
    prepared = file.path(app, "results", "prep_analysis_data.rds"),
    fit_primary_full = file.path(fit_dir, "fit_primary_full.rds"),
    fit_primary_nested = file.path(fit_dir, "fit_primary_nested.rds"),
    fit_primary_assembled = file.path(fit_dir, "fit_primary_assembled.rds"),
    optimization_full = file.path(fit_dir, "optimization_primary_full.rds"),
    optimization_nested = file.path(fit_dir, "optimization_primary_nested.rds"),
    fit_manifest = file.path(fit_dir, "fit_manifest.rds"),
    q_sensitivity = file.path(fit_dir, "q_sensitivity.rds"),
    qoi_plugin = file.path(fit_dir, "qoi_plugin.rds"),
    inference_diagnostic = file.path(fit_dir, "inference_diagnostic.rds"),
    inference_manifest = file.path(fit_dir, "inference_manifest.rds"),
    integration_refinement = file.path(fit_dir, "integration_refinement.rds"),
    assessment_bundle = file.path(assessment_dir, "objects",
                                  "assessment_bundle.rds")
  )
  required <- c("prepared", "fit_primary_full", "fit_primary_nested",
                "fit_primary_assembled", "optimization_full",
                "optimization_nested", "fit_manifest", "q_sensitivity",
                "qoi_plugin", "inference_diagnostic", "inference_manifest",
                "integration_refinement", "assessment_bundle")
  missing <- required[!file.exists(paths[required])]
  if (isTRUE(opt$require_complete) && length(missing)) {
    stop("Post-fit extraction requires completed upstream artifacts. Missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  read_one <- function(nm) .read_stable_rds(
    paths[[nm]], required = isTRUE(opt$require_complete) && nm %in% required
  )
  prepared <- read_one("prepared")
  full <- read_one("fit_primary_full")
  nested <- read_one("fit_primary_nested")
  assembled <- read_one("fit_primary_assembled")
  full_audit <- read_one("optimization_full")
  nested_audit <- read_one("optimization_nested")
  fit_manifest <- read_one("fit_manifest")
  q_sensitivity <- read_one("q_sensitivity")
  qoi_plugin <- read_one("qoi_plugin")
  inference <- read_one("inference_diagnostic")
  inference_manifest <- read_one("inference_manifest")
  numerical_artifact <- read_one("integration_refinement")
  assessment <- read_one("assessment_bundle")
  misspecification <- .load_misspecification_evidence(fit_dir, profile)
  party_gender_mean <- .load_party_gender_mean_evidence(app, profile)

  if (!is.null(assessment) &&
      !inherits(assessment, "sw2022_application_assessment")) {
    stop("Assessment artifact has the wrong class.", call. = FALSE)
  }
  stamped_profiles <- vapply(
    list(full, nested, assembled),
    function(x) as.character(x$sw_application_specification$profile %||%
                               NA_character_), character(1L)
  )
  if (any(!is.na(stamped_profiles) & stamped_profiles != profile)) {
    stop("Fit stamp does not match --profile=", profile, call. = FALSE)
  }
  if (!is.null(assessment) && !identical(assessment$profile, profile)) {
    stop("Assessment profile does not match --profile=", profile,
         call. = FALSE)
  }

  nested_tables <- .nested_cv_tables(full, nested, assembled)
  optimization <- .optimization_tables(full_audit, nested_audit)
  heldout <- .heldout_tables(assessment, assembled)
  qoi <- .qoi_tables(assessment)
  numerical <- .numerical_tables(numerical_artifact)
  signatures <- .signature_rows(
    full, nested, assembled, qoi_plugin, inference, numerical_artifact,
    assessment
  )
  provenance <- .provenance_checks(paths, fit_manifest, assessment)
  recorded_mismatch <-
    (!is.na(provenance$fit_manifest_match) &
       !provenance$fit_manifest_match) |
    (!is.na(provenance$assessment_manifest_match) &
       !provenance$assessment_manifest_match)
  if (isTRUE(opt$require_complete) && any(recorded_mismatch)) {
    stop("A recorded source checksum does not match: ",
         paste(provenance$artifact[recorded_mismatch], collapse = ", "),
         call. = FALSE)
  }
  ledger <- .evidence_ledger(
    profile, paths, assessment, nested_tables, optimization, heldout, qoi,
    numerical, signatures
  )
  misspec_state <- if (isTRUE(misspecification$validated)) {
    paste0(
      "validated_design_specific_simulation; ",
      "empirical_alternative_refits_not_run; assumptions_not_verified"
    )
  } else "not_run"
  shape_row <- match("shape_covariance", ledger$evidence_id)
  if (!is.na(shape_row)) {
    ledger$state[[shape_row]] <- misspec_state
    ledger$manuscript_constraint[[shape_row]] <- paste(
      "Use simulated-data bias/stability only as a misspecification diagnostic;",
      "do not call it an empirical alternative-family fit, identification",
      "result, formal coverage assessment, or verification of normality/common covariance."
    )
  }
  ledger <- rbind(
    ledger,
    data.frame(
      evidence_id = "scale_serial_misspecification",
      topic = "random-scale and serial-shock sensitivity",
      state = misspec_state,
      manuscript_constraint = paste(
        "Random-scale raw coefficient magnitudes are not compared; formal",
        "coverage is unavailable; empirical alternative likelihoods remain not run."
      ),
      profile = profile, stringsAsFactors = FALSE
    )
  )
  party_gender_state <- if (isTRUE(party_gender_mean$validated)) {
    paste0(
      "run_crossfitted_posthoc_diagnostic; formal_inference_withheld; ",
      "outcome_blind_selection_false; maintained_model_false"
    )
  } else "not_run"
  ledger <- rbind(
    ledger,
    data.frame(
      evidence_id = "party_gender_mean_diagnostic",
      topic = "party-by-candidate-gender conditional-mean diagnostic",
      state = party_gender_state,
      manuscript_constraint = paste(
        "Label this as a post-hoc descriptive diagnostic only. It supplies",
        "no formal inference, no outcome-blind model-selection claim, and",
        "does not replace or modify the maintained mixed-logit estimator."
      ),
      profile = profile, stringsAsFactors = FALSE
    )
  )
  validation <- .validation_table(
    profile, paths, full, nested, assembled, full_audit, nested_audit,
    qoi_plugin, assessment, nested_tables, optimization, numerical,
    signatures, provenance
  )
  validation <- rbind(
    validation,
    data.frame(
      check = "validated design-specific misspecification evidence",
      pass = isTRUE(misspecification$validated),
      severity = "reporting_gate",
      consequence_if_failed = paste(
        "Shape, covariance-by-party, random-scale, and serial-shock",
        "simulation results remain not run and must not be discussed as evidence."
      ), stringsAsFactors = FALSE
    )
  )
  validation <- rbind(
    validation,
    data.frame(
      check = "validated party-by-candidate-gender post-hoc diagnostic",
      pass = isTRUE(party_gender_mean$validated),
      severity = "reporting_gate",
      consequence_if_failed = paste(
        "The party-gender diagnostic remains not run and must not be",
        "discussed as evidence. Formal inference, outcome-blind selection,",
        "and maintained-model claims remain unavailable in every state."
      ), stringsAsFactors = FALSE
    )
  )
  integrity_failure <- validation$severity == "integrity" & !validation$pass
  if (isTRUE(opt$require_complete) && any(integrity_failure)) {
    stop("Post-fit evidence integrity validation failed: ",
         paste(validation$check[integrity_failure], collapse = "; "),
         call. = FALSE)
  }

  rank_gate <- assessment$inference$rank_gate %||% data.frame()
  information <- assessment$local_information %||% data.frame()
  q_stability <- assessment$q_stability %||% data.frame()
  q_sensitivity_table <- q_sensitivity$table %||%
    assessment$q_sensitivity$table %||% data.frame()
  component_status <- assessment$component_status %||% data.frame()
  claims <- assessment$claims_ledger %||% data.frame()

  tables <- list(
    nested_cv_candidates = nested_tables$candidates,
    nested_cv_selected_by_outer_fold = nested_tables$selected,
    optimization_summary = optimization$summary,
    optimization_starts = optimization$starts,
    optimization_gate_summary = optimization$gates,
    rank_interiority_diagnostic = rank_gate,
    information_eigenvalues = information,
    rank_q_stability = q_stability,
    rank_q_qoi_sensitivity = q_sensitivity_table,
    heldout_sequence_score = heldout$score,
    heldout_sequence_score_by_outer_fold = heldout$by_fold,
    calibration_marginal = heldout$marginal,
    calibration_joint = heldout$joint,
    calibration_summary = heldout$calibration_summary,
    structural_plugin_quantities_all = qoi$plugin,
    inference_targets_all = qoi$targets,
    inference_transforms_all = qoi$transforms,
    quantity_reporting_gates = qoi$gates,
    qoi_evidence_catalog = qoi$catalog,
    numerical_refinement_status = numerical$status,
    numerical_refinement_settings = numerical$settings,
    numerical_refinement_checks = numerical$checks,
    numerical_refinement_gate = numerical$comparison,
    analysis_signature_checks = signatures,
    provenance_checks = provenance,
    assessment_component_status = component_status,
    manuscript_claims_ledger = claims,
    section5_1_evidence_ledger = ledger,
    extraction_validation = validation
  )
  if (length(misspecification$tables)) {
    tables <- c(tables, misspecification$tables)
  }
  if (length(party_gender_mean$tables)) {
    tables <- c(tables, party_gender_mean$tables)
  }
  tables <- tables[vapply(tables, is.data.frame, logical(1L))]
  tables <- tables[vapply(tables, ncol, integer(1L)) > 0L]

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  table_dir <- file.path(out_dir, "tables")
  manifest_dir <- file.path(out_dir, "manifests")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
  stale_party_gender_tables <- list.files(
    table_dir, pattern = "^party_gender_mean_.*\\.csv$", full.names = TRUE
  )
  if (length(stale_party_gender_tables) &&
      !all(file.remove(stale_party_gender_tables))) {
    stop("Could not clear stale party-gender evidence exports.", call. = FALSE)
  }
  for (nm in names(tables)) {
    .atomic_write_csv(tables[[nm]], file.path(table_dir, paste0(nm, ".csv")))
  }

  source_manifest <- provenance
  source_manifest$path <- vapply(source_manifest$path, .relative,
                                 character(1L), root = project)
  .atomic_write_csv(source_manifest,
                    file.path(manifest_dir, "source_artifact_manifest.csv"))

  sample_N <- prepared$sample$N %||% NA_integer_
  sample_tasks <- prepared$sample$task_rows %||% NA_integer_
  readme <- c(
    "# Saha--Weeks post-fit evidence audit",
    "",
    paste0("Profile: `", profile, "`."),
    paste0("Frozen sample: N = ", sample_N, " respondents; ", sample_tasks,
           " tasks."),
    "",
    "This directory is a read-only extraction from completed fit, inference, and assessment artifacts. It is not manuscript prose.",
    "",
    "Start with `tables/section5_1_evidence_ledger.csv`. The all-QOI tables preserve every structural plug-in, primitive one-step target, transformed target, and reporting gate. Nested-CV tables expose every candidate score and the selected learner within each outer training fold.",
    "",
    paste0("Inference state: `", assessment$inference$status %||% "not_run",
           "`; formal inference available: `",
           isTRUE(assessment$inference$inference_available), "`."),
    "Passing optimization, rank, or finite-node refinement diagnostics is not a global-optimum certificate and does not prove an asymptotic numerical-error rate.",
    "Held-out verification flags are copied without promotion. Normality, common covariance, shock independence, and noninformative completion remain maintained assumptions rather than verified facts.",
    paste0(
      "Design-specific misspecification simulation: `",
      if (isTRUE(misspecification$validated)) "validated" else "not_run",
      "`. This is distinct from empirical alternative-family refits, which remain `not_run`; formal coverage remains unavailable."
    ),
    paste0(
      "Party-by-candidate-gender mean diagnostic: `",
      if (isTRUE(party_gender_mean$validated))
        "validated_posthoc_descriptive" else "not_run",
      "`. It supplies no formal inference, was not selected outcome-blind, and is not the maintained model."
    ),
    "",
    paste0("Git revision at extraction: `", .git_revision(project), "`."),
    paste0("Created UTC: `", format(Sys.time(), tz = "UTC", usetz = TRUE), "`.")
  )
  .atomic_write_lines(readme, file.path(out_dir, "README.md"))

  bundle <- list(
    schema_version = "sw2022-postfit-evidence-v1",
    profile = profile,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    git_revision = .git_revision(project),
    source_paths = paths,
    source_provenance = provenance,
    sample = prepared$sample,
    estimand = prepared$estimand,
    tables = tables,
    inference_state = list(
      status = assessment$inference$status %||% "not_run",
      available = isTRUE(assessment$inference$inference_available),
      claim = assessment$inference$inference_claim %||% "not_available",
      reason = assessment$inference$reason %||% ""
    ),
    misspecification_evidence = list(
      available = isTRUE(misspecification$available),
      validated = isTRUE(misspecification$validated),
      source_paths = misspecification$paths %||% character(),
      source_md5 = misspecification$source_md5 %||% character(),
      scenarios = misspecification$scenarios %||% character(),
      frozen_replications_per_scenario =
        misspecification$frozen_replications_per_scenario %||%
        NA_integer_,
      frozen_total_refits = misspecification$frozen_total_refits %||%
        NA_integer_,
      minimum_defensible_replications_per_scenario =
        misspecification$minimum_defensible_replications_per_scenario %||%
        NA_integer_,
      requested_replications_per_scenario =
        misspecification$requested_replications_per_scenario %||%
        NA_integer_,
      requested_total_refits = misspecification$requested_total_refits %||%
        NA_integer_,
      completed_total_refits = misspecification$completed_total_refits %||%
        0L,
      design_specific_simulation = isTRUE(misspecification$validated),
      empirical_alternative_family_refits = "not_run",
      alternative_identification_established = FALSE,
      formal_inference_available = FALSE,
      coverage_evaluated = FALSE,
      maintained_assumptions_verified = FALSE
    ),
    party_gender_mean_evidence = list(
      available = isTRUE(party_gender_mean$available),
      validated = isTRUE(party_gender_mean$validated),
      source_paths = party_gender_mean$paths %||% character(),
      source_md5 = party_gender_mean$source_md5 %||% character(),
      artifact_audit = party_gender_mean$artifact_audit %||% data.frame(),
      input_audit = party_gender_mean$input_audit %||% data.frame(),
      rds_csv_agreement =
        party_gender_mean$table_csv_agreement %||% data.frame(),
      descriptive_use_gate =
        isTRUE(party_gender_mean$descriptive_use_gate),
      post_hoc = TRUE,
      formal_inference_available = FALSE,
      diagnostic_selection_outcome_blind = FALSE,
      outcome_blind = FALSE,
      maintained_model = FALSE,
      primary_artifacts_modified = FALSE
    ),
    posterior_summaries_used = isTRUE(qoi_plugin$posterior_summaries_used),
    source_project_modified = FALSE
  )
  class(bundle) <- c("sw2022_postfit_evidence", "list")
  .atomic_save_rds(bundle, file.path(out_dir, "evidence_bundle.rds"))

  generated <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  generated <- generated[file.info(generated)$isdir %in% FALSE]
  manifest_path <- file.path(manifest_dir, "generated_artifact_manifest.csv")
  generated <- setdiff(generated, manifest_path)
  generated_manifest <- data.frame(
    path = vapply(generated, .relative, character(1L), root = project),
    bytes = file.info(generated)$size,
    md5 = unname(tools::md5sum(generated)),
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    git_revision = .git_revision(project), stringsAsFactors = FALSE
  )
  .atomic_write_csv(generated_manifest, manifest_path)

  ## A second source checksum confirms that extraction did not race a producer.
  after <- provenance
  after$current_md5_after_extraction <- NA_character_
  after$current_md5_after_extraction[after$exists] <-
    unname(tools::md5sum(paths[after$exists]))
  changed <- after$exists &
    after$current_md5 != after$current_md5_after_extraction
  if (any(changed)) {
    stop("One or more source artifacts changed during extraction: ",
         paste(after$artifact[changed], collapse = ", "),
         ". Discard this audit and rerun after upstream stages finish.",
         call. = FALSE)
  }
  if (isTRUE(misspecification$validated)) {
    misspec_after <- vapply(misspecification$paths, .md5, character(1L))
    if (!identical(unname(misspec_after),
                   unname(misspecification$source_md5))) {
      stop(
        "Misspecification evidence changed during extraction. Discard this ",
        "audit and rerun after its producer and validator finish.",
        call. = FALSE
      )
    }
  }
  if (isTRUE(party_gender_mean$validated)) {
    party_source_after <- vapply(
      party_gender_mean$paths, .md5, character(1L)
    )
    party_artifact_after <- vapply(
      party_gender_mean$artifact_audit$path, .md5, character(1L)
    )
    party_input_after <- vapply(
      party_gender_mean$input_audit$path, .md5, character(1L)
    )
    party_race_ok <-
      identical(
        unname(party_source_after),
        unname(party_gender_mean$source_md5)
      ) &&
      identical(
        unname(party_artifact_after),
        unname(party_gender_mean$artifact_audit$current_md5)
      ) &&
      identical(
        unname(party_input_after),
        unname(party_gender_mean$input_audit$current_md5)
      )
    if (!party_race_ok) {
      stop(
        "Party-gender diagnostic or one of its frozen inputs changed during ",
        "extraction. Discard this audit and rerun after its producer finishes.",
        call. = FALSE
      )
    }
  } else if (isTRUE(party_gender_mean$directory_was_absent) &&
             dir.exists(party_gender_mean$directory)) {
    stop(
      "Party-gender diagnostic appeared during extraction. Discard this ",
      "audit and rerun after its producer finishes.", call. = FALSE
    )
  }

  cat(sprintf(
    paste0("Saha--Weeks post-fit evidence audit (%s): %d normalized tables; ",
           "formal inference=%s.\n"),
    profile, length(tables),
    isTRUE(assessment$inference$inference_available)
  ))
  invisible(bundle)
}

if (sys.nframe() == 0L) .main()
