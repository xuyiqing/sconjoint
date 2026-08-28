#!/usr/bin/env Rscript

## Independent hash/status validator for v2.1 application sensitivities and
## design-specific simulations. It never promotes simulations to empirical
## alternative likelihoods or enables formal inference.

options(stringsAsFactors = FALSE, warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  z <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(z)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", z[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(profile = "validated_fallback", component = "all")
  for (arg in x) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg,
                                        call. = FALSE)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", bits[[1L]], fixed = TRUE)
    if (!key %in% names(out)) stop("Unknown argument --", bits[[1L]],
                                    call. = FALSE)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  if (!out$component %in% c("all", "application", "misspecification")) {
    stop("--component must be all, application, or misspecification.",
         call. = FALSE)
  }
  out
}

.artifact_gate <- function(manifest, out_dir) {
  a <- manifest$artifact_md5
  if (!is.character(a) || !length(a) || is.null(names(a)) ||
      anyDuplicated(names(a)) || any(grepl("^/|[.][.][/\\\\]", names(a)))) {
    return(FALSE)
  }
  paths <- file.path(out_dir, names(a)); names(paths) <- names(a)
  all(file.exists(paths)) && .sw_v21_same_md5(.sw_v21_md5(paths), a)
}

.source_gate <- function(manifest) {
  p <- manifest$source_paths; h <- manifest$source_md5
  is.character(p) && length(p) && !is.null(names(p)) &&
    all(file.exists(p)) && .sw_v21_same_md5(.sw_v21_md5(p), h)
}

.application_gate <- function(path, out_dir, profile, context) {
  if (!file.exists(path)) return(list(pass = FALSE, reasons = "manifest_absent"))
  m <- readRDS(path); reasons <- character()
  add <- function(ok, why) if (!isTRUE(ok)) reasons <<- c(reasons, why)
  add(identical(m$schema_version,
                "sw2022-v2.1-application-sensitivity-manifest-v1"),
      "schema")
  add(identical(m$profile, profile), "profile")
  add(identical(m$complete_battery, TRUE), "incomplete_battery")
  add(setequal(m$completed_components,
               c("z19", "interaction", "process", "completion")),
      "components")
  add(identical(m$reported_primary, context$pointer$reported_primary),
      "reported_primary")
  add(.sw_v21_same_md5(m$pointer_lock_md5, context$lock_md5),
      "pointer_lock")
  add(.source_gate(m), "source_hashes")
  add(.artifact_gate(m, out_dir), "artifact_hashes")
  add(identical(m$primary_artifacts_modified, FALSE), "primary_modified")
  add(identical(m$maintained_assumptions_verified, FALSE),
      "assumption_claim")
  add(identical(m$outcome_blind, FALSE), "outcome_label")
  add(identical(m$formal_inference_available, FALSE), "formal_inference")
  add(identical(m$empirical_alternative_likelihoods, "not_run"),
      "alternative_likelihood_status")
  add(identical(m$profile_likelihoods, "not_run"),
      "profile_likelihood_status")
  if (!is.na(m$completion_raw_path %||% NA_character_)) {
    add(file.exists(m$completion_raw_path) &&
          identical(unname(tools::md5sum(m$completion_raw_path)),
                    m$completion_raw_md5), "completion_raw_hash")
  } else add(FALSE, "completion_raw_missing")
  status_path <- file.path(out_dir, "tables", "application_status.csv")
  alt_path <- file.path(out_dir, "tables",
                        "empirical_alternative_likelihood_status.csv")
  profile_path <- file.path(out_dir, "tables", "profile_likelihood_status.csv")
  add(all(file.exists(c(status_path, alt_path, profile_path))),
      "status_tables_absent")
  component_paths <- c(
    z19 = file.path(out_dir, "fit_z19_sensitivity.rds"),
    interaction = file.path(out_dir, "fit_male_run_interaction.rds"),
    process = file.path(out_dir, "task_process_diagnostics.rds"),
    completion = file.path(out_dir, "completion_sample_sensitivity.rds"))
  add(all(file.exists(component_paths)), "component_artifacts_absent")
  if (all(file.exists(component_paths))) {
    component <- lapply(component_paths, readRDS)
    add(all(component$z19$fit$optimization_gate_by_fold) &&
          isTRUE(component$z19$fit$full_optimization_gate),
        "z19_optimizer_gates")
    add(all(component$interaction$fit$optimization_gate_by_fold) &&
          isTRUE(component$interaction$fit$full_optimization_gate),
        "interaction_optimizer_gates")
    add(isTRUE(component$process$profile_swap_fit$gate$pass),
        "position_swap_optimizer_gate")
    add(all(component$completion$optimizer_gate),
        "completion_optimizer_gates")
    add(identical(component$process$task_process_alternative_likelihood,
                  "not_run") &&
          identical(component$process$serial_shock_alternative_likelihood,
                    "not_run"), "process_alternative_status")
    add(all(vapply(component, function(x)
      identical(x$outcome_blind, FALSE) &&
        identical(x$formal_inference_available, FALSE), logical(1L))),
      "component_fail_closed_labels")
  }
  if (file.exists(alt_path)) {
    z <- utils::read.csv(alt_path, stringsAsFactors = FALSE)
    add(nrow(z) >= 6L && all(z$status == "not_run") &&
          !any(z$implemented) && !any(z$empirical_refit),
        "alternative_likelihood_table")
  }
  if (file.exists(profile_path)) {
    z <- utils::read.csv(profile_path, stringsAsFactors = FALSE)
    add(nrow(z) >= 4L && all(z$status == "not_run") &&
          !any(z$verified_profile), "profile_likelihood_table")
  }
  list(pass = !length(reasons), reasons = unique(reasons), manifest = m)
}

.misspecification_gate <- function(path, out_dir, profile, context,
                                    misspec_config) {
  if (!file.exists(path)) return(list(pass = FALSE, reasons = "manifest_absent"))
  m <- readRDS(path); reasons <- character()
  add <- function(ok, why) if (!isTRUE(ok)) reasons <<- c(reasons, why)
  add(identical(m$schema_version,
                "sw2022-v2.1-design-misspecification-manifest-v1"),
      "schema")
  add(identical(m$profile, profile), "profile")
  add(setequal(m$scenarios, misspec_config$scenarios), "scenario_battery")
  add(identical(m$complete_scenario_battery, TRUE), "incomplete_battery")
  add(isTRUE(m$replications >= m$minimum_defensible_replications),
      "replication_minimum")
  add(identical(m$all_optimizer_gates_pass, TRUE), "optimizer_gates")
  add(identical(m$calibration_gate, TRUE), "dgp_calibration")
  add(identical(m$truth_refinement_gate, TRUE), "truth_refinement")
  add(identical(m$random_scale_comparability_gate, TRUE),
      "random_scale_comparability")
  add(identical(m$simulation_validation_pass, TRUE), "simulation_validation")
  add(identical(m$reported_primary, context$pointer$reported_primary),
      "reported_primary")
  add(.sw_v21_same_md5(m$pointer_lock_md5, context$lock_md5),
      "pointer_lock")
  add(.source_gate(m), "source_hashes")
  add(.artifact_gate(m, out_dir), "artifact_hashes")
  add(identical(m$dgp_definitions_reused_exactly, TRUE), "dgp_reuse")
  add(identical(m$scenario_order_reused_exactly, TRUE), "scenario_reuse")
  add(identical(m$seed_reused_exactly, TRUE), "seed_reuse")
  add(identical(m$primary_artifacts_modified, FALSE), "primary_modified")
  add(identical(m$empirical_alternative_likelihoods, "not_run"),
      "alternative_likelihood_status")
  add(identical(m$profile_likelihoods, "not_run"),
      "profile_likelihood_status")
  add(identical(m$coverage_evaluated, FALSE), "coverage_claim")
  add(identical(m$materiality_pass_issued, FALSE), "materiality_claim")
  add(identical(m$maintained_assumptions_verified, FALSE),
      "assumption_claim")
  add(identical(m$outcome_blind, FALSE), "outcome_label")
  add(identical(m$formal_inference_available, FALSE), "formal_inference")
  status_path <- file.path(out_dir, "tables", "structural_component_status.csv")
  coverage_path <- file.path(out_dir, "tables", "coverage_status.csv")
  add(all(file.exists(c(status_path, coverage_path))), "status_tables_absent")
  if (file.exists(status_path)) {
    z <- utils::read.csv(status_path, stringsAsFactors = FALSE)
    empirical <- z$empirical_alternative_likelihood
    add(sum(empirical) >= 6L && all(z$status[empirical] == "not_run") &&
          !any(z$maintained_assumption_verified) &&
          !any(z$formal_inference) && !any(z$outcome_blind),
        "structural_status_table")
  }
  list(pass = !length(reasons), reasons = unique(reasons), manifest = m)
}

.main <- function() {
  cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
  script <- .script_file()
  root <- normalizePath(file.path(dirname(script), "../../../../.."),
                        mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  options(sconjoint.sw_application_root = app)
  parent_config_path <- file.path(app, "v2_1", "config",
                                  "analysis_config_v2_1.R")
  source(parent_config_path, local = FALSE)
  sens_config_path <- file.path(dirname(script), "..", "config",
                                "sensitivity_config_v2_1.R")
  misspec_config_path <- file.path(dirname(script), "..", "config",
                                   "misspecification_config_v2_1.R")
  source(sens_config_path, local = FALSE)
  source(misspec_config_path, local = FALSE)
  if (!cli$profile %in% names(sw_v21_sensitivity_config$profiles)) {
    stop("Unknown profile: ", cli$profile, call. = FALSE)
  }
  contract_path <- file.path(dirname(script),
                             "reported_primary_contract_v2_1.R")
  helper_path <- file.path(dirname(script), "fit_helpers_v2_1.R")
  source(contract_path, local = FALSE); source(helper_path, local = FALSE)
  context <- .sw_v21_validate_reported_primary(
    sw_v21_sensitivity_config$reported_primary_pointer, sw_v21_config,
    load_fits = TRUE)
  profile_root <- file.path(sw_v21_sensitivity_config$output_root,
                            cli$profile)
  checks <- list()
  if (cli$component %in% c("all", "application")) {
    out <- file.path(profile_root, "application")
    checks$application <- .application_gate(
      file.path(out, "manifest.rds"), out, cli$profile, context)
  }
  if (cli$component %in% c("all", "misspecification")) {
    out <- file.path(profile_root, "misspecification")
    checks$misspecification <- .misspecification_gate(
      file.path(out, "manifest.rds"), out, cli$profile, context,
      sw_v21_misspecification_config)
  }
  pass <- length(checks) > 0L &&
    all(vapply(checks, function(x) isTRUE(x$pass), logical(1L)))
  table <- do.call(rbind, lapply(names(checks), function(nm) data.frame(
    component = nm, pass = checks[[nm]]$pass,
    reasons = paste(checks[[nm]]$reasons, collapse = ";"),
    stringsAsFactors = FALSE)))
  validation <- list(
    schema_version = "sw2022-v2.1-sensitivity-validation-v1",
    profile = cli$profile, requested_component = cli$component,
    pass = pass, table = table,
    reported_primary = context$pointer$reported_primary,
    pointer_path = context$pointer_path, pointer_lock_md5 = context$lock_md5,
    outcome_blind = FALSE, formal_inference_available = FALSE,
    empirical_alternative_likelihoods = "not_run",
    profile_likelihoods = "not_run",
    validated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  dir.create(profile_root, recursive = TRUE, showWarnings = FALSE)
  .sw_v21_atomic_save(validation, file.path(profile_root, "validation.rds"),
                      portable = FALSE)
  .sw_v21_write_csv(table, file.path(profile_root, "validation.csv"))
  if (!pass) stop("v2.1 sensitivity validation failed: ",
                  paste(unique(unlist(lapply(checks, `[[`, "reasons"))),
                        collapse = ", "), call. = FALSE)
  message("v2.1 sensitivity validation passed: ", profile_root)
  invisible(validation)
}

if (sys.nframe() == 0L) .main()
