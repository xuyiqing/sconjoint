#!/usr/bin/env Rscript

## Immutable Graham--Svolik v2.8.2 q=1 target-stage runner.
##
## This is deliberately separate from 05_run_fast_v282.R.  It never modifies
## fit, assembly, refinement, or rank-comparison checkpoints.  It reads only
## the hash-bound v2.8.2 q=1 assembly and pre-estimation preflight.  It does not
## inspect q=2 or use rank comparisons to select targets.  It never reads,
## copies, or resumes any complete or partial v2.8.1 target checkpoint.
##
## Examples, after committing the protocol and implementation:
##   applications/bin/Rscript45 applications/gs2020/R/06_run_targets_v282.R status
##   applications/bin/Rscript45 applications/gs2020/R/06_run_targets_v282.R bind
##   applications/bin/Rscript45 applications/gs2020/R/06_run_targets_v282.R basis
##   applications/bin/Rscript45 applications/gs2020/R/06_run_targets_v282.R family --family=heterogeneity_core
##   applications/bin/Rscript45 applications/gs2020/R/06_run_targets_v282.R export

options(stringsAsFactors = FALSE, warn = 1)

.gs_v282t_script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/",
                mustWork = TRUE)
}

.gs_v282t_repo_root_from_script <- function() {
  normalizePath(file.path(dirname(.gs_v282t_script_path()), "..", "..", ".."),
                winslash = "/", mustWork = TRUE)
}

.gs_v282t_bootstrap_sha256 <- function(path) {
  executable <- Sys.which("shasum")
  if (!nzchar(executable)) stop("The `shasum` executable is required.",
                                call. = FALSE)
  line <- system2(executable, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    stop("Could not hash bootstrap dependency: ", path, call. = FALSE)
  }
  sub("[[:space:]].*$", "", line)
}

.gs_v282t_bootstrap_guard <- function(repo_root,
                                     versions = c("v282", "v282-targets")) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is required before target execution.",
                         call. = FALSE)
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--untracked-files=all"), stdout = TRUE,
                    stderr = TRUE)
  if (length(status)) {
    stop("Target execution requires a clean repository: ",
         paste(status, collapse = "; "), call. = FALSE)
  }
  tracked <- system2(git, c("-C", shQuote(repo_root), "ls-files"),
                     stdout = TRUE, stderr = TRUE)
  all_r <- file.path("R", list.files(file.path(repo_root, "R"),
                                      pattern = "[.]R$", recursive = TRUE))
  for (version in versions) {
    protocol_dir <- file.path(repo_root, "applications", "gs2020",
                              "protocol", version)
    checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
    lines <- readLines(checksum_path, warn = FALSE)
    pieces <- strsplit(lines, "  ", fixed = TRUE)
    if (!length(lines) || any(lengths(pieces) != 2L)) {
      stop("Malformed bootstrap checksum registry for ", version, ".",
           call. = FALSE)
    }
    expected_hash <- vapply(pieces, `[[`, character(1L), 1L)
    registered <- vapply(pieces, `[[`, character(1L), 2L)
    actual_protocol <- list.files(protocol_dir, recursive = TRUE,
                                  all.files = TRUE, no.. = TRUE)
    actual_protocol <- setdiff(actual_protocol, "CHECKSUMS.sha256")
    unsafe <- grepl("^/|(^|/)\\.\\.(/|$)", registered)
    if (anyDuplicated(registered) || any(unsafe) ||
        !setequal(registered, actual_protocol)) {
      stop("Bootstrap protocol coverage is incomplete for ", version, ".",
           call. = FALSE)
    }
    observed_hash <- vapply(registered, function(name) {
      .gs_v282t_bootstrap_sha256(file.path(protocol_dir, name))
    }, character(1L))
    if (!identical(unname(observed_hash), unname(expected_hash))) {
      stop("Bootstrap protocol hash mismatch for ", version, ".",
           call. = FALSE)
    }
    implementation <- utils::read.csv(
      file.path(protocol_dir, "implementation-manifest.csv"),
      stringsAsFactors = FALSE, check.names = FALSE,
      na.strings = character())
    paths <- implementation$relative_path
    if (anyDuplicated(paths) || any(grepl("^/|(^|/)\\.\\.(/|$)", paths)) ||
        any(implementation$sha256 == "PENDING_FINALIZATION") ||
        !setequal(paths[startsWith(paths, "R/")], all_r) ||
        !"applications/bin/Rscript45" %in% paths ||
        !all(paths %in% tracked) ||
        !all(file.path("applications", "gs2020", "protocol", version,
                       c("CHECKSUMS.sha256", actual_protocol)) %in% tracked)) {
      stop("Bootstrap implementation coverage is incomplete for ", version,
           ".", call. = FALSE)
    }
    implementation_hash <- vapply(paths, function(path) {
      .gs_v282t_bootstrap_sha256(file.path(repo_root, path))
    }, character(1L))
    if (!identical(unname(implementation_hash),
                   unname(implementation$sha256))) {
      stop("Bootstrap implementation hash mismatch for ", version, ".",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.gs_v282t_load_code <- function(repo_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The `pkgload` package is required.", call. = FALSE)
  }
  pkgload::load_all(repo_root, quiet = TRUE, export_all = FALSE)
  sys.source(file.path(repo_root, "applications", "gs2020", "R",
                       "gs_v282_contract.R"), envir = .GlobalEnv)
  .gs_v282_set_rng_contract()
  sys.source(file.path(repo_root, "applications", "R",
                       "targeted_q1_inference.R"), envir = .GlobalEnv)
  sys.source(file.path(repo_root, "applications", "gs2020", "R",
                       "gs_v282_targets_contract.R"), envir = .GlobalEnv)
  invisible(TRUE)
}

.gs_v282t_parse_cli <- function(args) {
  if (!length(args)) {
    stop("Supply status, bind, basis, family, all-families, or export.",
         call. = FALSE)
  }
  out <- list(command = args[[1L]], family = "")
  for (arg in args[-1L]) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg,
                                        call. = FALSE)
    pieces <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- pieces[[1L]]
    if (!key %in% "family") stop("Unknown argument --", key, call. = FALSE)
    out[[key]] <- paste(pieces[-1L], collapse = "=")
  }
  out
}

.gs_v282t_one_thread <- function() {
  .gs_v282_set_thread_contract()
}

.gs_v282t_parent_protocol <- function(repo_root, source_root, require_clean) {
  .gs_v282_validate_protocol(
    repo_root, source_root = source_root, require_external = TRUE,
    require_clean = require_clean)
}

.gs_v282t_bind_stage <- function(repo_root, protocol, v28_protocol) {
  output_dir <- .gs_v282t_output_dir(repo_root)
  upstream <- .gs_v282t_verify_upstream(repo_root, v28_protocol)
  path <- .gs_v282t_checkpoint_path(output_dir, "binding")
  binding <- .gs_v282t_binding(protocol, "binding", upstream = upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable target binding checkpoint.")
    return(invisible(path))
  }
  payload <- list(
    schema_version = "gs2020-v2.8.2-target-binding-v1",
    upstream_hashes = upstream$hashes,
    upstream_paths = upstream$paths,
    parent_protocol_hash = v28_protocol$protocol_hash,
    parent_git_commit = upstream$hashes$parent_run_git_commit,
    frozen_registry_hashes = stats::setNames(
      protocol$frozen$sha256, protocol$frozen$dependency_id),
    target_registry_count = nrow(protocol$targets),
    family_registry_count = nrow(protocol$families),
    q1_only = TRUE,
    q2_used_for_target_selection = FALSE,
    historical_target_estimates_used = FALSE,
    prior_target_checkpoint_reuse = FALSE,
    v281_partial_checkpoint_reuse = FALSE,
    reporting_status = .gs_v282t_supported_status,
    formal_release = FALSE)
  .gs_v282t_save_atomic(payload, path, binding)
  message("Created immutable q=1 target binding; no target values were computed.")
  invisible(path)
}

.gs_v282t_basis_stage <- function(repo_root, protocol, v28_protocol) {
  linked <- .gs_v282t_load_binding(repo_root, protocol, v28_protocol)
  output_dir <- .gs_v282t_output_dir(repo_root)
  path <- .gs_v282t_checkpoint_path(output_dir, "basis")
  binding <- .gs_v282t_binding(protocol, "basis",
                              upstream = linked$upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=1 target basis checkpoint.")
    return(invisible(path))
  }
  fit <- linked$upstream$assembled$assembled
  basis <- tq1_rebuild_inference_basis(fit, prediction_tolerance = 5e-10)
  if (any(!is.finite(basis$prediction_max_abs_deviation)) ||
      max(basis$prediction_max_abs_deviation) > basis$prediction_tolerance) {
    stop("The rebuilt target basis failed its prediction check.",
         call. = FALSE)
  }
  payload <- list(
    schema_version = "gs2020-v2.8.2-target-basis-v1",
    basis = basis,
    source_q1_assembled_sha256 = linked$upstream$hashes$q1_assembled_sha256,
    q2_used_for_target_selection = FALSE)
  .gs_v282t_save_atomic(payload, path, binding)
  message("Created immutable q=1 target basis; no target values were printed.")
  invisible(path)
}

.gs_v282t_family_stage <- function(repo_root, protocol, v28_protocol,
                                  family_id) {
  if (!family_id %in% protocol$families$family_id) {
    stop("Unknown --family=", family_id, call. = FALSE)
  }
  linked <- .gs_v282t_load_binding(repo_root, protocol, v28_protocol)
  output_dir <- .gs_v282t_output_dir(repo_root)
  basis_path <- .gs_v282t_checkpoint_path(output_dir, "basis")
  basis_binding <- .gs_v282t_binding(protocol, "basis",
                                    upstream = linked$upstream$hashes)
  basis <- .gs_v282t_read_checkpoint(
    basis_path, binding = basis_binding)$payload$basis
  path <- .gs_v282t_checkpoint_path(output_dir, "family", family_id)
  binding <- .gs_v282t_binding(protocol, "family", family_id,
                              linked$upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable target-family checkpoint: ", family_id)
    return(invisible(path))
  }
  registries <- .gs_v282t_registries(repo_root, protocol)
  fit <- linked$upstream$assembled$assembled
  bundle <- .gs_v282t_build_family(
    family_id, fit, linked$upstream$preflight, registries)
  if (identical(bundle$status, .gs_v282t_unavailable_status)) {
    payload <- .gs_v282t_unavailable_family(bundle)
  } else {
    seed_row <- protocol$seeds[protocol$seeds$family_id == family_id, ,
                               drop = FALSE]
    if (nrow(seed_row) != 1L) stop("Missing family seed.", call. = FALSE)
    dml <- tryCatch(.gs_v282t_run_dml_family(
      fit, basis$mu_basis, bundle,
      seed = as.integer(seed_row$dml_seed[[1L]])),
      error = function(e) list(
        status = "withheld_dml_error",
        reason = conditionMessage(e)))
    fit_gate <- isTRUE(
      isTRUE(linked$upstream$assembled$v28_application_gate_pass) &&
        isTRUE(all(linked$upstream$assembled$fit_gate_by_scope)))
    payload <- .gs_v282t_finalize_family(
      dml, bundle, fit_gate,
      multiplier_seed = as.integer(seed_row$multiplier_seed[[1L]]))
    reported_se <- stats::setNames(
      payload$summary$diagnostic_se, payload$summary$target_id)
    stability <- tryCatch(
      .gs_v282t_near_optimum_stability(
        application_assembly = linked$upstream$assembled,
        preflight = linked$upstream$preflight,
        bundle = bundle, reported_se = reported_se),
      error = function(e) .gs_v282t_near_optimum_failure(
        bundle$rows$target_id, reported_se, conditionMessage(e)))
    payload$near_optimum_stability <- stability
    stability_detail <- stability$target_detail[
      match(payload$summary$target_id,
            stability$target_detail$target_id), , drop = FALSE]
    payload$summary$near_optimum_selected_movement_se <-
      stability_detail$near_optimum_selected_movement_se
    payload$summary$near_optimum_gate_state <- stability_detail$gate_state
    payload$summary$near_optimum_gate_pass <- stability_detail$gate_pass
    payload$summary$reason <- paste0(
      payload$summary$reason, "; near_optimum_gate_state=",
      payload$summary$near_optimum_gate_state)
    if (.gs_v282t_stability_evidence_complete(stability)) {
      payload$unavailable_formal_gates <- setdiff(
        payload$unavailable_formal_gates,
        "target_near_optimum_selected_movement")
    }
  }
  payload$target_protocol_id <- protocol$protocol_id
  payload$target_protocol_hash <- protocol$protocol_hash
  payload$source_q1_assembled_sha256 <-
    linked$upstream$hashes$q1_assembled_sha256
  payload$riesz_rule <- "target_specific_training_only_validation"
  payload$riesz_grid <- "0,Ntrain^-1,Ntrain^-3/4,Ntrain^-1/2"
  payload$riesz_validation_fraction <- 0.2
  payload$fast_riesz_evidence_status <-
    "diagnostic_outer_nuisance_held_fixed"
  payload$dml_execution <-
    "one_target_specific_DML_call_per_registered_target"
  payload$dml_seed_rule <-
    "family_base_seed_plus_registered_target_index"
  payload$fixed_numerical_rank_rule <- "dimension_epsilon"
  payload$riesz_cutoff <- .gs_v282t_riesz_cutoff
  payload$riesz_correction_relative_cutoff <-
    .gs_v282t_riesz_correction_cutoff
  payload$ridge_movement_cutoff_se <- .gs_v282t_ridge_movement_cutoff
  payload$ridge_movement_status <-
    "unavailable_pending_full_target_and_interval_endpoint_mapping"
  payload$derivative_refinement_tolerance <- 1e-4
  payload$analytic_derivatives_only <- TRUE
  payload$common_family_multiplier_draws <- .gs_v282t_family_draws
  payload$reporting_label <- .gs_v282t_supported_status
  payload$formal_release <- FALSE
  .gs_v282t_save_atomic(payload, path, binding)
  message("Created immutable family checkpoint without printing estimates: ",
          family_id)
  invisible(path)
}

.gs_v282t_export_stage <- function(repo_root, protocol, v28_protocol) {
  linked <- .gs_v282t_load_binding(repo_root, protocol, v28_protocol)
  output_dir <- .gs_v282t_output_dir(repo_root)
  path <- .gs_v282t_checkpoint_path(output_dir, "export")
  binding <- .gs_v282t_binding(protocol, "export", "all",
                              linked$upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable target export/manifest.")
    return(invisible(path))
  }
  family_objects <- lapply(protocol$families$family_id, function(family_id) {
    family_path <- .gs_v282t_checkpoint_path(output_dir, "family", family_id)
    family_binding <- .gs_v282t_binding(protocol, "family", family_id,
                                       linked$upstream$hashes)
    .gs_v282t_read_checkpoint(family_path, binding = family_binding)
  })
  names(family_objects) <- protocol$families$family_id
  results <- do.call(rbind, lapply(family_objects, function(x) {
    z <- x$payload$summary
    z$multiplicity_family <- x$payload$family_id
    z
  }))
  results <- results[order(as.integer(results$target_index)), , drop = FALSE]
  expected <- protocol$targets[order(as.integer(protocol$targets$target_index)),
                               , drop = FALSE]
  if (nrow(results) != nrow(expected) ||
      !identical(results$target_id, expected$target_id) ||
      anyDuplicated(results$target_id) || any(results$formal_release)) {
    stop("Target export does not exactly cover the five-target v2.8.2 core.",
         call. = FALSE)
  }
  registry_columns <- c("target_id", "reporting_tier", "orientation",
                        "standardization", "tie_rule", "primitive_bundle")
  results <- merge(
    results, expected[, registry_columns, drop = FALSE],
    by = "target_id", all.x = TRUE, sort = FALSE)
  results <- results[match(expected$target_id, results$target_id), , drop = FALSE]
  manifest <- data.frame(
    artifact = c("binding", "basis", paste0("family/",
      protocol$families$family_id)),
    sha256 = c(
      .gs_v282t_sha256(.gs_v282t_checkpoint_path(output_dir, "binding")),
      .gs_v282t_sha256(.gs_v282t_checkpoint_path(output_dir, "basis")),
      vapply(protocol$families$family_id, function(family_id) {
        .gs_v282t_sha256(.gs_v282t_checkpoint_path(
          output_dir, "family", family_id))
      }, character(1L))),
    stringsAsFactors = FALSE)
  payload <- list(
    schema_version = "gs2020-v2.8.2-target-export-v1",
    results = results,
    availability = data.frame(
      target_id = results$target_id, family_id = results$multiplicity_family,
      status = results$status, formal_release = FALSE,
      reason = results$reason, stringsAsFactors = FALSE),
    artifact_manifest = manifest,
    target_count = nrow(results), represented_target_count =
      sum(results$status != .gs_v282t_unavailable_status),
    unavailable_target_count =
      sum(results$status == .gs_v282t_unavailable_status),
    family_count = length(family_objects),
    upstream_hashes = linked$upstream$hashes,
    protocol_hash = protocol$protocol_hash,
    q2_used_for_target_selection = FALSE,
    common_multiplier_rule =
      "9,999 independent standard-normal respondent multipliers shared within each core family",
    interval_status = paste(
      "diagnostic delta-method pointwise and type-8 normal-multiplier ranges",
      "only; not range-respecting formal confidence sets"),
    reporting_label = .gs_v282t_supported_status,
    formal_release = FALSE,
    formal_release_reason = paste(
      "The manuscript core was selected after earlier same-sample results, and",
      "signed inner-nuisance refits, target-level integration, transformed-Riesz,",
      "ridge-neighborhood, regularity, and nonlinear confidence-set evidence",
      "remain unavailable. Same-sample intervals are descriptive and conditional",
      "even if every numerical gate later passes."))
  .gs_v282t_save_atomic(payload, path, binding)
  message("Created immutable five-target core export without printing values.")
  invisible(path)
}

.gs_v282t_main <- function() {
  repo_root <- .gs_v282t_repo_root_from_script()
  cli <- .gs_v282t_parse_cli(commandArgs(trailingOnly = TRUE))
  allowed <- c("status", "bind", "basis", "family", "all-families", "export")
  if (!cli$command %in% allowed) stop("Unknown command: ", cli$command,
                                      call. = FALSE)
  outcome_command <- cli$command != "status"
  if (outcome_command) .gs_v282t_bootstrap_guard(repo_root)
  .gs_v282t_load_code(repo_root)
  .gs_v282t_one_thread()
  protocol <- .gs_v282t_validate_protocol(
    repo_root, require_clean = outcome_command)
  if (cli$command == "status") {
    print(.gs_v282t_status(repo_root, protocol), row.names = FALSE)
    return(invisible(TRUE))
  }
  source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
  if (!nzchar(source_root)) {
    stop("Set CONJOINTSTRUCTURAL_ROOT to the shared ConjointStructural root.",
         call. = FALSE)
  }
  v28_protocol <- .gs_v282t_parent_protocol(
    repo_root, source_root, require_clean = TRUE)
  if (cli$command == "bind") {
    .gs_v282t_bind_stage(repo_root, protocol, v28_protocol)
  } else if (cli$command == "basis") {
    .gs_v282t_basis_stage(repo_root, protocol, v28_protocol)
  } else if (cli$command == "family") {
    if (!nzchar(cli$family)) stop("family requires --family=ID.", call. = FALSE)
    .gs_v282t_family_stage(repo_root, protocol, v28_protocol, cli$family)
  } else if (cli$command == "all-families") {
    for (family_id in protocol$families$family_id) {
      .gs_v282t_family_stage(repo_root, protocol, v28_protocol, family_id)
    }
  } else if (cli$command == "export") {
    .gs_v282t_export_stage(repo_root, protocol, v28_protocol)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) .gs_v282t_main()
