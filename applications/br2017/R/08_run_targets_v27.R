#!/usr/bin/env Rscript

## Immutable Ballard--Rosa v2.7 q=1 target-stage runner.
##
## This is deliberately separate from 07_run_fast_v27.R.  It never modifies
## fit, assembly, refinement, or rank-comparison checkpoints.  It reads only
## the hash-bound v2.7 q=1 assembly and response-free preflight.  It does not
## inspect q=2 or use rank comparisons to select targets.
##
## Examples, after committing the protocol and implementation:
##   applications/bin/Rscript45 applications/br2017/R/08_run_targets_v27.R status
##   applications/bin/Rscript45 applications/br2017/R/08_run_targets_v27.R bind
##   applications/bin/Rscript45 applications/br2017/R/08_run_targets_v27.R basis
##   applications/bin/Rscript45 applications/br2017/R/08_run_targets_v27.R family --family=directional_shares
##   applications/bin/Rscript45 applications/br2017/R/08_run_targets_v27.R export

options(stringsAsFactors = FALSE, warn = 1)

.br_v27t_script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/",
                mustWork = TRUE)
}

.br_v27t_repo_root_from_script <- function() {
  normalizePath(file.path(dirname(.br_v27t_script_path()), "..", "..", ".."),
                winslash = "/", mustWork = TRUE)
}

.br_v27t_load_code <- function(repo_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The `pkgload` package is required.", call. = FALSE)
  }
  pkgload::load_all(repo_root, quiet = TRUE, export_all = FALSE)
  sys.source(file.path(repo_root, "applications", "br2017", "R",
                       "br_v27_contract.R"), envir = .GlobalEnv)
  sys.source(file.path(repo_root, "applications", "R",
                       "targeted_q1_inference.R"), envir = .GlobalEnv)
  sys.source(file.path(repo_root, "applications", "br2017", "R",
                       "br_v27_targets_contract.R"), envir = .GlobalEnv)
  invisible(TRUE)
}

.br_v27t_parse_cli <- function(args) {
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

.br_v27t_one_thread <- function() {
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
             OPENBLAS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
             TORCH_NUM_THREADS = "1")
  if (requireNamespace("torch", quietly = TRUE)) {
    try(torch::torch_set_num_threads(1L), silent = TRUE)
    try(torch::torch_set_num_interop_threads(1L), silent = TRUE)
  }
  invisible(TRUE)
}

.br_v27t_parent_protocol <- function(repo_root, source_root, require_clean) {
  .br_v27_validate_protocol(
    repo_root, source_root = source_root, require_external = TRUE,
    require_clean = require_clean)
}

.br_v27t_bind_stage <- function(repo_root, protocol, v27_protocol) {
  output_dir <- .br_v27t_output_dir(repo_root)
  upstream <- .br_v27t_verify_upstream(repo_root, v27_protocol)
  path <- .br_v27t_checkpoint_path(output_dir, "binding")
  binding <- .br_v27t_binding(protocol, "binding", upstream = upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v27t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable target binding checkpoint.")
    return(invisible(path))
  }
  payload <- list(
    schema_version = "br2017-v2.7-target-binding-v1",
    upstream_hashes = upstream$hashes,
    upstream_paths = upstream$paths,
    parent_protocol_hash = v27_protocol$protocol_hash,
    parent_git_commit = upstream$hashes$parent_run_git_commit,
    frozen_registry_hashes = stats::setNames(
      protocol$frozen$sha256, protocol$frozen$dependency_id),
    target_registry_count = nrow(protocol$targets),
    family_registry_count = nrow(protocol$families),
    q1_only = TRUE,
    q2_used_for_target_selection = FALSE,
    historical_target_estimates_used = FALSE,
    reporting_status = .br_v27t_supported_status,
    formal_release = FALSE)
  .br_v27t_save_atomic(payload, path, binding)
  message("Created immutable q=1 target binding; no target values were computed.")
  invisible(path)
}

.br_v27t_basis_stage <- function(repo_root, protocol, v27_protocol) {
  linked <- .br_v27t_load_binding(repo_root, protocol, v27_protocol)
  output_dir <- .br_v27t_output_dir(repo_root)
  path <- .br_v27t_checkpoint_path(output_dir, "basis")
  binding <- .br_v27t_binding(protocol, "basis",
                              upstream = linked$upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v27t_read_checkpoint(path, binding = binding, payload = FALSE)
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
    schema_version = "br2017-v2.7-target-basis-v1",
    basis = basis,
    source_q1_assembled_sha256 = linked$upstream$hashes$q1_assembled_sha256,
    q2_used_for_target_selection = FALSE)
  .br_v27t_save_atomic(payload, path, binding)
  message("Created immutable q=1 target basis; no target values were printed.")
  invisible(path)
}

.br_v27t_family_stage <- function(repo_root, protocol, v27_protocol,
                                  family_id) {
  if (!family_id %in% protocol$families$family_id) {
    stop("Unknown --family=", family_id, call. = FALSE)
  }
  linked <- .br_v27t_load_binding(repo_root, protocol, v27_protocol)
  output_dir <- .br_v27t_output_dir(repo_root)
  basis_path <- .br_v27t_checkpoint_path(output_dir, "basis")
  basis_binding <- .br_v27t_binding(protocol, "basis",
                                    upstream = linked$upstream$hashes)
  basis <- .br_v27t_read_checkpoint(
    basis_path, binding = basis_binding)$payload$basis
  path <- .br_v27t_checkpoint_path(output_dir, "family", family_id)
  binding <- .br_v27t_binding(protocol, "family", family_id,
                              linked$upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v27t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable target-family checkpoint: ", family_id)
    return(invisible(path))
  }
  registries <- .br_v27t_registries(repo_root, protocol)
  fit <- linked$upstream$assembled$assembled
  bundle <- .br_v27t_build_family(
    family_id, fit, linked$upstream$preflight, registries)
  if (identical(bundle$status, .br_v27t_unavailable_status)) {
    payload <- .br_v27t_unavailable_family(bundle)
  } else {
    seed_row <- protocol$seeds[protocol$seeds$family_id == family_id, ,
                               drop = FALSE]
    if (nrow(seed_row) != 1L) stop("Missing family seed.", call. = FALSE)
    dml <- tryCatch(.br_v27t_run_dml_family(
      fit, basis$mu_basis, bundle$targets,
      seed = as.integer(seed_row$dml_seed[[1L]]),
      primitive_registry = bundle$primitive_registry),
      error = function(e) list(
        status = "withheld_dml_error",
        reason = conditionMessage(e)))
    fit_gate <- isTRUE(
      isTRUE(linked$upstream$assembled$v27_application_gate_pass) &&
        isTRUE(all(linked$upstream$assembled$fit_gate_by_scope)))
    payload <- .br_v27t_finalize_family(
      dml, bundle, fit_gate,
      multiplier_seed = as.integer(seed_row$multiplier_seed[[1L]]))
  }
  payload$target_protocol_id <- protocol$protocol_id
  payload$target_protocol_hash <- protocol$protocol_hash
  payload$source_q1_assembled_sha256 <-
    linked$upstream$hashes$q1_assembled_sha256
  payload$fixed_riesz_rule <- "fixed_n_train_minus_three_quarters"
  payload$fixed_numerical_rank_rule <- "dimension_epsilon"
  payload$riesz_cutoff <- .br_v27t_riesz_cutoff
  payload$ridge_cutoff <- .br_v27t_ridge_cutoff
  payload$analytic_derivatives_only <- TRUE
  payload$common_family_multiplier_draws <- .br_v27t_family_draws
  payload$reporting_label <- .br_v27t_supported_status
  payload$formal_release <- FALSE
  .br_v27t_save_atomic(payload, path, binding)
  message("Created immutable family checkpoint without printing estimates: ",
          family_id)
  invisible(path)
}

.br_v27t_export_stage <- function(repo_root, protocol, v27_protocol) {
  linked <- .br_v27t_load_binding(repo_root, protocol, v27_protocol)
  output_dir <- .br_v27t_output_dir(repo_root)
  path <- .br_v27t_checkpoint_path(output_dir, "export")
  binding <- .br_v27t_binding(protocol, "export", "all",
                              linked$upstream$hashes)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v27t_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable target export/manifest.")
    return(invisible(path))
  }
  family_objects <- lapply(protocol$families$family_id, function(family_id) {
    family_path <- .br_v27t_checkpoint_path(output_dir, "family", family_id)
    family_binding <- .br_v27t_binding(protocol, "family", family_id,
                                       linked$upstream$hashes)
    .br_v27t_read_checkpoint(family_path, binding = family_binding)
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
    stop("Target export does not exactly cover the frozen 72-target registry.",
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
      .br_v27t_sha256(.br_v27t_checkpoint_path(output_dir, "binding")),
      .br_v27t_sha256(.br_v27t_checkpoint_path(output_dir, "basis")),
      vapply(protocol$families$family_id, function(family_id) {
        .br_v27t_sha256(.br_v27t_checkpoint_path(
          output_dir, "family", family_id))
      }, character(1L))),
    stringsAsFactors = FALSE)
  payload <- list(
    schema_version = "br2017-v2.7-target-export-v1",
    results = results,
    availability = data.frame(
      target_id = results$target_id, family_id = results$multiplicity_family,
      status = results$status, formal_release = FALSE,
      reason = results$reason, stringsAsFactors = FALSE),
    artifact_manifest = manifest,
    target_count = nrow(results), represented_target_count =
      sum(results$status != .br_v27t_unavailable_status),
    unavailable_target_count =
      sum(results$status == .br_v27t_unavailable_status),
    family_count = length(family_objects),
    upstream_hashes = linked$upstream$hashes,
    protocol_hash = protocol$protocol_hash,
    q2_used_for_target_selection = FALSE,
    common_multiplier_rule =
      "9,999 independent standard-normal respondent multipliers shared within each frozen family",
    interval_status = paste(
      "diagnostic delta-method pointwise and type-8 normal-multiplier ranges",
      "only; not range-respecting formal confidence sets"),
    reporting_label = .br_v27t_supported_status,
    formal_release = FALSE,
    formal_release_reason = paste(
      "Confirmatory calibration and the full frozen target-level integration,",
      "near-optimum, and chart checks are not available in the rapid stage."))
  .br_v27t_save_atomic(payload, path, binding)
  message("Created immutable 72-target export/manifest without printing values.")
  invisible(path)
}

.br_v27t_main <- function() {
  repo_root <- .br_v27t_repo_root_from_script()
  .br_v27t_load_code(repo_root)
  .br_v27t_one_thread()
  cli <- .br_v27t_parse_cli(commandArgs(trailingOnly = TRUE))
  allowed <- c("status", "bind", "basis", "family", "all-families", "export")
  if (!cli$command %in% allowed) stop("Unknown command: ", cli$command,
                                      call. = FALSE)
  outcome_command <- cli$command != "status"
  protocol <- .br_v27t_validate_protocol(
    repo_root, require_clean = outcome_command)
  if (cli$command == "status") {
    print(.br_v27t_status(repo_root, protocol), row.names = FALSE)
    return(invisible(TRUE))
  }
  source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
  if (!nzchar(source_root)) {
    stop("Set CONJOINTSTRUCTURAL_ROOT to the shared ConjointStructural root.",
         call. = FALSE)
  }
  v27_protocol <- .br_v27t_parent_protocol(
    repo_root, source_root, require_clean = TRUE)
  if (cli$command == "bind") {
    .br_v27t_bind_stage(repo_root, protocol, v27_protocol)
  } else if (cli$command == "basis") {
    .br_v27t_basis_stage(repo_root, protocol, v27_protocol)
  } else if (cli$command == "family") {
    if (!nzchar(cli$family)) stop("family requires --family=ID.", call. = FALSE)
    .br_v27t_family_stage(repo_root, protocol, v27_protocol, cli$family)
  } else if (cli$command == "all-families") {
    for (family_id in protocol$families$family_id) {
      .br_v27t_family_stage(repo_root, protocol, v27_protocol, family_id)
    }
  } else if (cli$command == "export") {
    .br_v27t_export_stage(repo_root, protocol, v27_protocol)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) .br_v27t_main()
