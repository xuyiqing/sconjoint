#!/usr/bin/env Rscript

## Supplement a legacy full-sample selected fit with a durable Torch network
## state. This script never overwrites the primary, nested, assembled, QOI, or
## assessment artifacts. It refits only the already selected full-sample
## specification; it does not rerun candidate selection or nested CV.

options(stringsAsFactors = FALSE, warn = 1)

.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(.script_arg)) stop("Run this file with Rscript.", call. = FALSE)
.script <- normalizePath(sub("^--file=", "", .script_arg[[1L]]),
                         mustWork = TRUE)
.project <- normalizePath(file.path(dirname(.script), "../../.."),
                          mustWork = TRUE)
.app <- file.path(.project, "applications", "sw2022")
options(sconjoint.sw_application_root = .app)
source(file.path(.app, "config", "analysis_config.R"), local = FALSE)

.parse_cli <- function(args) {
  out <- list(profile = "production", force = FALSE)
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- bits[[1L]]
    if (!key %in% names(out)) stop("Unknown argument --", key)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out
}
.atomic_save <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path)
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
if (!cli$profile %in% names(sw_analysis_config$profiles)) {
  stop("--profile must be one of: ",
       paste(names(sw_analysis_config$profiles), collapse = ", "))
}
profile <- sw_analysis_config$profiles[[cli$profile]]
fit_dir <- file.path(sw_analysis_config$output_root, cli$profile)
source_path <- file.path(fit_dir, "fit_primary_full.rds")
if (!file.exists(source_path)) {
  stop("Missing ", source_path, "; run 03_fit_models.R first.")
}
output_dir <- file.path(fit_dir, "network_state")
output_path <- file.path(output_dir, "fit_primary_full_network_supplement.rds")
if (file.exists(output_path) && !cli$force) {
  existing <- readRDS(output_path)
  if (inherits(existing$network_state, "scmix_network_state") &&
      identical(existing$source_fit_md5,
                unname(tools::md5sum(source_path)))) {
    message("checkpoint: valid network-state supplement already exists: ",
            output_path)
    quit(save = "no", status = 0L)
  }
  stop("A stale supplement exists. Inspect it, then rerun with --force=true.")
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The local analysis library must include pkgload.")
}
suppressPackageStartupMessages(pkgload::load_all(.project, quiet = TRUE))
source_fit <- readRDS(source_path)
stamp <- source_fit$sw_application_specification
if (!is.list(stamp) ||
    !identical(stamp$config_version, sw_analysis_config$version) ||
    !identical(stamp$profile, cli$profile) ||
    !identical(stamp$role, "primary_full")) {
  stop("The source full fit does not match the active application config.")
}
if (!inherits(source_fit, "scmix_tuning") ||
    !inherits(source_fit$refit, "scmix_tuned_matrix_fit")) {
  stop("The source artifact lacks a selected full-sample matrix refit.")
}
spec <- source_fit$selected
old <- source_fit$refit
source_family <- if (is.null(spec$mean_family)) "legacy" else spec$mean_family
if (!identical(source_family, "legacy")) {
  stop("This supplement is only for the legacy v1 fit. Current corrected-",
       "family fits must already carry a portable network_state.")
}
raw <- old$raw_data
needed <- c("deltaX", "y", "Z", "respondent_id")
if (!is.list(raw) || !all(needed %in% names(raw))) {
  stop("The source refit lacks the raw matrix inputs needed for a fixed refit.")
}
base_seed <- sw_analysis_config$optimizer$seed + 1000L * as.integer(spec$q)
fresh <- scmix_refit_selected_matrix(
  deltaX = raw$deltaX, y = raw$y, Z = raw$Z,
  respondent_id = raw$respondent_id, specification = spec,
  integration_grid = old$integration_grid,
  preprocessing = old$preprocessing,
  n_epochs = profile$n_epochs, learning_rate = profile$learning_rate,
  n_starts = profile$n_starts,
  mu_bound = sw_analysis_config$optimizer$mu_bound,
  kappa_bound = sw_analysis_config$optimizer$kappa_bound,
  a_bound = sw_analysis_config$optimizer$a_bound,
  weight_bound = sw_analysis_config$optimizer$weight_bound,
  opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
  seed = base_seed, device = sw_analysis_config$optimizer$device,
  verbose = FALSE, source_analysis_signature = source_fit$analysis_signature)

portable_prediction <- scmix_predict_network(fresh$network_state, raw$Z)
roundtrip_error <- max(abs(portable_prediction - fresh$mu))
if (!is.finite(roundtrip_error) || roundtrip_error > 1e-6) {
  stop("Portable network round-trip failed (max error = ",
       signif(roundtrip_error, 6), ").")
}
comparison <- data.frame(
  metric = c("mu_max_abs", "Sigma_max_abs", "kappa_abs",
             "penalized_objective_abs", "portable_prediction_max_abs"),
  difference = c(
    max(abs(fresh$mu - old$mu)),
    max(abs(fresh$Sigma - old$Sigma)),
    abs(fresh$kappa - old$kappa),
    abs(fresh$optimization$objective - old$optimization$objective),
    roundtrip_error),
  tolerance = c(1e-5, 1e-5, 1e-6, 1e-6, 1e-6),
  stringsAsFactors = FALSE)
comparison$pass <- comparison$difference <= comparison$tolerance
matches_source <- all(comparison$pass)

supplement <- list(
  status = if (matches_source) "portable_state_reproduces_source_fit" else
    "fresh_selected_refit_differs_from_source_fit",
  network_state = fresh$network_state,
  fresh_refit = list(
    mu = fresh$mu, A = fresh$A, Sigma = fresh$Sigma, kappa = fresh$kappa,
    specification = fresh$specification,
    integration_grid = fresh$integration_grid,
    preprocessing = fresh$preprocessing,
    optimization = fresh$optimization,
    analysis_signature = fresh$analysis_signature,
    refit_analysis_signature = fresh$refit_analysis_signature),
  source_fit_path = source_path,
  source_fit_md5 = unname(tools::md5sum(source_path)),
  source_analysis_signature = source_fit$analysis_signature,
  source_selected_specification = source_fit$selected,
  comparison = comparison,
  matches_source_fit_within_tolerance = matches_source,
  retuning_performed = FALSE,
  upstream_artifacts_modified = FALSE,
  use_note = if (matches_source) paste(
    "The portable predictor reproduces a deterministic fresh refit and that",
    "refit matches the stored source quantities within declared tolerances.")
  else paste(
    "The original nn_module state is unrecoverable. This is a fresh fixed",
    "selected-specification refit and must not be substituted silently for",
    "the stored source QOIs or cross-fitted nuisance quantities."),
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  session_info = utils::capture.output(sessionInfo())
)
.atomic_save(supplement, output_path)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(comparison,
                 file.path(output_dir, "fit_primary_full_network_comparison.csv"),
                 row.names = FALSE)
message("wrote separate network-state supplement: ", output_path)
if (!matches_source) {
  warning("Fresh selected refit did not reproduce every stored source metric; ",
          "the supplement is labeled accordingly and no upstream artifact ",
          "was changed.", call. = FALSE)
}
