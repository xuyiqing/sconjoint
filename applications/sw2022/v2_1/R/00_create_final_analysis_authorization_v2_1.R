#!/usr/bin/env Rscript

## One-time, versioned authorization creator for the independently audited
## Saha--Weeks v2.1 post-pilot final-analysis runner. This script does not fit
## a model. It reconstructs the runner's exact generation context, writes one
## authorization atomically, and immediately validates the on-disk copy.

options(stringsAsFactors = FALSE, warn = 1)

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

creator_path <- .script_file()
root <- normalizePath(file.path(dirname(creator_path), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
config_path <- file.path(app, "v2_1", "config", "analysis_config_v2_1.R")
runner_path <- file.path(app, "v2_1", "R",
                         "04_fit_postpilot_final_v2_1.R")
contract_path <- file.path(app, "v2_1", "R",
                           "postpilot_contract_v2_1.R")
predecessor_config_path <- file.path(
  app, "v2", "config", "analysis_config_v2.R")
provenance_path <- file.path(root, "R", "provenance.R")

source(config_path, local = FALSE)
predecessor_env <- new.env(parent = baseenv())
sys.source(predecessor_config_path, envir = predecessor_env)
predecessor_config <- predecessor_env$sw_v2_config
source(provenance_path, local = FALSE)
source(contract_path, local = FALSE)

pilot_manifest_path <- sw_v21_config$input$failed_v2_pilot_manifest
pilot_manifest <- if (file.exists(pilot_manifest_path)) {
  tryCatch(readRDS(pilot_manifest_path), error = function(e) NULL)
} else NULL
if (!is.list(pilot_manifest) || !is.character(pilot_manifest$artifacts)) {
  stop("The reviewed failed-pilot manifest is missing or malformed.",
       call. = FALSE)
}
package_sources <- sort(list.files(
  file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
names(package_sources) <- paste0("package_source:", basename(package_sources))
pilot_artifact_paths <- file.path(
  dirname(pilot_manifest_path), names(pilot_manifest$artifacts))
names(pilot_artifact_paths) <- paste0(
  "failed_pilot_artifact:", names(pilot_manifest$artifacts))

## This order and naming exactly reproduce `input_paths` in the fit runner.
input_paths <- c(
  prepared = sw_v21_config$input$prepared,
  v1_nested = sw_v21_config$input$v1_nested,
  v1_party_diagnostic = sw_v21_config$input$v1_party_diagnostic,
  postpilot_config = config_path, postpilot_runner = runner_path,
  authorization_creator = creator_path,
  predecessor_config = predecessor_config_path,
  package_description = file.path(root, "DESCRIPTION"),
  package_namespace = file.path(root, "NAMESPACE"),
  launcher_R45 = file.path(root, "applications", "bin", "R45"),
  launcher_Rscript45 = file.path(root, "applications", "bin", "Rscript45"),
  postpilot_contract = contract_path,
  failed_pilot_manifest = pilot_manifest_path,
  package_sources, pilot_artifact_paths)
if (any(!file.exists(input_paths))) {
  stop("Missing authorization input(s): ",
       paste(names(input_paths)[!file.exists(input_paths)], collapse = ", "),
       call. = FALSE)
}
generation_md5 <- .sc_md5_paths(input_paths)
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
runtime_signature <- .sc_runtime_signature(
  input_paths[["package_description"]])
if (!.sw_v21_prepilot_spec_unchanged(
    predecessor_config, sw_v21_config)) {
  stop("The v2.1 grid or computation schedule differs from the reviewed pre-pilot specification.",
       call. = FALSE)
}
if (!.sw_v21_failed_pilot_valid(
    pilot_manifest, pilot_manifest_path, runtime_signature,
    sw_v21_config$predecessor)) {
  stop("The reviewed failed pilot, its inputs, or its artifacts are stale.",
       call. = FALSE)
}

authorization <- list(
  schema_version = "sw2022-v2.1-final-analysis-authorization-v1",
  authorized = TRUE,
  purpose = "sw2022-v2.1-postpilot-final-analysis",
  reviewed_by = "Codex primary agent after independent audit",
  authorized_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  acknowledged_postpilot_outcome_informed = TRUE,
  acknowledged_formal_inference_unavailable = TRUE,
  acknowledged_failed_pilot_not_rewritten = TRUE,
  config_version = sw_v21_config$version,
  config_md5 = unname(tools::md5sum(config_path)),
  predecessor_config_md5 =
    unname(tools::md5sum(predecessor_config_path)),
  reviewed_failed_pilot_manifest_md5 =
    unname(tools::md5sum(pilot_manifest_path)),
  reviewed_failed_pilot_generation_input_md5 =
    pilot_manifest$generation_input_md5,
  reviewed_failed_pilot_artifact_md5 = pilot_manifest$artifacts,
  postpilot_generation_input_md5 = generation_md5,
  runtime_signature = runtime_signature,
  noninferiority_margin =
    sw_v21_config$postpilot_guardrail$noninferiority_margin,
  formal_inference_available = FALSE,
  outcome_blind = FALSE,
  authorization_creator_md5 = generation_md5[["authorization_creator"]])

if (!.sw_v21_authorization_valid(
    authorization, sw_v21_config, config_path, predecessor_config_path,
    pilot_manifest, pilot_manifest_path, generation_md5,
    runtime_signature)) {
  stop("The in-memory authorization failed its contract before writing.",
       call. = FALSE)
}

authorization_path <- sw_v21_config$authorization_file
if (file.exists(authorization_path)) {
  stop("Authorization already exists; refusing to overwrite: ",
       authorization_path, call. = FALSE)
}
dir.create(dirname(authorization_path), recursive = TRUE,
           showWarnings = FALSE)
tmp <- tempfile(paste0(".", basename(authorization_path), "-"),
                tmpdir = dirname(authorization_path))
on.exit(unlink(tmp), add = TRUE)
saveRDS(authorization, tmp, version = 3, compress = "xz")
if (!file.rename(tmp, authorization_path)) {
  stop("Could not atomically install the authorization.", call. = FALSE)
}

on_disk <- tryCatch(readRDS(authorization_path), error = function(e) NULL)
generation_md5_after <- .sc_md5_paths(input_paths)
self_valid <- .sw_v21_authorization_valid(
  on_disk, sw_v21_config, config_path, predecessor_config_path,
  pilot_manifest, pilot_manifest_path, generation_md5,
  runtime_signature) &&
  .sc_identical_md5_vectors(generation_md5, generation_md5_after) &&
  .sw_v21_failed_pilot_valid(
    readRDS(pilot_manifest_path), pilot_manifest_path, runtime_signature,
    sw_v21_config$predecessor)
if (!self_valid) {
  stop("The on-disk authorization failed immediate self-validation.",
       call. = FALSE)
}

cat("authorization_path=", normalizePath(authorization_path, mustWork = TRUE),
    "\nauthorization_md5=", unname(tools::md5sum(authorization_path)),
    "\ngeneration_inputs=", length(generation_md5),
    "\nself_valid=TRUE\n", sep = "")
