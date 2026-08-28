#!/usr/bin/env Rscript

## Create the separately reviewed execution authorization for the downstream
## v2.1 rank/numerical diagnostics. This script never starts a fit.

options(stringsAsFactors = FALSE, warn = 1)

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(args) {
  out <- list(
    reviewer = "",
    acknowledge_outcome_informed = FALSE,
    acknowledge_no_formal_inference = FALSE,
    acknowledge_no_rank_selection = FALSE)
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", bits[[1L]], fixed = TRUE)
    if (!key %in% names(out)) stop("Unknown argument --", bits[[1L]])
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  for (key in setdiff(names(out), "reviewer")) {
    out[[key]] <- tolower(as.character(out[[key]])) %in%
      c("1", "true", "yes")
  }
  out
}

.atomic_save <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write ", path, call. = FALSE)
  }
  invisible(path)
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
if (!nzchar(trimws(cli$reviewer)) ||
    !cli$acknowledge_outcome_informed ||
    !cli$acknowledge_no_formal_inference ||
    !cli$acknowledge_no_rank_selection) {
  stop(
    "Authorization requires a reviewer and all three explicit acknowledgements; no authorization was written.",
    call. = FALSE)
}

root <- normalizePath(file.path(dirname(.script_file()), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
rank_config_path <- file.path(
  app, "v2_1", "config", "rank_numerical_config_v2_1.R")
rank_contract_path <- file.path(
  app, "v2_1", "R", "rank_numerical_contract_v2_1.R")
rank_runner_path <- file.path(
  app, "v2_1", "R", "05_rank_numerical_v2_1.R")
parent_config_path <- file.path(
  app, "v2_1", "config", "analysis_config_v2_1.R")
parent_contract_path <- file.path(
  app, "v2_1", "R", "postpilot_contract_v2_1.R")
parent_runner_path <- file.path(
  app, "v2_1", "R", "04_fit_postpilot_final_v2_1.R")
parent_authorization_creator_path <- file.path(
  app, "v2_1", "R", "00_create_final_analysis_authorization_v2_1.R")
source(file.path(root, "R", "provenance.R"), local = FALSE)
source(parent_config_path, local = FALSE)
source(rank_config_path, local = FALSE)
source(rank_contract_path, local = FALSE)

if (!identical(sw_v21_rank_config$parent_version, sw_v21_config$version) ||
    !identical(sw_v21_rank_config$formal_inference_available, FALSE) ||
    !identical(sw_v21_rank_config$rank_selected, FALSE)) {
  stop("The downstream configuration does not match the frozen parent.",
       call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
runtime_signature <- .sc_runtime_signature(file.path(root, "DESCRIPTION"))
manifest_path <- file.path(sw_v21_config$output_root, "manifest.rds")
pointer_path <- file.path(sw_v21_config$output_root,
                          "reported_primary_pointer.rds")
if (!file.exists(manifest_path) || !file.exists(pointer_path)) {
  stop("The completed v2.1 final manifest and pointer are required; no authorization was written.",
       call. = FALSE)
}
manifest <- readRDS(manifest_path)
pointer <- readRDS(pointer_path)
if (!.sw_v21_rank_final_bundle_valid(
    pointer, pointer_path, manifest, manifest_path, sw_v21_config,
    runtime_signature)) {
  stop("The v2.1 reported-primary pointer or final manifest failed validation; no authorization was written.",
       call. = FALSE)
}
generation_paths <- .sw_v21_rank_generation_paths(
  root = root, app = app,
  rank_config_path = rank_config_path,
  rank_contract_path = rank_contract_path,
  rank_runner_path = rank_runner_path,
  authorization_creator_path = .script_file(),
  parent_config_path = parent_config_path,
  parent_contract_path = parent_contract_path,
  parent_runner_path = parent_runner_path,
  parent_authorization_creator_path = parent_authorization_creator_path,
  pointer_path = pointer_path, manifest_path = manifest_path,
  manifest = manifest)
generation_md5 <- .sc_md5_paths(generation_paths)

authorization <- list(
  schema_version = "sw2022-v2.1-rank-numerical-authorization-v1",
  authorized = TRUE,
  purpose = "sw2022-v2.1-rank-numerical-diagnostics",
  reviewed_by = trimws(cli$reviewer),
  authorized_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  acknowledged_outcome_informed = TRUE,
  acknowledged_formal_inference_unavailable = TRUE,
  acknowledged_no_rank_selection = TRUE,
  config_version = sw_v21_rank_config$version,
  config_md5 = unname(tools::md5sum(rank_config_path)),
  generation_input_md5 = generation_md5,
  runtime_signature = runtime_signature,
  reviewed_pointer_md5 = unname(tools::md5sum(pointer_path)),
  reviewed_final_manifest_md5 = unname(tools::md5sum(manifest_path)),
  reported_primary = pointer$reported_primary,
  fallback_applied = isTRUE(pointer$fallback_applied),
  formal_inference_available = FALSE,
  rank_selected = FALSE,
  outcome_blind = FALSE)
if (!.sc_identical_md5_vectors(
      generation_md5, .sc_md5_paths(generation_paths)) ||
    !.sw_v21_rank_final_bundle_valid(
      readRDS(pointer_path), pointer_path, readRDS(manifest_path),
      manifest_path, sw_v21_config, runtime_signature)) {
  stop("A reviewed source, input, or parent artifact changed while authorization was being prepared; no authorization was written.",
       call. = FALSE)
}
.atomic_save(authorization, sw_v21_rank_config$authorization_file)
cat("Rank/numerical authorization written: ",
    sw_v21_rank_config$authorization_file, "\n", sep = "")
