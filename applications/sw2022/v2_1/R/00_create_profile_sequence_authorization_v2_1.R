#!/usr/bin/env Rscript

## Create (but do not execute) the reviewed authorization for the add-only
## Saha--Weeks v2.1 descriptive penalized-criterion profile-sequence runner.

options(stringsAsFactors = FALSE, warn = 1)

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  if (length(x) != 1L || !grepl("^--reviewed-by=", x)) {
    stop("Supply exactly one nonempty --reviewed-by=NAME value.",
         call. = FALSE)
  }
  reviewer <- trimws(sub("^--reviewed-by=", "", x))
  if (!nzchar(reviewer)) stop("Reviewer name cannot be empty.", call. = FALSE)
  reviewer
}

reviewer <- .parse_cli(commandArgs(trailingOnly = TRUE))
creator_path <- .script_file()
root <- normalizePath(file.path(dirname(creator_path), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
config_path <- file.path(
  app, "v2_1", "config", "profile_sequence_config_v2_1.R")
helper_path <- file.path(
  app, "v2_1", "R", "profile_sequence_helpers_v2_1.R")
runner_path <- file.path(
  app, "v2_1", "R", "06_profile_sequence_likelihoods_v2_1.R")
primary_contract_path <- file.path(
  app, "v2_1", "sensitivity", "R", "reported_primary_contract_v2_1.R")
parent_config_path <- file.path(
  app, "v2_1", "config", "analysis_config_v2_1.R")
postfit_config_path <- file.path(
  app, "v2_1", "config", "postfit_evidence_config_v2_1.R")
source(config_path, local = FALSE)
source(helper_path, local = FALSE)
source(primary_contract_path, local = FALSE)
source(parent_config_path, local = FALSE)
source(postfit_config_path, local = FALSE)
source(file.path(root, "R", "provenance.R"), local = FALSE)

if (file.exists(sw_v21_profile_config$authorization_file)) {
  stop("The profile authorization already exists and will not be overwritten.",
       call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
context <- .sw_v21_validate_reported_primary(
  sw_v21_profile_config$input$reported_primary_pointer,
  sw_v21_config, load_fits = TRUE)
if (!identical(context$pointer$reported_primary,
               sw_v21_profile_config$fixed_fit$require_reported_primary)) {
  stop("The reviewed primary is not the required selected q=1 learner.",
       call. = FALSE)
}
generation_paths <- .sw_v21_profile_generation_paths(
  root, app, config_path, helper_path, runner_path, creator_path,
  primary_contract_path, parent_config_path, postfit_config_path, context)
generation_md5 <- .sc_md5_paths(generation_paths)
runtime_signature <- .sc_runtime_signature(file.path(root, "DESCRIPTION"))
if (!identical(runtime_signature, context$pointer$runtime_signature)) {
  stop("The current runtime differs from the reported-primary runtime.",
       call. = FALSE)
}

authorization <- list(
  schema_version = "sw2022-v2.1-profile-sequence-authorization-v2",
  authorized = TRUE,
  purpose = "sw2022-v2.1-descriptive-penalized-criterion-profile-sequences",
  reviewed_by = reviewer,
  authorized_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  config_version = sw_v21_profile_config$version,
  config_md5 = unname(tools::md5sum(config_path)),
  generation_input_paths = generation_paths,
  generation_input_md5 = generation_md5,
  runtime_signature = runtime_signature,
  reviewed_pointer_md5 = unname(tools::md5sum(context$pointer_path)),
  reviewed_manifest_md5 = unname(tools::md5sum(file.path(
    dirname(context$pointer_path), "manifest.rds"))),
  reported_primary_lock_md5 = context$lock_md5,
  reported_primary = context$pointer$reported_primary,
  acknowledged_outcome_informed = TRUE,
  acknowledged_descriptive_penalized_criterion_sequences = TRUE,
  acknowledged_formal_inference_unavailable = TRUE,
  acknowledged_no_lr_critical_values = TRUE,
  acknowledged_fixed_learner_tuning_sieve = TRUE,
  formal_inference_available = FALSE,
  outcome_blind = FALSE)

if (!.sw_v21_profile_authorization_valid(
    authorization, sw_v21_profile_config, config_path, generation_md5,
    runtime_signature, context)) {
  stop("The proposed profile authorization failed validation.",
       call. = FALSE)
}
.sw_v21_profile_atomic_save(
  authorization, sw_v21_profile_config$authorization_file,
  portable = FALSE)
on_disk <- readRDS(sw_v21_profile_config$authorization_file)
if (!.sw_v21_profile_authorization_valid(
    on_disk, sw_v21_profile_config, config_path, generation_md5,
    runtime_signature, context)) {
  stop("The on-disk profile authorization failed validation.",
       call. = FALSE)
}
cat("Profile-sequence authorization created; no fit was launched:\n",
    sw_v21_profile_config$authorization_file, "\n", sep = "")
