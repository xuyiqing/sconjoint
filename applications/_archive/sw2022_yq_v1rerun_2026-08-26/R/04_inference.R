#!/usr/bin/env Rscript

## Diagnostic orthogonal inference for Saha--Weeks (reconstructed step 04).
##
## Avidit's backup carries 01, 02, 03, and 05 but not the script that produced
## `inference_diagnostic.rds`, so this file rebuilds that step from the
## package's exported inference surface. It calls scmix_dml() in DIAGNOSTIC
## mode (no verification record): the frozen config sets
## `enable_conditional_formal_inference = FALSE` and leaves the tangent
## arguments empty, so conditional formal inference is unavailable by
## construction. What this step decides is whether the Riesz-equation and
## ridge-sensitivity gates pass their prespecified tolerances.
##
## Usage from the package root:
##   Rscript applications/sw2022/R/04_inference.R --profile=production
##
## Reads  applications/sw2022/results/mixed_logit/<profile>/fit_primary_assembled.rds
## Writes applications/sw2022/results/mixed_logit/<profile>/inference_diagnostic.rds
##        applications/sw2022/results/mixed_logit/<profile>/inference_summary.csv

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

parse_cli <- function(args) {
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

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
app_root <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app_root)
source(file.path(app_root, "config", "analysis_config.R"), local = FALSE)

cli <- parse_cli(commandArgs(trailingOnly = TRUE))
fit_dir <- file.path(sw_analysis_config$output_root, cli$profile)
fit_path <- file.path(fit_dir, "fit_primary_assembled.rds")
if (!file.exists(fit_path)) {
  stop("Assembled primary fit not found: ", fit_path,
       ". Run 03_fit_models.R --stage=primary first.")
}
out_rds <- file.path(fit_dir, "inference_diagnostic.rds")
out_csv <- file.path(fit_dir, "inference_summary.csv")
if (file.exists(out_rds) && !cli$force) {
  stop("Output exists (use --force=true to overwrite): ", out_rds)
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The local analysis library must include pkgload.")
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

fit <- readRDS(fit_path)
prepared <- readRDS(sw_analysis_config$input$prepared)

## ---- Fitted-sieve tangent basis (extracted by 03 in the fit session) ----
## Torch modules do not survive saveRDS, so the basis is computed by
## 03_fit_models.R while the fold networks are alive and read back here.
basis_path <- file.path(fit_dir, "inference_basis.rds")
if (!file.exists(basis_path)) {
  stop("inference_basis.rds not found in ", fit_dir,
       ". Rerun 03_fit_models.R --stage=primary in a fresh session; the ",
       "basis can only be extracted while the fold networks are live.")
}
basis_obj <- readRDS(basis_path)
if (!identical(basis_obj$analysis_signature, fit$analysis_signature)) {
  stop("inference_basis.rds does not match the assembled fit's ",
       "analysis_signature; rerun 03_fit_models.R with --force=true.")
}
mu_basis <- basis_obj$mu_basis
respondent_id <- as.character(fit$respondent_id)
resp <- unique(respondent_id)
if (!identical(basis_obj$respondents, resp)) {
  stop("inference_basis.rds respondent order does not match the fit.")
}

## ---- Typed targets from the frozen config ----
inf_cfg <- sw_analysis_config$inference
gender_contrast <- as.numeric(colnames(as.matrix(fit$deltaX)) == "cand_genderMale")
names(gender_contrast) <- colnames(as.matrix(fit$deltaX))

party <- prepared$respondent_meta$party[
  match(resp, as.character(prepared$respondent_meta$respondent_id))]
if (anyNA(party)) stop("Could not align party labels to respondents.")
party_indicator <- function(level) {
  g <- as.numeric(party == level)
  names(g) <- resp
  g
}

targets <- list()
for (nm in names(sw_analysis_config$qoi$contests)) {
  targets[[paste0("contest_", nm)]] <- scmix_inference_target(
    type = "choice",
    contrast = sw_analysis_config$qoi$contests[[nm]],
    position_neutral = isTRUE(sw_analysis_config$qoi$contests_are_position_neutral),
    n_nodes = inf_cfg$choice_nodes,
    label = paste0("contest_", nm))
}
for (lv in c("Democrat", "Independent", "Republican (GOP)")) {
  short <- c("Democrat" = "dem", "Independent" = "ind",
             "Republican (GOP)" = "rep")[[lv]]
  targets[[paste0("gender_tau_", short)]] <- scmix_inference_target(
    type = "subgroup_tau_primitives",
    contrast = gender_contrast,
    subgroup = party_indicator(lv),
    label = paste0("gender_tau_", short))
}

dml_args <- list(
  fit = fit,
  targets = "theta",
  plugin_targets = targets,
  mu_basis = mu_basis,
  riesz_validation_fraction = inf_cfg$riesz_validation_fraction,
  riesz_equation_tolerance = inf_cfg$riesz_equation_tolerance,
  ridge_sensitivity_tolerance = inf_cfg$ridge_sensitivity_tolerance,
  active_eigenvalue_min = inf_cfg$active_eigenvalue_min,
  information_eigenvalue_min = inf_cfg$information_eigenvalue_min,
  rank_tolerance = inf_cfg$rank_tolerance,
  multiplier_draws = inf_cfg$multiplier_draws,
  multiplier = inf_cfg$multiplier,
  level = inf_cfg$level,
  seed = sw_analysis_config$optimizer$seed
)
main <- do.call(scmix_dml, dml_args)

## Sign-share target constructed fail-closed: below-floor directional variance
## errors at construction, so it runs in its own call.
sign_out <- tryCatch({
  sign_args <- dml_args
  sign_args$targets <- character()
  sign_args$plugin_targets <- list(
    gender_sign = scmix_inference_target(
      type = "sign", contrast = gender_contrast,
      variance_floor = inf_cfg$variance_floor,
      label = "gender_sign"))
  do.call(scmix_dml, sign_args)
}, error = function(e) list(status = "construction_failed",
                            reason = conditionMessage(e)))

riesz_max <- main$riesz_equation_max_relative_residual
ridge_max <- max(unlist(lapply(main$fold_details,
                               `[[`, "ridge_relative_sensitivity")))

result <- list(
  profile = cli$profile,
  analysis_signature = fit$analysis_signature,
  main = main,
  sign = sign_out,
  riesz_equation_max_relative_residual = riesz_max,
  ridge_relative_sensitivity_max = ridge_max,
  riesz_gate_pass = is.finite(riesz_max) &&
    riesz_max <= inf_cfg$riesz_equation_tolerance,
  ridge_gate_pass = is.finite(ridge_max) &&
    ridge_max <= inf_cfg$ridge_sensitivity_tolerance,
  config_version = sw_analysis_config$version,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
saveRDS(result, out_rds)

labels <- names(main$estimate)
summary_df <- data.frame(
  label = labels,
  estimate = as.numeric(main$estimate[labels]),
  plugin = as.numeric(main$plugin_estimate[labels]),
  se = as.numeric(main$se[labels]),
  diagnostic_se = as.numeric(main$diagnostic_se[labels]),
  ci_lower = as.numeric(main$ci_lower[labels]),
  ci_upper = as.numeric(main$ci_upper[labels])
)
summary_df$status <- main$status
summary_df$riesz_max <- riesz_max
summary_df$ridge_max <- ridge_max
utils::write.csv(summary_df, out_csv, row.names = FALSE)

message(sprintf(
  "04 inference [%s]: status=%s riesz_max=%.4g (tol %.3g, pass=%s) ridge_max=%.4g (tol %.3g, pass=%s) sign=%s",
  cli$profile, main$status,
  riesz_max, inf_cfg$riesz_equation_tolerance, result$riesz_gate_pass,
  ridge_max, inf_cfg$ridge_sensitivity_tolerance, result$ridge_gate_pass,
  if (is.list(sign_out) && !is.null(sign_out$status)) sign_out$status else class(sign_out)[1]))
