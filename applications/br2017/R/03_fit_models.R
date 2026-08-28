#!/usr/bin/env Rscript

## Fit the low-rank normal mixed-logit models for Ballard-Rosa et al.
## Thin wrapper around applications/R/fit_models_common.R.
##
## Usage from the package root:
##   Rscript applications/br2017/R/03_fit_models.R --profile=production_amended --stage=all

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

parse_cli <- function(args) {
  out <- list(profile = "production_amended", stage = "all", force = FALSE)
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
app_name <- "br2017"
app_root <- file.path(root, "applications", app_name)
options(sconjoint.br_application_root = app_root)
source(file.path(app_root, "config", "analysis_config.R"), local = FALSE)
cfg <- br_analysis_config
cli <- parse_cli(commandArgs(trailingOnly = TRUE))
source(file.path(root, "applications", "R", "fit_models_common.R"),
       local = FALSE)
