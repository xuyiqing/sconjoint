#!/usr/bin/env Rscript

## Diagnostic orthogonal inference for br2017.
## Thin wrapper around applications/R/inference_common.R.
##
## Usage from the package root:
##   Rscript applications/br2017/R/04_inference.R --profile=production_amended

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

parse_cli <- function(args) {
  out <- list(profile = "production_amended", force = FALSE)
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
source(file.path(root, "applications", "R", "inference_common.R"),
       local = FALSE)
