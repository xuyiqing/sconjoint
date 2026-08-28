#!/usr/bin/env Rscript

## Design/completion audit for br2017.
## Thin wrapper around applications/R/design_audit_common.R.

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
app_name <- "br2017"
app_root <- file.path(root, "applications", app_name)
options(sconjoint.br_application_root = app_root)
source(file.path(app_root, "config", "analysis_config.R"), local = FALSE)
cfg <- br_analysis_config
source(file.path(root, "applications", "R", "design_audit_common.R"),
       local = FALSE)
