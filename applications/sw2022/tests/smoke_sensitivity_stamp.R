#!/usr/bin/env Rscript

## Verify that changing only the design audit invalidates the checkpoint stamp.
## The test operates on a temporary copy and never rewrites analysis artifacts.

options(stringsAsFactors = FALSE)

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

script <- .script_file()
app <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
source(file.path(app, "sensitivity", "07_run_sensitivities.R"))

profile_arg <- grep("^--profile=", commandArgs(trailingOnly = TRUE),
                    value = TRUE)
profile <- if (length(profile_arg)) {
  sub("^--profile=", "", profile_arg[[1L]])
} else "smoke"
fit_dir <- file.path(app, "results", "mixed_logit", profile)
paths <- list(
  prepared = file.path(app, "results", "prep_analysis_data.rds"),
  design = file.path(app, "results", "design_completion_audit.rds"),
  primary_full = file.path(fit_dir, "fit_primary_full.rds"),
  primary_nested = file.path(fit_dir, "fit_primary_nested.rds"),
  primary_assembled = file.path(fit_dir, "fit_primary_assembled.rds")
)
if (any(!file.exists(unlist(paths)))) {
  stop("Production inputs are required for the sensitivity-stamp smoke test.",
       call. = FALSE)
}
controls <- list(test = "metadata-only")
stamp <- .sensitivity_component_stamp(
  schema_version = "stamp-smoke-v1", config_version = "config-smoke-v1",
  profile = profile, controls = controls, paths = paths
)
component <- .stamp_sensitivity_component(list(payload = 1), stamp, "test")
stopifnot(.valid_sensitivity_component(component, stamp, "test"),
          identical(stamp$design_md5,
                    unname(tools::md5sum(paths$design))))

temporary_design <- tempfile("sw-design-stamp-", fileext = ".rds")
on.exit(unlink(temporary_design), add = TRUE)
changed_design <- readRDS(paths$design)
changed_design$stamp_smoke_change <- TRUE
saveRDS(changed_design, temporary_design, version = 3)
changed_paths <- paths
changed_paths$design <- temporary_design
changed_stamp <- .sensitivity_component_stamp(
  schema_version = "stamp-smoke-v1", config_version = "config-smoke-v1",
  profile = profile, controls = controls, paths = changed_paths
)
stopifnot(!identical(stamp$design_md5, changed_stamp$design_md5),
          !.valid_sensitivity_component(component, changed_stamp, "test"))

temporary_component <- tempfile("sw-component-stamp-", fileext = ".rds")
on.exit(unlink(temporary_component), add = TRUE)
saveRDS(component, temporary_component, version = 3)
expression_evaluated <- FALSE
stale_error <- tryCatch({
  .run_or_load(
    temporary_component, overwrite = FALSE,
    expr = {
      expression_evaluated <- TRUE
      list(unexpected = TRUE)
    },
    validator = function(x) .valid_sensitivity_component(
      x, changed_stamp, "test")
  )
  NULL
}, error = identity)
stopifnot(inherits(stale_error, "error"),
          grepl("stale analysis stamp", conditionMessage(stale_error),
                fixed = TRUE),
          identical(expression_evaluated, FALSE))

cat("Sensitivity cache stamp rejects a changed design audit.\n")
