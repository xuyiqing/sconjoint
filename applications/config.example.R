# Copy this file to `applications/config.local.R` (which should remain
# untracked) or set SCONJOINT_APPLICATION_ROOT in the process environment.

application_source_root <- Sys.getenv(
  "SCONJOINT_APPLICATION_ROOT",
  unset = "/path/to/ConjointStructural"
)

application_output_root <- file.path(
  normalizePath(".", mustWork = TRUE), "applications", "output"
)

if (!dir.exists(application_source_root)) {
  stop("Application source root does not exist: ", application_source_root)
}

# Analysis scripts must never write below application_source_root.
paths_are_separate <- !identical(
  normalizePath(application_source_root, mustWork = TRUE),
  normalizePath(application_output_root, mustWork = FALSE)
)
if (!paths_are_separate) {
  stop("Source and output roots must be different.")
}
