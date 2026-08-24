# Read-only adapters for the three paper applications.
#
# These helpers never write below `source_root`.  They return frozen prepared
# matrices with provenance flags; application-specific recoding is performed by
# later adapters and written only below `output_root`.

.scapp_normalize <- function(path, mustWork) {
  normalizePath(path, winslash = "/", mustWork = mustWork)
}

.scapp_is_within <- function(path, parent) {
  path <- paste0(.scapp_normalize(path, FALSE), "/")
  parent <- paste0(sub("/+$", "", .scapp_normalize(parent, TRUE)), "/")
  startsWith(path, parent)
}

scapp_paths <- function(
    source_root = Sys.getenv("SCONJOINT_APPLICATION_ROOT", unset = ""),
    output_root = file.path(getwd(), "applications", "output")) {
  if (!nzchar(source_root)) {
    stop("Set SCONJOINT_APPLICATION_ROOT or supply `source_root`.",
         call. = FALSE)
  }
  source_root <- .scapp_normalize(source_root, TRUE)
  output_root <- .scapp_normalize(output_root, FALSE)
  if (.scapp_is_within(output_root, source_root)) {
    stop("The application output root may not be inside the read-only source ",
         "project.", call. = FALSE)
  }
  list(source_root = source_root, output_root = output_root,
       source_policy = "read-only", output_policy = "local clone only")
}

.scapp_frozen_path <- function(application, source_root) {
  application <- match.arg(application, c("sw2022", "gs2020", "br2017"))
  file.path(source_root, "code", "analysis", application, "out",
            "prep_matrices.rds")
}

scapp_read_frozen <- function(application,
                              source_root = Sys.getenv(
                                "SCONJOINT_APPLICATION_ROOT", unset = "")) {
  paths <- scapp_paths(source_root)
  application <- match.arg(application, c("sw2022", "gs2020", "br2017"))
  input <- .scapp_frozen_path(application, paths$source_root)
  if (!file.exists(input)) {
    stop("Frozen prepared input not found: ", input, call. = FALSE)
  }
  object <- readRDS(input)
  if (application == "br2017") {
    if (is.null(object$DeltaX_fixed)) {
      stop("Ballard-Rosa frozen object lacks `DeltaX_fixed`; do not fall back ",
           "to the known-corrupted contrast matrix.", call. = FALSE)
    }
    object$DeltaX <- object$DeltaX_fixed
  }
  flags <- switch(
    application,
    sw2022 = c(
      "Completion and invalid-demographic exclusions must be reconstructed from raw preparation records."
    ),
    gs2020 = c(
      "Do not fit the structural model from this frozen DeltaX/Y pair until candidate-1 left/right orientation is rebuilt using c_onLeft.",
      "Candidate age and experience were manipulated but omitted from the legacy p=30 utility specification; include them or state the zero-effect restriction."
    ),
    br2017 = c(
      "DeltaX is explicitly aliased to DeltaX_fixed; DeltaX_corrupted must never be used."
    )
  )
  out <- list(application = application, data = object,
              provenance = list(
                source_file = input,
                source_md5 = unname(tools::md5sum(input)),
                source_root = paths$source_root,
                source_was_modified = FALSE,
                flags = flags
              ))
  class(out) <- c("scapp_frozen_input", "list")
  out
}

scapp_write_local <- function(object, name,
                              source_root = Sys.getenv(
                                "SCONJOINT_APPLICATION_ROOT", unset = ""),
                              output_root = file.path(getwd(), "applications",
                                                      "output"),
                              overwrite = FALSE) {
  paths <- scapp_paths(source_root, output_root)
  if (!is.character(name) || length(name) != 1L || !nzchar(name) ||
      grepl("[/\\\\]", name)) {
    stop("`name` must be one safe filename stem.", call. = FALSE)
  }
  dir.create(paths$output_root, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(paths$output_root, paste0(name, ".rds"))
  if (file.exists(destination) && !isTRUE(overwrite)) {
    stop("Local output already exists: ", destination, call. = FALSE)
  }
  saveRDS(object, destination)
  invisible(destination)
}
