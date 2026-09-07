#!/usr/bin/env Rscript

## Hash-bound post-fit assessments for completed Ballard--Rosa v2.7
## q=0,1,2 assembled checkpoints.
##
## Usage from the package root, after committing this assessment protocol:
##   applications/bin/Rscript45 applications/br2017/R/08_assess_fast_v27.R status
##   applications/bin/Rscript45 applications/br2017/R/08_assess_fast_v27.R assess
##
## The assess command never refits a model and never overwrites a v2.7 fit
## checkpoint.  Its only output is the separately bound assessment checkpoint.

options(stringsAsFactors = FALSE, warn = 1)

.br_v27a_script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/",
                mustWork = TRUE)
}

.br_v27a_repo_root_from_script <- function() {
  normalizePath(file.path(dirname(.br_v27a_script_path()), "..", "..", ".."),
                winslash = "/", mustWork = TRUE)
}

.br_v27a_load_code <- function(repo_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The `pkgload` package is required.", call. = FALSE)
  }
  pkgload::load_all(repo_root, quiet = TRUE, export_all = FALSE)
  sys.source(file.path(repo_root, "applications", "br2017", "R",
                       "br_v27_contract.R"), envir = .GlobalEnv)
  sys.source(file.path(repo_root, "applications", "br2017", "R",
                       "br_v27_assessment_contract.R"), envir = .GlobalEnv)
  invisible(TRUE)
}

.br_v27a_status <- function(repo_root) {
  protocol <- .br_v27a_validate_protocol(repo_root, require_clean = FALSE)
  upstream <- .br_v27a_upstream_status(repo_root)
  output <- .br_v27a_output_path(repo_root)
  list(
    protocol_id = protocol$protocol_id,
    protocol_hash = protocol$protocol_hash,
    implementation_committed_and_clean = protocol$git$clean,
    upstream = upstream,
    assessment_output = data.frame(
      relative_path = gsub("\\\\", "/", .br_v27a_run_relative),
      complete = file.exists(output) && file.exists(paste0(output, ".sha256")) &&
        isTRUE(tryCatch({ .br_v27a_read_output(output, payload = FALSE); TRUE },
                        error = function(e) FALSE)),
      stringsAsFactors = FALSE),
    outcome_generation_permitted = protocol$git$clean &&
      all(upstream$complete) && !file.exists(output) &&
      !file.exists(paste0(output, ".sha256")))
}

.br_v27a_assess <- function(repo_root) {
  protocol <- .br_v27a_validate_protocol(repo_root, require_clean = TRUE)
  output <- .br_v27a_output_path(repo_root)
  entries <- .br_v27a_extract_upstream(repo_root)
  binding <- .br_v27a_binding(protocol, entries)
  if (file.exists(output) || file.exists(paste0(output, ".sha256"))) {
    .br_v27a_read_output(output, binding = binding, payload = TRUE)
    message("Verified existing immutable v2.7 assessment output.")
    return(invisible(output))
  }
  payload <- .br_v27a_build_assessment(entries, protocol$registry)
  installed <- .br_v27a_save_atomic(payload, output, binding)
  message(
    "Created hash-bound v2.7 assessment checkpoint. No substantive target ",
    "estimate was computed or printed. SHA-256: ", installed$sha256)
  invisible(output)
}

.br_v27a_main <- function() {
  repo_root <- .br_v27a_repo_root_from_script()
  .br_v27a_load_code(repo_root)
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L || !args[[1L]] %in% c("status", "assess")) {
    stop("Supply exactly one command: status or assess.", call. = FALSE)
  }
  if (identical(args[[1L]], "status")) {
    status <- .br_v27a_status(repo_root)
    print(status$protocol_id)
    print(status$implementation_committed_and_clean)
    print(status$upstream, row.names = FALSE)
    print(status$assessment_output, row.names = FALSE)
    print(status$outcome_generation_permitted)
  } else {
    .br_v27a_assess(repo_root)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) .br_v27a_main()
