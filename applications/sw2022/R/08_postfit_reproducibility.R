#!/usr/bin/env Rscript

## Rebuild the post-fit reproducibility record after installing the current
## package source. The shell driver refuses to run while 03_fit_models.R is
## active; invoke this file through applications/sw2022/bin/postfit_validate.sh.

options(stringsAsFactors = FALSE)

.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(.script_arg)) stop("Run this file with Rscript.", call. = FALSE)
.script <- normalizePath(sub("^--file=", "", .script_arg[[1L]]),
                         mustWork = TRUE)
.project <- normalizePath(file.path(dirname(.script), "../../.."),
                          mustWork = TRUE)
.app <- file.path(.project, "applications", "sw2022")
.out <- file.path(.app, "manifests", "postfit_validation")
dir.create(.out, recursive = TRUE, showWarnings = FALSE)

required <- c("digest", "pkgload", "testthat", "torch", "sconjoint")
missing <- required[!vapply(required, requireNamespace, logical(1L),
                            quietly = TRUE)]
if (length(missing)) {
  stop("Missing validation packages: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

.relative <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(.project, .Platform$file.sep)
  ifelse(startsWith(path, prefix), substring(path, nchar(prefix) + 1L), path)
}
.sha256 <- function(path) {
  unname(vapply(path, function(x) {
    digest::digest(x, algo = "sha256", serialize = FALSE, file = TRUE)
  }, character(1L)))
}
.artifact_table <- function(path, kind) {
  path <- unique(path[file.exists(path) & !dir.exists(path)])
  info <- file.info(path)
  data.frame(
    kind = kind,
    path = .relative(path),
    bytes = as.numeric(info$size),
    modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE),
    sha256 = .sha256(path),
    stringsAsFactors = FALSE
  )
}

runtime_resources <- file.path(
  .project, ".runtime", "R-4.5.3-root", "Library", "Frameworks",
  "R.framework", "Versions", "4.5-arm64", "Resources"
)
local_library <- file.path(.project, ".R-library", "4.5")
critical_runtime <- file.path(runtime_resources, c(
  "bin/R", "bin/exec/R", "lib/libR.dylib", "etc/Makeconf", "etc/Renviron"
))
torch_library <- file.path(local_library, "torch", "lib")
critical_torch <- if (dir.exists(torch_library)) {
  list.files(torch_library, pattern = "^lib.*[.]dylib$", full.names = TRUE)
} else character()
critical_tracked <- c(
  file.path(.project, "applications", "bin", c("R45", "Rscript45")),
  file.path(.app, "R", c("00_verify_environment.R",
                          "08_postfit_reproducibility.R")),
  file.path(.app, "bin", "postfit_validate.sh")
)
critical <- rbind(
  .artifact_table(critical_runtime, "runtime"),
  .artifact_table(critical_torch, "torch_native_library"),
  .artifact_table(critical_tracked, "launcher_or_validator")
)
write.csv(critical, file.path(.out, "critical_artifact_hashes.csv"),
          row.names = FALSE)

installed <- as.data.frame(
  utils::installed.packages(lib.loc = local_library),
  stringsAsFactors = FALSE
)
description_path <- file.path(local_library, installed$Package, "DESCRIPTION")
library_manifest <- data.frame(
  package = installed$Package,
  version = installed$Version,
  built = installed$Built,
  priority = installed$Priority,
  description_sha256 = .sha256(description_path),
  stringsAsFactors = FALSE
)
library_manifest <- library_manifest[order(library_manifest$package), ]
write.csv(library_manifest, file.path(.out, "local_library_manifest.csv"),
          row.names = FALSE)

focused_tests <- file.path(.project, "tests", "testthat", c(
  "test-data-prep.R",
  "test-paperps-ordering-regression.R",
  "test-paperps-computation.R",
  "test-paperps-inference.R",
  "test-paperps-quantities.R",
  "test-specification-assessment-paperps.R"
))
package_sources <- c(
  file.path(.project, "DESCRIPTION"), file.path(.project, "NAMESPACE"),
  list.files(file.path(.project, "R"), pattern = "[.]R$", full.names = TRUE)
)
application_sources <- c(
  list.files(file.path(.app, "R"), pattern = "[.]R$", full.names = TRUE),
  list.files(file.path(.app, "config"), pattern = "[.]R$", full.names = TRUE),
  file.path(.project, "applications", "bin", c("R45", "Rscript45")),
  file.path(.app, "bin", "postfit_validate.sh")
)
validated_sources <- unique(c(package_sources, application_sources,
                              focused_tests))
validated_sources <- validated_sources[file.exists(validated_sources)]
source_hashes <- .artifact_table(validated_sources, "validated_source")
write.csv(source_hashes, file.path(.out, "validated_source_hashes.csv"),
          row.names = FALSE)

r_sources <- validated_sources[grepl("[.]R$", validated_sources)]
parse_rows <- lapply(r_sources, function(path) {
  error <- tryCatch({
    parse(path, keep.source = FALSE)
    ""
  }, error = function(e) conditionMessage(e))
  data.frame(path = .relative(path), parsed = !nzchar(error), error = error,
             stringsAsFactors = FALSE)
})
parse_table <- do.call(rbind, parse_rows)
write.csv(parse_table, file.path(.out, "r_parse_checks.csv"), row.names = FALSE)

text_rows <- lapply(validated_sources, function(path) {
  lines <- readLines(path, warn = FALSE)
  trailing <- which(grepl("[[:blank:]]+$", lines))
  conflict <- which(grepl("^(<<<<<<<|=======|>>>>>>>)", lines))
  data.frame(
    path = .relative(path),
    trailing_whitespace_lines = paste(trailing, collapse = ";"),
    conflict_marker_lines = paste(conflict, collapse = ";"),
    pass = !length(trailing) && !length(conflict),
    stringsAsFactors = FALSE
  )
})
text_table <- do.call(rbind, text_rows)
write.csv(text_table, file.path(.out, "source_text_checks.csv"),
          row.names = FALSE)

runtime_ok <- isTRUE(getRversion() == "4.5.3") &&
  identical(R.version$arch, "aarch64")
library_ok <- identical(normalizePath(.libPaths()[1L], mustWork = TRUE),
                        normalizePath(local_library, mustWork = TRUE))
torch_version_ok <- identical(as.character(utils::packageVersion("torch")),
                              "0.16.3")

suppressPackageStartupMessages(library(torch))
torch_native_ok <- isTRUE(torch::torch_is_installed())
torch_sum <- as.numeric(torch::torch_tensor(c(1, 2, 3), device = "cpu")$sum())
x <- torch::torch_tensor(2, device = "cpu", requires_grad = TRUE)
(x^2)$backward()
torch_gradient <- as.numeric(x$grad$item())
torch_smoke_ok <- identical(torch_sum, 6) &&
  isTRUE(all.equal(torch_gradient, 4, tolerance = 1e-7))

installed_sconjoint <- utils::packageDescription(
  "sconjoint", lib.loc = local_library
)
installed_package_ok <- !is.null(installed_sconjoint) &&
  identical(installed_sconjoint$Version, "0.3.0.9000")

if (!all(parse_table$parsed) || !all(text_table$pass)) {
  test_rows <- data.frame(
    source_file = character(), file = character(), context = character(),
    test = character(), nb = integer(), failed = integer(),
    skipped = logical(), error = logical(), warning = integer(),
    user = numeric(), system = numeric(), real = numeric(), passed = integer(),
    stringsAsFactors = FALSE
  )
} else {
  pkgload::load_all(.project, quiet = TRUE)
  test_rows <- lapply(focused_tests, function(path) {
    result <- tryCatch(
      testthat::test_file(path, reporter = "summary", package = "sconjoint"),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      return(data.frame(
        source_file = basename(path), file = basename(path), context = "",
        test = paste0("test_file error: ", conditionMessage(result)), nb = 1L,
        failed = 0L, skipped = FALSE, error = TRUE, warning = 0L,
        user = NA_real_, system = NA_real_, real = NA_real_, passed = 0L,
        stringsAsFactors = FALSE
      ))
    }
    tab <- as.data.frame(result)
    if ("result" %in% names(tab)) tab$result <- NULL
    tab$source_file <- basename(path)
    tab[, c("source_file", setdiff(names(tab), "source_file")), drop = FALSE]
  })
  test_rows <- do.call(rbind, test_rows)
}
write.csv(test_rows, file.path(.out, "focused_test_results.csv"),
          row.names = FALSE)

if (nrow(test_rows)) {
  split_rows <- split(test_rows, test_rows$source_file)
  test_summary <- do.call(rbind, lapply(split_rows, function(x) {
    data.frame(
      source_file = x$source_file[[1L]], tests = nrow(x),
      expectations = sum(x$nb, na.rm = TRUE),
      passed = sum(x$passed, na.rm = TRUE),
      failed = sum(x$failed, na.rm = TRUE),
      errors = sum(x$error, na.rm = TRUE),
      warnings = sum(x$warning, na.rm = TRUE),
      skipped = sum(x$skipped, na.rm = TRUE),
      elapsed_seconds = sum(x$real, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(test_summary) <- NULL
} else {
  test_summary <- data.frame(
    source_file = basename(focused_tests), tests = 0L, expectations = 0L,
    passed = 0L, failed = 0L, errors = 1L, warnings = 0L, skipped = 0L,
    elapsed_seconds = NA_real_, stringsAsFactors = FALSE
  )
}
write.csv(test_summary, file.path(.out, "focused_test_summary.csv"),
          row.names = FALSE)

tests_ok <- nrow(test_rows) > 0L &&
  sum(test_rows$failed, na.rm = TRUE) == 0L &&
  sum(test_rows$error, na.rm = TRUE) == 0L &&
  sum(test_rows$warning, na.rm = TRUE) == 0L
checks <- data.frame(
  check = c(
    "R_4.5.3_aarch64", "project_library_first", "torch_0.16.3",
    "torch_native_cpu_and_autograd", "installed_sconjoint_0.3.0.9000",
    "all_R_sources_parse", "source_text_clean", "focused_tests"
  ),
  pass = c(
    runtime_ok, library_ok, torch_version_ok,
    torch_native_ok && torch_smoke_ok, installed_package_ok,
    all(parse_table$parsed), all(text_table$pass), tests_ok
  ),
  detail = c(
    paste(R.version.string, R.version$arch, sep = " / "),
    .libPaths()[1L], as.character(utils::packageVersion("torch")),
    paste0("tensor_sum=", torch_sum, "; autograd_gradient=", torch_gradient),
    if (is.null(installed_sconjoint)) "not installed" else
      paste0(installed_sconjoint$Package, " ", installed_sconjoint$Version,
             "; built=", installed_sconjoint$Built),
    paste0(sum(parse_table$parsed), "/", nrow(parse_table)),
    paste0(sum(text_table$pass), "/", nrow(text_table)),
    paste0("failed=", sum(test_rows$failed, na.rm = TRUE),
           "; errors=", sum(test_rows$error, na.rm = TRUE),
           "; warnings=", sum(test_rows$warning, na.rm = TRUE),
           "; skipped=", sum(test_rows$skipped, na.rm = TRUE))
  ),
  stringsAsFactors = FALSE
)
write.csv(checks, file.path(.out, "validation_summary.csv"), row.names = FALSE)

session_lines <- c(
  paste0("validated_at_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("project: ", .project),
  paste0("local_library: ", local_library),
  paste0("critical_artifact_manifest_sha256: ", .sha256(
    file.path(.out, "critical_artifact_hashes.csv"))),
  paste0("library_manifest_sha256: ", .sha256(
    file.path(.out, "local_library_manifest.csv"))),
  paste0("source_manifest_sha256: ", .sha256(
    file.path(.out, "validated_source_hashes.csv"))),
  "", utils::capture.output(sessionInfo())
)
writeLines(session_lines, file.path(.out, "sessionInfo.txt"))
saveRDS(
  list(
    status = if (all(checks$pass)) "passed" else "failed",
    checks = checks, tests = test_summary,
    expected_installer_sha256 =
      "8c1d5005547926425037ffa7d9062099231e033022275648625b32791dd43eb5",
    installer_present_in_project = FALSE,
    validation_scope = paste(
      "Current package reinstall; focused paperps, ordering, data-prep,",
      "specification-assessment and native CPU Torch tests; R parse and",
      "source-text checks. This is not the complete legacy test suite."
    )
  ),
  file.path(.out, "validation_record.rds"), version = 3
)

if (!all(checks$pass)) {
  failed <- checks$check[!checks$pass]
  stop("Post-fit reproducibility validation failed: ",
       paste(failed, collapse = ", "), call. = FALSE)
}
cat("Post-fit reproducibility validation passed.\n")
