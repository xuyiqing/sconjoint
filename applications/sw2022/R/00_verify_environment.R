#!/usr/bin/env Rscript

## Verify and record the native, project-local analysis environment.

options(stringsAsFactors = FALSE)

.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(.script_arg)) stop("Run this file with Rscript.", call. = FALSE)
.script <- normalizePath(sub("^--file=", "", .script_arg[[1L]]),
                         mustWork = TRUE)
.project <- normalizePath(file.path(dirname(.script), "../../.."),
                          mustWork = TRUE)
.app <- file.path(.project, "applications", "sw2022")
.manifests <- file.path(.app, "manifests")
dir.create(.manifests, recursive = TRUE, showWarnings = FALSE)

if (getRversion() != "4.5.3" || R.version$arch != "aarch64") {
  stop("The production analysis requires native aarch64 R 4.5.3; observed ",
       R.version.string, " / ", R.version$arch, ".", call. = FALSE)
}

required <- c(
  "sconjoint", "torch", "testthat", "qrng", "randtoolbox", "haven",
  "data.table", "future", "future.apply", "parallelly", "withr",
  "ggplot2", "ggridges", "glmnet", "grf", "lme4", "patchwork", "renv"
)
missing <- required[!vapply(required, requireNamespace, logical(1L),
                            quietly = TRUE)]
if (length(missing)) {
  stop("Missing required local R packages: ", paste(missing, collapse = ", "),
       call. = FALSE)
}
if (as.character(utils::packageVersion("torch")) != "0.16.3") {
  stop("Torch must be pinned at 0.16.3 for this analysis.", call. = FALSE)
}

suppressPackageStartupMessages(library(torch))
if (!isTRUE(torch::torch_is_installed())) {
  stop("Torch's native libraries are not available.", call. = FALSE)
}
torch_sum <- as.numeric(torch::torch_tensor(c(1, 2, 3))$sum())
if (!identical(torch_sum, 6)) stop("Torch tensor smoke test failed.")

installed <- as.data.frame(utils::installed.packages(), stringsAsFactors = FALSE)
packages <- installed[match(required, installed$Package),
                      c("Package", "Version", "LibPath", "Built"), drop = FALSE]
packages$requested_for_sw2022 <- TRUE
write.csv(packages, file.path(.manifests, "environment_packages.csv"),
          row.names = FALSE)

source_files <- c(
  file.path(.project, "DESCRIPTION"), file.path(.project, "NAMESPACE"),
  list.files(file.path(.project, "R"), pattern = "\\.R$", full.names = TRUE)
)
source_manifest <- data.frame(
  path = substring(source_files, nchar(.project) + 2L),
  bytes = file.info(source_files)$size,
  md5 = unname(tools::md5sum(source_files)),
  stringsAsFactors = FALSE
)
write.csv(source_manifest,
          file.path(.manifests, "environment_package_source.csv"),
          row.names = FALSE)

git_head <- tryCatch(
  system2("git", c("-C", .project, "rev-parse", "HEAD"), stdout = TRUE,
          stderr = FALSE),
  error = function(e) NA_character_
)
git_status <- tryCatch(
  system2("git", c("-C", .project, "status", "--short"), stdout = TRUE,
          stderr = FALSE),
  error = function(e) "git status unavailable"
)

session_lines <- c(
  paste0("verified_at_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("project: ", .project),
  paste0("git_head: ", paste(git_head, collapse = "")),
  "git_status:", if (length(git_status)) paste0("  ", git_status) else
    "  clean",
  "", utils::capture.output(sessionInfo())
)
writeLines(session_lines, file.path(.manifests, "environment_session.txt"))

verification <- list(
  status = "passed",
  R_version = R.version.string,
  architecture = R.version$arch,
  R_home = R.home(),
  library = .libPaths()[1L],
  torch_version = as.character(utils::packageVersion("torch")),
  torch_tensor_smoke = torch_sum,
  CPU_primary = TRUE,
  MPS_primary = FALSE,
  git_head = paste(git_head, collapse = ""),
  git_status = git_status,
  source_manifest_md5 = unname(tools::md5sum(
    file.path(.manifests, "environment_package_source.csv")
  )),
  maintained_caveat = paste(
    "The local runtime was extracted without administrator privileges because",
    "the host could not validate the official installer signature; see",
    "manifests/environment.md for the installer SHA-256 and details."
  )
)
saveRDS(verification, file.path(.manifests, "environment_check.rds"),
        version = 3)

cat("Environment verification passed: native R 4.5.3, Torch 0.16.3, CPU.\n")
