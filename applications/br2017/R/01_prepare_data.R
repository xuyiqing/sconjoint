#!/usr/bin/env Rscript

## Ballard-Rosa, Martin, and Scheve (2017) preparation for the rebuilt
## structural model.
##
## Starts from the frozen prep_matrices.rds. The loader has already aliased
## DeltaX to DeltaX_fixed (the corrected code-5 tax-rate recode; the corrupted
## matrix errors out if absent), so no further coding repair is required here.
## Plan 1 is the displayed-left alternative in this design, so no orientation
## rebuild is needed. Equal respondent weighting; the survey weight is carried
## in respondent_meta only.
##
## Usage from the package root:
##   SCONJOINT_APPLICATION_ROOT=... Rscript applications/br2017/R/01_prepare_data.R

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
source(file.path(root, "applications", "R", "read_only_sources.R"))

frozen <- scapp_read_frozen("br2017")
d <- frozen$data

out_dir <- file.path(root, "applications", "br2017", "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

deltaX <- as.matrix(d$DeltaX)   # aliased to DeltaX_fixed by the loader
y <- as.numeric(d$Y)
Z <- as.matrix(d$Z)
respondent_id <- as.character(d$respondent)
task <- as.integer(d$task)

if (!identical(dim(deltaX), c(16000L, 7L))) {
  stop("Expected 16,000 x 7 corrected contrasts; got ",
       paste(dim(deltaX), collapse = " x "), ".")
}
if (length(unique(respondent_id)) != 2000L) {
  stop("Expected 2,000 respondents; got ", length(unique(respondent_id)), ".")
}
if (!all(table(respondent_id) == 8L)) {
  stop("Every Ballard-Rosa respondent must contribute 8 tasks.")
}
if (any(!y %in% c(0, 1)) || anyNA(deltaX) || anyNA(Z)) {
  stop("Invalid outcomes or missing values in the frozen matrices.")
}
corrupted <- as.matrix(d$DeltaX_corrupted)
n_changed <- sum(rowSums(abs(deltaX - corrupted)) > 0)
if (n_changed != 5044L) {
  stop("Expected the code-5 repair to change 5,044 task contrasts; found ",
       n_changed, ".")
}

respondent_meta <- d$resp_meta
names(respondent_meta)[names(respondent_meta) == "respondent"] <- "respondent_id"

prepared <- list(
  schema_version = "br2017-prep-2026-08-26-v1",
  application = "br2017",
  deltaX = deltaX,
  y = y,
  Z_primary = Z,
  respondent_id = respondent_id,
  task = task,
  respondent_meta = respondent_meta,
  sample = list(
    n_respondents = 2000L,
    n_tasks = 16000L,
    tasks_changed_by_code5_repair = n_changed,
    weighting = "equal respondent weight; survey weight kept in respondent_meta only"
  ),
  estimand = paste(
    "Equal-weighted preference distribution among the 2,000 respondents;",
    "alternative 1 is the displayed-left tax plan."),
  provenance = list(
    frozen_source = frozen$provenance$source_file,
    frozen_md5 = frozen$provenance$source_md5,
    contrast_matrix = "DeltaX_fixed (corrected code-5 recode); corrupted matrix untouched",
    flags = frozen$provenance$flags
  )
)
saveRDS(prepared, file.path(out_dir, "prep_analysis_data.rds"), version = 3)
message(sprintf(
  "Prepared Ballard-Rosa: N=%d, tasks=%d, p=%d, Z=%d, repaired contrasts=%d.",
  2000L, nrow(deltaX), ncol(deltaX), ncol(Z), n_changed))
