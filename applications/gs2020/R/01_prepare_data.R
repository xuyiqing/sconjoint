#!/usr/bin/env Rscript

## Graham--Svolik (2020) preparation for the rebuilt structural model.
##
## Starts from the frozen prep_matrices.rds (verified in the legacy audit to
## reproduce the production run bit-for-bit) and applies the coding repair the
## feasibility audit requires: the legacy object conditions on candNum == 1,
## but candidate 1 is displayed on the left only about half the time, so the
## alternative-1 slot of the structural model (which carries the position
## intercept kappa) must be reoriented to the displayed-left candidate using
## the raw file's c_onLeft. Rows where candidate 1 was displayed on the right
## flip: Y* = 1 - Y and DeltaX* = -DeltaX.
##
## Decisions recorded here (Yiqing, 2026-08-24): candidate age and experience
## stay OUT of the structural utility; the zero-effect restriction is stated
## in the provenance block rather than silently imposed. Equal respondent
## weighting; the survey weight is carried in respondent_meta only.
##
## Usage from the package root:
##   SCONJOINT_APPLICATION_ROOT=... Rscript applications/gs2020/R/01_prepare_data.R

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
source(file.path(root, "applications", "R", "read_only_sources.R"))

paths <- scapp_paths()
frozen <- scapp_read_frozen("gs2020")
d <- frozen$data

out_dir <- file.path(root, "applications", "gs2020", "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## ---- Reproduce the legacy row pipeline on the raw file to recover c_onLeft ----
raw_path <- file.path(paths$source_root, "data",
                      "replication_materials_graham_svolik_2020", "data",
                      "data_experiment.csv")
if (!file.exists(raw_path)) stop("Raw experiment file not found: ", raw_path)
gs_raw <- if (requireNamespace("data.table", quietly = TRUE)) {
  as.data.frame(data.table::fread(raw_path, showProgress = FALSE))
} else read.csv(raw_path, stringsAsFactors = FALSE)

gs <- gs_raw[gs_raw$candNum == 1, ]
tp <- table(gs$id)
gs <- gs[gs$id %in% as.integer(names(tp)[tp == 13]), ]
gs <- gs[!is.na(gs$c_win), ]

## Base-Z complete-case filter, verbatim maps from the legacy 04_data_prep.R.
ideo_map <- c("Extremely liberal" = 1, "Liberal" = 2, "Slightly liberal" = 3,
              "Moderate" = 4, "Slightly conservative" = 5, "Conservative" = 6,
              "Extremely conservative" = 7)
trump_map <- c("Strongly disapprove" = 1, "Somewhat disapprove" = 2,
               "Somewhat approve" = 3, "Strongly approve" = 4)
educ_map <- c("Did not complete high school" = 1, "High school graduate" = 2,
              "Some college, no degree" = 3, "Associate's degree" = 4,
              "Bachelor's degree" = 5, "Graduate or professional degree" = 6)
hhi_levels <- c("Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999",
  "$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999",
  "$40,000 to $44,999", "$45,000 to $49,999", "$50,000 to $54,999",
  "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999",
  "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999",
  "$85,000 to $89,999", "$90,000 to $94,999", "$95,000 to $99,999",
  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999",
  "$175,000 to $199,999", "$200,000 to $249,999", "$250,000 and above")
hhi_map <- stats::setNames(seq_along(hhi_levels), hhi_levels)
zc <- data.frame(
  z_ideo = ideo_map[gs$ideo], z_pid7 = as.numeric(gs$pid7),
  z_trump = trump_map[gs$trump], z_age = as.numeric(gs$age),
  z_educ = educ_map[gs$educ], z_hhi = hhi_map[gs$hhi],
  z_auth = as.numeric(gs$auth_total), z_knowl = as.numeric(gs$knowl_anes_total))
cc <- stats::complete.cases(zc)
ok_by_id <- tapply(cc, gs$id, all)
gs <- gs[gs$id %in% as.integer(names(ok_by_id)[ok_by_id]), ]

## ---- Alignment gates against the frozen object (before any flip) ----
if (nrow(gs) != nrow(d$DeltaX)) {
  stop("Row-pipeline mismatch: raw rebuild has ", nrow(gs), " tasks, frozen has ",
       nrow(d$DeltaX), ".")
}
if (!identical(sprintf("%06d", gs$id), as.character(d$respondent))) {
  stop("Respondent sequence mismatch between raw rebuild and frozen object.")
}
for (col in c("diff_respParty", "diff_sex_Female", "diff_pro_Teacher")) {
  if (max(abs(as.numeric(gs[[col]]) - as.numeric(d$DeltaX[, col]))) > 0) {
    stop("Bit-equality gate failed on ", col, ".")
  }
}
if (max(abs(as.numeric(gs$c_win) - d$Y)) > 0) {
  stop("Bit-equality gate failed on Y.")
}

## ---- Orientation rebuild ----
on_left <- as.numeric(gs$c_onLeft)
if (anyNA(on_left) || !all(on_left %in% c(0, 1))) {
  stop("c_onLeft must be a complete 0/1 indicator on retained rows.")
}
if (!("o_onLeft" %in% names(gs)) ||
    any(as.numeric(gs$o_onLeft) + on_left != 1)) {
  stop("c_onLeft and o_onLeft are not complementary; check the raw file.")
}
flip <- on_left == 0
deltaX <- as.matrix(d$DeltaX)
y <- as.numeric(d$Y)
deltaX[flip, ] <- -deltaX[flip, , drop = FALSE]
y[flip] <- 1 - y[flip]

Z <- as.matrix(d$Z)
respondent_id <- as.character(d$respondent)
task <- as.integer(d$task)

respondent_meta <- d$resp_meta
names(respondent_meta)[names(respondent_meta) == "respondent"] <- "respondent_id"

prepared <- list(
  schema_version = "gs2020-prep-2026-08-26-v1",
  application = "gs2020",
  deltaX = deltaX,
  y = y,
  Z_primary = Z,
  respondent_id = respondent_id,
  task = task,
  respondent_meta = respondent_meta,
  sample = list(
    n_respondents = length(unique(respondent_id)),
    n_tasks = nrow(deltaX),
    tasks_dropped_missing_outcome = 262L,
    task_count_range = range(table(respondent_id)),
    flipped_tasks = sum(flip),
    weighting = "equal respondent weight; survey weight kept in respondent_meta only"
  ),
  estimand = paste(
    "Equal-weighted preference distribution among the 1,605 complete-case",
    "respondents; alternative 1 is the displayed-left candidate."),
  restrictions = paste(
    "Candidate age and experience were manipulated but are excluded from the",
    "structural utility; their utility coefficients are restricted to zero",
    "(decision 2026-08-24). The 30 legacy contrasts are retained."),
  provenance = list(
    frozen_source = frozen$provenance$source_file,
    frozen_md5 = frozen$provenance$source_md5,
    raw_source = raw_path,
    orientation = paste(
      "Reoriented to displayed-left alternative using c_onLeft;",
      sum(flip), "of", nrow(deltaX), "tasks flipped (Y and DeltaX negated)."),
    flags = frozen$provenance$flags
  )
)
saveRDS(prepared, file.path(out_dir, "prep_analysis_data.rds"), version = 3)
message(sprintf(
  "Prepared Graham--Svolik: N=%d, tasks=%d, p=%d, Z=%d, flipped=%d (%.1f%%).",
  prepared$sample$n_respondents, nrow(deltaX), ncol(deltaX), ncol(Z),
  sum(flip), 100 * mean(flip)))
