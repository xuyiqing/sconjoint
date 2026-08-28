#!/usr/bin/env Rscript

## Deterministic application smoke test for the Saha--Weeks assessment-only
## extensions.  It does not refit a model or write an analysis artifact.

options(stringsAsFactors = FALSE)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}
.parse_profile <- function() {
  arg <- grep("^--profile=", commandArgs(trailingOnly = TRUE), value = TRUE)
  if (length(arg)) sub("^--profile=", "", arg[[1L]]) else "smoke"
}

script <- .script_file()
app <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
profile <- .parse_profile()
source(file.path(app, "R", "assessment_extensions.R"))

prepared <- readRDS(file.path(app, "results", "prep_analysis_data.rds"))
assembled_path <- file.path(app, "results", "mixed_logit", profile,
                            "fit_primary_assembled.rds")
if (!file.exists(assembled_path)) {
  stop("Assembled fit is missing for profile ", profile, ".", call. = FALSE)
}
assembled <- readRDS(assembled_path)
x <- .sw_exact_joint_predictions(assembled, prepared)
stopifnot(
  nrow(x$full) == prepared$sample$N * 8L,
  nrow(x$pair) == prepared$sample$N * 3L * 4L,
  nrow(x$repeated) == 4L,
  length(unique(x$repeated$respondent_id)) == 1L,
  max(x$probability_sum_error, na.rm = TRUE) <= 1e-10
)

if (!requireNamespace("sconjoint", quietly = TRUE)) {
  stop("Install this checkout before running the smoke test.", call. = FALSE)
}
base <- getExportedValue("sconjoint", "scmix_heldout_predictions")(
  assembled, task_order = prepared$task, include_counts = TRUE,
  include_adjacent = TRUE, include_repeated = TRUE
)
adjacent <- base$joint[base$joint$type == "adjacent_pair", , drop = FALSE]
adjacent$key <- paste(
  adjacent$respondent_id,
  paste0("tasks_", adjacent$task_1, "_", adjacent$task_2),
  adjacent$stratum
)
x$pair$key <- paste(x$pair$respondent_id, x$pair$pair, x$pair$event)
adjacent_match <- match(adjacent$key, x$pair$key)
stopifnot(!anyNA(adjacent_match),
          max(abs(adjacent$predicted -
                    x$pair$predicted[adjacent_match])) <= 1e-12)

repeat_base <- base$joint[
  base$joint$type == "repeated_contrast_pair", , drop = FALSE
]
repeat_base$key <- paste(
  repeat_base$respondent_id,
  paste0("tasks_", repeat_base$task_1, "_", repeat_base$task_2),
  repeat_base$stratum
)
x$repeated$key <- paste(x$repeated$respondent_id, x$repeated$pair,
                        x$repeated$event)
repeat_match <- match(repeat_base$key, x$repeated$key)
stopifnot(!anyNA(repeat_match),
          max(abs(repeat_base$predicted -
                    x$repeated$predicted[repeat_match])) <= 1e-12)

completion_task <- readRDS(file.path(app, "results",
                                     "completion_task_audit.rds"))
completion <- .sw_completion_comparisons(completion_task)
stopifnot(
  nrow(completion_task) == 3740L,
  length(unique(completion_task$respondent_id)) == 1249L,
  nrow(completion$summary) == 3L * 3L * 14L,
  all(c("eventual_tasks_3_vs_2", "finished_vs_unfinished",
        "primary_included_vs_excluded") %in% completion$summary$comparison)
)

protocol <- .sw_conditional_randomization_status(
  readRDS(file.path(app, "results", "design_completion_audit.rds"))
)
stopifnot(
  all(protocol$status == "protocol_unavailable_not_run"),
  all(!protocol$protocol_verified), all(is.na(protocol$p_value))
)

cat(sprintf(
  paste0("Assessment-extension smoke test (%s) passed: ",
         "%d full-pattern rows, %d task-pair rows, %d exact-repeat rows.\n"),
  profile, nrow(x$full), nrow(x$pair), nrow(x$repeated)
))
