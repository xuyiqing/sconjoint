root <- normalizePath(dirname(sub("^--file=", "", grep(
  "^--file=", commandArgs(FALSE), value = TRUE)[1L])), mustWork = TRUE)

read_one_dcf <- function(path) {
  x <- read.dcf(path, all = TRUE)
  stopifnot(nrow(x) == 1L)
  setNames(as.character(x[1L, ]), colnames(x))
}

p <- read_one_dcf(file.path(root, "protocol.dcf"))
timeline <- read.csv(file.path(root, "same-day-timeline.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE)
scope <- read.csv(file.path(root, "required-vs-deferred.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
required_files <- c(
  "README.md", "protocol.dcf", "validation-plan.md",
  "application-plan.md", "same-day-timeline.csv",
  "required-vs-deferred.csv", "test-prototype.R")

stopifnot(
  identical(p[["Status"]],
            "draft_for_author_review_outcomes_locked_not_frozen_not_approved"),
  identical(p[["Outcome-Generation"]], "locked"),
  identical(as.integer(p[["Same-Day-Reps-Per-Cell"]]), 8L),
  identical(as.integer(p[["Same-Day-Total-Reps"]]), 48L),
  identical(as.integer(p[["Same-Day-Waves"]]), 4L),
  identical(as.integer(p[["Workers"]]), 12L),
  identical(as.integer(p[["Wave-One-Cutoff-Seconds"]]), 3600L),
  identical(as.integer(p[["Application-Multiplier-Draws"]]), 9999L),
  identical(as.integer(p[["Target-Delivery-Hours"]]), 10L),
  identical(as.integer(p[["Hard-Delivery-Hours"]]), 16L),
  identical(as.integer(p[["Confirmatory-Minimum-Reps-Per-Cell"]]), 50L),
  identical(as.integer(p[["Confirmatory-Preferred-Reps-Per-Cell"]]), 100L),
  all(file.exists(file.path(root, required_files))),
  nrow(timeline) == 8L,
  !anyDuplicated(timeline$checkpoint),
  max(timeline$hard_hour) == 16,
  all(c("required", "preliminary", "conditional", "deferred") %in%
        unique(scope$same_day_status)),
  !anyDuplicated(scope$item),
  all(c("loading_scale_contract", "respondent_level_cross_fitting",
        "fixed_riesz_and_ridge_gates", "local_injectivity_certificate") %in%
        scope$item[scope$same_day_status == "required"])
)

cat("v2.6 prototype checks passed:",
    18L + nrow(timeline) + nrow(scope), "\n")
