#!/usr/bin/env Rscript
## check-rscript-parity.R
##
## Extracts ```{r} code blocks from each chapter in tutorial/*.qmd that
## has a mirror in vignettes/rscript/, concatenates them, and diffs the
## normalized text against the corresponding .R mirror.  Exits with
## status 1 on any parity failure; 0 on success.
##
## Usage: Rscript inst/dev/check-rscript-parity.R [package-root]

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[1L] else "."
tut_dir <- file.path(root, "tutorial")
rs_dir  <- file.path(root, "vignettes", "rscript")

if (!dir.exists(tut_dir) || !dir.exists(rs_dir)) {
  message("check-rscript-parity: tutorial/ or vignettes/rscript/ not present; nothing to check.")
  quit(status = 0L)
}

## Chapters that must have a mirror.  Non-code chapters are excluded.
mirrors <- c(
  "02-quickstart",
  "04-fitting",
  "05-interpreting",
  "06-quantities",
  "07-visualization",
  "08-case-saha-weeks",
  "09-case-graham-svolik",
  "10-case-bechtel-scheve",
  "11-simulation-check"
)

extract_r_blocks <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  out <- character(0)
  i <- 1L
  while (i <= length(lines)) {
    if (grepl("^```\\{r", lines[i])) {
      i <- i + 1L
      while (i <= length(lines) && !grepl("^```\\s*$", lines[i])) {
        out <- c(out, lines[i])
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  out
}

normalize <- function(x) {
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  x
}

failed <- character(0)
for (stem in mirrors) {
  qmd <- file.path(tut_dir, paste0(stem, ".qmd"))
  rsc <- file.path(rs_dir, paste0(stem, ".R"))
  if (!file.exists(qmd)) next
  if (!file.exists(rsc)) {
    failed <- c(failed, sprintf("missing mirror: %s", rsc))
    next
  }
  q_code <- normalize(extract_r_blocks(qmd))
  r_code <- normalize(readLines(rsc, warn = FALSE))
  ## Strip comment-only lines from the R mirror; the .qmd chunk content
  ## may or may not include them.  Keep code only.
  r_code <- r_code[!grepl("^#", r_code)]
  q_code <- q_code[!grepl("^#", q_code)]
  if (!identical(q_code, r_code)) {
    failed <- c(failed, sprintf(
      "parity failure: %s vs %s\n  qmd has %d non-comment lines, R mirror has %d",
      basename(qmd), basename(rsc), length(q_code), length(r_code)
    ))
  }
}

if (length(failed) > 0L) {
  message(paste(failed, collapse = "\n"))
  quit(status = 1L)
}
message("check-rscript-parity: OK")
quit(status = 0L)
