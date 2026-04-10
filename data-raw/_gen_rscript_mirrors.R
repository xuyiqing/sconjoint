## Helper (not shipped) to regenerate vignettes/rscript/*.R mirrors
## from tutorial/*.qmd code blocks.  Run from package root.
extract_r_blocks <- function(qmd_path) {
  lines <- readLines(qmd_path, warn = FALSE)
  out <- character(0)
  i <- 1L
  re_open <- "^```[{]r"
  re_close <- "^```\\s*$"
  while (i <= length(lines)) {
    if (grepl(re_open, lines[i])) {
      i <- i + 1L
      while (i <= length(lines) && !grepl(re_close, lines[i])) {
        out <- c(out, lines[i])
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  out
}

stems <- c("02-quickstart", "04-fitting", "05-interpreting",
           "06-quantities", "07-visualization",
           "08-case-saha-weeks", "09-case-graham-svolik",
           "10-case-bechtel-scheve", "11-simulation-check")
for (stem in stems) {
  qmd <- file.path("tutorial", paste0(stem, ".qmd"))
  if (!file.exists(qmd)) next
  blocks <- extract_r_blocks(qmd)
  writeLines(blocks, file.path("vignettes/rscript", paste0(stem, ".R")))
  cat(stem, ": ", length(blocks), " lines\n", sep = "")
}
