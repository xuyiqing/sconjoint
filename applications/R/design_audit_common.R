## Shared lean design/completion audit for gs2020 and br2017 (step 02).
##
## The caller (a thin per-app wrapper) must define, before sourcing this file:
##   app_name, cfg, app_root, root
## Lighter than Avidit's sw2022 02 (which reconstructs sw-specific exclusion
## waterfalls): this audit records completion patterns, realized-cell
## sparsity, affine and unrestricted-vech ranks via scmix_design_audit(), and
## attribute-level exposure marginals. Protocol support is NOT supplied here;
## that is the separate archival protocol-metadata step.

suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

prepared <- readRDS(cfg$input$prepared)
deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
respondent_id <- as.character(prepared$respondent_id)

out_dir <- file.path(app_root, "results")
table_dir <- file.path(app_root, "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

## Completion patterns
tt <- table(table(respondent_id))
completion <- data.frame(tasks_completed = as.integer(names(tt)),
                         respondents = as.integer(tt))
utils::write.csv(completion, file.path(table_dir, "completion_pattern.csv"),
                 row.names = FALSE)

## Realized-cell sparsity
cell_key <- apply(deltaX, 1L, paste, collapse = "|")
cells <- table(cell_key)
cell_summary <- data.frame(
  n_tasks = nrow(deltaX),
  distinct_cells = length(cells),
  largest_cell = max(cells),
  singleton_cells = sum(cells == 1L))
utils::write.csv(cell_summary, file.path(table_dir, "realized_cells.csv"),
                 row.names = FALSE)

## Rank audit at primary and alternative q
unique_contrasts <- deltaX[!duplicated(cell_key), , drop = FALSE]
## Realized-repeat matching (deltaX/respondent_id/task_order) is omitted: the
## package's matching pass is quadratic in tasks and the cell table above
## already records repeat structure exactly.
rank_rows <- lapply(c(cfg$primary$q, cfg$primary$alternative_q), function(q) {
  audit <- scmix_design_audit(
    contrasts = unique_contrasts, q = as.integer(q))
  df <- as.data.frame(audit)
  df$q <- q
  df
})
rank_table <- do.call(rbind, rank_rows)
utils::write.csv(rank_table, file.path(table_dir, "design_rank_by_q.csv"),
                 row.names = FALSE)

## Exposure marginals: mean and sd of each contrast coordinate, outcome rate
exposure <- data.frame(
  coordinate = colnames(deltaX),
  mean = colMeans(deltaX),
  sd = apply(deltaX, 2L, stats::sd),
  abs_mean_over_sd = abs(colMeans(deltaX)) / apply(deltaX, 2L, stats::sd))
utils::write.csv(exposure, file.path(table_dir, "exposure_marginals.csv"),
                 row.names = FALSE)

audit_obj <- list(
  application = app_name,
  completion = completion,
  cells = cell_summary,
  rank_table = rank_table,
  exposure = exposure,
  outcome_rate = mean(y),
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
saveRDS(audit_obj, file.path(out_dir, "design_completion_audit.rds"),
        version = 3)
message(sprintf(
  "%s design audit: %d tasks, %d distinct cells (largest %d), outcome rate %.3f.",
  app_name, nrow(deltaX), cell_summary$distinct_cells,
  cell_summary$largest_cell, mean(y)))
