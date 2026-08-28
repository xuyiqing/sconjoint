#!/usr/bin/env Rscript

## Saha--Weeks (2022): completion and conjoint-support audits.
##
## The full-support calculations below are conditional on the article's
## description of independent profile randomization.  The public replication
## package does not contain the fielded randomizer/QSF or assignment
## probabilities, so this script deliberately does not label that protocol as
## verified.  Uniform assignment probabilities are included only as a
## transparent sensitivity benchmark.

options(stringsAsFactors = FALSE)

.script_file <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(arg)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", arg[[1L]]), mustWork = TRUE)
}

.script <- .script_file()
.project <- normalizePath(file.path(dirname(.script), "../../.."),
                          mustWork = TRUE)
.app <- file.path(.project, "applications", "sw2022")
.results <- file.path(.app, "results")
.tables <- file.path(.app, "tables")
.manifests <- file.path(.app, "manifests")
invisible(lapply(c(.results, .tables, .manifests), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

prepared_path <- file.path(.results, "prep_analysis_data.rds")
if (!file.exists(prepared_path)) {
  stop("Run 01_prepare_data.R before this audit.", call. = FALSE)
}
prepared <- readRDS(prepared_path)

.source_root <- normalizePath(
  Sys.getenv(
    "SCONJOINT_APPLICATION_ROOT",
    unset = "/Users/avidit/Dropbox/Research - Active/Conjoint/ConjointStructural"
  ),
  mustWork = TRUE
)
.raw_file <- file.path(
  .source_root, "data", "replication_materials_saha_weeks_2022", "ssi.csv"
)
ssi_raw <- read.csv(.raw_file, check.names = FALSE, stringsAsFactors = FALSE)

VALID_INCOME <- c(
  "Less than $ 20,000", "$ 20,000 - $ 29,999", "$ 30,000 - $ 39,999",
  "$ 40,000 - $ 49,999", "$ 50,000 - $ 59,999", "$ 60,000 - $ 74,999",
  "$ 75,000 - $ 99,999", "$ 100,000 - $ 149,999", "$ 150,000 +"
)
VALID_PARTY <- c("Democrat", "Republican (GOP)", "Independent")
VALID_EDUC <- c("Low", "Middle", "High")
VALID_REGION <- c("MIDWEST", "NORTHEAST", "SOUTH", "WEST")
VALID_EMPLOY <- c(
  "employed fulltime", "employed part-time (less than 32 hours)",
  "Homemaker", "not working/looking for work",
  "retired/unable to work/disabled", "student/at school"
)

.is_blank <- function(x) is.na(x) | as.character(x) == ""
.first_constant <- function(x, id, what) {
  z <- split(x, id)
  if (any(vapply(z, function(v) length(unique(v[!is.na(v)])) > 1L,
                 logical(1L)))) {
    stop(what, " varies within respondent.", call. = FALSE)
  }
  vapply(z, function(v) v[[1L]], v[[1L]])
}

rid_levels <- sort(unique(ssi_raw$ResponseId), method = "radix")
raw_split <- split(seq_len(nrow(ssi_raw)),
                   factor(ssi_raw$ResponseId, levels = rid_levels))
first_idx <- vapply(raw_split, `[[`, integer(1L), 1L)
rf <- ssi_raw[first_idx, , drop = FALSE]
rownames(rf) <- NULL
stopifnot(identical(rf$ResponseId, rid_levels))

valid_matrix <- cbind(
  gender = !.is_blank(rf$resp_gender) & rf$resp_gender %in% c("female", "male"),
  age = !is.na(rf$Age) & is.finite(rf$Age),
  income = !.is_blank(rf$Income) & rf$Income %in% VALID_INCOME,
  education = !.is_blank(rf$Education_Level_General) &
    rf$Education_Level_General %in% VALID_EDUC,
  party = !.is_blank(rf$`Interests.Political.Affiliation`) &
    rf$`Interests.Political.Affiliation` %in% VALID_PARTY,
  region = !.is_blank(rf$`Geo.Census.Region..US.`) &
    rf$`Geo.Census.Region..US.` %in% VALID_REGION,
  employment = !.is_blank(rf$Employment_status) &
    rf$Employment_status %in% VALID_EMPLOY
)
T_i <- vapply(raw_split, function(ii) length(unique(ssi_raw$election[ii])),
              integer(1L))
profile_rows <- lengths(raw_split)
finished <- as.logical(rf$Finished)
progress <- as.numeric(rf$Progress)
all_demo_missing <- apply(
  cbind(
    .is_blank(rf$resp_gender), is.na(rf$Age), .is_blank(rf$Income),
    .is_blank(rf$Education_Level_General),
    .is_blank(rf$`Interests.Political.Affiliation`),
    .is_blank(rf$`Geo.Census.Region..US.`), .is_blank(rf$Employment_status)
  ),
  1L, all
)
primary_valid <- apply(valid_matrix, 1L, all)
included <- T_i == 3L & primary_valid
invalid_fields <- apply(!valid_matrix, 1L, function(z) {
  bad <- colnames(valid_matrix)[z]
  if (length(bad)) paste(bad, collapse = "+") else ""
})
exclusion_reason <- ifelse(
  T_i != 3L, "fewer_than_three_tasks",
  ifelse(!primary_valid, paste0("invalid_primary_", invalid_fields), "included")
)

completion_status <- data.frame(
  respondent_id = rid_levels,
  tasks = T_i,
  profile_rows = profile_rows,
  finished = finished,
  progress = progress,
  all_primary_demographics_missing = all_demo_missing,
  primary_demographics_valid = primary_valid,
  final_analysis_sample = included,
  exclusion_reason = exclusion_reason,
  stringsAsFactors = FALSE
)
if (sum(included) != 1191L) {
  stop("Completion audit does not recover N=1,191.", call. = FALSE)
}
write.csv(completion_status,
          file.path(.tables, "completion_status.csv"), row.names = FALSE)

.aggregate_completion <- aggregate(
  rep(1L, nrow(completion_status)),
  by = completion_status[c(
    "tasks", "finished", "progress", "all_primary_demographics_missing",
    "primary_demographics_valid", "final_analysis_sample"
  )],
  FUN = sum
)
names(.aggregate_completion)[ncol(.aggregate_completion)] <- "respondents"
.aggregate_completion <- .aggregate_completion[
  do.call(order, .aggregate_completion[seq_len(ncol(.aggregate_completion) - 1L)]),
  , drop = FALSE
]
write.csv(.aggregate_completion,
          file.path(.tables, "completion_summary.csv"), row.names = FALSE)

completion_missingness <- data.frame(
  field = colnames(valid_matrix),
  raw_respondents_invalid = colSums(!valid_matrix),
  three_task_respondents_invalid = colSums(
    !valid_matrix[T_i == 3L, , drop = FALSE]
  ),
  marginal_count_only = TRUE,
  stringsAsFactors = FALSE
)
write.csv(completion_missingness,
          file.path(.tables, "completion_missingness.csv"), row.names = FALSE)

completion_key_facts <- data.frame(
  fact = c(
    "raw respondents", "two-task unfinished respondents",
    "three-task unfinished respondents", "three-task finished respondents",
    "all-primary-demographics-missing block", "final analysis respondents",
    "final task observations"
  ),
  value = c(
    nrow(completion_status),
    sum(T_i == 2L & !finished),
    sum(T_i == 3L & !finished),
    sum(T_i == 3L & finished),
    sum(all_demo_missing), sum(included), nrow(prepared$deltaX)
  ),
  note = c(
    "anonymous ResponseId is the respondent key",
    "all have Progress=50 in these data",
    "seven have Progress=78 and one has Progress=83",
    "35 of these are in the all-demographics-missing block",
    "contains seven two-task unfinished, eight three-task unfinished, and 35 three-task finished respondents",
    "three tasks and valid primary pre-conjoint demographics",
    "three candidate-pair choices per included respondent"
  ),
  stringsAsFactors = FALSE
)
write.csv(completion_key_facts,
          file.path(.tables, "completion_key_facts.csv"), row.names = FALSE)

## Preserve a local, read-only-derived task universe for completion diagnostics.
## This contains only the paired conjoint outcome, its exact 13-coordinate
## contrast, task order, and the completion/sample flags already summarized
## above.  Later assessment never has to reopen or modify the source project.
.encode_profile <- function(d) {
  level_ok <-
    d$candidate_gender %in% c(0, 1) & d$candidate_run %in% c(0, 1) &
    d$Talent %in% prepared$factor_levels$cand_talent &
    d$Agenda %in% prepared$factor_levels$cand_agenda &
    d$Children %in% prepared$factor_levels$cand_child
  if (anyNA(level_ok) || !all(level_ok)) {
    stop("An excluded-sample profile has an unrecognized attribute level.",
         call. = FALSE)
  }
  X <- cbind(
    cand_genderMale = as.numeric(d$candidate_gender == 0),
    cand_runYes = as.numeric(d$candidate_run == 1),
    vapply(prepared$factor_levels$cand_talent[-1L],
           function(x) as.numeric(d$Talent == x), numeric(nrow(d))),
    vapply(prepared$factor_levels$cand_agenda[-1L],
           function(x) as.numeric(d$Agenda == x), numeric(nrow(d))),
    vapply(prepared$factor_levels$cand_child[-1L],
           function(x) as.numeric(d$Children == x), numeric(nrow(d)))
  )
  colnames(X) <- prepared$coordinate_dictionary$name
  storage.mode(X) <- "double"
  X
}
raw_a <- ssi_raw[ssi_raw$variable == "candidateA", , drop = FALSE]
raw_b <- ssi_raw[ssi_raw$variable == "candidateB", , drop = FALSE]
raw_a <- raw_a[order(raw_a$ResponseId, raw_a$election, method = "radix"),
               , drop = FALSE]
raw_b <- raw_b[order(raw_b$ResponseId, raw_b$election, method = "radix"),
               , drop = FALSE]
if (nrow(raw_a) != 3740L || nrow(raw_b) != 3740L ||
    !identical(raw_a$ResponseId, raw_b$ResponseId) ||
    !identical(raw_a$election, raw_b$election) ||
    any(raw_a$candidate_vote + raw_b$candidate_vote != 1L)) {
  stop("The raw completion universe does not form 3,740 paired tasks.",
       call. = FALSE)
}
raw_delta <- .encode_profile(raw_a) - .encode_profile(raw_b)
status_index <- match(raw_a$ResponseId, completion_status$respondent_id)
if (anyNA(status_index)) stop("Raw tasks do not match the completion ledger.",
                             call. = FALSE)
completion_task_audit <- data.frame(
  respondent_id = as.character(raw_a$ResponseId),
  task = as.integer(raw_a$election),
  y = as.numeric(raw_a$candidate_vote),
  as.data.frame(raw_delta, check.names = FALSE),
  eventual_tasks = completion_status$tasks[status_index],
  finished = completion_status$finished[status_index],
  progress = completion_status$progress[status_index],
  all_primary_demographics_missing =
    completion_status$all_primary_demographics_missing[status_index],
  primary_demographics_valid =
    completion_status$primary_demographics_valid[status_index],
  final_analysis_sample =
    completion_status$final_analysis_sample[status_index],
  exclusion_reason = completion_status$exclusion_reason[status_index],
  stringsAsFactors = FALSE
)
primary_task_rows <- completion_task_audit$final_analysis_sample
if (sum(primary_task_rows) != nrow(prepared$deltaX) ||
    !identical(completion_task_audit$respondent_id[primary_task_rows],
               as.character(prepared$respondent_id)) ||
    !identical(completion_task_audit$task[primary_task_rows],
               as.integer(prepared$task)) ||
    !identical(completion_task_audit$y[primary_task_rows],
               as.numeric(prepared$y)) ||
    max(abs(as.matrix(completion_task_audit[
      primary_task_rows, prepared$coordinate_dictionary$name,
      drop = FALSE]) - as.matrix(prepared$deltaX))) > 1e-12) {
  stop("The raw-universe task audit does not reproduce the prepared sample.",
       call. = FALSE)
}
completion_task_path <- file.path(.results, "completion_task_audit.rds")
saveRDS(completion_task_audit, completion_task_path, version = 3)

completion_response_by_task <- aggregate(
  completion_task_audit$y,
  completion_task_audit[c("task", "eventual_tasks", "finished",
                          "final_analysis_sample")],
  function(x) c(rate = mean(x), respondents = length(x))
)
unpacked <- completion_response_by_task$x
if (is.list(unpacked)) unpacked <- do.call(rbind, unpacked)
unpacked <- as.matrix(unpacked)
completion_response_by_task$x <- NULL
completion_response_by_task$candidate_A_choice_rate <- unpacked[, "rate"]
completion_response_by_task$respondents <- as.integer(unpacked[, "respondents"])
write.csv(completion_response_by_task,
          file.path(.tables, "completion_response_by_task.csv"),
          row.names = FALSE)

## ----- Theoretical profile and contrast support ---------------------------

coordinate_names <- prepared$coordinate_dictionary$name
p <- length(coordinate_names)
blocks <- prepared$attribute_blocks
profile_grid <- expand.grid(
  cand_gender = prepared$factor_levels$cand_gender,
  cand_run = prepared$factor_levels$cand_run,
  cand_talent = prepared$factor_levels$cand_talent,
  cand_agenda = prepared$factor_levels$cand_agenda,
  cand_child = prepared$factor_levels$cand_child,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
profile_X <- matrix(0, nrow(profile_grid), p,
                    dimnames = list(NULL, coordinate_names))
profile_X[, 1L] <- profile_grid$cand_gender == "Male"
profile_X[, 2L] <- profile_grid$cand_run == "Yes"
for (j in 3:8) {
  profile_X[, j] <- profile_grid$cand_talent ==
    prepared$coordinate_dictionary$level[j]
}
for (j in 9:10) {
  profile_X[, j] <- profile_grid$cand_agenda ==
    prepared$coordinate_dictionary$level[j]
}
for (j in 11:13) {
  profile_X[, j] <- profile_grid$cand_child ==
    prepared$coordinate_dictionary$level[j]
}
storage.mode(profile_X) <- "double"
stopifnot(nrow(profile_X) == 336L, !anyDuplicated(as.data.frame(profile_X)))

.key <- function(M) {
  M <- as.matrix(M)
  apply(M, 1L, paste, collapse = ",")
}
all_ordered_delta <- do.call(
  rbind,
  lapply(seq_len(nrow(profile_X)), function(a) {
    matrix(profile_X[a, ], nrow(profile_X), p, byrow = TRUE) - profile_X
  })
)
colnames(all_ordered_delta) <- coordinate_names
all_ordered_key <- .key(all_ordered_delta)
theoretical_frequency <- table(all_ordered_key)
theoretical_support <- unique(all_ordered_delta)
if (nrow(theoretical_support) != 35217L) {
  stop("The 13-coordinate theoretical contrast support is not 35,217.",
       call. = FALSE)
}

## The transparent 91-vector witness set C*: coordinate unit vectors;
## within-attribute differences; and across-attribute sums.
unit <- diag(p)
colnames(unit) <- coordinate_names
cstar <- list()
cstar_meta <- list()
.append_cstar <- function(v, kind, j, k = NA_integer_) {
  idx <- length(cstar) + 1L
  cstar[[idx]] <<- v
  cstar_meta[[idx]] <<- data.frame(
    cstar_id = idx, kind = kind, coordinate_j = j, coordinate_k = k,
    name_j = coordinate_names[j],
    name_k = if (is.na(k)) NA_character_ else coordinate_names[k],
    stringsAsFactors = FALSE
  )
}
for (j in seq_len(p)) .append_cstar(unit[j, ], "coordinate_unit", j)
for (b in blocks) {
  if (length(b) >= 2L) {
    pairs <- combn(b, 2L)
    for (h in seq_len(ncol(pairs))) {
      j <- pairs[1L, h]; k <- pairs[2L, h]
      .append_cstar(unit[j, ] - unit[k, ],
                    "within_attribute_difference", j, k)
    }
  }
}
block_id <- integer(p)
for (b in seq_along(blocks)) block_id[blocks[[b]]] <- b
for (j in seq_len(p - 1L)) {
  for (k in (j + 1L):p) {
    if (block_id[j] != block_id[k]) {
      .append_cstar(unit[j, ] + unit[k, ], "across_attribute_sum", j, k)
    }
  }
}
Cstar <- do.call(rbind, cstar)
colnames(Cstar) <- coordinate_names
cstar_meta <- do.call(rbind, cstar_meta)
if (nrow(Cstar) != 91L || anyDuplicated(.key(Cstar))) {
  stop("C* must contain 91 distinct nonzero contrasts.", call. = FALSE)
}
cstar_key <- .key(Cstar)
if (!all(cstar_key %in% names(theoretical_frequency))) {
  stop("A C* witness is absent from theoretical profile support.", call. = FALSE)
}

## The first-moment design spans (1,d); the quadratic design spans all 91
## unique entries of a symmetric 13x13 covariance matrix.
mean_design <- cbind(intercept = 1, Cstar)
pair_index <- which(upper.tri(matrix(0, p, p)), arr.ind = TRUE)
quadratic_design <- cbind(
  Cstar^2,
  vapply(seq_len(nrow(pair_index)), function(h) {
    2 * Cstar[, pair_index[h, 1L]] * Cstar[, pair_index[h, 2L]]
  }, numeric(nrow(Cstar)))
)
mean_rank <- qr(mean_design)$rank
quadratic_rank <- qr(quadratic_design)$rank
if (mean_rank != 14L || quadratic_rank != 91L) {
  stop("C* failed the advertised mean/covariance rank audit.", call. = FALSE)
}

## ----- Realized support and repeated sequences ----------------------------

realized_key <- .key(prepared$deltaX)
cell_n <- as.integer(table(realized_key))
cell_keys <- names(table(realized_key))
cell_y_sum <- as.numeric(rowsum(prepared$y, realized_key, reorder = TRUE))
cell_matrix <- do.call(rbind, strsplit(cell_keys, ",", fixed = TRUE))
storage.mode(cell_matrix) <- "numeric"
colnames(cell_matrix) <- coordinate_names
realized_cells <- data.frame(
  design_cell = cell_keys,
  task_count = cell_n,
  choice_a_rate = cell_y_sum / cell_n,
  as.data.frame(cell_matrix, check.names = FALSE),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
realized_cells <- realized_cells[
  order(-realized_cells$task_count, realized_cells$design_cell), , drop = FALSE
]
write.csv(realized_cells,
          file.path(.tables, "design_realized_cells.csv"), row.names = FALSE)

cstar_realized_tasks <- tabulate(match(realized_key, cstar_key),
                                 nbins = nrow(Cstar))
cstar_realized_respondents <- vapply(seq_len(nrow(Cstar)), function(j) {
  length(unique(prepared$respondent_id[realized_key == cstar_key[j]]))
}, integer(1L))
cstar_uniform_count <- as.integer(theoretical_frequency[cstar_key])
cstar_table <- cbind(
  cstar_meta,
  data.frame(
    theoretical_ordered_profile_pairs = cstar_uniform_count,
    uniform_independent_probability =
      cstar_uniform_count / nrow(all_ordered_delta),
    realized_tasks = cstar_realized_tasks,
    realized_respondents = cstar_realized_respondents,
    as.data.frame(Cstar, check.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)
write.csv(cstar_table, file.path(.tables, "design_cstar.csv"), row.names = FALSE)

repeat_rows <- list()
rr <- 0L
for (id in unique(prepared$respondent_id)) {
  ii <- which(prepared$respondent_id == id)
  if (length(ii) < 2L) next
  cmb <- combn(ii, 2L)
  same <- vapply(seq_len(ncol(cmb)), function(h) {
    identical(unname(prepared$deltaX[cmb[1L, h], ]),
              unname(prepared$deltaX[cmb[2L, h], ]))
  }, logical(1L))
  if (!any(same)) next
  for (h in which(same)) {
    rr <- rr + 1L
    i1 <- cmb[1L, h]; i2 <- cmb[2L, h]
    repeat_rows[[rr]] <- data.frame(
      respondent_id = id,
      task_1 = prepared$task[i1], task_2 = prepared$task[i2],
      y_1 = prepared$y[i1], y_2 = prepared$y[i2],
      design_cell = realized_key[i1],
      member_of_cstar = realized_key[i1] %in% cstar_key,
      stringsAsFactors = FALSE
    )
  }
}
design_repeats <- if (length(repeat_rows)) do.call(rbind, repeat_rows) else {
  data.frame(
    respondent_id = character(), task_1 = integer(), task_2 = integer(),
    y_1 = numeric(), y_2 = numeric(), design_cell = character(),
    member_of_cstar = logical(), stringsAsFactors = FALSE
  )
}
write.csv(design_repeats, file.path(.tables, "design_repeats.csv"),
          row.names = FALSE)

zero_key <- paste(rep(0, p), collapse = ",")
rank_summary <- data.frame(
  diagnostic = c(
    "profile basis dimension", "theoretical profiles",
    "theoretical distinct contrasts including zero", "C* nonzero witnesses",
    "rank of [1,C*]", "rank of symmetric quadratic map on C*",
    "realized distinct contrast cells", "realized singleton cells",
    "realized zero-contrast tasks", "within-respondent repeated contrasts",
    "within-respondent repeated C* contrasts"
  ),
  value = c(
    p, nrow(profile_X), nrow(theoretical_support), nrow(Cstar),
    mean_rank, quadratic_rank, length(unique(realized_key)),
    sum(cell_n == 1L), sum(realized_key == zero_key), nrow(design_repeats),
    sum(design_repeats$member_of_cstar)
  ),
  interpretation = c(
    "13 reference-coded profile coordinates",
    "2 x 2 x 7 x 3 x 4",
    "ordered profile differences collapsed to unique d",
    "13 units + 19 within-block differences + 59 across-block sums",
    "full intercept/mean span",
    "full 13(13+1)/2 covariance span",
    "empirical support is sparse relative to theoretical support",
    "cells observed once in 3,573 analysis tasks",
    "candidate profiles have identical 13-coordinate coding",
    "one respondent contributes the only exact repeated contrast pair",
    "no C* contrast is repeated within a respondent"
  ),
  stringsAsFactors = FALSE
)
write.csv(rank_summary, file.path(.tables, "design_rank_summary.csv"),
          row.names = FALSE)

rank_by_q <- data.frame(
  q = 0:12,
  covariance_rank_bound = 0:12,
  conditional_identification_witness_pass = TRUE,
  condition = paste(
    "Conditional on independent full profile support with positive cross-task",
    "probability; loading rotations are normalized only computationally."
  ),
  stringsAsFactors = FALSE
)
write.csv(rank_by_q, file.path(.tables, "design_rank_by_q.csv"),
          row.names = FALSE)

design_summary <- data.frame(
  item = c(
    "fielded randomizer/QSF available", "assignment probabilities verified",
    "cross-task positive-support rule verified", "article-level design claim",
    "theoretical rank conclusion", "uniform probability status",
    "smallest C* uniform per-task probability",
    "largest C* uniform per-task probability",
    "smallest C* uniform same-contrast two-task probability",
    "largest C* uniform same-contrast two-task probability"
  ),
  value = c(
    "no", "no", "no",
    "five profile attributes described as independently randomized, without combination restrictions",
    paste(
      "C* gives rank 14 for [1,d] and rank 91 for the symmetric quadratic",
      "map; hence covariance injectivity, conditionally, for every q<=12"
    ),
    "illustrative sensitivity only; not treated as known protocol probability",
    format(min(cstar_uniform_count / nrow(all_ordered_delta)),
           scientific = FALSE, digits = 12),
    format(max(cstar_uniform_count / nrow(all_ordered_delta)),
           scientific = FALSE, digits = 12),
    format(min((cstar_uniform_count / nrow(all_ordered_delta))^2),
           scientific = FALSE, digits = 12),
    format(max((cstar_uniform_count / nrow(all_ordered_delta))^2),
           scientific = FALSE, digits = 12)
  ),
  stringsAsFactors = FALSE
)
write.csv(design_summary, file.path(.tables, "design_summary.csv"),
          row.names = FALSE)

task_design_meta <- cbind(
  prepared$task_meta,
  data.frame(
    design_cell = realized_key,
    realized_cell_frequency = as.integer(table(realized_key)[realized_key]),
    in_cstar = realized_key %in% cstar_key,
    stringsAsFactors = FALSE
  )
)
saveRDS(task_design_meta, file.path(.results, "design_task_metadata.rds"),
        version = 3)

audit <- list(
  schema_version = "sw2022-design-completion-v1",
  completion = list(
    status = completion_status,
    summary = .aggregate_completion,
    key_facts = completion_key_facts,
    task_audit_path = completion_task_path,
    task_audit_md5 = unname(tools::md5sum(completion_task_path)),
    response_by_task = completion_response_by_task,
    estimand = prepared$estimand
  ),
  design = list(
    cstar = cstar_table,
    rank_summary = rank_summary,
    rank_by_q = rank_by_q,
    repeats = design_repeats,
    profile_grid = profile_grid,
    profile_X = profile_X,
    theoretical_support_count = nrow(theoretical_support),
    realized_support_count = length(unique(realized_key)),
    protocol_verified = FALSE,
    protocol_caveat = paste(
      "The fielded randomizer/QSF, exact assignment probabilities, and",
      "cross-task randomization rules are absent from the public package.",
      "Rank conclusions are conditional on the article's advertised",
      "independent full-support design."
    ),
    uniform_probability_caveat = paste(
      "Uniform independent assignment is an illustrative calculation, not a",
      "verified inverse-probability design for empirical estimation."
    )
  ),
  provenance = list(
    source_raw_file = .raw_file,
    source_raw_md5 = unname(tools::md5sum(.raw_file)),
    prepared_file = prepared_path,
    prepared_md5 = unname(tools::md5sum(prepared_path)),
    source_policy = "read-only"
  )
)
saveRDS(audit, file.path(.results, "design_completion_audit.rds"), version = 3)

artifact_paths <- c(
  file.path(.results, "design_completion_audit.rds"),
  file.path(.results, "design_task_metadata.rds"),
  completion_task_path,
  file.path(.tables, "completion_status.csv"),
  file.path(.tables, "completion_summary.csv"),
  file.path(.tables, "completion_missingness.csv"),
  file.path(.tables, "completion_key_facts.csv"),
  file.path(.tables, "completion_response_by_task.csv"),
  file.path(.tables, "design_realized_cells.csv"),
  file.path(.tables, "design_cstar.csv"),
  file.path(.tables, "design_repeats.csv"),
  file.path(.tables, "design_rank_summary.csv"),
  file.path(.tables, "design_rank_by_q.csv"),
  file.path(.tables, "design_summary.csv")
)
write.csv(
  data.frame(
    path = substring(artifact_paths, nchar(.project) + 2L),
    bytes = file.info(artifact_paths)$size,
    md5 = unname(tools::md5sum(artifact_paths)),
    stringsAsFactors = FALSE
  ),
  file.path(.manifests, "design_completion_artifact_manifest.csv"),
  row.names = FALSE
)

cat(sprintf(
  paste0(
    "Completion audit: raw N=%d, final N=%d. Design audit: %d theoretical ",
    "cells, %d realized cells; ranks=%d/%d; exact repeats=%d.\n"
  ),
  nrow(completion_status), sum(included), nrow(theoretical_support),
  length(unique(realized_key)), mean_rank, quadratic_rank,
  nrow(design_repeats)
))
