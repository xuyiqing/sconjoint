#!/usr/bin/env Rscript

## Saha--Weeks (2022): read-only reconstruction of the analysis matrices.
##
## The replication project named by SCONJOINT_APPLICATION_ROOT is an input
## only.  Every artifact produced here is written below applications/sw2022 in
## this repository.  Moderators are deliberately saved on their raw numeric
## scale: scmix_tune_outer_matrix() estimates its centering and scaling on the
## relevant training respondents.

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

.source_root <- Sys.getenv(
  "SCONJOINT_APPLICATION_ROOT",
  unset = "/Users/avidit/Dropbox/Research - Active/Conjoint/ConjointStructural"
)
.source_root <- normalizePath(.source_root, mustWork = TRUE)
if (startsWith(paste0(normalizePath(.app, mustWork = TRUE), "/"),
               paste0(.source_root, "/"))) {
  stop("Local outputs may not be placed inside the source project.",
       call. = FALSE)
}

.raw_file <- file.path(
  .source_root, "data", "replication_materials_saha_weeks_2022", "ssi.csv"
)
.frozen_file <- file.path(
  .source_root, "code", "analysis", "sw2022", "out", "prep_matrices.rds"
)
.legacy_prep_file <- file.path(
  .source_root, "code", "analysis", "sw2022", "04_data_prep.R"
)
stopifnot(file.exists(.raw_file), file.exists(.frozen_file))

ssi_raw <- read.csv(.raw_file, check.names = FALSE, stringsAsFactors = FALSE)
required <- c(
  "ResponseId", "election", "variable", "candidate_vote",
  "candidate_gender", "candidate_run", "Talent", "Agenda", "Children",
  "resp_gender", "Age", "Income", "Education_Level_General",
  "Interests.Political.Affiliation", "Geo.Census.Region..US.",
  "Employment_status", "Finished", "Progress", "Q24.1", "Q23",
  "Q29_1.1", "Q29_2", "Q29_3", "Q29_4", "Q29_5"
)
missing_columns <- setdiff(required, names(ssi_raw))
if (length(missing_columns)) {
  stop("ssi.csv is missing: ", paste(missing_columns, collapse = ", "),
       call. = FALSE)
}
if (anyNA(ssi_raw$ResponseId) || any(ssi_raw$ResponseId == "")) {
  stop("ResponseId contains missing values.", call. = FALSE)
}

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

task_count <- tapply(ssi_raw$election, ssi_raw$ResponseId,
                     function(x) length(unique(x)))
three_ids <- names(task_count)[task_count == 3L]
ssi_three <- ssi_raw[ssi_raw$ResponseId %in% three_ids, , drop = FALSE]
respondent_first <- ssi_three[!duplicated(ssi_three$ResponseId), , drop = FALSE]

valid_components <- data.frame(
  respondent_id = respondent_first$ResponseId,
  gender = !is.na(respondent_first$resp_gender) &
    respondent_first$resp_gender %in% c("female", "male"),
  age = !is.na(respondent_first$Age) & is.finite(respondent_first$Age),
  income = !is.na(respondent_first$Income) &
    respondent_first$Income %in% VALID_INCOME,
  education = !is.na(respondent_first$Education_Level_General) &
    respondent_first$Education_Level_General %in% VALID_EDUC,
  party = !is.na(respondent_first$`Interests.Political.Affiliation`) &
    respondent_first$`Interests.Political.Affiliation` %in% VALID_PARTY,
  region = !is.na(respondent_first$`Geo.Census.Region..US.`) &
    respondent_first$`Geo.Census.Region..US.` %in% VALID_REGION,
  employment = !is.na(respondent_first$Employment_status) &
    respondent_first$Employment_status %in% VALID_EMPLOY,
  check.names = FALSE
)
valid_components$all_primary <- apply(
  valid_components[, setdiff(names(valid_components), "respondent_id"),
                   drop = FALSE],
  1L, all
)
analysis_ids <- valid_components$respondent_id[valid_components$all_primary]
ssi <- ssi_three[ssi_three$ResponseId %in% analysis_ids, , drop = FALSE]

if (length(unique(analysis_ids)) != 1191L) {
  stop("The final complete-case respondent count is not 1,191.",
       call. = FALSE)
}

FACTOR_LEVELS <- list(
  cand_gender = c("Female", "Male"),
  cand_run = c("No", "Yes"),
  cand_talent = c(
    "Assertive", "Collaborative", "Determined to Succeed", "Empathetic",
    "Good Communicator", "Hard-Working", "Tough Negotiator"
  ),
  cand_agenda = c("Very Few Changes", "Moderate Changes", "Complete Overhaul"),
  cand_child = c("No children", "1 child", "2 children", "3 children")
)

ssi$cand_gender <- factor(
  ifelse(ssi$candidate_gender == 1, "Female", "Male"),
  levels = FACTOR_LEVELS$cand_gender
)
ssi$cand_run <- factor(
  ifelse(ssi$candidate_run == 1, "Yes", "No"),
  levels = FACTOR_LEVELS$cand_run
)
ssi$cand_talent <- factor(ssi$Talent, levels = FACTOR_LEVELS$cand_talent)
ssi$cand_agenda <- factor(ssi$Agenda, levels = FACTOR_LEVELS$cand_agenda)
ssi$cand_child <- factor(ssi$Children, levels = FACTOR_LEVELS$cand_child)
if (anyNA(ssi[, names(FACTOR_LEVELS), drop = FALSE])) {
  stop("A fielded profile contains an unrecognized attribute level.",
       call. = FALSE)
}

X <- model.matrix(
  ~ cand_gender + cand_run + cand_talent + cand_agenda + cand_child,
  data = ssi
)[, -1L, drop = FALSE]
colnames(X) <- make.names(colnames(X))
coordinate_names <- c(
  "cand_genderMale", "cand_runYes", "cand_talentCollaborative",
  "cand_talentDetermined.to.Succeed", "cand_talentEmpathetic",
  "cand_talentGood.Communicator", "cand_talentHard.Working",
  "cand_talentTough.Negotiator", "cand_agendaModerate.Changes",
  "cand_agendaComplete.Overhaul", "cand_child1.child",
  "cand_child2.children", "cand_child3.children"
)
if (!identical(colnames(X), coordinate_names)) {
  stop("The 13-coordinate profile basis changed unexpectedly.", call. = FALSE)
}
ssi <- cbind(ssi, X)

cand_a <- ssi[ssi$variable == "candidateA", , drop = FALSE]
cand_b <- ssi[ssi$variable == "candidateB", , drop = FALSE]
cand_a <- cand_a[order(cand_a$ResponseId, cand_a$election, method = "radix"),
                 , drop = FALSE]
cand_b <- cand_b[order(cand_b$ResponseId, cand_b$election, method = "radix"),
                 , drop = FALSE]
if (nrow(cand_a) != 3573L || nrow(cand_b) != 3573L ||
    !identical(cand_a$ResponseId, cand_b$ResponseId) ||
    !identical(cand_a$election, cand_b$election)) {
  stop("Candidate A/B rows do not form 3,573 matched tasks.", call. = FALSE)
}
if (any(cand_a$candidate_vote + cand_b$candidate_vote != 1L)) {
  stop("Candidate choice indicators are not complementary within task.",
       call. = FALSE)
}

Xa <- unname(as.matrix(cand_a[, coordinate_names, drop = FALSE]))
Xb <- unname(as.matrix(cand_b[, coordinate_names, drop = FALSE]))
colnames(Xa) <- colnames(Xb) <- coordinate_names
deltaX <- Xa - Xb
y <- as.numeric(cand_a$candidate_vote)
respondent_id <- as.character(cand_a$ResponseId)
task <- as.integer(cand_a$election)
stopifnot(all(y %in% 0:1), nrow(deltaX) == length(y))

## Primary moderators: all are pre-conjoint panel or demographic variables.
income_map <- setNames(seq_along(VALID_INCOME), VALID_INCOME)
z_educ <- model.matrix(
  ~ e - 1,
  data.frame(e = factor(cand_a$Education_Level_General,
                        levels = VALID_EDUC))
)[, -1L, drop = FALSE]
colnames(z_educ) <- c("educ_Middle", "educ_High")
z_party <- model.matrix(
  ~ p - 1,
  data.frame(p = factor(cand_a$`Interests.Political.Affiliation`,
                        levels = VALID_PARTY))
)[, -1L, drop = FALSE]
colnames(z_party) <- c("party_Republican", "party_Independent")
z_region <- model.matrix(
  ~ r - 1,
  data.frame(r = factor(cand_a$`Geo.Census.Region..US.`,
                        levels = VALID_REGION))
)[, -1L, drop = FALSE]
colnames(z_region) <- c("region_NORTHEAST", "region_SOUTH", "region_WEST")
z_employ <- model.matrix(
  ~ e - 1,
  data.frame(e = factor(cand_a$Employment_status,
                        levels = VALID_EMPLOY))
)[, -1L, drop = FALSE]
colnames(z_employ) <- c(
  "employ_parttime", "employ_homemaker", "employ_not_working",
  "employ_retired", "employ_student"
)
Z_primary <- cbind(
  gender_num = as.numeric(cand_a$resp_gender == "male"),
  age = as.numeric(cand_a$Age),
  income = unname(as.numeric(income_map[cand_a$Income])),
  z_educ, z_party, z_region, z_employ
)
storage.mode(Z_primary) <- "double"
if (ncol(Z_primary) != 15L || any(!is.finite(Z_primary))) {
  stop("Primary moderator matrix must be finite and have 15 columns.",
       call. = FALSE)
}
if (any(vapply(split(seq_along(respondent_id), respondent_id), function(ii) {
  any(abs(sweep(Z_primary[ii, , drop = FALSE], 2L,
                Z_primary[ii[1L], ], `-`)) > 0)
}, logical(1L)))) {
  stop("A primary moderator varies within respondent.", call. = FALSE)
}

## Four post-conjoint variables are retained only for the disclosed 19-Z
## sensitivity.  Missing values stay missing here so that any imputation can be
## estimated within training folds; this object must not be passed directly to
## a fitter that requires a finite Z matrix.
ideo <- ifelse(
  is.na(cand_a$Q24.1) | cand_a$Q24.1 == "", NA_real_,
  as.numeric(cand_a$Q24.1 == "Conservative")
)
vote_missing <- is.na(cand_a$Q23) | cand_a$Q23 == ""
vote_trump <- ifelse(vote_missing, NA_real_,
                     as.numeric(grepl("Trump", cand_a$Q23, fixed = TRUE)))
vote_clinton <- ifelse(vote_missing, NA_real_,
                       as.numeric(grepl("Clinton", cand_a$Q23, fixed = TRUE)))
likert <- c(
  "Strongly disagree" = 1, "Somewhat disagree" = 2,
  "Neither agree nor disagree" = 3, "Somewhat agree" = 4,
  "Strongly agree" = 5
)
q29 <- vapply(
  c("Q29_1.1", "Q29_2", "Q29_3", "Q29_4", "Q29_5"),
  function(nm) unname(likert[as.character(cand_a[[nm]])]),
  numeric(nrow(cand_a))
)
gender_att <- rowMeans(q29, na.rm = TRUE)
gender_att[!rowSums(is.finite(q29))] <- NA_real_
Z_sensitivity19_raw <- cbind(
  Z_primary,
  ideo_conservative = ideo,
  vote_trump = vote_trump,
  vote_clinton = vote_clinton,
  gender_att = gender_att
)
stopifnot(ncol(Z_sensitivity19_raw) == 19L)

coordinate_dictionary <- data.frame(
  coordinate = seq_along(coordinate_names),
  name = coordinate_names,
  attribute = c(
    "candidate gender", "prior run", rep("talent", 6L), rep("agenda", 2L),
    rep("children", 3L)
  ),
  level = c(
    "Male", "Yes", "Collaborative", "Determined to Succeed", "Empathetic",
    "Good Communicator", "Hard-Working", "Tough Negotiator",
    "Moderate Changes", "Complete Overhaul", "1 child", "2 children",
    "3 children"
  ),
  reference = c(
    "Female", "No", rep("Assertive", 6L), rep("Very Few Changes", 2L),
    rep("No children", 3L)
  ),
  stringsAsFactors = FALSE
)
attribute_blocks <- list(
  candidate_gender = 1L,
  prior_run = 2L,
  talent = 3:8,
  agenda = 9:10,
  children = 11:13
)

resp_final <- cand_a[!duplicated(cand_a$ResponseId), , drop = FALSE]
respondent_meta <- data.frame(
  respondent_id = resp_final$ResponseId,
  party = resp_final$`Interests.Political.Affiliation`,
  respondent_gender = resp_final$resp_gender,
  age = as.numeric(resp_final$Age),
  income = resp_final$Income,
  education = resp_final$Education_Level_General,
  region = resp_final$`Geo.Census.Region..US.`,
  employment = resp_final$Employment_status,
  finished = as.logical(resp_final$Finished),
  progress = as.numeric(resp_final$Progress),
  stringsAsFactors = FALSE
)

.profile_fields <- function(prefix, d) {
  out <- data.frame(
    gender = as.character(d$cand_gender),
    prior_run = as.character(d$cand_run),
    talent = as.character(d$cand_talent),
    agenda = as.character(d$cand_agenda),
    children = as.character(d$cand_child),
    stringsAsFactors = FALSE
  )
  names(out) <- paste0(prefix, names(out))
  out
}
task_meta <- cbind(
  data.frame(
    respondent_id = respondent_id,
    task = task,
    y = y,
    profile_a_label = "candidateA",
    profile_b_label = "candidateB",
    stringsAsFactors = FALSE
  ),
  .profile_fields("a_", cand_a),
  .profile_fields("b_", cand_b)
)

## Equivalence gate against the earlier frozen reconstruction.  We do not use
## the frozen Z because its continuous columns were scaled before folding.
frozen <- readRDS(.frozen_file)
if (!identical(unname(deltaX), unname(as.matrix(frozen$DeltaX))) ||
    !identical(y, as.numeric(frozen$Y)) ||
    !identical(respondent_id, as.character(frozen$respondent))) {
  stop("Raw reconstruction does not reproduce the frozen DeltaX/Y/order.",
       call. = FALSE)
}

sample_flow <- data.frame(
  stage = c(
    "raw SSI", "exactly three distinct tasks",
    "three tasks plus valid primary demographics"
  ),
  respondents = c(
    length(unique(ssi_raw$ResponseId)), length(three_ids), length(analysis_ids)
  ),
  tasks = c(
    length(unique(paste(ssi_raw$ResponseId, ssi_raw$election, sep = "::"))),
    length(unique(paste(ssi_three$ResponseId, ssi_three$election, sep = "::"))),
    nrow(cand_a)
  ),
  profile_rows = c(nrow(ssi_raw), nrow(ssi_three), nrow(ssi)),
  stringsAsFactors = FALSE
)
write.csv(sample_flow, file.path(.tables, "prep_sample_flow.csv"),
          row.names = FALSE)
id_to_response <- tapply(
  ssi_raw$ResponseId, ssi_raw$id,
  function(x) length(unique(as.character(x)))
)
identifier_audit <- data.frame(
  candidate_key = c("ResponseId", "id"),
  unique_nonmissing_values = c(
    length(unique(ssi_raw$ResponseId[!is.na(ssi_raw$ResponseId)])),
    length(unique(ssi_raw$id[!is.na(ssi_raw$id)]))
  ),
  numeric_key_values_mapping_to_multiple_respondents = c(
    0L, sum(id_to_response > 1L)
  ),
  selected_respondent_key = c(TRUE, FALSE),
  note = c(
    "stable anonymous Qualtrics respondent identifier",
    "not unique: at least one numeric value maps to multiple ResponseIds"
  ),
  stringsAsFactors = FALSE
)
write.csv(identifier_audit,
          file.path(.tables, "prep_identifier_audit.csv"), row.names = FALSE)
write.csv(coordinate_dictionary,
          file.path(.tables, "prep_coordinate_dictionary.csv"),
          row.names = FALSE)
write.csv(
  data.frame(
    moderator = colnames(Z_sensitivity19_raw),
    primary_15 = seq_len(19L) <= 15L,
    timing = c(rep("pre-conjoint panel/demographic", 15L),
               rep("post-conjoint survey item", 4L)),
    raw_missing_task_rows = colSums(is.na(Z_sensitivity19_raw)),
    preprocessing = c(
      rep("training-fold centering/scaling", 15L),
      rep("training-fold median imputation, then centering/scaling", 4L)
    ),
    stringsAsFactors = FALSE
  ),
  file.path(.tables, "prep_moderator_dictionary.csv"), row.names = FALSE
)

prepared <- list(
  schema_version = "sw2022-prep-v1",
  application = "Saha--Weeks SSI conjoint",
  deltaX = deltaX,
  y = y,
  Z_primary = Z_primary,
  Z_sensitivity19_raw = Z_sensitivity19_raw,
  z19_imputation_recipe = list(
    fields = colnames(Z_sensitivity19_raw)[16:19],
    rule = paste(
      "Within each training fold, replace missing values by that training",
      "fold's column median; freeze those medians for held-out respondents;",
      "then apply training-fold centering/scaling."
    )
  ),
  respondent_id = respondent_id,
  task = task,
  Xa = Xa,
  Xb = Xb,
  task_meta = task_meta,
  respondent_meta = respondent_meta,
  coordinate_dictionary = coordinate_dictionary,
  attribute_blocks = attribute_blocks,
  factor_levels = FACTOR_LEVELS,
  sample = list(N = 1191L, task_rows = 3573L, T = 3L, p = 13L,
                primary_z = 15L, sensitivity_z = 19L),
  estimand = paste(
    "Equal-weight distribution of the 1,191 SSI respondents with all three",
    "tasks and valid primary pre-conjoint demographics; no survey weights."
  ),
  provenance = list(
    source_root = .source_root,
    source_policy = "read-only",
    raw_file = .raw_file,
    raw_md5 = unname(tools::md5sum(.raw_file)),
    frozen_gate_file = .frozen_file,
    frozen_gate_md5 = unname(tools::md5sum(.frozen_file)),
    response_identifier = "ResponseId (the numeric id column is not unique)",
    outcome = "y=1 iff candidate A is chosen",
    contrast = "deltaX = X(candidate A) - X(candidate B)",
    primary_z_timing = "15 pre-conjoint panel/demographic moderators",
    sensitivity_z_timing = "four post-conjoint fields added only in sensitivity"
  )
)
saveRDS(prepared, file.path(.results, "prep_analysis_data.rds"), version = 3)

long_a <- cbind(
  data.frame(respondent_id = respondent_id, task = task, profile = 1L,
             choice = y, stringsAsFactors = FALSE),
  as.data.frame(Xa, check.names = FALSE),
  as.data.frame(Z_primary, check.names = FALSE)
)
long_b <- cbind(
  data.frame(respondent_id = respondent_id, task = task, profile = 2L,
             choice = 1 - y, stringsAsFactors = FALSE),
  as.data.frame(Xb, check.names = FALSE),
  as.data.frame(Z_primary, check.names = FALSE)
)
prep_long <- rbind(long_a, long_b)
prep_long <- prep_long[order(prep_long$respondent_id, prep_long$task,
                             prep_long$profile, method = "radix"), , drop = FALSE]
rownames(prep_long) <- NULL
saveRDS(prep_long, file.path(.results, "prep_long.rds"), version = 3)

source_paths <- c(.raw_file, .frozen_file, .legacy_prep_file)
source_manifest <- data.frame(
  role = c("raw replication data", "frozen reconstruction equivalence gate",
           "legacy preparation code (provenance only)"),
  path = source_paths,
  exists = file.exists(source_paths),
  bytes = ifelse(file.exists(source_paths), file.info(source_paths)$size, NA_real_),
  md5 = ifelse(file.exists(source_paths),
               unname(tools::md5sum(source_paths)), NA_character_),
  access = "read-only; no source-tree writes",
  stringsAsFactors = FALSE
)
write.csv(source_manifest,
          file.path(.manifests, "prep_source_manifest.csv"), row.names = FALSE)

artifact_paths <- c(
  file.path(.results, "prep_analysis_data.rds"),
  file.path(.results, "prep_long.rds"),
  file.path(.tables, "prep_sample_flow.csv"),
  file.path(.tables, "prep_identifier_audit.csv"),
  file.path(.tables, "prep_coordinate_dictionary.csv"),
  file.path(.tables, "prep_moderator_dictionary.csv")
)
write.csv(
  data.frame(
    path = substring(artifact_paths, nchar(.project) + 2L),
    bytes = file.info(artifact_paths)$size,
    md5 = unname(tools::md5sum(artifact_paths)),
    stringsAsFactors = FALSE
  ),
  file.path(.manifests, "prep_artifact_manifest.csv"), row.names = FALSE
)

cat(sprintf(
  "Prepared Saha--Weeks: N=%d, tasks=%d, p=%d, primary Z=%d.\n",
  length(unique(respondent_id)), nrow(deltaX), ncol(deltaX), ncol(Z_primary)
))
