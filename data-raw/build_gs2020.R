###############################################################################
# data-raw/build_gs2020.R
#
# Build the bundled `gs2020` dataset from Graham-Svolik (2020) replication data.
#
# Provenance: Graham, Matthew H. and Milan W. Svolik. 2020.
#   "Democracy in America?  Partisanship, Polarization, and the
#   Robustness of Support for Democracy in the United States."
#   American Political Science Review 114(2):392-409.
#
# Source: Experimental data from published replication materials.
###############################################################################

library(data.table)

# --- Load raw data -----------------------------------------------------------

data_path <- file.path("~", "Dropbox", "Projects", "ConjointStructural",
                        "data", "replication_materials_graham_svolik_2020",
                        "data", "data_experiment.csv")
gs_raw <- fread(data_path)

cat("Raw rows:", nrow(gs_raw), "\n")
cat("Unique respondents:", uniqueN(gs_raw$id), "\n")

# --- Clean: keep respondents with 13 complete matchups (26 rows) -------------

rows_per <- gs_raw[, .N, by = id]
keep_ids <- rows_per[N == 26, id]
gs <- gs_raw[id %in% keep_ids]
cat("After keeping complete respondents:", nrow(gs), "rows,",
    uniqueN(gs$id), "respondents\n")

# Drop matchups with missing outcome
gs <- gs[!is.na(c_win)]

# --- Create factor attribute columns ----------------------------------------

# Democracy behavior codes (reference = g_boardElect)
dem_levels <- c("g_boardElect",
                "g_committee", "g_officestructure", "g_procedure",
                "g_progEval", "g_record", "g_schedule",
                "u_banProtest", "u_court", "u_execRule",
                "u_gerry2", "u_gerry10", "u_journalists", "u_limitVote",
                "v_affair", "v_tax")

# Ideology mapping
ideo_map <- c(
  "Extremely liberal"      = 1,
  "Liberal"                = 2,
  "Slightly liberal"       = 3,
  "Moderate"               = 4,
  "Slightly conservative"  = 5,
  "Conservative"           = 6,
  "Extremely conservative" = 7
)

# --- Drop rows with NA in key columns ---------------------------------------

key_cols <- c("c_Party", "c_dem_code", "c_sex", "c_race",
              "c_prox", "ideo", "pid7", "age", "c_win")
gs <- gs[complete.cases(gs[, ..key_cols])]

# Re-filter to respondents with >= 10 matchups after dropping
matchups_per <- gs[, .(n = uniqueN(matchNum)), by = id]
keep_ids2 <- matchups_per[n >= 10, id]
gs <- gs[id %in% keep_ids2]

cat("After cleaning:", nrow(gs), "rows,", uniqueN(gs$id), "respondents\n")

# --- Reshape to long format (2 profiles per matchup) ------------------------

# Take candNum=1 rows, extract both candidate and opponent as profiles
cand1 <- gs[candNum == 1]
cat("Matchups (candNum=1):", nrow(cand1), "\n")

# Profile 1 (candidate)
p1 <- cand1[, .(
  respondent  = as.character(id),
  task        = matchNum,
  profile     = 1L,
  choice      = as.integer(c_win),
  cand_party  = factor(c_Party, levels = c("Democrat", "Republican")),
  dem_code    = factor(c_dem_code, levels = dem_levels),
  cand_sex    = factor(c_sex, levels = c("Male", "Female")),
  cand_race   = factor(c_race, levels = c("White", "Black", "Hispanic", "Asian")),
  policy_prox = c_prox,
  resp_ideo   = ideo_map[ideo],
  resp_pid    = as.numeric(pid7),
  resp_age    = as.numeric(age)
)]

# Profile 2 (opponent)
p2 <- cand1[, .(
  respondent  = as.character(id),
  task        = matchNum,
  profile     = 2L,
  choice      = 1L - as.integer(c_win),
  cand_party  = factor(o_Party, levels = c("Democrat", "Republican")),
  dem_code    = factor(o_dem_code, levels = dem_levels),
  cand_sex    = factor(o_sex, levels = c("Male", "Female")),
  cand_race   = factor(o_race, levels = c("White", "Black", "Hispanic", "Asian")),
  policy_prox = o_prox,
  resp_ideo   = ideo_map[ideo],
  resp_pid    = as.numeric(pid7),
  resp_age    = as.numeric(age)
)]

dat <- rbind(p1, p2)
setorder(dat, respondent, task, profile)
dat <- as.data.frame(dat)

# Drop any remaining NAs
dat <- dat[complete.cases(dat), ]

# Ensure every (respondent, task) has exactly 2 profiles
pair_counts <- table(paste(dat$respondent, dat$task))
complete_pairs <- names(pair_counts[pair_counts == 2])
dat$rt_key <- paste(dat$respondent, dat$task)
dat <- dat[dat$rt_key %in% complete_pairs, ]
dat$rt_key <- NULL

gs2020 <- dat

# --- Summary ----------------------------------------------------------------

cat("Final dataset:", nrow(gs2020), "rows\n")
cat("Respondents:", length(unique(gs2020$respondent)), "\n")
cat("Tasks:", nrow(gs2020) / 2, "\n")

# --- Save -------------------------------------------------------------------

usethis::use_data(gs2020, overwrite = TRUE)
