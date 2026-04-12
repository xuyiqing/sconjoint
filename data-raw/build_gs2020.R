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
#
# Attributes (P_BETA = 30): copartisan, p1 (ordinal 1-4), p2 (ordinal 1-4),
#   dem_code (16-level factor), cand_sex (2), cand_race (4), cand_pro (9).
# Moderators (P_Z = 12): ideo, pid7, trump, age, female, race_black,
#   race_asian, race_other, educ, hhi, auth, knowl.
###############################################################################

library(data.table)

# --- Load raw data -----------------------------------------------------------

data_path <- file.path("~", "Dropbox", "Projects", "ConjointStructural",
                        "data", "replication_materials_graham_svolik_2020",
                        "data", "data_experiment.csv")
gs_raw <- fread(data_path)

cat("Raw rows:", nrow(gs_raw), "\n")
cat("Unique respondents:", uniqueN(gs_raw$id), "\n")

# --- Clean: keep respondents with 26 rows (13 matchups x 2 candNum) ---------

rows_per <- gs_raw[, .N, by = id]
keep_ids <- rows_per[N == 26, id]
gs <- gs_raw[id %in% keep_ids]
cat("After keeping complete respondents:", nrow(gs), "rows,",
    uniqueN(gs$id), "respondents\n")

# Drop matchups with missing outcome
gs <- gs[!is.na(c_win)]
cat("After dropping NA c_win:", nrow(gs), "rows,",
    uniqueN(gs$id), "respondents\n")

# --- Ideology mapping -------------------------------------------------------

ideo_map <- c(
  "Extremely liberal"      = 1,
  "Liberal"                = 2,
  "Slightly liberal"       = 3,
  "Moderate"               = 4,
  "Slightly conservative"  = 5,
  "Conservative"           = 6,
  "Extremely conservative" = 7
)

# Trump approval mapping
trump_map <- c(
  "Strongly disapprove"  = 1,
  "Somewhat disapprove"  = 2,
  "Somewhat approve"     = 3,
  "Strongly approve"     = 4
)

# Education mapping
educ_map <- c(
  "Did not complete high school"     = 1,
  "High school graduate"             = 2,
  "Some college, no degree"          = 3,
  "Associate's degree"               = 4,
  "Bachelor's degree"                = 5,
  "Graduate or professional degree"  = 6
)

# Household income mapping (ordinal 1-24)
hhi_levels <- c(
  "Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999",
  "$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999",
  "$40,000 to $44,999", "$45,000 to $49,999", "$50,000 to $54,999",
  "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999",
  "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999",
  "$85,000 to $89,999", "$90,000 to $94,999", "$95,000 to $99,999",
  "$100,000 to $124,999", "$125,000 to $149,999",
  "$150,000 to $174,999", "$175,000 to $199,999",
  "$200,000 to $249,999", "$250,000 and above"
)
hhi_map <- setNames(seq_along(hhi_levels), hhi_levels)

# Profession levels
pro_levels <- c("Business executive", "Farmer", "Lawyer",
                "Legislative staffer", "Police officer",
                "Served in the army", "Served in the navy",
                "Small business owner", "Teacher")

# Democracy behavior levels (reference = g_boardElect)
dem_levels <- c("g_boardElect",
                "g_committee", "g_officestructure", "g_procedure",
                "g_progEval", "g_record", "g_schedule",
                "u_banProtest", "u_court", "u_execRule",
                "u_gerry2", "u_gerry10", "u_journalists", "u_limitVote",
                "v_affair", "v_tax")

# --- Build moderator columns on candNum=1 rows (one per matchup) -----------

gs1 <- gs[candNum == 1]

# Compute moderators (raw, unstandardized)
gs1[, resp_ideo        := ideo_map[ideo]]
gs1[, resp_pid         := as.numeric(pid7)]
gs1[, resp_trump       := trump_map[trump]]
gs1[, resp_age         := as.numeric(age)]
gs1[, resp_female      := as.numeric(gender == "Female")]
gs1[, resp_race_black  := as.numeric(race == "Black")]
gs1[, resp_race_asian  := as.numeric(race == "Asian or Pacific Islander")]
gs1[, resp_race_other  := as.numeric(race %in% c("Other",
                                                   "American Indian or Alaska Native",
                                                   "Prefer not to answer"))]
gs1[, resp_educ        := educ_map[educ]]
gs1[, resp_income      := hhi_map[hhi]]
gs1[, resp_auth        := as.numeric(auth_total)]
gs1[, resp_knowledge   := as.numeric(knowl_anes_total)]

# Drop respondents with any missing moderator
z_cols <- c("resp_ideo", "resp_pid", "resp_trump", "resp_age", "resp_female",
            "resp_race_black", "resp_race_asian", "resp_race_other",
            "resp_educ", "resp_income", "resp_auth", "resp_knowledge")
gs1[, z_complete := complete.cases(gs1[, ..z_cols])]
valid_ids <- gs1[, .(all_valid = all(z_complete)), by = id]
keep_ids_z <- valid_ids[all_valid == TRUE, id]
gs1 <- gs1[id %in% keep_ids_z]
gs1[, z_complete := NULL]

cat("After dropping missing moderators:", nrow(gs1), "matchups,",
    uniqueN(gs1$id), "respondents\n")

# --- Reshape to long format (2 profiles per matchup) ------------------------

# Profile 1 (candidate)
p1 <- gs1[, .(
  respondent     = as.character(id),
  task           = matchNum,
  profile        = 1L,
  choice         = as.integer(c_win),
  copartisan     = factor(ifelse(
    (c_Party == "Democrat" & pid7 < 0) | (c_Party == "Republican" & pid7 > 0),
    "Co-partisan", "Not"), levels = c("Not", "Co-partisan")),
  p1             = as.numeric(c_p1_num),
  p2             = as.numeric(c_p2_num),
  dem_code       = factor(c_dem_code, levels = dem_levels),
  cand_sex       = factor(c_sex, levels = c("Male", "Female")),
  cand_race      = factor(c_race, levels = c("White", "Black", "Hispanic", "Asian")),
  cand_pro       = factor(c_pro, levels = pro_levels),
  resp_ideo      = resp_ideo,
  resp_pid       = resp_pid,
  resp_trump     = resp_trump,
  resp_age       = resp_age,
  resp_female    = resp_female,
  resp_race_black  = resp_race_black,
  resp_race_asian  = resp_race_asian,
  resp_race_other  = resp_race_other,
  resp_educ      = resp_educ,
  resp_income    = resp_income,
  resp_auth      = resp_auth,
  resp_knowledge = resp_knowledge
)]

# Profile 2 (opponent)
p2 <- gs1[, .(
  respondent     = as.character(id),
  task           = matchNum,
  profile        = 2L,
  choice         = 1L - as.integer(c_win),
  copartisan     = factor(ifelse(
    (o_Party == "Democrat" & pid7 < 0) | (o_Party == "Republican" & pid7 > 0),
    "Co-partisan", "Not"), levels = c("Not", "Co-partisan")),
  p1             = as.numeric(o_p1_num),
  p2             = as.numeric(o_p2_num),
  dem_code       = factor(o_dem_code, levels = dem_levels),
  cand_sex       = factor(o_sex, levels = c("Male", "Female")),
  cand_race      = factor(o_race, levels = c("White", "Black", "Hispanic", "Asian")),
  cand_pro       = factor(o_pro, levels = pro_levels),
  resp_ideo      = resp_ideo,
  resp_pid       = resp_pid,
  resp_trump     = resp_trump,
  resp_age       = resp_age,
  resp_female    = resp_female,
  resp_race_black  = resp_race_black,
  resp_race_asian  = resp_race_asian,
  resp_race_other  = resp_race_other,
  resp_educ      = resp_educ,
  resp_income    = resp_income,
  resp_auth      = resp_auth,
  resp_knowledge = resp_knowledge
)]

dat <- rbind(p1, p2)
setorder(dat, respondent, task, profile)
gs2020 <- as.data.frame(dat)

# Verify complete pairs
pair_counts <- table(paste(gs2020$respondent, gs2020$task))
complete_pairs <- names(pair_counts[pair_counts == 2])
gs2020$rt_key <- paste(gs2020$respondent, gs2020$task)
gs2020 <- gs2020[gs2020$rt_key %in% complete_pairs, ]
gs2020$rt_key <- NULL

# --- Summary ----------------------------------------------------------------

cat("Final dataset:", nrow(gs2020), "rows\n")
cat("Respondents:", length(unique(gs2020$respondent)), "\n")
cat("Tasks:", nrow(gs2020) / 2, "\n")

# --- Save -------------------------------------------------------------------

usethis::use_data(gs2020, overwrite = TRUE)
