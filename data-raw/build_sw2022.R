###############################################################################
# data-raw/build_sw2022.R
#
# Build the bundled `sw2022` dataset from Saha-Weeks (2022) replication data.
#
# Provenance: Saha, Sparsha and Jessica L. P. Weeks. 2022. "Ambitious
#   Women: Gender and Voter Perceptions of Candidate Ambition."
#   Political Behavior 44(2):779-805.
#
# Source: SSI survey data from published replication materials.
###############################################################################

library(data.table)

# --- Load raw data -----------------------------------------------------------

data_path <- file.path("~", "Dropbox", "Projects", "ConjointStructural",
                        "data", "replication_materials_saha_weeks_2022", "ssi.csv")
ssi_raw <- fread(data_path)

cat("Raw rows:", nrow(ssi_raw), "\n")
cat("Unique respondents:", length(unique(ssi_raw$ResponseId)), "\n")

# --- Clean: drop respondents with fewer than 3 elections ---------------------

elections_per_resp <- ssi_raw[, .(n_elections = uniqueN(election)), by = ResponseId]
keep_resp <- elections_per_resp[n_elections == 3, ResponseId]
ssi <- ssi_raw[ResponseId %in% keep_resp]

# --- Clean: drop respondents with missing/invalid demographics ---------------

VALID_INCOME <- c("Less than $ 20,000", "$ 20,000 - $ 29,999",
                  "$ 30,000 - $ 39,999", "$ 40,000 - $ 49,999",
                  "$ 50,000 - $ 59,999", "$ 60,000 - $ 74,999",
                  "$ 75,000 - $ 99,999", "$ 100,000 - $ 149,999",
                  "$ 150,000 +")
VALID_PARTY  <- c("Democrat", "Republican (GOP)", "Independent")
VALID_EDUC   <- c("Low", "Middle", "High")
VALID_REGION <- c("MIDWEST", "NORTHEAST", "SOUTH", "WEST")
VALID_EMPLOY <- c("employed fulltime", "employed part-time (less than 32 hours)",
                  "Homemaker", "not working/looking for work",
                  "retired/unable to work/disabled", "student/at school")

ssi[, demo_valid := (
  !is.na(resp_gender) & resp_gender %in% c("female", "male") &
  !is.na(Age) &
  Income %in% VALID_INCOME &
  Education_Level_General %in% VALID_EDUC &
  Interests.Political.Affiliation %in% VALID_PARTY &
  `Geo.Census.Region..US.` %in% VALID_REGION &
  Employment_status %in% VALID_EMPLOY
)]

valid_resp <- ssi[, .(all_valid = all(demo_valid)), by = ResponseId]
keep_resp_demo <- valid_resp[all_valid == TRUE, ResponseId]
ssi <- ssi[ResponseId %in% keep_resp_demo]
ssi[, demo_valid := NULL]

cat("After cleaning:", nrow(ssi), "rows,",
    length(unique(ssi$ResponseId)), "respondents\n")

# --- Create factor attribute columns ----------------------------------------

ssi[, cand_gender := factor(ifelse(candidate_gender == 1, "Female", "Male"),
                            levels = c("Male", "Female"))]
ssi[, prior_office := factor(ifelse(candidate_run == 1, "Yes", "No"),
                             levels = c("No", "Yes"))]
ssi[, talent := factor(Talent,
                       levels = c("Assertive", "Collaborative",
                                  "Determined to Succeed", "Empathetic",
                                  "Good Communicator", "Hard-Working",
                                  "Tough Negotiator"))]
ssi[, agenda := factor(Agenda,
                       levels = c("Very Few Changes", "Moderate Changes",
                                  "Complete Overhaul"))]
ssi[, children := factor(Children,
                         levels = c("No children", "1 child", "2 children",
                                    "3 children"))]

# --- Respondent-level covariates --------------------------------------------

ssi[, resp_female := as.numeric(resp_gender == "female")]
ssi[, age := as.numeric(Age)]
ssi[, pid := factor(Interests.Political.Affiliation,
                    levels = c("Democrat", "Republican (GOP)", "Independent"))]

# --- Reshape to long format -------------------------------------------------

ssi[, respondent := as.character(ResponseId)]
ssi[, task := as.integer(election)]
ssi[, profile := ifelse(variable == "candidateA", 1L, 2L)]
ssi[, choice := as.integer(candidate_vote)]

# Select columns for sconjoint
cols_keep <- c("respondent", "task", "profile", "choice",
               "agenda", "talent", "children", "cand_gender", "prior_office",
               "resp_female", "age", "pid")
sw2022 <- as.data.frame(ssi[, ..cols_keep])

# --- Summary ----------------------------------------------------------------

cat("Final dataset:", nrow(sw2022), "rows\n")
cat("Respondents:", length(unique(sw2022$respondent)), "\n")
cat("Tasks:", nrow(sw2022) / 2, "\n")

# --- Save -------------------------------------------------------------------

usethis::use_data(sw2022, overwrite = TRUE)
