###############################################################################
# data-raw/build_br2017.R
#
# Build the bundled `br2017` dataset from Ballard-Rosa, Martin & Scheve (2017)
# replication data.
#
# Provenance: Ballard-Rosa, Cameron, Lucy Martin, and Kenneth Scheve. 2017.
#   "The Structure of American Income Tax Policy Preferences."
#   Journal of Politics 79(1):1-16.
#
# Source: Stata .dta file from published replication materials.
###############################################################################

library(data.table)
library(haven)

# --- Load raw data -----------------------------------------------------------

data_path <- file.path("~", "Dropbox", "Projects", "ConjointStructural",
                        "data", "ballard_rosa_2017",
                        "us_fullsurvey_taxplan_level.dta")
br_raw <- as.data.table(read_dta(data_path))

cat("Raw rows:", nrow(br_raw), "\n")
cat("Unique respondents:", uniqueN(br_raw$ID), "\n")

# --- Filter to respondents who saw the revenue column -----------------------

br <- br_raw[saw_revenue == 1]
cat("saw_revenue == 1:", nrow(br), "rows,", uniqueN(br$ID), "respondents\n")

# --- Keep respondents with exactly 16 rows (8 tasks x 2 plans) -------------

rows_per <- br[, .(n_rows = .N), by = ID]
keep_ids <- rows_per[n_rows == 16, ID]
br <- br[ID %in% keep_ids]
cat("After keeping 16-row respondents:", nrow(br), "rows,",
    uniqueN(br$ID), "respondents\n")

# --- Create revenue score ---------------------------------------------------

# taxrev: 1 = much less revenue, 5 = much more revenue
# revenue_score = 3 - taxrev => -2 (much more) to +2 (much less)
# Actually: 3 - 1 = +2 (much less rev), 3 - 5 = -2 (much more rev)
# Re-read spec: "higher = more revenue" so we want the reverse:
# revenue_score = taxrev - 3 => -2 (much less) to +2 (much more)
# Wait, spec says: revenue_score = 3 - taxrev, higher = more revenue
# 3 - 1 = 2 (taxrev=1 is "much less"), 3 - 5 = -2 (taxrev=5 is "much more")
# That makes higher = LESS revenue. Let me follow spec exactly.
br[, revenue_score := 3 - taxrev]

# --- Create long-format data frame ------------------------------------------

br2017 <- data.frame(
  respondent    = as.character(br$ID),
  task          = as.integer(br$table),
  profile       = as.integer(br$plan),
  choice        = as.integer(br$chose_plan),
  rate_L10      = as.numeric(br$rate_L10),
  rate_10_35    = as.numeric(br$rate_10_35),
  rate_35_85    = as.numeric(br$rate_35_85),
  rate_85_175   = as.numeric(br$rate_85_175),
  rate_175_375  = as.numeric(br$rate_175_375),
  rate_375P     = as.numeric(br$rate_375P),
  revenue_score = as.numeric(br$revenue_score),
  resp_pid7     = as.numeric(br$pid7),
  resp_age      = as.numeric(br$age),
  resp_female   = as.numeric(br$female),
  stringsAsFactors = FALSE
)

# Drop rows with NA in key columns
br2017 <- br2017[complete.cases(br2017), ]

# --- Summary ----------------------------------------------------------------

cat("Final dataset:", nrow(br2017), "rows\n")
cat("Respondents:", length(unique(br2017$respondent)), "\n")
cat("Tasks:", nrow(br2017) / 2, "\n")

# --- Save -------------------------------------------------------------------

usethis::use_data(br2017, overwrite = TRUE)
