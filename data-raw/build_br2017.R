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
#
# Moderators (P_Z = 12): age, female, pid7, educ, race_white, income,
#   ineq_averse, work_vs_luck, taxes_harm, hardwork, high_econ_know,
#   employed_ft.
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

br <- br_raw[as.numeric(saw_revenue) == 1]
cat("saw_revenue == 1:", nrow(br), "rows,", uniqueN(br$ID), "respondents\n")

# --- Keep respondents with exactly 16 rows (8 tasks x 2 plans) -------------

rows_per <- br[, .(n_rows = .N), by = ID]
keep_ids <- rows_per[n_rows == 16, ID]
br <- br[ID %in% keep_ids]
cat("After keeping 16-row respondents:", nrow(br), "rows,",
    uniqueN(br$ID), "respondents\n")

# --- Create revenue score ---------------------------------------------------

# taxrev: 1 = Much more, 2 = More, 3 = Same, 4 = Less, 5 = Much less
# revenue_score = 3 - taxrev: higher = more revenue
br[, revenue_score := 3 - as.numeric(taxrev)]

# --- Helper: impute NAs to median ------------------------------------------

impute_median <- function(x, name) {
  n_na <- sum(is.na(x))
  if (n_na > 0) {
    med <- median(x, na.rm = TRUE)
    cat(sprintf("  Imputing %d NAs (%.1f%%) in %s to median = %.2f\n",
                n_na, 100 * n_na / length(x), name, med))
    x[is.na(x)] <- med
  }
  x
}

# --- Build moderator columns -----------------------------------------------

br[, resp_age          := as.numeric(age)]
br[, resp_female       := as.numeric(female)]
br[, resp_pid7         := as.numeric(pid7)]
br[, resp_educ         := as.numeric(educ)]
br[, resp_race_white   := as.numeric(race_white)]
br[, resp_income       := as.numeric(hh_income)]
br[, resp_ineq_averse  := as.numeric(ineq_averse_dum)]
br[, resp_work_vs_luck := as.numeric(work_vs_luck)]
br[, resp_taxes_harm   := as.numeric(taxes_harm_econ)]
br[, resp_hardwork     := as.numeric(hardwork)]
br[, resp_high_econ_know := as.numeric(high_econ_know)]
br[, resp_employed_ft  := as.numeric(employed_full_time)]

# Impute modest missingness to medians (same logic as prototype)
br[, resp_income       := impute_median(resp_income, "hh_income")]
br[, resp_work_vs_luck := impute_median(resp_work_vs_luck, "work_vs_luck")]
br[, resp_taxes_harm   := impute_median(resp_taxes_harm, "taxes_harm_econ")]
br[, resp_educ         := impute_median(resp_educ, "educ")]
br[, resp_hardwork     := impute_median(resp_hardwork, "hardwork")]

# Drop rows with any remaining NA in moderators
z_cols <- c("resp_age", "resp_female", "resp_pid7", "resp_educ",
            "resp_race_white", "resp_income", "resp_ineq_averse",
            "resp_work_vs_luck", "resp_taxes_harm", "resp_hardwork",
            "resp_high_econ_know", "resp_employed_ft")
br <- br[complete.cases(br[, ..z_cols])]

# Re-filter to respondents with 16 rows after dropping
rows_per2 <- br[, .(n_rows = .N), by = ID]
keep_ids2 <- rows_per2[n_rows == 16, ID]
br <- br[ID %in% keep_ids2]

cat("After cleaning moderators:", nrow(br), "rows,",
    uniqueN(br$ID), "respondents\n")

# --- Create long-format data frame ------------------------------------------

br2017 <- data.frame(
  respondent         = as.character(br$ID),
  task               = as.integer(br$table),
  profile            = as.integer(br$plan),
  choice             = as.integer(br$chose_plan),
  rate_L10           = as.numeric(br$rate_L10),
  rate_10_35         = as.numeric(br$rate_10_35),
  rate_35_85         = as.numeric(br$rate_35_85),
  rate_85_175        = as.numeric(br$rate_85_175),
  rate_175_375       = as.numeric(br$rate_175_375),
  rate_375P          = as.numeric(br$rate_375P),
  revenue_score      = as.numeric(br$revenue_score),
  resp_age           = as.numeric(br$resp_age),
  resp_female        = as.numeric(br$resp_female),
  resp_pid7          = as.numeric(br$resp_pid7),
  resp_educ          = as.numeric(br$resp_educ),
  resp_race_white    = as.numeric(br$resp_race_white),
  resp_income        = as.numeric(br$resp_income),
  resp_ineq_averse   = as.numeric(br$resp_ineq_averse),
  resp_work_vs_luck  = as.numeric(br$resp_work_vs_luck),
  resp_taxes_harm    = as.numeric(br$resp_taxes_harm),
  resp_hardwork      = as.numeric(br$resp_hardwork),
  resp_high_econ_know = as.numeric(br$resp_high_econ_know),
  resp_employed_ft   = as.numeric(br$resp_employed_ft),
  stringsAsFactors   = FALSE
)

# --- Summary ----------------------------------------------------------------

cat("Final dataset:", nrow(br2017), "rows\n")
cat("Respondents:", length(unique(br2017$respondent)), "\n")
cat("Tasks:", nrow(br2017) / 2, "\n")

# --- Save -------------------------------------------------------------------

usethis::use_data(br2017, overwrite = TRUE)
