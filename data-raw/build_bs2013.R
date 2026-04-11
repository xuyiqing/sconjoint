###############################################################################
# data-raw/build_bs2013.R
#
# Build the bundled `bs2013` dataset from Bechtel-Scheve (2013) replication data.
#
# Provenance: Bechtel, Michael M. and Kenneth F. Scheve. 2013.
#   "Mass Support for Global Climate Agreements Depends on
#   Institutional Design." PNAS 110(34):13763-13768.
#
# Source: Stata .dta file from published replication materials (US subsample).
###############################################################################

library(data.table)
library(haven)

# --- Load raw data -----------------------------------------------------------

data_path <- file.path("~", "Dropbox", "Projects", "ConjointStructural",
                        "data", "bechtel_scheve_2013",
                        "bechtel_scheve_pnas.dta")
bs_raw <- as.data.table(read_dta(data_path))

cat("Raw rows:", nrow(bs_raw), "\n")
cat("Unique respondents:", uniqueN(bs_raw$ID), "\n")

# --- Filter to US respondents (country == 4) ---------------------------------

bs <- bs_raw[country == 4]
cat("US rows:", nrow(bs), "\n")

# --- Keep respondents with exactly 8 rows (4 tasks x 2 profiles) ------------

tasks_per <- bs[, .(n_rows = .N), by = ID]
keep_ids <- tasks_per[n_rows == 8, ID]
bs <- bs[ID %in% keep_ids]
cat("After keeping 8-row respondents:", nrow(bs), "rows,",
    uniqueN(bs$ID), "respondents\n")

# --- Cost dollar mapping (US) -----------------------------------------------

COST_DOLLARS <- c(28, 56, 84, 113, 141)  # cost_cj 1-5 -> $/month
cost_map <- setNames(COST_DOLLARS, 1:5)
bs[, cost_usd := cost_map[as.character(cost_cj)]]

# --- Encode conjoint attributes as factors -----------------------------------

# Distribution (ref: "Only rich pay")
distrib_levels <- c("Only rich pay", "Prop. current emissions",
                    "Prop. hist. emissions", "Rich pay more, shared")
distrib_map <- setNames(distrib_levels, 1:4)
bs[, distribution := factor(distrib_map[as.character(distrib_cj)],
                            levels = distrib_levels)]

# Countries participating (ref: "20 countries")
ctries_levels <- c("20 countries", "80 countries", "160 countries")
ctries_map <- setNames(ctries_levels, 1:3)
bs[, participation := factor(ctries_map[as.character(ctries_cj)],
                             levels = ctries_levels)]

# Emissions target (ref: "40% reduction")
emis_levels <- c("40% reduction", "60% reduction", "80% reduction")
emis_map <- setNames(emis_levels, 1:3)
bs[, emissions := factor(emis_map[as.character(emissions_cj)],
                         levels = emis_levels)]

# Sanctions (ref: "No sanctions")
sanctions_levels <- c("No sanctions", "$6/mo sanctions",
                      "$17/mo sanctions", "$23/mo sanctions")
sanctions_map <- setNames(sanctions_levels, 1:4)
bs[, sanctions := factor(sanctions_map[as.character(sanctions_cj)],
                         levels = sanctions_levels)]

# Monitoring (ref: "Your government")
monitor_levels <- c("Your government", "Independent commission",
                    "United Nations", "Greenpeace")
monitor_map <- setNames(monitor_levels, 1:4)
bs[, monitoring := factor(monitor_map[as.character(monitoring_cj)],
                          levels = monitor_levels)]

# --- Respondent-level covariates --------------------------------------------

bs[, resp_female := as.numeric(female)]
bs[, resp_age := as.numeric(age)]
bs[, resp_ideo := as.numeric(ideology)]  # 0-10 scale

# --- Create long format for sconjoint ---------------------------------------

# Each conjoint task has 2 profiles (cj_order distinguishes them within task)
bs[, profile := seq_len(.N), by = .(ID, conjoint)]
stopifnot(all(bs[, .N, by = .(ID, conjoint)]$N == 2))

dat <- data.frame(
  respondent    = as.character(bs$ID),
  task          = as.integer(bs$conjoint),
  profile       = as.integer(bs$profile),
  choice        = as.integer(bs$choice_cj),
  cost_usd      = bs$cost_usd,
  distribution  = bs$distribution,
  participation = bs$participation,
  emissions     = bs$emissions,
  sanctions     = bs$sanctions,
  monitoring    = bs$monitoring,
  resp_female   = bs$resp_female,
  resp_age      = bs$resp_age,
  resp_ideo     = bs$resp_ideo,
  stringsAsFactors = FALSE
)

# Drop rows with NA in choice or key attributes
dat <- dat[complete.cases(dat), ]

bs2013 <- dat

# --- Summary ----------------------------------------------------------------

cat("Final dataset:", nrow(bs2013), "rows\n")
cat("Respondents:", length(unique(bs2013$respondent)), "\n")
cat("Tasks:", nrow(bs2013) / 2, "\n")

# --- Save -------------------------------------------------------------------

usethis::use_data(bs2013, overwrite = TRUE)
