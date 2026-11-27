###############################################################################
# data-raw/build_br2017.R
#
# Build the bundled `br2017` dataset from the Ballard-Rosa, Martin and Scheve
# (2017) replication file.
#
# Ballard-Rosa, Cameron, Lucy Martin, and Kenneth Scheve. 2017. "The Structure
#   of American Income Tax Policy Preferences." Journal of Politics 79(1):1-16.
#   Harvard Dataverse doi:10.7910/DVN/NGRGS5 (CC0 1.0).
#
# Input (read only): us_fullsurvey_taxplan_level.dta. Set SCONJOINT_BR_RAW to
# the folder holding it; the default is the paper project's copy.
#
# The sample and coding follow the paper's prep (replication package
# 2026-06_v3, code/data_prep/br2017/data_prep.R): respondents who saw the
# revenue column (2,000 respondents, 8 tasks, 2 plans). All six bracket rates
# are rebuilt from the coded taxrate1-6 variables and their value labels: the
# file's derived rate_175_375 column stores the 45 percent level as 5.
#
# Raw covariate codes (added 2026-11-25). The paper's models use the raw codes
# below, not the standardized columns, as network inputs (Z):
#   resp_age     age in years
#   resp_educ    education code 1-6 (No HS ... Post-grad)
#   resp_income  household income code 1-13 (hh_income); NA when skipped
#   resp_pid7    party ID 1-8 (8 = "Not sure")
#   female, race_white  0/1 (already raw)
# valid_income is FALSE for the 9 respondents with no income answer. The
# paper's sample is the 1,991 respondents with valid_income == TRUE. The
# standardized income_std column imputes their income at the median, so the
# restriction cannot be recovered from it.
#
# Run from the package root:  Rscript data-raw/build_br2017.R
###############################################################################

options(stringsAsFactors = FALSE, warn = 1)

raw_dir <- Sys.getenv(
  "SCONJOINT_BR_RAW",
  file.path("~", "Dropbox", "Research_Hub", "Projects", "ConjointStructural",
            "data", "ballard_rosa_2017"))
raw_file <- normalizePath(file.path(raw_dir, "us_fullsurvey_taxplan_level.dta"),
                          mustWork = TRUE)
br_raw <- haven::read_dta(raw_file)

br <- br_raw[as.numeric(br_raw$saw_revenue) == 1, ]
stopifnot(length(unique(br$ID)) == 2000L, nrow(br) == 32000L)
br <- br[order(as.numeric(br$ID), as.numeric(br$table), as.numeric(br$plan)), ]

# --- Rates from codes and value labels ----------------------------------------
code_cols <- paste0("taxrate", 1:6)
rate_cols <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
               "rate_175_375", "rate_375P")
rebuilt <- matrix(NA_real_, nrow(br), 6, dimnames = list(NULL, rate_cols))
for (j in 1:6) {
  lab <- attr(br_raw[[code_cols[j]]], "labels")
  pct <- suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", names(lab))))
  stopifnot(!is.null(lab), !anyNA(pct))
  rebuilt[, j] <- pct[match(as.numeric(br[[code_cols[j]]]), as.numeric(lab))]
  stopifnot(!anyNA(rebuilt[, j]))
}
for (j in c(1:4, 6)) stopifnot(all(rebuilt[, j] == as.numeric(br[[rate_cols[j]]])))
bad5 <- rebuilt[, 5] != as.numeric(br$rate_175_375)
stopifnot(all(as.numeric(br$taxrate5)[bad5] == 5), all(rebuilt[bad5, 5] == 45))

# Revenue: taxrev 1 = much more ... 5 = much less; score +2 = much more.
revenue_score <- 3 - as.numeric(br$taxrev)

# --- Respondent covariates (23 standardized / 0-1 columns, as in the paper's prep)
plan <- as.integer(as.numeric(br$plan))
prof_a <- br[plan == 1L, ]
std <- function(x) (x - mean(x)) / stats::sd(x)
imp_med <- function(x) { x[is.na(x)] <- stats::median(x, na.rm = TRUE); x }
num_a <- function(v) suppressWarnings(as.numeric(prof_a[[v]]))
z_spec <- list(
  c("age_std", "age", "std"), c("female", "female", "raw"),
  c("pid7_std", "pid7", "std"), c("educ_std", "educ", "std"),
  c("race_white", "race_white", "raw"), c("income_std", "hh_income", "std"),
  c("ineq_averse", "ineq_averse_dum", "raw"), c("work_vs_luck", "work_vs_luck", "std"),
  c("taxes_harm_econ", "taxes_harm_econ", "std"), c("hardwork", "hardwork", "raw"),
  c("high_econ_know", "high_econ_know", "raw"), c("employed_ft", "employed_full_time", "raw"),
  c("conserv_ideo", "conserv_ideo_impute", "std"), c("govt_serv", "govt_serv_7pt", "std"),
  c("newsint", "newsint", "std"), c("numeracy", "high_numeracy", "raw"),
  c("gen_mobile", "gen_mobile", "raw"), c("future_mobile", "future_mobile", "raw"),
  c("gov_assist", "gov_assist_any", "raw"), c("risk_averse", "risk_averse", "raw"),
  c("hardship", "recent_hardship", "raw"), c("children", "children_dum", "raw"),
  c("trust", "trust_dummy", "raw"))
Za <- sapply(z_spec, function(s) { x <- imp_med(num_a(s[2])); if (s[3] == "std") std(x) else x })
colnames(Za) <- vapply(z_spec, `[`, "", 1)
stopifnot(ncol(Za) == 23L, !anyNA(Za))

# --- Raw codes and the income-validity flag -----------------------------------
inc <- as.numeric(prof_a$hh_income)            # haven turns codes 98/99 into NA
raw_a <- data.frame(
  resp_pid7   = as.numeric(prof_a$pid7),
  resp_age    = as.numeric(prof_a$age),
  resp_educ   = as.numeric(prof_a$educ),
  resp_income = inc,
  valid_income = !is.na(inc) & inc %in% 1:13)
stopifnot(!anyNA(raw_a$resp_pid7), !anyNA(raw_a$resp_age), !anyNA(raw_a$resp_educ),
          all(raw_a$resp_educ %in% 1:6), all(raw_a$resp_pid7 %in% 1:8),
          identical(is.na(raw_a$resp_income), !raw_a$valid_income))
invalid_ids <- sort(unique(as.character(prof_a$ID[!raw_a$valid_income])))
stopifnot(identical(invalid_ids, c("206770863", "206819471", "206979199", "207003707",
  "207124543", "207155412", "207177378", "207185872", "207186054")))

# --- Long format ---------------------------------------------------------------
idx_a <- which(plan == 1L); idx_b <- which(plan == 2L)
stopifnot(all(br$ID[idx_a] == br$ID[idx_b]), all(br$table[idx_a] == br$table[idx_b]))
one <- function(ix) data.frame(
  respondent = as.character(as.numeric(br$ID[ix])),
  task = as.integer(as.numeric(br$table[ix])),
  profile = as.integer(plan[ix]),
  choice = as.integer(as.numeric(br$chose_plan[ix])),
  rebuilt[ix, , drop = FALSE], revenue_score = revenue_score[ix],
  raw_a, Za, check.names = FALSE)
br2017 <- rbind(one(idx_a), one(idx_b))
br2017 <- br2017[order(br2017$respondent, br2017$task, br2017$profile), ]
rownames(br2017) <- NULL
stopifnot(nrow(br2017) == 32000L)

# --- Gate against the previously bundled copy (if present) -------------------
new_cols <- c("resp_age", "resp_educ", "resp_income", "valid_income")
old_file <- file.path("data", "br2017.rda")
if (file.exists(old_file)) {
  e <- new.env(); load(old_file, envir = e); old <- e$br2017
  shared <- setdiff(names(old), new_cols)
  stopifnot(nrow(old) == nrow(br2017), identical(setdiff(names(br2017), new_cols), shared))
  for (v in shared) {
    a <- br2017[[v]]; b <- old[[v]]
    ok <- if (is.numeric(a)) isTRUE(max(abs(a - b)) == 0) else identical(a, b)
    if (!ok) stop("Rebuilt br2017 differs from the bundled copy in column ", v)
  }
  message("br2017: all ", length(shared), " previously bundled columns reproduced exactly.")
}

message(sprintf("br2017: %d rows, %d respondents; %d without a valid income answer.",
                nrow(br2017), length(unique(br2017$respondent)), length(invalid_ids)))
save(br2017, file = file.path("data", "br2017.rda"), compress = "bzip2")
