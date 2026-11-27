###############################################################################
# data-raw/build_gs2020.R
#
# Build the bundled `gs2020` dataset from the Graham and Svolik (2020)
# replication files.
#
# Graham, Matthew H. and Milan W. Svolik. 2020. "Democracy in America?
#   Partisanship, Polarization, and the Robustness of Support for Democracy
#   in the United States." American Political Science Review 114(2):392-409.
#   Harvard Dataverse doi:10.7910/DVN/EEARKA (CC BY 4.0).
#
# Inputs (read only): data/data_experiment.csv, data/data_covariates.csv and
# data/data_howDemocratic.csv from the replication materials. Set
# SCONJOINT_GS_RAW to the folder holding them; the default is the paper
# project's copy.
#
# The sample and coding follow the paper's prep (replication package
# 2026-06_v3, code/data_prep/gs2020/data_prep.R): rows of candidate 1
# (candNum == 1), respondents with 13 such rows, matchups with an observed
# outcome, and respondents with complete base covariates (1,605 respondents,
# 20,657 matchups). Each matchup is stored as two rows: profile 1 carries the
# 30 precomputed `diff_*` contrasts (candidate 1 minus candidate 2) with
# choice = c_win; profile 2 is a row of zeros with choice = 1 - c_win.
#
# Screen side (added 2026-11-25). The raw file records which candidate was
# displayed on the left (c_onLeft; o_onLeft is its complement). `on_left` is
# that flag for the row's candidate: c_onLeft on profile 1 rows and o_onLeft on
# profile 2 rows. The paper's models put the left-displayed candidate first
# (y = 1 selects the left candidate; d = left minus right), so a matchup whose
# candidate 1 was shown on the right is flipped: y = 1 - choice, d = -diff.
# sc_example_data("gs") does this.
#
# Run from the package root:  Rscript data-raw/build_gs2020.R
###############################################################################

options(stringsAsFactors = FALSE, warn = 1)

raw_dir <- Sys.getenv(
  "SCONJOINT_GS_RAW",
  file.path("~", "Dropbox", "Research_Hub", "Projects", "ConjointStructural",
            "data", "replication_materials_graham_svolik_2020", "data"))
raw_dir <- normalizePath(raw_dir, mustWork = TRUE)
read_csv <- function(f) as.data.frame(data.table::fread(file.path(raw_dir, f),
                                                        showProgress = FALSE))

gs_raw <- read_csv("data_experiment.csv")

# --- Sample (as in the paper's prep) -----------------------------------------
gs <- gs_raw[gs_raw$candNum == 1, ]
tp <- table(gs$id)
gs <- gs[gs$id %in% as.integer(names(tp)[tp == 13]), ]   # vacuous; kept for fidelity
gs <- gs[!is.na(gs$c_win), ]                             # drops 262 matchups

ideo_map <- c("Extremely liberal" = 1, "Liberal" = 2, "Slightly liberal" = 3,
              "Moderate" = 4, "Slightly conservative" = 5, "Conservative" = 6,
              "Extremely conservative" = 7)
trump_map <- c("Strongly disapprove" = 1, "Somewhat disapprove" = 2,
               "Somewhat approve" = 3, "Strongly approve" = 4)
educ_map <- c("Did not complete high school" = 1, "High school graduate" = 2,
              "Some college, no degree" = 3, "Associate's degree" = 4,
              "Bachelor's degree" = 5, "Graduate or professional degree" = 6)
hhi_levels <- c("Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999",
  "$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999",
  "$40,000 to $44,999", "$45,000 to $49,999", "$50,000 to $54,999",
  "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999",
  "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999",
  "$85,000 to $89,999", "$90,000 to $94,999", "$95,000 to $99,999",
  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999",
  "$175,000 to $199,999", "$200,000 to $249,999", "$250,000 and above")
hhi_map <- stats::setNames(seq_along(hhi_levels), hhi_levels)

gs$z_ideo  <- ideo_map[gs$ideo];   gs$z_pid7 <- as.numeric(gs$pid7)
gs$z_trump <- trump_map[gs$trump]; gs$z_age  <- as.numeric(gs$age)
gs$z_female     <- as.numeric(gs$gender == "Female")
gs$z_race_black <- as.numeric(gs$race == "Black")
gs$z_race_asian <- as.numeric(gs$race == "Asian or Pacific Islander")
gs$z_race_other <- as.numeric(gs$race %in% c("Other",
                     "American Indian or Alaska Native", "Prefer not to answer"))
gs$z_educ  <- educ_map[gs$educ]; gs$z_hhi <- hhi_map[gs$hhi]
gs$z_auth  <- as.numeric(gs$auth_total); gs$z_knowl <- as.numeric(gs$knowl_anes_total)
z_base <- c("z_ideo", "z_pid7", "z_trump", "z_age", "z_female", "z_race_black",
            "z_race_asian", "z_race_other", "z_educ", "z_hhi", "z_auth", "z_knowl")
cc <- stats::complete.cases(gs[, z_base])
ok_by_id <- tapply(cc, gs$id, all)
gs <- gs[gs$id %in% as.integer(names(ok_by_id)[ok_by_id]), ]
stopifnot(length(unique(gs$id)) == 1605L, nrow(gs) == 20657L)

# --- Attribute contrasts (candidate 1 minus candidate 2) ---------------------
DIFF <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote", "diff_dem_code_v_affair", "diff_dem_code_v_tax",
  "diff_sex_Female", "diff_race_Asian", "diff_race_Black", "diff_race_Hispanic",
  "diff_pro_Farmer", "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")
stopifnot(all(DIFF %in% names(gs)), !anyNA(gs[, DIFF]))
DeltaX <- as.matrix(gs[, DIFF]); mode(DeltaX) <- "numeric"
Y <- as.numeric(gs$c_win)
stopifnot(all(Y %in% 0:1))

# --- Screen side --------------------------------------------------------------
on_left <- as.integer(gs$c_onLeft)
stopifnot(!anyNA(on_left), all(on_left %in% 0:1),
          all(as.integer(gs$o_onLeft) + on_left == 1L))

# --- Respondent covariates (22; the paper's main fit uses the first 16) -------
cov <- read_csv("data_covariates.csv")
how <- read_csv("data_howDemocratic.csv")
m_cov <- match(gs$id, cov$id); m_how <- match(gs$id, how$id)
zc <- cbind(E_ideal = cov$E_ideal[m_cov], I_ideal = cov$I_ideal[m_cov],
            M_ideal = cov$M_ideal[m_cov], T_ideal = cov$T_ideal[m_cov])
zb <- cbind(dem_better = how$dem_better[m_how],
            dem_satisfied = how$dem_satisfied[m_how],
            dem_treat_journalists = how$dem_treat_journalists[m_how],
            dem_treat_banProtest = how$dem_treat_banProtest[m_how],
            dem_treat_execRule = how$dem_treat_execRule[m_how],
            dem_treat_ignoreCourt = how$dem_treat_ignoreCourt[m_how])
for (j in seq_len(ncol(zc))) { na <- is.na(zc[, j]); if (any(na)) zc[na, j] <- stats::median(zc[!na, j]) }
for (j in seq_len(ncol(zb))) { na <- is.na(zb[, j]); if (any(na)) zb[na, j] <- stats::median(zb[!na, j]) }
# Standardized over matchup rows (each respondent weighted by its matchups),
# exactly as in the paper's prep.
Z <- cbind(scale(as.matrix(gs[, c("z_ideo", "z_pid7", "z_trump", "z_age",
                                  "z_educ", "z_hhi", "z_auth", "z_knowl")])),
           as.matrix(gs[, c("z_female", "z_race_black", "z_race_asian", "z_race_other")]),
           scale(zc), zb)
attr(Z, "scaled:center") <- NULL; attr(Z, "scaled:scale") <- NULL
stopifnot(ncol(Z) == 22L, !anyNA(Z))

# --- Long format: two rows per matchup ----------------------------------------
resp <- sprintf("%06d", gs$id)
task <- ave(seq_len(nrow(gs)), gs$id, FUN = seq_along)   # 1..T in file order
meta <- data.frame(ideo7 = unname(ideo_map[gs$ideo]), pid7 = as.numeric(gs$pid7),
                   weight = as.numeric(gs$weight))
zeros <- DeltaX; zeros[] <- 0
long <- rbind(
  data.frame(respondent = resp, task = task, profile = 1L, choice = as.integer(Y),
             on_left = on_left, DeltaX, Z, meta, check.names = FALSE),
  data.frame(respondent = resp, task = task, profile = 2L, choice = as.integer(1L - Y),
             on_left = 1L - on_left, zeros, Z, meta, check.names = FALSE))
long <- long[order(long$respondent, long$task, long$profile), ]
rownames(long) <- NULL
gs2020 <- long

# --- Gate against the previously bundled copy (if present) -------------------
old_file <- file.path("data", "gs2020.rda")
if (file.exists(old_file)) {
  e <- new.env(); load(old_file, envir = e); old <- e$gs2020
  shared <- setdiff(names(old), "on_left")
  stopifnot(nrow(old) == nrow(gs2020), identical(setdiff(names(gs2020), "on_left"), shared))
  for (v in shared) {
    a <- gs2020[[v]]; b <- old[[v]]
    ok <- if (is.numeric(a)) isTRUE(max(abs(a - b)) == 0) else identical(a, b)
    if (!ok) stop("Rebuilt gs2020 differs from the bundled copy in column ", v)
  }
  message("gs2020: all ", length(shared), " previously bundled columns reproduced exactly.")
}

message(sprintf("gs2020: %d rows, %d respondents, %d matchups; candidate 1 on the right in %d.",
                nrow(gs2020), length(unique(gs2020$respondent)), nrow(gs2020) / 2,
                sum(on_left == 0L)))
save(gs2020, file = file.path("data", "gs2020.rda"), compress = "bzip2")
