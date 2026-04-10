###############################################################################
# data-raw/build_sw2022.R
#
# Build the bundled `sw2022` example dataset.
#
# Provenance: The Saha-Weeks (2022) candidate-choice conjoint experiment
#   (Saha, Sparsha and Jessica L. P. Weeks. 2022. "Ambitious Women:
#   Gender and Voter Perceptions of Candidate Ambition."
#   Political Behavior 44(2):779-805.)
#
# Note (M5): The Dropbox prototype raw CSV is not redistributable with
#   the package (licensing not confirmed per spec §8.1).  This script
#   therefore generates a SEED-FIXED SYNTHETIC dataset whose design
#   mirrors the published conjoint -- 5 candidate attributes, 3 forced-
#   choice tasks per respondent, paired profiles -- so that the
#   case-study chapter (tutorial/08-case-saha-weeks.qmd) is
#   self-contained and works on any machine without additional data
#   downloads.  The `synthetic = TRUE` flag is documented in R/data.R
#   and repeated in the first chunk of the case-study chapter.
#
# Design (matching Saha-Weeks 2022 Table 1):
#   - 5 attributes: agenda, talent, children, cand_gender, prior_office
#   - 3 tasks per respondent
#   - 2 profiles per task (forced-choice)
#   - Respondent moderators Z: resp_female, age (standardized), pid (-1/0/+1)
#
# Size budget: <=500 KB target.  At ~200 respondents x 3 tasks x 2
# profiles = ~1200 rows the gzipped .rda file lands well under 40 KB.
#
# Downsampling seed: 20260409 (fixed per M5 task instructions).
###############################################################################

set.seed(20260409)

M <- 200L          # respondents
T_tasks <- 3L
P <- 2L            # profiles per task

agenda_lv   <- c("status_quo", "progressive", "conservative")
talent_lv   <- c("average", "hard_working", "experienced")
children_lv <- c("no_children", "has_children")
gender_lv   <- c("male", "female")
prior_lv    <- c("none", "state_leg", "us_house")

n_rows <- M * T_tasks * P
rid <- rep(seq_len(M), each = T_tasks * P)
tid <- rep(rep(seq_len(T_tasks), each = P), M)
pos <- rep(c(1L, 2L), M * T_tasks)

## Draw attributes independently for each profile.
agenda   <- factor(sample(agenda_lv,   n_rows, replace = TRUE), levels = agenda_lv)
talent   <- factor(sample(talent_lv,   n_rows, replace = TRUE), levels = talent_lv)
children <- factor(sample(children_lv, n_rows, replace = TRUE), levels = children_lv)
cand_gender <- factor(sample(gender_lv, n_rows, replace = TRUE), levels = gender_lv)
prior_office <- factor(sample(prior_lv, n_rows, replace = TRUE), levels = prior_lv)

## Respondent-level moderators.
resp_female <- rbinom(M, 1L, 0.52)
age_raw     <- round(runif(M, 18, 82))
age_std     <- as.numeric(scale(age_raw))
pid         <- sample(c(-1L, 0L, 1L), M, replace = TRUE,
                      prob = c(0.35, 0.30, 0.35))  # dem / indep / rep

Z_row <- cbind(resp_female = resp_female, age = age_std, pid = pid)

## Build DeltaX-style dummy vector per profile row using the same
## model.matrix contrast scheme the package uses internally.
attr_df <- data.frame(agenda, talent, children, cand_gender, prior_office)
X <- stats::model.matrix(~ agenda + talent + children + cand_gender + prior_office,
                         data = attr_df)[, -1L, drop = FALSE]
colnames(X) <- make.names(colnames(X))

## Heterogeneous true preferences.
p <- ncol(X)
beta_base <- c(
  agendaprogressive    =  0.35,
  agendaconservative   = -0.20,
  talenthard_working   =  0.55,
  talentexperienced    =  0.45,
  childrenhas_children =  0.20,
  cand_genderfemale    =  0.10,
  prior_officestate_leg=  0.25,
  prior_officeus_house =  0.60
)
## Ensure names align with X's columns in case of re-ordering.
beta_base <- setNames(beta_base[colnames(X)], colnames(X))
beta_base[is.na(beta_base)] <- 0

## Respondent-specific beta: base + Z interactions.
beta_true <- matrix(0, M, p, dimnames = list(NULL, colnames(X)))
for (j in seq_len(p)) beta_true[, j] <- beta_base[j]
beta_true[, "cand_genderfemale"] <- beta_true[, "cand_genderfemale"] +
  0.30 * resp_female + 0.15 * (pid == -1L)
beta_true[, "agendaprogressive"] <- beta_true[, "agendaprogressive"] +
  0.50 * (pid == -1L) - 0.40 * (pid == 1L)
beta_true[, "agendaconservative"] <- beta_true[, "agendaconservative"] -
  0.50 * (pid == -1L) + 0.40 * (pid == 1L)

## Compute utility per row.
beta_row <- beta_true[rid, , drop = FALSE]
U <- rowSums(X * beta_row)

## Aggregate to (respondent, task): pick the profile with higher U + Gumbel noise.
choice <- integer(n_rows)
for (i in seq_len(M)) {
  for (t in seq_len(T_tasks)) {
    idx <- which(rid == i & tid == t)
    u <- U[idx] + -log(-log(runif(length(idx))))  # Gumbel noise
    pick <- which.max(u)
    choice[idx] <- 0L
    choice[idx[pick]] <- 1L
  }
}

sw2022 <- data.frame(
  respondent = rid,
  task = tid,
  profile = pos,
  choice = choice,
  agenda = agenda,
  talent = talent,
  children = children,
  cand_gender = cand_gender,
  prior_office = prior_office,
  resp_female = resp_female[rid],
  age = age_std[rid],
  pid = pid[rid],
  stringsAsFactors = FALSE
)

attr(sw2022, "synthetic") <- TRUE
attr(sw2022, "source") <- "Saha & Weeks (2022) design; synthetic data with matching structure."

## Save to data/
out_path <- file.path("data", "sw2022.rda")
save(sw2022, file = out_path, compress = "xz", version = 2L)
cat(sprintf("Wrote %s (%.1f KB)\n", out_path,
            file.info(out_path)$size / 1024))
