###############################################################################
# data-raw/build_gs2020.R
#
# Build the bundled `gs2020` example dataset.
#
# Provenance: Graham, Matthew H. and Milan W. Svolik. 2020.
#   "Democracy in America?  Partisanship, Polarization, and the
#   Robustness of Support for Democracy in the United States."
#   American Political Science Review 114(2):392-409.
#
# Note (M5): See build_sw2022.R for the rationale -- this script
#   produces a SEED-FIXED SYNTHETIC dataset whose structure mirrors
#   the published design.  The `synthetic = TRUE` attribute is set on
#   the saved object and documented in R/data.R.
#
# Design (matching Graham-Svolik 2020 Table 1):
#   - 6 attributes: federal_pgm, immigration, tax, abortion,
#       undemocratic, partisanship
#   - 8 tasks per respondent
#   - 2 profiles per task (forced-choice between candidates A and B)
#   - Respondent moderators Z: resp_pid (-1/0/+1), resp_ideo, age
#
# Downsampling seed: 20260409
###############################################################################

set.seed(20260409)

M <- 200L
T_tasks <- 8L
P <- 2L

federal_lv    <- c("decrease", "same", "increase")
immigration_lv<- c("decrease", "same", "increase")
tax_lv        <- c("cut_all", "cut_low", "raise_high")
abortion_lv   <- c("ban", "restrict", "legal")
undem_lv      <- c("none", "ignore_court", "close_press",
                   "ban_opposition_rally")
party_lv      <- c("Dem", "Rep", "Ind")

n_rows <- M * T_tasks * P
rid <- rep(seq_len(M), each = T_tasks * P)
tid <- rep(rep(seq_len(T_tasks), each = P), M)
pos <- rep(c(1L, 2L), M * T_tasks)

federal      <- factor(sample(federal_lv,    n_rows, replace = TRUE), levels = federal_lv)
immigration  <- factor(sample(immigration_lv,n_rows, replace = TRUE), levels = immigration_lv)
tax          <- factor(sample(tax_lv,        n_rows, replace = TRUE), levels = tax_lv)
abortion     <- factor(sample(abortion_lv,   n_rows, replace = TRUE), levels = abortion_lv)
undem        <- factor(sample(undem_lv,      n_rows, replace = TRUE), levels = undem_lv)
cand_party   <- factor(sample(party_lv,      n_rows, replace = TRUE), levels = party_lv)

resp_pid  <- sample(c(-1L, 0L, 1L), M, replace = TRUE, prob = c(0.4, 0.2, 0.4))
resp_ideo <- as.numeric(scale(rnorm(M)))
age_std   <- as.numeric(scale(round(runif(M, 18, 82))))

attr_df <- data.frame(federal, immigration, tax, abortion, undem, cand_party)
X <- stats::model.matrix(~ federal + immigration + tax + abortion + undem + cand_party,
                         data = attr_df)[, -1L, drop = FALSE]
colnames(X) <- make.names(colnames(X))

p <- ncol(X)
beta_base <- setNames(numeric(p), colnames(X))
beta_base[grepl("federalincrease",    names(beta_base))] <-  0.15
beta_base[grepl("federaldecrease",    names(beta_base))] <- -0.05
beta_base[grepl("immigrationincrease",names(beta_base))] <-  0.00
beta_base[grepl("immigrationdecrease",names(beta_base))] <-  0.10
beta_base[grepl("taxcut_all",         names(beta_base))] <-  0.25
beta_base[grepl("taxcut_low",         names(beta_base))] <-  0.10
beta_base[grepl("abortionlegal",      names(beta_base))] <-  0.10
beta_base[grepl("abortionban",        names(beta_base))] <- -0.15
beta_base[grepl("undemignore_court",  names(beta_base))] <- -0.70
beta_base[grepl("undemclose_press",   names(beta_base))] <- -0.90
beta_base[grepl("undemban_opposition",names(beta_base))] <- -1.10
beta_base[grepl("cand_partyRep",      names(beta_base))] <-  0.00
beta_base[grepl("cand_partyInd",      names(beta_base))] <- -0.10

beta_true <- matrix(0, M, p, dimnames = list(NULL, colnames(X)))
for (j in seq_len(p)) beta_true[, j] <- beta_base[j]
## Partisans like co-partisan candidates and are more tolerant of
## undemocratic behavior if their own side does it.
col_rep <- "cand_partyRep"
if (col_rep %in% colnames(beta_true)) {
  beta_true[, col_rep] <- beta_true[, col_rep] + 1.10 * (resp_pid == 1L) -
    0.80 * (resp_pid == -1L)
}
for (cn in grep("^undem", colnames(beta_true), value = TRUE)) {
  beta_true[, cn] <- beta_true[, cn] + 0.30 * abs(resp_pid)  # copartisan excuse
}

beta_row <- beta_true[rid, , drop = FALSE]
U <- rowSums(X * beta_row)

choice <- integer(n_rows)
for (i in seq_len(M)) {
  for (t in seq_len(T_tasks)) {
    idx <- which(rid == i & tid == t)
    u <- U[idx] + -log(-log(runif(length(idx))))
    pick <- which.max(u)
    choice[idx] <- 0L
    choice[idx[pick]] <- 1L
  }
}

gs2020 <- data.frame(
  respondent = rid,
  task = tid,
  profile = pos,
  choice = choice,
  federal = federal,
  immigration = immigration,
  tax = tax,
  abortion = abortion,
  undem = undem,
  cand_party = cand_party,
  resp_pid = resp_pid[rid],
  resp_ideo = resp_ideo[rid],
  age = age_std[rid],
  stringsAsFactors = FALSE
)

attr(gs2020, "synthetic") <- TRUE
attr(gs2020, "source") <- "Graham & Svolik (2020) design; synthetic data with matching structure."

out_path <- file.path("data", "gs2020.rda")
save(gs2020, file = out_path, compress = "xz", version = 2L)
cat(sprintf("Wrote %s (%.1f KB)\n", out_path,
            file.info(out_path)$size / 1024))
