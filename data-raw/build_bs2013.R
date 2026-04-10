###############################################################################
# data-raw/build_bs2013.R
#
# Build the bundled `bs2013` example dataset.
#
# Provenance: Bechtel, Michael M. and Kenneth F. Scheve. 2013.
#   "Mass Support for Global Climate Agreements Depends on
#   Institutional Design." PNAS 110(34):13763-13768.
#
# Note (M5): See build_sw2022.R for the rationale -- this script
#   produces a SEED-FIXED SYNTHETIC dataset that mirrors the published
#   climate-treaty conjoint structure.  It is a drop-in example
#   fixture for the WTP-flagship case-study chapter
#   (tutorial/10-case-bechtel-scheve.qmd).
#
# Design (approximating Bechtel-Scheve 2013 Table 1):
#   - 6 attributes: distribution, enforcement, monitoring,
#       participation, sanctions, cost_usd (numeric, "cost" attribute)
#   - 6 tasks per respondent
#   - 2 profiles per task
#   - Respondent moderators Z: resp_female, age (standardized),
#       resp_ideo (ideology continuous)
#
# Downsampling seed: 20260409
###############################################################################

set.seed(20260409)

M <- 200L
T_tasks <- 6L
P <- 2L

dist_lv   <- c("equal_share", "ability_to_pay", "historical_polluter")
enforce_lv<- c("none", "trade_sanctions", "fines")
monitor_lv<- c("none", "national", "international")
part_lv   <- c("50pct", "75pct", "90pct")
sanct_lv  <- c("none", "trade", "aid_cut")

n_rows <- M * T_tasks * P
rid <- rep(seq_len(M), each = T_tasks * P)
tid <- rep(rep(seq_len(T_tasks), each = P), M)
pos <- rep(c(1L, 2L), M * T_tasks)

distribution  <- factor(sample(dist_lv,    n_rows, replace = TRUE), levels = dist_lv)
enforcement   <- factor(sample(enforce_lv, n_rows, replace = TRUE), levels = enforce_lv)
monitoring    <- factor(sample(monitor_lv, n_rows, replace = TRUE), levels = monitor_lv)
participation <- factor(sample(part_lv,    n_rows, replace = TRUE), levels = part_lv)
sanctions     <- factor(sample(sanct_lv,   n_rows, replace = TRUE), levels = sanct_lv)
## Cost levels are drawn from a small numeric grid (USD per
## household per month).  Numeric so we can compute a WTP on a
## dollar scale via sc_wtp.
cost_grid <- c(5, 15, 30, 50, 85, 130)
cost_usd <- sample(cost_grid, n_rows, replace = TRUE)

resp_female <- rbinom(M, 1L, 0.51)
age_std     <- as.numeric(scale(round(runif(M, 18, 82))))
resp_ideo   <- as.numeric(scale(rnorm(M)))

attr_df <- data.frame(distribution, enforcement, monitoring,
                      participation, sanctions, cost_usd)
X <- stats::model.matrix(~ distribution + enforcement + monitoring +
                         participation + sanctions + cost_usd,
                         data = attr_df)[, -1L, drop = FALSE]
colnames(X) <- make.names(colnames(X))

p <- ncol(X)
beta_base <- setNames(numeric(p), colnames(X))
beta_base[grepl("distributionability_to_pay",      names(beta_base))] <-  0.25
beta_base[grepl("distributionhistorical_polluter", names(beta_base))] <-  0.15
beta_base[grepl("enforcementtrade_sanctions",      names(beta_base))] <-  0.20
beta_base[grepl("enforcementfines",                names(beta_base))] <-  0.30
beta_base[grepl("monitoringnational",              names(beta_base))] <-  0.10
beta_base[grepl("monitoringinternational",         names(beta_base))] <-  0.30
beta_base[grepl("participation75pct",              names(beta_base))] <-  0.30
beta_base[grepl("participation90pct",              names(beta_base))] <-  0.60
beta_base[grepl("sanctionstrade",                  names(beta_base))] <-  0.15
beta_base[grepl("sanctionsaid_cut",                names(beta_base))] <-  0.10
beta_base[grepl("cost_usd",                        names(beta_base))] <- -0.015

beta_true <- matrix(0, M, p, dimnames = list(NULL, colnames(X)))
for (j in seq_len(p)) beta_true[, j] <- beta_base[j]
## Heterogeneity: more-liberal respondents (resp_ideo < 0) are less
## price-sensitive and more supportive of ambitious participation.
beta_true[, "cost_usd"] <- beta_true[, "cost_usd"] - 0.005 * resp_ideo
if ("participation90pct" %in% colnames(beta_true)) {
  beta_true[, "participation90pct"] <- beta_true[, "participation90pct"] -
    0.30 * resp_ideo
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

bs2013 <- data.frame(
  respondent = rid,
  task = tid,
  profile = pos,
  choice = choice,
  distribution = distribution,
  enforcement = enforcement,
  monitoring = monitoring,
  participation = participation,
  sanctions = sanctions,
  cost_usd = cost_usd,
  resp_female = resp_female[rid],
  age = age_std[rid],
  resp_ideo = resp_ideo[rid],
  stringsAsFactors = FALSE
)

attr(bs2013, "synthetic") <- TRUE
attr(bs2013, "source") <- "Bechtel & Scheve (2013) design; synthetic data with matching structure."

out_path <- file.path("data", "bs2013.rda")
save(bs2013, file = out_path, compress = "xz", version = 2L)
cat(sprintf("Wrote %s (%.1f KB)\n", out_path,
            file.info(out_path)$size / 1024))
