#!/usr/bin/env Rscript
## sw2022: S_0 sign shares for all 13 level-vs-reference contrasts on the
## v2.1 reproduction fit (closed form + simulation cross-check per the
## algorithm memo). PROVISIONAL pending author review.
options(stringsAsFactors = FALSE, warn = 1)
root <- path.expand("~/GitHub/sconjoint")
wt <- path.expand("~/GitHub/sconjoint-v21-repro")
suppressPackageStartupMessages(pkgload::load_all(wt, quiet = TRUE))
source(file.path(root, "applications/R/estimands_v21.R"))
dir <- file.path(wt, "applications/sw2022/results/mixed_logit_v2_1_postpilot_final")
out <- file.path(dir, "estimands")
fit <- est_fit(file.path(dir, "fit_selected_full.rds"),
               file.path(wt, "applications/sw2022/results/prep_analysis_data.rds"))
rows <- list(); ok <- TRUE
for (k in fit$coord) {
  cv <- as.numeric(fit$coord == k)
  r <- est_S0(fit, cv)
  if (!all(r$checks$pass)) { ok <- FALSE; cat("CHECK FAILED:", k, "\n") }
  rows[[k]] <- data.frame(coordinate = k, share_positive = r$value[["Overall"]],
                          s2 = r$s2, floored = r$floored,
                          checks_pass = all(r$checks$pass))
}
df <- do.call(rbind, rows)
write.csv(df, file.path(out, "sign_shares_13.csv"), row.names = FALSE)
cat("SW SHARES13 DONE | all checks:", ok, "\n")
print(df[, c("coordinate", "share_positive", "floored")], digits = 3)
