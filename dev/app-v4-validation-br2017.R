# E3: tax external-validation correlations from integrated-model
# posterior means (replaces the two-stage MAP coefficients in the v3
# check; the check itself transfers unchanged since correlation is
# scale- and shrinkage-tolerant).
#
# v3 comparators (rerun_FIXED, replicate compare_results.R):
#   r_slope 0.4302, r_gap 0.4666 (n = 401); r_top 0.42 (n = 2000);
#   by party 0.29 / 0.36 / 0.39.
source("dev/app-v4-common.R")

mx <- readRDS(file.path(OUT_DIR, "scmix_fit_br2017.rds"))

## posterior means: reuse the E2 slot when present, else compute
qpath <- file.path(OUT_DIR, "v4_quantities_br2017.rds")
post <- NULL
if (file.exists(qpath)) {
  Q <- readRDS(qpath)
  post <- Q$posterior
}
if (is.null(post)) post <- scmix_posterior(mx, what = "mean")

RATE_COLS <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
               "rate_175_375", "rate_375P")
B <- post$mean
stopifnot(all(c(RATE_COLS, "revenue_score") %in% colnames(B)))

## ID-join to the frozen respondent metadata (never positional)
mats <- readRDS(file.path(REPL_DIR, "data", "derived", "br2017",
                          "prep_matrices.rds"))
meta_ids <- as.character(mats$resp_meta$respondent)
stopifnot(setequal(post$respondent, meta_ids))
mxm <- mats$resp_meta[match(post$respondent, meta_ids), ]
stopifnot(identical(as.character(mxm$respondent), post$respondent))

party <- party3_from_pid7(mxm$pid7)
levels(party) <- c("Dem", "Indep", "Rep")

## constructions verbatim from compare_results.R qty()
log_mid_c <- log(c(5, 22.5, 60, 130, 275, 500))
log_mid_c <- log_mid_c - mean(log_mid_c)
slopes <- as.numeric(B[, RATE_COLS] %*% log_mid_c) / sum(log_mid_c^2)
tmb <- B[, "rate_375P"] - B[, "rate_L10"]
ideal_tmb <- mxm$ideal_gt375 - mxm$ideal_L10
hb <- !is.na(mxm$ideal_L10) & !is.na(mxm$ideal_gt375)
ht <- !is.na(mxm$ideal_gt375)
stopifnot(sum(hb) == 401L, sum(ht) == 2000L)

r_slope <- stats::cor(slopes[hb], ideal_tmb[hb])
r_gap <- stats::cor(tmb[hb], ideal_tmb[hb])
r_top <- stats::cor(B[ht, "rate_375P"], mxm$ideal_gt375[ht])
r_top_party <- vapply(c("Dem", "Indep", "Rep"), function(p) {
  i <- which(ht & party == p)
  stats::cor(B[i, "rate_375P"], mxm$ideal_gt375[i])
}, numeric(1L))

out <- list(r_slope = r_slope, r_gap = r_gap, r_top = r_top,
            r_top_party = r_top_party,
            n_both = sum(hb), n_top = sum(ht),
            v3 = list(r_slope = 0.4302, r_gap = 0.4666, r_top = 0.42,
                      r_top_party = c(Dem = 0.29, Indep = 0.36,
                                      Rep = 0.39)),
            basis = "scmix posterior means (P10), ID-joined to resp_meta")
saveRDS(out, file.path(OUT_DIR, "v4_validation_br2017.rds"))

say("r_slope: %.4f (v3 0.4302, n = %d)", r_slope, sum(hb))
say("r_gap:   %.4f (v3 0.4666)", r_gap)
say("r_top:   %.4f (v3 0.42, n = %d)", r_top, sum(ht))
say("r_top by party: %s (v3 0.29 / 0.36 / 0.39)",
    paste(sprintf("%s %.3f", names(r_top_party), r_top_party),
          collapse = ", "))
dv <- c(r_slope - 0.4302, r_gap - 0.4666, r_top - 0.42)
if (any(abs(dv) > 0.15) ||
    sign(r_slope) != 1 || sign(r_gap) != 1 || sign(r_top) != 1) {
  say("WARNING: validation correlation moved by more than 0.15 or flipped sign -- investigate before use")
}
cat("\nDONE E3 br2017 validation\n")
