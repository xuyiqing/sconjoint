# E1 sensitivity: gs2020 residual dimension q = 2 and q = 3.
#
# The q = 1 fit concentrates the residual factor on diff_respParty and
# lands the zero-floor ratio at 1.45 (< 2). Before reading that as "the
# distributional tier is unsupported here", check whether a richer
# residual dimension recovers structure a single factor cannot express
# (p = 30 makes q = 1 a tight constraint). Cache-gated like the main
# E1 script; each q gets its own fit + zero floor + design check.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
stopifnot(dir.exists(OUT_DIR))

ts <- function() format(Sys.time(), "[%H:%M:%S]")
say <- function(...) cat(ts(), sprintf(...), "\n")

bt <- function(x) paste0("`", x, "`")
gs_attrs <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote", "diff_dem_code_v_affair",
  "diff_dem_code_v_tax", "diff_sex_Female", "diff_race_Asian",
  "diff_race_Black", "diff_race_Hispanic", "diff_pro_Farmer",
  "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")
gs_z <- c("z_ideo", "z_pid7", "z_trump", "z_age", "z_educ", "z_hhi",
          "z_auth", "z_knowl", "z_female", "z_race_black", "z_race_asian",
          "z_race_other", "E_ideal", "I_ideal", "M_ideal", "T_ideal")
gs_formula <- stats::as.formula(
  paste("choice ~", paste(bt(gs_attrs), collapse = " + "),
        "|", paste(bt(gs_z), collapse = " + ")))

data(gs2020, package = "sconjoint")

summarize_fit <- function(mx, tag) {
  sc <- sconjoint:::.scmix_scores(mx)
  Sig <- Reduce(`+`, lapply(mx$A_folds, tcrossprod)) / length(mx$A_folds)
  sd_resid <- sqrt(pmax(diag(Sig), 0))
  names(sd_resid) <- mx$attr_names
  say("%s | sum out-of-fold loglik: %.1f", tag, sum(sc$loglik))
  say("%s | top residual SDs: %s", tag,
      paste(sprintf("%s=%.2f", names(sort(sd_resid, decreasing = TRUE))[1:6],
                    sort(sd_resid, decreasing = TRUE)[1:6]), collapse = ", "))
}

for (qq in 2:3) {
  fit_path <- file.path(OUT_DIR, sprintf("scmix_fit_gs2020_q%d.rds", qq))
  if (file.exists(fit_path)) {
    mx <- readRDS(fit_path)
    say("q=%d fit loaded from cache", qq)
  } else {
    say("q=%d fit starting", qq)
    t0 <- Sys.time()
    mx <- scmix(gs_formula, gs2020, respondent = "respondent", task = "task",
                profile = "profile", q = qq, K = 5L, n_epochs = 600L,
                seed = 42L)
    say("q=%d fit done: %.1f min", qq,
        as.numeric(difftime(Sys.time(), t0, units = "mins")))
    saveRDS(mx, fit_path)
  }
  if (is.null(mx$zero_floor)) {
    say("q=%d zero floor starting (R = 2)", qq)
    t0 <- Sys.time()
    mx$zero_floor <- scmix_calibrate_zero(mx, R = 2L)
    say("q=%d zero floor done: %.1f min | ratio = %.2f", qq,
        as.numeric(difftime(Sys.time(), t0, units = "mins")),
        mx$zero_floor$ratio)
    saveRDS(mx, fit_path)
  } else {
    say("q=%d zero floor cached: ratio = %.2f", qq, mx$zero_floor$ratio)
  }
  dc_path <- file.path(OUT_DIR, sprintf("design_check_gs2020_q%d.rds", qq))
  if (!file.exists(dc_path)) {
    dc <- scmix_design_check(mx, n_bins = 50L, seed = 7L)
    saveRDS(dc, dc_path)
    say("q=%d design check | weak directions: %d | identified coords: %d/30",
        qq, dc$weak_directions, length(dc$identified))
  }
  summarize_fit(mx, sprintf("q=%d", qq))
}

## q = 1 reference line for the comparison table
mx1 <- readRDS(file.path(OUT_DIR, "scmix_fit_gs2020.rds"))
summarize_fit(mx1, "q=1")
say("q=1 floor ratio: %.2f", mx1$zero_floor$ratio)

cat("\nDONE E1 gs2020 q-sweep\n")
