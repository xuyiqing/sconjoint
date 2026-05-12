## inst/benchmarks/se_ratio_sw2022_paper_config.R
##
## Test the hypothesis that the SE-ratio gap on sw2022 (package 1.04
## vs paper 1.78) closes when scfit() is trained at the paper's
## reported config: K = 50, n_epochs = 5000.
##
## v0.2.1 spec.md investigation: at K = 5-50 and n_epochs <= 300, the
## package gives ratio ~1.04 stably.  Hypothesis: longer training
## tightens per-respondent beta_hat enough that the DML correction
## term becomes small relative to beta_hat, raising within-respondent
## constancy, raising rho_between, raising the SE ratio.

suppressPackageStartupMessages({
  library(sconjoint)
})

data(sw2022, package = "sconjoint")

t0 <- Sys.time()
## Config knobs.  Paper target is K=50, n_epochs=5000; here we run
## K_test / NE_test for fit-time tractability.  Override via env
## vars when you have the wall budget for the full paper config.
K_test  <- as.integer(Sys.getenv("SC_K",  unset = "50"))
NE_test <- as.integer(Sys.getenv("SC_NE", unset = "5000"))
NC_test <- as.integer(Sys.getenv("SC_NC", unset = "8"))
cat("[", format(t0, "%H:%M:%S"), "] starting K=", K_test,
    ", n_epochs=", NE_test, ", n_cores=", NC_test, "\n", sep = "")

fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
               resp_female + age + pid,
             data = sw2022,
             respondent = "respondent", task = "task", profile = "profile",
             K = K_test, n_epochs = NE_test,
             seed = 1, stage2 = "map_c5",
             parallel = TRUE, n_cores = NC_test,
             verbose = FALSE)

t1 <- Sys.time()
cat("[", format(t1, "%H:%M:%S"), "] done in ",
    round(as.numeric(difftime(t1, t0, units = "mins")), 1),
    " min\n", sep = "")

se_clu  <- sqrt(diag(fit$vcov))
se_iid  <- sqrt(diag(fit$vcov_iid))
ratio   <- se_clu / se_iid

cat("\nPer-param cluster/iid SE ratio:\n")
print(setNames(round(ratio, 3), names(fit$theta)))
cat(sprintf("\nMean ratio across params: %.3f\n", mean(ratio)))
cat("Paper target: 1.78\n")

## sw2022 agenda importance (vs paper's 0.65)
imp_dv      <- sc_importance(fit, design = "design_variance")$estimate
imp_uniform <- sc_importance(fit, design = "uniform")$estimate
imp_emp     <- sc_importance(fit, design = "empirical")$estimate
imp_dnn_dv  <- sc_importance(fit, design = "design_variance",
                             which_beta = "dnn")$estimate
agenda <- function(df) round(df$share[df$attribute == "agenda"], 3)
cat("\nAgenda importance (paper=0.65):\n",
    "  hybrid + design_variance (default) = ", agenda(imp_dv),    "\n",
    "  hybrid + uniform                   = ", agenda(imp_uniform), "\n",
    "  hybrid + empirical                 = ", agenda(imp_emp),   "\n",
    "  dnn    + design_variance           = ", agenda(imp_dnn_dv), "\n",
    sep = "")

saveRDS(list(
  fit_lite = list(
    theta = fit$theta,
    vcov  = fit$vcov,
    vcov_iid = fit$vcov_iid,
    K = K_test, n_epochs = NE_test,
    sd_dx = fit$sd_dx,
    normalize_deltaX = fit$normalize_deltaX,
    stage2_method = fit$stage2_method
  ),
  se_ratio  = ratio,
  importance = list(
    hybrid_design_variance = imp_dv,
    hybrid_uniform         = imp_uniform,
    hybrid_empirical       = imp_emp,
    dnn_design_variance    = imp_dnn_dv
  ),
  wall_secs = as.numeric(difftime(t1, t0, units = "secs"))
), "inst/benchmarks/se_ratio_sw2022_paper_config.rds")

cat("\nWrote inst/benchmarks/se_ratio_sw2022_paper_config.rds\n")
