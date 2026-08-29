#!/usr/bin/env Rscript
## One-step (DML) inference for the structural AME on Saha--Weeks.
## Fit: the v2.1 postpilot-final SELECTED assembled fit (worktree), the same
## object the evidence chain's diagnostic DML ran on. Inference settings copy
## the evidence chain's postfit config (riesz 0.05 / ridge 0.10, active
## eigenvalue 1e-6, information 1e-8, validation fraction 0.2,
## multiplier_draws 0), and the fold basis replicates .swv21_fold_basis.
## Draw set: est_AME's construction at seed 1, matching the banked plug-in
## run (ame_sw.csv), so the DML plug-in column is comparable to the banked
## ame_neutral within design Monte Carlo error.
##
## TARGET (audit work package A1). This runner estimates the FIXED-DRAW
## AME Psi_M, conditional on the frozen draw set recorded in
## ame_dml_draw_spec.csv. Its intervals cover Psi_M, not the exact
## design-integrated Psi. The `mc_ok` column is a finite-sample heuristic,
## not the paper's numerical-integration condition, and is named to say so.
##
## Probe mode (AME_PROBE=1): M = 1000, two targets, to measure cost.
## Full mode: M = 20000, all 13 coordinates, position-neutral.
## Output (worktree estimands/): ame_dml_sw.csv, ame_dml_inference.rds.

options(stringsAsFactors = FALSE, warn = 1)
t0 <- Sys.time()
log <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n"); flush.console()
}

root <- path.expand("~/GitHub/sconjoint")
wt <- path.expand("~/GitHub/sconjoint-v21-repro")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/ame_dml.R"))
source(file.path(root, "applications/R/estimands_v21.R"))
source(file.path(root, "applications/R/provenance.R"))

probe <- identical(Sys.getenv("AME_PROBE"), "1")
M_D <- if (probe) 1000L else 20000L
dir <- file.path(wt, "applications/sw2022/results/mixed_logit_v2_1_postpilot_final")
out_dir <- file.path(dir, "estimands")
dir.create(out_dir, showWarnings = FALSE)

assembled <- readRDS(file.path(dir, "fit_selected_assembled.rds"))
prepared <- readRDS(file.path(wt, "applications/sw2022/results/prep_analysis_data.rds"))
full <- est_fit(file.path(dir, "fit_selected_full.rds"),
                file.path(wt, "applications/sw2022/results/prep_analysis_data.rds"))
log("fit loaded: N", length(unique(assembled$respondent_id)),
    "p", ncol(assembled$deltaX), "K", assembled$K, "q", assembled$q)

attrs <- list(
  gender = "cand_genderMale",
  run = "cand_runYes",
  talent = c("cand_talentCollaborative", "cand_talentDetermined.to.Succeed",
             "cand_talentEmpathetic", "cand_talentGood.Communicator",
             "cand_talentHard.Working", "cand_talentTough.Negotiator"),
  agenda = c("cand_agendaModerate.Changes", "cand_agendaComplete.Overhaul"),
  children = c("cand_child1.child", "cand_child2.children",
               "cand_child3.children"))
stopifnot(setequal(unlist(attrs), assembled$attr_names))
if (probe) attrs <- list(gender = "cand_genderMale", run = "cand_runYes")

## Fold basis: verbatim logic of .swv21_fold_basis (postfit_helpers_v2_1.R).
fold_basis <- local({
  rid <- unique(as.character(prepared$respondent_id))
  first <- match(rid, as.character(prepared$respondent_id))
  fold_raw <- assembled$fold_id
  fold_task <- if (length(fold_raw) == nrow(prepared$deltaX)) {
    as.integer(fold_raw)
  } else as.integer(fold_raw)[match(as.character(prepared$respondent_id), rid)]
  fold_resp <- fold_task[first]
  Z_resp <- as.matrix(prepared$Z_primary)[first, , drop = FALSE]
  K <- length(unique(fold_resp))
  lapply(seq_len(K), function(k) {
    train <- fold_resp != k
    center <- colMeans(Z_resp[train, , drop = FALSE])
    scale <- apply(Z_resp[train, , drop = FALSE], 2L, stats::sd)
    scale[!is.finite(scale) | scale < 1e-12] <- 1
    B <- cbind(`(Intercept)` = 1,
               sweep(sweep(Z_resp, 2L, center, `-`), 2L, scale, `/`))
    qr_train <- qr(B[train, , drop = FALSE], tol = 1e-10, LAPACK = FALSE)
    keep <- sort(qr_train$pivot[seq_len(qr_train$rank)])
    B[, keep, drop = FALSE]
  })
})
log("fold basis built:", paste(vapply(fold_basis, ncol, 1L), collapse = "/"),
    "columns per fold")

built <- ame_dml_targets(assembled$attr_names, attrs, M_D = M_D, seed = 1L,
                         n_nodes = 31L, position_neutral = TRUE,
                         chunk = 2048L)
## Instrument every callback with a call counter and timer.
.calls <- new.env(); .calls$n <- 0L
targets <- lapply(built$targets, function(cb) {
  function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    tt <- system.time(out <- cb(mu, kappa, Sigma, Z, respondent_id, fold,
                                attr_names))
    .calls$n <- .calls$n + 1L
    log("callback", .calls$n, sprintf("(%.1fs)", tt[["elapsed"]]))
    out
  }
})
names(targets) <- names(built$targets)
log("targets built:", length(targets), "M =", M_D)

inf <- scmix_dml(
  assembled, targets = character(0), plugin_targets = targets,
  mu_basis = fold_basis, nu_grid = NULL, riesz_penalty = "identity",
  riesz_validation_fraction = 0.2,
  active_eigenvalue_min = 1e-6,
  rank_tolerance = 1e-8,
  information_eigenvalue_min = 1e-8,
  riesz_equation_tolerance = 0.05,
  ridge_sensitivity_tolerance = 0.10,
  allow_numeric_derivatives = FALSE, verification = NULL,
  multiplier_draws = 0L, level = 0.95, seed = 20260827L)
log("scmix_dml done: status", inf$status,
    "| riesz_max", sprintf("%.4f", inf$riesz_equation_max_relative_residual),
    "| ridge_max", sprintf("%.4f", inf$ridge_max_relative_sensitivity),
    "| callbacks", .calls$n)

## Full-sample product-form plug-in + design Monte Carlo se, same draws.
## The value doubles as the bridge to the banked plug-in run: identical
## seed and M give the identical draw set, so it must match the banked
## joint-sampled ame_neutral within that run's own mc_se.
Sigma_full <- tcrossprod(full$A)
mcv <- lapply(names(built$targets), function(nm) {
  cn <- sub("^ame_fixed_draw_neutral:", "", nm)
  a <- names(attrs)[vapply(attrs, function(v) cn %in% v, logical(1L))]
  pair <- ame_contrast_pair(built$draws, a,
                            built$draws$coord_index[[a]][match(cn, attrs[[a]])])
  ame_design_mc_se(full$mu, full$kappa, Sigma_full, pair$d_focal, pair$d_ref,
                   n_nodes = 31L, position_neutral = TRUE, chunk = 2048L)
})
names(mcv) <- names(built$targets)
mc <- vapply(mcv, `[[`, 0, "mc_se")
full_plugin <- vapply(mcv, `[[`, 0, "value")
log("design mc_se range:", sprintf("%.2e", min(mc)), "-",
    sprintf("%.2e", max(mc)))

se_diag <- sqrt(diag(as.matrix(inf$diagnostic_covariance)))
res <- data.frame(
  coordinate = sub("^ame_fixed_draw_neutral:", "", names(inf$estimate)),
  target = "ame_fixed_draw",
  plugin = as.numeric(inf$plugin_estimate),
  one_step = as.numeric(inf$estimate),
  diagnostic_se = as.numeric(se_diag),
  full_fit_plugin = as.numeric(full_plugin[names(inf$estimate)]),
  design_mc_se = as.numeric(mc[names(inf$estimate)]),
  status = inf$status,
  riesz_max = inf$riesz_equation_max_relative_residual,
  ridge_max = inf$ridge_max_relative_sensitivity)
banked <- file.path(out_dir, "ame_sw.csv")
if (file.exists(banked)) {
  b <- read.csv(banked)
  res <- merge(res, b[, c("coordinate", "ame_neutral", "mc_se",
                          "design_amce")],
               by = "coordinate", all.x = TRUE, sort = FALSE)
  names(res)[names(res) == "ame_neutral"] <- "banked_plugin_neutral"
  names(res)[names(res) == "mc_se"] <- "banked_mc_se"
}
## DIAGNOSTIC HEURISTIC, not the paper's condition. An O_p(M^{-1/2}) design
## Monte Carlo error does not establish sqrt(N)|Psi_M - Psi| = o_p(1); the
## intervals above cover Psi_M regardless. Named `mc_heuristic_ok` so no
## reader can take it for a certificate.
res$mc_heuristic_ok <- res$design_mc_se < 0.25 * res$diagnostic_se
## Bridge to the banked plug-in run (identical draws; their estimator adds
## respondent-pairing noise quantified by banked_mc_se).
if ("banked_plugin_neutral" %in% names(res)) {
  res$bridge_ok <- abs(res$full_fit_plugin - res$banked_plugin_neutral) <
    pmax(4 * res$banked_mc_se, 1e-6)
}
print(res, digits = 4)

suffix <- if (probe) "_probe" else ""
res <- sb_stamp_provenance(res, app = "sw2022",
                           profile = "mixed_logit_v2_1_postpilot_final",
                           fit = assembled, calibration = NULL,
                           seed = 20260827L,
                           producer = "applications/sw2022/ame_dml_run.R",
                           target_label = "ame_fixed_draw",
                           sources = "applications/R/ame_dml.R")
spec <- ame_draw_spec(built$draws, mc_se = mc)
res$prov_draw_hash <- spec$draw_hash
res$prov_draw_M <- spec$M
res$prov_draw_seed <- spec$seed
res$prov_design_law <- spec$design_law
res$prov_integration_contract <- spec$integration_contract
write.csv(res, file.path(out_dir, paste0("ame_dml_sw", suffix, ".csv")),
          row.names = FALSE)
prov_write_manifest(out_dir, c(
  list(artifact = paste0("ame_dml_sw", suffix, ".csv")), spec,
  list(commit = prov_git_commit(root))),
  name = paste0("provenance_ame", suffix, ".csv"))
saveRDS(inf, file.path(out_dir, paste0("ame_dml_inference", suffix, ".rds")))
log("written:", file.path(out_dir, paste0("ame_dml_sw", suffix, ".csv")))
log("total", sprintf("%.1f min", as.numeric(difftime(Sys.time(), t0,
                                                     units = "mins"))))
log("DONE")
