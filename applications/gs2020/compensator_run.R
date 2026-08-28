#!/usr/bin/env Rscript
## Compensator columns on the gs2020 v21_corrected assembled fit
## (queue item 5; algorithms memo Section 8). First run of the audited
## engine on a real fit. All outputs PROVISIONAL pending author review.
##
## Input decisions, recorded:
##   - ideology: raw ideo7 per respondent (engine applies the archived
##     1-3 / 4 / 5-7 partition via cc_ideology_group).
##   - floors: the bound memo's per-coordinate zero-calibration floor
##     (share_bound_floor.rds), reduced per column by cc_reduce_floor's
##     conservative rule (max over coordinates entering any support
##     line), then maximized over the seven actions -- one scalar per
##     spec, the hardest gate. Judgment call per the engine's docs.
##   - none_bounds = NULL, deliberately. The Section 8 inheritance rule
##     presupposes an acceptance-side certified lower bound for the
##     None cell; the gs bound rows certify the OPPOSITION side (all
##     seven actions have negative modal side), whose acceptance-side
##     implication is a trivial lower bound of zero. Floored cells are
##     therefore withheld rather than given a wrong-side bound; the
##     orientation question is flagged for author review.

options(stringsAsFactors = FALSE, warn = 1)
log <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n"); flush.console() }
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/compensator_columns.R"))

dir <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected")
out <- file.path(dir, "estimands")
assembled <- readRDS(file.path(dir, "fit_primary_assembled.rds"))
prep <- readRDS(file.path(root, "applications/gs2020/results/prep_analysis_data.rds"))
fl <- readRDS(file.path(dir, "share_bound_floor.rds"))

inp <- cc_fit_inputs(assembled)
log("fit: n_resp", nrow(inp$mu), "p", ncol(inp$mu), "q", inp$q, "K", inp$K)

meta <- prep$respondent_meta
ideo7 <- meta$ideo7[match(inp$respondents, as.character(meta$respondent_id))]
stopifnot(!anyNA(ideo7), all(ideo7 %in% 1:7))

ACT <- grep("^diff_dem_code_u_", inp$attr_names, value = TRUE)
GOV <- grep("^diff_dem_code_g_", inp$attr_names, value = TRUE)
stopifnot(length(ACT) == 7L, length(GOV) >= 1L,
          all(c("diff_respParty", "diff_p1_num", "diff_p2_num") %in%
                inp$attr_names))
log("actions:", length(ACT), "| governance:", length(GOV))

## Per-spec conservative floors (max over actions of the reduced floor).
fv <- fl$floor[inp$attr_names]
stopifnot(!anyNA(fv))
floors <- sapply(cc_specs(), function(sp) {
  max(vapply(ACT, function(a) {
    sel <- cc_selectors(inp$attr_names, action = a,
                        coparty = "diff_respParty", econ = "diff_p1_num",
                        social = "diff_p2_num", governance = GOV)
    cc_reduce_floor(as.numeric(fv), sel, sp)
  }, numeric(1L)))
})
log("per-spec floors:", paste(sprintf("%s=%.3f", names(floors), floors),
                              collapse = "  "))
log("floor artifact signature:", fl$analysis_signature,
    "| R =", fl$R, "gamma =", fl$gamma)

acts <- setNames(as.list(ACT), sub("^diff_dem_code_u_", "", ACT))
tab <- cc_compensator_columns(
  assembled, actions = acts, coparty = "diff_respParty",
  econ = "diff_p1_num", social = "diff_p2_num", governance = GOV,
  ideology = ideo7, floors = floors, none_bounds = NULL,
  assert_units = TRUE)
log("table rows:", nrow(tab))

dom <- cc_check_domination(tab)
log("domination violations:", nrow(dom))
stopifnot(nrow(dom) == 0L)

## Same-path capstone (memo check i): the engine's party column must
## match the closed form ON THE SAME assembled inputs to ~1e-13.
cap_dev <- vapply(names(acts), function(nm) {
  cvec <- as.numeric(inp$attr_names == acts[[nm]]) +
    as.numeric(inp$attr_names == "diff_respParty")
  cf <- cc_closed_form_linear(inp$mu, inp$A, cvec, loading_form = "rows")
  eng <- tab[tab$action == nm & tab$group == "Overall" &
               tab$spec == "party", "share"]
  abs(mean(cf) - eng)
}, numeric(1L))
log("capstone: engine party vs closed form, max dev",
    sprintf("%.2e", max(cap_dev)))
stopifnot(max(cap_dev) < 1e-12)

## Informational: compare against the archived full-fit est_C0 run.
## The archived run used the FULL-sample fit; this engine uses the
## cross-fitted assembled means and fold-local loadings, so deviations
## of order 1e-2 are expected, not failures.
arch_path <- file.path(out, "compdiff_C0.csv")
if (file.exists(arch_path)) {
  arch <- read.csv(arch_path)
  m <- merge(tab[tab$spec == "party", c("action", "group", "share")],
             arch, by = c("action", "group"))
  log("vs archived full-fit C0 (party column):",
      sprintf("max |dev| = %.4f over %d cells",
              max(abs(m$share.x - m$share.y)), nrow(m)))
}

keep_cols <- c("action", "group", "spec", "column", "n_respondents",
               "share", "released_value", "release_kind", "floor",
               "floored", "max_abs_slope", "min_respondent_max_slope",
               "sensitivity", "sensitivity_max",
               "n_binding_below_floor", "frac_binding_below_floor",
               "n_endpoint_tied", "n_zero_line")
keep_cols <- intersect(keep_cols, names(tab))
write.csv(tab[, keep_cols], file.path(out, "compensator_columns.csv"),
          row.names = FALSE)
saveRDS(tab, file.path(out, "compensator_columns.rds"))

## Console summary: the Overall row per action x spec.
ov <- tab[tab$group == "Overall", c("action", "spec", "share",
                                    "released_value", "release_kind",
                                    "floored", "max_abs_slope", "floor")]
print(ov, digits = 3)
log("written:", file.path(out, "compensator_columns.csv"))
log("DONE")
