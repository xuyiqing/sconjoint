#!/usr/bin/env Rscript
## Compensator columns on the gs2020 v21_corrected assembled fit
## (queue item 5; algorithms memo Section 8). First run of the audited
## engine on a real fit. All outputs PROVISIONAL pending author review.
##
## Input decisions, recorded (revised 2026-08-28 after the estimand/bounds
## audit; see history/sconjoint/agent-exchange/2026-08-28-*.md):
##   - ideology: raw ideo7 per respondent (engine applies the archived
##     1-3 / 4 / 5-7 partition via cc_ideology_group).
##   - floors: the MATCHED COMPOSITE calibration when signed loadings are
##     available (share_bound_floor_signed.rds, which reproduces the
##     calibration of record exactly and additionally retains A~), and the
##     triangle envelope only as a named fallback. Either way the
##     aggregation is the same: fold-average within replication, maximise
##     over the column's support lines within the replication, then the
##     (1-gamma) quantile across replications. The former cc_reduce_floor()
##     rule (max MARGINAL coordinate floor) is not conservative for
##     composite contrasts such as c_p + 3c_e + 3c_s and is not used for
##     the gate; it is printed for comparison only.
##   - release gate: fail-closed on BINDING slopes. A cell releases only
##     when every respondent's binding slope clears the column floor. The
##     pre-audit rule released on the cell-wide MAXIMUM support-line slope,
##     which let one favourable respondent release a cell in which most
##     binding slopes sat below the floor.
##   - none_bounds = NULL, deliberately. Pointwise domination transfers a
##     lower bound only for the SAME event. These cells need an
##     ACCEPTANCE-side lower bound; the gs bound rows certify the
##     OPPOSITION side, whose acceptance-side implication is the trivial
##     zero. The engine now enforces this with `event_side`, so a
##     wrong-side or unknown-side bound is refused rather than inherited.

options(stringsAsFactors = FALSE, warn = 1)
log <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n"); flush.console() }
root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/compensator_columns.R"))
source(file.path(root, "applications/R/share_bounds.R"))
source(file.path(root, "applications/R/provenance.R"))

dir <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected")
out <- file.path(dir, "estimands")
assembled <- readRDS(file.path(dir, "fit_primary_assembled.rds"))
prep <- readRDS(file.path(root, "applications/gs2020/results/prep_analysis_data.rds"))
fl <- readRDS(file.path(dir, "share_bound_floor.rds"))
## Prefer the signed calibration when it exists. It is the SAME
## calibration --- run_floor_signed.R refuses to write it unless the
## per-coordinate norms reproduce share_bound_floor.rds exactly --- with
## the signed loading matrices retained, which is what the exact composite
## ceiling needs.
signed_path <- file.path(dir, "share_bound_floor_signed.rds")
cal <- fl
if (file.exists(signed_path)) {
  cs <- readRDS(signed_path)
  if (isTRUE(cs$reproduces_calibration_of_record) &&
      sb_calibration_has_loadings(cs)) {
    cal <- cs
  } else {
    warning("share_bound_floor_signed.rds is present but does not certify ",
            "that it reproduces the calibration of record; ignoring it.",
            call. = FALSE)
  }
}

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

## Per-spec floors: composite ceiling per column, maximised over the seven
## actions. The hardest gate the calibration supports.
stopifnot(ncol(as.matrix(cal$draws)) == length(inp$attr_names))
mk_sel <- function(a) cc_selectors(inp$attr_names, action = a,
                                   coparty = "diff_respParty",
                                   econ = "diff_p1_num",
                                   social = "diff_p2_num", governance = GOV)
comp <- lapply(cc_specs(), function(sp)
  vapply(ACT, function(a)
    cc_composite_floor(cal, mk_sel(a), sp, gamma = cal$gamma)$column,
    numeric(1L)))
names(comp) <- cc_specs()
floors <- vapply(comp, max, numeric(1L))
fr <- cc_composite_floor(cal, mk_sel(ACT[[1]]), "pol", gamma = cal$gamma)
floor_rule <- fr$floor_rule
floor_rule_detail <- fr$rule
log("floor method:", floor_rule)
## The triangle fallback on the same calibration, for the comparison the
## audit asks for: the matched composite ceiling must never EXCEED it.
tri <- vapply(cc_specs(), function(sp)
  max(vapply(ACT, function(a)
    cc_composite_floor(cal, mk_sel(a), sp, gamma = cal$gamma,
                       method = "triangle_fallback")$column,
    numeric(1L))), numeric(1L))
log("triangle fallback floors:",
    paste(sprintf("%s=%.3f", names(tri), tri), collapse = "  "))
log("matched / triangle ratio:",
    paste(sprintf("%s=%.2f", names(floors), floors / tri), collapse = "  "))
stopifnot(`matched composite floor must not exceed the triangle envelope` =
            all(floors <= tri + 1e-9))
## Pre-audit marginal-maximum rule, printed for comparison only.
fv <- fl$floor[inp$attr_names]
stopifnot(!anyNA(fv))
floors_marginal <- vapply(cc_specs(), function(sp)
  max(vapply(ACT, function(a)
    cc_reduce_floor(as.numeric(fv), mk_sel(a), sp, quiet = TRUE),
    numeric(1L))), numeric(1L))
log("per-spec floors (matched composite envelope):",
    paste(sprintf("%s=%.3f", names(floors), floors), collapse = "  "))
log("per-spec floors (pre-audit marginal max, NOT used):",
    paste(sprintf("%s=%.3f", names(floors_marginal), floors_marginal),
          collapse = "  "))
log("envelope / marginal ratio:",
    paste(sprintf("%s=%.2f", names(floors), floors / floors_marginal),
          collapse = "  "))
log("floor artifact signature:", cal$analysis_signature,
    "| R =", cal$R, "gamma =", cal$gamma,
    "| signed loadings:", sb_calibration_has_loadings(cal))

acts <- setNames(as.list(ACT), sub("^diff_dem_code_u_", "", ACT))
tab <- cc_compensator_columns(
  assembled, actions = acts, coparty = "diff_respParty",
  econ = "diff_p1_num", social = "diff_p2_num", governance = GOV,
  ideology = ideo7, floors = floors, none_bounds = NULL,
  required_event_side = "acceptance", floor_rule = floor_rule,
  floor_rule_detail = floor_rule_detail, assert_units = TRUE)
log("table rows:", nrow(tab))
log("released as point:", sum(tab$release_kind == "point"),
    "| withheld:", sum(grepl("^withheld", tab$release_kind)),
    "| inherited bound:", sum(tab$release_kind == "lower_bound"))

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
               "share", "released_value", "release_kind",
               "inherited_bound", "inherited_event_side",
               "required_event_side", "floor", "floor_rule",
               "floor_rule_detail",
               "floored", "gate_rule", "n_gate_failures",
               "frac_gate_failures", "binding_slope_min",
               "binding_slope_q05", "binding_slope_q25",
               "binding_slope_median", "n_no_endpoint", "binding_lines",
               "max_abs_slope", "min_respondent_max_slope",
               "sensitivity", "sensitivity_max",
               "n_binding_below_floor", "frac_binding_below_floor",
               "n_endpoint_tied", "n_zero_line")
keep_cols <- intersect(keep_cols, names(tab))
## Release-status comparison against the previous run, if one is on disk.
prev_path <- file.path(out, "compensator_columns.csv")
if (file.exists(prev_path)) {
  pv <- read.csv(prev_path)
  key <- function(d) paste(d$action, d$group, d$spec, sep = "\r")
  m <- match(key(tab), key(pv))
  ok <- !is.na(m)
  changed <- sum(tab$release_kind[ok] != pv$release_kind[m[ok]])
  log("release status changed in", changed, "of", sum(ok),
      "matched cells vs the previous run")
}

tab_out <- sb_stamp_provenance(
  tab[, keep_cols], app = "gs2020", profile = "v21_corrected",
  fit = assembled, calibration = cal, seed = 20260827L,
  producer = "applications/gs2020/compensator_run.R",
  target_label = "nonlinear_compensator_column",
  sources = c("applications/R/compensator_columns.R",
              "applications/R/share_bounds.R"))
write.csv(tab_out, file.path(out, "compensator_columns.csv"),
          row.names = FALSE)
saveRDS(tab, file.path(out, "compensator_columns.rds"))
prov_write_manifest(out, list(
  artifact = "compensator_columns.csv",
  engine = "cc_compensator_columns (exact q=1 union-of-half-lines)",
  release_gate = "all binding slopes >= matched composite floor",
  floor_rule = floor_rule,
  floor_rule_detail = floor_rule_detail,
  inheritance = "same-side only; none_bounds = NULL (gs bounds are opposition-side)",
  status = "provisional; conditional on the maintained dispersion ceiling",
  commit = prov_git_commit(root)),
  name = "provenance_compensator.csv")

## Console summary: the Overall row per action x spec.
ov <- tab[tab$group == "Overall", c("action", "spec", "share",
                                    "released_value", "release_kind",
                                    "floored", "frac_gate_failures",
                                    "binding_slope_min", "floor")]
print(ov, digits = 3)
log("written:", file.path(out, "compensator_columns.csv"))
log("DONE")
