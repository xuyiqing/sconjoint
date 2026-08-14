# v4 numbers registry (E2): one authoritative CSV for the canon draft
# (E7) and later drafting. Reads the three v4_quantities_<app>.rds slot
# caches (+ the E3 validation output when present) and the frozen v3
# registry for the was -> now join. Idempotent; fails on duplicate keys.
source("dev/app-v4-common.R")

V3 <- utils::read.csv(file.path(REPL_DIR, "results", "numbers",
                                "numbers.csv"), stringsAsFactors = FALSE)
OUT_CSV <- file.path(OUT_DIR, "v4_numbers.csv")

rows <- list()
row <- function(key, value, se = NA_real_, ci_lower = NA_real_,
                ci_upper = NA_real_, ci_type = "none", display = NA,
                na_reason = "", source, note = "", v3_key = key) {
  if (is.na(display)) {
    display <- if (is.na(value)) "NA" else sprintf("%.3f", value)
  }
  rows[[length(rows) + 1L]] <<- data.frame(
    key = key, value = value, se = se, ci_lower = ci_lower,
    ci_upper = ci_upper, ci_type = ci_type, display = display,
    na_reason = na_reason, source = source, note = note, v3_key = v3_key,
    stringsAsFactors = FALSE)
  invisible()
}

## pull one labeled entry out of an scmix_quantity
qx <- function(q, label) {
  i <- match(label, names(q$estimate))
  stopifnot(!is.na(i))
  list(est = unname(q$estimate[i]), se = unname(q$se[i]),
       lo = unname(q$ci_lower[i]), hi = unname(q$ci_upper[i]))
}

## na_reason for a signshare label from its gate table
gate_reason <- function(q, label) {
  g <- q$extra$gates
  ## subgroup labels are "<group>: <contrast>"; strip the group
  lab <- sub("^.*: ", "", label)
  j <- match(lab, g$label)
  if (is.na(j) || g$reported[j]) return("")
  if (g$gate_floor[j]) "floored" else "design_gate_t"
}

sig_row <- function(key, q, label, source, note = "", v3_key = key) {
  v <- qx(q, label)
  row(key, v$est, v$se, v$lo, v$hi,
      ci_type = if (is.na(v$est)) "none" else "wald",
      na_reason = gate_reason(q, label), source = source, note = note,
      v3_key = v3_key)
}

## ===========================================================================
## gs2020
## ===========================================================================
gs <- readRDS(file.path(OUT_DIR, "v4_quantities_gs2020.rds"))
SRC_GS <- "mixedlogit_prototype/v4_quantities_gs2020.rds"
PARTY <- "diff_respParty"
UNDEM <- c("diff_dem_code_u_banProtest", "diff_dem_code_u_court",
           "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
           "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
           "diff_dem_code_u_limitVote")

th <- qx(gs$theta, PARTY)
row("gs.theta.party", th$est, th$se, th$lo, th$hi, ci_type = "wald",
    source = SRC_GS, note = "two-stage canon 0.722")
for (k in names(gs$theta$estimate)) {
  v <- qx(gs$theta, k)
  row(paste0("gs.theta.", k), v$est, v$se, v$lo, v$hi, ci_type = "wald",
      source = SRC_GS)
}

for (a in c("journalists", "court", "gerry10")) {
  m <- gs[[paste0("mrs_", a)]]
  key <- paste0("gs.mrs.population.", a)
  row(key, unname(m$estimate), unname(m$se),
      m$extra$fieller_lo, m$extra$fieller_hi, ci_type = "fieller",
      source = SRC_GS,
      note = sprintf("Fieller CI; delta-method [%0.3f, %0.3f]; t_den %.1f",
                     m$ci_lower, m$ci_upper, m$extra$t_den))
  row(paste0(key, ".ci_lower"), m$extra$fieller_lo, source = SRC_GS)
  row(paste0(key, ".ci_upper"), m$extra$fieller_hi, source = SRC_GS)
}

grp_map <- c(Liberal = "lib", Moderate = "mod", Conservative = "con")
act_map <- c(journalists = "journalists", limitVote = "limitVote",
             court = "court")
for (a in names(act_map)) {
  for (g in names(grp_map)) {
    lab <- paste0(g, ": compdiff_", a)
    key <- sprintf("gs.compdiff.coparty.%s.%s", act_map[[a]], grp_map[[g]])
    sig_row(key, gs$compdiff_by_tercile, lab, SRC_GS,
            note = "P(beta_action + beta_party >= 0), gated sign share")
  }
}

pi_undem <- gs$pi$estimate[UNDEM]
all_na <- all(is.na(pi_undem))
row("gs.frac_undem_positive.max",
    if (all_na) NA_real_ else max(pi_undem, na.rm = TRUE),
    na_reason = if (all_na) "floored" else "",
    source = SRC_GS,
    note = "max model-implied pi over the 7 undemocratic coordinates; v3 0.0611 was a MAP fraction")
for (k in names(gs$pi$estimate)) {
  v <- qx(gs$pi, k)
  row(paste0("gs.pi.", k), v$est, v$se, v$lo, v$hi,
      ci_type = if (is.na(v$est)) "none" else "wald",
      na_reason = if (is.na(v$est)) "floored" else "",
      source = SRC_GS)
}

for (j in seq_along(gs$contest_share$estimate)) {
  lab <- names(gs$contest_share$estimate)[j]
  sig_row(paste0("gs.", lab, ".share"), gs$contest_share, lab, SRC_GS,
          note = "P((e_party + e_u)'beta > 0): co-partisan takes action vs clean opponent")
}
raw <- gs$contest_V$extra$raw
for (j in seq_along(gs$contest_V$estimate)) {
  lab <- names(gs$contest_V$estimate)[j]
  v <- qx(gs$contest_V, lab)
  row(paste0("gs.", lab, ".V"), v$est, v$se, v$lo, v$hi, ci_type = "wald",
      source = SRC_GS)
  row(paste0("gs.", lab, ".raw_share"), raw$raw_share[j],
      na_reason = if (is.na(raw$raw_share[j])) "off_design" else "",
      display = if (is.na(raw$raw_share[j])) "off-design" else
        sprintf("%.3f", raw$raw_share[j]),
      source = SRC_GS,
      note = sprintf("n_match = %d tasks", raw$raw_n_tasks[j]))
}

imp <- gs$importance
for (k in names(imp$estimate)) {
  v <- qx(imp, k)
  row(paste0("gs.importance.", k), v$est, v$se, v$lo, v$hi,
      ci_type = "wald", source = SRC_GS,
      note = sprintf("between-Z %.4f + residual %.4f of numerator",
                     imp$extra$between_Z[k], imp$extra$residual[k]))
}

row("diag.gs2020.floor_ratio", gs$zero_floor$ratio, source = SRC_GS,
    note = "threshold 2; q = 1 fit; q = 2 sweep also 1.45", v3_key = "")
row("diag.gs2020.weak_directions", gs$design_check$weak_directions,
    source = SRC_GS, v3_key = "")
row("diag.gs2020.identified_coords", length(gs$design_check$identified),
    source = SRC_GS, note = paste(gs$design_check$identified,
                                  collapse = ", "), v3_key = "")

## ===========================================================================
## br2017
## ===========================================================================
br <- readRDS(file.path(OUT_DIR, "v4_quantities_br2017.rds"))
SRC_BR <- "mixedlogit_prototype/v4_quantities_br2017.rds"

v <- qx(br$theta, "revenue_score")
row("br.theta.revenue", v$est, v$se, v$lo, v$hi, ci_type = "wald",
    source = SRC_BR, note = "banked prototype value 0.1311")
for (k in names(br$theta$estimate)) {
  v <- qx(br$theta, k)
  row(paste0("br.theta.", k), v$est, v$se, v$lo, v$hi, ci_type = "wald",
      source = SRC_BR)
}
for (k in names(br$pi$estimate)) {
  v <- qx(br$pi, k)
  row(paste0("br.pi.", k), v$est, v$se, v$lo, v$hi,
      ci_type = if (is.na(v$est)) "none" else "wald",
      na_reason = if (is.na(v$est)) "floored" else "",
      source = SRC_BR,
      note = if (k == "revenue_score") "banked 0.68 [0.59, 0.77]; v3 MAP ~1.0" else "")
}

sig_row("br.frac.slope_positive", br$slope_share, "progressivity_slope",
        SRC_BR, note = "model-implied share with positive progressivity slope; v3 0.929 was a MAP fraction")
sig_row("br.frac.top_gt_bottom", br$tmb_share, "top_minus_bottom", SRC_BR,
        note = "P(beta_375P > beta_L10); v3 0.91 was a MAP fraction",
        v3_key = "br.frac.top_gt_bottom")
sig_row("br.frac.rep_slope_positive", br$slope_share_by_party,
        "Republican: progressivity_slope", SRC_BR,
        note = "v3 0.90 was a MAP fraction")

ps <- br$party_slopes$by_party
for (i in seq_len(nrow(ps))) {
  key <- paste0("br.slope.mean.",
                c(Democrat = "dem", Independent = "indep",
                  Republican = "rep")[ps$group[i]])
  row(key, ps$estimate[i], ps$se[i],
      ps$estimate[i] - 1.96 * ps$se[i], ps$estimate[i] + 1.96 * ps$se[i],
      ci_type = "wald", source = SRC_BR,
      note = "slope weights on the corrected theta signal; v3 Dem 0.0210 / Rep 0.0142")
}

imp <- br$importance
for (k in names(imp$estimate)) {
  v <- qx(imp, k)
  row(paste0("br.importance.", k), v$est, v$se, v$lo, v$hi,
      ci_type = "wald", source = SRC_BR,
      note = sprintf("levels weighting; between-Z %.4f + residual %.4f",
                     imp$extra$between_Z[k], imp$extra$residual[k]))
}
impp <- br$importance_by_party
for (k in names(impp$estimate)) {
  key <- paste0("br.importance.",
                gsub(": ", ".", tolower(sub("Democrat", "dem",
                     sub("Independent", "indep",
                         sub("Republican", "rep", k))))))
  v <- qx(impp, k)
  row(key, v$est, v$se, v$lo, v$hi, ci_type = "wald", source = SRC_BR,
      v3_key = "")
}

rawb <- br$plans_V$extra$raw
for (j in seq_along(br$plans_V$estimate)) {
  lab <- names(br$plans_V$estimate)[j]
  v <- qx(br$plans_V, lab)
  row(paste0("br.plan.V.", lab), v$est, v$se, v$lo, v$hi, ci_type = "wald",
      source = SRC_BR,
      note = sprintf("raw share %s (n_match %d)",
                     ifelse(is.na(rawb$raw_share[j]), "off-design",
                            sprintf("%.3f", rawb$raw_share[j])),
                     rawb$raw_n_tasks[j]))
  sig_row(paste0("br.plan.du_share.", lab), br$plans_du_share, lab, SRC_BR,
          note = "share with du > 0; v3 MAP fractions 0.89 (flat) / 0.83 (regressive)")
}

row("diag.br2017.floor_ratio", br$zero_floor$ratio, source = SRC_BR,
    note = "banked 3.4; threshold 2", v3_key = "")
row("diag.br2017.weak_directions", br$design_check$weak_directions,
    source = SRC_BR, v3_key = "")
dcl <- br$design_check$loadings
row("diag.br2017.t.rate_10_35",
    dcl$t[match("rate_10_35", dcl$coord)], source = SRC_BR,
    note = "banked 1.5 (the one weak coordinate)", v3_key = "")

## E3 validation rows (when the validation script has run)
val_path <- file.path(OUT_DIR, "v4_validation_br2017.rds")
if (file.exists(val_path)) {
  val <- readRDS(val_path)
  row("br.r.slope_vs_ideal", val$r_slope, source = SRC_BR,
      note = sprintf("posterior means, n = %d; v3 0.4302", val$n_both))
  row("br.r.gap_vs_ideal", val$r_gap, source = SRC_BR,
      note = sprintf("posterior means, n = %d; v3 0.4666", val$n_both))
  row("br.r.top_vs_ideal", val$r_top, source = SRC_BR,
      note = "posterior means, n = 2000; v3 0.42")
  for (p in names(val$r_top_party)) {
    row(paste0("br.r.top_vs_ideal.", tolower(p)), val$r_top_party[[p]],
        source = SRC_BR, v3_key = "")
  }
} else {
  say("validation rds not found; br.r.* rows deferred")
}

## ===========================================================================
## sw2022
## ===========================================================================
sw <- readRDS(file.path(OUT_DIR, "v4_quantities_sw2022.rds"))
SRC_SW <- "mixedlogit_prototype/v4_quantities_sw2022.rds"
lvp <- c(Democrat = "dem", Independent = "indep", Republican = "rep")

for (g in names(lvp)) {
  v <- qx(sw$theta_by_party, paste(g, "cand_genderMale", sep = ": "))
  row(paste0("sw.gender.betamean.", lvp[[g]]), v$est, v$se, v$lo, v$hi,
      ci_type = "wald", source = SRC_SW,
      note = "v3 DML -0.33 / -0.14 / +0.15")
}
for (k in names(sw$theta$estimate)) {
  v <- qx(sw$theta, k)
  row(paste0("sw.theta.", k), v$est, v$se, v$lo, v$hi, ci_type = "wald",
      source = SRC_SW)
}

## the v3 within-party majority shares, now gated
sig_row("sw.gender.prefer_male.rep", sw$gender_share_by_party,
        "Republican: prefer_male", SRC_SW,
        note = "v3 0.657/0.688 (two fits); v4 NA under the design gate")
for (g in c("Democrat", "Independent")) {
  lab <- paste(g, "prefer_male", sep = ": ")
  v <- qx(sw$gender_share_by_party, lab)
  ## prefer_female = 1 - prefer_male; NA propagates
  row(paste0("sw.gender.prefer_female.", lvp[[g]]),
      if (is.na(v$est)) NA_real_ else 1 - v$est,
      v$se,
      if (is.na(v$est)) NA_real_ else 1 - v$hi,
      if (is.na(v$est)) NA_real_ else 1 - v$lo,
      ci_type = if (is.na(v$est)) "none" else "wald",
      na_reason = gate_reason(sw$gender_share_by_party, lab),
      source = SRC_SW,
      note = "v3 0.665/0.68 (Dem, two fits); v4 NA under the design gate")
}

for (k in names(sw$agenda_share$estimate)) {
  sig_row(paste0("sw.agenda.share.", k), sw$agenda_share, k, SRC_SW,
          note = "agenda coordinates carry the only identified loadings (t 9.7 / 3.9)")
}

imp <- sw$importance
for (k in names(imp$estimate)) {
  v <- qx(imp, k)
  row(paste0("sw.importance.", k), v$est, v$se, v$lo, v$hi,
      ci_type = "wald", source = SRC_SW,
      note = sprintf("between-Z %.4f + residual %.4f; residual component gated by floor ratio %.2f < 2",
                     imp$extra$between_Z[k], imp$extra$residual[k],
                     sw$zero_floor$ratio))
}

for (k in names(sw$pi$estimate)) {
  v <- qx(sw$pi, k)
  row(paste0("sw.pi.", k), v$est, v$se, v$lo, v$hi,
      ci_type = if (is.na(v$est)) "none" else "wald",
      na_reason = if (is.na(v$est)) "floored" else "",
      source = SRC_SW)
}

row("diag.sw2022.floor_ratio", sw$zero_floor$ratio, source = SRC_SW,
    note = "banked 1.7; threshold 2 -> distributional tier unsupported",
    v3_key = "")
row("diag.sw2022.fold_spread_lo", min(sw$fold_spread), source = SRC_SW,
    note = "banked 1.02", v3_key = "")
row("diag.sw2022.fold_spread_hi", max(sw$fold_spread), source = SRC_SW,
    note = "banked 1.26", v3_key = "")
row("diag.sw2022.identified_coords", length(sw$design_check$identified),
    source = SRC_SW, note = paste(sw$design_check$identified,
                                  collapse = ", "), v3_key = "")

## ===========================================================================
## assemble, join v3, write
## ===========================================================================
reg <- do.call(rbind, rows)
stopifnot(!anyDuplicated(reg$key))
j <- match(reg$v3_key, V3$key)
reg$v3_value <- ifelse(is.na(j) | reg$v3_key == "", NA_real_, V3$value[j])
reg$v3_key[is.na(j)] <- ""
utils::write.csv(reg, OUT_CSV, row.names = FALSE)
say("wrote %d rows to %s", nrow(reg), OUT_CSV)
say("v3-joined rows: %d", sum(!is.na(reg$v3_value)))
say("NA rows by reason: %s",
    paste(names(table(reg$na_reason[reg$na_reason != ""])),
          table(reg$na_reason[reg$na_reason != ""]),
          collapse = ", ", sep = "="))
cat("\nDONE v4 numbers registry\n")
