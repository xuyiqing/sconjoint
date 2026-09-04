#!/usr/bin/env Rscript
## Targeted q = 1 inference for Graham--Svolik.
##
## Families may be run separately and resume from fingerprinted checkpoints.
## All intervals remain diagnostic when the saved fit is diagnostic-only or
## lacks the paper's classed verification evidence.

options(stringsAsFactors = FALSE, warn = 1)

script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.")
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

parse_cli <- function(args) {
  out <- list(profile = "v21_corrected", family = "all", force = FALSE,
              fit_dir = "", prepared = "", output = "",
              multiplier_draws = "1999")
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- bits[[1L]]
    if (!key %in% names(out)) stop("Unknown argument --", key)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out$multiplier_draws <- as.integer(out$multiplier_draws)
  out
}

root <- normalizePath(file.path(dirname(script_path()), "..", "..", ".."),
                      mustWork = TRUE)
Sys.setenv(SCONJOINT_ROOT = root)
app_root <- file.path(root, "applications/gs2020")
options(sconjoint.project_root = root, sconjoint.gs_application_root = app_root)
source(file.path(app_root, "config/analysis_config.R"), local = FALSE)
cfg <- gs_analysis_config
cli <- parse_cli(commandArgs(trailingOnly = TRUE))

families <- if (identical(cli$family, "all")) {
  c("basis", "direction", "compensating", "heterogeneity")
} else strsplit(cli$family, ",", fixed = TRUE)[[1L]]
allowed <- c("basis", "direction", "compensating", "heterogeneity")
if (length(setdiff(families, allowed))) {
  stop("--family must be all or a comma-separated subset of: ",
       paste(allowed, collapse = ", "), ".")
}

fit_dir <- if (nzchar(cli$fit_dir)) cli$fit_dir else
  file.path(cfg$output_root, cli$profile)
prepared_path <- if (nzchar(cli$prepared)) cli$prepared else cfg$input$prepared
out_dir <- if (nzchar(cli$output)) cli$output else
  file.path(root, "runs/gs2020", cli$profile, "targeted_q1")
fit_path <- file.path(fit_dir, "fit_primary_assembled.rds")
for (path in c(fit_path, prepared_path)) {
  if (!file.exists(path)) stop("Required input not found: ", path, ".")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required.")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/targeted_q1_inference.R"), local = FALSE)

fit <- readRDS(fit_path)
prepared <- readRDS(prepared_path)
if (!inherits(fit, "scmix_nested_assembled") || fit$q != 1L) {
  stop("The targeted runner requires an assembled q = 1 fit.")
}
if (!identical(as.character(fit$attr_names), cfg$coefficients$order)) {
  stop("The saved fit does not use the frozen Graham--Svolik coefficient order.")
}
inf_cfg <- cfg$inference
inf_cfg$multiplier_draws <- cli$multiplier_draws
seed <- as.integer(cfg$optimizer$seed + 41000L)
runner_path <- script_path()
base_settings <- list(application = "gs2020", profile = cli$profile,
  q = 1L, seed = seed, multiplier_draws = inf_cfg$multiplier_draws,
  runner_md5 = unname(tools::md5sum(runner_path)))
fingerprint <- tq1_fingerprint(root, c(fit_path, prepared_path), base_settings)

basis_path <- file.path(out_dir, "basis_reconstruction.rds")
basis_obj <- tq1_checkpoint(basis_path, fingerprint, cli$force,
  tq1_rebuild_inference_basis(fit))
if (!identical(basis_obj$respondents, unique(as.character(fit$respondent_id)))) {
  stop("Rebuilt basis respondent order does not match the fit.")
}
## These dimensions reproduce the tangent construction used in the dated run.
if (!identical(basis_obj$basis_dimension, c(5L, 5L, 9L, 5L, 5L)) ||
    !identical(basis_obj$tangent_dimension,
               c(181L, 181L, 301L, 181L, 181L))) {
  stop("Unexpected GS fitted-sieve dimensions: basis ",
       paste(basis_obj$basis_dimension, collapse = ","), "; tangent ",
       paste(basis_obj$tangent_dimension, collapse = ","), ".")
}
if (identical(families, "basis")) {
  message("Basis reconstruction complete. No DML family requested.")
}

attr_names <- fit$attr_names
respondents <- basis_obj$respondents
unit <- function(nm, sign = 1) tq1_unit_contrast(attr_names, nm, sign)
party_coord <- "diff_respParty"
action_coords <- grep("^diff_dem_code_u_", attr_names, value = TRUE)
valence_coords <- grep("^diff_dem_code_v_", attr_names, value = TRUE)
short_action <- function(x) sub("^diff_dem_code_u_", "", x)
short_valence <- function(x) sub("^diff_dem_code_v_", "", x)

direction_contrasts <- c(
  list(coparty = unit(party_coord, 1)),
  stats::setNames(lapply(action_coords, unit, sign = -1),
                  paste0("oppose_", vapply(action_coords, short_action, ""))),
  stats::setNames(lapply(valence_coords, unit, sign = -1),
                  paste0("oppose_", vapply(valence_coords, short_valence, "")))
)
direction_variance <- t(vapply(direction_contrasts, function(d) {
  vapply(fit$A_folds, function(A) sum(crossprod(A, d)^2), numeric(1L))
}, numeric(fit$K)))
colnames(direction_variance) <- paste0("fold", seq_len(fit$K))
direction_gate <- apply(direction_variance, 1L, min) >= inf_cfg$variance_floor

direction_dml <- NULL
direction_summary <- NULL
direction_path <- file.path(out_dir, "direction_dml.rds")
if ("direction" %in% families) {
  if (!all(direction_gate)) {
    stop("A prespecified direction is below the variance floor: ",
         paste(names(direction_gate)[!direction_gate], collapse = ", "), ".")
  }
  direction_targets <- lapply(names(direction_contrasts), function(nm)
    scmix_inference_target(type = "sign", contrast = direction_contrasts[[nm]],
      variance_floor = inf_cfg$variance_floor, label = nm))
  names(direction_targets) <- paste0("direction_", names(direction_contrasts))
  direction_dml <- tq1_checkpoint(direction_path, fingerprint, cli$force,
    tq1_run_family(fit, basis_obj$mu_basis, direction_targets, inf_cfg, seed))
  direction_summary <- tq1_family_summary(
    direction_dml, "preference_direction", inf_cfg$level)
  direction_summary$direction <- sub("^direction_", "",
                                     direction_summary$target)
  direction_summary$reported_event <- ifelse(
    direction_summary$direction == "coparty", "prefers co-partisan",
    "opposes violation")
  direction_summary$variance_floor <- inf_cfg$variance_floor
  direction_summary$minimum_directional_variance <- apply(
    direction_variance[direction_summary$direction, , drop = FALSE], 1L, min)
  tq1_atomic_write_csv(direction_summary,
                       file.path(out_dir, "direction_summary.csv"))
}

meta <- prepared$respondent_meta
meta <- meta[match(respondents, as.character(meta$respondent_id)), , drop = FALSE]
if (anyNA(meta$ideo7)) stop("Missing ideology for a fitted respondent.")
groups <- list(liberal = meta$ideo7 <= 3, moderate = meta$ideo7 == 4,
               conservative = meta$ideo7 >= 5)
groups <- lapply(groups, function(g) stats::setNames(as.numeric(g), respondents))
if (any(vapply(groups, mean, numeric(1L)) < 0.10)) {
  stop("A prespecified ideology group is below the 0.10 probability margin.")
}

comp_dml <- NULL
comp_summary <- NULL
if ("compensating" %in% families) {
  comp_targets <- list()
  comp_specs <- list()
  for (u in action_coords) {
    action <- short_action(u)
    base <- scmix_inference_target(
      type = "compensating", penalty = unit(u), benefit = unit(party_coord),
      amount = 1, variance_floor = inf_cfg$variance_floor,
      label = paste0("compensating_", action))
    overall_stem <- paste0("compensating_", action, "_overall")
    comp_targets[[overall_stem]] <- base
    comp_specs[[overall_stem]] <- list(action = action, group = "Overall",
                                      transform = FALSE)
    for (g in names(groups)) {
      stem <- paste0("compensating_", action, "_", g)
      comp_targets[[stem]] <- tq1_subgroup_target(base, groups[[g]])
      comp_specs[[stem]] <- list(action = action, group = g, transform = TRUE)
    }
  }
  comp_dml <- tq1_checkpoint(file.path(out_dir, "compensating_dml.rds"),
    fingerprint, cli$force,
    tq1_run_family(fit, basis_obj$mu_basis, comp_targets, inf_cfg, seed + 1L))
  primitive_summary <- tq1_family_summary(
    comp_dml, "compensating_primitives", inf_cfg$level)
  tq1_atomic_write_csv(primitive_summary,
                       file.path(out_dir, "compensating_primitives.csv"))
  rows <- list()
  for (stem in names(comp_specs)) {
    spec <- comp_specs[[stem]]
    if (!spec$transform) {
      z <- primitive_summary[primitive_summary$target == stem, , drop = FALSE]
      z$action <- spec$action; z$group <- spec$group
      rows[[stem]] <- z
    } else {
      tr <- scmix_delta_transform(
        comp_dml, type = "subgroup_ratio",
        primitives = paste0(stem, c(":weighted_target",
                                    ":subgroup_probability")),
        denominator_margin = 0.10, level = inf_cfg$level)
      z <- tq1_transform_summary(tr, "compensating_share", stem,
                                 inf_cfg$level)
      z$plugin <- NA_real_; z$adjustment <- NA_real_
      z$action <- spec$action; z$group <- spec$group
      rows[[stem]] <- z
    }
  }
  keep_cols <- Reduce(intersect, lapply(rows, names))
  comp_summary <- do.call(rbind, lapply(rows, function(x)
    x[, keep_cols, drop = FALSE]))
  rownames(comp_summary) <- NULL
  tq1_atomic_write_csv(comp_summary,
                       file.path(out_dir, "compensating_summary.csv"))
}

heterogeneity_dml <- NULL
heterogeneity_summary <- NULL
if ("heterogeneity" %in% families) {
  heterogeneity_targets <- lapply(names(direction_contrasts), function(nm)
    scmix_inference_target(type = "heterogeneity_primitives",
                           contrast = direction_contrasts[[nm]], label = nm))
  names(heterogeneity_targets) <- paste0("heterogeneity_",
                                         names(direction_contrasts))
  heterogeneity_dml <- tq1_checkpoint(
    file.path(out_dir, "heterogeneity_dml.rds"), fingerprint, cli$force,
    tq1_run_family(fit, basis_obj$mu_basis, heterogeneity_targets,
                   inf_cfg, seed + 2L))
  primitive_summary <- tq1_family_summary(
    heterogeneity_dml, "heterogeneity_primitives", inf_cfg$level)
  tq1_atomic_write_csv(primitive_summary,
                       file.path(out_dir, "heterogeneity_primitives.csv"))
  rows <- lapply(names(direction_contrasts), function(nm) {
    stem <- paste0("heterogeneity_", nm)
    tr <- scmix_delta_transform(
      heterogeneity_dml, type = "directional_heterogeneity",
      primitives = paste0(stem, c(":mean", ":second_moment",
                                  ":residual_variance")),
      total_margin = 1e-4, level = inf_cfg$level)
    tq1_transform_summary(tr, "directional_heterogeneity", nm,
                          inf_cfg$level)
  })
  heterogeneity_summary <- do.call(rbind, rows)
  rownames(heterogeneity_summary) <- NULL
  tq1_atomic_write_csv(heterogeneity_summary,
                       file.path(out_dir, "heterogeneity_summary.csv"))
  if (is.null(direction_summary) && file.exists(
      file.path(out_dir, "direction_summary.csv"))) {
    direction_summary <- utils::read.csv(
      file.path(out_dir, "direction_summary.csv"), check.names = FALSE)
  }
  if (!is.null(direction_summary)) {
    hs <- reshape(heterogeneity_summary[, c("target", "component", "one_step")],
                  idvar = "target", timevar = "component", direction = "wide")
    pol <- merge(direction_summary[, c("direction", "plugin", "one_step",
      "diagnostic_se", "status", "formal_inference_available")],
      hs, by.x = "direction", by.y = "target", all = TRUE,
      suffixes = c("_share", "_heterogeneity"))
    tq1_atomic_write_csv(pol, file.path(out_dir, "polarization_summary.csv"))
  }
}

summaries <- Filter(Negate(is.null), list(direction_summary, comp_summary,
                                           heterogeneity_summary))
if (length(summaries)) {
  common <- Reduce(intersect, lapply(summaries, names))
  gate_summary <- do.call(rbind, lapply(summaries, function(x)
    x[, common, drop = FALSE]))
  tq1_atomic_write_csv(gate_summary, file.path(out_dir, "gate_summary.csv"))
}

manifest <- list(
  application = "gs2020", profile = cli$profile, q = 1L,
  completed_families = families, completed_at = format(Sys.time(), tz = "UTC",
                                                        usetz = TRUE),
  fingerprint = fingerprint, fit_status = list(
    diagnostic_only = fit$diagnostic_only,
    eligible_for_ordinary_inference = fit$eligible_for_ordinary_inference,
    computational_gate_pass = fit$computational_gate_pass),
  basis_checks = basis_obj[c("prediction_max_abs_deviation",
    "prediction_tolerance", "basis_dimension", "tangent_dimension")],
  reporting_rule = paste(
    "Diagnostic estimates and diagnostic standard errors are retained.",
    "Formal intervals are withheld unless every package and verification gate passes."),
  session = utils::sessionInfo()
)
tq1_atomic_save_rds(manifest, file.path(out_dir, "run_manifest.rds"))
message("GS targeted q=1 stage complete: ", out_dir)
