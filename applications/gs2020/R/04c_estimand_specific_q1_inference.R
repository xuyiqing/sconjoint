#!/usr/bin/env Rscript
## Estimand-specific q = 1 inference for Graham--Svolik.
##
## Unlike 04b_targeted_q1_inference.R, this runner selects the Riesz ridge
## separately for every direction, every overall/subgroup compensating share,
## and every directional heterogeneity decomposition. The smallest joint
## primitive vector required by a smooth transformation stays together so its
## covariance is retained. Formal intervals remain fail-closed when either the
## saved fit or the target-specific numerical checks are ineligible.

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
config_path <- file.path(app_root, "config/analysis_config.R")

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
  file.path(root, "runs/gs2020", cli$profile, "targeted_q1_estimand")
checkpoint_dir <- file.path(out_dir, "checkpoints")
fit_path <- file.path(fit_dir, "fit_primary_assembled.rds")
for (path in c(fit_path, prepared_path)) {
  if (!file.exists(path)) stop("Required input not found: ", path, ".")
}
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required.")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/targeted_q1_inference.R"), local = FALSE)

fit <- readRDS(fit_path)
prepared <- readRDS(prepared_path)
if (!inherits(fit, "scmix_nested_assembled") || fit$q != 1L) {
  stop("The estimand-specific runner requires an assembled q = 1 fit.")
}
if (!identical(as.character(fit$attr_names), cfg$coefficients$order)) {
  stop("The saved fit does not use the frozen Graham--Svolik coefficient order.")
}
inf_cfg <- cfg$inference
inf_cfg$multiplier_draws <- cli$multiplier_draws
seed <- as.integer(cfg$optimizer$seed + 41000L)
runner_path <- script_path()
base_settings <- list(application = "gs2020", profile = cli$profile,
  q = 1L, granularity = "estimand-specific", seed = seed,
  inference = inf_cfg,
  runner_md5 = unname(tools::md5sum(runner_path)))
package_sources <- sort(list.files(file.path(root, "R"), pattern = "[.]R$",
                                   full.names = TRUE))
fingerprint <- tq1_fingerprint(
  root, c(fit_path, prepared_path, config_path, package_sources),
  base_settings)
checkpoint_fingerprint <- function(target) {
  out <- fingerprint
  out$settings$checkpoint_target <- target
  out
}

manifest_path <- file.path(out_dir, "run_manifest.rds")
previous_manifest <- NULL
if (file.exists(manifest_path) && !cli$force) {
  previous_manifest <- readRDS(manifest_path)
  if (!identical(previous_manifest$fingerprint, fingerprint)) {
    stop("Existing output manifest has a different code, input, or settings ",
         "fingerprint. Use a fresh output directory or --force=true.")
  }
}

basis_path <- file.path(out_dir, "basis_reconstruction.rds")
basis_obj <- tq1_checkpoint(basis_path, fingerprint, cli$force,
  tq1_rebuild_inference_basis(fit))
if (!identical(basis_obj$respondents, unique(as.character(fit$respondent_id)))) {
  stop("Rebuilt basis respondent order does not match the fit.")
}
if (!identical(basis_obj$basis_dimension, c(5L, 5L, 9L, 5L, 5L)) ||
    !identical(basis_obj$tangent_dimension,
               c(181L, 181L, 301L, 181L, 181L))) {
  stop("Unexpected GS fitted-sieve dimensions: basis ",
       paste(basis_obj$basis_dimension, collapse = ","), "; tangent ",
       paste(basis_obj$tangent_dimension, collapse = ","), ".")
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

meta <- prepared$respondent_meta
meta <- meta[match(respondents, as.character(meta$respondent_id)), , drop = FALSE]
if (anyNA(meta$ideo7)) stop("Missing ideology for a fitted respondent.")
groups <- list(liberal = meta$ideo7 <= 3, moderate = meta$ideo7 == 4,
               conservative = meta$ideo7 >= 5)
groups <- lapply(groups, function(g) stats::setNames(as.numeric(g), respondents))
group_probability <- vapply(groups, mean, numeric(1L))
if (any(group_probability < 0.10)) {
  stop("A prespecified ideology group is below the 0.10 probability margin.")
}

computed_dml <- function(x) {
  inherits(x, "scmix_dml") && !is.null(x$estimate) &&
    !is.null(x$diagnostic_covariance) &&
    !(length(x$estimate) == 1L && is.na(x$estimate))
}

primitive_diagnostics <- function(dml) {
  ps <- tq1_family_summary(dml, "primitive", inf_cfg$level)
  if (!all(c("riesz_max", "ridge_max", "information_min") %in% names(ps))) {
    return(list(riesz_max = NA_real_, ridge_max = NA_real_,
                information_min = NA_real_, fold = list()))
  }
  fold <- lapply(seq_len(fit$K), function(k) list(
    riesz = max(ps[[paste0("fold", k, "_riesz")]], na.rm = TRUE),
    ridge = max(ps[[paste0("fold", k, "_ridge")]], na.rm = TRUE),
    nu = unique(ps[[paste0("fold", k, "_nu")]])[[1L]],
    information_min = min(ps[[paste0("fold", k,
                                      "_information_min")]], na.rm = TRUE)))
  list(riesz_max = max(ps$riesz_max, na.rm = TRUE),
       ridge_max = max(ps$ridge_max, na.rm = TRUE),
       information_min = min(ps$information_min, na.rm = TRUE), fold = fold)
}

add_primitive_diagnostics <- function(z, dml) {
  dg <- primitive_diagnostics(dml)
  z$riesz_max <- dg$riesz_max
  z$ridge_max <- dg$ridge_max
  z$information_min <- dg$information_min
  for (k in seq_len(fit$K)) {
    if (length(dg$fold)) {
      z[[paste0("fold", k, "_riesz")]] <- dg$fold[[k]]$riesz
      z[[paste0("fold", k, "_ridge")]] <- dg$fold[[k]]$ridge
      z[[paste0("fold", k, "_nu")]] <- dg$fold[[k]]$nu
      z[[paste0("fold", k, "_information_min")]] <-
        dg$fold[[k]]$information_min
    }
  }
  z
}

withheld_transform <- function(dml, family, target, components) {
  data.frame(
    family = family, target = target, component = components,
    plugin = NA_real_, one_step = NA_real_, adjustment = NA_real_,
    diagnostic_se = NA_real_, diagnostic_lower = NA_real_,
    diagnostic_upper = NA_real_, formal_se = NA_real_,
    formal_lower = NA_real_, formal_upper = NA_real_,
    status = if (is.null(dml$status)) "withheld" else dml$status,
    formal_inference_available = FALSE, quantity_gate_pass = FALSE,
    withholding_reason = if (is.null(dml$reason))
      "primitive estimates unavailable" else dml$reason,
    stringsAsFactors = FALSE)
}

transform_bundle <- function(dml, type, primitives, family, target,
                             denominator_margin = NULL,
                             total_margin = NULL) {
  components <- if (identical(type, "subgroup_ratio")) "subgroup_ratio" else
    c("H_Z", "H_R", "H_T", "share_Z")
  if (!computed_dml(dml)) {
    return(add_primitive_diagnostics(
      withheld_transform(dml, family, target, components), dml))
  }
  tr <- scmix_delta_transform(
    dml, type = type, primitives = primitives,
    denominator_margin = denominator_margin, total_margin = total_margin,
    level = inf_cfg$level)
  z <- tq1_transform_summary(tr, family, target, inf_cfg$level)
  plugin <- tq1_plugin_transform(dml, type = type, primitives = primitives)
  z$plugin <- as.numeric(plugin[z$component])
  z$adjustment <- z$one_step - z$plugin
  add_primitive_diagnostics(z, dml)
}

direction_summary <- comp_summary <- heterogeneity_summary <- NULL
primitive_rows <- list()

if ("direction" %in% families) {
  if (!all(direction_gate)) {
    stop("A prespecified direction is below the variance floor: ",
         paste(names(direction_gate)[!direction_gate], collapse = ", "), ".")
  }
  rows <- vector("list", length(direction_contrasts))
  for (j in seq_along(direction_contrasts)) {
    nm <- names(direction_contrasts)[[j]]
    message("direction ", j, "/", length(direction_contrasts), ": ", nm)
    target_name <- paste0("direction_", nm)
    targets <- stats::setNames(list(scmix_inference_target(
      type = "sign", contrast = direction_contrasts[[nm]],
      variance_floor = inf_cfg$variance_floor, label = nm)), target_name)
    dml <- tq1_checkpoint(file.path(checkpoint_dir,
      paste0(target_name, ".rds")), checkpoint_fingerprint(target_name),
      cli$force,
      tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed))
    z <- tq1_family_summary(dml, "preference_direction", inf_cfg$level)
    z$direction <- nm
    z$reported_event <- if (nm == "coparty") "prefers co-partisan" else
      "opposes violation"
    z$variance_floor <- inf_cfg$variance_floor
    z$minimum_directional_variance <- min(direction_variance[nm, ])
    z$one_step_in_parameter_space <- is.finite(z$one_step) &
      z$one_step >= 0 & z$one_step <= 1
    rows[[j]] <- z
  }
  direction_summary <- tq1_bind_rows(rows)
  tq1_atomic_write_csv(direction_summary,
                       file.path(out_dir, "direction_summary.csv"))
}

if ("compensating" %in% families) {
  rows <- list()
  p_rows <- list()
  counter <- 0L
  for (a in seq_along(action_coords)) {
    u <- action_coords[[a]]
    action <- short_action(u)
    base <- scmix_inference_target(
      type = "compensating", penalty = unit(u), benefit = unit(party_coord),
      amount = 1, variance_floor = inf_cfg$variance_floor,
      label = paste0("compensating_", action))
    for (group in c("Overall", names(groups))) {
      counter <- counter + 1L
      stem <- paste0("compensating_", action, "_", tolower(group))
      message("compensating ", counter, "/",
              length(action_coords) * (length(groups) + 1L), ": ", stem)
      target <- if (group == "Overall") base else
        tq1_subgroup_target(base, groups[[group]])
      targets <- stats::setNames(list(target), stem)
      dml <- tq1_checkpoint(file.path(checkpoint_dir, paste0(stem, ".rds")),
        checkpoint_fingerprint(stem), cli$force,
        tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg,
                       seed + 1L))
      ps <- tq1_family_summary(dml, "compensating_primitives", inf_cfg$level)
      ps$action <- action; ps$group <- group; ps$estimand <- stem
      p_rows[[stem]] <- ps
      if (group == "Overall") {
        z <- ps
        z$family <- "compensating_share"
        z$component <- "overall"
        z$quantity_gate_pass <- TRUE
      } else {
        primitives <- paste0(stem, c(":weighted_target",
                                      ":subgroup_probability"))
        z <- transform_bundle(
          dml, "subgroup_ratio", primitives, "compensating_share", stem,
          denominator_margin = 0.10)
      }
      z$action <- action; z$group <- group
      z$group_probability <- if (group == "Overall") 1 else
        group_probability[[group]]
      z$one_step_in_parameter_space <- is.finite(z$one_step) &
        z$one_step >= 0 & z$one_step <= 1
      rows[[stem]] <- z
    }
  }
  comp_summary <- tq1_bind_rows(rows)
  comp_primitives <- tq1_bind_rows(p_rows)
  tq1_atomic_write_csv(comp_primitives,
                       file.path(out_dir, "compensating_primitives.csv"))
  tq1_atomic_write_csv(comp_summary,
                       file.path(out_dir, "compensating_summary.csv"))
}

if ("heterogeneity" %in% families) {
  rows <- vector("list", length(direction_contrasts))
  p_rows <- vector("list", length(direction_contrasts))
  for (j in seq_along(direction_contrasts)) {
    nm <- names(direction_contrasts)[[j]]
    stem <- paste0("heterogeneity_", nm)
    message("heterogeneity ", j, "/", length(direction_contrasts), ": ", nm)
    targets <- stats::setNames(list(scmix_inference_target(
      type = "heterogeneity_primitives", contrast = direction_contrasts[[nm]],
      label = nm)), stem)
    dml <- tq1_checkpoint(file.path(checkpoint_dir, paste0(stem, ".rds")),
      checkpoint_fingerprint(stem), cli$force,
      tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg,
                     seed + 2L))
    ps <- tq1_family_summary(dml, "heterogeneity_primitives", inf_cfg$level)
    ps$direction <- nm
    p_rows[[j]] <- ps
    primitives <- paste0(stem, c(":mean", ":second_moment",
                                  ":residual_variance"))
    z <- transform_bundle(dml, "directional_heterogeneity", primitives,
                          "directional_heterogeneity", nm,
                          total_margin = 1e-4)
    if (all(c("H_Z", "H_R", "H_T", "share_Z") %in% z$component)) {
      one <- stats::setNames(z$one_step, z$component)
      plugin <- stats::setNames(z$plugin, z$component)
      z$one_step_in_parameter_space <- all(is.finite(one)) &&
        one[["H_Z"]] >= 0 && one[["H_R"]] >= 0 && one[["H_T"]] > 0 &&
        one[["share_Z"]] >= 0 && one[["share_Z"]] <= 1
      z$plugin_in_parameter_space <- all(is.finite(plugin)) &&
        plugin[["H_Z"]] >= 0 && plugin[["H_R"]] >= 0 &&
        plugin[["H_T"]] > 0 && plugin[["share_Z"]] >= 0 &&
        plugin[["share_Z"]] <= 1
    } else {
      z$one_step_in_parameter_space <- FALSE
      z$plugin_in_parameter_space <- FALSE
    }
    rows[[j]] <- z
  }
  heterogeneity_summary <- tq1_bind_rows(rows)
  heterogeneity_primitives <- tq1_bind_rows(p_rows)
  tq1_atomic_write_csv(heterogeneity_primitives,
                       file.path(out_dir, "heterogeneity_primitives.csv"))
  tq1_atomic_write_csv(heterogeneity_summary,
                       file.path(out_dir, "heterogeneity_summary.csv"))
}

load_prior_summary <- function(current, family, filename) {
  if (!is.null(current)) return(current)
  path <- file.path(out_dir, filename)
  if (!is.null(previous_manifest) &&
      family %in% previous_manifest$completed_families && file.exists(path)) {
    return(utils::read.csv(path, check.names = FALSE))
  }
  NULL
}
direction_all <- load_prior_summary(
  direction_summary, "direction", "direction_summary.csv")
comp_all <- load_prior_summary(
  comp_summary, "compensating", "compensating_summary.csv")
heterogeneity_all <- load_prior_summary(
  heterogeneity_summary, "heterogeneity", "heterogeneity_summary.csv")

if (!is.null(heterogeneity_all)) {
  pol <- heterogeneity_all[
    heterogeneity_all$component == "share_Z",
    c("target", "plugin", "one_step", "diagnostic_se", "status",
      "formal_inference_available", "riesz_max", "ridge_max",
      "information_min", "one_step_in_parameter_space",
      "plugin_in_parameter_space"), drop = FALSE]
  names(pol)[names(pol) == "target"] <- "direction"
  if (!is.null(direction_all)) {
    pol <- merge(direction_all[, c("direction", "plugin", "one_step",
      "diagnostic_se", "status", "formal_inference_available")],
      pol, by = "direction", all = TRUE,
      suffixes = c("_direction", "_heterogeneity"))
  }
  tq1_atomic_write_csv(pol, file.path(out_dir, "polarization_summary.csv"))
}

summaries <- Filter(Negate(is.null),
                    list(direction_all, comp_all, heterogeneity_all))
if (length(summaries)) {
  gate_summary <- tq1_bind_rows(summaries)
  gate_summary$interval_status <- ifelse(
    gate_summary$formal_inference_available,
    "formal_interval_available", "diagnostic_range_only_not_a_confidence_interval")
  tq1_atomic_write_csv(gate_summary, file.path(out_dir, "gate_summary.csv"))
}

completed_families <- unique(c(
  if (is.null(previous_manifest)) character() else
    previous_manifest$completed_families,
  families))
manifest <- list(
  application = "gs2020", profile = cli$profile, q = 1L,
  granularity = paste(
    "one Riesz ridge per direction, compensating share, or directional",
    "heterogeneity primitive bundle"),
  completed_families = completed_families,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  fingerprint = fingerprint,
  fit_status = list(
    diagnostic_only = fit$diagnostic_only,
    eligible_for_ordinary_inference = fit$eligible_for_ordinary_inference,
    computational_gate_pass = fit$computational_gate_pass),
  basis_checks = basis_obj[c("prediction_max_abs_deviation",
    "prediction_tolerance", "basis_dimension", "tangent_dimension")],
  target_checks = list(
    variance_floor = inf_cfg$variance_floor,
    minimum_directional_variance = apply(direction_variance, 1L, min),
    group_probability = group_probability,
    denominator_margin = 0.10, total_heterogeneity_margin = 1e-4),
  reporting_rule = paste(
    "Cross-fitted plug-in and one-step diagnostic estimates are retained.",
    "A diagnostic range is not a confidence interval. Formal intervals are",
    "withheld unless every fit, numerical, and verification gate passes."),
  exploratory_ridge_check = paste(
    "The confirmatory run uses the package default rate grid.",
    "Any denser ridge-grid calculation must be labeled exploratory."),
  session = utils::sessionInfo())
tq1_atomic_save_rds(manifest, manifest_path)
message("GS estimand-specific q=1 stage complete: ", out_dir)
