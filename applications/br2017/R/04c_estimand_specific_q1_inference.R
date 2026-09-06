#!/usr/bin/env Rscript
## Estimand-specific q = 1 inference for Ballard-Rosa, Martin, and Scheve.
##
## This runner selects the Riesz ridge separately for every progressivity
## direction, every directional heterogeneity decomposition, and every overall
## or subgroup choice probability. The smallest joint primitive vector needed
## by a smooth transformation remains together. Formal intervals remain
## fail-closed when either the saved fit or target-specific gates are
## ineligible.

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
app_root <- file.path(root, "applications/br2017")
options(sconjoint.project_root = root, sconjoint.br_application_root = app_root)
source(file.path(app_root, "config/analysis_config.R"), local = FALSE)
cfg <- br_analysis_config
cli <- parse_cli(commandArgs(trailingOnly = TRUE))
config_path <- file.path(app_root, "config/analysis_config.R")

families <- if (identical(cli$family, "all")) {
  c("basis", "direction", "heterogeneity", "choice")
} else strsplit(cli$family, ",", fixed = TRUE)[[1L]]
allowed <- c("basis", "direction", "heterogeneity", "choice")
if (length(setdiff(families, allowed))) {
  stop("--family must be all or a comma-separated subset of: ",
       paste(allowed, collapse = ", "), ".")
}

fit_dir <- if (nzchar(cli$fit_dir)) cli$fit_dir else
  file.path(cfg$output_root, cli$profile)
prepared_path <- if (nzchar(cli$prepared)) cli$prepared else cfg$input$prepared
out_dir <- if (nzchar(cli$output)) cli$output else
  file.path(root, "runs/br2017", cli$profile, "targeted_q1_estimand")
checkpoint_dir <- file.path(out_dir, "checkpoints")
fit_path <- file.path(fit_dir, "fit_primary_assembled.rds")
nested_path <- file.path(fit_dir, "fit_primary_nested.rds")
for (path in c(fit_path, nested_path, prepared_path)) {
  if (!file.exists(path)) stop("Required input not found: ", path, ".")
}
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required.")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
target_helper <- file.path(root, "applications/R/targeted_q1_inference.R")
contrast_helper <- file.path(root, "applications/R/br_progressivity_contrasts.R")
source(target_helper, local = FALSE)
source(contrast_helper, local = FALSE)

fit <- readRDS(fit_path)
nested <- readRDS(nested_path)
prepared <- readRDS(prepared_path)
if (!inherits(fit, "scmix_nested_assembled") || fit$q != 1L) {
  stop("The estimand-specific runner requires an assembled q = 1 fit.")
}
if (!identical(as.character(fit$attr_names), cfg$coefficients$order)) {
  stop("The saved fit does not use the frozen Ballard-Rosa coefficient order.")
}
if (fit$K != length(nested$tuning)) {
  stop("The assembled and nested fits contain different fold counts.")
}
loading_deviation <- vapply(seq_len(fit$K), function(k) {
  max(abs(as.matrix(fit$A_folds[[k]]) -
            as.matrix(nested$tuning[[k]]$refit$A)))
}, numeric(1L))
if (max(loading_deviation) > 1e-12) {
  stop("Assembled and selected-refit raw loadings disagree. Maximum deviation: ",
       signif(max(loading_deviation), 6), ".")
}

inf_cfg <- cfg$inference
inf_cfg$multiplier_draws <- cli$multiplier_draws
seed <- as.integer(cfg$optimizer$seed + 42000L)
runner_path <- script_path()
base_settings <- list(
  application = "br2017", profile = cli$profile, q = 1L,
  granularity = "estimand-specific", seed = seed, inference = inf_cfg,
  runner_md5 = unname(tools::md5sum(runner_path)))
package_sources <- sort(list.files(file.path(root, "R"), pattern = "[.]R$",
                                   full.names = TRUE))
fingerprint <- tq1_fingerprint(
  root,
  c(fit_path, nested_path, prepared_path, config_path, target_helper,
    contrast_helper, package_sources),
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
basis_obj <- tq1_checkpoint(
  basis_path, fingerprint, cli$force, tq1_rebuild_inference_basis(fit))
if (!identical(basis_obj$respondents,
               unique(as.character(fit$respondent_id)))) {
  stop("Rebuilt basis respondent order does not match the fit.")
}

attr_names <- fit$attr_names
respondents <- basis_obj$respondents
dense <- function(x) {
  out <- stats::setNames(numeric(length(attr_names)), attr_names)
  out[names(x)] <- x
  out
}
progressivity_rank <- br_progressivity_contrasts(
  attr_names, include = c("top_minus_bottom", "slope", "mean_rate"))
progressivity_rank <- lapply(progressivity_rank, dense)
progressivity_log <- br_progressivity_contrasts(
  attr_names, spacing = "log_midpoint", include = "slope")
progressivity_log <- lapply(progressivity_log, dense)
names(progressivity_log) <- "slope_log_midpoint"
progressivity <- c(progressivity_rank, progressivity_log)
direction_contrasts_raw <- progressivity[
  c("top_minus_bottom", "slope", "slope_log_midpoint")]
direction_normalized <- lapply(direction_contrasts_raw,
                               tq1_normalize_contrast)
direction_contrasts <- lapply(direction_normalized, `[[`, "analysis")
direction_raw_scale <- vapply(direction_normalized, `[[`, numeric(1L),
                              "raw_scale")
direction_variance <- t(vapply(direction_contrasts, function(d) {
  vapply(fit$A_folds, function(A) sum(crossprod(A, d)^2), numeric(1L))
}, numeric(fit$K)))
colnames(direction_variance) <- paste0("fold", seq_len(fit$K))
direction_gate <- apply(direction_variance, 1L, min) >=
  inf_cfg$variance_floor

meta <- prepared$respondent_meta
meta <- meta[match(respondents, as.character(meta$respondent_id)), , drop = FALSE]
pid_name <- intersect(c("pid7", "resp_pid7"), names(meta))
if (length(pid_name) != 1L || anyNA(meta[[pid_name]])) {
  stop("Could not recover the seven-point party identifier.")
}
pid <- as.numeric(meta[[pid_name]])
groups <- list(democrat = pid %in% 1:3, independent = pid == 4,
               republican = pid %in% 5:7)
groups <- lapply(groups, function(g)
  stats::setNames(as.numeric(g), respondents))
group_probability <- vapply(groups, mean, numeric(1L))
if (any(group_probability < 0.05)) {
  stop("A prespecified party group is below the 0.05 probability margin.")
}

computed_dml <- function(x) {
  inherits(x, "scmix_dml") && !is.null(x$estimate) &&
    !is.null(x$diagnostic_covariance) &&
    !(length(x$estimate) == 1L && is.na(x$estimate))
}

primitive_diagnostics <- function(dml) {
  ps <- tq1_family_summary(dml, "primitive", inf_cfg$level)
  needed <- c("riesz_max", "ridge_max", "information_min")
  if (!all(needed %in% names(ps))) {
    return(list(riesz_max = NA_real_, ridge_max = NA_real_,
                information_min = NA_real_, fold = list()))
  }
  fold <- lapply(seq_len(fit$K), function(k) list(
    riesz = max(ps[[paste0("fold", k, "_riesz")]], na.rm = TRUE),
    ridge = max(ps[[paste0("fold", k, "_ridge")]], na.rm = TRUE),
    nu = unique(ps[[paste0("fold", k, "_nu")]])[[1L]],
    information_min = min(
      ps[[paste0("fold", k, "_information_min")]], na.rm = TRUE)))
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

add_gate_flags <- function(z) {
  z$riesz_gate_pass <- is.finite(z$riesz_max) &
    z$riesz_max <= inf_cfg$riesz_equation_tolerance
  z$ridge_gate_pass <- is.finite(z$ridge_max) &
    z$ridge_max <= inf_cfg$ridge_sensitivity_tolerance
  z$target_numerical_gate_pass <- z$riesz_gate_pass & z$ridge_gate_pass
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
    return(add_gate_flags(add_primitive_diagnostics(
      withheld_transform(dml, family, target, components), dml)))
  }
  tr <- scmix_delta_transform(
    dml, type = type, primitives = primitives,
    denominator_margin = denominator_margin, total_margin = total_margin,
    level = inf_cfg$level)
  z <- tq1_transform_summary(tr, family, target, inf_cfg$level)
  plugin <- tq1_plugin_transform(dml, type = type, primitives = primitives)
  z$plugin <- as.numeric(plugin[z$component])
  z$adjustment <- z$one_step - z$plugin
  add_gate_flags(add_primitive_diagnostics(z, dml))
}

direction_summary <- heterogeneity_summary <- choice_summary <- NULL

if ("direction" %in% families) {
  if (!all(direction_gate)) {
    stop("A prespecified direction is below the variance floor: ",
         paste(names(direction_gate)[!direction_gate], collapse = ", "), ".")
  }
  rows <- list()
  primitive_rows <- list()
  counter <- 0L
  for (j in seq_along(direction_contrasts)) {
    nm <- names(direction_contrasts)[[j]]
    base <- scmix_inference_target(
      type = "sign", contrast = direction_contrasts[[nm]],
      variance_floor = inf_cfg$variance_floor, label = nm)
    for (group in c("Overall", names(groups))) {
      counter <- counter + 1L
      stem <- paste0("direction_", nm, "_", tolower(group))
      message("direction ", counter, "/",
              length(direction_contrasts) * (length(groups) + 1L), ": ",
              stem)
      target <- if (group == "Overall") base else
        tq1_subgroup_target(base, groups[[group]])
      targets <- stats::setNames(list(target), stem)
      dml <- tq1_checkpoint(
        file.path(checkpoint_dir, paste0(stem, ".rds")),
        checkpoint_fingerprint(stem), cli$force,
        tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed))
      ps <- add_gate_flags(tq1_family_summary(
        dml, "preference_direction_primitives", inf_cfg$level))
      ps$direction <- nm
      ps$group <- group
      ps$estimand <- stem
      primitive_rows[[stem]] <- ps
      if (group == "Overall") {
        z <- ps
        z$family <- "preference_direction"
        z$component <- "overall"
        z$quantity_gate_pass <- TRUE
      } else {
        primitives <- paste0(stem, c(":weighted_target",
                                      ":subgroup_probability"))
        z <- transform_bundle(
          dml, "subgroup_ratio", primitives, "preference_direction", stem,
          denominator_margin = 0.05)
      }
      z$direction <- nm
      z$group <- group
      z$reported_event <- "prefers greater tax progressivity"
      z$group_probability <- if (group == "Overall") 1 else
        group_probability[[group]]
      z$contrast_max_abs_scale <- direction_raw_scale[[nm]]
      z$variance_floor <- inf_cfg$variance_floor
      z$minimum_directional_variance <- min(direction_variance[nm, ])
      z$one_step_in_parameter_space <- is.finite(z$one_step) &
        z$one_step >= 0 & z$one_step <= 1
      rows[[stem]] <- z
    }
  }
  direction_summary <- tq1_bind_rows(rows)
  direction_primitives <- tq1_bind_rows(primitive_rows)
  tq1_atomic_write_csv(direction_primitives,
                       file.path(out_dir, "direction_primitives.csv"))
  tq1_atomic_write_csv(direction_summary,
                       file.path(out_dir, "direction_summary.csv"))
}

if ("heterogeneity" %in% families) {
  unit_directions <- stats::setNames(lapply(attr_names, function(nm)
    tq1_unit_contrast(attr_names, nm)), attr_names)
  directions_raw <- c(unit_directions, progressivity)
  direction_normalized_all <- lapply(directions_raw,
                                     tq1_normalize_contrast)
  directions <- lapply(direction_normalized_all, `[[`, "analysis")
  direction_scale_all <- vapply(direction_normalized_all, `[[`, numeric(1L),
                                "raw_scale")
  rows <- vector("list", length(directions_raw))
  primitive_rows <- vector("list", length(directions_raw))
  for (j in seq_along(directions_raw)) {
    nm <- names(directions_raw)[[j]]
    stem <- paste0("heterogeneity_", nm)
    message("heterogeneity ", j, "/", length(directions), ": ", nm)
    targets <- stats::setNames(list(scmix_inference_target(
      type = "heterogeneity_primitives", contrast = directions[[nm]],
      label = nm)), stem)
    dml <- tq1_checkpoint(
      file.path(checkpoint_dir, paste0(stem, ".rds")),
      checkpoint_fingerprint(stem), cli$force,
      tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed + 1L))
    ps <- add_gate_flags(tq1_family_summary(
      dml, "heterogeneity_primitives", inf_cfg$level))
    raw_scale <- direction_scale_all[[nm]]
    primitive_scale <- ifelse(grepl(":mean$", ps$target), raw_scale,
                              raw_scale^2)
    ps <- tq1_rescale_summary_rows(ps, primitive_scale)
    ps$primitive_units <- "original_contrast_units"
    ps$contrast_max_abs_scale <- raw_scale
    ps$direction <- nm
    primitive_rows[[j]] <- ps
    primitives <- paste0(stem, c(":mean", ":second_moment",
                                  ":residual_variance"))
    z <- transform_bundle(dml, "directional_heterogeneity", primitives,
                          "directional_heterogeneity", nm,
                          total_margin = 1e-4)
    raw_variance_scale <- direction_scale_all[[nm]]^2
    variance_rows <- z$component %in% c("H_Z", "H_R", "H_T")
    z <- tq1_rescale_summary_rows(
      z, ifelse(variance_rows, raw_variance_scale, 1))
    z$contrast_max_abs_scale <- direction_scale_all[[nm]]
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
  heterogeneity_primitives <- tq1_bind_rows(primitive_rows)
  tq1_atomic_write_csv(heterogeneity_primitives,
                       file.path(out_dir, "heterogeneity_primitives.csv"))
  tq1_atomic_write_csv(heterogeneity_summary,
                       file.path(out_dir, "heterogeneity_summary.csv"))
}

if ("choice" %in% families) {
  plans <- list(
    steeply_progressive = c(0, 5, 15, 25, 35, 45, 2),
    flat_15 = c(15, 15, 15, 15, 15, 15, 0),
    status_quo = c(5, 15, 25, 25, 35, 35, 0),
    regressive = c(25, 25, 15, 15, 5, 5, -2))
  contrasts <- lapply(plans[c("flat_15", "status_quo", "regressive")],
    function(opp) stats::setNames(
      plans$steeply_progressive - opp, attr_names))
  rows <- list()
  primitive_rows <- list()
  counter <- 0L
  for (nm in names(contrasts)) {
    base <- scmix_inference_target(
      type = "choice", contrast = contrasts[[nm]], position_neutral = TRUE,
      n_nodes = inf_cfg$choice_nodes, label = paste0("choice_", nm))
    for (group in c("Overall", names(groups))) {
      counter <- counter + 1L
      stem <- paste0("choice_", nm, "_", tolower(group))
      message("choice ", counter, "/",
              length(contrasts) * (length(groups) + 1L), ": ", stem)
      target <- if (group == "Overall") base else
        tq1_subgroup_target(base, groups[[group]])
      targets <- stats::setNames(list(target), stem)
      dml <- tq1_checkpoint(
        file.path(checkpoint_dir, paste0(stem, ".rds")),
        checkpoint_fingerprint(stem), cli$force,
        tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed + 2L))
      ps <- add_gate_flags(tq1_family_summary(
        dml, "choice_primitives", inf_cfg$level))
      ps$opponent <- nm
      ps$group <- group
      ps$estimand <- stem
      primitive_rows[[stem]] <- ps
      if (group == "Overall") {
        z <- ps
        z$family <- "position_neutral_choice"
        z$component <- "overall"
        z$quantity_gate_pass <- TRUE
      } else {
        primitives <- paste0(stem, c(":weighted_target",
                                      ":subgroup_probability"))
        z <- transform_bundle(
          dml, "subgroup_ratio", primitives, "position_neutral_choice", stem,
          denominator_margin = 0.05)
      }
      z$opponent <- nm
      z$group <- group
      z$group_probability <- if (group == "Overall") 1 else
        group_probability[[group]]
      z$one_step_in_parameter_space <- is.finite(z$one_step) &
        z$one_step >= 0 & z$one_step <= 1
      rows[[stem]] <- z
    }
  }
  choice_summary <- tq1_bind_rows(rows)
  choice_primitives <- tq1_bind_rows(primitive_rows)
  tq1_atomic_write_csv(choice_primitives,
                       file.path(out_dir, "choice_primitives.csv"))
  tq1_atomic_write_csv(choice_summary,
                       file.path(out_dir, "choice_summary.csv"))
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
heterogeneity_all <- load_prior_summary(
  heterogeneity_summary, "heterogeneity", "heterogeneity_summary.csv")
choice_all <- load_prior_summary(choice_summary, "choice", "choice_summary.csv")

if (!is.null(heterogeneity_all)) {
  pol <- heterogeneity_all[
    heterogeneity_all$component == "share_Z",
    c("target", "plugin", "one_step", "diagnostic_se", "status",
      "formal_inference_available", "riesz_max", "ridge_max",
      "information_min", "target_numerical_gate_pass",
      "one_step_in_parameter_space", "plugin_in_parameter_space"),
    drop = FALSE]
  names(pol)[names(pol) == "target"] <- "direction"
  if (!is.null(direction_all)) {
    direction_overall <- direction_all[
      direction_all$group == "Overall", , drop = FALSE]
    pol <- merge(
      direction_overall[, c("direction", "plugin", "one_step", "diagnostic_se",
        "status", "formal_inference_available", "riesz_max", "ridge_max",
        "target_numerical_gate_pass")],
      pol, by = "direction", all = TRUE,
      suffixes = c("_direction", "_heterogeneity"))
  }
  tq1_atomic_write_csv(pol, file.path(out_dir, "polarization_summary.csv"))
}

summaries <- Filter(Negate(is.null),
                    list(direction_all, heterogeneity_all, choice_all))
if (length(summaries)) {
  gate_summary <- tq1_bind_rows(summaries)
  gate_summary$interval_status <- ifelse(
    gate_summary$formal_inference_available,
    "formal_interval_available",
    "diagnostic_range_only_not_a_confidence_interval")
  tq1_atomic_write_csv(gate_summary, file.path(out_dir, "gate_summary.csv"))
}

completed_families <- unique(c(
  if (is.null(previous_manifest)) character() else
    previous_manifest$completed_families,
  families))
manifest <- list(
  application = "br2017", profile = cli$profile, q = 1L,
  granularity = paste(
    "one Riesz ridge per progressivity direction, directional heterogeneity",
    "primitive bundle, or overall/subgroup choice probability"),
  completed_families = completed_families,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  fingerprint = fingerprint,
  fit_status = list(
    diagnostic_only = fit$diagnostic_only,
    eligible_for_ordinary_inference = fit$eligible_for_ordinary_inference,
    computational_gate_pass = fit$computational_gate_pass),
  loading_scale_check = list(
    contract = "assembled A_folds equal nested selected-refit raw loadings",
    fold_max_abs_deviation = loading_deviation,
    tolerance = 1e-12, pass = max(loading_deviation) <= 1e-12),
  basis_checks = basis_obj[c("prediction_max_abs_deviation",
    "prediction_tolerance", "basis_dimension", "tangent_dimension")],
  target_checks = list(
    contrast_normalization = paste(
      "Target gates use each contrast divided by its maximum absolute",
      "coordinate. Preference shares and explained-heterogeneity shares are",
      "invariant to this positive rescaling; variance components are returned",
      "to the original contrast units."),
    variance_floor = inf_cfg$variance_floor,
    minimum_directional_variance = apply(direction_variance, 1L, min),
    group_probability = group_probability,
    denominator_margin = 0.05, total_heterogeneity_margin = 1e-4,
    riesz_equation_tolerance = inf_cfg$riesz_equation_tolerance,
    ridge_sensitivity_tolerance = inf_cfg$ridge_sensitivity_tolerance),
  reporting_rule = paste(
    "Cross-fitted plug-in and one-step diagnostic estimates are retained.",
    "A diagnostic range is not a confidence interval. Formal intervals are",
    "withheld unless every fit, numerical, and verification gate passes."),
  session = utils::sessionInfo())
tq1_atomic_save_rds(manifest, manifest_path)
message("BR estimand-specific q=1 stage complete: ", out_dir)
