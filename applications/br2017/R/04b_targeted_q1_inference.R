#!/usr/bin/env Rscript
## Targeted q = 1 inference for Ballard--Rosa, Martin, and Scheve.
## Requires a corrected v2.1 full nested fit. The dated shared snapshot does
## not contain that fit, so this runner fails closed rather than mixing old
## summaries with corrected Graham--Svolik post-processing.

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
    z <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    if (!z[[1L]] %in% names(out)) stop("Unknown argument --", z[[1L]])
    out[[z[[1L]]]] <- paste(z[-1L], collapse = "=")
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
  file.path(root, "runs/br2017", cli$profile, "targeted_q1")
fit_path <- file.path(fit_dir, "fit_primary_assembled.rds")
needed <- c(fit_path, prepared_path)
if (any(!file.exists(needed))) {
  stop("Corrected Ballard--Rosa inputs are absent. Run 01_prepare_data.R and ",
       "03_fit_models.R --profile=v21_corrected --stage=primary first, or ",
       "supply --fit_dir and --prepared. Missing: ",
       paste(needed[!file.exists(needed)], collapse = ", "), ".",
       call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required.")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/targeted_q1_inference.R"), local = FALSE)
source(file.path(root, "applications/R/br_progressivity_contrasts.R"),
       local = FALSE)

fit <- readRDS(fit_path)
prepared <- readRDS(prepared_path)
if (!inherits(fit, "scmix_nested_assembled") || fit$q != 1L) {
  stop("The targeted runner requires an assembled q = 1 fit.")
}
if (!identical(as.character(fit$attr_names), cfg$coefficients$order)) {
  stop("The saved fit does not use the frozen Ballard--Rosa coefficient order.")
}
inf_cfg <- cfg$inference
inf_cfg$multiplier_draws <- cli$multiplier_draws
seed <- as.integer(cfg$optimizer$seed + 42000L)
fingerprint <- tq1_fingerprint(root, c(fit_path, prepared_path), list(
  application = "br2017", profile = cli$profile, q = 1L, seed = seed,
  multiplier_draws = inf_cfg$multiplier_draws,
  runner_md5 = unname(tools::md5sum(script_path()))))

basis_obj <- tq1_checkpoint(file.path(out_dir, "basis_reconstruction.rds"),
  fingerprint, cli$force, tq1_rebuild_inference_basis(fit))
respondents <- basis_obj$respondents
attr_names <- fit$attr_names
progressivity <- br_progressivity_contrasts(
  attr_names, include = c("top_minus_bottom", "slope", "mean_rate"))
dense <- function(x) {
  out <- stats::setNames(numeric(length(attr_names)), attr_names)
  out[names(x)] <- x
  out
}
progressivity <- lapply(progressivity, dense)

direction_dml <- NULL
direction_summary <- NULL
if ("direction" %in% families) {
  dset <- progressivity[c("top_minus_bottom", "slope")]
  var_by_fold <- t(vapply(dset, function(d)
    vapply(fit$A_folds, function(A) sum(crossprod(A, d)^2), numeric(1L)),
    numeric(fit$K)))
  if (any(apply(var_by_fold, 1L, min) < inf_cfg$variance_floor)) {
    stop("A prespecified progressivity direction is below the variance floor.")
  }
  targets <- lapply(names(dset), function(nm) scmix_inference_target(
    type = "sign", contrast = dset[[nm]],
    variance_floor = inf_cfg$variance_floor, label = nm))
  names(targets) <- paste0("direction_", names(dset))
  direction_dml <- tq1_checkpoint(file.path(out_dir, "direction_dml.rds"),
    fingerprint, cli$force,
    tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed))
  direction_summary <- tq1_family_summary(
    direction_dml, "preference_direction", inf_cfg$level)
  direction_summary$direction <- sub("^direction_", "",
                                     direction_summary$target)
  direction_summary$reported_event <- "prefers greater tax progressivity"
  tq1_atomic_write_csv(direction_summary,
                       file.path(out_dir, "direction_summary.csv"))
}

heterogeneity_summary <- NULL
if ("heterogeneity" %in% families) {
  dirs <- c(stats::setNames(lapply(attr_names, function(nm)
    tq1_unit_contrast(attr_names, nm)), attr_names), progressivity)
  targets <- lapply(names(dirs), function(nm) scmix_inference_target(
    type = "heterogeneity_primitives", contrast = dirs[[nm]], label = nm))
  names(targets) <- paste0("heterogeneity_", names(dirs))
  hdml <- tq1_checkpoint(file.path(out_dir, "heterogeneity_dml.rds"),
    fingerprint, cli$force,
    tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed + 1L))
  primitive_summary <- tq1_family_summary(
    hdml, "heterogeneity_primitives", inf_cfg$level)
  tq1_atomic_write_csv(primitive_summary,
                       file.path(out_dir, "heterogeneity_primitives.csv"))
  rows <- lapply(names(dirs), function(nm) {
    stem <- paste0("heterogeneity_", nm)
    tr <- scmix_delta_transform(hdml, type = "directional_heterogeneity",
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
}

choice_summary <- NULL
if ("choice" %in% families) {
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
  if (any(vapply(groups, mean, numeric(1L)) < 0.05)) {
    stop("A prespecified party group is below the 0.05 probability margin.")
  }
  plans <- list(
    steeply_progressive = c(0, 5, 15, 25, 35, 45, 2),
    flat_15 = c(15, 15, 15, 15, 15, 15, 0),
    status_quo = c(5, 15, 25, 25, 35, 35, 0),
    regressive = c(25, 25, 15, 15, 5, 5, -2))
  contrasts <- lapply(plans[c("flat_15", "status_quo", "regressive")],
    function(opp) stats::setNames(plans$steeply_progressive - opp, attr_names))
  targets <- list(); specs <- list()
  for (nm in names(contrasts)) {
    base <- scmix_inference_target(
      type = "choice", contrast = contrasts[[nm]], position_neutral = TRUE,
      n_nodes = inf_cfg$choice_nodes, label = paste0("choice_", nm))
    stem <- paste0("choice_", nm, "_overall")
    targets[[stem]] <- base
    specs[[stem]] <- list(opponent = nm, group = "Overall", transform = FALSE)
    for (g in names(groups)) {
      stem <- paste0("choice_", nm, "_", g)
      targets[[stem]] <- tq1_subgroup_target(base, groups[[g]])
      specs[[stem]] <- list(opponent = nm, group = g, transform = TRUE)
    }
  }
  cdml <- tq1_checkpoint(file.path(out_dir, "choice_dml.rds"), fingerprint,
    cli$force,
    tq1_run_family(fit, basis_obj$mu_basis, targets, inf_cfg, seed + 2L))
  primitive_summary <- tq1_family_summary(
    cdml, "choice_primitives", inf_cfg$level)
  tq1_atomic_write_csv(primitive_summary,
                       file.path(out_dir, "choice_primitives.csv"))
  rows <- list()
  for (stem in names(specs)) {
    spec <- specs[[stem]]
    if (!spec$transform) {
      z <- primitive_summary[primitive_summary$target == stem, , drop = FALSE]
      z$opponent <- spec$opponent; z$group <- spec$group
      rows[[stem]] <- z
    } else {
      tr <- scmix_delta_transform(cdml, type = "subgroup_ratio",
        primitives = paste0(stem, c(":weighted_target",
                                    ":subgroup_probability")),
        denominator_margin = 0.05, level = inf_cfg$level)
      z <- tq1_transform_summary(tr, "position_neutral_choice", stem,
                                 inf_cfg$level)
      z$plugin <- NA_real_; z$adjustment <- NA_real_
      z$opponent <- spec$opponent; z$group <- spec$group
      rows[[stem]] <- z
    }
  }
  ## Retain a common manuscript-facing column set across overall and subgroup rows.
  keep_cols <- Reduce(intersect, lapply(rows, names))
  choice_summary <- do.call(rbind, lapply(rows, function(x)
    x[, keep_cols, drop = FALSE]))
  rownames(choice_summary) <- NULL
  tq1_atomic_write_csv(choice_summary, file.path(out_dir, "choice_summary.csv"))
}

summaries <- Filter(Negate(is.null), list(direction_summary,
                                           heterogeneity_summary,
                                           choice_summary))
if (length(summaries)) {
  common <- Reduce(intersect, lapply(summaries, names))
  tq1_atomic_write_csv(do.call(rbind, lapply(summaries, function(x)
    x[, common, drop = FALSE])), file.path(out_dir, "gate_summary.csv"))
}
manifest <- list(
  application = "br2017", profile = cli$profile, q = 1L,
  completed_families = families,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  fingerprint = fingerprint,
  fit_status = list(diagnostic_only = fit$diagnostic_only,
    eligible_for_ordinary_inference = fit$eligible_for_ordinary_inference,
    computational_gate_pass = fit$computational_gate_pass),
  basis_checks = basis_obj[c("prediction_max_abs_deviation",
    "prediction_tolerance", "basis_dimension", "tangent_dimension")],
  reporting_rule = paste(
    "Diagnostic estimates and diagnostic standard errors are retained.",
    "Formal intervals are withheld unless every package and verification gate passes."),
  session = utils::sessionInfo())
tq1_atomic_save_rds(manifest, file.path(out_dir, "run_manifest.rds"))
message("BR targeted q=1 stage complete: ", out_dir)
