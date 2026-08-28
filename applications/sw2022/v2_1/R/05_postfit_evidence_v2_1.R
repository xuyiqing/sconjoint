#!/usr/bin/env Rscript

## Build the Saha--Weeks v2.1 post-fit evidence bundle.
##
## This runner has one model-resolution boundary: it reads and hash-validates
## the completed reported_primary_pointer.rds exactly once, then carries the
## resulting immutable snapshot through every downstream calculation. It
## refuses to read an unmanifested or partial fit and publishes only by an
## atomic directory rename after all gates pass.

options(stringsAsFactors = FALSE, warn = 1)

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(args) {
  out <- list(output_name = "final")
  for (arg in args) {
    if (!grepl("^--output-name=[A-Za-z0-9._-]+$", arg)) {
      stop("Only --output-name=<safe-name> is accepted.", call. = FALSE)
    }
    out$output_name <- sub("^--output-name=", "", arg)
  }
  if (startsWith(out$output_name, ".")) {
    stop("--output-name may not begin with a dot.", call. = FALSE)
  }
  out
}

.runner_source_paths <- function(app) {
  c(
    helper = file.path(app, "v2_1", "R", "postfit_helpers_v2_1.R"),
    postfit_config = file.path(
      app, "v2_1", "config", "postfit_evidence_config_v2_1.R"),
    producer_config = file.path(
      app, "v2_1", "config", "analysis_config_v2_1.R"),
    assessment_extensions = file.path(
      app, "R", "assessment_extensions.R"))
}

.load_runner_sources <- function(app, envir = globalenv()) {
  if (!is.environment(envir)) stop("`envir` must be an environment.",
                                   call. = FALSE)
  paths <- .runner_source_paths(app)
  if (any(!file.exists(paths))) {
    stop("A required v2.1 post-fit source is absent: ",
         paste(names(paths)[!file.exists(paths)], collapse = ", "),
         call. = FALSE)
  }
  options(sconjoint.sw_application_root = app)
  for (path in paths) sys.source(path, envir = envir)
  required <- c(
    "%||%", ".swv21_contrast_labels", ".swv21_contrasts",
    ".swv21_resolve_reported_primary", ".swv21_stable_read_rds",
    ".swv21_validate_fit_stamp", ".swv21_crossfit_contract",
    ".swv21_direct_amce", ".swv21_fold_basis", ".swv21_kappa_target",
    ".swv21_serial_summary", ".swv21_group_calibration",
    ".swv21_read_artifact_manifest", ".swv21_table_inventory",
    ".sw_exact_joint_predictions", ".sw_joint_calibration_table",
    ".sw_joint_extension_status", ".sw_completion_comparisons",
    ".sw_conditional_randomization_status", "sw_v21_postfit_config",
    "sw_v21_config")
  available <- vapply(required, exists, logical(1L), envir = envir,
                      inherits = TRUE)
  if (!all(available)) {
    stop("Post-fit runner dependency load is incomplete: ",
         paste(required[!available], collapse = ", "), call. = FALSE)
  }
  invisible(list(paths = paths, required = required, available = available))
}

.score_summary <- function(scores, meta, crossfit_contract) {
  groups <- list(Overall = rep("Overall", length(scores)),
                 party = meta$party3,
                 respondent_gender = meta$respondent_gender2)
  rows <- list(); z <- 0L
  for (variable in names(groups)) {
    g <- groups[[variable]]
    for (level in unique(g)) {
      keep <- g == level
      z <- z + 1L
      rows[[z]] <- data.frame(
        group_variable = variable, group = level,
        mean_complete_sequence_log_score = mean(scores[keep]),
        respondent_se = stats::sd(scores[keep]) / sqrt(sum(keep)),
        n_respondents = sum(keep),
        crossfit_contract_validated = crossfit_contract$validated,
        package_verified_heldout =
          crossfit_contract$package_verified_heldout,
        ordinary_inference_eligible =
          crossfit_contract$ordinary_inference_eligible,
        end_to_end_outcome_blind = FALSE,
        assessment_role = "cross_fitted_postpilot_diagnostic",
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.structural_tables <- function(full, prepared, meta, contrasts,
                               postfit_config) {
  core <- full$refit
  rid_task <- as.character(prepared$respondent_id)
  rid <- unique(rid_task)
  first <- match(rid, rid_task)
  mu <- as.matrix(core$mu)
  mu_resp <- if (nrow(mu) == length(rid_task)) mu[first, , drop = FALSE] else
    if (nrow(mu) == length(rid)) mu else
      stop("Full-fit conditional means have incompatible dimensions.",
           call. = FALSE)
  if (any(!is.finite(mu_resp)) || ncol(mu_resp) != ncol(prepared$deltaX)) {
    stop("Full-fit conditional means are malformed.", call. = FALSE)
  }
  colnames(mu_resp) <- colnames(prepared$deltaX)
  Sigma <- as.matrix(core$Sigma)
  A <- as.matrix(core$A)
  if (!identical(dim(Sigma), c(ncol(mu_resp), ncol(mu_resp))) ||
      any(!is.finite(Sigma)) ||
      !identical(dim(A), c(ncol(mu_resp), 1L)) || any(!is.finite(A))) {
    stop("The q=1 covariance or loading is malformed.", call. = FALSE)
  }
  dimnames(Sigma) <- list(colnames(mu_resp), colnames(mu_resp))

  view <- list(
    respondent_id = rid_task, Z = as.matrix(prepared$Z_primary),
    attr_names = colnames(prepared$deltaX),
    full_fit = list(mu = core$mu, Sigma = Sigma, A = A,
                    kappa = core$kappa),
    analysis_signature = full$analysis_signature %||%
      core$analysis_signature)

  group_defs <- list(
    Overall = stats::setNames(rep("Overall", length(rid)), rid),
    party = stats::setNames(meta$party3, rid),
    respondent_gender = stats::setNames(meta$respondent_gender2, rid))
  coordinate_rows <- list(); contrast_rows <- list(); z <- w <- 0L
  for (variable in names(group_defs)) {
    g <- group_defs[[variable]]
    for (level in unique(g)) {
      keep <- g == level
      theta <- colMeans(mu_resp[keep, , drop = FALSE])
      for (j in seq_along(theta)) {
        z <- z + 1L
        coordinate_rows[[z]] <- data.frame(
          group_variable = variable, group = level,
          coordinate = names(theta)[[j]], estimate = theta[[j]],
          n_respondents = sum(keep),
          estimand = "equal-respondent structural conditional-mean coordinate",
          posterior_summaries_used = FALSE, formal_inference_available = FALSE,
          stringsAsFactors = FALSE)
      }
      for (nm in names(contrasts)) {
        w <- w + 1L
        contrast_rows[[w]] <- data.frame(
          group_variable = variable, group = level, contrast = nm,
          label = unname(.swv21_contrast_labels()[[nm]]),
          estimate = sum(contrasts[[nm]] * theta), n_respondents = sum(keep),
          estimand = "structural latent-preference contrast",
          posterior_summaries_used = FALSE, formal_inference_available = FALSE,
          stringsAsFactors = FALSE)
      }
    }
  }

  choices <- list(); cidx <- 0L
  for (nm in names(postfit_config$qoi$contests)) {
    quantity <- scmix_paper_choice(
      view, contrast = postfit_config$qoi$contests[[nm]],
      position_neutral = TRUE,
      n_nodes = postfit_config$inference$choice_nodes, on_support = NA)
    conditional <- as.numeric(quantity$details$conditional_probability)
    for (variable in names(group_defs)) {
      g <- group_defs[[variable]]
      for (level in unique(g)) {
        keep <- g == level
        cidx <- cidx + 1L
        choices[[cidx]] <- data.frame(
          contest = nm, group_variable = variable, group = level,
          estimate = mean(conditional[keep]), n_respondents = sum(keep),
          position_neutral = TRUE,
          directional_residual_variance =
            quantity$details$directional_variance,
          support_status = quantity$details$support,
          support_note = postfit_config$qoi$support_note,
          posterior_summaries_used = FALSE,
          formal_inference_available = FALSE,
          stringsAsFactors = FALSE)
      }
    }
  }

  heterogeneity_names <- c(
    "female_vs_male", "agenda_moderate_vs_very_few",
    "agenda_complete_vs_very_few")
  heterogeneity <- do.call(rbind, lapply(heterogeneity_names, function(nm) {
    q <- scmix_paper_heterogeneity(
      view, direction = contrasts[[nm]],
      total_margin = postfit_config$inference$total_heterogeneity_margin)
    data.frame(
      contrast = nm, component = names(q$estimate),
      estimate = as.numeric(q$estimate),
      reporting_gate_pass = isTRUE(q$gate$pass),
      reporting_margin = q$gate$margin,
      posterior_summaries_used = FALSE,
      formal_inference_available = FALSE,
      stringsAsFactors = FALSE)
  }))
  sign_names <- c(
    "female_vs_male", "talent_hard_working_vs_empathetic",
    "agenda_moderate_vs_very_few", "agenda_complete_vs_very_few")
  sign <- do.call(rbind, lapply(sign_names, function(nm) {
    q <- scmix_paper_signshare(
      view, contrast = contrasts[[nm]], ties = "exclude",
      variance_margin = postfit_config$inference$variance_floor, ci = NULL)
    data.frame(
      contrast = nm, estimate = as.numeric(q$estimate),
      directional_residual_variance = q$details$directional_variance,
      variance_gate_pass = isTRUE(q$gate$pass),
      majority_claim = q$details$majority_claim,
      posterior_summaries_used = FALSE,
      formal_inference_available = FALSE,
      stringsAsFactors = FALSE)
  }))
  decomposition <- scmix_paper_heterogeneity(view)
  covariance <- data.frame(
    component = c("trace_between_Z", "trace_residual", "trace_total",
                  "minimum_residual_eigenvalue",
                  "maximum_residual_eigenvalue", "full_fit_kappa"),
    estimate = c(
      sum(diag(decomposition$details$Omega_Z)),
      sum(diag(decomposition$details$Omega_R)),
      sum(diag(decomposition$details$Omega_T)),
      min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values),
      max(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values),
      as.numeric(core$kappa)),
    formal_inference_available = FALSE, stringsAsFactors = FALSE)
  list(
    view = view, mu_resp = mu_resp,
    coordinates = do.call(rbind, coordinate_rows),
    contrasts = do.call(rbind, contrast_rows),
    choice = do.call(rbind, choices),
    heterogeneity = heterogeneity, sign = sign, covariance = covariance,
    qoi_objects = list(covariance_decomposition = decomposition))
}

.build_dml_targets <- function(assembled, prepared, meta, contrasts,
                               postfit_config) {
  rid <- unique(as.character(prepared$respondent_id))
  targets <- list(kappa = .swv21_kappa_target)
  for (nm in names(contrasts)) {
    targets[[paste0("tau_", nm)]] <- scmix_inference_target(
      type = "tau", contrast = contrasts[[nm]], label = nm)
  }
  for (level in c("Democrat", "Independent", "Republican")) {
    subgroup <- stats::setNames(as.numeric(meta$party3 == level), rid)
    nm <- tolower(level)
    targets[[paste0("female_party_", nm)]] <- scmix_inference_target(
      type = "subgroup_tau_primitives",
      contrast = contrasts$female_vs_male, subgroup = subgroup)
  }
  for (level in c("Female", "Male")) {
    subgroup <- stats::setNames(
      as.numeric(meta$respondent_gender2 == level), rid)
    nm <- tolower(level)
    targets[[paste0("female_respgender_", nm)]] <- scmix_inference_target(
      type = "subgroup_tau_primitives",
      contrast = contrasts$female_vs_male, subgroup = subgroup)
  }
  for (nm in names(postfit_config$qoi$contests)) {
    targets[[paste0("choice_contest_", nm)]] <- scmix_inference_target(
      type = "choice", contrast = postfit_config$qoi$contests[[nm]],
      position_neutral = TRUE,
      n_nodes = postfit_config$inference$choice_nodes,
      label = paste0("contest_", nm))
  }
  for (nm in c("female_vs_male", "agenda_moderate_vs_very_few",
               "agenda_complete_vs_very_few")) {
    targets[[paste0("heterogeneity_", nm)]] <- scmix_inference_target(
      type = "heterogeneity_primitives", contrast = contrasts[[nm]])
  }
  loading_folds <- assembled$A_computational_folds %||% assembled$A_folds
  for (nm in c("female_vs_male", "talent_hard_working_vs_empathetic",
               "agenda_moderate_vs_very_few",
               "agenda_complete_vs_very_few")) {
    variance <- vapply(loading_folds, function(A) {
      sum(as.numeric(crossprod(as.matrix(A), contrasts[[nm]]))^2)
    }, numeric(1L))
    if (all(variance >= postfit_config$inference$variance_floor)) {
      targets[[paste0("sign_", nm)]] <- scmix_inference_target(
        type = "sign", contrast = contrasts[[nm]],
        variance_floor = postfit_config$inference$variance_floor,
        label = paste0("sign_", nm))
    }
  }
  targets
}

.run_diagnostic_dml <- function(assembled, prepared, meta, contrasts,
                                postfit_config) {
  basis <- .swv21_fold_basis(assembled, prepared)
  targets <- .build_dml_targets(
    assembled, prepared, meta, contrasts, postfit_config)
  cfg <- postfit_config$inference
  inference <- scmix_dml(
    assembled, targets = "theta", plugin_targets = targets,
    mu_basis = basis, nu_grid = NULL, riesz_penalty = "identity",
    riesz_validation_fraction = cfg$riesz_validation_fraction,
    active_eigenvalue_min = cfg$active_eigenvalue_min,
    rank_tolerance = cfg$rank_tolerance,
    information_eigenvalue_min = cfg$information_eigenvalue_min,
    riesz_equation_tolerance = cfg$riesz_equation_tolerance,
    ridge_sensitivity_tolerance = cfg$ridge_sensitivity_tolerance,
    allow_numeric_derivatives = FALSE, verification = NULL,
    multiplier_draws = 0L, level = cfg$level, seed = 20260824L + 990000L)
  if (!inherits(inference, "scmix_dml") ||
      isTRUE(inference$inference_available) ||
      !identical(inference$status, "conditional_unverified") ||
      !is.numeric(inference$estimate) || any(!is.finite(inference$estimate)) ||
      !is.numeric(inference$diagnostic_se) ||
      any(!is.finite(inference$diagnostic_se))) {
    stop("Diagnostic DML did not return the required fail-closed result.",
         call. = FALSE)
  }
  table <- data.frame(
    target = names(inference$estimate),
    one_step_estimate = as.numeric(inference$estimate),
    plugin_estimate = as.numeric(inference$plugin_estimate),
    one_step_adjustment = as.numeric(inference$one_step_adjustment),
    diagnostic_se_not_formal = as.numeric(inference$diagnostic_se),
    formal_se = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
    inference_available = FALSE, status = inference$status,
    reason = inference$reason, stringsAsFactors = FALSE)
  fold <- do.call(rbind, lapply(seq_along(inference$fold_details), function(k) {
    x <- inference$fold_details[[k]]
    data.frame(
      outer_fold = k,
      heldout_respondents = length(x$heldout_respondents),
      training_n = x$training_n, heldout_n = x$heldout_n,
      tangent_dimension = x$tangent_dimension,
      information_structural_min = x$information_structural_min,
      riesz_equation_relative_residual =
        max(x$riesz_equation_relative_residual),
      ridge_relative_sensitivity = max(x$ridge_relative_sensitivity),
      mean_training_loglik = x$mean_training_loglik,
      mean_heldout_loglik = x$mean_heldout_loglik,
      formal_inference_available = FALSE, stringsAsFactors = FALSE)
  }))
  status <- data.frame(
    component = c("diagnostic one-step calculation", "formal inference",
                  "Riesz equation tolerance", "ridge sensitivity tolerance"),
    status = c("run_conditional_unverified", "withheld",
               if (inference$riesz_equation_max_relative_residual <=
                   cfg$riesz_equation_tolerance) "diagnostic_gate_pass" else
                 "diagnostic_gate_fail",
               if (inference$ridge_max_relative_sensitivity <=
                   cfg$ridge_sensitivity_tolerance) "diagnostic_gate_pass" else
                 "diagnostic_gate_fail"),
    value = c(NA_real_, NA_real_,
              inference$riesz_equation_max_relative_residual,
              inference$ridge_max_relative_sensitivity),
    threshold = c(NA_real_, NA_real_, cfg$riesz_equation_tolerance,
                  cfg$ridge_sensitivity_tolerance),
    interpretation = c(
      "Cross-fitted one-step values and diagnostic SEs only.",
      "No fitted-sieve approximation/product-rate verification record exists.",
      "A diagnostic numerical equation check; not a formal-inference switch.",
      "A diagnostic ridge sensitivity check; not a formal-inference switch."),
    stringsAsFactors = FALSE)
  transforms <- list()
  for (nm in c("democrat", "independent", "republican")) {
    stem <- paste0("female_party_", nm)
    transforms[[stem]] <- scmix_delta_transform(
      inference, type = "subgroup_ratio",
      primitives = c(paste0(stem, ":weighted_tau"),
                     paste0(stem, ":subgroup_probability")),
      denominator_margin = cfg$subgroup_probability_margin,
      level = cfg$level)
  }
  for (nm in c("female", "male")) {
    stem <- paste0("female_respgender_", nm)
    transforms[[stem]] <- scmix_delta_transform(
      inference, type = "subgroup_ratio",
      primitives = c(paste0(stem, ":weighted_tau"),
                     paste0(stem, ":subgroup_probability")),
      denominator_margin = cfg$subgroup_probability_margin,
      level = cfg$level)
  }
  for (nm in c("female_vs_male", "agenda_moderate_vs_very_few",
               "agenda_complete_vs_very_few")) {
    stem <- paste0("heterogeneity_", nm)
    transforms[[stem]] <- scmix_delta_transform(
      inference, type = "directional_heterogeneity",
      primitives = paste0(stem, c(":mean", ":second_moment",
                                  ":residual_variance")),
      total_margin = cfg$total_heterogeneity_margin, level = cfg$level)
  }
  transform_table <- do.call(rbind, lapply(names(transforms), function(nm) {
    x <- transforms[[nm]]
    data.frame(
      quantity = nm, component = names(x$estimate),
      estimate = as.numeric(x$estimate),
      diagnostic_se_not_formal = as.numeric(x$diagnostic_se),
      formal_se = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      quantity_gate_pass = isTRUE(x$gate$pass),
      inference_available = FALSE, status = x$status,
      reason = x$reason, stringsAsFactors = FALSE)
  }))
  list(object = inference, table = table, fold = fold, status = status,
       transforms = transforms, transform_table = transform_table,
       basis_dimensions = vapply(basis, ncol, integer(1L)),
       targets = names(targets))
}

.main <- function(args = commandArgs(trailingOnly = TRUE)) {
  cli <- .parse_cli(args)
  script <- .script_file()
  project <- normalizePath(file.path(dirname(script), "../../../.."),
                           mustWork = TRUE)
  app <- file.path(project, "applications", "sw2022")
  runner_sources <- .runner_source_paths(app)
  helper_path <- runner_sources[["helper"]]
  postfit_config_path <- runner_sources[["postfit_config"]]
  producer_config_path <- runner_sources[["producer_config"]]
  extension_path <- runner_sources[["assessment_extensions"]]
  .load_runner_sources(app, envir = globalenv())
  if (!identical(sw_v21_postfit_config$formal_inference_available, FALSE) ||
      !identical(sw_v21_postfit_config$outcome_blind, FALSE)) {
    stop("Post-fit evidence config violated its fail-closed labels.",
         call. = FALSE)
  }

  producer_dir <- sw_v21_config$output_root
  snapshot <- .swv21_resolve_reported_primary(
    producer_dir, sw_v21_config, sw_v21_postfit_config)

  role_prefix <- if (isTRUE(snapshot$fallback_applied)) "constant" else
    "selected"
  fit_reads <- lapply(snapshot$fit_paths, .swv21_stable_read_rds)
  fits <- lapply(fit_reads, `[[`, "value")
  for (nm in names(fit_reads)) {
    if (!identical(fit_reads[[nm]]$md5, snapshot$fit_md5[[nm]])) {
      stop("A reported-primary fit changed after pointer resolution.",
           call. = FALSE)
    }
    .swv21_validate_fit_stamp(
      fits[[nm]], paste0(role_prefix, "_", nm), snapshot, sw_v21_config)
  }
  full <- fits$full; nested <- fits$nested; assembled <- fits$assembled

  prep_manifest_path <- file.path(
    app, "manifests", "prep_artifact_manifest.csv")
  design_manifest_path <- file.path(
    app, "manifests", "design_completion_artifact_manifest.csv")
  prep_audit <- .swv21_read_artifact_manifest(prep_manifest_path, project)
  design_artifact_audit <- .swv21_read_artifact_manifest(
    design_manifest_path, project)
  prepared_path <- file.path(app, "results", "prep_analysis_data.rds")
  design_audit_path <- file.path(
    app, "results", "design_completion_audit.rds")
  design_task_path <- file.path(app, "results", "design_task_metadata.rds")
  completion_task_path <- file.path(
    app, "results", "completion_task_audit.rds")
  prepared <- .swv21_stable_read_rds(prepared_path)$value
  design_audit <- .swv21_stable_read_rds(design_audit_path)$value
  design_task <- .swv21_stable_read_rds(design_task_path)$value
  completion_task <- .swv21_stable_read_rds(completion_task_path)$value
  if (!identical(nrow(prepared$deltaX), 3573L) ||
      length(unique(prepared$respondent_id)) != 1191L ||
      !identical(ncol(prepared$deltaX), 13L) ||
      !identical(ncol(prepared$Z_primary), 15L) ||
      !identical(as.matrix(assembled$deltaX), as.matrix(prepared$deltaX)) ||
      !identical(as.numeric(assembled$y), as.numeric(prepared$y)) ||
      !identical(as.character(assembled$respondent_id),
                 as.character(prepared$respondent_id))) {
    stop("Prepared data and the reported assembled fit are not exactly aligned.",
         call. = FALSE)
  }
  if (!identical(as.character(design_audit$completion$task_audit_md5),
                 .swv21_md5(completion_task_path))) {
    stop("Completion-task audit is not linked to the design audit.",
         call. = FALSE)
  }

  postfit_source_paths <- c(
    runner = script, helper = helper_path,
    postfit_config = postfit_config_path,
    producer_config = producer_config_path,
    assessment_extensions = extension_path,
    prep_manifest = prep_manifest_path,
    design_manifest = design_manifest_path,
    package_description = file.path(project, "DESCRIPTION"),
    package_namespace = file.path(project, "NAMESPACE"),
    stats::setNames(sort(list.files(file.path(project, "R"),
                                    pattern = "[.]R$", full.names = TRUE)),
                    paste0("package_source:", basename(sort(list.files(
                      file.path(project, "R"), pattern = "[.]R$",
                      full.names = TRUE)))))
  )
  postfit_source_md5 <- .swv21_hash_paths(postfit_source_paths)

  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The project-local pkgload package is required.", call. = FALSE)
  }
  suppressPackageStartupMessages(pkgload::load_all(project, quiet = TRUE))
  meta <- .swv21_meta(prepared)
  contrasts <- .swv21_contrasts(colnames(prepared$deltaX))

  output_parent <- file.path(app, "results",
                             sw_v21_postfit_config$output_directory_name)
  final_dir <- file.path(output_parent, cli$output_name)
  if (file.exists(final_dir)) {
    stop("Refusing to overwrite an existing evidence directory: ", final_dir,
         call. = FALSE)
  }
  dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
  staging <- file.path(
    output_parent,
    paste0(".staging-", cli$output_name, "-", Sys.getpid(), "-",
           format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")))
  table_dir <- file.path(staging, "tables")
  object_dir <- file.path(staging, "objects")
  manifest_dir <- file.path(staging, "manifests")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list()
  add_table <- function(name, value) {
    if (is.null(value)) return(invisible(FALSE))
    value <- as.data.frame(value, stringsAsFactors = FALSE,
                           check.names = FALSE)
    tables[[name]] <<- value
    .swv21_write_csv(value, file.path(table_dir, paste0(name, ".csv")))
    invisible(TRUE)
  }

  ## Preserve byte-identical estimator-independent preparation/design tables.
  reusable <- unique(rbind(prep_audit, design_artifact_audit))
  reusable_csv <- reusable[grepl("[.]csv$", reusable$path), , drop = FALSE]
  for (i in seq_len(nrow(reusable_csv))) {
    source_path <- reusable_csv$path[[i]]
    name <- paste0("preparation__", sub("[.]csv$", "", basename(source_path)))
    before <- .swv21_md5(source_path)
    value <- utils::read.csv(source_path, stringsAsFactors = FALSE,
                             check.names = FALSE)
    if (!identical(before, .swv21_md5(source_path))) {
      stop("An estimator-independent source table changed while read.",
           call. = FALSE)
    }
    target <- file.path(table_dir, paste0(name, ".csv"))
    copied <- file.copy(source_path, target, overwrite = FALSE,
                        copy.mode = TRUE)
    if (!isTRUE(copied) || !identical(before, .swv21_md5(target))) {
      stop("Could not preserve an estimator-independent source table ",
           "byte-for-byte.", call. = FALSE)
    }
    tables[[name]] <- value
  }
  add_table("provenance__prep_artifact_audit", prep_audit)
  add_table("provenance__design_artifact_audit", design_artifact_audit)

  ## Full-sample structural plug-ins from the pointer-selected full fit.
  structural <- .structural_tables(
    full, prepared, meta, contrasts, sw_v21_postfit_config)
  add_table("structural__mean_coordinates", structural$coordinates)
  add_table("structural__latent_preference_contrasts", structural$contrasts)
  add_table("structural__position_neutral_choice", structural$choice)
  add_table("structural__heterogeneity", structural$heterogeneity)
  add_table("structural__sign_shares", structural$sign)
  add_table("structural__covariance_summary", structural$covariance)

  ## Diagnostic-only cross-fitted one-step/Riesz calculation.
  dml <- .run_diagnostic_dml(
    assembled, prepared, meta, contrasts, sw_v21_postfit_config)
  add_table("inference__diagnostic_one_step", dml$table)
  add_table("inference__fold_diagnostics", dml$fold)
  add_table("inference__status", dml$status)
  add_table("inference__diagnostic_transforms", dml$transform_table)
  add_table("inference__rank_gate", dml$object$rank_gate)

  ## Respondent-cross-fitted predictive diagnostics.
  predictions <- scmix_heldout_predictions(
    assembled, task_order = prepared$task, include_counts = TRUE,
    include_adjacent = TRUE, include_repeated = TRUE)
  if (!isTRUE(predictions$out_of_fold) ||
      !isTRUE(predictions$complete_sequence) ||
      !isTRUE(predictions$shared_factor_within_sequence) ||
      isTRUE(predictions$posterior_summaries_used)) {
    stop("Held-out prediction provenance failed.", call. = FALSE)
  }
  design_cell <- if (is.data.frame(design_task) &&
                     "design_cell" %in% names(design_task) &&
                     nrow(design_task) == nrow(prepared$deltaX)) {
    design_task$design_cell
  } else paste0("nonzero_coordinates_",
                rowSums(abs(prepared$deltaX) > 1e-12))
  party_task <- meta$party3[match(
    as.character(prepared$respondent_id), as.character(meta$respondent_id))]
  assessment <- scmix_prediction_assessment(
    predictions, design_cell = design_cell, respondent_group = party_task)
  crossfit_contract <- .swv21_crossfit_contract(
    nested, assembled, prepared, predictions, assessment)
  if (!isTRUE(crossfit_contract$validated)) {
    failed <- crossfit_contract$checks$check[
      !crossfit_contract$checks$pass]
    stop("Respondent outer-fold application contract failed: ",
         paste(failed, collapse = "; "), call. = FALSE)
  }
  task_prediction <- assessment$predictions$task
  task_prediction$party <- party_task
  task_prediction$respondent_gender <- meta$respondent_gender2[match(
    task_prediction$respondent_id, as.character(meta$respondent_id))]
  task_prediction$assessment_role <- "cross_fitted_postpilot_diagnostic"
  task_prediction$end_to_end_outcome_blind <- FALSE
  sequence_scores <- as.numeric(predictions$sequence_loglik[
    as.character(meta$respondent_id)])
  if (any(!is.finite(sequence_scores))) stop("Sequence scores are malformed.",
                                             call. = FALSE)
  respondent_score <- data.frame(
    respondent_id = meta$respondent_id,
    outer_fold = task_prediction$fold[match(
      meta$respondent_id, task_prediction$respondent_id)],
    party = meta$party3, respondent_gender = meta$respondent_gender2,
    observed_candidate_A_count = as.numeric(tapply(
      task_prediction$observed, task_prediction$respondent_id, sum)[
        meta$respondent_id]),
    complete_sequence_log_score = sequence_scores,
    assessment_role = "cross_fitted_postpilot_diagnostic",
    formal_test = FALSE, stringsAsFactors = FALSE)
  add_table("prediction__respondent_sequence_scores", respondent_score)
  add_table("prediction__sequence_score_summary",
            .score_summary(sequence_scores, meta, crossfit_contract))
  add_table("prediction__crossfit_contract", crossfit_contract$checks)
  add_table("prediction__heldout_task_predictions", task_prediction)
  add_table("prediction__calibration_marginal",
            assessment$calibration$marginal)
  add_table("prediction__calibration_joint",
            assessment$calibration$joint)
  add_table("prediction__calibration_response_count",
            assessment$calibration$response_count)

  group_calibration <- rbind(
    .swv21_group_calibration(
      task_prediction, "Overall", rep("Overall", nrow(task_prediction))),
    .swv21_group_calibration(task_prediction, "party",
                             task_prediction$party),
    .swv21_group_calibration(task_prediction, "respondent_gender",
                             task_prediction$respondent_gender),
    .swv21_group_calibration(task_prediction, "task_order",
                             as.character(task_prediction$task_order)))
  group_calibration$assessment_role <- "cross_fitted_postpilot_diagnostic"
  add_table("prediction__calibration_by_group_and_order", group_calibration)
  serial <- .swv21_serial_summary(task_prediction)
  add_table("prediction__order_serial_diagnostics", serial)
  position <- rbind(
    transform(group_calibration[
      group_calibration$group_variable %in% c("Overall", "task_order"),
      c("group_variable", "group", "observed_rate", "predicted_rate",
        "calibration_gap", "respondent_se_gap", "n_respondents", "n_tasks")],
      diagnostic = "candidate-A position calibration"),
    data.frame(
      group_variable = "full_fit", group = "Overall",
      observed_rate = NA_real_, predicted_rate = NA_real_,
      calibration_gap = as.numeric(full$refit$kappa),
      respondent_se_gap = NA_real_, n_respondents = nrow(meta),
      n_tasks = nrow(prepared$deltaX),
      diagnostic = "structural candidate-A intercept kappa",
      stringsAsFactors = FALSE))
  position$role <- paste(
    "Descriptive alternative-side diagnostic; not a profile-swap refit and",
    "not evidence that position assignment is ignorable.")
  add_table("prediction__position_diagnostics", position)

  joint <- .sw_exact_joint_predictions(assembled, prepared)
  add_table("prediction__full_response_pattern_calibration",
            .sw_joint_calibration_table(joint$full))
  add_table("prediction__prespecified_task_pair_calibration",
            .sw_joint_calibration_table(joint$pair))
  add_table("prediction__exact_repeated_contrast_calibration",
            .sw_joint_calibration_table(joint$repeated))
  joint_status <- .sw_joint_extension_status(
    joint, "run_crossfitted_postpilot_diagnostic")
  add_table("prediction__joint_calibration_status", joint_status)
  add_table("prediction__joint_probability_checks", data.frame(
    diagnostic = names(joint$probability_sum_error),
    maximum_probability_sum_error =
      as.numeric(joint$probability_sum_error),
    tolerance = 1e-10,
    gate_pass = is.na(joint$probability_sum_error) |
      joint$probability_sum_error <= 1e-10,
    stringsAsFactors = FALSE))

  ## Direct randomized-design benchmarks and deliberately cross-scale checks.
  direct <- .swv21_direct_amce(prepared)
  for (variable in c("party3", "respondent_gender2")) {
    task_group <- meta[[variable]][match(
      as.character(prepared$respondent_id), as.character(meta$respondent_id))]
    for (level in unique(task_group)) {
      direct <- rbind(direct, .swv21_direct_amce(
        prepared, keep = task_group == level,
        group_variable = sub("[23]$", "", variable), group = level))
    }
  }
  add_table("design__direct_amce", direct)
  structural_key <- structural$contrasts[
    structural$contrasts$group_variable %in%
      c("Overall", "party", "respondent_gender"), , drop = FALSE]
  compare <- merge(
    direct[, c("contrast", "group_variable", "group", "estimate",
               "respondent_cluster_se")],
    structural_key[, c("contrast", "group_variable", "group", "estimate")],
    by = c("contrast", "group_variable", "group"), suffixes = c("_amce",
                                                                  "_structural"),
    all = FALSE)
  compare$sign_agreement <- sign(compare$estimate_amce) ==
    sign(compare$estimate_structural)
  compare$comparison_status <- paste(
    "Qualitative parallel check only: different estimands and scales; no",
    "equality test or discrepancy interval.")
  add_table("design__amce_structural_parallel", compare)

  design_status <- data.frame(
    component = c(
      "realized support and algebraic rank",
      "conditional identification under advertised full support",
      "fielded randomizer/protocol certification",
      "exact ordered-contrast Horvitz--Thompson benchmark",
      "marginal AMCE-style benchmark",
      "structural-versus-design discrepancy test"),
    status = c(
      "reused_hash_validated_estimator_independent_audit",
      "maintained_conditional_result_not_empirically_certified",
      "unavailable", "protocol_unavailable_not_run",
      "run_conditional_on_advertised_randomization", "not_run"),
    maintained_assumption_verified = FALSE,
    note = c(
      "Realized ranks and theoretical design algebra were preserved byte-for-byte.",
      "Advertised full support supplies the algebraic result; exact fielding support was not recovered.",
      design_audit$design$protocol_caveat,
      "Exact respondent exposure probabilities and cross-task restrictions are absent.",
      "Respondent-clustered marginal probability effects are a different estimand from latent preferences.",
      "No exact structural-versus-HT discrepancy is formed without protocol probabilities."),
    stringsAsFactors = FALSE)
  add_table("design__assessment_ledger", design_status)
  conditional_randomization <- .sw_conditional_randomization_status(
    design_audit)
  add_table("design__conditional_randomization_status",
            conditional_randomization)

  ## Completion evidence uses only the frozen raw-universe audit.
  completion_comparison <- .sw_completion_comparisons(completion_task)
  add_table("completion__early_response_assignment_by_task",
            completion_comparison$summary)
  add_table("completion__early_response_assignment_status",
            completion_comparison$status)
  status_raw <- design_audit$completion$status
  predictors <- status_raw[, intersect(
    c("finished", "progress", "all_primary_demographics_missing",
      "primary_demographics_valid", "final_analysis_sample"),
    names(status_raw)), drop = FALSE]
  completion_pattern <- paste0(
    ifelse(status_raw$finished, "finished", "unfinished"),
    "_progress", status_raw$progress, "_T", status_raw$tasks)
  completion <- scmix_completion_diagnostics(
    completed_tasks = status_raw$tasks, predictors = predictors,
    completion_pattern = completion_pattern,
    respondent_id = status_raw$respondent_id)
  if (!inherits(completion, "scmix_completion_assessment")) {
    stop("Raw-universe completion diagnostics failed.", call. = FALSE)
  }
  add_table("completion__summary", as.data.frame(as.list(
    completion$summary), stringsAsFactors = FALSE))
  add_table("completion__associations", completion$associations)
  add_table("completion__patterns", completion$completion_patterns)
  completion_ledger <- data.frame(
    component = c("raw completion universe", "early response/assignment",
                  "noninformative completion", "primary estimand"),
    status = c("run_descriptive", "run_descriptive", "not_verified",
               "complete_case_equal_respondent"),
    maintained_assumption_verified = FALSE,
    note = c(
      "1,249 respondents and observed two/three-task completion patterns were audited.",
      "Early responses and exact 13-coordinate assignments were compared by eventual completion.",
      "Descriptive balance and completion models cannot establish noninformative completion.",
      prepared$estimand), stringsAsFactors = FALSE)
  add_table("completion__assessment_ledger", completion_ledger)

  ## Current-producer computation tables are copied; no v1 assessment/QOI,
  ## rank, sensitivity, or post-hoc diagnostic artifact is ingested.
  producer_table_names <- c("candidates.csv", "selections.csv",
                            "reporting_gates.csv")
  for (name in producer_table_names) {
    path <- file.path(producer_dir, name)
    if (!identical(.swv21_md5(path),
                   as.character(snapshot$manifest$artifacts[[name]]))) {
      stop("A producer computation table is stale: ", name,
           call. = FALSE)
    }
    add_table(paste0("computation__", sub("[.]csv$", "", name)),
              utils::read.csv(path, stringsAsFactors = FALSE,
                              check.names = FALSE))
  }

  claims <- data.frame(
    claim = c(
      "reported primary model", "outcome-blind workflow",
      "respondent cross-fitted predictions", "formal structural inference",
      "normal residual shape", "common residual covariance",
      "independent logit shocks", "noninformative completion",
      "exact design-based ordered-contrast benchmark",
      "full three-task response-pattern calibration",
      "prespecified task-pair calibration",
      "exact repeated-contrast calibration", "off-support contests"),
    evidence_state = c(
      paste0("resolved once from completed pointer: ",
             snapshot$reported_primary),
      "false: v2.1 is explicitly post-pilot/outcome-informed",
      paste(
        "run; respondent outer-training construction validated by the v2.1",
        "application contract; package ordinary-inference verified-heldout",
        "flag remains false; post-pilot diagnostic role"
      ),
      "withheld: diagnostic-only DML; no verification record",
      "maintained; not verified", "maintained; not verified",
      "maintained; not verified", "maintained; not verified",
      "withheld: fielded protocol probabilities unavailable",
      joint_status$status[[1L]], joint_status$status[[2L]],
      joint_status$status[[3L]],
      "conditional on advertised support; fielded support not certified"),
    evidence_does_not_establish_assumption = TRUE,
    stringsAsFactors = FALSE)
  add_table("evidence__claims_ledger", claims)

  input_paths <- c(
    producer_manifest = snapshot$manifest_path,
    reported_primary_pointer = snapshot$pointer_path,
    stats::setNames(snapshot$fit_paths,
                    paste0("reported_fit:", names(snapshot$fit_paths))),
    prepared = prepared_path, design_audit = design_audit_path,
    design_task = design_task_path, completion_task = completion_task_path,
    stats::setNames(
      reusable$path,
      paste0("estimator_independent:", basename(reusable$path))),
    postfit_source_paths)
  input_paths <- input_paths[!duplicated(normalizePath(input_paths,
                                                       mustWork = TRUE))]
  input_md5 <- .swv21_hash_paths(input_paths)
  input_manifest <- data.frame(
    role = names(input_paths), path = normalizePath(input_paths,
                                                    mustWork = TRUE),
    bytes = file.info(input_paths)$size,
    md5 = unname(input_md5), stringsAsFactors = FALSE)
  add_table("provenance__input_manifest", input_manifest)
  add_table("provenance__primary_resolution", data.frame(
    reported_primary = snapshot$reported_primary,
    fallback_applied = snapshot$fallback_applied,
    pointer_path = snapshot$pointer_path, pointer_md5 = snapshot$pointer_md5,
    producer_manifest_path = snapshot$manifest_path,
    producer_manifest_md5 = snapshot$manifest_md5,
    pointer_resolved_once = TRUE, formal_inference_available = FALSE,
    outcome_blind = FALSE, stringsAsFactors = FALSE))
  final_gates <- data.frame(
    gate = c(
      "completed producer manifest and all artifacts hash-valid",
      "reported-primary pointer resolved exactly once",
      "chosen full/nested/assembled fit stamps valid",
      "prepared/design/completion artifacts hash-valid",
      "diagnostic DML completed with formal inference withheld",
      "respondent outer-training prediction contract validated",
      "same-sample post-pilot status disclosed",
      "v1 assessment/QOI/sensitivity artifacts not ingested"),
    pass = TRUE,
    detail = c(
      paste(length(snapshot$producer_artifact_paths), "producer artifacts"),
      snapshot$reported_primary, paste(role_prefix, c("full", "nested",
                                                       "assembled"), collapse = "; "),
      paste(nrow(prep_audit) + nrow(design_artifact_audit),
            "estimator-independent artifacts"),
      dml$object$status, crossfit_contract$role,
      sw_v21_postfit_config$assessment_role,
      "Only v2.1 producer artifacts plus estimator-independent preparation/design artifacts were consumed."),
    stringsAsFactors = FALSE)
  add_table("evidence__final_gates", final_gates)

  inventory <- .swv21_table_inventory(table_dir)
  add_table("evidence__table_inventory", inventory)
  evidence <- list(
    schema_version = sw_v21_postfit_config$evidence_schema,
    configuration = sw_v21_postfit_config,
    reported_primary_resolution = snapshot,
    application = prepared$application, sample = prepared$sample,
    estimand = prepared$estimand,
    structural = structural,
    diagnostic_dml = dml,
    prediction = list(
      raw = predictions, assessment = assessment, joint = joint,
      crossfit_contract = crossfit_contract,
      respondent_scores = respondent_score, serial = serial,
      position = position),
    direct_design = list(amce = direct, parallel = compare,
                         status = design_status,
                         conditional_randomization = conditional_randomization),
    completion = list(diagnostics = completion,
                      comparisons = completion_comparison,
                      ledger = completion_ledger),
    tables = tables, final_gates = final_gates,
    formal_inference_available = FALSE, outcome_blind = FALSE,
    production_result = FALSE,
    maintained_assumptions_verified = FALSE,
    posterior_summaries_used = FALSE,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  class(evidence) <- c("sw2022_v21_postfit_evidence", "list")
  .swv21_atomic_save_rds(
    evidence, file.path(object_dir, "evidence_bundle.rds"))
  writeLines(c(
    "# Saha--Weeks v2.1 post-fit evidence bundle",
    "",
    "This is a technical evidence export, not Section 5.1 prose.",
    paste0("Reported primary: `", snapshot$reported_primary, "`."),
    paste0("Fallback applied: `", snapshot$fallback_applied, "`."),
    "Formal inference is unavailable; DML standard errors are diagnostic only.",
    "Cross-fitted predictions are post-pilot diagnostics, not an outcome-blind assessment.",
    "All preparation/design tables are hash-validated estimator-independent artifacts.",
    "No v1 assessment, QOI, rank, sensitivity, or post-hoc diagnostic artifact was ingested."
  ), file.path(staging, "README.md"), useBytes = TRUE)
  writeLines(utils::capture.output(sessionInfo()),
             file.path(manifest_dir, "sessionInfo.txt"), useBytes = TRUE)

  .swv21_assert_resolution_unchanged(snapshot)
  if (!.swv21_same_named_vector(
      .swv21_hash_paths(postfit_source_paths), postfit_source_md5) ||
      !.swv21_same_named_vector(.swv21_hash_paths(input_paths), input_md5)) {
    stop("A post-fit source or input changed during evidence construction.",
         call. = FALSE)
  }
  artifact_paths <- sort(list.files(staging, recursive = TRUE,
                                    full.names = TRUE, all.files = FALSE))
  artifact_paths <- artifact_paths[!dir.exists(artifact_paths)]
  artifact_md5 <- unname(as.character(tools::md5sum(artifact_paths)))
  artifact_manifest <- data.frame(
    path = substring(artifact_paths, nchar(staging) + 2L),
    bytes = file.info(artifact_paths)$size, md5 = artifact_md5,
    stringsAsFactors = FALSE)
  .swv21_write_csv(
    artifact_manifest,
    file.path(manifest_dir, "bundle_artifact_manifest.csv"))
  evidence_manifest <- list(
    schema_version = "sw2022-v2.1-postfit-evidence-manifest-v1",
    evidence_schema = sw_v21_postfit_config$evidence_schema,
    reported_primary = snapshot$reported_primary,
    fallback_applied = snapshot$fallback_applied,
    producer_manifest_md5 = snapshot$manifest_md5,
    reported_primary_pointer_md5 = snapshot$pointer_md5,
    input_md5 = input_md5, artifacts = stats::setNames(
      artifact_manifest$md5, artifact_manifest$path),
    final_gates_pass = all(final_gates$pass),
    formal_inference_available = FALSE, outcome_blind = FALSE,
    production_result = FALSE,
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  .swv21_atomic_save_rds(
    evidence_manifest, file.path(manifest_dir, "evidence_manifest.rds"))

  if (!file.rename(staging, final_dir)) {
    stop("Evidence completed but could not be atomically published from ",
         staging, call. = FALSE)
  }
  cat("Saha--Weeks v2.1 post-fit evidence complete: ", final_dir, "\n",
      sep = "")
  invisible(evidence)
}

if (sys.nframe() == 0L) .main()
