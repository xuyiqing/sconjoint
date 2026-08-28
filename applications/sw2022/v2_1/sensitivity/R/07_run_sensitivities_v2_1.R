#!/usr/bin/env Rscript

## Pointer-locked Saha--Weeks v2.1 application sensitivities.
## Descriptive and outcome-informed; formal inference is unavailable.

options(stringsAsFactors = FALSE, warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  z <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(z)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", z[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(profile = "validated_fallback", stage = "all", force = FALSE)
  for (arg in x) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg,
                                        call. = FALSE)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", bits[[1L]], fixed = TRUE)
    if (!key %in% names(out)) stop("Unknown argument --", bits[[1L]],
                                    call. = FALSE)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out
}

.run_or_load <- function(path, overwrite, code, validator) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    message("checkpoint: loading ", path)
    out <- readRDS(path)
    if (!isTRUE(validator(out))) {
      stop("Stale v2.1 sensitivity checkpoint: ", path,
           ". Rerun with --force=true.", call. = FALSE)
    }
    return(out)
  }
  out <- base::force(code)
  .sw_v21_atomic_save(out, path, portable = TRUE)
  message("checkpoint: wrote ", path)
  out
}

.choice_table <- function(view, contests, prefix = "") {
  value <- vapply(names(contests), function(nm) {
    sconjoint::scmix_paper_choice(
      view, contrast = contests[[nm]], position_neutral = TRUE,
      n_nodes = 45L, on_support = NA)$estimate
  }, numeric(1L))
  data.frame(
    quantity = paste0(prefix, names(value)), estimate = value,
    support = "conditional on advertised support; protocol unverified",
    stringsAsFactors = FALSE)
}

.fit_z19 <- function(prepared, context, controls, analysis_config, seed) {
  fit <- .sw_v21_fit_fixed_nested(
    prepared$deltaX, prepared$y, prepared$Z_sensitivity19_raw,
    prepared$respondent_id, prepared$task, context, controls, seed,
    role = "postconjoint_19Z")
  paired <- .sw_v21_score_comparison(
    context$assembled, fit$assembled, prepared$task,
    c("reported_primary_15Z", "postconjoint_19Z"))
  primary_view <- .sw_v21_primary_view(context, prepared)
  theta0 <- sconjoint::scmix_paper_theta(primary_view)$estimate
  theta1 <- sconjoint::scmix_paper_theta(fit$full_fit)$estimate
  theta <- data.frame(
    coordinate = names(theta0), reported_primary_15Z = as.numeric(theta0),
    postconjoint_19Z = as.numeric(theta1[names(theta0)]),
    difference = as.numeric(theta1[names(theta0)] - theta0),
    stringsAsFactors = FALSE)
  c0 <- .choice_table(primary_view, analysis_config$qoi$contests)
  c1 <- .choice_table(fit$full_fit, analysis_config$qoi$contests)
  choice <- merge(c0[, c("quantity", "estimate")],
                  c1[, c("quantity", "estimate")], by = "quantity",
                  suffixes = c("_reported_primary_15Z", "_postconjoint_19Z"))
  choice$difference <- choice$estimate_postconjoint_19Z -
    choice$estimate_reported_primary_15Z
  list(
    schema_version = "sw2022-v2.1-z19-sensitivity-v1", fit = fit,
    score_comparison = paired$score, predictions = paired,
    theta_comparison = theta, choice_comparison = choice,
    missing_task_rows = colSums(is.na(prepared$Z_sensitivity19_raw)),
    timing_sensitive = TRUE, materially_sensitive = NA,
    interpretation = paste(
      "Post-conjoint fields remain excluded from the reported primary.",
      "This pointer-locked perturbation has no formal intervals or approved",
      "materiality threshold."),
    pointer_lock_md5 = context$lock_md5, outcome_blind = FALSE,
    formal_inference_available = FALSE, maintained_model = FALSE)
}

.fit_interaction <- function(prepared, design, context, controls, seed,
                             helper) {
  gender <- match("cand_genderMale", colnames(prepared$deltaX))
  run <- match("cand_runYes", colnames(prepared$deltaX))
  if (anyNA(c(gender, run))) stop("Male/run coordinates are absent.",
                                  call. = FALSE)
  interaction <- prepared$Xa[, gender] * prepared$Xa[, run] -
    prepared$Xb[, gender] * prepared$Xb[, run]
  dx <- cbind(prepared$deltaX, male_x_prior_run = interaction)
  fit <- .sw_v21_fit_fixed_nested(
    dx, prepared$y, prepared$Z_primary, prepared$respondent_id,
    prepared$task, context, controls, seed, role = "male_x_prior_run")
  paired <- .sw_v21_score_comparison(
    context$assembled, fit$assembled, prepared$task,
    c("reported_primary_additive", "male_x_prior_run"))
  theta <- sconjoint::scmix_paper_theta(fit$full_fit)$estimate
  interaction_name <- "male_x_prior_run"
  effects <- c(
    male_effect_when_no_prior_run = theta[["cand_genderMale"]],
    male_effect_when_prior_run = theta[["cand_genderMale"]] +
      theta[[interaction_name]],
    prior_run_effect_for_female = theta[["cand_runYes"]],
    prior_run_effect_for_male = theta[["cand_runYes"]] +
      theta[[interaction_name]],
    male_by_prior_run_difference_in_differences = theta[[interaction_name]])
  effects <- data.frame(
    quantity = names(effects), estimate = as.numeric(effects),
    formal_interval = "withheld", stringsAsFactors = FALSE)
  d <- list(
    male_no_run_vs_female_no_run = c(1, 0, rep(0, 12)),
    female_run_vs_female_no_run = c(0, 1, rep(0, 12)),
    male_run_vs_female_no_run = c(1, 1, rep(0, 11), 1))
  choices <- .choice_table(fit$full_fit, d)
  names(choices)[names(choices) == "estimate"] <-
    "interaction_fit_probability"
  choices$probability_difference_in_differences <- NA_real_
  choices$probability_difference_in_differences[1L] <-
    choices$interaction_fit_probability[
      choices$quantity == "male_run_vs_female_no_run"] -
    choices$interaction_fit_probability[
      choices$quantity == "male_no_run_vs_female_no_run"] -
    choices$interaction_fit_probability[
      choices$quantity == "female_run_vs_female_no_run"] + 0.5
  list(
    schema_version = "sw2022-v2.1-male-run-sensitivity-v1", fit = fit,
    score_comparison = paired$score, predictions = paired,
    conditional_effects = effects, choice_probabilities = choices,
    design_audit = helper$.interaction_design_audit(prepared, design),
    feature_definition = paste(
      "[Male_A * PriorRun_A] - [Male_B * PriorRun_B]; all other coordinates",
      "retain the frozen reference coding."),
    identification_established = FALSE,
    pointer_lock_md5 = context$lock_md5, outcome_blind = FALSE,
    formal_inference_available = FALSE, maintained_model = FALSE)
}

.fit_process <- function(prepared, context, controls, seed, helper) {
  predictions <- sconjoint::scmix_heldout_predictions(
    context$assembled, task_order = prepared$task, include_counts = TRUE,
    include_adjacent = TRUE, include_repeated = TRUE)
  tables <- helper$.serial_order_tables(predictions, context$full)
  swap <- .sw_v21_fit_one_fixed(
    dx = -as.matrix(prepared$deltaX), y = 1 - prepared$y,
    Z_raw = prepared$Z_primary, rid = prepared$respondent_id,
    train = rep(TRUE, length(prepared$y)),
    specification = context$full$selected,
    integration_grid = context$full$refit$integration_grid,
    controls = controls, seed = seed, role = "profile_AB_swap_full",
    pointer_lock_md5 = context$lock_md5)
  first <- !duplicated(prepared$respondent_id)
  theta_primary <- colMeans(context$full$refit$mu[first, , drop = FALSE])
  theta_swap <- colMeans(swap$mu_all[first, , drop = FALSE])
  swap_summary <- data.frame(
    diagnostic = c("kappa sign reversal gap", "maximum theta gap",
                   "Sigma Frobenius gap", "swap optimization gate"),
    value = c(swap$kappa + context$full$refit$kappa,
              max(abs(theta_swap - theta_primary)),
              sqrt(sum((swap$Sigma - context$full$refit$Sigma)^2)),
              as.numeric(isTRUE(swap$gate$pass))),
    expected_under_exact_equivariance = c(0, 0, 0, 1),
    interpretation = c(
      rep("Full-sample A/B relabeling replication; no formal test.", 3L),
      "One means every attained-state optimizer/nesting/bound stage passed."),
    stringsAsFactors = FALSE)
  list(
    schema_version = "sw2022-v2.1-task-process-sensitivity-v1",
    heldout_predictions = predictions, tables = tables,
    profile_swap_fit = swap, profile_swap_summary = swap_summary,
    task_process_alternative_likelihood = "not_run",
    serial_shock_alternative_likelihood = "not_run",
    interpretation = paste(
      "Task-order, residual, and adjacent-pattern checks are held-out",
      "diagnostics. The A/B swap is an optimization/equivariance replication;",
      "neither implements an empirical task-process or serial-shock likelihood."),
    pointer_lock_md5 = context$lock_md5, outcome_blind = FALSE,
    formal_inference_available = FALSE, maintained_model = FALSE)
}

.completion_view <- function(fit, rows, expanded, Z0) {
  list(
    respondent_id = expanded$respondent_id[rows],
    Z = Z0[rows, , drop = FALSE], attr_names = colnames(expanded$deltaX),
    full_fit = list(mu = fit$mu_all[rows, , drop = FALSE], A = fit$A,
                    Sigma = fit$Sigma, kappa = fit$kappa))
}

.fit_completion <- function(prepared, design, context, controls,
                            analysis_config, seed, helper) {
  expanded <- helper$.raw_expanded_matrices(prepared)
  primary_keep <- expanded$respondent_id %in% unique(prepared$respondent_id)
  if (sum(primary_keep) != nrow(prepared$deltaX) ||
      length(unique(expanded$respondent_id)) != 1249L) {
    stop("Expanded completion universe does not reproduce 1,249 respondents.",
         call. = FALSE)
  }
  Z0 <- matrix(0, nrow(expanded$deltaX), 1L,
               dimnames = list(NULL, "no_moderators"))
  fit_primary <- .sw_v21_fit_one_fixed(
    expanded$deltaX[primary_keep, , drop = FALSE], expanded$y[primary_keep],
    Z0[primary_keep, , drop = FALSE], expanded$respondent_id[primary_keep],
    train = rep(TRUE, sum(primary_keep)),
    specification = context$full$selected,
    integration_grid = context$full$refit$integration_grid,
    controls = controls, seed = seed, role = "completion_1191_noZ",
    pointer_lock_md5 = context$lock_md5)
  fit_expanded <- .sw_v21_fit_one_fixed(
    expanded$deltaX, expanded$y, Z0, expanded$respondent_id,
    train = rep(TRUE, nrow(expanded$deltaX)),
    specification = context$full$selected,
    integration_grid = context$full$refit$integration_grid,
    controls = controls, seed = seed + 10000L,
    role = "completion_1249_noZ", pointer_lock_md5 = context$lock_md5)
  vp <- list(
    respondent_id = expanded$respondent_id[primary_keep],
    Z = Z0[primary_keep, , drop = FALSE],
    attr_names = colnames(expanded$deltaX),
    full_fit = list(mu = fit_primary$mu_all, A = fit_primary$A,
                    Sigma = fit_primary$Sigma, kappa = fit_primary$kappa))
  ve <- .completion_view(fit_expanded, rep(TRUE, nrow(expanded$deltaX)),
                         expanded, Z0)
  theta_p <- sconjoint::scmix_paper_theta(vp)$estimate
  theta_e <- sconjoint::scmix_paper_theta(ve)$estimate
  theta <- data.frame(
    coordinate = names(theta_p), primary_1191 = as.numeric(theta_p),
    expanded_1249 = as.numeric(theta_e),
    difference = as.numeric(theta_e - theta_p), stringsAsFactors = FALSE)
  cp <- .choice_table(vp, analysis_config$qoi$contests)
  ce <- .choice_table(ve, analysis_config$qoi$contests)
  choices <- merge(cp[, c("quantity", "estimate")],
                   ce[, c("quantity", "estimate")], by = "quantity",
                   suffixes = c("_primary1191", "_expanded1249"))
  choices$difference <- choices$estimate_expanded1249 -
    choices$estimate_primary1191
  amce <- rbind(
    helper$.amce_raw(expanded$deltaX[primary_keep, , drop = FALSE],
                     expanded$y[primary_keep],
                     expanded$respondent_id[primary_keep], "primary_1191"),
    helper$.amce_raw(expanded$deltaX, expanded$y, expanded$respondent_id,
                     "expanded_1249"))
  status <- design$completion$status
  task1 <- expanded$task == 1L
  s1 <- status[match(expanded$respondent_id[task1], status$respondent_id), ]
  group <- ifelse(s1$tasks == 2L, "two_tasks_unfinished",
    ifelse(s1$final_analysis_sample, "primary_complete_case",
      ifelse(s1$finished, "three_tasks_finished_excluded",
             "three_tasks_unfinished")))
  early <- do.call(rbind, lapply(unique(group), function(g) {
    take <- group == g; z <- expanded$y[task1][take]
    data.frame(
      eventual_completion_group = g,
      task1_candidate_A_choice_rate = mean(z),
      descriptive_se = stats::sd(z) / sqrt(length(z)),
      respondents = length(z), stringsAsFactors = FALSE)
  }))
  status_all <- status[match(expanded$respondent_id, status$respondent_id), ]
  features <- cbind(choice_A = expanded$y, expanded$deltaX)
  early_balance <- do.call(rbind, lapply(c(1L, 2L), function(t) {
    rows <- expanded$task == t
    do.call(rbind, lapply(seq_len(ncol(features)), function(j) {
      value <- features[rows, j]; completed <- status_all$tasks[rows]
      x2 <- value[completed == 2L]; x3 <- value[completed == 3L]
      data.frame(
        task = t, feature = colnames(features)[j],
        mean_eventual_T2 = mean(x2), mean_eventual_T3 = mean(x3),
        difference_T2_minus_T3 = mean(x2) - mean(x3),
        descriptive_se = sqrt(stats::var(x2) / length(x2) +
                                stats::var(x3) / length(x3)),
        respondents_T2 = length(x2), respondents_T3 = length(x3),
        stringsAsFactors = FALSE)
    }))
  }))
  list(
    schema_version = "sw2022-v2.1-completion-sensitivity-v1",
    primary_noZ_fit = fit_primary, expanded_noZ_fit = fit_expanded,
    theta_comparison = theta, choice_comparison = choices,
    amce_comparison = amce, early_task_by_eventual_completion = early,
    early_assignment_response_balance = early_balance,
    sample = data.frame(
      sample = c("primary", "expanded"), respondents = c(1191L, 1249L),
      tasks = c(sum(primary_keep), nrow(expanded$deltaX))),
    source_path = expanded$source_path, source_md5 = expanded$source_md5,
    source_policy = "read-only",
    optimizer_gate = c(primary = fit_primary$gate$pass,
                       expanded = fit_expanded$gate$pass),
    completion_independence_verified = FALSE,
    pointer_lock_md5 = context$lock_md5, outcome_blind = FALSE,
    formal_inference_available = FALSE, maintained_model = FALSE)
}

.export_component <- function(name, x, table_dir) {
  if (name == "z19") {
    .sw_v21_write_csv(x$theta_comparison, file.path(table_dir, "z19_theta.csv"))
    .sw_v21_write_csv(x$choice_comparison, file.path(table_dir, "z19_choices.csv"))
    .sw_v21_write_csv(x$score_comparison$paired_differences,
      file.path(table_dir, "z19_heldout_score_difference.csv"))
  } else if (name == "interaction") {
    .sw_v21_write_csv(x$conditional_effects,
      file.path(table_dir, "male_run_conditional_effects.csv"))
    .sw_v21_write_csv(x$choice_probabilities,
      file.path(table_dir, "male_run_choice_probabilities.csv"))
    .sw_v21_write_csv(x$design_audit,
      file.path(table_dir, "male_run_design_audit.csv"))
    .sw_v21_write_csv(x$score_comparison$paired_differences,
      file.path(table_dir, "male_run_heldout_score_difference.csv"))
  } else if (name == "process") {
    .sw_v21_write_csv(x$tables$by_task,
      file.path(table_dir, "task_order_calibration.csv"))
    .sw_v21_write_csv(x$tables$serial,
      file.path(table_dir, "serial_residual_diagnostics.csv"))
    .sw_v21_write_csv(x$tables$transition,
      file.path(table_dir, "adjacent_transition_calibration.csv"))
    .sw_v21_write_csv(x$tables$position,
      file.path(table_dir, "position_diagnostics.csv"))
    .sw_v21_write_csv(x$profile_swap_summary,
      file.path(table_dir, "position_profile_swap.csv"))
  } else if (name == "completion") {
    for (nm in c("sample", "theta_comparison", "choice_comparison",
                 "amce_comparison", "early_task_by_eventual_completion",
                 "early_assignment_response_balance")) {
      .sw_v21_write_csv(x[[nm]], file.path(table_dir,
        paste0("completion_", nm, ".csv")))
    }
  }
}

.status_table <- function(objects) {
  has <- function(x) !is.null(objects[[x]])
  row <- function(component, status, note) data.frame(
    component = component, status = status, note = note,
    maintained_assumption_verified = FALSE, formal_inference = FALSE,
    outcome_blind = FALSE, stringsAsFactors = FALSE)
  rbind(
    row("post-conjoint 19-Z moderators",
        if (has("z19")) "run_descriptive_sensitivity" else "not_run",
        "Post-conjoint variables remain outside the reported primary."),
    row("Male x prior-run interaction",
        if (has("interaction")) "run_descriptive_sensitivity" else "not_run",
        "Augmented basis; protocol support is not document-verified."),
    row("task/order diagnostics",
        if (has("process")) "run_heldout_diagnostic" else "not_run",
        "Task-varying empirical likelihood remains not_run."),
    row("serial diagnostics",
        if (has("process")) "run_heldout_diagnostic" else "not_run",
        "Empirical serial-shock likelihood remains not_run."),
    row("A/B position-profile swap",
        if (!has("process")) "not_run" else if (
          isTRUE(objects$process$profile_swap_fit$gate$pass))
          "run_optimization_replication" else "run_failed_optimizer_gate",
        "Equivariance/optimization replication; no formal test."),
    row("completion 1191 vs 1249 no-Z",
        if (!has("completion")) "not_run" else if (
          all(objects$completion$optimizer_gate))
          "run_descriptive_sensitivity" else "run_failed_optimizer_gate",
        "Does not verify noninformative completion."),
    row("empirical alternative likelihoods", "not_run",
        "Skew/bimodal/t5/covariance-scale/random-scale/AR1 empirical refits are not implemented."),
    row("profile likelihoods", "not_run",
        "No nuisance-reoptimized likelihood profiles were implemented."))
}

.main <- function() {
  cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
  allowed_stage <- c("all", "z19", "interaction", "process", "completion")
  if (!cli$stage %in% allowed_stage) stop("Unknown --stage.", call. = FALSE)
  script <- .script_file()
  root <- normalizePath(file.path(dirname(script), "../../../../.."),
                        mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  options(sconjoint.sw_application_root = app)
  config_path <- file.path(app, "v2_1", "config",
                           "analysis_config_v2_1.R")
  source(config_path, local = FALSE)
  sens_config_path <- file.path(dirname(script), "..", "config",
                                "sensitivity_config_v2_1.R")
  source(sens_config_path, local = FALSE)
  if (!cli$profile %in% names(sw_v21_sensitivity_config$profiles)) {
    stop("Unknown profile: ", cli$profile, call. = FALSE)
  }
  contract_path <- file.path(dirname(script),
                             "reported_primary_contract_v2_1.R")
  helper_path <- file.path(dirname(script), "fit_helpers_v2_1.R")
  source(contract_path, local = FALSE); source(helper_path, local = FALSE)
  if (!requireNamespace("pkgload", quietly = TRUE) ||
      !requireNamespace("torch", quietly = TRUE)) {
    stop("Project-local pkgload and torch are required.", call. = FALSE)
  }
  suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
  context <- .sw_v21_validate_reported_primary(
    sw_v21_sensitivity_config$reported_primary_pointer, sw_v21_config,
    load_fits = TRUE)
  controls <- .sw_v21_controls(sw_v21_sensitivity_config, cli$profile)
  analysis_config_path <- file.path(app, "config", "analysis_config.R")
  source(analysis_config_path, local = FALSE)
  prepared_path <- sw_v21_config$input$prepared
  design_path <- file.path(app, "results", "design_completion_audit.rds")
  v1_helper_path <- file.path(app, "sensitivity", "07_run_sensitivities.R")
  source_paths <- c(
    runner = script, config = sens_config_path, contract = contract_path,
    fit_helpers = helper_path, parent_config = config_path,
    analysis_config = analysis_config_path, v1_helper = v1_helper_path,
    prepared = prepared_path, design = design_path)
  if (any(!file.exists(source_paths))) stop("A sensitivity input is absent.",
                                            call. = FALSE)
  source_md5 <- .sw_v21_md5(source_paths)
  prepared <- readRDS(prepared_path); design <- readRDS(design_path)
  v1_helper <- new.env(parent = .GlobalEnv)
  sys.source(v1_helper_path, envir = v1_helper)
  out_dir <- file.path(sw_v21_sensitivity_config$output_root, cli$profile,
                       "application")
  table_dir <- file.path(out_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- list(
    schema_version = sw_v21_sensitivity_config$schema_version,
    config_version = sw_v21_sensitivity_config$version,
    profile = cli$profile, controls = controls,
    reported_primary = context$pointer$reported_primary,
    pointer_lock_md5 = context$lock_md5,
    source_md5 = source_md5, outcome_blind = FALSE,
    formal_inference_available = FALSE)
  stamp_one <- function(x, component) {
    x$sw_v21_sensitivity_specification <- c(stamp, list(component = component))
    x
  }
  valid_one <- function(x, component) {
    ok <- identical(x$sw_v21_sensitivity_specification,
                    c(stamp, list(component = component)))
    if (ok && identical(component, "completion")) {
      ok <- is.character(x$source_path) && length(x$source_path) == 1L &&
        file.exists(x$source_path) &&
        identical(unname(tools::md5sum(x$source_path)), x$source_md5)
    }
    ok
  }
  paths <- c(
    z19 = file.path(out_dir, "fit_z19_sensitivity.rds"),
    interaction = file.path(out_dir, "fit_male_run_interaction.rds"),
    process = file.path(out_dir, "task_process_diagnostics.rds"),
    completion = file.path(out_dir, "completion_sample_sensitivity.rds"))
  requested <- if (cli$stage == "all") names(paths) else cli$stage
  objects <- stats::setNames(vector("list", length(paths)), names(paths))
  seed <- sw_v21_sensitivity_config$seed
  for (nm in names(paths)) {
    if (nm %in% requested) {
      objects[[nm]] <- .run_or_load(
        paths[[nm]], cli$force, stamp_one(switch(nm,
          z19 = .fit_z19(prepared, context, controls, sw_analysis_config,
                         seed + 10000L),
          interaction = .fit_interaction(
            prepared, design, context, controls, seed + 20000L, v1_helper),
          process = .fit_process(
            prepared, context, controls, seed + 30000L, v1_helper),
          completion = .fit_completion(
            prepared, design, context, controls, sw_analysis_config,
            seed + 40000L, v1_helper)), nm),
        function(x) valid_one(x, nm))
    } else if (file.exists(paths[[nm]])) {
      z <- readRDS(paths[[nm]])
      if (valid_one(z, nm)) objects[[nm]] <- z
    }
    if (!is.null(objects[[nm]])) .export_component(nm, objects[[nm]], table_dir)
  }
  status <- .status_table(objects)
  .sw_v21_write_csv(status, file.path(table_dir, "application_status.csv"))
  alternative <- data.frame(
    component = names(sw_v21_sensitivity_config$empirical_alternative_likelihoods),
    status = unlist(sw_v21_sensitivity_config$empirical_alternative_likelihoods),
    implemented = FALSE, empirical_refit = FALSE,
    note = "Design-specific simulations, if run, are not empirical alternative likelihoods.")
  .sw_v21_write_csv(alternative,
                    file.path(table_dir, "empirical_alternative_likelihood_status.csv"))
  profile_status <- data.frame(
    target = c("kappa", "female-vs-male preference",
               "Male x prior-run interaction", "smallest covariance eigenvalue",
               names(sw_analysis_config$qoi$contests)),
    status = "not_run", verified_profile = FALSE,
    note = paste(
      "No nuisance reoptimization at fixed target values was implemented;",
      "a likelihood slice is not relabeled as a profile."))
  .sw_v21_write_csv(profile_status,
                    file.path(table_dir, "profile_likelihood_status.csv"))
  context_after <- .sw_v21_validate_reported_primary(
    sw_v21_sensitivity_config$reported_primary_pointer, sw_v21_config,
    load_fits = FALSE)
  if (!.sw_v21_same_md5(context$lock_md5, context_after$lock_md5)) {
    stop("The reported-primary lock changed during sensitivity execution.",
         call. = FALSE)
  }
  capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))
  artifacts <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  artifacts <- artifacts[!file.info(artifacts)$isdir &
    !basename(artifacts) %in% c("manifest.rds", "validation.rds")]
  manifest <- list(
    schema_version = "sw2022-v2.1-application-sensitivity-manifest-v1",
    profile = cli$profile, stage = cli$stage,
    completed_components = names(objects)[!vapply(objects, is.null, logical(1L))],
    complete_battery = all(!vapply(objects, is.null, logical(1L))),
    reported_primary = context$pointer$reported_primary,
    pointer_path = context$pointer_path, pointer_lock_md5 = context$lock_md5,
    source_paths = source_paths, source_md5 = source_md5,
    completion_raw_path = objects$completion$source_path %||% NA_character_,
    completion_raw_md5 = objects$completion$source_md5 %||% NA_character_,
    artifact_md5 = stats::setNames(unname(tools::md5sum(artifacts)),
      sub(paste0("^", out_dir, "/"), "", artifacts)),
    primary_artifacts_modified = FALSE, maintained_assumptions_verified = FALSE,
    outcome_blind = FALSE, formal_inference_available = FALSE,
    empirical_alternative_likelihoods = "not_run",
    profile_likelihoods = "not_run",
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  .sw_v21_atomic_save(manifest, file.path(out_dir, "manifest.rds"),
                      portable = FALSE)
  message("Saha--Weeks v2.1 application sensitivity stage complete: ", out_dir)
  invisible(manifest)
}

if (sys.nframe() == 0L) .main()
