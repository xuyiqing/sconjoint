#!/usr/bin/env Rscript

## Pointer-locked, design-specific Saha--Weeks v2.1 simulated-data stress
## tests. These do not implement empirical alternative likelihoods.

options(stringsAsFactors = FALSE, warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  z <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(z)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", z[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(profile = "validated_fallback", scenarios = "all",
              replications = 0L, force = FALSE)
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
  out$replications <- as.integer(out$replications)
  if (is.na(out$replications) || out$replications < 0L) {
    stop("--replications must be zero or positive.", call. = FALSE)
  }
  out
}

.run_or_load <- function(path, overwrite, code, validator) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    message("checkpoint: loading ", path)
    out <- readRDS(path)
    if (!isTRUE(validator(out))) {
      stop("Stale v2.1 misspecification checkpoint: ", path,
           ". Rerun with --force=true.", call. = FALSE)
    }
    return(out)
  }
  out <- base::force(code)
  .sw_v21_atomic_save(out, path, portable = TRUE)
  message("checkpoint: wrote ", path)
  out
}

.fit_replication_v21 <- function(scenario, replication, prepared, context,
                                 controls, definitions, party, gender,
                                 party_multiplier, orientation,
                                 analysis_config, misspec_config, seed,
                                 v1) {
  dx <- as.matrix(prepared$deltaX); Z <- as.matrix(prepared$Z_primary)
  rid <- as.character(prepared$respondent_id); ids <- unique(rid)
  first <- match(ids, rid); respondent_index <- match(rid, ids)
  mu <- as.matrix(context$full$refit$mu)[first, , drop = FALSE]
  random <- v1$.common_random_numbers(length(ids), nrow(dx), seed)
  simulated <- v1$.simulate_choices(
    scenario, random, mu, orientation$A, context$full$refit$kappa, dx,
    respondent_index, as.integer(prepared$task), party_multiplier,
    misspec_config)
  fit <- .sw_v21_fit_one_fixed(
    dx, simulated$y, Z, rid, train = rep(TRUE, nrow(dx)),
    specification = context$full$selected,
    integration_grid = context$full$refit$integration_grid,
    controls = controls, seed = seed + 500000L,
    role = paste0("misspec_", scenario, "_r", sprintf("%03d", replication)),
    pointer_lock_md5 = context$lock_md5)
  view <- v1$.make_view(fit$mu_all, fit$A, fit$kappa, rid, Z)
  estimate <- v1$.extract_qoi(
    view, definitions, party, gender,
    choice_nodes = analysis_config$inference$choice_nodes)
  bounds <- fit$optimization$bounds %||% list()
  active_names <- intersect(names(bounds),
    c("mu_active", "alpha_active", "kappa_active", "a_active",
      "weight_active"))
  list(
    schema_version = "sw2022-v2.1-misspecification-replication-v1",
    scenario = scenario, replication = as.integer(replication), seed = seed,
    status = if (isTRUE(fit$gate$pass))
      "completed_optimizer_gate_pass" else "completed_optimizer_gate_fail",
    optimization_gate_pass = isTRUE(fit$gate$pass),
    optimization = list(
      objective = fit$optimization$objective,
      gradient_norm = fit$optimization$gradient_norm,
      structural_gradient_norm = fit$optimization$structural_gradient_norm,
      sieve_gradient_norm = fit$optimization$sieve_gradient_norm,
      stop_reason = fit$optimization$stop_reason,
      stage_gate = fit$gate,
      bound_activity = if (length(active_names))
        any(unlist(bounds[active_names])) else NA),
    simulated_choice_rate = mean(simulated$y),
    mean_simulated_probability = mean(simulated$probability),
    latent_realization = simulated$latent, estimate = estimate,
    pointer_lock_md5 = context$lock_md5,
    tuning_repeated = FALSE, posterior_summaries_used = FALSE,
    empirical_alternative_likelihood_fit = FALSE,
    outcome_blind = FALSE, formal_inference_computed = FALSE,
    scope = paste(
      "Design-specific simulated outcomes refit with the pointer-reported",
      "normal q=1 specification; not an empirical alternative-family fit,",
      "identification result, materiality pass, or formal inference."))
}

.safe_replication_v21 <- function(...) {
  tryCatch(.fit_replication_v21(...), error = function(e) list(
    schema_version = "sw2022-v2.1-misspecification-replication-v1",
    status = "failed_captured", error = conditionMessage(e),
    optimization_gate_pass = FALSE,
    empirical_alternative_likelihood_fit = FALSE,
    outcome_blind = FALSE, formal_inference_computed = FALSE))
}

.structural_status <- function(scenarios, objects, B, minimum) {
  sim_row <- function(component, ss) {
    z <- objects[vapply(objects, function(x)
      (x$scenario %||% "") %in% ss, logical(1L))]
    attempted <- length(z); passed <- sum(vapply(z, function(x)
      isTRUE(x$optimization_gate_pass), logical(1L)))
    data.frame(
      component = component,
      status = if (!all(ss %in% scenarios)) "not_run" else if (
        attempted >= minimum * length(ss) && passed == attempted)
        "simulation_executed_optimizer_gates_pass" else
          "simulation_incomplete_or_optimizer_gate_fail",
      attempted_replications = attempted,
      optimizer_gate_passing_replications = passed,
      empirical_alternative_likelihood = FALSE,
      maintained_assumption_verified = FALSE, formal_inference = FALSE,
      outcome_blind = FALSE, stringsAsFactors = FALSE)
  }
  out <- rbind(
    sim_row("normal benchmark", "normal_benchmark"),
    sim_row("skewed standardized factor",
            c("shape_skewed_positive", "shape_skewed_negative")),
    sim_row("symmetric bimodal standardized factor", "shape_bimodal"),
    sim_row("variance-standardized Student t5 factor", "shape_heavy_tail"),
    sim_row("party-varying covariance scale", "covariance_by_party"),
    sim_row("random response scale", "random_scale"),
    sim_row("Gaussian AR1 serial index shock", "serial_shock"))
  not_run <- data.frame(
    component = c(
      "empirical skewed-factor likelihood",
      "empirical bimodal-factor likelihood",
      "empirical Student-t5-factor likelihood",
      "empirical party-varying covariance likelihood",
      "empirical random-response-scale likelihood",
      "empirical AR1 serial-shock likelihood", "profile likelihoods"),
    status = "not_run", attempted_replications = 0L,
    optimizer_gate_passing_replications = 0L,
    empirical_alternative_likelihood = c(rep(TRUE, 6L), FALSE),
    maintained_assumption_verified = FALSE, formal_inference = FALSE,
    outcome_blind = FALSE, stringsAsFactors = FALSE)
  rbind(out, not_run)
}

.main <- function() {
  cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
  script <- .script_file()
  root <- normalizePath(file.path(dirname(script), "../../../../.."),
                        mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  options(sconjoint.sw_application_root = app)
  parent_config_path <- file.path(app, "v2_1", "config",
                                  "analysis_config_v2_1.R")
  source(parent_config_path, local = FALSE)
  sens_config_path <- file.path(dirname(script), "..", "config",
                                "sensitivity_config_v2_1.R")
  misspec_config_path <- file.path(dirname(script), "..", "config",
                                   "misspecification_config_v2_1.R")
  source(sens_config_path, local = FALSE)
  source(misspec_config_path, local = FALSE)
  if (!cli$profile %in% names(sw_v21_misspecification_config$profiles)) {
    stop("Unknown profile: ", cli$profile, call. = FALSE)
  }
  p <- sw_v21_misspecification_config$profiles[[cli$profile]]
  B <- if (cli$replications > 0L) cli$replications else p$replications
  scenarios <- if (cli$scenarios == "all") {
    sw_v21_misspecification_config$scenarios
  } else trimws(strsplit(cli$scenarios, ",", fixed = TRUE)[[1L]])
  unknown <- setdiff(scenarios, sw_v21_misspecification_config$scenarios)
  if (length(unknown)) stop("Unknown scenario(s): ",
                            paste(unknown, collapse = ", "), call. = FALSE)
  contract_path <- file.path(dirname(script),
                             "reported_primary_contract_v2_1.R")
  fit_helper_path <- file.path(dirname(script), "fit_helpers_v2_1.R")
  source(contract_path, local = FALSE); source(fit_helper_path, local = FALSE)
  if (!requireNamespace("pkgload", quietly = TRUE) ||
      !requireNamespace("torch", quietly = TRUE)) {
    stop("Project-local pkgload and torch are required.", call. = FALSE)
  }
  suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
  context <- .sw_v21_validate_reported_primary(
    sw_v21_sensitivity_config$reported_primary_pointer, sw_v21_config,
    load_fits = TRUE)
  controls <- .sw_v21_controls(sw_v21_sensitivity_config, cli$profile)
  controls$n_epochs <- as.integer(p$n_epochs)
  controls$n_starts <- as.integer(p$n_starts)
  analysis_config_path <- file.path(app, "config", "analysis_config.R")
  source(analysis_config_path, local = FALSE)
  v1_runner_path <- file.path(app, "sensitivity",
                              "08_run_misspecification_experiments.R")
  v1_config_path <- sw_v21_misspecification_config$dgp_source_path
  prepared_path <- sw_v21_config$input$prepared
  source_paths <- c(
    runner = script, config = misspec_config_path,
    sensitivity_config = sens_config_path, contract = contract_path,
    fit_helpers = fit_helper_path, parent_config = parent_config_path,
    analysis_config = analysis_config_path, v1_dgp_runner = v1_runner_path,
    v1_dgp_config = v1_config_path, prepared = prepared_path)
  if (any(!file.exists(source_paths))) stop("A simulation input is absent.",
                                            call. = FALSE)
  source_md5 <- .sw_v21_md5(source_paths)
  v1 <- new.env(parent = .GlobalEnv)
  sys.source(v1_runner_path, envir = v1)
  prepared <- readRDS(prepared_path)
  dx <- as.matrix(prepared$deltaX); Z <- as.matrix(prepared$Z_primary)
  rid <- as.character(prepared$respondent_id); ids <- unique(rid)
  first <- match(ids, rid)
  meta <- v1$.map_meta(prepared, ids)
  orientation <- v1$.orient_loading(
    context$full$refit$A, colnames(dx))
  party_multiplier <- v1$.party_multiplier(
    meta$party, sw_v21_misspecification_config)
  definitions <- v1$.qoi_definitions(colnames(dx), sw_analysis_config)
  base_view <- v1$.make_view(
    context$full$refit$mu, orientation$A, context$full$refit$kappa,
    rid, Z, context$full$analysis_signature)
  base_qoi <- v1$.extract_qoi(
    base_view, definitions, meta$party, meta$gender,
    sw_analysis_config$inference$choice_nodes)
  mu <- as.matrix(context$full$refit$mu)[first, , drop = FALSE]

  out_dir <- file.path(sw_v21_sensitivity_config$output_root, cli$profile,
                       "misspecification")
  truth_dir <- file.path(out_dir, "truth")
  refit_dir <- file.path(out_dir, "refits")
  table_dir <- file.path(out_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- list(
    schema_version = sw_v21_misspecification_config$schema_version,
    config_version = sw_v21_misspecification_config$version,
    profile = cli$profile, controls = controls, B = B,
    truth_draws = p$truth_draws,
    reported_primary = context$pointer$reported_primary,
    pointer_lock_md5 = context$lock_md5, source_md5 = source_md5,
    dgp_seed = sw_v21_misspecification_config$seed,
    outcome_blind = FALSE, formal_inference_available = FALSE)
  truths <- list()
  for (scenario in scenarios) {
    path <- file.path(truth_dir, paste0(scenario, ".rds"))
    truths[[scenario]] <- .run_or_load(
      path, cli$force,
      {
        z <- v1$.scenario_truth(
          scenario, p$truth_draws, base_qoi, mu, orientation$A,
          context$full$refit$kappa, definitions, party_multiplier,
          sw_v21_misspecification_config)
        z$sw_v21_misspecification_specification <-
          c(stamp, list(component = "truth", scenario = scenario))
        z$outcome_blind <- FALSE; z$formal_inference_available <- FALSE
        z
      },
      function(x) identical(
        x$sw_v21_misspecification_specification,
        c(stamp, list(component = "truth", scenario = scenario))))
  }
  objects <- list()
  for (scenario in scenarios) {
    scenario_index <- match(
      scenario, sw_v21_misspecification_config$scenarios)
    for (b in seq_len(B)) {
      seed <- sw_v21_misspecification_config$seed +
        100000L * scenario_index + b
      path <- file.path(refit_dir, scenario,
                        paste0("rep_", sprintf("%03d", b), ".rds"))
      spec <- c(stamp, list(component = "replication", scenario = scenario,
                            replication = b, seed = seed))
      obj <- .run_or_load(
        path, cli$force,
        {
          z <- .safe_replication_v21(
            scenario, b, prepared, context, controls, definitions,
            meta$party, meta$gender, party_multiplier, orientation,
            sw_analysis_config, sw_v21_misspecification_config, seed, v1)
          z$scenario <- scenario; z$replication <- b; z$seed <- seed
          z$sw_v21_misspecification_specification <- spec
          z
        },
        function(x) identical(x$sw_v21_misspecification_specification, spec))
      objects[[length(objects) + 1L]] <- obj
    }
  }
  tables <- v1$.replication_tables(objects, truths)
  summary <- v1$.summarize_qoi(tables$qoi)
  calibration <- do.call(rbind, lapply(truths, `[[`, "calibration"))
  refinement <- do.call(rbind, lapply(truths, function(x) {
    cbind(scenario = x$scenario, x$truth_refinement,
          stringsAsFactors = FALSE)
  }))
  truth_summary <- data.frame(
    scenario = names(truths),
    truth_draws = vapply(truths, `[[`, integer(1L), "truth_draws"),
    max_truth_refinement_difference = vapply(
      truths, `[[`, numeric(1L), "max_truth_refinement_difference"),
    stringsAsFactors = FALSE)
  .sw_v21_write_csv(tables$qoi, file.path(table_dir, "replication_qoi.csv"))
  .sw_v21_write_csv(tables$optimization,
                    file.path(table_dir, "replication_optimization.csv"))
  .sw_v21_write_csv(summary, file.path(table_dir, "qoi_bias_stability.csv"))
  .sw_v21_write_csv(calibration, file.path(table_dir, "dgp_calibration.csv"))
  .sw_v21_write_csv(refinement, file.path(table_dir, "truth_refinement.csv"))
  .sw_v21_write_csv(truth_summary,
                    file.path(table_dir, "truth_resolution.csv"))
  coverage <- data.frame(
    coverage_evaluated = FALSE, nominal_coverage = NA_real_,
    formal_inference_available = FALSE, oracle_interval_substituted = FALSE,
    status = sw_v21_misspecification_config$coverage$reason)
  .sw_v21_write_csv(coverage, file.path(table_dir, "coverage_status.csv"))
  status <- .structural_status(scenarios, objects, B,
                               p$minimum_defensible_replications)
  .sw_v21_write_csv(status,
                    file.path(table_dir, "structural_component_status.csv"))
  all_scenarios <- setequal(scenarios,
                            sw_v21_misspecification_config$scenarios)
  all_gates <- length(objects) == B * length(scenarios) &&
    all(vapply(objects, function(x) isTRUE(x$optimization_gate_pass),
               logical(1L)))
  minimum_met <- B >= p$minimum_defensible_replications
  get_calibration <- function(s) {
    calibration[calibration$scenario == s, , drop = FALSE]
  }
  calibration_gate <- all_scenarios &&
    get_calibration("shape_skewed_positive")$factor_skewness > 1 &&
    get_calibration("shape_skewed_negative")$factor_skewness < -1 &&
    get_calibration("shape_bimodal")$factor_excess_kurtosis < -0.5 &&
    get_calibration("shape_heavy_tail")$factor_excess_kurtosis > 1 &&
    abs(get_calibration("covariance_by_party")$
          party_multiplier_mean_square - 1) < 1e-12 &&
    abs(get_calibration("random_scale")$random_scale_mean - 1) < 1e-12
  truth_refinement_gate <- all_scenarios &&
    all(is.finite(truth_summary$max_truth_refinement_difference)) &&
    all(truth_summary$max_truth_refinement_difference < 0.005)
  random_scale <- tables$qoi[
    tables$qoi$scenario == "random_scale", , drop = FALSE]
  random_scale_comparability_gate <- nrow(random_scale) > 0L &&
    all(random_scale$comparable ==
          grepl("^(choice|sign):", random_scale$quantity))
  simulation_validation_pass <- all_scenarios && all_gates && minimum_met &&
    calibration_gate && truth_refinement_gate &&
    random_scale_comparability_gate
  bundle <- list(
    schema_version = "sw2022-v2.1-design-misspecification-results-v1",
    profile = cli$profile, requested_scenarios = scenarios,
    requested_replications = B, minimum_replications_met = minimum_met,
    complete_scenario_battery = all_scenarios,
    calibration_gate = calibration_gate,
    truth_refinement_gate = truth_refinement_gate,
    random_scale_comparability_gate = random_scale_comparability_gate,
    simulation_validation_pass = simulation_validation_pass,
    config = sw_v21_misspecification_config,
    factor_orientation = orientation,
    party_multiplier = stats::setNames(party_multiplier, ids),
    truths = truths, replications = objects, qoi = tables$qoi,
    optimization = tables$optimization, qoi_summary = summary,
    calibration = calibration, truth_summary = truth_summary,
    coverage = coverage, structural_status = status,
    reported_primary = context$pointer$reported_primary,
    pointer_path = context$pointer_path, pointer_lock_md5 = context$lock_md5,
    source_paths = source_paths, source_md5 = source_md5,
    primary_artifacts_modified = FALSE, posterior_summaries_used = FALSE,
    empirical_alternative_likelihoods = "not_run",
    profile_likelihoods = "not_run", maintained_assumptions_verified = FALSE,
    outcome_blind = FALSE, formal_inference_available = FALSE)
  .sw_v21_atomic_save(bundle,
    file.path(out_dir, "misspecification_results.rds"), portable = TRUE)
  context_after <- .sw_v21_validate_reported_primary(
    sw_v21_sensitivity_config$reported_primary_pointer, sw_v21_config,
    load_fits = FALSE)
  if (!.sw_v21_same_md5(context$lock_md5, context_after$lock_md5)) {
    stop("The reported-primary lock changed during simulation execution.",
         call. = FALSE)
  }
  capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))
  artifacts <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  artifacts <- artifacts[!file.info(artifacts)$isdir &
    !basename(artifacts) %in% c("manifest.rds", "validation.rds")]
  manifest <- list(
    schema_version = "sw2022-v2.1-design-misspecification-manifest-v1",
    profile = cli$profile, scenarios = scenarios, replications = B,
    minimum_defensible_replications = p$minimum_defensible_replications,
    complete_scenario_battery = all_scenarios,
    all_optimizer_gates_pass = all_gates,
    calibration_gate = calibration_gate,
    truth_refinement_gate = truth_refinement_gate,
    random_scale_comparability_gate = random_scale_comparability_gate,
    simulation_validation_pass = simulation_validation_pass,
    reported_primary = context$pointer$reported_primary,
    pointer_path = context$pointer_path, pointer_lock_md5 = context$lock_md5,
    source_paths = source_paths, source_md5 = source_md5,
    artifact_md5 = stats::setNames(unname(tools::md5sum(artifacts)),
      sub(paste0("^", out_dir, "/"), "", artifacts)),
    dgp_definitions_reused_exactly = TRUE,
    scenario_order_reused_exactly = TRUE, seed_reused_exactly = TRUE,
    primary_artifacts_modified = FALSE,
    empirical_alternative_likelihoods = "not_run",
    profile_likelihoods = "not_run", coverage_evaluated = FALSE,
    materiality_pass_issued = FALSE, maintained_assumptions_verified = FALSE,
    outcome_blind = FALSE, formal_inference_available = FALSE,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  .sw_v21_atomic_save(manifest, file.path(out_dir, "manifest.rds"),
                      portable = FALSE)
  message("Saha--Weeks v2.1 misspecification stage complete: ", out_dir)
  invisible(manifest)
}

if (sys.nframe() == 0L) .main()
