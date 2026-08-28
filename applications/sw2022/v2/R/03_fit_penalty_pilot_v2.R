#!/usr/bin/env Rscript

## Version-2 Saha--Weeks penalty/tuning diagnostic.
##
## This is a developmental, outcome-informed rerun created after the v1
## party diagnostic.  It never overwrites v1, never enables formal inference,
## and refuses to execute the declared production profile until the pilot has
## been reviewed and the configuration is explicitly unlocked.
##
## From the package root:
##   applications/bin/Rscript45 \
##     applications/sw2022/v2/R/03_fit_penalty_pilot_v2.R \
##     --profile=smoke --force=true
##   applications/bin/Rscript45 \
##     applications/sw2022/v2/R/03_fit_penalty_pilot_v2.R \
##     --profile=pilot --force=true

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(profile = "smoke", force = FALSE)
  for (arg in x) {
    if (!grepl("^--[^=]+=", arg)) {
      stop("Malformed argument: ", arg, call. = FALSE)
    }
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- gsub("-", "_", bits[[1L]], fixed = TRUE)
    if (!key %in% names(out)) {
      stop("Unknown argument --", bits[[1L]], call. = FALSE)
    }
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  out
}

.atomic_save <- function(x, path, portable = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  if (isTRUE(portable)) x <- scmix_portable_copy(x)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write ", path, call. = FALSE)
  }
  invisible(path)
}

.write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.stamp <- function(x, config, profile_name, profile, role,
                   generation_input_md5, runtime_signature) {
  x$sw_v2_application_specification <- list(
    config_version = config$version, profile = profile_name,
    profile_specification = profile, role = role,
    generation_input_md5 = generation_input_md5,
    runtime_signature = runtime_signature,
    formal_inference_available = FALSE, outcome_blind = FALSE)
  x
}

.valid_stamp <- function(x, config, profile_name, profile, role,
                         generation_input_md5, runtime_signature) {
  s <- x$sw_v2_application_specification
  is.list(s) && identical(s$config_version, config$version) &&
    identical(s$profile, profile_name) &&
    identical(s$profile_specification, profile) &&
    identical(s$role, role) &&
    identical(s$formal_inference_available, FALSE) &&
    identical(s$generation_input_md5, generation_input_md5) &&
    identical(s$runtime_signature, runtime_signature)
}

.run_or_load <- function(path, overwrite, code, validator, portable = TRUE) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    message("checkpoint: loading ", path)
    out <- readRDS(path)
    if (!isTRUE(validator(out))) {
      stop("Stale or incompatible v2 checkpoint: ", path,
           ". Rerun with --force=true.", call. = FALSE)
    }
    return(out)
  }
  out <- force(code)
  .atomic_save(out, path, portable = portable)
  message("checkpoint: wrote ", path)
  out
}

.party_labels <- function(prepared, rid) {
  meta <- prepared$respondent_meta
  party <- as.character(meta$party[match(rid, meta$respondent_id)])
  party <- ifelse(grepl("Republican", party), "Republican", party)
  if (anyNA(party) ||
      !setequal(unique(party), c("Democrat", "Independent", "Republican"))) {
    stop("Party mapping failed.", call. = FALSE)
  }
  party
}

.score_summary <- function(scores, party) {
  strata <- c("Overall", sort(unique(party)))
  rows <- list(); z <- 0L
  for (model in names(scores)) {
    for (stratum in strata) {
      keep <- if (stratum == "Overall") rep(TRUE, length(party)) else
        party == stratum
      value <- scores[[model]][keep]
      z <- z + 1L
      rows[[z]] <- data.frame(
        model = model, party = stratum,
        mean_complete_sequence_log_score = mean(value),
        respondent_se = stats::sd(value) / sqrt(length(value)),
        n_respondents = length(value), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.paired_summary <- function(scores, party) {
  comparisons <- list(
    v2_selected_minus_v2_pooled = c("v2_selected_q1", "v2_constant_q1"),
    v2_selected_minus_v1_primary = c("v2_selected_q1", "v1_primary_q1"),
    v2_selected_minus_v1_pooled = c("v2_selected_q1", "v1_pooled_q1"),
    v2_selected_minus_v1_targeted = c("v2_selected_q1", "v1_targeted_q1"),
    v1_targeted_minus_v1_pooled = c("v1_targeted_q1", "v1_pooled_q1"))
  strata <- c("Overall", sort(unique(party)))
  rows <- list(); z <- 0L
  for (nm in names(comparisons)) {
    pair <- comparisons[[nm]]
    difference <- scores[[pair[[1L]]]] - scores[[pair[[2L]]]]
    for (stratum in strata) {
      keep <- if (stratum == "Overall") rep(TRUE, length(party)) else
        party == stratum
      value <- difference[keep]
      z <- z + 1L
      rows[[z]] <- data.frame(
        comparison = nm, party = stratum,
        mean_difference = mean(value),
        respondent_se = stats::sd(value) / sqrt(length(value)),
        n_respondents = length(value),
        interpretation = paste(
          "Positive favors the first model. Descriptive paired outer-fold",
          "comparison from an outcome-informed model-development exercise."),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.calibration <- function(y, rid, party_task, probabilities) {
  strata <- c("Overall", sort(unique(party_task)))
  rows <- list(); z <- 0L
  for (model in names(probabilities)) {
    p <- probabilities[[model]]
    for (stratum in strata) {
      keep <- if (stratum == "Overall") rep(TRUE, length(y)) else
        party_task == stratum
      z <- z + 1L
      rows[[z]] <- data.frame(
        model = model, party = stratum,
        observed_rate = mean(y[keep]), predicted_rate = mean(p[keep]),
        calibration_gap = mean(p[keep] - y[keep]),
        brier_score = mean((p[keep] - y[keep])^2),
        n_tasks = sum(keep),
        n_respondents = length(unique(rid[keep])),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.cluster_projection <- function(outcome, deltaX, cluster) {
  X <- cbind(intercept = 1, deltaX)
  fit <- stats::lm.fit(X, outcome)
  if (fit$rank != ncol(X) || any(!is.finite(fit$coefficients))) {
    stop("A descriptive AMCE projection was rank deficient.", call. = FALSE)
  }
  bread <- solve(crossprod(X))
  score <- rowsum(X * as.numeric(fit$residuals), as.character(cluster),
                  reorder = FALSE)
  G <- nrow(score); n <- nrow(X); k <- ncol(X)
  correction <- (G / (G - 1)) * ((n - 1) / (n - k))
  vcov <- correction * bread %*% crossprod(score) %*% bread
  list(coef = fit$coefficients[-1L], se = sqrt(diag(vcov))[-1L])
}

.amce_projection <- function(deltaX, y, rid, party_task, probabilities) {
  strata <- c("Overall", sort(unique(party_task)))
  rows <- list(); z <- 0L
  for (stratum in strata) {
    keep <- if (stratum == "Overall") rep(TRUE, length(y)) else
      party_task == stratum
    observed <- .cluster_projection(y[keep], deltaX[keep, , drop = FALSE],
                                    rid[keep])
    for (model in names(probabilities)) {
      projected <- .cluster_projection(
        probabilities[[model]][keep], deltaX[keep, , drop = FALSE], rid[keep])
      z <- z + 1L
      rows[[z]] <- data.frame(
        model = model, party = stratum,
        contrast = colnames(deltaX), observed_projection = observed$coef,
        observed_cluster_se = observed$se,
        model_projection = projected$coef,
        model_minus_observed = projected$coef - observed$coef,
        formal_inference_available = FALSE, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.mean_by_party <- function(mu, party_task, coefficient_names) {
  levels <- c("Democrat", "Independent", "Republican")
  means <- t(vapply(levels, function(g) {
    colMeans(mu[party_task == g, , drop = FALSE])
  }, numeric(ncol(mu))))
  out <- data.frame(
    party = rep(levels, each = ncol(mu)),
    coefficient = rep(coefficient_names, times = length(levels)),
    estimate = as.numeric(t(means)), stringsAsFactors = FALSE)
  ranges <- data.frame(
    coefficient = coefficient_names,
    party_range = apply(means, 2L, function(x) diff(range(x))),
    stringsAsFactors = FALSE)
  list(means = out, ranges = ranges)
}

.candidate_tables <- function(full, nested) {
  full_table <- full$candidates
  full_table$fit_scope <- "full_sample_inner_cv"
  full_table$outer_fold <- NA_character_
  outer <- do.call(rbind, lapply(seq_along(nested$tuning), function(k) {
    out <- nested$tuning[[k]]$candidates
    out$fit_scope <- "outer_training_inner_cv"
    out$outer_fold <- as.character(nested$outer_folds[[k]])
    out
  }))
  rbind(full_table, outer)
}

.selected_table <- function(full, nested) {
  full_row <- data.frame(
    fit_scope = "full_sample", outer_fold = NA_character_,
    candidate = full$selected$name,
    mean_family = full$selected$mean_family,
    hidden = paste(full$selected$hidden, collapse = "-"),
    weight_decay = full$selected$weight_decay,
    nesting_gate_pass = isTRUE(
      full$refit$optimization$nested_objective_gate$pass),
    optimization_gate_pass = isTRUE(
      full$refit$optimization$optimization_gate_pass),
    pooled_prefit_gate_pass = is.null(
      full$refit$pooled_prefit_optimization) || isTRUE(
        full$refit$pooled_prefit_optimization$optimization_gate_pass),
    continued_constant_gate_pass = is.null(
      full$refit$continued_constant_optimization) || isTRUE(
        full$refit$continued_constant_optimization$optimization_gate_pass) &&
      (is.null(full$refit$continued_constant_optimization) || isTRUE(
        full$refit$continued_constant_optimization$nested_objective_gate$pass)),
    alpha_bound_active = isTRUE(full$refit$optimization$bounds$alpha_active),
    stringsAsFactors = FALSE)
  outer <- do.call(rbind, lapply(seq_along(nested$tuning), function(k) {
    x <- nested$tuning[[k]]
    data.frame(
      fit_scope = "outer_training", outer_fold = nested$outer_folds[[k]],
      candidate = x$selected$name, mean_family = x$selected$mean_family,
      hidden = paste(x$selected$hidden, collapse = "-"),
      weight_decay = x$selected$weight_decay,
      nesting_gate_pass = isTRUE(
        x$refit$optimization$nested_objective_gate$pass),
      optimization_gate_pass = isTRUE(
        x$refit$optimization$optimization_gate_pass),
      pooled_prefit_gate_pass = is.null(
        x$refit$pooled_prefit_optimization) || isTRUE(
          x$refit$pooled_prefit_optimization$optimization_gate_pass),
      continued_constant_gate_pass = is.null(
        x$refit$continued_constant_optimization) || isTRUE(
          x$refit$continued_constant_optimization$optimization_gate_pass) &&
        (is.null(x$refit$continued_constant_optimization) || isTRUE(
          x$refit$continued_constant_optimization$nested_objective_gate$pass)),
      alpha_bound_active = isTRUE(x$refit$optimization$bounds$alpha_active),
      stringsAsFactors = FALSE)
  }))
  rbind(full_row, outer)
}

.schedule_optimization_table <- function(full, nested) {
  rows <- list(); z <- 0L
  add <- function(opt, fit_scope, outer_fold, stage) {
    if (is.null(opt)) return(NULL)
    data.frame(
      fit_scope = fit_scope, outer_fold = outer_fold, stage = stage,
      objective = opt$objective, gradient_norm = opt$gradient_norm,
      structural_gradient_norm = opt$structural_gradient_norm,
      alpha_gradient_norm = opt$alpha_gradient_norm,
      sieve_gradient_norm = opt$sieve_gradient_norm,
      criterion_tolerance_met = isTRUE(opt$criterion_tolerance_met),
      stationarity_met = isTRUE(opt$stationarity_met),
      optimization_gate_pass = isTRUE(opt$optimization_gate_pass),
      objective_dominance_gate_pass = is.null(opt$nested_objective_gate) ||
        isTRUE(opt$nested_objective_gate$pass),
      objective_dominance_gap = if (is.null(opt$nested_objective_gate)) {
        NA_real_
      } else opt$nested_objective_gate$gap,
      mu_bound_active = isTRUE(opt$bounds$mu_active),
      alpha_bound_active = isTRUE(opt$bounds$alpha_active),
      kappa_bound_active = isTRUE(opt$bounds$kappa_active),
      loading_bound_active = isTRUE(opt$bounds$a_active),
      deviation_weight_bound_active = isTRUE(opt$bounds$weight_active),
      stringsAsFactors = FALSE)
  }
  for (stage in c("pooled_prefit_optimization",
                  "continued_constant_optimization", "optimization")) {
    z <- z + 1L
    rows[[z]] <- add(full$refit[[stage]], "full_sample", NA_character_, stage)
  }
  for (k in seq_along(nested$tuning)) {
    for (stage in c("pooled_prefit_optimization",
                    "continued_constant_optimization", "optimization")) {
      z <- z + 1L
      rows[[z]] <- add(nested$tuning[[k]]$refit[[stage]],
                       "outer_training", nested$outer_folds[[k]], stage)
    }
  }
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
root <- normalizePath(file.path(dirname(.script_file()), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
source(file.path(app, "v2", "config", "analysis_config_v2.R"), local = FALSE)
if (!cli$profile %in% names(sw_v2_config$profiles)) {
  stop("--profile must be one of: ",
       paste(names(sw_v2_config$profiles), collapse = ", "), call. = FALSE)
}
profile <- sw_v2_config$profiles[[cli$profile]]
provenance_source <- file.path(root, "R", "provenance.R")
if (!file.exists(provenance_source)) {
  stop("Missing fail-closed provenance helper: ", provenance_source,
       call. = FALSE)
}
source(provenance_source, local = FALSE)
package_sources <- sort(list.files(
  file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
names(package_sources) <- paste0("package_source:", basename(package_sources))
input_paths <- c(
  prepared = sw_v2_config$input$prepared,
  v1_nested = sw_v2_config$input$v1_nested,
  v1_party_diagnostic = sw_v2_config$input$v1_party_diagnostic,
  config = file.path(app, "v2", "config", "analysis_config_v2.R"),
  runner = .script_file(),
  package_description = file.path(root, "DESCRIPTION"),
  package_namespace = file.path(root, "NAMESPACE"),
  launcher_R45 = file.path(root, "applications", "bin", "R45"),
  launcher_Rscript45 = file.path(root, "applications", "bin", "Rscript45"),
  package_sources)
if (any(!file.exists(input_paths))) {
  stop("Missing input(s): ",
       paste(names(input_paths)[!file.exists(input_paths)], collapse = ", "),
       call. = FALSE)
}
input_md5_before <- .sc_md5_paths(input_paths)
frozen_names <- c("prepared", "v1_nested", "v1_party_diagnostic")

if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
runtime_signature <- .sc_runtime_signature(
  input_paths[["package_description"]])

if (identical(cli$profile, "production")) {
  authorization_path <- profile$execution_authorization_file
  config_path <- input_paths[["config"]]
  pilot_manifest_path <- file.path(sw_v2_config$output_root, "pilot",
                                   "manifest.rds")
  authorization <- if (is.character(authorization_path) &&
      length(authorization_path) == 1L && file.exists(authorization_path)) {
    tryCatch(readRDS(authorization_path), error = function(e) NULL)
  } else NULL
  reviewed_manifest <- if (file.exists(pilot_manifest_path)) {
    tryCatch(readRDS(pilot_manifest_path), error = function(e) NULL)
  } else NULL
  authorized <- file.exists(pilot_manifest_path) &&
    .sc_reviewed_pilot_authorized(
      authorization = authorization,
      reviewed_manifest = reviewed_manifest,
      current_generation_md5 = input_md5_before,
      current_runtime_signature = runtime_signature,
      config_version = sw_v2_config$version,
      current_config_md5 = unname(tools::md5sum(config_path)),
      current_manifest_md5 = unname(tools::md5sum(pilot_manifest_path)),
      reviewed_manifest_path = pilot_manifest_path)
  if (!authorized) {
    stop(
      "The v2 production profile is fail-closed. It requires a separate, ",
      "explicit review authorization whose stored config, reviewed-pilot ",
      "manifest, and complete named generation-input hash vector exactly match ",
      "the successful reviewed pilot and the current execution, whose runtime ",
      "signature matches, and whose listed artifacts remain intact. Missing or ",
      "stale authorization never unlocks execution.", call. = FALSE)
  }
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

prepared <- readRDS(input_paths[["prepared"]])
v1_nested <- readRDS(input_paths[["v1_nested"]])
v1_diagnostic <- readRDS(input_paths[["v1_party_diagnostic"]])
deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[sw_v2_config$input$primary_Z]])
rid <- as.character(prepared$respondent_id)
task <- as.numeric(prepared$task)
if (!identical(nrow(deltaX), 3573L) || length(unique(rid)) != 1191L ||
    nrow(deltaX) != nrow(Z) || nrow(deltaX) != length(y) ||
    nrow(deltaX) != length(rid) || any(!is.finite(deltaX)) ||
    any(!is.finite(Z)) || any(!y %in% c(0, 1))) {
  stop("The frozen 1,191-respondent prepared sample is malformed.",
       call. = FALSE)
}
if (any(vapply(split(seq_len(nrow(Z)), rid), function(ii) {
  max(abs(sweep(Z[ii, , drop = FALSE], 2L, Z[ii[1L], ], `-`))) > 1e-12
}, logical(1L)))) {
  stop("Primary moderators are not constant within respondent.", call. = FALSE)
}
party_task <- .party_labels(prepared, rid)
if (!identical(as.numeric(Z[, "party_Republican"]),
               as.numeric(party_task == "Republican")) ||
    !identical(as.numeric(Z[, "party_Independent"]),
               as.numeric(party_task == "Independent"))) {
  stop("Party metadata do not reproduce the frozen moderator columns.",
       call. = FALSE)
}

v1_fold <- as.integer(v1_nested$outer_fold_id)
if (length(v1_fold) != nrow(deltaX) ||
    any(vapply(split(v1_fold, rid), function(x) length(unique(x)) != 1L,
               logical(1L)))) {
  stop("The v1 nested artifact does not contain intact respondent folds.",
       call. = FALSE)
}
outer_fold <- if (identical(cli$profile, "pilot")) {
  v1_fold
} else {
  labels <- unique(v1_fold)
  collapsed <- 1L + (match(v1_fold, labels) - 1L) %% profile$outer_K
  as.integer(collapsed)
}
if (any(vapply(split(outer_fold, rid), function(x) length(unique(x)) != 1L,
               logical(1L)))) {
  stop("The v2 diagnostic outer folds split a respondent.", call. = FALSE)
}

output_dir <- file.path(sw_v2_config$output_root, cli$profile)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
seed <- as.integer(sw_v2_config$optimizer$seed)
nested_tol <- profile$nested_objective_tol %||% 1e-6
common <- list(
  deltaX = deltaX, y = y, Z = Z, respondent_id = rid,
  grid = profile$grid, q = sw_v2_config$model$q,
  allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
  n_epochs = profile$n_epochs, learning_rate = profile$learning_rate,
  n_starts = profile$n_starts,
  mu_bound = sw_v2_config$bounds$mu,
  kappa_bound = sw_v2_config$bounds$kappa,
  alpha_bound = sw_v2_config$bounds$alpha,
  a_bound = sw_v2_config$bounds$loading,
  weight_bound = sw_v2_config$bounds$deviation_parameter,
  opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
  nested_objective_tol = nested_tol,
  device = sw_v2_config$optimizer$device,
  keep_cv_fits = FALSE, verbose = FALSE)

validator <- function(role) function(x) {
  .valid_stamp(x, sw_v2_config, cli$profile, profile, role,
               input_md5_before, runtime_signature)
}
full_path <- file.path(output_dir, "fit_v2_full.rds")
full <- .run_or_load(
  full_path, cli$force,
  .stamp(do.call(scmix_tune_matrix,
                 c(common, list(K = profile$inner_K, refit = TRUE,
                                seed = seed + 101L))),
         sw_v2_config, cli$profile, profile, "v2_full", input_md5_before,
         runtime_signature),
  validator("v2_full"))

nested_path <- file.path(output_dir, "fit_v2_nested.rds")
nested <- .run_or_load(
  nested_path, cli$force,
  .stamp(do.call(scmix_tune_outer_matrix,
                 c(common, list(outer_K = profile$outer_K,
                                inner_K = profile$inner_K,
                                outer_fold_id = outer_fold,
                                seed = seed + 201L))),
         sw_v2_config, cli$profile, profile, "v2_nested", input_md5_before,
         runtime_signature),
  validator("v2_nested"))

assembled_path <- file.path(output_dir, "fit_v2_assembled.rds")
assembled <- .run_or_load(
  assembled_path, cli$force,
  .stamp(scmix_assemble_nested(
    nested, attr_names = colnames(deltaX), z_names = colnames(Z),
    require_optimization_gate = FALSE, diagnostic_only = TRUE),
    sw_v2_config, cli$profile, profile, "v2_assembled", input_md5_before,
    runtime_signature),
  validator("v2_assembled"))

constant_index <- which(vapply(
  profile$grid, function(x) identical(x$mean_family, "constant"), logical(1L)))
if (length(constant_index) != 1L) {
  stop("The v2 profile must contain exactly one constant candidate.",
       call. = FALSE)
}
constant_grid <- profile$grid[constant_index]
pooled_common <- common
pooled_common$grid <- constant_grid
pooled_nested_path <- file.path(output_dir, "fit_v2_constant_nested.rds")
pooled_nested <- .run_or_load(
  pooled_nested_path, cli$force,
  .stamp(do.call(scmix_tune_outer_matrix,
                 c(pooled_common, list(outer_K = profile$outer_K,
                                       inner_K = profile$inner_K,
                                       outer_fold_id = outer_fold,
                                       seed = seed + 201L))),
         sw_v2_config, cli$profile, profile, "v2_constant_nested",
         input_md5_before, runtime_signature),
  validator("v2_constant_nested"))
pooled_assembled_path <- file.path(output_dir,
                                   "fit_v2_constant_assembled.rds")
pooled_assembled <- .run_or_load(
  pooled_assembled_path, cli$force,
  .stamp(scmix_assemble_nested(
    pooled_nested, attr_names = colnames(deltaX), z_names = colnames(Z),
    require_optimization_gate = FALSE, diagnostic_only = TRUE),
    sw_v2_config, cli$profile, profile, "v2_constant_assembled",
    input_md5_before, runtime_signature),
  validator("v2_constant_assembled"))

prediction <- scmix_heldout_predictions(
  assembled, task_order = task, include_counts = FALSE,
  include_adjacent = FALSE, include_repeated = FALSE)
pooled_prediction <- scmix_heldout_predictions(
  pooled_assembled, task_order = task, include_counts = FALSE,
  include_adjacent = FALSE, include_repeated = FALSE)
ids <- unique(rid)
if (!identical(prediction$respondent_id, ids) ||
    !identical(pooled_prediction$respondent_id, ids) ||
    !identical(as.character(prediction$task$respondent_id), rid) ||
    !identical(as.numeric(prediction$task$observed), y)) {
  stop("V2 held-out predictions are not aligned to frozen task rows.",
       call. = FALSE)
}

v1_ids <- names(v1_diagnostic$pooled$sequence_score)
if (length(v1_ids) != length(ids) || !identical(v1_ids, ids)) {
  stop("The v1 party diagnostic is not aligned with the prepared respondents.",
       call. = FALSE)
}
scores <- list(
  v2_selected_q1 = as.numeric(prediction$sequence_loglik[ids]),
  v2_constant_q1 = as.numeric(pooled_prediction$sequence_loglik[ids]),
  v1_primary_q1 = as.numeric(v1_diagnostic$primary_sequence_score),
  v1_pooled_q1 = as.numeric(v1_diagnostic$pooled$sequence_score[ids]),
  v1_targeted_q1 = as.numeric(v1_diagnostic$targeted$sequence_score[ids]))
if (any(!vapply(scores, function(x) length(x) == length(ids) &&
                              all(is.finite(x)), logical(1L)))) {
  stop("At least one complete-sequence score vector is malformed.",
       call. = FALSE)
}
party_respondent <- party_task[match(ids, rid)]
probabilities <- list(
  v2_selected_q1 = prediction$task$predicted,
  v2_constant_q1 = pooled_prediction$task$predicted)
## V1 task probabilities were not retained in the compact diagnostic. The v2
## calibration and AMCE projections are therefore compared to one another;
## complete-sequence score vectors supply the v1 comparisons.

candidate_table <- .candidate_tables(full, nested)
selected_table <- .selected_table(full, nested)
schedule_optimization_table <- .schedule_optimization_table(full, nested)
score_table <- .score_summary(scores, party_respondent)
paired_table <- .paired_summary(scores, party_respondent)
calibration_table <- .calibration(y, rid, party_task, probabilities)
amce_table <- .amce_projection(deltaX, y, rid, party_task, probabilities)
party_mean <- .mean_by_party(full$refit$mu, party_task, colnames(deltaX))
full_audit <- scmix_optimization_audit(full$refit)
nested_audit <- scmix_optimization_audit(assembled)
pooled_nested_audit <- scmix_optimization_audit(pooled_assembled)

score_gate_value <- paired_table$mean_difference[
  paired_table$comparison == "v2_selected_minus_v2_pooled" &
    paired_table$party == "Overall"]
outer_selected_gate <- all(assembled$optimization$gate_by_fold)
outer_pooled_gate <- all(pooled_assembled$optimization$gate_by_fold)
full_gate <- isTRUE(full$refit$optimization$optimization_gate_pass) &&
  isTRUE(full$refit$optimization$nested_objective_gate$pass) &&
  !isTRUE(full$refit$optimization$bounds$alpha_active)
pooled_prefit_gate <- all(selected_table$pooled_prefit_gate_pass)
continued_constant_gate <- all(selected_table$continued_constant_gate_pass)
nesting_gate <- all(selected_table$nesting_gate_pass)
alpha_gate <- !any(selected_table$alpha_bound_active)
gates <- data.frame(
  gate = c(
    "prepared and v1 row alignment", "respondent outer-fold isolation",
    "selected nested-objective gate", "selected alpha bound inactive",
    "shared pooled prefit optimization gates",
    "continued constant optimization gates",
    "full selected optimization gate", "outer selected optimization gates",
    "outer constant optimization gates",
    "selected held-out score at least exact constant",
    "all generation inputs unchanged", "formal inference enabled",
    "outcome-blind model development"),
  pass = c(
    TRUE,
    !any(vapply(split(outer_fold, rid),
                function(x) length(unique(x)) != 1L, logical(1L))),
    nesting_gate, alpha_gate, pooled_prefit_gate, continued_constant_gate,
    full_gate, outer_selected_gate,
    outer_pooled_gate, is.finite(score_gate_value) && score_gate_value >= 0,
    NA, FALSE, FALSE),
  required_for_pilot_success = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE),
  value = c(
    "1,191 respondents; 3,573 tasks; p=13",
    if (cli$profile == "pilot") "exact five v1 outer folds reused" else
      "v1 fold labels deterministically collapsed for interface smoke test",
    as.character(nesting_gate), as.character(alpha_gate),
    as.character(pooled_prefit_gate), as.character(continued_constant_gate),
    as.character(full_gate), as.character(outer_selected_gate),
    as.character(outer_pooled_gate), signif(score_gate_value, 8),
    "pending final hash check", "FALSE", "FALSE"),
  stringsAsFactors = FALSE)

input_md5_after <- .sc_md5_paths(input_paths)
all_inputs_unchanged <- .sc_identical_md5_vectors(
  input_md5_before, input_md5_after)
frozen_unchanged <- .sc_identical_md5_vectors(
  input_md5_before[frozen_names], input_md5_after[frozen_names])
gates$pass[gates$gate == "all generation inputs unchanged"] <-
  all_inputs_unchanged
gates$value[gates$gate == "all generation inputs unchanged"] <-
  as.character(all_inputs_unchanged)
if (!all_inputs_unchanged) {
  stop(
    "A generation-time input or source file changed while v2 ran. No mixed ",
    "checkpoint/result set may combine states from different hashes.",
    call. = FALSE)
}
required <- gates$required_for_pilot_success
pilot_success <- all(!is.na(gates$pass[required]) & gates$pass[required])

tables <- list(
  candidates = candidate_table, selections = selected_table,
  optimization_schedule = schedule_optimization_table,
  sequence_score_summary = score_table,
  sequence_score_paired_differences = paired_table,
  calibration = calibration_table, amce_projection = amce_table,
  full_fit_party_means = party_mean$means,
  full_fit_party_ranges = party_mean$ranges,
  optimization_full = full_audit$summary,
  optimization_nested = nested_audit$summary,
  optimization_constant_nested = pooled_nested_audit$summary,
  reporting_gates = gates)
for (nm in names(tables)) {
  .write_csv(tables[[nm]], file.path(output_dir, paste0(nm, ".csv")))
}

result <- list(
  schema_version = "sw2022-mixed-logit-v2-penalty-pilot-v1",
  profile = cli$profile, configuration = sw_v2_config,
  profile_specification = profile,
  provenance = sw_v2_config$provenance,
  outcome_blind = FALSE, formal_inference_available = FALSE,
  production_result = FALSE, primary_v1_artifacts_modified = FALSE,
  inherited_v1_outer_folds = identical(cli$profile, "pilot"),
  outer_fold_id = outer_fold,
  sample = list(n_respondents = length(ids), n_tasks = nrow(deltaX),
                p = ncol(deltaX), party_n = table(party_respondent)),
  analysis_signatures = list(
    full = full$analysis_signature, nested = nested$analysis_signature,
    constant_nested = pooled_nested$analysis_signature),
  selected_full = full$selected,
  selected_outer = lapply(nested$tuning, `[[`, "selected"),
  sequence_scores = scores,
  task_predictions = probabilities,
  tables = tables, gates = gates, pilot_success = pilot_success,
  interpretation = paste(
    "This post-hoc v2 diagnostic separates an unpenalized compact reference",
    "mean from penalized centered moderator deviations and makes the exact",
    "constant q=1 model eligible. Its nested respondent-level held-out scores",
    "are descriptive model-development evidence; formal inference is withheld."),
  input_paths = input_paths, generation_input_md5 = input_md5_before,
  runtime_signature = runtime_signature,
  completion_input_md5 = input_md5_after,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  session_info = utils::capture.output(sessionInfo()))
result_path <- file.path(output_dir, "penalty_pilot_v2.rds")
.atomic_save(result, result_path, portable = FALSE)

artifact_paths <- c(
  full_path, nested_path, assembled_path, pooled_nested_path,
  pooled_assembled_path, result_path,
  file.path(output_dir, paste0(names(tables), ".csv")))
artifact_paths <- artifact_paths[file.exists(artifact_paths)]
manifest <- list(
  schema_version = "sw2022-mixed-logit-v2-penalty-pilot-manifest-v1",
  profile = cli$profile, configuration_version = sw_v2_config$version,
  input_paths = input_paths, generation_input_md5 = input_md5_before,
  runtime_signature = runtime_signature,
  completion_input_md5 = input_md5_after,
  artifacts = stats::setNames(unname(tools::md5sum(artifact_paths)),
                              basename(artifact_paths)),
  frozen_v1_artifacts_unchanged = frozen_unchanged,
  all_generation_inputs_unchanged = all_inputs_unchanged,
  pilot_success = pilot_success,
  formal_inference_available = FALSE, production_result = FALSE,
  outcome_blind = FALSE,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
.atomic_save(manifest, file.path(output_dir, "manifest.rds"), portable = FALSE)

report <- c(
  "# Saha--Weeks mixed-logit v2 penalty pilot",
  "",
  paste0("- Profile: `", cli$profile, "`."),
  paste0("- Pilot gate: `", pilot_success, "`."),
  paste0("- Full-sample selected family: `", full$selected$mean_family,
         "` (`", full$selected$name, "`)."),
  paste0("- V2 selected minus exact-constant held-out sequence score: `",
         signif(score_gate_value, 8), "`."),
  paste0("- Largest selected full-fit party mean range: `",
         signif(max(party_mean$ranges$party_range), 8), "`."),
  "- This exercise is outcome-informed; formal inference is withheld.",
  "- V1 artifacts were read-only and retained under their original paths.",
  if (!pilot_success) paste(
    "- At least one required pilot gate failed. Do not launch or unlock",
    "production; inspect `reporting_gates.csv` and the optimization tables.")
  else paste(
    "- The declared pilot gates passed. Production remains locked until the",
    "audit is reviewed and the next-application configuration is frozen."))
writeLines(report, file.path(output_dir, "AUDIT.md"), useBytes = TRUE)

message("v2 ", cli$profile, " complete; pilot_success=", pilot_success,
        "; output=", output_dir)
