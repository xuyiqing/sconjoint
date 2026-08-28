#!/usr/bin/env Rscript

## Saha--Weeks v2.1 post-pilot final analysis.
## Outcome-informed; descriptive only; formal inference is unavailable.
## Execution is fail-closed until a separately reviewed authorization exists.

options(stringsAsFactors = FALSE, warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(x) {
  out <- list(force = FALSE)
  for (arg in x) {
    if (!grepl("^--force=", arg)) {
      stop("Only --force=true or --force=false is accepted.", call. = FALSE)
    }
    out$force <- tolower(sub("^--force=", "", arg)) %in%
      c("1", "true", "yes")
  }
  out
}

.atomic_save <- function(x, path, portable = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  if (isTRUE(portable)) x <- scmix_portable_copy(x)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.stamp <- function(x, config, role, generation_md5, runtime_signature,
                   authorization_md5) {
  x$sw_v21_application_specification <- list(
    config_version = config$version, role = role,
    generation_input_md5 = generation_md5,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5,
    guardrail = config$postpilot_guardrail,
    outcome_blind = FALSE, formal_inference_available = FALSE,
    production_result = FALSE)
  x
}

.valid_stamp <- function(x, config, role, generation_md5,
                         runtime_signature, authorization_md5) {
  s <- x$sw_v21_application_specification
  is.list(s) && identical(s$config_version, config$version) &&
    identical(s$role, role) &&
    identical(s$generation_input_md5, generation_md5) &&
    identical(s$runtime_signature, runtime_signature) &&
    identical(s$authorization_md5, authorization_md5) &&
    identical(s$guardrail, config$postpilot_guardrail) &&
    identical(s$outcome_blind, FALSE) &&
    identical(s$formal_inference_available, FALSE) &&
    identical(s$production_result, FALSE)
}

.run_or_load <- function(path, overwrite, code, validator,
                         portable = TRUE) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    message("checkpoint: loading ", path)
    out <- readRDS(path)
    if (!isTRUE(validator(out))) {
      stop("Stale or incompatible v2.1 checkpoint: ", path,
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
  out <- as.character(meta$party[match(rid, meta$respondent_id)])
  out <- ifelse(grepl("Republican", out), "Republican", out)
  if (anyNA(out) ||
      !setequal(unique(out), c("Democrat", "Independent", "Republican"))) {
    stop("Party mapping failed.", call. = FALSE)
  }
  out
}

.score_summary <- function(scores, party) {
  strata <- c("Overall", sort(unique(party)))
  rows <- list(); z <- 0L
  for (model in names(scores)) for (stratum in strata) {
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
  do.call(rbind, rows)
}

.paired_summary <- function(scores, party) {
  comparisons <- list(
    selected_minus_constant = c("selected_procedure_q1", "exact_constant_q1"),
    selected_minus_v1_primary = c("selected_procedure_q1", "v1_primary_q1"),
    selected_minus_v1_pooled = c("selected_procedure_q1", "v1_pooled_q1"),
    selected_minus_v1_targeted = c("selected_procedure_q1", "v1_targeted_q1"),
    constant_minus_v1_targeted = c("exact_constant_q1", "v1_targeted_q1"))
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
        descriptive_only = TRUE, formal_test = FALSE,
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

.candidate_table <- function(full, nested) {
  a <- full$candidates
  a$fit_scope <- "full_sample_inner_cv"
  a$outer_fold <- NA_character_
  b <- do.call(rbind, lapply(seq_along(nested$tuning), function(k) {
    out <- nested$tuning[[k]]$candidates
    out$fit_scope <- "outer_training_inner_cv"
    out$outer_fold <- as.character(nested$outer_folds[[k]])
    out
  }))
  rbind(a, b)
}

.stage_gate <- function(x) {
  is.null(x) || isTRUE(sconjoint:::.sc_comp_inner_fit_gate(x)$pass)
}

.selection_table <- function(full, nested) {
  one <- function(tuning, scope, fold) {
    data.frame(
      fit_scope = scope, outer_fold = fold,
      candidate = tuning$selected$name,
      mean_family = tuning$selected$mean_family,
      hidden = paste(tuning$selected$hidden, collapse = "-"),
      weight_decay = tuning$selected$weight_decay,
      candidate_selection_gate = isTRUE(tuning$candidate_selection_gate$pass),
      selected_refit_optimization_gate = .stage_gate(
        tuning$refit$optimization),
      selected_refit_nesting_gate =
        isTRUE(tuning$refit$optimization$nested_objective_gate$pass),
      pooled_prefit_gate = .stage_gate(
        tuning$refit$pooled_prefit_optimization),
      continued_constant_gate = .stage_gate(
        tuning$refit$continued_constant_optimization),
      any_compact_bound_active = any(unlist(
        tuning$refit$optimization$bounds[
          c("mu_active", "alpha_active", "kappa_active", "a_active",
            "weight_active")], use.names = FALSE)),
      stringsAsFactors = FALSE)
  }
  rows <- list(one(full, "full_sample", NA_character_))
  for (k in seq_along(nested$tuning)) {
    rows[[length(rows) + 1L]] <- one(
      nested$tuning[[k]], "outer_training",
      as.character(nested$outer_folds[[k]]))
  }
  do.call(rbind, rows)
}

.tuning_gate <- function(tuning) {
  sel <- tuning$selected_index
  isTRUE(tuning$candidate_selection_gate$pass) &&
    length(sel) == 1L && !is.na(sel) &&
    isTRUE(tuning$candidate_selection_gate$selection_eligible[sel]) &&
    .stage_gate(tuning$refit$optimization) &&
    .stage_gate(tuning$refit$pooled_prefit_optimization) &&
    .stage_gate(tuning$refit$continued_constant_optimization)
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
root <- normalizePath(file.path(dirname(.script_file()), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
config_path <- file.path(app, "v2_1", "config", "analysis_config_v2_1.R")
source(config_path, local = FALSE)
predecessor_config_path <- file.path(
  app, "v2", "config", "analysis_config_v2.R")
predecessor_env <- new.env(parent = baseenv())
sys.source(predecessor_config_path, envir = predecessor_env)
predecessor_config <- predecessor_env$sw_v2_config
provenance_source <- file.path(root, "R", "provenance.R")
source(provenance_source, local = FALSE)
contract_source <- file.path(
  app, "v2_1", "R", "postpilot_contract_v2_1.R")
source(contract_source, local = FALSE)

package_sources <- sort(list.files(
  file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
names(package_sources) <- paste0("package_source:", basename(package_sources))
pilot_manifest_path <- sw_v21_config$input$failed_v2_pilot_manifest
pilot_manifest <- if (file.exists(pilot_manifest_path)) {
  tryCatch(readRDS(pilot_manifest_path), error = function(e) NULL)
} else NULL
pilot_artifact_paths <- if (is.list(pilot_manifest) &&
    is.character(pilot_manifest$artifacts)) {
  out <- file.path(dirname(pilot_manifest_path), names(pilot_manifest$artifacts))
  names(out) <- paste0("failed_pilot_artifact:", names(pilot_manifest$artifacts))
  out
} else character()
input_paths <- c(
  prepared = sw_v21_config$input$prepared,
  v1_nested = sw_v21_config$input$v1_nested,
  v1_party_diagnostic = sw_v21_config$input$v1_party_diagnostic,
  postpilot_config = config_path, postpilot_runner = .script_file(),
  authorization_creator = file.path(
    app, "v2_1", "R", "00_create_final_analysis_authorization_v2_1.R"),
  predecessor_config = predecessor_config_path,
  package_description = file.path(root, "DESCRIPTION"),
  package_namespace = file.path(root, "NAMESPACE"),
  launcher_R45 = file.path(root, "applications", "bin", "R45"),
  launcher_Rscript45 = file.path(root, "applications", "bin", "Rscript45"),
  postpilot_contract = contract_source,
  failed_pilot_manifest = pilot_manifest_path,
  package_sources, pilot_artifact_paths)
if (any(!file.exists(input_paths))) {
  stop("Missing v2.1 generation input(s): ",
       paste(names(input_paths)[!file.exists(input_paths)], collapse = ", "),
       call. = FALSE)
}
generation_md5 <- .sc_md5_paths(input_paths)
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
runtime_signature <- .sc_runtime_signature(
  input_paths[["package_description"]])
prepilot_spec_unchanged <- .sw_v21_prepilot_spec_unchanged(
  predecessor_config, sw_v21_config)
authorization <- if (file.exists(sw_v21_config$authorization_file)) {
  tryCatch(readRDS(sw_v21_config$authorization_file),
           error = function(e) NULL)
} else NULL
authorized <- prepilot_spec_unchanged &&
  .sw_v21_failed_pilot_valid(
    pilot_manifest, pilot_manifest_path, runtime_signature,
    sw_v21_config$predecessor) &&
  .sw_v21_authorization_valid(
    authorization, sw_v21_config, config_path, predecessor_config_path,
    pilot_manifest,
    pilot_manifest_path, generation_md5, runtime_signature)
if (!authorized) {
  stop(
    "The v2.1 post-pilot analysis is fail-closed. A separate authorization ",
    "must match the failed-pilot manifest and artifacts, every reviewed ",
    "failed-pilot generation hash, the complete current v2.1 source/input ",
    "hash vector, runtime, config, and -0.001 guardrail. No fit was started.",
    call. = FALSE)
}
authorization_md5 <- unname(tools::md5sum(sw_v21_config$authorization_file))

suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
prepared <- readRDS(input_paths[["prepared"]])
v1_nested <- readRDS(input_paths[["v1_nested"]])
v1_diagnostic <- readRDS(input_paths[["v1_party_diagnostic"]])
deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[sw_v21_config$input$primary_Z]])
rid <- as.character(prepared$respondent_id)
if (!identical(nrow(deltaX), 3573L) || length(unique(rid)) != 1191L ||
    nrow(deltaX) != nrow(Z) || nrow(deltaX) != length(y) ||
    nrow(deltaX) != length(rid) || any(!is.finite(deltaX)) ||
    any(!is.finite(Z)) || any(!y %in% c(0, 1))) {
  stop("The frozen Saha--Weeks analysis sample is malformed.", call. = FALSE)
}
if (any(vapply(split(seq_len(nrow(Z)), rid), function(ii) {
  max(abs(sweep(Z[ii, , drop = FALSE], 2L, Z[ii[1L], ], `-`))) > 1e-12
}, logical(1L)))) {
  stop("Moderators are not constant within respondent.", call. = FALSE)
}
outer_fold <- as.integer(v1_nested$outer_fold_id)
if (length(outer_fold) != nrow(deltaX) ||
    !setequal(unique(outer_fold), seq_len(sw_v21_config$folds$outer_K)) ||
    any(vapply(split(outer_fold, rid), function(x) length(unique(x)) != 1L,
               logical(1L)))) {
  stop("The exact v1 respondent outer folds are unavailable.", call. = FALSE)
}
constant_index <- which(vapply(
  sw_v21_config$grid,
  function(x) identical(x$mean_family, "constant"), logical(1L)))
if (length(sw_v21_config$grid) != 10L || length(constant_index) != 1L ||
    !identical(sw_v21_config$grid[[constant_index]]$weight_decay, 0)) {
  stop("The frozen grid must contain ten candidates and one exact constant.",
       call. = FALSE)
}

output_dir <- sw_v21_config$output_root
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
opt <- sw_v21_config$optimizer
seed <- as.integer(opt$seed)
common <- list(
  deltaX = deltaX, y = y, Z = Z, respondent_id = rid,
  grid = sw_v21_config$grid, q = sw_v21_config$model$q,
  allow_q_tuning = FALSE, allow_integration_tuning = FALSE,
  n_epochs = opt$n_epochs, learning_rate = opt$learning_rate,
  n_starts = opt$n_starts,
  mu_bound = sw_v21_config$bounds$mu,
  kappa_bound = sw_v21_config$bounds$kappa,
  alpha_bound = sw_v21_config$bounds$alpha,
  a_bound = sw_v21_config$bounds$loading,
  weight_bound = sw_v21_config$bounds$deviation_parameter,
  opt_tol = opt$opt_tol, grad_tol = opt$grad_tol,
  nested_objective_tol = opt$nested_objective_tol,
  selection_tie_tol = opt$selection_tie_tol,
  device = opt$device, keep_cv_fits = FALSE, verbose = FALSE)
validator <- function(role) function(x) .valid_stamp(
  x, sw_v21_config, role, generation_md5, runtime_signature,
  authorization_md5)

selected_full_path <- file.path(output_dir, "fit_selected_full.rds")
selected_full <- .run_or_load(
  selected_full_path, cli$force,
  .stamp(do.call(scmix_tune_matrix, c(common, list(
    K = sw_v21_config$folds$inner_K, refit = TRUE,
    seed = seed + 101L))), sw_v21_config, "selected_full",
    generation_md5, runtime_signature, authorization_md5),
  validator("selected_full"))

selected_nested_path <- file.path(output_dir, "fit_selected_nested.rds")
selected_nested <- .run_or_load(
  selected_nested_path, cli$force,
  .stamp(do.call(scmix_tune_outer_matrix, c(common, list(
    outer_K = sw_v21_config$folds$outer_K,
    inner_K = sw_v21_config$folds$inner_K,
    outer_fold_id = outer_fold, seed = seed + 201L))),
    sw_v21_config, "selected_nested", generation_md5, runtime_signature,
    authorization_md5),
  validator("selected_nested"))

selected_assembled_path <- file.path(output_dir, "fit_selected_assembled.rds")
selected_assembled <- .run_or_load(
  selected_assembled_path, cli$force,
  .stamp(scmix_assemble_nested(
    selected_nested, attr_names = colnames(deltaX), z_names = colnames(Z),
    require_optimization_gate = FALSE, diagnostic_only = TRUE),
    sw_v21_config, "selected_assembled", generation_md5, runtime_signature,
    authorization_md5),
  validator("selected_assembled"))

constant_common <- common
constant_common$grid <- sw_v21_config$grid[constant_index]
constant_full_path <- file.path(output_dir, "fit_constant_full.rds")
constant_full <- .run_or_load(
  constant_full_path, cli$force,
  .stamp(do.call(scmix_tune_matrix, c(constant_common, list(
    K = sw_v21_config$folds$inner_K, refit = TRUE,
    seed = seed + 101L))), sw_v21_config, "constant_full",
    generation_md5, runtime_signature, authorization_md5),
  validator("constant_full"))

constant_nested_path <- file.path(output_dir, "fit_constant_nested.rds")
constant_nested <- .run_or_load(
  constant_nested_path, cli$force,
  .stamp(do.call(scmix_tune_outer_matrix, c(constant_common, list(
    outer_K = sw_v21_config$folds$outer_K,
    inner_K = sw_v21_config$folds$inner_K,
    outer_fold_id = outer_fold, seed = seed + 201L))),
    sw_v21_config, "constant_nested", generation_md5, runtime_signature,
    authorization_md5),
  validator("constant_nested"))

constant_assembled_path <- file.path(output_dir, "fit_constant_assembled.rds")
constant_assembled <- .run_or_load(
  constant_assembled_path, cli$force,
  .stamp(scmix_assemble_nested(
    constant_nested, attr_names = colnames(deltaX), z_names = colnames(Z),
    require_optimization_gate = FALSE, diagnostic_only = TRUE),
    sw_v21_config, "constant_assembled", generation_md5, runtime_signature,
    authorization_md5),
  validator("constant_assembled"))

selected_prediction <- scmix_heldout_predictions(selected_assembled)
constant_prediction <- scmix_heldout_predictions(constant_assembled)
ids <- unique(rid)
v1_ids <- names(v1_diagnostic$pooled$sequence_score)
if (!identical(ids, v1_ids)) {
  stop("The frozen v1 scores do not align with respondent order.", call. = FALSE)
}
scores <- list(
  selected_procedure_q1 = as.numeric(
    selected_prediction$sequence_loglik[ids]),
  exact_constant_q1 = as.numeric(constant_prediction$sequence_loglik[ids]),
  v1_primary_q1 = as.numeric(v1_diagnostic$primary_sequence_score),
  v1_pooled_q1 = as.numeric(v1_diagnostic$pooled$sequence_score[ids]),
  v1_targeted_q1 = as.numeric(v1_diagnostic$targeted$sequence_score[ids]))
if (any(!vapply(scores, function(x) length(x) == length(ids) &&
                              all(is.finite(x)), logical(1L)))) {
  stop("At least one respondent-sequence score vector is malformed.",
       call. = FALSE)
}
party_task <- .party_labels(prepared, rid)
party_respondent <- party_task[match(ids, rid)]
score_table <- .score_summary(scores, party_respondent)
paired_table <- .paired_summary(scores, party_respondent)
candidate_table <- .candidate_table(selected_full, selected_nested)
selection_table <- .selection_table(selected_full, selected_nested)

margin <- sw_v21_config$postpilot_guardrail$noninferiority_margin
guardrail <- .sw_v21_guardrail_decision(
  scores$selected_procedure_q1, scores$exact_constant_q1, margin)
score_difference <- guardrail$mean_difference
score_difference_se <- guardrail$respondent_se
guardrail_pass <- guardrail$pass

selected_gate <- .tuning_gate(selected_full) &&
  all(vapply(selected_nested$tuning, .tuning_gate, logical(1L))) &&
  all(selected_assembled$optimization$gate_by_fold)
constant_gate <- .tuning_gate(constant_full) &&
  all(vapply(constant_nested$tuning, .tuning_gate, logical(1L))) &&
  all(constant_assembled$optimization$gate_by_fold)
constant_grid_gate <- all(vapply(
  c(list(selected_full), selected_nested$tuning), function(x) {
    j <- which(vapply(x$specifications,
      function(s) identical(s$mean_family, "constant"), logical(1L)))
    length(j) == 1L && isTRUE(x$candidates$selection_eligible[j])
  }, logical(1L)))
input_md5_after <- .sc_md5_paths(input_paths)
inputs_unchanged <- .sc_identical_md5_vectors(
  generation_md5, input_md5_after)
authorization_md5_after <- if (file.exists(sw_v21_config$authorization_file)) {
  unname(tools::md5sum(sw_v21_config$authorization_file))
} else NA_character_
authorization_still_valid <- identical(
  authorization_md5_after, authorization_md5) &&
  .sw_v21_authorization_valid(
    tryCatch(readRDS(sw_v21_config$authorization_file),
             error = function(e) NULL),
    sw_v21_config, config_path, predecessor_config_path,
    readRDS(pilot_manifest_path), pilot_manifest_path, generation_md5,
    runtime_signature)
pilot_still_valid <- .sw_v21_failed_pilot_valid(
  readRDS(pilot_manifest_path), pilot_manifest_path, runtime_signature,
  sw_v21_config$predecessor)
fold_gate <- identical(as.integer(outer_fold),
                       as.integer(v1_nested$outer_fold_id))
procedural_primary_available <- all(c(
  fold_gate, pilot_still_valid, inputs_unchanged,
  authorization_still_valid, prepilot_spec_unchanged,
  constant_grid_gate, selected_gate, constant_gate))
if (procedural_primary_available) {
  fallback_applied <- guardrail$fallback_applied
  reported_primary <- guardrail$reported_primary
  reported_full_path <- if (fallback_applied) constant_full_path else
    selected_full_path
  reported_nested_path <- if (fallback_applied) constant_nested_path else
    selected_nested_path
  reported_assembled_path <- if (fallback_applied) constant_assembled_path else
    selected_assembled_path
} else {
  fallback_applied <- NA
  reported_primary <- "unavailable_due_to_failed_computational_or_provenance_gate"
  reported_full_path <- reported_nested_path <- reported_assembled_path <-
    NA_character_
}
fallback_rule_gate <- procedural_primary_available &&
  identical(fallback_applied, !guardrail_pass)
reported_primary_gate <- procedural_primary_available &&
  if (isTRUE(fallback_applied)) constant_gate else selected_gate

gates <- data.frame(
  gate = c(
    "exact v1 outer folds reused",
    "failed v2 pilot remains byte-and-artifact valid",
    "complete generation inputs unchanged",
    "execution authorization remains byte-identical and valid",
    "pre-pilot production grid and schedule mechanically unchanged",
    "exact constant eligible in every tuning problem",
    "selected procedure computational/nesting/bound gates",
    "constant procedure computational/nesting/bound gates",
    "selected procedure meets post-pilot -0.001 guardrail",
    "fallback applied exactly when guardrail missed",
    "reported primary computational gate",
    "formal inference enabled", "outcome-blind analysis"),
  pass = c(
    fold_gate,
    pilot_still_valid, inputs_unchanged, authorization_still_valid,
    prepilot_spec_unchanged, constant_grid_gate,
    selected_gate, constant_gate, guardrail_pass,
    fallback_rule_gate, reported_primary_gate,
    FALSE, FALSE),
  required_for_final_success = c(
    rep(TRUE, 8L), FALSE, TRUE, TRUE, FALSE, FALSE),
  value = c(
    "exact locked v1 respondent-fold vector", as.character(pilot_still_valid),
    as.character(inputs_unchanged), as.character(authorization_still_valid),
    as.character(prepilot_spec_unchanged), as.character(constant_grid_gate),
    as.character(selected_gate), as.character(constant_gate),
    signif(score_difference, 10),
    paste0("fallback=", fallback_applied), reported_primary,
    "FALSE", "FALSE"), stringsAsFactors = FALSE)
required <- gates$required_for_final_success
final_analysis_success <- all(
  !is.na(gates$pass[required]) & gates$pass[required])
if (!inputs_unchanged || !authorization_still_valid) {
  stop("A generation input or authorization changed during v2.1 execution; final outputs are invalid.",
       call. = FALSE)
}

tables <- list(
  candidates = candidate_table, selections = selection_table,
  sequence_score_summary = score_table,
  sequence_score_paired_differences = paired_table,
  reporting_gates = gates)
for (nm in names(tables)) {
  .write_csv(tables[[nm]], file.path(output_dir, paste0(nm, ".csv")))
}

pointer_path <- NA_character_
if (procedural_primary_available) {
  pointer <- list(
    schema_version = "sw2022-v2.1-reported-primary-pointer-v1",
    reported_primary = reported_primary,
    fallback_applied = fallback_applied,
    full_fit_path = reported_full_path,
    nested_fit_path = reported_nested_path,
    assembled_fit_path = reported_assembled_path,
    selected_procedure_paths = list(
      full = selected_full_path, nested = selected_nested_path,
      assembled = selected_assembled_path),
    exact_constant_paths = list(
      full = constant_full_path, nested = constant_nested_path,
      assembled = constant_assembled_path),
    score_difference = score_difference,
    score_difference_respondent_se = score_difference_se,
    noninferiority_margin = margin,
    descriptive_only = TRUE, formal_test = FALSE,
    formal_inference_available = FALSE, outcome_blind = FALSE,
    generation_input_md5 = generation_md5,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5)
  pointer_path <- file.path(output_dir, "reported_primary_pointer.rds")
  .atomic_save(pointer, pointer_path, portable = FALSE)
}

result <- list(
  schema_version = "sw2022-v2.1-postpilot-final-analysis-v1",
  configuration = sw_v21_config,
  final_analysis_success = final_analysis_success,
  selected_procedure = list(
    full = selected_full$selected,
    outer = lapply(selected_nested$tuning, `[[`, "selected")),
  postpilot_guardrail = list(
    mean_difference = score_difference,
    respondent_se = score_difference_se, margin = margin,
    pass = guardrail_pass, formal_test = FALSE),
  reported_primary = reported_primary,
  procedural_primary_available = procedural_primary_available,
  fallback_applied = fallback_applied,
  pointer_path = pointer_path,
  sequence_scores = scores, tables = tables,
  formal_inference_available = FALSE, outcome_blind = FALSE,
  production_result = FALSE,
  failed_v2_pilot_rewritten = FALSE,
  input_paths = input_paths,
  generation_input_md5 = generation_md5,
  completion_input_md5 = input_md5_after,
  runtime_signature = runtime_signature,
  authorization_md5 = authorization_md5,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  session_info = utils::capture.output(sessionInfo()))
result_path <- file.path(output_dir, "postpilot_final_result.rds")
.atomic_save(result, result_path, portable = FALSE)

artifact_paths <- c(
  selected_full_path, selected_nested_path, selected_assembled_path,
  constant_full_path, constant_nested_path, constant_assembled_path,
  pointer_path, result_path,
  file.path(output_dir, paste0(names(tables), ".csv")))
artifact_paths <- artifact_paths[!is.na(artifact_paths) &
                                   file.exists(artifact_paths)]
manifest <- list(
  schema_version = "sw2022-v2.1-postpilot-final-manifest-v1",
  configuration_version = sw_v21_config$version,
  final_analysis_success = final_analysis_success,
  reported_primary = reported_primary,
  procedural_primary_available = procedural_primary_available,
  fallback_applied = fallback_applied,
  postpilot_guardrail = result$postpilot_guardrail,
  input_paths = input_paths,
  generation_input_md5 = generation_md5,
  completion_input_md5 = input_md5_after,
  runtime_signature = runtime_signature,
  authorization_md5 = authorization_md5,
  artifacts = stats::setNames(unname(tools::md5sum(artifact_paths)),
                              basename(artifact_paths)),
  failed_pilot_manifest_md5 = unname(tools::md5sum(pilot_manifest_path)),
  failed_pilot_artifacts_md5 = pilot_manifest$artifacts,
  formal_inference_available = FALSE, outcome_blind = FALSE,
  production_result = FALSE,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
.atomic_save(manifest, file.path(output_dir, "manifest.rds"), portable = FALSE)

report <- c(
  "# Saha--Weeks v2.1 post-pilot final analysis",
  "",
  paste0("- Final execution gate: `", final_analysis_success, "`."),
  paste0("- Selected-minus-constant held-out sequence score: `",
         signif(score_difference, 9), "` (respondent SE `",
         signif(score_difference_se, 9), "`)."),
  paste0("- Post-pilot descriptive margin: `", margin, "`."),
  paste0("- Guardrail pass: `", guardrail_pass, "`."),
  paste0("- Procedural primary available: `",
         procedural_primary_available, "`."),
  paste0("- Fallback applied: `", fallback_applied, "`."),
  paste0("- Reported primary: `", reported_primary, "`."),
  "- The selected CV procedure is retained unchanged in the diagnostic artifacts.",
  if (!procedural_primary_available) paste(
    "- No reported-primary pointer was emitted because a required",
    "computational or provenance gate failed."),
  "- This outcome-informed analysis provides no formal inference.",
  "- The failed v2 pilot and all prior artifacts were read-only.")
writeLines(report, file.path(output_dir, "AUDIT.md"), useBytes = TRUE)

cat("v2.1 post-pilot final analysis complete; success=",
    final_analysis_success, "; primary=", reported_primary,
    "; output=", output_dir, "\n", sep = "")
