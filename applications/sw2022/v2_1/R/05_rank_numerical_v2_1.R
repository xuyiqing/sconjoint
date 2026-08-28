#!/usr/bin/env Rscript

## Saha--Weeks v2.1 common-fold q=0/1/2 rank sensitivity and GH checks.
## Outcome-informed diagnostics only: rank is never selected and formal
## inference remains unavailable. Execution is fail-closed until the completed
## v2.1 pointer/manifest and a separately reviewed authorization all validate.

options(stringsAsFactors = FALSE, warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(args) {
  out <- list(stage = "all", force = FALSE)
  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- bits[[1L]]
    if (!key %in% names(out)) stop("Unknown argument --", key)
    out[[key]] <- paste(bits[-1L], collapse = "=")
  }
  out$force <- tolower(as.character(out$force)) %in% c("1", "true", "yes")
  if (!out$stage %in% c("rank", "q1_refinement", "q2_refinement", "all")) {
    stop("--stage must be rank, q1_refinement, q2_refinement, or all.",
         call. = FALSE)
  }
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

.atomic_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write ", path, call. = FALSE)
  }
  invisible(path)
}

.atomic_write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(x, tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write ", path, call. = FALSE)
  }
  invisible(path)
}

cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
root <- normalizePath(file.path(dirname(.script_file()), "..", "..", "..",
                                ".."), mustWork = TRUE)
app <- file.path(root, "applications", "sw2022")
options(sconjoint.sw_application_root = app)
parent_config_path <- file.path(
  app, "v2_1", "config", "analysis_config_v2_1.R")
parent_contract_path <- file.path(
  app, "v2_1", "R", "postpilot_contract_v2_1.R")
parent_runner_path <- file.path(
  app, "v2_1", "R", "04_fit_postpilot_final_v2_1.R")
parent_authorization_creator_path <- file.path(
  app, "v2_1", "R", "00_create_final_analysis_authorization_v2_1.R")
rank_config_path <- file.path(
  app, "v2_1", "config", "rank_numerical_config_v2_1.R")
rank_contract_path <- file.path(
  app, "v2_1", "R", "rank_numerical_contract_v2_1.R")
rank_authorization_creator_path <- file.path(
  app, "v2_1", "R", "00_create_rank_numerical_authorization_v2_1.R")
provenance_source <- file.path(root, "R", "provenance.R")
source(provenance_source, local = FALSE)
source(parent_config_path, local = FALSE)
source(rank_config_path, local = FALSE)
source(rank_contract_path, local = FALSE)

if (!identical(sw_v21_rank_config$parent_version, sw_v21_config$version) ||
    !identical(sw_v21_rank_config$ranks, c(0L, 1L, 2L)) ||
    !identical(sw_v21_rank_config$rank_selected, FALSE) ||
    !identical(sw_v21_rank_config$formal_inference_available, FALSE) ||
    !identical(sw_v21_config$formal_inference_available, FALSE) ||
    !identical(sw_v21_rank_config$base_nodes,
               c(`0` = 31L, `1` = 31L, `2` = 15L)) ||
    !identical(sw_v21_rank_config$refinement_nodes$`1`,
               c(15L, 31L, 45L)) ||
    !identical(sw_v21_rank_config$refinement_nodes$`2`,
               c(9L, 15L, 21L))) {
  stop("The downstream rank/numerical specification is malformed.",
       call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.",
       call. = FALSE)
}
runtime_signature <- .sc_runtime_signature(file.path(root, "DESCRIPTION"))
parent_manifest_path <- file.path(sw_v21_config$output_root, "manifest.rds")
pointer_path <- file.path(sw_v21_config$output_root,
                          "reported_primary_pointer.rds")
if (!file.exists(parent_manifest_path) || !file.exists(pointer_path)) {
  stop("The completed v2.1 final manifest and reported-primary pointer are required. No rank fit was started.",
       call. = FALSE)
}
parent_manifest <- readRDS(parent_manifest_path)
pointer <- readRDS(pointer_path)
if (!.sw_v21_rank_final_bundle_valid(
    pointer, pointer_path, parent_manifest, parent_manifest_path,
    sw_v21_config, runtime_signature)) {
  stop("The completed v2.1 pointer/manifest bundle failed validation. No rank fit was started.",
       call. = FALSE)
}
generation_paths <- .sw_v21_rank_generation_paths(
  root = root, app = app,
  rank_config_path = rank_config_path,
  rank_contract_path = rank_contract_path,
  rank_runner_path = .script_file(),
  authorization_creator_path = rank_authorization_creator_path,
  parent_config_path = parent_config_path,
  parent_contract_path = parent_contract_path,
  parent_runner_path = parent_runner_path,
  parent_authorization_creator_path = parent_authorization_creator_path,
  pointer_path = pointer_path, manifest_path = parent_manifest_path,
  manifest = parent_manifest)
generation_md5 <- .sc_md5_paths(generation_paths)
authorization <- if (file.exists(sw_v21_rank_config$authorization_file)) {
  tryCatch(readRDS(sw_v21_rank_config$authorization_file),
           error = function(e) NULL)
} else NULL
if (!.sw_v21_rank_authorization_valid(
    authorization, sw_v21_rank_config, rank_config_path, generation_md5,
    runtime_signature, pointer, pointer_path, parent_manifest_path)) {
  stop(
    "The rank/numerical analysis is fail-closed. Create a separately reviewed authorization that binds the completed v2.1 bundle, current source/input hashes, runtime, outcome-informed status, absence of formal inference, and no-rank-selection rule. No fit was started.",
    call. = FALSE)
}
authorization_md5 <- unname(tools::md5sum(
  sw_v21_rank_config$authorization_file))

suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
ns_fun <- function(name) get(name, envir = asNamespace("sconjoint"),
                             inherits = FALSE)
stage_gate <- ns_fun(".sc_comp_inner_fit_gate")

prepared <- readRDS(sw_v21_config$input$prepared)
deltaX <- as.matrix(prepared$deltaX)
y <- as.numeric(prepared$y)
Z <- as.matrix(prepared[[sw_v21_config$input$primary_Z]])
rid <- as.character(prepared$respondent_id)
ids <- unique(rid)
first <- !duplicated(rid)
if (!identical(nrow(deltaX), 3573L) || length(ids) != 1191L ||
    nrow(deltaX) != nrow(Z) || nrow(deltaX) != length(y) ||
    nrow(deltaX) != length(rid) || any(!is.finite(deltaX)) ||
    any(!is.finite(Z)) || any(!y %in% c(0, 1)) ||
    is.null(colnames(deltaX)) || is.null(colnames(Z))) {
  stop("The frozen Saha--Weeks analysis sample is malformed.", call. = FALSE)
}
if (any(vapply(split(seq_len(nrow(Z)), rid), function(ii) {
  max(abs(sweep(Z[ii, , drop = FALSE], 2L, Z[ii[1L], ], `-`))) > 1e-12
}, logical(1L)))) {
  stop("Moderators are not constant within respondent.", call. = FALSE)
}

v1_nested <- readRDS(sw_v21_config$input$v1_nested)
outer_fold_id <- as.integer(v1_nested$outer_fold_id)
if (length(outer_fold_id) != nrow(deltaX) ||
    !setequal(unique(outer_fold_id), seq_len(sw_v21_config$folds$outer_K)) ||
    any(vapply(split(outer_fold_id, rid),
               function(x) length(unique(x)) != 1L, logical(1L)))) {
  stop("The exact locked respondent outer folds are unavailable.",
       call. = FALSE)
}
active_panels <- "selected_procedure"
if (isTRUE(pointer$fallback_applied)) {
  active_panels <- c(active_panels, "exact_constant")
}
parent_paths <- list(
  selected_procedure = pointer$selected_procedure_paths,
  exact_constant = pointer$exact_constant_paths)

.stamp <- function(x, role, panel, q, nodes) {
  x$sw_v21_rank_numerical_specification <- list(
    config_version = sw_v21_rank_config$version,
    parent_version = sw_v21_config$version,
    role = role, panel = panel, q = as.integer(q),
    nodes = as.integer(nodes),
    generation_input_md5 = generation_md5,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5,
    outer_fold_id = outer_fold_id,
    centered_alpha_architecture = "mixed-conjoint-mean-family-v2",
    rank_selected = FALSE, formal_inference_available = FALSE,
    outcome_blind = FALSE)
  x
}

.valid_stamp <- function(x, role, panel, q, nodes) {
  s <- x$sw_v21_rank_numerical_specification
  is.list(s) &&
    identical(s$config_version, sw_v21_rank_config$version) &&
    identical(s$parent_version, sw_v21_config$version) &&
    identical(s$role, role) && identical(s$panel, panel) &&
    identical(s$q, as.integer(q)) &&
    identical(s$nodes, as.integer(nodes)) &&
    .sc_identical_md5_vectors(s$generation_input_md5, generation_md5) &&
    identical(s$runtime_signature, runtime_signature) &&
    identical(s$authorization_md5, authorization_md5) &&
    identical(as.integer(s$outer_fold_id), outer_fold_id) &&
    identical(s$centered_alpha_architecture,
              "mixed-conjoint-mean-family-v2") &&
    identical(s$rank_selected, FALSE) &&
    identical(s$formal_inference_available, FALSE) &&
    identical(s$outcome_blind, FALSE)
}

.run_or_load <- function(path, overwrite, code, validator,
                         portable = TRUE) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    out <- readRDS(path)
    if (!isTRUE(validator(out))) {
      stop("Stale or incompatible rank/numerical checkpoint: ", path,
           ". Rerun the complete stage with --force=true.", call. = FALSE)
    }
    message("checkpoint: loading ", path)
    return(out)
  }
  out <- force(code)
  if (!isTRUE(validator(out))) {
    stop("A new rank/numerical checkpoint failed its own stamp: ", path,
         call. = FALSE)
  }
  .atomic_save(out, path, portable = portable)
  message("checkpoint: wrote ", path)
  out
}

.tuning_gate <- function(x, q) {
  .sw_v21_rank_tuning_gate(x, q = q, stage_gate = stage_gate)
}

.triplet_gate <- function(x, panel, q, nodes) {
  is.list(x) &&
    .valid_stamp(x$full, "full", panel, q, nodes) &&
    .valid_stamp(x$nested, "nested", panel, q, nodes) &&
    .valid_stamp(x$assembled, "assembled", panel, q, nodes) &&
    .tuning_gate(x$full, q) &&
    .sw_v21_rank_nested_gate(
      x$nested, q, outer_fold_id, stage_gate) &&
    isTRUE(x$assembled$computational_gate_pass) &&
    all(x$assembled$optimization$gate_by_fold) &&
    identical(as.integer(x$assembled$fold_id),
              match(as.character(outer_fold_id),
                    x$nested$outer_folds))
}

.validate_imported_q1 <- function(panel) {
  paths <- parent_paths[[panel]]
  x <- list(full = readRDS(paths$full), nested = readRDS(paths$nested),
            assembled = readRDS(paths$assembled), paths = paths,
            imported = TRUE)
  if (!.tuning_gate(x$full, 1L) ||
      !.sw_v21_rank_nested_gate(
        x$nested, 1L, outer_fold_id, stage_gate) ||
      !all(x$assembled$optimization$gate_by_fold) ||
      !identical(as.integer(x$nested$outer_fold_id), outer_fold_id) ||
      !identical(as.integer(x$assembled$fold_id),
                 match(as.character(outer_fold_id),
                       x$nested$outer_folds))) {
    stop("The imported q=1 ", panel,
         " artifact failed its computational, centered-alpha, or fold gate.",
         call. = FALSE)
  }
  x
}
imported_q1 <- stats::setNames(lapply(active_panels, .validate_imported_q1),
                               active_panels)
if ("exact_constant" %in% active_panels &&
    !all(vapply(seq_along(
      imported_q1$selected_procedure$nested$tuning), function(k) {
        identical(
          imported_q1$selected_procedure$nested$tuning[[k]]$fold_id,
          imported_q1$exact_constant$nested$tuning[[k]]$fold_id)
      }, logical(1L)))) {
  stop("Selected and exact-constant q=1 panels do not share inner folds.",
       call. = FALSE)
}

output_dir <- sw_v21_rank_config$output_root
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
prior_manifest_path <- file.path(output_dir, "manifest.rds")
if (file.exists(prior_manifest_path)) {
  unlink(prior_manifest_path)
  if (file.exists(prior_manifest_path)) {
    stop("Could not invalidate the prior downstream manifest before rerun.",
         call. = FALSE)
  }
}
opt <- sw_v21_config$optimizer

.grid_for <- function(panel, q, nodes) {
  .sw_v21_rank_grid_at_nodes(
    sw_v21_config$grid, panel = panel, q = q, nodes = nodes)
}

.fit_triplet <- function(panel, q, nodes, overwrite = cli$force) {
  if (q == 1L && nodes == sw_v21_rank_config$base_nodes[["1"]]) {
    return(imported_q1[[panel]])
  }
  prefix <- paste0(panel, "_q", q, "_nodes", nodes)
  paths <- list(
    full = file.path(output_dir, paste0(prefix, "_full.rds")),
    nested = file.path(output_dir, paste0(prefix, "_nested.rds")),
    assembled = file.path(output_dir, paste0(prefix, "_assembled.rds")))
  common <- list(
    deltaX = deltaX, y = y, Z = Z, respondent_id = rid,
    grid = .grid_for(panel, q, nodes), q = as.integer(q),
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
  full <- .run_or_load(
    paths$full, overwrite,
    .stamp(do.call(scmix_tune_matrix, c(common, list(
      K = sw_v21_config$folds$inner_K, refit = TRUE,
      seed = as.integer(opt$seed) + 101L))),
      "full", panel, q, nodes),
    function(x) .valid_stamp(x, "full", panel, q, nodes) &&
      .tuning_gate(x, q))
  nested <- .run_or_load(
    paths$nested, overwrite,
    .stamp(do.call(scmix_tune_outer_matrix, c(common, list(
      outer_K = sw_v21_config$folds$outer_K,
      inner_K = sw_v21_config$folds$inner_K,
      outer_fold_id = outer_fold_id,
      seed = as.integer(opt$seed) + 201L))),
      "nested", panel, q, nodes),
    function(x) .valid_stamp(x, "nested", panel, q, nodes) &&
      .sw_v21_rank_nested_gate(x, q, outer_fold_id, stage_gate))
  assembled <- .run_or_load(
    paths$assembled, overwrite,
    .stamp(scmix_assemble_nested(
      nested, attr_names = colnames(deltaX), z_names = colnames(Z),
      require_optimization_gate = TRUE, diagnostic_only = FALSE),
      "assembled", panel, q, nodes),
    function(x) .valid_stamp(x, "assembled", panel, q, nodes) &&
      isTRUE(x$computational_gate_pass) &&
      all(x$optimization$gate_by_fold) &&
      identical(as.integer(x$fold_id),
                match(as.character(outer_fold_id), nested$outer_folds)))
  out <- list(full = full, nested = nested, assembled = assembled,
              paths = paths, imported = FALSE)
  if (!.triplet_gate(out, panel, q, nodes)) {
    stop("A fitted triplet failed its fail-closed gate: ", prefix,
         call. = FALSE)
  }
  out
}

.prediction <- function(assembled) {
  scmix_heldout_predictions(
    assembled, task_order = prepared$task,
    include_counts = FALSE, include_adjacent = FALSE,
    include_repeated = FALSE)
}

.respondent_scores <- function(assembled) {
  prediction <- .prediction(assembled)
  score <- as.numeric(prediction$sequence_loglik[ids])
  if (length(score) != length(ids) || any(!is.finite(score))) {
    stop("A held-out respondent score vector is malformed.", call. = FALSE)
  }
  score
}

.direction <- function(...) {
  out <- stats::setNames(numeric(ncol(deltaX)), colnames(deltaX))
  entries <- list(...)
  for (entry in entries) out[[entry[[1L]]]] <-
    out[[entry[[1L]]]] + entry[[2L]]
  unname(out)
}
.e <- function(name, value = 1) list(name, value)
qoi_directions <- list(
  female_vs_male = .direction(.e("cand_genderMale", -1)),
  run_yes_vs_no = .direction(.e("cand_runYes", 1)),
  agenda_moderate_vs_very_few = .direction(
    .e("cand_agendaModerate.Changes", 1)),
  agenda_complete_vs_very_few = .direction(
    .e("cand_agendaComplete.Overhaul", 1)),
  talent_hard_working_vs_assertive = .direction(
    .e("cand_talentHard.Working", 1)))

meta <- prepared$respondent_meta
meta <- meta[match(ids, as.character(meta$respondent_id)), , drop = FALSE]
party <- ifelse(grepl("Republican", meta$party), "Republican",
                ifelse(grepl("Independent", meta$party), "Independent",
                       "Democrat"))
respondent_gender <- tolower(as.character(meta$respondent_gender))
if (anyNA(party) || anyNA(respondent_gender)) {
  stop("Respondent subgroup mapping failed.", call. = FALSE)
}

.qoi_vector <- function(full) {
  refit <- full$refit
  mu <- as.matrix(refit$mu[first, , drop = FALSE])
  Sigma <- as.matrix(refit$Sigma)
  if (!identical(dim(mu), c(length(ids), ncol(deltaX))) ||
      !identical(dim(Sigma), c(ncol(deltaX), ncol(deltaX))) ||
      any(!is.finite(mu)) || any(!is.finite(Sigma))) {
    stop("A full-sample plug-in fit is malformed.", call. = FALSE)
  }
  theta <- colMeans(mu)
  out <- c(kappa = as.numeric(refit$kappa),
           stats::setNames(theta, paste0("theta.", colnames(deltaX))))
  for (nm in names(qoi_directions)) {
    d <- qoi_directions[[nm]]
    conditional <- as.numeric(mu %*% d)
    residual <- as.numeric(crossprod(d, Sigma %*% d))
    out[paste0("tau.", nm)] <- mean(conditional)
    out[paste0("heterogeneity.observed.", nm)] <-
      mean((conditional - mean(conditional))^2)
    out[paste0("heterogeneity.residual.", nm)] <- residual
    out[paste0("heterogeneity.total.", nm)] <-
      out[[paste0("heterogeneity.observed.", nm)]] + residual
  }
  d_gender <- qoi_directions$female_vs_male
  for (g in c("Democrat", "Independent", "Republican")) {
    out[paste0("tau.female_vs_male.party_", tolower(g))] <-
      mean(as.numeric(mu[party == g, , drop = FALSE] %*% d_gender))
  }
  for (g in c("female", "male")) {
    out[paste0("tau.female_vs_male.respondent_gender_", g)] <-
      mean(as.numeric(mu[respondent_gender == g, , drop = FALSE] %*%
                        d_gender))
  }
  eig <- sort(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values,
              decreasing = TRUE)
  out <- c(out, Sigma_trace = sum(diag(Sigma)),
           stats::setNames(eig, paste0("Sigma.eigen_", seq_along(eig))))
  if (any(!is.finite(out)) || anyDuplicated(names(out))) {
    stop("A plug-in QOI vector is nonfinite or ambiguously named.",
         call. = FALSE)
  }
  out
}

.selection_table <- function(panel, q, nodes, triplet) {
  rows <- list(data.frame(
    panel = panel, q = q, nodes = nodes, scope = "full_sample",
    outer_fold = NA_character_, candidate = triplet$full$selected$name,
    mean_family = triplet$full$selected$mean_family,
    weight_decay = triplet$full$selected$weight_decay,
    stringsAsFactors = FALSE))
  for (k in seq_along(triplet$nested$tuning)) {
    selected <- triplet$nested$tuning[[k]]$selected
    rows[[length(rows) + 1L]] <- data.frame(
      panel = panel, q = q, nodes = nodes, scope = "outer_training",
      outer_fold = as.character(triplet$nested$outer_folds[[k]]),
      candidate = selected$name, mean_family = selected$mean_family,
      weight_decay = selected$weight_decay, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

.fold_audit <- function(panel, fits) {
  q1 <- fits[["1"]]$nested
  do.call(rbind, lapply(names(fits), function(key) {
    candidate <- fits[[key]]$nested
    do.call(rbind, lapply(seq_along(candidate$tuning), function(k) {
      data.frame(
        panel = panel, q = as.integer(key),
        outer_fold = as.character(candidate$outer_folds[[k]]),
        exact_locked_outer_fold_vector = identical(
          as.integer(candidate$outer_fold_id), outer_fold_id),
        same_outer_index_as_q1 = identical(
          candidate$outer_fold_index, q1$outer_fold_index),
        same_inner_assignment_as_q1 = identical(
          candidate$tuning[[k]]$fold_id, q1$tuning[[k]]$fold_id),
        stringsAsFactors = FALSE)
    }))
  }))
}

.save_stage_result <- function(x, path, role, panel, q, nodes) {
  out <- .stamp(x, role, panel, q, nodes)
  .atomic_save(out, path, portable = FALSE)
  out
}

.artifact_binding <- function(fits) {
  paths <- unlist(lapply(fits, `[[`, "paths"), use.names = TRUE)
  if (!is.character(paths) || !length(paths) || any(!file.exists(paths)) ||
      is.null(names(paths)) || any(!nzchar(names(paths))) ||
      anyDuplicated(names(paths))) {
    stop("Stage fit dependencies are missing or ambiguously named.",
         call. = FALSE)
  }
  list(paths = paths, md5 = .sc_md5_paths(paths))
}

.run_rank_panel <- function(panel) {
  fits <- list(
    `0` = .fit_triplet(
      panel, 0L, sw_v21_rank_config$base_nodes[["0"]]),
    `1` = imported_q1[[panel]],
    `2` = .fit_triplet(
      panel, 2L, sw_v21_rank_config$base_nodes[["2"]]))
  scores <- lapply(fits, function(x) .respondent_scores(x$assembled))
  qois <- lapply(fits, function(x) .qoi_vector(x$full))
  if (!all(vapply(qois, function(x) identical(names(x), names(qois[[1L]])),
                  logical(1L)))) {
    stop("Rank QOI vectors are not conformable in panel ", panel,
         call. = FALSE)
  }
  score_table <- do.call(rbind, lapply(names(scores), function(q) {
    x <- scores[[q]]
    data.frame(
      panel = panel, q = as.integer(q),
      mean_complete_sequence_log_score = mean(x),
      respondent_se = stats::sd(x) / sqrt(length(x)),
      n_respondents = length(x), rank_selected = FALSE,
      formal_inference_available = FALSE, stringsAsFactors = FALSE)
  }))
  comparisons <- list(q1_minus_q0 = c("1", "0"),
                      q2_minus_q1 = c("2", "1"),
                      q2_minus_q0 = c("2", "0"))
  paired <- do.call(rbind, lapply(names(comparisons), function(nm) {
    pair <- comparisons[[nm]]
    d <- scores[[pair[[1L]]]] - scores[[pair[[2L]]]]
    data.frame(
      panel = panel, comparison = nm, mean_difference = mean(d),
      respondent_se = stats::sd(d) / sqrt(length(d)),
      respondent_l2 = sqrt(mean(d^2)), n_respondents = length(d),
      descriptive_only = TRUE, formal_test = FALSE,
      stringsAsFactors = FALSE)
  }))
  qoi_table <- do.call(rbind, lapply(names(qois), function(q) {
    data.frame(
      panel = panel, q = as.integer(q), quantity = names(qois[[q]]),
      estimate = as.numeric(qois[[q]]),
      role = "fixed-rank descriptive plug-in sensitivity",
      formal_inference_available = FALSE, stringsAsFactors = FALSE)
  }))
  selections <- do.call(rbind, lapply(names(fits), function(q) {
    .selection_table(panel, as.integer(q),
                     sw_v21_rank_config$base_nodes[[q]], fits[[q]])
  }))
  folds <- .fold_audit(panel, fits)
  if (!all(folds$exact_locked_outer_fold_vector) ||
      !all(folds$same_outer_index_as_q1) ||
      !all(folds$same_inner_assignment_as_q1)) {
    stop("Common outer/inner fold audit failed in rank panel ", panel,
         call. = FALSE)
  }
  prefix <- paste0(panel, "_rank")
  tables <- list(
    sequence_scores = score_table,
    paired_score_differences = paired,
    qoi = qoi_table, selections = selections, fold_audit = folds)
  table_paths <- stats::setNames(file.path(
    output_dir, paste0(prefix, "_", names(tables), ".csv")), names(tables))
  for (nm in names(tables)) .atomic_write_csv(tables[[nm]], table_paths[[nm]])
  dependencies <- .artifact_binding(fits)
  out <- list(
    panel = panel, ranks = c(0L, 1L, 2L), rank_selected = FALSE,
    primary_rank_changed = FALSE, fit_paths = lapply(fits, `[[`, "paths"),
    imported_q1 = TRUE, scores = scores, qois = qois, tables = tables,
    fit_artifact_paths = dependencies$paths,
    fit_artifact_md5 = dependencies$md5,
    table_paths = table_paths, table_md5 = .sc_md5_paths(table_paths),
    computational_gate_pass = TRUE, common_fold_gate_pass = TRUE,
    formal_inference_available = FALSE, outcome_blind = FALSE,
    interpretation = sw_v21_rank_config$interpretation)
  .save_stage_result(
    out, file.path(output_dir, paste0(prefix, "_result.rds")),
    "rank_result", panel, -1L, -1L)
}

.rotation_table <- function(panel, nodes, assembled) {
  baseline_A <- assembled$A_computational_folds %||% assembled$A_folds
  baseline_scores <- NULL
  rows <- vector("list", length(sw_v21_rank_config$rotation_angles))
  for (j in seq_along(sw_v21_rank_config$rotation_angles)) {
    angle <- sw_v21_rank_config$rotation_angles[[j]]
    R <- .sw_v21_rank_rotation_matrix(angle)
    rotated <- assembled
    rotated_A <- lapply(baseline_A, `%*%`, R)
    rotated$A_folds <- rotated_A
    rotated$A_computational_folds <- rotated_A
    score <- .respondent_scores(rotated)
    if (is.null(baseline_scores)) baseline_scores <- score
    covariance_error <- max(vapply(seq_along(baseline_A), function(k) {
      max(abs(tcrossprod(rotated_A[[k]]) -
                tcrossprod(baseline_A[[k]])))
    }, numeric(1L)))
    difference <- score - baseline_scores
    rows[[j]] <- data.frame(
      panel = panel, q = 2L, nodes = as.integer(nodes),
      angle_radians = angle,
      mean_complete_sequence_log_score = mean(score),
      mean_difference_from_zero_angle = mean(difference),
      respondent_difference_l2 = sqrt(mean(difference^2)),
      respondent_difference_max_abs = max(abs(difference)),
      covariance_max_absolute_error = covariance_error,
      mean_score_gate_pass = abs(mean(difference)) <=
        sw_v21_rank_config$tolerances$rotation_mean_sequence_log_score,
      respondent_l2_gate_pass = sqrt(mean(difference^2)) <=
        sw_v21_rank_config$tolerances$rotation_respondent_score_l2,
      covariance_gate_pass = covariance_error <=
        sw_v21_rank_config$tolerances$
          rotation_covariance_max_absolute_error,
      stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

.run_refinement_panel <- function(panel, q) {
  nodes <- sw_v21_rank_config$refinement_nodes[[as.character(q)]]
  base_nodes <- sw_v21_rank_config$base_nodes[[as.character(q)]]
  fits <- stats::setNames(lapply(nodes, function(m) {
    .fit_triplet(panel, q, as.integer(m),
                 overwrite = if (m == base_nodes) FALSE else cli$force)
  }), as.character(nodes))
  scores <- lapply(fits, function(x) .respondent_scores(x$assembled))
  qois <- lapply(fits, function(x) .qoi_vector(x$full))
  if (!all(vapply(qois, function(x) identical(names(x), names(qois[[1L]])),
                  logical(1L)))) {
    stop("Refinement QOI vectors are not conformable in panel ", panel,
         ", q=", q, call. = FALSE)
  }
  reference <- as.character(max(nodes))
  summary <- do.call(rbind, lapply(names(fits), function(key) {
    ds <- scores[[key]] - scores[[reference]]
    dq <- qois[[key]] - qois[[reference]]
    data.frame(
      panel = panel, q = as.integer(q), nodes = as.integer(key),
      reference_nodes = as.integer(reference),
      mean_complete_sequence_log_score = mean(scores[[key]]),
      mean_sequence_log_score_difference = mean(ds),
      respondent_score_l2 = sqrt(mean(ds^2)),
      respondent_score_max_abs = max(abs(ds)),
      qoi_max_absolute_difference = max(abs(dq)),
      mean_score_gate_pass = abs(mean(ds)) <=
        sw_v21_rank_config$tolerances$mean_sequence_log_score,
      respondent_l2_gate_pass = sqrt(mean(ds^2)) <=
        sw_v21_rank_config$tolerances$respondent_score_l2,
      qoi_gate_pass = max(abs(dq)) <=
        sw_v21_rank_config$tolerances$qoi_max_absolute,
      formal_inference_available = FALSE, stringsAsFactors = FALSE)
  }))
  qoi_table <- do.call(rbind, lapply(names(qois), function(key) {
    data.frame(
      panel = panel, q = as.integer(q), nodes = as.integer(key),
      reference_nodes = as.integer(reference),
      quantity = names(qois[[key]]), estimate = as.numeric(qois[[key]]),
      difference_from_highest_resolution =
        as.numeric(qois[[key]] - qois[[reference]]),
      formal_inference_available = FALSE, stringsAsFactors = FALSE)
  }))
  selections <- do.call(rbind, lapply(names(fits), function(key) {
    .selection_table(panel, q, as.integer(key), fits[[key]])
  }))
  fold_rows <- do.call(rbind, lapply(names(fits), function(key) {
    candidate <- fits[[key]]$nested
    reference_nested <- fits[[reference]]$nested
    do.call(rbind, lapply(seq_along(candidate$tuning), function(k) {
      data.frame(
        panel = panel, q = as.integer(q), nodes = as.integer(key),
        outer_fold = as.character(candidate$outer_folds[[k]]),
        exact_locked_outer_fold_vector = identical(
          as.integer(candidate$outer_fold_id), outer_fold_id),
        same_inner_assignment_as_highest_resolution = identical(
          candidate$tuning[[k]]$fold_id,
          reference_nested$tuning[[k]]$fold_id),
        stringsAsFactors = FALSE)
    }))
  }))
  if (!all(fold_rows$exact_locked_outer_fold_vector) ||
      !all(fold_rows$same_inner_assignment_as_highest_resolution)) {
    stop("Common-fold refinement audit failed in panel ", panel,
         ", q=", q, call. = FALSE)
  }
  rotation <- if (q == 2L) do.call(rbind, lapply(names(fits), function(key) {
    .rotation_table(panel, as.integer(key), fits[[key]]$assembled)
  })) else NULL
  empirical_gate <- all(summary$mean_score_gate_pass) &&
    all(summary$respondent_l2_gate_pass) &&
    all(summary$qoi_gate_pass) &&
    (is.null(rotation) ||
       (all(rotation$mean_score_gate_pass) &&
          all(rotation$respondent_l2_gate_pass) &&
          all(rotation$covariance_gate_pass)))
  prefix <- paste0(panel, "_q", q, "_refinement")
  tables <- list(summary = summary, qoi = qoi_table,
                 selections = selections, fold_audit = fold_rows)
  if (!is.null(rotation)) tables$rotation <- rotation
  table_paths <- stats::setNames(file.path(
    output_dir, paste0(prefix, "_", names(tables), ".csv")), names(tables))
  for (nm in names(tables)) .atomic_write_csv(tables[[nm]], table_paths[[nm]])
  dependencies <- .artifact_binding(fits)
  out <- list(
    panel = panel, q = as.integer(q), nodes = nodes,
    reference_nodes = as.integer(reference),
    procedure = paste(
      "fresh rerun of the complete frozen within-rank grid and controls",
      "at every GH resolution"),
    fit_paths = lapply(fits, `[[`, "paths"), scores = scores, qois = qois,
    tables = tables, computational_gate_pass = TRUE,
    fit_artifact_paths = dependencies$paths,
    fit_artifact_md5 = dependencies$md5,
    table_paths = table_paths, table_md5 = .sc_md5_paths(table_paths),
    common_fold_gate_pass = TRUE,
    empirical_numerical_stability_gate_pass = empirical_gate,
    empirical_gate_is_asymptotic_certificate = FALSE,
    rank_selected = FALSE, formal_inference_available = FALSE,
    outcome_blind = FALSE,
    interpretation = sw_v21_rank_config$interpretation)
  .save_stage_result(
    out, file.path(output_dir, paste0(prefix, "_result.rds")),
    paste0("q", q, "_refinement_result"), panel, q, -1L)
}

if (cli$stage %in% c("rank", "all")) {
  invisible(lapply(active_panels, .run_rank_panel))
}
if (cli$stage %in% c("q1_refinement", "all")) {
  invisible(lapply(active_panels, .run_refinement_panel, q = 1L))
}
if (cli$stage %in% c("q2_refinement", "all")) {
  invisible(lapply(active_panels, .run_refinement_panel, q = 2L))
}

.result_path <- function(panel, component) {
  switch(component,
    rank = file.path(output_dir, paste0(panel, "_rank_result.rds")),
    q1 = file.path(output_dir,
                   paste0(panel, "_q1_refinement_result.rds")),
    q2 = file.path(output_dir,
                   paste0(panel, "_q2_refinement_result.rds")))
}
required_results <- do.call(c, lapply(active_panels, function(panel) {
  stats::setNames(
    c(.result_path(panel, "rank"), .result_path(panel, "q1"),
      .result_path(panel, "q2")),
    paste(panel, c("rank", "q1", "q2"), sep = "."))
}))

if (all(file.exists(required_results))) {
  results <- lapply(required_results, readRDS)
  valid_result <- mapply(function(x, nm) {
    bits <- strsplit(nm, "[.]", perl = TRUE)[[1L]]
    panel <- bits[[1L]]
    component <- bits[[2L]]
    role <- switch(component, rank = "rank_result",
                   q1 = "q1_refinement_result",
                   q2 = "q2_refinement_result")
    q <- switch(component, rank = -1L, q1 = 1L, q2 = 2L)
    .valid_stamp(x, role, panel, q, -1L) &&
      identical(x$computational_gate_pass, TRUE) &&
      identical(x$common_fold_gate_pass, TRUE) &&
      identical(x$formal_inference_available, FALSE) &&
      identical(x$rank_selected, FALSE) &&
      is.character(x$fit_artifact_paths) &&
      all(file.exists(x$fit_artifact_paths)) &&
      .sc_identical_md5_vectors(
        x$fit_artifact_md5, .sc_md5_paths(x$fit_artifact_paths)) &&
      is.character(x$table_paths) && all(file.exists(x$table_paths)) &&
      .sc_identical_md5_vectors(
        x$table_md5, .sc_md5_paths(x$table_paths))
  }, results, names(results), SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if (!all(valid_result)) {
    stop("At least one stage result is stale or failed its final gate.",
         call. = FALSE)
  }
  input_md5_after <- .sc_md5_paths(generation_paths)
  pointer_after <- readRDS(pointer_path)
  parent_manifest_after <- readRDS(parent_manifest_path)
  authorization_after <- readRDS(sw_v21_rank_config$authorization_file)
  provenance_gate <- .sc_identical_md5_vectors(
      generation_md5, input_md5_after) &&
    .sw_v21_rank_final_bundle_valid(
      pointer_after, pointer_path, parent_manifest_after,
      parent_manifest_path, sw_v21_config, runtime_signature) &&
    .sw_v21_rank_authorization_valid(
      authorization_after, sw_v21_rank_config, rank_config_path,
      generation_md5, runtime_signature, pointer_after, pointer_path,
      parent_manifest_path) &&
    identical(unname(tools::md5sum(
      sw_v21_rank_config$authorization_file)), authorization_md5)
  if (!provenance_gate) {
    stop("A generation input, parent artifact, or authorization changed during execution. Outputs are invalid.",
         call. = FALSE)
  }
  stability <- vapply(results, function(x) {
    if (is.null(x$empirical_numerical_stability_gate_pass)) TRUE else
      isTRUE(x$empirical_numerical_stability_gate_pass)
  }, logical(1L))
  final <- list(
    schema_version = "sw2022-v2.1-rank-numerical-final-v1",
    configuration = sw_v21_rank_config,
    parent_configuration_version = sw_v21_config$version,
    parent_reported_primary = pointer$reported_primary,
    parent_fallback_applied = isTRUE(pointer$fallback_applied),
    panels = active_panels, stage_result_paths = required_results,
    computational_and_provenance_gate_pass = TRUE,
    empirical_numerical_stability_gate_pass = all(stability),
    empirical_gate_is_asymptotic_certificate = FALSE,
    rank_selected = FALSE, primary_rank_changed = FALSE,
    formal_inference_available = FALSE, outcome_blind = FALSE,
    generation_input_md5 = generation_md5,
    completion_input_md5 = input_md5_after,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5,
    completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    session_info = utils::capture.output(sessionInfo()))
  final_path <- file.path(output_dir, "rank_numerical_final_result.rds")
  .atomic_save(final, final_path, portable = FALSE)
  report <- c(
    "# Saha--Weeks v2.1 rank and numerical diagnostics",
    "",
    paste0("- Computational/provenance gate: `",
           final$computational_and_provenance_gate_pass, "`."),
    paste0("- Empirical numerical-stability gate: `",
           final$empirical_numerical_stability_gate_pass, "`."),
    paste0("- Parent reported primary: `", pointer$reported_primary, "`."),
    paste0("- Exact-constant fallback panel run: `",
           "exact_constant" %in% active_panels, "`."),
    "- Fixed-rank panels: `q=0,1,2`; rank was not selected.",
    "- q=1 GH ladder: `15,31,45`; q=2 ladder: `9,15,21`.",
    "- q=2 includes finite-product-GH loading-rotation checks.",
    "- Every positive-rank resolution reruns the frozen within-rank grid and controls.",
    "- The analysis is outcome-informed and provides no formal inference.",
    "- Passing empirical gates does not verify paperps asymptotic numerical-error rates.")
  .atomic_write_lines(report, file.path(output_dir, "AUDIT.md"))
  .atomic_write_lines(utils::capture.output(sessionInfo()),
                      file.path(output_dir, "sessionInfo.txt"))
  artifact_paths <- list.files(output_dir, full.names = TRUE,
                               recursive = FALSE)
  artifact_paths <- artifact_paths[file.info(artifact_paths)$isdir %in% FALSE]
  manifest_path <- file.path(output_dir, "manifest.rds")
  artifact_paths <- setdiff(
    artifact_paths,
    c(sw_v21_rank_config$authorization_file, manifest_path))
  artifact_names <- basename(artifact_paths)
  if (!length(artifact_paths) || anyDuplicated(artifact_names)) {
    stop("Final rank/numerical artifact names are empty or ambiguous.",
         call. = FALSE)
  }
  manifest <- list(
    schema_version = "sw2022-v2.1-rank-numerical-manifest-v1",
    configuration_version = sw_v21_rank_config$version,
    parent_configuration_version = sw_v21_config$version,
    completed = TRUE,
    computational_and_provenance_gate_pass = TRUE,
    empirical_numerical_stability_gate_pass = all(stability),
    parent_reported_primary = pointer$reported_primary,
    parent_fallback_applied = isTRUE(pointer$fallback_applied),
    panels = active_panels, ranks = c(0L, 1L, 2L),
    rank_selected = FALSE, formal_inference_available = FALSE,
    outcome_blind = FALSE,
    generation_input_paths = generation_paths,
    generation_input_md5 = generation_md5,
    completion_input_md5 = input_md5_after,
    runtime_signature = runtime_signature,
    authorization_md5 = authorization_md5,
    parent_manifest_md5 = unname(tools::md5sum(parent_manifest_path)),
    parent_pointer_md5 = unname(tools::md5sum(pointer_path)),
    artifacts = stats::setNames(
      unname(tools::md5sum(artifact_paths)), artifact_names),
    completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
  .atomic_save(manifest, manifest_path, portable = FALSE)
  cat("v2.1 rank/numerical diagnostics complete; empirical_stability=",
      all(stability), "; output=", output_dir, "\n", sep = "")
} else {
  missing <- required_results[!file.exists(required_results)]
  message(
    "Requested stage complete. Final manifest withheld until all required ",
    "stage results exist: ", paste(basename(missing), collapse = ", "))
}
