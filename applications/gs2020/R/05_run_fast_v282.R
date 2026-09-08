#!/usr/bin/env Rscript

## Restartable Graham--Svolik v2.8.2 prospective q=0,1,2 fit runner.
##
## Examples from the package root, after committing the protocol and code:
##   applications/bin/Rscript45 applications/gs2020/R/05_run_fast_v282.R status
##   applications/bin/Rscript45 applications/gs2020/R/05_run_fast_v282.R preflight
##   applications/bin/Rscript45 applications/gs2020/R/05_run_fast_v282.R fit --q=1 --scope=outer_2
##   applications/bin/Rscript45 applications/gs2020/R/05_run_fast_v282.R assemble --q=1
##   applications/bin/Rscript45 applications/gs2020/R/05_run_fast_v282.R refine --q=1
##
## Set CONJOINTSTRUCTURAL_ROOT to the shared project root.  There is no output
## directory override.  Source artifacts remain read-only.

options(stringsAsFactors = FALSE, warn = 1)

.gs_v282_script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/",
                mustWork = TRUE)
}

.gs_v282_repo_root_from_script <- function() {
  normalizePath(file.path(dirname(.gs_v282_script_path()), "..", "..", ".."),
                winslash = "/", mustWork = TRUE)
}

.gs_v282_bootstrap_sha256 <- function(path) {
  executable <- Sys.which("shasum")
  if (!nzchar(executable)) stop("The `shasum` executable is required.",
                                call. = FALSE)
  line <- system2(executable, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    stop("Could not hash bootstrap dependency: ", path, call. = FALSE)
  }
  sub("[[:space:]].*$", "", line)
}

.gs_v282_bootstrap_guard <- function(repo_root, versions = "v282") {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is required before outcome execution.",
                         call. = FALSE)
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--untracked-files=all"), stdout = TRUE,
                    stderr = TRUE)
  if (length(status)) {
    stop("Outcome execution requires a clean repository: ",
         paste(status, collapse = "; "), call. = FALSE)
  }
  tracked <- system2(git, c("-C", shQuote(repo_root), "ls-files"),
                     stdout = TRUE, stderr = TRUE)
  all_r <- file.path("R", list.files(file.path(repo_root, "R"),
                                      pattern = "[.]R$", recursive = TRUE))
  for (version in versions) {
    protocol_dir <- file.path(repo_root, "applications", "gs2020",
                              "protocol", version)
    checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
    lines <- readLines(checksum_path, warn = FALSE)
    pieces <- strsplit(lines, "  ", fixed = TRUE)
    if (!length(lines) || any(lengths(pieces) != 2L)) {
      stop("Malformed bootstrap checksum registry for ", version, ".",
           call. = FALSE)
    }
    expected_hash <- vapply(pieces, `[[`, character(1L), 1L)
    registered <- vapply(pieces, `[[`, character(1L), 2L)
    actual_protocol <- list.files(protocol_dir, recursive = TRUE,
                                  all.files = TRUE, no.. = TRUE)
    actual_protocol <- setdiff(actual_protocol, "CHECKSUMS.sha256")
    unsafe <- grepl("^/|(^|/)\\.\\.(/|$)", registered)
    if (anyDuplicated(registered) || any(unsafe) ||
        !setequal(registered, actual_protocol)) {
      stop("Bootstrap protocol coverage is incomplete for ", version, ".",
           call. = FALSE)
    }
    observed_hash <- vapply(registered, function(name) {
      .gs_v282_bootstrap_sha256(file.path(protocol_dir, name))
    }, character(1L))
    if (!identical(unname(observed_hash), unname(expected_hash))) {
      stop("Bootstrap protocol hash mismatch for ", version, ".",
           call. = FALSE)
    }
    implementation <- utils::read.csv(
      file.path(protocol_dir, "implementation-manifest.csv"),
      stringsAsFactors = FALSE, check.names = FALSE,
      na.strings = character())
    paths <- implementation$relative_path
    if (anyDuplicated(paths) || any(grepl("^/|(^|/)\\.\\.(/|$)", paths)) ||
        any(implementation$sha256 == "PENDING_FINALIZATION") ||
        !setequal(paths[startsWith(paths, "R/")], all_r) ||
        !"applications/bin/Rscript45" %in% paths ||
        !all(paths %in% tracked) ||
        !all(file.path("applications", "gs2020", "protocol", version,
                       c("CHECKSUMS.sha256", actual_protocol)) %in% tracked)) {
      stop("Bootstrap implementation coverage is incomplete for ", version,
           ".", call. = FALSE)
    }
    implementation_hash <- vapply(paths, function(path) {
      .gs_v282_bootstrap_sha256(file.path(repo_root, path))
    }, character(1L))
    if (!identical(unname(implementation_hash),
                   unname(implementation$sha256))) {
      stop("Bootstrap implementation hash mismatch for ", version, ".",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.gs_v282_load_code <- function(repo_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The `pkgload` package is required to run source-bound v2.8.2 code.",
         call. = FALSE)
  }
  pkgload::load_all(repo_root, quiet = TRUE, export_all = FALSE)
  sys.source(file.path(repo_root, "applications", "gs2020", "R",
                       "gs_v282_contract.R"), envir = .GlobalEnv)
  .gs_v282_set_rng_contract()
  invisible(TRUE)
}

.gs_v282_parse_cli <- function(args) {
  if (!length(args)) stop("Supply status, preflight, fit, assemble, refine, q2-decision, or analysis.",
                          call. = FALSE)
  out <- list(command = args[[1L]], q = NA_integer_, scope = NA_character_,
              verbose = FALSE)
  for (arg in args[-1L]) {
    if (!grepl("^--[^=]+=", arg)) stop("Malformed argument: ", arg,
                                       call. = FALSE)
    bits <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- bits[[1L]]
    value <- paste(bits[-1L], collapse = "=")
    if (!key %in% c("q", "scope", "verbose")) {
      stop("Unknown argument --", key, call. = FALSE)
    }
    out[[key]] <- value
  }
  if (!is.na(out$q)) out$q <- as.integer(out$q)
  out$verbose <- tolower(as.character(out$verbose)) %in% c("1", "true", "yes")
  out
}

.gs_v282_one_thread <- function() {
  .gs_v282_set_thread_contract()
}

.gs_v282_binding <- function(protocol, kind, q = NA_integer_,
                            scope = NA_character_) {
  list(protocol_id = protocol$protocol_id,
       protocol_hash = protocol$protocol_hash,
       git_commit = protocol$git$commit,
       application = "gs2020", kind = kind,
       q = as.integer(q), scope = as.character(scope),
       runtime = .gs_v282_runtime_binding(),
       rng_kind = .gs_v282_assert_rng_contract(),
       input_hashes = stats::setNames(protocol$inputs$sha256,
                                      protocol$inputs$input_id))
}

.gs_v282_preflight <- function(repo_root, source_root, protocol) {
  run_dir <- .gs_v282_run_dir(repo_root)
  path <- .gs_v282_checkpoint_path(run_dir, "preflight")
  binding <- .gs_v282_binding(protocol, "preflight")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing v2.8.2 preflight checkpoint.")
    return(invisible(path))
  }
  .gs_v282_assert_no_prior_gs_v282(repo_root)
  start <- Sys.time()
  payload <- .gs_v282_prepare_input(protocol, source_root)
  payload$run_started_utc <- format(start, tz = "UTC", usetz = TRUE)
  payload$prospective_numerical_amendment <- list(
    prior_gs_v282_outcome_exists = FALSE,
    prior_gs_v281_partial_checkpoint_scope =
      "preflight plus q1 outer_1 and outer_2 only",
    prior_gs_v281_printed_substantive_results_inspected = FALSE,
    prior_gs_v281_artifact_reused = FALSE,
    historical_estimate_reused = FALSE,
    historical_gate_or_target_reused = FALSE,
    amendment_scope =
      "caller-supplied warm-tensor ownership isolation",
    inherited_numerical_amendment =
      "v2.8.1 one-dimensional gradient-parameter shape preservation",
    application_adapter = paste(
      "matching historical outer-fold q=1 states initialize their own folds;",
      "outer-fold position one deterministically initializes the full fit",
      "because no historical full-sample state exists; each candidate start",
      "receives a validated application-owned clone before optimization"),
    package_ownership_boundary = paste(
      "the package state loader also detaches and clones each supplied warm",
      "tensor; the application clone is an independent defense-in-depth layer"),
    polish_cap = .gs_v282_polish_max_iter,
    stage_max_iterations = .gs_v282_polish_schedule$max_iter,
    stage_learning_rates = .gs_v282_polish_schedule$learning_rate,
    optimizer_tolerance_grad = 0, optimizer_tolerance_change = 0,
    statistical_gates_changed = FALSE,
    inspected_information = paste(
      "the earlier Graham--Svolik q=1 diagnostics and targets had already",
      "been seen; the Ballard--Rosa v2.8.1 implementation and its negative",
      "gate results were also known; v2.8.1 wrote only preflight and q1",
      "outer_1 and outer_2 before q1 full and outer_3 exposed the Torch",
      "shared-storage application-adapter bug"),
    excluded_information = paste(
      "no printed substantive result from the partial Graham--Svolik v2.8.1",
      "checkpoints was inspected or reused; no Graham--Svolik v2.8.2 fit,",
      "prediction, target, interval, or gate result existed when this",
      "protocol was frozen"))
  payload$integrity <- list(
    source_files_unchanged = TRUE,
    prepared_sha256 = protocol$inputs$sha256[
      protocol$inputs$input_id == "prepared_primary"],
    displayed_side_reoriented_tasks =
      payload$prepared$sample$flipped_tasks,
    task_count_range = as.integer(range(table(
      payload$prepared$respondent_id))),
    n_respondents = length(unique(payload$prepared$respondent_id)),
    n_tasks = length(payload$prepared$y), elapsed_seconds = 0)
  payload$integrity$elapsed_seconds <- as.numeric(difftime(
    Sys.time(), start, units = "secs"))
  .gs_v282_save_atomic(payload, path, binding)
  message("Created v2.8.2 preflight checkpoint without modifying source files.")
  invisible(path)
}

.gs_v282_load_preflight <- function(repo_root, protocol) {
  path <- .gs_v282_checkpoint_path(.gs_v282_run_dir(repo_root), "preflight")
  payload <- .gs_v282_read_checkpoint(
    path, binding = .gs_v282_binding(protocol, "preflight"))$payload
  payload
}

.gs_v282_fit_scope <- function(repo_root, source_root, protocol, q, scope,
                              verbose = FALSE) {
  q <- as.integer(q)
  if (!q %in% .gs_v282_ranks || !scope %in% .gs_v282_scopes) {
    stop("fit requires --q=0,1,2 and a registered --scope.", call. = FALSE)
  }
  run_dir <- .gs_v282_run_dir(repo_root)
  path <- .gs_v282_checkpoint_path(run_dir, "fit", q, scope)
  binding <- .gs_v282_binding(protocol, "fit", q, scope)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=", q, " ", scope, " checkpoint.")
    return(invisible(path))
  }
  if (q == 2L && scope != "outer_2") {
    decision_path <- .gs_v282_checkpoint_path(run_dir, "timing-decision")
    decision <- .gs_v282_read_checkpoint(
      decision_path, binding = .gs_v282_binding(protocol, "timing-decision"))$payload
    if (!identical(decision$decision, "continue")) {
      stop("The frozen q=2 timing decision does not permit this scope.",
           call. = FALSE)
    }
  }
  preflight <- .gs_v282_load_preflight(repo_root, protocol)
  start <- Sys.time()

  if (q == 1L) {
    legacy <- .gs_v282_legacy_scope(protocol, source_root, scope)
    source_rows <- .gs_v282_scope_rows(preflight$prepared, legacy)
    fitting_rows <- if (identical(scope, "full")) {
      seq_len(nrow(preflight$prepared$deltaX))
    } else source_rows
    initialized <- .gs_v282_q1_initializer(
      legacy, preflight, fitting_rows, source_rows)
    dat <- list(
      deltaX = preflight$prepared$deltaX[fitting_rows, , drop = FALSE],
      y = preflight$prepared$y[fitting_rows],
      respondent_id = preflight$prepared$respondent_id[fitting_rows])
    repaired <- initialized$repaired
    warm <- initialized$warm
    state_source <- legacy$network_state
    source <- list(
      architecture = initialized$target_architecture,
      preprocessing = list(Z = initialized$repaired$transform,
                           deltaX = initialized$dx_transform),
      prediction_difference = initialized$valid_prediction_max_difference,
      changed_rows = initialized$intentionally_changed_rows,
      provenance = list(
        mode = if (identical(scope, "full")) paste(
          "no historical full fit exists; the hash-bound outer-position-one",
          "state is remapped to full-sample preprocessing and used only as",
          "a deterministic initializer before complete reoptimization") else
          paste(
            "the matching hash-bound outer-training v2.1 state initializes",
            "only its own outer scope before complete reoptimization"),
        source_position = attr(legacy, "v282_source_position"),
        source_label = attr(legacy, "v282_source_label"),
        initializer_role = attr(legacy, "v282_initializer_role"),
        heldout_outcomes_used_for_outer_initializer = FALSE,
        carried_model_choices = c(
          "mean_specification", "network_architecture", "weight_decay"),
        all_trainable_parameters_freshly_reoptimized = TRUE,
        source_state_hash = sconjoint:::.scmix_stable_state_hash(
          state_source$state_dict), affine_remap = initialized$affine,
        float32_to_float64_prediction_max_difference =
          initialized$float32_to_float64_prediction_max_difference))
    spec <- legacy$specification
  } else {
    q1_path <- .gs_v282_checkpoint_path(run_dir, "fit", 1L, scope)
    q1 <- .gs_v282_read_checkpoint(
      q1_path, binding = .gs_v282_binding(protocol, "fit", 1L, scope))$payload$fit
    initialized <- .gs_v282_rank_initializer(q1, q)
    dat <- list(deltaX = q1$raw_data$deltaX, y = q1$raw_data$y,
                respondent_id = q1$raw_data$respondent_id)
    repaired <- list(
      Z = q1$raw_data$Z, transform = q1$preprocessing$Z,
      internal = sconjoint:::.sc_apply_z_transform(q1$raw_data$Z,
                                                    q1$preprocessing$Z),
      imputation = "none")
    warm <- initialized$warm
    state_source <- q1$network_state
    source <- list(
      architecture = state_source$architecture,
      preprocessing = q1$preprocessing,
      prediction_difference = 0, changed_rows = 0L,
      provenance = list(
        mode = paste0("new q=", q,
                      " likelihood optimization from corresponding completed v2.8.2 q=1 state"),
        source_q1_state_hash = initialized$source_q1_state_hash,
        second_loading_raw = initialized$second_loading_raw))
    spec <- q1$specification
  }
  scale <- .gs_v282_canonical_coefficient_scale(
    source$preprocessing$deltaX$scale,
    source$architecture$coefficient_scale,
    colnames(dat$deltaX))
  grid <- .gs_v282_grid(q, refined = FALSE)
  exact <- .gs_v282_fit_one_start(
    warm, dat, repaired$internal, scale, spec, q,
    .gs_v282_seed(q, scope, "exact"), source$architecture, grid, verbose)
  perturbed_warm <- .gs_v282_perturb_warm(
    warm, .gs_v282_seed(q, scope, "perturbed"))
  perturbed <- .gs_v282_fit_one_start(
    perturbed_warm, dat, repaired$internal, scale, spec, q,
    .gs_v282_seed(q, scope, "perturbed"), source$architecture, grid, verbose)
  constant <- .gs_v282_fit_one_start(
    .gs_v282_constant_warm(warm), dat, repaired$internal, scale, spec, q,
    .gs_v282_seed(q, scope, "constant"), source$architecture, grid, verbose)
  starts <- list(exact = exact, perturbed = perturbed)
  fit <- .gs_v282_make_fit(
    starts, constant, dat, repaired, spec, q, grid, scale, source, scope,
    protocol$protocol_hash, protocol$git$commit)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  payload <- list(
    schema_version = "gs2020-v2.8.2-fit-checkpoint-v1", q = q, scope = scope,
    fit = fit, gate_pass = isTRUE(fit$v28_application_gate$pass),
    timing = list(elapsed_seconds = elapsed,
                  started_utc = format(start, tz = "UTC", usetz = TRUE),
                  finished_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
                  workers = 1L, threads = 1L),
    integrity = list(
      source_artifacts_read_only = TRUE, prior_estimate_reused = FALSE,
      prior_target_reused = FALSE, prior_gate_reused = FALSE,
      prior_parameter_state_used_as_initializer = q == 1L,
      mean_specification_carried_forward = TRUE,
      network_architecture_carried_forward = TRUE,
      weight_decay_carried_forward = TRUE,
      all_trainable_parameters_freshly_reoptimized = TRUE,
      optimizer_stage_ledgers_present = all(vapply(
        fit$v28_optimizer_stage_ledgers, is.data.frame, logical(1L))),
      every_candidate_state_retained =
        length(fit$multistart_artifacts) == length(starts),
      selected_gradient_contract_pass =
        isTRUE(fit$v28_gradient_contract$overall_pass),
      fit_state_hash = sconjoint:::.scmix_stable_state_hash(
        fit$network_state$state_dict),
      source_state_hash = fit$v28_source$source_state_hash %||%
        fit$v28_source$source_q1_state_hash))
  .gs_v282_save_atomic(payload, path, binding)
  message("Completed immutable q=", q, " ", scope,
          " checkpoint in ", sprintf("%.1f", elapsed), " seconds. ",
          "Substantive values were not printed.")
  invisible(path)
}

.gs_v282_logsumexp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

.gs_v282_heldout_evaluation <- function(preflight, outer_fits,
                                       outer_fold_index, grid_override = NULL) {
  dat <- preflight$prepared
  n <- nrow(dat$deltaX)
  marginal <- rep(NA_real_, n)
  sequence <- vector("list", length(outer_fits))
  for (k in seq_along(outer_fits)) {
    fit <- outer_fits[[k]]
    heldout <- outer_fold_index == k
    Z <- as.matrix(dat$Z_primary[heldout, , drop = FALSE])
    mu <- sconjoint::scmix_predict_network(fit$network_state, Z,
                                            input = "raw", output = "raw")
    A <- sconjoint::scmix_loadings(fit, scale = "raw", scope = "full")
    grid <- if (is.null(grid_override)) fit$integration_grid else grid_override
    base <- fit$kappa + rowSums(dat$deltaX[heldout, , drop = FALSE] * mu)
    index <- if (fit$specification$q == 0L) {
      matrix(base, ncol = 1L)
    } else {
      sweep(dat$deltaX[heldout, , drop = FALSE] %*% A %*% t(grid$U),
            1L, base, `+`)
    }
    probability <- stats::plogis(index)
    marginal[heldout] <- as.numeric(probability %*% grid$w)
    y <- dat$y[heldout]
    ## Stable Bernoulli log probabilities.  Unlike 0 * log(0), these remain
    ## finite even if an extreme index rounds its probability to zero or one.
    log_probability <- -pmax(-index, 0) - log1p(exp(-abs(index)))
    log_one_minus_probability <- -pmax(index, 0) - log1p(exp(-abs(index)))
    log_task <- sweep(log_probability, 1L, y, `*`) +
      sweep(log_one_minus_probability, 1L, 1 - y, `*`)
    rid <- as.character(dat$respondent_id[heldout])
    by_id <- split(seq_along(rid), rid)
    sequence[[k]] <- data.frame(
      respondent_id = names(by_id), fold = k,
      log_score = vapply(by_id, function(rows) {
        .gs_v282_logsumexp(colSums(log_task[rows, , drop = FALSE]) +
                            log(grid$w))
      }, numeric(1L)), stringsAsFactors = FALSE)
  }
  if (any(!is.finite(marginal))) stop("Incomplete held-out predictions.",
                                      call. = FALSE)
  list(
    task = data.frame(respondent_id = dat$respondent_id, task = dat$task,
                      fold = outer_fold_index, y = dat$y,
                      probability = marginal, stringsAsFactors = FALSE),
    sequence = do.call(rbind, sequence),
    mean_sequence_log_score = mean(unlist(lapply(sequence, `[[`, "log_score"))),
    marginal_calibration = mean(dat$y) - mean(marginal))
}

.gs_v282_assemble <- function(repo_root, source_root, protocol, q) {
  q <- as.integer(q)
  if (!q %in% .gs_v282_ranks) stop("assemble requires --q=0,1,2.", call. = FALSE)
  run_dir <- .gs_v282_run_dir(repo_root)
  path <- .gs_v282_checkpoint_path(run_dir, "assembled", q)
  binding <- .gs_v282_binding(protocol, "assembled", q, "all")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=", q, " assembly.")
    return(invisible(path))
  }
  checkpoints <- lapply(.gs_v282_scopes, function(scope) {
    .gs_v282_read_checkpoint(
      .gs_v282_checkpoint_path(run_dir, "fit", q, scope),
      binding = .gs_v282_binding(protocol, "fit", q, scope))$payload
  })
  names(checkpoints) <- .gs_v282_scopes
  fits <- lapply(checkpoints, `[[`, "fit")
  preflight <- .gs_v282_load_preflight(repo_root, protocol)
  legacy_nested <- readRDS(.gs_v282_input_path(protocol, source_root,
                                               "v21_primary_nested"))
  fold_index <- legacy_nested$outer_fold_index
  if (is.null(fold_index)) {
    fold_index <- match(as.character(legacy_nested$outer_fold_id),
                        legacy_nested$outer_folds)
  }
  Z_full <- as.matrix(preflight$prepared$Z_primary)
  nested <- legacy_nested
  nested$raw_data <- list(deltaX = preflight$prepared$deltaX,
                          y = preflight$prepared$y, Z = Z_full,
                          respondent_id = preflight$prepared$respondent_id)
  nested$analysis_signature <- paste0(
    "gs2020-v282-q", q, "-prospective-assembly")
  for (k in 1:5) {
    nested$tuning[[k]]$refit <- fits[[paste0("outer_", k)]]
    nested$tuning[[k]]$selected_index <- 1L
    nested$tuning[[k]]$candidate_selection_gate <- list(
      pass = FALSE, selection_eligible = FALSE,
      reason = paste(
        "legacy assembly adapter does not constitute v2.8.2 candidate",
        "selection evidence; application gates live in v2.8.2 fit artifacts"))
    nested$tuning[[k]]$primary_inference_eligible <- FALSE
  }
  assembled <- sconjoint::scmix_assemble_nested(
    nested, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  heldout <- .gs_v282_heldout_evaluation(
    preflight, fits[-1L], as.integer(fold_index))
  assembled$v28_application_gate_pass <- all(vapply(
    fits[-1L], function(x) isTRUE(x$v28_application_gate$pass), logical(1L)))
  assembled$v28_formal_dml_scope <- if (q == 1L) {
    paste("draft-conditional rank-one target-specific DML only; package",
          "production eligibility remains false pending confirmatory validation")
  } else "not permitted in the prospective protocol"
  payload <- list(
    schema_version = "gs2020-v2.8.2-assembled-v1", q = q,
    full_fit = fits$full, assembled = sconjoint::scmix_portable_copy(assembled),
    multistart_artifacts_by_scope = lapply(
      fits, `[[`, "multistart_artifacts"),
    heldout = heldout,
    fit_gate_by_scope = vapply(fits, function(x) {
      isTRUE(x$v28_application_gate$pass)
    }, logical(1L)),
    inference_role = if (q == 1L) {
      "post_diagnostic_same_sample_DML_descriptive_and_conditional"
    } else
      "predictive_calibration_and_plugin_sensitivity_only",
    no_historical_estimate_reuse = TRUE)
  .gs_v282_save_atomic(payload, path, binding)
  message("Created q=", q,
          " assembly and held-out evaluation. Substantive values were not printed.")
  invisible(path)
}

.gs_v282_refine <- function(repo_root, protocol, q) {
  q <- as.integer(q)
  if (!q %in% c(1L, 2L)) stop("refine requires --q=1 or --q=2.", call. = FALSE)
  run_dir <- .gs_v282_run_dir(repo_root)
  path <- .gs_v282_checkpoint_path(run_dir, "refinement", q)
  binding <- .gs_v282_binding(protocol, "refinement", q, "all")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=", q, " refinement.")
    return(invisible(path))
  }
  assembled <- .gs_v282_read_checkpoint(
    .gs_v282_checkpoint_path(run_dir, "assembled", q),
    binding = .gs_v282_binding(protocol, "assembled", q, "all"))$payload
  preflight <- .gs_v282_load_preflight(repo_root, protocol)
  grid <- .gs_v282_grid(q, refined = TRUE)
  outer_fits <- lapply(1:5, function(k) {
    .gs_v282_read_checkpoint(
      .gs_v282_checkpoint_path(run_dir, "fit", q, paste0("outer_", k)),
      binding = .gs_v282_binding(protocol, "fit", q, paste0("outer_", k)))$payload$fit
  })
  fixed <- .gs_v282_heldout_evaluation(
    preflight, outer_fits, assembled$assembled$fold_id, grid_override = grid)
  payload <- list(
    schema_version = "gs2020-v2.8.2-fixed-state-refinement-v1", q = q,
    nodes_per_dimension = if (q == 1L) 45L else 15L,
    parameters_unchanged = TRUE,
    primary_fit_checkpoint_sha256 = .gs_v282_sha256(
      .gs_v282_checkpoint_path(run_dir, "assembled", q)),
    heldout = fixed,
    comparison = list(
      sequence_log_score_change = fixed$mean_sequence_log_score -
        assembled$heldout$mean_sequence_log_score,
      max_task_probability_change = max(abs(
        fixed$task$probability - assembled$heldout$task$probability))))
  .gs_v282_save_atomic(payload, path, binding)
  message("Created fixed-state q=", q,
          " integration refinement. Substantive values were not printed.")
  invisible(path)
}

.gs_v282_q2_decision <- function(repo_root, protocol) {
  run_dir <- .gs_v282_run_dir(repo_root)
  path <- .gs_v282_checkpoint_path(run_dir, "timing-decision")
  binding <- .gs_v282_binding(protocol, "timing-decision")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=2 timing decision.")
    return(invisible(path))
  }
  sentinel_path <- .gs_v282_checkpoint_path(run_dir, "fit", 2L, "outer_2")
  sentinel <- .gs_v282_read_checkpoint(
    sentinel_path, binding = .gs_v282_binding(protocol, "fit", 2L, "outer_2"))$payload
  ## Verify the declared q=0/q=1 prerequisites and base the timing rule only
  ## on recorded stage runtimes. Operator pauses and command order therefore
  ## cannot alter the decision. An outer fold has 4/5 of the full sample, so
  ## five outer fits plus one full fit equal 6.25 sentinel-fold runtimes.
  .gs_v282_read_checkpoint(
    .gs_v282_checkpoint_path(run_dir, "assembled", 0L),
    binding = .gs_v282_binding(protocol, "assembled", 0L, "all"),
    payload = FALSE)
  .gs_v282_read_checkpoint(
    .gs_v282_checkpoint_path(run_dir, "refinement", 1L),
    binding = .gs_v282_binding(protocol, "refinement", 1L, "all"),
    payload = FALSE)
  prior_fit_seconds <- sum(vapply(c(0L, 1L), function(q) {
    sum(vapply(.gs_v282_scopes, function(scope) {
      fit <- .gs_v282_read_checkpoint(
        .gs_v282_checkpoint_path(run_dir, "fit", q, scope),
        binding = .gs_v282_binding(protocol, "fit", q, scope))$payload
      as.numeric(fit$timing$elapsed_seconds)
    }, numeric(1L)))
  }, numeric(1L)))
  sentinel_seconds <- as.numeric(sentinel$timing$elapsed_seconds)
  projected <- prior_fit_seconds + 6.25 * sentinel_seconds
  decision <- if (isTRUE(sentinel$integrity$source_artifacts_read_only) &&
      is.finite(projected) && projected <= 14 * 3600) "continue" else "stop"
  payload <- list(
    schema_version = "gs2020-v2.8.2-q2-timing-decision-v1",
    decision = decision, inspected_fields = c(
      "recorded_fit_elapsed_seconds", "source_artifacts_read_only",
      "checkpoint_sha256"),
    statistical_fields_inspected = FALSE,
    completed_q0_q1_fit_seconds = prior_fit_seconds,
    sentinel_seconds = sentinel_seconds,
    q2_projected_total_multiplier = 6.25,
    projected_completion_seconds = projected,
    cutoff_seconds = 14 * 3600,
    sentinel_sha256 = .gs_v282_sha256(sentinel_path))
  .gs_v282_save_atomic(payload, path, binding)
  message("Recorded frozen q=2 timing decision: ", decision,
          ". No substantive field was printed.")
  invisible(path)
}

.gs_v282_calibration_summary <- function(task, bins = 10L) {
  stopifnot(is.data.frame(task), all(c("respondent_id", "task", "fold", "y",
                                      "probability") %in% names(task)))
  n <- nrow(task)
  rank <- rank(task$probability, ties.method = "first")
  bin <- pmin(as.integer(bins), ceiling(rank * as.integer(bins) / n))
  by_bin <- do.call(rbind, lapply(split(seq_len(n), bin), function(rows) {
    data.frame(bin = bin[rows[[1L]]], n = length(rows),
               observed = mean(task$y[rows]),
               predicted = mean(task$probability[rows]))
  }))
  by_task <- do.call(rbind, lapply(split(seq_len(n), task$task), function(rows) {
    data.frame(task = task$task[rows[[1L]]], n = length(rows),
               observed = mean(task$y[rows]),
               predicted = mean(task$probability[rows]))
  }))
  list(
    by_bin = by_bin, by_task = by_task,
    max_bin_gap = max(abs(by_bin$observed - by_bin$predicted)),
    max_task_gap = max(abs(by_task$observed - by_task$predicted)))
}

.gs_v282_analysis <- function(repo_root, protocol) {
  run_dir <- .gs_v282_run_dir(repo_root)
  path <- .gs_v282_checkpoint_path(run_dir, "analysis")
  binding <- .gs_v282_binding(protocol, "analysis", 1L, "all")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .gs_v282_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable v2.8.2 rank-comparison analysis.")
    return(invisible(path))
  }
  timing <- .gs_v282_read_checkpoint(
    .gs_v282_checkpoint_path(run_dir, "timing-decision"),
    binding = .gs_v282_binding(protocol, "timing-decision"))$payload
  if (!timing$decision %in% c("continue", "stop")) {
    stop("The frozen q=2 timing decision is malformed.", call. = FALSE)
  }
  available_ranks <- if (identical(timing$decision, "continue")) {
    .gs_v282_ranks
  } else c(0L, 1L)
  assembled <- lapply(available_ranks, function(q) {
    .gs_v282_read_checkpoint(
      .gs_v282_checkpoint_path(run_dir, "assembled", q),
      binding = .gs_v282_binding(protocol, "assembled", q, "all"))$payload
  })
  names(assembled) <- paste0("q", available_ranks)
  refinements <- list(q1 = .gs_v282_read_checkpoint(
      .gs_v282_checkpoint_path(run_dir, "refinement", 1L),
      binding = .gs_v282_binding(protocol, "refinement", 1L, "all"))$payload)
  if (2L %in% available_ranks) {
    refinements$q2 <- .gs_v282_read_checkpoint(
      .gs_v282_checkpoint_path(run_dir, "refinement", 2L),
      binding = .gs_v282_binding(protocol, "refinement", 2L, "all"))$payload
  }
  comparisons <- do.call(rbind, lapply(available_ranks, function(q) {
    x <- assembled[[paste0("q", q)]]
    refined <- refinements[[paste0("q", q)]]
    data.frame(
      q = q, inference_role = x$inference_role,
      mean_heldout_sequence_log_score = x$heldout$mean_sequence_log_score,
      marginal_calibration_gap = x$heldout$marginal_calibration,
      all_scope_fit_gates_pass = all(x$fit_gate_by_scope),
      fixed_state_refinement_nodes = if (is.null(refined)) NA_integer_ else
        refined$nodes_per_dimension,
      fixed_state_sequence_score_change = if (is.null(refined)) NA_real_ else
        refined$comparison$sequence_log_score_change,
      fixed_state_max_probability_change = if (is.null(refined)) NA_real_ else
        refined$comparison$max_task_probability_change,
      stringsAsFactors = FALSE)
  }))
  calibration <- lapply(assembled, function(x) {
    .gs_v282_calibration_summary(x$heldout$task)
  })
  sequence <- Reduce(function(x, y) merge(x, y, by = "respondent_id",
                                           all = FALSE, sort = TRUE),
    lapply(available_ranks, function(q) {
      x <- assembled[[paste0("q", q)]]$heldout$sequence
      data.frame(respondent_id = x$respondent_id,
                 value = x$log_score, stringsAsFactors = FALSE,
                 check.names = FALSE) |>
        stats::setNames(c("respondent_id", paste0("q", q)))
    }))
  paired <- do.call(rbind, lapply(setdiff(available_ranks, 1L), function(q) {
    difference <- sequence[[paste0("q", q)]] - sequence$q1
    data.frame(comparison = paste0("q", q, "_minus_q1"),
               mean = mean(difference),
               se = stats::sd(difference) / sqrt(length(difference)),
               lower_95 = mean(difference) - 1.96 * stats::sd(difference) /
                 sqrt(length(difference)),
               upper_95 = mean(difference) + 1.96 * stats::sd(difference) /
                 sqrt(length(difference)), stringsAsFactors = FALSE)
  }))
  payload <- list(
    schema_version = "gs2020-v2.8.2-prospective-analysis-v1",
    comparisons = comparisons, paired_sequence_scores = paired,
    calibration = calibration,
    q2_timing_decision = timing,
    q1_inference_status = paste(
      "draft conditional only; target-specific DML and all unchanged gates",
      "use target-specific training-only ridge validation and 9,999 common",
      "within-family multipliers in a separately hash-bound target stage;",
      "the fast held-nuisance ridge check remains diagnostic and package",
      "production eligibility requires signed inner-nuisance refits"),
    q0_share_inference = "prohibited",
    q2_regular_dml = "not_permitted_for_formal_inference",
    q2_predictive_status = if (2L %in% available_ranks) {
      "available_under_frozen_timing_rule"
    } else "deferred_by_frozen_timing_rule",
    historical_estimates_used = FALSE)
  .gs_v282_save_atomic(payload, path, binding)
  message("Created v2.8.2 rank-comparison analysis. Substantive values were not printed.")
  invisible(path)
}

.gs_v282_main <- function() {
  repo_root <- .gs_v282_repo_root_from_script()
  cli <- .gs_v282_parse_cli(commandArgs(trailingOnly = TRUE))
  outcome_command <- cli$command != "status"
  if (outcome_command) .gs_v282_bootstrap_guard(repo_root, "v282")
  .gs_v282_load_code(repo_root)
  .gs_v282_one_thread()
  source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
  if (outcome_command && !nzchar(source_root)) {
    stop("Set CONJOINTSTRUCTURAL_ROOT to the shared ConjointStructural root.",
         call. = FALSE)
  }
  protocol <- .gs_v282_validate_protocol(
    repo_root, source_root = if (nzchar(source_root)) source_root else NULL,
    require_external = outcome_command,
    require_clean = outcome_command)
  if (!cli$command %in% c("status", "preflight")) {
    .gs_v282_load_preflight(repo_root, protocol)
  }
  if (cli$command == "status") {
    print(.gs_v282_status(repo_root), row.names = FALSE)
  } else if (cli$command == "preflight") {
    .gs_v282_preflight(repo_root, source_root, protocol)
  } else if (cli$command == "fit") {
    .gs_v282_fit_scope(repo_root, source_root, protocol, cli$q, cli$scope,
                      cli$verbose)
  } else if (cli$command == "assemble") {
    .gs_v282_assemble(repo_root, source_root, protocol, cli$q)
  } else if (cli$command == "refine") {
    .gs_v282_refine(repo_root, protocol, cli$q)
  } else if (cli$command == "q2-decision") {
    .gs_v282_q2_decision(repo_root, protocol)
  } else if (cli$command == "analysis") {
    .gs_v282_analysis(repo_root, protocol)
  } else {
    stop("Unknown command: ", cli$command, call. = FALSE)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) .gs_v282_main()
