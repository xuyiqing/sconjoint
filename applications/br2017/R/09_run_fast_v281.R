#!/usr/bin/env Rscript

## Restartable Ballard--Rosa v2.8.1 prospective q=0,1,2 fit runner.
##
## Examples from the package root, after committing the protocol and code:
##   applications/bin/Rscript45 applications/br2017/R/09_run_fast_v281.R status
##   applications/bin/Rscript45 applications/br2017/R/09_run_fast_v281.R preflight
##   applications/bin/Rscript45 applications/br2017/R/09_run_fast_v281.R fit --q=1 --scope=outer_2
##   applications/bin/Rscript45 applications/br2017/R/09_run_fast_v281.R assemble --q=1
##   applications/bin/Rscript45 applications/br2017/R/09_run_fast_v281.R refine --q=1
##
## Set CONJOINTSTRUCTURAL_ROOT to the shared project root.  There is no output
## directory override.  Source artifacts remain read-only.

options(stringsAsFactors = FALSE, warn = 1)

.br_v281_script_path <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), winslash = "/",
                mustWork = TRUE)
}

.br_v281_repo_root_from_script <- function() {
  normalizePath(file.path(dirname(.br_v281_script_path()), "..", "..", ".."),
                winslash = "/", mustWork = TRUE)
}

.br_v281_bootstrap_sha256 <- function(path) {
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

.br_v281_bootstrap_guard <- function(repo_root, versions = "v281") {
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
    protocol_dir <- file.path(repo_root, "applications", "br2017",
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
      .br_v281_bootstrap_sha256(file.path(protocol_dir, name))
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
        !all(file.path("applications", "br2017", "protocol", version,
                       c("CHECKSUMS.sha256", actual_protocol)) %in% tracked)) {
      stop("Bootstrap implementation coverage is incomplete for ", version,
           ".", call. = FALSE)
    }
    implementation_hash <- vapply(paths, function(path) {
      .br_v281_bootstrap_sha256(file.path(repo_root, path))
    }, character(1L))
    if (!identical(unname(implementation_hash),
                   unname(implementation$sha256))) {
      stop("Bootstrap implementation hash mismatch for ", version, ".",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.br_v281_load_code <- function(repo_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The `pkgload` package is required to run source-bound v2.8.1 code.",
         call. = FALSE)
  }
  pkgload::load_all(repo_root, quiet = TRUE, export_all = FALSE)
  sys.source(file.path(repo_root, "applications", "br2017", "R",
                       "br_v281_contract.R"), envir = .GlobalEnv)
  .br_v281_set_rng_contract()
  invisible(TRUE)
}

.br_v281_parse_cli <- function(args) {
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

.br_v281_one_thread <- function() {
  .br_v281_set_thread_contract()
}

.br_v281_binding <- function(protocol, kind, q = NA_integer_,
                            scope = NA_character_) {
  list(protocol_id = protocol$protocol_id,
       protocol_hash = protocol$protocol_hash,
       git_commit = protocol$git$commit,
       application = "br2017", kind = kind,
       q = as.integer(q), scope = as.character(scope),
       runtime = .br_v281_runtime_binding(),
       rng_kind = .br_v281_assert_rng_contract(),
       input_hashes = stats::setNames(protocol$inputs$sha256,
                                      protocol$inputs$input_id))
}

.br_v281_preflight <- function(repo_root, source_root, protocol) {
  run_dir <- .br_v281_run_dir(repo_root)
  path <- .br_v281_checkpoint_path(run_dir, "preflight")
  binding <- .br_v281_binding(protocol, "preflight")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    existing <- .br_v281_read_checkpoint(path, binding = binding)$payload
    .br_v281_assert_v26_checkpoint_snapshot(
      repo_root, existing$v26_checkpoint_snapshot)
    .br_v281_assert_v28_checkpoint_snapshot(
      repo_root, existing$v28_stopped_checkpoint_snapshot)
    message("Verified existing v2.8.1 preflight checkpoint.")
    return(invisible(path))
  }
  start <- Sys.time()
  v26_snapshot <- .br_v281_v26_checkpoint_snapshot(repo_root)
  v28_snapshot <- .br_v281_v28_checkpoint_snapshot(repo_root)
  payload <- .br_v281_prepare_input(protocol, source_root)
  payload$run_started_utc <- format(start, tz = "UTC", usetz = TRUE)
  payload$v26_checkpoint_snapshot <- v26_snapshot
  payload$v28_stopped_checkpoint_snapshot <- v28_snapshot
  payload$prospective_numerical_amendment <- list(
    prior_v28_preflight_reused = FALSE,
    prior_v28_failed_optimization_state_reused = FALSE,
    amendment_scope =
      "one_dimensional_parameter_shape_preservation_in_gradient_audit",
    optimizer_effort_changed_from_v27 = FALSE,
    polish_cap = .br_v281_polish_max_iter,
    stage_max_iterations = .br_v281_polish_schedule$max_iter,
    stage_learning_rates = .br_v281_polish_schedule$learning_rate,
    optimizer_tolerance_grad = 0, optimizer_tolerance_change = 0,
    statistical_gates_changed = FALSE,
    inspected_information = paste(
      "the v2.7 application diagnostics and targets had already been seen;",
      "v2.8 stopped before a fit checkpoint because its gradient audit lost",
      "the shape of a one-dimensional ReLU bias parameter; v2.8.1 inherits",
      "all frozen v2.8 model, estimator, seed, optimizer, gate, and target",
      "choices and changes only that implementation shape handling"),
    excluded_information = paste(
      "no v2.8.1 outcomes, estimates, intervals, or gate results existed when",
      "this prospective protocol was frozen"))
  payload$integrity <- list(
    source_files_unchanged = TRUE,
    prepared_sha256 = protocol$inputs$sha256[
      protocol$inputs$input_id == "prepared_primary"],
    contrast_changed_tasks = payload$prepared$sample$tasks_changed_by_code5_repair,
    n_invalid_work_luck_tasks = length(payload$work_vs_luck$invalid_task_rows),
    n_invalid_work_luck_respondents = length(unique(
      payload$prepared$respondent_id[payload$work_vs_luck$invalid_task_rows])),
    n_respondents = length(unique(payload$prepared$respondent_id)),
    n_tasks = length(payload$prepared$y), elapsed_seconds = 0)
  payload$integrity$elapsed_seconds <- as.numeric(difftime(
    Sys.time(), start, units = "secs"))
  .br_v281_save_atomic(payload, path, binding)
  .br_v281_assert_v26_checkpoint_snapshot(repo_root, v26_snapshot)
  .br_v281_assert_v28_checkpoint_snapshot(repo_root, v28_snapshot)
  message("Created v2.8.1 preflight checkpoint without modifying source files.")
  invisible(path)
}

.br_v281_load_preflight <- function(repo_root, protocol) {
  path <- .br_v281_checkpoint_path(.br_v281_run_dir(repo_root), "preflight")
  payload <- .br_v281_read_checkpoint(
    path, binding = .br_v281_binding(protocol, "preflight"))$payload
  .br_v281_assert_v26_checkpoint_snapshot(
    repo_root, payload$v26_checkpoint_snapshot)
  .br_v281_assert_v28_checkpoint_snapshot(
    repo_root, payload$v28_stopped_checkpoint_snapshot)
  payload
}

.br_v281_fit_scope <- function(repo_root, source_root, protocol, q, scope,
                              verbose = FALSE) {
  q <- as.integer(q)
  if (!q %in% .br_v281_ranks || !scope %in% .br_v281_scopes) {
    stop("fit requires --q=0,1,2 and a registered --scope.", call. = FALSE)
  }
  run_dir <- .br_v281_run_dir(repo_root)
  path <- .br_v281_checkpoint_path(run_dir, "fit", q, scope)
  binding <- .br_v281_binding(protocol, "fit", q, scope)
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v281_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=", q, " ", scope, " checkpoint.")
    return(invisible(path))
  }
  if (q == 2L && scope != "outer_2") {
    decision_path <- .br_v281_checkpoint_path(run_dir, "timing-decision")
    decision <- .br_v281_read_checkpoint(
      decision_path, binding = .br_v281_binding(protocol, "timing-decision"))$payload
    if (!identical(decision$decision, "continue")) {
      stop("The frozen q=2 timing decision does not permit this scope.",
           call. = FALSE)
    }
  }
  preflight <- .br_v281_load_preflight(repo_root, protocol)
  start <- Sys.time()

  if (q == 1L) {
    legacy <- .br_v281_legacy_scope(protocol, source_root, scope)
    prepared_rows <- .br_v281_scope_rows(preflight$prepared, legacy)
    initialized <- .br_v281_q1_initializer(legacy, preflight, prepared_rows)
    dat <- list(deltaX = legacy$raw_data$deltaX,
                y = legacy$raw_data$y,
                respondent_id = legacy$raw_data$respondent_id)
    repaired <- initialized$repaired
    warm <- initialized$warm
    state_source <- legacy$network_state
    source <- list(
      architecture = state_source$architecture,
      preprocessing = legacy$preprocessing,
      prediction_difference = initialized$valid_prediction_max_difference,
      changed_rows = initialized$intentionally_changed_rows,
      provenance = list(
        mode = paste(
          "hash-bound v2.1 selected parameter state initializes a fresh",
          "optimization; mean specification, architecture, and weight decay",
          "are carried model choices rather than newly selected choices"),
        carried_model_choices = c(
          "mean_specification", "network_architecture", "weight_decay"),
        all_trainable_parameters_freshly_reoptimized = TRUE,
        source_state_hash = sconjoint:::.scmix_stable_state_hash(
          state_source$state_dict), affine_remap = initialized$affine,
        float32_to_float64_prediction_max_difference =
          initialized$float32_to_float64_prediction_max_difference))
    spec <- legacy$specification
  } else {
    q1_path <- .br_v281_checkpoint_path(run_dir, "fit", 1L, scope)
    q1 <- .br_v281_read_checkpoint(
      q1_path, binding = .br_v281_binding(protocol, "fit", 1L, scope))$payload$fit
    initialized <- .br_v281_rank_initializer(q1, q)
    dat <- list(deltaX = q1$raw_data$deltaX, y = q1$raw_data$y,
                respondent_id = q1$raw_data$respondent_id)
    repaired <- list(
      Z = q1$raw_data$Z, transform = q1$preprocessing$Z,
      internal = sconjoint:::.sc_apply_z_transform(q1$raw_data$Z,
                                                    q1$preprocessing$Z),
      imputation_median = q1$v28_work_luck$imputation_median)
    warm <- initialized$warm
    state_source <- q1$network_state
    source <- list(
      architecture = state_source$architecture,
      preprocessing = q1$preprocessing,
      prediction_difference = 0, changed_rows = 0L,
      provenance = list(
        mode = paste0("new q=", q,
                      " likelihood optimization from corresponding completed v2.8.1 q=1 state"),
        source_q1_state_hash = initialized$source_q1_state_hash,
        second_loading_raw = initialized$second_loading_raw))
    spec <- q1$specification
  }
  scale <- .br_v281_canonical_coefficient_scale(
    source$preprocessing$deltaX$scale,
    source$architecture$coefficient_scale,
    colnames(dat$deltaX))
  grid <- .br_v281_grid(q, refined = FALSE)
  exact <- .br_v281_fit_one_start(
    warm, dat, repaired$internal, scale, spec, q,
    .br_v281_seed(q, scope, "exact"), source$architecture, grid, verbose)
  perturbed_warm <- .br_v281_perturb_warm(
    warm, .br_v281_seed(q, scope, "perturbed"))
  perturbed <- .br_v281_fit_one_start(
    perturbed_warm, dat, repaired$internal, scale, spec, q,
    .br_v281_seed(q, scope, "perturbed"), source$architecture, grid, verbose)
  constant <- .br_v281_fit_one_start(
    .br_v281_constant_warm(warm), dat, repaired$internal, scale, spec, q,
    .br_v281_seed(q, scope, "constant"), source$architecture, grid, verbose)
  starts <- list(exact = exact, perturbed = perturbed)
  fit <- .br_v281_make_fit(
    starts, constant, dat, repaired, spec, q, grid, scale, source, scope,
    protocol$protocol_hash, protocol$git$commit)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  payload <- list(
    schema_version = "br2017-v2.8.1-fit-checkpoint-v1", q = q, scope = scope,
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
  .br_v281_save_atomic(payload, path, binding)
  message("Completed immutable q=", q, " ", scope,
          " checkpoint in ", sprintf("%.1f", elapsed), " seconds. ",
          "Substantive values were not printed.")
  invisible(path)
}

.br_v281_logsumexp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

.br_v281_heldout_evaluation <- function(preflight, outer_fits,
                                       outer_fold_index, grid_override = NULL) {
  dat <- preflight$prepared
  n <- nrow(dat$deltaX)
  marginal <- rep(NA_real_, n)
  sequence <- vector("list", length(outer_fits))
  for (k in seq_along(outer_fits)) {
    fit <- outer_fits[[k]]
    heldout <- outer_fold_index == k
    Z <- as.matrix(dat$Z_primary[heldout, , drop = FALSE])
    j <- match("work_vs_luck", colnames(Z))
    raw <- preflight$work_vs_luck$repaired_raw[heldout]
    raw[is.na(raw)] <- fit$v28_work_luck$imputation_median
    Z[, j] <- raw
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
        .br_v281_logsumexp(colSums(log_task[rows, , drop = FALSE]) +
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

.br_v281_assemble <- function(repo_root, source_root, protocol, q) {
  q <- as.integer(q)
  if (!q %in% .br_v281_ranks) stop("assemble requires --q=0,1,2.", call. = FALSE)
  run_dir <- .br_v281_run_dir(repo_root)
  path <- .br_v281_checkpoint_path(run_dir, "assembled", q)
  binding <- .br_v281_binding(protocol, "assembled", q, "all")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v281_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=", q, " assembly.")
    return(invisible(path))
  }
  checkpoints <- lapply(.br_v281_scopes, function(scope) {
    .br_v281_read_checkpoint(
      .br_v281_checkpoint_path(run_dir, "fit", q, scope),
      binding = .br_v281_binding(protocol, "fit", q, scope))$payload
  })
  names(checkpoints) <- .br_v281_scopes
  fits <- lapply(checkpoints, `[[`, "fit")
  preflight <- .br_v281_load_preflight(repo_root, protocol)
  legacy_nested <- readRDS(.br_v281_input_path(protocol, source_root,
                                               "v21_primary_nested"))
  fold_index <- legacy_nested$outer_fold_index
  if (is.null(fold_index)) {
    fold_index <- match(as.character(legacy_nested$outer_fold_id),
                        legacy_nested$outer_folds)
  }
  medians <- vapply(fits[-1L], function(x) {
    x$v28_work_luck$imputation_median
  }, numeric(1L))
  if (max(medians) != min(medians)) {
    stop("Fold-specific work/luck medians differ; a fold-aware Z container is required.",
         call. = FALSE)
  }
  Z_full <- as.matrix(preflight$prepared$Z_primary)
  j <- match("work_vs_luck", colnames(Z_full))
  raw <- preflight$work_vs_luck$repaired_raw
  raw[is.na(raw)] <- medians[[1L]]
  Z_full[, j] <- raw
  nested <- legacy_nested
  nested$raw_data <- list(deltaX = preflight$prepared$deltaX,
                          y = preflight$prepared$y, Z = Z_full,
                          respondent_id = preflight$prepared$respondent_id)
  nested$analysis_signature <- paste0(
    "br2017-v281-q", q, "-prospective-assembly")
  for (k in 1:5) {
    nested$tuning[[k]]$refit <- fits[[paste0("outer_", k)]]
    nested$tuning[[k]]$selected_index <- 1L
    nested$tuning[[k]]$candidate_selection_gate <- list(
      pass = FALSE, selection_eligible = FALSE,
      reason = paste(
        "legacy assembly adapter does not constitute v2.8.1 candidate",
        "selection evidence; application gates live in v2.8.1 fit artifacts"))
    nested$tuning[[k]]$primary_inference_eligible <- FALSE
  }
  assembled <- sconjoint::scmix_assemble_nested(
    nested, require_optimization_gate = FALSE, diagnostic_only = TRUE)
  heldout <- .br_v281_heldout_evaluation(
    preflight, fits[-1L], as.integer(fold_index))
  assembled$v28_application_gate_pass <- all(vapply(
    fits[-1L], function(x) isTRUE(x$v28_application_gate$pass), logical(1L)))
  assembled$v28_formal_dml_scope <- if (q == 1L) {
    paste("draft-conditional rank-one target-specific DML only; package",
          "production eligibility remains false pending confirmatory validation")
  } else "not permitted in the prospective protocol"
  payload <- list(
    schema_version = "br2017-v2.8.1-assembled-v1", q = q,
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
  .br_v281_save_atomic(payload, path, binding)
  message("Created q=", q,
          " assembly and held-out evaluation. Substantive values were not printed.")
  invisible(path)
}

.br_v281_refine <- function(repo_root, protocol, q) {
  q <- as.integer(q)
  if (!q %in% c(1L, 2L)) stop("refine requires --q=1 or --q=2.", call. = FALSE)
  run_dir <- .br_v281_run_dir(repo_root)
  path <- .br_v281_checkpoint_path(run_dir, "refinement", q)
  binding <- .br_v281_binding(protocol, "refinement", q, "all")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v281_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=", q, " refinement.")
    return(invisible(path))
  }
  assembled <- .br_v281_read_checkpoint(
    .br_v281_checkpoint_path(run_dir, "assembled", q),
    binding = .br_v281_binding(protocol, "assembled", q, "all"))$payload
  preflight <- .br_v281_load_preflight(repo_root, protocol)
  grid <- .br_v281_grid(q, refined = TRUE)
  outer_fits <- lapply(1:5, function(k) {
    .br_v281_read_checkpoint(
      .br_v281_checkpoint_path(run_dir, "fit", q, paste0("outer_", k)),
      binding = .br_v281_binding(protocol, "fit", q, paste0("outer_", k)))$payload$fit
  })
  fixed <- .br_v281_heldout_evaluation(
    preflight, outer_fits, assembled$assembled$fold_id, grid_override = grid)
  payload <- list(
    schema_version = "br2017-v2.8.1-fixed-state-refinement-v1", q = q,
    nodes_per_dimension = if (q == 1L) 45L else 15L,
    parameters_unchanged = TRUE,
    primary_fit_checkpoint_sha256 = .br_v281_sha256(
      .br_v281_checkpoint_path(run_dir, "assembled", q)),
    heldout = fixed,
    comparison = list(
      sequence_log_score_change = fixed$mean_sequence_log_score -
        assembled$heldout$mean_sequence_log_score,
      max_task_probability_change = max(abs(
        fixed$task$probability - assembled$heldout$task$probability))))
  .br_v281_save_atomic(payload, path, binding)
  message("Created fixed-state q=", q,
          " integration refinement. Substantive values were not printed.")
  invisible(path)
}

.br_v281_q2_decision <- function(repo_root, protocol) {
  run_dir <- .br_v281_run_dir(repo_root)
  path <- .br_v281_checkpoint_path(run_dir, "timing-decision")
  binding <- .br_v281_binding(protocol, "timing-decision")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v281_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable q=2 timing decision.")
    return(invisible(path))
  }
  sentinel_path <- .br_v281_checkpoint_path(run_dir, "fit", 2L, "outer_2")
  sentinel <- .br_v281_read_checkpoint(
    sentinel_path, binding = .br_v281_binding(protocol, "fit", 2L, "outer_2"))$payload
  ## Verify the declared q=0/q=1 prerequisites and base the timing rule only
  ## on recorded stage runtimes. Operator pauses and command order therefore
  ## cannot alter the decision. An outer fold has 4/5 of the full sample, so
  ## five outer fits plus one full fit equal 6.25 sentinel-fold runtimes.
  .br_v281_read_checkpoint(
    .br_v281_checkpoint_path(run_dir, "assembled", 0L),
    binding = .br_v281_binding(protocol, "assembled", 0L, "all"),
    payload = FALSE)
  .br_v281_read_checkpoint(
    .br_v281_checkpoint_path(run_dir, "refinement", 1L),
    binding = .br_v281_binding(protocol, "refinement", 1L, "all"),
    payload = FALSE)
  prior_fit_seconds <- sum(vapply(c(0L, 1L), function(q) {
    sum(vapply(.br_v281_scopes, function(scope) {
      fit <- .br_v281_read_checkpoint(
        .br_v281_checkpoint_path(run_dir, "fit", q, scope),
        binding = .br_v281_binding(protocol, "fit", q, scope))$payload
      as.numeric(fit$timing$elapsed_seconds)
    }, numeric(1L)))
  }, numeric(1L)))
  sentinel_seconds <- as.numeric(sentinel$timing$elapsed_seconds)
  projected <- prior_fit_seconds + 6.25 * sentinel_seconds
  decision <- if (isTRUE(sentinel$integrity$source_artifacts_read_only) &&
      is.finite(projected) && projected <= 14 * 3600) "continue" else "stop"
  payload <- list(
    schema_version = "br2017-v2.8.1-q2-timing-decision-v1",
    decision = decision, inspected_fields = c(
      "recorded_fit_elapsed_seconds", "source_artifacts_read_only",
      "checkpoint_sha256"),
    statistical_fields_inspected = FALSE,
    completed_q0_q1_fit_seconds = prior_fit_seconds,
    sentinel_seconds = sentinel_seconds,
    q2_projected_total_multiplier = 6.25,
    projected_completion_seconds = projected,
    cutoff_seconds = 14 * 3600,
    sentinel_sha256 = .br_v281_sha256(sentinel_path))
  .br_v281_save_atomic(payload, path, binding)
  message("Recorded frozen q=2 timing decision: ", decision,
          ". No substantive field was printed.")
  invisible(path)
}

.br_v281_calibration_summary <- function(task, bins = 10L) {
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

.br_v281_analysis <- function(repo_root, protocol) {
  run_dir <- .br_v281_run_dir(repo_root)
  path <- .br_v281_checkpoint_path(run_dir, "analysis")
  binding <- .br_v281_binding(protocol, "analysis", 1L, "all")
  if (file.exists(path) || file.exists(paste0(path, ".sha256"))) {
    .br_v281_read_checkpoint(path, binding = binding, payload = FALSE)
    message("Verified existing immutable v2.8.1 rank-comparison analysis.")
    return(invisible(path))
  }
  timing <- .br_v281_read_checkpoint(
    .br_v281_checkpoint_path(run_dir, "timing-decision"),
    binding = .br_v281_binding(protocol, "timing-decision"))$payload
  if (!timing$decision %in% c("continue", "stop")) {
    stop("The frozen q=2 timing decision is malformed.", call. = FALSE)
  }
  available_ranks <- if (identical(timing$decision, "continue")) {
    .br_v281_ranks
  } else c(0L, 1L)
  assembled <- lapply(available_ranks, function(q) {
    .br_v281_read_checkpoint(
      .br_v281_checkpoint_path(run_dir, "assembled", q),
      binding = .br_v281_binding(protocol, "assembled", q, "all"))$payload
  })
  names(assembled) <- paste0("q", available_ranks)
  refinements <- list(q1 = .br_v281_read_checkpoint(
      .br_v281_checkpoint_path(run_dir, "refinement", 1L),
      binding = .br_v281_binding(protocol, "refinement", 1L, "all"))$payload)
  if (2L %in% available_ranks) {
    refinements$q2 <- .br_v281_read_checkpoint(
      .br_v281_checkpoint_path(run_dir, "refinement", 2L),
      binding = .br_v281_binding(protocol, "refinement", 2L, "all"))$payload
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
    .br_v281_calibration_summary(x$heldout$task)
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
    schema_version = "br2017-v2.8.1-prospective-analysis-v1",
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
  .br_v281_save_atomic(payload, path, binding)
  message("Created v2.8.1 rank-comparison analysis. Substantive values were not printed.")
  invisible(path)
}

.br_v281_main <- function() {
  repo_root <- .br_v281_repo_root_from_script()
  cli <- .br_v281_parse_cli(commandArgs(trailingOnly = TRUE))
  outcome_command <- cli$command != "status"
  if (outcome_command) .br_v281_bootstrap_guard(repo_root, "v281")
  .br_v281_load_code(repo_root)
  .br_v281_one_thread()
  source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
  if (outcome_command && !nzchar(source_root)) {
    stop("Set CONJOINTSTRUCTURAL_ROOT to the shared ConjointStructural root.",
         call. = FALSE)
  }
  protocol <- .br_v281_validate_protocol(
    repo_root, source_root = if (nzchar(source_root)) source_root else NULL,
    require_external = outcome_command,
    require_clean = outcome_command)
  if (!cli$command %in% c("status", "preflight")) {
    .br_v281_load_preflight(repo_root, protocol)
  }
  if (cli$command == "status") {
    print(.br_v281_status(repo_root), row.names = FALSE)
  } else if (cli$command == "preflight") {
    .br_v281_preflight(repo_root, source_root, protocol)
  } else if (cli$command == "fit") {
    .br_v281_fit_scope(repo_root, source_root, protocol, cli$q, cli$scope,
                      cli$verbose)
  } else if (cli$command == "assemble") {
    .br_v281_assemble(repo_root, source_root, protocol, cli$q)
  } else if (cli$command == "refine") {
    .br_v281_refine(repo_root, protocol, cli$q)
  } else if (cli$command == "q2-decision") {
    .br_v281_q2_decision(repo_root, protocol)
  } else if (cli$command == "analysis") {
    .br_v281_analysis(repo_root, protocol)
  } else {
    stop("Unknown command: ", cli$command, call. = FALSE)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) .br_v281_main()
