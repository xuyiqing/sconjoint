## Helpers for the Saha--Weeks v2.1 post-fit evidence pipeline.
## This file is side-effect free when sourced.

`%||%` <- function(x, y) if (is.null(x)) y else x

.swv21_md5 <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path) ||
      dir.exists(path)) return(NA_character_)
  unname(as.character(tools::md5sum(path)))
}

.swv21_stable_read_rds <- function(path) {
  before <- .swv21_md5(path)
  if (is.na(before)) stop("Required RDS artifact is absent: ", path,
                          call. = FALSE)
  value <- readRDS(path)
  after <- .swv21_md5(path)
  if (!identical(before, after)) {
    stop("Artifact changed while being read: ", path,
         ". Wait for its producer to finish.", call. = FALSE)
  }
  list(value = value, md5 = before, path = normalizePath(path,
                                                          mustWork = TRUE))
}

.swv21_same_named_vector <- function(x, y) {
  is.atomic(x) && is.atomic(y) && identical(names(x), names(y)) &&
    identical(unname(as.character(x)), unname(as.character(y)))
}

.swv21_hash_paths <- function(paths) {
  if (!is.character(paths) || !length(paths) || is.null(names(paths)) ||
      any(!nzchar(names(paths))) || anyDuplicated(names(paths)) ||
      any(!file.exists(paths)) || any(dir.exists(paths))) {
    stop("A named file vector is malformed or incomplete.", call. = FALSE)
  }
  out <- unname(as.character(tools::md5sum(paths)))
  stats::setNames(out, names(paths))
}

.swv21_path_within <- function(path, directory) {
  path <- normalizePath(path, mustWork = TRUE)
  directory <- normalizePath(directory, mustWork = TRUE)
  identical(path, directory) || startsWith(
    path, paste0(directory, .Platform$file.sep))
}

.swv21_resolve_reported_primary <- function(producer_dir, producer_config,
                                             postfit_config) {
  producer_dir <- normalizePath(producer_dir, mustWork = TRUE)
  manifest_path <- file.path(producer_dir, "manifest.rds")
  pointer_path <- file.path(producer_dir, "reported_primary_pointer.rds")
  manifest_read <- .swv21_stable_read_rds(manifest_path)
  manifest <- manifest_read$value

  manifest_gate <- is.list(manifest) &&
    identical(manifest$schema_version,
              postfit_config$producer_manifest_schema) &&
    identical(manifest$configuration_version,
              postfit_config$producer_config_version) &&
    identical(manifest$configuration_version, producer_config$version) &&
    identical(manifest$final_analysis_success, TRUE) &&
    identical(manifest$procedural_primary_available, TRUE) &&
    identical(manifest$formal_inference_available, FALSE) &&
    identical(manifest$outcome_blind, FALSE) &&
    identical(manifest$production_result, FALSE) &&
    is.character(manifest$artifacts) && length(manifest$artifacts) > 0L &&
    !is.null(names(manifest$artifacts)) &&
    !anyDuplicated(names(manifest$artifacts)) &&
    "reported_primary_pointer.rds" %in% names(manifest$artifacts)
  if (!manifest_gate) {
    stop("The v2.1 producer manifest is incomplete, unsuccessful, or has an ",
         "incompatible schema. No post-fit evidence was read.", call. = FALSE)
  }

  artifact_paths <- file.path(producer_dir, names(manifest$artifacts))
  names(artifact_paths) <- names(manifest$artifacts)
  artifact_hashes <- .swv21_hash_paths(artifact_paths)
  if (!.swv21_same_named_vector(artifact_hashes, manifest$artifacts)) {
    stop("At least one manifested v2.1 producer artifact is absent or stale.",
         call. = FALSE)
  }

  pointer_read <- .swv21_stable_read_rds(pointer_path)
  pointer <- pointer_read$value
  if (!identical(pointer_read$md5,
                 as.character(manifest$artifacts[[
                   "reported_primary_pointer.rds"]]))) {
    stop("The reported-primary pointer does not match the completed manifest.",
         call. = FALSE)
  }

  expected_primary <- if (isTRUE(pointer$fallback_applied)) {
    "exact_constant_q1"
  } else "selected_procedure_q1"
  pointer_gate <- is.list(pointer) &&
    identical(pointer$schema_version,
              postfit_config$producer_pointer_schema) &&
    identical(pointer$reported_primary, manifest$reported_primary) &&
    identical(pointer$reported_primary, expected_primary) &&
    identical(pointer$fallback_applied, manifest$fallback_applied) &&
    identical(pointer$formal_inference_available, FALSE) &&
    identical(pointer$outcome_blind, FALSE) &&
    identical(pointer$descriptive_only, TRUE) &&
    identical(pointer$formal_test, FALSE) &&
    .swv21_same_named_vector(pointer$generation_input_md5,
                             manifest$generation_input_md5) &&
    identical(pointer$runtime_signature, manifest$runtime_signature) &&
    identical(pointer$authorization_md5, manifest$authorization_md5)
  if (!pointer_gate) {
    stop("The completed reported-primary pointer is internally inconsistent.",
         call. = FALSE)
  }

  chosen <- if (isTRUE(pointer$fallback_applied)) {
    pointer$exact_constant_paths
  } else pointer$selected_procedure_paths
  path_gate <- is.list(chosen) &&
    identical(unname(as.character(c(
      pointer$full_fit_path, pointer$nested_fit_path,
      pointer$assembled_fit_path))),
      unname(as.character(c(chosen$full, chosen$nested, chosen$assembled))))
  if (!path_gate) {
    stop("The pointer's chosen-fit paths do not implement its fallback rule.",
         call. = FALSE)
  }
  fit_paths <- c(full = chosen$full, nested = chosen$nested,
                 assembled = chosen$assembled)
  if (any(!file.exists(fit_paths)) ||
      any(!vapply(fit_paths, .swv21_path_within, logical(1L),
                  directory = producer_dir)) ||
      any(!basename(fit_paths) %in% names(manifest$artifacts))) {
    stop("A reported-primary fit path is missing, unmanifested, or outside ",
         "the completed producer directory.", call. = FALSE)
  }
  fit_hashes <- .swv21_hash_paths(fit_paths)
  expected_fit_hashes <- stats::setNames(
    as.character(manifest$artifacts[basename(fit_paths)]), names(fit_paths))
  if (!.swv21_same_named_vector(fit_hashes, expected_fit_hashes)) {
    stop("A reported-primary fit no longer matches the producer manifest.",
         call. = FALSE)
  }

  input_paths <- manifest$input_paths
  if (!is.character(input_paths) || !length(input_paths) ||
      is.null(names(input_paths)) || any(!nzchar(names(input_paths))) ||
      anyDuplicated(names(input_paths))) {
    stop("The producer generation-input vector is malformed.", call. = FALSE)
  }
  live_input_hashes <- .swv21_hash_paths(input_paths)
  if (!.swv21_same_named_vector(live_input_hashes,
                                manifest$generation_input_md5) ||
      !.swv21_same_named_vector(live_input_hashes,
                                manifest$completion_input_md5)) {
    stop("A frozen producer input changed before post-fit evidence extraction.",
         call. = FALSE)
  }

  list(
    schema_version = "sw2022-v2.1-primary-resolution-v1",
    producer_dir = producer_dir,
    manifest = manifest,
    manifest_path = manifest_read$path,
    manifest_md5 = manifest_read$md5,
    pointer = pointer,
    pointer_path = pointer_read$path,
    pointer_md5 = pointer_read$md5,
    reported_primary = pointer$reported_primary,
    fallback_applied = pointer$fallback_applied,
    fit_paths = stats::setNames(
      normalizePath(fit_paths, mustWork = TRUE), names(fit_paths)),
    fit_md5 = fit_hashes,
    producer_artifact_paths = stats::setNames(
      normalizePath(artifact_paths, mustWork = TRUE), names(artifact_paths)),
    producer_artifact_md5 = artifact_hashes,
    producer_input_paths = stats::setNames(
      normalizePath(input_paths, mustWork = TRUE), names(input_paths)),
    producer_input_md5 = live_input_hashes,
    formal_inference_available = FALSE,
    outcome_blind = FALSE,
    resolved_once = TRUE
  )
}

.swv21_assert_resolution_unchanged <- function(snapshot) {
  checks <- list(
    manifest = identical(.swv21_md5(snapshot$manifest_path),
                         snapshot$manifest_md5),
    pointer = identical(.swv21_md5(snapshot$pointer_path),
                        snapshot$pointer_md5),
    artifacts = .swv21_same_named_vector(
      .swv21_hash_paths(snapshot$producer_artifact_paths),
      snapshot$producer_artifact_md5),
    inputs = .swv21_same_named_vector(
      .swv21_hash_paths(snapshot$producer_input_paths),
      snapshot$producer_input_md5)
  )
  if (!all(unlist(checks, use.names = FALSE))) {
    stop("The resolved producer snapshot changed during post-fit extraction.",
         call. = FALSE)
  }
  invisible(checks)
}

.swv21_validate_fit_stamp <- function(fit, expected_role, snapshot,
                                      producer_config) {
  stamp <- fit$sw_v21_application_specification
  ok <- is.list(stamp) &&
    identical(stamp$config_version, producer_config$version) &&
    identical(stamp$role, expected_role) &&
    .swv21_same_named_vector(stamp$generation_input_md5,
                             snapshot$producer_input_md5) &&
    identical(stamp$runtime_signature, snapshot$manifest$runtime_signature) &&
    identical(stamp$authorization_md5,
              snapshot$manifest$authorization_md5) &&
    identical(stamp$formal_inference_available, FALSE) &&
    identical(stamp$outcome_blind, FALSE) &&
    identical(stamp$production_result, FALSE)
  if (!ok) stop("A reported-primary fit has a stale or incompatible stamp: ",
                expected_role, call. = FALSE)
  invisible(TRUE)
}

.swv21_atomic_save_rds <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.swv21_write_csv <- function(x, path) {
  if (is.null(x)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.swv21_contrasts <- function(coordinate_names) {
  expected <- c(
    "cand_genderMale", "cand_runYes", "cand_talentCollaborative",
    "cand_talentDetermined.to.Succeed", "cand_talentEmpathetic",
    "cand_talentGood.Communicator", "cand_talentHard.Working",
    "cand_talentTough.Negotiator", "cand_agendaModerate.Changes",
    "cand_agendaComplete.Overhaul", "cand_child1.child",
    "cand_child2.children", "cand_child3.children")
  if (!identical(as.character(coordinate_names), expected)) {
    stop("The frozen Saha--Weeks 13-coordinate basis changed.",
         call. = FALSE)
  }
  one <- function(...) {
    entries <- list(...)
    out <- stats::setNames(numeric(length(expected)), expected)
    for (entry in entries) out[[entry[[1L]]]] <-
      out[[entry[[1L]]]] + entry[[2L]]
    unname(out)
  }
  e <- function(name, value = 1) list(name, value)
  list(
    female_vs_male = one(e("cand_genderMale", -1)),
    run_yes_vs_no = one(e("cand_runYes", 1)),
    talent_collaborative_vs_empathetic = one(
      e("cand_talentCollaborative", 1), e("cand_talentEmpathetic", -1)),
    talent_determined_vs_empathetic = one(
      e("cand_talentDetermined.to.Succeed", 1),
      e("cand_talentEmpathetic", -1)),
    talent_assertive_vs_empathetic = one(e("cand_talentEmpathetic", -1)),
    talent_good_communicator_vs_empathetic = one(
      e("cand_talentGood.Communicator", 1),
      e("cand_talentEmpathetic", -1)),
    talent_hard_working_vs_empathetic = one(
      e("cand_talentHard.Working", 1), e("cand_talentEmpathetic", -1)),
    talent_tough_negotiator_vs_empathetic = one(
      e("cand_talentTough.Negotiator", 1),
      e("cand_talentEmpathetic", -1)),
    agenda_moderate_vs_very_few = one(e("cand_agendaModerate.Changes", 1)),
    agenda_complete_vs_very_few = one(e("cand_agendaComplete.Overhaul", 1)),
    one_child_vs_none = one(e("cand_child1.child", 1)),
    two_children_vs_none = one(e("cand_child2.children", 1)),
    three_children_vs_none = one(e("cand_child3.children", 1))
  )
}

.swv21_contrast_labels <- function() c(
  female_vs_male = "Female vs Male",
  run_yes_vs_no = "Previously ran: Yes vs No",
  talent_collaborative_vs_empathetic = "Collaborative vs Empathetic",
  talent_determined_vs_empathetic = "Determined vs Empathetic",
  talent_assertive_vs_empathetic = "Assertive vs Empathetic",
  talent_good_communicator_vs_empathetic = "Good Communicator vs Empathetic",
  talent_hard_working_vs_empathetic = "Hard-Working vs Empathetic",
  talent_tough_negotiator_vs_empathetic = "Tough Negotiator vs Empathetic",
  agenda_moderate_vs_very_few = "Moderate Changes vs Very Few",
  agenda_complete_vs_very_few = "Complete Overhaul vs Very Few",
  one_child_vs_none = "1 child vs No children",
  two_children_vs_none = "2 children vs No children",
  three_children_vs_none = "3 children vs No children")

.swv21_meta <- function(prepared) {
  rid <- unique(as.character(prepared$respondent_id))
  meta <- as.data.frame(prepared$respondent_meta, stringsAsFactors = FALSE)
  if (!all(c("respondent_id", "party", "respondent_gender") %in%
           names(meta)) || anyDuplicated(meta$respondent_id) ||
      !setequal(as.character(meta$respondent_id), rid)) {
    stop("respondent_meta does not uniquely cover fitted respondents.",
         call. = FALSE)
  }
  meta <- meta[match(rid, as.character(meta$respondent_id)), , drop = FALSE]
  meta$party3 <- ifelse(grepl("Republican", meta$party), "Republican",
    ifelse(grepl("Independent", meta$party), "Independent",
      ifelse(grepl("Democrat", meta$party), "Democrat", NA_character_)))
  meta$respondent_gender2 <- tools::toTitleCase(
    tolower(trimws(as.character(meta$respondent_gender))))
  if (anyNA(meta$party3) ||
      any(!meta$respondent_gender2 %in% c("Female", "Male"))) {
    stop("Party or respondent-gender mapping failed.", call. = FALSE)
  }
  rownames(meta) <- NULL
  meta
}

.swv21_cluster_lm <- function(y, X, cluster) {
  X <- as.matrix(X); y <- as.numeric(y); cluster <- as.character(cluster)
  if (nrow(X) != length(y) || length(cluster) != length(y) ||
      any(!is.finite(X)) || any(!is.finite(y)) || anyNA(cluster)) {
    stop("Malformed respondent-clustered linear-model inputs.",
         call. = FALSE)
  }
  fit <- stats::lm.fit(X, y)
  if (fit$rank != ncol(X)) stop("The benchmark design is rank deficient.",
                                call. = FALSE)
  bread <- solve(crossprod(X))
  cluster_score <- rowsum(X * as.numeric(fit$residuals), cluster,
                          reorder = FALSE)
  G <- nrow(cluster_score); n <- nrow(X); k <- ncol(X)
  correction <- if (G > 1L && n > k) {
    (G / (G - 1)) * ((n - 1) / (n - k))
  } else NA_real_
  vcov <- correction * bread %*% crossprod(cluster_score) %*% bread
  list(coef = as.numeric(fit$coefficients), vcov = vcov,
       residual = as.numeric(fit$residuals), clusters = G, n = n)
}

.swv21_direct_amce <- function(prepared, keep = rep(TRUE, length(prepared$y)),
                               group_variable = "Overall",
                               group = "Overall") {
  dx <- as.matrix(prepared$deltaX)[keep, , drop = FALSE]
  y <- as.numeric(prepared$y)[keep]
  rid <- as.character(prepared$respondent_id)[keep]
  contrasts <- .swv21_contrasts(colnames(dx))
  C <- do.call(rbind, contrasts)
  fit <- .swv21_cluster_lm(y, cbind(`Candidate A intercept` = 1, dx), rid)
  b <- fit$coef[-1L]
  V <- fit$vcov[-1L, -1L, drop = FALSE]
  estimate <- as.numeric(C %*% b)
  se <- sqrt(pmax(diag(C %*% V %*% t(C)), 0))
  data.frame(
    contrast = names(contrasts), label = unname(.swv21_contrast_labels()[
      names(contrasts)]), group_variable = group_variable, group = group,
    estimate = estimate, respondent_cluster_se = se,
    conf_low_diagnostic = estimate - stats::qnorm(0.975) * se,
    conf_high_diagnostic = estimate + stats::qnorm(0.975) * se,
    n_tasks = fit$n, n_respondents = fit$clusters,
    estimand = paste(
      "Respondent-clustered difference-coded linear-probability AMCE-style",
      "marginal choice effect; not a mixed-logit preference coefficient."),
    design_condition = paste(
      "Conditional on advertised independent randomized profile assignment;",
      "fielded protocol probabilities were not machine-verified."),
    formal_inference_for_structural_model = FALSE,
    stringsAsFactors = FALSE)
}

.swv21_fold_basis <- function(assembled, prepared) {
  rid <- unique(as.character(prepared$respondent_id))
  first <- match(rid, as.character(prepared$respondent_id))
  fold_raw <- assembled$fold_id
  fold_task <- if (length(fold_raw) == nrow(prepared$deltaX)) {
    as.integer(fold_raw)
  } else if (length(fold_raw) == length(rid)) {
    as.integer(fold_raw)[match(as.character(prepared$respondent_id), rid)]
  } else stop("The assembled fold vector has incompatible length.",
              call. = FALSE)
  fold_resp <- fold_task[first]
  if (any(vapply(split(fold_task, prepared$respondent_id),
                 function(x) length(unique(x)) != 1L, logical(1L)))) {
    stop("A respondent is split across outer folds.", call. = FALSE)
  }
  Z_resp <- as.matrix(prepared$Z_primary)[first, , drop = FALSE]
  K <- length(unique(fold_resp))
  lapply(seq_len(K), function(k) {
    train <- fold_resp != k
    center <- colMeans(Z_resp[train, , drop = FALSE])
    scale <- apply(Z_resp[train, , drop = FALSE], 2L, stats::sd)
    scale[!is.finite(scale) | scale < 1e-12] <- 1
    B <- cbind(`(Intercept)` = 1,
               sweep(sweep(Z_resp, 2L, center, `-`), 2L, scale, `/`))
    qr_train <- qr(B[train, , drop = FALSE], tol = 1e-10,
                   LAPACK = FALSE)
    keep <- sort(qr_train$pivot[seq_len(qr_train$rank)])
    B[, keep, drop = FALSE]
  })
}

.swv21_crossfit_contract <- function(nested, assembled, prepared,
                                     predictions, assessment) {
  rid <- as.character(prepared$respondent_id)
  raw_fold <- as.integer(nested$outer_fold_id)
  outer_order <- as.character(nested$outer_folds)
  mapped_fold <- match(as.character(raw_fold), outer_order)
  tuning_gate <- is.list(nested$tuning) &&
    length(nested$tuning) == length(outer_order) &&
    all(vapply(nested$tuning, function(x) {
      isTRUE(x$candidate_selection_gate$pass)
    }, logical(1L)))
  checks <- data.frame(
    check = c(
      "one raw outer-fold label per task row",
      "respondents never split across raw outer folds",
      "raw outer folds map exactly to assembled fold indices",
      "all outer-training candidate-selection gates pass",
      "all assembled optimizer/nesting gates pass",
      "nested object records outer-training-only construction",
      "assembled object records selected nested refits",
      "prediction routine records out-of-fold evaluation",
      "shared factor used across each complete sequence",
      "ordinary-inference eligibility deliberately withheld"),
    pass = c(
      length(raw_fold) == nrow(prepared$deltaX) &&
        setequal(unique(raw_fold), as.integer(outer_order)),
      length(raw_fold) == length(rid) &&
        all(vapply(split(raw_fold, rid), function(x) {
          length(unique(x)) == 1L
        }, logical(1L))),
      length(mapped_fold) == length(assembled$fold_id) &&
        identical(as.integer(mapped_fold), as.integer(assembled$fold_id)),
      tuning_gate &&
        all(nested$candidate_selection_gate_by_outer_fold),
      is.list(assembled$optimization) &&
        all(assembled$optimization$gate_by_fold) &&
        all(assembled$optimization$candidate_selection_gate_by_fold) &&
        all(assembled$optimization$nested_objective_gate_by_fold) &&
        all(assembled$optimization$pooled_prefit_gate_by_fold) &&
        all(assembled$optimization$continued_constant_gate_by_fold) &&
        all(assembled$optimization$compact_bound_gate_by_fold),
      is.character(nested$nesting) && length(nested$nesting) == 1L &&
        grepl("outer-training respondents only", nested$nesting,
              fixed = TRUE),
      identical(assembled$source,
                "selected refits from respondent-level nested tuning"),
      isTRUE(predictions$out_of_fold),
      isTRUE(predictions$complete_sequence) &&
        isTRUE(predictions$shared_factor_within_sequence),
      isTRUE(assembled$diagnostic_only) &&
        identical(assembled$eligible_for_ordinary_inference, FALSE) &&
        identical(predictions$training_only_tuning, FALSE) &&
        identical(assessment$score$verified_heldout, FALSE) &&
        identical(assessment$calibration$verified_heldout, FALSE)),
    stringsAsFactors = FALSE)
  checks$detail <- c(
    paste(length(raw_fold), "task rows across", length(outer_order),
          "raw folds"),
    paste(length(unique(rid)), "respondents"),
    paste("assembled order:", paste(outer_order, collapse = ",")),
    paste(length(nested$tuning), "outer tuning problems"),
    paste(length(assembled$optimization$gate_by_fold), "assembled folds"),
    nested$nesting %||% "missing",
    assembled$source %||% "missing",
    predictions$provenance %||% "missing",
    predictions$note %||% "missing",
    paste(
      "The package's ordinary verified-heldout flag is false because the",
      "assembled object is intentionally diagnostic-only. The application",
      "contract above validates respondent outer-training construction but",
      "does not restore outcome-blindness or ordinary-inference eligibility."
    ))
  list(
    validated = all(checks$pass), checks = checks,
    raw_outer_fold = raw_fold, assembled_fold = as.integer(mapped_fold),
    package_verified_heldout = FALSE,
    ordinary_inference_eligible = FALSE,
    outcome_blind = FALSE,
    role = "respondent_cross_fitted_postpilot_diagnostic")
}

.swv21_kappa_target <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                                attr_names) {
  N <- nrow(mu); p <- ncol(mu)
  list(target_type = "rowwise_expectation",
       value = matrix(rep(kappa, N), ncol = 1L),
       d_mu = array(0, c(N, 1L, p)), d_kappa = matrix(1, N, 1L),
       sigma_invariant = TRUE, labels = "kappa")
}

.swv21_serial_summary <- function(task_predictions) {
  d <- as.data.frame(task_predictions, stringsAsFactors = FALSE)
  d$residual <- d$observed - d$predicted
  d <- d[order(d$respondent_id, d$task_order, method = "radix"),,
         drop = FALSE]
  parts <- split(seq_len(nrow(d)), d$respondent_id)
  pairs <- do.call(rbind, lapply(parts, function(ii) {
    if (length(ii) < 2L) return(NULL)
    data.frame(
      respondent_id = d$respondent_id[ii[-1L]],
      task_order = d$task_order[ii[-1L]],
      lag_residual = d$residual[ii[-length(ii)]],
      residual = d$residual[ii[-1L]], stringsAsFactors = FALSE)
  }))
  if (is.null(pairs) || nrow(pairs) < 2L) return(data.frame())
  serial_fit <- .swv21_cluster_lm(
    pairs$residual, cbind(intercept = 1, lag = pairs$lag_residual),
    pairs$respondent_id)
  order_fit <- .swv21_cluster_lm(
    d$residual, cbind(intercept = 1, task = d$task_order), d$respondent_id)
  data.frame(
    diagnostic = c("adjacent residual correlation",
                   "adjacent residual slope", "task-order residual slope"),
    estimate = c(stats::cor(pairs$lag_residual, pairs$residual),
                 serial_fit$coef[[2L]], order_fit$coef[[2L]]),
    diagnostic_se = c(NA_real_, sqrt(serial_fit$vcov[2L, 2L]),
                      sqrt(order_fit$vcov[2L, 2L])),
    n_rows = c(nrow(pairs), nrow(pairs), nrow(d)),
    role = paste(
      "Descriptive cross-fitted post-pilot diagnostic; cannot establish",
      "independent shocks or absence of task-process misspecification."),
    stringsAsFactors = FALSE)
}

.swv21_group_calibration <- function(task, group_variable, group) {
  if (length(group) != nrow(task) || anyNA(group)) {
    stop("Calibration group is malformed.", call. = FALSE)
  }
  levels <- unique(as.character(group))
  do.call(rbind, lapply(levels, function(lev) {
    keep <- as.character(group) == lev
    gap <- task$observed[keep] - task$predicted[keep]
    cluster_gap <- tapply(gap, task$respondent_id[keep], mean)
    data.frame(
      group_variable = group_variable, group = lev,
      observed_rate = mean(task$observed[keep]),
      predicted_rate = mean(task$predicted[keep]),
      calibration_gap = mean(gap),
      respondent_se_gap = stats::sd(cluster_gap) / sqrt(length(cluster_gap)),
      brier_score = mean((task$observed[keep] - task$predicted[keep])^2),
      n_respondents = length(unique(task$respondent_id[keep])),
      n_tasks = sum(keep), stringsAsFactors = FALSE)
  }))
}

.swv21_read_artifact_manifest <- function(path, project_root) {
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  if (!identical(names(manifest), c("path", "bytes", "md5")) ||
      !nrow(manifest) || anyDuplicated(manifest$path)) {
    stop("Estimator-independent artifact manifest is malformed: ", path,
         call. = FALSE)
  }
  paths <- ifelse(startsWith(manifest$path, "/"), manifest$path,
                  file.path(project_root, manifest$path))
  exists <- file.exists(paths) & !dir.exists(paths)
  observed <- rep(NA_character_, length(paths))
  observed[exists] <- unname(as.character(tools::md5sum(paths[exists])))
  bytes <- rep(NA_real_, length(paths))
  bytes[exists] <- file.info(paths[exists])$size
  audit <- data.frame(
    path = normalizePath(paths, mustWork = FALSE), exists = exists,
    expected_bytes = manifest$bytes, observed_bytes = bytes,
    expected_md5 = as.character(manifest$md5), observed_md5 = observed,
    pass = exists & bytes == manifest$bytes & observed == manifest$md5,
    stringsAsFactors = FALSE)
  if (!all(audit$pass)) {
    stop("Estimator-independent preparation/design artifacts are stale: ",
         path, call. = FALSE)
  }
  audit
}

.swv21_table_inventory <- function(table_dir) {
  paths <- sort(list.files(table_dir, pattern = "[.]csv$", full.names = TRUE))
  if (!length(paths)) return(data.frame())
  do.call(rbind, lapply(paths, function(path) {
    x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    data.frame(
      table = basename(path), rows = nrow(x), columns = ncol(x),
      column_names = paste(names(x), collapse = " | "),
      bytes = file.info(path)$size, md5 = .swv21_md5(path),
      stringsAsFactors = FALSE)
  }))
}
