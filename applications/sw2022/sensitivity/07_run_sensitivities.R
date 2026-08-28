#!/usr/bin/env Rscript

## Fit-aware application sensitivities for Saha--Weeks (2022).
##
## This script is intentionally isolated from 03_fit_models.R. It consumes the
## frozen primary artifacts but never changes them or the production config.
## All outputs live in <fit_dir>/sensitivity_analysis.

options(stringsAsFactors = FALSE, warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

.script_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this file with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.parse_cli <- function(args) {
  out <- list(profile = "smoke", stage = "all", force = FALSE)
  for (arg in args) {
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

.atomic_save <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path,
                                    call. = FALSE)
  invisible(path)
}

.write_csv <- function(x, path) {
  if (is.null(x)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(TRUE)
}

.run_or_load <- function(path, overwrite, expr, validator = NULL) {
  if (file.exists(path) && !overwrite) {
    message("checkpoint: loading ", path)
    value <- readRDS(path)
    if (!is.null(validator) && !isTRUE(validator(value))) {
      stop("Sensitivity checkpoint has a stale analysis stamp: ", path,
           ". Rerun with --force=true.", call. = FALSE)
    }
    return(value)
  }
  value <- base::force(expr)
  .atomic_save(value, path)
  message("checkpoint: wrote ", path)
  value
}

.sensitivity_component_stamp <- function(schema_version, config_version,
                                         profile, controls, paths) {
  required <- c("prepared", "design", "primary_full", "primary_nested",
                "primary_assembled")
  if (!is.list(paths) || !all(required %in% names(paths)) ||
      any(!file.exists(unlist(paths[required])))) {
    stop("Sensitivity component stamps require every frozen input artifact.",
         call. = FALSE)
  }
  list(
    schema_version = schema_version,
    primary_config_version = config_version,
    profile = profile,
    controls = controls,
    prepared_md5 = unname(tools::md5sum(paths$prepared)),
    design_md5 = unname(tools::md5sum(paths$design)),
    primary_full_md5 = unname(tools::md5sum(paths$primary_full)),
    primary_nested_md5 = unname(tools::md5sum(paths$primary_nested)),
    primary_assembled_md5 = unname(tools::md5sum(paths$primary_assembled))
  )
}

.stamp_sensitivity_component <- function(x, stamp, component) {
  x$sw_sensitivity_specification <- c(stamp, list(component = component))
  x
}

.valid_sensitivity_component <- function(x, stamp, component) {
  observed <- x$sw_sensitivity_specification
  is.list(observed) && identical(observed,
                                 c(stamp, list(component = component)))
}

.matrix_rank <- function(x, tol = 1e-9) {
  x <- as.matrix(x)
  if (!length(x)) return(0L)
  s <- svd(x, nu = 0L, nv = 0L)$d
  if (!length(s) || max(s) == 0) return(0L)
  sum(s > tol * max(s))
}

.cluster_lm <- function(y, X, respondent_id) {
  X <- as.matrix(X); y <- as.numeric(y); rid <- as.character(respondent_id)
  fit <- stats::lm.fit(X, y)
  if (fit$rank != ncol(X)) stop("Clustered LPM is rank deficient.",
                                call. = FALSE)
  bread <- solve(crossprod(X))
  score <- rowsum(X * as.numeric(fit$residuals), rid, reorder = FALSE)
  G <- nrow(score); n <- nrow(X); k <- ncol(X)
  correction <- (G / (G - 1)) * ((n - 1) / (n - k))
  vcov <- correction * bread %*% crossprod(score) %*% bread
  list(coef = as.numeric(fit$coefficients), vcov = vcov,
       residual = as.numeric(fit$residuals), G = G, n = n)
}

.check_constant_with_na <- function(Z, rid) {
  ids <- unique(as.character(rid))
  for (id in ids) {
    ii <- which(as.character(rid) == id)
    ref <- Z[ii[1L], ]
    for (j in ii[-1L]) {
      same <- (is.na(Z[j, ]) & is.na(ref)) |
        (!is.na(Z[j, ]) & !is.na(ref) & abs(Z[j, ] - ref) <= 1e-12)
      if (!all(same)) stop("A moderator varies within respondent: ", id,
                           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.fit_imputer <- function(Z, rid, train) {
  Z <- as.matrix(Z)
  train_index <- which(train)
  first_index <- train_index[!duplicated(as.character(rid[train_index]))]
  med <- apply(Z[first_index, , drop = FALSE], 2L, stats::median, na.rm = TRUE)
  if (any(!is.finite(med))) {
    stop("A moderator is entirely missing in a training sample.", call. = FALSE)
  }
  names(med) <- colnames(Z)
  list(median = med, training_respondents = unique(as.character(rid[train])),
       method = "training-respondent column median")
}

.apply_imputer <- function(Z, imputer) {
  out <- as.matrix(Z)
  for (j in seq_len(ncol(out))) {
    miss <- is.na(out[, j])
    if (any(miss)) out[miss, j] <- imputer$median[[j]]
  }
  if (any(!is.finite(out))) stop("Imputation did not produce finite Z.",
                                  call. = FALSE)
  out
}

.fit_one_fixed <- function(dx, y, Z_raw, rid, train, spec, grid, controls,
                           seed, role) {
  train <- as.logical(train)
  if (length(train) != nrow(dx) || sum(train) < 2L ||
      length(unique(as.character(rid[train]))) < 2L) {
    stop("Malformed training rows for ", role, ".", call. = FALSE)
  }
  imputer <- .fit_imputer(Z_raw, rid, train)
  Z <- .apply_imputer(Z_raw, imputer)
  z_transform <- .sc_fit_z_transform(Z[train, , drop = FALSE], rid[train])
  dx_transform <- .sc_comp_fit_dx_scale(dx[train, , drop = FALSE], rid[train])
  Z_scaled <- .sc_apply_z_transform(Z, z_transform)
  dx_scaled <- sweep(dx, 2L, dx_transform$scale, `/`)
  fit <- .sc_train_mixed_multistart(
    deltaX = dx_scaled[train, , drop = FALSE], y = y[train],
    Z = Z_scaled[train, , drop = FALSE], respondent_id = rid[train],
    gh = grid, hidden = spec$hidden,
    n_epochs = controls$n_epochs,
    learning_rate = controls$learning_rate,
    weight_decay = spec$weight_decay,
    n_starts = controls$n_starts, seed = seed,
    device = controls$device, verbose = FALSE, warm_state = NULL,
    early_stop = FALSE, opt_tol = controls$opt_tol,
    grad_tol = controls$grad_tol, mu_bound = controls$mu_bound,
    kappa_bound = controls$kappa_bound, a_bound = controls$a_bound,
    weight_bound = controls$weight_bound,
    coefficient_scale = dx_transform$scale
  )
  summary <- .sc_comp_fit_summary(fit)
  mu_all <- sweep(.sc_predict_beta(fit$net, Z_scaled), 2L,
                  dx_transform$scale, `/`)
  colnames(mu_all) <- colnames(dx)
  A <- fit$A / dx_transform$scale
  rownames(A) <- colnames(dx)
  list(
    role = role, net = fit$net, mu_all = mu_all, A = A,
    Sigma = tcrossprod(A), kappa = fit$kappa, specification = spec,
    integration_grid = grid, preprocessing = list(
      imputation = imputer, Z = z_transform, deltaX = dx_transform
    ), optimization = summary, training_rows = which(train),
    training_respondents = unique(as.character(rid[train])),
    missing_values_before_imputation = colSums(is.na(Z_raw)),
    full_fit = all(train)
  )
}

.fit_fixed_sensitivity <- function(dx, y, Z_raw, rid, task, primary_full,
                                   primary_nested = NULL, controls, seed,
                                   role, outer_fold_id = NULL,
                                   heldout_tuning_is_training_only = TRUE) {
  dx <- as.matrix(dx); Z_raw <- as.matrix(Z_raw); y <- as.numeric(y)
  rid <- as.character(rid)
  stopifnot(nrow(dx) == length(y), nrow(Z_raw) == nrow(dx),
            length(rid) == nrow(dx), length(task) == nrow(dx))
  .check_constant_with_na(Z_raw, rid)
  full_spec <- primary_full$selected
  full_grid <- primary_full$refit$integration_grid
  q <- as.integer(full_spec$q)

  if (is.null(outer_fold_id)) {
    outer_fold_id <- .sc_make_folds(rid, K = controls$outer_K,
                                    seed = seed + 17L)
    fold_specs <- rep(list(full_spec), controls$outer_K)
    fold_grids <- rep(list(full_grid), controls$outer_K)
    heldout_tuning_is_training_only <- FALSE
  } else {
    if (length(outer_fold_id) != length(rid)) {
      stop("Inherited outer folds do not match the sensitivity sample.",
           call. = FALSE)
    }
    folds0 <- unique(as.character(outer_fold_id))
    if (is.null(primary_nested) ||
        length(primary_nested$tuning) != length(folds0)) {
      stop("Inherited folds require the matching primary nested tuning object.",
           call. = FALSE)
    }
    fold_specs <- lapply(primary_nested$tuning, `[[`, "selected")
    fold_grids <- lapply(primary_nested$tuning,
                         function(x) x$refit$integration_grid)
  }
  folds <- unique(as.character(outer_fold_id))
  fold_index <- match(as.character(outer_fold_id), folds)
  if (any(vapply(split(fold_index, rid), function(x)
    length(unique(x)) != 1L, logical(1L)))) {
    stop("A respondent was split across sensitivity folds.", call. = FALSE)
  }

  fold_fits <- vector("list", length(folds))
  for (k in seq_along(folds)) {
    message(role, ": fitting outer fold ", k, "/", length(folds))
    fold_fits[[k]] <- .fit_one_fixed(
      dx, y, Z_raw, rid, train = fold_index != k,
      spec = fold_specs[[k]], grid = fold_grids[[k]], controls = controls,
      seed = seed + 1000L * k, role = paste0(role, "_outer_", k)
    )
  }
  message(role, ": fitting full sample")
  full <- .fit_one_fixed(
    dx, y, Z_raw, rid, train = rep(TRUE, length(y)), spec = full_spec,
    grid = full_grid, controls = controls, seed = seed + 90001L,
    role = paste0(role, "_full")
  )

  Z_signature <- .apply_imputer(Z_raw, .fit_imputer(
    Z_raw, rid, rep(TRUE, length(rid))))
  signature <- .sc_analysis_signature(
    deltaX = dx, y = y, Z = Z_signature, respondent_id = rid,
    fold_id = fold_index, specification = list(
      workflow = "sw2022-application-fixed-spec-sensitivity",
      role = role, q = q,
      fold_specifications = lapply(fold_specs, function(x)
        x[c("name", "hidden", "weight_decay", "integration", "q")]),
      imputation = "training-respondent median",
      outcome_dependent_tuning_uses_heldout =
        !isTRUE(heldout_tuning_is_training_only)
    )
  )
  opt_gate <- vapply(fold_fits, function(x)
    isTRUE(x$optimization$optimization_gate_pass), logical(1L))
  primary_selection_gate <- if (!is.null(primary_nested)) {
    vapply(primary_nested$tuning, function(x)
      isTRUE(x$candidate_selection_gate$pass) &&
        isTRUE(x$primary_inference_eligible), logical(1L))
  } else rep(FALSE, length(folds))
  diagnostic_profile <- isTRUE(controls$diagnostic_only)
  eligible <- isTRUE(heldout_tuning_is_training_only) &&
    all(primary_selection_gate) && all(opt_gate) && !diagnostic_profile

  assembled <- list(
    deltaX = dx, y = y, Z = Z_signature, respondent_id = rid,
    fold_id = as.integer(fold_index), K = length(folds),
    N = length(unique(rid)), q = q,
    mu_hat = do.call(rbind, lapply(seq_len(nrow(dx)), function(j)
      fold_fits[[fold_index[j]]]$mu_all[j, , drop = FALSE])),
    mu_all_folds = lapply(fold_fits, `[[`, "mu_all"),
    A_folds = lapply(fold_fits, `[[`, "A"),
    A_computational_folds = lapply(fold_fits, `[[`, "A"),
    kappa_folds = vapply(fold_fits, `[[`, numeric(1L), "kappa"),
    integration_grids_folds = lapply(fold_fits, `[[`, "integration_grid"),
    integration_grid = fold_fits[[1L]]$integration_grid,
    gh = fold_fits[[1L]]$integration_grid,
    attr_names = colnames(dx), z_names = colnames(Z_raw),
    selected_specifications = fold_specs,
    preprocessing_folds = lapply(fold_fits, `[[`, "preprocessing"),
    optimization = list(
      folds = lapply(fold_fits, `[[`, "optimization"),
      gate_by_fold = opt_gate,
      candidate_selection_gate_by_fold = primary_selection_gate,
      diagnostics_are_certificates = FALSE
    ),
    computational_gate_pass = eligible,
    eligible_for_ordinary_inference = eligible,
    diagnostic_only = !eligible, analysis_signature = signature,
    source = paste(
      "application sensitivity with primary outer-training architecture",
      "and fold-specific imputation/preprocessing"
    ),
    scope = paste(
      "Sensitivity only. Reuse of training-only primary tuning does not",
      "promote the alternative specification to the maintained model."
    )
  )
  class(assembled) <- c("scmix_nested_assembled", "list")

  full_fit <- list(
    respondent_id = rid, Z = Z_signature, attr_names = colnames(dx),
    full_fit = list(mu = full$mu_all, Sigma = full$Sigma,
                    A = full$A, kappa = full$kappa),
    analysis_signature = signature
  )
  out <- list(
    schema_version = "sw2022-fixed-sensitivity-fit-v1", role = role,
    full = full, full_fit = full_fit, folds = fold_fits,
    assembled = assembled, task = task,
    heldout_tuning_is_training_only = heldout_tuning_is_training_only,
    optimization_gate_by_fold = opt_gate,
    primary_selection_gate_by_fold = primary_selection_gate,
    heldout_assessment_eligible = eligible,
    formal_inference_available = FALSE,
    maintained_model = FALSE,
    analysis_signature = signature,
    posterior_summaries_used = FALSE
  )
  class(out) <- c("sw2022_sensitivity_fit", "list")
  out
}

.score_comparison <- function(primary_assembled, alternative_assembled,
                              task, model_names) {
  pp <- scmix_heldout_predictions(primary_assembled, task_order = task,
                                  include_counts = TRUE,
                                  include_adjacent = TRUE,
                                  include_repeated = TRUE)
  ap <- scmix_heldout_predictions(alternative_assembled, task_order = task,
                                  include_counts = TRUE,
                                  include_adjacent = TRUE,
                                  include_repeated = TRUE)
  ids <- intersect(pp$respondent_id, ap$respondent_id)
  pidx <- match(ids, pp$respondent_id); aidx <- match(ids, ap$respondent_id)
  ll <- cbind(pp$sequence_loglik[pidx], ap$sequence_loglik[aidx])
  colnames(ll) <- model_names
  score <- scmix_heldout_sequence_score(
    ll, respondent_id = ids, out_of_fold = TRUE,
    training_only_tuning = isTRUE(pp$training_only_tuning) &&
      isTRUE(ap$training_only_tuning),
    provenance = paste(
      "paired complete-sequence predictions from primary and application",
      "sensitivity outer fits; common held-out respondents"
    )
  )
  list(score = score, primary_predictions = pp,
       alternative_predictions = ap)
}

.choice_table <- function(view, contests, prefix = "") {
  value <- vapply(names(contests), function(nm) {
    scmix_paper_choice(view, contrast = contests[[nm]],
                       position_neutral = TRUE, n_nodes = 45L,
                       on_support = NA)$estimate
  }, numeric(1L))
  data.frame(quantity = paste0(prefix, names(value)), estimate = value,
             support = "conditional on advertised support; protocol unverified",
             stringsAsFactors = FALSE)
}

.fit_z19 <- function(prepared, primary_full, primary_nested,
                     primary_assembled, controls, config, seed) {
  fit <- .fit_fixed_sensitivity(
    dx = prepared$deltaX, y = prepared$y,
    Z_raw = prepared$Z_sensitivity19_raw,
    rid = prepared$respondent_id, task = prepared$task,
    primary_full = primary_full, primary_nested = primary_nested,
    controls = controls, seed = seed, role = "postconjoint_19Z",
    outer_fold_id = primary_nested$outer_fold_id,
    heldout_tuning_is_training_only = TRUE
  )
  paired <- .score_comparison(primary_assembled, fit$assembled,
                              prepared$task, c("primary_15Z", "postconjoint_19Z"))
  primary_view <- list(
    respondent_id = prepared$respondent_id, Z = prepared$Z_primary,
    attr_names = colnames(prepared$deltaX),
    full_fit = list(mu = primary_full$refit$mu,
                    Sigma = primary_full$refit$Sigma,
                    A = primary_full$refit$A,
                    kappa = primary_full$refit$kappa),
    analysis_signature = primary_full$analysis_signature
  )
  theta0 <- scmix_paper_theta(primary_view)$estimate
  theta1 <- scmix_paper_theta(fit$full_fit)$estimate
  theta <- data.frame(
    coordinate = names(theta0), primary_15Z = as.numeric(theta0),
    postconjoint_19Z = as.numeric(theta1[names(theta0)]),
    difference = as.numeric(theta1[names(theta0)] - theta0),
    stringsAsFactors = FALSE
  )
  c0 <- .choice_table(primary_view, config$qoi$contests)
  c1 <- .choice_table(fit$full_fit, config$qoi$contests)
  choices <- merge(c0[, c("quantity", "estimate")],
                   c1[, c("quantity", "estimate")], by = "quantity",
                   suffixes = c("_primary_15Z", "_postconjoint_19Z"))
  choices$difference <- choices$estimate_postconjoint_19Z -
    choices$estimate_primary_15Z
  list(
    schema_version = "sw2022-z19-sensitivity-v1", fit = fit,
    score_comparison = paired$score, predictions = paired,
    theta_comparison = theta, choice_comparison = choices,
    missing_task_rows = colSums(is.na(prepared$Z_sensitivity19_raw)),
    imputation_verified_training_only = all(vapply(
      fit$folds, function(x) identical(x$preprocessing$imputation$method,
                                       "training-respondent column median"),
      logical(1L))),
    interpretation = paste(
      "Post-conjoint fields remain excluded from the primary model. This is",
      "a timing-sensitive moderator perturbation with no formal intervals or",
      "preapproved materiality threshold."
    ),
    identification_established = FALSE,
    formal_inference_available = FALSE,
    materially_sensitive = NA
  )
}

.interaction_design_audit <- function(prepared, design_audit) {
  X <- as.matrix(design_audit$design$profile_X)
  X14 <- cbind(X, male_x_prior_run = X[, 1L] * X[, 2L])
  pieces <- lapply(seq_len(nrow(X14)), function(i)
    sweep(X14, 2L, X14[i, ], `-`))
  D <- unique(do.call(rbind, pieces))
  p <- ncol(D)
  lower <- lower.tri(matrix(0, p, p), diag = TRUE)
  V <- t(vapply(seq_len(nrow(D)), function(i)
    tcrossprod(D[i, ])[lower], numeric(p * (p + 1L) / 2L)))
  dx14 <- cbind(prepared$deltaX,
                male_x_prior_run = prepared$Xa[, 1L] * prepared$Xa[, 2L] -
                  prepared$Xb[, 1L] * prepared$Xb[, 2L])
  Dr <- unique(dx14)
  Vr <- t(vapply(seq_len(nrow(Dr)), function(i)
    tcrossprod(Dr[i, ])[lower], numeric(p * (p + 1L) / 2L)))
  data.frame(
    support = c("theoretical full-profile", "realized contrasts"),
    distinct_contrasts = c(nrow(D), nrow(Dr)),
    affine_rank = c(.matrix_rank(cbind(1, D)),
                    .matrix_rank(cbind(1, Dr))),
    affine_required = p + 1L,
    covariance_vech_rank = c(.matrix_rank(V), .matrix_rank(Vr)),
    covariance_vech_required = p * (p + 1L) / 2L,
    protocol_verified = FALSE,
    interpretation = c(
      paste("Algebra under unrestricted full-profile support; the fielded",
            "protocol and repeated-event probabilities are unavailable."),
      paste("Realized rank is a precision diagnostic and cannot replace",
            "protocol support for the interaction-augmented model.")
    ), stringsAsFactors = FALSE
  )
}

.fit_interaction <- function(prepared, design_audit, primary_full,
                             primary_nested, primary_assembled, controls,
                             config, seed) {
  int_dx <- prepared$Xa[, 1L] * prepared$Xa[, 2L] -
    prepared$Xb[, 1L] * prepared$Xb[, 2L]
  dx <- cbind(prepared$deltaX, male_x_prior_run = int_dx)
  fit <- .fit_fixed_sensitivity(
    dx = dx, y = prepared$y, Z_raw = prepared$Z_primary,
    rid = prepared$respondent_id, task = prepared$task,
    primary_full = primary_full, primary_nested = primary_nested,
    controls = controls, seed = seed, role = "male_x_prior_run",
    outer_fold_id = primary_nested$outer_fold_id,
    heldout_tuning_is_training_only = TRUE
  )
  paired <- .score_comparison(primary_assembled, fit$assembled,
                              prepared$task,
                              c("primary_additive", "male_x_prior_run"))
  theta <- scmix_paper_theta(fit$full_fit)$estimate
  effects <- c(
    male_effect_when_no_prior_run = theta[[1L]],
    male_effect_when_prior_run = theta[[1L]] + theta[[14L]],
    prior_run_effect_for_female = theta[[2L]],
    prior_run_effect_for_male = theta[[2L]] + theta[[14L]],
    male_by_prior_run_difference_in_differences = theta[[14L]]
  )
  effects <- data.frame(quantity = names(effects), estimate = as.numeric(effects),
                        formal_interval = "withheld", stringsAsFactors = FALSE)
  d <- list(
    male_no_run_vs_female_no_run = c(1, 0, rep(0, 12)),
    female_run_vs_female_no_run = c(0, 1, rep(0, 12)),
    male_run_vs_female_no_run = c(1, 1, rep(0, 11), 1)
  )
  choices <- .choice_table(fit$full_fit, d)
  names(choices)[names(choices) == "estimate"] <- "interaction_fit_probability"
  choices$probability_difference_in_differences <- NA_real_
  did <- choices$interaction_fit_probability[choices$quantity ==
    "male_run_vs_female_no_run"] -
    choices$interaction_fit_probability[choices$quantity ==
      "male_no_run_vs_female_no_run"] -
    choices$interaction_fit_probability[choices$quantity ==
      "female_run_vs_female_no_run"] + 0.5
  choices$probability_difference_in_differences[1L] <- did
  list(
    schema_version = "sw2022-male-run-sensitivity-v1", fit = fit,
    score_comparison = paired$score, predictions = paired,
    conditional_effects = effects, choice_probabilities = choices,
    design_audit = .interaction_design_audit(prepared, design_audit),
    feature_definition = paste(
      "[Male_A * PriorRun_A] - [Male_B * PriorRun_B]; all other profile",
      "coordinates retain the frozen reference coding."
    ),
    interpretation = paste(
      "Targeted interaction sensitivity for the original gender-by-ambition",
      "claim. Formal intervals are withheld, and the fielded repeated-support",
      "condition for the augmented basis is not document-verified."
    ),
    identification_established = FALSE,
    formal_inference_available = FALSE
  )
}

.serial_order_tables <- function(predictions, primary_full) {
  d <- predictions$task
  d$residual <- d$observed - d$predicted
  by_task <- do.call(rbind, lapply(sort(unique(d$task_order)), function(t) {
    z <- d[d$task_order == t, , drop = FALSE]
    gap <- z$observed - z$predicted
    data.frame(
      task_order = t, observed = mean(z$observed), predicted = mean(z$predicted),
      gap = mean(gap), respondent_se_gap = stats::sd(gap) / sqrt(nrow(z)),
      marginal_log_score = mean(z$observed * log(pmax(z$predicted, 1e-12)) +
        (1 - z$observed) * log(pmax(1 - z$predicted, 1e-12))),
      n_respondents = length(unique(z$respondent_id)), stringsAsFactors = FALSE
    )
  }))
  d <- d[order(d$respondent_id, d$task_order), , drop = FALSE]
  parts <- split(seq_len(nrow(d)), d$respondent_id)
  pairs <- do.call(rbind, lapply(parts, function(ii) {
    if (length(ii) < 2L) return(NULL)
    data.frame(
      respondent_id = d$respondent_id[ii[-1L]],
      task_order = d$task_order[ii[-1L]],
      lag_observed = d$observed[ii[-length(ii)]],
      observed = d$observed[ii[-1L]],
      lag_residual = d$residual[ii[-length(ii)]],
      residual = d$residual[ii[-1L]], stringsAsFactors = FALSE
    )
  }))
  serial_fit <- .cluster_lm(
    pairs$residual, cbind(intercept = 1, lag_residual = pairs$lag_residual),
    pairs$respondent_id
  )
  order_fit <- .cluster_lm(
    d$residual, cbind(intercept = 1, task_order = d$task_order),
    d$respondent_id
  )
  serial <- data.frame(
    diagnostic = c("adjacent residual correlation", "adjacent residual slope",
                   "task-order residual slope"),
    estimate = c(stats::cor(pairs$lag_residual, pairs$residual),
                 serial_fit$coef[[2L]], order_fit$coef[[2L]]),
    respondent_cluster_se = c(NA_real_, sqrt(serial_fit$vcov[2L, 2L]),
                              sqrt(order_fit$vcov[2L, 2L])),
    n_rows = c(nrow(pairs), nrow(pairs), nrow(d)),
    interpretation = paste(
      "Held-out residual diagnostic; it can reveal dependence or order lack",
      "of fit but cannot verify independent shocks or absence of learning."
    ), stringsAsFactors = FALSE
  )
  j <- predictions$joint
  transition <- NULL
  if (is.data.frame(j) && nrow(j)) {
    j <- j[j$type == "adjacent_pair", , drop = FALSE]
    if (nrow(j)) {
      key <- interaction(j$task_1, j$task_2, j$stratum, drop = TRUE,
                         lex.order = TRUE)
      transition <- do.call(rbind, lapply(split(seq_len(nrow(j)), key),
                                          function(ii) {
        gap <- j$observed[ii] - j$predicted[ii]
        data.frame(
          task_1 = j$task_1[ii[1L]], task_2 = j$task_2[ii[1L]],
          response_pattern = j$stratum[ii[1L]],
          observed = mean(j$observed[ii]), predicted = mean(j$predicted[ii]),
          gap = mean(gap), respondent_se_gap = stats::sd(gap) / sqrt(length(ii)),
          n_respondents = length(ii), stringsAsFactors = FALSE
        )
      }))
      rownames(transition) <- NULL
    }
  }
  position <- rbind(
    data.frame(
      diagnostic = c("candidate-A observed choice rate",
                     "candidate-A held-out predicted rate",
                     "candidate-A calibration gap", "full-fit kappa"),
      task_order = NA_real_,
      estimate = c(mean(d$observed), mean(d$predicted), mean(d$residual),
                   primary_full$refit$kappa),
      stringsAsFactors = FALSE
    ),
    data.frame(
      diagnostic = "candidate-A held-out calibration gap by task",
      task_order = by_task$task_order, estimate = by_task$gap,
      stringsAsFactors = FALSE
    )
  )
  list(by_task = by_task, serial = serial, transition = transition,
       position = position)
}

.fit_process <- function(prepared, primary_full, primary_assembled,
                         controls, seed) {
  predictions <- scmix_heldout_predictions(
    primary_assembled, task_order = prepared$task, include_counts = TRUE,
    include_adjacent = TRUE, include_repeated = TRUE
  )
  tables <- .serial_order_tables(predictions, primary_full)
  swap <- .fit_one_fixed(
    dx = -as.matrix(prepared$deltaX), y = 1 - prepared$y,
    Z_raw = prepared$Z_primary, rid = prepared$respondent_id,
    train = rep(TRUE, length(prepared$y)), spec = primary_full$selected,
    grid = primary_full$refit$integration_grid, controls = controls,
    seed = seed, role = "profile_AB_swap_full"
  )
  theta_primary <- colMeans(primary_full$refit$mu[
    !duplicated(prepared$respondent_id), , drop = FALSE])
  theta_swap <- colMeans(swap$mu_all[
    !duplicated(prepared$respondent_id), , drop = FALSE])
  swap_summary <- data.frame(
    diagnostic = c("kappa sign reversal gap", "maximum theta gap",
                   "Sigma Frobenius gap", "swap optimization gate"),
    value = c(swap$kappa + primary_full$refit$kappa,
              max(abs(theta_swap - theta_primary)),
              sqrt(sum((swap$Sigma - primary_full$refit$Sigma)^2)),
              as.numeric(isTRUE(swap$optimization$optimization_gate_pass))),
    expected_under_exact_equivariance = c(0, 0, 0, 1),
    interpretation = c(
      rep("Full-sample A/B relabeling optimization replication; no formal test.", 3L),
      "One indicates the attained-state optimization and bound gate passed."
    ), stringsAsFactors = FALSE
  )
  list(
    schema_version = "sw2022-task-process-sensitivity-v1",
    heldout_predictions = predictions, tables = tables,
    profile_swap_fit = swap, profile_swap_summary = swap_summary,
    task_process_alternative_refit = "not_run",
    serial_shock_alternative_refit = "not_run",
    interpretation = paste(
      "The residual, transition, and task-order checks are held out. They do",
      "not establish independent shocks. The A/B swap is an optimization and",
      "position-equivariance check, not a substitute for a task-process model."
    )
  )
}

.raw_expanded_matrices <- function(prepared) {
  raw_path <- prepared$provenance$raw_file
  if (!file.exists(raw_path)) stop("Raw read-only replication file is missing: ",
                                   raw_path, call. = FALSE)
  d <- read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
  lev <- prepared$factor_levels
  d$cand_gender <- factor(ifelse(d$candidate_gender == 1, "Female", "Male"),
                          levels = lev$cand_gender)
  d$cand_run <- factor(ifelse(d$candidate_run == 1, "Yes", "No"),
                       levels = lev$cand_run)
  d$cand_talent <- factor(d$Talent, levels = lev$cand_talent)
  d$cand_agenda <- factor(d$Agenda, levels = lev$cand_agenda)
  d$cand_child <- factor(d$Children, levels = lev$cand_child)
  X <- model.matrix(
    ~ cand_gender + cand_run + cand_talent + cand_agenda + cand_child,
    data = d
  )[, -1L, drop = FALSE]
  colnames(X) <- make.names(colnames(X))
  expected <- colnames(prepared$deltaX)
  if (!identical(colnames(X), expected) || anyNA(X)) {
    stop("Raw expanded profiles do not match the frozen 13-coordinate basis.",
         call. = FALSE)
  }
  d <- cbind(d, X)
  a <- d[d$variable == "candidateA", , drop = FALSE]
  b <- d[d$variable == "candidateB", , drop = FALSE]
  a <- a[order(a$ResponseId, a$election, method = "radix"), , drop = FALSE]
  b <- b[order(b$ResponseId, b$election, method = "radix"), , drop = FALSE]
  if (!identical(a$ResponseId, b$ResponseId) ||
      !identical(a$election, b$election) ||
      any(a$candidate_vote + b$candidate_vote != 1L)) {
    stop("Expanded A/B task pairs are malformed.", call. = FALSE)
  }
  dx <- unname(as.matrix(a[, expected, drop = FALSE]) -
                 as.matrix(b[, expected, drop = FALSE]))
  colnames(dx) <- expected
  rid <- as.character(a$ResponseId); task <- as.integer(a$election)
  counts <- table(rid)
  keep_id <- names(counts)[counts %in% c(2L, 3L)]
  keep <- rid %in% keep_id
  list(
    deltaX = dx[keep, , drop = FALSE], y = as.numeric(a$candidate_vote[keep]),
    respondent_id = rid[keep], task = task[keep],
    source_md5 = unname(tools::md5sum(raw_path)), source_path = raw_path,
    source_policy = "read-only"
  )
}

.amce_raw <- function(dx, y, rid, label) {
  fit <- .cluster_lm(y, cbind(intercept = 1, dx), rid)
  b <- fit$coef[-1L]; se <- sqrt(diag(fit$vcov)[-1L])
  data.frame(sample = label, coordinate = colnames(dx), estimate = b, se = se,
             n_tasks = nrow(dx), n_respondents = length(unique(rid)),
             estimand = "respondent-clustered difference-coded LPM coefficient",
             stringsAsFactors = FALSE)
}

.fit_completion <- function(prepared, design_audit, primary_full, controls,
                            config, seed) {
  expanded <- .raw_expanded_matrices(prepared)
  primary_keep <- expanded$respondent_id %in% unique(prepared$respondent_id)
  if (sum(primary_keep) != nrow(prepared$deltaX) ||
      length(unique(expanded$respondent_id)) != 1249L) {
    stop("Expanded completion sample does not reproduce 1,249 respondents.",
         call. = FALSE)
  }
  Z0 <- matrix(0, nrow(expanded$deltaX), 1L,
               dimnames = list(NULL, "intercept_only"))
  fit_primary <- .fit_one_fixed(
    expanded$deltaX, expanded$y, Z0, expanded$respondent_id,
    train = primary_keep, spec = primary_full$selected,
    grid = primary_full$refit$integration_grid, controls = controls,
    seed = seed, role = "completion_primary1191_noZ"
  )
  fit_expanded <- .fit_one_fixed(
    expanded$deltaX, expanded$y, Z0, expanded$respondent_id,
    train = rep(TRUE, nrow(expanded$deltaX)), spec = primary_full$selected,
    grid = primary_full$refit$integration_grid, controls = controls,
    seed = seed + 10000L, role = "completion_expanded1249_noZ"
  )
  make_view <- function(f, rows) list(
    respondent_id = expanded$respondent_id[rows], Z = Z0[rows, , drop = FALSE],
    attr_names = colnames(expanded$deltaX),
    full_fit = list(mu = f$mu_all[rows, , drop = FALSE], Sigma = f$Sigma,
                    A = f$A, kappa = f$kappa)
  )
  vp <- make_view(fit_primary, primary_keep)
  ve <- make_view(fit_expanded, rep(TRUE, nrow(expanded$deltaX)))
  theta_p <- scmix_paper_theta(vp)$estimate
  theta_e <- scmix_paper_theta(ve)$estimate
  theta <- data.frame(
    coordinate = names(theta_p), primary_1191 = as.numeric(theta_p),
    expanded_1249 = as.numeric(theta_e),
    difference = as.numeric(theta_e - theta_p), stringsAsFactors = FALSE
  )
  cp <- .choice_table(vp, config$qoi$contests)
  ce <- .choice_table(ve, config$qoi$contests)
  choices <- merge(cp[, c("quantity", "estimate")],
                   ce[, c("quantity", "estimate")], by = "quantity",
                   suffixes = c("_primary1191", "_expanded1249"))
  choices$difference <- choices$estimate_expanded1249 -
    choices$estimate_primary1191
  amce <- rbind(
    .amce_raw(expanded$deltaX[primary_keep, , drop = FALSE],
              expanded$y[primary_keep], expanded$respondent_id[primary_keep],
              "primary_1191"),
    .amce_raw(expanded$deltaX, expanded$y, expanded$respondent_id,
              "expanded_1249")
  )
  status <- design_audit$completion$status
  task1 <- expanded$task == 1L
  idx <- match(expanded$respondent_id[task1], status$respondent_id)
  group <- ifelse(status$tasks[idx] == 2L, "two_tasks_unfinished",
    ifelse(status$final_analysis_sample[idx], "primary_complete_case",
      ifelse(status$finished[idx], "three_tasks_finished_excluded",
             "three_tasks_unfinished")))
  early <- do.call(rbind, lapply(unique(group), function(g) {
    take <- group == g
    data.frame(
      eventual_completion_group = g,
      task1_candidate_A_choice_rate = mean(expanded$y[task1][take]),
      se = stats::sd(expanded$y[task1][take]) / sqrt(sum(take)),
      respondents = sum(take), stringsAsFactors = FALSE
      )
  }))
  ## Every respondent completed tasks 1 and 2. Compare those early randomized
  ## contrasts and responses by whether task 3 was ultimately observed. This
  ## is descriptive because only seven respondents stopped after task 2 and
  ## completion can still depend on latent preferences or unobserved shocks.
  status_all <- status[match(expanded$respondent_id, status$respondent_id), ]
  early_features <- cbind(choice_A = expanded$y, expanded$deltaX)
  early_balance <- do.call(rbind, lapply(c(1L, 2L), function(t) {
    rows <- expanded$task == t
    do.call(rbind, lapply(seq_len(ncol(early_features)), function(j) {
      value <- early_features[rows, j]
      completed <- status_all$tasks[rows]
      x2 <- value[completed == 2L]; x3 <- value[completed == 3L]
      data.frame(
        task = t, feature = colnames(early_features)[j],
        mean_eventual_T2 = mean(x2), mean_eventual_T3 = mean(x3),
        difference_T2_minus_T3 = mean(x2) - mean(x3),
        descriptive_se = sqrt(stats::var(x2) / length(x2) +
                                stats::var(x3) / length(x3)),
        respondents_T2 = length(x2), respondents_T3 = length(x3),
        interpretation = paste(
          "Descriptive early-task assignment/response balance by eventual",
          "completion; no test of noninformative completion."
        ), stringsAsFactors = FALSE
      )
    }))
  }))
  list(
    schema_version = "sw2022-completion-sensitivity-v1",
    primary_noZ_fit = fit_primary, expanded_noZ_fit = fit_expanded,
    theta_comparison = theta, choice_comparison = choices,
    amce_comparison = amce, early_task_by_eventual_completion = early,
    early_assignment_response_balance = early_balance,
    sample = data.frame(
      sample = c("primary", "expanded"),
      respondents = c(length(unique(expanded$respondent_id[primary_keep])),
                      length(unique(expanded$respondent_id))),
      tasks = c(sum(primary_keep), nrow(expanded$deltaX)),
      stringsAsFactors = FALSE
    ),
    source_path = expanded$source_path, source_md5 = expanded$source_md5,
    source_policy = expanded$source_policy,
    optimizer_gate = c(
      primary = isTRUE(fit_primary$optimization$optimization_gate_pass),
      expanded = isTRUE(fit_expanded$optimization$optimization_gate_pass)
    ),
    interpretation = paste(
      "The structural comparison holds the no-moderator specification fixed",
      "across samples. It can reveal observed sample sensitivity but cannot",
      "verify completion independence from latent preferences or shocks."
    ),
    formal_inference_available = FALSE
  )
}

.export_component <- function(name, x, table_dir) {
  if (identical(name, "z19")) {
    .write_csv(x$theta_comparison, file.path(table_dir, "z19_theta.csv"))
    .write_csv(x$choice_comparison, file.path(table_dir, "z19_choices.csv"))
    .write_csv(x$score_comparison$paired_differences,
               file.path(table_dir, "z19_heldout_score_difference.csv"))
  } else if (identical(name, "interaction")) {
    .write_csv(x$conditional_effects,
               file.path(table_dir, "male_run_conditional_effects.csv"))
    .write_csv(x$choice_probabilities,
               file.path(table_dir, "male_run_choice_probabilities.csv"))
    .write_csv(x$design_audit,
               file.path(table_dir, "male_run_design_audit.csv"))
    .write_csv(x$score_comparison$paired_differences,
               file.path(table_dir, "male_run_heldout_score_difference.csv"))
  } else if (identical(name, "process")) {
    .write_csv(x$tables$by_task,
               file.path(table_dir, "task_order_calibration.csv"))
    .write_csv(x$tables$serial,
               file.path(table_dir, "serial_residual_diagnostics.csv"))
    .write_csv(x$tables$transition,
               file.path(table_dir, "adjacent_transition_calibration.csv"))
    .write_csv(x$tables$position,
               file.path(table_dir, "position_diagnostics.csv"))
    .write_csv(x$profile_swap_summary,
               file.path(table_dir, "position_profile_swap.csv"))
  } else if (identical(name, "completion")) {
    .write_csv(x$sample, file.path(table_dir, "completion_sample.csv"))
    .write_csv(x$theta_comparison,
               file.path(table_dir, "completion_theta.csv"))
    .write_csv(x$choice_comparison,
               file.path(table_dir, "completion_choices.csv"))
    .write_csv(x$amce_comparison,
               file.path(table_dir, "completion_amce.csv"))
    .write_csv(x$early_task_by_eventual_completion,
               file.path(table_dir, "completion_early_task.csv"))
    .write_csv(x$early_assignment_response_balance,
               file.path(table_dir,
                         "completion_early_assignment_response_balance.csv"))
  }
}

.build_status <- function(z19, interaction, process, completion,
                          structural) {
  row <- function(component, status, note) data.frame(
    component = component, status = status, note = note,
    maintained_assumption_verified = FALSE, formal_inference = FALSE,
    stringsAsFactors = FALSE
  )
  rbind(
    row("primary 15-Z vs post-conjoint 19-Z",
        if (is.null(z19)) "not_run" else "run_descriptive_sensitivity",
        "Training-fold median imputation; post-conjoint variables remain nonprimary."),
    row("Male x prior-run interaction",
        if (is.null(interaction)) "not_run" else "run_descriptive_sensitivity",
        "Targeted augmented-basis refit; fielded repeated support unverified."),
    row("task-order/fatigue/learning diagnostics",
        if (is.null(process)) "not_run" else "run_heldout_diagnostic",
        "Task-order structural alternative refit remains not_run."),
    row("serial dependence diagnostics",
        if (is.null(process)) "not_run" else "run_heldout_diagnostic",
        "Explicit serial-shock likelihood remains not_run."),
    row("position/profile swap",
        if (is.null(process)) "not_run" else
          if (isTRUE(process$profile_swap_fit$optimization$optimization_gate_pass))
            "run_optimization_replication" else "run_failed_optimization_gate",
        "A/B relabeling refit; not a test of the maintained position model."),
    row("completion/sample sensitivity",
        if (is.null(completion)) "not_run" else
          if (all(completion$optimizer_gate)) "run_descriptive_sensitivity" else
            "run_failed_optimization_gate",
        "Primary versus expanded no-Z refits plus early-task diagnostics."),
    row("skewed residual refit/simulation", "not_run",
        "Alternative standardized residual-family code is not implemented."),
    row("bimodal residual refit/simulation", "not_run",
        "Alternative standardized residual-family code is not implemented."),
    row("heavy-tailed residual refit/simulation", "not_run",
        "Alternative standardized residual-family code is not implemented."),
    row("covariance varying with Z/party", "not_run",
        "The current likelihood has common Sigma; no alternative refit was run."),
    row("explicit serial-shock model", "not_run",
        "Residual diagnostics do not implement a serial transitory component."),
    row("task-process interaction model", "not_run",
        "Held-out order diagnostics do not implement task-varying coefficients."),
    row("random/heteroskedastic scale", "not_run",
        "The maintained fixed-logit-scale likelihood was not relaxed."),
    row("structural sensitivity protocol completeness",
        if (isTRUE(structural$complete)) "complete" else "incomplete_fail_closed",
        "No quantitative materiality margins were preregistered; no substantive pass is issued.")
  )
}

.main <- function() {
  cli <- .parse_cli(commandArgs(trailingOnly = TRUE))
  if (!cli$stage %in% c("all", "z19", "interaction", "process", "completion")) {
    stop("--stage must be all, z19, interaction, process, or completion.",
         call. = FALSE)
  }
  script <- .script_file()
  root <- normalizePath(file.path(dirname(script), "../../.."), mustWork = TRUE)
  app <- file.path(root, "applications", "sw2022")
  options(sconjoint.sw_application_root = app)
  source(file.path(app, "config", "analysis_config.R"), local = FALSE)
  source(file.path(dirname(script), "sensitivity_config.R"), local = FALSE)
  if (!cli$profile %in% names(sw_analysis_config$profiles)) {
    stop("Unknown profile: ", cli$profile, call. = FALSE)
  }
  profile <- sw_analysis_config$profiles[[cli$profile]]
  fit_dir <- file.path(sw_analysis_config$output_root, cli$profile)
  out_dir <- file.path(fit_dir, "sensitivity_analysis")
  table_dir <- file.path(out_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- list(
    prepared = sw_analysis_config$input$prepared,
    design = file.path(app, "results", "design_completion_audit.rds"),
    primary_full = file.path(fit_dir, "fit_primary_full.rds"),
    primary_nested = file.path(fit_dir, "fit_primary_nested.rds"),
    primary_assembled = file.path(fit_dir, "fit_primary_assembled.rds")
  )
  missing <- names(paths)[!file.exists(unlist(paths))]
  if (length(missing)) stop("Required input artifact(s) missing: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The project-local library must contain pkgload.", call. = FALSE)
  }
  suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
  ## The runner deliberately uses the same low-level fitter and transforms as
  ## the maintained estimator. Bind the development namespace explicitly so a
  ## stale installed package cannot silently supply a different implementation.
  internal_names <- c(
    ".sc_fit_z_transform", ".sc_comp_fit_dx_scale",
    ".sc_apply_z_transform", ".sc_train_mixed_multistart",
    ".sc_comp_fit_summary", ".sc_predict_beta", ".sc_analysis_signature",
    ".sc_make_folds", "scmix_heldout_predictions",
    "scmix_heldout_sequence_score", "scmix_paper_choice",
    "scmix_paper_theta", "scmix_structural_sensitivity"
  )
  ns <- asNamespace("sconjoint")
  for (nm in internal_names) {
    assign(nm, get(nm, envir = ns, inherits = FALSE), envir = .GlobalEnv)
  }
  prepared <- readRDS(paths$prepared)
  design <- readRDS(paths$design)
  primary_full <- readRDS(paths$primary_full)
  primary_nested <- readRDS(paths$primary_nested)
  primary_assembled <- readRDS(paths$primary_assembled)
  controls <- list(
    outer_K = profile$outer_K, n_epochs = profile$n_epochs,
    learning_rate = profile$learning_rate, n_starts = profile$n_starts,
    opt_tol = profile$opt_tol, grad_tol = profile$grad_tol,
    mu_bound = sw_analysis_config$optimizer$mu_bound,
    kappa_bound = sw_analysis_config$optimizer$kappa_bound,
    a_bound = sw_analysis_config$optimizer$a_bound,
    weight_bound = sw_analysis_config$optimizer$weight_bound,
    device = sw_analysis_config$optimizer$device,
    diagnostic_only = profile$diagnostic_only
  )
  seed <- sw_analysis_config$optimizer$seed + 700000L

  component_paths <- c(
    z19 = file.path(out_dir, "fit_z19_sensitivity.rds"),
    interaction = file.path(out_dir, "fit_male_run_interaction.rds"),
    process = file.path(out_dir, "task_process_diagnostics.rds"),
    completion = file.path(out_dir, "completion_sample_sensitivity.rds")
  )
  requested <- if (cli$stage == "all") names(component_paths) else cli$stage
  stamp_value <- .sensitivity_component_stamp(
    schema_version = sw_sensitivity_config$schema_version,
    config_version = sw_analysis_config$version,
    profile = cli$profile, controls = controls, paths = paths
  )
  stamp_component <- function(x, component) {
    .stamp_sensitivity_component(x, stamp_value, component)
  }
  valid_component <- function(x, component) {
    .valid_sensitivity_component(x, stamp_value, component)
  }
  objects <- stats::setNames(vector("list", length(component_paths)),
                             names(component_paths))
  for (nm in names(component_paths)) {
    if (nm %in% requested) {
      objects[[nm]] <- .run_or_load(component_paths[[nm]], cli$force,
        stamp_component(switch(nm,
          z19 = .fit_z19(prepared, primary_full, primary_nested,
                         primary_assembled, controls, sw_analysis_config,
                         seed + 10000L),
          interaction = .fit_interaction(
            prepared, design, primary_full, primary_nested,
            primary_assembled, controls, sw_analysis_config, seed + 20000L),
          process = .fit_process(prepared, primary_full, primary_assembled,
                                 controls, seed + 30000L),
          completion = .fit_completion(
            prepared, design, primary_full, controls, sw_analysis_config,
            seed + 40000L)
        ), nm),
        validator = function(x) valid_component(x, nm)
      )
    } else if (file.exists(component_paths[[nm]])) {
      objects[[nm]] <- readRDS(component_paths[[nm]])
    }
    if (!is.null(objects[[nm]])) .export_component(nm, objects[[nm]], table_dir)
  }

  q_path <- file.path(fit_dir, "q_sensitivity.rds")
  sensitivity_results <- list()
  if (file.exists(q_path)) {
    sensitivity_results$rank_q_stability <- list(
      status = "run_pass", provenance = q_path,
      result = list(
        summary = readRDS(q_path)$table,
        tolerance_applied = FALSE,
        note = "Executed rank sensitivity; no materiality pass inferred here."
      )
    )
  }
  if (!is.null(objects$process)) {
    sensitivity_results$task_order_fatigue_learning <- list(
      status = "not_run",
      provenance = component_paths[["process"]],
      result = list(diagnostic = objects$process$tables$by_task),
      justification = paste(
        "Held-out diagnostics were run, but the task-varying intercept/",
        "coefficient alternative specified in Section 4 was not fit."
      )
    )
    sensitivity_results$serial_shocks <- list(
      status = "not_run", provenance = component_paths[["process"]],
      result = list(diagnostic = objects$process$tables$serial),
      justification = paste(
        "Held-out residual and transition diagnostics were run, but no",
        "serial-transitory-shock likelihood was implemented."
      )
    )
    sensitivity_results$position <- list(
      status = if (isTRUE(objects$process$profile_swap_fit$optimization$
        optimization_gate_pass)) "run_pass" else "run_fail",
      provenance = component_paths[["process"]],
      result = list(
        summary = objects$process$profile_swap_summary,
        tolerance_applied = FALSE,
        materiality_value = max(abs(objects$process$profile_swap_summary$value[1:3])),
        passed = NA
      )
    )
  }
  if (!is.null(objects$completion)) {
    sensitivity_results$completion <- list(
      status = if (all(objects$completion$optimizer_gate)) "run_pass" else
        "run_fail",
      provenance = component_paths[["completion"]],
      result = list(
        summary = objects$completion$choice_comparison,
        tolerance_applied = FALSE,
        materiality_value = max(abs(objects$completion$choice_comparison$difference)),
        passed = NA
      )
    )
  }
  structural <- scmix_structural_sensitivity(
    results = sensitivity_results,
    q_values = c(sw_analysis_config$primary$q,
                 sw_analysis_config$primary$alternative_q),
    materiality_tolerances = NULL,
    prespecified = FALSE
  )
  structural$application_specific <- list(
    z19 = if (is.null(objects$z19)) "not_run" else
      "run_descriptive_sensitivity",
    male_run_interaction = if (is.null(objects$interaction)) "not_run" else
      "run_descriptive_sensitivity",
    materiality_thresholds_approved = FALSE,
    formal_inference_available = FALSE
  )
  structural$application_config <- sw_sensitivity_config
  .atomic_save(structural, file.path(out_dir, "structural_sensitivity.rds"))
  .write_csv(structural$status,
             file.path(table_dir, "structural_sensitivity_protocol.csv"))
  status <- .build_status(objects$z19, objects$interaction, objects$process,
                          objects$completion, structural)
  .write_csv(status, file.path(table_dir, "application_sensitivity_status.csv"))

  capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))
  artifact_paths <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  artifact_paths <- artifact_paths[file.info(artifact_paths)$isdir %in% FALSE]
  ## The manifest cannot hash itself. Validation artifacts are generated only
  ## after this manifest and therefore have their own validator provenance;
  ## excluding stale copies here prevents a later validation rewrite from
  ## invalidating the upstream component manifest.
  artifact_paths <- setdiff(artifact_paths, c(
    file.path(out_dir, "sensitivity_manifest.rds"),
    file.path(out_dir, "sensitivity_validation.rds"),
    file.path(out_dir, "tables", "sensitivity_validation.csv")
  ))
  manifest <- list(
    schema_version = "sw2022-sensitivity-manifest-v1",
    profile = cli$profile, stage = cli$stage,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    primary_artifacts_modified = FALSE,
    input_paths = paths,
    input_md5 = unname(tools::md5sum(unlist(paths))),
    sensitivity_config = sw_sensitivity_config,
    output_root = out_dir,
    component_status = status,
    artifacts = stats::setNames(unname(tools::md5sum(artifact_paths)),
                                sub(paste0("^", out_dir, "/"), "", artifact_paths)),
    formal_inference_available = FALSE,
    maintained_assumptions_verified = FALSE,
    session_info = utils::capture.output(sessionInfo())
  )
  .atomic_save(manifest, file.path(out_dir, "sensitivity_manifest.rds"))
  message("Saha--Weeks sensitivity stage complete: ", out_dir)
  invisible(manifest)
}

if (sys.nframe() == 0L) .main()
