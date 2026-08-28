## Shared fitting helpers for v2.1 application sensitivities and simulated-data
## stress tests.  Callers must pass a context returned by the fail-closed
## reported-primary contract.

`%||%` <- function(x, y) if (is.null(x)) y else x

.sw_v21_atomic_save <- function(x, path, portable = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  if (isTRUE(portable) && requireNamespace("sconjoint", quietly = TRUE)) {
    x <- sconjoint::scmix_portable_copy(x)
  }
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write ", path, call. = FALSE)
  }
  invisible(path)
}

.sw_v21_write_csv <- function(x, path) {
  if (is.null(x)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(TRUE)
}

.sw_v21_fit_imputer <- function(Z, rid, train) {
  Z <- as.matrix(Z); rid <- as.character(rid); train <- as.logical(train)
  if (nrow(Z) != length(rid) || length(train) != length(rid) ||
      !any(train)) stop("Malformed moderator imputation inputs.", call. = FALSE)
  first <- which(train)[!duplicated(rid[train])]
  med <- apply(Z[first, , drop = FALSE], 2L, stats::median, na.rm = TRUE)
  if (any(!is.finite(med))) {
    stop("A moderator is entirely missing in a training sample.", call. = FALSE)
  }
  names(med) <- colnames(Z)
  list(median = med, training_respondents = unique(rid[train]),
       method = "training-respondent column median")
}

.sw_v21_apply_imputer <- function(Z, imputer) {
  out <- as.matrix(Z)
  for (j in seq_len(ncol(out))) {
    miss <- is.na(out[, j])
    if (any(miss)) out[miss, j] <- imputer$median[[j]]
  }
  if (any(!is.finite(out))) stop("Imputation did not produce finite Z.",
                                  call. = FALSE)
  out
}

.sw_v21_constant_within_respondent <- function(Z, rid) {
  Z <- as.matrix(Z); rid <- as.character(rid)
  bad <- vapply(split(seq_len(nrow(Z)), rid), function(ii) {
    ref <- Z[ii[1L], ]
    any(vapply(ii, function(j) {
      same <- (is.na(Z[j, ]) & is.na(ref)) |
        (!is.na(Z[j, ]) & !is.na(ref) &
           abs(Z[j, ] - ref) <= 1e-12)
      !all(same)
    }, logical(1L)))
  }, logical(1L))
  if (any(bad)) stop("A moderator varies within respondent.", call. = FALSE)
  invisible(TRUE)
}

.sw_v21_stage_gate <- function(x) {
  if (is.null(x)) return(TRUE)
  isTRUE(sconjoint:::.sc_comp_inner_fit_gate(x)$pass)
}

.sw_v21_refit_gate <- function(fit) {
  opt <- fit$optimization
  main <- .sw_v21_stage_gate(opt)
  pooled <- .sw_v21_stage_gate(fit$pooled_prefit_optimization)
  continued <- .sw_v21_stage_gate(fit$continued_constant_optimization)
  nesting <- is.null(opt$nested_objective_gate) ||
    isTRUE(opt$nested_objective_gate$pass)
  list(pass = all(main, pooled, continued, nesting), main = main,
       pooled_prefit = pooled, continued_constant = continued,
       nested_objective = nesting)
}

.sw_v21_controls <- function(sensitivity_config, profile) {
  p <- sensitivity_config$profiles[[profile]]
  if (is.null(p)) stop("Unknown v2.1 sensitivity profile: ", profile,
                       call. = FALSE)
  list(
    profile = profile, n_epochs = as.integer(p$n_epochs),
    n_starts = as.integer(p$n_starts),
    learning_rate = as.numeric(p$learning_rate %||%
      sw_v21_config$optimizer$learning_rate),
    opt_tol = as.numeric(p$opt_tol %||% sw_v21_config$optimizer$opt_tol),
    grad_tol = as.numeric(p$grad_tol %||% sw_v21_config$optimizer$grad_tol),
    nested_objective_tol = as.numeric(p$nested_objective_tol %||%
      sw_v21_config$optimizer$nested_objective_tol),
    mu_bound = sensitivity_config$bounds$mu,
    kappa_bound = sensitivity_config$bounds$kappa,
    alpha_bound = sensitivity_config$bounds$alpha,
    a_bound = sensitivity_config$bounds$loading,
    weight_bound = sensitivity_config$bounds$deviation_parameter,
    device = sensitivity_config$device,
    outcome_blind = FALSE, formal_inference_available = FALSE)
}

.sw_v21_fit_one_fixed <- function(dx, y, Z_raw, rid, train, specification,
                                  integration_grid, controls, seed, role,
                                  pointer_lock_md5) {
  dx <- as.matrix(dx); y <- as.numeric(y); Z_raw <- as.matrix(Z_raw)
  rid <- as.character(rid); train <- as.logical(train)
  if (!is.numeric(dx) || any(!is.finite(dx)) || nrow(dx) != length(y) ||
      nrow(Z_raw) != nrow(dx) || length(rid) != nrow(dx) ||
      length(train) != nrow(dx) || any(!y %in% c(0, 1)) ||
      length(unique(rid[train])) < 2L ||
      !is.list(specification) || !identical(as.integer(specification$q), 1L) ||
      !is.character(pointer_lock_md5) || !length(pointer_lock_md5)) {
    stop("Malformed fixed v2.1 refit inputs for ", role, ".", call. = FALSE)
  }
  .sw_v21_constant_within_respondent(Z_raw, rid)
  imputer <- .sw_v21_fit_imputer(Z_raw, rid, train)
  Z <- .sw_v21_apply_imputer(Z_raw, imputer)
  fit <- sconjoint::scmix_refit_selected_matrix(
    deltaX = dx[train, , drop = FALSE], y = y[train],
    Z = Z[train, , drop = FALSE], respondent_id = rid[train],
    specification = specification, integration_grid = integration_grid,
    n_epochs = controls$n_epochs,
    learning_rate = controls$learning_rate,
    n_starts = controls$n_starts,
    mu_bound = controls$mu_bound, kappa_bound = controls$kappa_bound,
    alpha_bound = controls$alpha_bound, a_bound = controls$a_bound,
    weight_bound = controls$weight_bound,
    opt_tol = controls$opt_tol, grad_tol = controls$grad_tol,
    nested_objective_tol = controls$nested_objective_tol,
    seed = as.integer(seed), device = controls$device, verbose = FALSE,
    source_analysis_signature = paste0("v2.1-pointer-lock:",
                                       pointer_lock_md5[[1L]]))
  gate <- .sw_v21_refit_gate(fit)
  mu_all <- sconjoint::scmix_predict_network(
    fit$network_state, Z, input = "raw", output = "raw",
    device = controls$device)
  if (!identical(dim(mu_all), dim(dx)) || any(!is.finite(mu_all))) {
    stop("The v2.1 fixed refit did not predict all task rows.", call. = FALSE)
  }
  list(
    schema_version = "sw2022-v2.1-fixed-refit-v1",
    role = role, fit = fit, mu_all = mu_all,
    A = as.matrix(fit$A), Sigma = as.matrix(fit$Sigma),
    kappa = as.numeric(fit$kappa), specification = fit$specification,
    integration_grid = fit$integration_grid,
    preprocessing = c(fit$preprocessing, list(imputation = imputer)),
    optimization = fit$optimization,
    pooled_prefit_optimization = fit$pooled_prefit_optimization,
    continued_constant_optimization = fit$continued_constant_optimization,
    gate = gate, optimization_gate_pass = isTRUE(gate$pass),
    training_rows = which(train),
    training_respondents = unique(rid[train]),
    full_fit = all(train), pointer_lock_md5 = pointer_lock_md5,
    outcome_blind = FALSE, formal_inference_available = FALSE,
    maintained_model = FALSE)
}

.sw_v21_fit_fixed_nested <- function(dx, y, Z_raw, rid, task,
                                     primary_context, controls, seed, role) {
  full0 <- primary_context$full
  nested0 <- primary_context$nested
  assembled0 <- primary_context$assembled
  fold_id <- as.integer(assembled0$fold_id)
  K <- as.integer(assembled0$K)
  if (length(fold_id) != nrow(dx) || K != length(nested0$tuning) ||
      !setequal(unique(fold_id), seq_len(K)) ||
      any(vapply(split(fold_id, as.character(rid)),
                 function(x) length(unique(x)) != 1L, logical(1L)))) {
    stop("The pointer's outer folds do not match this sensitivity sample.",
         call. = FALSE)
  }
  folds <- vector("list", K)
  for (k in seq_len(K)) {
    message(role, ": fitting pointer-locked outer fold ", k, "/", K)
    folds[[k]] <- .sw_v21_fit_one_fixed(
      dx, y, Z_raw, rid, train = fold_id != k,
      specification = nested0$tuning[[k]]$selected,
      integration_grid = nested0$tuning[[k]]$refit$integration_grid,
      controls = controls, seed = seed + 1000L * k,
      role = paste0(role, "_outer_", k),
      pointer_lock_md5 = primary_context$lock_md5)
  }
  message(role, ": fitting pointer-locked full sample")
  full <- .sw_v21_fit_one_fixed(
    dx, y, Z_raw, rid, train = rep(TRUE, nrow(dx)),
    specification = full0$selected,
    integration_grid = full0$refit$integration_grid,
    controls = controls, seed = seed + 90001L,
    role = paste0(role, "_full"),
    pointer_lock_md5 = primary_context$lock_md5)
  fold_gate <- vapply(folds, function(x) isTRUE(x$gate$pass), logical(1L))
  Z_signature <- .sw_v21_apply_imputer(
    Z_raw, .sw_v21_fit_imputer(Z_raw, rid, rep(TRUE, length(rid))))
  signature <- sconjoint:::.sc_analysis_signature(
    deltaX = dx, y = y, Z = Z_signature, respondent_id = rid,
    fold_id = fold_id, specification = list(
      workflow = "sw2022-v2.1-pointer-locked-fixed-sensitivity",
      role = role, pointer_lock_md5 = primary_context$lock_md5,
      profile = controls$profile,
      outcome_blind = FALSE, formal_inference_available = FALSE,
      fold_specifications = lapply(folds, `[[`, "specification")))
  assembled <- list(
    deltaX = dx, y = y, Z = Z_signature, respondent_id = rid,
    fold_id = fold_id, K = K, N = length(unique(rid)), q = 1L,
    mu_hat = do.call(rbind, lapply(seq_len(nrow(dx)), function(j) {
      folds[[fold_id[j]]]$mu_all[j, , drop = FALSE]
    })),
    mu_all_folds = lapply(folds, `[[`, "mu_all"),
    A_folds = lapply(folds, `[[`, "A"),
    A_computational_folds = lapply(folds, `[[`, "A"),
    kappa_folds = vapply(folds, `[[`, numeric(1L), "kappa"),
    integration_grids_folds = lapply(folds, `[[`, "integration_grid"),
    integration_grid = folds[[1L]]$integration_grid,
    gh = folds[[1L]]$integration_grid,
    attr_names = colnames(dx), z_names = colnames(Z_raw),
    selected_specifications = lapply(folds, `[[`, "specification"),
    optimization = list(
      folds = lapply(folds, `[[`, "optimization"),
      gate_by_fold = fold_gate,
      diagnostics_are_certificates = FALSE),
    computational_gate_pass = FALSE,
    eligible_for_ordinary_inference = FALSE, diagnostic_only = TRUE,
    analysis_signature = signature,
    source = "fixed sensitivity specifications inherited from validated pointer",
    scope = paste(
      "Outcome-informed descriptive sensitivity. Training-fold refits and",
      "held-out predictions do not restore outcome-blind selection or formal",
      "inference eligibility."))
  class(assembled) <- c("scmix_nested_assembled", "list")
  view <- list(
    respondent_id = rid, Z = Z_signature, attr_names = colnames(dx),
    full_fit = list(mu = full$mu_all, A = full$A, Sigma = full$Sigma,
                    kappa = full$kappa),
    analysis_signature = signature)
  list(
    schema_version = "sw2022-v2.1-fixed-sensitivity-fit-v1",
    role = role, full = full, folds = folds, assembled = assembled,
    full_fit = view, task = task,
    optimization_gate_by_fold = fold_gate,
    full_optimization_gate = isTRUE(full$gate$pass),
    heldout_assessment_eligible = all(fold_gate),
    pointer_lock_md5 = primary_context$lock_md5,
    outcome_blind = FALSE, formal_inference_available = FALSE,
    maintained_model = FALSE, analysis_signature = signature)
}

.sw_v21_primary_view <- function(context, prepared) {
  list(
    respondent_id = prepared$respondent_id,
    Z = prepared$Z_primary,
    attr_names = colnames(prepared$deltaX),
    full_fit = list(
      mu = context$full$refit$mu,
      A = context$full$refit$A,
      Sigma = context$full$refit$Sigma,
      kappa = context$full$refit$kappa),
    analysis_signature = context$full$analysis_signature)
}

.sw_v21_score_comparison <- function(primary_assembled,
                                     alternative_assembled, task,
                                     model_names) {
  p <- sconjoint::scmix_heldout_predictions(
    primary_assembled, task_order = task, include_counts = TRUE,
    include_adjacent = TRUE, include_repeated = TRUE)
  a <- sconjoint::scmix_heldout_predictions(
    alternative_assembled, task_order = task, include_counts = TRUE,
    include_adjacent = TRUE, include_repeated = TRUE)
  ids <- intersect(p$respondent_id, a$respondent_id)
  ll <- cbind(p$sequence_loglik[match(ids, p$respondent_id)],
              a$sequence_loglik[match(ids, a$respondent_id)])
  colnames(ll) <- model_names
  score <- sconjoint::scmix_heldout_sequence_score(
    ll, respondent_id = ids, out_of_fold = TRUE,
    training_only_tuning = FALSE,
    provenance = paste(
      "Paired complete-sequence predictions from the validated v2.1",
      "reported primary and a pointer-locked, outcome-informed sensitivity."))
  list(score = score, primary_predictions = p,
       alternative_predictions = a)
}
