## Shared utilities for the targeted q = 1 application analyses.
##
## These functions use the paper's respondent-sequence DML implementation and
## its typed, rotation-invariant targets. They deliberately preserve diagnostic
## estimates while withholding formal intervals whenever the fit or verification
## gates do not support them.

tq1_atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path, ".")
  invisible(path)
}

tq1_atomic_write_csv <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(object, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) stop("Could not atomically write ", path, ".")
  invisible(path)
}

tq1_git_commit <- function(root) {
  out <- tryCatch(system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"),
                          stdout = TRUE, stderr = FALSE),
                  error = function(e) NA_character_)
  if (!length(out)) NA_character_ else out[[1L]]
}

tq1_fingerprint <- function(root, inputs, settings = list()) {
  inputs <- normalizePath(inputs, mustWork = TRUE)
  list(
    git_commit = tq1_git_commit(root),
    input_md5 = stats::setNames(unname(tools::md5sum(inputs)), inputs),
    settings = settings,
    source_md5 = unname(tools::md5sum(file.path(
      root, "applications/R/targeted_q1_inference.R")))
  )
}

tq1_checkpoint <- function(path, fingerprint, overwrite = FALSE, code) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    old <- readRDS(path)
    if (!identical(attr(old, "mlcodex_fingerprint"), fingerprint)) {
      stop("Checkpoint exists but its code, inputs, or settings differ: ", path,
           ". Use a fresh output directory or --force=true.", call. = FALSE)
    }
    message("checkpoint: loading ", path)
    return(old)
  }
  value <- force(code)
  attr(value, "mlcodex_fingerprint") <- fingerprint
  tq1_atomic_save_rds(value, path)
  message("checkpoint: wrote ", path)
  value
}

tq1_hidden_features <- function(state, Z_raw) {
  family <- state$architecture$mean_family
  if (is.null(family)) family <- "legacy"
  Z_fit <- sconjoint:::.sc_apply_z_transform(
    as.matrix(Z_raw), state$preprocessing$Z)
  if (identical(family, "constant")) {
    return(matrix(numeric(), nrow(Z_fit), 0L))
  }
  if (identical(family, "linear")) return(Z_fit)
  net <- scmix_restore_network(state)
  live_state <- net$state_dict()
  if (!is.list(live_state) || !length(live_state)) {
    stop("Restored network has no tensor state from which to infer dtype.")
  }
  reference_tensor <- live_state[[1L]]
  torch::with_no_grad({
    ## Match the live restored network. Legacy states remain float32 while
    ## protocol-v2.2 states can be evaluated end to end in float64.
    h <- torch::torch_tensor(
      Z_fit, dtype = reference_tensor$dtype,
      device = reference_tensor$device)
    for (j in seq_along(net$hidden)) {
      h <- torch::nnf_relu(net$hidden[[j]](h))
    }
    as.matrix(h$to(dtype = torch::torch_float64()))
  })
}

## Rebuild the fitted-sieve basis from portable network states. The saved
## all-fold predictions are an independent numerical check on restoration.
tq1_rebuild_inference_basis <- function(fit, prediction_tolerance = 5e-6) {
  states <- fit$network_states
  if (!is.list(states) || length(states) != fit$K) {
    stop("The assembled fit lacks one portable network state per fold.")
  }
  rid_task <- as.character(fit$respondent_id)
  first <- !duplicated(rid_task)
  respondents <- rid_task[first]
  Z_raw <- as.matrix(fit$Z)[first, , drop = FALSE]
  mu_basis <- vector("list", fit$K)
  prediction_max_abs_deviation <- numeric(fit$K)
  basis_dimension <- integer(fit$K)
  tangent_dimension <- integer(fit$K)
  p <- length(fit$attr_names)
  for (k in seq_len(fit$K)) {
    pred <- scmix_predict_network(states[[k]], Z_raw,
                                  input = "raw", output = "raw")
    reference <- as.matrix(fit$mu_all_folds[[k]])[first, , drop = FALSE]
    prediction_max_abs_deviation[k] <- max(abs(pred - reference))
    if (!is.finite(prediction_max_abs_deviation[k]) ||
        prediction_max_abs_deviation[k] > prediction_tolerance) {
      stop("Restored fold ", k, " predictions differ from the saved values by ",
           signif(prediction_max_abs_deviation[k], 6), ".", call. = FALSE)
    }
    H <- tq1_hidden_features(states[[k]], Z_raw)
    if (nrow(H) != length(respondents) || any(!is.finite(H))) {
      stop("Invalid restored hidden features in fold ", k, ".")
    }
    if (ncol(H)) colnames(H) <- paste0("feature", seq_len(ncol(H)))
    mu_basis[[k]] <- cbind(intercept = 1, H)
    basis_dimension[k] <- ncol(mu_basis[[k]])
    A_raw <- if (is.list(fit$loading_contract) &&
        identical(fit$loading_contract$contract_id,
                  "scmix-loading-scale-v1")) {
      scmix_loadings(fit, scale = "raw", scope = "fold", fold = k)
    } else {
      as.matrix(fit$A_folds[[k]])
    }
    horizontal <- sconjoint:::.scmix_horizontal_basis(A_raw, 1e-8)
    tangent_dimension[k] <- 1L + basis_dimension[k] * p + ncol(horizontal)
  }
  list(
    mu_basis = mu_basis, respondents = respondents,
    analysis_signature = fit$analysis_signature,
    prediction_max_abs_deviation = prediction_max_abs_deviation,
    prediction_tolerance = prediction_tolerance,
    basis_dimension = basis_dimension,
    tangent_dimension = tangent_dimension,
    source = "portable network states plus saved fold-specific Z transforms"
  )
}

tq1_unit_contrast <- function(attr_names, coordinate, sign = 1) {
  j <- match(coordinate, attr_names)
  if (is.na(j)) stop("Unknown coefficient coordinate: ", coordinate, ".")
  out <- stats::setNames(numeric(length(attr_names)), attr_names)
  out[j] <- sign
  out
}

## Canonical representative of an oriented contrast ray. Threshold shares and
## directional heterogeneity shares are unchanged by positive rescaling, but
## absolute numerical margin checks are not. Using max(abs(c)) = 1 prevents a
## target from passing or failing solely because an equivalent contrast was
## written in different units. The returned scale converts means and standard
## deviations back by `scale` and variance components back by `scale^2`.
tq1_normalize_contrast <- function(contrast) {
  original_names <- names(contrast)
  contrast <- as.numeric(contrast)
  names(contrast) <- original_names
  scale <- max(abs(contrast))
  if (!length(contrast) || is.null(original_names) || any(!is.finite(contrast)) ||
      !is.finite(scale) || scale <= 0) {
    stop("contrast must be a finite, nonzero named numeric vector.")
  }
  list(analysis = contrast / scale, raw_scale = scale,
       normalization = "max_abs_coordinate_equals_one")
}

## Rescale estimand rows after inference on a canonical contrast ray. This is
## used to return dimensional means, variances, standard errors, and endpoints
## to the declared raw contrast units. Dimensionless shares receive row_scale
## one. Scaling is positive, so parameter-space membership is unchanged.
tq1_rescale_summary_rows <- function(x, row_scale) {
  if (!is.data.frame(x) || length(row_scale) != nrow(x) ||
      any(!is.finite(row_scale)) || any(row_scale <= 0)) {
    stop("x must be a data frame with one finite positive scale per row.")
  }
  columns <- intersect(
    c("plugin", "one_step", "adjustment", "diagnostic_se",
      "diagnostic_lower", "diagnostic_upper", "formal_se",
      "formal_lower", "formal_upper"), names(x))
  for (column in columns) x[[column]] <- x[[column]] * row_scale
  x
}

## Convert a one-column typed target into E[g f] and E[g]. The subsequent
## subgroup_ratio transform targets E[f | g = 1] and carries the joint
## primitive covariance through the delta method.
tq1_subgroup_target <- function(base_target, subgroup) {
  force(base_target); force(subgroup)
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    g <- subgroup
    if (!is.null(names(g))) {
      if (anyDuplicated(names(g)) || !setequal(names(g), respondent_id)) {
        stop("Subgroup names must uniquely match respondent_id.")
      }
      g <- g[respondent_id]
    }
    if (length(g) != nrow(mu) || anyNA(g) ||
        any(!g %in% c(0, 1, FALSE, TRUE))) {
      stop("subgroup must be a respondent-level binary indicator.")
    }
    g <- as.numeric(g)
    base <- base_target(mu = mu, kappa = kappa, Sigma = Sigma, Z = Z,
                        respondent_id = respondent_id, fold = fold,
                        attr_names = attr_names)
    value <- as.matrix(base$value)
    if (ncol(value) != 1L) stop("The subgroup wrapper needs one base target.")
    N <- nrow(mu); p <- ncol(mu)
    dm <- array(0, c(N, 2L, p))
    dm[, 1L, ] <- base$d_mu[, 1L, ] * g
    dk <- matrix(0, N, 2L)
    dk[, 1L] <- as.numeric(base$d_kappa[, 1L]) * g
    ans <- list(
      target_type = "rowwise_expectation",
      value = cbind(weighted_target = g * value[, 1L],
                    subgroup_probability = g),
      d_mu = dm, d_kappa = dk,
      labels = c("weighted_target", "subgroup_probability")
    )
    if (!is.null(base$d_Sigma)) {
      ds <- array(0, c(N, 2L, p, p))
      for (i in seq_len(N)) ds[i, 1L, , ] <- base$d_Sigma[i, 1L, , ] * g[i]
      ans$d_Sigma <- ds
    } else {
      ans$sigma_invariant <- isTRUE(base$sigma_invariant)
    }
    ans
  }
  class(callback) <- c("scmix_rowwise_target", "function")
  callback
}

tq1_run_family <- function(fit, basis, targets, inf_cfg, seed) {
  scmix_dml(
    fit = fit, targets = character(), plugin_targets = targets,
    mu_basis = basis, nu_grid = NULL, riesz_penalty = "identity",
    riesz_validation_fraction = inf_cfg$riesz_validation_fraction,
    active_eigenvalue_min = inf_cfg$active_eigenvalue_min,
    information_eigenvalue_min = inf_cfg$information_eigenvalue_min,
    rank_tolerance = inf_cfg$rank_tolerance,
    riesz_equation_tolerance = inf_cfg$riesz_equation_tolerance,
    ridge_sensitivity_tolerance = inf_cfg$ridge_sensitivity_tolerance,
    allow_numeric_derivatives = FALSE, verification = NULL,
    multiplier_draws = as.integer(inf_cfg$multiplier_draws),
    multiplier = inf_cfg$multiplier, level = inf_cfg$level,
    seed = as.integer(seed)
  )
}

tq1_family_summary <- function(dml, family, level = 0.95) {
  if (is.null(dml$estimate) || length(dml$estimate) == 1L &&
      is.na(dml$estimate)) {
    return(data.frame(family = family, target = NA_character_,
      status = dml$status, formal_inference_available = FALSE,
      withholding_reason = dml$reason, stringsAsFactors = FALSE))
  }
  labels <- names(dml$estimate)
  K <- length(dml$fold_details)
  extract <- function(field) {
    out <- do.call(rbind, lapply(dml$fold_details, function(fd) {
      x <- as.numeric(fd[[field]])
      if (length(x) == 1L) rep(x, length(labels)) else x
    }))
    if (!is.matrix(out)) out <- matrix(out, nrow = K, byrow = TRUE)
    if (!identical(dim(out), c(K, length(labels)))) {
      stop("Unexpected fold-diagnostic shape for ", field, ".")
    }
    dimnames(out) <- list(paste0("fold", seq_len(K)), labels)
    out
  }
  riesz <- extract("riesz_equation_relative_residual")
  ridge <- extract("ridge_relative_sensitivity")
  info <- extract("information_structural_min")
  nu <- extract("selected_nu")
  z <- stats::qnorm(1 - (1 - level) / 2)
  dse <- as.numeric(dml$diagnostic_se[labels])
  out <- data.frame(
    family = family, target = labels,
    plugin = as.numeric(dml$plugin_estimate[labels]),
    one_step = as.numeric(dml$estimate[labels]),
    adjustment = as.numeric(dml$one_step_adjustment[labels]),
    diagnostic_se = dse,
    diagnostic_lower = as.numeric(dml$estimate[labels]) - z * dse,
    diagnostic_upper = as.numeric(dml$estimate[labels]) + z * dse,
    formal_se = as.numeric(dml$se[labels]),
    formal_lower = as.numeric(dml$ci_lower[labels]),
    formal_upper = as.numeric(dml$ci_upper[labels]),
    status = dml$status,
    formal_inference_available = as.logical(
      dml$target_inference_available[labels]),
    riesz_max = apply(riesz, 2L, max),
    ridge_max = apply(ridge, 2L, max),
    information_min = apply(info, 2L, min),
    withholding_reason = if (is.null(dml$reason)) "" else dml$reason,
    stringsAsFactors = FALSE
  )
  for (k in seq_len(K)) {
    out[[paste0("fold", k, "_riesz")]] <- riesz[k, ]
    out[[paste0("fold", k, "_ridge")]] <- ridge[k, ]
    out[[paste0("fold", k, "_nu")]] <- nu[k, ]
    out[[paste0("fold", k, "_information_min")]] <- info[k, ]
  }
  out
}

## Apply a named smooth transformation to the cross-fitted plug-in primitive
## vector. scmix_delta_transform() intentionally transforms the one-step
## primitive vector, so application tables need this separate calculation to
## avoid silently presenting a one-step transform as the structural plug-in.
tq1_plugin_transform <- function(
    dml, type = c("subgroup_ratio", "directional_heterogeneity"),
    primitives) {
  type <- match.arg(type)
  if (!inherits(dml, "scmix_dml") || is.null(dml$plugin_estimate) ||
      !all(primitives %in% names(dml$plugin_estimate))) {
    stop("The requested plug-in primitives are not available.")
  }
  est <- as.numeric(dml$plugin_estimate[primitives])
  if (any(!is.finite(est))) stop("Plug-in primitives must be finite.")
  if (identical(type, "subgroup_ratio")) {
    if (length(est) != 2L || est[2L] == 0) {
      return(stats::setNames(NA_real_, "subgroup_ratio"))
    }
    return(stats::setNames(est[1L] / est[2L], "subgroup_ratio"))
  }
  if (length(est) != 3L) {
    stop("Directional heterogeneity requires three plug-in primitives.")
  }
  H_Z <- est[2L] - est[1L]^2
  H_R <- est[3L]
  H_T <- H_Z + H_R
  c(H_Z = H_Z, H_R = H_R, H_T = H_T,
    share_Z = if (H_T != 0) H_Z / H_T else NA_real_)
}

tq1_bind_rows <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(columns, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[, columns, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

tq1_transform_summary <- function(x, family, target, level = 0.95) {
  est <- x$estimate
  z <- stats::qnorm(1 - (1 - level) / 2)
  dse <- as.numeric(x$diagnostic_se[names(est)])
  quantity_gate_pass <- rep(TRUE, length(est))
  if (identical(x$type, "directional_heterogeneity")) {
    quantity_gate_pass[names(est) == "share_Z"] <- isTRUE(x$gate$pass)
  } else if (!identical(x$type, "covariance_decomposition")) {
    quantity_gate_pass[] <- isTRUE(x$gate$pass)
  }
  data.frame(
    family = family, target = target, component = names(est),
    one_step = as.numeric(est), diagnostic_se = dse,
    diagnostic_lower = as.numeric(est) - z * dse,
    diagnostic_upper = as.numeric(est) + z * dse,
    formal_se = as.numeric(x$se[names(est)]),
    formal_lower = as.numeric(x$ci_lower[names(est)]),
    formal_upper = as.numeric(x$ci_upper[names(est)]),
    status = x$status,
    formal_inference_available = as.logical(
      x$target_inference_available[names(est)]),
    quantity_gate_pass = quantity_gate_pass,
    withholding_reason = if (is.null(x$reason)) "" else x$reason,
    stringsAsFactors = FALSE
  )
}

## Protocol-v2.2 application helpers. These are intentionally separate from
## tq1_run_family(), which reproduces the v2.1 application artifacts and lets
## each call create its own multiplier draws. A v2.2 application creates one
## bank before any target is finalized and reuses it for every substantive
## family and every separately tuned primitive object.
tq1_v22_multiplier_bank <- function(fit, inf_cfg, seed,
                                    analysis_contract =
                                      fit$v22_analysis_contract) {
  respondent_id <- unique(as.character(fit$respondent_id))
  ## Do not inherit the legacy v2.1 multiplier_draws field, which is commonly
  ## 1,999. A separate explicitly named v2.2 override remains fail-closed in
  ## scmix_v22_finalize() unless it is exactly the frozen 9,999.
  draws <- if (!is.null(inf_cfg$v22_multiplier_draws)) {
    as.integer(inf_cfg$v22_multiplier_draws)
  } else 9999L
  scmix_v22_multiplier_bank(
    respondent_id = respondent_id, draws = draws,
    seed = as.integer(seed), distribution = "normal",
    analysis_contract = analysis_contract)
}

tq1_v22_finalize_family <- function(
    dml, registry, full_sample_plugin, multiplier_bank,
    analysis_contract = dml$v22_analysis_contract,
    derivative_audit = dml$v22_derivative_audit,
    calibration = NULL, external_gates = NULL,
    complete_external_record = FALSE, level = 0.95) {
  scmix_v22_finalize(
    inference = dml, registry = registry,
    full_sample_plugin = full_sample_plugin,
    analysis_contract = analysis_contract,
    derivative_audit = derivative_audit,
    multiplier_bank = multiplier_bank, calibration = calibration,
    external_gates = external_gates,
    complete_external_record = complete_external_record,
    level = level)
}

tq1_v22_collect_job <- function(
    dml, registry, full_sample_plugin = NULL,
    analysis_contract = dml$v22_analysis_contract,
    derivative_audit = dml$v22_derivative_audit) {
  scmix_v22_collect(
    inference = dml, registry = registry,
    full_sample_plugin = full_sample_plugin,
    analysis_contract = analysis_contract,
    derivative_audit = derivative_audit)
}

tq1_v22_finalize_jobs <- function(
    collections, multiplier_bank, calibration = NULL,
    external_gates = NULL, complete_external_record = FALSE,
    level = 0.95) {
  scmix_v22_finalize_collections(
    collections = collections, multiplier_bank = multiplier_bank,
    calibration = calibration, external_gates = external_gates,
    complete_external_record = complete_external_record,
    level = level)
}

tq1_v22_summary <- function(x) {
  if (!inherits(x, "scmix_v22_inference")) {
    stop("x must be a scmix_v22_inference object.")
  }
  rows <- lapply(x$targets, function(z) {
    interval <- if (z$status$multiplicity_status %in%
                    c("simultaneous_complete", "bonferroni_incomplete")) {
      z$simultaneous_interval
    } else z$pointwise_interval
    data.frame(
      target_id = z$spec$target_id,
      target_type = z$spec$target_type,
      family_id = z$spec$target_family_id,
      full_sample_plugin = z$full_sample_plugin,
      crossfit_plugin = z$crossfit_plugin,
      raw_one_step = z$raw_one_step,
      reported_estimate = z$reported_estimate,
      correction_se_ratio = z$correction_se_ratio,
      chart_discrepancy_se_ratio = z$chart_discrepancy_se_ratio,
      raw_domain_distance = z$raw_domain_distance,
      plugin_boundary_distance = z$plugin_boundary_distance,
      lower = interval[["lower"]], upper = interval[["upper"]],
      calculation_status = z$status$calculation_status,
      fit_status = z$status$fit_status,
      target_status = z$status$target_status,
      release_class = z$status$release_class,
      interval_method = z$status$interval_method,
      multiplicity_status = z$status$multiplicity_status,
      interpretation_status = z$status$interpretation_status,
      hard_failure_codes = paste(z$status$hard_failure_codes, collapse = ";"),
      warning_codes = paste(z$status$warning_codes, collapse = ";"),
      first_failed_gate = z$status$first_failed_gate,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
