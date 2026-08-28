## Helpers for the add-only Saha--Weeks v2.1 descriptive penalized-criterion
## profile-sequence runner with unpenalized likelihood overlays. Sourcing this
## file has no side effects.

`%||%` <- function(x, y) if (is.null(x)) y else x

.sw_v21_profile_same_md5 <- function(x, y) {
  is.character(x) && is.character(y) && identical(names(x), names(y)) &&
    identical(unname(x), unname(y))
}

.sw_v21_profile_atomic_save <- function(x, path, portable = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  if (isTRUE(portable)) x <- sconjoint::scmix_portable_copy(x)
  saveRDS(x, tmp, version = 3, compress = "xz")
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write ", path, call. = FALSE)
  }
  invisible(path)
}

.sw_v21_profile_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x, stringsAsFactors = FALSE,
                                 check.names = FALSE),
                   path, row.names = FALSE, na = "")
  invisible(path)
}

.sw_v21_profile_generation_paths <- function(
    root, app, config_path, helper_path, runner_path,
    authorization_creator_path, primary_contract_path,
    parent_config_path, postfit_config_path, context) {
  package_sources <- sort(list.files(
    file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  names(package_sources) <- paste0("package_source:",
                                    basename(package_sources))
  parent_lock <- context$lock_paths
  names(parent_lock) <- paste0("reported_primary_lock:", names(parent_lock))
  paths <- c(
    profile_config = config_path,
    profile_helpers = helper_path,
    profile_runner = runner_path,
    profile_authorization_creator = authorization_creator_path,
    reported_primary_contract = primary_contract_path,
    parent_config = parent_config_path,
    postfit_config = postfit_config_path,
    prepared = file.path(app, "results", "prep_analysis_data.rds"),
    package_description = file.path(root, "DESCRIPTION"),
    package_namespace = file.path(root, "NAMESPACE"),
    launcher_R45 = file.path(root, "applications", "bin", "R45"),
    launcher_Rscript45 = file.path(root, "applications", "bin", "Rscript45"),
    package_sources, parent_lock)
  if (is.null(names(paths)) || any(!nzchar(names(paths))) ||
      anyDuplicated(names(paths)) || any(!file.exists(paths)) ||
      any(dir.exists(paths))) {
    stop("Profile generation paths are missing or not uniquely named.",
         call. = FALSE)
  }
  paths
}

.sw_v21_profile_authorization_valid <- function(
    authorization, config, config_path, generation_md5, runtime_signature,
    context) {
  is.list(authorization) &&
    identical(authorization$schema_version,
              "sw2022-v2.1-profile-sequence-authorization-v2") &&
    identical(authorization$authorized, TRUE) &&
    identical(authorization$purpose,
              "sw2022-v2.1-descriptive-penalized-criterion-profile-sequences") &&
    is.character(authorization$reviewed_by) &&
    length(authorization$reviewed_by) == 1L &&
    !is.na(authorization$reviewed_by) && nzchar(authorization$reviewed_by) &&
    is.character(authorization$authorized_at_utc) &&
    length(authorization$authorized_at_utc) == 1L &&
    !is.na(authorization$authorized_at_utc) &&
    nzchar(authorization$authorized_at_utc) &&
    identical(authorization$config_version, config$version) &&
    identical(as.character(authorization$config_md5),
              unname(tools::md5sum(config_path))) &&
    .sw_v21_profile_same_md5(authorization$generation_input_md5,
                             generation_md5) &&
    identical(authorization$runtime_signature, runtime_signature) &&
    identical(as.character(authorization$reviewed_pointer_md5),
              unname(tools::md5sum(context$pointer_path))) &&
    identical(as.character(authorization$reviewed_manifest_md5),
              unname(tools::md5sum(file.path(
                dirname(context$pointer_path), "manifest.rds")))) &&
    .sw_v21_profile_same_md5(authorization$reported_primary_lock_md5,
                             context$lock_md5) &&
    identical(authorization$reported_primary,
              context$pointer$reported_primary) &&
    identical(authorization$acknowledged_outcome_informed, TRUE) &&
    identical(
      authorization$acknowledged_descriptive_penalized_criterion_sequences,
      TRUE) &&
    identical(authorization$acknowledged_formal_inference_unavailable, TRUE) &&
    identical(authorization$acknowledged_no_lr_critical_values, TRUE) &&
    identical(authorization$acknowledged_fixed_learner_tuning_sieve, TRUE) &&
    identical(authorization$formal_inference_available, FALSE) &&
    identical(authorization$outcome_blind, FALSE)
}

.sw_v21_profile_select_eligible_start <- function(starts) {
  if (!is.list(starts) || !length(starts)) {
    stop("A profile point must contain at least one start.", call. = FALSE)
  }
  well_formed <- vapply(starts, function(x) {
    is.list(x) && is.logical(x$gate_pass) && length(x$gate_pass) == 1L &&
      !is.na(x$gate_pass) && is.numeric(x$penalized_objective) &&
      length(x$penalized_objective) == 1L &&
      is.finite(x$penalized_objective)
  }, logical(1L))
  if (!all(well_formed)) {
    stop("At least one profile start has malformed gate/objective diagnostics.",
         call. = FALSE)
  }
  eligible <- which(vapply(starts, `[[`, logical(1L), "gate_pass"))
  if (!length(eligible)) {
    stop(
      "No optimizer start passed the target, objective, compact-bound, and constrained-gradient gates; this profile point is unavailable and no checkpoint or direction manifest may be emitted.",
      call. = FALSE)
  }
  objective <- vapply(starts[eligible], `[[`, numeric(1L),
                      "penalized_objective")
  chosen <- eligible[[which.max(objective)]]
  list(index = chosen, eligible = eligible, best = starts[[chosen]])
}

.sw_v21_profile_checkpoint_valid <- function(
    x, direction, target, generation_md5, runtime_signature,
    authorization_md5, context, selected_spec, ids, config) {
  near <- function(a, b, tolerance = 1e-6) {
    is.numeric(a) && is.numeric(b) && length(a) == 1L && length(b) == 1L &&
      is.finite(a) && is.finite(b) &&
      abs(a - b) <= tolerance * max(1, abs(a), abs(b))
  }
  scalar <- function(value, nonnegative = FALSE) {
    is.numeric(value) && length(value) == 1L && is.finite(value) &&
      (!isTRUE(nonnegative) || value >= 0)
  }
  if (!is.list(x) || !is.list(x$profile_specification)) return(FALSE)
  s <- x$profile_specification
  grad <- x$projected_gradient
  bounds <- x$bounds
  selected <- x$selected_start
  diagnostics <- x$all_start_diagnostics
  sequence_ok <- is.numeric(x$sequence_loglik) &&
    length(x$sequence_loglik) == length(ids) &&
    identical(names(x$sequence_loglik), ids) &&
    all(is.finite(x$sequence_loglik)) &&
    near(mean(x$sequence_loglik), x$mean_sequence_loglik)
  target_ok <- scalar(x$target_attained) && scalar(x$target_error) &&
    near(x$target_error, x$target_attained - target, tolerance = 1e-8) &&
    abs(x$target_error) <= config$optimizer$target_tol
  objective_ok <- scalar(x$penalized_objective) &&
    scalar(x$penalty, nonnegative = TRUE) &&
    near(x$penalized_objective,
         x$mean_sequence_loglik - x$penalty)
  stability_ok <- scalar(x$last_relative_change, nonnegative = TRUE) &&
    x$last_relative_change <= config$optimizer$opt_tol
  gradient_ok <- is.list(grad) && identical(grad$pass, TRUE) &&
    scalar(grad$projected_gradient_norm, nonnegative = TRUE) &&
    grad$projected_gradient_norm <=
      config$optimizer$projected_gradient_tol &&
    scalar(grad$constraint_gradient_norm, nonnegative = TRUE) &&
    grad$constraint_gradient_norm > 0 &&
    scalar(grad$lagrange_multiplier)
  required_bounds <- c("mu_active", "alpha_active", "kappa_active",
                       "a_active", "weight_active")
  bounds_ok <- is.list(bounds) && all(required_bounds %in% names(bounds)) &&
    all(vapply(bounds[required_bounds], function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value) && !value
    }, logical(1L))) && identical(x$bound_active, FALSE)
  selected_ok <- is.numeric(selected) && length(selected) == 1L &&
    !is.na(selected) && selected == as.integer(selected) && selected >= 1L &&
    is.list(diagnostics) && selected <= length(diagnostics) &&
    is.list(diagnostics[[selected]]) &&
    identical(diagnostics[[selected]]$gate_pass, TRUE) &&
    near(diagnostics[[selected]]$target_attained, x$target_attained) &&
    near(diagnostics[[selected]]$target_error, x$target_error) &&
    near(diagnostics[[selected]]$mean_sequence_loglik,
         x$mean_sequence_loglik) &&
    near(diagnostics[[selected]]$penalized_objective,
         x$penalized_objective) &&
    near(diagnostics[[selected]]$penalty, x$penalty) &&
    near(diagnostics[[selected]]$last_relative_change,
         x$last_relative_change, tolerance = 1e-8) &&
    identical(diagnostics[[selected]]$bound_active, FALSE) &&
    is.list(diagnostics[[selected]]$projected_gradient) &&
    near(diagnostics[[selected]]$projected_gradient$projected_gradient_norm,
         grad$projected_gradient_norm)
  stamp_ok <-
    identical(x$schema_version, "sw2022-v2.1-profile-point-v2") &&
    identical(x$direction, direction) &&
    identical(as.numeric(x$target), as.numeric(target)) &&
    identical(s$config_version, config$version) &&
    identical(s$direction, direction) &&
    identical(as.numeric(s$target), as.numeric(target)) &&
    .sw_v21_profile_same_md5(s$generation_input_md5, generation_md5) &&
    identical(s$runtime_signature, runtime_signature) &&
    identical(s$authorization_md5, authorization_md5) &&
    .sw_v21_profile_same_md5(s$reported_primary_lock_md5,
                             context$lock_md5) &&
    identical(s$reported_primary, context$pointer$reported_primary) &&
    identical(s$selected_specification, selected_spec) &&
    identical(s$learner_tuning_sieve_fixed, TRUE) &&
    identical(s$retuning_performed, FALSE) &&
    identical(s$penalized_nuisance_reoptimization, TRUE) &&
    identical(s$artifact_kind,
              "descriptive penalized-criterion profile sequence") &&
    identical(s$literal_likelihood_profile, FALSE) &&
    identical(s$unpenalized_sequence_likelihood_overlay, TRUE) &&
    identical(s$descriptive_only, TRUE) &&
    identical(s$formal_inference_available, FALSE) &&
    identical(s$formal_test, FALSE) &&
    identical(s$likelihood_ratio_critical_values, FALSE) &&
    identical(s$outcome_blind, FALSE)
  stamp_ok && identical(x$nuisance_reoptimization_gate_pass, TRUE) &&
    sequence_ok && target_ok && objective_ok && stability_ok && gradient_ok &&
    bounds_ok && selected_ok &&
    inherits(x$network_state, "scmix_network_state") &&
    scalar(x$elapsed_seconds, nonnegative = TRUE)
}

.sw_v21_profile_grid <- function(center, direction, config) {
  if (!is.numeric(center) || length(center) != 1L || !is.finite(center) ||
      !direction %in% names(config$grids)) {
    stop("Malformed profile center or direction.", call. = FALSE)
  }
  g <- config$grids[[direction]]
  values <- if (!is.null(g$offsets)) center + as.numeric(g$offsets) else
    center * as.numeric(g$multipliers)
  if (length(values) != 5L || any(!is.finite(values)) ||
      anyDuplicated(values)) {
    stop("Every profile direction must define five unique finite points.",
         call. = FALSE)
  }
  if (identical(direction, "active_covariance_eigenvalue") &&
      any(values <= 0)) {
    stop("The active-eigenvalue grid must remain strictly inside rank one.",
         call. = FALSE)
  }
  if (identical(direction, "headline_contest_probability") &&
      any(values <= 0 | values >= 1)) {
    stop("The contest-probability grid must remain inside (0,1).",
         call. = FALSE)
  }
  values
}

.sw_v21_profile_inv_bounded_tanh <- function(value, bound) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      !is.numeric(bound) || length(bound) != 1L || !is.finite(bound) ||
      bound <= 0 || abs(value) >= bound) {
    stop("A fixed bounded-tanh target must be strictly inside its bound.",
         call. = FALSE)
  }
  bound * atanh(value / bound)
}

.sw_v21_profile_metric_from_arrays <- function(
    direction, mu_raw, kappa, A_raw, U, w, contrast = NULL,
    position_neutral = TRUE) {
  mu_raw <- as.matrix(mu_raw); A_raw <- as.matrix(A_raw)
  U <- as.matrix(U); w <- as.numeric(w)
  if (any(!is.finite(mu_raw)) || any(!is.finite(A_raw)) ||
      any(!is.finite(U)) || any(!is.finite(w)) ||
      nrow(A_raw) != ncol(mu_raw) || ncol(A_raw) != ncol(U) ||
      nrow(U) != length(w) || any(w <= 0) ||
      abs(sum(w) - 1) > 1e-8 || !is.finite(kappa)) {
    stop("Malformed structural arrays for a profile metric.", call. = FALSE)
  }
  if (identical(direction, "kappa")) return(as.numeric(kappa))
  if (identical(direction, "active_covariance_eigenvalue")) {
    return(sum(A_raw^2))
  }
  d <- as.numeric(contrast)
  if (length(d) != ncol(mu_raw) || any(!is.finite(d))) {
    stop("A mean or contest profile needs one finite structural contrast.",
         call. = FALSE)
  }
  m <- as.numeric(mu_raw %*% d)
  if (identical(direction, "female_vs_male_mean")) return(mean(m))
  if (!identical(direction, "headline_contest_probability")) {
    stop("Unknown profile direction.", call. = FALSE)
  }
  residual <- as.numeric(U %*% as.numeric(crossprod(A_raw, d)))
  one <- function(sign_kappa) {
    index <- outer(sign_kappa * kappa + m, residual, `+`)
    as.numeric(stats::plogis(index) %*% w)
  }
  value <- if (isTRUE(position_neutral)) {
    0.5 * (one(1) + one(-1))
  } else one(1)
  mean(value)
}

.sw_v21_profile_metric_tensor <- function(
    net, direction, z_resp_t, coefficient_scale_t, U_t, w_t,
    contrast_t = NULL, position_neutral = TRUE) {
  mu_raw <- net$get_beta(z_resp_t) / coefficient_scale_t$unsqueeze(1L)
  kappa <- net$get_kappa()
  A_raw <- net$A / coefficient_scale_t$unsqueeze(2L)
  if (identical(direction, "kappa")) return(kappa$squeeze())
  if (identical(direction, "active_covariance_eigenvalue")) {
    return(torch::torch_sum(A_raw^2))
  }
  m <- torch::torch_sum(mu_raw * contrast_t$unsqueeze(1L), dim = 2L)
  if (identical(direction, "female_vs_male_mean")) {
    return(torch::torch_mean(m))
  }
  if (!identical(direction, "headline_contest_probability")) {
    stop("Unknown profile direction.", call. = FALSE)
  }
  a_d <- torch::torch_sum(
    A_raw * contrast_t$unsqueeze(2L), dim = 1L)
  residual <- torch::torch_matmul(U_t, a_d$unsqueeze(2L))$squeeze(2L)
  one <- function(sign_kappa) {
    index <- sign_kappa * kappa + m$unsqueeze(2L) + residual$unsqueeze(1L)
    torch::torch_mean(torch::torch_sum(
      torch::torch_sigmoid(index) * w_t$unsqueeze(1L), dim = 2L))
  }
  if (isTRUE(position_neutral)) 0.5 * (one(1) + one(-1)) else one(1)
}

.sw_v21_profile_project_constraint <- function(
    net, direction, target, z_resp_t, coefficient_scale_t, U_t, w_t,
    contrast_t = NULL, position_neutral = TRUE, alpha_bound = 5,
    base_A_raw = NULL, tolerance = 2e-5, max_iter = 80L) {
  scale <- as.numeric(torch::as_array(coefficient_scale_t$detach()$cpu()))
  if (identical(direction, "kappa")) {
    raw <- .sw_v21_profile_inv_bounded_tanh(target, net$kappa_bound)
    torch::with_no_grad(net$kappa_raw$fill_(raw))
  } else if (identical(direction, "active_covariance_eigenvalue")) {
    current <- as.matrix(torch::as_array(net$A$detach()$cpu())) /
      matrix(scale, nrow = length(scale), ncol = net$q)
    norm <- sqrt(sum(current^2))
    if (!is.finite(norm) || norm < 1e-12) {
      current <- as.matrix(base_A_raw)
      norm <- sqrt(sum(current^2))
    }
    if (!is.finite(norm) || norm < 1e-12) {
      stop("Cannot project a zero loading onto a positive eigenvalue.",
           call. = FALSE)
    }
    raw_A <- current * sqrt(target) / norm
    internal <- raw_A * matrix(scale, nrow = length(scale), ncol = net$q)
    tensor <- torch::torch_tensor(internal, dtype = net$A$dtype,
                                  device = net$A$device)
    torch::with_no_grad(net$A$copy_(tensor))
  } else {
    if (!direction %in% c("female_vs_male_mean",
                          "headline_contest_probability")) {
      stop("Unknown constraint direction.", call. = FALSE)
    }
    coordinate <- which(abs(as.numeric(torch::as_array(
      contrast_t$detach()$cpu()))) > 0)[1L]
    if (!identical(coordinate, 1L)) {
      stop("The prespecified scalar projector requires gender coordinate 1.",
           call. = FALSE)
    }
    eval_at <- function(alpha_raw_unit) {
      torch::with_no_grad(net$alpha_raw[coordinate]$fill_(
        alpha_raw_unit * scale[[coordinate]]))
      as.numeric(torch::with_no_grad(.sw_v21_profile_metric_tensor(
        net, direction, z_resp_t, coefficient_scale_t, U_t, w_t,
        contrast_t, position_neutral))$cpu()$item())
    }
    lo <- -as.numeric(alpha_bound); hi <- as.numeric(alpha_bound)
    flo <- eval_at(lo) - target; fhi <- eval_at(hi) - target
    if (!is.finite(flo) || !is.finite(fhi) || flo * fhi > 0) {
      stop("The requested mean/contest target is infeasible inside the fixed alpha bound.",
           call. = FALSE)
    }
    mid <- (lo + hi) / 2
    for (iter in seq_len(as.integer(max_iter))) {
      mid <- (lo + hi) / 2
      fmid <- eval_at(mid) - target
      if (abs(fmid) <= tolerance / 4) break
      if (flo * fmid <= 0) {
        hi <- mid; fhi <- fmid
      } else {
        lo <- mid; flo <- fmid
      }
    }
    eval_at(mid)
  }
  attained <- as.numeric(torch::with_no_grad(.sw_v21_profile_metric_tensor(
    net, direction, z_resp_t, coefficient_scale_t, U_t, w_t,
    contrast_t, position_neutral))$cpu()$item())
  list(attained = attained, target_error = attained - target,
       pass = is.finite(attained) && abs(attained - target) <= tolerance)
}

.sw_v21_profile_projected_gradient <- function(
    net, loss_fun, metric_fun, constraint_floor = 1e-14) {
  params <- net$parameters
  collect <- function(value) {
    out <- torch::autograd_grad(
      value, params, allow_unused = TRUE, create_graph = FALSE)
    lapply(seq_along(out), function(j) {
      if (torch::is_undefined_tensor(out[[j]])) {
        torch::torch_zeros_like(params[[j]])
      } else out[[j]]$detach()$clone()
    })
  }
  g <- collect(loss_fun())
  h <- collect(metric_fun())
  gdot <- sum(vapply(seq_along(g), function(j) {
    as.numeric(torch::torch_sum(g[[j]] * h[[j]])$cpu()$item())
  }, numeric(1L)))
  hdot <- sum(vapply(h, function(x) {
    as.numeric(torch::torch_sum(x^2)$cpu()$item())
  }, numeric(1L)))
  if (!is.finite(hdot) || hdot <= constraint_floor) {
    return(list(projected_gradient_norm = Inf,
                constraint_gradient_norm = sqrt(max(hdot, 0)),
                lagrange_multiplier = NA_real_, pass = FALSE))
  }
  lagrange <- gdot / hdot
  residual <- mapply(function(gg, hh) gg - lagrange * hh, g, h,
                     SIMPLIFY = FALSE)
  norm <- max(vapply(residual, function(x) {
    as.numeric(torch::torch_max(torch::torch_abs(x))$cpu()$item())
  }, numeric(1L)), 0)
  list(projected_gradient_norm = norm,
       constraint_gradient_norm = sqrt(hdot),
       lagrange_multiplier = lagrange, pass = is.finite(norm))
}
