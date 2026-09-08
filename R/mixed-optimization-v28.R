## Prospective version 2.8 numerical audits with the v2.8.1 shape amendment.
##
## These helpers do not change the version 2.2 optimizer.  They add an
## explicit selection policy for nonconvex starts, a transparent summary of
## the preprocessing-normalized blockwise KKT diagnostic, and an independent
## directional finite-difference check of the gradients used by that
## diagnostic.  Application protocols must freeze their constants before
## inspecting version 2.8 outcomes. Version 2.8.1 preserves the R vector shape
## of rank-one Torch parameters and otherwise retains the frozen audit.

.scmix_v28_start_surface <- function(
    starts, near_objective_cutoff = 1e-6, kkt_cutoff = 1e-5) {
  needed <- c(
    "start_id", "penalized_mean_nll", "objective_finite", "dtype",
    "lbfgs_completed", "lbfgs_accepted", "lbfgs_nonworsening",
    "projection_diagnostic_complete", "projection_changed",
    "bound_activity", "gradient_contract_pass",
    "kkt_maximum")
  if (!is.data.frame(starts) || !all(needed %in% names(starts)) ||
      nrow(starts) < 1L || anyNA(starts$start_id) ||
      any(!nzchar(starts$start_id)) || anyDuplicated(starts$start_id)) {
    stop("`starts` is not a complete version 2.8 start audit.", call. = FALSE)
  }
  scalar_cutoff <- function(x, what) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
      stop(what, " must be one finite nonnegative value.", call. = FALSE)
    }
    as.numeric(x)
  }
  near_objective_cutoff <- scalar_cutoff(
    near_objective_cutoff, "near_objective_cutoff")
  kkt_cutoff <- scalar_cutoff(kkt_cutoff, "kkt_cutoff")
  logical_columns <- c(
    "objective_finite", "lbfgs_completed", "lbfgs_accepted",
    "lbfgs_nonworsening", "projection_diagnostic_complete",
    "projection_changed", "bound_activity", "gradient_contract_pass")
  if (any(!vapply(starts[logical_columns], is.logical, logical(1L))) ||
      anyNA(starts[logical_columns]) ||
      !is.character(starts$dtype) || anyNA(starts$dtype) ||
      any(!nzchar(starts$dtype)) ||
      !is.numeric(starts$penalized_mean_nll) ||
      !is.numeric(starts$kkt_maximum)) {
    stop("Version 2.8 start flags and objectives have invalid types.",
         call. = FALSE)
  }

  computationally_eligible <- with(starts,
    objective_finite & is.finite(penalized_mean_nll) & dtype == "float64" &
      lbfgs_completed & lbfgs_accepted & lbfgs_nonworsening &
      projection_diagnostic_complete & !projection_changed &
      !bound_activity & gradient_contract_pass)
  finite <- starts$objective_finite & is.finite(starts$penalized_mean_nll)
  if (!any(finite)) {
    stop("Every registered start has a nonfinite objective.", call. = FALSE)
  }
  selection_pool <- which(computationally_eligible)
  diagnostic_selection <- !length(selection_pool)
  if (diagnostic_selection) selection_pool <- which(finite)
  selected <- selection_pool[[which.min(
    starts$penalized_mean_nll[selection_pool])]]
  selected_objective <- starts$penalized_mean_nll[[selected]]
  relative_gap <- (starts$penalized_mean_nll - selected_objective) /
    max(1, abs(selected_objective))
  relative_gap[!finite] <- Inf
  lower_inadmissible <- finite & !computationally_eligible &
    starts$penalized_mean_nll < selected_objective -
      100 * .Machine$double.eps * max(1, abs(selected_objective))
  near <- computationally_eligible & is.finite(relative_gap) &
    relative_gap <= near_objective_cutoff
  near[[selected]] <- computationally_eligible[[selected]]

  surface <- starts
  surface$selection_eligible <- computationally_eligible
  surface$selected <- seq_len(nrow(surface)) == selected
  surface$relative_objective_gap <- relative_gap
  surface$near_optimum <- near
  surface$target_comparison_eligible <- near
  surface$inferior_start_diagnostic <- computationally_eligible & !near
  surface$lower_objective_inadmissible <- lower_inadmissible
  n_near <- sum(near)
  selected_kkt_pass <- computationally_eligible[[selected]] &&
    is.finite(starts$kkt_maximum[[selected]]) &&
    starts$kkt_maximum[[selected]] <= kkt_cutoff
  fit_gate_pass <- !diagnostic_selection && !any(lower_inadmissible) &&
    selected_kkt_pass
  list(
    surface = surface,
    selected_index = as.integer(selected),
    selected_start_id = surface$start_id[[selected]],
    selected_objective = selected_objective,
    diagnostic_selection = diagnostic_selection,
    lower_objective_inadmissible = any(lower_inadmissible),
    n_near = as.integer(n_near),
    near_optimum_status = if (!n_near) {
      "not_applicable_no_admissible_near_state"
    } else if (n_near == 1L) {
      "not_applicable_singleton_near_set"
    } else {
      "applicable_multiple_near_states"
    },
    selected_kkt_pass = selected_kkt_pass,
    fit_gate_pass = fit_gate_pass,
    policy = paste(
      "select the minimum penalized respondent-sequence mean NLL among",
      "computationally eligible starts; inferior starts are descriptive;",
      "only eligible starts within the relative objective cutoff enter",
      "target-stability comparisons"),
    near_objective_cutoff = near_objective_cutoff,
    kkt_cutoff = kkt_cutoff)
}

.scmix_v28_kkt_contract <- function(kkt, cutoff = 1e-5) {
  if (!is.list(kkt) || !isTRUE(kkt$all_finite) ||
      !is.data.frame(kkt$blocks) || !is.list(kkt$raw_residual_vectors)) {
    stop("A complete finite blockwise KKT object is required.", call. = FALSE)
  }
  cutoff <- as.numeric(cutoff)
  if (length(cutoff) != 1L || !is.finite(cutoff) || cutoff < 0) {
    stop("`cutoff` must be one finite nonnegative value.", call. = FALSE)
  }
  rows <- kkt$blocks
  expected <- c(
    "position_effect", "unpenalized_mean_reference",
    "centered_mean_deviation", "horizontal_loadings",
    "all_structural_coordinates")
  if (!identical(rows$block, expected)) {
    stop("The KKT block order does not match the version 2.8 contract.",
         call. = FALSE)
  }
  structural <- expected[-length(expected)]
  maximum <- vapply(structural, function(nm) {
    z <- as.numeric(kkt$raw_residual_vectors[[nm]])
    if (!length(z)) NA_real_ else max(abs(z))
  }, numeric(1L))
  maximum <- c(maximum,
               all_structural_coordinates = if (all(is.na(maximum))) {
                 NA_real_
               } else max(maximum, na.rm = TRUE))
  rows$projected_kkt_max_abs <- unname(maximum[rows$block])
  rows$gate_metric <- rows$block != "all_structural_coordinates" &
    rows$applicable
  gate_rows <- rows$gate_metric
  if (!any(gate_rows)) {
    stop("No applicable structural KKT block was supplied.", call. = FALSE)
  }
  pass <- all(is.finite(rows$projected_kkt_rms[gate_rows])) &&
    max(rows$projected_kkt_rms[gate_rows]) <= cutoff
  list(
    metric = "preprocessing_normalized_blockwise_projected_gradient_RMS",
    gated_summary = "maximum RMS across applicable declared blocks",
    cutoff = cutoff, blocks = rows, pass = pass,
    maximum_block_rms = max(rows$projected_kkt_rms[gate_rows]),
    maximum_coordinate_diagnostic = max(
      rows$projected_kkt_max_abs[gate_rows], na.rm = TRUE),
    scale_claim = paste(
      "invariant to the registered diagonal DeltaX preprocessing",
      "reparameterization when coefficients, output rows, and loadings are",
      "mapped under the loading-scale contract; the objective is fixed as",
      "mean negative complete-sequence log likelihood per respondent plus",
      "the declared penalty in nats"),
    objective_definition_id = .scmix_v22_objective_id)
}

.scmix_v28_audit_tensors <- function(deltaX, y, Z, respondent_id, grid,
                                     coefficient_scale, device = "cpu") {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("The version 2.8 gradient audit requires torch.", call. = FALSE)
  }
  deltaX <- as.matrix(deltaX); Z <- as.matrix(Z); y <- as.numeric(y)
  respondent_id <- as.character(respondent_id)
  if (nrow(deltaX) != length(y) || nrow(Z) != length(y) ||
      length(respondent_id) != length(y) || anyNA(y) ||
      any(!is.finite(deltaX)) || any(!is.finite(Z)) ||
      !is.list(grid) || is.null(grid$U) || is.null(grid$w)) {
    stop("Malformed data or integration grid for the gradient audit.",
         call. = FALSE)
  }
  scale <- .scmix_loading_scale(
    coefficient_scale, ncol(deltaX), "coefficient_scale")
  rf <- factor(respondent_id, levels = unique(respondent_id))
  dev <- torch::torch_device(device)
  own <- function(x, dtype) {
    torch::torch_tensor(x, dtype = dtype, device = dev)$clone()
  }
  q <- ncol(as.matrix(grid$U))
  list(
    data = list(
      dx = own(sweep(deltaX, 2L, scale, `/`), torch::torch_float64()),
      zt = own(Z, torch::torch_float64()),
      yt = own(y, torch::torch_float64()),
      idx1 = own(as.integer(rf), torch::torch_long()),
      N = nlevels(rf)),
    U_t = if (q == 0L) {
      torch::torch_zeros(1L, 1L, dtype = torch::torch_float64(), device = dev)
    } else own(as.matrix(grid$U), torch::torch_float64()),
    logw_t = own(log(as.numeric(grid$w)), torch::torch_float64()),
    coefficient_scale = scale)
}

.scmix_v28_direction_bank <- function(net, coefficient_scale,
                                      kappa_bound, alpha_bound,
                                      a_bound, weight_bound, seed) {
  params <- net$parameters
  p <- net$p_beta; q <- net$q
  coefficient_scale <- .scmix_loading_scale(
    coefficient_scale, p, "coefficient_scale")
  random_sign <- function(n) sample(c(-1, 1), n, replace = TRUE)
  shape_like <- function(values, template) {
    values <- as.numeric(values)
    if (length(values) != length(template)) {
      stop("A gradient-audit direction has the wrong length.", call. = FALSE)
    }
    template_dim <- dim(template)
    if (is.null(template_dim)) values else array(values, dim = template_dim)
  }
  out <- list()
  .scmix_dml_with_seed(as.integer(seed), {
    if ("kappa_raw" %in% names(params)) {
      theta_kappa <- .scmix_parameter_array(params$kappa_raw)
      delta_kappa <- rep(kappa_bound, length(theta_kappa))
      if (!is.null(dim(theta_kappa))) dim(delta_kappa) <- dim(theta_kappa)
      out$position_effect <- list(kappa_raw = delta_kappa)
    }
    if ("alpha_raw" %in% names(params)) {
      theta_alpha <- .scmix_parameter_array(params$alpha_raw)
      d <- random_sign(p) / sqrt(p)
      out$unpenalized_mean_reference <- list(
        alpha_raw = shape_like(
          d * coefficient_scale * alpha_bound, theta_alpha))
    }
    deviation_names <- setdiff(
      names(params), c("A", "kappa_raw", "alpha_raw"))
    if (length(deviation_names)) {
      total <- sum(vapply(deviation_names, function(nm)
        length(.scmix_parameter_array(params[[nm]])), integer(1L)))
      if (!is.finite(total) || total <= 0L) {
        stop("The gradient-audit deviation block is empty.", call. = FALSE)
      }
      dev <- lapply(deviation_names, function(nm) {
        theta <- .scmix_parameter_array(params[[nm]])
        direction <- shape_like(
          random_sign(length(theta)) / sqrt(total), theta)
        scale <- if (!identical(net$mean_family, "legacy") &&
            identical(nm, "param_layer.weight")) {
          matrix(coefficient_scale * weight_bound, nrow = p,
                 ncol = ncol(as.matrix(theta)))
        } else shape_like(rep(weight_bound, length(theta)), theta)
        direction * scale
      })
      names(dev) <- deviation_names
      out$centered_mean_deviation <- dev
    }
    if (q > 0L && "A" %in% names(params)) {
      A_opt <- .scmix_parameter_array(params$A)
      dim(A_opt) <- c(p, q)
      A_raw <- .scmix_loading_to_raw(A_opt, coefficient_scale)
      proposed <- matrix(random_sign(p * q), p, q)
      horizontal <- .scmix_horizontal_loading_gradient(
        A_raw, proposed)$gradient
      norm <- sqrt(sum(horizontal^2))
      if (!is.finite(norm) || norm <= 0) {
        stop("Could not construct a horizontal loading audit direction.",
             call. = FALSE)
      }
      delta_raw <- a_bound * horizontal / norm
      out$horizontal_loadings <- list(
        A = sweep(delta_raw, 1L, coefficient_scale, `*`))
    }
  })
  out
}

.scmix_v28_copy_parameter_arrays <- function(net) {
  out <- lapply(net$parameters, .scmix_parameter_array)
  names(out) <- names(net$parameters)
  out
}

.scmix_v28_load_parameter_arrays <- function(net, state) {
  if (!identical(names(net$parameters), names(state))) {
    stop("Parameter-array state names do not match the network.",
         call. = FALSE)
  }
  torch::with_no_grad({
    for (nm in names(state)) {
      template <- net$parameters[[nm]]
      template_array <- .scmix_parameter_array(template)
      if (length(state[[nm]]) != length(template_array) ||
          !identical(dim(state[[nm]]), dim(template_array))) {
        stop("Parameter-array state shape does not match the network for ",
             nm, ".", call. = FALSE)
      }
      value <- torch::torch_tensor(
        state[[nm]], dtype = template$dtype, device = template$device)
      template$copy_(value)
    }
  })
  invisible(net)
}

.scmix_v28_directional_ad <- function(net, direction) {
  sum(vapply(names(direction), function(nm) {
    grad <- .scmix_parameter_array(net$parameters[[nm]], gradient = TRUE)
    sum(as.numeric(grad) * as.numeric(direction[[nm]]))
  }, numeric(1L)))
}

.scmix_v28_gradient_contract_audit <- function(
    net, deltaX, y, Z, respondent_id, grid, weight_decay,
    coefficient_scale, kappa_bound, alpha_bound, a_bound, weight_bound,
    steps = c(1e-3, 3e-4, 1e-4, 3e-5, 1e-5, 3e-6, 1e-6, 3e-7, 1e-7),
    seed = 1L, relative_tolerance = 1e-4,
    absolute_tolerance = 1e-8, device = "cpu") {
  if (!inherits(net, "nn_module") ||
      !identical(.scmix_net_dtype_label(net), "float64")) {
    stop("The version 2.8 gradient audit requires a float64 nn_module.",
         call. = FALSE)
  }
  steps <- sort(unique(as.numeric(steps)), decreasing = TRUE)
  if (length(steps) < 2L || any(!is.finite(steps)) || any(steps <= 0) ||
      !is.numeric(relative_tolerance) || length(relative_tolerance) != 1L ||
      !is.finite(relative_tolerance) || relative_tolerance <= 0 ||
      !is.numeric(absolute_tolerance) || length(absolute_tolerance) != 1L ||
      !is.finite(absolute_tolerance) || absolute_tolerance <= 0) {
    stop("Invalid version 2.8 gradient-audit controls.", call. = FALSE)
  }
  tensors <- .scmix_v28_audit_tensors(
    deltaX, y, Z, respondent_id, grid, coefficient_scale, device)
  original <- .scmix_v28_copy_parameter_arrays(net)
  original_hash <- .scmix_stable_state_hash(original)
  on.exit(.scmix_v28_load_parameter_arrays(net, original), add = TRUE)
  evaluated <- .sc_mixed_evaluate_full_objective(
    net, tensors$data, tensors$U_t, tensors$logw_t, weight_decay,
    tensors$coefficient_scale, kappa_bound, alpha_bound, a_bound,
    weight_bound, backward = TRUE)
  directions <- .scmix_v28_direction_bank(
    net, tensors$coefficient_scale, kappa_bound, alpha_bound,
    a_bound, weight_bound, seed)
  for (block in names(directions)) {
    direction <- directions[[block]]
    if (!length(direction) || is.null(names(direction)) ||
        !all(names(direction) %in% names(net$parameters))) {
      stop("A gradient-audit block has an invalid parameter registry.",
           call. = FALSE)
    }
    for (nm in names(direction)) {
      template <- original[[nm]]
      value <- direction[[nm]]
      if (length(value) != length(template) ||
          !identical(dim(value), dim(template)) ||
          any(!is.finite(value))) {
        stop("A gradient-audit direction is nonfinite or has the wrong shape for ",
             nm, ".", call. = FALSE)
      }
    }
  }
  evaluate_at <- function(direction, amount) {
    .scmix_v28_load_parameter_arrays(net, original)
    torch::with_no_grad({
      for (nm in names(direction)) {
        template <- net$parameters[[nm]]
        value <- original[[nm]] + amount * direction[[nm]]
        template$copy_(torch::torch_tensor(
          value, dtype = template$dtype, device = template$device))
      }
    })
    .sc_mixed_evaluate_full_objective(
      net, tensors$data, tensors$U_t, tensors$logw_t, weight_decay,
      tensors$coefficient_scale, kappa_bound, alpha_bound, a_bound,
      weight_bound, backward = FALSE)$objective
  }
  tables <- lapply(names(directions), function(block) {
    direction <- directions[[block]]
    .scmix_v28_load_parameter_arrays(net, original)
    evaluated_block <- .sc_mixed_evaluate_full_objective(
      net, tensors$data, tensors$U_t, tensors$logw_t, weight_decay,
      tensors$coefficient_scale, kappa_bound, alpha_bound, a_bound,
      weight_bound, backward = TRUE)
    ad <- .scmix_v28_directional_ad(net, direction)
    fd <- vapply(steps, function(h) {
      plus <- evaluate_at(direction, h)
      minus <- evaluate_at(direction, -h)
      (plus - minus) / (2 * h)
    }, numeric(1L))
    scale <- pmax(abs(ad), abs(fd))
    tolerance <- absolute_tolerance + relative_tolerance * scale
    close_to_ad <- is.finite(fd) & abs(fd - ad) <= tolerance
    adjacent <- rep(FALSE, length(steps))
    if (length(steps) > 1L) {
      for (j in seq_len(length(steps) - 1L)) {
        mutual_scale <- max(abs(ad), abs(fd[j]), abs(fd[j + 1L]))
        mutual_tol <- 2 * (absolute_tolerance +
                           relative_tolerance * mutual_scale)
        adjacent[j] <- close_to_ad[j] && close_to_ad[j + 1L] &&
          is.finite(fd[j + 1L]) && abs(fd[j] - fd[j + 1L]) <= mutual_tol
      }
    }
    data.frame(
      block = block, direction = 1L, scheme = "central_step_ladder",
      step = steps, autodiff = ad, finite_difference = fd,
      absolute_error = abs(fd - ad),
      symmetric_relative_error = abs(fd - ad) /
        pmax(abs(fd) + abs(ad), .Machine$double.xmin),
      close_to_autodiff = close_to_ad,
      adjacent_plateau = adjacent,
      pass = adjacent, stringsAsFactors = FALSE)
  })
  table <- do.call(rbind, tables); rownames(table) <- NULL
  pass_by_block <- vapply(split(table, table$block), function(x)
    any(x$pass), logical(1L))
  .scmix_v28_load_parameter_arrays(net, original)
  restored_hash <- .scmix_stable_state_hash(
    .scmix_v28_copy_parameter_arrays(net))
  list(
    protocol = "paperps-gradient-contract-v2.8.1",
    objective_definition_id = .scmix_v22_objective_id,
    dtype = .scmix_net_dtype_label(net),
    state_hash_before = original_hash,
    state_hash_after = restored_hash,
    state_restored_exactly = identical(original_hash, restored_hash),
    steps = steps, relative_tolerance = relative_tolerance,
    absolute_tolerance = absolute_tolerance, seed = as.integer(seed),
    table = table, pass_by_block = pass_by_block,
    nonsmooth_or_unverified_by_block = !pass_by_block,
    overall_pass = all(pass_by_block) &&
      identical(original_hash, restored_hash),
    interpretation = paste(
      "a block passes only when autodiff agrees with central differences",
      "at two adjacent prespecified step sizes; failure may indicate a",
      "gradient-contract error or local nonsmoothness and withholds release"))
}
