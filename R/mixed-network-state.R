## Portable serialization for the Torch conditional-mean network used by
## scmix.  An nn_module contains external pointers and therefore cannot be
## made durable with saveRDS() alone.  These helpers copy every state-dict
## tensor to an ordinary CPU R array, retain the exact architecture and
## training-scale transforms, and reconstruct a fresh module when needed.

.scmix_state_integer <- function(x, name, lower = 0L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      x != as.integer(x) || x < lower) {
    stop("Network-state `", name, "` must be one integer >= ", lower, ".",
         call. = FALSE)
  }
  as.integer(x)
}

.scmix_state_positive <- function(x, name, length_out = 1L) {
  if (!is.numeric(x) || length(x) != length_out || any(!is.finite(x)) ||
      any(x <= 0)) {
    stop("Network-state `", name, "` must contain ", length_out,
         " finite positive value(s).", call. = FALSE)
  }
  as.numeric(x)
}

.scmix_state_names <- function(x, n, prefix, what) {
  if (is.null(x)) return(paste0(prefix, seq_len(n)))
  x <- as.character(x)
  if (length(x) != n || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    stop("Network-state ", what, " must contain ", n,
         " unique nonempty names.", call. = FALSE)
  }
  x
}

.scmix_tensor_cpu_array <- function(x) {
  value <- torch::as_array(x$detach()$cpu())
  storage.mode(value) <- "double"
  value
}

.scmix_stable_state_hash <- function(x) {
  bytes <- as.integer(serialize(x, NULL, ascii = FALSE, version = 2L))
  modulus <- 2147483629
  roll <- function(seed, multiplier) {
    value <- as.double(seed)
    for (byte in bytes) value <- (value * multiplier + byte + 1) %% modulus
    as.integer(value)
  }
  paste0("scmix-state-v1-",
         sprintf("%08x", roll(14695981, 65599)),
         sprintf("%08x", roll(21661363, 131071)))
}

.scmix_state_order_vector <- function(x, expected_names, what,
                                      positive = FALSE,
                                      logical_only = FALSE) {
  if (is.null(x) || length(x) != length(expected_names) || anyNA(x)) {
    stop("Network-state ", what, " has incompatible dimension or missing values.",
         call. = FALSE)
  }
  if (isTRUE(logical_only)) {
    if (!is.logical(x)) {
      stop("Network-state ", what, " must be logical.", call. = FALSE)
    }
  } else {
    if (!is.numeric(x) || any(!is.finite(x)) ||
        (isTRUE(positive) && any(x <= 0))) {
      stop("Network-state ", what, " must be finite",
           if (isTRUE(positive)) " and positive" else "", ".",
           call. = FALSE)
    }
  }
  observed_names <- names(x)
  if (!is.null(observed_names)) {
    if (anyNA(observed_names) || any(!nzchar(observed_names)) ||
        anyDuplicated(observed_names) ||
        !setequal(observed_names, expected_names)) {
      stop("Network-state ", what,
           " names do not match the stored column order.", call. = FALSE)
    }
    x <- x[expected_names]
  }
  names(x) <- expected_names
  x
}

.scmix_state_preprocessing <- function(z_transform, dx_transform,
                                       moderator_names, coefficient_names,
                                       coefficient_scale) {
  if (!is.list(z_transform) || is.null(z_transform$center) ||
      is.null(z_transform$scale)) {
    stop("A portable network state requires the fitted Z center and scale.",
         call. = FALSE)
  }
  z_transform$center <- .scmix_state_order_vector(
    z_transform$center, moderator_names, "Z center")
  z_transform$scale <- .scmix_state_order_vector(
    z_transform$scale, moderator_names, "Z scale", positive = TRUE)
  if (!is.null(z_transform$constant)) {
    z_transform$constant <- .scmix_state_order_vector(
      z_transform$constant, moderator_names, "Z constant indicator",
      logical_only = TRUE)
  }
  if (!is.list(dx_transform) || is.null(dx_transform$scale)) {
    stop("A portable network state requires the fitted DeltaX scale.",
         call. = FALSE)
  }
  dx_transform$scale <- .scmix_state_order_vector(
    dx_transform$scale, coefficient_names, "DeltaX scale", positive = TRUE)
  tolerance <- 1e-12 * max(1, max(abs(coefficient_scale)))
  if (max(abs(unname(dx_transform$scale) - unname(coefficient_scale))) >
      tolerance) {
    stop("The stored DeltaX scale differs from the network coefficient scale.",
         call. = FALSE)
  }
  list(Z = z_transform, deltaX = dx_transform)
}

.scmix_capture_network_state <- function(
    net, p, p_Z, q, hidden, mean_family = "legacy",
    mu_bound, kappa_bound, alpha_bound = 5, a_bound, weight_bound,
    coefficient_scale, z_transform = NULL, dx_transform = NULL,
    coefficient_names = NULL, moderator_names = NULL,
    integration_grid = NULL, analysis_signature = NULL, scope = NULL) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("Capturing a mixed-network state requires the 'torch' package.",
         call. = FALSE)
  }
  if (!inherits(net, "nn_module")) {
    stop("`net` must be a live Torch nn_module.", call. = FALSE)
  }
  p <- .scmix_state_integer(p, "p", 1L)
  p_Z <- .scmix_state_integer(p_Z, "p_Z", 1L)
  q <- .scmix_state_integer(q, "q", 0L)
  if (q > p) stop("Network-state `q` cannot exceed `p`.", call. = FALSE)
  family <- .sc_mixed_mean_family(mean_family, hidden)
  mean_family <- family$mean_family
  hidden <- family$hidden
  mu_bound <- .scmix_state_positive(mu_bound, "mu_bound")
  kappa_bound <- .scmix_state_positive(kappa_bound, "kappa_bound")
  alpha_bound <- .scmix_state_positive(alpha_bound, "alpha_bound")
  a_bound <- .scmix_state_positive(a_bound, "a_bound")
  weight_bound <- .scmix_state_positive(weight_bound, "weight_bound")
  coefficient_names <- .scmix_state_names(
    coefficient_names, p, "b", "coefficient names")
  moderator_names <- .scmix_state_names(
    moderator_names, p_Z, "z", "moderator names")
  coefficient_scale <- .scmix_state_positive(
    coefficient_scale, "coefficient_scale", p)
  names(coefficient_scale) <- coefficient_names
  preprocessing <- .scmix_state_preprocessing(
    z_transform, dx_transform, moderator_names, coefficient_names,
    coefficient_scale)
  integration <- NULL
  if (!is.null(integration_grid)) {
    if (!is.list(integration_grid) || is.null(integration_grid$U) ||
        is.null(integration_grid$w)) {
      stop("Network-state integration_grid must contain U and w.",
           call. = FALSE)
    }
    U <- as.matrix(integration_grid$U)
    w <- as.numeric(integration_grid$w)
    if (!is.numeric(U) || ncol(U) != q || nrow(U) != length(w) ||
        !length(w) || any(!is.finite(U)) || any(!is.finite(w)) ||
        any(w <= 0)) {
      stop("Network-state integration nodes or weights are malformed.",
           call. = FALSE)
    }
    integration <- list(
      U = U, w = w, metadata = integration_grid$metadata,
      pairing = paste(
        "These exact unrotated nodes are paired with the serialized A;",
        "do not rotate A while retaining a finite asymmetric grid."))
  }

  live <- net$state_dict()
  if (!is.list(live) || !length(live) || is.null(names(live)) ||
      any(!nzchar(names(live))) || anyDuplicated(names(live))) {
    stop("The Torch module returned a malformed state_dict.", call. = FALSE)
  }
  state <- lapply(live, .scmix_tensor_cpu_array)
  shapes <- lapply(live, function(x) as.integer(x$shape))
  dtypes <- lapply(live, function(x) as.character(x$dtype))
  names(dtypes) <- names(shapes) <- names(state) <- names(live)

  out <- list(
    format = "scmix-network-state",
    format_version = 2L,
    architecture_id = "mixed-conjoint-mean-family-v2",
    tensor_storage = "ordinary double R arrays copied from CPU tensors",
    architecture = list(
      p = p, p_Z = p_Z, q = q, hidden = hidden,
      mean_family = mean_family,
      compute_dtype = .scmix_net_dtype_label(net),
      mu_bound = unname(mu_bound), kappa_bound = unname(kappa_bound),
      alpha_bound = unname(alpha_bound),
      a_bound = unname(a_bound), weight_bound = unname(weight_bound),
      coefficient_scale = coefficient_scale
    ),
    state_dict = state,
    state_shapes = shapes,
    state_dtypes = dtypes,
    preprocessing = preprocessing,
    integration_grid = integration,
    coefficient_names = coefficient_names,
    moderator_names = moderator_names,
    analysis_signature = analysis_signature,
    scope = scope,
    loading_contract = list(
      contract_id = "scmix-loading-scale-v1",
      canonical_structural_scale = "raw",
      stored_loading_scale = "optimization",
      stored_loading_field = "state_dict$A",
      coefficient_scale_field = "architecture$coefficient_scale",
      raw_to_optimization =
        "A_optimization = diag(coefficient_scale) %*% A_raw"),
    prediction_contract = paste(
      "Raw moderator rows are transformed with preprocessing$Z; network",
      "outputs are divided by architecture$coefficient_scale to return",
      "coefficients in the raw DeltaX units."
    ),
    resumable_training = FALSE
  )
  class(out) <- c("scmix_network_state", "list")
  .scmix_validate_network_state(out)
  out
}

.scmix_attach_network_loading_evidence <- function(bundle, evidence) {
  .scmix_validate_network_state(bundle)
  .scmix_validate_loading_invariance_evidence(evidence)
  if (!identical(bundle$analysis_signature, evidence$analysis_signature) ||
      !identical(bundle$scope, evidence$scope)) {
    stop("L002 evidence does not match the portable network-state scope.",
         call. = FALSE)
  }
  bundle$loading_contract$gate_evidence <- list(L002 = evidence)
  .scmix_validate_network_state(bundle)
  bundle
}

## Complete the lightweight state dictionaries retained by the optimizer with
## one already validated architecture/preprocessing template. This keeps every
## production start portable without retaining duplicate live Torch pointers.
.scmix_complete_multistart_artifacts <- function(fit, template_state,
                                                  coefficient_scale) {
  starts <- fit$start_artifacts
  if (!is.list(starts) || !length(starts)) return(NULL)
  q <- template_state$architecture$q
  p <- length(coefficient_scale)
  out <- lapply(starts, function(x) {
    bundle <- template_state
    bundle$state_dict <- x$state_dict
    bundle$state_shapes <- x$state_shapes
    bundle$state_dtypes <- x$state_dtypes
    ## The fit scope remains identical across starts. `start_id` supplies the
    ## within-fit identity and the record fingerprint binds the two together.
    ## L002 is evidence for the selected fit, not for every nonselected start.
    ## Do not silently copy selected-fit probability evidence onto another
    ## state; F008 consumers reconstruct their own registered targets.
    bundle$loading_contract$gate_evidence <- NULL
    .scmix_validate_network_state(bundle)
    A_opt <- .scmix_loading_matrix(x$A_optimization, p = p, q = q,
                                   what = "start A_optimization")
    A_raw <- .scmix_loading_to_raw(A_opt, coefficient_scale)
    record <- list(
      start = as.integer(x$start), start_id = x$start_id,
      seed = x$seed,
      state_hash = x$state_hash,
      selected = isTRUE(x$selected),
      mean_objective = as.numeric(x$mean_objective),
      objective_definition_id = x$objective_definition_id,
      unpenalized_mean_nll = as.numeric(x$unpenalized_mean_nll),
      relative_objective_from_best = NA_real_,
      A_raw = A_raw, A_optimization = A_opt,
      kappa = as.numeric(x$kappa), dtype = x$dtype,
      protocol_mode = x$protocol_mode, polished = isTRUE(x$polished),
      calibration_hash = x$calibration_hash %||% NA_character_,
      analysis_signature = bundle$analysis_signature,
      scope = bundle$scope,
      optimizer_evidence = x$optimizer_evidence,
      v22_optimization_status = x$v22_optimization_status,
      network_state = bundle)
    record
  })
  objective <- vapply(out, `[[`, numeric(1L), "mean_objective")
  best <- min(objective)
  relative <- (objective - best) / max(1, abs(best))
  for (s in seq_along(out)) {
    out[[s]]$relative_objective_from_best <- relative[s]
    out[[s]]$artifact_fingerprint <-
      .scmix_multistart_record_fingerprint(out[[s]])
  }
  result <- structure(out,
            best_start = which.min(objective),
            protocol_mode = unique(vapply(out, `[[`, character(1L),
                                           "protocol_mode")),
            adapter_contract = list(
              consumer = "scmix_v22_multistart_stability",
              record_fields = c(
                "start_id", "seed", "state_hash", "mean_objective",
                "objective_definition_id", "polished", "dtype",
                "protocol_mode", "calibration_hash", "analysis_signature",
                "scope", "optimizer_evidence", "artifact_fingerprint"),
              application_runner_adds = c(
                "target_estimates", "raw_if_standard_errors",
                "analysis_signature", "registry_hash"),
              objective_orientation = "lower_is_better",
              target_values_must_be_computed_from = paste(
                "the record's network_state, A_raw, and kappa; never from",
                "the selected fit for a nonselected start")),
            class = c("scmix_multistart_artifacts", "list"))
  attr(result, "artifact_set_fingerprint") <- .scmix_stable_state_hash(
    lapply(result, `[[`, "artifact_fingerprint"))
  .scmix_validate_multistart_artifacts(result)
  result
}

.scmix_multistart_record_fingerprint <- function(x) {
  x$artifact_fingerprint <- NULL
  .scmix_stable_state_hash(x)
}

.scmix_validate_multistart_artifacts <- function(x) {
  if (!inherits(x, "scmix_multistart_artifacts") || !is.list(x) ||
      !length(x)) {
    stop("Malformed scmix multistart artifact set.", call. = FALSE)
  }
  required <- c(
    "start_id", "seed", "state_hash", "mean_objective", "objective_definition_id",
    "polished", "dtype", "protocol_mode", "calibration_hash",
    "analysis_signature", "scope", "optimizer_evidence",
    "artifact_fingerprint", "network_state", "A_raw", "A_optimization",
    "kappa")
  for (s in seq_along(x)) {
    record <- x[[s]]
    if (!is.list(record) || !all(required %in% names(record)) ||
        !identical(record$objective_definition_id, .scmix_v22_objective_id) ||
        !identical(record$dtype, "float64") ||
        !record$protocol_mode %in% c("calibration", "production") ||
        !is.character(record$start_id) || length(record$start_id) != 1L ||
        !(is.null(record$seed) ||
          (is.numeric(record$seed) && length(record$seed) == 1L &&
           is.finite(record$seed) && record$seed == as.integer(record$seed))) ||
        !is.character(record$state_hash) || length(record$state_hash) != 1L ||
        !is.numeric(record$mean_objective) ||
        length(record$mean_objective) != 1L ||
        !is.finite(record$mean_objective) ||
        !inherits(record$network_state, "scmix_network_state")) {
      stop("Malformed multistart record ", s, ".", call. = FALSE)
    }
    .scmix_validate_network_state(record$network_state)
    if (!identical(record$analysis_signature,
                   record$network_state$analysis_signature) ||
        !identical(record$scope, record$network_state$scope)) {
      stop("Multistart record ", s,
           " is not bound to its network-state scope and analysis.",
           call. = FALSE)
    }
    state_hash <- .scmix_stable_state_hash(list(
      state_dict = record$network_state$state_dict,
      A_optimization = record$A_optimization, kappa = record$kappa))
    if (!identical(record$state_hash, state_hash)) {
      stop("Multistart record ", s, " has a state-hash mismatch.",
           call. = FALSE)
    }
    evidence <- record$optimizer_evidence
    objective <- evidence$objective
    polish <- evidence$polish
    required_evidence <- is.list(evidence) &&
      identical(evidence$protocol_id,
                "paperps-inference-2026-09-06-v2.3") &&
      identical(evidence$protocol_mode, record$protocol_mode) &&
      identical(evidence$calibration_hash, record$calibration_hash) &&
      is.list(objective) && is.list(polish) && is.list(evidence$bounds) &&
      is.list(evidence$kkt) && is.list(evidence$gate_evidence) &&
      identical(objective$definition_id, .scmix_v22_objective_id) &&
      identical(polish$method_id, .scmix_v22_polish_method) &&
      identical(polish$method, "full_batch_lbfgs") &&
      identical(polish$line_search, "strong_wolfe") &&
      all(c("before_polish", "after_polish", "returned",
            "nonworsening_numerical_allowance") %in% names(objective)) &&
      all(c("completed", "accepted", "projection_changed", "nonworsening",
            "same_objective_all_stages",
            "returned_matches_accepted_objective", "iterations",
            "function_evaluations", "line_search_status") %in% names(polish)) &&
      all(c("before_polish", "after_polish", "returned") %in%
            names(evidence$kkt)) &&
      all(paste0("F00", 1:6) %in% names(evidence$gate_evidence))
    if (!required_evidence) {
      stop("Multistart record ", s,
           " lacks complete optimizer evidence.", call. = FALSE)
    }
    expected <- .scmix_multistart_record_fingerprint(record)
    if (!identical(record$artifact_fingerprint, expected)) {
      stop("Multistart record ", s,
           " has an optimizer-evidence fingerprint mismatch (stored ",
           record$artifact_fingerprint, ", recomputed ", expected, ").",
           call. = FALSE)
    }
  }
  expected_set <- .scmix_stable_state_hash(
    lapply(x, `[[`, "artifact_fingerprint"))
  if (!identical(attr(x, "artifact_set_fingerprint"), expected_set)) {
    stop("The multistart artifact-set fingerprint does not match its records.",
         call. = FALSE)
  }
  invisible(x)
}

.scmix_validate_network_state <- function(bundle) {
  is_v1 <- identical(bundle$format_version, 1L) &&
    identical(bundle$architecture_id, "mixed-conjoint-dnn-relu-tanh-v1")
  is_v2 <- identical(bundle$format_version, 2L) &&
    identical(bundle$architecture_id, "mixed-conjoint-mean-family-v2")
  if (!inherits(bundle, "scmix_network_state") ||
      !identical(bundle$format, "scmix-network-state") ||
      !(is_v1 || is_v2)) {
    stop("`bundle` is not a supported scmix network-state object.",
         call. = FALSE)
  }
  a <- bundle$architecture
  if (!is.list(a)) stop("The network-state architecture is missing.",
                        call. = FALSE)
  p <- .scmix_state_integer(a$p, "p", 1L)
  p_Z <- .scmix_state_integer(a$p_Z, "p_Z", 1L)
  q <- .scmix_state_integer(a$q, "q", 0L)
  if (q > p) stop("Network-state `q` cannot exceed `p`.", call. = FALSE)
  family <- if (is_v1) {
    .sc_mixed_mean_family("legacy", a$hidden)
  } else {
    .sc_mixed_mean_family(a$mean_family, a$hidden)
  }
  if (!is.null(a$compute_dtype) &&
      (!is.character(a$compute_dtype) || length(a$compute_dtype) != 1L ||
       is.na(a$compute_dtype) ||
       !a$compute_dtype %in% c("float32", "float64"))) {
    stop("Network-state `compute_dtype` must be 'float32' or 'float64'.",
         call. = FALSE)
  }
  if (!is.null(bundle$loading_contract) &&
      (!is.list(bundle$loading_contract) ||
       !identical(bundle$loading_contract$contract_id,
                  "scmix-loading-scale-v1"))) {
    stop("The portable state's loading-scale contract is unsupported.",
         call. = FALSE)
  }
  .scmix_state_positive(a$mu_bound, "mu_bound")
  .scmix_state_positive(a$kappa_bound, "kappa_bound")
  if (is_v2) .scmix_state_positive(a$alpha_bound, "alpha_bound")
  .scmix_state_positive(a$a_bound, "a_bound")
  .scmix_state_positive(a$weight_bound, "weight_bound")
  coefficient_scale <- .scmix_state_positive(
    a$coefficient_scale, "coefficient_scale", p)
  coefficient_names <- .scmix_state_names(
    bundle$coefficient_names, p, "b", "coefficient names")
  moderator_names <- .scmix_state_names(
    bundle$moderator_names, p_Z, "z", "moderator names")
  .scmix_state_preprocessing(
    bundle$preprocessing$Z, bundle$preprocessing$deltaX,
    moderator_names, coefficient_names, coefficient_scale)
  grid <- bundle$integration_grid
  if (!is.null(grid)) {
    U <- as.matrix(grid$U)
    w <- as.numeric(grid$w)
    if (!is.numeric(U) || ncol(U) != q || nrow(U) != length(w) ||
        !length(w) || any(!is.finite(U)) || any(!is.finite(w)) ||
        any(w <= 0)) {
      stop("The network-state integration grid is malformed.", call. = FALSE)
    }
  }
  state <- bundle$state_dict
  shapes <- bundle$state_shapes
  dtypes <- bundle$state_dtypes
  if (!is.list(state) || !length(state) || is.null(names(state)) ||
      !is.list(shapes) || !identical(names(state), names(shapes)) ||
      !is.list(dtypes) || !identical(names(state), names(dtypes))) {
    stop("The network-state state_dict or shape manifest is malformed.",
         call. = FALSE)
  }
  for (nm in names(state)) {
    value <- state[[nm]]
    shape <- as.integer(shapes[[nm]])
    dtype <- dtypes[[nm]]
    if (!is.numeric(value) || any(!is.finite(value)) || anyNA(shape) ||
        any(shape < 0L) || prod(shape) != length(value) ||
        !is.character(dtype) || length(dtype) != 1L || is.na(dtype) ||
        !nzchar(dtype)) {
      stop("Malformed serialized tensor `", nm, "`.", call. = FALSE)
    }
  }
  l002 <- bundle$loading_contract$gate_evidence$L002
  if (!is.null(l002)) {
    .scmix_validate_loading_invariance_evidence(l002)
    if (!identical(bundle$analysis_signature, l002$analysis_signature) ||
        !identical(bundle$scope, l002$scope) ||
        !identical(l002$network_state_hash,
                   .scmix_stable_state_hash(bundle$state_dict))) {
      stop("Portable-state L002 evidence is bound to another scope or state.",
           call. = FALSE)
    }
  }
  invisible(bundle)
}

#' Restore a portable scmix Torch network
#'
#' Reconstructs a fresh Torch module from the architecture and ordinary CPU
#' arrays stored in an `scmix_network_state`.  Unlike an `nn_module` embedded
#' directly in an RDS file, the returned predictor does not rely on a stale
#' external pointer from the fitting session.
#'
#' @param bundle A portable network state embedded by [scmix()] or
#'   [scmix_tune_matrix()].
#' @param device Torch device on which to return the reconstructed module.
#' @return A live Torch `nn_module` in evaluation mode.
#' @export
scmix_restore_network <- function(bundle, device = "cpu") {
  .scmix_validate_network_state(bundle)
  a <- bundle$architecture
  is_v1 <- identical(bundle$format_version, 1L)
  compute_dtype <- a$compute_dtype
  if (is.null(compute_dtype)) {
    ## States written before v2.2 did not record a module dtype. Preserve
    ## their Float behavior when the tensor manifest says Float; new states
    ## explicitly record float64 and never take this compatibility branch.
    recorded <- tolower(unlist(bundle$state_dtypes, use.names = FALSE))
    compute_dtype <- if (any(grepl("double|float64", recorded))) {
      "float64"
    } else "float32"
  }
  ## Building a module initializes parameters randomly. The local seed prevents
  ## a pure reload operation from advancing the caller's Torch RNG stream.
  net <- torch::with_torch_manual_seed({
    .sc_build_mixed_network(
      p = a$p, p_Z = a$p_Z, q = a$q, hidden = a$hidden,
      mean_family = if (is_v1) "legacy" else a$mean_family,
      mu_bound = a$mu_bound, kappa_bound = a$kappa_bound,
      alpha_bound = if (is_v1) 5 else a$alpha_bound,
      a_bound = a$a_bound, weight_bound = a$weight_bound,
      coefficient_scale = a$coefficient_scale,
      compute_dtype = compute_dtype
    )
  }, seed = 1L)
  target <- net$state_dict()
  if (!identical(names(target), names(bundle$state_dict))) {
    stop("Serialized state names do not match the reconstructed architecture.",
         call. = FALSE)
  }
  restored <- lapply(names(target), function(nm) {
    expected <- as.integer(target[[nm]]$shape)
    recorded <- as.integer(bundle$state_shapes[[nm]])
    if (!identical(expected, recorded)) {
      stop("Serialized tensor `", nm, "` has shape ",
           paste(recorded, collapse = "x"), "; expected ",
           paste(expected, collapse = "x"), ".", call. = FALSE)
    }
    value <- torch::torch_tensor(
      bundle$state_dict[[nm]], dtype = target[[nm]]$dtype,
      device = "cpu"
    )
    if (!identical(as.integer(value$shape), expected)) {
      stop("Serialized tensor `", nm,
           "` does not preserve its recorded array dimensions.", call. = FALSE)
    }
    value
  })
  names(restored) <- names(target)
  net$load_state_dict(restored)
  net$to(device = torch::torch_device(device))
  net$eval()
  net
}

.scmix_network_is_live <- function(net) {
  if (!inherits(net, "nn_module")) return(FALSE)
  ok <- try({
    state <- net$state_dict()
    if (!is.list(state) || !length(state)) stop("empty state")
    invisible(lapply(state, function(x) {
      x$shape
      torch::as_array(x$detach()$cpu())
    }))
    TRUE
  }, silent = TRUE)
  isTRUE(ok)
}

.scmix_resolve_network <- function(network_state = NULL, net = NULL,
                                   what = "network") {
  if (!is.null(network_state)) {
    return(scmix_restore_network(network_state))
  }
  if (.scmix_network_is_live(net)) return(net)
  stop(
    what, " has neither a portable network state nor a usable live Torch ",
    "module. This is a legacy serialized fit: its stored numeric predictions ",
    "remain usable, but new-moderator prediction requires a fixed selected-",
    "specification refit.", call. = FALSE
  )
}

#' Make a mixed-logit fit safe for RDS serialization
#'
#' Returns a copy in which session-local Torch modules are removed while the
#' portable network-state bundles and all stored numeric estimates and
#' predictions are retained. This is intended for disk artifacts, not for
#' resumable training: optimizer and random-number-generator states are not
#' serialized.
#'
#' @param object A mixed-logit fit or a nested object containing such fits.
#' @return A pointer-free copy suitable for `saveRDS()`.
#' @export
scmix_portable_copy <- function(object) {
  strip <- function(x) {
    if (inherits(x, "nn_module")) return(NULL)
    if (!is.list(x)) return(x)
    for (j in seq_along(x)) x[j] <- list(strip(x[[j]]))
    x
  }
  out <- strip(object)
  contains_module <- function(x) {
    if (inherits(x, "nn_module")) return(TRUE)
    is.list(x) && any(vapply(x, contains_module, logical(1L)))
  }
  if (contains_module(out)) {
    stop("Could not remove every live Torch module from the portable copy.",
         call. = FALSE)
  }
  out
}

#' Predict raw-scale conditional means from a portable scmix network
#'
#' @param bundle A portable `scmix_network_state`.
#' @param Z Numeric moderator matrix. By default it is on the raw scale used
#'   when fitting and is transformed using the stored training-only transform.
#' @param input Either `"raw"` or `"standardized"` moderator units.
#' @param output Either raw DeltaX coefficient units or the internal network
#'   units used after contrast scaling.
#' @param device Torch prediction device.
#' @return A numeric matrix with one conditional-mean row per row of `Z`.
#' @export
scmix_predict_network <- function(bundle, Z,
                                  input = c("raw", "standardized"),
                                  output = c("raw", "network"),
                                  device = "cpu") {
  .scmix_validate_network_state(bundle)
  input <- match.arg(input)
  output <- match.arg(output)
  Z <- as.matrix(Z)
  a <- bundle$architecture
  if (!is.numeric(Z) || ncol(Z) != a$p_Z || any(!is.finite(Z))) {
    stop("`Z` must be a finite numeric matrix with ", a$p_Z,
         " columns.", call. = FALSE)
  }
  expected_names <- bundle$moderator_names
  if (!is.null(colnames(Z))) {
    if (anyDuplicated(colnames(Z)) ||
        !setequal(colnames(Z), expected_names)) {
      stop("Named `Z` columns must match the stored moderator names.",
           call. = FALSE)
    }
    Z <- Z[, expected_names, drop = FALSE]
  }
  if (identical(input, "raw")) {
    tr <- bundle$preprocessing$Z
    if (!is.list(tr)) {
      stop("Raw-moderator prediction requires a stored Z transform.",
           call. = FALSE)
    }
    Z <- .sc_apply_z_transform(Z, tr)
  }
  net <- scmix_restore_network(bundle, device = device)
  ans <- .sc_predict_beta(net, Z)
  if (identical(output, "raw")) {
    ans <- sweep(ans, 2L, a$coefficient_scale, `/`)
  }
  colnames(ans) <- bundle$coefficient_names
  ans
}
