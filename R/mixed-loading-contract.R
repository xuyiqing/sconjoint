## Explicit loading-scale contract for paperps v2.2.
##
## The likelihood is evaluated after dividing every raw contrast coordinate by
## a training-sample scale d_j.  Consequently the network parameter is
## A_opt = diag(d) A_raw.  Structural quantities use A_raw.  Likelihood and
## network-state calculations use A_opt together with the standardized design.
## These helpers keep both representations explicit and make it impossible for
## downstream code to divide an already-raw loading by d a second time.

.scmix_loading_matrix <- function(x, p, q, what) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!identical(dim(x), c(as.integer(p), as.integer(q))) ||
      any(!is.finite(x))) {
    stop("`", what, "` must be a finite ", p, " by ", q,
         " loading matrix.", call. = FALSE)
  }
  x
}

.scmix_loading_scale <- function(x, p, what) {
  x <- as.numeric(x)
  if (length(x) != p || any(!is.finite(x)) || any(x <= 0)) {
    stop("`", what, "` must contain one finite positive value per loading row.",
         call. = FALSE)
  }
  x
}

.scmix_loading_to_optimization <- function(A_raw, coefficient_scale) {
  out <- sweep(as.matrix(A_raw), 1L, as.numeric(coefficient_scale), `*`)
  dimnames(out) <- dimnames(A_raw)
  out
}

.scmix_loading_to_raw <- function(A_optimization, coefficient_scale) {
  out <- sweep(as.matrix(A_optimization), 1L, as.numeric(coefficient_scale), `/`)
  dimnames(out) <- dimnames(A_optimization)
  out
}

.scmix_loading_invariance_fingerprint <- function(x) {
  x$evidence_fingerprint <- NULL
  .scmix_stable_state_hash(x)
}

.scmix_validate_loading_invariance_evidence <- function(x) {
  required <- c(
    "protocol_id", "analysis_signature", "scope", "protocol_mode",
    "calibration_hash", "network_state_hash", "evaluation_data_hash",
    "maximum_absolute_probability_difference",
    "maximum_absolute_sequence_loglik_difference", "threshold", "state",
    "pass", "evidence_fingerprint")
  if (!inherits(x, "scmix_loading_invariance_evidence") || !is.list(x) ||
      !all(required %in% names(x)) ||
      !identical(x$protocol_id, "paperps-inference-2026-09-05-v2.2") ||
      !x$protocol_mode %in% c("calibration", "production") ||
      !x$state %in% c("pass", "warn", "fail", "not_run",
                      "not_applicable") ||
      !is.numeric(x$maximum_absolute_probability_difference) ||
      length(x$maximum_absolute_probability_difference) != 1L ||
      !is.finite(x$maximum_absolute_probability_difference) ||
      !is.numeric(x$maximum_absolute_sequence_loglik_difference) ||
      length(x$maximum_absolute_sequence_loglik_difference) != 1L ||
      !is.finite(x$maximum_absolute_sequence_loglik_difference) ||
      !identical(x$evidence_fingerprint,
                 .scmix_loading_invariance_fingerprint(x))) {
    stop("Malformed or tampered L002 loading-invariance evidence.",
         call. = FALSE)
  }
  invisible(x)
}

## Evaluate the same fitted respondent-sequence model in optimizer coordinates
## and raw conjoint coordinates. This is intentionally performed on a live
## fitted network rather than asserted from the algebraic loading identity.
.scmix_loading_invariance_evidence <- function(
    net, deltaX_raw, y, Z_transformed, respondent_id, integration_grid,
    coefficient_scale, analysis_signature, scope,
    protocol_mode = c("calibration", "production"),
    calibration_verified = FALSE, calibration_hash = NA_character_,
    probability_cutoff = NULL, device = "cpu") {
  protocol_mode <- .sc_v22_protocol_mode(protocol_mode)
  deltaX_raw <- as.matrix(deltaX_raw)
  Z_transformed <- as.matrix(Z_transformed)
  y <- as.numeric(y)
  coefficient_scale <- .scmix_loading_scale(
    coefficient_scale, ncol(deltaX_raw), "coefficient_scale")
  if (nrow(deltaX_raw) != length(y) || nrow(Z_transformed) != length(y) ||
      length(respondent_id) != length(y) || any(!is.finite(deltaX_raw)) ||
      any(!is.finite(Z_transformed)) || any(!y %in% c(0, 1))) {
    stop("Malformed data for L002 loading-invariance evidence.", call. = FALSE)
  }
  deltaX_opt <- sweep(deltaX_raw, 2L, coefficient_scale, `/`)
  mu_opt <- .sc_predict_beta(net, Z_transformed)
  mu_raw <- sweep(mu_opt, 2L, coefficient_scale, `/`)
  q <- net$q
  A_opt <- if (q == 0L) matrix(numeric(), ncol(deltaX_raw), 0L) else
    as.matrix(torch::as_array(net$A$detach()$cpu()))
  A_raw <- .scmix_loading_to_raw(A_opt, coefficient_scale)
  kappa <- as.numeric(net$get_kappa()$detach()$cpu()$item())
  U <- as.matrix(integration_grid$U)
  w <- as.numeric(integration_grid$w)
  optimizer_index <- kappa + rowSums(deltaX_opt * mu_opt)
  raw_index <- kappa + rowSums(deltaX_raw * mu_raw)
  optimizer_factor <- deltaX_opt %*% A_opt %*% t(U)
  raw_factor <- deltaX_raw %*% A_raw %*% t(U)
  optimizer_eta <- sweep(optimizer_factor, 1L, optimizer_index, `+`)
  raw_eta <- sweep(raw_factor, 1L, raw_index, `+`)
  probability_difference <- max(abs(
    stats::plogis(optimizer_eta) - stats::plogis(raw_eta)))

  optimizer_sequence <- .sc_comp_sequence_loglik(
    net, deltaX_opt, y, Z_transformed, respondent_id, integration_grid,
    device = device)
  log_sigmoid <- function(x) -pmax(-x, 0) - log1p(exp(-abs(x)))
  raw_lp <- y * log_sigmoid(raw_eta) +
    (1 - y) * log_sigmoid(-raw_eta)
  respondent_levels <- unique(as.character(respondent_id))
  raw_sequence <- vapply(respondent_levels, function(id) {
    rows <- as.character(respondent_id) == id
    by_draw <- colSums(raw_lp[rows, , drop = FALSE])
    maximum <- max(by_draw)
    maximum + log(sum(w * exp(by_draw - maximum)))
  }, numeric(1L))
  sequence_difference <- max(abs(
    unname(optimizer_sequence[respondent_levels]) - raw_sequence))
  cutoff_available <- is.numeric(probability_cutoff) &&
    length(probability_cutoff) == 1L && is.finite(probability_cutoff) &&
    probability_cutoff >= 0
  gate_pass <- identical(protocol_mode, "production") &&
    isTRUE(calibration_verified) && cutoff_available &&
    probability_difference <= probability_cutoff
  out <- list(
    protocol_id = "paperps-inference-2026-09-05-v2.2",
    analysis_signature = analysis_signature, scope = scope,
    protocol_mode = protocol_mode,
    calibration_hash = calibration_hash,
    network_state_hash = .scmix_stable_state_hash(lapply(
      net$state_dict(), .scmix_tensor_cpu_array)),
    evaluation_data_hash = .scmix_stable_state_hash(list(
      deltaX_raw = deltaX_raw, y = y, Z_transformed = Z_transformed,
      respondent_id = as.character(respondent_id),
      integration_U = U, integration_w = w,
      coefficient_scale = coefficient_scale)),
    statistic = "fitted_probability_scale_invariance",
    maximum_absolute_probability_difference = probability_difference,
    maximum_absolute_sequence_loglik_difference = sequence_difference,
    threshold = if (cutoff_available) probability_cutoff else
      "CALIBRATE_IN_STEP_3",
    comparator = "less_than_or_equal",
    units = "maximum_absolute_probability_difference",
    provenance = paste(
      "live fitted network evaluated on the same respondent sequences in",
      "raw and optimization contrast coordinates"),
    pass = gate_pass,
    state = if (identical(protocol_mode, "calibration")) {
      "not_run"
    } else if (!isTRUE(calibration_verified) || !cutoff_available) {
      "fail"
    } else if (gate_pass) "pass" else "fail")
  class(out) <- c("scmix_loading_invariance_evidence", "list")
  out$evidence_fingerprint <- .scmix_loading_invariance_fingerprint(out)
  .scmix_validate_loading_invariance_evidence(out)
  out
}

.scmix_add_loading_contract <- function(object, q,
                                        raw_full = NULL,
                                        optimization_full = NULL,
                                        coefficient_scale_full = NULL,
                                        raw_folds = NULL,
                                        optimization_folds = NULL,
                                        coefficient_scale_folds = NULL,
                                        invariance_evidence = NULL,
                                        source = NULL) {
  q <- as.integer(q)
  p <- if (!is.null(coefficient_scale_full)) {
    length(coefficient_scale_full)
  } else if (length(coefficient_scale_folds)) {
    length(coefficient_scale_folds[[1L]])
  } else {
    stop("A loading contract requires a full or fold coefficient scale.",
         call. = FALSE)
  }
  tolerance <- 100 * .Machine$double.eps
  full_available <- !is.null(raw_full) || !is.null(optimization_full)
  if (full_available) {
    coefficient_scale_full <- .scmix_loading_scale(
      coefficient_scale_full, p, "coefficient_scale_full")
    if (is.null(raw_full)) {
      optimization_full <- .scmix_loading_matrix(
        optimization_full, p, q, "optimization_full")
      raw_full <- .scmix_loading_to_raw(
        optimization_full, coefficient_scale_full)
    } else {
      raw_full <- .scmix_loading_matrix(raw_full, p, q, "raw_full")
    }
    if (is.null(optimization_full)) {
      optimization_full <- .scmix_loading_to_optimization(
        raw_full, coefficient_scale_full)
    } else {
      optimization_full <- .scmix_loading_matrix(
        optimization_full, p, q, "optimization_full")
    }
  }

  fold_available <- !is.null(raw_folds) || !is.null(optimization_folds)
  if (fold_available) {
    K <- max(length(raw_folds), length(optimization_folds),
             length(coefficient_scale_folds))
    if (!is.list(coefficient_scale_folds) ||
        length(coefficient_scale_folds) != K ||
        (!is.null(raw_folds) && (!is.list(raw_folds) || length(raw_folds) != K)) ||
        (!is.null(optimization_folds) &&
         (!is.list(optimization_folds) || length(optimization_folds) != K))) {
      stop("Fold loading representations and scales must have the same length.",
           call. = FALSE)
    }
    coefficient_scale_folds <- lapply(seq_len(K), function(k) {
      .scmix_loading_scale(coefficient_scale_folds[[k]], p,
                           paste0("coefficient_scale_folds[[", k, "]]"))
    })
    if (is.null(raw_folds)) {
      raw_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_to_raw(
          .scmix_loading_matrix(optimization_folds[[k]], p, q,
                                paste0("optimization_folds[[", k, "]]")),
          coefficient_scale_folds[[k]])
      })
    } else {
      raw_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_matrix(raw_folds[[k]], p, q,
                              paste0("raw_folds[[", k, "]]"))
      })
    }
    if (is.null(optimization_folds)) {
      optimization_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_to_optimization(
          raw_folds[[k]], coefficient_scale_folds[[k]])
      })
    } else {
      optimization_folds <- lapply(seq_len(K), function(k) {
        .scmix_loading_matrix(optimization_folds[[k]], p, q,
                              paste0("optimization_folds[[", k, "]]"))
      })
    }
  } else {
    K <- 0L
  }

  errors <- numeric()
  if (full_available) {
    errors <- c(errors, max(abs(
      .scmix_loading_to_optimization(raw_full, coefficient_scale_full) -
        optimization_full), 0))
  }
  if (fold_available) {
    errors <- c(errors, vapply(seq_len(K), function(k) {
      max(abs(.scmix_loading_to_optimization(
        raw_folds[[k]], coefficient_scale_folds[[k]]) -
          optimization_folds[[k]]), 0)
    }, numeric(1L)))
  }
  max_error <- if (length(errors)) max(errors) else NA_real_
  reference <- c(
    if (full_available) as.numeric(optimization_full) else numeric(),
    if (fold_available) unlist(optimization_folds, use.names = FALSE) else numeric())
  round_trip_tolerance <- tolerance * max(1, abs(reference))
  round_trip_pass <- is.finite(max_error) &&
    max_error <= max(round_trip_tolerance)
  if (!round_trip_pass) {
    stop("Raw and optimization-scale loadings do not satisfy A_opt = diag(d) A_raw; ",
         "maximum discrepancy is ", signif(max_error, 8), ".", call. = FALSE)
  }

  object$A_raw_full <- if (full_available) raw_full else NULL
  object$A_optimization_full <- if (full_available) optimization_full else NULL
  object$A_raw_folds <- if (fold_available) raw_folds else NULL
  object$A_optimization_folds <- if (fold_available) optimization_folds else NULL
  evidence_list <- if (is.null(invariance_evidence)) list() else
    if (inherits(invariance_evidence, "scmix_loading_invariance_evidence")) {
      list(invariance_evidence)
    } else invariance_evidence
  if (!is.list(evidence_list)) {
    stop("`invariance_evidence` must be an L002 evidence object or list.",
         call. = FALSE)
  }
  if (length(evidence_list)) {
    lapply(evidence_list, .scmix_validate_loading_invariance_evidence)
  }
  l002_state <- if (!length(evidence_list)) "not_run" else if (
      all(vapply(evidence_list, function(x) identical(x$state, "pass"),
                 logical(1L)))) "pass" else if (
      all(vapply(evidence_list, function(x) identical(x$state, "not_run"),
                 logical(1L)))) "not_run" else "fail"
  l002_observed <- if (length(evidence_list)) max(vapply(
    evidence_list, `[[`, numeric(1L),
    "maximum_absolute_probability_difference")) else NA_real_
  object$loading_contract <- list(
    contract_id = "scmix-loading-scale-v1",
    contract_version = 1L,
    canonical_structural_scale = "raw",
    raw_to_optimization = "A_optimization = diag(coefficient_scale) %*% A_raw",
    design_to_optimization =
      "deltaX_optimization = deltaX_raw %*% diag(1 / coefficient_scale)",
    fitted_index_identity = paste(
      "deltaX_raw %*% A_raw equals deltaX_optimization %*%",
      "A_optimization for every integration draw"),
    full = list(
      available = full_available,
      raw_field = if (full_available) "A_raw_full" else NA_character_,
      optimization_field = if (full_available) "A_optimization_full" else
        NA_character_,
      coefficient_scale = if (full_available) coefficient_scale_full else NULL),
    folds = list(
      available = fold_available,
      count = as.integer(K),
      raw_field = if (fold_available) "A_raw_folds" else NA_character_,
      optimization_field = if (fold_available) "A_optimization_folds" else
        NA_character_,
      coefficient_scale = if (fold_available) coefficient_scale_folds else NULL),
    legacy_aliases = list(
      A_hat = "raw full-sample loading in scmix objects",
      A = "raw full-sample loading in scmix_tuned_matrix_fit objects",
      A_folds = "raw outer-fold loadings",
      A_computational_folds = paste(
        "legacy name for raw outer-fold loadings; do not infer scale from",
        "this alias")),
    source = source,
    gate_evidence = list(
      L001 = list(
        statistic = "raw_optimization_loading_round_trip",
        observed = round_trip_pass, threshold = TRUE, comparator = "equal",
        units = "boolean", provenance = "loading_contract_test",
        pass = round_trip_pass,
        state = if (round_trip_pass) "pass" else "fail"),
      L002 = list(
        statistic = "fitted_probability_scale_invariance",
        observed = l002_observed,
        threshold = if (length(evidence_list))
          evidence_list[[1L]]$threshold else "CALIBRATE_IN_STEP_3",
        comparator = "less_than_or_equal",
        units = "maximum_absolute_probability_difference",
        provenance = "loading_contract_test",
        pass = identical(l002_state, "pass"), state = l002_state,
        fit_scoped_evidence = evidence_list)),
    construction_check = list(
      max_round_trip_error = max_error,
      tolerance = max(round_trip_tolerance),
      pass = round_trip_pass)
  )
  object
}

.scmix_validate_loading_contract <- function(object) {
  contract <- object$loading_contract
  if (!is.list(contract) ||
      !identical(contract$contract_id, "scmix-loading-scale-v1") ||
      !identical(contract$contract_version, 1L)) {
    stop("The object lacks an explicit supported loading-scale contract. ",
         "Legacy loading aliases are not paper-eligible because their scale is ",
         "ambiguous. Refit or explicitly migrate the artifact.", call. = FALSE)
  }
  check_pair <- function(raw, optimization, coefficient_scale, label) {
    raw <- as.matrix(raw)
    optimization <- as.matrix(optimization)
    if (!identical(dim(raw), dim(optimization)) || any(!is.finite(raw)) ||
        any(!is.finite(optimization)) ||
        length(coefficient_scale) != nrow(raw) ||
        any(!is.finite(coefficient_scale)) || any(coefficient_scale <= 0)) {
      stop("Malformed ", label, " loading-scale contract.", call. = FALSE)
    }
    expected <- .scmix_loading_to_optimization(raw, coefficient_scale)
    tolerance <- 100 * .Machine$double.eps *
      max(1, abs(as.numeric(optimization)))
    if (max(abs(expected - optimization), 0) > max(tolerance)) {
      stop("The ", label, " loading contract fails its round-trip identity.",
           call. = FALSE)
    }
  }
  if (isTRUE(contract$full$available)) {
    check_pair(object[[contract$full$raw_field]],
               object[[contract$full$optimization_field]],
               contract$full$coefficient_scale, "full-sample")
  }
  if (isTRUE(contract$folds$available)) {
    raw <- object[[contract$folds$raw_field]]
    optimization <- object[[contract$folds$optimization_field]]
    scales <- contract$folds$coefficient_scale
    K <- contract$folds$count
    if (!is.list(raw) || !is.list(optimization) || !is.list(scales) ||
        length(raw) != K || length(optimization) != K ||
        length(scales) != K) {
      stop("Malformed fold loading-scale contract.", call. = FALSE)
    }
    for (k in seq_len(K)) {
      check_pair(raw[[k]], optimization[[k]], scales[[k]],
                 paste0("fold ", k))
    }
  }
  l002 <- contract$gate_evidence$L002$fit_scoped_evidence
  if (!is.null(l002)) {
    if (!is.list(l002)) {
      stop("Malformed L002 evidence in the loading contract.", call. = FALSE)
    }
    lapply(l002, .scmix_validate_loading_invariance_evidence)
  }
  invisible(contract)
}

#' Extract mixed-logit loadings on an explicit scale
#'
#' Returns the loading paired with either the raw conjoint contrasts or the
#' standardized contrasts used by the optimizer. New fits store both forms, a
#' versioned round-trip contract, and fit-scoped L002 evidence from evaluating
#' the live respondent-sequence model under equivalent preprocessing scales.
#' Legacy aliases such as `A`, `A_hat`, and `A_computational_folds` are
#' deliberately not guessed by this accessor.
#'
#' @param object An `scmix`, `scmix_tuned_matrix_fit`,
#'   `scmix_nested_assembled`, or portable `scmix_network_state` object.
#' @param scale Either `"raw"` or `"optimization"`.
#' @param scope Either `"full"` or `"fold"`.
#' @param fold Optional positive fold index. When omitted, all fold loadings
#'   are returned as a list.
#' @return A numeric loading matrix, or a list of matrices when all folds are
#'   requested. Attributes record the scale, scope, contract ID, and associated
#'   contrast scale.
#' @export
scmix_loadings <- function(object, scale = c("raw", "optimization"),
                           scope = c("full", "fold"), fold = NULL) {
  scale <- match.arg(scale)
  scope <- match.arg(scope)

  if (inherits(object, "scmix_network_state")) {
    .scmix_validate_network_state(object)
    if (scope != "full") {
      stop("A portable network state contains one loading; use scope = 'full'.",
           call. = FALSE)
    }
    a <- object$architecture
    coefficient_scale <- .scmix_loading_scale(
      a$coefficient_scale, a$p, "architecture$coefficient_scale")
    optimization <- if (a$q == 0L) {
      matrix(numeric(0), a$p, 0L)
    } else {
      .scmix_loading_matrix(object$state_dict$A, a$p, a$q, "state_dict$A")
    }
    value <- if (scale == "optimization") optimization else
      .scmix_loading_to_raw(optimization, coefficient_scale)
    rownames(value) <- object$coefficient_names
    attr(value, "loading_contract_id") <- "scmix-loading-scale-v1"
    attr(value, "loading_scale") <- scale
    attr(value, "loading_scope") <- "network_state"
    attr(value, "coefficient_scale") <- coefficient_scale
    return(value)
  }

  contract <- .scmix_validate_loading_contract(object)
  branch <- contract[[if (scope == "full") "full" else "folds"]]
  if (!isTRUE(branch$available)) {
    stop("The requested loading scope is not available in this object.",
         call. = FALSE)
  }
  field <- branch[[if (scale == "raw") "raw_field" else
    "optimization_field"]]
  value <- object[[field]]
  coefficient_scale <- branch$coefficient_scale
  if (scope == "fold") {
    if (!is.null(fold)) {
      if (!is.numeric(fold) || length(fold) != 1L || is.na(fold) ||
          fold != as.integer(fold) || fold < 1L || fold > length(value)) {
        stop("`fold` must identify one available fold.", call. = FALSE)
      }
      value <- value[[as.integer(fold)]]
      coefficient_scale <- coefficient_scale[[as.integer(fold)]]
    }
  } else if (!is.null(fold)) {
    stop("`fold` is only used with scope = 'fold'.", call. = FALSE)
  }
  attr(value, "loading_contract_id") <- contract$contract_id
  attr(value, "loading_scale") <- scale
  attr(value, "loading_scope") <- scope
  attr(value, "coefficient_scale") <- coefficient_scale
  value
}
