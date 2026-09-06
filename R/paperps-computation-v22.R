## Pure computation-contract artifacts for protocol v2.2.
##
## Expensive target recomputation remains in the application runner.  The
## functions below validate its signed evidence, calculate the frozen
## statistics, and emit fail-closed gate rows.  They never refit a model or
## silently supply a Step-3 cutoff.

.scmix_v22_comp_protocol_id <- "paperps-inference-2026-09-05-v2.2"
.scmix_v22_pending_cutoff <- "CALIBRATE_IN_STEP_3"

.scmix_v22_comp_scalar_text <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x))) {
    stop("`", what, "` must be one nonempty character value.",
         call. = FALSE)
  }
  x
}

.scmix_v22_comp_mode <- function(x) {
  match.arg(x, c("calibration", "production"))
}

.scmix_v22_comp_cutoff <- function(x, what) {
  if (is.null(x)) return(.scmix_v22_pending_cutoff)
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    stop("`", what, "` must be NULL or one finite nonnegative number.",
         call. = FALSE)
  }
  as.numeric(x)
}

.scmix_v22_comp_gate_state <- function(statistic, threshold,
                                        mode, applicable = TRUE) {
  if (!isTRUE(applicable)) return("not_applicable")
  if (is.character(threshold)) {
    return(if (identical(mode, "calibration")) "not_run" else "fail")
  }
  if (!is.numeric(statistic) || length(statistic) != 1L ||
      !is.finite(statistic)) return("fail")
  if (statistic <= threshold) "pass" else "fail"
}

.scmix_v22_comp_gate <- function(gate_id, scope, severity, state,
                                  statistic, threshold, units,
                                  provenance, message, scope_id) {
  category <- if (substr(gate_id, 1L, 1L) == "I") {
    "numerical_integration"
  } else "fit_computation"
  if (exists("scmix_v22_gate_record", mode = "function", inherits = TRUE)) {
    gate_constructor <- get(
      "scmix_v22_gate_record", mode = "function", inherits = TRUE)
    return(gate_constructor(
      gate_id, category, scope, severity, state, statistic, threshold,
      "less_than_or_equal", units, provenance, message, scope_id))
  }
  data.frame(
    gate_id = gate_id, category = category, scope = scope,
    severity = severity, state = state,
    statistic = I(list(statistic)), threshold = I(list(threshold)),
    comparator = "less_than_or_equal", units = units,
    provenance = provenance, message = message,
    scope_id = as.character(scope_id), stringsAsFactors = FALSE)
}

.scmix_v22_comp_bind_gates <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

.scmix_v22_comp_registered_starts <- function(x, scopes) {
  if (is.character(x)) {
    x <- stats::setNames(rep(list(x), length(scopes)), scopes)
  }
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !setequal(names(x), scopes)) {
    stop("`registered_starts` must be a character vector or a named list ",
         "with one entry for every fit scope.", call. = FALSE)
  }
  x <- x[scopes]
  valid <- vapply(x, function(ids) {
    is.character(ids) && length(ids) >= 1L && !anyNA(ids) &&
      all(nzchar(ids)) && !anyDuplicated(ids)
  }, logical(1L))
  if (!all(valid)) {
    stop("Every registered-start entry must contain unique nonempty IDs.",
         call. = FALSE)
  }
  x
}

#' Validate target stability across polished near-optimum starts
#'
#' This is the pure F008 calculation in frozen inference protocol v2.2.  The
#' caller must first recompute every registered target under every registered
#' polished start path.  For cross-fitted targets, a fit scope may represent a
#' one-fold substitution or another start path frozen by the application
#' registry.  The function does not construct such paths itself.
#'
#' @param evaluations A data frame with one row for each fit-scope, start, and
#'   target combination. Required columns are `fit_scope_id`, `start_id`,
#'   `state_hash`, `objective`, `objective_definition_id`, `polished`, `dtype`,
#'   `target_id`, `reported_estimate`, `standard_error`,
#'   `standard_error_definition_id`, `analysis_signature`, and `registry_hash`.
#'   The standard-error definition must be centered raw one-step respondent
#'   influence. `objective` is the minimized
#'   mean penalized negative complete-sequence log likelihood.
#' @param registered_targets Frozen target IDs.
#' @param registered_starts Either one vector of start IDs used in every fit
#'   scope or a named list indexed by fit scope.
#' @param near_optimum_objective_cutoff Step-3 cutoff for the relative
#'   objective gap. `NULL` leaves the statistic pending.
#' @param multistart_target_se_cutoff Step-3 F008 cutoff. `NULL` fails closed
#'   for production and remains visibly pending in calibration mode.
#' @param protocol_mode Either `"calibration"` or `"production"`.
#' @param protocol_id Frozen protocol ID.
#' @return A signed multistart evidence object with all relative objective
#'   gaps, target changes, targetwise maxima, and F008 gate rows.
#' @export
scmix_v22_multistart_stability <- function(
    evaluations, registered_targets, registered_starts,
    near_optimum_objective_cutoff = NULL,
    multistart_target_se_cutoff = NULL,
    protocol_mode = c("calibration", "production"),
    protocol_id = "paperps-inference-2026-09-05-v2.2") {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  protocol_id <- .scmix_v22_comp_scalar_text(protocol_id, "protocol_id")
  if (!identical(protocol_id, .scmix_v22_comp_protocol_id)) {
    stop("The protocol ID does not match frozen inference protocol v2.2.",
         call. = FALSE)
  }
  required <- c(
    "fit_scope_id", "start_id", "state_hash", "objective",
    "objective_definition_id", "polished", "dtype", "target_id",
    "reported_estimate", "standard_error", "standard_error_definition_id",
    "analysis_signature", "registry_hash")
  if (!is.data.frame(evaluations) || !all(required %in% names(evaluations)) ||
      !nrow(evaluations)) {
    stop("`evaluations` is missing required v2.2 multistart columns.",
         call. = FALSE)
  }
  ev <- evaluations[, required, drop = FALSE]
  text_columns <- setdiff(required, c(
    "objective", "polished", "reported_estimate", "standard_error"))
  if (any(vapply(ev[text_columns], function(x) {
    !is.character(x) || anyNA(x) || any(!nzchar(x))
  }, logical(1L)))) {
    stop("All multistart identifiers and hashes must be nonempty character values.",
         call. = FALSE)
  }
  if (!is.logical(ev$polished) || anyNA(ev$polished) ||
      !all(ev$polished)) {
    stop("Every registered start must be a completed polished state.",
         call. = FALSE)
  }
  if (any(ev$dtype != "float64")) {
    stop("Every registered start must have dtype `float64`.", call. = FALSE)
  }
  if (!is.numeric(ev$objective) || !is.numeric(ev$reported_estimate) ||
      !is.numeric(ev$standard_error) ||
      any(!is.finite(ev$objective)) ||
      any(!is.finite(ev$reported_estimate)) ||
      any(!is.finite(ev$standard_error)) || any(ev$standard_error <= 0)) {
    stop("Objectives, target estimates, and positive standard errors must be finite.",
         call. = FALSE)
  }
  registered_targets <- as.character(registered_targets)
  if (!length(registered_targets) || anyNA(registered_targets) ||
      any(!nzchar(registered_targets)) || anyDuplicated(registered_targets)) {
    stop("`registered_targets` must contain unique nonempty target IDs.",
         call. = FALSE)
  }
  if (!setequal(unique(ev$target_id), registered_targets)) {
    stop("Observed target IDs do not equal the frozen target registry.",
         call. = FALSE)
  }
  scopes <- unique(ev$fit_scope_id)
  starts <- .scmix_v22_comp_registered_starts(registered_starts, scopes)
  if (length(unique(ev$analysis_signature)) != 1L ||
      length(unique(ev$registry_hash)) != 1L ||
      length(unique(ev$objective_definition_id)) != 1L ||
      length(unique(ev$standard_error_definition_id)) != 1L) {
    stop("Analysis, registry, objective, and standard-error identifiers must be constant.",
         call. = FALSE)
  }
  if (!identical(unique(ev$standard_error_definition_id),
                 "raw_one_step_respondent_influence")) {
    stop("F008 standard errors must come from centered raw one-step respondent influence values.",
         call. = FALSE)
  }
  key <- paste(ev$fit_scope_id, ev$start_id, ev$target_id, sep = "\r")
  if (anyDuplicated(key)) {
    stop("Each fit-scope, start, and target combination must occur once.",
         call. = FALSE)
  }
  for (scope in scopes) {
    es <- ev[ev$fit_scope_id == scope, , drop = FALSE]
    if (!setequal(unique(es$start_id), starts[[scope]])) {
      stop("Observed starts do not equal the frozen registry in scope `",
           scope, "`.", call. = FALSE)
    }
    expected <- expand.grid(
      start_id = starts[[scope]], target_id = registered_targets,
      stringsAsFactors = FALSE)
    observed <- unique(es[, c("start_id", "target_id")])
    if (nrow(observed) != nrow(expected) ||
        !setequal(paste(observed$start_id, observed$target_id),
                  paste(expected$start_id, expected$target_id))) {
      stop("Every registered start must evaluate every registered target in scope `",
           scope, "`.", call. = FALSE)
    }
    invariant <- split(es, es$start_id)
    ok <- vapply(invariant, function(z) {
      length(unique(z$objective)) == 1L &&
        length(unique(z$state_hash)) == 1L &&
        length(unique(z$dtype)) == 1L &&
        length(unique(z$polished)) == 1L
    }, logical(1L))
    if (!all(ok)) {
      stop("Start-level state and objective fields vary across target rows.",
           call. = FALSE)
    }
  }

  near_cut <- .scmix_v22_comp_cutoff(
    near_optimum_objective_cutoff, "near_optimum_objective_cutoff")
  target_cut <- .scmix_v22_comp_cutoff(
    multistart_target_se_cutoff, "multistart_target_se_cutoff")
  details <- vector("list", length(scopes))
  names(details) <- scopes
  comparisons <- list()
  for (scope in scopes) {
    es <- ev[ev$fit_scope_id == scope, , drop = FALSE]
    order_ids <- starts[[scope]]
    q_by_start <- vapply(order_ids, function(id) {
      unique(es$objective[es$start_id == id])
    }, numeric(1L))
    q_star <- min(q_by_start)
    best <- order_ids[which(q_by_start == q_star)[1L]]
    relative_gap <- (q_by_start - q_star) / max(1, abs(q_star))
    names(relative_gap) <- order_ids
    near <- if (is.character(near_cut)) {
      stats::setNames(rep(NA, length(order_ids)), order_ids)
    } else relative_gap <= near_cut
    details[[scope]] <- data.frame(
      fit_scope_id = scope, start_id = order_ids,
      state_hash = vapply(order_ids, function(id) {
        unique(es$state_hash[es$start_id == id])
      }, character(1L)),
      objective = q_by_start, objective_relative_gap = relative_gap,
      best_start = order_ids == best, near_optimum = unname(near),
      stringsAsFactors = FALSE)
    for (target in registered_targets) {
      z <- es[es$target_id == target, , drop = FALSE]
      z <- z[match(order_ids, z$start_id), , drop = FALSE]
      star <- z[z$start_id == best, , drop = FALSE]
      change <- abs(z$reported_estimate - star$reported_estimate) /
        star$standard_error
      comparisons[[length(comparisons) + 1L]] <- data.frame(
        fit_scope_id = scope, target_id = target, start_id = order_ids,
        best_start_id = best, objective_relative_gap = relative_gap,
        near_optimum = unname(near),
        reported_estimate = z$reported_estimate,
        reference_estimate = star$reported_estimate,
        reference_standard_error = star$standard_error,
        target_change_over_se = change, stringsAsFactors = FALSE)
    }
  }
  start_details <- do.call(rbind, details)
  rownames(start_details) <- NULL
  comparison_table <- do.call(rbind, comparisons)
  rownames(comparison_table) <- NULL
  applicable <- any(lengths(starts) > 1L)
  target_statistic <- stats::setNames(rep(NA_real_, length(registered_targets)),
                                      registered_targets)
  if (!is.character(near_cut)) {
    target_statistic[] <- vapply(registered_targets, function(target) {
      z <- comparison_table[
        comparison_table$target_id == target &
          comparison_table$near_optimum, , drop = FALSE]
      if (!nrow(z)) NA_real_ else max(z$target_change_over_se)
    }, numeric(1L))
  }
  gates <- lapply(registered_targets, function(target) {
    state <- if (applicable && is.character(near_cut)) {
      if (identical(mode, "calibration")) "not_run" else "fail"
    } else .scmix_v22_comp_gate_state(
      target_statistic[[target]], target_cut, mode, applicable)
    message <- if (!applicable) {
      "F008 does not apply because exactly one production start is registered."
    } else if (is.character(near_cut)) {
      "The near-optimum objective cutoff remains CALIBRATE_IN_STEP_3."
    } else if (is.character(target_cut)) {
      "The multistart target-stability cutoff remains CALIBRATE_IN_STEP_3."
    } else {
      "The registered target is compared across every polished near-optimum start."
    }
    .scmix_v22_comp_gate(
      "F008", "target", "hard_target", state,
      target_statistic[[target]], target_cut, "standard_errors",
      "signed multistart target-recomputation artifact", message, target)
  })
  gates <- .scmix_v22_comp_bind_gates(gates)
  out <- list(
    protocol_id = protocol_id, protocol_mode = mode,
    analysis_signature = unique(ev$analysis_signature),
    registry_hash = unique(ev$registry_hash),
    objective_definition_id = unique(ev$objective_definition_id),
    standard_error_definition_id = unique(ev$standard_error_definition_id),
    registered_targets = registered_targets,
    registered_starts = starts,
    near_optimum_objective_cutoff = near_cut,
    multistart_target_se_cutoff = target_cut,
    start_details = start_details, comparisons = comparison_table,
    statistic_by_target = target_statistic, gates = gates,
    calibration_only = identical(mode, "calibration"),
    production_ready = identical(mode, "production") &&
      all(gates$state %in% c("pass", "not_applicable")),
    application_runner_note = paste(
      "The application runner, not this pure validator, must recompute each",
      "registered target under every registered polished start path."))
  class(out) <- c("scmix_v22_multistart_stability", "list")
  out
}

.scmix_v22_comp_named_numeric <- function(x, what, positive = FALSE) {
  if (!is.numeric(x) || !length(x) || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x)) ||
      any(!is.finite(x)) || (isTRUE(positive) && any(x <= 0))) {
    stop("`", what, "` must be a finite uniquely named numeric vector",
         if (positive) " with positive entries" else "", ".", call. = FALSE)
  }
  x
}

.scmix_v22_comp_norm2 <- function(x) sqrt(sum(as.numeric(x)^2))
.scmix_v22_comp_frobenius <- function(x) sqrt(sum(as.numeric(x)^2))

.scmix_v22_comp_relative <- function(numerator, denominator) {
  if (!is.finite(numerator) || !is.finite(denominator)) return(NA_real_)
  numerator / max(1, denominator)
}

.scmix_v22_comp_if_ratio <- function(a, b) {
  rms <- sqrt(mean((b - a)^2))
  sigma <- sqrt(mean((a - mean(a))^2))
  if (sigma == 0) {
    if (rms == 0) 0 else Inf
  } else rms / sigma
}

.scmix_v22_comp_validate_evaluation <- function(x, what,
                                                 require_se = FALSE) {
  required <- c(
    "protocol_id", "analysis_signature", "registry_hash", "fold_hash",
    "nuisance_selection_hash", "integration_rule", "parameter_state_hashes",
    "objective", "score", "covariance", "targets", "influence",
    "respondent_id", "role", "q", "objective_definition_id",
    "score_definition_id", "covariance_definition_id",
    "influence_definition_id", "riesz_solution_hashes",
    "riesz_integration_rule_hash", "uses_numerical_integration",
    "integrated_target_ids", "integrated_primitive_ids")
  if (!is.list(x) || !all(required %in% names(x))) {
    stop("`", what, "` is missing required v2.2 evaluation fields.",
         call. = FALSE)
  }
  for (nm in c("protocol_id", "analysis_signature", "registry_hash",
               "fold_hash", "nuisance_selection_hash", "role",
               "objective_definition_id", "score_definition_id",
               "covariance_definition_id", "influence_definition_id",
               "riesz_integration_rule_hash")) {
    .scmix_v22_comp_scalar_text(x[[nm]], paste0(what, "$", nm))
  }
  if (!identical(x$protocol_id, .scmix_v22_comp_protocol_id)) {
    stop("`", what, "` has the wrong protocol ID.", call. = FALSE)
  }
  if (!identical(x$influence_definition_id,
                 "raw_one_step_respondent_influence")) {
    stop("Integration influence values must be centered raw one-step respondent influence values.",
         call. = FALSE)
  }
  if (!is.logical(x$uses_numerical_integration) ||
      length(x$uses_numerical_integration) != 1L ||
      is.na(x$uses_numerical_integration)) {
    stop("`", what, "$uses_numerical_integration` must be one logical value.",
         call. = FALSE)
  }
  if (!is.numeric(x$q) || length(x$q) != 1L || is.na(x$q) ||
      x$q < 0 || x$q != as.integer(x$q)) {
    stop("`", what, "$q` must be one nonnegative integer.", call. = FALSE)
  }
  rule_required <- c("rule_id", "rule_hash", "method",
                     "nesting_supported")
  if (!is.list(x$integration_rule) ||
      !all(rule_required %in% names(x$integration_rule)) ||
      any(vapply(x$integration_rule[rule_required[1:3]], function(z)
        !is.character(z) || length(z) != 1L || is.na(z) || !nzchar(z),
        logical(1L))) ||
      !is.logical(x$integration_rule$nesting_supported) ||
      length(x$integration_rule$nesting_supported) != 1L ||
      is.na(x$integration_rule$nesting_supported)) {
    stop("`", what, "$integration_rule` is malformed.", call. = FALSE)
  }
  x$parameter_state_hashes <- .scmix_v22_comp_named_character(
    x$parameter_state_hashes, paste0(what, "$parameter_state_hashes"))
  x$objective <- .scmix_v22_comp_named_numeric(
    x$objective, paste0(what, "$objective"))
  if (!is.list(x$score) || is.null(names(x$score)) ||
      anyDuplicated(names(x$score)) ||
      !setequal(names(x$score), names(x$objective))) {
    stop("`", what, "$score` must be a named list matching objective scopes.",
         call. = FALSE)
  }
  x$score <- x$score[names(x$objective)]
  if (any(vapply(x$score, function(z)
    !is.numeric(z) || !length(z) || any(!is.finite(z)), logical(1L)))) {
    stop("Every score vector must be finite and nonempty.", call. = FALSE)
  }
  if (!is.list(x$covariance) || is.null(names(x$covariance)) ||
      anyDuplicated(names(x$covariance)) ||
      !setequal(names(x$covariance), names(x$objective))) {
    stop("`", what, "$covariance` must match objective scopes.", call. = FALSE)
  }
  x$covariance <- x$covariance[names(x$objective)]
  if (any(vapply(x$covariance, function(z) {
    !is.matrix(z) || !is.numeric(z) || nrow(z) != ncol(z) ||
      any(!is.finite(z))
  }, logical(1L)))) {
    stop("Every covariance entry must be a finite square matrix.",
         call. = FALSE)
  }
  x$targets <- .scmix_v22_comp_named_numeric(
    x$targets, paste0(what, "$targets"))
  if (isTRUE(require_se)) {
    x$standard_errors <- .scmix_v22_comp_named_numeric(
      x$standard_errors, paste0(what, "$standard_errors"), positive = TRUE)
    if (!setequal(names(x$standard_errors), names(x$targets))) {
      stop("Level-A standard errors must match its registered targets.",
           call. = FALSE)
    }
    x$standard_errors <- x$standard_errors[names(x$targets)]
  }
  if (!is.matrix(x$influence) || !is.numeric(x$influence) ||
      !nrow(x$influence) || !ncol(x$influence) ||
      any(!is.finite(x$influence)) || is.null(colnames(x$influence)) ||
      any(!nzchar(colnames(x$influence))) ||
      anyDuplicated(colnames(x$influence)) ||
      length(x$respondent_id) != nrow(x$influence) ||
      anyNA(x$respondent_id) || anyDuplicated(as.character(x$respondent_id))) {
    stop("Influence values need unique primitive columns and respondent rows.",
         call. = FALSE)
  }
  x$riesz_solution_hashes <- .scmix_v22_comp_named_character(
    x$riesz_solution_hashes, paste0(what, "$riesz_solution_hashes"))
  if (!setequal(names(x$riesz_solution_hashes), colnames(x$influence))) {
    stop("Riesz-solution hashes must name every influence primitive exactly.",
         call. = FALSE)
  }
  x$riesz_solution_hashes <- x$riesz_solution_hashes[colnames(x$influence)]
  if (!identical(x$riesz_integration_rule_hash,
                 x$integration_rule$rule_hash)) {
    stop("Riesz evidence was not recomputed under the declared integration rule.",
         call. = FALSE)
  }
  for (nm in c("integrated_target_ids", "integrated_primitive_ids")) {
    ids <- x[[nm]]
    if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids)) ||
        anyDuplicated(ids)) {
      stop("`", what, "$", nm, "` must contain unique nonempty IDs.",
           call. = FALSE)
    }
  }
  if (any(!x$integrated_target_ids %in% names(x$targets)) ||
      any(!x$integrated_primitive_ids %in% colnames(x$influence))) {
    stop("Integrated target or primitive IDs are outside the signed evaluation.",
         call. = FALSE)
  }
  if (!isTRUE(x$uses_numerical_integration) &&
      (length(x$integrated_target_ids) || length(x$integrated_primitive_ids))) {
    stop("A fit without numerical integration cannot register integrated targets or primitives.",
         call. = FALSE)
  }
  x
}

.scmix_v22_comp_named_character <- function(x, what) {
  if (!is.character(x) || !length(x) || is.null(names(x)) || anyNA(x) ||
      any(!nzchar(x)) || anyNA(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x))) {
    stop("`", what, "` must be a uniquely named nonempty character vector.",
         call. = FALSE)
  }
  x
}

.scmix_v22_comp_compare_shapes <- function(a, b, what) {
  if (!identical(names(a$objective), names(b$objective)) ||
      !identical(names(a$score), names(b$score)) ||
      !identical(names(a$covariance), names(b$covariance)) ||
      !identical(names(a$targets), names(b$targets)) ||
      !identical(colnames(a$influence), colnames(b$influence)) ||
      !identical(as.character(a$respondent_id),
                 as.character(b$respondent_id))) {
    stop("`", what, "` does not preserve scopes, targets, primitives, or respondents.",
         call. = FALSE)
  }
  for (nm in names(a$score)) {
    if (!identical(names(a$score[[nm]]), names(b$score[[nm]])) ||
        length(a$score[[nm]]) != length(b$score[[nm]])) {
      stop("Score coordinates differ in scope `", nm, "`.", call. = FALSE)
    }
  }
  for (nm in names(a$covariance)) {
    if (!identical(dim(a$covariance[[nm]]), dim(b$covariance[[nm]])) ||
        !identical(dimnames(a$covariance[[nm]]),
                   dimnames(b$covariance[[nm]]))) {
      stop("Covariance dimensions differ in scope `", nm, "`.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.scmix_v22_comp_branch_metrics <- function(a, b) {
  objective <- vapply(names(a$objective), function(nm) {
    .scmix_v22_comp_relative(
      abs(b$objective[[nm]] - a$objective[[nm]]), abs(a$objective[[nm]]))
  }, numeric(1L))
  score <- vapply(names(a$score), function(nm) {
    .scmix_v22_comp_relative(
      .scmix_v22_comp_norm2(b$score[[nm]] - a$score[[nm]]),
      .scmix_v22_comp_norm2(a$score[[nm]]))
  }, numeric(1L))
  covariance <- vapply(names(a$covariance), function(nm) {
    .scmix_v22_comp_relative(
      .scmix_v22_comp_frobenius(
        b$covariance[[nm]] - a$covariance[[nm]]),
      .scmix_v22_comp_frobenius(a$covariance[[nm]]))
  }, numeric(1L))
  target <- abs(b$targets - a$targets) / a$standard_errors
  influence <- vapply(colnames(a$influence), function(nm) {
    .scmix_v22_comp_if_ratio(a$influence[, nm], b$influence[, nm])
  }, numeric(1L))
  list(objective = objective, score = score, covariance = covariance,
       target = target, influence = influence)
}

.scmix_v22_comp_worse <- function(a, b) {
  Map(function(x, y) pmax(x, y), a, b)
}

#' Validate and compare the two required integration-refinement branches
#'
#' The function implements the frozen level-A versus level-B comparison after
#' an application runner has created both required branches. It rejects a
#' generic independent level-B refit. One branch must evaluate level B at the
#' exact fixed level-A parameter states. The other must begin from those same
#' states, polish under level B, and then recompute every supplied object.
#'
#' @param level_a,fixed_b,repolished_b Structured evaluation lists. All contain
#'   signed manifests, named full/fold objectives, normalized score vectors,
#'   covariance matrices, registered targets, and respondent-by-primitive
#'   influence matrices. `level_a` also contains named `standard_errors`.
#'   `fixed_b$parameter_state_hashes` must equal level A.
#'   `repolished_b$initial_parameter_state_hashes` must equal level A and
#'   `repolished_b$repolish_completed` must be `TRUE`.
#' @param target_primitives Named list mapping every target to its supporting
#'   influence primitive IDs.
#' @param calibration Named list of the five frozen integration cutoffs.
#' @param protocol_mode Either `"calibration"` or `"production"`.
#' @param protocol_id Frozen protocol ID.
#' @return A structured two-branch artifact, worse-of metrics, and I001--I005
#'   gate rows.
#' @export
scmix_v22_integration_compare <- function(
    level_a, fixed_b, repolished_b, target_primitives,
    calibration = list(),
    protocol_mode = c("calibration", "production"),
    protocol_id = "paperps-inference-2026-09-05-v2.2") {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  protocol_id <- .scmix_v22_comp_scalar_text(protocol_id, "protocol_id")
  if (!identical(protocol_id, .scmix_v22_comp_protocol_id)) {
    stop("The protocol ID does not match frozen inference protocol v2.2.",
         call. = FALSE)
  }
  a <- .scmix_v22_comp_validate_evaluation(level_a, "level_a", TRUE)
  f <- .scmix_v22_comp_validate_evaluation(fixed_b, "fixed_b", FALSE)
  r <- .scmix_v22_comp_validate_evaluation(repolished_b, "repolished_b", FALSE)
  if (!identical(a$role, "level_a_production") ||
      !identical(f$role, "level_b_fixed_parameters") ||
      !identical(r$role, "level_b_repolished")) {
    stop("Integration branch roles are not the frozen level-A/two-branch roles.",
         call. = FALSE)
  }
  manifest_fields <- c("analysis_signature", "registry_hash", "fold_hash",
                       "nuisance_selection_hash", "q",
                       "objective_definition_id", "score_definition_id",
                       "covariance_definition_id", "influence_definition_id",
                       "uses_numerical_integration", "integrated_target_ids",
                       "integrated_primitive_ids")
  for (nm in manifest_fields) {
    if (!identical(a[[nm]], f[[nm]]) || !identical(a[[nm]], r[[nm]])) {
      stop("Integration refinement changed the frozen `", nm, "`.",
           call. = FALSE)
    }
  }
  if (!identical(f$integration_rule$rule_hash,
                 r$integration_rule$rule_hash) ||
      identical(a$integration_rule$rule_hash,
                f$integration_rule$rule_hash)) {
    stop("Both branches must use one distinct signed level-B integration rule.",
         call. = FALSE)
  }
  if (!identical(a$integration_rule$method, f$integration_rule$method) ||
      !identical(a$integration_rule$method, r$integration_rule$method) ||
      !identical(a$integration_rule$nesting_supported,
                 f$integration_rule$nesting_supported) ||
      !identical(a$integration_rule$nesting_supported,
                 r$integration_rule$nesting_supported)) {
    stop("Integration refinement changed the signed integration method or nesting capability.",
         call. = FALSE)
  }
  if (isTRUE(a$integration_rule$nesting_supported)) {
    nested_hash_f <- f$integration_rule$nested_level_a_hash
    nested_hash_r <- r$integration_rule$nested_level_a_hash
    if (!is.character(nested_hash_f) || !is.character(nested_hash_r) ||
        !identical(nested_hash_f, a$integration_rule$rule_hash) ||
        !identical(nested_hash_r, a$integration_rule$rule_hash)) {
      stop("The signed level-B rule does not verify required nesting of level A.",
           call. = FALSE)
    }
  }
  if (!identical(a$parameter_state_hashes, f$parameter_state_hashes)) {
    stop("The fixed-parameter branch did not use the exact level-A states.",
         call. = FALSE)
  }
  if (!isTRUE(r$repolish_completed) ||
      is.null(r$initial_parameter_state_hashes) ||
      !identical(a$parameter_state_hashes,
                 r$initial_parameter_state_hashes)) {
    stop("The repolished branch did not start from the exact level-A states.",
         call. = FALSE)
  }
  if (!identical(names(a$parameter_state_hashes),
                 names(r$parameter_state_hashes))) {
    stop("Repolished level-B states do not match the full/fold fit scopes.",
         call. = FALSE)
  }
  .scmix_v22_comp_compare_shapes(a, f, "fixed_b")
  .scmix_v22_comp_compare_shapes(a, r, "repolished_b")
  if (!identical(names(a$parameter_state_hashes), names(a$objective))) {
    stop("Level-A parameter-state hashes must name every full/fold fit scope.",
         call. = FALSE)
  }
  if (!is.list(target_primitives) || is.null(names(target_primitives)) ||
      anyDuplicated(names(target_primitives)) ||
      !setequal(names(target_primitives), names(a$targets))) {
    stop("`target_primitives` must map every registered target exactly once.",
         call. = FALSE)
  }
  target_primitives <- target_primitives[names(a$targets)]
  primitive_ids <- colnames(a$influence)
  if (any(vapply(target_primitives, function(ids) {
    !is.character(ids) || !length(ids) || anyNA(ids) ||
      any(!ids %in% primitive_ids) || anyDuplicated(ids)
  }, logical(1L)))) {
    stop("Every target-to-primitive map must contain valid unique primitive IDs.",
         call. = FALSE)
  }
  if (!setequal(unique(unlist(target_primitives, use.names = FALSE)),
                primitive_ids)) {
    stop("The target-to-primitive maps must use every influence primitive exactly as a union.",
         call. = FALSE)
  }
  if (!is.list(calibration)) stop("`calibration` must be a named list.",
                                  call. = FALSE)
  cut <- function(name) .scmix_v22_comp_cutoff(calibration[[name]], name)
  target_cut <- cut("integration_target_se_cutoff")
  if_cut <- cut("integration_if_l2_cutoff")
  objective_cut <- cut("integration_objective_relative_cutoff")
  score_cut <- cut("integration_score_relative_cutoff")
  covariance_cut <- cut("integration_covariance_relative_cutoff")
  fixed_metrics <- .scmix_v22_comp_branch_metrics(a, f)
  repolished_metrics <- .scmix_v22_comp_branch_metrics(a, r)
  worse <- .scmix_v22_comp_worse(fixed_metrics, repolished_metrics)
  influence_by_target <- vapply(names(a$targets), function(id) {
    max(worse$influence[target_primitives[[id]]])
  }, numeric(1L))
  fit_max <- c(
    objective = max(worse$objective), score = max(worse$score),
    covariance = max(worse$covariance))
  gates <- list()
  for (id in names(a$targets)) {
    target_applicable <- isTRUE(a$uses_numerical_integration) &&
      id %in% a$integrated_target_ids
    influence_applicable <- isTRUE(a$uses_numerical_integration) &&
      any(target_primitives[[id]] %in% a$integrated_primitive_ids)
    gates[[length(gates) + 1L]] <- .scmix_v22_comp_gate(
      "I001", "target", "hard_target",
      .scmix_v22_comp_gate_state(worse$target[[id]], target_cut, mode,
                                 target_applicable),
      if (target_applicable) worse$target[[id]] else NA_real_, target_cut,
      "standard_errors",
      "signed two-branch integration refinement artifact",
      if (target_applicable) {
        "The target uses the worse fixed-parameter or repolished level-B discrepancy."
      } else "I001 is not applicable because this target uses no numerical integration.",
      id)
    gates[[length(gates) + 1L]] <- .scmix_v22_comp_gate(
      "I002", "target", "hard_target",
      .scmix_v22_comp_gate_state(influence_by_target[[id]], if_cut, mode,
                                 influence_applicable),
      if (influence_applicable) influence_by_target[[id]] else NA_real_,
      if_cut, "influence_standard_deviations",
      "signed two-branch integration refinement artifact",
      if (influence_applicable) {
        "The target uses the worst level-B discrepancy among its supporting influence primitives."
      } else "I002 is not applicable because its primitives use no numerical integration.",
      id)
  }
  fit_applicable <- isTRUE(a$uses_numerical_integration)
  gates[[length(gates) + 1L]] <- .scmix_v22_comp_gate(
    "I003", "fit", "hard_fit",
    .scmix_v22_comp_gate_state(fit_max[["objective"]], objective_cut, mode,
                               fit_applicable),
    if (fit_applicable) fit_max[["objective"]] else NA_real_, objective_cut,
    "relative_mean_objective",
    "signed two-branch integration refinement artifact",
    if (fit_applicable) {
      "The maximum objective discrepancy spans full and outer-fold fits and both branches."
    } else "I003 is not applicable because this fit uses no numerical integration.",
    "all_full_and_outer_fits")
  gates[[length(gates) + 1L]] <- .scmix_v22_comp_gate(
    "I004", "fit", "hard_fit",
    .scmix_v22_comp_gate_state(fit_max[["score"]], score_cut, mode,
                               fit_applicable),
    if (fit_applicable) fit_max[["score"]] else NA_real_, score_cut,
    "relative_normalized_gradient",
    "signed two-branch integration refinement artifact",
    if (fit_applicable) {
      "The maximum full-batch score discrepancy spans full and outer-fold fits and both branches."
    } else "I004 is not applicable because this fit uses no numerical integration.",
    "all_full_and_outer_fits")
  covariance_applicable <- fit_applicable && as.integer(a$q) > 0L
  covariance_state <- .scmix_v22_comp_gate_state(
    fit_max[["covariance"]], covariance_cut, mode, covariance_applicable)
  gates[[length(gates) + 1L]] <- .scmix_v22_comp_gate(
    "I005", "fit", "hard_fit", covariance_state,
    if (covariance_applicable) fit_max[["covariance"]] else NA_real_,
    covariance_cut, "relative_frobenius_norm",
    "signed two-branch integration refinement artifact",
    if (covariance_applicable) {
      "The maximum structural-covariance discrepancy spans full and outer-fold fits and both branches."
    } else "I005 is not applicable because q=0 or the fit uses no numerical integration.",
    "all_full_and_outer_fits")
  gates <- .scmix_v22_comp_bind_gates(gates)
  out <- list(
    protocol_id = protocol_id, protocol_mode = mode,
    analysis_signature = a$analysis_signature,
    registry_hash = a$registry_hash, fold_hash = a$fold_hash,
    nuisance_selection_hash = a$nuisance_selection_hash,
    level_a_rule = a$integration_rule,
    level_b_rule = f$integration_rule,
    fixed_parameter_metrics = fixed_metrics,
    repolished_metrics = repolished_metrics,
    worse_of_metrics = worse,
    influence_statistic_by_target = influence_by_target,
    maximum_fit_metrics = fit_max, target_primitives = target_primitives,
    gates = gates, calibration_only = identical(mode, "calibration"),
    production_ready = identical(mode, "production") &&
      all(gates$state %in% c("pass", "not_applicable")),
    branches = list(level_a = a, fixed_b = f, repolished_b = r),
    application_runner_note = paste(
      "The application runner must compute likelihood, score, covariance,",
      "targets, Riesz solutions, and influence values under both level-B branches."))
  class(out) <- c("scmix_v22_integration_refinement", "list")
  out
}

#' Orchestrate the two required integration-refinement branches
#'
#' This callback wrapper gives application runners one auditable path through
#' the fixed-parameter evaluation and the repolish-from-level-A evaluation.
#' Model fitting remains application-specific. The returned objects are
#' validated by [scmix_v22_integration_compare()].
#'
#' @param level_a Signed level-A evaluation artifact.
#' @param level_b_rule Signed higher-resolution rule passed unchanged to every
#'   callback.
#' @param fixed_parameter_evaluator Function of `level_a, level_b_rule`.
#' @param repolisher Function of `level_a, level_b_rule` that starts from the
#'   exact level-A parameter states.
#' @param repolished_evaluator Function of `repolished_fit, level_b_rule`.
#' @param ... Arguments passed to [scmix_v22_integration_compare()].
#' @return A `scmix_v22_integration_refinement` artifact.
#' @export
scmix_v22_integration_refinement <- function(
    level_a, level_b_rule, fixed_parameter_evaluator, repolisher,
    repolished_evaluator, ...) {
  callbacks <- list(fixed_parameter_evaluator, repolisher,
                    repolished_evaluator)
  if (any(!vapply(callbacks, is.function, logical(1L)))) {
    stop("All integration-refinement callbacks must be functions.",
         call. = FALSE)
  }
  if (!is.list(level_b_rule) ||
      !all(c("rule_id", "rule_hash") %in% names(level_b_rule))) {
    stop("`level_b_rule` must be a signed integration-rule list.",
         call. = FALSE)
  }
  fixed_b <- fixed_parameter_evaluator(level_a, level_b_rule)
  repolished_fit <- repolisher(level_a, level_b_rule)
  repolished_b <- repolished_evaluator(repolished_fit, level_b_rule)
  if (!is.list(fixed_b) || !is.list(repolished_b) ||
      !identical(fixed_b$integration_rule$rule_hash,
                 level_b_rule$rule_hash) ||
      !identical(repolished_b$integration_rule$rule_hash,
                 level_b_rule$rule_hash)) {
    stop("Integration callbacks did not use the supplied signed level-B rule.",
         call. = FALSE)
  }
  out <- scmix_v22_integration_compare(
    level_a = level_a, fixed_b = fixed_b,
    repolished_b = repolished_b, ...)
  out$callback_contract <- list(
    fixed_parameter_evaluator_calls = 1L,
    repolisher_calls = 1L, repolished_evaluator_calls = 1L,
    signed_level_b_rule = level_b_rule)
  out
}
