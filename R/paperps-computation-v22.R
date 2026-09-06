## Pure computation-contract artifacts for protocol v2.2.
##
## Expensive target recomputation remains in the application runner.  The
## functions below validate its signed evidence, calculate the frozen
## statistics, and emit fail-closed gate rows.  They never refit a model or
## silently supply a Step-3 cutoff.

.scmix_v22_comp_protocol_id <- "paperps-inference-2026-09-05-v2.2"
.scmix_v22_pending_cutoff <- "CALIBRATE_IN_STEP_3"

.scmix_v22_comp_fingerprint <- function(x) {
  path <- tempfile("scmix-v22-comp-fingerprint-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 3, compress = FALSE)
  unname(as.character(tools::md5sum(path)))
}

.scmix_v22_comp_sign <- function(payload, class_name) {
  payload$fingerprint <- .scmix_v22_comp_fingerprint(payload)
  class(payload) <- c(class_name, "list")
  payload
}

.scmix_v22_comp_intact <- function(x, class_name) {
  if (!inherits(x, class_name) || !is.list(x)) return(FALSE)
  payload <- unclass(x)
  recorded <- payload$fingerprint
  payload$fingerprint <- NULL
  is.character(recorded) && length(recorded) == 1L && !is.na(recorded) &&
    identical(recorded, .scmix_v22_comp_fingerprint(payload))
}

.scmix_v22_comp_public_intact <- function(x, class_name) {
  if (exists(".scmix_v22_intact", mode = "function", inherits = TRUE)) {
    checker <- get(".scmix_v22_intact", mode = "function", inherits = TRUE)
    return(isTRUE(checker(x, class_name)))
  }
  .scmix_v22_comp_intact(x, class_name)
}

.scmix_v22_comp_registry <- function(registry) {
  if (!is.list(registry) || !length(registry) ||
      !all(vapply(registry, .scmix_v22_comp_public_intact, logical(1L),
                  class_name = "scmix_v22_target_spec"))) {
    stop("`registry` must contain intact fingerprinted v2.2 target specifications.",
         call. = FALSE)
  }
  ids <- vapply(registry, `[[`, character(1L), "target_id")
  if (anyDuplicated(ids)) stop("Target registry IDs must be unique.",
                               call. = FALSE)
  registry <- registry[order(ids, method = "radix")]
  list(payload = registry,
       fingerprint = .scmix_v22_comp_fingerprint(registry))
}

.scmix_v22_comp_contract <- function(analysis_contract) {
  if (!.scmix_v22_comp_public_intact(
      analysis_contract, "scmix_v22_analysis_contract")) {
    stop("`analysis_contract` must be an intact fingerprinted v2.2 contract.",
         call. = FALSE)
  }
  analysis_contract
}

#' Sign the frozen numerical-integration contract
#'
#' @param q Structural rank.
#' @param method Registered integration method.
#' @param uses_numerical_integration Whether the fit uses numerical integration.
#' @param level_a_rule_id,level_b_rule_id Frozen rule IDs.
#' @param level_a_resolution,level_b_resolution Numeric resolutions. Level B
#'   must be strictly larger whenever numerical integration is used.
#' @param level_a_rule_hash,level_b_rule_hash Immutable rule hashes.
#' @param nesting_supported Whether this registered method supports nesting.
#' @param nesting_evidence_hash Required immutable evidence when nesting is
#'   supported.
#' @param covariance_dimension Dimension of the structural covariance matrix,
#'   normally the number of coded attributes. It is not the factor rank.
#' @return An intact fingerprinted integration contract.
#' @export
scmix_v22_integration_contract <- function(
    q, method, uses_numerical_integration,
    level_a_rule_id, level_b_rule_id,
    level_a_resolution, level_b_resolution,
    level_a_rule_hash, level_b_rule_hash,
    nesting_supported, nesting_evidence_hash = NULL,
    covariance_dimension) {
  if (!is.numeric(q) || length(q) != 1L || is.na(q) || q < 0 ||
      q != as.integer(q)) stop("`q` must be one nonnegative integer.",
                               call. = FALSE)
  if (!is.logical(uses_numerical_integration) ||
      length(uses_numerical_integration) != 1L ||
      is.na(uses_numerical_integration) || !is.logical(nesting_supported) ||
      length(nesting_supported) != 1L || is.na(nesting_supported)) {
    stop("Integration-use and nesting predicates must be nonmissing logical values.",
         call. = FALSE)
  }
  method <- .scmix_v22_comp_scalar_text(method, "method")
  text <- c(level_a_rule_id = level_a_rule_id,
            level_b_rule_id = level_b_rule_id,
            level_a_rule_hash = level_a_rule_hash,
            level_b_rule_hash = level_b_rule_hash)
  text <- stats::setNames(vapply(seq_along(text), function(i)
    .scmix_v22_comp_scalar_text(text[[i]], names(text)[i]), character(1L)),
    names(text))
  resolution <- c(level_a = level_a_resolution, level_b = level_b_resolution)
  if (!is.numeric(resolution) || any(!is.finite(resolution)) ||
      any(resolution <= 0) ||
      (isTRUE(uses_numerical_integration) && resolution[["level_b"]] <=
         resolution[["level_a"]])) {
    stop("Numerical level B must have a strictly higher positive resolution than level A.",
         call. = FALSE)
  }
  if (!is.numeric(covariance_dimension) || length(covariance_dimension) != 1L ||
      is.na(covariance_dimension) || covariance_dimension < 1L ||
      covariance_dimension != as.integer(covariance_dimension)) {
    stop("`covariance_dimension` must be one positive integer.",
         call. = FALSE)
  }
  if (isTRUE(nesting_supported)) {
    nesting_evidence_hash <- .scmix_v22_comp_scalar_text(
      nesting_evidence_hash, "nesting_evidence_hash")
  } else if (!is.null(nesting_evidence_hash)) {
    stop("Nesting evidence may be supplied only when nesting is registered as supported.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id, q = as.integer(q),
    method = method,
    uses_numerical_integration = uses_numerical_integration,
    level_a = list(rule_id = unname(text[["level_a_rule_id"]]),
                   resolution = unname(resolution[["level_a"]]),
                   rule_hash = unname(text[["level_a_rule_hash"]])),
    level_b = list(rule_id = unname(text[["level_b_rule_id"]]),
                   resolution = unname(resolution[["level_b"]]),
                   rule_hash = unname(text[["level_b_rule_hash"]])),
    nesting_supported = nesting_supported,
    nesting_evidence_hash = nesting_evidence_hash,
    covariance_dimension = as.integer(covariance_dimension)),
    "scmix_v22_integration_contract")
}

#' Sign the exact fit-scope and optimizer-start registry
#'
#' @param application_id,specification_id,fit_id Frozen fit identifiers.
#' @param outer_fold_ids Complete registered outer-fold IDs.
#' @param starts A data frame with `fit_scope_id`, `start_id`, and integer
#'   `seed`. Scope IDs must equal the returned full and outer-fold convention.
#' @param integration_contract An intact signed integration contract.
#' @return A fingerprinted fit-scope registry suitable for the
#'   `optimizer_starts` field of `scmix_v22_analysis_contract()`.
#' @export
scmix_v22_fit_scope_registry <- function(
    application_id, specification_id, fit_id, outer_fold_ids, starts,
    integration_contract) {
  text <- c(application_id = application_id,
            specification_id = specification_id, fit_id = fit_id)
  text <- stats::setNames(vapply(seq_along(text), function(i)
    .scmix_v22_comp_scalar_text(text[[i]], names(text)[i]), character(1L)),
    names(text))
  if (!.scmix_v22_comp_intact(
      integration_contract, "scmix_v22_integration_contract")) {
    stop("`integration_contract` must be intact and fingerprinted.",
         call. = FALSE)
  }
  outer_fold_ids <- as.character(outer_fold_ids)
  if (!length(outer_fold_ids) || anyNA(outer_fold_ids) ||
      any(!nzchar(outer_fold_ids)) || anyDuplicated(outer_fold_ids)) {
    stop("`outer_fold_ids` must contain every unique nonempty outer-fold ID.",
         call. = FALSE)
  }
  scope_ids <- c(
    paste(text[["specification_id"]], "full", sep = "::"),
    paste(text[["specification_id"]], "outer", outer_fold_ids, sep = "::"))
  required <- c("fit_scope_id", "start_id", "seed")
  if (!is.data.frame(starts) || !all(required %in% names(starts)) ||
      !nrow(starts) || anyNA(starts[, required])) {
    stop("`starts` must contain complete fit-scope, start-ID, and seed columns.",
         call. = FALSE)
  }
  starts <- starts[, required, drop = FALSE]
  starts$fit_scope_id <- as.character(starts$fit_scope_id)
  starts$start_id <- as.character(starts$start_id)
  if (!is.numeric(starts$seed) || any(!is.finite(starts$seed)) ||
      any(starts$seed != as.integer(starts$seed)) ||
      any(!nzchar(starts$start_id)) ||
      anyDuplicated(paste(starts$fit_scope_id, starts$start_id))) {
    stop("Start IDs and integer seeds must be valid and unique within scope.",
         call. = FALSE)
  }
  if (!setequal(unique(starts$fit_scope_id), scope_ids)) {
    stop("Start registry must include the full fit and every registered outer fold.",
         call. = FALSE)
  }
  counts <- table(starts$fit_scope_id)
  if (length(unique(as.integer(counts))) != 1L) {
    stop("Every fit scope must register the same number of starts.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    application_id = unname(text[["application_id"]]),
    specification_id = unname(text[["specification_id"]]),
    fit_id = unname(text[["fit_id"]]),
    q = integration_contract$q,
    integration_contract_fingerprint = integration_contract$fingerprint,
    uses_numerical_integration =
      integration_contract$uses_numerical_integration,
    outer_fold_ids = sort(outer_fold_ids, method = "radix"),
    fit_scope_ids = sort(scope_ids, method = "radix"),
    starts = starts[order(starts$fit_scope_id, starts$start_id,
                          method = "radix"), , drop = FALSE]),
    "scmix_v22_fit_scope_registry")
}

.scmix_v22_comp_cutoff_names <- c(
  "kkt_cutoff", "integration_target_se_cutoff",
  "integration_if_l2_cutoff", "integration_objective_relative_cutoff",
  "integration_score_relative_cutoff",
  "integration_covariance_relative_cutoff",
  "near_optimum_objective_cutoff", "multistart_target_se_cutoff",
  "nested_fit_relative_cutoff")

#' Sign the Step-3 computation calibration constants
#'
#' @param analysis_contract Intact signed analysis contract.
#' @param cutoffs Named numeric list containing every frozen computation cutoff.
#' @param provenance Immutable calibration provenance.
#' @return A fingerprinted computation-calibration contract.
#' @export
scmix_v22_computation_calibration <- function(analysis_contract, cutoffs,
                                               provenance) {
  analysis_contract <- .scmix_v22_comp_contract(analysis_contract)
  if (!is.list(cutoffs) || is.null(names(cutoffs)) ||
      !identical(sort(names(cutoffs), method = "radix"),
                 sort(.scmix_v22_comp_cutoff_names, method = "radix"))) {
    stop("`cutoffs` must contain every and only the frozen computation cutoff.",
         call. = FALSE)
  }
  cutoffs <- cutoffs[.scmix_v22_comp_cutoff_names]
  if (any(!vapply(cutoffs, function(x)
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0,
    logical(1L)))) {
    stop("Every computation cutoff must be one finite nonnegative number.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    calibration_hash = analysis_contract$calibration_hash,
    cutoffs = lapply(cutoffs, as.numeric),
    provenance = .scmix_v22_comp_scalar_text(provenance, "provenance")),
    "scmix_v22_computation_calibration")
}

.scmix_v22_comp_bind_contracts <- function(analysis_contract, registry,
                                            integration_contract,
                                            calibration_contract = NULL,
                                            require_calibration = FALSE) {
  analysis_contract <- .scmix_v22_comp_contract(analysis_contract)
  registry_info <- .scmix_v22_comp_registry(registry)
  if (!.scmix_v22_comp_intact(
      integration_contract, "scmix_v22_integration_contract")) {
    stop("`integration_contract` must be intact and fingerprinted.",
         call. = FALSE)
  }
  fit_registry <- analysis_contract$optimizer_starts
  if (!.scmix_v22_comp_intact(
      fit_registry, "scmix_v22_fit_scope_registry")) {
    stop("The analysis contract must contain an intact fit-scope/start registry.",
         call. = FALSE)
  }
  fold_ids <- if (is.list(analysis_contract$outer_folds) &&
      !is.null(names(analysis_contract$outer_folds)) &&
      all(nzchar(names(analysis_contract$outer_folds)))) {
    names(analysis_contract$outer_folds)
  } else sort(unique(as.character(unlist(
    analysis_contract$outer_folds, use.names = FALSE))), method = "radix")
  bindings <- c(
    identical(fit_registry$application_id, analysis_contract$application_id),
    identical(fit_registry$specification_id,
              analysis_contract$specification_id),
    identical(fit_registry$outer_fold_ids,
              sort(fold_ids, method = "radix")),
    identical(fit_registry$integration_contract_fingerprint,
              integration_contract$fingerprint),
    identical(analysis_contract$integration_rule,
              integration_contract$fingerprint),
    identical(fit_registry$q, integration_contract$q),
    identical(fit_registry$uses_numerical_integration,
              integration_contract$uses_numerical_integration))
  if (!all(bindings)) {
    stop("Analysis, fit-scope, rank, folds, and integration contracts are not exactly bound.",
         call. = FALSE)
  }
  calibration <- NULL
  if (!is.null(calibration_contract)) {
    if (!.scmix_v22_comp_intact(
        calibration_contract, "scmix_v22_computation_calibration") ||
        !identical(calibration_contract$analysis_contract_fingerprint,
                   analysis_contract$fingerprint) ||
        !identical(calibration_contract$calibration_hash,
                   analysis_contract$calibration_hash)) {
      stop("The computation calibration is not intact or bound to the exact analysis calibration hash.",
           call. = FALSE)
    }
    calibration <- calibration_contract
  } else if (isTRUE(require_calibration)) {
    stop("Production mode requires the intact Step-3 computation calibration.",
         call. = FALSE)
  }
  list(analysis = analysis_contract, registry = registry_info$payload,
       registry_fingerprint = registry_info$fingerprint,
       integration = integration_contract, fit_registry = fit_registry,
       calibration = calibration)
}

#' Sign optimizer and polish evidence for one registered start
#'
#' @param analysis_contract,registry,integration_contract Intact frozen
#'   contracts.
#' @param fit_scope_id,start_id,seed Registered start identifiers.
#' @param state_hash Immutable portable-state hash.
#' @param objective Minimized frozen mean objective.
#' @param objective_definition_id Frozen objective ID.
#' @param dtype Must be `"float64"`.
#' @param polish_completed,nonworsening Nonmissing polish attestations.
#' @param objective_before_polish,objective_after_polish Finite objectives.
#' @param kkt_by_block Named finite projected-KKT values.
#' @param line_search_status Nonempty final line-search status.
#' @return An intact per-start optimizer evidence object.
#' @export
scmix_v22_start_optimizer_evidence <- function(
    analysis_contract, registry, integration_contract,
    fit_scope_id, start_id, seed, state_hash, objective,
    objective_definition_id, dtype, polish_completed, nonworsening,
    objective_before_polish, objective_after_polish, kkt_by_block,
    line_search_status) {
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract)
  fit_scope_id <- .scmix_v22_comp_scalar_text(fit_scope_id, "fit_scope_id")
  start_id <- .scmix_v22_comp_scalar_text(start_id, "start_id")
  row <- bound$fit_registry$starts[
    bound$fit_registry$starts$fit_scope_id == fit_scope_id &
      bound$fit_registry$starts$start_id == start_id, , drop = FALSE]
  if (nrow(row) != 1L || !is.numeric(seed) || length(seed) != 1L ||
      seed != as.integer(seed) || !identical(as.integer(seed),
                                          as.integer(row$seed))) {
    stop("Start ID, fit scope, and seed do not match the signed start registry.",
         call. = FALSE)
  }
  nums <- c(objective = objective,
            objective_before_polish = objective_before_polish,
            objective_after_polish = objective_after_polish)
  if (any(!is.finite(nums)) || !is.numeric(kkt_by_block) ||
      !length(kkt_by_block) || is.null(names(kkt_by_block)) ||
      any(!is.finite(kkt_by_block)) || any(kkt_by_block < 0) ||
      anyDuplicated(names(kkt_by_block)) || any(!nzchar(names(kkt_by_block)))) {
    stop("Objective and named KKT evidence must be finite and complete.",
         call. = FALSE)
  }
  if (!is.logical(polish_completed) || length(polish_completed) != 1L ||
      is.na(polish_completed) || !is.logical(nonworsening) ||
      length(nonworsening) != 1L || is.na(nonworsening)) {
    stop("Polish and nonworsening evidence must be nonmissing logical values.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    application_id = bound$analysis$application_id,
    specification_id = bound$analysis$specification_id,
    fit_id = bound$fit_registry$fit_id, q = bound$fit_registry$q,
    uses_numerical_integration =
      bound$fit_registry$uses_numerical_integration,
    fit_scope_id = fit_scope_id, start_id = start_id,
    seed = as.integer(seed),
    state_hash = .scmix_v22_comp_scalar_text(state_hash, "state_hash"),
    objective = as.numeric(objective),
    objective_definition_id = .scmix_v22_comp_scalar_text(
      objective_definition_id, "objective_definition_id"),
    dtype = .scmix_v22_comp_scalar_text(dtype, "dtype"),
    polish_completed = polish_completed, nonworsening = nonworsening,
    objective_before_polish = as.numeric(objective_before_polish),
    objective_after_polish = as.numeric(objective_after_polish),
    kkt_by_block = kkt_by_block,
    line_search_status = .scmix_v22_comp_scalar_text(
      line_search_status, "line_search_status")),
    "scmix_v22_start_optimizer_evidence")
}

#' Sign target recomputation at one registered optimizer start
#'
#' @param optimizer_evidence Intact start evidence.
#' @param analysis_contract,registry,integration_contract Intact contracts.
#' @param reported_estimates,standard_errors Named values in exact target
#'   registry order.
#' @return A fingerprinted target-recomputation artifact.
#' @export
scmix_v22_target_recomputation <- function(
    optimizer_evidence, analysis_contract, registry, integration_contract,
    reported_estimates, standard_errors) {
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract)
  if (!.scmix_v22_comp_intact(
      optimizer_evidence, "scmix_v22_start_optimizer_evidence") ||
      !identical(optimizer_evidence$analysis_contract_fingerprint,
                 bound$analysis$fingerprint) ||
      !identical(optimizer_evidence$registry_fingerprint,
                 bound$registry_fingerprint) ||
      !identical(optimizer_evidence$integration_contract_fingerprint,
                 bound$integration$fingerprint)) {
    stop("`optimizer_evidence` is not intact or contract-linked.",
         call. = FALSE)
  }
  target_ids <- vapply(bound$registry, `[[`, character(1L), "target_id")
  reported_estimates <- .scmix_v22_comp_named_numeric(
    reported_estimates, "reported_estimates")
  standard_errors <- .scmix_v22_comp_named_numeric(
    standard_errors, "standard_errors", positive = TRUE)
  if (!identical(names(reported_estimates), target_ids) ||
      !identical(names(standard_errors), target_ids)) {
    stop("Target estimates and raw-IF standard errors must use exact signed registry order.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    optimizer_evidence_fingerprint = optimizer_evidence$fingerprint,
    fit_id = bound$fit_registry$fit_id,
    specification_id = bound$analysis$specification_id,
    fit_scope_id = optimizer_evidence$fit_scope_id,
    start_id = optimizer_evidence$start_id,
    state_hash = optimizer_evidence$state_hash,
    reported_estimates = reported_estimates,
    standard_errors = standard_errors,
    standard_error_definition_id =
      "centered_raw_one_step_respondent_sequence_influence"),
    "scmix_v22_target_recomputation")
}

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
#' @param evaluations Intact fingerprinted target-recomputation artifacts.
#' @param optimizer_evidence Intact fingerprinted optimizer/polish evidence,
#'   one object for every signed fit-scope and start pair.
#' @param analysis_contract,registry,integration_contract Intact frozen
#'   contracts.
#' @param calibration_contract Intact Step-3 computation calibration. It is
#'   required in production mode and must match the exact analysis calibration
#'   hash.
#' @param protocol_mode Either `"calibration"` or `"production"`.
#' @return A signed multistart evidence object with all relative objective
#'   gaps, target changes, targetwise maxima, and F008 gate rows.
#' @export
scmix_v22_multistart_stability <- function(
    evaluations, optimizer_evidence, analysis_contract, registry,
    integration_contract, calibration_contract = NULL,
    protocol_mode = c("calibration", "production")) {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    calibration_contract, require_calibration = identical(mode, "production"))
  if (!is.list(optimizer_evidence) || !length(optimizer_evidence) ||
      !all(vapply(optimizer_evidence, .scmix_v22_comp_intact, logical(1L),
                  class_name = "scmix_v22_start_optimizer_evidence"))) {
    stop("`optimizer_evidence` must contain intact signed per-start artifacts.",
         call. = FALSE)
  }
  if (!is.list(evaluations) || !length(evaluations) ||
      !all(vapply(evaluations, .scmix_v22_comp_intact, logical(1L),
                  class_name = "scmix_v22_target_recomputation"))) {
    stop("`evaluations` must contain intact signed target recomputations.",
         call. = FALSE)
  }
  expected <- bound$fit_registry$starts
  evidence_key <- vapply(optimizer_evidence, function(x)
    paste(x$fit_scope_id, x$start_id, sep = "\r"), character(1L))
  expected_key <- paste(expected$fit_scope_id, expected$start_id, sep = "\r")
  if (anyDuplicated(evidence_key) || !setequal(evidence_key, expected_key)) {
    stop("Optimizer evidence must cover every and only the signed fit-scope/start registry.",
         call. = FALSE)
  }
  optimizer_evidence <- optimizer_evidence[match(expected_key, evidence_key)]
  objective_id <- paste0(
    "mean_negative_complete_sequence_loglik_per_respondent_",
    "plus_declared_penalty_in_nats")
  for (i in seq_along(optimizer_evidence)) {
    x <- optimizer_evidence[[i]]
    row <- expected[i, ]
    links <- c(
      identical(x$analysis_contract_fingerprint, bound$analysis$fingerprint),
      identical(x$registry_fingerprint, bound$registry_fingerprint),
      identical(x$integration_contract_fingerprint,
                bound$integration$fingerprint),
      identical(x$fit_registry_fingerprint, bound$fit_registry$fingerprint),
      identical(x$fit_id, bound$fit_registry$fit_id),
      identical(x$specification_id, bound$analysis$specification_id),
      identical(x$q, bound$integration$q),
      identical(x$uses_numerical_integration,
                bound$integration$uses_numerical_integration),
      identical(x$fit_scope_id, row$fit_scope_id),
      identical(x$start_id, row$start_id),
      identical(x$seed, as.integer(row$seed)),
      identical(x$objective_definition_id, objective_id),
      identical(x$dtype, "float64"), isTRUE(x$polish_completed),
      isTRUE(x$nonworsening),
      is.finite(x$objective_before_polish),
      is.finite(x$objective_after_polish),
      x$objective_after_polish <= x$objective_before_polish,
      is.numeric(x$kkt_by_block) && length(x$kkt_by_block) > 0L &&
        all(is.finite(x$kkt_by_block)),
      is.character(x$line_search_status) && length(x$line_search_status) == 1L &&
        nzchar(x$line_search_status))
    if (!all(links)) {
      stop("Per-start optimizer, polish, seed, KKT, or contract evidence is inconsistent.",
           call. = FALSE)
    }
  }
  recompute_key <- vapply(evaluations, function(x)
    paste(x$fit_scope_id, x$start_id, sep = "\r"), character(1L))
  if (anyDuplicated(recompute_key) || !setequal(recompute_key, expected_key)) {
    stop("Target recomputations must cover every signed fit-scope/start pair.",
         call. = FALSE)
  }
  evaluations <- evaluations[match(expected_key, recompute_key)]
  registered_targets <- vapply(
    bound$registry, `[[`, character(1L), "target_id")
  rows <- vector("list", length(evaluations))
  for (i in seq_along(evaluations)) {
    x <- evaluations[[i]]; opt <- optimizer_evidence[[i]]
    links <- c(
      identical(x$analysis_contract_fingerprint, bound$analysis$fingerprint),
      identical(x$registry_fingerprint, bound$registry_fingerprint),
      identical(x$integration_contract_fingerprint,
                bound$integration$fingerprint),
      identical(x$optimizer_evidence_fingerprint, opt$fingerprint),
      identical(x$fit_id, bound$fit_registry$fit_id),
      identical(x$specification_id, bound$analysis$specification_id),
      identical(x$fit_scope_id, opt$fit_scope_id),
      identical(x$start_id, opt$start_id),
      identical(x$state_hash, opt$state_hash),
      identical(names(x$reported_estimates), registered_targets),
      identical(names(x$standard_errors), registered_targets),
      identical(x$standard_error_definition_id,
                "centered_raw_one_step_respondent_sequence_influence"))
    if (!all(links) || any(!is.finite(x$reported_estimates)) ||
        any(!is.finite(x$standard_errors)) || any(x$standard_errors <= 0)) {
      stop("Target recomputation is incomplete or not linked to its exact optimizer state.",
           call. = FALSE)
    }
    rows[[i]] <- data.frame(
      fit_scope_id = opt$fit_scope_id, start_id = opt$start_id,
      state_hash = opt$state_hash, objective = opt$objective,
      target_id = registered_targets,
      reported_estimate = unname(x$reported_estimates),
      standard_error = unname(x$standard_errors), stringsAsFactors = FALSE)
  }
  ev <- do.call(rbind, rows); rownames(ev) <- NULL
  scopes <- bound$fit_registry$fit_scope_ids
  starts <- split(expected$start_id, expected$fit_scope_id)
  starts <- starts[scopes]
  near_cut <- if (is.null(bound$calibration)) .scmix_v22_pending_cutoff else
    bound$calibration$cutoffs$near_optimum_objective_cutoff
  target_cut <- if (is.null(bound$calibration)) .scmix_v22_pending_cutoff else
    bound$calibration$cutoffs$multistart_target_se_cutoff
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
    protocol_id = .scmix_v22_comp_protocol_id, protocol_mode = mode,
    analysis_signature = bound$analysis$analysis_signature,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    calibration_contract_fingerprint = if (is.null(bound$calibration)) NULL else
      bound$calibration$fingerprint,
    calibration_hash = bound$analysis$calibration_hash,
    fit_id = bound$fit_registry$fit_id,
    specification_id = bound$analysis$specification_id,
    q = bound$integration$q,
    objective_definition_id = objective_id,
    standard_error_definition_id =
      "centered_raw_one_step_respondent_sequence_influence",
    registered_targets = registered_targets,
    registered_starts = starts,
    near_optimum_objective_cutoff = near_cut,
    multistart_target_se_cutoff = target_cut,
    start_details = start_details, comparisons = comparison_table,
    statistic_by_target = target_statistic, gates = gates,
    calibration_only = identical(mode, "calibration"),
    selected_gate_subset_passed = identical(mode, "production") &&
      all(gates$state %in% c("pass", "not_applicable")),
    application_runner_note = paste(
      "The application runner, not this pure validator, must recompute each",
      "registered target under every registered polished start path."))
  .scmix_v22_comp_sign(out, "scmix_v22_multistart_stability")
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
  if (!is.finite(sigma) || sigma <= 0 || !is.finite(rms)) return(NA_real_)
  rms / sigma
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
  if (!.scmix_v22_comp_intact(x, "scmix_v22_integration_evaluation")) {
    stop("`", what, "` must be an intact fingerprinted integration evaluation.",
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
      nrow(z) < 1L || any(!is.finite(z)) || is.null(rownames(z)) ||
      is.null(colnames(z)) || any(!nzchar(rownames(z))) ||
      any(!nzchar(colnames(z))) || anyDuplicated(rownames(z)) ||
      anyDuplicated(colnames(z)) || !identical(rownames(z), colnames(z)) ||
      max(abs(z - t(z))) > sqrt(.Machine$double.eps) * max(1, max(abs(z))) ||
      min(eigen((z + t(z)) / 2, symmetric = TRUE,
                only.values = TRUE)$values) <
        -sqrt(.Machine$double.eps) * max(1, max(abs(z)))
  }, logical(1L)))) {
    stop("Every covariance entry must be nonempty, named, symmetric, and positive semidefinite.",
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
      (id %in% a$integrated_target_ids ||
       any(target_primitives[[id]] %in% a$integrated_primitive_ids))
    influence_applicable <- target_applicable
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
    a$specification_id)
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
    a$specification_id)
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
    a$specification_id)
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
    selected_gate_subset_passed = identical(mode, "production") &&
      all(gates$state %in% c("pass", "not_applicable")),
    branches = list(level_a = a, fixed_b = f, repolished_b = r),
    application_runner_note = paste(
      "The application runner must compute likelihood, score, covariance,",
      "targets, Riesz solutions, and influence values under both level-B branches."))
  .scmix_v22_comp_sign(out, "scmix_v22_integration_refinement")
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
