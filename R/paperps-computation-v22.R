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

.scmix_v22_comp_state_hash <- function(state_dict, A_optimization, kappa) {
  bytes <- as.integer(serialize(
    list(state_dict = state_dict, A_optimization = A_optimization,
         kappa = kappa), NULL, ascii = FALSE, version = 2L))
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
  if (any(vapply(registry, function(x)
    !identical(x$protocol_id, .scmix_v22_comp_protocol_id) ||
      !is.character(x$target_family_id) ||
      length(x$target_family_id) != 1L || !nzchar(x$target_family_id) ||
      !is.character(x$primitive_registry_ids) ||
      !length(x$primitive_registry_ids) || anyNA(x$primitive_registry_ids) ||
      any(!nzchar(x$primitive_registry_ids)) ||
      anyDuplicated(x$primitive_registry_ids), logical(1L)))) {
    stop("Target specifications must contain valid protocol, family, and primitive IDs.",
         call. = FALSE)
  }
  families <- split(registry, vapply(
    registry, `[[`, character(1L), "target_family_id"))
  for (fam in families) {
    observed_targets <- sort(vapply(fam, `[[`, character(1L), "target_id"),
                             method = "radix")
    observed_primitives <- sort(unique(unlist(lapply(
      fam, `[[`, "primitive_registry_ids"), use.names = FALSE)),
      method = "radix")
    if (any(vapply(fam, function(x)
      !identical(sort(x$family_target_ids, method = "radix"),
                 observed_targets) ||
        !identical(sort(x$family_primitive_registry_ids, method = "radix"),
                   observed_primitives), logical(1L)))) {
      stop("Every target must carry the complete signed family target and primitive registry.",
           call. = FALSE)
    }
  }
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

#' Sign the structural parameter contract used by computation diagnostics
#'
#' The structural covariance in the integration gate is indexed by the
#' utility-coefficient coordinates declared here.  Its dimension is therefore
#' `p` by `p`; the factor rank `q` is recorded separately and never substitutes
#' for `p`.
#'
#' @param application_id,specification_id Frozen identifiers.
#' @param q Prespecified nonnegative factor rank.
#' @param covariance_coordinate_ids Ordered raw-scale utility-coefficient IDs.
#' @param score_coordinate_ids Ordered structural-orthonormal score IDs.
#' @return An intact fingerprinted structural parameter contract.
#' @export
scmix_v22_structural_parameter_contract <- function(
    application_id, specification_id, q, covariance_coordinate_ids,
    score_coordinate_ids) {
  application_id <- .scmix_v22_comp_scalar_text(
    application_id, "application_id")
  specification_id <- .scmix_v22_comp_scalar_text(
    specification_id, "specification_id")
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q < 0 ||
      q != as.integer(q)) {
    stop("`q` must be one nonnegative integer.", call. = FALSE)
  }
  ids <- function(x, what) {
    if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x)) ||
        anyDuplicated(x)) {
      stop("`", what, "` must be a nonempty ordered vector of unique IDs.",
           call. = FALSE)
    }
    unname(x)
  }
  covariance_coordinate_ids <- ids(
    covariance_coordinate_ids, "covariance_coordinate_ids")
  score_coordinate_ids <- ids(score_coordinate_ids, "score_coordinate_ids")
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    application_id = application_id,
    specification_id = specification_id,
    q = as.integer(q),
    covariance_coordinate_ids = covariance_coordinate_ids,
    covariance_dimension = length(covariance_coordinate_ids),
    score_coordinate_ids = score_coordinate_ids,
    score_dimension = length(score_coordinate_ids),
    covariance_scale = "raw_utility_coefficient_scale",
    score_scale = "structural_orthonormal_coordinates"),
    "scmix_v22_structural_parameter_contract")
}

.scmix_v22_comp_node_keys <- function(nodes) {
  apply(nodes, 1L, function(z) paste(sprintf("%.17g", z), collapse = "\r"))
}

#' Sign an exact numerical-integration rule
#'
#' @param rule_id,method Frozen rule identifier and method.
#' @param level Either `"a"` or `"b"`.
#' @param nodes Finite matrix containing the exact ordered nodes or draws.
#' @param weights Optional finite weights in the same row order.
#' @param nesting_supported Whether this method permits an exactly nested rule.
#' @return An intact fingerprinted integration-rule artifact.
#' @export
scmix_v22_integration_rule <- function(
    rule_id, method, level = c("a", "b"), nodes, weights = NULL,
    nesting_supported) {
  level <- match.arg(level)
  rule_id <- .scmix_v22_comp_scalar_text(rule_id, "rule_id")
  method <- .scmix_v22_comp_scalar_text(method, "method")
  if (!is.matrix(nodes) || !is.numeric(nodes) || nrow(nodes) < 1L ||
      ncol(nodes) < 1L || any(!is.finite(nodes))) {
    stop("`nodes` must be a nonempty finite numeric matrix.", call. = FALSE)
  }
  if (!is.null(weights) && (!is.numeric(weights) ||
      length(weights) != nrow(nodes) || any(!is.finite(weights)) ||
      any(weights <= 0) ||
      abs(sum(weights) - 1) > sqrt(.Machine$double.eps))) {
    stop("`weights` must be NULL or positive finite weights summing to one.",
         call. = FALSE)
  }
  if (!is.logical(nesting_supported) || length(nesting_supported) != 1L ||
      is.na(nesting_supported)) {
    stop("`nesting_supported` must be one nonmissing logical value.",
         call. = FALSE)
  }
  node_keys <- .scmix_v22_comp_node_keys(nodes)
  if (anyDuplicated(node_keys)) {
    stop("Integration nodes or draws must be unique.", call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    rule_id = rule_id, method = method, level = level,
    resolution = nrow(nodes), integration_dimension = ncol(nodes),
    nodes = unname(nodes),
    weights = if (is.null(weights)) NULL else unname(weights),
    node_keys = unname(node_keys),
    node_hash = .scmix_v22_comp_fingerprint(unname(nodes)),
    weight_hash = if (is.null(weights)) NULL else
      .scmix_v22_comp_fingerprint(unname(weights)),
    nesting_supported = nesting_supported),
    "scmix_v22_integration_rule")
}

#' Sign the frozen numerical-integration contract
#'
#' @param structural_parameter_contract Intact signed structural contract.
#' @param registry Intact signed target specifications.
#' @param uses_numerical_integration Whether the fit uses integration.
#' @param level_a_rule,level_b_rule Intact exact rule artifacts.
#' @param integrated_target_ids,integrated_primitive_ids Prespecified IDs whose
#'   target or primitive calculation uses numerical integration.
#' @param nonnesting_reason Required when numerical integration is used but the
#'   registered method does not support nesting.
#' @return An intact fingerprinted integration contract.
#' @export
scmix_v22_integration_contract <- function(
    structural_parameter_contract, registry, uses_numerical_integration,
    level_a_rule, level_b_rule, integrated_target_ids,
    integrated_primitive_ids, nonnesting_reason = NULL) {
  if (!.scmix_v22_comp_intact(
      structural_parameter_contract,
      "scmix_v22_structural_parameter_contract")) {
    stop("`structural_parameter_contract` must be intact and fingerprinted.",
         call. = FALSE)
  }
  registry_info <- .scmix_v22_comp_registry(registry)
  if (!is.logical(uses_numerical_integration) ||
      length(uses_numerical_integration) != 1L ||
      is.na(uses_numerical_integration)) {
    stop("`uses_numerical_integration` must be one logical value.",
         call. = FALSE)
  }
  if (!.scmix_v22_comp_intact(level_a_rule, "scmix_v22_integration_rule") ||
      !.scmix_v22_comp_intact(level_b_rule, "scmix_v22_integration_rule")) {
    stop("Both integration rules must be intact signed artifacts.",
         call. = FALSE)
  }
  if (!identical(level_a_rule$level, "a") ||
      !identical(level_b_rule$level, "b") ||
      !identical(level_a_rule$method, level_b_rule$method) ||
      !identical(level_a_rule$integration_dimension,
                 level_b_rule$integration_dimension) ||
      !identical(is.null(level_a_rule$weight_hash),
                 is.null(level_b_rule$weight_hash)) ||
      !identical(level_a_rule$nesting_supported,
                 level_b_rule$nesting_supported)) {
    stop("Level A and B must be signed, distinct rules for the same method and dimension.",
         call. = FALSE)
  }
  if (identical(level_a_rule$fingerprint, level_b_rule$fingerprint)) {
    stop("Level B must be distinct from level A.", call. = FALSE)
  }
  target_ids <- vapply(registry_info$payload, `[[`, character(1L),
                       "target_id")
  primitive_ids <- sort(unique(unlist(lapply(
    registry_info$payload, `[[`, "primitive_registry_ids"),
    use.names = FALSE)), method = "radix")
  clean_subset <- function(ids, universe, what) {
    if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids)) ||
        anyDuplicated(ids) || any(!ids %in% universe)) {
      stop("`", what, "` must be unique IDs in the signed registry.",
           call. = FALSE)
    }
    sort(unname(ids), method = "radix")
  }
  integrated_target_ids <- clean_subset(
    integrated_target_ids, target_ids, "integrated_target_ids")
  integrated_primitive_ids <- clean_subset(
    integrated_primitive_ids, primitive_ids, "integrated_primitive_ids")
  nested <- all(level_a_rule$node_keys %in% level_b_rule$node_keys)
  if (isTRUE(uses_numerical_integration)) {
    if (structural_parameter_contract$q < 1L ||
        level_a_rule$integration_dimension !=
          structural_parameter_contract$q) {
      stop("Numerical integration dimension must equal the signed factor rank q.",
           call. = FALSE)
    }
    if (level_b_rule$resolution <= level_a_rule$resolution) {
      stop("Signed level B must have strictly higher resolution than level A.",
           call. = FALSE)
    }
    if (!length(integrated_target_ids) && !length(integrated_primitive_ids)) {
      stop("A numerical fit must prespecify integrated targets or primitives.",
           call. = FALSE)
    }
    if (isTRUE(level_a_rule$nesting_supported) && !nested) {
      stop("The signed level-B nodes do not exactly contain every level-A node.",
           call. = FALSE)
    }
    if (!isTRUE(level_a_rule$nesting_supported)) {
      nonnesting_reason <- .scmix_v22_comp_scalar_text(
        nonnesting_reason, "nonnesting_reason")
    } else if (!is.null(nonnesting_reason)) {
      stop("`nonnesting_reason` is allowed only for a nonnesting method.",
           call. = FALSE)
    }
  } else {
    if (length(integrated_target_ids) || length(integrated_primitive_ids)) {
      stop("A nonintegrated fit cannot register integrated IDs.",
           call. = FALSE)
    }
    nonnesting_reason <- NULL
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    application_id = structural_parameter_contract$application_id,
    specification_id = structural_parameter_contract$specification_id,
    q = structural_parameter_contract$q,
    structural_parameter_contract_fingerprint =
      structural_parameter_contract$fingerprint,
    covariance_coordinate_ids =
      structural_parameter_contract$covariance_coordinate_ids,
    covariance_dimension =
      structural_parameter_contract$covariance_dimension,
    score_coordinate_ids = structural_parameter_contract$score_coordinate_ids,
    registry_fingerprint = registry_info$fingerprint,
    uses_numerical_integration = uses_numerical_integration,
    method = level_a_rule$method,
    level_a_rule = level_a_rule,
    level_b_rule = level_b_rule,
    nesting_supported = level_a_rule$nesting_supported,
    exact_nesting_verified = isTRUE(level_a_rule$nesting_supported) && nested,
    nonnesting_reason = nonnesting_reason,
    integrated_target_ids = integrated_target_ids,
    integrated_primitive_ids = integrated_primitive_ids),
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
    structural_parameter_contract_fingerprint =
      integration_contract$structural_parameter_contract_fingerprint,
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

.scmix_v22_comp_validate_calibration <- function(
    calibration_artifact, analysis_contract, require_complete = TRUE) {
  if (is.null(calibration_artifact)) return(FALSE)
  if (exists(".scmix_v22_validate_calibration", mode = "function",
             inherits = TRUE)) {
    validator <- get(".scmix_v22_validate_calibration", mode = "function",
                     inherits = TRUE)
    return(isTRUE(validator(calibration_artifact, analysis_contract,
                            require_complete = require_complete)))
  }
  .scmix_v22_comp_public_intact(
    calibration_artifact, "scmix_v22_calibration_artifact") &&
    identical(calibration_artifact$protocol_id,
              .scmix_v22_comp_protocol_id) &&
    identical(calibration_artifact$analysis_signature,
              analysis_contract$analysis_signature) &&
    identical(calibration_artifact$analysis_contract_fingerprint,
              analysis_contract$fingerprint) &&
    identical(calibration_artifact$calibration_hash,
              analysis_contract$calibration_hash) &&
    is.character(calibration_artifact$source_hash) &&
    length(calibration_artifact$source_hash) == 1L &&
    is.list(calibration_artifact$constants) &&
    all(.scmix_v22_comp_cutoff_names %in%
          names(calibration_artifact$constants)) &&
    all(vapply(calibration_artifact$constants, function(x)
      is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0,
      logical(1L))) &&
    identical(calibration_artifact$calibration_hash,
              .scmix_v22_comp_fingerprint(list(
                protocol_id = .scmix_v22_comp_protocol_id,
                source_hash = calibration_artifact$source_hash,
                constants = calibration_artifact$constants[
                  sort(names(calibration_artifact$constants),
                       method = "radix")]))) &&
    (!isTRUE(require_complete) || isTRUE(calibration_artifact$complete))
}

.scmix_v22_comp_bind_contracts <- function(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, calibration_artifact = NULL,
    require_calibration = FALSE) {
  analysis_contract <- .scmix_v22_comp_contract(analysis_contract)
  registry_info <- .scmix_v22_comp_registry(registry)
  if (!.scmix_v22_comp_intact(
      integration_contract, "scmix_v22_integration_contract")) {
    stop("`integration_contract` must be intact and fingerprinted.",
         call. = FALSE)
  }
  if (!.scmix_v22_comp_intact(
      structural_parameter_contract,
      "scmix_v22_structural_parameter_contract")) {
    stop("`structural_parameter_contract` must be intact and fingerprinted.",
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
    identical(integration_contract$registry_fingerprint,
              registry_info$fingerprint),
    identical(integration_contract$structural_parameter_contract_fingerprint,
              structural_parameter_contract$fingerprint),
    identical(fit_registry$structural_parameter_contract_fingerprint,
              structural_parameter_contract$fingerprint),
    identical(structural_parameter_contract$application_id,
              analysis_contract$application_id),
    identical(structural_parameter_contract$specification_id,
              analysis_contract$specification_id),
    identical(fit_registry$q, integration_contract$q),
    identical(integration_contract$q, structural_parameter_contract$q),
    identical(fit_registry$uses_numerical_integration,
              integration_contract$uses_numerical_integration))
  if (!all(bindings)) {
    stop("Analysis, fit-scope, rank, folds, and integration contracts are not exactly bound.",
         call. = FALSE)
  }
  calibration <- NULL
  if (!is.null(calibration_artifact)) {
    if (!.scmix_v22_comp_validate_calibration(
        calibration_artifact, analysis_contract, require_complete = TRUE)) {
      stop("The calibration artifact is not intact or bound to the exact analysis calibration hash.",
           call. = FALSE)
    }
    calibration <- calibration_artifact
  } else if (isTRUE(require_calibration)) {
    stop("Production mode requires the intact Step-3 calibration artifact.",
         call. = FALSE)
  }
  list(analysis = analysis_contract, registry = registry_info$payload,
       registry_fingerprint = registry_info$fingerprint,
       integration = integration_contract, fit_registry = fit_registry,
       structural = structural_parameter_contract,
       calibration = calibration)
}

#' Sign optimizer and polish evidence for one registered start
#'
#' @param analysis_contract,registry,integration_contract Intact frozen
#'   contracts.
#' @param fit_scope_id,seed Registered scope and seed.
#' @param optimizer_artifact Portable per-start optimizer record. All numerical
#'   fields are derived from this object rather than caller attestations.
#' @param role Branch role.
#' @param initial_state_hash Exact level-A origin for a level-B repolish.
#' @param calibration_artifact Exact analysis-linked Step-3 calibration
#'   artifact. Required for production optimizer evidence.
#' @return An intact per-start optimizer evidence object.
#' @export
scmix_v22_start_optimizer_evidence <- function(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, integration_rule,
    fit_scope_id, seed, optimizer_artifact,
    role = c("level_a_production_start", "level_b_repolish"),
    initial_state_hash = NULL, calibration_artifact = NULL) {
  role <- match.arg(role)
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract)
  if (!.scmix_v22_comp_intact(
      integration_rule, "scmix_v22_integration_rule")) {
    stop("`integration_rule` must be an intact signed rule artifact.",
         call. = FALSE)
  }
  expected_rule <- if (identical(role, "level_a_production_start")) {
    bound$integration$level_a_rule
  } else bound$integration$level_b_rule
  if (!identical(integration_rule$fingerprint, expected_rule$fingerprint)) {
    stop("Optimizer evidence uses the wrong signed integration rule for its role.",
         call. = FALSE)
  }
  required_source <- c(
    "start_id", "state_hash", "selected", "mean_objective",
    "objective_definition_id", "dtype", "polished", "protocol_mode",
    "calibration_hash", "optimizer_evidence", "v22_optimization_status",
    "network_state", "A_optimization", "kappa")
  if (!is.list(optimizer_artifact) ||
      !all(required_source %in% names(optimizer_artifact))) {
    stop("`optimizer_artifact` lacks required portable v2.2 optimizer fields.",
         call. = FALSE)
  }
  start_id <- optimizer_artifact$start_id
  state_hash <- optimizer_artifact$state_hash
  selected <- optimizer_artifact$selected
  objective <- optimizer_artifact$mean_objective
  objective_definition_id <- optimizer_artifact$objective_definition_id
  dtype <- optimizer_artifact$dtype
  polish_completed <- optimizer_artifact$polished
  optimizer_protocol_mode <- .scmix_v22_comp_mode(
    optimizer_artifact$protocol_mode)
  source_evidence <- optimizer_artifact$optimizer_evidence
  source_status <- optimizer_artifact$v22_optimization_status
  if (!is.list(source_evidence) || !is.list(source_evidence$objective) ||
      !is.list(source_evidence$polish) || !is.list(source_evidence$kkt) ||
      !is.list(source_evidence$kkt$returned) ||
      !is.list(source_status) || !is.list(source_status$gate_evidence)) {
    stop("The portable optimizer record lacks its derived v2.2 evidence tree.",
         call. = FALSE)
  }
  objective_before_polish <- source_evidence$objective$before_polish
  objective_after_polish <- source_evidence$objective$after_polish
  objective_returned <- source_evidence$objective$returned
  objective_allowance <- source_evidence$objective$nonworsening_numerical_allowance
  line_search_status <- source_evidence$polish$line_search_status
  nonworsening <- source_evidence$polish$nonworsening
  kkt_by_block <- source_evidence$kkt$returned$blocks
  network_state <- optimizer_artifact$network_state
  integration_grid <- if (is.list(network_state))
    network_state$integration_grid else NULL
  grid_u <- if (is.list(integration_grid))
    as.matrix(integration_grid$U) else NULL
  grid_w <- if (is.list(integration_grid))
    as.numeric(integration_grid$w) else NULL
  grid_rule_ok <- is.matrix(grid_u) && is.numeric(grid_u) &&
    identical(dim(grid_u), c(integration_rule$resolution,
                             integration_rule$integration_dimension)) &&
    all(is.finite(grid_u)) &&
    identical(.scmix_v22_comp_fingerprint(unname(grid_u)),
              integration_rule$node_hash) &&
    identical(if (is.null(grid_w)) NULL else
      .scmix_v22_comp_fingerprint(unname(grid_w)),
      integration_rule$weight_hash)
  if (!is.list(network_state) ||
      !identical(network_state$analysis_signature,
                 bound$analysis$analysis_signature) ||
      !is.list(network_state$architecture) ||
      !identical(as.integer(network_state$architecture$q),
                 bound$structural$q) ||
      !identical(as.integer(network_state$architecture$p),
                 bound$structural$covariance_dimension) ||
      !identical(as.character(network_state$coefficient_names),
                 bound$structural$covariance_coordinate_ids) ||
      !identical(optimizer_artifact$calibration_hash,
                 bound$analysis$calibration_hash) ||
      !grid_rule_ok ||
      !is.matrix(optimizer_artifact$A_optimization) ||
      !identical(dim(optimizer_artifact$A_optimization),
                 c(bound$structural$covariance_dimension,
                   bound$structural$q)) ||
      !is.numeric(optimizer_artifact$kappa) ||
      length(optimizer_artifact$kappa) != 1L ||
      !is.finite(optimizer_artifact$kappa) ||
      !identical(state_hash, .scmix_v22_comp_state_hash(
        network_state$state_dict, optimizer_artifact$A_optimization,
        optimizer_artifact$kappa))) {
    stop("The source optimizer state is not bound to the exact analysis, rank, p coordinates, and calibration hash.",
         call. = FALSE)
  }
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
            objective_after_polish = objective_after_polish,
            objective_returned = objective_returned,
            objective_allowance = objective_allowance)
  required_kkt_columns <- c("block", "applicable", "dimension",
                            "projected_kkt_rms")
  if (any(!is.finite(nums)) || !is.data.frame(kkt_by_block) ||
      !all(required_kkt_columns %in% names(kkt_by_block)) ||
      anyNA(kkt_by_block$block) || anyDuplicated(kkt_by_block$block) ||
      any(!nzchar(kkt_by_block$block)) ||
      !is.logical(kkt_by_block$applicable) || anyNA(kkt_by_block$applicable) ||
      !is.numeric(kkt_by_block$dimension) || anyNA(kkt_by_block$dimension) ||
      any(kkt_by_block$dimension < 0) ||
      any(!is.finite(kkt_by_block$projected_kkt_rms[
        kkt_by_block$applicable])) ||
      any(kkt_by_block$projected_kkt_rms[kkt_by_block$applicable] < 0)) {
    stop("Objective and named KKT evidence must be finite and complete.",
         call. = FALSE)
  }
  required_kkt <- c("position_effect", "unpenalized_mean_reference",
                    "centered_mean_deviation", "horizontal_loadings",
                    "all_structural_coordinates")
  if (!setequal(kkt_by_block$block, required_kkt)) {
    stop("KKT evidence must contain every and only the frozen structural blocks.",
         call. = FALSE)
  }
  kkt_by_block <- kkt_by_block[
    match(required_kkt, kkt_by_block$block), , drop = FALSE]
  if (!is.logical(polish_completed) || length(polish_completed) != 1L ||
      is.na(polish_completed) || !is.logical(nonworsening) ||
      length(nonworsening) != 1L || is.na(nonworsening)) {
    stop("Polish and nonworsening evidence must be nonmissing logical values.",
         call. = FALSE)
  }
  exact_objective_id <- paste0(
    "mean_negative_complete_sequence_loglik_per_respondent_",
    "plus_declared_penalty_in_nats")
  polish <- source_evidence$polish
  gate_evidence <- source_status$gate_evidence
  gate_ids <- paste0("F00", 1:6)
  gates_complete <- all(gate_ids %in% names(gate_evidence)) &&
    all(vapply(gate_evidence[gate_ids], function(x)
      is.list(x) && is.character(x$state) && length(x$state) == 1L,
      logical(1L)))
  applicable_kkt <- kkt_by_block$applicable
  kkt_maximum <- max(kkt_by_block$projected_kkt_rms[applicable_kkt])
  f005 <- gate_evidence$F005
  f006 <- gate_evidence$F006
  calibration_ok <- if (identical(optimizer_protocol_mode, "production")) {
    .scmix_v22_comp_validate_calibration(
      calibration_artifact, bound$analysis, require_complete = TRUE)
  } else is.null(calibration_artifact) || .scmix_v22_comp_validate_calibration(
    calibration_artifact, bound$analysis, require_complete = TRUE)
  expected_states <- if (identical(optimizer_protocol_mode, "production")) {
    stats::setNames(rep("pass", 6L), gate_ids)
  } else stats::setNames(c(rep("pass", 4L), "not_run", "pass"), gate_ids)
  observed_states <- if (gates_complete) vapply(
    gate_evidence[gate_ids], `[[`, character(1L), "state") else character()
  source_contract_ok <-
    identical(source_evidence$protocol_id, .scmix_v22_comp_protocol_id) &&
    identical(source_evidence$objective$definition_id, exact_objective_id) &&
    identical(polish$method, "full_batch_lbfgs") &&
    identical(polish$line_search, "strong_wolfe") &&
    identical(polish$method_id, "full_batch_lbfgs_strong_wolfe") &&
    isTRUE(polish$completed) && isTRUE(polish$accepted) &&
    identical(polish$projection_changed, FALSE) &&
    isTRUE(polish$same_objective_all_stages) &&
    isTRUE(polish$returned_matches_accepted_objective) &&
    isTRUE(source_evidence$kkt$returned$all_finite) &&
    identical(as.numeric(source_evidence$kkt$returned$maximum),
              as.numeric(kkt_maximum)) &&
    gates_complete && identical(unname(observed_states),
                                unname(expected_states)) &&
    is.list(f005) && identical(as.numeric(f005$observed),
                               as.numeric(kkt_maximum)) &&
    is.list(f006) && identical(f006$observed, FALSE)
  if (!calibration_ok || !source_contract_ok ||
      (identical(optimizer_protocol_mode, "production") &&
       (!is.numeric(f005$threshold) || length(f005$threshold) != 1L ||
        !identical(as.numeric(f005$threshold),
                   as.numeric(calibration_artifact$constants$kkt_cutoff)) ||
        kkt_maximum > f005$threshold ||
        !isTRUE(source_status$optimization_gate_pass))) ||
      (identical(optimizer_protocol_mode, "calibration") &&
       !isTRUE(source_status$precalibration_fit_valid))) {
    stop("The source optimizer evidence does not derive a valid v2.2 polish, KKT, bound, and calibration result.",
         call. = FALSE)
  }
  if (objective_allowance < 0 ||
      abs(objective - objective_after_polish) > objective_allowance ||
      abs(objective - objective_returned) > objective_allowance ||
      !identical(isTRUE(nonworsening),
                 objective_after_polish <=
                   objective_before_polish + objective_allowance)) {
    stop("Final objective and nonworsening evidence must be internally consistent.",
         call. = FALSE)
  }
  if (!is.logical(selected) || length(selected) != 1L || is.na(selected)) {
    stop("`selected` must be one nonmissing logical value.", call. = FALSE)
  }
  if (identical(role, "level_b_repolish")) {
    initial_state_hash <- .scmix_v22_comp_scalar_text(
      initial_state_hash, "initial_state_hash")
  } else if (!is.null(initial_state_hash)) {
    stop("Only level-B repolish evidence may name an initial level-A state.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    analysis_signature = bound$analysis$analysis_signature,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    integration_rule_fingerprint = integration_rule$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    application_id = bound$analysis$application_id,
    specification_id = bound$analysis$specification_id,
    code_commit = bound$analysis$code_commit,
    data_hash = bound$analysis$data_hash,
    design_hash = bound$analysis$design_hash,
    calibration_hash = bound$analysis$calibration_hash,
    calibration_artifact_fingerprint = if (is.null(calibration_artifact))
      NULL else calibration_artifact$fingerprint,
    fit_id = bound$fit_registry$fit_id, q = bound$fit_registry$q,
    uses_numerical_integration =
      bound$fit_registry$uses_numerical_integration,
    fit_scope_id = fit_scope_id, start_id = start_id,
    seed = as.integer(seed),
    state_hash = .scmix_v22_comp_scalar_text(state_hash, "state_hash"),
    initial_state_hash = initial_state_hash,
    optimizer_artifact_hash = .scmix_v22_comp_scalar_text(
      .scmix_v22_comp_fingerprint(optimizer_artifact),
      "optimizer_artifact_hash"),
    source_optimizer_artifact = optimizer_artifact,
    role = role, selected = selected,
    optimizer_protocol_mode = optimizer_protocol_mode,
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
    structural_parameter_contract,
    reported_estimates, standard_errors) {
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract)
  if (!.scmix_v22_comp_intact(
      optimizer_evidence, "scmix_v22_start_optimizer_evidence") ||
      !identical(optimizer_evidence$analysis_contract_fingerprint,
                 bound$analysis$fingerprint) ||
      !identical(optimizer_evidence$registry_fingerprint,
                 bound$registry_fingerprint) ||
      !identical(optimizer_evidence$integration_contract_fingerprint,
                 bound$integration$fingerprint) ||
      !identical(optimizer_evidence$structural_parameter_contract_fingerprint,
                 bound$structural$fingerprint) ||
      !identical(optimizer_evidence$role,
                 "level_a_production_start")) {
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
    analysis_signature = bound$analysis$analysis_signature,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    optimizer_evidence_fingerprint = optimizer_evidence$fingerprint,
    fit_id = bound$fit_registry$fit_id,
    specification_id = bound$analysis$specification_id,
    code_commit = bound$analysis$code_commit,
    data_hash = bound$analysis$data_hash,
    design_hash = bound$analysis$design_hash,
    calibration_hash = bound$analysis$calibration_hash,
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
#' @param calibration_artifact Intact exact Step-3 calibration artifact. It is
#'   required in production mode and must match the analysis calibration hash.
#' @param protocol_mode Either `"calibration"` or `"production"`.
#' @return A signed multistart evidence object with all relative objective
#'   gaps, target changes, targetwise maxima, and F008 gate rows.
#' @export
scmix_v22_multistart_stability <- function(
    evaluations, optimizer_evidence, analysis_contract, registry,
    integration_contract, structural_parameter_contract,
    calibration_artifact = NULL,
    protocol_mode = c("calibration", "production")) {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, calibration_artifact,
    require_calibration = identical(mode, "production"))
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
      identical(x$analysis_signature, bound$analysis$analysis_signature),
      identical(x$analysis_contract_fingerprint, bound$analysis$fingerprint),
      identical(x$registry_fingerprint, bound$registry_fingerprint),
      identical(x$integration_contract_fingerprint,
                bound$integration$fingerprint),
      identical(x$structural_parameter_contract_fingerprint,
                bound$structural$fingerprint),
      identical(x$fit_registry_fingerprint, bound$fit_registry$fingerprint),
      identical(x$fit_id, bound$fit_registry$fit_id),
      identical(x$specification_id, bound$analysis$specification_id),
      identical(x$code_commit, bound$analysis$code_commit),
      identical(x$data_hash, bound$analysis$data_hash),
      identical(x$design_hash, bound$analysis$design_hash),
      identical(x$calibration_hash, bound$analysis$calibration_hash),
      identical(x$calibration_artifact_fingerprint,
                if (is.null(bound$calibration)) NULL else
                  bound$calibration$fingerprint),
      identical(x$q, bound$integration$q),
      identical(x$uses_numerical_integration,
                bound$integration$uses_numerical_integration),
      identical(x$fit_scope_id, row$fit_scope_id),
      identical(x$start_id, row$start_id),
      identical(x$seed, as.integer(row$seed)),
      identical(x$objective_definition_id, objective_id),
      identical(x$integration_rule_fingerprint,
                bound$integration$level_a_rule$fingerprint),
      identical(x$role, "level_a_production_start"),
      identical(x$optimizer_protocol_mode, mode),
      identical(x$dtype, "float64"), isTRUE(x$polish_completed),
      isTRUE(x$nonworsening),
      is.finite(x$objective_before_polish),
      is.finite(x$objective_after_polish),
      x$objective_after_polish <= x$objective_before_polish,
      is.data.frame(x$kkt_by_block) && nrow(x$kkt_by_block) == 5L &&
        all(c("block", "applicable", "projected_kkt_rms") %in%
              names(x$kkt_by_block)) &&
        identical(x$kkt_by_block$block,
                  c("position_effect", "unpenalized_mean_reference",
                    "centered_mean_deviation", "horizontal_loadings",
                    "all_structural_coordinates")) &&
        all(is.finite(x$kkt_by_block$projected_kkt_rms[
          x$kkt_by_block$applicable])),
      is.character(x$line_search_status) && length(x$line_search_status) == 1L &&
        nzchar(x$line_search_status),
      is.character(x$optimizer_artifact_hash) &&
        length(x$optimizer_artifact_hash) == 1L &&
        nzchar(x$optimizer_artifact_hash))
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
      identical(x$analysis_signature, bound$analysis$analysis_signature),
      identical(x$analysis_contract_fingerprint, bound$analysis$fingerprint),
      identical(x$registry_fingerprint, bound$registry_fingerprint),
      identical(x$integration_contract_fingerprint,
                bound$integration$fingerprint),
      identical(x$structural_parameter_contract_fingerprint,
                bound$structural$fingerprint),
      identical(x$optimizer_evidence_fingerprint, opt$fingerprint),
      identical(x$fit_id, bound$fit_registry$fit_id),
      identical(x$specification_id, bound$analysis$specification_id),
      identical(x$code_commit, bound$analysis$code_commit),
      identical(x$data_hash, bound$analysis$data_hash),
      identical(x$design_hash, bound$analysis$design_hash),
      identical(x$calibration_hash, bound$analysis$calibration_hash),
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
    bound$calibration$constants$near_optimum_objective_cutoff
  target_cut <- if (is.null(bound$calibration)) .scmix_v22_pending_cutoff else
    bound$calibration$constants$multistart_target_se_cutoff
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
    selected_ids <- vapply(order_ids, function(id) {
      unique(vapply(optimizer_evidence[
        vapply(optimizer_evidence, function(x)
          identical(x$fit_scope_id, scope) && identical(x$start_id, id),
          logical(1L))], `[[`, logical(1L), "selected"))
    }, logical(1L))
    if (sum(selected_ids) != 1L ||
        q_by_start[which(selected_ids)] != q_star) {
      stop("Exactly one selected start per scope must equal the lowest polished objective.",
           call. = FALSE)
    }
    best <- order_ids[which(selected_ids)]
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
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    calibration_artifact_fingerprint = if (is.null(bound$calibration)) NULL else
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

#' Sign an exact restricted-to-full nesting contract
#'
#' @param restricted_analysis_contract,full_analysis_contract Intact v2.2
#'   analysis contracts.
#' @param restricted_structural_contract,full_structural_contract Intact
#'   signed structural-coordinate contracts.
#' @param embedding_matrix Named matrix mapping restricted structural
#'   coordinates into the full coordinate chart.
#' @param restriction_offset Named full-coordinate restriction offset.
#' @param verification_artifact_hash Immutable code/test artifact verifying
#'   that the restriction yields identical probabilities and penalties.
#' @return An intact fingerprinted exact-nesting contract.
#' @export
scmix_v22_nested_specification_contract <- function(
    restricted_analysis_contract, full_analysis_contract,
    restricted_structural_contract, full_structural_contract,
    embedding_matrix, restriction_offset, verification_artifact_hash) {
  restricted_analysis_contract <- .scmix_v22_comp_contract(
    restricted_analysis_contract)
  full_analysis_contract <- .scmix_v22_comp_contract(full_analysis_contract)
  contracts <- list(restricted = restricted_structural_contract,
                    full = full_structural_contract)
  if (!all(vapply(contracts, .scmix_v22_comp_intact, logical(1L),
                  class_name = "scmix_v22_structural_parameter_contract"))) {
    stop("Both structural contracts must be intact and fingerprinted.",
         call. = FALSE)
  }
  if (!identical(restricted_analysis_contract$application_id,
                 full_analysis_contract$application_id) ||
      identical(restricted_analysis_contract$specification_id,
                full_analysis_contract$specification_id) ||
      !identical(restricted_structural_contract$application_id,
                 restricted_analysis_contract$application_id) ||
      !identical(full_structural_contract$application_id,
                 full_analysis_contract$application_id) ||
      !identical(restricted_structural_contract$specification_id,
                 restricted_analysis_contract$specification_id) ||
      !identical(full_structural_contract$specification_id,
                 full_analysis_contract$specification_id)) {
    stop("Nested specifications must be distinct specifications of the same application.",
         call. = FALSE)
  }
  r_ids <- restricted_structural_contract$score_coordinate_ids
  f_ids <- full_structural_contract$score_coordinate_ids
  if (!is.matrix(embedding_matrix) || !is.numeric(embedding_matrix) ||
      any(!is.finite(embedding_matrix)) ||
      !identical(dim(embedding_matrix), c(length(f_ids), length(r_ids))) ||
      !identical(rownames(embedding_matrix), f_ids) ||
      !identical(colnames(embedding_matrix), r_ids) ||
      qr(embedding_matrix)$rank != length(r_ids)) {
    stop("`embedding_matrix` must be a finite, named, full-column-rank exact structural embedding.",
         call. = FALSE)
  }
  restriction_offset <- .scmix_v22_comp_named_numeric(
    restriction_offset, "restriction_offset")
  if (!identical(names(restriction_offset), f_ids)) {
    stop("`restriction_offset` must use exact full structural-coordinate order.",
         call. = FALSE)
  }
  r_fit <- restricted_analysis_contract$optimizer_starts
  f_fit <- full_analysis_contract$optimizer_starts
  if (!.scmix_v22_comp_intact(r_fit, "scmix_v22_fit_scope_registry") ||
      !.scmix_v22_comp_intact(f_fit, "scmix_v22_fit_scope_registry") ||
      !identical(r_fit$outer_fold_ids, f_fit$outer_fold_ids) ||
      !identical(r_fit$structural_parameter_contract_fingerprint,
                 restricted_structural_contract$fingerprint) ||
      !identical(f_fit$structural_parameter_contract_fingerprint,
                 full_structural_contract$fingerprint)) {
    stop("Nested specifications must share the exact registered outer folds.",
         call. = FALSE)
  }
  suffix <- c("full", paste("outer", r_fit$outer_fold_ids, sep = "::"))
  scope_map <- data.frame(
    scope_suffix = suffix,
    restricted_scope_id = paste(r_fit$specification_id, suffix, sep = "::"),
    full_scope_id = paste(f_fit$specification_id, suffix, sep = "::"),
    stringsAsFactors = FALSE)
  if (!setequal(scope_map$restricted_scope_id, r_fit$fit_scope_ids) ||
      !setequal(scope_map$full_scope_id, f_fit$fit_scope_ids)) {
    stop("Nested scope IDs do not cover the full fit and every outer fold.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    application_id = full_analysis_contract$application_id,
    restricted_specification_id =
      restricted_analysis_contract$specification_id,
    full_specification_id = full_analysis_contract$specification_id,
    restricted_analysis_contract_fingerprint =
      restricted_analysis_contract$fingerprint,
    full_analysis_contract_fingerprint = full_analysis_contract$fingerprint,
    restricted_structural_contract_fingerprint =
      restricted_structural_contract$fingerprint,
    full_structural_contract_fingerprint = full_structural_contract$fingerprint,
    restricted_fit_registry_fingerprint = r_fit$fingerprint,
    full_fit_registry_fingerprint = f_fit$fingerprint,
    scope_map = scope_map, embedding_matrix = embedding_matrix,
    restriction_offset = restriction_offset,
    objective_definition_id =
      "mean_negative_complete_sequence_loglik_per_respondent_plus_declared_penalty_in_nats",
    exact_embedding_verified = TRUE,
    verification_artifact_hash = .scmix_v22_comp_scalar_text(
      verification_artifact_hash, "verification_artifact_hash")),
    "scmix_v22_nested_specification_contract")
}

.scmix_v22_comp_selected_start_evidence <- function(
    evidence, analysis, registry_fingerprint, integration_fingerprint,
    structural_fingerprint, fit_registry, scopes, protocol_mode) {
  if (!is.list(evidence) || length(evidence) != length(scopes) ||
      !all(vapply(evidence, .scmix_v22_comp_intact, logical(1L),
                  class_name = "scmix_v22_start_optimizer_evidence"))) {
    stop("One intact selected optimizer artifact is required per nested fit scope.",
         call. = FALSE)
  }
  ids <- vapply(evidence, `[[`, character(1L), "fit_scope_id")
  if (anyDuplicated(ids) || !setequal(ids, scopes)) {
    stop("Nested optimizer evidence must cover every and only registered scope.",
         call. = FALSE)
  }
  evidence <- evidence[match(scopes, ids)]
  ok <- vapply(evidence, function(x) {
    identical(x$analysis_contract_fingerprint, analysis$fingerprint) &&
      identical(x$registry_fingerprint, registry_fingerprint) &&
      identical(x$integration_contract_fingerprint, integration_fingerprint) &&
      identical(x$structural_parameter_contract_fingerprint,
                structural_fingerprint) &&
      identical(x$fit_registry_fingerprint, fit_registry$fingerprint) &&
      identical(x$specification_id, analysis$specification_id) &&
      identical(x$role, "level_a_production_start") && isTRUE(x$selected) &&
      identical(x$optimizer_protocol_mode, protocol_mode) &&
      identical(x$dtype, "float64") && isTRUE(x$polish_completed) &&
      isTRUE(x$nonworsening) &&
      identical(x$objective_definition_id,
                "mean_negative_complete_sequence_loglik_per_respondent_plus_declared_penalty_in_nats")
  }, logical(1L))
  if (!all(ok)) {
    stop("Nested optimizer evidence is not exact selected v2.2 production evidence.",
         call. = FALSE)
  }
  evidence
}

#' Evaluate the exact nested-fit dominance gate F007
#'
#' @param nested_contract Intact exact-nesting contract.
#' @param restricted_optimizer_evidence,full_optimizer_evidence One selected
#'   level-A optimizer artifact per full/fold scope.
#' @param restricted_registry,full_registry Signed target registries.
#' @param restricted_integration_contract,full_integration_contract Signed
#'   integration contracts.
#' @param restricted_calibration_artifact,full_calibration_artifact Exact
#'   analysis-linked Step-3 calibration artifacts.
#' @param protocol_mode `"calibration"` or `"production"`.
#' @return A fingerprinted F007 evidence object.
#' @export
scmix_v22_nested_fit_stability <- function(
    nested_contract,
    restricted_analysis_contract, full_analysis_contract,
    restricted_registry, full_registry,
    restricted_integration_contract, full_integration_contract,
    restricted_structural_contract, full_structural_contract,
    restricted_optimizer_evidence, full_optimizer_evidence,
    restricted_calibration_artifact = NULL,
    full_calibration_artifact = NULL,
    protocol_mode = c("calibration", "production")) {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  if (!.scmix_v22_comp_intact(
      nested_contract, "scmix_v22_nested_specification_contract")) {
    stop("`nested_contract` must be intact and fingerprinted.",
         call. = FALSE)
  }
  rb <- .scmix_v22_comp_bind_contracts(
    restricted_analysis_contract, restricted_registry,
    restricted_integration_contract, restricted_structural_contract,
    restricted_calibration_artifact,
    require_calibration = identical(mode, "production"))
  fb <- .scmix_v22_comp_bind_contracts(
    full_analysis_contract, full_registry, full_integration_contract,
    full_structural_contract, full_calibration_artifact,
    require_calibration = identical(mode, "production"))
  links <- c(
    identical(nested_contract$restricted_analysis_contract_fingerprint,
              rb$analysis$fingerprint),
    identical(nested_contract$full_analysis_contract_fingerprint,
              fb$analysis$fingerprint),
    identical(nested_contract$restricted_structural_contract_fingerprint,
              rb$structural$fingerprint),
    identical(nested_contract$full_structural_contract_fingerprint,
              fb$structural$fingerprint),
    identical(nested_contract$restricted_fit_registry_fingerprint,
              rb$fit_registry$fingerprint),
    identical(nested_contract$full_fit_registry_fingerprint,
              fb$fit_registry$fingerprint),
    isTRUE(nested_contract$exact_embedding_verified))
  if (!all(links)) {
    stop("The nesting contract does not match the exact restricted and full analyses.",
         call. = FALSE)
  }
  r_scopes <- nested_contract$scope_map$restricted_scope_id
  f_scopes <- nested_contract$scope_map$full_scope_id
  re <- .scmix_v22_comp_selected_start_evidence(
    restricted_optimizer_evidence, rb$analysis, rb$registry_fingerprint,
    rb$integration$fingerprint, rb$structural$fingerprint,
    rb$fit_registry, r_scopes, mode)
  fe <- .scmix_v22_comp_selected_start_evidence(
    full_optimizer_evidence, fb$analysis, fb$registry_fingerprint,
    fb$integration$fingerprint, fb$structural$fingerprint,
    fb$fit_registry, f_scopes, mode)
  if (!is.null(rb$calibration) && any(vapply(re, function(x)
      !identical(x$calibration_artifact_fingerprint,
                 rb$calibration$fingerprint), logical(1L)))) {
    stop("Restricted optimizer evidence uses a different calibration artifact.",
         call. = FALSE)
  }
  if (!is.null(fb$calibration) && any(vapply(fe, function(x)
      !identical(x$calibration_artifact_fingerprint,
                 fb$calibration$fingerprint), logical(1L)))) {
    stop("Full optimizer evidence uses a different calibration artifact.",
         call. = FALSE)
  }
  qr <- vapply(re, `[[`, numeric(1L), "objective")
  qf <- vapply(fe, `[[`, numeric(1L), "objective")
  violation <- pmax(0, qf - qr) / pmax(1, abs(qr))
  names(violation) <- nested_contract$scope_map$scope_suffix
  statistic <- max(violation)
  cutoff <- if (is.null(fb$calibration)) .scmix_v22_pending_cutoff else
    fb$calibration$constants$nested_fit_relative_cutoff
  if (!is.null(rb$calibration) &&
      !identical(rb$calibration$constants$nested_fit_relative_cutoff,
                 cutoff)) {
    stop("Restricted and full calibration artifacts disagree on the frozen F007 cutoff.",
         call. = FALSE)
  }
  state <- .scmix_v22_comp_gate_state(statistic, cutoff, mode, TRUE)
  gate <- .scmix_v22_comp_gate(
    "F007", "nested_fit", "hard_fit", state, statistic, cutoff,
    "relative_mean_objective", "signed exact-nesting and optimizer evidence",
    if (is.character(cutoff)) {
      "The nested-fit cutoff remains CALIBRATE_IN_STEP_3."
    } else {
      "The maximum exact nested-fit violation covers the full fit and every registered outer fold."
    }, paste(rb$analysis$specification_id,
             fb$analysis$specification_id, sep = "->"))
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id, protocol_mode = mode,
    nested_contract_fingerprint = nested_contract$fingerprint,
    restricted_analysis_contract_fingerprint = rb$analysis$fingerprint,
    full_analysis_contract_fingerprint = fb$analysis$fingerprint,
    restricted_registry_fingerprint = rb$registry_fingerprint,
    full_registry_fingerprint = fb$registry_fingerprint,
    restricted_integration_contract_fingerprint = rb$integration$fingerprint,
    full_integration_contract_fingerprint = fb$integration$fingerprint,
    restricted_structural_parameter_contract_fingerprint =
      rb$structural$fingerprint,
    full_structural_parameter_contract_fingerprint = fb$structural$fingerprint,
    restricted_fit_registry_fingerprint = rb$fit_registry$fingerprint,
    full_fit_registry_fingerprint = fb$fit_registry$fingerprint,
    restricted_calibration_artifact_fingerprint = if (is.null(rb$calibration))
      NULL else rb$calibration$fingerprint,
    full_calibration_artifact_fingerprint = if (is.null(fb$calibration))
      NULL else fb$calibration$fingerprint,
    scope_map = nested_contract$scope_map,
    restricted_optimizer_evidence_fingerprints = vapply(
      re, `[[`, character(1L), "fingerprint"),
    full_optimizer_evidence_fingerprints = vapply(
      fe, `[[`, character(1L), "fingerprint"),
    objective_by_scope = data.frame(
      scope_suffix = names(violation), restricted = qr, full = qf,
      relative_violation = unname(violation), stringsAsFactors = FALSE),
    statistic = statistic, threshold = cutoff, gate = gate,
    calibration_only = identical(mode, "calibration"),
    selected_gate_subset_passed = identical(mode, "production") &&
      identical(gate$state, "pass")),
    "scmix_v22_nested_fit_stability")
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
  sigma <- stats::sd(a)
  if (!is.finite(sigma) || sigma <= 0 || !is.finite(rms)) return(NA_real_)
  rms / sigma
}

.scmix_v22_comp_scope_ids <- function(bound) {
  sort(bound$fit_registry$fit_scope_ids, method = "radix")
}

.scmix_v22_comp_target_ids <- function(bound) {
  vapply(bound$registry, `[[`, character(1L), "target_id")
}

.scmix_v22_comp_primitive_ids <- function(bound) {
  sort(unique(unlist(lapply(bound$registry, `[[`,
                            "primitive_registry_ids"),
                     use.names = FALSE)), method = "radix")
}

#' Sign integration-specific Riesz recomputation for one primitive
#'
#' @param primitive_id A signed primitive registry ID.
#' @param parameter_state_hashes Named full-and-outer-fold parameter states.
#' @param solution_by_scope Named full-and-outer-fold structural-coordinate
#'   coefficient vectors.
#' @param integration_rule Exact signed rule used in every solve.
#' @param code_hash,provenance Immutable audit fields.
#' @inheritParams scmix_v22_integration_target_evidence
#' @return An intact fingerprinted Riesz recomputation artifact.
#' @export
scmix_v22_integration_riesz_solution <- function(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, primitive_id, parameter_state_hashes,
    solution_by_scope, integration_rule, code_hash, provenance) {
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract)
  primitive_id <- .scmix_v22_comp_scalar_text(
    primitive_id, "primitive_id")
  if (!primitive_id %in% .scmix_v22_comp_primitive_ids(bound)) {
    stop("`primitive_id` is outside the signed target registry.",
         call. = FALSE)
  }
  if (!.scmix_v22_comp_intact(
      integration_rule, "scmix_v22_integration_rule") ||
      !integration_rule$fingerprint %in% c(
        bound$integration$level_a_rule$fingerprint,
        bound$integration$level_b_rule$fingerprint)) {
    stop("Riesz recomputation requires the exact signed A or B rule.",
         call. = FALSE)
  }
  scopes <- .scmix_v22_comp_scope_ids(bound)
  parameter_state_hashes <- .scmix_v22_comp_named_character(
    parameter_state_hashes, "parameter_state_hashes")
  if (!setequal(names(parameter_state_hashes), scopes)) {
    stop("Riesz parameter states must cover every registered fit scope.",
         call. = FALSE)
  }
  parameter_state_hashes <- parameter_state_hashes[scopes]
  if (!is.list(solution_by_scope) || is.null(names(solution_by_scope)) ||
      anyDuplicated(names(solution_by_scope)) ||
      !setequal(names(solution_by_scope), scopes)) {
    stop("Riesz solutions must cover the full fit and every registered outer fold.",
         call. = FALSE)
  }
  solution_by_scope <- solution_by_scope[scopes]
  coordinate_ids <- bound$structural$score_coordinate_ids
  if (any(vapply(solution_by_scope, function(x)
    !is.numeric(x) || any(!is.finite(x)) ||
      !identical(names(x), coordinate_ids), logical(1L)))) {
    stop("Every Riesz solution must use the exact signed structural coordinates.",
         call. = FALSE)
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    specification_id = bound$analysis$specification_id,
    primitive_id = primitive_id,
    integration_rule_fingerprint = integration_rule$fingerprint,
    parameter_state_hashes = parameter_state_hashes,
    solution_by_scope = solution_by_scope,
    code_hash = .scmix_v22_comp_scalar_text(code_hash, "code_hash"),
    provenance = .scmix_v22_comp_scalar_text(provenance, "provenance")),
    "scmix_v22_integration_riesz_solution")
}

#' Sign recomputed target and influence evidence for an integration rule
#'
#' @param analysis_contract,registry,integration_contract,
#'   structural_parameter_contract Intact frozen contracts.
#' @param integration_rule Exact signed level-A or level-B rule.
#' @param parameter_state_hashes Named full-and-outer-fold state hashes.
#' @param targets,standard_errors Named registered target values and raw-IF
#'   standard errors.
#' @param influence Respondent-by-primitive centered raw one-step influence
#'   matrix.
#' @param respondent_id Respondent IDs in the exact influence-row order.
#' @param riesz_solution_artifacts One intact recomputed Riesz artifact per
#'   primitive.
#' @param code_hash,provenance Immutable audit fields.
#' @return An intact fingerprinted target-recomputation evidence object.
#' @export
scmix_v22_integration_target_evidence <- function(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, integration_rule,
    parameter_state_hashes, targets, standard_errors, influence,
    respondent_id, riesz_solution_artifacts, code_hash, provenance) {
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract)
  if (!.scmix_v22_comp_intact(
      integration_rule, "scmix_v22_integration_rule") ||
      !integration_rule$fingerprint %in% c(
        bound$integration$level_a_rule$fingerprint,
        bound$integration$level_b_rule$fingerprint)) {
    stop("`integration_rule` must be the exact signed A or B rule.",
         call. = FALSE)
  }
  scopes <- .scmix_v22_comp_scope_ids(bound)
  parameter_state_hashes <- .scmix_v22_comp_named_character(
    parameter_state_hashes, "parameter_state_hashes")
  if (!setequal(names(parameter_state_hashes), scopes)) {
    stop("Parameter-state evidence must cover the full fit and every registered outer fold.",
         call. = FALSE)
  }
  parameter_state_hashes <- parameter_state_hashes[scopes]
  target_ids <- .scmix_v22_comp_target_ids(bound)
  primitive_ids <- .scmix_v22_comp_primitive_ids(bound)
  targets <- .scmix_v22_comp_named_numeric(targets, "targets")
  standard_errors <- .scmix_v22_comp_named_numeric(
    standard_errors, "standard_errors", positive = TRUE)
  if (!identical(names(targets), target_ids) ||
      !identical(names(standard_errors), target_ids)) {
    stop("Targets and standard errors must use exact signed target-registry order.",
         call. = FALSE)
  }
  if (!is.matrix(influence) || !is.numeric(influence) ||
      nrow(influence) < 2L || ncol(influence) < 1L ||
      any(!is.finite(influence)) ||
      !identical(colnames(influence), primitive_ids)) {
    stop("Influence evidence must be a finite respondent-by-primitive matrix in signed registry order.",
         call. = FALSE)
  }
  respondent_id <- as.character(respondent_id)
  if (length(respondent_id) != nrow(influence) || anyNA(respondent_id) ||
      any(!nzchar(respondent_id)) || anyDuplicated(respondent_id)) {
    stop("Respondent IDs must be complete and unique in influence-row order.",
         call. = FALSE)
  }
  influence_sd <- apply(influence, 2L, stats::sd)
  if (any(!is.finite(influence_sd)) || any(influence_sd <= 0)) {
    stop("Every primitive must have a finite positive respondent influence SD.",
         call. = FALSE)
  }
  influence_mean <- colMeans(influence)
  if (any(abs(influence_mean) > sqrt(.Machine$double.eps) *
          pmax(1, influence_sd))) {
    stop("Primitive influence columns must be centered at respondent level.",
         call. = FALSE)
  }
  if (!is.list(riesz_solution_artifacts) ||
      length(riesz_solution_artifacts) != length(primitive_ids) ||
      !all(vapply(riesz_solution_artifacts, .scmix_v22_comp_intact,
                  logical(1L),
                  class_name = "scmix_v22_integration_riesz_solution"))) {
    stop("One intact integration-specific Riesz artifact is required per primitive.",
         call. = FALSE)
  }
  artifact_ids <- vapply(riesz_solution_artifacts, `[[`, character(1L),
                         "primitive_id")
  if (anyDuplicated(artifact_ids) || !setequal(artifact_ids, primitive_ids)) {
    stop("Riesz artifacts must cover every and only signed primitive.",
         call. = FALSE)
  }
  riesz_solution_artifacts <- riesz_solution_artifacts[
    match(primitive_ids, artifact_ids)]
  riesz_ok <- vapply(riesz_solution_artifacts, function(x) {
    identical(x$analysis_contract_fingerprint, bound$analysis$fingerprint) &&
      identical(x$registry_fingerprint, bound$registry_fingerprint) &&
      identical(x$integration_contract_fingerprint,
                bound$integration$fingerprint) &&
      identical(x$structural_parameter_contract_fingerprint,
                bound$structural$fingerprint) &&
      identical(x$fit_registry_fingerprint, bound$fit_registry$fingerprint) &&
      identical(x$specification_id, bound$analysis$specification_id) &&
      identical(x$integration_rule_fingerprint, integration_rule$fingerprint) &&
      identical(x$parameter_state_hashes, parameter_state_hashes)
  }, logical(1L))
  if (!all(riesz_ok)) {
    stop("Riesz artifacts are not bound to the exact analysis and integration rule.",
         call. = FALSE)
  }
  riesz_solution_hashes <- vapply(
    riesz_solution_artifacts, `[[`, character(1L), "fingerprint")
  names(riesz_solution_hashes) <- primitive_ids
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    analysis_signature = bound$analysis$analysis_signature,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    specification_id = bound$analysis$specification_id,
    integration_rule_fingerprint = integration_rule$fingerprint,
    integration_rule_level = integration_rule$level,
    parameter_state_hashes = parameter_state_hashes,
    target_ids = target_ids, primitive_ids = primitive_ids,
    integrated_target_ids = bound$integration$integrated_target_ids,
    integrated_primitive_ids = bound$integration$integrated_primitive_ids,
    targets = targets, standard_errors = standard_errors,
    influence = influence, respondent_id = respondent_id,
    influence_sd = influence_sd, influence_mean = influence_mean,
    influence_definition_id =
      "centered_raw_one_step_respondent_sequence",
    riesz_solution_artifacts = riesz_solution_artifacts,
    riesz_solution_hashes = riesz_solution_hashes,
    riesz_integration_rule_fingerprint = integration_rule$fingerprint,
    code_hash = .scmix_v22_comp_scalar_text(code_hash, "code_hash"),
    provenance = .scmix_v22_comp_scalar_text(provenance, "provenance")),
    "scmix_v22_integration_target_evidence")
}

#' Construct a signed full-and-fold integration evaluation
#'
#' This constructor is the only supported input path to the v2.2 integration
#' comparison.  It binds each numerical object to the exact analysis,
#' specification, target registry, fit scopes, structural coordinates,
#' integration rule, calibration artifact, optimizer states, and recomputed
#' target/Riesz evidence.
#'
#' @param role One of the frozen level-A, fixed-level-B, or repolished-level-B
#'   branches.
#' @param objective Named full-and-fold mean objectives.
#' @param score Named list of structural-orthonormal full-batch scores.
#' @param covariance Named list of raw-scale structural covariance matrices.
#' @param target_evidence Intact recomputed target evidence.
#' @param optimizer_evidence For level A and repolished B, exactly one selected
#'   intact optimizer object per full/fold scope. Must be `NULL` for fixed B.
#' @param protocol_mode `"calibration"` or `"production"`.
#' @param calibration_artifact Intact exact Step-3 artifact. Required for
#'   production.
#' @return An intact fingerprinted integration evaluation.
#' @export
scmix_v22_integration_evaluation <- function(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract,
    role = c("level_a_production", "level_b_fixed_parameters",
             "level_b_repolished"),
    objective, score, covariance, target_evidence,
    optimizer_evidence = NULL,
    calibration_artifact = NULL,
    protocol_mode = c("calibration", "production")) {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  role <- match.arg(role)
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, calibration_artifact,
    require_calibration = identical(mode, "production"))
  if (!.scmix_v22_comp_intact(
      target_evidence, "scmix_v22_integration_target_evidence")) {
    stop("`target_evidence` must be intact and fingerprinted.",
         call. = FALSE)
  }
  expected_rule <- if (identical(role, "level_a_production")) {
    bound$integration$level_a_rule
  } else bound$integration$level_b_rule
  expected_links <- c(
    identical(target_evidence$analysis_contract_fingerprint,
              bound$analysis$fingerprint),
    identical(target_evidence$registry_fingerprint,
              bound$registry_fingerprint),
    identical(target_evidence$integration_contract_fingerprint,
              bound$integration$fingerprint),
    identical(target_evidence$structural_parameter_contract_fingerprint,
              bound$structural$fingerprint),
    identical(target_evidence$fit_registry_fingerprint,
              bound$fit_registry$fingerprint),
    identical(target_evidence$specification_id,
              bound$analysis$specification_id),
    identical(target_evidence$integration_rule_fingerprint,
              expected_rule$fingerprint),
    identical(target_evidence$target_ids,
              .scmix_v22_comp_target_ids(bound)),
    identical(target_evidence$primitive_ids,
              .scmix_v22_comp_primitive_ids(bound)),
    identical(target_evidence$integrated_target_ids,
              bound$integration$integrated_target_ids),
    identical(target_evidence$integrated_primitive_ids,
              bound$integration$integrated_primitive_ids),
    identical(target_evidence$riesz_integration_rule_fingerprint,
              expected_rule$fingerprint))
  if (!all(expected_links)) {
    stop("Target evidence is not bound to the exact analysis, registry, fit scopes, structural contract, and integration rule.",
         call. = FALSE)
  }
  scopes <- .scmix_v22_comp_scope_ids(bound)
  objective <- .scmix_v22_comp_named_numeric(objective, "objective")
  if (!setequal(names(objective), scopes)) {
    stop("Objective evidence must cover the full fit and every registered outer fold.",
         call. = FALSE)
  }
  objective <- objective[scopes]
  if (!is.list(score) || is.null(names(score)) ||
      !setequal(names(score), scopes) || anyDuplicated(names(score))) {
    stop("Score evidence must cover the exact registered fit scopes.",
         call. = FALSE)
  }
  score <- score[scopes]
  score_ids <- bound$structural$score_coordinate_ids
  if (any(vapply(score, function(x)
    !is.numeric(x) || any(!is.finite(x)) ||
      !identical(names(x), score_ids), logical(1L)))) {
    stop("Every score must use the exact signed structural-orthonormal coordinates.",
         call. = FALSE)
  }
  if (!is.list(covariance) || is.null(names(covariance)) ||
      !setequal(names(covariance), scopes) ||
      anyDuplicated(names(covariance))) {
    stop("Covariance evidence must cover the exact registered fit scopes.",
         call. = FALSE)
  }
  covariance <- covariance[scopes]
  covariance_ids <- bound$structural$covariance_coordinate_ids
  p <- bound$structural$covariance_dimension
  covariance_ok <- vapply(covariance, function(x) {
    is.matrix(x) && is.numeric(x) && identical(dim(x), c(p, p)) &&
      identical(rownames(x), covariance_ids) &&
      identical(colnames(x), covariance_ids) && all(is.finite(x)) &&
      max(abs(x - t(x))) <= sqrt(.Machine$double.eps) *
        max(1, max(abs(x))) &&
      min(eigen((x + t(x)) / 2, symmetric = TRUE,
                only.values = TRUE)$values) >=
        -sqrt(.Machine$double.eps) * max(1, max(abs(x)))
  }, logical(1L))
  if (!all(covariance_ok)) {
    stop("Every covariance must be the signed p-by-p, named, symmetric, positive-semidefinite structural covariance; q-by-q is invalid.",
         call. = FALSE)
  }
  states <- target_evidence$parameter_state_hashes
  required_optimizer_role <- switch(
    role, level_a_production = "level_a_production_start",
    level_b_repolished = "level_b_repolish", NULL)
  optimizer_fingerprints <- NULL
  initial_states <- NULL
  if (is.null(required_optimizer_role)) {
    if (!is.null(optimizer_evidence)) {
      stop("The fixed-parameter branch cannot supply optimizer evidence.",
           call. = FALSE)
    }
  } else {
    if (!is.list(optimizer_evidence) || length(optimizer_evidence) !=
        length(scopes) || !all(vapply(
          optimizer_evidence, .scmix_v22_comp_intact, logical(1L),
          class_name = "scmix_v22_start_optimizer_evidence"))) {
      stop("This branch requires one intact selected optimizer artifact per fit scope.",
           call. = FALSE)
    }
    by_scope <- vapply(optimizer_evidence, `[[`, character(1L),
                       "fit_scope_id")
    if (anyDuplicated(by_scope) || !setequal(by_scope, scopes)) {
      stop("Optimizer evidence must cover every and only registered fit scope.",
           call. = FALSE)
    }
    optimizer_evidence <- optimizer_evidence[match(scopes, by_scope)]
    ok <- vapply(seq_along(scopes), function(j) {
      x <- optimizer_evidence[[j]]
      identical(x$analysis_contract_fingerprint, bound$analysis$fingerprint) &&
        identical(x$registry_fingerprint, bound$registry_fingerprint) &&
        identical(x$integration_contract_fingerprint,
                  bound$integration$fingerprint) &&
        identical(x$structural_parameter_contract_fingerprint,
                  bound$structural$fingerprint) &&
        identical(x$integration_rule_fingerprint,
                  expected_rule$fingerprint) &&
        identical(x$fit_scope_id, scopes[[j]]) &&
        identical(x$state_hash, unname(states[[j]])) &&
        identical(x$calibration_artifact_fingerprint,
                  if (is.null(bound$calibration)) NULL else
                    bound$calibration$fingerprint) &&
        identical(x$role, required_optimizer_role) && isTRUE(x$selected) &&
        identical(x$optimizer_protocol_mode, mode) &&
        identical(x$dtype, "float64") && isTRUE(x$polish_completed) &&
        isTRUE(x$nonworsening) &&
        identical(x$objective_definition_id,
                  "mean_negative_complete_sequence_loglik_per_respondent_plus_declared_penalty_in_nats")
    }, logical(1L))
    if (!all(ok)) {
      stop("Optimizer artifacts are not the exact selected, polished, float64 states for this branch.",
           call. = FALSE)
    }
    optimizer_objective <- stats::setNames(vapply(
      optimizer_evidence, `[[`, numeric(1L), "objective"), scopes)
    if (!identical(as.numeric(objective), as.numeric(optimizer_objective))) {
      stop("Evaluation objectives must equal the returned objectives of the exact optimizer states.",
           call. = FALSE)
    }
    optimizer_fingerprints <- vapply(
      optimizer_evidence, `[[`, character(1L), "fingerprint")
    names(optimizer_fingerprints) <- scopes
    if (identical(role, "level_b_repolished")) {
      initial_states <- vapply(
        optimizer_evidence, `[[`, character(1L), "initial_state_hash")
      names(initial_states) <- scopes
    }
  }
  .scmix_v22_comp_sign(list(
    protocol_id = .scmix_v22_comp_protocol_id,
    protocol_mode = mode, role = role,
    analysis_signature = bound$analysis$analysis_signature,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    calibration_artifact_fingerprint = if (is.null(bound$calibration)) NULL else
      bound$calibration$fingerprint,
    calibration_hash = bound$analysis$calibration_hash,
    application_id = bound$analysis$application_id,
    specification_id = bound$analysis$specification_id,
    q = bound$structural$q, fit_scope_ids = scopes,
    integration_rule = expected_rule,
    integration_rule_fingerprint = expected_rule$fingerprint,
    parameter_state_hashes = states,
    initial_parameter_state_hashes = initial_states,
    optimizer_evidence_fingerprints = optimizer_fingerprints,
    objective = objective,
    objective_definition_id =
      "mean_negative_complete_sequence_loglik_per_respondent_plus_declared_penalty_in_nats",
    score = score,
    score_definition_id =
      "block_scaled_full_batch_score_structural_orthonormal",
    covariance = covariance,
    covariance_definition_id = "raw_scale_structural_covariance",
    covariance_coordinate_ids = covariance_ids,
    target_evidence = target_evidence,
    target_evidence_fingerprint = target_evidence$fingerprint,
    targets = target_evidence$targets,
    standard_errors = target_evidence$standard_errors,
    influence = target_evidence$influence,
    respondent_id = target_evidence$respondent_id,
    influence_sd = target_evidence$influence_sd,
    influence_definition_id = target_evidence$influence_definition_id,
    riesz_solution_hashes = target_evidence$riesz_solution_hashes,
    riesz_integration_rule_fingerprint =
      target_evidence$riesz_integration_rule_fingerprint,
    uses_numerical_integration =
      bound$integration$uses_numerical_integration,
    integrated_target_ids = bound$integration$integrated_target_ids,
    integrated_primitive_ids = bound$integration$integrated_primitive_ids),
    "scmix_v22_integration_evaluation")
}

.scmix_v22_comp_validate_evaluation <- function(x, what,
                                                 require_se = FALSE) {
  if (!.scmix_v22_comp_intact(x, "scmix_v22_integration_evaluation")) {
    stop("`", what, "` must be an intact fingerprinted integration evaluation produced by scmix_v22_integration_evaluation().",
         call. = FALSE)
  }
  if (!identical(x$protocol_id, .scmix_v22_comp_protocol_id) ||
      !.scmix_v22_comp_intact(
        x$integration_rule, "scmix_v22_integration_rule") ||
      !.scmix_v22_comp_intact(
        x$target_evidence, "scmix_v22_integration_target_evidence") ||
      !identical(x$target_evidence$fingerprint,
                 x$target_evidence_fingerprint) ||
      !identical(x$integration_rule$fingerprint,
                 x$integration_rule_fingerprint) ||
      !identical(x$target_evidence$parameter_state_hashes,
                 x$parameter_state_hashes) ||
      !identical(x$target_evidence$targets, x$targets) ||
      !identical(x$target_evidence$standard_errors, x$standard_errors) ||
      !identical(x$target_evidence$influence, x$influence) ||
      !identical(x$target_evidence$riesz_solution_hashes,
                 x$riesz_solution_hashes) ||
      !all(vapply(x$target_evidence$riesz_solution_artifacts,
                  .scmix_v22_comp_intact, logical(1L),
                  class_name = "scmix_v22_integration_riesz_solution")) ||
      !identical(x$influence_definition_id,
                 "centered_raw_one_step_respondent_sequence")) {
    stop("`", what, "` has broken rule, target, Riesz, or state bindings.",
         call. = FALSE)
  }
  if (isTRUE(require_se) &&
      (any(!is.finite(x$standard_errors)) || any(x$standard_errors <= 0))) {
    stop("Level-A standard errors must be finite and positive.",
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
#' @param level_a,fixed_b,repolished_b Intact artifacts returned by
#'   [scmix_v22_integration_evaluation()].
#' @param analysis_contract,registry,integration_contract,
#'   structural_parameter_contract Intact frozen contracts.
#' @param calibration_artifact Intact exact Step-3 calibration artifact.
#' @param protocol_mode Either `"calibration"` or `"production"`.
#' @return A structured two-branch artifact, worse-of metrics, and I001--I005
#'   gate rows.
#' @export
scmix_v22_integration_compare <- function(
    level_a, fixed_b, repolished_b, analysis_contract, registry,
    integration_contract, structural_parameter_contract,
    calibration_artifact = NULL,
    protocol_mode = c("calibration", "production")) {
  mode <- .scmix_v22_comp_mode(protocol_mode)
  bound <- .scmix_v22_comp_bind_contracts(
    analysis_contract, registry, integration_contract,
    structural_parameter_contract, calibration_artifact,
    require_calibration = identical(mode, "production"))
  a <- .scmix_v22_comp_validate_evaluation(level_a, "level_a", TRUE)
  f <- .scmix_v22_comp_validate_evaluation(fixed_b, "fixed_b", FALSE)
  r <- .scmix_v22_comp_validate_evaluation(repolished_b, "repolished_b", FALSE)
  if (!identical(a$role, "level_a_production") ||
      !identical(f$role, "level_b_fixed_parameters") ||
      !identical(r$role, "level_b_repolished")) {
    stop("Integration branch roles are not the frozen level-A/two-branch roles.",
         call. = FALSE)
  }
  manifest_fields <- c("analysis_signature", "analysis_contract_fingerprint",
                       "registry_fingerprint",
                       "integration_contract_fingerprint",
                       "structural_parameter_contract_fingerprint",
                       "fit_registry_fingerprint", "calibration_hash",
                       "application_id", "specification_id", "q",
                       "fit_scope_ids",
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
  exact_bindings <- c(
    identical(a$analysis_contract_fingerprint, bound$analysis$fingerprint),
    identical(a$registry_fingerprint, bound$registry_fingerprint),
    identical(a$integration_contract_fingerprint,
              bound$integration$fingerprint),
    identical(a$structural_parameter_contract_fingerprint,
              bound$structural$fingerprint),
    identical(a$fit_registry_fingerprint, bound$fit_registry$fingerprint),
    identical(a$integration_rule_fingerprint,
              bound$integration$level_a_rule$fingerprint),
    identical(f$integration_rule_fingerprint,
              bound$integration$level_b_rule$fingerprint),
    identical(r$integration_rule_fingerprint,
              bound$integration$level_b_rule$fingerprint),
    identical(a$calibration_artifact_fingerprint,
              if (is.null(bound$calibration)) NULL else
                bound$calibration$fingerprint),
    identical(f$calibration_artifact_fingerprint,
              a$calibration_artifact_fingerprint),
    identical(r$calibration_artifact_fingerprint,
              a$calibration_artifact_fingerprint),
    identical(a$protocol_mode, mode), identical(f$protocol_mode, mode),
    identical(r$protocol_mode, mode))
  if (!all(exact_bindings)) {
    stop("Integration evaluations are not bound to the exact contracts, calibration, modes, and signed A/B rules.",
         call. = FALSE)
  }
  if (isTRUE(bound$integration$nesting_supported) &&
      !isTRUE(bound$integration$exact_nesting_verified)) {
    stop("The signed level-B integration contract lacks exact nesting evidence.",
         call. = FALSE)
  }
  if (!identical(a$parameter_state_hashes, f$parameter_state_hashes)) {
    stop("The fixed-parameter branch did not use the exact level-A states.",
         call. = FALSE)
  }
  if (is.null(r$initial_parameter_state_hashes) ||
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
  integrated_primitives <- bound$integration$integrated_primitive_ids
  if (length(integrated_primitives) &&
      (any(a$riesz_solution_hashes[integrated_primitives] ==
           f$riesz_solution_hashes[integrated_primitives]) ||
       any(a$riesz_solution_hashes[integrated_primitives] ==
           r$riesz_solution_hashes[integrated_primitives]))) {
    stop("Integrated primitive Riesz artifacts were not recomputed and rule-bound under level B.",
         call. = FALSE)
  }
  .scmix_v22_comp_compare_shapes(a, f, "fixed_b")
  .scmix_v22_comp_compare_shapes(a, r, "repolished_b")
  if (!identical(names(a$parameter_state_hashes), names(a$objective))) {
    stop("Level-A parameter-state hashes must name every full/fold fit scope.",
         call. = FALSE)
  }
  target_primitives <- stats::setNames(lapply(
    bound$registry, `[[`, "primitive_registry_ids"),
    .scmix_v22_comp_target_ids(bound))
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
  cut <- function(name) {
    if (is.null(bound$calibration)) .scmix_v22_pending_cutoff else
      bound$calibration$constants[[name]]
  }
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
    protocol_id = .scmix_v22_comp_protocol_id, protocol_mode = mode,
    analysis_signature = a$analysis_signature,
    analysis_contract_fingerprint = bound$analysis$fingerprint,
    registry_fingerprint = bound$registry_fingerprint,
    integration_contract_fingerprint = bound$integration$fingerprint,
    structural_parameter_contract_fingerprint = bound$structural$fingerprint,
    fit_registry_fingerprint = bound$fit_registry$fingerprint,
    calibration_artifact_fingerprint = if (is.null(bound$calibration)) NULL else
      bound$calibration$fingerprint,
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
  if (!.scmix_v22_comp_intact(
      level_b_rule, "scmix_v22_integration_rule") ||
      !identical(level_b_rule$level, "b")) {
    stop("`level_b_rule` must be an intact signed level-B rule.",
         call. = FALSE)
  }
  fixed_b <- fixed_parameter_evaluator(level_a, level_b_rule)
  repolished_fit <- repolisher(level_a, level_b_rule)
  repolished_b <- repolished_evaluator(repolished_fit, level_b_rule)
  if (!is.list(fixed_b) || !is.list(repolished_b) ||
      !identical(fixed_b$integration_rule_fingerprint,
                 level_b_rule$fingerprint) ||
      !identical(repolished_b$integration_rule_fingerprint,
                 level_b_rule$fingerprint)) {
    stop("Integration callbacks did not use the supplied signed level-B rule.",
         call. = FALSE)
  }
  out <- scmix_v22_integration_compare(
    level_a = level_a, fixed_b = fixed_b,
    repolished_b = repolished_b, ...)
  payload <- unclass(out)
  payload$fingerprint <- NULL
  payload$callback_contract <- list(
    fixed_parameter_evaluator_calls = 1L,
    repolisher_calls = 1L, repolished_evaluator_calls = 1L,
    signed_level_b_rule = level_b_rule)
  .scmix_v22_comp_sign(payload, "scmix_v22_integration_refinement")
}

#' Authenticate a v2.2 computation artifact for the inference finalizer
#'
#' @param artifact An F007, F008, or I001--I005 artifact returned by this file.
#' @param analysis_contract,registry Exact signed analysis and target registry.
#' @param calibration_artifact Exact signed Step-3 calibration artifact.
#' @return The authenticated gate rows, with artifact metadata as attributes.
#' @export
scmix_v22_validate_computation_artifact <- function(
    artifact, analysis_contract, registry, calibration_artifact) {
  analysis_contract <- .scmix_v22_comp_contract(analysis_contract)
  registry_info <- .scmix_v22_comp_registry(registry)
  if (!.scmix_v22_comp_validate_calibration(
      calibration_artifact, analysis_contract, require_complete = TRUE)) {
    stop("The exact analysis-linked calibration artifact is required.",
         call. = FALSE)
  }
  type <- if (.scmix_v22_comp_intact(
      artifact, "scmix_v22_nested_fit_stability")) {
    "F007"
  } else if (.scmix_v22_comp_intact(
      artifact, "scmix_v22_multistart_stability")) {
    "F008"
  } else if (.scmix_v22_comp_intact(
      artifact, "scmix_v22_integration_refinement")) {
    "integration"
  } else {
    stop("Unknown, damaged, or unsigned v2.2 computation artifact.",
         call. = FALSE)
  }
  if (!identical(artifact$protocol_id, .scmix_v22_comp_protocol_id) ||
      !identical(artifact$protocol_mode, "production")) {
    stop("Only an intact production-mode v2.2 computation artifact is admissible.",
         call. = FALSE)
  }
  if (identical(type, "F007")) {
    linked <- identical(
      artifact$full_analysis_contract_fingerprint,
      analysis_contract$fingerprint) &&
      identical(artifact$full_registry_fingerprint,
                registry_info$fingerprint) &&
      identical(artifact$full_calibration_artifact_fingerprint,
                calibration_artifact$fingerprint)
    gates <- artifact$gate
    expected_ids <- "F007"
  } else {
    linked <- identical(artifact$analysis_contract_fingerprint,
                        analysis_contract$fingerprint) &&
      identical(artifact$registry_fingerprint,
                registry_info$fingerprint) &&
      identical(artifact$calibration_artifact_fingerprint,
                calibration_artifact$fingerprint)
    gates <- artifact$gates
    expected_ids <- if (identical(type, "F008")) "F008" else
      sprintf("I%03d", 1:5)
  }
  if (!linked || !is.data.frame(gates) || !nrow(gates) ||
      !setequal(unique(gates$gate_id), expected_ids)) {
    stop("Computation artifact is not linked to this exact analysis, registry, calibration, and gate family.",
         call. = FALSE)
  }
  target_ids <- vapply(registry_info$payload, `[[`, character(1L),
                       "target_id")
  if (identical(type, "F008") &&
      !setequal(gates$scope_id, target_ids)) {
    stop("F008 evidence does not cover every registered target.",
         call. = FALSE)
  }
  if (identical(type, "integration")) {
    for (id in c("I001", "I002")) {
      if (!setequal(gates$scope_id[gates$gate_id == id], target_ids)) {
        stop(id, " evidence does not cover every registered target.",
             call. = FALSE)
      }
    }
  }
  attr(gates, "computation_artifact_type") <- type
  attr(gates, "computation_artifact_fingerprint") <- artifact$fingerprint
  gates
}
