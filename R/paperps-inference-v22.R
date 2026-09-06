## Protocol-v2.2 target registration, range-respecting updates, constrained
## confidence sets, common respondent multipliers, and fail-closed statuses.
##
## This file is a compatibility layer around scmix_dml().  It deliberately
## leaves the raw respondent-sequence DML calculation unchanged.  A v2.2
## result is created only by scmix_v22_finalize(), after the caller supplies a
## signed target registry, the full-sample structural plug-in when available,
## a common multiplier bank, calibration constants, and the remaining gate
## record.  Existing scmix_dml fields therefore remain readable, but they are
## never silently relabeled as the paper's reported estimator.

.scmix_v22_protocol_id <- "paperps-inference-2026-09-05-v2.2"
.scmix_v22_required_gate_ids <- c(
  "P001", "P002", "D001", "D002",
  sprintf("F%03d", 1:8), sprintf("I%03d", 1:5),
  sprintf("L%03d", 1:2), sprintf("N%03d", 1:3),
  sprintf("G%03d", 1:6), sprintf("R%03d", 1:5),
  sprintf("C%03d", 1:4), sprintf("M%03d", 1:3), "S001")
.scmix_v22_gate_fields <- c(
  "gate_id", "category", "scope", "severity", "state", "statistic",
  "threshold", "comparator", "units", "provenance", "message", "scope_id")
.scmix_v22_required_calibration_names <- c(
  "subgroup_interior_margin",
  "directional_heterogeneity_regularity_margin",
  "mean_tradeoff_denominator_margin",
  "chart_discrepancy_cutoff", "nonlinear_remainder_cutoff",
  "interior_distance_cutoff", "kkt_cutoff",
  "integration_target_se_cutoff", "integration_if_l2_cutoff",
  "integration_objective_relative_cutoff",
  "integration_score_relative_cutoff",
  "integration_covariance_relative_cutoff",
  "near_optimum_objective_cutoff", "multistart_target_se_cutoff",
  "nested_fit_relative_cutoff", "loading_probability_invariance_cutoff",
  "active_eigenvalue_standardized_cutoff",
  "generalized_information_margin",
  "directional_variance_standardized_cutoff",
  "total_heterogeneity_standardized_cutoff",
  "denominator_standardized_cutoff", "derivative_cutoff",
  "null_space_materiality_cutoff", "numerical_rank_tolerance",
  "defect_cutoff", "ridge_cutoff")
.scmix_v22_local_gate_ids <- c(
  "P001", "P002", "N002", "G006", sprintf("R%03d", 1:5),
  sprintf("C%03d", 1:4), sprintf("M%03d", 1:3))
.scmix_v22_conditional_gate_ids <- c(
  "F007", "F008", sprintf("I%03d", 1:5), sprintf("L%03d", 1:2),
  "G002", "G003", "G004", "G006", sprintf("R%03d", 1:5),
  sprintf("C%03d", 1:4), "M002", "M003")
.scmix_v22_computation_gate_ids <- c(
  "F007", "F008", sprintf("I%03d", 1:5))

.scmix_v22_gate_dictionary <- local({
  make <- function(ids, category, scope, severity, comparator, units) data.frame(
    gate_id = ids, category = category, scope = scope,
    severity = severity, comparator = comparator, units = units,
    stringsAsFactors = FALSE)
  rbind(
    make(c("P001", "P002"), "protocol_integrity", "analysis", "hard_fit",
         "equal", "boolean"),
    make("D001", "design_data", "target", "interpretation",
         "contains", "support_cells"),
    make("D002", "design_data", "sample", "interpretation",
         "consistent_with", "audit_record"),
    make(sprintf("F%03d", 1:4), "fit_computation", "fit", "hard_fit",
         "equal", "boolean"),
    make("F005", "fit_computation", "fit_block", "hard_fit",
         "less_than_or_equal", "dimensionless"),
    make("F006", "fit_computation", "fit", "hard_fit", "equal", "boolean"),
    make("F007", "fit_computation", "nested_fit", "hard_fit",
         "less_than_or_equal", "relative_mean_objective"),
    make("F008", "fit_computation", "target", "hard_target",
         "less_than_or_equal", "standard_errors"),
    make(c("I001", "I002"), "numerical_integration", "target",
         "hard_target", "less_than_or_equal",
         c("standard_errors", "influence_standard_deviations")),
    make(sprintf("I%03d", 3:5), "numerical_integration", "fit", "hard_fit",
         "less_than_or_equal",
         c("relative_mean_objective", "relative_normalized_gradient",
           "relative_frobenius_norm")),
    make("L001", "loading_contract", "fit", "hard_fit", "equal", "boolean"),
    make("L002", "loading_contract", "fit", "hard_fit",
         "less_than_or_equal", "maximum_absolute_probability_difference"),
    make(c("N001", "N002"), "nuisance_verification", "analysis",
         "hard_fit", "equal", "boolean"),
    make("N003", "nuisance_verification", "fit", "hard_fit",
         "equal", "boolean"),
    make("G001", "identification_regularity", "specification", "hard_fit",
         "equal", "boolean"),
    make("G002", "identification_regularity", "fit", "hard_fit",
         "equal", "boolean"),
    make("G003", "identification_regularity",
         "target_parameter_dependencies", "hard_target", "equal", "boolean"),
    make("G004", "identification_regularity", "fit", "hard_fit",
         "greater_than_or_equal", "standard_errors"),
    make("G005", "identification_regularity", "target", "hard_target",
         "greater_than_or_equal", "declared_structural_norm"),
    make("G006", "identification_regularity", "target", "hard_target",
         "greater_than_or_equal", "standard_errors"),
    make("R001", "inverse_problem", "primitive", "hard_target",
         "equal", "boolean"),
    make("R002", "inverse_problem", "primitive", "hard_target",
         "less_than_or_equal", "relative_gradient_norm"),
    make("R003", "inverse_problem", "primitive", "hard_target",
         "less_than_or_equal", "influence_standard_deviations"),
    make("R004", "inverse_problem", "primitive", "hard_target",
         "less_than_or_equal", "standard_errors"),
    make("R005", "inverse_problem", "primitive", "hard_target",
         "equal", "boolean"),
    make(c("C001", "C002"), "target_constraint", "target",
         "hard_target", "equal", "boolean"),
    make("C003", "target_constraint", "target", "hard_target",
         "less_than_or_equal", "standard_errors"),
    make("C004", "target_constraint", "target", "warning",
         "equal", "boolean"),
    make("M001", "resampling_multiplicity", "primitive", "hard_target",
         "equal", "boolean"),
    make(c("M002", "M003"), "resampling_multiplicity", "family",
         "hard_family", "equal", "boolean"),
    make("S001", "specification_assessment", "application",
         "interpretation", "equal", "boolean")
  )
})

.scmix_v22_choice <- function(x, choices, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !x %in% choices) {
    stop(what, " must be one of: ", paste(choices, collapse = ", "), ".",
         call. = FALSE)
  }
  x
}

.scmix_v22_text <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x))) {
    stop(what, " must be a nonempty character scalar.", call. = FALSE)
  }
  x
}

.scmix_v22_names <- function(x, what, expected = NULL) {
  if (!is.character(x) || !length(x) || anyNA(x) ||
      any(!nzchar(trimws(x))) || anyDuplicated(x)) {
    stop(what, " must contain unique nonempty names.", call. = FALSE)
  }
  if (!is.null(expected) && length(x) != expected) {
    stop(what, " must have length ", expected, ".", call. = FALSE)
  }
  x
}

.scmix_v22_default_domain <- function(target_type) {
  switch(
    target_type,
    unrestricted_scalar = list(kind = "unrestricted", chart = "identity"),
    positive_scalar = list(kind = "lower_bounded", lower = 0,
                           chart = "shifted_log"),
    probability =,
    preference_direction_share =,
    compensating_benefit_share = list(
      kind = "bounded", lower = 0, upper = 1, chart = "scaled_logit"),
    subgroup_share = list(
      kind = "subgroup_share",
      constraints = c("0 <= numerator", "numerator <= denominator",
                      "denominator <= 1"),
      chart = "logit_share_plus_logit_denominator"),
    directional_heterogeneity = list(
      kind = "directional_heterogeneity",
      constraints = c("explained_variance >= 0",
                      "residual_variance >= 0"),
      target_domain = "explained_variance + residual_variance > 0",
      chart = "log_explained_variance_plus_log_residual_variance"),
    mean_tradeoff_mrs_ratio = list(
      kind = "ratio", denominator_excludes = 0, chart = "identity"),
    stop("No default domain for target type `", target_type, "`.",
         call. = FALSE)
  )
}

#' Register one protocol-v2.2 target
#'
#' The registry separates the supporting moment columns stored by
#' [scmix_dml()] from the smallest inferential primitive vector.  Directional
#' heterogeneity, for example, uses supporting columns for the mean, second
#' moment, and residual variance, but its inferential primitive IDs correspond
#' to `(H_Z, H_R)`.
#'
#' @param target_id, family_id Stable signed identifiers.
#' @param target_type One of the target types in the v2.2 protocol.
#' @param supporting_names Columns in the source `scmix_dml` object.
#' @param primitive_registry_ids Stable IDs for the smallest nonredundant
#'   primitive vector.
#' @param orientation,units Prespecified target orientation and units.
#' @param parameter_dependencies Named structural blocks on which the target
#'   depends.
#' @param family_primitive_registry_ids Complete frozen set of unique scalar
#'   primitive IDs in `family_id`. This lets a final collector verify that
#'   targets split across jobs still use the full family for max-t inference.
#' @param confirmatory_status `"confirmatory"`, `"sensitivity"`, or
#'   `"outcome_informed"`.
#' @param uses_numerical_integration Whether this target requires the frozen
#'   target-level numerical-integration refinement checks.
#' @param domain Must be `NULL`. The v2.2 target domains are frozen. Changing
#'   one requires a new protocol ID rather than an in-protocol amendment.
#' @return A validated `scmix_v22_target_spec`.
#' @export
scmix_v22_target_spec <- function(
    target_id, family_id,
    target_type = c(
      "unrestricted_scalar", "positive_scalar", "probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_share", "directional_heterogeneity",
      "mean_tradeoff_mrs_ratio"),
    supporting_names, primitive_registry_ids,
    orientation, units, parameter_dependencies,
    family_primitive_registry_ids = primitive_registry_ids,
    family_target_ids = target_id,
    confirmatory_status = c("confirmatory", "sensitivity",
                           "outcome_informed"),
    uses_numerical_integration = TRUE,
    domain = NULL, reference_rule = "none",
    influence_definition = "centered_raw_one_step_respondent_sequence",
    primitive_estimands = NULL, primitive_orientations = NULL,
    primitive_units = NULL, primitive_reference_rules = NULL,
    primitive_influence_definitions = NULL) {
  target_type <- match.arg(target_type)
  confirmatory_status <- match.arg(confirmatory_status)
  if (!is.logical(uses_numerical_integration) ||
      length(uses_numerical_integration) != 1L ||
      is.na(uses_numerical_integration)) {
    stop("uses_numerical_integration must be one nonmissing logical value.",
         call. = FALSE)
  }
  target_id <- .scmix_v22_text(target_id, "target_id")
  family_id <- .scmix_v22_text(family_id, "family_id")
  orientation <- .scmix_v22_text(orientation, "orientation")
  units <- .scmix_v22_text(units, "units")
  reference_rule <- .scmix_v22_text(reference_rule, "reference_rule")
  influence_definition <- .scmix_v22_text(
    influence_definition, "influence_definition")
  supporting_names <- .scmix_v22_names(supporting_names,
                                        "supporting_names")
  expected_support <- switch(
    target_type,
    subgroup_share = 2L,
    directional_heterogeneity = 3L,
    mean_tradeoff_mrs_ratio = 2L,
    1L
  )
  if (length(supporting_names) != expected_support) {
    stop("Target type `", target_type, "` requires ", expected_support,
         " supporting columns.", call. = FALSE)
  }
  expected_primitives <- if (target_type %in% c(
    "subgroup_share", "directional_heterogeneity",
    "mean_tradeoff_mrs_ratio")) 2L else 1L
  primitive_registry_ids <- .scmix_v22_names(
    primitive_registry_ids, "primitive_registry_ids", expected_primitives)
  fill_primitive <- function(x, default, what) {
    if (is.null(x)) x <- rep_len(default, expected_primitives)
    if (!is.character(x) || length(x) != expected_primitives || anyNA(x) ||
        any(!nzchar(trimws(x)))) {
      stop(what, " must contain one nonempty value per primitive.",
           call. = FALSE)
    }
    unname(x)
  }
  primitive_estimands <- fill_primitive(
    primitive_estimands,
    paste0(target_type, " primitive ", seq_len(expected_primitives)),
    "primitive_estimands")
  primitive_orientations <- fill_primitive(
    primitive_orientations, orientation, "primitive_orientations")
  primitive_units <- fill_primitive(primitive_units, units, "primitive_units")
  primitive_reference_rules <- fill_primitive(
    primitive_reference_rules, reference_rule, "primitive_reference_rules")
  primitive_influence_definitions <- fill_primitive(
    primitive_influence_definitions, influence_definition,
    "primitive_influence_definitions")
  family_primitive_registry_ids <- .scmix_v22_names(
    family_primitive_registry_ids, "family_primitive_registry_ids")
  if (!all(primitive_registry_ids %in% family_primitive_registry_ids)) {
    stop("family_primitive_registry_ids must include every target primitive.",
         call. = FALSE)
  }
  family_primitive_registry_ids <- sort(family_primitive_registry_ids,
                                        method = "radix")
  family_target_ids <- sort(.scmix_v22_names(
    family_target_ids, "family_target_ids"), method = "radix")
  if (!target_id %in% family_target_ids) {
    stop("family_target_ids must include target_id.", call. = FALSE)
  }
  parameter_dependencies <- .scmix_v22_names(
    parameter_dependencies, "parameter_dependencies")
  if (!is.null(domain)) {
    stop("The v2.2 target domain is frozen; changing it requires a new protocol ID.",
         call. = FALSE)
  }
  domain <- .scmix_v22_default_domain(target_type)
  out <- list(
    protocol_id = .scmix_v22_protocol_id,
    target_id = target_id,
    target_type = target_type,
    target_orientation = orientation,
    target_units = units,
    target_reference_rule = reference_rule,
    target_influence_definition = influence_definition,
    target_domain = domain,
    target_domain_amendment = NULL,
    target_family_id = family_id,
    target_parameter_dependencies = parameter_dependencies,
    supporting_names = supporting_names,
    primitive_registry_ids = primitive_registry_ids,
    primitive_estimands = primitive_estimands,
    primitive_orientations = primitive_orientations,
    primitive_units = primitive_units,
    primitive_reference_rules = primitive_reference_rules,
    primitive_influence_definitions = primitive_influence_definitions,
    family_primitive_registry_ids = family_primitive_registry_ids,
    family_target_ids = family_target_ids,
    target_to_primitive_map = switch(
      target_type,
      subgroup_share = "numerator / denominator",
      directional_heterogeneity =
        "explained_variance / (explained_variance + residual_variance)",
      mean_tradeoff_mrs_ratio = "-numerator / denominator",
      "identity"),
    uses_numerical_integration = uses_numerical_integration,
    confirmatory_status = confirmatory_status
  )
  out$fingerprint <- .scmix_v22_fingerprint(out)
  class(out) <- c("scmix_v22_target_spec", "list")
  out
}

.scmix_v22_with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv,
                      inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

.scmix_v22_fingerprint <- function(x) {
  path <- tempfile("scmix-v22-fingerprint-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 3, compress = FALSE)
  unname(as.character(tools::md5sum(path)))
}

.scmix_v22_intact <- function(x, class_name) {
  if (!inherits(x, class_name)) return(FALSE)
  payload <- unclass(x); recorded <- payload$fingerprint
  payload$fingerprint <- NULL
  is.character(recorded) && length(recorded) == 1L && !is.na(recorded) &&
    identical(recorded, .scmix_v22_fingerprint(payload))
}

.scmix_v22_normalize_calibration <- function(values,
                                              require_complete = FALSE) {
  if (is.numeric(values) && !is.null(names(values))) values <- as.list(values)
  if (!is.list(values) || !length(values) || is.null(names(values)) ||
      anyNA(names(values)) || any(!nzchar(names(values))) ||
      anyDuplicated(names(values))) {
    stop("values must be a uniquely named nonempty list of constants.",
         call. = FALSE)
  }
  unknown <- setdiff(names(values), .scmix_v22_required_calibration_names)
  if (length(unknown)) {
    stop("Unknown v2.2 calibration constant(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  valid <- vapply(values, function(x)
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0,
    logical(1L))
  if (!all(valid)) {
    stop("Every calibration constant must be one finite nonnegative number.",
         call. = FALSE)
  }
  if (isTRUE(require_complete)) {
    missing <- setdiff(.scmix_v22_required_calibration_names, names(values))
    if (length(missing)) {
      stop("Calibration is incomplete; missing: ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
  }
  values[sort(names(values), method = "radix")]
}

#' Hash the exact frozen v2.2 calibration values
#'
#' The analysis contract stores this hash. It binds the exact numeric constants
#' and their immutable source-file hash before any production finalizer can use
#' them.
#'
#' @param values Named numeric constants from Step 3.
#' @param source_hash Immutable hash of the calibrated constants file.
#' @return A scalar fingerprint to place in
#'   `scmix_v22_analysis_contract(calibration_hash=)`.
#' @export
scmix_v22_calibration_hash <- function(values, source_hash) {
  constants <- .scmix_v22_normalize_calibration(values)
  source_hash <- .scmix_v22_text(source_hash, "source_hash")
  .scmix_v22_fingerprint(list(
    protocol_id = .scmix_v22_protocol_id,
    source_hash = source_hash, constants = constants))
}

.scmix_v22_approved_calibration_manifest <- function() {
  path <- system.file(
    "protocol", "inference-v2.2-calibration.dcf", package = "sconjoint")
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  manifest <- try(utils::read.dcf(path), silent = TRUE)
  required <- c("Protocol-ID", "Status", "Calibration-Hash", "Source-Hash")
  if (inherits(manifest, "try-error") || nrow(manifest) != 1L ||
      !all(required %in% colnames(manifest))) return(NULL)
  values <- as.character(manifest[1L, required])
  names(values) <- required
  if (anyNA(values) || any(!nzchar(trimws(values)))) return(NULL)
  list(
    protocol_id = unname(values[["Protocol-ID"]]),
    status = unname(values[["Status"]]),
    calibration_hash = unname(values[["Calibration-Hash"]]),
    source_hash = unname(values[["Source-Hash"]]),
    manifest_fingerprint = .scmix_v22_fingerprint(readLines(
      path, warn = FALSE, encoding = "UTF-8")))
}

.scmix_v22_calibration_approval_matches <- function(
    approval, calibration_hash, source_hash) {
  is.list(approval) &&
    identical(approval$protocol_id, .scmix_v22_protocol_id) &&
    identical(approval$status, "frozen_for_production") &&
    identical(approval$calibration_hash, calibration_hash) &&
    identical(approval$source_hash, source_hash) &&
    is.character(approval$manifest_fingerprint) &&
    length(approval$manifest_fingerprint) == 1L &&
    !is.na(approval$manifest_fingerprint) &&
    nzchar(approval$manifest_fingerprint)
}

#' Link Step-3 constants to a signed v2.2 analysis contract
#'
#' A plain list of cutoffs is never production evidence. This constructor
#' verifies that the exact values and their immutable source hash equal the
#' calibration hash frozen in the analysis contract.
#'
#' @param values Complete named numeric Step-3 constants.
#' @param analysis_contract Signed [scmix_v22_analysis_contract()].
#' @param source_hash Immutable hash of the constants file.
#' @param provenance Nonempty audit provenance.
#' @return A fingerprinted `scmix_v22_calibration_artifact`.
#' @export
scmix_v22_calibration_artifact <- function(
    values, analysis_contract, source_hash, provenance) {
  constants <- .scmix_v22_normalize_calibration(values,
                                                 require_complete = TRUE)
  if (!.scmix_v22_intact(analysis_contract,
                         "scmix_v22_analysis_contract")) {
    stop("analysis_contract must be an intact signed v2.2 contract.",
         call. = FALSE)
  }
  source_hash <- .scmix_v22_text(source_hash, "source_hash")
  calibration_hash <- scmix_v22_calibration_hash(constants, source_hash)
  if (!identical(calibration_hash, analysis_contract$calibration_hash)) {
    stop("Exact calibration values and source hash do not match the signed analysis contract.",
         call. = FALSE)
  }
  approval <- .scmix_v22_approved_calibration_manifest()
  if (!.scmix_v22_calibration_approval_matches(
      approval, calibration_hash, source_hash)) {
    stop("No package-shipped Step-3 calibration manifest approves these constants and source hash for production.",
         call. = FALSE)
  }
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = analysis_contract$analysis_signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    calibration_hash = calibration_hash, source_hash = source_hash,
    constants = constants, complete = TRUE,
    approval_manifest = approval,
    provenance = .scmix_v22_text(provenance, "provenance"))
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_calibration_artifact", "list")
  payload
}

.scmix_v22_validate_calibration <- function(
    calibration, analysis_contract, require_complete = TRUE) {
  if (!.scmix_v22_intact(calibration,
                         "scmix_v22_calibration_artifact") ||
      !.scmix_v22_intact(analysis_contract,
                         "scmix_v22_analysis_contract")) return(FALSE)
  constants <- try(.scmix_v22_normalize_calibration(
    calibration$constants, require_complete = require_complete), silent = TRUE)
  if (inherits(constants, "try-error")) return(FALSE)
  approval <- .scmix_v22_approved_calibration_manifest()
  identical(calibration$protocol_id, .scmix_v22_protocol_id) &&
    identical(calibration$analysis_signature,
              analysis_contract$analysis_signature) &&
    identical(calibration$analysis_contract_fingerprint,
              analysis_contract$fingerprint) &&
    identical(calibration$calibration_hash,
              analysis_contract$calibration_hash) &&
    identical(calibration$calibration_hash,
              scmix_v22_calibration_hash(constants,
                                         calibration$source_hash)) &&
    identical(calibration$approval_manifest, approval) &&
    .scmix_v22_calibration_approval_matches(
      approval, calibration$calibration_hash, calibration$source_hash) &&
    (!isTRUE(require_complete) || isTRUE(calibration$complete))
}

.scmix_v22_registry_payload <- function(registry) {
  if (!is.list(registry) || !length(registry) ||
      !all(vapply(registry, function(x)
        .scmix_v22_intact(x, "scmix_v22_target_spec"), logical(1L)))) {
    stop("registry must contain intact signed v2.2 target specifications.",
         call. = FALSE)
  }
  ids <- vapply(registry, `[[`, character(1L), "target_id")
  if (anyDuplicated(ids)) stop("registry target IDs must be unique.",
                               call. = FALSE)
  primitive_records <- unlist(lapply(registry, function(spec) {
    lapply(seq_along(spec$primitive_registry_ids), function(j) list(
      id = spec$primitive_registry_ids[j],
      metadata = list(
        estimand = spec$primitive_estimands[j],
        orientation = spec$primitive_orientations[j],
        units = spec$primitive_units[j],
        reference_rule = spec$primitive_reference_rules[j],
        influence_definition = spec$primitive_influence_definitions[j])))
  }), recursive = FALSE)
  by_primitive <- split(primitive_records, vapply(
    primitive_records, `[[`, character(1L), "id"))
  mismatch <- names(Filter(function(records) {
    length(records) > 1L && any(vapply(records[-1L], function(x)
      !identical(x$metadata, records[[1L]]$metadata), logical(1L)))
  }, by_primitive))
  if (length(mismatch)) {
    stop("Duplicate primitive IDs have incompatible estimand, orientation, units, reference, or influence definitions: ",
         paste(mismatch, collapse = ", "), ".", call. = FALSE)
  }
  registry[order(ids, method = "radix")]
}

#' Sign the application-wide v2.2 analysis contract
#'
#' @param analysis_signature Fit-linked analysis signature.
#' @param application_id,specification_id,specification_role Frozen IDs.
#' @param respondent_sample_rule,completion_rule,missing_data_rule,
#'   preprocessing_rule,integration_rule Frozen analysis rules.
#' @param outer_folds,inner_folds,optimizer_starts,learner_grid,ridge_grid,
#'   seeds Nonempty frozen design objects.
#' @param code_commit,data_hash,design_hash,calibration_hash Immutable hashes.
#' @param q Frozen nonnegative latent-factor dimension.
#' @param fit_uses_numerical_integration,exact_nested_pair_registered Frozen
#'   logical declarations governing fit-level integration and nested-fit
#'   evidence.
#' @param derivative_coordinate_registry,derivative_coordinate_subset,
#'   derivative_subset_rule Pre-fit structural-coordinate registry, audited
#'   subset in registry order, and deterministic selection rule.
#' @return A fingerprinted `scmix_v22_analysis_contract`.
#' @export
scmix_v22_analysis_contract <- function(
    analysis_signature, application_id, specification_id, specification_role,
    respondent_sample_rule, completion_rule, missing_data_rule,
    outer_folds, inner_folds, preprocessing_rule, integration_rule,
    optimizer_starts, learner_grid, ridge_grid, seeds,
    code_commit, data_hash, design_hash, calibration_hash,
    q, fit_uses_numerical_integration, exact_nested_pair_registered,
    derivative_coordinate_registry, derivative_coordinate_subset,
    derivative_subset_rule) {
  text_fields <- c(
    analysis_signature = analysis_signature, application_id = application_id,
    specification_id = specification_id,
    specification_role = specification_role,
    respondent_sample_rule = respondent_sample_rule,
    completion_rule = completion_rule, missing_data_rule = missing_data_rule,
    preprocessing_rule = preprocessing_rule,
    integration_rule = integration_rule, code_commit = code_commit,
    data_hash = data_hash, design_hash = design_hash,
    calibration_hash = calibration_hash)
  text_fields <- stats::setNames(vapply(seq_along(text_fields), function(j)
    .scmix_v22_text(text_fields[[j]], names(text_fields)[j]), character(1L)),
    names(text_fields))
  objects <- list(
    outer_folds = outer_folds, inner_folds = inner_folds,
    optimizer_starts = optimizer_starts, learner_grid = learner_grid,
    ridge_grid = ridge_grid, seeds = seeds)
  if (any(vapply(objects, function(x) is.null(x) || !length(x), logical(1L)))) {
    stop("Fold, optimizer, learner, ridge, and seed declarations must be nonempty.",
         call. = FALSE)
  }
  frozen_grid <- c("0", "n_k^-1", "n_k^-3/4", "n_k^-1/2")
  if (!identical(as.character(ridge_grid), frozen_grid)) {
    stop("ridge_grid must equal the frozen v2.2 fold-size grid.",
         call. = FALSE)
  }
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q < 0 ||
      q != as.integer(q)) {
    stop("q must be one nonnegative integer.", call. = FALSE)
  }
  logical_fields <- c(
    fit_uses_numerical_integration = fit_uses_numerical_integration,
    exact_nested_pair_registered = exact_nested_pair_registered)
  if (!is.logical(logical_fields) || anyNA(logical_fields)) {
    stop("fit_uses_numerical_integration and exact_nested_pair_registered must be nonmissing logical values.",
         call. = FALSE)
  }
  derivative_coordinate_registry <- .scmix_v22_names(
    derivative_coordinate_registry, "derivative_coordinate_registry")
  derivative_coordinate_subset <- .scmix_v22_names(
    derivative_coordinate_subset, "derivative_coordinate_subset")
  if (!all(derivative_coordinate_subset %in%
             derivative_coordinate_registry) ||
      !identical(derivative_coordinate_subset,
                 derivative_coordinate_registry[
                   derivative_coordinate_registry %in%
                     derivative_coordinate_subset])) {
    stop("derivative_coordinate_subset must be a subset in the frozen coordinate-registry order.",
         call. = FALSE)
  }
  derivative_subset_rule <- .scmix_v22_text(
    derivative_subset_rule, "derivative_subset_rule")
  payload <- c(as.list(text_fields), objects,
                list(
                  q = as.integer(q),
                  fit_uses_numerical_integration =
                    fit_uses_numerical_integration,
                  exact_nested_pair_registered =
                    exact_nested_pair_registered,
                  derivative_coordinate_registry =
                    derivative_coordinate_registry,
                  derivative_coordinate_subset = derivative_coordinate_subset,
                  derivative_subset_rule = derivative_subset_rule,
                  protocol_id = .scmix_v22_protocol_id))
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_analysis_contract", "list")
  payload
}

.scmix_v22_contract_fold_ids <- function(contract) {
  x <- contract$outer_folds
  if (is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
    return(names(x))
  }
  sort(unique(as.character(unlist(x, use.names = FALSE))), method = "radix")
}

#' Link full-sample structural plug-ins to the signed final fit
#'
#' @param inference Fit-linked `scmix_dml` result.
#' @param values Finite uniquely named full-sample plug-in values.
#' @param analysis_contract Signed [scmix_v22_analysis_contract()].
#' @param final_fit_hash,code_hash,provenance Immutable audit identifiers.
#' @return A fingerprinted `scmix_v22_full_plugin_artifact`.
#' @export
scmix_v22_full_plugin_artifact <- function(
    inference, values, analysis_contract, final_fit_hash, code_hash,
    provenance) {
  signature <- if (is.list(inference)) inference$analysis_signature else NULL
  if (!.scmix_v22_intact(analysis_contract, "scmix_v22_analysis_contract") ||
      !identical(signature, analysis_contract$analysis_signature)) {
    stop("analysis_contract must be intact and linked to inference.",
         call. = FALSE)
  }
  if (!is.numeric(values) || !length(values) || is.null(names(values)) ||
      anyDuplicated(names(values)) || any(!nzchar(names(values))) ||
      any(!is.finite(values))) {
    stop("values must be a finite uniquely named numeric vector.",
         call. = FALSE)
  }
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    final_fit_hash = .scmix_v22_text(final_fit_hash, "final_fit_hash"),
    code_hash = .scmix_v22_text(code_hash, "code_hash"),
    provenance = .scmix_v22_text(provenance, "provenance"), values = values)
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_full_plugin_artifact", "list")
  payload
}

#' Generate the common protocol-v2.2 respondent multiplier bank
#'
#' Respondent identifiers are sorted before generation. A later calculation
#' aligns the bank back to the influence rows by identifier. Consequently the
#' random numbers do not depend on target order, target batching, or the input
#' respondent order.
#'
#' @param respondent_id Unique respondent identifiers.
#' @param draws Number of multiplier draws. Final paper results require 9,999.
#' @param seed Signed analysis seed.
#' @param distribution Currently only the protocol's standard Normal law.
#' @param analysis_contract Optional signed contract. Final simultaneous status
#'   requires this link and requires `seed` to equal its multiplier seed.
#' @return A reusable `scmix_v22_multiplier_bank`.
#' @export
scmix_v22_multiplier_bank <- function(
    respondent_id, draws = 9999L, seed = 1L,
    distribution = "normal", analysis_contract = NULL) {
  respondent_id <- as.character(respondent_id)
  if (!length(respondent_id) || anyNA(respondent_id) ||
      any(!nzchar(respondent_id)) || anyDuplicated(respondent_id)) {
    stop("respondent_id must contain unique nonmissing identifiers.",
         call. = FALSE)
  }
  if (!is.numeric(draws) || length(draws) != 1L || is.na(draws) ||
      draws < 1 || draws != as.integer(draws)) {
    stop("draws must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed != as.integer(seed)) {
    stop("seed must be a nonnegative integer.", call. = FALSE)
  }
  .scmix_v22_choice(distribution, "normal", "distribution")
  contract_linked <- .scmix_v22_intact(
    analysis_contract, "scmix_v22_analysis_contract")
  if (!is.null(analysis_contract) && !contract_linked) {
    stop("analysis_contract must be an intact signed v2.2 contract.",
         call. = FALSE)
  }
  if (contract_linked) {
    registered_seed <- analysis_contract$seeds[["multiplier"]]
    if (is.null(registered_seed) || length(registered_seed) != 1L ||
        !identical(as.integer(registered_seed), as.integer(seed))) {
      stop("seed must equal the signed analysis contract's `multiplier` seed.",
           call. = FALSE)
    }
  }
  canonical_id <- sort(enc2utf8(respondent_id), method = "radix")
  rng_kind <- RNGkind()
  zeta <- .scmix_v22_with_seed(as.integer(seed), {
    matrix(stats::rnorm(length(canonical_id) * as.integer(draws)),
           nrow = length(canonical_id), ncol = as.integer(draws),
           dimnames = list(canonical_id, NULL))
  })
  signature <- list(protocol_id = .scmix_v22_protocol_id,
                    respondent_id = canonical_id,
                    draws = as.integer(draws), seed = as.integer(seed),
                    distribution = distribution,
                    rng_kind = rng_kind,
                    r_version = as.character(getRversion()),
                    analysis_signature = if (contract_linked)
                      analysis_contract$analysis_signature else NA_character_,
                    analysis_contract_fingerprint = if (contract_linked)
                      analysis_contract$fingerprint else NA_character_)
  out <- c(signature, list(multipliers = zeta,
                           final_draw_count = identical(ncol(zeta), 9999L)))
  out$fingerprint <- .scmix_v22_fingerprint(out)
  class(out) <- c("scmix_v22_multiplier_bank", "list")
  out
}

#' Run signed respondent-level inner nuisance refits for protocol v2.2
#'
#' The callback is invoked once per outer-fold/inner-fold pair. It must perform
#' all outcome-dependent preprocessing and nuisance fitting on the supplied
#' inner-training respondents and return validation risks evaluated only on
#' the supplied inner-validation respondents. Hashes link those fitted objects
#' to the trace. A free-text assertion cannot substitute for this artifact.
#'
#' @param fit Fit carrying respondent and outer-fold identifiers and an analysis
#'   signature.
#' @param primitive_registry_ids Frozen unique scalar primitive IDs.
#' @param inner_folds Number of respondent-level inner folds.
#' @param refit Callback accepting `fit`, `outer_fold`, `inner_fold`,
#'   `training_respondent_id`, `validation_respondent_id`, `ridge_grid`, and
#'   `primitive_registry_ids`. It returns `success = TRUE`, a finite
#'   ridge-by-primitive `validation_risk` matrix, the exact `ridge_grid`, and
#'   nonempty `preprocessing_hash`, `nuisance_fit_hash`, and `score_hash`.
#' @param seed Signed split seed.
#' @param code_hash,provenance Nonempty immutable identifiers.
#' @param analysis_contract Signed analysis-wide contract. Its code hash,
#'   inner-fold declaration, and `riesz_inner` seed must match this run.
#' @return A fingerprinted `scmix_v22_inner_refit_trace`.
#' @export
scmix_v22_run_inner_refits <- function(
    fit, primitive_registry_ids, inner_folds = 3L, refit,
    seed = 1L, code_hash, provenance,
    analysis_contract = fit$v22_analysis_contract) {
  signature <- if (is.list(fit)) fit$analysis_signature else NULL
  if (is.null(signature) || !is.character(signature) ||
      length(signature) != 1L || is.na(signature) || !nzchar(signature)) {
    stop("fit must carry a nonempty analysis_signature.", call. = FALSE)
  }
  primitive_registry_ids <- .scmix_v22_names(
    primitive_registry_ids, "primitive_registry_ids")
  if (!is.function(refit)) stop("refit must be a function.", call. = FALSE)
  if (!is.numeric(inner_folds) || length(inner_folds) != 1L ||
      is.na(inner_folds) || inner_folds < 2L ||
      inner_folds != as.integer(inner_folds)) {
    stop("inner_folds must be an integer of at least two.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed != as.integer(seed)) {
    stop("seed must be a nonnegative integer.", call. = FALSE)
  }
  code_hash <- .scmix_v22_text(code_hash, "code_hash")
  provenance <- .scmix_v22_text(provenance, "provenance")
  contract_ok <- .scmix_v22_intact(
    analysis_contract, "scmix_v22_analysis_contract") &&
    identical(analysis_contract$analysis_signature, signature) &&
    identical(analysis_contract$code_commit, code_hash) &&
    !is.null(analysis_contract$seeds[["riesz_inner"]]) &&
    identical(as.integer(analysis_contract$seeds[["riesz_inner"]]),
              as.integer(seed))
  if (!contract_ok) {
    stop("analysis_contract must be intact and must bind this fit, code hash, and `riesz_inner` seed.",
         call. = FALSE)
  }
  if (is.numeric(analysis_contract$inner_folds) &&
      length(analysis_contract$inner_folds) == 1L &&
      !identical(as.integer(analysis_contract$inner_folds),
                 as.integer(inner_folds))) {
    stop("inner_folds does not match the signed analysis contract.",
         call. = FALSE)
  }
  layout <- .scmix_v22_refit_layout(fit)
  respondent_id <- layout$respondent_id
  fold_resp <- layout$fold_resp
  K <- layout$K; J <- length(primitive_registry_ids)
  if (!identical(.scmix_v22_contract_fold_ids(analysis_contract),
                 paste0("fold", seq_len(K)))) {
    stop("Outer-fold IDs do not match the signed analysis contract.",
         call. = FALSE)
  }
  selected_nu <- matrix(NA_real_, K, J,
                        dimnames = list(paste0("fold", seq_len(K)),
                                        primitive_registry_ids))
  training_n <- integer(K)
  inner_validation <- vector("list", K)
  records <- list()
  for (k in seq_len(K)) {
    outer_train <- which(fold_resp != k)
    training_n[k] <- length(outer_train)
    if (length(outer_train) < inner_folds) {
      stop("Outer training fold ", k, " is too small for inner_folds.",
           call. = FALSE)
    }
    assignment <- .scmix_v22_with_seed(as.integer(seed + 1009L * k), {
      perm <- sample(outer_train, length(outer_train), replace = FALSE)
      out <- integer(length(respondent_id))
      out[perm] <- rep(seq_len(inner_folds), length.out = length(perm))
      out
    })
    grid <- sort(unique(c(0, training_n[k]^(-1),
                          training_n[k]^(-3 / 4),
                          training_n[k]^(-1 / 2))))
    risks <- vector("list", inner_folds)
    weights <- integer(inner_folds)
    for (v in seq_len(inner_folds)) {
      validation <- which(assignment == v)
      training <- setdiff(outer_train, validation)
      ans <- refit(
        fit = fit, outer_fold = k, inner_fold = v,
        training_respondent_id = respondent_id[training],
        validation_respondent_id = respondent_id[validation],
        ridge_grid = grid,
        primitive_registry_ids = primitive_registry_ids)
      needed <- c("success", "validation_risk", "ridge_grid",
                  "preprocessing_hash", "nuisance_fit_hash", "score_hash")
      if (!is.list(ans) || any(vapply(needed, function(nm)
        is.null(ans[[nm]]), logical(1L))) || !isTRUE(ans$success)) {
        stop("Inner refit callback failed or omitted required evidence in ",
             "outer fold ", k, ", inner fold ", v, ".", call. = FALSE)
      }
      risk <- as.matrix(ans$validation_risk)
      storage.mode(risk) <- "double"
      if (!isTRUE(all.equal(as.numeric(ans$ridge_grid), grid,
                            tolerance = 0, check.attributes = FALSE)) ||
          !identical(dim(risk), c(length(grid), J)) ||
          !identical(colnames(risk), primitive_registry_ids) ||
          any(!is.finite(risk))) {
        stop("Inner refit callback returned a malformed ridge/risk artifact ",
             "in outer fold ", k, ", inner fold ", v, ".", call. = FALSE)
      }
      hashes <- vapply(c("preprocessing_hash", "nuisance_fit_hash",
                         "score_hash"), function(nm)
        .scmix_v22_text(ans[[nm]], nm), character(1L))
      weights[v] <- length(validation)
      risks[[v]] <- risk
      record <- data.frame(
        outer_fold = k, inner_fold = v,
        training_n = length(training), validation_n = length(validation),
        training_respondent_hash = .scmix_v22_fingerprint(
          sort(respondent_id[training], method = "radix")),
        validation_respondent_hash = .scmix_v22_fingerprint(
          sort(respondent_id[validation], method = "radix")),
        preprocessing_hash = hashes[[1L]], nuisance_fit_hash = hashes[[2L]],
        score_hash = hashes[[3L]],
        validation_risk_hash = .scmix_v22_fingerprint(risk),
        code_hash = code_hash, success = TRUE,
        stringsAsFactors = FALSE)
      record$training_respondent_id <- I(list(respondent_id[training]))
      record$validation_respondent_id <- I(list(respondent_id[validation]))
      record$ridge_grid <- I(list(grid))
      record$validation_risk <- I(list(risk))
      records[[length(records) + 1L]] <- record
    }
    aggregate <- Reduce(`+`, Map(function(x, w) x * w, risks, weights)) /
      sum(weights)
    colnames(aggregate) <- primitive_registry_ids
    rownames(aggregate) <- format(grid, digits = 17, scientific = TRUE,
                                  trim = TRUE)
    inner_validation[[k]] <- aggregate
    selected_nu[k, ] <- vapply(seq_len(J), function(j) {
      mn <- min(aggregate[, j])
      tied <- which(abs(aggregate[, j] - mn) <=
                      1e-12 * max(1, abs(mn)))
      max(grid[tied])
    }, numeric(1L))
  }
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    primitive_registry_ids = primitive_registry_ids,
    inner_folds = as.integer(inner_folds), seed = as.integer(seed),
    rng_kind = RNGkind(), r_version = as.character(getRversion()),
    training_n = training_n, selected_nu = selected_nu,
    inner_validation = inner_validation,
    trace = do.call(rbind, records), code_hash = code_hash,
    provenance = provenance)
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_inner_refit_trace", "list")
  if (!.scmix_v22_validate_inner_refit_trace(
      fit, payload, analysis_contract)) {
    stop("The generated inner-refit trace failed its own integrity audit.",
         call. = FALSE)
  }
  payload
}

.scmix_v22_refit_layout <- function(fit) {
  rid_task <- if (is.list(fit)) as.character(fit$respondent_id) else NULL
  fold_task <- if (is.list(fit)) fit$fold_id else NULL
  if (!length(rid_task) || length(fold_task) != length(rid_task) ||
      anyNA(rid_task) || any(!nzchar(rid_task)) || anyNA(fold_task)) {
    stop("fit must carry task-row respondent_id and fold_id vectors.",
         call. = FALSE)
  }
  first <- !duplicated(rid_task)
  respondent_id <- rid_task[first]
  fold_resp <- suppressWarnings(as.integer(fold_task[first]))
  consistent <- vapply(split(seq_along(rid_task), rid_task), function(ii)
    length(unique(as.integer(fold_task[ii]))) == 1L, logical(1L))
  if (!all(consistent) || anyNA(fold_resp) || any(fold_resp < 1L) ||
      !identical(sort(unique(fold_resp)), seq_len(max(fold_resp)))) {
    stop("Outer folds must be positive consecutive respondent-level folds.",
         call. = FALSE)
  }
  list(respondent_id = respondent_id, fold_resp = fold_resp,
       K = max(fold_resp))
}

.scmix_v22_inner_assignment <- function(n_respondents, outer_train,
                                         inner_folds, seed, outer_fold) {
  .scmix_v22_with_seed(as.integer(seed + 1009L * outer_fold), {
    perm <- sample(outer_train, length(outer_train), replace = FALSE)
    out <- integer(n_respondents)
    out[perm] <- rep(seq_len(inner_folds), length.out = length(perm))
    out
  })
}

.scmix_v22_validate_inner_refit_trace <- function(
    fit, trace, analysis_contract) {
  if (!.scmix_v22_intact(trace, "scmix_v22_inner_refit_trace") ||
      !.scmix_v22_intact(analysis_contract,
                         "scmix_v22_analysis_contract")) return(FALSE)
  layout <- try(.scmix_v22_refit_layout(fit), silent = TRUE)
  if (inherits(layout, "try-error")) return(FALSE)
  primitive_ids <- try(.scmix_v22_names(
    trace$primitive_registry_ids, "primitive_registry_ids"), silent = TRUE)
  if (inherits(primitive_ids, "try-error")) return(FALSE)
  K <- layout$K
  V <- suppressWarnings(as.integer(trace$inner_folds))
  seed <- suppressWarnings(as.integer(trace$seed))
  basic <- identical(trace$protocol_id, .scmix_v22_protocol_id) &&
    identical(trace$analysis_signature, fit$analysis_signature) &&
    identical(trace$analysis_signature,
              analysis_contract$analysis_signature) &&
    identical(trace$analysis_contract_fingerprint,
              analysis_contract$fingerprint) &&
    identical(trace$code_hash, analysis_contract$code_commit) &&
    length(V) == 1L && !is.na(V) && V >= 2L &&
    identical(V, as.integer(analysis_contract$inner_folds)) &&
    length(seed) == 1L && !is.na(seed) &&
    !is.null(analysis_contract$seeds[["riesz_inner"]]) &&
    identical(seed, as.integer(analysis_contract$seeds[["riesz_inner"]])) &&
    is.character(trace$rng_kind) && length(trace$rng_kind) == 3L &&
    identical(trace$r_version, as.character(getRversion())) &&
    is.numeric(trace$training_n) && length(trace$training_n) == K &&
    is.matrix(trace$selected_nu) &&
    identical(dim(trace$selected_nu), c(K, length(primitive_ids))) &&
    identical(rownames(trace$selected_nu), paste0("fold", seq_len(K))) &&
    identical(colnames(trace$selected_nu), primitive_ids) &&
    is.list(trace$inner_validation) && length(trace$inner_validation) == K &&
    is.data.frame(trace$trace)
  if (!basic) return(FALSE)
  required <- c(
    "outer_fold", "inner_fold", "training_n", "validation_n",
    "training_respondent_hash", "validation_respondent_hash",
    "preprocessing_hash", "nuisance_fit_hash", "score_hash",
    "validation_risk_hash", "code_hash", "success",
    "training_respondent_id", "validation_respondent_id", "ridge_grid",
    "validation_risk")
  records <- trace$trace
  if (!all(required %in% names(records)) || nrow(records) != K * V ||
      any(!records$success) || any(records$code_hash != trace$code_hash)) {
    return(FALSE)
  }
  respondent_id <- layout$respondent_id
  fold_resp <- layout$fold_resp
  expected_selected <- matrix(
    NA_real_, K, length(primitive_ids),
    dimnames = list(paste0("fold", seq_len(K)), primitive_ids))
  expected_training_n <- integer(K)
  expected_validation <- vector("list", K)
  for (k in seq_len(K)) {
    outer_train <- which(fold_resp != k)
    expected_training_n[k] <- length(outer_train)
    if (length(outer_train) < V) return(FALSE)
    assignment <- .scmix_v22_inner_assignment(
      length(respondent_id), outer_train, V, seed, k)
    grid <- sort(unique(c(
      0, expected_training_n[k]^(-1),
      expected_training_n[k]^(-3 / 4),
      expected_training_n[k]^(-1 / 2))))
    risks <- vector("list", V); weights <- integer(V)
    for (v in seq_len(V)) {
      at <- which(records$outer_fold == k & records$inner_fold == v)
      if (length(at) != 1L) return(FALSE)
      validation <- which(assignment == v)
      training <- setdiff(outer_train, validation)
      train_id <- respondent_id[training]
      validation_id <- respondent_id[validation]
      risk <- as.matrix(records$validation_risk[[at]])
      storage.mode(risk) <- "double"
      text_hashes <- c("preprocessing_hash", "nuisance_fit_hash",
                       "score_hash")
      valid <- identical(as.integer(records$training_n[at]),
                         length(training)) &&
        identical(as.integer(records$validation_n[at]),
                  length(validation)) &&
        identical(records$training_respondent_id[[at]], train_id) &&
        identical(records$validation_respondent_id[[at]], validation_id) &&
        identical(records$training_respondent_hash[at],
                  .scmix_v22_fingerprint(sort(train_id, method = "radix"))) &&
        identical(records$validation_respondent_hash[at],
                  .scmix_v22_fingerprint(sort(validation_id,
                                              method = "radix"))) &&
        all(vapply(records[at, text_hashes, drop = FALSE], function(z)
          is.character(z) && length(z) == 1L && !is.na(z) && nzchar(z),
          logical(1L))) &&
        isTRUE(all.equal(as.numeric(records$ridge_grid[[at]]), grid,
                         tolerance = 0, check.attributes = FALSE)) &&
        identical(dim(risk), c(length(grid), length(primitive_ids))) &&
        identical(colnames(risk), primitive_ids) &&
        all(is.finite(risk)) &&
        identical(records$validation_risk_hash[at],
                  .scmix_v22_fingerprint(risk))
      if (!valid) return(FALSE)
      risks[[v]] <- risk; weights[v] <- length(validation)
    }
    aggregate <- Reduce(`+`, Map(function(x, w) x * w, risks, weights)) /
      sum(weights)
    colnames(aggregate) <- primitive_ids
    rownames(aggregate) <- format(grid, digits = 17, scientific = TRUE,
                                  trim = TRUE)
    expected_validation[[k]] <- aggregate
    expected_selected[k, ] <- vapply(seq_along(primitive_ids), function(j) {
      mn <- min(aggregate[, j])
      tied <- which(abs(aggregate[, j] - mn) <=
                      1e-12 * max(1, abs(mn)))
      max(grid[tied])
    }, numeric(1L))
  }
  identical(as.integer(trace$training_n), expected_training_n) &&
    identical(trace$inner_validation, expected_validation) &&
    identical(trace$selected_nu, expected_selected)
}

#' Record externally nested ridge selection for protocol v2.2
#'
#' @param fit Exact fit whose analysis signature is being certified.
#' @param nested_refit A trace returned by [scmix_v22_run_inner_refits()].
#' @param ridge_sequence_provenance Nonempty signed provenance for the frozen
#'   fold-size-dependent vanishing ridge sequence.
#' @param analysis_contract Signed analysis-wide contract linked to the trace.
#' @return A fingerprinted `scmix_v22_riesz_evidence`.
#' @export
scmix_v22_riesz_evidence <- function(
    fit, nested_refit, ridge_sequence_provenance,
    analysis_contract = fit$v22_analysis_contract) {
  signature <- if (is.list(fit)) fit$analysis_signature else NULL
  if (is.null(signature) || !is.character(signature) ||
      length(signature) != 1L || is.na(signature) || !nzchar(signature)) {
    stop("fit must carry a nonempty analysis_signature.", call. = FALSE)
  }
  contract_valid <- .scmix_v22_intact(
    analysis_contract, "scmix_v22_analysis_contract") &&
    identical(analysis_contract$analysis_signature, signature)
  trace_valid <- contract_valid && .scmix_v22_validate_inner_refit_trace(
    fit, nested_refit, analysis_contract)
  if (!trace_valid) {
    stop("nested_refit must be an intact signed inner-refit trace for this fit.",
         call. = FALSE)
  }
  ridge_sequence_provenance <- .scmix_v22_text(
    ridge_sequence_provenance, "ridge_sequence_provenance")
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    selected_nu = nested_refit$selected_nu,
    training_n = nested_refit$training_n,
    inner_validation = nested_refit$inner_validation,
    inner_nuisance_refit = TRUE,
    inner_nuisance_provenance = nested_refit$provenance,
    inner_refit_trace = nested_refit,
    inner_refit_trace_fingerprint = nested_refit$fingerprint,
    ridge_sequence_provenance = ridge_sequence_provenance)
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_riesz_evidence", "list")
  payload
}

.scmix_v22_validate_riesz_evidence <- function(
    evidence, fit, analysis_contract, required_primitive_ids = NULL) {
  if (!.scmix_v22_intact(evidence, "scmix_v22_riesz_evidence") ||
      !.scmix_v22_intact(analysis_contract,
                         "scmix_v22_analysis_contract") ||
      !.scmix_v22_validate_inner_refit_trace(
        fit, evidence$inner_refit_trace, analysis_contract)) return(FALSE)
  trace <- evidence$inner_refit_trace
  required_primitive_ids <- if (is.null(required_primitive_ids)) {
    trace$primitive_registry_ids
  } else {
    z <- try(.scmix_v22_names(required_primitive_ids,
                              "required_primitive_ids"), silent = TRUE)
    if (inherits(z, "try-error")) return(FALSE)
    z
  }
  identical(evidence$protocol_id, .scmix_v22_protocol_id) &&
    identical(evidence$analysis_signature, fit$analysis_signature) &&
    identical(evidence$analysis_signature,
              analysis_contract$analysis_signature) &&
    identical(evidence$analysis_contract_fingerprint,
              analysis_contract$fingerprint) &&
    identical(evidence$selected_nu, trace$selected_nu) &&
    identical(evidence$training_n, trace$training_n) &&
    identical(evidence$inner_validation, trace$inner_validation) &&
    isTRUE(evidence$inner_nuisance_refit) &&
    identical(evidence$inner_nuisance_provenance, trace$provenance) &&
    identical(evidence$inner_refit_trace_fingerprint, trace$fingerprint) &&
    is.character(evidence$ridge_sequence_provenance) &&
    length(evidence$ridge_sequence_provenance) == 1L &&
    !is.na(evidence$ridge_sequence_provenance) &&
    nzchar(trimws(evidence$ridge_sequence_provenance)) &&
    all(required_primitive_ids %in% trace$primitive_registry_ids)
}

.scmix_v22_validate_bank <- function(bank, respondent_id,
                                     analysis_contract = NULL) {
  if (!inherits(bank, "scmix_v22_multiplier_bank") ||
      !identical(bank$protocol_id, .scmix_v22_protocol_id) ||
      is.null(bank$multipliers)) {
    stop("bank must be created by scmix_v22_multiplier_bank().",
         call. = FALSE)
  }
  respondent_id <- as.character(respondent_id)
  payload <- unclass(bank); recorded <- payload$fingerprint
  payload$fingerprint <- NULL
  fingerprint_ok <- is.character(recorded) && length(recorded) == 1L &&
    !is.na(recorded) && identical(recorded, .scmix_v22_fingerprint(payload))
  zeta <- bank$multipliers
  dimensions_ok <- is.matrix(zeta) && is.numeric(zeta) &&
    identical(nrow(zeta), length(bank$respondent_id)) &&
    identical(ncol(zeta), as.integer(bank$draws)) &&
    identical(rownames(zeta), bank$respondent_id) && all(is.finite(zeta))
  metadata_ok <- identical(bank$distribution, "normal") &&
    is.character(bank$rng_kind) && length(bank$rng_kind) == 3L &&
    is.character(bank$r_version) && length(bank$r_version) == 1L &&
    identical(bank$final_draw_count, identical(ncol(zeta), 9999L))
  generated_ok <- FALSE
  if (dimensions_ok && metadata_ok &&
      identical(bank$r_version, as.character(getRversion()))) {
    old_kind <- RNGkind()
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                  inherits = FALSE)
    on.exit({
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv,
                        inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    do.call(RNGkind, as.list(bank$rng_kind))
    set.seed(as.integer(bank$seed))
    expected <- matrix(
      stats::rnorm(length(bank$respondent_id) * as.integer(bank$draws)),
      nrow = length(bank$respondent_id), ncol = as.integer(bank$draws),
      dimnames = list(bank$respondent_id, NULL))
    generated_ok <- identical(zeta, expected)
  }
  if (!fingerprint_ok || !dimensions_ok || !metadata_ok || !generated_ok) {
    stop("The multiplier bank failed its value, dimension, RNG, or distribution integrity check.",
         call. = FALSE)
  }
  if (!setequal(respondent_id, bank$respondent_id)) {
    stop("The multiplier bank and influence matrix cover different respondents.",
         call. = FALSE)
  }
  contract_linked <- .scmix_v22_intact(
    analysis_contract, "scmix_v22_analysis_contract") &&
    identical(bank$analysis_signature, analysis_contract$analysis_signature) &&
    identical(bank$analysis_contract_fingerprint,
              analysis_contract$fingerprint) &&
    !is.null(analysis_contract$seeds[["multiplier"]]) &&
    identical(as.integer(bank$seed),
              as.integer(analysis_contract$seeds[["multiplier"]]))
  out <- zeta[match(respondent_id, bank$respondent_id), , drop = FALSE]
  attr(out, "analysis_contract_linked") <- contract_linked
  out
}

#' Run the exact v2.2 structural-coordinate derivative audit
#'
#' The supplied evaluator is called at centered `h` and `h/2` perturbations of
#' the declared structural-orthonormal coordinates in every outer fold. All
#' evaluations and the deterministic coordinate subset are retained and
#' fingerprinted. Legacy rowwise/raw-coordinate differences cannot create this
#' artifact.
#'
#' @param fit Object carrying the analysis signature.
#' @param fold_coordinates Named list of finite named numeric vectors. Names of
#'   the list are outer-fold IDs; vector names and order are the signed
#'   structural-orthonormal coordinate registry.
#' @param primitive_registry_ids Ordered unique scalar primitive IDs.
#' @param evaluate Callback accepting `fold_id`, `coordinates`, and
#'   `primitive_registry_ids`, and returning a finite numeric vector with names
#'   exactly equal to `primitive_registry_ids`.
#' @param analytic_gradient Optional named list, one matrix per fold, with rows
#'   equal to the signed coordinate subset and columns equal to primitive IDs.
#' @param coordinate_subset Deterministic coordinate subset in registry order.
#' @param subset_rule,code_hash,provenance Nonempty signed documentation.
#' @param analysis_contract Signed pre-fit contract that freezes the coordinate
#'   registry, audited subset, subset rule, folds, and code hash.
#' @return A fingerprinted `scmix_v22_derivative_audit`.
#' @export
scmix_v22_derivative_audit <- function(
    fit, fold_coordinates, primitive_registry_ids, evaluate,
    analytic_gradient = NULL,
    coordinate_subset = names(fold_coordinates[[1L]]),
    subset_rule = "all registered structural coordinates",
    code_hash, provenance,
    analysis_contract = fit$v22_analysis_contract) {
  signature <- if (is.list(fit)) fit$analysis_signature else NULL
  signature <- .scmix_v22_text(signature, "fit$analysis_signature")
  primitive_registry_ids <- .scmix_v22_names(
    primitive_registry_ids, "primitive_registry_ids")
  if (!is.function(evaluate)) stop("evaluate must be a function.", call. = FALSE)
  if (!is.list(fold_coordinates) || !length(fold_coordinates) ||
      is.null(names(fold_coordinates)) || anyNA(names(fold_coordinates)) ||
      any(!nzchar(names(fold_coordinates))) || anyDuplicated(names(fold_coordinates))) {
    stop("fold_coordinates must be a uniquely named nonempty list.",
         call. = FALSE)
  }
  coordinate_names <- names(fold_coordinates[[1L]])
  coordinate_names <- .scmix_v22_names(
    coordinate_names, "structural coordinate names")
  valid_coordinates <- all(vapply(fold_coordinates, function(u)
    is.numeric(u) && length(u) == length(coordinate_names) &&
      identical(names(u), coordinate_names) && all(is.finite(u)), logical(1L)))
  if (!valid_coordinates) {
    stop("Every fold coordinate vector must be finite and use the identical signed coordinate order.",
         call. = FALSE)
  }
  coordinate_subset <- .scmix_v22_names(
    coordinate_subset, "coordinate_subset")
  if (!all(coordinate_subset %in% coordinate_names) ||
      !identical(coordinate_subset,
                 coordinate_names[coordinate_names %in% coordinate_subset])) {
    stop("coordinate_subset must be a subset in signed structural-coordinate order.",
         call. = FALSE)
  }
  subset_rule <- .scmix_v22_text(subset_rule, "subset_rule")
  code_hash <- .scmix_v22_text(code_hash, "code_hash")
  provenance <- .scmix_v22_text(provenance, "provenance")
  contract_ok <- .scmix_v22_intact(
    analysis_contract, "scmix_v22_analysis_contract") &&
    identical(analysis_contract$analysis_signature, signature) &&
    identical(analysis_contract$code_commit, code_hash) &&
    identical(analysis_contract$derivative_coordinate_registry,
              coordinate_names) &&
    identical(analysis_contract$derivative_coordinate_subset,
              coordinate_subset) &&
    identical(analysis_contract$derivative_subset_rule, subset_rule) &&
    identical(.scmix_v22_contract_fold_ids(analysis_contract),
              names(fold_coordinates))
  if (!contract_ok) {
    stop("analysis_contract must pre-freeze the exact derivative coordinates, subset, subset rule, folds, and code hash.",
         call. = FALSE)
  }
  if (!is.null(analytic_gradient)) {
    if (!is.list(analytic_gradient) ||
        !identical(names(analytic_gradient), names(fold_coordinates))) {
      stop("analytic_gradient must be NULL or a list in outer-fold order.",
           call. = FALSE)
    }
  }
  eps <- .Machine$double.eps^(1 / 3)
  fold_artifacts <- vector("list", length(fold_coordinates))
  names(fold_artifacts) <- names(fold_coordinates)
  metric <- matrix(NA_real_, length(fold_coordinates),
                   length(primitive_registry_ids),
                   dimnames = list(names(fold_coordinates),
                                   primitive_registry_ids))
  call_evaluator <- function(fold_id, u) {
    value <- evaluate(
      fold_id = fold_id, coordinates = u,
      primitive_registry_ids = primitive_registry_ids)
    if (!is.numeric(value) || length(value) != length(primitive_registry_ids) ||
        !identical(names(value), primitive_registry_ids) ||
        any(!is.finite(value))) {
      stop("evaluate must return finite primitives in exact signed registry order.",
           call. = FALSE)
    }
    unname(value)
  }
  for (k in seq_along(fold_coordinates)) {
    fold_id <- names(fold_coordinates)[k]
    u <- fold_coordinates[[k]]
    base <- call_evaluator(fold_id, u)
    h <- eps * pmax(1, abs(u[coordinate_subset]))
    names(h) <- coordinate_subset
    gh <- gh2 <- matrix(
      NA_real_, length(coordinate_subset), length(primitive_registry_ids),
      dimnames = list(coordinate_subset, primitive_registry_ids))
    evaluations <- vector("list", length(coordinate_subset))
    names(evaluations) <- coordinate_subset
    for (a in seq_along(coordinate_subset)) {
      nm <- coordinate_subset[a]; pos <- match(nm, coordinate_names)
      evaluate_step <- function(step) {
        up <- down <- u
        up[pos] <- up[pos] + step; down[pos] <- down[pos] - step
        plus <- call_evaluator(fold_id, up)
        minus <- call_evaluator(fold_id, down)
        list(plus = plus, minus = minus,
             derivative = (plus - minus) / (2 * step))
      }
      coarse <- evaluate_step(h[a]); fine <- evaluate_step(h[a] / 2)
      gh[a, ] <- coarse$derivative; gh2[a, ] <- fine$derivative
      evaluations[[a]] <- list(
        coordinate = nm, h = unname(h[a]), h_over_2 = unname(h[a] / 2),
        plus_h = coarse$plus, minus_h = coarse$minus,
        plus_h_over_2 = fine$plus, minus_h_over_2 = fine$minus)
    }
    analytic <- NULL
    if (!is.null(analytic_gradient)) {
      analytic <- as.matrix(analytic_gradient[[k]])
      storage.mode(analytic) <- "double"
      if (!identical(dim(analytic), dim(gh)) ||
          !identical(rownames(analytic), coordinate_subset) ||
          !identical(colnames(analytic), primitive_registry_ids) ||
          any(!is.finite(analytic))) {
        stop("Each analytic gradient must use the exact signed coordinate and primitive order.",
             call. = FALSE)
      }
    }
    metric[k, ] <- vapply(seq_along(primitive_registry_ids), function(j) {
      coarse_fine <- sqrt(sum((gh[, j] - gh2[, j])^2))
      if (is.null(analytic)) {
        coarse_fine / max(sqrt(sum(gh2[, j]^2)), 1e-15)
      } else {
        analytic_fine <- sqrt(sum((analytic[, j] - gh2[, j])^2))
        max(analytic_fine, coarse_fine) /
          max(sqrt(sum(analytic[, j]^2)), 1e-15)
      }
    }, numeric(1L))
    fold_artifacts[[k]] <- list(
      fold_id = fold_id, coordinates = u, coordinate_subset = coordinate_subset,
      base = stats::setNames(base, primitive_registry_ids), steps_h = h,
      gradient_h = gh, gradient_h_over_2 = gh2,
      analytic_gradient = analytic, evaluations = evaluations)
  }
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    primitive_registry_ids = primitive_registry_ids,
    coordinate_names = coordinate_names,
    coordinate_subset = coordinate_subset, subset_rule = subset_rule,
    derivative_step_rule =
      "machine_epsilon^(1/3) * max(1, abs(normalized_coordinate))",
    refinement_rule = "centered structural-orthonormal steps h and h/2",
    derivative_metric =
      "relative_l2_in_structural_orthonormal_coordinates",
    machine_epsilon = .Machine$double.eps,
    metric = metric, fold_artifacts = fold_artifacts,
    analytic_checked = !is.null(analytic_gradient),
    code_hash = code_hash, provenance = provenance)
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_derivative_audit", "list")
  payload
}

.scmix_v22_chart_update <- function(plugin, raw, domain) {
  kind <- domain$kind
  if (kind %in% c("unrestricted", "ratio")) {
    return(list(value = raw, interior = TRUE, chart = "identity"))
  }
  if (kind == "bounded") {
    L <- domain$lower; U <- domain$upper
    interior <- is.finite(plugin) && plugin > L && plugin < U
    if (!interior) return(list(value = NA_real_, interior = FALSE,
                               chart = domain$chart))
    eta <- log((plugin - L) / (U - plugin))
    deriv <- (U - L) / ((plugin - L) * (U - plugin))
    value <- L + (U - L) * stats::plogis(eta + deriv * (raw - plugin))
    return(list(value = value, interior = TRUE, chart = domain$chart))
  }
  if (kind == "lower_bounded") {
    L <- domain$lower
    interior <- is.finite(plugin) && plugin > L
    if (!interior) return(list(value = NA_real_, interior = FALSE,
                               chart = domain$chart))
    value <- L + exp(log(plugin - L) + (raw - plugin) / (plugin - L))
    return(list(value = value, interior = TRUE, chart = domain$chart))
  }
  if (kind == "upper_bounded") {
    U <- domain$upper
    interior <- is.finite(plugin) && plugin < U
    if (!interior) return(list(value = NA_real_, interior = FALSE,
                               chart = domain$chart))
    value <- U - exp(log(U - plugin) - (raw - plugin) / (U - plugin))
    return(list(value = value, interior = TRUE, chart = domain$chart))
  }
  stop("Scalar chart is not defined for domain kind `", kind, "`.",
       call. = FALSE)
}

.scmix_v22_target_geometry <- function(spec, supporting) {
  x <- as.numeric(supporting)
  type <- spec$target_type
  if (type %in% c("unrestricted_scalar", "positive_scalar", "probability",
                  "preference_direction_share",
                  "compensating_benefit_share")) {
    return(list(primitive = x, supporting_jacobian = matrix(1, 1L, 1L),
                target = x, target_jacobian = matrix(1, 1L, 1L),
                primitive_names = spec$primitive_registry_ids))
  }
  if (type == "subgroup_share") {
    a <- x[1L]; b <- x[2L]
    h <- if (b != 0) a / b else NA_real_
    dh <- if (b != 0) matrix(c(1 / b, -a / b^2), 1L) else
      matrix(NA_real_, 1L, 2L)
    return(list(primitive = c(numerator = a, denominator = b),
                supporting_jacobian = diag(2L), target = h,
                target_jacobian = dh,
                primitive_names = spec$primitive_registry_ids))
  }
  if (type == "directional_heterogeneity") {
    m <- x[1L]; second <- x[2L]; HR <- x[3L]
    HZ <- second - m^2; total <- HZ + HR
    dh_support <- rbind(c(-2 * m, 1, 0), c(0, 0, 1))
    dh <- if (total != 0) {
      matrix(c(HR / total^2, -HZ / total^2), 1L)
    } else matrix(NA_real_, 1L, 2L)
    return(list(primitive = c(explained_variance = HZ,
                             residual_variance = HR),
                supporting_jacobian = dh_support,
                target = if (total != 0) HZ / total else NA_real_,
                target_jacobian = dh,
                primitive_names = spec$primitive_registry_ids,
                supporting_audit = c(mean = m, second_moment = second,
                                     total_variance = total)))
  }
  if (type == "mean_tradeoff_mrs_ratio") {
    a <- x[1L]; b <- x[2L]
    h <- if (b != 0) -a / b else NA_real_
    dh <- if (b != 0) matrix(c(-1 / b, a / b^2), 1L) else
      matrix(NA_real_, 1L, 2L)
    return(list(primitive = c(numerator = a, denominator = b),
                supporting_jacobian = diag(2L), target = h,
                target_jacobian = dh,
                primitive_names = spec$primitive_registry_ids))
  }
  stop("Unsupported target type `", type, "`.", call. = FALSE)
}

.scmix_v22_primitive_geometry <- function(spec, primitive) {
  primitive <- as.numeric(primitive)
  type <- spec$target_type
  if (length(primitive) == 1L) {
    return(list(target = primitive[1L],
                target_jacobian = matrix(1, 1L, 1L)))
  }
  a <- primitive[1L]; b <- primitive[2L]
  if (type == "subgroup_share") {
    return(list(
      target = if (b != 0) a / b else NA_real_,
      target_jacobian = if (b != 0) matrix(c(1 / b, -a / b^2), 1L) else
        matrix(NA_real_, 1L, 2L)))
  }
  if (type == "directional_heterogeneity") {
    total <- a + b
    return(list(
      target = if (total != 0) a / total else NA_real_,
      target_jacobian = if (total != 0)
        matrix(c(b / total^2, -a / total^2), 1L) else
          matrix(NA_real_, 1L, 2L)))
  }
  if (type == "mean_tradeoff_mrs_ratio") {
    return(list(
      target = if (b != 0) -a / b else NA_real_,
      target_jacobian = if (b != 0) matrix(c(-1 / b, a / b^2), 1L) else
        matrix(NA_real_, 1L, 2L)))
  }
  stop("No primitive target geometry for target type `", type, "`.",
       call. = FALSE)
}

.scmix_v22_composite_chart <- function(spec, plugin, raw) {
  type <- spec$target_type
  if (type == "subgroup_share") {
    a <- plugin[1L]; b <- plugin[2L]
    interior <- all(is.finite(plugin)) && a > 0 && a < b && b < 1
    if (!interior) return(list(primitive = rep(NA_real_, 2L),
                               target = NA_real_, interior = FALSE))
    q <- a / b
    G <- c(stats::qlogis(q), stats::qlogis(b))
    DG <- rbind(c(1 / (b * q * (1 - q)), -1 / (b * (1 - q))),
                c(0, 1 / (b * (1 - b))))
    eta <- G + as.numeric(DG %*% (raw - plugin))
    b_rr <- stats::plogis(eta[2L]); q_rr <- stats::plogis(eta[1L])
    primitive <- c(numerator = q_rr * b_rr, denominator = b_rr)
    return(list(primitive = primitive, target = q_rr, interior = TRUE))
  }
  if (type == "directional_heterogeneity") {
    interior <- all(is.finite(plugin)) && all(plugin > 0)
    if (!interior) return(list(primitive = rep(NA_real_, 2L),
                               target = NA_real_, interior = FALSE))
    primitive <- exp(log(plugin) + (raw - plugin) / plugin)
    names(primitive) <- c("explained_variance", "residual_variance")
    return(list(primitive = primitive,
                target = primitive[1L] / sum(primitive), interior = TRUE))
  }
  if (type == "mean_tradeoff_mrs_ratio") {
    target <- if (raw[2L] != 0) -raw[1L] / raw[2L] else NA_real_
    return(list(primitive = raw, target = target,
                interior = all(is.finite(plugin)) && plugin[2L] != 0))
  }
  stop("Composite chart is not defined for target type `", type, "`.",
       call. = FALSE)
}

.scmix_v22_target_in_support <- function(value, domain) {
  if (!is.finite(value)) return(FALSE)
  switch(domain$kind,
         bounded = value >= domain$lower && value <= domain$upper,
         lower_bounded = value >= domain$lower,
         upper_bounded = value <= domain$upper,
         TRUE)
}

.scmix_v22_domain_distance <- function(type, raw, se) {
  if (any(!is.finite(se)) || any(se <= 0)) return(NA_real_)
  if (type %in% c("probability", "preference_direction_share",
                  "compensating_benefit_share")) {
    return(max(c((0 - raw) / se, (raw - 1) / se, 0)))
  }
  if (type == "positive_scalar") return(max((0 - raw) / se, 0))
  if (type == "directional_heterogeneity") {
    return(max(c(-raw / se, 0)))
  }
  if (type == "subgroup_share") {
    feasible <- function(t) {
      lo <- pmax(raw - t * se, 0); hi <- pmin(raw + t * se, 1)
      all(lo <= hi) && lo[1L] <= hi[2L]
    }
    if (feasible(0)) return(0)
    hi <- 1
    while (!feasible(hi) && hi < 2^30) hi <- hi * 2
    if (!feasible(hi)) return(Inf)
    lo <- 0
    for (j in seq_len(80L)) {
      mid <- (lo + hi) / 2
      if (feasible(mid)) hi <- mid else lo <- mid
    }
    return(hi)
  }
  0
}

.scmix_v22_boundary_distance <- function(type, plugin, se) {
  if (any(!is.finite(plugin)) || any(!is.finite(se)) || any(se <= 0)) {
    return(NA_real_)
  }
  if (type %in% c("probability", "preference_direction_share",
                  "compensating_benefit_share")) {
    return(min(plugin / se, (1 - plugin) / se))
  }
  if (type == "positive_scalar") return(plugin / se)
  if (type == "directional_heterogeneity") return(min(plugin / se))
  if (type == "subgroup_share") {
    a <- plugin[1L]; b <- plugin[2L]
    return(min(a / se[1L], (1 - b) / se[2L],
               (b - a) / (se[1L] + se[2L])))
  }
  if (type == "mean_tradeoff_mrs_ratio") return(abs(plugin[2L]) / se[2L])
  Inf
}

.scmix_v22_map_rectangle <- function(type, lower, upper) {
  lower <- as.numeric(lower); upper <- as.numeric(upper)
  if (anyNA(c(lower, upper)) || any(lower > upper)) {
    return(list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
                boundary_touch = NA, uninformative = FALSE,
                reason = "invalid primitive rectangle"))
  }
  if (type %in% c("unrestricted_scalar", "mean_tradeoff_mrs_ratio")) {
    if (type == "unrestricted_scalar") {
      return(list(nonempty = TRUE, lower = lower[1L], upper = upper[1L],
                  boundary_touch = FALSE, uninformative = FALSE,
                  reason = "identity map"))
    }
    if (lower[2L] <= 0 && upper[2L] >= 0) {
      return(list(nonempty = TRUE, lower = NA_real_, upper = NA_real_,
                  boundary_touch = TRUE, uninformative = TRUE,
                  reason = "denominator confidence region contains zero"))
    }
    vals <- as.vector(outer(c(lower[1L], upper[1L]),
                            c(lower[2L], upper[2L]),
                            function(a, b) -a / b))
    return(list(nonempty = TRUE, lower = min(vals), upper = max(vals),
                boundary_touch = FALSE, uninformative = FALSE,
                reason = "corner image of ratio rectangle"))
  }
  if (type %in% c("positive_scalar", "probability",
                  "preference_direction_share",
                  "compensating_benefit_share")) {
    L <- if (type == "positive_scalar") 0 else 0
    U <- if (type == "positive_scalar") Inf else 1
    lo <- max(lower[1L], L); hi <- min(upper[1L], U)
    return(list(nonempty = lo <= hi, lower = if (lo <= hi) lo else NA_real_,
                upper = if (lo <= hi) hi else NA_real_,
                boundary_touch = lo <= L || (is.finite(U) && hi >= U),
                uninformative = FALSE,
                reason = "scalar support intersection"))
  }
  if (type == "directional_heterogeneity") {
    lo <- pmax(lower, 0); hi <- upper
    if (any(hi < 0) || any(lo > hi)) {
      return(list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
                  boundary_touch = NA, uninformative = FALSE,
                  reason = "rectangle misses the nonnegative orthant"))
    }
    contains_origin <- all(lo == 0)
    has_positive_total <- any(hi > 0)
    if (!has_positive_total) {
      return(list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
                  boundary_touch = TRUE, uninformative = FALSE,
                  reason = "intersection contains no positive-total point"))
    }
    if (contains_origin) {
      return(list(nonempty = TRUE, lower = 0, upper = 1,
                  boundary_touch = TRUE, uninformative = TRUE,
                  reason = "origin rule gives the closure [0,1]"))
    }
    lo_share <- lo[1L] / (lo[1L] + hi[2L])
    hi_share <- hi[1L] / (hi[1L] + lo[2L])
    return(list(nonempty = TRUE, lower = lo_share, upper = hi_share,
                boundary_touch = any(lo == 0), uninformative = FALSE,
                reason = "image of nonnegative primitive rectangle"))
  }
  if (type == "subgroup_share") {
    lo <- pmax(lower, 0); hi <- pmin(upper, 1)
    feasible <- all(lo <= hi) && lo[1L] <= hi[2L]
    if (!feasible || hi[2L] <= 0) {
      return(list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
                  boundary_touch = NA, uninformative = FALSE,
                  reason = "rectangle misses 0 <= numerator <= denominator <= 1"))
    }
    qlo <- lo[1L] / hi[2L]
    amax <- min(hi[1L], hi[2L])
    if (amax <= 0) {
      qhi <- 0
    } else if (amax >= lo[2L]) {
      qhi <- 1
    } else {
      qhi <- amax / lo[2L]
    }
    boundary <- lo[1L] == 0 || hi[2L] == 1 || qhi == 1
    return(list(nonempty = TRUE, lower = max(0, qlo),
                upper = min(1, qhi), boundary_touch = boundary,
                uninformative = qlo == 0 && qhi == 1,
                reason = "image of constrained subgroup primitive rectangle"))
  }
  stop("No confidence-set map for target type `", type, "`.",
       call. = FALSE)
}

#' Create one protocol-v2.2 gate row
#'
#' @param gate_id,category,scope,severity,state,comparator,units,message Text
#'   fields from the frozen gate schema.
#' @param statistic,threshold Recorded values. They are list-columns so numeric,
#'   logical, and calibrated-text thresholds are retained without coercion.
#' @param provenance Nonempty audit provenance.
#' @param scope_id Optional target, primitive, family, fit, or analysis ID.
#' @return A one-row data frame satisfying the v2.2 gate schema.
#' @export
scmix_v22_gate_record <- function(
    gate_id, category, scope, severity, state, statistic, threshold,
    comparator, units, provenance, message, scope_id = NA_character_) {
  gate_id <- .scmix_v22_text(gate_id, "gate_id")
  dictionary_at <- match(gate_id, .scmix_v22_gate_dictionary$gate_id)
  if (is.na(dictionary_at)) {
    stop("gate_id is outside the frozen v2.2 registry.", call. = FALSE)
  }
  category <- .scmix_v22_choice(category, c(
    "protocol_integrity", "design_data", "fit_computation",
    "numerical_integration", "loading_contract", "nuisance_verification",
    "identification_regularity", "inverse_problem", "target_constraint",
    "resampling_multiplicity", "specification_assessment"), "category")
  scope <- .scmix_v22_text(scope, "scope")
  severity <- .scmix_v22_choice(severity, c(
    "hard_fit", "hard_target", "hard_family", "warning",
    "interpretation"), "severity")
  state <- .scmix_v22_choice(state, c(
    "pass", "warn", "fail", "not_run", "not_applicable"), "state")
  comparator <- .scmix_v22_text(comparator, "comparator")
  units <- .scmix_v22_text(units, "units")
  expected_comparator <- .scmix_v22_gate_dictionary$comparator[dictionary_at]
  expected_units <- .scmix_v22_gate_dictionary$units[dictionary_at]
  if (!identical(comparator, expected_comparator) ||
      !identical(units, expected_units)) {
    stop("Gate ", gate_id, " must use frozen comparator `",
         expected_comparator, "` and units `", expected_units, "`.",
         call. = FALSE)
  }
  provenance <- .scmix_v22_text(provenance, "provenance")
  message <- .scmix_v22_text(message, "message")
  if (state == "not_applicable" && !grepl("not applicable|does not apply|q=0|single",
                                           message, ignore.case = TRUE)) {
    stop("A not_applicable gate must state why it does not apply.",
         call. = FALSE)
  }
  data.frame(
    gate_id = gate_id, category = category, scope = scope,
    severity = severity, state = state,
    statistic = I(list(statistic)), threshold = I(list(threshold)),
    comparator = comparator, units = units, provenance = provenance,
    message = message, scope_id = as.character(scope_id),
    stringsAsFactors = FALSE
  )
}

.scmix_v22_mechanical_gate_state <- function(
    statistic, threshold, comparator, severity) {
  scalar <- function(x) length(x) == 1L && !is.list(x) && !is.na(x)
  passed <- if (comparator == "equal") {
    if (!scalar(statistic) || !scalar(threshold)) return(NA_character_)
    identical(statistic, threshold)
  } else if (comparator %in% c(
    "less_than_or_equal", "greater_than_or_equal")) {
    if (!scalar(statistic) || !scalar(threshold) ||
        !is.numeric(statistic) || !is.numeric(threshold) ||
        !is.finite(statistic) || !is.finite(threshold)) {
      return(NA_character_)
    }
    if (comparator == "less_than_or_equal") {
      statistic <= threshold
    } else statistic >= threshold
  } else {
    return(NA_character_)
  }
  if (isTRUE(passed)) "pass" else if (severity == "warning") "warn" else
    "fail"
}

#' Start a fail-closed protocol-v2.2 gate table
#'
#' Every frozen gate appears once in state `not_run`. The application pipeline
#' must replace each row with signed evidence or an explicit, reasoned
#' `not_applicable` row. Target-, primitive-, dependency-, and family-scoped
#' gates should be expanded to one row per registered scope ID.
#'
#' @return A gate data frame with all required fields.
#' @export
scmix_v22_gate_template <- function() {
  rows <- lapply(seq_len(nrow(.scmix_v22_gate_dictionary)), function(i) {
    x <- .scmix_v22_gate_dictionary[i, ]
    scmix_v22_gate_record(
      x$gate_id, x$category, x$scope, x$severity, "not_run",
      NA, "FROM_FROZEN_PROTOCOL", x$comparator, x$units,
      "inst/protocol/inference-v2.2.yaml",
      paste0("Gate ", x$gate_id,
             " has not yet been populated from signed evidence."),
      paste0("TEMPLATE:", x$scope))
  })
  do.call(rbind, rows)
}

#' Validate a complete protocol-v2.2 gate artifact
#'
#' Conditional gates remain in the artifact with state `not_applicable` and a
#' reason. Repeated gate IDs are allowed across distinct target, primitive, or
#' family scope IDs, but an exact scoped row may occur only once.
#'
#' @param gates Gate data frame.
#' @param require_all_ids Whether every frozen gate ID must occur at least once.
#' @return The input, invisibly, or an error identifying the malformed record.
#' @export
scmix_v22_validate_gate_record <- function(gates, require_all_ids = TRUE) {
  if (!is.data.frame(gates)) stop("gates must be a data frame.", call. = FALSE)
  missing_fields <- setdiff(.scmix_v22_gate_fields, names(gates))
  if (length(missing_fields)) {
    stop("Gate record is missing field(s): ",
         paste(missing_fields, collapse = ", "), ".", call. = FALSE)
  }
  if (any(!gates$state %in% c(
    "pass", "warn", "fail", "not_run", "not_applicable"))) {
    stop("Gate record contains an invalid state.", call. = FALSE)
  }
  if (any(!gates$severity %in% c(
    "hard_fit", "hard_target", "hard_family", "warning",
    "interpretation"))) {
    stop("Gate record contains an invalid severity.", call. = FALSE)
  }
  if (any(!gates$gate_id %in% .scmix_v22_required_gate_ids)) {
    stop("Gate record contains an ID outside the frozen v2.2 registry: ",
         paste(unique(setdiff(gates$gate_id,
                              .scmix_v22_required_gate_ids)),
               collapse = ", "), ".", call. = FALSE)
  }
  dictionary_row <- match(gates$gate_id,
                          .scmix_v22_gate_dictionary$gate_id)
  expected <- .scmix_v22_gate_dictionary[
    dictionary_row,
    c("category", "scope", "severity", "comparator", "units"),
    drop = FALSE]
  mismatch <- gates$category != expected$category |
    gates$scope != expected$scope | gates$severity != expected$severity |
    gates$comparator != expected$comparator | gates$units != expected$units
  mismatch[is.na(mismatch)] <- TRUE
  if (any(mismatch)) {
    bad <- unique(gates$gate_id[mismatch])
    stop("Gate record changes frozen category, scope, severity, comparator, or units for: ",
         paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  mechanically_expected <- vapply(seq_len(nrow(gates)), function(i) {
    if (gates$state[i] == "not_applicable") return(NA_character_)
    .scmix_v22_mechanical_gate_state(
      gates$statistic[[i]], gates$threshold[[i]], gates$comparator[i],
      gates$severity[i])
  }, character(1L))
  inconsistent_state <- !is.na(mechanically_expected) &
    gates$state != mechanically_expected
  if (any(inconsistent_state)) {
    bad <- paste0(gates$gate_id[inconsistent_state], "@",
                  gates$scope_id[inconsistent_state])
    stop("Gate state disagrees with its frozen comparator and recorded statistic/threshold: ",
         paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  if (isTRUE(require_all_ids)) {
    missing_ids <- setdiff(.scmix_v22_required_gate_ids,
                           unique(gates$gate_id))
    if (length(missing_ids)) {
      stop("Gate record is incomplete; missing: ",
           paste(missing_ids, collapse = ", "), ".", call. = FALSE)
    }
  }
  scope_id <- if ("scope_id" %in% names(gates)) gates$scope_id else
    rep(NA_character_, nrow(gates))
  unscoped <- is.na(scope_id) | !nzchar(trimws(scope_id))
  if (any(unscoped)) {
    stop("Every v2.2 gate row must carry an explicit scope_id.",
         call. = FALSE)
  }
  key <- paste(gates$gate_id, gates$scope,
               ifelse(is.na(scope_id), "", scope_id), sep = "|")
  if (anyDuplicated(key)) {
    stop("Gate record contains a duplicated gate/scope/scope_id row.",
         call. = FALSE)
  }
  bad_na <- gates$state == "not_applicable" &
    !grepl("not applicable|does not apply|q=0|single", gates$message,
           ignore.case = TRUE)
  if (any(bad_na)) {
    stop("Every not_applicable gate must record a reason.", call. = FALSE)
  }
  unconditional_na <- gates$state == "not_applicable" &
    !gates$gate_id %in% .scmix_v22_conditional_gate_ids
  if (any(unconditional_na)) {
    stop("A default-always v2.2 gate cannot be marked not_applicable: ",
         paste(unique(gates$gate_id[unconditional_na]), collapse = ", "),
         ".", call. = FALSE)
  }
  invisible(gates)
}

.scmix_v22_expected_scope_ids <- function(scope, registry, contract) {
  switch(scope,
    analysis = contract$analysis_signature,
    target = vapply(registry, `[[`, character(1L), "target_id"),
    primitive = unique(unlist(lapply(registry, `[[`,
                                     "primitive_registry_ids"))),
    family = unique(vapply(registry, `[[`, character(1L),
                                     "target_family_id")),
    target_parameter_dependencies = unique(unlist(lapply(
      registry, `[[`, "target_parameter_dependencies"))),
    sample = contract$application_id,
    specification = contract$specification_id,
    application = contract$application_id,
    fit = contract$specification_id,
    fit_block = c("position_effect", "unpenalized_mean_reference",
                  "centered_mean_deviation", "horizontal_loadings",
                  "all_structural_coordinates"),
    character())
}

.scmix_v22_external_gate_applicable <- function(
    gate_id, scope_id, registry, contract) {
  if (!gate_id %in% .scmix_v22_conditional_gate_ids) return(TRUE)
  if (gate_id == "F007") {
    return(isTRUE(contract$exact_nested_pair_registered))
  }
  if (gate_id == "F008") {
    return(length(unique(unlist(contract$optimizer_starts,
                                recursive = TRUE))) > 1L)
  }
  if (gate_id %in% c("I001", "I002")) {
    target_ids <- vapply(registry, `[[`, character(1L), "target_id")
    at <- match(scope_id, target_ids)
    if (is.na(at)) return(NA)
    return(isTRUE(registry[[at]]$uses_numerical_integration))
  }
  if (gate_id %in% c("I003", "I004")) {
    return(isTRUE(contract$fit_uses_numerical_integration))
  }
  if (gate_id == "I005") {
    return(contract$q > 0L &&
             isTRUE(contract$fit_uses_numerical_integration))
  }
  if (gate_id %in% c("L001", "L002", "G002", "G004")) {
    return(contract$q > 0L)
  }
  if (gate_id == "G003") {
    return(scope_id %in% unique(unlist(lapply(
      registry, `[[`, "target_parameter_dependencies"))))
  }
  ## The remaining conditional gates are local and cannot enter an external
  ## artifact. Their applicability is derived by the finalizer itself.
  NA
}

#' Sign nonlocal v2.2 gate evidence against the frozen registry
#'
#' @param gates Scoped gate rows produced by the fit and application audits.
#' @param registry The complete frozen target registry, including failed
#'   targets and all family primitives.
#' @param analysis_contract Signed [scmix_v22_analysis_contract()].
#' @param provenance Nonempty immutable audit provenance.
#' @return A fingerprinted `scmix_v22_gate_artifact`.
#' @export
scmix_v22_gate_artifact <- function(gates, registry, analysis_contract,
                                    provenance) {
  registry <- .scmix_v22_registry_payload(registry)
  if (!.scmix_v22_intact(analysis_contract, "scmix_v22_analysis_contract")) {
    stop("analysis_contract must be an intact signed v2.2 contract.",
         call. = FALSE)
  }
  scmix_v22_validate_gate_record(gates, require_all_ids = FALSE)
  forbidden <- intersect(unique(gates$gate_id), .scmix_v22_local_gate_ids)
  if (length(forbidden)) {
    stop("Externally signed gate artifacts may not replace local gate(s): ",
         paste(forbidden, collapse = ", "), ".", call. = FALSE)
  }
  computation_forbidden <- intersect(
    unique(gates$gate_id), .scmix_v22_computation_gate_ids)
  if (length(computation_forbidden)) {
    stop("Generic external gate artifacts may not supply typed computation gate(s): ",
         paste(computation_forbidden, collapse = ", "), ".", call. = FALSE)
  }
  deterministic <- c("analysis", "target", "primitive", "family",
                     "target_parameter_dependencies", "sample",
                     "specification", "application", "fit", "fit_block")
  for (id in unique(gates$gate_id)) {
    rows <- gates[gates$gate_id == id, , drop = FALSE]
    scope <- unique(rows$scope)
    if (length(scope) != 1L) {
      stop("A gate ID cannot change scope inside an artifact.", call. = FALSE)
    }
    if (scope %in% deterministic) {
      expected <- sort(unique(.scmix_v22_expected_scope_ids(
        scope, registry, analysis_contract)), method = "radix")
      observed <- sort(unique(rows$scope_id), method = "radix")
      if (!identical(expected, observed)) {
        stop("Gate ", id, " does not cover its complete frozen `", scope,
             "` scope.", call. = FALSE)
      }
    }
  }
  applicable <- mapply(
    .scmix_v22_external_gate_applicable,
    gates$gate_id, gates$scope_id,
    MoreArgs = list(registry = registry, contract = analysis_contract),
    USE.NAMES = FALSE)
  if (anyNA(applicable)) {
    stop("Gate applicability could not be derived from the signed registry and analysis contract.",
         call. = FALSE)
  }
  wrong_applicability <- (applicable & gates$state == "not_applicable") |
    (!applicable & gates$state != "not_applicable")
  if (any(wrong_applicability)) {
    bad <- paste0(gates$gate_id[wrong_applicability], "@",
                  gates$scope_id[wrong_applicability])
    stop("Gate state disagrees with frozen, contract-derived applicability: ",
         paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = analysis_contract$analysis_signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    registry_fingerprint = .scmix_v22_fingerprint(registry),
    registry = registry,
    gates = gates,
    provenance = .scmix_v22_text(provenance, "provenance"))
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_gate_artifact", "list")
  payload
}

.scmix_v22_computation_applicability <- function(contract, registry) {
  valid_scalar_logical <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }
  ## Missing declarations are treated as applicable. This conservative
  ## fallback protects old signed contracts rather than converting absence
  ## into a self-declared exception. Current v2.2 contracts carry every field.
  exact_pair <- if (valid_scalar_logical(
      contract$exact_nested_pair_registered)) {
    contract$exact_nested_pair_registered
  } else TRUE
  fit_integration <- if (valid_scalar_logical(
      contract$fit_uses_numerical_integration)) {
    contract$fit_uses_numerical_integration
  } else TRUE
  q <- contract$q
  q_positive <- if (is.numeric(q) && length(q) == 1L && is.finite(q) &&
      q >= 0 && q == as.integer(q)) as.integer(q) > 0L else TRUE
  starts <- contract$optimizer_starts
  more_than_one_start <- if (inherits(
      starts, "scmix_v22_fit_scope_registry") && is.data.frame(starts$starts)) {
    counts <- table(starts$starts$fit_scope_id)
    length(counts) > 0L && any(counts > 1L)
  } else {
    length(unique(unlist(starts, use.names = FALSE))) > 1L
  }
  target_ids <- vapply(registry, `[[`, character(1L), "target_id")
  target_integration <- stats::setNames(vapply(registry, function(spec) {
    if (valid_scalar_logical(spec$uses_numerical_integration)) {
      spec$uses_numerical_integration
    } else fit_integration
  }, logical(1L)), target_ids)
  list(F007 = isTRUE(exact_pair), F008 = isTRUE(more_than_one_start),
       target_integration = target_integration,
       fit_integration = isTRUE(fit_integration),
       covariance_integration = isTRUE(fit_integration) && q_positive)
}

.scmix_v22_missing_computation_gate <- function(
    gate_id, scope_id, applicable, threshold, units, message) {
  dictionary <- .scmix_v22_gate_dictionary[
    .scmix_v22_gate_dictionary$gate_id == gate_id, , drop = FALSE]
  state <- if (isTRUE(applicable)) "not_run" else "not_applicable"
  scmix_v22_gate_record(
    gate_id, dictionary$category, dictionary$scope, dictionary$severity,
    state, NA_real_, threshold, "less_than_or_equal", units,
    "v2.2 typed computation-artifact finalizer",
    if (isTRUE(applicable)) {
      paste(message, "Required authenticated evidence was not supplied.")
    } else {
      paste(message, "This gate is not applicable under the signed contract.")
    }, scope_id)
}

.scmix_v22_computation_placeholder_rows <- function(
    families, applicability, registry, contract, calibration) {
  threshold <- function(name) {
    if (.scmix_v22_validate_calibration(
        calibration, contract, require_complete = TRUE)) {
      calibration$constants[[name]]
    } else "CALIBRATE_IN_STEP_3"
  }
  target_ids <- vapply(registry, `[[`, character(1L), "target_id")
  rows <- list()
  add <- function(x) rows[[length(rows) + 1L]] <<- x
  if ("F007" %in% families) add(.scmix_v22_missing_computation_gate(
    "F007", paste0(contract$specification_id, "::nested_pair"),
    applicability$F007, threshold("nested_fit_relative_cutoff"),
    "relative_mean_objective", "Exact nested-fit evidence is conditional on a registered pair."))
  if ("F008" %in% families) for (id in target_ids) {
    add(.scmix_v22_missing_computation_gate(
      "F008", id, applicability$F008,
      threshold("multistart_target_se_cutoff"), "standard_errors",
      "Multi-start target stability is conditional on multiple registered starts."))
  }
  if ("integration" %in% families) {
    for (id in target_ids) {
      applicable <- applicability$target_integration[[id]]
      add(.scmix_v22_missing_computation_gate(
        "I001", id, applicable, threshold("integration_target_se_cutoff"),
        "standard_errors", "Target integration refinement follows the signed target specification."))
      add(.scmix_v22_missing_computation_gate(
        "I002", id, applicable, threshold("integration_if_l2_cutoff"),
        "influence_standard_deviations",
        "Influence integration refinement follows the signed target specification."))
    }
    for (spec in list(
      list(id = "I003", applicable = applicability$fit_integration,
           cutoff = "integration_objective_relative_cutoff",
           units = "relative_mean_objective"),
      list(id = "I004", applicable = applicability$fit_integration,
           cutoff = "integration_score_relative_cutoff",
           units = "relative_normalized_gradient"),
      list(id = "I005", applicable = applicability$covariance_integration,
           cutoff = "integration_covariance_relative_cutoff",
           units = "relative_frobenius_norm"))) {
      add(.scmix_v22_missing_computation_gate(
        spec$id, contract$specification_id, spec$applicable,
        threshold(spec$cutoff), spec$units,
        "Fit-level integration refinement follows the signed rank and integration contract."))
    }
  }
  if (!length(rows)) NULL else do.call(.scmix_v22_bind_gates, rows)
}

.scmix_v22_validate_computation_rows <- function(
    rows, family, applicability, registry, contract) {
  scmix_v22_validate_gate_record(rows, require_all_ids = FALSE)
  target_ids <- vapply(registry, `[[`, character(1L), "target_id")
  require_applicability <- function(z, expected, what) {
    observed <- z$state != "not_applicable"
    if (!identical(unname(observed), unname(expected))) {
      stop("Authenticated ", what,
           " rows disagree with applicability derived from the signed contract.",
           call. = FALSE)
    }
  }
  if (identical(family, "F007")) {
    if (nrow(rows) != 1L || !identical(rows$gate_id, "F007")) {
      stop("Authenticated F007 evidence must contain exactly one F007 row.",
           call. = FALSE)
    }
    require_applicability(rows, applicability$F007, "F007")
  } else if (identical(family, "F008")) {
    if (nrow(rows) != length(target_ids) ||
        anyDuplicated(rows$scope_id) ||
        anyNA(match(target_ids, rows$scope_id)) ||
        any(rows$gate_id != "F008")) {
      stop("Authenticated F008 evidence must cover every exact target once.",
           call. = FALSE)
    }
    z <- rows[match(target_ids, rows$scope_id), , drop = FALSE]
    require_applicability(z, rep(applicability$F008, length(target_ids)),
                          "F008")
  } else {
    for (id in c("I001", "I002")) {
      z <- rows[rows$gate_id == id, , drop = FALSE]
      if (nrow(z) != length(target_ids) || anyDuplicated(z$scope_id) ||
          anyNA(match(target_ids, z$scope_id))) {
        stop("Authenticated ", id,
             " evidence must cover every exact target once.", call. = FALSE)
      }
      z <- z[match(target_ids, z$scope_id), , drop = FALSE]
      require_applicability(z, unname(applicability$target_integration), id)
    }
    expected_fit <- c(I003 = applicability$fit_integration,
                      I004 = applicability$fit_integration,
                      I005 = applicability$covariance_integration)
    for (id in names(expected_fit)) {
      z <- rows[rows$gate_id == id, , drop = FALSE]
      if (nrow(z) != 1L ||
          !identical(z$scope_id, contract$specification_id)) {
        stop("Authenticated ", id,
             " evidence must contain the exact fit scope once.", call. = FALSE)
      }
      require_applicability(z, expected_fit[[id]], id)
    }
  }
  rows
}

.scmix_v22_computation_gate_rows <- function(
    computation_artifacts, contract, registry, calibration) {
  applicability <- .scmix_v22_computation_applicability(contract, registry)
  families <- c("F007", "F008", "integration")
  if (is.null(computation_artifacts)) computation_artifacts <- list()
  if (!is.list(computation_artifacts)) {
    computation_artifacts <- list(computation_artifacts)
  } else if (inherits(computation_artifacts,
                      c("scmix_v22_nested_fit_stability",
                        "scmix_v22_multistart_stability",
                        "scmix_v22_integration_refinement"))) {
    computation_artifacts <- list(computation_artifacts)
  }
  authenticated <- lapply(computation_artifacts, function(artifact) {
    rows <- scmix_v22_validate_computation_artifact(
      artifact, contract, registry, calibration)
    list(family = attr(rows, "computation_artifact_type"), rows = rows,
         fingerprint = attr(rows, "computation_artifact_fingerprint"))
  })
  observed <- vapply(authenticated, `[[`, character(1L), "family")
  if (any(!observed %in% families) || anyDuplicated(observed)) {
    stop("Typed computation artifacts must contain at most one exact artifact per F007, F008, or integration family.",
         call. = FALSE)
  }
  supplied <- lapply(authenticated, function(x)
    .scmix_v22_validate_computation_rows(
      x$rows, x$family, applicability, registry, contract))
  missing <- setdiff(families, observed)
  supplied_rows <- if (!length(supplied)) NULL else
    do.call(.scmix_v22_bind_gates, supplied)
  rows <- .scmix_v22_bind_gates(
    supplied_rows,
    .scmix_v22_computation_placeholder_rows(
      missing, applicability, registry, contract, calibration))
  list(rows = rows, applicability = applicability,
       fingerprints = if (length(authenticated)) stats::setNames(
         vapply(authenticated, `[[`, character(1L), "fingerprint"), observed)
       else character())
}

.scmix_v22_bind_gates <- function(...) {
  rows <- Filter(Negate(is.null), list(...))
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.scmix_v22_deduplicate_identical_gates <- function(gates) {
  if (is.null(gates) || !nrow(gates)) return(gates)
  key <- paste(gates$gate_id, gates$scope, gates$scope_id, sep = "|")
  keep <- rep(TRUE, nrow(gates))
  for (indices in split(seq_len(nrow(gates)), key)) {
    if (length(indices) < 2L) next
    reference <- gates[indices[1L], , drop = FALSE]
    rownames(reference) <- NULL
    same <- vapply(indices[-1L], function(i) {
      candidate <- gates[i, , drop = FALSE]
      rownames(candidate) <- NULL
      identical(candidate, reference)
    }, logical(1L))
    if (all(same)) keep[indices[-1L]] <- FALSE
  }
  out <- gates[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.scmix_v22_gate_state <- function(value, threshold, comparator) {
  if (is.null(threshold) || length(threshold) != 1L ||
      is.character(threshold) || is.na(threshold) ||
      length(value) != 1L || is.na(value)) return("not_run")
  pass <- switch(comparator,
                 less_than_or_equal = value <= threshold,
                 greater_than_or_equal = value >= threshold,
                 equal = identical(value, threshold),
                 stop("Unsupported comparator `", comparator, "`.",
                      call. = FALSE))
  if (isTRUE(pass)) "pass" else "fail"
}

#' Reduce v2.2 gate rows to the non-overloaded status schema
#'
#' Applicable hard gates in state `not_run` are treated as missing required
#' evidence and therefore fail closed. A caller that omits the complete signed
#' external gate record receives a synthetic P001 hard-fit failure.
#'
#' @param gates Gate rows from [scmix_v22_gate_record()].
#' @param target_id,family_id IDs whose scoped rows should be included.
#' @param primitive_ids,parameter_dependencies Signed primitive and structural
#'   block IDs whose scoped rows propagate to this target.
#' @param release_class,interval_method,multiplicity_status Proposed reporting
#'   labels; they are reset when a hard gate withholds the result.
#' @param complete_external_record Whether all nonlocal v2.2 gates have been
#'   supplied by the signed analysis pipeline.
#' @return A list containing every required v2.2 status field.
#' @export
scmix_v22_status <- function(
    gates, target_id, family_id, primitive_ids = character(),
    parameter_dependencies = character(),
    release_class = "none", interval_method = "none",
    multiplicity_status = "none", complete_external_record = FALSE) {
  target_id <- .scmix_v22_text(target_id, "target_id")
  family_id <- .scmix_v22_text(family_id, "family_id")
  if (is.null(gates)) gates <- data.frame()
  if (!is.data.frame(gates)) stop("gates must be a data frame.", call. = FALSE)
  if (!isTRUE(complete_external_record)) {
    has_p001 <- "gate_id" %in% names(gates) && any(gates$gate_id == "P001")
    if (has_p001) {
      rows <- gates$gate_id == "P001"
      gates$state[rows] <- "fail"
      gates$statistic[rows] <- rep(list(FALSE), sum(rows))
      gates$threshold[rows] <- rep(list(TRUE), sum(rows))
      gates$comparator[rows] <- "equal"
      gates$units[rows] <- "boolean"
      gates$provenance[rows] <- "v2.2 finalizer"
      gates$message[rows] <-
        "The complete signed and scoped gate record was not supplied or did not validate."
    } else {
      missing <- scmix_v22_gate_record(
        "P001", "protocol_integrity", "analysis", "hard_fit", "fail",
        FALSE, TRUE, "equal", "boolean", "v2.2 finalizer",
        "The complete signed external gate record was not supplied.",
        scope_id = "analysis")
      gates <- .scmix_v22_bind_gates(gates, missing)
    }
  }
  scmix_v22_validate_gate_record(gates, require_all_ids = FALSE)
  relevant <- rep(TRUE, nrow(gates))
  if ("scope_id" %in% names(gates)) {
    scoped <- !is.na(gates$scope_id) & nzchar(gates$scope_id)
    target_scope <- gates$scope == "target"
    primitive_scope <- gates$scope == "primitive"
    dependency_scope <- gates$scope == "target_parameter_dependencies"
    family_scope <- gates$scope == "family"
    relevant[scoped & target_scope] <-
      gates$scope_id[scoped & target_scope] == target_id
    relevant[scoped & primitive_scope] <-
      gates$scope_id[scoped & primitive_scope] %in% primitive_ids
    relevant[scoped & dependency_scope] <-
      gates$scope_id[scoped & dependency_scope] %in%
        c(target_id, parameter_dependencies)
    relevant[scoped & family_scope] <-
      gates$scope_id[scoped & family_scope] == family_id
  }
  g <- gates[relevant, , drop = FALSE]
  effective_fail <- g$state == "fail" |
    (g$state == "not_run" & g$severity %in%
       c("hard_fit", "hard_target", "hard_family"))
  hard_fit <- effective_fail & g$severity == "hard_fit"
  hard_target <- effective_fail & g$severity == "hard_target"
  hard_family <- effective_fail & g$severity == "hard_family"
  warnings <- g$state == "warn" | (g$state == "fail" &
                                      g$severity == "warning")
  interpretation_rows <- g$severity == "interpretation"
  interpretation_status <- if (any(g$state[interpretation_rows] == "fail")) {
    "unsupported"
  } else if (!any(interpretation_rows) ||
             any(g$state[interpretation_rows] %in% c("warn", "not_run"))) {
    "qualified"
  } else "supported"
  fit_status <- if (any(hard_fit)) "withheld" else "eligible"
  target_status <- if (any(hard_fit) || any(hard_target)) "withheld" else
    "conditional_available"
  hard_observed <- unique(g$gate_id[hard_fit | hard_target | hard_family])
  warning_observed <- unique(g$gate_id[warnings])
  hard_codes <- .scmix_v22_required_gate_ids[
    .scmix_v22_required_gate_ids %in% hard_observed]
  warning_codes <- .scmix_v22_required_gate_ids[
    .scmix_v22_required_gate_ids %in% warning_observed]
  first_failed <- if (length(hard_codes)) hard_codes[1L] else NA_character_
  if (target_status == "withheld") {
    release_class <- "none"; interval_method <- "none"
    multiplicity_status <- "none"
  } else if (any(hard_family) &&
             !identical(multiplicity_status, "bonferroni_incomplete")) {
    multiplicity_status <- "none"
  }
  list(
    calculation_status = "computed", fit_status = fit_status,
    target_status = target_status, release_class = release_class,
    interval_method = interval_method,
    multiplicity_status = multiplicity_status,
    interpretation_status = interpretation_status,
    hard_failure_codes = hard_codes, warning_codes = warning_codes,
    first_failed_gate = first_failed, gate_record = g
  )
}

.scmix_v22_prepare <- function(inference, spec, full_sample_plugin = NULL) {
  if (!inherits(inference, "scmix_dml") || is.null(inference$influence)) {
    stop("inference must be a computed scmix_dml object with influence rows.",
         call. = FALSE)
  }
  cf_all <- if (!is.null(inference$crossfit_plugin))
    inference$crossfit_plugin else inference$plugin_estimate
  raw_all <- if (!is.null(inference$raw_one_step))
    inference$raw_one_step else inference$estimate
  primitive_mode <- isTRUE(inference$v22_primitive_mode)
  if (primitive_mode) {
    need <- spec$primitive_registry_ids
    if (is.null(names(cf_all)) || is.null(names(raw_all)) ||
        !all(need %in% names(cf_all)) || !all(need %in% names(raw_all)) ||
        !all(need %in% colnames(inference$influence))) {
      stop("The v2.2 DML object lacks a directly fitted minimal primitive for target `",
           spec$target_id, "`.", call. = FALSE)
    }
    cf_primitive <- cf_all[need]
    raw_primitive <- raw_all[need]
    primitive_if <- as.matrix(inference$influence[, need, drop = FALSE])
    geom <- .scmix_v22_primitive_geometry(spec, cf_primitive)
    cf <- list(
      primitive = cf_primitive, target = geom$target,
      target_jacobian = geom$target_jacobian,
      supporting_jacobian = diag(length(need)),
      primitive_names = need,
      supporting_audit = lapply(inference$fold_details, function(fd)
        fd$v22_supporting_audit[[spec$target_id]]))
    cf_support <- cf_primitive
    raw_support <- raw_primitive
    riesz_names <- need
  } else {
    if (identical(spec$target_type, "directional_heterogeneity")) {
      stop("Directional-heterogeneity inference requires scmix_dml(..., ",
           "v22_registry=...) so H_Z and H_R receive direct primitive Riesz ",
           "solves.", call. = FALSE)
    }
    if (is.null(names(cf_all)) || is.null(names(raw_all)) ||
        !all(spec$supporting_names %in% names(cf_all)) ||
        !all(spec$supporting_names %in% names(raw_all)) ||
        !all(spec$supporting_names %in% colnames(inference$influence))) {
      stop("The DML object lacks a registered supporting moment column for target `",
           spec$target_id, "`.", call. = FALSE)
    }
    cf_support <- cf_all[spec$supporting_names]
    raw_support <- raw_all[spec$supporting_names]
    cf <- .scmix_v22_target_geometry(spec, cf_support)
    delta_support <- raw_support - cf_support
    raw_primitive <- as.numeric(cf$primitive +
      cf$supporting_jacobian %*% delta_support)
    names(raw_primitive) <- cf$primitive_names
    names(cf$primitive) <- cf$primitive_names
    support_if <- as.matrix(inference$influence[, spec$supporting_names,
                                               drop = FALSE])
    primitive_if <- support_if %*% t(cf$supporting_jacobian)
    colnames(primitive_if) <- spec$primitive_registry_ids
    riesz_names <- spec$supporting_names
  }
  names(raw_primitive) <- names(cf$primitive) <- spec$primitive_registry_ids
  colnames(primitive_if) <- spec$primitive_registry_ids
  respondent_id <- rownames(primitive_if)
  if (is.null(respondent_id) && !is.null(inference$respondent_id)) {
    respondent_id <- as.character(inference$respondent_id)
  }
  if (is.null(respondent_id) || anyDuplicated(respondent_id)) {
    stop("The DML influence matrix must retain unique respondent row names.",
         call. = FALSE)
  }
  N <- nrow(primitive_if)
  primitive_se <- apply(primitive_if, 2L, stats::sd) / sqrt(N)
  target_if <- as.numeric(primitive_if %*% t(cf$target_jacobian))
  target_if <- target_if - mean(target_if)
  target_se <- stats::sd(target_if) / sqrt(N)
  raw_target <- as.numeric(cf$target +
    cf$target_jacobian %*% (raw_primitive - cf$primitive))

  full_target <- NA_real_; full_primitive <- rep(NA_real_, length(cf$primitive))
  if (!is.null(full_sample_plugin)) {
    if (all(spec$primitive_registry_ids %in% names(full_sample_plugin))) {
      full_primitive <- full_sample_plugin[spec$primitive_registry_ids]
      full_target <- .scmix_v22_primitive_geometry(
        spec, full_primitive)$target
    } else if (all(spec$supporting_names %in% names(full_sample_plugin))) {
      full <- .scmix_v22_target_geometry(
        spec, full_sample_plugin[spec$supporting_names])
      full_target <- as.numeric(full$target)
      full_primitive <- as.numeric(full$primitive)
    }
  }
  names(full_primitive) <- spec$primitive_registry_ids

  direct <- length(cf$primitive) == 1L
  if (direct) {
    rr <- .scmix_v22_chart_update(as.numeric(cf$primitive),
                                  raw_primitive, spec$target_domain)
    reported_primitive <- reported_target <- rr$value
    chart_interior <- rr$interior
  } else {
    rr <- .scmix_v22_composite_chart(spec, cf$primitive, raw_primitive)
    reported_primitive <- rr$primitive; reported_target <- rr$target
    chart_interior <- rr$interior
  }
  names(reported_primitive) <- spec$primitive_registry_ids
  chart_ratio <- if (is.finite(target_se) && target_se > 0 &&
                     is.finite(reported_target))
    abs(reported_target - raw_target) / target_se else NA_real_
  correction_ratio <- if (is.finite(target_se) && target_se > 0)
    abs(raw_target - cf$target) / target_se else NA_real_
  list(
    spec = spec, respondent_id = respondent_id,
    full_sample_plugin = full_target,
    crossfit_plugin = as.numeric(cf$target),
    raw_one_step = raw_target,
    reported_estimate = as.numeric(reported_target),
    full_sample_primitive = full_primitive,
    crossfit_primitive = cf$primitive,
    raw_one_step_primitive = raw_primitive,
    reported_primitive = reported_primitive,
    supporting_crossfit = cf_support,
    supporting_raw_one_step = raw_support,
    supporting_jacobian = cf$supporting_jacobian,
    riesz_names = riesz_names,
    riesz_jacobian = if (primitive_mode) diag(length(riesz_names)) else
      cf$supporting_jacobian,
    supporting_audit = cf$supporting_audit,
    primitive_influence = primitive_if,
    target_influence = target_if,
    primitive_se = primitive_se,
    target_se = target_se,
    chart_interior = chart_interior,
    correction_se_ratio = correction_ratio,
    chart_discrepancy_se_ratio = chart_ratio,
    nonlinear_remainder_se_ratio = if (direct) NA_real_ else chart_ratio,
    raw_domain_distance = .scmix_v22_domain_distance(
      spec$target_type, raw_primitive, primitive_se),
    plugin_boundary_distance = .scmix_v22_boundary_distance(
      spec$target_type, cf$primitive, primitive_se)
  )
}

.scmix_v22_family_critical <- function(prepared, bank, level,
                                       analysis_contract = NULL) {
  registered_ids <- sort(unique(unlist(lapply(prepared, function(x)
    x$spec$primitive_registry_ids))), method = "radix")
  declared_sets <- lapply(prepared, function(x)
    x$spec$family_primitive_registry_ids)
  declaration_consistent <- all(vapply(declared_sets, function(x)
    identical(x, declared_sets[[1L]]), logical(1L)))
  ids <- if (declaration_consistent) declared_sets[[1L]] else registered_ids
  membership_complete <- declaration_consistent &&
    identical(registered_ids, ids)
  respondent_id <- sort(enc2utf8(as.character(prepared[[1L]]$respondent_id)),
                        method = "radix")
  columns <- vector("list", length(registered_ids)); names(columns) <- registered_ids
  conflict <- metadata_conflict <- character()
  for (id in registered_ids) {
    candidates <- lapply(prepared, function(x) {
      j <- match(id, colnames(x$primitive_influence))
      if (is.na(j)) NULL else {
        s <- match(id, x$spec$primitive_registry_ids)
        list(
          influence = x$primitive_influence[
            match(respondent_id, x$respondent_id), j],
          metadata = list(
            estimand = x$spec$primitive_estimands[s],
            orientation = x$spec$primitive_orientations[s],
            units = x$spec$primitive_units[s],
            reference_rule = x$spec$primitive_reference_rules[s],
            influence_definition =
              x$spec$primitive_influence_definitions[s]))
      }
    })
    candidates <- Filter(Negate(is.null), candidates)
    ref <- candidates[[1L]]$influence
    if (length(candidates) > 1L && any(vapply(candidates[-1L], function(x)
      !identical(unname(x$influence), unname(ref)), logical(1L)))) {
      conflict <- c(conflict, id)
    }
    if (length(candidates) > 1L && any(vapply(candidates[-1L], function(x)
      !identical(x$metadata, candidates[[1L]]$metadata), logical(1L)))) {
      metadata_conflict <- c(metadata_conflict, id)
    }
    columns[[id]] <- ref
  }
  IF <- do.call(cbind, columns)
  rownames(IF) <- respondent_id
  se <- apply(IF, 2L, stats::sd) / sqrt(nrow(IF))
  usable <- is.finite(se) & se > 0
  base <- list(ids = ids, registered_ids = registered_ids,
               membership_complete = membership_complete,
               declaration_consistent = declaration_consistent,
               respondent_id = respondent_id,
               influence_matrix = IF)
  if (!declaration_consistent || any(!registered_ids %in% ids)) {
    return(c(list(method = "none", critical = NA_real_, usable = usable,
                  conflict = character(),
                  reason = paste(
                    "family primitive declarations disagree or contain",
                    "an unregistered signed-family primitive")), base))
  }
  if (length(conflict) || length(metadata_conflict)) {
    all_conflict <- sort(unique(c(conflict, metadata_conflict)),
                         method = "radix")
    return(c(list(method = "none", critical = NA_real_,
                  usable = usable, conflict = all_conflict,
                  metadata_conflict = metadata_conflict,
                  reason = paste(
                    "duplicate primitive IDs have incompatible estimand,",
                    "orientation, units, reference, influence definition,",
                    "or exact respondent influence values")),
             base))
  }
  if (!membership_complete || !all(usable)) {
    crit <- stats::qnorm(1 - (1 - level) / (2 * length(ids)))
    reason <- if (!membership_complete) {
      "not all primitives in the frozen signed family were collected"
    } else {
      "one or more registered family primitives lacks a usable influence column"
    }
    return(c(list(method = "fixed_family_bonferroni", critical = crit,
                  usable = usable, conflict = character(), reason = reason),
             base))
  }
  if (length(ids) == 1L) {
    return(c(list(method = "pointwise", critical = stats::qnorm(
      1 - (1 - level) / 2), usable = usable,
      conflict = character(), reason = "single-primitive family",
      final_draw_count = NA, multiplier_draws = 0L), base))
  }
  if (is.null(bank)) {
    crit <- stats::qnorm(1 - (1 - level) / (2 * length(ids)))
    return(c(list(method = "fixed_family_bonferroni", critical = crit,
                  usable = usable, conflict = character(),
                  reason = paste("common respondent multiplier bank was not",
                                 "supplied; original-family Bonferroni used"),
                  final_draw_count = FALSE), base))
  }
  zeta <- .scmix_v22_validate_bank(bank, respondent_id, analysis_contract)
  errors <- crossprod(zeta, IF) / nrow(IF)
  studentized <- sweep(errors, 2L, se, `/`)
  critical <- unname(stats::quantile(
    apply(abs(studentized), 1L, max), probs = level,
    names = FALSE, type = 8))
  final_draw_count <- identical(ncol(zeta), 9999L)
  contract_linked <- isTRUE(attr(zeta, "analysis_contract_linked"))
  if (!final_draw_count || !contract_linked) {
    bonferroni <- stats::qnorm(1 - (1 - level) / (2 * length(ids)))
    return(c(list(
      method = "fixed_family_bonferroni", critical = bonferroni,
      diagnostic_method = "common_draw_max_t",
      diagnostic_critical = critical,
      usable = usable, conflict = character(),
      reason = if (!contract_linked) {
        paste("multiplier bank is not linked to the signed analysis seed and",
              "contract; original-family Bonferroni used")
      } else paste("diagnostic max-t used fewer than the required 9,999",
                   "draws; original-family Bonferroni used"),
      multiplier_fingerprint = bank$fingerprint,
      multiplier_draws = bank$draws,
      final_draw_count = final_draw_count,
      analysis_contract_linked = contract_linked), base))
  }
  c(list(method = "common_draw_max_t", critical = critical,
         usable = usable, conflict = character(), reason = "passed",
         multiplier_fingerprint = bank$fingerprint,
         multiplier_draws = bank$draws,
         final_draw_count = final_draw_count,
         analysis_contract_linked = contract_linked), base)
}

.scmix_v22_honest_riesz <- function(x, inference,
                                     numerical_rank_tolerance = NULL) {
  J <- x$riesz_jacobian
  support <- x$riesz_names
  primitive_ids <- x$spec$primitive_registry_ids
  K <- length(inference$fold_details)
  null <- defect <- matrix(NA_real_, K, length(primitive_ids),
                           dimnames = list(paste0("fold", seq_len(K)),
                                           primitive_ids))
  tolerance_ok <- is.numeric(numerical_rank_tolerance) &&
    length(numerical_rank_tolerance) == 1L &&
    is.finite(numerical_rank_tolerance) && numerical_rank_tolerance >= 0
  if (!K || !tolerance_ok) {
    return(list(null = null, defect = defect, available = FALSE,
                numerical_rank_tolerance_verified = FALSE))
  }
  influence_sd <- apply(x$primitive_influence, 2L, stats::sd)
  for (k in seq_len(K)) {
    fd <- inference$fold_details[[k]]
    required <- c("honest_evaluation_information",
                  "honest_evaluation_information_pseudoinverse",
                  "honest_evaluation_gradient",
                  "riesz_coefficients_whitened", "training_n")
    if (any(vapply(required, function(nm) is.null(fd[[nm]]), logical(1L)))) {
      next
    }
    if (is.null(fd$riesz_numerical_rank_tolerance) ||
        !isTRUE(all.equal(as.numeric(fd$riesz_numerical_rank_tolerance),
                          numerical_rank_tolerance, tolerance = 0,
                          check.attributes = FALSE))) next
    I <- as.matrix(fd$honest_evaluation_information)
    Iplus <- as.matrix(fd$honest_evaluation_information_pseudoinverse)
    g <- as.matrix(fd$honest_evaluation_gradient)
    alpha <- as.matrix(fd$riesz_coefficients_whitened)
    if (is.null(colnames(g))) colnames(g) <- colnames(inference$influence)
    if (is.null(colnames(alpha))) colnames(alpha) <- colnames(inference$influence)
    idx_g <- match(support, colnames(g)); idx_a <- match(support, colnames(alpha))
    if (anyNA(idx_g) || anyNA(idx_a) || nrow(g) != nrow(I) ||
        nrow(alpha) != nrow(I) || !identical(dim(I), dim(Iplus))) next
    gp <- g[, idx_g, drop = FALSE] %*% t(J)
    ap <- alpha[, idx_a, drop = FALSE] %*% t(J)
    projection <- I %*% Iplus
    for (j in seq_along(primitive_ids)) {
      gj <- gp[, j]
      outside <- (diag(nrow(I)) - projection) %*% gj
      null[k, j] <- sqrt(sum(outside^2)) /
        max(sqrt(sum(gj^2)), 1e-15)
      residual <- I %*% ap[, j] - gj
      quadratic <- as.numeric(crossprod(residual, Iplus %*% residual))
      numerator <- sqrt(nrow(x$primitive_influence) / fd$training_n) *
        sqrt(max(quadratic, 0))
      defect[k, j] <- numerator / max(influence_sd[j], 1e-15)
    }
  }
  maximum_null <- vapply(seq_len(ncol(null)), function(j) {
         z <- null[, j]; if (any(is.finite(z))) max(z[is.finite(z)]) else NA_real_
       }, numeric(1L))
  maximum_defect <- vapply(seq_len(ncol(defect)), function(j) {
         z <- defect[, j]; if (any(is.finite(z))) max(z[is.finite(z)]) else NA_real_
       }, numeric(1L))
  names(maximum_null) <- names(maximum_defect) <- primitive_ids
  list(null = null, defect = defect,
       maximum_null = maximum_null,
       maximum_defect = maximum_defect,
       available = all(is.finite(null)) && all(is.finite(defect)),
       numerical_rank_tolerance_verified = all(vapply(
         inference$fold_details, function(fd) {
           !is.null(fd$riesz_numerical_rank_tolerance) &&
             isTRUE(all.equal(as.numeric(fd$riesz_numerical_rank_tolerance),
                              numerical_rank_tolerance, tolerance = 0,
                              check.attributes = FALSE))
         }, logical(1L))))
}

.scmix_v22_ridge_neighborhood <- function(
    x, inference, family_result, level, multiplier_bank = NULL,
    analysis_contract = NULL) {
  primitive_ids <- x$spec$primitive_registry_ids
  support <- x$riesz_names
  J <- x$riesz_jacobian
  empty <- list(
    available = FALSE,
    maximum_change_over_se = stats::setNames(
      rep(NA_real_, length(primitive_ids)), primitive_ids),
    evaluations = data.frame(),
    reason = "full selected-ridge neighborhood artifact is absent")
  if (!length(inference$fold_details) ||
      !is.finite(x$target_se) || x$target_se <= 0) return(empty)
  crit <- family_result$critical
  if (!is.finite(crit)) crit <- stats::qnorm(1 - (1 - level) / 2)
  base_lo <- x$reported_primitive - crit * x$primitive_se
  base_hi <- x$reported_primitive + crit * x$primitive_se
  base_map <- .scmix_v22_map_rectangle(x$spec$target_type,
                                        base_lo, base_hi)
  recompute_critical <- function(primitive_if, affected) {
    if (!identical(family_result$method, "common_draw_max_t")) return(crit)
    family_if <- family_result$influence_matrix
    family_id <- family_result$ids
    family_respondent <- family_result$respondent_id
    if (!is.matrix(family_if) ||
        !identical(colnames(family_if), family_id) ||
        !identical(rownames(family_if), family_respondent) ||
        is.null(rownames(primitive_if))) return(NA_real_)
    for (id in affected) {
      at <- match(id, colnames(primitive_if))
      if (is.na(at) || !id %in% colnames(family_if)) return(NA_real_)
      aligned <- primitive_if[match(family_respondent,
                                    rownames(primitive_if)), at]
      if (length(aligned) != length(family_respondent) ||
          any(!is.finite(aligned))) return(NA_real_)
      family_if[, id] <- aligned
    }
    se_family <- apply(family_if, 2L, stats::sd) / sqrt(nrow(family_if))
    if (any(!is.finite(se_family)) || any(se_family <= 0)) return(NA_real_)
    zeta <- try(.scmix_v22_validate_bank(
      multiplier_bank, family_respondent, analysis_contract), silent = TRUE)
    if (inherits(zeta, "try-error") ||
        !isTRUE(attr(zeta, "analysis_contract_linked")) ||
        !identical(ncol(zeta), 9999L)) return(NA_real_)
    errors <- crossprod(zeta, family_if) / nrow(family_if)
    studentized <- sweep(errors, 2L, se_family, `/`)
    unname(stats::quantile(
      apply(abs(studentized), 1L, max), probs = level,
      names = FALSE, type = 8))
  }
  rows <- list()
  for (k in seq_along(inference$fold_details)) {
    fold_detail <- inference$fold_details[[k]]
    artifacts <- fold_detail$ridge_neighborhood_artifact
    if (!is.list(artifacts) || !all(support %in% names(artifacts))) {
      return(empty)
    }
    training_n <- fold_detail$training_n
    selected_by_target <- fold_detail$selected_nu_by_target
    if (!is.numeric(training_n) || length(training_n) != 1L ||
        !is.finite(training_n) || training_n <= 0 ||
        is.null(names(selected_by_target)) ||
        !all(support %in% names(selected_by_target))) return(empty)
    frozen_grid <- sort(unique(c(
      0, training_n^(-1), training_n^(-3 / 4), training_n^(-1 / 2))))
    for (s in seq_along(support)) {
      art <- artifacts[[support[s]]]
      needed <- c("nu", "raw_one_step", "standard_error", "influence")
      if (!is.list(art) || any(vapply(needed, function(nm)
        is.null(art[[nm]]), logical(1L)))) return(empty)
      selected <- as.numeric(selected_by_target[[support[s]]])
      selected_at <- match(selected, frozen_grid)
      if (is.na(selected_at)) return(empty)
      neighbor_at <- unique(c(
        1L, selected_at,
        if (selected_at > 1L) selected_at - 1L else integer(),
        if (selected_at < length(frozen_grid)) selected_at + 1L else
          integer()))
      expected_nu <- frozen_grid[sort(neighbor_at)]
      if (anyDuplicated(art$nu) || !isTRUE(all.equal(
          sort(as.numeric(art$nu)), expected_nu,
          tolerance = 0, check.attributes = FALSE))) return(empty)
      alt_if <- as.matrix(art$influence)
      if (nrow(alt_if) != nrow(x$primitive_influence) ||
          ncol(alt_if) != length(art$nu) ||
          length(art$raw_one_step) != length(art$nu) ||
          length(art$standard_error) != length(art$nu) ||
          any(!is.finite(alt_if)) || any(!is.finite(art$raw_one_step))) {
        return(empty)
      }
      affected <- which(abs(J[, s]) > sqrt(.Machine$double.eps))
      if (!length(affected)) next
      for (r in seq_along(art$nu)) {
        alt_support <- x$supporting_raw_one_step
        alt_support[s] <- art$raw_one_step[r]
        alt_primitive <- as.numeric(x$crossfit_primitive +
          J %*% (alt_support - x$supporting_crossfit))
        names(alt_primitive) <- primitive_ids
        support_if <- as.matrix(inference$influence[, support, drop = FALSE])
        support_if[, s] <- alt_if[, r]
        primitive_if <- support_if %*% t(J)
        colnames(primitive_if) <- primitive_ids
        rownames(primitive_if) <- rownames(support_if)
        primitive_se <- apply(primitive_if, 2L, stats::sd) /
          sqrt(nrow(primitive_if))
        alt_crit <- recompute_critical(
          primitive_if, primitive_ids[affected])
        rr <- if (length(primitive_ids) == 1L) {
          .scmix_v22_chart_update(
            as.numeric(x$crossfit_primitive), alt_primitive,
            x$spec$target_domain)
        } else {
          .scmix_v22_composite_chart(
            x$spec, x$crossfit_primitive, alt_primitive)
        }
        reported_primitive <- if (length(primitive_ids) == 1L) {
          stats::setNames(rr$value, primitive_ids)
        } else stats::setNames(rr$primitive, primitive_ids)
        alt_target <- if (!is.null(rr$value)) rr$value else rr$target
        alt_map <- if (is.finite(alt_crit) &&
                           all(is.finite(reported_primitive)) &&
                           all(is.finite(primitive_se))) {
          .scmix_v22_map_rectangle(
            x$spec$target_type,
            reported_primitive - alt_crit * primitive_se,
            reported_primitive + alt_crit * primitive_se)
        } else list(nonempty = FALSE, lower = NA_real_, upper = NA_real_)
        point_change <- if (is.finite(alt_target) &&
                            is.finite(x$reported_estimate)) {
          abs(alt_target - x$reported_estimate) / x$target_se
        } else Inf
        endpoint_change <- if (isTRUE(base_map$nonempty) &&
          isTRUE(alt_map$nonempty) &&
          all(is.finite(c(base_map$lower, base_map$upper,
                          alt_map$lower, alt_map$upper)))) {
          max(abs(c(alt_map$lower - base_map$lower,
                    alt_map$upper - base_map$upper))) / x$target_se
        } else Inf
        rows[[length(rows) + 1L]] <- data.frame(
          fold = k, supporting_name = support[s], nu = art$nu[r],
          affected_primitive_ids = paste(primitive_ids[affected],
                                         collapse = ","),
          reported_estimate = as.numeric(alt_target),
          critical_value = alt_crit,
          lower = alt_map$lower, upper = alt_map$upper,
          point_change_over_se = point_change,
          endpoint_change_over_se = endpoint_change,
          maximum_change_over_se = max(point_change, endpoint_change),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(rows)) return(empty)
  evaluations <- do.call(rbind, rows)
  maximum <- vapply(primitive_ids, function(id) {
    use <- vapply(strsplit(evaluations$affected_primitive_ids, ",",
                           fixed = TRUE), function(x) id %in% x, logical(1L))
    if (!any(use)) NA_real_ else max(evaluations$maximum_change_over_se[use])
  }, numeric(1L))
  list(available = all(is.finite(maximum)),
       maximum_change_over_se = maximum,
       evaluations = evaluations,
       reason = if (all(is.finite(maximum))) "passed artifact validation" else
         "one or more primitive lacks a finite full-neighborhood diagnostic")
}

.scmix_v22_riesz_gates <- function(
    x, inference, calibration, family_result, level,
    multiplier_bank = NULL, analysis_contract = NULL) {
  evidence_ok <- identical(
    inference$v22_riesz_status,
    "signed_inner_nuisance_refit_evidence_used") &&
    isTRUE(inference$v22_riesz_contract_linked) &&
    is.character(inference$v22_riesz_evidence_fingerprint) &&
    length(inference$v22_riesz_evidence_fingerprint) == 1L &&
    !is.na(inference$v22_riesz_evidence_fingerprint) &&
    nzchar(inference$v22_riesz_evidence_fingerprint)
  honest <- .scmix_v22_honest_riesz(
    x, inference, calibration$numerical_rank_tolerance)
  null_cut <- if (!is.null(calibration$null_space_materiality_cutoff)) {
    calibration$null_space_materiality_cutoff
  } else "CALIBRATE_IN_STEP_3"
  defect_cut <- if (!is.null(calibration$defect_cutoff)) {
    calibration$defect_cutoff
  } else "CALIBRATE_IN_STEP_3"
  ridge_cut <- if (!is.null(calibration$ridge_cutoff)) {
    calibration$ridge_cutoff
  } else "CALIBRATE_IN_STEP_3"
  derivative_cut <- if (!is.null(calibration$derivative_cutoff)) {
    calibration$derivative_cutoff
  } else "CALIBRATE_IN_STEP_3"
  ridge <- .scmix_v22_ridge_neighborhood(
    x, inference, family_result, level, multiplier_bank, analysis_contract)
  rows <- list(scmix_v22_gate_record(
    "N002", "nuisance_verification", "analysis", "hard_fit",
    if (evidence_ok) "pass" else "fail", evidence_ok, TRUE, "equal",
    "boolean", "signed v2.2 Riesz evidence",
    if (evidence_ok) {
      "Outcome-dependent inner-fold nuisances were refit under the signed evidence record."
    } else {
      "Signed evidence of inner-training nuisance refits is absent."
    }, inference$analysis_signature))
  for (j in seq_along(x$spec$primitive_registry_ids)) {
    id <- x$spec$primitive_registry_ids[j]
    active_riesz <- which(abs(x$riesz_jacobian[j, ]) >
                            sqrt(.Machine$double.eps))
    derivative_names <- x$riesz_names[active_riesz]
    audit <- inference$v22_derivative_audit
    derivative_exact <- .scmix_v22_intact(
      audit, "scmix_v22_derivative_audit") &&
      identical(audit$analysis_signature, inference$analysis_signature) &&
      id %in% colnames(audit$metric) &&
      identical(audit$derivative_step_rule,
        "machine_epsilon^(1/3) * max(1, abs(normalized_coordinate))") &&
      identical(audit$refinement_rule,
                "centered structural-orthonormal steps h and h/2")
    derivative_value <- if (derivative_exact) {
      values <- audit$metric[, id]
      if (all(is.finite(values))) max(values) else NA_real_
    } else NA_real_
    derivative_verified <- if (
      derivative_exact && is.numeric(derivative_cut) &&
      length(derivative_cut) == 1L && is.finite(derivative_cut) &&
      is.finite(derivative_value)) {
      derivative_value <= derivative_cut
    } else NA
    derivative_state <- .scmix_v22_gate_state(
      derivative_verified, TRUE, "equal")
    rows[[length(rows) + 1L]] <- scmix_v22_gate_record(
      "R001", "inverse_problem", "primitive", "hard_target",
      derivative_state,
      derivative_verified, TRUE, "equal", "boolean",
      if (derivative_exact) audit$provenance else
        "frozen centered h and h/2 derivative artifact absent",
      if (derivative_exact) {
        "The analytic or numerical primitive derivative must pass the frozen structural-coordinate refinement calculation."
      } else {
        "Only legacy raw-coordinate h/h2 evidence is available. A signed exact structural-orthonormal perturbation artifact remains required."
      },
      id)
    null_value <- if (honest$available) honest$maximum_null[j] else NA_real_
    defect_value <- if (honest$available) honest$maximum_defect[j] else NA_real_
    rows[[length(rows) + 1L]] <- scmix_v22_gate_record(
      "R002", "inverse_problem", "primitive", "hard_target",
      .scmix_v22_gate_state(null_value, null_cut, "less_than_or_equal"),
      null_value, null_cut, "less_than_or_equal", "relative_gradient_norm",
      "untouched outer-fold information-range audit",
      "The target derivative must lie in the held-out information range.", id)
    rows[[length(rows) + 1L]] <- scmix_v22_gate_record(
      "R003", "inverse_problem", "primitive", "hard_target",
      .scmix_v22_gate_state(defect_value, defect_cut, "less_than_or_equal"),
      defect_value, defect_cut, "less_than_or_equal",
      "influence_standard_deviations",
      "untouched outer-fold information-scaled Riesz audit",
      "The honest information-scaled Riesz defect must pass its calibrated cutoff.",
      id)
    ridge_value <- ridge$maximum_change_over_se[[id]]
    rows[[length(rows) + 1L]] <- scmix_v22_gate_record(
      "R004", "inverse_problem", "primitive", "hard_target",
      .scmix_v22_gate_state(ridge_value, ridge_cut, "less_than_or_equal"),
      ridge_value, ridge_cut, "less_than_or_equal", "standard_errors",
      "full selected-ridge neighborhood target-and-endpoint mapping",
      paste("Zero and adjacent ridge values must not materially change the",
            "reported target or mapped interval endpoints."), id)
    selected <- vapply(inference$fold_details, function(fd) {
      z <- fd$selected_nu_by_target
      if (is.null(z) || is.null(names(z)) ||
          !all(derivative_names %in% names(z))) NA_real_ else
        max(as.numeric(z[derivative_names]))
    }, numeric(1L))
    positive <- any(selected > 0, na.rm = TRUE)
    sequence_ok <- evidence_ok &&
      is.character(inference$v22_ridge_sequence_provenance) &&
      length(inference$v22_ridge_sequence_provenance) == 1L &&
      !is.na(inference$v22_ridge_sequence_provenance) &&
      nzchar(trimws(inference$v22_ridge_sequence_provenance))
    rows[[length(rows) + 1L]] <- scmix_v22_gate_record(
      "R005", "inverse_problem", "primitive", "hard_target",
      if (!positive) "not_applicable" else if (sequence_ok) "pass" else "fail",
      if (positive) sequence_ok else FALSE, TRUE, "equal", "boolean",
      "signed v2.2 inner-refit and vanishing-ridge artifact",
      if (!positive) {
        "Not applicable because zero ridge was selected in every outer fold."
      } else if (sequence_ok) {
        "Every selected positive ridge belongs to the signed vanishing fold-size sequence."
      } else {
        "A positive selected ridge lacks signed vanishing-sequence provenance."
      }, id)
  }
  list(gates = do.call(.scmix_v22_bind_gates, rows),
       diagnostics = honest, ridge_neighborhood = ridge)
}

.scmix_v22_local_gates <- function(x, mapped, family_result, calibration) {
  tid <- x$spec$target_id; fid <- x$spec$target_family_id
  threshold <- function(name) if (!is.null(calibration[[name]]))
    calibration[[name]] else "CALIBRATE_IN_STEP_3"
  composite <- x$spec$target_type %in% c(
    "subgroup_share", "directional_heterogeneity",
    "mean_tradeoff_mrs_ratio")
  chart_applicable <- composite ||
    !identical(x$spec$target_domain$chart, "identity")
  chart_cut <- if (composite) threshold("nonlinear_remainder_cutoff") else
    threshold("chart_discrepancy_cutoff")
  boundary_cut <- threshold("interior_distance_cutoff")
  chart_statistic <- if (composite) x$nonlinear_remainder_se_ratio else
    x$chart_discrepancy_se_ratio
  chart_state <- if (chart_applicable) .scmix_v22_gate_state(
    chart_statistic, chart_cut, "less_than_or_equal") else "not_applicable"
  boundary_applicable <- x$spec$target_type != "unrestricted_scalar"
  boundary_state <- if (boundary_applicable) .scmix_v22_gate_state(
    x$plugin_boundary_distance, boundary_cut, "greater_than_or_equal") else
      "not_applicable"
  constraint_applicable <- x$spec$target_type != "unrestricted_scalar"
  raw_out <- if (x$spec$target_type %in% c(
    "probability", "preference_direction_share",
    "compensating_benefit_share")) {
    !.scmix_v22_target_in_support(x$raw_one_step,
                                  x$spec$target_domain)
  } else x$raw_domain_distance > 0
  m3_applicable <- length(family_result$ids) > 1L ||
    length(family_result$conflict) > 0L
  membership_ok <- isTRUE(family_result$membership_complete) &&
    isTRUE(family_result$declaration_consistent) &&
    !length(family_result$conflict)
  multiplier_ok <- identical(family_result$method, "common_draw_max_t") &&
    isTRUE(family_result$final_draw_count)
  m1 <- lapply(seq_along(x$spec$primitive_registry_ids), function(j) {
    primitive_ok <- is.finite(x$primitive_se[j]) && x$primitive_se[j] > 0
    scmix_v22_gate_record(
      "M001", "resampling_multiplicity", "primitive", "hard_target",
      if (primitive_ok) "pass" else "fail", primitive_ok, TRUE, "equal",
      "boolean", "v2.2 primitive influence matrix",
      "Each inferential primitive requires a finite positive respondent-level standard error.",
      x$spec$primitive_registry_ids[j])
  })
  .scmix_v22_bind_gates(
    scmix_v22_gate_record(
      "C001", "target_constraint", "target", "hard_target",
      if (!chart_applicable) "not_applicable" else if (
        isTRUE(x$chart_interior)) "pass" else "fail",
      x$chart_interior, TRUE, "equal", "boolean", "v2.2 target finalizer",
      if (!chart_applicable) "Not applicable to an identity-chart scalar."
      else "The cross-fit plug-in must be in the declared relative interior.", tid),
    scmix_v22_gate_record(
      "C002", "target_constraint", "target", "hard_target",
      if (!constraint_applicable) "not_applicable" else if (
        isTRUE(mapped$nonempty) && !is.na(mapped$lower)) "pass" else "fail",
      isTRUE(mapped$nonempty) && !is.na(mapped$lower), TRUE, "equal",
      "boolean", "v2.2 confidence mapper",
      if (!constraint_applicable) "Not applicable to an unrestricted scalar."
      else mapped$reason, tid),
    scmix_v22_gate_record(
      "C003", "target_constraint", "target", "hard_target", chart_state,
      chart_statistic, chart_cut, "less_than_or_equal",
      "standard_errors", "v2.2 target finalizer",
      if (!chart_applicable) "Not applicable to an identity-chart scalar."
      else if (composite) {
        "The nonlinear remainder must pass its calibrated cutoff."
      } else "The chart discrepancy must pass its calibrated cutoff.", tid),
    scmix_v22_gate_record(
      "C004", "target_constraint", "target", "warning",
      if (isTRUE(raw_out)) "warn" else "pass", raw_out, FALSE, "equal",
      "boolean", "v2.2 target finalizer",
      if (isTRUE(raw_out)) "The raw one-step point is outside known support."
      else "The raw one-step point is compatible with known support.", tid),
    scmix_v22_gate_record(
      "G006", "identification_regularity", "target", "hard_target",
      boundary_state, x$plugin_boundary_distance, boundary_cut,
      "greater_than_or_equal", "standard_errors", "v2.2 target finalizer",
      if (!boundary_applicable) "Not applicable to an unrestricted scalar."
      else "The plug-in must pass the calibrated target-boundary margin.", tid),
    do.call(.scmix_v22_bind_gates, m1),
    scmix_v22_gate_record(
      "M002", "resampling_multiplicity", "family", "hard_family",
      if (!m3_applicable) "not_applicable" else if (
        membership_ok) "pass" else "fail",
      membership_ok, TRUE, "equal", "boolean",
      "v2.2 signed family manifest and collected primitive artifacts",
      if (!m3_applicable) "Not applicable to a single-primitive family."
      else if (membership_ok) {
        "The collected primitive IDs equal the frozen signed family membership."
      } else family_result$reason, fid),
    scmix_v22_gate_record(
      "M003", "resampling_multiplicity", "family", "hard_family",
      if (!m3_applicable) "not_applicable" else if (
        multiplier_ok) "pass" else
          "fail",
      multiplier_ok, TRUE, "equal",
      "boolean", "v2.2 multiplier bank",
      if (!m3_applicable) "Not applicable to a single-primitive family."
      else if (!isTRUE(family_result$final_draw_count)) {
        paste("Final simultaneous inference requires exactly 9,999 common",
              "respondent multiplier draws.")
      } else family_result$reason, fid)
  )
}

#' Collect one independently computed v2.2 target job
#'
#' @param inference A computed `scmix_dml` result containing every registered
#'   supporting moment.
#' @param registry List of [scmix_v22_target_spec()] objects.
#' @param full_sample_plugin Optional named supporting-moment vector calculated
#'   from the exact full-sample structural fit.
#' @return A portable target-job collection. It has no familywise critical
#'   value and is not a final inference result.
#' @export
scmix_v22_collect <- function(inference, registry, full_sample_plugin = NULL,
                              analysis_contract =
                                inference$v22_analysis_contract,
                              derivative_audit =
                                inference$v22_derivative_audit) {
  if (!inherits(inference, "scmix_dml") || is.null(inference$influence)) {
    stop("inference must be a computed scmix_dml result.", call. = FALSE)
  }
  registry <- .scmix_v22_registry_payload(registry)
  ids <- vapply(registry, `[[`, character(1L), "target_id")
  if (anyDuplicated(ids)) stop("target_id must be unique in a job registry.",
                               call. = FALSE)
  contract_linked <- .scmix_v22_intact(
    analysis_contract, "scmix_v22_analysis_contract") &&
    identical(analysis_contract$analysis_signature,
              inference$analysis_signature)
  required_primitive_ids <- sort(unique(unlist(lapply(
    registry, `[[`, "primitive_registry_ids"))), method = "radix")
  scalar_identity <- all(vapply(registry, function(spec)
    length(spec$primitive_registry_ids) == 1L &&
      length(spec$supporting_names) == 1L, logical(1L)))
  minimal_primitive_geometry <- isTRUE(inference$v22_primitive_mode) ||
    scalar_identity
  required_evidence_ids <- if (isTRUE(inference$v22_primitive_mode)) {
    required_primitive_ids
  } else {
    sort(unique(unlist(lapply(registry, `[[`, "supporting_names"))),
         method = "radix")
  }
  riesz_evidence <- inference$v22_riesz_evidence
  riesz_evidence_linked <- contract_linked && minimal_primitive_geometry &&
    .scmix_v22_validate_riesz_evidence(
      riesz_evidence, inference, analysis_contract,
      required_primitive_ids = required_evidence_ids) &&
    is.character(inference$v22_riesz_evidence_fingerprint) &&
    length(inference$v22_riesz_evidence_fingerprint) == 1L &&
    !is.na(inference$v22_riesz_evidence_fingerprint) &&
    identical(inference$v22_riesz_evidence_fingerprint,
              riesz_evidence$fingerprint) &&
    identical(inference$v22_riesz_analysis_contract_fingerprint,
              analysis_contract$fingerprint)
  inference$v22_riesz_contract_linked <- riesz_evidence_linked
  audit_linked <- .scmix_v22_intact(
    derivative_audit, "scmix_v22_derivative_audit") &&
    contract_linked &&
    identical(derivative_audit$analysis_signature,
              inference$analysis_signature) &&
    identical(derivative_audit$analysis_contract_fingerprint,
              analysis_contract$fingerprint) &&
    identical(derivative_audit$code_hash, analysis_contract$code_commit) &&
    identical(derivative_audit$coordinate_names,
              analysis_contract$derivative_coordinate_registry) &&
    identical(derivative_audit$coordinate_subset,
              analysis_contract$derivative_coordinate_subset) &&
    identical(derivative_audit$subset_rule,
              analysis_contract$derivative_subset_rule) &&
    identical(names(derivative_audit$fold_artifacts),
              .scmix_v22_contract_fold_ids(analysis_contract)) &&
    all(unique(unlist(lapply(registry, `[[`, "primitive_registry_ids"))) %in%
          derivative_audit$primitive_registry_ids)
  inference$v22_derivative_audit <- if (audit_linked) derivative_audit else NULL
  plugin_artifact <- NULL
  plugin_linked <- FALSE
  if (inherits(full_sample_plugin, "scmix_v22_full_plugin_artifact")) {
    plugin_artifact <- full_sample_plugin
    plugin_linked <- .scmix_v22_intact(
      plugin_artifact, "scmix_v22_full_plugin_artifact") &&
      contract_linked &&
      identical(plugin_artifact$analysis_signature,
                inference$analysis_signature) &&
      identical(plugin_artifact$analysis_contract_fingerprint,
                analysis_contract$fingerprint) &&
      identical(plugin_artifact$code_hash, analysis_contract$code_commit)
    full_sample_plugin <- plugin_artifact$values
  }
  if (!is.null(full_sample_plugin)) {
    if (!is.numeric(full_sample_plugin) || is.null(names(full_sample_plugin)) ||
        anyDuplicated(names(full_sample_plugin)) ||
        any(!is.finite(full_sample_plugin))) {
      stop("full_sample_plugin must be a finite uniquely named numeric vector.",
           call. = FALSE)
    }
  }
  prepared <- stats::setNames(lapply(registry, function(spec) {
    x <- .scmix_v22_prepare(inference, spec, full_sample_plugin)
    x$source_inference <- inference
    x
  }), ids)
  plugin_linked <- plugin_linked && all(vapply(prepared, function(x)
    is.finite(x$full_sample_plugin) &&
      all(is.finite(x$full_sample_primitive)), logical(1L)))
  out <- list(
    protocol_id = .scmix_v22_protocol_id,
    targets = prepared,
    registry = registry,
    analysis_signature = inference$analysis_signature,
    analysis_contract = analysis_contract,
    analysis_contract_linked = contract_linked,
    full_plugin_artifact = plugin_artifact,
    full_plugin_linked = plugin_linked,
    derivative_audit = derivative_audit,
    derivative_audit_linked = audit_linked,
    riesz_evidence = riesz_evidence,
    riesz_evidence_linked = riesz_evidence_linked,
    minimal_primitive_geometry = minimal_primitive_geometry,
    respondent_id = prepared[[1L]]$respondent_id,
    source_inference = inference,
    source_inference_fingerprint =
      .scmix_v22_source_inference_fingerprint(inference),
    final_inference_status = "not_finalized_no_family_critical")
  target_payload <- lapply(prepared, function(x) list(
    spec_fingerprint = x$spec$fingerprint,
    respondent_id = x$respondent_id,
    crossfit_primitive = x$crossfit_primitive,
    raw_one_step_primitive = x$raw_one_step_primitive,
    reported_primitive = x$reported_primitive,
    primitive_influence = x$primitive_influence))
  out$fingerprint <- .scmix_v22_fingerprint(list(
    protocol_id = out$protocol_id, target_ids = ids,
    family_ids = vapply(registry, `[[`, character(1L), "target_family_id"),
    primitive_ids = lapply(registry, `[[`, "primitive_registry_ids"),
    family_primitive_ids = lapply(
      registry, `[[`, "family_primitive_registry_ids"),
    minimal_primitive_geometry = out$minimal_primitive_geometry,
    analysis_signature = out$analysis_signature,
    respondent_id = sort(as.character(out$respondent_id), method = "radix"),
    analysis_contract_fingerprint = if (contract_linked)
      analysis_contract$fingerprint else NA_character_,
    full_plugin_fingerprint = if (plugin_linked)
      plugin_artifact$fingerprint else NA_character_,
    derivative_audit_fingerprint = if (audit_linked)
      derivative_audit$fingerprint else NA_character_,
    riesz_evidence_fingerprint = if (riesz_evidence_linked)
      riesz_evidence$fingerprint else NA_character_,
    source_inference_fingerprint = out$source_inference_fingerprint,
    target_payload = target_payload))
  class(out) <- c("scmix_v22_job_collection", "list")
  out
}

.scmix_v22_source_inference_fingerprint <- function(inference) {
  .scmix_v22_fingerprint(list(
    analysis_signature = inference$analysis_signature,
    respondent_id = inference$respondent_id,
    crossfit_plugin = inference$crossfit_plugin,
    raw_one_step = inference$raw_one_step,
    influence = inference$influence,
    fold_details = inference$fold_details,
    v22_primitive_mode = inference$v22_primitive_mode,
    v22_riesz_status = inference$v22_riesz_status,
    v22_riesz_evidence_fingerprint =
      inference$v22_riesz_evidence_fingerprint,
    v22_riesz_evidence = inference$v22_riesz_evidence,
    v22_riesz_analysis_contract_fingerprint =
      inference$v22_riesz_analysis_contract_fingerprint,
    v22_ridge_sequence_provenance =
      inference$v22_ridge_sequence_provenance,
    v22_riesz_contract_linked = inference$v22_riesz_contract_linked,
    v22_derivative_audit = inference$v22_derivative_audit))
}

.scmix_v22_collection_fingerprint <- function(x) {
  target_payload <- lapply(x$targets, function(z) list(
    spec_fingerprint = z$spec$fingerprint,
    respondent_id = z$respondent_id,
    crossfit_primitive = z$crossfit_primitive,
    raw_one_step_primitive = z$raw_one_step_primitive,
    reported_primitive = z$reported_primitive,
    primitive_influence = z$primitive_influence))
  .scmix_v22_fingerprint(list(
    protocol_id = x$protocol_id,
    target_ids = vapply(x$registry, `[[`, character(1L), "target_id"),
    family_ids = vapply(x$registry, `[[`, character(1L), "target_family_id"),
    primitive_ids = lapply(x$registry, `[[`, "primitive_registry_ids"),
    family_primitive_ids = lapply(
      x$registry, `[[`, "family_primitive_registry_ids"),
    minimal_primitive_geometry = x$minimal_primitive_geometry,
    analysis_signature = x$analysis_signature,
    respondent_id = sort(as.character(x$respondent_id), method = "radix"),
    analysis_contract_fingerprint = if (isTRUE(x$analysis_contract_linked))
      x$analysis_contract$fingerprint else NA_character_,
    full_plugin_fingerprint = if (isTRUE(x$full_plugin_linked))
      x$full_plugin_artifact$fingerprint else NA_character_,
    derivative_audit_fingerprint = if (isTRUE(x$derivative_audit_linked))
      x$derivative_audit$fingerprint else NA_character_,
    riesz_evidence_fingerprint = if (isTRUE(x$riesz_evidence_linked))
      x$riesz_evidence$fingerprint else NA_character_,
    source_inference_fingerprint =
      .scmix_v22_source_inference_fingerprint(x$source_inference),
    target_payload = target_payload))
}

#' Finalize one or more independently computed v2.2 target jobs
#'
#' All job artifacts are collected before any familywise critical value is
#' calculated. Thus splitting a frozen target family across workers cannot
#' turn the family max-t calculation into separate targetwise calculations.
#'
#' @param collections Nonempty list returned by [scmix_v22_collect()].
#' @param multiplier_bank One reusable bank from
#'   [scmix_v22_multiplier_bank()]. It is required for a multi-primitive family.
#' @param calibration Intact [scmix_v22_calibration_artifact()]. A plain list,
#'   missing artifact, or contract mismatch leaves calibrated gates `not_run`
#'   and therefore fails closed.
#' @param external_gates Complete signed gate rows from the fitting,
#'   identification, inverse-problem, and specification-assessment pipeline.
#'   F007, F008, and I001--I005 are excluded and accepted only through
#'   `computation_artifacts`.
#' @param computation_artifacts Optional intact typed computation artifacts.
#'   Supply at most one F007, one F008, and one integration-refinement
#'   artifact. Missing applicable evidence remains `not_run` and fails closed.
#' @param complete_external_record Set `TRUE` only after that record has been
#'   validated against the signed registry and protocol hash.
#' @param level Confidence level.
#' @return A `scmix_v22_inference` object.
#' @export
scmix_v22_finalize_collections <- function(
    collections,
    multiplier_bank = NULL, calibration = NULL, external_gates = NULL,
    computation_artifacts = NULL,
    complete_external_record = FALSE, level = 0.95) {
  if (!is.list(collections) || !length(collections) ||
      !all(vapply(collections, inherits, logical(1L),
                  what = "scmix_v22_job_collection"))) {
    stop("collections must be a nonempty list of scmix_v22_collect() artifacts.",
         call. = FALSE)
  }
  if (any(vapply(collections, function(x)
    !identical(x$protocol_id, .scmix_v22_protocol_id), logical(1L)))) {
    stop("Every collection must use the frozen v2.2 protocol ID.",
         call. = FALSE)
  }
  collection_intact <- vapply(collections, function(x)
    is.character(x$fingerprint) && length(x$fingerprint) == 1L &&
      !is.na(x$fingerprint) &&
      identical(x$fingerprint, .scmix_v22_collection_fingerprint(x)) &&
      (!isTRUE(x$analysis_contract_linked) || .scmix_v22_intact(
        x$analysis_contract, "scmix_v22_analysis_contract")) &&
      (!isTRUE(x$full_plugin_linked) || .scmix_v22_intact(
        x$full_plugin_artifact, "scmix_v22_full_plugin_artifact")) &&
      (!isTRUE(x$derivative_audit_linked) || .scmix_v22_intact(
        x$derivative_audit, "scmix_v22_derivative_audit")) &&
      (!isTRUE(x$riesz_evidence_linked) || .scmix_v22_intact(
        x$riesz_evidence, "scmix_v22_riesz_evidence")),
    logical(1L))
  if (!all(collection_intact)) {
    stop("One or more target-job collections failed fingerprint validation.",
         call. = FALSE)
  }
  prepared <- unlist(lapply(collections, `[[`, "targets"), recursive = FALSE)
  ids <- vapply(prepared, function(x) x$spec$target_id, character(1L))
  if (anyDuplicated(ids)) stop("target_id must be unique across collected jobs.",
                               call. = FALSE)
  names(prepared) <- ids
  respondent_sets_match <- all(vapply(prepared, function(x)
    setequal(x$respondent_id, prepared[[1L]]$respondent_id), logical(1L)))
  if (!respondent_sets_match) {
    stop("All collected target jobs must cover the same respondents.",
         call. = FALSE)
  }
  signatures <- vapply(collections, function(x) {
    sig <- x$analysis_signature
    if (is.null(sig) || length(sig) != 1L) NA_character_ else as.character(sig)
  }, character(1L))
  if (anyNA(signatures) || any(!nzchar(signatures)) ||
      length(unique(signatures)) != 1L) {
    stop("All collected target jobs must share one nonempty analysis signature.",
         call. = FALSE)
  }
  collected_registry <- lapply(prepared, `[[`, "spec")
  collected_registry <- .scmix_v22_registry_payload(collected_registry)
  contract_linked <- vapply(collections, function(x)
    isTRUE(x$analysis_contract_linked) &&
      .scmix_v22_intact(x$analysis_contract,
                        "scmix_v22_analysis_contract"), logical(1L))
  contract_fingerprints <- vapply(collections, function(x) {
    if (!isTRUE(x$analysis_contract_linked)) NA_character_ else
      x$analysis_contract$fingerprint
  }, character(1L))
  if (any(!is.na(contract_fingerprints)) &&
      length(unique(stats::na.omit(contract_fingerprints))) != 1L) {
    stop("All target jobs must share the same signed folds, data, design, preprocessing, and seed contract.",
         call. = FALSE)
  }
  common_contract <- if (all(contract_linked))
    collections[[1L]]$analysis_contract else NULL
  calibration_linked <- .scmix_v22_validate_calibration(
    calibration, common_contract, require_complete = TRUE)
  calibration_values <- if (calibration_linked) calibration$constants else
    list()
  full_plugins_linked <- all(vapply(collections, function(x)
    isTRUE(x$full_plugin_linked) &&
      .scmix_v22_intact(x$full_plugin_artifact,
                        "scmix_v22_full_plugin_artifact"), logical(1L)))
  riesz_evidence_linked <- all(vapply(collections, function(x) {
    required <- if (isTRUE(x$source_inference$v22_primitive_mode)) {
      sort(unique(unlist(lapply(
        x$registry, `[[`, "primitive_registry_ids"))), method = "radix")
    } else {
      sort(unique(unlist(lapply(
        x$registry, `[[`, "supporting_names"))), method = "radix")
    }
    isTRUE(x$minimal_primitive_geometry) &&
      isTRUE(x$riesz_evidence_linked) &&
      .scmix_v22_validate_riesz_evidence(
        x$riesz_evidence, x$source_inference, x$analysis_contract,
        required_primitive_ids = required)
  }, logical(1L)))
  gate_artifact_ok <- .scmix_v22_intact(
    external_gates, "scmix_v22_gate_artifact")
  signed_registry <- if (gate_artifact_ok) {
    try(.scmix_v22_registry_payload(external_gates$registry), silent = TRUE)
  } else collected_registry
  if (inherits(signed_registry, "try-error")) {
    gate_artifact_ok <- FALSE
    signed_registry <- collected_registry
  }
  if (gate_artifact_ok) {
    collected_ids <- sort(unname(vapply(
      collected_registry, `[[`, character(1L), "target_id")),
      method = "radix")
    signed_ids <- sort(unname(vapply(
      signed_registry, `[[`, character(1L), "target_id")),
      method = "radix")
    collected_match <- identical(collected_ids, signed_ids) &&
      all(vapply(collected_registry, function(spec) {
      j <- match(spec$target_id, vapply(
        signed_registry, `[[`, character(1L), "target_id"))
      !is.na(j) && identical(spec$fingerprint,
                             signed_registry[[j]]$fingerprint)
    }, logical(1L)))
    gate_artifact_ok <- all(contract_linked) &&
      collected_match &&
      identical(external_gates$analysis_signature, signatures[[1L]]) &&
      identical(external_gates$analysis_contract_fingerprint,
                common_contract$fingerprint) &&
      identical(external_gates$registry_fingerprint,
                .scmix_v22_fingerprint(signed_registry))
  }
  external_gate_rows <- if (inherits(external_gates,
                                      "scmix_v22_gate_artifact")) {
    external_gates$gates
  } else external_gates
  protected_external <- if (is.data.frame(external_gate_rows) &&
      "gate_id" %in% names(external_gate_rows)) {
    intersect(unique(external_gate_rows$gate_id),
              .scmix_v22_computation_gate_ids)
  } else character()
  if (length(protected_external)) {
    stop("Generic external_gates may not supply typed computation gate(s): ",
         paste(protected_external, collapse = ", "),
         ". Use computation_artifacts.", call. = FALSE)
  }
  if (is.null(common_contract)) {
    if (!is.null(computation_artifacts)) {
      stop("Typed computation artifacts require one intact common analysis contract.",
           call. = FALSE)
    }
    computation <- list(rows = NULL, applicability = NULL,
                        fingerprints = character())
  } else {
    computation <- .scmix_v22_computation_gate_rows(
      computation_artifacts, common_contract, signed_registry, calibration)
  }
  family_target_complete <- all(vapply(split(
    signed_registry, vapply(signed_registry, `[[`, character(1L),
                                             "target_family_id")),
    function(family_specs) {
      declared <- lapply(family_specs, `[[`, "family_target_ids")
      consistent <- all(vapply(declared, function(x)
        identical(x, declared[[1L]]), logical(1L)))
      observed <- sort(vapply(family_specs, `[[`, character(1L),
                              "target_id"), method = "radix")
      consistent && identical(observed, declared[[1L]])
    }, logical(1L)))
  registry_complete <- family_target_complete && all(vapply(
    signed_registry, function(x)
    .scmix_v22_intact(x, "scmix_v22_target_spec"), logical(1L))) &&
    all(contract_linked) && gate_artifact_ok
  final_fit_hashes <- vapply(collections, function(x) {
    if (!isTRUE(x$full_plugin_linked)) NA_character_ else
      x$full_plugin_artifact$final_fit_hash
  }, character(1L))
  one_final_fit <- all(!is.na(final_fit_hashes)) &&
    length(unique(final_fit_hashes)) == 1L
  hashes_match <- registry_complete && full_plugins_linked && one_final_fit &&
    riesz_evidence_linked && calibration_linked && all(collection_intact)
  integrity_gates <- .scmix_v22_bind_gates(
    scmix_v22_gate_record(
      "P001", "protocol_integrity", "analysis", "hard_fit",
      if (registry_complete) "pass" else "fail", registry_complete, TRUE,
      "equal", "boolean", "signed v2.2 registry and analysis contract",
      if (registry_complete) {
        "The complete target registry and application-wide analysis contract validate."
      } else {
        "The complete signed registry, analysis contract, or scoped gate artifact is absent or mismatched."
      }, signatures[[1L]]),
    scmix_v22_gate_record(
      "P002", "protocol_integrity", "analysis", "hard_fit",
      if (hashes_match) "pass" else "fail", hashes_match, TRUE,
      "equal", "boolean",
      "job, full-fit plug-in, Riesz, calibration, and gate fingerprints",
      if (hashes_match) {
        "Protocol, code, data, design, fit, plug-in, and gate artifacts are linked by intact fingerprints."
      } else {
        "One or more required artifact hash, signed full-sample fit plug-in, nested Riesz, or calibration link is absent or mismatched."
      }, signatures[[1L]]))
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) stop("level must lie in (0,1).", call. = FALSE)
  if (!is.null(multiplier_bank)) {
    invisible(.scmix_v22_validate_bank(
      multiplier_bank, prepared[[1L]]$respondent_id, common_contract))
  }
  families <- split(prepared, vapply(prepared, function(x)
    x$spec$target_family_id, character(1L)))
  family_results <- lapply(families, .scmix_v22_family_critical,
                           bank = multiplier_bank, level = level,
                           analysis_contract = common_contract)
  ## Family completeness is not a numerical-influence check alone. A common
  ## max-t label also requires every frozen primitive to pass its own v2.2
  ## inverse-problem and standard-error gates. Run a first diagnostic pass,
  ## then demote an otherwise computable max-t result to the original-family
  ## Bonferroni fallback when another primitive is ineligible. The failed
  ## primitive remains in the diagnostic max-t calculation and is never
  ## silently dropped.
  preliminary_primitive_gates <- lapply(prepared, function(x) {
    fam <- family_results[[x$spec$target_family_id]]
    crit <- fam$critical
    lo <- x$reported_primitive - crit * x$primitive_se
    hi <- x$reported_primitive + crit * x$primitive_se
    mapped <- if (is.finite(crit) && all(is.finite(lo)) &&
                  all(is.finite(hi))) {
      .scmix_v22_map_rectangle(x$spec$target_type, lo, hi)
    } else list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
                boundary_touch = NA, uninformative = FALSE,
                reason = fam$reason)
    local <- .scmix_v22_local_gates(
      x, mapped, fam, calibration_values)
    riesz <- .scmix_v22_riesz_gates(
      x, x$source_inference, calibration_values, fam, level,
      multiplier_bank, common_contract)$gates
    .scmix_v22_bind_gates(local, riesz)
  })
  for (family_id in names(family_results)) {
    fam <- family_results[[family_id]]
    member_targets <- which(vapply(prepared, function(x)
      identical(x$spec$target_family_id, family_id), logical(1L)))
    g <- do.call(.scmix_v22_bind_gates,
                 preliminary_primitive_gates[member_targets])
    required_primitive_gates <- c("R001", "R002", "R003", "R004",
                                  "R005", "M001")
    primitive_pass <- vapply(fam$ids, function(id) {
      rows <- g$scope == "primitive" & g$scope_id == id &
        g$gate_id %in% required_primitive_gates
      present <- unique(g$gate_id[rows])
      states <- g$state[rows]
      identical(sort(present, method = "radix"),
                sort(required_primitive_gates, method = "radix")) &&
        !any(states %in% c("fail", "not_run"))
    }, logical(1L))
    fam$primitive_gate_pass <- stats::setNames(primitive_pass, fam$ids)
    fam$primitive_gate_complete <- all(primitive_pass)
    if (identical(fam$method, "common_draw_max_t") &&
        !fam$primitive_gate_complete) {
      fam$diagnostic_method <- "common_draw_max_t"
      fam$diagnostic_critical <- fam$critical
      fam$method <- "fixed_family_bonferroni"
      fam$critical <- stats::qnorm(
        1 - (1 - level) / (2 * length(fam$ids)))
      fam$reason <- paste(
        "one or more frozen family primitives failed or lacked complete",
        "target-specific Riesz/standard-error gates; original-family",
        "Bonferroni used")
    }
    family_results[[family_id]] <- fam
  }
  results <- lapply(prepared, function(x) {
    fam <- family_results[[x$spec$target_family_id]]
    crit <- fam$critical
    lo_p <- x$reported_primitive - crit * x$primitive_se
    hi_p <- x$reported_primitive + crit * x$primitive_se
    mapped <- if (is.finite(crit) && all(is.finite(lo_p)) &&
                  all(is.finite(hi_p))) {
      .scmix_v22_map_rectangle(x$spec$target_type, lo_p, hi_p)
    } else list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
                boundary_touch = NA, uninformative = FALSE,
                reason = fam$reason)
    z <- stats::qnorm(1 - (1 - level) / 2)
    point_lo_p <- x$reported_primitive - z * x$primitive_se
    point_hi_p <- x$reported_primitive + z * x$primitive_se
    point <- .scmix_v22_map_rectangle(x$spec$target_type,
                                      point_lo_p, point_hi_p)
    formal_map <- if (identical(fam$method, "none")) point else mapped
    local <- .scmix_v22_local_gates(
      x, formal_map, fam, calibration_values)
    riesz <- .scmix_v22_riesz_gates(
      x, x$source_inference, calibration_values, fam, level,
      multiplier_bank, common_contract)
    all_gates <- .scmix_v22_bind_gates(
      external_gate_rows, computation$rows, integrity_gates, local,
      riesz$gates)
    all_gates <- .scmix_v22_deduplicate_identical_gates(all_gates)
    record_complete <- FALSE
    if (isTRUE(complete_external_record) && gate_artifact_ok &&
        registry_complete && hashes_match) {
      record_complete <- !inherits(try(
        scmix_v22_validate_gate_record(all_gates, require_all_ids = TRUE),
        silent = TRUE), "try-error")
    }
    release <- if (isTRUE(formal_map$boundary_touch)) "boundary_limited" else if (
      x$spec$target_type %in% c("unrestricted_scalar",
                                "mean_tradeoff_mrs_ratio")) {
      "regular_interior"
    } else "constraint_mapped"
    interval_method <- if (fam$method == "fixed_family_bonferroni") {
      "fixed_family_bonferroni"
    } else if (x$spec$target_type %in% c("unrestricted_scalar")) {
      "regular_if"
    } else if (length(x$spec$primitive_registry_ids) == 1L) {
      "support_intersection"
    } else "primitive_set_mapping"
    multiplicity <- switch(fam$method,
                           common_draw_max_t = "simultaneous_complete",
                           fixed_family_bonferroni = "bonferroni_incomplete",
                           pointwise = "pointwise_only", "none")
    status <- scmix_v22_status(
      all_gates, x$spec$target_id, x$spec$target_family_id,
      primitive_ids = x$spec$primitive_registry_ids,
      parameter_dependencies = x$spec$target_parameter_dependencies,
      release_class = release, interval_method = interval_method,
      multiplicity_status = multiplicity,
      complete_external_record = record_complete)
    x$diagnostic_pointwise_interval <- c(lower = point$lower,
                                         upper = point$upper)
    x$diagnostic_family_interval <- c(lower = mapped$lower,
                                      upper = mapped$upper)
    target_available <- identical(status$target_status,
                                  "conditional_available")
    x$pointwise_interval <- if (target_available) {
      x$diagnostic_pointwise_interval
    } else c(lower = NA_real_, upper = NA_real_)
    family_available <- target_available &&
      status$multiplicity_status %in%
        c("simultaneous_complete", "bonferroni_incomplete")
    x$simultaneous_interval <- if (family_available) {
      x$diagnostic_family_interval
    } else c(lower = NA_real_, upper = NA_real_)
    x$confidence_set <- if (target_available) formal_map else
      list(nonempty = FALSE, lower = NA_real_, upper = NA_real_,
           boundary_touch = NA, uninformative = FALSE,
           reason = "formal set withheld by v2.2 gates")
    x$confidence_center <- x$reported_estimate
    x$family_resampling <- fam
    x$honest_riesz <- riesz$diagnostics
    x$ridge_neighborhood <- riesz$ridge_neighborhood
    x$status <- status
    class(x) <- c("scmix_v22_target_result", "list")
    x
  })
  names(results) <- ids
  out <- list(
    protocol_id = .scmix_v22_protocol_id,
    protocol_status = if (isTRUE(complete_external_record) &&
      gate_artifact_ok && registry_complete && hashes_match &&
      calibration_linked) "implemented_with_supplied_calibration" else
        "implementation_complete_calibration_or_gates_pending",
    retained_estimates = c("full_sample_plugin", "crossfit_plugin",
                           "raw_one_step", "reported_estimate"),
    targets = results, family_resampling = family_results,
    registry = signed_registry, external_gates = external_gate_rows,
    computation_gates = computation$rows,
    computation_artifact_fingerprints = computation$fingerprints,
    computation_gate_applicability = computation$applicability,
    external_gate_artifact_fingerprint = if (gate_artifact_ok)
      external_gates$fingerprint else NA_character_,
    analysis_contract_fingerprint = if (all(contract_linked))
      common_contract$fingerprint else NA_character_,
    calibration_fingerprint = if (calibration_linked)
      calibration$fingerprint else NA_character_,
    multiplier_fingerprint = if (is.null(multiplier_bank)) NA_character_ else
      multiplier_bank$fingerprint,
    multiplier_draws = if (is.null(multiplier_bank)) 0L else
      multiplier_bank$draws,
    analysis_signature = signatures[[1L]],
    source_inference = lapply(collections, `[[`, "source_inference"),
    job_collection_fingerprints = vapply(collections, `[[`, character(1L),
                                         "fingerprint"),
    theory_scope = paste(
      "The chart update is first-order equivalent to the raw one-step",
      "estimator only for regular interior targets. Support intersection",
      "preserves coverage only when the underlying influence-function set",
      "and every preceding v2.2 gate are valid."
    )
  )
  class(out) <- c("scmix_v22_inference", "list")
  out
}

#' Finalize respondent-sequence DML output under protocol v2.2
#'
#' Convenience wrapper for a single target job. Use
#' [scmix_v22_finalize_collections()] after [scmix_v22_collect()] when a frozen
#' target family is split across jobs.
#'
#' @inheritParams scmix_v22_collect
#' @inheritParams scmix_v22_finalize_collections
#' @return A `scmix_v22_inference` object.
#' @export
scmix_v22_finalize <- function(
    inference, registry, full_sample_plugin = NULL,
    analysis_contract = inference$v22_analysis_contract,
    derivative_audit = inference$v22_derivative_audit,
    multiplier_bank = NULL, calibration = NULL, external_gates = NULL,
    computation_artifacts = NULL,
    complete_external_record = FALSE, level = 0.95) {
  collection <- scmix_v22_collect(
    inference, registry, full_sample_plugin, analysis_contract,
    derivative_audit)
  scmix_v22_finalize_collections(
    list(collection), multiplier_bank = multiplier_bank,
    calibration = calibration, external_gates = external_gates,
    computation_artifacts = computation_artifacts,
    complete_external_record = complete_external_record, level = level)
}

#' @export
print.scmix_v22_inference <- function(x, ...) {
  cat("paperps respondent-level inference protocol v2.2\n")
  cat("  protocol status:", x$protocol_status, "\n")
  tab <- do.call(rbind, lapply(x$targets, function(z) {
    interval <- if (all(is.na(z$simultaneous_interval)))
      z$pointwise_interval else z$simultaneous_interval
    data.frame(
    target_id = z$spec$target_id,
    full_sample_plugin = z$full_sample_plugin,
    crossfit_plugin = z$crossfit_plugin,
    raw_one_step = z$raw_one_step,
    reported_estimate = z$reported_estimate,
    lower = interval[["lower"]],
    upper = interval[["upper"]],
    fit_status = z$status$fit_status,
    target_status = z$status$target_status,
    release_class = z$status$release_class,
    stringsAsFactors = FALSE)
  }))
  print(tab, row.names = FALSE)
  invisible(x)
}
