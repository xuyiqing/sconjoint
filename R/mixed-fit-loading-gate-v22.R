## Typed bridge from a fitted scmix object to the fit/loading gates consumed by
## the v2.2 finalization layer.  It deliberately derives every state from the
## portable fit records.  Callers cannot submit gate states themselves.

.scmix_v22_fit_loading_fingerprint <- function(x) {
  payload <- unclass(x)
  payload$fingerprint <- NULL
  .scmix_v22_fingerprint(payload)
}

#' Validate fit and loading gate evidence
#'
#' @param artifact A `scmix_v22_fit_loading_gate_artifact`.
#' @param analysis_contract The exact signed analysis contract.
#' @param registry The exact frozen target registry.
#' @param calibration_artifact The complete production calibration artifact.
#' @return The artifact, invisibly. An error is raised for missing or altered
#'   evidence or contract bindings.
#' @export
scmix_v22_validate_fit_loading_gate_artifact <- function(
    artifact, analysis_contract, registry, calibration_artifact) {
  registry <- .scmix_v22_registry_payload(registry)
  registry_hash <- .scmix_v22_fingerprint(registry)
  if (!inherits(artifact, "scmix_v22_fit_loading_gate_artifact") ||
      !.scmix_v22_intact(analysis_contract, "scmix_v22_analysis_contract") ||
      !.scmix_v22_validate_calibration(
        calibration_artifact, analysis_contract, require_complete = TRUE) ||
      !is.list(artifact) ||
      !identical(artifact$protocol_id, .scmix_v22_protocol_id) ||
      !identical(artifact$analysis_signature,
                 analysis_contract$analysis_signature) ||
      !identical(artifact$analysis_contract_fingerprint,
                 analysis_contract$fingerprint) ||
      !identical(artifact$registry_fingerprint, registry_hash) ||
      !identical(artifact$calibration_artifact_fingerprint,
                 calibration_artifact$fingerprint) ||
      !identical(artifact$calibration_hash,
                 analysis_contract$calibration_hash) ||
      !identical(artifact$code_hash, analysis_contract$code_commit) ||
      !identical(artifact$data_hash, analysis_contract$data_hash) ||
      !identical(artifact$design_hash, analysis_contract$design_hash) ||
      !identical(artifact$fingerprint,
                 .scmix_v22_fit_loading_fingerprint(artifact))) {
    stop("Malformed, tampered, or contract-mismatched v2.2 fit/loading gate artifact.",
         call. = FALSE)
  }
  fit_registry <- analysis_contract$optimizer_starts
  if (!.scmix_v22_comp_intact(
      fit_registry, "scmix_v22_fit_scope_registry") ||
      !identical(artifact$fit_registry_fingerprint, fit_registry$fingerprint) ||
      !identical(artifact$fit_scope_ids, fit_registry$fit_scope_ids)) {
    stop("Fit/loading evidence does not cover the exact signed fit registry.",
         call. = FALSE)
  }
  entries <- artifact$fit_scopes
  if (!is.list(entries) || !identical(names(entries), artifact$fit_scope_ids)) {
    stop("Fit/loading evidence is missing a registered full or outer-fold scope.",
         call. = FALSE)
  }
  required_gates <- c(paste0("F00", 1:6), "L001", "L002")
  for (scope_id in artifact$fit_scope_ids) {
    entry <- entries[[scope_id]]
    if (!is.list(entry) || !identical(entry$fit_scope_id, scope_id) ||
        !identical(names(entry$gates), required_gates) ||
        !all(vapply(entry$gates, function(g) {
          is.list(g) && g$state %in% c("pass", "fail") &&
            identical(isTRUE(g$pass), identical(g$state, "pass"))
        }, logical(1L))) ||
        !is.character(entry$selected_start_id) ||
        length(entry$selected_start_id) != 1L ||
        !is.character(entry$selected_state_hash) ||
        !identical(entry$calibration_hash, artifact$calibration_hash) ||
        !identical(entry$analysis_signature, artifact$analysis_signature)) {
      stop("A registered fit scope lacks typed F001-F006/L001-L002 evidence.",
           call. = FALSE)
    }
    registered <- fit_registry$starts[
      fit_registry$starts$fit_scope_id == scope_id, , drop = FALSE]
    if (!identical(sort(entry$start_ids, method = "radix"),
                   sort(registered$start_id, method = "radix")) ||
        !identical(as.integer(entry$start_seeds[match(
          registered$start_id, entry$start_ids)]), as.integer(registered$seed))) {
      stop("Optimizer start IDs or seeds differ from the signed registry.",
           call. = FALSE)
    }
  }
  invisible(artifact)
}

#' Construct fit-scoped v2.2 optimizer and loading gate evidence
#'
#' This constructor reads the selected full-sample and outer-fold optimizer
#' records, the explicit raw-loading contract, and live L002 invariance checks
#' from a production `scmix` fit. It signs their complete scope coverage and
#' immutable analysis provenance. Gate states cannot be supplied by the caller.
#'
#' @param fit A production `scmix` fit containing portable multistart records.
#' @param registry The complete frozen v2.2 target registry.
#' @param analysis_contract The exact signed analysis contract.
#' @param calibration_artifact The complete production calibration artifact.
#' @param provenance Nonempty immutable audit provenance.
#' @return A fingerprinted `scmix_v22_fit_loading_gate_artifact`.
#' @export
scmix_v22_fit_loading_gate_artifact <- function(
    fit, registry, analysis_contract, calibration_artifact, provenance) {
  registry <- .scmix_v22_registry_payload(registry)
  if (!inherits(fit, "scmix") ||
      !.scmix_v22_intact(analysis_contract, "scmix_v22_analysis_contract") ||
      !.scmix_v22_validate_calibration(
        calibration_artifact, analysis_contract, require_complete = TRUE)) {
    stop("A production scmix fit and exact intact analysis/calibration contracts are required.",
         call. = FALSE)
  }
  fit_registry <- analysis_contract$optimizer_starts
  if (!.scmix_v22_comp_intact(
      fit_registry, "scmix_v22_fit_scope_registry") ||
      !identical(fit$analysis_signature,
                 analysis_contract$analysis_signature) ||
      !identical(fit$protocol_mode, "production") ||
      !identical(as.integer(fit$q), as.integer(analysis_contract$q)) ||
      !identical(fit_registry$application_id,
                 analysis_contract$application_id) ||
      !identical(fit_registry$specification_id,
                 analysis_contract$specification_id)) {
    stop("The fit is not bound to the signed analysis and fit-scope registry.",
         call. = FALSE)
  }
  .scmix_validate_loading_contract(fit)
  sets <- fit$multistart_artifacts
  fold_ids <- fit_registry$outer_fold_ids
  if (!is.list(sets) || is.null(sets$full) || !is.list(sets$folds) ||
      length(sets$folds) != length(fold_ids)) {
    stop("The fit must retain the full fit and every outer-fold optimizer set.",
         call. = FALSE)
  }
  scope_ids <- c(paste(analysis_contract$specification_id, "full", sep = "::"),
                 paste(analysis_contract$specification_id, "outer", fold_ids,
                       sep = "::"))
  if (!identical(sort(scope_ids, method = "radix"),
                 fit_registry$fit_scope_ids)) {
    stop("Outer-fold optimizer scopes disagree with the signed registry.",
         call. = FALSE)
  }
  ordered_sets <- c(list(sets$full), sets$folds)
  loading <- fit$loading_contract
  l002 <- loading$gate_evidence$L002$fit_scoped_evidence
  if (!is.list(l002) || length(l002) != length(scope_ids)) {
    stop("L002 evidence must cover the full fit and every outer fold.",
         call. = FALSE)
  }
  entries <- stats::setNames(vector("list", length(scope_ids)), scope_ids)
  for (j in seq_along(scope_ids)) {
    records <- ordered_sets[[j]]
    .scmix_validate_multistart_artifacts(records)
    selected_index <- which(vapply(records, `[[`, logical(1L), "selected"))
    if (length(selected_index) != 1L) {
      stop("Each fit scope must retain exactly one selected optimizer start.",
           call. = FALSE)
    }
    selected <- records[[selected_index]]
    status <- selected$v22_optimization_status
    gates <- status$gate_evidence[paste0("F00", 1:6)]
    states <- vapply(gates, `[[`, character(1L), "state")
    if (!is.list(status) || any(!states %in% c("pass", "fail")) ||
        !identical(isTRUE(status$optimization_gate_pass), all(states == "pass")) ||
        !identical(selected$protocol_mode, "production") ||
        !identical(selected$calibration_hash,
                   analysis_contract$calibration_hash) ||
        !identical(as.numeric(gates$F005$threshold),
                   as.numeric(calibration_artifact$constants$kkt_cutoff))) {
      stop("Selected optimizer evidence is not a valid production F001-F006 record.",
           call. = FALSE)
    }
    registered <- fit_registry$starts[
      fit_registry$starts$fit_scope_id == scope_ids[j], , drop = FALSE]
    ids <- vapply(records, `[[`, character(1L), "start_id")
    if (any(vapply(records, function(x) is.null(x$seed), logical(1L)))) {
      stop("Production optimizer evidence must retain every registered start seed.",
           call. = FALSE)
    }
    seeds <- vapply(records, function(x) as.integer(x$seed), integer(1L))
    if (!setequal(ids, registered$start_id) ||
        !identical(seeds[match(registered$start_id, ids)],
                   as.integer(registered$seed))) {
      stop("Retained optimizer starts do not match the signed IDs and seeds.",
           call. = FALSE)
    }
    raw <- if (j == 1L) fit[[loading$full$raw_field]] else
      fit[[loading$folds$raw_field]][[j - 1L]]
    opt <- if (j == 1L) fit[[loading$full$optimization_field]] else
      fit[[loading$folds$optimization_field]][[j - 1L]]
    scale <- if (j == 1L) loading$full$coefficient_scale else
      loading$folds$coefficient_scale[[j - 1L]]
    roundtrip_error <- max(abs(.scmix_loading_to_optimization(raw, scale) - opt), 0)
    roundtrip_threshold <- 100 * .Machine$double.eps * max(1, abs(opt))
    l001_pass <- is.finite(roundtrip_error) &&
      roundtrip_error <= max(roundtrip_threshold)
    live <- l002[[j]]
    .scmix_validate_loading_invariance_evidence(live)
    live_state_hash <- .scmix_stable_state_hash(selected$network_state$state_dict)
    l002_bound <- identical(live$analysis_signature, fit$analysis_signature) &&
      identical(live$scope, selected$scope) &&
      identical(live$protocol_mode, "production") &&
      identical(live$calibration_hash, analysis_contract$calibration_hash) &&
      identical(live$network_state_hash, live_state_hash) &&
      live$state %in% c("pass", "fail") &&
      identical(isTRUE(live$pass), identical(live$state, "pass")) &&
      identical(as.numeric(live$threshold), as.numeric(
        calibration_artifact$constants$loading_probability_invariance_cutoff))
    if (!l001_pass || !l002_bound) {
      stop("Loading-scale gates L001-L002 are not valid for every fit scope.",
           call. = FALSE)
    }
    gates$L001 <- list(state = "pass", pass = TRUE,
      statistic = "raw_optimization_loading_round_trip",
      observed = roundtrip_error, threshold = max(roundtrip_threshold),
      source_fingerprint = .scmix_v22_fingerprint(list(raw, opt, scale)))
    gates$L002 <- list(state = live$state, pass = isTRUE(live$pass),
      statistic = live$statistic,
      observed = live$maximum_absolute_probability_difference,
      sequence_observed = live$maximum_absolute_sequence_loglik_difference,
      threshold = live$threshold,
      source_fingerprint = live$evidence_fingerprint)
    entries[[scope_ids[j]]] <- list(
      fit_scope_id = scope_ids[j], analysis_signature = fit$analysis_signature,
      calibration_hash = analysis_contract$calibration_hash,
      source_scope = selected$scope, start_ids = ids, start_seeds = seeds,
      optimizer_artifact_set_fingerprint =
        attr(records, "artifact_set_fingerprint"),
      selected_start_id = selected$start_id,
      selected_state_hash = selected$state_hash,
      selected_optimizer_artifact_fingerprint = selected$artifact_fingerprint,
      loading_contract_id = loading$contract_id, gates = gates)
  }
  payload <- list(
    protocol_id = .scmix_v22_protocol_id,
    analysis_signature = analysis_contract$analysis_signature,
    analysis_contract_fingerprint = analysis_contract$fingerprint,
    registry_fingerprint = .scmix_v22_fingerprint(registry),
    fit_registry_fingerprint = fit_registry$fingerprint,
    calibration_artifact_fingerprint = calibration_artifact$fingerprint,
    calibration_hash = analysis_contract$calibration_hash,
    code_hash = analysis_contract$code_commit,
    data_hash = analysis_contract$data_hash,
    design_hash = analysis_contract$design_hash,
    application_id = analysis_contract$application_id,
    specification_id = analysis_contract$specification_id,
    fit_scope_ids = fit_registry$fit_scope_ids,
    fit_scopes = entries[fit_registry$fit_scope_ids],
    provenance = .scmix_v22_text(provenance, "provenance"))
  payload$fingerprint <- .scmix_v22_fingerprint(payload)
  class(payload) <- c("scmix_v22_fit_loading_gate_artifact", "list")
  scmix_v22_validate_fit_loading_gate_artifact(
    payload, analysis_contract, registry, calibration_artifact)
  payload
}
