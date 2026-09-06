## Prospective, response-free Step-3 calibration harness for protocol v2.3.
##
## This module deliberately does not construct a production calibration
## artifact. It reads the frozen prospective plan, verifies response-free
## design templates, generates synthetic outcomes from prespecified DGPs, and
## returns auditable threshold proposals and untouched-validation summaries.

.scmix_v23_step3_plan_id <- "paperps-step3-calibration-2026-09-06-v2.3-v1"
.scmix_v23_step3_protocol_id <- "paperps-inference-2026-09-06-v2.3"
.scmix_v23_step3_file_names <- c(
  plan = "inference-v2.3-step3-plan.dcf",
  regimes = "inference-v2.3-step3-regimes.csv",
  constants = "inference-v2.3-step3-constant-registry.csv",
  designs = "inference-v2.3-step3-design-manifest.csv",
  dgp = "inference-v2.3-step3-dgp-registry.csv",
  severity = "inference-v2.3-step3-severity-registry.csv",
  targets = "inference-v2.3-step3-target-registry.csv",
  operating_characteristics = "inference-v2.3-step3-oc-registry.csv")
.scmix_v23_step3_forbidden_names <- c(
  "y", "choice", "chosen", "outcome", "response", "selected", "vote",
  "candidatevote", "cwin")

.scmix_v23_step3_file <- function(name) {
  file_name <- unname(.scmix_v23_step3_file_names[[name]])
  if (is.null(file_name)) stop("Unknown Step-3 plan file `", name, "`.",
                               call. = FALSE)
  installed <- system.file("protocol", file_name, package = "sconjoint")
  candidates <- c(
    installed,
    file.path(getwd(), "inst", "protocol", file_name),
    file.path(Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = ""),
              "inst", "protocol", file_name))
  candidates <- unique(candidates[nzchar(candidates)])
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("Cannot locate the frozen Step-3 plan file `", file_name, "`.",
         call. = FALSE)
  }
  normalizePath(found[[1L]], mustWork = TRUE)
}

.scmix_v23_step3_asset_file <- function(...) {
  rel <- file.path(...)
  installed <- system.file(rel, package = "sconjoint")
  candidates <- c(
    installed, file.path(getwd(), "inst", rel),
    file.path(Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = ""), "inst", rel))
  candidates <- unique(candidates[nzchar(candidates)])
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("Cannot locate frozen Step-3 asset `", rel, "`.",
                           call. = FALSE)
  normalizePath(found[[1L]], mustWork = TRUE)
}

.scmix_v23_step3_sha256 <- function(x) {
  path <- tempfile("scmix-v23-step3-hash-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 3, compress = FALSE)
  unname(as.character(tools::sha256sum(path)))
}

.scmix_v23_step3_sign <- function(payload, class_name) {
  payload$fingerprint <- .scmix_v23_step3_sha256(payload)
  class(payload) <- c(class_name, "list")
  payload
}

.scmix_v23_step3_intact <- function(x, class_name) {
  if (!inherits(x, class_name) || !is.list(x)) return(FALSE)
  payload <- unclass(x)
  recorded <- payload$fingerprint
  payload$fingerprint <- NULL
  is.character(recorded) && length(recorded) == 1L && !is.na(recorded) &&
    identical(recorded, .scmix_v23_step3_sha256(payload))
}

.scmix_v23_step3_number <- function(x, name, integer = FALSE) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) != 1L || !is.finite(out) ||
      (integer && out != as.integer(out))) {
    stop("Frozen Step-3 field `", name, "` is malformed.", call. = FALSE)
  }
  if (integer) as.integer(out) else out
}

.scmix_v23_step3_forbidden <- function(x) {
  nm <- names(x)
  if (is.null(nm)) return(character())
  key <- tolower(gsub("[^[:alnum:]]", "", nm))
  nm[key %in% .scmix_v23_step3_forbidden_names]
}

#' Read the frozen protocol-v2.3 Step-3 calibration plan
#'
#' The returned object combines the prospective DCF plan, the nine-regime
#' registry, the 26-constant registry, and the response-free design manifest.
#' It contains no calibrated values and is not a production approval artifact.
#'
#' @return An intact `scmix_v23_calibration_plan` object.
#' @export
scmix_v23_calibration_plan <- function() {
  paths <- vapply(names(.scmix_v23_step3_file_names),
                  .scmix_v23_step3_file, character(1L))
  raw <- as.list(read.dcf(paths[["plan"]])[1L, , drop = TRUE])
  regimes <- utils::read.csv(paths[["regimes"]], stringsAsFactors = FALSE,
                             check.names = FALSE)
  constants <- utils::read.csv(paths[["constants"]],
                               stringsAsFactors = FALSE,
                               check.names = FALSE)
  designs <- utils::read.csv(paths[["designs"]], stringsAsFactors = FALSE,
                             check.names = FALSE)
  dgp <- utils::read.csv(paths[["dgp"]], stringsAsFactors = FALSE,
                         check.names = FALSE)
  severity <- utils::read.csv(paths[["severity"]], stringsAsFactors = FALSE,
                              check.names = FALSE)
  targets <- utils::read.csv(paths[["targets"]], stringsAsFactors = FALSE,
                             check.names = FALSE)
  operating_characteristics <- utils::read.csv(
    paths[["operating_characteristics"]], stringsAsFactors = FALSE,
    check.names = FALSE)
  if (!identical(raw[["Plan-Id"]], .scmix_v23_step3_plan_id) ||
      !identical(raw[["Protocol-Id"]], .scmix_v23_step3_protocol_id)) {
    stop("The installed Step-3 plan does not match the frozen protocol.",
         call. = FALSE)
  }
  required_regimes <- c(
    "well_informed_interior", "probability_near_boundary",
    "directional_variance_near_zero", "weak_inverse_problem",
    "optimization_error", "integration_error",
    "residual_shape_misspecification", "serial_dependence",
    "informative_completion")
  if (!identical(regimes$regime_id, required_regimes) ||
      !identical(regimes$regime_index, seq_along(required_regimes))) {
    stop("The installed Step-3 regime registry is not the frozen registry.",
         call. = FALSE)
  }
  if (!identical(dgp$regime_id, required_regimes) ||
      !identical(dgp$regime_index, seq_along(required_regimes)) ||
      anyDuplicated(dgp$regime_id)) {
    stop("The executable DGP registry is not aligned with the frozen regimes.",
         call. = FALSE)
  }
  if (any(!required_regimes %in% severity$regime_id) ||
      anyDuplicated(paste(severity$regime_id, severity$severity_id,
                          sep = "\r")) ||
      any(severity$severity_index < 1L) ||
      any(!severity$certification_truth %in%
            c("safe", "transition", "unsafe"))) {
    stop("The frozen severity grid is malformed or incomplete.",
         call. = FALSE)
  }
  targeted_flag <- tolower(as.character(regimes$targeted_failure)) == "true"
  if (anyNA(targeted_flag)) {
    stop("The targeted-failure flags are malformed.", call. = FALSE)
  }
  targeted <- regimes$regime_id[targeted_flag]
  if (any(vapply(targeted, function(id) !identical(
      severity$certification_truth[severity$regime_id == id],
      c("safe", "transition", "unsafe")), logical(1L)))) {
    stop("Each targeted-failure regime must freeze safe, transition, and unsafe rows.",
         call. = FALSE)
  }
  required_targets <- c(
    "unrestricted_mean_direction", "probability", "subgroup_share",
    "preference_direction_share", "compensating_benefit_share",
    "directional_heterogeneity", "mean_tradeoff_mrs_ratio")
  required_ocs <- c(
    "unconditional_coverage", "coverage_conditional_on_release",
    "false_certification_rate", "interval_length_natural_median",
    "interval_length_natural_p90", "interval_length_se_units_median",
    "interval_length_se_units_p90",
    "empty_constraint_set_rate", "uninformative_constraint_set_rate",
    "gate_passage_frequency", "gate_state_frequency",
    "first_failure_frequency")
  if (!identical(targets$target_id, required_targets) ||
      !identical(operating_characteristics$characteristic, required_ocs)) {
    stop("The target or operating-characteristic registry changed.",
         call. = FALSE)
  }
  required_constants <- if (exists(
    ".scmix_v22_required_calibration_names", inherits = TRUE)) {
    get(".scmix_v22_required_calibration_names", inherits = TRUE)
  } else constants$constant
  if (!identical(constants$constant, required_constants) ||
      nrow(constants) != 26L || anyDuplicated(constants$constant) ||
      any(!constants$comparator %in%
            c("less_than_or_equal", "greater_than_or_equal"))) {
    stop("The installed Step-3 constant registry is not the frozen 26-name registry.",
         call. = FALSE)
  }
  if (!identical(designs$design_id, c("br2017", "gs2020", "sw2022")) ||
      any(!grepl("^[[:xdigit:]]{64}$", designs$response_free_payload_hash)) ||
      any(!grepl("^[[:xdigit:]]{64}$", designs$template_file_sha256)) ||
      any(!nzchar(designs$ti_histogram)) ||
      any(designs$hash_algorithm !=
            "R_serialization_v3_uncompressed_SHA256")) {
    stop("The installed response-free design manifest is malformed.",
         call. = FALSE)
  }
  extractor <- .scmix_v23_step3_asset_file(
    "calibration", "v23", "extract_response_free_design.R")
  extractor_hash <- unname(as.character(tools::sha256sum(extractor)))
  if (any(designs$extractor_file_sha256 != extractor_hash)) {
    stop("The frozen response-free extractor hash does not match the design manifest.",
         call. = FALSE)
  }
  template_paths <- vapply(designs$template_filename, function(file_name)
    .scmix_v23_step3_asset_file("calibration", "v23", "designs", file_name),
    character(1L))
  template_hashes <- vapply(template_paths, function(path)
    unname(as.character(tools::sha256sum(path))), character(1L))
  if (!identical(unname(template_hashes), designs$template_file_sha256)) {
    stop("A committed response-free template file hash does not match the manifest.",
         call. = FALSE)
  }
  template_wrappers <- lapply(template_paths, readRDS)
  wrapper_ok <- vapply(seq_along(template_wrappers), function(i) {
    x <- template_wrappers[[i]]
    is.list(x) && identical(x$design_id, designs$design_id[[i]]) &&
      isTRUE(x$response_free) && !length(x$outcome_fields_stored) &&
      identical(x$response_free_payload_sha256,
                designs$response_free_payload_hash[[i]]) &&
      identical(.scmix_v23_step3_sha256(x$payload),
                designs$response_free_payload_hash[[i]]) &&
      !length(.scmix_v23_step3_forbidden(x$payload))
  }, logical(1L))
  if (any(!wrapper_ok)) {
    stop("A committed design wrapper is not intact and response-free.",
         call. = FALSE)
  }
  whitelist_paths <- vapply(designs$whitelist_file, function(file_name)
    .scmix_v23_step3_asset_file("protocol", file_name), character(1L))
  whitelist <- lapply(whitelist_paths, function(path)
    as.list(read.dcf(path)[1L, , drop = TRUE]))
  if (any(vapply(seq_along(whitelist), function(i) {
    rule <- whitelist[[i]]
    !identical(rule[["Protocol-Id"]], .scmix_v23_step3_protocol_id) ||
      !identical(rule[["Design-Id"]], designs$design_id[[i]]) ||
      !identical(rule[["Source-SHA256"]], designs$source_file_sha256[[i]]) ||
      !identical(rule[["Response-Free-Payload-SHA256"]],
                 designs$response_free_payload_hash[[i]])
  }, logical(1L)))) {
    stop("A frozen design whitelist disagrees with the design manifest.",
         call. = FALSE)
  }
  if (!identical(raw[["Application-Outcomes-Permitted"]], "false") ||
      !identical(raw[["Threshold-Equality-Policy"]],
                 paste0("comparator_equality_passes_with_exact_double_",
                        "comparison_and_no_hidden_tolerance")) ||
      !identical(raw[["Boundary-Alias-Policy"]], paste0(
        "no_cross_constant_aliases;all_26_constants_are_selected_",
        "independently;metric_equal_to_its_own_cutoff_passes")) ||
      !identical(raw[["Production-Rank-Scope"]], "q1_only") ||
      !identical(raw[["Production-Manifest-Creation"]],
                 "conditional_on_all_frozen_validation_criteria_passing")) {
    stop("The installed Step-3 outcome lock or equality policy changed.",
         call. = FALSE)
  }
  numeric_fields <- c(
    oracle_calibration_reps = "Oracle-Calibration-Replications-Per-Cell",
    oracle_validation_reps = "Oracle-Validation-Replications-Per-Cell",
    numerical_calibration_base_outcomes =
      "Numerical-Calibration-Base-Outcomes-Per-Design",
    numerical_validation_base_outcomes =
      "Numerical-Validation-Base-Outcomes-Per-Design",
    transport_validation_reps =
      "Transport-Validation-Replications-Per-Design",
    oracle_calibration_seed = "Oracle-Calibration-Master-Seed",
    oracle_validation_seed = "Oracle-Validation-Master-Seed",
    numerical_calibration_seed = "Numerical-Calibration-Master-Seed",
    numerical_validation_seed = "Numerical-Validation-Master-Seed",
    transport_validation_seed = "Transport-Validation-Master-Seed",
    maximum_workers = "Maximum-Workers",
    torch_threads_per_worker = "Torch-Threads-Per-Worker",
    blas_threads_per_worker = "BLAS-Threads-Per-Worker")
  counts <- vapply(numeric_fields, function(nm)
    .scmix_v23_step3_number(raw[[nm]], nm, integer = TRUE), integer(1L))
  rates <- c(
    nominal_coverage = .scmix_v23_step3_number(
      raw[["Nominal-Coverage"]], "Nominal-Coverage"),
    minimum_benign_passage = .scmix_v23_step3_number(
      raw[["Minimum-Benign-Calibration-Passage"]],
      "Minimum-Benign-Calibration-Passage"),
    maximum_calibration_false_certification = .scmix_v23_step3_number(
      raw[["Maximum-Targeted-Calibration-False-Certification"]],
      "Maximum-Targeted-Calibration-False-Certification"))
  payload <- list(
    plan_id = raw[["Plan-Id"]], protocol_id = raw[["Protocol-Id"]],
    frozen_on = raw[["Frozen-On"]], status = raw[["Status"]],
    raw = raw, counts = counts, rates = rates,
    regimes = regimes, constants = constants, designs = designs,
    dgp = dgp, severity = severity, targets = targets,
    operating_characteristics = operating_characteristics,
    source_files = c(basename(paths), basename(whitelist_paths),
                     basename(extractor), basename(template_paths)),
    source_hashes = vapply(paths, function(path)
      unname(as.character(tools::sha256sum(path))), character(1L)),
    whitelist_hashes = vapply(whitelist_paths, function(path)
      unname(as.character(tools::sha256sum(path))), character(1L)),
    template_file_hashes = template_hashes,
    extractor_hash = extractor_hash,
    application_outcomes_permitted = FALSE,
    creates_production_manifest = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_calibration_plan")
}

.scmix_v23_step3_design_payload <- function(
    deltaX, Z, respondent_id, task_id, position) {
  list(
    deltaX = unname(deltaX), deltaX_names = colnames(deltaX),
    Z = unname(Z), Z_names = colnames(Z),
    respondent_id = as.character(respondent_id),
    task_id = as.character(task_id), position = position)
}

.scmix_v23_step3_ti_histogram <- function(respondent_id) {
  counts <- as.integer(table(factor(
    respondent_id, levels = unique(respondent_id))))
  freq <- table(counts)
  paste0(names(freq), ":", as.integer(freq), collapse = ";")
}

#' Construct a response-free design template for Step-3 calibration
#'
#' Only profile contrasts, moderators, and respondent/task layout are accepted.
#' No argument or stored field can contain observed choices. Frozen application
#' templates are verified against the SHA-256 manifest shipped with the plan.
#'
#' @param deltaX Finite task-level profile-contrast matrix.
#' @param Z Finite task-level moderator matrix.
#' @param respondent_id,task_id Nonmissing task-layout identifiers.
#' @param design_id Stable design identifier.
#' @param position Optional response-free position assignment.
#' @param verify_manifest Require exact equality with a frozen design manifest
#'   entry. Set to `FALSE` only for explicitly labeled pilot fixtures.
#' @return An intact `scmix_v23_design_template` containing no outcomes.
#' @export
scmix_v23_design_template <- function(
    deltaX, Z, respondent_id, task_id, design_id,
    position = NULL, verify_manifest = TRUE) {
  design_id <- as.character(design_id)
  if (length(design_id) != 1L || is.na(design_id) || !nzchar(design_id)) {
    stop("`design_id` must be one nonempty identifier.", call. = FALSE)
  }
  deltaX <- as.matrix(deltaX)
  Z <- as.matrix(Z)
  storage.mode(deltaX) <- storage.mode(Z) <- "double"
  n <- nrow(deltaX)
  if (!n || !ncol(deltaX) || nrow(Z) != n ||
      any(!is.finite(deltaX)) || any(!is.finite(Z))) {
    stop("`deltaX` and `Z` must be finite task-level matrices with equal row counts.",
         call. = FALSE)
  }
  bad_names <- c(.scmix_v23_step3_forbidden(as.data.frame(deltaX)),
                 .scmix_v23_step3_forbidden(as.data.frame(Z)))
  if (length(bad_names)) {
    stop("Outcome-like fields are prohibited in a response-free template: ",
         paste(unique(bad_names), collapse = ", "), ".", call. = FALSE)
  }
  if (length(respondent_id) != n || length(task_id) != n ||
      anyNA(respondent_id) || anyNA(task_id)) {
    stop("Respondent and task identifiers must be complete and task-level.",
         call. = FALSE)
  }
  respondent_id <- as.character(respondent_id)
  task_id <- as.character(task_id)
  if (any(!nzchar(respondent_id)) || any(!nzchar(task_id)) ||
      anyDuplicated(paste(respondent_id, task_id, sep = "\r"))) {
    stop("Each response-free respondent-task pair must be unique.",
         call. = FALSE)
  }
  if (!is.null(position)) {
    if (length(position) != n || anyNA(position)) {
      stop("`position` must be NULL or one complete task-level vector.",
           call. = FALSE)
    }
    position <- unname(position)
  }
  payload <- .scmix_v23_step3_design_payload(
    deltaX, Z, respondent_id, task_id, position)
  counts <- table(factor(respondent_id, levels = unique(respondent_id)))
  ti_histogram <- .scmix_v23_step3_ti_histogram(respondent_id)
  hashes <- c(
    deltaX_hash = .scmix_v23_step3_sha256(unname(deltaX)),
    Z_hash = .scmix_v23_step3_sha256(unname(Z)),
    layout_hash = .scmix_v23_step3_sha256(list(
      respondent_id = respondent_id, task_id = task_id,
      position = position)),
    response_free_payload_hash = .scmix_v23_step3_sha256(payload))
  plan <- scmix_v23_calibration_plan()
  manifest <- plan$designs[plan$designs$design_id == design_id, , drop = FALSE]
  verified <- FALSE
  if (nrow(manifest)) {
    observed <- c(
      n_task_rows = n, n_respondents = length(counts),
      min_tasks = min(counts), max_tasks = max(counts),
      p_deltaX = ncol(deltaX), p_Z = ncol(Z))
    declared <- unlist(manifest[1L, names(observed)], use.names = TRUE)
    dimensions_ok <- identical(as.numeric(observed), as.numeric(declared)) &&
      identical(ti_histogram, as.character(manifest$ti_histogram[[1L]]))
    hashes_ok <- all(vapply(names(hashes), function(nm)
      identical(unname(hashes[[nm]]), as.character(manifest[[nm]][[1L]])),
      logical(1L)))
    verified <- dimensions_ok && hashes_ok
    if (isTRUE(verify_manifest) && !verified) {
      stop("Response-free template `", design_id,
           "` does not match its frozen SHA-256 manifest.", call. = FALSE)
    }
  } else if (isTRUE(verify_manifest)) {
    stop("No frozen design-manifest entry exists for `", design_id, "`.",
         call. = FALSE)
  }
  out <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    design_id = design_id, deltaX = unname(deltaX),
    deltaX_names = colnames(deltaX), Z = unname(Z), Z_names = colnames(Z),
    respondent_id = respondent_id, task_id = task_id, position = position,
    n_task_rows = n, n_respondents = length(counts),
    min_tasks = as.integer(min(counts)), max_tasks = as.integer(max(counts)),
    ti_histogram = ti_histogram,
    p_deltaX = ncol(deltaX), p_Z = ncol(Z), hashes = hashes,
    manifest_verified = verified,
    response_free = TRUE, outcome_fields_stored = character())
  .scmix_v23_step3_sign(out, "scmix_v23_design_template")
}

#' Load one committed response-free Step-3 design template
#'
#' The compressed wrapper and its canonical uncompressed inner payload are
#' both verified against the prospective manifest before a signed template is
#' returned. Application outcomes are neither stored nor accepted.
#'
#' @param design_id One of `br2017`, `gs2020`, or `sw2022`.
#' @return An intact `scmix_v23_design_template`.
#' @export
scmix_v23_load_frozen_design <- function(design_id) {
  plan <- scmix_v23_calibration_plan()
  row <- plan$designs[plan$designs$design_id == design_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("`design_id` is not in the frozen Step-3 design manifest.",
         call. = FALSE)
  }
  path <- .scmix_v23_step3_asset_file(
    "calibration", "v23", "designs", row$template_filename[[1L]])
  if (!identical(unname(as.character(tools::sha256sum(path))),
                 row$template_file_sha256[[1L]])) {
    stop("The committed compressed design wrapper failed SHA-256 verification.",
         call. = FALSE)
  }
  wrapper <- readRDS(path)
  if (!is.list(wrapper) || !identical(wrapper$design_id, design_id) ||
      !isTRUE(wrapper$response_free) || length(wrapper$outcome_fields_stored) ||
      !identical(.scmix_v23_step3_sha256(wrapper$payload),
                 row$response_free_payload_hash[[1L]])) {
    stop("The committed design wrapper is not intact and response-free.",
         call. = FALSE)
  }
  x <- wrapper$payload
  deltaX <- x$deltaX
  Z <- x$Z
  colnames(deltaX) <- x$deltaX_names
  colnames(Z) <- x$Z_names
  scmix_v23_design_template(
    deltaX = deltaX, Z = Z,
    respondent_id = x$respondent_id, task_id = x$task_id,
    position = x$position, design_id = design_id, verify_manifest = TRUE)
}

.scmix_v23_step3_check_design <- function(x, require_frozen = TRUE) {
  if (!.scmix_v23_step3_intact(x, "scmix_v23_design_template") ||
      !isTRUE(x$response_free) || length(x$outcome_fields_stored) ||
      (isTRUE(require_frozen) && !isTRUE(x$manifest_verified))) {
    stop("An intact response-free Step-3 design template is required.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.scmix_v23_step3_seed <- function(
    plan, layer, stream, design_index, regime_index, severity_index,
    replication) {
  key <- paste(layer, stream, "seed", sep = "_")
  master <- unname(plan$counts[[key]])
  if (is.null(master) || !is.finite(master)) {
    stop("The frozen plan does not define seed stream `", key, "`.",
         call. = FALSE)
  }
  seed <- master + (as.integer(design_index) - 1L) * 30000000 +
    (as.integer(regime_index) - 1L) * 2000000 +
    (as.integer(severity_index) - 1L) * 500000 + as.integer(replication)
  if (!is.finite(seed) || seed < 1 || seed > .Machine$integer.max) {
    stop("A requested Step-3 seed lies outside R's integer seed range.",
         call. = FALSE)
  }
  as.integer(seed)
}

.scmix_v23_step3_repetitions <- function(plan, layer, stream, regime_role) {
  if (identical(layer, "oracle")) {
    return(unname(plan$counts[[paste0("oracle_", stream, "_reps")]]))
  }
  unname(plan$counts[[paste0("numerical_", stream, "_base_outcomes")]])
}

.scmix_v23_step3_fix_sign <- function(v) {
  j <- which(abs(v) > sqrt(.Machine$double.eps))[1L]
  if (length(j) && is.finite(j) && v[j] < 0) -v else v
}

.scmix_v23_step3_standardize <- function(x) {
  x <- as.matrix(x)
  center <- colMeans(x)
  scale <- apply(x, 2L, stats::sd)
  scale[!is.finite(scale) | scale <= sqrt(.Machine$double.eps)] <- 1
  sweep(sweep(x, 2L, center, `-`), 2L, scale, `/`)
}

.scmix_v23_step3_rowsum_mean <- function(x, group, n_group) {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1L)
  sums <- rowsum(x, group = group, reorder = FALSE)
  sweep(sums, 1L, n_group, `/`)
}

.scmix_v23_step3_prepare <- function(template) {
  id_levels <- unique(template$respondent_id)
  group <- match(template$respondent_id, id_levels)
  task_count <- tabulate(group, nbins = length(id_levels))
  dx <- .scmix_v23_step3_standardize(template$deltaX)
  gram <- crossprod(dx) / max(1, nrow(dx))
  eig <- eigen(gram, symmetric = TRUE)
  strong_direction <- .scmix_v23_step3_fix_sign(eig$vectors[, 1L])
  weak_direction <- .scmix_v23_step3_fix_sign(
    eig$vectors[, ncol(eig$vectors)])
  strong_row <- drop(dx %*% strong_direction)
  weak_row <- drop(dx %*% weak_direction)
  strong <- drop(.scmix_v23_step3_rowsum_mean(
    strong_row, group, task_count))
  weak <- drop(.scmix_v23_step3_rowsum_mean(weak_row, group, task_count))
  rescale <- function(x) {
    s <- stats::sd(x)
    if (!is.finite(s) || s <= sqrt(.Machine$double.eps)) {
      return(seq_along(x) / max(1, length(x)) - 0.5)
    }
    as.numeric((x - mean(x)) / s)
  }
  strong <- rescale(strong)
  weak <- rescale(weak)
  if (ncol(template$Z)) {
    z_task <- .scmix_v23_step3_standardize(template$Z)
    z_resp <- .scmix_v23_step3_rowsum_mean(z_task, group, task_count)
    varying <- which(apply(z_resp, 2L, stats::sd) >
                       sqrt(.Machine$double.eps))
    z <- if (length(varying)) rescale(z_resp[, varying[[1L]]]) else
      rescale(seq_along(id_levels))
  } else z <- rescale(seq_along(id_levels))
  subgroup <- z > stats::median(z)
  if (!any(subgroup) || all(subgroup)) subgroup <- seq_along(z) %% 2L == 0L
  info_ratio <- min(pmax(eig$values, 0)) /
    max(max(pmax(eig$values, 0)), .Machine$double.eps)
  list(
    design_id = template$design_id, n = length(id_levels),
    respondent_id = id_levels, group = group, task_count = task_count,
    strong = strong, weak = weak, z = z, subgroup = subgroup,
    info_ratio = as.numeric(info_ratio), dx_standardized = dx,
    strong_direction = strong_direction, weak_direction = weak_direction)
}

.scmix_v23_step3_regime_parameters <- function(regime_id) {
  out <- list(
    intercept = 0, loading = 0.55, residual = "normal",
    serial_rho = 0, informative_completion = FALSE,
    weak_information = FALSE, optimization_bias_se = 0,
    integration_bias_se = 0)
  if (identical(regime_id, "probability_near_boundary")) {
    out$intercept <- -3.45
  } else if (identical(regime_id, "directional_variance_near_zero")) {
    out$loading <- 0.02
  } else if (identical(regime_id, "weak_inverse_problem")) {
    out$weak_information <- TRUE
  } else if (identical(regime_id, "optimization_error")) {
    out$optimization_bias_se <- 1.4
  } else if (identical(regime_id, "integration_error")) {
    out$integration_bias_se <- 1.1
  } else if (identical(regime_id, "residual_shape_misspecification")) {
    out$residual <- "centered_exponential"
  } else if (identical(regime_id, "serial_dependence")) {
    out$serial_rho <- 0.70
  } else if (identical(regime_id, "informative_completion")) {
    out$informative_completion <- TRUE
  }
  out
}

.scmix_v23_step3_latent <- function(n, residual) {
  if (identical(residual, "centered_exponential")) {
    return((stats::rexp(n) - 1))
  }
  stats::rnorm(n)
}

.scmix_v23_step3_truth <- function(prep, regime_id) {
  par <- .scmix_v23_step3_regime_parameters(regime_id)
  u <- if (identical(par$residual, "centered_exponential")) {
    stats::qexp((seq_len(257L) - 0.5) / 257L) - 1
  } else stats::qnorm((seq_len(257L) - 0.5) / 257L)
  base <- par$intercept + 0.35 * prep$strong + 0.20 * prep$z
  pbar <- rowMeans(stats::plogis(outer(base, par$loading * u, `+`)))
  mean_direction <- 0.22 * prep$z + 0.12 * prep$strong
  sign_share <- mean(outer(mean_direction, par$loading * u, `+`) > 0)
  hz <- stats::var(mean_direction)
  hr <- par$loading^2
  denominator <- if (isTRUE(par$weak_information)) {
    0.025 + 0.005 * prep$weak
  } else 0.60 + 0.08 * prep$z
  numerator <- 0.75 + 0.10 * prep$z
  list(
    probability = mean(pbar), subgroup_share = mean(pbar[prep$subgroup]),
    preference_direction_share = sign_share,
    directional_heterogeneity = hz / max(hz + hr, .Machine$double.eps),
    mean_tradeoff_mrs_ratio = mean(numerator) / mean(denominator),
    pbar = pbar, mean_direction = mean_direction, hz = hz, hr = hr,
    numerator = numerator, denominator = denominator)
}

.scmix_v23_step3_interval <- function(
    estimate, se, level, lower = -Inf, upper = Inf) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  lo <- max(lower, estimate - z * se)
  hi <- min(upper, estimate + z * se)
  empty <- !is.finite(se) || se <= 0 || !is.finite(lo) || !is.finite(hi) ||
    lo > hi
  uninformative <- !empty && is.finite(lower) && is.finite(upper) &&
    lo <= lower + 1e-12 && hi >= upper - 1e-12
  c(lower = lo, upper = hi, empty = as.numeric(empty),
    uninformative = as.numeric(uninformative))
}

.scmix_v23_step3_target_row <- function(
    target_id, target_type, truth, estimate, se, level,
    lower = -Inf, upper = Inf) {
  interval <- .scmix_v23_step3_interval(
    estimate, se, level, lower = lower, upper = upper)
  data.frame(
    target_id = target_id, target_type = target_type,
    truth = truth, estimate = estimate, se = se,
    lower = interval[["lower"]], upper = interval[["upper"]],
    covered = !as.logical(interval[["empty"]]) && truth >= interval[["lower"]] &&
      truth <= interval[["upper"]],
    empty_constraint_set = as.logical(interval[["empty"]]),
    uninformative_constraint_set = as.logical(interval[["uninformative"]]),
    stringsAsFactors = FALSE)
}

.scmix_v23_step3_metric_defaults <- function(registry) {
  upper <- c(
    chart_discrepancy_cutoff = 0.08, nonlinear_remainder_cutoff = 0.08,
    kkt_cutoff = 2e-5, integration_target_se_cutoff = 0.04,
    integration_if_l2_cutoff = 0.04,
    integration_objective_relative_cutoff = 2e-5,
    integration_score_relative_cutoff = 0.04,
    integration_covariance_relative_cutoff = 0.02,
    near_optimum_objective_cutoff = 2e-4,
    multistart_target_se_cutoff = 0.08,
    nested_fit_relative_cutoff = 2e-5,
    loading_probability_invariance_cutoff = 2e-12,
    derivative_cutoff = 0.03, null_space_materiality_cutoff = 0.03,
    numerical_rank_tolerance = 1e-8, defect_cutoff = 0.08,
    ridge_cutoff = 0.08)
  out <- stats::setNames(rep(4, nrow(registry)), registry$constant)
  out[names(upper)] <- upper
  out
}

.scmix_v23_step3_metrics <- function(
    plan, prep, regime_id, target_summary, par) {
  stop(paste(
    "Disabled preliminary pseudo-metric generator.",
    "Step-3 evidence must come from the task-level oracle engine or a",
    "full-fit numerical callback."), call. = FALSE)
  registry <- plan$constants
  base <- .scmix_v23_step3_metric_defaults(registry)
  value <- base * exp(stats::rnorm(length(base), sd = 0.18))
  names(value) <- names(base)
  target_regime <- registry$targeted_failure_regime == regime_id
  upper <- registry$comparator == "less_than_or_equal"
  value[target_regime & upper] <- value[target_regime & upper] * 30
  value[target_regime & !upper] <- value[target_regime & !upper] / 30
  safe_ratio <- function(a, b) abs(a) / max(abs(b), 1e-12)
  value[["chart_discrepancy_cutoff"]] <- target_summary$chart_discrepancy
  value[["nonlinear_remainder_cutoff"]] <- target_summary$nonlinear_remainder
  value[["interior_distance_cutoff"]] <- target_summary$interior_distance
  value[["subgroup_interior_margin"]] <- target_summary$subgroup_margin
  value[["directional_heterogeneity_regularity_margin"]] <-
    target_summary$total_heterogeneity_standardized
  value[["mean_tradeoff_denominator_margin"]] <-
    target_summary$denominator_standardized
  value[["active_eigenvalue_standardized_cutoff"]] <-
    target_summary$directional_variance_standardized
  value[["generalized_information_margin"]] <- if (
    isTRUE(par$weak_information)) {
    max(1e-5, sqrt(prep$n) * prep$info_ratio / 30)
  } else max(2, sqrt(prep$n) * max(prep$info_ratio, 0.05))
  value[["directional_variance_standardized_cutoff"]] <-
    target_summary$directional_variance_standardized
  value[["total_heterogeneity_standardized_cutoff"]] <-
    target_summary$total_heterogeneity_standardized
  value[["denominator_standardized_cutoff"]] <-
    target_summary$denominator_standardized
  if (isTRUE(par$weak_information)) {
    value[c("null_space_materiality_cutoff", "defect_cutoff",
            "ridge_cutoff")] <- c(0.45, 1.25, 1.10) *
      exp(stats::rnorm(3L, sd = 0.15))
    value[["derivative_cutoff"]] <- 0.30 * exp(stats::rnorm(1L, sd = 0.15))
    value[["numerical_rank_tolerance"]] <-
      5e-6 * exp(stats::rnorm(1L, sd = 0.15))
  }
  if (par$optimization_bias_se > 0) {
    value[c("kkt_cutoff", "near_optimum_objective_cutoff",
            "multistart_target_se_cutoff", "nested_fit_relative_cutoff",
            "loading_probability_invariance_cutoff")] <-
      base[c("kkt_cutoff", "near_optimum_objective_cutoff",
             "multistart_target_se_cutoff", "nested_fit_relative_cutoff",
             "loading_probability_invariance_cutoff")] *
      35 * exp(stats::rnorm(5L, sd = 0.12))
  }
  if (par$integration_bias_se > 0) {
    ids <- c("integration_target_se_cutoff", "integration_if_l2_cutoff",
             "integration_objective_relative_cutoff",
             "integration_score_relative_cutoff",
             "integration_covariance_relative_cutoff")
    value[ids] <- base[ids] * 35 * exp(stats::rnorm(length(ids), sd = 0.12))
  }
  value[!is.finite(value) | value < 0] <- .Machine$double.xmax^0.25
  unname(value[registry$constant])
}

.scmix_v23_step3_safe_ratio <- function(a, b) {
  abs(a) / max(abs(b), 1e-12)
}

.scmix_v23_step3_oracle_one <- function(
    prep, regime_id, seed, plan, truth = NULL) {
  set.seed(seed)
  par <- .scmix_v23_step3_regime_parameters(regime_id)
  if (is.null(truth)) truth <- .scmix_v23_step3_truth(prep, regime_id)
  u <- .scmix_v23_step3_latent(prep$n, par$residual)
  base <- par$intercept + 0.35 * prep$strong + 0.20 * prep$z
  if (par$serial_rho > 0) {
    base <- base + par$serial_rho * stats::rnorm(prep$n, sd = 0.45)
  }
  p <- stats::plogis(base + par$loading * u)
  observed_tasks <- prep$task_count
  if (isTRUE(par$informative_completion)) {
    completion_probability <- stats::plogis(
      0.8 - 1.0 * u - 0.45 * abs(base))
    observed_tasks <- stats::rbinom(prep$n, prep$task_count,
                                    completion_probability)
  }
  keep <- observed_tasks > 0L
  if (sum(keep) < 10L) {
    keep[order(observed_tasks, decreasing = TRUE)[seq_len(10L)]] <- TRUE
    observed_tasks[keep & observed_tasks == 0L] <- 1L
  }
  ybar <- stats::rbinom(prep$n, pmax(observed_tasks, 1L), p) /
    pmax(observed_tasks, 1L)
  ybar <- ybar[keep]
  subgroup <- prep$subgroup[keep]
  u_keep <- u[keep]
  z_keep <- prep$z[keep]
  strong_keep <- prep$strong[keep]
  n <- length(ybar)
  probability_raw <- mean(ybar)
  probability_se <- stats::sd(ybar) / sqrt(n)
  bias_se <- par$optimization_bias_se + par$integration_bias_se
  if (bias_se > 0 && is.finite(probability_se)) {
    probability_raw <- probability_raw + bias_se * probability_se
  }
  correction <- 0.25 * probability_se *
    ifelse(probability_raw >= 0.5, 1, -1)
  plugin <- min(max(probability_raw - correction, 1e-8), 1 - 1e-8)
  reported <- stats::plogis(stats::qlogis(plugin) +
                              (probability_raw - plugin) /
                              (plugin * (1 - plugin)))
  group_values <- ybar[subgroup]
  subgroup_estimate <- mean(group_values)
  subgroup_se <- stats::sd(group_values) / sqrt(length(group_values))
  mean_direction <- 0.22 * z_keep + 0.12 * strong_keep
  sign_value <- as.numeric(mean_direction + par$loading * u_keep > 0)
  sign_estimate <- mean(sign_value)
  sign_se <- stats::sd(sign_value) / sqrt(n)
  hz_value <- (mean_direction - mean(mean_direction))^2
  hr_value <- (par$loading * u_keep -
                 mean(par$loading * u_keep))^2
  hz_hat <- mean(hz_value)
  hr_hat <- mean(hr_value)
  hetero_estimate <- hz_hat / max(hz_hat + hr_hat, .Machine$double.eps)
  grad <- c(hr_hat, -hz_hat) / max((hz_hat + hr_hat)^2,
                                   .Machine$double.eps)
  hetero_if <- grad[[1L]] * (hz_value - hz_hat) +
    grad[[2L]] * (hr_value - hr_hat)
  hetero_se <- stats::sd(hetero_if) / sqrt(n)
  numerator_value <- 0.75 + 0.10 * z_keep + 0.08 * u_keep
  denominator_value <- if (isTRUE(par$weak_information)) {
    0.025 + 0.005 * prep$weak[keep] + 0.08 * u_keep
  } else 0.60 + 0.08 * z_keep + 0.05 * u_keep
  num_hat <- mean(numerator_value)
  den_hat <- mean(denominator_value)
  mrs_estimate <- num_hat / den_hat
  mrs_if <- (numerator_value - num_hat) / den_hat -
    num_hat * (denominator_value - den_hat) / den_hat^2
  mrs_se <- stats::sd(mrs_if) / sqrt(n)
  level <- unname(plan$rates[["nominal_coverage"]])
  targets <- rbind(
    .scmix_v23_step3_target_row(
      "probability", "probability", truth$probability,
      reported, probability_se, level, 0, 1),
    .scmix_v23_step3_target_row(
      "subgroup_share", "subgroup_share", truth$subgroup_share,
      subgroup_estimate, subgroup_se, level, 0, 1),
    .scmix_v23_step3_target_row(
      "preference_direction_share", "preference_direction_share",
      truth$preference_direction_share, sign_estimate, sign_se, level, 0, 1),
    .scmix_v23_step3_target_row(
      "directional_heterogeneity", "directional_heterogeneity",
      truth$directional_heterogeneity, hetero_estimate, hetero_se,
      level, 0, 1),
    .scmix_v23_step3_target_row(
      "mean_tradeoff_mrs_ratio", "mean_tradeoff_mrs_ratio",
      truth$mean_tradeoff_mrs_ratio, mrs_estimate, mrs_se, level))
  probability_safe_se <- max(probability_se, 1e-12)
  total_value <- hz_value + hr_value
  total_se <- stats::sd(total_value) / sqrt(n)
  hr_se <- stats::sd(hr_value) / sqrt(n)
  den_se <- stats::sd(denominator_value) / sqrt(n)
  subgroup_rate <- mean(subgroup)
  subgroup_margin <- min(subgroup_rate, 1 - subgroup_rate) * sqrt(n)
  summary <- list(
    chart_discrepancy = abs(reported - probability_raw) /
      probability_safe_se,
    nonlinear_remainder = abs(reported - probability_raw) /
      probability_safe_se,
    interior_distance = min(plugin, 1 - plugin) / probability_safe_se,
    subgroup_margin = subgroup_margin,
    directional_variance_standardized = .scmix_v23_step3_safe_ratio(
      hr_hat, hr_se),
    total_heterogeneity_standardized = .scmix_v23_step3_safe_ratio(
      hz_hat + hr_hat, total_se),
    denominator_standardized = .scmix_v23_step3_safe_ratio(
      den_hat, den_se))
  metrics <- .scmix_v23_step3_metrics(plan, prep, regime_id, summary, par)
  list(
    targets = targets, metrics = metrics, observed_n = n,
    observed_task_fraction = sum(observed_tasks) / sum(prep$task_count),
    specification_supported = !regime_id %in% c(
      "residual_shape_misspecification", "serial_dependence",
      "informative_completion"))
}

.scmix_v23_step3_validate_run_request <- function(
    designs, plan, stream, regime_ids, replications, pilot,
    layer, workers) {
  if (!.scmix_v23_step3_intact(plan, "scmix_v23_calibration_plan")) {
    stop("`plan` must be the intact frozen Step-3 plan.", call. = FALSE)
  }
  stream <- match.arg(stream, c("calibration", "validation"))
  if (inherits(designs, "scmix_v23_design_template")) designs <- list(designs)
  if (!is.list(designs) || !length(designs)) {
    stop("`designs` must contain response-free design templates.",
         call. = FALSE)
  }
  lapply(designs, .scmix_v23_step3_check_design,
         require_frozen = !isTRUE(pilot))
  design_ids <- vapply(designs, `[[`, character(1L), "design_id")
  if (anyDuplicated(design_ids)) stop("Design IDs must be unique.",
                                      call. = FALSE)
  if (!is.character(regime_ids) || !length(regime_ids) ||
      anyDuplicated(regime_ids) ||
      any(!regime_ids %in% plan$regimes$regime_id)) {
    stop("`regime_ids` must be unique IDs in the frozen regime registry.",
         call. = FALSE)
  }
  if (!is.numeric(workers) || length(workers) != 1L || !is.finite(workers) ||
      workers < 1L || workers != as.integer(workers) ||
      workers > plan$counts[["maximum_workers"]]) {
    stop("`workers` must be an integer between one and the frozen maximum.",
         call. = FALSE)
  }
  frozen_reps <- vapply(regime_ids, function(regime_id) {
    role <- plan$regimes$role[match(regime_id, plan$regimes$regime_id)]
    .scmix_v23_step3_repetitions(plan, layer, stream, role)
  }, integer(1L))
  if (is.null(replications)) {
    reps <- frozen_reps
  } else {
    if (!is.numeric(replications) || any(!is.finite(replications)) ||
        any(replications < 1L) || any(replications != as.integer(replications))) {
      stop("`replications` must contain positive integers.", call. = FALSE)
    }
    reps <- rep_len(as.integer(replications), length(regime_ids))
    names(reps) <- regime_ids
    if (!isTRUE(pilot) && !identical(unname(reps), unname(frozen_reps))) {
      stop("Frozen replication counts can be changed only in `pilot = TRUE` runs.",
           call. = FALSE)
    }
  }
  names(reps) <- regime_ids
  list(designs = designs, design_ids = design_ids, stream = stream,
       regime_ids = regime_ids, reps = reps, frozen_reps = frozen_reps,
       pilot = isTRUE(pilot), workers = as.integer(workers))
}

.scmix_v23_step3_apply <- function(X, FUN, workers) {
  if (workers <= 1L || length(X) <= 1L) return(lapply(X, FUN))
  if (.Platform$OS.type == "windows") {
    warning("Parallel Step-3 dispatch is unavailable on Windows; using one worker.",
            call. = FALSE)
    return(lapply(X, FUN))
  }
  parallel::mclapply(X, FUN, mc.cores = min(workers, length(X)),
                     mc.preschedule = FALSE, mc.set.seed = FALSE)
}

#' Disabled preliminary oracle prototype
#'
#' This layer uses only response-free design summaries and synthetic outcomes.
#' It isolates interval geometry, target regularity, and inverse-problem gate
#' behavior. Numerical fit diagnostics are generated only as labeled proxies;
#' their production proposals must be replaced by the end-to-end layer.
#'
#' @param designs One template or a list from [scmix_v23_design_template()].
#' @param stream Either the calibration or untouched validation stream.
#' @param regime_ids Frozen regimes to run.
#' @param replications `NULL` uses frozen counts. Smaller counts require
#'   `pilot = TRUE` and cannot support a production proposal.
#' @param plan Intact frozen plan.
#' @param workers Parallel cell workers, never exceeding ten.
#' @param pilot Whether this is an explicitly non-production pilot.
#' @return This function always fails closed.
#' @keywords internal
#' @noRd
.scmix_v23_step3_deprecated_oracle_calibration <- function(
    designs, stream = c("calibration", "validation"),
    regime_ids = scmix_v23_calibration_plan()$regimes$regime_id,
    replications = NULL, plan = scmix_v23_calibration_plan(),
    workers = 1L, pilot = FALSE) {
  stop(paste(
    "The preliminary summary-statistic oracle simulator is disabled.",
    "Use the task-level q=1 oracle engine in calibration-v23-oracle.R."),
    call. = FALSE)
  stream <- match.arg(stream)
  request <- .scmix_v23_step3_validate_run_request(
    designs, plan, stream, regime_ids, replications, pilot,
    layer = "oracle", workers = workers)
  prepared <- lapply(request$designs, .scmix_v23_step3_prepare)
  jobs <- do.call(rbind, lapply(seq_along(prepared), function(d) {
    data.frame(
      design_index = d,
      regime_index = match(request$regime_ids, plan$regimes$regime_id),
      regime_id = request$regime_ids, stringsAsFactors = FALSE)
  }))
  run_cell <- function(job_index) {
    job <- jobs[job_index, , drop = FALSE]
    d <- job$design_index[[1L]]
    regime_id <- job$regime_id[[1L]]
    regime_index <- job$regime_index[[1L]]
    prep <- prepared[[d]]
    truth <- .scmix_v23_step3_truth(prep, regime_id)
    B <- request$reps[[regime_id]]
    metrics <- matrix(NA_real_, B, nrow(plan$constants),
                      dimnames = list(NULL, plan$constants$constant))
    rep_rows <- vector("list", B)
    target_rows <- vector("list", B)
    for (b in seq_len(B)) {
      seed <- .scmix_v23_step3_seed(
        plan, "oracle", stream, d, regime_index, 1L, b)
      one <- .scmix_v23_step3_oracle_one(
        prep, regime_id, seed, plan, truth = truth)
      metrics[b, ] <- one$metrics
      rep_rows[[b]] <- data.frame(
        design_id = prep$design_id, design_index = d,
        regime_id = regime_id, regime_index = regime_index,
        replication = b, seed = seed, observed_n = one$observed_n,
        observed_task_fraction = one$observed_task_fraction,
        specification_supported = one$specification_supported,
        stringsAsFactors = FALSE)
      one$targets$design_id <- prep$design_id
      one$targets$regime_id <- regime_id
      one$targets$replication <- b
      one$targets$seed <- seed
      target_rows[[b]] <- one$targets
    }
    rep_data <- cbind(do.call(rbind, rep_rows),
                      as.data.frame(metrics, check.names = FALSE))
    target_data <- do.call(rbind, target_rows)
    rownames(rep_data) <- rownames(target_data) <- NULL
    list(replications = rep_data, targets = target_data)
  }
  cells <- .scmix_v23_step3_apply(
    seq_len(nrow(jobs)), run_cell, request$workers)
  if (any(vapply(cells, inherits, logical(1L), "try-error"))) {
    stop("At least one oracle calibration cell failed.", call. = FALSE)
  }
  reps <- do.call(rbind, lapply(cells, `[[`, "replications"))
  targets <- do.call(rbind, lapply(cells, `[[`, "targets"))
  rownames(reps) <- rownames(targets) <- NULL
  layer <- ifelse(plan$constants$primary_layer == "oracle",
                  "oracle", "oracle_proxy")
  names(layer) <- plan$constants$constant
  out <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint,
    stream = stream, layer = "oracle", pilot = request$pilot,
    design_ids = request$design_ids, regime_ids = request$regime_ids,
    replications_per_regime = request$reps,
    frozen_replications_per_regime = request$frozen_reps,
    metric_evidence_layer = layer,
    replications = reps, targets = targets,
    application_outcomes_used = FALSE,
    production_manifest_created = FALSE,
    eligible_for_threshold_selection =
      identical(stream, "calibration") && !request$pilot)
  .scmix_v23_step3_sign(out, "scmix_v23_oracle_calibration_run")
}

.scmix_v23_step3_candidate_grid <- function(x) {
  sort(unique(x[is.finite(x)]))
}

.scmix_v23_step3_pass <- function(value, threshold, comparator) {
  if (identical(comparator, "less_than_or_equal")) {
    value <= threshold
  } else value >= threshold
}

.scmix_v23_step3_g006_compound <- function(components, cutoffs) {
  if (!is.numeric(components) || !is.numeric(cutoffs) ||
      is.null(names(components)) || is.null(names(cutoffs)) ||
      anyDuplicated(names(components)) || anyDuplicated(names(cutoffs)) ||
      !identical(names(components), names(cutoffs)) ||
      any(!is.finite(components)) || any(!is.finite(cutoffs)) ||
      any(cutoffs <= 0)) {
    stop("G006 components and independently calibrated cutoffs must be aligned positive finite named vectors.",
         call. = FALSE)
  }
  ratio <- components / cutoffs
  list(ratio = ratio, statistic = min(ratio), threshold = 1,
       units = "dimensionless", pass = min(ratio) >= 1)
}

.scmix_v23_step3_select_group <- function(
    member_ids, evidence, plan) {
  registry <- plan$constants[match(member_ids, plan$constants$constant),
                             , drop = FALSE]
  comparators <- unique(registry$comparator)
  if (length(comparators) != 1L) {
    stop("Every frozen alias group must use one comparator.", call. = FALSE)
  }
  comparator <- comparators[[1L]]
  candidates <- .scmix_v23_step3_candidate_grid(unlist(lapply(
    member_ids, function(id) evidence[[id]]$value), use.names = FALSE))
  if (!length(candidates)) {
    return(list(threshold = NA_real_, feasible = FALSE,
                benign_passage = NA_real_, false_certification = NA_real_,
                reason = "no_finite_metric_evidence"))
  }
  evaluate <- function(cutoff) {
    member <- lapply(member_ids, function(id) {
      dat <- evidence[[id]]
      adverse_id <- plan$constants$targeted_failure_regime[
        match(id, plan$constants$constant)]
      by_design <- lapply(unique(dat$design_id), function(design_id) {
        benign <- dat$value[
          dat$design_id == design_id &
            dat$regime_id == "well_informed_interior" &
            dat$certification_truth == "safe"]
        adverse <- dat$value[
          dat$design_id == design_id & dat$regime_id == adverse_id &
            dat$certification_truth == "unsafe"]
        if (!length(benign) || !length(adverse)) {
          stop("Threshold evidence lacks a per-design benign or unsafe cell.",
               call. = FALSE)
        }
        c(benign = mean(.scmix_v23_step3_pass(
          benign, cutoff, comparator)),
          adverse = mean(.scmix_v23_step3_pass(
            adverse, cutoff, comparator)))
      })
      by_design <- do.call(rbind, by_design)
      c(benign = min(by_design[, "benign"]),
        adverse = max(by_design[, "adverse"]))
    })
    member <- do.call(rbind, member)
    c(benign = min(member[, "benign"]),
      adverse = max(member[, "adverse"]))
  }
  rates <- t(vapply(candidates, evaluate, numeric(2L)))
  feasible <- rates[, "benign"] >=
    plan$rates[["minimum_benign_passage"]] &
    rates[, "adverse"] <=
    plan$rates[["maximum_calibration_false_certification"]]
  strict <- if (identical(comparator, "less_than_or_equal")) {
    candidates
  } else -candidates
  if (any(feasible)) {
    idx <- which(feasible)
    ord <- order(-rates[idx, "benign"], rates[idx, "adverse"], strict[idx],
                 method = "radix")
    chosen <- idx[ord[[1L]]]
    reason <- "frozen_constraints_satisfied"
  } else {
    violation <- pmax(
      plan$rates[["minimum_benign_passage"]] - rates[, "benign"], 0) +
      pmax(rates[, "adverse"] -
             plan$rates[["maximum_calibration_false_certification"]], 0)
    ord <- order(violation, -rates[, "benign"], rates[, "adverse"], strict,
                 method = "radix")
    chosen <- ord[[1L]]
    reason <- "no_candidate_satisfies_frozen_constraints"
  }
  list(
    threshold = as.numeric(candidates[[chosen]]),
    feasible = isTRUE(feasible[[chosen]]),
    benign_passage = as.numeric(rates[chosen, "benign"]),
    false_certification = as.numeric(rates[chosen, "adverse"]),
    reason = reason)
}

.scmix_v23_step3_alias_groups <- function(plan) {
  lapply(plan$constants$constant, function(id) id)
}

#' Select a 26-constant Step-3 calibration proposal
#'
#' Cutoffs maximize benign calibration passage subject to the frozen benign
#' and targeted false-certification constraints. All 26 constants are selected
#' independently. A complete vector is then assessed jointly without retuning.
#' Missing full-fit numerical evidence leaves those values unset and prevents
#' promotion to a production manifest.
#'
#' @param oracle_run Calibration-stream oracle run.
#' @param numerical_run Optional calibration-stream end-to-end numerical run.
#' @param plan Intact frozen plan.
#' @return An intact `scmix_v23_calibration_proposal`. It is not accepted by
#'   [scmix_v23_calibration_artifact()].
#' @export
scmix_v23_select_calibration_constants <- function(
    oracle_run, numerical_run = NULL,
    plan = scmix_v23_calibration_plan()) {
  if (!.scmix_v23_step3_intact(
      oracle_run, "scmix_v23_oracle_calibration_run") ||
      !identical(oracle_run$stream, "calibration") ||
      !identical(oracle_run$plan_fingerprint, plan$fingerprint)) {
    stop("`oracle_run` must be an intact calibration-stream run under this plan.",
         call. = FALSE)
  }
  numerical_ok <- !is.null(numerical_run) &&
    .scmix_v23_step3_intact(
      numerical_run, "scmix_v23_numerical_calibration_run") &&
    identical(numerical_run$stream, "calibration") &&
    identical(numerical_run$plan_fingerprint, plan$fingerprint)
  if (!is.null(numerical_run) && !numerical_ok) {
    stop("`numerical_run` does not match the frozen calibration stream.",
         call. = FALSE)
  }
  evidence <- list()
  evidence_layer <- stats::setNames(character(nrow(plan$constants)),
                                    plan$constants$constant)
  for (i in seq_len(nrow(plan$constants))) {
    id <- plan$constants$constant[[i]]
    primary <- plan$constants$primary_layer[[i]]
    use_numerical <- identical(primary, "numerical") && numerical_ok &&
      id %in% names(numerical_run$replications)
    source <- if (use_numerical) numerical_run$replications else
      oracle_run$replications
    required_context <- c(
      "design_id", "regime_id", "severity_id", "certification_truth")
    if (any(!required_context %in% names(source))) {
      stop("Calibration evidence must carry design, regime, severity, and certification truth.",
           call. = FALSE)
    }
    value <- if (id %in% names(source) &&
                 (identical(primary, "oracle") || use_numerical)) {
      as.numeric(source[[id]])
    } else rep(NA_real_, nrow(source))
    evidence[[id]] <- data.frame(
      value = value, design_id = source$design_id,
      regime_id = source$regime_id, severity_id = source$severity_id,
      certification_truth = source$certification_truth,
      stringsAsFactors = FALSE)
    evidence_layer[[id]] <- if (use_numerical) "numerical" else if (
      identical(primary, "oracle")) "oracle" else "missing_numerical"
  }
  groups <- .scmix_v23_step3_alias_groups(plan)
  selected <- lapply(groups, .scmix_v23_step3_select_group,
                     evidence = evidence, plan = plan)
  rows <- list()
  for (g in seq_along(groups)) {
    ids <- groups[[g]]
    ans <- selected[[g]]
    for (id in ids) {
      reg <- plan$constants[plan$constants$constant == id, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        constant = id, value = ans$threshold,
        comparator = reg$comparator,
        primary_layer = reg$primary_layer,
        evidence_layer = evidence_layer[[id]],
        targeted_failure_regime = reg$targeted_failure_regime,
        benign_passage = ans$benign_passage,
        calibration_false_certification = ans$false_certification,
        feasible = ans$feasible, selection_reason = ans$reason,
        alias_group = paste(ids, collapse = "="),
        stringsAsFactors = FALSE)
    }
  }
  table <- do.call(rbind, rows)
  table <- table[match(plan$constants$constant, table$constant), , drop = FALSE]
  rownames(table) <- NULL
  constants <- stats::setNames(as.numeric(table$value), table$constant)
  primary_complete <- !any(table$evidence_layer == "missing_numerical") &&
    all(is.finite(constants))
  frozen_size <- !isTRUE(oracle_run$pilot) &&
    (is.null(numerical_run) || !isTRUE(numerical_run$pilot))
  status <- if (any(!table$feasible)) {
    "calibration_infeasible_no_manifest"
  } else if (!primary_complete) {
    "primary_numerical_evidence_missing_no_manifest"
  } else if (!frozen_size) {
    "pilot_proposal_no_manifest"
  } else "calibration_feasible_pending_untouched_validation"
  out <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint, constants = constants,
    selection = table, status = status,
    primary_evidence_complete = primary_complete,
    frozen_replication_counts_used = frozen_size,
    validation_used_for_selection = FALSE,
    production_manifest_created = FALSE,
    forbidden_production_file_names = c(
      "inference-v2.3-calibration-values.dcf",
      "inference-v2.3-calibration.dcf"))
  .scmix_v23_step3_sign(out, "scmix_v23_calibration_proposal")
}
