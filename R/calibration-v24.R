## Lean prospective validation design for inference protocol v2.4.
##
## Version 2.4 does not estimate reporting thresholds from simulations.  It
## freezes numerical tolerances and statistical decision rules before any
## design-matched synthetic outcome is generated.  This file intentionally
## provides only response-free design preparation, exact DGP geometry, plan
## validation, and a fail-closed outcome lock.  One hash-bound end-to-end
## estimator/inference evaluator must be approved in a future commit before
## the lock can be opened.

.scmix_v24_protocol_id <- "paperps-inference-2026-09-06-v2.4"
.scmix_v24_plan_id <- "paperps-lean-validation-2026-09-06-v2.4-v1"
.scmix_v24_subset_algorithm <-
  "exact_Ti_x_first_varying_Z_above_mean_min1_hamilton_then_salted_hash_v1"
.scmix_v24_file_names <- c(
  plan = "inference-v2.4-validation-plan.dcf",
  designs = "inference-v2.4-design-manifest.csv",
  cells = "inference-v2.4-validation-cells.csv",
  targets = "inference-v2.4-target-registry.csv",
  decisions = "inference-v2.4-decision-registry.csv",
  approval = "inference-v2.4-approval-matrix.csv",
  fit_regularity = "inference-v2.4-fit-regularity-registry.csv",
  truths = "inference-v2.4-target-truths.csv")
.scmix_v24_forbidden_names <- c(
  "y", "choice", "chosen", "outcome", "response", "selected", "vote",
  "candidatevote", "cwin")

.scmix_v24_file <- function(name) {
  file_name <- unname(.scmix_v24_file_names[[name]])
  if (is.null(file_name)) {
    stop("Unknown v2.4 plan file `", name, "`.", call. = FALSE)
  }
  installed <- system.file("protocol", file_name, package = "sconjoint")
  source_root <- Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = "")
  candidates <- unique(c(
    installed,
    file.path(getwd(), "inst", "protocol", file_name),
    file.path(source_root, "inst", "protocol", file_name)))
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("Cannot locate frozen v2.4 plan file `", file_name, "`.",
         call. = FALSE)
  }
  normalizePath(found[[1L]], mustWork = TRUE)
}

.scmix_v24_sha256 <- function(x) {
  path <- tempfile("scmix-v24-hash-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 3, compress = FALSE)
  unname(as.character(tools::sha256sum(path)))
}

.scmix_v24_sign <- function(payload, class_name) {
  payload$fingerprint <- .scmix_v24_sha256(payload)
  class(payload) <- c(class_name, "list")
  payload
}

.scmix_v24_intact <- function(x, class_name) {
  if (!inherits(x, class_name) || !is.list(x)) return(FALSE)
  payload <- unclass(x)
  recorded <- payload$fingerprint
  payload$fingerprint <- NULL
  is.character(recorded) && length(recorded) == 1L && !is.na(recorded) &&
    identical(recorded, .scmix_v24_sha256(payload))
}

.scmix_v24_number <- function(x, name, integer = FALSE) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || !is.finite(value) ||
      (isTRUE(integer) && value != as.integer(value))) {
    stop("Frozen v2.4 field `", name, "` is malformed.", call. = FALSE)
  }
  if (isTRUE(integer)) as.integer(value) else value
}

.scmix_v24_forbidden <- function(x) {
  nm <- names(x)
  if (is.null(nm)) return(character())
  key <- tolower(gsub("[^[:alnum:]]", "", nm))
  nm[key %in% .scmix_v24_forbidden_names]
}

.scmix_v24_bool <- function(x, name) {
  value <- tolower(as.character(x))
  if (length(value) != 1L || is.na(value) ||
      !value %in% c("true", "false")) {
    stop("Frozen v2.4 field `", name, "` is malformed.", call. = FALSE)
  }
  identical(value, "true")
}

.scmix_v24_freeze_manifest_file <- function() {
  name <- "inference-v2.4-freeze-manifest.dcf"
  installed <- system.file("protocol", name, package = "sconjoint")
  source_root <- Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = "")
  candidates <- unique(c(
    installed, file.path(getwd(), "inst", "protocol", name),
    file.path(source_root, "inst", "protocol", name)))
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("Cannot locate the v2.4 freeze manifest.",
                           call. = FALSE)
  normalizePath(found[[1L]], mustWork = TRUE)
}

.scmix_v24_source_file <- function() {
  source_root <- Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = "")
  installed <- system.file(
    "calibration", "v24", "source", "calibration-v24.R",
    package = "sconjoint")
  candidates <- unique(c(
    file.path(getwd(), "R", "calibration-v24.R"),
    file.path(source_root, "R", "calibration-v24.R"), installed))
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("Cannot locate the hash-bound v2.4 source file.",
                           call. = FALSE)
  normalizePath(found[[1L]], mustWork = TRUE)
}

.scmix_v24_verify_freeze <- function(paths) {
  manifest_path <- .scmix_v24_freeze_manifest_file()
  manifest <- as.list(read.dcf(manifest_path)[1L, , drop = TRUE])
  if (!identical(manifest[["Manifest-Id"]],
                 "paperps-v2.4-freeze-2026-09-06-v3") ||
      !identical(manifest[["Protocol-Id"]], .scmix_v24_protocol_id) ||
      !identical(manifest[["Plan-Id"]], .scmix_v24_plan_id) ||
      !identical(manifest[["Pre-Outcome-Amended-On"]], "2026-09-07") ||
      !grepl("evaluator_implementation_completed_and_hash_contract_added",
             manifest[["Pre-Outcome-Amendment"]], fixed = TRUE) ||
      !identical(manifest[["Outcome-Count"]], "0") ||
      !identical(manifest[["Integrity-Pilot-Status"]], "not_run") ||
      !identical(manifest[["Evidence-Status"]], "not_run") ||
      !identical(manifest[["Evaluator-Approval-Status"]], "absent") ||
      !identical(manifest[["Approval-Status"]], "absent")) {
    stop("The v2.4 freeze manifest has an invalid identity or status.",
         call. = FALSE)
  }
  field <- c(
    plan = "Plan-SHA256", designs = "Design-Manifest-SHA256",
    cells = "Cell-Registry-SHA256", targets = "Target-Registry-SHA256",
    decisions = "Decision-Registry-SHA256",
    approval = "Approval-Matrix-SHA256",
    fit_regularity = "Fit-Regularity-Registry-SHA256",
    truths = "Truths-SHA256")
  observed <- vapply(names(field), function(name)
    unname(as.character(tools::sha256sum(paths[[name]]))), character(1L))
  expected <- vapply(field, function(name) manifest[[name]], character(1L))
  if (!identical(unname(observed), unname(expected))) {
    stop("A hash-bound v2.4 plan, registry, design, or truth file changed.",
         call. = FALSE)
  }
  source_hash <- unname(as.character(tools::sha256sum(
    .scmix_v24_source_file())))
  if (!identical(source_hash, manifest[["Source-Code-SHA256"]])) {
    stop("The hash-bound v2.4 source code changed.", call. = FALSE)
  }
  source_root <- Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = "")
  locate <- function(candidates, label, required = TRUE) {
    candidates <- unique(candidates[nzchar(candidates)])
    found <- candidates[file.exists(candidates)]
    if (!length(found)) {
      if (isTRUE(required)) {
        stop("Cannot locate the hash-bound v2.4 ", label, ".",
             call. = FALSE)
      }
      return(NA_character_)
    }
    normalizePath(found[[1L]], mustWork = TRUE)
  }
  companion <- c(
    protocol_markdown = locate(c(
      file.path(getwd(), "INFERENCE_PROTOCOL_V24.md"),
      file.path(source_root, "INFERENCE_PROTOCOL_V24.md")),
      "protocol Markdown", required = FALSE),
    yaml = locate(c(
      system.file("protocol", "inference-v2.4.yaml", package = "sconjoint"),
      file.path(getwd(), "inst", "protocol", "inference-v2.4.yaml"),
      file.path(source_root, "inst", "protocol", "inference-v2.4.yaml")),
      "protocol YAML"),
    readme = locate(c(
      system.file("calibration", "v24", "README.md", package = "sconjoint"),
      file.path(getwd(), "inst", "calibration", "v24", "README.md"),
      file.path(source_root, "inst", "calibration", "v24", "README.md")),
      "calibration README"),
    source_mirror = locate(c(
      system.file("calibration", "v24", "source", "calibration-v24.R",
                  package = "sconjoint"),
      file.path(getwd(), "inst", "calibration", "v24", "source",
                "calibration-v24.R"),
      file.path(source_root, "inst", "calibration", "v24", "source",
                "calibration-v24.R")), "source mirror"))
  companion_field <- c(
    protocol_markdown = "Protocol-Markdown-SHA256",
    yaml = "Protocol-YAML-SHA256", readme = "Readme-SHA256",
    source_mirror = "Source-Mirror-SHA256")
  available <- !is.na(companion)
  companion_hash <- vapply(companion[available], function(path)
    unname(as.character(tools::sha256sum(path))), character(1L))
  companion_expected <- vapply(companion_field[available], function(field)
    manifest[[field]], character(1L))
  if (!identical(unname(companion_hash), unname(companion_expected))) {
    stop("A hash-bound v2.4 protocol companion or source mirror changed.",
         call. = FALSE)
  }
  list(
    path = manifest_path,
    sha256 = unname(as.character(tools::sha256sum(manifest_path))),
    fields = manifest, source_sha256 = source_hash,
    companion_sha256 = companion_hash)
}

## Stable outcome-free ordering key.  Its arithmetic stays below 2^53, so the
## result is exact under every IEEE-754 double implementation supported by R.
.scmix_v24_id_key <- function(id, salt) {
  one <- function(text, multiplier, initial, modulus) {
    value <- as.double(initial)
    for (byte in utf8ToInt(enc2utf8(text))) {
      value <- (value * multiplier + byte + 1) %% modulus
    }
    sprintf("%010.0f", value)
  }
  text <- paste0(salt, "|", as.character(id))
  paste0(one(text, 131, 104729, 2147483629), "-",
         one(text, 257, 130363, 2147483587), "-", as.character(id))
}

.scmix_v24_hamilton_allocation <- function(stratum, target_n) {
  counts <- table(stratum)
  if (target_n > sum(counts)) {
    stop("The requested v2.4 subset exceeds the available respondents.",
         call. = FALSE)
  }
  if (target_n < length(counts)) {
    stop("The requested v2.4 subset cannot retain every nonempty stratum.",
         call. = FALSE)
  }
  allocation <- rep.int(1L, length(counts))
  names(allocation) <- names(counts)
  capacity <- as.integer(counts) - allocation
  remaining <- as.integer(target_n - sum(allocation))
  if (remaining > 0L) {
    raw <- remaining * capacity / sum(capacity)
    add <- pmin(capacity, floor(raw))
    allocation <- allocation + add
    left <- target_n - sum(allocation)
    if (left > 0L) {
      order_fraction <- order(-(raw - floor(raw)), names(counts))
      for (j in order_fraction) {
        if (!left) break
        if (allocation[[j]] < counts[[j]]) {
          allocation[[j]] <- allocation[[j]] + 1L
          left <- left - 1L
        }
      }
    }
  }
  if (sum(allocation) != target_n || any(allocation < 1L) ||
      any(allocation > counts)) {
    stop("Internal error in the frozen v2.4 Hamilton allocation.",
         call. = FALSE)
  }
  stats::setNames(as.integer(allocation), names(counts))
}

.scmix_v24_subset_selection <- function(template, target_n = 400L,
                                        salt = NULL) {
  .scmix_v23_step3_check_design(template, require_frozen = TRUE)
  if (!inherits(template, "scmix_v23_design_template") ||
      !isTRUE(template$response_free) ||
      length(template$outcome_fields_stored) ||
      length(.scmix_v24_forbidden(template))) {
    stop("An intact response-free v2.3 source template is required.",
         call. = FALSE)
  }
  target_n <- .scmix_v24_number(target_n, "Subset-N", integer = TRUE)
  respondent_id <- as.character(template$respondent_id)
  ids <- unique(respondent_id)
  group <- match(respondent_id, ids)
  counts <- tabulate(group, nbins = length(ids))
  Z <- as.matrix(template$Z)
  first <- match(ids, respondent_id)
  Z_resp <- Z[first, , drop = FALSE]
  scale <- apply(Z_resp, 2L, stats::sd)
  varying <- which(is.finite(scale) & scale > sqrt(.Machine$double.eps))
  if (!length(varying)) {
    stop("The v2.4 subset rule requires one varying moderator.",
         call. = FALSE)
  }
  z <- as.numeric((Z_resp[, varying[[1L]]] -
                     mean(Z_resp[, varying[[1L]]])) / scale[[varying[[1L]]]])
  high <- z > 0
  stratum <- sprintf("Ti%04d_%s", counts, ifelse(high, "high", "low"))
  allocation <- .scmix_v24_hamilton_allocation(stratum, target_n)
  salt <- as.character(salt %||% paste0("v24|", template$design_id))
  selected <- unlist(lapply(names(allocation), function(level) {
    members <- ids[stratum == level]
    key <- vapply(members, .scmix_v24_id_key, character(1L), salt = salt)
    members[order(key)][seq_len(allocation[[level]])]
  }), use.names = FALSE)
  ## The selected template retains source respondent order.  The selection
  ## itself is hash-randomized within frozen strata.
  selected <- ids[ids %in% selected]
  if (length(selected) != target_n || anyDuplicated(selected)) {
    stop("The frozen v2.4 subset rule did not return exactly 400 respondents.",
         call. = FALSE)
  }
  list(
    selected_ids = selected,
    selection_hash = .scmix_v24_sha256(selected),
    stratum = stratum[match(selected, ids)],
    stratum_allocation = allocation,
    moderator_column = varying[[1L]],
    moderator_name = colnames(Z)[varying[[1L]]] %||%
      paste0("Z", varying[[1L]]),
    high_count = sum(high[match(selected, ids)]),
    low_count = sum(!high[match(selected, ids)]),
    algorithm = .scmix_v24_subset_algorithm,
    salt = salt)
}

## Read and validate the prospective design without opening the outcome lock.
scmix_v24_validation_plan <- function() {
  paths <- vapply(names(.scmix_v24_file_names), .scmix_v24_file,
                  character(1L))
  raw <- as.list(read.dcf(paths[["plan"]])[1L, , drop = TRUE])
  tables <- lapply(paths[names(paths) != "plan"], utils::read.csv,
                   stringsAsFactors = FALSE, check.names = FALSE)
  if (!identical(raw[["Plan-Id"]], .scmix_v24_plan_id) ||
      !identical(raw[["Protocol-Id"]], .scmix_v24_protocol_id) ||
      !identical(raw[["Status"]],
                 "prospective_validation_design_frozen_outcomes_locked") ||
      !identical(raw[["Threshold-Selection"]], "none") ||
      !identical(raw[["Calibration-Stream"]], "none") ||
      !identical(raw[["Pre-Outcome-Amended-On"]], "2026-09-07") ||
      !grepl("directional_heterogeneity_primitive_metadata",
             raw[["Pre-Outcome-Amendment"]], fixed = TRUE) ||
      .scmix_v24_bool(raw[["Application-Outcomes-Permitted"]],
                       "Application-Outcomes-Permitted")) {
    stop("The installed v2.4 plan is not the frozen outcome-locked plan.",
         call. = FALSE)
  }
  counts <- list(
    subset_n = .scmix_v24_number(raw[["Respondents-Per-Template"]],
                                 "Respondents-Per-Template", TRUE),
    pilot_reps = .scmix_v24_number(raw[["Integrity-Pilot-Reps-Per-Cell"]],
                                   "Integrity-Pilot-Reps-Per-Cell", TRUE),
    evidence_reps = .scmix_v24_number(raw[["Evidence-Reps-Per-Cell"]],
                                      "Evidence-Reps-Per-Cell", TRUE),
    batch_size = .scmix_v24_number(raw[["Evidence-Batch-Size"]],
                                   "Evidence-Batch-Size", TRUE),
    outer_folds = .scmix_v24_number(raw[["Outer-Folds"]],
                                    "Outer-Folds", TRUE),
    inner_folds = .scmix_v24_number(raw[["Inner-Folds"]],
                                    "Inner-Folds", TRUE),
    starts = .scmix_v24_number(raw[["Optimizer-Starts"]],
                               "Optimizer-Starts", TRUE),
    gh_nodes = .scmix_v24_number(raw[["Reference-GH-Nodes"]],
                                 "Reference-GH-Nodes", TRUE),
    refined_gh_nodes = .scmix_v24_number(raw[["Refined-GH-Nodes"]],
                                         "Refined-GH-Nodes", TRUE),
    multipliers = .scmix_v24_number(raw[["Common-Respondent-Multipliers"]],
                                    "Common-Respondent-Multipliers", TRUE),
    maximum_workers = .scmix_v24_number(raw[["Maximum-Workers"]],
                                        "Maximum-Workers", TRUE),
    maximum_projected_hours = .scmix_v24_number(
      raw[["Maximum-Projected-Evidence-Wall-Hours"]],
      "Maximum-Projected-Evidence-Wall-Hours", TRUE))
  if (!identical(unname(unlist(counts)),
                 c(400L, 2L, 100L, 10L, 2L, 2L, 2L, 15L, 31L, 9999L,
                   6L, 8L))) {
    stop("The lean v2.4 computational design changed.", call. = FALSE)
  }
  if (!identical(.scmix_v24_number(raw[["Familywise-Confidence-Level"]],
                                    "Familywise-Confidence-Level"), 0.95) ||
      !identical(raw[["Multiplier-Critical-Value"]],
                 paste0("empirical_inverse_ECDF_type1_quantile_at_0.95_",
                        "rank_ceiling_0.95_times_9999_no_interpolation")) ||
      !grepl("maximum_absolute_studentized_multiplier_statistic",
             raw[["Multiplier-Max-Statistic"]], fixed = TRUE) ||
      !grepl("two_sided_Wilson90_interval_using_z_equals_qnorm_0.95",
             raw[["Coverage-MC-Summary"]], fixed = TRUE) ||
      !grepl("one_sided_Clopper_Pearson_qbeta_0.95",
             raw[["Boundary-Exact-Upper-Rule"]], fixed = TRUE)) {
    stop("The frozen v2.4 simultaneous-inference convention changed.",
         call. = FALSE)
  }
  cells <- tables$cells
  required_cells <- c(
    "br_regular", "gs_regular", "sw_regular",
    "br_probability_zero_denominator_boundary", "br_q0_rank_boundary",
    "gs_weak_inverse")
  if (!identical(cells$cell_id, required_cells) || nrow(cells) != 6L ||
      any(cells$fit_rank != 1L) || any(cells$replications != 100L) ||
      anyDuplicated(cells$cell_id)) {
    stop("The frozen six-cell v2.4 registry changed.", call. = FALSE)
  }
  designs <- tables$designs
  if (!identical(designs$design_id, c("br2017", "gs2020", "sw2022")) ||
      any(designs$subset_n != 400L) ||
      any(designs$subset_algorithm != .scmix_v24_subset_algorithm) ||
      any(!grepl("^[[:xdigit:]]{64}$", designs$source_template_sha256)) ||
      any(!grepl("^[[:xdigit:]]{64}$", designs$selection_hash)) ||
      any(!grepl("^[[:xdigit:]]{64}$", designs$subset_payload_hash))) {
    stop("The frozen v2.4 design manifest is malformed.", call. = FALSE)
  }
  source_plan <- scmix_v23_calibration_plan()
  source_index <- match(designs$design_id, source_plan$designs$design_id)
  if (anyNA(source_index) || !identical(
      designs$source_template_sha256,
      source_plan$designs$template_file_sha256[source_index])) {
    stop("The v2.4 design manifest is not bound to the frozen v2.3 wrappers.",
         call. = FALSE)
  }
  required_targets <- c(
    "smooth_means", "choice_probabilities", "directional_shares",
    "subgroup_ratios", "directional_heterogeneity", "mrs")
  if (!identical(tables$targets$family_id, required_targets) ||
      anyDuplicated(tables$targets$family_id) ||
      !all(c("target_definition", "contrast_rule",
             "reference_weight_rule", "primitive_rule") %in%
           names(tables$targets)) ||
      any(!nzchar(tables$targets$target_definition)) ||
      any(!grepl("frozen_original_template",
                 tables$targets$reference_weight_rule, fixed = TRUE)) ||
      !identical(tables$targets$primitive_rule[
        tables$targets$family_id == "directional_heterogeneity"],
        paste0("joint_H_Z_and_H_R_with_directional_mean_and_second_moment_",
               "retained_as_supporting_audit_columns"))) {
    stop("The frozen v2.4 target-family registry changed.", call. = FALSE)
  }
  if (!identical(
      raw[["Respondent-Superpopulation-Resampling"]],
      paste0("sample_400_complete_respondent_records_with_replacement_",
             "from_the_frozen_template_preserve_each_sequence_and_",
             "assign_new_replication_ids")) ||
      !identical(raw[["Truth-Conditioning"]],
                 "original_frozen_400_respondent_empirical_distribution")) {
    stop("The frozen v2.4 respondent-resampling contract changed.",
         call. = FALSE)
  }
  required_cell_columns <- c("exact_boundary_families",
                             "weak_or_bounded_families")
  if (!all(required_cell_columns %in% names(cells)) ||
      !identical(cells$exact_boundary_families,
                 c("none", "none", "none", "mrs", "all", "none")) ||
      !identical(cells$weak_or_bounded_families,
                 c("none", "none", "none",
                   "choice_probabilities|subgroup_ratios", "none",
                   "smooth_means|directional_shares|directional_heterogeneity"))) {
    stop("The v2.4 exact and weak boundary-family map changed.",
         call. = FALSE)
  }
  if (nrow(tables$fit_regularity) != 1L ||
      !identical(tables$fit_regularity$primitive_id,
                 "active_q1_covariance_eigenvalue") ||
      !identical(tables$fit_regularity$definition,
                 "trace_Sigma_for_q1") ||
      !identical(tables$fit_regularity$reporting_role,
                 "fit_regularity_not_reported_target_family")) {
    stop("The v2.4 fit-regularity primitive registry changed.",
         call. = FALSE)
  }
  required_rules <- c(
    "regularity_z", "chart_nonlinear_discrepancy", "riesz_defect",
    "ridge_movement", "block_kkt", "nested_objective",
    "near_optimum_objective", "near_optimum_target_spread",
    "integration_target", "integration_if", "loading_invariance",
    "derivative", "null_space", "svd_rank", "all_cell_completion",
    "regular_release", "regular_coverage", "boundary_false_release",
    "weak_release_low", "weak_release_high", "pilot_runtime",
    "coverage_mc_uncertainty")
  if (!identical(tables$decisions$rule_id, required_rules) ||
      anyDuplicated(tables$decisions$rule_id)) {
    stop("The fixed v2.4 decision registry changed.", call. = FALSE)
  }
  approval <- tables$approval
  expected_approval <- do.call(rbind, lapply(required_targets, function(family) {
    criterion <- rep("regular", length(required_cells))
    required <- rep(TRUE, length(required_cells))
    criterion[required_cells == "br_q0_rank_boundary"] <- "exact_boundary"
    if (family %in% c("choice_probabilities", "subgroup_ratios")) {
      criterion[required_cells ==
                  "br_probability_zero_denominator_boundary"] <-
        "weak_or_bounded"
    } else if (identical(family, "mrs")) {
      criterion[required_cells ==
                  "br_probability_zero_denominator_boundary"] <-
        "exact_boundary"
    } else {
      criterion[required_cells ==
                  "br_probability_zero_denominator_boundary"] <-
        "descriptive_only"
      required[required_cells ==
                 "br_probability_zero_denominator_boundary"] <- FALSE
    }
    if (family %in% c("smooth_means", "directional_shares",
                      "directional_heterogeneity")) {
      criterion[required_cells == "gs_weak_inverse"] <- "weak_or_bounded"
    } else {
      criterion[required_cells == "gs_weak_inverse"] <- "descriptive_only"
      required[required_cells == "gs_weak_inverse"] <- FALSE
    }
    data.frame(family_id = family, cell_id = required_cells,
               required = required, criterion = criterion,
               stringsAsFactors = FALSE)
  }))
  observed_approval <- approval[, c("family_id", "cell_id", "required",
                                    "criterion"), drop = FALSE]
  if (nrow(approval) != 36L || sum(approval$required) != 30L || anyDuplicated(
      paste(approval$family_id, approval$cell_id, sep = "\r")) ||
      !identical(observed_approval, expected_approval) ||
      !all(approval$release_definition == raw[["Family-Release-Definition"]])) {
    stop("The deterministic v2.4 approval matrix changed.", call. = FALSE)
  }
  truths <- tables$truths
  target_order <- c(
    "mean_direction", "mean_numerator", "mean_denominator",
    "choice_probability", "preference_direction_share",
    "compensating_benefit_share", "subgroup_high_choice_share",
    "subgroup_low_choice_share", "heterogeneity_explained_variance",
    "heterogeneity_residual_variance", "heterogeneity_explained_share",
    "mrs", "active_q1_covariance_eigenvalue")
  expected_truth_order <- unlist(lapply(required_cells, function(cell)
    paste(cell, target_order, sep = "\r")), use.names = FALSE)
  observed_truth_order <- paste(truths$cell_id, truths$target_id, sep = "\r")
  truth_hash <- unname(as.character(tools::sha256sum(paths[["truths"]])))
  if (nrow(truths) != 78L ||
      !identical(observed_truth_order, expected_truth_order) ||
      anyDuplicated(observed_truth_order) ||
      any(truths$application_outcomes_used != FALSE) ||
      !all(truths$status %in% c("defined", "undefined_exact_boundary")) ||
      !identical(raw[["Target-Truths-SHA256"]], truth_hash) ||
      !identical(raw[["Truth-Normal-Nodes"]], "257")) {
    stop("The frozen response-free v2.4 target truths changed.",
         call. = FALSE)
  }
  forbidden <- c(
    "inference-v2.4-calibration-values.dcf",
    "inference-v2.4-calibration.dcf")
  protocol_dir <- dirname(paths[["plan"]])
  if (any(file.exists(file.path(protocol_dir, forbidden)))) {
    stop("A prohibited v2.4 calibration-values file exists.",
         call. = FALSE)
  }
  freeze <- .scmix_v24_verify_freeze(paths)
  .scmix_v24_sign(list(
    plan_id = raw[["Plan-Id"]], protocol_id = raw[["Protocol-Id"]],
    status = raw[["Status"]], raw = raw, counts = counts,
    designs = designs, cells = cells, targets = tables$targets,
    decisions = tables$decisions,
    approval = approval,
    fit_regularity = tables$fit_regularity, truths = truths, files = paths,
    freeze = freeze,
    application_outcomes_permitted = FALSE,
    threshold_selection = "none", calibration_stream = "none"),
    "scmix_v24_validation_plan")
}

scmix_v24_load_design <- function(design_id,
                                  plan = scmix_v24_validation_plan()) {
  if (!.scmix_v24_intact(plan, "scmix_v24_validation_plan")) {
    stop("An intact frozen v2.4 plan is required.", call. = FALSE)
  }
  row <- plan$designs[plan$designs$design_id == design_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("`design_id` is not in the frozen v2.4 manifest.", call. = FALSE)
  }
  source <- scmix_v23_load_frozen_design(design_id)
  source_plan <- scmix_v23_calibration_plan()
  source_row <- source_plan$designs[
    source_plan$designs$design_id == design_id, , drop = FALSE]
  if (nrow(source_row) != 1L || !identical(
      row$source_template_sha256[[1L]],
      source_row$template_file_sha256[[1L]])) {
    stop("The v2.4 source-wrapper binding is not intact.", call. = FALSE)
  }
  selection <- .scmix_v24_subset_selection(
    source, target_n = row$subset_n[[1L]], salt = row$selection_salt[[1L]])
  if (!identical(selection$selection_hash, row$selection_hash[[1L]])) {
    stop("The derived v2.4 respondent selection failed its frozen hash.",
         call. = FALSE)
  }
  keep <- source$respondent_id %in% selection$selected_ids
  payload <- list(
    deltaX = unname(source$deltaX[keep, , drop = FALSE]),
    deltaX_names = source$deltaX_names,
    Z = unname(source$Z[keep, , drop = FALSE]),
    Z_names = source$Z_names,
    respondent_id = as.character(source$respondent_id[keep]),
    task_id = as.character(source$task_id[keep]),
    position = if (is.null(source$position)) NULL else source$position[keep])
  payload_hash <- .scmix_v24_sha256(payload)
  if (!identical(payload_hash, row$subset_payload_hash[[1L]])) {
    stop("The derived v2.4 response-free payload failed its frozen hash.",
         call. = FALSE)
  }
  .scmix_v24_sign(c(payload, list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    design_id = design_id, source_protocol_id = source$protocol_id,
    source_template_sha256 = row$source_template_sha256[[1L]],
    response_free = TRUE, outcome_fields_stored = character(),
    manifest_verified = TRUE, subset_n = nrow(unique(data.frame(
      respondent_id = payload$respondent_id))),
    selection = selection, subset_payload_hash = payload_hash)),
    "scmix_v24_design_template")
}

.scmix_v24_weighted_gram <- function(X, respondent_id) {
  ids <- unique(as.character(respondent_id))
  group <- match(as.character(respondent_id), ids)
  count <- tabulate(group, nbins = length(ids))
  weight <- 1 / (length(ids) * count[group])
  crossprod(X * sqrt(weight))
}

.scmix_v24_fix_sign <- function(v) {
  j <- which(abs(v) > sqrt(.Machine$double.eps))[1L]
  if (length(j) && is.finite(j) && v[[j]] < 0) -v else v
}

.scmix_v24_pc_index <- function(x, p) {
  if (identical(as.character(x), "last")) return(p)
  value <- suppressWarnings(as.integer(x))
  if (length(value) != 1L || is.na(value) || value < 1L || value > p) {
    stop("A frozen v2.4 PC index is invalid.", call. = FALSE)
  }
  value
}

## Exact q=1 DGP geometry in the estimator's uncentered respondent-RMS scale.
## No outcomes are generated here.
scmix_v24_dgp_geometry <- function(
    cell_id, plan = scmix_v24_validation_plan()) {
  if (!.scmix_v24_intact(plan, "scmix_v24_validation_plan")) {
    stop("An intact frozen v2.4 plan is required.", call. = FALSE)
  }
  cell <- plan$cells[plan$cells$cell_id == cell_id, , drop = FALSE]
  if (nrow(cell) != 1L) {
    stop("`cell_id` is not in the frozen v2.4 cell registry.",
         call. = FALSE)
  }
  template <- scmix_v24_load_design(cell$design_id[[1L]], plan)
  transform <- .sc_comp_fit_dx_scale(
    template$deltaX, template$respondent_id)
  if (!identical(transform$centering, "none")) {
    stop("The v2.4 DGP requires uncentered contrast scaling.",
         call. = FALSE)
  }
  S <- as.numeric(transform$scale)
  X_internal <- sweep(template$deltaX, 2L, S, `/`)
  gram <- .scmix_v24_weighted_gram(X_internal, template$respondent_id)
  eig <- eigen(gram, symmetric = TRUE)
  vectors <- apply(eig$vectors, 2L, .scmix_v24_fix_sign)
  if (is.null(dim(vectors))) vectors <- matrix(vectors, ncol = 1L)
  values <- pmax(as.numeric(eig$values), 0)
  rank_tolerance <- max(dim(gram)) * .Machine$double.eps * max(values)
  p <- ncol(X_internal)
  index <- c(
    direction = .scmix_v24_pc_index(cell$direction_pc[[1L]], p),
    numerator = .scmix_v24_pc_index(cell$numerator_pc[[1L]], p),
    denominator = .scmix_v24_pc_index(cell$denominator_pc[[1L]], p))
  if (any(values[index] <= rank_tolerance) || anyDuplicated(index)) {
    stop("The selected v2.4 DGP directions are not distinct positive directions.",
         call. = FALSE)
  }
  weak <- identical(cell$geometry[[1L]], "weak_unstandardized_last_pc")
  coefficient_basis_raw <- matrix(NA_real_, p, 3L,
                                  dimnames = list(template$deltaX_names,
                                                  names(index)))
  target_dual_raw <- coefficient_basis_raw
  for (j in seq_along(index)) {
    k <- index[[j]]
    is_weak_direction <- weak && names(index)[[j]] == "direction"
    normalizer <- if (is_weak_direction) 1 else sqrt(values[[k]])
    coefficient_basis_raw[, j] <- vectors[, k] / (S * normalizer)
    target_dual_raw[, j] <- S * vectors[, k] * normalizer
  }
  duality <- crossprod(target_dual_raw, coefficient_basis_raw)
  if (max(abs(duality - diag(3L))) > 1e-10) {
    stop("The v2.4 coefficient bases and target contrasts are not exact duals.",
         call. = FALSE)
  }
  z_transform <- .sc_fit_z_transform(template$Z, template$respondent_id)
  varying <- which(!z_transform$constant)
  if (!length(varying)) stop("The v2.4 DGP requires one varying moderator.",
                             call. = FALSE)
  z_task <- .sc_apply_z_transform(template$Z, z_transform)[, varying[[1L]]]
  theta0 <- c(
    direction = cell$direction_intercept[[1L]],
    numerator = cell$numerator_intercept[[1L]],
    denominator = cell$denominator_intercept[[1L]])
  theta_z <- c(
    direction = cell$direction_moderator[[1L]],
    numerator = cell$numerator_moderator[[1L]],
    denominator = cell$denominator_moderator[[1L]])
  beta0_raw <- drop(coefficient_basis_raw %*% theta0)
  beta_z_raw <- drop(coefficient_basis_raw %*% theta_z)
  loading_raw <- coefficient_basis_raw[, "direction"] *
    cell$loading_sd[[1L]]
  beta0_internal <- S * beta0_raw
  beta_z_internal <- S * beta_z_raw
  loading_internal <- S * loading_raw
  fixed_raw <- cell$kappa[[1L]] +
    drop(template$deltaX %*% beta0_raw) +
    drop(template$deltaX %*% beta_z_raw) * z_task
  fixed_internal <- cell$kappa[[1L]] +
    drop(X_internal %*% beta0_internal) +
    drop(X_internal %*% beta_z_internal) * z_task
  random_raw <- drop(template$deltaX %*% loading_raw)
  random_internal <- drop(X_internal %*% loading_internal)
  equality_error <- max(abs(c(fixed_raw - fixed_internal,
                              random_raw - random_internal)))
  if (!is.finite(equality_error) || equality_error > 1e-12) {
    stop("Raw and estimator-scale v2.4 utility indices are not invariant.",
         call. = FALSE)
  }
  direction_score <- drop(template$deltaX %*%
                            coefficient_basis_raw[, "direction"])
  direction_score_rms <- sqrt(.scmix_v24_weighted_gram(
    matrix(direction_score, ncol = 1L), template$respondent_id)[1L, 1L])
  expected_direction_rms <- if (weak) sqrt(values[index[["direction"]]]) else 1
  .scmix_v24_sign(list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    cell_id = cell_id, design_id = cell$design_id[[1L]], fit_rank = 1L,
    dgp_rank = as.integer(cell$dgp_rank[[1L]]),
    response_free = TRUE, application_outcomes_used = FALSE,
    centering = "none", respondent_weighted_rms = S,
    X_internal = X_internal, z_task = z_task,
    gram = gram, eigenvalues = values, eigenvectors = vectors,
    rank_tolerance = rank_tolerance, pc_index = index,
    geometry = cell$geometry[[1L]],
    coefficient_basis_raw = coefficient_basis_raw,
    target_dual_raw = target_dual_raw, duality = duality,
    beta0_raw = beta0_raw, beta_z_raw = beta_z_raw,
    loading_raw = loading_raw, beta0_internal = beta0_internal,
    beta_z_internal = beta_z_internal,
    loading_internal = loading_internal, kappa = cell$kappa[[1L]],
    fixed_index = fixed_raw, random_score = random_raw,
    scale_invariance_error = equality_error,
    direction_score_rms = direction_score_rms,
    expected_direction_score_rms = expected_direction_rms,
    residual_distribution = "standard_normal_persistent_respondent_draw"),
    "scmix_v24_dgp_geometry")
}

.scmix_v24_truth_class <- function(cell, family) {
  exact <- strsplit(cell$exact_boundary_families[[1L]], "|", fixed = TRUE)[[1L]]
  weak <- strsplit(cell$weak_or_bounded_families[[1L]], "|", fixed = TRUE)[[1L]]
  if ("all" %in% exact || family %in% exact) return("exact_boundary")
  if ("all" %in% weak || family %in% weak) return("weak_or_bounded")
  "regular"
}

.scmix_v24_respondent_mean <- function(x, respondent_id) {
  ids <- unique(as.character(respondent_id))
  group <- match(as.character(respondent_id), ids)
  drop(rowsum(as.numeric(x), group, reorder = FALSE) /
         tabulate(group, nbins = length(ids)))
}

## Compute every v2.4 truth from a response-free template and the exact frozen
## DGP.  This function integrates probabilities but generates no choices.
scmix_v24_target_truths <- function(plan = scmix_v24_validation_plan(),
                                    normal_nodes = 257L) {
  if (!.scmix_v24_intact(plan, "scmix_v24_validation_plan")) {
    stop("An intact frozen v2.4 plan is required.", call. = FALSE)
  }
  normal_nodes <- .scmix_v24_number(normal_nodes, "Truth-Normal-Nodes", TRUE)
  if (normal_nodes != 257L) {
    stop("The frozen v2.4 truth rule uses exactly 257 normal GH nodes.",
         call. = FALSE)
  }
  grid <- .sc_gh_grid(1L, n_nodes = normal_nodes)
  out <- vector("list", nrow(plan$cells))
  for (i in seq_len(nrow(plan$cells))) {
    cell <- plan$cells[i, , drop = FALSE]
    geometry <- scmix_v24_dgp_geometry(cell$cell_id[[1L]], plan)
    template <- scmix_v24_load_design(cell$design_id[[1L]], plan)
    probability <- drop(stats::plogis(outer(
      geometry$fixed_index, grid$U[, 1L],
      function(a, u) a + geometry$random_score * u)) %*% grid$w)
    probability_i <- .scmix_v24_respondent_mean(
      probability, template$respondent_id)
    ids <- unique(template$respondent_id)
    first <- match(ids, template$respondent_id)
    z_i <- geometry$z_task[first]
    high <- z_i > 0
    direction_mean <- cell$direction_intercept[[1L]] +
      cell$direction_moderator[[1L]] * z_i
    denominator_mean <- cell$denominator_intercept[[1L]] +
      cell$denominator_moderator[[1L]] * z_i
    loading_sd <- cell$loading_sd[[1L]]
    direction_share_i <- if (loading_sd > 0) {
      stats::pnorm(direction_mean / loading_sd)
    } else as.numeric(direction_mean > 0)
    compensating_mean <- direction_mean + 0.5 * denominator_mean
    compensating_share_i <- if (loading_sd > 0) {
      stats::pnorm(compensating_mean / loading_sd)
    } else as.numeric(compensating_mean >= 0)
    explained <- mean((cell$direction_moderator[[1L]] * z_i -
                         mean(cell$direction_moderator[[1L]] * z_i))^2)
    residual <- loading_sd^2
    numerator_truth <- cell$numerator_intercept[[1L]] +
      cell$numerator_moderator[[1L]] * mean(z_i)
    denominator_truth <- mean(denominator_mean)
    mrs_truth <- if (abs(denominator_truth) <=
                     sqrt(.Machine$double.eps)) NA_real_ else
      -numerator_truth / denominator_truth
    value <- c(
      mean_direction = mean(direction_mean),
      mean_numerator = numerator_truth,
      mean_denominator = denominator_truth,
      choice_probability = mean(probability_i),
      preference_direction_share = mean(direction_share_i),
      compensating_benefit_share = mean(compensating_share_i),
      subgroup_high_choice_share = mean(probability_i[high]),
      subgroup_low_choice_share = mean(probability_i[!high]),
      heterogeneity_explained_variance = explained,
      heterogeneity_residual_variance = residual,
      heterogeneity_explained_share = if (explained + residual > 0)
        explained / (explained + residual) else NA_real_,
      mrs = mrs_truth,
      active_q1_covariance_eigenvalue = sum(geometry$loading_raw^2))
    family <- c(
      rep("smooth_means", 3L), "choice_probabilities",
      rep("directional_shares", 2L), rep("subgroup_ratios", 2L),
      rep("directional_heterogeneity", 3L), "mrs", "fit_regularity")
    status <- rep("defined", length(value))
    status[is.na(value)] <- "undefined_exact_boundary"
    out[[i]] <- data.frame(
      cell_index = i, cell_id = cell$cell_id[[1L]],
      design_id = cell$design_id[[1L]], family_id = family,
      target_id = names(value), truth = unname(value), status = status,
      validation_class = vapply(
        family, function(x) .scmix_v24_truth_class(cell, x), character(1L)),
      truth_rule = "response_free_exact_dgp_GH257",
      application_outcomes_used = FALSE, stringsAsFactors = FALSE)
  }
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

.scmix_v24_evaluator_approval_path <- function(plan) {
  file.path(dirname(plan$files[["plan"]]),
            "inference-v2.4-evaluator-approval.dcf")
}

scmix_v24_readiness <- function(plan = scmix_v24_validation_plan()) {
  .scmix_v24_evaluator_readiness(plan = plan)
}

## The generator is implemented in the evaluator state machine.  It remains
## fail-closed until one complete evaluator and its exact interface are
## hash-bound in the pre-outcome approval DCF.
scmix_v24_generate_study_outcome <- function(...) {
  .scmix_v24_evaluator_generate_study_outcome(...)
}
