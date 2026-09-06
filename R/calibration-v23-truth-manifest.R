## Response-free target truths and design diagnostics frozen before Step 3.

.scmix_v23_truth_files <- c(
  manifest = "inference-v2.3-step3-truth-manifest.dcf",
  target_truths = "inference-v2.3-step3-target-truths.csv",
  design_diagnostics = "inference-v2.3-step3-design-diagnostics.csv")

.scmix_v23_truth_format_number <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.17g", x))
}

.scmix_v23_truth_csv_ready <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  for (name in names(x)) {
    if (is.numeric(x[[name]])) {
      x[[name]] <- .scmix_v23_truth_format_number(x[[name]])
    } else if (is.logical(x[[name]])) {
      x[[name]] <- ifelse(is.na(x[[name]]), "NA",
                          ifelse(x[[name]], "true", "false"))
    } else x[[name]] <- as.character(x[[name]])
  }
  x
}

.scmix_v23_truth_vector <- function(x) {
  paste(.scmix_v23_truth_format_number(as.numeric(x)), collapse = ";")
}

.scmix_v23_truth_loading_vector <- function(v, active, p) {
  out <- numeric(p)
  out[active] <- v
  .scmix_v23_truth_vector(out)
}

.scmix_v23_design_diagnostic_row <- function(template) {
  .scmix_v23_step3_check_design(template, require_frozen = TRUE)
  x <- as.matrix(template$deltaX)
  center <- colMeans(x)
  scale <- apply(x, 2L, stats::sd)
  active <- which(is.finite(scale) & scale > sqrt(.Machine$double.eps))
  if (length(active) < 3L) {
    stop("A frozen design has fewer than three active contrast columns.",
         call. = FALSE)
  }
  standardized <- sweep(x[, active, drop = FALSE], 2L, center[active], `-`)
  standardized <- sweep(standardized, 2L, scale[active], `/`)
  eig <- eigen(crossprod(standardized) / nrow(standardized), symmetric = TRUE)
  vectors <- apply(eig$vectors, 2L, .scmix_v23_oracle_sign)
  if (is.null(dim(vectors))) vectors <- matrix(vectors, ncol = 1L)
  prep <- .scmix_v23_oracle_prepare(template, require_frozen = TRUE)
  if (!isTRUE(all.equal(unname(pmax(eig$values, 0)),
                        unname(prep$eigenvalues), tolerance = 0))) {
    stop("Design diagnostics do not reproduce the frozen oracle eigenvalues.",
         call. = FALSE)
  }
  z <- prep$z_respondent
  data.frame(
    design_id = template$design_id,
    response_free_payload_hash =
      unname(template$hashes[["response_free_payload_hash"]]),
    n_task_rows = template$n_task_rows,
    n_respondents = template$n_respondents,
    p_deltaX = template$p_deltaX,
    active_deltaX_columns = paste(active, collapse = ";"),
    exact_Ti_histogram = template$ti_histogram,
    pc_eigenvalues_descending = .scmix_v23_truth_vector(pmax(eig$values, 0)),
    pc1_loading_original_coordinates = .scmix_v23_truth_loading_vector(
      vectors[, 1L], active, template$p_deltaX),
    pc2_loading_original_coordinates = .scmix_v23_truth_loading_vector(
      vectors[, 2L], active, template$p_deltaX),
    pc3_loading_original_coordinates = .scmix_v23_truth_loading_vector(
      vectors[, 3L], active, template$p_deltaX),
    weakest_pc_loading_original_coordinates = .scmix_v23_truth_loading_vector(
      vectors[, ncol(vectors)], active, template$p_deltaX),
    moderator_rule = paste0(
      "first_nonconstant_response_free_Z_column_at_respondent_level_",
      "centered_and_sample_sd_standardized"),
    subgroup_rule =
      "z_positive_after_standardization_vs_z_nonpositive",
    z_mean = mean(z), z_sd = stats::sd(z), z_min = min(z),
    z_q25 = unname(stats::quantile(z, .25, type = 8)),
    z_median = stats::median(z),
    z_q75 = unname(stats::quantile(z, .75, type = 8)), z_max = max(z),
    n_z_positive = sum(prep$subgroup),
    n_z_nonpositive = sum(!prep$subgroup),
    pc_sign_rule = "first_nonzero_loading_positive",
    standardization = "column_center_and_sample_sd_then_unit_SD_PC_scores",
    application_outcomes_used = FALSE,
    stringsAsFactors = FALSE)
}

.scmix_v23_build_truth_tables <- function(
    plan = scmix_v23_calibration_plan()) {
  if (!.scmix_v23_step3_intact(plan, "scmix_v23_calibration_plan")) {
    stop("`plan` must be the intact prospective v2.3 plan.", call. = FALSE)
  }
  designs <- lapply(plan$designs$design_id, scmix_v23_load_frozen_design)
  target_truths <- scmix_v23_oracle_truths(
    designs, regime_ids = plan$regimes$regime_id, plan = plan,
    require_frozen = TRUE)
  design_diagnostics <- do.call(rbind, lapply(
    designs, .scmix_v23_design_diagnostic_row))
  if (any(target_truths$application_outcomes_used) ||
      any(design_diagnostics$application_outcomes_used) ||
      nrow(target_truths) != nrow(plan$designs) * nrow(plan$severity) * 8L ||
      nrow(design_diagnostics) != nrow(plan$designs)) {
    stop("The response-free truth tables are incomplete or outcome contaminated.",
         call. = FALSE)
  }
  key <- paste(target_truths$design_id, target_truths$regime_id,
               target_truths$severity_id, target_truths$target_key, sep = "\r")
  if (anyDuplicated(key)) {
    stop("The target-truth table has duplicate design-cell targets.",
         call. = FALSE)
  }
  list(target_truths = .scmix_v23_truth_csv_ready(target_truths),
       design_diagnostics = .scmix_v23_truth_csv_ready(design_diagnostics),
       application_outcomes_used = FALSE)
}

.scmix_v23_truth_manifest_number <- function(x, name) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) != 1L || !is.finite(out)) {
    stop("Truth-manifest field `", name, "` is malformed.", call. = FALSE)
  }
  out
}

#' Load the frozen response-free Step-3 truth manifest
#'
#' This artifact binds the exact design-derived target truths and diagnostics
#' that were written before any synthetic outcome was generated. It is not a
#' calibration-values file and cannot approve a production analysis.
#'
#' @param plan Intact prospective protocol-v2.3 plan.
#' @return An intact `scmix_v23_truth_manifest`.
#' @export
scmix_v23_truth_manifest <- function(plan = scmix_v23_calibration_plan()) {
  manifest_path <- .scmix_v23_step3_asset_file(
    "protocol", .scmix_v23_truth_files[["manifest"]])
  raw <- as.list(read.dcf(manifest_path)[1L, , drop = TRUE])
  if (!identical(raw[["Plan-Id"]], plan$plan_id) ||
      !identical(raw[["Protocol-Id"]], plan$protocol_id) ||
      !identical(raw[["Status"]], "frozen_before_synthetic_outcomes") ||
      !identical(raw[["Application-Outcomes-Used"]], "false") ||
      !identical(raw[["Synthetic-Outcomes-Generated-Before-Freeze"]],
                 "false")) {
    stop("The Step-3 truth manifest is not the frozen outcome-free artifact.",
         call. = FALSE)
  }
  target_path <- .scmix_v23_step3_asset_file(
    "protocol", raw[["Target-Truth-File"]])
  diagnostic_path <- .scmix_v23_step3_asset_file(
    "protocol", raw[["Design-Diagnostics-File"]])
  observed_hash <- c(
    target_truth_file_sha256 =
      unname(as.character(tools::sha256sum(target_path))),
    design_diagnostics_file_sha256 =
      unname(as.character(tools::sha256sum(diagnostic_path))),
    design_manifest_sha256 = unname(as.character(tools::sha256sum(
      .scmix_v23_step3_file("designs")))),
    dgp_registry_sha256 = unname(as.character(tools::sha256sum(
      .scmix_v23_step3_file("dgp")))),
    severity_registry_sha256 = unname(as.character(tools::sha256sum(
      .scmix_v23_step3_file("severity")))),
    target_registry_sha256 = unname(as.character(tools::sha256sum(
      .scmix_v23_step3_file("targets")))))
  declared_hash <- c(
    target_truth_file_sha256 = raw[["Target-Truth-SHA256"]],
    design_diagnostics_file_sha256 =
      raw[["Design-Diagnostics-SHA256"]],
    design_manifest_sha256 = raw[["Design-Manifest-SHA256"]],
    dgp_registry_sha256 = raw[["DGP-Registry-SHA256"]],
    severity_registry_sha256 = raw[["Severity-Registry-SHA256"]],
    target_registry_sha256 = raw[["Target-Registry-SHA256"]])
  if (!identical(observed_hash, declared_hash)) {
    stop("A frozen truth or source-registry SHA-256 does not match.",
         call. = FALSE)
  }
  target <- utils::read.csv(target_path, stringsAsFactors = FALSE,
                            check.names = FALSE, colClasses = "character")
  diagnostic <- utils::read.csv(
    diagnostic_path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character")
  required_target <- c(
    "design_id", "regime_id", "severity_id", "certification_truth",
    "target_key", "target_id", "variant_id", "truth",
    "truth_conditioning", "application_outcomes_used",
    "primitive_a_truth", "primitive_b_truth")
  required_diagnostic <- c(
    "design_id", "response_free_payload_hash", "exact_Ti_histogram",
    "pc_eigenvalues_descending", "pc1_loading_original_coordinates",
    "pc2_loading_original_coordinates", "pc3_loading_original_coordinates",
    "weakest_pc_loading_original_coordinates", "z_mean", "z_sd", "z_min",
    "z_median", "z_max", "n_z_positive",
    "n_z_nonpositive", "application_outcomes_used")
  if (any(!required_target %in% names(target)) ||
      any(!required_diagnostic %in% names(diagnostic)) ||
      nrow(target) != nrow(plan$designs) * nrow(plan$severity) * 8L ||
      !identical(diagnostic$design_id, plan$designs$design_id) ||
      any(target$application_outcomes_used != "false") ||
      any(diagnostic$application_outcomes_used != "false")) {
    stop("The frozen response-free truth tables are malformed.",
         call. = FALSE)
  }
  target$truth <- vapply(target$truth, .scmix_v23_truth_manifest_number,
                         numeric(1L), name = "truth")
  for (name in c("primitive_a_truth", "primitive_b_truth")) {
    target[[name]] <- suppressWarnings(as.numeric(target[[name]]))
  }
  key <- paste(target$design_id, target$regime_id, target$severity_id,
               target$target_key, sep = "\r")
  if (anyDuplicated(key)) stop("The frozen target truths are not unique.",
                               call. = FALSE)
  payload <- list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    plan_fingerprint = plan$fingerprint, frozen_on = raw[["Frozen-On"]],
    target_truths = target, design_diagnostics = diagnostic,
    source_hashes = observed_hash,
    application_outcomes_used = FALSE,
    frozen_before_synthetic_outcomes = TRUE,
    production_manifest_created = FALSE)
  .scmix_v23_step3_sign(payload, "scmix_v23_truth_manifest")
}
