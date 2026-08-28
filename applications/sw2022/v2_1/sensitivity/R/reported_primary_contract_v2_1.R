## Fail-closed binding of downstream Saha--Weeks v2.1 work to the reported
## primary pointer.  This file has no side effects and may be sourced by tests.

`%||%` <- function(x, y) if (is.null(x)) y else x

.sw_v21_md5 <- function(paths) {
  path_names <- names(paths)
  paths <- as.character(paths)
  if (!length(paths) || anyNA(paths) || any(!nzchar(paths)) ||
      any(!file.exists(paths))) {
    stop("Cannot hash a missing v2.1 provenance input.", call. = FALSE)
  }
  out <- unname(tools::md5sum(paths))
  names(out) <- path_names
  out
}

.sw_v21_same_md5 <- function(x, y) {
  is.character(x) && is.character(y) &&
    identical(names(x), names(y)) && identical(unname(x), unname(y))
}

.sw_v21_inside <- function(paths, root) {
  root <- normalizePath(root, mustWork = TRUE)
  paths <- normalizePath(paths, mustWork = TRUE)
  prefix <- paste0(root, .Platform$file.sep)
  paths == root | startsWith(paths, prefix)
}

.sw_v21_pointer_shape <- function(pointer, config) {
  reasons <- character()
  add <- function(ok, why) if (!isTRUE(ok)) reasons <<- c(reasons, why)
  add(is.list(pointer), "pointer_not_list")
  if (!is.list(pointer)) return(list(pass = FALSE, reasons = reasons))
  add(identical(pointer$schema_version,
                "sw2022-v2.1-reported-primary-pointer-v1"),
      "wrong_pointer_schema")
  add(pointer$reported_primary %in%
        c("selected_procedure_q1", "exact_constant_q1"),
      "invalid_reported_primary")
  add(is.logical(pointer$fallback_applied) &&
        length(pointer$fallback_applied) == 1L &&
        !is.na(pointer$fallback_applied), "invalid_fallback_flag")
  add(identical(pointer$descriptive_only, TRUE),
      "descriptive_only_not_true")
  add(identical(pointer$formal_test, FALSE), "formal_test_not_false")
  add(identical(pointer$formal_inference_available, FALSE),
      "formal_inference_not_withheld")
  add(identical(pointer$outcome_blind, FALSE),
      "outcome_informed_label_missing")
  add(is.numeric(pointer$noninferiority_margin) &&
        length(pointer$noninferiority_margin) == 1L &&
        is.finite(pointer$noninferiority_margin) &&
        identical(as.numeric(pointer$noninferiority_margin),
                  as.numeric(config$postpilot_guardrail$noninferiority_margin)),
      "guardrail_margin_mismatch")
  add(is.numeric(pointer$score_difference) &&
        length(pointer$score_difference) == 1L &&
        is.finite(pointer$score_difference), "invalid_score_difference")
  add(is.numeric(pointer$score_difference_respondent_se) &&
        length(pointer$score_difference_respondent_se) == 1L &&
        is.finite(pointer$score_difference_respondent_se) &&
        pointer$score_difference_respondent_se >= 0,
      "invalid_score_difference_se")
  add(is.list(pointer$selected_procedure_paths) &&
        identical(names(pointer$selected_procedure_paths),
                  c("full", "nested", "assembled")),
      "selected_paths_malformed")
  add(is.list(pointer$exact_constant_paths) &&
        identical(names(pointer$exact_constant_paths),
                  c("full", "nested", "assembled")),
      "constant_paths_malformed")
  chosen <- if (isTRUE(pointer$fallback_applied)) {
    pointer$exact_constant_paths
  } else pointer$selected_procedure_paths
  if (is.list(chosen)) {
    add(identical(pointer$full_fit_path, chosen$full),
        "chosen_full_path_mismatch")
    add(identical(pointer$nested_fit_path, chosen$nested),
        "chosen_nested_path_mismatch")
    add(identical(pointer$assembled_fit_path, chosen$assembled),
        "chosen_assembled_path_mismatch")
  }
  add(identical(isTRUE(pointer$fallback_applied),
                identical(pointer$reported_primary, "exact_constant_q1")),
      "reported_primary_fallback_inconsistent")
  add(is.character(pointer$generation_input_md5) &&
        length(pointer$generation_input_md5) > 0L &&
        !is.null(names(pointer$generation_input_md5)),
      "generation_hash_vector_malformed")
  add(is.list(pointer$runtime_signature) &&
        length(pointer$runtime_signature) > 0L &&
        !is.null(names(pointer$runtime_signature)) &&
        all(nzchar(unlist(pointer$runtime_signature, use.names = FALSE))),
      "runtime_signature_missing")
  add(is.character(pointer$authorization_md5) &&
        length(pointer$authorization_md5) == 1L &&
        !is.na(pointer$authorization_md5) && nzchar(pointer$authorization_md5),
      "authorization_hash_missing")
  list(pass = !length(reasons), reasons = unique(reasons))
}

.sw_v21_fit_stamp_valid <- function(x, config, role, pointer) {
  stamp <- x$sw_v21_application_specification
  is.list(stamp) && identical(stamp$config_version, config$version) &&
    identical(stamp$role, role) &&
    .sw_v21_same_md5(stamp$generation_input_md5,
                     pointer$generation_input_md5) &&
    identical(stamp$runtime_signature, pointer$runtime_signature) &&
    identical(stamp$authorization_md5, pointer$authorization_md5) &&
    identical(stamp$guardrail, config$postpilot_guardrail) &&
    identical(stamp$outcome_blind, FALSE) &&
    identical(stamp$formal_inference_available, FALSE) &&
    identical(stamp$production_result, FALSE)
}

.sw_v21_validate_reported_primary <- function(pointer_path, config,
                                               load_fits = TRUE) {
  if (!is.character(pointer_path) || length(pointer_path) != 1L ||
      !file.exists(pointer_path)) {
    stop("The v2.1 reported-primary pointer is absent.", call. = FALSE)
  }
  output_root <- normalizePath(config$output_root, mustWork = TRUE)
  pointer_path <- normalizePath(pointer_path, mustWork = TRUE)
  expected_pointer <- file.path(output_root, "reported_primary_pointer.rds")
  if (!identical(pointer_path,
                 normalizePath(expected_pointer, mustWork = TRUE))) {
    stop("The pointer is not the canonical v2.1 reported-primary pointer.",
         call. = FALSE)
  }
  pointer <- readRDS(pointer_path)
  shape <- .sw_v21_pointer_shape(pointer, config)
  if (!shape$pass) {
    stop("Malformed v2.1 reported-primary pointer: ",
         paste(shape$reasons, collapse = ", "), call. = FALSE)
  }

  parent_result_path <- file.path(output_root, "postpilot_final_result.rds")
  parent_manifest_path <- file.path(output_root, "manifest.rds")
  required_parent <- c(pointer = pointer_path, result = parent_result_path,
                       manifest = parent_manifest_path)
  if (any(!file.exists(required_parent))) {
    stop("The pointer's result or manifest is absent.", call. = FALSE)
  }
  result <- readRDS(parent_result_path)
  manifest <- readRDS(parent_manifest_path)
  if (!is.list(result) ||
      !identical(result$schema_version,
                 "sw2022-v2.1-postpilot-final-analysis-v1") ||
      !identical(result$final_analysis_success, TRUE) ||
      !identical(result$procedural_primary_available, TRUE) ||
      !identical(result$reported_primary, pointer$reported_primary) ||
      !identical(result$fallback_applied, pointer$fallback_applied) ||
      !identical(normalizePath(result$pointer_path, mustWork = TRUE),
                 pointer_path) ||
      !identical(result$formal_inference_available, FALSE) ||
      !identical(result$outcome_blind, FALSE) ||
      !identical(result$production_result, FALSE) ||
      !.sw_v21_same_md5(result$generation_input_md5,
                        pointer$generation_input_md5) ||
      !.sw_v21_same_md5(result$completion_input_md5,
                        pointer$generation_input_md5) ||
      !identical(result$runtime_signature, pointer$runtime_signature) ||
      !identical(result$authorization_md5, pointer$authorization_md5)) {
    stop("The v2.1 parent result does not validate the reported pointer.",
         call. = FALSE)
  }
  if (!is.list(manifest) ||
      !identical(manifest$schema_version,
                 "sw2022-v2.1-postpilot-final-manifest-v1") ||
      !identical(manifest$configuration_version, config$version) ||
      !identical(manifest$final_analysis_success, TRUE) ||
      !identical(manifest$procedural_primary_available, TRUE) ||
      !identical(manifest$reported_primary, pointer$reported_primary) ||
      !identical(manifest$fallback_applied, pointer$fallback_applied) ||
      !identical(manifest$formal_inference_available, FALSE) ||
      !identical(manifest$outcome_blind, FALSE) ||
      !identical(manifest$production_result, FALSE) ||
      !.sw_v21_same_md5(manifest$generation_input_md5,
                        pointer$generation_input_md5) ||
      !.sw_v21_same_md5(manifest$completion_input_md5,
                        pointer$generation_input_md5) ||
      !identical(manifest$runtime_signature, pointer$runtime_signature) ||
      !identical(manifest$authorization_md5, pointer$authorization_md5)) {
    stop("The v2.1 parent manifest does not validate the reported pointer.",
         call. = FALSE)
  }

  input_paths <- manifest$input_paths
  if (!is.character(input_paths) || !length(input_paths) ||
      is.null(names(input_paths)) || anyDuplicated(names(input_paths)) ||
      any(!file.exists(input_paths)) ||
      !.sw_v21_same_md5(.sw_v21_md5(input_paths),
                        manifest$generation_input_md5)) {
    stop("A frozen v2.1 generation input changed after the primary run.",
         call. = FALSE)
  }
  if (!file.exists(config$authorization_file) ||
      !identical(unname(tools::md5sum(config$authorization_file)),
                 pointer$authorization_md5)) {
    stop("The v2.1 authorization no longer matches the pointer.",
         call. = FALSE)
  }
  artifacts <- manifest$artifacts
  if (!is.character(artifacts) || !length(artifacts) ||
      is.null(names(artifacts)) || anyDuplicated(names(artifacts)) ||
      any(grepl("[/\\\\]", names(artifacts)))) {
    stop("The parent artifact hash table is malformed.", call. = FALSE)
  }
  artifact_paths <- file.path(output_root, names(artifacts))
  names(artifact_paths) <- names(artifacts)
  if (any(!file.exists(artifact_paths)) ||
      !.sw_v21_same_md5(.sw_v21_md5(artifact_paths), artifacts)) {
    stop("At least one parent v2.1 artifact changed after completion.",
         call. = FALSE)
  }

  path_groups <- c(
    selected_full = pointer$selected_procedure_paths$full,
    selected_nested = pointer$selected_procedure_paths$nested,
    selected_assembled = pointer$selected_procedure_paths$assembled,
    constant_full = pointer$exact_constant_paths$full,
    constant_nested = pointer$exact_constant_paths$nested,
    constant_assembled = pointer$exact_constant_paths$assembled)
  expected_names <- paste0("fit_", c(
    "selected_full.rds", "selected_nested.rds", "selected_assembled.rds",
    "constant_full.rds", "constant_nested.rds", "constant_assembled.rds"))
  if (any(!file.exists(path_groups)) ||
      !all(.sw_v21_inside(path_groups, output_root)) ||
      !identical(basename(path_groups), expected_names)) {
    stop("The pointer's six fit paths are missing or outside its output root.",
         call. = FALSE)
  }
  chosen_paths <- c(full = pointer$full_fit_path,
                    nested = pointer$nested_fit_path,
                    assembled = pointer$assembled_fit_path)
  chosen_roles <- if (isTRUE(pointer$fallback_applied)) {
    c(full = "constant_full", nested = "constant_nested",
      assembled = "constant_assembled")
  } else c(full = "selected_full", nested = "selected_nested",
           assembled = "selected_assembled")

  fits <- NULL
  if (isTRUE(load_fits)) {
    fits <- lapply(chosen_paths, readRDS)
    if (!all(vapply(names(fits), function(nm) {
      .sw_v21_fit_stamp_valid(fits[[nm]], config, chosen_roles[[nm]], pointer)
    }, logical(1L)))) {
      stop("A reported-primary fit stamp is stale or inconsistent.",
           call. = FALSE)
    }
    full <- fits$full; nested <- fits$nested; assembled <- fits$assembled
    if (is.null(full$selected) || is.null(full$refit) ||
        !identical(as.integer(full$selected$q), 1L) ||
        is.null(nested$tuning) || length(nested$tuning) != 5L ||
        is.null(assembled$fold_id) || is.null(assembled$mu_all_folds) ||
        length(assembled$mu_all_folds) != 5L ||
        !identical(as.integer(assembled$q), 1L) ||
        !identical(assembled$analysis_signature,
                   nested$analysis_signature) ||
        !identical(as.integer(assembled$fold_id),
                   as.integer(nested$outer_fold_index))) {
      stop("The three reported-primary fit objects are mutually inconsistent.",
           call. = FALSE)
    }
  }

  lock_paths <- c(required_parent, path_groups,
                  authorization = config$authorization_file,
                  parent_inputs = input_paths)
  lock_paths <- lock_paths[!duplicated(names(lock_paths))]
  list(
    pointer = pointer, pointer_path = pointer_path,
    result = result, manifest = manifest,
    full = if (isTRUE(load_fits)) fits$full else NULL,
    nested = if (isTRUE(load_fits)) fits$nested else NULL,
    assembled = if (isTRUE(load_fits)) fits$assembled else NULL,
    chosen_paths = chosen_paths, chosen_roles = chosen_roles,
    lock_paths = lock_paths, lock_md5 = .sw_v21_md5(lock_paths),
    validated = TRUE, descriptive_only = TRUE,
    formal_inference_available = FALSE, outcome_blind = FALSE)
}
