## Pure fail-closed helpers for the Saha--Weeks v2.1 rank/numerical runner.

.sw_v21_rank_path_equal <- function(x, y) {
  is.character(x) && length(x) == 1L && !is.na(x) && file.exists(x) &&
    is.character(y) && length(y) == 1L && !is.na(y) && file.exists(y) &&
    identical(normalizePath(x, mustWork = TRUE),
              normalizePath(y, mustWork = TRUE))
}

.sw_v21_rank_paths_inside <- function(paths, directory) {
  if (!is.character(paths) || !length(paths) || anyNA(paths) ||
      any(!file.exists(paths)) || !dir.exists(directory)) return(FALSE)
  root <- paste0(normalizePath(directory, mustWork = TRUE), .Platform$file.sep)
  normalized <- normalizePath(paths, mustWork = TRUE)
  all(startsWith(normalized, root))
}

.sw_v21_rank_final_bundle_valid <- function(
    pointer, pointer_path, manifest, manifest_path, parent_config,
    runtime_signature) {
  if (!is.list(pointer) || !is.list(manifest) ||
      !is.list(parent_config) || !file.exists(pointer_path) ||
      !file.exists(manifest_path)) return(FALSE)
  pointer_on_disk <- tryCatch(readRDS(pointer_path), error = function(e) NULL)
  manifest_on_disk <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
  if (!identical(pointer, pointer_on_disk) ||
      !identical(manifest, manifest_on_disk)) return(FALSE)
  output_root <- parent_config$output_root
  artifacts <- manifest$artifacts
  artifact_paths <- if (is.character(artifacts) && length(artifacts) &&
      !is.null(names(artifacts))) {
    stats::setNames(file.path(dirname(manifest_path), names(artifacts)),
                    names(artifacts))
  } else character()
  selected_paths <- unlist(pointer$selected_procedure_paths,
                           use.names = FALSE)
  constant_paths <- unlist(pointer$exact_constant_paths,
                           use.names = FALSE)
  expected_primary <- if (isTRUE(pointer$fallback_applied)) {
    pointer$exact_constant_paths
  } else pointer$selected_procedure_paths
  expected_label <- if (isTRUE(pointer$fallback_applied)) {
    "exact_constant_q1"
  } else "selected_procedure_q1"
  all_pointer_paths <- c(selected_paths, constant_paths,
                         pointer$full_fit_path, pointer$nested_fit_path,
                         pointer$assembled_fit_path)
  input_hash_valid <- is.character(manifest$input_paths) &&
    length(manifest$input_paths) && all(file.exists(manifest$input_paths)) &&
    .sc_identical_md5_vectors(
      manifest$generation_input_md5,
      .sc_md5_paths(manifest$input_paths)) &&
    .sc_identical_md5_vectors(
      manifest$generation_input_md5,
      manifest$completion_input_md5)
  artifact_hash_valid <- .sc_manifest_artifacts_valid(
    manifest, manifest_path)
  pointer_is_artifact <- basename(pointer_path) %in% names(artifacts) &&
    identical(unname(artifacts[[basename(pointer_path)]]),
              unname(tools::md5sum(pointer_path)))
  referenced_artifacts_bound <- length(all_pointer_paths) == 9L &&
    all(basename(all_pointer_paths) %in% names(artifacts)) &&
    all(vapply(all_pointer_paths, function(path) {
      identical(unname(artifacts[[basename(path)]]),
                unname(tools::md5sum(path)))
    }, logical(1L)))
  auth_valid <- file.exists(parent_config$authorization_file) &&
    identical(as.character(pointer$authorization_md5),
              unname(tools::md5sum(parent_config$authorization_file))) &&
    identical(as.character(manifest$authorization_md5),
              unname(tools::md5sum(parent_config$authorization_file)))
  primary_paths_valid <- .sw_v21_rank_path_equal(
    pointer$full_fit_path, expected_primary$full) &&
    .sw_v21_rank_path_equal(
      pointer$nested_fit_path, expected_primary$nested) &&
    .sw_v21_rank_path_equal(
      pointer$assembled_fit_path, expected_primary$assembled)
  guardrail_valid <- is.list(manifest$postpilot_guardrail) &&
    is.numeric(pointer$score_difference) &&
    length(pointer$score_difference) == 1L &&
    is.finite(pointer$score_difference) &&
    identical(as.numeric(pointer$score_difference),
              as.numeric(manifest$postpilot_guardrail$mean_difference)) &&
    identical(as.numeric(pointer$noninferiority_margin),
              as.numeric(manifest$postpilot_guardrail$margin)) &&
    identical(isTRUE(pointer$fallback_applied),
              isTRUE(manifest$fallback_applied))
  identical(
    pointer$schema_version,
    "sw2022-v2.1-reported-primary-pointer-v1") &&
    identical(
      manifest$schema_version,
      "sw2022-v2.1-postpilot-final-manifest-v1") &&
    identical(manifest$configuration_version, parent_config$version) &&
    identical(manifest$final_analysis_success, TRUE) &&
    identical(manifest$procedural_primary_available, TRUE) &&
    identical(pointer$reported_primary, expected_label) &&
    identical(manifest$reported_primary, expected_label) &&
    identical(pointer$formal_inference_available, FALSE) &&
    identical(pointer$formal_test, FALSE) &&
    identical(pointer$outcome_blind, FALSE) &&
    identical(manifest$formal_inference_available, FALSE) &&
    identical(manifest$outcome_blind, FALSE) &&
    identical(manifest$production_result, FALSE) &&
    identical(pointer$runtime_signature, runtime_signature) &&
    identical(manifest$runtime_signature, runtime_signature) &&
    .sc_identical_md5_vectors(
      pointer$generation_input_md5,
      manifest$generation_input_md5) &&
    .sw_v21_rank_path_equal(
      pointer_path,
      file.path(output_root, "reported_primary_pointer.rds")) &&
    .sw_v21_rank_paths_inside(all_pointer_paths, output_root) &&
    input_hash_valid && artifact_hash_valid && pointer_is_artifact &&
    referenced_artifacts_bound && auth_valid && primary_paths_valid &&
    guardrail_valid
}

.sw_v21_rank_generation_paths <- function(
    root, app, rank_config_path, rank_contract_path, rank_runner_path,
    authorization_creator_path, parent_config_path, parent_contract_path,
    parent_runner_path, parent_authorization_creator_path, pointer_path,
    manifest_path, manifest) {
  package_sources <- sort(list.files(
    file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  names(package_sources) <- paste0(
    "package_source:", basename(package_sources))
  final_inputs <- manifest$input_paths
  names(final_inputs) <- paste0("final_input:", names(final_inputs))
  final_artifacts <- file.path(dirname(manifest_path),
                               names(manifest$artifacts))
  names(final_artifacts) <- paste0(
    "final_artifact:", names(manifest$artifacts))
  paths <- c(
    rank_config = rank_config_path,
    rank_contract = rank_contract_path,
    rank_runner = rank_runner_path,
    rank_authorization_creator = authorization_creator_path,
    parent_config = parent_config_path,
    parent_contract = parent_contract_path,
    parent_runner = parent_runner_path,
    parent_authorization_creator = parent_authorization_creator_path,
    final_pointer = pointer_path,
    final_manifest = manifest_path,
    package_description = file.path(root, "DESCRIPTION"),
    package_namespace = file.path(root, "NAMESPACE"),
    launcher_R45 = file.path(root, "applications", "bin", "R45"),
    launcher_Rscript45 = file.path(root, "applications", "bin", "Rscript45"),
    package_sources, final_inputs, final_artifacts)
  if (is.null(names(paths)) || any(!nzchar(names(paths))) ||
      anyDuplicated(names(paths)) || any(!file.exists(paths))) {
    stop("Rank/numerical generation paths are missing or not uniquely named.",
         call. = FALSE)
  }
  paths
}

.sw_v21_rank_authorization_valid <- function(
    authorization, config, config_path, generation_md5, runtime_signature,
    pointer, pointer_path, manifest_path) {
  is.list(authorization) &&
    identical(authorization$authorized, TRUE) &&
    identical(authorization$purpose,
              "sw2022-v2.1-rank-numerical-diagnostics") &&
    is.character(authorization$reviewed_by) &&
    length(authorization$reviewed_by) == 1L &&
    !is.na(authorization$reviewed_by) &&
    nzchar(authorization$reviewed_by) &&
    is.character(authorization$authorized_at_utc) &&
    length(authorization$authorized_at_utc) == 1L &&
    !is.na(authorization$authorized_at_utc) &&
    nzchar(authorization$authorized_at_utc) &&
    identical(authorization$acknowledged_outcome_informed, TRUE) &&
    identical(authorization$acknowledged_formal_inference_unavailable, TRUE) &&
    identical(authorization$acknowledged_no_rank_selection, TRUE) &&
    identical(authorization$config_version, config$version) &&
    identical(as.character(authorization$config_md5),
              unname(tools::md5sum(config_path))) &&
    .sc_identical_md5_vectors(
      authorization$generation_input_md5, generation_md5) &&
    identical(authorization$runtime_signature, runtime_signature) &&
    identical(as.character(authorization$reviewed_pointer_md5),
              unname(tools::md5sum(pointer_path))) &&
    identical(as.character(authorization$reviewed_final_manifest_md5),
              unname(tools::md5sum(manifest_path))) &&
    identical(authorization$reported_primary, pointer$reported_primary) &&
    identical(authorization$fallback_applied,
              isTRUE(pointer$fallback_applied)) &&
    identical(authorization$formal_inference_available, FALSE) &&
    identical(authorization$rank_selected, FALSE) &&
    identical(authorization$outcome_blind, FALSE)
}

.sw_v21_rank_grid_at_nodes <- function(parent_grid, panel, q, nodes) {
  if (!is.list(parent_grid) || length(parent_grid) != 10L ||
      !panel %in% c("selected_procedure", "exact_constant") ||
      !q %in% 0:2 || length(nodes) != 1L || !is.finite(nodes) ||
      nodes < 1L || nodes != as.integer(nodes)) {
    stop("Malformed frozen grid, panel, rank, or GH resolution.",
         call. = FALSE)
  }
  constant <- which(vapply(parent_grid, function(x) {
    identical(x$mean_family, "constant") &&
      identical(as.numeric(x$weight_decay), 0)
  }, logical(1L)))
  if (length(constant) != 1L) {
    stop("The frozen parent grid lacks one exact constant.", call. = FALSE)
  }
  grid <- if (identical(panel, "exact_constant")) {
    parent_grid[constant]
  } else parent_grid
  lapply(grid, function(spec) {
    spec$q <- NULL
    spec$n_draws <- NULL
    spec$integration <- "gh"
    spec$n_nodes <- as.integer(nodes)
    spec
  })
}

.sw_v21_rank_corrected_refit_valid <- function(refit, q) {
  state <- refit$network_state
  arch <- state$architecture
  dict <- state$state_dict
  is.list(refit) && is.list(state) && is.list(arch) && is.list(dict) &&
    identical(state$format, "scmix-network-state") &&
    identical(state$format_version, 2L) &&
    identical(state$architecture_id, "mixed-conjoint-mean-family-v2") &&
    identical(arch$q, as.integer(q)) &&
    arch$mean_family %in% c("constant", "linear", "relu") &&
    is.numeric(dict$alpha_raw) && length(dict$alpha_raw) == arch$p &&
    is.numeric(dict$mu_bound_internal) &&
    length(dict$mu_bound_internal) == arch$p &&
    is.list(state$preprocessing$deltaX) &&
    identical(state$preprocessing$deltaX$centering, "none") &&
    is.list(refit$optimization$bounds) &&
    identical(refit$optimization$bounds$alpha_diagnostics_applicable, TRUE)
}

.sw_v21_rank_tuning_gate <- function(tuning, q, stage_gate) {
  if (!is.list(tuning) || !is.function(stage_gate)) return(FALSE)
  selected <- tuning$selected_index
  selection <- tuning$candidate_selection_gate
  refit <- tuning$refit
  length(selected) == 1L && !is.na(selected) &&
    is.list(selection) && isTRUE(selection$pass) &&
    isTRUE(selection$selection_eligible[selected]) &&
    isTRUE(stage_gate(refit$optimization)$pass) &&
    (is.null(refit$pooled_prefit_optimization) ||
       isTRUE(stage_gate(refit$pooled_prefit_optimization)$pass)) &&
    (is.null(refit$continued_constant_optimization) ||
       isTRUE(stage_gate(refit$continued_constant_optimization)$pass)) &&
    .sw_v21_rank_corrected_refit_valid(refit, q)
}

.sw_v21_rank_nested_gate <- function(nested, q, outer_fold_id, stage_gate) {
  is.list(nested) &&
    identical(as.integer(nested$outer_fold_id), as.integer(outer_fold_id)) &&
    length(nested$tuning) == length(unique(outer_fold_id)) &&
    all(vapply(nested$tuning, .sw_v21_rank_tuning_gate, logical(1L),
               q = q, stage_gate = stage_gate))
}

.sw_v21_rank_rotation_matrix <- function(angle) {
  if (!is.numeric(angle) || length(angle) != 1L || !is.finite(angle)) {
    stop("Rotation angle must be one finite number.", call. = FALSE)
  }
  matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2L, 2L)
}
