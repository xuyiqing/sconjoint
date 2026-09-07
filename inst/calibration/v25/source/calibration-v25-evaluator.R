## Pre-outcome evaluator contract and state machine for inference protocol v2.5.
##
## This file does not fit a model and does not create an approval.  It makes
## the prospective lock executable.  Synthetic outcomes can be generated only
## after an approval DCF has hash-bound the complete evaluator implementation,
## its driver and family finalizer, and the exact settings below.

.scmix_v25_settings_name <- "inference-v2.5-evaluator-settings.dcf"
.scmix_v25_state_names <- c(
  evaluator = "inference-v2.5-evaluator-approval.dcf",
  pilot = "inference-v2.5-integrity-pilot-approval.dcf",
  evidence = "inference-v2.5-evidence-completion.dcf",
  family = "inference-v2.5-family-approval.dcf")
.scmix_v25_evaluator_interface_id <-
  "paperps_v25_one_fit_all_targets_end_to_end_v1"
.scmix_v25_release_algorithm_id <-
  "paperps_v25_type1_common_multiplier_mechanical_family_release_v1"
.scmix_v25_runtime_projection_id <- paste0(
  "1.25_times_25_times_exact_observed_wall_time_of_the_complete_",
  "first_valid_12_job_12_worker_pilot_wave")

.scmix_v25_locate <- function(candidates, label) {
  candidates <- unique(candidates[nzchar(candidates)])
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("Cannot locate the v2.5 ", label, ".", call. = FALSE)
  }
  normalizePath(found[[1L]], mustWork = TRUE)
}

.scmix_v25_settings_file <- function() {
  source_root <- Sys.getenv("SCONJOINT_SOURCE_ROOT", unset = "")
  .scmix_v25_locate(c(
    system.file("protocol", .scmix_v25_settings_name,
                package = "sconjoint"),
    file.path(getwd(), "inst", "protocol", .scmix_v25_settings_name),
    file.path(source_root, "inst", "protocol", .scmix_v25_settings_name)),
    "evaluator settings file")
}

.scmix_v25_settings_expected <- function() {
  c(
    `Settings-Id` = "paperps-v2.5-end-to-end-evaluator-settings-v1",
    `Protocol-Id` = "paperps-inference-2026-09-07-v2.5",
    `Plan-Id` = "paperps-lean-validation-2026-09-07-v2.5-v1",
    Status = paste0("prospective_evaluator_settings_frozen_outcomes_",
                    "locked_implementation_absent"),
    `Evaluator-Source-Prefix` = "calibration-v25",
    `Evaluator-Function-Prefix` = "scmix_v25",
    `Canonical-State-Directory` = .scmix_v25_state_relative,
    `Canonical-Run-Directory` = .scmix_v25_run_relative,
    `Fit-Rank` = "1", `Mean-Candidates` = "constant,linear",
    `Deviation-Penalty` = "0.01", `Adam-Epochs` = "400",
    `Adam-Learning-Rate` = "0.01", `Mu-Bound` = "10",
    `Kappa-Bound` = "10", `Alpha-Bound` = "5",
    `Loading-A-Bound` = "10", `Network-Weight-Bound` = "10",
    `Outer-Folds` = "2", `Inner-Folds` = "2",
    `Optimizer-Starts` = "2", `Reference-GH-Nodes` = "15",
    `Refined-GH-Nodes` = "31", `Batch-Mode` = "full_batch",
    `Mean-Selection-Score` = "respondent_level_complete_sequence_log_score",
    `Mean-Selection-Tie-Break` = "least_complex_then_registry_order",
    `Numeric-Precision` = "float64", `Compute-Device` = "cpu",
    `Parallel-Backend` = "base_R_PSOCK_spawn_twelve_processes",
    `Maximum-Workers` = "12", `Workers-Per-Cell` = "2",
    `Deterministic-Lanes-Per-Cell` = "2",
    `Worker-Id-Formula` = "2*(cell_index-1)+lane_index",
    `Pilot-Lane-Assignment` = "lane_1_replication_1;lane_2_replication_2",
    `Evidence-Lane-Assignment` = paste0(
      "lane_1_odd_within_batch_positions;",
      "lane_2_even_within_batch_positions"),
    `Torch-Threads-Per-Worker` = "1",
    `Polish-Method` = "full_batch_LBFGS_strong_wolfe",
    `Polish-Max-Iterations` = "50", `Polish-History-Size` = "50",
    `Polish-Tolerance-Grad` = "0.0000000001",
    `Polish-Tolerance-Change` = "0.000000000001",
    `Polish-Learning-Rate` = "1", `Ridge-Primary` = "Ntrain^(-3/4)",
    `Ridge-Sensitivities` = "Ntrain^(-1),Ntrain^(-1/2)",
    `Ridge-Selection` = "none",
    `Riesz-Numerical-Rank-Rule` =
      "dimension_times_machine_epsilon_times_leading_eigenvalue",
    `Riesz-Heldout-Defect` = "honest_information_scaled",
    `Derivative-Step` = "machine_epsilon^(1/3)",
    `Derivative-Gate-Input` = paste0(
      "actual_analytic_vs_h_and_h_over_2_relative_error_",
      "v2.2_protocol_exact_flag_not_used"),
    `Integration-Branches` = paste0(
      "fixed_GH15_state_evaluated_at_GH31|",
      "GH31_LBFGS_from_exact_GH15_state_zero_Adam"),
    `Integration-Movement-Rule` = "maximum_over_both_GH31_branches",
    `Near-Optimum-Objective-Gap` = "0.000001",
    `Near-Optimum-Target-Spread` = "0.10",
    `Near-Optimum-Start-Set` = paste0(
      "both_frozen_optimizer_starts_with_relative_objective_gap_",
      "at_most_0.000001"),
    `Fit-Gate-Start-Scope` = paste0(
      "selected_full_and_outer_states_for_KKT_loading_and_nested_gates;",
      "near_optimum_states_for_polish_precision_and_finiteness;",
      "discarded_far_starts_descriptive"),
    `Repolished-Gate-Scope` = paste0(
      "selected_full_and_outer_GH31_exact_state_zero_Adam_repolishes_for_",
      "KKT_loading_polish_precision_and_finiteness"),
    `Share-Boundary-Evaluation` = paste0(
      "exact_declared_tie_rule_at_zero_variance_with_no_positive_",
      "variance_floor"),
    `Share-Regularity-Gate` = paste0(
      "own_directional_variance_studentized_margin_at_least_1.96"),
    `Target-Constraint-Map-Id` =
      "paperps_v25_registered_target_constraint_map_v1",
    `Confidence-Set-Map-Id` = "paperps_v25_rectangle_mapping_v1",
    `Family-Critical-Value` = paste0(
      "type1_empirical_0.95_quantile_of_common_9999_respondent_",
      "multiplier_maximum_no_interpolation"),
    `Fit-Seed-Offset` = "500000",
    `Outer-Fold-Seed-Component` = "v25-outer-folds",
    `Optimizer-Start-Seeds` = "deterministic_package_seed_derivation",
    `RNG-Kind` = "L'Ecuyer-CMRG", `RNG-Normal-Kind` = "Inversion",
    `RNG-Sample-Kind` = "Rejection",
    `Pilot-Schedule` = paste0(
      "one_complete_12_job_wave_with_exactly_12_one_thread_workers_",
      "and_two_deterministic_lanes_per_cell"),
    `Pilot-Selection` = paste0(
      "chronologically_first_complete_integrity_valid_wave_only"),
    `Pilot-Rerun-After-Valid-Wave` =
      "prohibited_regardless_of_runtime",
    `Pilot-Wall-Clock-Definition` = paste0(
      "monotonic_elapsed_from_before_cluster_creation_through_cluster_",
      "shutdown_and_all_artifact_and_manifest_verification"),
    `Pilot-Runtime-Projection` = .scmix_v25_runtime_projection_id,
    `Pilot-Runtime-Safety-Factor` = "1.25",
    `Pilot-Wall-Clock-Cutoff-Seconds` = "921.6",
    `Pilot-Artifact-Visibility` = paste0(
      "sealed_replication_artifacts_public_integrity_time_resource_only"),
    `Pilot-Inspection-Firewall` = paste0(
      "no_synthetic_choice_fit_target_gate_interval_coverage_or_",
      "operating_characteristic_result_available_or_inspected_for_go_no_go"),
    `Evidence-Reps-Per-Cell` = "50",
    `Evidence-Batch-Size-Per-Cell` = "10",
    `Evidence-Batches` = "5",
    `Evidence-Early-Success` = "false",
    `Evidence-Early-Stop` = "none_all_300_attempts_required")
}

## Exact, machine-readable evaluator settings.  The returned fingerprint is
## separately recorded in the future evaluator approval DCF, while the DCF's
## byte hash is recorded as well.
scmix_v25_evaluator_settings <- function(
    plan = NULL, path = .scmix_v25_settings_file()) {
  raw_matrix <- read.dcf(path)
  if (nrow(raw_matrix) != 1L) {
    stop("The v2.5 evaluator settings DCF must contain one record.",
         call. = FALSE)
  }
  raw <- as.list(raw_matrix[1L, , drop = TRUE])
  expected <- .scmix_v25_settings_expected()
  if (!identical(names(raw), names(expected)) ||
      !identical(unname(unlist(raw, use.names = FALSE)),
                 unname(expected))) {
    stop("The v2.5 evaluator settings changed or are malformed.",
         call. = FALSE)
  }
  if (!is.null(plan)) {
    if (!.scmix_v25_intact(plan, "scmix_v25_validation_plan") ||
        !identical(raw[["Protocol-Id"]], plan$protocol_id) ||
        !identical(raw[["Plan-Id"]], plan$plan_id)) {
      stop("The evaluator settings are not linked to the intact v2.5 plan.",
           call. = FALSE)
    }
  }
  numeric_fields <- c(
    fit_rank = "Fit-Rank", deviation_penalty = "Deviation-Penalty",
    adam_epochs = "Adam-Epochs", adam_learning_rate = "Adam-Learning-Rate",
    mu_bound = "Mu-Bound", kappa_bound = "Kappa-Bound",
    alpha_bound = "Alpha-Bound", loading_a_bound = "Loading-A-Bound",
    network_weight_bound = "Network-Weight-Bound",
    outer_folds = "Outer-Folds", inner_folds = "Inner-Folds",
    starts = "Optimizer-Starts", gh_nodes = "Reference-GH-Nodes",
    refined_gh_nodes = "Refined-GH-Nodes",
    threads_per_worker = "Torch-Threads-Per-Worker",
    polish_max_iterations = "Polish-Max-Iterations",
    polish_history_size = "Polish-History-Size",
    polish_tolerance_grad = "Polish-Tolerance-Grad",
    polish_tolerance_change = "Polish-Tolerance-Change",
    polish_learning_rate = "Polish-Learning-Rate",
    fit_seed_offset = "Fit-Seed-Offset",
    pilot_runtime_safety_factor = "Pilot-Runtime-Safety-Factor")
  integer_fields <- c(
    "fit_rank", "adam_epochs", "outer_folds", "inner_folds", "starts",
    "gh_nodes", "refined_gh_nodes", "threads_per_worker",
    "polish_max_iterations", "polish_history_size", "fit_seed_offset")
  values <- lapply(names(numeric_fields), function(name)
    .scmix_v25_number(raw[[numeric_fields[[name]]]], numeric_fields[[name]],
                      integer = name %in% integer_fields))
  names(values) <- names(numeric_fields)
  payload <- list(
    settings_id = raw[["Settings-Id"]], protocol_id = raw[["Protocol-Id"]],
    plan_id = raw[["Plan-Id"]], status = raw[["Status"]], raw = raw,
    values = values, mean_candidates = c("constant", "linear"),
    batch_mode = "full_batch", numeric_precision = "float64",
    compute_device = "cpu",
    canonical = list(state_relative = raw[["Canonical-State-Directory"]],
                     run_relative = raw[["Canonical-Run-Directory"]]),
    parallel_backend = raw[["Parallel-Backend"]],
    ridge_primary = "Ntrain^(-3/4)",
    ridge_sensitivities = c("Ntrain^(-1)", "Ntrain^(-1/2)"),
    ridge_selection = "none",
    riesz_numerical_rank_rule = "dimension_epsilon",
    integration_branches = c(
      "fixed_GH15_state_evaluated_at_GH31",
      "GH31_LBFGS_from_exact_GH15_state_zero_Adam"),
    integration_movement_rule = "maximum_over_both_GH31_branches",
    derivative_gate_input = paste0(
      "actual_analytic_vs_h_and_h_over_2_relative_error_",
      "v2.2_protocol_exact_flag_not_used"),
    fit_gate_start_scope = raw[["Fit-Gate-Start-Scope"]],
    repolished_gate_scope = raw[["Repolished-Gate-Scope"]],
    share_boundary_evaluation = raw[["Share-Boundary-Evaluation"]],
    share_regularity_gate = raw[["Share-Regularity-Gate"]],
    derivative_step = .Machine$double.eps^(1 / 3),
    rng = c(kind = "L'Ecuyer-CMRG", normal.kind = "Inversion",
            sample.kind = "Rejection"),
    runtime_projection_id = .scmix_v25_runtime_projection_id,
    file_name = .scmix_v25_settings_name,
    file_sha256 = unname(as.character(tools::sha256sum(path))))
  .scmix_v25_sign(payload, "scmix_v25_evaluator_settings")
}

.scmix_v25_source_root <- function(plan) {
  normalizePath(file.path(dirname(plan$files[["plan"]]), "..", ".."),
                mustWork = TRUE)
}

.scmix_v25_absolute_lexical_path <- function(path) {
  path <- path.expand(as.character(path))
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("A v2.5 canonical path must be one nonempty path.", call. = FALSE)
  }
  absolute <- grepl("^/", path) || grepl("^[[:alpha:]]:[/\\\\]", path)
  if (!absolute) path <- file.path(getwd(), path)
  path <- gsub("\\\\", "/", path)
  gsub("/+", "/", path)
}

.scmix_v25_canonical_state_dir <- function(
    plan, source_root = .scmix_v25_source_root(plan)) {
  if (!is.list(plan$canonical) ||
      !identical(plan$canonical$state_relative,
                 .scmix_v25_state_relative)) {
    stop("The frozen v2.5 canonical state-directory binding is absent.",
         call. = FALSE)
  }
  source_root <- normalizePath(source_root, mustWork = TRUE)
  .scmix_v25_reject_symlink_components(
    source_root, .scmix_v25_state_relative, "canonical state directory")
  state_dir <- file.path(source_root, .scmix_v25_state_relative)
  if (!dir.exists(state_dir)) {
    stop("The frozen v2.5 canonical state directory is absent.",
         call. = FALSE)
  }
  normalizePath(state_dir, mustWork = TRUE)
}

.scmix_v25_require_canonical_state_dir <- function(
    plan, state_dir = NULL, source_root = .scmix_v25_source_root(plan)) {
  canonical <- .scmix_v25_canonical_state_dir(plan, source_root)
  supplied <- if (is.null(state_dir)) canonical else state_dir
  if (!identical(.scmix_v25_absolute_lexical_path(supplied),
                 .scmix_v25_absolute_lexical_path(canonical))) {
    stop(paste0(
      "The outcome-capable v2.5 state machine requires its one frozen ",
      "canonical state directory."), call. = FALSE)
  }
  canonical
}

.scmix_v25_require_canonical_source_root <- function(plan, source_root) {
  canonical <- .scmix_v25_source_root(plan)
  supplied <- normalizePath(source_root, mustWork = TRUE)
  if (!identical(supplied, canonical) ||
      !.scmix_v25_canonical_source_root(supplied, plan)) {
    stop(paste0(
      "The outcome-capable v2.5 state machine requires the canonical ",
      "source root linked to the frozen plan."), call. = FALSE)
  }
  canonical_plan <- scmix_v25_validation_plan()
  if (!.scmix_v25_intact(canonical_plan, "scmix_v25_validation_plan") ||
      !identical(plan$fingerprint, canonical_plan$fingerprint)) {
    stop(paste0(
      "The outcome-capable v2.5 state machine requires the canonical ",
      "validated plan object."), call. = FALSE)
  }
  supplied
}

.scmix_v25_reject_symlink_components <- function(root, relative, label) {
  components <- strsplit(gsub("\\\\", "/", relative), "/",
                         fixed = TRUE)[[1L]]
  candidate <- path.expand(root)
  for (component in components) {
    candidate <- file.path(candidate, component)
    destination <- Sys.readlink(candidate)
    if (length(destination) == 1L && !is.na(destination) &&
        nzchar(destination)) {
      stop("The hash-bound v2.5 ", label,
           " must not contain symbolic links.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.scmix_v25_relative_file <- function(root, relative, label) {
  relative <- as.character(relative)
  if (length(relative) != 1L || is.na(relative) || !nzchar(relative) ||
      grepl("^/", relative) || grepl("(^|/)\\.\\.(/|$)", relative)) {
    stop("The v2.5 ", label, " must be a safe source-root-relative path.",
         call. = FALSE)
  }
  path <- file.path(root, relative)
  .scmix_v25_reject_symlink_components(root, relative, label)
  if (!file.exists(path)) {
    stop("The hash-bound v2.5 ", label, " is absent.", call. = FALSE)
  }
  normalized <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(normalizePath(root, mustWork = TRUE), .Platform$file.sep)
  if (!startsWith(normalized, prefix)) {
    stop("The hash-bound v2.5 ", label, " escapes the source root.",
         call. = FALSE)
  }
  normalized
}

.scmix_v25_required_implementation_paths <- function(root) {
  collect <- function(relative, pattern = NULL) {
    directory <- file.path(root, relative)
    if (!dir.exists(directory)) return(character())
    files <- list.files(
      directory, pattern = pattern, recursive = TRUE,
      full.names = FALSE, all.files = FALSE, no.. = TRUE)
    files <- files[file.info(file.path(directory, files))$isdir %in% FALSE]
    file.path(relative, files)
  }
  protocol <- collect("inst/protocol", "^inference-v2\\.5.*")
  state_names <- c(
    .scmix_v25_state_names,
    "inference-v2.5-evaluator-implementation.csv")
  protocol <- protocol[!basename(protocol) %in% state_names]
  paths <- c(
    "DESCRIPTION", "NAMESPACE", "INFERENCE_PROTOCOL_V25.md",
    collect("R", "\\.R$"), collect("src"), collect("man", "\\.Rd$"),
    "inst/calibration/v25/run-validation.R",
    "inst/calibration/v25/README.md",
    collect("inst/calibration/v25/source", "\\.R$"), protocol,
    collect("tests/testthat", "\\.R$"))
  paths <- sort(unique(paths[file.exists(file.path(root, paths))]),
                method = "radix")
  required <- c("R/calibration-v25.R",
             "R/calibration-v25-evaluator.R",
             "R/calibration-v25-fit.R", "R/calibration-v25-gates.R",
             "R/calibration-v25-runner.R", "R/calibration-v25-targets.R",
             "inst/calibration/v25/run-validation.R",
             "inst/calibration/v25/README.md",
             "inst/calibration/v25/source/calibration-v25.R",
             "inst/calibration/v25/source/calibration-v25-evaluator.R",
             "inst/protocol/inference-v2.5-evaluator-settings.dcf") %in%
           paths
  if (!all(required)) {
    stop("The canonical v2.5 implementation inventory is incomplete.",
         call. = FALSE)
  }
  mirror_pairs <- rbind(
    c("R/calibration-v25.R",
      "inst/calibration/v25/source/calibration-v25.R"),
    c("R/calibration-v25-evaluator.R",
      "inst/calibration/v25/source/calibration-v25-evaluator.R"))
  exact_copy <- apply(mirror_pairs, 1L, function(pair) {
    left <- file.path(root, pair[[1L]])
    right <- file.path(root, pair[[2L]])
    identical(readBin(left, "raw", n = file.info(left)$size),
              readBin(right, "raw", n = file.info(right)$size))
  })
  if (!all(exact_copy)) {
    stop("A required v2.5 source mirror does not match its package source.",
         call. = FALSE)
  }
  paths
}

.scmix_v25_canonical_source_root <- function(root, plan) {
  canonical <- file.path(
    root, "inst", "protocol", "inference-v2.5-validation-plan.dcf")
  file.exists(canonical) && identical(
    normalizePath(canonical, mustWork = TRUE),
    normalizePath(plan$files[["plan"]], mustWork = TRUE))
}

.scmix_v25_validate_implementation_manifest <- function(
    path, root, plan = NULL) {
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  required_columns <- c("role", "path", "sha256")
  required_roles <- c("state_and_outcome_generator",
                      "end_to_end_fit_and_infer", "family_finalizer",
                      "study_driver")
  if (!identical(names(manifest), required_columns) ||
      anyDuplicated(manifest$role) || anyDuplicated(manifest$path) ||
      !all(vapply(required_roles, function(role)
        sum(manifest$role == role) == 1L, logical(1L))) ||
      any(!grepl("^[[:xdigit:]]{64}$", manifest$sha256))) {
    stop("The v2.5 evaluator implementation manifest is malformed.",
         call. = FALSE)
  }
  files <- vapply(seq_len(nrow(manifest)), function(i)
    .scmix_v25_relative_file(root, manifest$path[[i]],
                             paste0("implementation file `",
                                    manifest$role[[i]], "`")), character(1L))
  observed <- vapply(files, function(file)
    unname(as.character(tools::sha256sum(file))), character(1L))
  if (!identical(unname(observed), unname(manifest$sha256))) {
    stop("A hash-bound v2.5 evaluator implementation file changed.",
         call. = FALSE)
  }
  if (!is.null(plan) && .scmix_v25_canonical_source_root(root, plan)) {
    required_paths <- .scmix_v25_required_implementation_paths(root)
    if (!identical(sort(as.character(manifest$path), method = "radix"),
                   required_paths)) {
      stop(paste0(
        "The v2.5 implementation manifest does not cover the complete ",
        "transitive source, protocol, documentation, and test inventory."),
        call. = FALSE)
    }
  }
  list(table = manifest, files = files, sha256 = observed)
}

.scmix_v25_validate_implementation_commit <- function(
    commit, root, implementation, plan) {
  if (!.scmix_v25_canonical_source_root(root, plan)) return(invisible(TRUE))
  git <- Sys.which("git")
  if (!nzchar(git)) {
    stop("Git is required to verify the v2.5 implementation commit.",
         call. = FALSE)
  }
  run <- function(arguments) suppressWarnings(system2(
    git, c("-C", shQuote(root), arguments), stdout = TRUE, stderr = TRUE))
  inside <- run(c("rev-parse", "--is-inside-work-tree"))
  if (!identical(attr(inside, "status") %||% 0L, 0L) ||
      length(inside) != 1L ||
      !identical(trimws(inside[[1L]]), "true")) {
    stop("The v2.5 source root is not a verifiable Git work tree.",
         call. = FALSE)
  }
  exists <- run(c("cat-file", "-e", paste0(commit, "^{commit}")))
  if (!identical(attr(exists, "status") %||% 0L, 0L)) {
    stop("The approved v2.5 implementation commit does not exist.",
         call. = FALSE)
  }
  ancestor <- run(c("merge-base", "--is-ancestor", commit, "HEAD"))
  if (!identical(attr(ancestor, "status") %||% 0L, 0L)) {
    stop("The approved v2.5 implementation commit is not in HEAD history.",
         call. = FALSE)
  }
  paths <- as.character(implementation$table$path)
  tree <- run(c("ls-tree", "-r", "--full-tree", commit, "--", paths))
  separator <- regexpr("\t", tree, fixed = TRUE)
  tree_paths <- ifelse(separator > 0L,
                       substring(tree, separator + 1L), NA_character_)
  tree_objects <- ifelse(
    separator > 0L,
    sub("^.* ([[:xdigit:]]+)$", "\\1",
        substring(tree, 1L, pmax(0L, separator - 1L))),
    NA_character_)
  located <- match(paths, tree_paths)
  if (!identical(attr(tree, "status") %||% 0L, 0L) ||
      anyNA(located) || anyDuplicated(tree_paths[located]) ||
      any(!grepl("^[[:xdigit:]]{40}([[:xdigit:]]{24})?$",
                 tree_objects[located]))) {
    stop("The approved implementation commit lacks a manifest file.",
         call. = FALSE)
  }
  working_objects <- run(c("hash-object", "--no-filters", "--", paths))
  if (!identical(attr(working_objects, "status") %||% 0L, 0L) ||
      length(working_objects) != length(paths) ||
      !identical(unname(working_objects),
                 unname(tree_objects[located]))) {
    stop(paste0(
      "The current v2.5 implementation differs from its approved commit."),
      call. = FALSE)
  }
  clean <- suppressWarnings(system2(
    git, c("-C", shQuote(root), "diff", "--quiet", commit, "--", paths),
    stdout = FALSE, stderr = FALSE))
  if (!identical(as.integer(clean), 0L)) {
    stop(paste0(
      "The current v2.5 implementation differs from its approved commit."),
      call. = FALSE)
  }
  invisible(TRUE)
}

.scmix_v25_read_one_dcf <- function(path, label) {
  x <- read.dcf(path)
  if (nrow(x) != 1L) {
    stop("The v2.5 ", label, " must contain one DCF record.",
         call. = FALSE)
  }
  as.list(x[1L, , drop = TRUE])
}

.scmix_v25_validate_evaluator_approval <- function(
    path, plan, source_root = .scmix_v25_source_root(plan)) {
  fields <- c(
    "Approval-Id", "Protocol-Id", "Plan-Id", "Status", "Approved-On",
    "Outcome-Count", "Application-Outcomes-Used",
    "Implementation-Commit", "Evaluator-Interface-Id",
    "Release-Algorithm-Id", "Evaluator-Settings-File",
    "Evaluator-Settings-SHA256", "Evaluator-Settings-Payload-SHA256",
    "Implementation-Manifest-File", "Implementation-Manifest-SHA256",
    "Runtime-Projection-Id")
  raw <- .scmix_v25_read_one_dcf(path, "evaluator approval")
  if (!identical(names(raw), fields) ||
      !identical(raw[["Approval-Id"]],
                 "paperps-v2.5-evaluator-approval-v1") ||
      !identical(raw[["Protocol-Id"]], plan$protocol_id) ||
      !identical(raw[["Plan-Id"]], plan$plan_id) ||
      !identical(raw[["Status"]], "approved_for_integrity_pilot") ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", raw[["Approved-On"]]) ||
      !identical(raw[["Outcome-Count"]], "0") ||
      !identical(raw[["Application-Outcomes-Used"]], "false") ||
      !grepl("^[[:xdigit:]]{40}([[:xdigit:]]{24})?$",
             raw[["Implementation-Commit"]]) ||
      !identical(raw[["Evaluator-Interface-Id"]],
                 .scmix_v25_evaluator_interface_id) ||
      !identical(raw[["Release-Algorithm-Id"]],
                 .scmix_v25_release_algorithm_id) ||
      !identical(raw[["Runtime-Projection-Id"]],
                 .scmix_v25_runtime_projection_id)) {
    stop("The v2.5 evaluator approval is malformed or not pre-outcome.",
         call. = FALSE)
  }
  settings_path <- .scmix_v25_relative_file(
    source_root, raw[["Evaluator-Settings-File"]], "evaluator settings")
  settings <- scmix_v25_evaluator_settings(plan, settings_path)
  if (!identical(raw[["Evaluator-Settings-SHA256"]],
                 settings$file_sha256) ||
      !identical(raw[["Evaluator-Settings-Payload-SHA256"]],
                 settings$fingerprint)) {
    stop("The evaluator approval does not bind the current v2.5 settings.",
         call. = FALSE)
  }
  manifest_path <- .scmix_v25_relative_file(
    source_root, raw[["Implementation-Manifest-File"]],
    "implementation manifest")
  manifest_hash <- unname(as.character(tools::sha256sum(manifest_path)))
  if (!identical(raw[["Implementation-Manifest-SHA256"]], manifest_hash)) {
    stop("The evaluator approval does not bind its implementation manifest.",
         call. = FALSE)
  }
  implementation <- .scmix_v25_validate_implementation_manifest(
    manifest_path, source_root, plan)
  .scmix_v25_validate_implementation_commit(
    raw[["Implementation-Commit"]], source_root, implementation, plan)
  .scmix_v25_sign(list(
    approval_id = raw[["Approval-Id"]], protocol_id = plan$protocol_id,
    plan_id = plan$plan_id, status = raw[["Status"]], raw = raw,
    path = normalizePath(path, mustWork = TRUE),
    sha256 = unname(as.character(tools::sha256sum(path))),
    settings = settings, implementation = implementation,
    application_outcomes_used = FALSE, outcome_count = 0L),
    "scmix_v25_evaluator_approval")
}

.scmix_v25_expected_pilot_jobs <- function(cell_ids) {
  expand.grid(cell_id = as.character(cell_ids), replication = 1:2,
              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

## Runtime is not extrapolated from target results or per-target diagnostics.
## The exact complete 12-job pilot uses one job on each deterministic lane.
## Evidence has 25 jobs on each lane, so its projected wall time is 25 times
## the pilot wall time with the frozen 1.25 safety factor.
scmix_v25_runtime_projection <- function(
    pilot_jobs, wave_started_unix, wave_finished_unix, workers = 12L,
    cell_ids = NULL, maximum_hours = 8) {
  if (!is.data.frame(pilot_jobs)) {
    stop("`pilot_jobs` must be a pilot metadata data frame.", call. = FALSE)
  }
  required <- c("cell_id", "replication", "completed", "worker_id",
                "elapsed_seconds")
  if (!identical(names(pilot_jobs), required)) {
    stop("The pilot metadata schema changed.", call. = FALSE)
  }
  if (is.null(cell_ids)) cell_ids <- unique(as.character(pilot_jobs$cell_id))
  cell_ids <- as.character(cell_ids)
  expected <- .scmix_v25_expected_pilot_jobs(cell_ids)
  observed <- pilot_jobs[order(match(pilot_jobs$cell_id, cell_ids),
                               pilot_jobs$replication), , drop = FALSE]
  expected <- expected[order(match(expected$cell_id, cell_ids),
                             expected$replication), , drop = FALSE]
  rownames(observed) <- rownames(expected) <- NULL
  workers <- .scmix_v25_number(workers, "Pilot-Workers", TRUE)
  started <- .scmix_v25_number(wave_started_unix, "Pilot-Wave-Started")
  finished <- .scmix_v25_number(wave_finished_unix, "Pilot-Wave-Finished")
  maximum_hours <- .scmix_v25_number(maximum_hours,
                                     "Maximum-Projected-Wall-Hours")
  expected_worker <- 2L *
    (match(as.character(observed$cell_id), cell_ids) - 1L) +
    as.integer(observed$replication)
  if (length(cell_ids) != 6L || anyDuplicated(cell_ids) || nrow(observed) != 12L ||
      !identical(observed[, c("cell_id", "replication"), drop = FALSE],
                 expected) || workers != 12L ||
      !all(as.logical(observed$completed)) ||
      anyNA(observed$completed) ||
      any(!is.finite(observed$elapsed_seconds)) ||
      any(observed$elapsed_seconds <= 0) ||
      !is.numeric(observed$worker_id) || anyNA(observed$worker_id) ||
      any(!is.finite(observed$worker_id)) ||
      any(observed$worker_id != as.integer(observed$worker_id)) ||
      !identical(sort(unique(as.integer(observed$worker_id))), 1:12) ||
      !identical(as.integer(observed$worker_id), expected_worker) ||
      finished <= started) {
    stop("The runtime projection requires one complete exact 12-worker, one-job-per-lane pilot wave.",
         call. = FALSE)
  }
  wall_seconds <- finished - started
  projected_seconds <- 1.25 * 25 * wall_seconds
  .scmix_v25_sign(list(
    rule_id = .scmix_v25_runtime_projection_id,
    pilot_jobs = 12L, pilot_workers = 12L,
    pilot_wave_wall_seconds = wall_seconds, multiplier = 25,
    safety_factor = 1.25,
    projected_evidence_wall_seconds = projected_seconds,
    projected_evidence_wall_hours = projected_seconds / 3600,
    maximum_projected_wall_hours = maximum_hours,
    pass = projected_seconds / 3600 <= maximum_hours),
    "scmix_v25_runtime_projection")
}

.scmix_v25_state_paths <- function(plan, state_dir = NULL) {
  if (is.null(state_dir)) state_dir <- dirname(plan$files[["plan"]])
  stats::setNames(file.path(state_dir, .scmix_v25_state_names),
                  names(.scmix_v25_state_names))
}

.scmix_v25_validate_pilot_approval <- function(
    path, plan, evaluator, source_root = .scmix_v25_source_root(plan)) {
  fields <- c(
    "Approval-Id", "Protocol-Id", "Plan-Id", "Status", "Approved-On",
    "Evaluator-Approval-SHA256", "Pilot-Run-Manifest-File",
    "Pilot-Run-Manifest-SHA256", "Pilot-Wave-Started-Unix",
    "Pilot-Wave-Finished-Unix", "Pilot-Workers", "Completed-Jobs",
    "Projected-Evidence-Wall-Seconds", "Projected-Evidence-Wall-Hours",
    "Runtime-Projection-Id", "Integrity-Status",
    "Target-Or-Gate-Results-Inspected", "Application-Outcomes-Used")
  raw <- .scmix_v25_read_one_dcf(path, "integrity-pilot approval")
  if (!identical(names(raw), fields) ||
      !identical(raw[["Approval-Id"]], "paperps-v2.5-pilot-approval-v1") ||
      !identical(raw[["Protocol-Id"]], plan$protocol_id) ||
      !identical(raw[["Plan-Id"]], plan$plan_id) ||
      !identical(raw[["Status"]], "passed_integrity_and_runtime") ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", raw[["Approved-On"]]) ||
      !identical(raw[["Evaluator-Approval-SHA256"]], evaluator$sha256) ||
      !identical(raw[["Pilot-Workers"]], "12") ||
      !identical(raw[["Completed-Jobs"]], "12") ||
      !identical(raw[["Runtime-Projection-Id"]],
                 .scmix_v25_runtime_projection_id) ||
      !identical(raw[["Integrity-Status"]], "pass") ||
      !identical(raw[["Target-Or-Gate-Results-Inspected"]], "false") ||
      !identical(raw[["Application-Outcomes-Used"]], "false")) {
    stop("The v2.5 integrity-pilot approval is malformed.", call. = FALSE)
  }
  manifest_path <- .scmix_v25_relative_file(
    source_root, raw[["Pilot-Run-Manifest-File"]], "pilot run manifest")
  manifest_hash <- unname(as.character(tools::sha256sum(manifest_path)))
  if (!identical(raw[["Pilot-Run-Manifest-SHA256"]], manifest_hash)) {
    stop("The pilot approval does not bind its run manifest.", call. = FALSE)
  }
  jobs <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                          check.names = FALSE)
  projection <- scmix_v25_runtime_projection(
    jobs, raw[["Pilot-Wave-Started-Unix"]],
    raw[["Pilot-Wave-Finished-Unix"]], workers = 12L,
    cell_ids = plan$cells$cell_id,
    maximum_hours = plan$counts$maximum_projected_hours)
  recorded_seconds <- .scmix_v25_number(
    raw[["Projected-Evidence-Wall-Seconds"]],
    "Projected-Evidence-Wall-Seconds")
  recorded_hours <- .scmix_v25_number(
    raw[["Projected-Evidence-Wall-Hours"]],
    "Projected-Evidence-Wall-Hours")
  if (abs(recorded_seconds - projection$projected_evidence_wall_seconds) >
      1e-8 * max(1, recorded_seconds) ||
      abs(recorded_hours - projection$projected_evidence_wall_hours) >
      1e-10 * max(1, recorded_hours) || !isTRUE(projection$pass)) {
    stop("The pilot runtime projection is incorrect or exceeds eight hours.",
         call. = FALSE)
  }
  .scmix_v25_sign(list(
    approval_id = raw[["Approval-Id"]], protocol_id = plan$protocol_id,
    plan_id = plan$plan_id, status = raw[["Status"]], raw = raw,
    path = normalizePath(path, mustWork = TRUE),
    sha256 = unname(as.character(tools::sha256sum(path))), jobs = jobs,
    runtime = projection, application_outcomes_used = FALSE),
    "scmix_v25_pilot_approval")
}

.scmix_v25_validate_evidence_completion <- function(
    path, plan, evaluator, pilot,
    source_root = .scmix_v25_source_root(plan)) {
  fields <- c(
    "Completion-Id", "Protocol-Id", "Plan-Id", "Status", "Completed-On",
    "Evaluator-Approval-SHA256", "Pilot-Approval-SHA256",
    "Evidence-Run-Manifest-File", "Evidence-Run-Manifest-SHA256",
    "Expected-Attempts", "Attempted-Replications",
    "Application-Outcomes-Used")
  raw <- .scmix_v25_read_one_dcf(path, "evidence completion")
  if (!identical(names(raw), fields) ||
      !identical(raw[["Completion-Id"]],
                 "paperps-v2.5-evidence-completion-v1") ||
      !identical(raw[["Protocol-Id"]], plan$protocol_id) ||
      !identical(raw[["Plan-Id"]], plan$plan_id) ||
      !identical(raw[["Status"]], "all_prespecified_attempts_finished") ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", raw[["Completed-On"]]) ||
      !identical(raw[["Evaluator-Approval-SHA256"]], evaluator$sha256) ||
      !identical(raw[["Pilot-Approval-SHA256"]], pilot$sha256) ||
      !identical(raw[["Expected-Attempts"]], "300") ||
      !identical(raw[["Attempted-Replications"]], "300") ||
      !identical(raw[["Application-Outcomes-Used"]], "false")) {
    stop("The v2.5 evidence-completion record is malformed.", call. = FALSE)
  }
  manifest_path <- .scmix_v25_relative_file(
    source_root, raw[["Evidence-Run-Manifest-File"]],
    "evidence run manifest")
  manifest_hash <- unname(as.character(tools::sha256sum(manifest_path)))
  if (!identical(raw[["Evidence-Run-Manifest-SHA256"]], manifest_hash)) {
    stop("The evidence-completion record does not bind its run manifest.",
         call. = FALSE)
  }
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  required <- c("cell_id", "replication", "completed", "artifact_file",
                "artifact_sha256")
  expected <- expand.grid(
    cell_id = plan$cells$cell_id,
    replication = seq_len(plan$counts$evidence_reps),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  key <- paste(manifest$cell_id, manifest$replication, sep = "\r")
  expected_key <- paste(expected$cell_id, expected$replication, sep = "\r")
  if (!all(required %in% names(manifest)) || nrow(manifest) != 300L ||
      anyDuplicated(key) || !setequal(key, expected_key) ||
      anyNA(manifest$completed) ||
      any(nzchar(manifest$artifact_sha256) &
            !grepl("^[[:xdigit:]]{64}$", manifest$artifact_sha256))) {
    stop("The v2.5 evidence run manifest is incomplete or malformed.",
         call. = FALSE)
  }
  .scmix_v25_sign(list(
    completion_id = raw[["Completion-Id"]], protocol_id = plan$protocol_id,
    plan_id = plan$plan_id, status = raw[["Status"]], raw = raw,
    path = normalizePath(path, mustWork = TRUE),
    sha256 = unname(as.character(tools::sha256sum(path))),
    manifest = manifest, completed_replications = sum(manifest$completed),
    application_outcomes_used = FALSE),
    "scmix_v25_evidence_completion")
}

.scmix_v25_evidence_artifact_file <- function(
    source_root, path, cell_id, replication) {
  path <- as.character(path)
  label <- paste0("evidence artifact for `", cell_id, "` replication ",
                  replication)
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("The completed v2.5 ", label, " is absent.", call. = FALSE)
  }
  if (!grepl("^/", path)) {
    return(.scmix_v25_relative_file(source_root, path, label))
  }
  if (!file.exists(path)) {
    stop("The completed v2.5 ", label, " is absent.", call. = FALSE)
  }
  normalized <- normalizePath(path, mustWork = TRUE)
  root <- normalizePath(source_root, mustWork = TRUE)
  if (!startsWith(normalized, paste0(root, .Platform$file.sep))) {
    stop("The hash-bound v2.5 ", label, " escapes the source root.",
         call. = FALSE)
  }
  normalized
}

## Reconstruct the family-by-cell result from the evidence manifest and every
## hash-bound completed artifact.  Family approval uses this reconstruction,
## never the result CSV's submitted `pass` column, as its decision input.
.scmix_v25_recompute_family_result <- function(
    plan, evaluator, evidence,
    source_root = .scmix_v25_source_root(plan)) {
  if (!.scmix_v25_intact(plan, "scmix_v25_validation_plan") ||
      !.scmix_v25_intact(evaluator, "scmix_v25_evaluator_approval") ||
      !.scmix_v25_intact(evidence, "scmix_v25_evidence_completion") ||
      !identical(evidence$protocol_id, plan$protocol_id) ||
      !identical(evidence$plan_id, plan$plan_id)) {
    stop("Intact linked v2.5 plan, evaluator, and evidence records are required.",
         call. = FALSE)
  }
  manifest_relative <- evidence$raw[["Evidence-Run-Manifest-File"]]
  recorded_manifest_hash <- evidence$raw[["Evidence-Run-Manifest-SHA256"]]
  if (is.null(manifest_relative) || is.null(recorded_manifest_hash) ||
      !grepl("^[[:xdigit:]]{64}$", recorded_manifest_hash)) {
    stop("The v2.5 evidence record does not bind a run manifest.",
         call. = FALSE)
  }
  manifest_path <- .scmix_v25_relative_file(
    source_root, manifest_relative, "evidence run manifest")
  observed_manifest_hash <- unname(as.character(
    tools::sha256sum(manifest_path)))
  if (!identical(observed_manifest_hash, recorded_manifest_hash)) {
    stop("The v2.5 evidence run manifest changed after completion.",
         call. = FALSE)
  }
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  required <- c("cell_id", "replication", "completed", "artifact_file",
                "artifact_sha256")
  expected <- expand.grid(
    cell_id = as.character(plan$cells$cell_id),
    replication = seq_len(plan$counts$evidence_reps),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (!all(required %in% names(manifest)) ||
      nrow(manifest) != nrow(expected)) {
    stop("The hash-bound v2.5 evidence manifest has the wrong schedule.",
         call. = FALSE)
  }
  key <- paste(manifest$cell_id, manifest$replication, sep = "\r")
  expected_key <- paste(expected$cell_id, expected$replication, sep = "\r")
  index <- match(expected_key, key)
  completed <- manifest$completed
  artifact_file <- as.character(manifest$artifact_file)
  artifact_hash <- as.character(manifest$artifact_sha256)
  if (anyDuplicated(key) || anyNA(index) || !is.logical(completed) ||
      anyNA(completed) ||
      any(completed & (is.na(artifact_file) | !nzchar(artifact_file))) ||
      any(completed & (is.na(artifact_hash) |
                         !grepl("^[[:xdigit:]]{64}$", artifact_hash))) ||
      any(!completed & !is.na(artifact_file) & nzchar(artifact_file)) ||
      any(!completed & !is.na(artifact_hash) & nzchar(artifact_hash))) {
    stop("The hash-bound v2.5 evidence manifest is malformed.",
         call. = FALSE)
  }
  manifest <- manifest[index, , drop = FALSE]
  completed <- completed[index]
  family_ids <- as.character(plan$targets$family_id)
  rows <- vector("list", nrow(manifest) * length(family_ids))
  position <- 0L
  expected_implementation_hash <-
    evaluator$raw[["Implementation-Manifest-SHA256"]]
  for (i in seq_len(nrow(manifest))) {
    artifact <- NULL
    if (completed[[i]]) {
      artifact_path <- .scmix_v25_evidence_artifact_file(
        source_root, manifest$artifact_file[[i]], manifest$cell_id[[i]],
        manifest$replication[[i]])
      observed_hash <- unname(as.character(tools::sha256sum(artifact_path)))
      if (!identical(observed_hash,
                     as.character(manifest$artifact_sha256[[i]]))) {
        stop("A completed evidence artifact no longer matches its manifest.",
             call. = FALSE)
      }
      artifact <- .scmix_v25_read_artifact(artifact_path, manifest[i, ])
      if (!identical(artifact$evaluator_approval_sha256, evaluator$sha256) ||
          !identical(artifact$protocol_id, plan$protocol_id) ||
          !identical(artifact$plan_id, plan$plan_id) ||
          !identical(artifact$stream, "evidence") ||
          !identical(artifact$implementation_manifest_sha256,
                     expected_implementation_hash)) {
        stop("An evidence artifact is bound to another evaluator approval.",
             call. = FALSE)
      }
      family_cell <- artifact$family_cell
      if (!is.data.frame(family_cell) ||
          !all(c("family_id", "released", "covered") %in%
               names(family_cell)) ||
          nrow(family_cell) != length(family_ids) ||
          anyDuplicated(as.character(family_cell$family_id)) ||
          !setequal(as.character(family_cell$family_id), family_ids)) {
        stop("An evidence artifact has an incomplete family result.",
             call. = FALSE)
      }
    }
    for (family in family_ids) {
      position <- position + 1L
      value <- if (is.null(artifact)) NULL else
        artifact$family_cell[
          as.character(artifact$family_cell$family_id) == family,
          , drop = FALSE]
      if (!is.null(value)) {
        released <- value$released[[1L]]
        covered <- value$covered[[1L]]
        if (!is.logical(released) || length(released) != 1L ||
            is.na(released) || !is.logical(covered) ||
            length(covered) != 1L ||
            (!isTRUE(released) && !is.na(covered))) {
          stop("An evidence artifact has a malformed family result.",
               call. = FALSE)
        }
      }
      rows[[position]] <- data.frame(
        cell_id = as.character(manifest$cell_id[[i]]), family_id = family,
        replication = as.integer(manifest$replication[[i]]),
        completed = completed[[i]],
        released = if (is.null(value)) FALSE else value$released[[1L]],
        covered = if (is.null(value)) NA else value$covered[[1L]],
        stringsAsFactors = FALSE)
    }
  }
  replication <- do.call(rbind, rows)
  assessment <- .scmix_v25_assess_validation(
    replication, plan$approval,
    expected_reps = plan$counts$evidence_reps)
  result <- assessment$family_cell
  result$pass <- ifelse(result$required, result$passed, TRUE)
  result$passed <- NULL
  rownames(result) <- NULL
  result
}

.scmix_v25_family_result_matches <- function(observed, expected) {
  if (!is.data.frame(observed) || !is.data.frame(expected) ||
      !identical(names(observed), names(expected)) ||
      nrow(observed) != nrow(expected)) return(FALSE)
  for (name in names(expected)) {
    lhs <- observed[[name]]
    rhs <- expected[[name]]
    if (is.logical(rhs)) {
      if (!is.logical(lhs) || !identical(lhs, rhs)) return(FALSE)
    } else if (is.integer(rhs)) {
      if (is.logical(lhs) && all(is.na(lhs))) lhs <- as.integer(lhs)
      if (!is.numeric(lhs) || anyNA(lhs) != anyNA(rhs) ||
          !identical(is.na(lhs), is.na(rhs)) ||
          any(!is.na(lhs) & lhs != as.integer(lhs)) ||
          !identical(as.integer(lhs), rhs)) return(FALSE)
    } else if (is.numeric(rhs)) {
      if (is.logical(lhs) && all(is.na(lhs))) lhs <- as.numeric(lhs)
      if (!is.numeric(lhs) || !identical(is.na(lhs), is.na(rhs))) return(FALSE)
      keep <- !is.na(rhs)
      if (any(!is.finite(lhs[keep])) ||
          any(abs(lhs[keep] - rhs[keep]) >
                1e-12 * pmax(1, abs(rhs[keep])))) return(FALSE)
    } else if (!identical(as.character(lhs), as.character(rhs))) {
      return(FALSE)
    }
  }
  TRUE
}

.scmix_v25_validate_family_approval <- function(
    path, plan, evaluator, evidence,
    source_root = .scmix_v25_source_root(plan)) {
  fields <- c(
    "Approval-Id", "Protocol-Id", "Plan-Id", "Status", "Approved-On",
    "Evaluator-Approval-SHA256", "Evidence-Completion-SHA256",
    "Approval-Matrix-SHA256", "Family-Result-Manifest-File",
    "Family-Result-Manifest-SHA256", "Approved-Families",
    "Rejected-Families", "Discretionary-Override",
    "Application-Outcomes-Used")
  raw <- .scmix_v25_read_one_dcf(path, "family approval")
  matrix_hash <- unname(as.character(tools::sha256sum(
    plan$files[["approval"]])))
  if (!identical(names(raw), fields) ||
      !identical(raw[["Approval-Id"]], "paperps-v2.5-family-approval-v1") ||
      !identical(raw[["Protocol-Id"]], plan$protocol_id) ||
      !identical(raw[["Plan-Id"]], plan$plan_id) ||
      !identical(raw[["Status"]], "mechanical_family_results_recorded") ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", raw[["Approved-On"]]) ||
      !identical(raw[["Evaluator-Approval-SHA256"]], evaluator$sha256) ||
      !identical(raw[["Evidence-Completion-SHA256"]], evidence$sha256) ||
      !identical(raw[["Approval-Matrix-SHA256"]], matrix_hash) ||
      !identical(raw[["Discretionary-Override"]], "false") ||
      !identical(raw[["Application-Outcomes-Used"]], "false")) {
    stop("The v2.5 family approval is malformed or not mechanical.",
         call. = FALSE)
  }
  manifest_path <- .scmix_v25_relative_file(
    source_root, raw[["Family-Result-Manifest-File"]],
    "family result manifest")
  manifest_hash <- unname(as.character(tools::sha256sum(manifest_path)))
  if (!identical(raw[["Family-Result-Manifest-SHA256"]], manifest_hash)) {
    stop("The family approval does not bind its result manifest.",
         call. = FALSE)
  }
  submitted_result <- utils::read.csv(
    manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  recomputed_result <- .scmix_v25_recompute_family_result(
    plan, evaluator, evidence, source_root)
  required <- c("family_id", "cell_id", "required", "criterion", "pass")
  if (!all(required %in% names(submitted_result))) {
    stop("The mechanical v2.5 family result manifest is malformed.",
         call. = FALSE)
  }
  key <- paste(submitted_result$family_id, submitted_result$cell_id, sep = "\r")
  expected_key <- paste(plan$approval$family_id, plan$approval$cell_id,
                        sep = "\r")
  index <- match(expected_key, key)
  if (nrow(submitted_result) != 36L || anyNA(index) || anyDuplicated(key)) {
    stop("The mechanical v2.5 family result manifest is malformed.",
         call. = FALSE)
  }
  submitted_result <- submitted_result[index, , drop = FALSE]
  rownames(submitted_result) <- NULL
  if (!.scmix_v25_family_result_matches(
      submitted_result, recomputed_result)) {
    stop(paste0(
      "The mechanical v2.5 family result manifest does not reproduce ",
      "the hash-bound evidence artifacts."), call. = FALSE)
  }
  result <- recomputed_result
  family_ids <- as.character(plan$targets$family_id)
  approved <- family_ids[vapply(family_ids, function(family) {
    rows <- result$family_id == family & result$required
    all(result$pass[rows])
  }, logical(1L))]
  rejected <- setdiff(family_ids, approved)
  parse_family_list <- function(x) {
    if (identical(x, "none")) return(character())
    strsplit(x, ",", fixed = TRUE)[[1L]]
  }
  if (!identical(parse_family_list(raw[["Approved-Families"]]), approved) ||
      !identical(parse_family_list(raw[["Rejected-Families"]]), rejected)) {
    stop("The family approval does not record the mechanical result.",
         call. = FALSE)
  }
  .scmix_v25_sign(list(
    approval_id = raw[["Approval-Id"]], protocol_id = plan$protocol_id,
    plan_id = plan$plan_id, status = raw[["Status"]], raw = raw,
    path = normalizePath(path, mustWork = TRUE),
    sha256 = unname(as.character(tools::sha256sum(path))), result = result,
    approved_families = approved, rejected_families = rejected,
    discretionary_override = FALSE, application_outcomes_used = FALSE),
    "scmix_v25_family_approval")
}

.scmix_v25_try_state <- function(path, validator) {
  if (!file.exists(path)) {
    return(list(present = FALSE, valid = FALSE, value = NULL,
                error = NULL))
  }
  tryCatch(
    list(present = TRUE, valid = TRUE, value = validator(path), error = NULL),
    error = function(e) list(present = TRUE, valid = FALSE, value = NULL,
                             error = conditionMessage(e)))
}

.scmix_v25_evaluator_readiness <- function(
    plan = scmix_v25_validation_plan(), state_dir = NULL,
    source_root = .scmix_v25_source_root(plan), require_canonical = TRUE) {
  if (!.scmix_v25_intact(plan, "scmix_v25_validation_plan")) {
    stop("An intact frozen v2.5 plan is required.", call. = FALSE)
  }
  if (isTRUE(require_canonical)) {
    source_root <- .scmix_v25_require_canonical_source_root(plan, source_root)
    state_dir <- .scmix_v25_require_canonical_state_dir(
      plan, state_dir, source_root)
  }
  paths <- .scmix_v25_state_paths(plan, state_dir)
  evaluator <- .scmix_v25_try_state(paths[["evaluator"]], function(path)
    .scmix_v25_validate_evaluator_approval(path, plan, source_root))
  pilot <- if (evaluator$valid) {
    .scmix_v25_try_state(paths[["pilot"]], function(path)
      .scmix_v25_validate_pilot_approval(
        path, plan, evaluator$value, source_root))
  } else list(present = file.exists(paths[["pilot"]]), valid = FALSE,
              value = NULL, error = if (file.exists(paths[["pilot"]]))
                "Pilot approval exists without a valid evaluator approval."
              else NULL)
  evidence <- if (pilot$valid) {
    .scmix_v25_try_state(paths[["evidence"]], function(path)
      .scmix_v25_validate_evidence_completion(
        path, plan, evaluator$value, pilot$value, source_root))
  } else list(present = file.exists(paths[["evidence"]]), valid = FALSE,
              value = NULL, error = if (file.exists(paths[["evidence"]]))
                "Evidence completion exists without a valid pilot approval."
              else NULL)
  family <- if (evidence$valid) {
    .scmix_v25_try_state(paths[["family"]], function(path)
      .scmix_v25_validate_family_approval(
        path, plan, evaluator$value, evidence$value, source_root))
  } else list(present = file.exists(paths[["family"]]), valid = FALSE,
              value = NULL, error = if (file.exists(paths[["family"]]))
                "Family approval exists before valid evidence completion."
              else NULL)
  invalid <- c(evaluator = evaluator$present && !evaluator$valid,
               pilot = pilot$present && !pilot$valid,
               evidence = evidence$present && !evidence$valid,
               family = family$present && !family$valid)
  invalid_name <- names(invalid)[which(invalid)[1L]]
  pilot_permitted <- evaluator$valid && !pilot$valid && !any(invalid)
  evidence_permitted <- evaluator$valid && pilot$valid &&
    !evidence$valid && !any(invalid)
  status <- if (any(invalid)) {
    paste0("invalid_", invalid_name, "_state")
  } else if (!evaluator$valid) {
    "not_ready_single_evaluator_approval_absent"
  } else if (!pilot$valid) {
    "ready_for_integrity_pilot"
  } else if (!evidence$valid) {
    "ready_for_evidence_study"
  } else if (!family$valid) {
    "evidence_complete_family_approval_absent"
  } else {
    "validation_complete_family_results_recorded"
  }
  error <- c(evaluator = evaluator$error %||% "",
             pilot = pilot$error %||% "",
             evidence = evidence$error %||% "",
             family = family$error %||% "")
  .scmix_v25_sign(list(
    plan_id = plan$plan_id, protocol_id = plan$protocol_id,
    prospective_plan_frozen = TRUE, threshold_selection_absent = TRUE,
    calibration_stream_absent = TRUE, response_free_designs_frozen = TRUE,
    respondent_superpopulation_resampling_frozen = TRUE,
    deterministic_family_cell_approval_frozen = TRUE,
    exact_uncentered_rms_dgp_frozen = TRUE,
    single_end_to_end_evaluator_required = TRUE,
    evaluator_approval_present = evaluator$present,
    evaluator_approval_valid = evaluator$valid,
    integrity_pilot_completed = pilot$valid,
    evidence_study_completed = evidence$valid,
    family_approval_present = family$present,
    family_approval_valid = family$valid,
    pilot_outcome_generation_permitted = pilot_permitted,
    evidence_outcome_generation_permitted = evidence_permitted,
    outcome_generation_permitted = pilot_permitted || evidence_permitted,
    state_paths = paths, state_errors = error[nzchar(error)],
    evaluator = evaluator$value, pilot = pilot$value,
    evidence = evidence$value, family = family$value, status = status),
    "scmix_v25_readiness")
}

.scmix_v25_generator_with_rng <- function(seed, fun) {
  seed <- .scmix_v25_number(seed, "Replication-Seed", TRUE)
  if (seed < 0L) stop("A v2.5 seed cannot be negative.", call. = FALSE)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind(kind = "L'Ecuyer-CMRG", normal.kind = "Inversion",
          sample.kind = "Rejection")
  set.seed(seed)
  fun()
}

.scmix_v25_generate_from_template <- function(
    template, geometry, stream, cell_index, replication, seeds) {
  needed_template <- c("deltaX", "Z", "respondent_id", "task_id",
                       "design_id", "subset_payload_hash")
  needed_geometry <- c("fixed_index", "random_score", "cell_id",
                       "design_id", "dgp_rank")
  if (!is.list(template) || !all(needed_template %in% names(template)) ||
      !is.list(geometry) || !all(needed_geometry %in% names(geometry)) ||
      !identical(as.character(template$design_id),
                 as.character(geometry$design_id))) {
    stop("A linked v2.5 template and DGP geometry are required.",
         call. = FALSE)
  }
  respondent <- as.character(template$respondent_id)
  ids <- unique(respondent)
  split_rows <- split(seq_along(respondent),
                      factor(respondent, levels = ids), drop = TRUE)
  n <- length(ids)
  if (n != 400L || any(lengths(split_rows) < 1L) ||
      length(geometry$fixed_index) != length(respondent) ||
      length(geometry$random_score) != length(respondent)) {
    stop("The v2.5 generator requires the complete frozen 400-respondent template.",
         call. = FALSE)
  }
  sampled <- .scmix_v25_generator_with_rng(seeds[["resample"]], function()
    sample.int(n, n, replace = TRUE))
  selected_rows <- split_rows[sampled]
  source_row_index <- unlist(selected_rows, use.names = FALSE)
  new_group <- rep.int(seq_len(n), lengths(selected_rows))
  new_ids <- sprintf("v25_%s_c%02d_r%03d_i%04d", substr(stream, 1L, 1L),
                     as.integer(cell_index), as.integer(replication),
                     seq_len(n))
  latent <- .scmix_v25_generator_with_rng(seeds[["latent"]], function()
    stats::rnorm(n))
  eta <- as.numeric(geometry$fixed_index[source_row_index] +
                      geometry$random_score[source_row_index] *
                        latent[new_group])
  probability <- stats::plogis(eta)
  y <- .scmix_v25_generator_with_rng(seeds[["choice"]], function()
    stats::rbinom(length(probability), size = 1L, prob = probability))
  source_id_by_draw <- ids[sampled]
  source_id_by_row <- source_id_by_draw[new_group]
  if (!identical(source_id_by_row, respondent[source_row_index]) ||
      any(!is.finite(probability)) || any(probability <= 0 | probability >= 1) ||
      any(!y %in% 0:1)) {
    stop("The complete-sequence v2.5 outcome generator failed an invariant.",
         call. = FALSE)
  }
  payload <- list(
    protocol_id = .scmix_v25_protocol_id,
    plan_id = .scmix_v25_plan_id, stream = stream,
    cell_id = as.character(geometry$cell_id),
    design_id = as.character(template$design_id),
    cell_index = as.integer(cell_index), replication = as.integer(replication),
    seed = as.integer(seeds[["resample"]]),
    seeds = as.list(stats::setNames(as.integer(seeds), names(seeds))),
    respondent_id = new_ids[new_group],
    respondent_id_unique = new_ids,
    source_respondent_id = source_id_by_row,
    selected_source_respondent_id = source_id_by_draw,
    selected_source_index = as.integer(sampled),
    source_row_index = as.integer(source_row_index),
    task_id = as.character(template$task_id[source_row_index]),
    position = if (is.null(template$position)) NULL else
      template$position[source_row_index],
    deltaX = unname(template$deltaX[source_row_index, , drop = FALSE]),
    deltaX_names = template$deltaX_names %||% colnames(template$deltaX),
    Z = unname(template$Z[source_row_index, , drop = FALSE]),
    Z_names = template$Z_names %||% colnames(template$Z),
    y = as.integer(y), observed = rep(TRUE, length(y)),
    synthetic_probability = probability,
    latent_u = latent, dgp_rank = as.integer(geometry$dgp_rank),
    source_template_fingerprint = template$fingerprint %||% NA_character_,
    source_payload_hash = template$subset_payload_hash,
    application_outcomes_used = FALSE, synthetic_outcome = TRUE,
    generator = "complete_sequence_resampled_q1_mixed_logit_v2.5",
    resampling = paste0(
      "400_complete_respondent_sequences_with_replacement_from_frozen_",
      "template_with_new_replication_ids"),
    residual_distribution =
      "standard_normal_one_persistent_draw_per_resampled_respondent")
  .scmix_v25_sign(payload, "scmix_v25_generated_outcome")
}

.scmix_v25_evaluator_generate_study_outcome <- function(
    cell_id, replication, stream = c("pilot", "evidence"),
    plan = scmix_v25_validation_plan(), state_dir = NULL,
    source_root = .scmix_v25_source_root(plan)) {
  stream <- match.arg(stream)
  if (!.scmix_v25_intact(plan, "scmix_v25_validation_plan")) {
    stop("An intact frozen v2.5 plan is required.", call. = FALSE)
  }
  readiness <- .scmix_v25_evaluator_readiness(
    plan, state_dir = state_dir, source_root = source_root)
  permitted <- if (identical(stream, "pilot"))
    readiness$pilot_outcome_generation_permitted else
      readiness$evidence_outcome_generation_permitted
  if (!isTRUE(permitted)) {
    detail <- if (length(readiness$state_errors))
      paste(unname(readiness$state_errors), collapse = " ") else
        readiness$status
    stop("Protocol v2.5 outcome locked for the `", stream,
         "` stream. ", detail, call. = FALSE)
  }
  cell_index <- match(cell_id, plan$cells$cell_id)
  if (is.na(cell_index)) {
    stop("`cell_id` is not in the frozen v2.5 registry.", call. = FALSE)
  }
  replication <- .scmix_v25_number(replication, "Replication", TRUE)
  maximum <- if (identical(stream, "pilot")) plan$counts$pilot_reps else
    plan$counts$evidence_reps
  if (replication < 1L || replication > maximum) {
    stop("`replication` is outside the frozen v2.5 stream.", call. = FALSE)
  }
  master <- if (identical(stream, "pilot")) 1910000000L else 2010000000L
  base <- as.integer(master + (cell_index - 1L) * 1000000L + replication)
  seeds <- c(resample = base, latent = base + 200000L,
             choice = base + 400000L, fit = base + 500000L,
             multiplier = base + 600000L)
  ## `load_design()` is keyed by design, while the generator is keyed by cell.
  design_id <- plan$cells$design_id[[cell_index]]
  template <- scmix_v25_load_design(design_id, plan)
  geometry <- scmix_v25_dgp_geometry(cell_id, plan)
  out <- .scmix_v25_generate_from_template(
    template, geometry, stream, cell_index, replication, seeds)
  out$evaluator_approval_sha256 <- readiness$evaluator$sha256
  out$evaluator_settings_fingerprint <-
    readiness$evaluator$settings$fingerprint
  out$plan_fingerprint <- plan$fingerprint
  out$dgp_geometry_fingerprint <- geometry$fingerprint
  out$truth_table_sha256 <- unname(as.character(tools::sha256sum(
    plan$files[["truths"]])))
  payload <- unclass(out)
  payload$fingerprint <- NULL
  .scmix_v25_sign(payload, "scmix_v25_generated_outcome")
}
