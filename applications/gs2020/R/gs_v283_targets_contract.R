## Graham--Svolik v2.8.3 target-stage contract and implementation helpers.
##
## This file has no top-level outcome-generating action.  It consumes only the
## fresh immutable q=1 v2.8.3 assembly and preflight checkpoint. It never reads
## q=0 or q=2 results and never reads or resumes a v2.8.1 or v2.8.2 target
## checkpoint. The five-target core is post-diagnostic but prospective with
## respect to the new v2.8.3 Graham--Svolik outcomes.

.gs_v283t_protocol_id <- "gs2020-paperps-targets-2026-09-07-v2.8.3-v1"
.gs_v283t_parent_protocol_id <- "gs2020-paperps-prospective-2026-09-07-v2.8.3-v1"
.gs_v283t_output_relative <- file.path(
  "runs", "gs2020", "v283", "prospective-2026-09-07-v1", "targets")
.gs_v283t_supported_status <-
  "post_diagnostic_same_sample_intervals_descriptive_and_conditional"
.gs_v283t_unavailable_status <- "unavailable_not_invented"
.gs_v283t_family_draws <- 9999L
.gs_v283t_level <- 0.95
.gs_v283t_riesz_cutoff <- 0.20
.gs_v283t_riesz_validation_fraction <- 0.20
.gs_v283t_riesz_correction_cutoff <- 0.20
.gs_v283t_ridge_movement_cutoff <- 0.20
.gs_v283t_variance_floor <- 1e-6
.gs_v283t_group_margin <- 0.05
.gs_v283t_heterogeneity_margin <- 1e-4
.gs_v283t_multiplier_block <- 250L
.gs_v283t_near_objective_cutoff <- 1e-6
.gs_v283t_target_movement_cutoff <- 0.10
.gs_v283t_loading_rank_tolerance <- 1e-8

.gs_v283t_stop <- function(...) stop(..., call. = FALSE)

.gs_v283t_sha256 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  exe <- Sys.which("shasum")
  if (!nzchar(exe)) .gs_v283t_stop("The `shasum` executable is required.")
  line <- system2(exe, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    .gs_v283t_stop("Could not compute SHA-256 for ", path, ".")
  }
  sub("[[:space:]].*$", "", line)
}

.gs_v283t_object_sha256 <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .gs_v283t_stop("The `digest` package is required.")
  }
  digest::digest(x, algo = "sha256", serialize = TRUE,
                 serializeVersion = 2L)
}

.gs_v283t_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = character())
}

.gs_v283t_read_dcf <- function(path) {
  x <- read.dcf(path, all = TRUE)
  if (nrow(x) != 1L) .gs_v283t_stop("Contract must contain exactly one row: ", path)
  unlist(x[1L, , drop = TRUE], use.names = TRUE)
}

.gs_v283t_protocol_dir <- function(repo_root) {
  file.path(normalizePath(repo_root, winslash = "/", mustWork = TRUE),
            "applications", "gs2020", "protocol", "v283-targets")
}

.gs_v283t_output_dir <- function(repo_root) {
  file.path(normalizePath(repo_root, winslash = "/", mustWork = TRUE),
            .gs_v283t_output_relative)
}

.gs_v283t_git_snapshot <- function(repo_root, implementation_paths,
                                  require_clean = TRUE) {
  git <- Sys.which("git")
  if (!nzchar(git)) .gs_v283t_stop("Git is required for target execution.")
  implementation_paths <- as.character(implementation_paths)
  if (!length(implementation_paths) || anyNA(implementation_paths) ||
      any(!nzchar(implementation_paths)) ||
      anyDuplicated(implementation_paths)) {
    .gs_v283t_stop("The target implementation path registry is malformed.")
  }
  commit <- system2(git, c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
                    stdout = TRUE, stderr = TRUE)
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    .gs_v283t_stop("Could not identify the target-stage Git commit.")
  }
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--untracked-files=all"), stdout = TRUE,
                    stderr = TRUE)
  listed <- system2(git, c("-C", shQuote(repo_root), "ls-files"),
                    stdout = TRUE, stderr = TRUE)
  protocol_files <- unlist(lapply(c("v283", "v283-targets"), function(version) {
    files <- list.files(
      file.path(repo_root, "applications", "gs2020", "protocol", version),
      recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE)
    file.path("applications", "gs2020", "protocol", version, files)
  }), use.names = FALSE)
  loaded_r <- list.files(
    file.path(repo_root, "R"), pattern = "[.]R$", recursive = TRUE,
    full.names = FALSE)
  loaded_r <- file.path("R", loaded_r)
  manifested_r <- implementation_paths[startsWith(implementation_paths, "R/")]
  required <- unique(c(implementation_paths, protocol_files))
  if (isTRUE(require_clean) && length(status)) {
    .gs_v283t_stop("Target protocol or implementation is uncommitted or modified: ",
                  paste(status, collapse = "; "))
  }
  if (isTRUE(require_clean) && !all(required %in% listed)) {
    .gs_v283t_stop("Every target-stage implementation and protocol file must be tracked before outcomes.")
  }
  if (isTRUE(require_clean) && !setequal(loaded_r, manifested_r)) {
    .gs_v283t_stop(paste(
      "The target manifest must hash every package R file loaded by pkgload",
      "and no unregistered R file may be present."))
  }
  list(commit = commit, clean = !length(status), status = status,
       tracked = listed)
}

.gs_v283t_validate_protocol <- function(repo_root, require_clean = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  protocol_dir <- .gs_v283t_protocol_dir(repo_root)
  contract <- .gs_v283t_read_dcf(file.path(protocol_dir,
                                           "target-stage-contract.dcf"))
  if (!identical(unname(contract[["Protocol-Id"]]), .gs_v283t_protocol_id) ||
      !identical(unname(contract[["Parent-Protocol-Id"]]),
                 .gs_v283t_parent_protocol_id) ||
      !identical(unname(contract[["Output-Directory"]]),
                 gsub("\\\\", "/", .gs_v283t_output_relative))) {
    .gs_v283t_stop("Unexpected target-stage identity or output location.")
  }
  exact <- c(
    `Historical-Target-Protocol` = "gs2020-targeted-q1-2026-09-03",
    `Historical-Target-Role` = "diagnostic_comparator_only",
    `Historical-Disclosure` = paste0(
      "earlier_GS_q1_targets_GS_v282_target_results_were_seen_but_",
      "invalidated_and_not_reused_and_BR_v281_results_were_seen_so_this_",
      "core_is_post_diagnostic_but_prospective_with_respect_to_its_own_",
      "GS_v283_outcomes"),
    `Prior-V282-Target-Role` =
      "invalidated_diagnostic_seen_but_not_reused",
    `Upstream-Fit` = "exact_hash_bound_v283_q1_assembled_checkpoint",
    `Upstream-Preflight` =
      "exact_hash_bound_v283_pre_estimation_verified_input_checkpoint",
    `Fresh-Upstream-Requirement` =
      "v283_q1_assembly_and_preflight_generated_by_exact_parent_protocol",
    `Prior-Target-Checkpoint-Reuse` =
      "prohibited_including_v281_or_v282_partial_or_complete_checkpoints",
    `Historical-Target-Estimate-Reuse` = "prohibited",
    `Checkpoint-Pair-Recovery` = paste0(
      "validate_installed_envelope_and_binding_then_recreate_only_",
      "missing_sha_sidecar"),
    `Prospective-Boundary` =
      "no_GS_v283_target_outcome_existed_when_this_protocol_was_written",
    `Target-Rank` = "1",
    `Core-Target-Selection` = paste0(
      "exactly_five_outcome_informed_post_diagnostic_manuscript_targets_",
      "in_core-target-registry.csv"),
    `Core-Target-Count` = "5",
    `Core-Family-Count` = "2",
    `Riesz-Rule` = "target_specific_training_only_validation",
    `Riesz-Grid` = "0,Ntrain^-1,Ntrain^-3/4,Ntrain^-1/2",
    `Riesz-Validation-Fraction` = "0.20",
    `Riesz-Tie-Rule` = paste0(
      "largest_ridge_when_absolute_risk_gap_le_1e-12_times_max_1_",
      "abs_minimum"),
    `Riesz-Numerical-Rank-Rule` = "dimension_epsilon",
    `Riesz-Preconditioning` = paste0(
      "structural_Gram_whitening_plus_diagonal_congruence_",
      "equilibration_of_full_penalized_system"),
    `Riesz-Rank-Assessment` = "after_diagonal_congruence_equilibration",
    `Riesz-Zero-Ridge-Singular-Rule` =
      "retain_frozen_cutoff_numerical_pseudoinverse_representative",
    `Riesz-Ill-Conditioned-Positive-Ridge-Rule` =
      "withhold_never_truncate",
    `Target-Execution` =
      "one_target_specific_DML_call_per_registered_target",
    `Target-Seed-Rule` =
      "family_base_seed_plus_registered_target_index",
    `Family-Assembly` =
      "transform_target_influences_then_apply_one_common_multiplier_bank",
    `Crossfit-HZ-Aggregation` =
      "global_crossfit_second_moment_minus_squared_global_crossfit_mean",
    `Crossfit-HZ-Riesz-Gradient-Center` = "outer_training_only",
    `Crossfit-HZ-Support-Moments` = "required_global_assembly_inputs",
    `Riesz-Defect-Cutoff` = "0.20",
    `Riesz-Correction-Relative-Cutoff` = "0.20",
    `Ridge-Movement-Cutoff` = "0.20",
    `Derivative-Cutoff` = "0.0001",
    `Null-Space-Cutoff` = "0.000001",
    `Fit-Blockwise-KKT-Cutoff` = "0.00001",
    `Fit-Near-Optimum-Relative-Cutoff` = "0.000001",
    `Target-Near-Optimum-Selected-Movement-Cutoff-SE` = "0.10",
    `Loading-Invariance-Cutoff` = "0.00000001",
    `Loading-Rank-Tolerance` = "0.00000001",
    `Analytic-Derivatives-Only` = "true",
    `Common-Family-Multiplier-Draws` = "9999",
    `Familywise-Level` = "0.95",
    `Choice-GH-Nodes` = "31",
    `Directional-Variance-Floor` = "0.000001",
    `Subgroup-Probability-Margin` = "0.05",
    `Directional-Heterogeneity-Total-Margin` = "0.0001",
    `Multiplier-Block-Size` = "250",
    `Formal-Release` =
      "withheld_post_diagnostic_same_sample_and_pending_complete_gates",
    `Confirmatory-Claim-Eligibility` = paste0(
      "independent_data_or_genuinely_pre_result_registry_or_",
      "valid_selective_inference"),
    `Q2-Target-Selection-Role` = "none",
    `Untracked-File-Rule` = "reject_all_visible_untracked_files",
    `Loaded-Package-R-Coverage` =
      "every_loaded_R_file_exactly_hashed_and_tracked",
    `RNG-Kind` = "L'Ecuyer-CMRG,Inversion,Rejection",
    `Runtime-Wrapper` = "applications/bin/Rscript45_exact_hash",
    `Runtime-Link-Rule` =
      "create_once_or_validate_exact_target_never_replace_during_concurrent_runs",
    `Runtime-Binding` = paste0(
      "R_executable_sha256_R_version_platform_package_versions_",
      "native_torch_version_BLAS_and_single_thread_state"),
    `Manifest-Finalization` = "completed_before_first_outcome_command")
  observed_exact <- if (all(names(exact) %in% names(contract))) {
    vapply(contract[names(exact)], function(x) {
      if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
        .gs_v283t_stop("Each frozen target-stage field must be one nonempty string.")
      }
      x
    }, character(1L))
  } else {
    character()
  }
  if (!identical(unname(observed_exact), unname(exact))) {
    .gs_v283t_stop("A frozen target-stage statistical rule changed.")
  }

  checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
  lines <- readLines(checksum_path, warn = FALSE)
  pieces <- strsplit(lines, "  ", fixed = TRUE)
  if (!length(lines) || any(lengths(pieces) != 2L)) {
    .gs_v283t_stop("Malformed target-stage checksum registry.")
  }
  expected <- vapply(pieces, `[[`, character(1L), 1L)
  names(expected) <- vapply(pieces, `[[`, character(1L), 2L)
  actual_protocol <- setdiff(list.files(
    protocol_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    "CHECKSUMS.sha256")
  if (anyDuplicated(names(expected)) ||
      any(grepl("^/|(^|/)\\.\\.(/|$)", names(expected))) ||
      !setequal(names(expected), actual_protocol)) {
    .gs_v283t_stop("The target checksum registry does not cover exactly ",
                  "every protocol file.")
  }
  observed <- vapply(names(expected), function(name) {
    .gs_v283t_sha256(file.path(protocol_dir, name))
  }, character(1L))
  if (!identical(unname(observed), unname(expected))) {
    .gs_v283t_stop("A target-stage protocol file differs from CHECKSUMS.sha256.")
  }

  frozen <- .gs_v283t_read_csv(file.path(protocol_dir,
                                         "frozen-registry-manifest.csv"))
  for (i in seq_len(nrow(frozen))) {
    path <- file.path(repo_root, frozen$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.gs_v283t_sha256(path), frozen$sha256[[i]])) {
      .gs_v283t_stop("Frozen dependency hash mismatch: ",
                    frozen$relative_path[[i]], ".")
    }
  }
  implementation <- .gs_v283t_read_csv(file.path(
    protocol_dir, "implementation-manifest.csv"))
  implementation_paths <- implementation$relative_path
  if (anyDuplicated(implementation_paths) ||
      any(grepl("^/|(^|/)\\.\\.(/|$)", implementation_paths)) ||
      any(implementation$sha256 == "PENDING_FINALIZATION") ||
      !"applications/bin/Rscript45" %in% implementation_paths) {
    .gs_v283t_stop("The target implementation manifest is unsafe or ",
                  "unfinished.")
  }
  for (i in seq_len(nrow(implementation))) {
    path <- file.path(repo_root, implementation$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.gs_v283t_sha256(path), implementation$sha256[[i]])) {
      .gs_v283t_stop("Target implementation hash mismatch: ",
                    implementation$relative_path[[i]], ".")
    }
  }
  target_types <- .gs_v283t_read_csv(file.path(protocol_dir,
                                               "target-type-registry.csv"))
  target_registry <- .gs_v283t_read_csv(file.path(
    protocol_dir, "core-target-registry.csv"))
  family_registry <- .gs_v283t_read_csv(file.path(
    protocol_dir, "core-family-registry.csv"))
  primitive_registry <- .gs_v283t_read_csv(file.path(
    protocol_dir, "core-primitive-registry.csv"))
  if (!all(unique(target_registry$target_type) %in%
           target_types$target_type) ||
      anyDuplicated(target_types$target_type) || nrow(target_registry) != 5L ||
      !setequal(unique(target_registry$multiplicity_family),
                family_registry$family_id)) {
    .gs_v283t_stop("Target-type coverage does not exactly cover the v2.8.3 core registry.")
  }
  expected_supported <- target_types$status[
    match(target_registry$target_type, target_types$target_type)] == "supported"
  core_ids <- c("share_oppose_vote_restriction",
                "compensating_coparty_vote_restriction",
                "heterogeneity_vote_restriction")
  context_ids <- c("mean_coparty_utility",
                   "choice_coparty_vote_restrictor")
  if (!all(expected_supported) ||
      !identical(target_registry$target_id, c(context_ids, core_ids)) ||
      !all(tolower(target_registry$post_diagnostic_manuscript_core) ==
             "true") ||
      !identical(family_registry$family_id,
                 c("context_preferences", "heterogeneity_core")) ||
      !all(family_registry$release_rule ==
             "same_sample_formal_release_prohibited") ||
      !identical(family_registry$reporting_role,
                 c("post_diagnostic_context",
                   "post_diagnostic_heterogeneity_core")) ||
      nrow(primitive_registry[primitive_registry$family_id ==
                                "heterogeneity_core", , drop = FALSE]) != 4L) {
    .gs_v283t_stop("The v2.8.3 core, context, or primitive registry changed.")
  }
  seeds <- .gs_v283t_read_csv(file.path(protocol_dir, "seed-registry.csv"))
  if (!identical(seeds$family_id, family_registry$family_id) ||
      anyDuplicated(c(seeds$dml_seed, seeds$multiplier_seed)) ||
      !all(seeds$multiplier_draws == .gs_v283t_family_draws)) {
    .gs_v283t_stop("Family seed registry is incomplete or changed.")
  }
  git <- .gs_v283t_git_snapshot(
    repo_root, implementation_paths,
    require_clean = require_clean)
  list(protocol_id = .gs_v283t_protocol_id, protocol_dir = protocol_dir,
       protocol_hash = .gs_v283t_sha256(checksum_path), contract = contract,
       frozen = frozen, implementation = implementation,
       target_types = target_types, targets = target_registry,
       families = family_registry, primitives = primitive_registry,
       seeds = seeds, git = git)
}

.gs_v283t_registries <- function(repo_root, protocol) {
  base <- protocol$protocol_dir
  list(
    targets = protocol$targets,
    contrasts = .gs_v283t_read_csv(file.path(base, "contrast-registry.csv")),
    groups = .gs_v283t_read_csv(file.path(base, "group-registry.csv")),
    families = protocol$families,
    target_types = protocol$target_types)
}

.gs_v283t_binding <- function(protocol, kind, family_id = "all",
                             upstream = NULL) {
  out <- list(
    protocol_id = protocol$protocol_id,
    protocol_hash = protocol$protocol_hash,
    git_commit = protocol$git$commit,
    parent_protocol_id = .gs_v283t_parent_protocol_id,
    application = "gs2020", rank = 1L, kind = kind,
    family_id = family_id,
    runtime = .gs_v283_runtime_binding(),
    rng_kind = .gs_v283_assert_rng_contract(),
    q2_used_for_target_selection = FALSE)
  if (!is.null(upstream)) out$upstream <- upstream
  out
}

.gs_v283t_checkpoint_path <- function(output_dir, kind, family_id = NULL) {
  if (identical(kind, "binding")) return(file.path(output_dir, "binding.rds"))
  if (identical(kind, "basis")) return(file.path(output_dir, "basis.rds"))
  if (identical(kind, "export")) return(file.path(output_dir, "export.rds"))
  if (identical(kind, "family")) {
    if (is.null(family_id) || !nzchar(family_id)) {
      .gs_v283t_stop("A family checkpoint requires family_id.")
    }
    return(file.path(output_dir, "families", paste0(family_id, ".rds")))
  }
  .gs_v283t_stop("Unknown target checkpoint kind: ", kind, ".")
}

.gs_v283t_save_atomic <- function(payload, path, binding) {
  if (!is.list(binding) ||
      !identical(binding$protocol_id, .gs_v283t_protocol_id)) {
    .gs_v283t_stop("Target checkpoint requires the exact target-stage binding.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) || file.exists(sidecar)) {
    .gs_v283t_stop("Refusing to overwrite immutable target checkpoint: ", path)
  }
  token <- paste0(Sys.getpid(), "-", as.integer(Sys.time()))
  tmp <- paste0(path, ".tmp-", token)
  tmp_sidecar <- paste0(sidecar, ".tmp-", token)
  on.exit(unlink(c(tmp, tmp_sidecar), force = TRUE), add = TRUE)
  envelope <- list(
    schema_version = "gs2020-v2.8.3-target-atomic-checkpoint-v1",
    binding = binding,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    payload = payload)
  saveRDS(envelope, tmp, version = 3L)
  hash <- .gs_v283t_sha256(tmp)
  writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
  if (!file.rename(tmp, path)) .gs_v283t_stop("Could not install ", path, ".")
  if (!file.rename(tmp_sidecar, sidecar)) {
    .gs_v283t_stop("Checkpoint installed but SHA sidecar failed: ", path)
  }
  invisible(list(path = path, sha256 = hash))
}

.gs_v283t_read_checkpoint <- function(path, binding = NULL,
                                     payload = TRUE) {
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) && !file.exists(sidecar) && !is.null(binding)) {
    object <- tryCatch(readRDS(path), error = function(e) NULL)
    required <- names(binding)
    valid <- is.list(object) && identical(
      object$schema_version,
      "gs2020-v2.8.3-target-atomic-checkpoint-v1") &&
      identical(object$binding$protocol_id, .gs_v283t_protocol_id) &&
      all(required %in% names(object$binding)) &&
      identical(object$binding[required], binding[required])
    if (!isTRUE(valid)) {
      .gs_v283t_stop("Cannot recover an incomplete or foreign target checkpoint: ",
                    path)
    }
    hash <- .gs_v283t_sha256(path)
    token <- paste0(Sys.getpid(), "-", as.integer(Sys.time()))
    tmp_sidecar <- paste0(sidecar, ".tmp-", token)
    on.exit(unlink(tmp_sidecar, force = TRUE), add = TRUE)
    writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
    if (!file.rename(tmp_sidecar, sidecar)) {
      .gs_v283t_stop("Could not recover the target checkpoint SHA sidecar: ",
                    path)
    }
  }
  if (!file.exists(path) || !file.exists(sidecar)) {
    .gs_v283t_stop("Incomplete target checkpoint: ", path)
  }
  expected <- strsplit(readLines(sidecar, warn = FALSE)[[1L]],
                       "[[:space:]]+")[[1L]][1L]
  observed <- .gs_v283t_sha256(path)
  if (!identical(expected, observed)) {
    .gs_v283t_stop("Target checkpoint SHA mismatch: ", path)
  }
  object <- readRDS(path)
  if (!is.list(object) ||
      !identical(object$schema_version,
                 "gs2020-v2.8.3-target-atomic-checkpoint-v1") ||
      !identical(object$binding$protocol_id, .gs_v283t_protocol_id)) {
    .gs_v283t_stop("Malformed or foreign target checkpoint: ", path)
  }
  if (!is.null(binding)) {
    required <- names(binding)
    if (!all(required %in% names(object$binding)) ||
        !identical(object$binding[required], binding[required])) {
      .gs_v283t_stop("Target checkpoint binding mismatch: ", path)
    }
  }
  if (!isTRUE(payload)) {
    return(list(path = path, sha256 = observed, binding = object$binding))
  }
  object
}

.gs_v283t_git_is_ancestor <- function(repo_root, commit) {
  git <- Sys.which("git")
  if (!nzchar(git)) .gs_v283t_stop("Git is required for provenance checks.")
  status <- system2(
    git, c("-C", shQuote(repo_root), "merge-base", "--is-ancestor",
           shQuote(commit), "HEAD"), stdout = FALSE, stderr = FALSE)
  identical(as.integer(status), 0L)
}

.gs_v283t_assert_upstream_binding <- function(object, v28_protocol, kind,
                                             q = NA_integer_,
                                             scope = NA_character_) {
  expected <- list(
    protocol_id = v28_protocol$protocol_id,
    protocol_hash = v28_protocol$protocol_hash,
    application = "gs2020", kind = kind, q = as.integer(q),
    scope = as.character(scope),
    runtime = .gs_v283_runtime_binding(),
    rng_kind = .gs_v283_assert_rng_contract(),
    input_hashes = stats::setNames(v28_protocol$inputs$sha256,
                                   v28_protocol$inputs$input_id))
  actual <- object$binding
  if (!is.list(actual) || !all(names(expected) %in% names(actual)) ||
      !identical(actual[names(expected)], expected) ||
      !is.character(actual$git_commit) || length(actual$git_commit) != 1L ||
      !grepl("^[0-9a-f]{40}$", actual$git_commit)) {
    .gs_v283t_stop("Upstream v2.8.3 checkpoint binding mismatch for ", kind, ".")
  }
  actual$git_commit
}

.gs_v283t_verify_upstream <- function(repo_root, v28_protocol) {
  run_dir <- .gs_v283_run_dir(repo_root)
  assembled_path <- .gs_v283_checkpoint_path(run_dir, "assembled", 1L)
  preflight_path <- .gs_v283_checkpoint_path(run_dir, "preflight")
  ## Read with the parent's own sidecar/schema check first.  The checkpoint's
  ## run commit is intentionally allowed to precede the later target-stage
  ## commit.  Every other binding field is matched to the still-frozen parent
  ## protocol below, and both upstream checkpoints must name the same run
  ## commit.
  assembled <- .gs_v283_read_checkpoint(assembled_path)
  preflight <- .gs_v283_read_checkpoint(preflight_path)
  assembled_commit <- .gs_v283t_assert_upstream_binding(
    assembled, v28_protocol, "assembled", 1L, "all")
  preflight_commit <- .gs_v283t_assert_upstream_binding(
    preflight, v28_protocol, "preflight")
  if (!identical(assembled_commit, preflight_commit)) {
    .gs_v283t_stop("Upstream assembly and preflight name different run commits.")
  }
  if (!.gs_v283t_git_is_ancestor(repo_root, assembled_commit)) {
    .gs_v283t_stop(
      "The upstream generating commit is not an ancestor of current HEAD.")
  }
  ap <- assembled$payload
  if (!is.list(ap) ||
      !identical(ap$schema_version, "gs2020-v2.8.3-assembled-v1") ||
      !identical(ap$q, 1L) ||
      !inherits(ap$assembled, "scmix_nested_assembled") ||
      !identical(ap$assembled$q, 1L) ||
      !isTRUE(ap$no_historical_estimate_reuse) ||
      !identical(ap$inference_role,
        "post_diagnostic_same_sample_DML_descriptive_and_conditional")) {
    .gs_v283t_stop("The upstream q=1 assembly is not the frozen v2.8.3 object.")
  }
  pp <- preflight$payload
  if (!is.list(pp) ||
      !identical(pp$schema_version, "gs2020-v2.8.3-verified-input-v1") ||
      !isFALSE(pp$preparation_uses_outcome) ||
      !isFALSE(pp$prior_full_fit_available)) {
    .gs_v283t_stop("The upstream verified-input checkpoint is malformed.")
  }
  list(
    assembled = ap, preflight = pp,
    hashes = list(
      q1_assembled_sha256 = .gs_v283t_sha256(assembled_path),
      q1_assembled_sidecar_sha256 = .gs_v283t_sha256(
        paste0(assembled_path, ".sha256")),
      preflight_sha256 = .gs_v283t_sha256(preflight_path),
      preflight_sidecar_sha256 = .gs_v283t_sha256(
        paste0(preflight_path, ".sha256")),
      parent_run_git_commit = assembled_commit),
    paths = list(q1_assembled = assembled_path, preflight = preflight_path))
}

.gs_v283t_load_binding <- function(repo_root, protocol, v28_protocol) {
  output_dir <- .gs_v283t_output_dir(repo_root)
  upstream <- .gs_v283t_verify_upstream(repo_root, v28_protocol)
  path <- .gs_v283t_checkpoint_path(output_dir, "binding")
  binding <- .gs_v283t_binding(protocol, "binding", upstream = upstream$hashes)
  object <- .gs_v283t_read_checkpoint(path, binding = binding)
  if (!identical(object$payload$upstream_hashes, upstream$hashes) ||
      !isFALSE(object$payload$q2_used_for_target_selection) ||
      !isFALSE(object$payload$historical_target_estimates_used) ||
      !isFALSE(object$payload$prior_target_checkpoint_reuse) ||
      !isFALSE(object$payload$v281_partial_checkpoint_reuse) ||
      !isFALSE(object$payload$v282_partial_checkpoint_reuse) ||
      !isTRUE(object$payload$v282_target_results_seen) ||
      !isTRUE(object$payload$v282_target_results_invalidated) ||
      !isFALSE(object$payload$v282_target_estimates_reused)) {
    .gs_v283t_stop("Target binding checkpoint does not match the upstream hashes.")
  }
  list(checkpoint = object, upstream = upstream, binding = binding)
}

.gs_v283t_contrast <- function(registries, id, attr_names) {
  row <- registries$contrasts[
    registries$contrasts$contrast_id == id, , drop = FALSE]
  if (nrow(row) != 1L || !all(attr_names %in% names(row))) {
    .gs_v283t_stop("Unknown or malformed frozen contrast: ", id, ".")
  }
  out <- as.numeric(row[1L, attr_names, drop = TRUE])
  names(out) <- attr_names
  if (any(!is.finite(out))) .gs_v283t_stop("Nonfinite contrast: ", id, ".")
  out
}

.gs_v283t_groups <- function(preflight, respondent_id, registries) {
  respondent_id <- as.character(respondent_id)
  if (!length(respondent_id) || anyNA(respondent_id) ||
      anyDuplicated(respondent_id)) {
    .gs_v283t_stop("Malformed respondent IDs for the overall group.")
  }
  out <- list(overall = stats::setNames(rep(1, length(respondent_id)),
                                        respondent_id))
  required <- registries$groups$group_id
  if (!setequal(names(out), required)) {
    .gs_v283t_stop("Implemented groups do not match the frozen registry.")
  }
  out
}

.gs_v283t_position_target <- function(label = "position_effect") {
  force(label)
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                       attr_names) {
    N <- nrow(mu); p <- ncol(mu)
    list(
      target_type = "rowwise_expectation",
      value = matrix(rep(kappa, N), ncol = 1L),
      d_mu = array(0, c(N, 1L, p)),
      d_kappa = matrix(1, N, 1L),
      sigma_invariant = TRUE,
      labels = label)
  }
  attr(callback, "scmix_v22_metadata_template") <- list(
    source_type = "position_effect", recommended_target_type =
      "unrestricted_scalar", requires_signed_registry = TRUE)
  class(callback) <- c("scmix_rowwise_target", "function")
  callback
}

.gs_v283t_recipe <- function(target_id, target_type, primitive_names,
                            kind = "identity") {
  list(target_id = target_id, target_type = target_type,
       primitive_names = primitive_names, kind = kind)
}

.gs_v283t_heterogeneity_registry <- function(rows) {
  ids <- rows$target_id
  primitive_by_target <- lapply(ids, function(id) {
    paste0(id, c(":H_Z", ":H_R"))
  })
  names(primitive_by_target) <- ids
  family_primitives <- unname(unlist(primitive_by_target, use.names = FALSE))
  out <- lapply(ids, function(id) {
    sconjoint::scmix_v22_target_spec(
      target_id = id,
      family_id = rows$multiplicity_family[[1L]],
      target_type = "directional_heterogeneity",
      supporting_names = paste0(
        id, c(":mean", ":second_moment", ":residual_variance")),
      primitive_registry_ids = primitive_by_target[[id]],
      orientation = "positive_registered_direction",
      units = "squared_utility_index",
      parameter_dependencies = c("mean", "loading"),
      family_primitive_registry_ids = family_primitives,
      family_target_ids = ids,
      confirmatory_status = "outcome_informed",
      uses_numerical_integration = FALSE,
      reference_rule = "respondent_equal_finite_population_variance_N",
      influence_definition =
        paste0(
          "global_empirical_PZ_HZ_with_outer_training_centered_Riesz_",
          "gradient_and_HR_respondent_sequence"),
      primitive_estimands = c(
        "finite_population_variance_of_directional_conditional_mean",
        "directional_residual_variance"),
      primitive_orientations = c("nonnegative", "nonnegative"),
      primitive_units = rep("squared_utility_index", 2L),
      primitive_reference_rules = rep(
        "respondent_equal_finite_population_variance_N", 2L),
      primitive_influence_definitions = c(
        paste0(
          "global_crossfit_centered_squared_directional_mean_plus_outer_",
          "training_centered_Riesz_correction"),
        "directional_residual_variance_plus_Riesz_correction"))
  })
  names(out) <- ids
  out
}

.gs_v283t_build_family <- function(family_id, fit, preflight, registries) {
  rows <- registries$targets[
    registries$targets$multiplicity_family == family_id, , drop = FALSE]
  rows <- rows[order(as.integer(rows$target_index)), , drop = FALSE]
  if (!nrow(rows)) .gs_v283t_stop("Unknown frozen target family: ", family_id)
  type_status <- registries$target_types$status[
    match(rows$target_type, registries$target_types$target_type)]
  if (anyNA(type_status)) .gs_v283t_stop("Unmapped target type in ", family_id)
  if (any(type_status == "unavailable")) {
    if (!all(type_status == "unavailable")) {
      .gs_v283t_stop("A family cannot mix supported and unavailable targets.")
    }
    reason <- unique(registries$target_types$reason[
      match(rows$target_type, registries$target_types$target_type)])
    return(list(family_id = family_id, status = .gs_v283t_unavailable_status,
                rows = rows, targets = list(), recipes = list(),
                reason = paste(reason, collapse = "; ")))
  }
  attr_names <- as.character(fit$attr_names)
  if (!identical(attr_names, c(
    "diff_respParty", "diff_p1_num", "diff_p2_num",
    "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
    "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
    "diff_dem_code_g_record", "diff_dem_code_g_schedule",
    "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
    "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
    "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
    "diff_dem_code_u_limitVote", "diff_dem_code_v_affair",
    "diff_dem_code_v_tax", "diff_sex_Female", "diff_race_Asian",
    "diff_race_Black", "diff_race_Hispanic", "diff_pro_Farmer",
    "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
    "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
    "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
    "diff_pro_Teacher"))) {
    .gs_v283t_stop("Unexpected Graham--Svolik coefficient order.")
  }
  respondents <- unique(as.character(fit$respondent_id))
  groups <- .gs_v283t_groups(preflight, respondents, registries)
  targets <- recipes <- list()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    id <- row$target_id[[1L]]
    type <- row$target_type[[1L]]
    group_id <- row$group_id[[1L]]
    contrast <- if (row$contrast_id[[1L]] == "none") NULL else
      .gs_v283t_contrast(registries, row$contrast_id[[1L]], attr_names)
    if (type == "position_effect") {
      targets[[id]] <- .gs_v283t_position_target(id)
      recipes[[id]] <- .gs_v283t_recipe(id, type, id)
    } else if (type == "mean") {
      targets[[id]] <- sconjoint::scmix_inference_target(
        type = "tau", contrast = contrast, label = id)
      recipes[[id]] <- .gs_v283t_recipe(id, type, id)
    } else if (type == "subgroup_mean") {
      base <- sconjoint::scmix_inference_target(
        type = "tau", contrast = contrast, label = id)
      targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
      recipes[[id]] <- .gs_v283t_recipe(
        id, type, paste0(id, c(":weighted_target",
                               ":subgroup_probability")), "subgroup_ratio")
    } else if (type %in% c("choice_probability",
                            "subgroup_choice_probability")) {
      base <- sconjoint::scmix_inference_target(
        type = "choice", contrast = contrast, position_neutral = TRUE,
        n_nodes = 31L, label = id)
      if (type == "choice_probability") {
        targets[[id]] <- base
        recipes[[id]] <- .gs_v283t_recipe(id, type, id)
      } else {
        targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
        recipes[[id]] <- .gs_v283t_recipe(
          id, type, paste0(id, c(":weighted_target",
                                 ":subgroup_probability")), "subgroup_ratio")
      }
    } else if (type %in% c("directional_share",
                            "subgroup_directional_share")) {
      base <- sconjoint::scmix_inference_target(
        type = "sign", contrast = contrast,
        variance_floor = .gs_v283t_variance_floor, label = id)
      if (type == "directional_share") {
        targets[[id]] <- base
        recipes[[id]] <- .gs_v283t_recipe(id, type, id)
      } else {
        targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
        recipes[[id]] <- .gs_v283t_recipe(
          id, type, paste0(id, c(":weighted_target",
                                 ":subgroup_probability")), "subgroup_ratio")
      }
    } else if (type %in% c("compensating_benefit_share",
                            "subgroup_compensating_benefit_share")) {
      benefit <- .gs_v283t_contrast(
        registries, row$benefit_contrast_id[[1L]], attr_names)
      amount <- as.numeric(row$benefit_amount[[1L]])
      if (!is.finite(amount)) .gs_v283t_stop("Invalid benefit amount for ", id)
      base <- sconjoint::scmix_inference_target(
        type = "compensating", penalty = contrast, benefit = benefit,
        amount = amount, variance_floor = .gs_v283t_variance_floor,
        label = id)
      if (type == "compensating_benefit_share") {
        targets[[id]] <- base
        recipes[[id]] <- .gs_v283t_recipe(id, type, id)
      } else {
        targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
        recipes[[id]] <- .gs_v283t_recipe(
          id, type, paste0(id, c(":weighted_target",
                                 ":subgroup_probability")), "subgroup_ratio")
      }
    } else if (type == "directional_heterogeneity") {
      targets[[id]] <- sconjoint::scmix_inference_target(
        type = "heterogeneity_primitives", contrast = contrast, label = id)
      recipes[[id]] <- .gs_v283t_recipe(
        id, type, paste0(id, c(":H_Z", ":H_R")),
        "heterogeneity_share")
    } else if (type == "mean_mrs") {
      denominator <- .gs_v283t_contrast(
        registries, row$benefit_contrast_id[[1L]], attr_names)
      targets[[id]] <- sconjoint::scmix_inference_target(
        type = "mrs_primitives", numerator = contrast,
        denominator = denominator, label = id)
      recipes[[id]] <- .gs_v283t_recipe(
        id, type, paste0(id, c(":numerator", ":denominator")), "mrs")
    } else {
      .gs_v283t_stop("Supported target type lacks a builder: ", type, ".")
    }
  }
  heterogeneity_rows <- rows[
    rows$target_type == "directional_heterogeneity", , drop = FALSE]
  primitive_registry <- if (nrow(heterogeneity_rows)) {
    .gs_v283t_heterogeneity_registry(heterogeneity_rows)
  } else NULL
  list(family_id = family_id, status = "supported", rows = rows,
       targets = targets, recipes = recipes,
       primitive_registry = primitive_registry, reason = "")
}

.gs_v283t_geometry <- function(theta, kind, target_type = NULL) {
  theta <- as.numeric(theta)
  if (kind == "identity") {
    bounded_types <- c(
      "choice_probability", "subgroup_choice_probability",
      "directional_share", "subgroup_directional_share",
      "compensating_benefit_share",
      "subgroup_compensating_benefit_share")
    bounded <- length(target_type) == 1L && !is.na(target_type) &&
      target_type %in% bounded_types
    valid <- !bounded || (is.finite(theta[[1L]]) &&
                           theta[[1L]] >= 0 && theta[[1L]] <= 1)
    return(list(
      value = theta[[1L]], gradient = 1,
      supporting = stats::setNames(theta, "scalar"), gate = valid,
      gate_reason = if (!bounded) "not_applicable" else if (valid) {
        "probability_in_unit_interval"
      } else "probability_outside_unit_interval"))
  }
  if (kind == "subgroup_ratio") {
    a <- theta[[1L]]; b <- theta[[2L]]
    valid <- is.finite(b) && b >= .gs_v283t_group_margin
    return(list(
      value = if (b != 0) a / b else NA_real_,
      gradient = if (b != 0) c(1 / b, -a / b^2) else rep(NA_real_, 2L),
      supporting = c(weighted_target = a, subgroup_probability = b),
      gate = valid,
      gate_reason = if (valid) "passed" else "group_probability_below_0.05"))
  }
  if (kind == "heterogeneity_share") {
    hz <- theta[[1L]]; hr <- theta[[2L]]; total <- hz + hr
    gradient <- if (is.finite(total) && total != 0) {
      c(hr, -hz) / total^2
    } else rep(NA_real_, 2L)
    valid <- is.finite(total) && total >= .gs_v283t_heterogeneity_margin &&
      is.finite(hz) && is.finite(hr) && hz >= 0 && hr >= 0
    return(list(
      value = if (total != 0) hz / total else NA_real_, gradient = gradient,
      supporting = c(H_Z = hz, H_R = hr, H_T = total), gate = valid,
      gate_reason = if (valid) "passed" else
        "heterogeneity_components_or_total_margin_failed"))
  }
  if (kind == "mrs") {
    a <- theta[[1L]]; b <- theta[[2L]]
    valid <- is.finite(b) && b != 0
    return(list(
      value = if (b != 0) -a / b else NA_real_,
      gradient = if (b != 0) c(-1 / b, a / b^2) else rep(NA_real_, 2L),
      supporting = c(numerator = a, denominator = b), gate = valid,
      gate_reason = if (valid) "raw_denominator_nonzero_diagnostic_only" else
        "zero_denominator"))
  }
  .gs_v283t_stop("Unknown target geometry: ", kind, ".")
}

## Reconstruct one raw moderator row per requested respondent. Every retained
## start carries its own training-fold transform. A nonselected start must never
## be evaluated with the selected start's preprocessing state.
.gs_v283t_record_z <- function(preflight, respondent_ids, record) {
  respondent_ids <- as.character(respondent_ids)
  if (!length(respondent_ids) || anyNA(respondent_ids) ||
      anyDuplicated(respondent_ids) || !is.list(record) ||
      !inherits(record$network_state, "scmix_network_state")) {
    .gs_v283t_stop("Malformed respondent IDs or retained start state.")
  }
  prepared <- preflight$prepared
  task_ids <- as.character(prepared$respondent_id)
  if (!length(task_ids) || nrow(prepared$Z_primary) != length(task_ids) ||
      any(!is.finite(as.matrix(prepared$Z_primary)))) {
    .gs_v283t_stop("Malformed verified moderator checkpoint.")
  }
  all_ids <- unique(task_ids)
  first <- match(all_ids, task_ids)
  keep <- match(respondent_ids, all_ids)
  if (anyNA(keep)) {
    .gs_v283t_stop("A retained fit scope contains an unknown respondent.")
  }
  Z <- as.matrix(prepared$Z_primary[first[keep], , drop = FALSE])
  expected <- record$network_state$moderator_names
  if (is.null(colnames(Z)) || anyDuplicated(colnames(Z)) ||
      !setequal(colnames(Z), expected)) {
    .gs_v283t_stop("Moderator columns do not match the retained state.")
  }
  Z <- Z[, expected, drop = FALSE]
  if (any(!is.finite(Z))) {
    .gs_v283t_stop("Nonfinite raw moderators remain.")
  }
  rownames(Z) <- respondent_ids
  Z
}

## Evaluate the registered rowwise callbacks at one retained start.  Labels
## mirror scmix_dml(): scalar callbacks use their registered target ID and
## vector callbacks use target_id:callback_label.  Directional heterogeneity
## is reduced from (mean, second moment, residual variance) to (H_Z, H_R)
## before the reporting recipe is applied.
.gs_v283t_plugin_primitives <- function(
    record, targets, recipes, preflight, respondent_ids, attr_names,
    fold = 0L) {
  if (!is.list(targets) || !length(targets) || is.null(names(targets)) ||
      !identical(names(targets), names(recipes))) {
    .gs_v283t_stop("Plug-in stability requires matching named targets and recipes.")
  }
  Z <- .gs_v283t_record_z(preflight, respondent_ids, record)
  mu <- sconjoint::scmix_predict_network(
    record$network_state, Z, input = "raw", output = "raw", device = "cpu")
  A <- as.matrix(record$A_raw)
  attr_names <- as.character(attr_names)
  if (!identical(colnames(mu), attr_names) || nrow(A) != length(attr_names) ||
      !ncol(A) || any(!is.finite(A)) || !is.numeric(record$kappa) ||
      length(record$kappa) != 1L || !is.finite(record$kappa)) {
    .gs_v283t_stop("A retained start has incompatible raw coefficient state.")
  }
  Sigma <- tcrossprod(A)
  expected <- unique(unlist(lapply(recipes, `[[`, "primitive_names"),
                            use.names = FALSE))
  heterogeneity_ids <- names(recipes)[vapply(
    recipes, function(x) identical(x$kind, "heterogeneity_share"),
    logical(1L))]
  heterogeneity_support <- unlist(lapply(heterogeneity_ids, function(id) {
    paste0(".v28_support:", id,
           c(":mean", ":second_moment", ":residual_variance"))
  }), use.names = FALSE)
  expected <- c(expected, heterogeneity_support)
  primitive <- stats::setNames(rep(NA_real_, length(expected)), expected)
  errors <- stats::setNames(rep("", length(targets)), names(targets))
  for (id in names(targets)) {
    ans <- tryCatch(
      targets[[id]](
        mu = mu, kappa = as.numeric(record$kappa), Sigma = Sigma, Z = Z,
        respondent_id = as.character(respondent_ids), fold = fold,
        attr_names = attr_names),
      error = function(e) e)
    if (inherits(ans, "error")) {
      errors[[id]] <- conditionMessage(ans)
      next
    }
    value <- as.matrix(ans$value)
    labels <- if (!is.null(ans$labels)) as.character(ans$labels) else
      colnames(value)
    if (nrow(value) != length(respondent_ids) || !ncol(value) ||
        length(labels) != ncol(value) || anyDuplicated(labels) ||
        any(!is.finite(value))) {
      errors[[id]] <- "callback returned malformed or nonfinite values"
      next
    }
    raw_names <- if (ncol(value) == 1L) id else paste0(id, ":", labels)
    raw <- stats::setNames(colMeans(value), raw_names)
    recipe <- recipes[[id]]
    if (identical(recipe$kind, "heterogeneity_share")) {
      needed <- paste0(id, c(":mean", ":second_moment",
                              ":residual_variance"))
      if (!all(needed %in% names(raw))) {
        errors[[id]] <- "heterogeneity callback lacks its supporting moments"
        next
      }
      values <- c(raw[[needed[[2L]]]] - raw[[needed[[1L]]]]^2,
                  raw[[needed[[3L]]]])
      names(values) <- recipe$primitive_names
      support_names <- paste0(
        ".v28_support:", id,
        c(":mean", ":second_moment", ":residual_variance"))
      support_values <- unname(raw[needed])
      names(support_values) <- support_names
      values <- c(values, support_values)
    } else {
      values <- raw[recipe$primitive_names]
      if (length(values) != length(recipe$primitive_names) || anyNA(values)) {
        errors[[id]] <- "callback labels do not match the reporting recipe"
        next
      }
    }
    primitive[names(values)] <- values
  }
  attr(primitive, "target_errors") <- errors[nzchar(errors)]
  primitive
}

.gs_v283t_reported_from_primitives <- function(primitives, recipes) {
  out <- vapply(recipes, function(recipe) {
    value <- primitives[recipe$primitive_names]
    if (identical(recipe$kind, "heterogeneity_share")) {
      target_id <- recipe$target_id
      support_names <- paste0(
        ".v28_support:", target_id,
        c(":mean", ":second_moment", ":residual_variance"))
      support <- primitives[support_names]
      if (length(support) == 3L && all(is.finite(support))) {
        ## Both a full-fit plug-in and a fold-weighted cross-fit plug-in use
        ## the same declared finite-population H_Z. Aggregate the raw support
        ## moments first, then compute E[m^2] - E[m]^2. The package DML path
        ## applies the same global assembly to respondent-level contributions,
        ## while keeping every Riesz gradient center outer-training-only.
        value <- c(
          support[[2L]] - support[[1L]]^2,
          support[[3L]])
        names(value) <- recipe$primitive_names
      }
    }
    if (length(value) != length(recipe$primitive_names)) return(NA_real_)
    if (any(!is.finite(value))) return(NA_real_)
    geometry <- .gs_v283t_geometry(
      value, recipe$kind, target_type = recipe$target_type)
    if (!isTRUE(geometry$gate) || !is.finite(geometry$value)) NA_real_ else
      geometry$value
  }, numeric(1L))
  names(out) <- names(recipes)
  out
}

.gs_v283t_scope_start_ledger <- function(records, scope) {
  if (!is.list(records) || !length(records) || is.null(names(records)) ||
      any(!nzchar(names(records))) || anyDuplicated(names(records))) {
    .gs_v283t_stop("Malformed retained start collection for scope ", scope, ".")
  }
  required <- c("start_id", "selected", "selection_eligible",
                "target_comparison_eligible", "relative_objective_gap",
                "network_state", "A_raw", "kappa",
                "moderator_preprocessing")
  if (any(!vapply(records, function(x)
    is.list(x) && all(required %in% names(x)), logical(1L)))) {
    .gs_v283t_stop("A retained start lacks stability fields in scope ", scope, ".")
  }
  if (any(!vapply(records, function(x) {
    is.list(x$moderator_preprocessing) &&
      identical(x$moderator_preprocessing$imputation, "none") &&
      is.character(x$moderator_preprocessing$transform_hash) &&
      length(x$moderator_preprocessing$transform_hash) == 1L &&
      nzchar(x$moderator_preprocessing$transform_hash)
  }, logical(1L)))) {
    .gs_v283t_stop("A retained start has incompatible moderator preprocessing in scope ",
                  scope, ".")
  }
  start_id <- vapply(records, `[[`, character(1L), "start_id")
  selected <- vapply(records, function(x) isTRUE(x$selected), logical(1L))
  selection_eligible <- vapply(
    records, function(x) isTRUE(x$selection_eligible), logical(1L))
  target_eligible <- vapply(
    records, function(x) isTRUE(x$target_comparison_eligible), logical(1L))
  gap <- vapply(records, `[[`, numeric(1L), "relative_objective_gap")
  computed_target_eligible <- selection_eligible & is.finite(gap) &
    gap <= .gs_v283t_near_objective_cutoff
  if (!identical(names(records), unname(start_id)) || sum(selected) != 1L ||
      !all(target_eligible[selected]) ||
      !identical(target_eligible, computed_target_eligible)) {
    .gs_v283t_stop("Retained start semantics are inconsistent in scope ",
                  scope, ".")
  }
  disposition <- ifelse(
    target_eligible,
    ifelse(selected, "included_selected", "included_near_alternative"),
    ifelse(selection_eligible & is.finite(gap) &
             gap > .gs_v283t_near_objective_cutoff,
           "excluded_inferior_objective", "excluded_computationally_inadmissible"))
  data.frame(
    scope = scope, start_id = start_id, selected = selected,
    selection_eligible = selection_eligible,
    target_comparison_eligible = target_eligible,
    relative_objective_gap = gap, disposition = disposition,
    stringsAsFactors = FALSE)
}

.gs_v283t_respondent_folds <- function(assembled) {
  task_ids <- as.character(assembled$respondent_id)
  all_ids <- unique(task_ids)
  fold <- assembled$fold_id
  if (length(fold) == length(task_ids)) {
    by_id <- split(as.integer(fold), factor(task_ids, levels = all_ids))
    if (any(lengths(lapply(by_id, unique)) != 1L)) {
      .gs_v283t_stop("Outer folds vary within respondent.")
    }
    respondent_fold <- vapply(by_id, function(x) unique(x)[[1L]], integer(1L))
  } else if (length(fold) == length(all_ids)) {
    if (!is.null(names(fold))) {
      if (anyDuplicated(names(fold)) || !setequal(names(fold), all_ids)) {
        .gs_v283t_stop("Named respondent-fold vector does not match respondents.")
      }
      respondent_fold <- as.integer(fold[all_ids])
    } else {
      respondent_fold <- as.integer(fold)
      names(respondent_fold) <- all_ids
    }
  } else {
    .gs_v283t_stop("Could not recover one outer fold per respondent.")
  }
  if (length(respondent_fold) != length(all_ids) || anyNA(respondent_fold) ||
      any(respondent_fold < 1L)) {
    .gs_v283t_stop("Invalid respondent outer folds.")
  }
  stats::setNames(as.integer(respondent_fold), all_ids)
}

## Target-relevant numerical stability over the application fit's retained
## near-optimal states.  Inferior starts are audit records, not counterfactual
## estimators.  A scope containing only its selected near-optimal state is not
## evidence of zero movement and therefore receives an explicit N/A state.
.gs_v283t_near_optimum_stability <- function(
    application_assembly, preflight, bundle, reported_se,
    objective_tolerance = .gs_v283t_near_objective_cutoff,
    movement_cutoff = .gs_v283t_target_movement_cutoff) {
  if (!identical(as.numeric(objective_tolerance),
                 .gs_v283t_near_objective_cutoff) ||
      !identical(as.numeric(movement_cutoff),
                 .gs_v283t_target_movement_cutoff)) {
    .gs_v283t_stop("The frozen near-optimum stability thresholds changed.")
  }
  fit <- application_assembly$assembled
  records_by_scope <- application_assembly$multistart_artifacts_by_scope
  ids <- names(bundle$recipes)
  reported_se <- as.numeric(reported_se[ids])
  names(reported_se) <- ids
  if (!inherits(fit, "scmix_nested_assembled") || !length(ids) ||
      is.null(names(records_by_scope)) || !"full" %in% names(records_by_scope)) {
    .gs_v283t_stop("Malformed v2.8.3 assembly for target stability.")
  }
  ledgers <- lapply(names(records_by_scope), function(scope) {
    .gs_v283t_scope_start_ledger(records_by_scope[[scope]], scope)
  })
  names(ledgers) <- names(records_by_scope)
  start_ledger <- do.call(rbind, ledgers)
  rownames(start_ledger) <- NULL
  all_ids <- unique(as.character(fit$respondent_id))
  attr_names <- as.character(fit$attr_names)
  comparisons <- list()
  append_comparison <- function(surface, scope, record, reference, candidate,
                                selected_start_id) {
    for (id in ids) {
      se <- reported_se[[id]]
      finite_pair <- is.finite(reference[[id]]) && is.finite(candidate[[id]])
      valid_se <- is.finite(se) && se > 0
      difference <- if (finite_pair) abs(candidate[[id]] - reference[[id]]) else
        NA_real_
      standardized <- if (finite_pair && valid_se) difference / se else Inf
      comparisons[[length(comparisons) + 1L]] <<- data.frame(
        target_id = id, surface = surface, scope = scope,
        start_id = record$start_id,
        selected_start_id = selected_start_id,
        relative_objective_gap = record$relative_objective_gap,
        reference = reference[[id]], candidate = candidate[[id]],
        absolute_difference = difference,
        diagnostic_se = se, standardized_difference = standardized,
        evaluable = finite_pair && valid_se,
        evaluation_reason = if (!valid_se) "invalid_or_zero_standard_error" else
          if (!finite_pair) "nonfinite_target_evaluation" else "evaluated",
        stringsAsFactors = FALSE)
    }
    invisible(NULL)
  }

  full_records <- records_by_scope$full
  full_ledger <- ledgers$full
  full_selected_index <- which(full_ledger$selected)
  full_selected <- full_records[[full_selected_index]]
  full_reference_primitive <- .gs_v283t_plugin_primitives(
    full_selected, bundle$targets, bundle$recipes, preflight, all_ids,
    attr_names, fold = 0L)
  full_reference <- .gs_v283t_reported_from_primitives(
    full_reference_primitive, bundle$recipes)
  full_alternatives <- which(full_ledger$target_comparison_eligible &
                               !full_ledger$selected)
  for (j in full_alternatives) {
    record <- full_records[[j]]
    primitive <- .gs_v283t_plugin_primitives(
      record, bundle$targets, bundle$recipes, preflight, all_ids,
      attr_names, fold = 0L)
    candidate <- .gs_v283t_reported_from_primitives(primitive, bundle$recipes)
    append_comparison("full_sample_plugin", "full", record,
                      full_reference, candidate,
                      full_selected$start_id)
  }

  respondent_fold <- .gs_v283t_respondent_folds(fit)
  folds <- sort(unique(unname(respondent_fold)))
  expected_scopes <- paste0("outer_", folds)
  if (!all(expected_scopes %in% names(records_by_scope))) {
    .gs_v283t_stop("Retained outer-scope starts do not match assembled folds.")
  }
  selected_components <- vector("list", length(folds))
  weights <- numeric(length(folds))
  selected_by_fold <- vector("list", length(folds))
  held_by_fold <- vector("list", length(folds))
  for (h in seq_along(folds)) {
    k <- folds[[h]]
    scope <- paste0("outer_", k)
    ledger <- ledgers[[scope]]
    records <- records_by_scope[[scope]]
    selected_by_fold[[h]] <- records[[which(ledger$selected)]]
    held_by_fold[[h]] <- names(respondent_fold)[respondent_fold == k]
    if (!length(held_by_fold[[h]])) {
      .gs_v283t_stop("An assembled outer fold contains no respondents.")
    }
    weights[[h]] <- length(held_by_fold[[h]]) / length(all_ids)
    selected_components[[h]] <- .gs_v283t_plugin_primitives(
      selected_by_fold[[h]], bundle$targets, bundle$recipes, preflight,
      held_by_fold[[h]], attr_names, fold = k)
  }
  crossfit_primitive <- Reduce(`+`, Map(
    function(component, weight) component * weight,
    selected_components, weights))
  crossfit_reference <- .gs_v283t_reported_from_primitives(
    crossfit_primitive, bundle$recipes)
  ## Preserve the statistic calibrated in v2.6. Each outer-fold alternative
  ## replaces only that fold's selected nuisance while every other fold stays
  ## at its selected state. The diagnostic is the maximum selected-reference
  ## movement across the full-fit surface and these one-fold substitution
  ## surfaces. It is not a Cartesian-product spread across simultaneous
  ## alternatives in several folds.
  for (h in seq_along(folds)) {
    k <- folds[[h]]
    scope <- paste0("outer_", k)
    ledger <- ledgers[[scope]]
    records <- records_by_scope[[scope]]
    alternatives <- which(ledger$target_comparison_eligible & !ledger$selected)
    for (j in alternatives) {
      record <- records[[j]]
      component <- .gs_v283t_plugin_primitives(
        record, bundle$targets, bundle$recipes, preflight,
        held_by_fold[[h]], attr_names, fold = k)
      alternative_primitive <- crossfit_primitive + weights[[h]] *
        (component - selected_components[[h]])
      candidate <- .gs_v283t_reported_from_primitives(
        alternative_primitive, bundle$recipes)
      append_comparison(
        "crossfit_plugin_one_fold_substitution", scope, record,
        crossfit_reference, candidate, selected_by_fold[[h]]$start_id)
    }
  }
  comparison_table <- if (length(comparisons)) do.call(rbind, comparisons) else
    data.frame(
      target_id = character(), surface = character(), scope = character(),
      start_id = character(), selected_start_id = character(),
      relative_objective_gap = numeric(), reference = numeric(),
      candidate = numeric(), absolute_difference = numeric(),
      diagnostic_se = numeric(), standardized_difference = numeric(),
      evaluable = logical(), evaluation_reason = character(),
      stringsAsFactors = FALSE)
  target_detail <- do.call(rbind, lapply(ids, function(id) {
    rows <- comparison_table[comparison_table$target_id == id, , drop = FALSE]
    n_comparisons <- nrow(rows)
    valid_se <- is.finite(reported_se[[id]]) && reported_se[[id]] > 0
    movement <- if (!n_comparisons) NA_real_ else if (!valid_se ||
      any(!rows$evaluable)) Inf else max(rows$standardized_difference)
    state <- if (!n_comparisons) {
      "not_applicable_singleton_near_set"
    } else if (!valid_se) {
      "failed_invalid_standard_error"
    } else if (any(!rows$evaluable)) {
      "failed_nonfinite_target_evaluation"
    } else if (is.finite(movement) && movement <= movement_cutoff) {
      "passed"
    } else "failed_exceeds_0.10_se"
    data.frame(
      target_id = id, diagnostic_se = reported_se[[id]],
      comparison_count = n_comparisons,
      near_optimum_selected_movement_se = movement,
      gate_state = state,
      gate_pass = if (state == "passed") TRUE else if (grepl("^failed", state))
        FALSE else NA,
      stringsAsFactors = FALSE)
  }))
  rownames(target_detail) <- NULL
  failure <- grepl("^failed", target_detail$gate_state)
  overall_status <- if (any(failure)) "failed" else if
    (all(is.na(target_detail$gate_pass))) "not_applicable_singleton_near_set" else
      if (anyNA(target_detail$gate_pass)) "partially_evaluated" else "evaluated"
  list(
    protocol = "gs2020-v2.8.3-target-near-optimum-stability-v1",
    status = overall_status,
    objective_tolerance = objective_tolerance,
    target_selected_movement_cutoff_se = movement_cutoff,
    target_detail = target_detail,
    comparisons = comparison_table,
    start_ledger = start_ledger,
    excluded_starts = start_ledger[
      !start_ledger$target_comparison_eligible, , drop = FALSE],
    excluded_inferior_count = sum(
      start_ledger$disposition == "excluded_inferior_objective"),
    excluded_inadmissible_count = sum(
      start_ledger$disposition == "excluded_computationally_inadmissible"),
    near_alternative_count = sum(
      start_ledger$disposition == "included_near_alternative"),
    full_selected_plugin = full_reference,
    crossfit_selected_plugin = crossfit_reference,
    formal_release = FALSE,
    formal_release_reason =
      "Target stability alone does not satisfy the remaining formal gates.")
}

.gs_v283t_stability_evidence_complete <- function(stability) {
  detail <- stability$target_detail
  is.data.frame(detail) && nrow(detail) > 0L &&
    stability$status %in% c("evaluated", "failed") &&
    all(!is.na(detail$gate_pass))
}

.gs_v283t_near_optimum_failure <- function(target_ids, reported_se, reason) {
  target_ids <- as.character(target_ids)
  reported_se <- as.numeric(reported_se[target_ids])
  list(
    protocol = "gs2020-v2.8.3-target-near-optimum-stability-v1",
    status = "withheld_computation_error",
    objective_tolerance = .gs_v283t_near_objective_cutoff,
    target_selected_movement_cutoff_se = .gs_v283t_target_movement_cutoff,
    target_detail = data.frame(
      target_id = target_ids, diagnostic_se = reported_se,
      comparison_count = NA_integer_,
      near_optimum_selected_movement_se = Inf,
      gate_state = "withheld_computation_error", gate_pass = FALSE,
      stringsAsFactors = FALSE),
    comparisons = NULL, start_ledger = NULL, excluded_starts = NULL,
    excluded_inferior_count = NA_integer_,
    excluded_inadmissible_count = NA_integer_,
    near_alternative_count = NA_integer_,
    formal_release = FALSE,
    formal_release_reason = paste(
      "Target stability could not be evaluated and fails closed.", reason))
}

.gs_v283t_run_dml_target <- function(fit, basis, target_id, target, seed,
                                    primitive_spec = NULL) {
  if (!inherits(fit, "scmix_nested_assembled") || !identical(fit$q, 1L)) {
    .gs_v283t_stop("The v2.8.3 target adapter accepts only assembled q=1 fits.")
  }
  if (!is.character(target_id) || length(target_id) != 1L ||
      is.na(target_id) || !nzchar(target_id) || !is.function(target)) {
    .gs_v283t_stop("Supply one registered target and target ID.")
  }
  registry <- if (is.null(primitive_spec)) NULL else list(primitive_spec)
  if (!is.null(registry) &&
      !inherits(primitive_spec, "scmix_v22_target_spec")) {
    .gs_v283t_stop("A directional-heterogeneity primitive spec is malformed.")
  }
  targets <- stats::setNames(list(target), target_id)
  sconjoint::scmix_dml(
    fit = fit, targets = character(), plugin_targets = targets,
    mu_basis = basis, nu_grid = NULL,
    riesz_rule = "select",
    riesz_numerical_rank_rule = "dimension_epsilon",
    riesz_penalty = "identity",
    riesz_validation_fraction = .gs_v283t_riesz_validation_fraction,
    active_eigenvalue_min = 1e-6, information_eigenvalue_min = 1e-8,
    rank_tolerance = .gs_v283t_loading_rank_tolerance,
    riesz_equation_tolerance = 0.20,
    ridge_sensitivity_tolerance = 0.20,
    derivative_refinement_tolerance = 1e-4,
    allow_numeric_derivatives = FALSE, v22_registry = registry,
    verification = NULL, multiplier_draws = 0L,
    multiplier = "normal", level = 0.95, seed = as.integer(seed))
}

## Run each registered target separately.  This keeps ridge validation and its
## audit trail target specific.  The familywise multiplier is applied only
## after the target influence columns have been transformed and recombined.
.gs_v283t_run_dml_family <- function(fit, basis, bundle, seed) {
  if (!is.list(bundle) || !is.data.frame(bundle$rows)) {
    .gs_v283t_stop("Malformed target bundle or family DML seed.")
  }
  ids <- bundle$rows$target_id
  if (!identical(names(bundle$targets), ids) ||
      !identical(names(bundle$recipes), ids) || !is.numeric(seed) ||
      length(seed) != 1L || !is.finite(seed)) {
    .gs_v283t_stop("Malformed target bundle or family DML seed.")
  }
  out <- lapply(seq_along(ids), function(j) {
    id <- ids[[j]]
    spec <- if (identical(
      bundle$rows$target_type[[j]], "directional_heterogeneity")) {
      bundle$primitive_registry[[id]]
    } else NULL
    target_seed <- as.double(seed) + as.double(bundle$rows$target_index[[j]])
    if (target_seed > .Machine$integer.max) {
      .gs_v283t_stop("A target-specific DML seed exceeds the integer range.")
    }
    tryCatch(
      .gs_v283t_run_dml_target(
        fit, basis, id, bundle$targets[[id]], as.integer(target_seed), spec),
      error = function(error) structure(list(
        status = "withheld_target_specific_computation_error",
        inference_available = FALSE,
        target_id = id,
        reason = conditionMessage(error),
        target_seed = as.integer(target_seed)),
        class = c("gs_v283_target_dml_error", "list")))
  })
  names(out) <- ids
  structure(out, base_seed = as.integer(seed),
            seed_rule = "family_base_seed_plus_registered_target_index",
            class = c("gs_v283_target_dml_collection", "list"))
}

.gs_v283t_fold_metric <- function(dml, field, primitives, fun = max) {
  values <- lapply(dml$fold_details, function(fd) {
    x <- fd[[field]]
    if (is.null(names(x)) && length(x) == length(dml$estimate)) {
      names(x) <- names(dml$estimate)
    }
    x <- as.numeric(x[primitives])
    if (!length(x) || any(!is.finite(x))) return(NA_real_)
    fun(x)
  })
  as.numeric(unlist(values, use.names = FALSE))
}

.gs_v283t_transform_dml <- function(dml, recipes, rows,
                                   fit_gate_pass = FALSE) {
  ids <- names(recipes)
  if (!inherits(dml, "scmix_dml") || is.null(dml$estimate) ||
      is.null(dml$influence) || is.null(dml$plugin_estimate) ||
      any(!vapply(recipes, function(x)
        all(x$primitive_names %in% names(dml$estimate)), logical(1L)))) {
    return(list(
      summary = data.frame(
        target_index = as.integer(rows$target_index), target_id = rows$target_id,
        target_type = rows$target_type, estimate = NA_real_,
        crossfit_plugin = NA_real_, diagnostic_se = NA_real_,
        draft_pointwise_lower = NA_real_, draft_pointwise_upper = NA_real_,
        draft_simultaneous_lower = NA_real_, draft_simultaneous_upper = NA_real_,
        riesz_max = NA_real_,
        riesz_correction_relative_sensitivity_max = NA_real_,
        ridge_movement_se = NA_real_,
        ridge_neighborhood_gate_pass = FALSE,
        derivative_error_max = NA_real_, null_space_defect_max = NA_real_,
        fit_gate_pass = fit_gate_pass,
        primitive_diagnostic_gate_pass = FALSE,
        fast_diagnostic_gate_pass = FALSE,
        target_gate_pass = FALSE,
        formal_release = FALSE, status = "withheld_dml_unavailable",
        reason = if (is.null(dml$reason)) "DML primitives unavailable" else
          dml$reason, stringsAsFactors = FALSE),
      influence = NULL, plugin = NULL))
  }
  N <- nrow(dml$influence)
  target_if <- matrix(NA_real_, N, length(ids),
                      dimnames = list(dml$respondent_id, ids))
  estimate <- plugin <- se <- riesz <- correction_sensitivity <- derivative <-
    null_defect <- rep(NA_real_, length(ids))
  quantity_gate <- rep(FALSE, length(ids))
  gate_reason <- rep("", length(ids))
  supporting <- vector("list", length(ids)); names(supporting) <- ids
  for (j in seq_along(ids)) {
    recipe <- recipes[[j]]; primitives <- recipe$primitive_names
    geo <- .gs_v283t_geometry(
      dml$estimate[primitives], recipe$kind,
      target_type = recipe$target_type)
    pgeo <- .gs_v283t_geometry(
      dml$plugin_estimate[primitives], recipe$kind,
      target_type = recipe$target_type)
    estimate[j] <- geo$value; plugin[j] <- pgeo$value
    supporting[[j]] <- geo$supporting
    if (all(is.finite(geo$gradient))) {
      target_if[, j] <- as.matrix(dml$influence[, primitives, drop = FALSE]) %*%
        geo$gradient
      target_if[, j] <- target_if[, j] - mean(target_if[, j])
      se[j] <- sqrt(mean(target_if[, j]^2) / N)
    }
    riesz_fold <- .gs_v283t_fold_metric(
      dml, "honest_information_scaled_riesz_defect", primitives, max)
    if (all(is.na(riesz_fold))) {
      riesz_fold <- .gs_v283t_fold_metric(
        dml, "riesz_equation_relative_residual", primitives, max)
    }
    correction_sensitivity_fold <- .gs_v283t_fold_metric(
      dml, "ridge_relative_sensitivity", primitives, max)
    riesz[j] <- if (all(is.finite(riesz_fold))) max(riesz_fold) else NA_real_
    correction_sensitivity[j] <- if (all(is.finite(
      correction_sensitivity_fold))) max(correction_sensitivity_fold) else
        NA_real_
    derivative[j] <- max(dml$derivative_refinement_error[, primitives,
                                                        drop = FALSE],
                         na.rm = TRUE)
    nd <- dml$honest_riesz_null_space_defect_max[primitives]
    null_defect[j] <- if (length(nd) && all(is.finite(nd))) max(nd) else NA_real_
    quantity_gate[j] <- isTRUE(geo$gate)
    gate_reason[j] <- geo$gate_reason
  }
  names(estimate) <- names(plugin) <- names(se) <- names(riesz) <-
    names(correction_sensitivity) <- names(derivative) <-
    names(null_defect) <- ids
  primitive_diagnostic_gate <- is.finite(estimate) & is.finite(se) & se > 0 &
    is.finite(riesz) & riesz <= .gs_v283t_riesz_cutoff &
    is.finite(correction_sensitivity) &
      correction_sensitivity <= .gs_v283t_riesz_correction_cutoff &
    is.finite(derivative) & derivative <= 1e-4 &
    is.finite(null_defect) & null_defect <= 1e-6 & quantity_gate
  z <- stats::qnorm(1 - (1 - .gs_v283t_level) / 2)
  status <- rep("diagnostic_computed_formal_withheld", length(ids))
  reason <- paste0(
    "formal_release_withheld; fit_gate_pass=", fit_gate_pass,
    "; primitive_diagnostic_gate_pass=", primitive_diagnostic_gate,
    "; quantity_domain_check=", quantity_gate, " (", gate_reason, "); ",
    "target-level ridge-neighborhood movement in SE units, regularity-z, ",
    "transformed-Riesz, ",
    "and nonlinear confidence-set gates unavailable")
  list(
    summary = data.frame(
      target_index = as.integer(rows$target_index[match(ids, rows$target_id)]),
      target_id = ids,
      target_type = rows$target_type[match(ids, rows$target_id)],
      estimate = estimate, crossfit_plugin = plugin, diagnostic_se = se,
      draft_pointwise_lower = estimate - z * se,
      draft_pointwise_upper = estimate + z * se,
      draft_simultaneous_lower = NA_real_,
      draft_simultaneous_upper = NA_real_,
      riesz_max = riesz,
      riesz_correction_relative_sensitivity_max = correction_sensitivity,
      ridge_movement_se = NA_real_,
      ridge_neighborhood_gate_pass = FALSE,
      derivative_error_max = derivative,
      null_space_defect_max = null_defect,
      fit_gate_pass = fit_gate_pass,
      primitive_diagnostic_gate_pass = primitive_diagnostic_gate,
      fast_diagnostic_gate_pass = FALSE,
      target_gate_pass = FALSE,
      formal_release = FALSE, status = status, reason = reason,
      stringsAsFactors = FALSE),
    influence = target_if, plugin = plugin, supporting = supporting)
}

.gs_v283t_family_multiplier <- function(influence, estimate, seed,
                                       draws = .gs_v283t_family_draws,
                                       level = .gs_v283t_level,
                                       block_size = .gs_v283t_multiplier_block) {
  influence <- as.matrix(influence)
  estimate <- as.numeric(estimate); names(estimate) <- colnames(influence)
  if (!nrow(influence) || !ncol(influence) || length(estimate) != ncol(influence) ||
      any(!is.finite(influence)) || any(!is.finite(estimate))) {
    .gs_v283t_stop("Family multiplier requires finite target influence columns.")
  }
  N <- nrow(influence); J <- ncol(influence)
  se <- sqrt(colMeans(influence^2) / N)
  valid <- is.finite(se) & se > 0
  out <- matrix(NA_real_, draws, J, dimnames = list(NULL, colnames(influence)))
  withr::local_preserve_seed()
  set.seed(as.integer(seed))
  starts <- seq.int(1L, draws, by = as.integer(block_size))
  for (st in starts) {
    en <- min(draws, st + block_size - 1L)
    multipliers <- matrix(stats::rnorm((en - st + 1L) * N),
                          nrow = en - st + 1L, ncol = N)
    out[st:en, ] <- multipliers %*% influence / N
  }
  critical <- if (any(valid)) stats::quantile(
    apply(abs(sweep(out[, valid, drop = FALSE], 2L, se[valid], `/`)),
          1L, max), probs = level, names = FALSE, type = 8) else NA_real_
  list(
    draws = as.integer(draws), distribution = "independent_standard_normal",
    seed = as.integer(seed), level = level,
    common_across_complete_frozen_family = TRUE,
    included_targets = colnames(influence)[valid],
    withheld_targets = colnames(influence)[!valid],
    simultaneous_critical_value = critical,
    simultaneous_lower = estimate - critical * se,
    simultaneous_upper = estimate + critical * se,
    centered_draw_hash = .gs_v283t_object_sha256(out),
    centered_draws_persisted = FALSE,
    formal_release = FALSE,
    label = .gs_v283t_supported_status)
}

.gs_v283t_transform_dml_collection <- function(dml, bundle,
                                               fit_gate_pass = FALSE) {
  ids <- bundle$rows$target_id
  if (!inherits(dml, "gs_v283_target_dml_collection") ||
      !identical(names(dml), ids)) {
    dml <- stats::setNames(rep(list(dml), length(ids)), ids)
  }
  pieces <- lapply(seq_along(ids), function(j) {
    id <- ids[[j]]
    .gs_v283t_transform_dml(
      dml[[id]], bundle$recipes[id], bundle$rows[j, , drop = FALSE],
      fit_gate_pass)
  })
  summary <- do.call(rbind, lapply(pieces, `[[`, "summary"))
  rownames(summary) <- NULL
  influence_ok <- all(vapply(
    pieces, function(x) !is.null(x$influence) &&
      is.matrix(x$influence) && ncol(x$influence) == 1L,
    logical(1L)))
  influence <- NULL
  if (influence_ok) {
    reference_rows <- rownames(pieces[[1L]]$influence)
    if (all(vapply(pieces, function(x)
      identical(rownames(x$influence), reference_rows), logical(1L)))) {
      influence <- do.call(cbind, lapply(pieces, `[[`, "influence"))
      colnames(influence) <- ids
    }
  }
  list(
    summary = summary, influence = influence,
    plugin = stats::setNames(summary$crossfit_plugin, summary$target_id),
    supporting = stats::setNames(lapply(pieces, function(x) {
      if (is.null(x$supporting) || !length(x$supporting)) NULL else
        x$supporting[[1L]]
    }), ids))
}

.gs_v283t_finalize_family <- function(dml, bundle, fit_gate_pass,
                                     multiplier_seed) {
  transformed <- .gs_v283t_transform_dml_collection(
    dml, bundle, fit_gate_pass)
  multiplier <- NULL
  if (!is.null(transformed$influence) &&
      all(is.finite(transformed$influence)) &&
      all(is.finite(transformed$summary$estimate))) {
    multiplier <- .gs_v283t_family_multiplier(
      transformed$influence, transformed$summary$estimate, multiplier_seed)
    transformed$summary$draft_simultaneous_lower <-
      multiplier$simultaneous_lower[transformed$summary$target_id]
    transformed$summary$draft_simultaneous_upper <-
      multiplier$simultaneous_upper[transformed$summary$target_id]
  }
  list(
    schema_version = "gs2020-v2.8.3-target-family-v1",
    family_id = bundle$family_id,
    status = if (all(transformed$summary$status ==
                     "diagnostic_computed_formal_withheld")) {
      .gs_v283t_supported_status
    } else "withheld_one_or_more_family_members",
    complete_frozen_family = identical(
      transformed$summary$target_id, bundle$rows$target_id),
    dml = dml, summary = transformed$summary,
    supporting = transformed$supporting,
    multiplier = multiplier,
    formal_release = FALSE,
    unavailable_formal_gates = c(
      "independent_data_or_valid_selection_adjustment",
      "target_level_GH45_integration",
      "target_near_optimum_selected_movement",
      "target_level_ridge_neighborhood",
      "target_level_regularity_z", "transformed_target_riesz",
      "range_respecting_nonlinear_confidence_set"),
    q2_used_for_target_selection = FALSE)
}

.gs_v283t_unavailable_family <- function(bundle) {
  rows <- bundle$rows
  list(
    schema_version = "gs2020-v2.8.3-target-family-v1",
    family_id = bundle$family_id, status = .gs_v283t_unavailable_status,
    complete_frozen_family = TRUE, dml = NULL,
    summary = data.frame(
      target_index = as.integer(rows$target_index), target_id = rows$target_id,
      target_type = rows$target_type, estimate = NA_real_,
      crossfit_plugin = NA_real_,
      diagnostic_se = NA_real_, draft_pointwise_lower = NA_real_,
      draft_pointwise_upper = NA_real_, draft_simultaneous_lower = NA_real_,
      draft_simultaneous_upper = NA_real_, riesz_max = NA_real_,
      riesz_correction_relative_sensitivity_max = NA_real_,
      ridge_movement_se = NA_real_, ridge_neighborhood_gate_pass = FALSE,
      derivative_error_max = NA_real_,
      null_space_defect_max = NA_real_, fit_gate_pass = NA,
      primitive_diagnostic_gate_pass = FALSE,
      fast_diagnostic_gate_pass = FALSE, target_gate_pass = FALSE,
      near_optimum_selected_movement_se = NA_real_,
      near_optimum_gate_state = "not_applicable_target_unavailable",
      near_optimum_gate_pass = NA,
      formal_release = FALSE,
      status = .gs_v283t_unavailable_status, reason = bundle$reason,
      stringsAsFactors = FALSE),
    supporting = NULL, multiplier = NULL, formal_release = FALSE,
    unavailable_formal_gates = "target_not_represented",
    q2_used_for_target_selection = FALSE)
}

.gs_v283t_status <- function(repo_root, protocol) {
  output_dir <- .gs_v283t_output_dir(repo_root)
  paths <- c(
    binding = .gs_v283t_checkpoint_path(output_dir, "binding"),
    basis = .gs_v283t_checkpoint_path(output_dir, "basis"),
    stats::setNames(vapply(protocol$families$family_id, function(id) {
      .gs_v283t_checkpoint_path(output_dir, "family", id)
    }, character(1L)), paste0("family_", protocol$families$family_id)),
    export = .gs_v283t_checkpoint_path(output_dir, "export"))
  data.frame(
    checkpoint = names(paths), path = unname(paths),
    complete = vapply(paths, function(path) isTRUE(tryCatch({
      .gs_v283t_read_checkpoint(path, payload = FALSE); TRUE
    }, error = function(e) FALSE)), logical(1L)),
    stringsAsFactors = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
