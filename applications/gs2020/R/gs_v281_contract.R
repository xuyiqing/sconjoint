## Graham--Svolik version 2.8.1 prospective numerical-protocol helpers.
##
## This file contains no top-level outcome-generating action.  The CLI in
## 05_run_fast_v281.R calls these functions only after the protocol and code
## have been committed.  Source artifacts are always read-only.

.gs_v281_protocol_id <- "gs2020-paperps-prospective-2026-09-07-v2.8.1-v1"
.gs_v281_run_relative <- file.path(
  "runs", "gs2020", "v281", "prospective-2026-09-07-v1")
.gs_v281_scopes <- c("full", paste0("outer_", 1:5))
.gs_v281_ranks <- 0:2
.gs_v281_polish_schedule <- data.frame(
  stage = 1:4,
  max_iter = c(100L, 400L, 500L, 1000L),
  learning_rate = c(1, 1, 1, 0.25),
  stringsAsFactors = FALSE)
.gs_v281_polish_max_iter <- sum(.gs_v281_polish_schedule$max_iter)
.gs_v281_kkt_stop <- 1e-5
.gs_v281_near_objective_cutoff <- 1e-6
.gs_v281_loading_invariance_cutoff <- 1e-8
.gs_v281_gradient_steps <- c(
  1e-3, 3e-4, 1e-4, 3e-5, 1e-5, 3e-6, 1e-6, 3e-7, 1e-7)
.gs_v281_gradient_relative_tolerance <- 1e-4
.gs_v281_gradient_absolute_tolerance <- 1e-8
.gs_v281_rng_kind <- c("L'Ecuyer-CMRG", "Inversion", "Rejection")

.gs_v281_stop <- function(...) stop(..., call. = FALSE)

.gs_v281_set_rng_contract <- function() {
  do.call(RNGkind, as.list(.gs_v281_rng_kind))
  .gs_v281_assert_rng_contract()
}

.gs_v281_assert_rng_contract <- function() {
  observed <- unname(RNGkind())
  if (!identical(observed, .gs_v281_rng_kind)) {
    .gs_v281_stop("The active RNG kinds do not match the frozen v2.8.1 registry.")
  }
  observed
}

.gs_v281_set_thread_contract <- function() {
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
             OPENBLAS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
             TORCH_NUM_THREADS = "1")
  if (requireNamespace("torch", quietly = TRUE)) {
    torch::torch_set_num_threads(1L)
    torch::torch_set_num_interop_threads(1L)
  }
  invisible(.gs_v281_assert_thread_contract())
}

.gs_v281_assert_thread_contract <- function() {
  variables <- c("OMP_NUM_THREADS", "MKL_NUM_THREADS",
                 "OPENBLAS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS",
                 "TORCH_NUM_THREADS")
  environment <- Sys.getenv(variables, unset = NA_character_)
  torch_threads <- c(
    intraop = as.integer(torch::torch_get_num_threads()),
    interop = as.integer(torch::torch_get_num_interop_threads()))
  if (anyNA(environment) || any(environment != "1") ||
      !identical(unname(torch_threads), c(1L, 1L))) {
    .gs_v281_stop("The active thread settings do not match the frozen ",
                 "single-thread runtime contract.")
  }
  list(environment = stats::setNames(environment, variables),
       torch_threads = torch_threads)
}

.gs_v281_runtime_binding <- function() {
  required <- c("torch", "pkgload", "digest", "withr")
  missing <- required[!vapply(required, requireNamespace, logical(1L),
                              quietly = TRUE)]
  if (length(missing)) {
    .gs_v281_stop("Missing runtime package(s): ", paste(missing, collapse = ", "),
                 ".")
  }
  r_executable <- normalizePath(file.path(R.home("bin"), "R"),
                                winslash = "/", mustWork = TRUE)
  torch_native <- get("torch_version", envir = asNamespace("torch"),
                      inherits = FALSE)
  if (is.function(torch_native)) torch_native <- torch_native()
  thread_contract <- .gs_v281_assert_thread_contract()
  list(
    R_version = R.version.string,
    R_platform = R.version$platform,
    R_executable_sha256 = .gs_v281_sha256(r_executable),
    package_versions = stats::setNames(vapply(
      required, function(package) as.character(utils::packageVersion(package)),
      character(1L)), required),
    native_torch_version = as.character(torch_native),
    blas = unname(extSoftVersion()[["BLAS"]]),
    thread_contract = thread_contract,
    rng_kind = .gs_v281_assert_rng_contract())
}

.gs_v281_scalar_text <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .gs_v281_stop(what, " must be one nonempty string.")
  }
  x
}

.gs_v281_bool <- function(x) {
  out <- tolower(as.character(x))
  if (length(out) != 1L || is.na(out) || !out %in% c("true", "false")) {
    .gs_v281_stop("Expected one literal true/false value.")
  }
  identical(out, "true")
}

.gs_v281_sha256 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  shasum <- Sys.which("shasum")
  if (!nzchar(shasum)) .gs_v281_stop("The `shasum` executable is required.")
  line <- system2(shasum, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    .gs_v281_stop("Could not compute SHA-256 for ", path, ".")
  }
  sub("[[:space:]].*$", "", line)
}

.gs_v281_object_sha256 <- function(object) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .gs_v281_stop("The `digest` package is required.")
  }
  digest::digest(object, algo = "sha256", serialize = TRUE,
                 serializeVersion = 2L)
}

.gs_v281_read_csv <- function(protocol_dir, name) {
  utils::read.csv(file.path(protocol_dir, name), stringsAsFactors = FALSE,
                  check.names = FALSE, na.strings = character())
}

.gs_v281_read_contract <- function(protocol_dir) {
  x <- read.dcf(file.path(protocol_dir, "application-contract.dcf"), all = TRUE)
  if (nrow(x) != 1L) .gs_v281_stop("The v2.8.1 contract must have one record.")
  unlist(x[1L, , drop = TRUE], use.names = TRUE)
}

.gs_v281_assert_inside <- function(path, parent, what = "path") {
  parent <- paste0(normalizePath(parent, winslash = "/", mustWork = TRUE), "/")
  candidate <- path.expand(path)
  if (!grepl("^/", candidate)) candidate <- file.path(getwd(), candidate)
  probe <- candidate
  suffix <- character()
  while (!file.exists(probe) && !identical(probe, dirname(probe))) {
    suffix <- c(basename(probe), suffix)
    probe <- dirname(probe)
  }
  resolved <- normalizePath(probe, winslash = "/", mustWork = TRUE)
  if (length(suffix)) {
    resolved <- do.call(file.path, c(list(resolved), as.list(suffix)))
  }
  if (!startsWith(paste0(resolved, "/"), parent)) {
    .gs_v281_stop(what, " is outside its permitted root.")
  }
  invisible(resolved)
}

.gs_v281_run_dir <- function(repo_root) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  out <- file.path(repo_root, .gs_v281_run_relative)
  .gs_v281_assert_inside(out, repo_root, "The v2.8.1 run directory")
  out
}

.gs_v281_assert_no_prior_gs_v281 <- function(repo_root) {
  run_dir <- .gs_v281_run_dir(repo_root)
  files <- if (dir.exists(run_dir)) list.files(
    run_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE) else character()
  ## A hard process kill may leave a never-installed temporary file. Such a
  ## file is not an outcome checkpoint and must not poison a prospective rerun.
  installed <- files[!grepl("[.]tmp-[^/]+$", files)]
  if (length(installed)) {
    .gs_v281_stop(
      "A Graham--Svolik v2.8.1 output already exists at the prospective run location.")
  }
  invisible(TRUE)
}

.gs_v281_git_snapshot <- function(repo_root, implementation_paths,
                                 require_clean = TRUE) {
  git <- Sys.which("git")
  if (!nzchar(git)) .gs_v281_stop("Git is required for the v2.8.1 run.")
  implementation_paths <- as.character(implementation_paths)
  if (!length(implementation_paths) || anyNA(implementation_paths) ||
      any(!nzchar(implementation_paths)) ||
      anyDuplicated(implementation_paths)) {
    .gs_v281_stop("The v2.8.1 implementation path registry is malformed.")
  }
  commit <- system2(git, c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
                    stdout = TRUE, stderr = TRUE)
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    .gs_v281_stop("Could not identify the v2.8.1 Git commit.")
  }
  ## Target and fit execution load the package working tree.  Checking only
  ## the application wrapper would leave transitive numerical code mutable.
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--untracked-files=all"), stdout = TRUE,
                    stderr = TRUE)
  if (isTRUE(require_clean) && length(status)) {
    .gs_v281_stop("The v2.8.1 protocol or implementation is uncommitted or modified: ",
                 paste(status, collapse = "; "))
  }
  listed <- system2(git, c("-C", shQuote(repo_root), "ls-files"),
                    stdout = TRUE, stderr = TRUE)
  protocol_files <- list.files(
    file.path(repo_root, "applications", "gs2020", "protocol", "v281"),
    recursive = TRUE, full.names = FALSE, all.files = TRUE,
    no.. = TRUE)
  protocol_files <- file.path(
    "applications", "gs2020", "protocol", "v281", protocol_files)
  loaded_r <- list.files(
    file.path(repo_root, "R"), pattern = "[.]R$", recursive = TRUE,
    full.names = FALSE)
  loaded_r <- file.path("R", loaded_r)
  manifested_r <- implementation_paths[startsWith(implementation_paths, "R/")]
  required <- unique(c(implementation_paths, protocol_files))
  if (isTRUE(require_clean) && !all(required %in% listed)) {
    .gs_v281_stop("Every v2.8.1 implementation and protocol file must be tracked before outcomes.")
  }
  if (isTRUE(require_clean) && !setequal(loaded_r, manifested_r)) {
    .gs_v281_stop(paste(
      "The v2.8.1 manifest must hash every package R file loaded by pkgload",
      "and no unregistered R file may be present."))
  }
  list(commit = commit, clean = !length(status), status = status,
       tracked = listed)
}

.gs_v281_validate_protocol <- function(repo_root, source_root = NULL,
                                      require_external = TRUE,
                                      require_clean = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  protocol_dir <- file.path(repo_root, "applications", "gs2020", "protocol",
                            "v281")
  contract <- .gs_v281_read_contract(protocol_dir)
  if (!identical(unname(contract[["Protocol-Id"]]), .gs_v281_protocol_id)) {
    .gs_v281_stop("Unexpected Graham--Svolik v2.8.1 protocol identifier.")
  }
  if (!identical(unname(contract[["Output-Directory"]]),
                 gsub("\\\\", "/", .gs_v281_run_relative))) {
    .gs_v281_stop("The canonical v2.8.1 run directory changed.")
  }
  if (!identical(unname(contract[["Polish-Max-Iterations"]]),
                 as.character(.gs_v281_polish_max_iter)) ||
      !identical(unname(contract[["Amendment-Scope"]]),
                 "one_dimensional_parameter_shape_preservation_in_gradient_audit") ||
      !identical(unname(contract[["Polish-Stage-Max-Iterations"]]),
                 paste(.gs_v281_polish_schedule$max_iter, collapse = ",")) ||
      !identical(unname(contract[["Polish-Stage-Learning-Rates"]]),
                 paste(.gs_v281_polish_schedule$learning_rate,
                       collapse = ","))) {
    .gs_v281_stop("The v2.8.1 prospective numerical contract changed.")
  }
  exact <- c(
    `Prior-GS-V281-Outcome` = "none",
    `Prior-GS-V28-Outcome` = "none",
    `Historical-Fit-Role` =
      "matching_outer_fold_q1_states_are_initializers_only",
    `Historical-Full-Fit` = "unavailable_not_invented",
    `Full-Initializer` = paste0(
      "deterministic_outer_1_state_affine_input_and_output_scale_remap_",
      "followed_by_full_reoptimization"),
    `Heldout-Leakage-Rule` =
      "each_outer_scope_uses_only_its_matching_outer_training_state",
    `Prospective-Boundary` =
      "no_v281_application_outcome_existed_when_this_protocol_was_written",
    `Historical-Estimate-Reuse` = "prohibited",
    `Historical-Gate-Reuse` = "prohibited",
    `Historical-Target-Reuse` = "prohibited",
    `Checkpoint-Pair-Recovery` = paste0(
      "validate_installed_envelope_and_binding_then_recreate_only_",
      "missing_sha_sidecar"),
    `Defensive-Hardening` = paste0(
      "exact_parameter_name_length_dimension_and_finiteness_checks_on_",
      "gradient_audit_state"),
    `Ranks` = "0,1,2",
    `Primary-Rank` = "1",
    `Carried-Forward-Model-Choices` =
      "matching_outer_v21_specifications_and_deterministic_relu4_wd0.1_full_scope",
    `Fresh-Optimization-Rule` =
      "all_trainable_parameters_reoptimized_from_initializer",
    `Q2-Timing-Decision` = paste0(
      "deterministic_completed_q0_q1_fit_seconds_plus_6.25_times_",
      "q2_sentinel_seconds"),
    `Q2-Timing-Cutoff-Seconds` = "50400",
    `Q2-Stop-Analysis-Rule` =
      "complete_q0_q1_comparison_and_record_q2_deferred",
    `Analysis-Conditional-Dependency` =
      "require_q2_gh15_checkpoint_when_q2_timing_decision_is_continue",
    `Q2-Second-Column-Raw-Norm-Ratio` = "0.05",
    `Q2-Minimum-Q1-Raw-Loading-Norm` = "0.0000000001",
    `Optimizer` = "float64_full_batch_LBFGS_strong_wolfe",
    `Gradient-Steps` = paste(
      "0.001,0.0003,0.0001,0.00003,0.00001,0.000003,",
      "0.000001,0.0000003,0.0000001", sep = ""),
    `Gradient-Relative-Tolerance` = "0.0001",
    `Gradient-Absolute-Tolerance` = "0.00000001",
    `Blockwise-KKT-Cutoff` = "0.00001",
    `Nested-Objective-Relative-Cutoff` = "0.000001",
    `Near-Optimum-Relative-Cutoff` = "0.000001",
    `Near-Optimum-Target-Selected-Movement-Cutoff-SE` = "0.10",
    `Projection-Diagnostic-Rule` =
      "missing_or_nonboolean_is_computationally_inadmissible",
    `Loading-Invariance-Cutoff` = "0.00000001",
    `Loading-Scale-Metadata-Rule` = paste0(
      "preprocessing_and_architecture_names_order_and_values_must_match_",
      "before_optimization"),
    `Q1-Riesz-Rule` =
      "target_specific_training_only_validation_on_outer_training_respondents",
    `Q1-Riesz-Grid` = "0,Ntrain^-1,Ntrain^-3/4,Ntrain^-1/2",
    `Q1-Riesz-Tie-Rule` = paste0(
      "largest_ridge_when_absolute_risk_gap_le_1e-12_times_max_1_",
      "abs_minimum"),
    `Q1-Riesz-Preconditioning` = paste0(
      "structural_Gram_whitening_plus_diagonal_congruence_",
      "equilibration_of_full_penalized_system"),
    `Q1-Riesz-Rank-Assessment` =
      "after_diagonal_congruence_equilibration",
    `Q1-Zero-Ridge-Singular-Rule` =
      "retain_frozen_cutoff_numerical_pseudoinverse_representative",
    `Q1-Ill-Conditioned-Positive-Ridge-Rule` =
      "withhold_never_truncate",
    `Riesz-Defect-Cutoff` = "0.20",
    `Ridge-Neighborhood-Cutoff-SE` = "0.20",
    `Derivative-Cutoff` = "0.0001",
    `Null-Space-Cutoff` = "0.000001",
    `Active-Loading-Eigenvalue-Minimum` = "0.000001",
    `Generalized-Information-Eigenvalue-Minimum` = "0.00000001",
    `Core-Multiplier-Draws` = "9999",
    `Untracked-File-Rule` = "reject_all_visible_untracked_files",
    `Loaded-Package-R-Coverage` =
      "every_loaded_R_file_exactly_hashed_and_tracked",
    `RNG-Kind` = "L'Ecuyer-CMRG,Inversion,Rejection",
    `Runtime-Wrapper` = "applications/bin/Rscript45_exact_hash",
    `Runtime-Binding` = paste0(
      "R_executable_sha256_R_version_platform_package_versions_",
      "native_torch_version_BLAS_and_single_thread_state"),
    `Manifest-Finalization` = "completed_before_first_outcome_command",
    `Threshold-Change` = "none")
  observed_exact <- if (all(names(exact) %in% names(contract))) {
    vapply(contract[names(exact)], .gs_v281_scalar_text, character(1L),
           what = "Each frozen v2.8.1 contract field")
  } else {
    character()
  }
  if (!identical(unname(observed_exact), unname(exact))) {
    .gs_v281_stop("A fixed v2.8.1 threshold or statistical rule changed.")
  }

  checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
  if (!file.exists(checksum_path)) {
    .gs_v281_stop("The v2.8.1 protocol checksum registry is absent.")
  }
  lines <- readLines(checksum_path, warn = FALSE)
  pieces <- strsplit(lines, "  ", fixed = TRUE)
  if (!length(lines) || any(lengths(pieces) != 2L)) {
    .gs_v281_stop("Malformed v2.8.1 protocol checksum registry.")
  }
  registered <- vapply(pieces, `[[`, character(1L), 2L)
  expected_hash <- vapply(pieces, `[[`, character(1L), 1L)
  actual_protocol <- setdiff(list.files(
    protocol_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    "CHECKSUMS.sha256")
  if (anyDuplicated(registered) ||
      any(grepl("^/|(^|/)\\.\\.(/|$)", registered)) ||
      !setequal(registered, actual_protocol)) {
    .gs_v281_stop("The v2.8.1 checksum registry does not cover exactly every ",
                 "protocol file.")
  }
  observed_hash <- vapply(registered, function(nm) {
    .gs_v281_sha256(file.path(protocol_dir, nm))
  }, character(1L))
  if (!identical(unname(observed_hash), unname(expected_hash))) {
    .gs_v281_stop("A v2.8.1 protocol file differs from CHECKSUMS.sha256.")
  }

  states <- .gs_v281_read_csv(protocol_dir, "state-registry.csv")
  expected_scopes <- .gs_v281_scopes
  if (!identical(states$scope, expected_scopes) ||
      anyDuplicated(states$scope) ||
      !identical(states$source_position,
                 c("outer_1", paste0("outer_", 1:5))) ||
      !identical(states$source_label,
                 c("outer_2", "outer_2", "outer_4", "outer_3",
                   "outer_1", "outer_5")) ||
      !all(states$q == 1L) ||
      !identical(states$initializer_role,
                 c("deterministic_full_transfer",
                   rep("matching_outer_training_initializer", 5L)))) {
    .gs_v281_stop("The Graham--Svolik initializer registry changed.")
  }

  implementation <- .gs_v281_read_csv(
    protocol_dir, "implementation-manifest.csv")
  implementation_paths <- implementation$relative_path
  if (anyDuplicated(implementation_paths) ||
      any(grepl("^/|(^|/)\\.\\.(/|$)", implementation_paths)) ||
      any(implementation$sha256 == "PENDING_FINALIZATION") ||
      !"applications/bin/Rscript45" %in% implementation_paths) {
    .gs_v281_stop("The transitive v2.8.1 implementation manifest is unsafe or ",
                 "unfinished.")
  }
  for (i in seq_len(nrow(implementation))) {
    path <- file.path(repo_root, implementation$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.gs_v281_sha256(path), implementation$sha256[[i]])) {
      .gs_v281_stop("Transitive v2.8.1 implementation mismatch: ",
                   implementation$relative_path[[i]], ".")
    }
  }

  inputs <- .gs_v281_read_csv(protocol_dir, "input-manifest.csv")
  if (isTRUE(require_external)) {
    source_root <- .gs_v281_scalar_text(source_root, "source_root")
    source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)
    for (i in seq_len(nrow(inputs))) {
      if (!.gs_v281_bool(inputs$required[[i]])) next
      path <- file.path(source_root,
                        inputs$relative_to_ConjointStructural[[i]])
      if (!file.exists(path) ||
          !identical(.gs_v281_sha256(path), inputs$sha256[[i]])) {
        .gs_v281_stop("External input hash mismatch: ", inputs$input_id[[i]], ".")
      }
    }
  }
  git <- .gs_v281_git_snapshot(
    repo_root, implementation_paths,
    require_clean = require_clean)
  list(protocol_id = .gs_v281_protocol_id, protocol_dir = protocol_dir,
       contract = contract, inputs = inputs, states = states,
       implementation = implementation,
       protocol_hash = .gs_v281_sha256(checksum_path), git = git)
}

.gs_v281_input_path <- function(protocol, source_root, input_id) {
  row <- protocol$inputs[protocol$inputs$input_id == input_id, , drop = FALSE]
  if (nrow(row) != 1L) .gs_v281_stop("Unknown v2.8.1 input: ", input_id, ".")
  path <- file.path(source_root, row$relative_to_ConjointStructural[[1L]])
  if (!file.exists(path) || !identical(.gs_v281_sha256(path), row$sha256[[1L]])) {
    .gs_v281_stop("Input failed its v2.8.1 hash binding: ", input_id, ".")
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.gs_v281_checkpoint_path <- function(run_dir, kind, q = NULL, scope = NULL) {
  kind <- match.arg(kind, c("preflight", "fit", "assembled", "refinement",
                            "timing-decision", "analysis"))
  if (kind == "preflight") return(file.path(run_dir, "preflight.rds"))
  if (kind == "timing-decision") return(file.path(run_dir, "q2-timing-decision.rds"))
  if (kind == "analysis") return(file.path(run_dir, "analysis.rds"))
  if (!q %in% .gs_v281_ranks) .gs_v281_stop("Invalid checkpoint rank.")
  if (kind == "fit") {
    if (!scope %in% .gs_v281_scopes) .gs_v281_stop("Invalid checkpoint scope.")
    return(file.path(run_dir, paste0("q", q), paste0(scope, ".rds")))
  }
  file.path(run_dir, paste0("q", q), paste0(kind, ".rds"))
}

.gs_v281_save_atomic <- function(payload, path, binding) {
  if (!is.list(binding) || !identical(binding$protocol_id, .gs_v281_protocol_id)) {
    .gs_v281_stop("A checkpoint requires the exact v2.8.1 binding.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) || file.exists(sidecar)) {
    .gs_v281_stop("Refusing to overwrite an existing v2.8.1 checkpoint: ", path)
  }
  token <- paste0(Sys.getpid(), "-", format(as.integer(Sys.time()),
                                             scientific = FALSE))
  tmp <- paste0(path, ".tmp-", token)
  tmp_sidecar <- paste0(sidecar, ".tmp-", token)
  on.exit(unlink(c(tmp, tmp_sidecar), force = TRUE), add = TRUE)
  envelope <- list(
    schema_version = "gs2020-v2.8.1-atomic-checkpoint-v1",
    binding = binding,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    payload = payload)
  saveRDS(envelope, tmp, version = 3L)
  hash <- .gs_v281_sha256(tmp)
  writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
  if (!file.rename(tmp, path)) .gs_v281_stop("Could not atomically install ", path)
  if (!file.rename(tmp_sidecar, sidecar)) {
    .gs_v281_stop("Checkpoint data installed but SHA sidecar failed: ", path)
  }
  invisible(list(path = path, sha256 = hash))
}

.gs_v281_read_checkpoint <- function(path, binding = NULL,
                                    payload = TRUE) {
  sidecar <- paste0(path, ".sha256")
  ## The data file is renamed first. If a hard kill lands between the two
  ## renames, validate the complete envelope and its requested binding before
  ## recreating only the missing hash sidecar. A sidecar without data remains
  ## unrecoverable and is never treated as a checkpoint.
  if (file.exists(path) && !file.exists(sidecar) && !is.null(binding)) {
    object <- tryCatch(readRDS(path), error = function(e) NULL)
    required <- names(binding)
    valid <- is.list(object) &&
      identical(object$schema_version,
                "gs2020-v2.8.1-atomic-checkpoint-v1") &&
      identical(object$binding$protocol_id, .gs_v281_protocol_id) &&
      all(required %in% names(object$binding)) &&
      identical(object$binding[required], binding[required])
    if (!isTRUE(valid)) {
      .gs_v281_stop("Cannot recover an incomplete or foreign checkpoint: ", path)
    }
    hash <- .gs_v281_sha256(path)
    token <- paste0(Sys.getpid(), "-", format(as.integer(Sys.time()),
                                               scientific = FALSE))
    tmp_sidecar <- paste0(sidecar, ".tmp-", token)
    on.exit(unlink(tmp_sidecar, force = TRUE), add = TRUE)
    writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
    if (!file.rename(tmp_sidecar, sidecar)) {
      .gs_v281_stop("Could not recover the checkpoint SHA sidecar: ", path)
    }
  }
  if (!file.exists(path) || !file.exists(sidecar)) {
    .gs_v281_stop("Incomplete v2.8.1 checkpoint: ", path)
  }
  expected <- strsplit(readLines(sidecar, warn = FALSE)[[1L]],
                       "[[:space:]]+")[[1L]][1L]
  observed <- .gs_v281_sha256(path)
  if (!identical(expected, observed)) {
    .gs_v281_stop("Checkpoint SHA mismatch: ", path)
  }
  object <- readRDS(path)
  if (!is.list(object) ||
      !identical(object$schema_version, "gs2020-v2.8.1-atomic-checkpoint-v1") ||
      !identical(object$binding$protocol_id, .gs_v281_protocol_id)) {
    .gs_v281_stop("Malformed or foreign v2.8.1 checkpoint: ", path)
  }
  if (!is.null(binding)) {
    required <- names(binding)
    if (!all(required %in% names(object$binding)) ||
        !identical(object$binding[required], binding[required])) {
      .gs_v281_stop("Checkpoint binding mismatch: ", path)
    }
  }
  if (!isTRUE(payload)) {
    return(list(path = path, sha256 = observed, binding = object$binding))
  }
  object
}

.gs_v281_status <- function(repo_root) {
  run_dir <- .gs_v281_run_dir(repo_root)
  paths <- c(
    preflight = .gs_v281_checkpoint_path(run_dir, "preflight"),
    unlist(lapply(.gs_v281_ranks, function(q) {
      stats::setNames(vapply(.gs_v281_scopes, function(scope) {
        .gs_v281_checkpoint_path(run_dir, "fit", q, scope)
      }, character(1L)), paste0("q", q, "_", .gs_v281_scopes))
    })),
    q0_assembled = .gs_v281_checkpoint_path(run_dir, "assembled", 0L),
    q1_assembled = .gs_v281_checkpoint_path(run_dir, "assembled", 1L),
    q2_assembled = .gs_v281_checkpoint_path(run_dir, "assembled", 2L),
    q1_refinement = .gs_v281_checkpoint_path(run_dir, "refinement", 1L),
    q2_refinement = .gs_v281_checkpoint_path(run_dir, "refinement", 2L),
    q2_timing = .gs_v281_checkpoint_path(run_dir, "timing-decision"),
    analysis = .gs_v281_checkpoint_path(run_dir, "analysis"))
  data.frame(
    checkpoint = names(paths), path = unname(paths),
    complete = vapply(paths, function(path) {
      isTRUE(tryCatch({ .gs_v281_read_checkpoint(path, payload = FALSE); TRUE },
                      error = function(e) FALSE))
    }, logical(1L)), stringsAsFactors = FALSE)
}

.gs_v281_scope_index <- function(scope) {
  match(scope, .gs_v281_scopes) - 1L
}

.gs_v281_seed <- function(q, scope, start = c("exact", "perturbed", "constant")) {
  start <- match.arg(start)
  as.integer(202609176 + 100000L * as.integer(q) +
               1000L * .gs_v281_scope_index(scope) +
               match(start, c("exact", "perturbed", "constant")) - 1L)
}

.gs_v281_legacy_scope <- function(protocol, source_root, scope) {
  if (!scope %in% .gs_v281_scopes) .gs_v281_stop("Unknown fit scope: ", scope)
  registry <- protocol$states
  row <- registry[registry$scope == scope, , drop = FALSE]
  if (nrow(row) != 1L) .gs_v281_stop("Missing state registry row for ", scope)
  container <- readRDS(.gs_v281_input_path(protocol, source_root,
                                           "v21_primary_nested"))
  source_position <- as.integer(sub(
    "outer_", "", row$source_position[[1L]]))
  fit <- container$tuning[[source_position]]$refit
  if (!inherits(fit, "scmix_tuned_matrix_fit") ||
      !inherits(fit$network_state, "scmix_network_state")) {
    .gs_v281_stop("The registered v2.1 initializer is not portable: ", scope)
  }
  state_hash <- sconjoint:::.scmix_stable_state_hash(
    fit$network_state$state_dict)
  ids <- sort(unique(as.character(fit$raw_data$respondent_id)))
  id_hash <- .gs_v281_object_sha256(ids)
  spec <- fit$specification
  valid <- identical(state_hash, row$state_hash[[1L]]) &&
    identical(id_hash, row$training_ids_sha256[[1L]]) &&
    length(ids) == as.integer(row$n_respondents[[1L]]) &&
    identical(spec$q, 1L) &&
    identical(spec$mean_family, row$mean_family[[1L]]) &&
    identical(as.integer(spec$hidden), as.integer(row$hidden[[1L]])) &&
    identical(as.numeric(spec$weight_decay),
              as.numeric(row$weight_decay[[1L]])) &&
    identical(fit$analysis_signature,
              row$source_analysis_signature[[1L]]) &&
    identical(names(container$tuning)[[source_position]],
              row$source_label[[1L]])
  if (!isTRUE(valid)) {
    .gs_v281_stop("The v2.1 initializer differs from the frozen registry: ",
                 scope)
  }
  attr(fit, "v281_requested_scope") <- scope
  attr(fit, "v281_source_position") <- row$source_position[[1L]]
  attr(fit, "v281_source_label") <- row$source_label[[1L]]
  attr(fit, "v281_initializer_role") <- row$initializer_role[[1L]]
  fit
}

.gs_v281_occurrence <- function(id) {
  ave(seq_along(id), as.character(id), FUN = seq_along)
}

.gs_v281_scope_rows <- function(prepared, fit) {
  full_key <- paste(as.character(prepared$respondent_id),
                    .gs_v281_occurrence(prepared$respondent_id), sep = "\r")
  fit_id <- as.character(fit$raw_data$respondent_id)
  fit_key <- paste(fit_id, .gs_v281_occurrence(fit_id), sep = "\r")
  rows <- match(fit_key, full_key)
  if (anyNA(rows) || anyDuplicated(rows) ||
      length(fit$raw_data$y) != length(rows) ||
      nrow(as.matrix(fit$raw_data$Z)) != length(rows) ||
      max(abs(fit$raw_data$deltaX - prepared$deltaX[rows, , drop = FALSE])) >
        1e-12 ||
      !identical(fit$raw_data$y, prepared$y[rows]) ||
      !identical(as.matrix(fit$raw_data$Z),
                 as.matrix(prepared$Z_primary[rows, , drop = FALSE]))) {
    .gs_v281_stop("Could not map a historical training scope to prepared rows.")
  }
  rows
}

.gs_v281_assert_historical_inner_tuning <- function(prepared, nested,
                                                     fold_index) {
  if (!is.list(nested$tuning) || length(nested$tuning) != 5L) {
    .gs_v281_stop("Historical nested tuning must contain five outer folds.")
  }
  full_id <- as.character(prepared$respondent_id)
  for (k in 1:5) {
    outer_rows <- which(fold_index != k)
    outer_id <- full_id[outer_rows]
    tuning <- nested$tuning[[k]]
    if (!is.null(tuning$cv_fits)) {
      .gs_v281_stop("Historical tuning does not match the frozen retained-",
                   "evidence schema in outer fold ", k, ".")
    }
    inner_fold <- tuning$fold_id
    if (!is.integer(inner_fold) || length(inner_fold) != length(outer_rows) ||
        anyNA(inner_fold) || !setequal(unique(inner_fold), 1:2) ||
        any(vapply(split(inner_fold, outer_id), function(x) {
          length(unique(x)) != 1L
        }, logical(1L)))) {
      .gs_v281_stop("Historical inner folds are malformed in outer fold ", k,
                   ".")
    }
    if (!is.list(tuning$preprocessing_folds) ||
        length(tuning$preprocessing_folds) != 2L) {
      .gs_v281_stop("Historical inner preprocessing is incomplete in outer fold ",
                   k, ".")
    }
    outer_deltaX <- prepared$deltaX[outer_rows, , drop = FALSE]
    outer_Z <- prepared$Z_primary[outer_rows, , drop = FALSE]
    validation_ids <- vector("list", 2L)
    for (h in 1:2) {
      validation <- inner_fold == h
      expected_validation <- sort(unique(outer_id[validation]), method = "radix")
      expected_training <- sort(unique(outer_id[!validation]), method = "radix")
      preprocessing <- tuning$preprocessing_folds[[h]]
      observed_training <- as.character(preprocessing$training_respondents)
      observed_validation <- as.character(preprocessing$validation_respondents)
      valid_partition <- !anyNA(observed_training) &&
        !anyNA(observed_validation) &&
        !anyDuplicated(observed_training) &&
        !anyDuplicated(observed_validation) &&
        identical(sort(observed_training, method = "radix"), expected_training) &&
        identical(sort(observed_validation, method = "radix"),
                  expected_validation) &&
        !length(intersect(observed_training, observed_validation)) &&
        setequal(c(observed_training, observed_validation), unique(outer_id))
      expected_z <- sconjoint:::.sc_fit_z_transform(
        outer_Z[!validation, , drop = FALSE], outer_id[!validation])
      expected_deltaX <- sconjoint:::.sc_comp_fit_dx_scale(
        outer_deltaX[!validation, , drop = FALSE], outer_id[!validation])
      if (!isTRUE(valid_partition) ||
          !identical(preprocessing$z, expected_z) ||
          !identical(preprocessing$deltaX, expected_deltaX)) {
        .gs_v281_stop("Historical inner preprocessing used the wrong rows in ",
                     "outer fold ", k, ", inner fold ", h, ".")
      }
      validation_ids[[h]] <- expected_validation
    }

    candidates <- tuning$candidates
    specifications <- tuning$specifications
    J <- length(specifications)
    score <- tuning$fold_sequence_log_score
    fold_n <- tuning$fold_n_respondents
    computational_gate <- tuning$fold_computational_gate
    gate_matrices <- list(
      all_inner_fits_computationally_valid = computational_gate,
      all_inner_nesting_gates_pass = tuning$fold_nesting_gate,
      all_shared_pooled_prefits_valid = tuning$fold_pooled_prefit_gate,
      all_continued_constant_fits_valid =
        tuning$fold_continued_constant_gate)
    if (!is.data.frame(candidates) || J < 1L || nrow(candidates) != J ||
        !is.matrix(score) || !is.numeric(score) ||
        !identical(dim(score), c(J, 2L)) ||
        !is.matrix(fold_n) || !is.numeric(fold_n) ||
        !identical(dim(fold_n), c(J, 2L)) ||
        any(!vapply(gate_matrices, function(x) {
          is.matrix(x) && is.logical(x) && !anyNA(x) &&
            identical(dim(x), c(J, 2L))
        }, logical(1L))) ||
        !is.list(tuning$sequence_scores) ||
        length(tuning$sequence_scores) != J ||
        any(lengths(tuning$sequence_scores) != 2L)) {
      .gs_v281_stop("Historical candidate scores are malformed in outer fold ",
                   k, ".")
    }
    for (j in seq_len(J)) {
      for (h in 1:2) {
        sequence_score <- tuning$sequence_scores[[j]][[h]]
        score_names <- names(sequence_score)
        if (!is.numeric(sequence_score) || !length(sequence_score) ||
            any(!is.finite(sequence_score)) || is.null(score_names) ||
            anyNA(score_names) || any(!nzchar(score_names)) ||
            anyDuplicated(score_names) ||
            !identical(sort(score_names, method = "radix"),
                       validation_ids[[h]]) ||
            !identical(length(sequence_score), fold_n[j, h]) ||
            !identical(mean(sequence_score), score[j, h])) {
          .gs_v281_stop("Historical validation scores do not match their ",
                       "inner-validation respondents in outer fold ", k,
                       ", candidate ", j, ", inner fold ", h, ".")
        }
      }
    }

    candidate_fields <- list(
      candidate = vapply(specifications, `[[`, character(1L), "name"),
      q = vapply(specifications, `[[`, integer(1L), "q"),
      mean_family = vapply(specifications, `[[`, character(1L), "mean_family"),
      hidden = vapply(specifications, function(x) {
        paste(x$hidden, collapse = "-")
      }, character(1L)),
      weight_decay = vapply(specifications, `[[`, numeric(1L), "weight_decay"),
      integration = vapply(specifications, `[[`, character(1L), "integration"),
      n_nodes = vapply(specifications, `[[`, integer(1L), "n_nodes"),
      n_draws = vapply(specifications, `[[`, integer(1L), "n_draws"))
    if (!all(names(candidate_fields) %in% names(candidates)) ||
        any(!vapply(names(candidate_fields), function(nm) {
          identical(candidates[[nm]], candidate_fields[[nm]])
        }, logical(1L)))) {
      .gs_v281_stop("Historical candidate specifications changed in outer fold ",
                   k, ".")
    }
    gate <- tuning$candidate_selection_gate
    required_gate <- c(
      "pass", "selected_index", "computationally_eligible", "score_eligible",
      "nesting_eligible", "shared_pooled_prefit_eligible",
      "continued_constant_eligible", "selection_eligible",
      "within_selection_tie", "tie_tolerance", "ineligible_reason")
    if (!is.list(gate) || !all(required_gate %in% names(gate)) ||
        !is.numeric(candidates$complexity_rank) ||
        length(candidates$complexity_rank) != J) {
      .gs_v281_stop("Historical candidate-selection evidence is incomplete in ",
                   "outer fold ", k, ".")
    }
    selected <- sconjoint:::.sc_comp_select_candidate(
      score, fold_n, computational_gate,
      complexity_rank = candidates$complexity_rank,
      tie_tolerance = gate$tie_tolerance)
    candidate_gate_eligibility <- lapply(
      gate_matrices, function(x) apply(x, 1L, all))
    gate_eligibility <- unname(candidate_gate_eligibility)
    names(gate_eligibility) <- c(
      "computationally_eligible", "nesting_eligible",
      "shared_pooled_prefit_eligible", "continued_constant_eligible")
    combined_eligibility <- Reduce(`&`, gate_eligibility) &
      selected$score_eligible
    selection_valid <- isTRUE(gate$pass) &&
      identical(selected$selected, tuning$selected_index) &&
      identical(selected$selected, gate$selected_index) &&
      identical(selected$cv_log_score, candidates$cv_sequence_log_score) &&
      identical(selected$selection_score, candidates$selection_score) &&
      identical(selected$computationally_eligible,
                gate$computationally_eligible) &&
      all(vapply(names(gate_eligibility), function(nm) {
        identical(gate[[nm]], gate_eligibility[[nm]])
      }, logical(1L))) &&
      identical(selected$score_eligible, gate$score_eligible) &&
      identical(combined_eligibility, gate$selection_eligible) &&
      identical(selected$within_selection_tie, gate$within_selection_tie) &&
      identical(selected$ineligible_reason, gate$ineligible_reason) &&
      identical(selected$complexity_rank, candidates$complexity_rank) &&
      all(vapply(names(gate_matrices), function(nm) {
        identical(candidates[[nm]], candidate_gate_eligibility[[nm]])
      }, logical(1L))) &&
      identical(candidates$all_inner_scores_finite,
                selected$score_eligible) &&
      identical(candidates$selection_eligible, combined_eligibility) &&
      identical(candidates$within_selection_tie,
                selected$within_selection_tie) &&
      identical(candidates$ineligible_reason, selected$ineligible_reason) &&
      identical(sum(candidates$selected), 1L) &&
      isTRUE(candidates$selected[[selected$selected]]) &&
      identical(tuning$selected, specifications[[selected$selected]]) &&
      identical(tuning$refit$specification, tuning$selected)
    if (!isTRUE(selection_valid)) {
      .gs_v281_stop("Historical candidate selection cannot be reproduced in ",
                   "outer fold ", k, ".")
    }
  }
  outer_gate <- nested$candidate_selection_gate_by_outer_fold
  expected_outer_gate <- vapply(nested$tuning, function(x) {
    selected <- x$selected_index
    is.list(x$candidate_selection_gate) &&
      isTRUE(x$candidate_selection_gate$pass) &&
      length(selected) == 1L && !is.na(selected) &&
      isTRUE(x$candidate_selection_gate$selection_eligible[[selected]])
  }, logical(1L))
  if (!is.logical(outer_gate) || anyNA(outer_gate) ||
      !identical(names(outer_gate), names(nested$tuning)) ||
      !identical(unname(outer_gate), unname(expected_outer_gate))) {
    .gs_v281_stop("Historical outer-fold candidate-selection gates disagree ",
                 "with their tuning records.")
  }
  invisible(TRUE)
}

.gs_v281_prepare_input <- function(protocol, source_root) {
  prepared <- readRDS(.gs_v281_input_path(protocol, source_root,
                                          "prepared_primary"))
  required_names <- c("deltaX", "y", "Z_primary", "respondent_id", "task")
  if (!is.list(prepared) || !all(required_names %in% names(prepared)) ||
      !identical(dim(prepared$deltaX), c(20657L, 30L)) ||
      !identical(dim(prepared$Z_primary), c(20657L, 22L)) ||
      length(prepared$y) != 20657L ||
      length(prepared$respondent_id) != 20657L ||
      length(prepared$task) != 20657L ||
      is.null(colnames(prepared$deltaX)) ||
      is.null(colnames(prepared$Z_primary)) ||
      anyDuplicated(colnames(prepared$deltaX)) ||
      anyDuplicated(colnames(prepared$Z_primary)) ||
      any(!is.finite(prepared$deltaX)) ||
      any(!is.finite(prepared$Z_primary)) ||
      length(unique(prepared$respondent_id)) != 1605L ||
      !identical(as.integer(range(table(prepared$respondent_id))), c(1L, 13L)) ||
      any(!prepared$y %in% 0:1)) {
    .gs_v281_stop("The authoritative prepared input has an unexpected schema.")
  }
  nested <- readRDS(.gs_v281_input_path(protocol, source_root,
                                        "v21_primary_nested"))
  assembled <- readRDS(.gs_v281_input_path(protocol, source_root,
                                           "v21_primary_assembled"))
  if (!inherits(nested, "scmix_nested_tuning") ||
      !inherits(assembled, "scmix_nested_assembled") ||
      !identical(nested$raw_data$deltaX, prepared$deltaX) ||
      !identical(nested$raw_data$y, prepared$y) ||
      !identical(nested$raw_data$Z, prepared$Z_primary) ||
      !identical(as.character(nested$raw_data$respondent_id),
                 as.character(prepared$respondent_id)) ||
      !identical(as.character(assembled$attr_names),
                 colnames(prepared$deltaX)) ||
      !identical(as.character(assembled$z_names),
                 colnames(prepared$Z_primary)) ||
      !identical(as.integer(assembled$fold_id),
                 as.integer(nested$outer_fold_index))) {
    .gs_v281_stop(
      "Prepared, nested, and assembled Graham--Svolik inputs do not agree.")
  }
  fold_index <- as.integer(nested$outer_fold_index)
  if (length(fold_index) != nrow(prepared$deltaX) ||
      !setequal(unique(fold_index), 1:5)) {
    .gs_v281_stop("The frozen outer-fold index is malformed.")
  }
  by_respondent <- split(fold_index, as.character(prepared$respondent_id))
  if (any(lengths(lapply(by_respondent, unique)) != 1L)) {
    .gs_v281_stop("The frozen outer fold varies within respondent.")
  }
  for (k in 1:5) {
    historical <- nested$tuning[[k]]$refit
    training_rows <- .gs_v281_scope_rows(prepared, historical)
    if (!identical(training_rows, which(fold_index != k))) {
      .gs_v281_stop("Historical outer initializer is not the exact training ",
                   "complement for fold ", k, ".")
    }
  }
  .gs_v281_assert_historical_inner_tuning(prepared, nested, fold_index)
  out <- list(
    schema_version = "gs2020-v2.8.1-verified-input-v1",
    prepared = prepared,
    fold_index = as.integer(nested$outer_fold_index),
    outer_fold_labels = as.character(nested$outer_folds),
    orientation = prepared$provenance$orientation,
    source_hashes = stats::setNames(protocol$inputs$sha256,
                                    protocol$inputs$input_id),
    preparation_uses_outcome = FALSE,
    prior_full_fit_available = FALSE)
  out
}

.gs_v281_prepare_scope_z <- function(preflight, rows) {
  prepared <- preflight$prepared
  rows <- as.integer(rows)
  rid <- as.character(prepared$respondent_id[rows])
  Z <- as.matrix(prepared$Z_primary[rows, , drop = FALSE])
  if (any(!is.finite(Z))) {
    .gs_v281_stop("The frozen Graham--Svolik moderators must be finite.")
  }
  transform <- sconjoint:::.sc_fit_z_transform(Z, rid)
  internal <- sconjoint:::.sc_apply_z_transform(Z, transform)
  if (any(!is.finite(internal))) .gs_v281_stop("Nonfinite moderator transform.")
  list(Z = Z, transform = transform, internal = internal,
       imputation = "none")
}

.gs_v281_affine_map <- function(x_new, x_old, eligible,
                               tolerance = 1e-10) {
  eligible <- as.logical(eligible)
  if (length(x_new) != length(x_old) || length(eligible) != length(x_new) ||
      sum(eligible) < 2L || any(!is.finite(x_new[eligible])) ||
      any(!is.finite(x_old[eligible]))) {
    .gs_v281_stop("Malformed affine-remap inputs.")
  }
  xn <- x_new[eligible]
  xo <- x_old[eligible]
  denominator <- sum((xn - mean(xn))^2)
  if (!is.finite(denominator) || denominator <= 0) {
    .gs_v281_stop("The new moderator coordinate has no valid variation.")
  }
  slope <- sum((xn - mean(xn)) * (xo - mean(xo))) / denominator
  intercept <- mean(xo) - slope * mean(xn)
  error <- max(abs(xo - (intercept + slope * xn)))
  if (!is.finite(error) || error > tolerance) {
    .gs_v281_stop("The old and new moderator coordinates are not affine.")
  }
  list(intercept = intercept, slope = slope, max_error = error,
       n_eligible = sum(eligible))
}

.gs_v281_affine_maps <- function(x_new, x_old, tolerance = 1e-10) {
  x_new <- as.matrix(x_new)
  x_old <- as.matrix(x_old)
  if (!identical(dim(x_new), dim(x_old)) ||
      !identical(colnames(x_new), colnames(x_old))) {
    .gs_v281_stop("Moderator matrices do not share the affine-remap contract.")
  }
  maps <- lapply(seq_len(ncol(x_new)), function(j) {
    .gs_v281_affine_map(x_new[, j], x_old[, j],
                       rep(TRUE, nrow(x_new)), tolerance = tolerance)
  })
  data.frame(
    moderator = colnames(x_new),
    intercept = vapply(maps, `[[`, numeric(1L), "intercept"),
    slope = vapply(maps, `[[`, numeric(1L), "slope"),
    max_error = vapply(maps, `[[`, numeric(1L), "max_error"),
    stringsAsFactors = FALSE)
}

.gs_v281_canonical_coefficient_scale <- function(
    preprocessing_scale, architecture_scale, attribute_names,
    tolerance = 1e-12) {
  ## Preserve names before coercion so the contract checks identity, not just
  ## positional numerical agreement.
  prep_names <- names(preprocessing_scale)
  arch_names <- names(architecture_scale)
  attribute_names <- as.character(attribute_names)
  prep_raw <- as.numeric(preprocessing_scale)
  arch_raw <- as.numeric(architecture_scale)
  if (!length(attribute_names) || anyDuplicated(attribute_names) ||
      any(!nzchar(attribute_names)) || length(prep_raw) != length(attribute_names) ||
      length(arch_raw) != length(attribute_names) ||
      !identical(prep_names, attribute_names) ||
      !identical(arch_names, attribute_names) ||
      any(!is.finite(prep_raw)) || any(prep_raw <= 0) ||
      any(!is.finite(arch_raw)) || any(arch_raw <= 0)) {
    .gs_v281_stop("The preprocessing and architecture coefficient-scale ",
                 "metadata do not share the exact attribute contract.")
  }
  discrepancy <- max(abs(prep_raw - arch_raw) /
                       pmax(abs(prep_raw), abs(arch_raw), 1))
  if (!is.finite(discrepancy) || discrepancy > tolerance) {
    .gs_v281_stop("The preprocessing and architecture coefficient scales differ.")
  }
  stats::setNames(prep_raw, attribute_names)
}

.gs_v281_tensor_array <- function(x) {
  if (inherits(x, "torch_tensor")) {
    return(torch::as_array(x$detach()$cpu()))
  }
  x
}

.gs_v281_build_target_net <- function(state, q, coefficient_scale = NULL) {
  a <- state$architecture
  if (is.null(coefficient_scale)) coefficient_scale <- a$coefficient_scale
  sconjoint:::.sc_build_mixed_network(
    p = a$p, p_Z = a$p_Z, q = as.integer(q), hidden = a$hidden,
    mean_family = a$mean_family, mu_bound = a$mu_bound,
    kappa_bound = a$kappa_bound, alpha_bound = a$alpha_bound,
    a_bound = a$a_bound, weight_bound = a$weight_bound,
    coefficient_scale = coefficient_scale, compute_dtype = "float64")
}

.gs_v281_arrays_to_warm <- function(arrays, target_net) {
  target <- target_net$state_dict()
  if (!identical(names(arrays), names(target))) {
    .gs_v281_stop("Initializer fields do not match the target network.")
  }
  out <- lapply(names(target), function(nm) {
    value <- arrays[[nm]]
    if (!identical(as.integer(dim(value)), as.integer(target[[nm]]$shape))) {
      ## Vectors have NULL dim but a one-dimensional Torch shape.
      if (!(is.null(dim(value)) && length(target[[nm]]$shape) == 1L &&
            length(value) == as.integer(target[[nm]]$shape[[1L]]))) {
        .gs_v281_stop("Initializer tensor shape mismatch for ", nm, ".")
      }
    }
    torch::torch_tensor(value, dtype = target[[nm]]$dtype, device = "cpu")
  })
  names(out) <- names(target)
  out
}

.gs_v281_unbounded_relu_deviation <- function(net, Z_new) {
  if (!inherits(net, "nn_module") ||
      !identical(net$mean_family, "relu") ||
      !is.matrix(Z_new) || !is.numeric(Z_new) || any(!is.finite(Z_new))) {
    .gs_v281_stop("Cannot evaluate the unbounded corrected-ReLU deviation.")
  }
  z <- torch::torch_tensor(
    Z_new, dtype = net$kappa_raw$dtype, device = net$kappa_raw$device)
  deviation <- torch::with_no_grad({
    h <- z
    for (i in seq_along(net$hidden)) {
      h <- torch::nnf_relu(net$hidden[[i]](h))
    }
    z0 <- torch::torch_zeros_like(z)
    h0 <- z0
    for (i in seq_along(net$hidden)) {
      h0 <- torch::nnf_relu(net$hidden[[i]](h0))
    }
    net$param_layer(h) - net$param_layer(h0)
  })
  as.matrix(torch::as_array(deviation$detach()$cpu()))
}

.gs_v281_q1_initializer <- function(fit, preflight, fitting_rows, source_rows,
                                   prediction_tolerance = 1e-6) {
  state <- fit$network_state
  fit_attribute_names <- colnames(as.matrix(fit$raw_data$deltaX))
  fit_moderator_names <- colnames(as.matrix(fit$raw_data$Z))
  if (!inherits(state, "scmix_network_state") ||
      !identical(fit$preprocessing, state$preprocessing) ||
      !identical(fit_attribute_names, state$coefficient_names) ||
      !identical(fit_moderator_names, state$moderator_names)) {
    .gs_v281_stop("The fit-level preprocessing aliases differ from the ",
                 "portable q=1 network state.")
  }
  old_scale <- .gs_v281_canonical_coefficient_scale(
    fit$preprocessing$deltaX$scale,
    state$architecture$coefficient_scale,
    fit_attribute_names)
  repaired <- .gs_v281_prepare_scope_z(preflight, fitting_rows)
  source_Z <- as.matrix(
    preflight$prepared$Z_primary[source_rows, , drop = FALSE])
  source_internal_new <- sconjoint:::.sc_apply_z_transform(
    source_Z, repaired$transform)
  old_Z <- as.matrix(fit$raw_data$Z)
  old_internal <- sconjoint:::.sc_apply_z_transform(
    old_Z, fit$preprocessing$Z)
  affine <- .gs_v281_affine_maps(source_internal_new, old_internal)

  dx_transform <- sconjoint:::.sc_comp_fit_dx_scale(
    preflight$prepared$deltaX[fitting_rows, , drop = FALSE],
    preflight$prepared$respondent_id[fitting_rows])
  new_scale <- dx_transform$scale
  if (!identical(names(old_scale), names(new_scale)) ||
      any(!is.finite(old_scale)) || any(old_scale <= 0) ||
      any(!is.finite(new_scale)) || any(new_scale <= 0)) {
    .gs_v281_stop("Old and new coefficient scales cannot be remapped.")
  }
  output_ratio <- as.numeric(new_scale / old_scale)
  names(output_ratio) <- names(new_scale)

  original_arrays <- lapply(state$state_dict, .gs_v281_tensor_array)
  arrays <- original_arrays
  first_weight <- "hidden.0.weight"
  first_bias <- "hidden.0.bias"
  if (!all(c(first_weight, first_bias) %in% names(arrays))) {
    .gs_v281_stop("The selected q=1 state lacks its first ReLU layer.")
  }
  old_weight <- arrays[[first_weight]]
  arrays[[first_weight]] <- sweep(old_weight, 2L, affine$slope, `*`)
  arrays[[first_bias]] <- arrays[[first_bias]] +
    as.numeric(old_weight %*% affine$intercept)
  before_net <- .gs_v281_build_target_net(state, q = 1L)
  before_warm <- .gs_v281_arrays_to_warm(original_arrays, before_net)
  before_net$load_state_dict(before_warm)
  before_net$eval()
  ## Corrected ReLU means are alpha + g(z)-g(0), followed by clipping.  Mapping
  ## z_old=a+b*z_new preserves g(z), but changes which raw point is called
  ## zero.  Evaluate the unbounded deviation directly: differencing clipped
  ## beta predictions here would produce the wrong alpha shift near a bound.
  zero_old <- matrix(0, nrow = 1L, ncol = ncol(old_internal))
  mapped_new_zero <- matrix(affine$intercept, nrow = 1L)
  alpha_shift <- as.numeric(
    .gs_v281_unbounded_relu_deviation(before_net, mapped_new_zero) -
      .gs_v281_unbounded_relu_deviation(before_net, zero_old))
  arrays$alpha_raw <- (arrays$alpha_raw + alpha_shift) * output_ratio
  arrays$param_layer.weight <- sweep(
    arrays$param_layer.weight, 1L, output_ratio, `*`)
  arrays$A <- sweep(arrays$A, 1L, output_ratio, `*`)
  target_architecture <- state$architecture
  target_architecture$coefficient_scale <- new_scale
  target_state <- state
  target_state$architecture <- target_architecture
  target_net <- .gs_v281_build_target_net(target_state, q = 1L,
                                          coefficient_scale = new_scale)
  arrays$mu_bound_internal <- .gs_v281_tensor_array(
    target_net$state_dict()$mu_bound_internal)
  warm <- .gs_v281_arrays_to_warm(arrays, target_net)
  target_net$load_state_dict(warm)
  target_net$eval()

  fitting_Z <- as.matrix(
    preflight$prepared$Z_primary[fitting_rows, , drop = FALSE])
  fitting_internal_old <- sconjoint:::.sc_apply_z_transform(
    fitting_Z, state$preprocessing$Z)
  mapped_internal_old <- sweep(repaired$internal, 2L, affine$slope, `*`)
  mapped_internal_old <- sweep(
    mapped_internal_old, 2L, affine$intercept, `+`)
  affine_fitting_error <- max(abs(fitting_internal_old - mapped_internal_old))
  if (!is.finite(affine_fitting_error) || affine_fitting_error > 1e-10) {
    .gs_v281_stop("The moderator remap does not extend to every fitting row.")
  }
  before_prediction <- sconjoint:::.sc_predict_beta(
    before_net, fitting_internal_old)
  new_prediction <- sconjoint:::.sc_predict_beta(
    target_net, repaired$internal)
  reproduction_error <- max(abs(
    sweep(before_prediction, 2L, old_scale, `/`) -
      sweep(new_prediction, 2L, new_scale, `/`)))
  if (!is.finite(reproduction_error) ||
      reproduction_error > prediction_tolerance) {
    .gs_v281_stop("The full/fold state remap failed raw prediction invariance.")
  }
  old_net <- sconjoint::scmix_restore_network(state, device = "cpu")
  historical_prediction <- sconjoint:::.sc_predict_beta(
    old_net, fitting_internal_old)
  conversion_error <- max(abs(historical_prediction - before_prediction))
  list(warm = warm, repaired = repaired, affine = affine,
       alpha_shift = alpha_shift,
       dx_transform = dx_transform, coefficient_scale = new_scale,
       target_architecture = target_architecture,
       valid_prediction_max_difference = reproduction_error,
       affine_fitting_max_difference = affine_fitting_error,
       float32_to_float64_prediction_max_difference = conversion_error,
       intentionally_changed_rows = 0L, source_state = state)
}

.gs_v281_orthogonal_column <- function(a1_raw, ratio = 0.05) {
  a1_raw <- as.numeric(a1_raw)
  p <- length(a1_raw)
  if (!p || any(!is.finite(a1_raw)) || !is.finite(ratio) || ratio <= 0) {
    .gs_v281_stop("Invalid q=2 loading initializer request.")
  }
  base <- sin(seq_len(p) * sqrt(2)) + cos(seq_len(p) * sqrt(3))
  denom <- sum(a1_raw^2)
  if (!is.finite(denom) || sqrt(denom) <= 1e-10) {
    .gs_v281_stop("The q=1 raw loading norm is too small to define the frozen ",
                 "relative q=2 initializer.")
  }
  v <- base - a1_raw * sum(base * a1_raw) / denom
  if (sum(v^2) < 1e-20) {
    basis <- rep(0, p)
    basis[which.min(abs(a1_raw))] <- 1
    v <- basis - a1_raw * sum(basis * a1_raw) / denom
  }
  v <- v / sqrt(sum(v^2))
  pivot <- which.max(abs(v))
  if (v[pivot] < 0) v <- -v
  magnitude <- ratio * sqrt(denom)
  out <- magnitude * v
  if (abs(sum(out * a1_raw)) > 1e-12 *
      max(1, sqrt(sum(out^2) * denom))) {
    .gs_v281_stop("The deterministic q=2 column is not orthogonal.")
  }
  out
}

.gs_v281_rank_initializer <- function(q1_fit, q) {
  q <- as.integer(q)
  if (!q %in% c(0L, 2L) ||
      !inherits(q1_fit$network_state, "scmix_network_state") ||
      !identical(q1_fit$specification$q, 1L)) {
    .gs_v281_stop("Rank sensitivity initialization requires a completed q=1 fit.")
  }
  state <- q1_fit$network_state
  target_net <- .gs_v281_build_target_net(state, q)
  target <- target_net$state_dict()
  source <- lapply(state$state_dict, .gs_v281_tensor_array)
  arrays <- lapply(target, .gs_v281_tensor_array)
  common <- intersect(names(source), names(arrays))
  for (nm in setdiff(common, "A")) arrays[[nm]] <- source[[nm]]
  second_raw <- numeric()
  if (q == 2L) {
    scale <- as.numeric(state$architecture$coefficient_scale)
    A1 <- as.matrix(source$A)
    if (!identical(dim(A1), c(length(scale), 1L))) {
      .gs_v281_stop("The q=1 loading initializer is malformed.")
    }
    a1_raw <- A1[, 1L] / scale
    second_raw <- .gs_v281_orthogonal_column(a1_raw, ratio = 0.05)
    arrays$A <- cbind(A1[, 1L], scale * second_raw)
  }
  warm <- .gs_v281_arrays_to_warm(arrays, target_net)
  target_net$load_state_dict(warm)
  list(warm = warm, second_loading_raw = second_raw,
       source_q1_state_hash = sconjoint:::.scmix_stable_state_hash(
         state$state_dict))
}

.gs_v281_perturb_warm <- function(warm, seed, scale = 0.02) {
  withr::local_preserve_seed()
  set.seed(seed)
  out <- lapply(warm, function(tensor) {
    value <- torch::as_array(tensor$detach()$cpu())
    ## Buffers are not free parameters and must remain exact.
    value
  })
  free <- setdiff(names(out), "mu_bound_internal")
  for (nm in free) {
    value <- out[[nm]]
    delta <- stats::rnorm(length(value)) * scale * pmax(1, abs(as.numeric(value)))
    dim(delta) <- dim(value)
    out[[nm]] <- value + delta
  }
  target <- lapply(warm, function(x) x$clone())
  for (nm in names(target)) {
    target[[nm]] <- torch::torch_tensor(out[[nm]], dtype = warm[[nm]]$dtype,
                                         device = "cpu")
  }
  target
}

.gs_v281_constant_warm <- function(warm) {
  out <- lapply(warm, function(x) x$clone())
  deviations <- grep("^(hidden\\.|param_layer\\.)", names(out), value = TRUE)
  torch::with_no_grad({
    for (nm in deviations) out[[nm]]$zero_()
  })
  out
}

.gs_v281_grid <- function(q, refined = FALSE) {
  q <- as.integer(q)
  nodes <- if (q == 0L) 1L else if (q == 1L) {
    if (isTRUE(refined)) 45L else 31L
  } else if (q == 2L) {
    if (isTRUE(refined)) 15L else 9L
  } else .gs_v281_stop("The prospective runner supports only q=0,1,2.")
  sconjoint:::.sc_mixed_grid(q = q, integration = "gh", n_nodes = nodes,
                             n_draws = 1L, seed = 1L,
                             antithetic = TRUE, scramble = FALSE)
}

.gs_v281_fit_one_stage <- function(warm, dat, z_internal, scale, spec, q,
                                  seed, architecture, grid, max_iter,
                                  learning_rate, verbose = FALSE) {
  sconjoint:::.sc_train_mixed_multistart(
    deltaX = sweep(dat$deltaX, 2L, scale, `/`), y = dat$y,
    Z = z_internal, respondent_id = dat$respondent_id, gh = grid,
    hidden = spec$hidden, mean_family = spec$mean_family,
    n_epochs = 0L, learning_rate = 0.01,
    weight_decay = spec$weight_decay, n_starts = 1L, seed = seed,
    device = "cpu", verbose = isTRUE(verbose), warm_state = warm,
    warm_start_mode = "exact_state", early_stop = FALSE,
    opt_tol = 0, grad_tol = 0, kkt_tol = NULL,
    protocol_mode = "calibration", calibration_verification = NULL,
    polish_max_iter = as.integer(max_iter),
    polish_history_size = 50L,
    polish_tolerance_grad = 0, polish_tolerance_change = 0,
    polish_lr = as.numeric(learning_rate), mu_bound = architecture$mu_bound,
    kappa_bound = architecture$kappa_bound,
    alpha_bound = architecture$alpha_bound,
    a_bound = architecture$a_bound, weight_bound = architecture$weight_bound,
    coefficient_scale = scale)
}

.gs_v281_endpoint_warm <- function(fit) {
  if (!is.list(fit) || is.null(fit$net) ||
      !is.function(fit$net$state_dict)) {
    .gs_v281_stop("A staged optimizer result lacks its returned network state.")
  }
  state <- fit$net$state_dict()
  lapply(state, function(x) x$detach()$cpu()$clone())
}

.gs_v281_stage_ledger_row <- function(fit, stage, max_iter, learning_rate,
                                     cumulative_configured_max,
                                     cumulative_reported_iterations,
                                     input_state_hash,
                                     returned_state_hash) {
  actual <- as.integer(fit$polish_iterations)
  if (length(actual) != 1L ||
      (!is.na(actual) && (actual < 0L || actual > as.integer(max_iter)))) {
    .gs_v281_stop("A staged L-BFGS call reported an invalid iteration count.")
  }
  accepted <- isTRUE(fit$polish$accepted) &&
    isTRUE(fit$lbfgs_completed) && isTRUE(fit$lbfgs_nonworsening)
  projection_free <- identical(fit$polish$projection_changed, FALSE)
  kkt_reached <- accepted && projection_free &&
    is.finite(fit$kkt_maximum) && fit$kkt_maximum <= .gs_v281_kkt_stop
  data.frame(
    stage = as.integer(stage), configured_max_iter = as.integer(max_iter),
    learning_rate = as.numeric(learning_rate), actual_iterations = actual,
    cumulative_configured_max = as.integer(cumulative_configured_max),
    cumulative_reported_iterations =
      as.integer(cumulative_reported_iterations),
    penalized_mean_nll = as.numeric(fit$penalized_loss),
    unpenalized_mean_nll = as.numeric(fit$final_loss),
    objective_before = as.numeric(fit$objective_before_polish),
    objective_after = as.numeric(fit$objective_after_polish),
    kkt_maximum = as.numeric(fit$kkt_maximum),
    accepted = accepted, nonworsening = isTRUE(fit$lbfgs_nonworsening),
    projection_free = projection_free, kkt_stop_reached = kkt_reached,
    line_search_status = as.character(fit$line_search_status),
    input_state_hash = as.character(input_state_hash),
    returned_state_hash = as.character(returned_state_hash),
    stringsAsFactors = FALSE)
}

.gs_v281_fit_one_start <- function(warm, dat, z_internal, scale, spec, q,
                                  seed, architecture, grid, verbose = FALSE) {
  current_warm <- warm
  ledger <- vector("list", nrow(.gs_v281_polish_schedule))
  cumulative_configured <- 0L
  reported_iterations <- integer()
  fit <- NULL
  for (s in seq_len(nrow(.gs_v281_polish_schedule))) {
    schedule <- .gs_v281_polish_schedule[s, , drop = FALSE]
    input_state_hash <- sconjoint:::.scmix_stable_state_hash(lapply(
      current_warm, .gs_v281_tensor_array))
    fit <- .gs_v281_fit_one_stage(
      current_warm, dat, z_internal, scale, spec, q, seed, architecture,
      grid, schedule$max_iter[[1L]], schedule$learning_rate[[1L]], verbose)
    cumulative_configured <- cumulative_configured +
      schedule$max_iter[[1L]]
    reported_iterations <- c(reported_iterations,
                             as.integer(fit$polish_iterations))
    cumulative_reported <- if (anyNA(reported_iterations)) NA_integer_ else
      sum(reported_iterations)
    state_hash <- sconjoint:::.scmix_stable_state_hash(lapply(
      fit$net$state_dict(), .gs_v281_tensor_array))
    ledger[[s]] <- .gs_v281_stage_ledger_row(
      fit, schedule$stage[[1L]], schedule$max_iter[[1L]],
      schedule$learning_rate[[1L]], cumulative_configured,
      cumulative_reported, input_state_hash, state_hash)
    current_warm <- .gs_v281_endpoint_warm(fit)
    if (isTRUE(ledger[[s]]$kkt_stop_reached[[1L]])) break
  }
  used <- !vapply(ledger, is.null, logical(1L))
  ledger <- do.call(rbind, ledger[used])
  rownames(ledger) <- NULL
  if (nrow(ledger) < 1L ||
      max(ledger$cumulative_configured_max) > .gs_v281_polish_max_iter) {
    .gs_v281_stop("The staged L-BFGS ledger violates its fixed effort cap.")
  }
  if (nrow(ledger) > 1L && !identical(
      ledger$input_state_hash[-1L],
      ledger$returned_state_hash[-nrow(ledger)])) {
    .gs_v281_stop("A staged L-BFGS restart did not use the prior returned state.")
  }
  fit$v28_stage_ledger <- ledger
  fit$v28_stage_stop_reason <- if (tail(ledger$kkt_stop_reached, 1L)) {
    "accepted_state_reached_unchanged_KKT_cutoff"
  } else {
    "fixed_2000_iteration_schedule_exhausted_without_KKT_release"
  }
  fit$v28_cumulative_polish_iterations <-
    tail(ledger$cumulative_reported_iterations, 1L)
  fit
}

.gs_v281_is_float64 <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    tolower(x) %in% c("float64", "torch_float64", "torch.float64", "double",
                      "torch_double", "torch.double")
}

.gs_v281_gradient_audit <- function(fit, dat, repaired, scale, spec, grid,
                                   architecture, seed) {
  sconjoint:::.scmix_v28_gradient_contract_audit(
    net = fit$net, deltaX = dat$deltaX, y = dat$y,
    Z = repaired$internal, respondent_id = dat$respondent_id, grid = grid,
    weight_decay = spec$weight_decay, coefficient_scale = scale,
    kappa_bound = architecture$kappa_bound,
    alpha_bound = architecture$alpha_bound,
    a_bound = architecture$a_bound,
    weight_bound = architecture$weight_bound,
    steps = .gs_v281_gradient_steps, seed = as.integer(seed),
    relative_tolerance = .gs_v281_gradient_relative_tolerance,
    absolute_tolerance = .gs_v281_gradient_absolute_tolerance,
    device = "cpu")
}

.gs_v281_start_table <- function(starts, gradient_audits) {
  if (!is.list(starts) || length(starts) < 2L || is.null(names(starts)) ||
      any(!nzchar(names(starts))) ||
      !identical(names(starts), names(gradient_audits))) {
    .gs_v281_stop("Start audit requires matching named fits and gradients.")
  }
  data.frame(
    start_id = names(starts),
    penalized_mean_nll = vapply(starts, `[[`, numeric(1L),
                                  "penalized_loss"),
    unpenalized_mean_nll = vapply(starts, `[[`, numeric(1L), "final_loss"),
    objective_finite = vapply(starts, function(x)
      all(is.finite(c(x$penalized_loss, x$final_loss))), logical(1L)),
    dtype = vapply(starts, `[[`, character(1L), "dtype"),
    lbfgs_completed = vapply(starts, `[[`, logical(1L), "lbfgs_completed"),
    lbfgs_accepted = vapply(starts, function(x)
      isTRUE(x$polish$accepted), logical(1L)),
    lbfgs_nonworsening = vapply(starts, `[[`, logical(1L),
                                "lbfgs_nonworsening"),
    projection_diagnostic_complete = vapply(starts, function(x) {
      value <- x$polish$projection_changed
      is.logical(value) && length(value) == 1L && !is.na(value)
    }, logical(1L)),
    projection_changed = vapply(starts, function(x)
      isTRUE(x$polish$projection_changed), logical(1L)),
    bound_activity = vapply(starts, `[[`, logical(1L), "bound_activity"),
    gradient_contract_pass = vapply(gradient_audits, function(x)
      isTRUE(x$overall_pass), logical(1L)),
    kkt_maximum = vapply(starts, `[[`, numeric(1L), "kkt_maximum"),
    stringsAsFactors = FALSE)
}

.gs_v281_constant_gate <- function(x, gradient_audit = NULL) {
  is.list(x) &&
    all(is.finite(c(x$penalized_loss, x$final_loss, x$kkt_maximum))) &&
    .gs_v281_is_float64(x$dtype) && isTRUE(x$lbfgs_completed) &&
    isTRUE(x$polish$accepted) && isTRUE(x$lbfgs_nonworsening) &&
    identical(x$polish$projection_changed, FALSE) &&
    isTRUE(x$bound_diagnostics_complete) && !isTRUE(x$bound_activity) &&
    is.list(gradient_audit) && isTRUE(gradient_audit$overall_pass) &&
    x$kkt_maximum <= 1e-5
}

.gs_v281_capture_start_artifacts <- function(
    starts, gradient_audits, surface, dat, repaired, spec, q, grid,
    architecture, scale, signature, fit_scope) {
  out <- lapply(seq_along(starts), function(j) {
    x <- starts[[j]]
    state <- sconjoint:::.scmix_capture_network_state(
      x$net, p = ncol(dat$deltaX), p_Z = ncol(repaired$Z), q = q,
      hidden = spec$hidden, mean_family = spec$mean_family,
      mu_bound = architecture$mu_bound,
      kappa_bound = architecture$kappa_bound,
      alpha_bound = architecture$alpha_bound,
      a_bound = architecture$a_bound,
      weight_bound = architecture$weight_bound,
      coefficient_scale = scale, z_transform = repaired$transform,
      dx_transform = list(scale = stats::setNames(
        scale, colnames(dat$deltaX))),
      coefficient_names = colnames(dat$deltaX),
      moderator_names = colnames(repaired$Z), integration_grid = grid,
      analysis_signature = signature, scope = fit_scope)
    record <- list(
      start_id = names(starts)[[j]],
      selected = surface$selected[[j]],
      selection_eligible = surface$selection_eligible[[j]],
      target_comparison_eligible =
        surface$target_comparison_eligible[[j]],
      near_optimum = surface$near_optimum[[j]],
      inferior_start_diagnostic =
        surface$inferior_start_diagnostic[[j]],
      relative_objective_gap = surface$relative_objective_gap[[j]],
      penalized_mean_nll = x$penalized_loss,
      unpenalized_mean_nll = x$final_loss,
      A_raw = sconjoint:::.scmix_loading_to_raw(x$A, scale),
      A_optimization = x$A, kappa = x$kappa,
      moderator_preprocessing = list(
        imputation = "none",
        transform_hash = .gs_v281_object_sha256(repaired$transform)),
      network_state = state,
      state_hash = sconjoint:::.scmix_stable_state_hash(state$state_dict),
      optimization = sconjoint:::.sc_comp_fit_summary(x),
      kkt_contract = sconjoint:::.scmix_v28_kkt_contract(
        x$kkt_returned, .gs_v281_kkt_stop),
      gradient_contract = gradient_audits[[j]],
      stage_ledger = x$v28_stage_ledger)
    record$artifact_hash <- .gs_v281_object_sha256(record)
    record
  })
  names(out) <- names(starts)
  structure(out,
            protocol = "gs2020-v2.8.1-portable-start-artifacts-v1",
            class = c("gs_v281_start_artifacts", "list"))
}

.gs_v281_make_fit <- function(starts, constant_fit, dat, repaired,
                             spec, q, grid, scale, source, scope,
                             protocol_hash, git_commit) {
  architecture <- source$architecture
  canonical_scale <- .gs_v281_canonical_coefficient_scale(
    source$preprocessing$deltaX$scale,
    architecture$coefficient_scale, colnames(dat$deltaX))
  if (!identical(as.numeric(scale), as.numeric(canonical_scale)) ||
      !identical(names(scale), names(canonical_scale))) {
    .gs_v281_stop("The fit constructor did not receive the canonical ",
                 "coefficient scale used in optimization.")
  }
  signature <- sconjoint:::.sc_analysis_signature(
    dat$deltaX, dat$y, repaired$Z, dat$respondent_id,
    rep.int(1L, nrow(dat$deltaX)),
    list(protocol_id = .gs_v281_protocol_id, q = q, scope = scope,
         mean_family = spec$mean_family, hidden = spec$hidden,
         weight_decay = spec$weight_decay, integration_nodes = nrow(grid$U)))
  fit_scope <- paste0("gs2020 v2.8.1 q=", q, " ", scope,
                      " prospective numerical protocol")
  gradient_audits <- lapply(seq_along(starts), function(j) {
    .gs_v281_gradient_audit(
      starts[[j]], dat, repaired, scale, spec, grid, architecture,
      .gs_v281_seed(q, scope, names(starts)[[j]]))
  })
  names(gradient_audits) <- names(starts)
  start_input <- .gs_v281_start_table(starts, gradient_audits)
  selection <- sconjoint:::.scmix_v28_start_surface(
    start_input, near_objective_cutoff = .gs_v281_near_objective_cutoff,
    kkt_cutoff = .gs_v281_kkt_stop)
  selected_index <- selection$selected_index
  selected <- starts[[selected_index]]
  selected_gradient <- gradient_audits[[selected_index]]
  constant_gradient <- .gs_v281_gradient_audit(
    constant_fit, dat, repaired, scale, spec, grid, architecture,
    .gs_v281_seed(q, scope, "constant"))

  loading <- sconjoint:::.scmix_loading_invariance_evidence(
    selected$net, dat$deltaX, dat$y, repaired$internal,
    dat$respondent_id, grid, scale,
    analysis_signature = signature, scope = fit_scope,
    protocol_mode = "calibration", calibration_verified = FALSE,
    calibration_hash = NA_character_, probability_cutoff = NULL,
    device = "cpu")
  state <- sconjoint:::.scmix_capture_network_state(
    selected$net, p = ncol(dat$deltaX), p_Z = ncol(repaired$Z), q = q,
    hidden = spec$hidden, mean_family = spec$mean_family,
    mu_bound = architecture$mu_bound, kappa_bound = architecture$kappa_bound,
    alpha_bound = architecture$alpha_bound, a_bound = architecture$a_bound,
    weight_bound = architecture$weight_bound, coefficient_scale = scale,
    z_transform = repaired$transform,
    dx_transform = source$preprocessing$deltaX,
    coefficient_names = colnames(dat$deltaX),
    moderator_names = colnames(repaired$Z), integration_grid = grid,
    analysis_signature = signature, scope = fit_scope)
  state <- sconjoint:::.scmix_attach_network_loading_evidence(state, loading)
  A <- sconjoint:::.scmix_loading_to_raw(selected$A, scale)
  penalized_objective <- start_input$penalized_mean_nll
  unpenalized_objective <- start_input$unpenalized_mean_nll
  start_audit <- selection$surface
  nested_relative <- (selected$penalized_loss - constant_fit$penalized_loss) /
    max(1, abs(constant_fit$penalized_loss))
  l002_diff <- loading$maximum_absolute_probability_difference %||% Inf
  constant_ok <- .gs_v281_constant_gate(constant_fit, constant_gradient)
  selected_kkt <- sconjoint:::.scmix_v28_kkt_contract(
    selected$kkt_returned, .gs_v281_kkt_stop)
  gates <- list(
    selected_and_constant_finite = all(is.finite(c(
      selected$penalized_loss, selected$final_loss,
      selected_kkt$maximum_block_rms, constant_fit$penalized_loss,
      constant_fit$final_loss))),
    float64 = .gs_v281_is_float64(selected$dtype),
    lbfgs = isTRUE(selected$lbfgs_completed) &&
      isTRUE(selected$polish$accepted) &&
      isTRUE(selected$lbfgs_nonworsening),
    gradient_contract = isTRUE(selected_gradient$overall_pass),
    blockwise_kkt = isTRUE(selected_kkt$pass),
    bounds = isTRUE(selected$bound_diagnostics_complete) &&
      !isTRUE(selected$bound_activity),
    constant_reference = constant_ok,
    nested_objective = constant_ok && is.finite(nested_relative) &&
      nested_relative <= 1e-6,
    start_selection_contract = !selection$diagnostic_selection &&
      !selection$lower_objective_inadmissible,
    loading_invariance = is.finite(l002_diff) &&
      l002_diff <= .gs_v281_loading_invariance_cutoff)
  gates$pass <- all(unlist(gates, use.names = FALSE))
  optimization <- sconjoint:::.sc_comp_fit_summary(selected)
  optimization$v28_stage_ledger <- selected$v28_stage_ledger
  optimization$v28_stage_stop_reason <- selected$v28_stage_stop_reason
  constant_optimization <- sconjoint:::.sc_comp_fit_summary(constant_fit)
  constant_optimization$v28_stage_ledger <- constant_fit$v28_stage_ledger
  constant_optimization$v28_stage_stop_reason <-
    constant_fit$v28_stage_stop_reason
  optimization$nested_objective_gate <- list(
    pass = gates$nested_objective, relative_gap = nested_relative,
    tolerance = 1e-6, applicable = TRUE, v22_applicable = FALSE,
    v22_pass = FALSE)
  start_artifacts <- .gs_v281_capture_start_artifacts(
    starts, gradient_audits, selection$surface, dat, repaired, spec, q,
    grid, architecture, scale, signature, fit_scope)
  fit <- list(
    net = selected$net, network_state = state,
    multistart_artifacts = start_artifacts,
    mu = sweep(sconjoint:::.sc_predict_beta(selected$net, repaired$internal),
               2L, scale, `/`),
    A = A, Sigma = tcrossprod(A), kappa = selected$kappa,
    specification = list(
      name = paste0("v281_same_mean_q", q), mean_family = spec$mean_family,
      hidden = as.integer(spec$hidden), weight_decay = spec$weight_decay,
      integration = "gh", n_nodes = as.integer(if (q == 0L) 1L else
        if (q == 1L) 31L else 9L), n_draws = 1L, q = as.integer(q)),
    integration_grid = grid,
    preprocessing = list(Z = repaired$transform,
                         deltaX = source$preprocessing$deltaX),
    optimization = optimization,
    pooled_prefit_optimization = NULL,
    continued_constant_optimization = constant_optimization,
    analysis_signature = signature,
    raw_data = list(deltaX = dat$deltaX, y = dat$y, Z = repaired$Z,
                    respondent_id = dat$respondent_id),
    scope = fit_scope, protocol_mode = "calibration",
    calibration_only = TRUE, calibration_verification = NULL,
    v22_fit_computation_subset_pass = FALSE,
    v28_application_gate = gates,
    v28_start_audit = start_audit,
    v28_all_start_finiteness_audit = data.frame(
      start_id = start_audit$start_id,
      objective_finite = is.finite(penalized_objective) &
        is.finite(unpenalized_objective),
      kkt_finite = is.finite(start_audit$kkt_maximum),
      gated = start_audit$selected,
      stringsAsFactors = FALSE),
    v28_start_selection = selection,
    v28_gradient_contract = selected_gradient,
    v28_constant_gradient_contract = constant_gradient,
    v28_kkt_contract = selected_kkt,
    v28_selected_start = names(starts)[selected_index],
    v28_start_selection_criterion = "minimum penalized training mean NLL",
    v28_constant_relative_gap = nested_relative,
    v28_optimizer_stage_ledgers = c(
      lapply(starts, `[[`, "v28_stage_ledger"),
      list(constant = constant_fit$v28_stage_ledger)),
    v28_optimizer_effort = list(
      polish_max_iterations = .gs_v281_polish_max_iter,
      stage_max_iterations = .gs_v281_polish_schedule$max_iter,
      stage_learning_rates = .gs_v281_polish_schedule$learning_rate,
      optimizer_tolerance_grad = 0, optimizer_tolerance_change = 0,
      statistical_gates_changed = FALSE,
      amendment_scope = paste(
        "start eligibility, near-optimum semantics, portable start states,",
        "and independent blockwise gradient verification")),
    v28_source = source$provenance,
    v28_protocol_hash = protocol_hash, v28_git_commit = git_commit,
    v281_initialization = list(
      moderator_imputation = "none",
      valid_prediction_max_difference = source$prediction_difference %||% 0,
      intentionally_changed_rows = source$changed_rows %||% 0))
  fit <- sconjoint:::.scmix_add_loading_contract(
    fit, q = q, raw_full = A, optimization_full = selected$A,
    coefficient_scale_full = scale, invariance_evidence = loading,
    source = "Graham--Svolik v2.8.1 prospective numerical protocol with paired raw and optimization scales")
  class(fit) <- c("scmix_tuned_matrix_fit", "list")
  sconjoint::scmix_portable_copy(fit)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
