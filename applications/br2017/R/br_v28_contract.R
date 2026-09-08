## Ballard--Rosa version 2.8 optimizer-effort amendment helpers.
##
## This file contains no top-level outcome-generating action.  The CLI in
## 07_run_fast_v28.R calls these functions only after the protocol and code
## have been committed.  Source artifacts are always read-only.

.br_v28_protocol_id <- "br2017-paperps-prospective-2026-09-07-v2.8-v1"
.br_v28_run_relative <- file.path(
  "runs", "br2017", "v28", "prospective-2026-09-07-v1")
.br_v28_scopes <- c("full", paste0("outer_", 1:5))
.br_v28_ranks <- 0:2
.br_v28_polish_schedule <- data.frame(
  stage = 1:4,
  max_iter = c(100L, 400L, 500L, 1000L),
  learning_rate = c(1, 1, 1, 0.25),
  stringsAsFactors = FALSE)
.br_v28_polish_max_iter <- sum(.br_v28_polish_schedule$max_iter)
.br_v28_kkt_stop <- 1e-5
.br_v28_near_objective_cutoff <- 1e-6
.br_v28_loading_invariance_cutoff <- 1e-8
.br_v28_gradient_steps <- c(
  1e-3, 3e-4, 1e-4, 3e-5, 1e-5, 3e-6, 1e-6, 3e-7, 1e-7)
.br_v28_gradient_relative_tolerance <- 1e-4
.br_v28_gradient_absolute_tolerance <- 1e-8
.br_v28_rng_kind <- c("L'Ecuyer-CMRG", "Inversion", "Rejection")

.br_v28_stop <- function(...) stop(..., call. = FALSE)

.br_v28_set_rng_contract <- function() {
  do.call(RNGkind, as.list(.br_v28_rng_kind))
  .br_v28_assert_rng_contract()
}

.br_v28_assert_rng_contract <- function() {
  observed <- unname(RNGkind())
  if (!identical(observed, .br_v28_rng_kind)) {
    .br_v28_stop("The active RNG kinds do not match the frozen v2.8 registry.")
  }
  observed
}

.br_v28_set_thread_contract <- function() {
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
             OPENBLAS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
             TORCH_NUM_THREADS = "1")
  if (requireNamespace("torch", quietly = TRUE)) {
    torch::torch_set_num_threads(1L)
    torch::torch_set_num_interop_threads(1L)
  }
  invisible(.br_v28_assert_thread_contract())
}

.br_v28_assert_thread_contract <- function() {
  variables <- c("OMP_NUM_THREADS", "MKL_NUM_THREADS",
                 "OPENBLAS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS",
                 "TORCH_NUM_THREADS")
  environment <- Sys.getenv(variables, unset = NA_character_)
  torch_threads <- c(
    intraop = as.integer(torch::torch_get_num_threads()),
    interop = as.integer(torch::torch_get_num_interop_threads()))
  if (anyNA(environment) || any(environment != "1") ||
      !identical(unname(torch_threads), c(1L, 1L))) {
    .br_v28_stop("The active thread settings do not match the frozen ",
                 "single-thread runtime contract.")
  }
  list(environment = stats::setNames(environment, variables),
       torch_threads = torch_threads)
}

.br_v28_runtime_binding <- function() {
  required <- c("torch", "pkgload", "digest", "withr", "haven")
  missing <- required[!vapply(required, requireNamespace, logical(1L),
                              quietly = TRUE)]
  if (length(missing)) {
    .br_v28_stop("Missing runtime package(s): ", paste(missing, collapse = ", "),
                 ".")
  }
  r_executable <- normalizePath(file.path(R.home("bin"), "R"),
                                winslash = "/", mustWork = TRUE)
  torch_native <- get("torch_version", envir = asNamespace("torch"),
                      inherits = FALSE)
  if (is.function(torch_native)) torch_native <- torch_native()
  thread_contract <- .br_v28_assert_thread_contract()
  list(
    R_version = R.version.string,
    R_platform = R.version$platform,
    R_executable_sha256 = .br_v28_sha256(r_executable),
    package_versions = stats::setNames(vapply(
      required, function(package) as.character(utils::packageVersion(package)),
      character(1L)), required),
    native_torch_version = as.character(torch_native),
    blas = unname(extSoftVersion()[["BLAS"]]),
    thread_contract = thread_contract,
    rng_kind = .br_v28_assert_rng_contract())
}

.br_v28_scalar_text <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .br_v28_stop(what, " must be one nonempty string.")
  }
  x
}

.br_v28_bool <- function(x) {
  out <- tolower(as.character(x))
  if (length(out) != 1L || is.na(out) || !out %in% c("true", "false")) {
    .br_v28_stop("Expected one literal true/false value.")
  }
  identical(out, "true")
}

.br_v28_sha256 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  shasum <- Sys.which("shasum")
  if (!nzchar(shasum)) .br_v28_stop("The `shasum` executable is required.")
  line <- system2(shasum, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    .br_v28_stop("Could not compute SHA-256 for ", path, ".")
  }
  sub("[[:space:]].*$", "", line)
}

.br_v28_object_sha256 <- function(object) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .br_v28_stop("The `digest` package is required.")
  }
  digest::digest(object, algo = "sha256", serialize = TRUE,
                 serializeVersion = 2L)
}

.br_v28_read_csv <- function(protocol_dir, name) {
  utils::read.csv(file.path(protocol_dir, name), stringsAsFactors = FALSE,
                  check.names = FALSE, na.strings = character())
}

.br_v28_read_contract <- function(protocol_dir) {
  x <- read.dcf(file.path(protocol_dir, "application-contract.dcf"), all = TRUE)
  if (nrow(x) != 1L) .br_v28_stop("The v2.8 contract must have one record.")
  unlist(x[1L, , drop = TRUE], use.names = TRUE)
}

.br_v28_assert_inside <- function(path, parent, what = "path") {
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
    .br_v28_stop(what, " is outside its permitted root.")
  }
  invisible(resolved)
}

.br_v28_run_dir <- function(repo_root) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  out <- file.path(repo_root, .br_v28_run_relative)
  .br_v28_assert_inside(out, repo_root, "The v2.8 run directory")
  out
}

.br_v28_v26_checkpoint_snapshot <- function(repo_root) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  prior_root <- file.path(repo_root, "runs", "br2017", "v26")
  empty <- data.frame(relative_path = character(), bytes = numeric(),
                      sha256 = character(), stringsAsFactors = FALSE)
  if (!dir.exists(prior_root)) return(empty)
  paths <- list.files(prior_root, all.files = TRUE, no.. = TRUE,
                      recursive = TRUE, full.names = TRUE)
  if (!length(paths)) return(empty)
  info <- file.info(paths)
  paths <- paths[!is.na(info$isdir) & !info$isdir]
  if (!length(paths)) return(empty)
  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  relative <- substring(paths, nchar(repo_root) + 2L)
  order_index <- order(relative, method = "radix")
  paths <- paths[order_index]
  relative <- relative[order_index]
  info <- file.info(paths)
  data.frame(
    relative_path = relative,
    bytes = as.numeric(info$size),
    sha256 = unname(vapply(paths, .br_v28_sha256, character(1L))),
    stringsAsFactors = FALSE)
}

.br_v28_assert_v26_checkpoint_snapshot <- function(repo_root, expected) {
  observed <- .br_v28_v26_checkpoint_snapshot(repo_root)
  if (!is.data.frame(expected) ||
      !identical(names(expected), names(observed)) ||
      !identical(expected, observed)) {
    .br_v28_stop(
      "The sealed v2.6 checkpoint tree changed after v2.8 preflight.")
  }
  invisible(TRUE)
}

.br_v28_git_snapshot <- function(repo_root, implementation_paths,
                                 require_clean = TRUE) {
  git <- Sys.which("git")
  if (!nzchar(git)) .br_v28_stop("Git is required for the v2.8 run.")
  implementation_paths <- as.character(implementation_paths)
  if (!length(implementation_paths) || anyNA(implementation_paths) ||
      any(!nzchar(implementation_paths)) ||
      anyDuplicated(implementation_paths)) {
    .br_v28_stop("The v2.8 implementation path registry is malformed.")
  }
  commit <- system2(git, c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
                    stdout = TRUE, stderr = TRUE)
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    .br_v28_stop("Could not identify the v2.8 Git commit.")
  }
  ## Target and fit execution load the package working tree.  Checking only
  ## the application wrapper would leave transitive numerical code mutable.
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--untracked-files=all"), stdout = TRUE,
                    stderr = TRUE)
  if (isTRUE(require_clean) && length(status)) {
    .br_v28_stop("The v2.8 protocol or implementation is uncommitted or modified: ",
                 paste(status, collapse = "; "))
  }
  listed <- system2(git, c("-C", shQuote(repo_root), "ls-files"),
                    stdout = TRUE, stderr = TRUE)
  protocol_files <- list.files(
    file.path(repo_root, "applications", "br2017", "protocol", "v28"),
    recursive = TRUE, full.names = FALSE, all.files = TRUE,
    no.. = TRUE)
  protocol_files <- file.path(
    "applications", "br2017", "protocol", "v28", protocol_files)
  loaded_r <- list.files(
    file.path(repo_root, "R"), pattern = "[.]R$", recursive = TRUE,
    full.names = FALSE)
  loaded_r <- file.path("R", loaded_r)
  manifested_r <- implementation_paths[startsWith(implementation_paths, "R/")]
  required <- unique(c(implementation_paths, protocol_files))
  if (isTRUE(require_clean) && !all(required %in% listed)) {
    .br_v28_stop("Every v2.8 implementation and protocol file must be tracked before outcomes.")
  }
  if (isTRUE(require_clean) && !setequal(loaded_r, manifested_r)) {
    .br_v28_stop(paste(
      "The v2.8 manifest must hash every package R file loaded by pkgload",
      "and no unregistered R file may be present."))
  }
  list(commit = commit, clean = !length(status), status = status,
       tracked = listed)
}

.br_v28_validate_protocol <- function(repo_root, source_root = NULL,
                                      require_external = TRUE,
                                      require_clean = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  protocol_dir <- file.path(repo_root, "applications", "br2017", "protocol",
                            "v28")
  contract <- .br_v28_read_contract(protocol_dir)
  if (!identical(unname(contract[["Protocol-Id"]]), .br_v28_protocol_id)) {
    .br_v28_stop("Unexpected Ballard--Rosa v2.8 protocol identifier.")
  }
  if (!identical(unname(contract[["Output-Directory"]]),
                 gsub("\\\\", "/", .br_v28_run_relative))) {
    .br_v28_stop("The canonical v2.8 run directory changed.")
  }
  if (!identical(unname(contract[["Polish-Max-Iterations"]]),
                 as.character(.br_v28_polish_max_iter)) ||
      !identical(unname(contract[["Amendment-Scope"]]),
                 "start_semantics_gradient_contract_and_riesz_numerics") ||
      !identical(unname(contract[["Polish-Stage-Max-Iterations"]]),
                 paste(.br_v28_polish_schedule$max_iter, collapse = ",")) ||
      !identical(unname(contract[["Polish-Stage-Learning-Rates"]]),
                 paste(.br_v28_polish_schedule$learning_rate,
                       collapse = ","))) {
    .br_v28_stop("The v2.8 prospective numerical contract changed.")
  }
  exact <- c(
    `Ranks` = "0,1,2",
    `Primary-Rank` = "1",
    `Carried-Forward-Model-Choices` =
      "v21_mean_specification_network_architecture_and_weight_decay",
    `Fresh-Optimization-Rule` =
      "all_trainable_parameters_reoptimized_from_initializer",
    `Q2-Timing-Decision` = paste0(
      "deterministic_completed_q0_q1_fit_seconds_plus_6.25_times_",
      "q2_sentinel_seconds"),
    `Q2-Timing-Cutoff-Seconds` = "50400",
    `Q2-Stop-Analysis-Rule` =
      "complete_q0_q1_comparison_and_record_q2_deferred",
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
    `Threshold-Change` = "none")
  observed_exact <- if (all(names(exact) %in% names(contract))) {
    vapply(contract[names(exact)], .br_v28_scalar_text, character(1L),
           what = "Each frozen v2.8 contract field")
  } else {
    character()
  }
  if (!identical(unname(observed_exact), unname(exact))) {
    .br_v28_stop("A fixed v2.8 threshold or statistical rule changed.")
  }

  checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
  if (!file.exists(checksum_path)) {
    .br_v28_stop("The v2.8 protocol checksum registry is absent.")
  }
  lines <- readLines(checksum_path, warn = FALSE)
  pieces <- strsplit(lines, "  ", fixed = TRUE)
  if (!length(lines) || any(lengths(pieces) != 2L)) {
    .br_v28_stop("Malformed v2.8 protocol checksum registry.")
  }
  registered <- vapply(pieces, `[[`, character(1L), 2L)
  expected_hash <- vapply(pieces, `[[`, character(1L), 1L)
  actual_protocol <- setdiff(list.files(
    protocol_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    "CHECKSUMS.sha256")
  if (anyDuplicated(registered) ||
      any(grepl("^/|(^|/)\\.\\.(/|$)", registered)) ||
      !setequal(registered, actual_protocol)) {
    .br_v28_stop("The v2.8 checksum registry does not cover exactly every ",
                 "protocol file.")
  }
  observed_hash <- vapply(registered, function(nm) {
    .br_v28_sha256(file.path(protocol_dir, nm))
  }, character(1L))
  if (!identical(unname(observed_hash), unname(expected_hash))) {
    .br_v28_stop("A v2.8 protocol file differs from CHECKSUMS.sha256.")
  }

  prior <- .br_v28_read_csv(protocol_dir, "prior-v26-manifest.csv")
  for (i in seq_len(nrow(prior))) {
    path <- file.path(repo_root, prior$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.br_v28_sha256(path), prior$sha256[[i]])) {
      .br_v28_stop("A sealed v2.6 file changed: ",
                   prior$relative_path[[i]], ".")
    }
  }

  carried <- .br_v28_read_csv(protocol_dir, "carry-forward-manifest.csv")
  for (i in seq_len(nrow(carried))) {
    path <- file.path(repo_root, carried$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.br_v28_sha256(path), carried$sha256[[i]])) {
      .br_v28_stop("Carried-forward registry mismatch: ",
                   carried$registry[[i]], ".")
    }
  }

  implementation <- .br_v28_read_csv(
    protocol_dir, "implementation-manifest.csv")
  implementation_paths <- implementation$relative_path
  if (anyDuplicated(implementation_paths) ||
      any(grepl("^/|(^|/)\\.\\.(/|$)", implementation_paths)) ||
      any(implementation$sha256 == "PENDING_FINALIZATION") ||
      !"applications/bin/Rscript45" %in% implementation_paths) {
    .br_v28_stop("The transitive v2.8 implementation manifest is unsafe or ",
                 "unfinished.")
  }
  for (i in seq_len(nrow(implementation))) {
    path <- file.path(repo_root, implementation$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.br_v28_sha256(path), implementation$sha256[[i]])) {
      .br_v28_stop("Transitive v2.8 implementation mismatch: ",
                   implementation$relative_path[[i]], ".")
    }
  }

  inputs <- .br_v28_read_csv(protocol_dir, "input-manifest.csv")
  if (isTRUE(require_external)) {
    source_root <- .br_v28_scalar_text(source_root, "source_root")
    source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)
    for (i in seq_len(nrow(inputs))) {
      if (!.br_v28_bool(inputs$required[[i]])) next
      path <- file.path(source_root,
                        inputs$relative_to_ConjointStructural[[i]])
      if (!file.exists(path) ||
          !identical(.br_v28_sha256(path), inputs$sha256[[i]])) {
        .br_v28_stop("External input hash mismatch: ", inputs$input_id[[i]], ".")
      }
    }
  }
  git <- .br_v28_git_snapshot(
    repo_root, implementation_paths,
    require_clean = require_clean)
  list(protocol_id = .br_v28_protocol_id, protocol_dir = protocol_dir,
       contract = contract, inputs = inputs, carried = carried, prior = prior,
       implementation = implementation,
       protocol_hash = .br_v28_sha256(checksum_path), git = git)
}

.br_v28_input_path <- function(protocol, source_root, input_id) {
  row <- protocol$inputs[protocol$inputs$input_id == input_id, , drop = FALSE]
  if (nrow(row) != 1L) .br_v28_stop("Unknown v2.8 input: ", input_id, ".")
  path <- file.path(source_root, row$relative_to_ConjointStructural[[1L]])
  if (!file.exists(path) || !identical(.br_v28_sha256(path), row$sha256[[1L]])) {
    .br_v28_stop("Input failed its v2.8 hash binding: ", input_id, ".")
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.br_v28_checkpoint_path <- function(run_dir, kind, q = NULL, scope = NULL) {
  kind <- match.arg(kind, c("preflight", "fit", "assembled", "refinement",
                            "timing-decision", "analysis"))
  if (kind == "preflight") return(file.path(run_dir, "preflight.rds"))
  if (kind == "timing-decision") return(file.path(run_dir, "q2-timing-decision.rds"))
  if (kind == "analysis") return(file.path(run_dir, "analysis.rds"))
  if (!q %in% .br_v28_ranks) .br_v28_stop("Invalid checkpoint rank.")
  if (kind == "fit") {
    if (!scope %in% .br_v28_scopes) .br_v28_stop("Invalid checkpoint scope.")
    return(file.path(run_dir, paste0("q", q), paste0(scope, ".rds")))
  }
  file.path(run_dir, paste0("q", q), paste0(kind, ".rds"))
}

.br_v28_save_atomic <- function(payload, path, binding) {
  if (!is.list(binding) || !identical(binding$protocol_id, .br_v28_protocol_id)) {
    .br_v28_stop("A checkpoint requires the exact v2.8 binding.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) || file.exists(sidecar)) {
    .br_v28_stop("Refusing to overwrite an existing v2.8 checkpoint: ", path)
  }
  token <- paste0(Sys.getpid(), "-", format(as.integer(Sys.time()),
                                             scientific = FALSE))
  tmp <- paste0(path, ".tmp-", token)
  tmp_sidecar <- paste0(sidecar, ".tmp-", token)
  on.exit(unlink(c(tmp, tmp_sidecar), force = TRUE), add = TRUE)
  envelope <- list(
    schema_version = "br2017-v2.8-atomic-checkpoint-v1",
    binding = binding,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    payload = payload)
  saveRDS(envelope, tmp, version = 3L)
  hash <- .br_v28_sha256(tmp)
  writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
  if (!file.rename(tmp, path)) .br_v28_stop("Could not atomically install ", path)
  if (!file.rename(tmp_sidecar, sidecar)) {
    .br_v28_stop("Checkpoint data installed but SHA sidecar failed: ", path)
  }
  invisible(list(path = path, sha256 = hash))
}

.br_v28_read_checkpoint <- function(path, binding = NULL,
                                    payload = TRUE) {
  sidecar <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(sidecar)) {
    .br_v28_stop("Incomplete v2.8 checkpoint: ", path)
  }
  expected <- strsplit(readLines(sidecar, warn = FALSE)[[1L]],
                       "[[:space:]]+")[[1L]][1L]
  observed <- .br_v28_sha256(path)
  if (!identical(expected, observed)) {
    .br_v28_stop("Checkpoint SHA mismatch: ", path)
  }
  object <- readRDS(path)
  if (!is.list(object) ||
      !identical(object$schema_version, "br2017-v2.8-atomic-checkpoint-v1") ||
      !identical(object$binding$protocol_id, .br_v28_protocol_id)) {
    .br_v28_stop("Malformed or foreign v2.8 checkpoint: ", path)
  }
  if (!is.null(binding)) {
    required <- names(binding)
    if (!all(required %in% names(object$binding)) ||
        !identical(object$binding[required], binding[required])) {
      .br_v28_stop("Checkpoint binding mismatch: ", path)
    }
  }
  if (!isTRUE(payload)) {
    return(list(path = path, sha256 = observed, binding = object$binding))
  }
  object
}

.br_v28_status <- function(repo_root) {
  run_dir <- .br_v28_run_dir(repo_root)
  paths <- c(
    preflight = .br_v28_checkpoint_path(run_dir, "preflight"),
    unlist(lapply(.br_v28_ranks, function(q) {
      stats::setNames(vapply(.br_v28_scopes, function(scope) {
        .br_v28_checkpoint_path(run_dir, "fit", q, scope)
      }, character(1L)), paste0("q", q, "_", .br_v28_scopes))
    })),
    q0_assembled = .br_v28_checkpoint_path(run_dir, "assembled", 0L),
    q1_assembled = .br_v28_checkpoint_path(run_dir, "assembled", 1L),
    q2_assembled = .br_v28_checkpoint_path(run_dir, "assembled", 2L),
    q1_refinement = .br_v28_checkpoint_path(run_dir, "refinement", 1L),
    q2_refinement = .br_v28_checkpoint_path(run_dir, "refinement", 2L),
    q2_timing = .br_v28_checkpoint_path(run_dir, "timing-decision"),
    analysis = .br_v28_checkpoint_path(run_dir, "analysis"))
  data.frame(
    checkpoint = names(paths), path = unname(paths),
    complete = vapply(paths, function(path) {
      isTRUE(tryCatch({ .br_v28_read_checkpoint(path, payload = FALSE); TRUE },
                      error = function(e) FALSE))
    }, logical(1L)), stringsAsFactors = FALSE)
}

.br_v28_scope_index <- function(scope) {
  match(scope, .br_v28_scopes) - 1L
}

.br_v28_seed <- function(q, scope, start = c("exact", "perturbed", "constant")) {
  start <- match.arg(start)
  as.integer(202609076 + 100000L * as.integer(q) +
               1000L * .br_v28_scope_index(scope) +
               match(start, c("exact", "perturbed", "constant")) - 1L)
}

.br_v28_legacy_scope <- function(protocol, source_root, scope) {
  if (!scope %in% .br_v28_scopes) .br_v28_stop("Unknown fit scope: ", scope)
  registry <- .br_v28_read_csv(protocol$protocol_dir, "state-registry.csv")
  row <- registry[registry$scope == scope, , drop = FALSE]
  if (nrow(row) != 1L) .br_v28_stop("Missing state registry row for ", scope)
  if (scope == "full") {
    container <- readRDS(.br_v28_input_path(protocol, source_root,
                                             "v21_primary_full"))
    fit <- container$refit
  } else {
    container <- readRDS(.br_v28_input_path(protocol, source_root,
                                             "v21_primary_nested"))
    k <- as.integer(sub("outer_", "", scope))
    fit <- container$tuning[[k]]$refit
  }
  if (!inherits(fit, "scmix_tuned_matrix_fit") ||
      !inherits(fit$network_state, "scmix_network_state")) {
    .br_v28_stop("The registered v2.1 initializer is not portable: ", scope)
  }
  state_hash <- sconjoint:::.scmix_stable_state_hash(
    fit$network_state$state_dict)
  ids <- sort(unique(as.character(fit$raw_data$respondent_id)))
  id_hash <- .br_v28_object_sha256(ids)
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
              row$source_analysis_signature[[1L]])
  if (!isTRUE(valid)) {
    .br_v28_stop("The v2.1 initializer differs from the frozen registry: ",
                 scope)
  }
  fit
}

.br_v28_occurrence <- function(id) {
  ave(seq_along(id), as.character(id), FUN = seq_along)
}

.br_v28_scope_rows <- function(prepared, fit) {
  full_key <- paste(as.character(prepared$respondent_id),
                    .br_v28_occurrence(prepared$respondent_id), sep = "\r")
  fit_id <- as.character(fit$raw_data$respondent_id)
  fit_key <- paste(fit_id, .br_v28_occurrence(fit_id), sep = "\r")
  rows <- match(fit_key, full_key)
  if (anyNA(rows) || anyDuplicated(rows) ||
      max(abs(fit$raw_data$deltaX - prepared$deltaX[rows, , drop = FALSE])) >
        1e-12) {
    .br_v28_stop("Could not map a historical training scope to prepared rows.")
  }
  rows
}

.br_v28_extract_work_luck <- function(raw_path, prepared) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    .br_v28_stop("The `haven` package is required for the work/luck repair.")
  }
  raw <- haven::read_dta(raw_path,
                         col_select = c("ID", "table", "plan", "saw_revenue",
                                        "work_vs_luck"))
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  keep <- as.numeric(raw$saw_revenue) == 1
  raw <- raw[!is.na(keep) & keep, , drop = FALSE]
  counts <- table(as.character(raw$ID))
  raw <- raw[as.character(raw$ID) %in% names(counts[counts == 16L]), ,
             drop = FALSE]
  raw$plan_num <- as.integer(as.numeric(raw$plan))
  raw <- raw[raw$plan_num == 1L, , drop = FALSE]
  raw <- raw[order(as.numeric(raw$ID), as.numeric(raw$table)), , drop = FALSE]
  raw_id <- sprintf("%05d", as.integer(as.numeric(raw$ID)))
  raw_task <- as.integer(as.numeric(raw$table))
  key <- paste(raw_id, raw_task, sep = "\r")
  prepared_key <- paste(as.character(prepared$respondent_id),
                        as.integer(prepared$task), sep = "\r")
  rows <- match(prepared_key, key)
  if (anyNA(rows) || anyDuplicated(key) || length(unique(raw_id)) != 2000L) {
    .br_v28_stop("Raw work/luck rows do not match the prepared application.")
  }
  code <- as.numeric(raw$work_vs_luck[rows])
  observed <- sort(unique(code[!is.na(code)]))
  if (!all(observed %in% c(1, 2, 3, 8, 9))) {
    .br_v28_stop("Unexpected raw work_vs_luck code.")
  }
  repaired <- code
  repaired[repaired %in% c(8, 9)] <- NA_real_
  invalid <- which(code %in% c(8, 9))
  invalid_respondents <- unique(as.character(prepared$respondent_id[invalid]))
  if (length(invalid) != 8L || length(invalid_respondents) != 1L) {
    .br_v28_stop(
      "The frozen work/luck repair must affect exactly one respondent and eight task rows.")
  }
  list(raw_code = code, repaired_raw = repaired,
       invalid_task_rows = invalid,
       invalid_respondents = invalid_respondents,
       observed_codes = observed,
       rule = "1/2/3 substantive; 8/9 missing")
}

.br_v28_prepare_input <- function(protocol, source_root) {
  prepared <- readRDS(.br_v28_input_path(protocol, source_root,
                                          "prepared_primary"))
  required_names <- c("deltaX", "y", "Z_primary", "respondent_id", "task")
  if (!is.list(prepared) || !all(required_names %in% names(prepared)) ||
      !identical(dim(prepared$deltaX), c(16000L, 7L)) ||
      !identical(dim(prepared$Z_primary), c(16000L, 23L)) ||
      length(unique(prepared$respondent_id)) != 2000L ||
      !all(table(prepared$respondent_id) == 8L) ||
      any(!prepared$y %in% 0:1)) {
    .br_v28_stop("The authoritative prepared input has an unexpected schema.")
  }
  raw_path <- .br_v28_input_path(protocol, source_root, "raw_fielded_data")
  work <- .br_v28_extract_work_luck(raw_path, prepared)
  out <- list(
    schema_version = "br2017-v2.8-repaired-input-v1",
    prepared = prepared,
    work_vs_luck = work,
    source_hashes = stats::setNames(protocol$inputs$sha256,
                                    protocol$inputs$input_id),
    repair_uses_outcome = FALSE)
  out
}

.br_v28_impute_scope_z <- function(preflight, rows) {
  prepared <- preflight$prepared
  rows <- as.integer(rows)
  rid <- as.character(prepared$respondent_id[rows])
  Z <- as.matrix(prepared$Z_primary[rows, , drop = FALSE])
  j <- match("work_vs_luck", colnames(Z))
  if (is.na(j)) .br_v28_stop("Prepared Z lacks work_vs_luck.")
  raw <- preflight$work_vs_luck$repaired_raw[rows]
  first <- !duplicated(rid)
  median_value <- stats::median(raw[first], na.rm = TRUE)
  if (!is.finite(median_value)) .br_v28_stop("No valid work/luck training code.")
  raw[is.na(raw)] <- median_value
  Z[, j] <- raw
  transform <- sconjoint:::.sc_fit_z_transform(Z, rid)
  internal <- sconjoint:::.sc_apply_z_transform(Z, transform)
  if (any(!is.finite(internal))) .br_v28_stop("Nonfinite repaired Z transform.")
  list(Z = Z, transform = transform, internal = internal,
       imputation_median = median_value, work_column = j)
}

.br_v28_affine_map <- function(x_new, x_old, eligible,
                               tolerance = 1e-10) {
  eligible <- as.logical(eligible)
  if (length(x_new) != length(x_old) || length(eligible) != length(x_new) ||
      sum(eligible) < 2L || any(!is.finite(x_new[eligible])) ||
      any(!is.finite(x_old[eligible]))) {
    .br_v28_stop("Malformed affine-remap inputs.")
  }
  xn <- x_new[eligible]
  xo <- x_old[eligible]
  denominator <- sum((xn - mean(xn))^2)
  if (!is.finite(denominator) || denominator <= 0) {
    .br_v28_stop("The repaired moderator has no valid variation.")
  }
  slope <- sum((xn - mean(xn)) * (xo - mean(xo))) / denominator
  intercept <- mean(xo) - slope * mean(xn)
  error <- max(abs(xo - (intercept + slope * xn)))
  if (!is.finite(error) || error > tolerance) {
    .br_v28_stop("The old and repaired valid moderator rows are not affine.")
  }
  list(intercept = intercept, slope = slope, max_error = error,
       n_eligible = sum(eligible))
}

.br_v28_canonical_coefficient_scale <- function(
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
    .br_v28_stop("The preprocessing and architecture coefficient-scale ",
                 "metadata do not share the exact attribute contract.")
  }
  discrepancy <- max(abs(prep_raw - arch_raw) /
                       pmax(abs(prep_raw), abs(arch_raw), 1))
  if (!is.finite(discrepancy) || discrepancy > tolerance) {
    .br_v28_stop("The preprocessing and architecture coefficient scales differ.")
  }
  stats::setNames(prep_raw, attribute_names)
}

.br_v28_tensor_array <- function(x) {
  if (inherits(x, "torch_tensor")) {
    return(torch::as_array(x$detach()$cpu()))
  }
  x
}

.br_v28_build_target_net <- function(state, q, coefficient_scale = NULL) {
  a <- state$architecture
  if (is.null(coefficient_scale)) coefficient_scale <- a$coefficient_scale
  sconjoint:::.sc_build_mixed_network(
    p = a$p, p_Z = a$p_Z, q = as.integer(q), hidden = a$hidden,
    mean_family = a$mean_family, mu_bound = a$mu_bound,
    kappa_bound = a$kappa_bound, alpha_bound = a$alpha_bound,
    a_bound = a$a_bound, weight_bound = a$weight_bound,
    coefficient_scale = coefficient_scale, compute_dtype = "float64")
}

.br_v28_arrays_to_warm <- function(arrays, target_net) {
  target <- target_net$state_dict()
  if (!identical(names(arrays), names(target))) {
    .br_v28_stop("Initializer fields do not match the target network.")
  }
  out <- lapply(names(target), function(nm) {
    value <- arrays[[nm]]
    if (!identical(as.integer(dim(value)), as.integer(target[[nm]]$shape))) {
      ## Vectors have NULL dim but a one-dimensional Torch shape.
      if (!(is.null(dim(value)) && length(target[[nm]]$shape) == 1L &&
            length(value) == as.integer(target[[nm]]$shape[[1L]]))) {
        .br_v28_stop("Initializer tensor shape mismatch for ", nm, ".")
      }
    }
    torch::torch_tensor(value, dtype = target[[nm]]$dtype, device = "cpu")
  })
  names(out) <- names(target)
  out
}

.br_v28_q1_initializer <- function(fit, preflight, prepared_rows,
                                   prediction_tolerance = 1e-6) {
  repaired <- .br_v28_impute_scope_z(preflight, prepared_rows)
  old_Z <- as.matrix(fit$raw_data$Z)
  old_internal <- sconjoint:::.sc_apply_z_transform(
    old_Z, fit$preprocessing$Z)
  j <- match("work_vs_luck", colnames(old_Z))
  invalid <- preflight$work_vs_luck$raw_code[prepared_rows] %in% c(8, 9)
  affine <- .br_v28_affine_map(
    repaired$internal[, j], old_internal[, j], !invalid)

  state <- fit$network_state
  original_arrays <- lapply(state$state_dict, .br_v28_tensor_array)
  arrays <- original_arrays
  first_weight <- "hidden.0.weight"
  first_bias <- "hidden.0.bias"
  if (!all(c(first_weight, first_bias) %in% names(arrays))) {
    .br_v28_stop("The selected q=1 state lacks its first ReLU layer.")
  }
  old_weight <- arrays[[first_weight]]
  arrays[[first_weight]][, j] <- old_weight[, j] * affine$slope
  arrays[[first_bias]] <- arrays[[first_bias]] +
    old_weight[, j] * affine$intercept
  before_net <- .br_v28_build_target_net(state, q = 1L)
  before_warm <- .br_v28_arrays_to_warm(original_arrays, before_net)
  before_net$load_state_dict(before_warm)
  before_net$eval()
  ## Corrected ReLU means are g(z)-g(0).  Mapping z_old=a+b*z_new
  ## preserves g(z), but changes which raw point is called zero.  Shift the
  ## unpenalized constant by g(a)-g(0) so the whole conditional mean, not just
  ## the hidden activation, remains invariant on valid rows.
  zero_old <- matrix(0, nrow = 1L, ncol = ncol(old_internal))
  mapped_new_zero <- zero_old
  mapped_new_zero[, j] <- affine$intercept
  alpha_shift <- as.numeric(
    sconjoint:::.sc_predict_beta(before_net, mapped_new_zero) -
      sconjoint:::.sc_predict_beta(before_net, zero_old))
  arrays$alpha_raw <- arrays$alpha_raw + alpha_shift
  target_net <- .br_v28_build_target_net(state, q = 1L)
  warm <- .br_v28_arrays_to_warm(arrays, target_net)
  target_net$load_state_dict(warm)
  target_net$eval()

  before_prediction <- sconjoint:::.sc_predict_beta(
    before_net, old_internal[!invalid, , drop = FALSE])
  new_prediction <- sconjoint:::.sc_predict_beta(
    target_net, repaired$internal[!invalid, , drop = FALSE])
  reproduction_error <- max(abs(before_prediction - new_prediction))
  if (!is.finite(reproduction_error) ||
      reproduction_error > prediction_tolerance) {
    .br_v28_stop("The work/luck affine state remap failed prediction invariance.")
  }
  old_net <- sconjoint::scmix_restore_network(state, device = "cpu")
  historical_prediction <- sconjoint:::.sc_predict_beta(
    old_net, old_internal[!invalid, , drop = FALSE])
  conversion_error <- max(abs(historical_prediction - before_prediction))
  list(warm = warm, repaired = repaired, affine = affine,
       alpha_shift = alpha_shift,
       valid_prediction_max_difference = reproduction_error,
       float32_to_float64_prediction_max_difference = conversion_error,
       intentionally_changed_rows = sum(invalid), source_state = state)
}

.br_v28_orthogonal_column <- function(a1_raw, ratio = 0.05) {
  a1_raw <- as.numeric(a1_raw)
  p <- length(a1_raw)
  if (!p || any(!is.finite(a1_raw)) || !is.finite(ratio) || ratio <= 0) {
    .br_v28_stop("Invalid q=2 loading initializer request.")
  }
  base <- sin(seq_len(p) * sqrt(2)) + cos(seq_len(p) * sqrt(3))
  denom <- sum(a1_raw^2)
  if (!is.finite(denom) || sqrt(denom) <= 1e-10) {
    .br_v28_stop("The q=1 raw loading norm is too small to define the frozen ",
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
    .br_v28_stop("The deterministic q=2 column is not orthogonal.")
  }
  out
}

.br_v28_rank_initializer <- function(q1_fit, q) {
  q <- as.integer(q)
  if (!q %in% c(0L, 2L) ||
      !inherits(q1_fit$network_state, "scmix_network_state") ||
      !identical(q1_fit$specification$q, 1L)) {
    .br_v28_stop("Rank sensitivity initialization requires a completed q=1 fit.")
  }
  state <- q1_fit$network_state
  target_net <- .br_v28_build_target_net(state, q)
  target <- target_net$state_dict()
  source <- lapply(state$state_dict, .br_v28_tensor_array)
  arrays <- lapply(target, .br_v28_tensor_array)
  common <- intersect(names(source), names(arrays))
  for (nm in setdiff(common, "A")) arrays[[nm]] <- source[[nm]]
  second_raw <- numeric()
  if (q == 2L) {
    scale <- as.numeric(state$architecture$coefficient_scale)
    A1 <- as.matrix(source$A)
    if (!identical(dim(A1), c(length(scale), 1L))) {
      .br_v28_stop("The q=1 loading initializer is malformed.")
    }
    a1_raw <- A1[, 1L] / scale
    second_raw <- .br_v28_orthogonal_column(a1_raw, ratio = 0.05)
    arrays$A <- cbind(A1[, 1L], scale * second_raw)
  }
  warm <- .br_v28_arrays_to_warm(arrays, target_net)
  target_net$load_state_dict(warm)
  list(warm = warm, second_loading_raw = second_raw,
       source_q1_state_hash = sconjoint:::.scmix_stable_state_hash(
         state$state_dict))
}

.br_v28_perturb_warm <- function(warm, seed, scale = 0.02) {
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

.br_v28_constant_warm <- function(warm) {
  out <- lapply(warm, function(x) x$clone())
  deviations <- grep("^(hidden\\.|param_layer\\.)", names(out), value = TRUE)
  torch::with_no_grad({
    for (nm in deviations) out[[nm]]$zero_()
  })
  out
}

.br_v28_grid <- function(q, refined = FALSE) {
  q <- as.integer(q)
  nodes <- if (q == 0L) 1L else if (q == 1L) {
    if (isTRUE(refined)) 45L else 31L
  } else if (q == 2L) {
    if (isTRUE(refined)) 15L else 9L
  } else .br_v28_stop("The prospective runner supports only q=0,1,2.")
  sconjoint:::.sc_mixed_grid(q = q, integration = "gh", n_nodes = nodes,
                             n_draws = 1L, seed = 1L,
                             antithetic = TRUE, scramble = FALSE)
}

.br_v28_fit_one_stage <- function(warm, dat, z_internal, scale, spec, q,
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

.br_v28_endpoint_warm <- function(fit) {
  if (!is.list(fit) || is.null(fit$net) ||
      !is.function(fit$net$state_dict)) {
    .br_v28_stop("A staged optimizer result lacks its returned network state.")
  }
  state <- fit$net$state_dict()
  lapply(state, function(x) x$detach()$cpu()$clone())
}

.br_v28_stage_ledger_row <- function(fit, stage, max_iter, learning_rate,
                                     cumulative_configured_max,
                                     cumulative_reported_iterations,
                                     input_state_hash,
                                     returned_state_hash) {
  actual <- as.integer(fit$polish_iterations)
  if (length(actual) != 1L ||
      (!is.na(actual) && (actual < 0L || actual > as.integer(max_iter)))) {
    .br_v28_stop("A staged L-BFGS call reported an invalid iteration count.")
  }
  accepted <- isTRUE(fit$polish$accepted) &&
    isTRUE(fit$lbfgs_completed) && isTRUE(fit$lbfgs_nonworsening)
  projection_free <- identical(fit$polish$projection_changed, FALSE)
  kkt_reached <- accepted && projection_free &&
    is.finite(fit$kkt_maximum) && fit$kkt_maximum <= .br_v28_kkt_stop
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

.br_v28_fit_one_start <- function(warm, dat, z_internal, scale, spec, q,
                                  seed, architecture, grid, verbose = FALSE) {
  current_warm <- warm
  ledger <- vector("list", nrow(.br_v28_polish_schedule))
  cumulative_configured <- 0L
  reported_iterations <- integer()
  fit <- NULL
  for (s in seq_len(nrow(.br_v28_polish_schedule))) {
    schedule <- .br_v28_polish_schedule[s, , drop = FALSE]
    input_state_hash <- sconjoint:::.scmix_stable_state_hash(lapply(
      current_warm, .br_v28_tensor_array))
    fit <- .br_v28_fit_one_stage(
      current_warm, dat, z_internal, scale, spec, q, seed, architecture,
      grid, schedule$max_iter[[1L]], schedule$learning_rate[[1L]], verbose)
    cumulative_configured <- cumulative_configured +
      schedule$max_iter[[1L]]
    reported_iterations <- c(reported_iterations,
                             as.integer(fit$polish_iterations))
    cumulative_reported <- if (anyNA(reported_iterations)) NA_integer_ else
      sum(reported_iterations)
    state_hash <- sconjoint:::.scmix_stable_state_hash(lapply(
      fit$net$state_dict(), .br_v28_tensor_array))
    ledger[[s]] <- .br_v28_stage_ledger_row(
      fit, schedule$stage[[1L]], schedule$max_iter[[1L]],
      schedule$learning_rate[[1L]], cumulative_configured,
      cumulative_reported, input_state_hash, state_hash)
    current_warm <- .br_v28_endpoint_warm(fit)
    if (isTRUE(ledger[[s]]$kkt_stop_reached[[1L]])) break
  }
  used <- !vapply(ledger, is.null, logical(1L))
  ledger <- do.call(rbind, ledger[used])
  rownames(ledger) <- NULL
  if (nrow(ledger) < 1L ||
      max(ledger$cumulative_configured_max) > .br_v28_polish_max_iter) {
    .br_v28_stop("The staged L-BFGS ledger violates its fixed effort cap.")
  }
  if (nrow(ledger) > 1L && !identical(
      ledger$input_state_hash[-1L],
      ledger$returned_state_hash[-nrow(ledger)])) {
    .br_v28_stop("A staged L-BFGS restart did not use the prior returned state.")
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

.br_v28_is_float64 <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    tolower(x) %in% c("float64", "torch_float64", "torch.float64", "double",
                      "torch_double", "torch.double")
}

.br_v28_gradient_audit <- function(fit, dat, repaired, scale, spec, grid,
                                   architecture, seed) {
  sconjoint:::.scmix_v28_gradient_contract_audit(
    net = fit$net, deltaX = dat$deltaX, y = dat$y,
    Z = repaired$internal, respondent_id = dat$respondent_id, grid = grid,
    weight_decay = spec$weight_decay, coefficient_scale = scale,
    kappa_bound = architecture$kappa_bound,
    alpha_bound = architecture$alpha_bound,
    a_bound = architecture$a_bound,
    weight_bound = architecture$weight_bound,
    steps = .br_v28_gradient_steps, seed = as.integer(seed),
    relative_tolerance = .br_v28_gradient_relative_tolerance,
    absolute_tolerance = .br_v28_gradient_absolute_tolerance,
    device = "cpu")
}

.br_v28_start_table <- function(starts, gradient_audits) {
  if (!is.list(starts) || length(starts) < 2L || is.null(names(starts)) ||
      any(!nzchar(names(starts))) ||
      !identical(names(starts), names(gradient_audits))) {
    .br_v28_stop("Start audit requires matching named fits and gradients.")
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

.br_v28_constant_gate <- function(x, gradient_audit = NULL) {
  is.list(x) &&
    all(is.finite(c(x$penalized_loss, x$final_loss, x$kkt_maximum))) &&
    .br_v28_is_float64(x$dtype) && isTRUE(x$lbfgs_completed) &&
    isTRUE(x$polish$accepted) && isTRUE(x$lbfgs_nonworsening) &&
    identical(x$polish$projection_changed, FALSE) &&
    isTRUE(x$bound_diagnostics_complete) && !isTRUE(x$bound_activity) &&
    is.list(gradient_audit) && isTRUE(gradient_audit$overall_pass) &&
    x$kkt_maximum <= 1e-5
}

.br_v28_capture_start_artifacts <- function(
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
      moderator_imputation = list(
        work_vs_luck = repaired$imputation_median),
      network_state = state,
      state_hash = sconjoint:::.scmix_stable_state_hash(state$state_dict),
      optimization = sconjoint:::.sc_comp_fit_summary(x),
      kkt_contract = sconjoint:::.scmix_v28_kkt_contract(
        x$kkt_returned, .br_v28_kkt_stop),
      gradient_contract = gradient_audits[[j]],
      stage_ledger = x$v28_stage_ledger)
    record$artifact_hash <- .br_v28_object_sha256(record)
    record
  })
  names(out) <- names(starts)
  structure(out,
            protocol = "br2017-v2.8-portable-start-artifacts-v1",
            class = c("br_v28_start_artifacts", "list"))
}

.br_v28_make_fit <- function(starts, constant_fit, dat, repaired,
                             spec, q, grid, scale, source, scope,
                             protocol_hash, git_commit) {
  architecture <- source$architecture
  canonical_scale <- .br_v28_canonical_coefficient_scale(
    source$preprocessing$deltaX$scale,
    architecture$coefficient_scale, colnames(dat$deltaX))
  if (!identical(as.numeric(scale), as.numeric(canonical_scale)) ||
      !identical(names(scale), names(canonical_scale))) {
    .br_v28_stop("The fit constructor did not receive the canonical ",
                 "coefficient scale used in optimization.")
  }
  signature <- sconjoint:::.sc_analysis_signature(
    dat$deltaX, dat$y, repaired$Z, dat$respondent_id,
    rep.int(1L, nrow(dat$deltaX)),
    list(protocol_id = .br_v28_protocol_id, q = q, scope = scope,
         mean_family = spec$mean_family, hidden = spec$hidden,
         weight_decay = spec$weight_decay, integration_nodes = nrow(grid$U)))
  fit_scope <- paste0("br2017 v2.8 q=", q, " ", scope,
                      " prospective numerical protocol")
  gradient_audits <- lapply(seq_along(starts), function(j) {
    .br_v28_gradient_audit(
      starts[[j]], dat, repaired, scale, spec, grid, architecture,
      .br_v28_seed(q, scope, names(starts)[[j]]))
  })
  names(gradient_audits) <- names(starts)
  start_input <- .br_v28_start_table(starts, gradient_audits)
  selection <- sconjoint:::.scmix_v28_start_surface(
    start_input, near_objective_cutoff = .br_v28_near_objective_cutoff,
    kkt_cutoff = .br_v28_kkt_stop)
  selected_index <- selection$selected_index
  selected <- starts[[selected_index]]
  selected_gradient <- gradient_audits[[selected_index]]
  constant_gradient <- .br_v28_gradient_audit(
    constant_fit, dat, repaired, scale, spec, grid, architecture,
    .br_v28_seed(q, scope, "constant"))

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
  constant_ok <- .br_v28_constant_gate(constant_fit, constant_gradient)
  selected_kkt <- sconjoint:::.scmix_v28_kkt_contract(
    selected$kkt_returned, .br_v28_kkt_stop)
  gates <- list(
    selected_and_constant_finite = all(is.finite(c(
      selected$penalized_loss, selected$final_loss,
      selected_kkt$maximum_block_rms, constant_fit$penalized_loss,
      constant_fit$final_loss))),
    float64 = .br_v28_is_float64(selected$dtype),
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
      l002_diff <= .br_v28_loading_invariance_cutoff)
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
  start_artifacts <- .br_v28_capture_start_artifacts(
    starts, gradient_audits, selection$surface, dat, repaired, spec, q,
    grid, architecture, scale, signature, fit_scope)
  fit <- list(
    net = selected$net, network_state = state,
    multistart_artifacts = start_artifacts,
    mu = sweep(sconjoint:::.sc_predict_beta(selected$net, repaired$internal),
               2L, scale, `/`),
    A = A, Sigma = tcrossprod(A), kappa = selected$kappa,
    specification = list(
      name = paste0("v28_same_mean_q", q), mean_family = spec$mean_family,
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
      polish_max_iterations = .br_v28_polish_max_iter,
      stage_max_iterations = .br_v28_polish_schedule$max_iter,
      stage_learning_rates = .br_v28_polish_schedule$learning_rate,
      optimizer_tolerance_grad = 0, optimizer_tolerance_change = 0,
      statistical_gates_changed = FALSE,
      amendment_scope = paste(
        "start eligibility, near-optimum semantics, portable start states,",
        "and independent blockwise gradient verification")),
    v28_source = source$provenance,
    v28_protocol_hash = protocol_hash, v28_git_commit = git_commit,
    v28_work_luck = list(
      imputation_median = repaired$imputation_median,
      valid_prediction_max_difference = source$prediction_difference %||% 0,
      intentionally_changed_rows = source$changed_rows %||% 0))
  fit <- sconjoint:::.scmix_add_loading_contract(
    fit, q = q, raw_full = A, optimization_full = selected$A,
    coefficient_scale_full = scale, invariance_evidence = loading,
    source = "Ballard--Rosa v2.8 prospective numerical protocol with paired raw and optimization scales")
  class(fit) <- c("scmix_tuned_matrix_fit", "list")
  sconjoint::scmix_portable_copy(fit)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
