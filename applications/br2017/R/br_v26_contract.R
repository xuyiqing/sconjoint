## Ballard--Rosa version 2.6 rapid continuation helpers.
##
## This file contains no top-level outcome-generating action.  The CLI in
## 06_run_fast_v26.R calls these functions only after the protocol and code
## have been committed.  Source artifacts are always read-only.

.br_v26_protocol_id <- "br2017-paperps-rapid-2026-09-07-v2.6-v1"
.br_v26_run_relative <- file.path(
  "runs", "br2017", "v26", "rapid-2026-09-07-v1")
.br_v26_scopes <- c("full", paste0("outer_", 1:5))
.br_v26_ranks <- 0:2

.br_v26_stop <- function(...) stop(..., call. = FALSE)

.br_v26_scalar_text <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .br_v26_stop(what, " must be one nonempty string.")
  }
  x
}

.br_v26_bool <- function(x) {
  out <- tolower(as.character(x))
  if (length(out) != 1L || is.na(out) || !out %in% c("true", "false")) {
    .br_v26_stop("Expected one literal true/false value.")
  }
  identical(out, "true")
}

.br_v26_sha256 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  shasum <- Sys.which("shasum")
  if (!nzchar(shasum)) .br_v26_stop("The `shasum` executable is required.")
  line <- system2(shasum, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    .br_v26_stop("Could not compute SHA-256 for ", path, ".")
  }
  sub("[[:space:]].*$", "", line)
}

.br_v26_object_sha256 <- function(object) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .br_v26_stop("The `digest` package is required.")
  }
  digest::digest(object, algo = "sha256", serialize = TRUE,
                 serializeVersion = 2L)
}

.br_v26_read_csv <- function(protocol_dir, name) {
  utils::read.csv(file.path(protocol_dir, name), stringsAsFactors = FALSE,
                  check.names = FALSE, na.strings = character())
}

.br_v26_read_contract <- function(protocol_dir) {
  x <- read.dcf(file.path(protocol_dir, "application-contract.dcf"), all = TRUE)
  if (nrow(x) != 1L) .br_v26_stop("The v2.6 contract must have one record.")
  x[1L, , drop = TRUE]
}

.br_v26_assert_inside <- function(path, parent, what = "path") {
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
    .br_v26_stop(what, " is outside its permitted root.")
  }
  invisible(resolved)
}

.br_v26_run_dir <- function(repo_root) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  out <- file.path(repo_root, .br_v26_run_relative)
  .br_v26_assert_inside(out, repo_root, "The v2.6 run directory")
  out
}

.br_v26_git_snapshot <- function(repo_root, require_clean = TRUE) {
  git <- Sys.which("git")
  if (!nzchar(git)) .br_v26_stop("Git is required for the v2.6 run.")
  tracked <- c("applications/br2017/protocol/v26",
               "applications/br2017/R/br_v26_contract.R",
               "applications/br2017/R/06_run_fast_v26.R",
               "applications/br2017/R/test_br_v26_contract.R")
  commit <- system2(git, c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
                    stdout = TRUE, stderr = TRUE)
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    .br_v26_stop("Could not identify the v2.6 Git commit.")
  }
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--", tracked), stdout = TRUE, stderr = TRUE)
  if (isTRUE(require_clean) && length(status)) {
    .br_v26_stop("The v2.6 protocol or implementation is uncommitted or modified: ",
                 paste(status, collapse = "; "))
  }
  listed <- system2(git, c("-C", shQuote(repo_root), "ls-files", "--", tracked),
                    stdout = TRUE, stderr = TRUE)
  required <- c("applications/br2017/R/br_v26_contract.R",
                "applications/br2017/R/06_run_fast_v26.R",
                "applications/br2017/R/test_br_v26_contract.R")
  if (isTRUE(require_clean) && !all(required %in% listed)) {
    .br_v26_stop("Every v2.6 implementation file must be tracked before outcomes.")
  }
  list(commit = commit, clean = !length(status), status = status,
       tracked = listed)
}

.br_v26_validate_protocol <- function(repo_root, source_root = NULL,
                                      require_external = TRUE,
                                      require_clean = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  protocol_dir <- file.path(repo_root, "applications", "br2017", "protocol",
                            "v26")
  contract <- .br_v26_read_contract(protocol_dir)
  if (!identical(unname(contract[["Protocol-Id"]]), .br_v26_protocol_id)) {
    .br_v26_stop("Unexpected Ballard--Rosa v2.6 protocol identifier.")
  }
  if (!identical(unname(contract[["Output-Directory"]]),
                 gsub("\\\\", "/", .br_v26_run_relative))) {
    .br_v26_stop("The canonical v2.6 run directory changed.")
  }

  checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
  if (!file.exists(checksum_path)) {
    .br_v26_stop("The v2.6 protocol checksum registry is absent.")
  }
  lines <- readLines(checksum_path, warn = FALSE)
  pieces <- strsplit(lines, "  ", fixed = TRUE)
  if (!length(lines) || any(lengths(pieces) != 2L)) {
    .br_v26_stop("Malformed v2.6 protocol checksum registry.")
  }
  registered <- vapply(pieces, `[[`, character(1L), 2L)
  expected_hash <- vapply(pieces, `[[`, character(1L), 1L)
  observed_hash <- vapply(registered, function(nm) {
    .br_v26_sha256(file.path(protocol_dir, nm))
  }, character(1L))
  if (!identical(unname(observed_hash), unname(expected_hash))) {
    .br_v26_stop("A v2.6 protocol file differs from CHECKSUMS.sha256.")
  }

  carried <- .br_v26_read_csv(protocol_dir, "carry-forward-manifest.csv")
  for (i in seq_len(nrow(carried))) {
    path <- file.path(repo_root, carried$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.br_v26_sha256(path), carried$sha256[[i]])) {
      .br_v26_stop("Carried-forward registry mismatch: ",
                   carried$registry[[i]], ".")
    }
  }

  inputs <- .br_v26_read_csv(protocol_dir, "input-manifest.csv")
  if (isTRUE(require_external)) {
    source_root <- .br_v26_scalar_text(source_root, "source_root")
    source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)
    for (i in seq_len(nrow(inputs))) {
      if (!.br_v26_bool(inputs$required[[i]])) next
      path <- file.path(source_root,
                        inputs$relative_to_ConjointStructural[[i]])
      if (!file.exists(path) ||
          !identical(.br_v26_sha256(path), inputs$sha256[[i]])) {
        .br_v26_stop("External input hash mismatch: ", inputs$input_id[[i]], ".")
      }
    }
  }
  git <- .br_v26_git_snapshot(repo_root, require_clean = require_clean)
  list(protocol_id = .br_v26_protocol_id, protocol_dir = protocol_dir,
       contract = contract, inputs = inputs, carried = carried,
       protocol_hash = .br_v26_sha256(checksum_path), git = git)
}

.br_v26_input_path <- function(protocol, source_root, input_id) {
  row <- protocol$inputs[protocol$inputs$input_id == input_id, , drop = FALSE]
  if (nrow(row) != 1L) .br_v26_stop("Unknown v2.6 input: ", input_id, ".")
  path <- file.path(source_root, row$relative_to_ConjointStructural[[1L]])
  if (!file.exists(path) || !identical(.br_v26_sha256(path), row$sha256[[1L]])) {
    .br_v26_stop("Input failed its v2.6 hash binding: ", input_id, ".")
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.br_v26_checkpoint_path <- function(run_dir, kind, q = NULL, scope = NULL) {
  kind <- match.arg(kind, c("preflight", "fit", "assembled", "refinement",
                            "timing-decision", "analysis"))
  if (kind == "preflight") return(file.path(run_dir, "preflight.rds"))
  if (kind == "timing-decision") return(file.path(run_dir, "q2-timing-decision.rds"))
  if (kind == "analysis") return(file.path(run_dir, "analysis.rds"))
  if (!q %in% .br_v26_ranks) .br_v26_stop("Invalid checkpoint rank.")
  if (kind == "fit") {
    if (!scope %in% .br_v26_scopes) .br_v26_stop("Invalid checkpoint scope.")
    return(file.path(run_dir, paste0("q", q), paste0(scope, ".rds")))
  }
  file.path(run_dir, paste0("q", q), paste0(kind, ".rds"))
}

.br_v26_save_atomic <- function(payload, path, binding) {
  if (!is.list(binding) || !identical(binding$protocol_id, .br_v26_protocol_id)) {
    .br_v26_stop("A checkpoint requires the exact v2.6 binding.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) || file.exists(sidecar)) {
    .br_v26_stop("Refusing to overwrite an existing v2.6 checkpoint: ", path)
  }
  token <- paste0(Sys.getpid(), "-", format(as.integer(Sys.time()),
                                             scientific = FALSE))
  tmp <- paste0(path, ".tmp-", token)
  tmp_sidecar <- paste0(sidecar, ".tmp-", token)
  on.exit(unlink(c(tmp, tmp_sidecar), force = TRUE), add = TRUE)
  envelope <- list(
    schema_version = "br2017-v2.6-atomic-checkpoint-v1",
    binding = binding,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    payload = payload)
  saveRDS(envelope, tmp, version = 3L)
  hash <- .br_v26_sha256(tmp)
  writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
  if (!file.rename(tmp, path)) .br_v26_stop("Could not atomically install ", path)
  if (!file.rename(tmp_sidecar, sidecar)) {
    .br_v26_stop("Checkpoint data installed but SHA sidecar failed: ", path)
  }
  invisible(list(path = path, sha256 = hash))
}

.br_v26_read_checkpoint <- function(path, binding = NULL,
                                    payload = TRUE) {
  sidecar <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(sidecar)) {
    .br_v26_stop("Incomplete v2.6 checkpoint: ", path)
  }
  expected <- strsplit(readLines(sidecar, warn = FALSE)[[1L]],
                       "[[:space:]]+")[[1L]][1L]
  observed <- .br_v26_sha256(path)
  if (!identical(expected, observed)) {
    .br_v26_stop("Checkpoint SHA mismatch: ", path)
  }
  if (!isTRUE(payload)) return(list(path = path, sha256 = observed))
  object <- readRDS(path)
  if (!is.list(object) ||
      !identical(object$schema_version, "br2017-v2.6-atomic-checkpoint-v1") ||
      !identical(object$binding$protocol_id, .br_v26_protocol_id)) {
    .br_v26_stop("Malformed or foreign v2.6 checkpoint: ", path)
  }
  if (!is.null(binding)) {
    required <- names(binding)
    if (!all(required %in% names(object$binding)) ||
        !identical(object$binding[required], binding[required])) {
      .br_v26_stop("Checkpoint binding mismatch: ", path)
    }
  }
  object
}

.br_v26_status <- function(repo_root) {
  run_dir <- .br_v26_run_dir(repo_root)
  paths <- c(
    preflight = .br_v26_checkpoint_path(run_dir, "preflight"),
    unlist(lapply(.br_v26_ranks, function(q) {
      stats::setNames(vapply(.br_v26_scopes, function(scope) {
        .br_v26_checkpoint_path(run_dir, "fit", q, scope)
      }, character(1L)), paste0("q", q, "_", .br_v26_scopes))
    })),
    q0_assembled = .br_v26_checkpoint_path(run_dir, "assembled", 0L),
    q1_assembled = .br_v26_checkpoint_path(run_dir, "assembled", 1L),
    q2_assembled = .br_v26_checkpoint_path(run_dir, "assembled", 2L),
    q1_refinement = .br_v26_checkpoint_path(run_dir, "refinement", 1L),
    q2_refinement = .br_v26_checkpoint_path(run_dir, "refinement", 2L),
    q2_timing = .br_v26_checkpoint_path(run_dir, "timing-decision"),
    analysis = .br_v26_checkpoint_path(run_dir, "analysis"))
  data.frame(
    checkpoint = names(paths), path = unname(paths),
    complete = vapply(paths, function(path) {
      isTRUE(tryCatch({ .br_v26_read_checkpoint(path, payload = FALSE); TRUE },
                      error = function(e) FALSE))
    }, logical(1L)), stringsAsFactors = FALSE)
}

.br_v26_scope_index <- function(scope) {
  match(scope, .br_v26_scopes) - 1L
}

.br_v26_seed <- function(q, scope, start = c("exact", "perturbed", "constant")) {
  start <- match.arg(start)
  as.integer(202609076 + 100000L * as.integer(q) +
               1000L * .br_v26_scope_index(scope) +
               match(start, c("exact", "perturbed", "constant")) - 1L)
}

.br_v26_legacy_scope <- function(protocol, source_root, scope) {
  if (!scope %in% .br_v26_scopes) .br_v26_stop("Unknown fit scope: ", scope)
  registry <- .br_v26_read_csv(protocol$protocol_dir, "state-registry.csv")
  row <- registry[registry$scope == scope, , drop = FALSE]
  if (nrow(row) != 1L) .br_v26_stop("Missing state registry row for ", scope)
  if (scope == "full") {
    container <- readRDS(.br_v26_input_path(protocol, source_root,
                                             "v21_primary_full"))
    fit <- container$refit
  } else {
    container <- readRDS(.br_v26_input_path(protocol, source_root,
                                             "v21_primary_nested"))
    k <- as.integer(sub("outer_", "", scope))
    fit <- container$tuning[[k]]$refit
  }
  if (!inherits(fit, "scmix_tuned_matrix_fit") ||
      !inherits(fit$network_state, "scmix_network_state")) {
    .br_v26_stop("The registered v2.1 initializer is not portable: ", scope)
  }
  state_hash <- sconjoint:::.scmix_stable_state_hash(
    fit$network_state$state_dict)
  ids <- sort(unique(as.character(fit$raw_data$respondent_id)))
  id_hash <- .br_v26_object_sha256(ids)
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
    .br_v26_stop("The v2.1 initializer differs from the frozen registry: ",
                 scope)
  }
  fit
}

.br_v26_occurrence <- function(id) {
  ave(seq_along(id), as.character(id), FUN = seq_along)
}

.br_v26_scope_rows <- function(prepared, fit) {
  full_key <- paste(as.character(prepared$respondent_id),
                    .br_v26_occurrence(prepared$respondent_id), sep = "\r")
  fit_id <- as.character(fit$raw_data$respondent_id)
  fit_key <- paste(fit_id, .br_v26_occurrence(fit_id), sep = "\r")
  rows <- match(fit_key, full_key)
  if (anyNA(rows) || anyDuplicated(rows) ||
      max(abs(fit$raw_data$deltaX - prepared$deltaX[rows, , drop = FALSE])) >
        1e-12) {
    .br_v26_stop("Could not map a historical training scope to prepared rows.")
  }
  rows
}

.br_v26_extract_work_luck <- function(raw_path, prepared) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    .br_v26_stop("The `haven` package is required for the work/luck repair.")
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
    .br_v26_stop("Raw work/luck rows do not match the prepared application.")
  }
  code <- as.numeric(raw$work_vs_luck[rows])
  observed <- sort(unique(code[!is.na(code)]))
  if (!all(observed %in% c(1, 2, 3, 8, 9))) {
    .br_v26_stop("Unexpected raw work_vs_luck code.")
  }
  repaired <- code
  repaired[repaired %in% c(8, 9)] <- NA_real_
  invalid <- which(code %in% c(8, 9))
  invalid_respondents <- unique(as.character(prepared$respondent_id[invalid]))
  if (length(invalid) != 8L || length(invalid_respondents) != 1L) {
    .br_v26_stop(
      "The frozen work/luck repair must affect exactly one respondent and eight task rows.")
  }
  list(raw_code = code, repaired_raw = repaired,
       invalid_task_rows = invalid,
       invalid_respondents = invalid_respondents,
       observed_codes = observed,
       rule = "1/2/3 substantive; 8/9 missing")
}

.br_v26_prepare_input <- function(protocol, source_root) {
  prepared <- readRDS(.br_v26_input_path(protocol, source_root,
                                          "prepared_primary"))
  required_names <- c("deltaX", "y", "Z_primary", "respondent_id", "task")
  if (!is.list(prepared) || !all(required_names %in% names(prepared)) ||
      !identical(dim(prepared$deltaX), c(16000L, 7L)) ||
      !identical(dim(prepared$Z_primary), c(16000L, 23L)) ||
      length(unique(prepared$respondent_id)) != 2000L ||
      !all(table(prepared$respondent_id) == 8L) ||
      any(!prepared$y %in% 0:1)) {
    .br_v26_stop("The authoritative prepared input has an unexpected schema.")
  }
  raw_path <- .br_v26_input_path(protocol, source_root, "raw_fielded_data")
  work <- .br_v26_extract_work_luck(raw_path, prepared)
  out <- list(
    schema_version = "br2017-v2.6-repaired-input-v1",
    prepared = prepared,
    work_vs_luck = work,
    source_hashes = stats::setNames(protocol$inputs$sha256,
                                    protocol$inputs$input_id),
    repair_uses_outcome = FALSE)
  out
}

.br_v26_impute_scope_z <- function(preflight, rows) {
  prepared <- preflight$prepared
  rows <- as.integer(rows)
  rid <- as.character(prepared$respondent_id[rows])
  Z <- as.matrix(prepared$Z_primary[rows, , drop = FALSE])
  j <- match("work_vs_luck", colnames(Z))
  if (is.na(j)) .br_v26_stop("Prepared Z lacks work_vs_luck.")
  raw <- preflight$work_vs_luck$repaired_raw[rows]
  first <- !duplicated(rid)
  median_value <- stats::median(raw[first], na.rm = TRUE)
  if (!is.finite(median_value)) .br_v26_stop("No valid work/luck training code.")
  raw[is.na(raw)] <- median_value
  Z[, j] <- raw
  transform <- sconjoint:::.sc_fit_z_transform(Z, rid)
  internal <- sconjoint:::.sc_apply_z_transform(Z, transform)
  if (any(!is.finite(internal))) .br_v26_stop("Nonfinite repaired Z transform.")
  list(Z = Z, transform = transform, internal = internal,
       imputation_median = median_value, work_column = j)
}

.br_v26_affine_map <- function(x_new, x_old, eligible,
                               tolerance = 1e-10) {
  eligible <- as.logical(eligible)
  if (length(x_new) != length(x_old) || length(eligible) != length(x_new) ||
      sum(eligible) < 2L || any(!is.finite(x_new[eligible])) ||
      any(!is.finite(x_old[eligible]))) {
    .br_v26_stop("Malformed affine-remap inputs.")
  }
  xn <- x_new[eligible]
  xo <- x_old[eligible]
  denominator <- sum((xn - mean(xn))^2)
  if (!is.finite(denominator) || denominator <= 0) {
    .br_v26_stop("The repaired moderator has no valid variation.")
  }
  slope <- sum((xn - mean(xn)) * (xo - mean(xo))) / denominator
  intercept <- mean(xo) - slope * mean(xn)
  error <- max(abs(xo - (intercept + slope * xn)))
  if (!is.finite(error) || error > tolerance) {
    .br_v26_stop("The old and repaired valid moderator rows are not affine.")
  }
  list(intercept = intercept, slope = slope, max_error = error,
       n_eligible = sum(eligible))
}

.br_v26_tensor_array <- function(x) {
  if (inherits(x, "torch_tensor")) {
    return(torch::as_array(x$detach()$cpu()))
  }
  x
}

.br_v26_build_target_net <- function(state, q, coefficient_scale = NULL) {
  a <- state$architecture
  if (is.null(coefficient_scale)) coefficient_scale <- a$coefficient_scale
  sconjoint:::.sc_build_mixed_network(
    p = a$p, p_Z = a$p_Z, q = as.integer(q), hidden = a$hidden,
    mean_family = a$mean_family, mu_bound = a$mu_bound,
    kappa_bound = a$kappa_bound, alpha_bound = a$alpha_bound,
    a_bound = a$a_bound, weight_bound = a$weight_bound,
    coefficient_scale = coefficient_scale, compute_dtype = "float64")
}

.br_v26_arrays_to_warm <- function(arrays, target_net) {
  target <- target_net$state_dict()
  if (!identical(names(arrays), names(target))) {
    .br_v26_stop("Initializer fields do not match the target network.")
  }
  out <- lapply(names(target), function(nm) {
    value <- arrays[[nm]]
    if (!identical(as.integer(dim(value)), as.integer(target[[nm]]$shape))) {
      ## Vectors have NULL dim but a one-dimensional Torch shape.
      if (!(is.null(dim(value)) && length(target[[nm]]$shape) == 1L &&
            length(value) == as.integer(target[[nm]]$shape[[1L]]))) {
        .br_v26_stop("Initializer tensor shape mismatch for ", nm, ".")
      }
    }
    torch::torch_tensor(value, dtype = target[[nm]]$dtype, device = "cpu")
  })
  names(out) <- names(target)
  out
}

.br_v26_q1_initializer <- function(fit, preflight, prepared_rows,
                                   prediction_tolerance = 1e-6) {
  repaired <- .br_v26_impute_scope_z(preflight, prepared_rows)
  old_Z <- as.matrix(fit$raw_data$Z)
  old_internal <- sconjoint:::.sc_apply_z_transform(
    old_Z, fit$preprocessing$Z)
  j <- match("work_vs_luck", colnames(old_Z))
  invalid <- preflight$work_vs_luck$raw_code[prepared_rows] %in% c(8, 9)
  affine <- .br_v26_affine_map(
    repaired$internal[, j], old_internal[, j], !invalid)

  state <- fit$network_state
  original_arrays <- lapply(state$state_dict, .br_v26_tensor_array)
  arrays <- original_arrays
  first_weight <- "hidden.0.weight"
  first_bias <- "hidden.0.bias"
  if (!all(c(first_weight, first_bias) %in% names(arrays))) {
    .br_v26_stop("The selected q=1 state lacks its first ReLU layer.")
  }
  old_weight <- arrays[[first_weight]]
  arrays[[first_weight]][, j] <- old_weight[, j] * affine$slope
  arrays[[first_bias]] <- arrays[[first_bias]] +
    old_weight[, j] * affine$intercept
  before_net <- .br_v26_build_target_net(state, q = 1L)
  before_warm <- .br_v26_arrays_to_warm(original_arrays, before_net)
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
  target_net <- .br_v26_build_target_net(state, q = 1L)
  warm <- .br_v26_arrays_to_warm(arrays, target_net)
  target_net$load_state_dict(warm)
  target_net$eval()

  before_prediction <- sconjoint:::.sc_predict_beta(
    before_net, old_internal[!invalid, , drop = FALSE])
  new_prediction <- sconjoint:::.sc_predict_beta(
    target_net, repaired$internal[!invalid, , drop = FALSE])
  reproduction_error <- max(abs(before_prediction - new_prediction))
  if (!is.finite(reproduction_error) ||
      reproduction_error > prediction_tolerance) {
    .br_v26_stop("The work/luck affine state remap failed prediction invariance.")
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

.br_v26_orthogonal_column <- function(a1_raw, ratio = 0.05) {
  a1_raw <- as.numeric(a1_raw)
  p <- length(a1_raw)
  if (!p || any(!is.finite(a1_raw)) || !is.finite(ratio) || ratio <= 0) {
    .br_v26_stop("Invalid q=2 loading initializer request.")
  }
  base <- sin(seq_len(p) * sqrt(2)) + cos(seq_len(p) * sqrt(3))
  denom <- sum(a1_raw^2)
  v <- if (denom > 1e-20) base - a1_raw * sum(base * a1_raw) / denom else base
  if (sum(v^2) < 1e-20) {
    basis <- rep(0, p)
    basis[which.min(abs(a1_raw))] <- 1
    v <- if (denom > 1e-20) basis - a1_raw * sum(basis * a1_raw) / denom else basis
  }
  v <- v / sqrt(sum(v^2))
  pivot <- which.max(abs(v))
  if (v[pivot] < 0) v <- -v
  magnitude <- ratio * max(sqrt(denom), 0.1)
  out <- magnitude * v
  if (denom > 1e-20 && abs(sum(out * a1_raw)) > 1e-12 *
      max(1, sqrt(sum(out^2) * denom))) {
    .br_v26_stop("The deterministic q=2 column is not orthogonal.")
  }
  out
}

.br_v26_rank_initializer <- function(q1_fit, q) {
  q <- as.integer(q)
  if (!q %in% c(0L, 2L) ||
      !inherits(q1_fit$network_state, "scmix_network_state") ||
      !identical(q1_fit$specification$q, 1L)) {
    .br_v26_stop("Rank sensitivity initialization requires a completed q=1 fit.")
  }
  state <- q1_fit$network_state
  target_net <- .br_v26_build_target_net(state, q)
  target <- target_net$state_dict()
  source <- lapply(state$state_dict, .br_v26_tensor_array)
  arrays <- lapply(target, .br_v26_tensor_array)
  common <- intersect(names(source), names(arrays))
  for (nm in setdiff(common, "A")) arrays[[nm]] <- source[[nm]]
  second_raw <- numeric()
  if (q == 2L) {
    scale <- as.numeric(state$architecture$coefficient_scale)
    A1 <- as.matrix(source$A)
    if (!identical(dim(A1), c(length(scale), 1L))) {
      .br_v26_stop("The q=1 loading initializer is malformed.")
    }
    a1_raw <- A1[, 1L] / scale
    second_raw <- .br_v26_orthogonal_column(a1_raw, ratio = 0.05)
    arrays$A <- cbind(A1[, 1L], scale * second_raw)
  }
  warm <- .br_v26_arrays_to_warm(arrays, target_net)
  target_net$load_state_dict(warm)
  list(warm = warm, second_loading_raw = second_raw,
       source_q1_state_hash = sconjoint:::.scmix_stable_state_hash(
         state$state_dict))
}

.br_v26_perturb_warm <- function(warm, seed, scale = 1e-4) {
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

.br_v26_constant_warm <- function(warm) {
  out <- lapply(warm, function(x) x$clone())
  deviations <- grep("^(hidden\\.|param_layer\\.)", names(out), value = TRUE)
  torch::with_no_grad({
    for (nm in deviations) out[[nm]]$zero_()
  })
  out
}

.br_v26_grid <- function(q, refined = FALSE) {
  q <- as.integer(q)
  nodes <- if (q == 0L) 1L else if (q == 1L) {
    if (isTRUE(refined)) 45L else 31L
  } else if (q == 2L) {
    if (isTRUE(refined)) 15L else 9L
  } else .br_v26_stop("The rapid runner supports only q=0,1,2.")
  sconjoint:::.sc_mixed_grid(q = q, integration = "gh", n_nodes = nodes,
                             n_draws = 1L, seed = 1L,
                             antithetic = TRUE, scramble = FALSE)
}

.br_v26_fit_one_start <- function(warm, dat, z_internal, scale, spec, q,
                                  seed, architecture, grid, verbose = FALSE) {
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
    polish_max_iter = 50L, polish_history_size = 50L,
    polish_tolerance_grad = 1e-10, polish_tolerance_change = 1e-12,
    polish_lr = 1, mu_bound = architecture$mu_bound,
    kappa_bound = architecture$kappa_bound,
    alpha_bound = architecture$alpha_bound,
    a_bound = architecture$a_bound, weight_bound = architecture$weight_bound,
    coefficient_scale = scale)
}

.br_v26_is_float64 <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    tolower(x) %in% c("float64", "torch_float64", "torch.float64", "double",
                      "torch_double", "torch.double")
}

.br_v26_select_start <- function(starts) {
  if (!is.list(starts) || length(starts) < 2L || is.null(names(starts)) ||
      any(!nzchar(names(starts)))) {
    .br_v26_stop("Start selection requires at least two named fits.")
  }
  objective <- vapply(starts, function(x) as.numeric(x$penalized_loss),
                      numeric(1L))
  if (any(!is.finite(objective))) {
    .br_v26_stop("Every start must have a finite penalized objective.")
  }
  unname(which.min(objective))
}

.br_v26_constant_gate <- function(x) {
  is.list(x) &&
    all(is.finite(c(x$penalized_loss, x$final_loss, x$kkt_maximum))) &&
    .br_v26_is_float64(x$dtype) && isTRUE(x$lbfgs_completed) &&
    isTRUE(x$lbfgs_nonworsening) &&
    isTRUE(x$bound_diagnostics_complete) && !isTRUE(x$bound_activity) &&
    x$kkt_maximum <= 1e-5
}

.br_v26_make_fit <- function(selected, starts, constant_fit, dat, repaired,
                             spec, q, grid, source, scope,
                             protocol_hash, git_commit) {
  architecture <- source$architecture
  scale <- as.numeric(source$architecture$coefficient_scale)
  signature <- sconjoint:::.sc_analysis_signature(
    dat$deltaX, dat$y, repaired$Z, dat$respondent_id,
    rep.int(1L, nrow(dat$deltaX)),
    list(protocol_id = .br_v26_protocol_id, q = q, scope = scope,
         mean_family = spec$mean_family, hidden = spec$hidden,
         weight_decay = spec$weight_decay, integration_nodes = nrow(grid$U)))
  fit_scope <- paste0("br2017 v2.6 q=", q, " ", scope,
                      " rapid continuation")
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
  penalized_objective <- vapply(starts, `[[`, numeric(1L), "penalized_loss")
  unpenalized_objective <- vapply(starts, `[[`, numeric(1L), "final_loss")
  relative <- (penalized_objective - min(penalized_objective)) /
    max(1, abs(min(penalized_objective)))
  start_audit <- data.frame(
    start = names(starts), penalized_mean_nll = penalized_objective,
    unpenalized_mean_nll = unpenalized_objective,
    relative_from_best = relative,
    kkt_maximum = vapply(starts, `[[`, numeric(1L), "kkt_maximum"),
    lbfgs_completed = vapply(starts, `[[`, logical(1L), "lbfgs_completed"),
    lbfgs_nonworsening = vapply(starts, `[[`, logical(1L),
                               "lbfgs_nonworsening"),
    bound_activity = vapply(starts, `[[`, logical(1L), "bound_activity"),
    stringsAsFactors = FALSE)
  selected_index <- .br_v26_select_start(starts)
  nested_relative <- (selected$penalized_loss - constant_fit$penalized_loss) /
    max(1, abs(constant_fit$penalized_loss))
  l002_diff <- loading$maximum_absolute_probability_difference %||% Inf
  constant_ok <- .br_v26_constant_gate(constant_fit)
  gates <- list(
    all_finite = all(is.finite(c(penalized_objective,
                                 unpenalized_objective,
                                 start_audit$kkt_maximum,
                                 selected$penalized_loss,
                                 constant_fit$penalized_loss))),
    float64 = .br_v26_is_float64(selected$dtype),
    lbfgs = isTRUE(selected$lbfgs_completed) &&
      isTRUE(selected$lbfgs_nonworsening),
    blockwise_kkt = is.finite(selected$kkt_maximum) &&
      selected$kkt_maximum <= 1e-5,
    bounds = isTRUE(selected$bound_diagnostics_complete) &&
      !isTRUE(selected$bound_activity),
    constant_reference = constant_ok,
    nested_objective = constant_ok && is.finite(nested_relative) &&
      nested_relative <= 1e-6,
    competing_start = is.finite(relative[setdiff(seq_along(relative),
                                                 selected_index)]) &&
      relative[setdiff(seq_along(relative), selected_index)] <= 1e-6,
    loading_invariance = is.finite(l002_diff) && l002_diff <= 1e-8)
  gates$pass <- all(unlist(gates, use.names = FALSE))
  optimization <- sconjoint:::.sc_comp_fit_summary(selected)
  optimization$nested_objective_gate <- list(
    pass = gates$nested_objective, relative_gap = nested_relative,
    tolerance = 1e-6, applicable = TRUE, v22_applicable = FALSE,
    v22_pass = FALSE)
  fit <- list(
    net = selected$net, network_state = state,
    multistart_artifacts = NULL,
    mu = sweep(sconjoint:::.sc_predict_beta(selected$net, repaired$internal),
               2L, scale, `/`),
    A = A, Sigma = tcrossprod(A), kappa = selected$kappa,
    specification = list(
      name = paste0("v26_same_mean_q", q), mean_family = spec$mean_family,
      hidden = as.integer(spec$hidden), weight_decay = spec$weight_decay,
      integration = "gh", n_nodes = as.integer(if (q == 0L) 1L else
        if (q == 1L) 31L else 9L), n_draws = 1L, q = as.integer(q)),
    integration_grid = grid,
    preprocessing = list(Z = repaired$transform,
                         deltaX = source$preprocessing$deltaX),
    optimization = optimization,
    pooled_prefit_optimization = NULL,
    continued_constant_optimization = sconjoint:::.sc_comp_fit_summary(
      constant_fit),
    analysis_signature = signature,
    raw_data = list(deltaX = dat$deltaX, y = dat$y, Z = repaired$Z,
                    respondent_id = dat$respondent_id),
    scope = fit_scope, protocol_mode = "calibration",
    calibration_only = TRUE, calibration_verification = NULL,
    v22_fit_computation_subset_pass = FALSE,
    v26_application_gate = gates,
    v26_start_audit = start_audit,
    v26_selected_start = names(starts)[selected_index],
    v26_start_selection_criterion = "minimum penalized training mean NLL",
    v26_constant_relative_gap = nested_relative,
    v26_source = source$provenance,
    v26_protocol_hash = protocol_hash, v26_git_commit = git_commit,
    v26_work_luck = list(
      imputation_median = repaired$imputation_median,
      valid_prediction_max_difference = source$prediction_difference %||% 0,
      intentionally_changed_rows = source$changed_rows %||% 0))
  fit <- sconjoint:::.scmix_add_loading_contract(
    fit, q = q, raw_full = A, optimization_full = selected$A,
    coefficient_scale_full = scale, invariance_evidence = loading,
    source = "Ballard--Rosa v2.6 rapid continuation with paired raw and optimization scales")
  class(fit) <- c("scmix_tuned_matrix_fit", "list")
  sconjoint::scmix_portable_copy(fit)
}

## The application-specific target builder supplies one complete registered
## family of analytic callbacks.  This adapter freezes the v2.6 Riesz rule and
## produces diagnostic one-step values while package release remains withheld.
.br_v26_run_dml_family <- function(fit, basis, targets, seed) {
  if (!inherits(fit, "scmix_nested_assembled") || !identical(fit$q, 1L)) {
    .br_v26_stop("The v2.6 DML adapter accepts only the assembled q=1 fit.")
  }
  if (!is.list(targets) || !length(targets) || is.null(names(targets)) ||
      any(!nzchar(names(targets)))) {
    .br_v26_stop("Supply one complete named target family.")
  }
  sconjoint::scmix_dml(
    fit = fit, targets = character(), plugin_targets = targets,
    mu_basis = basis, nu_grid = NULL,
    riesz_rule = "fixed_n_train_minus_three_quarters",
    riesz_numerical_rank_rule = "dimension_epsilon",
    riesz_penalty = "identity", riesz_validation_fraction = 0,
    active_eigenvalue_min = 1e-6, information_eigenvalue_min = 1e-8,
    rank_tolerance = 1e-8, riesz_equation_tolerance = 0.20,
    ridge_sensitivity_tolerance = 0.20,
    allow_numeric_derivatives = FALSE, verification = NULL,
    multiplier_draws = 9999L, multiplier = "normal", level = 0.95,
    seed = as.integer(seed))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
