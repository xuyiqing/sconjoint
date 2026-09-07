## Ballard--Rosa v2.7 target-stage contract and implementation helpers.
##
## This file has no top-level outcome-generating action.  It consumes only the
## immutable q=1 v2.7 assembly and preflight checkpoint.  It never reads q=0 or
## q=2 results.  The v2.5 target, contrast, group, family, and gate registries
## are used byte-for-byte through their prospective SHA-256 bindings.

.br_v27t_protocol_id <- "br2017-paperps-targets-2026-09-07-v2.7-v1"
.br_v27t_parent_protocol_id <- "br2017-paperps-rapid-2026-09-07-v2.7-v1"
.br_v27t_output_relative <- file.path(
  "runs", "br2017", "v27", "rapid-2026-09-07-v1", "targets")
.br_v27t_supported_status <- "draft_conditional_pending_confirmatory_validation"
.br_v27t_unavailable_status <- "unavailable_not_invented"
.br_v27t_family_draws <- 9999L
.br_v27t_level <- 0.95
.br_v27t_riesz_cutoff <- 0.20
.br_v27t_ridge_cutoff <- 0.20
.br_v27t_variance_floor <- 1e-6
.br_v27t_group_margin <- 0.05
.br_v27t_heterogeneity_margin <- 1e-4
.br_v27t_multiplier_block <- 250L

.br_v27t_stop <- function(...) stop(..., call. = FALSE)

.br_v27t_sha256 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  exe <- Sys.which("shasum")
  if (!nzchar(exe)) .br_v27t_stop("The `shasum` executable is required.")
  line <- system2(exe, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    .br_v27t_stop("Could not compute SHA-256 for ", path, ".")
  }
  sub("[[:space:]].*$", "", line)
}

.br_v27t_object_sha256 <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .br_v27t_stop("The `digest` package is required.")
  }
  digest::digest(x, algo = "sha256", serialize = TRUE,
                 serializeVersion = 2L)
}

.br_v27t_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = character())
}

.br_v27t_read_dcf <- function(path) {
  x <- read.dcf(path, all = TRUE)
  if (nrow(x) != 1L) .br_v27t_stop("Contract must contain exactly one row: ", path)
  unlist(x[1L, , drop = TRUE], use.names = TRUE)
}

.br_v27t_protocol_dir <- function(repo_root) {
  file.path(normalizePath(repo_root, winslash = "/", mustWork = TRUE),
            "applications", "br2017", "protocol", "v27-targets")
}

.br_v27t_output_dir <- function(repo_root) {
  file.path(normalizePath(repo_root, winslash = "/", mustWork = TRUE),
            .br_v27t_output_relative)
}

.br_v27t_git_snapshot <- function(repo_root, require_clean = TRUE) {
  git <- Sys.which("git")
  if (!nzchar(git)) .br_v27t_stop("Git is required for target execution.")
  tracked_root <- c(
    "applications/br2017/protocol/v27-targets",
    "applications/br2017/R/br_v27_targets_contract.R",
    "applications/br2017/R/08_run_targets_v27.R",
    "applications/br2017/R/test_br_v27_targets_contract.R")
  commit <- system2(git, c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
                    stdout = TRUE, stderr = TRUE)
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    .br_v27t_stop("Could not identify the target-stage Git commit.")
  }
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--", tracked_root), stdout = TRUE, stderr = TRUE)
  listed <- system2(git, c("-C", shQuote(repo_root), "ls-files", "--",
                           tracked_root), stdout = TRUE, stderr = TRUE)
  required <- c(
    "applications/br2017/R/br_v27_targets_contract.R",
    "applications/br2017/R/08_run_targets_v27.R",
    "applications/br2017/R/test_br_v27_targets_contract.R")
  if (isTRUE(require_clean) && length(status)) {
    .br_v27t_stop("Target protocol or implementation is uncommitted or modified: ",
                  paste(status, collapse = "; "))
  }
  if (isTRUE(require_clean) && !all(required %in% listed)) {
    .br_v27t_stop("Every target-stage implementation file must be tracked before outcomes.")
  }
  list(commit = commit, clean = !length(status), status = status,
       tracked = listed)
}

.br_v27t_validate_protocol <- function(repo_root, require_clean = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  protocol_dir <- .br_v27t_protocol_dir(repo_root)
  contract <- .br_v27t_read_dcf(file.path(protocol_dir,
                                           "target-stage-contract.dcf"))
  if (!identical(unname(contract[["Protocol-Id"]]), .br_v27t_protocol_id) ||
      !identical(unname(contract[["Parent-Protocol-Id"]]),
                 .br_v27t_parent_protocol_id) ||
      !identical(unname(contract[["Output-Directory"]]),
                 gsub("\\\\", "/", .br_v27t_output_relative))) {
    .br_v27t_stop("Unexpected target-stage identity or output location.")
  }
  exact <- c(
    `Target-Rank` = "1",
    `Riesz-Rule` = "fixed_n_train_minus_three_quarters",
    `Riesz-Numerical-Rank-Rule` = "dimension_epsilon",
    `Riesz-Defect-Cutoff` = "0.20",
    `Ridge-Movement-Cutoff` = "0.20",
    `Analytic-Derivatives-Only` = "true",
    `Common-Family-Multiplier-Draws` = "9999",
    `Formal-Release` = "withheld_pending_confirmatory_validation",
    `Q2-Target-Selection-Role` = "none")
  if (!all(names(exact) %in% names(contract)) ||
      !identical(unname(contract[names(exact)]), unname(exact))) {
    .br_v27t_stop("A frozen target-stage statistical rule changed.")
  }

  checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
  lines <- readLines(checksum_path, warn = FALSE)
  pieces <- strsplit(lines, "  ", fixed = TRUE)
  if (!length(lines) || any(lengths(pieces) != 2L)) {
    .br_v27t_stop("Malformed target-stage checksum registry.")
  }
  expected <- vapply(pieces, `[[`, character(1L), 1L)
  names(expected) <- vapply(pieces, `[[`, character(1L), 2L)
  observed <- vapply(names(expected), function(name) {
    .br_v27t_sha256(file.path(protocol_dir, name))
  }, character(1L))
  if (!identical(unname(observed), unname(expected))) {
    .br_v27t_stop("A target-stage protocol file differs from CHECKSUMS.sha256.")
  }

  frozen <- .br_v27t_read_csv(file.path(protocol_dir,
                                         "frozen-registry-manifest.csv"))
  for (i in seq_len(nrow(frozen))) {
    path <- file.path(repo_root, frozen$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.br_v27t_sha256(path), frozen$sha256[[i]])) {
      .br_v27t_stop("Frozen dependency hash mismatch: ",
                    frozen$relative_path[[i]], ".")
    }
  }
  implementation <- .br_v27t_read_csv(file.path(
    protocol_dir, "implementation-manifest.csv"))
  for (i in seq_len(nrow(implementation))) {
    path <- file.path(repo_root, implementation$relative_path[[i]])
    if (!file.exists(path) ||
        !identical(.br_v27t_sha256(path), implementation$sha256[[i]])) {
      .br_v27t_stop("Target implementation hash mismatch: ",
                    implementation$relative_path[[i]], ".")
    }
  }
  target_types <- .br_v27t_read_csv(file.path(protocol_dir,
                                               "target-type-registry.csv"))
  target_registry <- .br_v27t_read_csv(file.path(
    repo_root, "applications/br2017/protocol/v25/target-registry.csv"))
  family_registry <- .br_v27t_read_csv(file.path(
    repo_root, "applications/br2017/protocol/v25/family-registry.csv"))
  if (!setequal(unique(target_registry$target_type), target_types$target_type) ||
      anyDuplicated(target_types$target_type) || nrow(target_registry) != 72L ||
      !setequal(unique(target_registry$multiplicity_family),
                family_registry$family_id)) {
    .br_v27t_stop("Target-type coverage does not exactly cover the frozen registry.")
  }
  expected_supported <- target_types$status[
    match(target_registry$target_type, target_types$target_type)] == "supported"
  if (sum(expected_supported) != 66L || sum(!expected_supported) != 6L) {
    .br_v27t_stop("Expected 66 represented and six explicitly unavailable targets.")
  }
  seeds <- .br_v27t_read_csv(file.path(protocol_dir, "seed-registry.csv"))
  if (!identical(seeds$family_id, family_registry$family_id) ||
      anyDuplicated(c(seeds$dml_seed, seeds$multiplier_seed)) ||
      !all(seeds$multiplier_draws == .br_v27t_family_draws)) {
    .br_v27t_stop("Family seed registry is incomplete or changed.")
  }
  git <- .br_v27t_git_snapshot(repo_root, require_clean = require_clean)
  list(protocol_id = .br_v27t_protocol_id, protocol_dir = protocol_dir,
       protocol_hash = .br_v27t_sha256(checksum_path), contract = contract,
       frozen = frozen, implementation = implementation,
       target_types = target_types, targets = target_registry,
       families = family_registry, seeds = seeds, git = git)
}

.br_v27t_registries <- function(repo_root, protocol) {
  base <- file.path(repo_root, "applications", "br2017", "protocol", "v25")
  list(
    targets = protocol$targets,
    contrasts = .br_v27t_read_csv(file.path(base, "contrast-registry.csv")),
    groups = .br_v27t_read_csv(file.path(base, "group-registry.csv")),
    families = protocol$families,
    gates = .br_v27t_read_csv(file.path(base, "inference-gate-registry.csv")),
    target_types = protocol$target_types)
}

.br_v27t_binding <- function(protocol, kind, family_id = "all",
                             upstream = NULL) {
  out <- list(
    protocol_id = protocol$protocol_id,
    protocol_hash = protocol$protocol_hash,
    git_commit = protocol$git$commit,
    parent_protocol_id = .br_v27t_parent_protocol_id,
    application = "br2017", rank = 1L, kind = kind,
    family_id = family_id,
    q2_used_for_target_selection = FALSE)
  if (!is.null(upstream)) out$upstream <- upstream
  out
}

.br_v27t_checkpoint_path <- function(output_dir, kind, family_id = NULL) {
  if (identical(kind, "binding")) return(file.path(output_dir, "binding.rds"))
  if (identical(kind, "basis")) return(file.path(output_dir, "basis.rds"))
  if (identical(kind, "export")) return(file.path(output_dir, "export.rds"))
  if (identical(kind, "family")) {
    if (is.null(family_id) || !nzchar(family_id)) {
      .br_v27t_stop("A family checkpoint requires family_id.")
    }
    return(file.path(output_dir, "families", paste0(family_id, ".rds")))
  }
  .br_v27t_stop("Unknown target checkpoint kind: ", kind, ".")
}

.br_v27t_save_atomic <- function(payload, path, binding) {
  if (!is.list(binding) ||
      !identical(binding$protocol_id, .br_v27t_protocol_id)) {
    .br_v27t_stop("Target checkpoint requires the exact target-stage binding.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) || file.exists(sidecar)) {
    .br_v27t_stop("Refusing to overwrite immutable target checkpoint: ", path)
  }
  token <- paste0(Sys.getpid(), "-", as.integer(Sys.time()))
  tmp <- paste0(path, ".tmp-", token)
  tmp_sidecar <- paste0(sidecar, ".tmp-", token)
  on.exit(unlink(c(tmp, tmp_sidecar), force = TRUE), add = TRUE)
  envelope <- list(
    schema_version = "br2017-v2.7-target-atomic-checkpoint-v1",
    binding = binding,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    payload = payload)
  saveRDS(envelope, tmp, version = 3L)
  hash <- .br_v27t_sha256(tmp)
  writeLines(paste(hash, basename(path)), tmp_sidecar, useBytes = TRUE)
  if (!file.rename(tmp, path)) .br_v27t_stop("Could not install ", path, ".")
  if (!file.rename(tmp_sidecar, sidecar)) {
    .br_v27t_stop("Checkpoint installed but SHA sidecar failed: ", path)
  }
  invisible(list(path = path, sha256 = hash))
}

.br_v27t_read_checkpoint <- function(path, binding = NULL,
                                     payload = TRUE) {
  sidecar <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(sidecar)) {
    .br_v27t_stop("Incomplete target checkpoint: ", path)
  }
  expected <- strsplit(readLines(sidecar, warn = FALSE)[[1L]],
                       "[[:space:]]+")[[1L]][1L]
  observed <- .br_v27t_sha256(path)
  if (!identical(expected, observed)) {
    .br_v27t_stop("Target checkpoint SHA mismatch: ", path)
  }
  object <- readRDS(path)
  if (!is.list(object) ||
      !identical(object$schema_version,
                 "br2017-v2.7-target-atomic-checkpoint-v1") ||
      !identical(object$binding$protocol_id, .br_v27t_protocol_id)) {
    .br_v27t_stop("Malformed or foreign target checkpoint: ", path)
  }
  if (!is.null(binding)) {
    required <- names(binding)
    if (!all(required %in% names(object$binding)) ||
        !identical(object$binding[required], binding[required])) {
      .br_v27t_stop("Target checkpoint binding mismatch: ", path)
    }
  }
  if (!isTRUE(payload)) {
    return(list(path = path, sha256 = observed, binding = object$binding))
  }
  object
}

.br_v27t_git_is_ancestor <- function(repo_root, commit) {
  git <- Sys.which("git")
  if (!nzchar(git)) .br_v27t_stop("Git is required for provenance checks.")
  status <- system2(
    git, c("-C", shQuote(repo_root), "merge-base", "--is-ancestor",
           shQuote(commit), "HEAD"), stdout = FALSE, stderr = FALSE)
  identical(as.integer(status), 0L)
}

.br_v27t_assert_upstream_binding <- function(object, v27_protocol, kind,
                                             q = NA_integer_,
                                             scope = NA_character_) {
  expected <- list(
    protocol_id = v27_protocol$protocol_id,
    protocol_hash = v27_protocol$protocol_hash,
    application = "br2017", kind = kind, q = as.integer(q),
    scope = as.character(scope),
    input_hashes = stats::setNames(v27_protocol$inputs$sha256,
                                   v27_protocol$inputs$input_id))
  actual <- object$binding
  if (!is.list(actual) || !all(names(expected) %in% names(actual)) ||
      !identical(actual[names(expected)], expected) ||
      !is.character(actual$git_commit) || length(actual$git_commit) != 1L ||
      !grepl("^[0-9a-f]{40}$", actual$git_commit)) {
    .br_v27t_stop("Upstream v2.7 checkpoint binding mismatch for ", kind, ".")
  }
  actual$git_commit
}

.br_v27t_verify_upstream <- function(repo_root, v27_protocol) {
  run_dir <- .br_v27_run_dir(repo_root)
  assembled_path <- .br_v27_checkpoint_path(run_dir, "assembled", 1L)
  preflight_path <- .br_v27_checkpoint_path(run_dir, "preflight")
  ## Read with the parent's own sidecar/schema check first.  The checkpoint's
  ## run commit is intentionally allowed to precede the later target-stage
  ## commit.  Every other binding field is matched to the still-frozen parent
  ## protocol below, and both upstream checkpoints must name the same run
  ## commit.
  assembled <- .br_v27_read_checkpoint(assembled_path)
  preflight <- .br_v27_read_checkpoint(preflight_path)
  assembled_commit <- .br_v27t_assert_upstream_binding(
    assembled, v27_protocol, "assembled", 1L, "all")
  preflight_commit <- .br_v27t_assert_upstream_binding(
    preflight, v27_protocol, "preflight")
  if (!identical(assembled_commit, preflight_commit)) {
    .br_v27t_stop("Upstream assembly and preflight name different run commits.")
  }
  if (!.br_v27t_git_is_ancestor(repo_root, assembled_commit)) {
    .br_v27t_stop(
      "The upstream generating commit is not an ancestor of current HEAD.")
  }
  ap <- assembled$payload
  if (!is.list(ap) ||
      !identical(ap$schema_version, "br2017-v2.7-assembled-v1") ||
      !identical(ap$q, 1L) ||
      !inherits(ap$assembled, "scmix_nested_assembled") ||
      !identical(ap$assembled$q, 1L) ||
      !isTRUE(ap$no_historical_estimate_reuse) ||
      !identical(ap$inference_role,
        "draft_conditional_DML_pending_confirmatory_validation")) {
    .br_v27t_stop("The upstream q=1 assembly is not the frozen v2.7 object.")
  }
  pp <- preflight$payload
  if (!is.list(pp) ||
      !identical(pp$schema_version, "br2017-v2.7-repaired-input-v1") ||
      !isFALSE(pp$repair_uses_outcome)) {
    .br_v27t_stop("The upstream repaired-input checkpoint is malformed.")
  }
  list(
    assembled = ap, preflight = pp,
    hashes = list(
      q1_assembled_sha256 = .br_v27t_sha256(assembled_path),
      q1_assembled_sidecar_sha256 = .br_v27t_sha256(
        paste0(assembled_path, ".sha256")),
      preflight_sha256 = .br_v27t_sha256(preflight_path),
      preflight_sidecar_sha256 = .br_v27t_sha256(
        paste0(preflight_path, ".sha256")),
      parent_run_git_commit = assembled_commit),
    paths = list(q1_assembled = assembled_path, preflight = preflight_path))
}

.br_v27t_load_binding <- function(repo_root, protocol, v27_protocol) {
  output_dir <- .br_v27t_output_dir(repo_root)
  upstream <- .br_v27t_verify_upstream(repo_root, v27_protocol)
  path <- .br_v27t_checkpoint_path(output_dir, "binding")
  binding <- .br_v27t_binding(protocol, "binding", upstream = upstream$hashes)
  object <- .br_v27t_read_checkpoint(path, binding = binding)
  if (!identical(object$payload$upstream_hashes, upstream$hashes) ||
      !isFALSE(object$payload$q2_used_for_target_selection)) {
    .br_v27t_stop("Target binding checkpoint does not match the upstream hashes.")
  }
  list(checkpoint = object, upstream = upstream, binding = binding)
}

.br_v27t_contrast <- function(registries, id, attr_names) {
  row <- registries$contrasts[
    registries$contrasts$contrast_id == id, , drop = FALSE]
  if (nrow(row) != 1L || !all(attr_names %in% names(row))) {
    .br_v27t_stop("Unknown or malformed frozen contrast: ", id, ".")
  }
  out <- as.numeric(row[1L, attr_names, drop = TRUE])
  names(out) <- attr_names
  if (any(!is.finite(out))) .br_v27t_stop("Nonfinite contrast: ", id, ".")
  out
}

.br_v27t_groups <- function(preflight, respondent_id, registries) {
  meta <- preflight$prepared$respondent_meta
  id_name <- intersect(c("respondent_id", "respondent"), names(meta))
  pid_name <- intersect(c("pid7", "resp_pid7"), names(meta))
  if (length(id_name) != 1L || length(pid_name) != 1L) {
    .br_v27t_stop("Cannot recover frozen respondent party groups.")
  }
  meta <- meta[match(respondent_id, as.character(meta[[id_name]])), ,
               drop = FALSE]
  if (anyNA(meta[[pid_name]])) .br_v27t_stop("Party group metadata are missing.")
  pid <- as.numeric(meta[[pid_name]])
  out <- list(
    overall = stats::setNames(rep(1, length(pid)), respondent_id),
    democrat = stats::setNames(as.numeric(pid %in% 1:3), respondent_id),
    independent = stats::setNames(as.numeric(pid == 4), respondent_id),
    republican = stats::setNames(as.numeric(pid %in% 5:7), respondent_id),
    unclassified = stats::setNames(as.numeric(!pid %in% 1:7), respondent_id))
  required <- registries$groups$group_id
  if (!setequal(names(out), required)) {
    .br_v27t_stop("Implemented party groups do not match the frozen registry.")
  }
  out
}

.br_v27t_position_target <- function(label = "position_effect") {
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

.br_v27t_recipe <- function(target_id, target_type, primitive_names,
                            kind = "identity") {
  list(target_id = target_id, target_type = target_type,
       primitive_names = primitive_names, kind = kind)
}

.br_v27t_heterogeneity_registry <- function(rows) {
  ids <- rows$target_id
  primitive_by_target <- lapply(ids, function(id) {
    paste0(id, c(":H_Z", ":H_R"))
  })
  names(primitive_by_target) <- ids
  family_primitives <- unname(unlist(primitive_by_target, use.names = FALSE))
  lapply(ids, function(id) {
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
      confirmatory_status = "confirmatory",
      uses_numerical_integration = FALSE,
      reference_rule = "respondent_equal_finite_population_variance_N",
      influence_definition =
        "centered_direct_HZ_and_HR_respondent_sequence",
      primitive_estimands = c(
        "finite_population_variance_of_directional_conditional_mean",
        "directional_residual_variance"),
      primitive_orientations = c("nonnegative", "nonnegative"),
      primitive_units = rep("squared_utility_index", 2L),
      primitive_reference_rules = rep(
        "respondent_equal_finite_population_variance_N", 2L),
      primitive_influence_definitions = c(
        "direct_centered_squared_directional_mean_plus_Riesz_correction",
        "directional_residual_variance_plus_Riesz_correction"))
  })
}

.br_v27t_build_family <- function(family_id, fit, preflight, registries) {
  rows <- registries$targets[
    registries$targets$multiplicity_family == family_id, , drop = FALSE]
  rows <- rows[order(as.integer(rows$target_index)), , drop = FALSE]
  if (!nrow(rows)) .br_v27t_stop("Unknown frozen target family: ", family_id)
  type_status <- registries$target_types$status[
    match(rows$target_type, registries$target_types$target_type)]
  if (anyNA(type_status)) .br_v27t_stop("Unmapped target type in ", family_id)
  if (any(type_status == "unavailable")) {
    if (!all(type_status == "unavailable")) {
      .br_v27t_stop("A family cannot mix supported and unavailable targets.")
    }
    reason <- unique(registries$target_types$reason[
      match(rows$target_type, registries$target_types$target_type)])
    return(list(family_id = family_id, status = .br_v27t_unavailable_status,
                rows = rows, targets = list(), recipes = list(),
                reason = paste(reason, collapse = "; ")))
  }
  attr_names <- as.character(fit$attr_names)
  if (!identical(attr_names, c(
    "rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
    "rate_175_375", "rate_375P", "revenue_score"))) {
    .br_v27t_stop("Unexpected Ballard--Rosa coefficient order.")
  }
  respondents <- unique(as.character(fit$respondent_id))
  groups <- .br_v27t_groups(preflight, respondents, registries)
  targets <- recipes <- list()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    id <- row$target_id[[1L]]
    type <- row$target_type[[1L]]
    group_id <- row$group_id[[1L]]
    contrast <- if (row$contrast_id[[1L]] == "none") NULL else
      .br_v27t_contrast(registries, row$contrast_id[[1L]], attr_names)
    if (type == "position_effect") {
      targets[[id]] <- .br_v27t_position_target(id)
      recipes[[id]] <- .br_v27t_recipe(id, type, id)
    } else if (type == "mean") {
      targets[[id]] <- sconjoint::scmix_inference_target(
        type = "tau", contrast = contrast, label = id)
      recipes[[id]] <- .br_v27t_recipe(id, type, id)
    } else if (type == "subgroup_mean") {
      base <- sconjoint::scmix_inference_target(
        type = "tau", contrast = contrast, label = id)
      targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
      recipes[[id]] <- .br_v27t_recipe(
        id, type, paste0(id, c(":weighted_target",
                               ":subgroup_probability")), "subgroup_ratio")
    } else if (type %in% c("choice_probability",
                            "subgroup_choice_probability")) {
      base <- sconjoint::scmix_inference_target(
        type = "choice", contrast = contrast, position_neutral = TRUE,
        n_nodes = 31L, label = id)
      if (type == "choice_probability") {
        targets[[id]] <- base
        recipes[[id]] <- .br_v27t_recipe(id, type, id)
      } else {
        targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
        recipes[[id]] <- .br_v27t_recipe(
          id, type, paste0(id, c(":weighted_target",
                                 ":subgroup_probability")), "subgroup_ratio")
      }
    } else if (type %in% c("directional_share",
                            "subgroup_directional_share")) {
      base <- sconjoint::scmix_inference_target(
        type = "sign", contrast = contrast,
        variance_floor = .br_v27t_variance_floor, label = id)
      if (type == "directional_share") {
        targets[[id]] <- base
        recipes[[id]] <- .br_v27t_recipe(id, type, id)
      } else {
        targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
        recipes[[id]] <- .br_v27t_recipe(
          id, type, paste0(id, c(":weighted_target",
                                 ":subgroup_probability")), "subgroup_ratio")
      }
    } else if (type %in% c("compensating_benefit_share",
                            "subgroup_compensating_benefit_share")) {
      benefit <- .br_v27t_contrast(
        registries, row$benefit_contrast_id[[1L]], attr_names)
      amount <- as.numeric(row$benefit_amount[[1L]])
      if (!is.finite(amount)) .br_v27t_stop("Invalid benefit amount for ", id)
      base <- sconjoint::scmix_inference_target(
        type = "compensating", penalty = contrast, benefit = benefit,
        amount = amount, variance_floor = .br_v27t_variance_floor,
        label = id)
      if (type == "compensating_benefit_share") {
        targets[[id]] <- base
        recipes[[id]] <- .br_v27t_recipe(id, type, id)
      } else {
        targets[[id]] <- tq1_subgroup_target(base, groups[[group_id]])
        recipes[[id]] <- .br_v27t_recipe(
          id, type, paste0(id, c(":weighted_target",
                                 ":subgroup_probability")), "subgroup_ratio")
      }
    } else if (type == "directional_heterogeneity") {
      targets[[id]] <- sconjoint::scmix_inference_target(
        type = "heterogeneity_primitives", contrast = contrast, label = id)
      recipes[[id]] <- .br_v27t_recipe(
        id, type, paste0(id, c(":H_Z", ":H_R")),
        "heterogeneity_share")
    } else if (type == "mean_mrs") {
      denominator <- .br_v27t_contrast(
        registries, row$benefit_contrast_id[[1L]], attr_names)
      targets[[id]] <- sconjoint::scmix_inference_target(
        type = "mrs_primitives", numerator = contrast,
        denominator = denominator, label = id)
      recipes[[id]] <- .br_v27t_recipe(
        id, type, paste0(id, c(":numerator", ":denominator")), "mrs")
    } else {
      .br_v27t_stop("Supported target type lacks a builder: ", type, ".")
    }
  }
  primitive_registry <- if (all(rows$target_type ==
                                 "directional_heterogeneity")) {
    .br_v27t_heterogeneity_registry(rows)
  } else NULL
  list(family_id = family_id, status = "supported", rows = rows,
       targets = targets, recipes = recipes,
       primitive_registry = primitive_registry, reason = "")
}

.br_v27t_geometry <- function(theta, kind) {
  theta <- as.numeric(theta)
  if (kind == "identity") {
    return(list(value = theta[[1L]], gradient = 1,
                supporting = stats::setNames(theta, "scalar"), gate = TRUE,
                gate_reason = "not_applicable"))
  }
  if (kind == "subgroup_ratio") {
    a <- theta[[1L]]; b <- theta[[2L]]
    valid <- is.finite(b) && b >= .br_v27t_group_margin
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
    valid <- is.finite(total) && total >= .br_v27t_heterogeneity_margin &&
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
  .br_v27t_stop("Unknown target geometry: ", kind, ".")
}

.br_v27t_run_dml_family <- function(fit, basis, targets, seed,
                                    primitive_registry = NULL) {
  if (!inherits(fit, "scmix_nested_assembled") || !identical(fit$q, 1L)) {
    .br_v27t_stop("The v2.7 target adapter accepts only assembled q=1 fits.")
  }
  if (!is.list(targets) || !length(targets) || is.null(names(targets)) ||
      any(!nzchar(names(targets)))) {
    .br_v27t_stop("Supply one complete named target family.")
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
    allow_numeric_derivatives = FALSE, v22_registry = primitive_registry,
    verification = NULL, multiplier_draws = 9999L,
    multiplier = "normal", level = 0.95, seed = as.integer(seed))
}

.br_v27t_fold_metric <- function(dml, field, primitives, fun = max) {
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

.br_v27t_transform_dml <- function(dml, recipes, rows,
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
        riesz_max = NA_real_, ridge_max = NA_real_,
        derivative_error_max = NA_real_, null_space_defect_max = NA_real_,
        fit_gate_pass = fit_gate_pass,
        primitive_diagnostic_gate_pass = FALSE,
        rapid_available_gate_pass = FALSE,
        target_gate_pass = FALSE,
        formal_release = FALSE, status = "withheld_dml_unavailable",
        reason = if (is.null(dml$reason)) "DML primitives unavailable" else
          dml$reason, stringsAsFactors = FALSE),
      influence = NULL, plugin = NULL))
  }
  N <- nrow(dml$influence)
  target_if <- matrix(NA_real_, N, length(ids),
                      dimnames = list(dml$respondent_id, ids))
  estimate <- plugin <- se <- riesz <- ridge <- derivative <- null_defect <-
    rep(NA_real_, length(ids))
  quantity_gate <- rep(FALSE, length(ids))
  gate_reason <- rep("", length(ids))
  supporting <- vector("list", length(ids)); names(supporting) <- ids
  for (j in seq_along(ids)) {
    recipe <- recipes[[j]]; primitives <- recipe$primitive_names
    geo <- .br_v27t_geometry(dml$estimate[primitives], recipe$kind)
    pgeo <- .br_v27t_geometry(dml$plugin_estimate[primitives], recipe$kind)
    estimate[j] <- geo$value; plugin[j] <- pgeo$value
    supporting[[j]] <- geo$supporting
    if (all(is.finite(geo$gradient))) {
      target_if[, j] <- as.matrix(dml$influence[, primitives, drop = FALSE]) %*%
        geo$gradient
      target_if[, j] <- target_if[, j] - mean(target_if[, j])
      se[j] <- sqrt(mean(target_if[, j]^2) / N)
    }
    riesz_fold <- .br_v27t_fold_metric(
      dml, "honest_information_scaled_riesz_defect", primitives, max)
    if (all(is.na(riesz_fold))) {
      riesz_fold <- .br_v27t_fold_metric(
        dml, "riesz_equation_relative_residual", primitives, max)
    }
    ridge_fold <- .br_v27t_fold_metric(
      dml, "ridge_relative_sensitivity", primitives, max)
    riesz[j] <- if (all(is.finite(riesz_fold))) max(riesz_fold) else NA_real_
    ridge[j] <- if (all(is.finite(ridge_fold))) max(ridge_fold) else NA_real_
    derivative[j] <- max(dml$derivative_refinement_error[, primitives,
                                                        drop = FALSE],
                         na.rm = TRUE)
    nd <- dml$honest_riesz_null_space_defect_max[primitives]
    null_defect[j] <- if (length(nd) && all(is.finite(nd))) max(nd) else NA_real_
    quantity_gate[j] <- isTRUE(geo$gate)
    gate_reason[j] <- geo$gate_reason
  }
  names(estimate) <- names(plugin) <- names(se) <- names(riesz) <-
    names(ridge) <- names(derivative) <- names(null_defect) <- ids
  primitive_diagnostic_gate <- is.finite(estimate) & is.finite(se) & se > 0 &
    is.finite(riesz) & riesz <= .br_v27t_riesz_cutoff &
    is.finite(ridge) & ridge <= .br_v27t_ridge_cutoff &
    is.finite(derivative) & derivative <= 1e-4 &
    is.finite(null_defect) & null_defect <= 1e-6 & quantity_gate
  z <- stats::qnorm(1 - (1 - .br_v27t_level) / 2)
  status <- rep("diagnostic_computed_formal_withheld", length(ids))
  reason <- paste0(
    "formal_release_withheld; fit_gate_pass=", fit_gate_pass,
    "; primitive_diagnostic_gate_pass=", primitive_diagnostic_gate,
    "; quantity_domain_check=", quantity_gate, " (", gate_reason, "); ",
    "target-level ridge-neighborhood, regularity-z, transformed-Riesz, ",
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
      riesz_max = riesz, ridge_max = ridge,
      derivative_error_max = derivative,
      null_space_defect_max = null_defect,
      fit_gate_pass = fit_gate_pass,
      primitive_diagnostic_gate_pass = primitive_diagnostic_gate,
      rapid_available_gate_pass = FALSE,
      target_gate_pass = FALSE,
      formal_release = FALSE, status = status, reason = reason,
      stringsAsFactors = FALSE),
    influence = target_if, plugin = plugin, supporting = supporting)
}

.br_v27t_family_multiplier <- function(influence, estimate, seed,
                                       draws = .br_v27t_family_draws,
                                       level = .br_v27t_level,
                                       block_size = .br_v27t_multiplier_block) {
  influence <- as.matrix(influence)
  estimate <- as.numeric(estimate); names(estimate) <- colnames(influence)
  if (!nrow(influence) || !ncol(influence) || length(estimate) != ncol(influence) ||
      any(!is.finite(influence)) || any(!is.finite(estimate))) {
    .br_v27t_stop("Family multiplier requires finite target influence columns.")
  }
  N <- nrow(influence); J <- ncol(influence)
  se <- sqrt(colMeans(influence^2) / N)
  valid <- is.finite(se) & se > 0
  out <- matrix(NA_real_, draws, J, dimnames = list(NULL, colnames(influence)))
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
    centered_draw_hash = .br_v27t_object_sha256(out),
    centered_draws_persisted = FALSE,
    formal_release = FALSE,
    label = .br_v27t_supported_status)
}

.br_v27t_finalize_family <- function(dml, bundle, fit_gate_pass,
                                     multiplier_seed) {
  transformed <- .br_v27t_transform_dml(
    dml, bundle$recipes, bundle$rows, fit_gate_pass)
  multiplier <- NULL
  if (!is.null(transformed$influence) &&
      all(is.finite(transformed$influence)) &&
      all(is.finite(transformed$summary$estimate))) {
    multiplier <- .br_v27t_family_multiplier(
      transformed$influence, transformed$summary$estimate, multiplier_seed)
    transformed$summary$draft_simultaneous_lower <-
      multiplier$simultaneous_lower[transformed$summary$target_id]
    transformed$summary$draft_simultaneous_upper <-
      multiplier$simultaneous_upper[transformed$summary$target_id]
  }
  list(
    schema_version = "br2017-v2.7-target-family-v1",
    family_id = bundle$family_id,
    status = if (all(transformed$summary$status ==
                     .br_v27t_supported_status)) {
      .br_v27t_supported_status
    } else "withheld_one_or_more_family_members",
    complete_frozen_family = identical(
      transformed$summary$target_id, bundle$rows$target_id),
    dml = dml, summary = transformed$summary,
    supporting = transformed$supporting,
    multiplier = multiplier,
    formal_release = FALSE,
    unavailable_formal_gates = c(
      "confirmatory_calibration", "target_level_GH45_integration",
      "target_near_optimum_spread", "target_level_ridge_neighborhood",
      "target_level_regularity_z", "transformed_target_riesz",
      "range_respecting_nonlinear_confidence_set"),
    q2_used_for_target_selection = FALSE)
}

.br_v27t_unavailable_family <- function(bundle) {
  rows <- bundle$rows
  list(
    schema_version = "br2017-v2.7-target-family-v1",
    family_id = bundle$family_id, status = .br_v27t_unavailable_status,
    complete_frozen_family = TRUE, dml = NULL,
    summary = data.frame(
      target_index = as.integer(rows$target_index), target_id = rows$target_id,
      target_type = rows$target_type, estimate = NA_real_,
      crossfit_plugin = NA_real_,
      diagnostic_se = NA_real_, draft_pointwise_lower = NA_real_,
      draft_pointwise_upper = NA_real_, draft_simultaneous_lower = NA_real_,
      draft_simultaneous_upper = NA_real_, riesz_max = NA_real_,
      ridge_max = NA_real_, derivative_error_max = NA_real_,
      null_space_defect_max = NA_real_, fit_gate_pass = NA,
      primitive_diagnostic_gate_pass = FALSE,
      rapid_available_gate_pass = FALSE, target_gate_pass = FALSE,
      formal_release = FALSE,
      status = .br_v27t_unavailable_status, reason = bundle$reason,
      stringsAsFactors = FALSE),
    supporting = NULL, multiplier = NULL, formal_release = FALSE,
    unavailable_formal_gates = "target_not_represented",
    q2_used_for_target_selection = FALSE)
}

.br_v27t_status <- function(repo_root, protocol) {
  output_dir <- .br_v27t_output_dir(repo_root)
  paths <- c(
    binding = .br_v27t_checkpoint_path(output_dir, "binding"),
    basis = .br_v27t_checkpoint_path(output_dir, "basis"),
    stats::setNames(vapply(protocol$families$family_id, function(id) {
      .br_v27t_checkpoint_path(output_dir, "family", id)
    }, character(1L)), paste0("family_", protocol$families$family_id)),
    export = .br_v27t_checkpoint_path(output_dir, "export"))
  data.frame(
    checkpoint = names(paths), path = unname(paths),
    complete = vapply(paths, function(path) isTRUE(tryCatch({
      .br_v27t_read_checkpoint(path, payload = FALSE); TRUE
    }, error = function(e) FALSE)), logical(1L)),
    stringsAsFactors = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
