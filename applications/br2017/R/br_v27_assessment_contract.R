## Ballard--Rosa v2.7 post-fit assessment helpers.
##
## This layer is deliberately separate from the immutable v2.7 fitting
## protocol.  It reads only hash-verified assembled checkpoints and writes one
## new assessment checkpoint under a disjoint output directory.  Nothing in
## this file fits a model, changes an assembled checkpoint, or constructs a
## new substantive estimand.

.br_v27a_protocol_id <- "br2017-paperps-assessment-2026-09-07-v2.7-a1"
.br_v27a_upstream_protocol_id <- "br2017-paperps-rapid-2026-09-07-v2.7-v1"
.br_v27a_run_relative <- file.path(
  "runs", "br2017", "v27", "assessment-2026-09-07-v1")
.br_v27a_upstream_relative <- file.path(
  "runs", "br2017", "v27", "rapid-2026-09-07-v1")
.br_v27a_ranks <- 0:2
.br_v27a_local_ranks <- 1:2
.br_v27a_calibration_bins <- 10L

.br_v27a_stop <- function(...) stop(..., call. = FALSE)

.br_v27a_sha256 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  shasum <- Sys.which("shasum")
  if (!nzchar(shasum)) .br_v27a_stop("The `shasum` executable is required.")
  line <- system2(shasum, c("-a", "256", shQuote(path)), stdout = TRUE,
                  stderr = TRUE)
  if (length(line) != 1L || !grepl("^[0-9a-f]{64}[[:space:]]", line)) {
    .br_v27a_stop("Could not compute SHA-256 for ", path, ".")
  }
  sub("[[:space:]].*$", "", line)
}

.br_v27a_object_sha256 <- function(object) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    .br_v27a_stop("The `digest` package is required.")
  }
  digest::digest(object, algo = "sha256", serialize = TRUE,
                 serializeVersion = 2L)
}

.br_v27a_assert_inside <- function(path, parent, what = "path") {
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
    .br_v27a_stop(what, " is outside its permitted root.")
  }
  invisible(resolved)
}

.br_v27a_protocol_dir <- function(repo_root) {
  file.path(repo_root, "applications", "br2017", "protocol",
            "v27-assessment")
}

.br_v27a_run_dir <- function(repo_root) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  path <- file.path(repo_root, .br_v27a_run_relative)
  .br_v27a_assert_inside(path, repo_root, "The assessment output directory")
  path
}

.br_v27a_output_path <- function(repo_root) {
  file.path(.br_v27a_run_dir(repo_root), "assessment.rds")
}

.br_v27a_read_dcf <- function(path) {
  raw <- read.dcf(path, all = TRUE)
  if (nrow(raw) != 1L) .br_v27a_stop(path, " must contain one DCF record.")
  setNames(as.character(raw[1L, ]), colnames(raw))
}

.br_v27a_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = character())
}

.br_v27a_git_snapshot <- function(repo_root, require_clean = TRUE) {
  git <- Sys.which("git")
  if (!nzchar(git)) .br_v27a_stop("Git is required for the assessment.")
  tracked <- c(
    "applications/br2017/protocol/v27-assessment",
    "applications/br2017/R/br_v27_assessment_contract.R",
    "applications/br2017/R/08_assess_fast_v27.R",
    "applications/br2017/R/test_br_v27_assessment.R")
  commit <- system2(git, c("-C", shQuote(repo_root), "rev-parse", "HEAD"),
                    stdout = TRUE, stderr = TRUE)
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    .br_v27a_stop("Could not identify the assessment Git commit.")
  }
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain",
                           "--", tracked), stdout = TRUE, stderr = TRUE)
  listed <- system2(git, c("-C", shQuote(repo_root), "ls-files", "--", tracked),
                    stdout = TRUE, stderr = TRUE)
  required <- c(
    "applications/br2017/R/br_v27_assessment_contract.R",
    "applications/br2017/R/08_assess_fast_v27.R",
    "applications/br2017/R/test_br_v27_assessment.R")
  if (isTRUE(require_clean) && length(status)) {
    .br_v27a_stop(
      "The assessment protocol or implementation is uncommitted or modified: ",
      paste(status, collapse = "; "))
  }
  if (isTRUE(require_clean) && !all(required %in% listed)) {
    .br_v27a_stop("Every assessment implementation file must be tracked before use.")
  }
  list(commit = commit, clean = !length(status), status = status,
       tracked = listed)
}

.br_v27a_validate_protocol <- function(repo_root, require_clean = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  protocol_dir <- .br_v27a_protocol_dir(repo_root)
  contract <- .br_v27a_read_dcf(file.path(protocol_dir,
                                           "assessment-contract.dcf"))
  required <- c(
    `Protocol-Id` = .br_v27a_protocol_id,
    `Upstream-Protocol-Id` = .br_v27a_upstream_protocol_id,
    `Upstream-Run-Directory` = gsub("\\\\", "/", .br_v27a_upstream_relative),
    `Output-Directory` = gsub("\\\\", "/", .br_v27a_run_relative),
    `Output-File` = "assessment.rds",
    `Ranks` = "0,1,2",
    `Local-Covariance-Injectivity-Ranks` = "1,2",
    `Calibration-Bins` = "10",
    `Numerical-Rank-Rule` =
      "max_matrix_dimension_times_machine_double_epsilon_times_leading_singular_value",
    `Structural-AME-Versus-Design-AMCE` = "unavailable_without_new_target_construction",
    `Substantive-Target-Construction` = "prohibited",
    `Fit-Or-Refit` = "prohibited")
  for (field in names(required)) {
    if (!identical(unname(contract[[field]]), unname(required[[field]]))) {
      .br_v27a_stop("Unexpected assessment contract field: ", field, ".")
    }
  }
  upstream_checksum <- file.path(repo_root, "applications", "br2017",
                                 "protocol", "v27", "CHECKSUMS.sha256")
  if (!identical(.br_v27a_sha256(upstream_checksum),
                 unname(contract[["Upstream-Protocol-Checksum-SHA256"]]))) {
    .br_v27a_stop("The immutable v2.7 fit protocol checksum changed.")
  }

  checksum_path <- file.path(protocol_dir, "CHECKSUMS.sha256")
  lines <- readLines(checksum_path, warn = FALSE)
  fields <- strsplit(lines, "  ", fixed = TRUE)
  if (!length(lines) || any(lengths(fields) != 2L)) {
    .br_v27a_stop("Malformed assessment checksum registry.")
  }
  hashes <- vapply(fields, `[[`, character(1L), 1L)
  paths <- vapply(fields, `[[`, character(1L), 2L)
  observed <- vapply(paths, function(path) {
    .br_v27a_sha256(file.path(protocol_dir, path))
  }, character(1L))
  if (!identical(unname(hashes), unname(observed))) {
    .br_v27a_stop("An assessment protocol file differs from CHECKSUMS.sha256.")
  }

  implementation <- .br_v27a_read_csv(file.path(
    protocol_dir, "implementation-manifest.csv"))
  expected_names <- c("implementation_id", "relative_path", "sha256")
  if (!identical(names(implementation), expected_names) ||
      anyDuplicated(implementation$implementation_id) ||
      anyDuplicated(implementation$relative_path) || !nrow(implementation)) {
    .br_v27a_stop("Malformed assessment implementation manifest.")
  }
  implementation_hash <- vapply(implementation$relative_path, function(path) {
    .br_v27a_sha256(file.path(repo_root, path))
  }, character(1L))
  if (!identical(unname(implementation_hash),
                 unname(implementation$sha256))) {
    .br_v27a_stop("Assessment implementation hash mismatch.")
  }
  registry <- .br_v27a_read_csv(file.path(protocol_dir,
                                           "assessment-registry.csv"))
  upstream <- .br_v27a_read_csv(file.path(protocol_dir,
                                           "upstream-registry.csv"))
  expected_upstream <- data.frame(
    q = 0:2,
    relative_path = gsub("\\\\", "/", file.path(
      .br_v27a_upstream_relative, paste0("q", 0:2), "assembled.rds")),
    required_schema = rep("br2017-v2.7-assembled-v1", 3L),
    required_upstream_protocol_id = rep(.br_v27a_upstream_protocol_id, 3L),
    stringsAsFactors = FALSE)
  if (!identical(upstream, expected_upstream)) {
    .br_v27a_stop("The assessment upstream registry changed.")
  }
  git <- .br_v27a_git_snapshot(repo_root, require_clean = require_clean)
  list(protocol_id = .br_v27a_protocol_id, protocol_dir = protocol_dir,
       protocol_hash = .br_v27a_sha256(checksum_path), contract = contract,
       registry = registry, upstream = upstream,
       implementation = implementation, git = git)
}

.br_v27a_matrix_spectrum <- function(M) {
  M <- as.matrix(M)
  if (!is.numeric(M) || any(!is.finite(M)) || !length(M) ||
      nrow(M) < 1L || ncol(M) < 1L) {
    .br_v27a_stop("A numerical-rank matrix must be finite and nonempty.")
  }
  singular <- svd(M, nu = 0L, nv = 0L)$d
  largest <- if (length(singular)) singular[[1L]] else 0
  dimension_factor <- max(dim(M))
  threshold <- dimension_factor * .Machine$double.eps * largest
  rank <- if (largest == 0) 0L else sum(singular > threshold)
  smallest <- if (rank) singular[[rank]] else NA_real_
  list(
    rule = paste0(
      "max(nrow,ncol) * .Machine$double.eps * leading singular value; ",
      "strict singular-value comparison"),
    dimension_factor = as.integer(dimension_factor),
    machine_double_epsilon = .Machine$double.eps,
    threshold = threshold,
    rank = as.integer(rank),
    singular_values = singular,
    largest = largest,
    smallest_retained = smallest,
    first_excluded = if (rank < length(singular)) singular[[rank + 1L]] else
      NA_real_,
    condition_retained = if (rank) largest / smallest else Inf)
}

.br_v27a_quadratic_design <- function(D) {
  D <- as.matrix(D)
  p <- ncol(D)
  pairs <- which(upper.tri(matrix(FALSE, p, p), diag = TRUE),
                 arr.ind = TRUE)
  out <- vapply(seq_len(nrow(pairs)), function(h) {
    j <- pairs[h, 1L]
    k <- pairs[h, 2L]
    if (j == k) D[, j]^2 else 2 * D[, j] * D[, k]
  }, numeric(nrow(D)))
  dim(out) <- c(nrow(D), nrow(pairs))
  dnames <- colnames(D) %||% paste0("x", seq_len(p))
  colnames(out) <- vapply(seq_len(nrow(pairs)), function(h) {
    paste0(dnames[pairs[h, 1L]], "__", dnames[pairs[h, 2L]])
  }, character(1L))
  out
}

.br_v27a_rotation_residual <- function(J, A) {
  q <- ncol(A)
  if (q < 2L) return(0)
  residual <- numeric(q * (q - 1L) / 2L)
  h <- 0L
  for (r in seq_len(q - 1L)) {
    for (s in (r + 1L):q) {
      h <- h + 1L
      omega <- matrix(0, q, q)
      omega[r, s] <- 1
      omega[s, r] <- -1
      residual[[h]] <- max(abs(as.numeric(J %*% as.numeric(A %*% omega))))
    }
  }
  max(residual)
}

.br_v27a_design_diagnostics <- function(D) {
  D <- as.matrix(D)
  if (is.null(colnames(D))) colnames(D) <- paste0("x", seq_len(ncol(D)))
  unique_D <- unique(D)
  linear <- .br_v27a_matrix_spectrum(unique_D)
  Q <- .br_v27a_quadratic_design(unique_D)
  quadratic <- .br_v27a_matrix_spectrum(Q)
  list(
    n_rows_full = nrow(D), n_unique_realized_contrasts = nrow(unique_D),
    p = ncol(unique_D), contrast_sha256 = .br_v27a_object_sha256(unique_D),
    linear = linear, unrestricted_quadratic = quadratic,
    summary = data.frame(
      design = c("linear", "unrestricted_quadratic"),
      rows = c(nrow(unique_D), nrow(Q)),
      columns = c(ncol(unique_D), ncol(Q)),
      numerical_rank = c(linear$rank, quadratic$rank),
      numerical_threshold = c(linear$threshold, quadratic$threshold),
      condition_retained = c(linear$condition_retained,
                             quadratic$condition_retained),
      stringsAsFactors = FALSE),
    unique_contrasts = unique_D)
}

.br_v27a_local_certificate <- function(D, A, q, source_sha256 = NA_character_) {
  D <- as.matrix(D)
  A <- as.matrix(A)
  q <- as.integer(q)
  if (!q %in% .br_v27a_local_ranks || !is.numeric(D) || !is.numeric(A) ||
      any(!is.finite(D)) || any(!is.finite(A)) ||
      !identical(dim(A), c(ncol(D), q))) {
    .br_v27a_stop("A local certificate requires finite D and a p by q loading.")
  }
  D <- unique(D)
  loading <- .br_v27a_matrix_spectrum(A)
  projected <- D %*% A
  J <- do.call(cbind, lapply(seq_len(q), function(k) {
    2 * projected[, k] * D
  }))
  tangent <- .br_v27a_matrix_spectrum(J)
  p <- ncol(D)
  loading_dim <- p * q
  rotation_dim <- q * (q - 1L) / 2L
  covariance_tangent_dim <- loading_dim - rotation_dim
  nullity <- ncol(J) - tangent$rank
  projection_norm <- sqrt(rowSums(projected^2))
  established <- loading$rank == q &&
    tangent$rank == covariance_tangent_dim && nullity == rotation_dim
  certificate <- list(
    verified = TRUE, established = established, q = q,
    method = paste0(
      "Fitted-point SVD rank of the rank-", q,
      " covariance-tangent Jacobian after quotienting loading rotations."),
    domain = paste0(
      "Local rank-", q,
      " positive-semidefinite covariance manifold at the fitted raw-scale loading."),
    numerical_rank_rule = tangent$rule,
    source_assembled_sha256 = source_sha256,
    contrast_sha256 = .br_v27a_object_sha256(D),
    loading_sha256 = .br_v27a_object_sha256(A),
    dimensions = list(
      n_unique_realized_contrasts = nrow(D), p = p,
      loading_parameter_dim = loading_dim, rotation_dim = rotation_dim,
      covariance_tangent_dim = covariance_tangent_dim,
      jacobian_rows = nrow(J), jacobian_columns = ncol(J)),
    ranks = list(
      loading = loading$rank,
      covariance_tangent_jacobian = tangent$rank,
      jacobian_nullity = nullity,
      extra_nullity_beyond_rotations = nullity - rotation_dim),
    information = list(
      tangent_threshold = tangent$threshold,
      tangent_singular_values = tangent$singular_values,
      tangent_largest_singular_value = tangent$largest,
      tangent_smallest_retained_singular_value = tangent$smallest_retained,
      tangent_first_excluded_singular_value = tangent$first_excluded,
      tangent_condition_retained = tangent$condition_retained,
      rotation_null_max_absolute_residual =
        .br_v27a_rotation_residual(J, A),
      n_exactly_orthogonal_contrasts = sum(projection_norm == 0),
      min_projected_loading_norm = min(projection_norm),
      median_projected_loading_norm = stats::median(projection_norm),
      max_projected_loading_norm = max(projection_norm)),
    disclaimer = paste(
      "This fitted-point derivative-rank certificate establishes only local",
      "injectivity of the rank-constrained covariance measurement map at the",
      "reported fitted loading. It does not establish global injectivity,",
      "optimizer convergence, target-specific information, or precise",
      "finite-sample inference."))
  class(certificate) <- c("br_v27_covariance_injectivity_certificate", "list")
  certificate
}

.br_v27a_validate_task <- function(task) {
  required <- c("respondent_id", "task", "fold", "y", "probability")
  if (!is.data.frame(task) || !all(required %in% names(task)) || !nrow(task) ||
      anyNA(task[, required]) || any(!is.finite(task$probability)) ||
      any(task$probability < 0 | task$probability > 1) ||
      any(!task$y %in% c(0, 1))) {
    .br_v27a_stop("A held-out task table is malformed.")
  }
  key <- paste(as.character(task$respondent_id), as.character(task$task),
               sep = "\r")
  if (anyDuplicated(key)) .br_v27a_stop("Held-out task keys are duplicated.")
  invisible(TRUE)
}

.br_v27a_calibration_summary <- function(task, bins = 10L) {
  .br_v27a_validate_task(task)
  bins <- as.integer(bins)
  if (is.na(bins) || bins < 2L) .br_v27a_stop("bins must be at least two.")
  n <- nrow(task)
  probability_rank <- rank(task$probability, ties.method = "first")
  bin <- pmin(bins, ceiling(probability_rank * bins / n))
  by_bin <- do.call(rbind, lapply(split(seq_len(n), bin), function(rows) {
    data.frame(bin = bin[rows[[1L]]], n = length(rows),
               observed = mean(task$y[rows]),
               predicted = mean(task$probability[rows]),
               gap = mean(task$y[rows]) - mean(task$probability[rows]),
               stringsAsFactors = FALSE)
  }))
  by_task <- do.call(rbind, lapply(split(seq_len(n), task$task), function(rows) {
    data.frame(task = task$task[rows[[1L]]], n = length(rows),
               observed = mean(task$y[rows]),
               predicted = mean(task$probability[rows]),
               gap = mean(task$y[rows]) - mean(task$probability[rows]),
               stringsAsFactors = FALSE)
  }))
  rownames(by_bin) <- rownames(by_task) <- NULL
  list(
    overall = data.frame(
      n = n, observed = mean(task$y), predicted = mean(task$probability),
      gap = mean(task$y) - mean(task$probability),
      brier_score = mean((task$y - task$probability)^2),
      max_bin_gap = max(abs(by_bin$gap)),
      max_task_gap = max(abs(by_task$gap)), stringsAsFactors = FALSE),
    by_bin = by_bin, by_task = by_task)
}

.br_v27a_response_diagnostics <- function(task, calibration) {
  .br_v27a_validate_task(task)
  order_index <- order(as.character(task$respondent_id), task$task,
                       method = "radix")
  task <- task[order_index, , drop = FALSE]
  by_id <- split(seq_len(nrow(task)), as.character(task$respondent_id))
  respondent <- do.call(rbind, lapply(by_id, function(rows) {
    y <- task$y[rows]
    switches <- if (length(y) > 1L) sum(y[-1L] != y[-length(y)]) else 0L
    pattern <- if (all(y == 0)) "always_B" else if (all(y == 1)) {
      "always_A"
    } else if (switches == 1L) "one_switch" else "multiple_switches"
    data.frame(n_tasks = length(y), n_A_choices = sum(y),
               n_B_choices = sum(1 - y), n_transitions = max(0, length(y) - 1L),
               n_switches = switches,
               switching_rate = if (length(y) > 1L) switches / (length(y) - 1L)
               else NA_real_, pattern = pattern, stringsAsFactors = FALSE)
  }))
  transition <- do.call(rbind, lapply(by_id, function(rows) {
    y <- task$y[rows]
    if (length(y) < 2L) return(NULL)
    data.frame(from = y[-length(y)], to = y[-1L], stringsAsFactors = FALSE)
  }))
  if (is.null(transition)) {
    transition_table <- data.frame(from = integer(), to = integer(),
                                   count = integer(), rate = numeric())
  } else {
    transition_table <- as.data.frame(table(transition$from, transition$to),
                                      stringsAsFactors = FALSE)
    names(transition_table) <- c("from", "to", "count")
    transition_table$from <- as.integer(as.character(transition_table$from))
    transition_table$to <- as.integer(as.character(transition_table$to))
    transition_table$rate <- transition_table$count / sum(transition_table$count)
  }
  count_distribution <- as.data.frame(table(respondent$n_tasks),
                                      stringsAsFactors = FALSE)
  names(count_distribution) <- c("n_tasks", "n_respondents")
  count_distribution$n_tasks <- as.integer(as.character(
    count_distribution$n_tasks))
  choice_distribution <- as.data.frame(table(respondent$n_A_choices),
                                       stringsAsFactors = FALSE)
  names(choice_distribution) <- c("n_A_choices", "n_respondents")
  choice_distribution$n_A_choices <- as.integer(as.character(
    choice_distribution$n_A_choices))
  pattern <- as.data.frame(table(respondent$pattern), stringsAsFactors = FALSE)
  names(pattern) <- c("pattern", "n_respondents")
  pattern$fraction <- pattern$n_respondents / sum(pattern$n_respondents)
  task_order <- do.call(rbind, lapply(names(calibration), function(rank_name) {
    out <- calibration[[rank_name]]$by_task
    out$q <- as.integer(sub("^q", "", rank_name))
    out[, c("q", "task", "n", "observed", "predicted", "gap")]
  }))
  rownames(task_order) <- NULL
  list(
    analytic_sample_limitation = paste(
      "Response-count and switching diagnostics condition on respondents",
      "retained in the assembled analytic sample. They cannot diagnose",
      "selection caused by pre-assembly completion or missingness rules."),
    transition_limitation = paste(
      "Transition and switch-pattern checks are descriptive observed-sequence",
      "checks. The assembled checkpoint stores marginal held-out task",
      "probabilities but not joint posterior-predictive response paths, so no",
      "model-based transition residual is constructed."),
    n_respondents = nrow(respondent),
    response_count_distribution = count_distribution,
    A_choice_count_distribution = choice_distribution,
    switching_summary = data.frame(
      mean_switches = mean(respondent$n_switches),
      median_switches = stats::median(respondent$n_switches),
      mean_switching_rate = mean(respondent$switching_rate, na.rm = TRUE),
      stringsAsFactors = FALSE),
    switch_pattern = pattern,
    transition_table = transition_table,
    task_order_calibration = task_order)
}

.br_v27a_availability <- function(registry) {
  required <- c("assessment_id", "status", "reason")
  if (!is.data.frame(registry) || !identical(names(registry), required) ||
      anyDuplicated(registry$assessment_id) || any(!nzchar(registry$status)) ||
      any(!nzchar(registry$reason))) {
    .br_v27a_stop("The assessment availability registry is malformed.")
  }
  registry
}

## Reconstruct the immutable fit-runner binding locally.  The upstream CLI
## defines its own convenience helper, but the assessment deliberately loads
## only the upstream contract so it has no hidden dependency on executable
## command code.
.br_v27a_upstream_binding <- function(protocol, q) {
  list(
    protocol_id = protocol$protocol_id,
    protocol_hash = protocol$protocol_hash,
    git_commit = protocol$git$commit,
    application = "br2017",
    kind = "assembled",
    q = as.integer(q),
    scope = "all",
    input_hashes = stats::setNames(protocol$inputs$sha256,
                                   protocol$inputs$input_id))
}

.br_v27a_extract_upstream <- function(repo_root) {
  if (!exists(".br_v27_validate_protocol", mode = "function") ||
      !exists(".br_v27_read_checkpoint", mode = "function")) {
    .br_v27a_stop("Load the immutable v2.7 fit contract before assessment.")
  }
  upstream_protocol <- .br_v27_validate_protocol(
    repo_root, require_external = FALSE, require_clean = FALSE)
  if (!identical(upstream_protocol$protocol_id,
                 .br_v27a_upstream_protocol_id)) {
    .br_v27a_stop("Unexpected upstream v2.7 protocol.")
  }
  run_dir <- .br_v27_run_dir(repo_root)
  entries <- lapply(.br_v27a_ranks, function(q) {
    path <- .br_v27_checkpoint_path(run_dir, "assembled", q)
    binding <- .br_v27a_upstream_binding(upstream_protocol, q)
    envelope <- .br_v27_read_checkpoint(path, binding = binding)
    expected_schema <- "br2017-v2.7-assembled-v1"
    if (!is.list(envelope$payload) ||
        !identical(envelope$payload$schema_version, expected_schema) ||
        !identical(as.integer(envelope$payload$q), as.integer(q))) {
      .br_v27a_stop("Malformed v2.7 assembled checkpoint for q=", q, ".")
    }
    list(q = as.integer(q), path = path, sha256 = .br_v27a_sha256(path),
         envelope = envelope)
  })
  names(entries) <- paste0("q", .br_v27a_ranks)
  entries
}

.br_v27a_validate_upstream_alignment <- function(entries) {
  if (!is.list(entries) || !identical(names(entries), paste0("q", 0:2))) {
    .br_v27a_stop("Three named q0/q1/q2 assembled inputs are required.")
  }
  tasks <- lapply(entries, function(x) x$envelope$payload$heldout$task)
  lapply(tasks, .br_v27a_validate_task)
  reference <- tasks$q1[, c("respondent_id", "task", "fold", "y"),
                        drop = FALSE]
  aligned <- vapply(tasks, function(x) {
    identical(x[, c("respondent_id", "task", "fold", "y"), drop = FALSE],
              reference)
  }, logical(1L))
  if (!all(aligned)) .br_v27a_stop("Held-out rows differ across q=0,1,2.")
  D <- as.matrix(entries$q1$envelope$payload$full_fit$raw_data$deltaX)
  if (!is.numeric(D) || any(!is.finite(D)) || !nrow(D) || !ncol(D)) {
    .br_v27a_stop("The q1 full fit has no finite contrast matrix.")
  }
  for (rank_name in names(entries)) {
    other <- as.matrix(entries[[rank_name]]$envelope$payload$full_fit$
                         raw_data$deltaX)
    if (!identical(other, D)) {
      .br_v27a_stop("Full-sample contrasts differ across ranks.")
    }
  }
  list(tasks = tasks, D = D)
}

.br_v27a_raw_loading <- function(fit, q) {
  if (!inherits(fit, "scmix_tuned_matrix_fit")) {
    .br_v27a_stop("A v2.7 full fit is not a tuned matrix fit.")
  }
  A <- sconjoint::scmix_loadings(fit, scale = "raw", scope = "full")
  A <- as.matrix(A)
  if (!identical(dim(A), c(ncol(fit$raw_data$deltaX), as.integer(q))) ||
      any(!is.finite(A))) {
    .br_v27a_stop("The v2.7 raw loading has the wrong dimension or scale.")
  }
  if (!is.null(fit$A) &&
      !isTRUE(all.equal(A, as.matrix(fit$A), tolerance = 1e-12,
                        check.attributes = FALSE))) {
    .br_v27a_stop("The public raw-loading accessor disagrees with fit$A.")
  }
  A
}

.br_v27a_build_assessment <- function(entries, registry) {
  aligned <- .br_v27a_validate_upstream_alignment(entries)
  design <- .br_v27a_design_diagnostics(aligned$D)
  calibration <- lapply(aligned$tasks, .br_v27a_calibration_summary,
                        bins = .br_v27a_calibration_bins)
  predictive <- do.call(rbind, lapply(names(entries), function(rank_name) {
    heldout <- entries[[rank_name]]$envelope$payload$heldout
    stored_gap <- as.numeric(heldout$marginal_calibration)
    computed_gap <- calibration[[rank_name]]$overall$gap[[1L]]
    score <- as.numeric(heldout$mean_sequence_log_score)
    if (length(stored_gap) != 1L || !is.finite(stored_gap) ||
        length(score) != 1L || !is.finite(score) ||
        abs(stored_gap - computed_gap) > 1e-12) {
      .br_v27a_stop("A stored held-out predictive summary is inconsistent.")
    }
    data.frame(q = as.integer(sub("^q", "", rank_name)),
               mean_sequence_log_score = score,
               marginal_calibration_gap = stored_gap,
               stringsAsFactors = FALSE)
  }))
  certificates <- lapply(.br_v27a_local_ranks, function(q) {
    rank_name <- paste0("q", q)
    fit <- entries[[rank_name]]$envelope$payload$full_fit
    .br_v27a_local_certificate(
      design$unique_contrasts, .br_v27a_raw_loading(fit, q), q,
      source_sha256 = entries[[rank_name]]$sha256)
  })
  names(certificates) <- paste0("q", .br_v27a_local_ranks)
  design$unique_contrasts <- NULL
  list(
    schema_version = "br2017-v2.7-assessment-v1",
    application = "br2017", ranks = .br_v27a_ranks,
    upstream = data.frame(
      q = .br_v27a_ranks,
      relative_path = vapply(entries, function(x) {
        gsub("\\\\", "/", file.path(.br_v27a_upstream_relative,
                                         paste0("q", x$q), "assembled.rds"))
      }, character(1L)),
      sha256 = vapply(entries, `[[`, character(1L), "sha256"),
      stringsAsFactors = FALSE),
    full_sample_design = design,
    local_covariance_injectivity = certificates,
    heldout_predictive_summary = predictive,
    heldout_calibration = calibration,
    response_and_task_order = .br_v27a_response_diagnostics(
      aligned$tasks$q1, calibration),
    assessment_availability = .br_v27a_availability(registry),
    structural_ame_vs_design_amce = list(
      status = "unavailable",
      reason = paste(
        "The assembled v2.7 checkpoints do not contain a prespecified",
        "structural-AME target map paired with the fielded-design AMCE",
        "weights. Constructing one now would be a new target stage, so this",
        "assessment records the check as unavailable rather than using a",
        "post hoc substitute.")),
    substantive_targets_computed = FALSE,
    general_disclaimer = paste(
      "These are design, predictive-calibration, behavioral-pattern, and",
      "fitted-point local-rank diagnostics. Local rank is not global",
      "identification and none of these checks alone establishes precise",
      "finite-sample heterogeneity inference."))
}

.br_v27a_binding <- function(protocol, entries) {
  list(
    protocol_id = protocol$protocol_id,
    protocol_hash = protocol$protocol_hash,
    git_commit = protocol$git$commit,
    upstream_protocol_id = .br_v27a_upstream_protocol_id,
    upstream_assembled_sha256 = vapply(entries, `[[`, character(1L),
                                          "sha256"))
}

.br_v27a_save_atomic <- function(payload, path, binding) {
  if (!is.list(binding) ||
      !identical(binding$protocol_id, .br_v27a_protocol_id)) {
    .br_v27a_stop("An assessment output requires its exact protocol binding.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sidecar <- paste0(path, ".sha256")
  if (file.exists(path) || file.exists(sidecar)) {
    .br_v27a_stop("Refusing to overwrite an assessment output: ", path)
  }
  token <- paste0(Sys.getpid(), "-", format(as.integer(Sys.time()),
                                             scientific = FALSE))
  temporary <- paste0(path, ".tmp-", token)
  temporary_sidecar <- paste0(sidecar, ".tmp-", token)
  on.exit(unlink(c(temporary, temporary_sidecar), force = TRUE), add = TRUE)
  envelope <- list(
    schema_version = "br2017-v2.7-assessment-checkpoint-v1",
    binding = binding,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    payload = payload)
  saveRDS(envelope, temporary, version = 3L)
  hash <- .br_v27a_sha256(temporary)
  writeLines(paste(hash, basename(path)), temporary_sidecar, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    .br_v27a_stop("Could not atomically install the assessment output.")
  }
  if (!file.rename(temporary_sidecar, sidecar)) {
    .br_v27a_stop("Assessment installed but its SHA sidecar failed.")
  }
  invisible(list(path = path, sha256 = hash))
}

.br_v27a_read_output <- function(path, binding = NULL, payload = TRUE) {
  sidecar <- paste0(path, ".sha256")
  if (!file.exists(path) || !file.exists(sidecar)) {
    .br_v27a_stop("The assessment output is incomplete.")
  }
  expected <- strsplit(readLines(sidecar, warn = FALSE)[[1L]],
                       "[[:space:]]+")[[1L]][1L]
  observed <- .br_v27a_sha256(path)
  if (!identical(expected, observed)) {
    .br_v27a_stop("Assessment output SHA mismatch.")
  }
  if (!isTRUE(payload)) return(list(path = path, sha256 = observed))
  object <- readRDS(path)
  if (!is.list(object) ||
      !identical(object$schema_version,
                 "br2017-v2.7-assessment-checkpoint-v1") ||
      !identical(object$binding$protocol_id, .br_v27a_protocol_id)) {
    .br_v27a_stop("Malformed or foreign assessment output.")
  }
  if (!is.null(binding)) {
    required <- names(binding)
    if (!all(required %in% names(object$binding)) ||
        !identical(object$binding[required], binding[required])) {
      .br_v27a_stop("Assessment output binding mismatch.")
    }
  }
  object
}

.br_v27a_upstream_status <- function(repo_root) {
  run_dir <- file.path(repo_root, .br_v27a_upstream_relative)
  paths <- stats::setNames(file.path(run_dir, paste0("q", 0:2),
                                     "assembled.rds"), paste0("q", 0:2))
  data.frame(
    q = 0:2, relative_path = gsub("\\\\", "/", substring(
      paths, nchar(normalizePath(repo_root, winslash = "/",
                                mustWork = TRUE)) + 2L)),
    complete = vapply(paths, function(path) {
      file.exists(path) && file.exists(paste0(path, ".sha256")) &&
        isTRUE(tryCatch({
          expected <- strsplit(readLines(paste0(path, ".sha256"),
                                         warn = FALSE)[[1L]],
                               "[[:space:]]+")[[1L]][1L]
          identical(expected, .br_v27a_sha256(path))
        }, error = function(e) FALSE))
    }, logical(1L)), stringsAsFactors = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
