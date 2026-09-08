#!/usr/bin/env Rscript

## Outcome-free focused tests for the Graham--Svolik v2.8.3 fit runner.

options(stringsAsFactors = FALSE, warn = 1)
.test_script <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this test with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(.test_script()), "..", "..", ".."),
                      mustWork = TRUE)
if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required")
pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
sys.source(file.path(root, "applications", "gs2020", "R",
                     "gs_v283_contract.R"), envir = environment())

checks <- 0L
check <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}

check(identical(.gs_v283_set_rng_contract(), .gs_v283_rng_kind),
      "The RNG-kind contract was not installed")
.gs_v283_set_thread_contract()
runtime <- .gs_v283_runtime_binding()
check(identical(names(runtime$package_versions),
                c("torch", "pkgload", "digest", "withr")) &&
        grepl("^[0-9a-f]{64}$", runtime$R_executable_sha256) &&
        identical(unname(runtime$thread_contract$torch_threads), c(1L, 1L)),
      "The runtime binding is incomplete")

checkpoint_dir <- tempfile("gs-v283-binding-")
checkpoint_path <- file.path(checkpoint_dir, "checkpoint.rds")
checkpoint_binding <- list(protocol_id = .gs_v283_protocol_id,
                           kind = "expected")
.gs_v283_save_atomic(list(value = 1), checkpoint_path, checkpoint_binding)
check(identical(.gs_v283_read_checkpoint(
  checkpoint_path, checkpoint_binding)$payload$value, 1),
  "An immutable checkpoint did not round trip")
unlink(paste0(checkpoint_path, ".sha256"))
recovered <- .gs_v283_read_checkpoint(checkpoint_path, checkpoint_binding)
check(identical(recovered$payload$value, 1) &&
        file.exists(paste0(checkpoint_path, ".sha256")),
      "A valid installed envelope with a missing sidecar was not recovered")
wrong <- checkpoint_binding; wrong$kind <- "foreign"
check(inherits(try(.gs_v283_read_checkpoint(
  checkpoint_path, wrong), silent = TRUE), "try-error"),
  "A checkpoint with a foreign binding was accepted")
unlink(checkpoint_dir, recursive = TRUE, force = TRUE)

check(identical(.gs_v283_polish_schedule$max_iter,
                c(100L, 400L, 500L, 1000L)) &&
        identical(.gs_v283_polish_schedule$learning_rate,
                  c(1, 1, 1, 0.25)) &&
        identical(.gs_v283_polish_max_iter, 2000L),
      "The staged L-BFGS schedule changed")
stage_body <- paste(deparse(body(.gs_v283_fit_one_stage)), collapse = "\n")
check(grepl("polish_tolerance_grad = 0", stage_body, fixed = TRUE) &&
        grepl("polish_tolerance_change = 0", stage_body, fixed = TRUE),
      "The internal absolute early-stop tolerances are not disabled")

map <- .gs_v283_affine_map(c(-1, 0, 1, 2), c(1, 3, 5, 7), rep(TRUE, 4))
check(abs(map$intercept - 3) < 1e-14 && abs(map$slope - 2) < 1e-14,
      "The affine input remap is incorrect")
new <- cbind(a = c(-1, 0, 1), b = c(1, 3, 5))
old <- cbind(a = 4 + 2 * new[, "a"], b = -1 + 0.5 * new[, "b"])
maps <- .gs_v283_affine_maps(new, old)
check(max(abs(maps$intercept - c(4, -1))) < 1e-14 &&
        max(abs(maps$slope - c(2, 0.5))) < 1e-14,
      "The multivariate affine input remap is incorrect")

nm <- c("a", "b", "c")
scale <- stats::setNames(c(2, 3, 4), nm)
check(identical(.gs_v283_canonical_coefficient_scale(scale, scale, nm),
                scale), "Matching coefficient scales were rejected")
bad <- scale; bad[[2L]] <- 3.1
check(inherits(try(.gs_v283_canonical_coefficient_scale(
  scale, bad, nm), silent = TRUE), "try-error"),
  "Mismatched coefficient scales were accepted")

shared_source <- torch::torch_tensor(c(1, -2, 3), dtype = torch::torch_float64())
shared_warm <- list(first = shared_source, second = shared_source)
owned_warm <- .gs_v283_clone_owned_warm(shared_warm)
invisible(owned_warm$first$zero_())
check(identical(as.numeric(shared_source), c(1, -2, 3)) &&
        identical(as.numeric(owned_warm$second), c(1, -2, 3)) &&
        !identical(owned_warm$first$storage()$data_ptr(),
                   shared_source$storage()$data_ptr()) &&
        !identical(owned_warm$first$storage()$data_ptr(),
                   owned_warm$second$storage()$data_ptr()),
      "The application-owned warm tensors retain shared storage")
invisible(shared_source$fill_(9))
check(identical(as.numeric(owned_warm$second), c(1, -2, 3)),
      "A caller mutation reached an application-owned warm tensor")
check(inherits(try(.gs_v283_clone_owned_warm(
  list(bad = torch::torch_tensor(c(1, Inf)))), silent = TRUE), "try-error"),
  "A nonfinite caller-owned warm tensor was accepted")

relu_net <- sconjoint:::.sc_build_mixed_network(
  p = 1L, p_Z = 1L, q = 1L, hidden = 1L, mean_family = "relu",
  mu_bound = 1, kappa_bound = 10, alpha_bound = 0.9,
  a_bound = 10, weight_bound = 10, coefficient_scale = 1,
  compute_dtype = "float64")
relu_arrays <- lapply(relu_net$state_dict(), .gs_v283_tensor_array)
relu_arrays[["hidden.0.weight"]][] <- 1
relu_arrays[["hidden.0.bias"]][] <- 0
relu_arrays[["param_layer.weight"]][] <- 2
relu_arrays$alpha_raw[] <- 0.9
relu_net$load_state_dict(.gs_v283_arrays_to_warm(relu_arrays, relu_net))
relu_net$eval()
z0 <- matrix(0, 1L, 1L)
za <- matrix(0.8, 1L, 1L)
unbounded_shift <- as.numeric(
  .gs_v283_unbounded_relu_deviation(relu_net, za) -
    .gs_v283_unbounded_relu_deviation(relu_net, z0))
clipped_shift <- as.numeric(
  sconjoint:::.sc_predict_beta(relu_net, za) -
    sconjoint:::.sc_predict_beta(relu_net, z0))
check(abs(unbounded_shift - 1.6) < 1e-14 &&
        abs(clipped_shift - 0.1) < 1e-14,
      "The q=1 alpha shift was inferred from a clipped prediction")

a1 <- c(1, -2, 3, 0, 1, -1, 2)
a2 <- .gs_v283_orthogonal_column(a1)
check(abs(sum(a1 * a2)) < 1e-12 &&
        abs(sqrt(sum(a2^2)) / sqrt(sum(a1^2)) - 0.05) < 1e-12 &&
        identical(a2, .gs_v283_orthogonal_column(a1)),
      "The q=2 loading initializer is not deterministic and orthogonal")

mock_starts <- data.frame(
  start_id = c("best", "inferior"),
  penalized_mean_nll = c(1, 1.1), objective_finite = TRUE,
  dtype = "float64", lbfgs_completed = TRUE, lbfgs_accepted = TRUE,
  lbfgs_nonworsening = TRUE, projection_diagnostic_complete = TRUE,
  projection_changed = FALSE, bound_activity = FALSE,
  gradient_contract_pass = TRUE, kkt_maximum = c(1e-6, 1e-4),
  stringsAsFactors = FALSE)
surface <- sconjoint:::.scmix_v28_start_surface(mock_starts)
check(identical(surface$selected_start_id, "best") && surface$fit_gate_pass &&
        surface$n_near == 1L &&
        surface$surface$inferior_start_diagnostic[[2L]],
      "The near-optimum start rule changed")

runner_text <- paste(readLines(file.path(
  root, "applications", "gs2020", "R", "05_run_fast_v283.R"), warn = FALSE),
  collapse = "\n")
check(grepl(paste0(
  'if (outcome_command) .gs_v283_bootstrap_guard(repo_root, "v283")',
  "\n  .gs_v283_load_code(repo_root)"), runner_text, fixed = TRUE),
  "Outcome code can load before the clean hash-bound bootstrap guard")
check(grepl("prior_fit_seconds + 6.25 * sentinel_seconds", runner_text,
            fixed = TRUE) &&
        grepl("deferred_by_frozen_timing_rule", runner_text, fixed = TRUE),
      "The q=2 timing rule is missing or order-dependent")
fit_one_start_text <- paste(
  deparse(body(.gs_v283_fit_one_start)), collapse = "\n")
clone_at <- regexpr(
  "current_warm <- .gs_v283_clone_owned_warm(warm)", fit_one_start_text,
  fixed = TRUE)[[1L]]
optimizer_at <- regexpr(
  ".gs_v283_fit_one_stage(", fit_one_start_text, fixed = TRUE)[[1L]]
check(clone_at > 0L && optimizer_at > clone_at,
      "fit_one_start does not claim independent warm storage before optimization")

## Exercise the real staged optimizer on synthetic data.  The dimensions and
## ReLU family reproduce the ownership-sensitive q=1 state shape without
## reading any application outcome.
set.seed(283)
synthetic_p <- 30L
synthetic_pz <- 22L
synthetic_scale <- seq(0.6, 1.4, length.out = synthetic_p)
synthetic_net <- sconjoint:::.sc_build_mixed_network(
  p = synthetic_p, p_Z = synthetic_pz, q = 1L, hidden = 4L,
  mean_family = "relu", coefficient_scale = synthetic_scale,
  compute_dtype = "float64")
synthetic_arrays <- lapply(synthetic_net$state_dict(),
                           .gs_v283_private_host_array)
synthetic_warm <- .gs_v283_arrays_to_warm(synthetic_arrays, synthetic_net)
synthetic_template <- .gs_v283_warm_template(synthetic_warm)
synthetic_base_hash <- .gs_v283_template_state_hash(synthetic_template)
synthetic_templates <- list(
  exact = .gs_v283_copy_warm_template(synthetic_template),
  perturbed = .gs_v283_perturb_template(
    synthetic_template, .gs_v283_seed(1L, "full", "perturbed")),
  constant = .gs_v283_constant_template(synthetic_template))
synthetic_expected <- vapply(synthetic_templates,
                             .gs_v283_template_state_hash, character(1L))
check(length(unique(synthetic_expected)) == 3L,
      "The three synthetic start templates are not distinct")

## Drop construction locals and place allocation pressure between tensorizing
## and fitting. This detects borrowed R-host storage that pointer tests miss.
rm(synthetic_arrays, synthetic_warm)
invisible(gc(verbose = FALSE))
pressure <- lapply(seq_len(32L), function(i) numeric(4096L))
rm(pressure)
invisible(gc(verbose = FALSE))

synthetic_ids <- rep(sprintf("synthetic-%02d", seq_len(10L)), each = 2L)
synthetic_dat <- list(
  deltaX = matrix(rnorm(length(synthetic_ids) * synthetic_p),
                  ncol = synthetic_p),
  y = rep(c(0, 1), 10L), respondent_id = synthetic_ids)
synthetic_z <- matrix(rnorm(length(synthetic_ids) * synthetic_pz),
                      ncol = synthetic_pz)
synthetic_spec <- list(hidden = 4L, mean_family = "relu",
                       weight_decay = 0.1)
synthetic_architecture <- list(
  mu_bound = 10, kappa_bound = 10, alpha_bound = 5,
  a_bound = 10, weight_bound = 10)
saved_schedule <- .gs_v283_polish_schedule
saved_cap <- .gs_v283_polish_max_iter
.gs_v283_polish_schedule <- data.frame(
  stage = 1:2, max_iter = c(1L, 1L), learning_rate = c(1, 0.25),
  stringsAsFactors = FALSE)
.gs_v283_polish_max_iter <- 2L
synthetic_fits <- lapply(seq_along(synthetic_templates), function(j) {
  .gs_v283_fit_template_start(
    synthetic_templates[[j]], synthetic_dat, synthetic_z, synthetic_scale,
    synthetic_spec, 1L, 2830L + j, synthetic_architecture,
    .gs_v283_grid(1L), FALSE)
})
.gs_v283_polish_schedule <- saved_schedule
.gs_v283_polish_max_iter <- saved_cap
synthetic_observed <- vapply(synthetic_fits, function(x) {
  x$v28_stage_ledger$input_state_hash[[1L]]
}, character(1L))
check(identical(unname(synthetic_observed), unname(synthetic_expected)) &&
        identical(.gs_v283_template_state_hash(synthetic_template),
                  synthetic_base_hash) &&
        all(vapply(synthetic_fits, function(x) {
          isTRUE(x$v283_caller_input_unchanged) &&
            isTRUE(x$v283_input_hash_verified) &&
            nrow(x$v28_stage_ledger) == 2L
        }, logical(1L))),
      paste("Sequential exact, perturbed, and constant optimizer calls did not",
            "preserve and reconstruct their immutable input hashes"))

source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
if (nzchar(source_root)) {
  protocol <- .gs_v283_validate_protocol(
    root, source_root, require_external = TRUE, require_clean = FALSE)
  preflight <- .gs_v283_prepare_input(protocol, source_root)
  check(identical(dim(preflight$prepared$deltaX), c(20657L, 30L)) &&
          identical(dim(preflight$prepared$Z_primary), c(20657L, 22L)) &&
          length(unique(preflight$prepared$respondent_id)) == 1605L &&
          isFALSE(preflight$prior_full_fit_available),
        "The verified input does not match the frozen sample")
  nested <- readRDS(.gs_v283_input_path(
    protocol, source_root, "v21_primary_nested"))
  score_tamper <- nested
  names(score_tamper$tuning[[1L]]$sequence_scores[[1L]][[1L]])[[1L]] <-
    "__not_an_inner_validation_respondent__"
  check(inherits(try(.gs_v283_assert_historical_inner_tuning(
    preflight$prepared, score_tamper, preflight$fold_index), silent = TRUE),
    "try-error"),
  "An inner score detached from its validation respondent was accepted")
  preprocessing_tamper <- nested
  preprocessing_tamper$tuning[[1L]]$preprocessing_folds[[1L]]$
    training_respondents[[1L]] <- preprocessing_tamper$tuning[[1L]]$
      preprocessing_folds[[1L]]$validation_respondents[[1L]]
  check(inherits(try(.gs_v283_assert_historical_inner_tuning(
    preflight$prepared, preprocessing_tamper, preflight$fold_index),
    silent = TRUE), "try-error"),
  "A historical inner preprocessing partition was accepted after tampering")
  selection_tamper <- nested
  selection_tamper$tuning[[1L]]$candidate_selection_gate$selected_index <-
    1L + selection_tamper$tuning[[1L]]$selected_index
  check(inherits(try(.gs_v283_assert_historical_inner_tuning(
    preflight$prepared, selection_tamper, preflight$fold_index),
    silent = TRUE), "try-error"),
  "An inconsistent historical candidate selection was accepted")
  legacy_full <- .gs_v283_legacy_scope(protocol, source_root, "full")
  source_rows_full <- .gs_v283_scope_rows(preflight$prepared, legacy_full)
  alias_tamper <- legacy_full
  alias_tamper$preprocessing$deltaX$scale[[1L]] <-
    alias_tamper$preprocessing$deltaX$scale[[1L]] * 1.01
  check(inherits(try(.gs_v283_q1_initializer(
    alias_tamper, preflight,
    seq_len(nrow(preflight$prepared$deltaX)), source_rows_full),
    silent = TRUE), "try-error"),
  "A divergent fit-level/state preprocessing alias was accepted")
  reproduction <- vapply(.gs_v283_scopes, function(scope) {
    legacy <- .gs_v283_legacy_scope(protocol, source_root, scope)
    source_rows <- .gs_v283_scope_rows(preflight$prepared, legacy)
    fitting_rows <- if (scope == "full") {
      seq_len(nrow(preflight$prepared$deltaX))
    } else source_rows
    initialized <- .gs_v283_q1_initializer(
      legacy, preflight, fitting_rows, source_rows)
    if (scope != "full" && !identical(fitting_rows, source_rows)) {
      stop("An outer initializer used nontraining rows.", call. = FALSE)
    }
    initialized$valid_prediction_max_difference
  }, numeric(1L))
  check(all(is.finite(reproduction)) && max(reproduction) <= 1e-6,
        "An actual historical q=1 state failed affine/scale invariance")

  ## Use the actual hash-bound full-scope initializer but synthetic outcomes
  ## and covariates. This reproduces the storage-sensitive state without
  ## estimating or printing an application result.
  actual_initialized <- .gs_v283_q1_initializer(
    legacy_full, preflight, seq_len(nrow(preflight$prepared$deltaX)),
    source_rows_full)
  actual_template <- .gs_v283_warm_template(actual_initialized$warm)
  actual_templates <- list(
    exact = .gs_v283_copy_warm_template(actual_template),
    perturbed = .gs_v283_perturb_template(
      actual_template, .gs_v283_seed(1L, "full", "perturbed")),
    constant = .gs_v283_constant_template(actual_template))
  actual_expected <- vapply(actual_templates,
                            .gs_v283_template_state_hash, character(1L))
  actual_base_hash <- .gs_v283_template_state_hash(actual_template)
  actual_initialized$warm <- NULL
  invisible(gc(verbose = FALSE))
  pressure <- lapply(seq_len(32L), function(i) numeric(4096L))
  rm(pressure)
  invisible(gc(verbose = FALSE))
  saved_schedule <- .gs_v283_polish_schedule
  saved_cap <- .gs_v283_polish_max_iter
  .gs_v283_polish_schedule <- data.frame(
    stage = 1:2, max_iter = c(1L, 1L), learning_rate = c(1, 0.25),
    stringsAsFactors = FALSE)
  .gs_v283_polish_max_iter <- 2L
  actual_fits <- lapply(seq_along(actual_templates), function(j) {
    .gs_v283_fit_template_start(
      actual_templates[[j]], synthetic_dat, synthetic_z,
      actual_initialized$coefficient_scale, legacy_full$specification, 1L,
      3830L + j, actual_initialized$target_architecture,
      .gs_v283_grid(1L), FALSE)
  })
  .gs_v283_polish_schedule <- saved_schedule
  .gs_v283_polish_max_iter <- saved_cap
  actual_observed <- vapply(actual_fits, function(x) {
    x$v28_stage_ledger$input_state_hash[[1L]]
  }, character(1L))
  check(identical(unname(actual_observed), unname(actual_expected)) &&
          identical(.gs_v283_template_state_hash(actual_template),
                    actual_base_hash) &&
          all(vapply(actual_fits, function(x) {
            isTRUE(x$v283_caller_input_unchanged) &&
              isTRUE(x$v283_input_hash_verified)
          }, logical(1L))),
        paste("The actual q=1 initializer did not preserve exact, perturbed,",
              "and constant hashes through sequential synthetic optimization"))
}

cat(sprintf("Graham--Svolik v2.8.3 fit helper checks passed: %d\n", checks))
