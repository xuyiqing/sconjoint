#!/usr/bin/env Rscript

## Outcome-free focused tests for the Graham--Svolik v2.8.1 fit runner.

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
                     "gs_v281_contract.R"), envir = environment())

checks <- 0L
check <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}

check(identical(.gs_v281_set_rng_contract(), .gs_v281_rng_kind),
      "The RNG-kind contract was not installed")
.gs_v281_set_thread_contract()
runtime <- .gs_v281_runtime_binding()
check(identical(names(runtime$package_versions),
                c("torch", "pkgload", "digest", "withr")) &&
        grepl("^[0-9a-f]{64}$", runtime$R_executable_sha256) &&
        identical(unname(runtime$thread_contract$torch_threads), c(1L, 1L)),
      "The runtime binding is incomplete")

checkpoint_dir <- tempfile("gs-v281-binding-")
checkpoint_path <- file.path(checkpoint_dir, "checkpoint.rds")
checkpoint_binding <- list(protocol_id = .gs_v281_protocol_id,
                           kind = "expected")
.gs_v281_save_atomic(list(value = 1), checkpoint_path, checkpoint_binding)
check(identical(.gs_v281_read_checkpoint(
  checkpoint_path, checkpoint_binding)$payload$value, 1),
  "An immutable checkpoint did not round trip")
unlink(paste0(checkpoint_path, ".sha256"))
recovered <- .gs_v281_read_checkpoint(checkpoint_path, checkpoint_binding)
check(identical(recovered$payload$value, 1) &&
        file.exists(paste0(checkpoint_path, ".sha256")),
      "A valid installed envelope with a missing sidecar was not recovered")
wrong <- checkpoint_binding; wrong$kind <- "foreign"
check(inherits(try(.gs_v281_read_checkpoint(
  checkpoint_path, wrong), silent = TRUE), "try-error"),
  "A checkpoint with a foreign binding was accepted")
unlink(checkpoint_dir, recursive = TRUE, force = TRUE)

check(identical(.gs_v281_polish_schedule$max_iter,
                c(100L, 400L, 500L, 1000L)) &&
        identical(.gs_v281_polish_schedule$learning_rate,
                  c(1, 1, 1, 0.25)) &&
        identical(.gs_v281_polish_max_iter, 2000L),
      "The staged L-BFGS schedule changed")
stage_body <- paste(deparse(body(.gs_v281_fit_one_stage)), collapse = "\n")
check(grepl("polish_tolerance_grad = 0", stage_body, fixed = TRUE) &&
        grepl("polish_tolerance_change = 0", stage_body, fixed = TRUE),
      "The internal absolute early-stop tolerances are not disabled")

map <- .gs_v281_affine_map(c(-1, 0, 1, 2), c(1, 3, 5, 7), rep(TRUE, 4))
check(abs(map$intercept - 3) < 1e-14 && abs(map$slope - 2) < 1e-14,
      "The affine input remap is incorrect")
new <- cbind(a = c(-1, 0, 1), b = c(1, 3, 5))
old <- cbind(a = 4 + 2 * new[, "a"], b = -1 + 0.5 * new[, "b"])
maps <- .gs_v281_affine_maps(new, old)
check(max(abs(maps$intercept - c(4, -1))) < 1e-14 &&
        max(abs(maps$slope - c(2, 0.5))) < 1e-14,
      "The multivariate affine input remap is incorrect")

nm <- c("a", "b", "c")
scale <- stats::setNames(c(2, 3, 4), nm)
check(identical(.gs_v281_canonical_coefficient_scale(scale, scale, nm),
                scale), "Matching coefficient scales were rejected")
bad <- scale; bad[[2L]] <- 3.1
check(inherits(try(.gs_v281_canonical_coefficient_scale(
  scale, bad, nm), silent = TRUE), "try-error"),
  "Mismatched coefficient scales were accepted")

relu_net <- sconjoint:::.sc_build_mixed_network(
  p = 1L, p_Z = 1L, q = 1L, hidden = 1L, mean_family = "relu",
  mu_bound = 1, kappa_bound = 10, alpha_bound = 0.9,
  a_bound = 10, weight_bound = 10, coefficient_scale = 1,
  compute_dtype = "float64")
relu_arrays <- lapply(relu_net$state_dict(), .gs_v281_tensor_array)
relu_arrays[["hidden.0.weight"]][] <- 1
relu_arrays[["hidden.0.bias"]][] <- 0
relu_arrays[["param_layer.weight"]][] <- 2
relu_arrays$alpha_raw[] <- 0.9
relu_net$load_state_dict(.gs_v281_arrays_to_warm(relu_arrays, relu_net))
relu_net$eval()
z0 <- matrix(0, 1L, 1L)
za <- matrix(0.8, 1L, 1L)
unbounded_shift <- as.numeric(
  .gs_v281_unbounded_relu_deviation(relu_net, za) -
    .gs_v281_unbounded_relu_deviation(relu_net, z0))
clipped_shift <- as.numeric(
  sconjoint:::.sc_predict_beta(relu_net, za) -
    sconjoint:::.sc_predict_beta(relu_net, z0))
check(abs(unbounded_shift - 1.6) < 1e-14 &&
        abs(clipped_shift - 0.1) < 1e-14,
      "The q=1 alpha shift was inferred from a clipped prediction")

a1 <- c(1, -2, 3, 0, 1, -1, 2)
a2 <- .gs_v281_orthogonal_column(a1)
check(abs(sum(a1 * a2)) < 1e-12 &&
        abs(sqrt(sum(a2^2)) / sqrt(sum(a1^2)) - 0.05) < 1e-12 &&
        identical(a2, .gs_v281_orthogonal_column(a1)),
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
  root, "applications", "gs2020", "R", "05_run_fast_v281.R"), warn = FALSE),
  collapse = "\n")
check(grepl(paste0(
  'if (outcome_command) .gs_v281_bootstrap_guard(repo_root, "v281")',
  "\n  .gs_v281_load_code(repo_root)"), runner_text, fixed = TRUE),
  "Outcome code can load before the clean hash-bound bootstrap guard")
check(grepl("prior_fit_seconds + 6.25 * sentinel_seconds", runner_text,
            fixed = TRUE) &&
        grepl("deferred_by_frozen_timing_rule", runner_text, fixed = TRUE),
      "The q=2 timing rule is missing or order-dependent")

source_root <- Sys.getenv("CONJOINTSTRUCTURAL_ROOT", unset = "")
if (nzchar(source_root)) {
  protocol <- .gs_v281_validate_protocol(
    root, source_root, require_external = TRUE, require_clean = FALSE)
  preflight <- .gs_v281_prepare_input(protocol, source_root)
  check(identical(dim(preflight$prepared$deltaX), c(20657L, 30L)) &&
          identical(dim(preflight$prepared$Z_primary), c(20657L, 22L)) &&
          length(unique(preflight$prepared$respondent_id)) == 1605L &&
          isFALSE(preflight$prior_full_fit_available),
        "The verified input does not match the frozen sample")
  nested <- readRDS(.gs_v281_input_path(
    protocol, source_root, "v21_primary_nested"))
  score_tamper <- nested
  names(score_tamper$tuning[[1L]]$sequence_scores[[1L]][[1L]])[[1L]] <-
    "__not_an_inner_validation_respondent__"
  check(inherits(try(.gs_v281_assert_historical_inner_tuning(
    preflight$prepared, score_tamper, preflight$fold_index), silent = TRUE),
    "try-error"),
  "An inner score detached from its validation respondent was accepted")
  preprocessing_tamper <- nested
  preprocessing_tamper$tuning[[1L]]$preprocessing_folds[[1L]]$
    training_respondents[[1L]] <- preprocessing_tamper$tuning[[1L]]$
      preprocessing_folds[[1L]]$validation_respondents[[1L]]
  check(inherits(try(.gs_v281_assert_historical_inner_tuning(
    preflight$prepared, preprocessing_tamper, preflight$fold_index),
    silent = TRUE), "try-error"),
  "A historical inner preprocessing partition was accepted after tampering")
  selection_tamper <- nested
  selection_tamper$tuning[[1L]]$candidate_selection_gate$selected_index <-
    1L + selection_tamper$tuning[[1L]]$selected_index
  check(inherits(try(.gs_v281_assert_historical_inner_tuning(
    preflight$prepared, selection_tamper, preflight$fold_index),
    silent = TRUE), "try-error"),
  "An inconsistent historical candidate selection was accepted")
  legacy_full <- .gs_v281_legacy_scope(protocol, source_root, "full")
  source_rows_full <- .gs_v281_scope_rows(preflight$prepared, legacy_full)
  alias_tamper <- legacy_full
  alias_tamper$preprocessing$deltaX$scale[[1L]] <-
    alias_tamper$preprocessing$deltaX$scale[[1L]] * 1.01
  check(inherits(try(.gs_v281_q1_initializer(
    alias_tamper, preflight,
    seq_len(nrow(preflight$prepared$deltaX)), source_rows_full),
    silent = TRUE), "try-error"),
  "A divergent fit-level/state preprocessing alias was accepted")
  reproduction <- vapply(.gs_v281_scopes, function(scope) {
    legacy <- .gs_v281_legacy_scope(protocol, source_root, scope)
    source_rows <- .gs_v281_scope_rows(preflight$prepared, legacy)
    fitting_rows <- if (scope == "full") {
      seq_len(nrow(preflight$prepared$deltaX))
    } else source_rows
    initialized <- .gs_v281_q1_initializer(
      legacy, preflight, fitting_rows, source_rows)
    if (scope != "full" && !identical(fitting_rows, source_rows)) {
      stop("An outer initializer used nontraining rows.", call. = FALSE)
    }
    initialized$valid_prediction_max_difference
  }, numeric(1L))
  check(all(is.finite(reproduction)) && max(reproduction) <= 1e-6,
        "An actual historical q=1 state failed affine/scale invariance")
}

cat(sprintf("Graham--Svolik v2.8.1 fit helper checks passed: %d\n", checks))
