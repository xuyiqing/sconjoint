#!/usr/bin/env Rscript

## Outcome-free focused tests for the Ballard--Rosa v2.8.1 application runner.

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
sys.source(file.path(root, "applications", "br2017", "R",
                     "br_v281_contract.R"), envir = environment())

checks <- 0L
check <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}

check(identical(.br_v281_set_rng_contract(), .br_v281_rng_kind) &&
        identical(unname(RNGkind()), .br_v281_rng_kind),
      "The v2.8.1 RNG-kind contract was not installed exactly")
.br_v281_set_thread_contract()
runtime_binding <- .br_v281_runtime_binding()
check(identical(runtime_binding$rng_kind, .br_v281_rng_kind) &&
        identical(names(runtime_binding$package_versions),
                  c("torch", "pkgload", "digest", "withr", "haven")) &&
        grepl("^[0-9a-f]{64}$", runtime_binding$R_executable_sha256) &&
        nzchar(runtime_binding$native_torch_version) &&
        identical(unname(runtime_binding$thread_contract$torch_threads),
                  c(1L, 1L)),
      "The v2.8.1 runtime binding is incomplete")

checkpoint_dir <- tempfile("br-v28-binding-")
checkpoint_path <- file.path(checkpoint_dir, "checkpoint.rds")
checkpoint_binding <- list(protocol_id = .br_v281_protocol_id,
                           kind = "expected")
.br_v281_save_atomic(list(value = 1), checkpoint_path, checkpoint_binding)
check(identical(.br_v281_read_checkpoint(
  checkpoint_path, checkpoint_binding, payload = FALSE)$binding,
  checkpoint_binding),
  "A hash-valid checkpoint did not preserve its verified binding")
wrong_binding <- checkpoint_binding
wrong_binding$kind <- "foreign"
check(inherits(try(.br_v281_read_checkpoint(
  checkpoint_path, wrong_binding, payload = FALSE), silent = TRUE),
  "try-error"),
  "A hash-valid checkpoint with a foreign binding was accepted")
unlink(checkpoint_dir, recursive = TRUE, force = TRUE)

check(identical(.br_v281_polish_schedule$max_iter,
                c(100L, 400L, 500L, 1000L)) &&
        identical(.br_v281_polish_schedule$learning_rate,
                  c(1, 1, 1, 0.25)) &&
        identical(.br_v281_polish_max_iter, 2000L),
      "The v2.8.1 staged optimizer schedule changed")
stage_body <- paste(deparse(body(.br_v281_fit_one_stage)), collapse = "\n")
check(grepl("polish_tolerance_grad = 0", stage_body, fixed = TRUE) &&
        grepl("polish_tolerance_change = 0", stage_body, fixed = TRUE) &&
        grepl("polish_max_iter = as.integer(max_iter)", stage_body,
              fixed = TRUE),
      "The staged optimizer call does not disable its internal early stops")

mock_stage <- list(
  polish_iterations = 17L, penalized_loss = 1.1, final_loss = 1,
  objective_before_polish = 1.2, objective_after_polish = 1.1,
  kkt_maximum = 9e-6, lbfgs_completed = TRUE,
  lbfgs_nonworsening = TRUE, line_search_status = "complete",
  polish = list(accepted = TRUE, projection_changed = FALSE))
mock_ledger <- .br_v281_stage_ledger_row(
  mock_stage, 1L, 100L, 1, 100L, 17L, "input-state", "mock-state")
check(nrow(mock_ledger) == 1L && mock_ledger$kkt_stop_reached &&
        mock_ledger$accepted && mock_ledger$projection_free &&
        mock_ledger$cumulative_configured_max == 100L &&
        mock_ledger$cumulative_reported_iterations == 17L,
      "The per-stage optimizer ledger does not encode its release rule")
rejected_stage <- mock_stage
rejected_stage$polish_iterations <- NA_integer_
rejected_stage$lbfgs_completed <- FALSE
rejected_stage$lbfgs_nonworsening <- FALSE
rejected_stage$polish$accepted <- FALSE
rejected_stage$polish$projection_changed <- NA
rejected_ledger <- .br_v281_stage_ledger_row(
  rejected_stage, 2L, 400L, 1, 500L, NA_integer_, "mock-state",
  "fallback-state")
check(!rejected_ledger$accepted && !rejected_ledger$kkt_stop_reached &&
        is.na(rejected_ledger$actual_iterations) &&
        rejected_ledger$cumulative_configured_max == 500L,
      "A rejected stage did not retain an auditable continuation ledger")

snapshot_root <- tempfile("br-v28-prior-tree-")
dir.create(file.path(snapshot_root, "runs", "br2017", "v26"),
           recursive = TRUE)
prior_file <- file.path(snapshot_root, "runs", "br2017", "v26",
                        "sealed-checkpoint.sha256")
writeLines("sealed", prior_file, useBytes = TRUE)
prior_snapshot <- .br_v281_v26_checkpoint_snapshot(snapshot_root)
check(nrow(prior_snapshot) == 1L &&
        identical(prior_snapshot$relative_path,
                  "runs/br2017/v26/sealed-checkpoint.sha256") &&
        isTRUE(.br_v281_assert_v26_checkpoint_snapshot(
          snapshot_root, prior_snapshot)),
      "The v2.6 checkpoint snapshot is incomplete")
writeLines("changed", prior_file, useBytes = TRUE)
check(inherits(try(.br_v281_assert_v26_checkpoint_snapshot(
  snapshot_root, prior_snapshot), silent = TRUE), "try-error"),
  "A changed v2.6 checkpoint was not rejected")
unlink(snapshot_root, recursive = TRUE, force = TRUE)

stopped_v28 <- .br_v281_v28_checkpoint_snapshot(root)
check(nrow(stopped_v28) == 2L &&
        identical(stopped_v28$relative_path, c(
          "runs/br2017/v28/prospective-2026-09-07-v1/preflight.rds",
          "runs/br2017/v28/prospective-2026-09-07-v1/preflight.rds.sha256")) &&
        isTRUE(.br_v281_assert_v28_checkpoint_snapshot(root, stopped_v28)),
      "The stopped v2.8 pre-estimation preflight was not sealed exactly")

affine <- .br_v281_affine_map(
  x_new = c(-1, 0, 1, 2), x_old = c(1, 3, 5, 99),
  eligible = c(TRUE, TRUE, TRUE, FALSE))
check(abs(affine$intercept - 3) < 1e-14 &&
        abs(affine$slope - 2) < 1e-14 && affine$n_eligible == 3L,
      "Affine work/luck reparameterization is incorrect")

scale_names <- c("a", "b", "c")
preprocessing_scale <- stats::setNames(c(2, 3, 4), scale_names)
architecture_scale <- preprocessing_scale
canonical_scale <- .br_v281_canonical_coefficient_scale(
  preprocessing_scale, architecture_scale, scale_names)
check(identical(canonical_scale, preprocessing_scale),
      "Matching loading-scale metadata did not produce one canonical scale")
bad_architecture_scale <- architecture_scale
bad_architecture_scale[[2L]] <- bad_architecture_scale[[2L]] * 1.01
check(inherits(try(.br_v281_canonical_coefficient_scale(
  preprocessing_scale, bad_architecture_scale, scale_names), silent = TRUE),
  "try-error"),
  "Mismatched preprocessing and architecture scales were accepted")

a1 <- c(1, -2, 3, 0, 1, -1, 2)
a2 <- .br_v281_orthogonal_column(a1)
check(length(a2) == length(a1) && all(is.finite(a2)) &&
        abs(sum(a1 * a2)) < 1e-12 &&
        abs(sqrt(sum(a2^2)) / sqrt(sum(a1^2)) - 0.05) < 1e-12,
      "The q=2 second loading is not deterministic and orthogonal")
check(identical(a2, .br_v281_orthogonal_column(a1)),
      "The q=2 second loading is not deterministic")
check(inherits(try(.br_v281_orthogonal_column(rep(0, length(a1))),
                   silent = TRUE), "try-error") &&
        inherits(try(.br_v281_orthogonal_column(rep(1e-12, length(a1))),
                     silent = TRUE), "try-error"),
      "An essentially zero q=1 loading received a non-relative q=2 column")

warm <- list(
  alpha_raw = torch::torch_tensor(c(1, 2), dtype = torch::torch_float64()),
  mu_bound_internal = torch::torch_tensor(c(10, 10),
                                           dtype = torch::torch_float64()),
  `hidden.0.weight` = torch::torch_tensor(matrix(1:4, 2, 2),
                                          dtype = torch::torch_float64()),
  `hidden.0.bias` = torch::torch_tensor(c(1, -1),
                                        dtype = torch::torch_float64()),
  `param_layer.weight` = torch::torch_tensor(matrix(1:4, 2, 2),
                                             dtype = torch::torch_float64()))
constant <- .br_v281_constant_warm(warm)
check(all(vapply(constant[c("hidden.0.weight", "hidden.0.bias",
                            "param_layer.weight")], function(x) {
  all(torch::as_array(x) == 0)
}, logical(1L))) && all(torch::as_array(constant$alpha_raw) == c(1, 2)),
"The nested constant initializer changed structural coordinates")

p1 <- .br_v281_perturb_warm(warm, 9123L)
p2 <- .br_v281_perturb_warm(warm, 9123L)
check(all(vapply(names(p1), function(nm) {
  identical(torch::as_array(p1[[nm]]), torch::as_array(p2[[nm]]))
}, logical(1L))), "Perturbed starts are not deterministic")
check(identical(torch::as_array(p1$mu_bound_internal),
                torch::as_array(warm$mu_bound_internal)),
      "A deterministic perturbation modified a fixed buffer")

mock_starts <- data.frame(
  start_id = c("best", "inferior"),
  penalized_mean_nll = c(1, 1.1), objective_finite = c(TRUE, TRUE),
  dtype = c("float64", "float64"), lbfgs_completed = c(TRUE, TRUE),
  lbfgs_accepted = c(TRUE, TRUE), lbfgs_nonworsening = c(TRUE, TRUE),
  projection_diagnostic_complete = c(TRUE, TRUE),
  projection_changed = c(FALSE, FALSE),
  bound_activity = c(FALSE, FALSE),
  gradient_contract_pass = c(TRUE, TRUE),
  kkt_maximum = c(1e-6, 1e-4), stringsAsFactors = FALSE)
mock_selection <- sconjoint:::.scmix_v28_start_surface(mock_starts)
check(identical(mock_selection$selected_start_id, "best") &&
        mock_selection$fit_gate_pass && mock_selection$n_near == 1L &&
        mock_selection$surface$inferior_start_diagnostic[[2L]],
      "An inferior start incorrectly fails or enters the near-optimum set")
good_constant <- list(
  penalized_loss = 1, final_loss = 0.9, kkt_maximum = 1e-6,
  dtype = "float64", lbfgs_completed = TRUE, lbfgs_nonworsening = TRUE,
  polish = list(accepted = TRUE, projection_changed = FALSE),
  bound_diagnostics_complete = TRUE, bound_activity = FALSE)
good_gradient <- list(overall_pass = TRUE)
check(.br_v281_is_float64("float64") &&
        .br_v281_is_float64("torch.float64") &&
        !.br_v281_is_float64("float32"),
      "The float64 gate accepts or rejects an incorrect dtype label")
check(.br_v281_constant_gate(good_constant, good_gradient) &&
        !.br_v281_constant_gate(
          within(good_constant, kkt_maximum <- 1e-3), good_gradient) &&
        !.br_v281_constant_gate(
          within(good_constant, lbfgs_completed <- FALSE), good_gradient) &&
        !.br_v281_constant_gate(
          within(good_constant, polish$projection_changed <- NA),
          good_gradient) &&
        !.br_v281_constant_gate(good_constant, list(overall_pass = FALSE)),
      "The nested constant reference does not fail closed")
missing_projection <- good_constant
missing_projection$polish$projection_changed <- NA
projection_starts <- list(good = good_constant, missing = missing_projection)
projection_gradients <- list(good = good_gradient, missing = good_gradient)
projection_table <- .br_v281_start_table(
  projection_starts, projection_gradients)
projection_surface <- sconjoint:::.scmix_v28_start_surface(projection_table)
check(!projection_table$projection_diagnostic_complete[[2L]] &&
        !projection_surface$surface$selection_eligible[[2L]],
      "A missing projection diagnostic was treated as projection-free")

td <- tempfile("br-v28-atomic-")
dir.create(td)
path <- file.path(td, "checkpoint.rds")
binding <- list(protocol_id = .br_v281_protocol_id, q = 1L)
.br_v281_save_atomic(list(message = "test"), path, binding)
got <- .br_v281_read_checkpoint(path, binding)
check(identical(got$payload$message, "test") &&
        file.exists(paste0(path, ".sha256")),
      "Atomic checkpoint round trip failed")
missing_binding <- c(binding, list(scope = "full"))
check(inherits(try(.br_v281_read_checkpoint(path, missing_binding),
                   silent = TRUE), "try-error"),
      "A checkpoint missing a requested binding field was accepted")
check(inherits(try(.br_v281_save_atomic(list(), path, binding), silent = TRUE),
               "try-error"), "Atomic checkpoint overwrite was not rejected")

unlink(td, recursive = TRUE, force = TRUE)
runner_text <- paste(readLines(file.path(
  root, "applications", "br2017", "R", "09_run_fast_v281.R"), warn = FALSE),
  collapse = "\n")
check(grepl("prior_fit_seconds + 6.25 * sentinel_seconds", runner_text,
            fixed = TRUE) &&
        !grepl("elapsed_from_preflight_seconds", runner_text, fixed = TRUE) &&
        grepl("available_ranks", runner_text, fixed = TRUE) &&
        grepl("deferred_by_frozen_timing_rule", runner_text, fixed = TRUE),
      "The q=2 timing stop is order-dependent or blocks required analysis")
check(grepl(paste0(
  'if (outcome_command) .br_v281_bootstrap_guard(repo_root, "v281")',
  "\n  .br_v281_load_code(repo_root)"), runner_text, fixed = TRUE),
  "Outcome code can load before the clean hash-bound bootstrap guard")
check(grepl("if (outcome_command && !nzchar(source_root))", runner_text,
            fixed = TRUE) &&
        grepl("require_external = outcome_command", runner_text,
              fixed = TRUE),
      "The read-only status command unnecessarily requires external inputs")
fit_constructor <- paste(deparse(body(.br_v281_make_fit)), collapse = "\n")
check(grepl("selected_kkt$maximum_block_rms", fit_constructor, fixed = TRUE) &&
        grepl("v28_all_start_finiteness_audit", fit_constructor,
              fixed = TRUE) &&
        !grepl("all(is.finite(c(penalized_objective", fit_constructor,
               fixed = TRUE),
      "Inferior starts still control the selected-fit finiteness gate")
cat(sprintf("Ballard--Rosa v2.8.1 focused helper checks passed: %d\n", checks))
