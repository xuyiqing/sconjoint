#!/usr/bin/env Rscript

## Outcome-free focused tests for the Ballard--Rosa v2.6 application runner.

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
                     "br_v26_contract.R"), envir = environment())

checks <- 0L
check <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}

affine <- .br_v26_affine_map(
  x_new = c(-1, 0, 1, 2), x_old = c(1, 3, 5, 99),
  eligible = c(TRUE, TRUE, TRUE, FALSE))
check(abs(affine$intercept - 3) < 1e-14 &&
        abs(affine$slope - 2) < 1e-14 && affine$n_eligible == 3L,
      "Affine work/luck reparameterization is incorrect")

a1 <- c(1, -2, 3, 0, 1, -1, 2)
a2 <- .br_v26_orthogonal_column(a1)
check(length(a2) == length(a1) && all(is.finite(a2)) &&
        abs(sum(a1 * a2)) < 1e-12 &&
        abs(sqrt(sum(a2^2)) / sqrt(sum(a1^2)) - 0.05) < 1e-12,
      "The q=2 second loading is not deterministic and orthogonal")
check(identical(a2, .br_v26_orthogonal_column(a1)),
      "The q=2 second loading is not deterministic")

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
constant <- .br_v26_constant_warm(warm)
check(all(vapply(constant[c("hidden.0.weight", "hidden.0.bias",
                            "param_layer.weight")], function(x) {
  all(torch::as_array(x) == 0)
}, logical(1L))) && all(torch::as_array(constant$alpha_raw) == c(1, 2)),
"The nested constant initializer changed structural coordinates")

p1 <- .br_v26_perturb_warm(warm, 9123L)
p2 <- .br_v26_perturb_warm(warm, 9123L)
check(all(vapply(names(p1), function(nm) {
  identical(torch::as_array(p1[[nm]]), torch::as_array(p2[[nm]]))
}, logical(1L))), "Perturbed starts are not deterministic")
check(identical(torch::as_array(p1$mu_bound_internal),
                torch::as_array(warm$mu_bound_internal)),
      "A deterministic perturbation modified a fixed buffer")

mock_starts <- list(
  lower_unpenalized = list(penalized_loss = 2, final_loss = 0.8),
  lower_penalized = list(penalized_loss = 1, final_loss = 0.9))
check(identical(.br_v26_select_start(mock_starts), 2L),
      "Start selection did not minimize the penalized training objective")
good_constant <- list(
  penalized_loss = 1, final_loss = 0.9, kkt_maximum = 1e-6,
  dtype = "float64", lbfgs_completed = TRUE, lbfgs_nonworsening = TRUE,
  bound_diagnostics_complete = TRUE, bound_activity = FALSE)
check(.br_v26_is_float64("float64") &&
        .br_v26_is_float64("torch.float64") &&
        !.br_v26_is_float64("float32"),
      "The float64 gate accepts or rejects an incorrect dtype label")
check(.br_v26_constant_gate(good_constant) &&
        !.br_v26_constant_gate(within(good_constant, kkt_maximum <- 1e-3)) &&
        !.br_v26_constant_gate(within(good_constant,
                                      lbfgs_completed <- FALSE)),
      "The nested constant reference does not fail closed")

dml_body <- paste(deparse(body(.br_v26_run_dml_family)), collapse = "\n")
check(grepl('riesz_rule = "fixed_n_train_minus_three_quarters"', dml_body,
            fixed = TRUE) && grepl("nu_grid = NULL", dml_body, fixed = TRUE) &&
        grepl("multiplier_draws = 9999L", dml_body, fixed = TRUE) &&
        grepl('riesz_numerical_rank_rule = "dimension_epsilon"', dml_body,
              fixed = TRUE),
      "The rapid q=1 DML adapter changed its fixed ridge or multiplier rule")

td <- tempfile("br-v26-atomic-")
dir.create(td)
path <- file.path(td, "checkpoint.rds")
binding <- list(protocol_id = .br_v26_protocol_id, q = 1L)
.br_v26_save_atomic(list(message = "test"), path, binding)
got <- .br_v26_read_checkpoint(path, binding)
check(identical(got$payload$message, "test") &&
        file.exists(paste0(path, ".sha256")),
      "Atomic checkpoint round trip failed")
missing_binding <- c(binding, list(scope = "full"))
check(inherits(try(.br_v26_read_checkpoint(path, missing_binding),
                   silent = TRUE), "try-error"),
      "A checkpoint missing a requested binding field was accepted")
check(inherits(try(.br_v26_save_atomic(list(), path, binding), silent = TRUE),
               "try-error"), "Atomic checkpoint overwrite was not rejected")

unlink(td, recursive = TRUE, force = TRUE)
cat(sprintf("Ballard--Rosa v2.6 focused helper checks passed: %d\n", checks))
