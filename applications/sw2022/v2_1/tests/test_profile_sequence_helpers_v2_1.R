#!/usr/bin/env Rscript

## Read-only tests for profile-grid definitions, target projectors, provenance
## authorization, and the runner's descriptive-only interface.  No fit starts.

options(stringsAsFactors = FALSE)
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script <- normalizePath(sub("^--file=", "", .script_arg[[1L]]),
                         mustWork = TRUE)
.app <- normalizePath(file.path(dirname(.script), "..", ".."),
                      mustWork = TRUE)
.root <- normalizePath(file.path(.app, "..", ".."), mustWork = TRUE)
options(sconjoint.sw_application_root = .app)
.config_path <- file.path(
  .app, "v2_1", "config", "profile_sequence_config_v2_1.R")
.helper_path <- file.path(
  .app, "v2_1", "R", "profile_sequence_helpers_v2_1.R")
.runner_path <- file.path(
  .app, "v2_1", "R", "06_profile_sequence_likelihoods_v2_1.R")
source(.config_path, local = FALSE)
source(.helper_path, local = FALSE)

.expect_error <- function(code) {
  failed <- tryCatch({ force(code); FALSE }, error = function(e) TRUE)
  stopifnot(failed)
}

## Five-point grids are finite, ordered, and interior where required.
centers <- c(kappa = 0.2, female_vs_male_mean = 0.1,
             active_covariance_eigenvalue = 4,
             headline_contest_probability = 0.35)
for (direction in names(sw_v21_profile_config$grids)) {
  grid <- .sw_v21_profile_grid(
    centers[[direction]], direction, sw_v21_profile_config)
  stopifnot(length(grid) == 5L, !anyDuplicated(grid), all(is.finite(grid)))
}
stopifnot(all(.sw_v21_profile_grid(
  centers[["active_covariance_eigenvalue"]],
  "active_covariance_eigenvalue", sw_v21_profile_config) > 0))
.expect_error(.sw_v21_profile_inv_bounded_tanh(10, 10))
stopifnot(abs(.sw_v21_profile_inv_bounded_tanh(0.2, 10) -
                10 * atanh(0.02)) < 1e-14)

## Array metrics use one row per respondent and the fixed quadrature weights.
mu <- matrix(c(-0.2, 0.4, -0.1, 0.2), nrow = 2L, byrow = TRUE)
A <- matrix(c(0.3, -0.4), ncol = 1L)
U <- matrix(c(-1, 1), ncol = 1L)
w <- c(0.5, 0.5)
d <- c(-1, 0)
stopifnot(
  identical(.sw_v21_profile_metric_from_arrays(
    "kappa", mu, 0.3, A, U, w), 0.3),
  abs(.sw_v21_profile_metric_from_arrays(
    "female_vs_male_mean", mu, 0.3, A, U, w, d) - 0.15) < 1e-12,
  abs(.sw_v21_profile_metric_from_arrays(
    "active_covariance_eigenvalue", mu, 0.3, A, U, w) - 0.25) < 1e-12)
contest <- .sw_v21_profile_metric_from_arrays(
  "headline_contest_probability", mu, 0.3, A, U, w, d,
  position_neutral = TRUE)
stopifnot(is.finite(contest), contest > 0, contest < 1)

## Authorization binds config, generation hashes, pointer, manifest, runtime,
## and the complete reported-primary lock vector.
tmp <- tempfile("sw-v21-profile-auth-"); dir.create(tmp)
pointer_path <- file.path(tmp, "reported_primary_pointer.rds")
manifest_path <- file.path(tmp, "manifest.rds")
input_path <- file.path(tmp, "input.txt")
saveRDS(list(primary = TRUE), pointer_path)
saveRDS(list(complete = TRUE), manifest_path)
writeLines("frozen", input_path)
generation_md5 <- stats::setNames(
  unname(tools::md5sum(input_path)), "input")
lock_paths <- c(pointer = pointer_path, manifest = manifest_path,
                input = input_path)
lock_md5 <- unname(tools::md5sum(lock_paths)); names(lock_md5) <- names(lock_paths)
runtime <- list(R = "test", torch = "test")
context <- list(
  pointer_path = pointer_path,
  pointer = list(reported_primary = "selected_procedure_q1"),
  lock_paths = lock_paths, lock_md5 = lock_md5)
authorization <- list(
  schema_version = "sw2022-v2.1-profile-sequence-authorization-v2",
  authorized = TRUE,
  purpose = "sw2022-v2.1-descriptive-penalized-criterion-profile-sequences",
  reviewed_by = "unit test", authorized_at_utc = "2026-08-24 UTC",
  config_version = sw_v21_profile_config$version,
  config_md5 = unname(tools::md5sum(.config_path)),
  generation_input_md5 = generation_md5,
  runtime_signature = runtime,
  reviewed_pointer_md5 = unname(tools::md5sum(pointer_path)),
  reviewed_manifest_md5 = unname(tools::md5sum(manifest_path)),
  reported_primary_lock_md5 = lock_md5,
  reported_primary = "selected_procedure_q1",
  acknowledged_outcome_informed = TRUE,
  acknowledged_descriptive_penalized_criterion_sequences = TRUE,
  acknowledged_formal_inference_unavailable = TRUE,
  acknowledged_no_lr_critical_values = TRUE,
  acknowledged_fixed_learner_tuning_sieve = TRUE,
  formal_inference_available = FALSE, outcome_blind = FALSE)
stopifnot(.sw_v21_profile_authorization_valid(
  authorization, sw_v21_profile_config, .config_path, generation_md5,
  runtime, context))
bad <- authorization; bad$acknowledged_no_lr_critical_values <- FALSE
stopifnot(!.sw_v21_profile_authorization_valid(
  bad, sw_v21_profile_config, .config_path, generation_md5,
  runtime, context))

## Start selection is fail-closed: there is no diagnostic fallback when all
## optimizers fail their gates.
.expect_error(.sw_v21_profile_select_eligible_start(list(
  list(gate_pass = FALSE, penalized_objective = -1.1),
  list(gate_pass = FALSE, penalized_objective = -1.0))))
selection <- .sw_v21_profile_select_eligible_start(list(
  list(gate_pass = TRUE, penalized_objective = -1.1),
  list(gate_pass = FALSE, penalized_objective = -0.5),
  list(gate_pass = TRUE, penalized_objective = -1.0)))
stopifnot(identical(selection$index, 3L),
          identical(selection$eligible, c(1L, 3L)),
          identical(selection$best$gate_pass, TRUE))

## Exercise all four exact target projectors at the completed primary state.
## Restoring and projecting a network is not a fit and creates no artifact.
if (!requireNamespace("pkgload", quietly = TRUE) ||
    !requireNamespace("torch", quietly = TRUE)) {
  stop("The project-local pkgload and torch packages are required.")
}
suppressPackageStartupMessages(pkgload::load_all(.root, quiet = TRUE))
pointer <- readRDS(sw_v21_profile_config$input$reported_primary_pointer)
fit <- readRDS(pointer$full_fit_path)$refit
prepared <- readRDS(sw_v21_profile_config$input$prepared)
state <- fit$network_state
first <- !duplicated(prepared$respondent_id)
apply_z <- get(".sc_apply_z_transform", envir = asNamespace("sconjoint"))
Z_fit <- apply_z(
  as.matrix(prepared$Z_primary[first, , drop = FALSE]),
  state$preprocessing$Z)
z_t <- torch::torch_tensor(Z_fit)
scale_t <- torch::torch_tensor(
  as.numeric(state$architecture$coefficient_scale))
U_t <- torch::torch_tensor(fit$integration_grid$U)
w_t <- torch::torch_tensor(fit$integration_grid$w)
female_t <- torch::torch_tensor(
  sw_v21_profile_config$grids$female_vs_male_mean$contrast)
contest_t <- torch::torch_tensor(
  sw_v21_profile_config$grids$headline_contest_probability$contrast)
targets <- list(
  list("kappa", fit$kappa + 0.03, NULL, FALSE),
  list("female_vs_male_mean", 0.16, female_t, FALSE),
  list("active_covariance_eigenvalue", sum(fit$A^2) * 0.8, NULL, FALSE),
  list("headline_contest_probability", 0.36, contest_t, TRUE))
for (target in targets) {
  net <- sconjoint::scmix_restore_network(state)
  projected <- .sw_v21_profile_project_constraint(
    net, target[[1L]], target[[2L]], z_t, scale_t, U_t, w_t,
    target[[3L]], target[[4L]], base_A_raw = fit$A)
  stopifnot(projected$pass, abs(projected$target_error) <= 2e-5)
}
net <- sconjoint::scmix_restore_network(state)
gradient_check <- .sw_v21_profile_projected_gradient(
  net,
  function() {
    value <- torch::torch_zeros(1L)
    for (parameter in net$parameters) {
      value <- value + torch::torch_sum(parameter^2)
    }
    value
  },
  function() net$get_kappa()$squeeze())
stopifnot(gradient_check$pass,
          is.finite(gradient_check$projected_gradient_norm),
          gradient_check$constraint_gradient_norm > 0)

## A checkpoint is consumable only when its selected start and every stored
## target/objective/gradient/bound/log-likelihood diagnostic agree.
checkpoint_ids <- c("a", "b")
checkpoint_spec <- list(name = "fixed-selected-q1")
checkpoint_context <- list(
  lock_md5 = lock_md5,
  pointer = list(reported_primary = "selected_procedure_q1"))
selected_diagnostic <- list(
  gate_pass = TRUE, target_attained = 0.200001,
  target_error = 0.000001, mean_sequence_loglik = -1.5,
  penalized_objective = -1.6, penalty = 0.1, bound_active = FALSE,
  last_relative_change = 1e-5,
  projected_gradient = list(projected_gradient_norm = 0.001))
checkpoint <- list(
  schema_version = "sw2022-v2.1-profile-point-v2",
  direction = "kappa", target = 0.2,
  selected_start = 1L,
  nuisance_reoptimization_gate_pass = TRUE,
  all_start_diagnostics = list(selected_diagnostic),
  sequence_loglik = stats::setNames(c(-1, -2), checkpoint_ids),
  mean_sequence_loglik = -1.5, penalized_objective = -1.6,
  penalty = 0.1, target_attained = 0.200001,
  target_error = 0.000001,
  last_relative_change = 1e-5,
  projected_gradient = list(
    pass = TRUE, projected_gradient_norm = 0.001,
    constraint_gradient_norm = 1, lagrange_multiplier = 0.2),
  bounds = list(mu_active = FALSE, alpha_active = FALSE,
                kappa_active = FALSE, a_active = FALSE,
                weight_active = FALSE),
  bound_active = FALSE, elapsed_seconds = 1,
  network_state = state,
  profile_specification = list(
    config_version = sw_v21_profile_config$version,
    direction = "kappa", target = 0.2,
    generation_input_md5 = generation_md5,
    runtime_signature = runtime, authorization_md5 = "auth",
    reported_primary_lock_md5 = lock_md5,
    reported_primary = "selected_procedure_q1",
    selected_specification = checkpoint_spec,
    learner_tuning_sieve_fixed = TRUE, retuning_performed = FALSE,
    penalized_nuisance_reoptimization = TRUE,
    artifact_kind = "descriptive penalized-criterion profile sequence",
    literal_likelihood_profile = FALSE,
    unpenalized_sequence_likelihood_overlay = TRUE,
    descriptive_only = TRUE,
    formal_inference_available = FALSE, formal_test = FALSE,
    likelihood_ratio_critical_values = FALSE, outcome_blind = FALSE))
valid_checkpoint <- function(x) .sw_v21_profile_checkpoint_valid(
  x, "kappa", 0.2, generation_md5, runtime, "auth",
  checkpoint_context, checkpoint_spec, checkpoint_ids,
  sw_v21_profile_config)
stopifnot(valid_checkpoint(checkpoint))
bad_checkpoint <- checkpoint
bad_checkpoint$nuisance_reoptimization_gate_pass <- FALSE
stopifnot(!valid_checkpoint(bad_checkpoint))
bad_checkpoint <- checkpoint
bad_checkpoint$sequence_loglik[[1L]] <- -1.2
stopifnot(!valid_checkpoint(bad_checkpoint))
bad_checkpoint <- checkpoint
bad_checkpoint$target_error <- 0.01
stopifnot(!valid_checkpoint(bad_checkpoint))
bad_checkpoint <- checkpoint
bad_checkpoint$projected_gradient$projected_gradient_norm <- 1
stopifnot(!valid_checkpoint(bad_checkpoint))
bad_checkpoint <- checkpoint
bad_checkpoint$penalized_objective <- -1.4
stopifnot(!valid_checkpoint(bad_checkpoint))
bad_checkpoint <- checkpoint
bad_checkpoint$last_relative_change <-
  sw_v21_profile_config$optimizer$opt_tol * 2
stopifnot(!valid_checkpoint(bad_checkpoint))

## The script advertises and mechanically requires the reviewed launch flag,
## fixed tuning, nuisance reoptimization, and no LR inference.
runner <- paste(readLines(.runner_path, warn = FALSE), collapse = "\n")
stopifnot(
  grepl("--reviewed-launch=true", runner, fixed = TRUE),
  grepl(".sw_v21_profile_select_eligible_start(starts)", runner,
        fixed = TRUE),
  grepl("if (!direction_verified)", runner, fixed = TRUE),
  grepl("learner_tuning_sieve_fixed = TRUE", runner, fixed = TRUE),
  grepl("penalized_nuisance_reoptimization = TRUE", runner, fixed = TRUE),
  grepl("literal_likelihood_profile = FALSE", runner, fixed = TRUE),
  grepl("likelihood_ratio_critical_values = FALSE", runner, fixed = TRUE),
  grepl("formal_inference_available = FALSE", runner, fixed = TRUE))

cat("v2.1 profile-sequence helper tests passed; no fit was launched\n")
