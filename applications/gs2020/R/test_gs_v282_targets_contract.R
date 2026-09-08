#!/usr/bin/env Rscript

## Outcome-free focused tests for the Graham--Svolik v2.8.2 target stage.

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
                     "gs_v282_contract.R"), envir = environment())
sys.source(file.path(root, "applications", "R",
                     "targeted_q1_inference.R"), envir = environment())
sys.source(file.path(root, "applications", "gs2020", "R",
                     "gs_v282_targets_contract.R"), envir = environment())

checks <- 0L
check <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
  checks <<- checks + 1L
  invisible(TRUE)
}

check(identical(.gs_v282_set_rng_contract(), .gs_v282_rng_kind),
      "The target stage did not install the frozen RNG-kind contract")
.gs_v282_set_thread_contract()

protocol <- .gs_v282t_validate_protocol(root, require_clean = FALSE)
registries <- .gs_v282t_registries(root, protocol)
expected_ids <- c(
  "mean_coparty_utility", "choice_coparty_vote_restrictor",
  "share_oppose_vote_restriction",
  "compensating_coparty_vote_restriction",
  "heterogeneity_vote_restriction")
check(identical(registries$targets$target_id, expected_ids) &&
        identical(registries$families$family_id,
                  c("context_preferences", "heterogeneity_core")),
      "The five-target core or its two families changed")

target_adapter <- paste(deparse(body(.gs_v282t_run_dml_target)),
                        collapse = "\n")
family_adapter <- paste(deparse(body(.gs_v282t_run_dml_family)),
                        collapse = "\n")
check(grepl('riesz_rule = "select"', target_adapter, fixed = TRUE) &&
        grepl("nu_grid = NULL", target_adapter, fixed = TRUE) &&
        grepl(paste0("riesz_validation_fraction = ",
                     ".gs_v282t_riesz_validation_fraction"), target_adapter,
              fixed = TRUE) &&
        grepl('riesz_numerical_rank_rule = "dimension_epsilon"',
              target_adapter, fixed = TRUE) &&
        grepl("allow_numeric_derivatives = FALSE", target_adapter,
              fixed = TRUE) &&
        grepl("derivative_refinement_tolerance = 1e-04", target_adapter,
              fixed = TRUE) &&
        grepl("rank_tolerance = .gs_v282t_loading_rank_tolerance",
              target_adapter, fixed = TRUE) &&
        grepl("multiplier_draws = 0L", target_adapter, fixed = TRUE) &&
        grepl("bundle$rows$target_index", family_adapter, fixed = TRUE),
      "Target-specific DML, training-only ridge, or seed rules changed")

mock_parent <- list(
  protocol_id = .gs_v282t_parent_protocol_id,
  protocol_hash = paste(rep("b", 64L), collapse = ""),
  git = list(commit = paste(rep("c", 40L), collapse = "")),
  inputs = data.frame(input_id = "prepared", sha256 =
    paste(rep("d", 64L), collapse = ""), stringsAsFactors = FALSE))
old_run_commit <- paste(rep("a", 40L), collapse = "")
mock_upstream <- list(binding = list(
  protocol_id = mock_parent$protocol_id,
  protocol_hash = mock_parent$protocol_hash,
  git_commit = old_run_commit,
  application = "gs2020", kind = "assembled", q = 1L, scope = "all",
  runtime = .gs_v282_runtime_binding(),
  rng_kind = .gs_v282_assert_rng_contract(),
  input_hashes = stats::setNames(mock_parent$inputs$sha256,
                                 mock_parent$inputs$input_id)))
check(identical(.gs_v282t_assert_upstream_binding(
  mock_upstream, mock_parent, "assembled", 1L, "all"), old_run_commit),
  "An intact upstream checkpoint from the fit commit was rejected")
bad_upstream <- mock_upstream
bad_upstream$binding$protocol_hash <- paste(rep("e", 64L), collapse = "")
check(inherits(try(.gs_v282t_assert_upstream_binding(
  bad_upstream, mock_parent, "assembled", 1L, "all"), silent = TRUE),
  "try-error"), "A checkpoint with the wrong protocol hash was accepted")

respondents <- sprintf("r%02d", 1:12)
synthetic_preflight <- list(prepared = list())
attr_names <- c(
  "diff_respParty", "diff_p1_num", "diff_p2_num",
  "diff_dem_code_g_committee", "diff_dem_code_g_officestructure",
  "diff_dem_code_g_procedure", "diff_dem_code_g_progEval",
  "diff_dem_code_g_record", "diff_dem_code_g_schedule",
  "diff_dem_code_u_banProtest", "diff_dem_code_u_court",
  "diff_dem_code_u_execRule", "diff_dem_code_u_gerry2",
  "diff_dem_code_u_gerry10", "diff_dem_code_u_journalists",
  "diff_dem_code_u_limitVote", "diff_dem_code_v_affair",
  "diff_dem_code_v_tax", "diff_sex_Female", "diff_race_Asian",
  "diff_race_Black", "diff_race_Hispanic", "diff_pro_Farmer",
  "diff_pro_Lawyer", "diff_pro_Legislative_staffer",
  "diff_pro_Police_officer", "diff_pro_Served_in_the_army",
  "diff_pro_Served_in_the_navy", "diff_pro_Small_business_owner",
  "diff_pro_Teacher")
synthetic_fit <- list(attr_names = attr_names, respondent_id = respondents)
families <- lapply(registries$families$family_id, function(id) {
  .gs_v282t_build_family(id, synthetic_fit, synthetic_preflight, registries)
})
names(families) <- registries$families$family_id
check(sum(vapply(families, function(x) length(x$targets), integer(1L))) == 5L &&
        all(vapply(families, function(x) {
          identical(names(x$targets), x$rows$target_id) &&
            identical(names(x$recipes), x$rows$target_id)
        }, logical(1L))),
      "The analytic builders do not exactly cover the five-target core")
heterogeneity <- families$heterogeneity_core
check(identical(names(heterogeneity$primitive_registry),
                "heterogeneity_vote_restriction") &&
        inherits(heterogeneity$primitive_registry[[1L]],
                 "scmix_v22_target_spec") &&
        identical(heterogeneity$recipes$
          heterogeneity_vote_restriction$primitive_names,
          paste0("heterogeneity_vote_restriction", c(":H_Z", ":H_R"))),
      "Directional heterogeneity does not use its direct H_Z/H_R primitives")

N <- length(respondents); p <- length(attr_names)
mu <- matrix(seq_len(N * p) / 100, N, p)
Sigma <- diag(seq(0.2, 0.5, length.out = p))
all_targets <- c(families$context_preferences$targets,
                 families$heterogeneity_core$targets)
evaluated <- lapply(all_targets, function(fun) fun(
  mu = mu, kappa = 0.1, Sigma = Sigma, Z = matrix(0, N, 1L),
  respondent_id = respondents, fold = 1L, attr_names = attr_names))
check(all(vapply(evaluated, function(x) {
  is.matrix(x$value) && nrow(x$value) == N &&
    is.array(x$d_mu) && dim(x$d_mu)[1L] == N &&
    is.matrix(x$d_kappa) && nrow(x$d_kappa) == N
}, logical(1L))), "A core target lacks analytic rowwise derivatives")

finite_difference <- function(theta, kind, j, eps = 1e-6) {
  up <- down <- theta
  up[j] <- up[j] + eps; down[j] <- down[j] - eps
  (.gs_v282t_geometry(up, kind)$value -
     .gs_v282t_geometry(down, kind)$value) / (2 * eps)
}
geometry_cases <- list(
  subgroup_ratio = c(0.2, 0.4),
  heterogeneity_share = c(0.3, 0.2), mrs = c(0.4, -0.3))
check(all(vapply(names(geometry_cases), function(kind) {
  theta <- geometry_cases[[kind]]
  analytic <- .gs_v282t_geometry(theta, kind)$gradient
  numeric <- vapply(seq_along(theta), function(j)
    finite_difference(theta, kind, j), numeric(1L))
  max(abs(analytic - numeric)) < 1e-6
}, logical(1L))), "A smooth reporting geometry has an incorrect derivative")
check(!.gs_v282t_geometry(
        1.01, "identity", "directional_share")$gate &&
        !.gs_v282t_geometry(
          -0.01, "identity", "choice_probability")$gate &&
        .gs_v282t_geometry(
          1.01, "identity", "mean")$gate,
      "Probability/share domain checks are not target-type aware")

hz_id <- "heterogeneity_vote_restriction"
hz_recipe <- heterogeneity$recipes[hz_id]
hz_primitives <- c(
  stats::setNames(c(0.25, 0.75), paste0(hz_id, c(":H_Z", ":H_R"))),
  stats::setNames(c(1, 4, 0.75), paste0(
    ".v28_support:", hz_id,
    c(":mean", ":second_moment", ":residual_variance"))))
hz_reported <- .gs_v282t_reported_from_primitives(hz_primitives, hz_recipe)
check(isTRUE(all.equal(unname(hz_reported), 0.8, tolerance = 1e-14)),
      paste("Cross-fit directional heterogeneity did not use the global",
            "finite-population variance assembled from support moments"))

mock_dml <- function(id, estimate) {
  rid <- paste0("m", seq_len(20L))
  influence <- matrix(seq(-1, 1, length.out = 20L), ncol = 1L,
                      dimnames = list(rid, id))
  influence <- sweep(influence, 2L, colMeans(influence), `-`)
  structure(list(
    estimate = stats::setNames(estimate, id),
    plugin_estimate = stats::setNames(estimate - 0.01, id),
    influence = influence, respondent_id = rid,
    fold_details = replicate(5L, list(
      honest_information_scaled_riesz_defect = stats::setNames(0.1, id),
      riesz_equation_relative_residual = stats::setNames(0.1, id),
      ridge_relative_sensitivity = stats::setNames(0.1, id)), simplify = FALSE),
    derivative_refinement_error = matrix(
      1e-8, 5L, 1L, dimnames = list(NULL, id)),
    honest_riesz_null_space_defect_max = stats::setNames(1e-8, id)),
    class = c("scmix_dml", "list"))
}
mock_ids <- c("a", "b")
mock_rows <- data.frame(target_index = 1:2, target_id = mock_ids,
                        target_type = "mean", stringsAsFactors = FALSE)
mock_bundle <- list(
  rows = mock_rows,
  recipes = stats::setNames(lapply(mock_ids, function(id)
    .gs_v282t_recipe(id, "mean", id, "identity")), mock_ids))
mock_collection <- structure(
  stats::setNames(list(mock_dml("a", 0.3), mock_dml("b", 0.4)), mock_ids),
  class = c("gs_v282_target_dml_collection", "list"))
mock_transformed <- .gs_v282t_transform_dml_collection(
  mock_collection, mock_bundle, fit_gate_pass = TRUE)
check(identical(mock_transformed$summary$target_id, mock_ids) &&
        identical(colnames(mock_transformed$influence), mock_ids) &&
        all(mock_transformed$summary$primitive_diagnostic_gate_pass) &&
        all(mock_transformed$summary$
          riesz_correction_relative_sensitivity_max == 0.1) &&
        all(is.na(mock_transformed$summary$ridge_movement_se)) &&
        all(!mock_transformed$summary$ridge_neighborhood_gate_pass) &&
        all(!mock_transformed$summary$fast_diagnostic_gate_pass) &&
        all(!mock_transformed$summary$formal_release),
      "Target-specific DML results were not safely reassembled by family")

original_target_runner <- .gs_v282t_run_dml_target
.gs_v282t_run_dml_target <- function(fit, basis, target_id, target, seed,
                                    primitive_spec = NULL) {
  if (identical(target_id, "b")) stop("synthetic target failure")
  mock_dml(target_id, 0.3)
}
isolation_bundle <- mock_bundle
isolation_bundle$targets <- stats::setNames(
  lapply(mock_ids, function(id) function(...) NULL), mock_ids)
isolated <- .gs_v282t_run_dml_family(
  fit = NULL, basis = NULL, bundle = isolation_bundle, seed = 100L)
.gs_v282t_run_dml_target <- original_target_runner
check(inherits(isolated$a, "scmix_dml") &&
        inherits(isolated$b, "gs_v282_target_dml_error") &&
        grepl("synthetic target failure", isolated$b$reason, fixed = TRUE),
      "One target-specific error discarded a successful family member")

## A tiny portable network verifies target-stability semantics without reading
## or generating any application outcome.
tiny_ids <- paste0("t", 1:4)
tiny_task_ids <- rep(tiny_ids, each = 2L)
tiny_Z <- matrix(rep(1:4, each = 2L), ncol = 1L,
                 dimnames = list(NULL, "z1"))
tiny_transform <- sconjoint:::.sc_fit_z_transform(tiny_Z, tiny_task_ids)
tiny_grid <- sconjoint:::.sc_mixed_grid(
  q = 1L, integration = "gh", n_nodes = 5L, n_draws = 1L,
  seed = 1L, antithetic = TRUE, scramble = FALSE)
tiny_state <- function(alpha) {
  net <- sconjoint:::.sc_build_mixed_network(
    p = 2L, p_Z = 1L, q = 1L, hidden = integer(),
    mean_family = "linear", coefficient_scale = c(1, 1),
    compute_dtype = "float64")
  torch::with_no_grad({
    for (nm in names(net$parameters)) net$parameters[[nm]]$zero_()
    net$parameters$alpha_raw$copy_(torch::torch_tensor(
      c(alpha, 0), dtype = torch::torch_float64()))
  })
  sconjoint:::.scmix_capture_network_state(
    net, p = 2L, p_Z = 1L, q = 1L, hidden = integer(),
    mean_family = "linear", mu_bound = 10, kappa_bound = 10,
    alpha_bound = 5, a_bound = 10, weight_bound = 10,
    coefficient_scale = c(x1 = 1, x2 = 1),
    z_transform = tiny_transform,
    dx_transform = list(scale = c(x1 = 1, x2 = 1)),
    coefficient_names = c("x1", "x2"),
    moderator_names = "z1", integration_grid = tiny_grid,
    analysis_signature = "synthetic-v28-stability", scope = "synthetic")
}
tiny_record <- function(id, alpha, selected, gap, eligible = TRUE) {
  list(
    start_id = id, selected = selected, selection_eligible = eligible,
    target_comparison_eligible = eligible && gap <= 1e-6,
    relative_objective_gap = gap, network_state = tiny_state(alpha),
    A_raw = matrix(c(0.2, 0.1), 2L, 1L), kappa = 0,
    moderator_preprocessing = list(
      imputation = "none", transform_hash = "synthetic-transform"))
}
selected_record <- tiny_record("selected", 0, TRUE, 0)
near_record <- tiny_record("near", 0.005, FALSE, 5e-7)
tiny_records <- list(selected = selected_record, near = near_record)
tiny_assembled <- structure(list(
  respondent_id = tiny_task_ids,
  fold_id = rep(c(1L, 1L, 2L, 2L), each = 2L),
  attr_names = c("x1", "x2"), q = 1L),
  class = c("scmix_nested_assembled", "list"))
tiny_preflight <- list(
  prepared = list(respondent_id = tiny_task_ids, Z_primary = tiny_Z))
tiny_target <- sconjoint::scmix_inference_target(
  type = "tau", contrast = c(1, 0), label = "mean_x1")
tiny_bundle <- list(
  rows = data.frame(target_index = 1L, target_id = "mean_x1",
                    target_type = "mean", stringsAsFactors = FALSE),
  targets = list(mean_x1 = tiny_target),
  recipes = list(mean_x1 = .gs_v282t_recipe(
    "mean_x1", "mean", "mean_x1", "identity")))
tiny_application <- list(
  assembled = tiny_assembled,
  multistart_artifacts_by_scope = list(
    full = tiny_records, outer_1 = tiny_records, outer_2 = tiny_records))
stability <- .gs_v282t_near_optimum_stability(
  tiny_application, tiny_preflight, tiny_bundle, c(mean_x1 = 0.1))
check(identical(stability$target_detail$gate_state, "passed") &&
        stability$target_detail$comparison_count == 3L &&
        stability$target_detail$near_optimum_selected_movement_se < 0.10,
      "Near-optimum target stability did not use the full and fold surfaces")
inferior_record <- tiny_record("inferior", 1, FALSE, 0.1)
singleton_records <- list(selected = selected_record,
                          inferior = inferior_record)
tiny_application$multistart_artifacts_by_scope <- list(
  full = singleton_records, outer_1 = singleton_records,
  outer_2 = singleton_records)
singleton <- .gs_v282t_near_optimum_stability(
  tiny_application, tiny_preflight, tiny_bundle, c(mean_x1 = 0.1))
check(identical(singleton$target_detail$gate_state,
                "not_applicable_singleton_near_set") &&
        singleton$excluded_inferior_count == 3L &&
        singleton$near_alternative_count == 0L,
      "Inferior starts were used or a singleton near set was called a pass")
target_runner_text <- paste(readLines(file.path(
  root, "applications", "gs2020", "R", "06_run_targets_v282.R"),
  warn = FALSE), collapse = "\n")
check(grepl(".gs_v282t_stability_evidence_complete(stability)",
            target_runner_text, fixed = TRUE),
      "A singleton near set is incorrectly removed from unavailable evidence")
mixed_stability <- list(
  status = "partially_evaluated",
  target_detail = data.frame(gate_pass = c(TRUE, NA)))
check(!.gs_v282t_stability_evidence_complete(mixed_stability),
      "Partial target-stability evidence was treated as complete")
complete_stability <- list(
  status = "failed",
  target_detail = data.frame(gate_pass = c(TRUE, FALSE)))
check(.gs_v282t_stability_evidence_complete(complete_stability),
      "Complete pass/fail target-stability evidence was not recognized")
check(grepl(paste0(
  "if (outcome_command) .gs_v282t_bootstrap_guard(repo_root)",
  "\n  .gs_v282t_load_code(repo_root)"), target_runner_text, fixed = TRUE),
  "Target code can load before the clean hash-bound bootstrap guard")

IF <- cbind(a = seq(-1, 1, length.out = 21),
            b = cos(seq(0, 2 * pi, length.out = 21)))
IF <- sweep(IF, 2L, colMeans(IF), `-`)
m1 <- .gs_v282t_family_multiplier(IF, c(a = 0.2, b = 0.4),
                                  seed = 9876L, draws = 99L,
                                  block_size = 17L)
m2 <- .gs_v282t_family_multiplier(IF, c(a = 0.2, b = 0.4),
                                  seed = 9876L, draws = 99L,
                                  block_size = 17L)
check(identical(m1$centered_draw_hash, m2$centered_draw_hash) &&
        identical(m1$included_targets, c("a", "b")) &&
        is.finite(m1$simultaneous_critical_value),
      "The common within-family multiplier is not deterministic")

td <- tempfile("gs-v282-target-checkpoint-")
dir.create(td)
path <- file.path(td, "family.rds")
binding <- list(protocol_id = .gs_v282t_protocol_id, family_id = "mock")
.gs_v282t_save_atomic(list(message = "test"), path, binding)
got <- .gs_v282t_read_checkpoint(path, binding)
check(identical(got$payload$message, "test") &&
        file.exists(paste0(path, ".sha256")),
      "Target checkpoint round trip failed")
unlink(paste0(path, ".sha256"))
recovered <- .gs_v282t_read_checkpoint(path, binding)
check(identical(recovered$payload$message, "test") &&
        file.exists(paste0(path, ".sha256")),
      "A valid target envelope with a missing sidecar was not recovered")
check(inherits(try(.gs_v282t_save_atomic(list(), path, binding), silent = TRUE),
               "try-error"), "An immutable target checkpoint was overwritten")
wrong <- readRDS(path); wrong$binding$family_id <- "foreign"
saveRDS(wrong, path, version = 3L)
writeLines(paste(.gs_v282t_sha256(path), basename(path)),
           paste0(path, ".sha256"), useBytes = TRUE)
check(inherits(try(.gs_v282t_read_checkpoint(
  path, binding, payload = FALSE), silent = TRUE), "try-error"),
  "Hash-valid but mismatched checkpoint metadata were accepted")
unlink(td, recursive = TRUE, force = TRUE)

runner_text <- paste(readLines(file.path(
  root, "applications", "gs2020", "R", "06_run_targets_v282.R"),
  warn = FALSE), collapse = "\n")
check(grepl(".gs_v282t_verify_upstream", runner_text, fixed = TRUE) &&
        !grepl('.gs_v282_checkpoint_path(run_dir, "assembled", 2',
               runner_text, fixed = TRUE) &&
        !grepl(".gs_v281", runner_text, fixed = TRUE) &&
        !grepl('"v281"', runner_text, fixed = TRUE) &&
        grepl("q2_used_for_target_selection = FALSE", runner_text,
              fixed = TRUE) &&
        grepl("prior_target_checkpoint_reuse = FALSE", runner_text,
              fixed = TRUE) &&
        grepl("v281_partial_checkpoint_reuse = FALSE", runner_text,
              fixed = TRUE),
      paste("The target runner does not preserve the fresh-v2.8.2 q=1-only",
            "information and checkpoint boundary"))

cat(sprintf("Graham--Svolik v2.8.2 target-stage checks passed: %d\n", checks))
