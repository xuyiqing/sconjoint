.v24_runner_plan <- function() {
  families <- c("smooth_means", "choice_probabilities",
                "directional_shares", "subgroup_ratios",
                "directional_heterogeneity", "mrs")
  cells <- paste0("cell", seq_len(6L))
  list(
    protocol_id = "paperps-inference-2026-09-06-v2.4",
    plan_id = "paperps-lean-validation-2026-09-06-v2.4-v1",
    cells = data.frame(cell_id = cells, design_id = "toy",
                       stringsAsFactors = FALSE),
    targets = data.frame(family_id = families,
                         stringsAsFactors = FALSE),
    counts = list(pilot_reps = 2L, evidence_reps = 100L,
                  batch_size = 10L, multipliers = 9999L,
                  maximum_projected_hours = 8L),
    approval = expand.grid(
      family_id = families, cell_id = cells,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
}

.v24_runner_hash <- function(letter) paste(rep(letter, 64L), collapse = "")

.v24_runner_evaluator <- function() list(
  sha256 = .v24_runner_hash("a"),
  settings = list(fingerprint = .v24_runner_hash("b")),
  raw = list(`Implementation-Manifest-SHA256` = .v24_runner_hash("c")))

.v24_runner_pilot_artifact <- function(
    run_dir, plan, evaluator, cell, replication, elapsed = 1) {
  payload <- sconjoint:::.scmix_v24_sign(list(
    protocol_id = plan$protocol_id, plan_id = plan$plan_id,
    stream = "pilot", cell_id = cell, replication = as.integer(replication),
    design_id = plan$cells$design_id[match(cell, plan$cells$cell_id)],
    evaluator_approval_sha256 = evaluator$sha256,
    evaluator_settings_fingerprint = evaluator$settings$fingerprint,
    implementation_manifest_sha256 =
      evaluator$raw[["Implementation-Manifest-SHA256"]],
    evaluation_elapsed_seconds = as.numeric(elapsed),
    synthetic_outcome = TRUE, application_outcomes_used = FALSE,
    completed = TRUE), "scmix_v24_replication_artifact")
  path <- sconjoint:::.scmix_v24_artifact_file(
    run_dir, list(stream = "pilot", cell_id = cell,
                  replication = replication))
  path <- sconjoint:::.scmix_v24_save_atomic(payload, path)
  list(
    artifact_file = path,
    artifact_sha256 = unname(as.character(tools::sha256sum(path))),
    elapsed_seconds = as.numeric(elapsed))
}

test_that("v2.4 raw-to-canonical primitive labels are not reversed", {
  labels <- c(
    "mean_direction", "mean_numerator", "mean_denominator",
    "choice_probability",
    "preference_direction_share:preference_direction_share",
    paste0("preference_direction_share:preference_direction_share:",
           "directional_variance"),
    "compensating_benefit_share:compensating_benefit_share",
    paste0("compensating_benefit_share:compensating_benefit_share:",
           "directional_variance"),
    "subgroup_high:choice_probability:numerator",
    "subgroup_high:choice_probability:denominator",
    "subgroup_low:choice_probability:numerator",
    "subgroup_low:choice_probability:denominator",
    "mrs:numerator", "mrs:denominator",
    "active_q1_covariance_eigenvalue")
  map <- sconjoint:::.scmix_v24_nonheterogeneity_map(labels)
  expect_identical(unname(map[[
    "subgroup_high:choice_probability:numerator"]]),
    "subgroup_high_numerator")
  expect_identical(unname(map[["mrs:denominator"]]), "mrs_denominator")
  expect_identical(unname(map[[
    "preference_direction_share:preference_direction_share"]]),
    "preference_direction_share")
  expect_identical(unname(map[[paste0(
    "preference_direction_share:preference_direction_share:",
    "directional_variance")]]), "preference_direction_variance")
  expect_true(all(names(map) %in% labels))
  expect_true(all(c("mean_direction", "active_q1_covariance_eigenvalue",
                    "subgroup_low_denominator",
                    "preference_direction_variance",
                    "compensating_benefit_variance") %in% unname(map)))
})

test_that("v2.4 heterogeneity Riesz primitives are exactly H_Z and H_R", {
  outcome <- list(
    deltaX = rbind(c(1, 0), c(0, 1)),
    deltaX_names = c("a", "b"), respondent_id = c("r1", "r2"),
    source_row_index = 1:2)
  geometry <- list(
    target_dual_raw = matrix(
      c(1, 0, 0, 1, 1, 1), nrow = 2L,
      dimnames = list(c("a", "b"),
                      c("direction", "numerator", "denominator"))),
    z_task = c(-1, 1))
  bundle <- sconjoint:::.scmix_v24_target_callbacks(
    outcome, geometry, n_nodes = 15L)
  expect_identical(bundle$heterogeneity$primitive_ids, c("H_Z", "H_R"))
  mu <- rbind(r1 = c(0.2, -0.1), r2 = c(-0.3, 0.4))
  colnames(mu) <- c("a", "b")
  share <- bundle$callbacks$preference_direction_share(
    mu, 0, diag(c(0.2, 0.1)), matrix(0, 2L, 1L), rownames(mu),
    1L, colnames(mu))
  expect_identical(
    share$labels,
    c("preference_direction_share",
      "preference_direction_share:directional_variance"))
  expect_identical(ncol(share$value), 2L)
})

test_that("v2.4 DML assembly maps raw labels to canonical result labels", {
  raw_by_analysis <- list(
    active_q1_covariance_eigenvalue = "active_q1_covariance_eigenvalue",
    mean_direction = "mean_direction",
    mean_numerator = "mean_numerator",
    mean_denominator = "mean_denominator",
    choice_probability = "choice_probability",
    preference_direction_share = c(
      "preference_direction_share:preference_direction_share",
      paste0("preference_direction_share:preference_direction_share:",
             "directional_variance")),
    compensating_benefit_share = c(
      "compensating_benefit_share:compensating_benefit_share",
      paste0("compensating_benefit_share:compensating_benefit_share:",
             "directional_variance")),
    subgroup_high_choice_share = c(
      "subgroup_high:x:numerator", "subgroup_high:x:denominator"),
    subgroup_low_choice_share = c(
      "subgroup_low:x:numerator", "subgroup_low:x:denominator"),
    mrs = c("mrs:numerator", "mrs:denominator"),
    directional_heterogeneity = c("H_Z", "H_R"))
  make_dml <- function(raw, estimate = seq_along(raw)) {
    fold <- list(
      derivative_refinement_error = stats::setNames(rep(0, length(raw)), raw),
      derivative_refinement_protocol_exact = stats::setNames(
        rep(TRUE, length(raw)), raw),
      honest_information_scaled_riesz_defect = stats::setNames(
        rep(0, length(raw)), raw),
      honest_riesz_null_space_defect = stats::setNames(
        rep(0, length(raw)), raw),
      ridge_neighborhood_artifact = stats::setNames(
        lapply(raw, function(label) list(label = label)), raw))
    structure(list(
      estimate = stats::setNames(estimate, raw),
      crossfit_plugin = stats::setNames(estimate / 10, raw),
      influence = matrix(
        seq_len(2L * length(raw)), 2L,
        dimnames = list(c("r1", "r2"), raw)),
      respondent_id = c("r1", "r2"), fold_details = list(fold)),
      class = c("scmix_dml", "list"))
  }
  local_mocked_bindings(
    scmix_dml = function(..., v22_registry = NULL) {
      marker <- unname(list(...)$plugin_targets[[1L]])
      raw <- raw_by_analysis[[marker]]
      if (identical(marker, "directional_heterogeneity")) {
        make_dml(raw, c(2, 3))
      } else {
        make_dml(raw)
      }
    },
    .package = "sconjoint")
  callbacks <- stats::setNames(
    as.list(c(
      "active_q1_covariance_eigenvalue", "mean_direction",
      "mean_numerator", "mean_denominator", "choice_probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_high_choice_share", "subgroup_low_choice_share", "mrs")),
    c("active_q1_covariance_eigenvalue", "mean_direction",
      "mean_numerator", "mean_denominator", "choice_probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_high", "subgroup_low", "mrs"))
  targets <- list(
    callbacks = callbacks,
    heterogeneity = list(
      plugin_targets = list("directional_heterogeneity"),
      primitive_registry = list(list()),
      primitive_ids = c("H_Z", "H_R")))
  result <- sconjoint:::.scmix_v24_dml_once(
    list(deltaX = matrix(0, 2L, 2L)), targets, seed = 1L)
  expect_true(all(c(
    "subgroup_high_numerator", "mrs_denominator",
    "preference_direction_share", "preference_direction_variance",
    "compensating_benefit_share", "compensating_benefit_variance",
    "heterogeneity_explained_variance",
    "heterogeneity_residual_variance") %in% names(result$estimate)))
  expect_identical(result$estimate[["heterogeneity_explained_variance"]], 2)
  expect_identical(result$heterogeneity_label_map,
                   c(H_Z = "heterogeneity_explained_variance",
                     H_R = "heterogeneity_residual_variance"))
})

test_that("v2.4 target-specific DML failure is isolated to its family", {
  raw_by_analysis <- list(
    active = "active_q1_covariance_eigenvalue",
    mean_direction = "mean_direction",
    mean_numerator = "mean_numerator",
    mean_denominator = "mean_denominator",
    choice_probability = "choice_probability",
    preference_direction_share = c(
      "preference_direction_share:preference_direction_share",
      "preference_direction_share:x:directional_variance"),
    compensating_benefit_share = c(
      "compensating_benefit_share:compensating_benefit_share",
      "compensating_benefit_share:x:directional_variance"),
    subgroup_high = c("subgroup_high:x:numerator",
                      "subgroup_high:x:denominator"),
    subgroup_low = c("subgroup_low:x:numerator",
                     "subgroup_low:x:denominator"),
    mrs = c("mrs:numerator", "mrs:denominator"),
    heterogeneity = c("H_Z", "H_R"))
  make_dml <- function(raw) {
    metric <- function(value) stats::setNames(rep(value, length(raw)), raw)
    fold <- list(
      derivative_refinement_error = metric(0),
      derivative_refinement_protocol_exact = metric(TRUE),
      honest_information_scaled_riesz_defect = metric(0),
      honest_riesz_null_space_defect = metric(0),
      ridge_neighborhood_artifact = stats::setNames(
        lapply(raw, function(x) list(id = x)), raw),
      honest_evaluation_information = diag(2),
      riesz_numerical_rank_rule = "dimension_epsilon",
      riesz_numerical_rank_cutoff = 2 * .Machine$double.eps,
      honest_evaluation_numerical_rank = 2L)
    value <- seq_along(raw) + 0.5
    structure(list(
      estimate = stats::setNames(value, raw),
      crossfit_plugin = stats::setNames(value - 0.1, raw),
      influence = matrix(
        seq_len(4L * length(raw)), 4L,
        dimnames = list(paste0("r", 1:4), raw)),
      respondent_id = paste0("r", 1:4), fold_details = list(fold)),
      class = c("scmix_dml", "list"))
  }
  callbacks <- stats::setNames(
    as.list(c("active", "mean_direction", "mean_numerator",
              "mean_denominator", "choice_probability",
              "preference_direction_share", "compensating_benefit_share",
              "subgroup_high", "subgroup_low", "mrs")),
    c("active_q1_covariance_eigenvalue", "mean_direction",
      "mean_numerator", "mean_denominator", "choice_probability",
      "preference_direction_share", "compensating_benefit_share",
      "subgroup_high", "subgroup_low", "mrs"))
  targets <- list(
    callbacks = callbacks,
    heterogeneity = list(
      plugin_targets = list("heterogeneity"),
      primitive_registry = list(list()), primitive_ids = c("H_Z", "H_R")))
  calls <- character()
  local_mocked_bindings(
    scmix_dml = function(..., v22_registry = NULL) {
      marker <- unname(list(...)$plugin_targets[[1L]])
      calls <<- c(calls, marker)
      if (identical(marker, "mrs")) stop("target-specific MRS failure")
      make_dml(raw_by_analysis[[marker]])
    },
    .package = "sconjoint")
  result <- sconjoint:::.scmix_v24_dml_once(
    list(deltaX = matrix(0, 4L, 2L)), targets, seed = 2L)
  expect_identical(calls, c(
    "active", "mean_direction", "mean_numerator", "mean_denominator",
    "choice_probability", "preference_direction_share",
    "compensating_benefit_share", "subgroup_high", "subgroup_low", "mrs",
    "heterogeneity"))
  expect_identical(result$failed_targets, "mrs")
  expect_identical(result$failed_families, "mrs")
  expect_false(any(c("mrs_numerator", "mrs_denominator") %in%
                     names(result$estimate)))
  expect_true(all(c("mean_direction", "choice_probability",
                    "heterogeneity_explained_variance") %in%
                  names(result$estimate)))
  expect_true(result$base$target_specific_composite)
  expect_true(all(c("mean_direction", "choice_probability") %in%
                    names(result$base$estimate)))
  view <- sconjoint:::.scmix_v24_inference_view(result)
  expect_true(is.na(view$reported_estimate[["mrs"]]))
  expect_identical(view$reported_se[["mrs"]], Inf)
  expect_true(is.finite(view$reported_estimate[["mean_direction"]]))
  expect_identical(view$failed_families, "mrs")
})

test_that("v2.4 global fit-regularity DML failure fails the replication", {
  targets <- list(
    callbacks = list(active_q1_covariance_eigenvalue = "active"),
    heterogeneity = list(plugin_targets = list("heterogeneity"),
                         primitive_registry = list(list())))
  local_mocked_bindings(
    scmix_dml = function(...) stop("common score failure"),
    .package = "sconjoint")
  expect_error(sconjoint:::.scmix_v24_dml_once(
    list(deltaX = matrix(0, 4L, 2L)), targets, seed = 2L),
    "global v2.4 fit-regularity DML preflight failed")
})

test_that("v2.4 truth coverage is simultaneous within a released family", {
  family <- structure(list(
    members = list(
      a = list(interval = c(lower = 0, upper = 2)),
      b = list(interval = c(lower = -1, upper = 1)))),
    class = c("scmix_v24_family_result", "list"))
  truth <- data.frame(
    family_id = c("smooth_means", "smooth_means"),
    target_id = c("a", "b"), truth = c(1, 0.5),
    status = "defined", stringsAsFactors = FALSE)
  result <- sconjoint:::.scmix_v24_family_truth_result(
    "smooth_means", family, TRUE, truth)
  expect_true(result$released)
  expect_true(result$covered)
  family$members$b$interval <- c(-1, 0)
  result <- sconjoint:::.scmix_v24_family_truth_result(
    "smooth_means", family, TRUE, truth)
  expect_false(result$covered)
  truth$status[[2L]] <- "undefined_exact_boundary"
  result <- sconjoint:::.scmix_v24_family_truth_result(
    "smooth_means", family, TRUE, truth)
  expect_true(is.na(result$covered))
})

test_that("v2.4 artifact writes are signed, private, and no-overwrite", {
  root <- tempfile("v24-artifact-")
  dir.create(root)
  payload <- sconjoint:::.scmix_v24_sign(list(
    cell_id = "cell1", replication = 1L, synthetic_outcome = TRUE,
    application_outcomes_used = FALSE, completed = TRUE),
    "scmix_v24_replication_artifact")
  path <- file.path(root, "one.rds")
  saved <- sconjoint:::.scmix_v24_save_atomic(payload, path)
  expect_identical(
    sconjoint:::.scmix_v24_read_artifact(
      saved, data.frame(cell_id = "cell1", replication = 1L)),
    payload)
  expect_error(sconjoint:::.scmix_v24_save_atomic(payload, path),
               "Refusing to overwrite")
})

test_that("v2.4 artifact reuse requires every frozen evaluator binding", {
  root <- tempfile("v24-reuse-")
  dir.create(root)
  run_dir <- file.path(root, "run")
  dir.create(run_dir)
  plan <- .v24_runner_plan()
  evaluator <- .v24_runner_evaluator()
  written <- .v24_runner_pilot_artifact(
    run_dir, plan, evaluator, "cell1", 1L, elapsed = 2.5)
  verified <- sconjoint:::.scmix_v24_validate_reusable_artifact(
    written$artifact_file,
    list(stream = "pilot", cell_id = "cell1", replication = 1L),
    plan, evaluator, expected_sha256 = written$artifact_sha256)
  expect_identical(verified$artifact_sha256, written$artifact_sha256)
  expect_identical(verified$elapsed_seconds, 2.5)
  expect_error(sconjoint:::.scmix_v24_validate_reusable_artifact(
    written$artifact_file,
    list(stream = "pilot", cell_id = "cell1", replication = 1L),
    plan, evaluator, expected_sha256 = .v24_runner_hash("d")),
    "recorded file hash")
  changed <- evaluator
  changed$settings$fingerprint <- .v24_runner_hash("e")
  expect_error(sconjoint:::.scmix_v24_validate_reusable_artifact(
    written$artifact_file,
    list(stream = "pilot", cell_id = "cell1", replication = 1L),
    plan, changed), "exact current schedule and evaluator binding")
})

test_that("v2.4 worker allocation is deterministic by cell", {
  plan <- .v24_runner_plan()
  pilot <- expand.grid(
    cell_id = plan$cells$cell_id, replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result <- sconjoint:::.scmix_v24_run_worker_schedule(
    pilot, 6L, function(cell, replication, worker) list(
      cell_id = cell, replication = replication, worker_id = worker),
    parallel = FALSE)
  by_worker <- split(result, vapply(result, `[[`, integer(1L), "worker_id"))
  expect_length(by_worker, 6L)
  expect_true(all(vapply(by_worker, function(x)
    length(unique(vapply(x, `[[`, character(1L), "cell_id"))) == 1L,
    logical(1L))))

  evidence <- sconjoint:::.scmix_v24_evidence_jobs(plan, 3L)
  expect_equal(nrow(evidence), 60L)
  expect_identical(sort(unique(evidence$replication)), 21:30)
})

test_that("v2.4 PSOCK backend maps six workers and always stops them", {
  plan <- .v24_runner_plan()
  pilot <- expand.grid(
    cell_id = plan$cells$cell_id, replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  source_root <- normalizePath(testthat::test_path("..", ".."),
                               winslash = "/", mustWork = TRUE)
  withr::local_envvar(SCONJOINT_SOURCE_ROOT = source_root)

  factory_calls <- 0L
  methods_seen <- NULL
  bootstrap_calls <- 0L
  stop_calls <- 0L
  fake_cluster <- structure(as.list(seq_len(6L)),
                            class = "v24_fake_cluster")
  factory <- function(workers, methods) {
    factory_calls <<- factory_calls + 1L
    methods_seen <<- methods
    expect_identical(workers, 6L)
    fake_cluster
  }
  initialize <- function(cluster, bootstrap, root) {
    bootstrap_calls <<- bootstrap_calls + 1L
    expect_identical(cluster, fake_cluster)
    expect_true(is.function(bootstrap))
    expect_identical(environment(bootstrap), baseenv())
    expect_identical(root, source_root)
    lapply(seq_len(6L), function(worker) list(
      source_root = root, pid = as.integer(1000L + worker),
      torch_threads = 1L))
  }
  apply <- function(cluster, worker_ids, work) {
    expect_identical(cluster, fake_cluster)
    expect_identical(worker_ids, seq_len(6L))
    lapply(worker_ids, work)
  }
  stop <- function(cluster) {
    expect_identical(cluster, fake_cluster)
    stop_calls <<- stop_calls + 1L
    invisible(NULL)
  }
  result <- sconjoint:::.scmix_v24_run_worker_schedule(
    pilot, 6L, function(cell, replication, worker) list(
      cell_id = cell, replication = replication, worker_id = worker),
    parallel = TRUE, .cluster_factory = factory,
    .cluster_call = initialize, .cluster_apply = apply,
    .cluster_stop = stop)

  expect_identical(factory_calls, 1L)
  expect_identical(methods_seen, FALSE)
  expect_identical(bootstrap_calls, 1L)
  expect_identical(stop_calls, 1L)
  expect_length(result, 12L)
  expect_true(all(vapply(result, function(x)
    identical(x$worker_id, match(x$cell_id, plan$cells$cell_id)),
    logical(1L))))
  expect_false(any(c("source_root", "pid", "torch_threads") %in%
                     unique(unlist(lapply(result, names)))))

  failing_apply <- function(cluster, worker_ids, work) {
    stop("private worker detail", call. = FALSE)
  }
  expect_error(sconjoint:::.scmix_v24_run_worker_schedule(
    pilot, 6L, function(...) NULL, parallel = TRUE,
    .cluster_factory = factory, .cluster_call = initialize,
    .cluster_apply = failing_apply, .cluster_stop = stop),
    "One or more v2.4 worker processes failed outside a job guard",
    fixed = TRUE)
  expect_identical(factory_calls, 2L)
  expect_identical(bootstrap_calls, 2L)
  expect_identical(stop_calls, 2L)
})

test_that("v2.4 PSOCK dispatch forces an outer job-function promise", {
  plan <- .v24_runner_plan()
  jobs <- expand.grid(
    cell_id = plan$cells$cell_id, replication = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  source_root <- normalizePath(testthat::test_path("..", ".."),
                               winslash = "/", mustWork = TRUE)
  withr::local_envvar(SCONJOINT_SOURCE_ROOT = source_root)
  fake_cluster <- structure(as.list(seq_len(6L)),
                            class = "v24_fake_cluster")
  factory <- function(workers, methods) fake_cluster
  initialize <- function(cluster, bootstrap, root) {
    lapply(seq_len(6L), function(worker) list(
      source_root = root, pid = as.integer(2000L + worker),
      torch_threads = 1L))
  }
  stopped <- FALSE
  stop <- function(cluster) {
    stopped <<- TRUE
    invisible(NULL)
  }

  caller <- new.env(parent = environment())
  caller$fun <- function(cell, replication, worker) list(
    cell_id = cell, replication = replication, worker_id = worker)
  caller$invoke <- function(jobs, factory, initialize, apply, stop) {
    sconjoint:::.scmix_v24_run_worker_schedule(
      jobs, 6L, job_fun = fun, parallel = TRUE,
      .cluster_factory = factory, .cluster_call = initialize,
      .cluster_apply = apply, .cluster_stop = stop)
  }
  environment(caller$invoke) <- caller
  apply <- function(cluster, worker_ids, work) {
    rm("fun", envir = caller)
    remote_work <- unserialize(serialize(work, NULL))
    lapply(worker_ids, remote_work)
  }
  result <- caller$invoke(jobs, factory, initialize, apply, stop)
  expect_true(stopped)
  expect_length(result, 12L)
  expect_true(all(vapply(result, function(x)
    identical(x$worker_id, match(x$cell_id, plan$cells$cell_id)),
    logical(1L))))
})

test_that("v2.4 production waves expose and use only fixed dispatch", {
  expect_identical(names(formals(scmix_v24_run_pilot_wave)), c(
    "run_dir", "plan", "state_dir", "source_root", "verbose"))
  expect_identical(names(formals(scmix_v24_run_evidence_batch)), c(
    "batch_index", "run_dir", "plan", "state_dir", "source_root",
    "verbose"))
  expect_false(any(c(".parallel", ".job_fun") %in%
                     names(formals(scmix_v24_run_pilot_wave))))
  expect_false(any(c(".parallel", ".job_fun") %in%
                     names(formals(scmix_v24_run_evidence_batch))))

  pilot_test_dispatch <- "not called"
  evidence_test_dispatch <- "not called"
  scheduled <- NULL
  local_mocked_bindings(
    .scmix_v24_pilot_wave_core = function(..., .test_dispatch = NULL) {
      pilot_test_dispatch <<- .test_dispatch
      "pilot"
    },
    .scmix_v24_evidence_batch_core = function(..., .test_dispatch = NULL) {
      evidence_test_dispatch <<- .test_dispatch
      "evidence"
    },
    .scmix_v24_run_worker_schedule = function(
        jobs, workers, job_fun, parallel, ...) {
      scheduled <<- list(jobs = jobs, workers = workers,
                         job_fun = job_fun, parallel = parallel)
      "scheduled"
    },
    .package = "sconjoint")
  plan <- .v24_runner_plan()
  expect_identical(scmix_v24_run_pilot_wave(
    "unused", plan = plan, source_root = "unused"), "pilot")
  expect_identical(scmix_v24_run_evidence_batch(
    1L, "unused", plan = plan, source_root = "unused"), "evidence")
  fixed_dispatch <- sconjoint:::.scmix_v24_fixed_psock_dispatch
  expect_null(pilot_test_dispatch)
  expect_null(evidence_test_dispatch)

  jobs <- data.frame(cell_id = paste0("cell", seq_len(6L)),
                     replication = 1L)
  job_fun <- function(...) NULL
  expect_identical(fixed_dispatch(jobs, job_fun), "scheduled")
  expect_identical(scheduled$jobs, jobs)
  expect_identical(scheduled$workers, 6L)
  expect_identical(scheduled$job_fun, job_fun)
  expect_identical(scheduled$parallel, TRUE)
})

test_that("v2.4 internal wave cores reject non-test dispatch injection", {
  withr::local_envvar(TESTTHAT = NA_character_)
  expect_error(sconjoint:::.scmix_v24_pilot_wave_core(
    "unused", .v24_runner_plan(), NULL, "unused", FALSE,
    .test_dispatch = identity), "available only to unit tests",
    fixed = TRUE)
  expect_error(sconjoint:::.scmix_v24_evidence_batch_core(
    1L, "unused", .v24_runner_plan(), NULL, "unused", FALSE,
    .test_dispatch = identity), "available only to unit tests",
    fixed = TRUE)
})

test_that("v2.4 evidence injection is confined to its test-only harness", {
  root <- tempfile("v24-evidence-test-")
  dir.create(root)
  run_dir <- file.path(root, "run")
  plan <- .v24_runner_plan()
  local_mocked_bindings(
    .scmix_v24_evaluator_readiness = function(...)
      list(evidence_outcome_generation_permitted = TRUE),
    .package = "sconjoint")
  job <- function(cell, replication, worker) list(
    cell_id = cell, replication = replication, completed = TRUE,
    artifact_file = file.path(run_dir, paste0(cell, "-", replication)),
    artifact_sha256 = .v24_runner_hash("a"), worker_id = worker,
    elapsed_seconds = 1, resumed = FALSE, error = "")
  result <- sconjoint:::.scmix_v24_test_run_evidence_batch(
    1L, run_dir, plan = plan, source_root = root,
    .parallel = FALSE, .job_fun = job)
  manifest <- utils::read.csv(result$manifest_file,
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  expect_identical(result$attempted_jobs, 60L)
  expect_identical(result$completed_jobs, 60L)
  expect_identical(result$workers, 6L)
  expect_equal(nrow(manifest), 60L)
  expect_identical(
    manifest$worker_id,
    as.integer(match(manifest$cell_id, plan$cells$cell_id)))
})

test_that("v2.4 pilot public manifest contains metadata only", {
  root <- tempfile("v24-pilot-")
  dir.create(root)
  plan <- .v24_runner_plan()
  evaluator <- .v24_runner_evaluator()
  local_mocked_bindings(
    .scmix_v24_evaluator_readiness = function(...)
      list(pilot_outcome_generation_permitted = TRUE,
           evaluator = evaluator),
    scmix_v24_runtime_projection = function(...)
      list(projected_evidence_wall_seconds = 120,
           projected_evidence_wall_hours = 1 / 30, pass = TRUE),
    scmix_v24_generate_study_outcome = function(...)
      stop("a valid pilot artifact should have been resumed"),
    .package = "sconjoint")
  run_dir <- file.path(root, "run")
  job <- function(cell, replication, worker) {
    written <- .v24_runner_pilot_artifact(
      run_dir, plan, evaluator, cell, replication, elapsed = worker)
    c(list(
      cell_id = cell, replication = replication, worker_id = worker,
      completed = TRUE, resumed = FALSE, error = ""), written)
  }
  result <- sconjoint:::.scmix_v24_test_run_pilot_wave(
    run_dir, plan = plan, source_root = root,
    .parallel = FALSE, .job_fun = job)
  manifest <- utils::read.csv(result$pilot_run_manifest,
                              stringsAsFactors = FALSE,
                              check.names = FALSE)
  expect_identical(names(manifest), c(
    "cell_id", "replication", "completed", "worker_id",
    "elapsed_seconds"))
  expect_equal(nrow(manifest), 12L)
  expect_false(any(grepl(
    "estimate|interval|coverage|artifact|released|failed_gates",
    names(manifest), ignore.case = TRUE)))
  expect_false(any(c("estimates", "gates", "coverage") %in%
                     names(result)))
  expect_false(result$target_or_gate_results_inspected)
  expect_true(file.exists(result$private_artifact_manifest))
  expect_identical(
    unname(as.character(tools::sha256sum(result$private_artifact_manifest))),
    result$private_artifact_manifest_sha256)
  private <- readRDS(result$private_artifact_manifest)
  expect_true(sconjoint:::.scmix_v24_intact(
    private, "scmix_v24_pilot_artifact_manifest"))
  expect_true(private$complete)
  expect_equal(nrow(private$records), 12L)
  expect_true(all(grepl(
    "^[[:xdigit:]]{64}$", private$records$artifact_sha256)))

  first_started <- result$wave_started_unix
  first_finished <- result$wave_finished_unix
  resumed <- sconjoint:::.scmix_v24_test_run_pilot_wave(
    run_dir, plan = plan, source_root = root, .parallel = FALSE)
  expect_identical(resumed$wave_started_unix, first_started)
  expect_identical(resumed$wave_finished_unix, first_finished)
  resumed_private <- readRDS(resumed$private_artifact_manifest)
  expect_identical(resumed_private$attempt_count, 2L)
  expect_true(all(resumed_private$records$resumed))
  expect_identical(
    utils::read.csv(resumed$pilot_run_manifest,
                    stringsAsFactors = FALSE, check.names = FALSE),
    manifest)
})
