.sw_v21_rank_test_root <- normalizePath(test_path("..", ".."),
                                          mustWork = TRUE)
.sw_v21_rank_app <- file.path(.sw_v21_rank_test_root,
                              "applications", "sw2022")
.sw_v21_rank_env <- new.env(parent = baseenv())
.sw_v21_rank_env$.sc_manifest_artifacts_valid <-
  sconjoint:::.sc_manifest_artifacts_valid
.sw_v21_rank_env$.sc_identical_md5_vectors <-
  sconjoint:::.sc_identical_md5_vectors
.sw_v21_rank_env$.sc_md5_paths <- sconjoint:::.sc_md5_paths
sys.source(file.path(.sw_v21_rank_app, "v2_1", "R",
                     "rank_numerical_contract_v2_1.R"),
           envir = .sw_v21_rank_env)

test_that("v2.1 rank diagnostics freeze ranks, panels, and GH ladders", {
  old <- getOption("sconjoint.sw_application_root")
  on.exit(options(sconjoint.sw_application_root = old), add = TRUE)
  options(sconjoint.sw_application_root = .sw_v21_rank_app)
  env <- new.env(parent = baseenv())
  sys.source(file.path(.sw_v21_rank_app, "v2_1", "config",
                       "rank_numerical_config_v2_1.R"), envir = env)
  cfg <- env$sw_v21_rank_config
  expect_identical(cfg$ranks, c(0L, 1L, 2L))
  expect_identical(cfg$base_nodes,
                   c(`0` = 31L, `1` = 31L, `2` = 15L))
  expect_identical(cfg$refinement_nodes$`1`, c(15L, 31L, 45L))
  expect_identical(cfg$refinement_nodes$`2`, c(9L, 15L, 21L))
  expect_length(cfg$rotation_angles, 4L)
  expect_true(cfg$panels$selected_procedure$always_run)
  expect_true(
    cfg$panels$exact_constant$run_if_reported_primary_fallback)
  expect_false(cfg$rank_selected)
  expect_false(cfg$primary_rank_changed)
  expect_false(cfg$formal_inference_available)
  expect_false(cfg$outcome_blind)
})

test_that("node grids preserve the frozen estimator family and constant nest", {
  old <- getOption("sconjoint.sw_application_root")
  on.exit(options(sconjoint.sw_application_root = old), add = TRUE)
  options(sconjoint.sw_application_root = .sw_v21_rank_app)
  env <- new.env(parent = baseenv())
  sys.source(file.path(.sw_v21_rank_app, "v2_1", "config",
                       "analysis_config_v2_1.R"), envir = env)
  grid <- env$sw_v21_config$grid
  make_grid <- .sw_v21_rank_env$.sw_v21_rank_grid_at_nodes
  selected <- make_grid(grid, "selected_procedure", 2L, 21L)
  constant <- make_grid(grid, "exact_constant", 1L, 45L)
  expect_length(selected, 10L)
  expect_length(constant, 1L)
  expect_identical(constant[[1L]]$mean_family, "constant")
  expect_identical(constant[[1L]]$weight_decay, 0)
  expect_true(all(vapply(selected, `[[`, integer(1L), "n_nodes") == 21L))
  expect_true(all(vapply(selected, `[[`, character(1L),
                         "integration") == "gh"))
  expect_identical(
    vapply(selected, `[[`, character(1L), "mean_family"),
    vapply(grid, `[[`, character(1L), "mean_family"))
  expect_identical(
    vapply(selected, `[[`, numeric(1L), "weight_decay"),
    vapply(grid, `[[`, numeric(1L), "weight_decay"))
})

test_that("completed parent pointer and manifest are bound fail-closed", {
  td <- tempfile("v21-rank-parent-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  write_rds <- function(x, name) {
    path <- file.path(td, name)
    saveRDS(x, path, version = 3)
    path
  }
  selected <- list(
    full = write_rds(list(role = "selected-full"),
                     "fit_selected_full.rds"),
    nested = write_rds(list(role = "selected-nested"),
                       "fit_selected_nested.rds"),
    assembled = write_rds(list(role = "selected-assembled"),
                          "fit_selected_assembled.rds"))
  constant <- list(
    full = write_rds(list(role = "constant-full"),
                     "fit_constant_full.rds"),
    nested = write_rds(list(role = "constant-nested"),
                       "fit_constant_nested.rds"),
    assembled = write_rds(list(role = "constant-assembled"),
                          "fit_constant_assembled.rds"))
  auth_path <- write_rds(list(authorized = TRUE),
                         "FINAL_ANALYSIS_AUTHORIZATION.rds")
  input_path <- write_rds(list(input = TRUE), "input.rds")
  input_paths <- c(input = input_path)
  generation <- .sw_v21_rank_env$.sc_md5_paths(input_paths)
  runtime <- list(R = "test", torch = "test")
  pointer <- list(
    schema_version = "sw2022-v2.1-reported-primary-pointer-v1",
    reported_primary = "exact_constant_q1", fallback_applied = TRUE,
    full_fit_path = constant$full,
    nested_fit_path = constant$nested,
    assembled_fit_path = constant$assembled,
    selected_procedure_paths = selected,
    exact_constant_paths = constant,
    score_difference = -0.002,
    noninferiority_margin = -0.001,
    formal_test = FALSE, formal_inference_available = FALSE,
    outcome_blind = FALSE, generation_input_md5 = generation,
    runtime_signature = runtime,
    authorization_md5 = unname(tools::md5sum(auth_path)))
  pointer_path <- write_rds(pointer, "reported_primary_pointer.rds")
  artifact_paths <- c(unlist(selected, use.names = FALSE),
                      unlist(constant, use.names = FALSE), pointer_path)
  artifacts <- stats::setNames(unname(tools::md5sum(artifact_paths)),
                               basename(artifact_paths))
  manifest <- list(
    schema_version = "sw2022-v2.1-postpilot-final-manifest-v1",
    configuration_version = "parent-v1", final_analysis_success = TRUE,
    procedural_primary_available = TRUE,
    reported_primary = "exact_constant_q1", fallback_applied = TRUE,
    postpilot_guardrail = list(mean_difference = -0.002,
                               margin = -0.001),
    input_paths = input_paths, generation_input_md5 = generation,
    completion_input_md5 = generation, runtime_signature = runtime,
    authorization_md5 = unname(tools::md5sum(auth_path)),
    artifacts = artifacts, formal_inference_available = FALSE,
    outcome_blind = FALSE, production_result = FALSE)
  manifest_path <- write_rds(manifest, "manifest.rds")
  parent <- list(version = "parent-v1", output_root = td,
                 authorization_file = auth_path)
  validate <- .sw_v21_rank_env$.sw_v21_rank_final_bundle_valid
  expect_true(validate(pointer, pointer_path, manifest, manifest_path,
                       parent, runtime))

  tampered <- pointer
  tampered$reported_primary <- "selected_procedure_q1"
  saveRDS(tampered, pointer_path, version = 3)
  expect_false(validate(tampered, pointer_path, manifest, manifest_path,
                        parent, runtime))
})

test_that("authorization and centered-alpha state are explicit gates", {
  td <- tempfile("v21-rank-auth-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  config_path <- file.path(td, "config.R")
  pointer_path <- file.path(td, "pointer.rds")
  manifest_path <- file.path(td, "manifest.rds")
  writeLines("config", config_path)
  saveRDS(list(pointer = TRUE), pointer_path)
  saveRDS(list(manifest = TRUE), manifest_path)
  generation <- c(source = "abc")
  runtime <- list(R = "test")
  config <- list(version = "rank-v1")
  pointer <- list(reported_primary = "selected_procedure_q1",
                  fallback_applied = FALSE)
  authorization <- list(
    authorized = TRUE,
    purpose = "sw2022-v2.1-rank-numerical-diagnostics",
    reviewed_by = "reviewer", authorized_at_utc = "now",
    acknowledged_outcome_informed = TRUE,
    acknowledged_formal_inference_unavailable = TRUE,
    acknowledged_no_rank_selection = TRUE,
    config_version = "rank-v1",
    config_md5 = unname(tools::md5sum(config_path)),
    generation_input_md5 = generation,
    runtime_signature = runtime,
    reviewed_pointer_md5 = unname(tools::md5sum(pointer_path)),
    reviewed_final_manifest_md5 = unname(tools::md5sum(manifest_path)),
    reported_primary = "selected_procedure_q1", fallback_applied = FALSE,
    formal_inference_available = FALSE, rank_selected = FALSE,
    outcome_blind = FALSE)
  validate <- .sw_v21_rank_env$.sw_v21_rank_authorization_valid
  expect_true(validate(authorization, config, config_path, generation,
                       runtime, pointer, pointer_path, manifest_path))
  authorization$rank_selected <- TRUE
  expect_false(validate(authorization, config, config_path, generation,
                        runtime, pointer, pointer_path, manifest_path))

  corrected <- list(
    network_state = list(
      format = "scmix-network-state", format_version = 2L,
      architecture_id = "mixed-conjoint-mean-family-v2",
      architecture = list(q = 1L, mean_family = "relu", p = 2L),
      state_dict = list(alpha_raw = c(0, 0),
                        mu_bound_internal = c(1, 1)),
      preprocessing = list(deltaX = list(centering = "none"))),
    optimization = list(bounds = list(
      alpha_diagnostics_applicable = TRUE)))
  expect_true(.sw_v21_rank_env$.sw_v21_rank_corrected_refit_valid(
    corrected, 1L))
  corrected$network_state$architecture_id <- "legacy"
  expect_false(.sw_v21_rank_env$.sw_v21_rank_corrected_refit_valid(
    corrected, 1L))
})

test_that("q2 rotation chart is orthogonal", {
  rotate <- .sw_v21_rank_env$.sw_v21_rank_rotation_matrix
  for (angle in c(0, pi / 8, pi / 4, 3 * pi / 8)) {
    R <- rotate(angle)
    expect_equal(crossprod(R), diag(2), tolerance = 1e-14)
    expect_equal(det(R), 1, tolerance = 1e-14)
  }
})

rm(.sw_v21_rank_test_root, .sw_v21_rank_app, .sw_v21_rank_env)
