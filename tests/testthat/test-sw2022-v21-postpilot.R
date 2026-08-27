.sw_v21_test_root <- normalizePath(test_path("..", ".."), mustWork = TRUE)
.sw_v21_app <- file.path(.sw_v21_test_root, "applications", "sw2022")
.sw_v21_contract_env <- new.env(parent = baseenv())
.sw_v21_contract_env$.sc_manifest_artifacts_valid <-
  sconjoint:::.sc_manifest_artifacts_valid
.sw_v21_contract_env$.sc_identical_md5_vectors <-
  sconjoint:::.sc_identical_md5_vectors
.sw_v21_contract_env$.sc_md5_paths <- sconjoint:::.sc_md5_paths
sys.source(file.path(.sw_v21_app, "v2_1", "R",
                     "postpilot_contract_v2_1.R"),
           envir = .sw_v21_contract_env)

test_that("v2.1 freezes the previously declared full grid and schedule", {
  cfg_env <- new.env(parent = baseenv())
  old <- getOption("sconjoint.sw_application_root")
  on.exit(options(sconjoint.sw_application_root = old), add = TRUE)
  options(sconjoint.sw_application_root = .sw_v21_app)
  sys.source(file.path(.sw_v21_app, "v2_1", "config",
                       "analysis_config_v2_1.R"), envir = cfg_env)
  cfg <- cfg_env$sw_v21_config
  predecessor_env <- new.env(parent = baseenv())
  predecessor_path <- file.path(.sw_v21_app, "v2", "config",
                                "analysis_config_v2.R")
  sys.source(predecessor_path, envir = predecessor_env)

  expect_length(cfg$grid, 10L)
  expect_identical(
    vapply(cfg$grid, `[[`, character(1L), "mean_family"),
    c("constant", rep("linear", 3L), rep("relu", 6L)))
  expect_equal(
    vapply(cfg$grid, `[[`, numeric(1L), "weight_decay"),
    c(0, rep(c(0.001, 0.01, 0.1), 3L)), tolerance = 0)
  expect_identical(
    vapply(cfg$grid, function(x) paste(x$hidden, collapse = "-"),
           character(1L)),
    c("", "", "", "", rep("4", 3L), rep("8", 3L)))
  expect_identical(cfg$folds$outer_K, 5L)
  expect_identical(cfg$folds$inner_K, 3L)
  expect_identical(cfg$optimizer$n_epochs, 1400L)
  expect_identical(cfg$optimizer$n_starts, 3L)
  expect_identical(cfg$model$n_nodes, 31L)
  expect_identical(
    cfg$postpilot_guardrail$noninferiority_margin, -0.001)
  expect_true(cfg$postpilot_guardrail$introduced_after_failed_pilot)
  expect_true(cfg$postpilot_guardrail$descriptive_only)
  expect_false(cfg$postpilot_guardrail$formal_test)
  expect_false(cfg$formal_inference_available)
  expect_false(cfg$outcome_blind)
  expect_true(.sw_v21_contract_env$.sw_v21_prepilot_spec_unchanged(
    predecessor_env$sw_v2_config, cfg))
  changed <- cfg
  changed$optimizer$n_epochs <- 1401L
  expect_false(.sw_v21_contract_env$.sw_v21_prepilot_spec_unchanged(
    predecessor_env$sw_v2_config, changed))
})

test_that("post-pilot guardrail reports SE and applies fallback mechanically", {
  decide <- .sw_v21_contract_env$.sw_v21_guardrail_decision
  constant <- c(-2, -1.9, -2.1, -2)

  pass <- decide(constant + c(-0.0008, -0.0010, -0.0009, -0.0009),
                 constant, -0.001)
  expect_true(pass$pass)
  expect_false(pass$fallback_applied)
  expect_identical(pass$reported_primary, "selected_procedure_q1")
  expect_gt(pass$respondent_se, 0)
  expect_false(pass$formal_test)

  boundary <- decide(constant - 0.001, constant, -0.001)
  expect_true(boundary$pass)
  loss <- decide(constant + c(-0.0012, -0.0011, -0.0010, -0.0011),
                 constant, -0.001)
  expect_false(loss$pass)
  expect_true(loss$fallback_applied)
  expect_identical(loss$reported_primary, "exact_constant_q1")
})

test_that("authorization binds the failed pilot and current generation", {
  cfg_env <- new.env(parent = baseenv())
  old <- getOption("sconjoint.sw_application_root")
  on.exit(options(sconjoint.sw_application_root = old), add = TRUE)
  options(sconjoint.sw_application_root = .sw_v21_app)
  config_path <- file.path(.sw_v21_app, "v2_1", "config",
                           "analysis_config_v2_1.R")
  sys.source(config_path, envir = cfg_env)
  cfg <- cfg_env$sw_v21_config
  manifest_path <- cfg$input$failed_v2_pilot_manifest
  manifest <- readRDS(manifest_path)
  runtime <- sconjoint:::.sc_runtime_signature(
    file.path(.sw_v21_test_root, "DESCRIPTION"))
  expect_true(.sw_v21_contract_env$.sw_v21_failed_pilot_valid(
    manifest, manifest_path, runtime, cfg$predecessor))

  current <- c(postpilot_config = "abc", postpilot_runner = "def")
  auth <- list(
    authorized = TRUE,
    purpose = "sw2022-v2.1-postpilot-final-analysis",
    reviewed_by = "independent-reviewer",
    authorized_at_utc = "2026-08-24 00:00:00 UTC",
    acknowledged_postpilot_outcome_informed = TRUE,
    acknowledged_formal_inference_unavailable = TRUE,
    acknowledged_failed_pilot_not_rewritten = TRUE,
    config_version = cfg$version,
    config_md5 = unname(tools::md5sum(config_path)),
    predecessor_config_md5 = unname(tools::md5sum(file.path(
      .sw_v21_app, "v2", "config", "analysis_config_v2.R"))),
    reviewed_failed_pilot_manifest_md5 =
      unname(tools::md5sum(manifest_path)),
    reviewed_failed_pilot_generation_input_md5 =
      manifest$generation_input_md5,
    reviewed_failed_pilot_artifact_md5 = manifest$artifacts,
    postpilot_generation_input_md5 = current,
    runtime_signature = runtime,
    noninferiority_margin = -0.001,
    formal_inference_available = FALSE, outcome_blind = FALSE)
  validate <- function(x = auth, generation = current) {
    .sw_v21_contract_env$.sw_v21_authorization_valid(
      x, cfg, config_path,
      file.path(.sw_v21_app, "v2", "config", "analysis_config_v2.R"),
      manifest, manifest_path, generation, runtime)
  }
  expect_true(validate())
  stale <- auth
  stale$postpilot_generation_input_md5[[1L]] <- "changed"
  expect_false(validate(stale))
  wrong_margin <- auth
  wrong_margin$noninferiority_margin <- -0.0011
  expect_false(validate(wrong_margin))
  malformed_manifest <- manifest
  malformed_manifest$pilot_success <- TRUE
  expect_false(.sw_v21_contract_env$.sw_v21_failed_pilot_valid(
    malformed_manifest, manifest_path, runtime, cfg$predecessor))
  wrong_predecessor <- manifest
  wrong_predecessor$configuration_version <- "wrong"
  expect_false(.sw_v21_contract_env$.sw_v21_failed_pilot_valid(
    wrong_predecessor, manifest_path, runtime, cfg$predecessor))
  tampered_manifest <- manifest
  tampered_manifest$artifacts[[1L]] <- "tampered"
  expect_false(.sw_v21_contract_env$.sw_v21_failed_pilot_valid(
    tampered_manifest, manifest_path, runtime, cfg$predecessor))
})

rm(.sw_v21_test_root, .sw_v21_app, .sw_v21_contract_env)
