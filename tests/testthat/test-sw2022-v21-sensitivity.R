.sw_v21_sens_root <- normalizePath(test_path("..", ".."), mustWork = TRUE)
.sw_v21_sens_app <- file.path(.sw_v21_sens_root, "applications", "sw2022")
.sw_v21_sens_dir <- file.path(.sw_v21_sens_app, "v2_1", "sensitivity")

.source_v21_configs <- function() {
  e <- new.env(parent = baseenv())
  old <- getOption("sconjoint.sw_application_root")
  options(sconjoint.sw_application_root = .sw_v21_sens_app)
  on.exit(options(sconjoint.sw_application_root = old), add = TRUE)
  sys.source(file.path(.sw_v21_sens_app, "v2_1", "config",
                       "analysis_config_v2_1.R"), envir = e)
  sys.source(file.path(.sw_v21_sens_dir, "config",
                       "sensitivity_config_v2_1.R"), envir = e)
  sys.source(file.path(.sw_v21_sens_dir, "config",
                       "misspecification_config_v2_1.R"), envir = e)
  e
}

test_that("v2.1 sensitivity profiles preserve fail-closed labels", {
  e <- .source_v21_configs()
  cfg <- e$sw_v21_sensitivity_config
  expect_named(cfg$profiles, c("production", "validated_fallback"))
  expect_identical(cfg$profiles$production$n_epochs, 1400L)
  expect_identical(cfg$profiles$production$n_starts, 3L)
  expect_identical(cfg$profiles$validated_fallback$n_epochs, 800L)
  expect_identical(cfg$profiles$validated_fallback$n_starts, 2L)
  expect_false(cfg$outcome_blind)
  expect_false(cfg$formal_inference_available)
  expect_false(cfg$maintained_model)
  expect_null(cfg$materiality_tolerances)
  expect_true(all(unlist(cfg$empirical_alternative_likelihoods) == "not_run"))
  expect_identical(cfg$profile_likelihoods, "not_run")
})

test_that("v2.1 simulations reuse the exact v1 DGP, scenario order, and seed", {
  e <- .source_v21_configs()
  v1 <- new.env(parent = baseenv())
  sys.source(file.path(.sw_v21_sens_app, "sensitivity",
                       "misspecification_config.R"), envir = v1)
  cfg <- e$sw_v21_misspecification_config
  expect_identical(cfg$scenarios, v1$sw_misspecification_config$scenarios)
  expect_identical(cfg$dgp, v1$sw_misspecification_config$dgp)
  expect_identical(cfg$seed, v1$sw_misspecification_config$seed)
  expect_identical(cfg$profiles$production$replications, 30L)
  expect_identical(cfg$profiles$validated_fallback$replications, 5L)
  expect_identical(cfg$profiles$validated_fallback$truth_draws, 10000L)
  expect_true(cfg$dgp_definitions_reused_exactly)
  expect_true(cfg$scenario_order_reused_exactly)
  expect_true(cfg$seed_reused_exactly)
  expect_identical(cfg$empirical_alternative_likelihoods, "not_run")
  expect_identical(cfg$profile_likelihoods, "not_run")
  expect_false(cfg$outcome_blind)
  expect_false(cfg$formal_inference_available)
})

test_that("reported-primary pointer shape is fail-closed", {
  e <- .source_v21_configs()
  contract <- new.env(parent = baseenv())
  sys.source(file.path(.sw_v21_sens_dir, "R",
                       "reported_primary_contract_v2_1.R"), envir = contract)
  cfg <- e$sw_v21_config
  selected <- list(
    full = "/tmp/fit_selected_full.rds",
    nested = "/tmp/fit_selected_nested.rds",
    assembled = "/tmp/fit_selected_assembled.rds")
  constant <- list(
    full = "/tmp/fit_constant_full.rds",
    nested = "/tmp/fit_constant_nested.rds",
    assembled = "/tmp/fit_constant_assembled.rds")
  pointer <- list(
    schema_version = "sw2022-v2.1-reported-primary-pointer-v1",
    reported_primary = "selected_procedure_q1", fallback_applied = FALSE,
    full_fit_path = selected$full, nested_fit_path = selected$nested,
    assembled_fit_path = selected$assembled,
    selected_procedure_paths = selected, exact_constant_paths = constant,
    score_difference = 0, score_difference_respondent_se = 0.001,
    noninferiority_margin = -0.001,
    descriptive_only = TRUE, formal_test = FALSE,
    formal_inference_available = FALSE, outcome_blind = FALSE,
    generation_input_md5 = c(config = "abc"),
    runtime_signature = list(R_version = "runtime"),
    authorization_md5 = "auth")
  expect_true(contract$.sw_v21_pointer_shape(pointer, cfg)$pass)
  bad <- pointer; bad$formal_inference_available <- TRUE
  expect_false(contract$.sw_v21_pointer_shape(bad, cfg)$pass)
  bad <- pointer; bad$outcome_blind <- TRUE
  expect_false(contract$.sw_v21_pointer_shape(bad, cfg)$pass)
  bad <- pointer; bad$full_fit_path <- constant$full
  expect_false(contract$.sw_v21_pointer_shape(bad, cfg)$pass)
  bad <- pointer; bad$fallback_applied <- TRUE
  expect_false(contract$.sw_v21_pointer_shape(bad, cfg)$pass)
})

test_that("pointer provenance hashing preserves names and detects changes", {
  contract <- new.env(parent = baseenv())
  sys.source(file.path(.sw_v21_sens_dir, "R",
                       "reported_primary_contract_v2_1.R"), envir = contract)
  td <- tempfile("v21-sensitivity-hash-"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  paths <- c(first = file.path(td, "a"), second = file.path(td, "b"))
  writeLines("a", paths[[1L]]); writeLines("b", paths[[2L]])
  before <- contract$.sw_v21_md5(paths)
  expect_identical(names(before), names(paths))
  expect_true(contract$.sw_v21_same_md5(before, before))
  writeLines("changed", paths[[2L]])
  expect_false(contract$.sw_v21_same_md5(
    before, contract$.sw_v21_md5(paths)))
})

test_that("every v2.1 sensitivity runner parses and calls pointer validation", {
  runners <- file.path(.sw_v21_sens_dir, "R", c(
    "07_run_sensitivities_v2_1.R", "08_run_misspecification_v2_1.R",
    "09_validate_sensitivities_v2_1.R"))
  helpers <- file.path(.sw_v21_sens_dir, "R", c(
    "reported_primary_contract_v2_1.R", "fit_helpers_v2_1.R"))
  expect_silent(lapply(c(runners, helpers), parse))
  for (path in runners) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(text, ".sw_v21_validate_reported_primary", fixed = TRUE)
    expect_match(text, "formal_inference_available = FALSE", fixed = TRUE)
  }
  app_text <- paste(readLines(runners[[1L]], warn = FALSE), collapse = "\n")
  sim_text <- paste(readLines(runners[[2L]], warn = FALSE), collapse = "\n")
  expect_match(app_text, "profile_likelihoods = \"not_run\"", fixed = TRUE)
  expect_match(sim_text, "empirical_alternative_likelihoods = \"not_run\"",
               fixed = TRUE)
})

rm(.sw_v21_sens_root, .sw_v21_sens_app, .sw_v21_sens_dir,
   .source_v21_configs)
