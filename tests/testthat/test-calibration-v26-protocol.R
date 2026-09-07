.v26_source_root <- function() {
  normalizePath(testthat::test_path("..", ".."), winslash = "/",
                mustWork = TRUE)
}

.v26_protocol_path <- function(name) {
  file.path(.v26_source_root(), "inst", "protocol", name)
}

.v26_dcf <- function(name) {
  raw <- read.dcf(.v26_protocol_path(name), all = TRUE)
  stopifnot(nrow(raw) == 1L)
  setNames(as.character(raw[1L, ]), colnames(raw))
}

.v26_csv <- function(name) {
  utils::read.csv(.v26_protocol_path(name), stringsAsFactors = FALSE,
                  check.names = FALSE)
}

.v26_bytes <- function(path) readBin(path, "raw", n = file.info(path)$size)

.v26_normalize_source <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("v2\\.6", "v2.5", text)
  text <- gsub("V26", "V25", text, fixed = TRUE)
  gsub("v26", "v25", text, fixed = TRUE)
}

test_that("v2.6 freezes one rapid evidence stream and two extensions", {
  plan <- .v26_dcf("inference-v2.6-validation-plan.dcf")
  settings <- .v26_dcf("inference-v2.6-evaluator-settings.dcf")
  freeze <- .v26_dcf("inference-v2.6-freeze-manifest.dcf")

  expect_identical(plan[["Protocol-Id"]],
                   "paperps-inference-2026-09-07-v2.6")
  expect_identical(plan[["Plan-Id"]],
                   "paperps-rapid-validation-2026-09-07-v2.6-v1")
  expect_identical(plan[["Preliminary-Reps-Per-Cell"]], "8")
  expect_identical(plan[["Preliminary-Total-Reps"]], "48")
  expect_identical(plan[["Preliminary-Waves"]], "4")
  expect_identical(plan[["Wave-One-Evidence-Eligible"]], "true")
  expect_identical(plan[["Confirmatory-Minimum-Reps-Per-Cell"]], "50")
  expect_identical(plan[["Confirmatory-Preferred-Reps-Per-Cell"]], "100")
  expect_identical(plan[["Preliminary-Family-Approval"]], "prohibited")
  expect_identical(settings[["Plan-Id"]], plan[["Plan-Id"]])
  expect_identical(settings[["Preliminary-Family-Approval"]], "prohibited")
  expect_identical(
    settings[["Optimizer-Start-Selection"]],
    "minimize_declared_penalized_training_criterion"
  )
  expect_identical(freeze[["Outcome-Count"]], "0")
  expect_identical(freeze[["Outcome-Inspection-Status"]],
                   "none_exists_or_inspected")
  expect_identical(freeze[["Outcome-Generation"]], "locked")
  expect_identical(freeze[["Evaluator-Implementation-Status"]], "absent")
  expect_identical(freeze[["Evaluator-Approval-Status"]], "absent")
})

test_that("v2.6 carries forward v2.5 scientific tables exactly", {
  root <- .v26_source_root()
  suffixes <- c(
    "approval-matrix.csv", "design-manifest.csv",
    "fit-regularity-registry.csv", "target-registry.csv",
    "target-truths.csv", "validation-cells.csv")
  for (suffix in suffixes) {
    v25 <- file.path(root, "inst", "protocol",
                     paste0("inference-v2.5-", suffix))
    v26 <- file.path(root, "inst", "protocol",
                     paste0("inference-v2.6-", suffix))
    expect_identical(.v26_bytes(v26), .v26_bytes(v25), info = suffix)
  }
  d25 <- .v26_csv("inference-v2.5-decision-registry.csv")
  d26 <- .v26_csv("inference-v2.6-decision-registry.csv")
  expect_identical(d26[1:20, ], d25[1:20, ])
  expect_identical(d26[22, ], d25[22, ])
  expect_identical(d26$rule_id[[21L]], "wave_one_runtime")
  expect_equal(nrow(.v26_csv("inference-v2.6-target-truths.csv")), 78L)
})

test_that("v2.6 statistical implementation is normalized v2.5 code", {
  root <- .v26_source_root()
  for (stem in c("fit", "gates", "targets")) {
    v25 <- file.path(root, "R", paste0("calibration-v25-", stem, ".R"))
    v26 <- file.path(root, "R", paste0("calibration-v26-", stem, ".R"))
    expect_identical(.v26_normalize_source(v26),
                     paste(readLines(v25, warn = FALSE), collapse = "\n"),
                     info = stem)
  }
  read_statistical_prefix <- function(path, marker) {
    x <- readLines(path, warn = FALSE)
    x[seq_len(grep(marker, x, fixed = TRUE)[[1L]] - 1L)]
  }
  v25 <- read_statistical_prefix(
    file.path(root, "R", "calibration-v25-runner.R"),
    "scmix_v25_fit_and_infer <- function")
  v26 <- read_statistical_prefix(
    file.path(root, "R", "calibration-v26-runner.R"),
    "scmix_v26_fit_and_infer <- function")
  normalize <- function(x) {
    x <- gsub("v2\\.6", "v2.5", x)
    x <- gsub("V26", "V25", x, fixed = TRUE)
    gsub("v26", "v25", x, fixed = TRUE)
  }
  expect_identical(normalize(v26), v25)
})

test_that("v2.6 wave schedule is complete deterministic and contiguous", {
  withr::local_envvar(SCONJOINT_SOURCE_ROOT = .v26_source_root())
  plan <- scmix_v26_validation_plan()
  preliminary <- do.call(rbind, lapply(1:4, function(wave)
    sconjoint:::.scmix_v26_wave_jobs(plan, wave, "preliminary")))
  expect_equal(nrow(preliminary), 48L)
  expect_identical(sort(unique(preliminary$replication)), 1:8)
  expect_true(all(table(preliminary$cell_id) == 8L))
  expect_true(all(table(preliminary$wave) == 12L))
  expect_true(all(table(preliminary$worker_id, preliminary$wave) == 1L))

  minimum <- do.call(rbind, lapply(1:25, function(wave)
    sconjoint:::.scmix_v26_wave_jobs(plan, wave, "50")))
  preferred <- do.call(rbind, lapply(1:50, function(wave)
    sconjoint:::.scmix_v26_wave_jobs(plan, wave, "100")))
  expect_equal(nrow(minimum), 300L)
  expect_equal(nrow(preferred), 600L)
  expect_true(all(table(minimum$cell_id) == 50L))
  expect_true(all(table(preferred$cell_id) == 100L))
  expect_identical(minimum[minimum$replication <= 8,
                           c("cell_id", "replication")],
                   preliminary[, c("cell_id", "replication")])
})

test_that("v2.6 wave-one runtime rule is exact and firewall safe", {
  cells <- paste0("c", 1:6)
  jobs <- sconjoint:::.scmix_v26_expected_wave_jobs(cells, 1L)
  jobs <- jobs[order(match(jobs$cell_id, cells), jobs$replication), ]
  jobs$completed <- TRUE
  jobs$worker_id <- seq_len(12L)
  jobs$elapsed_seconds <- rep(100, 12L)
  jobs <- jobs[, c("cell_id", "replication", "completed", "worker_id",
                   "elapsed_seconds")]
  at_cutoff <- scmix_v26_runtime_projection(
    jobs, 0, 3600, cell_ids = cells, maximum_hours = 5)
  expect_true(at_cutoff$pass)
  expect_equal(at_cutoff$projected_preliminary_wall_hours, 5)
  over <- scmix_v26_runtime_projection(
    jobs, 0, 3600 + 1e-6, cell_ids = cells, maximum_hours = 5)
  expect_false(over$pass)
  expect_identical(at_cutoff$multiplier, 4L)
  expect_match(.v26_dcf("inference-v2.6-validation-plan.dcf")[[
    "Wave-One-Inspection"]],
    "no_synthetic_choice_fit_target_gate_interval_coverage", fixed = TRUE)
})

test_that("v2.6 uses a new nonoverlapping seed namespace", {
  seed_stream <- function(master, reps) {
    base <- unlist(lapply(seq_len(6L), function(cell)
      master + (cell - 1L) * 1000000 + seq_len(reps)))
    as.vector(outer(base, c(0, 200000, 400000, 500000, 600000), `+`))
  }
  v26 <- seed_stream(2020000000, 100L)
  v25 <- c(seed_stream(1910000000, 2L),
           seed_stream(2010000000, 50L))
  v24 <- c(seed_stream(1900000000, 2L),
           seed_stream(2000000000, 100L))
  expect_identical(anyDuplicated(v26), 0L)
  expect_length(intersect(v26, c(v24, v25)), 0L)
  expect_lt(max(v26), .Machine$integer.max)
})

test_that("v2.6 has no stale path and forbids eight-rep approval", {
  root <- .v26_source_root()
  files <- c(
    "INFERENCE_PROTOCOL_V26.md",
    "inst/calibration/v26/README.md",
    "inst/calibration/v26/run-validation.R",
    "inst/protocol/inference-v2.6-validation-plan.dcf",
    "inst/protocol/inference-v2.6-evaluator-settings.dcf",
    "inst/protocol/inference-v2.6.yaml")
  text <- unlist(lapply(file.path(root, files), readLines, warn = FALSE))
  expect_false(any(grepl(
    "runs/v26/step5c_integrity_and_evidence_2026-09-07", text,
    fixed = TRUE)))
  expect_true(all(grepl("runs/v26/rapid_validation_2026-09-07",
                        text[grepl("runs/v26/", text)], fixed = TRUE)))
  expect_error(scmix_v26_run_pilot_wave(), "no discarded pilot")
  expect_error(scmix_v26_run_evidence_batch(), "fixed waves")
  expect_match(paste(text, collapse = "\n"),
               "Preliminary-Family-Approval: prohibited", fixed = TRUE)
})

test_that("v2.6 application boundary fixes rank and selection interpretation", {
  plan <- .v26_dcf("inference-v2.6-validation-plan.dcf")
  contract <- plan[["Application-Rerun-Contract"]]
  expect_match(contract, "optimizer_starts_minimize_the_declared_penalized_training_criterion",
               fixed = TRUE)
  expect_match(contract, "heldout_rank_and_specification_comparisons_use_unpenalized_respondent_sequence_scores",
               fixed = TRUE)
  expect_match(contract, "q1_GH31_primary_with_GH45_fixed_state_check",
               fixed = TRUE)
  expect_match(contract, "q2_GH9_primary_with_GH15_fixed_state_check",
               fixed = TRUE)
  expect_match(contract, "no_regular_directional_share_inference_at_q0",
               fixed = TRUE)
})

test_that("v2.6 remains outcome locked before evaluator approval", {
  withr::local_envvar(SCONJOINT_SOURCE_ROOT = .v26_source_root())
  plan <- scmix_v26_validation_plan()
  ready <- sconjoint:::.scmix_v26_evaluator_readiness(plan)
  expect_false(ready$evaluator_approval_valid)
  expect_false(ready$outcome_generation_permitted)
  expect_identical(ready$outcome_generation_stage, "locked")
  expect_error(scmix_v26_generate_study_outcome(
    "br_regular", 1L, plan = plan), "outcome locked")
})

test_that("v2.6 freeze hashes and audit mirrors are exact", {
  root <- .v26_source_root()
  freeze <- .v26_dcf("inference-v2.6-freeze-manifest.dcf")
  mapping <- c(
    `Protocol-Markdown-SHA256` = "INFERENCE_PROTOCOL_V26.md",
    `Protocol-YAML-SHA256` = "inst/protocol/inference-v2.6.yaml",
    `Plan-SHA256` = "inst/protocol/inference-v2.6-validation-plan.dcf",
    `Design-Manifest-SHA256` =
      "inst/protocol/inference-v2.6-design-manifest.csv",
    `Cell-Registry-SHA256` =
      "inst/protocol/inference-v2.6-validation-cells.csv",
    `Target-Registry-SHA256` =
      "inst/protocol/inference-v2.6-target-registry.csv",
    `Decision-Registry-SHA256` =
      "inst/protocol/inference-v2.6-decision-registry.csv",
    `Approval-Matrix-SHA256` =
      "inst/protocol/inference-v2.6-approval-matrix.csv",
    `Fit-Regularity-Registry-SHA256` =
      "inst/protocol/inference-v2.6-fit-regularity-registry.csv",
    `Truths-SHA256` = "inst/protocol/inference-v2.6-target-truths.csv",
    `Evaluator-Settings-SHA256` =
      "inst/protocol/inference-v2.6-evaluator-settings.dcf")
  observed <- vapply(file.path(root, unname(mapping)), function(path)
    unname(as.character(tools::sha256sum(path))), character(1L))
  expect_identical(unname(observed), unname(freeze[names(mapping)]))

  mirrors <- rbind(
    c("R/calibration-v26.R",
      "inst/calibration/v26/source/calibration-v26.R"),
    c("R/calibration-v26-evaluator.R",
      "inst/calibration/v26/source/calibration-v26-evaluator.R"))
  for (i in seq_len(nrow(mirrors))) {
    expect_identical(.v26_bytes(file.path(root, mirrors[i, 1L])),
                     .v26_bytes(file.path(root, mirrors[i, 2L])))
  }
  absent <- c(
    "inst/protocol/inference-v2.6-evaluator-implementation.csv",
    "inst/protocol/inference-v2.6-evaluator-approval.dcf",
    "inst/protocol/inference-v2.6-wave-one-approval.dcf",
    "inst/protocol/inference-v2.6-preliminary-completion.dcf",
    "inst/protocol/inference-v2.6-confirmatory-50-completion.dcf",
    "inst/protocol/inference-v2.6-confirmatory-100-completion.dcf",
    "inst/protocol/inference-v2.6-confirmatory-50-family-approval.dcf")
  expect_false(any(file.exists(file.path(root, absent))))
})

test_that("v2.6 external provenance inventory is exact", {
  root <- .v26_source_root()
  checksum <- file.path(root, "provenance", "inference_protocol_v2.6.sha256")
  expect_true(file.exists(checksum))
  fields <- strsplit(readLines(checksum, warn = FALSE), "  ", fixed = TRUE)
  expect_true(all(lengths(fields) == 2L))
  hashes <- vapply(fields, `[[`, character(1L), 1L)
  paths <- vapply(fields, `[[`, character(1L), 2L)
  expected <- c(
    "INFERENCE_PROTOCOL_V26.md",
    "inst/protocol/inference-v2.6.yaml",
    "inst/protocol/inference-v2.6-validation-plan.dcf",
    "inst/protocol/inference-v2.6-design-manifest.csv",
    "inst/protocol/inference-v2.6-validation-cells.csv",
    "inst/protocol/inference-v2.6-target-registry.csv",
    "inst/protocol/inference-v2.6-decision-registry.csv",
    "inst/protocol/inference-v2.6-approval-matrix.csv",
    "inst/protocol/inference-v2.6-fit-regularity-registry.csv",
    "inst/protocol/inference-v2.6-target-truths.csv",
    "inst/protocol/inference-v2.6-evaluator-settings.dcf",
    "inst/protocol/inference-v2.6-freeze-manifest.dcf",
    "tests/testthat/test-calibration-v26-protocol.R")
  expect_identical(paths, expected)
  expect_false(any(grepl("v2.5|v25|step5c", paths)))
  observed <- vapply(file.path(root, paths), function(path)
    unname(as.character(tools::sha256sum(path))), character(1L))
  expect_identical(unname(hashes), unname(observed))
})
