.v25_source_root <- function() {
  normalizePath(testthat::test_path("..", ".."), winslash = "/",
                mustWork = TRUE)
}

.v25_protocol_path <- function(name) {
  file.path(.v25_source_root(), "inst", "protocol", name)
}

.v25_dcf <- function(name) {
  raw <- read.dcf(.v25_protocol_path(name), all = TRUE)
  stopifnot(nrow(raw) == 1L)
  setNames(as.character(raw[1L, ]), colnames(raw))
}

.v25_csv <- function(name) {
  utils::read.csv(.v25_protocol_path(name), stringsAsFactors = FALSE,
                  check.names = FALSE)
}

.v25_sha256 <- function(path) {
  unname(as.character(tools::sha256sum(path)))
}

test_that("v2.5 is a new outcome-locked prospective protocol", {
  plan <- .v25_dcf("inference-v2.5-validation-plan.dcf")
  settings <- .v25_dcf("inference-v2.5-evaluator-settings.dcf")
  freeze <- .v25_dcf("inference-v2.5-freeze-manifest.dcf")

  expect_identical(plan[["Protocol-Id"]],
                   "paperps-inference-2026-09-07-v2.5")
  expect_identical(plan[["Plan-Id"]],
                   "paperps-lean-validation-2026-09-07-v2.5-v1")
  expect_identical(plan[["Frozen-On"]], "2026-09-07")
  expect_identical(settings[["Settings-Id"]],
                   "paperps-v2.5-end-to-end-evaluator-settings-v1")
  expect_identical(settings[["Protocol-Id"]], plan[["Protocol-Id"]])
  expect_identical(settings[["Plan-Id"]], plan[["Plan-Id"]])
  expect_identical(freeze[["Protocol-Id"]], plan[["Protocol-Id"]])
  expect_identical(freeze[["Plan-Id"]], plan[["Plan-Id"]])
  expect_identical(freeze[["Outcome-Count"]], "0")
  expect_identical(freeze[["Integrity-Pilot-Status"]], "not_run")
  expect_identical(freeze[["Integrity-Pilot-Attempt-Count"]], "0")
  expect_identical(freeze[["Evidence-Status"]], "not_run")
  expect_identical(freeze[["Evaluator-Implementation-Status"]], "absent")
  expect_identical(freeze[["Evaluator-Approval-Status"]], "absent")
  expect_identical(freeze[["Pilot-Approval-Status"]], "absent")
  expect_identical(freeze[["Family-Approval-Status"]], "absent")
  expect_identical(freeze[["Outcome-Generation"]], "locked")
  expect_identical(plan[["Pilot-Approval-File"]],
                   "inference-v2.5-integrity-pilot-approval.dcf")
  expect_identical(freeze[["Required-Pilot-Approval-File"]],
                   plan[["Pilot-Approval-File"]])
  expect_identical(plan[["Evaluator-Implementation-Manifest-File"]],
                   "inference-v2.5-evaluator-implementation.csv")
  expect_identical(freeze[["Required-Evaluator-Implementation-Manifest-File"]],
                   plan[["Evaluator-Implementation-Manifest-File"]])
  expect_identical(plan[["Family-Approval-File"]],
                   "inference-v2.5-family-approval.dcf")
  expect_identical(freeze[["Required-Family-Approval-File"]],
                   plan[["Family-Approval-File"]])

  absent <- c("inference-v2.5-evaluator-implementation.csv",
              "inference-v2.5-evaluator-approval.dcf",
              "inference-v2.5-integrity-pilot-approval.dcf",
              "inference-v2.5-family-approval.dcf")
  expect_false(any(file.exists(.v25_protocol_path(absent))))
})

test_that("v2.5 carries forward cells targets truths and per-rep gates", {
  protocol_dir <- dirname(.v25_protocol_path(
    "inference-v2.5-validation-plan.dcf"))
  identical_files <- c(
    "design-manifest.csv", "target-registry.csv", "approval-matrix.csv",
    "fit-regularity-registry.csv", "target-truths.csv")
  for (suffix in identical_files) {
    v24 <- file.path(protocol_dir, paste0("inference-v2.4-", suffix))
    v25 <- file.path(protocol_dir, paste0("inference-v2.5-", suffix))
    expect_identical(readBin(v25, "raw", n = file.info(v25)$size),
                     readBin(v24, "raw", n = file.info(v24)$size),
                     info = suffix)
  }

  cells24 <- .v25_csv("inference-v2.4-validation-cells.csv")
  cells25 <- .v25_csv("inference-v2.5-validation-cells.csv")
  expect_identical(cells25$cell_id, c(
    "br_regular", "gs_regular", "sw_regular",
    "br_probability_zero_denominator_boundary", "br_q0_rank_boundary",
    "gs_weak_inverse"))
  expect_true(all(cells25$replications == 50L))
  cells25$replications <- 100L
  expect_identical(cells25, cells24)

  decisions24 <- .v25_csv("inference-v2.4-decision-registry.csv")
  decisions25 <- .v25_csv("inference-v2.5-decision-registry.csv")
  expect_identical(decisions25[seq_len(14L), ],
                   decisions24[seq_len(14L), ])
  expect_identical(decisions25[22L, ], decisions24[22L, ])

  targets <- .v25_csv("inference-v2.5-target-registry.csv")
  truths <- .v25_csv("inference-v2.5-target-truths.csv")
  approval <- .v25_csv("inference-v2.5-approval-matrix.csv")
  expect_identical(targets$family_id, c(
    "smooth_means", "choice_probabilities", "directional_shares",
    "subgroup_ratios", "directional_heterogeneity", "mrs"))
  expect_equal(nrow(truths), 78L)
  expect_true(all(!truths$application_outcomes_used))
  expect_equal(nrow(approval), 36L)
  expect_equal(sum(approval$required), 30L)
  expect_equal(sum(!approval$required), 6L)
})

test_that("v2.5 freezes exact translated n=50 decision counts", {
  decisions <- .v25_csv("inference-v2.5-decision-registry.csv")
  value <- setNames(as.character(decisions$value), decisions$rule_id)
  expect_identical(value[["all_cell_completion"]], "48")
  expect_identical(value[["regular_release"]], "38")
  expect_identical(value[["regular_coverage"]], "0.90")
  expect_identical(value[["boundary_false_release"]],
                   "point_at_most_0.05_and_upper_at_most_0.10")
  expect_identical(value[["weak_release_low"]], "2")
  expect_identical(value[["weak_release_high"]], "15")
  expect_identical(value[["pilot_runtime"]], "8")

  expect_identical(ceiling(0.95 * 50), 48)
  expect_identical(ceiling(0.75 * 50), 38)
  expect_identical(floor(0.05 * 50), 2)
  expect_identical(ceiling(0.30 * 50), 15)
  exact_pass <- function(invalid, completed) {
    point <- invalid / completed <= 0.05
    upper <- if (invalid == completed) 1 else
      stats::qbeta(0.95, invalid + 1, completed - invalid)
    point && upper <= 0.10
  }
  for (completed in 48:50) {
    passing <- which(vapply(0:completed, exact_pass, logical(1L),
                            completed = completed)) - 1L
    expect_identical(max(passing), 1L)
  }
  expect_identical(round(stats::qbeta(0.95, 2, 49), 6), 0.091398)
  expect_identical(round(stats::qbeta(0.95, 3, 48), 6), 0.120614)
})

test_that("v2.5 freezes twelve workers two lanes and five batches", {
  plan <- .v25_dcf("inference-v2.5-validation-plan.dcf")
  settings <- .v25_dcf("inference-v2.5-evaluator-settings.dcf")
  expect_identical(plan[["Parallel-Backend"]],
                   "base_R_PSOCK_spawn_twelve_processes")
  expect_identical(settings[["Parallel-Backend"]],
                   plan[["Parallel-Backend"]])
  expect_identical(plan[["Maximum-Workers"]], "12")
  expect_identical(plan[["Workers-Per-Cell"]], "2")
  expect_identical(plan[["Deterministic-Lanes-Per-Cell"]], "2")
  expect_identical(plan[["Worker-Id-Formula"]],
                   "2*(cell_index-1)+lane_index")
  expect_identical(plan[["Integrity-Pilot-Jobs"]], "12")
  expect_identical(plan[["Evidence-Reps-Per-Cell"]], "50")
  expect_identical(plan[["Evidence-Total-Reps"]], "300")
  expect_identical(plan[["Evidence-Batch-Size-Per-Cell"]], "10")
  expect_identical(plan[["Evidence-Batches"]], "5")
  expect_identical(plan[["Early-Stop-Rule"]],
                   "none_all_300_attempts_required")
  expect_identical(settings[["Evidence-Early-Stop"]],
                   "none_all_300_attempts_required")

  pilot <- expand.grid(cell_index = seq_len(6L), lane_index = 1:2,
                       KEEP.OUT.ATTRS = FALSE)
  pilot$worker_id <- 2L * (pilot$cell_index - 1L) + pilot$lane_index
  pilot$replication <- pilot$lane_index
  expect_identical(sort(pilot$worker_id), seq_len(12L))
  expect_true(all(table(pilot$worker_id) == 1L))
  expect_true(all(table(pilot$cell_index) == 2L))

  evidence <- expand.grid(cell_index = seq_len(6L), replication = 1:50,
                          KEEP.OUT.ATTRS = FALSE)
  evidence$batch <- (evidence$replication - 1L) %/% 10L + 1L
  evidence$within_batch <- (evidence$replication - 1L) %% 10L + 1L
  evidence$lane_index <- ifelse(evidence$within_batch %% 2L == 1L, 1L, 2L)
  evidence$worker_id <- 2L * (evidence$cell_index - 1L) +
    evidence$lane_index
  expect_true(all(table(evidence$cell_index, evidence$batch) == 10L))
  expect_true(all(table(evidence$worker_id) == 25L))
  expect_true(all(table(evidence$worker_id, evidence$batch) == 5L))
})

test_that("v2.5 pilot timing and first-valid firewall are exact", {
  plan <- .v25_dcf("inference-v2.5-validation-plan.dcf")
  settings <- .v25_dcf("inference-v2.5-evaluator-settings.dcf")
  expect_identical(plan[["First-Valid-Pilot-Rule"]],
                   "chronologically_first_complete_integrity_valid_12_job_wave_is_binding")
  expect_identical(plan[["Valid-Pilot-Rerun-Rule"]],
                   "prohibited_even_if_runtime_is_unfavorable")
  expect_match(plan[["Pilot-Inspection"]],
               "integrity_elapsed_time_and_resource_use_only", fixed = TRUE)
  expect_match(plan[["Pilot-Inspection"]],
               "no_synthetic_choice_fit_target_gate_interval_coverage", fixed = TRUE)
  expect_identical(settings[["Pilot-Runtime-Safety-Factor"]], "1.25")
  expect_identical(settings[["Pilot-Wall-Clock-Cutoff-Seconds"]], "921.6")
  expect_equal(1.25 * 25 * 921.6 / 3600, 8, tolerance = 1e-12)
  expect_gt(1.25 * 25 * (921.6 + 1e-6) / 3600, 8)
  expect_match(settings[["Pilot-Inspection-Firewall"]],
               "no_synthetic_choice_fit_target_gate_interval_coverage",
               fixed = TRUE)
})

test_that("v2.5 uses nonoverlapping frozen seed streams", {
  plan <- .v25_dcf("inference-v2.5-validation-plan.dcf")
  expect_identical(plan[["Master-Pilot-Seed"]], "1910000000")
  expect_identical(plan[["Master-Evidence-Seed"]], "2010000000")
  offsets <- c(0, 200000, 400000, 600000)
  stream <- function(master, reps) {
    base <- unlist(lapply(seq_len(6L), function(cell)
      master + (cell - 1L) * 1000000 + seq_len(reps)))
    as.vector(outer(base, offsets, `+`))
  }
  v25 <- c(stream(1910000000, 2L), stream(2010000000, 50L))
  v24 <- c(stream(1900000000, 2L), stream(2000000000, 100L))
  expect_identical(anyDuplicated(v25), 0L)
  expect_length(intersect(v25, v24), 0L)
  expect_lt(max(v25), .Machine$integer.max)
})

test_that("v2.5 freeze hashes bind only the new protocol paths", {
  freeze <- .v25_dcf("inference-v2.5-freeze-manifest.dcf")
  root <- .v25_source_root()
  mapping <- c(
    `Protocol-Markdown-SHA256` = "INFERENCE_PROTOCOL_V25.md",
    `Protocol-YAML-SHA256` = "inst/protocol/inference-v2.5.yaml",
    `Plan-SHA256` = "inst/protocol/inference-v2.5-validation-plan.dcf",
    `Design-Manifest-SHA256` = "inst/protocol/inference-v2.5-design-manifest.csv",
    `Cell-Registry-SHA256` = "inst/protocol/inference-v2.5-validation-cells.csv",
    `Target-Registry-SHA256` = "inst/protocol/inference-v2.5-target-registry.csv",
    `Decision-Registry-SHA256` = "inst/protocol/inference-v2.5-decision-registry.csv",
    `Approval-Matrix-SHA256` = "inst/protocol/inference-v2.5-approval-matrix.csv",
    `Fit-Regularity-Registry-SHA256` = "inst/protocol/inference-v2.5-fit-regularity-registry.csv",
    `Truths-SHA256` = "inst/protocol/inference-v2.5-target-truths.csv",
    `Evaluator-Settings-SHA256` = "inst/protocol/inference-v2.5-evaluator-settings.dcf")
  for (field in names(mapping)) {
    expect_identical(freeze[[field]],
                     .v25_sha256(file.path(root, mapping[[field]])),
                     info = field)
  }

  designs <- .v25_csv("inference-v2.5-design-manifest.csv")
  source_fields <- c(br2017 = "BR2017-Source-Template-SHA256",
                     gs2020 = "GS2020-Source-Template-SHA256",
                     sw2022 = "SW2022-Source-Template-SHA256")
  for (design in names(source_fields)) {
    row <- designs[designs$design_id == design, ]
    expect_equal(nrow(row), 1L)
    expect_identical(freeze[[source_fields[[design]]]],
                     row$source_template_sha256)
    expect_identical(row$source_template_sha256,
                     .v25_sha256(file.path(root, row$source_template_file)))
  }
})

test_that("v2.5 external provenance inventory is exact", {
  root <- .v25_source_root()
  checksum <- file.path(root, "provenance", "inference_protocol_v2.5.sha256")
  expect_true(file.exists(checksum))
  fields <- strsplit(readLines(checksum, warn = FALSE), "  ", fixed = TRUE)
  expect_true(all(lengths(fields) == 2L))
  hashes <- vapply(fields, `[[`, character(1L), 1L)
  paths <- vapply(fields, `[[`, character(1L), 2L)
  expected <- c(
    "INFERENCE_PROTOCOL_V25.md",
    "inst/protocol/inference-v2.5.yaml",
    "inst/protocol/inference-v2.5-validation-plan.dcf",
    "inst/protocol/inference-v2.5-design-manifest.csv",
    "inst/protocol/inference-v2.5-validation-cells.csv",
    "inst/protocol/inference-v2.5-target-registry.csv",
    "inst/protocol/inference-v2.5-decision-registry.csv",
    "inst/protocol/inference-v2.5-approval-matrix.csv",
    "inst/protocol/inference-v2.5-fit-regularity-registry.csv",
    "inst/protocol/inference-v2.5-target-truths.csv",
    "inst/protocol/inference-v2.5-evaluator-settings.dcf",
    "inst/protocol/inference-v2.5-freeze-manifest.dcf",
    "tests/testthat/test-calibration-v25-protocol.R")
  expect_identical(paths, expected)
  expect_false(any(grepl("v2.4|v24", paths)))
  observed <- vapply(file.path(root, paths), .v25_sha256, character(1L))
  expect_identical(unname(hashes), unname(observed))
})
