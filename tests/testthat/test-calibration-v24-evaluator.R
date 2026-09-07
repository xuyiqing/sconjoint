.v24_test_plan <- function(root = tempdir(), evidence_reps = 100L) {
  approval <- expand.grid(
    family_id = c("smooth_means", "choice_probabilities",
                  "directional_shares", "subgroup_ratios",
                  "directional_heterogeneity", "mrs"),
    cell_id = paste0("cell", seq_len(6L)), KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
  approval$required <- TRUE
  approval$criterion <- "regular"
  plan_file <- file.path(root, "inst", "protocol", "plan.dcf")
  dir.create(dirname(plan_file), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(plan_file)) writeLines("Plan: toy", plan_file)
  matrix_file <- file.path(root, "inst", "protocol", "approval.csv")
  utils::write.csv(approval, matrix_file, row.names = FALSE)
  sconjoint:::.scmix_v24_sign(list(
    plan_id = "paperps-lean-validation-2026-09-06-v2.4-v1",
    protocol_id = "paperps-inference-2026-09-06-v2.4",
    files = c(plan = plan_file, approval = matrix_file),
    cells = data.frame(cell_id = paste0("cell", seq_len(6L)),
                       design_id = rep("toy", 6L), stringsAsFactors = FALSE),
    targets = data.frame(
      family_id = c("smooth_means", "choice_probabilities",
                    "directional_shares", "subgroup_ratios",
                    "directional_heterogeneity", "mrs"),
      stringsAsFactors = FALSE),
    approval = approval,
    counts = list(pilot_reps = 2L, evidence_reps = as.integer(evidence_reps),
                  maximum_projected_hours = 8L)),
    "scmix_v24_validation_plan")
}

.v24_write_dcf <- function(x, path) {
  writeLines(paste(names(x), unname(unlist(x)), sep = ": "), path)
}

.v24_settings_source <- function() {
  normalizePath(file.path(testthat::test_path("..", ".."), "inst",
                          "protocol",
                          "inference-v2.4-evaluator-settings.dcf"),
                mustWork = TRUE)
}

.v24_toy_evaluator_approval <- function(root, plan, state_dir) {
  # This deliberately noncanonical fixture is test-only. Outcome-capable
  # readiness rejects it unless the test explicitly disables that guard.
  dir.create(file.path(root, "inst", "protocol"), recursive = TRUE,
             showWarnings = FALSE)
  settings_rel <- file.path("inst", "protocol",
                            "inference-v2.4-evaluator-settings.dcf")
  file.copy(.v24_settings_source(), file.path(root, settings_rel),
            overwrite = TRUE)
  roles <- c("state_and_outcome_generator", "end_to_end_fit_and_infer",
             "family_finalizer", "study_driver")
  rel <- file.path("toy", paste0(seq_along(roles), ".R"))
  dir.create(file.path(root, "toy"), showWarnings = FALSE)
  for (i in seq_along(rel)) {
    writeLines(paste("toy implementation", roles[[i]]),
               file.path(root, rel[[i]]))
  }
  implementation <- data.frame(
    role = roles, path = rel,
    sha256 = vapply(file.path(root, rel), function(path)
      unname(as.character(tools::sha256sum(path))), character(1L)),
    stringsAsFactors = FALSE)
  manifest_rel <- file.path("toy", "implementation.csv")
  utils::write.csv(implementation, file.path(root, manifest_rel),
                   row.names = FALSE)
  settings <- scmix_v24_evaluator_settings(
    plan, file.path(root, settings_rel))
  approval <- c(
    `Approval-Id` = "paperps-v2.4-evaluator-approval-v2",
    `Protocol-Id` = plan$protocol_id, `Plan-Id` = plan$plan_id,
    Status = "approved_for_integrity_pilot", `Approved-On` = "2026-09-07",
    `Outcome-Count` = "0", `Application-Outcomes-Used` = "false",
    `Implementation-Commit` = paste(rep("a", 40L), collapse = ""),
    `Evaluator-Interface-Id` =
      sconjoint:::.scmix_v24_evaluator_interface_id,
    `Release-Algorithm-Id` = sconjoint:::.scmix_v24_release_algorithm_id,
    `Evaluator-Settings-File` = settings_rel,
    `Evaluator-Settings-SHA256` = settings$file_sha256,
    `Evaluator-Settings-Payload-SHA256` = settings$fingerprint,
    `Implementation-Manifest-File` = manifest_rel,
    `Implementation-Manifest-SHA256` = unname(as.character(
      tools::sha256sum(file.path(root, manifest_rel)))),
    `Runtime-Projection-Id` = sconjoint:::.scmix_v24_runtime_projection_id)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(state_dir, "inference-v2.4-evaluator-approval.dcf")
  .v24_write_dcf(approval, path)
  list(path = path, implementation = implementation,
       approval = approval, settings = settings)
}

.v24_git <- function(root, arguments) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is unavailable for the canonical fixture.")
  output <- suppressWarnings(system2(
    git, c("-C", shQuote(root), arguments), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Git fixture command failed: ", paste(output, collapse = "\n"))
  }
  output
}

.v24_canonical_git_fixture <- function() {
  root <- tempfile("v24-canonical-git-root-")
  paths <- c(
    "DESCRIPTION", "NAMESPACE", "INFERENCE_PROTOCOL_V24.md",
    "R/calibration-v24-evaluator.R", "R/calibration-v24-fit.R",
    "R/calibration-v24-gates.R", "R/calibration-v24-runner.R",
    "R/calibration-v24-targets.R", "src/fixture.c", "man/fixture.Rd",
    "inst/calibration/v24/run-validation.R",
    "inst/calibration/v24/README.md",
    "inst/calibration/v24/source/calibration-v24-evaluator.R",
    "inst/protocol/inference-v2.4-validation-plan.dcf",
    "inst/protocol/inference-v2.4-evaluator-settings.dcf",
    "tests/testthat/test-calibration-v24-evaluator.R")
  for (relative in paths) {
    path <- file.path(root, relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(paste("canonical fixture", relative), path)
  }
  plan_path <- file.path(
    root, "inst", "protocol", "inference-v2.4-validation-plan.dcf")
  plan <- sconjoint:::.scmix_v24_sign(list(
    protocol_id = "paperps-inference-2026-09-06-v2.4",
    plan_id = "paperps-lean-validation-2026-09-06-v2.4-v1",
    files = c(plan = plan_path)), "scmix_v24_validation_plan")
  inventory <- sconjoint:::.scmix_v24_required_implementation_paths(root)
  roles <- sprintf("inventory_%03d", seq_along(inventory))
  roles[match("R/calibration-v24-evaluator.R", inventory)] <-
    "state_and_outcome_generator"
  roles[match("R/calibration-v24-runner.R", inventory)] <-
    "end_to_end_fit_and_infer"
  roles[match("R/calibration-v24-targets.R", inventory)] <-
    "family_finalizer"
  roles[match("inst/calibration/v24/run-validation.R", inventory)] <-
    "study_driver"
  manifest <- data.frame(
    role = roles, path = inventory,
    sha256 = vapply(file.path(root, inventory), function(path)
      unname(as.character(tools::sha256sum(path))), character(1L)),
    stringsAsFactors = FALSE)
  manifest_path <- file.path(
    root, "inst", "protocol",
    "inference-v2.4-evaluator-implementation.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  .v24_git(root, c("init", "-q"))
  .v24_git(root, c("config", "user.email", "v24-fixture@example.invalid"))
  .v24_git(root, c("config", "user.name", "v24-fixture"))
  .v24_git(root, c("config", "commit.gpgsign", "false"))
  .v24_git(root, c("add", "-f", "."))
  .v24_git(root, c("commit", "-q", "-m", "canonical-fixture"))
  commit <- trimws(.v24_git(root, c("rev-parse", "HEAD")))
  list(root = root, plan = plan, inventory = inventory,
       manifest = manifest, manifest_path = manifest_path,
       commit = commit)
}

.v24_family_evidence_fixture <- function(root, plan) {
  implementation_hash <- paste(rep("f", 64L), collapse = "")
  evaluator <- sconjoint:::.scmix_v24_sign(list(
    approval_id = "paperps-v2.4-evaluator-approval-v2",
    protocol_id = plan$protocol_id, plan_id = plan$plan_id,
    raw = list(`Implementation-Manifest-SHA256` = implementation_hash),
    sha256 = paste(rep("d", 64L), collapse = "")),
    "scmix_v24_evaluator_approval")
  jobs <- expand.grid(
    cell_id = plan$cells$cell_id,
    replication = seq_len(plan$counts$evidence_reps),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  artifact_dir <- file.path(root, "toy", "artifacts")
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  artifact_paths <- character(nrow(jobs))
  manifest <- jobs
  manifest$completed <- TRUE
  manifest$artifact_file <- ""
  manifest$artifact_sha256 <- ""
  for (i in seq_len(nrow(jobs))) {
    family_cell <- data.frame(
      family_id = plan$targets$family_id, released = TRUE, covered = TRUE,
      truth_status = "defined", stringsAsFactors = FALSE)
    if (identical(jobs$cell_id[[i]], "cell2") &&
        identical(as.integer(jobs$replication[[i]]), 1L)) {
      family_cell$released[family_cell$family_id == "mrs"] <- FALSE
      family_cell$covered[family_cell$family_id == "mrs"] <- NA
    }
    artifact <- sconjoint:::.scmix_v24_sign(list(
      protocol_id = plan$protocol_id, plan_id = plan$plan_id,
      stream = "evidence", cell_id = jobs$cell_id[[i]],
      replication = as.integer(jobs$replication[[i]]),
      evaluator_approval_sha256 = evaluator$sha256,
      implementation_manifest_sha256 = implementation_hash,
      family_cell = family_cell, completed = TRUE,
      synthetic_outcome = TRUE, application_outcomes_used = FALSE),
      "scmix_v24_replication_artifact")
    artifact_paths[[i]] <- file.path(
      artifact_dir, paste0(jobs$cell_id[[i]], "-",
                           jobs$replication[[i]], ".rds"))
    saveRDS(artifact, artifact_paths[[i]], version = 3L)
    manifest$artifact_file[[i]] <- file.path(
      "toy", "artifacts", basename(artifact_paths[[i]]))
    manifest$artifact_sha256[[i]] <- unname(as.character(
      tools::sha256sum(artifact_paths[[i]])))
  }
  manifest_relative <- file.path("toy", "evidence-run-manifest.csv")
  manifest_path <- file.path(root, manifest_relative)
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  manifest_hash <- unname(as.character(tools::sha256sum(manifest_path)))
  evidence <- sconjoint:::.scmix_v24_sign(list(
    completion_id = "paperps-v2.4-evidence-completion-v1",
    protocol_id = plan$protocol_id, plan_id = plan$plan_id,
    raw = list(`Evidence-Run-Manifest-File` = manifest_relative,
               `Evidence-Run-Manifest-SHA256` = manifest_hash),
    sha256 = paste(rep("e", 64L), collapse = ""), manifest = manifest,
    application_outcomes_used = FALSE),
    "scmix_v24_evidence_completion")
  list(
    evaluator = evaluator, evidence = evidence, manifest = manifest,
    manifest_path = manifest_path, artifact_paths = artifact_paths,
    result = sconjoint:::.scmix_v24_recompute_family_result(
      plan, evaluator, evidence, root))
}

test_that("v2.4 evaluator settings encode the exact lean contract", {
  settings <- scmix_v24_evaluator_settings(path = .v24_settings_source())
  expect_s3_class(settings, "scmix_v24_evaluator_settings")
  expect_identical(settings$settings_id,
                   "paperps-v2.4-end-to-end-evaluator-settings-v2")
  expect_identical(settings$parallel_backend,
                   "base_R_PSOCK_spawn_six_processes")
  expect_identical(settings$mean_candidates, c("constant", "linear"))
  expect_identical(settings$values$fit_rank, 1L)
  expect_identical(settings$values$adam_epochs, 400L)
  expect_identical(settings$values$deviation_penalty, .01)
  expect_identical(settings$values$polish_tolerance_grad, 1e-10)
  expect_identical(settings$values$polish_tolerance_change, 1e-12)
  expect_identical(settings$ridge_selection, "none")
  expect_identical(settings$riesz_numerical_rank_rule, "dimension_epsilon")
  expect_identical(settings$integration_movement_rule,
                   "maximum_over_both_GH31_branches")
  expect_length(settings$integration_branches, 2L)
  expect_match(settings$fit_gate_start_scope,
               "discarded_far_starts_descriptive", fixed = TRUE)
  expect_match(settings$repolished_gate_scope,
               "exact_state_zero_Adam", fixed = TRUE)
  expect_identical(
    settings$share_boundary_evaluation,
    paste0("exact_declared_tie_rule_at_zero_variance_with_no_positive_",
           "variance_floor"))
  expect_match(settings$share_regularity_gate,
               "studentized_margin_at_least_1.96", fixed = TRUE)
  expect_identical(settings$raw[["Evidence-Early-Stop"]],
                   "none_all_600_attempts_required")
  expect_identical(settings$rng[["kind"]], "L'Ecuyer-CMRG")
  expect_identical(settings$runtime_projection_id,
                   sconjoint:::.scmix_v24_runtime_projection_id)
})

test_that("v2.4 runtime projection is exactly fifty pilot wave walls", {
  cells <- paste0("cell", seq_len(6L))
  jobs <- expand.grid(cell_id = cells, replication = 1:2,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  jobs$completed <- TRUE
  jobs$worker_id <- rep(seq_len(6L), 2L)
  jobs$elapsed_seconds <- seq_len(12L)
  jobs <- jobs[, c("cell_id", "replication", "completed", "worker_id",
                   "elapsed_seconds")]
  out <- scmix_v24_runtime_projection(
    jobs, wave_started_unix = 100, wave_finished_unix = 220,
    workers = 6L, cell_ids = cells, maximum_hours = 8)
  expect_s3_class(out, "scmix_v24_runtime_projection")
  expect_identical(out$pilot_wave_wall_seconds, 120)
  expect_identical(out$projected_evidence_wall_seconds, 6000)
  expect_equal(out$projected_evidence_wall_hours, 6000 / 3600)
  expect_true(out$pass)
  bad <- jobs[-1L, ]
  expect_error(scmix_v24_runtime_projection(
    bad, 100, 220, cell_ids = cells), "complete exact six-worker")
  crossed <- jobs
  crossed$worker_id[c(7L, 8L)] <- crossed$worker_id[c(8L, 7L)]
  expect_identical(sort(unique(crossed$worker_id)), 1:6)
  expect_error(scmix_v24_runtime_projection(
    crossed, 100, 220, cell_ids = cells), "complete exact six-worker")
  shifted <- jobs
  shifted$worker_id <- shifted$worker_id + 10L
  expect_length(unique(shifted$worker_id), 6L)
  expect_error(scmix_v24_runtime_projection(
    shifted, 100, 220, cell_ids = cells), "complete exact six-worker")
})

test_that("test-only noncanonical approval binds its declared sources", {
  root <- tempfile("v24-approval-root-")
  state <- file.path(root, "state")
  plan <- .v24_test_plan(root)
  fixture <- .v24_toy_evaluator_approval(root, plan, state)
  approved <- sconjoint:::.scmix_v24_validate_evaluator_approval(
    fixture$path, plan, root)
  expect_s3_class(approved, "scmix_v24_evaluator_approval")
  expect_identical(approved$outcome_count, 0L)
  expect_true(sconjoint:::.scmix_v24_intact(
    approved, "scmix_v24_evaluator_approval"))
  superseded <- fixture$approval
  superseded[["Approval-Id"]] <- "paperps-v2.4-evaluator-approval-v1"
  .v24_write_dcf(superseded, fixture$path)
  expect_error(sconjoint:::.scmix_v24_validate_evaluator_approval(
    fixture$path, plan, root), "malformed or not pre-outcome")
  .v24_write_dcf(fixture$approval, fixture$path)
  writeLines("tampered", file.path(root, fixture$implementation$path[[2L]]))
  expect_error(sconjoint:::.scmix_v24_validate_evaluator_approval(
    fixture$path, plan, root), "implementation file changed")
})

test_that("canonical approval exhaustively binds inventory and Git commit", {
  skip_if(!nzchar(Sys.which("git")), "Git is required for this fixture")
  fixture <- .v24_canonical_git_fixture()
  implementation <-
    sconjoint:::.scmix_v24_validate_implementation_manifest(
      fixture$manifest_path, fixture$root, fixture$plan)
  expect_identical(
    sort(as.character(implementation$table$path), method = "radix"),
    fixture$inventory)
  expect_silent(sconjoint:::.scmix_v24_validate_implementation_commit(
    fixture$commit, fixture$root, implementation, fixture$plan))

  missing <- fixture$manifest[fixture$manifest$path != "DESCRIPTION", ]
  missing_path <- file.path(fixture$root, "missing-implementation.csv")
  utils::write.csv(missing, missing_path, row.names = FALSE)
  expect_error(sconjoint:::.scmix_v24_validate_implementation_manifest(
    missing_path, fixture$root, fixture$plan),
    "does not cover the complete transitive")

  extra_file <- file.path(fixture$root, "untracked-extra.R")
  writeLines("untracked extra implementation", extra_file)
  extra <- rbind(fixture$manifest, data.frame(
    role = "untracked_extra", path = "untracked-extra.R",
    sha256 = unname(as.character(tools::sha256sum(extra_file))),
    stringsAsFactors = FALSE))
  extra_path <- file.path(fixture$root, "extra-implementation.csv")
  utils::write.csv(extra, extra_path, row.names = FALSE)
  expect_error(sconjoint:::.scmix_v24_validate_implementation_manifest(
    extra_path, fixture$root, fixture$plan),
    "does not cover the complete transitive")

  expect_error(sconjoint:::.scmix_v24_validate_implementation_commit(
    paste(rep("0", 40L), collapse = ""), fixture$root,
    implementation, fixture$plan), "implementation commit does not exist")

  final_target <- file.path(fixture$root, "untracked-final-target.R")
  final_link <- file.path(fixture$root, "untracked-final-link.R")
  writeLines("symlink target", final_target)
  if (isTRUE(file.symlink(final_target, final_link))) {
    linked <- rbind(fixture$manifest, data.frame(
      role = "final_symlink", path = "untracked-final-link.R",
      sha256 = unname(as.character(tools::sha256sum(final_link))),
      stringsAsFactors = FALSE))
    linked_path <- file.path(fixture$root, "final-link-implementation.csv")
    utils::write.csv(linked, linked_path, row.names = FALSE)
    expect_error(sconjoint:::.scmix_v24_validate_implementation_manifest(
      linked_path, fixture$root, fixture$plan),
      "must not contain symbolic links")
  }

  real_directory <- file.path(fixture$root, "untracked-real-directory")
  linked_directory <- file.path(fixture$root, "untracked-linked-directory")
  dir.create(real_directory)
  writeLines("nested symlink target",
             file.path(real_directory, "nested.R"))
  if (isTRUE(file.symlink(real_directory, linked_directory))) {
    linked <- rbind(fixture$manifest, data.frame(
      role = "component_symlink",
      path = file.path("untracked-linked-directory", "nested.R"),
      sha256 = unname(as.character(tools::sha256sum(file.path(
        linked_directory, "nested.R")))), stringsAsFactors = FALSE))
    linked_path <- file.path(fixture$root,
                             "component-link-implementation.csv")
    utils::write.csv(linked, linked_path, row.names = FALSE)
    expect_error(sconjoint:::.scmix_v24_validate_implementation_manifest(
      linked_path, fixture$root, fixture$plan),
      "must not contain symbolic links")
  }

  changed_path <- file.path("R", "calibration-v24-fit.R")
  # `git diff` alone trusts this index hint and can miss the edit. The
  # commit-object comparison must still reject the changed working bytes.
  .v24_git(fixture$root,
           c("update-index", "--assume-unchanged", changed_path))
  writeLines("tracked file changed after the approved commit",
             file.path(fixture$root, changed_path))
  expect_error(sconjoint:::.scmix_v24_validate_implementation_commit(
    fixture$commit, fixture$root, implementation, fixture$plan),
    "differs from its approved commit")
})

test_that("v2.4 readiness distinguishes absent, approved, and premature states", {
  root <- tempfile("v24-readiness-root-")
  state <- file.path(root, "state")
  dir.create(state, recursive = TRUE)
  plan <- .v24_test_plan(root)
  expect_error(sconjoint:::.scmix_v24_evaluator_readiness(
    plan, state_dir = state, source_root = root),
    "requires the canonical source root")
  absent <- sconjoint:::.scmix_v24_evaluator_readiness(
    plan, state_dir = state, source_root = root, require_canonical = FALSE)
  expect_identical(absent$status,
                   "not_ready_single_evaluator_approval_absent")
  expect_false(absent$outcome_generation_permitted)
  fixture <- .v24_toy_evaluator_approval(root, plan, state)
  ready <- sconjoint:::.scmix_v24_evaluator_readiness(
    plan, state_dir = state, source_root = root, require_canonical = FALSE)
  expect_identical(ready$status, "ready_for_integrity_pilot")
  expect_true(ready$pilot_outcome_generation_permitted)
  expect_false(ready$evidence_outcome_generation_permitted)
  writeLines("premature: yes", file.path(
    state, "inference-v2.4-family-approval.dcf"))
  premature <- sconjoint:::.scmix_v24_evaluator_readiness(
    plan, state_dir = state, source_root = root, require_canonical = FALSE)
  expect_identical(premature$status, "invalid_family_state")
  expect_false(premature$outcome_generation_permitted)
  expect_true(nzchar(premature$state_errors[["family"]]))
})

test_that("v2.4 toy generator resamples whole persistent sequences", {
  ids <- sprintf("r%03d", seq_len(400L))
  task_count <- rep(c(2L, 3L), 200L)
  respondent <- rep(ids, task_count)
  task <- unlist(lapply(task_count, seq_len), use.names = FALSE)
  X <- cbind(x1 = seq_along(respondent) / length(respondent),
             x2 = rep(c(-1, 1), length.out = length(respondent)))
  template <- list(
    design_id = "toy", deltaX = X, deltaX_names = colnames(X),
    Z = cbind(z = rep(seq(-1, 1, length.out = 400L), task_count)),
    Z_names = "z", respondent_id = respondent,
    task_id = as.character(task), position = task,
    subset_payload_hash = paste(rep("b", 64L), collapse = ""),
    fingerprint = paste(rep("c", 64L), collapse = ""))
  geometry <- list(
    cell_id = "toy_cell", design_id = "toy", dgp_rank = 1L,
    fixed_index = -.2 + .3 * X[, 1L], random_score = .4 * X[, 2L])
  seeds <- c(resample = 1900000001L, latent = 1900200001L,
             choice = 1900400001L, fit = 1900500001L,
             multiplier = 1900600001L)
  set.seed(818L)
  before <- .Random.seed
  first <- sconjoint:::.scmix_v24_generate_from_template(
    template, geometry, "pilot", 1L, 1L, seeds)
  after <- .Random.seed
  second <- sconjoint:::.scmix_v24_generate_from_template(
    template, geometry, "pilot", 1L, 1L, seeds)
  expect_identical(before, after)
  expect_identical(first, second)
  expect_s3_class(first, "scmix_v24_generated_outcome")
  expect_length(unique(first$respondent_id), 400L)
  expect_true(anyDuplicated(first$selected_source_respondent_id) > 0L)
  expect_true(all(first$y %in% 0:1))
  expect_false(first$application_outcomes_used)
  expect_true(sconjoint:::.scmix_v24_intact(
    first, "scmix_v24_generated_outcome"))
  for (i in seq_len(400L)) {
    got <- which(first$respondent_id == first$respondent_id_unique[[i]])
    source <- which(respondent == first$selected_source_respondent_id[[i]])
    expect_identical(first$source_row_index[got], source)
    expect_identical(first$task_id[got], as.character(task[source]))
  }
})

test_that("post-evidence family approval is accepted only mechanically", {
  root <- tempfile("v24-family-root-")
  plan <- .v24_test_plan(root, evidence_reps = 1L)
  fixture <- .v24_family_evidence_fixture(root, plan)
  result <- fixture$result
  dir.create(file.path(root, "toy"), recursive = TRUE, showWarnings = FALSE)
  rel <- file.path("toy", "family-results.csv")
  result_path <- file.path(root, rel)
  utils::write.csv(result, result_path, row.names = FALSE, na = "")
  evaluator <- fixture$evaluator
  evidence <- fixture$evidence
  raw <- c(
    `Approval-Id` = "paperps-v2.4-family-approval-v1",
    `Protocol-Id` = plan$protocol_id, `Plan-Id` = plan$plan_id,
    Status = "mechanical_family_results_recorded",
    `Approved-On` = "2026-09-07",
    `Evaluator-Approval-SHA256` = evaluator$sha256,
    `Evidence-Completion-SHA256` = evidence$sha256,
    `Approval-Matrix-SHA256` = unname(as.character(
      tools::sha256sum(plan$files[["approval"]]))),
    `Family-Result-Manifest-File` = rel,
    `Family-Result-Manifest-SHA256` = unname(as.character(
      tools::sha256sum(result_path))),
    `Approved-Families` = paste(setdiff(plan$targets$family_id, "mrs"),
                                collapse = ","),
    `Rejected-Families` = "mrs", `Discretionary-Override` = "false",
    `Application-Outcomes-Used` = "false")
  path <- file.path(root, "family.dcf")
  .v24_write_dcf(raw, path)
  out <- sconjoint:::.scmix_v24_validate_family_approval(
    path, plan, evaluator, evidence, root)
  expect_s3_class(out, "scmix_v24_family_approval")
  expect_identical(out$rejected_families, "mrs")

  forged <- result
  forged$pass[forged$family_id == "mrs" & forged$cell_id == "cell2"] <- TRUE
  utils::write.csv(forged, result_path, row.names = FALSE, na = "")
  raw[["Family-Result-Manifest-SHA256"]] <- unname(as.character(
    tools::sha256sum(result_path)))
  raw[["Approved-Families"]] <- paste(plan$targets$family_id, collapse = ",")
  raw[["Rejected-Families"]] <- "none"
  .v24_write_dcf(raw, path)
  expect_error(sconjoint:::.scmix_v24_validate_family_approval(
    path, plan, evaluator, evidence, root),
    "does not reproduce the hash-bound evidence artifacts")

  utils::write.csv(result, result_path, row.names = FALSE, na = "")
  raw[["Family-Result-Manifest-SHA256"]] <- unname(as.character(
    tools::sha256sum(result_path)))
  raw[["Approved-Families"]] <- paste(
    setdiff(plan$targets$family_id, "mrs"), collapse = ",")
  raw[["Rejected-Families"]] <- "mrs"
  .v24_write_dcf(raw, path)
  manifest_lines <- readLines(fixture$manifest_path, warn = FALSE)
  writeLines(c(manifest_lines, "tampered"), fixture$manifest_path)
  expect_error(sconjoint:::.scmix_v24_validate_family_approval(
    path, plan, evaluator, evidence, root),
    "evidence run manifest changed after completion")
  writeLines(manifest_lines, fixture$manifest_path)
  expect_identical(
    unname(as.character(tools::sha256sum(fixture$manifest_path))),
    evidence$raw[["Evidence-Run-Manifest-SHA256"]])
  artifact <- readRDS(fixture$artifact_paths[[1L]])
  artifact$family_cell$covered[[1L]] <- FALSE
  saveRDS(artifact, fixture$artifact_paths[[1L]], version = 3L)
  expect_error(sconjoint:::.scmix_v24_validate_family_approval(
    path, plan, evaluator, evidence, root),
    "artifact no longer matches its manifest")
})
