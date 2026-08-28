#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script <- if (length(.script_arg)) {
  normalizePath(sub("^--file=", "", .script_arg[[1L]]), mustWork = TRUE)
} else normalizePath(
  "/private/tmp/sw_v21_postfit_patch/applications/sw2022/v2_1/tests/test_postfit_helpers_v2_1.R",
  mustWork = TRUE)
source(file.path(dirname(.script), "..", "R", "postfit_helpers_v2_1.R"),
       local = FALSE)
source(file.path(dirname(.script), "..", "config",
                 "postfit_evidence_config_v2_1.R"), local = FALSE)

.expect_error <- function(code) {
  out <- tryCatch({ force(code); FALSE }, error = function(e) TRUE)
  stopifnot(out)
}

.save <- function(x, path) saveRDS(x, path, version = 3)

tmp <- tempfile("sw-v21-pointer-test-")
dir.create(tmp)
input <- file.path(tmp, "generation-input.txt")
writeLines("frozen input", input)
generation <- stats::setNames(.swv21_md5(input), "input")
producer_config <- list(
  version = sw_v21_postfit_config$producer_config_version)

stamp <- function(role) list(
  sw_v21_application_specification = list(
    config_version = producer_config$version, role = role,
    generation_input_md5 = generation, runtime_signature = "runtime",
    authorization_md5 = "authorization", formal_inference_available = FALSE,
    outcome_blind = FALSE, production_result = FALSE))
fit_paths <- c(
  full = file.path(tmp, "fit_selected_full.rds"),
  nested = file.path(tmp, "fit_selected_nested.rds"),
  assembled = file.path(tmp, "fit_selected_assembled.rds"))
.save(stamp("selected_full"), fit_paths[["full"]])
.save(stamp("selected_nested"), fit_paths[["nested"]])
.save(stamp("selected_assembled"), fit_paths[["assembled"]])

pointer <- list(
  schema_version = sw_v21_postfit_config$producer_pointer_schema,
  reported_primary = "selected_procedure_q1", fallback_applied = FALSE,
  full_fit_path = fit_paths[["full"]],
  nested_fit_path = fit_paths[["nested"]],
  assembled_fit_path = fit_paths[["assembled"]],
  selected_procedure_paths = as.list(fit_paths),
  exact_constant_paths = as.list(fit_paths),
  descriptive_only = TRUE, formal_test = FALSE,
  formal_inference_available = FALSE, outcome_blind = FALSE,
  generation_input_md5 = generation, runtime_signature = "runtime",
  authorization_md5 = "authorization")
pointer_path <- file.path(tmp, "reported_primary_pointer.rds")
.save(pointer, pointer_path)
artifact_paths <- c(fit_paths, pointer = pointer_path)
names(artifact_paths) <- basename(artifact_paths)
manifest <- list(
  schema_version = sw_v21_postfit_config$producer_manifest_schema,
  configuration_version = producer_config$version,
  final_analysis_success = TRUE, procedural_primary_available = TRUE,
  reported_primary = pointer$reported_primary,
  fallback_applied = pointer$fallback_applied,
  input_paths = stats::setNames(input, "input"),
  generation_input_md5 = generation, completion_input_md5 = generation,
  runtime_signature = "runtime", authorization_md5 = "authorization",
  artifacts = .swv21_hash_paths(artifact_paths),
  formal_inference_available = FALSE, outcome_blind = FALSE,
  production_result = FALSE)
.save(manifest, file.path(tmp, "manifest.rds"))

snapshot <- .swv21_resolve_reported_primary(
  tmp, producer_config, sw_v21_postfit_config)
stopifnot(
  identical(snapshot$reported_primary, "selected_procedure_q1"),
  identical(snapshot$fallback_applied, FALSE),
  identical(snapshot$resolved_once, TRUE),
  identical(names(snapshot$fit_paths), c("full", "nested", "assembled")))
.swv21_assert_resolution_unchanged(snapshot)
for (nm in names(fit_paths)) {
  fit <- readRDS(fit_paths[[nm]])
  .swv21_validate_fit_stamp(
    fit, paste0("selected_", nm), snapshot, producer_config)
}

## A pointer cannot be changed after the completed manifest is written.
bad <- pointer
bad$reported_primary <- "exact_constant_q1"
.save(bad, pointer_path)
.expect_error(.swv21_resolve_reported_primary(
  tmp, producer_config, sw_v21_postfit_config))
.save(pointer, pointer_path)
stopifnot(identical(.swv21_md5(pointer_path),
                    manifest$artifacts[["reported_primary_pointer.rds"]]))

## A frozen generation input cannot drift after fit completion.
writeLines("changed input", input)
.expect_error(.swv21_resolve_reported_primary(
  tmp, producer_config, sw_v21_postfit_config))
writeLines("frozen input", input)
stopifnot(identical(.swv21_md5(input), generation[[1L]]))

coordinate_names <- c(
  "cand_genderMale", "cand_runYes", "cand_talentCollaborative",
  "cand_talentDetermined.to.Succeed", "cand_talentEmpathetic",
  "cand_talentGood.Communicator", "cand_talentHard.Working",
  "cand_talentTough.Negotiator", "cand_agendaModerate.Changes",
  "cand_agendaComplete.Overhaul", "cand_child1.child",
  "cand_child2.children", "cand_child3.children")
contrasts <- .swv21_contrasts(coordinate_names)
stopifnot(
  length(contrasts) == 13L,
  identical(contrasts$female_vs_male[[1L]], -1),
  identical(sum(abs(contrasts$talent_collaborative_vs_empathetic)), 2),
  identical(contrasts$agenda_complete_vs_very_few[[10L]], 1))
.expect_error(.swv21_contrasts(rev(coordinate_names)))

## The respondent-clustered benchmark recovers a noiseless coefficient.
dx <- matrix(c(0, 1, 0, 1), ncol = 1L)
y <- c(0.2, 0.7, 0.2, 0.7)
fit <- .swv21_cluster_lm(y, cbind(1, dx), c("a", "a", "b", "b"))
stopifnot(abs(fit$coef[[2L]] - 0.5) < 1e-12)

## Diagnostic-only assembly does not receive the package's ordinary-inference
## verified-heldout flag, so the application contract proves the narrower
## outer-training construction mechanically and keeps eligibility withheld.
prepared_cf <- list(
  deltaX = matrix(0, 6L, 1L),
  respondent_id = rep(c("a", "b"), each = 3L))
gate <- list(candidate_selection_gate = list(pass = TRUE))
nested_cf <- list(
  outer_fold_id = rep(c(3L, 5L), each = 3L),
  outer_folds = c("3", "5"), tuning = list(gate, gate),
  candidate_selection_gate_by_outer_fold = c(TRUE, TRUE),
  nesting = "each tuning and selected refit used outer-training respondents only")
assembled_cf <- list(
  fold_id = rep(1:2, each = 3L), diagnostic_only = TRUE,
  eligible_for_ordinary_inference = FALSE,
  source = "selected refits from respondent-level nested tuning",
  optimization = list(
    gate_by_fold = c(TRUE, TRUE),
    candidate_selection_gate_by_fold = c(TRUE, TRUE),
    nested_objective_gate_by_fold = c(TRUE, TRUE),
    pooled_prefit_gate_by_fold = c(TRUE, TRUE),
    continued_constant_gate_by_fold = c(TRUE, TRUE),
    compact_bound_gate_by_fold = c(TRUE, TRUE)))
predictions_cf <- list(
  out_of_fold = TRUE, complete_sequence = TRUE,
  shared_factor_within_sequence = TRUE, training_only_tuning = FALSE,
  provenance = "fold-specific nuisance fits evaluated out of fold",
  note = "one shared factor per complete sequence")
assessment_cf <- list(
  score = list(verified_heldout = FALSE),
  calibration = list(verified_heldout = FALSE))
contract <- .swv21_crossfit_contract(
  nested_cf, assembled_cf, prepared_cf, predictions_cf, assessment_cf)
stopifnot(contract$validated, !contract$package_verified_heldout,
          !contract$ordinary_inference_eligible, all(contract$checks$pass))
nested_cf$outer_fold_id[[2L]] <- 5L
bad_contract <- .swv21_crossfit_contract(
  nested_cf, assembled_cf, prepared_cf, predictions_cf, assessment_cf)
stopifnot(!bad_contract$validated)

cat("v2.1 post-fit helper tests passed\n")
