#!/usr/bin/env Rscript

## Unit/contract tests for the add-only Section 5.1 evidence aggregator.
## These tests use synthetic temporary artifacts plus the already-completed
## post-fit fail-closed tables. They do not invoke the final aggregator or
## require the incomplete rank/profile batteries.

options(stringsAsFactors = FALSE, warn = 1)

.test_file <- function() {
  hit <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(hit)) stop("Run this test with Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", hit[[1L]]), mustWork = TRUE)
}

.expect_error <- function(expr, pattern = NULL) {
  value <- tryCatch({ force(expr); NULL }, error = identity)
  if (!inherits(value, "error")) stop("Expected an error.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(value))) {
    stop("Unexpected error: ", conditionMessage(value), call. = FALSE)
  }
  invisible(value)
}

test_file <- .test_file()
runner <- normalizePath(file.path(
  dirname(test_file), "..", "R", "10_export_section5_1_bundle_v2_1.R"),
  mustWork = TRUE)
source(runner, local = FALSE)

stopifnot(
  identical(.sw51_parse_cli(character())$output_name, "final"),
  identical(.sw51_parse_cli("--output-name=archive-01")$output_name,
            "archive-01"),
  .sw51_safe_relative(c("a.csv", "tables/b.csv")),
  !.sw51_safe_relative("/absolute.csv"),
  !.sw51_safe_relative("../escape.csv"),
  !.sw51_safe_relative("tables/../../escape.csv"))
.expect_error(.sw51_parse_cli("--profile=production"), "Only")
.expect_error(.sw51_parse_cli("--output-name=../escape"), "Only")

tmp <- tempfile("sw51-unit-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE, force = FALSE), add = TRUE)
one <- file.path(tmp, "one.txt")
writeLines("one", one)
paths <- c(one = one)
hash <- stats::setNames(unname(tools::md5sum(one)), "one")
audit <- .sw51_audit_named_paths("synthetic", "input", paths, hash)
stopifnot(nrow(audit) == 1L, audit$match, audit$exists)
.expect_error(.sw51_audit_named_paths(
  "synthetic", "input", paths,
  stats::setNames("00000000000000000000000000000000", "one")),
  "validation failed")

artifact_dir <- file.path(tmp, "artifacts")
dir.create(file.path(artifact_dir, "tables"), recursive = TRUE)
artifact <- file.path(artifact_dir, "tables", "x.csv")
writeLines("x,y\n1,2", artifact)
artifact_hash <- stats::setNames(unname(tools::md5sum(artifact)),
                                "tables/x.csv")
artifact_audit <- .sw51_audit_relative_artifacts(
  "synthetic", artifact_dir, artifact_hash)
stopifnot(nrow(artifact_audit) == 1L, artifact_audit$match)
.expect_error(.sw51_audit_relative_artifacts(
  "synthetic", artifact_dir,
  stats::setNames(artifact_hash, "../escape.csv")), "malformed")

profile_absent <- list(
  root_present = FALSE, complete = FALSE,
  available = FALSE, reportable = FALSE, status = "not_run",
  reason = "synthetic explicit not_run",
  direction_status = data.frame(
    direction = .sw51_profile_directions(), manifest_available = FALSE,
    artifact_state = "not_run", reportable_descriptive = FALSE,
    literal_likelihood_profile = FALSE,
    formal_inference_available = FALSE,
    stringsAsFactors = FALSE))
application_gates <- list(
  z19 = TRUE, interaction = TRUE, process = TRUE, position_swap = TRUE,
  completion_structural = FALSE, completion_design = TRUE)
application_component_status <- data.frame(
  component = c("postconjoint_19Z", "male_x_prior_run",
    "task_position_serial", "position_profile_swap",
    "completion_structural_refit", "completion_design_descriptive"),
  reportable_descriptive = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
  formal_inference_available = FALSE, stringsAsFactors = FALSE)
application_disposition <- .sw51_application_table_disposition(
  application_gates)
sensitivity <- list(
  validation = list(
    pass = FALSE,
    table = data.frame(
      component = c("application", "misspecification"),
      pass = c(FALSE, TRUE),
      reasons = c("completion_optimizer_gates", ""),
      stringsAsFactors = FALSE)),
  application_components = list(
    gates = application_gates,
    component_status = application_component_status,
    table_disposition = application_disposition))
claims <- .sw51_claims_ledger(profile_absent, sensitivity)
reporting <- .sw51_reporting_ledger(profile_absent, sensitivity)
guardrails <- .sw51_later_writing_guardrails(profile_absent, sensitivity)
stopifnot(
  !any(claims$formal_inference),
  !any(claims$majority_claim),
  !any(claims$exact_design_ht_claim),
  !any(claims$maintained_assumption_verified),
  !any(reporting$formal_interval_allowed),
  !any(reporting$hypothesis_test_allowed),
  !any(reporting$majority_interpretation_allowed),
  !any(reporting$exact_design_ht_interpretation_allowed),
  any(claims$claim_id == "profile_sequences" &
        claims$evidence_state == "not_run" &
        !claims$allowed_in_section_5_1),
  any(claims$claim_id == "completion_design_descriptives" &
        claims$allowed_in_section_5_1),
  any(claims$claim_id == "completion_structural_refits" &
        !claims$allowed_in_section_5_1 &
        claims$evidence_state == "unavailable_nested_objective_gate_fail"),
  all(application_disposition$reportable_descriptive[
    application_disposition$component == "completion_design_descriptive"]),
  !any(application_disposition$reportable_descriptive[
    application_disposition$component == "completion_structural_refit"]),
  any(grepl("literal profile likelihoods not constructed",
            guardrails$required_wording, fixed = TRUE)))

cwd <- normalizePath(".", mustWork = TRUE)
project <- if (file.exists(file.path(cwd, "DESCRIPTION")) &&
               dir.exists(file.path(cwd, "applications", "sw2022"))) cwd else
  normalizePath(file.path(dirname(test_file), "..", "..", "..", ".."),
                mustWork = TRUE)
results_dir <- file.path(project, "applications", "sw2022", "results")
actual_parent <- .sw51_validate_parent(file.path(
  results_dir, "mixed_logit_v2_1_postpilot_final"))
actual_sensitivity <- .sw51_validate_sensitivity(file.path(
  results_dir, "mixed_logit_v2_1_sensitivity"), actual_parent, "production")
actual_state <- .sw51_sensitivity_claim_state(actual_sensitivity)
actual_filtered <- .sw51_import_csvs(
  "sensitivity_application",
  actual_sensitivity$application$artifact_audit,
  file.path(actual_sensitivity$profile_dir, "application"),
  include_roles = actual_sensitivity$application_components$
    table_disposition$role[
      actual_sensitivity$application_components$
        table_disposition$reportable_descriptive])
stopifnot(
  !actual_sensitivity$validation$pass,
  actual_state$gated_application_components,
  actual_state$completion_design,
  !actual_state$completion_structural,
  !any(grepl("completion_(theta|choice)_comparison$",
             names(actual_filtered$tables))),
  all(c("sensitivity_application__tables__completion_sample",
        "sensitivity_application__tables__completion_amce_comparison") %in%
        names(actual_filtered$tables)))
postfit_tables <- file.path(
  results_dir,
  "postfit_evidence_v2_1", "final", "tables")
read_pf <- function(name) utils::read.csv(
  file.path(postfit_tables, paste0(name, ".csv")),
  stringsAsFactors = FALSE, check.names = FALSE)
tables <- list(
  postfit__tables__structural__sign_shares =
    read_pf("structural__sign_shares"),
  postfit__tables__design__conditional_randomization_status =
    read_pf("design__conditional_randomization_status"),
  postfit__tables__inference__status = read_pf("inference__status"),
  postfit__tables__preparation__prep_sample_flow =
    read_pf("preparation__prep_sample_flow"),
  postfit__tables__preparation__prep_moderator_dictionary =
    read_pf("preparation__prep_moderator_dictionary"),
  postfit__tables__preparation__prep_coordinate_dictionary =
    read_pf("preparation__prep_coordinate_dictionary"),
  postfit__tables__preparation__design_rank_summary =
    read_pf("preparation__design_rank_summary"),
  postfit__tables__preparation__completion_key_facts =
    read_pf("preparation__completion_key_facts"),
  postfit__tables__design__assessment_ledger =
    read_pf("design__assessment_ledger"))
for (role in application_disposition$role[
       application_disposition$reportable_descriptive]) {
  key <- paste(
    "sensitivity_application",
    gsub("[^A-Za-z0-9._-]+", "__", sub("[.]csv$", "", role,
                                         ignore.case = TRUE)), sep = "__")
  tables[[key]] <- data.frame(synthetic = TRUE)
}
component_status <- data.frame(formal_inference_available = FALSE)
gates <- .sw51_fail_closed_gate(
  component_status, claims, reporting, tables, profile_absent, sensitivity)
facts <- .sw51_sample_design_completion_facts(tables)
stopifnot(
  all(gates$pass),
  nrow(facts) >= 12L,
  facts$value[facts$fact == "primary respondents"] == "1191",
  facts$value[facts$fact == "primary tasks"] == "3573",
  facts$value[facts$fact == "profile contrast dimension"] == "13",
  facts$status[facts$fact == "exact ordered-contrast HT benchmark"] ==
    "protocol_unavailable_not_run")

bad_tables <- tables
bad_tables$postfit__tables__structural__sign_shares$majority_claim[[1L]] <-
  "majority"
.expect_error(.sw51_fail_closed_gate(
  component_status, claims, reporting, bad_tables, profile_absent,
  sensitivity),
  "fail-closed")

bad_tables <- tables
bad_tables$sensitivity_application__tables__completion_theta_comparison <-
  data.frame(synthetic = TRUE)
.expect_error(.sw51_fail_closed_gate(
  component_status, claims, reporting, bad_tables, profile_absent,
  sensitivity), "application CSV export")

status_path <- file.path(tmp, "profile_likelihood_status.csv")
utils::write.csv(data.frame(
  target = c("kappa", "female-vs-male preference"),
  status = "not_run", verified_profile = FALSE,
  stringsAsFactors = FALSE), status_path, row.names = FALSE)
sensitivity$profile_dir <- tmp
sensitivity$application <- list(
  manifest = list(profile_likelihoods = "not_run"))
sensitivity$misspecification <- list(
  manifest = list(profile_likelihoods = "not_run"))
dir.create(file.path(tmp, "application", "tables"), recursive = TRUE)
invisible(file.copy(status_path,
  file.path(tmp, "application", "tables",
            "profile_likelihood_status.csv")))
profile <- .sw51_validate_optional_profile(
  file.path(tmp, "absent_profile_root"), parent = list(),
  sensitivity = sensitivity)
stopifnot(!profile$root_present, !profile$available,
          identical(profile$status, "not_run"),
          grepl("explicitly", profile$reason))

directions <- .sw51_profile_directions()
profile_input <- file.path(tmp, "profile_input.txt")
writeLines("locked input", profile_input)
input_paths <- c(profile_input = profile_input)
input_md5 <- stats::setNames(unname(tools::md5sum(profile_input)),
                             names(input_paths))
pointer_lock <- c(pointer = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
sensitivity$validation$pointer_lock_md5 <- pointer_lock
parent <- list(
  pointer = list(reported_primary = "selected_procedure_q1"),
  pointer_read = list(md5 = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),
  manifest_read = list(md5 = "cccccccccccccccccccccccccccccccc"))
runtime <- list(R = "synthetic")
config_version <- "synthetic-profile-v2"

.write_profile_root <- function(root, completed) {
  dir.create(root, recursive = TRUE)
  authorization <- list(
    schema_version = "sw2022-v2.1-profile-sequence-authorization-v2",
    authorized = TRUE,
    purpose = "sw2022-v2.1-descriptive-penalized-criterion-profile-sequences",
    reviewed_by = "synthetic reviewer",
    config_version = config_version,
    generation_input_paths = input_paths,
    generation_input_md5 = input_md5,
    runtime_signature = runtime,
    reviewed_pointer_md5 = parent$pointer_read$md5,
    reviewed_manifest_md5 = parent$manifest_read$md5,
    reported_primary_lock_md5 = pointer_lock,
    reported_primary = parent$pointer$reported_primary,
    acknowledged_outcome_informed = TRUE,
    acknowledged_descriptive_penalized_criterion_sequences = TRUE,
    acknowledged_formal_inference_unavailable = TRUE,
    acknowledged_no_lr_critical_values = TRUE,
    acknowledged_fixed_learner_tuning_sieve = TRUE,
    formal_inference_available = FALSE, outcome_blind = FALSE)
  authorization_path <- file.path(root, "PROFILE_SEQUENCE_AUTHORIZATION.rds")
  saveRDS(authorization, authorization_path)
  authorization_md5 <- unname(tools::md5sum(authorization_path))
  for (direction in directions) {
    direction_dir <- file.path(root, direction)
    dir.create(direction_dir, recursive = TRUE)
    if (!direction %in% completed) next
    table_path <- file.path(direction_dir, "profile_sequence.csv")
    utils::write.csv(data.frame(
      grid = 1:2, loglik = c(-1, -2), direction = direction,
      verified_penalized_criterion_profile_sequence = TRUE,
      literal_likelihood_profile = FALSE,
      formal_inference_available = FALSE),
      table_path, row.names = FALSE)
    manifest <- list(
      schema_version = "sw2022-v2.1-profile-direction-manifest-v2",
      config_version = config_version, direction = direction,
      verified_penalized_criterion_profile_sequence = TRUE,
      literal_likelihood_profile = FALSE,
      unpenalized_likelihood_overlay = TRUE,
      all_nuisance_reoptimization_gates_pass = TRUE,
      learner_tuning_sieve_fixed = TRUE, retuning_performed = FALSE,
      penalized_nuisance_reoptimization = TRUE,
      unpenalized_complete_sequence_likelihood_reported = TRUE,
      descriptive_only = TRUE, formal_inference_available = FALSE,
      formal_test = FALSE, likelihood_ratio_critical_values = FALSE,
      outcome_blind = FALSE, input_paths = input_paths,
      generation_input_md5 = input_md5, runtime_signature = runtime,
      authorization_md5 = authorization_md5,
      reported_primary_lock_md5 = pointer_lock,
      artifacts = c(profile_sequence.csv =
        unname(tools::md5sum(table_path))),
      completed_at_utc = "2026-08-24 00:00:00 UTC")
    saveRDS(manifest, file.path(direction_dir, "manifest.rds"))
  }
  invisible(root)
}

partial_root <- file.path(tmp, "partial_profile_root")
.write_profile_root(partial_root, directions[1:3])
partial_profile <- .sw51_validate_optional_profile(
  partial_root, parent = parent, sensitivity = sensitivity)
stopifnot(
  partial_profile$root_present, !partial_profile$complete,
  partial_profile$available, partial_profile$reportable,
  identical(partial_profile$status, "incomplete_failed_closed"),
  sum(partial_profile$direction_status$reportable_descriptive) == 3L,
  identical(partial_profile$direction_status$artifact_state[[4L]],
            "unavailable_no_manifest_or_artifact"),
  nrow(partial_profile$absence_audit) == 1L,
  partial_profile$absence_audit$unmanifested_file_count == 0L)
.partial_claims <- .sw51_claims_ledger(partial_profile, sensitivity)
.partial_reporting <- .sw51_reporting_ledger(partial_profile, sensitivity)
.partial_guardrails <- .sw51_later_writing_guardrails(
  partial_profile, sensitivity)
stopifnot(
  .partial_claims$allowed_in_section_5_1[
    .partial_claims$claim_id == "profile_sequences"],
  .partial_reporting$point_estimate_allowed[
    .partial_reporting$quantity_family == "profile sequences"],
  any(grepl("incomplete and failed closed",
            .partial_guardrails$required_wording, fixed = TRUE)),
  all(.sw51_fail_closed_gate(
    component_status, .partial_claims, .partial_reporting, tables,
    partial_profile, sensitivity)$pass))
.sw51_revalidate_profile_absences(partial_profile)

unmanifested <- file.path(
  partial_root, "headline_contest_probability", "unexpected.rds")
saveRDS(list(partial = TRUE), unmanifested)
.expect_error(.sw51_revalidate_profile_absences(partial_profile),
              "unmanifested output")
unlink(unmanifested)

tampered <- file.path(partial_root, "kappa", "profile_sequence.csv")
writeLines("tampered", tampered)
.expect_error(.sw51_validate_optional_profile(
  partial_root, parent = parent, sensitivity = sensitivity),
  "validation failed")

complete_root <- file.path(tmp, "complete_profile_root")
.write_profile_root(complete_root, directions)
complete_profile <- .sw51_validate_optional_profile(
  complete_root, parent = parent, sensitivity = sensitivity)
stopifnot(
  complete_profile$root_present, complete_profile$complete,
  complete_profile$available, complete_profile$reportable,
  identical(complete_profile$status,
            "completed_descriptive_penalized_criterion_sequences"),
  identical(names(complete_profile$manifests), directions),
  nrow(complete_profile$artifact_audit) == 4L)

message("Section 5.1 bundle unit/contract tests passed.")
