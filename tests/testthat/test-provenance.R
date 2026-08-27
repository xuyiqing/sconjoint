test_that("generation hashes react to any named source change", {
  td <- tempfile("scmix-provenance-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  paths <- c(
    runner = file.path(td, "runner.R"),
    unrelated_package_source = file.path(td, "unrelated.R"))
  writeLines("runner <- TRUE", paths[["runner"]])
  writeLines("unrelated <- 1", paths[["unrelated_package_source"]])

  before <- .sc_md5_paths(paths)
  expect_true(.sc_identical_md5_vectors(before, before))
  writeLines("unrelated <- 2", paths[["unrelated_package_source"]])
  after <- .sc_md5_paths(paths)
  expect_false(.sc_identical_md5_vectors(before, after))
})

test_that("production authorization binds the reviewed full hash vector", {
  td <- tempfile("scmix-reviewed-pilot-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  artifact_path <- file.path(td, "pilot-result.rds")
  saveRDS(list(ok = TRUE), artifact_path)
  artifact_hash <- unname(tools::md5sum(artifact_path))
  current <- c(config = "aaa", runner = "bbb",
               `package_source:unrelated.R` = "ccc")
  manifest <- list(
    schema_version = "sw2022-mixed-logit-v2-penalty-pilot-manifest-v1",
    profile = "pilot", configuration_version = "v2",
    pilot_success = TRUE, all_generation_inputs_unchanged = TRUE,
    frozen_v1_artifacts_unchanged = TRUE,
    formal_inference_available = FALSE, production_result = FALSE,
    outcome_blind = FALSE,
    runtime_signature = list(R_version = "R-test", torch_version = "1"),
    generation_input_md5 = current,
    artifacts = c(`pilot-result.rds` = artifact_hash))
  manifest_path <- file.path(td, "manifest.rds")
  saveRDS(manifest, manifest_path)
  manifest_hash <- unname(tools::md5sum(manifest_path))
  authorization <- list(
    authorized = TRUE, config_version = "v2", config_md5 = "aaa",
    reviewed_pilot_manifest_md5 = manifest_hash,
    reviewed_runtime_signature = manifest$runtime_signature,
    reviewed_generation_input_md5 = current)
  gate <- function(auth = authorization, reviewed = manifest,
                   execution = current) {
    .sc_reviewed_pilot_authorized(
      authorization = auth, reviewed_manifest = reviewed,
      current_generation_md5 = execution,
      current_runtime_signature = manifest$runtime_signature,
      config_version = "v2",
      current_config_md5 = "aaa", current_manifest_md5 = manifest_hash,
      reviewed_manifest_path = manifest_path)
  }

  expect_true(gate())
  changed <- current
  changed[["package_source:unrelated.R"]] <- "changed"
  expect_false(gate(execution = changed))
  stale_auth <- authorization
  stale_auth$reviewed_generation_input_md5 <- changed
  expect_false(gate(auth = stale_auth))
  failed_manifest <- manifest
  failed_manifest$pilot_success <- FALSE
  expect_false(gate(reviewed = failed_manifest))
  stale_runtime <- manifest
  stale_runtime$runtime_signature$torch_version <- "2"
  expect_false(gate(reviewed = stale_runtime))
  malformed <- manifest
  malformed$schema_version <- "wrong"
  expect_false(gate(reviewed = malformed))

  saveRDS(list(ok = FALSE), artifact_path)
  expect_false(gate())
  unlink(artifact_path)
  expect_false(gate())
})

test_that("runtime signatures contain the resumable numerical stack", {
  signature <- .sc_runtime_signature(test_path("..", "..", "DESCRIPTION"))
  expect_named(signature, c(
    "R_version", "platform", "torch_version", "pkgload_version",
    "sconjoint_source_version"))
  expect_true(all(nzchar(unlist(signature, use.names = FALSE))))
})
