# Fail-closed provenance helpers -------------------------------------------

.sc_md5_paths <- function(paths) {
  if (!is.character(paths) || !length(paths) || is.null(names(paths)) ||
      any(!nzchar(names(paths))) || anyDuplicated(names(paths)) ||
      anyNA(paths) || any(!file.exists(paths))) {
    stop("Provenance paths must be an existing, uniquely named character vector.",
         call. = FALSE)
  }
  out <- unname(tools::md5sum(paths))
  names(out) <- names(paths)
  out
}

.sc_identical_md5_vectors <- function(x, y) {
  is.character(x) && is.character(y) &&
    !is.null(names(x)) && !is.null(names(y)) &&
    !anyDuplicated(names(x)) && !anyDuplicated(names(y)) &&
    identical(names(x), names(y)) &&
    identical(unname(x), unname(y))
}

.sc_runtime_signature <- function(description_path) {
  if (!is.character(description_path) || length(description_path) != 1L ||
      !file.exists(description_path)) {
    stop("A package DESCRIPTION is required for the runtime signature.",
         call. = FALSE)
  }
  description <- read.dcf(description_path)
  if (!"Version" %in% colnames(description)) {
    stop("The package DESCRIPTION has no Version field.", call. = FALSE)
  }
  list(
    R_version = R.version.string,
    platform = R.version$platform,
    torch_version = as.character(utils::packageVersion("torch")),
    pkgload_version = as.character(utils::packageVersion("pkgload")),
    sconjoint_source_version = unname(description[1L, "Version"])
  )
}

.sc_manifest_artifacts_valid <- function(manifest, manifest_path) {
  artifacts <- manifest$artifacts
  if (!is.character(artifacts) || !length(artifacts) ||
      is.null(names(artifacts)) || any(!nzchar(names(artifacts))) ||
      anyDuplicated(names(artifacts)) || anyNA(artifacts) ||
      any(basename(names(artifacts)) != names(artifacts)) ||
      !is.character(manifest_path) || length(manifest_path) != 1L) {
    return(FALSE)
  }
  paths <- file.path(dirname(manifest_path), names(artifacts))
  names(paths) <- names(artifacts)
  if (any(!file.exists(paths))) return(FALSE)
  .sc_identical_md5_vectors(.sc_md5_paths(paths), artifacts)
}

.sc_reviewed_pilot_authorized <- function(authorization,
                                          reviewed_manifest,
                                          current_generation_md5,
                                          current_runtime_signature,
                                          config_version,
                                          current_config_md5,
                                          current_manifest_md5,
                                          reviewed_manifest_path) {
  is.list(authorization) && is.list(reviewed_manifest) &&
    identical(authorization$authorized, TRUE) &&
    identical(authorization$config_version, config_version) &&
    identical(as.character(authorization$config_md5),
              as.character(current_config_md5)) &&
    identical(as.character(authorization$reviewed_pilot_manifest_md5),
              as.character(current_manifest_md5)) &&
    identical(
      reviewed_manifest$schema_version,
      "sw2022-mixed-logit-v2-penalty-pilot-manifest-v1") &&
    identical(reviewed_manifest$profile, "pilot") &&
    identical(reviewed_manifest$configuration_version, config_version) &&
    identical(reviewed_manifest$pilot_success, TRUE) &&
    identical(reviewed_manifest$all_generation_inputs_unchanged, TRUE) &&
    identical(reviewed_manifest$frozen_v1_artifacts_unchanged, TRUE) &&
    identical(reviewed_manifest$formal_inference_available, FALSE) &&
    identical(reviewed_manifest$production_result, FALSE) &&
    identical(reviewed_manifest$outcome_blind, FALSE) &&
    .sc_manifest_artifacts_valid(reviewed_manifest,
                                 reviewed_manifest_path) &&
    identical(authorization$reviewed_runtime_signature,
              reviewed_manifest$runtime_signature) &&
    identical(reviewed_manifest$runtime_signature,
              current_runtime_signature) &&
    .sc_identical_md5_vectors(
      authorization$reviewed_generation_input_md5,
      reviewed_manifest$generation_input_md5) &&
    .sc_identical_md5_vectors(
      reviewed_manifest$generation_input_md5,
      current_generation_md5)
}
