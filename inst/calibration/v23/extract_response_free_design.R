## Frozen v2.3 importer for the three Step-3 response-free design payloads.
## This file deliberately has no package dependency so it can be audited and
## run before loading sconjoint. The source object may contain an outcome, but
## the whitelist makes that field unreachable from the returned payload.

scmix_v23_extract_response_free_design <- function(
    source_path, whitelist_path, output_path = NULL) {
  source_path <- normalizePath(source_path, mustWork = TRUE)
  whitelist_path <- normalizePath(whitelist_path, mustWork = TRUE)
  rule <- as.list(read.dcf(whitelist_path)[1L, , drop = TRUE])
  source_hash <- unname(as.character(tools::sha256sum(source_path)))
  if (!identical(source_hash, rule[["Source-SHA256"]])) {
    stop("Source SHA-256 does not match the frozen design whitelist.",
         call. = FALSE)
  }
  source <- readRDS(source_path)
  if (!is.list(source) ||
      !identical(as.character(source$schema_version), rule[["Source-Schema"]])) {
    stop("Source schema does not match the frozen design whitelist.",
         call. = FALSE)
  }
  allowed <- strsplit(rule[["Allowed-Source-Fields"]], ",", fixed = TRUE)[[1L]]
  if (any(!allowed %in% names(source))) {
    stop("A whitelisted response-free source field is absent.", call. = FALSE)
  }
  selected <- source[allowed]
  rm(source)
  position <- if (identical(rule[["Position-Field"]], "none")) NULL else
    unname(selected[[rule[["Position-Field"]]]])
  deltaX <- as.matrix(selected[[rule[["DeltaX-Field"]]]])
  Z <- as.matrix(selected[[rule[["Z-Field"]]]])
  storage.mode(deltaX) <- storage.mode(Z) <- "double"
  payload <- list(
    deltaX = unname(deltaX), deltaX_names = colnames(deltaX),
    Z = unname(Z), Z_names = colnames(Z),
    respondent_id = as.character(selected[[rule[["Respondent-Field"]]]]),
    task_id = as.character(selected[[rule[["Task-Field"]]]]),
    position = position)
  forbidden <- strsplit(
    rule[["Forbidden-Output-Fields"]], ",", fixed = TRUE)[[1L]]
  keys <- tolower(gsub("[^[:alnum:]]", "", names(payload)))
  if (any(keys %in% tolower(gsub("[^[:alnum:]]", "", forbidden)))) {
    stop("The extracted payload contains an outcome-like field.",
         call. = FALSE)
  }
  ## The manifest hash is over uncompressed saveRDS bytes, not serialize()
  ## bytes, so reproduce that exact contract before release.
  temp <- tempfile("scmix-step3-design-", fileext = ".rds")
  on.exit(unlink(temp), add = TRUE)
  saveRDS(payload, temp, version = 3L, compress = FALSE)
  payload_hash <- unname(as.character(tools::sha256sum(temp)))
  if (!identical(payload_hash, rule[["Response-Free-Payload-SHA256"]])) {
    stop("Extracted response-free payload does not match its frozen SHA-256.",
         call. = FALSE)
  }
  out <- list(
    design_id = rule[["Design-Id"]], source_sha256 = source_hash,
    response_free_payload_sha256 = payload_hash,
    payload = payload, response_free = TRUE,
    outcome_fields_stored = character())
  if (!is.null(output_path)) {
    if (!is.character(output_path) || length(output_path) != 1L ||
        !nzchar(output_path)) {
      stop("`output_path` must be NULL or one nonempty path.", call. = FALSE)
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, output_path, version = 3L, compress = "xz")
  }
  out
}
