#!/usr/bin/env Rscript

## LOCAL RELOCALIZATION (Yiqing, 2026-08-26) --- not part of Avidit's
## shipped pipeline. The shipped failed-pilot manifest binds absolute
## paths and the runtime signature of the authoring machine, so the
## fail-closed authorization creator cannot validate it anywhere else.
## This script re-attests ONLY the location and runtime context:
##   - input_paths are re-rooted from the authoring machine's tree to the
##     package root this script is run from;
##   - runtime_signature is recomputed for the current runtime;
##   - every attested generation-input md5 must match the local bytes
##     EXACTLY, or the script refuses to write (no content re-attestation);
##   - the original manifest is preserved beside the rewritten one.
## The pilot outcome fields (pilot_success = FALSE, outcome_blind = FALSE,
## formal_inference_available = FALSE, ...) are untouched. Portable
## re-attestation semantics remain an open upstream ask.

options(stringsAsFactors = FALSE, warn = 1)

root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run from the package root.", call. = FALSE)
}
suppressMessages(pkgload::load_all(root, quiet = TRUE))

manifest_path <- file.path(root, "applications", "sw2022", "results",
                           "mixed_logit_v2", "pilot", "manifest.rds")
manifest <- readRDS(manifest_path)

old_root <- "/Users/avidit/Dropbox/Codex/Conjoint/sconjoint-mixed-logit"
if (!all(startsWith(manifest$input_paths, paste0(old_root, "/")))) {
  stop("Unexpected input-path root in the shipped manifest.", call. = FALSE)
}
rel <- sub(paste0("^", old_root, "/"), "", manifest$input_paths)
local_paths <- file.path(root, rel)
names(local_paths) <- names(manifest$input_paths)

missing <- !file.exists(local_paths)
if (any(missing)) {
  stop("Missing local inputs:\n  ",
       paste(rel[missing], collapse = "\n  "), call. = FALSE)
}
local_md5 <- .sc_md5_paths(local_paths)
same <- unname(local_md5) == unname(manifest$generation_input_md5)
if (!all(same)) {
  stop("Local bytes differ from the attested generation for:\n  ",
       paste(rel[!same], collapse = "\n  "),
       "\nRefusing to relocalize.", call. = FALSE)
}

preserved <- paste0(manifest_path, ".avidit-original")
if (!file.exists(preserved)) file.copy(manifest_path, preserved)

manifest$input_paths <- local_paths
manifest$runtime_signature <-
  .sc_runtime_signature(file.path(root, "DESCRIPTION"))
manifest$relocalized <- list(
  by = "yiqing", on_utc = "2026-08-26",
  from_root = old_root, to_root = root,
  note = paste("Paths and runtime re-attested locally; all",
               length(local_paths),
               "generation-input md5s verified byte-identical",
               "to the attested generation before rewriting."))

tmp <- tempfile("manifest-", tmpdir = dirname(manifest_path))
saveRDS(manifest, tmp, version = 3)
if (!file.rename(tmp, manifest_path)) stop("Atomic write failed.")

check <- readRDS(manifest_path)
rt <- .sc_runtime_signature(file.path(root, "DESCRIPTION"))
cat("relocalized:", manifest_path, "\n")
cat("runtime match now:", identical(check$runtime_signature, rt), "\n")
cat("inputs exist:", all(file.exists(check$input_paths)),
    "| md5 verified:", all(same), "\n")
