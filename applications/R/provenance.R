## Artifact provenance for the application layer (audit work package D4).
##
## Every released number must carry enough of its own history that a reader
## can tell WHICH code, WHICH fit, and WHICH calibration produced it, and
## what status the producer assigned. Before this file the application CSVs
## carried the numbers and (sometimes) a status string, and the report named
## a code-of-record commit by hand -- which then went stale.
##
## Base R + digest. Source alongside the runner that writes the artifact.

prov_git_commit <- function(root = path.expand("~/GitHub/sconjoint")) {
  out <- tryCatch(
    suppressWarnings(system2("git", c("-C", shQuote(root), "rev-parse",
                                      "--short", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) NA_character_)
  if (!length(out) || is.na(out[[1]]) || !nzchar(out[[1]])) return(NA_character_)
  dirty <- tryCatch(
    length(suppressWarnings(system2("git", c("-C", shQuote(root), "status",
                                             "--porcelain"),
                                    stdout = TRUE, stderr = FALSE))) > 0L,
    error = function(e) NA)
  paste0(out[[1]], if (isTRUE(dirty)) "-dirty" else "")
}

prov_file_hash <- function(path) {
  if (!length(path) || !file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

prov_object_hash <- function(x) {
  if (is.null(x)) return(NA_character_)
  tryCatch(digest::digest(x, algo = "sha256"), error = function(e) NA_character_)
}

## Source hashes for the files whose code produced the artifact.
prov_source_hashes <- function(files,
                               root = path.expand("~/GitHub/sconjoint")) {
  paths <- ifelse(file.exists(files), files, file.path(root, files))
  paste(sprintf("%s=%s", basename(paths),
                substr(vapply(paths, prov_file_hash, character(1L)), 1L, 12L)),
        collapse = "; ")
}

## Stamp a bounds/estimand table with provenance columns. Returns the table.
##   `fit`         the assembled (or full) fit object the numbers came from.
##   `calibration` the dispersion calibration object, or NULL.
##   `target_label` a short name for the estimand family.
##   `sources`     character vector of code files (repo-relative or absolute).
sb_stamp_provenance <- function(tab, app, profile, fit = NULL,
                                calibration = NULL, seed = NA_integer_,
                                producer = NA_character_,
                                target_label = "directional_share_bound",
                                sources = c("applications/R/share_bounds.R",
                                            "applications/R/contrast_bounds.R"),
                                root = path.expand("~/GitHub/sconjoint")) {
  tab$prov_application <- app
  tab$prov_profile <- profile
  tab$prov_commit <- prov_git_commit(root)
  tab$prov_producer <- producer
  tab$prov_target_label <- target_label
  tab$prov_seed <- seed
  tab$prov_source_hashes <- prov_source_hashes(sources, root)
  tab$prov_fit_hash <- prov_object_hash(fit)
  tab$prov_fit_signature <- if (is.null(fit$analysis_signature)) NA_character_
    else as.character(fit$analysis_signature)
  tab$prov_calibration_hash <- prov_object_hash(calibration)
  tab$prov_calibration_R <- if (is.null(calibration$R)) NA_integer_ else
    as.integer(calibration$R)
  tab$prov_calibration_gamma <- if (is.null(calibration$gamma)) NA_real_ else
    as.numeric(calibration$gamma)
  tab$prov_written_utc <- format(Sys.time(), tz = "UTC",
                                 "%Y-%m-%dT%H:%M:%SZ")
  tab
}

## Write a one-row provenance manifest beside an artifact directory. Use it
## for outputs whose natural shape is not a data frame with room for extra
## columns (figures, RDS objects, multi-file estimand dumps).
## `name` must be unique per PRODUCER, not per directory: several runners
## write into the same estimands/ directory, and a shared
## "provenance.csv" meant the last one to finish silently replaced the
## others' manifest (found 2026-08-28).
prov_write_manifest <- function(dir, entries, name = "provenance.csv") {
  stopifnot(is.list(entries))
  df <- data.frame(key = names(entries),
                   value = vapply(entries, function(v)
                     paste(as.character(v), collapse = " | "), character(1L)),
                   stringsAsFactors = FALSE)
  path <- file.path(dir, name)
  utils::write.csv(df, path, row.names = FALSE)
  path
}
