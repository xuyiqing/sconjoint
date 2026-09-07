#!/usr/bin/env Rscript

## Static, outcome-free integrity checks for the BR v2.7 target protocol.

args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1L]])
protocol_dir <- normalizePath(dirname(script), mustWork = TRUE)
repo_root <- normalizePath(file.path(protocol_dir, "..", "..", "..", ".."),
                           mustWork = TRUE)
read_csv <- function(name) utils::read.csv(
  file.path(protocol_dir, name), stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = character())
sha256 <- function(path) {
  line <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE)
  sub("[[:space:]].*$", "", line)
}
assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)

contract <- read.dcf(file.path(protocol_dir, "target-stage-contract.dcf"),
                     all = TRUE)[1L, ]
assert(identical(unname(contract[["Protocol-Id"]]),
                 "br2017-paperps-targets-2026-09-07-v2.7-v1"),
       "Protocol ID changed")
assert(identical(unname(contract[["Target-Rank"]]), "1") &&
         identical(unname(contract[["Q2-Target-Selection-Role"]]), "none") &&
         identical(unname(contract[["Formal-Release"]]),
                   "withheld_pending_confirmatory_validation"),
       "Rank or release contract changed")

frozen <- read_csv("frozen-registry-manifest.csv")
assert(all(vapply(seq_len(nrow(frozen)), function(i) {
  path <- file.path(repo_root, frozen$relative_path[[i]])
  file.exists(path) && identical(sha256(path), frozen$sha256[[i]])
}, logical(1L))), "A frozen registry or parent helper changed")

implementation <- read_csv("implementation-manifest.csv")
assert(all(vapply(seq_len(nrow(implementation)), function(i) {
  path <- file.path(repo_root, implementation$relative_path[[i]])
  file.exists(path) && identical(sha256(path), implementation$sha256[[i]])
}, logical(1L))), "Target implementation manifest mismatch")

targets <- utils::read.csv(file.path(
  repo_root, "applications/br2017/protocol/v25/target-registry.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = character())
families <- utils::read.csv(file.path(
  repo_root, "applications/br2017/protocol/v25/family-registry.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = character())
types <- read_csv("target-type-registry.csv")
mapped <- types$status[match(targets$target_type, types$target_type)]
assert(nrow(targets) == 72L && nrow(families) == 10L &&
         sum(mapped == "supported") == 66L &&
         sum(mapped == "unavailable") == 6L,
       "Frozen coverage count changed")
assert(all(targets$target_type[mapped == "unavailable"] == "standardized_ame"),
       "A non-AME target is unavailable")

seeds <- read_csv("seed-registry.csv")
assert(identical(seeds$family_id, families$family_id) &&
         all(seeds$multiplier_draws == 9999L) &&
         !anyDuplicated(c(seeds$dml_seed, seeds$multiplier_seed)),
       "Family seeds or multiplier counts changed")

lines <- readLines(file.path(protocol_dir, "CHECKSUMS.sha256"), warn = FALSE)
pieces <- strsplit(lines, "  ", fixed = TRUE)
assert(length(lines) > 0L && all(lengths(pieces) == 2L),
       "Malformed CHECKSUMS.sha256")
assert(all(vapply(pieces, function(x) {
  identical(sha256(file.path(protocol_dir, x[[2L]])), x[[1L]])
}, logical(1L))), "Protocol checksum mismatch")

cat("Ballard--Rosa v2.7 target protocol checks passed\n")
