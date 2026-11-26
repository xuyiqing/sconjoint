###############################################################################
# data-raw/build_sysdata.R
#
# Build R/sysdata.rda: the paper's cross-fitting folds and per-fold
# validation respondents for the two applications, read by sc_example_data().
#
#   folds: G_inputs.rds (analysis/2026-11-04_rum_gsbr_linear/results/<app>/),
#          field `fold` (integer 1-5, in respondent order `ids`).
#   validation: the `val` field of each fold's fit file (positions in `ids`
#          of the training respondents held out for early stopping):
#          GS  analysis/2026-11-14_rum_qfactor/results/s3/fits/fit_gs_net_q3_f<k>.rds
#          BR  analysis/2026-11-14_rum_qfactor/results/s3b/fits_q4/fit_br_net_q4_f<k>.rds
#
# Neither can be derived from the data, so the vectors are shipped. Group
# variables (pid7, party, ideology, partisans and leaners) are rebuilt from
# the bundled datasets in sc_example_data() and are not stored here.
#
# Set SCONJOINT_PAPER_ROOT to the paper project folder.
# Run from the package root:  Rscript data-raw/build_sysdata.R
###############################################################################

options(stringsAsFactors = FALSE, warn = 1)

root <- normalizePath(Sys.getenv(
  "SCONJOINT_PAPER_ROOT",
  file.path("~", "Dropbox", "Research_Hub", "Projects", "ConjointStructural")),
  mustWork = TRUE)
g_file <- function(app) file.path(root, "analysis", "2026-11-04_rum_gsbr_linear",
                                  "results", app, "G_inputs.rds")
fit_file <- function(app, k) {
  res <- file.path(root, "analysis", "2026-11-14_rum_qfactor", "results")
  if (app == "gs") file.path(res, "s3", "fits", sprintf("fit_gs_net_q3_f%d.rds", k))
  else file.path(res, "s3b", "fits_q4", sprintf("fit_br_net_q4_f%d.rds", k))
}

one_app <- function(app) {
  G <- readRDS(g_file(app))
  fold <- as.integer(G$fold)
  ids <- as.character(G$ids)
  stopifnot(identical(ids, G$data$ids), length(fold) == length(ids),
            setequal(fold, 1:5))
  validation <- lapply(1:5, function(k) {
    f <- readRDS(fit_file(app, k))
    stopifnot(identical(sort(as.integer(f$te)), which(fold == k)),
              identical(sort(as.integer(f$tr)), which(fold != k)),
              all(f$val %in% f$tr), !anyDuplicated(f$val))
    as.integer(f$val)
  })
  names(validation) <- paste0("fold", 1:5)
  list(ids = ids, fold = fold, validation = validation,
       source = list(
         fold = "G_inputs.rds (analysis/2026-11-04_rum_gsbr_linear)",
         validation = if (app == "gs") "fit_gs_net_q3_f<k>.rds$val (analysis/2026-11-14_rum_qfactor/results/s3/fits)"
                      else "fit_br_net_q4_f<k>.rds$val (analysis/2026-11-14_rum_qfactor/results/s3b/fits_q4)"))
}

.sc_paper_splits <- list(gs = one_app("gs"), br = one_app("br"))
for (a in names(.sc_paper_splits)) {
  s <- .sc_paper_splits[[a]]
  message(sprintf("%s: n = %d, fold sizes %s, validation sizes %s", a, length(s$ids),
                  paste(tabulate(s$fold), collapse = "/"),
                  paste(lengths(s$validation), collapse = "/")))
}
save(.sc_paper_splits, file = file.path("R", "sysdata.rda"), compress = "bzip2")
