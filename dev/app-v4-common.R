# Shared machinery for the v4 estimand-menu scripts (E2).
# Sourced from the repo root by dev/app-v4-quantities-*.R.
#
# Slot cache: each quantity lands in OUT_DIR/v4_quantities_<app>.rds under
# a named slot, saved the moment it is computed; present slots are skipped
# on rerun, so a crash or an interrupted run never repeats finished work.

suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))
options(warn = 1)  # stream warnings into the log at the point they fire

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
REPL_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/replicate")
stopifnot(dir.exists(OUT_DIR), dir.exists(REPL_DIR))

ts <- function() format(Sys.time(), "[%H:%M:%S]")
say <- function(...) cat(ts(), sprintf(...), "\n")

## slot cache -----------------------------------------------------------------
.slot_env <- new.env(parent = emptyenv())

slots_open <- function(app) {
  .slot_env$path <- file.path(OUT_DIR, sprintf("v4_quantities_%s.rds", app))
  .slot_env$Q <- if (file.exists(.slot_env$path)) readRDS(.slot_env$path)
    else list()
  invisible()
}

slot <- function(name, expr) {
  if (!is.null(.slot_env$Q[[name]])) {
    say("slot %-24s cached", name)
    return(invisible(.slot_env$Q[[name]]))
  }
  say("slot %-24s computing", name)
  t0 <- Sys.time()
  val <- expr
  .slot_env$Q[[name]] <- val
  saveRDS(.slot_env$Q, .slot_env$path)
  say("slot %-24s done: %.1f min", name,
      as.numeric(difftime(Sys.time(), t0, units = "mins")))
  invisible(val)
}

slot_get <- function(name) .slot_env$Q[[name]]

## respondent metadata joins --------------------------------------------------
## Always join by respondent id against the frozen replicate prep tables;
## never positional.
resp_meta_for <- function(app, fit) {
  mats <- readRDS(file.path(REPL_DIR, "data", "derived", app,
                            "prep_matrices.rds"))
  ids <- as.character(unique(fit$respondent_id))
  meta <- mats$resp_meta[match(ids, as.character(mats$resp_meta$respondent)), ]
  stopifnot(nrow(meta) == length(ids), !anyNA(meta$respondent),
            identical(as.character(meta$respondent), ids))
  meta
}

## production party cut (BR: pid7 >= 8 -> Independent)
party3_from_pid7 <- function(pid7) {
  pidc <- pid7
  pidc[pidc >= 8] <- 4
  factor(cut(pidc, c(-Inf, 3.5, 4.5, 7.5),
             labels = c("Democrat", "Independent", "Republican")))
}

## production ideology terciles (GS)
tercile_from_ideo7 <- function(ideo7) {
  cut(ideo7, c(0, 3, 4, 7), labels = c("Liberal", "Moderate", "Conservative"))
}

## per-fold index-scale residual SDs (the sw2022 fold-spread display)
fold_index_sd <- function(fit) {
  vapply(fit$A_folds, function(A) sqrt(mean((fit$deltaX %*% A)^2)),
         numeric(1L))
}

## subgroup means and SEs of a respondent-level signal vector
group_stats <- function(v, g) {
  do.call(rbind, lapply(base::levels(g), function(lev) {
    x <- v[g == lev]
    data.frame(group = lev, n = length(x), estimate = mean(x),
               se = stats::sd(x) / sqrt(length(x)),
               stringsAsFactors = FALSE)
  }))
}
