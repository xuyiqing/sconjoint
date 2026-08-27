## Package-local mutable state. Used for once-per-session warnings so
## that informational warnings (e.g. the Stage-2 downgrade for non-DNN
## learners) fire the first time they apply but do not nag on every call.
.sc_state <- new.env(parent = emptyenv())
.sc_state$warned <- character()

#' Emit a warning at most once per session, keyed by `id`
#'
#' @param id Character key identifying the warning site.
#' @param msg Character message passed to `warning()`.
#' @keywords internal
#' @noRd
.sc_warn_once <- function(id, msg) {
  if (id %in% .sc_state$warned) {
    return(invisible(FALSE))
  }
  .sc_state$warned <- c(.sc_state$warned, id)
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

#' Emit a message at most once per session, keyed by `id`
#'
#' Like `.sc_warn_once()` but for informational notes (e.g. the
#' population-claim scope note on the plug-in distributional
#' functions), which should orient the user without polluting every
#' call with a condition.
#' @keywords internal
#' @noRd
.sc_note_once <- function(id, msg) {
  if (id %in% .sc_state$warned) {
    return(invisible(FALSE))
  }
  .sc_state$warned <- c(.sc_state$warned, id)
  message(msg)
  invisible(TRUE)
}

#' Standard population-claim scope note for plug-in distributional
#' functions (correspondence-table rule; estimand memo P5)
#' @keywords internal
#' @noRd
.sc_population_claim_note <- function(fn) {
  .sc_note_once(paste0("popclaim_", fn), paste0(
    fn, "(): describes the fitted respondent-level (MAP) estimates. ",
    "It is a legacy descriptive output and is not a paperps estimand or ",
    "inference procedure. Use scmix(), the scmix_paper_* quantity helpers, ",
    "scmix_dml(), and the Section 4 assessment gates for the rebuilt paper. ",
    "This note prints once per session."))
}

.onAttach <- function(libname, pkgname) {
  # sconjoint will depend on 'torch' once the DNN backend lands in M2.
  # During M1 (skeleton), torch is in Suggests and is not required.
  # Emit a friendly note if torch is not installed so users know what
  # will be needed going forward.
  if (!requireNamespace("torch", quietly = TRUE)) {
    packageStartupMessage(
      "sconjoint: the 'torch' package is not installed. ",
      "It will become a hard dependency when the deep-learning ",
      "backend lands in a future release. Install with ",
      "install.packages('torch') and then torch::install_torch()."
    )
  }
}
