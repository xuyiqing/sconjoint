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
