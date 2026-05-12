## Centralized helper for picking the "hybrid" vs raw-DNN beta matrix
## on an `sc_fit`.  All quantity functions delegate to this rather than
## reading `object$beta_hat` directly, so the slot-name knowledge lives
## in one place.

#' Internal: pick the appropriate `beta_hat` matrix for a quantity
#'
#' Quantity functions accept a `which_beta = c("hybrid", "dnn")` argument
#' and call this helper to look up the right matrix.
#'
#' Semantics:
#'
#' * `"hybrid"` (the default): return the Stage-2-refined betas
#'   (`object$beta_hat`).  These are the MAP / BLUP / etc.\ values
#'   produced by whichever Stage 2 ran.  If `stage2 = "none"` was used,
#'   or `object$beta_hat` is missing, the function silently falls back
#'   to the single-DNN matrix (`object$beta_hat_dnn`) because they are
#'   then numerically equal anyway.
#' * `"dnn"`: return the raw Stage-1 cross-fitted DNN matrix
#'   (`object$beta_hat_dnn`).  On `sc_fit` objects produced by v0.1
#'   (which lack `beta_hat_dnn`), fall back to `object$beta_hat` for
#'   forward compatibility.
#'
#' @param object An `sc_fit`.
#' @param which_beta One of `"hybrid"` or `"dnn"`.
#' @return A numeric `n_task` x `p` matrix.
#' @keywords internal
#' @noRd
.sc_pick_beta <- function(object,
                          which_beta = c("hybrid", "dnn")) {
  which_beta <- match.arg(which_beta)
  if (which_beta == "dnn") {
    if (!is.null(object$beta_hat_dnn)) {
      return(object$beta_hat_dnn)
    }
    ## v0.1 fits stored only `beta_hat`; treat it as DNN-only.
    return(object$beta_hat)
  }
  ## "hybrid": prefer the Stage-2 refined matrix when available
  if (!is.null(object$beta_hat) &&
      !identical(object$stage2_method, "none")) {
    return(object$beta_hat)
  }
  ## fall through to the DNN matrix when no Stage 2 ran
  if (!is.null(object$beta_hat_dnn)) {
    return(object$beta_hat_dnn)
  }
  object$beta_hat
}
