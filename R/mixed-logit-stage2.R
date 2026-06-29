## Stage-2: DNN-offset mixed-logit BLUP via lme4::glmer (paper Section A.4;
## Acharya, Hainmueller & Xu 2026).  Used when
## `scfit(..., stage2 = "mixed_logit")`.

#' Stage 2 via DNN-offset mixed-logit BLUP (`lme4::glmer`)
#'
#' Fits `Y ~ 0 + offset(deltaX %*% beta_hat_ens_resp) + (0 + x_j | resp)`
#' for j = 1..P with `nAGQ = 0` (Laplace approximation), wrapped in
#' tryCatch + suppressMessages + suppressWarnings.  On success, recovers
#' BLUPs as `beta_hat_ens_resp + ranef(fit)$resp`.  On failure, the
#' caller (the stage2 controller) emits a warning and falls back to
#' single-DNN behavior.
#'
#' @param deltaX Numeric `n_task` x `P` matrix.
#' @param y Numeric length-`n_task` vector of 0/1 outcomes.
#' @param beta_hat_ens_resp Numeric `n_resp` x `P` ensemble means.
#' @param respondent_idx Integer length-`n_task` vector in 1..n_resp.
#' @return A list with:
#'   * `beta_hat_resp`: numeric `n_resp` x `P` matrix or `NULL` on
#'     failure;
#'   * `status`: one of `"converged"`, `"converged_with_warnings"`, or
#'     `"failed"`;
#'   * `warnings`: character vector of accumulated warning / error
#'     messages.
#' @keywords internal
#' @noRd
.sc_mixed_logit_stage2 <- function(deltaX, y, beta_hat_ens_resp,
                                   respondent_idx) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("stage2 = \"mixed_logit\" requires the 'lme4' package; ",
         "install.packages(\"lme4\") and retry.")
  }
  P <- ncol(deltaX)
  n_resp <- nrow(beta_hat_ens_resp)
  offset_vec <- rowSums(
    deltaX * beta_hat_ens_resp[respondent_idx, , drop = FALSE]
  )

  df <- data.frame(
    Y      = as.numeric(y),
    resp   = factor(respondent_idx, levels = seq_len(n_resp)),
    offset = offset_vec
  )
  for (j in seq_len(P)) df[[paste0("x", j)]] <- deltaX[, j]

  re_terms <- paste(
    sprintf("(0 + x%d | resp)", seq_len(P)),
    collapse = " + "
  )
  f <- stats::as.formula(
    paste0("Y ~ 0 + offset(offset) + ", re_terms)
  )

  warns <- character()
  fit <- withCallingHandlers(
    tryCatch(
      lme4::glmer(
        f, data = df, family = stats::binomial(),
        control = lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 10000)
        ),
        nAGQ = 0
      ),
      error = function(e) {
        warns <<- c(warns, paste0("glmer error: ", conditionMessage(e)))
        NULL
      }
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      invokeRestart("muffleMessage")
    }
  )

  if (is.null(fit)) {
    return(list(
      beta_hat_resp = NULL,
      status        = "failed",
      warnings      = warns
    ))
  }

  re <- as.matrix(lme4::ranef(fit)$resp)
  colnames(re) <- paste0("x", seq_len(P))
  ## Reorder columns to x1..xP just in case glmer reordered them
  re <- re[, paste0("x", seq_len(P)), drop = FALSE]
  beta_hat_resp <- beta_hat_ens_resp + re
  colnames(beta_hat_resp) <- colnames(beta_hat_ens_resp)

  list(
    beta_hat_resp = beta_hat_resp,
    status = if (length(warns) > 0L) "converged_with_warnings"
             else "converged",
    warnings = warns
  )
}
