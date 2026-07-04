## Stage-2 dispatch controller.
##
## After `scfit()` has run Stage 1 (DNN cross-fit), Lambda(Z), and DML
## inference, this controller is invoked to (optionally) run Stage 2 and
## return the new `beta_hat` (Stage-2 refined) together with all
## auxiliary slots.  The DML output (`theta`, `vcov`, `influence_raw`)
## is computed BEFORE this controller runs and is never touched by it,
## so the orthogonal-score property is preserved bit-exactly.

#' Run the requested Stage-2 estimator
#'
#' @param stage2 One of `"none"`, `"map_c5"`, `"varref"`, `"mixed_logit"`.
#' @param beta_hat_dnn Numeric `n_task` x `P` matrix from Stage 1.
#' @param deltaX,y,Z,respondent_id Existing model arrays.
#' @param fold_id_stage1 Integer length-`n_task` from Stage 1 (used only
#'   to size things; the 2nd DNN gets its own fold partition).
#' @param hidden,n_epochs,learning_rate,weight_decay Training
#'   hyperparameters reused for the 2nd DNN.  `weight_decay` must be a
#'   numeric scalar here (the `"adaptive"` sentinel is resolved
#'   upstream in `scfit()`).
#' @param K Number of folds.
#' @param stage2_seed Integer seed for the 2nd DNN cross-fit.
#' @param varref_floor Numeric lower bound on the diagonal prior
#'   variance when `stage2 = "varref"`.  Default `1e-3`.  Ignored for
#'   other Stage-2 methods.
#' @param parallel,n_cores,device,verbose Same as in `scfit()`.
#' @param interactions,X_A,X_B,F_int,int_pairs,interaction_rank,lambda_V
#'   Attribute-interaction configuration (see `.sc_crossfit()`).  When
#'   `interactions != "none"` the 2nd DNN is trained with the same
#'   interaction head, and the per-task interaction offset used by the
#'   MAP update / sigma-score estimator is the ensemble average
#'   `(g_offset_stage1 + g_offset_stage2) / 2`, mirroring the beta
#'   ensemble.  `stage2 = "mixed_logit"` is rejected upstream when an
#'   interaction term is requested.
#' @param g_offset_stage1 Numeric N-vector of Stage-1 cross-fitted
#'   interaction offsets (or NULL).
#' @return A named list of slots to merge into the `sc_fit` object:
#'   `beta_hat`, `beta_hat_dnn`, `beta_hat_dnn2`, `beta_hat_ens`,
#'   `beta_hat_resp`, `sigma_prior`, `sigma_post_diag`,
#'   `stage2_method`, `stage2_warnings`, plus `g_offset_ens` and
#'   `w_fold2` when an interaction term is present.
#' @keywords internal
#' @noRd
.sc_run_stage2 <- function(stage2,
                           beta_hat_dnn,
                           deltaX, y, Z, respondent_id,
                           hidden, n_epochs, learning_rate, weight_decay,
                           K, stage2_seed,
                           varref_floor = 1e-3,
                           parallel = FALSE,
                           n_cores = NULL,
                           device = "cpu",
                           verbose = FALSE,
                           interactions = "none",
                           X_A = NULL,
                           X_B = NULL,
                           F_int = NULL,
                           int_pairs = NULL,
                           interaction_rank = 2L,
                           lambda_V = 1e-2,
                           g_offset_stage1 = NULL) {
  stage2 <- match.arg(stage2,
                      choices = c("map_c5", "none", "varref", "mixed_logit"))

  if (identical(stage2, "none")) {
    return(list(
      beta_hat        = beta_hat_dnn,
      beta_hat_dnn    = beta_hat_dnn,
      beta_hat_dnn2   = NULL,
      beta_hat_ens    = NULL,
      beta_hat_resp   = NULL,
      sigma_prior     = NULL,
      sigma_post_diag = NULL,
      stage2_method   = "none",
      stage2_warnings = character()
    ))
  }

  ## --- Train the 2nd DNN with a fresh fold partition --------------------
  uniq_resp <- sort(unique(respondent_id))
  n_resp <- length(uniq_resp)
  respondent_idx <- match(respondent_id, uniq_resp)  # 1..n_resp

  fold_id_stage2 <- .sc_make_folds(respondent_id, K = K, seed = stage2_seed)
  cf2 <- .sc_crossfit(
    deltaX        = deltaX,
    y             = y,
    Z             = Z,
    fold_id       = fold_id_stage2,
    hidden        = hidden,
    n_epochs      = n_epochs,
    learning_rate = learning_rate,
    weight_decay  = weight_decay,
    seed          = stage2_seed,
    parallel      = parallel,
    n_cores       = n_cores,
    device        = device,
    verbose       = verbose,
    interactions  = interactions,
    X_A           = X_A,
    X_B           = X_B,
    F_int         = F_int,
    int_pairs     = int_pairs,
    interaction_rank = interaction_rank,
    lambda_V         = lambda_V
  )
  beta_hat_dnn2 <- cf2$beta_hat
  colnames(beta_hat_dnn2) <- colnames(beta_hat_dnn)

  ## --- Ensemble and respondent collapse --------------------------------
  beta_hat_ens <- (beta_hat_dnn + beta_hat_dnn2) / 2
  colnames(beta_hat_ens) <- colnames(beta_hat_dnn)
  beta_hat_ens_resp <- .sc_collapse_to_resp(beta_hat_ens,
                                            respondent_idx,
                                            n_resp = n_resp)

  ## Ensemble interaction offset: average of the two cross-fitted
  ## population-level g terms, mirroring the beta ensemble.  NULL when
  ## no interaction term is present (preserves the historical paths).
  g_offset_ens <- NULL
  if (!identical(interactions, "none")) {
    g_offset_ens <- (as.numeric(g_offset_stage1) + as.numeric(cf2$g_offset)) / 2
  }

  ## --- Stage-2 algorithm dispatch --------------------------------------
  if (identical(stage2, "map_c5")) {
    sigma_score <- .sc_estimate_sigma_score(
      deltaX, y, beta_hat_ens_resp, respondent_idx,
      offset = g_offset_ens
    )
    sigma_prior <- sigma_score / 5
    out <- .sc_map_all(
      deltaX, y, beta_hat_ens_resp, respondent_idx, sigma_prior,
      offset = g_offset_ens
    )
    beta_hat_resp <- out$beta_hat_resp_map
    sigma_post_diag <- colMeans(out$post_var_resp_diag, na.rm = TRUE)
    stage2_method <- "map_c5"
    warns <- character()
  } else if (identical(stage2, "varref")) {
    sigma_prior <- .sc_estimate_sigma_varref(beta_hat_ens_resp,
                                             floor = varref_floor)
    out <- .sc_map_all(
      deltaX, y, beta_hat_ens_resp, respondent_idx, sigma_prior,
      offset = g_offset_ens
    )
    beta_hat_resp <- out$beta_hat_resp_map
    sigma_post_diag <- colMeans(out$post_var_resp_diag, na.rm = TRUE)
    stage2_method <- "varref"
    warns <- character()
  } else if (identical(stage2, "mixed_logit")) {
    ## See R/mixed-logit-stage2.R
    ml <- .sc_mixed_logit_stage2(
      deltaX = deltaX,
      y = y,
      beta_hat_ens_resp = beta_hat_ens_resp,
      respondent_idx = respondent_idx
    )
    if (identical(ml$status, "failed")) {
      warning(
        "stage2 = \"mixed_logit\" failed to converge; falling back to ",
        "single-DNN beta_hat. Recommended action: use stage2 = \"map_c5\". ",
        "Underlying issue(s): ",
        paste(ml$warnings, collapse = "; ")
      )
      return(list(
        beta_hat        = beta_hat_dnn,
        beta_hat_dnn    = beta_hat_dnn,
        beta_hat_dnn2   = beta_hat_dnn2,
        beta_hat_ens    = beta_hat_ens,
        beta_hat_resp   = NULL,
        sigma_prior     = NULL,
        sigma_post_diag = NULL,
        stage2_method   = "mixed_logit_failed",
        stage2_warnings = ml$warnings
      ))
    }
    beta_hat_resp <- ml$beta_hat_resp
    sigma_prior <- NULL
    sigma_post_diag <- NULL
    stage2_method <- "mixed_logit"
    warns <- ml$warnings
  }

  ## --- Expand back to task level ---------------------------------------
  beta_hat_task <- .sc_expand_to_task(beta_hat_resp, respondent_idx)
  colnames(beta_hat_task) <- colnames(beta_hat_dnn)

  list(
    beta_hat        = beta_hat_task,
    beta_hat_dnn    = beta_hat_dnn,
    beta_hat_dnn2   = beta_hat_dnn2,
    beta_hat_ens    = beta_hat_ens,
    beta_hat_resp   = beta_hat_resp,
    sigma_prior     = sigma_prior,
    sigma_post_diag = sigma_post_diag,
    stage2_method   = stage2_method,
    stage2_warnings = warns,
    g_offset_ens    = g_offset_ens,
    w_fold2         = cf2$w_fold
  )
}
