## Stage-2 empirical-Bayes MAP update (paper "EnsC5").
##
## Algorithm reference: see
## `statsclaw-workspace/sconjoint/ref/map-stage2-algorithm.md` in the
## workbench, and `code/04b_map_update.R` in the ConjointStructural
## prototype.
##
## All functions are internal.  The user-facing entry point is the
## `stage2 = "map_c5"` (or `"varref"`) argument on `scfit()`, which
## routes through `.sc_run_stage2()` in `R/stage2-controller.R`.

## ----------------------------------------------------------------------
## Newton MAP solver for a single respondent
## ----------------------------------------------------------------------

#' Solve the MAP problem for one respondent
#'
#' Maximizes
#' \deqn{\ell_i(\beta) - \tfrac{1}{2}(\beta - f_i)' \Sigma_{\eta}^{-1} (\beta - f_i)}{
#'   ell_i(beta) - 1/2 (beta - f_i)' Sigma_eta^{-1} (beta - f_i)}
#' by Newton iterations in the residual parameterization
#' \eqn{\eta = \beta - f_i}.
#'
#' @param DX_i Numeric `T_i` x `P` matrix of `DeltaX` rows for this respondent.
#' @param y_i Numeric vector of length `T_i` with 0/1 outcomes.
#' @param f_i Numeric prior mean (length `P`), typically `beta_hat_ens_resp[i, ]`.
#' @param sigma_prior Numeric prior variance (length `P` or scalar; reused
#'   across coordinates if scalar).  This is the *variance*, not the precision.
#' @param max_iter Integer maximum Newton iterations (default 20).
#' @param tol Numeric convergence tolerance on `max |gradient|` (default 1e-6).
#' @param offset Optional numeric length-`T_i` vector of KNOWN per-task
#'   index offsets `o_it = g(X_A,it) - g(X_B,it)` from the cross-fitted
#'   population-level interaction term.  The respondent update on
#'   `beta_i` is otherwise unchanged: the index is
#'   `DX_i %*% (f_i + eta) + offset`.  `NULL` (default) means no offset.
#' @return A list with:
#'   * `eta`: numeric length-`P` residual, so that `beta_hat = f_i + eta`;
#'   * `post_var_diag`: numeric length-`P` diagonal of the posterior
#'     covariance \eqn{-H^{-1}} evaluated at the optimum (`NA` if the
#'     Hessian could not be inverted);
#'   * `iters`: integer iterations taken;
#'   * `converged`: logical, did `max |gradient|` fall below `tol`.
#' @keywords internal
#' @noRd
.sc_map_one <- function(DX_i, y_i, f_i, sigma_prior,
                        max_iter = 20L, tol = 1e-6, offset = NULL) {
  P <- length(f_i)
  if (length(sigma_prior) == 1L) {
    sigma_prior <- rep(sigma_prior, P)
  }
  prec <- 1 / sigma_prior  # diagonal prior precision
  off <- if (is.null(offset)) 0 else as.numeric(offset)

  eta <- rep(0, P)
  converged <- FALSE
  iters <- 0L

  for (it in seq_len(as.integer(max_iter))) {
    iters <- it
    v <- as.numeric(DX_i %*% (f_i + eta)) + off
    p <- 1 / (1 + exp(-v))
    r <- y_i - p
    g <- as.numeric(crossprod(DX_i, r)) - prec * eta
    if (max(abs(g)) < tol) {
      converged <- TRUE
      break
    }
    w <- p * (1 - p)
    H <- -crossprod(DX_i * w, DX_i) - diag(prec, nrow = P)
    step <- tryCatch(
      solve(H, g),
      error = function(e) g / diag(H)
    )
    eta <- eta - step
  }

  ## Final Hessian + posterior var at the optimum
  v <- as.numeric(DX_i %*% (f_i + eta)) + off
  p <- 1 / (1 + exp(-v))
  w <- p * (1 - p)
  H <- -crossprod(DX_i * w, DX_i) - diag(prec, nrow = P)
  post_var_diag <- tryCatch(
    diag(solve(-H)),
    error = function(e) rep(NA_real_, P)
  )

  list(
    eta = eta,
    post_var_diag = post_var_diag,
    iters = iters,
    converged = converged
  )
}

#' Solve the MAP problem for every respondent
#'
#' Loops `.sc_map_one()` over respondents.  Returns the respondent-level
#' MAP matrix and the per-respondent posterior variance diagonal.
#'
#' @param deltaX Numeric `n_task` x `P` matrix of task-level `DeltaX`.
#' @param y Numeric vector of length `n_task` with 0/1 outcomes.
#' @param beta_hat_resp Numeric `n_resp` x `P` matrix of prior means
#'   (typically `beta_hat_ens` collapsed to respondent level).  Note: this
#'   MUST be respondent-indexed (one row per unique respondent), NOT
#'   task-indexed.  See the prototype's 2026-04-26 bug guard.
#' @param respondent_idx Integer vector of length `n_task`; the
#'   respondent number (1..n_resp) of each task row.
#' @param sigma_prior Numeric length-`P` (or scalar) diagonal prior variance.
#' @param offset Optional numeric length-`n_task` vector of known per-task
#'   interaction offsets, forwarded row-subset to `.sc_map_one()`.
#' @return A list with:
#'   * `beta_hat_resp_map`: numeric `n_resp` x `P` matrix of MAP estimates;
#'   * `post_var_resp_diag`: numeric `n_resp` x `P` matrix of posterior
#'     variance diagonals (one row per respondent);
#'   * `n_converged`: integer number of respondents whose Newton step met
#'     the tolerance within `max_iter`.
#' @keywords internal
#' @noRd
.sc_map_all <- function(deltaX, y, beta_hat_resp,
                        respondent_idx, sigma_prior,
                        max_iter = 20L, tol = 1e-6, offset = NULL) {
  n_resp <- nrow(beta_hat_resp)
  P <- ncol(beta_hat_resp)
  if (length(sigma_prior) == 1L) {
    sigma_prior <- rep(sigma_prior, P)
  }
  beta_map <- matrix(NA_real_, n_resp, P)
  pv       <- matrix(NA_real_, n_resp, P)
  n_conv <- 0L

  rows_by_resp <- split(seq_along(respondent_idx), respondent_idx)
  for (i in seq_len(n_resp)) {
    rows <- rows_by_resp[[as.character(i)]]
    if (is.null(rows) || length(rows) == 0L) {
      ## Respondent with no tasks (shouldn't happen in practice; defensive)
      beta_map[i, ] <- beta_hat_resp[i, ]
      pv[i, ]       <- sigma_prior
      next
    }
    DX_i <- deltaX[rows, , drop = FALSE]
    y_i  <- y[rows]
    res <- .sc_map_one(
      DX_i        = DX_i,
      y_i         = y_i,
      f_i         = beta_hat_resp[i, ],
      sigma_prior = sigma_prior,
      max_iter    = max_iter,
      tol         = tol,
      offset      = if (is.null(offset)) NULL else offset[rows]
    )
    beta_map[i, ] <- beta_hat_resp[i, ] + res$eta
    pv[i, ]       <- res$post_var_diag
    if (isTRUE(res$converged)) n_conv <- n_conv + 1L
  }

  colnames(beta_map) <- colnames(beta_hat_resp)
  colnames(pv)       <- colnames(beta_hat_resp)

  list(
    beta_hat_resp_map  = beta_map,
    post_var_resp_diag = pv,
    n_converged        = n_conv
  )
}

## ----------------------------------------------------------------------
## Score-based diagonal prior variance estimator (paper "sigma_score")
## ----------------------------------------------------------------------

#' Estimate the diagonal score-based prior variance
#'
#' For each coefficient \eqn{k}:
#' \deqn{\hat\sigma^2_{score,k} = \max\!\left(
#'   \frac{T\,\mathrm{Var}_i(\bar s_{ik})}{\bar w^2} -
#'   \frac{1}{\bar w\,T},\; 0.01 \right),}{
#'   sigma_score_k^2 = max( T * Var_i(s-bar_ik) / w-bar^2 - 1/(w-bar T), 0.01 ),}
#' where
#' \eqn{\bar s_{ik} = T_i^{-1}\sum_t \Delta X_{itk}\,r_{it}},
#' \eqn{r_{it} = Y_{it} - G(\Delta X'_{it}\,\hat\beta_{ens,resp}(Z_i))}, and
#' \eqn{\bar w = \mathrm{mean}_{i,t}\,\hat p_{it}(1-\hat p_{it})}.
#'
#' The `0.01` floor prevents degenerate priors on near-separated or
#' weak-signal cells.
#'
#' @param deltaX Numeric `n_task` x `P` matrix.
#' @param y Numeric length-`n_task` vector of 0/1 outcomes.
#' @param beta_hat_resp Numeric `n_resp` x `P` matrix of respondent-level
#'   ensemble means.
#' @param respondent_idx Integer length-`n_task` vector of respondent index.
#' @param floor Numeric lower bound for the per-coefficient variance.
#'   Default 0.01.
#' @param offset Optional numeric length-`n_task` vector of known per-task
#'   interaction offsets `o_it = g(X_A,it) - g(X_B,it)`, added to the
#'   fitted index before forming the residual.
#' @return Numeric length-`P` vector of floored score-based variances.
#' @keywords internal
#' @noRd
.sc_estimate_sigma_score <- function(deltaX, y, beta_hat_resp,
                                     respondent_idx, floor = 0.01,
                                     offset = NULL) {
  P <- ncol(deltaX)
  n_resp <- nrow(beta_hat_resp)
  n_task <- nrow(deltaX)
  T_avg <- max(1, round(n_task / n_resp))

  ## Task-level fitted prob using the respondent-indexed prior mean
  bht <- beta_hat_resp[respondent_idx, , drop = FALSE]
  v   <- rowSums(deltaX * bht)
  if (!is.null(offset)) v <- v + as.numeric(offset)
  p   <- 1 / (1 + exp(-v))
  r   <- y - p
  w_bar <- mean(p * (1 - p))

  sigma_score <- numeric(P)
  for (j in seq_len(P)) {
    s_bar_i <- tapply(deltaX[, j] * r, respondent_idx, mean)
    v_j     <- stats::var(as.numeric(s_bar_i), na.rm = TRUE)
    raw     <- v_j * T_avg / (w_bar^2) - 1 / (w_bar * T_avg)
    sigma_score[j] <- max(raw, floor)
  }
  names(sigma_score) <- colnames(deltaX)
  sigma_score
}

#' Estimate the "varref" alternative prior variance
#'
#' Variant prior: \eqn{0.5\,\mathrm{Var}_i(\hat\beta_{ens,resp,k})},
#' floored at `floor`.  The recommended path for continuous-attribute
#' designs (e.g. Ballard-Rosa tax rates); see `?scfit` argument
#' `varref_floor` and paper memo 42.
#'
#' @param beta_hat_resp Numeric `n_resp` x `P` matrix.
#' @param floor Numeric lower bound. Default `1e-3` (matches the
#'   production `MAP_VARREF_FLOOR` in `code/60_setup_ballard_rosa.R`).
#'   The prior `0.01` default clipped every coefficient on continuous
#'   designs and over-shrank the per-respondent betas; lowering to
#'   `1e-3` restores the BR validation `r` to the paper value (0.39).
#' @return Numeric length-`P` vector.
#' @keywords internal
#' @noRd
.sc_estimate_sigma_varref <- function(beta_hat_resp, floor = 1e-3) {
  v <- apply(beta_hat_resp, 2L, stats::var)
  out <- pmax(0.5 * v, floor)
  names(out) <- colnames(beta_hat_resp)
  out
}

## ----------------------------------------------------------------------
## Task-to-respondent collapse and respondent-to-task expansion
## ----------------------------------------------------------------------

#' Collapse a task-level beta matrix to respondent level
#'
#' For each respondent, average the rows of `beta_task` over that
#' respondent's tasks.  Any all-NA column gets the column-mean imputed
#' (defensive; should not arise in practice).
#'
#' @param beta_task Numeric `n_task` x `P` matrix.
#' @param respondent_idx Integer length-`n_task` vector mapping tasks to
#'   the respondent (1..n_resp).
#' @param n_resp Integer total number of unique respondents.
#' @return A numeric `n_resp` x `P` matrix.
#' @keywords internal
#' @noRd
.sc_collapse_to_resp <- function(beta_task, respondent_idx, n_resp) {
  P <- ncol(beta_task)
  out <- matrix(NA_real_, n_resp, P)
  rows_by_resp <- split(seq_along(respondent_idx), respondent_idx)
  for (i in seq_len(n_resp)) {
    rows <- rows_by_resp[[as.character(i)]]
    if (!is.null(rows) && length(rows) > 0L) {
      out[i, ] <- colMeans(beta_task[rows, , drop = FALSE])
    }
  }
  for (j in seq_len(P)) {
    na_j <- is.na(out[, j])
    if (any(na_j) && any(!na_j)) {
      out[na_j, j] <- mean(out[!na_j, j])
    }
  }
  colnames(out) <- colnames(beta_task)
  out
}

#' Expand a respondent-level beta matrix back to task level
#'
#' Inverse of `.sc_collapse_to_resp()`.
#'
#' @param beta_resp Numeric `n_resp` x `P` matrix.
#' @param respondent_idx Integer length-`n_task` vector of respondent
#'   indices (1..n_resp).
#' @return A numeric `n_task` x `P` matrix.
#' @keywords internal
#' @noRd
.sc_expand_to_task <- function(beta_resp, respondent_idx) {
  beta_resp[respondent_idx, , drop = FALSE]
}
