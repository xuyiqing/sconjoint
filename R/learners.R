## Alternative first-stage learners for sconjoint.
##
## The DML layer (Lambda(Z), orthogonal score, clustered vcov, and every
## `sc_*` quantity function) consumes a single object: the out-of-sample
## first-stage matrix `beta_hat` (N x p), the held-out estimate of the
## per-respondent preference weights beta(Z_i).  The default learner is the
## cross-fitted DNN (`.sc_crossfit()` in crossfit.R).  This file adds two
## flexible alternatives that produce the *same* `beta_hat` object, so they
## drop into the identical downstream machinery -- only f-hat(Z) changes:
##
##   * elastic net  -- penalized logit on [deltaX , deltaX (x) Z]
##                     (main effects + first-order moderator interactions),
##                     cross-fit by respondent fold; beta_k(Z_i) is the
##                     analytic gradient B0_k + <B_int[k, ], Z_i>.
##   * GRF          -- a generalized random forest local logit: grow a
##                     multi_regression_forest on per-respondent crude
##                     ridge-logit coefficients, then for each held-out
##                     respondent use the forest's adaptive neighbor weights
##                     to solve the *pooled weighted* logit moment by Newton.
##                     This is a genuine forest-localized logit, not an OOB
##                     prediction of the crude coefficients.
##
## Both reuse the harness fold assignment (`fold_id`, respondent-clustered)
## so a learner swap respects `scfit()`'s `K` and `seed`.  Each returns the
## same shape as `.sc_crossfit()`: list(beta_hat, nets, loss_traces, fold_id, K).

## ----------------------------------------------------------------------------
## Elastic-net first stage
## ----------------------------------------------------------------------------

#' Cross-fitted elastic-net first stage
#'
#' Penalized logistic regression of the binary task outcome on the attribute
#' differences and their first-order interactions with the moderators,
#' `[deltaX , deltaX (x) Z]`, fit with `glmnet::cv.glmnet()` at `lambda.min`.
#' Because the index is linear in the interactions, the implied
#' per-respondent coefficient is the analytic gradient with respect to
#' `deltaX_k`,
#' \deqn{\hat\beta_k(Z_i) = B^0_k + \sum_{l} B^{int}_{k,l} Z_{i,l},}
#' which is evaluated out of sample on each held-out fold.
#'
#' @param deltaX Numeric N x p matrix of per-task attribute differences.
#' @param y Numeric 0/1 vector, length N.
#' @param Z Numeric N x p_Z matrix of (task-level) moderators.
#' @param fold_id Integer vector, length N, respondent-clustered fold ids.
#' @param alpha Elastic-net mixing parameter in [0, 1]; 1 = lasso, 0 = ridge.
#' @param nfolds Inner CV folds for `cv.glmnet`.
#' @param seed Integer seed for the inner CV fold draw (determinism).
#' @return list(beta_hat, nets, loss_traces, fold_id, K) matching
#'   `.sc_crossfit()`.  `nets` holds the per-fold `cv.glmnet` objects;
#'   `loss_traces` is a length-K list of NULLs (no training curve).
#' @keywords internal
#' @noRd
.sc_crossfit_enet <- function(deltaX, y, Z, fold_id,
                              alpha = 0.5, nfolds = 5L, seed = NULL) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop(".sc_crossfit_enet(): the 'glmnet' package is required for ",
         "learner = \"enet\".  Install it or use learner = \"dnn\".")
  }
  if (!is.matrix(deltaX) || !is.numeric(deltaX)) {
    stop(".sc_crossfit_enet(): `deltaX` must be a numeric matrix.")
  }
  if (!is.matrix(Z) || !is.numeric(Z)) {
    stop(".sc_crossfit_enet(): `Z` must be a numeric matrix.")
  }
  n <- nrow(deltaX); p <- ncol(deltaX); pZ <- ncol(Z)
  if (length(y) != n || nrow(Z) != n || length(fold_id) != n) {
    stop(".sc_crossfit_enet(): row counts of `deltaX`, `y`, `Z`, `fold_id` disagree.")
  }
  fold_id <- as.integer(fold_id)
  K <- max(fold_id)

  ## Full design: main effects + first-order deltaX x Z interactions.
  ## Block l of `inter` is deltaX[, l] * Z (the p_Z moderator interactions
  ## for attribute l), so the coefficient layout is
  ##   [ B0_1..B0_p , B_int(1,1..pZ) , ... , B_int(p,1..pZ) ].
  inter <- do.call(cbind, lapply(seq_len(p), function(l) deltaX[, l] * Z))
  Xf <- cbind(deltaX, inter)

  beta_hat <- matrix(NA_real_, n, p)
  colnames(beta_hat) <- colnames(deltaX)
  nets <- vector("list", K)

  for (k in seq_len(K)) {
    train_in <- which(fold_id != k)
    holdout  <- which(fold_id == k)
    ## Deterministic inner-CV fold ids derived from `seed` and `k`.
    foldid_in <- NULL
    if (!is.null(seed)) {
      foldid_in <- .sc_inner_cv_foldid(length(train_in), nfolds,
                                       seed = .sc_fold_seed(seed, k))
    }
    cv <- glmnet::cv.glmnet(
      Xf[train_in, , drop = FALSE], y[train_in],
      family = "binomial", alpha = alpha, intercept = FALSE,
      nfolds = nfolds, foldid = foldid_in
    )
    B <- as.numeric(stats::coef(cv, s = "lambda.min"))[-1L]  # drop intercept slot
    B0   <- B[seq_len(p)]
    Bint <- B[p + seq_len(p * pZ)]
    ## beta_k(Z_i) = B0_k + <B_int[k, ], Z_i>, evaluated on held-out rows.
    Bint_mat <- matrix(Bint, nrow = p, ncol = pZ, byrow = TRUE)  # p x pZ
    beta_hat[holdout, ] <- sweep(Z[holdout, , drop = FALSE] %*% t(Bint_mat),
                                 2L, B0, FUN = "+")
    nets[[k]] <- cv
  }

  if (any(is.na(beta_hat))) {
    stop(".sc_crossfit_enet(): produced NA in beta_hat (fold mis-assignment?).")
  }
  list(beta_hat = beta_hat, nets = nets,
       loss_traces = vector("list", K), fold_id = fold_id, K = K)
}

## ----------------------------------------------------------------------------
## GRF first stage (forest-localized logit)
## ----------------------------------------------------------------------------

#' Per-respondent crude ridge-logit coefficients
#'
#' Newton step from a pooled logit fit, ridged for stability, computed
#' separately for each respondent's own tasks.  These are deliberately
#' noisy point estimates -- they are the *response* the forest smooths, not
#' the final first stage.
#'
#' @param deltaX,y per-task arrays (subset to the held-in respondents).
#' @param rid respondent id per row (length nrow(deltaX)).
#' @param ridge Ridge added to each per-respondent Hessian.
#' @return list(beta = M x p crude coefficients, ur = sorted unique rids).
#' @keywords internal
#' @noRd
.sc_grf_crude_beta <- function(deltaX, y, rid, ridge = 0.2) {
  p <- ncol(deltaX)
  ur <- sort(unique(rid))
  M <- length(ur)
  b0 <- tryCatch(
    stats::glm.fit(deltaX, y, family = stats::binomial(),
                   intercept = FALSE)$coefficients,
    error = function(e) rep(0, p)
  )
  b0[!is.finite(b0)] <- 0
  crude <- matrix(0, M, p)
  for (i in seq_len(M)) {
    idx <- which(rid == ur[i])
    dX  <- deltaX[idx, , drop = FALSE]
    g   <- stats::plogis(as.numeric(dX %*% b0))
    w   <- g * (1 - g)
    H   <- crossprod(dX, dX * w) + ridge * diag(p)
    step <- tryCatch(solve(H, crossprod(dX, y[idx] - g)),
                     error = function(e) rep(0, p))
    crude[i, ] <- b0 + as.numeric(step)
  }
  list(beta = crude, ur = ur)
}

#' Forest-weighted local logit for one held-out respondent
#'
#' Solves the pooled weighted logit moment
#' \deqn{\sum_j w_j \sum_{t \in j} \Delta X_{jt}\,(y_{jt} - \Lambda(\Delta
#'   X_{jt}'\beta)) = 0}
#' by weighted Newton, where `w_j` are the forest's adaptive neighbor
#' weights for held-in respondent `j` and the inner sum runs over that
#' respondent's tasks.  Warm-started at `beta0` (the forest's smoothed crude
#' prediction), so a few Newton steps suffice.
#'
#' @param weights Numeric vector of respondent weights (length = #held-in
#'   respondents), already restricted to the nonzero support by the caller.
#' @param dX_list,y_list Lists (length = #held-in respondents) of the task
#'   matrices / outcomes per held-in respondent, in `weights` order.
#' @param beta0 Numeric p-vector warm start.
#' @param ridge Ridge added to the weighted Hessian.
#' @param max_iter,tol Newton controls.
#' @return Numeric p-vector beta-hat for the held-out respondent.
#' @keywords internal
#' @noRd
.sc_grf_local_solve <- function(weights, dX_list, y_list, beta0,
                                ridge = 1e-3, max_iter = 25L, tol = 1e-7) {
  p <- length(beta0)
  beta <- beta0
  for (it in seq_len(max_iter)) {
    grad <- numeric(p)
    H <- matrix(0, p, p)
    for (j in seq_along(weights)) {
      wj <- weights[j]
      dX <- dX_list[[j]]
      g  <- stats::plogis(as.numeric(dX %*% beta))
      grad <- grad + wj * crossprod(dX, y_list[[j]] - g)
      H    <- H + wj * crossprod(dX, dX * (g * (1 - g)))
    }
    H <- H + ridge * diag(p)
    step <- tryCatch(solve(H, grad), error = function(e) rep(0, p))
    beta <- beta + as.numeric(step)
    if (max(abs(step)) < tol) break
  }
  beta
}

#' Cross-fitted GRF (forest-localized logit) first stage
#'
#' For each held-out fold: grow a `grf::multi_regression_forest` on the
#' held-in respondents' crude ridge-logit coefficients (features = Z), then
#' for each held-out respondent (i) read the forest's adaptive neighbor
#' weights over held-in respondents and (ii) solve the pooled weighted logit
#' moment by Newton, warm-started at the forest's smoothed prediction.  The
#' result is a genuine forest-localized logit estimate of beta(Z_i),
#' evaluated out of sample.
#'
#' @param deltaX,y,Z per-task arrays (N rows); Z is constant within respondent.
#' @param fold_id Integer respondent-clustered fold ids, length N.
#' @param respondent_id Respondent id per row, length N.
#' @param num_trees Forest size.
#' @param ridge_crude,ridge_local Ridge penalties for the crude per-respondent
#'   solve and the local weighted Newton, respectively.
#' @param weight_floor Drop held-in respondents whose forest weight is below
#'   this fraction of the max weight (sparsity / speed).
#' @param seed Integer seed forwarded to the forest.
#' @return list(beta_hat, nets, loss_traces, fold_id, K) matching
#'   `.sc_crossfit()`.  `nets` holds the per-fold forests.
#' @keywords internal
#' @noRd
.sc_crossfit_grf <- function(deltaX, y, Z, fold_id, respondent_id,
                             num_trees = 600L,
                             ridge_crude = 0.2, ridge_local = 1e-3,
                             weight_floor = 1e-3, seed = NULL) {
  if (!requireNamespace("grf", quietly = TRUE)) {
    stop(".sc_crossfit_grf(): the 'grf' package is required for ",
         "learner = \"grf\".  Install it or use learner = \"dnn\".")
  }
  if (!is.matrix(deltaX) || !is.numeric(deltaX)) {
    stop(".sc_crossfit_grf(): `deltaX` must be a numeric matrix.")
  }
  if (!is.matrix(Z) || !is.numeric(Z)) {
    stop(".sc_crossfit_grf(): `Z` must be a numeric matrix.")
  }
  n <- nrow(deltaX); p <- ncol(deltaX)
  if (length(y) != n || nrow(Z) != n ||
      length(fold_id) != n || length(respondent_id) != n) {
    stop(".sc_crossfit_grf(): row counts of `deltaX`, `y`, `Z`, `fold_id`, ",
         "`respondent_id` disagree.")
  }
  fold_id <- as.integer(fold_id)
  K <- max(fold_id)
  forest_seed <- if (is.null(seed)) 11L else as.integer(seed)

  beta_hat <- matrix(NA_real_, n, p)
  colnames(beta_hat) <- colnames(deltaX)
  forests <- vector("list", K)

  ## Per-respondent feature row (Z is constant within respondent).
  Zr_of <- function(rows_idx, rid_sub) {
    ur <- sort(unique(rid_sub))
    Z[rows_idx[match(ur, rid_sub)], , drop = FALSE]
  }

  for (k in seq_len(K)) {
    in_rows  <- which(fold_id != k)
    out_rows <- which(fold_id == k)
    rid_in   <- respondent_id[in_rows]

    ## (1) crude per-respondent coefficients on held-in respondents.
    crude <- .sc_grf_crude_beta(deltaX[in_rows, , drop = FALSE], y[in_rows],
                                rid_in, ridge = ridge_crude)
    ur_in <- crude$ur                      # sorted held-in respondent ids
    Zr_in <- Z[in_rows[match(ur_in, rid_in)], , drop = FALSE]  # M_in x pZ

    ## Pre-split held-in tasks by respondent (for the weighted moment).
    dX_by <- lapply(ur_in, function(u) deltaX[in_rows[rid_in == u], , drop = FALSE])
    y_by  <- lapply(ur_in, function(u) y[in_rows[rid_in == u]])

    ## (2) grow the forest on (Z, crude beta) over held-in respondents.
    frst <- grf::multi_regression_forest(
      Zr_in, crude$beta, num.trees = num_trees,
      seed = forest_seed + k, num.threads = 1L
    )
    forests[[k]] <- frst

    ## Held-out respondents and their feature rows.
    rid_out <- respondent_id[out_rows]
    ur_out  <- sort(unique(rid_out))
    Zr_out  <- Z[out_rows[match(ur_out, rid_out)], , drop = FALSE]

    ## (3) forest weights of each held-out respondent over held-in respondents.
    W <- grf::get_forest_weights(frst, newdata = Zr_out, num.threads = 1L)
    ## Warm start = forest's smoothed crude prediction at the held-out Z.
    warm <- predict(frst, newdata = Zr_out)$predictions
    warm <- matrix(as.matrix(warm), nrow = length(ur_out), ncol = p)

    beta_out <- matrix(NA_real_, length(ur_out), p)
    for (i in seq_along(ur_out)) {
      wi <- as.numeric(W[i, ])
      keep <- which(wi > weight_floor * max(wi))
      if (length(keep) == 0L) {            # degenerate: fall back to warm start
        beta_out[i, ] <- warm[i, ]; next
      }
      beta_out[i, ] <- .sc_grf_local_solve(
        weights  = wi[keep],
        dX_list  = dX_by[keep],
        y_list   = y_by[keep],
        beta0    = warm[i, ],
        ridge    = ridge_local
      )
    }
    ## Expand respondent-level estimates back to held-out rows.
    beta_hat[out_rows, ] <- beta_out[match(rid_out, ur_out), , drop = FALSE]
  }

  if (any(is.na(beta_hat))) {
    stop(".sc_crossfit_grf(): produced NA in beta_hat (fold mis-assignment?).")
  }
  list(beta_hat = beta_hat, nets = forests,
       loss_traces = vector("list", K), fold_id = fold_id, K = K)
}

## ----------------------------------------------------------------------------
## shared helper
## ----------------------------------------------------------------------------

#' Deterministic inner-CV fold ids for cv.glmnet
#'
#' @param n Number of held-in observations.
#' @param nfolds Number of CV folds.
#' @param seed Integer seed.
#' @return Integer vector length `n` in `1..nfolds`.
#' @keywords internal
#' @noRd
.sc_inner_cv_foldid <- function(n, nfolds, seed) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  sample(rep_len(seq_len(nfolds), n))
}
