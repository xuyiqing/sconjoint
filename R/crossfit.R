## Cross-fitting engine for sconjoint (M3).
##
## Ports `04_training.R` cross-fitting logic: respondent-clustered K-fold
## assignment, per-fold training on the (K-1)/K held-in rows, and
## out-of-sample beta(Z) prediction on the held-out fold.
##
## Parallel execution uses future.apply with L'Ecuyer-CMRG streams
## pre-generated in the parent process, so that the per-fold RNG path
## is a deterministic function of (master_seed, fold_id) only -- never
## of which worker picks up which fold.  This gives the M3 bit-exact
## cross-core guarantee:
##
##   identical(
##     scfit(..., seed = 42, parallel = FALSE),
##     scfit(..., seed = 42, parallel = TRUE, n_cores = 4)
##   )

#' Assign respondents to K folds, clustered at the respondent level
#'
#' Every respondent is wholly contained in exactly one fold, so that
#' all tasks from a given respondent land together.  The assignment is
#' deterministic given `seed`: within a temporary `set.seed(seed)`
#' scope, a balanced `rep(1:K, length.out = M)` is permuted and
#' returned keyed by the sorted unique respondent ids.
#'
#' @param respondent_id Vector of respondent ids (length N, one per
#'   task row).
#' @param K Integer, number of folds (>= 2).
#' @param seed Integer master seed (or NULL for the current RNG).
#' @return An integer vector of length `length(respondent_id)` giving
#'   the fold assignment of every row.  Row-level assignments share
#'   the same fold id whenever they share the same respondent id.
#' @keywords internal
#' @noRd
.sc_make_folds <- function(respondent_id, K, seed = NULL) {
  if (length(respondent_id) == 0L) {
    stop(".sc_make_folds(): `respondent_id` is empty.")
  }
  K <- as.integer(K)
  if (is.na(K) || K < 2L) {
    stop(".sc_make_folds(): `K` must be an integer >= 2.")
  }
  uniq <- sort(unique(respondent_id))
  M <- length(uniq)
  if (M < K) {
    stop(sprintf(
      ".sc_make_folds(): %d respondents is fewer than K = %d folds.",
      M, K
    ))
  }

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  if (!is.null(seed)) {
    set.seed(seed)
  }
  resp_folds <- sample(rep(seq_len(K), length.out = M))
  names(resp_folds) <- as.character(uniq)

  as.integer(resp_folds[as.character(respondent_id)])
}

#' Derive a deterministic integer sub-seed from (master_seed, fold_id)
#'
#' Used to seed `.sc_train_one()` inside each cross-fit fold.  The
#' function is a pure arithmetic transform of its arguments, so the
#' per-fold seed is independent of worker scheduling, wall-clock time,
#' or anything else.  A master seed of `NULL` means "use random state
#' from the current RNG" -- in that case we fall back to a draw from
#' `sample.int()`, but the whole cross-fit becomes non-deterministic
#' and the bit-exact guarantee is waived (as documented on `scfit()`).
#'
#' @param master_seed Integer master seed or NULL.
#' @param k Integer fold id in `seq_len(K)`.
#' @return An integer seed suitable for `set.seed()` /
#'   `torch::torch_manual_seed()`.
#' @keywords internal
#' @noRd
.sc_fold_seed <- function(master_seed, k) {
  k <- as.integer(k)
  if (is.null(master_seed)) {
    return(sample.int(.Machine$integer.max, 1L))
  }
  ## Affine in (seed, k); bounded away from integer overflow and
  ## distinct for k = 1..K across typical seed ranges.
  base <- as.integer(master_seed) %% 100000L
  as.integer(base * 1000L + k)
}

#' Run K-fold cross-fitting for the sconjoint DNN
#'
#' For each fold `k` in `1..K`:
#'
#'   1. Train a fresh DNN on the held-in rows using `.sc_train_one()`
#'      seeded with `.sc_fold_seed(seed, k)`.
#'   2. Predict `beta(Z)` on the held-out rows via `.sc_predict_beta()`.
#'
#' The held-out beta matrix is assembled by fold id, not completion
#' order, so parallel and sequential runs produce `identical()` output.
#'
#' @param deltaX Numeric N x p matrix of per-task attribute differences.
#' @param y Numeric vector of length N with 0/1 outcomes.
#' @param Z Numeric N x p_Z matrix of respondent moderators.
#' @param fold_id Integer vector of length N mapping rows to folds.
#' @param hidden Integer vector of hidden-layer widths.
#' @param n_epochs,learning_rate,lambda Training hyperparameters
#'   forwarded to `.sc_train_one()`.
#' @param seed Integer master seed (or NULL).
#' @param parallel Logical, run folds in parallel.
#' @param n_cores Integer number of workers to request when parallel.
#' @param device Character `"cpu"` or `"cuda"`.
#' @param verbose Logical, per-epoch training noise.
#' @return A list with:
#'   * `beta_hat` -- N x p matrix of out-of-sample beta predictions;
#'   * `nets` -- list of length K, the trained networks per fold;
#'   * `loss_traces` -- list of length K, per-fold training loss curves;
#'   * `fold_id` -- integer vector of length N (copy of input);
#'   * `K` -- integer number of folds.
#' @keywords internal
#' @noRd
.sc_crossfit <- function(deltaX, y, Z, fold_id,
                         hidden = NULL,
                         n_epochs = 2000L,
                         learning_rate = 0.01,
                         lambda = 1e-4,
                         seed = NULL,
                         parallel = FALSE,
                         n_cores = NULL,
                         device = "cpu",
                         verbose = FALSE) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_crossfit(): the 'torch' package is required.")
  }
  if (!is.matrix(deltaX) || !is.numeric(deltaX)) {
    stop(".sc_crossfit(): `deltaX` must be a numeric matrix.")
  }
  if (!is.matrix(Z) || !is.numeric(Z)) {
    stop(".sc_crossfit(): `Z` must be a numeric matrix.")
  }
  n <- nrow(deltaX)
  if (length(y) != n || nrow(Z) != n || length(fold_id) != n) {
    stop(".sc_crossfit(): row counts of `deltaX`, `y`, `Z`, `fold_id` disagree.")
  }
  fold_id <- as.integer(fold_id)
  K <- max(fold_id)
  if (any(is.na(fold_id)) || min(fold_id) < 1L || K < 2L) {
    stop(".sc_crossfit(): `fold_id` must be integer-valued in 1..K with K >= 2.")
  }
  if (is.null(hidden)) {
    hidden <- .sc_auto_hidden(n)
  }

  ## Pre-generate K L'Ecuyer-CMRG streams in the parent process.  These
  ## seed future.apply's RNG on a per-fold basis; see future.apply docs
  ## for `future.seed = list(...)` semantics.  The streams are derived
  ## from `seed` if provided, otherwise from the current RNG state.
  stream_seed <- if (is.null(seed)) {
    ## Draw a master seed from the current RNG; this is only reached
    ## when the caller explicitly passed `seed = NULL`, so losing
    ## bit-exactness is acceptable.
    sample.int(.Machine$integer.max, 1L)
  } else {
    as.integer(seed)
  }
  streams <- .sc_make_seed_streams(stream_seed, K)

  ## Deterministic per-fold integer seed, used INSIDE `.sc_train_one()`
  ## to seed both base R and torch.  This is the part that guarantees
  ## bit-exact behavior across worker counts: fold k's training depends
  ## only on (seed, k), not on which worker processes it.
  fold_seeds <- vapply(seq_len(K),
                       function(k) .sc_fold_seed(seed, k),
                       integer(1L))

  ## Worker callable: train on held-in, predict on held-out, return
  ## (beta_holdout, holdout_idx, net, loss_trace).
  fit_one <- function(k) {
    holdout  <- which(fold_id == k)
    train_in <- which(fold_id != k)
    trained <- .sc_train_one(
      deltaX        = deltaX[train_in, , drop = FALSE],
      y             = y[train_in],
      Z             = Z[train_in, , drop = FALSE],
      hidden        = hidden,
      n_epochs      = n_epochs,
      learning_rate = learning_rate,
      lambda        = lambda,
      seed          = fold_seeds[k],
      device        = device,
      verbose       = verbose
    )
    beta_holdout <- .sc_predict_beta(trained$net, Z[holdout, , drop = FALSE])
    list(
      k            = k,
      holdout      = holdout,
      beta_holdout = beta_holdout,
      net          = trained$net,
      loss_trace   = trained$loss_trace
    )
  }

  use_parallel <- isTRUE(parallel) &&
    requireNamespace("future",       quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)

  if (use_parallel) {
    if (is.null(n_cores) || n_cores < 2L) {
      n_cores <- 2L
    }
    ## Cap at the number of physically available cores (parallelly is
    ## more conservative than parallel::detectCores() on shared hosts).
    max_cores <- tryCatch(parallelly::availableCores(omit = 1L),
                          error = function(e) 2L)
    n_cores <- min(as.integer(n_cores), K, as.integer(max_cores))
    if (n_cores < 2L) n_cores <- 2L
    oplan <- future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(oplan), add = TRUE)

    results <- future.apply::future_lapply(
      seq_len(K),
      fit_one,
      future.seed = streams
    )
  } else {
    results <- lapply(seq_len(K), fit_one)
  }

  ## Assemble by fold id (not by completion order) so that parallel
  ## and sequential runs produce identical matrices.
  p <- ncol(deltaX)
  beta_hat <- matrix(NA_real_, nrow = n, ncol = p)
  colnames(beta_hat) <- colnames(deltaX)
  nets        <- vector("list", K)
  loss_traces <- vector("list", K)
  for (res in results) {
    k <- res$k
    beta_hat[res$holdout, ] <- res$beta_holdout
    nets[[k]]        <- res$net
    loss_traces[[k]] <- res$loss_trace
  }

  if (any(is.na(beta_hat))) {
    stop(".sc_crossfit(): cross-fit produced NA in beta_hat (fold mis-assignment?).")
  }

  list(
    beta_hat    = beta_hat,
    nets        = nets,
    loss_traces = loss_traces,
    fold_id     = fold_id,
    K           = K
  )
}
