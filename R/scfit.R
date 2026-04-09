## `scfit()` -- structural conjoint DML estimator (M3 export).
##
## Wires together the M2 data-prep / DNN / training layers and the M3
## cross-fitting / Lambda / DML inference layers into a single
## formula-driven entry point.  `scfit()` is the first user-facing
## export of the package.  Full `sc_fit` methods (summary, predict,
## plot) land in M4; M3 ships only the minimum (print, coef, vcov).

#' Structural deep-learning estimator for forced-choice conjoint
#'
#' Fits the Acharya-Hainmueller-Xu structural conjoint estimator via a
#' cross-fitted deep neural network with double/debiased machine
#' learning (DML) inference.  The DNN flexibly maps respondent
#' moderators `Z` into per-respondent preference weights `beta(Z)`,
#' and the DML correction debiases `E[beta(Z)]` for the non-parametric
#' first stage.  Standard errors are clustered at the respondent level.
#'
#' @param formula A two-sided formula
#'   `choice ~ attr1 + attr2 + ... | z1 + z2 + ...` (fixest-style).
#'   The left-hand side of `~` is the binary choice outcome; the
#'   left-hand side of `|` is the conjoint attributes (factors are
#'   auto dummy-encoded, numerics pass through); the right-hand side
#'   of `|` is the respondent-level moderators.
#' @param data A data frame in long format: one row per
#'   (respondent, task, profile).  Every `(respondent, task)` pair
#'   must contain exactly two profiles.
#' @param respondent Column name (character, length 1) of the
#'   respondent id in `data`.
#' @param task Column name of the task id in `data`.
#' @param profile Column name of the within-task profile id in `data`.
#' @param hidden Either the character string `"auto"` (default, picks
#'   a three-tier default from `N*T`, see `.sc_auto_hidden()`), or an
#'   integer vector of hidden-layer widths.
#' @param K Integer, number of respondent-clustered folds for
#'   cross-fitting.  Defaults to 10.
#' @param n_epochs Integer, number of full-batch Adam epochs per fold.
#' @param learning_rate Numeric, Adam learning rate.
#' @param lambda Numeric L2 penalty on DNN parameters during training.
#' @param ridge_lambda Numeric ridge penalty used both in the
#'   Lambda(Z) regression and in the Lambda inversion.
#' @param seed Integer master seed.  When supplied, the cross-fit
#'   output is bit-identical on 1 core and on N cores.  The function
#'   saves and restores the R and torch RNG states on exit.
#' @param parallel Logical.  If `TRUE` and `n_cores > 1`, folds are
#'   trained in parallel using `future.apply::future_lapply()` with
#'   L'Ecuyer-CMRG streams.
#' @param n_cores Integer number of parallel workers (only used when
#'   `parallel = TRUE`).  Defaults to 2.
#' @param device Character `"cpu"` (default) or `"cuda"`.  The
#'   bit-exact determinism guarantee applies only on CPU.
#' @param verbose Logical, print per-epoch training progress.
#' @return An object of class `sc_fit` -- see Details.  Key components
#'   include the DML point estimates `theta`, the full `p x p`
#'   clustered variance-covariance `vcov`, the out-of-sample
#'   `beta_hat` matrix, the Lambda(Z) object, and the fold assignment.
#' @details
#' The returned `sc_fit` list contains at least the following fields:
#' * `theta` -- named p-vector of DML point estimates;
#' * `vcov` -- full `p x p` clustered variance-covariance;
#' * `vcov_iid` -- full `p x p` iid variance-covariance (diagnostic);
#' * `se_ratio_dml_iid` -- list with `per_param` and `mean`;
#' * `beta_hat` -- N x p matrix of held-out beta(Z);
#' * `Z` -- N x p_Z matrix of moderators;
#' * `fold_id` -- integer vector of fold assignments;
#' * `lambda_obj` -- Lambda(Z) estimation output;
#' * `call`, `formula`, `attr_names`, `z_names`, `respondent_id`;
#' * `K`, `hidden`, `seed`, `n_epochs`, `learning_rate`, `device`,
#'   `parallel`, `n_cores`;
#' * `loss_traces` -- list of per-fold training loss curves.
#'
#' `summary()`, `predict()`, and `plot()` methods land in M4.
#' @examples
#' \donttest{
#' if (requireNamespace("torch", quietly = TRUE) &&
#'     torch::torch_is_installed()) {
#'   ## Tiny synthetic conjoint: 60 respondents, 3 tasks, 3 binary attrs,
#'   ## 2 continuous Z covariates.
#'   set.seed(1)
#'   M <- 60; T_i <- 3; p <- 3; p_Z <- 2
#'   Z_mat <- matrix(stats::rnorm(M * p_Z), M, p_Z)
#'   beta_true <- cbind(0.5 + 0.3 * Z_mat[, 1],
#'                      -0.4 + 0.5 * Z_mat[, 2],
#'                      0.2)
#'   rid <- rep(seq_len(M), each = T_i)
#'   dX  <- matrix(sample(c(-1, 0, 1), M * T_i * p, replace = TRUE),
#'                 M * T_i, p)
#'   logit <- rowSums(dX * beta_true[rid, ])
#'   y <- stats::rbinom(M * T_i, 1, stats::plogis(logit))
#'
#'   ## Inflate to long format (2 profiles per task, one all-zero).
#'   long <- data.frame(
#'     rid  = rep(rid, each = 2),
#'     tid  = rep(rep(seq_len(T_i), M), each = 2),
#'     pos  = rep(c(1L, 2L), M * T_i),
#'     a1   = as.vector(rbind(dX[, 1], 0)),
#'     a2   = as.vector(rbind(dX[, 2], 0)),
#'     a3   = as.vector(rbind(dX[, 3], 0)),
#'     z1   = rep(Z_mat[rid, 1], each = 2),
#'     z2   = rep(Z_mat[rid, 2], each = 2),
#'     y    = as.vector(rbind(y, 1 - y))
#'   )
#'
#'   fit <- scfit(y ~ a1 + a2 + a3 | z1 + z2,
#'                data = long,
#'                respondent = "rid", task = "tid", profile = "pos",
#'                K = 2, n_epochs = 50, seed = 1)
#'   print(fit)
#'   coef(fit)
#' }
#' }
#' @export
scfit <- function(formula, data,
                  respondent, task, profile,
                  hidden = "auto",
                  K = 10L,
                  n_epochs = 2000L,
                  learning_rate = 0.01,
                  lambda = 1e-4,
                  ridge_lambda = 1e-4,
                  seed = NULL,
                  parallel = FALSE,
                  n_cores = NULL,
                  device = "cpu",
                  verbose = FALSE) {
  call <- match.call()

  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("scfit(): the 'torch' package is required.")
  }
  if (!inherits(formula, "formula")) {
    stop("scfit(): `formula` must be a formula object.")
  }
  if (!is.data.frame(data)) {
    stop("scfit(): `data` must be a data frame.")
  }
  respondent <- .sc_coerce_colname(respondent, "respondent")
  task       <- .sc_coerce_colname(task, "task")
  profile    <- .sc_coerce_colname(profile, "profile")

  K <- as.integer(K)
  if (is.na(K) || K < 2L) {
    stop("scfit(): `K` must be an integer >= 2.")
  }

  ## ---- 1. Parse formula ----
  parsed    <- .sc_parse_formula(formula)
  response  <- parsed$response
  attr_vars <- parsed$attr_vars
  z_vars    <- parsed$z_vars
  if (length(z_vars) == 0L) {
    stop("scfit(): at least one Z variable (after `|`) is required by the DML estimator.")
  }
  for (nm in c(response, attr_vars, z_vars,
               respondent, task, profile)) {
    if (!nm %in% names(data)) {
      stop(sprintf("scfit(): column '%s' not found in `data`.", nm))
    }
  }

  ## ---- 2. Row-sort and shape-check ----
  data_sorted <- .sc_to_long(data, respondent = respondent,
                             task = task, profile = profile)

  ## ---- 3. Encode attributes and moderators ----
  enc <- .sc_encode(data_sorted, attr_vars = attr_vars, z_vars = z_vars)
  X      <- enc$X
  Z      <- enc$Z
  x_names <- enc$x_names
  z_names <- enc$z_names

  ## ---- 4. Build Delta X, task-level Z, respondent index ----
  built <- .sc_build_deltax(
    X             = X,
    Z             = Z,
    task_id       = data_sorted[[task]],
    profile_id    = data_sorted[[profile]],
    respondent_id = data_sorted[[respondent]]
  )
  deltaX          <- built$deltaX
  Z_task          <- built$Z_task
  respondent_task <- built$respondent_task
  colnames(deltaX) <- x_names
  colnames(Z_task) <- z_names

  ## ---- 5. Task-level outcome y ----
  ## The two profile rows of a task have complementary 0/1 choice
  ## indicators; `.sc_build_deltax()` takes X[profile==first] minus
  ## X[profile==second], so the natural task-level y is the choice of
  ## the FIRST profile.
  y_profile <- as.numeric(data_sorted[[response]])
  if (any(!y_profile %in% c(0, 1))) {
    stop(sprintf("scfit(): response column '%s' must be coded 0/1.",
                 response))
  }
  key <- paste(data_sorted[[respondent]], data_sorted[[task]], sep = "\r")
  ord <- order(key, data_sorted[[profile]])
  y_sorted <- y_profile[ord]
  idx1 <- seq(1L, length(y_sorted), by = 2L)
  y <- y_sorted[idx1]
  if (length(y) != nrow(deltaX)) {
    stop("scfit(): internal error aligning task-level outcome with deltaX.")
  }

  ## ---- 6. Resolve hidden ----
  if (is.character(hidden) && length(hidden) == 1L && hidden == "auto") {
    hidden_use <- .sc_auto_hidden(nrow(deltaX))
  } else if (is.numeric(hidden) && length(hidden) >= 1L && all(hidden >= 1)) {
    hidden_use <- as.integer(hidden)
  } else {
    stop("scfit(): `hidden` must be \"auto\" or a positive integer vector.")
  }

  ## ---- 7. Master RNG state save/restore (R-level via withr) ----
  ## `withr::defer()` is tied to the calling frame and guarantees
  ## cleanup order regardless of how the function exits.
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_r_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  withr::defer({
    if (had_seed) {
      assign(".Random.seed", old_r_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  old_torch_state <- tryCatch(torch::torch_get_rng_state(),
                              error = function(e) NULL)
  withr::defer({
    if (!is.null(old_torch_state)) {
      tryCatch(torch::torch_set_rng_state(old_torch_state),
               error = function(e) NULL)
    }
  })

  ## ---- 8. Fold assignment ----
  fold_id <- .sc_make_folds(respondent_task, K = K, seed = seed)

  ## ---- 9. Cross-fitting ----
  cf <- .sc_crossfit(
    deltaX        = deltaX,
    y             = y,
    Z             = Z_task,
    fold_id       = fold_id,
    hidden        = hidden_use,
    n_epochs      = n_epochs,
    learning_rate = learning_rate,
    lambda        = lambda,
    seed          = seed,
    parallel      = parallel,
    n_cores       = n_cores,
    device        = device,
    verbose       = verbose
  )
  beta_hat <- cf$beta_hat
  colnames(beta_hat) <- x_names

  ## ---- 10. Lambda(Z) ----
  lambda_obj <- .sc_estimate_lambda(
    beta_hat     = beta_hat,
    deltaX       = deltaX,
    Z            = Z_task,
    ridge_lambda = ridge_lambda
  )

  ## ---- 11. DML influence and point estimates ----
  infl <- .sc_influence_function(
    beta_hat   = beta_hat,
    lambda_obj = lambda_obj,
    deltaX     = deltaX,
    y          = y
  )
  theta <- infl$theta_hat
  names(theta) <- x_names

  ## ---- 12. Clustered vcov + iid vcov + DML/iid ratio ----
  vcov_cluster <- .sc_cluster_vcov(
    influence_raw = infl$influence_raw,
    theta_hat     = theta,
    respondent_id = respondent_task
  )
  vcov_iid <- .sc_iid_vcov(
    influence_raw = infl$influence_raw,
    theta_hat     = theta
  )
  rownames(vcov_cluster$vcov) <- colnames(vcov_cluster$vcov) <- x_names
  rownames(vcov_iid$vcov)     <- colnames(vcov_iid$vcov)     <- x_names
  se_ratio <- .sc_dml_iid_ratio(vcov_cluster$vcov, vcov_iid$vcov)

  ## ---- 13. Assemble sc_fit ----
  fit <- list(
    theta              = theta,
    vcov               = vcov_cluster$vcov,
    vcov_iid           = vcov_iid$vcov,
    se_ratio_dml_iid   = se_ratio,
    beta_hat           = beta_hat,
    Z                  = Z_task,
    deltaX             = deltaX,
    y                  = y,
    plugin             = infl$plugin,
    correction         = infl$correction,
    influence_raw      = infl$influence_raw,
    fold_id            = fold_id,
    lambda_obj         = lambda_obj,
    call               = call,
    formula            = formula,
    attr_names         = x_names,
    z_names            = z_names,
    respondent_id      = respondent_task,
    K                  = K,
    hidden             = hidden_use,
    seed               = seed,
    n_epochs           = as.integer(n_epochs),
    learning_rate      = learning_rate,
    device             = device,
    parallel           = isTRUE(parallel),
    n_cores            = n_cores,
    loss_traces        = cf$loss_traces
  )
  class(fit) <- c("sc_fit", "list")
  fit
}

#' Coerce a column-name argument to a length-1 character
#'
#' Accepts either a character string or an unquoted symbol (via NSE
#' from the caller).  Internal helper for `scfit()`.
#' @keywords internal
#' @noRd
.sc_coerce_colname <- function(x, arg_name) {
  if (is.character(x) && length(x) == 1L) {
    return(x)
  }
  stop(sprintf("scfit(): argument `%s` must be a single column name as a string.",
               arg_name))
}
