## `scfit()` -- structural conjoint DML estimator (M3 export).
##
## Wires together the M2 data-prep / DNN / training layers and the M3
## cross-fitting / Lambda / DML inference layers into a single
## formula-driven entry point.  `scfit()` is the first user-facing
## export of the package.  Full `sc_fit` methods (summary, predict,
## Full S3 method set: print, coef, vcov, summary, predict, plot.

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
#' @param learner First-stage learner for `beta(Z)`.  One of `"dnn"`
#'   (default; the cross-fitted structural deep network), `"enet"`
#'   (cross-fitted elastic-net logit on attribute differences and their
#'   first-order moderator interactions; requires the `glmnet` package), or
#'   `"grf"` (a generalized-random-forest localized logit; requires the
#'   `grf` package).  All three feed the identical DML inference layer ---
#'   only the first-stage `beta_hat` differs.  The DNN is the estimator used
#'   throughout the paper's applications; `"enet"` and `"grf"` are provided
#'   so the same debiased quantities can be computed with a flexible learner
#'   of the user's choice (see the package vignette / paper appendix).  The
#'   DNN-specific arguments (`hidden`, `n_epochs`, `learning_rate`,
#'   `weight_decay`, `device`, `keep_modules`, and the `stage2` family) are
#'   ignored when `learner != "dnn"`, and `stage2` is forced to `"none"`.
#' @param hidden Either the character string `"auto"` (default, picks
#'   a three-tier default from `N*T`, see `.sc_auto_hidden()`), or an
#'   integer vector of hidden-layer widths.
#' @param K Integer, number of respondent-clustered folds for
#'   cross-fitting.  Defaults to 10.
#' @param n_epochs Integer, number of full-batch Adam epochs per fold.
#'   Default `1000L` (matches the paper's v13 production runtime; see
#'   `code/04_training.R`).
#' @param learning_rate Numeric, Adam learning rate.
#' @param weight_decay Either the character string `"adaptive"`
#'   (default), or a non-negative numeric scalar passed to the Adam
#'   optimizer's `weight_decay` argument.  When `"adaptive"`, the
#'   per-fit weight decay is resolved from the dataset shape using the
#'   paper's v13 rule:
#'   \deqn{\mathrm{weight\_decay} = K_{adaptive} / NT,\quad
#'         K_{adaptive} = \begin{cases} 15 & NT/p < 300 \\
#'                                      25 & NT/p \ge 300 \end{cases}}{
#'         weight_decay = K_adaptive / NT, K_adaptive = 15 if NT/p<300 else 25}
#'   where `NT` is the number of task-level observations and `p` is the
#'   number of attribute dummies.  This is what generated every current
#'   paper number; see `code/04_training.R` and memo 42 for the
#'   rationale.  Pass a fixed numeric (e.g. `1e-4`) to override.
#' @param ridge_lambda Numeric ridge penalty used both in the
#'   Lambda(Z) regression and in the Lambda inversion.  Distinct from
#'   `weight_decay`, which regularises the DNN; this regularises the
#'   Lambda(Z) ridge.
#' @param enet_alpha Elastic-net mixing parameter in `[0, 1]` used when
#'   `learner = "enet"` (`1` = lasso, `0` = ridge).  Default `0.5`.
#'   Ignored for other learners.
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
#' @param keep_modules Logical. If \code{TRUE} (the default), the
#'   per-fold trained torch \code{nn_module} objects are stored on the
#'   returned \code{sc_fit} object, enabling
#'   \code{predict(fit, newdata = ...)} forward-pass on new moderator
#'   data.  Set to \code{FALSE} to reduce object size when forward-pass
#'   prediction is not needed.
#' @param verbose Logical, print per-epoch training progress.
#' @param stage2 Stage-2 estimator.  One of `"map_c5"` (default; paper
#'   EnsC5), `"none"` (skip Stage 2; v0.1 behavior), `"varref"`
#'   (experimental alternative prior), or `"mixed_logit"` (DNN-offset
#'   `lme4::glmer` BLUP per paper §A.4; requires the `lme4` package).
#'   The Stage-2 result is what `object$beta_hat` holds and what all
#'   `sc_*` quantity functions read by default; the Stage-1 single-DNN
#'   matrix is kept on `object$beta_hat_dnn`.  DML point estimates and
#'   clustered SEs are unchanged across `stage2` choices on the same
#'   `seed`: they are always computed from the Stage-1 cross-fit.
#' @param stage2_seed Integer seed for the 2nd DNN in the Stage-2
#'   ensemble.  Independent of `seed`.  Default `12345L`.
#' @param varref_floor Numeric lower bound on the per-coefficient prior
#'   variance when `stage2 = "varref"`.  Default `1e-3`, which matches
#'   the production setting used for continuous-attribute designs
#'   (Ballard-Rosa tax rates; see paper memo 42).  The previous default
#'   of `0.01` clipped every coefficient and over-shrank under v13,
#'   collapsing BR top-bracket validation `r` from `0.39` to `0.13`.
#'   On factor-dummy designs the natural varref value sits well above
#'   `1e-3`, so the floor is rarely binding there.  Ignored unless
#'   `stage2 = "varref"`.
#' @param normalize_deltaX Logical (default `FALSE`, matching the
#'   paper's stated runtime).  When `TRUE`,
#'   each column of the internal `deltaX` matrix is divided by its
#'   sample SD before training, Lambda(Z) estimation, DML inference, and
#'   the Stage-2 MAP update.  At return all user-facing slots
#'   (`theta`, `vcov`, `beta_hat`, `sigma_prior`, etc.) are
#'   un-standardized back to the original-units scale, so
#'   `fit$deltaX %*% fit$theta` reproduces the correct logit index in
#'   the user's input units.  The SDs used are stored on
#'   `fit$sd_dx` for diagnostics.
#'
#'   Use `normalize_deltaX = TRUE` on designs with continuous
#'   attributes whose ΔX columns span very different scales (e.g.
#'   percentage-point tax rates alongside 0/1 dummies).  The default
#'   v0.2 score-based MAP prior is calibrated assuming
#'   `Var(ΔX_k) ~ 1` (factor dummies under typical randomization);
#'   on large-scale continuous attributes the prior becomes loose by
#'   the same factor as `Var(ΔX_k)` and per-respondent MAP estimates
#'   drift to extreme tails.  Internal standardization puts every
#'   coefficient on a common-variance internal scale, restoring the
#'   c5 prior's intended regularization strength.  For factor-dummy
#'   designs `normalize_deltaX` is a near-no-op (each dummy's SD is
#'   already on a similar scale).
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
#' See `summary.sc_fit()`, `predict.sc_fit()`, and `plot.sc_fit()`.
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
                  learner = c("dnn", "enet", "grf"),
                  hidden = "auto",
                  K = 10L,
                  n_epochs = 1000L,
                  learning_rate = 0.01,
                  weight_decay = "adaptive",
                  ridge_lambda = 1e-4,
                  enet_alpha = 0.5,
                  seed = NULL,
                  parallel = FALSE,
                  n_cores = NULL,
                  device = "cpu",
                  keep_modules = TRUE,
                  verbose = FALSE,
                  stage2 = c("map_c5", "none", "varref", "mixed_logit"),
                  stage2_seed = 12345L,
                  varref_floor = 1e-3,
                  normalize_deltaX = FALSE) {
  call <- match.call()
  learner <- match.arg(learner)
  stage2_supplied <- !missing(stage2)
  stage2 <- match.arg(stage2)

  ## Stage 2 (the MAP/varref/mixed-logit refinement) is DNN-specific: its
  ## ensemble retrains a second DNN.  For the flexible alternative learners
  ## the first-stage matrix `beta_hat` already is the estimate every quantity
  ## function reads, so we pass it through unchanged.  DML point estimates and
  ## clustered SEs are computed from the Stage-1 cross-fit regardless of
  ## `stage2`, so this does not affect inference.  Warn only if the user
  ## explicitly asked for a DNN-only stage2; the default downgrade is silent.
  if (learner != "dnn" && stage2 != "none") {
    if (stage2_supplied) {
      warning(sprintf(
        "scfit(): stage2 = \"%s\" is only available for learner = \"dnn\"; using stage2 = \"none\" for learner = \"%s\".",
        stage2, learner), call. = FALSE)
    }
    stage2 <- "none"
  }

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
  factor_levels <- enc$factor_levels
  attr_map_enc  <- enc$attr_map

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

  ## ---- 4b. Optional internal standardization of deltaX ----
  ## When `normalize_deltaX = TRUE`, divide each deltaX column by its
  ## sample SD so the internal training / Lambda(Z) / DML / MAP pipeline
  ## sees attributes on a common ~unit scale.  At assembly time we
  ## un-standardize the user-facing slots (beta_hat, theta, vcov,
  ## sigma_prior, sigma_post_diag, etc.) back to the original-units
  ## scale, so a user reading `coef(fit)`, `vcov(fit)`, or
  ## `fit$deltaX %*% fit$theta` sees results consistent with the input
  ## data they supplied.
  ##
  ## Motivation: the v0.2 score-based MAP prior is calibrated assuming
  ## ΔX columns have Var ~ 1 (true for factor dummies in {-1, 0, 1}
  ## under typical balanced randomization).  On designs with
  ## large-scale continuous attributes (e.g. Ballard-Rosa tax rates in
  ## percentage points, where Var(ΔX_k) ~ 200), `sigma_prior` blows up
  ## by the same factor and the MAP step barely regularizes ---
  ## per-respondent betas drift to extreme tails.  Standardizing ΔX
  ## first puts every column's variance at 1 and gives the c5 prior
  ## a uniform meaning across attribute types.
  if (isTRUE(normalize_deltaX)) {
    sd_dx <- apply(deltaX, 2L, stats::sd)
    sd_dx[!is.finite(sd_dx) | sd_dx < 1e-10] <- 1
    deltaX_internal <- sweep(deltaX, 2L, sd_dx, FUN = "/")
    colnames(deltaX_internal) <- x_names
  } else {
    sd_dx <- rep(1, ncol(deltaX))
    names(sd_dx) <- x_names
    deltaX_internal <- deltaX
  }

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
    hidden_use <- .sc_auto_hidden(nrow(deltaX), p_beta = ncol(deltaX))
  } else if (is.numeric(hidden) && length(hidden) >= 1L && all(hidden >= 1)) {
    hidden_use <- as.integer(hidden)
  } else {
    stop("scfit(): `hidden` must be \"auto\" or a positive integer vector.")
  }

  ## ---- 6b. Resolve weight_decay ----
  ## "adaptive" --> paper v13 rule (memo 42, code/04_training.R).
  ## Numeric pass-through otherwise.  Delegated to
  ## `.sc_resolve_weight_decay()` so the rule itself can be unit-tested.
  weight_decay_use <- .sc_resolve_weight_decay(
    weight_decay,
    NT = nrow(deltaX_internal),
    p  = ncol(deltaX_internal)
  )

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

  ## ---- 9. Cross-fitting (first stage) ----
  ## All internal computations downstream use `deltaX_internal` (the
  ## standardized matrix when normalize_deltaX = TRUE; identical to
  ## deltaX otherwise).  We un-standardize the user-facing slots at
  ## assembly time using `sd_dx`.  The first stage is pluggable: the DNN
  ## (default) or a flexible alternative learner.  Each produces the same
  ## out-of-sample `beta_hat` (N x p) that the DML layer consumes.
  cf <- switch(
    learner,
    dnn = .sc_crossfit(
      deltaX        = deltaX_internal,
      y             = y,
      Z             = Z_task,
      fold_id       = fold_id,
      hidden        = hidden_use,
      n_epochs      = n_epochs,
      learning_rate = learning_rate,
      weight_decay  = weight_decay_use,
      seed          = seed,
      parallel      = parallel,
      n_cores       = n_cores,
      device        = device,
      verbose       = verbose
    ),
    enet = .sc_crossfit_enet(
      deltaX  = deltaX_internal,
      y       = y,
      Z       = Z_task,
      fold_id = fold_id,
      alpha   = enet_alpha,
      seed    = seed
    ),
    grf = .sc_crossfit_grf(
      deltaX        = deltaX_internal,
      y             = y,
      Z             = Z_task,
      fold_id       = fold_id,
      respondent_id = respondent_task,
      seed          = seed
    )
  )
  beta_hat <- cf$beta_hat
  colnames(beta_hat) <- x_names

  ## ---- 10. Lambda(Z) ----
  lambda_obj <- .sc_estimate_lambda(
    beta_hat     = beta_hat,
    deltaX       = deltaX_internal,
    Z            = Z_task,
    ridge_lambda = ridge_lambda
  )

  ## ---- 11. DML influence and point estimates ----
  infl <- .sc_influence_function(
    beta_hat      = beta_hat,
    lambda_obj    = lambda_obj,
    deltaX        = deltaX_internal,
    y             = y,
    respondent_id = respondent_task
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
    theta_hat     = theta,
    respondent_id = respondent_task
  )
  rownames(vcov_cluster$vcov) <- colnames(vcov_cluster$vcov) <- x_names
  rownames(vcov_iid$vcov)     <- colnames(vcov_iid$vcov)     <- x_names
  se_ratio <- .sc_dml_iid_ratio(vcov_cluster$vcov, vcov_iid$vcov)

  ## ---- 12b. Stage 2 (paper §3 hybrid update) ----
  ## DML output above is frozen — it used `beta_hat` (single Stage-1 DNN).
  ## Stage 2 (re-)produces a Stage-2-refined task-level `beta_hat` and
  ## adds auxiliary slots.  When `stage2 = "none"` this is a fast
  ## pass-through.
  stage2_out <- .sc_run_stage2(
    stage2         = stage2,
    beta_hat_dnn   = beta_hat,
    deltaX         = deltaX_internal,
    y              = y,
    Z              = Z_task,
    respondent_id  = respondent_task,
    hidden         = hidden_use,
    n_epochs       = n_epochs,
    learning_rate  = learning_rate,
    weight_decay   = weight_decay_use,
    K              = K,
    stage2_seed    = stage2_seed,
    varref_floor   = varref_floor,
    parallel       = parallel,
    n_cores        = n_cores,
    device         = device,
    verbose        = verbose
  )

  ## ---- 12c. Un-standardize coefficients back to original-deltaX scale ----
  ## All internal slots above ran on `deltaX_internal` (= deltaX / sd_dx
  ## column-wise when normalize_deltaX = TRUE).  Apply the inverse
  ## transform so user-facing slots are on the original-units scale:
  ## β_orig = β_std / sd_dx (col), θ_orig = θ_std / sd_dx,
  ## Vcov_orig[i,j] = Vcov_std[i,j] / (sd_dx[i] * sd_dx[j]),
  ## σ²_orig = σ²_std / sd_dx^2.  No-op when normalize_deltaX = FALSE
  ## (sd_dx = 1).
  unstd_beta <- function(B) {
    if (is.null(B)) return(B)
    sweep(B, 2L, sd_dx, FUN = "/")
  }
  unstd_vec <- function(v) {
    if (is.null(v)) return(v)
    v / sd_dx
  }
  unstd_var <- function(v) {  # variance scales like 1/sd^2
    if (is.null(v)) return(v)
    v / (sd_dx^2)
  }
  unstd_vcov <- function(V) {
    if (is.null(V)) return(V)
    out <- V / outer(sd_dx, sd_dx)
    rownames(out) <- colnames(out) <- x_names
    out
  }

  theta_orig          <- unstd_vec(theta)
  vcov_orig           <- unstd_vcov(vcov_cluster$vcov)
  vcov_iid_orig       <- unstd_vcov(vcov_iid$vcov)
  plugin_orig         <- unstd_vec(infl$plugin)
  correction_orig     <- unstd_beta(infl$correction)
  influence_raw_orig  <- unstd_beta(infl$influence_raw)

  ## ---- 13. Assemble sc_fit ----
  fit <- list(
    theta              = theta_orig,
    vcov               = vcov_orig,
    vcov_iid           = vcov_iid_orig,
    se_ratio_dml_iid   = se_ratio,
    beta_hat           = unstd_beta(stage2_out$beta_hat),
    beta_hat_dnn       = unstd_beta(stage2_out$beta_hat_dnn),
    beta_hat_dnn2      = unstd_beta(stage2_out$beta_hat_dnn2),
    beta_hat_ens       = unstd_beta(stage2_out$beta_hat_ens),
    beta_hat_resp      = unstd_beta(stage2_out$beta_hat_resp),
    sigma_prior        = unstd_var(stage2_out$sigma_prior),
    sigma_post_diag    = unstd_var(stage2_out$sigma_post_diag),
    sd_dx              = sd_dx,
    normalize_deltaX   = isTRUE(normalize_deltaX),
    stage2_method      = stage2_out$stage2_method,
    stage2_warnings    = stage2_out$stage2_warnings,
    stage2_seed        = as.integer(stage2_seed),
    Z                  = Z_task,
    deltaX             = deltaX,
    y                  = y,
    plugin             = plugin_orig,
    correction         = correction_orig,
    influence_raw      = influence_raw_orig,
    fold_id            = fold_id,
    lambda_obj         = lambda_obj,
    call               = call,
    formula            = formula,
    attr_names         = x_names,
    attr_vars          = attr_vars,
    factor_levels      = factor_levels,
    attr_map           = attr_map_enc,
    z_names            = z_names,
    respondent_id      = respondent_task,
    K                  = K,
    hidden             = hidden_use,
    seed               = seed,
    n_epochs           = as.integer(n_epochs),
    learning_rate      = learning_rate,
    weight_decay_used  = weight_decay_use,
    device             = device,
    parallel           = isTRUE(parallel),
    n_cores            = n_cores,
    loss_traces        = cf$loss_traces,
    learner            = learner,
    nets               = if (learner == "dnn" && isTRUE(keep_modules)) cf$nets else NULL,
    fold_models        = if (learner != "dnn" && isTRUE(keep_modules)) cf$nets else NULL,
    keep_modules       = isTRUE(keep_modules)
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
