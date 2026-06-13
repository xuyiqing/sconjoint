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
#' @param respondent_weights Optional survey/design weights for respondents.
#'   May be `NULL` (equal respondent weights), a column name in `data`, or a
#'   numeric vector of length `nrow(data)`. Weights must be finite,
#'   non-negative, and constant within respondent. They affect the
#'   respondent-level estimand aggregation and clustered standard errors; they
#'   do not reweight first-stage learner training.
#' @param enet_alpha Elastic-net mixing parameter in `[0, 1]` used when
#'   `learner = "enet"` (`1` = lasso, `0` = ridge).  Default `0.5`.
#'   Ignored for other learners.
#' @param enet_df,enet_interactions Basis-expansion controls for
#'   `learner = "enet"`.  Each continuous moderator is expanded into a natural
#'   cubic spline with `enet_df` degrees of freedom (default `4`), and pairwise
#'   moderator products are added when `enet_interactions = TRUE` (default).
#'   This lets the elastic-net first stage approximate a nonlinear `beta(Z)`,
#'   with the penalty selecting among the expanded terms.  Set `enet_df = 1`
#'   and `enet_interactions = FALSE` for a linear-in-moderators first stage.
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
#' @param torch_threads Optional integer; if supplied, caps the number of
#'   intra-op threads torch uses (`torch::torch_set_num_threads()`).
#'   Results are invariant to the thread count, so this is purely a
#'   stability / performance control. The main use is `torch_threads = 1`
#'   as a workaround for the rare case where torch's default multi-threaded
#'   CPU backend aborts the R session (reported on some Windows systems).
#'   torch honors the thread count only on the first such call of a session,
#'   so set it on your first `scfit()` call in a fresh session; later calls
#'   are silently ignored. `NULL` (the default) leaves torch's setting
#'   unchanged.
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
#'
#'   Stage 2 is only implemented for `learner = "dnn"`.  With
#'   `learner = "enet"` or `"grf"` it is forced to `"none"` and a
#'   once-per-session warning is emitted, because the per-respondent
#'   betas then carry no empirical-Bayes shrinkage (see the `learner`
#'   argument and the *Inference validity by quantity* section below).
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
#'   attributes whose `deltaX` columns span very different scales (e.g.
#'   percentage-point tax rates alongside 0/1 dummies).  The default
#'   v0.2 score-based MAP prior is calibrated assuming
#'   `Var(deltaX_k) ~ 1` (factor dummies under typical randomization);
#'   on large-scale continuous attributes the prior becomes loose by
#'   the same factor as `Var(deltaX_k)` and per-respondent MAP estimates
#'   drift to extreme tails.  Internal standardization puts every
#'   coefficient on a common-variance internal scale, restoring the
#'   c5 prior's intended regularization strength.  For factor-dummy
#'   designs `normalize_deltaX` is a near-no-op (each dummy's SD is
#'   already on a similar scale).
#' @param interactions One of `"none"` (default), `"lowrank"`, or
#'   `"explicit"`.  Adds a POPULATION-LEVEL attribute-interaction term
#'   to the profile utility, `u(X) = X'beta_i + g(X)`, so the choice
#'   index becomes `deltaX'beta_i + g(X_A) - g(X_B)`.
#'
#'   * `"lowrank"`: `g(X) = ||V'X||^2` with `V` a `p x interaction_rank`
#'     parameter trained inside the torch mean stage and ridge-penalized
#'     by `lambda_V`.  Note the difference-of-quadratics form: the
#'     quadratic-in-the-difference `||V' deltaX||^2` is profile-swap
#'     invariant and structurally incoherent for forced choice, and is
#'     deliberately not used.
#'   * `"explicit"`: `g(X_A) - g(X_B) = (q_A - q_B)'w` over the
#'     IDENTIFIED interaction features only (cross-attribute dummy
#'     products; within-attribute pairs are structurally zero and
#'     diagonal terms are collinear with the main effects), with `w` a
#'     ridge-penalized (`lambda_V`) linear head in the same torch
#'     optimization.
#'
#'   The interaction term is population-level (it does not vary with
#'   `Z`): respondent-level interaction residuals are not estimable at
#'   typical conjoint task counts.  Inference always uses the explicit
#'   linear-in-parameters representation: the expanded regressor
#'   `[deltaX, q_A - q_B]` (identified features only), an expanded local
#'   Gram `Lambda(Z)`, and the orthogonal score on the expanded
#'   coefficient vector with cross-fitted nuisances -- under
#'   `interactions = "lowrank"` the trained `V V'` is linearized onto
#'   the identified features (the diagonal of `V V'` is absorbed into
#'   the main effects), so the DML theory for the linear-in-parameters
#'   logit applies verbatim.  `theta` and `vcov` on the returned fit
#'   remain the main-effect subvector / submatrix; the interaction
#'   coefficients and their clustered variance live on
#'   `fit$interaction`.  Under `interactions != "none"`, `beta_hat`
#'   (and `beta_i` generally) is the main-effect part of the utility at
#'   the no-interaction baseline, not the all-else-equal effect.
#'
#'   Requires `learner = "dnn"`, all-factor attributes,
#'   `normalize_deltaX = FALSE`, and `stage2 != "mixed_logit"`.  With
#'   the default `"none"` the historical code path runs untouched
#'   (byte-identical results under the same seed).
#' @param interaction_rank Integer rank `r` of the low-rank head
#'   (default `2L`).  Only used when `interactions = "lowrank"`.  Note
#'   that only the cross-attribute blocks of `W = V V'` are identified,
#'   never `V` itself; the fit reports `W` (`fit$interaction$W_avg`).
#' @param lambda_V Non-negative ridge penalty on the interaction head
#'   (default `1e-2`), added to the training loss as
#'   `lambda_V * sum(V^2)` or `lambda_V * sum(w^2)`.  See the section
#'   *Plugin-path attenuation under interactions* below: `lambda_V`
#'   shrinks the mean-stage interaction weights `w_hat` that every
#'   plugin-path quantity consumes.
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
#' * `loss_traces` -- list of per-fold training loss curves;
#' * `interaction` -- `NULL` unless `interactions != "none"`; a list with
#'   the identified feature set (`pairs`, `feature_names`), the DML
#'   interaction coefficients and clustered variance (`theta`, `vcov`,
#'   `se`; the full expanded `theta_full` / `vcov_full`), the
#'   population-level plugin coefficients (`w_hat`; per-fold `w_fold`),
#'   the averaged `W_avg = mean(V V')` under `"lowrank"` (only the
#'   cross-attribute blocks of `W` are identified -- `V` itself never
#'   is), the realized-task features `F_int` and cross-fitted offsets
#'   (`g_offset`, `g_offset_ens`, `g_offset_task`), and the interaction
#'   block of the orthogonal-score correction (`correction_int`).
#'
#' @section Inference validity by quantity:
#' The `sc_*` quantity functions fall into two groups with different
#' inferential status, and it is worth being explicit about which is which.
#'
#' \strong{DML quantities (valid debiased inference).} The average
#' structural parameters and the functionals built on the orthogonal
#' (Neyman-orthogonal) score carry asymptotically valid, respondent-clustered
#' standard errors and confidence intervals: `coef()` / `summary()` /
#' `vcov()` (the average `theta`), and the debiased quantities
#' `sc_average()`, `sc_ame()`, `sc_counterfactual(vartype = "orthogonal")`,
#' and `sc_mrs()` / `sc_wtp()`. Their SEs come from the cross-fitted
#' influence function and are unchanged across `stage2` choices.
#'
#' \strong{Model-based / empirical-Bayes summaries (descriptive, not
#' debiased).} The distribution-over-respondents quantities are plug-in
#' functionals of the recovered per-respondent `beta(Z_i)` and inherit that
#' object's finite-`T` shrinkage. They are descriptive summaries, not
#' debiased estimators: `sc_polarization()`, `sc_fraction_preferring()`,
#' `sc_direction_intensity()`, `sc_heterogeneity_test()`, `sc_clusters()`,
#' `sc_optimal_profile()`, and the other per-respondent quantities. For the
#' threshold/fraction quantities (`sc_polarization()`,
#' `sc_fraction_preferring()`) a respondent-cluster wild bootstrap is
#' available via `se_method = "wild_bootstrap"`, which quantifies the
#' \emph{sampling} variability of the fraction; it does not remove the
#' shrinkage bias that pulls each `beta_i` toward the population mean (and so
#' biases these fractions toward consensus under short panels).
#'
#' See `summary.sc_fit()`, `predict.sc_fit()`, and `plot.sc_fit()`.
#'
#' @section Plugin-path attenuation under interactions:
#' **Under `interactions != "none"` the package carries two estimates of
#' the interaction coefficients, and they are NOT interchangeable.**
#' The debiased (DML-corrected) interaction estimates live on
#' `fit$interaction$theta` (clustered variance `fit$interaction$vcov`)
#' and are approximately unbiased.  The mean-stage plugin weights
#' `fit$interaction$w_hat` are ridge-attenuated by `lambda_V`: at the
#' default `lambda_V = 1e-2` on a strong-interaction simulation they
#' are shrunk to roughly 40--50% of the true interaction coefficients.
#' Main effects are approximately unbiased on both paths.
#'
#' The following quantities consume `w_hat` (via the cross-fitted
#' offsets built from it), so their interaction contribution inherits
#' the attenuation: `sc_counterfactual(vartype = "plugin")`,
#' `sc_decisiveness()`, `sc_optimal_profile()`, `sc_surplus()`,
#' `sc_welfare_change()`, `predict(type = "logit")` and
#' `predict(type = "prob")`, and the Stage-2 MAP update (which takes
#' the cross-fitted interaction term as a known per-task offset when
#' refining `beta_hat`).  For population-level interaction effects,
#' read `fit$interaction$theta` or use an orthogonal-path quantity
#' (e.g. `sc_counterfactual(vartype = "orthogonal")`).
#'
#' `lambda_V` therefore trades prediction stability (a stronger ridge
#' stabilizes the trained interaction head) against plugin-path
#' attenuation (a stronger ridge shrinks every `w_hat`-based offset
#' toward the no-interaction index).  There is currently no automatic
#' selection rule for `lambda_V`.
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
                  respondent_weights = NULL,
                  enet_alpha = 0.5,
                  enet_df = 4L,
                  enet_interactions = TRUE,
                  seed = NULL,
                  parallel = FALSE,
                  n_cores = NULL,
                  device = "cpu",
                  torch_threads = NULL,
                  keep_modules = TRUE,
                  verbose = FALSE,
                  stage2 = c("map_c5", "none", "varref", "mixed_logit"),
                  stage2_seed = 12345L,
                  varref_floor = 1e-3,
                  normalize_deltaX = FALSE,
                  interactions = c("none", "lowrank", "explicit"),
                  interaction_rank = 2L,
                  lambda_V = 1e-2) {
  call <- match.call()
  learner <- match.arg(learner)
  stage2_supplied <- !missing(stage2)
  stage2 <- match.arg(stage2)
  interactions <- match.arg(interactions)

  if (!identical(interactions, "none")) {
    if (!identical(learner, "dnn")) {
      stop("scfit(): interactions != \"none\" is only available for ",
           "learner = \"dnn\".")
    }
    if (identical(stage2, "mixed_logit")) {
      stop("scfit(): stage2 = \"mixed_logit\" does not support an ",
           "interaction offset; use stage2 = \"map_c5\", \"varref\", or \"none\".")
    }
    if (isTRUE(normalize_deltaX)) {
      stop("scfit(): `normalize_deltaX = TRUE` is not supported with ",
           "interactions != \"none\" (the interaction features are built on ",
           "the original dummy scale; factor designs do not need ",
           "standardization).")
    }
    if (!is.numeric(interaction_rank) || length(interaction_rank) != 1L ||
        is.na(interaction_rank) || interaction_rank < 1L) {
      stop("scfit(): `interaction_rank` must be a positive integer.")
    }
    if (!is.numeric(lambda_V) || length(lambda_V) != 1L ||
        !is.finite(lambda_V) || lambda_V < 0) {
      stop("scfit(): `lambda_V` must be a non-negative scalar.")
    }
  }

  ## Optional cap on torch's intra-op thread count.  torch honors this only
  ## on the first torch_set_num_threads() of a session (before any torch
  ## work); later calls warn and are ignored.  Results are invariant to the
  ## count (the determinism guarantee holds), so this is purely a
  ## stability / performance control --- e.g. `torch_threads = 1` works
  ## around the R-session abort some users hit with torch's default
  ## multi-threaded backend on Windows.  No restore: the count is
  ## process-global and cannot be reset once torch work has started.
  if (!is.null(torch_threads)) {
    if (!is.numeric(torch_threads) || length(torch_threads) != 1L ||
        is.na(torch_threads) || torch_threads < 1) {
      stop("`torch_threads` must be a single positive integer, or NULL.")
    }
    torch_threads <- as.integer(torch_threads)
    if (torch::torch_get_num_threads() != torch_threads) {
      suppressWarnings(try(torch::torch_set_num_threads(torch_threads),
                           silent = TRUE))
    }
  }

  ## Stage 2 (the MAP/varref/mixed-logit refinement) is DNN-specific: its
  ## ensemble retrains a second DNN.  For the flexible alternative learners
  ## the first-stage matrix `beta_hat` already is the estimate every quantity
  ## function reads, so we pass it through unchanged.  DML point estimates and
  ## clustered SEs (theta / vcov, and the debiased quantities built on them)
  ## are computed from the Stage-1 cross-fit regardless of `stage2`, so the
  ## downgrade does not affect the inferential quantities.  What it does
  ## affect is the empirical-Bayes shrinkage of the per-respondent beta(Z):
  ## with stage2 = "none" the model-based / distributional summaries
  ## (sc_polarization, sc_fraction_preferring, sc_clusters, ...) read the
  ## raw first-stage betas rather than the Stage-2-refined ones.  Users
  ## should know this happened, so warn once per session whenever the
  ## downgrade actually applies (not only when stage2 was passed explicitly).
  if (learner != "dnn" && stage2 != "none") {
    reason <- if (stage2_supplied) {
      sprintf("you requested stage2 = \"%s\"", stage2)
    } else {
      sprintf("the default stage2 = \"%s\"", stage2)
    }
    .sc_warn_once(
      "stage2_downgrade",
      sprintf(paste0(
        "scfit(): stage2 (%s) is only implemented for learner = \"dnn\"; ",
        "for learner = \"%s\" it has been set to \"none\", so the per-respondent ",
        "beta(Z) receive no empirical-Bayes / MAP shrinkage. The DML quantities ",
        "(theta from coef()/summary(), and the debiased quantities) are computed ",
        "from the Stage-1 cross-fit and are unaffected. The model-based, ",
        "distribution-over-respondents summaries (sc_polarization(), ",
        "sc_fraction_preferring(), sc_clusters(), and the other per-respondent ",
        "quantities) will read the un-shrunk first-stage betas. ",
        "(This warning is shown once per session.)"),
        reason, learner))
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
  weight_col <- NULL
  if (!is.null(respondent_weights)) {
    if (is.character(respondent_weights) && length(respondent_weights) == 1L) {
      weight_col <- respondent_weights
      if (!weight_col %in% names(data)) {
        stop(sprintf("scfit(): respondent_weights column '%s' not found in `data`.", weight_col))
      }
    } else if (is.numeric(respondent_weights) && length(respondent_weights) == nrow(data)) {
      weight_col <- ".sc_tmp_respondent_weight"
      while (weight_col %in% names(data)) {
        weight_col <- paste0(weight_col, "_")
      }
      data[[weight_col]] <- respondent_weights
    } else {
      stop("scfit(): `respondent_weights` must be NULL, a column name, or a numeric vector with length nrow(data).")
    }
  }

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
               respondent, task, profile, weight_col)) {
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

  ## Interaction extension requires all-factor attributes: the
  ## identification accounting (diagonal terms collinear with mains,
  ## within-attribute products structurally zero) relies on one-hot
  ## dummies with X_k^2 = X_k.
  if (!identical(interactions, "none")) {
    numeric_attrs <- setdiff(attr_vars, names(factor_levels))
    if (length(numeric_attrs) > 0L) {
      stop("scfit(): interactions != \"none\" currently supports factor ",
           "attributes only; numeric attribute(s): ",
           paste(numeric_attrs, collapse = ", "), ".")
    }
  }

  ## ---- 4. Build Delta X, task-level Z, respondent index ----
  built <- .sc_build_deltax(
    X             = X,
    Z             = Z,
    task_id       = data_sorted[[task]],
    profile_id    = data_sorted[[profile]],
    respondent_id = data_sorted[[respondent]],
    keep_profiles = !identical(interactions, "none")
  )
  deltaX          <- built$deltaX
  Z_task          <- built$Z_task
  respondent_task <- built$respondent_task
  colnames(deltaX) <- x_names
  colnames(Z_task) <- z_names
  X_A_task <- built$X_A   # NULL when interactions = "none"
  X_B_task <- built$X_B

  ## ---- 4a. Identified interaction features (cross-attribute pairs) ----
  int_ctx <- NULL
  if (!identical(interactions, "none")) {
    attr_assign <- .sc_int_attr_assign(attr_map_enc, ncol(deltaX))
    ip    <- .sc_int_pairs(attr_assign, x_names)
    if (nrow(ip$pairs) == 0L) {
      stop("scfit(): interactions != \"none\" needs at least two attributes ",
           "(no cross-attribute pairs exist).")
    }
    F_all <- .sc_int_features(X_A_task, X_B_task, ip$pairs)
    ## Drop features with no empirical support (all-zero columns: level
    ## combinations never co-observed in this sample).
    nz <- which(colSums(abs(F_all)) > 0)
    if (length(nz) == 0L) {
      stop("scfit(): no identified interaction feature has empirical ",
           "support in this design.")
    }
    int_ctx <- list(
      pairs   = ip$pairs[nz, , drop = FALSE],
      names   = ip$names[nz],
      F_int   = F_all[, nz, drop = FALSE],
      n_dropped_nosupport = nrow(ip$pairs) - length(nz)
    )
    colnames(int_ctx$F_int) <- int_ctx$names
  }

  respondent_weight_task <- NULL
  if (!is.null(weight_col)) {
    w_profile <- as.numeric(data_sorted[[weight_col]])
    if (any(!is.finite(w_profile)) || any(w_profile < 0) || sum(w_profile) <= 0) {
      stop("scfit(): `respondent_weights` must be finite, non-negative, and not all zero.")
    }
    key_w <- as.character(data_sorted[[respondent]])
    dev_w <- tapply(w_profile, key_w, function(z) max(abs(z - z[1L])), simplify = TRUE)
    if (any(dev_w > 1e-8)) {
      stop("scfit(): `respondent_weights` must be constant within respondent.")
    }
    key_task <- paste(data_sorted[[respondent]], data_sorted[[task]], sep = "\r")
    ord_task <- order(key_task, data_sorted[[profile]])
    respondent_weight_task <- w_profile[ord_task][seq(1L, length(w_profile), by = 2L)]
    if (length(respondent_weight_task) != nrow(deltaX)) {
      stop("scfit(): internal error aligning respondent_weights with task rows.")
    }
  }

  ## Single-profile pool for debiased AME integration (the E_X integral over
  ## the design law P_X). A capped, deterministic subsample of the encoded
  ## single profiles, on the same original attribute scale as `deltaX`.
  n_pool   <- min(1000L, nrow(X))
  pool_idx <- if (nrow(X) > n_pool) {
    round(seq(1, nrow(X), length.out = n_pool))
  } else seq_len(nrow(X))
  profile_pool <- X[pool_idx, , drop = FALSE]
  colnames(profile_pool) <- x_names

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
  ## deltaX columns have Var ~ 1 (true for factor dummies in {-1, 0, 1}
  ## under typical balanced randomization).  On designs with
  ## large-scale continuous attributes (e.g. Ballard-Rosa tax rates in
  ## percentage points, where Var(deltaX_k) ~ 200), `sigma_prior` blows up
  ## by the same factor and the MAP step barely regularizes ---
  ## per-respondent betas drift to extreme tails.  Standardizing deltaX
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
      verbose       = verbose,
      interactions  = interactions,
      X_A           = X_A_task,
      X_B           = X_B_task,
      F_int         = if (is.null(int_ctx)) NULL else int_ctx$F_int,
      int_pairs     = if (is.null(int_ctx)) NULL else int_ctx$pairs,
      interaction_rank = interaction_rank,
      lambda_V         = lambda_V
    ),
    enet = .sc_crossfit_enet(
      deltaX       = deltaX_internal,
      y            = y,
      Z            = Z_task,
      fold_id      = fold_id,
      alpha        = enet_alpha,
      df           = enet_df,
      interactions = enet_interactions,
      seed         = seed
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

  ## ---- 10-12. Lambda(Z), DML influence, clustered vcov ----
  ##
  ## With an interaction term, inference runs on the EXPLICIT
  ## linear-in-parameters representation regardless of how the mean
  ## stage was fit: expanded regressor W_it = [deltaX_it, (q_A - q_B)_it]
  ## (identified features only), expanded coefficient
  ## gamma_hat = [beta_hat, w^(fold)], expanded local Gram Lambda(Z)
  ## over W, orthogonal score on the expanded coefficient vector.  The
  ## model is again a linear-in-parameters logit, so the existing DML
  ## machinery (`.sc_estimate_lambda()`, `.sc_influence_function()`,
  ## `.sc_cluster_vcov()`) applies verbatim on the expanded objects.
  ## The main-effect theta / vcov are the corresponding subvector /
  ## submatrix.  With interactions = "none" the expanded objects reduce
  ## exactly to the historical ones (q = 0), and the historical code
  ## path below runs untouched.
  theta_int_full <- NULL
  vcov_int_full  <- NULL
  infl_full      <- NULL
  if (is.null(int_ctx)) {
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
      respondent_id = respondent_task,
      respondent_weights = respondent_weight_task
    )
    theta <- infl$theta_hat
    names(theta) <- x_names

    ## ---- 12. Clustered vcov + iid vcov + DML/iid ratio ----
    vcov_cluster <- .sc_cluster_vcov(
      influence_raw = infl$influence_raw,
      theta_hat     = theta,
      respondent_id = respondent_task,
      respondent_weights = respondent_weight_task
    )
    vcov_iid <- .sc_iid_vcov(
      influence_raw = infl$influence_raw,
      theta_hat     = theta,
      respondent_id = respondent_task,
      respondent_weights = respondent_weight_task
    )
    rownames(vcov_cluster$vcov) <- colnames(vcov_cluster$vcov) <- x_names
    rownames(vcov_iid$vcov)     <- colnames(vcov_iid$vcov)     <- x_names
    se_ratio <- .sc_dml_iid_ratio(vcov_cluster$vcov, vcov_iid$vcov)
  } else {
    p_main   <- ncol(deltaX_internal)
    idx_main <- seq_len(p_main)
    exp_names <- c(x_names, int_ctx$names)

    ## Expanded regressor and out-of-fold expanded coefficient matrix.
    ## Row it (in fold k) carries [beta_hat_it, w^(k)]: the interaction
    ## coefficient is population-level but cross-fitted, so each row
    ## uses the head trained WITHOUT that respondent's fold.
    W_exp <- cbind(deltaX_internal, int_ctx$F_int)
    colnames(W_exp) <- exp_names
    gamma_hat <- cbind(beta_hat, cf$w_fold[fold_id, , drop = FALSE])
    colnames(gamma_hat) <- exp_names

    lambda_obj <- .sc_estimate_lambda(
      beta_hat     = gamma_hat,
      deltaX       = W_exp,
      Z            = Z_task,
      ridge_lambda = ridge_lambda
    )
    infl_full <- .sc_influence_function(
      beta_hat      = gamma_hat,
      lambda_obj    = lambda_obj,
      deltaX        = W_exp,
      y             = y,
      respondent_id = respondent_task,
      respondent_weights = respondent_weight_task
    )
    theta_full <- infl_full$theta_hat
    names(theta_full) <- exp_names
    vcov_cluster_full <- .sc_cluster_vcov(
      influence_raw = infl_full$influence_raw,
      theta_hat     = theta_full,
      respondent_id = respondent_task,
      respondent_weights = respondent_weight_task
    )
    vcov_iid_full <- .sc_iid_vcov(
      influence_raw = infl_full$influence_raw,
      theta_hat     = theta_full,
      respondent_id = respondent_task,
      respondent_weights = respondent_weight_task
    )
    rownames(vcov_cluster_full$vcov) <- colnames(vcov_cluster_full$vcov) <- exp_names
    rownames(vcov_iid_full$vcov)     <- colnames(vcov_iid_full$vcov)     <- exp_names

    ## Main-effect subvector / submatrix for the user-facing slots;
    ## interaction block kept for `fit$interaction`.
    theta <- theta_full[idx_main]
    infl <- list(
      theta_hat     = theta,
      plugin        = infl_full$plugin[idx_main],
      correction    = infl_full$correction[, idx_main, drop = FALSE],
      influence_raw = infl_full$influence_raw[, idx_main, drop = FALSE],
      phi_bar       = NULL
    )
    vcov_cluster <- list(
      vcov = vcov_cluster_full$vcov[idx_main, idx_main, drop = FALSE],
      se   = vcov_cluster_full$se[idx_main],
      M    = vcov_cluster_full$M
    )
    vcov_iid <- list(
      vcov = vcov_iid_full$vcov[idx_main, idx_main, drop = FALSE],
      se   = vcov_iid_full$se[idx_main]
    )
    se_ratio <- .sc_dml_iid_ratio(vcov_cluster$vcov, vcov_iid$vcov)

    theta_int_full <- theta_full
    vcov_int_full  <- vcov_cluster_full$vcov
  }

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
    verbose        = verbose,
    interactions   = interactions,
    X_A            = X_A_task,
    X_B            = X_B_task,
    F_int          = if (is.null(int_ctx)) NULL else int_ctx$F_int,
    int_pairs      = if (is.null(int_ctx)) NULL else int_ctx$pairs,
    interaction_rank = interaction_rank,
    lambda_V         = lambda_V,
    g_offset_stage1  = cf$g_offset
  )

  ## ---- 12c. Un-standardize coefficients back to original-deltaX scale ----
  ## All internal slots above ran on `deltaX_internal` (= deltaX / sd_dx
  ## column-wise when normalize_deltaX = TRUE).  Apply the inverse
  ## transform so user-facing slots are on the original-units scale:
  ## beta_orig = beta_std / sd_dx (col), theta_orig = theta_std / sd_dx,
  ## Vcov_orig[i,j] = Vcov_std[i,j] / (sd_dx[i] * sd_dx[j]),
  ## sigma^2_orig = sigma^2_std / sd_dx^2.  No-op when normalize_deltaX = FALSE
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

  ## ---- 12d. Interaction slot ----
  ## All interaction state in one place (NULL when interactions =
  ## "none").  `normalize_deltaX` is rejected upstream when an
  ## interaction term is requested, so no un-standardization applies
  ## here (sd_dx is identically 1).
  interaction_slot <- NULL
  if (!is.null(int_ctx)) {
    q_int    <- ncol(int_ctx$F_int)
    idx_int  <- p_main + seq_len(q_int)
    ## Population-level w for plugin index evaluations downstream:
    ## average over every cross-fitted head (Stage-1 folds, plus the
    ## Stage-2 ensemble's folds when a second DNN was trained).
    w_all <- cf$w_fold
    if (!is.null(stage2_out$w_fold2)) {
      w_all <- rbind(w_all, stage2_out$w_fold2)
    }
    w_hat <- colMeans(w_all)
    names(w_hat) <- int_ctx$names
    W_avg <- if (!is.null(cf$W_fold)) {
      Reduce(`+`, cf$W_fold) / length(cf$W_fold)
    } else NULL
    if (!is.null(W_avg)) {
      rownames(W_avg) <- colnames(W_avg) <- x_names
    }
    ## Per-task offset consistent with the `beta_hat` the quantity
    ## functions read: the Stage-2 ensemble offset when a second DNN
    ## was trained, the Stage-1 cross-fitted offset otherwise.
    g_offset_task <- if (!is.null(stage2_out$g_offset_ens)) {
      stage2_out$g_offset_ens
    } else {
      cf$g_offset
    }
    vcov_int <- vcov_int_full[idx_int, idx_int, drop = FALSE]
    interaction_slot <- list(
      type           = interactions,
      rank           = if (identical(interactions, "lowrank"))
                         as.integer(interaction_rank) else NA_integer_,
      lambda_V       = lambda_V,
      pairs          = int_ctx$pairs,
      feature_names  = int_ctx$names,
      n_features     = q_int,
      n_dropped_nosupport = int_ctx$n_dropped_nosupport,
      theta          = theta_int_full[idx_int],
      vcov           = vcov_int,
      se             = sqrt(pmax(diag(vcov_int), 0)),
      theta_full     = theta_int_full,
      vcov_full      = vcov_int_full,
      w_hat          = w_hat,
      w_fold         = cf$w_fold,
      w_fold2        = stage2_out$w_fold2,
      V_fold         = cf$V_fold,
      W_avg          = W_avg,
      F_int          = int_ctx$F_int,
      g_offset       = cf$g_offset,
      g_offset_ens   = stage2_out$g_offset_ens,
      g_offset_task  = g_offset_task,
      correction_int = infl_full$correction[, idx_int, drop = FALSE],
      influence_int  = infl_full$influence_raw[, idx_int, drop = FALSE]
    )
  }

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
    interaction        = interaction_slot,
    stage2_method      = stage2_out$stage2_method,
    stage2_warnings    = stage2_out$stage2_warnings,
    stage2_seed        = as.integer(stage2_seed),
    Z                  = Z_task,
    deltaX             = deltaX,
    profile_pool       = profile_pool,
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
    respondent_weights = respondent_weight_task,
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
