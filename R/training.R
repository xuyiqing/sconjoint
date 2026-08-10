## Training loop for the structural conjoint DNN (M2).
##
## Single-core port of `04_training.R` and the training function in
## `03_structural_dnn.R`.  Cross-fitting with respondent-clustered
## folds lives in M3; this file only implements `.sc_train_one()`
## which trains one network on a (delta_x, y, Z) triple.

#' Train a single conjoint DNN
#'
#' Full-batch gradient descent with Adam and BCE-with-logits loss,
#' matching the paper's v13 production training loop
#' (`code/03_structural_dnn.R::train_one_fold`).  L2 regularization
#' is applied via the optimizer's `weight_decay` argument (the same
#' channel the production code uses), not as an explicit term added
#' to the loss --- for Adam the two are not equivalent because the
#' loss-side L2 gradient is rescaled by Adam's per-parameter adaptive
#' rates, whereas `weight_decay` is applied to the parameter update
#' directly.  The production rate is the v13 NT-adaptive
#' `K_adaptive / NT` rule (see `?scfit` argument `weight_decay`).
#'
#' Seeds are handled carefully: the call saves both the R RNG state
#' and the torch RNG state, sets them from `seed`, runs training,
#' then restores both.  No global state is leaked.
#'
#' @param deltaX Numeric matrix, `n x p`, the per-task attribute differences.
#' @param y Numeric vector of length `n` with 0/1 choice outcomes.
#' @param Z Numeric matrix, `n x p_z`, of respondent moderators.
#' @param hidden Integer vector of hidden-layer widths.  Defaults to
#'   `.sc_auto_hidden(nrow(deltaX))`.
#' @param n_epochs Integer, number of full-batch epochs.
#' @param learning_rate Numeric, Adam learning rate.
#' @param weight_decay Numeric, non-negative L2 coefficient passed
#'   to `torch::optim_adam(weight_decay = ...)`.  `0` disables L2.
#' @param seed Integer, master seed.  When `NULL` the current RNG
#'   state is used and nothing is restored.
#' @param device Character, `"cpu"` (default) or `"cuda"`.  Only CPU
#'   is bit-exact.
#' @param verbose Logical, print per-epoch summary if `TRUE`.
#' @param interactions One of `"none"` (default; historical behavior,
#'   bit-identical), `"lowrank"`, or `"explicit"`.  See
#'   `.sc_build_network()`.
#' @param X_A,X_B Numeric `n x p` profile-level dummy matrices (first /
#'   second profile of each task).  Required for
#'   `interactions = "lowrank"`.
#' @param F_int Numeric `n x q` matrix of identified interaction
#'   features `q_A - q_B`.  Required for `interactions = "explicit"`.
#' @param interaction_rank Integer rank of the low-rank head.
#' @param lambda_V Non-negative ridge penalty added to the loss for the
#'   interaction head (`lambda_V * sum(V^2)` or `lambda_V * sum(w^2)`),
#'   on top of the optimizer-level `weight_decay` shared by all
#'   parameters.
#' @return A list with `net` (trained `nn_module`), `loss_trace`
#'   (numeric vector of per-epoch training losses), and `final_loss`.
#' @keywords internal
#' @noRd
.sc_train_one <- function(deltaX, y, Z,
                          hidden = NULL,
                          n_epochs = 1000L,
                          learning_rate = 0.01,
                          weight_decay = 1e-4,
                          seed = NULL,
                          device = "cpu",
                          verbose = FALSE,
                          interactions = "none",
                          X_A = NULL,
                          X_B = NULL,
                          F_int = NULL,
                          interaction_rank = 2L,
                          lambda_V = 1e-2) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_train_one(): the 'torch' package is required.")
  }
  if (!is.matrix(deltaX) || !is.numeric(deltaX)) {
    stop(".sc_train_one(): `deltaX` must be a numeric matrix.")
  }
  if (!is.matrix(Z) || !is.numeric(Z)) {
    stop(".sc_train_one(): `Z` must be a numeric matrix.")
  }
  if (length(y) != nrow(deltaX) || nrow(Z) != nrow(deltaX)) {
    stop(".sc_train_one(): dimension mismatch between `deltaX`, `y`, and `Z`.")
  }
  if (is.null(hidden)) {
    hidden <- .sc_auto_hidden(nrow(deltaX))
  }
  interactions <- match.arg(interactions, c("none", "lowrank", "explicit"))
  if (identical(interactions, "lowrank")) {
    if (is.null(X_A) || is.null(X_B) ||
        nrow(X_A) != nrow(deltaX) || nrow(X_B) != nrow(deltaX)) {
      stop(".sc_train_one(): interactions = \"lowrank\" requires `X_A`, ",
           "`X_B` matching nrow(deltaX).")
    }
  }
  if (identical(interactions, "explicit")) {
    if (is.null(F_int) || nrow(F_int) != nrow(deltaX)) {
      stop(".sc_train_one(): interactions = \"explicit\" requires `F_int` ",
           "matching nrow(deltaX).")
    }
  }

  ## ------- RNG state capture + seeding ---------
  if (!is.null(seed)) {
    withr::local_preserve_seed()
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  dev <- torch::torch_device(device)
  dx  <- torch::torch_tensor(deltaX, dtype = torch::torch_float(), device = dev)
  zt  <- torch::torch_tensor(Z,      dtype = torch::torch_float(), device = dev)
  yt  <- torch::torch_tensor(as.numeric(y), dtype = torch::torch_float(), device = dev)
  xa_t <- xb_t <- fint_t <- NULL
  if (identical(interactions, "lowrank")) {
    xa_t <- torch::torch_tensor(X_A, dtype = torch::torch_float(), device = dev)
    xb_t <- torch::torch_tensor(X_B, dtype = torch::torch_float(), device = dev)
  } else if (identical(interactions, "explicit")) {
    fint_t <- torch::torch_tensor(F_int, dtype = torch::torch_float(), device = dev)
  }

  p_beta <- ncol(deltaX)
  p_z    <- ncol(Z)
  net <- .sc_build_network(p = p_beta, p_Z = p_z, hidden = hidden,
                           interactions = interactions,
                           interaction_rank = interaction_rank,
                           n_int_features = if (is.null(F_int)) 0L else ncol(F_int))
  net$to(device = dev)

  optimizer <- torch::optim_adam(net$parameters, lr = learning_rate,
                                 weight_decay = weight_decay)
  loss_fn   <- torch::nn_bce_with_logits_loss()

  loss_trace <- numeric(n_epochs)
  for (epoch in seq_len(n_epochs)) {
    net$train()
    optimizer$zero_grad()
    logit <- if (identical(interactions, "none")) {
      net$forward(dx, zt)
    } else if (identical(interactions, "lowrank")) {
      net$forward(dx, zt, x_a = xa_t, x_b = xb_t)
    } else {
      net$forward(dx, zt, f_int = fint_t)
    }
    loss  <- loss_fn(logit, yt)
    if (identical(interactions, "lowrank")) {
      loss <- loss + lambda_V * torch::torch_sum(net$V^2)
    } else if (identical(interactions, "explicit")) {
      loss <- loss + lambda_V * torch::torch_sum(net$w_int^2)
    }
    loss$backward()
    optimizer$step()
    loss_trace[epoch] <- as.numeric(loss$item())

    if (verbose && (epoch %% 100L == 0L || epoch == 1L)) {
      message(sprintf("  epoch %4d  loss = %.6f", epoch, loss_trace[epoch]))
    }
  }

  net$eval()
  list(
    net        = net,
    loss_trace = loss_trace,
    final_loss = loss_trace[n_epochs]
  )
}

#' Resolve `weight_decay` argument to a numeric L2 coefficient
#'
#' Implements the paper's v13 NT-adaptive rule when
#' `weight_decay = "adaptive"`:
#' \deqn{K_{adaptive} = \begin{cases} 15 & NT/p < 300 \\
#'                                    25 & NT/p \ge 300 \end{cases},\qquad
#'   \mathrm{weight\_decay} = K_{adaptive} / NT.}
#' Numeric input passes through unchanged (after validation).
#'
#' @param weight_decay Either the string `"adaptive"` or a
#'   non-negative numeric scalar.
#' @param NT Integer, the number of task-level observations.
#' @param p Integer, the number of attribute dummies.
#' @return A non-negative numeric scalar.
#' @keywords internal
#' @noRd
.sc_resolve_weight_decay <- function(weight_decay, NT, p) {
  if (is.character(weight_decay) && length(weight_decay) == 1L &&
      weight_decay == "adaptive") {
    K_adaptive <- if (NT / p < 300) 15L else 25L
    return(K_adaptive / NT)
  }
  if (is.numeric(weight_decay) && length(weight_decay) == 1L &&
      is.finite(weight_decay) && weight_decay >= 0) {
    return(as.numeric(weight_decay))
  }
  stop("scfit(): `weight_decay` must be \"adaptive\" or a non-negative numeric scalar.")
}

#' Predict `beta(Z)` from a trained network
#'
#' @param net A trained network with a `get_beta()` method.
#' @param Z_new Numeric matrix of new respondent moderators.
#' @return A numeric matrix of shape `nrow(Z_new) x p_beta`.
#' @keywords internal
#' @noRd
.sc_predict_beta <- function(net, Z_new) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_predict_beta(): the 'torch' package is required.")
  }
  if (!is.matrix(Z_new) || !is.numeric(Z_new)) {
    stop(".sc_predict_beta(): `Z_new` must be a numeric matrix.")
  }
  net$eval()
  net_dev <- tryCatch(net$parameters[[1L]]$device, error = function(e) NULL)
  zt <- torch::torch_tensor(Z_new, dtype = torch::torch_float())
  if (!is.null(net_dev)) zt <- zt$to(device = net_dev)
  beta <- torch::with_no_grad({
    net$get_beta(zt)
  })
  as.matrix(torch::as_array(beta))
}
