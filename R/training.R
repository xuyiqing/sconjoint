## Training loop for the structural conjoint DNN (M2).
##
## Single-core port of `04_training.R` and the training function in
## `03_structural_dnn.R`.  Cross-fitting with respondent-clustered
## folds lives in M3; this file only implements `.sc_train_one()`
## which trains one network on a (delta_x, y, Z) triple.

#' Train a single conjoint DNN
#'
#' Full-batch gradient descent with Adam and BCE-with-logits loss,
#' matching the prototype exactly.  An explicit L2 penalty on the
#' network parameters is added to the loss so that callers can pass
#' any `lambda` without depending on the optimizer's `weight_decay`
#' argument (which changed defaults across torch-for-R versions).
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
#' @param lambda Numeric, L2 penalty coefficient added to the loss.
#' @param seed Integer, master seed.  When `NULL` the current RNG
#'   state is used and nothing is restored.
#' @param device Character, `"cpu"` (default) or `"cuda"`.  Only CPU
#'   is bit-exact.
#' @param verbose Logical, print per-epoch summary if `TRUE`.
#' @return A list with `net` (trained `nn_module`), `loss_trace`
#'   (numeric vector of per-epoch training losses), and `final_loss`.
#' @keywords internal
#' @noRd
.sc_train_one <- function(deltaX, y, Z,
                          hidden = NULL,
                          n_epochs = 2000L,
                          learning_rate = 0.01,
                          lambda = 1e-4,
                          seed = NULL,
                          device = "cpu",
                          verbose = FALSE) {
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

  ## ------- RNG state capture + seeding ---------
  restore_R <- FALSE
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_r_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      restore_R <- TRUE
    } else {
      old_r_seed <- NULL
    }
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }
  on.exit({
    if (restore_R) {
      assign(".Random.seed", old_r_seed, envir = globalenv())
    }
  }, add = TRUE)

  dev <- torch::torch_device(device)
  dx  <- torch::torch_tensor(deltaX, dtype = torch::torch_float(), device = dev)
  zt  <- torch::torch_tensor(Z,      dtype = torch::torch_float(), device = dev)
  yt  <- torch::torch_tensor(as.numeric(y), dtype = torch::torch_float(), device = dev)

  p_beta <- ncol(deltaX)
  p_z    <- ncol(Z)
  net <- .sc_build_network(p = p_beta, p_Z = p_z, hidden = hidden)
  net$to(device = dev)

  optimizer <- torch::optim_adam(net$parameters, lr = learning_rate)
  loss_fn   <- torch::nn_bce_with_logits_loss()

  loss_trace <- numeric(n_epochs)
  for (epoch in seq_len(n_epochs)) {
    net$train()
    optimizer$zero_grad()
    logit <- net$forward(dx, zt)
    loss  <- loss_fn(logit, yt)

    if (lambda > 0) {
      l2 <- torch::torch_zeros(1L, device = dev)
      for (par in net$parameters) {
        l2 <- l2 + torch::torch_sum(par * par)
      }
      loss <- loss + lambda * l2
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
  zt <- torch::torch_tensor(Z_new, dtype = torch::torch_float())
  beta <- torch::with_no_grad({
    net$get_beta(zt)
  })
  as.matrix(torch::as_array(beta))
}
