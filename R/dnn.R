## DNN architecture for sconjoint (M2).
##
## Ports the `conjoint_dnn` nn_module from the prototype
## (`03_structural_dnn.R` lines 14-60).  The architecture is
## Z(p_Z) -> [hidden_1 -> ReLU -> ... -> hidden_L -> ReLU] -> linear(p_beta)
## with no activation on the parameter-head (the index is taken to the
## BCE-with-logits loss downstream).
##
## All functions in this file are internal.

#' Choose a default hidden-layer configuration from N * T
#'
#' Implements the memo-06 "three-tier" rule documented in spec.md
#' section 20:
#'
#' * N*T <  2000: c(32L, 16L)
#' * N*T < 10000: c(32L, 32L, 16L)
#' * N*T >=10000: c(64L, 64L, 32L)
#'
#' @param n_obs Number of (respondent x task) observations on which
#'   the network will be trained, i.e. the number of rows of
#'   `delta_x` passed to the trainer.
#' @return An integer vector giving the hidden-layer widths.
#' @keywords internal
#' @noRd
.sc_auto_hidden <- function(n_obs) {
  if (!is.numeric(n_obs) || length(n_obs) != 1L || !is.finite(n_obs) || n_obs < 1) {
    stop(".sc_auto_hidden(): `n_obs` must be a single positive finite number.")
  }
  if (n_obs < 2000) {
    return(c(32L, 16L))
  }
  if (n_obs < 10000) {
    return(c(32L, 32L, 16L))
  }
  c(64L, 64L, 32L)
}

#' Build the conjoint structural DNN `nn_module`
#'
#' Mirrors the prototype `conjoint_dnn` (see
#' `03_structural_dnn.R` lines 14-60) exactly: ReLU hidden layers,
#' a linear parameter head with bias, and a `get_beta()` method
#' that extracts beta(Z) without computing the index.  `forward()`
#' returns the logit index `sum(delta_x * beta(Z), dim = 2)`.
#'
#' @param p_z Integer, number of respondent-moderator columns.
#' @param p_beta Integer, number of attribute dummies.
#' @param hidden Integer vector of hidden-layer widths.
#' @return An `nn_module` generator that takes `p_z`, `p_beta`, `hidden`.
#' @keywords internal
#' @noRd
.sc_build_network <- function(p, p_Z, hidden = c(64L, 64L, 32L)) {
  ## p is p_beta in the prototype's naming; we keep the more descriptive
  ## argument name `p` per the dispatch prompt but forward it to the
  ## module generator below as `p_beta`.
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_build_network(): the 'torch' package is required.")
  }
  if (!is.numeric(p) || length(p) != 1L || p < 1) {
    stop(".sc_build_network(): `p` must be a positive integer.")
  }
  if (!is.numeric(p_Z) || length(p_Z) != 1L || p_Z < 1) {
    stop(".sc_build_network(): `p_Z` must be a positive integer.")
  }
  if (!is.numeric(hidden) || length(hidden) < 1L || any(hidden < 1)) {
    stop(".sc_build_network(): `hidden` must be a positive integer vector.")
  }

  hidden <- as.integer(hidden)
  p      <- as.integer(p)
  p_Z    <- as.integer(p_Z)

  generator <- torch::nn_module(
    "ConjointDNN",
    initialize = function() {
      self$p_z    <- p_Z
      self$p_beta <- p

      layers <- list()
      in_dim <- p_Z
      for (i in seq_along(hidden)) {
        layers[[paste0("hidden_", i)]] <- torch::nn_linear(in_dim, hidden[i])
        in_dim <- hidden[i]
      }
      self$hidden <- torch::nn_module_list(layers)
      self$param_layer <- torch::nn_linear(in_dim, p)
    },
    forward = function(delta_x, z) {
      h <- z
      for (i in seq_along(self$hidden)) {
        h <- torch::nnf_relu(self$hidden[[i]](h))
      }
      beta_z <- self$param_layer(h)
      torch::torch_sum(delta_x * beta_z, dim = 2L)
    },
    get_beta = function(z) {
      h <- z
      for (i in seq_along(self$hidden)) {
        h <- torch::nnf_relu(self$hidden[[i]](h))
      }
      self$param_layer(h)
    }
  )

  generator()
}

#' Forward pass returning predicted choice probabilities
#'
#' Convenience wrapper that calls the module's `forward()` method
#' (which returns the logit index) and applies a sigmoid to return
#' choice probabilities on the [0, 1] scale.
#'
#' @param net A network produced by `.sc_build_network()`.
#' @param Z,deltaX torch tensors of type float32.
#' @return A torch tensor of shape `[batch]` with probabilities.
#' @keywords internal
#' @noRd
.sc_forward <- function(net, Z, deltaX) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_forward(): the 'torch' package is required.")
  }
  logit <- net$forward(deltaX, Z)
  torch::nnf_sigmoid(logit)
}
