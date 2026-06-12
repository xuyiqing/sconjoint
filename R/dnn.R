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
#' Implements the paper's v13 base architecture rule
#' (`code/04_training.R`, memo 42):
#'
#' * N*T < 2000: c(32L, 16L)  (small-fixture safety; paper does not
#'   use this regime directly)
#' * N*T >= 2000: c(32L, 32L, 16L)  (paper v13 base; used for all
#'   three showcase apps SW / GS / BR regardless of NT)
#' * Override at p >= 40 AND N*T >= 80,000: c(128L, 64L, 64L)
#'   (paper v13 large-design override; none of the showcase apps
#'   trigger this)
#'
#' This *changed* in the v13 alignment (memo 42): the earlier rule
#' scaled up to c(64L, 64L, 32L) at NT >= 10,000.  The paper uses
#' c(32L, 32L, 16L) for all three apps including BR (NT=16000) and
#' GS (NT=20657), so the auto-scaling was diverging from paper for
#' medium-to-large conjoint designs.
#'
#' @param n_obs Number of (respondent x task) observations on which
#'   the network will be trained.
#' @param p_beta Number of attribute dummies (number of `deltaX`
#'   columns).  Used only by the v13 large-design override.  Default
#'   `NULL` skips the override check.
#' @return An integer vector giving the hidden-layer widths.
#' @keywords internal
#' @noRd
.sc_auto_hidden <- function(n_obs, p_beta = NULL) {
  if (!is.numeric(n_obs) || length(n_obs) != 1L || !is.finite(n_obs) || n_obs < 1) {
    stop(".sc_auto_hidden(): `n_obs` must be a single positive finite number.")
  }
  if (!is.null(p_beta) &&
      is.numeric(p_beta) && length(p_beta) == 1L && is.finite(p_beta) &&
      p_beta >= 40L && n_obs >= 80000L) {
    return(c(128L, 64L, 64L))
  }
  if (n_obs < 2000) {
    return(c(32L, 16L))
  }
  c(32L, 32L, 16L)
}

#' Build the conjoint structural DNN `nn_module`
#'
#' Mirrors the prototype `conjoint_dnn` (see
#' `03_structural_dnn.R` lines 14-60) exactly: ReLU hidden layers,
#' a linear parameter head with bias, and a `get_beta()` method
#' that extracts beta(Z) without computing the index.  `forward()`
#' returns the logit index `sum(delta_x * beta(Z), dim = 2)`.
#'
#' Optional population-level attribute-interaction heads
#' (`interactions != "none"`):
#'
#' * `"lowrank"` adds a `p x r` parameter `V` and the difference-of-
#'   quadratics term `||V'X_A||^2 - ||V'X_B||^2` to the index.  The
#'   quadratic-in-the-difference form `||V' deltaX||^2` is deliberately
#'   not used -- it is invariant under an A/B profile swap and therefore
#'   structurally incoherent for forced choice (see R/interactions.R).
#' * `"explicit"` adds a length-`n_int` linear head `w_int` on
#'   precomputed identified interaction features `f_int = q_A - q_B`
#'   (cross-attribute dummy products only).
#'
#' Both heads are population-level: they do not depend on Z.  When
#' `interactions = "none"` the module is bit-identical to the historical
#' one (no extra parameters are created, so the torch RNG stream is
#' untouched).
#'
#' @param p_z Integer, number of respondent-moderator columns.
#' @param p_beta Integer, number of attribute dummies.
#' @param hidden Integer vector of hidden-layer widths.
#' @param interactions One of `"none"`, `"lowrank"`, `"explicit"`.
#' @param interaction_rank Integer rank `r` of the low-rank head.
#' @param n_int_features Integer, number of identified interaction
#'   features (required by the `"explicit"` head).
#' @return An `nn_module` generator that takes `p_z`, `p_beta`, `hidden`.
#' @keywords internal
#' @noRd
.sc_build_network <- function(p, p_Z, hidden = c(64L, 64L, 32L),
                              interactions = "none",
                              interaction_rank = 2L,
                              n_int_features = 0L) {
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

  interactions <- match.arg(interactions, c("none", "lowrank", "explicit"))
  interaction_rank <- as.integer(interaction_rank)
  n_int_features   <- as.integer(n_int_features)
  if (identical(interactions, "lowrank") && interaction_rank < 1L) {
    stop(".sc_build_network(): `interaction_rank` must be >= 1 for ",
         "interactions = \"lowrank\".")
  }
  if (identical(interactions, "explicit") && n_int_features < 1L) {
    stop(".sc_build_network(): `n_int_features` must be >= 1 for ",
         "interactions = \"explicit\".")
  }

  generator <- torch::nn_module(
    "ConjointDNN",
    initialize = function() {
      self$p_z    <- p_Z
      self$p_beta <- p
      self$int_type <- interactions

      layers <- list()
      in_dim <- p_Z
      for (i in seq_along(hidden)) {
        layers[[paste0("hidden_", i)]] <- torch::nn_linear(in_dim, hidden[i])
        in_dim <- hidden[i]
      }
      self$hidden <- torch::nn_module_list(layers)
      self$param_layer <- torch::nn_linear(in_dim, p)
      ## Interaction heads come AFTER the main network so that the main
      ## network's parameter initialization consumes the identical RNG
      ## stream regardless of `interactions`.  `torch_zeros` consumes no
      ## RNG at all.
      if (identical(interactions, "lowrank")) {
        self$V <- torch::nn_parameter(
          torch::torch_randn(p, interaction_rank) * 0.05
        )
      } else if (identical(interactions, "explicit")) {
        self$w_int <- torch::nn_parameter(torch::torch_zeros(n_int_features))
      }
    },
    forward = function(delta_x, z, x_a = NULL, x_b = NULL, f_int = NULL) {
      h <- z
      for (i in seq_along(self$hidden)) {
        h <- torch::nnf_relu(self$hidden[[i]](h))
      }
      beta_z <- self$param_layer(h)
      idx <- torch::torch_sum(delta_x * beta_z, dim = 2L)
      if (identical(self$int_type, "lowrank")) {
        ## Difference of quadratics (profile-swap antisymmetric); NOT
        ## ||V' deltaX||^2, which is swap-invariant and incoherent.
        qa <- torch::torch_sum(torch::torch_mm(x_a, self$V)^2, dim = 2L)
        qb <- torch::torch_sum(torch::torch_mm(x_b, self$V)^2, dim = 2L)
        idx <- idx + qa - qb
      } else if (identical(self$int_type, "explicit")) {
        idx <- idx + torch::torch_mv(f_int, self$w_int)
      }
      idx
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
#' choice probabilities on the \eqn{[0, 1]} scale.
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
