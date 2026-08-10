## Integrated-likelihood mixed-logit estimator (feat/mixed-logit).
##
## `scmix()` fits the rebuilt structural model
##
##   beta_i = mu(Z_i) + A u_i,   u_i ~ N(0, I_q),   q << p,
##   Pr(Y_it = 1 | deltaX_it, beta_i) = G(deltaX_it' beta_i),
##
## by maximizing the respondent-sequence *marginal* likelihood: each
## respondent's T_i choices are integrated over u_i jointly, so the
## DNN mean mu(Z) targets E[beta_i | Z_i] directly rather than the
## conditional-logit projection.  This removes the projection/attenuation
## wedge of the two-stage estimator by construction.
##
## The likelihood integral uses Gauss-Hermite quadrature on a tensor
## grid (exact-in-practice for q = 1, 2; for q = 3 the grid is thinned
## by dropping negligible-weight nodes).  Everything runs full-batch in
## torch, so the marginal likelihood is differentiable end to end and
## the mean network and the loading matrix A are trained jointly.
##
## Cross-fitting mirrors `scfit()`: respondents are split into K folds,
## the model is trained on the (K-1)/K held-in respondents, and each
## respondent's nuisances (mu_hat(Z_i) and the training fold's A) come
## from the fold that did NOT see them.  Downstream inference
## (R/mixed-inference.R) consumes exactly these out-of-fold nuisances.

#' Gauss-Hermite grid for N(0, I_q)
#'
#' Probabilists' Gauss-Hermite nodes and weights, tensorized to q
#' dimensions and normalized to sum to one.  Nodes with joint weight
#' below `prune` are dropped (only relevant for q = 3, where the full
#' tensor grid is large).
#'
#' @param q Integer, dimension of the residual factor u.
#' @param n_nodes Integer, univariate nodes per dimension.
#' @param prune Numeric, drop tensor nodes with weight < `prune`.
#' @return A list with `U` (G x q matrix of nodes) and `w` (length-G
#'   normalized weights).
#' @keywords internal
#' @noRd
.sc_gh_grid <- function(q, n_nodes = 15L, prune = 1e-10) {
  if (!is.numeric(q) || length(q) != 1L || q < 1L || q > 3L) {
    stop(".sc_gh_grid(): `q` must be 1, 2, or 3.")
  }
  ## Probabilists' Hermite nodes/weights via the Golub-Welsch tridiagonal
  ## eigen decomposition (no extra dependency; matches
  ## np.polynomial.hermite_e.hermegauss).
  n <- as.integer(n_nodes)
  if (n < 3L) stop(".sc_gh_grid(): `n_nodes` must be >= 3.")
  off <- sqrt(seq_len(n - 1L))
  J <- matrix(0, n, n)
  J[cbind(seq_len(n - 1L), 2L:n)] <- off
  J[cbind(2L:n, seq_len(n - 1L))] <- off
  eig <- eigen(J, symmetric = TRUE)
  x <- eig$values
  w <- eig$vectors[1L, ]^2          # weights sum to 1 for the probabilists' kernel
  ord <- order(x)
  x <- x[ord]; w <- w[ord]

  if (q == 1L) {
    U <- matrix(x, ncol = 1L)
    wq <- w
  } else {
    idx <- as.matrix(do.call(expand.grid, rep(list(seq_len(n)), q)))
    U <- matrix(x[idx], ncol = q)
    wq <- apply(matrix(w[idx], ncol = q), 1L, prod)
  }
  keep <- wq >= prune
  U <- U[keep, , drop = FALSE]
  wq <- wq[keep] / sum(wq[keep])
  list(U = U, w = wq)
}

#' Build the mixed-logit conjoint network
#'
#' Same Z -> hidden -> linear(p) trunk as `.sc_build_network()` (so the
#' mean architecture is directly comparable to the two-stage DNN), plus
#' a `p x q` loading parameter `A` for the residual factor.  `A` is
#' initialized at small random values; no rotation constraint is
#' imposed during training, and reported residual quantities use the
#' rotation-invariant `A A'` (see the memo's normalization note).
#'
#' @keywords internal
#' @noRd
.sc_build_mixed_network <- function(p, p_Z, q, hidden, a_init_sd = 0.05) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_build_mixed_network(): the 'torch' package is required.")
  }
  hidden <- as.integer(hidden)
  p <- as.integer(p); p_Z <- as.integer(p_Z); q <- as.integer(q)

  generator <- torch::nn_module(
    "MixedConjointDNN",
    initialize = function() {
      self$p_beta <- p
      self$q <- q
      layers <- list()
      in_dim <- p_Z
      for (i in seq_along(hidden)) {
        layers[[paste0("hidden_", i)]] <- torch::nn_linear(in_dim, hidden[i])
        in_dim <- hidden[i]
      }
      self$hidden <- torch::nn_module_list(layers)
      self$param_layer <- torch::nn_linear(in_dim, p)
      self$A <- torch::nn_parameter(torch::torch_randn(p, q) * a_init_sd)
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

#' Marginal (integrated) negative log-likelihood, respondent-weighted
#'
#' Computes -mean_i log L_i where
#' L_i = sum_g w_g prod_t G(idx_itg)^y (1 - G)^{1-y},
#' idx_itg = deltaX_it' mu(Z_i) + deltaX_it' A u_g.
#'
#' `resp_index1` maps each task row to its respondent (1-based).  The
#' respondent aggregation uses `index_add`, and the mixture sum uses a
#' numerically stable logsumexp.  Note the loss is a mean over
#' RESPONDENTS, so the objective is respondent-weighted by construction
#' (unlike the two-stage task-weighted BCE loss).
#'
#' @keywords internal
#' @noRd
.sc_mixed_nll <- function(net, dx, zt, yt, U_t, logw_t, resp_index1, N) {
  mu <- net$get_beta(zt)                                   # n x p (task rows)
  base <- torch::torch_sum(dx * mu, dim = 2L)              # n
  fac <- torch::torch_mm(dx, net$A)                        # n x q
  idx <- base$unsqueeze(2L) + torch::torch_mm(fac, U_t$t())  # n x G
  ## log G(idx)^y (1-G)^{1-y} = -softplus(-idx)*y - softplus(idx)*(1-y)
  lp <- -torch::nnf_softplus(-idx) * yt$unsqueeze(2L) -
    torch::nnf_softplus(idx) * (1 - yt)$unsqueeze(2L)      # n x G
  agg <- torch::torch_zeros(N, lp$shape[2], dtype = lp$dtype)
  agg <- agg$index_add(1L, resp_index1, lp)                # N x G, sum over tasks
  ll_i <- torch::torch_logsumexp(agg + logw_t$unsqueeze(1L), dim = 2L)  # N
  -torch::torch_mean(ll_i)
}

#' Train one mixed-logit network on a data subset
#'
#' Full-batch Adam on the integrated likelihood.  Seed discipline
#' matches `.sc_train_one()`: R and torch RNG states are preserved.
#'
#' @return A list with `net`, `loss_trace`, `final_loss`, and `A`
#'   (the trained loading matrix as a plain p x q matrix).
#' @keywords internal
#' @noRd
.sc_train_mixed_one <- function(deltaX, y, Z, respondent_id, gh,
                                hidden,
                                n_epochs = 400L,
                                learning_rate = 0.01,
                                weight_decay = 1e-4,
                                seed = NULL,
                                device = "cpu",
                                verbose = FALSE,
                                warm_state = NULL,
                                early_stop = TRUE,
                                val_frac = 0.1,
                                check_every = 20L,
                                patience = 3L) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_train_mixed_one(): the 'torch' package is required.")
  }
  if (!is.null(seed)) {
    withr::local_preserve_seed()
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }
  ## Early stopping on a held-out respondent slice.  Without it, at
  ## small T with rich (nearly respondent-unique) Z the mean network
  ## can absorb the residual heterogeneity respondent by respondent:
  ## the training likelihood keeps improving, A collapses to zero, and
  ## the out-of-fold mu_hat overdisperses badly (observed on the T = 3
  ## candidate application).  Validation NLL on held-out respondents is
  ## the model-consistent stopping signal.
  resp_all <- unique(respondent_id)
  if (isTRUE(early_stop) && length(resp_all) >= 50L) {
    n_val <- max(10L, floor(val_frac * length(resp_all)))
    val_resp <- sample(resp_all, n_val)
    is_val <- respondent_id %in% val_resp
  } else {
    is_val <- rep(FALSE, length(respondent_id))
    early_stop <- FALSE
  }

  mk_tensors <- function(rows) {
    rf <- factor(respondent_id[rows], levels = unique(respondent_id[rows]))
    list(
      dx = torch::torch_tensor(deltaX[rows, , drop = FALSE],
                               dtype = torch::torch_float(), device = dev),
      zt = torch::torch_tensor(Z[rows, , drop = FALSE],
                               dtype = torch::torch_float(), device = dev),
      yt = torch::torch_tensor(as.numeric(y[rows]),
                               dtype = torch::torch_float(), device = dev),
      idx1 = torch::torch_tensor(as.integer(rf), dtype = torch::torch_long(),
                                 device = dev),
      N = nlevels(rf)
    )
  }

  dev <- torch::torch_device(device)
  tr <- mk_tensors(!is_val)
  va <- if (early_stop) mk_tensors(is_val) else NULL
  dx <- tr$dx; zt <- tr$zt; yt <- tr$yt
  U_t <- torch::torch_tensor(gh$U, dtype = torch::torch_float(), device = dev)
  logw_t <- torch::torch_tensor(log(gh$w), dtype = torch::torch_float(), device = dev)
  resp_index1 <- tr$idx1
  N <- tr$N

  q <- ncol(gh$U)
  net <- .sc_build_mixed_network(p = ncol(deltaX), p_Z = ncol(Z), q = q,
                                 hidden = hidden)
  if (!is.null(warm_state)) {
    ## Warm start the mean trunk from a same-architecture state dict
    ## (e.g. a trained two-stage ConjointDNN); `A` keeps its fresh init.
    ok <- tryCatch({
      own <- net$state_dict()
      for (nm in names(warm_state)) {
        if (nm %in% names(own) && !identical(nm, "A")) own[[nm]] <- warm_state[[nm]]
      }
      net$load_state_dict(own)
      TRUE
    }, error = function(e) FALSE)
    if (!ok && isTRUE(verbose)) {
      message(".sc_train_mixed_one(): warm start skipped (state mismatch).")
    }
  }
  net$to(device = dev)

  ## Two parameter groups: the mean trunk gets the usual L2 (it is the
  ## regularized nonparametric nuisance); the loading matrix A is a
  ## finite-dimensional structural parameter and is exempt --- L2 on A
  ## would shrink the residual variance toward zero and re-introduce
  ## exactly the attenuation the integrated likelihood removes.
  par_names <- names(net$parameters)

  ## Two-phase training.  Phase 1 trains the mean trunk with A frozen,
  ## early-stopped on held-out-respondent NLL -- this is what prevents
  ## the trunk from absorbing residual heterogeneity respondent by
  ## respondent when Z is rich and T small.  Phase 2 unfreezes A and
  ## continues jointly with fresh early stopping, so the loading matrix
  ## gets its own validated growth window instead of being cut short by
  ## phase 1's stopping point (A starts near zero and trains slowly;
  ## a single shared stopping rule systematically under-trains it).
  run_phase <- function(train_A, max_epochs) {
    net$A$requires_grad_(train_A)
    trunk <- net$parameters[par_names != "A"]
    groups <- list(list(params = trunk, weight_decay = weight_decay))
    if (train_A) {
      groups[[2L]] <- list(params = net$parameters[par_names == "A"],
                           weight_decay = 0)
    }
    optimizer <- torch::optim_adam(groups, lr = learning_rate)
    loss_trace <- numeric(max_epochs)
    val_trace <- c()
    best_val <- Inf
    best_state <- NULL
    bad_checks <- 0L
    stopped_at <- max_epochs
    for (epoch in seq_len(max_epochs)) {
      net$train()
      optimizer$zero_grad()
      loss <- .sc_mixed_nll(net, dx, zt, yt, U_t, logw_t, resp_index1, N)
      loss$backward()
      optimizer$step()
      loss_trace[epoch] <- as.numeric(loss$item())
      if (early_stop && (epoch %% check_every == 0L)) {
        net$eval()
        vloss <- as.numeric(torch::with_no_grad(
          .sc_mixed_nll(net, va$dx, va$zt, va$yt, U_t, logw_t, va$idx1, va$N)
        )$item())
        val_trace <- c(val_trace, vloss)
        if (vloss < best_val - 1e-5) {
          best_val <- vloss
          best_state <- lapply(net$state_dict(), function(t) t$clone())
          bad_checks <- 0L
        } else {
          bad_checks <- bad_checks + 1L
          if (bad_checks >= patience) { stopped_at <- epoch; break }
        }
      }
      if (verbose && (epoch %% 100L == 0L || epoch == 1L)) {
        message(sprintf("  epoch %4d  nll = %.6f (train_A=%s)",
                        epoch, loss_trace[epoch], train_A))
      }
    }
    if (early_stop && !is.null(best_state)) net$load_state_dict(best_state)
    list(loss = loss_trace[seq_len(stopped_at)], val = val_trace,
         stopped_at = stopped_at)
  }

  ph1 <- run_phase(train_A = FALSE, max_epochs = n_epochs)
  ph2 <- run_phase(train_A = TRUE, max_epochs = n_epochs)

  net$eval()
  A_hat <- as.matrix(torch::as_array(net$A))
  loss_all <- c(ph1$loss, ph2$loss)
  list(net = net, loss_trace = loss_all,
       val_trace = c(ph1$val, ph2$val),
       stopped_at = c(ph1$stopped_at, ph2$stopped_at),
       final_loss = loss_all[length(loss_all)], A = A_hat)
}

#' Integrated-likelihood mixed-logit conjoint estimator
#'
#' Fits the rebuilt structural model in which respondent preferences
#' are `beta_i = mu(Z_i) + A u_i` with `u_i ~ N(0, I_q)` and the
#' respondent's whole choice sequence enters one marginal likelihood.
#' Unlike [scfit()], whose first stage targets the conditional-logit
#' projection (attenuated toward zero whenever residual heterogeneity
#' is present), `scmix()` targets `E[beta_i | Z_i]` directly: the
#' mixture is inside the objective, so no mean-logit bridge assumption
#' is needed.
#'
#' The interface deliberately mirrors [scfit()] (same formula, same
#' long-format data contract).  Cross-fitting is respondent-clustered;
#' each respondent's stored `mu_hat` and loading matrix come from the
#' fold model that did not train on them, which is what the
#' orthogonal-score inference in [scmix_theta()] and friends requires.
#'
#' @inheritParams scfit
#' @param q Integer, dimension of the residual factor (1, 2, or 3).
#' @param n_nodes Integer, univariate Gauss-Hermite nodes per factor
#'   dimension (default 15; the q-dim grid is the tensor product).
#' @param K Integer, respondent-clustered folds (default 5).
#' @param n_epochs Adam epochs per fold (default 400; the marginal
#'   likelihood typically converges faster per epoch than the
#'   task-level BCE because each respondent contributes one pooled
#'   term).
#' @param init Optional [scfit] object used to warm start each fold's
#'   mean trunk (architecture must match `hidden`).
#' @return An object of class `scmix`; see [scmix_theta()],
#'   [scmix_polarization()], [scmix_counterfactual()] for inference.
#' @export
scmix <- function(formula, data,
                  respondent = "resp_id", task = "task_id", profile = "profile_id",
                  q = 1L,
                  n_nodes = 15L,
                  K = 5L,
                  hidden = "auto",
                  n_epochs = 400L,
                  learning_rate = 0.01,
                  weight_decay = "adaptive",
                  seed = NULL,
                  init = NULL,
                  device = "cpu",
                  verbose = FALSE,
                  early_stop = TRUE) {
  call <- match.call()
  respondent <- .sc_coerce_colname(respondent, "respondent")
  task <- .sc_coerce_colname(task, "task")
  profile <- .sc_coerce_colname(profile, "profile")

  parsed <- .sc_parse_formula(formula)
  data <- .sc_to_long(data, respondent, task, profile)
  enc <- .sc_encode(data, parsed$attr_vars, parsed$z_vars)
  if (ncol(enc$Z) == 0L) {
    stop("scmix(): at least one respondent moderator is required after `|`.")
  }
  built <- .sc_build_deltax(enc$X, enc$Z, data[[task]], data[[profile]],
                            data[[respondent]])
  deltaX <- built$deltaX
  Z_task <- built$Z_task
  resp_task <- built$respondent_task

  ## Response: first-profile choice indicator per task, matching scfit's
  ## convention (rows are (respondent, task, profile)-sorted; the first
  ## profile row of each task carries y = 1 iff profile 1 was chosen).
  y_long <- data[[parsed$response]]
  ord <- order(paste(data[[respondent]], data[[task]], sep = "\r"),
               data[[profile]])
  y_first <- y_long[ord][seq(1L, length(y_long), by = 2L)]
  if (!all(y_first %in% c(0, 1))) {
    stop("scmix(): the response must be a 0/1 choice indicator on profile rows.")
  }
  y <- as.numeric(y_first)

  n <- nrow(deltaX)
  hidden_use <- if (identical(hidden, "auto")) {
    .sc_auto_hidden(n, ncol(deltaX))
  } else as.integer(hidden)
  wd_use <- .sc_resolve_weight_decay(weight_decay, n, ncol(deltaX))
  gh <- .sc_gh_grid(q = as.integer(q), n_nodes = as.integer(n_nodes))

  ## Internal contrast standardization.  Continuous attributes (e.g. tax
  ## rates in percentage points) put deltaX entries at O(10-50); the
  ## loading initialization and the quadrature nodes then start the
  ## mixture wildly over-dispersed and training fails.  Standardizing
  ## each contrast column for TRAINING ONLY and rescaling (mu, A) back
  ## to raw units on output leaves the index deltaX' mu exactly
  ## invariant, so every downstream score works on the raw scale.
  sd_dx <- apply(deltaX, 2L, stats::sd)
  sd_dx[!is.finite(sd_dx) | sd_dx < 1e-12] <- 1
  deltaX_std <- sweep(deltaX, 2L, sd_dx, `/`)

  fold_id <- .sc_make_folds(resp_task, K = K, seed = seed)
  warm_state <- NULL
  if (!is.null(init)) {
    if (!inherits(init, "sc_fit") || is.null(init$nets) || length(init$nets) == 0L) {
      warning("scmix(): `init` has no stored nets; warm start skipped.")
    } else {
      warm_state <- init$nets[[1L]]$state_dict()
    }
  }

  N_all <- length(unique(resp_task))
  mu_hat <- matrix(NA_real_, nrow = n, ncol = ncol(deltaX))
  A_folds <- vector("list", K)
  loss_traces <- vector("list", K)
  nets <- vector("list", K)

  for (k in seq_len(K)) {
    in_k <- fold_id != k
    fit_k <- .sc_train_mixed_one(
      deltaX = deltaX_std[in_k, , drop = FALSE],
      y = y[in_k],
      Z = Z_task[in_k, , drop = FALSE],
      respondent_id = resp_task[in_k],
      gh = gh, hidden = hidden_use,
      n_epochs = n_epochs, learning_rate = learning_rate,
      weight_decay = wd_use,
      seed = if (is.null(seed)) NULL else .sc_fold_seed(seed, k),
      device = device, verbose = verbose,
      warm_state = warm_state,
      early_stop = early_stop
    )
    out_k <- fold_id == k
    ## rescale from the standardized training scale back to raw units:
    ## mu_raw_k = mu_std_k / sd_k, A_raw[k, ] = A_std[k, ] / sd_k
    mu_hat[out_k, ] <- sweep(
      .sc_predict_beta(fit_k$net, Z_task[out_k, , drop = FALSE]),
      2L, sd_dx, `/`)
    A_folds[[k]] <- fit_k$A / sd_dx
    loss_traces[[k]] <- fit_k$loss_trace
    nets[[k]] <- fit_k$net
    if (verbose) {
      message(sprintf("scmix fold %d/%d done (nll = %.5f)", k, K, fit_k$final_loss))
    }
  }

  fit <- list(
    mu_hat = mu_hat,               # task rows; constant within respondent
    A_folds = A_folds,
    q = as.integer(q),
    gh = gh,
    deltaX = deltaX,
    y = y,
    Z = Z_task,
    respondent_id = resp_task,
    fold_id = fold_id,
    K = as.integer(K),
    N = N_all,
    attr_names = enc$x_names,
    attr_vars = parsed$attr_vars,
    factor_levels = enc$factor_levels,
    attr_map = enc$attr_map,
    z_names = enc$z_names,
    hidden = hidden_use,
    n_epochs = as.integer(n_epochs),
    weight_decay_used = wd_use,
    seed = seed,
    loss_traces = loss_traces,
    nets = nets,
    call = call
  )
  class(fit) <- c("scmix", "list")
  fit
}

#' @export
print.scmix <- function(x, ...) {
  cat("Integrated-likelihood mixed-logit conjoint fit (scmix)\n")
  cat(sprintf("  respondents: %d   tasks: %d   attributes (dummies): %d\n",
              x$N, nrow(x$deltaX), ncol(x$deltaX)))
  cat(sprintf("  residual factor dimension q = %d, %d quadrature nodes, K = %d folds\n",
              x$q, length(x$gh$w), x$K))
  Sig <- Reduce(`+`, lapply(x$A_folds, tcrossprod)) / length(x$A_folds)
  cat("  residual SDs (sqrt diag of fold-averaged AA'):\n")
  sds <- sqrt(pmax(diag(Sig), 0))
  names(sds) <- x$attr_names
  print(round(sds, 3))
  invisible(x)
}
