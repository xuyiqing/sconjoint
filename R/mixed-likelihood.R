## Integrated-likelihood mixed-logit estimator (feat/mixed-logit).
##
## `scmix()` fits the rebuilt structural model
##
##   beta_i = mu(Z_i) + A u_i,   u_i ~ N(0, I_q),   q << p,
##   Pr(Y_it = 1 | deltaX_it, beta_i)
##     = G(kappa + deltaX_it' beta_i),
##
## by maximizing the respondent-sequence *marginal* likelihood: each
## respondent's T_i choices are integrated over u_i jointly, so the
## DNN sieve represents the structural conditional mean E[beta_i | Z_i]
## inside the maintained normal mixed-logit model.
##
## The likelihood integral is exact when q = 0, uses product
## Gauss--Hermite quadrature for small q, and optionally uses randomized
## quasi--Monte Carlo for larger q. Everything runs full-batch in torch,
## so the marginal likelihood is differentiable end to end and the
## position effect, mean network, and loading matrix A are trained jointly.
##
## Cross-fitting mirrors `scfit()`: respondents are split into K folds,
## the model is trained on the (K-1)/K held-in respondents, and each
## respondent's nuisances (mu_hat(Z_i), the training fold's A, and its
## position effect) come from the fold that did NOT see them. A separate
## full-sample fit estimates the plug-in structural object. Downstream
## paper-aligned inference (R/paperps-inference.R) consumes the out-of-fold
## nuisances. R/mixed-inference.R contains legacy exploratory corrections.

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
  if (!is.numeric(q) || length(q) != 1L || is.na(q) ||
      q < 0L || q != as.integer(q) || q > 3L) {
    stop(".sc_gh_grid(): `q` must be 0, 1, 2, or 3.")
  }
  q <- as.integer(q)
  if (q == 0L) {
    return(list(
      U = matrix(numeric(0), nrow = 1L, ncol = 0L),
      w = 1,
      metadata = list(method = "exact", q = 0L, n_points = 1L,
                      deterministic = TRUE, antithetic = FALSE)
    ))
  }
  ## Probabilists' Hermite nodes/weights via the Golub-Welsch tridiagonal
  ## eigen decomposition (no extra dependency; matches
  ## np.polynomial.hermite_e.hermegauss).
  if (!is.numeric(n_nodes) || length(n_nodes) != 1L || is.na(n_nodes) ||
      n_nodes < 3L || n_nodes != as.integer(n_nodes)) {
    stop(".sc_gh_grid(): `n_nodes` must be an integer >= 3.")
  }
  n <- as.integer(n_nodes)
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
  ## prune only the q = 3 tensor grid; at q <= 2 keep every node so the
  ## deployed grids retain full Gauss accuracy
  if (q == 3L) {
    keep <- wq >= prune
    U <- U[keep, , drop = FALSE]
    wq <- wq[keep] / sum(wq[keep])
  }
  list(
    U = U,
    w = wq,
    metadata = list(method = "gauss-hermite", q = q,
                    nodes_per_dimension = n,
                    n_points = nrow(U), prune = if (q == 3L) prune else 0,
                    deterministic = TRUE, antithetic = FALSE)
  )
}

#' Randomized quasi--Monte Carlo grid for N(0, I_q)
#'
#' Uses an optional Sobol implementation and pairs transformed points
#' antithetically. The returned draw set is generated once per `scmix()`
#' call and then held fixed across respondents, folds, starts, and updates.
#'
#' @keywords internal
#' @noRd
.sc_qmc_grid <- function(q, n_draws = 4096L, seed = NULL,
                         antithetic = TRUE, scramble = TRUE) {
  if (!is.logical(antithetic) || length(antithetic) != 1L ||
      is.na(antithetic) || !is.logical(scramble) || length(scramble) != 1L ||
      is.na(scramble)) {
    stop(".sc_qmc_grid(): `antithetic` and `scramble` must be logical scalars.")
  }
  if (!is.numeric(q) || length(q) != 1L || is.na(q) ||
      q < 1L || q != as.integer(q)) {
    stop(".sc_qmc_grid(): `q` must be a positive integer.")
  }
  if (!is.numeric(n_draws) || length(n_draws) != 1L || is.na(n_draws) ||
      n_draws < 16L || n_draws != as.integer(n_draws)) {
    stop(".sc_qmc_grid(): `n_draws` must be an integer >= 16.")
  }
  if (isTRUE(antithetic) && n_draws %% 2L != 0L) {
    stop(".sc_qmc_grid(): `n_draws` must be even when `antithetic = TRUE`.")
  }
  q <- as.integer(q)
  n_draws <- as.integer(n_draws)
  withr::local_preserve_seed()
  if (!is.null(seed)) set.seed(seed)
  n_base <- if (isTRUE(antithetic)) ceiling(n_draws / 2) else n_draws
  provider <- NULL
  U01 <- NULL

  if (requireNamespace("qrng", quietly = TRUE)) {
    sobol <- getExportedValue("qrng", "sobol")
    randomize <- if (isTRUE(scramble)) "Owen" else "none"
    call_sobol <- function(randomize) {
      args <- list(n = n_base, d = q, randomize = randomize)
      if (!is.null(seed)) args$seed <- seed
      do.call(sobol, args)
    }
    U01 <- tryCatch(
      call_sobol(randomize),
      error = function(e) {
        if (!isTRUE(scramble)) stop(e)
        call_sobol("digital.shift")
      }
    )
    provider <- "qrng"
  } else if (requireNamespace("randtoolbox", quietly = TRUE)) {
    sobol <- getExportedValue("randtoolbox", "sobol")
    U01 <- sobol(n = n_base, dim = q,
                 scrambling = if (isTRUE(scramble)) 1L else 0L,
                 seed = seed, normal = FALSE)
    provider <- "randtoolbox"
  } else {
    stop("QMC integration requires the optional 'qrng' or 'randtoolbox' ",
         "package. Install one of them, or use integration = 'gh' with q <= 3.")
  }

  U01 <- matrix(as.numeric(U01), ncol = q)
  eps <- sqrt(.Machine$double.eps)
  U01 <- pmin(pmax(U01, eps), 1 - eps)
  U <- stats::qnorm(U01)
  if (isTRUE(antithetic)) U <- rbind(U, -U)
  U <- U[seq_len(n_draws), , drop = FALSE]
  list(
    U = U,
    w = rep(1 / n_draws, n_draws),
    metadata = list(method = "qmc-sobol", provider = provider, q = q,
                    n_points = n_draws, seed = seed,
                    randomized = isTRUE(scramble),
                    antithetic = isTRUE(antithetic),
                    deterministic = !isTRUE(scramble))
  )
}

#' Select the integration rule used by the mixed likelihood
#' @keywords internal
#' @noRd
.sc_mixed_grid <- function(q, integration = c("auto", "gh", "qmc"),
                           n_nodes = 31L, n_draws = 4096L, seed = NULL,
                           antithetic = TRUE, scramble = TRUE) {
  integration <- match.arg(integration)
  if (!is.logical(antithetic) || length(antithetic) != 1L ||
      is.na(antithetic) || !is.logical(scramble) || length(scramble) != 1L ||
      is.na(scramble)) {
    stop(".sc_mixed_grid(): `antithetic` and `scramble` must be logical scalars.")
  }
  if (!is.numeric(q) || length(q) != 1L || is.na(q) ||
      q < 0L || q != as.integer(q)) {
    stop(".sc_mixed_grid(): `q` must be a non-negative integer.")
  }
  q <- as.integer(q)
  if (q == 0L) return(.sc_gh_grid(0L, n_nodes = n_nodes))
  if (integration == "auto") integration <- if (q <= 3L) "gh" else "qmc"
  if (integration == "gh") {
    if (q > 3L) {
      stop("Product Gauss-Hermite integration is restricted to q <= 3; ",
           "use integration = 'qmc' for larger q.")
    }
    return(.sc_gh_grid(q, n_nodes = n_nodes))
  }
  .sc_qmc_grid(q, n_draws = n_draws, seed = seed,
               antithetic = antithetic, scramble = scramble)
}

#' Stable signature for one statistical-analysis specification
#'
#' The signature deliberately excludes fitted parameters and numerical
#' integration resolution.  It links diagnostics computed from fresh numerical
#' refits to the same data, learner/model specification, and respondent folds.
#' Two independent modular hashes of an XDR serialization are used to avoid a
#' package dependency for this internal audit identifier.
#'
#' @keywords internal
#' @noRd
.sc_analysis_signature <- function(deltaX, y, Z, respondent_id, fold_id,
                                   specification) {
  deltaX <- as.matrix(deltaX)
  Z <- as.matrix(Z)
  n <- nrow(deltaX)
  if (!is.numeric(deltaX) || !is.numeric(Z) || nrow(Z) != n ||
      length(y) != n || length(respondent_id) != n || length(fold_id) != n ||
      any(!is.finite(deltaX)) || any(!is.finite(Z)) || any(!is.finite(y)) ||
      anyNA(respondent_id) || anyNA(fold_id) || !is.list(specification)) {
    stop(".sc_analysis_signature(): malformed data, folds, or specification.")
  }
  payload <- list(
    version = 1L,
    deltaX = deltaX,
    y = as.numeric(y),
    Z = Z,
    respondent_id = as.character(respondent_id),
    fold_id = as.character(fold_id),
    specification = specification
  )
  bytes <- as.integer(serialize(payload, NULL, ascii = FALSE, version = 2L))
  modulus <- 2147483629
  roll <- function(seed, multiplier) {
    value <- as.double(seed)
    for (byte in bytes) {
      value <- (value * multiplier + byte + 1) %% modulus
    }
    as.integer(value)
  }
  paste0(
    "scmix-v1-",
    sprintf("%08x", roll(14695981, 65599)),
    sprintf("%08x", roll(21661363, 131071))
  )
}

#' Fit respondent-weighted moderator centering and scaling
#'
#' The transform is estimated from one row per respondent, rather than one
#' row per task, and therefore does not give respondents with more completed
#' tasks greater influence. Outer-fold callers must pass training respondents
#' only. Constant coordinates are centered and assigned scale one.
#'
#' @keywords internal
#' @noRd
.sc_fit_z_transform <- function(Z, respondent_id) {
  Z <- as.matrix(Z)
  if (nrow(Z) != length(respondent_id)) {
    stop(".sc_fit_z_transform(): `Z` and `respondent_id` have incompatible lengths.")
  }
  if (any(!is.finite(Z))) {
    stop(".sc_fit_z_transform(): moderator inputs must be finite.")
  }
  resp_levels <- unique(respondent_id)
  resp_index <- match(respondent_id, resp_levels)
  first_index <- match(resp_levels, respondent_id)
  reference <- Z[first_index[resp_index], , drop = FALSE]
  if (any(abs(Z - reference) > 1e-12)) {
    stop(".sc_fit_z_transform(): moderators must be constant within respondent.")
  }
  first <- !duplicated(respondent_id)
  Z_resp <- Z[first, , drop = FALSE]
  center <- colMeans(Z_resp)
  scale <- apply(Z_resp, 2L, stats::sd)
  constant <- !is.finite(scale) | scale < 1e-12
  scale[constant] <- 1
  names(center) <- names(scale) <- colnames(Z)
  list(center = center, scale = scale, constant = constant,
       n_respondents = nrow(Z_resp), weighting = "respondent")
}

#' Apply a stored moderator transform
#' @keywords internal
#' @noRd
.sc_apply_z_transform <- function(Z, transform) {
  Z <- as.matrix(Z)
  if (ncol(Z) != length(transform$center) ||
      length(transform$scale) != length(transform$center)) {
    stop(".sc_apply_z_transform(): transform has incompatible dimension.")
  }
  sweep(sweep(Z, 2L, transform$center, `-`), 2L, transform$scale, `/`)
}

#' Retrieve the moderator inputs used by an scmix network
#'
#' With `source = "crossfit"`, each requested row is transformed using the
#' training-only transform for its held-out fold. With `source = "full"`, the
#' full-sample transform is used. Older fits without stored transforms return
#' their stored `Z` unchanged.
#'
#' @keywords internal
#' @noRd
.scmix_z_for_rows <- function(fit, rows = NULL,
                              source = c("crossfit", "full")) {
  source <- match.arg(source)
  if (is.null(rows)) rows <- seq_len(nrow(fit$Z))
  if (anyNA(rows) || any(rows < 1L) || any(rows > nrow(fit$Z))) {
    stop(".scmix_z_for_rows(): `rows` contains an invalid row index.")
  }
  Z <- fit$Z[rows, , drop = FALSE]
  if (source == "full") {
    if (is.null(fit$z_transform_full)) return(Z)
    return(.sc_apply_z_transform(Z, fit$z_transform_full))
  }
  if (is.null(fit$z_transform_folds)) return(Z)
  fold <- fit$fold_id[rows]
  out <- matrix(NA_real_, nrow = nrow(Z), ncol = ncol(Z),
                dimnames = dimnames(Z))
  for (k in unique(fold)) {
    here <- fold == k
    out[here, ] <- .sc_apply_z_transform(
      Z[here, , drop = FALSE], fit$z_transform_folds[[k]])
  }
  out
}

#' Validate the artificial compact parameter bounds
#' @keywords internal
#' @noRd
.sc_mixed_validate_compact_bounds <- function(p, coefficient_scale,
                                              a_bound, weight_bound) {
  if (!is.numeric(a_bound) || length(a_bound) != 1L ||
      !is.finite(a_bound) || a_bound <= 0) {
    stop("`a_bound` must be one finite strictly positive raw-unit Frobenius bound.")
  }
  if (!is.numeric(weight_bound) || length(weight_bound) != 1L ||
      !is.finite(weight_bound) || weight_bound <= 0) {
    stop("`weight_bound` must be one finite strictly positive coordinate bound.")
  }
  if (is.null(coefficient_scale)) coefficient_scale <- rep(1, p)
  coefficient_scale <- as.numeric(coefficient_scale)
  if (length(coefficient_scale) != p || any(!is.finite(coefficient_scale)) ||
      any(coefficient_scale <= 0)) {
    stop("`coefficient_scale` must contain one finite positive scale per coefficient.")
  }
  list(a_bound = as.numeric(a_bound),
       weight_bound = as.numeric(weight_bound),
       coefficient_scale = coefficient_scale)
}

#' Project computational parameters onto the artificial compact sets
#'
#' `A` is optimized on standardized-contrast coordinates.  Dividing its rows
#' by `coefficient_scale` recovers raw coefficient units, so the loading
#' projection is with respect to that raw-unit Frobenius norm.  Every parameter
#' in the mean-network sieve (weights and biases) is clipped coordinatewise.
#' The structural position parameter is excluded because its reported value is
#' already smoothly bounded by `kappa_bound`.
#'
#' @keywords internal
#' @noRd
.sc_mixed_project_parameters <- function(net, coefficient_scale,
                                         a_bound, weight_bound) {
  compact <- .sc_mixed_validate_compact_bounds(
    p = net$p_beta, coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound)
  sieve_names <- setdiff(names(net$parameters), c("A", "kappa_raw"))
  torch::with_no_grad({
    for (nm in sieve_names) {
      net$parameters[[nm]]$clamp_(min = -compact$weight_bound,
                                  max = compact$weight_bound)
    }
    if (net$q > 0L) {
      scale_t <- torch::torch_tensor(
        compact$coefficient_scale,
        dtype = net$A$dtype, device = net$A$device
      )$unsqueeze(2L)
      raw_norm <- torch::torch_sqrt(torch::torch_sum((net$A / scale_t)^2))
      shrink <- torch::torch_clamp(compact$a_bound /
                                     torch::torch_clamp(raw_norm, min = 1e-30),
                                   max = 1)
      net$A$mul_(shrink)
    }
  })
  invisible(net)
}

#' Compact-bound diagnostics at an attained state
#' @keywords internal
#' @noRd
.sc_mixed_bound_diagnostics <- function(net, mu, kappa, coefficient_scale,
                                        mu_bound, kappa_bound,
                                        a_bound, weight_bound,
                                        activity_fraction = 0.99) {
  compact <- .sc_mixed_validate_compact_bounds(
    p = net$p_beta, coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound)
  sieve_names <- setdiff(names(net$parameters), c("A", "kappa_raw"))
  weight_max <- if (length(sieve_names)) {
    max(vapply(sieve_names, function(nm) {
      as.numeric(net$parameters[[nm]]$detach()$abs()$max()$item())
    }, numeric(1L)))
  } else 0
  a_raw_norm <- if (net$q > 0L) {
    A_internal <- as.matrix(torch::as_array(net$A$detach()$cpu()))
    sqrt(sum(sweep(A_internal, 1L, compact$coefficient_scale, `/`)^2))
  } else 0
  mu_max <- if (length(mu)) max(abs(mu)) else 0
  list(
    mu = mu_bound,
    kappa = kappa_bound,
    a = compact$a_bound,
    weight = compact$weight_bound,
    a_units = "raw-coefficient Frobenius norm",
    weight_units = "coordinatewise network parameter",
    mu_max = mu_max,
    kappa_abs = abs(kappa),
    a_raw_frobenius = a_raw_norm,
    weight_max_abs = weight_max,
    mu_active = mu_max >= activity_fraction * mu_bound,
    kappa_active = abs(kappa) >= activity_fraction * kappa_bound,
    a_active = a_raw_norm >= activity_fraction * compact$a_bound,
    weight_active = weight_max >= activity_fraction * compact$weight_bound,
    activity_fraction = activity_fraction
  )
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
.sc_build_mixed_network <- function(p, p_Z, q, hidden, a_init_sd = 0.15,
                                    mu_bound = 10, kappa_bound = 10,
                                    a_bound = 10, weight_bound = 10,
                                    coefficient_scale = rep(1, p)) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_build_mixed_network(): the 'torch' package is required.")
  }
  hidden <- as.integer(hidden)
  p <- as.integer(p); p_Z <- as.integer(p_Z); q <- as.integer(q)
  compact <- .sc_mixed_validate_compact_bounds(
    p = p, coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound)
  if (!is.finite(mu_bound) || mu_bound <= 0) {
    stop(".sc_build_mixed_network(): `mu_bound` must be positive and finite.")
  }
  if (!is.finite(kappa_bound) || kappa_bound <= 0) {
    stop(".sc_build_mixed_network(): `kappa_bound` must be positive and finite.")
  }

  generator <- torch::nn_module(
    "MixedConjointDNN",
    initialize = function() {
      self$p_beta <- p
      self$q <- q
      self$mu_bound <- mu_bound
      self$kappa_bound <- kappa_bound
      layers <- list()
      in_dim <- p_Z
      for (i in seq_along(hidden)) {
        layers[[paste0("hidden_", i)]] <- torch::nn_linear(in_dim, hidden[i])
        in_dim <- hidden[i]
      }
      self$hidden <- torch::nn_module_list(layers)
      self$param_layer <- torch::nn_linear(in_dim, p)
      self$kappa_raw <- torch::nn_parameter(torch::torch_zeros(1L))
      if (q > 0L) {
        scale_t <- torch::torch_tensor(compact$coefficient_scale,
                                       dtype = torch::torch_float())$unsqueeze(2L)
        self$A <- torch::nn_parameter(
          torch::torch_randn(p, q) * a_init_sd * scale_t
        )
      }
    },
    get_beta = function(z) {
      h <- z
      for (i in seq_along(self$hidden)) {
        h <- torch::nnf_relu(self$hidden[[i]](h))
      }
      raw <- self$param_layer(h)
      self$mu_bound * torch::torch_tanh(raw / self$mu_bound)
    },
    get_kappa = function() {
      self$kappa_bound * torch::torch_tanh(self$kappa_raw / self$kappa_bound)
    }
  )
  net <- generator()
  .sc_mixed_project_parameters(
    net, coefficient_scale = compact$coefficient_scale,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound
  )
  net
}

#' Marginal (integrated) negative log-likelihood, respondent-weighted
#'
#' Computes -mean_i log L_i where
#' L_i = sum_g w_g prod_t G(idx_itg)^y (1 - G)^{1-y},
#' idx_itg = kappa + deltaX_it' mu(Z_i) + deltaX_it' A u_g.
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
  base <- net$get_kappa() + torch::torch_sum(dx * mu, dim = 2L) # n
  if (net$q == 0L) {
    idx <- base$unsqueeze(2L)                              # n x 1
  } else {
    fac <- torch::torch_mm(dx, net$A)                      # n x q
    idx <- base$unsqueeze(2L) + torch::torch_mm(fac, U_t$t()) # n x G
  }
  ## log G(idx)^y (1-G)^{1-y} = -softplus(-idx)*y - softplus(idx)*(1-y)
  lp <- -torch::nnf_softplus(-idx) * yt$unsqueeze(2L) -
    torch::nnf_softplus(idx) * (1 - yt)$unsqueeze(2L)      # n x G
  agg <- torch::torch_zeros(N, lp$shape[2], dtype = lp$dtype,
                            device = lp$device)
  agg <- agg$index_add(1L, resp_index1, lp)                # N x G, sum over tasks
  ll_i <- torch::torch_logsumexp(agg + logw_t$unsqueeze(1L), dim = 2L)  # N
  -torch::torch_mean(ll_i)
}

#' Squared-weight penalty for the mean network
#' @keywords internal
#' @noRd
.sc_mixed_penalty <- function(net, weight_decay) {
  structural <- c("A", "kappa_raw")
  nms <- setdiff(names(net$parameters), structural)
  if (length(nms) == 0L || weight_decay == 0) {
    return(torch::torch_zeros(1L, dtype = net$kappa_raw$dtype,
                              device = net$kappa_raw$device))
  }
  out <- torch::torch_zeros(1L, dtype = net$kappa_raw$dtype,
                            device = net$kappa_raw$device)
  for (nm in nms) out <- out + torch::torch_sum(net$parameters[[nm]]^2)
  weight_decay * out
}

#' Absolute parameter-gradient diagnostics
#' @keywords internal
#' @noRd
.sc_mixed_gradient_diagnostics <- function(net) {
  vals <- vapply(net$parameters, function(p) {
    if (is.null(p$grad)) return(0)
    as.numeric(p$grad$detach()$abs()$max()$item())
  }, numeric(1L))
  structural_names <- intersect(names(vals), c("A", "kappa_raw"))
  sieve_names <- setdiff(names(vals), structural_names)
  group_max <- function(nms) {
    if (!length(nms)) 0 else max(vals[nms], 0)
  }
  list(
    by_parameter = vals,
    total = max(vals, 0),
    structural = group_max(structural_names),
    sieve = group_max(sieve_names),
    structural_parameters = structural_names,
    sieve_parameters = sieve_names
  )
}

#' Largest absolute parameter gradient
#' @keywords internal
#' @noRd
.sc_mixed_gradient_norm <- function(net) {
  .sc_mixed_gradient_diagnostics(net)$total
}

#' Fail-closed diagnostics for one returned optimization state
#'
#' The criterion comparison is meaningful only when `previous_loss` is the
#' objective at the state immediately before the returned state. In
#' particular, it is deliberately unavailable after an early-stopping state
#' has been restored. The gate is an attained-solution diagnostic; it is not a
#' certificate of global optimality for the nonconvex objective.
#'
#' @keywords internal
#' @noRd
.sc_mixed_optimization_status <- function(final_loss, final_nll,
                                          previous_loss = NA_real_,
                                          gradient,
                                          opt_tol, grad_tol,
                                          bounds,
                                          state_restored = FALSE) {
  if (!is.numeric(opt_tol) || length(opt_tol) != 1L || !is.finite(opt_tol) ||
      opt_tol < 0 || !is.numeric(grad_tol) || length(grad_tol) != 1L ||
      !is.finite(grad_tol) || grad_tol < 0) {
    stop("Optimization tolerances must be finite nonnegative scalars.")
  }
  required_gradient <- c("total", "structural", "sieve")
  if (!is.list(gradient) ||
      !all(required_gradient %in% names(gradient))) {
    stop("Internal error: malformed optimization-gradient diagnostics.")
  }
  grad_values <- unlist(gradient[required_gradient], use.names = TRUE)
  objective_components <- c(
    penalized_nll = as.numeric(final_loss),
    unpenalized_nll = as.numeric(final_nll),
    penalty = as.numeric(final_loss - final_nll)
  )
  objective_finite <- all(is.finite(objective_components))

  criterion_source <- if (isTRUE(state_restored)) {
    "unavailable_after_early_stop_state_restoration"
  } else if (!is.finite(previous_loss)) {
    "unavailable_without_preceding_attained_state"
  } else {
    "returned_state_vs_immediately_preceding_attained_state"
  }
  last_relative_change <- if (identical(
    criterion_source,
    "returned_state_vs_immediately_preceding_attained_state"
  )) {
    abs(final_loss - previous_loss) / max(1, abs(previous_loss))
  } else {
    NA_real_
  }
  criterion_tolerance_met <- is.finite(last_relative_change) &&
    last_relative_change <= opt_tol
  gradients_finite <- all(is.finite(grad_values))
  stationarity_met <- gradients_finite && gradient$total <= grad_tol
  structural_stationarity_met <- gradients_finite &&
    gradient$structural <= grad_tol
  sieve_stationarity_met <- gradients_finite && gradient$sieve <= grad_tol
  required_bound_flags <- c("mu_active", "kappa_active", "a_active",
                            "weight_active")
  bound_diagnostics_complete <- is.list(bounds) &&
    all(required_bound_flags %in% names(bounds)) &&
    all(vapply(bounds[required_bound_flags], function(x) {
      is.logical(x) && length(x) == 1L && !is.na(x)
    }, logical(1L)))
  bound_activity <- if (bound_diagnostics_complete) {
    any(unlist(bounds[required_bound_flags], use.names = FALSE))
  } else TRUE

  failure_reasons <- character()
  if (!objective_finite) failure_reasons <- c(failure_reasons, "nonfinite_objective")
  if (!gradients_finite) failure_reasons <- c(failure_reasons, "nonfinite_gradient")
  if (!stationarity_met && gradients_finite) {
    failure_reasons <- c(failure_reasons, "gradient_tolerance_not_met")
  }
  if (!criterion_tolerance_met) {
    failure_reasons <- c(failure_reasons, if (isTRUE(state_restored)) {
      "criterion_unavailable_after_state_restoration"
    } else if (!is.finite(previous_loss)) {
      "criterion_unavailable_without_preceding_state"
    } else {
      "criterion_tolerance_not_met"
    })
  }
  if (!bound_diagnostics_complete) {
    failure_reasons <- c(failure_reasons, "compact_bound_diagnostics_incomplete")
  } else if (bound_activity) {
    failure_reasons <- c(failure_reasons, "parameter_bound_active")
  }

  optimization_gate_pass <- objective_finite && stationarity_met &&
    criterion_tolerance_met && bound_diagnostics_complete && !bound_activity
  list(
    objective_components = objective_components,
    objective_finite = objective_finite,
    gradients_finite = gradients_finite,
    stationarity_met = stationarity_met,
    structural_stationarity_met = structural_stationarity_met,
    sieve_stationarity_met = sieve_stationarity_met,
    last_relative_change = as.numeric(last_relative_change),
    criterion_tolerance_met = criterion_tolerance_met,
    criterion_diagnostic_source = criterion_source,
    state_restored = isTRUE(state_restored),
    bound_diagnostics_complete = bound_diagnostics_complete,
    bound_activity = bound_activity,
    optimization_gate_pass = optimization_gate_pass,
    failure_reasons = unique(failure_reasons),
    global_optimality_gap_known = FALSE
  )
}

#' Train one mixed-logit start on a data subset
#'
#' Full-batch Adam targets the respondent-average integrated negative log
#' likelihood plus the explicit squared-weight penalty. The returned
#' diagnostics report the attained criterion and gradient residual; they do
#' not certify that the global nonconvex optimum has been found.
#'
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
                                early_stop = FALSE,
                                val_frac = 0.1,
                                check_every = 20L,
                                patience = 3L,
                                opt_tol = 1e-7,
                                grad_tol = 1e-4,
                                mu_bound = 10,
                                kappa_bound = 10,
                                a_bound = 10,
                                weight_bound = 10,
                                coefficient_scale = rep(1, ncol(deltaX))) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_train_mixed_one(): the 'torch' package is required.")
  }
  n_epochs <- as.integer(n_epochs)
  if (is.na(n_epochs) || n_epochs < 1L) {
    stop(".sc_train_mixed_one(): `n_epochs` must be positive.")
  }
  if (!is.numeric(check_every) || length(check_every) != 1L ||
      is.na(check_every) || check_every < 1L ||
      check_every != as.integer(check_every) ||
      !is.numeric(patience) || length(patience) != 1L || is.na(patience) ||
      patience < 1L || patience != as.integer(patience)) {
    stop(".sc_train_mixed_one(): `check_every` and `patience` must be positive integers.")
  }
  if (!is.numeric(opt_tol) || length(opt_tol) != 1L || !is.finite(opt_tol) ||
      opt_tol < 0 || !is.numeric(grad_tol) || length(grad_tol) != 1L ||
      !is.finite(grad_tol) || grad_tol < 0) {
    stop(".sc_train_mixed_one(): optimization tolerances must be finite and nonnegative.")
  }
  compact <- .sc_mixed_validate_compact_bounds(
    p = ncol(deltaX), coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound)
  if (!is.null(seed)) {
    withr::local_preserve_seed()
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  resp_all <- unique(respondent_id)
  if (isTRUE(early_stop) && length(resp_all) >= 50L) {
    n_val <- max(10L, floor(val_frac * length(resp_all)))
    val_resp <- sample(resp_all, n_val)
    is_val <- respondent_id %in% val_resp
  } else {
    is_val <- rep(FALSE, length(respondent_id))
    early_stop <- FALSE
  }

  dev <- torch::torch_device(device)
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
  tr <- mk_tensors(!is_val)
  va <- if (early_stop) mk_tensors(is_val) else NULL
  all_data <- mk_tensors(rep(TRUE, length(y)))
  q <- ncol(gh$U)
  ## Some torch builds do not construct zero-column tensors reliably. The
  ## q = 0 likelihood never reads U_t, so use a harmless one-point placeholder.
  U_t <- if (q == 0L) {
    torch::torch_zeros(1L, 1L, dtype = torch::torch_float(), device = dev)
  } else {
    torch::torch_tensor(gh$U, dtype = torch::torch_float(), device = dev)
  }
  logw_t <- torch::torch_tensor(log(gh$w), dtype = torch::torch_float(),
                                device = dev)

  net <- .sc_build_mixed_network(
    p = ncol(deltaX), p_Z = ncol(Z), q = q, hidden = hidden,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound,
    coefficient_scale = compact$coefficient_scale
  )
  if (!is.null(warm_state)) {
    ## Warm starts are permitted only when the caller has established that
    ## the state contains no held-out outcomes. `scmix()` therefore uses this
    ## path for its full-sample fit, never for an outer-fold nuisance fit.
    ok <- tryCatch({
      own <- net$state_dict()
      for (nm in intersect(names(warm_state), names(own))) {
        if (!nm %in% c("A", "kappa_raw")) own[[nm]] <- warm_state[[nm]]
      }
      net$load_state_dict(own)
      TRUE
    }, error = function(e) FALSE)
    if (!ok && isTRUE(verbose)) {
      message(".sc_train_mixed_one(): warm start skipped (state mismatch).")
    }
  }
  net$to(device = dev)
  ## Warm states and device transfers are followed by the same compact-set
  ## projection used after every optimizer update.
  .sc_mixed_project_parameters(
    net, coefficient_scale = compact$coefficient_scale,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound
  )
  optimizer <- torch::optim_adam(net$parameters, lr = learning_rate,
                                 weight_decay = 0)

  loss_trace <- nll_trace <- numeric(n_epochs)
  grad_trace <- rep(NA_real_, n_epochs)
  val_trace <- numeric(0)
  best_val <- Inf
  best_state <- NULL
  bad_checks <- 0L
  stable_checks <- 0L
  previous_check <- Inf
  stopped_at <- n_epochs
  stop_reason <- "maximum_epochs"

  for (epoch in seq_len(n_epochs)) {
    net$train()
    optimizer$zero_grad()
    nll <- .sc_mixed_nll(net, tr$dx, tr$zt, tr$yt, U_t, logw_t,
                         tr$idx1, tr$N)
    loss <- nll + .sc_mixed_penalty(net, weight_decay)
    loss$backward()
    grad_now <- .sc_mixed_gradient_norm(net)
    optimizer$step()
    .sc_mixed_project_parameters(
      net, coefficient_scale = compact$coefficient_scale,
      a_bound = compact$a_bound, weight_bound = compact$weight_bound
    )
    loss_trace[epoch] <- as.numeric(loss$item())
    nll_trace[epoch] <- as.numeric(nll$item())
    grad_trace[epoch] <- grad_now

    if (epoch %% check_every == 0L) {
      current <- loss_trace[epoch]
      rel_change <- abs(previous_check - current) / max(1, abs(previous_check))
      if (is.finite(rel_change) && rel_change <= opt_tol && grad_now <= grad_tol) {
        stable_checks <- stable_checks + 1L
      } else {
        stable_checks <- 0L
      }
      previous_check <- current
      if (!early_stop && stable_checks >= patience) {
        stopped_at <- epoch
        stop_reason <- "criterion_and_gradient_tolerance"
        break
      }
      if (early_stop) {
        net$eval()
        vloss <- as.numeric(torch::with_no_grad(
          .sc_mixed_nll(net, va$dx, va$zt, va$yt, U_t, logw_t,
                        va$idx1, va$N)
        )$item())
        val_trace <- c(val_trace, vloss)
        if (vloss < best_val - opt_tol) {
          best_val <- vloss
          best_state <- lapply(net$state_dict(), function(t) t$clone())
          bad_checks <- 0L
        } else {
          bad_checks <- bad_checks + 1L
          if (bad_checks >= patience) {
            stopped_at <- epoch
            stop_reason <- "validation_early_stop"
            break
          }
        }
      }
    }
    if (verbose && (epoch %% 100L == 0L || epoch == 1L)) {
      message(sprintf("  epoch %4d  penalized nll = %.6f", epoch,
                      loss_trace[epoch]))
    }
  }
  state_restored <- isTRUE(early_stop) && !is.null(best_state)
  if (state_restored) net$load_state_dict(best_state)
  .sc_mixed_project_parameters(
    net, coefficient_scale = compact$coefficient_scale,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound
  )

  ## Re-evaluate the returned state on every respondent supplied to this
  ## start, so starts remain comparable even when validation stopping is used.
  net$eval()
  optimizer$zero_grad()
  final_nll_t <- .sc_mixed_nll(net, all_data$dx, all_data$zt, all_data$yt,
                               U_t, logw_t, all_data$idx1, all_data$N)
  final_loss_t <- final_nll_t + .sc_mixed_penalty(net, weight_decay)
  final_loss_t$backward()
  final_gradient <- .sc_mixed_gradient_diagnostics(net)
  final_grad <- final_gradient$total
  final_nll <- as.numeric(final_nll_t$item())
  final_penalized_loss <- as.numeric(final_loss_t$item())
  n_recorded <- length(loss_trace[seq_len(stopped_at)])
  A_hat <- if (q == 0L) matrix(numeric(0), nrow = ncol(deltaX), ncol = 0L) else
    as.matrix(torch::as_array(net$A))
  kappa_hat <- as.numeric(net$get_kappa()$detach()$cpu()$item())
  mu_train <- .sc_predict_beta(net, Z)
  bounds <- .sc_mixed_bound_diagnostics(
    net = net, mu = mu_train, kappa = kappa_hat,
    coefficient_scale = compact$coefficient_scale,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound
  )
  ## loss_trace[stopped_at] is evaluated immediately before the final Adam
  ## update. It is therefore the correct adjacent attained state only when
  ## early stopping did not reserve a validation subset and did not restore a
  ## different state after the loop.
  previous_attained_loss <- if (!isTRUE(early_stop) && n_recorded >= 1L) {
    loss_trace[stopped_at]
  } else {
    NA_real_
  }
  status <- .sc_mixed_optimization_status(
    final_loss = final_penalized_loss, final_nll = final_nll,
    previous_loss = previous_attained_loss, gradient = final_gradient,
    opt_tol = opt_tol, grad_tol = grad_tol, bounds = bounds,
    state_restored = state_restored
  )

  list(
    net = net,
    loss_trace = loss_trace[seq_len(stopped_at)],
    nll_trace = nll_trace[seq_len(stopped_at)],
    grad_trace = grad_trace[seq_len(stopped_at)],
    val_trace = val_trace,
    stopped_at = stopped_at,
    final_loss = final_nll,
    penalized_loss = final_penalized_loss,
    objective = -final_penalized_loss,
    final_gradient_norm = final_grad,
    final_gradient_by_parameter = final_gradient$by_parameter,
    structural_gradient_norm = final_gradient$structural,
    sieve_gradient_norm = final_gradient$sieve,
    converged = status$objective_finite && status$stationarity_met &&
      status$criterion_tolerance_met,
    stationarity_met = status$stationarity_met,
    structural_stationarity_met = status$structural_stationarity_met,
    sieve_stationarity_met = status$sieve_stationarity_met,
    last_relative_change = status$last_relative_change,
    criterion_tolerance_met = status$criterion_tolerance_met,
    criterion_diagnostic_source = status$criterion_diagnostic_source,
    state_restored = status$state_restored,
    objective_components = status$objective_components,
    objective_finite = status$objective_finite,
    bound_diagnostics_complete = status$bound_diagnostics_complete,
    bound_activity = status$bound_activity,
    optimization_gate_pass = status$optimization_gate_pass,
    optimization_failure_reasons = status$failure_reasons,
    stop_reason = stop_reason,
    early_stop = early_stop,
    A = A_hat,
    kappa = kappa_hat,
    bounds = bounds
  )
}

#' Fit multiple starts and retain the highest attained penalized objective
#' @keywords internal
#' @noRd
.sc_train_mixed_multistart <- function(..., n_starts = 2L, seed = NULL,
                                       warm_state = NULL) {
  if (!is.numeric(n_starts) || length(n_starts) != 1L || is.na(n_starts) ||
      n_starts < 1L || n_starts != as.integer(n_starts)) {
    stop(".sc_train_mixed_multistart(): `n_starts` must be positive.")
  }
  n_starts <- as.integer(n_starts)
  fits <- vector("list", n_starts)
  for (s in seq_len(n_starts)) {
    seed_s <- if (is.null(seed)) NULL else {
      out <- (as.double(seed) + 104729 * (s - 1L)) %% (.Machine$integer.max - 1)
      as.integer(out + 1)
    }
    fits[[s]] <- .sc_train_mixed_one(
      ..., seed = seed_s,
      warm_state = if (s == 1L) warm_state else NULL
    )
  }
  objective <- vapply(fits, `[[`, numeric(1L), "objective")
  finite_objective <- is.finite(objective)
  if (!any(finite_objective)) {
    stop(".sc_train_mixed_multistart(): every start returned a nonfinite objective.")
  }
  objective_for_selection <- objective
  objective_for_selection[!finite_objective] <- -Inf
  best <- which.max(objective_for_selection)
  ans <- fits[[best]]
  ans$best_start <- best
  ans$start_diagnostics <- data.frame(
    start = seq_len(n_starts),
    objective = objective,
    penalized_nll = -objective,
    unpenalized_nll = vapply(fits, `[[`, numeric(1L), "final_loss"),
    gradient_norm = vapply(fits, `[[`, numeric(1L), "final_gradient_norm"),
    structural_gradient_norm = vapply(fits, `[[`, numeric(1L),
                                        "structural_gradient_norm"),
    sieve_gradient_norm = vapply(fits, `[[`, numeric(1L),
                                   "sieve_gradient_norm"),
    objective_finite = vapply(fits, `[[`, logical(1L), "objective_finite"),
    criterion_tolerance_met = vapply(fits, `[[`, logical(1L),
                                         "criterion_tolerance_met"),
    state_restored = vapply(fits, `[[`, logical(1L), "state_restored"),
    mu_bound_active = vapply(fits, function(x) isTRUE(x$bounds$mu_active),
                             logical(1L)),
    kappa_bound_active = vapply(fits, function(x) isTRUE(x$bounds$kappa_active),
                                logical(1L)),
    a_bound_active = vapply(fits, function(x) isTRUE(x$bounds$a_active),
                            logical(1L)),
    weight_bound_active = vapply(fits, function(x) isTRUE(x$bounds$weight_active),
                                 logical(1L)),
    bound_diagnostics_complete = vapply(
      fits, `[[`, logical(1L), "bound_diagnostics_complete"),
    bound_activity = vapply(fits, `[[`, logical(1L), "bound_activity"),
    optimization_gate_pass = vapply(fits, `[[`, logical(1L),
                                       "optimization_gate_pass"),
    converged = vapply(fits, `[[`, logical(1L), "converged"),
    epochs = vapply(fits, `[[`, integer(1L), "stopped_at"),
    stop_reason = vapply(fits, `[[`, character(1L), "stop_reason"),
    stringsAsFactors = FALSE
  )
  sorted <- sort(objective[finite_objective], decreasing = TRUE)
  ans$start_objective_range <- diff(range(objective[finite_objective]))
  ans$best_minus_second <- if (length(sorted) >= 2L) sorted[1L] - sorted[2L] else NA_real_
  ## These are attained-start diagnostics, not a computable upper bound on
  ## the gap between the selected nonconvex solution and the global optimum.
  ans$global_optimality_gap_known <- FALSE
  ans
}

#' Integrated-likelihood mixed-logit conjoint estimator
#'
#' Fits the rebuilt structural model in which respondent preferences
#' are `beta_i = mu(Z_i) + A u_i` with `u_i ~ N(0, I_q)` and the
#' respondent's whole choice sequence enters one marginal likelihood.
#' The conditional mean and residual covariance are estimated jointly within
#' the maintained normal mixed-logit family. The mixture is inside the
#' respondent-sequence objective, so the estimator does not relabel a
#' task-level conditional-logit projection as a latent preference mean.
#'
#' The interface deliberately mirrors [scfit()] (same formula and long-format
#' data contract). A full-sample fit estimates the structural plug-in object.
#' Separate respondent-level outer-fold fits provide nuisance estimates for
#' [scmix_dml()] and held-out specification assessment.
#'
#' @inheritParams scfit
#' @param q Integer, dimension of the residual factor. `q = 0` is the
#'   homogeneous model; product Gauss--Hermite is available through `q = 3`.
#' @param n_nodes Integer, univariate Gauss-Hermite nodes per factor
#'   dimension (default 31; the q-dim grid is the tensor product).
#' @param integration One of `"auto"`, `"gh"`, or `"qmc"`. Auto uses exact
#'   evaluation at `q = 0`, Gauss--Hermite through `q = 3`, and optional
#'   Sobol QMC above `q = 3`.
#' @param n_draws Number of QMC draws; ignored by exact and GH integration.
#'   Must be even when `qmc_antithetic = TRUE`.
#' @param qmc_antithetic,qmc_scramble Logical QMC controls.
#' @param K Integer, respondent-clustered folds (default 5).
#' @param hidden Mean-network hidden-layer widths, or `"auto"`.
#' @param n_epochs Maximum full-batch Adam epochs per optimization start.
#' @param learning_rate Adam learning rate.
#' @param weight_decay Nonnegative squared-weight penalty coefficient, or the
#'   backward-compatible `"adaptive"` rule. Paper analyses should tune a finite
#'   prespecified penalty grid inside each outer training sample.
#' @param init Optional [scfit] object used to warm start the full-sample
#'   mean trunk (architecture must match `hidden`). To prevent cross-fit
#'   leakage, this state is never used for an outer-fold fit.
#' @param n_starts Number of prespecified optimization starts per fit.
#' @param mu_bound,kappa_bound Positive finite tanh bounds for the fitted
#'   conditional mean coordinates and position effect.
#' @param a_bound Positive finite Frobenius-norm bound on the loading matrix in
#'   raw coefficient units. Training-scale loadings are projected after every
#'   optimizer update using the corresponding contrast scales.
#' @param weight_bound Positive finite coordinatewise bound on every
#'   mean-network weight and bias. Network parameters are projected after every
#'   optimizer update.
#' @param opt_tol,grad_tol Numerical diagnostic tolerances. Passing these
#'   tolerances does not certify a global optimum.
#' @param seed Reproducibility seed for folds, integration nodes, and starts.
#' @param device Torch device.
#' @param verbose Print optimization progress.
#' @param early_stop Legacy validation-stopping option. With the default
#'   `FALSE`, a start stops at the joint diagnostic tolerance or the maximum
#'   epoch count. Every returned state is audited separately.
#'
#' With a bounded number of tasks per respondent, this estimator targets the
#' population and subgroup preference distribution within the maintained
#' mixed-logit model. It does not consistently recover any respondent's
#' realized preference vector; no posterior mean or mode is returned as an
#' estimated individual preference.
#'
#' @return An object of class `scmix`, including a stable `analysis_signature`
#'   based on the data, model/learner specification, and respondent folds but
#'   not fitted parameters or integration resolution. Use [scmix_dml()] for
#'   paper-aligned regular inference and the `scmix_paper_*` functions for
#'   structural plug-in quantities.
#' @export
scmix <- function(formula, data,
                  respondent = "resp_id", task = "task_id", profile = "profile_id",
                  q = 1L,
                  n_nodes = 31L,
                  integration = c("auto", "gh", "qmc"),
                  n_draws = 4096L,
                  qmc_antithetic = TRUE,
                  qmc_scramble = TRUE,
                  K = 5L,
                  hidden = "auto",
                  n_epochs = 400L,
                  learning_rate = 0.01,
                  weight_decay = "adaptive",
                  n_starts = 2L,
                  mu_bound = 10,
                  kappa_bound = 10,
                  a_bound = 10,
                  weight_bound = 10,
                  opt_tol = 1e-7,
                  grad_tol = 1e-4,
                  seed = NULL,
                  init = NULL,
                  device = "cpu",
                  verbose = FALSE,
                  early_stop = FALSE) {
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
  ## `.sc_to_long()` has already imposed this typed canonical order.  Repeat
  ## it directly rather than sorting a pasted character key, which would put
  ## numeric respondent 10 before respondent 2.
  ord <- order(data[[respondent]], data[[task]], data[[profile]])
  y_sorted <- y_long[ord]
  idx1 <- seq(1L, length(y_sorted), by = 2L)
  y_first <- y_sorted[idx1]
  y_second <- y_sorted[idx1 + 1L]
  if (!all(y_first %in% c(0, 1))) {
    stop("scmix(): the response must be a 0/1 choice indicator on profile rows.")
  }
  if (!all(y_first + y_second == 1)) {
    stop("scmix(): the two profile rows of each task must have complementary ",
         "0/1 choices (exactly one profile chosen per task).")
  }
  y <- as.numeric(y_first)

  n <- nrow(deltaX)
  p <- ncol(deltaX)
  if (!is.numeric(q) || length(q) != 1L || is.na(q) ||
      q < 0L || q != as.integer(q) || q > p - 1L) {
    stop("scmix(): `q` must be an integer between 0 and p - 1.")
  }
  q <- as.integer(q)
  compact <- .sc_mixed_validate_compact_bounds(
    p = p, coefficient_scale = rep(1, p),
    a_bound = a_bound, weight_bound = weight_bound
  )
  hidden_use <- if (identical(hidden, "auto")) {
    .sc_auto_hidden(n, p)
  } else as.integer(hidden)
  wd_use <- .sc_resolve_weight_decay(weight_decay, n, p)
  integration <- match.arg(integration)
  gh <- .sc_mixed_grid(
    q = q, integration = integration, n_nodes = n_nodes,
    n_draws = n_draws, seed = seed,
    antithetic = qmc_antithetic, scramble = qmc_scramble
  )

  ## Internal contrast standardization.  Continuous attributes (e.g. tax
  ## rates in percentage points) put deltaX entries at O(10-50); the
  ## loading initialization and the integration nodes then start the
  ## mixture wildly over-dispersed and training fails.  Standardizing
  ## each contrast column for TRAINING ONLY and rescaling (mu, A) back
  ## to raw units on output leaves the index deltaX' mu exactly
  ## invariant, so every downstream score works on the raw scale. The
  ## full-sample scale below is used only by the full fit; each outer fold
  ## learns its own respondent-weighted scale inside that training sample.
  dx_transform_full <- .sc_comp_fit_dx_scale(deltaX, resp_task)
  sd_dx <- dx_transform_full$scale
  deltaX_std_full <- sweep(deltaX, 2L, sd_dx, `/`)

  fold_id <- .sc_make_folds(resp_task, K = K, seed = seed)
  warm_state <- NULL
  if (!is.null(init)) {
    if (!inherits(init, "sc_fit") || is.null(init$nets) || length(init$nets) == 0L) {
      warning("scmix(): `init` has no stored nets; warm start skipped.")
    } else {
      warm_state <- init$nets[[1L]]$state_dict()
      ## the scfit trunk predicts beta on the RAW deltaX scale (its default
      ## normalize_deltaX = FALSE); scmix trains on standardized contrasts,
      ## where mu_std = mu_raw * sd_dx, so rescale the output layer rows
      if ("param_layer.weight" %in% names(warm_state)) {
        sd_t <- torch::torch_tensor(sd_dx, dtype = torch::torch_float())
        warm_state[["param_layer.weight"]] <-
          warm_state[["param_layer.weight"]] * sd_t$unsqueeze(2L)
        warm_state[["param_layer.bias"]] <-
          warm_state[["param_layer.bias"]] * sd_t
      }
      if (isTRUE(verbose)) {
        message("scmix(): init state will warm-start the full-sample fit only; ",
                "outer-fold fits start independently to prevent leakage.")
      }
    }
  }

  N_all <- length(unique(resp_task))
  mu_hat <- matrix(NA_real_, nrow = n, ncol = ncol(deltaX))
  mu_all_folds <- vector("list", K)
  A_folds <- vector("list", K)
  kappa_folds <- numeric(K)
  sd_dx_folds <- vector("list", K)
  dx_transform_folds <- vector("list", K)
  z_transform_folds <- vector("list", K)
  loss_traces <- vector("list", K)
  optimization_folds <- vector("list", K)
  nets <- vector("list", K)

  for (k in seq_len(K)) {
    in_k <- fold_id != k
    z_transform_folds[[k]] <- .sc_fit_z_transform(
      Z_task[in_k, , drop = FALSE], resp_task[in_k])
    dx_transform_folds[[k]] <- .sc_comp_fit_dx_scale(
      deltaX[in_k, , drop = FALSE], resp_task[in_k])
    sd_dx_folds[[k]] <- dx_transform_folds[[k]]$scale
    deltaX_train_k <- sweep(deltaX[in_k, , drop = FALSE], 2L,
                            sd_dx_folds[[k]], `/`)
    Z_train_k <- .sc_apply_z_transform(
      Z_task[in_k, , drop = FALSE], z_transform_folds[[k]])
    fit_k <- .sc_train_mixed_multistart(
      deltaX = deltaX_train_k,
      y = y[in_k],
      Z = Z_train_k,
      respondent_id = resp_task[in_k],
      gh = gh, hidden = hidden_use,
      n_epochs = n_epochs, learning_rate = learning_rate,
      weight_decay = wd_use,
      seed = if (is.null(seed)) NULL else .sc_fold_seed(seed, k),
      device = device, verbose = verbose,
      warm_state = NULL,
      early_stop = early_stop,
      n_starts = n_starts,
      opt_tol = opt_tol, grad_tol = grad_tol,
      mu_bound = mu_bound, kappa_bound = kappa_bound,
      a_bound = compact$a_bound, weight_bound = compact$weight_bound,
      coefficient_scale = sd_dx_folds[[k]]
    )
    out_k <- fold_id == k
    ## rescale from the standardized training scale back to raw units:
    ## mu_raw_k = mu_std_k / sd_k, A_raw[k, ] = A_std[k, ] / sd_k
    ## Store each fold network's task-level predictions on all respondents.
    ## This lets inference evaluate training and held-out observations under
    ## the same frozen fold nuisance; mu_hat remains the OOF slice.
    Z_all_k <- .sc_apply_z_transform(Z_task, z_transform_folds[[k]])
    mu_all_folds[[k]] <- sweep(
      .sc_predict_beta(fit_k$net, Z_all_k), 2L, sd_dx_folds[[k]], `/`)
    mu_hat[out_k, ] <- mu_all_folds[[k]][out_k, , drop = FALSE]
    A_folds[[k]] <- fit_k$A / sd_dx_folds[[k]]
    kappa_folds[k] <- fit_k$kappa
    loss_traces[[k]] <- fit_k$loss_trace
    optimization_folds[[k]] <- list(
      best_start = fit_k$best_start,
      starts = fit_k$start_diagnostics,
      objective = fit_k$objective,
      gradient_norm = fit_k$final_gradient_norm,
      gradient_by_parameter = fit_k$final_gradient_by_parameter,
      structural_gradient_norm = fit_k$structural_gradient_norm,
      sieve_gradient_norm = fit_k$sieve_gradient_norm,
      converged = fit_k$converged,
      stationarity_met = fit_k$stationarity_met,
      structural_stationarity_met = fit_k$structural_stationarity_met,
      sieve_stationarity_met = fit_k$sieve_stationarity_met,
      last_relative_change = fit_k$last_relative_change,
      criterion_tolerance_met = fit_k$criterion_tolerance_met,
      criterion_diagnostic_source = fit_k$criterion_diagnostic_source,
      state_restored = fit_k$state_restored,
      objective_components = fit_k$objective_components,
      objective_finite = fit_k$objective_finite,
      optimization_gate_pass = fit_k$optimization_gate_pass,
      optimization_failure_reasons = fit_k$optimization_failure_reasons,
      stop_reason = fit_k$stop_reason,
      bounds = fit_k$bounds,
      start_objective_range = fit_k$start_objective_range,
      best_minus_second = fit_k$best_minus_second,
      global_optimality_gap_known = FALSE
    )
    nets[[k]] <- fit_k$net
    if (verbose) {
      message(sprintf("scmix fold %d/%d done (nll = %.5f)", k, K, fit_k$final_loss))
    }
  }

  ## The full-sample fit estimates the paper's plug-in structural object.
  ## The fold fits above are retained separately as nuisance estimates for
  ## cross-fitted inference and held-out assessment.
  full_seed <- if (is.null(seed)) NULL else
    as.integer((as.double(seed) + 900001) %% (.Machine$integer.max - 1) + 1)
  z_transform_full <- .sc_fit_z_transform(Z_task, resp_task)
  Z_full <- .sc_apply_z_transform(Z_task, z_transform_full)
  full_fit <- .sc_train_mixed_multistart(
    deltaX = deltaX_std_full, y = y, Z = Z_full, respondent_id = resp_task,
    gh = gh, hidden = hidden_use,
    n_epochs = n_epochs, learning_rate = learning_rate,
    weight_decay = wd_use, seed = full_seed,
    device = device, verbose = verbose,
    warm_state = warm_state, early_stop = early_stop,
    n_starts = n_starts, opt_tol = opt_tol, grad_tol = grad_tol,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound,
    coefficient_scale = sd_dx
  )
  mu_full <- sweep(.sc_predict_beta(full_fit$net, Z_full), 2L, sd_dx, `/`)
  A_full <- full_fit$A / sd_dx
  Sigma_hat <- tcrossprod(A_full)
  optimization_gate_by_fold <- vapply(
    optimization_folds,
    function(x) isTRUE(x$optimization_gate_pass), logical(1L))
  computational_optimization_gate_pass <-
    isTRUE(full_fit$optimization_gate_pass) && all(optimization_gate_by_fold)

  ## A is identified only up to right rotation. For descriptive output we
  ## retain a Procrustes-aligned copy, but A_folds itself remains exactly the
  ## matrix optimized with the stored finite integration nodes. An arbitrary
  ## post-fit rotation need not preserve a finite-node likelihood, so scores
  ## and likelihood assessment must use A_folds, never the aligned copy.
  A_folds_aligned <- lapply(A_folds, function(A) A)
  if (q > 0L && K >= 1L) {
    A_ref <- A_full
    for (k in seq_len(K)) {
      M_p <- crossprod(A_folds[[k]], A_ref)
      sv <- svd(M_p)
      A_folds_aligned[[k]] <- A_folds[[k]] %*% (sv$u %*% t(sv$v))
    }
  }

  analysis_signature <- .sc_analysis_signature(
    deltaX = deltaX, y = y, Z = Z_task, respondent_id = resp_task,
    fold_id = fold_id,
    specification = list(
      estimator = "respondent-sequence-normal-mixed-logit",
      q = q,
      hidden = hidden_use,
      integration_method = gh$metadata$method,
      qmc_antithetic = isTRUE(qmc_antithetic),
      n_epochs = as.integer(n_epochs),
      learning_rate = as.numeric(learning_rate),
      weight_decay = as.numeric(wd_use),
      n_starts = as.integer(n_starts),
      mu_bound = as.numeric(mu_bound),
      kappa_bound = as.numeric(kappa_bound),
      a_bound = compact$a_bound,
      weight_bound = compact$weight_bound,
      opt_tol = as.numeric(opt_tol),
      grad_tol = as.numeric(grad_tol),
      early_stop = isTRUE(early_stop),
      init_used_for_full_fit = !is.null(warm_state)
    )
  )

  fit <- list(
    mu_hat = mu_hat,               # task rows; constant within respondent
    mu_all_folds = mu_all_folds,   # task rows x p for every fold network
    mu_full = mu_full,              # full-sample plug-in mean estimate
    kappa_hat = full_fit$kappa,
    kappa_folds = kappa_folds,
    A_hat = A_full,
    Sigma_hat = Sigma_hat,
    A_folds = A_folds,               # optimized; used by likelihood/scores
    A_folds_aligned = A_folds_aligned, # reporting only
    factor_alignment = list(reference = "full-sample A",
                            method = if (q > 0L) "orthogonal Procrustes" else "none",
                            reporting_only = TRUE),
    sd_dx = sd_dx,                     # full-sample respondent-weighted scale
    sd_dx_full = sd_dx,
    sd_dx_folds = sd_dx_folds,
    dx_transform_full = dx_transform_full,
    dx_transform_folds = dx_transform_folds,
    q = q,
    gh = gh,                         # legacy name retained for compatibility
    integration_grid = gh,
    integration = gh$metadata,
    deltaX = deltaX,
    y = y,
    Z = Z_task,                       # raw encoded moderators
    z_transform_folds = z_transform_folds,
    z_transform_full = z_transform_full,
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
    learning_rate = learning_rate,
    weight_decay_used = wd_use,
    n_starts = as.integer(n_starts),
    bounds = list(mu = mu_bound, mu_internal = mu_bound,
                  mu_raw_by_coordinate = mu_bound / sd_dx,
                  kappa = kappa_bound,
                  a = compact$a_bound,
                  a_units = "raw-coefficient Frobenius norm",
                  weight = compact$weight_bound,
                  weight_units = "coordinatewise network parameter",
                  internal_mu_scale = "standardized contrasts"),
    optimization = list(
      full = list(best_start = full_fit$best_start,
                  starts = full_fit$start_diagnostics,
                  objective = full_fit$objective,
                  gradient_norm = full_fit$final_gradient_norm,
                  gradient_by_parameter = full_fit$final_gradient_by_parameter,
                  structural_gradient_norm = full_fit$structural_gradient_norm,
                  sieve_gradient_norm = full_fit$sieve_gradient_norm,
                  converged = full_fit$converged,
                  stationarity_met = full_fit$stationarity_met,
                  structural_stationarity_met = full_fit$structural_stationarity_met,
                  sieve_stationarity_met = full_fit$sieve_stationarity_met,
                  last_relative_change = full_fit$last_relative_change,
                  criterion_tolerance_met = full_fit$criterion_tolerance_met,
                  criterion_diagnostic_source = full_fit$criterion_diagnostic_source,
                  state_restored = full_fit$state_restored,
                  objective_components = full_fit$objective_components,
                  objective_finite = full_fit$objective_finite,
                  optimization_gate_pass = full_fit$optimization_gate_pass,
                  optimization_failure_reasons = full_fit$optimization_failure_reasons,
                  stop_reason = full_fit$stop_reason,
                  bounds = full_fit$bounds,
                  start_objective_range = full_fit$start_objective_range,
                  best_minus_second = full_fit$best_minus_second,
                  global_optimality_gap_known = FALSE),
      folds = optimization_folds,
      gate_by_fold = optimization_gate_by_fold,
      diagnostics_are_certificates = FALSE
    ),
    computational_optimization_gate_pass =
      computational_optimization_gate_pass,
    analysis_signature = analysis_signature,
    seed = seed,
    loss_traces = loss_traces,
    nets = nets,
    full_net = full_fit$net,
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
  int_method <- if (!is.null(x$integration$method)) x$integration$method else
    if (x$q == 0L) "exact" else "gauss-hermite"
  n_points <- if (!is.null(x$integration$n_points)) x$integration$n_points else
    length(x$gh$w)
  cat(sprintf("  residual factor dimension q = %d, integration = %s (%d points), K = %d folds\n",
              x$q, int_method, n_points, x$K))
  if (!is.null(x$kappa_hat)) {
    cat(sprintf("  position effect kappa = %.4f\n", x$kappa_hat))
  }
  Sig <- if (!is.null(x$Sigma_hat)) x$Sigma_hat else
    Reduce(`+`, lapply(x$A_folds, tcrossprod)) / length(x$A_folds)
  cat("  residual SDs (sqrt diag of full-sample AA'):\n")
  sds <- sqrt(pmax(diag(Sig), 0))
  names(sds) <- x$attr_names
  print(round(sds, 3))
  if (!is.null(x$optimization$full)) {
    od <- x$optimization$full
    structural_gradient <- if (is.null(od$structural_gradient_norm)) {
      NA_real_
    } else od$structural_gradient_norm
    cat(sprintf(paste0(
      "  selected start: %d/%d; total/structural gradient diagnostics",
      " = %.3g/%.3g\n"),
      od$best_start, nrow(od$starts), od$gradient_norm,
      structural_gradient))
    overall_gate <- if (!is.null(x$computational_optimization_gate_pass)) {
      x$computational_optimization_gate_pass
    } else od$optimization_gate_pass
    cat(sprintf(paste0(
      "  full-and-fold computational gate: %s",
      " (attained-solution diagnostic only)\n"),
      if (isTRUE(overall_gate)) "pass" else "fail"))
  }
  .scmix_print_floor(x)
  invisible(x)
}

#' Zero-floor status line shared by print.scmix and summary.scmix
#' @keywords internal
#' @noRd
.scmix_print_floor <- function(x) {
  cal <- x$zero_floor
  if (is.null(cal)) {
    cat("  zero-floor calibration: not run. Before interpreting residual",
        "SDs or\n  sign shares, run",
        "`fit$zero_floor <- scmix_calibrate_zero(fit)`.\n")
    cat("  descriptive legacy diagnostic only; not a paperps reporting gate\n")
    return(invisible(NULL))
  }
  cat(sprintf(paste0("  zero-floor calibration: fitted/floor index-SD ratio",
                     " = %.2f\n"), cal$ratio))
  ## Provisional threshold carried forward unchanged from the
  ## pre-paperps-rebuild print method (see scmix_calibrate_zero()'s
  ## documented `ratio`: values near 1 are indistinguishable from the
  ## small-T floor the diagnostic manufactures from a zero-heterogeneity
  ## truth). Not a paperps reporting gate.
  small_t_floor_ratio <- 2
  if (is.finite(cal$ratio) && cal$ratio < small_t_floor_ratio) {
    cat(sprintf(paste0(
      "  ratio < %d: the fitted heterogeneity is not clearly above the",
      " small-T\n  floor; distributional claims (sign shares, residual-SD",
      " magnitudes) are\n  not supported at this design.\n"),
      small_t_floor_ratio))
  } else {
    cat(sprintf(
      "  ratio >= %d: fitted heterogeneity sits above the small-T floor.\n",
      small_t_floor_ratio))
  }
  cat("  descriptive legacy diagnostic only; not a paperps reporting gate\n")
  invisible(NULL)
}

#' Summary of an scmix fit
#'
#' Prints the fit dimensions, integration rule, optimization diagnostics, and
#' residual standard deviations. The `calibrate` argument is retained only for
#' backward compatibility with an exploratory truth-zero refit diagnostic; it
#' is not one of the paper's reporting gates.
#'
#' @param object An `scmix` object.
#' @param calibrate Logical: run the legacy exploratory
#'   [scmix_calibrate_zero()] diagnostic (default `FALSE`; costs `R` refits).
#' @param R,seed Forwarded to [scmix_calibrate_zero()] when
#'   `calibrate = TRUE`.
#' @param ... Unused.
#' @return The fit, invisibly, with a legacy `$zero_floor` component when
#'   `calibrate = TRUE`.
#' @export
summary.scmix <- function(object, calibrate = FALSE, R = 2L, seed = 1L, ...) {
  if (isTRUE(calibrate) && is.null(object$zero_floor)) {
    warning("summary.scmix(): zero-floor calibration is a legacy exploratory ",
            "diagnostic, not a paperps reporting or inference gate.",
            call. = FALSE)
    object$zero_floor <- scmix_calibrate_zero(object, R = R, seed = seed)
  }
  ## print.scmix() already calls .scmix_print_floor() unconditionally, so
  ## no separate call is needed here (it would double-print the status
  ## line whenever calibrate = TRUE).
  print.scmix(object)
  invisible(object)
}
