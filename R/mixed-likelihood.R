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
                                              a_bound, weight_bound,
                                              alpha_bound = 5) {
  if (!is.numeric(a_bound) || length(a_bound) != 1L ||
      !is.finite(a_bound) || a_bound <= 0) {
    stop("`a_bound` must be one finite strictly positive raw-unit Frobenius bound.")
  }
  if (!is.numeric(weight_bound) || length(weight_bound) != 1L ||
      !is.finite(weight_bound) || weight_bound <= 0) {
    stop("`weight_bound` must be one finite strictly positive coordinate bound.")
  }
  if (!is.numeric(alpha_bound) || length(alpha_bound) != 1L ||
      !is.finite(alpha_bound) || alpha_bound <= 0) {
    stop("`alpha_bound` must be one finite strictly positive raw-coordinate bound.")
  }
  if (is.null(coefficient_scale)) coefficient_scale <- rep(1, p)
  coefficient_scale <- as.numeric(coefficient_scale)
  if (length(coefficient_scale) != p || any(!is.finite(coefficient_scale)) ||
      any(coefficient_scale <= 0)) {
    stop("`coefficient_scale` must contain one finite positive scale per coefficient.")
  }
  list(a_bound = as.numeric(a_bound),
       alpha_bound = as.numeric(alpha_bound),
       weight_bound = as.numeric(weight_bound),
       coefficient_scale = coefficient_scale)
}

.sc_mixed_mean_family <- function(mean_family = "legacy", hidden = integer()) {
  if (!is.character(mean_family) || length(mean_family) != 1L ||
      is.na(mean_family)) {
    stop("`mean_family` must be one of 'legacy', 'constant', 'linear', or 'relu'.")
  }
  mean_family <- match.arg(mean_family,
                           c("legacy", "constant", "linear", "relu"))
  hidden <- as.integer(hidden)
  if (anyNA(hidden) || any(hidden < 1L)) {
    stop("Every supplied hidden width must be a positive integer.")
  }
  if (mean_family %in% c("legacy", "relu") && !length(hidden)) {
    stop("The legacy and ReLU mean families require a positive-integer hidden architecture.")
  }
  if (mean_family %in% c("constant", "linear") && length(hidden)) {
    stop("The constant and linear mean families require `hidden = integer()`.")
  }
  list(mean_family = mean_family, hidden = hidden)
}

#' Project computational parameters onto the artificial compact sets
#'
#' `A` is optimized on standardized-contrast coordinates.  Dividing its rows
#' by `coefficient_scale` recovers raw coefficient units, so the loading
#' projection is with respect to that raw-unit Frobenius norm. In the corrected
#' families, `alpha_raw / coefficient_scale` is the unpenalized conditional
#' mean at the training-coded origin and is projected coordinatewise in raw
#' coefficient units. In corrected linear and ReLU families, each row of the
#' deviation output map is also a coefficient-scale coordinate. Its optimizer
#' bound is therefore `coefficient_scale[j] * weight_bound`, which represents
#' the same raw output-map bound in every equivalent contrast preprocessing.
#' Hidden weights and biases retain the scalar `weight_bound`. The legacy
#' family retains its original all-mean-parameter projection solely for
#' version-1 reproducibility.
#'
#' @keywords internal
#' @noRd
.sc_mixed_project_parameters <- function(net, coefficient_scale,
                                         a_bound, weight_bound,
                                         alpha_bound = 5) {
  compact <- .sc_mixed_validate_compact_bounds(
    p = net$p_beta, coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound,
    alpha_bound = alpha_bound)
  excluded <- c("A", "kappa_raw", "alpha_raw")
  sieve_names <- setdiff(names(net$parameters), excluded)
  corrected_output <- !is.null(net$mean_family) &&
    !identical(net$mean_family, "legacy") &&
    "param_layer.weight" %in% sieve_names
  torch::with_no_grad({
    for (nm in sieve_names) {
      if (corrected_output && identical(nm, "param_layer.weight")) {
        row_bound <- torch::torch_tensor(
          compact$coefficient_scale * compact$weight_bound,
          dtype = net$parameters[[nm]]$dtype,
          device = net$parameters[[nm]]$device)$unsqueeze(2L)
        net$parameters[[nm]]$copy_(torch::torch_maximum(
          torch::torch_minimum(net$parameters[[nm]], row_bound),
          -row_bound))
      } else {
        net$parameters[[nm]]$clamp_(min = -compact$weight_bound,
                                    max = compact$weight_bound)
      }
    }
    if ("alpha_raw" %in% names(net$parameters)) {
      scale_t <- torch::torch_tensor(
        compact$coefficient_scale,
        dtype = net$alpha_raw$dtype, device = net$alpha_raw$device)
      lower <- -compact$alpha_bound * scale_t
      upper <- compact$alpha_bound * scale_t
      net$alpha_raw$copy_(torch::torch_maximum(
        torch::torch_minimum(net$alpha_raw, upper), lower))
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
                                        alpha_bound = 5,
                                        activity_fraction = 0.99) {
  compact <- .sc_mixed_validate_compact_bounds(
    p = net$p_beta, coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound,
    alpha_bound = alpha_bound)
  sieve_names <- setdiff(names(net$parameters),
                         c("A", "kappa_raw", "alpha_raw"))
  corrected_output <- !is.null(net$mean_family) &&
    !identical(net$mean_family, "legacy")
  weight_max <- if (length(sieve_names)) {
    max(vapply(sieve_names, function(nm) {
      value <- .scmix_parameter_array(net$parameters[[nm]])
      if (corrected_output && identical(nm, "param_layer.weight")) {
        value <- sweep(as.matrix(value), 1L,
                       compact$coefficient_scale, `/`)
      }
      max(abs(value), 0)
    }, numeric(1L)))
  } else 0
  alpha_raw <- if ("alpha_raw" %in% names(net$parameters)) {
    as.numeric(torch::as_array(net$alpha_raw$detach()$cpu())) /
      compact$coefficient_scale
  } else numeric()
  alpha_max <- if (length(alpha_raw)) max(abs(alpha_raw)) else 0
  a_raw_norm <- if (net$q > 0L) {
    A_internal <- as.matrix(torch::as_array(net$A$detach()$cpu()))
    sqrt(sum(sweep(A_internal, 1L, compact$coefficient_scale, `/`)^2))
  } else 0
  mu_max_internal <- if (length(mu)) max(abs(mu)) else 0
  mu_raw <- if (length(mu)) {
    sweep(as.matrix(mu), 2L, compact$coefficient_scale, `/`)
  } else matrix(numeric(), 0L, net$p_beta)
  mu_max_raw <- if (length(mu_raw)) max(abs(mu_raw)) else 0
  ## Live modules created before `mean_family` was introduced have no such
  ## field and must retain the legacy standardized-coordinate interpretation.
  corrected_mean <- !is.null(net$mean_family) &&
    !identical(net$mean_family, "legacy")
  mu_max_for_gate <- if (corrected_mean) mu_max_raw else mu_max_internal
  list(
    mu = mu_bound,
    kappa = kappa_bound,
    a = compact$a_bound,
    alpha = compact$alpha_bound,
    weight = compact$weight_bound,
    a_units = "raw-coefficient Frobenius norm",
    alpha_units = "raw coefficient at training-coded moderator origin",
    weight_units = "coordinatewise network parameter",
    mu_max = mu_max_for_gate,
    mu_max_internal = mu_max_internal,
    mu_max_abs_raw = mu_max_raw,
    mu_units = if (corrected_mean) "raw coefficient" else
      "legacy standardized-contrast coefficient",
    kappa_abs = abs(kappa),
    a_raw_frobenius = a_raw_norm,
    alpha_max_abs_raw = alpha_max,
    weight_max_abs = weight_max,
    mu_active = mu_max_for_gate >= activity_fraction * mu_bound,
    kappa_active = abs(kappa) >= activity_fraction * kappa_bound,
    a_active = a_raw_norm >= activity_fraction * compact$a_bound,
    alpha_active = alpha_max >= activity_fraction * compact$alpha_bound,
    alpha_diagnostics_applicable = length(alpha_raw) > 0L,
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
                                    mean_family = "legacy",
                                    mu_bound = 10, kappa_bound = 10,
                                    alpha_bound = 5,
                                    a_bound = 10, weight_bound = 10,
                                    coefficient_scale = rep(1, p),
                                    compute_dtype = c("float64", "float32")) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_build_mixed_network(): the 'torch' package is required.")
  }
  compute_dtype <- match.arg(compute_dtype)
  family <- .sc_mixed_mean_family(mean_family, hidden)
  hidden <- family$hidden
  mean_family <- family$mean_family
  p <- as.integer(p); p_Z <- as.integer(p_Z); q <- as.integer(q)
  compact <- .sc_mixed_validate_compact_bounds(
    p = p, coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound,
    alpha_bound = alpha_bound)
  if (!is.finite(mu_bound) || mu_bound <= 0) {
    stop(".sc_build_mixed_network(): `mu_bound` must be positive and finite.")
  }
  if (!is.finite(kappa_bound) || kappa_bound <= 0) {
    stop(".sc_build_mixed_network(): `kappa_bound` must be positive and finite.")
  }
  if (mean_family != "legacy" && compact$alpha_bound >= mu_bound) {
    stop(".sc_build_mixed_network(): corrected families require ",
         "`alpha_bound < mu_bound` in raw coefficient units.")
  }

  generator <- torch::nn_module(
    "MixedConjointDNN",
    initialize = function() {
      self$p_beta <- p
      self$q <- q
      self$mean_family <- mean_family
      self$mu_bound <- mu_bound
      self$kappa_bound <- kappa_bound
      self$coefficient_scale <- compact$coefficient_scale
      if (mean_family != "legacy") {
        ## DeltaX is divided by coefficient_scale for optimization, hence the
        ## internal coefficient is beta-dagger = D beta. Coordinatewise
        ## internal clipping at mu_bound * D preserves one common raw-unit box
        ## |beta_j| <= mu_bound in every fold.
        self$register_buffer(
          "mu_bound_internal",
          torch::torch_tensor(mu_bound * compact$coefficient_scale,
                              dtype = torch::torch_float64()))
      }
      layers <- list()
      in_dim <- p_Z
      if (mean_family %in% c("legacy", "relu")) {
        for (i in seq_along(hidden)) {
          layers[[paste0("hidden_", i)]] <- torch::nn_linear(in_dim, hidden[i])
          in_dim <- hidden[i]
        }
      }
      self$hidden <- torch::nn_module_list(layers)
      if (mean_family == "legacy") {
        self$param_layer <- torch::nn_linear(in_dim, p)
      } else {
        self$alpha_raw <- torch::nn_parameter(torch::torch_zeros(p))
        if (mean_family == "linear") {
          self$param_layer <- torch::nn_linear(p_Z, p, bias = FALSE)
        } else if (mean_family == "relu") {
          ## No output bias: centering g(z)-g(0) would cancel it and leave a
          ## null computational coordinate.
          self$param_layer <- torch::nn_linear(in_dim, p, bias = FALSE)
        }
      }
      self$kappa_raw <- torch::nn_parameter(torch::torch_zeros(1L))
      if (q > 0L) {
        scale_t <- torch::torch_tensor(compact$coefficient_scale,
                                       dtype = torch::torch_float64())$unsqueeze(2L)
        self$A <- torch::nn_parameter(
          torch::torch_randn(p, q) * a_init_sd * scale_t
        )
      }
    },
    get_beta = function(z) {
      ## New mixed-logit fits are optimized in float64. Casting here also lets
      ## diagnostic callers pass ordinary float32 tensors without creating a
      ## silent mixed-dtype matrix multiplication.
      z <- z$to(dtype = self$kappa_raw$dtype, device = self$kappa_raw$device)
      if (self$mean_family == "constant") {
        alpha <- self$alpha_raw$unsqueeze(1L)$expand(c(z$shape[[1L]],
                                                       self$p_beta))
        upper <- self$mu_bound_internal$unsqueeze(1L)
        return(torch::torch_maximum(
          torch::torch_minimum(alpha, upper), -upper))
      }
      h <- z
      for (i in seq_along(self$hidden)) {
        h <- torch::nnf_relu(self$hidden[[i]](h))
      }
      if (self$mean_family == "legacy") {
        raw <- self$param_layer(h)
        return(self$mu_bound * torch::torch_tanh(raw / self$mu_bound))
      }
      deviation <- if (self$mean_family == "linear") {
        self$param_layer(z)
      } else {
        z0 <- torch::torch_zeros_like(z)
        h0 <- z0
        for (i in seq_along(self$hidden)) {
          h0 <- torch::nnf_relu(self$hidden[[i]](h0))
        }
        self$param_layer(h) - self$param_layer(h0)
      }
      alpha <- self$alpha_raw$unsqueeze(1L)$expand(c(z$shape[[1L]],
                                                     self$p_beta))
      upper <- self$mu_bound_internal$unsqueeze(1L)
      torch::torch_maximum(torch::torch_minimum(alpha + deviation, upper),
                           -upper)
    },
    get_kappa = function() {
      self$kappa_bound * torch::torch_tanh(self$kappa_raw / self$kappa_bound)
    }
  )
  net <- generator()
  ## Protocol v2.2 requires every stochastic stage and the final full-batch
  ## polish to use double precision. This converts linear-layer parameters as
  ## well as registered buffers before the first objective evaluation.
  net$to(dtype = if (compute_dtype == "float64") {
    torch::torch_float64()
  } else {
    torch::torch_float32()
  })
  .sc_mixed_project_parameters(
    net, coefficient_scale = compact$coefficient_scale,
    alpha_bound = compact$alpha_bound,
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
  ## Corrected families separate the compact common reference alpha from the
  ## moderator deviation. Alpha is optimized and audited but never penalized.
  ## Legacy modules have no alpha_raw and therefore reproduce the version-1
  ## all-mean-parameter penalty exactly.
  unpenalized <- c("A", "kappa_raw", "alpha_raw")
  nms <- setdiff(names(net$parameters), unpenalized)
  if (length(nms) == 0L || weight_decay == 0) {
    return(torch::torch_zeros(1L, dtype = net$kappa_raw$dtype,
                              device = net$kappa_raw$device))
  }
  out <- torch::torch_zeros(1L, dtype = net$kappa_raw$dtype,
                            device = net$kappa_raw$device)
  corrected_output <- !is.null(net$mean_family) &&
    !identical(net$mean_family, "legacy")
  for (nm in nms) {
    value <- net$parameters[[nm]]
    if (corrected_output && identical(nm, "param_layer.weight")) {
      scale_t <- torch::torch_tensor(
        net$coefficient_scale,
        dtype = value$dtype, device = value$device)$unsqueeze(2L)
      value <- value / scale_t
    }
    out <- out + torch::torch_sum(value^2)
  }
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
  alpha_names <- intersect(names(vals), "alpha_raw")
  sieve_names <- setdiff(names(vals), c(structural_names, alpha_names))
  group_max <- function(nms) {
    if (!length(nms)) 0 else max(vals[nms], 0)
  }
  list(
    by_parameter = vals,
    total = max(vals, 0),
    structural = group_max(structural_names),
    alpha = group_max(alpha_names),
    sieve = group_max(sieve_names),
    deviation = group_max(sieve_names),
    structural_parameters = structural_names,
    alpha_parameters = alpha_names,
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
  alpha_stationarity_met <- gradients_finite &&
    (is.null(gradient$alpha) || gradient$alpha <= grad_tol)
  sieve_stationarity_met <- gradients_finite && gradient$sieve <= grad_tol
  required_bound_flags <- c("mu_active", "kappa_active", "a_active",
                            "weight_active")
  if (isTRUE(bounds$alpha_diagnostics_applicable)) {
    required_bound_flags <- c(required_bound_flags, "alpha_active")
  }
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
    alpha_stationarity_met = alpha_stationarity_met,
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
                                mean_family = "legacy",
                                n_epochs = 400L,
                                learning_rate = 0.01,
                                weight_decay = 1e-4,
                                seed = NULL,
                                device = "cpu",
                                verbose = FALSE,
                                warm_state = NULL,
                                warm_start_mode = c("mean_only", "pooled_nested",
                                                    "pooled_structural"),
                                early_stop = FALSE,
                                val_frac = 0.1,
                                check_every = 20L,
                                patience = 3L,
                                opt_tol = 1e-7,
                                grad_tol = 1e-4,
                                kkt_tol = NULL,
                                protocol_mode = c("calibration",
                                                  "production"),
                                calibration_verification = NULL,
                                polish_max_iter = 50L,
                                polish_history_size = 50L,
                                polish_tolerance_grad = 1e-10,
                                polish_tolerance_change = 1e-12,
                                polish_lr = 1,
                                mu_bound = 10,
                                kappa_bound = 10,
                                alpha_bound = 5,
                                a_bound = 10,
                                weight_bound = 10,
                                coefficient_scale = rep(1, ncol(deltaX))) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(".sc_train_mixed_one(): the 'torch' package is required.")
  }
  warm_start_mode <- match.arg(warm_start_mode)
  protocol_mode <- .sc_v22_protocol_mode(protocol_mode)
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
  ## A numerical value alone never establishes the v2.2 cutoff provenance.
  ## Public entry points attach the protocol-linked verification result. The
  ## fallback labels below cover only direct internal diagnostic calls.
  kkt_tol_source <- calibration_verification$status %||%
    if (identical(protocol_mode, "calibration")) {
      "calibration_only"
    } else {
      "unverified_production_cutoff"
    }
  if (!is.null(kkt_tol) &&
      (!is.numeric(kkt_tol) || length(kkt_tol) != 1L ||
       !is.finite(kkt_tol) || kkt_tol < 0)) {
    stop(".sc_train_mixed_one(): `kkt_tol` must be NULL or finite and nonnegative.")
  }
  compact <- .sc_mixed_validate_compact_bounds(
    p = ncol(deltaX), coefficient_scale = coefficient_scale,
    a_bound = a_bound, weight_bound = weight_bound,
    alpha_bound = alpha_bound)
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
                               dtype = torch::torch_float64(), device = dev),
      zt = torch::torch_tensor(Z[rows, , drop = FALSE],
                               dtype = torch::torch_float64(), device = dev),
      yt = torch::torch_tensor(as.numeric(y[rows]),
                               dtype = torch::torch_float64(), device = dev),
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
    torch::torch_zeros(1L, 1L, dtype = torch::torch_float64(), device = dev)
  } else {
    torch::torch_tensor(gh$U, dtype = torch::torch_float64(), device = dev)
  }
  logw_t <- torch::torch_tensor(log(gh$w), dtype = torch::torch_float64(),
                                device = dev)

  net <- .sc_build_mixed_network(
    p = ncol(deltaX), p_Z = ncol(Z), q = q, hidden = hidden,
    mean_family = mean_family,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    alpha_bound = compact$alpha_bound,
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
        use <- if (warm_start_mode == "pooled_structural") {
          nm %in% c("A", "kappa_raw", "alpha_raw")
        } else {
          warm_start_mode == "pooled_nested" ||
            !nm %in% c("A", "kappa_raw")
        }
        if (use) own[[nm]] <- warm_state[[nm]]
      }
      net$load_state_dict(own)
      if (warm_start_mode == "pooled_nested") {
        torch::with_no_grad({
          deviation <- setdiff(names(net$parameters),
                               c("A", "kappa_raw", "alpha_raw"))
          for (nm in deviation) net$parameters[[nm]]$zero_()
        })
      } else if (warm_start_mode == "pooled_structural") {
        ## Retain randomized hidden layers but start at the pooled function by
        ## zeroing only the deviation output map. Its first gradient is then
        ## generally nonzero, unlike an all-zero multilayer ReLU network.
        torch::with_no_grad({
          if ("param_layer.weight" %in% names(net$parameters)) {
            net$parameters[["param_layer.weight"]]$zero_()
          }
        })
      }
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
    alpha_bound = compact$alpha_bound,
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
  last_finite_checkpoint <- .scmix_clone_network_state(net)
  checkpoint_epoch <- 0L

  for (epoch in seq_len(n_epochs)) {
    net$train()
    optimizer$zero_grad()
    nll <- .sc_mixed_nll(net, tr$dx, tr$zt, tr$yt, U_t, logw_t,
                         tr$idx1, tr$N)
    loss <- nll + .sc_mixed_penalty(net, weight_decay)
    loss$backward()
    grad_now <- .sc_mixed_gradient_norm(net)
    loss_now <- as.numeric(loss$item())
    nll_now <- as.numeric(nll$item())
    if (!is.finite(loss_now) || !is.finite(nll_now) ||
        !is.finite(grad_now)) {
      .scmix_load_network_state(net, last_finite_checkpoint)
      stopped_at <- checkpoint_epoch
      stop_reason <- "adam_nonfinite_state_restored"
      break
    }
    optimizer$step()
    .sc_mixed_project_parameters(
      net, coefficient_scale = compact$coefficient_scale,
      alpha_bound = compact$alpha_bound,
      a_bound = compact$a_bound, weight_bound = compact$weight_bound
    )
    loss_trace[epoch] <- loss_now
    nll_trace[epoch] <- nll_now
    grad_trace[epoch] <- grad_now

    if (epoch %% check_every == 0L) {
      if (.scmix_network_all_finite(net)) {
        last_finite_checkpoint <- .scmix_clone_network_state(net)
        checkpoint_epoch <- epoch
      } else {
        .scmix_load_network_state(net, last_finite_checkpoint)
        stopped_at <- checkpoint_epoch
        stop_reason <- "adam_nonfinite_state_restored"
        break
      }
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
          best_state <- .scmix_clone_network_state(net)
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
  if (!.scmix_network_all_finite(net)) {
    .scmix_load_network_state(net, last_finite_checkpoint)
    stopped_at <- checkpoint_epoch
    stop_reason <- "adam_nonfinite_state_restored"
  }
  adam_state_restored <- identical(stop_reason,
                                   "adam_nonfinite_state_restored")
  validation_state_restored <- isTRUE(early_stop) && !is.null(best_state)
  if (validation_state_restored) {
    .scmix_load_network_state(net, best_state)
  }
  state_restored <- adam_state_restored || validation_state_restored
  .sc_mixed_project_parameters(
    net, coefficient_scale = compact$coefficient_scale,
    alpha_bound = compact$alpha_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound
  )

  polish <- .sc_mixed_lbfgs_polish(
    net = net, data = all_data, U_t = U_t, logw_t = logw_t,
    weight_decay = weight_decay,
    coefficient_scale = compact$coefficient_scale,
    kappa_bound = kappa_bound, alpha_bound = compact$alpha_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound,
    max_iter = polish_max_iter, history_size = polish_history_size,
    tolerance_grad = polish_tolerance_grad,
    tolerance_change = polish_tolerance_change, lr = polish_lr,
    same_objective_all_stages = !isTRUE(early_stop))
  candidate_net <- polish$net
  fallback_net <- polish$fallback_net
  invisible(gc(verbose = FALSE))

  ## Recreate the full-batch tensors after the isolated libtorch optimizer.
  ## This also protects the returned fit on builds whose failed line search
  ## can leave tensors used by its closure in a nonfinite state.
  all_data <- mk_tensors(rep(TRUE, length(y)))
  U_t <- if (q == 0L) {
    torch::torch_zeros(1L, 1L, dtype = torch::torch_float64(), device = dev)
  } else {
    torch::torch_tensor(gh$U, dtype = torch::torch_float64(), device = dev)
  }
  logw_t <- torch::torch_tensor(log(gh$w), dtype = torch::torch_float64(),
                                device = dev)

  ## Audit the settled module after the R torch optimizer and closure have
  ## left scope. The binding may finalize strong-Wolfe trial storage only at
  ## that boundary, so this is the authoritative post-polish objective/KKT.
  settled_candidate <- .sc_mixed_evaluate_full_objective(
    candidate_net, all_data, U_t, logw_t, weight_decay,
    compact$coefficient_scale, kappa_bound, compact$alpha_bound,
    compact$a_bound, compact$weight_bound, backward = TRUE)
  settled_nonworsening <- is.finite(settled_candidate$objective) &&
    settled_candidate$objective <= polish$objective_before_polish +
      polish$nonworsening_numerical_allowance
  settled_accepted <- isTRUE(polish$completed) &&
    identical(polish$projection_changed, FALSE) && settled_nonworsening
  net <- if (settled_accepted) candidate_net else fallback_net
  if (!settled_accepted) {
    all_data <- mk_tensors(rep(TRUE, length(y)))
    settled_returned <- .sc_mixed_evaluate_full_objective(
      net, all_data, U_t, logw_t, weight_decay,
      compact$coefficient_scale, kappa_bound, compact$alpha_bound,
      compact$a_bound, compact$weight_bound, backward = TRUE)
  } else {
    settled_returned <- settled_candidate
  }
  polish$accepted <- settled_accepted
  polish$lbfgs_nonworsening <- settled_nonworsening
  polish$settled_state_status <- if (settled_accepted) {
    "accepted_after_optimizer_finalization"
  } else if (!isTRUE(polish$completed)) {
    "optimizer_failed_and_prepolish_state_returned"
  } else if (isTRUE(polish$projection_changed)) {
    "endpoint_required_projection_and_prepolish_state_returned"
  } else {
    "settled_endpoint_worsened_or_nonfinite_and_prepolish_state_returned"
  }
  polish$objective_after_polish <- settled_candidate$objective
  polish$unpenalized_objective_after_polish <-
    settled_candidate$unpenalized
  polish$kkt_after_polish <- settled_candidate$kkt
  polish$objective_returned <- settled_returned$objective
  polish$kkt_returned <- settled_returned$kkt
  polish$absolute_gradient_returned <- settled_returned$gradient
  polish$returned_matches_accepted_objective <-
    !settled_accepted || isTRUE(all.equal(
      settled_returned$objective, settled_candidate$objective,
      tolerance = polish$nonworsening_numerical_allowance,
      check.attributes = FALSE))
  polish$net <- NULL
  polish$fallback_net <- NULL

  ## Re-evaluate the returned state on every respondent supplied to this
  ## start, so starts remain comparable even when validation stopping is used.
  net$eval()
  net$zero_grad()
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
  origin_mu <- .sc_predict_beta(net, matrix(0, nrow = 1L, ncol = ncol(Z)))
  deviation_from_origin_max <- if (nrow(mu_train)) {
    max(abs(sweep(mu_train, 2L, origin_mu[1L, ], `-`)))
  } else 0
  mean_variation_max <- if (nrow(mu_train) > 1L) {
    max(apply(mu_train, 2L, function(x) diff(range(x))))
  } else 0
  bounds <- .sc_mixed_bound_diagnostics(
    net = net, mu = mu_train, kappa = kappa_hat,
    coefficient_scale = compact$coefficient_scale,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    alpha_bound = compact$alpha_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound
  )
  status <- .sc_mixed_v22_optimization_status(
    polish = polish, bounds = bounds, kkt_cutoff = kkt_tol,
    protocol_mode = protocol_mode,
    calibration_verified = isTRUE(calibration_verification$verified),
    calibration_failure_reason =
      calibration_verification$failure_reason %||% character())
  legacy_compatibility_gate <- identical(mean_family, "legacy") &&
    isTRUE(status$objective_finite)
  operational_gate <- if (identical(mean_family, "legacy")) {
    legacy_compatibility_gate
  } else status$precalibration_fit_valid
  operational_failure_reasons <- if (identical(mean_family, "legacy")) {
    c(if (!isTRUE(status$objective_finite)) "nonfinite_objective",
      if (!isTRUE(status$gate_evidence$F003$pass))
        "nonfinite_derivatives_v22_only")
  } else status$precalibration_failure_reasons
  last_relative_change <- abs(polish$objective_after_polish -
                                polish$objective_before_polish) /
    max(1, abs(polish$objective_before_polish))
  ## Retain the returned module in `net`, but do not duplicate an external
  ## pointer inside the serializable polish audit.
  polish$net <- NULL

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
    alpha_gradient_norm = final_gradient$alpha,
    sieve_gradient_norm = final_gradient$sieve,
    ## Step 3 must be able to generate fits before its KKT cutoff exists.
    ## `optimization_gate_pass` therefore records pre-calibration numerical
    ## validity for corrected families and a narrow legacy-reproducibility
    ## gate for old families. Only `v22_optimization_gate_pass` grants paper
    ## eligibility.
    converged = operational_gate,
    stationarity_met = status$stationarity_met,
    structural_stationarity_met = status$structural_stationarity_met,
    alpha_stationarity_met = status$alpha_stationarity_met,
    sieve_stationarity_met = status$sieve_stationarity_met,
    last_relative_change = last_relative_change,
    criterion_tolerance_met = isTRUE(polish$completed) &&
      isTRUE(polish$lbfgs_nonworsening),
    criterion_diagnostic_source =
      "same full-batch objective before and after L-BFGS polish",
    state_restored = state_restored,
    objective_components = c(
      penalized_nll = final_penalized_loss,
      unpenalized_nll = final_nll,
      penalty = final_penalized_loss - final_nll),
    objective_finite = status$objective_finite,
    bound_diagnostics_complete = status$bound_diagnostics_complete,
    bound_activity = status$bound_activity,
    optimization_gate_pass = operational_gate,
    optimization_failure_reasons = unique(operational_failure_reasons),
    legacy_compatibility_gate = legacy_compatibility_gate,
    v22_optimization_gate_pass = status$optimization_gate_pass,
    v22_optimization_failure_reasons = status$failure_reasons,
    protocol_mode = protocol_mode,
    calibration_only = identical(protocol_mode, "calibration"),
    calibration_verification = calibration_verification,
    v22_optimizer_subset_pass = isTRUE(status$optimizer_subset_pass),
    stop_reason = paste0(stop_reason, if (isTRUE(polish$accepted)) {
      "+lbfgs_polish"
    } else "+lbfgs_polish_rejected"),
    early_stop = early_stop,
    warm_start_mode = warm_start_mode,
    mean_family = mean_family,
    alpha_bound = compact$alpha_bound,
    deviation_from_origin_max = deviation_from_origin_max,
    mean_variation_max = mean_variation_max,
    dtype = polish$dtype,
    polish = polish,
    objective_before_polish = polish$objective_before_polish,
    objective_after_polish = polish$objective_after_polish,
    kkt_before_polish = polish$kkt_before_polish,
    kkt_after_polish = polish$kkt_after_polish,
    kkt_returned = polish$kkt_returned,
    kkt_maximum = status$kkt_maximum,
    kkt_cutoff = status$kkt_cutoff,
    kkt_cutoff_source = kkt_tol_source,
    lbfgs_completed = status$lbfgs_completed,
    lbfgs_nonworsening = status$lbfgs_nonworsening,
    polish_iterations = polish$iterations,
    line_search_status = polish$line_search_status,
    same_objective_all_stages = status$same_objective_all_stages,
    v22_optimization_status = status,
    A = A_hat,
    kappa = kappa_hat,
    bounds = bounds
  )
}

#' Fit multiple starts and retain the highest attained penalized objective
#' @keywords internal
#' @noRd
.sc_train_mixed_multistart <- function(..., n_starts = 2L, seed = NULL,
                                       warm_state = NULL,
                                       warm_start_mode = c(
                                         "mean_only", "pooled_nested",
                                         "pooled_structural")) {
  if (!is.numeric(n_starts) || length(n_starts) != 1L || is.na(n_starts) ||
      n_starts < 1L || n_starts != as.integer(n_starts)) {
    stop(".sc_train_mixed_multistart(): `n_starts` must be positive.")
  }
  n_starts <- as.integer(n_starts)
  warm_start_mode <- match.arg(warm_start_mode)
  fits <- vector("list", n_starts)
  for (s in seq_len(n_starts)) {
    seed_s <- if (is.null(seed)) NULL else {
      out <- (as.double(seed) + 104729 * (s - 1L)) %% (.Machine$integer.max - 1)
      as.integer(out + 1)
    }
    warm_s <- if (is.null(warm_state) ||
        (s > 1L && warm_start_mode == "mean_only")) NULL else warm_state
    mode_s <- if (s > 1L && warm_start_mode == "pooled_nested") {
      ## Start one is the exact zero-deviation pooled fallback. Additional
      ## starts inherit only its structural coordinates while retaining their
      ## random, trainable deviation networks. This avoids the all-zero ReLU
      ## saddle without sacrificing exact nesting of the attained candidate.
      "pooled_structural"
    } else warm_start_mode
    fits[[s]] <- .sc_train_mixed_one(
      ..., seed = seed_s, warm_state = warm_s,
      warm_start_mode = mode_s)
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
    projected_kkt_maximum = vapply(fits, `[[`, numeric(1L), "kkt_maximum"),
    kkt_cutoff = vapply(fits, `[[`, numeric(1L), "kkt_cutoff"),
    dtype = vapply(fits, `[[`, character(1L), "dtype"),
    lbfgs_completed = vapply(fits, `[[`, logical(1L), "lbfgs_completed"),
    lbfgs_nonworsening = vapply(fits, `[[`, logical(1L),
                                "lbfgs_nonworsening"),
    polish_iterations = vapply(fits, function(x) {
      as.integer(x$polish_iterations)
    }, integer(1L)),
    line_search_status = vapply(fits, `[[`, character(1L),
                                "line_search_status"),
    structural_gradient_norm = vapply(fits, `[[`, numeric(1L),
                                        "structural_gradient_norm"),
    alpha_gradient_norm = vapply(fits, function(x) {
      as.numeric(x$alpha_gradient_norm %||% 0)
    }, numeric(1L)),
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
    v22_optimization_gate_pass = vapply(fits, `[[`, logical(1L),
                                           "v22_optimization_gate_pass"),
    protocol_mode = vapply(fits, `[[`, character(1L), "protocol_mode"),
    converged = vapply(fits, `[[`, logical(1L), "converged"),
    epochs = vapply(fits, `[[`, integer(1L), "stopped_at"),
    stop_reason = vapply(fits, `[[`, character(1L), "stop_reason"),
    warm_start_mode = vapply(fits, `[[`, character(1L), "warm_start_mode"),
    deviation_from_origin_max = vapply(
      fits, `[[`, numeric(1L), "deviation_from_origin_max"),
    mean_variation_max = vapply(fits, `[[`, numeric(1L),
                                  "mean_variation_max"),
    stringsAsFactors = FALSE
  )
  ## Preserve every polished start in ordinary CPU arrays. F008 is target
  ## specific, so its statistic is computed later from registered targets,
  ## but those computations must not require rerunning or trusting a
  ## user-supplied description of the nonselected solutions.
  ans$start_artifacts <- lapply(seq_along(fits), function(s) {
    fit_s <- fits[[s]]
    seed_s <- if (is.null(seed)) NULL else {
      out <- (as.double(seed) + 104729 * (s - 1L)) %%
        (.Machine$integer.max - 1)
      as.integer(out + 1)
    }
    live <- fit_s$net$state_dict()
    state <- lapply(live, .scmix_tensor_cpu_array)
    names(state) <- names(live)
    shapes <- lapply(live, function(x) as.integer(x$shape))
    dtypes <- lapply(live, function(x) as.character(x$dtype))
    names(shapes) <- names(dtypes) <- names(live)
    start_id <- sprintf("start_%03d", s)
    state_hash <- .scmix_stable_state_hash(list(
      state_dict = state, A_optimization = fit_s$A,
      kappa = fit_s$kappa))
    optimizer_evidence <- list(
      protocol_id = fit_s$polish$protocol_id,
      protocol_mode = fit_s$protocol_mode,
      calibration_hash = fit_s$calibration_verification$calibration_hash %||%
        NA_character_,
      objective = list(
        before_polish = fit_s$polish$objective_before_polish,
        after_polish = fit_s$polish$objective_after_polish,
        returned = fit_s$polish$objective_returned,
        unpenalized_before_polish =
          fit_s$polish$unpenalized_objective_before_polish,
        unpenalized_after_polish =
          fit_s$polish$unpenalized_objective_after_polish,
        nonworsening_numerical_allowance =
          fit_s$polish$nonworsening_numerical_allowance,
        definition = fit_s$polish$objective_definition,
        definition_id = fit_s$polish$objective_definition_id),
      polish = list(
        method = fit_s$polish$method,
        line_search = fit_s$polish$line_search,
        method_id = fit_s$polish$optimization_method_id,
        line_search_status = fit_s$polish$line_search_status,
        completed = isTRUE(fit_s$polish$completed),
        accepted = isTRUE(fit_s$polish$accepted),
        projection_changed = fit_s$polish$projection_changed,
        nonworsening = isTRUE(fit_s$polish$lbfgs_nonworsening),
        same_objective_all_stages =
          isTRUE(fit_s$polish$same_objective_all_stages),
        returned_matches_accepted_objective =
          isTRUE(fit_s$polish$returned_matches_accepted_objective),
        iterations = fit_s$polish$iterations,
        function_evaluations = fit_s$polish$function_evaluations,
        closure_calls = fit_s$polish$closure_calls),
      bounds = fit_s$bounds,
      kkt = list(
        before_polish = fit_s$polish$kkt_before_polish,
        after_polish = fit_s$polish$kkt_after_polish,
        returned = fit_s$polish$kkt_returned),
      gate_evidence = fit_s$v22_optimization_status$gate_evidence,
      gate_failure_reasons =
        fit_s$v22_optimization_status$failure_reasons)
    list(
      start = as.integer(s),
      start_id = start_id,
      seed = seed_s,
      state_hash = state_hash,
      selected = identical(as.integer(s), as.integer(best)),
      mean_objective = as.numeric(fit_s$penalized_loss),
      objective_definition_id = paste0(
        "mean_negative_complete_sequence_loglik_per_respondent_",
        "plus_declared_penalty_in_nats"),
      unpenalized_mean_nll = as.numeric(fit_s$final_loss),
      state_dict = state,
      state_shapes = shapes,
      state_dtypes = dtypes,
      A_optimization = fit_s$A,
      kappa = fit_s$kappa,
      dtype = fit_s$dtype,
      polished = isTRUE(fit_s$lbfgs_completed) &&
        isTRUE(fit_s$polish$accepted),
      protocol_mode = fit_s$protocol_mode,
      calibration_hash = fit_s$calibration_verification$calibration_hash %||%
        NA_character_,
      optimizer_evidence = optimizer_evidence,
      v22_optimization_status = fit_s$v22_optimization_status)
  })
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
#' @param mean_family Conditional-mean parameterization. `"constant"`,
#'   `"linear"`, and `"relu"` use an unpenalized compact baseline plus a
#'   penalized deviation centered at the training-coded moderator origin.
#'   `"legacy"` reproduces the original all-parameter ReLU penalty.
#' @param n_epochs Maximum full-batch Adam epochs per optimization start.
#' @param learning_rate Adam learning rate.
#' @param weight_decay Nonnegative squared-weight penalty coefficient, or the
#'   backward-compatible `"adaptive"` rule. Paper analyses should tune a finite
#'   prespecified penalty grid inside each outer training sample.
#' @param init Optional [scfit] object used to warm start the full-sample
#'   mean trunk (architecture must match `hidden`). To prevent cross-fit
#'   leakage, this state is never used for an outer-fold fit.
#' @param n_starts Number of prespecified optimization starts per fit.
#' @param mu_bound,kappa_bound Positive finite bounds for the fitted
#'   conditional mean coordinates and position effect. In the corrected
#'   `"constant"`, `"linear"`, and `"relu"` families, `mu_bound` is a
#'   common coordinatewise bound in raw coefficient units; its training-scale
#'   representation is adjusted by each contrast scale. The `"legacy"`
#'   family retains its original scalar training-scale bound for exact
#'   backward compatibility.
#' @param alpha_bound Positive raw-coefficient coordinate bound for the
#'   unpenalized baseline in corrected mean families. It must be strictly less
#'   than `mu_bound`, so the reference mean lies in the interior of the mean
#'   output box.
#' @param a_bound Positive finite Frobenius-norm bound on the loading matrix in
#'   raw coefficient units. Training-scale loadings are projected after every
#'   optimizer update using the corresponding contrast scales.
#' @param weight_bound Positive finite coordinatewise raw-unit bound for the
#'   corrected deviation output map and optimizer-coordinate bound for hidden
#'   weights and biases. Output rows are rescaled with the training contrast
#'   scales. Network parameters are projected after every optimizer update.
#' @param opt_tol,grad_tol Legacy Adam-stage stopping controls. They do not
#'   determine the v2.2 computation gate.
#' @param kkt_tol Cutoff for the maximum block-scaled projected-KKT residual
#'   after the full-batch polish. The application protocol must supply the
#'   simulation-calibrated value once frozen. While it is `NULL`, fits may be
#'   generated for calibration but v2.2 inference eligibility fails closed.
#' @param protocol_mode Either `"calibration"` or `"production"`.
#'   Calibration fits retain every diagnostic needed by Step 3 but can
#'   never be labeled v2.2 eligible. Production is an explicit opt-in after
#'   the calibration registry and numerical cutoffs have been frozen.
#' @param calibration_artifact,analysis_contract Protocol-linked calibration
#'   inputs. Production mode accepts KKT cutoffs only after the inference
#'   contract validates this exact artifact and its calibration hash.
#' @param polish_max_iter,polish_history_size,polish_tolerance_grad,
#'   polish_tolerance_change,polish_lr Full-batch L-BFGS polish controls.
#'   The two L-BFGS tolerances control optimizer termination only. Eligibility
#'   is determined by `kkt_tol` and the recorded non-worsening gate.
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
                  mean_family = "legacy",
                  n_epochs = 400L,
                  learning_rate = 0.01,
                  weight_decay = "adaptive",
                  n_starts = 2L,
                  mu_bound = 10,
                  kappa_bound = 10,
                  alpha_bound = 5,
                  a_bound = 10,
                  weight_bound = 10,
                  opt_tol = 1e-7,
                  grad_tol = 1e-4,
                  kkt_tol = NULL,
                  protocol_mode = c("calibration", "production"),
                  calibration_artifact = NULL,
                  analysis_contract = NULL,
                  polish_max_iter = 50L,
                  polish_history_size = 50L,
                  polish_tolerance_grad = 1e-10,
                  polish_tolerance_change = 1e-12,
                  polish_lr = 1,
                  seed = NULL,
                  init = NULL,
                  device = "cpu",
                  verbose = FALSE,
                  early_stop = FALSE) {
  call <- match.call()
  protocol_mode <- .sc_v22_protocol_mode(protocol_mode)
  calibration_verification <- .sc_v22_resolve_optimizer_calibration(
    protocol_mode = protocol_mode,
    calibration_artifact = calibration_artifact,
    analysis_contract = analysis_contract,
    kkt_tol = kkt_tol)
  .sc_v22_require_verified_production_calibration(calibration_verification)
  kkt_tol_use <- calibration_verification$kkt_cutoff
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
    a_bound = a_bound, weight_bound = weight_bound,
    alpha_bound = alpha_bound
  )
  hidden_use <- if (identical(hidden, "auto")) {
    if (mean_family %in% c("constant", "linear")) integer() else
    .sc_auto_hidden(n, p)
  } else as.integer(hidden)
  family_use <- .sc_mixed_mean_family(mean_family, hidden_use)$mean_family
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
    if (family_use != "legacy") {
      stop("scmix(): `init` from scfit is supported only for the legacy mean family.")
    }
    if (!inherits(init, "sc_fit") || is.null(init$nets) || length(init$nets) == 0L) {
      warning("scmix(): `init` has no stored nets; warm start skipped.")
    } else {
      warm_state <- init$nets[[1L]]$state_dict()
      ## the scfit trunk predicts beta on the RAW deltaX scale (its default
      ## normalize_deltaX = FALSE); scmix trains on standardized contrasts,
      ## where mu_std = mu_raw * sd_dx, so rescale the output layer rows
      if ("param_layer.weight" %in% names(warm_state)) {
        ## Preserve the warm state's dtype. New v2.2 fits use Double, while
        ## legacy Float warm starts remain readable and are cast by the new
        ## module's state loader.
        warm_dtype <- warm_state[["param_layer.weight"]]$dtype
        warm_device <- warm_state[["param_layer.weight"]]$device
        sd_t <- torch::torch_tensor(sd_dx, dtype = warm_dtype,
                                    device = warm_device)
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
  A_optimization_folds <- vector("list", K)
  kappa_folds <- numeric(K)
  sd_dx_folds <- vector("list", K)
  dx_transform_folds <- vector("list", K)
  z_transform_folds <- vector("list", K)
  loss_traces <- vector("list", K)
  start_artifacts_folds <- vector("list", K)
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
      gh = gh, hidden = hidden_use, mean_family = family_use,
      n_epochs = n_epochs, learning_rate = learning_rate,
      weight_decay = wd_use,
      seed = if (is.null(seed)) NULL else .sc_fold_seed(seed, k),
      device = device, verbose = verbose,
      warm_state = NULL,
      early_stop = early_stop,
      n_starts = n_starts,
      opt_tol = opt_tol, grad_tol = grad_tol, kkt_tol = kkt_tol_use,
      protocol_mode = protocol_mode,
      calibration_verification = calibration_verification,
      polish_max_iter = polish_max_iter,
      polish_history_size = polish_history_size,
      polish_tolerance_grad = polish_tolerance_grad,
      polish_tolerance_change = polish_tolerance_change,
      polish_lr = polish_lr,
      mu_bound = mu_bound, kappa_bound = kappa_bound,
      alpha_bound = compact$alpha_bound,
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
    A_folds[[k]] <- .scmix_loading_to_raw(
      fit_k$A, sd_dx_folds[[k]])
    A_optimization_folds[[k]] <- fit_k$A
    kappa_folds[k] <- fit_k$kappa
    loss_traces[[k]] <- fit_k$loss_trace
    optimization_folds[[k]] <- .sc_comp_fit_summary(fit_k)
    start_artifacts_folds[[k]] <- fit_k$start_artifacts
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
    mean_family = family_use,
    n_epochs = n_epochs, learning_rate = learning_rate,
    weight_decay = wd_use, seed = full_seed,
    device = device, verbose = verbose,
    warm_state = warm_state, early_stop = early_stop,
    n_starts = n_starts, opt_tol = opt_tol, grad_tol = grad_tol,
    kkt_tol = kkt_tol_use,
    protocol_mode = protocol_mode,
    calibration_verification = calibration_verification,
    polish_max_iter = polish_max_iter,
    polish_history_size = polish_history_size,
    polish_tolerance_grad = polish_tolerance_grad,
    polish_tolerance_change = polish_tolerance_change,
    polish_lr = polish_lr,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    alpha_bound = compact$alpha_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound,
    coefficient_scale = sd_dx
  )
  mu_full <- sweep(.sc_predict_beta(full_fit$net, Z_full), 2L, sd_dx, `/`)
  A_full <- .scmix_loading_to_raw(full_fit$A, sd_dx)
  Sigma_hat <- tcrossprod(A_full)
  optimization_gate_by_fold <- vapply(
    optimization_folds,
    function(x) isTRUE(x$optimization_gate_pass), logical(1L))
  computational_optimization_gate_pass <-
    isTRUE(full_fit$optimization_gate_pass) && all(optimization_gate_by_fold)
  v22_optimization_gate_by_fold <- vapply(
    optimization_folds,
    function(x) isTRUE(x$v22_optimization_gate_pass), logical(1L))
  v22_computational_optimization_gate_pass <-
    isTRUE(full_fit$v22_optimization_gate_pass) &&
    all(v22_optimization_gate_by_fold)

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
      mean_family = family_use,
      hidden = hidden_use,
      integration_method = gh$metadata$method,
      qmc_antithetic = isTRUE(qmc_antithetic),
      n_epochs = as.integer(n_epochs),
      learning_rate = as.numeric(learning_rate),
      weight_decay = as.numeric(wd_use),
      n_starts = as.integer(n_starts),
      mu_bound = as.numeric(mu_bound),
      kappa_bound = as.numeric(kappa_bound),
      alpha_bound = compact$alpha_bound,
      a_bound = compact$a_bound,
      weight_bound = compact$weight_bound,
      opt_tol = as.numeric(opt_tol),
      grad_tol = as.numeric(grad_tol),
      kkt_tol = if (is.null(kkt_tol_use)) NA_real_ else
        as.numeric(kkt_tol_use),
      kkt_tol_source = calibration_verification$status,
      calibration_hash = calibration_verification$calibration_hash,
      protocol_mode = protocol_mode,
      polish_max_iter = as.integer(polish_max_iter),
      polish_history_size = as.integer(polish_history_size),
      polish_tolerance_grad = as.numeric(polish_tolerance_grad),
      polish_tolerance_change = as.numeric(polish_tolerance_change),
      polish_lr = as.numeric(polish_lr),
      dtype = "float64",
      early_stop = isTRUE(early_stop),
      init_used_for_full_fit = !is.null(warm_state)
    )
  )

  full_scope <- "full-sample structural plug-in fit"
  fold_scopes <- paste0("outer-fold nuisance fit ", seq_len(K))
  loading_invariance_full <- .scmix_loading_invariance_evidence(
    full_fit$net, deltaX, y, Z_full, resp_task, gh, sd_dx,
    analysis_signature = analysis_signature, scope = full_scope,
    protocol_mode = protocol_mode,
    calibration_verified = calibration_verification$verified,
    calibration_hash = calibration_verification$calibration_hash,
    probability_cutoff =
      calibration_verification$loading_probability_invariance_cutoff,
    device = device)
  loading_invariance_folds <- lapply(seq_len(K), function(k) {
    Z_all_k <- .sc_apply_z_transform(Z_task, z_transform_folds[[k]])
    .scmix_loading_invariance_evidence(
      nets[[k]], deltaX, y, Z_all_k, resp_task, gh, sd_dx_folds[[k]],
      analysis_signature = analysis_signature, scope = fold_scopes[k],
      protocol_mode = protocol_mode,
      calibration_verified = calibration_verification$verified,
      calibration_hash = calibration_verification$calibration_hash,
      probability_cutoff =
        calibration_verification$loading_probability_invariance_cutoff,
      device = device)
  })

  ## Torch nn_modules contain session-local external pointers and are not
  ## durable when embedded directly in an RDS file.  Retain the live modules
  ## for backward compatibility within this process, and also capture a
  ## pointer-free CPU state/architecture/transform bundle for later reload.
  network_state_full <- .scmix_capture_network_state(
    full_fit$net, p = p, p_Z = ncol(Z_task), q = q, hidden = hidden_use,
    mean_family = family_use,
    mu_bound = mu_bound, kappa_bound = kappa_bound,
    alpha_bound = compact$alpha_bound,
    a_bound = compact$a_bound, weight_bound = compact$weight_bound,
    coefficient_scale = sd_dx, z_transform = z_transform_full,
    dx_transform = dx_transform_full, coefficient_names = enc$x_names,
    moderator_names = enc$z_names, integration_grid = gh,
    analysis_signature = analysis_signature,
    scope = full_scope
  )
  network_state_full <- .scmix_attach_network_loading_evidence(
    network_state_full, loading_invariance_full)
  network_state_folds <- lapply(seq_len(K), function(k) {
    state <- .scmix_capture_network_state(
      nets[[k]], p = p, p_Z = ncol(Z_task), q = q, hidden = hidden_use,
      mean_family = family_use,
      mu_bound = mu_bound, kappa_bound = kappa_bound,
      alpha_bound = compact$alpha_bound,
      a_bound = compact$a_bound, weight_bound = compact$weight_bound,
      coefficient_scale = sd_dx_folds[[k]],
      z_transform = z_transform_folds[[k]],
      dx_transform = dx_transform_folds[[k]],
      coefficient_names = enc$x_names, moderator_names = enc$z_names,
      integration_grid = gh, analysis_signature = analysis_signature,
      scope = fold_scopes[k]
    )
    .scmix_attach_network_loading_evidence(
      state, loading_invariance_folds[[k]])
  })
  multistart_full <- .scmix_complete_multistart_artifacts(
    full_fit, network_state_full, sd_dx)
  multistart_folds <- lapply(seq_len(K), function(k) {
    ## The selected fold network is already represented by its portable
    ## template; substitute each retained start state into that same contract.
    .scmix_complete_multistart_artifacts(
      list(start_artifacts = start_artifacts_folds[[k]]),
      network_state_folds[[k]], sd_dx_folds[[k]])
  })

  fit <- list(
    mu_hat = mu_hat,               # task rows; constant within respondent
    mu_all_folds = mu_all_folds,   # task rows x p for every fold network
    mu_full = mu_full,              # full-sample plug-in mean estimate
    kappa_hat = full_fit$kappa,
    kappa_folds = kappa_folds,
    A_hat = A_full,
    A_computational_full = A_full,   # legacy alias; explicitly raw scale
    Sigma_hat = Sigma_hat,
    A_folds = A_folds,               # legacy raw-scale alias
    A_computational_folds = A_folds, # legacy alias; explicitly raw scale
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
    mean_family = family_use,
    hidden = hidden_use,
    n_epochs = as.integer(n_epochs),
    learning_rate = learning_rate,
    weight_decay_used = wd_use,
    n_starts = as.integer(n_starts),
    bounds = list(mu = mu_bound,
                  mu_units = if (family_use == "legacy") {
                    "legacy standardized-contrast coefficient"
                  } else "raw coefficient",
                  mu_internal = if (family_use == "legacy") mu_bound else
                    mu_bound * sd_dx,
                  mu_raw_by_coordinate = if (family_use == "legacy") {
                    mu_bound / sd_dx
                  } else rep(mu_bound, length(sd_dx)),
                  alpha = compact$alpha_bound,
                  alpha_units = "raw coefficient at training-coded origin",
                  kappa = kappa_bound,
                  a = compact$a_bound,
                  a_units = "raw-coefficient Frobenius norm",
                  weight = compact$weight_bound,
                  weight_units = "coordinatewise network parameter",
                  internal_mu_scale = "standardized contrasts"),
    optimization = list(
      full = .sc_comp_fit_summary(full_fit),
      folds = optimization_folds,
      gate_by_fold = optimization_gate_by_fold,
      v22_gate_by_fold = v22_optimization_gate_by_fold,
      diagnostics_are_certificates = FALSE
    ),
    computational_optimization_gate_pass =
      computational_optimization_gate_pass,
    v22_computational_optimization_gate_pass =
      v22_computational_optimization_gate_pass,
    protocol_mode = protocol_mode,
    calibration_only = identical(protocol_mode, "calibration"),
    calibration_verification = calibration_verification,
    v22_fit_computation_subset_pass =
      isTRUE(v22_computational_optimization_gate_pass) &&
      identical(protocol_mode, "production"),
    analysis_signature = analysis_signature,
    seed = seed,
    loss_traces = loss_traces,
    nets = nets,
    full_net = full_fit$net,
    network_state_folds = network_state_folds,
    network_state_full = network_state_full,
    multistart_artifacts = list(full = multistart_full,
                                folds = multistart_folds),
    call = call
  )
  fit <- .scmix_add_loading_contract(
    fit, q = q,
    raw_full = A_full, optimization_full = full_fit$A,
    coefficient_scale_full = sd_dx,
    raw_folds = A_folds, optimization_folds = A_optimization_folds,
    coefficient_scale_folds = sd_dx_folds,
    invariance_evidence = c(list(loading_invariance_full),
                            loading_invariance_folds),
    source = paste(
      "Explicit raw and optimizer-scale loadings from the full fit and",
      "respondent-level outer-fold fits."))
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
    operational_gate <- if (!is.null(x$computational_optimization_gate_pass)) {
      x$computational_optimization_gate_pass
    } else od$optimization_gate_pass
    cat(sprintf(paste0(
      "  pre-calibration/legacy operational gate: %s",
      " (not v2.2 eligibility)\n"),
      if (isTRUE(operational_gate)) "pass" else "fail"))
    v22_gate <- if (!is.null(x$v22_computational_optimization_gate_pass)) {
      x$v22_computational_optimization_gate_pass
    } else isTRUE(od$v22_optimization_gate_pass)
    cat(sprintf("  v2.2 full-and-fold optimization gate: %s [%s]\n",
                if (isTRUE(v22_gate)) "pass" else "fail",
                x$protocol_mode %||% od$protocol_mode %||%
                  "legacy_unspecified"))
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
  print.scmix(object)
  if (isTRUE(calibrate)) .scmix_print_floor(object)
  invisible(object)
}
