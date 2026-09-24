# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/dnn-gaussian.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

# Numeric reference implementation of the structured Gaussian DNN likelihood.
# No optimization, penalty, posterior summaries, or inferential calibration is
# performed here. The existing native engine integrates a complete sequence;
# ordinary matrix backpropagation carries its output scores through the net.

.rum_dnn_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x > .Machine$integer.max || x != floor(x))
    stop(name, " must be one positive integer.")
  as.integer(x)
}

.rum_dnn_check_network <- function(network) {
  if (!inherits(network, "rum_dnn_gaussian"))
    stop("Expected a network from rum_dnn_gaussian().")
  p_z <- .rum_dnn_integer(network$p_z, "network$p_z")
  p <- .rum_dnn_integer(network$p, "network$p")
  hidden <- network$hidden
  if (!is.numeric(hidden) || any(!is.finite(hidden)) ||
      any(hidden < 1 | hidden > .Machine$integer.max | hidden != floor(hidden)))
    stop("network$hidden must contain positive integers, or be empty.")
  widths <- c(p_z, as.integer(hidden), p)
  layers <- length(widths) - 1L
  if (!is.list(network$weights) || !is.list(network$biases) ||
      length(network$weights) != layers || length(network$biases) != layers)
    stop("Network weights and biases do not match the declared layers.")
  for (j in seq_len(layers)) {
    w <- network$weights[[j]]; b <- network$biases[[j]]
    if (!is.matrix(w) || !is.numeric(w) ||
        !identical(dim(w), widths[c(j, j + 1L)]) || any(!is.finite(w)) ||
        !is.numeric(b) || !is.null(dim(b)) || length(b) != widths[j + 1L] ||
        any(!is.finite(b)))
      stop("Invalid weights or biases in network layer ", j, ".")
  }
  invisible(network)
}

.rum_dnn_matrix <- function(x, rows, cols, name, ids = NULL) {
  if (!is.matrix(x) || !is.numeric(x) ||
      !identical(dim(x), c(as.integer(rows), as.integer(cols))) ||
      any(!is.finite(x)))
    stop(name, " must be a finite numeric ", rows, " by ", cols, " matrix.")
  if (!is.null(ids) && !is.null(rownames(x)) &&
      !identical(rownames(x), as.character(ids)))
    stop(name, " row names must match data$ids exactly and in order.")
  invisible(x)
}

.rum_dnn_forward_cache <- function(network, Z) {
  .rum_dnn_check_network(network)
  if (!is.matrix(Z) || !nrow(Z)) stop("Z must be a nonempty numeric matrix.")
  .rum_dnn_matrix(Z, nrow(Z), network$p_z, "Z")
  layers <- length(network$weights)
  activations <- vector("list", layers + 1L)
  preactivations <- vector("list", layers)
  activations[[1L]] <- Z
  for (j in seq_len(layers)) {
    value <- sweep(activations[[j]] %*% network$weights[[j]],
                   2L, network$biases[[j]], "+")
    if (any(!is.finite(value)))
      stop("Non-finite network activation in layer ", j, ".")
    preactivations[[j]] <- value
    activations[[j + 1L]] <- if (j < layers) pmax(value, 0) else value
  }
  list(mu = activations[[layers + 1L]], activations = activations,
       preactivations = preactivations)
}

.rum_dnn_backprop <- function(network, cache, output_gradient) {
  layers <- length(network$weights)
  weights <- biases <- vector("list", layers)
  delta <- output_gradient
  for (j in rev(seq_len(layers))) {
    weights[[j]] <- crossprod(cache$activations[[j]], delta)
    biases[[j]] <- colSums(delta)
    if (any(!is.finite(weights[[j]])) || any(!is.finite(biases[[j]])))
      stop("Non-finite network gradient in layer ", j, ".")
    if (j > 1L) {
      delta <- (delta %*% t(network$weights[[j]])) *
        (cache$preactivations[[j - 1L]] > 0)
      if (any(!is.finite(delta)))
        stop("Non-finite backpropagated derivative before layer ", j, ".")
    }
  }
  names(weights) <- names(network$weights)
  names(biases) <- names(network$biases)
  list(weights = weights, biases = biases)
}

.rum_dnn_negate <- function(x) {
  if (is.list(x)) lapply(x, .rum_dnn_negate) else -x
}

# Structured ReLU network for a Gaussian repeated-choice model
# @param p_z Number of encoded respondent inputs.
# @param p Number of utility-feature mean outputs.
# @param hidden Hidden-layer widths; integer(0) gives an affine network.
# @param seed Private base-R initialization seed; caller RNG is restored.
# @return A numeric network with input-by-output weights and bias vectors.
#' @export
rum_dnn_gaussian <- function(p_z = 16L, p = 16L,
                             hidden = c(32L, 32L, 16L), seed = 1L) {
  p_z <- .rum_dnn_integer(p_z, "p_z")
  p <- .rum_dnn_integer(p, "p")
  if (!is.numeric(hidden) || any(!is.finite(hidden)) ||
      any(hidden < 1 | hidden > .Machine$integer.max | hidden != floor(hidden)))
    stop("hidden must contain positive integers, or be empty.")
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be one nonnegative integer.")
  hidden <- as.integer(hidden)
  widths <- c(p_z, hidden, p)
  .rum_with_seed(as.integer(seed), {
    weights <- biases <- vector("list", length(widths) - 1L)
    for (j in seq_along(weights)) {
      bound <- 1 / sqrt(widths[j])
      weights[[j]] <- matrix(stats::runif(widths[j] * widths[j + 1L],
        -bound, bound), nrow = widths[j], ncol = widths[j + 1L])
      biases[[j]] <- stats::runif(widths[j + 1L], -bound, bound)
    }
    names(weights) <- names(biases) <- paste0("layer", seq_along(weights))
    structure(list(p_z = p_z, p = p, hidden = hidden,
      weights = weights, biases = biases, seed = as.integer(seed),
      parameter_count = sum(vapply(weights, length, integer(1))) +
        sum(vapply(biases, length, integer(1))),
      initialization = "Independent uniform weights and biases in +/-1/sqrt(fan_in).",
      relu_derivative_at_zero = 0,
      inference_status = "unavailable"), class = "rum_dnn_gaussian")
  })
}

# @rdname rum_dnn_gaussian
# @param network A network returned by rum_dnn_gaussian().
# @param Z Finite respondent-by-input matrix, in the data respondent order.
# @return rum_dnn_gaussian_forward() returns a respondent-by-p matrix.
#' @export
rum_dnn_gaussian_forward <- function(network, Z) {
  .rum_dnn_forward_cache(network, Z)$mu
}

.rum_dnn_check_sequence_data <- function(data) {
  .rum_check_data(data)
  n <- .rum_dnn_integer(data$n, "data$n")
  p <- .rum_dnn_integer(data$p, "data$p")
  if (!is.matrix(data$d) || !is.numeric(data$d) || !nrow(data$d) ||
      ncol(data$d) != p || any(!is.finite(data$d)))
    stop("Invalid packed feature matrix in data.")
  tasks <- nrow(data$d)
  offsets <- data$offsets
  if (!is.numeric(offsets) || length(offsets) != n + 1L ||
      any(!is.finite(offsets)) || any(offsets != floor(offsets)) ||
      offsets[1L] != 0L || tail(offsets, 1L) != tasks ||
      any(diff(offsets) < 1L))
    stop("Invalid respondent sequence offsets in data.")
  if (!is.numeric(data$y) || length(data$y) != tasks ||
      any(!is.finite(data$y)) || any(!data$y %in% c(0, 1)))
    stop("data$y must contain one binary response per packed task.")
  if (!is.numeric(data$T) || !identical(as.numeric(data$T), as.numeric(diff(offsets))))
    stop("data$T must agree with the packed sequence offsets.")
  if (!is.character(data$ids) || length(data$ids) != n || anyNA(data$ids) ||
      any(!nzchar(data$ids)) || anyDuplicated(data$ids))
    stop("Invalid respondent IDs in data.")
  invisible(data)
}

.rum_dnn_check_normal_rule <- function(quadrature) {
  if (!inherits(quadrature, "rum_quadrature") ||
      !identical(quadrature$policy_law, "normal"))
    stop("The Gaussian DNN evaluator requires a normal rum_quadrature rule.")
  count <- length(quadrature$weight)
  if (count < 1L || !is.numeric(quadrature$weight) ||
      !is.numeric(quadrature$u) || !is.numeric(quadrature$v) ||
      length(quadrature$u) != count || length(quadrature$v) != count ||
      any(!is.finite(c(quadrature$u, quadrature$v, quadrature$weight))) ||
      any(quadrature$weight <= 0) || abs(sum(quadrature$weight) - 1) > 1e-10)
    stop("Invalid Gaussian quadrature nodes or positive normalized weights.")
  invisible(quadrature)
}

# @rdname rum_dnn_gaussian
# @param data Complete packed respondent records from rum_data().
# @param mu Numeric n-by-p conditional mean outputs. These replace data's
#   finite mean basis; their order must match data$ids and data$feature_names.
# @param kappa Common recorded-label intercept.
# @param loading Length-p loading vector for one persistent Gaussian factor.
# @param omega Nonnegative variance of the independent Gaussian label effect.
# @param quadrature Gaussian tensor-product rule from rum_quadrature().
# @return The evaluator returns sequence log likelihoods, output/common scores,
#   and signed mean-log-likelihood and negative-mean-log-likelihood gradients.
#' @export
rum_dnn_gaussian_evaluate <- function(data, mu, kappa, loading, omega,
                                      quadrature = rum_quadrature()) {
  .rum_dnn_check_sequence_data(data)
  .rum_dnn_matrix(mu, data$n, data$p, "mu", data$ids)
  if (!is.numeric(kappa) || length(kappa) != 1L || !is.finite(kappa) ||
      !is.numeric(loading) || !is.null(dim(loading)) || length(loading) != data$p ||
      any(!is.finite(loading)) || !is.numeric(omega) || length(omega) != 1L ||
      !is.finite(omega) || omega < 0)
    stop("Provide finite kappa, a length-p loading vector, and nonnegative omega.")
  .rum_dnn_check_normal_rule(quadrature)
  loglik <- numeric(data$n)
  output_score <- matrix(0, data$n, data$p)
  common_score <- matrix(0, data$n, data$p + 2L)
  mean_columns <- 1L + seq_len(data$p)
  common_columns <- c(1L, 1L + data$p + seq_len(data$p), 2L + 2L * data$p)
  for (i in seq_len(data$n)) {
    rows <- seq.int(data$offsets[i] + 1L, data$offsets[i + 1L])
    di <- data$d[rows, , drop = FALSE]
    # a=di makes the native finite mean coefficients exactly this mu_i.
    # One native respondent call preserves the same u,v across every task.
    one <- rum_sequence_cpp(data$y[rows], di, di,
      as.integer(c(0L, length(rows))), c(kappa, mu[i, ], loading, omega),
      quadrature$u, quadrature$v, quadrature$weight)
    loglik[i] <- one$loglik[1L]
    output_score[i, ] <- one$score[1L, mean_columns]
    common_score[i, ] <- one$score[1L, common_columns]
  }
  names(loglik) <- data$ids
  dimnames(output_score) <- list(data$ids, data$feature_names)
  dimnames(common_score) <- list(data$ids,
    c("kappa", paste0("loading:", data$feature_names), "omega"))
  common_gradient <- colMeans(common_score)
  objective_gradient <- list(mu = output_score / data$n,
    kappa = unname(common_gradient[1L]),
    loading = unname(common_gradient[1L + seq_len(data$p)]),
    omega = unname(tail(common_gradient, 1L)))
  list(loglik = loglik, objective = mean(loglik), loss = -mean(loglik),
    mu = mu, output_score = output_score, common_score = common_score,
    objective_gradient = objective_gradient,
    loss_gradient = .rum_dnn_negate(objective_gradient),
    n = data$n, tasks = data$T, quadrature = quadrature,
    gradient_convention = if (omega == 0) "omega_right_derivative" else "interior",
    policy_loading_zero = all(loading == 0),
    inference_status = "unavailable")
}

# @rdname rum_dnn_gaussian
# @return The loss wrapper additionally backpropagates output derivatives to
#   network weights and biases; it does not run an optimizer.
#' @export
rum_dnn_gaussian_loss <- function(network, data, Z, kappa, loading, omega,
                                  quadrature = rum_quadrature()) {
  .rum_dnn_check_sequence_data(data)
  .rum_dnn_check_network(network)
  if (network$p != data$p) stop("Network outputs must match data's utility features.")
  .rum_dnn_matrix(Z, data$n, network$p_z, "Z", data$ids)
  cache <- .rum_dnn_forward_cache(network, Z)
  result <- rum_dnn_gaussian_evaluate(data, cache$mu, kappa, loading, omega, quadrature)
  network_gradient <- .rum_dnn_backprop(network, cache, result$objective_gradient$mu)
  result$objective_gradient <- c(network_gradient,
    result$objective_gradient[c("kappa", "loading", "omega")])
  result$loss_gradient <- .rum_dnn_negate(result$objective_gradient)
  result$network_parameter_count <- network$parameter_count
  result$relu_derivative_at_zero <- 0
  result
}
