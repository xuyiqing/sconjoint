# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/quadrature.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

.rum_normal_rule <- function(order) {
  if (length(order) != 1L || !is.finite(order) || order != as.integer(order) ||
      order < 2L || order > 80L) stop("Normal quadrature orders must be integers from 2 to 80.")
  jacobi <- matrix(0, order, order)
  j <- seq_len(order - 1L)
  jacobi[cbind(j, j + 1L)] <- sqrt(j)
  jacobi[cbind(j + 1L, j)] <- sqrt(j)
  eg <- eigen(jacobi, symmetric = TRUE)
  ii <- base::order(eg$values)
  nodes <- eg$values[ii]
  weights <- eg$vectors[1L, ii]^2
  positive <- weights > 0
  nodes <- nodes[positive]; weights <- weights[positive] / sum(weights[positive])
  if (abs(sum(weights * nodes)) > 1e-10 || abs(sum(weights * nodes^2) - 1) > 1e-10)
    stop("Quadrature moment normalization failed.")
  list(nodes = nodes, weights = weights)
}

#' @export
rum_quadrature <- function(order = c(11L, 11L), policy_law = c("normal", "three_point")) {
  policy_law <- match.arg(policy_law)
  if (length(order) == 1L) order <- rep(order, 2L)
  if (length(order) != 2L) stop("Specify policy and position integration orders.")
  pu <- if (policy_law == "normal") .rum_normal_rule(order[1L]) else
    list(nodes = c(-sqrt(3), 0, sqrt(3)), weights = c(1, 4, 1)/6)
  pv <- .rum_normal_rule(order[2L])
  structure(list(u = rep(pu$nodes, times = length(pv$nodes)),
    v = rep(pv$nodes, each = length(pu$nodes)),
    weight = as.numeric(outer(pu$weights, pv$weights)), order = as.integer(order),
    policy_law = policy_law), class = "rum_quadrature")
}

#' @export
rum_loglik <- function(data, par, quadrature = rum_quadrature()) {
  .rum_check_data(data); .rum_check_par(data, par)
  if (!inherits(quadrature, "rum_quadrature")) stop("Expected a rum_quadrature object.")
  out <- rum_sequence_cpp(data$y, data$d, data$a, data$offsets,
    as.numeric(par), quadrature$u, quadrature$v, quadrature$weight)
  out$objective <- mean(out$loglik)
  out$gradient <- colMeans(out$score)
  colnames(out$score) <- names(rum_parameters(data))
  names(out$gradient) <- colnames(out$score)
  out
}

#' @export
rum_information <- function(data, par, quadrature = rum_quadrature(), step = 1e-4) {
  .rum_check_data(data); .rum_check_par(data, par)
  .rum_information(data, par, quadrature, step,
    function(par, quadrature) rum_loglik(data, par, quadrature))
}

.rum_information <- function(data, par, quadrature, step, evaluate, ev = NULL) {
  if (!is.numeric(step) || length(step) != 1L || !is.finite(step) || step <= 0)
    stop("step must be one finite positive number.")
  if (is.null(ev)) ev <- evaluate(par, quadrature)
  opg <- crossprod(ev$score) / data$n
  h <- length(par)
  hs <- matrix(NA_real_, h, h)
  # Finite differences differentiate the analytic integrated score, not fitted effects.
  for (j in seq_len(h)) {
    delta <- step * max(1, abs(par[j]))
    plus <- minus <- par
    plus[j] <- plus[j] + delta
    if (j == h && par[j] < delta) {
      plus2 <- par; plus2[j] <- plus2[j] + 2 * delta
      hs[, j] <- -(-3 * ev$gradient + 4 * evaluate(plus, quadrature)$gradient -
                         evaluate(plus2, quadrature)$gradient) / (2 * delta)
    } else {
      minus[j] <- minus[j] - delta
      hs[, j] <- -(evaluate(plus, quadrature)$gradient -
                        evaluate(minus, quadrature)$gradient) / (2 * delta)
    }
  }
  hs <- (hs + t(hs))/2
  dimnames(opg) <- dimnames(hs) <- list(names(rum_parameters(data)), names(rum_parameters(data)))
  list(opg = opg, hessian = hs, eigenvalues = eigen(opg, symmetric = TRUE, only.values = TRUE)$values,
       hessian_eigenvalues = eigen(hs, symmetric = TRUE, only.values = TRUE)$values,
       sample_identity_difference = max(abs(opg - hs)),
       interpretation = "Observed-design sample information; no regularity or coverage certificate.")
}
