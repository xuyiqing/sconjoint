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

# Integration rule over the n_fac taste factors u ~ N(0, I) and the side term v ~ N(0, 1).
# rule = "product": Gauss-Hermite product rule; order has one entry (all dimensions), two
#   (every u dimension, then v) or n_fac + 1 entries (u_1, ..., u_n_fac, v). Nodes run with
#   u_1 fastest and v slowest (at n_fac = 1 exactly the original two-dimensional rule).
# rule = "sobol": Owen-scrambled Sobol points over (u, v) in n_fac + 1 dimensions
#   (spacefillr::generate_sobol_owen_set with a fixed seed), mapped through qnorm, equal
#   weights; order is ignored.
# rule = "auto": the product rule at n_fac = 1 (today's rule, unchanged), Sobol with
#   `nodes` points at n_fac > 1.
# With n_fac = 1 and the product rule the object is the original one (u a length-G vector);
# otherwise u is a G x n_fac matrix and the object also records n_fac, rule, nodes and seed.
#' @export
rum_quadrature <- function(order = c(11L, 11L), policy_law = c("normal", "three_point"),
                           n_fac = 1L, rule = c("auto", "product", "sobol"),
                           nodes = 512L, seed = 20261114L) {
  policy_law <- match.arg(policy_law)
  rule <- match.arg(rule)
  if (!is.numeric(n_fac) || length(n_fac) != 1L || !is.finite(n_fac) || n_fac < 1 ||
      n_fac != floor(n_fac) || n_fac > 20) stop("n_fac must be one integer from 1 to 20.")
  n_fac <- as.integer(n_fac)
  if (rule == "auto") rule <- if (n_fac == 1L) "product" else "sobol"
  if (rule == "sobol") return(.rum_sobol_rule(n_fac, nodes, seed, policy_law))
  if (n_fac == 1L) {
    if (length(order) == 1L) order <- rep(order, 2L)
    if (length(order) != 2L) stop("Specify policy and position integration orders.")
    pu <- if (policy_law == "normal") .rum_normal_rule(order[1L]) else
      list(nodes = c(-sqrt(3), 0, sqrt(3)), weights = c(1, 4, 1)/6)
    pv <- .rum_normal_rule(order[2L])
    return(structure(list(u = rep(pu$nodes, times = length(pv$nodes)),
      v = rep(pv$nodes, each = length(pu$nodes)),
      weight = as.numeric(outer(pu$weights, pv$weights)), order = as.integer(order),
      policy_law = policy_law), class = "rum_quadrature"))
  }
  if (length(order) == 1L) order <- rep(order, n_fac + 1L) else
    if (length(order) == 2L) order <- c(rep(order[1L], n_fac), order[2L])
  if (length(order) != n_fac + 1L)
    stop("Specify one order, two (factors, position) or n_fac + 1 orders.")
  rules <- lapply(seq_len(n_fac + 1L), function(j)
    if (j <= n_fac && policy_law == "three_point")
      list(nodes = c(-sqrt(3), 0, sqrt(3)), weights = c(1, 4, 1)/6) else
      .rum_normal_rule(order[j]))
  sizes <- vapply(rules, function(r) length(r$nodes), integer(1))
  if (prod(as.numeric(sizes)) > 5e6) stop("The product rule has more than 5e6 nodes.")
  grid <- as.matrix(expand.grid(lapply(sizes, seq_len), KEEP.OUT.ATTRS = FALSE))
  u <- vapply(seq_len(n_fac), function(j) rules[[j]]$nodes[grid[, j]], numeric(nrow(grid)))
  u <- matrix(u, nrow(grid), n_fac)
  weight <- rep(1, nrow(grid))
  for (j in seq_len(n_fac + 1L)) weight <- weight * rules[[j]]$weights[grid[, j]]
  structure(list(u = u, v = rules[[n_fac + 1L]]$nodes[grid[, n_fac + 1L]],
    weight = weight / sum(weight), order = as.integer(order), policy_law = policy_law,
    n_fac = n_fac, rule = "product", nodes = nrow(grid), seed = NA_integer_),
    class = "rum_quadrature")
}

# Owen-scrambled Sobol rule over (u_1, ..., u_n_fac, v). spacefillr draws the scrambling from
# its own generator seeded by `seed`, so R's random-number state is neither used nor changed.
# The first m points of a seed's sequence do not depend on the requested count.
.rum_sobol_rule <- function(n_fac, nodes = 512L, seed = 20261114L, policy_law = "normal") {
  if (!identical(policy_law, "normal")) stop("The Sobol rule integrates a normal policy law only.")
  nodes <- .rum_dnn_integer(nodes, "nodes")
  if (nodes < 2L) stop("nodes must be at least 2.")
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be one nonnegative integer.")
  pts <- spacefillr::generate_sobol_owen_set(nodes, n_fac + 1L, seed = as.integer(seed))
  pts <- matrix(pts, nodes, n_fac + 1L)
  if (any(!is.finite(pts)) || any(pts <= 0 | pts >= 1))
    stop("Sobol points must lie strictly inside the unit cube.")
  z <- stats::qnorm(pts)
  structure(list(u = z[, seq_len(n_fac), drop = FALSE], v = z[, n_fac + 1L],
    weight = rep(1 / nodes, nodes), order = nodes, policy_law = "normal",
    n_fac = as.integer(n_fac), rule = "sobol", nodes = nodes, seed = as.integer(seed)),
    class = "rum_quadrature")
}

# Number of taste factors a rule integrates (a vector u is one factor).
.rum_quad_nfac <- function(quadrature)
  if (is.matrix(quadrature$u)) ncol(quadrature$u) else 1L

# Short label of a rule ("15x15", "11x11x15", "sobol512").
.rum_quad_label <- function(quadrature)
  if (identical(quadrature$rule, "sobol")) paste0("sobol", quadrature$nodes) else
    paste(quadrature$order, collapse = "x")

#' @export
rum_loglik <- function(data, par, quadrature = rum_quadrature()) {
  .rum_check_data(data); .rum_check_par(data, par)
  if (!inherits(quadrature, "rum_quadrature")) stop("Expected a rum_quadrature object.")
  if (.rum_quad_nfac(quadrature) != 1L)
    stop("The formula RUM has one taste factor; use a one-factor quadrature rule.")
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
