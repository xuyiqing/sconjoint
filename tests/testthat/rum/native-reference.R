# Ported from conjointRUM 0.1.3 tests/native-reference.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Independent small-space arithmetic checks; no fitting or application data.
# Run from the package root with Rscript tests/native-reference.R.
# RUM_NATIVE_SOURCECPP=1 forces a private sourceCpp build instead of installed code.

argv <- commandArgs(FALSE)
script_arg <- grep("^--file=", argv, value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[1L]) else ""
candidate_roots <- c(if (nzchar(script_path)) dirname(dirname(normalizePath(script_path))),
                     getwd(), dirname(getwd()))
has_source <- vapply(candidate_roots, function(x) file.exists(file.path(x, "src", "rum_core.cpp")), logical(1))
source_root <- if (any(has_source)) candidate_roots[which(has_source)[1L]] else NA_character_
force_source <- identical(Sys.getenv("RUM_NATIVE_SOURCECPP"), "1")
have_package <- requireNamespace("sconjoint", quietly = TRUE)
if (have_package && !force_source) {
  sequence_native <- getFromNamespace("rum_sequence_cpp", "sconjoint")
  moments_native <- getFromNamespace("rum_moments_cpp", "sconjoint")
} else {
  if (is.na(source_root)) stop("Cannot find native source or installed sconjoint")
  native_env <- new.env(parent = globalenv())
  Rcpp::sourceCpp(file.path(source_root, "src", "rum_core.cpp"), env = native_env,
                  cacheDir = file.path(tempdir(), "rum-native-reference-cache"))
  sequence_native <- native_env$rum_sequence_cpp
  moments_native <- native_env$rum_moments_cpp
}

checks <- list()
check <- function(name, actual, expected, tolerance = 1e-9) {
  same_shape <- identical(dim(actual), dim(expected)) && length(actual) == length(expected)
  error <- if (same_shape && all(is.finite(actual)) && all(is.finite(expected)))
    max(c(0, abs(actual - expected))) else Inf
  checks[[length(checks) + 1L]] <<- data.frame(check = name, max_abs_error = error,
    tolerance = tolerance, passed = error <= tolerance, stringsAsFactors = FALSE)
}
check_error <- function(name, expr) {
  failed <- tryCatch({force(expr); FALSE}, error = function(e) TRUE)
  check(name, as.numeric(failed), 1, 0)
}

# Standard-normal Gauss-Hermite from its Jacobi matrix, independent of package code.
gh_reference <- function(n) {
  jacobi <- matrix(0, n, n)
  if (n > 1L) {
    j <- seq_len(n - 1L)
    jacobi[cbind(j, j + 1L)] <- sqrt(j)
    jacobi[cbind(j + 1L, j)] <- sqrt(j)
  }
  eig <- eigen(jacobi, symmetric = TRUE)
  order <- order(eig$values)
  list(node = eig$values[order], weight = eig$vectors[1L, order]^2)
}
tensor_reference <- function(n = 15L) {
  rule <- gh_reference(n)
  ij <- expand.grid(i = seq_len(n), j = seq_len(n))
  list(u = rule$node[ij$i], v = rule$node[ij$j],
       weight = rule$weight[ij$i] * rule$weight[ij$j])
}
quad <- tensor_reference()
check("quadrature normalization", sum(quad$weight), 1, 2e-14)
check("quadrature normal moments", c(sum(quad$weight * quad$u),
  sum(quad$weight * quad$v), sum(quad$weight * quad$u^2), sum(quad$weight * quad$v^2)),
  c(0, 0, 1, 1), 5e-14)

d_list <- list(matrix(c(.8, -.4), 1, 2),
  matrix(c(1.1, -.5, .3, -.2, .9, .6), 3, 2),
  matrix(c(-.8, .7, 1.2, -.3, .4, -1.0, .2, .9), 4, 2))
basis_list <- list(matrix(c(1, .1, 0, .8, .2, -.3), 2, 3),
  matrix(c(1, -.2, .4, 1, -.1, .5), 2, 3),
  matrix(c(.9, .3, -.2, .7, .6, .1), 2, 3))
a_list <- Map(function(d, b) d %*% b, d_list, basis_list)
tasks <- vapply(d_list, nrow, integer(1))
offsets <- as.integer(c(0, cumsum(tasks)))
d <- do.call(rbind, d_list)
a <- do.call(rbind, a_list)
y <- c(1, 1, 0, 1, 0, 1, 1, 0)
par <- c(kappa = .17, a1 = .30, a2 = -.22, a3 = .15,
         l1 = .60, l2 = -.35, omega = .42)
p <- ncol(d); r <- ncol(a); h <- length(par)
patterns <- lapply(tasks, function(T) as.matrix(expand.grid(rep(list(0:1), T))))

# This reference enumerates outcomes and integrates raw products. It does not
# use the native log-sum-exp, score formula, or count/pair prediction recursion.
reference_prob <- function(di, ai, response, theta, rule = quad) {
  fixed <- as.vector(theta[1L] + ai %*% theta[1L + seq_len(r)])
  policy <- as.vector(di %*% theta[1L + r + seq_len(p)])
  eta <- outer(fixed, rep(1, length(rule$weight))) + outer(policy, rule$u)
  eta <- sweep(eta, 2L, sqrt(theta[h]) * rule$v, "+")
  probabilities <- plogis(eta)
  probabilities[response == 0, ] <- 1 - probabilities[response == 0, , drop = FALSE]
  by_node <- apply(probabilities, 2L, prod)
  sum(rule$weight * by_node)
}
reference_loglik <- function(theta, response = y, rule = quad) {
  vapply(seq_along(tasks), function(i) {
    rows <- seq.int(offsets[i] + 1L, offsets[i + 1L])
    log(reference_prob(d_list[[i]], a_list[[i]], response[rows], theta, rule))
  }, numeric(1))
}
native_sequence <- function(theta, response = y, dd = d, aa = a, rule = quad) {
  sequence_native(response, dd, aa, offsets, theta, rule$u, rule$v, rule$weight)
}
finite_derivative <- function(fun, theta, step = 1e-5) {
  value <- fun(theta)
  result <- matrix(NA_real_, length(value), length(theta))
  for (j in seq_along(theta)) {
    eps <- step * (1 + abs(theta[j]))
    plus <- minus <- theta; plus[j] <- plus[j] + eps; minus[j] <- minus[j] - eps
    result[, j] <- (fun(plus) - fun(minus)) / (2 * eps)
  }
  result
}
actual <- native_sequence(par)
check("observed loglik against independent product integral", actual$loglik, reference_loglik(par))
check("all sequence score blocks against independent central derivatives", actual$score,
      finite_derivative(reference_loglik, par), 3e-9)

J <- 8L
marginal <- matrix(0, nrow(d), J)
pair_counts <- tasks * (tasks - 1L) / 2L
pair <- matrix(0, sum(pair_counts), J)
count <- matrix(0, sum(tasks + 1L), J)
pair_offset <- as.integer(c(0, cumsum(pair_counts)))
count_offset <- as.integer(c(0, cumsum(tasks + 1L)))
for (i in seq_along(tasks)) {
  T <- tasks[i]; rows <- seq.int(offsets[i] + 1L, offsets[i + 1L])
  marginal[rows, 1L] <- 1 / T
  marginal[rows, 7L] <- sin(rows) / T
  at <- pair_offset[i]
  if (T > 1L) for (t in seq_len(T - 1L)) for (s in seq.int(t + 1L, T)) {
    at <- at + 1L
    pair[at, 2L] <- 1 / pair_counts[i]
    pair[at, 7L] <- cos(at) / pair_counts[i]
    if (s == t + 1L) {
      marginal[rows[c(t, s)], 3L] <- marginal[rows[c(t, s)], 3L] + 1 / (T - 1L)
      pair[at, 3L] <- -2 / (T - 1L)
    }
  }
  rows_count <- seq.int(count_offset[i] + 1L, count_offset[i + 1L])
  count[rows_count[1L], 4L] <- 1
  count[rows_count[T + 1L], 5L] <- 1
  count[rows_count[floor(T / 2L) + 1L], 6L] <- 1
  count[rows_count, 7L] <- seq(-.2, .3, length.out = T + 1L) / (T + 1L)
  count[rows_count, 8L] <- 1
}
observed_bank <- function(i, response) {
  rows <- seq.int(offsets[i] + 1L, offsets[i + 1L])
  value <- as.vector(response %*% marginal[rows, , drop = FALSE])
  at <- pair_offset[i]; T <- tasks[i]
  if (T > 1L) for (t in seq_len(T - 1L)) for (s in seq.int(t + 1L, T)) {
    at <- at + 1L
    value <- value + response[t] * response[s] * pair[at, ]
  }
  value + count[count_offset[i] + sum(response) + 1L, ]
}
bank_patterns <- lapply(seq_along(tasks), function(i)
  t(apply(patterns[[i]], 1L, function(response) observed_bank(i, response))))
reference_moments <- function(theta, rule = quad) {
  t(vapply(seq_along(tasks), function(i) {
    probabilities <- apply(patterns[[i]], 1L, function(response)
      reference_prob(d_list[[i]], a_list[[i]], response, theta, rule))
    as.vector(probabilities %*% bank_patterns[[i]])
  }, numeric(J)))
}
native_moments <- function(theta, dd = d, aa = a, rule = quad) {
  moments_native(dd, aa, offsets, theta, rule$u, rule$v, rule$weight, marginal, pair, count)
}
pred <- native_moments(par)
check("marginal/pair/count/switch mixed bank against full enumeration", pred$expected,
      reference_moments(par), 2e-13)
check("all average moment derivative blocks against independent derivatives", pred$derivative,
      finite_derivative(function(theta) colMeans(reference_moments(theta)), par), 2e-9)
check("constant count-sum expectation", pred$expected[, 8L], rep(1, length(tasks)), 2e-13)
check("constant count-sum derivative", pred$derivative[8L, ], rep(0, h), 2e-13)

score_identity <- matrix(0, J, h)
for (i in seq_along(tasks)) {
  response <- patterns[[i]]; size <- nrow(response); T <- tasks[i]
  out <- sequence_native(as.vector(t(response)),
    do.call(rbind, rep(list(d_list[[i]]), size)),
    do.call(rbind, rep(list(a_list[[i]]), size)),
    as.integer(seq(0, size * T, by = T)), par, quad$u, quad$v, quad$weight)
  probabilities <- exp(out$loglik)
  check(paste("response normalization design", i), sum(probabilities), 1, 5e-13)
  check(paste("model-weighted score mean design", i), colSums(out$score * probabilities), rep(0, h), 5e-13)
  score_identity <- score_identity + crossprod(bank_patterns[[i]], out$score * probabilities) / length(tasks)
}
check("moment derivative equals model-weighted raw-moment score", pred$derivative, score_identity, 2e-12)

boundary <- par; boundary[h] <- 0
out0 <- native_sequence(boundary); pred0 <- native_moments(boundary)
right_derivative <- function(fun, theta, eps = 1e-4) {
  at0 <- fun(theta); p1 <- p2 <- theta
  p1[h] <- eps; p2[h] <- eps / 2
  2 * (fun(p2) - at0) / (eps / 2) - (fun(p1) - at0) / eps
}
check("omega-zero likelihood right score", out0$score[, h],
      right_derivative(reference_loglik, boundary), 2e-7)
check("omega-zero marginal/pair/count heat derivatives", pred0$derivative[, h],
      right_derivative(function(theta) colMeans(reference_moments(theta)), boundary), 2e-7)
for (i in seq_along(tasks)) {
  response <- patterns[[i]]; size <- nrow(response); T <- tasks[i]
  out <- sequence_native(as.vector(t(response)),
    do.call(rbind, rep(list(d_list[[i]]), size)),
    do.call(rbind, rep(list(a_list[[i]]), size)),
    as.integer(seq(0, size * T, by = T)), boundary, quad$u, quad$v, quad$weight)
  check(paste("omega-zero pattern normalization design", i), sum(exp(out$loglik)), 1, 5e-13)
  check(paste("omega-zero model score mean design", i),
        colSums(out$score * exp(out$loglik)), rep(0, h), 5e-13)
}
for (j in seq_len(h - 1L)) {
  eps <- 1e-5 * (1 + abs(boundary[j])); plus <- minus <- boundary
  plus[j] <- plus[j] + eps; minus[j] <- minus[j] - eps
  check(paste("omega-zero sequence coordinate", j), out0$score[, j],
    (reference_loglik(plus) - reference_loglik(minus)) / (2 * eps), 3e-9)
  check(paste("omega-zero moment coordinate", j), pred0$derivative[, j],
    (colMeans(reference_moments(plus)) - colMeans(reference_moments(minus))) / (2 * eps), 3e-9)
}

sign_par <- par; loading_index <- 1L + r + seq_len(p)
sign_par[loading_index] <- -par[loading_index]
sign_score <- actual$score; sign_score[, loading_index] <- -sign_score[, loading_index]
sign_grad <- pred$derivative; sign_grad[, loading_index] <- -sign_grad[, loading_index]
check("loading-sign sequence invariance", native_sequence(sign_par)$loglik, actual$loglik, 5e-13)
check("loading-sign score chain rule", native_sequence(sign_par)$score, sign_score, 2e-12)
check("loading-sign moment invariance", native_moments(sign_par)$expected, pred$expected, 5e-13)
check("loading-sign moment chain rule", native_moments(sign_par)$derivative, sign_grad, 2e-12)
scales <- c(2.2, .65); scaled_par <- par; scaled_par[loading_index] <- scales * par[loading_index]
scaled_d <- sweep(d, 2L, scales, "/")
scaled_score <- actual$score; scaled_score[, loading_index] <- sweep(scaled_score[, loading_index], 2L, scales, "/")
scaled_grad <- pred$derivative; scaled_grad[, loading_index] <- sweep(scaled_grad[, loading_index], 2L, scales, "/")
check("feature-scaling likelihood", native_sequence(scaled_par, dd = scaled_d)$loglik, actual$loglik, 5e-13)
check("feature-scaling scores", native_sequence(scaled_par, dd = scaled_d)$score, scaled_score, 2e-12)
check("feature-scaling moments", native_moments(scaled_par, dd = scaled_d)$expected, pred$expected, 5e-13)
check("feature-scaling moment derivatives", native_moments(scaled_par, dd = scaled_d)$derivative, scaled_grad, 2e-12)
zero_policy <- par; zero_policy[loading_index] <- 0
check("zero-policy stationary sequence loading score", native_sequence(zero_policy)$score[, loading_index],
      matrix(0, length(tasks), p), 5e-13)
check("zero-policy stationary moment loading derivative", native_moments(zero_policy)$derivative[, loading_index],
      matrix(0, J, p), 5e-13)
zero_effects <- zero_policy; zero_effects[h] <- 0
fixed_index <- as.vector(zero_effects[1L] + a %*% zero_effects[1L + seq_len(r)])
ordinary_ll <- vapply(seq_along(tasks), function(i) {
  rows <- seq.int(offsets[i] + 1L, offsets[i + 1L])
  sum(dbinom(y[rows], 1, plogis(fixed_index[rows]), log = TRUE))
}, numeric(1))
check("both-zero random effects recover ordinary logit likelihood", native_sequence(zero_effects)$loglik,
      ordinary_ll, 5e-13)
i <- 3L; response <- y[seq.int(offsets[i] + 1L, offsets[i + 1L])]
separate_prob <- prod(vapply(seq_len(tasks[i]), function(t)
  reference_prob(d_list[[i]][t, , drop = FALSE], a_list[[i]][t, , drop = FALSE],
                 response[t], par), numeric(1)))
whole_prob <- reference_prob(d_list[[i]], a_list[[i]], response, par)
check("fixture distinguishes joint sequence from product of marginals",
      as.numeric(abs(whole_prob - separate_prob) > 1e-4), 1, 0)

extreme <- par; extreme[] <- 0; extreme[1L] <- 800
check("extreme indices keep finite log likelihood", as.numeric(all(is.finite(native_sequence(extreme)$loglik))), 1, 0)
check("extreme indices keep finite scores", as.numeric(all(is.finite(native_sequence(extreme)$score))), 1, 0)
check("extreme indices keep finite moment derivatives", as.numeric(all(is.finite(native_moments(extreme)$derivative))), 1, 0)

bad <- par; bad[h] <- -.1
check_error("negative variance rejected", native_sequence(bad))
check_error("nonbinary response rejected", native_sequence(par, response = replace(y, 1, .5)))
check_error("nonfinite design rejected", native_sequence(par, dd = replace(d, 1, NA_real_)))
check_error("wrong parameter length rejected", native_sequence(par[-1]))
check_error("non-normalized weights rejected", sequence_native(y, d, a, offsets, par, quad$u, quad$v, quad$weight * 2))
check_error("nonpositive weights rejected", sequence_native(y, d, a, offsets, par, quad$u, quad$v, replace(quad$weight, 1, 0)))
check_error("empty respondent rejected", sequence_native(y, d, a, as.integer(c(0, 0, 8)), par, quad$u, quad$v, quad$weight))
check_error("wrong pair rows rejected", moments_native(d, a, offsets, par, quad$u, quad$v, quad$weight,
  marginal, pair[-1, , drop = FALSE], count))

results <- do.call(rbind, checks)
print(results, row.names = FALSE)
if (!all(results$passed)) stop("Native reference checks failed")
cat(sprintf("Native independent reference: %d/%d checks passed.\n", nrow(results), nrow(results)))
