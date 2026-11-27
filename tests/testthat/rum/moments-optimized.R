# Ported from conjointRUM 0.1.3 tests/moments-optimized.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Old/new moment-engine equivalence; no fits, outcomes from applications, or coverage.
# RUM_NATIVE_SOURCECPP=1 tests the current source rather than an installed version.
# RUM_NATIVE_BENCHMARK=1 times synthetic N1991/T8/p16/r32/J11/GH21x21.
compile_reference <- function(...) {
  # R CMD check's relative startup file is unavailable in a child build folder.
  old_tests <- Sys.getenv("R_TESTS", unset = NA_character_)
  on.exit(if (is.na(old_tests)) Sys.unsetenv("R_TESTS") else
    Sys.setenv(R_TESTS = old_tests), add = TRUE)
  Sys.unsetenv("R_TESTS")
  Rcpp::sourceCpp(...)
}

argv <- commandArgs(FALSE)
arg <- grep("^--file=", argv, value = TRUE)
script <- if (length(arg)) sub("^--file=", "", arg[1L]) else ""
roots <- c(if (nzchar(script)) dirname(dirname(normalizePath(script))), getwd(), dirname(getwd()))
found <- vapply(roots, function(x) file.exists(file.path(x, "src", "rum_core.cpp")), logical(1))
root <- if (any(found)) roots[which(found)[1L]] else NA_character_
if (requireNamespace("sconjoint", quietly = TRUE) && !identical(Sys.getenv("RUM_NATIVE_SOURCECPP"), "1")) {
  current <- getFromNamespace("rum_moments_cpp", "sconjoint")
} else {
  if (is.na(root)) stop("Cannot find current native source or installed sconjoint")
  e <- new.env(parent = globalenv())
  compile_reference(file.path(root, "src", "rum_core.cpp"), env = e,
    cacheDir = file.path(tempdir(), "rum-moments-optimized-new"))
  current <- e$rum_moments_cpp
}
# Complete frozen previous moment implementation and original input validation.
old_source <- '#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::plugins(cpp11)]]

namespace {

struct Dimensions {
  int n, m, p, r, h, nodes, pairs, counts;
};

inline double logistic(double x) {
  if (x >= 0.0) return 1.0 / (1.0 + std::exp(-x));
  const double ex = std::exp(x);
  return ex / (1.0 + ex);
}

inline double softplus(double x) {
  return x > 0.0 ? x + std::log1p(std::exp(-x)) : std::log1p(std::exp(x));
}

inline double log_add(double x, double y) {
  if (x > y) return x + std::log1p(std::exp(y - x));
  return y + std::log1p(std::exp(x - y));
}

void require_finite(const Rcpp::NumericVector& x, const char* name) {
  for (R_xlen_t j = 0; j < x.size(); ++j)
    if (!R_FINITE(x[j])) Rcpp::stop("%s must contain only finite values", name);
}

Dimensions validate_common(const Rcpp::NumericMatrix& d,
                           const Rcpp::NumericMatrix& a,
                           const Rcpp::IntegerVector& offsets,
                           const Rcpp::NumericVector& par,
                           const Rcpp::NumericVector& u,
                           const Rcpp::NumericVector& v,
                           const Rcpp::NumericVector& weight) {
  if (d.nrow() < 1 || d.ncol() < 1)
    Rcpp::stop("d must have at least one task and one feature");
  if (a.nrow() != d.nrow()) Rcpp::stop("a and d must have the same task rows");
  if (offsets.size() < 2 || offsets.size() > std::numeric_limits<int>::max())
    Rcpp::stop("offsets must contain a supported number of respondent boundaries");
  const int n = offsets.size() - 1;
  if (offsets[0] != 0 || offsets[n] != d.nrow())
    Rcpp::stop("offsets must start at zero and end at the task count");
  double pair_count = 0.0;
  for (int i = 0; i < n; ++i) {
    if (offsets[i] == NA_INTEGER || offsets[i + 1] == NA_INTEGER ||
        offsets[i] < 0 || offsets[i + 1] <= offsets[i] ||
        offsets[i + 1] > d.nrow())
      Rcpp::stop("offsets must be increasing valid task boundaries");
    const double tasks = offsets[i + 1] - offsets[i];
    pair_count += tasks * (tasks - 1.0) / 2.0;
  }
  if (pair_count > std::numeric_limits<int>::max() ||
      static_cast<double>(d.nrow()) + n > std::numeric_limits<int>::max())
    Rcpp::stop("pair or count rows exceed the supported matrix size");
  const int h = 2 + a.ncol() + d.ncol();
  if (par.size() != h)
    Rcpp::stop("par must be [kappa, alpha, loading, position_variance]");
  if (u.size() < 1 || u.size() > std::numeric_limits<int>::max() ||
      u.size() != v.size() || u.size() != weight.size())
    Rcpp::stop("u, v, and weight must have the same positive length");
  require_finite(d, "d");
  require_finite(a, "a");
  require_finite(par, "par");
  require_finite(u, "u");
  require_finite(v, "v");
  require_finite(weight, "weight");
  if (par[h - 1] < 0.0) Rcpp::stop("position_variance must be nonnegative");
  double sum_weight = 0.0;
  for (R_xlen_t k = 0; k < weight.size(); ++k) {
    if (weight[k] <= 0.0) Rcpp::stop("quadrature weights must be positive");
    sum_weight += weight[k];
  }
  if (!R_FINITE(sum_weight) || std::fabs(sum_weight - 1.0) > 1e-8)
    Rcpp::stop("quadrature weights must sum to one");
  Dimensions out = {n, d.nrow(), d.ncol(), a.ncol(), h,
                    static_cast<int>(u.size()), static_cast<int>(pair_count),
                    d.nrow() + n};
  return out;
}

void predictor_parts(int start, int tasks, const Dimensions& dim,
                     const Rcpp::NumericMatrix& d, const Rcpp::NumericMatrix& a,
                     const Rcpp::NumericVector& par,
                     std::vector<double>& fixed, std::vector<double>& policy) {
  for (int t = 0; t < tasks; ++t) {
    const int row = start + t;
    fixed[t] = par[0];
    policy[t] = 0.0;
    for (int j = 0; j < dim.r; ++j) fixed[t] += a(row, j) * par[1 + j];
    for (int j = 0; j < dim.p; ++j)
      policy[t] += d(row, j) * par[1 + dim.r + j];
    if (!R_FINITE(fixed[t]) || !R_FINITE(policy[t]))
      Rcpp::stop("non-finite linear predictor; check parameter and design scales");
  }
}

void validate_bank_matrix(const Rcpp::NumericMatrix& x, int rows, int columns,
                          const char* name) {
  if (x.nrow() != rows || x.ncol() != columns)
    Rcpp::stop("%s has incompatible rows or moment columns", name);
  require_finite(x, name);
}

} // namespace

// Bounded raw marginal/pair/count summaries. Count derivatives are propagated
// through the Poisson-binomial recursion, with a common-shift heat derivative
// for the position-variance boundary. Memory is O(T*h + N*J + J*h).
// [[Rcpp::export]]
Rcpp::List rum_moments_before_cpp(Rcpp::NumericMatrix d, Rcpp::NumericMatrix a,
                         Rcpp::IntegerVector offsets, Rcpp::NumericVector par,
                         Rcpp::NumericVector u, Rcpp::NumericVector v,
                         Rcpp::NumericVector weight,
                         Rcpp::NumericMatrix marginal_weights,
                         Rcpp::NumericMatrix pair_weights,
                         Rcpp::NumericMatrix count_weights) {
  const Dimensions dim = validate_common(d, a, offsets, par, u, v, weight);
  const int moments = marginal_weights.ncol();
  if (moments < 1) Rcpp::stop("a moment bank must have at least one column");
  if (static_cast<double>(moments) * dim.h > std::numeric_limits<int>::max())
    Rcpp::stop("moment derivative workspace exceeds supported size");
  validate_bank_matrix(marginal_weights, dim.m, moments, "marginal_weights");
  validate_bank_matrix(pair_weights, dim.pairs, moments, "pair_weights");
  validate_bank_matrix(count_weights, dim.counts, moments, "count_weights");
  bool need_counts = false;
  for (R_xlen_t j = 0; j < count_weights.size(); ++j)
    if (count_weights[j] != 0.0) { need_counts = true; break; }
  const double omega = par[dim.h - 1], scale = std::sqrt(omega);
  const bool boundary = omega == 0.0;
  Rcpp::NumericMatrix expected(dim.n, moments), derivative(moments, dim.h);
  int pair_start = 0, count_start = 0;
  for (int i = 0; i < dim.n; ++i) {
    if (i % 32 == 0) Rcpp::checkUserInterrupt();
    const int start = offsets[i], tasks = offsets[i + 1] - start;
    std::vector<double> fixed(tasks), policy(tasks), prob(tasks), first(tasks),
      second(tasks), dp(static_cast<size_t>(tasks) * dim.h);
    std::vector<double> pmf(tasks + 1), pmf_first(tasks + 1), pmf_second(tasks + 1),
      pmf_derivative(static_cast<size_t>(tasks + 1) * dim.h);
    std::vector<double> value(moments), grad(static_cast<size_t>(moments) * dim.h);
    predictor_parts(start, tasks, dim, d, a, par, fixed, policy);
    for (int k = 0; k < dim.nodes; ++k) {
      std::fill(value.begin(), value.end(), 0.0);
      std::fill(grad.begin(), grad.end(), 0.0);
      for (int t = 0; t < tasks; ++t) {
        const int row = start + t;
        const double eta = fixed[t] + policy[t] * u[k] + scale * v[k];
        if (!R_FINITE(eta)) Rcpp::stop("non-finite latent utility index");
        prob[t] = logistic(eta);
        first[t] = prob[t] * (1.0 - prob[t]);
        second[t] = first[t] * (1.0 - 2.0 * prob[t]);
        const size_t at = static_cast<size_t>(t) * dim.h;
        dp[at] = first[t];
        for (int j = 0; j < dim.r; ++j) dp[at + 1 + j] = first[t] * a(row, j);
        for (int j = 0; j < dim.p; ++j)
          dp[at + 1 + dim.r + j] = first[t] * d(row, j) * u[k];
        dp[at + dim.h - 1] = boundary ? 0.0 : first[t] * v[k] / (2.0 * scale);
      }
      for (int t = 0; t < tasks; ++t) {
        const size_t at = static_cast<size_t>(t) * dim.h;
        for (int j = 0; j < moments; ++j) {
          const double w = marginal_weights(start + t, j);
          if (w == 0.0) continue;
          value[j] += w * prob[t];
          for (int h = 0; h < dim.h; ++h) grad[j * dim.h + h] += w * dp[at + h];
          if (boundary) grad[j * dim.h + dim.h - 1] += 0.5 * w * second[t];
        }
      }
      int pair_row = pair_start;
      for (int t = 0; t < tasks; ++t) {
        for (int s = t + 1; s < tasks; ++s, ++pair_row) {
          const size_t at = static_cast<size_t>(t) * dim.h;
          const size_t as = static_cast<size_t>(s) * dim.h;
          for (int j = 0; j < moments; ++j) {
            const double w = pair_weights(pair_row, j);
            if (w == 0.0) continue;
            value[j] += w * prob[t] * prob[s];
            for (int h = 0; h < dim.h; ++h)
              grad[j * dim.h + h] += w * (dp[at + h] * prob[s] + prob[t] * dp[as + h]);
            if (boundary) grad[j * dim.h + dim.h - 1] += 0.5 * w *
              (second[t] * prob[s] + 2.0 * first[t] * first[s] + prob[t] * second[s]);
          }
        }
      }
      if (need_counts) {
        std::fill(pmf.begin(), pmf.end(), 0.0);
        std::fill(pmf_first.begin(), pmf_first.end(), 0.0);
        std::fill(pmf_second.begin(), pmf_second.end(), 0.0);
        std::fill(pmf_derivative.begin(), pmf_derivative.end(), 0.0);
        pmf[0] = 1.0;
        for (int t = 0; t < tasks; ++t) {
          const double p = prob[t], p1 = first[t], p2 = second[t];
          for (int c = t + 1; c >= 0; --c) {
            const double old = pmf[c], previous = c > 0 ? pmf[c - 1] : 0.0;
            const double difference = previous - old;
            const size_t ac = static_cast<size_t>(c) * dim.h;
            const size_t at = static_cast<size_t>(t) * dim.h;
            for (int h = 0; h < dim.h; ++h) {
              const double prev_grad = c > 0 ? pmf_derivative[ac - dim.h + h] : 0.0;
              pmf_derivative[ac + h] = (1.0 - p) * pmf_derivative[ac + h]
                + p * prev_grad + dp[at + h] * difference;
            }
            if (boundary) {
              const double old1 = pmf_first[c], prev1 = c > 0 ? pmf_first[c - 1] : 0.0;
              const double prev2 = c > 0 ? pmf_second[c - 1] : 0.0;
              pmf_second[c] = (1.0 - p) * pmf_second[c] + p * prev2
                + 2.0 * p1 * (prev1 - old1) + p2 * difference;
              pmf_first[c] = (1.0 - p) * old1 + p * prev1 + p1 * difference;
            }
            pmf[c] = (1.0 - p) * old + p * previous;
          }
        }
        for (int c = 0; c <= tasks; ++c) {
          const size_t ac = static_cast<size_t>(c) * dim.h;
          for (int j = 0; j < moments; ++j) {
            const double w = count_weights(count_start + c, j);
            if (w == 0.0) continue;
            value[j] += w * pmf[c];
            for (int h = 0; h < dim.h; ++h)
              grad[j * dim.h + h] += w * pmf_derivative[ac + h];
            if (boundary) grad[j * dim.h + dim.h - 1] += 0.5 * w * pmf_second[c];
          }
        }
      }
      for (int j = 0; j < moments; ++j) {
        expected(i, j) += weight[k] * value[j];
        for (int h = 0; h < dim.h; ++h)
          derivative(j, h) += (weight[k] / dim.n) * grad[j * dim.h + h];
      }
    }
    pair_start += static_cast<int>(static_cast<long long>(tasks) * (tasks - 1) / 2);
    count_start += tasks + 1;
  }
  require_finite(expected, "computed moment expectations");
  require_finite(derivative, "computed moment derivatives");
  return Rcpp::List::create(Rcpp::Named("expected") = expected,
                            Rcpp::Named("derivative") = derivative);
}
'
e <- new.env(parent = globalenv())
compile_reference(code = old_source, env = e,
  cacheDir = file.path(tempdir(), "rum-moments-optimized-old"))
previous <- e$rum_moments_before_cpp
checks <- list()
check <- function(name, actual, expected, tolerance = 2e-10) {
  shape <- identical(dim(actual), dim(expected)) && length(actual) == length(expected)
  error <- if (shape && all(is.finite(actual)) && all(is.finite(expected)))
    max(c(0, abs(actual - expected))) else Inf
  checks[[length(checks) + 1L]] <<- data.frame(check = name,
    max_abs_error = error, tolerance = tolerance, passed = error <= tolerance,
    stringsAsFactors = FALSE)
}
rule <- function(n) {
  j <- matrix(0, n, n); k <- seq_len(n - 1L)
  j[cbind(k, k + 1L)] <- j[cbind(k + 1L, k)] <- sqrt(k)
  e <- eigen(j, symmetric = TRUE); ix <- order(e$values)
  g <- expand.grid(u = seq_len(n), v = seq_len(n))
  list(u = e$values[ix][g$u], v = e$values[ix][g$v],
    weight = e$vectors[1L, ix][g$u]^2 * e$vectors[1L, ix][g$v]^2)
}
make_bank <- function(n, tasks, p, J = 11L, dense = FALSE) {
  ti <- rep(tasks, length.out = n); m <- sum(ti)
  d <- matrix(rnorm(m*p, sd = .6), m, p)
  z <- rep(seq(-1, 1, length.out = n), ti)
  mw <- matrix(0, m, J)
  pw <- matrix(0, sum(ti*(ti-1L)/2L), J)
  cw <- matrix(0, sum(ti+1L), J)
  if (dense) {
    mw[] <- runif(length(mw), -.2, .2)
    pw[] <- runif(length(pw), -.1, .1)
    cw[] <- runif(length(cw), -.3, .3)
  } else {
    ms <- ps <- cs <- 0L
    for (i in seq_len(n)) {
      T <- ti[i]; P <- T*(T-1L)/2L
      mw[ms + seq_len(T), 1] <- 1/T
      mw[ms + seq_len(T), 2] <- seq(-1, 1, length.out = T)/T
      if (P) {
        pw[ps + seq_len(P), 3:6] <- outer(seq_len(P)/P, c(1, -.5, .3, .7))/P
      }
      cw[cs + seq_len(T+1L), 7:11] <- outer(0:T, seq_len(5), function(k, j) as.numeric(k %% 5 == j-1))
      ms <- ms+T; ps <- ps+P; cs <- cs+T+1L
    }
  }
  list(d = d, a = cbind(d, d*z), offsets = as.integer(c(0, cumsum(ti))),
    marginal_weights = mw, pair_weights = pw, count_weights = cw)
}
evaluate <- function(fun, bank, theta, q) do.call(fun,
  c(bank, list(par = theta, u = q$u, v = q$v, weight = q$weight)))
set.seed(2026091512)
small <- make_bank(5L, c(1L, 3L, 4L, 8L, 2L), 3L, dense = TRUE)
theta <- c(.17, .3, -.2, .1, .18, -.13, .07, .55, -.35, .25, .42)
h <- length(theta); loading <- 8:10
states <- list(regular = theta,
  zero_position = replace(theta, h, 0), zero_policy = replace(theta, loading, 0),
  both_zero = replace(theta, c(loading, h), 0),
  near_zero_position = replace(theta, h, 1e-10),
  positive_extreme = replace(theta, 1L, 800), negative_extreme = replace(theta, 1L, -800),
  dispersed = replace(theta, loading, c(3, -2, 1.5)))
for (nn in c(9L, 17L)) for (nm in names(states)) {
  q <- rule(nn); a <- evaluate(previous, small, states[[nm]], q)
  b <- evaluate(current, small, states[[nm]], q)
  check(paste(nm, nn, "expectations"), b$expected, a$expected, 0)
  check(paste(nm, nn, "derivatives"), b$derivative, a$derivative,
        if (nm == "near_zero_position") 2e-9 else 2e-11)
}
q <- rule(17L); b <- evaluate(current, small, theta, q)
fd <- vapply(seq_len(h), function(k) {
  plus <- minus <- theta; plus[k] <- plus[k] + 1e-5; minus[k] <- minus[k] - 1e-5
  (colMeans(evaluate(current, small, plus, q)$expected) -
   colMeans(evaluate(current, small, minus, q)$expected))/2e-5
}, numeric(11L))
check("independent central moment derivatives", b$derivative, fd, 3e-9)
zero <- states$zero_position
f0 <- evaluate(current, small, zero, q)
f1 <- colMeans(evaluate(current, small, replace(zero, h, 1e-5), q)$expected)
f2 <- colMeans(evaluate(current, small, replace(zero, h, 2e-5), q)$expected)
check("independent omega-zero heat derivatives", f0$derivative[, h],
  (-3*colMeans(f0$expected) + 4*f1 - f2)/2e-5, 3e-8)
flipped <- replace(theta, loading, -theta[loading])
f <- evaluate(current, small, flipped, q)
expected <- b$derivative; expected[, loading] <- -expected[, loading]
check("loading-sign expectations", f$expected, b$expected, 2e-12)
check("loading-sign derivative transport", f$derivative, expected, 2e-12)
for (which_bank in c("marginal", "pair", "count", "zero")) {
  only <- small
  if (which_bank != "marginal") only$marginal_weights[] <- 0
  if (which_bank != "pair") only$pair_weights[] <- 0
  if (which_bank != "count") only$count_weights[] <- 0
  a <- evaluate(previous, only, theta, q); b <- evaluate(current, only, theta, q)
  check(paste(which_bank, "only expectations"), b$expected, a$expected, 0)
  check(paste(which_bank, "only derivatives"), b$derivative, a$derivative, 2e-11)
}
# Exact count total identity checks the differentiated full probability mass.
identity <- small; identity$marginal_weights[] <- identity$pair_weights[] <- 0
identity$count_weights[] <- 1
id <- evaluate(current, identity, theta, q)
check("count-sum expectation one", id$expected, matrix(1, 5, 11), 2e-13)
check("count-sum derivative zero", id$derivative, matrix(0, 11, h), 2e-13)
if (identical(Sys.getenv("RUM_NATIVE_BENCHMARK"), "1")) {
  set.seed(2026091513)
  big <- make_bank(1991L, 8L, 16L); q <- rule(21L)
  tp <- c(.12, rep(c(.08, -.04, .03, .01), 8L), seq(-.12, .15, length.out = 16L), .30)
  a <- evaluate(previous, big, tp, q); b <- evaluate(current, big, tp, q)
  check("large-bank identical expectations", b$expected, a$expected, 0)
  check("large-bank derivative agreement", b$derivative, a$derivative, 2e-11)
  timing <- do.call(rbind, lapply(seq_len(3L), function(k) {
    fs <- if (k %% 2L) list(old = previous, new = current) else list(new = current, old = previous)
    elapsed <- vapply(fs, function(f) unname(system.time(evaluate(f, big, tp, q))["elapsed"]), numeric(1))
    data.frame(repetition = k, old_seconds = elapsed["old"], new_seconds = elapsed["new"])
  }))
  print(timing, row.names = FALSE)
  cat("Median moment-evaluation speedup:", median(timing$old_seconds)/median(timing$new_seconds), "x\n")
}
result <- do.call(rbind, checks)
print(result, row.names = FALSE)
stopifnot(all(result$passed))
cat(nrow(result), "moment optimization checks passed.\n")
