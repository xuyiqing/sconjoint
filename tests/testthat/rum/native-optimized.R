# Ported from conjointRUM 0.1.3 tests/native-optimized.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Exact old/new sequence-core comparison; no applications or fitting.
# RUM_NATIVE_SOURCECPP=1 tests source even if an installed package is available.
# RUM_NATIVE_BENCHMARK=1 also times a synthetic N=1991, T=8, p=16, r=32 bank.
# The embedded reference is the complete sequence core immediately before the
# posterior-residual optimization, including its original input validation.

compile_reference <- function(...) {
  # R CMD check's relative startup file is unavailable in a child build folder.
  old_tests <- Sys.getenv("R_TESTS", unset = NA_character_)
  on.exit(if (is.na(old_tests)) Sys.unsetenv("R_TESTS") else
    Sys.setenv(R_TESTS = old_tests), add = TRUE)
  Sys.unsetenv("R_TESTS")
  Rcpp::sourceCpp(...)
}

argv <- commandArgs(FALSE)
script_arg <- grep("^--file=", argv, value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[1L]) else ""
candidates <- c(if (nzchar(script_path)) dirname(dirname(normalizePath(script_path))),
                getwd(), dirname(getwd()))
have_source <- vapply(candidates, function(x) file.exists(file.path(x, "src", "rum_core.cpp")), logical(1))
source_root <- if (any(have_source)) candidates[which(have_source)[1L]] else NA_character_
force_source <- identical(Sys.getenv("RUM_NATIVE_SOURCECPP"), "1")
if (requireNamespace("sconjoint", quietly = TRUE) && !force_source) {
  sequence_new <- getFromNamespace("rum_sequence_cpp", "sconjoint")
} else {
  if (is.na(source_root)) stop("Cannot find current source or installed sconjoint")
  new_env <- new.env(parent = globalenv())
  compile_reference(file.path(source_root, "src", "rum_core.cpp"), env = new_env,
    cacheDir = file.path(tempdir(), "rum-native-optimized-new"))
  sequence_new <- new_env$rum_sequence_cpp
}

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

// Full sequence probabilities and observed-data scores. At omega = 0 the
// last score is the right derivative for adding independent Gaussian variance.
// [[Rcpp::export]]
Rcpp::List rum_sequence_before_cpp(Rcpp::NumericVector y, Rcpp::NumericMatrix d,
                           Rcpp::NumericMatrix a, Rcpp::IntegerVector offsets,
                           Rcpp::NumericVector par, Rcpp::NumericVector u,
                           Rcpp::NumericVector v, Rcpp::NumericVector weight) {
  const Dimensions dim = validate_common(d, a, offsets, par, u, v, weight);
  if (y.size() != dim.m) Rcpp::stop("y must have one entry per task");
  for (int t = 0; t < dim.m; ++t)
    if (!R_FINITE(y[t]) || (y[t] != 0.0 && y[t] != 1.0))
      Rcpp::stop("y must contain only zero and one");
  const double omega = par[dim.h - 1], scale = std::sqrt(omega);
  const bool boundary = omega == 0.0;
  Rcpp::NumericVector loglik(dim.n);
  Rcpp::NumericMatrix score(dim.n, dim.h);
  std::vector<double> node_score(dim.h), posterior_score(dim.h);
  for (int i = 0; i < dim.n; ++i) {
    if (i % 64 == 0) Rcpp::checkUserInterrupt();
    const int start = offsets[i], tasks = offsets[i + 1] - start;
    std::vector<double> fixed(tasks), policy(tasks);
    predictor_parts(start, tasks, dim, d, a, par, fixed, policy);
    double log_mass = R_NegInf;
    for (int k = 0; k < dim.nodes; ++k) {
      std::fill(node_score.begin(), node_score.end(), 0.0);
      double ll = 0.0, curvature = 0.0;
      for (int t = 0; t < tasks; ++t) {
        const int row = start + t;
        const double eta = fixed[t] + policy[t] * u[k] + scale * v[k];
        if (!R_FINITE(eta)) Rcpp::stop("non-finite latent utility index");
        const double prob = logistic(eta), residual = y[row] - prob;
        ll -= y[row] == 1.0 ? softplus(-eta) : softplus(eta);
        node_score[0] += residual;
        for (int j = 0; j < dim.r; ++j)
          node_score[1 + j] += residual * a(row, j);
        for (int j = 0; j < dim.p; ++j)
          node_score[1 + dim.r + j] += residual * d(row, j) * u[k];
        curvature += prob * (1.0 - prob);
      }
      node_score[dim.h - 1] = boundary ?
        0.5 * (node_score[0] * node_score[0] - curvature) :
        v[k] * node_score[0] / (2.0 * scale);
      for (int j = 0; j < dim.h; ++j)
        if (!R_FINITE(node_score[j])) Rcpp::stop("non-finite conditional score");
      const double log_node = std::log(weight[k]) + ll;
      if (!R_FINITE(log_node)) Rcpp::stop("non-finite sequence log probability");
      if (k == 0) {
        log_mass = log_node;
        posterior_score = node_score;
      } else {
        const double next = log_add(log_mass, log_node);
        const double fraction = std::exp(log_node - next);
        for (int j = 0; j < dim.h; ++j)
          posterior_score[j] += fraction * (node_score[j] - posterior_score[j]);
        log_mass = next;
      }
    }
    loglik[i] = log_mass;
    for (int j = 0; j < dim.h; ++j) score(i, j) = posterior_score[j];
  }
  require_finite(score, "computed sequence scores");
  return Rcpp::List::create(Rcpp::Named("loglik") = loglik,
                            Rcpp::Named("score") = score);
}

'
old_env <- new.env(parent = globalenv())
compile_reference(code = old_source, env = old_env,
  cacheDir = file.path(tempdir(), "rum-native-optimized-old"))
sequence_old <- old_env$rum_sequence_before_cpp

checks <- list()
check <- function(name, actual, expected, tolerance = 1e-10) {
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
make_bank <- function(n, tasks, p) {
  ti <- rep(tasks, length.out = n); m <- sum(ti)
  d <- matrix(rnorm(m * p, sd = .6), m, p)
  z <- rep(seq(-1, 1, length.out = n), ti)
  list(y = rbinom(m, 1, .55), d = d, a = cbind(d, d * z),
       offsets = as.integer(c(0, cumsum(ti))))
}
evaluate <- function(fun, bank, theta, q) do.call(fun,
  c(bank, list(par = theta, u = q$u, v = q$v, weight = q$weight)))
set.seed(2026091507)
small <- make_bank(5L, c(1L, 3L, 4L, 8L, 2L), 3L)
theta <- c(.17, .3, -.2, .1, .18, -.13, .07, .55, -.35, .25, .42)
h <- length(theta); loading <- 8:10
states <- list(regular = theta,
  zero_position = replace(theta, h, 0),
  zero_policy = replace(theta, loading, 0),
  both_zero = replace(theta, c(loading, h), 0),
  near_zero_position = replace(theta, h, 1e-10),
  positive_extreme = replace(theta, 1L, 800),
  negative_extreme = replace(theta, 1L, -800),
  dispersed = replace(theta, loading, c(3, -2, 1.5)))
for (nn in c(9L, 17L)) for (nm in names(states)) {
  q <- rule(nn); a <- evaluate(sequence_old, small, states[[nm]], q)
  b <- evaluate(sequence_new, small, states[[nm]], q)
  check(paste(nm, nn, "log likelihood"), b$loglik, a$loglik, 0)
  check(paste(nm, nn, "scores"), b$score, a$score, 2e-10)
}
q <- rule(17L)
b <- evaluate(sequence_new, small, theta, q)
flipped <- replace(theta, loading, -theta[loading])
bf <- evaluate(sequence_new, small, flipped, q)
expected <- b$score; expected[, loading] <- -expected[, loading]
check("loading-sign log likelihood", bf$loglik, b$loglik, 1e-13)
check("loading-sign score transport", bf$score, expected, 2e-12)
fd <- vapply(seq_len(h), function(k) {
  plus <- minus <- theta; plus[k] <- plus[k] + 1e-5; minus[k] <- minus[k] - 1e-5
  (evaluate(sequence_new, small, plus, q)$loglik -
     evaluate(sequence_new, small, minus, q)$loglik) / 2e-5
}, numeric(5L))
check("independent central score difference", b$score, fd, 3e-9)
zero <- states$zero_position
boundary <- evaluate(sequence_new, small, zero, q)$score[, h]
f0 <- evaluate(sequence_new, small, zero, q)$loglik
f1 <- evaluate(sequence_new, small, replace(zero, h, 1e-5), q)$loglik
f2 <- evaluate(sequence_new, small, replace(zero, h, 2e-5), q)$loglik
check("independent omega-zero heat derivative", boundary, (-3*f0 + 4*f1 - f2)/2e-5, 3e-8)

if (identical(Sys.getenv("RUM_NATIVE_BENCHMARK"), "1")) {
  set.seed(2026091508)
  bank <- make_bank(1991L, 8L, 16L); q <- rule(21L)
  theta_big <- c(.12, rep(c(.08, -.04, .03, .01), 8L),
                 seq(-.12, .15, length.out = 16L), .30)
  a <- evaluate(sequence_old, bank, theta_big, q)
  b <- evaluate(sequence_new, bank, theta_big, q)
  check("large synthetic exact log likelihood", b$loglik, a$loglik, 0)
  check("large synthetic scores", b$score, a$score, 2e-11)
  timing <- do.call(rbind, lapply(seq_len(3L), function(k) {
    # Alternate order to reduce a systematic first-call timing effect.
    fs <- if (k %% 2L) list(old = sequence_old, new = sequence_new) else
      list(new = sequence_new, old = sequence_old)
    elapsed <- vapply(fs, function(f) unname(system.time(evaluate(f, bank, theta_big, q))["elapsed"]), numeric(1))
    data.frame(repetition = k, old_seconds = elapsed["old"], new_seconds = elapsed["new"])
  }))
  print(timing, row.names = FALSE)
  cat("Median sequence-evaluation speedup:",
      median(timing$old_seconds) / median(timing$new_seconds), "x\n")
}
result <- do.call(rbind, checks)
print(result, row.names = FALSE)
stopifnot(all(result$passed))
cat(nrow(result), "native optimization checks passed.\n")
