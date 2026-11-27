#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::plugins(cpp11)]]

// Below this omega the q > 1 sequence scores use the boundary form of the omega score,
// E[v^2 ((sum_t r_t)^2 - sum_t p_t (1 - p_t)) | Y] / 2 at the current nodes, with no division by
// sqrt(omega). For a rule symmetric in v (the default Sobol rule, and every product rule) it is the
// limit of the interior form E[v sum_t r_t | Y] / (2 sqrt(omega)) as omega -> 0 (the odd terms in v
// cancel node pair by node pair), so it differs from the interior form by O(omega) and its value
// at omega = 0 is the right derivative of the quadrature log likelihood. (For a product rule the
// v^2 weight integrates to 1 within every u and the form equals E[R^2 - sum p(1-p) | Y] / 2.) The single definition of the threshold: R reads it through
// rum_omega_small_cpp() into .rum_omega_small when the package loads (R/rum-zzz.R), so the
// pure-R scores (.rum_seq_scores), the conditional information and this kernel switch at the same
// omega (Codex review of 829393d, finding 1). The one-factor kernel keeps its own rule
// (boundary only at omega == 0) for bitwise parity with f20956e.
static const double RUM_OMEGA_SMALL = 1e-8;

namespace {

struct Dimensions {
  int n, m, p, r, h, nodes, pairs, counts, nf;
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
                           const Rcpp::NumericVector& weight,
                           int n_fac = 1) {
  // n_fac taste factors: par is [kappa, alpha, vec(Lambda) (p x n_fac, column-major),
  // omega] and u holds the G x n_fac factor nodes column-major (length G * n_fac).
  if (n_fac == NA_INTEGER || n_fac < 1) Rcpp::stop("n_fac must be a positive integer");
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
  const double h_count = 2.0 + a.ncol() + static_cast<double>(d.ncol()) * n_fac;
  if (h_count > std::numeric_limits<int>::max())
    Rcpp::stop("parameter count exceeds the supported size");
  const int h = static_cast<int>(h_count);
  if (par.size() != h)
    Rcpp::stop("par must be [kappa, alpha, loading, position_variance]");
  if (v.size() < 1 || v.size() > std::numeric_limits<int>::max() ||
      u.size() != v.size() * static_cast<R_xlen_t>(n_fac) || v.size() != weight.size())
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
                    static_cast<int>(v.size()), static_cast<int>(pair_count),
                    d.nrow() + n, n_fac};
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

// Several taste factors: policy[f * tasks + t] = d_t' Lambda[, f].
void predictor_parts_q(int start, int tasks, const Dimensions& dim,
                       const Rcpp::NumericMatrix& d, const Rcpp::NumericMatrix& a,
                       const Rcpp::NumericVector& par,
                       std::vector<double>& fixed, std::vector<double>& policy) {
  for (int t = 0; t < tasks; ++t) {
    const int row = start + t;
    fixed[t] = par[0];
    for (int j = 0; j < dim.r; ++j) fixed[t] += a(row, j) * par[1 + j];
    if (!R_FINITE(fixed[t]))
      Rcpp::stop("non-finite linear predictor; check parameter and design scales");
    for (int f = 0; f < dim.nf; ++f) {
      double value = 0.0;
      for (int j = 0; j < dim.p; ++j)
        value += d(row, j) * par[1 + dim.r + f * dim.p + j];
      if (!R_FINITE(value))
        Rcpp::stop("non-finite linear predictor; check parameter and design scales");
      policy[static_cast<size_t>(f) * tasks + t] = value;
    }
  }
}

// The sequence likelihood and scores with n_fac > 1 taste factors:
// eta = kappa + a'alpha + sum_f (d'Lambda_f) u_f + sqrt(omega) v. The n_fac = 1 case
// keeps its own code in rum_sequence_cpp (bitwise parity with the one-factor build).
Rcpp::List rum_sequence_q(const Dimensions& dim, const Rcpp::NumericVector& y,
                          const Rcpp::NumericMatrix& d, const Rcpp::NumericMatrix& a,
                          const Rcpp::IntegerVector& offsets,
                          const Rcpp::NumericVector& par, const Rcpp::NumericVector& u,
                          const Rcpp::NumericVector& v, const Rcpp::NumericVector& weight) {
  const double omega = par[dim.h - 1], scale = std::sqrt(omega);
  const bool boundary = omega < RUM_OMEGA_SMALL;
  const int G = dim.nodes, nf = dim.nf;
  Rcpp::NumericVector loglik(dim.n);
  Rcpp::NumericMatrix score(dim.n, dim.h);
  for (int i = 0; i < dim.n; ++i) {
    if (i % 64 == 0) Rcpp::checkUserInterrupt();
    const int start = offsets[i], tasks = offsets[i + 1] - start;
    const size_t tq = static_cast<size_t>(tasks) * nf;
    std::vector<double> fixed(tasks), policy(tq), residual(tasks),
      posterior_residual(tasks), posterior_policy_residual(tq), node_u(nf);
    predictor_parts_q(start, tasks, dim, d, a, par, fixed, policy);
    double log_mass = R_NegInf, posterior_omega = 0.0;
    for (int k = 0; k < G; ++k) {
      for (int f = 0; f < nf; ++f) node_u[f] = u[k + static_cast<R_xlen_t>(G) * f];
      double ll = 0.0, residual_sum = 0.0, curvature = 0.0;
      for (int t = 0; t < tasks; ++t) {
        const int row = start + t;
        double taste = 0.0;
        for (int f = 0; f < nf; ++f)
          taste += policy[static_cast<size_t>(f) * tasks + t] * node_u[f];
        const double eta = fixed[t] + taste + scale * v[k];
        if (!R_FINITE(eta)) Rcpp::stop("non-finite latent utility index");
        const double prob = logistic(eta);
        residual[t] = y[row] - prob;
        ll -= y[row] == 1.0 ? softplus(-eta) : softplus(eta);
        residual_sum += residual[t];
        if (boundary) curvature += prob * (1.0 - prob);
      }
      const double node_omega = boundary ?
        0.5 * v[k] * v[k] * (residual_sum * residual_sum - curvature) :
        v[k] * residual_sum / (2.0 * scale);
      if (!R_FINITE(node_omega)) Rcpp::stop("non-finite conditional score");
      const double log_node = std::log(weight[k]) + ll;
      if (!R_FINITE(log_node)) Rcpp::stop("non-finite sequence log probability");
      if (k == 0) {
        log_mass = log_node;
        for (int t = 0; t < tasks; ++t) {
          posterior_residual[t] = residual[t];
          for (int f = 0; f < nf; ++f)
            posterior_policy_residual[static_cast<size_t>(f) * tasks + t] =
              node_u[f] * residual[t];
        }
        posterior_omega = node_omega;
      } else {
        const double next = log_add(log_mass, log_node);
        const double fraction = std::exp(log_node - next);
        for (int t = 0; t < tasks; ++t) {
          posterior_residual[t] += fraction * (residual[t] - posterior_residual[t]);
          for (int f = 0; f < nf; ++f) {
            const size_t at = static_cast<size_t>(f) * tasks + t;
            posterior_policy_residual[at] += fraction *
              (node_u[f] * residual[t] - posterior_policy_residual[at]);
          }
        }
        posterior_omega += fraction * (node_omega - posterior_omega);
        log_mass = next;
      }
    }
    loglik[i] = log_mass;
    // E[r_t | Y] for the mean and E[u_f r_t | Y] for loading column f (p x n_fac).
    for (int t = 0; t < tasks; ++t) {
      const int row = start + t;
      score(i, 0) += posterior_residual[t];
      for (int j = 0; j < dim.r; ++j)
        score(i, 1 + j) += a(row, j) * posterior_residual[t];
      for (int f = 0; f < nf; ++f) {
        const double pr = posterior_policy_residual[static_cast<size_t>(f) * tasks + t];
        for (int j = 0; j < dim.p; ++j)
          score(i, 1 + dim.r + f * dim.p + j) += d(row, j) * pr;
      }
    }
    score(i, dim.h - 1) = posterior_omega;
  }
  require_finite(score, "computed sequence scores");
  return Rcpp::List::create(Rcpp::Named("loglik") = loglik,
                            Rcpp::Named("score") = score);
}

void validate_bank_matrix(const Rcpp::NumericMatrix& x, int rows, int columns,
                          const char* name) {
  if (x.nrow() != rows || x.ncol() != columns)
    Rcpp::stop("%s has incompatible rows or moment columns", name);
  require_finite(x, name);
}

} // namespace

// The omega threshold of the q > 1 scores (see RUM_OMEGA_SMALL).
// [[Rcpp::export]]
double rum_omega_small_cpp() { return RUM_OMEGA_SMALL; }

// Full sequence probabilities and observed-data scores. At omega = 0 the
// last score is the right derivative for adding independent Gaussian variance.
// n_fac > 1 taste factors: par = [kappa, alpha, vec(Lambda), omega] with Lambda
// p x n_fac column-major, and u the G x n_fac node matrix.
// [[Rcpp::export]]
Rcpp::List rum_sequence_cpp(Rcpp::NumericVector y, Rcpp::NumericMatrix d,
                           Rcpp::NumericMatrix a, Rcpp::IntegerVector offsets,
                           Rcpp::NumericVector par, Rcpp::NumericVector u,
                           Rcpp::NumericVector v, Rcpp::NumericVector weight,
                           int n_fac = 1) {
  const Dimensions dim = validate_common(d, a, offsets, par, u, v, weight, n_fac);
  if (y.size() != dim.m) Rcpp::stop("y must have one entry per task");
  for (int t = 0; t < dim.m; ++t)
    if (!R_FINITE(y[t]) || (y[t] != 0.0 && y[t] != 1.0))
      Rcpp::stop("y must contain only zero and one");
  if (dim.nf != 1) return rum_sequence_q(dim, y, d, a, offsets, par, u, v, weight);
  const double omega = par[dim.h - 1], scale = std::sqrt(omega);
  const bool boundary = omega == 0.0;
  Rcpp::NumericVector loglik(dim.n);
  Rcpp::NumericMatrix score(dim.n, dim.h);
  for (int i = 0; i < dim.n; ++i) {
    if (i % 64 == 0) Rcpp::checkUserInterrupt();
    const int start = offsets[i], tasks = offsets[i + 1] - start;
    std::vector<double> fixed(tasks), policy(tasks), residual(tasks),
      posterior_residual(tasks), posterior_policy_residual(tasks);
    predictor_parts(start, tasks, dim, d, a, par, fixed, policy);
    double log_mass = R_NegInf, posterior_omega = 0.0;
    for (int k = 0; k < dim.nodes; ++k) {
      double ll = 0.0, residual_sum = 0.0, curvature = 0.0;
      for (int t = 0; t < tasks; ++t) {
        const int row = start + t;
        const double eta = fixed[t] + policy[t] * u[k] + scale * v[k];
        if (!R_FINITE(eta)) Rcpp::stop("non-finite latent utility index");
        const double prob = logistic(eta);
        residual[t] = y[row] - prob;
        ll -= y[row] == 1.0 ? softplus(-eta) : softplus(eta);
        residual_sum += residual[t];
        if (boundary) curvature += prob * (1.0 - prob);
      }
      const double node_omega = boundary ?
        0.5 * (residual_sum * residual_sum - curvature) :
        v[k] * residual_sum / (2.0 * scale);
      if (!R_FINITE(node_omega)) Rcpp::stop("non-finite conditional score");
      const double log_node = std::log(weight[k]) + ll;
      if (!R_FINITE(log_node)) Rcpp::stop("non-finite sequence log probability");
      if (k == 0) {
        log_mass = log_node;
        for (int t = 0; t < tasks; ++t) {
          posterior_residual[t] = residual[t];
          posterior_policy_residual[t] = u[k] * residual[t];
        }
        posterior_omega = node_omega;
      } else {
        const double next = log_add(log_mass, log_node);
        const double fraction = std::exp(log_node - next);
        for (int t = 0; t < tasks; ++t) {
          posterior_residual[t] += fraction * (residual[t] - posterior_residual[t]);
          posterior_policy_residual[t] += fraction *
            (u[k] * residual[t] - posterior_policy_residual[t]);
        }
        posterior_omega += fraction * (node_omega - posterior_omega);
        log_mass = next;
      }
    }
    loglik[i] = log_mass;
    // Design rows do not vary across latent nodes. Move their linear maps
    // outside integration: E[r_t | Y] for the mean and E[u r_t | Y] for
    // policy loadings. This is the same observed score, with O(T*(r+p))
    // feature products per respondent instead of O(nodes*T*(r+p)).
    for (int t = 0; t < tasks; ++t) {
      const int row = start + t;
      score(i, 0) += posterior_residual[t];
      for (int j = 0; j < dim.r; ++j)
        score(i, 1 + j) += a(row, j) * posterior_residual[t];
      for (int j = 0; j < dim.p; ++j)
        score(i, 1 + dim.r + j) += d(row, j) * posterior_policy_residual[t];
    }
    score(i, dim.h - 1) = posterior_omega;
  }
  require_finite(score, "computed sequence scores");
  return Rcpp::List::create(Rcpp::Named("loglik") = loglik,
                            Rcpp::Named("score") = score);
}

// Bounded raw marginal/pair/count summaries. Integrate derivatives with respect
// to task indices before mapping them into the fixed parameter design. Count
// derivatives use a Poisson-binomial recursion, with a separate common-shift
// heat derivative at zero position variance. Memory is O(T*T + J*T + N*J + J*h).
// [[Rcpp::export]]
Rcpp::List rum_moments_cpp(Rcpp::NumericMatrix d, Rcpp::NumericMatrix a,
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
      second(tasks);
    std::vector<double> pmf(tasks + 1), pmf_first(tasks + 1), pmf_second(tasks + 1),
      pmf_derivative(need_counts ? static_cast<size_t>(tasks + 1) * tasks : 0);
    const size_t partial_size = static_cast<size_t>(moments) * tasks;
    std::vector<double> value(moments), grad(partial_size), heat(moments),
      integrated_grad(partial_size, 0.0), integrated_policy_grad(partial_size, 0.0),
      integrated_omega(moments, 0.0);
    predictor_parts(start, tasks, dim, d, a, par, fixed, policy);
    for (int k = 0; k < dim.nodes; ++k) {
      std::fill(value.begin(), value.end(), 0.0);
      std::fill(grad.begin(), grad.end(), 0.0);
      std::fill(heat.begin(), heat.end(), 0.0);
      for (int t = 0; t < tasks; ++t) {
        const double eta = fixed[t] + policy[t] * u[k] + scale * v[k];
        if (!R_FINITE(eta)) Rcpp::stop("non-finite latent utility index");
        prob[t] = logistic(eta);
        first[t] = prob[t] * (1.0 - prob[t]);
        second[t] = first[t] * (1.0 - 2.0 * prob[t]);
      }
      for (int t = 0; t < tasks; ++t) {
        for (int j = 0; j < moments; ++j) {
          const double w = marginal_weights(start + t, j);
          if (w == 0.0) continue;
          value[j] += w * prob[t];
          grad[static_cast<size_t>(j) * tasks + t] += w * first[t];
          if (boundary) heat[j] += 0.5 * w * second[t];
        }
      }
      int pair_row = pair_start;
      for (int t = 0; t < tasks; ++t) {
        for (int s = t + 1; s < tasks; ++s, ++pair_row) {
          for (int j = 0; j < moments; ++j) {
            const double w = pair_weights(pair_row, j);
            if (w == 0.0) continue;
            value[j] += w * prob[t] * prob[s];
            const size_t at = static_cast<size_t>(j) * tasks;
            grad[at + t] += w * first[t] * prob[s];
            grad[at + s] += w * prob[t] * first[s];
            if (boundary) heat[j] += 0.5 * w *
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
            const size_t ac = static_cast<size_t>(c) * tasks;
            for (int s = 0; s <= t; ++s) {
              const double prev_grad = c > 0 ? pmf_derivative[ac - tasks + s] : 0.0;
              pmf_derivative[ac + s] = (1.0 - p) * pmf_derivative[ac + s]
                + p * prev_grad + (s == t ? p1 * difference : 0.0);
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
          const size_t ac = static_cast<size_t>(c) * tasks;
          for (int j = 0; j < moments; ++j) {
            const double w = count_weights(count_start + c, j);
            if (w == 0.0) continue;
            value[j] += w * pmf[c];
            const size_t at = static_cast<size_t>(j) * tasks;
            for (int t = 0; t < tasks; ++t)
              grad[at + t] += w * pmf_derivative[ac + t];
            if (boundary) heat[j] += 0.5 * w * pmf_second[c];
          }
        }
      }
      for (int j = 0; j < moments; ++j) {
        expected(i, j) += weight[k] * value[j];
        const size_t at = static_cast<size_t>(j) * tasks;
        double common_shift = 0.0;
        for (int t = 0; t < tasks; ++t) {
          const double partial = grad[at + t];
          integrated_grad[at + t] += weight[k] * partial;
          integrated_policy_grad[at + t] += weight[k] * u[k] * partial;
          common_shift += partial;
        }
        integrated_omega[j] += weight[k] * (boundary ? heat[j] :
          v[k] * common_shift / (2.0 * scale));
      }
    }
    // The fixed feature and mean-basis maps commute with latent integration.
    for (int j = 0; j < moments; ++j) {
      const size_t at = static_cast<size_t>(j) * tasks;
      for (int t = 0; t < tasks; ++t) {
        const int row = start + t;
        const double mean_partial = integrated_grad[at + t] / dim.n;
        const double policy_partial = integrated_policy_grad[at + t] / dim.n;
        derivative(j, 0) += mean_partial;
        for (int h = 0; h < dim.r; ++h)
          derivative(j, 1 + h) += a(row, h) * mean_partial;
        for (int h = 0; h < dim.p; ++h)
          derivative(j, 1 + dim.r + h) += d(row, h) * policy_partial;
      }
      derivative(j, dim.h - 1) += integrated_omega[j] / dim.n;
    }
    pair_start += static_cast<int>(static_cast<long long>(tasks) * (tasks - 1) / 2);
    count_start += tasks + 1;
  }
  require_finite(expected, "computed moment expectations");
  require_finite(derivative, "computed moment derivatives");
  return Rcpp::List::create(Rcpp::Named("expected") = expected,
                            Rcpp::Named("derivative") = derivative);
}
