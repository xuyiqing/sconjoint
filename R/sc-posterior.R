# Respondent-level posterior summaries (paper appendix "Population Preferences and Respondent
# Summaries", app:posterior_population). For respondent i with coded task differences d_it
# (y_it = 1 when the first profile is chosen),
#   Pr(y_it = 1 | u, v) = G(kappa + d_it' mu_i + d_it' Lambda u + sqrt(omega) v),
#   u ~ N(0, I_q), v ~ N(0, 1),
# the same parameterization as rum_dnn_gaussian_evaluate() and src/rum_core.cpp. The taste
# vector is beta_i = mu_i + Lambda u and the position effect is a_i = sqrt(omega) v. The
# posterior of z = (u, v) given the respondent's choices is
#   pi(z | Y_i) = L_i(z) phi_{q+1}(z) / p(Y_i),
# integrated with self-normalized importance sampling over a fixed set of draws shared by all
# respondents:
#   "quadrature": the package's Owen-scrambled Sobol rule for (u, v) from the prior, mirrored
#     in u and v (symmetric = "uv"); the weights are proportional to L_i.
#   "adaptive": a Student-t proposal centred at the posterior mode with scale matrix
#     scale * H^{-1} (H the negative Hessian of the log posterior at the mode), built from
#     Sobol points and their antithetic mirrors. A port of long_posterior() in the long-panel
#     author scripts (analysis/2026-11-24_rum_long_panel/posterior_integration.R, with the
#     Newton mode refinement of training_map.R): with the same nodes and seed it uses the same
#     draws.
# With no conditioning tasks the posterior is the prior; it is then computed exactly (normal
# formulas; one-dimensional integrals for predictions), not from draws.
#
# Wiring a fitted object: sc_posterior() is generic. A method for a fit class extracts mu
# (n x p, rows in data$ids order; for cross-fitted fits each respondent's held-out mean),
# kappa, loading and omega and calls .sc_posterior_run().

#' Posterior summaries of respondents' preferences
#'
#' For each respondent, `sc_posterior()` combines the fitted population law of tastes with that
#' respondent's observed choices. The fitted model says that respondent \eqn{i}'s tastes are
#' \eqn{\beta_i = \mu_i + \Lambda u_i} with \eqn{u_i \sim N(0, I_q)}, and that the respondent
#' also has a persistent pull toward the first profile, \eqn{a_i = \sqrt{\omega}\, v_i} with
#' \eqn{v_i \sim N(0, 1)}. Before any choices are seen, \eqn{\beta_i} is normal with mean
#' \eqn{\mu_i} and covariance \eqn{\Lambda\Lambda^\top}: the prior. Each observed choice is
#' more likely under some values of \eqn{(u_i, v_i)} than others, and Bayes' rule turns the
#' prior into the posterior given the choices, holding the fitted parameters fixed.
#'
#' Read the result as a prediction of the respondent's tastes, not as a measurement of them.
#' A respondent who answered five tasks has told us little; the posterior mean then stays
#' close to \eqn{\mu_i}, the population mean for people with the same characteristics, and
#' the posterior SD stays close to the prior SD. Only with many tasks does the posterior
#' concentrate around the respondent's own tastes. The summaries are empirical Bayes: they
#' condition on the fitted parameters and do not carry the sampling uncertainty of the fit.
#' The spread of posterior means across respondents understates the spread of tastes
#' (posterior means are shrunk), and the share of respondents whose posterior mean is
#' positive is not an estimate of a population preference share; use the population targets
#' for those questions.
#'
#' Two integrators are offered. `"adaptive"` (the default) centres a Student-t proposal at the
#' posterior mode with a scale taken from the curvature there, and stays accurate in long
#' panels where the posterior is far narrower than the prior. `"quadrature"` reweights the
#' package's Sobol rule for the prior; it is simple and exact in expectation but loses
#' effective draws as the number of tasks grows. Both use fixed scrambled Sobol points, so the
#' result is reproducible and does not use or change R's random-number state. The
#' `diagnostics` table reports the effective sample size of each respondent's weights.
#'
#' @param object Fitted parameters: a list with `mu` (an n x p matrix of mean tastes, rows in
#'   `data$ids` order), `kappa` (the first-profile intercept), `loading` (a p x q matrix, or a
#'   length-p vector when q = 1) and `omega` (the variance of the position effect). Methods
#'   for fitted model objects can be added.
#' @param data An [sc_data()] object (or the lower-level `rum_data` object it holds) with the
#'   respondents' coded tasks and choices.
#' @param respondents Respondents to summarize, by index or ID. `NULL` (the default) takes all.
#' @param contrasts Optional utility contrasts \eqn{c}. A matrix with one contrast per row and
#'   one column per feature (row names name the contrasts), a single vector of length p, or a
#'   named vector or matrix whose names are a subset of `data$feature_names` (missing
#'   features get 0; any other name is an error). A matrix whose rows are named by features,
#'   or a matrix with p unnamed rows and a different number of columns, is read with one
#'   contrast per column (its column names then name the contrasts).
#' @param newtasks Optional tasks to predict: a matrix of coded task differences (p columns)
#'   used for every respondent, a list with one such matrix per selected respondent (named by
#'   ID or in order), or `"remaining"` to predict each respondent's own tasks after `history`.
#' @param history Number of each respondent's first tasks to condition on: `NULL` (all tasks),
#'   one number, or one per selected respondent. `0` returns the prior.
#' @param method `"adaptive"` or `"quadrature"`; see Details.
#' @param nodes Number of draws (rounded up to a multiple of 2 for `"adaptive"` and of 4 for
#'   `"quadrature"`).
#' @param seed Seed of the Sobol scrambling.
#' @param ... For `"adaptive"`: `df` (degrees of freedom of the proposal, default 5) and
#'   `scale` (variance inflation of the proposal, default 1.2).
#'
#' @return An object of class `sc_posterior`, a list of data frames:
#' \describe{
#'   \item{`beta`}{One row per respondent and feature: `prior_mean` (\eqn{\mu_{ij}}),
#'     `prior_sd`, posterior `mean` and `sd` of \eqn{\beta_{ij}}, and `mc_se`, an importance
#'     sampling standard error of the posterior mean (conservative for Sobol points).}
#'   \item{`contrasts`}{One row per respondent and contrast: prior and posterior mean and SD
#'     of \eqn{c^\top\beta_i}, a 95\% posterior interval (`lower`, `upper`),
#'     `prob_positive` \eqn{= \Pr(c^\top\beta_i > 0 \mid \text{choices})}, and `mc_se`.
#'     `NULL` without contrasts.}
#'   \item{`position`}{Posterior mean and SD of the position effect \eqn{a_i}.}
#'   \item{`predictions`}{One row per predicted task: the posterior predictive probability that
#'     the first profile is chosen (`prob`), the same probability before any choices
#'     (`prior_prob`), and the observed choice `y` for `"remaining"` (otherwise `NA`).
#'     `NULL` without `newtasks`.}
#'   \item{`diagnostics`}{Per respondent: tasks conditioned on, the integration used, the number
#'     of draws, the effective sample size `ess` \eqn{= 1 / \sum_g w_g^2} and its fraction of
#'     the draws, the largest weight, the log marginal likelihood \eqn{\log p(Y_i)} at the fitted
#'     parameters, and (adaptive) the largest gradient of the log posterior at the mode.}
#'   \item{`settings`}{The method, draws, seed and proposal settings.}
#' }
#'
#' @examples
#' set.seed(1)
#' n <- 20; p <- 3; Tn <- 12
#' X <- matrix(sample(c(-1, 0, 1), n * Tn * p, TRUE), n * Tn, p,
#'             dimnames = list(NULL, c("a", "b", "c")))
#' id <- rep(seq_len(n), each = Tn)
#' mu <- matrix(rnorm(n * p, 0, .5), n, p)
#' L <- matrix(c(.8, -.4, .3), p, 1)
#' beta <- mu + rnorm(n) %o% L[, 1]
#' y <- rbinom(n * Tn, 1, plogis(.1 + rowSums(X * beta[id, ]) + sqrt(.3) * rnorm(n)[id]))
#' dat <- sc_data(y, X, id)
#' post <- sc_posterior(list(mu = mu, kappa = .1, loading = L, omega = .3), dat,
#'   respondents = 1:3, contrasts = c(a = 1, b = -1), nodes = 1024)
#' post$contrasts
#' post$diagnostics
#'
#' # A fitted object: each respondent at the fold that held it out
#' toy <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' sc_posterior(toy, respondents = 1:2, nodes = 1024)
#' @export
sc_posterior <- function(object, data, respondents = NULL, contrasts = NULL, newtasks = NULL,
                         history = NULL, method = c("adaptive", "quadrature"), nodes = 4096,
                         seed = 20261114, ...)
  UseMethod("sc_posterior")

#' @rdname sc_posterior
#' @export
sc_posterior.default <- function(object, data, respondents = NULL, contrasts = NULL,
                                 newtasks = NULL, history = NULL,
                                 method = c("adaptive", "quadrature"), nodes = 4096,
                                 seed = 20261114, ...) {
  if (!is.list(object) || !all(c("mu", "kappa", "loading", "omega") %in% names(object)))
    stop("object must be a list with mu, kappa, loading and omega (or a fitted object with ",
         "an sc_posterior() method).")
  .sc_posterior_run(object$mu, object$kappa, object$loading, object$omega, data,
    respondents = respondents, contrasts = contrasts, newtasks = newtasks, history = history,
    method = method, nodes = nodes, seed = seed, ...)
}

.sc_posterior_run <- function(mu, kappa, loading, omega, data, respondents = NULL,
                              contrasts = NULL, newtasks = NULL, history = NULL,
                              method = c("adaptive", "quadrature"), nodes = 4096,
                              seed = 20261114, df = 5, scale = 1.2, ...) {
  if (length(list(...)))
    stop("Unknown argument(s): ", paste(names(list(...)), collapse = ", "), ".")
  method <- match.arg(method)
  if (inherits(data, "sc_data")) data <- data$data
  .rum_check_data(data)
  p <- data$p
  .rum_dnn_matrix(mu, data$n, p, "mu", data$ids)
  q <- .rum_loading_nfac(loading, p)
  if (is.na(q)) stop("loading must be a finite length-p vector or p x q matrix.")
  L <- matrix(as.numeric(loading), p, q)
  if (!is.numeric(kappa) || length(kappa) != 1L || !is.finite(kappa))
    stop("kappa must be one finite number.")
  if (!is.numeric(omega) || length(omega) != 1L || !is.finite(omega) || omega < 0)
    stop("omega must be one finite nonnegative number.")
  for (nm in c("df", "scale")) {
    x <- get(nm)
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0)
      stop(nm, " must be one positive number.")
  }
  nodes <- .rum_dnn_integer(nodes, "nodes")
  if (nodes < 4L) stop("nodes must be at least 4.")
  sel <- .sc_post_respondents(data, respondents)
  hist <- .sc_post_history(data, sel, history)
  C <- .sc_post_contrasts(contrasts, data$feature_names)
  newt <- .sc_post_newtasks(newtasks, data, sel, hist)

  d <- q + 1L
  sd_a <- sqrt(omega)
  rule <- .sc_post_rule(method, q, nodes, seed, df)
  G <- nrow(rule$z)
  prior_sd_beta <- sqrt(rowSums(L^2))
  CL <- if (is.null(C)) NULL else C %*% L                    # K x q contrast loadings
  prior_sd_c <- if (is.null(C)) NULL else sqrt(rowSums(CL^2))

  out_beta <- out_con <- out_pos <- out_pred <- out_diag <- vector("list", length(sel))
  for (k in seq_along(sel)) {
    i <- sel[k]; h <- hist[k]; id <- data$ids[i]
    rows <- data$offsets[i] + seq_len(h)
    mui <- mu[i, ]
    if (h == 0L) {
      fit <- list(integration = "exact prior", draws = NA_integer_, ess = NA_real_,
        max_weight = NA_real_, log_ml = 0, gradient = NA_real_)
      Eu <- numeric(q); Cu <- diag(q); Ev <- 0; Vv <- 1
      mc_beta <- numeric(p)
    } else {
      Di <- data$d[rows, , drop = FALSE]
      b <- kappa + drop(Di %*% mui)
      X <- cbind(Di %*% L, sd_a)
      fit <- .sc_post_weights(rule, method, b, X, data$y[rows], scale)
      z <- fit$z; w <- fit$w
      Eu <- colSums(z[, seq_len(q), drop = FALSE] * w)
      Uc <- sweep(z[, seq_len(q), drop = FALSE], 2L, Eu)
      Cu <- crossprod(Uc * sqrt(w))
      S2 <- crossprod(Uc * w)                                  # sum_g w_g^2 (u - Eu)(u - Eu)'
      Ev <- sum(z[, d] * w); Vv <- sum(w * (z[, d] - Ev)^2)
      mc_beta <- sqrt(pmax(rowSums((L %*% S2) * L), 0))
    }
    out_beta[[k]] <- data.frame(id = id, feature = data$feature_names,
      prior_mean = unname(mui), prior_sd = prior_sd_beta, mean = unname(mui + drop(L %*% Eu)),
      sd = sqrt(pmax(rowSums((L %*% Cu) * L), 0)), mc_se = mc_beta)
    out_pos[[k]] <- data.frame(id = id, prior_sd = sd_a, mean = sd_a * Ev, sd = sd_a * sqrt(Vv))
    if (!is.null(C)) {
      m0 <- drop(C %*% mui)
      if (h == 0L) {
        s0 <- prior_sd_c
        con <- data.frame(mean = m0, sd = s0, lower = m0 + stats::qnorm(.025) * s0,
          upper = m0 + stats::qnorm(.975) * s0,
          prob_positive = ifelse(s0 > 0, stats::pnorm(m0 / pmax(s0, .Machine$double.xmin)),
                                 as.numeric(m0 > 0)), mc_se = 0)
      } else {
        val <- sweep(z[, seq_len(q), drop = FALSE] %*% t(CL), 2L, m0, "+")   # G x K
        con <- do.call(rbind, lapply(seq_len(nrow(C)), function(j) {
          x <- val[, j]; m <- sum(w * x); iv <- .sc_post_interval(x, w)
          data.frame(mean = m, sd = sqrt(sum(w * (x - m)^2)), lower = iv[1L], upper = iv[2L],
            prob_positive = min(1, max(0, sum(w[x > 0]))), mc_se = sqrt(sum(w^2 * (x - m)^2)))
        }))
      }
      out_con[[k]] <- data.frame(id = id, contrast = rownames(C), prior_mean = m0,
        prior_sd = prior_sd_c, con, row.names = NULL)
    }
    if (!is.null(newt)) {
      nt <- newt[[k]]
      if (nrow(nt$d)) {
        bn <- kappa + drop(nt$d %*% mui)
        sn <- sqrt(rowSums((nt$d %*% L)^2) + omega)
        prior_prob <- .sc_post_prior_prob(bn, sn)
        prob <- if (h == 0L) prior_prob else
          drop(stats::plogis(bn + cbind(nt$d %*% L, sd_a) %*% t(z)) %*% w)
        out_pred[[k]] <- data.frame(id = id, task = nt$task, prob = prob,
          prior_prob = prior_prob, y = nt$y)
      }
    }
    out_diag[[k]] <- data.frame(id = id, tasks = h, integration = fit$integration,
      draws = if (h == 0L) NA_integer_ else G, ess = fit$ess,
      ess_fraction = fit$ess / G, max_weight = fit$max_weight,
      log_marginal_likelihood = fit$log_ml, mode_gradient = fit$gradient)
  }
  bind <- function(x) {
    x <- x[!vapply(x, is.null, logical(1))]
    if (!length(x)) return(NULL)
    r <- do.call(rbind, x); rownames(r) <- NULL; r
  }
  structure(list(beta = bind(out_beta), contrasts = if (is.null(C)) NULL else bind(out_con),
    position = bind(out_pos),
    predictions = if (is.null(newt)) NULL else bind(out_pred), diagnostics = bind(out_diag),
    settings = list(method = method, draws = G, seed = as.integer(seed), q = q,
      df = if (method == "adaptive") df else NA_real_,
      scale = if (method == "adaptive") scale else NA_real_)),
    class = "sc_posterior")
}

# Fixed draws shared by all respondents. quadrature: prior draws z = (u, v) from the package
# Sobol rule mirrored in u and v. adaptive: standard Student-t draws in q + 1 dimensions, from
# the first q + 1 coordinates of a (q + 3)-dimensional Sobol set (the next one feeds the
# chi-square; the last is unused) and their antithetic mirrors, interleaved. This is the draw
# set of long_rule() in the long-panel scripts.
.sc_post_rule <- function(method, q, nodes, seed, df) {
  d <- q + 1L
  if (method == "quadrature") {
    s <- .rum_sobol_rule(q, ceiling(nodes / 4), seed, symmetric = "uv")
    return(list(z = cbind(s$u, s$v)))
  }
  s <- .rum_sobol_rule(d + 1L, ceiling(nodes / 2), seed, symmetric = "none")$u
  u <- s[rep(seq_len(nrow(s)), each = 2L), , drop = FALSE] * rep(c(1, -1), nrow(s))
  chi <- stats::qchisq(stats::pnorm(u[, d + 1L]), df = df)
  t <- u[, seq_len(d), drop = FALSE] * sqrt(df / chi)
  if (any(!is.finite(t))) stop("Non-finite proposal draws.")
  list(z = t, df = df,
       log_t = lgamma((df + d) / 2) - lgamma(df / 2) - .5 * d * log(df * pi) -
         .5 * (df + d) * log1p(rowSums(t^2) / df))
}

# Normalized weights, draws and diagnostics for one respondent with offsets b, design X
# (columns: taste factors, then the position draw) and choices y.
.sc_post_weights <- function(rule, method, b, X, y, scale) {
  s <- 2 * y - 1
  loglik <- function(z) colSums(stats::plogis(s * (b + X %*% t(z)), log.p = TRUE))
  if (method == "quadrature") {
    z <- rule$z
    lp <- loglik(z)
    gradient <- NA_real_
  } else {
    m <- .sc_post_mode(b, X, y)
    Cc <- chol(scale * solve(m$H))
    z <- sweep(rule$z %*% Cc, 2L, m$mode, "+")
    lp <- -.5 * rowSums(z^2) - .5 * ncol(z) * log(2 * pi) + loglik(z) -
      (rule$log_t - sum(log(diag(Cc))))
    gradient <- m$gradient
  }
  top <- max(lp)
  e <- exp(lp - top)
  w <- e / sum(e)
  list(z = z, w = w, integration = method, ess = 1 / sum(w^2), max_weight = max(w),
       log_ml = top + log(mean(e)), gradient = gradient)
}

# Posterior mode of z = (u, v) by damped Newton on the strictly convex negative log posterior
#   f(z) = |z|^2 / 2 - sum_t log G(s_t (b_t + x_t' z)).
.sc_post_mode <- function(b, X, y) {
  s <- 2 * y - 1; d <- ncol(X)
  fn <- function(z) .5 * sum(z^2) - sum(stats::plogis(s * (b + drop(X %*% z)), log.p = TRUE))
  z <- numeric(d)
  for (iter in 1:200) {
    pr <- stats::plogis(b + drop(X %*% z))
    g <- z + drop(crossprod(X, pr - y))
    H <- diag(d) + crossprod(X, X * (pr * (1 - pr)))
    if (max(abs(g)) < 1e-10) break
    step <- drop(solve(H, g)); f0 <- fn(z); frac <- 1
    repeat {
      cand <- z - frac * step
      if (fn(cand) <= f0 - 1e-4 * frac * sum(g * step) + 1e-13 || frac < 1e-12) break
      frac <- frac / 2
    }
    z <- cand
  }
  if (max(abs(g)) > 1e-6) stop("The posterior mode search did not converge.")
  list(mode = z, H = H, gradient = max(abs(g)))
}

# Weighted 2.5% and 97.5% quantiles (midpoint rule, as long_interval() in the author scripts).
.sc_post_interval <- function(value, weight) {
  keep <- weight > 0
  value <- value[keep]; weight <- weight[keep]
  o <- order(value)
  x <- cumsum(weight[o]) - .5 * weight[o]
  keep <- !duplicated(x)
  if (sum(keep) < 2L) return(rep(value[o][1L], 2L))
  stats::approx(x[keep], value[o][keep], xout = c(.025, .975), rule = 2, ties = "ordered")$y
}

# Pr(first profile) before any choices: E G(b + s Z), Z ~ N(0, 1).
.sc_post_prior_prob <- function(b, s)
  vapply(seq_along(b), function(j) if (s[j] == 0) stats::plogis(b[j]) else
    stats::integrate(function(x) stats::plogis(b[j] + s[j] * x) * stats::dnorm(x), -Inf, Inf,
      rel.tol = 1e-10, abs.tol = 1e-12)$value, numeric(1))

.sc_post_respondents <- function(data, respondents) {
  if (is.null(respondents)) return(seq_len(data$n))
  if (is.character(respondents)) {
    ix <- match(respondents, data$ids)
    if (anyNA(ix)) stop("Unknown respondent ID(s): ",
                        paste(utils::head(respondents[is.na(ix)], 5), collapse = ", "), ".")
    respondents <- ix
  }
  if (!is.numeric(respondents) || !length(respondents) || anyNA(respondents) ||
      any(respondents != floor(respondents)) || any(respondents < 1 | respondents > data$n) ||
      anyDuplicated(respondents))
    stop("respondents must be unique existing respondent indices or IDs.")
  as.integer(respondents)
}

.sc_post_history <- function(data, sel, history) {
  Ti <- as.integer(data$T[sel])
  if (is.null(history)) return(Ti)
  if (!is.numeric(history) || !(length(history) %in% c(1L, length(sel))) || anyNA(history) ||
      any(history != floor(history)) || any(history < 0))
    stop("history must be NULL, one nonnegative integer, or one per selected respondent.")
  h <- rep_len(as.integer(history), length(sel))
  if (any(h > Ti)) stop("history exceeds a respondent's number of tasks.")
  h
}

.sc_post_contrasts <- function(contrasts, features) {
  if (is.null(contrasts)) return(NULL)
  p <- length(features)
  if (is.list(contrasts) && !is.data.frame(contrasts)) {
    nm <- names(contrasts)
    contrasts <- do.call(rbind, lapply(contrasts, function(x) .sc_post_contrasts(x, features)))
    if (!is.null(nm) && length(nm) == nrow(contrasts)) rownames(contrasts) <- nm
  }
  if (is.data.frame(contrasts)) contrasts <- as.matrix(contrasts)
  if (!is.numeric(contrasts) || any(!is.finite(contrasts)))
    stop("contrasts must be finite numbers.")
  if (is.null(dim(contrasts))) {
    nm <- names(contrasts)
    contrasts <- matrix(contrasts, 1L, dimnames = list(NULL, nm))
  }
  # Names, when given, must be unique feature names (matched by name); unnamed contrasts are positional.
  # One contrast per column (column names then label the contrasts): a matrix whose rows are named by
  # features (and whose columns are not, or are not p), or a matrix with unnamed rows, p of them, and
  # another number of columns.
  given <- function(x) !is.null(x) && any(nzchar(x) & !is.na(x))
  is_features <- function(x) given(x) && all(x %in% features)
  cn <- colnames(contrasts); rn <- rownames(contrasts)
  if (!is_features(cn) && ((is_features(rn) && (given(cn) || ncol(contrasts) != p)) ||
                           (!given(rn) && ncol(contrasts) != p && nrow(contrasts) == p)))
    contrasts <- t(contrasts)
  cn <- colnames(contrasts)
  if (given(cn)) {
    bad <- unique(cn[is.na(cn) | !cn %in% features])
    if (length(bad))
      stop("contrasts named by features must use names from data$feature_names; unknown: ",
           paste(utils::head(bad, 5L), collapse = ", "), ".")
    if (anyDuplicated(cn)) stop("contrasts named by features must name each feature once; repeated: ", paste(unique(cn[duplicated(cn)]), collapse = ", "), ".")
    full <- matrix(0, nrow(contrasts), p, dimnames = list(rownames(contrasts), features))
    full[, cn] <- contrasts
    contrasts <- full
  } else if (ncol(contrasts) != p) {
    stop("contrasts need one column per feature, or names from data$feature_names.")
  }
  rn <- rownames(contrasts)
  if (is.null(rn)) rn <- paste0("contrast", seq_len(nrow(contrasts)))
  rn[!nzchar(rn) | is.na(rn)] <- paste0("contrast", which(!nzchar(rn) | is.na(rn)))
  if (anyDuplicated(rn)) rn <- make.unique(rn)
  dimnames(contrasts) <- list(rn, features)
  contrasts
}

.sc_post_newtasks <- function(newtasks, data, sel, hist) {
  if (is.null(newtasks)) return(NULL)
  p <- data$p
  check <- function(x) {
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.null(dim(x)) && is.numeric(x) && length(x) == p) x <- matrix(x, 1L)
    if (!is.matrix(x) || !is.numeric(x) || ncol(x) != p || any(!is.finite(x)))
      stop("Each newtasks matrix must be finite numeric with one column per feature.")
    if (!is.null(colnames(x)) && !identical(colnames(x), data$feature_names))
      stop("newtasks column names must match data$feature_names.")
    list(d = unname(x), task = seq_len(nrow(x)), y = rep(NA_real_, nrow(x)))
  }
  if (identical(newtasks, "remaining"))
    return(lapply(seq_along(sel), function(k) {
      i <- sel[k]; rr <- data$offsets[i] + seq.int(hist[k] + 1L, length.out = data$T[i] - hist[k])
      list(d = data$d[rr, , drop = FALSE], task = hist[k] + seq_along(rr), y = data$y[rr])
    }))
  if (is.list(newtasks) && !is.data.frame(newtasks)) {
    if (!is.null(names(newtasks)) && all(data$ids[sel] %in% names(newtasks)))
      newtasks <- newtasks[data$ids[sel]]
    if (length(newtasks) != length(sel))
      stop("A newtasks list needs one matrix per selected respondent (named by ID or in order).")
    return(lapply(newtasks, check))
  }
  one <- check(newtasks)
  rep(list(one), length(sel))
}

#' @rdname sc_posterior
#' @param x An `sc_posterior` object.
#' @export
print.sc_posterior <- function(x, ...) {
  dg <- x$diagnostics
  cat("Respondent posteriors:", nrow(dg), "respondents,", x$settings$method, "integration with",
      x$settings$draws, "draws.\n")
  cat("Tasks conditioned on: median", stats::median(dg$tasks), "(range",
      min(dg$tasks), "to", paste0(max(dg$tasks), ")."), "\n")
  if (any(!is.na(dg$ess_fraction)))
    cat("Effective sample size as a share of draws: median",
        format(stats::median(dg$ess_fraction, na.rm = TRUE), digits = 2), ", minimum",
        format(min(dg$ess_fraction, na.rm = TRUE), digits = 2), ".\n")
  cat("Tables: beta", if (!is.null(x$contrasts)) ", contrasts", ", position",
      if (!is.null(x$predictions)) ", predictions", ", diagnostics.\n", sep = "")
  invisible(x)
}
