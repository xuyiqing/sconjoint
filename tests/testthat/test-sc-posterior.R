# sc_posterior(): respondent posteriors of (u, v) at fixed parameters, on simulated data with
# known truth. Model: Pr(y = 1) = G(kappa + d'mu_i + d'Lambda u + sqrt(omega) v).
# Small problems, fixed seeds; nothing written to disk.

rq <- function(nm) get(nm, envir = asNamespace("sconjoint"))

post_sim <- function(n = 30L, p = 3L, q = 2L, Tn = 6L, seed = 1L, lsd = .8, omega = .3,
                     kappa = .1, zero_design = FALSE) {
  withr::with_seed(seed, {
    ids <- sprintf("r%03d", seq_len(n)); fn <- paste0("f", seq_len(p))
    d <- lapply(seq_len(n), function(i)
      matrix(if (zero_design) 0 else sample(c(-1, 0, 1), Tn * p, TRUE), Tn, p))
    mu <- matrix(stats::rnorm(n * p, 0, .5), n, p, dimnames = list(ids, fn))
    L <- matrix(stats::rnorm(p * q, 0, lsd), p, q)
    U <- matrix(stats::rnorm(n * q), n, q); V <- stats::rnorm(n)
    beta <- mu + U %*% t(L)
    y <- lapply(seq_len(n), function(i) stats::rbinom(Tn, 1,
      stats::plogis(kappa + drop(d[[i]] %*% beta[i, ]) + sqrt(omega) * V[i])))
  })
  list(data = rum_data(y, d, ids = ids, feature_names = fn,
         metadata = list(orientation = "y = 1: first profile chosen")),
       par = list(mu = mu, kappa = kappa, loading = L, omega = omega), beta = beta)
}

as_mat <- function(x, n, col = "mean") matrix(x[[col]], n, byrow = TRUE)

test_that("the log marginal likelihood matches the package likelihood on the same rule", {
  s <- post_sim(n = 12L, p = 3L, q = 2L, Tn = 5L, seed = 2L)
  out <- sc_posterior(s$par, s$data, method = "quadrature", nodes = 1024, seed = 7)
  rule <- rum_quadrature(n_fac = 2L, rule = "sobol", nodes = 256L, seed = 7L, symmetric = "uv")
  ev <- rum_dnn_gaussian_evaluate(s$data, s$par$mu, s$par$kappa, s$par$loading, s$par$omega, rule)
  expect_equal(out$diagnostics$log_marginal_likelihood, unname(ev$loglik), tolerance = 1e-10)
  expect_true(all(out$diagnostics$draws == 1024L))
  # one factor given as a vector: the same likelihood as a p x 1 loading matrix
  s1 <- post_sim(n = 8L, p = 3L, q = 1L, Tn = 4L, seed = 3L)
  o1 <- sc_posterior(s1$par, s1$data, method = "quadrature", nodes = 1024, seed = 7)
  s1$par$loading <- drop(s1$par$loading)
  o2 <- sc_posterior(s1$par, s1$data, method = "quadrature", nodes = 1024, seed = 7)
  expect_identical(o1$diagnostics, o2$diagnostics)
  r1 <- rum_quadrature(n_fac = 1L, rule = "sobol", nodes = 256L, seed = 7L, symmetric = "uv")
  e1 <- rum_dnn_gaussian_evaluate(s1$data, s1$par$mu, s1$par$kappa, matrix(s1$par$loading),
                                  s1$par$omega, r1)
  expect_equal(o2$diagnostics$log_marginal_likelihood, unname(e1$loglik), tolerance = 1e-10)
})

test_that("adaptive and quadrature integration agree within Monte Carlo error", {
  s <- post_sim(n = 25L, p = 3L, q = 2L, Tn = 6L, seed = 11L)
  C <- rbind(a = c(1, -1, 0), b = c(0, 1, 1))
  a <- sc_posterior(s$par, s$data, contrasts = C, method = "adaptive", nodes = 8192)
  b <- sc_posterior(s$par, s$data, contrasts = C, method = "quadrature", nodes = 16384)
  se <- sqrt(a$beta$mc_se^2 + b$beta$mc_se^2)
  expect_true(all(abs(a$beta$mean - b$beta$mean) <= 4 * se + 1e-3))
  expect_lt(max(abs(a$beta$sd / b$beta$sd - 1)), .05)
  sec <- sqrt(a$contrasts$mc_se^2 + b$contrasts$mc_se^2)
  expect_true(all(abs(a$contrasts$mean - b$contrasts$mean) <= 4 * sec + 1e-3))
  expect_lt(max(abs(a$contrasts$prob_positive - b$contrasts$prob_positive)), .02)
  expect_lt(max(abs(a$contrasts$lower - b$contrasts$lower)), .05)
  expect_lt(max(abs(a$position$mean - b$position$mean)), .02)
  expect_lt(max(abs(a$diagnostics$log_marginal_likelihood -
                    b$diagnostics$log_marginal_likelihood)), .005)
  nt <- s$data$d[1:4, ]
  pa <- sc_posterior(s$par, s$data, newtasks = nt, nodes = 8192)$predictions
  pb <- sc_posterior(s$par, s$data, newtasks = nt, method = "quadrature", nodes = 16384)$predictions
  expect_lt(max(abs(pa$prob - pb$prob)), .005)
})

test_that("with long panels the posterior means track the true tastes", {
  s <- post_sim(n = 120L, p = 3L, q = 2L, Tn = 30L, seed = 21L, lsd = 1)
  out <- sc_posterior(s$par, s$data, nodes = 2048)
  m <- as_mat(out$beta, 120L)
  dev_true <- s$beta - s$par$mu
  r <- diag(stats::cor(m - s$par$mu, dev_true))
  expect_true(all(r > .75))
  # the prior mean predicts beta with error E|Lambda u|^2; the posterior mean does far better
  expect_lt(mean((m - s$beta)^2), .4 * mean(dev_true^2))
  expect_true(all(out$diagnostics$ess_fraction > .3))
  # nominal coverage of the 95% posterior intervals of beta_i1 (empirical Bayes at the truth)
  C <- c(f1 = 1)
  ci <- sc_posterior(s$par, s$data, contrasts = C, nodes = 2048)$contrasts
  cover <- mean(ci$lower <= s$beta[, 1] & s$beta[, 1] <= ci$upper)
  expect_gt(cover, .85)
})

test_that("with no tasks the posterior is the prior, computed exactly", {
  s <- post_sim(n = 6L, p = 3L, q = 2L, Tn = 5L, seed = 4L)
  C <- rbind(c1 = c(1, 0, -1), zero = c(0, 0, 0))
  nt <- rbind(c(1, -1, 0), c(0, 0, 0))
  for (meth in c("adaptive", "quadrature")) {
    out <- sc_posterior(s$par, s$data, history = 0, contrasts = C, newtasks = nt, method = meth)
    L <- s$par$loading
    expect_equal(as_mat(out$beta, 6L), unname(s$par$mu), tolerance = 1e-14)
    expect_equal(out$beta$sd, rep(sqrt(rowSums(L^2)), 6L), tolerance = 1e-14)
    expect_equal(out$beta$sd, out$beta$prior_sd)
    m0 <- drop(s$par$mu %*% C[1, ]); s0 <- sqrt(sum((C[1, ] %*% L)^2))
    c1 <- out$contrasts[out$contrasts$contrast == "c1", ]
    expect_equal(c1$mean, unname(m0), tolerance = 1e-14)
    expect_equal(c1$sd, rep(s0, 6L), tolerance = 1e-14)
    expect_equal(c1$prob_positive, unname(stats::pnorm(m0 / s0)), tolerance = 1e-14)
    expect_equal(c1$upper - c1$lower, rep(2 * stats::qnorm(.975) * s0, 6L), tolerance = 1e-12)
    z0 <- out$contrasts[out$contrasts$contrast == "zero", ]
    expect_equal(z0$prob_positive, rep(0, 6L))
    expect_equal(out$position$mean, rep(0, 6L))
    expect_equal(out$position$sd, rep(sqrt(s$par$omega), 6L))
    pr <- out$predictions
    expect_equal(pr$prob, pr$prior_prob)
    expect_equal(pr$prob[pr$task == 2], rep(stats::integrate(function(x)
      stats::plogis(s$par$kappa + sqrt(s$par$omega) * x) * stats::dnorm(x), -Inf, Inf,
      rel.tol = 1e-12)$value, 6L), tolerance = 1e-8)
    expect_true(all(out$diagnostics$tasks == 0L))
    expect_true(all(out$diagnostics$log_marginal_likelihood == 0))
  }
  # Tasks with no attribute differences carry no information on tastes: beta keeps its
  # prior law (up to integration error) while the position effect moves.
  z <- post_sim(n = 6L, p = 3L, q = 2L, Tn = 8L, seed = 5L, zero_design = TRUE)
  for (meth in c("adaptive", "quadrature")) {
    out <- sc_posterior(z$par, z$data, method = meth, nodes = 8192)
    expect_lt(max(abs(as_mat(out$beta, 6L) - z$par$mu)), .02)
    expect_lt(max(abs(out$beta$sd / out$beta$prior_sd - 1)), .02)
    yb <- vapply(split(z$data$y, rep(seq_len(6L), z$data$T)), mean, numeric(1))
    expect_true(all(sign(out$position$mean[yb != .5] - 0) ==
                    sign(yb[yb != .5] - stats::plogis(z$par$kappa) + 0)))
    expect_true(all(out$position$sd < sqrt(z$par$omega)))
  }
})

test_that("with few tasks the posterior shrinks toward mu_i", {
  s <- post_sim(n = 60L, p = 3L, q = 2L, Tn = 30L, seed = 31L, lsd = 1)
  hs <- c(0L, 2L, 10L, 30L)
  res <- lapply(hs, function(h) sc_posterior(s$par, s$data, history = h, nodes = 2048))
  shift <- vapply(res, function(o) mean(abs(o$beta$mean - o$beta$prior_mean)), numeric(1))
  sdr <- vapply(res, function(o) mean(o$beta$sd / o$beta$prior_sd), numeric(1))
  expect_true(all(diff(shift) > 0))
  expect_true(all(diff(sdr) < 0))
  expect_equal(sdr[1], 1)
  expect_gt(sdr[2], .8)                                   # two tasks: still mostly the prior
  expect_lt(shift[2], .5 * mean(abs(s$beta - s$par$mu)))  # and close to mu_i
  expect_lt(sdr[4], .6)
  # law of total variance: posterior means are less dispersed than the prior law
  v_means <- mean(apply(as_mat(res[[2]]$beta, 60L) - s$par$mu, 2, stats::var))
  expect_lt(v_means, mean(rowSums(s$par$loading^2)))
})

test_that("probabilities lie in [0, 1] and ESS diagnostics behave", {
  s <- post_sim(n = 20L, p = 3L, q = 2L, Tn = 30L, seed = 41L, lsd = 1)
  C <- rbind(a = c(1, 0, 0), b = c(1, -1, 1))
  qd <- lapply(c(2L, 10L, 30L), function(h)
    sc_posterior(s$par, s$data, contrasts = C, history = h, method = "quadrature",
                 nodes = 2048, newtasks = "remaining"))
  ad <- sc_posterior(s$par, s$data, contrasts = C, nodes = 2048, newtasks = "remaining",
                     history = 25)
  for (o in c(qd, list(ad))) {
    expect_true(all(o$contrasts$prob_positive >= 0 & o$contrasts$prob_positive <= 1))
    expect_true(all(o$contrasts$lower <= o$contrasts$mean & o$contrasts$mean <= o$contrasts$upper))
    if (!is.null(o$predictions) && nrow(o$predictions))
      expect_true(all(o$predictions$prob > 0 & o$predictions$prob < 1))
    dg <- o$diagnostics
    expect_true(all(dg$ess >= 1 & dg$ess <= dg$draws + 1e-8))
    expect_true(all(dg$max_weight >= 1 / dg$draws & dg$max_weight <= 1))
  }
  ess <- vapply(qd, function(o) stats::median(o$diagnostics$ess_fraction), numeric(1))
  expect_true(all(diff(ess) < 0))                    # prior draws lose ESS as tasks accumulate
  expect_gt(min(ad$diagnostics$ess_fraction), .3)    # the adaptive proposal does not
  expect_true(all(ad$diagnostics$mode_gradient < 1e-6))
  expect_equal(nrow(ad$predictions), 20L * 5L)
  expect_equal(unique(ad$predictions$task), 26:30)
  expect_equal(ad$predictions$y, s$data$y[as.vector(outer(26:30, s$data$offsets[1:20], "+"))])
  expect_null(qd[[3]]$predictions)                   # history = all tasks: nothing remains
})

test_that("inputs are parsed and checked", {
  s <- post_sim(n = 5L, p = 3L, q = 1L, Tn = 4L, seed = 6L)
  a <- sc_posterior(s$par, s$data, respondents = c("r002", "r004"), nodes = 256,
    contrasts = c(f1 = 1, f3 = -1))
  b <- sc_posterior(s$par, s$data, respondents = c(2, 4), nodes = 256,
    contrasts = matrix(c(1, 0, -1), 3, 1, dimnames = list(NULL, "d13")))
  expect_identical(a$beta, b$beta)
  expect_equal(a$contrasts$mean, b$contrasts$mean)
  expect_identical(b$contrasts$contrast, c("d13", "d13"))
  l <- sc_posterior(s$par, s$data, respondents = 2:3, nodes = 256,
    contrasts = list(x = c(f2 = 1), y = c(1, 1, 1)))
  expect_identical(unique(l$contrasts$contrast), c("x", "y"))
  # the same draws every call, and R's random-number state is untouched
  set.seed(9); before <- .Random.seed
  a2 <- sc_posterior(s$par, s$data, respondents = c("r002", "r004"), nodes = 256,
    contrasts = c(f1 = 1, f3 = -1))
  expect_identical(.Random.seed, before)
  expect_identical(a, a2)
  nt <- list(r002 = diag(3), r004 = matrix(1, 1, 3))
  p <- sc_posterior(s$par, s$data, respondents = c("r002", "r004"), newtasks = nt, nodes = 256)
  expect_equal(p$predictions$id, c("r002", "r002", "r002", "r004"))
  expect_s3_class(a, "sc_posterior")
  expect_output(print(a), "Respondent posteriors")
  expect_error(sc_posterior(list(mu = s$par$mu), s$data), "kappa, loading and omega")
  expect_error(sc_posterior(s$par, s$data, respondents = "nobody"), "Unknown respondent")
  expect_error(sc_posterior(s$par, s$data, history = 9), "exceeds")
  expect_error(sc_posterior(s$par, s$data, contrasts = c(zz = 1)), "contrasts")
  expect_error(sc_posterior(s$par, s$data, bad = 1), "Unknown argument")
  expect_error(sc_posterior(modifyList(s$par, list(omega = -1)), s$data), "omega")
  bad <- s$par; bad$mu <- bad$mu[-1, ]
  expect_error(sc_posterior(bad, s$data), "mu")
  # zero loading and zero omega: tastes equal mu_i exactly
  flat <- modifyList(s$par, list(loading = rep(0, 3), omega = 0))
  o <- sc_posterior(flat, s$data, nodes = 256)
  expect_equal(as_mat(o$beta, 5L), unname(s$par$mu))
  expect_true(all(o$beta$sd == 0))
})
