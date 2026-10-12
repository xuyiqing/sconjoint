# Wrong-model truths for RUM coverage simulations (R/rum-simulate-misspec.R).

ms_toy <- function(n = 400L, spec = "br", seed = 11L) {
  sp <- sconjoint:::.rum_spec_arg(spec); p <- sp$p
  sconjoint:::.rum_with_seed(seed, {
    mu <- matrix(stats::rnorm(n * p, sd = 0.3), n, p)
    lam <- stats::rnorm(p, sd = 0.25); lam2 <- stats::rnorm(p, sd = 0.25)
  })
  dimnames(mu) <- list(sprintf("r%04d", seq_len(n)), sp$features)
  list(mu = mu, lam = lam, lam2 = lam2, kappa = 0.1, omega = 0.3, spec = sp)
}

test_that("the simulator is reproducible by seed and leaves the caller's RNG alone", {
  X <- ms_toy()
  tr <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, loading2 = X$lam2, pi_sl = 0.1)
  set.seed(5); before <- .Random.seed
  s1 <- sconjoint:::.rum_ms_simulate(tr, X$spec, seed = 101)
  expect_identical(.Random.seed, before)
  s2 <- sconjoint:::.rum_ms_simulate(tr, X$spec, seed = 101)
  s3 <- sconjoint:::.rum_ms_simulate(tr, X$spec, seed = 102)
  expect_identical(s1$y, s2$y); expect_identical(s1$d, s2$d)
  expect_identical(s1$metadata$simulation$latent, s2$metadata$simulation$latent)
  expect_false(identical(s1$y, s3$y))
  expect_s3_class(s1, "rum_data")
  expect_equal(s1$T, rep(8L, 400L))
})

test_that("with L2 = 0 the two-factor truth is the one-factor model: same draws, same choices", {
  X <- ms_toy()
  t0 <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega)
  tz <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, loading2 = rep(0, ncol(X$mu)))
  a <- sconjoint:::.rum_ms_simulate(t0, X$spec, seed = 7); b <- sconjoint:::.rum_ms_simulate(tz, X$spec, seed = 7)
  expect_identical(a$y, b$y); expect_identical(a$d, b$d)
  # an independent one-factor reference: the S-A harness's simulate_data recipe
  n <- nrow(X$mu); crn <- sconjoint:::.rum_draw_designs_crn(X$spec, NULL, M = n, seed = 7, n = n)
  dr <- sconjoint:::.rum_with_seed(7 + 1000003L, list(u = stats::rnorm(n), v = stats::rnorm(n), U = stats::runif(n * 8L)))
  D <- matrix(aperm(crn$designs[[1]], c(2, 1, 3)), n * 8L, ncol(X$mu))   # respondent-major task rows
  ri <- rep(seq_len(n), each = 8L)
  eta <- X$kappa + rowSums(D * (X$mu[ri, ] + outer(dr$u[ri], X$lam))) + sqrt(X$omega) * dr$v[ri]
  expect_equal(unname(a$d), unname(D))
  expect_identical(a$y, as.numeric(dr$U < stats::plogis(eta)))
  # a nonzero L2 changes choices only through u2: designs and u1, v unchanged
  t2 <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, loading2 = X$lam2)
  c2 <- sconjoint:::.rum_ms_simulate(t2, X$spec, seed = 7)
  expect_identical(c2$d, a$d)
  expect_identical(c2$metadata$simulation$latent$u1, a$metadata$simulation$latent$u1)
  expect_false(identical(c2$y, a$y))
  # pi = 0 draws no straight-liners; pi > 0 keeps every attentive respondent's choices
  t3 <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, pi_sl = 0.3)
  c3 <- sconjoint:::.rum_ms_simulate(t3, X$spec, seed = 7)
  att <- rep(!c3$metadata$simulation$latent$straight, 8L)[order(rep(seq_len(n), 8L))]
  expect_identical(c3$y[att], a$y[att])
})

test_that("the straight-liner fraction is recovered from simulated data", {
  X <- ms_toy(n = 3000L, seed = 3L)
  tr <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, pi_sl = 0.08, sl_side1 = 0.7)
  s <- sconjoint:::.rum_ms_simulate(tr, X$spec, seed = 21)
  lat <- s$metadata$simulation$latent
  expect_lt(abs(mean(lat$straight) - 0.08), 4 * sqrt(0.08 * 0.92 / 3000))
  # every drawn straight-liner chose one side throughout
  S <- rowsum(s$y, rep(seq_len(s$n), s$T))[, 1]
  expect_true(all(S[lat$straight] %in% c(0, 8)))
  # recovery from the data alone: observed same-side count against the attentive model's expectation
  dirs <- cbind(x = s$d[, 11]); sc <- 1
  ob <- sconjoint:::.rum_ms_observed_moments(s, dirs, sc)
  ex <- sconjoint:::.rum_ms_expected_moments(sconjoint:::.rum_ms_one_factor(tr), s, dirs, sc)
  cal <- sconjoint:::.rum_ms_calibrate_pi(sum(ob$same), sum(ex$same), sum(ob$all1), sum(ex$all1), s$n)
  se <- sqrt(0.08 * 0.92 / 3000 + sum(ex$same * (1 - ex$same)) / 3000^2)
  expect_lt(abs(cal[["pi"]] - 0.08), 4 * se)
  expect_lt(abs(cal[["q"]] - 0.7), 0.15)
})

test_that("expected fit-check moments agree with simulated data", {
  X <- ms_toy(n = 4000L, seed = 4L)
  tr <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, loading2 = X$lam2, pi_sl = 0.05, sl_side1 = 0.6)
  s <- sconjoint:::.rum_ms_simulate(tr, X$spec, seed = 31)
  dirs <- cbind(top = s$d[, 11], prog = drop(s$d[, seq(1, 11, 2)] %*% c(-5, -3, -1, 1, 3, 5)))
  sc <- sqrt(colMeans(dirs^2))
  ob <- sconjoint:::.rum_ms_observed_moments(s, dirs, sc)
  ex <- sconjoint:::.rum_ms_expected_moments(tr, s, dirs, sc)
  r <- ob$pair - ex$pair
  z <- colMeans(r) / (apply(r, 2, stats::sd) / sqrt(s$n))
  expect_true(all(abs(z) < 4))
  zs <- (sum(ob$same) - sum(ex$same)) / sqrt(sum(ex$same * (1 - ex$same)))
  expect_lt(abs(zs), 4)
  # the expected moments of the two-factor truth differ from the one-factor reading
  ex1 <- sconjoint:::.rum_ms_expected_moments(sconjoint:::.rum_ms_one_factor(tr), s, dirs, sc)
  expect_gt(max(abs(colMeans(ex$pair) - colMeans(ex1$pair))), 1e-3)
})

test_that("true values reduce to the package targets under one factor", {
  X <- ms_toy(n = 60L, seed = 5L); FN <- colnames(X$mu); p <- length(FN)
  tr <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega)
  th <- list(kappa = X$kappa, loading = X$lam, omega = X$omega)
  cc <- setNames(numeric(p), FN); cc[c(11, 12)] <- c(1, -0.5)
  d2 <- rbind(sign(seq_len(p) %% 3 - 1) * 0.5)
  raw <- sconjoint:::.rum_design_draw_raw("br", 20L, seed = 9)
  tg <- list(sconjoint:::.rum_tgt_mean("mean", cc),
             sconjoint:::.rum_tgt_share("share", cc, w = rep(c(1, 0), 30)),
             sconjoint:::.rum_tgt_choice_set_pref("win", d2),
             sconjoint:::.rum_tgt_choice_set("win (position)", d2),
             sconjoint:::.rum_tgt_switch_ame("ame", "br", raw, "r6", 6L, 1L, position = FALSE),
             sconjoint:::.rum_tgt_switch_ame("ame (position)", "br", raw, "r6", 6L, 1L, position = TRUE))
  tv <- sconjoint:::.rum_ms_true_values(tr, tg)
  pk <- vapply(tg, function(t) { w <- if (is.null(t$w)) rep(1, 60) else t$w; sum(w * t$m(X$mu, th)) / sum(w) }, 0)
  expect_equal(tv$value, unname(pk), tolerance = 1e-7)
  expect_equal(tv$value, tv$one_factor)
  expect_equal(tv$scope, c("taste", "taste", "taste", "behaviour", "taste", "behaviour"))
})

test_that("true values under two factors and straight-liners match brute-force Monte Carlo", {
  X <- ms_toy(n = 40L, seed = 6L); FN <- colnames(X$mu); p <- length(FN); n <- 40L
  tr <- sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, loading2 = X$lam2, pi_sl = 0.2, sl_side1 = 0.65)
  cc <- setNames(numeric(p), FN); cc[c(1, 11)] <- c(-1, 1)
  d2 <- rbind(sign(seq_len(p) %% 3 - 1) * 0.5)
  raw <- sconjoint:::.rum_design_draw_raw("br", 15L, seed = 9)
  tg <- list(sconjoint:::.rum_tgt_share("share", cc),
             sconjoint:::.rum_tgt_choice_set_pref("win", d2),
             sconjoint:::.rum_tgt_choice_set("win (position)", d2),
             sconjoint:::.rum_ms_switch_target("ame", "br", raw, "r6", 6L, 1L, position = FALSE),
             sconjoint:::.rum_ms_switch_target("ame (position)", "br", raw, "r6", 6L, 1L, position = TRUE))
  tv <- sconjoint:::.rum_ms_true_values(list(c = tr), tg)
  expect_true(all(tv$quad_err < 1e-6))
  expect_true(all(is.na(tv$ctx_se[1:3])) && all(tv$ctx_se[4:5] >= 0))
  # brute force: R draws of (u1, u2, v, straight-liner, side) per respondent
  R <- 20000L
  mc <- sconjoint:::.rum_with_seed(77, {
    u1 <- matrix(stats::rnorm(n * R), n); u2 <- matrix(stats::rnorm(n * R), n); v <- matrix(stats::rnorm(n * R), n)
    sl <- matrix(stats::runif(n * R) < 0.2, n); s1 <- matrix(stats::runif(n * R) < 0.65, n)
    beta_c <- function(c) drop(X$mu %*% c) + u1 * sum(X$lam * c) + u2 * sum(X$lam2 * c)
    rowsP <- function(Rw, position) {
      out <- 0
      for (r in seq_len(nrow(Rw$D))) {
        e <- beta_c(Rw$D[r, ]); if (position) e <- e + Rw$side[r] * (X$kappa + sqrt(X$omega) * v)
        P <- stats::plogis(e)
        if (position) P <- ifelse(sl, ifelse(Rw$side[r] > 0, s1, !s1), P)
        out <- out + Rw$coef[r] * mean(P)
      }
      out
    }
    tgp <- tg[[3]]; e3 <- environment(tgp$grad)
    c(mean(beta_c(cc) > 0), rowsP(list(D = d2, side = 1, coef = 1), FALSE),
      rowsP(list(D = rbind(d2, d2), side = c(1, -1), coef = c(0.5, 0.5)), TRUE),
      rowsP(tg[[4]]$rows[[1]], FALSE), rowsP(tg[[5]]$rows[[1]], TRUE))
  })
  # Monte Carlo SE <= 0.5 / sqrt(n R) per probability; the AMEs average differences over rows
  expect_true(all(abs(tv$value - mc) < 0.006))
  # straight-liners leave taste targets alone and shrink the behavioural switch AME by 1 - pi
  tv0 <- sconjoint:::.rum_ms_true_values(list(a = sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, loading2 = X$lam2)), tg)
  expect_equal(tv$value[c(1, 2, 4)], tv0$value[c(1, 2, 4)])
  expect_equal(tv$value[5], 0.8 * tv0$value[5], tolerance = 1e-12)
  expect_equal(tv$value[3], 0.8 * tv0$value[3] + 0.2 * 0.5, tolerance = 1e-12)
})

test_that("truth construction validates its inputs", {
  X <- ms_toy(n = 10L)
  expect_error(sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam[-1], X$omega), "loading")
  expect_error(sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, -1), "omega")
  expect_error(sconjoint:::.rum_ms_truth(X$mu, X$kappa, X$lam, X$omega, pi_sl = 1.5), "pi_sl")
  expect_error(sconjoint:::.rum_ms_truth(function(Z) Z, X$kappa, X$lam, X$omega), "supply Z")
  tr <- sconjoint:::.rum_ms_truth(function(Z) Z, X$kappa, X$lam, X$omega, Z = X$mu)
  expect_identical(tr$mu, X$mu)
  expect_identical(tr$type, "one-factor RUM")
  expect_identical(sconjoint:::.rum_ms_truth(X$mu, 0, X$lam, 0, loading2 = X$lam2, pi_sl = .1)$type,
                   "two-factor + straight-liners")
})
