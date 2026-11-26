# Quadratic-form importance (paper definition "both"): exact gradients against finite differences
# (paper-v4 spec 5.4 item 20), shares on the simplex, and the four-point scale form.

sc_imp_problem <- function(q, seed = 3L) {
  withr::with_seed(seed, {
    p <- 5L; n <- 4L
    mu <- matrix(rnorm(n * p, 0, .7), n, p)
    L <- matrix(rnorm(p * q, 0, .5), p, q)
    QL <- list(.sc_Qlev(p, 1:2, rbind(0, diag(2)), rep(1, 3)),     # a two-dummy attribute
               .sc_Qlev(p, 3L, matrix(1:4), rep(1, 4)),             # a four-point scale
               .sc_Qlev(p, 4:5, rbind(0, diag(2)), c(2, 1, 1)))     # unequal level weights
  })
  list(mu = mu, L = L, QL = QL, p = p, n = n)
}

test_that("Qlev is the variance of a group's utility over its menu", {
  p <- 4L; A <- rbind(0, diag(2)); pi <- c(.5, .25, .25); Q <- .sc_Qlev(p, 2:3, A, pi)
  b <- c(.3, 1, -2, .7); v <- drop(A %*% b[2:3])
  expect_equal(drop(t(b) %*% Q %*% b), sum(pi * v^2) - sum(pi * v)^2)
  expect_equal(Q[c(1, 4), ], matrix(0, 2, 4))
  Qs <- .sc_Qlev(p, 1L, matrix(1:4), rep(1, 4))
  expect_equal(Qs[1, 1], var(1:4) * 3 / 4)
})

test_that("one-factor importance gradients match finite differences", {
  pr <- sc_imp_problem(1L); lam <- drop(pr$L); h <- 1e-6
  s <- .sc_impQ_sums(pr$mu, lam, pr$QL)
  expect_equal(rowSums(s$m), rep(1, pr$n), tolerance = 1e-12)
  for (j in seq_len(pr$p)) {
    m1 <- pr$mu; m2 <- pr$mu; m1[, j] <- m1[, j] + h; m2[, j] <- m2[, j] - h
    fd <- (.sc_impQ_sums(m1, lam, pr$QL)$m - .sc_impQ_sums(m2, lam, pr$QL)$m) / (2 * h)
    expect_equal(s$gm[, j, ], fd, tolerance = 1e-6)
    l1 <- lam; l2 <- lam; l1[j] <- l1[j] + h; l2[j] <- l2[j] - h
    fd <- (.sc_impQ_sums(pr$mu, l1, pr$QL)$m - .sc_impQ_sums(pr$mu, l2, pr$QL)$m) / (2 * h)
    expect_equal(s$gl[, j, ], fd, tolerance = 1e-6)
  }
})

test_that("q-factor importance gradients match finite differences on a fixed rule", {
  pr <- sc_imp_problem(2L); h <- 1e-6
  ctl <- .rum_imp_control(n_start = 2048L, n_max = 2048L)          # a fixed rule: the estimate is smooth
  L <- .rum_canonical_loading(pr$L)
  s <- .sc_impQ_sums(pr$mu, L, pr$QL, control = ctl)
  expect_equal(rowSums(s$m), rep(1, pr$n), tolerance = 1e-12)
  for (j in seq_len(pr$p)) {
    m1 <- pr$mu; m2 <- pr$mu; m1[, j] <- m1[, j] + h; m2[, j] <- m2[, j] - h
    fd <- (.sc_impQ_sums(m1, L, pr$QL, control = ctl)$m - .sc_impQ_sums(m2, L, pr$QL, control = ctl)$m) / (2 * h)
    expect_equal(s$gm[, j, ], fd, tolerance = 1e-6)
  }
  # Loading: the exact (pathwise) gradient of the integral, against finite differences of the same
  # randomised rule; they differ by the rule's integration error (the draws rotate with Lambda's axes),
  # so the check is at the integration tolerance, and the rotation component of the gradient is zero.
  for (j in seq_along(L)) {
    l1 <- L; l2 <- L; l1[j] <- l1[j] + h; l2[j] <- l2[j] - h
    fd <- (.sc_impQ_sums(pr$mu, l1, pr$QL, control = ctl)$m - .sc_impQ_sums(pr$mu, l2, pr$QL, control = ctl)$m) / (2 * h)
    expect_lt(max(abs(s$gl[, j, ] - fd)), 5e-3)
  }
  R <- .rum_rotation_dirs(L)
  for (g in seq_along(pr$QL)) expect_lt(max(abs(cbind(0, s$gl[, , g], 0) %*% R)), 1e-10)
})

test_that("importance targets share one integration per state and sum to one", {
  pr <- sc_imp_problem(2L)
  tg <- .sc_imp_targets(pr$QL, c("a", "b", "c"), pr$p)
  th <- list(kappa = 0, loading = .rum_canonical_loading(pr$L), omega = .2)
  vals <- vapply(tg, function(t) t$m(pr$mu, th), numeric(pr$n))
  expect_equal(rowSums(vals), rep(1, pr$n), tolerance = 1e-12)
  g <- .rum_target_grad(tg[[1]], pr$mu, th)
  expect_identical(dim(g$gt), c(pr$n, .rum_th_len(pr$p, 2L)))
  expect_equal(g$gt[, 1], rep(0, pr$n)); expect_equal(g$gt[, ncol(g$gt)], rep(0, pr$n))
})
