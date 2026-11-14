# Stage 1a of the q-factor build (2026-11-14): the likelihood, quadrature, trainer bound,
# polish and principal-axes rotation with n_fac taste factors,
#   eta_it = kappa + d_it' mu_i + d_it' Lambda u_i + sqrt(omega) v_i,  u_i ~ N(0, I_q).
# Small simulated problems; single-threaded; nothing written outside tempdir().

rq <- function(nm) get(nm, envir = asNamespace("sconjoint"))
normal_rule <- rq(".rum_normal_rule")

qf_problem <- function(n = 30L, p = 3L, n_fac = 2L, seed = 5L, lsd = .7) {
  set.seed(seed)
  ids <- sprintf("r%02d", seq_len(n))
  Tn <- sample(3:5, n, TRUE)
  d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), p * Tn[i], TRUE), Tn[i], p,
                                               dimnames = list(NULL, paste0("f", seq_len(p)))))
  mu <- matrix(rnorm(n * p, 0, .6), n, p, dimnames = list(ids, paste0("f", seq_len(p))))
  L <- matrix(rnorm(p * n_fac, 0, lsd), p, n_fac)
  y <- lapply(seq_len(n), function(i) {
    u <- rnorm(n_fac); eta <- .2 + d[[i]] %*% (mu[i, ] + L %*% u) + sqrt(.4) * rnorm(1)
    as.numeric(runif(Tn[i]) < plogis(drop(eta)))
  })
  list(data = rum_data(y, d, ids = ids, metadata = list(orientation = "y = 1 picks the first profile")), mu = mu, kappa = .2, L = L, omega = .4)
}

# Plain-R integrator on its own product Gauss-Hermite grid (no package quadrature code).
brute_loglik <- function(data, mu, kappa, L, omega, orders) {
  L <- as.matrix(L); q <- ncol(L)
  rules <- lapply(orders, normal_rule)
  g <- as.matrix(expand.grid(lapply(rules, function(r) seq_along(r$nodes))))
  U <- sapply(seq_len(q), function(j) rules[[j]]$nodes[g[, j]])
  V <- rules[[q + 1L]]$nodes[g[, q + 1L]]
  W <- apply(sapply(seq_len(q + 1L), function(j) rules[[j]]$weights[g[, j]]), 1, prod)
  W <- W / sum(W)
  vapply(seq_len(data$n), function(i) {
    rows <- seq.int(data$offsets[i] + 1L, data$offsets[i + 1L])
    di <- data$d[rows, , drop = FALSE]; y <- data$y[rows]
    eta <- drop(kappa + di %*% mu[i, ]) + di %*% L %*% t(U) + matrix(sqrt(omega) * V, length(rows), length(V), byrow = TRUE)
    lp <- colSums(y * plogis(eta, log.p = TRUE) + (1 - y) * plogis(-eta, log.p = TRUE))
    m <- max(lp); m + log(sum(W * exp(lp - m)))
  }, 0)
}

ev_ll <- function(P, mu = P$mu, kappa = P$kappa, L = P$L, omega = P$omega, quad)
  rum_dnn_gaussian_evaluate(P$data, mu, kappa, L, omega, quad)

for (q in 2:3) {
  orders <- if (q == 2L) c(15L, 15L, 13L) else c(9L, 9L, 9L, 11L)
  P <- qf_problem(n_fac = q, seed = 4L + q, lsd = if (q == 2L) .7 else .4)
  quad <- rum_quadrature(orders, n_fac = q, rule = "product")

  test_that(paste0("q=", q, ": product rule layout, nodes and weights"), {
    expect_identical(dim(quad$u), c(as.integer(prod(orders)), q))
    expect_equal(sum(quad$weight), 1, tolerance = 1e-14)
    expect_equal(colSums(quad$weight * quad$u), rep(0, q), tolerance = 1e-12)
    expect_equal(crossprod(quad$u * sqrt(quad$weight)), diag(q), tolerance = 1e-12)
    expect_equal(sum(quad$weight * quad$v^2), 1, tolerance = 1e-12)
    expect_identical(rq(".rum_quad_nfac")(quad), q)
  })

  test_that(paste0("q=", q, ": evaluator log likelihood equals a plain-R integrator"), {
    ev <- ev_ll(P, quad = quad)
    expect_equal(unname(ev$loglik), brute_loglik(P$data, P$mu, P$kappa, P$L, P$omega, orders),
                 tolerance = 1e-11)
    # a much finer grid moves the answer only by quadrature error
    fine <- if (q == 2L) c(25L, 25L, 21L) else c(13L, 13L, 13L, 17L)
    expect_lt(max(abs(ev$loglik - brute_loglik(P$data, P$mu, P$kappa, P$L, P$omega, fine))), 2e-4)
    expect_identical(dim(ev$objective_gradient$loading), c(P$data$p, q))
    expect_identical(ncol(ev$common_score), P$data$p * q + 2L)
  })

  test_that(paste0("q=", q, ": scores match central finite differences (incl. the p x q loading)"), {
    ev <- ev_ll(P, quad = quad); h <- 1e-5
    ll <- function(...) ev_ll(P, ..., quad = quad)$loglik
    expect_equal(unname(ev$common_score[, 1]), unname((ll(kappa = P$kappa + h) - ll(kappa = P$kappa - h)) / (2 * h)),
                 tolerance = 1e-7)
    for (j in seq_len(P$data$p)) {
      M1 <- P$mu; M1[, j] <- M1[, j] + h; M0 <- P$mu; M0[, j] <- M0[, j] - h
      expect_equal(unname(ev$output_score[, j]), unname((ll(mu = M1) - ll(mu = M0)) / (2 * h)), tolerance = 1e-7)
    }
    ix <- rq(".rum_th_idx")(P$data$p, q)
    for (e in seq_len(P$data$p * q)) {
      L1 <- P$L; L1[e] <- L1[e] + h; L0 <- P$L; L0[e] <- L0[e] - h
      expect_equal(unname(ev$common_score[, ix$loading[e]]), unname((ll(L = L1) - ll(L = L0)) / (2 * h)),
                   tolerance = 1e-7)
    }
    expect_equal(unname(ev$common_score[, ix$omega]),
                 unname((ll(omega = P$omega + h) - ll(omega = P$omega - h)) / (2 * h)), tolerance = 1e-7)
    # omega = 0: the right derivative (second-order one-sided difference)
    e0 <- ev_ll(P, omega = 0, quad = quad); h0 <- 1e-5
    fd <- (-3 * ll(omega = 0) + 4 * ll(omega = h0) - ll(omega = 2 * h0)) / (2 * h0)
    expect_equal(unname(e0$common_score[, ix$omega]), unname(fd), tolerance = 1e-5)
    expect_identical(e0$gradient_convention, "omega_right_derivative")
  })

  test_that(paste0("q=", q, ": scrambled Sobol agrees with the product rule"), {
    s512 <- rum_quadrature(n_fac = q)                                 # auto -> Sobol 512
    expect_identical(s512$rule, "sobol"); expect_identical(dim(s512$u), c(512L, q))
    expect_equal(s512$weight, rep(1 / 512, 512))
    expect_identical(s512, rum_quadrature(n_fac = q, rule = "sobol", nodes = 512L, seed = 20261114L))
    s4k <- rum_quadrature(n_fac = q, nodes = 4096L)
    expect_identical(s4k$u[1:512, , drop = FALSE], s512$u)          # nested sequence
    ref <- ev_ll(P, quad = quad)$loglik
    d512 <- ev_ll(P, quad = s512)$loglik - ref; d4k <- ev_ll(P, quad = s4k)$loglik - ref
    expect_lt(abs(mean(d512)), 5e-3); expect_lt(max(abs(d512)), 2e-2)
    expect_lt(abs(mean(d4k)), 1e-3)
    # R's random-number state is untouched by the Sobol rule
    set.seed(1); s0 <- .Random.seed; rum_quadrature(n_fac = q, seed = 99L)
    expect_identical(.Random.seed, s0)
    expect_false(identical(rum_quadrature(n_fac = q, seed = 99L)$u, s512$u))
  })

  test_that(paste0("q=", q, ": rotation leaves Lambda Lambda' and the likelihood unchanged"), {
    set.seed(30 + q)
    Q <- qr.Q(qr(matrix(rnorm(q * q), q)))
    LQ <- P$L %*% Q
    fine <- if (q == 2L) c(31L, 31L, 15L) else c(15L, 15L, 15L, 15L)
    fq <- rum_quadrature(fine, n_fac = q, rule = "product")
    a <- ev_ll(P, quad = fq); b <- ev_ll(P, L = LQ, quad = fq)
    tol <- if (q == 2L) 1e-7 else 1e-5      # quadrature error of the finite rule
    expect_lt(max(abs(a$loglik - b$loglik)), tol)
    # the loading score rotates with Lambda: d/dLambda at Lambda Q equals (d/dLambda at Lambda) Q
    expect_equal(b$objective_gradient$loading, a$objective_gradient$loading %*% Q, tolerance = 100 * tol)
    canon <- rq(".rum_canonical_loading")
    C <- canon(P$L)
    expect_equal(tcrossprod(C), tcrossprod(P$L), tolerance = 1e-12)
    expect_equal(canon(LQ), C, tolerance = 1e-10)                    # same axes from any rotation
    expect_equal(canon(C), C, tolerance = 1e-12)                     # idempotent
    cc <- crossprod(C); expect_lt(max(abs(cc[upper.tri(cc)])), 1e-12)
    expect_true(all(diff(diag(cc)) <= 0))                            # decreasing variance
    expect_true(all(apply(C, 2, function(x) x[which.max(abs(x))] > 0)))
    V <- rq(".rum_canonical_rotation")(P$L); expect_equal(crossprod(V), diag(q), tolerance = 1e-12)
  })
}

test_that("row-length bound: projection, bounds layout, Adam step", {
  proj <- rq(".rum_project_rows")
  par <- c(9, 1, 2, 2, 0, 0, 4)        # position 1 is not a loading; rows (1,0), (2,0), (2,4)
  rows <- list(index = matrix(c(2L, 3L, 4L, 5L, 6L, 7L), 3, 2), radius = 3)
  out <- proj(par, rows)
  len <- sqrt(rowSums(matrix(out$par[rows$index], 3)^2))
  expect_equal(len, pmin(sqrt(rowSums(matrix(par[rows$index], 3)^2)), 3))
  expect_identical(out$par[1], 9)
  expect_identical(out$projected, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
  # rows inside the ball are untouched; the projected row keeps its direction
  expect_equal(out$par[c(4, 7)] / sqrt(sum(out$par[c(4, 7)]^2)), c(2, 4) / sqrt(20))
  # trainer bounds: one factor keeps the box, several factors use rows
  net <- rum_dnn_gaussian(2L, 3L, 2L, seed = 1L)
  ctl <- rq(".rum_dnn_train_control")(list(), 1L)
  b1 <- rq(".rum_dnn_train_bounds")(list(network = net, kappa = 0, loading = rep(.1, 3), omega = .5), ctl)
  expect_identical(names(b1), c("lower", "upper"))
  b2 <- rq(".rum_dnn_train_bounds")(list(network = net, kappa = 0, loading = matrix(.1, 3, 2), omega = .5), ctl)
  expect_identical(names(b2), c("lower", "upper", "rows"))
  expect_identical(c(b2$rows$index), net$parameter_count + 1L + 1:6)
  expect_identical(b2$rows$radius, 3)
  expect_true(all(is.infinite(b2$lower[b2$rows$index])))
  expect_error(rq(".rum_loading_radius")(modifyList(ctl, list(loading_bounds = c(-2, 3)))), "symmetric")
  # Adam with rows: same as the box step followed by the row projection
  adam <- rq(".rum_dnn_adam_step")
  set.seed(2); x <- rnorm(7); g <- rnorm(7) * 50
  r0 <- adam(x, g, numeric(7), numeric(7), 1L, 2, .9, .999, 1e-8, rep(-Inf, 7), rep(Inf, 7))
  r1 <- adam(x, g, numeric(7), numeric(7), 1L, 2, .9, .999, 1e-8, rep(-Inf, 7), rep(Inf, 7), rows)
  expect_equal(r1$par, proj(r0$par, rows)$par)
  expect_true(all(sqrt(rowSums(matrix(r1$par[rows$index], 3)^2)) <= 3 + 1e-12))
  # one factor: the row bound is the box (projection of a scalar onto [-3, 3] is a clamp)
  one <- list(index = matrix(2:4, 3, 1), radius = 3)
  z <- c(1, -5, 2, 7); expect_identical(proj(z, one)$par, c(1, -3, 2, 3))
})

test_that("radial polish map: inverse and chain rule", {
  from <- rq(".rum_radial_from"); to <- rq(".rum_radial_to"); grad <- rq(".rum_radial_grad")
  set.seed(3); L <- matrix(rnorm(8), 4, 2)
  expect_equal(from(to(L, 3), 3), L, tolerance = 1e-12)
  big <- L * 10; expect_true(all(sqrt(rowSums(from(to(big, 3), 3)^2)) < 3))
  W <- to(L, 3); gL <- matrix(rnorm(8), 4, 2); f <- function(w) sum(gL * from(matrix(w, 4, 2), 3))
  h <- 1e-6; fd <- vapply(1:8, function(e) { a <- b <- c(W); a[e] <- a[e] + h; b[e] <- b[e] - h
    (f(a) - f(b)) / (2 * h) }, 0)
  expect_equal(c(grad(W, gL, 3)), fd, tolerance = 1e-8)
})

test_that("q=2: trainer, start patterns, row bound, rotation and polish", {
  set.seed(9)
  P <- qf_problem(n = 40L, p = 2L, n_fac = 2L, seed = 9L)
  Z <- matrix(rnorm(40 * 2), 40, 2, dimnames = list(P$data$ids, c("z1", "z2")))
  ctl <- list(max_epochs = 6L, validation_every = 2L, patience = 3L, block_size = 16L,
              max_seconds = 600, start_seconds = 200, refit_seconds = 150, evaluation_reserve = 60)
  quad <- list(train = rum_quadrature(c(5L, 5L, 5L), n_fac = 2L, rule = "product"),
               check = list(rum_quadrature(c(7L, 7L, 7L), n_fac = 2L, rule = "product")))
  fit <- rum_dnn_gaussian_train(P$data, Z, P$data$ids[1:8], hidden = 3L, seeds = c(42L, 7L, 1609L),
                                control = ctl, n_fac = 2L, quadrature = quad)
  expect_true(fit$completed)
  pat <- function(i) .1 * switch(i, sin(1:2), cos(1:2), (-1)^(1:2))
  for (s in 1:3) {
    idx <- (s - 1L) %% 3L + 1L
    expect_identical(fit$starts[[s]]$initial_state$loading,
                     cbind(pat(idx), pat((idx + 2L - 2L) %% 3L + 1L)))
    expect_identical(fit$starts[[s]]$initial_state$omega, c(.25, .5, .75)[idx])
  }
  L <- fit$final$loading
  expect_identical(dim(L), c(2L, 2L))
  expect_true(all(sqrt(rowSums(L^2)) <= 3))
  expect_equal(L, rq(".rum_canonical_loading")(fit$refit$last_state$loading), tolerance = 1e-12)
  expect_equal(tcrossprod(L), tcrossprod(fit$refit$last_state$loading), tolerance = 1e-12)
  expect_identical(fit$numerical_checks$from, "5x5x5")
  expect_identical(fit$n_fac, 2L)
  # a loading row beyond the row bound is refused at the start
  bad <- fit$starts[[1]]$initial_state; bad$loading[1, ] <- c(2.5, 2.5)
  expect_error(rum_dnn_gaussian_train(P$data, Z, P$data$ids[1:8], hidden = 3L, seeds = 42L,
                                      control = ctl, n_fac = 2L, quadrature = quad, starts = list(bad)),
               "outside declared bounds")
  # a one-factor rule cannot train two factors
  expect_error(rum_dnn_gaussian_train(P$data, Z, P$data$ids[1:8], hidden = 3L, seeds = 42L, control = ctl,
    n_fac = 2L, quadrature = list(train = rum_quadrature(5L), check = list(rum_quadrature(7L)))),
    "taste factor")
  for (bias in c(FALSE, TRUE)) {
    pol <- rq(".rum_dnn_polish")(P$data, Z, fit$final, bias = bias, quadrature = quad$check[[1]])
    expect_gte(pol$loglik_after, pol$loglik_before - 1e-10)
    expect_true(pol$max_row_length < 3)
    expect_identical(dim(pol$final$loading), c(2L, 2L))
    cc <- crossprod(pol$final$loading); expect_lt(abs(cc[1, 2]), 1e-12)
    expect_lt(abs(pol$loglik_canonical - pol$loglik_after), 5e-3)   # 7x7x7 rule: quadrature error only
  }
})

test_that("q=1 paths are the original ones", {
  # the original one-factor product rule, built inline
  old_rule <- function(order) {
    if (length(order) == 1L) order <- rep(order, 2L)
    pu <- normal_rule(order[1L]); pv <- normal_rule(order[2L])
    structure(list(u = rep(pu$nodes, times = length(pv$nodes)), v = rep(pv$nodes, each = length(pu$nodes)),
      weight = as.numeric(outer(pu$weights, pv$weights)), order = as.integer(order),
      policy_law = "normal"), class = "rum_quadrature")
  }
  expect_identical(rum_quadrature(c(11L, 9L)), old_rule(c(11L, 9L)))
  expect_identical(rum_quadrature(), old_rule(c(11L, 11L)))
  expect_identical(rum_quadrature(7L, rule = "product"), old_rule(7L))
  expect_identical(rum_quadrature(7L, n_fac = 1L, rule = "auto"), old_rule(7L))
  P <- qf_problem(n_fac = 1L, seed = 21L); l <- drop(P$L)
  q1 <- rum_quadrature(c(11L, 9L))
  a <- ev_ll(P, L = l, quad = q1)
  # a p x 1 matrix loading and a G x 1 node matrix give the same bits
  b <- ev_ll(P, L = matrix(l, ncol = 1L), quad = q1)
  q1m <- q1; q1m$u <- matrix(q1$u, ncol = 1L)
  cc <- ev_ll(P, L = l, quad = q1m)
  for (nm in c("loglik", "output_score", "common_score")) {
    expect_identical(b[[nm]], a[[nm]]); expect_identical(cc[[nm]], a[[nm]])
  }
  expect_equal(unname(a$loglik), brute_loglik(P$data, P$mu, P$kappa, l, P$omega, c(11L, 9L)), tolerance = 1e-11)
  # the native core with an explicit n_fac = 1 is the default call
  d <- P$data; rows <- seq.int(d$offsets[1] + 1L, d$offsets[2])
  args <- list(d$y[rows], d$d[rows, , drop = FALSE], d$d[rows, , drop = FALSE], c(0L, length(rows)),
               c(P$kappa, P$mu[1, ], l, P$omega), q1$u, q1$v, q1$weight)
  sq <- rq("rum_sequence_cpp")
  expect_identical(do.call(sq, c(args, list(1L))), do.call(sq, args))
  # one-factor Sobol works and agrees loosely with the product rule
  s1 <- rum_quadrature(n_fac = 1L, rule = "sobol", nodes = 1024L)
  expect_lt(abs(ev_ll(P, L = l, quad = s1)$objective - a$objective), 2e-3)
  # canonical rotation is the identity at one factor (no sign flip)
  canon <- rq(".rum_canonical_loading")
  expect_identical(canon(-abs(l)), -abs(l))
  expect_identical(canon(matrix(-abs(l), ncol = 1L)), matrix(-abs(l), ncol = 1L))
  # the formula RUM stays one factor
  expect_error(rum_loglik(P$data, rep(0, 2 + P$data$r + P$data$p), rum_quadrature(n_fac = 2L)), "one taste factor")
  # wrong rule dimension is refused
  expect_error(ev_ll(P, L = cbind(l, l), quad = q1), "taste factor")
})
