# Fix round on the Codex review of the Stage 2 q-factor build (829393d, report/codex_review_s2.md,
# 2026-11-14). One block per finding; each block fails on 829393d and passes after its fix.
# Small simulated problems; single-threaded; nothing written outside tempdir().

rq <- function(nm) get(nm, envir = asNamespace("sconjoint"))

fx_problem <- function(n = 30L, p = 3L, n_fac = 2L, seed = 5L, lsd = .7, omega = .4) {
  set.seed(seed)
  ids <- sprintf("r%02d", seq_len(n))
  Tn <- sample(3:5, n, TRUE)
  d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), p * Tn[i], TRUE), Tn[i], p,
                                               dimnames = list(NULL, paste0("f", seq_len(p)))))
  mu <- matrix(rnorm(n * p, 0, .6), n, p, dimnames = list(ids, paste0("f", seq_len(p))))
  L <- matrix(rnorm(p * n_fac, 0, lsd), p, n_fac)
  y <- lapply(seq_len(n), function(i) {
    u <- rnorm(n_fac); eta <- .2 + d[[i]] %*% (mu[i, ] + L %*% u) + sqrt(omega) * rnorm(1)
    as.numeric(runif(Tn[i]) < plogis(drop(eta)))
  })
  list(data = rum_data(y, d, ids = ids, metadata = list(orientation = "y = 1 picks the first profile")),
       mu = mu, kappa = .2, L = L, omega = omega)
}

# ---- finding 3: the trainer's default starts at p = 1, q > 1 ---------------------------------
test_that("finding 3: default starts work with one feature and two factors", {
  set.seed(31)
  P <- fx_problem(n = 30L, p = 1L, n_fac = 2L, seed = 31L)
  Z <- matrix(rnorm(30 * 2), 30, 2, dimnames = list(P$data$ids, c("z1", "z2")))
  ctl <- list(max_epochs = 2L, validation_every = 1L, patience = 2L, block_size = 16L,
              max_seconds = 600, start_seconds = 200, refit_seconds = 150, evaluation_reserve = 60)
  quad <- list(train = rum_quadrature(c(5L, 5L, 5L), n_fac = 2L, rule = "product"),
               check = list(rum_quadrature(c(7L, 7L, 7L), n_fac = 2L, rule = "product")))
  fit <- rum_dnn_gaussian_train(P$data, Z, P$data$ids[1:6], hidden = 2L, seeds = c(42L, 7L),
                                control = ctl, n_fac = 2L, quadrature = quad)
  expect_true(fit$completed)
  pat <- function(i) .1 * switch(i, sin(1), cos(1), -1)
  for (s in 1:2) {
    idx <- (s - 1L) %% 3L + 1L
    expect_identical(fit$starts[[s]]$initial_state$loading,
                     matrix(c(pat(idx), pat((idx + 2L - 2L) %% 3L + 1L)), 1L, 2L))
  }
  expect_identical(dim(fit$final$loading), c(1L, 2L))
})

# ---- finding 8: .rum_cond_options() at one factor returns the f20956e object ---------------
test_that("finding 8: the conditional options keep the one-factor defaults (method exact, M 200)", {
  co <- rq(".rum_cond_options")
  # the f20956e constructor's output, written out
  old <- function(spec, resp = NULL, method = "exact", M = NULL, ny = 8L, louis_rb = TRUE,
                  seed = 20261011L, quad = rq(".rum_info_quadrature")(), cache_dir = NULL,
                  cache_net = NULL, cache_write = TRUE, cache_require = FALSE, crn = NULL,
                  null_zero = FALSE, draws = "common")
    list(spec = spec, resp = resp, method = method, crn = crn, null_zero = null_zero, draws = draws,
         M = if (is.null(M)) (if (method == "exact") 200L else 400L) else as.integer(M),
         ny = as.integer(ny), louis_rb = louis_rb, seed = as.integer(seed), quad = quad,
         cache_dir = cache_dir, cache_net = cache_net, cache_write = cache_write,
         cache_require = cache_require)
  expect_identical(co("br"), old("br"))
  expect_identical(co("br")$method, "exact"); expect_identical(co("br")$M, 200L)
  expect_identical(co("gs", resp = data.frame(pid7 = 1:3), M = 50L, draws = "own"),
                   old("gs", resp = data.frame(pid7 = 1:3), M = 50L, draws = "own"))
  expect_identical(co("sw_additive", method = "louis"), old("sw_additive", method = "louis"))
  expect_identical(co("br", method = "mc", ny = 4L, null_zero = TRUE), old("br", method = "mc", ny = 4L, null_zero = TRUE))
  expect_identical(co("br", n_fac = 1L), old("br"))
  # automatic selection only when asked for: method "auto", or n_fac > 1 with the default method
  expect_identical(co("br", n_fac = 2L)$method, "auto"); expect_true(is.na(co("br", n_fac = 2L)$M))
  expect_identical(co("br", method = "exact", n_fac = 2L)$M, 200L)
  expect_identical(co("br", method = "auto")$method, "auto")
  expect_null(co("br", n_fac = 3L)$n_fac)
})

# ---- finding 5: tied principal axes are fixed by a feature-coordinate convention ------------
test_that("finding 5: tied and near-tied principal axes do not depend on the rotation of Lambda", {
  canon <- rq(".rum_canonical_loading"); axes <- function(L) rq(".rum_canonical_axes")(L)
  Q <- rbind(c(.6, -.8), c(.8, .6))
  L <- 2 * diag(2); LQ <- L %*% Q
  expect_equal(canon(LQ), canon(L), tolerance = 1e-12)
  expect_equal(tcrossprod(canon(LQ)), tcrossprod(L), tolerance = 1e-12)
  expect_true(axes(L)$near_tie); expect_true(axes(LQ)$near_tie)   # (errors on 829393d: no such helper)
  # Codex's example: an order-2 product rule integrates the two loadings to the same probability
  P <- fx_problem(n = 12L, p = 2L, n_fac = 2L, seed = 51L)
  q2 <- rum_quadrature(c(2L, 2L, 2L), n_fac = 2L, rule = "product")
  e1 <- rum_dnn_gaussian_evaluate(P$data, P$mu, .7, canon(L), .4, q2)
  e2 <- rum_dnn_gaussian_evaluate(P$data, P$mu, .7, canon(LQ), .4, q2)
  expect_equal(e2$loglik, e1$loglik, tolerance = 1e-12)
  # without the convention the raw loadings give different numbers on this rule (the finding)
  r1 <- rum_dnn_gaussian_evaluate(P$data, P$mu, .7, L, .4, q2)$loglik
  r2 <- rum_dnn_gaussian_evaluate(P$data, P$mu, .7, LQ, .4, q2)$loglik
  expect_gt(max(abs(r1 - r2)), 1e-4)
  # a near tie (relative eigenvalue gap 1e-9) on a general p x 2 loading, rotated by a random Q
  set.seed(52)
  A <- qr.Q(qr(matrix(rnorm(9), 3, 3)))[, 1:2]                      # orthonormal 3 x 2
  Ln <- A %*% diag(c(1.5, 1.5 * sqrt(1 - 1e-9)))
  for (r in 1:5) {
    th <- runif(1, 0, 2 * pi); Rt <- rbind(c(cos(th), -sin(th)), c(sin(th), cos(th)))
    expect_equal(canon(Ln %*% Rt), canon(Ln), tolerance = 1e-10)
  }
  expect_true(axes(Ln)$near_tie); expect_lt(axes(Ln)$min_gap_rel, 1e-6)
  # convention: lower trapezoidal with a positive diagonal inside the tied cluster
  C <- canon(Ln); expect_lt(abs(C[1, 2]), 1e-12); expect_gt(C[1, 1], 0); expect_gt(C[2, 2], 0)
  # three factors with one tied pair: the separated axis keeps its eigenvector, the pair is fixed
  B <- qr.Q(qr(matrix(rnorm(16), 4, 4)))[, 1:3]
  L3 <- B %*% diag(c(2, 1, 1))
  for (r in 1:3) {
    R3 <- qr.Q(qr(matrix(rnorm(9), 3, 3)))
    expect_equal(canon(L3 %*% R3), canon(L3), tolerance = 1e-10)
  }
  a3 <- axes(L3); expect_identical(a3$clusters, list(2:3))
  # well separated axes: unchanged rule (no cluster), same as plain principal axes with signs
  Ls <- matrix(c(1, .2, -.3, .1, .8, .4), 3, 2)
  e <- eigen(crossprod(Ls), symmetric = TRUE); V <- e$vectors
  LV <- Ls %*% V; for (k in 1:2) if (LV[which.max(abs(LV[, k])), k] < 0) V[, k] <- -V[, k]
  expect_identical(canon(Ls), Ls %*% V); expect_false(axes(Ls)$near_tie)
  # the fold diagnostics record the near tie
  fx <- readRDS(test_path("fixtures", "rum-inference-fixture.rds"))
  fin <- fx$fins[[1]]; p <- fx$data$p
  fin$loading <- cbind(rep(.5, p) * c(1, 0, rep(0, p - 2)), c(0, .5, rep(0, p - 2)))
  ff <- rq(".rum_fold_fit")(fx$data, fx$Z, fin, rq(".rum_score_quadrature")())
  expect_true(ff$axes$near_tie); expect_equal(ff$axes$min_gap_rel, 0, tolerance = 1e-12)
})

# ---- finding 4: rank-deficient reduced information after the rotations are removed ----------
test_that("finding 4: the correction solves on the estimable range and flags incompatible targets", {
  p <- 3L; q <- 2L; n_th <- p * q + 2L; D <- p + n_th
  Lam <- cbind(c(1, 0, 0), c(0, 0, 0))
  rot <- rq(".rum_rotation_dirs")(Lam)
  n <- 6L; I <- array(0, c(n, D, D))
  for (i in seq_len(n)) I[i, , ] <- diag(c(rep(1, 7), rep(0, 3), 1)) * (1 + i / 10)
  W <- rq(".rum_cond_weight_parts")(I, p, n_th, 1e-6, rot)
  expect_identical(dim(W$Ic_red), c(7L, 7L))
  expect_identical(qr(W$Ic_red)$rank, 5L)                          # Codex's reproducer: rank 5 of 7
  set.seed(41); v <- matrix(rnorm(n * p), n, p)
  ix <- rq(".rum_th_idx")(p, q)
  # a target whose theta derivative stays off the second loading column: compatible
  gt <- numeric(n_th); gt[ix$kappa] <- .3; gt[ix$loading[1:3]] <- c(.2, -.1, .4); gt[ix$omega] <- .1
  cw <- rq(".rum_correction")(W, v, gt, seq_len(n))
  expect_true(all(is.finite(cw$g))); expect_true(all(is.finite(cw$a)))
  expect_identical(cw$status, "rank_deficient_compatible"); expect_identical(cw$null_dim, 2L)
  # it solves J g = r on the estimable range: the residual is zero there
  r <- gt - colMeans(rq(".rum_info_CLv")(W$Li, W$Lt, v))
  expect_lt(max(abs(drop(W$Ic %*% cw$g) - r)), 1e-12)
  expect_lt(max(abs(cw$g[ix$loading[5:6]])), 1e-14)                # no weight on the null directions
  # a target that loads on the uninformative second column: no solution, NA with a reason
  gt2 <- gt; gt2[ix$loading[5]] <- .5
  cw2 <- rq(".rum_correction")(W, v, gt2, seq_len(n))
  expect_identical(cw2$status, "not_identified"); expect_true(all(is.na(cw2$g))); expect_true(all(is.na(cw2$a)))
  expect_gt(cw2$resid_rel, 1e-6)
  # the shared (constant-basis) path
  Ws <- rq(".rum_weight_parts")(function(i) diag(c(rep(1, 7), rep(0, 3), 1)), 1L, p, n_th, 0, rot)
  cws <- rq(".rum_correction")(Ws, v, gt, seq_len(n))
  expect_true(all(is.finite(cws$g)))
  # a full-rank reduced system: solve() as before, status ok
  I2 <- I; for (i in seq_len(n)) I2[i, , ] <- diag(D) * (1 + i / 10)
  W2 <- rq(".rum_cond_weight_parts")(I2, p, n_th, 1e-6, rq(".rum_rotation_dirs")(cbind(c(1, 0, 0), c(0, 1, 0))))
  cw3 <- rq(".rum_correction")(W2, v, gt2, seq_len(n))
  expect_identical(cw3$status, "ok")
  expect_identical(cw3$g, drop(W2$B %*% solve(W2$Ic_red, crossprod(W2$B, gt2 - colMeans(rq(".rum_info_CLv")(W2$Li, W2$Lt, v))))))
  # the bounded updates recognise unsolved directions
  expect_true(rq(".rum_dirs_unsolved")(list(f1 = list(g = cw$g), f2 = list(g = cw2$g))))
  expect_false(rq(".rum_dirs_unsolved")(list(f1 = list(g = cw$g))))
})

# ---- finding 1: small omega; one omega score in R, C++ and the conditional information ------
# Helpers: all 2^T sequences of one respondent with design Dm, scored three ways.
fx_scores3 <- function(mu1, Dm, th, quad) {
  T <- nrow(Dm); p <- ncol(Dm); nf <- ncol(th$loading)
  Yall <- rq(".rum_cond_Y")(T); B <- nrow(Yall)
  sR <- rq(".rum_seq_scores")(matrix(mu1, B, p, byrow = TRUE),
                               lapply(seq_len(T), function(t) matrix(Dm[t, ], B, p, byrow = TRUE)), Yall, th, quad)
  ids <- sprintf("y%02d", seq_len(B))
  dat <- rum_data(lapply(seq_len(B), function(r) Yall[r, ]), rep(list(Dm), B), ids = ids,
                  metadata = list(orientation = "y = 1 picks the first profile"))
  ev <- rum_dnn_gaussian_evaluate(dat, matrix(mu1, B, p, byrow = TRUE, dimnames = list(ids, NULL)),
                                  th$kappa, th$loading, th$omega, quad)
  sC <- cbind(ev$output_score, ev$common_score)
  bnd <- nf > 1L && th$omega < rq(".rum_omega_small")
  s <- sqrt(th$omega); a <- th$kappa + drop(Dm %*% mu1); b <- Dm %*% th$loading
  e <- rq(".rum_cond_eta")(a, b, s, quad); po <- rq(".rum_cond_post")(Yall, e, log(quad$weight))
  cc <- rq(".rum_cond_c")(Yall, po$Pi, e, quad, bnd)
  A <- rq(".rum_cond_A")(Dm, s, nf, bnd)
  K <- rq(".rum_cond_K")(a, b, s, quad, "exact", Yall, NULL, TRUE, bnd)$K
  list(R = unname(sR), C = unname(sC), Ac = t(A %*% t(cc)), info = A %*% K %*% t(A),
       info_cpp = crossprod(unname(sC) * exp(ev$loglik), unname(sC)), py = exp(ev$loglik), bnd = bnd,
       e = e, Yall = Yall, a = a, b = b, s = s, A = A)
}
rel <- function(x, y) max(abs(x - y)) / max(abs(y))

test_that("finding 1: the q > 1 Sobol rule is exactly symmetric in v (512 points -> 1,024 nodes)", {
  q2 <- rum_quadrature(n_fac = 2L)
  expect_identical(q2$points, 512L); expect_identical(q2$nodes, 1024L); expect_identical(length(q2$v), 1024L)
  odd <- seq(1L, 1024L, 2L)
  expect_identical(q2$u[odd, ], q2$u[odd + 1L, ]); expect_identical(q2$v[odd], -q2$v[odd + 1L])
  expect_identical(rq(".rum_quad_label")(q2), "sobol512v")
  expect_identical(rq(".rum_info_quadrature")(2L)$nodes, 1024L)
  expect_identical(rq(".rum_score_quadrature")(2L)$points, 2048L)
  big <- rum_quadrature(n_fac = 2L, nodes = 2048L)                 # nested: first nodes are the smaller rule
  expect_identical(big$u[1:1024, ], q2$u); expect_identical(big$v[1:1024], q2$v)
  uv <- rum_quadrature(n_fac = 2L, nodes = 256L, symmetric = "uv")
  expect_identical(uv$nodes, 1024L)
  expect_identical(uv$u[seq(1L, 1024L, 4L), ], -uv$u[seq(3L, 1024L, 4L), ])
  expect_equal(colSums(uv$u * uv$weight), c(0, 0), tolerance = 1e-15)
  expect_identical(rum_quadrature(n_fac = 2L, symmetric = "none")$nodes, 512L)
  # the threshold has one definition (C++), read by R at load
  expect_identical(rq(".rum_omega_small"), rq("rum_omega_small_cpp")())
  expect_identical(rq(".rum_omega_small"), 1e-8)
})

test_that("finding 1: R scores, C++ scores and A_D c agree, and the information is the C++ score outer product", {
  set.seed(11)
  p <- 3L; q <- 2L; T <- 4L
  Lam <- matrix(c(.6, -.3, .4, .2, .5, -.4), p, q); mu1 <- c(.3, -.5, .2)
  Dm <- matrix(c(1, -1, 0, 1, 0, 1, -1, 1, -1, 0, 1, 1), T, p)
  quad <- rq(".rum_info_quadrature")(q)
  for (om in c(1e-12, 1e-6, 1e-4, 0.01, 0.25)) {
    th <- list(kappa = .7, loading = Lam, omega = om)
    S <- fx_scores3(mu1, Dm, th, quad)
    expect_identical(S$bnd, om < 1e-8)
    expect_lt(rel(S$R, S$C), 1e-10, label = sprintf("R vs C++ at omega %g", om))
    expect_lt(rel(S$Ac, S$C), 1e-10, label = sprintf("A_D c vs C++ at omega %g", om))
    expect_lt(rel(S$info, S$info_cpp), 1e-10, label = sprintf("A K A' vs sum_y p s s' at omega %g", om))
    expect_lt(abs(sum(S$py) - 1), 1e-12)
    expect_lt(max(abs(colSums(S$C * S$py))), 1e-12)                 # E_y[s] = 0 on the rule
  }
  # the boundary form is continuous with the interior form across the threshold (the v-symmetric rule)
  sc <- function(om) fx_scores3(mu1, Dm, list(kappa = .7, loading = Lam, omega = om), quad)$C[, 11]
  expect_lt(max(abs(sc(0.99e-8) - sc(1.01e-8))) / max(abs(sc(1e-2))), 1e-6)
  expect_lt(max(abs(sc(0) - sc(1e-12))) / max(abs(sc(1e-2))), 1e-9)
})

test_that("finding 1: the boundary block's H3 and the Louis variant match brute force and exact", {
  set.seed(12)
  p <- 3L; q <- 2L; T <- 3L
  Lam <- matrix(c(.6, -.3, .4, .2, .5, -.4), p, q); mu1 <- c(.3, -.5, .2)
  Dm <- matrix(c(1, -1, 0, 1, 0, 1, -1, 1, -1), T, p)
  quad <- rum_quadrature(n_fac = q, nodes = 64L)
  th <- list(kappa = .7, loading = Lam, omega = 1e-12)
  S <- fx_scores3(mu1, Dm, th, quad); expect_true(S$bnd)
  # H3 = sum_g w_g E_{y | g}[c_g c_g'] with the boundary block h_t = e_t R - pi_t, by enumeration
  Z <- cbind(1, quad$u); H <- matrix(0, 4L * T, 4L * T)
  for (g in seq_along(quad$weight)) {
    pg <- S$e$P[, g]; pyg <- apply(S$Yall, 1, function(y) prod(ifelse(y == 1, pg, 1 - pg)))
    for (r in seq_len(nrow(S$Yall))) {
      er <- S$Yall[r, ] - pg
      cg <- c(er * Z[g, 1], er * Z[g, 2], er * Z[g, 3], quad$v[g]^2 * (er * sum(er) - pg * (1 - pg)))
      H <- H + quad$weight[g] * pyg[r] * tcrossprod(cg)
    }
  }
  expect_lt(rel(rq(".rum_cond_H3")(S$e, quad, TRUE), H), 1e-12)
  # Louis (both forms) and mc against exact at the boundary, simulated sequences
  ny <- 4000L
  sim <- withr::with_seed(5L, list(g = sample.int(length(quad$weight), ny, TRUE, prob = quad$weight),
                                   U = matrix(runif(ny * T), ny, T)))
  KE <- rq(".rum_cond_K")(S$a, S$b, S$s, quad, "exact", S$Yall, NULL, TRUE, TRUE)$K
  IE <- S$A %*% KE %*% t(S$A)
  for (m in c("louis", "mc")) {
    KL <- rq(".rum_cond_K")(S$a, S$b, S$s, quad, m, NULL, sim, TRUE, TRUE)$K
    expect_lt(rel(S$A %*% KL %*% t(S$A), IE), if (m == "louis") 0.05 else 0.08, label = m)
  }
  KN <- rq(".rum_cond_K")(S$a, S$b, S$s, quad, "louis", NULL, sim, FALSE, TRUE)
  expect_identical(KN$xo, 0)
})

test_that("finding 1: the model information is the mean outer product of the C++ scores on its draws", {
  P <- fx_problem(n = 4L, p = 3L, n_fac = 2L, seed = 13L)
  dat <- P$data; p <- dat$p; M <- 40L; seed <- 77L
  for (om in c(1e-12, 0.25)) {
    th <- list(kappa = .2, loading = P$L, omega = om); quad <- rq(".rum_info_quadrature")(2L)
    MI <- rq(".rum_model_info")(dat, P$mu, th, M, quad, seed)
    dr <- rq(".rum_with_seed")(seed, list(j = sample.int(dat$n, M, replace = TRUE),
      u = matrix(stats::rnorm(M * 2L), M, 2L), v = stats::rnorm(M), U = matrix(stats::runif(M * max(dat$T)), M, max(dat$T))))
    for (i in seq_len(dat$n)) {
      ys <- ds <- list()
      for (m in seq_len(M)) {
        j <- dr$j[m]; rows <- dat$offsets[j] + seq_len(dat$T[j]); Dm <- dat$d[rows, , drop = FALSE]
        beta <- P$mu[i, ] + drop(th$loading %*% dr$u[m, ])
        ys[[m]] <- as.numeric(dr$U[m, seq_len(dat$T[j])] < plogis(th$kappa + drop(Dm %*% beta) + sqrt(om) * dr$v[m]))
        ds[[m]] <- Dm
      }
      ids <- sprintf("m%03d", seq_len(M))
      sd <- rum_data(ys, ds, ids = ids, metadata = list(orientation = "y = 1 picks the first profile"))
      ev <- rum_dnn_gaussian_evaluate(sd, matrix(P$mu[i, ], M, p, byrow = TRUE, dimnames = list(ids, NULL)),
                                      th$kappa, th$loading, om, quad)
      SC <- cbind(ev$output_score, ev$common_score)
      expect_lt(rel(MI$I[i, , ], crossprod(SC) / M), 1e-10, label = sprintf("respondent %d omega %g", i, om))
    }
  }
})

# ---- finding 2: importance shares near zero mean tastes; convergence-controlled integration -
test_that("finding 2: importance shares reach the closed form at mu = 0 and report their accuracy", {
  # Codex's case: mu = 0, Lambda = diag(1, .1), two groups {1}, {2}, uncentred:
  # share_1 = E[U1^2 / (U1^2 + .01 U2^2)] = 1 / (1 + .1)
  set1 <- rq(".rum_tgt_importance_set")(list(1L, 2L), centred = FALSE)
  mu <- matrix(0, 2L, 2L); th <- list(kappa = 0, loading = diag(c(1, .1)), omega = .3)
  g1 <- rq(".rum_target_grad")(set1[[1]], mu, th)
  expect_lt(max(abs(g1$m - 1 / 1.1)), 1e-3)
  se <- attr(set1[[1]]$grad(mu, th), "se")
  expect_true(all(is.finite(se))); expect_true(all(se <= 1e-4))
  # rotation of Lambda and swapping the factors change nothing
  th2 <- th; th2$loading <- diag(c(1, .1)) %*% rbind(c(.6, -.8), c(.8, .6))
  expect_equal(set1[[1]]$m(mu, th2), g1$m, tolerance = 1e-10)
  # a cap that is too small is reported, not hidden
  tight <- rq(".rum_imp_control")(tol = 1e-9, n_start = 64L, n_max = 128L)
  expect_warning(rq(".rum_imp_sums")(mu, th$loading, list(1L, 2L), FALSE, control = tight),
                 "did not reach the standard error")
})

test_that("finding 2: importance shares and gradients match brute-force Monte Carlo (1e7 draws)", {
  set.seed(21)
  for (cs in 1:3) {
    p <- 4L; q <- 2L; n <- 3L
    mu <- matrix(rnorm(n * p, 0, c(.05, .3, 1)[cs]), n, p)            # near zero to moderate
    Lam <- matrix(rnorm(p * q, 0, .5), p, q); GR <- list(1:2, 3L, 4L)
    S <- rq(".rum_imp_sums")(mu, Lam, GR, centred = TRUE)
    expect_true(all(S$se <= 1e-4))
    Nd <- 1e7; ch <- 1e6
    acc <- matrix(0, n, 3L); acc2 <- matrix(0, n, 3L); gacc <- array(0, c(n, p, q))
    for (b in seq_len(Nd / ch)) {
      U <- matrix(rnorm(ch * q), ch, q); LU <- U %*% t(Lam)
      for (i in seq_len(n)) {
        B <- sweep(LU, 2, mu[i, ], `+`); S1 <- B[, 1] + B[, 2]
        V1 <- (B[, 1]^2 + B[, 2]^2) / 3 - (S1 / 3)^2; V2 <- B[, 3]^2 / 4; V3 <- B[, 4]^2 / 4
        Tt <- V1 + V2 + V3; sh <- cbind(V1, V2, V3) / Tt
        acc[i, ] <- acc[i, ] + colSums(sh); acc2[i, ] <- acc2[i, ] + colSums(sh^2)
        if (b <= 2L) {                                               # gradient of share 1: 2e6 draws
          dV <- cbind(2 * B[, 1] / 3 - 2 * S1 / 9, 2 * B[, 2] / 3 - 2 * S1 / 9, B[, 3] / 2, B[, 4] / 2)
          gr <- -sh[, 1] * dV; gr[, 1:2] <- gr[, 1:2] + dV[, 1:2]; gr <- gr / Tt
          for (k in seq_len(q)) gacc[i, , k] <- gacc[i, , k] + colSums(gr * U[, k])
        }
      }
    }
    mc <- acc / Nd; mcse <- sqrt(pmax(acc2 / Nd - mc^2, 0) / Nd)
    expect_true(all(abs(S$m - mc) < 5 * mcse + 3e-4), label = sprintf("case %d values", cs))
    expect_lt(max(abs(rowSums(S$m) - 1)), 1e-12)
    G1 <- matrix(gacc, n) / 2e6                                      # n x (p q), column-major in Lambda
    gpath <- rq(".rum_rotation_project")(cbind(0, G1, 0), Lam)[, 1L + seq_len(p * q)]
    # (near b = 0 the pathwise integrand has a heavy tail, so the Monte Carlo check is for cases 2-3)
    if (cs >= 2L) expect_lt(max(abs(matrix(S$gl[, , 1], n) - gpath)), 0.02, label = sprintf("case %d gradient", cs))
  }
})

# ---- finding 6: importance dimensions (q > p; one respondent) --------------------------------
test_that("finding 6: importance works with q > p and with a single respondent", {
  set.seed(61)
  mu <- matrix(c(.8, -.4), 1L, 2L); Lam <- matrix(c(.5, .2, -.3, .4, .1, .6), 2L, 3L)   # p = 2, q = 3
  st <- rq(".rum_tgt_importance_set")(list(1L, 2L), centred = FALSE)
  th <- list(kappa = 0, loading = Lam, omega = .2)
  g <- rq(".rum_target_grad")(st[[1]], mu, th)                    # n = 1 and q > p together
  expect_identical(dim(g$gt), c(1L, 2L * 3L + 2L)); expect_identical(dim(g$gm), c(1L, 2L))
  # the same law with a 2-column loading (same Lambda Lambda'): same value
  L2 <- t(chol(tcrossprod(Lam)))
  expect_equal(st[[1]]$m(mu, list(kappa = 0, loading = L2, omega = .2)), g$m, tolerance = 5e-4)
  # Monte Carlo
  U <- withr::with_seed(62, matrix(rnorm(2e6 * 3), 2e6, 3)); B <- sweep(U %*% t(Lam), 2, mu[1, ], `+`)
  expect_lt(abs(g$m - mean(B[, 1]^2 / rowSums(B^2))), 1e-3)
  # pathwise gradient E[(d share / d b) u'] (mapped to Lambda's coordinates, rotations removed). With
  # Lambda of full rank p the plane mu + span(Lambda) is all of R^p and contains b = 0, where the
  # gradient integrand is not square integrable; the gradient is checked where it is: q = 3 > p = 2
  # with Lambda of rank 1 (reduced-rank integration over one axis).
  Lr <- outer(c(.5, .2), c(1, -.5, .3)); thr <- list(kappa = 0, loading = Lr, omega = .2)
  gr1 <- rq(".rum_target_grad")(st[[1]], mu, thr)
  expect_identical(dim(gr1$gt), c(1L, 8L))
  B <- sweep(U %*% t(Lr), 2, mu[1, ], `+`)
  sh <- B[, 1]^2 / rowSums(B^2); Tt <- rowSums(B^2)
  dsh <- cbind(2 * B[, 1] * (1 - sh), -2 * B[, 2] * sh) / Tt
  expect_lt(abs(gr1$m - mean(sh)), 1e-3)
  G <- vapply(1:3, function(k) colMeans(dsh * U[, k]), numeric(2))
  Gse <- vapply(1:3, function(k) apply(dsh * U[, k], 2, stats::sd) / sqrt(nrow(U)), numeric(2))
  gp <- rq(".rum_rotation_project")(cbind(0, matrix(as.vector(G), 1L), 0), Lr)[, 2:7]
  expect_true(all(abs(gr1$gt[, 2:7] - gp) < 5 * max(Gse) + 1e-4))
  expect_true(all(abs(gr1$gm - colMeans(dsh)) < 5 * apply(dsh, 2, stats::sd) / sqrt(nrow(U)) + 1e-4))
  # one respondent with p = q = 2 (Codex's second trigger) on the product rule and on the default
  mu2 <- matrix(c(.5, -.2), 1L); L22 <- matrix(c(.4, .1, -.2, .5), 2L)
  for (tg in list(rq(".rum_tgt_importance")("i", 1L, list(1L, 2L), order_multi = 20L),
                  rq(".rum_tgt_importance")("i", 1L, list(1L, 2L)))) {
    gg <- rq(".rum_target_grad")(tg, mu2, list(kappa = 0, loading = L22, omega = .2))
    expect_identical(dim(gg$gt), c(1L, 6L)); expect_true(all(is.finite(gg$gt)))
  }
  # the product rule on request also integrates q > p (reduced rank: 40^2 nodes, not 40^3); here
  # b = 0 lies in the plane, so it is less accurate than the default (the point of finding 2)
  tp <- rq(".rum_tgt_importance")("i", 1L, list(1L, 2L), order_multi = 40L)
  expect_lt(abs(tp$m(mu, th) - g$m), 1e-2)
})

# ---- finding 7: no unguarded exponential product grid ----------------------------------------
test_that("finding 7: importance at p = q = 10 uses the default integration; a huge product rule is refused before allocation", {
  set.seed(71)
  p <- 10L; mu <- matrix(rnorm(2 * p, 0, 1.5), 2L, p); Lam <- matrix(rnorm(p * p, 0, .2), p, p)
  GR <- as.list(seq_len(p))
  t0 <- proc.time()[["elapsed"]]
  S <- rq(".rum_imp_sums")(mu, Lam, GR, centred = FALSE)
  expect_lt(proc.time()[["elapsed"]] - t0, 120)
  expect_lt(max(abs(rowSums(S$m) - 1)), 1e-12); expect_true(all(S$se <= 1e-4))
  expect_error(rq(".rum_imp_sums")(mu, Lam, GR, centred = FALSE, order_multi = 12L), "more than")
  # the quadrature constructor keeps its own guard
  expect_error(rum_quadrature(12L, n_fac = 10L, rule = "product"), "more than 5e6")
})
