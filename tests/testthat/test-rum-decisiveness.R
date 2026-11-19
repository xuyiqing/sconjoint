# Stage D1 of the decisiveness build (run plan 2026-11-19): the model-implied decisiveness target
# .rum_tgt_decisiveness_set() (named swaps, APNS; best-worst, MAPNS). For respondent i, task t and a
# taste draw b = mu_i + Lambda u, the flip probability is G(c_t + S) - G(c_t - S), c_t the utility
# difference of the other attributes as recorded, S the swing (|a_j - a_k| for a named swap, the
# largest allowed a_j - a_k for best-worst, found within the draw); m_i averages it over the
# respondent's tasks and over u. Checked here on small simulated designs: q = 1 against an
# independent fine-grid computation written as Stage D0 (trapezoid on [-10, 10]); exact gradients
# against central differences; rotation invariance; a brute-force Monte Carlo of the flips (taste
# draws, logistic shocks, both assignments); the q = 3 named swap against q-dimensional randomised
# QMC; the allowed-pairs and fixed-revenue conventions; the output shape, the cache and the one-step.
# Real-data checks (the October one-factor states, the q-factor fold states) are scripts in the run
# folder, not tests. Single-threaded; nothing is written to disk.

rinf <- function(x) get(x, envir = asNamespace("sconjoint"))
dec_relmax <- function(x, y) max(abs(x - y) / pmax(abs(y), 1e-12))
# A design with three attribute groups coded from drawn levels (profile 1 minus profile 2):
#   rate: a BR-like quadratic tax map (4 levels, 2 columns); dummy: baseline + 3 dummies (GS-like,
#   levels 1-2 generic); rev: a BR-like Helmert revenue menu (4 levels, 3 columns). p = 8.
dec_menus <- function() {
  r <- c(.1, .2, .3, .45); rc <- r - mean(r)
  list(rate = cbind(rc, rc^2 - mean(rc^2)), dummy = rbind(0, diag(3)),
       rev = rbind(c(-1, -1, -1), c(1, -1, -1), c(0, 2, -1), c(0, 0, 3)) / 2)
}
dec_groups <- list(rate = 1:2, dummy = 3:5, rev = 6:8)
dec_setup <- function(q = 1L, n = 8L, seed = 1L, Tmax = 4L, mu_sd = .7, lam_sd = .5) {
  set.seed(seed); mn <- dec_menus(); p <- 8L
  Tn <- sample(2:Tmax, n, TRUE); N <- sum(Tn)
  X <- matrix(0, N, p)
  for (g in names(dec_groups)) {
    L <- nrow(mn[[g]]); l1 <- sample(L, N, TRUE); l2 <- sample(L, N, TRUE)
    X[, dec_groups[[g]]] <- mn[[g]][l1, , drop = FALSE] - mn[[g]][l2, , drop = FALSE]
  }
  data <- list(d = X, offsets = c(0L, cumsum(Tn)), T = Tn, n = n, p = p, ids = sprintf("r%02d", seq_len(n)))
  mu <- matrix(stats::rnorm(n * p, sd = mu_sd), n, p)
  Lam <- matrix(stats::rnorm(p * q, sd = lam_sd), p, q)
  list(data = data, mu = mu, Lam = Lam, q = q, n = n, p = p, menus = mn,
       th = list(kappa = .15, loading = if (q == 1L) drop(Lam) else Lam, omega = .3))
}
# allowed rule of the dummy group (Jens's GS governance rule): levels 1 and 2 generic
dec_allowed <- list(rate = NULL, dummy = c(TRUE, TRUE, FALSE, FALSE), rev = NULL)
dec_set <- function(X, ..., pairs = list(dummy = c(3, 4), rate = c(1, 4)), allowed = dec_allowed)
  rinf(".rum_tgt_decisiveness_set")(X$data, dec_groups, menus = X$menus, pairs = pairs, allowed = allowed, ...)
# the targets in order: rate, rate 1 vs 4, dummy, dummy 3 vs 4, rev
dec_ordered_pairs <- function(L, allowed = NULL) {
  P <- as.matrix(expand.grid(hi = seq_len(L), lo = seq_len(L))); P <- P[P[, 1] != P[, 2], , drop = FALSE]
  if (!is.null(allowed)) P <- P[allowed[P[, 1]] | allowed[P[, 2]], , drop = FALSE]
  P
}
dec_spec_pairs <- function(mn) list(rate = dec_ordered_pairs(4L), rate14 = rbind(c(1, 4), c(4, 1)),
  dummy = dec_ordered_pairs(4L, dec_allowed$dummy), dummy34 = rbind(c(3, 4), c(4, 3)), rev = dec_ordered_pairs(4L))
dec_spec_group <- c(rate = "rate", rate14 = "rate", dummy = "dummy", dummy34 = "dummy", rev = "rev")

# Independent computation (written as Stage D0): per respondent, the swing over an explicit list of
# ordered level pairs on a node grid in u (one factor), or over given taste draws U (any q).
dec_independent <- function(X, cols, menu, P, U, w, shift = 0) {
  d <- X$data; own <- rep(seq_len(d$n), d$T); Lam <- matrix(X$Lam, X$p)
  vapply(seq_len(d$n), function(i) {
    rows <- which(own == i); Xt <- d$d[rows, , drop = FALSE]; Xt[, cols] <- 0
    B <- sweep(U %*% t(Lam), 2, X$mu[i, ], `+`)
    ctx <- B %*% t(Xt) + shift; lv <- B[, cols, drop = FALSE] %*% t(menu)
    dd <- lv[, P[, 1], drop = FALSE] - lv[, P[, 2], drop = FALSE]
    S <- do.call(pmax, unname(as.data.frame(dd)))
    sum(w * rowMeans(stats::plogis(ctx + S) - stats::plogis(ctx - S)))
  }, 0)
}

test_that("q = 1: values match an independent fine-grid computation (D0's method), all target types", {
  X <- dec_setup(1L); TG <- dec_set(X)
  expect_identical(vapply(TG, `[[`, "", "name"), c("decisiveness: rate", "decisiveness: rate 1 vs 4",
    "decisiveness: dummy", "decisiveness: dummy 3 vs 4", "decisiveness: rev"))
  xg <- seq(-10, 10, length.out = 16001L); wg <- stats::dnorm(xg) / sum(stats::dnorm(xg))
  SP <- dec_spec_pairs(X$menus)
  for (k in seq_along(SP)) {
    g <- dec_spec_group[k]
    ind <- dec_independent(X, dec_groups[[g]], X$menus[[g]], SP[[k]], matrix(xg), wg)
    expect_lt(max(abs(TG[[k]]$m(X$mu, X$th) - ind)), 1e-6)       # the run plan's tolerance is 1e-5
  }
  # the composite rule has converged: halving the panels and adding nodes moves values by < 1e-10
  fine <- dec_set(X, control = rinf(".rum_dec_control")(width = .5, n_gl = 24L))
  for (k in seq_along(TG)) expect_lt(max(abs(fine[[k]]$m(X$mu, X$th) - TG[[k]]$m(X$mu, X$th))), 1e-10)
  # a p x 1 loading is the one-factor model: the same values
  thm <- X$th; thm$loading <- matrix(X$th$loading, X$p, 1L)
  for (k in seq_along(TG)) expect_lt(max(abs(TG[[k]]$m(X$mu, thm) - TG[[k]]$m(X$mu, X$th))), 1e-15)
})

test_that("exact gradients match central differences (q = 1; q = 3 named swap; side = fitted)", {
  for (cfg in list(list(q = 1L, seed = 2L), list(q = 3L, seed = 3L))) {
    X <- dec_setup(cfg$q, seed = cfg$seed); TG <- dec_set(X)
    ks <- if (cfg$q == 1L) seq_along(TG) else c(2L, 4L)            # q = 3: the named swaps (2-D rule)
    for (k in ks) {
      g <- rinf(".rum_target_grad")(TG[[k]], X$mu, X$th)
      tn <- TG[[k]]; tn$grad <- NULL; gn <- rinf(".rum_target_grad")(tn, X$mu, X$th, h = 1e-5)
      expect_equal(dim(g$gt), c(X$n, X$p * cfg$q + 2L))
      expect_lt(max(abs(g$gm - gn$gm)), 1e-7)
      expect_lt(max(abs(g$gt - gn$gt)), 1e-7)
      expect_true(all(g$gt[, c(1L, X$p * cfg$q + 2L)] == 0))     # side = "zero": no kappa, omega
    }
  }
  # side = "fitted": kappa and omega enter; their derivatives too (and omega below .rum_omega_small)
  for (q in c(1L, 2L)) {
    X <- dec_setup(q, seed = 4L, n = 5L)
    TG <- dec_set(X, side = "fitted", control = rinf(".rum_dec_control")(n_v = if (q == 1L) 21L else 5L))
    for (om in if (q == 1L) c(.3, 1e-10) else .3) {
      th <- X$th; th$omega <- om
      for (k in if (q == 1L) c(1L, 2L) else 2L) {
        g <- rinf(".rum_target_grad")(TG[[k]], X$mu, th)
        tn <- TG[[k]]; tn$grad <- NULL; gn <- rinf(".rum_target_grad")(tn, X$mu, th, h = 1e-6)
        expect_lt(max(abs(g$gm - gn$gm)), 1e-6)
        expect_lt(max(abs(g$gt[, -ncol(g$gt)] - gn$gt[, -ncol(g$gt)])), 1e-6)
        expect_lt(max(abs(g$gt[, ncol(g$gt)] - gn$gt[, ncol(g$gt)])), if (om > 1e-3) 1e-6 else 1e-4)
      }
    }
    # fitted with omega -> 0 and kappa = 0 equals zero side terms
    th0 <- X$th; th0$kappa <- 0; th0$omega <- 0
    expect_lt(max(abs(TG[[1]]$m(X$mu, th0) - dec_set(X)[[1]]$m(X$mu, th0))), 1e-12)
  }
})

test_that("q > 1 best-worst (randomised QMC): gradients match central differences on fixed draws", {
  X <- dec_setup(3L, seed = 5L)
  ctrl <- rinf(".rum_dec_control")(rqmc = rinf(".rum_imp_control")(n_start = 512L, n_max = 512L))
  sp <- rinf(".rum_dec_specs")(X$p, dec_groups, X$menus, NULL, dec_allowed, TRUE, NULL)
  lay <- rinf(".rum_dec_layout")(X$data); V0 <- rinf(".rum_imp_axes")(X$Lam)$V
  # a fixed 512 points per scrambling: some respondents stop above the SE target (a warning, expected)
  ev <- function(mu, Lam, s) suppressWarnings(rinf(".rum_dec_rqmc")(mu, Lam, sp[[s]], lay, 0, ctrl, V = V0))
  h <- 1e-6
  for (s in seq_along(sp)) {
    e <- ev(X$mu, X$Lam, s)
    gm <- sapply(seq_len(X$p), function(j) { a <- b <- X$mu; a[, j] <- a[, j] + h; b[, j] <- b[, j] - h
      (ev(a, X$Lam, s)$m - ev(b, X$Lam, s)$m) / (2 * h) })
    gl <- sapply(seq_along(X$Lam), function(j) { a <- b <- X$Lam; a[j] <- a[j] + h; b[j] <- b[j] - h
      (ev(X$mu, a, s)$m - ev(X$mu, b, s)$m) / (2 * h) })
    glp <- rinf(".rum_rotation_project")(cbind(0, gl, 0), X$Lam)[, 1L + seq_along(X$Lam)]
    expect_lt(max(abs(e$gm - gm)), 2e-5)
    expect_lt(max(abs(e$gl - glp)), 2e-5)
  }
  # the default (canonical axes, convergence control) agrees with the fixed-axis rule
  TG <- dec_set(X, pairs = NULL)
  for (s in seq_along(sp)) expect_lt(max(abs(TG[[s]]$m(X$mu, X$th) - ev(X$mu, X$Lam, s)$m)), 1e-3)
})

test_that("values and gradients do not depend on the rotation of the loading (random orthogonal A)", {
  for (q in 2:3) {
    X <- dec_setup(q, seed = 6L + q); set.seed(40 + q); A <- qr.Q(qr(matrix(stats::rnorm(q * q), q)))
    thA <- X$th; thA$loading <- X$Lam %*% A
    a <- dec_set(X); b <- dec_set(X)
    for (k in seq_along(a)) {
      g1 <- a[[k]]$grad(X$mu, X$th); g2 <- b[[k]]$grad(X$mu, thA)
      expect_lt(dec_relmax(g2$m, g1$m), 1e-6)
      expect_lt(max(abs(g2$gm - g1$gm)) / max(abs(g1$gm)), 1e-6)
      gl1 <- g1$gt[, 1L + seq_len(X$p * q)]; gl2 <- g2$gt[, 1L + seq_len(X$p * q)]
      gl1A <- t(vapply(seq_len(X$n), function(i) as.vector(matrix(gl1[i, ], X$p, q) %*% A), numeric(X$p * q)))
      expect_lt(max(abs(gl2 - gl1A)) / max(abs(gl1)), 1e-6)       # dm/d(Lambda A) = (dm/dLambda) A
      # the loading block has no rotation component
      expect_lt(max(abs(rinf(".rum_rotation_project")(g1$gt, X$Lam) - g1$gt)), 1e-12)
    }
  }
})

test_that("brute-force Monte Carlo of the flips agrees with the target (|z| <= 3 on at least two seeds of three)", {
  # taste draws, one logistic shock per task, both assignments of the best and worst (allowed) levels
  # built explicitly; a flip is a task whose choice differs between the two
  flips <- function(X, cols, menu, P, R, seed) {
    set.seed(seed); d <- X$data; own <- rep(seq_len(d$n), d$T); Lam <- matrix(X$Lam, X$p)
    z <- vapply(seq_len(d$n), function(i) {
      rows <- which(own == i); Xt <- d$d[rows, , drop = FALSE]; Xt[, cols] <- 0
      B <- sweep(matrix(stats::rnorm(R * X$q), R) %*% t(Lam), 2, X$mu[i, ], `+`)
      ctx <- B %*% t(Xt); lv <- B[, cols, drop = FALSE] %*% t(menu)
      gain <- lv[, P[, 1], drop = FALSE] - lv[, P[, 2], drop = FALSE]
      pk <- max.col(gain, ties.method = "first")
      focal <- lv[cbind(seq_len(R), P[pk, 1])] - lv[cbind(seq_len(R), P[pk, 2])]
      eps <- matrix(stats::rlogis(R * ncol(ctx)), R)
      x <- rowMeans(((ctx + focal + eps) > 0) != ((ctx - focal + eps) > 0))
      c(mean(x), stats::var(x) / R)
    }, numeric(2))
    c(est = mean(z[1, ]), se = sqrt(sum(z[2, ])) / d$n)
  }
  SP <- dec_spec_pairs(dec_menus())
  for (q in 1:2) {
    X <- dec_setup(q, seed = 20L + q); TG <- dec_set(X)
    for (k in c(1L, 2L, 3L)) {
      g <- dec_spec_group[k]; tv <- mean(TG[[k]]$m(X$mu, X$th))
      zs <- vapply(1:3, function(sd) { r <- flips(X, dec_groups[[g]], X$menus[[g]], SP[[k]], 20000L, 100L * q + sd)
        unname((r["est"] - tv) / r["se"]) }, 0)
      expect_lte(sum(abs(zs) > 3), 1L)
    }
  }
})

test_that("q = 3 named swap (two-dimensional rule) matches q-dimensional randomised QMC", {
  X <- dec_setup(3L, seed = 8L)
  a <- dec_set(X)
  b <- dec_set(X, control = rinf(".rum_dec_control")(swap = "rqmc",
                 rqmc = rinf(".rum_imp_control")(tol = 2e-5, n_max = 32768L)))
  for (k in c(2L, 4L)) {
    expect_identical(a[[k]]$type, "swap")
    ga <- a[[k]]$grad(X$mu, X$th); gb <- b[[k]]$grad(X$mu, X$th); se <- attr(gb, "se")
    expect_null(attr(ga, "se"))
    expect_true(all(abs(ga$m - gb$m) <= pmax(4 * se, 1e-6)))
    expect_lt(max(abs(ga$m - gb$m)), 1e-4)
    expect_lt(max(abs(ga$gm - gb$gm)), 2e-3)
  }
  # the 2-D rule has converged in its orders
  f <- dec_set(X, control = rinf(".rum_dec_control")(width = .5, n_gl = 24L, n_gh = 80L, n_lg = 64L))
  for (k in c(2L, 4L)) expect_lt(max(abs(f[[k]]$m(X$mu, X$th) - a[[k]]$m(X$mu, X$th))), 1e-8)
  # the c-direction rules (Gauss-Hermite in z, logistic-weight Gauss in the shock) agree where both apply
  rz <- rinf(".rum_normal_rule")(80L); rz <- list(x = rz$nodes, w = rz$weights); re <- rinf(".rum_logis_rule")(64L)
  aa <- c(-3, -.4, 0, 1.2, 4); ss <- c(1.8, 2, 2.2, 1.9, 2.1)
  i1 <- rinf(".rum_dec_inner")(aa, ss, rz, re, 100, TRUE); i2 <- rinf(".rum_dec_inner")(aa, ss, rz, re, 0, TRUE)
  for (nm in c("I0", "I1", "Iz", "I2")) expect_lt(max(abs(i1[[nm]] - i2[[nm]])), 1e-7)
})

test_that("GS allowed pairs: vector and matrix forms agree, restricted <= unrestricted, explicit pairs", {
  X <- dec_setup(1L, seed = 9L)
  al <- dec_allowed$dummy; Am <- outer(al, al, `|`); diag(Am) <- FALSE
  v_vec <- dec_set(X, pairs = NULL)[[2]]$m(X$mu, X$th)
  v_mat <- dec_set(X, pairs = NULL, allowed = list(rate = NULL, dummy = Am, rev = NULL))[[2]]$m(X$mu, X$th)
  v_all <- dec_set(X, pairs = NULL, allowed = NULL)[[2]]$m(X$mu, X$th)
  expect_identical(v_vec, v_mat)
  expect_true(all(v_vec <= v_all + 1e-12)); expect_gt(max(v_all - v_vec), 1e-4)
  # an upper-triangular matrix is symmetrised (a swap is the same pair in both directions)
  Au <- Am; Au[lower.tri(Au)] <- FALSE
  expect_identical(dec_set(X, pairs = NULL, allowed = list(rate = NULL, dummy = Au, rev = NULL))[[2]]$m(X$mu, X$th), v_vec)
  # only one allowed pair: best-worst equals that named swap
  A1 <- matrix(FALSE, 4, 4); A1[3, 4] <- TRUE
  one <- dec_set(X, pairs = list(dummy = c(3, 4)), allowed = list(rate = NULL, dummy = A1, rev = NULL))
  expect_lt(max(abs(one[[2]]$m(X$mu, X$th) - one[[3]]$m(X$mu, X$th))), 1e-12)
  expect_error(dec_set(X, allowed = list(rate = NULL, dummy = c(TRUE, FALSE), rev = NULL)), "one entry per level")
  expect_error(dec_set(X, pairs = list(dummy = c(2, 2))), "two different levels")
})

test_that("BR rate swaps hold the displayed revenue fixed: the rate target reads revenue from the recorded task", {
  X <- dec_setup(2L, seed = 10L); TG <- dec_set(X, pairs = NULL)          # rate, dummy, rev (best-worst)
  v <- TG[[1]]$m(X$mu, X$th)
  # the recorded rate columns do not enter the rate target (the swap replaces them)...
  X2 <- X; X2$data$d[, 1:2] <- X2$data$d[, 1:2] * -3 + 1
  expect_identical(dec_set(X2, pairs = NULL)[[1]]$m(X$mu, X$th), v)
  # ... the recorded revenue columns do (they stay as displayed, in the context)
  X3 <- X; X3$data$d[, 6:8] <- X3$data$d[, 6:8][nrow(X3$data$d):1, ]
  expect_gt(max(abs(dec_set(X3, pairs = NULL)[[1]]$m(X$mu, X$th) - v)), 1e-3)
  # and the rate swing is the menu's quadratic tax map: equal to the independent computation (q = 1)
  X1 <- dec_setup(1L, seed = 10L); xg <- seq(-10, 10, length.out = 16001L); wg <- stats::dnorm(xg) / sum(stats::dnorm(xg))
  ind <- dec_independent(X1, 1:2, X1$menus$rate, dec_ordered_pairs(4L), matrix(xg), wg)
  expect_lt(max(abs(dec_set(X1, pairs = NULL)[[1]]$m(X1$mu, X1$th) - ind)), 1e-6)
})

test_that("output shape as the importance set; kind not bounded; cache keyed on the loading's shape", {
  X <- dec_setup(1L, seed = 11L); TG <- dec_set(X, w = rep(c(1, 0), 4))
  for (t in TG) {
    expect_true(all(c("name", "kind", "w", "m", "grad") %in% names(t)))
    expect_identical(t$kind, "decisiveness"); expect_identical(t$class, "decisiveness")
    expect_identical(t$w, rep(c(1, 0), 4))
    g <- rinf(".rum_target_grad")(t, X$mu, X$th)
    expect_identical(names(g)[1:3], c("m", "gm", "gt"))
    expect_equal(dim(g$gm), c(X$n, X$p)); expect_equal(dim(g$gt), c(X$n, X$p + 2L))
    expect_true(all(g$m >= 0 & g$m <= 1))
  }
  expect_false("decisiveness" %in% rinf(".rum_bounded_kinds"))
  expect_identical(vapply(TG, `[[`, "", "type"), c("best_worst", "swap", "best_worst", "swap", "best_worst"))
  expect_identical(dec_set(X, names = paste0("t", 1:5))[[3]]$name, "t3")
  # cache: one shared computation per (mu, loading, shape); a new key recomputes
  cache <- get("cache", envir = environment(TG[[1]]$grad))
  a <- TG[[1]]$grad(X$mu, X$th); expect_null(cache$key$dim)
  expect_identical(TG[[1]]$grad(X$mu, X$th), a)
  thm <- X$th; thm$loading <- matrix(X$th$loading, X$p, 1L)
  b <- TG[[1]]$grad(X$mu, thm); expect_identical(cache$key$dim, c(X$p, 1L))
  expect_lt(max(abs(b$m - a$m)), 1e-15); expect_equal(dim(b$gt), dim(a$gt))
  # a p x 2 loading whose second column is 0 is the same model, recomputed on the q = 2 path
  th2 <- X$th; th2$loading <- cbind(X$th$loading, 0)
  c2 <- TG[[1]]$grad(X$mu, th2); expect_identical(cache$key$dim, c(X$p, 2L))
  expect_equal(dim(c2$gt), c(X$n, 2L * X$p + 2L)); expect_lt(max(abs(c2$m - a$m)), 5e-4)
  # q = 3 shapes, the integration SE of best-worst, and input checks
  X3 <- dec_setup(3L, seed = 12L); T3 <- dec_set(X3); g3 <- T3[[1]]$grad(X3$mu, X3$th)
  expect_equal(dim(g3$gt), c(X3$n, 3L * X3$p + 2L)); expect_true(all(attr(g3, "se") <= 1e-4))
  expect_true(all(g3$m >= 0 & g3$m <= 1))
  expect_error(T3[[1]]$m(X3$mu[-1, ], X3$th), "mu must be")
  Xb <- X; Xb$data$T[2] <- 0L
  expect_error(dec_set(Xb), "at least one recorded task")
})

test_that("decisiveness targets run through the one-step (finite estimates and SEs)", {
  fx <- readRDS(test_path("fixtures", "rum-inference-fixture.rds"))
  TG <- rinf(".rum_tgt_decisiveness_set")(fx$data, list(a = 1:2, b = 3L), pairs = list(a = c(2, 3)))
  r <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, function(k) fx$fins[[k]], TG)
  expect_identical(names(r$est), c("decisiveness: a", "decisiveness: a 2 vs 3", "decisiveness: b"))
  expect_true(all(is.finite(r$est)) && all(is.finite(r$IFP)))
  expect_true(all(r$plug > 0 & r$plug < 1))
  expect_true(all(r$kind == "decisiveness"))
})

# ---- fix round on the Codex review of 23cfc1c (report/codex_d1.md), one block per finding ----
dec_one <- function(x) list(d = matrix(x, 1L), offsets = c(0L, 1L), T = 1L, n = 1L, p = length(x), ids = "r1")

test_that("Codex finding 4: a one-level attribute is a constant-zero target with zero gradient", {
  # the Codex trigger: a valid one-row menu with best_worst = TRUE (crashed at 23cfc1c)
  mk <- function(L) rinf(".rum_tgt_decisiveness_set")(dec_one(c(1, 1)), list(a = 1L, b = 2L),
                                                        menus = list(a = matrix(0, 1, 1), b = NULL))
  TG <- mk()
  expect_identical(vapply(TG, `[[`, "", "name"), c("decisiveness: a", "decisiveness: b"))
  for (lo in list(c(.5, .4), cbind(c(.5, .4), c(-.2, .3)))) {
    th <- list(kappa = .1, omega = .2, loading = lo); g <- TG[[1]]$grad(matrix(c(.3, .2), 1), th)
    q <- NCOL(lo)
    expect_identical(g$m, 0); expect_identical(g$gm, matrix(0, 1, 2)); expect_identical(g$gt, matrix(0, 1, 2 * q + 2))
    expect_gt(TG[[2]]$m(matrix(c(.3, .2), 1), th), 0)                  # the other group is unaffected
  }
  # the same at several respondents, through the simulated design (a one-level rev menu)
  X <- dec_setup(1L, seed = 31L); mn <- X$menus; mn$rev <- matrix(c(.2, -.1, .4), 1)
  T1 <- rinf(".rum_tgt_decisiveness_set")(X$data, dec_groups, menus = mn, side = "fitted")
  g <- T1[[3]]$grad(X$mu, X$th)
  expect_identical(g$m, numeric(X$n)); expect_true(all(g$gm == 0) && all(g$gt == 0))
  # genuinely invalid restrictions on a multilevel menu still fail
  expect_error(rinf(".rum_tgt_decisiveness_set")(X$data, dec_groups, menus = X$menus,
                 allowed = list(rate = NULL, dummy = rep(FALSE, 4), rev = NULL)), "no allowed level pair")
  # a named swap needs two different levels of a one-level menu: still an error
  expect_error(rinf(".rum_tgt_decisiveness_set")(dec_one(c(1, 1)), list(a = 1L), menus = list(a = matrix(0, 1, 1)),
                 pairs = list(a = c(1, 2))), "two different levels")
})

test_that("Codex finding 1: a small nonzero loading axis is kept (values and gradients)", {
  gh <- rinf(".rum_normal_rule")(80L)
  zf <- seq(-12, 12, length.out = 240001L); wf <- stats::dnorm(zf) * (zf[2] - zf[1])
  # RQMC at a fixed 32,768 points per scrambling, so that its error is far below the tolerances
  ctl <- rinf(".rum_dec_control")(rqmc = rinf(".rum_imp_control")(n_start = 32768L, n_max = 32768L))
  # the Codex trigger: best-worst, mu = 0, Lambda = diag(1e-4, 1), binary group column 1, zero context.
  # m = E[2 G(1e-4 |z1|) - 1]; dm/dmu1 = 0 (symmetry); dm/dLambda11 = E[2 g(1e-4 |z1|) |z1|]
  TG <- rinf(".rum_tgt_decisiveness_set")(dec_one(c(0, 0)), list(a = 1L), control = ctl)
  g <- TG[[1]]$grad(matrix(0, 1, 2), list(kappa = 0, omega = 0, loading = diag(c(1e-4, 1))))
  ref_m <- sum(wf * (2 * stats::plogis(1e-4 * abs(zf)) - 1)); ref_l <- sum(wf * 2 * stats::dlogis(1e-4 * abs(zf)) * abs(zf))
  expect_lt(abs(g$m - ref_m), 1e-4 * ref_m)                           # 23cfc1c: 0 (ref 3.99e-5)
  expect_lt(abs(g$gm[1, 1]), 1e-9)                                    # 23cfc1c: 0.5
  expect_lt(abs(g$gt[1, 2] - ref_l), 1e-4 * ref_l)                    # 23cfc1c: 0 (ref 0.399)
  # with context (mu = (0, 0.3), task (0, 1)): a two-dimensional reference (fine grid x Gauss-Hermite);
  # the swing l1 |z1| is first order in l1, so dropping the axis would give 0 and a zero gradient
  ref2 <- function(l1) { S <- abs(l1 * zf)
    sum(vapply(seq_along(gh$nodes), function(k) gh$weights[k] *
      sum(wf * (stats::plogis(.3 + gh$nodes[k] + S) - stats::plogis(.3 + gh$nodes[k] - S))), 0)) }
  TG <- rinf(".rum_tgt_decisiveness_set")(dec_one(c(0, 1)), list(a = 1L), pairs = list(a = c(2, 1)), control = ctl)
  for (l1 in c(1e-4, 1e-7)) {
    th <- list(kappa = 0, omega = 0, loading = diag(c(l1, 1))); mu <- matrix(c(0, .3), 1)
    r0 <- ref2(l1); rl <- (ref2(l1 * 1.01) - ref2(l1 * .99)) / (.02 * l1)
    for (k in 1:2) {                                                    # best-worst (RQMC), named swap (2-D)
      g <- TG[[k]]$grad(mu, th); tl <- if (k == 1L) 1e-4 else 1e-7
      expect_lt(abs(g$m - r0), tl * r0); expect_lt(abs(g$gt[1, 2] - rl), tl * rl)
    }
  }
  # the named swap at a near-zero swing direction with D0 = 0: dm/dLambda11 = E[2 g(c) |z1|] (23cfc1c: 0)
  TS <- rinf(".rum_tgt_decisiveness_set")(dec_one(c(0, 1)), list(a = 1L), pairs = list(a = c(2, 1)), best_worst = FALSE)
  g <- TS[[1]]$grad(matrix(c(0, .3), 1), list(kappa = 0, omega = 0, loading = diag(c(1e-13, 1))))
  ref <- 2 * sqrt(2 / pi) * sum(gh$weights * stats::dlogis(.3 + gh$nodes))
  expect_lt(abs(g$gt[1, 2] - ref), 1e-8)
  # the decisiveness axes keep every nonzero axis; the importance axes are unchanged
  expect_identical(rinf(".rum_dec_axes")(diag(c(1e-4, 1)))$keep, 1:2)
  expect_identical(length(rinf(".rum_imp_axes")(diag(c(1e-4, 1)))$keep), 1L)
  expect_identical(rinf(".rum_dec_axes")(cbind(c(1, 2), 0))$keep, 1L)
})
