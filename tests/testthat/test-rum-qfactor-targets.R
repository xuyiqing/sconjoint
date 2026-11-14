# q-factor taste law in the RUM targets (Stage 1b of the 2026-11-14 q-factor run):
# beta_i = mu_i + Lambda u_i, Lambda p x q, u ~ N(0, I_q). Targets reduce d' Lambda u to a
# one-dimensional N(0, ||Lambda' d||^2) draw; importance shares integrate over the principal
# axes. Checked here at q = 2, 3 against brute force (product Gauss-Hermite in q + 1
# dimensions and Monte Carlo with 1e6 draws), against central differences, under rotations of
# Lambda, and at q = 1 against the one-factor code of 4eb9f86 (copied below).

rinf <- function(x) get(x, envir = asNamespace("sconjoint"))
gh <- function(k) { r <- rinf(".rum_normal_rule")(k); list(z = r$nodes, w = r$weights) }
# max absolute difference below tol (values and shapes; names ignored)
expect_close <- function(a, b, tol, info = NULL) {
  expect_equal(dim(a), dim(b))
  expect_lt(max(abs(unname(a) - unname(b))), tol, label = paste(c(info, "max abs difference"), collapse = ": "))
}

qf_setup <- function(q, n = 4L, p = 4L, seed = 1L, mu_sd = 0.6, lam_sd = 0.5) {
  set.seed(seed)
  mu <- matrix(rnorm(n * p, sd = mu_sd), n, p)
  Lam <- matrix(rnorm(p * q, sd = lam_sd), p, q)
  A <- qr.Q(qr(matrix(rnorm(q * q), q)))               # random orthogonal q x q
  list(mu = mu, Lam = Lam, A = A, n = n, p = p, q = q,
       th = list(kappa = 0.2, loading = Lam, omega = 0.4),
       D = rbind(c(1, -1, 0, 0.5), c(0, 1, 1, -1), c(0.5, 0, -1, 0)),
       cvec = c(1, 0, -1, 0.5))
}

# every target type of the engine, at a given setup
qf_targets <- function(X) {
  D <- X$D; rows <- list(D = D, side = c(1, -1, 1), coef = c(0.5, -0.5, 0.25))
  list(share = rinf(".rum_tgt_share")("share", X$cvec),
       choice = rinf(".rum_tgt_choice")("choice", D[1, ]),
       choice_pref = rinf(".rum_tgt_choice_pref")("choice_pref", D[2, ]),
       choice_set = rinf(".rum_tgt_choice_set")("choice_set", D, coef = c(0.5, 0.3, 0.2)),
       choice_set_pref = rinf(".rum_tgt_choice_set_pref")("choice_set_pref", D),
       rows_pos = rinf(".rum_tgt_choice_rows")("rows_pos", rows, position = TRUE),
       rows_nopos = rinf(".rum_tgt_choice_rows")("rows_nopos", rows, position = FALSE),
       importance = rinf(".rum_tgt_importance")("importance", 1L, list(1:2, 3L, 4L), centred = TRUE))
}

# brute-force values in (u_1..u_q, v): product Gauss-Hermite (order k per dimension) or Monte Carlo
G <- stats::plogis
brute <- function(X, nodes) {               # nodes: list(U = N x q, v = N, w = N)
  mu <- X$mu; Lam <- X$Lam; k <- X$th$kappa; s <- sqrt(X$th$omega); D <- X$D
  LU <- nodes$U %*% t(Lam)                  # N x p taste deviations
  E <- function(f) vapply(seq_len(X$n), function(i) sum(nodes$w * f(i)), 0)
  eta <- function(i, d) drop(sum(mu[i, ] * d) + LU %*% d)
  pn <- function(i, d) 0.5 * (G(k + s * nodes$v + eta(i, d)) + G(-k - s * nodes$v + eta(i, d)))
  rows <- list(D = D, side = c(1, -1, 1), coef = c(0.5, -0.5, 0.25))
  rv <- function(i, pos) Reduce(`+`, lapply(1:3, function(r)
    rows$coef[r] * G(drop(eta(i, D[r, ])) + if (pos) rows$side[r] * (k + s * nodes$v) else 0)))
  imp <- function(i) {
    b <- sweep(LU, 2, mu[i, ], `+`); S1 <- b[, 1] + b[, 2]
    V1 <- (b[, 1]^2 + b[, 2]^2) / 3 - (S1 / 3)^2; V2 <- b[, 3]^2 / 2 - (b[, 3] / 2)^2
    V3 <- b[, 4]^2 / 2 - (b[, 4] / 2)^2
    V1 / (V1 + V2 + V3)
  }
  list(share = E(function(i) as.numeric(eta(i, X$cvec) > 0)),
       choice = E(function(i) pn(i, D[1, ])),
       choice_pref = E(function(i) G(eta(i, D[2, ]))),
       choice_set = E(function(i) 0.5 * pn(i, D[1, ]) + 0.3 * pn(i, D[2, ]) + 0.2 * pn(i, D[3, ])),
       choice_set_pref = E(function(i) (G(eta(i, D[1, ])) + G(eta(i, D[2, ])) + G(eta(i, D[3, ]))) / 3),
       rows_pos = E(function(i) rv(i, TRUE)), rows_nopos = E(function(i) rv(i, FALSE)),
       importance = E(imp))
}
product_nodes <- function(q, k) {
  r <- gh(k); ix <- as.matrix(expand.grid(rep(list(seq_len(k)), q + 1L)))
  list(U = matrix(r$z[ix[, 1:q]], nrow(ix)), v = r$z[ix[, q + 1L]],
       w = apply(matrix(r$w[ix], nrow(ix)), 1L, prod))
}
mc_nodes <- function(q, R, seed) {
  set.seed(seed); list(U = matrix(rnorm(R * q), R, q), v = rnorm(R), w = rep(1 / R, R))
}

for (q in 2:3) {
  test_that(sprintf("q = %d: target values match product quadrature in q + 1 dimensions and 1e6 Monte Carlo draws", q), {
    X <- qf_setup(q); TG <- qf_targets(X)
    val <- lapply(TG, function(t) t$m(X$mu, X$th))
    pq <- brute(X, product_nodes(q, if (q == 2L) 30L else 16L))
    # the share is a closed form Phi(c'mu / ||Lambda'c||); the indicator does not suit a
    # product rule, so it is checked against Monte Carlo only
    for (nm in setdiff(names(TG), c("share", "importance")))
      expect_close(val[[nm]], pq[[nm]], 1e-7, nm)
    expect_equal(val$share, stats::pnorm(drop(X$mu %*% X$cvec) / sqrt(sum(crossprod(X$Lam, X$cvec)^2))))
    mc <- brute(X, mc_nodes(q, 1e6L, 100 + q))
    for (nm in setdiff(names(TG), "importance"))           # SE <= 0.5 / 1000 per value
      expect_lt(max(abs(val[[nm]] - mc[[nm]])), 2.5e-3, label = nm)
  })

  test_that(sprintf("q = %d: importance shares match Monte Carlo and converge in the quadrature order", q), {
    # mean tastes well away from b = 0 (the shares' only singularity), as in the applications
    X <- qf_setup(q, mu_sd = 2, lam_sd = 0.3, seed = 3L)
    GR <- list(1:2, 3L, 4L)
    for (g in 1:3) {
      t20 <- rinf(".rum_tgt_importance")("imp", g, GR, centred = TRUE)
      t40 <- rinf(".rum_tgt_importance")("imp", g, GR, centred = TRUE, order_multi = if (q == 2L) 40L else 28L)
      expect_close(t20$m(X$mu, X$th), t40$m(X$mu, X$th), if (q == 2L) 1e-6 else 1e-5)
    }
    # uncentred (orthonormal coding) and all groups through the set constructor
    set <- rinf(".rum_tgt_importance_set")(GR, centred = FALSE)
    tot <- Reduce(`+`, lapply(set, function(t) t$m(X$mu, X$th)))
    expect_lt(max(abs(tot - 1)), 1e-10)          # pruned nodes (weight < 1e-14) lose a little mass
    nodes <- mc_nodes(q, 1e6L, 7 + q); LU <- nodes$U %*% t(X$Lam)
    mc <- vapply(seq_len(X$n), function(i) { b <- sweep(LU, 2, X$mu[i, ], `+`); mean(b[, 3]^2 / rowSums(b^2)) }, 0)
    expect_lt(max(abs(set[[2]]$m(X$mu, X$th) - mc)), 2e-3)
  })

  test_that(sprintf("q = %d: exact gradients match central differences (.rum_target_grad numerical path)", q), {
    for (cfg in list(list(seed = 1L, mu_sd = 0.6), list(seed = 3L, mu_sd = 1.5))) {
      X <- qf_setup(q, seed = cfg$seed, mu_sd = cfg$mu_sd, lam_sd = 0.4); TG <- qf_targets(X)
      for (nm in setdiff(names(TG), "importance")) {
        tg <- TG[[nm]]; g <- rinf(".rum_target_grad")(tg, X$mu, X$th)
        tn <- tg; tn$grad <- NULL; gn <- rinf(".rum_target_grad")(tn, X$mu, X$th, h = 1e-5)
        expect_equal(dim(g$gt), c(X$n, X$p * q + 2L))
        expect_close(g$gm, gn$gm, 1e-7, nm)
        expect_close(g$gt, gn$gt, 1e-7, nm)
      }
      # the share has no exact gradient in the package: check the numerical one against the
      # closed form d Phi(c'mu / s) / d Lambda = -phi(t) t / s * c (Lambda'c)' / s
      g <- rinf(".rum_target_grad")(TG$share, X$mu, X$th)
      s <- sqrt(sum(crossprod(X$Lam, X$cvec)^2)); t <- drop(X$mu %*% X$cvec) / s
      gl <- outer(-stats::dnorm(t) * t / s, as.vector(outer(X$cvec, drop(crossprod(X$Lam, X$cvec)) / s)))
      expect_close(g$gt[, 1L + seq_len(X$p * q)], gl, 1e-8, NULL)
      expect_close(g$gm, outer(stats::dnorm(t) / s, X$cvec), 1e-8, NULL)
      expect_true(all(g$gt[, c(1L, X$p * q + 2L)] == 0))
    }
  })

  test_that(sprintf("q = %d: importance gradients match central differences and the Monte Carlo pathwise gradient", q), {
    # The importance value is computed on Lambda's principal axes Lambda V, so the central
    # difference also moves V; that part is the product rule's lack of rotation invariance (a
    # quadrature error) and vanishes as the order grows. The exact gradient holds V fixed. Both
    # are checked where the rule has converged: mean tastes well away from b = 0.
    X <- qf_setup(q, seed = 3L, mu_sd = 2, lam_sd = 0.3); GR <- list(1:2, 3L, 4L)
    tg <- rinf(".rum_tgt_importance")("imp", 1L, GR, centred = TRUE, order_multi = 30L)
    g <- rinf(".rum_target_grad")(tg, X$mu, X$th)
    tn <- tg; tn$grad <- NULL; gn <- rinf(".rum_target_grad")(tn, X$mu, X$th, h = 1e-5)
    expect_close(g$gm, gn$gm, 1e-8, NULL)
    expect_close(g$gt, gn$gt, 2e-5, NULL)
    expect_true(all(g$gt[, c(1L, X$p * q + 2L)] == 0))
    # pathwise Monte Carlo: d/dLambda E[share(mu + Lambda u)] = E[(d share / d b) u'] (1e6 draws)
    nodes <- mc_nodes(q, 1e6L, 11 + q)
    for (i in seq_len(X$n)) {
      b <- sweep(nodes$U %*% t(X$Lam), 2, X$mu[i, ], `+`); S1 <- b[, 1] + b[, 2]
      V1 <- (b[, 1]^2 + b[, 2]^2) / 3 - (S1 / 3)^2; Tt <- V1 + b[, 3]^2 / 4 + b[, 4]^2 / 4; sh <- V1 / Tt
      dV <- cbind(2 * b[, 1] / 3 - 2 * S1 / 9, 2 * b[, 2] / 3 - 2 * S1 / 9, b[, 3] / 2, b[, 4] / 2)
      gr <- -sh * dV; gr[, 1:2] <- gr[, 1:2] + dV[, 1:2]; gr <- gr / Tt
      pw <- gr[, rep(seq_len(X$p), q)] * nodes$U[, rep(seq_len(q), each = X$p)]
      se <- apply(pw, 2, stats::sd) / 1e3
      expect_true(all(abs(g$gt[i, 1L + seq_len(X$p * q)] - colMeans(pw)) < 5 * se + 1e-5))
      expect_true(all(abs(g$gm[i, ] - colMeans(gr)) < 5 * apply(gr, 2, stats::sd) / 1e3 + 1e-5))
    }
  })

  test_that(sprintf("q = %d: targets are invariant to rotating Lambda; gradients rotate with it", q), {
    X <- qf_setup(q); TG <- qf_targets(X); thR <- X$th; thR$loading <- X$Lam %*% X$A
    ix <- rinf(".rum_th_idx")(X$p, q)
    for (nm in names(TG)) {
      g <- rinf(".rum_target_grad")(TG[[nm]], X$mu, X$th)
      gR <- rinf(".rum_target_grad")(TG[[nm]], X$mu, thR)
      expect_close(gR$m, g$m, 1e-12, nm)
      expect_close(gR$gm, g$gm, 1e-10, nm)
      # m(Lambda) = m(Lambda A) for all Lambda, so dm/dLambda = (dm/dLambda at Lambda A) A'
      back <- t(apply(gR$gt[, ix$loading, drop = FALSE], 1L, function(r) as.vector(matrix(r, X$p) %*% t(X$A))))
      expect_close(back, g$gt[, ix$loading, drop = FALSE], 1e-9, nm)
      expect_close(gR$gt[, c(ix$kappa, ix$omega)], g$gt[, c(ix$kappa, ix$omega)], 1e-10, nm)
    }
  })

  test_that(sprintf("q = %d: theta-gradients have no component along the rotation directions", q), {
    X <- qf_setup(q); TG <- qf_targets(X); R <- rinf(".rum_rotation_dirs")(X$Lam)
    expect_equal(dim(R), c(X$p * q + 2L, q * (q - 1L) / 2L))
    for (nm in names(TG)) {
      g <- rinf(".rum_target_grad")(TG[[nm]], X$mu, X$th)
      tol <- if (is.null(TG[[nm]]$grad)) 1e-8 else 1e-12     # central differences vs exact
      expect_lt(max(abs(g$gt %*% R)), tol, label = nm)
    }
  })
}

test_that(".rum_rotation_dirs gives the tangents of Lambda expm(t A), A skew, in theta coordinates", {
  set.seed(2); p <- 5L; q <- 3L; Lam <- matrix(rnorm(p * q), p, q)
  R <- rinf(".rum_rotation_dirs")(Lam)
  expect_equal(colnames(R), c("rot1_2", "rot1_3", "rot2_3"))
  ix <- rinf(".rum_th_idx")(p, q)
  expect_true(all(R[c(ix$kappa, ix$omega), ] == 0))
  pr <- utils::combn(q, 2L)
  for (k in seq_len(ncol(pr))) {
    A <- matrix(0, q, q); A[pr[1, k], pr[2, k]] <- 1; A[pr[2, k], pr[1, k]] <- -1
    h <- 1e-6; rot <- function(t) { e <- eigen(t * A); Re(e$vectors %*% diag(exp(e$values)) %*% solve(e$vectors)) }
    fd <- (as.vector(Lam %*% rot(h)) - as.vector(Lam %*% rot(-h))) / (2 * h)
    expect_equal(R[ix$loading, k], fd, tolerance = 1e-8)
  }
  # placement after other entries (the means in the full score: D = p + n_th)
  R2 <- rinf(".rum_rotation_dirs")(Lam, offset = p, n_total = p + p * q + 2L)
  expect_equal(R2[p + seq_len(p * q + 2L), ], R)
  expect_true(all(R2[seq_len(p), ] == 0))
  # q = 1: no directions; projection is the identity
  expect_equal(dim(rinf(".rum_rotation_dirs")(Lam[, 1])), c(p + 2L, 0L))
  g <- matrix(rnorm(3 * (p + 2L)), 3)
  expect_identical(rinf(".rum_rotation_project")(g, Lam[, 1]), g)
  # projection removes exactly the rotation span
  g <- matrix(rnorm(4 * (p * q + 2L)), 4); gp <- rinf(".rum_rotation_project")(g, Lam)
  expect_lt(max(abs(gp %*% R)), 1e-12)
  expect_lt(max(abs((g - gp) - (g - gp) %*% R %*% solve(crossprod(R), t(R)))), 1e-12)
})

# ---- one factor: unchanged from 4eb9f86 ---------------------------------------------------
# The one-factor code of 4eb9f86 (verbatim arithmetic), to show that q = 1 is bitwise unchanged,
# for a plain loading vector and for a p x 1 loading matrix.
old_choice_fitted <- function(st, dvec, quad, random_side = TRUE) {
  m <- drop(st$mu %*% dvec); s <- sum(st$loading * dvec)
  eta <- outer(m, rep(1, length(quad$u))) + outer(rep(1, length(m)), s * quad$u)
  side <- if (random_side) sqrt(st$omega) * quad$v else 0 * quad$v
  sv <- matrix(side, nrow(eta), ncol(eta), byrow = TRUE)
  drop((0.5 * (stats::plogis(st$kappa + sv + eta) + stats::plogis(-st$kappa - sv + eta))) %*% quad$weight)
}
old_choice_set <- function(mu, th, D, coef, quad) {
  n <- nrow(mu); p <- ncol(mu); G <- length(quad$weight); s <- sqrt(th$omega)
  val <- numeric(n); gm <- matrix(0, n, p); gk <- gl <- go <- numeric(n); glv <- matrix(0, n, p)
  svm <- matrix(s * quad$v, n, G, byrow = TRUE); um <- matrix(quad$u, n, G, byrow = TRUE)
  vm <- matrix(quad$v, n, G, byrow = TRUE)
  for (r in seq_len(nrow(D))) {
    d <- D[r, ]; eta <- outer(drop(mu %*% d), rep(1, G)) + sum(th$loading * d) * um
    A <- th$kappa + svm + eta; B <- -th$kappa - svm + eta
    pa <- stats::plogis(A); pb <- stats::plogis(B); da <- pa * (1 - pa); db <- pb * (1 - pb)
    val <- val + coef[r] * drop((0.5 * (pa + pb)) %*% quad$weight)
    c1 <- drop((0.5 * (da + db)) %*% quad$weight)
    gm <- gm + coef[r] * outer(c1, d)
    gk <- gk + coef[r] * drop((0.5 * (da - db)) %*% quad$weight)
    glv <- glv + coef[r] * outer(drop((0.5 * (da + db) * um) %*% quad$weight), d)
    go <- go + coef[r] * (if (th$omega < 1e-8)
      drop((0.25 * (da * (1 - 2 * pa) + db * (1 - 2 * pb))) %*% quad$weight)
      else drop((0.5 * (da - db) * vm) %*% quad$weight) / (2 * s))
  }
  list(m = val, gm = gm, gt = cbind(gk, glv, go))
}
old_choice_set_pref <- function(mu, th, D, coef, quad) {
  n <- nrow(mu); p <- ncol(mu); G <- length(quad$weight)
  val <- numeric(n); gm <- matrix(0, n, p); glv <- matrix(0, n, p)
  um <- matrix(quad$u, n, G, byrow = TRUE)
  for (r in seq_len(nrow(D))) {
    d <- D[r, ]; eta <- outer(drop(mu %*% d), rep(1, G)) + sum(th$loading * d) * um
    pe <- stats::plogis(eta); de <- pe * (1 - pe)
    val <- val + coef[r] * drop(pe %*% quad$weight)
    gm  <- gm  + coef[r] * outer(drop(de %*% quad$weight), d)
    glv <- glv + coef[r] * outer(drop((de * um) %*% quad$weight), d)
  }
  list(m = val, gm = gm, gt = cbind(numeric(n), glv, numeric(n)))
}
old_rows <- function(mu, th, rows, position, order = 31L) {
  gh <- rinf(".rum_gh_rule")(order); n <- nrow(mu); p <- ncol(mu)
  m <- numeric(n); gm <- matrix(0, n, p); gl <- matrix(0, n, p); gk <- go <- numeric(n)
  ii <- seq_len(n); ni <- n; D <- rows$D; cf <- rows$coef; sd <- rows$side
  dl <- drop(D %*% th$loading)
  sig <- sqrt(dl^2 + if (position) th$omega else 0)
  MD <- mu[ii, , drop = FALSE] %*% t(D)
  if (position) MD <- MD + rep(th$kappa * sd, each = ni)
  V <- A <- B <- matrix(0, ni, nrow(D))
  for (j in seq_along(gh$z)) {
    P <- stats::plogis(MD + rep(sig * gh$z[j], each = ni)); dP <- P * (1 - P)
    V <- V + gh$w[j] * P; A <- A + gh$w[j] * dP; B <- B + (gh$w[j] * gh$z[j]) * dP
  }
  small <- sig < 1e-8
  K <- B / rep(pmax(sig, 1e-8), each = ni)
  if (any(small)) { P0 <- stats::plogis(MD[, small, drop = FALSE]); K[, small] <- P0 * (1 - P0) * (1 - 2 * P0) }
  m[ii] <- drop(V %*% cf); gm[ii, ] <- A %*% (cf * D); gl[ii, ] <- K %*% ((cf * dl) * D)
  if (position) { gk[ii] <- drop(A %*% (cf * sd)); go[ii] <- drop(K %*% cf) / 2 }
  list(m = m, gm = gm, gt = cbind(gk, gl, go))
}

test_that("q = 1 (a loading vector or a p x 1 matrix) is bitwise the one-factor code of 4eb9f86", {
  X <- qf_setup(1L); lam <- X$Lam[, 1]; quad <- rinf(".rum_law_quadrature")()
  rows <- list(D = X$D, side = c(1, -1, 1), coef = c(0.5, -0.5, 0.25))
  for (om in c(0.4, 5e-9, 0)) for (ld in list(lam, X$Lam)) {
    th <- list(kappa = 0.2, loading = ld, omega = om); tho <- list(kappa = 0.2, loading = lam, omega = om)
    expect_identical(rinf(".rum_tgt_share")("s", X$cvec)$m(X$mu, th),
                     stats::pnorm(drop(X$mu %*% X$cvec) / abs(sum(lam * X$cvec))))
    st <- c(list(mu = X$mu), th)
    expect_identical(rinf(".rum_choice_fitted")(st, X$D[1, ], quad, TRUE), old_choice_fitted(c(list(mu = X$mu), tho), X$D[1, ], quad, TRUE))
    expect_identical(rinf(".rum_choice_fitted")(st, X$D[1, ], quad, FALSE), old_choice_fitted(c(list(mu = X$mu), tho), X$D[1, ], quad, FALSE))
    cf <- c(0.5, 0.3, 0.2)
    expect_identical(rinf(".rum_tgt_choice_set")("cs", X$D, coef = cf)$grad(X$mu, th), old_choice_set(X$mu, tho, X$D, cf, quad))
    expect_identical(rinf(".rum_tgt_choice_set_pref")("csp", X$D, coef = cf)$grad(X$mu, th), old_choice_set_pref(X$mu, tho, X$D, cf, quad))
    for (pos in c(TRUE, FALSE))
      expect_identical(rinf(".rum_tgt_choice_rows")("r", rows, position = pos)$grad(X$mu, th), old_rows(X$mu, tho, rows, pos))
    # numerical gradients (.rum_target_grad) of the share and the one-contest choice
    for (tg in list(rinf(".rum_tgt_share")("s", X$cvec), rinf(".rum_tgt_choice")("c", X$D[1, ])))
      expect_identical(rinf(".rum_target_grad")(tg, X$mu, th), rinf(".rum_target_grad")(tg, X$mu, tho))
  }
})

test_that("q = 1 importance shares reproduce the 21-node single-feature target of test-rum-correction.R", {
  X <- qf_setup(1L, n = 6L); UQ <- rinf(".rum_gh_rule")(21L)
  imp_old <- function(mu, th, g) { acc <- 0
    for (j in seq_along(UQ$z)) {
      B <- mu + matrix(th$loading * UQ$z[j], nrow(mu), ncol(mu), byrow = TRUE)
      acc <- acc + UQ$w[j] * B[, g]^2 / rowSums(B^2)
    }
    acc }
  th <- list(kappa = 0.1, loading = X$Lam[, 1], omega = 0.5)
  set <- rinf(".rum_tgt_importance_set")(as.list(1:4), centred = FALSE, uq = list(u = UQ$z, w = UQ$w))
  for (g in 1:4) expect_close(set[[g]]$m(X$mu, th), imp_old(X$mu, th, g), 1e-14, NULL)
  # a p x 1 matrix loading gives the same; gradients match central differences
  thm <- th; thm$loading <- X$Lam
  expect_identical(set[[2]]$grad(X$mu, thm), set[[2]]$grad(X$mu, th))
  tn <- set[[2]]; tn$grad <- NULL
  expect_close(set[[2]]$grad(X$mu, th)$gt, rinf(".rum_target_grad")(tn, X$mu, th)$gt, 1e-7, NULL)
})

# ---- wrong-model simulator: a p x K truth ---------------------------------------------------
ms_toyq <- function(n = 400L, K = 3L, seed = 11L) {
  sp <- rinf(".rum_spec_arg")("br"); p <- sp$p
  rinf(".rum_with_seed")(seed, {
    mu <- matrix(stats::rnorm(n * p, sd = 0.3), n, p); L <- matrix(stats::rnorm(p * K, sd = 0.25), p, K)
  })
  dimnames(mu) <- list(sprintf("r%04d", seq_len(n)), sp$features)
  list(mu = mu, L = L, kappa = 0.1, omega = 0.3, spec = sp)
}

test_that("a p x 2 loading matrix is the two-factor interface; a zero third factor changes nothing", {
  X <- ms_toyq(K = 2L)
  t_old <- rinf(".rum_ms_truth")(X$mu, X$kappa, X$L[, 1], X$omega, loading2 = X$L[, 2], pi_sl = 0.1)
  t_mat <- rinf(".rum_ms_truth")(X$mu, X$kappa, X$L, X$omega, pi_sl = 0.1)
  expect_identical(t_mat, t_old)
  a <- rinf(".rum_ms_simulate")(t_old, X$spec, seed = 5); b <- rinf(".rum_ms_simulate")(t_mat, X$spec, seed = 5)
  expect_identical(a, b)
  t3 <- rinf(".rum_ms_truth")(X$mu, X$kappa, cbind(X$L, 0), X$omega, pi_sl = 0.1)
  expect_identical(t3$type, "two-factor + straight-liners")
  c3 <- rinf(".rum_ms_simulate")(t3, X$spec, seed = 5)
  expect_identical(c3$y, a$y); expect_identical(c3$d, a$d)
  expect_identical(c3$metadata$simulation$latent[names(a$metadata$simulation$latent)], a$metadata$simulation$latent)
  # a nonzero third factor moves choices only through u3
  t3b <- rinf(".rum_ms_truth")(X$mu, X$kappa, cbind(X$L, X$L[, 1]), X$omega, pi_sl = 0.1)
  expect_identical(t3b$type, "3-factor + straight-liners")
  d3 <- rinf(".rum_ms_simulate")(t3b, X$spec, seed = 5)
  expect_identical(d3$metadata$simulation$latent$u3, c3$metadata$simulation$latent$u3)
  expect_identical(d3$metadata$simulation$latent$u1, a$metadata$simulation$latent$u1)
  expect_false(identical(d3$y, a$y))
  expect_error(rinf(".rum_ms_truth")(X$mu, X$kappa, X$L, X$omega, loading2 = X$L[, 1]), "loading2")
  expect_error(rinf(".rum_ms_truth")(X$mu, X$kappa, X$L[-1, ], X$omega), "loading")
  # expected moments: a zero third factor gives the two-factor values exactly
  dirs <- cbind(top = a$d[, 11]); sc <- 1
  expect_identical(rinf(".rum_ms_expected_moments")(t3, a, dirs, sc), rinf(".rum_ms_expected_moments")(t_old, a, dirs, sc))
})

test_that("a three-factor truth: simulated choices agree with the expected moments", {
  X <- ms_toyq(n = 2500L, K = 3L, seed = 4L)
  tr <- rinf(".rum_ms_truth")(X$mu, X$kappa, X$L * 2, X$omega, pi_sl = 0.05, sl_side1 = 0.6)
  s <- rinf(".rum_ms_simulate")(tr, X$spec, seed = 31)
  lat <- s$metadata$simulation$latent
  # the choices are the RUM's: eta from the stored latent draws
  ri <- rep(seq_len(s$n), s$T)
  B <- X$mu[ri, ] + outer(lat$u1[ri], tr$L[, 1]) + outer(lat$u2[ri], tr$L[, 2]) + outer(lat$u3[ri], tr$L[, 3])
  eta <- X$kappa + rowSums(s$d * B) + sqrt(X$omega) * lat$v[ri]
  att <- !lat$straight[ri]
  expect_lt(abs(mean(s$y[att]) - mean(stats::plogis(eta[att]))), 4 * 0.5 / sqrt(sum(att)))
  dirs <- cbind(top = s$d[, 11], prog = drop(s$d[, seq(1, 11, 2)] %*% c(-5, -3, -1, 1, 3, 5)))
  sc <- sqrt(colMeans(dirs^2))
  ob <- rinf(".rum_ms_observed_moments")(s, dirs, sc)
  ex <- rinf(".rum_ms_expected_moments")(tr, s, dirs, sc, order = c(11L, 11L, 9L))
  r <- ob$pair - ex$pair
  z <- colMeans(r) / (apply(r, 2, stats::sd) / sqrt(s$n))
  expect_true(all(abs(z) < 4))
  zs <- (sum(ob$same) - sum(ex$same)) / sqrt(sum(ex$same * (1 - ex$same)))
  expect_lt(abs(zs), 4)
})

test_that("true values under a three-factor truth match brute-force Monte Carlo (1e6 draws)", {
  X <- ms_toyq(n = 6L, K = 3L, seed = 6L); FN <- colnames(X$mu); p <- length(FN); n <- 6L
  tr <- rinf(".rum_ms_truth")(X$mu, X$kappa, X$L, X$omega, pi_sl = 0.2, sl_side1 = 0.65)
  L <- tr$L
  cc <- setNames(numeric(p), FN); cc[c(1, 11)] <- c(-1, 1)
  d2 <- rbind(sign(seq_len(p) %% 3 - 1) * 0.5)
  raw <- rinf(".rum_design_draw_raw")("br", 3L, seed = 9)
  GR <- c(lapply(1:6, function(j) c(2L * j - 1L, 2L * j)), list(13:16))
  tg <- list(rinf(".rum_tgt_share")("share", cc),
             rinf(".rum_tgt_choice_set_pref")("win", d2),
             rinf(".rum_tgt_choice_set")("win (position)", d2),
             rinf(".rum_ms_switch_target")("ame", "br", raw, "r6", 6L, 1L, position = FALSE),
             rinf(".rum_ms_switch_target")("ame (position)", "br", raw, "r6", 6L, 1L, position = TRUE),
             rinf(".rum_tgt_importance")("importance: bracket 6", 6L, GR, centred = FALSE))
  tv <- rinf(".rum_ms_true_values")(list(c = tr), tg)
  expect_equal(tv$scope, c("taste", "taste", "behaviour", "taste", "behaviour", "taste"))
  expect_true(all(tv$quad_err[1:5] < 2e-5))
  R <- 1e6L
  mc <- rinf(".rum_with_seed")(77, {
    U <- matrix(stats::rnorm(R * 3), R, 3); v <- stats::rnorm(R)
    LU <- U %*% t(L)
    per_i <- function(f) mean(vapply(seq_len(n), f, 0))
    rowsP <- function(Rw, position) per_i(function(i) {
      out <- 0
      for (r in seq_len(nrow(Rw$D))) {
        e <- sum(X$mu[i, ] * Rw$D[r, ]) + drop(LU %*% Rw$D[r, ])
        if (position) e <- e + Rw$side[r] * (X$kappa + sqrt(X$omega) * v)
        out <- out + Rw$coef[r] * mean(stats::plogis(e))
      }
      out
    })
    imp <- per_i(function(i) { b <- sweep(LU, 2, X$mu[i, ], `+`)
      mean(rowSums(b[, 11:12]^2) / rowSums(b^2)) })
    c(per_i(function(i) mean(sum(X$mu[i, ] * cc) + drop(LU %*% cc) > 0)),
      rowsP(list(D = d2, side = 1, coef = 1), FALSE),
      rowsP(list(D = rbind(d2, d2), side = c(1, -1), coef = c(0.5, 0.5)), TRUE),
      rowsP(tg[[4]]$rows[[1]], FALSE), rowsP(tg[[5]]$rows[[1]], TRUE), imp)
  })
  # behavioural targets mix in the straight-liners' side choice
  mc[c(3, 5)] <- 0.8 * mc[c(3, 5)] + 0.2 * c(0.5, sum(tg[[5]]$rows[[1]]$coef * ifelse(tg[[5]]$rows[[1]]$side > 0, 0.65, 0.35)))
  expect_true(all(abs(tv$value - mc) < 3e-3))
  # the law-kind (importance) value is the target's own m at the truth's loading matrix
  th <- list(kappa = X$kappa, loading = L, omega = X$omega)
  expect_equal(tv$value[6], mean(tg[[6]]$m(X$mu, th)))
})
