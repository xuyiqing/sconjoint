# Stage 2 of the q-factor build (2026-11-14): the inference layer with n_fac = q > 1 taste factors,
#   eta_it = kappa + d_it' mu_i + d_it' Lambda u_i + sqrt(omega) v_i,  u_i ~ N(0, I_q).
# Scores (plain-R reference against C++ and finite differences), the conditional information's
# (q + 2)T stack, H3 and Louis variants, the model information, the removal of the q(q-1)/2
# rotation directions from the information (build plan D3), rotation invariance of one-step and
# bounded estimates and SEs, the common-parameter check, the path score, and p = 1.
# Data come from the Stage 1b q-factor simulator truth (.rum_ms_truth with a p x q loading,
# .rum_ms_simulate) on a small toy design law (p = 3 features, ternary codes). Untrained networks
# are enough here: every property tested holds at any fitted state. Single-threaded; nothing is
# written outside tempdir().

rq <- function(nm) get(nm, envir = asNamespace("sconjoint"))
toy_spec <- function(p = 3L, T = 4L) {
  fn <- paste0("f", seq_len(p))
  structure(list(study = "toy", T = T, p = p, features = fn, uses_resp = FALSE,
    draw = function(M, T = 4L) list(x = array(sample(c(-1, 0, 1), M * T * p, TRUE), c(M, T, p))),
    code = function(raw, key = NULL) { a <- raw$x; dimnames(a) <- list(NULL, NULL, fn); a },
    key = function(resp) "all"), class = "rum_design_spec")
}
rand_orth <- function(q, seed) { set.seed(seed); qr.Q(qr(matrix(stats::rnorm(q * q), q))) }
relmax <- function(x, y) max(abs(x - y) / pmax(abs(y), 1e-12))
# n respondents, covariates Z, an untrained network as the mean, data simulated from the q-factor
# truth (mu(Z), kappa 0.1, Lambda, omega 0.4) on the toy design, three folds whose fitted states
# differ a little (kappa, Lambda, omega), each loading on its principal axes.
qf_setup <- function(q, n = 45L, p = 3L, seed = 11L, T = 4L) {
  spec <- toy_spec(p, T); set.seed(seed)
  ids <- sprintf("r%03d", seq_len(n))
  Z <- matrix(stats::rnorm(n * 2L), n, 2L, dimnames = list(ids, c("z1", "z2")))
  net <- rum_dnn_gaussian(2L, p, 4L, seed = seed)
  mu <- rum_dnn_gaussian_forward(net, Z); dimnames(mu) <- list(ids, spec$features)
  L <- matrix(stats::rnorm(p * q, 0, .5), p, q)
  truth <- rq(".rum_ms_truth")(mu, 0.1, L, 0.4)
  data <- rq(".rum_ms_simulate")(truth, spec, seed = seed)
  fins <- lapply(1:3, function(k) list(network = net, kappa = 0.1 + 0.02 * k,
    loading = rq(".rum_canonical_loading")(L + 0.05 * k * matrix(cos(seq_len(p * q)), p, q)),
    omega = 0.4 + 0.03 * k))
  list(spec = spec, Z = Z, data = data, fold = rep_len(1:3, n), fins = fins, L = L, p = p, q = q,
       n = n, mu = mu)
}
qf_targets <- function(p) {
  e1 <- replace(numeric(p), 1, 1); c12 <- replace(numeric(p), 1:2, c(1, -0.5))
  c(list(rq(".rum_tgt_share")("share f1", e1), rq(".rum_tgt_share")("share f1-f2", c12),
         rq(".rum_tgt_mean")("mean f2", replace(numeric(p), 2, 1)),
         rq(".rum_tgt_choice_set_pref")("win c12", rbind(c12)),
         rq(".rum_tgt_choice_set")("set c12", rbind(c12, e1))),
    rq(".rum_tgt_importance_set")(as.list(seq_len(p)), names = paste0("imp", seq_len(p))))
}
# B respondents sharing T tasks: MU (B x p), DL (T matrices B x p), Y (B x T), and the same as rum_data
tiny_q <- function(q, B = 5L, p = 3L, T = 3L, seed = 1L) {
  set.seed(seed)
  MU <- matrix(stats::rnorm(B * p, 0, .5), B, p); L <- matrix(stats::rnorm(p * q, 0, .6), p, q)
  DL <- lapply(seq_len(T), function(t) matrix(sample(c(-1, 0, 1), B * p, TRUE), B, p))
  Y <- matrix(stats::rbinom(B * T, 1, .5), B, T)
  d <- lapply(seq_len(B), function(i) t(vapply(seq_len(T), function(t) DL[[t]][i, ], numeric(p))))
  data <- rum_data(lapply(seq_len(B), function(i) Y[i, ]), d, metadata = list(orientation = "test"))
  list(MU = MU, DL = DL, Y = Y, data = data, th = list(kappa = .2, loading = L, omega = .4),
       p = p, q = q, T = T, B = B)
}
small_rule <- function(q) if (q == 2L) rum_quadrature(c(7L, 7L, 7L), n_fac = 2L, rule = "product") else
  rum_quadrature(c(5L, 5L, 5L, 5L), n_fac = 3L, rule = "product")
# sum over all 2^T sequences of one design: the scores of every sequence, p(y), and the info
exact_one <- function(Dm, mu_i, th, quad) {
  T <- nrow(Dm); p <- ncol(Dm); Y <- rq(".rum_cond_Y")(T); R <- nrow(Y)
  sc <- function(mu_i, th) rq(".rum_seq_scores")(matrix(mu_i, R, p, byrow = TRUE),
    lapply(seq_len(T), function(t) matrix(Dm[t, ], R, p, byrow = TRUE)), Y, th, quad)
  e <- rq(".rum_cond_eta")(th$kappa + drop(Dm %*% mu_i), Dm %*% th$loading, sqrt(th$omega), quad)
  py <- exp(rq(".rum_cond_post")(Y, e, log(quad$weight))$logp)
  S <- sc(mu_i, th)
  list(I = crossprod(S * py, S), S = S, py = py, sc = sc, Y = Y, e = e)
}

# ---- scores --------------------------------------------------------------------------------------

for (q in 2:3) {
  test_that(paste0("q=", q, ": .rum_seq_scores equals the C++ scores and central differences"), {
    X <- tiny_q(q, seed = q); quad <- small_rule(q); p <- X$p; th <- X$th
    S <- rq(".rum_seq_scores")(X$MU, X$DL, X$Y, th, quad)
    expect_identical(ncol(S), p + p * q + 2L)
    ev <- rum_dnn_gaussian_evaluate(X$data, X$MU, th$kappa, th$loading, th$omega, quad)
    expect_lt(max(abs(S - cbind(ev$output_score, ev$common_score))), 1e-12)
    ll <- function(mu, kappa, L, omega) rum_dnn_gaussian_evaluate(X$data, mu, kappa, L, omega, quad)$loglik
    h <- 1e-6; FD <- matrix(0, X$B, ncol(S))
    for (j in seq_len(p)) { m1 <- X$MU; m2 <- X$MU; m1[, j] <- m1[, j] + h; m2[, j] <- m2[, j] - h
      FD[, j] <- (ll(m1, th$kappa, th$loading, th$omega) - ll(m2, th$kappa, th$loading, th$omega)) / (2 * h) }
    v <- rq(".rum_th_vec")(th); ix <- rq(".rum_th_idx")(p, q)
    for (j in seq_along(v)) { v1 <- v; v2 <- v; v1[j] <- v1[j] + h; v2[j] <- v2[j] - h
      a <- rq(".rum_th_list")(v1, p, q); b <- rq(".rum_th_list")(v2, p, q)
      FD[, p + j] <- (ll(X$MU, a$kappa, a$loading, a$omega) - ll(X$MU, b$kappa, b$loading, b$omega)) / (2 * h) }
    expect_lt(max(abs(S - FD)), 1e-7)
    # omega at the boundary: the right derivative (no division by sqrt(omega))
    th0 <- th; th0$omega <- 0
    S0 <- rq(".rum_seq_scores")(X$MU, X$DL, X$Y, th0, quad)
    r0 <- (ll(X$MU, th$kappa, th$loading, 2 * h) - ll(X$MU, th$kappa, th$loading, h)) / h
    expect_lt(max(abs(S0[, ncol(S0)] - r0)), 1e-4)
  })

  # ---- conditional information: the (q + 2)T stack, H3, Louis ------------------------------------
  test_that(paste0("q=", q, ": A_D c(y) equals the scores, sum_y p(y) s(y) = 0, I = A K A'"), {
    X <- tiny_q(q, seed = 10L + q); quad <- small_rule(q); p <- X$p
    Dm <- t(vapply(seq_len(X$T), function(t) X$DL[[t]][1, ], numeric(p)))
    ref <- exact_one(Dm, X$MU[1, ], X$th, quad); s <- sqrt(X$th$omega)
    A <- rq(".rum_cond_A")(Dm, s, q)
    expect_identical(dim(A), c(p + p * q + 2L, (q + 2L) * X$T))
    po <- rq(".rum_cond_post")(ref$Y, ref$e, log(quad$weight))
    cc <- rq(".rum_cond_c")(ref$Y, po$Pi, ref$e, quad)
    expect_identical(ncol(cc), (q + 2L) * X$T)
    expect_lt(max(abs(cc %*% t(A) - ref$S)), 1e-12)
    expect_lt(abs(sum(ref$py) - 1), 1e-12)
    expect_lt(max(abs(colSums(ref$S * ref$py))), 1e-12)
    K <- rq(".rum_cond_K")(th_a <- X$th$kappa + drop(Dm %*% X$MU[1, ]), Dm %*% X$th$loading, s, quad, "exact", ref$Y)
    expect_lt(max(K$check), 1e-12)
    expect_lt(max(abs(A %*% K$K %*% t(A) - ref$I)), 1e-12)
    # H3 = E_g[c_g c_g'] over the complete data, by brute force over nodes and choices
    H3 <- rq(".rum_cond_H3")(ref$e, quad); Zq <- cbind(1, quad$u, quad$v)
    Hb <- matrix(0, (q + 2L) * X$T, (q + 2L) * X$T)
    for (g in seq_along(quad$weight)) for (t in seq_len(X$T)) {
      pr <- ref$e$P[t, g]; ix <- (seq_len(q + 2L) - 1L) * X$T + t
      Hb[ix, ix] <- Hb[ix, ix] + quad$weight[g] * pr * (1 - pr) * tcrossprod(Zq[g, ])
    }
    expect_lt(max(abs(H3 - Hb)), 1e-13)
  })

  test_that(paste0("q=", q, ": information identity E[s s'] = -E[d s / d theta] (finite differences)"), {
    X <- tiny_q(q, seed = 20L + q); quad <- small_rule(q); p <- X$p
    Dm <- t(vapply(seq_len(X$T), function(t) X$DL[[t]][2, ], numeric(p)))
    ref <- exact_one(Dm, X$MU[2, ], X$th, quad)
    v0 <- c(X$MU[2, ], rq(".rum_th_vec")(X$th)); D <- length(v0)
    unp <- function(v) list(mu = v[seq_len(p)], th = rq(".rum_th_list")(v[-seq_len(p)], p, q))
    h <- 1e-5; H <- matrix(0, D, D)
    for (j in seq_len(D)) {
      v1 <- v0; v2 <- v0; v1[j] <- v1[j] + h; v2[j] <- v2[j] - h; a <- unp(v1); b <- unp(v2)
      H[, j] <- -colSums(ref$py * (ref$sc(a$mu, a$th) - ref$sc(b$mu, b$th))) / (2 * h)
    }
    expect_lt(max(abs((H + t(H)) / 2 - ref$I)) / max(abs(ref$I)), 1e-6)
  })

  test_that(paste0("q=", q, ": Louis (both forms) and simulated choices agree with exact enumeration"), {
    X <- qf_setup(q, n = 3L, seed = 30L + q, T = 3L); quad <- small_rule(q)
    set.seed(4); M <- 3L
    crn <- list(designs = list(all = array(sample(c(-1, 0, 1), M * 3L * X$p, TRUE), c(M, 3L, X$p))),
                key = rep(1L, X$n), M = M, T = 3L)
    th <- X$fins[[1]][c("kappa", "loading", "omega")]; ci <- rq(".rum_cond_info")
    ex <- ci(X$data, X$mu, th, crn, "exact", quad)
    expect_lt(max(ex$check), 1e-12)
    rel <- function(r) max(abs(r$I - ex$I)) / max(abs(ex$I))
    expect_lt(rel(ci(X$data, X$mu, th, crn, "louis", quad, ny = 3000L, seed = 5L)), 0.04)
    expect_lt(rel(ci(X$data, X$mu, th, crn, "louis", quad, ny = 3000L, seed = 5L, louis_rb = FALSE)), 0.04)
    e_small <- mean(vapply(1:3, function(s) rel(ci(X$data, X$mu, th, crn, "mc", quad, ny = 40L, seed = s)), 0))
    e_big <- rel(ci(X$data, X$mu, th, crn, "mc", quad, ny = 4000L, seed = 9L))
    expect_lt(e_big, 0.06); expect_lt(e_big, e_small)
    # own draws equal the common path when every respondent gets the shared draws
    own <- list(own = TRUE, M = M, T = 3L, designs_of = function(i) crn$designs$all)
    expect_lt(max(abs(ci(X$data, X$mu, th, own, "exact", quad)$I - ex$I)), 1e-12)
    # symmetric, the right size; halves average to the whole
    r <- ci(X$data, X$mu, th, crn, "exact", quad, halves = TRUE)
    expect_identical(dim(r$I), c(X$n, X$p + X$p * q + 2L, X$p + X$p * q + 2L))
    expect_lt(max(abs((2 * r$IA + r$IB) / 3 - r$I)), 1e-12)
  })
}

test_that("model information equals its expectation over the bank's designs (Monte Carlo)", {
  X <- tiny_q(2L, B = 4L, seed = 41L); quad <- small_rule(2L); p <- X$p
  # the model information draws a design uniformly from the bank (all T = 3 here), u ~ N(0, I_2),
  # v ~ N(0, 1), choices from the model, and averages s s' on the quadrature: its expectation is
  # the bank average of the exact conditional information
  bank <- array(0, c(X$B, X$T, p)); for (j in seq_len(X$B)) for (t in seq_len(X$T)) bank[j, t, ] <- X$DL[[t]][j, ]
  crn <- list(designs = list(all = bank), key = rep(1L, X$B), M = X$B, T = X$T)
  ex <- rq(".rum_cond_info")(X$data, X$MU, X$th, crn, "exact", quad)$I
  mi <- rq(".rum_model_info")(X$data, X$MU, X$th, 30000L, quad, seed = 3L, halves = TRUE)
  expect_identical(dim(mi$I), dim(ex))
  se <- abs(mi$IA - mi$IB) / 2                          # the Monte Carlo SE of the average, roughly
  z <- abs(mi$I - ex) / pmax(se, 1e-3 * max(abs(ex)))
  expect_lt(max(abs(mi$I - ex)) / max(abs(ex)), 0.05)
  expect_lt(stats::quantile(z, 0.95), 4)
  # the one-factor rule is refused for a two-factor state; the default is replaced in the fold routine
  expect_error(rq(".rum_model_info")(X$data, X$MU, X$th, 10L, rum_quadrature(c(5L, 5L))), "taste factors")
})

# ---- rotation directions removed from the information (build plan D3) -----------------------------

test_that("projection: the corrections at Lambda and Lambda A agree up to quadrature error (no canonical rotation)", {
  X <- qf_setup(2L, n = 12L, T = 3L, seed = 11L); p <- X$p; q <- 2L
  fin <- X$fins[[1]]; mu <- rum_dnn_gaussian_forward(fin$network, X$Z[X$data$ids, ])
  set.seed(3); M <- 6L
  crn <- list(designs = list(all = array(sample(c(-1, 0, 1), M * 3 * p, TRUE), c(M, 3, p))),
              key = rep(1L, X$n), M = M, T = 3L)
  tg <- rq(".rum_tgt_share")("share f1-f2", c(1, -0.5, 0)); Q <- rand_orth(2, 5)
  phi_at <- function(L, quad, project = TRUE) {
    th <- list(kappa = fin$kappa, loading = L, omega = fin$omega)
    I <- rq(".rum_cond_info")(X$data, mu, th, crn, "exact", quad)$I
    W <- rq(".rum_weight_parts")(function(i) I[i, , ], X$n, p, p * q + 2L, 1e-6,
                                 if (project) rq(".rum_rotation_dirs")(L) else NULL)
    gr <- rq(".rum_target_grad")(tg, mu, th)
    cw <- rq(".rum_correction")(W, gr$gm, colMeans(gr$gt), seq_len(X$n))
    ev <- rum_dnn_gaussian_evaluate(X$data, mu, th$kappa, L, th$omega, quad)
    list(phi = rowSums(cw$a * ev$output_score) + drop(ev$common_score %*% cw$g), g = cw$g, W = W)
  }
  fine <- rum_quadrature(c(21L, 21L, 15L), n_fac = 2L, rule = "product")
  a <- phi_at(fin$loading, fine); b <- phi_at(fin$loading %*% Q, fine)
  expect_lt(max(abs(a$phi - b$phi)) / max(abs(a$phi)), 1e-6)          # 21 x 21 x 15 rule: ~5e-10
  # Ic annihilates the rotation directions exactly; g has no rotation component
  R <- rq(".rum_rotation_dirs")(fin$loading)
  expect_lt(max(abs(a$W$Ic %*% R)), 1e-12 * max(abs(a$W$Ic)))
  expect_lt(abs(sum(a$g * R)) / sqrt(sum(a$g^2) * sum(R^2)), 1e-12)
  expect_identical(a$W$floored, 0L)
  # on a 512-node Sobol rule the floor alone decides by quadrature error: without the projection
  # the corrections move by a large amount (here > 10% of the largest), with it they do not
  sob <- rum_quadrature(n_fac = 2L, rule = "sobol", nodes = 512L)
  ps <- phi_at(fin$loading, sob); p0 <- phi_at(fin$loading, sob, project = FALSE)
  expect_gt(max(abs(ps$phi - p0$phi)) / max(abs(ps$phi)), 0.1)
  expect_gt(p0$W$floored, 0L); expect_identical(ps$W$floored, 0L)
})

test_that("q=2: one-step estimates and SEs do not depend on the rotation of any fold's loading", {
  X <- qf_setup(2L); TG <- qf_targets(X$p)
  base <- function(k) X$fins[[k]]
  same <- function(k) { f <- X$fins[[k]]; f$loading <- f$loading %*% rand_orth(2, 1); f }
  diff <- function(k) { f <- X$fins[[k]]; f$loading <- f$loading %*% rand_orth(2, 10 + k); f }
  se <- function(r) apply(r$IFP, 2, stats::sd) / sqrt(nrow(r$IFP))
  conds <- list(louis_own = rq(".rum_cond_options")(X$spec, method = "louis", M = 8L, ny = 4L, draws = "own"),
                exact_common = rq(".rum_cond_options")(X$spec, method = "exact", M = 6L))
  runs <- list(constant = list(basis = "constant"), model = list(basis = "model", M = 60L),
               cond_louis_own = list(basis = "conditional", cond = conds$louis_own),
               cond_exact = list(basis = "conditional", cond = conds$exact_common))
  for (nm in names(runs)) {
    rr <- runs[[nm]]
    run <- function(ff) rq(".rum_onestep")(X$data, X$Z, X$fold, ff, TG, info_basis = rr$basis,
                                           model_M = rr$M, cond = rr$cond)
    a <- run(base); b <- run(same); d <- run(diff)
    expect_true(all(is.finite(a$est)) && all(is.finite(se(a))))
    expect_lt(relmax(b$est, a$est), 1e-6); expect_lt(relmax(d$est, a$est), 1e-6)
    expect_lt(relmax(se(b), se(a)), 1e-6); expect_lt(relmax(se(d), se(a)), 1e-6)
    expect_lt(relmax(d$IFP, a$IFP), 1e-6)
    expect_identical(dim(a$fold_loadings), c(3L, X$p, 2L))
    expect_lt(max(abs(d$fold_loadings - a$fold_loadings)), 1e-10)
  }
  # the one-step importance corrections still sum to zero across the shares
  imp <- grep("^imp", names(a$est))
  expect_lt(max(abs(rowSums(a$phi[, imp]))), 1e-8)
})

test_that("q=2: bounded updates (single shares, joint importance) do not depend on the rotation", {
  X <- qf_setup(2L, seed = 13L); TG <- qf_targets(X$p)
  cond <- rq(".rum_cond_options")(X$spec, method = "louis", M = 8L, ny = 4L, draws = "own")
  base <- function(k) X$fins[[k]]
  diff <- function(k) { f <- X$fins[[k]]; f$loading <- f$loading %*% rand_orth(2, 20 + k); f }
  sh <- TG[1:2]; im <- TG[grep("^imp", vapply(TG, `[[`, "", "name"))]
  for (basis in c("constant", "conditional")) {
    tp <- function(ff) rq(".rum_tmle_path")(X$data, X$Z, X$fold, ff, sh, info_basis = basis, cond = cond)
    a <- tp(base); b <- tp(diff)
    expect_true(all(a$converged)); expect_identical(a$status, b$status)
    expect_lt(relmax(b$estimate, a$estimate), 1e-6)
    expect_lt(relmax(b$se_population, a$se_population), 1e-6)
    expect_true(all(a$estimate >= 0 & a$estimate <= 1))
    tj <- function(ff) rq(".rum_tmle_joint")(X$data, X$Z, X$fold, ff, im, info_basis = basis, cond = cond)
    ja <- tj(base); jb <- tj(diff)
    expect_true(ja$converged); expect_identical(ja$status, jb$status)
    expect_lt(relmax(jb$table$estimate, ja$table$estimate), 1e-6)
    expect_lt(relmax(jb$table$se_population, ja$table$se_population), 1e-6)
    expect_lt(abs(sum(ja$table$estimate[ja$table$kind != "total"]) - 1), 1e-12)
  }
})

test_that("q=3: one-step estimates and SEs do not depend on the rotation; parts carry the complement", {
  X <- qf_setup(3L, n = 36L, seed = 17L); TG <- qf_targets(X$p)[c(1, 3, 4, 6)]
  base <- function(k) X$fins[[k]]
  diff <- function(k) { f <- X$fins[[k]]; f$loading <- f$loading %*% rand_orth(3, 30 + k); f }
  cond <- rq(".rum_cond_options")(X$spec, method = "louis", M = 6L, ny = 4L, draws = "own")
  se <- function(r) apply(r$IFP, 2, stats::sd) / sqrt(nrow(r$IFP))
  for (basis in c("constant", "conditional")) {
    run <- function(ff) rq(".rum_onestep")(X$data, X$Z, X$fold, ff, TG, info_basis = basis, cond = cond)
    a <- run(base); b <- run(diff)
    expect_lt(relmax(b$est, a$est), 1e-6); expect_lt(relmax(se(b), se(a)), 1e-6)
  }
  # the weight parts: B is n_th x (n_th - 3), orthogonal to the three rotation directions
  ff <- rq(".rum_fold_fit")(X$data, X$Z, X$fins[[1]], rq(".rum_score_quadrature")())
  expect_identical(rq(".rum_quad_nfac")(ff$quad), 3L)
  W <- rq(".rum_weight_parts")(function(i) crossprod(ff$S) / nrow(ff$S), 1L, X$p, X$p * 3L + 2L, 1e-6, rq(".rum_th_rot")(ff$th))
  R <- rq(".rum_rotation_dirs")(ff$th$loading)
  expect_identical(dim(W$B), c(X$p * 3L + 2L, X$p * 3L + 2L - 3L))
  expect_lt(max(abs(crossprod(W$B, R))), 1e-12)
  expect_lt(max(abs(crossprod(W$B) - diag(ncol(W$B)))), 1e-12)
})

# ---- the common-parameter check (q = 2): rotations are not flagged ---------------------------------

test_that("q=2: a mean confounded with kappa is flagged; the rotation direction is not", {
  set.seed(41); p <- 3L; B <- 400L; T <- 6L; q <- 2L
  DL <- lapply(seq_len(T), function(t) cbind(1, matrix(sample(c(-1, 0, 1), B * 2, TRUE), B, 2)))
  th <- list(kappa = .2, loading = rq(".rum_canonical_loading")(cbind(c(.3, .4, -.2), c(-.1, .2, .35))), omega = .5)
  MU <- matrix(stats::rnorm(B * p, sd = .3), B, p); U <- matrix(stats::rnorm(B * q), B, q); v <- stats::rnorm(B)
  Y <- sapply(seq_len(T), function(t) as.numeric(stats::runif(B) < stats::plogis(th$kappa +
    rowSums(DL[[t]] * (MU + U %*% t(th$loading))) + sqrt(th$omega) * v)))
  quad <- rum_quadrature(c(9L, 9L, 9L), n_fac = 2L, rule = "product")
  S <- rq(".rum_seq_scores")(MU, DL, Y, th, quad)
  expect_lt(max(abs(S[, 1] - S[, p + 1])), 1e-12)            # s_mu1 = s_kappa
  I <- crossprod(S) / B
  tg <- list(rq(".rum_tgt_mean")("mean f1", c(1, 0, 0)), rq(".rum_tgt_mean")("mean f2", c(0, 1, 0)),
             rq(".rum_tgt_share")("share f1 + f2", c(1, 1, 0)), rq(".rum_tgt_mean")("mean f2 - f3", c(0, 1, -1)))
  chk <- rq(".rum_identification_check")
  null <- rq(".rum_nullspace_info")(array(rep(I, each = B), c(B, dim(I))), p)
  new <- chk(tg, MU, th, null, on_fail = "none", info = I)
  expect_identical(new$table$common_confounded, c(TRUE, FALSE, TRUE, FALSE))
  NJ <- new$common$null_J
  expect_identical(ncol(NJ), 1L)                                  # only the kappa direction
  expect_equal(abs(unname(NJ["kappa", 1])), 1, tolerance = 1e-6)
  R <- rq(".rum_rotation_dirs")(th$loading); R <- R / sqrt(sum(R^2))
  expect_lt(abs(sum(NJ[, 1] * R[, 1])), 1e-8)                    # the rotation is not among them
  expect_error(chk(tg, MU, th, null, info = I), "confounded")
  expect_silent(chk(tg[c(2, 4)], MU, th, null, info = I))
  # without the confounding (feature 1 varies) nothing is flagged, rotation included
  DL2 <- lapply(DL, function(d) { d[, 1] <- sample(c(-1, 0, 1), B, TRUE); d })
  I2 <- crossprod(rq(".rum_seq_scores")(MU, DL2, Y, th, quad)) / B
  c2 <- chk(tg, MU, th, null, on_fail = "none", info = I2)
  expect_false(any(c2$table$common_confounded)); expect_identical(ncol(c2$common$null_J), 0L)
  # the fold-level check on the q = 2 fixture flags nothing
  X <- qf_setup(2L, n = 30L, seed = 19L)
  null0 <- list(N = list(matrix(0, X$p, 0)), group = rep(1L, X$n), keys = "all", features = X$spec$features,
                source = "test", tol_rel = 1e-8)
  idf <- rq(".rum_identification_folds")(X$data, X$Z, X$fold, function(k) X$fins[[k]], qf_targets(X$p), null0)
  expect_false(any(idf$table$common_confounded))
  expect_true(all(vapply(idf$null_J, ncol, 0L) == 0L))
})

# ---- the targeted update's path score -----------------------------------------------------------

test_that("q=2: the path score is the derivative of the held-out log likelihood along the path", {
  X <- qf_setup(2L, n = 30L, seed = 23L); n <- X$n; p <- X$p
  model <- rq(".rum_model_options")(NULL, rq(".rum_info_quadrature")(), 1L, NULL, NULL, FALSE, FALSE)
  q1 <- rq(".rum_score_quadrature")()
  FS <- rq(".rum_tmle_folds")(X$data, X$Z, X$fold, function(k) X$fins[[k]], q1, "constant", 1e-2, 1e-6, model)
  TG <- qf_targets(p)
  dirs <- list(rq(".rum_tmle_dirs")(FS, TG[[1]], rep(1, n)), rq(".rum_tmle_dirs")(FS, TG[[6]], rep(1, n)))
  dirs[[1]] <- lapply(dirs[[1]], function(d) { d$g[length(d$g)] <- -0.6; d })
  for (mode in c("log", "clamp")) for (e1 in c(0.3, 1)) {
    eps <- c(e1, -0.2); h <- 1e-6
    r <- rq(".rum_path_all")(FS, dirs, eps, n, p, q1, mode)
    num <- vapply(1:2, function(j) { e <- eps; e[j] <- e[j] + h; a <- rq(".rum_path_all")(FS, dirs, e, n, p, q1, mode)$ll
      e[j] <- eps[j] - h; b <- rq(".rum_path_all")(FS, dirs, e, n, p, q1, mode)$ll; (a - b) / (2 * h) }, 0)
    expect_lt(max(abs(colSums(r$phi) - num)), 1e-5 * max(1, abs(num)))
    expect_true(is.matrix(r$st[[1]]$th$loading) && ncol(r$st[[1]]$th$loading) == 2L)
  }
})

# ---- p = 1, default rules, the exact/Louis rule ---------------------------------------------------

test_that("p = 1 runs (one and two factors; constant, model and conditional bases)", {
  for (q in 1:2) {
    X <- qf_setup(q, n = 30L, p = 1L, seed = 29L + q)
    if (q == 1L) X$fins <- lapply(X$fins, function(f) { f$loading <- as.vector(f$loading); f })
    tg <- list(rq(".rum_tgt_share")("share", 1), rq(".rum_tgt_mean")("mean", 1))
    cond <- rq(".rum_cond_options")(X$spec, method = "louis", M = 6L, ny = 4L, draws = "own")
    for (basis in c("constant", "model", "conditional")) {
      r <- rq(".rum_onestep")(X$data, X$Z, X$fold, function(k) X$fins[[k]], tg, info_basis = basis,
                              model_M = 40L, cond = cond)
      expect_true(all(is.finite(r$est)) && all(is.finite(r$IFP)))
    }
    nz <- rq(".rum_cond_options")(X$spec, method = "exact", M = 4L, null_zero = TRUE)
    r <- rq(".rum_onestep")(X$data, X$Z, X$fold, function(k) X$fins[[k]], tg, info_basis = "conditional", cond = nz)
    expect_true(all(is.finite(r$est)))
  }
})

test_that("default rules: one-factor defaults are replaced at q > 1; method 'auto' picks exact or Louis", {
  sq <- rq(".rum_quad_for")(rq(".rum_score_quadrature")(), 2L, "score")
  expect_identical(c(rq(".rum_quad_nfac")(sq), length(sq$weight)), c(2L, 2048L))
  iq <- rq(".rum_quad_for")(rq(".rum_info_quadrature")(), 3L, "info")
  expect_identical(c(rq(".rum_quad_nfac")(iq), length(iq$weight)), c(3L, 512L))
  expect_identical(rq(".rum_quad_for")(rq(".rum_score_quadrature")(), 1L), rq(".rum_score_quadrature")())
  expect_error(rq(".rum_quad_for")(rum_quadrature(c(5L, 5L)), 2L, "score"), "taste factor")
  m <- rq(".rum_cond_method")
  expect_identical(m("auto", 13L, 512L, 1L), "exact")              # one factor: as before
  expect_identical(m("auto", 8L, 512L, 2L), "exact")               # Ballard-Rosa
  expect_identical(m("auto", 8L, 2048L, 2L), "exact")
  expect_identical(m("auto", 13L, 512L, 2L), "louis")              # Graham-Svolik
  expect_identical(m("auto", 3L, 512L, 3L), "exact")               # Saha-Weeks
  expect_identical(m("louis", 3L, 512L, 1L), "louis")
  op <- rq(".rum_cond_options")("br")
  expect_identical(op$method, "auto"); expect_true(is.na(op$M))
  expect_identical(rq(".rum_cond_options")("br", method = "exact")$M, 200L)
  # at one factor "auto" gives exactly the old default (exact, M = 200, same cache key and numbers)
  fx <- readRDS(test_path("fixtures", "rum-inference-fixture.rds"))
  set.seed(7L); M <- 4L; T <- max(fx$data$T); p <- fx$data$p
  crn <- list(designs = list(all = array(sample(c(-1, 0, 1), M * T * p, TRUE), c(M, T, p))),
              key = rep(1L, fx$data$n), M = M, T = T)
  qd <- rum_quadrature(c(7L, 5L)); cache <- file.path(tempdir(), "rum-qf-auto")
  ff <- rq(".rum_fold_fit")(fx$data, fx$Z, fx$fins[[1]], rq(".rum_score_quadrature")())
  f1 <- rq(".rum_cond_info_fold")(fx$data, ff$mu, ff$th, 1L, rq(".rum_cond_options")(NULL, crn = crn, quad = qd,
          cache_dir = cache, cache_net = "a"), fx$fins[[1]])
  f2 <- rq(".rum_cond_info_fold")(fx$data, ff$mu, ff$th, 1L, rq(".rum_cond_options")(NULL, method = "exact", crn = crn,
          quad = qd, cache_dir = cache, cache_net = "a", cache_require = TRUE), fx$fins[[1]])
  expect_identical(f1$I, f2$I); expect_true(f2$hit)
})
