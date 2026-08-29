#!/usr/bin/env Rscript
## Self-contained tests for applications/R/ame_dml.R.
##   Rscript applications/R/test-ame-dml.R
## Prints PASS/FAIL per check; exits non-zero if anything fails.
##
## The load-bearing check is theory consistency with paperps: with a fixed
## draw set the AME target is a fixed finite linear combination of the
## manuscript's choice-probability functionals, so running it through
## scmix_dml() must reproduce, to numerical precision, the same linear
## combination of the package's OWN typed "choice" targets --- plug-in
## value, one-step estimate, and standard error from the joint diagnostic
## covariance. That check exercises the whole inherited machinery
## (cross-fitting, Riesz correction, influence assembly) rather than only
## the kernel.

options(stringsAsFactors = FALSE, warn = 1)
set.seed(20260827L)

root <- path.expand("~/GitHub/sconjoint")
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/ame_dml.R"))
source(file.path(root, "applications/R/estimands_v21.R"))

## --------------------------------------------------------------------
## Harness
## --------------------------------------------------------------------
.n_pass <- 0L; .n_fail <- 0L
ok <- function(label, passed, detail = "") {
  passed <- isTRUE(passed)   # force before detail: detail may use objects
  if (length(detail) != 1L) detail <- "(empty detail)"   # assigned in passed
  if (passed) {
    .n_pass <<- .n_pass + 1L
    cat(sprintf("PASS  %-62s %s\n", label, detail))
  } else {
    .n_fail <<- .n_fail + 1L
    cat(sprintf("FAIL  %-62s %s\n", label, detail))
  }
  invisible(passed)
}
section <- function(s) cat("\n## ", s, "\n", sep = "")

## Mock fit in the package DML tests' own shape (p = 2 coordinates).
mock_fit <- function(N = 120L, T = 4L, K = 3L, q = 1L, seed = 481L) {
  set.seed(seed)
  p <- 2L
  Z_resp <- matrix(seq(-1, 1, length.out = N), ncol = 1L,
                   dimnames = list(NULL, "z"))
  respondent_id <- rep(sprintf("r%03d", seq_len(N)), each = T)
  respondent_index <- rep(seq_len(N), each = T)
  fold_resp <- rep(seq_len(K), length.out = N)
  fold_id <- rep(fold_resp, each = T)
  deltaX <- matrix(stats::rnorm(N * T * p), ncol = p,
                   dimnames = list(NULL, c("x1", "x2")))
  mu <- cbind(0.2 + 0.3 * Z_resp[, 1L], -0.1 + 0.2 * Z_resp[, 1L])
  kappa <- 0.15
  index <- kappa + rowSums(deltaX * mu[respondent_index, , drop = FALSE])
  y <- stats::rbinom(length(index), 1L, stats::plogis(index))
  if (q == 0L) {
    A_folds <- replicate(K, matrix(numeric(0), p, 0L), simplify = FALSE)
    A_hat <- matrix(numeric(0), p, 0L)
    gh <- list(U = matrix(numeric(0), 1L, 0L), w = 1)
  } else {
    A_folds <- replicate(K, matrix(c(0.35, -0.2), p, 1L), simplify = FALSE)
    A_hat <- matrix(c(0.35, -0.2), p, 1L)
    gh <- list(U = matrix(c(-1, 0, 1), ncol = 1L), w = c(0.25, 0.5, 0.25))
  }
  list(deltaX = deltaX, y = y, respondent_id = respondent_id,
       fold_id = fold_id, K = K,
       Z = Z_resp[respondent_index, , drop = FALSE],
       q = q, A_folds = A_folds, A_hat = A_hat, gh = gh,
       kappa_folds = rep(kappa, K),
       mu_all_folds = replicate(K, mu, simplify = FALSE),
       attr_names = colnames(deltaX),
       analysis_signature = paste("mock", N, T, K, q, seed, sep = "-"))
}

## --------------------------------------------------------------------
## 1. Theory consistency: AME target == linear combination of the
##    package's typed choice targets, through the SAME scmix_dml call.
## --------------------------------------------------------------------
section("Theory consistency with the typed choice targets (paperps machinery)")

for (M in c(1L, 3L)) {
  for (pn in c(FALSE, TRUE)) {
    set.seed(1000L + M + 10L * pn)
    fit <- mock_fit(q = 1L)
    p <- 2L
    d_focal <- matrix(stats::rnorm(M * p, sd = 0.8), M, p)
    d_ref <- matrix(stats::rnorm(M * p, sd = 0.8), M, p)
    ame <- ame_dml_target(d_focal, d_ref, n_nodes = 31L,
                          position_neutral = pn, chunk = 2L,
                          label = "ame_test")
    typed <- list()
    for (m in seq_len(M)) {
      typed[[paste0("f", m)]] <- sconjoint:::scmix_inference_target(
        "choice", contrast = d_focal[m, ], position_neutral = pn,
        n_nodes = 31L, label = paste0("f", m))
      typed[[paste0("r", m)]] <- sconjoint:::scmix_inference_target(
        "choice", contrast = d_ref[m, ], position_neutral = pn,
        n_nodes = 31L, label = paste0("r", m))
    }
    inf <- sconjoint:::scmix_dml(
      fit, targets = character(0),
      plugin_targets = c(list(ame_test = ame), typed),
      nu_grid = 0, multiplier_draws = 0L)
    est <- inf$estimate
    lab <- names(est)
    stopifnot(!is.null(lab))
    cvec <- stats::setNames(numeric(length(est)), lab)
    cvec[paste0("f", seq_len(M))] <- 1 / M
    cvec[paste0("r", seq_len(M))] <- -1 / M
    combo_est <- sum(cvec * est)
    plug <- inf$plugin_estimate
    combo_plug <- sum(cvec * plug)
    V <- as.matrix(inf$diagnostic_covariance)
    i_ame <- match("ame_test", lab)
    combo_se <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
    ame_se <- sqrt(V[i_ame, i_ame])
    tagm <- sprintf("[M=%d, pn=%s]", M, pn)
    ok(paste("plug-in equals choice combination", tagm),
       abs(plug[[i_ame]] - combo_plug) < 1e-10,
       sprintf("dev %.2e", abs(plug[[i_ame]] - combo_plug)))
    ok(paste("one-step equals choice combination", tagm),
       abs(est[[i_ame]] - combo_est) < 1e-8,
       sprintf("dev %.2e", abs(est[[i_ame]] - combo_est)))
    ok(paste("se equals combination se", tagm),
       abs(ame_se - combo_se) < 1e-8 * max(1, combo_se),
       sprintf("ame %.6e combo %.6e", ame_se, combo_se))
    cross_dev <- max(abs(V[i_ame, -i_ame] -
                           as.numeric(t(cvec) %*% V)[-i_ame]))
    ok(paste("cross-covariances match the combination", tagm),
       cross_dev < 1e-8, sprintf("max dev %.2e", cross_dev))
  }
}

## --------------------------------------------------------------------
## 2. Analytic derivatives vs central finite differences (kernel only)
## --------------------------------------------------------------------
section("Analytic derivatives vs finite differences")

fd_max_dev <- function(cb, mu, kappa, Sigma, what, eps = 1e-5,
                       n_probe = 6L) {
  base <- cb(mu, kappa, Sigma, NULL, NULL, NULL, colnames(mu))
  N <- nrow(mu); p <- ncol(mu)
  devs <- numeric(0)
  if (what == "kappa") {
    up <- cb(mu, kappa + eps, Sigma, NULL, NULL, NULL, colnames(mu))
    dn <- cb(mu, kappa - eps, Sigma, NULL, NULL, NULL, colnames(mu))
    fd <- (up$value - dn$value) / (2 * eps)
    devs <- abs(fd - base$d_kappa)
  } else if (what == "mu") {
    for (t in seq_len(n_probe)) {
      i <- sample.int(N, 1L); k <- sample.int(p, 1L)
      mu_up <- mu; mu_up[i, k] <- mu[i, k] + eps
      mu_dn <- mu; mu_dn[i, k] <- mu[i, k] - eps
      up <- cb(mu_up, kappa, Sigma, NULL, NULL, NULL, colnames(mu))
      dn <- cb(mu_dn, kappa, Sigma, NULL, NULL, NULL, colnames(mu))
      fd <- (up$value[i, 1L] - dn$value[i, 1L]) / (2 * eps)
      devs <- c(devs, abs(fd - base$d_mu[i, 1L, k]))
    }
  } else if (what == "Sigma") {
    for (t in seq_len(n_probe)) {
      a <- sample.int(p, 1L); b <- sample.int(p, 1L)
      P <- matrix(0, p, p); P[a, b] <- P[a, b] + 1; P[b, a] <- P[b, a] + 1
      up <- cb(mu, kappa, Sigma + eps * P, NULL, NULL, NULL, colnames(mu))
      dn <- cb(mu, kappa, Sigma - eps * P, NULL, NULL, NULL, colnames(mu))
      fd <- (up$value - dn$value) / (2 * eps)
      want <- base$d_Sigma[, 1L, a, b] + base$d_Sigma[, 1L, b, a]
      devs <- c(devs, abs(fd - want))
    }
  }
  max(devs)
}

set.seed(7)
p <- 4L; N <- 40L; M <- 9L
mu <- matrix(stats::rnorm(N * p, sd = 0.6), N, p,
             dimnames = list(NULL, paste0("c", 1:p)))
A2 <- matrix(stats::rnorm(p * 2L, sd = 0.4), p, 2L)
Sigma <- tcrossprod(A2) + diag(0.05, p)       # strictly PD: FD-safe
d_f <- matrix(stats::rnorm(M * p), M, p)
d_r <- matrix(stats::rnorm(M * p), M, p)
for (pn in c(FALSE, TRUE)) {
  cb <- ame_dml_target(d_f, d_r, n_nodes = 31L, position_neutral = pn,
                       chunk = 4L)
  tg <- sprintf("[pn=%s]", pn)
  ok(paste("d_kappa matches FD", tg),
     (v <- fd_max_dev(cb, mu, 0.15, Sigma, "kappa")) < 1e-7,
     sprintf("max dev %.2e", v))
  ok(paste("d_mu matches FD", tg),
     (v <- fd_max_dev(cb, mu, 0.15, Sigma, "mu")) < 1e-7,
     sprintf("max dev %.2e", v))
  ok(paste("d_Sigma matches FD", tg),
     (v <- fd_max_dev(cb, mu, 0.15, Sigma, "Sigma")) < 1e-6,
     sprintf("max dev %.2e", v))
}

## --------------------------------------------------------------------
## 3. Zero-variance branch: continuity and the Stein limit
## --------------------------------------------------------------------
section("Zero-variance branch")

a_load <- c(1, 0, 0, 0)
Sig0 <- tcrossprod(a_load)                     # rank one
d_perp <- matrix(c(0, 1, -1, 0.5), 1L)         # exactly d' Sigma d = 0
d_gen <- matrix(stats::rnorm(4), 1L)
cb0 <- ame_dml_target(rbind(d_perp, d_gen), rbind(d_gen, d_perp),
                      n_nodes = 31L, chunk = 1L)
res0 <- cb0(mu, 0.15, Sig0, NULL, NULL, NULL, colnames(mu))
## AUDIT A2. This perturbation is deliberately SUB-THRESHOLD: the induced
## directional variance is d' Sig_eps d = 1e-12 * 3.25 = 3.25e-12, well
## below .ame_v_eps = 1e-10, so BOTH calls take the zero-variance branch
## and nothing crosses. The test therefore pins branch STABILITY under a
## sub-threshold nudge, not continuity across the switch; the Stein-limit
## check immediately below is the one that genuinely crosses.
v_induced <- as.numeric(d_perp %*% (1e-12 * tcrossprod(
  c(0, 1, -1, 0.5) / sqrt(2.25 + 1))) %*% t(d_perp))
ok("branch-switch fixture is sub-threshold (documents the label)",
   v_induced < .ame_v_eps,
   sprintf("induced variance %.2e < .ame_v_eps %.0e", v_induced, .ame_v_eps))
Sig_eps <- Sig0 + 1e-12 * tcrossprod(c(0, 1, -1, 0.5) / sqrt(2.25 + 1))
res_eps <- cb0(mu, 0.15, Sig_eps, NULL, NULL, NULL, colnames(mu))
ok("value stable under a sub-threshold variance nudge",
   max(abs(res0$value - res_eps$value)) < 1e-6,
   sprintf("max dev %.2e", max(abs(res0$value - res_eps$value))))
## And a nudge that DOES cross: the value must still move only by O(s).
Sig_cross <- Sig0 + 1e-6 * tcrossprod(c(0, 1, -1, 0.5) / sqrt(2.25 + 1))
v_cross <- as.numeric(d_perp %*% (1e-6 * tcrossprod(
  c(0, 1, -1, 0.5) / sqrt(2.25 + 1))) %*% t(d_perp))
res_cross <- cb0(mu, 0.15, Sig_cross, NULL, NULL, NULL, colnames(mu))
ok("value continuous ACROSS the branch switch (super-threshold nudge)",
   v_cross > .ame_v_eps &&
     max(abs(res0$value - res_cross$value)) < 1e-6,
   sprintf("induced variance %.2e crosses; max dev %.2e",
           v_cross, max(abs(res0$value - res_cross$value))))
## Stein limit against a tiny positive variance evaluated by fine GH.
s_small <- 1e-4
gh_fine <- ame_gh_nodes(201L)
m1 <- as.numeric(mu %*% d_perp[1L, ])
dv_num <- vapply(seq_len(N), function(i) {
  vjs <- 0.15 + m1[i] + s_small * gh_fine$x
  P <- stats::plogis(vjs); S <- P * (1 - P)
  sum(gh_fine$w * S * gh_fine$x / (2 * s_small))
}, numeric(1L))
P0 <- stats::plogis(0.15 + m1); S0v <- P0 * (1 - P0)
dv_stein <- 0.5 * S0v * (1 - 2 * P0)
ok("Stein limit matches small-s positive branch",
   max(abs(dv_num - dv_stein)) < 1e-6,
   sprintf("max dev %.2e", max(abs(dv_num - dv_stein))))

## --------------------------------------------------------------------
## 4. Value oracles
## --------------------------------------------------------------------
section("Value oracles")

## Quadrature accuracy: 31 nodes vs 201 nodes.
cb31 <- ame_dml_target(d_f, d_r, n_nodes = 31L, chunk = 3L)
cb201 <- ame_dml_target(d_f, d_r, n_nodes = 201L, chunk = 3L)
v31 <- cb31(mu, 0.15, Sigma, NULL, NULL, NULL, colnames(mu))$value
v201 <- cb201(mu, 0.15, Sigma, NULL, NULL, NULL, colnames(mu))$value
ok("31-node quadrature agrees with 201-node oracle",
   max(abs(v31 - v201)) < 1e-9,
   sprintf("max dev %.2e", max(abs(v31 - v201))))

## Plain Monte Carlo oracle on one draw pair and a respondent subset.
set.seed(99)
R <- 400000L
u <- stats::rnorm(R)
A1 <- t(chol(Sigma))                            # any factor: value uses Sigma
idx <- 1:8
mc_dev <- vapply(idx, function(i) {
  ld_f <- as.numeric(crossprod(d_f[1L, ], Sigma) %*% d_f[1L, ])
  ld_r <- as.numeric(crossprod(d_r[1L, ], Sigma) %*% d_r[1L, ])
  vf <- 0.15 + sum(d_f[1L, ] * mu[i, ]) + sqrt(ld_f) * u
  vr <- 0.15 + sum(d_r[1L, ] * mu[i, ]) + sqrt(ld_r) * u
  mean(stats::plogis(vf)) - mean(stats::plogis(vr))
}, numeric(1L))
cb_one <- ame_dml_target(d_f[1L, , drop = FALSE], d_r[1L, , drop = FALSE],
                         n_nodes = 31L)
v_one <- cb_one(mu, 0.15, Sigma, NULL, NULL, NULL, colnames(mu))$value[idx, 1L]
ok("value agrees with simulation oracle (4 mc-se)",
   max(abs(v_one - mc_dev)) < 4 * 0.5 / sqrt(R) * 2,
   sprintf("max dev %.2e", max(abs(v_one - mc_dev))))

## AME(a, a): identical draw pairs give exact zeros everywhere.
cb_zero <- ame_dml_target(d_f, d_f, n_nodes = 31L, chunk = 2L)
rz <- cb_zero(mu, 0.15, Sigma, NULL, NULL, NULL, colnames(mu))
ok("AME(a, a) value is exactly zero", all(rz$value == 0))
ok("AME(a, a) derivatives are exactly zero",
   all(rz$d_mu == 0) && all(rz$d_kappa == 0) && all(rz$d_Sigma == 0))

## Chunk-size invariance.
cA <- ame_dml_target(d_f, d_r, chunk = 1L)(mu, 0.15, Sigma, NULL, NULL,
                                           NULL, colnames(mu))
cB <- ame_dml_target(d_f, d_r, chunk = 9L)(mu, 0.15, Sigma, NULL, NULL,
                                           NULL, colnames(mu))
dev_chunk <- max(abs(cA$value - cB$value), abs(cA$d_mu - cB$d_mu),
                 abs(cA$d_kappa - cB$d_kappa),
                 abs(cA$d_Sigma - cB$d_Sigma))
ok("chunk-size invariance", dev_chunk < 1e-13,
   sprintf("max dev %.2e", dev_chunk))

## Position-neutral orientation identity: invariant to kappa -> -kappa.
cb_pn <- ame_dml_target(d_f, d_r, position_neutral = TRUE, chunk = 4L)
pn_a <- cb_pn(mu, 0.15, Sigma, NULL, NULL, NULL, colnames(mu))$value
pn_b <- cb_pn(mu, -0.15, Sigma, NULL, NULL, NULL, colnames(mu))$value
ok("position-neutral value invariant to kappa sign",
   max(abs(pn_a - pn_b)) < 1e-14,
   sprintf("max dev %.2e", max(abs(pn_a - pn_b))))

## --------------------------------------------------------------------
## 5. Draw construction: parity with est_AME on a synthetic fit
## --------------------------------------------------------------------
section("Draw parity with est_AME")

set.seed(11)
p5 <- 5L
coord5 <- c("attr1_l1", "attr1_l2", "attr2_l1", "attr3_l1", "attr3_l2")
attrs5 <- list(attr1 = coord5[1:2], attr2 = coord5[3], attr3 = coord5[4:5])
fit5 <- list(mu = matrix(stats::rnorm(60L * p5, sd = 0.5), 60L, p5),
             A = matrix(stats::rnorm(p5, sd = 0.3), p5, 1L),
             kappa = 0.1, coord = coord5, n = 60L)
M5 <- 4000L
ame_pi <- est_AME(fit5, attrs5, n_nodes = 31L, M_D = M5, seed = 3L)
tg5 <- ame_dml_targets(coord5, attrs5, M_D = M5, seed = 3L, n_nodes = 31L,
                       position_neutral = FALSE, chunk = 512L)
Sigma5 <- tcrossprod(fit5$A)
vals5 <- vapply(names(tg5$targets), function(nm) {
  mean(tg5$targets[[nm]](fit5$mu, fit5$kappa, Sigma5, NULL, NULL, NULL,
                         coord5)$value)
}, numeric(1L))
names(vals5) <- sub("^ame_fixed_draw:", "", names(vals5))
cmp <- ame_pi$value
dev5 <- abs(vals5[cmp$coordinate] - cmp$ame)
tol5 <- 4 * cmp$mc_se
ok("rowwise design average matches est_AME within 4 mc-se",
   all(dev5 < tol5),
   sprintf("max dev %.2e, min headroom %.2f", max(dev5),
           min(tol5 / pmax(dev5, 1e-16))))

## --------------------------------------------------------------------
## 6. scmix_dml end-to-end on mocks (q = 1 and q = 0)
## --------------------------------------------------------------------
section("End-to-end scmix_dml runs")

for (qq in c(1L, 0L)) {
  fitq <- mock_fit(q = qq)
  set.seed(5)
  d_f2 <- matrix(stats::rnorm(6L * 2L), 6L, 2L)
  d_r2 <- matrix(stats::rnorm(6L * 2L), 6L, 2L)
  cbq <- ame_dml_target(d_f2, d_r2, position_neutral = TRUE, chunk = 3L,
                        label = "ame_q")
  infq <- sconjoint:::scmix_dml(fitq, targets = character(0),
                                plugin_targets = list(ame_q = cbq),
                                nu_grid = 0, multiplier_draws = 0L)
  se_q <- sqrt(diag(as.matrix(infq$diagnostic_covariance)))[1L]
  ok(sprintf("q=%d run: analytic derivatives, finite diagnostic se", qq),
     all(infq$derivative_source == "analytic") &&
       is.finite(infq$estimate[[1L]]) && is.finite(se_q) && se_q > 0,
     sprintf("est %.4f se %.4f status %s", infq$estimate[[1L]], se_q,
             infq$status))
}

## --------------------------------------------------------------------
## 7. Injected-defect canaries: the checks must catch a broken kernel
## --------------------------------------------------------------------
section("Injected-defect canaries")

corrupt <- function(cb, field, factor = 1.01) {
  function(mu, kappa, Sigma, Z, respondent_id, fold, attr_names) {
    out <- cb(mu, kappa, Sigma, Z, respondent_id, fold, attr_names)
    out[[field]] <- out[[field]] * factor
    out
  }
}
cb_good <- ame_dml_target(d_f, d_r, chunk = 4L)
ok("canary: 1% d_kappa corruption is caught by the FD check",
   fd_max_dev(corrupt(cb_good, "d_kappa"), mu, 0.15, Sigma, "kappa") > 1e-5)
ok("canary: 1% d_mu corruption is caught by the FD check",
   fd_max_dev(corrupt(cb_good, "d_mu"), mu, 0.15, Sigma, "mu") > 1e-5)
ok("canary: 1% d_Sigma corruption is caught by the FD check",
   fd_max_dev(corrupt(cb_good, "d_Sigma"), mu, 0.15, Sigma, "Sigma") > 1e-6)
## A wrong 1/(2s) convention in dv is a factor-of-2 error: far above the
## FD tolerance. Emulate by doubling d_Sigma.
ok("canary: dropped 1/(2s) (factor-2 dv) is caught",
   fd_max_dev(corrupt(cb_good, "d_Sigma", 2), mu, 0.15, Sigma,
              "Sigma") > 1e-4)
## Sign flip on the reference side: value moves by O(1) against the
## 201-node oracle rebuilt with the correct sides.
cb_flip <- ame_dml_target(d_r, d_f, chunk = 4L)   # sides swapped
vflip <- cb_flip(mu, 0.15, Sigma, NULL, NULL, NULL, colnames(mu))$value
ok("canary: swapped focal/reference sides is caught by the oracle",
   max(abs(vflip - v201)) > 1e-3)

## --------------------------------------------------------------------
## 6. Target identity and provenance (audit work package A1/A2)
## --------------------------------------------------------------------
section("Fixed-draw target: identity, provenance, labelling")

attrs_t <- list(gender = "c1", agenda = c("c2", "c3"))
coord_t <- paste0("c", 1:4)

## A1 ACCEPTANCE. Two runs against the same frozen draw file must return
## BITWISE-identical Psi_M. The draws are the only randomness; the
## callback is deterministic in (mu, kappa, Sigma).
dr1 <- ame_design_draws(coord_t, attrs_t, M_D = 500L, seed = 4242L)
dr2 <- ame_design_draws(coord_t, attrs_t, M_D = 500L, seed = 4242L)
ok("frozen draw set: identical seed gives an identical draw hash",
   identical(dr1$draw_hash, dr2$draw_hash) && !is.na(dr1$draw_hash),
   substr(dr1$draw_hash, 1L, 16L))
pair1 <- ame_contrast_pair(dr1, "agenda", dr1$coord_index$agenda[[1L]])
pair2 <- ame_contrast_pair(dr2, "agenda", dr2$coord_index$agenda[[1L]])
cb_a <- ame_dml_target(pair1$d_focal, pair1$d_ref, n_nodes = 31L,
                       chunk = 128L, label = "psi_M")
cb_b <- ame_dml_target(pair2$d_focal, pair2$d_ref, n_nodes = 31L,
                       chunk = 128L, label = "psi_M")
mu_t <- matrix(stats::rnorm(30L * 4L, sd = 0.5), 30L, 4L,
               dimnames = list(NULL, coord_t))
Sig_t <- crossprod(matrix(stats::rnorm(16L, sd = 0.3), 4L, 4L)) +
  diag(0.05, 4L)
psi_a <- cb_a(mu_t, 0.1, Sig_t, NULL, NULL, NULL, coord_t)$value
psi_b <- cb_b(mu_t, 0.1, Sig_t, NULL, NULL, NULL, coord_t)$value
ok("frozen draw set: two runs return BITWISE-identical Psi_M",
   identical(psi_a, psi_b),
   sprintf("mean Psi_M = %.12f, identical() = TRUE", mean(psi_a)))
## A different seed is a different draw set, hence a different Psi_M.
dr3 <- ame_design_draws(coord_t, attrs_t, M_D = 500L, seed = 4243L)
pair3 <- ame_contrast_pair(dr3, "agenda", dr3$coord_index$agenda[[1L]])
psi_c <- ame_dml_target(pair3$d_focal, pair3$d_ref, n_nodes = 31L,
                        chunk = 128L)(mu_t, 0.1, Sig_t, NULL, NULL, NULL,
                                      coord_t)$value
ok("a different draw seed gives a different Psi_M (targets are distinct)",
   !identical(dr1$draw_hash, dr3$draw_hash) && !identical(psi_a, psi_c),
   sprintf("|Psi_M(seed 4242) - Psi_M(seed 4243)| = %.3e",
           mean(abs(psi_a - psi_c))))

## Provenance travels with the draw set and names the target explicitly.
spec <- ame_draw_spec(dr1)
ok("draw spec names the fixed-draw target, not the exact integral",
   identical(spec$target, "ame_fixed_draw") &&
     grepl("Psi_M", spec$target_definition, fixed = TRUE) &&
     grepl("do not cover the exact design integral",
           spec$integration_contract, fixed = TRUE),
   spec$target)
ok("draw spec carries M, seed, hash and the design law",
   identical(spec$M, 500L) && identical(spec$seed, 4242L) &&
     nzchar(spec$draw_hash) && nzchar(spec$design_law),
   sprintf("M = %d, seed = %d", spec$M, spec$seed))

built_t <- ame_dml_targets(coord_t, attrs_t, M_D = 200L, seed = 5L,
                           position_neutral = TRUE, chunk = 64L)
ok("target labels are prefixed ame_fixed_draw, never a bare 'ame'",
   all(grepl("^ame_fixed_draw_neutral:", names(built_t$targets))),
   names(built_t$targets)[[1L]])

## A2. Position-neutral is an ORIENTATION-AVERAGED structural AME. It is
## comparable only with a design AMCE averaged over the same orientations:
## against a single-orientation target it differs whenever kappa != 0, and
## coincides exactly when kappa = 0.
cb_or <- ame_dml_target(pair1$d_focal, pair1$d_ref, n_nodes = 31L,
                        position_neutral = FALSE, chunk = 128L)
cb_pn <- ame_dml_target(pair1$d_focal, pair1$d_ref, n_nodes = 31L,
                        position_neutral = TRUE, chunk = 128L)
v_or_k <- cb_or(mu_t, 0.4, Sig_t, NULL, NULL, NULL, coord_t)$value
v_pn_k <- cb_pn(mu_t, 0.4, Sig_t, NULL, NULL, NULL, coord_t)$value
v_or_0 <- cb_or(mu_t, 0, Sig_t, NULL, NULL, NULL, coord_t)$value
v_pn_0 <- cb_pn(mu_t, 0, Sig_t, NULL, NULL, NULL, coord_t)$value
ok("position-neutral differs from one orientation when kappa != 0",
   max(abs(v_or_k - v_pn_k)) > 1e-6,
   sprintf("max dev at kappa = 0.4 is %.3e", max(abs(v_or_k - v_pn_k))))
ok("position-neutral equals one orientation exactly when kappa = 0",
   max(abs(v_or_0 - v_pn_0)) < 1e-12,
   sprintf("max dev at kappa = 0 is %.3e", max(abs(v_or_0 - v_pn_0))))
## And it is the exact average of the two display orientations.
cb_flip_or <- ame_dml_target(pair1$d_focal, pair1$d_ref, n_nodes = 31L,
                             position_neutral = FALSE, chunk = 128L)
v_minus <- cb_flip_or(mu_t, -0.4, Sig_t, NULL, NULL, NULL, coord_t)$value
ok("position-neutral is the mean of the +kappa and -kappa orientations",
   max(abs(0.5 * (v_or_k + v_minus) - v_pn_k)) < 1e-12,
   sprintf("max dev %.3e", max(abs(0.5 * (v_or_k + v_minus) - v_pn_k))))

## A2. Joint respondent/design sampling and the Cartesian product share a
## conditional expectation but not a Monte Carlo standard error: the joint
## estimator carries respondent-resampling noise the product form does not.
## Compare on the SAME draw set at the same parameters.
prod_val <- ame_design_mc_se(mu_t, 0.1, Sig_t, pair1$d_focal, pair1$d_ref,
                             n_nodes = 31L, chunk = 128L)
joint_one <- function(seed) {
  set.seed(seed)
  ii <- sample.int(nrow(mu_t), nrow(pair1$d_focal), replace = TRUE)
  gh <- ame_gh_nodes(31L)
  side <- function(D) {
    m <- rowSums(D * mu_t[ii, , drop = FALSE])
    v <- pmax(rowSums((D %*% Sig_t) * D), 0)
    acc <- 0
    for (j in seq_along(gh$x))
      acc <- acc + gh$w[j] * stats::plogis(0.1 + m + sqrt(v) * gh$x[j])
    acc
  }
  d <- side(pair1$d_focal) - side(pair1$d_ref)
  c(est = mean(d), se = stats::sd(d) / sqrt(length(d)))
}
joint <- vapply(1:40, joint_one, numeric(2L))
joint_mean <- mean(joint["est", ])
joint_se_mean <- mean(joint["se", ])
ok("joint sampling and the Cartesian product share a conditional mean",
   abs(joint_mean - prod_val$value) <
     4 * stats::sd(joint["est", ]) / sqrt(ncol(joint)),
   sprintf("joint %.6f vs product %.6f (4 se of the joint mean = %.6f)",
           joint_mean, prod_val$value,
           4 * stats::sd(joint["est", ]) / sqrt(ncol(joint))))
ok("their Monte Carlo standard errors differ (joint adds respondent noise)",
   joint_se_mean > 3 * prod_val$mc_se,
   sprintf("joint mc_se %.3e vs product design mc_se %.3e (ratio %.1f)",
           joint_se_mean, prod_val$mc_se, joint_se_mean / prod_val$mc_se))

## --------------------------------------------------------------------
cat(sprintf("\n%d passed, %d failed\n", .n_pass, .n_fail))
if (.n_fail > 0L) quit(status = 1L)
