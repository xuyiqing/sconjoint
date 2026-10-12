# Targeted (bounded) updates for quantities in [0, 1]: the scalar update with omega on the
# log scale and an explicit failure status, and ONE joint update for a set of shares that sum
# to one (Avi's memo of 2026-10-08, items 4 and 7; paperps1011.tex Appendix D, "Bounded
# Targeting for Shares", and L2888-2890 on reparametrizing omega).
#
# What was wrong with .rum_tmle_dnn() (kept unchanged for parity):
#  (a) omega moved on a linear path, omega + eps g_omega, and was floored at 1e-8. When the
#      floor binds the fitted state stops moving in omega, but the score along the path
#      still uses the unfloored direction g_omega, so the "score equation" solved is not the
#      derivative of the path actually taken.
#  (b) after max_iter = 20 Fisher-scoring steps it returned the last iterate even when the
#      stopping rule had not been met, with only a converged = FALSE column.
#  (c) for importance shares, each share was targeted separately with its own eps; the
#      updated shares then come from different fitted states and their total is not 1
#      (Phase C: 103.5% in GS, up to 101.5% in SW). The plain one-step corrections already
#      sum to zero across the shares (the derivatives of shares that sum to one sum to zero,
#      and the correction is linear in the derivative), so the excess comes from (c) alone.
#
# The fix.
#  * Path: mu_eps(z) = mu(z) + sum_j eps_j a_j(z); kappa and loading move linearly,
#    kappa + sum_j eps_j g_j[kappa], ...; omega moves on the log scale,
#      log omega_eps = log omega + sum_j eps_j g_j[omega] / omega,
#    which has the same tangent at eps = 0 (d omega / d eps = g_omega), so the first-order
#    correction is unchanged, and omega_eps > 0 for every eps: no floor. The chain rule is
#    applied to the score along the path: d loglik / d eps_j = a_j' s_mu + g_j[-omega]'
#    s_(kappa, loading) + (omega_eps / omega) g_j[omega] s_omega. (The correction weights
#    themselves are invariant to the reparametrization: in log-omega coordinates the weight
#    is g_omega / omega and the score omega s_omega, the same product.) Targets that depend
#    on omega are evaluated at omega_eps, so their derivatives follow the same path.
#  * Step: Fisher scoring on the held-out log likelihood along the path, eps <- eps + t
#    I^{-1} S with S = sum_i phi_i(eps), I = sum_i phi_i phi_i'; t is halved until the
#    held-out log likelihood does not decrease (at most max_halving times).
#  * Convergence: sqrt(S' I^{-1} S) < tol (for one direction: |sum phi| / sqrt(sum phi^2)
#    < tol, the old rule; tol = 1e-3 as before).
#  * Failure: status "max_iter" (no convergence in max_iter steps), "step_failed" (no
#    ascent after max_halving halvings) or "nonfinite". A failed target gets estimate NA and
#    no interval; nothing is returned as if it had converged. Failures are counted.
#  * Joint update for G shares on the simplex (same population weights): the path uses the
#    correction directions of G - 1 of them (the G-th is minus their sum), with one eps
#    vector of length G - 1 shared by all folds, solving all G score equations at once.
#    The estimate of every share is the plug-in at the same updated states, so the shares
#    are in [0, 1] and add to exactly 1. Their joint covariance comes from the stacked
#    per-respondent influence values (n x G), which add to zero across shares.
#  * Influence values for standard errors: the correction with the fixed directions,
#    a' s_mu + g' s_theta, at the moved state (as the old routine), not the path tangent: on
#    the log path the tangent carries the factor omega_eps / omega on the omega component,
#    which is not part of the influence function.
#  * omega at the boundary. When a fold's fitted omega is essentially 0 (Saha-Weeks: about
#    1e-6), the log scale makes the omega coordinate extremely sensitive: a correction that
#    asks for omega + 0.004 multiplies omega by thousands along the log path, and the update
#    then moves omega instead of mu. omega_path = "clamp" is the alternative: omega moves
#    linearly, stops at 0, and its tangent is 0 from there on, so the path score is still the
#    derivative of the path actually taken (no floor inconsistency) without the log scale's
#    sensitivity. The tables report each fold's fitted omega (omega_fit_min) so the boundary
#    case is visible. The default, omega_path = "auto", uses the log path in folds whose fitted
#    omega is at least 1e-4 and the clamp in the others (the path may differ by fold: each fold
#    has its own fitted state, and every path has the same tangent at eps = 0). Neither choice
#    makes inference regular at the boundary (the paper's argument needs omega_0 > 0).
#  * Clipping is not targeting: .rum_bounded_dnn() truncates Wald intervals to [0, 1]; a
#    clipped point estimate is not a targeted estimate. The routines here never clip; their
#    estimates are in [0, 1] because they are plug-ins at updated fitted states.

# Fold states, as in .rum_tmle_dnn(): held-out rows, data subset, fitted mu and th, and the
# information parts.
.rum_tmle_folds <- function(data, Z, fold, fold_final, quad_score, info_basis, ridge_rel,
                            eig_floor_rel, model) {
  FS <- list()
  for (k in sort(unique(fold))) {
    fin <- fold_final(k); ff <- .rum_fold_fit(data, Z, fin, quad_score)
    tr <- fold != k; te <- fold == k
    inf <- .rum_fold_information(info_basis, data, Z, ff, fin, k, tr, te, ridge_rel,
                                 eig_floor_rel, model)
    FS[[as.character(k)]] <- list(te = which(te), sub = rum_subset(data, data$ids[te]),
                                  mu = ff$mu, th = ff$th, parts = inf$parts)
  }
  FS
}

# A target's correction directions at every fold (a: held-out rows x p; g: length q) and
# its held-out plug-in values.
.rum_tmle_dirs <- function(FS, tg, w) {
  wr <- w / mean(w)
  lapply(FS, function(f) {
    gr <- .rum_target_grad(tg, f$mu, f$th)
    cw <- .rum_correction(f$parts, wr * gr$gm, colMeans(wr * gr$gt), f$te)
    list(a = cw$a, g = cw$g, plug = gr$m[f$te])
  })
}

# One fold's held-out state and scores at eps along the path spanned by `path` (a list of
# directions); phi for every direction in `score` (default: the path's). omega_path:
#   "log"          (the fix asked for) log omega_eps = log omega + sum_j eps_j g_j[omega] / omega;
#   "clamp"        omega_eps = max(omega + sum_j eps_j g_j[omega], 0): linear, stops at 0, and its
#                  tangent is then 0, so the path score is the derivative of the path taken;
#   "linear_floor" the old path: omega + eps g_omega floored at `floor`, score with the
#                  unfloored direction (the inconsistency of item 4);
#   "auto"         "log" in a fold whose fitted omega is at least omega_boundary (default 1e-4),
#                  "clamp" in a fold whose fitted omega is below it (at the boundary).
# Returns phi (path scores: the chain rule applied, d loglik / d eps_j), phi_se (the correction
# a_j' s_mu + g_j' s_theta with the fixed directions at the moved state: the influence values
# used for standard errors; equal to phi except for the omega factor of the log path and the
# clamped omega tangent), ll, the moved state, floor_hit (legacy: the floor binds; clamp: omega
# is held at 0), omega, omega_linear (where the linear path would put omega).
.rum_path_fold <- function(f, path, eps, p, quad, omega_path, score = path, floor = 1e-8,
                           omega_boundary = 1e-4) {
  A <- 0; G <- 0
  for (j in seq_along(path)) { A <- A + eps[j] * path[[j]]$a; G <- G + eps[j] * path[[j]]$g }
  mu_e <- f$mu[f$te, , drop = FALSE] + A
  v0 <- .rum_th_vec(f$th); v <- v0 + G; om0 <- v0[p + 2L]; lin <- v[p + 2L]
  if (omega_path == "auto") omega_path <- if (om0 >= omega_boundary) "log" else "clamp"
  if (omega_path == "log") {
    if (om0 > 0) { om <- om0 * exp(G[p + 2L] / om0); dom <- om / om0 } else { om <- 0; dom <- 0 }
    hit <- FALSE
  } else if (omega_path == "clamp") {
    om <- max(lin, 0); dom <- if (lin > 0) 1 else 0; hit <- lin <= 0
  } else {
    om <- max(lin, floor); dom <- 1; hit <- lin < floor
  }
  v[p + 2L] <- om
  nte <- length(f$te)
  if (!all(is.finite(c(v, mu_e)))) {                 # a trial step too long to evaluate
    na <- matrix(NA_real_, nte, length(score))
    return(list(phi = na, phi_se = na, ll = -Inf, mu = mu_e, th = .rum_th_list(v, p),
                floor_hit = hit, omega = om, omega_linear = lin))
  }
  ev <- rum_dnn_gaussian_evaluate(f$sub, mu_e, v[1], v[1 + seq_len(p)], om, quad)
  base <- vapply(score, function(d) rowSums(d$a * ev$output_score) +
                   drop(ev$common_score[, -(p + 2L), drop = FALSE] %*% d$g[-(p + 2L)]), numeric(nte))
  so <- ev$common_score[, p + 2L]
  gom <- vapply(score, function(d) d$g[p + 2L], 0)
  base <- matrix(base, nte)
  phi <- base + outer(so * dom, gom); phi_se <- base + outer(so, gom)
  list(phi = phi, phi_se = phi_se, ll = sum(ev$loglik), mu = mu_e,
       th = .rum_th_list(v, p), floor_hit = hit, omega = om, omega_linear = lin)
}

# All folds at eps: phi and phi_se stacked in respondent order (n x J), total held-out log
# likelihood, and the per-fold states.
.rum_path_all <- function(FS, dirs, eps, n, p, quad, omega_path, score = NULL,
                          omega_boundary = 1e-4) {
  ks <- names(FS); J <- if (is.null(score)) length(dirs) else length(score)
  phi <- phi_se <- matrix(NA_real_, n, J); ll <- 0; st <- list(); hit <- FALSE; om <- numeric(0)
  for (k in ks) {
    f <- FS[[k]]; pd <- lapply(dirs, `[[`, k)
    sd <- if (is.null(score)) pd else lapply(score, `[[`, k)
    r <- .rum_path_fold(f, pd, eps, p, quad, omega_path, sd, omega_boundary = omega_boundary)
    phi[f$te, ] <- r$phi; phi_se[f$te, ] <- r$phi_se; ll <- ll + r$ll; st[[k]] <- r
    hit <- hit || r$floor_hit; om <- c(om, r$omega)
  }
  list(phi = phi, phi_se = phi_se, ll = ll, st = st, floor_hit = hit, omega = om)
}

# The convergence criterion and Fisher step for a score matrix phi (n x J), computed on
# normalized directions (Codex review of eae4719, finding 3). Each column is divided by its
# root sum of squares D_j, so In = crossprod(phi / D) has unit diagonal and the criterion does
# not depend on the directions' scales; In is eigen-decomposed and directions with eigenvalue
# below rank_tol (relative to the largest) are dropped from the step. crit =
# sqrt(Sn' In^+ Sn) over the kept directions (for J = 1: |sum phi| / sqrt(sum phi^2), the old
# rule). resid_dropped = the largest |v' Sn| over dropped unit directions v: the residual that
# the step cannot move, on the scale of the columns (an all-zero column has residual 0).
# Before, the step and the criterion both used a truncated pseudoinverse of the unnormalized
# matrix, so a direction with a small scale was dropped and its residual was never checked:
# for score rows (1, 1e-10) and (-1, 1e-10) the old criterion was 0 although the second
# direction's standardized residual is sqrt(2).
.rum_score_crit <- function(phi, rank_tol = 1e-12) {
  phi <- as.matrix(phi); S <- colSums(phi); D <- sqrt(colSums(phi^2)); D0 <- D > 0
  Dn <- ifelse(D0, D, 1); Sn <- S / Dn; In <- crossprod(sweep(phi, 2, Dn, "/"))
  e <- eigen((In + t(In)) / 2, symmetric = TRUE); mx <- max(e$values, 0)
  keep <- e$values > rank_tol * mx
  z <- drop(crossprod(e$vectors, Sn))
  crit <- sqrt(sum(z[keep]^2 / e$values[keep]))
  stepn <- if (any(keep)) drop(e$vectors[, keep, drop = FALSE] %*% (z[keep] / e$values[keep])) else numeric(length(S))
  list(S = S, step = stepn / Dn, crit = crit, rank = sum(keep),
       resid_dropped = if (any(!keep)) max(abs(z[!keep])) else 0)
}

# Fisher scoring with step halving on the held-out log likelihood along the path.
# eval(eps) -> list(phi, ll). Returns eps, the last evaluation, status, iterations,
# halvings, crit, rank and resid_dropped (.rum_score_crit). Status "converged" needs the
# criterion below tol on the kept directions AND resid_dropped below tol; when the kept
# directions are solved but a dropped one is not, the status is "rank_deficient" (a failure).
.rum_tmle_solve <- function(eval, J, max_iter = 20L, tol = 1e-3, max_halving = 30L,
                            rank_tol = 1e-12, eps0 = numeric(J)) {
  eps <- eps0; cur <- eval(eps); it <- 0L; halv <- 0L; status <- NA_character_
  rank <- NA_integer_; rdrop <- NA_real_
  repeat {
    if (!all(is.finite(cur$phi)) || !is.finite(cur$ll)) { status <- "nonfinite"; crit <- NA_real_; break }
    cc <- .rum_score_crit(cur$phi, rank_tol); crit <- cc$crit; rank <- cc$rank; rdrop <- cc$resid_dropped
    if (!is.finite(crit) || !is.finite(rdrop)) { status <- "nonfinite"; break }
    if (crit < tol && rdrop >= tol) { status <- "rank_deficient"; break }
    if (crit < tol) { status <- "converged"; break }
    if (it >= max_iter) { status <- "max_iter"; break }
    t <- 1; ok <- FALSE
    for (h in 0:max_halving) {
      cand <- tryCatch(eval(eps + t * cc$step), error = function(e) list(phi = NA_real_, ll = -Inf))
      if (all(is.finite(cand$phi)) && is.finite(cand$ll) &&
          cand$ll >= cur$ll - 1e-12 * abs(cur$ll)) { ok <- TRUE; break }
      t <- t / 2
    }
    if (!ok) { status <- "step_failed"; break }
    eps <- eps + t * cc$step; cur <- cand; it <- it + 1L; halv <- halv + h
  }
  list(eps = eps, cur = cur, status = status, iterations = it, halvings = halv, crit = crit,
       rank = rank, resid_dropped = rdrop)
}

# Held-out plug-in values m_i at the states of an evaluation.
.rum_path_m <- function(FS, ev, tg, n) {
  m <- numeric(n)
  for (k in names(FS)) {
    f <- FS[[k]]; r <- ev$st[[k]]; mu_full <- f$mu; mu_full[f$te, ] <- r$mu
    m[f$te] <- tg$m(mu_full, r$th)[f$te]
  }
  m
}

.rum_wald01 <- function(est, se, sgn) pmin(pmax(est + sgn * 1.96 * se, 0), 1)

# Targeting rounds (Codex review of eae4719, finding 4). The solver zeroes the PATH score phi,
# but the standard errors use phi_se (the correction a' s_mu + g' s_theta at the moved state).
# They differ on the omega component: by the factor omega_eps / omega on the log path, and
# where the clamp binds. So, at a solution, the influence equation's own residual (the
# criterion of .rum_score_crit on phi_se) is checked against the same tolerance. If it is
# material, the update is retargeted: a new path starts at the moved states with the same
# directions; at its start its path score IS phi_se (the log path's factor is 1 there, and the
# linear components compose exactly), so solving it removes that residual, and the new
# mismatch is of the order of the (small) second step. At most max_rounds rounds; then status
# "influence_residual", a failure (estimate NA). Typical cause: a fold at the variance
# boundary where the clamp binds and the omega part of the correction cannot be targeted.
# pdirs: the path directions (J); sdirs: all score directions; keep: the columns of the
# score that the path solves. Returns the last solve's fields plus FS (the states the final
# path starts from), cur (its .rum_path_all evaluation), rounds, iterations and halvings
# summed over rounds, eps (the sum over rounds of the step sizes; additive on the linear
# components) and if_crit, if_resid_dropped (the influence equation's residual at the end).
.rum_tmle_rounds <- function(FS, pdirs, sdirs, keep, n, p, quad, omega_path, omega_boundary,
                             max_iter, tol, max_halving, rank_tol = 1e-12, max_rounds = 5L) {
  J <- length(pdirs); rounds <- 0L; its <- 0L; halv <- 0L; eps_sum <- numeric(J)
  repeat {
    FSr <- FS
    eval <- function(eps) {
      r <- .rum_path_all(FSr, pdirs, eps, n, p, quad, omega_path, score = sdirs,
                         omega_boundary = omega_boundary)
      r$phi <- r$phi[, keep, drop = FALSE]; r       # the solved columns; phi_se keeps all
    }
    sol <- .rum_tmle_solve(eval, J, max_iter, tol, max_halving, rank_tol)
    rounds <- rounds + 1L; its <- its + sol$iterations; halv <- halv + sol$halvings
    eps_sum <- eps_sum + sol$eps
    ic <- if (all(is.finite(sol$cur$phi_se))) .rum_score_crit(sol$cur$phi_se[, keep, drop = FALSE], rank_tol)
          else list(crit = NA_real_, resid_dropped = NA_real_)
    if (!identical(sol$status, "converged")) break
    if (ic$crit < tol && ic$resid_dropped < tol) break
    if (rounds >= max_rounds) { sol$status <- "influence_residual"; break }
    for (k in names(FS)) {                           # restart from the moved states
      st <- sol$cur$st[[k]]; FS[[k]]$mu[FS[[k]]$te, ] <- st$mu; FS[[k]]$th <- st$th
    }
  }
  c(sol[c("status", "crit", "rank", "resid_dropped", "cur")],
    list(FS = FSr, rounds = rounds, iterations = its, halvings = halv, eps = eps_sum,
         if_crit = ic$crit, if_resid_dropped = ic$resid_dropped))
}

# ---- scalar targeted update ---------------------------------------------------------------
# For each target separately. omega_path "auto" (default: the log path, with "clamp" in folds
# whose fitted omega is below omega_boundary), "log", "clamp", or
# "legacy": the old .rum_tmle_dnn() iteration exactly (linear omega with the 1e-8 floor, no
# step control), with the floor hits and the non-convergence recorded. on_fail "na" (default)
# returns estimate NA for a failed target; "last" returns the last iterate (the old behavior;
# only for comparison). Returns a data frame: target, kind, n_group, method, status,
# converged, iterations, halvings, crit, eps, plug_in, onestep, estimate, SEs and intervals
# (Wald truncated to [0, 1]), floor_hit_final, floor_hit_any, omega_min (smallest fold omega
# at the final eps), omega_linear_min (where the linear path would put it); attributes IFF,
# IFP (NA columns for failed targets) and n_failed.
.rum_tmle_path <- function(data, Z, fold, fold_final, targets,
                           quad_score = .rum_score_quadrature(), ridge_rel = 1e-2,
                           eig_floor_rel = 1e-6, max_iter = 20L, tol = 1e-3,
                           omega_path = c("auto", "log", "clamp", "legacy"),
                           on_fail = c("na", "last"), max_halving = 30L, omega_boundary = 1e-4,
                           info_basis = c("constant", "model", "linear"), model_M = NULL,
                           model_quad = .rum_info_quadrature(), model_seed = 20260926L,
                           cache_net = NULL, cache_dir = NULL, cache_write = TRUE,
                           cache_require = FALSE, FS = NULL, max_rounds = 5L) {
  omega_path <- match.arg(omega_path); on_fail <- match.arg(on_fail)
  info_basis <- match.arg(info_basis)
  n <- data$n; p <- data$p
  model <- .rum_model_options(model_M, model_quad, model_seed, cache_net, cache_dir,
                              cache_write, cache_require)
  if (is.null(FS)) FS <- .rum_tmle_folds(data, Z, fold, fold_final, quad_score, info_basis,
                                         ridge_rel, eig_floor_rel, model)
  wmat <- .rum_target_weights(targets, n)
  path_kind <- if (omega_path == "legacy") "linear_floor" else omega_path
  res <- lapply(seq_along(targets), function(t) {
    w <- wmat[, t]; wr <- w / mean(w); tg <- targets[[t]]
    dirs <- list(.rum_tmle_dirs(FS, tg, w))
    eval <- function(eps) .rum_path_all(FS, dirs, eps, n, p, quad_score, path_kind,
                                        omega_boundary = omega_boundary)
    hit_any <- FALSE
    if (omega_path == "legacy") {             # the old loop, verbatim arithmetic
      eps <- 0; it <- 0L; conv <- FALSE
      repeat {
        cur <- eval(eps); hit_any <- hit_any || cur$floor_hit
        ph <- cur$phi[, 1]; sc <- sum(ph) / sqrt(sum(ph^2))
        if (abs(sc) < tol) { conv <- TRUE; break }
        if (it >= max_iter) break
        eps <- eps + sum(ph) / sum(ph^2); it <- it + 1L
      }
      sol <- list(eps = eps, cur = cur, status = if (conv) "converged" else "max_iter",
                  iterations = it, halvings = 0L, crit = abs(sc), rounds = 1L, FS = FS,
                  if_crit = abs(sum(cur$phi_se)) / sqrt(sum(cur$phi_se^2)))
    } else {
      sol <- .rum_tmle_rounds(FS, dirs, dirs, 1L, n, p, quad_score, path_kind, omega_boundary,
                              max_iter, tol, max_halving, max_rounds = max_rounds)
    }
    ev0 <- eval(0); phi0 <- ev0$phi[, 1]
    m0 <- numeric(n); for (k in names(FS)) m0[FS[[k]]$te] <- dirs[[1]][[k]]$plug
    plug <- sum(w * m0) / sum(w); onestep <- plug + mean(phi0)
    ok <- identical(sol$status, "converged") || on_fail == "last"
    if (ok) {
      m_star <- .rum_path_m(sol$FS, sol$cur, tg, n); phi <- sol$cur$phi_se[, 1]
      est <- sum(w * m_star) / sum(w)
      iff <- phi - mean(phi); ifp <- iff + wr * (m_star - est)
      sef <- stats::sd(iff) / sqrt(n); sep <- stats::sd(ifp) / sqrt(n)
    } else { est <- sef <- sep <- NA_real_; iff <- ifp <- phi <- rep(NA_real_, n) }
    lin <- vapply(names(FS), function(k) sol$cur$st[[k]]$omega_linear, 0)
    list(row = data.frame(target = tg$name, kind = tg$kind, n_group = sum(w > 0),
                          method = switch(omega_path, auto = sprintf("CV-TMLE, log-omega path (clamp in folds with omega < %g), step control", omega_boundary),
                                          log = "CV-TMLE, log-omega path, step control",
                                          clamp = "CV-TMLE, linear omega stopping at 0, step control",
                                          legacy = "CV-TMLE, legacy (linear omega, floor 1e-8)"),
                          status = sol$status, converged = identical(sol$status, "converged"),
                          iterations = sol$iterations, halvings = sol$halvings, crit = sol$crit,
                          rounds = sol$rounds, if_crit = sol$if_crit,
                          eps = sol$eps, plug_in = plug, onestep = onestep, estimate = est,
                          se_population = sep, lower_population = .rum_wald01(est, sep, -1),
                          upper_population = .rum_wald01(est, sep, 1), se_fixed = sef,
                          lower_fixed = .rum_wald01(est, sef, -1),
                          upper_fixed = .rum_wald01(est, sef, 1),
                          floor_hit_final = sol$cur$floor_hit, floor_hit_any = hit_any || sol$cur$floor_hit,
                          omega_min = min(sol$cur$omega), omega_linear_min = min(lin),
                          omega_fit_min = min(vapply(FS, function(f) f$th$omega, 0)),
                          folds_clamped = if (omega_path == "auto") sum(vapply(FS, function(f) f$th$omega < omega_boundary, TRUE))
                                          else if (omega_path == "clamp") length(FS) else 0L,
                          stringsAsFactors = FALSE),
         iff = iff, ifp = ifp, phi = phi)
  })
  out <- do.call(rbind, lapply(res, `[[`, "row")); rownames(out) <- out$target
  nmt <- vapply(targets, `[[`, "", "name")
  attr(out, "IFF") <- `colnames<-`(matrix(sapply(res, `[[`, "iff"), n), nmt)
  attr(out, "IFP") <- `colnames<-`(matrix(sapply(res, `[[`, "ifp"), n), nmt)
  # the uncentered influence values (phi_se at the final state): their mean is the residual of
  # the influence equation, which IFF's centering hides
  attr(out, "PHI") <- `colnames<-`(matrix(sapply(res, `[[`, "phi"), n), nmt)
  attr(out, "n_failed") <- sum(!out$converged)
  out
}

# ---- joint targeted update for shares on the simplex ---------------------------------------
# targets: G >= 2 share targets with the same population weights whose per-respondent values
# sum to one at every fitted state (checked at each fold's fit, to check_tol). The path uses
# the directions of all shares but the one with the largest plug-in (any G - 1 span the same
# path). Returns list(table = one row per share (plug_in, onestep, estimate, SEs, truncated
# Wald intervals) plus a TOTAL row, status, converged, iterations, eps (length G - 1),
# IFF, IFP (n x G), V_population and V_fixed (G x G joint covariances of the estimates,
# cov(IF) / n), dir_sum_max (largest |sum over shares of the plain corrections|, which is 0
# up to rounding), sum_check_max). On failure the estimates are NA.
.rum_tmle_joint <- function(data, Z, fold, fold_final, targets,
                            quad_score = .rum_score_quadrature(), ridge_rel = 1e-2,
                            eig_floor_rel = 1e-6, max_iter = 50L, tol = 1e-3,
                            max_halving = 30L, check_tol = 1e-8,
                            omega_path = c("auto", "log", "clamp"), omega_boundary = 1e-4,
                            info_basis = c("constant", "model", "linear"), model_M = NULL,
                            model_quad = .rum_info_quadrature(), model_seed = 20260926L,
                            cache_net = NULL, cache_dir = NULL, cache_write = TRUE,
                            cache_require = FALSE, FS = NULL, max_rounds = 5L) {
  info_basis <- match.arg(info_basis); omega_path <- match.arg(omega_path)
  n <- data$n; p <- data$p; G <- length(targets)
  if (G < 2L) stop("a joint update needs at least two shares")
  model <- .rum_model_options(model_M, model_quad, model_seed, cache_net, cache_dir,
                              cache_write, cache_require)
  wmat <- .rum_target_weights(targets, n)
  if (max(abs(wmat - wmat[, 1])) > 0) stop("the shares of one joint update need the same population weights")
  w <- wmat[, 1]; wr <- w / mean(w)
  if (is.null(FS)) FS <- .rum_tmle_folds(data, Z, fold, fold_final, quad_score, info_basis,
                                         ridge_rel, eig_floor_rel, model)
  dirs <- lapply(targets, function(tg) .rum_tmle_dirs(FS, tg, w))
  m0 <- matrix(0, n, G)
  for (g in seq_len(G)) for (k in names(FS)) m0[FS[[k]]$te, g] <- dirs[[g]][[k]]$plug
  sum_check <- max(abs(rowSums(m0[w > 0, , drop = FALSE]) - 1))
  if (sum_check > check_tol) stop(sprintf("the shares do not sum to one at the fitted states (max |sum - 1| = %.2e)", sum_check))
  plug <- colSums(w * m0) / sum(w)
  drop_g <- which.max(plug); keep <- setdiff(seq_len(G), drop_g)
  ev0 <- .rum_path_all(FS, dirs[keep], numeric(G - 1L), n, p, quad_score, omega_path, score = dirs,
                       omega_boundary = omega_boundary)
  dir_sum <- max(abs(rowSums(ev0$phi)))
  onestep <- plug + colMeans(ev0$phi)
  sol <- .rum_tmle_rounds(FS, dirs[keep], dirs, keep, n, p, quad_score, omega_path,
                          omega_boundary, max_iter, tol, max_halving, max_rounds = max_rounds)
  ok <- identical(sol$status, "converged")
  nm <- vapply(targets, `[[`, "", "name")
  if (ok) {
    full <- sol$cur; m_star <- matrix(0, n, G)
    for (g in seq_len(G)) m_star[, g] <- .rum_path_m(sol$FS, full, targets[[g]], n)
    est <- colSums(w * m_star) / sum(w); phi <- full$phi_se
    IFF <- sweep(phi, 2, colMeans(phi)); IFP <- IFF + wr * sweep(m_star, 2, est)
  } else {
    est <- rep(NA_real_, G); IFF <- IFP <- phi <- matrix(NA_real_, n, G)
  }
  colnames(IFF) <- colnames(IFP) <- nm
  Vp <- stats::cov(IFP) / n; Vf <- stats::cov(IFF) / n
  sep <- sqrt(pmax(diag(Vp), 0)); sef <- sqrt(pmax(diag(Vf), 0))
  tab <- data.frame(target = c(nm, "TOTAL"), kind = c(vapply(targets, `[[`, "", "kind"), "total"),
                    n_group = sum(w > 0), method = sprintf("CV-TMLE, joint simplex update (%s omega path)", omega_path),
                    status = sol$status, converged = ok, iterations = sol$iterations,
                    plug_in = c(plug, sum(plug)), onestep = c(onestep, sum(onestep)),
                    estimate = c(est, sum(est)),
                    se_population = c(sep, sqrt(max(0, sum(Vp)))),
                    se_fixed = c(sef, sqrt(max(0, sum(Vf)))), stringsAsFactors = FALSE)
  tab$lower_population <- .rum_wald01(tab$estimate, tab$se_population, -1)
  tab$upper_population <- .rum_wald01(tab$estimate, tab$se_population, 1)
  tab$lower_fixed <- .rum_wald01(tab$estimate, tab$se_fixed, -1)
  tab$upper_fixed <- .rum_wald01(tab$estimate, tab$se_fixed, 1)
  rownames(tab) <- tab$target
  list(table = tab, status = sol$status, converged = ok, iterations = sol$iterations,
       halvings = sol$halvings, crit = sol$crit, rounds = sol$rounds, if_crit = sol$if_crit,
       rank = sol$rank, resid_dropped = sol$resid_dropped, eps = stats::setNames(sol$eps, nm[keep]),
       IFF = IFF, IFP = IFP, PHI = `colnames<-`(phi, nm), V_population = Vp, V_fixed = Vf, dir_sum_max = dir_sum,
       sum_check_max = sum_check, omega_min = min(sol$cur$omega),
       omega_fit_min = min(vapply(FS, function(f) f$th$omega, 0)), omega_path = omega_path)
}
