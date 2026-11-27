# Population targets for RUM inference, and the fitted-state helpers they need.
# Ported from the RUM reproduction scripts of 2026-09-24/26 (Yiqing Xu):
# scripts_C/rum_debias.R (targets, target_grad) and scripts_C/rum_core.R
# (state_formula, choice_fitted), with the arithmetic unchanged. Internal; the engine
# that uses these targets is in rum-inference.R.
#
# Model: beta_i = mu(z_i) + Lambda u_i, u ~ N(0, I_q); eta_it = kappa + d_it' beta_i +
# sqrt(omega) * v_i, v ~ N(0, 1) (random side preference); P(y_it = 1) = logistic(eta_it).
# Lambda (th$loading) is p x q (q = n_fac taste factors); a plain length-p vector is one factor.
# Every target depends on Lambda only through Lambda Lambda' (the taste covariance), so it is
# unchanged when Lambda is rotated (Lambda A, A orthogonal q x q). For a contrast or contest d,
# d' Lambda u ~ N(0, ||Lambda' d||^2): the choice, share and switch targets keep their
# one-dimensional integrals with the scalar s = ||Lambda' d|| in place of the one-factor
# loading. With one factor the code keeps the signed s = lambda' d (same values in exact
# arithmetic, since the integrands are symmetric in u; kept so q = 1 is bitwise unchanged).
#
# A target is a list with
#   name  label (character),
#   kind  "mean", "share", "choice", "lincomb", ... (bounded kinds: .rum_bounded_kinds),
#   w     NULL or respondent weights in data$ids order (a subgroup: 1 inside, 0 outside),
#   m     function(mu, th) -> the n per-respondent values m_i,
#   grad  optional function(mu, th) -> list(m, gm = n x p, gt = n x n_th) with exact gradients.
# th = list(kappa, loading, omega); the common parameters theta are ordered as the
# evaluator's common score: kappa, vec(loading) (column-major, p q entries), omega
# (n_th = p q + 2 entries). The layout is defined once, by .rum_th_idx() below; code reads
# theta through it, never by fixed position. gt's loading block follows the same layout.
# A new target type only needs m (central differences are then used) or m and grad.

# Kinds that live in [0, 1]; .rum_bounded_dnn() reports them with the CV-TMLE.
.rum_bounded_kinds <- c("share", "share_nonlinear", "importance")

# Quadrature rules of the engine: fitted-law integrals in targets (41 x 41), sequence
# scores (21 x 21), and the model-based information (21 nodes in u, 11 in v). With n_fac > 1
# taste factors (Stage 2 of the q-factor build, 2026-11-14): the scores use the trainer's check
# rule (Owen-scrambled Sobol, 2,048 nodes over (u, v), seed 20261114) and the information the
# training rule (Sobol 512, the run plan's rule); the fitted-law targets keep their
# one-dimensional rule (d' Lambda u is N(0, ||Lambda' d||^2)). n_fac = 1 gives the old objects.
.rum_law_quadrature <- function() rum_quadrature(c(41L, 41L))
.rum_score_quadrature <- function(n_fac = 1L)
  if (n_fac == 1L) rum_quadrature(c(21L, 21L)) else rum_quadrature(n_fac = n_fac, rule = "sobol", nodes = 2048L)
.rum_info_quadrature <- function(n_fac = 1L)
  if (n_fac == 1L) rum_quadrature(c(21L, 11L)) else rum_quadrature(n_fac = n_fac, rule = "sobol", nodes = 512L)
# The rule for a state with n_fac factors. A rule with n_fac factors is returned as is. The
# one-factor default of the inference routines (quad_score = .rum_score_quadrature(),
# model_quad = .rum_info_quadrature(), cond$quad), given for a state with n_fac > 1 factors,
# is replaced by the n_fac-factor default of the same kind; any other mismatch stops.
.rum_quad_for <- function(quad, n_fac, kind = c("score", "info")) {
  kind <- match.arg(kind)
  if (.rum_quad_nfac(quad) == n_fac) return(quad)
  d1 <- if (kind == "score") .rum_score_quadrature() else .rum_info_quadrature()
  if (n_fac > 1L && identical(unclass(quad), unclass(d1)))
    return(if (kind == "score") .rum_score_quadrature(n_fac) else .rum_info_quadrature(n_fac))
  stop(sprintf("the %s quadrature has %d taste factor(s) but the fitted state has %d; pass a rule built with rum_quadrature(n_fac = %d)",
               kind, .rum_quad_nfac(quad), n_fac, n_fac), call. = FALSE)
}

# ---- fitted states ------------------------------------------------------------------
# Formula version: mu_i = B_i alpha. Returns list(mu, kappa, loading, omega, version).
.rum_state_formula <- function(data, par) {
  r <- data$r; p <- data$p; alpha <- par[1L + seq_len(r)]
  basis <- if (is.list(data$basis)) data$basis else rep(list(data$basis), data$n)
  # vapply() simplifies to a vector when p = 1; reshape to p x n before transposing so that
  # mu is always n x p
  mu <- t(matrix(vapply(basis, function(b) drop(b %*% alpha), numeric(p)), nrow = p))
  dimnames(mu) <- list(data$ids, data$feature_names)
  list(mu = mu, kappa = unname(par[1]), loading = unname(par[1L + r + seq_len(p)]),
       omega = unname(utils::tail(par, 1)), version = "formula")
}

# Scale of the taste draw along a contrast d: d' Lambda u ~ N(0, s^2). One factor: the signed
# s = lambda' d (the one-factor code path, unchanged); q > 1 factors: s = ||Lambda' d|| >= 0.
.rum_multi_fac <- function(loading) is.matrix(loading) && ncol(loading) > 1L
.rum_ld_scale <- function(loading, d)
  if (.rum_multi_fac(loading)) sqrt(sum(crossprod(loading, d)^2)) else sum(loading * d)
# Loading block of a gradient through s = ||Lambda' d|| (q > 1): given dm/ds per respondent (gs, n
# values), dm/dLambda = gs d (Lambda' d)' / s, returned n x (p q) in theta's column-major order.
# At s = 0 the block is 0 (the integrals are even in s, so dm/ds = 0 there).
.rum_ld_grad <- function(gs, loading, d) {
  ld <- drop(crossprod(loading, d)); s <- sqrt(sum(ld^2))
  if (!(s > 0)) return(matrix(0, length(gs), length(loading)))
  outer(gs, as.vector(outer(d, ld / s)))
}

# Position-neutral probability that profile A beats B (d = x_A - x_B), integrated over
# the fitted law, with the random side preference or without it. Per-respondent values.
.rum_choice_fitted <- function(st, dvec, quad, random_side = TRUE) {
  m <- drop(st$mu %*% dvec); s <- .rum_ld_scale(st$loading, dvec)
  eta <- outer(m, rep(1, length(quad$u))) + outer(rep(1, length(m)), s * quad$u)
  side <- if (random_side) sqrt(st$omega) * quad$v else 0 * quad$v
  sv <- matrix(side, nrow(eta), ncol(eta), byrow = TRUE)
  drop((0.5 * (stats::plogis(st$kappa + sv + eta) + stats::plogis(-st$kappa - sv + eta))) %*%
         quad$weight)
}

# ---- target constructors --------------------------------------------------------------
# Mean contrast c' mu_i.
.rum_tgt_mean <- function(name, c, w = NULL) list(name = name, kind = "mean", c = c, w = w,
  m = function(mu, th) drop(mu %*% c))

# Fitted-law share with c' beta_i > 0: Phi(c' mu_i / ||Lambda' c||) (one factor: |c' lambda|). When
# Lambda' c = 0 the law of c' beta_i is a point mass at c' mu_i: the share is 1(c' mu_i > 0).
.rum_tgt_share <- function(name, c, w = NULL) list(name = name, kind = "share", c = c, w = w,
  m = function(mu, th) {
    s <- abs(.rum_ld_scale(th$loading, c))
    if (s > 0) stats::pnorm(drop(mu %*% c) / s) else as.numeric(drop(mu %*% c) > 0)
  })

# Position-neutral win probability for one contest d, side preference integrated
# (gradients by central differences).
.rum_tgt_choice <- function(name, d, w = NULL, quad = .rum_law_quadrature()) {
  force(quad)
  list(name = name, kind = "choice", d = d, w = w,
       m = function(mu, th) .rum_choice_fitted(list(mu = mu, kappa = th$kappa,
         loading = th$loading, omega = th$omega), d, quad, TRUE))
}

# Fixed linear combination of per-respondent values (e.g. an AME over a frozen draw set).
.rum_tgt_lincomb <- function(name, parts, coefs, w = NULL)
  list(name = name, kind = "lincomb", w = w,
       m = function(mu, th) Reduce(`+`, Map(function(t, a) a * t$m(mu, th), parts, coefs)))

# Position-neutral win probabilities over contests D (R x p rows d_r = x_A - x_B) with
# coefficients a_r, averaged per respondent: m_i = sum_r a_r P_i(d_r). Exact gradients:
#   dm/dmu     = sum_r a_r E[.5(L'(A)+L'(B))] d_r
#   dm/dkappa  = sum_r a_r E[.5(L'(A)-L'(B))]
#   dm/dlambda = sum_r a_r E[.5(L'(A)+L'(B)) u] d_r
#   dm/domega  = sum_r a_r E[.5(L'(A)-L'(B)) v] / (2 sqrt(omega)),
# with A = kappa + s v + d'(mu + lambda u), B = -kappa - s v + d'(mu + lambda u), s = sqrt(omega).
# q > 1 factors: lambda u is replaced by ||Lambda' d|| u (d' Lambda u ~ N(0, ||Lambda' d||^2)), and
#   dm/dLambda = sum_r a_r E[.5(L'(A)+L'(B)) u] d_r (Lambda' d_r)' / ||Lambda' d_r||   (p x q;
# 0 when Lambda' d_r = 0, where the integral is even in the scale), stored column-major in gt.
# At the boundary the division is 0/0. For v ~ N(0, 1), E[f(s v) v] = s E[f'(s v)] (Stein), so
#   dm/domega  = sum_r a_r E[.5(L''(A)+L''(B))] / 2,
# with no division; its value at omega = 0 is the right derivative. It is used when omega is
# below .rum_omega_small (1e-8: where the division form has lost about 1e-16 / s of precision,
# s < 1e-4); above it the division form is kept (unchanged numbers).
.rum_tgt_choice_set <- function(name, D, coef = rep(1 / nrow(rbind(D)), nrow(rbind(D))),
                                w = NULL, quad = .rum_law_quadrature()) {
  D <- rbind(D); force(coef); force(w); force(quad)   # built in a loop, lazy coef took the last weights
  vg <- function(mu, th) {
    n <- nrow(mu); p <- ncol(mu); G <- length(quad$weight); s <- sqrt(th$omega)
    multi <- .rum_multi_fac(th$loading)
    val <- numeric(n); gm <- matrix(0, n, p); gk <- gl <- go <- numeric(n)
    glv <- matrix(0, n, if (multi) length(th$loading) else p)
    svm <- matrix(s * quad$v, n, G, byrow = TRUE); um <- matrix(quad$u, n, G, byrow = TRUE)
    vm <- matrix(quad$v, n, G, byrow = TRUE)
    for (r in seq_len(nrow(D))) {
      d <- D[r, ]; eta <- outer(drop(mu %*% d), rep(1, G)) + .rum_ld_scale(th$loading, d) * um
      A <- th$kappa + svm + eta; B <- -th$kappa - svm + eta
      pa <- stats::plogis(A); pb <- stats::plogis(B); da <- pa * (1 - pa); db <- pb * (1 - pb)
      val <- val + coef[r] * drop((0.5 * (pa + pb)) %*% quad$weight)
      c1 <- drop((0.5 * (da + db)) %*% quad$weight)
      gm <- gm + coef[r] * outer(c1, d)
      gk <- gk + coef[r] * drop((0.5 * (da - db)) %*% quad$weight)
      glv <- glv + coef[r] * (if (multi) .rum_ld_grad(drop((0.5 * (da + db) * um) %*% quad$weight), th$loading, d)
                              else outer(drop((0.5 * (da + db) * um) %*% quad$weight), d))
      go <- go + coef[r] * (if (th$omega < .rum_omega_small)
        drop((0.25 * (da * (1 - 2 * pa) + db * (1 - 2 * pb))) %*% quad$weight)
        else drop((0.5 * (da - db) * vm) %*% quad$weight) / (2 * s))
    }
    list(m = val, gm = gm, gt = cbind(gk, glv, go))
  }
  list(name = name, kind = "choice", w = w, m = function(mu, th) vg(mu, th)$m, grad = vg)
}

# Win probabilities WITHOUT the side preference (the paper's head-to-head choice
# G((x_A - x_B)' beta_i)): m_i = sum_r a_r E_u[G(d_r'(mu_i + lambda u))], kappa = 0.
# Gradients: dm/dmu = sum_r a_r E[G'(eta)] d_r; dm/dlambda = sum_r a_r E[G'(eta) u] d_r;
# dm/dkappa = dm/domega = 0. The (u, v) quadrature is reused; v does not enter. q > 1 factors:
# as .rum_tgt_choice_set (scale ||Lambda' d_r||, loading block through .rum_ld_grad).
.rum_tgt_choice_pref <- function(name, d, w = NULL, quad = .rum_law_quadrature()) {
  force(quad)
  list(name = name, kind = "choice", d = d, w = w,
       m = function(mu, th) .rum_choice_fitted(list(mu = mu, kappa = 0, loading = th$loading,
         omega = th$omega), d, quad, FALSE))
}

.rum_tgt_choice_set_pref <- function(name, D, coef = rep(1 / nrow(rbind(D)), nrow(rbind(D))),
                                     w = NULL, quad = .rum_law_quadrature()) {
  # Force every argument: a contest built in a loop otherwise reads coef, w and quad when it is
  # first evaluated, i.e. the last iteration's values (paper-v4 spec 5.1 item 1).
  D <- rbind(D); force(coef); force(w); force(quad)
  vg <- function(mu, th) {
    n <- nrow(mu); p <- ncol(mu); G <- length(quad$weight); multi <- .rum_multi_fac(th$loading)
    val <- numeric(n); gm <- matrix(0, n, p); glv <- matrix(0, n, if (multi) length(th$loading) else p)
    um <- matrix(quad$u, n, G, byrow = TRUE)
    for (r in seq_len(nrow(D))) {
      d <- D[r, ]; eta <- outer(drop(mu %*% d), rep(1, G)) + .rum_ld_scale(th$loading, d) * um
      pe <- stats::plogis(eta); de <- pe * (1 - pe)
      val <- val + coef[r] * drop(pe %*% quad$weight)
      gm  <- gm  + coef[r] * outer(drop(de %*% quad$weight), d)
      glv <- glv + coef[r] * (if (multi) .rum_ld_grad(drop((de * um) %*% quad$weight), th$loading, d)
                              else outer(drop((de * um) %*% quad$weight), d))
    }
    list(m = val, gm = gm, gt = cbind(numeric(n), glv, numeric(n)))
  }
  list(name = name, kind = "choice", w = w, m = function(mu, th) vg(mu, th)$m, grad = vg)
}

# ---- gradients --------------------------------------------------------------------------
# Below this omega, omega derivatives use their boundary forms (no division by sqrt(omega)).
# The value (1e-8) is defined once, in the C++ kernel (RUM_OMEGA_SMALL in src/rum_core.cpp), and
# assigned here when the namespace loads (R/rum-zzz.R), so R and C++ switch at the same omega.
.rum_omega_small <- NULL

# ---- layout of the common parameters theta ----------------------------------------------
# theta = (kappa, loading, omega), the order of the evaluator's common score. With n_fac taste
# factors the loading is p x n_fac and is stored column by column (factor 1's p entries first),
# so theta has n_th = p n_fac + 2 entries: kappa at 1, the loading at 1 + 1..(p n_fac), omega
# last. n_fac = 1 (the default everywhere) is the one-factor layout: kappa 1, loading
# 1 + 1..p, omega p + 2. offset shifts every index (theta placed after offset other entries,
# e.g. the means s_mu in the full score, or the network parameters in the trainer).
.rum_th_len <- function(p, n_fac = 1L) as.integer(p * n_fac + 2L)
.rum_th_idx <- function(p, n_fac = 1L, offset = 0L) {
  nl <- p * n_fac
  list(kappa = offset + 1L, loading = offset + 1L + seq_len(nl), omega = offset + nl + 2L,
       n_th = as.integer(nl + 2L))
}
# Number of taste factors of a state th (a loading matrix has one column per factor).
.rum_th_nfac <- function(th) if (is.matrix(th$loading)) ncol(th$loading) else 1L
# Names of theta's entries; lab: the p loading labels of one factor (with several factors the
# factor number is appended).
.rum_th_names <- function(lab, n_fac = 1L)
  c("kappa", if (n_fac == 1L) lab else paste0(rep(lab, n_fac), "_f", rep(seq_len(n_fac), each = length(lab))),
    "omega")
.rum_th_vec <- function(th, n_fac = .rum_th_nfac(th)) {
  if (length(th$loading) %% n_fac) stop("the loading does not have n_fac columns")
  c(th$kappa, th$loading, th$omega)
}
.rum_th_list <- function(v, p, n_fac = 1L) {
  ix <- .rum_th_idx(p, n_fac)
  list(kappa = v[ix$kappa],
       loading = if (n_fac == 1L) v[ix$loading] else matrix(v[ix$loading], p, n_fac),
       omega = v[ix$omega])
}

# Per-respondent value m_i and gradients m_mu (n x p) and m_theta (n x n_th). Exact gradients
# when the target supplies them; mean contrasts are analytic; otherwise central differences
# (m_i depends only on mu_i, so a whole column moves at once). omega >= 0: when omega - h < 0
# the omega column uses the one-sided second-order difference (-3 m(omega) + 4 m(omega + h)
# - m(omega + 2 h)) / (2 h), which never evaluates a target at a negative variance (at
# omega = 0 it is the right derivative).
.rum_target_grad <- function(tg, mu, th, h = 1e-6) {
  n <- nrow(mu); p <- ncol(mu); nf <- .rum_th_nfac(th); ix <- .rum_th_idx(p, nf); n_th <- ix$n_th
  if (!is.null(tg$grad)) return(tg$grad(mu, th))
  if (identical(tg$kind, "mean"))
    return(list(m = tg$m(mu, th), gm = matrix(tg$c, n, p, byrow = TRUE), gt = matrix(0, n, n_th)))
  gm <- matrix(0, n, p)
  for (j in seq_len(p)) {
    m1 <- mu; m2 <- mu; m1[, j] <- m1[, j] + h; m2[, j] <- m2[, j] - h
    gm[, j] <- (tg$m(m1, th) - tg$m(m2, th)) / (2 * h)
  }
  v <- .rum_th_vec(th, nf); gt <- matrix(0, n, n_th)
  for (j in seq_len(n_th)) {
    v1 <- v; v2 <- v; v1[j] <- v1[j] + h; v2[j] <- v2[j] - h
    if (j == ix$omega && v2[j] < 0) {                # omega: stay in the domain
      v3 <- v; v3[j] <- v3[j] + 2 * h
      gt[, j] <- (-3 * tg$m(mu, th) + 4 * tg$m(mu, .rum_th_list(v1, p, nf)) -
                    tg$m(mu, .rum_th_list(v3, p, nf))) / (2 * h)
    } else gt[, j] <- (tg$m(mu, .rum_th_list(v1, p, nf)) - tg$m(mu, .rum_th_list(v2, p, nf))) / (2 * h)
  }
  list(m = tg$m(mu, th), gm = gm, gt = gt)
}

# ---- rotations of the loading ---------------------------------------------------------------
# The law of beta_i depends on Lambda only through Lambda Lambda', so Lambda and Lambda A
# (A orthogonal q x q) give the same model. The tangent directions of these rotations at
# Lambda are vec(Lambda A), A skew-symmetric; a basis is A = E_ab - E_ba (a < b), which moves
# column b by +Lambda[, a] and column a by -Lambda[, b]. .rum_rotation_dirs() returns these
# q (q - 1) / 2 directions as the columns of an n_total x q(q-1)/2 matrix in theta coordinates
# (kappa and omega entries 0; theta placed after offset other entries, n_total rows in all;
# by default n_total = offset + p q + 2). Columns are not normalised and may be linearly
# dependent when Lambda has rank below q (orthonormalise with qr() before projecting). q = 1:
# a matrix with no columns. Every target's exact theta-gradient is orthogonal to them.
.rum_rotation_dirs <- function(Lambda, offset = 0L, n_total = NULL) {
  Lambda <- as.matrix(Lambda); p <- nrow(Lambda); q <- ncol(Lambda)
  ix <- .rum_th_idx(p, q, offset); if (is.null(n_total)) n_total <- offset + ix$n_th
  pr <- if (q > 1L) utils::combn(q, 2L) else matrix(0L, 2L, 0L)
  out <- matrix(0, n_total, ncol(pr),
                dimnames = list(NULL, if (ncol(pr)) paste0("rot", pr[1L, ], "_", pr[2L, ])))
  for (k in seq_len(ncol(pr))) {
    a <- pr[1L, k]; b <- pr[2L, k]
    M <- matrix(0, p, q); M[, b] <- Lambda[, a]; M[, a] <- -Lambda[, b]
    out[ix$loading, k] <- as.vector(M)
  }
  out
}
# Remove the rotation components from the rows of g (columns in the coordinates of
# .rum_rotation_dirs(Lambda, offset, ncol(g))): g - g Q Q', Q an orthonormal basis of the
# directions. q = 1 (or Lambda = 0): g unchanged.
.rum_rotation_project <- function(g, Lambda, offset = 0L) {
  R <- .rum_rotation_dirs(Lambda, offset, ncol(g))
  if (!ncol(R)) return(g)
  qrR <- qr(R); if (!qrR$rank) return(g)
  Q <- qr.Q(qrR)[, seq_len(qrR$rank), drop = FALSE]
  g - (g %*% Q) %*% t(Q)
}
# Orthonormal basis (n x (n - r) matrix) of the complement of the column space of rot, the
# rotation directions (.rum_rotation_dirs) in some coordinates; r = qr(rot)$rank, which depends
# on Lambda alone (r < q (q - 1) / 2 only when Lambda has rank below q), not on any quadrature.
# NULL when rot has no columns or rank 0 (one factor): nothing to remove.
.rum_rotation_complement <- function(rot) {
  if (is.null(rot) || !ncol(rot)) return(NULL)
  qr_r <- qr(rot); k <- qr_r$rank
  if (!k) return(NULL)
  if (k == nrow(rot)) return(matrix(0, nrow(rot), 0L))
  qr.Q(qr_r, complete = TRUE)[, (k + 1L):nrow(rot), drop = FALSE]
}
# Principal axes of Lambda: Lambda V (V from svd(Lambda)), orthogonal columns with decreasing
# lengths; a rotation of Lambda gives the same axes (up to column signs, and up to rotations
# within a set of equal singular values).
.rum_principal_axes <- function(Lambda) {
  sv <- svd(Lambda); list(L = Lambda %*% sv$v, V = sv$v)
}

# ---- importance shares ----------------------------------------------------------------------
# Fitted-law importance share of attribute group g for respondent i:
#   m_i = E[V_g(b) / sum_h V_h(b)],  b = mu_i + Lambda u, u ~ N(0, I_q),
# V_h(b) = sum_{j in h} b_j^2 / L_h - centred (sum_{j in h} b_j / L_h)^2, L_h = |h| + 1 when
# centred (dummy coding with a reference level: the variance of the group's utility over a
# uniform menu of its levels), else L_h = 1 (orthonormal coding). This is CV_importance.R of the
# 2026-10-11 inference run (imp_sums, tgt_imp), moved into the package; with one factor and
# the default rule the arithmetic is the same. groups: list of column indices, one per group;
# each share's denominator sums every group.
# Integration. One factor: b = mu + lambda u over the 1-D rule uq (list(u, w); default the
# u-marginal of rum_quadrature(c(80, 41)), as CV). Unchanged.
# q > 1 factors (Codex review of 829393d, findings 2, 6, 7). b = mu + C z over the canonical
# axes C = Lambda V_r (.rum_canonical_axes: principal axes, ties resolved by the feature
# convention, so the coordinates are the same for every rotation of Lambda), restricted to the r
# axes with nonzero length (reduced rank: q > p, or Lambda of rank below q; the other axes do not
# move b). The share is bounded but not smooth at b = 0, and a fixed product rule can miss it by
# several points when the plane mu_i + span(Lambda) passes near 0 (Codex: mu = 0,
# Lambda = diag(1, .1) gives 0.953 on the old 32 x 32 rule against the exact 1/1.1). The default is
# therefore convergence-controlled randomised QMC (.rum_imp_sums_rqmc): n_rep independent Owen
# scramblings of a Sobol set in r dimensions (spacefillr, seeds seed + 1..n_rep; R's random state is
# not touched), each point z used with its mirror -z, n points per scrambling starting at n_start and
# doubling, respondent by respondent, until the standard error across the scramblings of every one
# of the respondent's shares is at most tol (default 1e-4), or n reaches n_max (then a warning).
# The value is the average over all draws; the attribute-free list element se (n x groups) holds
# the achieved standard errors and n_points the points per scrambling used. The exact gradient uses
# the same draws. The old product rule is kept only on request (uq or order_multi given), with a
# size check before the grid is built (.rum_imp_max_nodes).
# Exact gradients (as CV): d share_g / d B_j = dV_j (1{j in g} - share_g) / T, T = sum_h V_h;
# dm/dmu = E[that]; the loading adds the factor u. q > 1: dm/dLambda = G V_r', G = E[(d share /
# d B) z'] on the canonical axes (the pathwise gradient E[(d share / d b) u']: the axes left out
# do not move b and contribute 0), then its component along the rotation directions (zero for the
# exact integral) is removed (.rum_rotation_project). gt = (0, vec(dm/dLambda), 0).
.rum_imp_rule <- function() {
  q <- rum_quadrature(c(80L, 41L)); k <- round(q$u, 12); w <- tapply(q$weight, k, sum)
  list(u = as.numeric(names(w)), w = unname(w))
}
.rum_imp_order <- function(nf) if (nf == 2L) 32L else if (nf == 3L) 20L else 12L
.rum_imp_max_nodes <- 2e6
.rum_imp_control <- function(tol = 1e-4, n_start = 256L, n_max = 16384L, n_rep = 8L, seed = 20261114L)
  list(tol = tol, n_start = as.integer(n_start), n_max = as.integer(n_max), n_rep = as.integer(n_rep),
       seed = as.integer(seed))
# The canonical integration axes of a q > 1 loading: V (q x q), the indices of the axes with
# nonzero length, and C = Lambda V[, keep] (p x r).
.rum_imp_axes <- function(loading) {
  ax <- .rum_canonical_axes(loading); C <- loading %*% ax$V; len <- colSums(C^2)
  keep <- which(len > .rum_axes_tie_tol * max(len, 0))
  list(V = ax$V, keep = keep, C = C[, keep, drop = FALSE])
}
# Shares and their gradients at the draws OFF (K x p offsets C z) for the respondents' means MU
# (n x p): returns sh (n x ng sums over the K draws) and, when Zr (K x r draws) is given, the sums
# gm (n x p x ng) and gz (n x p x r x ng) of d share / d b and of (d share / d b) z_k.
.rum_imp_block <- function(MU, OFF, Zr, GR, L, cc, grp_of, ing, grad = TRUE) {
  n <- nrow(MU); pp <- ncol(MU); K <- nrow(OFF); ng <- length(GR); r <- if (is.null(Zr)) 0L else ncol(Zr)
  ii <- rep(seq_len(n), K); kk <- rep(seq_len(K), each = n)
  B <- MU[ii, , drop = FALSE] + OFF[kk, , drop = FALSE]
  Sg <- matrix(vapply(seq_len(ng), function(g) rowSums(B[, GR[[g]], drop = FALSE]), numeric(n * K)), n * K)
  Vg <- matrix(vapply(seq_len(ng), function(g) rowSums(B[, GR[[g]], drop = FALSE]^2) / L[g], numeric(n * K)), n * K) -
    cc * (Sg / matrix(L, n * K, ng, byrow = TRUE))^2
  Tt <- rowSums(Vg); sh <- Vg / Tt
  sumk <- function(x) matrix(x, n, K) %*% rep(1, K)                         # sum over draws
  out <- list(sh = matrix(vapply(seq_len(ng), function(g) drop(sumk(sh[, g])), numeric(n)), n))
  if (!grad) return(out)
  dV <- matrix(0, n * K, pp)
  dV[, ing] <- 2 * B[, ing, drop = FALSE] / matrix(L[grp_of[ing]], n * K, length(ing), byrow = TRUE) -
    cc * 2 * Sg[, grp_of[ing], drop = FALSE] / matrix(L[grp_of[ing]]^2, n * K, length(ing), byrow = TRUE)
  gm <- array(0, c(n, pp, ng)); gz <- array(0, c(n, pp, max(r, 1L), ng))
  for (g in seq_len(ng)) {
    gr <- -sh[, g] * dV; gr[, GR[[g]]] <- gr[, GR[[g]]] + dV[, GR[[g]]]; gr <- gr / Tt
    for (j in seq_len(pp)) {
      Gj <- matrix(gr[, j], n, K)
      gm[, j, g] <- Gj %*% rep(1, K)
      if (r) gz[, j, , g] <- Gj %*% Zr
    }
  }
  out$gm <- gm; out$gz <- gz
  out
}
# Map the axis block G (n x p x r: E[(d share / d b) z_k]) to the loading's coordinates,
# dm/dLambda[, l] = sum_k V[l, keep_k] G[, , k], n x (p q) column-major, rotation part removed.
.rum_imp_gl <- function(G, V, keep, loading) {
  n <- dim(G)[1L]; pp <- dim(G)[2L]; nf <- ncol(loading); out <- matrix(0, n, pp * nf)
  for (l in seq_len(nf)) for (k in seq_along(keep))
    out[, (l - 1L) * pp + seq_len(pp)] <- out[, (l - 1L) * pp + seq_len(pp)] + V[l, keep[k]] * matrix(G[, , k], n, pp)
  .rum_rotation_project(cbind(0, out, 0), loading)[, 1L + seq_len(pp * nf), drop = FALSE]
}
.rum_imp_setup <- function(groups, centred, pp) {
  GR <- groups; ng <- length(GR); grp_of <- integer(pp)
  L <- if (centred) vapply(GR, length, 0L) + 1 else rep(1, ng)
  for (g in seq_len(ng)) grp_of[GR[[g]]] <- g
  list(GR = GR, ng = ng, L = L, grp_of = grp_of, ing = which(grp_of > 0), cc = as.numeric(centred))
}
# Randomised QMC with convergence control (see above).
.rum_imp_sums_rqmc <- function(mu, loading, groups, centred, control = .rum_imp_control()) {
  n <- nrow(mu); pp <- ncol(mu); nf <- ncol(loading); S <- .rum_imp_setup(groups, centred, pp); ng <- S$ng
  ax <- .rum_imp_axes(loading); r <- length(ax$keep)
  ev <- function(MU, OFF, Zr) .rum_imp_block(MU, OFF, Zr, S$GR, S$L, S$cc, S$grp_of, S$ing)
  if (!r) {                                     # Lambda = 0: b = mu, no integral
    e <- ev(mu, matrix(0, 1L, pp), NULL)
    return(list(m = e$sh, gm = e$gm, gl = array(0, c(n, pp * nf, ng)), se = matrix(0, n, ng),
                n_points = rep(0L, n)))
  }
  R <- control$n_rep; tol <- control$tol
  M <- matrix(0, n, ng); GM <- array(0, c(n, pp, ng)); GZ <- array(0, c(n, pp, r, ng))
  SE <- matrix(NA_real_, n, ng); NP <- integer(n)
  Sr <- array(0, c(n, ng, R)); GMs <- array(0, c(n, pp, ng)); GZs <- array(0, c(n, pp, r, ng))
  active <- seq_len(n); Ncur <- 0L; Nt <- control$n_start
  chunk <- max(1L, floor(2e5 / max(1L, n)))
  repeat {
    for (rp in seq_len(R)) {
      z <- stats::qnorm(matrix(spacefillr::generate_sobol_owen_set(Nt, r, seed = control$seed + rp), Nt, r))
      z <- z[(Ncur + 1L):Nt, , drop = FALSE]; z <- rbind(z, -z)            # each point with its mirror
      for (cs in split(seq_len(nrow(z)), ceiling(seq_len(nrow(z)) / chunk))) {
        Zc <- z[cs, , drop = FALSE]; e <- ev(mu[active, , drop = FALSE], Zc %*% t(ax$C), Zc)
        Sr[active, , rp] <- Sr[active, , rp] + e$sh
        GMs[active, , ] <- GMs[active, , , drop = FALSE] + e$gm
        GZs[active, , , ] <- GZs[active, , , , drop = FALSE] + e$gz
      }
    }
    Ncur <- Nt; nd <- 2 * Ncur                   # draws per scrambling
    est <- Sr[active, , , drop = FALSE] / nd       # active x ng x R
    se <- matrix(apply(est, c(1L, 2L), stats::sd) / sqrt(R), length(active))
    done <- apply(se, 1L, max) <= tol
    if (Nt >= control$n_max) done[] <- TRUE
    fin <- active[done]
    if (length(fin)) {
      M[fin, ] <- matrix(apply(est[done, , , drop = FALSE], c(1L, 2L), mean), length(fin))
      GM[fin, , ] <- GMs[fin, , , drop = FALSE] / (nd * R); GZ[fin, , , ] <- GZs[fin, , , , drop = FALSE] / (nd * R)
      SE[fin, ] <- se[done, , drop = FALSE]; NP[fin] <- Ncur
    }
    active <- active[!done]
    if (!length(active)) break
    Nt <- min(2L * Nt, control$n_max)
  }
  bad <- which(apply(SE, 1L, max) > tol)
  if (length(bad))
    warning(sprintf(paste0("importance shares: %d respondent(s) did not reach the standard error %g at %d ",
      "points per scrambling (largest %.2g); the values carry that integration error"),
      length(bad), tol, control$n_max, max(SE)), call. = FALSE)
  GL <- array(0, c(n, pp * nf, ng))
  for (g in seq_len(ng)) GL[, , g] <- .rum_imp_gl(array(GZ[, , , g], c(n, pp, r)), ax$V, ax$keep, loading)
  list(m = M, gm = GM, gl = GL, se = SE, n_points = NP)
}
.rum_imp_sums <- function(mu, loading, groups, centred, uq = NULL, order_multi = NULL, wmin = 1e-14,
                          control = .rum_imp_control()) {
  multi <- .rum_multi_fac(loading)
  if (multi && is.null(uq) && is.null(order_multi))
    return(.rum_imp_sums_rqmc(mu, loading, groups, centred, control))
  GR <- groups; n <- nrow(mu); pp <- ncol(mu); ng <- length(GR); grp_of <- integer(pp)
  L <- if (centred) vapply(GR, length, 0L) + 1 else rep(1, ng)
  for (g in seq_len(ng)) grp_of[GR[[g]]] <- g
  ing <- which(grp_of > 0); cc <- as.numeric(centred)
  if (multi) {                                  # product rule, on request only (with a size check)
    nf <- ncol(loading); ax <- .rum_imp_axes(loading); rk <- length(ax$keep)
    if (is.null(order_multi)) order_multi <- .rum_imp_order(nf)
    r1 <- if (is.null(uq)) { r <- .rum_gh_rule(order_multi); list(u = r$z, w = r$w) } else uq
    if (length(r1$u)^max(rk, 1L) > .rum_imp_max_nodes)
      stop(sprintf("the importance product rule would have %.3g nodes (more than %g); use the default integration",
                   length(r1$u)^max(rk, 1L), .rum_imp_max_nodes), call. = FALSE)
    if (rk) {
      ix <- as.matrix(expand.grid(rep(list(seq_along(r1$u)), rk)))
      Z <- matrix(r1$u[ix], nrow(ix)); W <- apply(matrix(r1$w[ix], nrow(ix)), 1L, prod)
    } else { Z <- matrix(0, 1L, 0L); W <- 1 }
    keep <- W >= wmin; Z <- Z[keep, , drop = FALSE]; W <- W[keep]
    OFF <- Z %*% t(ax$C)                                   # node x p offsets C z
  } else {
    nf <- 1L; r1 <- if (is.null(uq)) .rum_imp_rule() else uq; W <- r1$w; Z <- matrix(r1$u)
  }
  M <- matrix(0, n, ng); GM <- array(0, c(n, pp, ng))
  GL <- array(0, c(n, pp * (if (multi) max(ncol(Z), 1L) else nf), ng))
  for (x in seq_along(W)) {
    if (W[x] < wmin) next
    B <- if (multi) mu + matrix(OFF[x, ], n, pp, byrow = TRUE) else mu + matrix(loading * Z[x, 1L], n, pp, byrow = TRUE)
    Sg <- vapply(seq_len(ng), function(g) rowSums(B[, GR[[g]], drop = FALSE]), numeric(n))
    Vg <- vapply(seq_len(ng), function(g) rowSums(B[, GR[[g]], drop = FALSE]^2) / L[g], numeric(n)) -
      cc * (Sg / matrix(L, n, ng, byrow = TRUE))^2
    Vg <- matrix(Vg, n); Sg <- matrix(Sg, n)
    Tt <- rowSums(Vg); sh <- Vg / Tt; dV <- matrix(0, n, pp)
    dV[, ing] <- 2 * B[, ing, drop = FALSE] / matrix(L[grp_of[ing]], n, length(ing), byrow = TRUE) -
      cc * 2 * Sg[, grp_of[ing], drop = FALSE] / matrix(L[grp_of[ing]]^2, n, length(ing), byrow = TRUE)
    M <- M + W[x] * sh
    for (g in seq_len(ng)) {
      gr <- -sh[, g] * dV; gr[, GR[[g]]] <- gr[, GR[[g]]] + dV[, GR[[g]]]; gr <- gr / Tt
      GM[, , g] <- GM[, , g] + W[x] * gr
      if (multi) for (k in seq_len(ncol(Z))) {
        cols <- (k - 1L) * pp + seq_len(pp)
        GL[, cols, g] <- GL[, cols, g] + (W[x] * Z[x, k]) * gr
      } else GL[, , g] <- GL[, , g] + W[x] * Z[x, 1L] * gr
    }
  }
  if (multi) {                                   # axis block G (n x p r) -> G V_r' (n x p q)
    GLq <- array(0, c(n, pp * nf, ng))
    for (g in seq_len(ng))
      GLq[, , g] <- .rum_imp_gl(array(matrix(GL[, , g], n), c(n, pp, max(ncol(Z), 1L)))[, , seq_len(ncol(Z)), drop = FALSE],
                                ax$V, ax$keep, loading)
    GL <- GLq
  }
  list(m = M, gm = GM, gl = GL)
}
# All importance-share targets of one set of groups, sharing one computation per (mu, th).
# names: one per group (default "importance: <group name>"); w: respondent weights.
.rum_tgt_importance_set <- function(groups, centred = FALSE, names = NULL, w = NULL, uq = NULL,
                                    order_multi = NULL, wmin = 1e-14, control = .rum_imp_control()) {
  force(groups); force(centred); force(uq); force(control)
  if (is.null(names)) names <- paste0("importance: ", if (is.null(base::names(groups)))
    seq_along(groups) else base::names(groups))
  cache <- new.env()
  sums <- function(mu, th) {
    if (!is.null(cache$mu) && identical(cache$mu, mu) && identical(cache$loading, th$loading)) return(cache$val)
    v <- .rum_imp_sums(mu, th$loading, groups, centred, uq, order_multi, wmin, control)
    cache$mu <- mu; cache$loading <- th$loading; cache$val <- v; v
  }
  lapply(seq_along(groups), function(g) {
    force(g)
    gf <- function(mu, th) {
      s <- sums(mu, th); n <- nrow(mu)
      out <- list(m = s$m[, g], gm = matrix(s$gm[, , g], n), gt = cbind(0, matrix(s$gl[, , g], n), 0))
      if (!is.null(s$se)) attr(out, "se") <- s$se[, g]      # q > 1: achieved integration SE per respondent
      out
    }
    list(name = names[g], kind = "importance", w = w, class = "importance",
         m = function(mu, th) gf(mu, th)$m, grad = gf)
  })
}
.rum_tgt_importance <- function(name, g, groups, centred = FALSE, w = NULL, uq = NULL,
                                order_multi = NULL, wmin = 1e-14, control = .rum_imp_control()) {
  nm <- rep(name, length(groups))
  .rum_tgt_importance_set(groups, centred, nm, w, uq, order_multi, wmin, control)[[g]]
}

# ---- decisiveness (APNS / MAPNS) -------------------------------------------------------------
# Model-implied decisiveness of an attribute group g (decisiveness build plan, 2026-11-16; Stage D1).
# For respondent i with taste b = mu_i + Lambda u (u ~ N(0, I_q)) and recorded task t (row x_t of
# data$d, profile 1 minus profile 2), the context is c_t(b) = x~_t' b, x~_t = x_t with the group's
# columns set to 0 (the other attributes as recorded; for BR a rate swap leaves the displayed revenue
# as recorded, since revenue is its own group), and the level utilities are a_l(b) = A_l' b_g over the
# group's menu A (L levels x |g| columns; default the baseline plus one dummy per column). Then
#   named swap (APNS, levels j vs k):  m_i = (1/T_i) sum_t E_u[ G(c_t + |D|) - G(c_t - |D|) ],
#                                      D = a_j(b) - a_k(b);
#   best-worst (MAPNS):                the same with |D| replaced by S = max over allowed ordered level
#                                      pairs (j, k) of a_j(b) - a_k(b), found within each taste draw
#                                      (without restrictions, the best minus the worst level),
# G the logistic cdf. The task shock is held fixed across the swap, so G(c + S) - G(c - S) is the
# probability that the swap flips the choice. Respondents are weighted equally after averaging over
# their own tasks (the engine's mean over m_i). Position terms: side = "zero" leaves kappa and the side
# preference out of the context (Jens's convention, decision E2); side = "fitted" adds
# kappa + sqrt(omega) v, v ~ N(0, 1), integrated as one more normal term of the context (below).
# allowed: per group NULL (any two levels), a logical vector over the levels (a swap may pair two
# levels when at least one is TRUE: Jens's GS governance rule with TRUE for the baseline and the
# generic levels), or a symmetric L x L logical matrix. pairs: per group a two-column matrix (or a
# length-2 vector) of levels (j, k); each row adds a named-swap target. best_worst: add the best-worst
# target of every group. A group whose menu has one level has swing 0 for every taste: its best-worst
# target is the constant 0 with zero gradient (const0); a multilevel menu whose restrictions leave no
# allowed pair is an error.
# Integration. Every integrand is smooth except where the maximising level pair changes (the swing's
# kinks; for a named swap, D = 0) and steep where the context moves fast (large loadings: the
# logistic transitions of c +/- S, Codex finding 2). q = 1: the adaptive rule .rum_dec_line in u:
# composite Gauss-Legendre (n_gl nodes) on panels of length `width` on [-u_max, u_max], split at each
# respondent's kinks (the breakpoints of the upper envelope of the lines a_j - a_k in u) and, where
# an argument c_t +/- S is steep, at the points where it crosses a ladder of levels around 0; each
# task is checked against n_gl / 2 nodes and its panels halved until the two agree to tol (1e-9).
# q > 1, named swap: (c_t, D) is bivariate normal; D = D0 + s_D z1 with the same adaptive rule in z1
# (kink at D = 0, transitions of c_t +/- |D| along z1) and c_t = c0 + r_t z1 + s_t z2 with a
# Gauss-Hermite rule of n_gh nodes in z2 (no kink in that direction; where s_t > s_switch the
# logistic is steep in z2 and the c-integrals are taken over the logistic shock with a
# logistic-weight Gauss rule of n_lg nodes instead, .rum_dec_inner). Values
# depend on Lambda only through Lambda' x~_t and Lambda' (A_j - A_k), so they
# are the same for every rotation of Lambda. q > 1, best-worst: randomised QMC with convergence
# control, as the importance shares (.rum_imp_sums_rqmc: Owen-scrambled Sobol on the canonical axes,
# mirrored, n_rep scramblings, doubling per respondent until the standard error is at most tol).
# Exact gradients, pathwise in u (valid almost everywhere; the integral smooths the kinks):
#   d psi = psi_c (x~_t' db) + psi_S (A_{j*} - A_{k*})' db_g,  psi_c = g(c + S) - g(c - S),
#   psi_S = g(c + S) + g(c - S), (j*, k*) the maximising pair at the draw, db = dmu + dLambda u, so
#   dm/dmu = E[d psi / d b] and dm/dLambda = E[(d psi / d b) u'] (q > 1 named swap: E[k u] =
#   w1 E[k z1] + w2_t E[k z2] with z = W u, W's rows orthonormal; best-worst: on the canonical axes,
#   mapped back with .rum_imp_gl). The loading block's rotation component is removed
#   (.rum_rotation_project). side = "fitted": the side term sqrt(omega) v is normal and enters the
#   context linearly, so it is integrated with the context (Codex review of 23cfc1c, finding 3: a
#   separate 21-node rule in v overstated the target at large omega): at q = 1 and for the named swap
#   inside the inner integral of .rum_dec_inner (for the named swap combined with the z2 part, sd
#   sqrt(s_t^2 + omega)); for best-worst at q > 1 as one more RQMC dimension. dm/dkappa = E[psi_c];
#   dm/domega = E[psi_cc] / 2 (Stein's identity) at every omega.
# The kind "decisiveness" is not in .rum_bounded_kinds (that waits for the coverage check, Stage D3).
.rum_dec_control <- function(width = 1, n_gl = 16L, u_max = 9, n_gh = 48L, n_lg = 48L, s_switch = 2,
                             n_v = 21L, swap = c("2d", "rqmc"), rqmc = .rum_imp_control(),
                             ladder = c(.5, 1, 2, 4, 8, 16, 32), tol = 1e-9, max_refine = 8L)
  # n_v: unused since the fix round (the side term is no longer integrated by its own node rule); kept
  # so that existing calls still run. The arguments of 23cfc1c keep their positions; those added in
  # the fix round (ladder, tol, max_refine) come after them (Codex re-review of 2bdf96a, item 6).
  list(width = width, n_gl = as.integer(n_gl), u_max = u_max, n_gh = as.integer(n_gh), n_lg = as.integer(n_lg),
       s_switch = s_switch, ladder = ladder, tol = tol, max_refine = as.integer(max_refine),
       n_v = as.integer(n_v), swap = match.arg(swap), rqmc = rqmc)
# Gauss-Legendre rule on [-1, 1] (Golub-Welsch).
.rum_gl_rule <- function(n) {
  if (n == 1L) return(list(x = 0, w = 2))
  k <- seq_len(n - 1L); b <- k / sqrt(4 * k^2 - 1)
  J <- matrix(0, n, n); J[cbind(k, k + 1L)] <- b; J[cbind(k + 1L, k)] <- b
  e <- eigen(J, symmetric = TRUE); o <- order(e$values)
  list(x = e$values[o], w = 2 * e$vectors[1L, o]^2)
}
# Breakpoints in (lo, hi) of the upper envelope of the lines b + s u (one respondent). The start is
# the exact maximiser at lo (ties to the steepest line), with no absolute tie tolerance (Codex
# re-review of 2bdf96a, item 5: a tolerance of 1e-12 on the unit scale treated the two lines of a
# tiny swing, e.g. +/- 1e-14 (u - 0.3), as tied and dropped their kink). If rounding at lo picks a
# line that is in fact just below a steeper one, that steeper line's crossing lies at or before lo,
# so the first step below switches to it without a breakpoint.
.rum_dec_envelope <- function(b, s, lo, hi) {
  v <- b + s * lo; top <- which(v == max(v))
  cur <- top[which.max(s[top])]; u0 <- lo; out <- numeric(0)
  repeat {
    up <- which(s > s[cur]); if (!length(up)) break
    x <- pmax((b[cur] - b[up]) / (s[up] - s[cur]), u0)
    nx <- min(x); if (!(nx < hi)) break
    cand <- up[x <= nx]; cur <- cand[which.max(s[cand])]
    if (nx > u0) out <- c(out, nx)
    u0 <- nx
  }
  out
}
# Adaptive one-dimensional rule shared by q = 1 and the D direction of the q > 1 named swap (Codex
# review of 23cfc1c, finding 2: panels split only at the swing's kinks missed narrow context
# transitions). Integrates over t ~ N(0, 1), per task k of respondent owner[k], the flip terms at
#   S(t) = max_p (b[owner, p] + s_p t)  (the swing: upper envelope of the pair lines)
#   c(t) = c0_k + r_k t + sw_k w,        w ~ N(0, 1) independent of t (sw_k = 0: no w),
# the w integral by .rum_dec_inner. Panels: a grid of length width on [-u_max, u_max], split at the
# respondent's kinks, and where an argument c0 + r t +/- S(t) (linear on each envelope segment) is
# steep (|slope| width > max(1, sw) / 2) also at the points where it equals max(1, sw) x
# (0, +/- ladder): the logistic transition is then covered by panels a few of its scales wide whatever
# the slope. Error control: each task is integrated with n_gl and n_gl / 2 Gauss-Legendre nodes per
# panel; where any accumulated term differs by more than tol, every panel of that task is halved,
# up to max_refine times (a warning if a task still differs). Returns per task (N rows): E0 = E[psi],
# Ec = E[psi_c], Ect = E[psi_c t], Ecw = E[psi_c w], Ecc = E[psi_cc] (need2), and per task and pair
# (N x P): ES = E[psi_S 1{p*}], ESt = E[psi_S t 1{p*}], ESw = E[psi_S w 1{p*}], p* the maximising pair.
# kinks (optional, length n): one more known breakpoint per respondent (NA for none), added to the
# envelope's; a named swap passes its zero of the swing, -D0 / sf, so that its kink is split whatever
# the swing's scale (Codex re-review of 2bdf96a, item 5).
.rum_dec_line <- function(b, s, owner, c0, r, sw, ctrl, need2 = FALSE, kinks = NULL) {
  n <- nrow(b); P <- ncol(b); N <- length(owner); U <- ctrl$u_max
  kn <- lapply(seq_len(n), function(i) {
    k <- .rum_dec_envelope(b[i, ], s, -U, U)
    if (!is.null(kinks) && is.finite(kinks[i])) { k <- sort(c(k, kinks[i])); k <- k[c(TRUE, diff(k) > 1e-12)] }
    k[k > -U & k < U]
  })
  e <- lapply(kn, function(k) c(-U, k, U)); ns <- lengths(e) - 1L
  sr <- rep.int(seq_len(n), ns)
  slo <- unlist(lapply(e, function(x) x[-length(x)]), use.names = FALSE)
  shi <- unlist(lapply(e, function(x) x[-1L]), use.names = FALSE)
  spair <- max.col(b[sr, , drop = FALSE] + outer((slo + shi) / 2, s), ties.method = "first")
  sig <- pmax(1, sw); lad <- c(-rev(ctrl$ladder), 0, ctrl$ladder)
  first <- c(0L, cumsum(ns))[seq_len(n)]
  ts <- rep.int(seq_len(N), ns[owner]); sg <- sequence(ns[owner], from = first[owner] + 1L)
  lt <- list(); lv <- list()
  for (sgn in c(1, -1)) {
    A <- c0[ts] + sgn * b[cbind(sr[sg], spair[sg])]; K <- r[ts] + sgn * s[spair[sg]]
    ii <- which(abs(K) * ctrl$width > sig[ts] / 2)
    if (length(ii)) {
      tt <- (outer(sig[ts[ii]], lad) - A[ii]) / K[ii]
      ok <- tt > slo[sg[ii]] & tt < shi[sg[ii]]
      lt[[length(lt) + 1L]] <- rep(ts[ii], length(lad))[ok]; lv[[length(lv) + 1L]] <- tt[ok]
    }
  }
  g0 <- seq(-U, U, by = ctrl$width); if (g0[length(g0)] < U) g0 <- c(g0, U)
  et <- c(rep(seq_len(N), each = length(g0)), rep.int(seq_len(N), lengths(kn)[owner]), unlist(lt))
  ev <- c(rep(g0, N), unlist(kn[owner], use.names = FALSE), unlist(lv))
  o <- order(et, ev); et <- et[o]; ev <- ev[o]
  keep <- c(TRUE, diff(et) != 0L | diff(ev) > 1e-12); et <- et[keep]; ev <- ev[keep]
  nx <- length(et); same <- et[-1L] == et[-nx]
  pt <- et[-nx][same]; pa <- ev[-nx][same]; pb <- ev[-1L][same]
  rz <- .rum_normal_rule(ctrl$n_gh); rz <- list(x = rz$nodes, w = rz$weights)
  re <- if (any(sw > ctrl$s_switch)) .rum_logis_rule(ctrl$n_lg) else NULL
  nc <- 5L + 3L * P
  acc_of <- function(sel, rule) {
    out <- matrix(0, N, nc); G <- length(rule$x)
    bsz <- max(1L, floor((if (any(sw[pt[sel]] > 0)) 4e4 else 1e6) / G))
    for (bs in split(sel, ceiling(seq_along(sel) / bsz))) {
      tk <- pt[bs]; mid <- (pa[bs] + pb[bs]) / 2; half <- (pb[bs] - pa[bs]) / 2
      pair <- max.col(b[owner[tk], , drop = FALSE] + outer(mid, s), ties.method = "first")
      x <- rep(mid, each = G) + rep(half, each = G) * rule$x
      w <- rep(half, each = G) * rule$w * stats::dnorm(x)
      tn <- rep(tk, each = G); pn <- rep(pair, each = G)
      S <- b[cbind(owner[tn], pn)] + s[pn] * x; cc <- c0[tn] + r[tn] * x; sv <- sw[tn]
      M <- length(x); psi <- pc <- pS <- pcw <- pSw <- pcc <- numeric(M); d <- sv == 0
      if (any(d)) {
        j <- which(d); ps <- .rum_dec_psi(cc[j], S[j], need2)
        psi[j] <- ps$psi; pc[j] <- ps$pc; pS[j] <- ps$pS; if (need2) pcc[j] <- ps$pcc
      }
      if (!all(d)) {
        j <- which(!d)
        Ip <- .rum_dec_inner(cc[j] + S[j], sv[j], rz, re, ctrl$s_switch, need2)
        Im <- .rum_dec_inner(cc[j] - S[j], sv[j], rz, re, ctrl$s_switch, need2)
        psi[j] <- Ip$I0 - Im$I0; pc[j] <- Ip$I1 - Im$I1; pS[j] <- Ip$I1 + Im$I1
        pcw[j] <- Ip$Iz - Im$Iz; pSw[j] <- Ip$Iz + Im$Iz; if (need2) pcc[j] <- Ip$I2 - Im$I2
      }
      a1 <- rowsum(cbind(w * psi, w * pc, w * pc * x, w * pcw, w * pcc), tn, reorder = TRUE)
      ix <- as.integer(rownames(a1)); out[ix, 1:5] <- out[ix, 1:5] + a1
      a2 <- rowsum(cbind(w * pS, w * pS * x, w * pSw), (tn - 1L) * P + pn, reorder = TRUE)
      gi <- as.integer(rownames(a2)) - 1L; ti <- gi %/% P + 1L; pj <- gi %% P + 1L
      for (k in 0:2) { cx <- cbind(ti, 5L + k * P + pj); out[cx] <- out[cx] + a2[, k + 1L] }
    }
    out
  }
  hi <- .rum_gl_rule(ctrl$n_gl); lo <- .rum_gl_rule(max(1L, ctrl$n_gl %/% 2L))
  res <- matrix(0, N, nc); todo <- seq_len(N); sel <- seq_along(pt); lev <- 0L
  repeat {
    A1 <- acc_of(sel, hi); A0 <- acc_of(sel, lo); res[todo, ] <- A1[todo, , drop = FALSE]
    dif <- abs(A1[todo, , drop = FALSE] - A0[todo, , drop = FALSE])
    bad <- todo[do.call(pmax, c(lapply(seq_len(nc), function(k) dif[, k]), list(0))) > ctrl$tol]
    if (!length(bad) || lev >= ctrl$max_refine) break
    lev <- lev + 1L; isb <- pt %in% bad; mid <- (pa[isb] + pb[isb]) / 2
    pt <- c(pt[!isb], pt[isb], pt[isb]); pa2 <- c(pa[!isb], pa[isb], mid); pb <- c(pb[!isb], mid, pb[isb]); pa <- pa2
    sel <- which(pt %in% bad); todo <- bad
  }
  if (length(bad) && lev >= ctrl$max_refine)
    warning(sprintf("decisiveness: %d task(s) did not reach the integration tolerance %g after %d refinements",
                    length(bad), ctrl$tol, lev), call. = FALSE)
  cP <- function(k) res[, 5L + k * P + seq_len(P), drop = FALSE]
  list(E0 = res[, 1L], Ec = res[, 2L], Ect = res[, 3L], Ecw = res[, 4L], Ecc = res[, 5L],
       ES = cP(0L), ESt = cP(1L), ESw = cP(2L), refinements = lev)
}
# Task layout of the data: rows of data$d by respondent, owner, T, and the context design per group.
.rum_dec_layout <- function(data) {
  n <- data$n; Tn <- as.integer(data$T)
  if (any(Tn < 1L)) stop("decisiveness: every respondent needs at least one recorded task")
  rows <- unlist(lapply(seq_len(n), function(i) data$offsets[i] + seq_len(Tn[i])))
  list(n = n, p = data$p, Tn = Tn, owner = rep.int(seq_len(n), Tn), X = data$d[rows, , drop = FALSE])
}
# Specification of every target: group columns, menu, ordered level pairs P (hi, lo) and their
# feature-space differences Dl (one row per pair, p columns).
.rum_dec_specs <- function(p, groups, menus, pairs, allowed, best_worst, names) {
  gn <- if (is.null(base::names(groups))) as.character(seq_along(groups)) else base::names(groups)
  specs <- list()
  for (g in seq_along(groups)) {
    cc <- as.integer(groups[[g]])
    A <- if (!is.null(menus) && !is.null(menus[[g]])) as.matrix(menus[[g]]) else rbind(0, diag(length(cc)))
    if (ncol(A) != length(cc)) stop(sprintf("decisiveness: the menu of group %s needs %d columns", gn[g], length(cc)))
    L <- nrow(A); lab <- if (!is.null(rownames(A))) rownames(A) else as.character(seq_len(L))
    al <- if (is.null(allowed)) NULL else allowed[[g]]
    ok <- if (is.null(al)) matrix(TRUE, L, L) else if (is.matrix(al)) {
      if (!all(dim(al) == L)) stop("decisiveness: an allowed matrix must be L x L"); al | t(al)
    } else { if (length(al) != L) stop("decisiveness: an allowed vector needs one entry per level")
      outer(as.logical(al), as.logical(al), `|`) }
    diag(ok) <- FALSE
    mk <- function(P, type, name) {
      Dl <- matrix(0, nrow(P), p); Dl[, cc] <- A[P[, 1L], , drop = FALSE] - A[P[, 2L], , drop = FALSE]
      list(name = name, group = gn[g], cols = cc, menu = A, P = P, Dl = Dl, type = type)
    }
    if (best_worst) {
      P <- which(ok, arr.ind = TRUE); P <- P[order(P[, 2L], P[, 1L]), , drop = FALSE]; colnames(P) <- c("hi", "lo")
      if (L == 1L) {                          # one level: max(a) - min(a) = 0, a constant-zero target
        s0 <- mk(P, "best_worst", paste0("decisiveness: ", gn[g])); s0$const0 <- TRUE
        specs[[length(specs) + 1L]] <- s0
      } else {
        if (!nrow(P)) stop(sprintf("decisiveness: group %s has no allowed level pair", gn[g]))
        specs[[length(specs) + 1L]] <- mk(P, "best_worst", paste0("decisiveness: ", gn[g]))
      }
    }
    pr <- if (is.null(pairs)) NULL else if (!is.null(base::names(pairs))) pairs[[gn[g]]] else pairs[[g]]
    if (!is.null(pr)) {
      pr <- matrix(as.integer(pr), ncol = 2L)
      for (r in seq_len(nrow(pr))) {
        j <- pr[r, 1L]; k <- pr[r, 2L]
        if (j == k || min(j, k) < 1L || max(j, k) > L) stop("decisiveness: a named swap needs two different levels")
        specs[[length(specs) + 1L]] <- mk(rbind(c(j, k), c(k, j)), "swap",
                                          sprintf("decisiveness: %s %s vs %s", gn[g], lab[j], lab[k]))
      }
    }
  }
  if (!is.null(names)) {
    if (length(names) != length(specs)) stop("decisiveness: names needs one entry per target")
    for (s in seq_along(specs)) specs[[s]]$name <- names[s]
  }
  specs
}
# Flip terms at (c, S): psi, psi_c, psi_S (and psi_cc when need2).
.rum_dec_psi <- function(c, S, need2 = FALSE) {
  Gp <- stats::plogis(c + S); Gm <- stats::plogis(c - S); gp <- Gp * (1 - Gp); gm <- Gm * (1 - Gm)
  out <- list(psi = Gp - Gm, pc = gp - gm, pS = gp + gm)
  if (need2) out$pcc <- gp * (1 - 2 * Gp) - gm * (1 - 2 * Gm)
  out
}
# q = 1, any spec: the adaptive rule .rum_dec_line in u (sv: sd of an extra normal term in the
# context). Returns m, gm (n x p), gl (n x p), gs = dm/dshift, gss = d2m/dshift2 (need2).
.rum_dec_q1 <- function(mu, lam, sp, lay, shift, ctrl, need2 = FALSE, sv = 0) {
  owner <- lay$owner; Tn <- lay$Tn
  Xt <- lay$X; Xt[, sp$cols] <- 0
  c0 <- rowSums(Xt * mu[owner, , drop = FALSE]) + shift; c1 <- drop(Xt %*% lam)
  b <- mu %*% t(sp$Dl); s <- drop(sp$Dl %*% lam)
  # a named swap: split at the swing's zero -b1 / s1 whenever s1 != 0 (Codex re-review, item 5)
  kz <- if (sp$type == "swap" && s[1L] != 0) -b[, 1L] / s[1L] else NULL
  A <- .rum_dec_line(b, s, owner, c0, c1, rep(sv, length(owner)), ctrl, need2, kinks = kz)
  rs <- function(x) rowsum(x, owner, reorder = TRUE) / Tn
  list(m = drop(rs(A$E0)), gm = unname(rs(A$Ec * Xt + A$ES %*% sp$Dl)),
       gl = unname(rs(A$Ect * Xt + A$ESt %*% sp$Dl)), gs = drop(rs(A$Ec)),
       gss = if (need2) drop(rs(A$Ecc)) else NULL)
}
# Gauss rule for the logistic density (n nodes), by the discretised Stieltjes procedure on a trapezoid
# grid of M points on [-L, L] (the grid integrates the rule's polynomials to rounding).
.rum_logis_rule <- function(n, M = 40001L, L = 60) {
  x <- seq(-L, L, length.out = M); w <- stats::dlogis(x); w <- w / sum(w)
  a <- b <- numeric(n); p0 <- rep(0, M); p1 <- rep(1, M); nrm0 <- 1
  for (k in seq_len(n)) {
    nrm <- sum(w * p1^2); a[k] <- sum(w * x * p1^2) / nrm; b[k] <- if (k == 1L) 1 else nrm / nrm0
    p2 <- (x - a[k]) * p1 - (if (k == 1L) 0 else b[k]) * p0
    sc <- sqrt(sum(w * p2^2)); p0 <- p1 / sc; p1 <- p2 / sc; nrm0 <- nrm / sc^2
  }
  J <- diag(a, n); if (n > 1L) { k <- seq_len(n - 1L); J[cbind(k, k + 1L)] <- J[cbind(k + 1L, k)] <- sqrt(b[k + 1L]) }
  e <- eigen(J, symmetric = TRUE); o <- order(e$values)
  list(x = e$values[o], w = e$vectors[1L, o]^2)
}
# The c-direction integrals of the named swap, z ~ N(0, 1), for vectors a and s >= 0:
#   I0 = E[G(a + s z)], I1 = E[g(a + s z)], Iz = E[g(a + s z) z], I2 = E[g'(a + s z)] (need2).
# s <= s_switch: Gauss-Hermite in z (rz). Larger s: the logistic is steep in z, so the same integrals
# are taken over the logistic shock instead, E[G(a + s z)] = E_eps[Phi((a + eps) / s)], with the
# logistic-weight Gauss rule re (I1, Iz and I2 are its a- and s-derivatives). Both rules reach about
# 1e-9 at s = 2 with 48 nodes and better on either side.
.rum_dec_inner <- function(a, s, rz, re, s_switch, need2 = FALSE) {
  n <- length(a); I0 <- I1 <- Iz <- I2 <- numeric(n); lo <- s <= s_switch
  if (any(lo)) {
    i <- which(lo); G <- stats::plogis(a[i] + outer(s[i], rz$x)); g <- G * (1 - G)
    I0[i] <- drop(G %*% rz$w); I1[i] <- drop(g %*% rz$w); Iz[i] <- drop(g %*% (rz$w * rz$x))
    if (need2) I2[i] <- drop((g * (1 - 2 * G)) %*% rz$w)
  }
  if (any(!lo)) {
    i <- which(!lo); si <- s[i]; tt <- (a[i] + outer(rep(1, length(i)), re$x)) / si; ph <- stats::dnorm(tt)
    I0[i] <- drop(stats::pnorm(tt) %*% re$w); I1[i] <- drop(ph %*% re$w) / si
    pt <- drop((ph * tt) %*% re$w); Iz[i] <- -pt / si
    if (need2) I2[i] <- -pt / si^2
  }
  list(I0 = I0, I1 = I1, Iz = Iz, I2 = if (need2) I2 else NULL)
}
# q > 1, named swap: (c_t, D) is bivariate normal. D = D0 + sf z1 (z1 = w1' u, w1 = f / sf, f =
# Lambda' delta) and c_t = c0 + r_t z1 + s_t z2 (z2 the unit direction of Lambda' x~_t orthogonal to
# w1, s_t its length): the adaptive rule .rum_dec_line in z1 (the pair lines D and -D, kink at
# D = 0), with the z2 integral (and an extra normal term of sd sv) inside it by .rum_dec_inner.
# E[k u] = w1 E[k z1] + (Hp_t / s_t) E[k z2]. Returns m, gm, gl (n x p q, rotation part removed),
# gs, gss.
.rum_dec_swap2 <- function(mu, Lam, sp, lay, shift, ctrl, need2 = FALSE, sv = 0) {
  n <- lay$n; p <- lay$p; q <- ncol(Lam); owner <- lay$owner; Tn <- lay$Tn
  Xt <- lay$X; Xt[, sp$cols] <- 0; del <- sp$Dl[1L, ]
  c0 <- rowSums(Xt * mu[owner, , drop = FALSE]) + shift; H <- Xt %*% Lam
  f <- drop(crossprod(Lam, del)); sf <- sqrt(sum(f^2)); D0 <- drop(mu %*% del)
  # no relative thresholds (Codex finding 1): D moves with u whenever sf > 0, and the context's part
  # orthogonal to the swap direction is formed directly (no cancellation in |H|^2 - r^2)
  w1 <- if (sf > 0) f / sf else numeric(q); rr <- drop(H %*% w1)
  Hp <- H - outer(rr, w1); sp2 <- sqrt(rowSums(Hp^2)); sw <- sqrt(sp2^2 + sv^2)
  # split at the swing's zero -D0 / sf whenever sf > 0 (Codex re-review of 2bdf96a, item 5)
  A <- .rum_dec_line(cbind(D0, -D0), c(sf, -sf), owner, c0, rr, sw, ctrl, need2,
                     kinks = if (sf > 0) -D0 / sf else NULL)
  # E[k z2] = (sp2 / sw) E[k w] (w the combined normal), so E[k u] = w1 E[k z1] + (Hp / sw) E[k w]
  Dl2 <- rbind(del, -del); Wt <- Hp / ifelse(sw > 0, sw, 1)
  rs <- function(x) rowsum(x, owner, reorder = TRUE) / Tn
  SD <- A$ESt %*% Dl2; SW <- A$ESw %*% Dl2; gl <- matrix(0, n, p * q)
  for (l in seq_len(q))
    gl[, (l - 1L) * p + seq_len(p)] <- rs((w1[l] * A$Ect + Wt[, l] * A$Ecw) * Xt + w1[l] * SD + Wt[, l] * SW)
  gl <- .rum_rotation_project(cbind(0, gl, 0), Lam)[, 1L + seq_len(p * q), drop = FALSE]
  list(m = drop(rs(A$E0)), gm = unname(rs(A$Ec * Xt + A$ES %*% Dl2)), gl = unname(gl), gs = drop(rs(A$Ec)),
       gss = if (need2) drop(rs(A$Ecc)) else NULL)
}
# Integration axes of the decisiveness RQMC: Lambda's canonical axes (.rum_canonical_axes), keeping
# every axis of nonzero length. .rum_imp_axes (importance) drops axes shorter than a relative
# threshold, which here would drop a small but nonzero swing (Codex review of 23cfc1c, finding 1).
# An axis of length exactly 0 leaves the integrand unchanged along it, so dropping it changes neither
# the value nor the gradient (E[(d psi / d b) z_k] = 0 when psi does not depend on z_k).
.rum_dec_axes <- function(Lam) {
  ax <- .rum_canonical_axes(Lam); C <- Lam %*% ax$V; keep <- which(colSums(C^2) > 0)
  list(V = ax$V, keep = keep, C = C[, keep, drop = FALSE])
}
# q > 1, best-worst (or a named swap by request): randomised QMC with convergence control on the
# canonical axes (.rum_dec_axes), the scheme of .rum_imp_sums_rqmc. Returns m, gm, gl, gs, gss, se, n_points.
# V (tests only): hold the integration axes fixed at V (q x q, full rank) instead of Lambda's canonical
# axes, so that a finite difference in Lambda does not also rotate the draws.
# sv > 0 (side = "fitted"): the side term sv v enters the context, v one more Sobol dimension (Codex
# finding 3); dm/domega then comes from gss = E[psi_cc] (Stein), so need2 is set by the caller.
.rum_dec_rqmc <- function(mu, Lam, sp, lay, shift, ctrl, need2 = FALSE, V = NULL, sv = 0) {
  n <- lay$n; p <- lay$p; nf <- ncol(Lam); owner <- lay$owner; Tn <- lay$Tn; N <- length(owner)
  control <- ctrl$rqmc; Xt <- lay$X; Xt[, sp$cols] <- 0
  c0 <- rowSums(Xt * mu[owner, , drop = FALSE]) + shift; b <- mu %*% t(sp$Dl); nP <- nrow(sp$Dl)
  ax <- if (is.null(V)) .rum_dec_axes(Lam) else list(V = V, keep = seq_len(ncol(V)), C = Lam %*% V)
  r <- length(ax$keep)
  if (!r) {                                  # Lambda = 0: no taste integral; the side term by the line rule
    A <- .rum_dec_line(b, numeric(nP), owner, c0, numeric(N), rep(sv, N), ctrl, need2)
    rs <- function(x) rowsum(x, owner, reorder = TRUE) / Tn
    return(list(m = drop(rs(A$E0)), gm = unname(rs(A$Ec * Xt + A$ES %*% sp$Dl)), gl = matrix(0, n, p * nf),
                gs = drop(rs(A$Ec)), gss = if (need2) drop(rs(A$Ecc)) else NULL, se = numeric(n),
                n_points = rep(0L, n)))
  }
  Ht <- Xt %*% ax$C; DC <- sp$Dl %*% ax$C; dd <- r + (sv > 0)
  R <- control$n_rep; tol <- control$tol
  Sr <- matrix(0, n, R); Ec <- numeric(N); EcZ <- matrix(0, N, r); Ecc <- numeric(N)
  Wp <- matrix(0, n, nP); WpZ <- array(0, c(n, nP, r))
  M <- SE <- rep(NA_real_, n); NP <- integer(n)
  active <- seq_len(n); Ncur <- 0L; Nt <- control$n_start
  repeat {
    ta <- which(owner %in% active); oa <- match(owner[ta], active); Ta <- Tn[active]; na <- length(active)
    chunk <- max(1L, floor(2e6 / max(1L, length(ta))))
    for (rp in seq_len(R)) {
      z <- stats::qnorm(matrix(spacefillr::generate_sobol_owen_set(Nt, dd, seed = control$seed + rp), Nt, dd))
      z <- z[(Ncur + 1L):Nt, , drop = FALSE]; z <- rbind(z, -z)
      for (cs in split(seq_len(nrow(z)), ceiling(seq_len(nrow(z)) / chunk))) {
        Zc <- z[cs, seq_len(r), drop = FALSE]; Kc <- nrow(Zc)
        S <- NULL; arg <- NULL
        for (pi in seq_len(nP)) {
          v <- b[active, pi] + matrix(drop(Zc %*% DC[pi, ]), na, Kc, byrow = TRUE)
          if (is.null(S)) { S <- v; arg <- matrix(1L, na, Kc) } else { up <- v > S; S[up] <- v[up]; arg[up] <- pi }
        }
        cm <- c0[ta] + Ht[ta, , drop = FALSE] %*% t(Zc)
        if (dd > r) cm <- cm + matrix(sv * z[cs, dd], length(ta), Kc, byrow = TRUE)
        ps <- .rum_dec_psi(cm, S[oa, , drop = FALSE], need2)
        Sr[active, rp] <- Sr[active, rp] + rowSums(rowsum(ps$psi, oa, reorder = TRUE)) / Ta
        Ec[ta] <- Ec[ta] + rowSums(ps$pc); EcZ[ta, ] <- EcZ[ta, ] + ps$pc %*% Zc
        if (need2) Ecc[ta] <- Ecc[ta] + rowSums(ps$pcc)
        sS <- rowsum(ps$pS, oa, reorder = TRUE) / Ta                   # na x Kc
        for (pi in unique(as.vector(arg))) {
          sm <- sS * (arg == pi)
          Wp[active, pi] <- Wp[active, pi] + rowSums(sm)
          WpZ[active, pi, ] <- WpZ[active, pi, ] + sm %*% Zc
        }
      }
    }
    Ncur <- Nt; nd <- 2 * Ncur
    est <- Sr[active, , drop = FALSE] / nd
    se <- apply(est, 1L, stats::sd) / sqrt(R)
    done <- se <= tol; if (Nt >= control$n_max) done[] <- TRUE
    fin <- active[done]
    if (length(fin)) { M[fin] <- rowMeans(est[done, , drop = FALSE]); SE[fin] <- se[done]; NP[fin] <- Ncur }
    active <- active[!done]
    if (!length(active)) break
    Nt <- min(2L * Nt, control$n_max)
  }
  bad <- which(SE > tol)
  if (length(bad))
    warning(sprintf(paste0("decisiveness: %d respondent(s) did not reach the standard error %g at %d ",
      "points per scrambling (largest %.2g); the values carry that integration error"),
      length(bad), tol, control$n_max, max(SE)), call. = FALSE)
  tot <- 2 * NP * R                                   # draws per respondent
  Ec <- Ec / tot[owner]; EcZ <- EcZ / tot[owner]; Ecc <- Ecc / tot[owner]; Wp <- Wp / tot; WpZ <- WpZ / tot
  gm <- rowsum(Ec * Xt, owner, reorder = TRUE) / Tn + Wp %*% sp$Dl
  G <- array(0, c(n, p, r))
  for (k in seq_len(r))
    G[, , k] <- rowsum(EcZ[, k] * Xt, owner, reorder = TRUE) / Tn + matrix(WpZ[, , k], n, nP) %*% sp$Dl
  list(m = M, gm = unname(gm), gl = .rum_imp_gl(G, ax$V, ax$keep, Lam),
       gs = drop(rowsum(Ec, owner, reorder = TRUE)) / Tn,
       gss = if (need2) drop(rowsum(Ecc, owner, reorder = TRUE)) / Tn else NULL, se = SE, n_points = NP)
}
# One spec at (mu, th): dispatch on q and the spec type, integrate the side term when fitted, and
# return list(m, gm, gt) (gt = (dm/dkappa, dm/dLambda, dm/domega)) with the attribute "se" for RQMC.
.rum_dec_eval <- function(sp, mu, th, lay, side, ctrl) {
  multi <- .rum_multi_fac(th$loading)
  lam <- if (multi) th$loading else as.vector(th$loading)
  if (isTRUE(sp$const0)) {                   # one-level attribute: value and every derivative are 0
    n <- lay$n; q <- if (multi) ncol(lam) else 1L
    out <- list(m = numeric(n), gm = matrix(0, n, lay$p), gt = matrix(0, n, lay$p * q + 2L))
    if (multi) attr(out, "se") <- numeric(n)
    return(out)
  }
  fn <- if (!multi) .rum_dec_q1 else if (sp$type == "swap" && ctrl$swap == "2d") .rum_dec_swap2 else .rum_dec_rqmc
  # side = "fitted": kappa shifts the context and sqrt(omega) v is one more normal term in it,
  # integrated with the rest (Codex finding 3; no separate node rule in v). dm/dkappa = E[psi_c] and
  # dm/domega = E[psi_cc] / 2 (Stein's identity, at every omega).
  fit <- side == "fitted"
  e <- fn(mu, lam, sp, lay, if (fit) th$kappa else 0, ctrl, need2 = fit, sv = if (fit) sqrt(th$omega) else 0)
  out <- list(m = e$m, gm = e$gm, gt = if (fit) cbind(e$gs, e$gl, e$gss / 2) else cbind(0, e$gl, 0))
  if (!is.null(e$se)) attr(out, "se") <- e$se
  out
}
# All decisiveness targets of one design, sharing one layout and a cache per (mu, th). data: the
# rum_data the fitted states belong to (rows of mu in data$ids order). groups: named list of column
# indices; menus, pairs, allowed: per group (see above). Each target: list(name, kind
# "decisiveness", w, class, group, type, m, grad). The cache key holds mu, the loading WITH its shape
# (dim), and kappa and omega when side = "fitted"; a different key empties the cache.
.rum_tgt_decisiveness_set <- function(data, groups, menus = NULL, pairs = NULL, allowed = NULL,
                                      side = c("zero", "fitted"), best_worst = TRUE, names = NULL,
                                      w = NULL, control = .rum_dec_control()) {
  side <- match.arg(side); lay <- .rum_dec_layout(data)
  specs <- .rum_dec_specs(lay$p, groups, menus, pairs, allowed, best_worst, names)
  force(control); ns <- length(specs); cache <- new.env(); cache$key <- NULL; cache$val <- vector("list", ns)
  value <- function(s, mu, th) {
    if (!is.matrix(mu) || nrow(mu) != lay$n || ncol(mu) != lay$p)
      stop(sprintf("decisiveness: mu must be %d x %d (data$ids order)", lay$n, lay$p))
    key <- list(mu = mu, loading = th$loading, dim = dim(th$loading),
                side = if (side == "fitted") c(th$kappa, th$omega) else NULL)
    if (!identical(cache$key, key)) { cache$key <- key; cache$val <- vector("list", ns) }
    if (is.null(cache$val[[s]])) cache$val[[s]] <- .rum_dec_eval(specs[[s]], mu, th, lay, side, control)
    cache$val[[s]]
  }
  lapply(seq_along(specs), function(s) {
    force(s); sp <- specs[[s]]
    gf <- function(mu, th) value(s, mu, th)
    list(name = sp$name, kind = "decisiveness", w = w, class = "decisiveness", group = sp$group,
         type = sp$type, m = function(mu, th) gf(mu, th)$m, grad = gf)
  })
}
