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

# Fitted-law share with c' beta_i > 0: Phi(c' mu_i / ||Lambda' c||) (one factor: |c' lambda|).
.rum_tgt_share <- function(name, c, w = NULL) list(name = name, kind = "share", c = c, w = w,
  m = function(mu, th) stats::pnorm(drop(mu %*% c) / abs(.rum_ld_scale(th$loading, c))))

# Position-neutral win probability for one contest d, side preference integrated
# (gradients by central differences).
.rum_tgt_choice <- function(name, d, w = NULL, quad = .rum_law_quadrature())
  list(name = name, kind = "choice", d = d, w = w,
       m = function(mu, th) .rum_choice_fitted(list(mu = mu, kappa = th$kappa,
         loading = th$loading, omega = th$omega), d, quad, TRUE))

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
  D <- rbind(D)
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
.rum_tgt_choice_pref <- function(name, d, w = NULL, quad = .rum_law_quadrature())
  list(name = name, kind = "choice", d = d, w = w,
       m = function(mu, th) .rum_choice_fitted(list(mu = mu, kappa = 0, loading = th$loading,
         omega = th$omega), d, quad, FALSE))

.rum_tgt_choice_set_pref <- function(name, D, coef = rep(1 / nrow(rbind(D)), nrow(rbind(D))),
                                     w = NULL, quad = .rum_law_quadrature()) {
  D <- rbind(D)
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
# u-marginal of rum_quadrature(c(80, 41)), as CV). q > 1 factors: b = mu + Lambda V z over the
# principal axes Lambda V (.rum_principal_axes) and the product of uq over the q axes (default
# uq: Gauss-Hermite of order order_multi; NULL: 32 at q = 2, 20 at q = 3, 12 above), nodes with
# product weight below wmin dropped. The value depends on Lambda only through Lambda Lambda'
# (rotation invariant). Accuracy: the share is smooth except at b = 0; a respondent whose
# plane mu_i + span(Lambda) passes near b = 0 needs a higher order (check by raising
# order_multi; with p >= 8 features this is rare).
# Exact gradients (as CV): d share_g / d B_j = dV_j (1{j in g} - share_g) / T, T = sum_h V_h;
# dm/dmu = E[that]; the loading adds the factor u. q > 1: dm/dLambda = G V', G = E[(d share /
# d B) z'] at the principal axes, then its component along the rotation directions (zero for
# the exact integral) is removed (.rum_rotation_project). gt = (0, vec(dm/dLambda), 0). A central
# difference in Lambda also moves V; that extra part is the product rule's lack of rotation
# invariance, a quadrature error that vanishes as the order grows (the exact gradient is closer
# to the Monte Carlo pathwise gradient).
.rum_imp_rule <- function() {
  q <- rum_quadrature(c(80L, 41L)); k <- round(q$u, 12); w <- tapply(q$weight, k, sum)
  list(u = as.numeric(names(w)), w = unname(w))
}
.rum_imp_order <- function(nf) if (nf == 2L) 32L else if (nf == 3L) 20L else 12L
.rum_imp_sums <- function(mu, loading, groups, centred, uq = NULL, order_multi = NULL, wmin = 1e-14) {
  GR <- groups; n <- nrow(mu); pp <- ncol(mu); ng <- length(GR); grp_of <- integer(pp)
  L <- if (centred) vapply(GR, length, 0L) + 1 else rep(1, ng)
  for (g in seq_len(ng)) grp_of[GR[[g]]] <- g
  ing <- which(grp_of > 0); cc <- as.numeric(centred)
  multi <- .rum_multi_fac(loading)
  if (multi) {
    nf <- ncol(loading); pa <- .rum_principal_axes(loading)
    if (is.null(order_multi)) order_multi <- .rum_imp_order(nf)
    r1 <- if (is.null(uq)) { r <- .rum_gh_rule(order_multi); list(u = r$z, w = r$w) } else uq
    ix <- as.matrix(expand.grid(rep(list(seq_along(r1$u)), nf)))
    Z <- matrix(r1$u[ix], nrow(ix)); W <- apply(matrix(r1$w[ix], nrow(ix)), 1L, prod)
    keep <- W >= wmin; Z <- Z[keep, , drop = FALSE]; W <- W[keep]
    OFF <- Z %*% t(pa$L)                                   # node x p offsets Lambda V z
  } else {
    nf <- 1L; r1 <- if (is.null(uq)) .rum_imp_rule() else uq; W <- r1$w; Z <- matrix(r1$u)
  }
  M <- matrix(0, n, ng); GM <- array(0, c(n, pp, ng)); GL <- array(0, c(n, pp * nf, ng))
  for (x in seq_along(W)) {
    if (W[x] < wmin) next
    B <- if (multi) mu + matrix(OFF[x, ], n, pp, byrow = TRUE) else mu + matrix(loading * Z[x, 1L], n, pp, byrow = TRUE)
    Sg <- vapply(seq_len(ng), function(g) rowSums(B[, GR[[g]], drop = FALSE]), numeric(n))
    Vg <- vapply(seq_len(ng), function(g) rowSums(B[, GR[[g]], drop = FALSE]^2) / L[g], numeric(n)) -
      cc * (Sg / matrix(L, n, ng, byrow = TRUE))^2
    Vg <- matrix(Vg, n); Sg <- matrix(Sg, n)
    Tt <- rowSums(Vg); sh <- Vg / Tt; dV <- matrix(0, n, pp)
    dV[, ing] <- 2 * B[, ing] / matrix(L[grp_of[ing]], n, length(ing), byrow = TRUE) -
      cc * 2 * Sg[, grp_of[ing], drop = FALSE] / matrix(L[grp_of[ing]]^2, n, length(ing), byrow = TRUE)
    M <- M + W[x] * sh
    for (g in seq_len(ng)) {
      gr <- -sh[, g] * dV; gr[, GR[[g]]] <- gr[, GR[[g]]] + dV[, GR[[g]]]; gr <- gr / Tt
      GM[, , g] <- GM[, , g] + W[x] * gr
      if (multi) for (k in seq_len(nf)) {
        cols <- (k - 1L) * pp + seq_len(pp)
        GL[, cols, g] <- GL[, cols, g] + (W[x] * Z[x, k]) * gr
      } else GL[, , g] <- GL[, , g] + W[x] * Z[x, 1L] * gr
    }
  }
  if (multi) for (g in seq_len(ng)) {             # principal-axis block G (n x p q) -> G V'
    Gp <- GL[, , g]; out <- matrix(0, n, pp * nf)
    for (l in seq_len(nf)) for (k in seq_len(nf))
      out[, (l - 1L) * pp + seq_len(pp)] <- out[, (l - 1L) * pp + seq_len(pp)] +
        pa$V[l, k] * Gp[, (k - 1L) * pp + seq_len(pp), drop = FALSE]
    GL[, , g] <- .rum_rotation_project(cbind(0, out, 0), loading)[, 1L + seq_len(pp * nf), drop = FALSE]
  }
  list(m = M, gm = GM, gl = GL)
}
# All importance-share targets of one set of groups, sharing one computation per (mu, th).
# names: one per group (default "importance: <group name>"); w: respondent weights.
.rum_tgt_importance_set <- function(groups, centred = FALSE, names = NULL, w = NULL, uq = NULL,
                                    order_multi = NULL, wmin = 1e-14) {
  force(groups); force(centred); force(uq)
  if (is.null(names)) names <- paste0("importance: ", if (is.null(base::names(groups)))
    seq_along(groups) else base::names(groups))
  cache <- new.env()
  sums <- function(mu, th) {
    if (!is.null(cache$mu) && identical(cache$mu, mu) && identical(cache$loading, th$loading)) return(cache$val)
    v <- .rum_imp_sums(mu, th$loading, groups, centred, uq, order_multi, wmin)
    cache$mu <- mu; cache$loading <- th$loading; cache$val <- v; v
  }
  lapply(seq_along(groups), function(g) {
    force(g)
    gf <- function(mu, th) {
      s <- sums(mu, th); n <- nrow(mu)
      list(m = s$m[, g], gm = matrix(s$gm[, , g], n), gt = cbind(0, matrix(s$gl[, , g], n), 0))
    }
    list(name = names[g], kind = "importance", w = w, class = "importance",
         m = function(mu, th) gf(mu, th)$m, grad = gf)
  })
}
.rum_tgt_importance <- function(name, g, groups, centred = FALSE, w = NULL, uq = NULL,
                                order_multi = NULL, wmin = 1e-14) {
  nm <- rep(name, length(groups))
  .rum_tgt_importance_set(groups, centred, nm, w, uq, order_multi, wmin)[[g]]
}
