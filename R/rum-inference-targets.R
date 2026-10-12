# Population targets for RUM inference, and the fitted-state helpers they need.
# Ported from the RUM reproduction scripts of 2026-09-24/26 (Yiqing Xu):
# scripts_C/rum_debias.R (targets, target_grad) and scripts_C/rum_core.R
# (state_formula, choice_fitted), with the arithmetic unchanged. Internal; the engine
# that uses these targets is in rum-inference.R.
#
# Model: beta_i = mu(z_i) + loading * u_i, u ~ N(0, 1); eta_it = kappa + d_it' beta_i +
# sqrt(omega) * v_i, v ~ N(0, 1) (random side preference); P(y_it = 1) = logistic(eta_it).
#
# A target is a list with
#   name  label (character),
#   kind  "mean", "share", "choice", "lincomb", ... (bounded kinds: .rum_bounded_kinds),
#   w     NULL or respondent weights in data$ids order (a subgroup: 1 inside, 0 outside),
#   m     function(mu, th) -> the n per-respondent values m_i,
#   grad  optional function(mu, th) -> list(m, gm = n x p, gt = n x q) with exact gradients.
# th = list(kappa, loading, omega); the common parameters theta are ordered as the
# evaluator's common score: kappa, loading[1..p], omega (q = p + 2).
# A new target type only needs m (central differences are then used) or m and grad.

# Kinds that live in [0, 1]; .rum_bounded_dnn() reports them with the CV-TMLE.
.rum_bounded_kinds <- c("share", "share_nonlinear", "importance")

# Quadrature rules of the engine: fitted-law integrals in targets (41 x 41), sequence
# scores (21 x 21), and the model-based information (21 nodes in u, 11 in v).
.rum_law_quadrature <- function() rum_quadrature(c(41L, 41L))
.rum_score_quadrature <- function() rum_quadrature(c(21L, 21L))
.rum_info_quadrature <- function() rum_quadrature(c(21L, 11L))

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

# Position-neutral probability that profile A beats B (d = x_A - x_B), integrated over
# the fitted law, with the random side preference or without it. Per-respondent values.
.rum_choice_fitted <- function(st, dvec, quad, random_side = TRUE) {
  m <- drop(st$mu %*% dvec); s <- sum(st$loading * dvec)
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

# Fitted-law share with c' beta_i > 0: Phi(c' mu_i / |c' loading|).
.rum_tgt_share <- function(name, c, w = NULL) list(name = name, kind = "share", c = c, w = w,
  m = function(mu, th) stats::pnorm(drop(mu %*% c) / abs(sum(th$loading * c))))

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
# dm/dkappa = dm/domega = 0. The (u, v) quadrature is reused; v does not enter.
.rum_tgt_choice_pref <- function(name, d, w = NULL, quad = .rum_law_quadrature())
  list(name = name, kind = "choice", d = d, w = w,
       m = function(mu, th) .rum_choice_fitted(list(mu = mu, kappa = 0, loading = th$loading,
         omega = th$omega), d, quad, FALSE))

.rum_tgt_choice_set_pref <- function(name, D, coef = rep(1 / nrow(rbind(D)), nrow(rbind(D))),
                                     w = NULL, quad = .rum_law_quadrature()) {
  D <- rbind(D)
  vg <- function(mu, th) {
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
  list(name = name, kind = "choice", w = w, m = function(mu, th) vg(mu, th)$m, grad = vg)
}

# ---- gradients --------------------------------------------------------------------------
# Below this omega, omega derivatives use their boundary forms (no division by sqrt(omega)).
.rum_omega_small <- 1e-8

.rum_th_vec  <- function(th) c(th$kappa, th$loading, th$omega)
.rum_th_list <- function(v, p) list(kappa = v[1], loading = v[1 + seq_len(p)], omega = v[p + 2])

# Per-respondent value m_i and gradients m_mu (n x p) and m_theta (n x q). Exact gradients
# when the target supplies them; mean contrasts are analytic; otherwise central differences
# (m_i depends only on mu_i, so a whole column moves at once). omega >= 0: when omega - h < 0
# the omega column uses the one-sided second-order difference (-3 m(omega) + 4 m(omega + h)
# - m(omega + 2 h)) / (2 h), which never evaluates a target at a negative variance (at
# omega = 0 it is the right derivative).
.rum_target_grad <- function(tg, mu, th, h = 1e-6) {
  n <- nrow(mu); p <- ncol(mu); q <- p + 2L
  if (!is.null(tg$grad)) return(tg$grad(mu, th))
  if (identical(tg$kind, "mean"))
    return(list(m = tg$m(mu, th), gm = matrix(tg$c, n, p, byrow = TRUE), gt = matrix(0, n, q)))
  gm <- matrix(0, n, p)
  for (j in seq_len(p)) {
    m1 <- mu; m2 <- mu; m1[, j] <- m1[, j] + h; m2[, j] <- m2[, j] - h
    gm[, j] <- (tg$m(m1, th) - tg$m(m2, th)) / (2 * h)
  }
  v <- .rum_th_vec(th); gt <- matrix(0, n, q)
  for (j in seq_len(q)) {
    v1 <- v; v2 <- v; v1[j] <- v1[j] + h; v2[j] <- v2[j] - h
    if (j == q && v2[j] < 0) {                       # omega: stay in the domain
      v3 <- v; v3[j] <- v3[j] + 2 * h
      gt[, j] <- (-3 * tg$m(mu, th) + 4 * tg$m(mu, .rum_th_list(v1, p)) -
                    tg$m(mu, .rum_th_list(v3, p))) / (2 * h)
    } else gt[, j] <- (tg$m(mu, .rum_th_list(v1, p)) - tg$m(mu, .rum_th_list(v2, p))) / (2 * h)
  }
  list(m = tg$m(mu, th), gm = gm, gt = gt)
}
