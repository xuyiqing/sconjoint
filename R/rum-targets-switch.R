# Discrete-switch average marginal effects (Avi's memo of 2026-10-08, item 5; paperps1011.tex
# eq. standardized_ame and Appendix B, "Choice Probabilities and Average Marginal Effects").
#
# The Phase C "structural AME" is a derivative: the sensitivity of the win probability to an
# infinitesimal change in one coded feature, averaged over observed task rows. The paper's AME
# is the change in the probability that a profile is chosen when ONE attribute of that
# profile switches from a reference level to another level, with everything else about the
# contest held fixed: the profile's other attributes, the opponent, and the screen position.
# The switched profile is recoded through the experiment's coder, so interaction columns and
# respondent-relative columns (GS co-partisan) change with it.
#
# Contexts: a fixed set of draws from the experiment's design law (B1's generators,
# rum-design.R; common random numbers: one seed, the same draws for every fold and every
# fitted state). Every profile slot of every drawn task (both sides) is a focal profile, so
# the opponent, the other attributes and the presentation order follow the design. For focal
# slot r on side s (+1 = side 1, -1 = side 2), D_r = x(focal) - x(opponent) and the
# probability that respondent i chooses the focal profile is
#   with position effects:    E[G(s (kappa + a_i) + D_r' beta_i)],  a_i ~ N(0, omega),
#   without position effects: E[G(D_r' beta_i)]  (hypothetical contests: kappa = a_i = 0),
# with beta_i ~ N(mu(z_i), lambda lambda'). Both are one-dimensional normal integrals:
# s (kappa + a_i) + D_r' beta_i ~ N(s kappa + D_r' mu_i, sigma_r^2), sigma_r^2 = omega +
# (D_r' lambda)^2 (without position effects: (D_r' lambda)^2), computed with a Gauss-Hermite
# rule (order 31 by default).
#   context = "switch" (default): m_i = mean over slots of P_i(focal at level) - P_i(focal at
#     ref), the same contexts on both sides of the difference (the paper's AME over the
#     design law as the reference rule R).
#   context = "conditional": m_i = mean over slots whose focal ALREADY has the level of
#     P_i(focal) minus the same mean over slots at the reference level: the model's value of
#     the design-based difference in means (AMCE). Under a design in which the attribute is
#     independent of everything else in the task (SW, BR), the two agree up to the
#     simulation error of the draws; in GS the democracy codes are not independent of the
#     opponent's code, so they differ by design.
# Target derivatives are exact (no numerical step):
#   dm/dmu = sum_r c_r E[G'] D_r;  dm/dkappa = sum_r c_r s_r E[G'] (position only);
#   dm/dlambda = sum_r c_r K_r (D_r' lambda) D_r;  dm/domega = sum_r c_r K_r / 2 (position only),
#   K_r = E[G'(m_r + sigma_r z) z] / sigma_r  (-> G''(m_r) as sigma_r -> 0).

.rum_gh_rule <- function(order = 31L) { r <- .rum_normal_rule(order); list(z = r$nodes, w = r$weights) }

# Generic target over contest rows. rows: list(D = R x p, side = R values +1/-1, coef = R)
# or a list of such lists, one per respondent group; group: per-respondent index into rows
# (NULL: one group). A group whose rows are NULL gets m = 0 and zero derivatives (its
# respondents must have weight 0).
.rum_tgt_choice_rows <- function(name, rows, group = NULL, w = NULL, position = TRUE,
                                 order = 31L, kind = "ame") {
  if (!is.null(rows$D)) rows <- list(rows)
  gh <- .rum_gh_rule(order)
  vg <- function(mu, th) {
    n <- nrow(mu); p <- ncol(mu); grp <- if (is.null(group)) rep(1L, n) else group
    m <- numeric(n); gm <- matrix(0, n, p); gl <- matrix(0, n, p); gk <- go <- numeric(n)
    for (g in unique(grp)) {
      R <- rows[[g]]; if (is.null(R)) next
      ii <- which(grp == g); ni <- length(ii)
      D <- R$D; cf <- R$coef; sd <- R$side
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
      if (any(small)) {
        P0 <- stats::plogis(MD[, small, drop = FALSE])
        K[, small] <- P0 * (1 - P0) * (1 - 2 * P0)
      }
      m[ii] <- drop(V %*% cf)
      gm[ii, ] <- A %*% (cf * D)
      gl[ii, ] <- K %*% ((cf * dl) * D)
      if (position) { gk[ii] <- drop(A %*% (cf * sd)); go[ii] <- drop(K %*% cf) / 2 }
    }
    list(m = m, gm = gm, gt = cbind(gk, gl, go))
  }
  list(name = name, kind = kind, w = w, position = position,
       m = function(mu, th) vg(mu, th)$m, grad = vg)
}

# Contest rows for one switch from raw design draws (B1's raw format: profile attributes as
# M x T x 2 integer arrays; side 1 = the side coded first in d). code(raw) -> M x T x p
# array (the experiment's coder, with the respondent key bound in). attr: the raw attribute
# (e.g. "gender", "dem", "female"); level, ref: raw values (indices into the level list, or
# the raw value itself for GS female and the policy scales). scale multiplies the
# coefficients (e.g. 1/3 for a per-unit change over a three-unit switch).
.rum_switch_rows <- function(raw, code, attr, level, ref, context = c("switch", "conditional"),
                             scale = 1) {
  context <- match.arg(context)
  a <- raw[[attr]]; if (length(dim(a)) != 3L) stop("attribute ", attr, " is not a profile attribute")
  M <- dim(a)[1L]; T <- dim(a)[2L]; MT <- M * T
  flat <- function(d) matrix(d, MT, dim(d)[3L])
  if (context == "switch") {
    D <- NULL; side <- NULL; coef <- NULL
    for (val in c(level, ref)) for (s in 1:2) {
      r <- raw; r[[attr]][, , s] <- val
      d <- flat(code(r)); if (s == 2L) d <- -d
      D <- rbind(D, d); side <- c(side, rep(if (s == 1L) 1 else -1, MT))
      coef <- c(coef, rep(if (val == level) 1 else -1, MT) / (2 * MT))
    }
  } else {
    d <- flat(code(raw)); D <- NULL; side <- NULL; coef <- NULL
    for (val in c(level, ref)) {
      sel1 <- which(as.vector(a[, , 1L]) == val); sel2 <- which(as.vector(a[, , 2L]) == val)
      nv <- length(sel1) + length(sel2)
      if (!nv) stop("no drawn slot has value ", val, " of ", attr, "; draw more contexts")
      D <- rbind(D, d[sel1, , drop = FALSE], -d[sel2, , drop = FALSE])
      side <- c(side, rep(1, length(sel1)), rep(-1, length(sel2)))
      coef <- c(coef, rep(if (val == level) 1 else -1, nv) / nv)
    }
  }
  list(D = D, side = side, coef = coef * scale)
}

# A discrete-switch AME target for one attribute level from a design spec (rum-design.R).
# raw: the context draws (.rum_design_draw_raw(spec, M, seed = ...)); keys: per-respondent
# design keys (GS: D/R/I from pid7, see .rum_gs_key; others NULL); level, ref: raw values, or
# functions of the key returning the raw value (NA: the switch is not defined for that key,
# e.g. GS co-partisan for pure independents; those respondents get weight 0 and are listed in
# attr "excluded"). position = TRUE includes kappa and the side effect (the comparison with
# the design-based AMCE); FALSE drops them (hypothetical contests).
.rum_tgt_switch_ame <- function(name, spec, raw, attr, level, ref, keys = NULL, w = NULL,
                                position = TRUE, context = c("switch", "conditional"),
                                scale = 1, order = 31L) {
  context <- match.arg(context)
  spec <- .rum_spec_arg(spec)
  if (spec$uses_resp && is.null(keys)) stop("this design codes by respondent; give keys")
  n <- length(keys); uk <- if (spec$uses_resp) sort(unique(keys)) else "all"
  val <- function(x, k) if (is.function(x)) x(k) else x
  rows <- lapply(uk, function(k) {
    lv <- val(level, k); rf <- val(ref, k)
    if (is.na(lv) || is.na(rf)) return(NULL)
    code <- function(r) spec$code(r, if (spec$uses_resp) k else NULL)
    .rum_switch_rows(raw, code, attr, lv, rf, context, scale)
  })
  group <- if (spec$uses_resp) match(keys, uk) else NULL
  undefined <- vapply(rows, is.null, TRUE)
  excl <- if (spec$uses_resp) which(undefined[group]) else integer(0)
  if (length(excl)) {
    w <- if (is.null(w)) rep(1, n) else as.numeric(w)
    w[excl] <- 0
  }
  tg <- .rum_tgt_choice_rows(name, rows, group, w, position, order, kind = "ame")
  tg$excluded <- excl; tg$switch <- list(attr = attr, level = level, ref = ref, context = context,
                                         n_contexts = dim(raw[[attr]])[1L] * dim(raw[[attr]])[2L] * 2L)
  tg
}

# Design-based difference-in-means AMCE from raw profiles and choices (profile-stacked: each
# task gives two profiles; the side-1 profile is chosen when y = 1). a1, a2: the attribute's
# raw values on side 1 and side 2 (one per task); y: choices; id: respondent of each task.
# Returns estimate and respondent-clustered SE (influence values summed by respondent).
.rum_dm_amce <- function(a1, a2, y, id, level, ref) {
  val <- c(a1, a2); ch <- c(y, 1 - y); idd <- c(id, id)
  il <- val == level; ir <- val == ref
  pl <- mean(ch[il]); pr <- mean(ch[ir])
  psi <- numeric(length(val))
  psi[il] <- (ch[il] - pl) / sum(il); psi[ir] <- -(ch[ir] - pr) / sum(ir)
  cl <- tapply(psi, idd, sum); G <- length(cl)
  c(estimate = pl - pr, se = sqrt(sum(cl^2) * G / (G - 1)))
}
