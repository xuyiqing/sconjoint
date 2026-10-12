# Wrong-model truths for RUM coverage simulations (overnight run 2026-10-12, agent SC;
# Avi's memo of 2026-10-08, item 11: "a wrong-model truth with its own target values").
#
# RUM (the fitted model): beta_i = mu(z_i) + lambda u_i, u_i ~ N(0, 1); a random side
# preference v_i ~ N(0, omega); P(y_it = 1) = G(kappa + d_it' beta_i + v_i), G logistic.
# Fit checks reject it in all three studies (Phase C, fit_checks_C.md): pair checks show more
# variation along some attributes than one factor allows, and BR has twice the predicted
# straight-liners (88 of 1,991 vs 44). The truths here are built to fail in those two ways.
#
#   (a) two factors:   beta_i = mu(z_i) + L1 u1_i + L2 u2_i, u1, u2 iid N(0, 1);
#   (b) straight-liners: with probability pi, respondent i ignores the profiles and picks the
#       same side s_i in every task (s_i = side 1 with probability q; each choice flips with
#       probability eps, 0 by default); otherwise i follows the RUM above. Membership is
#       independent of z_i and of the tastes;
#   (c) both.
# L2 = 0 and pi = 0 give back the fitted one-factor RUM (same draws, same choices).
#
# Targets under a wrong truth. Taste targets (means of c'beta, shares Pr(c'beta > 0), win
# probabilities without position effects, AMEs without position effects) are properties of the
# taste law of ALL n respondents, straight-liners included: straight-lining is a response
# process that hides a taste, it does not remove it. Their true values use the two-factor law,
# c'beta_i ~ N(c'mu_i, sum_k (c'L_k)^2), and are one-dimensional normal integrals (Gauss-Hermite).
# Behavioural targets (position = TRUE: win probabilities and AMEs with the side preference) are
# properties of the observed choice process: a straight-liner chooses the focal profile with
# probability q (focal on side 1) or 1 - q (side 2), whatever the profiles, so
#   m_i(behaviour) = (1 - pi) m_i(attentive) + pi m_i(straight-liner).
# A design-based AMCE estimates the behavioural AME; a structural fit aims at the taste AME.
# Values are conditional on the sample's covariates (the average over the n respondents of a
# per-respondent value), as in the S-A harness.

# ---- truth object -------------------------------------------------------------------------
# mu: n x p matrix of mean tastes (or a function of Z returning one); kappa, loading (L1), omega:
# the fitted RUM's common parameters; loading2: L2 (NULL or 0: one factor); pi_sl, sl_side1,
# sl_flip: the straight-liner mixture.
.rum_ms_truth <- function(mu, kappa, loading, omega, loading2 = NULL, pi_sl = 0,
                          sl_side1 = 0.5, sl_flip = 0, Z = NULL, label = NULL) {
  if (is.function(mu)) {
    if (is.null(Z)) stop("mu is a function: supply Z.")
    mu <- mu(Z)
  }
  if (!is.matrix(mu) || !is.numeric(mu) || any(!is.finite(mu))) stop("mu must be a finite n x p matrix.")
  p <- ncol(mu)
  chk <- function(x, k, nm) if (!is.numeric(x) || length(x) != k || any(!is.finite(x))) stop(nm, " must be finite, length ", k, ".")
  chk(kappa, 1L, "kappa"); chk(loading, p, "loading"); chk(omega, 1L, "omega")
  if (omega < 0) stop("omega must be nonnegative.")
  if (is.null(loading2)) loading2 <- rep(0, p)
  chk(loading2, p, "loading2")
  for (x in list(pi_sl, sl_side1, sl_flip)) chk(x, 1L, "mixture parameters")
  if (pi_sl < 0 || pi_sl > 1 || sl_side1 < 0 || sl_side1 > 1 || sl_flip < 0 || sl_flip > 0.5)
    stop("pi_sl and sl_side1 must be in [0, 1], sl_flip in [0, 0.5].")
  L <- cbind(L1 = unname(loading), L2 = unname(loading2))
  rownames(L) <- colnames(mu)
  type <- paste(c(if (any(loading2 != 0)) "two-factor", if (pi_sl > 0) "straight-liners"), collapse = " + ")
  if (!nzchar(type)) type <- "one-factor RUM"
  structure(list(mu = mu, kappa = unname(kappa), L = L, omega = unname(omega), pi_sl = pi_sl,
                 sl_side1 = sl_side1, sl_flip = sl_flip, type = type,
                 label = if (is.null(label)) type else label), class = "rum_ms_truth")
}

# The fitted one-factor law of a truth (L2 = 0, no straight-liners): the "RUM reading".
.rum_ms_one_factor <- function(truth)
  .rum_ms_truth(truth$mu, truth$kappa, truth$L[, 1L], truth$omega, label = "one-factor reading")

# ---- simulator ----------------------------------------------------------------------------
# One data set from a truth. Designs: B1's generator (rum-design.R), one fresh sequence per
# respondent (draw i is respondent i's; GS coded by the respondent's party key), truncated to
# the respondent's task count T_i. Random draws, in this order under seed + 1000003: u1 (n),
# v (n), U (one uniform per task), then u2 (n), the straight-liner indicator uniforms (n), the
# side uniforms (n) and the flip uniforms (one per task). The first three are exactly the S-A
# harness's draws (SA_common.R simulate_data), so a one-factor truth reproduces its data and
# the extra draws never change the RUM part.
# data: a rum_data template (ids, T, basis, metadata are kept; d, a, y are replaced); or give
# n and T. resp: GS respondent party input (pid7 or D/R/I, one per respondent).
.rum_ms_simulate <- function(truth, spec, data = NULL, resp = NULL, T = NULL, seed) {
  if (!inherits(truth, "rum_ms_truth")) stop("truth must come from .rum_ms_truth().")
  spec <- .rum_spec_arg(spec)
  n <- nrow(truth$mu); p <- ncol(truth$mu)
  if (p != spec$p) stop("truth has ", p, " features; the design has ", spec$p, ".")
  if (!is.null(data)) {
    .rum_check_data(data)
    if (data$n != n || data$p != p) stop("data and truth disagree in n or p.")
    T <- data$T
  }
  if (is.null(T)) T <- rep(spec$T, n)
  T <- as.integer(T)
  if (length(T) != n || any(T < 1L | T > spec$T)) stop("T must give 1..", spec$T, " tasks per respondent.")
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) stop("seed must be one number.")
  crn <- .rum_draw_designs_crn(spec, resp, M = n, seed = seed, n = n)
  if (length(crn$key) != n) stop("resp must have one entry per respondent.")
  off <- as.integer(c(0L, cumsum(T))); nt <- off[n + 1L]
  dr <- .rum_with_seed(seed + 1000003L, {
    u <- stats::rnorm(n); v <- stats::rnorm(n); U <- stats::runif(nt)
    u2 <- stats::rnorm(n); S <- stats::runif(n); side <- stats::runif(n); flip <- stats::runif(nt)
    list(u = u, v = v, U = U, u2 = u2, S = S, side = side, flip = flip)
  })
  D <- matrix(0, nt, p)
  for (k in seq_along(crn$designs)) {
    who <- which(crn$key == k); arr <- crn$designs[[k]]
    for (t in seq_len(max(T[who]))) {
      w2 <- who[T[who] >= t]; D[off[w2] + t, ] <- arr[w2, t, ]
    }
  }
  ri <- rep(seq_len(n), T)
  L <- truth$L
  B <- truth$mu[ri, , drop = FALSE] + outer(dr$u[ri], L[, 1L])
  if (any(L[, 2L] != 0)) B <- B + outer(dr$u2[ri], L[, 2L])
  eta <- truth$kappa + rowSums(D * B) + sqrt(truth$omega) * dr$v[ri]
  y <- as.numeric(dr$U < stats::plogis(eta))
  straight <- dr$S < truth$pi_sl
  sl_side <- as.numeric(dr$side < truth$sl_side1)               # 1: side 1 every task
  if (any(straight)) {
    rows <- which(straight[ri])
    ys <- sl_side[ri[rows]]
    if (truth$sl_flip > 0) ys <- ifelse(dr$flip[rows] < truth$sl_flip, 1 - ys, ys)
    y[rows] <- ys
  }
  colnames(D) <- spec$features
  if (is.null(data)) {
    data <- rum_data(unname(split(y, ri)), unname(lapply(split(seq_len(nt), ri), function(j) D[j, , drop = FALSE])),
                     ids = rownames(truth$mu), feature_names = spec$features,
                     metadata = list(orientation = paste("y = 1 selects side 1 of the", spec$study, "design")))
  } else {
    data$d <- unname(D); data$y <- y
    data$a <- if (data$r == p && all(vapply(data$basis, function(b) isTRUE(all.equal(b, diag(p), check.attributes = FALSE)), TRUE)))
      unname(D) else do.call(rbind, lapply(seq_len(n), function(i) D[off[i] + seq_len(T[i]), , drop = FALSE] %*% data$basis[[i]]))
  }
  data$metadata$simulation <- list(seed = seed, truth = truth$label, type = truth$type,
    kappa = truth$kappa, L = truth$L, omega = truth$omega, pi_sl = truth$pi_sl,
    sl_side1 = truth$sl_side1, sl_flip = truth$sl_flip,
    latent = list(u1 = dr$u, u2 = dr$u2, v = dr$v, straight = straight, sl_side = sl_side))
  data
}

# ---- true target values -------------------------------------------------------------------
# Per-respondent probabilities over contest rows (D: R x p, side: +1/-1, coef: R; group:
# per-respondent index into a list of such row sets), integrated over the K-factor law:
#   x = s kappa + D_r' mu_i + sum_k (D_r' L_k) u_k (+ s v), normal with SD
#   sigma_r = sqrt(sum_k (D_r' L_k)^2 + omega 1{position}).
# Returns m (n values, sum_r coef_r P_ir) and, when seq is given, the weighted respondent
# average of each row's coefficient-weighted probability summed within each sequence id
# (for the simulation error of the contexts).
.rum_ms_rows_value <- function(mu, L, rows, group = NULL, kappa = 0, omega = 0, position = FALSE,
                               order = 31L, w = NULL, chunk = 1500L) {
  if (!is.null(rows$D)) rows <- list(rows)
  gh <- .rum_gh_rule(order); n <- nrow(mu)
  grp <- if (is.null(group)) rep(1L, n) else group
  wt <- if (is.null(w)) rep(1, n) else as.numeric(w)
  m <- numeric(n); seqsum <- NULL
  for (g in unique(grp)) {
    R <- rows[[g]]; if (is.null(R)) next
    ii <- which(grp == g); ni <- length(ii)
    side <- if (is.null(R$side)) rep(1, nrow(R$D)) else R$side
    for (cs in split(seq_len(nrow(R$D)), ceiling(seq_len(nrow(R$D)) / chunk))) {
      D <- R$D[cs, , drop = FALSE]; cf <- R$coef[cs]
      sig <- sqrt(rowSums((D %*% L)^2) + if (position) omega else 0)
      MD <- mu[ii, , drop = FALSE] %*% t(D)
      if (position) MD <- MD + rep(kappa * side[cs], each = ni)
      V <- matrix(0, ni, length(cs))
      for (j in seq_along(gh$z)) V <- V + gh$w[j] * stats::plogis(MD + rep(sig * gh$z[j], each = ni))
      m[ii] <- m[ii] + drop(V %*% cf)
      if (!is.null(R$seq)) {
        a <- colSums(wt[ii] * V) * cf                                   # weighted sum over respondents
        s <- rowsum(a, R$seq[cs], reorder = FALSE)
        seqsum <- if (is.null(seqsum)) s else {
          k <- union(rownames(seqsum), rownames(s)); z <- matrix(0, length(k), 1, dimnames = list(k, NULL))
          z[rownames(seqsum), 1] <- seqsum[, 1]; z[rownames(s), 1] <- z[rownames(s), 1] + s[, 1]; z }
      }
    }
  }
  list(m = m, seqsum = seqsum)
}

# A target spec for true values, from a package target (rum-inference-targets.R and
# rum-targets-switch.R) or built directly. Kinds:
#   linear  value linear in mu and free of the law (mean contrasts, subgroup contrasts): tg$m;
#   share   Pr(c'beta > 0) = Phi(c'mu / sqrt(sum_k (c'L_k)^2));
#   rows    contest rows (win probabilities and switch AMEs), with or without position.
.rum_ms_spec <- function(tg) {
  if (!is.null(tg$ms_kind)) return(tg)
  out <- list(name = tg$name, w = tg$w, class = tg$class)
  if (tg$kind %in% c("mean", "group contrast")) return(c(out, list(ms_kind = "linear", m = tg$m)))
  if (identical(tg$kind, "share")) return(c(out, list(ms_kind = "share", c = tg$c)))
  if (!is.null(tg$grad)) {
    env <- environment(tg$grad)
    if (!is.null(env$rows)) {                                            # .rum_tgt_choice_rows / switch AME
      rows <- env$rows; if (!is.null(rows$D)) rows <- list(rows)
      return(c(out, list(ms_kind = "rows", rows = rows, group = env$group, position = isTRUE(tg$position))))
    }
    if (!is.null(env$D) && !is.null(env$coef)) {                         # .rum_tgt_choice_set(_pref)
      pos <- "svm" %in% all.names(body(tg$grad))                         # choice_set integrates the side
      D <- rbind(env$D)
      # position-neutral: profile A on side 1 (side +1) and on side 2 (side -1); D stays A minus B
      rows <- if (pos) list(D = rbind(D, D), side = c(rep(1, nrow(D)), rep(-1, nrow(D))), coef = c(env$coef, env$coef) / 2)
              else list(D = D, side = rep(1, nrow(D)), coef = env$coef)
      return(c(out, list(ms_kind = "rows", rows = list(rows), group = NULL, position = pos)))
    }
  }
  stop("target ", tg$name, ": no wrong-model value for kind ", tg$kind)
}

# Rows of a switch AME with sequence ids (for the simulation error of the contexts), from raw
# context draws (.rum_design_draw_raw). Same arguments as .rum_tgt_switch_ame; context "switch".
.rum_ms_switch_target <- function(name, spec, raw, attr, level, ref, keys = NULL, w = NULL,
                                  position = FALSE, scale = 1, class = "ame") {
  spec <- .rum_spec_arg(spec)
  if (spec$uses_resp && is.null(keys)) stop("this design codes by respondent; give keys")
  M <- dim(raw[[attr]])[1L]; Tn <- dim(raw[[attr]])[2L]
  uk <- if (spec$uses_resp) sort(unique(keys)) else "all"
  val <- function(x, k) if (is.function(x)) x(k) else x
  rows <- lapply(uk, function(k) {
    lv <- val(level, k); rf <- val(ref, k)
    if (is.na(lv) || is.na(rf)) return(NULL)
    code <- function(r) spec$code(r, if (spec$uses_resp) k else NULL)
    R <- .rum_switch_rows(raw, code, attr, lv, rf, "switch", scale)
    R$seq <- rep(rep(seq_len(M), Tn), 4L)                                # (level, ref) x (side 1, 2) blocks of M*T
    R
  })
  group <- if (spec$uses_resp) match(keys, uk) else NULL
  if (spec$uses_resp && any(vapply(rows, is.null, TRUE))) {
    w <- if (is.null(w)) rep(1, length(keys)) else as.numeric(w)
    w[vapply(rows, is.null, TRUE)[group]] <- 0
  }
  list(name = name, ms_kind = "rows", rows = rows, group = group, position = position, w = w,
       class = class, n_seq = M)
}

# Per-respondent values of one spec under a truth (attentive law; no straight-liners).
.rum_ms_values <- function(truth, sp, order = 31L) {
  mu <- truth$mu; L <- truth$L
  switch(sp$ms_kind,
    linear = sp$m(mu, list(kappa = truth$kappa, loading = L[, 1L], omega = truth$omega)),
    share = {
      s <- sqrt(sum(crossprod(L, sp$c)^2)); m <- drop(mu %*% sp$c)
      if (s > 0) stats::pnorm(m / s) else as.numeric(m > 0)
    },
    rows = .rum_ms_rows_value(mu, L, sp$rows, sp$group, truth$kappa, truth$omega, sp$position, order)$m)
}

# A straight-liner's value of a behavioural (position) rows target: the focal profile wins
# with probability q* when it is on side 1, 1 - q* on side 2 (q* = q (1 - eps) + (1 - q) eps).
.rum_ms_sl_value <- function(truth, sp, n) {
  q <- truth$sl_side1 * (1 - truth$sl_flip) + (1 - truth$sl_side1) * truth$sl_flip
  grp <- if (is.null(sp$group)) rep(1L, n) else sp$group
  per <- vapply(sp$rows, function(R) if (is.null(R)) 0 else
    sum(R$coef * ifelse(R$side > 0, q, 1 - q)), 0)
  per[grp]
}

# True values of targets under one or more truths, with their precision. truths: one truth or
# a named list of truths; laws shared by several truths (same mu, kappa, L, omega) are
# integrated once. Returns one row per truth and target:
#   value      the true value (taste targets: all n respondents' tastes; behavioural targets:
#              the choice process, straight-liners included);
#   quad_err   |value(order) - value(order_check)| (Gauss-Hermite orders 45 and 31): the error of
#              the lower order, a conservative bound for the reported value's error (rows targets;
#              shares and linear targets are exact). Integrands with SD sigma_r > 5 converge slowly;
#              on the three studies' truths the bound is below 1e-6;
#   ctx_se     for switch AMEs built by .rum_ms_switch_target: the simulation SE of the context
#              draws relative to the design-law expectation (sequences are iid); 0 relative to
#              the given context set;
#   one_factor the same target under the truth's one-factor reading (L2 = 0, no
#              straight-liners): what RUM's law says if it recovered mu, kappa, L1, omega exactly.
#              It is NOT RUM's pseudo-true value (a fit to wrong-model data moves mu and lambda
#              to absorb L2 and the straight-liners); that is left to the simulation.
.rum_ms_true_values <- function(truths, targets, order = 45L, order_check = 31L) {
  if (inherits(truths, "rum_ms_truth")) truths <- list(truth = truths)
  if (is.null(names(truths)) || any(!nzchar(names(truths)))) stop("truths must be named.")
  specs <- lapply(targets, .rum_ms_spec)
  cache <- new.env()
  lawkey <- function(tr) paste(c(tr$kappa, tr$omega, tr$L, sum(tr$mu * seq_along(tr$mu))), collapse = "|")
  law_values <- function(tr, sp, k) {           # attentive per-respondent values + ctx per-sequence sums
    key <- paste(k, lawkey(tr), sep = "#")
    if (!is.null(cache[[key]])) return(cache[[key]])
    n <- nrow(tr$mu); w <- if (is.null(sp$w)) rep(1, n) else as.numeric(sp$w)
    out <- if (sp$ms_kind == "rows") {
      r1 <- .rum_ms_rows_value(tr$mu, tr$L, sp$rows, sp$group, tr$kappa, tr$omega, sp$position, order, w = w)
      r2 <- .rum_ms_rows_value(tr$mu, tr$L, sp$rows, sp$group, tr$kappa, tr$omega, sp$position, order_check)
      list(m = r1$m, m_check = r2$m, seqsum = r1$seqsum)
    } else { m <- .rum_ms_values(tr, sp, order); list(m = m, m_check = m, seqsum = NULL) }
    assign(key, out, envir = cache); out
  }
  do.call(rbind, lapply(names(truths), function(tn) {
    truth <- truths[[tn]]; n <- nrow(truth$mu); one <- .rum_ms_one_factor(truth)
    do.call(rbind, lapply(seq_along(specs), function(k) {
      sp <- specs[[k]]
      w <- if (is.null(sp$w)) rep(1, n) else as.numeric(sp$w)
      avg <- function(m) sum(w * m) / sum(w)
      behav <- identical(sp$ms_kind, "rows") && isTRUE(sp$position)
      fac <- if (behav) 1 - truth$pi_sl else 1
      mix <- function(m) if (behav && truth$pi_sl > 0) fac * m + truth$pi_sl * .rum_ms_sl_value(truth, sp, n) else m
      lv <- law_values(truth, sp, k); lo <- law_values(one, sp, k)
      v1 <- avg(mix(lv$m))
      ctx <- NA_real_
      if (!is.null(lv$seqsum)) {
        per <- lv$seqsum[, 1] / sum(w); M <- length(per)
        ctx <- fac * stats::sd(per * M) / sqrt(M)
      }
      data.frame(truth = tn, truth_type = truth$type, target = sp$name,
                 class = if (is.null(sp$class)) NA_character_ else sp$class,
                 kind = sp$ms_kind, scope = if (behav) "behaviour" else "taste",
                 value = v1, quad_err = abs(v1 - avg(mix(lv$m_check))), ctx_se = ctx,
                 one_factor = avg(lo$m), stringsAsFactors = FALSE)
    }))
  }))
}

# ---- fit-check statistics (calibration) ----------------------------------------------------
# Avi's pair and count moments (applications/br2017/specification.R br_moment_bank; the GS/SW
# banks of 09-24 use the same families): for a direction D_t (scaled by a fixed RMS),
#   pair moment of respondent i = sum_{s < t} y_s y_t D_s D_t / (T_i (T_i - 1) / 2),
# and the same-side count 1{all y = 1 or all y = 0}. Observed values per respondent:
.rum_ms_observed_moments <- function(data, dirs, scale) {
  n <- data$n; K <- ncol(dirs); out <- matrix(0, n, K, dimnames = list(data$ids, colnames(dirs)))
  same <- all1 <- numeric(n)
  for (i in seq_len(n)) {
    ix <- data$offsets[i] + seq_len(data$T[i]); y <- data$y[ix]; ti <- length(ix)
    if (ti > 1L) {
      X <- sweep(dirs[ix, , drop = FALSE], 2L, scale, "/") * y
      out[i, ] <- (colSums(X)^2 - colSums(X^2)) / 2 / (ti * (ti - 1) / 2)
    }
    all1[i] <- all(y == 1); same[i] <- all1[i] || all(y == 0)
  }
  list(pair = out, same = same, all1 = all1)
}

# Expected values under a truth on given designs (product Gauss-Hermite in u1, u2, v).
.rum_ms_expected_moments <- function(truth, data, dirs, scale, order = c(15L, 15L, 11L)) {
  n <- data$n; K <- ncol(dirs); L <- truth$L
  g1 <- .rum_gh_rule(order[1]); two <- any(L[, 2L] != 0)
  g2 <- if (two) .rum_gh_rule(order[2]) else list(z = 0, w = 1)
  g3 <- if (truth$omega > 0) .rum_gh_rule(order[3]) else list(z = 0, w = 1)
  grid <- expand.grid(a = seq_along(g1$z), b = seq_along(g2$z), c = seq_along(g3$z))
  U1 <- g1$z[grid$a]; U2 <- g2$z[grid$b]; V <- g3$z[grid$c]
  W <- g1$w[grid$a] * g2$w[grid$b] * g3$w[grid$c]
  out <- matrix(0, n, K, dimnames = list(data$ids, colnames(dirs))); same <- all1 <- numeric(n)
  for (i in seq_len(n)) {
    ix <- data$offsets[i] + seq_len(data$T[i]); ti <- length(ix); d <- data$d[ix, , drop = FALSE]
    base <- truth$kappa + drop(d %*% truth$mu[i, ])
    eta <- base + outer(drop(d %*% L[, 1L]), U1) + sqrt(truth$omega) * matrix(V, ti, length(W), byrow = TRUE)
    if (two) eta <- eta + outer(drop(d %*% L[, 2L]), U2)
    P <- stats::plogis(eta)
    p1 <- sum(W * apply(P, 2L, prod)); p0 <- sum(W * apply(1 - P, 2L, prod))
    if (ti > 1L) {
      Dn <- sweep(dirs[ix, , drop = FALSE], 2L, scale, "/")
      for (k in seq_len(K)) {
        X <- Dn[, k] * P
        out[i, k] <- sum(W * (colSums(X)^2 - colSums(X^2)) / 2) / (ti * (ti - 1) / 2)
      }
      # straight-liner: y_s = y_t = 1 with probability s1 (1 - eps)^2 + (1 - s1) eps^2
      if (truth$pi_sl > 0) {
        e <- truth$sl_flip; s1 <- truth$sl_side1
        sl_pair <- (s1 * (1 - e)^2 + (1 - s1) * e^2) * ((colSums(Dn)^2 - colSums(Dn^2)) / 2) / (ti * (ti - 1) / 2)
        out[i, ] <- (1 - truth$pi_sl) * out[i, ] + truth$pi_sl * sl_pair
      }
    }
    if (truth$pi_sl > 0) {
      e <- truth$sl_flip; s1 <- truth$sl_side1
      sl1 <- s1 * (1 - e)^ti + (1 - s1) * e^ti; sl0 <- s1 * e^ti + (1 - s1) * (1 - e)^ti
      p1 <- (1 - truth$pi_sl) * p1 + truth$pi_sl * sl1; p0 <- (1 - truth$pi_sl) * p0 + truth$pi_sl * sl0
    }
    all1[i] <- p1; same[i] <- p1 + p0
  }
  list(pair = out, same = same, all1 = all1)
}

# Straight-liner fraction and side share that close the observed excess of same-side
# respondents over a truth's prediction (on the observed designs):
#   pi = (S_obs - S_pred) / (n - S_pred), q = excess on side 1 / total excess (clipped to [0, 1]).
.rum_ms_calibrate_pi <- function(S_obs, S_pred, S1_obs, S1_pred, n) {
  pi <- max(0, (S_obs - S_pred) / (n - S_pred))
  ex1 <- S1_obs - S1_pred; ex0 <- (S_obs - S1_obs) - (S_pred - S1_pred)
  q <- if (ex1 + ex0 > 0) min(1, max(0, ex1 / (ex1 + ex0))) else 0.5
  c(pi = pi, q = q)
}
