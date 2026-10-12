# Conditional (per-respondent) information for the RUM one-step correction
# (info_basis "conditional"; Avi's memo of 2026-10-08, item 1; paper Appendix D).
#
# What it computes. Respondent i's information I(z_i) = E[s s' | Z = z_i], where s is the
# score of the respondent's marginal sequence likelihood (the posterior expectation, over
# the random effects u and v, of the complete-data score), and the expectation runs over
# the choices AND over the design distribution given z_i. At a fold's fit (theta, mu(z_i)):
#   I(z_i) = (1/M) sum_m I(z_i, D_m),   I(z, D) = sum_y p_Q(y | D, z) s_Q(y) s_Q(y)',
# where D_1..D_M are task sequences drawn from the experiment's own randomization law
# (R/rum-design.R) and coded with respondent i's own covariates (in Graham-Svolik, the
# party key; a respondent who answered T_i tasks uses the first T_i tasks of each draw),
# and p_Q and s_Q integrate the whole sequence over ONE latent quadrature Q. The outer
# product is taken after the posterior averaging. No held-out choices are used. Common
# random numbers: every respondent uses the same M design draws (and, for the simulation
# methods, the same choice uniforms), from a private seed; the caller's RNG is untouched.
#
# Three ways to get I(z, D):
#   "exact"  enumerates all 2^T choice sequences (p and s on the same quadrature), so
#            sum_y p(y) s(y) = 0 holds to rounding;
#   "louis"  simulates ny choice sequences per design from p_Q and uses the Louis identity,
#            observed information = posterior mean of the complete-data negative Hessian
#            minus the posterior variance of the complete-data score. With louis_rb = TRUE
#            (default) the first term is replaced by its expectation over y, which is exact
#            (sum over nodes of w_g sum_t p(1-p) x x'); its expectation equals I(z, D);
#   "mc"     the same simulated sequences, averaging s s' (the old trial's estimator, but
#            with the right designs).
#
# Structure used for speed. Write the score as s = A_D c(y), where c(y) stacks, for each
# task t, the posterior means of the residual e_t = y_t - p_t, of e_t u and of e_t v
# (3T numbers), and A_D (D x 3T) maps them to (s_mu = sum_t d_t R0_t, s_kappa = sum_t R0_t,
# s_loading = sum_t d_t R1_t, s_omega = sum_t R2_t / (2 sqrt(omega))), the column order of
# .rum_seq_scores(). Then I(z, D) = A_D K A_D' with K = E_y[c c'] (3T x 3T): only K depends
# on the respondent's mu. The complete-data analogue c_g of node g has
# E_g[c_g c_g'] = H3 (block diagonal over tasks), so the Louis identity holds in this
# 3T space: E_y[c c'] = H3 - E_y Var_post(c_g | y). (The omega coordinate's second
# derivative term (y - p) d2eta/domega2 has expectation zero; it enters only louis_rb = FALSE.)

# All 2^T binary sequences (rows), as a numeric matrix.
.rum_cond_Y <- function(T) {
  Y <- as.matrix(expand.grid(rep(list(c(0, 1)), T), KEEP.OUT.ATTRS = FALSE))
  dimnames(Y) <- NULL
  Y
}

# A_D: the D x 3T map from c(y) to the score, for one design (T x p) and s = sqrt(omega).
.rum_cond_A <- function(Dm, s) {
  T <- nrow(Dm); p <- ncol(Dm); D <- 2L * p + 2L; A <- matrix(0, D, 3L * T)
  A[seq_len(p), seq_len(T)] <- t(Dm); A[p + 1L, seq_len(T)] <- 1
  A[p + 1L + seq_len(p), T + seq_len(T)] <- t(Dm); A[D, 2L * T + seq_len(T)] <- 1 / (2 * s)
  A
}

# Linear predictors on the quadrature for one respondent and one design: a = kappa + D mu
# (length T), b = D loading (length T). Returns E (T x G), P, and the log probabilities.
.rum_cond_eta <- function(a, b, s, quad) {
  E <- a + outer(b, quad$u) + matrix(s * quad$v, length(a), length(quad$u), byrow = TRUE)
  list(P = stats::plogis(E), L1 = stats::plogis(E, log.p = TRUE), L0 = stats::plogis(-E, log.p = TRUE))
}

# Posterior over the nodes for each row of Y (R x T): returns the posterior weights Pi
# (R x G) and the log marginal probabilities log p_Q(y).
.rum_cond_post <- function(Y, e, lw) {
  R <- nrow(Y); G <- length(lw)
  LL <- Y %*% (e$L1 - e$L0) + matrix(colSums(e$L0) + lw, R, G, byrow = TRUE)
  mx <- LL[cbind(seq_len(R), max.col(LL, ties.method = "first"))]
  Q <- exp(LL - mx); den <- rowSums(Q)
  list(Pi = Q / den, logp = mx + log(den))
}

# c(y) for each row of Y given the posterior Pi: R x 3T, columns (R0_t, R1_t, R2_t).
.rum_cond_c <- function(Y, Pi, e, quad) {
  T <- ncol(Y); Pt <- t(e$P)
  Mx <- Pi %*% cbind(Pt, Pt * quad$u, Pt * quad$v, quad$u, quad$v)
  cbind(Y - Mx[, seq_len(T), drop = FALSE],
        Y * Mx[, 3L * T + 1L] - Mx[, T + seq_len(T), drop = FALSE],
        Y * Mx[, 3L * T + 2L] - Mx[, 2L * T + seq_len(T), drop = FALSE])
}

# H3 = sum_g w_g sum_t p_tg (1 - p_tg) b_tg b_tg' with b_tg = (e_t, u_g e_t, v_g e_t):
# the complete-data information in the 3T space (exact over y).
.rum_cond_H3 <- function(e, quad) {
  T <- nrow(e$P); u <- quad$u; v <- quad$v
  H6 <- (e$P * (1 - e$P)) %*% (quad$weight * cbind(1, u, v, u * u, u * v, v * v))
  H <- matrix(0, 3L * T, 3L * T); t1 <- seq_len(T); t2 <- T + t1; t3 <- 2L * T + t1
  H[cbind(t1, t1)] <- H6[, 1]; H[cbind(t1, t2)] <- H[cbind(t2, t1)] <- H6[, 2]
  H[cbind(t1, t3)] <- H[cbind(t3, t1)] <- H6[, 3]; H[cbind(t2, t2)] <- H6[, 4]
  H[cbind(t2, t3)] <- H[cbind(t3, t2)] <- H6[, 5]; H[cbind(t3, t3)] <- H6[, 6]
  H
}

# K = E_y[c c'] (3T x 3T) for one respondent and one design, by the chosen method.
# a, b: see .rum_cond_eta. Yall: all 2^T sequences (exact). sim: list(g = node index per
# simulated sequence, U = ny x T uniforms) (louis, mc). Returns list(K, xo, check): xo is
# the scalar added to I[omega, omega] (louis_rb = FALSE only); check (exact only) holds
# max |sum_y p(y) c(y)| and |sum_y p(y) - 1|.
.rum_cond_K <- function(a, b, s, quad, method, Yall = NULL, sim = NULL, louis_rb = TRUE) {
  e <- .rum_cond_eta(a, b, s, quad); lw <- log(quad$weight); T <- length(a)
  if (method == "exact") {
    po <- .rum_cond_post(Yall, e, lw); cc <- .rum_cond_c(Yall, po$Pi, e, quad); py <- exp(po$logp)
    return(list(K = crossprod(cc * py, cc), xo = 0,
                check = c(max(abs(colSums(cc * py))), abs(sum(py) - 1))))
  }
  ny <- length(sim$g)
  Ys <- (sim$U[, seq_len(T), drop = FALSE] < t(e$P[, sim$g, drop = FALSE])) + 0
  po <- .rum_cond_post(Ys, e, lw); cc <- .rum_cond_c(Ys, po$Pi, e, quad)
  if (method == "mc") return(list(K = crossprod(cc) / ny, xo = 0, check = c(NA_real_, NA_real_)))
  # louis: E_y[c c'] = H3 - E_y Var_post(c_g | y); posterior second moments of c_g over
  # the stacked (sequence, node) rows, weighted by the posterior.
  G <- length(quad$weight); kk <- rep(seq_len(ny), G); gg <- rep(seq_len(G), each = ny)
  er <- Ys[kk, , drop = FALSE] - t(e$P)[gg, , drop = FALSE]; pw <- as.vector(po$Pi)
  Cr <- cbind(er, er * quad$u[gg], er * quad$v[gg])
  Vpost <- (crossprod(Cr * pw, Cr) - crossprod(cc)) / ny
  if (louis_rb) return(list(K = .rum_cond_H3(e, quad) - Vpost, xo = 0, check = c(NA_real_, NA_real_)))
  # posterior mean of the complete-data negative Hessian, sequence by sequence
  q <- t(e$P * (1 - e$P))[gg, , drop = FALSE] * pw
  H3p <- matrix(0, 3L * T, 3L * T); t1 <- seq_len(T)
  for (r in 0:2) for (c2 in 0:2) {
    wrc <- colSums(q * (if (r == 1) quad$u[gg] else if (r == 2) quad$v[gg] else 1) *
                       (if (c2 == 1) quad$u[gg] else if (c2 == 2) quad$v[gg] else 1))
    H3p[cbind(r * T + t1, c2 * T + t1)] <- wrc / ny
  }
  xo <- sum(cc[, 2L * T + t1]) / ny / (4 * s^3)                 # posterior mean of (y-p) v/(4 s^3)
  list(K = H3p - Vpost, xo = xo, check = c(NA_real_, NA_real_))
}

# Per-respondent conditional information: n x D x D array (data$ids order), the same shape
# as .rum_model_info(). crn: designs from .rum_draw_designs_crn() for these respondents
# (list(designs, key, M, T); key[i] picks respondent i's coded array, M x T x p). halves =
# TRUE also returns the averages over odd and even design draws (IA, IB; two independent
# halves for Monte Carlo error checks). ny: simulated choice sequences per design (louis,
# mc). Returns list(I, IA, IB, M, method, ny, check) with check = (max |sum_y p s| in the
# 3T space, max |sum_y p - 1|) for the exact method.
.rum_cond_info <- function(data, mu, th, crn, method = c("exact", "louis", "mc"),
                           quad = .rum_info_quadrature(), ny = 8L, seed = 20261011L,
                           halves = FALSE, louis_rb = TRUE) {
  method <- match.arg(method)
  n <- nrow(mu); p <- ncol(mu); D <- 2L * p + 2L; s <- sqrt(th$omega); M <- as.integer(crn$M)
  Tj <- as.integer(data$T); G <- length(quad$weight)
  stopifnot(n == data$n, p == data$p, length(crn$key) == n, M >= 2L, all(Tj <= crn$T),
            all(vapply(crn$designs, function(x) dim(x)[3L], 1L) == p))
  if (method == "exact" && max(Tj) > 16L) stop("method 'exact' enumerates 2^T sequences; T > 16 is too many")
  sims <- if (method != "exact") .rum_with_seed(seed, list(
    g = matrix(sample.int(G, M * ny, replace = TRUE, prob = quad$weight), M, ny),
    U = array(stats::runif(M * ny * crn$T), c(M, ny, crn$T)))) else NULL
  I <- array(0, c(n, D, D)); IA <- IB <- if (halves) array(0, c(n, D, D)) else NULL
  chk <- c(0, 0); Ys <- list(); nA <- ceiling(M / 2)
  for (grp in split(seq_len(n), paste(crn$key, Tj))) {
    T <- Tj[grp[1L]]; dk <- crn$designs[[crn$key[grp[1L]]]]; ng <- length(grp); J <- 3L * T
    if (method == "exact" && is.null(Ys[[as.character(T)]])) Ys[[as.character(T)]] <- .rum_cond_Y(T)
    Yall <- Ys[[as.character(T)]]; MUg <- mu[grp, , drop = FALSE]
    acc <- accA <- array(0, c(D, ng, D))             # (b, respondent, a): I_j[b, a], summed over designs
    for (m in seq_len(M)) {
      Dm <- matrix(dk[m, seq_len(T), ], T, p)
      Am <- .rum_cond_A(Dm, s); amat <- th$kappa + MUg %*% t(Dm); b <- drop(Dm %*% th$loading)
      sim <- if (is.null(sims)) NULL else list(g = sims$g[m, ], U = matrix(sims$U[m, , ], ny))
      KK <- matrix(0, J * J, ng); xo <- numeric(ng)
      for (j in seq_len(ng)) {
        r <- .rum_cond_K(amat[j, ], b, s, quad, method, Yall, sim, louis_rb)
        KK[, j] <- r$K; xo[j] <- r$xo
        if (method == "exact") chk <- pmax(chk, r$check)
      }
      # A_m K_j A_m' for every respondent of the group, in two matrix products:
      # X = A_m [K_1 .. K_ng] (D x J ng); then A_m X_j' for all j at once
      X <- Am %*% matrix(KK, J, J * ng)
      Cm <- array(Am %*% matrix(t(X), J, ng * D), c(D, ng, D))
      if (any(xo != 0)) Cm[D, , D] <- Cm[D, , D] + xo
      acc <- acc + Cm
      if (halves && m %% 2L == 1L) accA <- accA + Cm
    }
    I[grp, , ] <- aperm(acc, c(2L, 1L, 3L))
    if (halves) { IA[grp, , ] <- aperm(accA, c(2L, 1L, 3L)); IB[grp, , ] <- aperm(acc - accA, c(2L, 1L, 3L)) }
  }
  I <- I / M
  if (halves) { IA <- IA / nA; IB <- IB / (M - nA) }
  list(I = I, IA = IA, IB = IB, M = M, method = method, ny = if (method == "exact") NA_integer_ else ny,
       check = if (method == "exact") chk else c(NA_real_, NA_real_))
}

# Respondents whose information hits the eigenvalue floor of .rum_weight_parts(), and the
# floored directions. Returns a data frame with one row per (respondent, floored
# eigenvalue): row (respondent index), id, eig_rel (eigenvalue / largest), and the
# coordinates carrying the eigenvector (|weight| >= 0.3, as name:weight).
.rum_cond_floor_report <- function(I, eig_floor_rel = 1e-6, coord_names = NULL, ids = NULL) {
  n <- dim(I)[1L]; D <- dim(I)[2L]
  if (is.null(coord_names)) coord_names <- paste0("c", seq_len(D))
  out <- list()
  for (i in seq_len(n)) {
    e <- eigen(I[i, , ], symmetric = TRUE); fl <- eig_floor_rel * max(e$values)
    for (k in which(e$values < fl)) {
      v <- e$vectors[, k]; big <- which(abs(v) >= 0.3); big <- big[order(-abs(v[big]))]
      out[[length(out) + 1L]] <- data.frame(row = i, id = if (is.null(ids)) NA_character_ else ids[i],
        eig_rel = e$values[k] / max(e$values),
        direction = paste(sprintf("%s:%.2f", coord_names[big], v[big]), collapse = " "),
        stringsAsFactors = FALSE)
    }
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(row = integer(0), id = character(0), eig_rel = numeric(0), direction = character(0))
}

# Options of the conditional information (passed to the inference routines as cond = ...).
# spec: a design spec or study string ("gs", "br", "sw_additive", "sw_explicit"); resp:
# respondent inputs for the design key in data$ids order (GS: a data frame with pid7);
# method, ny, louis_rb: see .rum_cond_info(); M: design draws (default 200 for exact,
# 400 for louis and mc). crn: optional fixed design draws (a .rum_draw_designs_crn()-shaped
# list) used for every fold instead of spec (for tests and custom designs).
.rum_cond_options <- function(spec, resp = NULL, method = c("exact", "louis", "mc"), M = NULL,
                              ny = 8L, louis_rb = TRUE, seed = 20261011L,
                              quad = .rum_info_quadrature(), cache_dir = NULL, cache_net = NULL,
                              cache_write = TRUE, cache_require = FALSE, crn = NULL) {
  method <- match.arg(method)
  list(spec = spec, resp = resp, method = method, crn = crn,
       M = if (is.null(M)) (if (method == "exact") 200L else 400L) else as.integer(M),
       ny = as.integer(ny), louis_rb = louis_rb, seed = as.integer(seed), quad = quad,
       cache_dir = cache_dir, cache_net = cache_net, cache_write = cache_write,
       cache_require = cache_require)
}

# One fold's conditional information, mirroring .rum_model_info_fold(): designs drawn with
# the fold's private seed (seed + k), cached at <cache_dir>/<cache_net>_fold<k>_<method>_M<M>
# [_ny<ny>].rds keyed by a hash of the fold's final state, mu, the respondents' design keys
# and task counts, the study, M, ny, the quadrature and the seed. Returns list(I, M, seconds,
# cache, hit, check).
.rum_cond_info_fold <- function(data, mu, th, k, cond, fin) {
  if (is.null(cond)) stop("info_basis 'conditional' needs cond = .rum_cond_options(...)")
  n <- data$n; D <- 2L * data$p + 2L; M <- as.integer(if (is.null(cond$crn)) cond$M else cond$crn$M); sd_k <- cond$seed + as.integer(k)
  spec <- if (is.null(cond$crn)) .rum_spec_arg(cond$spec) else NULL
  key_resp <- if (!is.null(cond$crn)) cond$crn$key else if (spec$uses_resp) spec$key(cond$resp) else rep("all", n)
  stopifnot(length(key_resp) == n)
  f <- NULL; key <- NULL; tag <- if (cond$method == "exact") "exact" else
    sprintf("%s%s_ny%d", cond$method, if (cond$method == "louis" && !cond$louis_rb) "nrb" else "", cond$ny)
  if (!is.null(cond$cache_net) && !is.null(cond$cache_dir)) {
    if (requireNamespace("digest", quietly = TRUE)) {
      key <- digest::digest(list(version = "rum_cond_info v1", final = fin[c("network", "kappa", "loading", "omega")],
        mu = unname(mu), ids = data$ids, T = data$T, keys = key_resp,
        study = if (is.null(spec)) cond$crn$designs else spec$study, M = M,
        method = cond$method, ny = cond$ny, louis_rb = cond$louis_rb,
        quad = unclass(cond$quad)[c("u", "v", "weight")], seed = sd_k))
      f <- file.path(cond$cache_dir, sprintf("%s_fold%d_%s_M%d.rds", cond$cache_net, k, tag, M))
    } else message("package 'digest' is not installed; the conditional information is not cached")
  }
  if (!is.null(f) && file.exists(f)) {
    C <- readRDS(f)
    if (identical(C$key, key))
      return(list(I = .rum_info_unpack(C$info, n, D), M = M, seconds = C$seconds, cache = f,
                  hit = TRUE, check = C$check))
    if (cond$cache_require) stop(sprintf("conditional information cache %s does not match this fold's state", basename(f)))
    message(sprintf("conditional information cache %s does not match this fold's state; recomputing", basename(f)))
  } else if (isTRUE(cond$cache_require)) stop("conditional information cache required but not found: ",
                                               if (is.null(f)) "no cache file" else f)
  t0 <- proc.time()[["elapsed"]]
  crn <- if (is.null(cond$crn)) .rum_draw_designs_crn(spec, cond$resp, M, seed = sd_k, n = n) else cond$crn
  M <- as.integer(crn$M)
  R <- .rum_cond_info(data, mu, th, crn, cond$method, cond$quad, cond$ny, sd_k + 7919L,
                      louis_rb = cond$louis_rb)
  pk <- .rum_info_pack(R$I); secs <- proc.time()[["elapsed"]] - t0
  if (!is.null(f) && isTRUE(cond$cache_write)) {
    dir.create(cond$cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(f, ".tmp", Sys.getpid())
    saveRDS(list(key = key, info = pk, n = n, D = D, M = M, method = cond$method, ny = cond$ny,
                 louis_rb = cond$louis_rb, study = if (is.null(spec)) "crn" else spec$study,
                 quad_order = cond$quad$order,
                 seed = sd_k, ids = data$ids, network = cond$cache_net, fold = k, seconds = secs,
                 check = R$check, created = format(Sys.time()),
                 storage = "upper triangles, float32, respondent-major"), tmp)
    file.rename(tmp, f)
  }
  list(I = .rum_info_unpack(pk, n, D), M = M, seconds = secs, cache = f, hit = FALSE, check = R$check)
}
