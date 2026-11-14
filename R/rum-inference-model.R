# Model-based conditional information for the RUM one-step correction (info_basis "model").
# Ported from scripts_C/rum_debias.R of the RUM reproduction (Phase C, 2026-09-26), with
# the arithmetic unchanged: .rum_seq_scores, .rum_model_info (was rum_model_info),
# .rum_info_pack/.rum_info_unpack, .rum_model_info_fold (was .model_info_fold, now with an
# optional cache), .rum_weight_parts (was .model_weight_parts, now also used by the
# constant and linear bases), .rum_info_CLv and .rum_info_a (were .model_CLv, .model_a).
#
# Why. With one average information matrix, the correction weights solve the orthogonality
# condition only on average over z, and a first-order bias remains. Under the conditional
# model and a design randomized independently of z, respondent i's information
# I(z_i) = E[s s' | z_i] depends on z only through mu(z_i) for given theta. At each fold's
# fit it is computed by averaging, over M draws, the outer product of the exact sequence
# score s = (s_mu, s_theta) of a synthetic respondent with respondent i's mu, the fold's
# theta, a design drawn from the bank's own designs, and choices simulated from the model.
# No held-out choices are used. Common random numbers: every respondent of a fold uses the
# same M draws, from a private seed (seed + fold); the caller's random stream is untouched.
# Inverting an average of M outer products inflates the weights by about M / (M - p - 1),
# so the default is M = 20 D (D = p + n_th = 2p + 2 at one factor).
#
# Known limitation, kept on purpose (Codex review of eae4719, finding 1). The synthetic
# respondents' designs are drawn from the whole bank of ALREADY-CODED sequences (dr$j below),
# so respondent i borrows a donor's coded design and the donor's task count. Where the coding
# depends on the respondent (Graham-Svolik: the co-partisan column is coded from the
# respondent's party and is always 0 for pure independents), an independent can receive a
# partisan's nonzero co-partisan column, and its model information then shows identification
# its own design does not have. This routine is left unchanged so that the "model" basis
# reproduces Phase C exactly (parity tests). The fix is the conditional information basis
# (agent B2, branch wip/rum-info: designs generated from the experiment's law and coded for the
# recipient's own design key and task count, R/rum-design.R generators); use it, not this
# basis, wherever the coding depends on the respondent.

# Exact RUM sequence scores for B synthetic respondents that share the task count T.
# MU: B x p means; DL: list of T design matrices (B x p); Y: B x T choices; th: list(kappa,
# loading, omega); quad: a rum_quadrature rule. Columns: s_mu[1..p], kappa, loading[1..p],
# omega (the evaluator's order). Equals rum_dnn_gaussian_evaluate()'s scores. omega score:
# E[sum_t r_t v | Y] / (2 sqrt(omega)) in the interior; when omega is below .rum_omega_small
# the boundary form E[(sum_t r_t)^2 - sum_t G'(e_t) | Y] / 2 (the evaluator's formula at
# omega = 0, the right derivative; integration by parts in v of the interior form), so there is
# no division by sqrt(omega).
.rum_seq_scores <- function(MU, DL, Y, th, quad) {
  if (.rum_th_nfac(th) > 1L) return(.rum_seq_scores_q(MU, DL, Y, th, quad))
  B <- nrow(MU); p <- ncol(MU); T <- length(DL); G <- length(quad$weight); s <- sqrt(th$omega)
  um <- matrix(quad$u, B, G, byrow = TRUE); vm <- matrix(quad$v, B, G, byrow = TRUE); svm <- s * vm
  ETA <- vector("list", T); ll <- matrix(0, B, G)
  for (t in seq_len(T)) {
    e <- th$kappa + rowSums(DL[[t]] * MU) + drop(DL[[t]] %*% th$loading) * um + svm; ETA[[t]] <- e
    ll <- ll + Y[, t] * stats::plogis(e, log.p = TRUE) + (1 - Y[, t]) * stats::plogis(-e, log.p = TRUE)
  }
  ll <- sweep(ll, 2, log(quad$weight), "+"); P <- exp(ll - apply(ll, 1, max)); P <- P / rowSums(P)
  smu <- slam <- matrix(0, B, p); sk <- som <- numeric(B)
  boundary <- th$omega < .rum_omega_small; RS <- CV <- 0
  for (t in seq_len(T)) {
    pr <- stats::plogis(ETA[[t]]); r <- P * (Y[, t] - pr); R0 <- rowSums(r)
    smu <- smu + DL[[t]] * R0; slam <- slam + DL[[t]] * rowSums(r * um); sk <- sk + R0
    if (boundary) { RS <- RS + (Y[, t] - pr); CV <- CV + pr * (1 - pr) }
    else som <- som + rowSums(r * vm)
  }
  if (boundary) return(cbind(smu, sk, slam, rowSums(P * 0.5 * (RS^2 - CV))))
  cbind(smu, sk, slam, som / (2 * s))
}

# The same with q = ncol(th$loading) > 1 taste factors: eta = kappa + d'mu + d'Lambda u + s v with
# u (G x q, quad$u) and the loading score for factor k, sum_t d_t E[e_t u_k | Y], in the
# column-major layout of theta (factor 1's p entries first). Columns: s_mu[1..p], kappa,
# vec(Lambda) (p q), omega (.rum_th_idx(p, q, offset = p)). Below .rum_omega_small (the C++ kernel's
# threshold, one definition) the omega score is E[v^2 (R^2 - sum_t p_t(1-p_t)) | Y] / 2, the C++ kernel's
# boundary form: for a rule symmetric in v the limit of the interior form (see src/rum_core.cpp).
.rum_seq_scores_q <- function(MU, DL, Y, th, quad) {
  B <- nrow(MU); p <- ncol(MU); T <- length(DL); G <- length(quad$weight); s <- sqrt(th$omega)
  L <- as.matrix(th$loading); nf <- ncol(L)
  if (.rum_quad_nfac(quad) != nf) stop("the quadrature rule does not have ncol(loading) taste factors")
  U <- matrix(quad$u, G, nf); vm <- matrix(quad$v, B, G, byrow = TRUE); svm <- s * vm
  ETA <- vector("list", T); ll <- matrix(0, B, G)
  for (t in seq_len(T)) {
    e <- th$kappa + rowSums(DL[[t]] * MU) + (DL[[t]] %*% L) %*% t(U) + svm; ETA[[t]] <- e
    ll <- ll + Y[, t] * stats::plogis(e, log.p = TRUE) + (1 - Y[, t]) * stats::plogis(-e, log.p = TRUE)
  }
  ll <- sweep(ll, 2, log(quad$weight), "+"); P <- exp(ll - apply(ll, 1, max)); P <- P / rowSums(P)
  smu <- matrix(0, B, p); slam <- matrix(0, B, p * nf); sk <- som <- numeric(B)
  boundary <- th$omega < .rum_omega_small; RS <- CV <- 0
  for (t in seq_len(T)) {
    pr <- stats::plogis(ETA[[t]]); r <- P * (Y[, t] - pr); R0 <- rowSums(r)
    smu <- smu + DL[[t]] * R0; sk <- sk + R0; RU <- r %*% U          # B x q: E[e_t u_k | Y]
    for (k in seq_len(nf)) slam[, (k - 1L) * p + seq_len(p)] <- slam[, (k - 1L) * p + seq_len(p)] + DL[[t]] * RU[, k]
    if (boundary) { RS <- RS + (Y[, t] - pr); CV <- CV + pr * (1 - pr) }
    else som <- som + rowSums(r * vm)
  }
  if (boundary) return(cbind(smu, sk, slam, rowSums(P * 0.5 * vm^2 * (RS^2 - CV))))
  cbind(smu, sk, slam, som / (2 * s))
}

# Per-respondent model information: n x D x D array (data$ids order). halves = TRUE also
# returns the averages over odd and even draws (two independent estimates, for Monte Carlo
# error checks). Respondents are processed in blocks of at most max_cells score cells.
.rum_model_info <- function(data, mu, th, M, quad = .rum_info_quadrature(), seed = 20260926L,
                            halves = FALSE, max_cells = 2e7) {
  nf <- .rum_th_nfac(th)
  n <- nrow(mu); p <- ncol(mu); D <- p + .rum_th_len(p, nf); G <- length(quad$weight); Tmax <- max(data$T)
  s <- sqrt(th$omega)
  if (.rum_quad_nfac(quad) != nf) stop("the quadrature rule does not have the state's number of taste factors")
  stopifnot(n == data$n, p == data$p, M >= 2L)
  off <- data$offsets; Tj <- data$T; nb <- max(1L, floor(max_cells / (M * G * Tmax)))
  I <- array(0, c(n, D, D)); IA <- IB <- if (halves) array(0, c(n, D, D)) else NULL
  oddm <- seq(1L, M, 2L); evenm <- seq(2L, M, 2L)
  # common random numbers: the same M designs, taste and side draws and uniforms for every
  # respondent (drawn once, so the result does not depend on batching)
  # (n_fac > 1: u is M x n_fac, drawn in the same position of the stream)
  dr <- if (nf == 1L) .rum_with_seed(seed, list(j = sample.int(data$n, M, replace = TRUE),
    u = stats::rnorm(M), v = stats::rnorm(M), U = matrix(stats::runif(M * Tmax), M, Tmax))) else
    .rum_with_seed(seed, list(j = sample.int(data$n, M, replace = TRUE),
    u = matrix(stats::rnorm(M * nf), M, nf), v = stats::rnorm(M), U = matrix(stats::runif(M * Tmax), M, Tmax)))
  for (bi in split(seq_len(n), ceiling(seq_len(n) / nb))) {
    nr <- length(bi); np <- nr * M; ii <- rep(bi, each = M); mm <- rep(seq_len(M), times = nr)
    jm <- dr$j[mm]; um <- if (nf == 1L) dr$u[mm] else dr$u[mm, , drop = FALSE]
    vm <- dr$v[mm]; U <- dr$U[mm, , drop = FALSE]
    Sc <- matrix(NA_real_, np, D)
    for (T in sort(unique(Tj[jm]))) {
      r <- which(Tj[jm] == T); MU <- mu[ii[r], , drop = FALSE]
      DL <- lapply(seq_len(T), function(t) data$d[off[jm[r]] + t, , drop = FALSE])
      Y <- matrix(0, length(r), T)
      if (nf == 1L) {
        for (t in seq_len(T)) Y[, t] <- as.numeric(U[r, t] < stats::plogis(th$kappa +
          rowSums(DL[[t]] * (MU + outer(um[r], th$loading))) + s * vm[r]))
      } else {
        BU <- MU + um[r, , drop = FALSE] %*% t(th$loading)          # beta = mu + Lambda u
        for (t in seq_len(T)) Y[, t] <- as.numeric(U[r, t] < stats::plogis(th$kappa +
          rowSums(DL[[t]] * BU) + s * vm[r]))
      }
      Sc[r, ] <- .rum_seq_scores(MU, DL, Y, th, quad)
    }
    for (j in seq_len(nr)) {
      X <- Sc[(j - 1L) * M + seq_len(M), , drop = FALSE]; I[bi[j], , ] <- crossprod(X) / M
      if (halves) {
        IA[bi[j], , ] <- crossprod(X[oddm, , drop = FALSE]) / length(oddm)
        IB[bi[j], , ] <- crossprod(X[evenm, , drop = FALSE]) / length(evenm)
      }
    }
  }
  list(I = I, IA = IA, IB = IB, M = M)
}

# Float32 round trip of the upper triangles (the cache's storage), applied to fresh results
# too, so a cached run and a fresh run give identical numbers.
.rum_info_pack <- function(I) {
  D <- dim(I)[2]; ut <- upper.tri(diag(D), diag = TRUE)
  writeBin(as.vector(t(vapply(seq_len(dim(I)[1]), function(i) I[i, , ][ut], numeric(sum(ut))))),
           raw(), size = 4)
}
.rum_info_unpack <- function(raw_ut, n, D) {
  V <- matrix(readBin(raw_ut, "double", n = n * D * (D + 1) / 2, size = 4), n)
  ut <- upper.tri(diag(D), diag = TRUE)
  I <- array(0, c(n, D, D))
  for (i in seq_len(n)) {
    L <- matrix(0, D, D); L[ut] <- V[i, ]; L[lower.tri(L)] <- t(L)[lower.tri(L)]; I[i, , ] <- L
  }
  I
}

# One fold's model information. With cache_net and cache_dir given (and the 'digest'
# package available), it is cached at <cache_dir>/<cache_net>_fold<k>_M<M>.rds, keyed by a
# hash of the fold's final state (network, kappa, loading, omega), the network's outputs mu,
# the bank's designs, M, the quadrature and the seed (the same key and file layout as the
# reproduction scripts, so their caches are read as is). A file whose key does not match is
# recomputed and, when cache_write is TRUE, overwritten, with a message. cache_require =
# TRUE stops instead of computing (for parity checks that must read a cache). Returns
# list(I, M, seconds, cache, hit).
.rum_model_info_fold <- function(data, mu, th, k, M, quad, seed, cache_net, cache_dir, fin,
                                 cache_write = TRUE, cache_require = FALSE) {
  nf <- .rum_th_nfac(th)
  if (NROW(th$loading) != data$p) stop("the loading does not have p rows")
  quad <- .rum_quad_for(quad, nf, "info")
  n <- data$n; D <- data$p + .rum_th_len(data$p, nf); M <- as.integer(if (is.null(M)) 20L * D else M)
  sd_k <- as.integer(seed) + as.integer(k)
  f <- NULL; key <- NULL
  if (!is.null(cache_net) && !is.null(cache_dir)) {
    if (requireNamespace("digest", quietly = TRUE)) {
      kl <- list(version = "rum_model_info v2 (common draws)",
        final = fin[c("network", "kappa", "loading", "omega")], mu = unname(mu),
        ids = data$ids, d = data$d, offsets = data$offsets, M = M,
        quad = unclass(quad)[c("u", "v", "weight")], seed = sd_k)
      if (nf > 1L) kl$n_fac <- nf                       # one factor keeps the v2 key
      key <- digest::digest(kl)
      f <- file.path(cache_dir, sprintf("%s_fold%d_M%d.rds", cache_net, k, M))
    } else message("package 'digest' is not installed; the model information is not cached")
  }
  if (!is.null(f) && file.exists(f)) {
    C <- readRDS(f)
    if (identical(C$key, key))
      return(list(I = .rum_info_unpack(C$info, n, D), M = M, seconds = C$seconds, cache = f,
                  hit = TRUE))
    if (cache_require) stop(sprintf("model information cache %s does not match this fold's state",
                                    basename(f)))
    message(sprintf("model information cache %s does not match this fold's state; recomputing",
                    basename(f)))
  } else if (cache_require) stop("model information cache required but not found: ",
                                 if (is.null(f)) "no cache file (cache_net, cache_dir, digest)" else f)
  t0 <- proc.time()[["elapsed"]]; R <- .rum_model_info(data, mu, th, M, quad, sd_k)
  pk <- .rum_info_pack(R$I); secs <- proc.time()[["elapsed"]] - t0
  if (!is.null(f) && cache_write) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(f, ".tmp", Sys.getpid())
    saveRDS(list(key = key, info = pk, n = n, D = D, M = M, quad_order = quad$order, seed = sd_k,
                 ids = data$ids, network = cache_net, fold = k, seconds = secs,
                 created = format(Sys.time()),
                 storage = "upper triangles, float32, respondent-major"), tmp)
    file.rename(tmp, f)
  }
  list(I = .rum_info_unpack(pk, n, D), M = M, seconds = secs, cache = f, hit = FALSE)
}

# Ingredients of the correction weights from respondent information matrices. info_of(i)
# returns respondent row i's D x D information (D = p + n_th), for i = 1..nrow. Each matrix
# is made positive definite by an eigenvalue floor (eig_floor_rel times its largest
# eigenvalue). Returns Li = I_mumu^{-1} (nrow x p x p), Lt = I_mutheta (nrow x p x n_th),
# Ic = the average over rows of I_thetatheta - I_thetamu I_mumu^{-1} I_mutheta (the
# efficient information for theta), floored = the number of floored matrices, and
# shared = (nrow == 1): one matrix for every respondent.
# rot (optional, appended last because external scripts call this positionally): the rotation
# directions of the fold's loading in theta coordinates, .rum_rotation_dirs(Lambda) (n_th x
# q(q-1)/2). With q > 1 factors the likelihood is flat along them, exactly for the exact integral
# and up to quadrature error for any rule, so a floor would decide by quadrature error whether
# they are kept. They are removed before the floor instead (.rum_weight_parts_rot): see there.
# rot NULL or with no columns (one factor): the code below, unchanged.
.rum_weight_parts <- function(info_of, nrow, p, n_th, eig_floor_rel, rot = NULL) {
  if (!is.null(rot) && ncol(rot) > 0L)
    return(.rum_weight_parts_rot(info_of, nrow, p, n_th, eig_floor_rel, rot, shared = nrow == 1L))
  .rum_warn_no_rot(p, n_th, rot)
  Li <- array(0, c(nrow, p, p)); Lt <- array(0, c(nrow, p, n_th)); Ic <- matrix(0, n_th, n_th); nfl <- 0L
  for (i in seq_len(nrow)) {
    L <- info_of(i); e <- eigen(L, symmetric = TRUE); fl <- eig_floor_rel * max(e$values)
    if (any(e$values < fl)) { nfl <- nfl + 1L; L <- e$vectors %*% (pmax(e$values, fl) * t(e$vectors)) }
    li <- solve(L[1:p, 1:p, drop = FALSE]); lt <- L[1:p, p + seq_len(n_th), drop = FALSE]  # drop = FALSE: p = 1
    Li[i, , ] <- li; Lt[i, , ] <- lt
    Ic <- Ic + L[p + seq_len(n_th), p + seq_len(n_th)] - crossprod(lt, li %*% lt)
  }
  list(Li = Li, Lt = Lt, Ic = Ic / nrow, floored = nfl, shared = nrow == 1L)
}

# Weight ingredients with the rotation directions removed (build plan D3). B (n_th x (n_th - r))
# is an orthonormal basis of the complement of the rotation directions (.rum_rotation_complement).
# Each respondent's information is written in the coordinates (mu, B' theta),
#   L_red = T' L T,  T = diag(I_p, B),
# i.e. restricted to the perturbations that change the model; the floor, the inverse of the mu
# block and the Schur complement are taken there, and the results are mapped back to theta
# coordinates: Lt = L_red[mu, theta] B' (p x n_th) and the theta block B L_red[theta, theta] B'.
# So Ic (n_th x n_th) annihilates every rotation direction exactly (to rounding), and
# .rum_correction() solves J g = r on the complement only: g = B (B' Ic B)^{-1} B' r. The result
# does not depend on which orthonormal basis B is used, and no eigenvalue cutoff decides whether a
# rotation direction is kept: they are removed by construction. Why it is exact: Lambda and
# Lambda A (A orthogonal) give the same law of the data, so every target and the true likelihood
# are constant along vec(Lambda K), K skew; the efficient influence function lives on the model's
# tangent space, which is the complement; a finite rule's own score along those directions is
# pure quadrature error and carries no information.
# null_zero (the conditional basis's option, .rum_cond_weight_parts): a coordinate whose row of a
# respondent's information is exactly zero is dropped first; the rotation directions are then
# restricted to the kept theta coordinates (where the dropped coordinates carry no information,
# L_KK r_K = L r, so r_K is the same near-null direction of the kept block). Returns
# .rum_weight_parts()'s list plus B, Ic_red = B' Ic B and null (respondents with a zero row).
.rum_weight_parts_rot <- function(info_of, nrow, p, n_th, eig_floor_rel, rot, shared = nrow == 1L,
                                  null_zero = FALSE) {
  D <- p + n_th; Bg <- .rum_rotation_complement(rot)
  if (is.null(Bg)) Bg <- diag(n_th)
  Li <- array(0, c(nrow, p, p)); Lt <- array(0, c(nrow, p, n_th)); Ic <- matrix(0, n_th, n_th)
  nfl <- 0L; nnull <- 0L
  for (i in seq_len(nrow)) {
    L <- info_of(i)
    keep <- if (null_zero) which(rowSums(abs(L)) > 0) else seq_len(D)
    if (length(keep) < D) nnull <- nnull + 1L
    km <- keep[keep <= p]; kt <- keep[keep > p] - p
    B <- if (length(kt) == n_th) Bg else {
      b <- .rum_rotation_complement(rot[kt, , drop = FALSE]); if (is.null(b)) diag(length(kt)) else b }
    nm <- length(km); nb <- ncol(B); im <- seq_len(nm); ir <- nm + seq_len(nb)
    Lx <- L[km, p + kt, drop = FALSE] %*% B
    R <- rbind(cbind(L[km, km, drop = FALSE], Lx), cbind(t(Lx), crossprod(B, L[p + kt, p + kt, drop = FALSE] %*% B)))
    R <- (R + t(R)) / 2
    e <- eigen(R, symmetric = TRUE); fl <- eig_floor_rel * max(e$values)
    if (any(e$values < fl)) { nfl <- nfl + 1L; R <- e$vectors %*% (pmax(e$values, fl) * t(e$vectors)) }
    li <- matrix(0, p, p); li[km, km] <- solve(R[im, im, drop = FALSE])
    lt <- matrix(0, p, n_th); lt[km, kt] <- R[im, ir, drop = FALSE] %*% t(B)
    tt <- matrix(0, n_th, n_th); tt[kt, kt] <- B %*% R[ir, ir, drop = FALSE] %*% t(B)
    Li[i, , ] <- li; Lt[i, , ] <- lt
    Ic <- Ic + tt - crossprod(lt, li %*% lt)
  }
  Ic <- Ic / nrow; Ic <- (Ic + t(Ic)) / 2
  list(Li = Li, Lt = Lt, Ic = Ic, floored = nfl, shared = shared, B = Bg,
       Ic_red = crossprod(Bg, Ic %*% Bg), null = nnull)
}
# A caller that builds weight parts for a q > 1 state (n_th = p q + 2 > p + 2) without passing the
# rotation directions (scripts that call .rum_weight_parts positionally) gets the unprojected path,
# where the eigenvalue floor decides by quadrature error: warn. Silent at one factor.
.rum_warn_no_rot <- function(p, n_th, rot) {
  if (is.null(rot) && n_th > p + 2L)
    warning(sprintf(paste0("weight parts for a %d-factor state without the rotation directions: pass ",
      "rot = .rum_rotation_dirs(loading) so they are removed before the eigenvalue floor"),
      (n_th - 2L) %/% p), call. = FALSE)
}
# The information matrix whose eigenvalues the diagnostics report: Ic, or with the rotation
# directions removed, Ic on their complement.
.rum_parts_Ic <- function(parts) if (is.null(parts$B)) parts$Ic else parts$Ic_red

.rum_slice3 <- function(A, rows, j) matrix(A[rows, j, ], nrow = length(rows))
# Rows (I_thetamu I_mumu^{-1} v_i)' for every respondent (respondent-specific matrices).
.rum_info_CLv <- function(Li, Lt, v) {
  n <- nrow(v); rr <- seq_len(n)
  Liv <- vapply(seq_len(ncol(v)), function(a) rowSums(.rum_slice3(Li, rr, a) * v), numeric(n))
  matrix(vapply(seq_len(dim(Lt)[3]), function(cc) rowSums(matrix(Lt[, , cc], nrow = n) * Liv),
                numeric(n)), nrow = n)
}
# a_i = I_mumu^{-1}(v_i - I_mutheta g) for the given rows (respondent-specific matrices).
.rum_info_a <- function(Li, Lt, v, g, rows) {
  p <- ncol(v)
  wv <- v[rows, , drop = FALSE] - matrix(vapply(seq_len(p), function(a)
    drop(.rum_slice3(Lt, rows, a) %*% g), numeric(length(rows))), nrow = length(rows))
  matrix(vapply(seq_len(p), function(a) rowSums(.rum_slice3(Li, rows, a) * wv),
                numeric(length(rows))), nrow = length(rows))
}
