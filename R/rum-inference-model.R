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
# so the default is M = 20 D (D = 2p + 2).
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
  B <- nrow(MU); p <- ncol(MU); T <- length(DL); G <- length(quad$weight); s <- sqrt(th$omega)
  um <- matrix(quad$u, B, G, byrow = TRUE); vm <- matrix(quad$v, B, G, byrow = TRUE); svm <- s * vm
  ETA <- vector("list", T); ll <- matrix(0, B, G)
  for (t in seq_len(T)) {
    e <- th$kappa + rowSums(DL[[t]] * MU) + drop(DL[[t]] %*% th$loading) * um + svm; ETA[[t]] <- e
    ll <- ll + Y[, t] * stats::plogis(e, log.p = TRUE) + (1 - Y[, t]) * stats::plogis(-e, log.p = TRUE)
  }
  ll <- sweep(ll, 2, log(quad$weight), "+"); P <- exp(ll - apply(ll, 1, max)); P <- P / rowSums(P)
  smu <- slam <- matrix(0, B, p); sk <- som <- numeric(B)
  for (t in seq_len(T)) {
    r <- P * (Y[, t] - stats::plogis(ETA[[t]])); R0 <- rowSums(r)
    smu <- smu + DL[[t]] * R0; slam <- slam + DL[[t]] * rowSums(r * um); sk <- sk + R0
    som <- som + rowSums(r * vm)
  }
  cbind(smu, sk, slam, som / (2 * s))
}

# Per-respondent model information: n x D x D array (data$ids order). halves = TRUE also
# returns the averages over odd and even draws (two independent estimates, for Monte Carlo
# error checks). Respondents are processed in blocks of at most max_cells score cells.
.rum_model_info <- function(data, mu, th, M, quad = .rum_info_quadrature(), seed = 20260926L,
                            halves = FALSE, max_cells = 2e7) {
  n <- nrow(mu); p <- ncol(mu); D <- 2L * p + 2L; G <- length(quad$weight); Tmax <- max(data$T)
  s <- sqrt(th$omega)
  stopifnot(n == data$n, p == data$p, M >= 2L)
  off <- data$offsets; Tj <- data$T; nb <- max(1L, floor(max_cells / (M * G * Tmax)))
  I <- array(0, c(n, D, D)); IA <- IB <- if (halves) array(0, c(n, D, D)) else NULL
  oddm <- seq(1L, M, 2L); evenm <- seq(2L, M, 2L)
  # common random numbers: the same M designs, taste and side draws and uniforms for every
  # respondent (drawn once, so the result does not depend on batching)
  dr <- .rum_with_seed(seed, list(j = sample.int(data$n, M, replace = TRUE),
    u = stats::rnorm(M), v = stats::rnorm(M), U = matrix(stats::runif(M * Tmax), M, Tmax)))
  for (bi in split(seq_len(n), ceiling(seq_len(n) / nb))) {
    nr <- length(bi); np <- nr * M; ii <- rep(bi, each = M); mm <- rep(seq_len(M), times = nr)
    jm <- dr$j[mm]; um <- dr$u[mm]; vm <- dr$v[mm]; U <- dr$U[mm, , drop = FALSE]
    Sc <- matrix(NA_real_, np, D)
    for (T in sort(unique(Tj[jm]))) {
      r <- which(Tj[jm] == T); MU <- mu[ii[r], , drop = FALSE]
      DL <- lapply(seq_len(T), function(t) data$d[off[jm[r]] + t, , drop = FALSE])
      Y <- matrix(0, length(r), T)
      for (t in seq_len(T)) Y[, t] <- as.numeric(U[r, t] < stats::plogis(th$kappa +
        rowSums(DL[[t]] * (MU + outer(um[r], th$loading))) + s * vm[r]))
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
  n <- data$n; D <- 2L * data$p + 2L; M <- as.integer(if (is.null(M)) 20L * D else M)
  sd_k <- as.integer(seed) + as.integer(k)
  f <- NULL; key <- NULL
  if (!is.null(cache_net) && !is.null(cache_dir)) {
    if (requireNamespace("digest", quietly = TRUE)) {
      key <- digest::digest(list(version = "rum_model_info v2 (common draws)",
        final = fin[c("network", "kappa", "loading", "omega")], mu = unname(mu),
        ids = data$ids, d = data$d, offsets = data$offsets, M = M,
        quad = unclass(quad)[c("u", "v", "weight")], seed = sd_k))
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
# returns respondent row i's D x D information (D = p + q), for i = 1..nrow. Each matrix
# is made positive definite by an eigenvalue floor (eig_floor_rel times its largest
# eigenvalue). Returns Li = I_mumu^{-1} (nrow x p x p), Lt = I_mutheta (nrow x p x q),
# Ic = the average over rows of I_thetatheta - I_thetamu I_mumu^{-1} I_mutheta (the
# efficient information for theta), floored = the number of floored matrices, and
# shared = (nrow == 1): one matrix for every respondent.
.rum_weight_parts <- function(info_of, nrow, p, q, eig_floor_rel) {
  Li <- array(0, c(nrow, p, p)); Lt <- array(0, c(nrow, p, q)); Ic <- matrix(0, q, q); nfl <- 0L
  for (i in seq_len(nrow)) {
    L <- info_of(i); e <- eigen(L, symmetric = TRUE); fl <- eig_floor_rel * max(e$values)
    if (any(e$values < fl)) { nfl <- nfl + 1L; L <- e$vectors %*% (pmax(e$values, fl) * t(e$vectors)) }
    li <- solve(L[1:p, 1:p]); lt <- L[1:p, p + seq_len(q)]; Li[i, , ] <- li; Lt[i, , ] <- lt
    Ic <- Ic + L[p + seq_len(q), p + seq_len(q)] - crossprod(lt, li %*% lt)
  }
  list(Li = Li, Lt = Lt, Ic = Ic / nrow, floored = nfl, shared = nrow == 1L)
}

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
