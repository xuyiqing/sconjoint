# Inference for RUM population targets.
#  - DNN version: cross-fitted one-step (debiased) estimator from fold fits, with correction
#    weights from an estimated information matrix (.rum_onestep, .rum_debias_dnn), and its
#    bounded variant for quantities in [0, 1], a cross-validated targeted update
#    (.rum_tmle_dnn, .rum_bounded_dnn).
#  - Formula version: maximum-likelihood influence function (delta method) from a rum_fit()
#    (.rum_formula_if, .rum_formula_ci).
# Ported from the RUM reproduction scripts (rum_reproduction_2026-09-24/scripts_C/
# rum_debias.R: debias_dnn, tmle_dnn, bounded_dnn, formula_ci; and the Graham-Svolik copy
# debias_dnn_if/formula_ci_if in B9_gs_intervals.R), with the numbers unchanged (parity:
# max abs diff <= 1e-10 on the three applications). The scripts had two copies of the
# one-step: debias_dnn() (a table) and debias_dnn_if() (the same estimator, vectorized for
# the constant basis, also returning per-respondent influence values). They are one routine
# here, .rum_onestep(), which returns the influence values; .rum_debias_dnn() formats its
# table. Targets: rum-inference-targets.R; model-based information: rum-inference-model.R.
# Derivation: the reproduction's report/dnn_debiasing_derivation.md and
# report/derivation_check_2026-09-26.md.
#
# Inputs of the DNN routines. data: rum_data; Z: covariates (rows named by id), the network's
# own inputs; fold: fold of each respondent (data$ids order); fold_final(k): the fitted state
# trained without fold k, a list with network, kappa, loading, omega; targets: a list of
# targets (rum-inference-targets.R).
#
# Correction weights. The information matrix Lambda(z) = E[s s' | z] of the full score
# s = (s_mu, s_theta) is estimated on each fold's fit, then
#   a(z) = L_mumu(z)^{-1} [ (w(z)/Wbar) m_mu(z) - L_mutheta(z) g ],
#   g    = Itilde^{-1} ( E[(w/Wbar) m_theta] - E[ L_thetamu L_mumu^{-1} (w/Wbar) m_mu ] ),
#   Itilde = E[ L_thetatheta - L_thetamu L_mumu^{-1} L_mutheta ]   (averages over the bank).
# Respondent i in fold k gets phi_i = a(z_i)' s_mu,i + g' s_theta,i at fold k's fit.
# info_basis "constant": one matrix, the training folds' average score outer product (a
#   ridge regression of the outer products on b(z) = 1);
# "linear": the regression on b(z) = [1, z], ridge penalty ridge_rel * mean(diag(B'B)) on the
#   slopes; one matrix per respondent;
# "model": each respondent's own model-based information (rum-inference-model.R);
# "conditional": each respondent's own information with designs drawn from the experiment's
#   randomization law and coded with the respondent's covariates (rum-information-conditional.R;
#   options through cond = .rum_cond_options(...)).
# Every matrix is made positive definite by an eigenvalue floor (eig_floor_rel).

.rum_target_weights <- function(targets, n)
  sapply(targets, function(t) if (is.null(t$w)) rep(1, n) else as.numeric(t$w))

# One fold's fitted state: th, mu (n x p, data$ids order), the evaluator's output, and the
# full score S = (s_mu, s_theta) (n x D).
.rum_fold_fit <- function(data, Z, fin, quad_score) {
  th <- list(kappa = fin$kappa, loading = fin$loading, omega = fin$omega)
  mu <- rum_dnn_gaussian_forward(fin$network, Z[data$ids, , drop = FALSE])
  ev <- rum_dnn_gaussian_evaluate(data, mu, th$kappa, th$loading, th$omega, quad_score)
  list(th = th, mu = mu, ev = ev, S = cbind(ev$output_score, ev$common_score))
}

# One fold's information estimate. Returns parts (.rum_weight_parts), diag (a data frame
# row: fold, info_heldout_rel_error = relative Frobenius error of the estimate's held-out
# average against the held-out score outer products, floored, min_eig_Itilde), and meta
# (model basis only: M, seconds, cache file, cache hit).
.rum_fold_information <- function(info_basis, data, Z, ff, fin, k, tr, te, ridge_rel,
                                  eig_floor_rel, model) {
  n <- data$n; p <- data$p; q <- p + 2L; D <- p + q; S <- ff$S
  if (info_basis %in% c("model", "conditional")) {
    MI <- if (info_basis == "model") .rum_model_info_fold(data, ff$mu, ff$th, k, model$M, model$quad, model$seed,
                               model$cache_net, model$cache_dir, fin, model$cache_write,
                               model$cache_require) else .rum_cond_info_fold(data, ff$mu, ff$th, k, model$cond, fin)
    W <- if (info_basis == "conditional" && isTRUE(model$cond$null_zero))
      .rum_cond_weight_parts(MI$I, p, q, eig_floor_rel) else
      .rum_weight_parts(function(i) MI$I[i, , ], n, p, q, eig_floor_rel)
    Ite <- apply(MI$I[te, , , drop = FALSE], c(2, 3), mean)
    Ote <- crossprod(S[te, , drop = FALSE]) / sum(te)
    err <- norm(Ite - Ote, "F") / norm(Ote, "F")
    meta <- data.frame(fold = k, M = MI$M, seconds = MI$seconds, cache_hit = MI$hit,
                       cache_file = if (is.null(MI$cache)) NA_character_ else MI$cache,
                       stringsAsFactors = FALSE)
  } else {
    ut <- which(upper.tri(diag(D), diag = TRUE), arr.ind = TRUE)
    unpack <- function(v) { L <- matrix(0, D, D); L[ut] <- v; L[lower.tri(L)] <- t(L)[lower.tri(L)]; L }
    Bz <- if (info_basis == "constant") matrix(1, n, 1) else cbind(1, Z[data$ids, , drop = FALSE])
    Y <- S[, ut[, 1]] * S[, ut[, 2]]                              # score outer products
    BtB <- crossprod(Bz[tr, , drop = FALSE]) / sum(tr)
    pen <- if (ncol(Bz) > 1) ridge_rel * mean(diag(BtB)) * diag(c(0, rep(1, ncol(Bz) - 1))) else 0
    coef <- solve(BtB + pen, crossprod(Bz[tr, , drop = FALSE], Y[tr, , drop = FALSE]) / sum(tr))
    W <- .rum_weight_parts(function(i) unpack(drop(Bz[i, , drop = FALSE] %*% coef)),
                           if (info_basis == "constant") 1L else n, p, q, eig_floor_rel)
    Lte_pred <- unpack(colMeans(Bz[te, , drop = FALSE] %*% coef))
    Lte_obs <- unpack(colMeans(Y[te, , drop = FALSE]))
    err <- norm(Lte_pred - Lte_obs, "F") / norm(Lte_obs, "F")
    meta <- NULL
  }
  diag <- data.frame(fold = k, info_heldout_rel_error = err, floored = W$floored,
                     min_eig_Itilde = min(eigen(W$Ic, symmetric = TRUE, only.values = TRUE)$values))
  list(parts = W, diag = diag, meta = meta)
}

# The correction weights for one target at one fold's fit: g (length q) and a (one row per
# entry of rows, p columns). v = (w/Wbar) m_mu (n x p); gt_bar = E[(w/Wbar) m_theta].
.rum_correction <- function(parts, v, gt_bar, rows) {
  p <- ncol(v)
  if (parts$shared) {                       # one matrix for everyone: vectorized
    Li <- matrix(parts$Li[1, , ], p, p); Lt <- matrix(parts$Lt[1, , ], p)
    g <- solve(parts$Ic, gt_bar - colMeans(v %*% (t(Li) %*% Lt)))
    a <- (v[rows, , drop = FALSE] - matrix(drop(Lt %*% g), length(rows), p, byrow = TRUE)) %*% t(Li)
  } else {
    g <- solve(parts$Ic, gt_bar - colMeans(.rum_info_CLv(parts$Li, parts$Lt, v)))
    a <- .rum_info_a(parts$Li, parts$Lt, v, g, rows)
  }
  list(a = a, g = g)
}

# Options of the model-based information (see .rum_model_info_fold).
.rum_model_options <- function(model_M, model_quad, model_seed, cache_net, cache_dir,
                               cache_write, cache_require)
  list(M = model_M, quad = model_quad, seed = model_seed, cache_net = cache_net,
       cache_dir = cache_dir, cache_write = cache_write, cache_require = cache_require)

# ---- DNN version: cross-fitted one-step -------------------------------------------------
# The one correction routine. Returns a list (names are target names):
#   est, plug         one-step estimates and cross-fitted plug-ins (weighted means of m_i);
#   IFF, IFP          n x nt per-respondent influence values, fixed reference (phi - mean
#                     phi) and population reference (IFF + (w/Wbar)(m_i - plug));
#                     SE = sd(IF) / sqrt(n) (se_kind "sd");
#   phi, m            n x nt raw corrections and held-out plug-in values m_i;
#   diag              fold diagnostics; fold_loadings (folds x p); model_info (model basis);
#   kind, n_group, info_basis.
.rum_onestep <- function(data, Z, fold, fold_final, targets, quad_score = .rum_score_quadrature(),
                         info_basis = c("constant", "linear", "model", "conditional"), ridge_rel = 1e-2,
                         eig_floor_rel = 1e-6, model_M = NULL, model_quad = .rum_info_quadrature(),
                         model_seed = 20260926L, cache_net = NULL, cache_dir = NULL,
                         cache_write = TRUE, cache_require = FALSE, cond = NULL) {
  info_basis <- match.arg(info_basis)
  ids <- data$ids; n <- data$n; p <- data$p; ks <- sort(unique(fold)); nt <- length(targets)
  model <- .rum_model_options(model_M, model_quad, model_seed, cache_net, cache_dir,
                              cache_write, cache_require)
  model$cond <- cond                     # conditional basis: .rum_cond_options() (rum-information-conditional.R)
  wmat <- .rum_target_weights(targets, n)
  M <- PHI <- matrix(NA_real_, n, nt); diag_fold <- meta <- list()
  loads <- matrix(NA_real_, length(ks), p)
  for (k in ks) {
    fin <- fold_final(k); loads[match(k, ks), ] <- fin$loading
    ff <- .rum_fold_fit(data, Z, fin, quad_score); tr <- fold != k; te <- fold == k
    inf <- .rum_fold_information(info_basis, data, Z, ff, fin, k, tr, te, ridge_rel,
                                 eig_floor_rel, model)
    diag_fold[[length(diag_fold) + 1L]] <- inf$diag
    if (!is.null(inf$meta)) meta[[length(meta) + 1L]] <- inf$meta
    for (t in seq_len(nt)) {
      w <- wmat[, t]; wr <- w / mean(w); gr <- .rum_target_grad(targets[[t]], ff$mu, ff$th)
      cw <- .rum_correction(inf$parts, wr * gr$gm, colMeans(wr * gr$gt), which(te))
      PHI[te, t] <- rowSums(cw$a * ff$ev$output_score[te, , drop = FALSE]) +
        drop(ff$ev$common_score[te, , drop = FALSE] %*% cw$g)
      M[te, t] <- gr$m[te]
    }
  }
  IFF <- IFP <- matrix(NA_real_, n, nt); est <- plug <- numeric(nt)
  for (t in seq_len(nt)) {
    w <- wmat[, t]; wr <- w / mean(w); plug[t] <- sum(w * M[, t]) / sum(w)
    phi <- PHI[, t]; est[t] <- plug[t] + mean(phi)
    IFF[, t] <- phi - mean(phi); IFP[, t] <- IFF[, t] + wr * (M[, t] - plug[t])
  }
  nm <- vapply(targets, `[[`, "", "name")
  names(est) <- names(plug) <- colnames(IFF) <- colnames(IFP) <- colnames(PHI) <- colnames(M) <- nm
  list(est = est, plug = plug, IFF = IFF, IFP = IFP, se_kind = "sd",
       diag = do.call(rbind, diag_fold), fold_loadings = loads,
       kind = stats::setNames(vapply(targets, `[[`, "", "kind"), nm),
       n_group = stats::setNames(colSums(wmat > 0), nm), info_basis = info_basis,
       phi = PHI, m = M, model_info = if (length(meta)) do.call(rbind, meta) else NULL)
}

# The one-step as a table (the columns of the reproduction's debias_dnn()), with attributes
# fold_diagnostics, IFF, IFP, fold_loadings and (model basis) model_info.
.rum_onestep_table <- function(r) {
  n <- nrow(r$IFF)
  out <- do.call(rbind, lapply(seq_along(r$est), function(t) {
    est <- unname(r$est[t]); plug <- unname(r$plug[t])
    sef <- stats::sd(r$IFF[, t]) / sqrt(n); sep <- stats::sd(r$IFP[, t]) / sqrt(n)
    data.frame(target = names(r$est)[t], kind = unname(r$kind[t]),
               n_group = as.integer(r$n_group[t]), info_basis = r$info_basis,
               plug_in = plug, estimate = est,
               se_population = sep, lower_population = est - 1.96 * sep,
               upper_population = est + 1.96 * sep,
               se_fixed = sef, lower_fixed = est - 1.96 * sef, upper_fixed = est + 1.96 * sef,
               correction_in_se = (est - plug) / sep, stringsAsFactors = FALSE)
  }))
  attr(out, "fold_diagnostics") <- r$diag
  attr(out, "IFF") <- r$IFF; attr(out, "IFP") <- r$IFP
  attr(out, "fold_loadings") <- r$fold_loadings
  if (!is.null(r$model_info)) attr(out, "model_info") <- r$model_info
  out
}

# Drop-in for the reproduction's debias_dnn() (same arguments in the same order, plus the
# cache controls; the cache directory has no default here).
.rum_debias_dnn <- function(data, Z, fold, fold_final, targets, quad_score = .rum_score_quadrature(),
                            info_basis = c("constant", "linear", "model", "conditional"), ridge_rel = 1e-2,
                            eig_floor_rel = 1e-6, model_M = NULL,
                            model_quad = .rum_info_quadrature(), model_seed = 20260926L,
                            cache_net = NULL, cache_dir = NULL, cache_write = TRUE,
                            cache_require = FALSE, cond = NULL) {
  info_basis <- match.arg(info_basis)
  .rum_onestep_table(.rum_onestep(data, Z, fold, fold_final, targets, quad_score, info_basis,
    ridge_rel, eig_floor_rel, model_M, model_quad, model_seed, cache_net, cache_dir,
    cache_write, cache_require, cond))
}

# ---- DNN version, bounded targets: cross-validated targeted update (CV-TMLE) ------------
# Same fold fits and the same correction weights (a(z), g) as .rum_onestep(). Instead of
# adding the linear correction mean(phi) to the plug-in, every fold's fitted state moves
# along its correction weights, mu(z) + eps a(z), theta + eps g, with ONE eps shared by all
# folds, chosen so that the held-out likelihood's slope along that path is zero:
# sum_i phi_i(eps) = 0, solved by Fisher scoring, eps <- eps + sum phi / sum phi^2, until
# |sum phi| / sqrt(sum phi^2) < tol. The estimate is the plug-in at the moved states, so a
# share stays in [0, 1]. The SE uses phi at the final eps, plus the composition term for the
# population reference, as the one-step. omega is kept at least 1e-8 along the path.
# Returns the table of the reproduction's tmle_dnn() with attributes IFF, IFP and (for a
# basis other than constant) info_basis.
.rum_tmle_dnn <- function(data, Z, fold, fold_final, targets, quad_score = .rum_score_quadrature(),
                          ridge_rel = 1e-2, eig_floor_rel = 1e-6, max_iter = 20L, tol = 1e-3,
                          info_basis = c("constant", "model", "linear", "conditional"), model_M = NULL,
                          model_quad = .rum_info_quadrature(), model_seed = 20260926L,
                          cache_net = NULL, cache_dir = NULL, cache_write = TRUE,
                          cache_require = FALSE, cond = NULL) {
  info_basis <- match.arg(info_basis)
  ids <- data$ids; n <- data$n; p <- data$p; ks <- sort(unique(fold)); nt <- length(targets)
  model <- .rum_model_options(model_M, model_quad, model_seed, cache_net, cache_dir,
                              cache_write, cache_require)
  model$cond <- cond
  wmat <- .rum_target_weights(targets, n)
  FS <- list()                                       # per fold: data subset, state, parts
  for (k in ks) {
    fin <- fold_final(k); ff <- .rum_fold_fit(data, Z, fin, quad_score)
    tr <- fold != k; te <- fold == k
    inf <- .rum_fold_information(info_basis, data, Z, ff, fin, k, tr, te, ridge_rel,
                                 eig_floor_rel, model)
    FS[[as.character(k)]] <- list(te = which(te), sub = rum_subset(data, ids[te]), mu = ff$mu,
                                  th = ff$th, parts = inf$parts)
  }
  eval_phi <- function(f, a, g, eps) {               # held-out scores along the path
    mu_e <- f$mu[f$te, , drop = FALSE] + eps * a; v <- .rum_th_vec(f$th) + eps * g
    v[p + 2] <- max(v[p + 2], 1e-8)
    ev <- rum_dnn_gaussian_evaluate(f$sub, mu_e, v[1], v[1 + seq_len(p)], v[p + 2], quad_score)
    list(phi = rowSums(a * ev$output_score) + drop(ev$common_score %*% g), mu = mu_e,
         th = .rum_th_list(v, p))
  }
  res <- lapply(seq_len(nt), function(t) {
    w <- wmat[, t]; wr <- w / mean(w); tg <- targets[[t]]
    dirs <- lapply(FS, function(f) {                 # correction weights per fold
      gr <- .rum_target_grad(tg, f$mu, f$th)
      cw <- .rum_correction(f$parts, wr * gr$gm, colMeans(wr * gr$gt), f$te)
      list(a = cw$a, g = cw$g, plug = gr$m[f$te])
    })
    eps <- 0; it <- 0L; conv <- FALSE
    repeat {
      ph <- unlist(lapply(names(FS), function(k) eval_phi(FS[[k]], dirs[[k]]$a, dirs[[k]]$g, eps)$phi))
      sc <- sum(ph) / sqrt(sum(ph^2))
      if (abs(sc) < tol) { conv <- TRUE; break }
      if (it >= max_iter) break
      eps <- eps + sum(ph) / sum(ph^2); it <- it + 1L
    }
    m_star <- numeric(n); phi <- numeric(n); m0 <- numeric(n)
    for (k in names(FS)) {
      f <- FS[[k]]; r <- eval_phi(f, dirs[[k]]$a, dirs[[k]]$g, eps)
      mu_full <- f$mu; mu_full[f$te, ] <- r$mu
      m_star[f$te] <- tg$m(mu_full, r$th)[f$te]; phi[f$te] <- r$phi; m0[f$te] <- dirs[[k]]$plug
    }
    est <- sum(w * m_star) / sum(w); plug <- sum(w * m0) / sum(w)
    iff <- phi - mean(phi); ifp <- iff + wr * (m_star - est)
    sef <- stats::sd(iff) / sqrt(n); sep <- stats::sd(ifp) / sqrt(n)
    list(row = data.frame(target = tg$name, kind = tg$kind, n_group = sum(w > 0), method = "cv-tmle",
               plug_in = plug, estimate = est, eps = eps, iterations = it, converged = conv,
               se_population = sep, lower_population = est - 1.96 * sep,
               upper_population = est + 1.96 * sep,
               se_fixed = sef, lower_fixed = est - 1.96 * sef, upper_fixed = est + 1.96 * sef,
               correction_in_se = (est - plug) / sep, stringsAsFactors = FALSE),
         iff = iff, ifp = ifp)
  })
  out <- do.call(rbind, lapply(res, `[[`, "row"))
  nmt <- vapply(targets, `[[`, "", "name")
  attr(out, "IFF") <- `colnames<-`(sapply(res, `[[`, "iff"), nmt)
  attr(out, "IFP") <- `colnames<-`(sapply(res, `[[`, "ifp"), nmt)
  if (info_basis != "constant") attr(out, "info_basis") <- info_basis
  out
}

# Bounded targets (quantities in [0, 1]): the CV-TMLE estimate with its Wald interval
# truncated to [0, 1]. Returns the one-step table's columns plus the method, so callers can
# swap these rows in; attributes IFF and IFP from the CV-TMLE. Arguments after targets go
# to .rum_tmle_dnn().
.rum_bounded_dnn <- function(data, Z, fold, fold_final, targets, ...) {
  tm <- .rum_tmle_dnn(data, Z, fold, fold_final, targets, ...)
  lg <- function(est, se, sgn) pmin(pmax(est + sgn * 1.96 * se, 0), 1)       # truncated Wald
  out <- data.frame(target = tm$target, kind = tm$kind, n_group = tm$n_group,
                    info_basis = if (is.null(attr(tm, "info_basis"))) "constant" else attr(tm, "info_basis"),
                    plug_in = tm$plug_in, estimate = tm$estimate,
                    se_population = tm$se_population,
                    lower_population = lg(tm$estimate, tm$se_population, -1),
                    upper_population = lg(tm$estimate, tm$se_population, 1),
                    se_fixed = tm$se_fixed, lower_fixed = lg(tm$estimate, tm$se_fixed, -1),
                    upper_fixed = lg(tm$estimate, tm$se_fixed, 1),
                    correction_in_se = tm$correction_in_se,
                    method = "CV-TMLE, Wald interval truncated to [0, 1]",
                    tmle_converged = tm$converged, tmle_iterations = tm$iterations,
                    stringsAsFactors = FALSE)
  rownames(out) <- out$target
  attr(out, "IFF") <- attr(tm, "IFF"); attr(out, "IFP") <- attr(tm, "IFP")
  out
}

# ---- Formula version: maximum-likelihood influence function ------------------------------
# Uses the fit's own per-respondent scores and average information (fit$score and
# fit$information, or fit$hessian_information). Parameter order: kappa, alpha, loading,
# omega. Returns a list like .rum_onestep()'s: est (= plug), IFF = s_i' I^{-1} grad,
# IFP = IFF + (w/Wbar)(m_i - est), se_kind "rms" (SE = sqrt(mean(IF^2)) / sqrt(n)),
# loading, kind, n_group.
.rum_formula_if <- function(fit, targets, info = c("information", "hessian_information")) {
  info <- match.arg(info)
  d <- fit$data; n <- d$n; p <- d$p; r <- d$r
  st <- .rum_state_formula(d, fit$par); th <- list(kappa = st$kappa, loading = st$loading, omega = st$omega)
  basis <- if (is.list(d$basis)) d$basis else rep(list(d$basis), n)
  Iinv <- solve(fit[[info]]); Sc <- fit$score
  stopifnot(ncol(Sc) == 1L + r + p + 1L)
  nt <- length(targets); IFF <- IFP <- matrix(NA_real_, n, nt); est <- numeric(nt)
  for (t in seq_len(nt)) {
    tg <- targets[[t]]; w <- if (is.null(tg$w)) rep(1, n) else as.numeric(tg$w); wr <- w / mean(w)
    gr <- .rum_target_grad(tg, st$mu, th)
    da <- Reduce(`+`, lapply(seq_len(n), function(i) wr[i] * drop(crossprod(basis[[i]], gr$gm[i, ])))) / n
    dt <- colMeans(wr * gr$gt)
    grad <- c(dt[1], da, dt[1 + seq_len(p)], dt[p + 2L])
    iff <- drop(Sc %*% (Iinv %*% grad)); plug <- sum(w * gr$m) / sum(w)
    IFF[, t] <- iff; IFP[, t] <- iff + wr * (gr$m - plug); est[t] <- plug
  }
  nm <- vapply(targets, `[[`, "", "name")
  names(est) <- colnames(IFF) <- colnames(IFP) <- nm
  list(est = est, plug = est, IFF = IFF, IFP = IFP, se_kind = "rms", loading = st$loading,
       kind = stats::setNames(vapply(targets, `[[`, "", "kind"), nm),
       n_group = stats::setNames(vapply(targets, function(t) if (is.null(t$w)) n else sum(t$w > 0), 0), nm))
}

# The formula version as a table (the columns of the reproduction's formula_ci()), with
# attributes IFF and IFP.
.rum_formula_ci <- function(fit, targets, info = c("information", "hessian_information")) {
  info <- match.arg(info)
  r <- .rum_formula_if(fit, targets, info); n <- nrow(r$IFF)
  out <- do.call(rbind, lapply(seq_along(targets), function(t) {
    tg <- targets[[t]]; w <- if (is.null(tg$w)) rep(1, n) else as.numeric(tg$w)
    plug <- unname(r$est[t])
    sef <- sqrt(mean(r$IFF[, t]^2)) / sqrt(n); sep <- sqrt(mean(r$IFP[, t]^2)) / sqrt(n)
    data.frame(target = tg$name, kind = tg$kind, n_group = sum(w > 0), estimate = plug,
               se_population = sep, lower_population = plug - 1.96 * sep,
               upper_population = plug + 1.96 * sep,
               se_fixed = sef, lower_fixed = plug - 1.96 * sef, upper_fixed = plug + 1.96 * sef,
               stringsAsFactors = FALSE)
  }))
  attr(out, "IFF") <- r$IFF; attr(out, "IFP") <- r$IFP
  out
}
