# Identification check for the one-step correction (Avi's memo of 2026-10-08, item 2;
# paperps1011.tex Appendix D, "Scores and Target Information").
#
# The correction for a target solves, for each respondent z,
#   I_mumu(z) a(z) + I_mutheta(z) b = h_mu(z)                       (riesz_mean)
# If I_mumu(z) has a null direction n (n' I_mumu(z) n = 0), then n' s_mu = 0 almost surely,
# so n' I_mutheta(z) = 0 too, and the equation has a solution only if n' h_mu(z) = 0. A
# respondent whose design never varies a feature (Graham-Svolik pure independents: the
# co-partisan column is always 0) has such a direction. The engine's eigenvalue floor
# (.rum_weight_parts) inverts the singular matrix anyway; the floored direction gets a huge
# weight times a score that is exactly 0, so the correction for the part of the target that
# loads on that direction comes out 0 and the plug-in's bias there is never corrected, with
# no message. This file detects that case before any correction is computed.
#
# Null directions of I_mumu(z) come from either source:
#   .rum_nullspace_design(): the span of the coded design rows the respondent can face
#     under the experiment's law (B1's generators, rum-design.R). I_mumu(z) is a positively
#     weighted sum of d d' over those rows, so its null space is the orthogonal complement
#     of their span; exact zeros, no simulation noise in the zero pattern.
#   .rum_nullspace_info(): eigen-decomposition of respondent information matrices (the model
#     or conditional information), with a relative tolerance.
# Tolerance: a direction is null when its eigenvalue is at most tol_rel times the largest
# eigenvalue of that respondent's I_mumu (default 1e-8; the engine's floor is 1e-6 of the
# largest eigenvalue of the FULL matrix, so every direction flagged here is also floored
# there). A target is "not identified for respondent i" when i is in the target's population
# (w_i > 0) and the norm of the target derivative's projection on i's null space exceeds
# tol_h times the norm of the derivative (default 1e-6).
#
# .rum_identification_check() returns a table (one row per target: population size,
# respondents flagged, largest relative component, the features spanning the null space) and,
# for flagged targets, a suggested restricted population (w with the flagged respondents set
# to 0). on_fail = "stop" (default) stops with a message naming each flagged target and its
# respondent count; "warn" and "none" return the check. .rum_onestep_checked() runs the check
# on every fold's fit, then either stops, or (on_unidentified = "restrict") restricts each
# flagged target's population and calls .rum_onestep() unchanged; identified targets get
# exactly the numbers .rum_onestep() gives.

# Orthonormal null directions of one symmetric PSD matrix (p x k; k = 0 gives p x 0).
.rum_null_directions <- function(A, tol_rel = 1e-8) {
  A <- (A + t(A)) / 2
  e <- eigen(A, symmetric = TRUE)
  mx <- max(e$values, 0)
  if (mx <= 0) return(diag(nrow(A)))
  e$vectors[, e$values <= tol_rel * mx, drop = FALSE]
}

# Null spaces from the design law. spec: a rum_design_spec or its name; resp: the
# respondents (GS: a data frame with pid7, or a pid7 / pid_lean vector; others NULL with n).
# Returns list(N = list of p x k matrices, one per design key, group = per-respondent index
# into N, keys, features, source, dims = null dimension per key, gram_min_rel = smallest
# relative eigenvalue above the tolerance per key).
.rum_nullspace_design <- function(spec, resp = NULL, n = NULL, M = 500L, seed = 20261012L,
                                  tol_rel = 1e-8) {
  spec <- .rum_spec_arg(spec)
  crn <- .rum_draw_designs_crn(spec, resp, M, seed = seed, n = n)
  p <- spec$p
  N <- lapply(crn$designs, function(d) {
    X <- matrix(d, dim(d)[1L] * dim(d)[2L], p)
    .rum_null_directions(crossprod(X), tol_rel)
  })
  names(N) <- crn$keys
  list(N = N, group = crn$key, keys = crn$keys, features = spec$features,
       source = sprintf("design support (%s, %d sequences, seed %d)", spec$study, M, seed),
       dims = vapply(N, ncol, 0L), tol_rel = tol_rel)
}

# Null spaces from respondent information matrices: I is an n x D x D array (or a function
# i -> D x D matrix, with n given); the mu block is the first p rows and columns.
# Respondents with the same null space share a group.
.rum_nullspace_info <- function(I, p, n = NULL, tol_rel = 1e-8, features = NULL) {
  get <- if (is.function(I)) I else function(i) I[i, , ]
  if (is.null(n)) n <- dim(I)[1L]
  Ns <- lapply(seq_len(n), function(i) .rum_null_directions(get(i)[1:p, 1:p, drop = FALSE], tol_rel))
  # group identical null spaces (projection matrices equal to 1e-8)
  key <- vapply(Ns, function(N) if (!ncol(N)) "0" else
    paste(round(tcrossprod(N), 8), collapse = ","), "")
  u <- unique(key)
  list(N = Ns[match(u, key)], group = match(key, u), keys = u, features = features,
       source = "information matrices", dims = vapply(Ns[match(u, key)], ncol, 0L),
       tol_rel = tol_rel)
}

# The check. targets: engine targets; mu (n x p, data$ids order) and th: the fitted state at
# which target derivatives are evaluated; null: from .rum_nullspace_design/_info. info
# (optional): the information the correction would use, as an n x D x D array, one D x D
# matrix (pooled), or a function i -> D x D with info_n rows; with it the check also tests
# confounding with the common parameters (.rum_common_check; columns max_rel_common and
# common_confounded; a confounded target fails like an unidentified one).
.rum_identification_check <- function(targets, mu, th, null, tol_h = 1e-6,
                                      on_fail = c("stop", "warn", "none"), info = NULL,
                                      info_n = NULL, tol_rel_common = 1e-8) {
  on_fail <- match.arg(on_fail)
  n <- nrow(mu); p <- ncol(mu)
  if (length(null$group) != n) stop("null$group must have one entry per respondent")
  fn <- if (!is.null(null$features)) null$features else colnames(mu)
  span_lab <- vapply(null$N, function(N) {
    if (!ncol(N)) return("")
    load <- rowSums(N^2); ix <- which(load > 1e-6)
    paste(if (is.null(fn)) ix else fn[ix], collapse = " + ")
  }, "")
  wmat <- .rum_target_weights(targets, n)
  flag <- matrix(FALSE, n, length(targets))
  rows <- vector("list", length(targets))
  has_null <- vapply(null$N, ncol, 0L) > 0
  for (t in seq_along(targets)) {
    tg <- targets[[t]]; w <- wmat[, t]
    gm <- .rum_target_grad(tg, mu, th)$gm
    rel <- numeric(n)
    for (g in which(has_null)) {
      ii <- which(null$group == g & w > 0); if (!length(ii)) next
      G <- gm[ii, , drop = FALSE]
      comp <- sqrt(rowSums((G %*% null$N[[g]])^2)); nrm <- sqrt(rowSums(G^2))
      rel[ii] <- ifelse(nrm > 0, comp / nrm, 0)
    }
    flag[, t] <- w > 0 & rel > tol_h
    gi <- unique(null$group[flag[, t]])
    rows[[t]] <- data.frame(target = tg$name, kind = tg$kind, n_population = sum(w > 0),
                            n_not_identified = sum(flag[, t]),
                            max_rel_component = if (any(w > 0)) max(rel[w > 0]) else 0,
                            null_span = paste(unique(span_lab[gi]), collapse = "; "),
                            stringsAsFactors = FALSE)
  }
  tab <- do.call(rbind, rows)
  cc <- NULL
  if (!is.null(info)) {
    q <- p + 2L
    io <- if (is.function(info)) info else if (is.matrix(info)) function(i) info else function(i) info[i, , ]
    nr <- if (is.function(info)) info_n else if (is.matrix(info)) 1L else dim(info)[1L]
    grads <- lapply(targets, function(tg) .rum_target_grad(tg, mu, th))
    cc <- .rum_common_check(io, nr, p, q, wmat, grads, tol_rel_common, tol_h,
                            c("kappa", paste0("loading", seq_len(p)), "omega"))
    tab$max_rel_common <- cc$rel_common; tab$common_confounded <- cc$confounded
  }
  nm <- vapply(targets, `[[`, "", "name"); colnames(flag) <- nm
  suggest <- lapply(which(colSums(flag) > 0), function(t) wmat[, t] * !flag[, t])
  names(suggest) <- nm[colSums(flag) > 0]
  res <- list(table = tab, flagged = flag, suggest_w = suggest,
              floor_hits = sum(has_null[null$group]),
              null_dims = stats::setNames(vapply(null$N, ncol, 0L), null$keys),
              null_span = stats::setNames(span_lab, null$keys), source = null$source,
              tol = c(tol_rel = null$tol_rel, tol_h = tol_h), common = cc)
  if (!is.null(cc) && any(cc$confounded) && on_fail != "none") {
    msg <- sprintf(paste0("the correction equation has no solution for %d target(s): the means ",
      "and the common parameters are confounded along a direction the target derivative loads ",
      "on (null direction of the common-parameter Schur complement). Not identified: %s"),
      sum(cc$confounded), paste(tab$target[cc$confounded], collapse = ", "))
    if (on_fail == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  bad <- tab[tab$n_not_identified > 0, , drop = FALSE]
  if (nrow(bad) && on_fail != "none") {
    msg <- sprintf(paste0("the correction equation has no solution for %d target(s): the target ",
      "derivative loads on a direction in which these respondents' choices carry no information ",
      "(%s). Not identified for these respondents:\n%s\nRestrict each target's population ",
      "(the check's suggest_w, or on_unidentified = \"restrict\"), or state an assumption that ",
      "links these respondents to the others."), nrow(bad), null$source,
      paste(sprintf("  %s: %d of %d respondents (null direction: %s)", bad$target,
                    bad$n_not_identified, bad$n_population, bad$null_span), collapse = "\n"))
    if (on_fail == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  res
}

# ---- confounding with the common parameters (Codex review of eae4719, finding 2) ----------
# The check above looks only at null directions of I_mumu(z). The full system can also fail
# through the common parameters theta = (kappa, loading, omega). Example: a feature with
# d = 1 in every task. Then s_mu = s_kappa, I_mumu > 0 (the check above passes), but raising
# every mean by c and lowering kappa by c leaves the likelihood unchanged, so a mean target
# is not identified; the engine's eigenvalue floor then manufactures an inverse.
# With C(z) = I_mumu(z)^+ I_mutheta(z) (pseudo-inverse: mean null directions are handled
# above), the common-parameter Schur complement is J = E[I_thth - I_thmu C] (Appendix D's J,
# eq. riesz_solution) and the b equation is J b = r, r = E[h_theta] - E[C(Z)' h_mu(Z)]. Every
# g with g' J g = 0 is, together with f(z) = -C(z) g, a perturbation (f, g) with zero score
# (each z's Schur complement is PSD), so the target derivative along it, -(g' r), must be 0:
# the system is solvable only if r is orthogonal to the null space of J. A target is
# "confounded with the common parameters" when the norm of r's projection on null(J) exceeds
# tol_h times the size of its derivative (max of |r|, |E[h_mu]|, |E[h_theta]|). Null
# directions of J: eigenvalues at most tol_rel times the largest eigenvalue of the average
# full information. All of it before any flooring.
# info_of(i): respondent row i's D x D information (D = p + q) for i = 1..nrow (nrow = 1: one
# pooled matrix, as the constant basis). wmat: n x targets weights; grads: per target
# list(gm = n x p, gt = n x q) from .rum_target_grad. Returns per target: rel_common and
# confounded; and null_J (q x k), the common null directions, labeled by parameter.
.rum_common_check <- function(info_of, nrow, p, q, wmat, grads, tol_rel = 1e-8, tol_h = 1e-6,
                              theta_names = NULL) {
  n <- nrow(wmat); J <- matrix(0, q, q); Lbar <- matrix(0, p + q, p + q)
  C <- array(0, c(nrow, p, q)); ip <- seq_len(p); iq <- p + seq_len(q)
  for (i in seq_len(nrow)) {
    L <- info_of(i); L <- (L + t(L)) / 2; Lbar <- Lbar + L / nrow
    e <- eigen(L[ip, ip, drop = FALSE], symmetric = TRUE); mx <- max(e$values, 0)
    k <- e$values > tol_rel * mx
    Mp <- if (any(k)) e$vectors[, k, drop = FALSE] %*% (t(e$vectors[, k, drop = FALSE]) / e$values[k]) else matrix(0, p, p)
    Ci <- Mp %*% L[ip, iq, drop = FALSE]; C[i, , ] <- Ci
    J <- J + (L[iq, iq, drop = FALSE] - crossprod(L[ip, iq, drop = FALSE], Ci)) / nrow
  }
  ej <- eigen((J + t(J)) / 2, symmetric = TRUE)
  scale <- max(eigen(Lbar, symmetric = TRUE, only.values = TRUE)$values, 0)
  NJ <- ej$vectors[, ej$values <= tol_rel * scale, drop = FALSE]
  if (!is.null(theta_names)) rownames(NJ) <- theta_names
  rel <- vapply(seq_along(grads), function(t) {
    if (!ncol(NJ)) return(0)
    w <- wmat[, t]; wr <- w / mean(w); v <- wr * grads[[t]]$gm; gt <- colMeans(wr * grads[[t]]$gt)
    Cv <- if (nrow == 1L) drop(crossprod(matrix(C[1, , ], p, q), colMeans(v)))
          else colMeans(t(vapply(seq_len(n), function(i) drop(crossprod(matrix(C[i, , ], p, q), v[i, ])), numeric(q))))
    r <- gt - Cv
    den <- max(sqrt(sum(r^2)), sqrt(sum(colMeans(v)^2)), sqrt(sum(gt^2)))
    if (den == 0) 0 else sqrt(sum(crossprod(NJ, r)^2)) / den
  }, 0)
  list(rel_common = rel, confounded = rel > tol_h, null_J = NJ, J = J,
       min_eig_J_rel = if (scale > 0) min(ej$values) / scale else NA_real_)
}

# A short printed summary of a check.
.rum_print_identification <- function(x) {
  cat(sprintf("identification check (%s): %d respondent(s) with a null direction; %d of %d targets not identified for some respondents\n",
              x$source, x$floor_hits, sum(x$table$n_not_identified > 0), nrow(x$table)))
  print(x$table[x$table$n_not_identified > 0, , drop = FALSE], row.names = FALSE)
  invisible(x)
}

# The check at every fold's fit, on the fold's held-out respondents (whose corrections that
# fit supplies). Returns list(flagged = n x targets, union over folds; table = the per-target
# counts; by_fold = the per-fold checks).
.rum_identification_folds <- function(data, Z, fold, fold_final, targets, null, tol_h = 1e-6,
                                       common = TRUE, quad_score = .rum_score_quadrature(),
                                       tol_rel_common = 1e-8) {
  n <- data$n; p <- data$p; q <- p + 2L
  flag <- matrix(FALSE, n, length(targets)); by <- list()
  wmat <- .rum_target_weights(targets, n); rel_common <- matrix(0, 0, length(targets))
  null_J <- list()
  for (k in sort(unique(fold))) {
    fin <- fold_final(k)
    mu <- rum_dnn_gaussian_forward(fin$network, Z[data$ids, , drop = FALSE])
    th <- list(kappa = fin$kappa, loading = fin$loading, omega = fin$omega)
    te <- fold == k
    tk <- lapply(targets, function(tg) { w <- if (is.null(tg$w)) rep(1, n) else as.numeric(tg$w)
      tg$w <- w * te; tg })
    ck <- .rum_identification_check(tk, mu, th, null, tol_h = tol_h, on_fail = "none")
    flag <- flag | ck$flagged; by[[as.character(k)]] <- ck
    if (common) {         # the fold's pooled training information (the constant basis), unfloored
      ff <- .rum_fold_fit(data, Z, fin, quad_score); tr <- fold != k
      Ib <- crossprod(ff$S[tr, , drop = FALSE]) / sum(tr)
      grads <- lapply(targets, function(tg) .rum_target_grad(tg, ff$mu, ff$th))
      cc <- .rum_common_check(function(i) Ib, 1L, p, q, wmat, grads, tol_rel_common, tol_h,
                              c("kappa", paste0("loading:", data$feature_names), "omega"))
      rel_common <- rbind(rel_common, cc$rel_common); null_J[[as.character(k)]] <- cc$null_J
    }
  }
  colnames(flag) <- vapply(targets, `[[`, "", "name")
  span <- vapply(seq_along(targets), function(t) {
    x <- unique(unlist(lapply(by, function(ck) ck$table$null_span[t]))); paste(x[nzchar(x)], collapse = "; ") }, "")
  tab <- data.frame(target = colnames(flag), kind = vapply(targets, `[[`, "", "kind"),
                    n_population = as.integer(colSums(wmat > 0)),
                    n_not_identified = as.integer(colSums(flag)),
                    max_rel_component = vapply(seq_along(targets), function(t)
                      max(vapply(by, function(ck) ck$table$max_rel_component[t], 0)), 0),
                    null_span = span, stringsAsFactors = FALSE)
  if (common) {
    tab$max_rel_common <- apply(rel_common, 2, max)
    tab$common_confounded <- tab$max_rel_common > tol_h
  }
  list(flagged = flag, table = tab, by_fold = by,
       floor_hits = sum(vapply(null$N, ncol, 0L)[null$group] > 0), null_J = null_J)
}

# The one-step with the check in front. The check runs at every fold's fit on the fold's
# held-out respondents (whose corrections that fit supplies). on_unidentified = "stop" stops
# (the message names the targets and respondent counts); "restrict" multiplies each flagged
# target's weights by the indicator of identified respondents (union over folds) and renames
# nothing. Then .rum_onestep() runs unchanged; the result carries $identification (the
# per-target table, with n_dropped) and, for restricted targets, $restricted_w.
.rum_onestep_checked <- function(data, Z, fold, fold_final, targets, null,
                                 on_unidentified = c("stop", "restrict"), tol_h = 1e-6,
                                 common = TRUE, ...) {
  on_unidentified <- match.arg(on_unidentified)
  n <- data$n
  idf <- .rum_identification_folds(data, Z, fold, fold_final, targets, null, tol_h,
                                   common = common)
  flag <- idf$flagged
  # confounding with the common parameters is a property of the whole population: no
  # restriction of respondents removes it, so it always stops
  if (common && any(idf$table$common_confounded)) {
    bad <- idf$table[idf$table$common_confounded, , drop = FALSE]
    nj <- unique(unlist(lapply(idf$null_J, function(N) if (ncol(N)) apply(N, 2, function(x)
      paste(sprintf("%+.2f %s", x[abs(x) > 1e-3], rownames(N)[abs(x) > 1e-3]), collapse = " ")) else character(0))))
    stop(sprintf(paste0("the correction equation has no solution for %d target(s): the target ",
      "derivative loads on a direction in which the means and the common parameters are ",
      "confounded (no change in the likelihood; null direction of the common-parameter Schur ",
      "complement J: %s). Not identified:\n%s\nRestricting respondents does not help; ",
      "re-specify the model (e.g. a feature that is constant in every task is aliased with kappa)."),
      nrow(bad), paste(nj, collapse = "; "),
      paste(sprintf("  %s (relative component %.2g)", bad$target, bad$max_rel_common), collapse = "\n")),
      call. = FALSE)
  }
  wmat <- .rum_target_weights(targets, n)
  tab <- data.frame(target = vapply(targets, `[[`, "", "name"),
                    n_population = as.integer(colSums(wmat > 0)),
                    n_not_identified = as.integer(colSums(flag)),
                    stringsAsFactors = FALSE)
  if (any(tab$n_not_identified > 0) && on_unidentified == "stop") {
    bad <- tab[tab$n_not_identified > 0, ]
    stop(sprintf(paste0("the correction equation has no solution for %d target(s) (%s). ",
      "Not identified for these respondents:\n%s\nUse on_unidentified = \"restrict\" to drop ",
      "them from each target's population, or state an assumption that links them to the others."),
      nrow(bad), null$source, paste(sprintf("  %s: %d of %d respondents", bad$target,
      bad$n_not_identified, bad$n_population), collapse = "\n")), call. = FALSE)
  }
  rw <- list()
  for (t in which(tab$n_not_identified > 0)) {
    w <- wmat[, t] * !flag[, t]; targets[[t]]$w <- w; rw[[targets[[t]]$name]] <- w
  }
  r <- .rum_onestep(data, Z, fold, fold_final, targets, ...)
  tab$n_dropped <- tab$n_not_identified
  r$identification <- tab; r$restricted_w <- rw
  r
}
