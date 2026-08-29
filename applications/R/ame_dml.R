## One-step (DML) inference for the FIXED-DRAW structural AME, via
## scmix_dml()'s documented plugin-callback contract.
##
## TWO TARGETS, NEVER INTERCHANGEABLE (audit work package A1).
##
##   Psi_M = (1/M) sum_m [ V_0(d(a, W_m)) - V_0(d(a', W_m)) ]      "ame_fixed_draw"
##   Psi   = E_W [ V_0(d(a, W)) - V_0(d(a', W)) ]                  "ame_design_integrated"
##
## Everything in this file targets Psi_M, CONDITIONAL ON A PRESPECIFIED
## FROZEN DRAW SET W_1..W_M from the known fielded design law D. That is a
## fixed finite linear combination of the manuscript's choice-probability
## functionals (eq:choice_estimand / eq:neutral_choice_estimand), so
## Proposition prop:regular_inference covers it by stacking the 2M choice
## targets (a fixed prespecified finite collection, condition (I1)); the
## influence function and the Riesz representer are the same linear
## combination of the choice targets'. No new domain gates arise: choice
## probabilities are smooth means. Because D is known by design, no
## estimated-design-law term enters.
##
## The intervals this file produces cover Psi_M. They do NOT cover Psi
## without an additional numerical-integration argument. Fixing M does not
## supply one: ordinary Monte Carlo error is O_p(M^{-1/2}), and the paper's
## condition needs sqrt(N)|Psi_M - Psi| = o_p(1), for which a sufficient
## asymptotic regime is M/N -> infinity or a deterministic certified
## integration rule. `ame_design_mc_se()` below is a DIAGNOSTIC for the gap,
## and its `mc_se < 0.25 * sampling_se` companion rule is a finite-sample
## heuristic, not the paper's asymptotic condition.
##
## Target labels are prefixed `ame_fixed_draw` so no downstream table can
## print a fixed-draw interval under an exact-AME heading. `ame_draw_spec()`
## persists the draw set, seed, design law, hash, M, and target label.
##
## The derivative kernel copies the package's typed "choice" target
## conventions exactly (R/paperps-inference.R):
##   - normalized Gauss--Hermite nodes with weights summing to one;
##   - positive-variance branch  dv = E[G'(v) z / (2 s)];
##   - zero-variance branch      dv = 0.5 G'(v)(1 - 2G(v))  (Stein limit);
##   - position_neutral averages the two display orientations, including
##     the sign flip on d_kappa.
## Contract: value N-vector; d_mu N x p; d_kappa N-vector; d_Sigma
## N x p x p (full symmetric matrix; the machinery forms (Gs + Gs')A).
## Everything depends on the loading only through d' Sigma d, so the
## rotation-invariance requirement holds by construction.

## Near-zero directional variances switch to the Stein branch to avoid
## catastrophic cancellation in z/(2s); the switch error is O(s).
.ame_v_eps <- 1e-10

## Normalized probabilists' Gauss--Hermite rule (Golub--Welsch), identical
## to the package's .scmix_dml_gh1 but local so this file stands alone.
ame_gh_nodes <- function(n_nodes = 31L) {
  stopifnot(is.numeric(n_nodes), length(n_nodes) == 1L, n_nodes >= 3L,
            n_nodes == as.integer(n_nodes))
  n <- as.integer(n_nodes)
  J <- matrix(0, n, n)
  off <- sqrt(seq_len(n - 1L))
  J[cbind(seq_len(n - 1L), 2L:n)] <- off
  J[cbind(2L:n, seq_len(n - 1L))] <- off
  ee <- eigen(J, symmetric = TRUE)
  ord <- order(ee$values)
  list(x = ee$values[ord], w = ee$vectors[1L, ord]^2)
}

## Design draws for one application, replicating est_AME's RNG sequence
## exactly (same seed => same X1, X2). Independent uniform assignment per
## attribute over its levels including the reference --- valid for
## Saha--Weeks per its protocol record; other designs need their own
## constructors, as in est_AME.
##   coord: character vector of coordinate names (fit$coord order).
##   attrs: named list, attribute -> its non-reference coordinate names.
## Returns X1, X2 (M x p profile matrices) and coord_index.
ame_design_draws <- function(coord, attrs, M_D = 20000L, seed = 1L,
                             design_law = paste0(
                               "attributes independent; levels uniform over ",
                               "each attribute's levels including the ",
                               "reference; both profiles drawn")) {
  p <- length(coord)
  coord_index <- lapply(attrs, function(cols) match(cols, coord))
  stopifnot(!anyNA(unlist(coord_index)))
  set.seed(seed)
  draw_profile <- function(M) {
    X <- matrix(0, M, p)
    for (a in names(attrs)) {
      k <- length(attrs[[a]]) + 1L
      lev <- sample.int(k, M, replace = TRUE)
      for (l in seq_len(k - 1L))
        X[lev == l, coord_index[[a]][l]] <- 1
    }
    X
  }
  X1 <- draw_profile(M_D)
  X2 <- draw_profile(M_D)
  out <- list(X1 = X1, X2 = X2, coord_index = coord_index, coord = coord,
              attrs = attrs, M_D = M_D, seed = seed,
              design_law = design_law,
              target = "ame_fixed_draw",
              target_definition = paste0(
                "Psi_M = M^{-1} sum_m [V_0{d(a,W_m)} - V_0{d(a',W_m)}], ",
                "conditional on this frozen draw set"))
  ## Draw-set hash: two runs that agree here return bitwise-identical
  ## Psi_M, because the callbacks are deterministic in (mu, kappa, Sigma).
  out$draw_hash <- tryCatch(
    digest::digest(list(X1, X2, coord, attrs), algo = "sha256"),
    error = function(e) NA_character_)
  out
}

## The provenance record that must travel with every AME artifact.
ame_draw_spec <- function(draws, mc_se = NULL) {
  list(target = draws$target,
       target_definition = draws$target_definition,
       design_law = draws$design_law,
       M = draws$M_D,
       seed = draws$seed,
       draw_hash = draws$draw_hash,
       coordinates = paste(draws$coord, collapse = ","),
       mc_se_max = if (is.null(mc_se)) NA_real_ else max(mc_se),
       integration_contract = paste0(
         "intervals cover the FIXED-DRAW target Psi_M conditional on this ",
         "draw set; they do not cover the exact design integral Psi without ",
         "a certified integration rate (M/N -> infinity or a deterministic ",
         "rule). The mc_se < 0.25 * sampling_se check is a finite-sample ",
         "heuristic, not the paper's asymptotic condition."))
}

## Focal/reference contrast pair for one attribute level:
## d(a, W_m) = X1 with the focal attribute's columns zeroed (level column
## set for the focal level, left zero for the reference) minus X2.
ame_contrast_pair <- function(draws, attr, level_col) {
  ci <- draws$coord_index[[attr]]
  stopifnot(!is.null(ci))
  X1f <- draws$X1
  X1f[, ci] <- 0
  d_ref <- X1f - draws$X2
  if (!is.na(level_col)) {
    stopifnot(level_col %in% ci)
    X1f[, level_col] <- 1
  }
  d_focal <- X1f - draws$X2
  list(d_focal = d_focal, d_ref = d_ref)
}

## Kernel: accumulated value / d_mu / d_kappa / d_Sigma for ONE side
## (a set of draw contrasts D, M x p), one orientation sign, all
## respondents. Returns sums over the chunk's draws (caller divides by M).
##   val: N; dk: N; dmu: N x p; dvP: N x npair (lower-triangle pairs,
##   for the d_Sigma assembly).
.ame_side_chunk <- function(mu, kappa, Sigma, Dc, gh, sign_kappa, pair_ab) {
  N <- nrow(mu)
  Mc <- nrow(Dc)
  m <- mu %*% t(Dc)                              # N x Mc respondent means
  v <- rowSums((Dc %*% Sigma) * Dc)              # Mc directional variances
  v <- pmax(v, 0)
  s <- sqrt(v)
  pos <- v > .ame_v_eps
  val <- matrix(0, N, Mc)
  slp <- matrix(0, N, Mc)                        # E[G'] weights (dm and dk)
  dv <- matrix(0, N, Mc)                         # d/d(sigma^2) weights
  if (any(pos)) {
    mp <- m[, pos, drop = FALSE]
    sp <- s[pos]
    for (j in seq_along(gh$x)) {
      idx <- sweep(mp, 2L, sp * gh$x[j], `+`) + sign_kappa * kappa
      P <- stats::plogis(idx)
      S <- P * (1 - P)
      val[, pos] <- val[, pos] + gh$w[j] * P
      slp[, pos] <- slp[, pos] + gh$w[j] * S
      dv[, pos] <- dv[, pos] +
        S * matrix(gh$w[j] * gh$x[j] / (2 * sp), N, sum(pos), byrow = TRUE)
    }
  }
  if (any(!pos)) {
    m0 <- m[, !pos, drop = FALSE] + sign_kappa * kappa
    P <- stats::plogis(m0)
    S <- P * (1 - P)
    val[, !pos] <- P
    slp[, !pos] <- S
    dv[, !pos] <- 0.5 * S * (1 - 2 * P)
  }
  ## Reductions over the chunk's draws (sums; caller divides by M).
  Dprod <- Dc[, pair_ab[, 1L], drop = FALSE] *
    Dc[, pair_ab[, 2L], drop = FALSE]            # Mc x npair
  list(val = rowSums(val),
       dk = sign_kappa * rowSums(slp),
       dmu = slp %*% Dc,                          # N x p
       dvP = dv %*% Dprod)                        # N x npair
}

## Build one contract-conforming scmix_dml() plugin callback for a single
## AME level (focal vs reference), given the fixed draw contrast pair.
##   d_focal, d_ref: M x p matrices of design contrasts (fixed; closure).
## The callback is deterministic in (mu, kappa, Sigma); no RNG inside.
ame_dml_target <- function(d_focal, d_ref, n_nodes = 31L,
                           position_neutral = FALSE, chunk = 1024L,
                           label = "ame") {
  stopifnot(is.matrix(d_focal), is.matrix(d_ref),
            all(dim(d_focal) == dim(d_ref)), nrow(d_focal) >= 1L)
  M <- nrow(d_focal)
  p_draw <- ncol(d_focal)
  gh <- ame_gh_nodes(n_nodes)
  stopifnot(abs(sum(gh$w) - 1) < 1e-12,
            abs(sum(gh$w * gh$x)) < 1e-10,
            abs(sum(gh$w * gh$x^2) - 1) < 1e-10)
  pair_ab <- which(lower.tri(matrix(0, p_draw, p_draw), diag = TRUE),
                   arr.ind = TRUE)
  force(chunk); force(position_neutral); force(label)
  callback <- function(mu, kappa, Sigma, Z, respondent_id, fold,
                       attr_names) {
    N <- nrow(mu)
    p <- ncol(mu)
    stopifnot(p == p_draw)
    signs <- if (isTRUE(position_neutral)) c(1, -1) else 1
    val <- numeric(N)
    dk <- numeric(N)
    dmu <- matrix(0, N, p)
    dvP <- matrix(0, N, nrow(pair_ab))
    starts <- seq.int(1L, M, by = as.integer(chunk))
    for (st in starts) {
      en <- min(st + chunk - 1L, M)
      Df <- d_focal[st:en, , drop = FALSE]
      Dr <- d_ref[st:en, , drop = FALSE]
      for (sg in signs) {
        f <- .ame_side_chunk(mu, kappa, Sigma, Df, gh, sg, pair_ab)
        r <- .ame_side_chunk(mu, kappa, Sigma, Dr, gh, sg, pair_ab)
        w <- 1 / length(signs)
        val <- val + w * (f$val - r$val)
        dk <- dk + w * (f$dk - r$dk)
        dmu <- dmu + w * (f$dmu - r$dmu)
        dvP <- dvP + w * (f$dvP - r$dvP)
      }
    }
    val <- val / M
    dk <- dk / M
    dmu <- dmu / M
    dvP <- dvP / M
    ds <- array(0, c(N, 1L, p, p))
    for (h in seq_len(nrow(pair_ab))) {
      a <- pair_ab[h, 1L]
      b <- pair_ab[h, 2L]
      ds[, 1L, a, b] <- dvP[, h]
      if (a != b) ds[, 1L, b, a] <- dvP[, h]
    }
    list(target_type = "rowwise_expectation",
         value = matrix(val, ncol = 1L),
         d_mu = array(dmu, c(N, 1L, p)),
         d_kappa = matrix(dk, ncol = 1L),
         d_Sigma = ds,
         labels = label)
  }
  attr(callback, "ame_spec") <- list(M = M, n_nodes = n_nodes,
                                     position_neutral = position_neutral,
                                     label = label)
  class(callback) <- c("scmix_rowwise_target", "function")
  callback
}

## Convenience: one callback per non-reference level of every attribute,
## sharing one fixed draw set. Returns a named list for plugin_targets,
## plus the draw object (for provenance) and the per-level mc_se of the
## design average computed at the supplied fit parameters.
ame_dml_targets <- function(coord, attrs, M_D = 20000L, seed = 1L,
                            n_nodes = 31L, position_neutral = FALSE,
                            chunk = 1024L, prefix = if (position_neutral)
                              "ame_fixed_draw_neutral" else "ame_fixed_draw") {
  draws <- ame_design_draws(coord, attrs, M_D = M_D, seed = seed)
  targets <- list()
  for (a in names(attrs)) {
    for (l in seq_along(attrs[[a]])) {
      cn <- attrs[[a]][l]
      pair <- ame_contrast_pair(draws, a, draws$coord_index[[a]][l])
      lab <- paste0(prefix, ":", cn)
      targets[[lab]] <- ame_dml_target(pair$d_focal, pair$d_ref,
                                       n_nodes = n_nodes,
                                       position_neutral = position_neutral,
                                       chunk = chunk, label = lab)
    }
  }
  list(targets = targets, draws = draws, spec = ame_draw_spec(draws))
}

## DIAGNOSTIC. Design-Monte-Carlo error of the M-draw average at given fit
## parameters. Returns both the value (the respondent-by-draw product-form
## plug-in at these parameters) and its design mc_se: sd over draws of the
## respondent-averaged per-draw difference, divided by sqrt(M). The mc_se
## quantifies the ORDINARY Monte Carlo size of Psi_M - Psi (design error
## only; respondents fixed at the fit's sample). It is not a certificate:
## an O_p(M^{-1/2}) gap does not establish sqrt(N)|Psi_M - Psi| = o_p(1).
## Chunked like the kernel.
ame_design_mc_se <- function(mu, kappa, Sigma, d_focal, d_ref,
                             n_nodes = 31L, position_neutral = FALSE,
                             chunk = 1024L) {
  gh <- ame_gh_nodes(n_nodes)
  M <- nrow(d_focal)
  per_draw <- numeric(M)
  signs <- if (isTRUE(position_neutral)) c(1, -1) else 1
  starts <- seq.int(1L, M, by = as.integer(chunk))
  one_side <- function(Dc, sg) {
    m <- mu %*% t(Dc)
    v <- pmax(rowSums((Dc %*% Sigma) * Dc), 0)
    s <- sqrt(v)
    val <- matrix(0, nrow(mu), nrow(Dc))
    pos <- v > .ame_v_eps
    if (any(pos)) {
      mp <- m[, pos, drop = FALSE]
      for (j in seq_along(gh$x)) {
        idx <- sweep(mp, 2L, s[pos] * gh$x[j], `+`) + sg * kappa
        val[, pos] <- val[, pos] + gh$w[j] * stats::plogis(idx)
      }
    }
    if (any(!pos)) {
      val[, !pos] <- stats::plogis(m[, !pos, drop = FALSE] + sg * kappa)
    }
    val
  }
  for (st in starts) {
    en <- min(st + chunk - 1L, M)
    Df <- d_focal[st:en, , drop = FALSE]
    Dr <- d_ref[st:en, , drop = FALSE]
    acc <- 0
    for (sg in signs) {
      acc <- acc + (one_side(Df, sg) - one_side(Dr, sg)) / length(signs)
    }
    per_draw[st:en] <- colMeans(acc)
  }
  list(value = mean(per_draw), mc_se = stats::sd(per_draw) / sqrt(M))
}
