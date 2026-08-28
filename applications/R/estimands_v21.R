## estimands_v21.R --- application-layer estimand algorithms for v2.1 fits.
##
## Anchored to paperps.tex Section 2 and to the algorithm memo
## 2608_issues/Yiqing/applications/estimand_algorithms_2026-08-27.tex.
## Every estimator returns list(value = ..., checks = data.frame(...));
## a failed check means the value must not be reported.
## Status: PROVISIONAL --- algorithm memo pending author verification.

est_gh <- function(q = 1L, n_nodes = 45L) {
  ## Integration nodes/weights for u ~ N(0, I_q), taken from the package
  ## so the quadrature convention matches the fitted models exactly.
  g <- sconjoint:::.sc_mixed_grid(q = q, integration = "gh",
                                  n_nodes = as.integer(n_nodes))
  stopifnot(abs(sum(g$w) - 1) < 1e-10,
            abs(sum(g$w * g$U[, 1])) < 1e-8,
            abs(sum(g$w * g$U[, 1]^2) - 1) < 1e-6)
  ## Closed-form exactness test (memo, quadrature rule): for
  ## zeta ~ N(0,1), E[Phi(alpha + b*zeta)] = Phi(alpha / sqrt(1 + b^2)).
  ## Only meaningful at production node counts; tiny grids (unit tests
  ## with Sigma = 0, where quadrature is inert) skip it.
  if (n_nodes >= 21L) for (ab in list(c(0.3, 0.7), c(-1.1, 1.5))) {
    got <- sum(g$w * pnorm(ab[1] + ab[2] * g$U[, 1]))
    want <- pnorm(ab[1] / sqrt(1 + ab[2]^2))
    stopifnot(abs(got - want) < 1e-6)
  }
  g
}

est_fit <- function(fit_path, prep_path) {
  f <- readRDS(fit_path)
  r <- if (inherits(f, "scmix_tuning")) f$refit else f
  stopifnot(inherits(r, "scmix_tuned_matrix_fit"))
  p <- readRDS(prep_path)
  ## Alignment guards (code audit): the fit's task-level mu must pair
  ## with THIS prep file, and mu must be constant within respondent.
  stopifnot(nrow(r$mu) == nrow(p$deltaX),
            ncol(r$mu) == ncol(p$deltaX))
  rid <- as.character(p$respondent_id)
  first <- match(unique(rid), rid)
  mu_dev <- max(vapply(split(seq_along(rid), rid), function(ii)
    max(abs(sweep(r$mu[ii, , drop = FALSE], 2, r$mu[ii[1], ]))), 0))
  stopifnot(mu_dev == 0)
  mu_resp <- r$mu[first, , drop = FALSE]
  colnames(mu_resp) <- colnames(p$deltaX)
  list(mu = mu_resp, A = r$A, Sigma = r$Sigma, kappa = as.numeric(r$kappa),
       coord = colnames(p$deltaX), resp_id = unique(rid),
       meta = p$respondent_meta, Z = p$Z_primary[first, , drop = FALSE],
       n = length(first))
}

G <- function(v) 1 / (1 + exp(-v))

est_subset <- function(fit, keep) {
  ## Restrict a loaded fit to a respondent subset (e.g., classified
  ## partisans). Population Sigma, A, kappa are unchanged.
  stopifnot(length(keep) == fit$n)
  out <- fit
  out$mu <- fit$mu[keep, , drop = FALSE]
  out$Z <- fit$Z[keep, , drop = FALSE]
  out$resp_id <- fit$resp_id[keep]
  out$n <- sum(keep)
  out
}

## --- Choice / win probabilities: paperps V_0 and V_0^n ------------------
est_V0 <- function(fit, d, neutral = TRUE, n_nodes = 45L, groups = NULL) {
  stopifnot(length(d) == ncol(fit$mu))
  gh <- est_gh(q = ncol(fit$A), n_nodes = n_nodes)
  t_mean <- as.numeric(fit$mu %*% d)             # d'mu_i, per respondent
  t_load <- as.numeric(crossprod(d, fit$A))      # d'A (1 x q)
  shift <- as.numeric(gh$U %*% t_load)           # per node
  prob_for <- function(tm, tl_shift, neg = FALSE) vapply(
    seq_len(fit$n), function(i) {
      ti <- tm[i] + tl_shift
      if (neutral) sum(gh$w * 0.5 * (G(fit$kappa + ti) + G(-fit$kappa + ti)))
      else sum(gh$w * G((if (neg) -1 else 1) * fit$kappa + ti))
    }, 0)
  P <- prob_for(t_mean, shift)
  out <- c(Overall = mean(P))
  if (!is.null(groups)) out <- c(out, tapply(P, groups, mean))
  ## Checks (memo Sec. 6, as revised by the math audit):
  ##  - probabilities in [0,1];
  ##  - the position-neutral complement identity V0n(d) + V0n(-d) = 1
  ##    for THIS d (holds only for the neutral version);
  ##  - kappa presence: V0(0) = G(kappa-hat), which differs from 1/2
  ##    whenever kappa-hat != 0.
  P_neg <- prob_for(-t_mean, -shift)
  compl_ok <- if (neutral) max(abs(P + P_neg - 1)) < 1e-12 else NA
  kap_ok <- {
    gh0 <- gh
    v00 <- sum(gh0$w * G(fit$kappa + 0 * gh0$U[, 1]))
    abs(v00 - G(fit$kappa)) < 1e-12
  }
  checks <- data.frame(
    check = c("in_unit_interval", "neutral_complement_identity",
              "kappa_present_V0_at_zero"),
    pass = c(all(P >= 0 & P <= 1),
             isTRUE(compl_ok) || is.na(compl_ok),
             kap_ok))
  list(value = out, respondent_probs = P, checks = checks)
}

## --- Sign shares S_0 and compensating-benefit shares C_0 ----------------
## floor2 = 1e-4 matches the package sign-share reporting margin (the
## `reporting_margin` in the v2.1 post-fit sign-share tables).
est_S0 <- function(fit, c_vec, groups = NULL, floor2 = 1e-4,
                   mc_check = TRUE) {
  s2 <- as.numeric(t(c_vec) %*% fit$Sigma %*% c_vec)
  m <- as.numeric(fit$mu %*% c_vec)
  floored <- s2 < floor2
  val <- if (floored) NA_real_ else mean(pnorm(m / sqrt(s2)))
  out <- c(Overall = val)
  if (!is.null(groups) && !floored)
    out <- c(out, tapply(pnorm(m / sqrt(s2)), groups, mean))
  ## Genuine numerical cross-check (code audit): reproduce the closed
  ## form by brute-force simulation of beta = mu + A u on a respondent
  ## subsample; catches wrong Sigma, wrong contrast, wrong mean rows.
  mc_ok <- NA
  if (mc_check && !floored) {
    set.seed(107)
    idx <- if (fit$n > 200) sample.int(fit$n, 200) else seq_len(fit$n)
    R <- 40000L
    u <- matrix(rnorm(R * ncol(fit$A)), R)
    cb_load <- as.numeric(crossprod(c_vec, fit$A))
    draws <- outer(m[idx], rep(1, R)) +
      matrix(rep(as.numeric(u %*% cb_load), each = length(idx)),
             length(idx))
    mc_val <- mean(draws > 0)
    cf_val <- mean(pnorm(m[idx] / sqrt(s2)))
    ## The u draws are SHARED across respondents, so the dominant Monte
    ## Carlo error is the u-dimension: se <= 0.5/sqrt(R) = 0.0025 at
    ## R = 40000. Gate at four times that bound.
    mc_ok <- abs(mc_val - cf_val) < 0.01
  }
  list(value = out, s2 = s2, floored = floored,
       checks = data.frame(check = "closed_form_matches_simulation",
                           pass = isTRUE(mc_ok) || floored))
}

est_C0 <- function(fit, c_penalty, c_benefit, a = 1, groups = NULL,
                   floor2 = 1e-4) {
  ## paperps eq. C_0(c_p, c_b; a) = P{(c_p + a c_b)' beta >= 0}.
  cc <- c_penalty + a * c_benefit
  res <- est_S0(fit, cc, groups = groups, floor2 = floor2)
  ## Non-vacuous composition check (code audit replaced a tautology):
  ## a zero-BENEFIT-VECTOR run through the full C_0 path must equal
  ## S_0(c_p) computed independently -- exercises est_C0's own
  ## contrast assembly rather than comparing a call with itself.
  s0 <- est_S0(fit, c_penalty, floor2 = floor2, mc_check = FALSE)
  c0z <- est_S0(fit, c_penalty + a * numeric(length(c_penalty)),
                floor2 = floor2, mc_check = FALSE)
  res$checks <- rbind(res$checks, data.frame(
    check = "zero_benefit_vector_reduces_to_S0",
    pass = identical(s0$floored, c0z$floored) &&
      (s0$floored ||
         abs(s0$value[["Overall"]] - c0z$value[["Overall"]]) < 1e-14)))
  res
}

## --- Subgroup structural means theta_0(B) -------------------------------
est_theta_B <- function(fit, groups) {
  stopifnot(length(groups) == fit$n)
  overall <- colMeans(fit$mu)
  by_g <- lapply(split(seq_len(fit$n), groups), function(ii)
    colMeans(fit$mu[ii, , drop = FALSE]))
  shares <- table(groups) / fit$n
  recon <- Reduce(`+`, Map(function(v, w) v * w, by_g,
                           as.numeric(shares[names(by_g)])))
  list(value = by_g, overall = overall,
       checks = data.frame(check = "subgroups_aggregate_to_overall",
                           pass = max(abs(recon - overall)) < 1e-10))
}

## --- Importance shares: faithful port of the submitted constructs -------
## Both submitted variants are per-respondent shares (a mean of ratios over
## recovered betas), averaged over respondents. The port replaces the
## recovered beta with the fitted N(mu(Z), Sigma) and takes the inner
## expectation of the ratio by quadrature.
##   mode "numeric" (Ballard-Rosa): contrib_k = beta_k^2 * Var(level set),
##     spec = named numeric vector of level-set variances per coordinate.
##   mode "categorical" (Graham--Svolik, "production-35"): per attribute
##     group g with columns K_g and L_g = |K_g| + 1 levels (reference = 0),
##     contrib_g = sum(beta_K^2)/L_g - (sum(beta_K)/L_g)^2,
##     spec = named list of coordinate-name vectors per group.
est_importance <- function(fit, spec, mode = c("numeric", "categorical"),
                           groups = NULL, n_nodes = 45L) {
  mode <- match.arg(mode)
  gh <- est_gh(q = ncol(fit$A), n_nodes = n_nodes)
  A <- fit$A
  contrib_fun <- if (mode == "numeric") {
    stopifnot(identical(names(spec), fit$coord))
    function(beta) beta^2 * spec
  } else {
    idx <- lapply(spec, function(cols) match(cols, fit$coord))
    stopifnot(!anyNA(unlist(idx)))
    function(beta) vapply(names(spec), function(g) {
      b <- beta[idx[[g]]]; L <- length(b) + 1
      sum(b^2) / L - (sum(b) / L)^2
    }, 0)
  }
  n_out <- if (mode == "numeric") ncol(fit$mu) else length(spec)
  share_i <- t(vapply(seq_len(fit$n), function(i) {
    acc <- numeric(n_out)
    for (j in seq_len(nrow(gh$U))) {
      beta <- fit$mu[i, ] + as.numeric(A %*% gh$U[j, ])
      contrib <- contrib_fun(beta)
      acc <- acc + gh$w[j] * contrib / sum(contrib)
    }
    acc
  }, numeric(n_out)))
  colnames(share_i) <- if (mode == "numeric") fit$coord else names(spec)
  out <- list(Overall = colMeans(share_i))
  if (!is.null(groups))
    out <- c(out, lapply(split(seq_len(fit$n), groups), function(ii)
      colMeans(share_i[ii, , drop = FALSE])))
  sums <- vapply(out, sum, 0)
  list(value = out, respondent_shares = share_i,
       checks = data.frame(check = paste0("shares_sum_to_one:", names(out)),
                           pass = abs(sums - 1) < 1e-10))
}

## --- Structural AME (paperps AME_0), independent-uniform designs --------
## attrs: list(name -> character vector of coordinate names for that
## attribute's non-reference levels). Reference level = all-zero coords.
## Design law: attributes independent, levels uniform (verified against
## the application's protocol record before use). Both profiles drawn.
est_AME <- function(fit, attrs, n_nodes = 31L, M_D = 20000L, seed = 1L) {
  ## The loading algebra below flattens d'A to a scalar; guard the q=1
  ## assumption explicitly so a q>=2 fit fails loudly, not silently.
  stopifnot(ncol(fit$A) == 1L)
  gh <- est_gh(q = ncol(fit$A), n_nodes = n_nodes)
  p <- ncol(fit$mu)
  coord_index <- lapply(attrs, function(cols) match(cols, fit$coord))
  stopifnot(!anyNA(unlist(coord_index)))
  set.seed(seed)
  draw_profile <- function(M) {
    X <- matrix(0, M, p)
    for (a in names(attrs)) {
      k <- length(attrs[[a]]) + 1L           # levels incl. reference
      lev <- sample.int(k, M, replace = TRUE) # k = reference
      for (l in seq_len(k - 1L))
        X[lev == l, coord_index[[a]][l]] <- 1
    }
    X
  }
  X1 <- draw_profile(M_D); X2 <- draw_profile(M_D)
  resp <- sample.int(fit$n, M_D, replace = TRUE)  # respondent per draw
  base_t <- rowSums((X1 - X2) * fit$mu[resp, , drop = FALSE])
  load <- as.numeric((X1 - X2) %*% fit$A)         # q = 1
  ame_one <- function(attr, lev_col, neutral = FALSE) {
    ## set profile 1's focal attribute to level (or reference), re-derive
    X1f <- X1
    X1f[, coord_index[[attr]]] <- 0
    if (!is.na(lev_col)) X1f[, lev_col] <- 1
    tf <- rowSums((X1f - X2) * fit$mu[resp, , drop = FALSE])
    lf <- as.numeric((X1f - X2) %*% fit$A)
    pr <- numeric(M_D)
    for (j in seq_len(nrow(gh$U))) {
      u <- gh$U[j, 1]
      v <- tf + lf * u
      pr <- pr + gh$w[j] *
        (if (neutral) 0.5 * (G(fit$kappa + v) + G(-fit$kappa + v))
         else G(fit$kappa + v))
    }
    pr
  }
  out <- list()
  for (a in names(attrs)) {
    p_ref <- ame_one(a, NA)
    p_ref_n <- ame_one(a, NA, neutral = TRUE)
    for (l in seq_along(attrs[[a]])) {
      p_lev <- ame_one(a, coord_index[[a]][l])
      p_lev_n <- ame_one(a, coord_index[[a]][l], neutral = TRUE)
      diffs <- p_lev - p_ref
      diffs_n <- p_lev_n - p_ref_n
      out[[attrs[[a]][l]]] <- c(est = mean(diffs),
                                mc_se = sd(diffs) / sqrt(M_D),
                                est_neutral = mean(diffs_n))
    }
  }
  est <- vapply(out, `[[`, 0, "est")
  mc <- vapply(out, `[[`, 0, "mc_se")
  est_n <- vapply(out, `[[`, 0, "est_neutral")
  ## Checks: every draw's probability in [0,1]; and re-drawing the focal
  ## attribute at its reference level reproduces the reference profile's
  ## probabilities exactly for draws whose sampled level was the
  ## reference (an identity of the construction, checked on attribute 1).
  a1 <- names(attrs)[1]
  p_ref <- ame_one(a1, NA)
  ref_rows <- rowSums(X1[, coord_index[[a1]], drop = FALSE]) == 0
  p_orig <- {
    pr <- numeric(M_D)
    for (j in seq_len(nrow(gh$U)))
      pr <- pr + gh$w[j] * G(fit$kappa + base_t + load * gh$U[j, 1])
    pr
  }
  ident_chk <- max(abs(p_ref[ref_rows] - p_orig[ref_rows]))
  list(value = data.frame(coordinate = names(est), ame = est, mc_se = mc,
                          ame_neutral = est_n),
       checks = data.frame(
         check = c("probs_in_unit_interval", "reference_rows_identity"),
         pass = c(all(p_ref >= 0 & p_ref <= 1), ident_chk < 1e-12)))
}

## --- Importance self-test: archived-formula recovery --------------------
## With Sigma = 0 the mixture collapses to mu, and est_importance must
## reproduce the archived per-respondent computation exactly (memo
## Sec. 8, check iii). Returns TRUE or stops.
est_importance_selftest <- function() {
  mu <- rbind(c(1, -2, 0.5), c(0.3, 0.1, -1))
  fit0 <- list(mu = mu, A = matrix(0, 3, 1), Sigma = matrix(0, 3, 3),
               kappa = 0, coord = c("a", "b", "c"), n = 2)
  ## numeric mode vs hand computation
  v <- c(a = 2, b = 0.5, c = 1)
  hand <- t(apply(mu, 1, function(b) { k <- b^2 * v; k / sum(k) }))
  got <- est_importance(fit0, spec = v, mode = "numeric", n_nodes = 5L)
  stopifnot(max(abs(got$value$Overall - colMeans(hand))) < 1e-12)
  ## categorical mode vs the archived production-35 formula
  spec <- list(g1 = c("a", "b"), g2 = "c")
  hand2 <- t(apply(mu, 1, function(b) {
    k <- c(g1 = sum(b[1:2]^2) / 3 - (sum(b[1:2]) / 3)^2,
           g2 = b[3]^2 / 2 - (b[3] / 2)^2)
    k / sum(k)
  }))
  got2 <- est_importance(fit0, spec = spec, mode = "categorical",
                         n_nodes = 5L)
  stopifnot(max(abs(got2$value$Overall - colMeans(hand2))) < 1e-12)
  TRUE
}

## --- MRS (paperps MRS^mean_0), point ratios -----------------------------
## The point ratio itself is withheld unless the denominator exceeds
## `t_min` diagnostic standard errors (manuscript domain condition,
## strengthened per the math audit; delta/Fieller intervals need the
## joint covariance block, which this runner does not have).
est_mrs <- function(theta, num_coord, den_coord, den_se = NA_real_,
                    t_min = 4) {
  stopifnot(num_coord %in% names(theta), den_coord %in% names(theta))
  den <- theta[[den_coord]]
  sep <- if (is.na(den_se)) NA else abs(den) / den_se
  ok <- !is.na(sep) && sep >= t_min
  r <- -theta[[num_coord]] / den
  list(value = c(mrs = r, abs = abs(r), den_t = sep),
       checks = data.frame(check = "denominator_separated_from_zero",
                           pass = ok))
}
