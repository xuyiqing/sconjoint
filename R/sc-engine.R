# The paper's estimation engine (paper-v4): held-out one-step corrections and bounded updates.
#
# Ported from the author scripts of the paper's rebuild: x_onestep() (analysis/2026-10-11_rum_inference/
# scripts/X_common.R:122-145) and the held-out correction directions that the rebuild substitutes for
# .rum_tmle_dirs() and .rum_path_m() (replication/2026-11_v4/rebuild_results.R:37-48, exhibit_helpers.R:21-44).
# The arithmetic is unchanged, so the numbers are those of the paper.
#
# Held-out calibration (paper, Appendix "Effect of covariate calibration"). Fold k's state holds the
# information parts of its held-out respondents only (built by sc_states()). The correction for a target
# solves the information equation with v = (w / Wbar) m_mu on the held-out rows and the common-parameter
# derivative gt_bar = E[(w / Wbar) m_theta] averaged over ALL n respondents at fold k's fitted state. The
# package's older .rum_onestep() builds information for every respondent instead; it is kept unchanged.
#
# A fold state FS[[k]] (names "1".."K") is a list with
#   k, te         the fold and its held-out positions (data$ids order);
#   mu            n x p fitted means for every respondent (fold k's network);
#   th            list(kappa, loading on principal axes, omega);
#   output_score, common_score   the held-out rows' scores (length(te) rows) on the score rule;
#   parts         the held-out rows' weight parts (.rum_weight_parts with the rotations removed);
#   sub           rum_subset of the held-out respondents (added by .sc_attach_sub(); bounded updates).

# The held-out one-step for a list of internal targets. Returns the list of x_onestep(): est, plug,
# IFF, IFP (n x targets), phi, m, kind, n_group.
.sc_onestep <- function(FS, data, targets) {
  n <- data$n; nt <- length(targets); TN <- vapply(targets, `[[`, "", "name")
  wmat <- .rum_target_weights(targets, n); if (!is.matrix(wmat)) wmat <- matrix(wmat, n)
  PHI <- M <- matrix(NA_real_, n, nt)
  for (k in names(FS)) {
    pc <- FS[[k]]; te <- pc$te
    for (t in seq_len(nt)) {
      w <- wmat[, t]; wr <- w / mean(w); gr <- .rum_target_grad(targets[[t]], pc$mu, pc$th)
      cw <- .rum_correction(pc$parts, (wr * gr$gm)[te, , drop = FALSE], colMeans(wr * gr$gt), seq_along(te))
      PHI[te, t] <- rowSums(cw$a * pc$output_score) + drop(pc$common_score %*% cw$g)
      M[te, t] <- gr$m[te]
    }
  }
  IFF <- IFP <- matrix(NA_real_, n, nt); est <- plug <- numeric(nt)
  for (t in seq_len(nt)) {
    w <- wmat[, t]; wr <- w / mean(w); plug[t] <- sum(w * M[, t]) / sum(w); est[t] <- plug[t] + mean(PHI[, t])
    IFF[, t] <- PHI[, t] - mean(PHI[, t]); IFP[, t] <- PHI[, t] + wr * (M[, t] - est[t])
  }
  names(est) <- names(plug) <- colnames(IFF) <- colnames(IFP) <- colnames(PHI) <- colnames(M) <- TN
  list(est = est, plug = plug, IFF = IFF, IFP = IFP, phi = PHI, m = M,
       kind = stats::setNames(vapply(targets, `[[`, "", "kind"), TN),
       n_group = stats::setNames(colSums(wmat > 0), TN))
}

# Held-out correction directions (the rebuild's override of .rum_tmle_dirs).
.sc_heldout_dirs <- function(FS, tg, w) {
  wr <- w / mean(w)
  lapply(FS, function(f) {
    gr <- .rum_target_grad(tg, f$mu, f$th)
    cw <- .rum_correction(f$parts, (wr * gr$gm)[f$te, , drop = FALSE], colMeans(wr * gr$gt), seq_along(f$te))
    list(a = cw$a, g = cw$g, plug = gr$m[f$te])
  })
}
# Plug-in values at the moved held-out states, evaluated on the held-out rows only (the rebuild's
# override of .rum_path_m for importance; the same values for any per-respondent target).
.sc_heldout_path_m <- function(FS, ev, tg, n) {
  m <- numeric(n)
  for (k in names(FS)) { f <- FS[[k]]; r <- ev$st[[k]]; m[f$te] <- tg$m(r$mu, r$th) }
  m
}

# Fold of every respondent from the states.
.sc_fold_vector <- function(FS, n) {
  fo <- integer(n); for (k in names(FS)) fo[FS[[k]]$te] <- as.integer(k); fo
}
# Add the held-out data subsets the bounded updates evaluate.
.sc_attach_sub <- function(FS, data) {
  for (k in names(FS)) if (is.null(FS[[k]]$sub)) FS[[k]]$sub <- rum_subset(data, data$ids[FS[[k]]$te])
  FS
}

# Bounded (targeted) update for single shares, held-out directions; the rebuild's call
# tmle(d, NULL, fold, NULL, targets, quad_score = FS[[1]]$quad_score, FS = FS) with package defaults.
.sc_bounded_path <- function(FS, data, targets, quad) {
  FS <- .sc_attach_sub(FS, data)
  .rum_tmle_path(data, NULL, .sc_fold_vector(FS, data$n), NULL, targets, quad_score = quad, FS = FS,
                 dirs_fun = .sc_heldout_dirs)
}
# Joint simplex update for a complete set of shares (importance), held-out directions and plug-ins.
.sc_bounded_joint <- function(FS, data, targets, quad, max_iter = 50L) {
  FS <- .sc_attach_sub(FS, data)
  .rum_tmle_joint(data, NULL, .sc_fold_vector(FS, data$n), NULL, targets, quad_score = quad, FS = FS,
                  max_iter = max_iter, dirs_fun = .sc_heldout_dirs, path_m_fun = .sc_heldout_path_m)
}

# Interval rules. Log-odds (paper eq. logodds_interval): plogis(qlogis(p) -/+ z se / (p (1 - p))),
# NA unless 0 < p < 1; Wald: est -/+ z se.
.sc_logodds_ci <- function(est, se, z = 1.96) {
  p <- ifelse(est > 0 & est < 1, est, NA_real_)
  cbind(stats::plogis(stats::qlogis(p) - z * se / (p * (1 - p))),
        stats::plogis(stats::qlogis(p) + z * se / (p * (1 - p))))
}
# Interval type by target kind.
.sc_interval_kind <- function(kind)
  ifelse(kind %in% c("choice", "share", "decisiveness"), "log-odds",
         ifelse(kind == "importance", "none", "wald"))
