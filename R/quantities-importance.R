#' Attribute importance via per-respondent variance decomposition
#'
#' For each respondent \eqn{i} and attribute group \eqn{g}, computes a
#' utility-variance share \eqn{\mathrm{share}_{i,g}}, then averages
#' over the subgroup.
#'
#' Four weighting conventions are supported, selected by `design`:
#'
#' \itemize{
#'   \item `"design_variance"` (default) implements the formula as
#'     displayed in the paper's text, \eqn{\mathrm{Imp}_{i,g} =
#'     \sum_{k \in g} \hat\beta_{ik}^2 \cdot \mathrm{Var}(\Delta X_k)},
#'     normalized to shares across groups, with `Var(ΔX_k)` computed
#'     empirically from `object$deltaX`.
#'   \item `"uniform"` computes the variance of the attribute's utility
#'     contribution over a uniform distribution on its levels (treating
#'     the reference level as \eqn{\beta = 0}).  **This is the formula
#'     behind the importance shares reported in Acharya, Hainmueller,
#'     and Xu (2026) for the factor-attribute applications** (Saha-Weeks
#'     agenda 52/talent 21/gender 17; Graham-Svolik policy 28/valence
#'     26/party 25/undemocratic 8): the production pipeline decomposes
#'     utility variance per attribute over uniform level draws.
#'   \item `"levels"` extends `"uniform"` to continuous attributes:
#'     attributes named in the `levels` argument contribute
#'     \eqn{\hat\beta_{ik}^2 \cdot \mathrm{Var}(L_k)}, where
#'     \eqn{\mathrm{Var}(L_k)} is the population variance of the
#'     attribute's design level set \eqn{L_k} (e.g.
#'     `c(0, 5, 15, 25)` for a tax-rate bracket); all other attributes
#'     use the `"uniform"` formula.  This reproduces the production
#'     pipeline's importance for continuous-attribute designs (the
#'     Ballard-Rosa by-party importance in the paper).
#'   \item `"empirical"` computes the variance of \eqn{\beta_{i,\cdot}}
#'     over the empirical level distribution (level frequencies read
#'     off `object$deltaX`).
#' }
#'
#' Note `"design_variance"` and `"uniform"` answer slightly different
#' questions and do NOT agree in general: `Var(ΔX_k)` couples dummies
#' of the same attribute through the realized pairing of profiles,
#' while `"uniform"` integrates over the attribute's marginal level
#' distribution and accounts for within-attribute covariance of the
#' dummies.  When reproducing the paper's reported shares, use
#' `"uniform"` (factor designs) or `"levels"` (continuous designs).
#'
#' @param object An `sc_fit`.
#' @param design One of `"design_variance"` (default; the paper-text
#'   formula), `"uniform"` (production formula for factor designs),
#'   `"levels"` (production formula with explicit level sets for
#'   continuous attributes), or `"empirical"`.
#' @param levels Named list of numeric vectors, used by
#'   `design = "levels"`: names are attribute names (as in
#'   `names(object$attr_map)`) of *single-column continuous*
#'   attributes; values are the attribute's design level sets.
#'   Attributes not listed fall back to the `"uniform"` formula.
#' @param subgroup Row selector.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @return An `sc_quantity` whose `estimate` is a data.frame with one
#'   row per attribute (columns: `attribute`, `share`, `se`, `ci_lo`,
#'   `ci_hi`).
#' @export
sc_importance <- function(object,
                          design = c("design_variance", "uniform", "empirical",
                                     "levels"),
                          vartype = c("plugin", "orthogonal"),
                          subgroup = NULL,
                          which_beta = c("hybrid", "dnn"),
                          levels = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  design <- match.arg(design)
  vartype <- match.arg(vartype)
  which_beta <- match.arg(which_beta)
  map <- .sc_attr_map(object)

  ## Opt-in debiased (orthogonal-score) shares -- the paper's Appendix C
  ## estimand: share_a = N_a / sum_a N_a with N_a = E[f_a' S_a f_a] and a
  ## clustered simplex-Jacobian standard error, on the Stage-1 f-hat
  ## (`subgroup`/`which_beta` do not apply; it is a population functional).
  ## The default plug-in shares below are always in [0, 1]; the debiased
  ## shares are a ratio and can fall outside [0, 1] when the first stage is
  ## noisy or under-fit, so they are opt-in via vartype = "orthogonal".
  if (vartype == "orthogonal") {
    if (design != "design_variance") {
      stop("sc_importance(): vartype = \"orthogonal\" is implemented only for ",
           "design = \"design_variance\" (the paper's weighting).", call. = FALSE)
    }
    df <- .sc_debiased_importance(object, map)
    return(.sc_quantity(
      name = "importance", estimate = df, se = NA_real_,
      details = list(design = design, vartype = "orthogonal",
                     se_method = "debiased orthogonal score, respondent-clustered"),
      call = match.call()))
  }

  B <- .sc_pick_beta(object, which_beta)
  S <- .sc_resolve_subgroup(object, subgroup)
  resp_s <- object$respondent_id[S]
  attrs <- names(map)
  K <- length(attrs)
  n_S <- length(S)

  if (identical(design, "levels")) {
    if (!is.null(levels)) {
      if (is.null(names(levels)) || !all(nzchar(names(levels))))
        stop("sc_importance(design = \"levels\"): `levels` must be a named list.",
             call. = FALSE)
      bad <- setdiff(names(levels), attrs)
      if (length(bad))
        stop("sc_importance(): unknown attribute(s) in `levels`: ",
             paste(bad, collapse = ", "), call. = FALSE)
      for (nm in names(levels)) {
        if (length(map[[nm]]) != 1L)
          stop("sc_importance(): `levels` applies to single-column ",
               "(continuous) attributes; '", nm, "' has ",
               length(map[[nm]]), " columns.", call. = FALSE)
        if (!is.numeric(levels[[nm]]) || length(levels[[nm]]) < 2L)
          stop("sc_importance(): `levels$", nm,
               "` must be a numeric vector of at least 2 design levels.",
               call. = FALSE)
      }
    }
  } else if (!is.null(levels)) {
    warning("sc_importance(): `levels` is ignored unless design = \"levels\".",
            call. = FALSE)
  }

  ## design_variance and empirical both need deltaX
  if (design %in% c("design_variance", "empirical")) {
    dX <- object$deltaX
    if (is.null(dX)) {
      stop(sprintf(
        "sc_importance(design='%s'): object$deltaX not stored.", design),
        call. = FALSE)
    }
  }

  ## Compute per-row V_{i,g} under the chosen design.
  V_mat <- matrix(0, n_S, K)
  for (a in seq_len(K)) {
    cols <- map[[attrs[a]]]
    if (identical(design, "design_variance")) {
      ## Paper formula: per-respondent sum_k beta_ik^2 * Var(deltaX_k).
      ## Var is the empirical column variance of deltaX (over all
      ## task rows in the full sample, not just the subgroup S),
      ## matching the paper's "given the realized design" framing.
      dvar <- apply(dX[, cols, drop = FALSE], 2L, stats::var)
      bsub <- B[S, cols, drop = FALSE]
      V_mat[, a] <- as.numeric((bsub^2) %*% dvar)
    } else if (identical(design, "uniform") ||
               (identical(design, "levels") &&
                !(attrs[a] %in% names(levels)))) {
      ## L_a = number of levels including reference; dummy contribution
      ## for the reference level is 0.
      L_a <- length(cols) + 1L
      bsub <- B[S, cols, drop = FALSE]
      m1 <- rowSums(bsub) / L_a
      m2 <- rowSums(bsub^2) / L_a
      V_mat[, a] <- m2 - m1^2
    } else if (identical(design, "levels")) {
      ## Continuous attribute with an explicit design level set:
      ## beta^2 * population variance of the level values.
      l <- levels[[attrs[a]]]
      var_lvl <- mean((l - mean(l))^2)
      bsub <- B[S, cols, drop = FALSE]
      V_mat[, a] <- as.numeric(bsub^2) * var_lvl
    } else {
      ## "empirical": variance of beta over empirical level distribution.
      w <- colMeans(abs(dX[, cols, drop = FALSE]))
      w_ref <- max(1 - sum(w), 0)
      tot <- w_ref + sum(w)
      if (tot == 0) {
        m1 <- rep(0, n_S); m2 <- rep(0, n_S)
      } else {
        w_norm <- w / tot
        bsub <- B[S, cols, drop = FALSE]
        m1 <- as.numeric(bsub %*% w_norm)
        m2 <- as.numeric((bsub^2) %*% w_norm)
      }
      V_mat[, a] <- m2 - m1^2
    }
  }
  ## Shares: per-row normalization, then average.
  row_sum <- rowSums(V_mat)
  row_sum[row_sum == 0] <- NA_real_
  share_mat <- V_mat / row_sum
  share_mat[is.na(share_mat)] <- 0
  est <- colMeans(share_mat)
  se  <- vapply(seq_len(K), function(a) {
    .sc_cluster_se(share_mat[, a], resp_s)
  }, numeric(1L))
  ci_q <- stats::qnorm(0.975)
  df <- data.frame(
    attribute = attrs,
    share     = est,
    se        = se,
    ci_lo     = est - ci_q * se,
    ci_hi     = est + ci_q * se,
    stringsAsFactors = FALSE
  )
  .sc_quantity(
    name = "importance",
    estimate = df,
    se = NA_real_,
    details = list(
      per_row_shares = share_mat,
      design         = design,
      subgroup_size  = n_S
    ),
    call = match.call()
  )
}
