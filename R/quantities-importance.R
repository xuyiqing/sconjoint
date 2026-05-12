#' Attribute importance via per-respondent variance decomposition
#'
#' For each respondent \eqn{i} and attribute block \eqn{a}, computes
#' a within-block variance-share \eqn{\mathrm{share}_{ia}}, then
#' averages over the subgroup.  Point estimate matches the prototype
#' `07b_structural_quantities.R` lines 643--664; the clustered SE of
#' the per-row share is new in sconjoint v0.1.
#'
#' `design = "uniform"` (default) weights each level of each
#' attribute equally; this is the prototype's default and matches the
#' paper's sw2022 agenda share more closely than empirical weighting
#' under the package's training config (see Note).
#' `design = "empirical"` replaces the uniform weights by empirical
#' level frequencies computed from `object$deltaX`.
#'
#' @note A 0.2.0.9000 spot-check on sw2022 found neither weighting
#'   matches the paper's reported agenda share of 0.65 under
#'   the package defaults (K=5, n_epochs=200, hybrid path).
#'   Uniform gives ~0.40, empirical ~0.31.  Switching to the DNN-only
#'   path (`which_beta = "dnn"`) brings uniform to ~0.57, much closer
#'   to the paper.  The remaining gap appears to be a training-config
#'   difference (paper uses K=50, n_epochs=5000), not a weighting-
#'   formula difference; see `statsclaw-workspace/sconjoint/ref/
#'   se-ratio-and-importance-share.md`.
#'
#' @param object An `sc_fit`.
#' @param design Either `"uniform"` (default) or `"empirical"`.
#' @param subgroup Row selector.
#' @param which_beta Either `"hybrid"` (default) or `"dnn"`. See `?sc_mrs`.
#' @return An `sc_quantity` whose `estimate` is a data.frame with one
#'   row per attribute (columns: `attribute`, `share`, `se`, `ci_lo`,
#'   `ci_hi`).
#' @export
sc_importance <- function(object,
                          design = c("uniform", "empirical"),
                          subgroup = NULL,
                          which_beta = c("hybrid", "dnn")) {
  stopifnot(inherits(object, "sc_fit"))
  design <- match.arg(design)
  which_beta <- match.arg(which_beta)
  map <- .sc_attr_map(object)
  B <- .sc_pick_beta(object, which_beta)
  S <- .sc_resolve_subgroup(object, subgroup)
  resp_s <- object$respondent_id[S]
  attrs <- names(map)
  K <- length(attrs)
  n_S <- length(S)
  ## Compute per-row V_{ia} under the chosen design.
  V_mat <- matrix(0, n_S, K)
  for (a in seq_len(K)) {
    cols <- map[[attrs[a]]]
    if (identical(design, "uniform")) {
      ## L_a = number of levels including reference; dummy contribution
      ## for the reference level is 0.
      L_a <- length(cols) + 1L
      bsub <- B[S, cols, drop = FALSE]
      ## mean over { 0, B[S, cols] } is sum(B)/L_a
      m1 <- rowSums(bsub) / L_a
      m2 <- rowSums(bsub^2) / L_a
    } else {
      ## Empirical frequencies from deltaX absolute values (treat
      ## nonzero as "level selected"); v0.1 approximation.
      dX <- object$deltaX
      if (is.null(dX)) {
        stop("sc_importance(design='empirical'): object$deltaX not stored.")
      }
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
    }
    V_mat[, a] <- m2 - m1^2
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
