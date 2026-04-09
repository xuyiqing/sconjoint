#' Attribute importance via per-respondent variance decomposition
#'
#' For each respondent \eqn{i} and attribute block \eqn{a}, computes
#' a within-block variance-share \eqn{\mathrm{share}_{ia}}, then
#' averages over the subgroup.  Point estimate matches the prototype
#' `07b_structural_quantities.R` lines 643--664; the clustered SE of
#' the per-row share is new in sconjoint v0.1.
#'
#' `design = "uniform"` weights each level of each attribute equally
#' (the prototype's default and the only branch tested for parity in
#' v0.1).  `design = "empirical"` replaces the uniform weights by
#' empirical level frequencies computed from `object$deltaX` and is
#' documented but not parity-tested.
#'
#' @param object An `sc_fit`.
#' @param design Either `"uniform"` or `"empirical"`.
#' @param subgroup Row selector.
#' @return An `sc_quantity` whose `estimate` is a data.frame with one
#'   row per attribute (columns: `attribute`, `share`, `se`, `ci_lo`,
#'   `ci_hi`).
#' @export
sc_importance <- function(object,
                          design = c("uniform", "empirical"),
                          subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  design <- match.arg(design)
  map <- .sc_attr_map(object)
  B <- object$beta_hat
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
