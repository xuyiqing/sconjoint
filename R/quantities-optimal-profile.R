#' Optimal profile via greedy per-attribute argmax
#'
#' For each attribute block, picks the level (including the
#' reference level, which contributes 0 to utility) whose
#' subgroup-averaged \eqn{\hat\beta_j(Z)} is largest.  Assembles a
#' dummy vector \eqn{X^\star} and reports the respondent-averaged
#' choice probability of \eqn{X^\star} against the all-reference
#' profile via \eqn{\hat p^\star = |S|^{-1}\sum_{i\in S} G((X^\star)^\top
#' \hat\beta(Z_i))}.
#'
#' Point estimate matches the prototype `07b_structural_quantities.R`
#' lines 526--563 (greedy loop + acceptance probability).  Clustered
#' SE on the per-row acceptance probability is new in sconjoint v0.1
#' and is conditional on the selected profile (argmax uncertainty is
#' not propagated).  `"exhaustive"` enumerates
#' \eqn{\prod_a L_a} profile combinations and errors out when the
#' product exceeds \eqn{10^5}.
#'
#' @param object An `sc_fit`.
#' @param search Either `"greedy"` (default) or `"exhaustive"`.
#' @param subgroup Row selector.
#' @return An `sc_quantity` with scalar estimate and rich `details`.
#' @export
sc_optimal_profile <- function(object,
                               search = c("greedy", "exhaustive"),
                               subgroup = NULL) {
  stopifnot(inherits(object, "sc_fit"))
  search <- match.arg(search)
  map <- .sc_attr_map(object)
  attrs <- names(map)
  B <- object$beta_hat
  S <- .sc_resolve_subgroup(object, subgroup)
  resp_s <- object$respondent_id[S]
  fl <- object$factor_levels
  p <- ncol(B)
  col_means <- colMeans(B[S, , drop = FALSE])
  pick_level <- function(cols) {
    ## vector of mean beta for each non-reference dummy of the block.
    cand <- c(0, col_means[cols])
    which.max(cand) - 1L  # 0 = reference, 1..L = non-reference
  }
  if (identical(search, "exhaustive")) {
    sizes <- vapply(map, function(cols) length(cols) + 1L, integer(1L))
    if (prod(as.numeric(sizes)) > 1e5) {
      stop(sprintf(
        "sc_optimal_profile(search='exhaustive'): %g combinations exceeds the 1e5 guard; use search='greedy'.",
        prod(as.numeric(sizes))
      ))
    }
    grid <- expand.grid(lapply(sizes, function(L) 0:(L - 1L)))
    best_val <- -Inf
    best_sel <- rep(0L, length(attrs))
    for (r in seq_len(nrow(grid))) {
      x_star <- numeric(p)
      for (a in seq_along(attrs)) {
        sel <- as.integer(grid[r, a])
        if (sel > 0L) x_star[map[[attrs[a]]][sel]] <- 1
      }
      lin <- as.numeric(B[S, , drop = FALSE] %*% x_star)
      pv <- mean(stats::plogis(lin))
      if (pv > best_val) {
        best_val <- pv
        best_sel <- as.integer(grid[r, ])
      }
    }
    sel_vec <- best_sel
  } else {
    sel_vec <- vapply(attrs, function(a) pick_level(map[[a]]), integer(1L))
  }
  x_star <- numeric(p)
  names(x_star) <- object$attr_names
  opt_profile <- list()
  for (a in seq_along(attrs)) {
    nm <- attrs[a]
    sel <- sel_vec[a]
    cols <- map[[nm]]
    if (sel == 0L) {
      opt_profile[[nm]] <- if (!is.null(fl) && !is.null(fl[[nm]])) fl[[nm]][1L] else 0
    } else {
      x_star[cols[sel]] <- 1
      if (!is.null(fl) && !is.null(fl[[nm]])) {
        opt_profile[[nm]] <- fl[[nm]][-1L][sel]
      } else {
        opt_profile[[nm]] <- 1
      }
    }
  }
  lin_star <- as.numeric(B[S, , drop = FALSE] %*% x_star)
  p_i <- stats::plogis(lin_star)
  est <- mean(p_i)
  se  <- .sc_cluster_se(p_i, resp_s)
  ci  <- .sc_ci_normal(est, se)
  ## worst profile diagnostic: greedy argmin
  worst_sel <- vapply(attrs, function(a) {
    cols <- map[[a]]
    cand <- c(0, col_means[cols])
    which.min(cand) - 1L
  }, integer(1L))
  x_worst <- numeric(p)
  worst_profile <- list()
  for (a in seq_along(attrs)) {
    nm <- attrs[a]
    sel <- worst_sel[a]
    cols <- map[[nm]]
    if (sel == 0L) {
      worst_profile[[nm]] <- if (!is.null(fl) && !is.null(fl[[nm]])) fl[[nm]][1L] else 0
    } else {
      x_worst[cols[sel]] <- 1
      if (!is.null(fl) && !is.null(fl[[nm]])) {
        worst_profile[[nm]] <- fl[[nm]][-1L][sel]
      } else {
        worst_profile[[nm]] <- 1
      }
    }
  }
  worst_prob_mean <- mean(stats::plogis(as.numeric(B[S, , drop = FALSE] %*% x_worst)))
  .sc_quantity(
    name = "optimal_profile",
    estimate = est,
    se = se,
    ci_lo = ci[1L],
    ci_hi = ci[2L],
    details = list(
      optimal_profile      = opt_profile,
      optimal_dummy_vector = x_star,
      worst_profile        = worst_profile,
      worst_prob_mean      = worst_prob_mean,
      subgroup_size        = length(S),
      search               = search,
      se_method            = "respondent-clustered, conditional on selected profile"
    ),
    call = match.call()
  )
}
