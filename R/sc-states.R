# sc_states(): fold states for the held-out estimator (paper-v4).
#
# Ported from the paper's fold-state driver (analysis/2026-11-14_rum_qfactor/scripts/s3/s3_state.R): the
# fold's loading is put on its principal axes; every respondent's scores are computed at the fold's fit on
# the score rule (Sobol 4,096 nodes: 2,048 points mirrored in v, seed 20261114); the HELD-OUT respondents'
# own conditional information is estimated by Louis's identity with Rao-Blackwellization, each respondent's
# own design draws from the experiment's randomization law (GS: M = 300 designs, ny = 4 simulated
# responses, the party key from pid7; BR: M = 250, ny = 8), on the information rule (Sobol 2,048 nodes);
# the q(q - 1)/2 rotation directions are removed and the eigenvalues floored (1e-6 relative) before the
# weight parts are formed. Seeds: the fold's information seed is seed + 10 k (inside, + k again and per
# respondent, as .rum_cond_info_fold).

.sc_info_defaults <- function(design)
  switch(design, gs = list(M = 300L, ny = 4L), br = list(M = 250L, ny = 8L))

# Constructor (also used to wrap states made outside sc_states()).
.sc_new_states <- function(x, q, folds, score, info, fits = NULL) {
  names(folds) <- vapply(folds, function(f) as.character(f$k), "")
  structure(list(data = x, q = as.integer(q), folds = folds, score = score, info = info, fits = fits,
                 version = as.character(utils::packageVersion("sconjoint"))), class = "sc_states")
}

#' Fold states for estimation
#'
#' Turns a cross-fitted fit into the fold states that [sc_estimate()] uses.
#' For each fold it puts the taste factors on their principal axes, scores
#' every respondent at the fold's fit, and estimates each held-out
#' respondent's conditional information: the expected information of their
#' choices, averaged over task designs drawn from the experiment's
#' randomization law. That information sets the one-step correction.
#'
#' The design law must be known to the package: only the built-in designs
#' `"gs"` (Graham and Svolik 2020; the respondent's party, `pid7` in the data's
#' groups, codes the co-partisan feature) and `"br"` (Ballard-Rosa, Martin and
#' Scheve 2017) are supported. Set it with `sc_data(design = )`. Other
#' experiments need their own design generator. The information is slow (at
#' the paper's settings about 15 seconds per held-out respondent and fold at
#' q = 3); `respondents` limits it to the first held-out respondents of every
#' fold, for checks.
#'
#' @param fit A fit from [sc_fit()].
#' @param M,ny Design draws per respondent and simulated responses per design
#'   (default: the paper's, GS 300 and 4, BR 250 and 8).
#' @param info_nodes,score_nodes Sobol nodes of the information and score rules (2048, 4096).
#' @param seed Base seed (default 20261114).
#' @param eig_floor Relative eigenvalue floor of each respondent's information.
#' @param folds Which folds (default all fitted folds).
#' @param respondents If given, only the first `respondents` held-out
#'   respondents of each fold (for checks; the states are then not for estimation).
#' @return An object of class `sc_states`.
#' @examples
#' \donttest{
#' toy <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(toy)) {
#'   f <- sc_fit(toy$data, q = 1, folds = 1, hidden = 4L, seeds = 1L,
#'               control = list(max_epochs = 10L), nodes = 64L, check_nodes = 128L,
#'               heldout_nodes = 128L, verbose = FALSE)
#'   s <- sc_states(f, M = 5L, ny = 2L, info_nodes = 64L, score_nodes = 64L, respondents = 3L)
#' }
#' }
#' @export
sc_states <- function(fit, M = NULL, ny = NULL, info_nodes = 2048L, score_nodes = 4096L, seed = 20261114L,
                      eig_floor = 1e-6, folds = NULL, respondents = NULL) {
  if (!inherits(fit, "sc_fit")) stop("expected a fit from sc_fit()", call. = FALSE)
  x <- fit$data; d <- x$data; q <- fit$q; p <- d$p; n_th <- .rum_th_len(p, q); IDS <- d$ids
  design <- x$design
  if (is.null(design)) stop("sc_states() needs the experiment's design law: sc_data(design = 'gs' or 'br'). ",
                            "Only these built-in designs are supported.", call. = FALSE)
  .sc_check_design_features(d$feature_names, design)
  def <- .sc_info_defaults(design)
  if (is.null(M)) M <- def$M
  if (is.null(ny)) ny <- def$ny
  resp <- NULL
  if (design == "gs") {
    if (is.null(x$groups$pid7)) stop("the GS design needs pid7 in the data's groups", call. = FALSE)
    resp <- data.frame(pid7 = x$groups$pid7)
  }
  QS <- rum_quadrature(n_fac = q, rule = "sobol", nodes = score_nodes %/% 2L, seed = seed)
  QI <- rum_quadrature(n_fac = q, rule = "sobol", nodes = info_nodes %/% 2L, seed = seed)
  if (q == 1L) { QI$u <- as.vector(QI$u); QS$u <- as.vector(QS$u) }   # one-factor information takes u as a vector
  ks <- names(fit$folds); if (!is.null(folds)) ks <- intersect(ks, as.character(folds))
  out <- list(); fits <- list()
  for (kk in ks) {
    ft <- fit$folds[[kk]]; k <- ft$k
    th <- list(kappa = ft$kappa, loading = if (q > 1L) .rum_canonical_loading(ft$loading) else ft$loading, omega = ft$omega)
    mu <- ft$mu_all
    te <- ft$te; if (!is.null(respondents)) te <- te[seq_len(min(respondents, length(te)))]
    sub <- rum_subset(d, IDS[te])
    ev <- rum_dnn_gaussian_evaluate(sub, unname(mu[te, , drop = FALSE]), th$kappa, th$loading, th$omega, QS)
    t0 <- proc.time()[["elapsed"]]
    cond <- .rum_cond_options(design, resp = if (is.null(resp)) NULL else resp[te, , drop = FALSE], method = "louis",
                              M = M, ny = ny, seed = seed + 10L * k, draws = "own", quad = QI)
    CI <- .rum_cond_info_fold(sub, unname(mu[te, , drop = FALSE]), th, k, cond, NULL)
    rot <- if (q > 1L) .rum_rotation_dirs(th$loading) else NULL
    Ih <- CI$I
    W <- .rum_weight_parts(function(i) Ih[i, , ], length(te), p, n_th, eig_floor, rot)
    diag <- data.frame(fold = k, n_heldout = length(te), M = M, ny = ny, info_nodes = length(QI$weight),
                       score_nodes = length(QS$weight), seconds_info = proc.time()[["elapsed"]] - t0,
                       floored = W$floored, omega = th$omega, kappa = th$kappa,
                       heldout_loglik_score_rule = mean(ev$loglik))
    out[[kk]] <- list(k = k, te = te, mu = mu, th = th, output_score = unname(ev$output_score),
                      common_score = unname(ev$common_score), parts = W, diag = diag, sub = sub,
                      fit_sha = NA_character_)
    fits[[kk]] <- ft[c("k", "network", "preprocess", "heldout", "validation", "loading")]
  }
  .sc_new_states(x, q, out, score = list(points = as.integer(score_nodes %/% 2L), seed = as.integer(seed)),
                 info = list(method = "louis", M = as.integer(M), ny = as.integer(ny), nodes = as.integer(info_nodes),
                             seed = as.integer(seed), eig_floor = eig_floor, partial = !is.null(respondents)),
                 fits = list(folds = fits, settings = fit$settings))
}

#' @export
print.sc_states <- function(x, ...) {
  d <- x$data$data
  cat(sprintf("sconjoint fold states: q = %d, %d folds, %d respondents, %d features\n",
              x$q, length(x$folds), d$n, d$p))
  cat(sprintf("  information: %s, M = %s, ny = %s, %s nodes; score rule %d nodes\n", x$info$method, x$info$M,
              x$info$ny, x$info$nodes, 2L * x$score$points))
  if (isTRUE(x$info$partial)) cat("  partial states (respondents limited): for checks only\n")
  invisible(x)
}
