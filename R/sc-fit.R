# sc_fit() and sc_select_rank(): cross-fitted network training with the paper's recipe (paper-v4).
#
# Ported from the paper's fold-fit driver (analysis/2026-11-14_rum_qfactor/scripts/s3/s3_fit.R, the same
# calls as .rum_dnn_fold_train): for fold k, learn the covariate preprocessing on the training respondents
# (median imputation, centring and RMS scaling), train the network with early stopping on the fold's
# validation respondents (three starts; best start and epoch by validation loss; refit on all training
# respondents for that many epochs), then refine the output bias and the common parameters (kappa,
# loading through a radial map, log omega) by L-BFGS-B ("polish"). The held-out respondents are scored on
# a finer rule (the rank selection's criterion).

# The paper's training settings (Avi's frozen manifest, time caps lifted; fit_*_q*_f*.rds$control).
.sc_paper_control <- function()
  list(max_epochs = 200L, validation_every = 5L, patience = 10L, min_delta = 0, learning_rate = 0.01,
       beta1 = 0.9, beta2 = 0.999, epsilon = 1e-08, network_l2 = 1e-04, max_seconds = 1e+06,
       start_seconds = 3e+05, refit_seconds = 3e+05, evaluation_reserve = 1e+05, train_order = 15L,
       check_orders = c(15L, 21L), block_size = 128L, kappa_bounds = c(-5, 5), loading_bounds = c(-3, 3),
       omega_bounds = c(1e-06, 4), loglik_tolerance = 1e-05, score_tolerance = 1e-04,
       network_gradient_tolerance = 1e-04)

# Sobol rule with `nodes` nodes (nodes / 2 points mirrored in v).
.sc_sobol <- function(q, nodes, seed) rum_quadrature(n_fac = q, rule = "sobol", nodes = nodes %/% 2L, seed = seed)

# Network object from stored weights and biases (fits saved without the constructor's metadata).
.sc_network <- function(net, p_z, p, hidden) {
  if (inherits(net, "rum_dnn_gaussian")) return(net)
  out <- rum_dnn_gaussian(p_z, p, hidden, 1L)
  out$weights[] <- net$weights; out$biases[] <- net$biases
  out$seed <- NA_integer_; out$initialization <- "restored from a saved fit"
  out
}

# Constructor of an sc_fit (also used to wrap fits made outside sc_fit()). folds: list of fold fits, each
# list(k, te, tr, validation, preprocess, network, kappa, loading, omega, mu_all, heldout, selection,
# status, polish, seconds).
.sc_new_fit <- function(x, q, folds, settings) {
  names(folds) <- vapply(folds, function(f) as.character(f$k), "")
  structure(list(data = x, q = as.integer(q), folds = folds, settings = settings,
                 version = as.character(utils::packageVersion("sconjoint"))), class = "sc_fit")
}

#' Fit the structural random-utility model by cross-fitting
#'
#' Trains the model once per fold, each time on the respondents outside the
#' fold, with the paper's recipe. Respondents' mean tastes are a neural
#' network of their characteristics `Z`; tastes vary around that mean with `q`
#' taste factors; task shocks are logistic; each respondent has a persistent
#' preference for one screen side. For each fold the function learns the
#' covariate scaling on the training respondents, trains the network with
#' early stopping on the fold's validation respondents (one start per seed;
#' the best start and epoch are refit on all training respondents), refines
#' the common parameters, and scores the held-out respondents.
#'
#' Training is slow at the paper's settings (several minutes per fold). The
#' held-out scores feed [sc_select_rank()]; [sc_states()] turns a fit into
#' the fold states that [sc_estimate()] uses.
#'
#' @param x Data from [sc_data()], with covariates `Z`.
#' @param q Number of taste factors.
#' @param folds Which folds to fit (default all).
#' @param K Number of folds when `x` has none (drawn with `fold_seed`).
#' @param fold_seed Seed of the fold draw.
#' @param hidden Hidden-layer widths of the network.
#' @param seeds Initialization seeds, one start per seed.
#' @param control Training settings; entries override the paper's
#'   (`max_epochs = 200`, `patience = 10`, `learning_rate = 0.01`,
#'   `network_l2 = 1e-4`, ...).
#' @param nodes,check_nodes,heldout_nodes Sobol nodes of the training,
#'   checking and held-out scoring rules (2048, 4096, 8192).
#' @param quad_seed Seed of the Sobol rules.
#' @param validation_share,validation_seed When `x` has no validation
#'   respondents: the share of each fold's training respondents used for early
#'   stopping and the base seed of their draw (fold k uses `validation_seed + k`).
#' @param polish Refine the common parameters after training (default `TRUE`).
#' @param verbose Print one line per fold.
#' @return An object of class `sc_fit`.
#' @examples
#' \donttest{
#' toy <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(toy)) {
#'   f <- sc_fit(toy$data, q = 1, folds = 1, hidden = 4L, seeds = 1L,
#'               control = list(max_epochs = 10L), nodes = 64L, check_nodes = 128L,
#'               heldout_nodes = 128L)
#'   f
#' }
#' }
#' @export
sc_fit <- function(x, q = 1L, folds = NULL, K = 5L, fold_seed = 20261125L, hidden = c(32L, 32L, 16L),
                   seeds = c(42L, 7L, 1609L), control = list(), nodes = 2048L, check_nodes = 4096L,
                   heldout_nodes = 8192L, quad_seed = 20261114L, validation_share = 0.15,
                   validation_seed = 2026092400L, polish = TRUE, verbose = TRUE) {
  x <- .sc_check_data(x)
  if (is.null(x$Z)) stop("sc_fit() needs respondent covariates Z (see sc_data())", call. = FALSE)
  D <- x$data; Z <- x$Z; IDS <- D$ids; q <- as.integer(q)
  if (is.null(x$fold)) x$fold <- withr::with_seed(fold_seed, sample(rep(seq_len(K), length.out = D$n)))
  FOLD <- x$fold; ks <- sort(unique(FOLD)); if (!is.null(folds)) ks <- intersect(ks, folds)
  ctl <- utils::modifyList(.sc_paper_control(), control)
  QT <- .sc_sobol(q, nodes, quad_seed); QC <- .sc_sobol(q, check_nodes, quad_seed); Q8 <- .sc_sobol(q, heldout_nodes, quad_seed)
  out <- list()
  for (k in ks) {
    t0 <- proc.time()[["elapsed"]]
    te <- which(FOLD == k); tr <- which(FOLD != k)
    val_ids <- if (!is.null(x$validation) && length(x$validation) >= k && length(x$validation[[k]]))
      x$validation[[k]] else IDS[withr::with_seed(validation_seed + k, sort(sample(tr, floor(validation_share * length(tr)))))]
    val <- which(IDS %in% val_ids)
    if (!all(val %in% tr)) stop(sprintf("fold %d: validation respondents must be training respondents", k), call. = FALSE)
    rule_pp <- .rum_preprocess_learn(Z, IDS[tr], impute = "median")
    Ztr <- .rum_preprocess_apply(rule_pp, Z, IDS[tr]); train <- rum_subset(D, IDS[tr])
    fit <- rum_dnn_gaussian_train(train, Ztr, IDS[val], hidden = hidden, seeds = seeds, control = ctl, starts = NULL,
                                  n_fac = q, quadrature = list(train = QT, check = list(QC)))
    if (is.null(fit$final)) stop(sprintf("fold %d: training gave no final state (%s)", k, fit$status), call. = FALSE)
    t1 <- proc.time()[["elapsed"]]
    pol <- if (polish) .rum_dnn_polish(train, Ztr, fit$final, bias = TRUE, quadrature = QT, control = ctl) else list(final = fit$final)
    post <- pol$final
    mu_all <- rum_dnn_gaussian_forward(post$network, .rum_preprocess_apply(rule_pp, Z, IDS))
    dimnames(mu_all) <- list(IDS, D$feature_names)
    test <- rum_subset(D, IDS[te]); mu_te <- mu_all[te, , drop = FALSE]
    score <- function(rl) rum_dnn_gaussian_evaluate(test, unname(mu_te), post$kappa, post$loading, post$omega, rl)$loglik
    held <- list(sobol8192 = score(Q8), sobol2048 = score(QT))
    names(held) <- c(paste0("sobol", heldout_nodes), paste0("sobol", nodes))
    t2 <- proc.time()[["elapsed"]]
    out[[as.character(k)]] <- list(k = k, te = te, tr = tr, validation = IDS[val], preprocess = rule_pp,
      network = post$network, kappa = post$kappa, loading = post$loading, omega = post$omega, mu_all = mu_all,
      heldout = held, selection = fit$selection[c("start_index", "seed", "epoch", "validation_loss")],
      status = fit$status,
      polish = pol[intersect(names(pol), c("loglik_before", "loglik_after", "loglik_canonical", "convergence",
                                           "message", "iterations", "max_row_length"))],
      seconds = c(train = t1 - t0, polish = t2 - t1))
    if (verbose) message(sprintf("fold %d: held-out log score %.4f per respondent (seed %s, epoch %s; %.0f s)",
                                 k, mean(held[[1]]), fit$selection$seed, fit$selection$epoch, t2 - t0))
  }
  .sc_new_fit(x, q, out, list(hidden = as.integer(hidden), seeds = as.integer(seeds), control = ctl,
                              nodes = as.integer(nodes), check_nodes = as.integer(check_nodes),
                              heldout_nodes = as.integer(heldout_nodes), quad_seed = as.integer(quad_seed),
                              heldout_rule = paste0("sobol", heldout_nodes)))
}

# Cross-fitted held-out log score per respondent (NA for respondents of folds not fitted).
.sc_heldout <- function(fit, rule = NULL) {
  if (is.null(rule)) rule <- fit$settings$heldout_rule
  h <- rep(NA_real_, fit$data$data$n)
  for (f in fit$folds) h[f$te] <- unname(f$heldout[[rule]])
  h
}

#' @export
print.sc_fit <- function(x, ...) {
  d <- x$data$data
  cat(sprintf("sconjoint fit: q = %d taste factor(s), %d of %d folds, %d respondents, %d features\n",
              x$q, length(x$folds), length(unique(x$data$fold)), d$n, d$p))
  h <- .sc_heldout(x)
  if (any(is.finite(h))) cat(sprintf("  held-out log score per respondent: %.4f (%s)\n", mean(h, na.rm = TRUE), x$settings$heldout_rule))
  invisible(x)
}

#' @export
summary.sc_fit <- function(object, ...) {
  rows <- lapply(object$folds, function(f) {
    data.frame(fold = f$k, n_heldout = length(f$te), heldout = mean(f$heldout[[object$settings$heldout_rule]]),
               kappa = f$kappa, omega = f$omega,
               seed = if (is.null(f$selection$seed)) NA else f$selection$seed,
               epoch = if (is.null(f$selection$epoch)) NA else f$selection$epoch,
               status = if (is.null(f$status)) NA else f$status, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  structure(list(q = object$q, folds = out, n = object$data$data$n), class = "summary.sc_fit")
}

#' @export
print.summary.sc_fit <- function(x, ...) {
  cat(sprintf("sconjoint fit, q = %d, n = %d\n", x$q, x$n))
  print(x$folds, row.names = FALSE, digits = 4)
  invisible(x)
}

#' Choose the number of taste factors by held-out log score
#'
#' Applies the paper's rank rule to fits at successive numbers of taste
#' factors. Each respondent's held-out log score comes from the fold that did
#' not train on them. Starting from the smallest q, the rule moves from q - 1
#' to q only if the paired gain (mean over respondents of the score at q minus
#' the score at q - 1) is more than `threshold` standard errors, and stops at
#' the first q that does not move.
#'
#' @param fits A list of fits from [sc_fit()] at different q (all folds), or a
#'   list of numeric vectors of per-respondent held-out log scores named by q.
#'   Scores are paired by respondent: fits must cover the same respondents
#'   (matched by id), and vectors must have equal lengths (matched by name
#'   when named by respondent id, otherwise by position).
#' @param threshold The z threshold (default 2).
#' @return A data frame with one row per q: `q`, `n`, `mean_heldout`,
#'   `diff_to_prev`, `se_diff`, `z`, `moves`, `selected`.
#' @examples
#' set.seed(2)
#' h1 <- rnorm(200, -5); h2 <- h1 + rnorm(200, .05, .2); h3 <- h2 + rnorm(200, 0, .2)
#' sc_select_rank(list(`1` = h1, `2` = h2, `3` = h3))
#' @export
sc_select_rank <- function(fits, threshold = 2) {
  if (!is.list(fits) || !length(fits)) stop("fits must be a nonempty list", call. = FALSE)
  if (all(vapply(fits, inherits, TRUE, "sc_fit"))) {
    qs <- vapply(fits, `[[`, 0L, "q")
    H <- lapply(fits, function(f) stats::setNames(.sc_heldout(f), f$data$data$ids))
  } else {
    if (is.null(names(fits))) stop("name the score vectors by q", call. = FALSE)
    if (!all(vapply(fits, is.numeric, TRUE))) stop("fits must be fits from sc_fit() or numeric score vectors", call. = FALSE)
    qs <- as.integer(names(fits)); H <- fits
  }
  if (anyNA(qs) || anyDuplicated(qs)) stop("every fit needs a distinct number of factors q", call. = FALSE)
  # Pair the scores by respondent: equal lengths, and (when named, as fits always are) the same respondents,
  # aligned by id to the first fit's order.
  len <- lengths(H)
  if (length(unique(len)) != 1L) stop("the score vectors have different lengths (", paste(len, collapse = ", "),
                                      "): every fit must score the same respondents", call. = FALSE)
  nms <- lapply(H, names); named <- !vapply(nms, is.null, TRUE)
  if (any(named)) {
    if (!all(named)) stop("name every score vector by respondent, or none", call. = FALSE)
    ref <- nms[[1L]]
    if (anyNA(ref) || anyDuplicated(ref)) stop("respondent ids of the scores must be unique", call. = FALSE)
    for (i in seq_along(H)) {
      j <- match(ref, nms[[i]])
      if (anyNA(j) || anyDuplicated(nms[[i]])) stop("the fits do not score the same respondents", call. = FALSE)
      H[[i]] <- H[[i]][j]
    }
  }
  H <- lapply(H, function(h) unname(as.numeric(h)))
  o <- order(qs); qs <- qs[o]; H <- H[o]
  if (any(vapply(H, anyNA, TRUE))) stop("every fit needs held-out scores for all respondents (fit all folds)", call. = FALSE)
  sel <- qs[1L]; moving <- TRUE
  rows <- lapply(seq_along(qs), function(i) {
    h <- H[[i]]; n <- length(h)
    r <- data.frame(q = qs[i], n = n, mean_heldout = mean(h), diff_to_prev = NA_real_, se_diff = NA_real_,
                    z = NA_real_, moves = NA)
    if (i > 1L) {
      dl <- h - H[[i - 1L]]; r$diff_to_prev <- mean(dl); r$se_diff <- stats::sd(dl) / sqrt(n)
      r$z <- r$diff_to_prev / r$se_diff; r$moves <- r$z > threshold
    }
    r
  })
  tab <- do.call(rbind, rows)
  for (i in seq_along(qs)[-1L]) {
    if (moving && sel == qs[i - 1L] && isTRUE(tab$moves[i])) sel <- qs[i] else moving <- FALSE
  }
  tab$selected <- tab$q == sel
  tab
}
