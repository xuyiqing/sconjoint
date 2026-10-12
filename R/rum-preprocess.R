# Fold-internal preprocessing and the "avipolb" fold trainer for the RUM network
# (Acharya memo of 2026-10-08, item 3: keep evaluation respondents outside training
# decisions). Internal helpers; nothing here is exported.
#
# Cross-fitting trains fold k's network on the other folds and evaluates it on fold k.
# Every quantity learned from data for fold k must come from fold k's training
# respondents only. Before this file, the application inputs standardized the
# covariates (and, for the Saha-Weeks 19-covariate set, filled missing values with
# medians) over all respondents, so each fold's network saw a transformation that used
# its evaluation respondents' covariates. Here:
#   .rum_preprocess_learn()  learns the imputation values, centers and scales from the
#                            training respondents' rows of the raw covariate matrix;
#   .rum_preprocess_apply()  applies a learned rule to any respondents (training,
#                            evaluation, or a new population bank);
#   .rum_dnn_polish()        moves (kappa, loadings, log omega) and, with bias = TRUE,
#                            the network's output bias to their maximum likelihood on
#                            the training respondents with the hidden layers fixed (the
#                            reproduction scripts' polish_common / polish_common_bias,
#                            rum_reproduction_2026-09-24/scripts_C/rule_b.R, ported
#                            verbatim, including their bounds);
#   .rum_dnn_fold_train()    runs the whole fold recipe (learn preprocessing, Avi's
#                            trainer, polish) reading only the training respondents.
#   .rum_dnn_fold_mu()       evaluates a fold fit's mean function on raw covariates,
#                            through that fold's own preprocessing.

# Learn a preprocessing rule from the training respondents' raw covariates.
# Z: numeric matrix, row names = respondent IDs, NA allowed. train_ids: character IDs.
# impute: "median" fills a missing value with the training median of its column;
# "none" refuses missing values. Centers and scales are the training mean and the
# root mean square deviation (divisor n, as Avi's .br_dnn_scale and the 09-24 inputs).
.rum_preprocess_learn <- function(Z, train_ids, impute = c("median", "none"),
                                  scale_tolerance = 1e-12) {
  impute <- match.arg(impute)
  if (!is.matrix(Z) || !is.numeric(Z) || is.null(rownames(Z)) || is.null(colnames(Z)))
    stop("Z must be a numeric matrix with respondent row names and column names.")
  if (anyDuplicated(rownames(Z)) || anyDuplicated(colnames(Z)))
    stop("Z row and column names must be unique.")
  if (!is.character(train_ids) || !length(train_ids) || anyNA(train_ids) ||
      anyDuplicated(train_ids) || any(!train_ids %in% rownames(Z)))
    stop("train_ids must be unique respondent IDs present in Z.")
  X <- Z[train_ids, , drop = FALSE]
  if (any(is.infinite(X))) stop("Z contains infinite values.")
  fill <- stats::setNames(rep(NA_real_, ncol(X)), colnames(X))
  missing <- colSums(is.na(X))
  if (any(missing > 0)) {
    if (impute == "none") stop("Missing covariate values and impute = \"none\".")
    if (any(missing == nrow(X))) stop("A covariate is missing for every training respondent: ",
                                       paste(colnames(X)[missing == nrow(X)], collapse = ", "))
  }
  if (impute == "median") {
    fill <- apply(X, 2L, stats::median, na.rm = TRUE)
    na <- which(is.na(X), arr.ind = TRUE)
    if (nrow(na)) X[na] <- fill[na[, "col"]]
  }
  center <- colMeans(X)
  scale <- sqrt(colMeans(sweep(X, 2L, center)^2))
  bad <- !is.finite(scale) | scale <= scale_tolerance * pmax(1, abs(center))
  if (any(bad)) stop("A covariate is constant among the training respondents: ",
                     paste(colnames(X)[bad], collapse = ", "))
  list(columns = colnames(Z), impute = impute, fill = fill, center = center, scale = scale,
       missing_in_training = missing, n_train = length(train_ids), divisor = "n",
       train_ids = train_ids)
}

# Apply a learned rule to respondents `ids` (default: all rows of Z).
.rum_preprocess_apply <- function(rule, Z, ids = rownames(Z)) {
  if (!is.matrix(Z) || !identical(colnames(Z), rule$columns))
    stop("Z must have the columns the rule was learned on, in the same order.")
  if (!is.character(ids) || any(!ids %in% rownames(Z))) stop("ids must be row names of Z.")
  X <- Z[ids, , drop = FALSE]
  na <- which(is.na(X), arr.ind = TRUE)
  if (nrow(na)) {
    if (rule$impute == "none") stop("Missing covariate values and the rule does not impute.")
    X[na] <- rule$fill[na[, "col"]]
  }
  X <- sweep(sweep(X, 2L, rule$center), 2L, rule$scale, "/")
  if (any(!is.finite(X))) stop("Preprocessed covariates are not finite.")
  X
}

# Maximize the training sequence likelihood over (kappa, loadings, log omega), and with
# bias = TRUE also the network's output bias, holding the hidden layers and output
# weights fixed. Z holds the PREPROCESSED covariates of data's respondents (row names =
# data$ids or a superset). Bounds come from the trainer control (kappa_bounds,
# loading_bounds, omega_bounds). Returns the polished final state and the training
# log-likelihood (mean per respondent) before and after.
.rum_dnn_polish <- function(data, Z, final, bias = TRUE,
                            quadrature = rum_quadrature(c(21L, 21L)), control = list()) {
  b <- .rum_dnn_train_control(control, 1L)
  p <- data$p; Zi <- Z[data$ids, , drop = FALSE]
  if (!bias) {
    mu <- rum_dnn_gaussian_forward(final$network, Zi)
    un <- function(x) list(kappa = x[1], loading = x[1 + seq_len(p)], omega = exp(x[p + 2]))
    ev <- function(x) { u <- un(x); rum_dnn_gaussian_evaluate(data, mu, u$kappa, u$loading, u$omega, quadrature) }
    fn <- function(x) -ev(x)$objective
    gr <- function(x) { e <- ev(x); g <- colMeans(e$common_score); g[p + 2] <- g[p + 2] * exp(x[p + 2]); -g }
    x0 <- c(final$kappa, final$loading, log(final$omega))
    lo <- c(b$kappa_bounds[1], rep(b$loading_bounds[1], p), log(b$omega_bounds[1])) * .999
    hi <- c(b$kappa_bounds[2], rep(b$loading_bounds[2], p), log(b$omega_bounds[2])) * .999
    x0 <- pmin(pmax(x0, lo), hi)
    o <- stats::optim(x0, fn, gr, method = "L-BFGS-B", lower = lo, upper = hi,
                      control = list(factr = 1e2, maxit = 2000))
    u <- un(o$par)
    out <- list(network = final$network, kappa = u$kappa, loading = u$loading, omega = u$omega)
  } else {
    net <- final$network; L <- length(net$biases)
    un <- function(x) { nt <- net; nt$biases[[L]] <- x[seq_len(p)]
      list(network = nt, kappa = x[p + 1], loading = x[p + 1 + seq_len(p)], omega = exp(x[2 * p + 2])) }
    cache <- new.env()
    ev <- function(x) {
      if (!is.null(cache$x) && identical(cache$x, x)) return(cache$v)
      u <- un(x); v <- rum_dnn_gaussian_loss(u$network, data, Zi, u$kappa, u$loading, u$omega, quadrature)
      cache$x <- x; cache$v <- v; v
    }
    fn <- function(x) ev(x)$loss
    gr <- function(x) { g <- ev(x)$loss_gradient; c(g$biases[[L]], g$kappa, g$loading, g$omega * exp(x[2 * p + 2])) }
    x0 <- c(net$biases[[L]], final$kappa, final$loading, log(final$omega))
    lo <- c(rep(-Inf, p), b$kappa_bounds[1] * .999, rep(b$loading_bounds[1] * .999, p), log(b$omega_bounds[1]))
    hi <- c(rep(Inf, p), b$kappa_bounds[2] * .999, rep(b$loading_bounds[2] * .999, p), log(b$omega_bounds[2]))
    x0 <- pmin(pmax(x0, lo), hi)
    o <- stats::optim(x0, fn, gr, method = "L-BFGS-B", lower = lo, upper = hi,
                      control = list(factr = 1e2, maxit = 3000))
    out <- un(o$par)
  }
  before <- -fn(x0); after <- -o$value
  if (after < before - 1e-10) stop("The polish lowered the training log-likelihood.")
  list(final = out, loglik_before = before, loglik_after = after, convergence = o$convergence,
       message = o$message, iterations = o$counts, bias = bias)
}

# One fold of the RUM network under fold-internal preprocessing. Reads only the rows of
# `data` and `Z` that belong to train_ids. validation_ids (a subset of train_ids) are the
# early-stopping respondents of Avi's trainer. trainers: any of "avi" (Avi's trainer as
# shipped), "avipol" (then the polish of kappa, loadings, omega) and "avipolb" (then the
# polish including the output bias); the three share one trained network.
.rum_dnn_fold_train <- function(data, Z, train_ids, validation_ids,
                                hidden = c(32L, 32L, 16L), seeds = c(42L, 7L, 1609L),
                                control = list(), impute = "median",
                                trainers = c("avi", "avipol", "avipolb"),
                                quadrature = rum_quadrature(c(21L, 21L))) {
  trainers <- match.arg(trainers, c("avi", "avipol", "avipolb"), several.ok = TRUE)
  if (any(!validation_ids %in% train_ids)) stop("validation_ids must be training respondents.")
  rule <- .rum_preprocess_learn(Z, train_ids, impute = impute)
  Ztr <- .rum_preprocess_apply(rule, Z, train_ids)
  train <- rum_subset(data, train_ids)
  t0 <- proc.time()[["elapsed"]]
  fit <- rum_dnn_gaussian_train(train, Ztr, validation_ids, hidden = hidden, seeds = seeds,
                                control = control)
  t_train <- proc.time()[["elapsed"]] - t0
  if (is.null(fit$final)) stop("Avi's trainer returned no final state: ", fit$status)
  finals <- list(avi = fit$final); polish <- list(); times <- c(train = t_train)
  for (tr in setdiff(trainers, "avi")) {
    t1 <- proc.time()[["elapsed"]]
    pol <- .rum_dnn_polish(train, Ztr, fit$final, bias = tr == "avipolb",
                           quadrature = quadrature, control = control)
    times[[tr]] <- proc.time()[["elapsed"]] - t1
    finals[[tr]] <- pol$final; pol$final <- NULL; polish[[tr]] <- pol
  }
  list(preprocess = rule, fit = fit, finals = finals[trainers], polish = polish,
       train_ids = train_ids, validation_ids = validation_ids, seconds = times)
}

# Mean function of a fold fit at raw covariates (through the fold's own preprocessing).
.rum_dnn_fold_mu <- function(fold_fit, Z, ids = rownames(Z), trainer = "avipolb") {
  rum_dnn_gaussian_forward(fold_fit$finals[[trainer]]$network,
                           .rum_preprocess_apply(fold_fit$preprocess, Z, ids))
}
