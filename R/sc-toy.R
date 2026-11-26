# A small simulated data set under the Ballard-Rosa et al. (2017) design law, for examples and tests.
# Tasks are drawn from the experiment's randomization (8 tasks per respondent, 16 coded features); two
# covariates move the mean tastes; one or more taste factors; logistic task shocks; a side preference.
.sc_toy_data <- function(n = 40L, q = 1L, K = 2L, seed = 1L) {
  withr::with_seed(seed, {
    d <- .rum_draw_designs("br", M = n, seed = seed + 1L)          # n x 8 x 16
    p <- dim(d)[3L]; T <- dim(d)[2L]
    Z <- cbind(z1 = stats::rnorm(n), z2 = stats::rbinom(n, 1, .5))
    A <- matrix(stats::rnorm(2L * p, 0, .3), 2L, p); b0 <- stats::rnorm(p, 0, .5)
    L <- matrix(stats::rnorm(p * q, 0, .3), p, q)
    mu <- sweep(Z %*% A, 2L, b0, "+")
    beta <- mu + t(L %*% matrix(stats::rnorm(q * n), q, n))
    side <- .1 + sqrt(.3) * stats::rnorm(n)
    X <- do.call(rbind, lapply(seq_len(n), function(i) d[i, , ]))
    eta <- rowSums(X * beta[rep(seq_len(n), each = T), , drop = FALSE]) + rep(side, each = T)
    y <- stats::rbinom(n * T, 1, stats::plogis(eta))
    colnames(X) <- .RUM_BR_RECIPE$features
    id <- sprintf("r%03d", rep(seq_len(n), each = T))
    groups <- data.frame(pid7 = sample(1:7, n, TRUE))
    groups$party <- ifelse(groups$pid7 <= 3, "Dem", ifelse(groups$pid7 >= 5, "Rep", "Ind"))
    fold <- sample(rep(seq_len(K), length.out = n))
  })
  rownames(Z) <- unique(id)
  sc_data(y, X, id, Z = Z, groups = groups, fold = fold, design = "br",
          orientation = "y = 1 selects plan A (simulated); X = plan A minus plan B")
}

# A toy fit and its fold states at small settings (seconds, not minutes).
.sc_toy_states <- function(n = 40L, q = 1L, K = 2L, seed = 1L) {
  x <- .sc_toy_data(n, q, K, seed)
  f <- sc_fit(x, q = q, hidden = 4L, seeds = c(1L, 2L), control = list(max_epochs = 20L, patience = 2L),
              nodes = 128L, check_nodes = 256L, heldout_nodes = 256L, verbose = FALSE)
  sc_states(f, M = 8L, ny = 2L, info_nodes = 128L, score_nodes = 256L)
}
