# Synthetic fold states for the sc_* tests: random data, fitted means, common parameters, scores from the
# evaluator, and random positive-definite held-out information (moved from test-sc-engine.R).
sc_test_states <- function(n = 30L, p = 3L, q = 1L, K = 2L, seed = 11L) {
  withr::with_seed(seed, {
    n_task <- 4L
    X <- matrix(sample(c(-1, 0, 1), n * n_task * p, TRUE), n * n_task, p, dimnames = list(NULL, paste0("f", seq_len(p))))
    y <- stats::rbinom(n * n_task, 1, .5); id <- rep(sprintf("i%02d", seq_len(n)), each = n_task)
    Z <- matrix(stats::rnorm(n * 2), n, 2)
    x <- sc_data(y, X, id, Z = Z, groups = data.frame(g = stats::rbinom(n, 1, .5)), fold = rep(seq_len(K), length.out = n))
    d <- x$data; n_th <- .rum_th_len(p, q); D <- p + n_th
    quad <- rum_quadrature(n_fac = q, rule = "sobol", nodes = 32L, seed = 3L)
    if (q == 1L) quad$u <- as.vector(quad$u)
    folds <- lapply(seq_len(K), function(k) {
      te <- which(x$fold == k)
      mu <- matrix(stats::rnorm(n * p, 0, .5), n, p, dimnames = list(d$ids, d$feature_names))
      L <- matrix(stats::rnorm(p * q, 0, .4), p, q)
      th <- list(kappa = .1, loading = if (q == 1L) drop(L) else .rum_canonical_loading(L), omega = .3)
      sub <- rum_subset(d, d$ids[te])
      ev <- rum_dnn_gaussian_evaluate(sub, unname(mu[te, , drop = FALSE]), th$kappa, th$loading, th$omega, quad)
      I <- array(0, c(length(te), D, D))
      for (i in seq_along(te)) { A <- matrix(stats::rnorm(D * D), D); I[i, , ] <- crossprod(A) / D + diag(D) }
      rot <- if (q > 1L) .rum_rotation_dirs(th$loading) else NULL
      parts <- .rum_weight_parts(function(i) I[i, , ], length(te), p, n_th, 1e-6, rot)
      list(k = k, te = te, mu = mu, th = th, output_score = unname(ev$output_score),
           common_score = unname(ev$common_score), parts = parts, I = I, diag = data.frame(fold = k), fit_sha = "test")
    })
    st <- .sc_new_states(x, q, folds, score = list(points = 16L, seed = 3L),
                         info = list(method = "synthetic", M = NA, ny = NA, nodes = NA, seed = NA, partial = FALSE))
  })
  st
}
