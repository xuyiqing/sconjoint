## Tests for scmix_importance (estimand-memo P9): the model-implied
## importance decomposition from (mu, A) with simplex-Jacobian SEs.

skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())
skip_on_cran()

fixed <- .fit_mixed_fixture()
fit <- fixed$fit

test_that("S-block builder encodes the four weightings (P9)", {
  am <- fit$attr_map
  dX <- fit$deltaX
  Su <- sconjoint:::.sc_importance_S_blocks(am, dX, "uniform", NULL, "t")
  ## fixture attributes are single-dummy (2 levels): S = 1/2 - 1/4 = 1/4
  expect_equal(Su[[1L]], matrix(0.25, 1, 1))

  lv <- list(mean((c(1, 3, 5) - 3)^2))
  names(lv) <- names(am)[1L]
  Sl <- sconjoint:::.sc_importance_S_blocks(
    am, dX, "levels", stats::setNames(list(c(1, 3, 5)), names(am)[1L]), "t")
  expect_equal(Sl[[1L]], matrix(mean((c(1, 3, 5) - 3)^2), 1, 1))
  expect_equal(Sl[[2L]], matrix(0.25, 1, 1))   # fallback to uniform

  Sd <- sconjoint:::.sc_importance_S_blocks(am, dX, "design_variance",
                                            NULL, "t")
  expect_equal(Sd[[1L]], stats::cov(dX[, am[[1L]], drop = FALSE]))

  Se <- sconjoint:::.sc_importance_S_blocks(am, dX, "empirical", NULL, "t")
  w <- colMeans(abs(dX[, am[[1L]], drop = FALSE]))
  tot <- max(1 - sum(w), 0) + sum(w)
  wn <- w / tot
  expect_equal(Se[[1L]], diag(wn, 1) - wn %o% wn)

  expect_error(
    sconjoint:::.sc_importance_S_blocks(am, dX, "levels",
                                        list(nope = c(1, 2)), "t"),
    "unknown attribute")
  expect_warning(
    sconjoint:::.sc_importance_S_blocks(
      am, dX, "uniform", stats::setNames(list(c(1, 2)), names(am)[1L]), "t"),
    "ignored")
})

test_that("numerator signals match a hand re-derivation (P9)", {
  out <- scmix_importance(fit, n_bins = 10L, M = 200L, seed = 2L)
  cf <- sconjoint:::.scmix_canon(fit)
  pr <- sconjoint:::.scmix_prep(cf, n_bins = 10L, M = 200L, seed = 2L)
  am <- cf$attr_map
  S_blocks <- sconjoint:::.sc_importance_S_blocks(am, cf$deltaX, "uniform",
                                                  NULL, "t")
  fold_resp <- pr$sc$fold_resp
  q <- ncol(cf$A_folds[[1L]])

  psiN_man <- matrix(0, pr$N, length(am))
  for (a in seq_along(am)) {
    cols <- am[[a]]
    S_g <- S_blocks[[a]]
    mu_g <- pr$mu_resp[, cols, drop = FALSE]
    h_mu <- rowSums((mu_g %*% S_g) * mu_g)
    h_res <- vapply(seq_len(pr$N), function(i) {
      A_g <- cf$A_folds[[fold_resp[i]]][cols, , drop = FALSE]
      sum(S_g * tcrossprod(A_g))
    }, numeric(1L))
    a_rows <- matrix(0, pr$N, pr$p)
    a_rows[, cols] <- 2 * (mu_g %*% S_g)
    dA_rows <- matrix(0, pr$N, pr$pq)
    for (i in seq_len(pr$N)) {
      A_g <- cf$A_folds[[fold_resp[i]]][cols, , drop = FALSE]
      SA <- 2 * (S_g %*% A_g)
      for (r in seq_len(q)) {
        dA_rows[i, (r - 1L) * pr$p + cols] <- SA[, r]
      }
    }
    psiN_man[, a] <- h_mu + h_res + rowSums(a_rows * pr$C) -
      sconjoint:::.scmix_A_adjust(pr, a_rows, dA_rows = dA_rows)
  }
  expect_equal(out$extra$numerator_signals, psiN_man, tolerance = 1e-10)
  expect_equal(unname(out$extra$numerators), colMeans(psiN_man),
               tolerance = 1e-12)
})

test_that("shares sum to one with simplex-Jacobian SEs (P9)", {
  out <- scmix_importance(fit, n_bins = 10L, M = 200L, seed = 2L)
  expect_equal(sum(out$estimate), 1, tolerance = 1e-12)

  psiN <- out$extra$numerator_signals
  N_hat <- colMeans(psiN)
  D <- sum(N_hat)
  s <- N_hat / D
  K <- length(s)
  J <- (diag(K) - matrix(s, K, K, byrow = TRUE)) / D
  V_N <- stats::var(psiN) / nrow(psiN)
  se_man <- sqrt(pmax(diag(J %*% V_N %*% t(J)), 0))
  expect_equal(unname(out$estimate), unname(s), tolerance = 1e-12)
  expect_equal(unname(out$se), se_man, tolerance = 1e-10)
})

test_that("the between-Z / residual split adds up (P9)", {
  out <- scmix_importance(fit, n_bins = 10L, M = 200L, seed = 2L)
  ## numerator = between_Z + residual + correction terms; the h-part
  ## split must reconstruct the uncorrected mean exactly
  cf <- sconjoint:::.scmix_canon(fit)
  pr <- sconjoint:::.scmix_prep(cf, n_bins = 10L, M = 200L, seed = 2L)
  fold_resp <- pr$sc$fold_resp
  resid_man <- vapply(seq_along(cf$attr_map), function(a) {
    cols <- cf$attr_map[[a]]
    S_g <- sconjoint:::.sc_importance_S_blocks(cf$attr_map, cf$deltaX,
                                               "uniform", NULL, "t")[[a]]
    mean(vapply(seq_len(pr$N), function(i) {
      A_g <- cf$A_folds[[fold_resp[i]]][cols, , drop = FALSE]
      sum(S_g * tcrossprod(A_g))
    }, numeric(1L)))
  }, numeric(1L))
  ## residual is respondent-weighted so between_Z + residual equals the
  ## uncorrected numerator mean
  expect_equal(unname(out$extra$residual), unname(resid_man),
               tolerance = 1e-12)
  expect_equal(length(out$extra$between_Z), length(cf$attr_map))
  expect_true(all(out$extra$residual > 0))
})

test_that("subgroup importance renormalizes within group (P9)", {
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  g <- factor(ifelse(fit$Z[first, "z1"] > 0, "hi", "lo"))
  out <- scmix_importance(fit, by = g, n_bins = 10L, M = 200L, seed = 2L)
  for (lev in levels(g)) {
    idx <- grep(paste0("^", lev, ": "), names(out$estimate))
    expect_equal(sum(out$estimate[idx]), 1, tolerance = 1e-12)
  }
  ## group shares equal a from-scratch simplex pass on the group rows
  full <- scmix_importance(fit, n_bins = 10L, M = 200L, seed = 2L)
  psiN <- full$extra$numerator_signals
  rows <- which(g == "hi")
  sh <- sconjoint:::.sc_simplex_share_psi(psiN[rows, , drop = FALSE],
                                          c("x", "y"), "t")
  idx <- grep("^hi: ", names(out$estimate))
  expect_equal(unname(out$estimate[idx]), unname(sh$share),
               tolerance = 1e-12)
})

test_that("the simplex helper warns on negative numerators (P9)", {
  set.seed(7)
  psiN <- cbind(rnorm(50, mean = -1), rnorm(50, mean = 3))
  w <- capture_warnings(
    sconjoint:::.sc_simplex_share_psi(psiN, c("bad", "good"), "t"))
  expect_true(any(grepl("non-positive importance numerator", w)))
  expect_true(any(grepl("outside \\[0, 1\\]", w)))
})
