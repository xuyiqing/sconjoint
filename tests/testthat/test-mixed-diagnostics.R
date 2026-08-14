## Tests for the shared scmix cores extracted from the information /
## design-check / subgroup paths, and (later in the file) the T3
## linearity diagnostic and the T5 hypothesized-value design precheck.

skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())
skip_on_cran()

fixed <- .fit_mixed_fixture()
fit <- fixed$fit

test_that(".scmix_eff_info_bar matches the hand-written accumulation", {
  info <- sconjoint:::.scmix_information(fit, n_bins = 10L, M = 100L,
                                         seed = 3L)
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  N <- sum(!duplicated(as.integer(resp_f)))
  eff <- sconjoint:::.scmix_eff_info_bar(info, N)

  pq <- ncol(info$I_AA[[1L]])
  man <- matrix(0, pq, pq)
  for (i in seq_len(N)) {
    b <- info$bin_of[i]
    B_b <- info$I_inv[[b]] %*% info$I_muA[[b]]
    man <- man + (info$I_AA[[b]] - crossprod(info$I_muA[[b]], B_b)) / N
  }
  expect_equal(eff$I_AAeff_bar, man, tolerance = 1e-12)
  expect_length(eff$B_bin, length(info$I_inv))
  expect_equal(eff$B_bin[[1L]], info$I_inv[[1L]] %*% info$I_muA[[1L]],
               tolerance = 1e-12)
})

test_that("scmix_design_check equals a manual pass through the cores", {
  chk <- scmix_design_check(fit, n_bins = 10L, M = 100L, seed = 3L)

  cf <- sconjoint:::.scmix_canon(fit)
  sc <- sconjoint:::.scmix_scores(cf)
  info <- sconjoint:::.scmix_information(cf, n_bins = 10L, M = 100L,
                                         seed = 3L)
  N <- length(sc$loglik)
  p <- ncol(cf$deltaX)
  q <- ncol(sc$S_A) / p
  eff <- sconjoint:::.scmix_eff_info_bar(info, N)
  sdx <- sconjoint:::.scmix_sd_dx(cf)
  pq <- p * q
  D_A <- diag(rep(1 / sdx, q), pq)
  I_std <- D_A %*% eff$I_AAeff_bar %*% D_A
  eA <- eigen(I_std, symmetric = TRUE)
  I_std_inv <- eA$vectors %*% diag(1 / pmax(eA$values, 1e-12), pq) %*%
    t(eA$vectors)
  se_std <- sqrt(pmax(diag(I_std_inv), 0) / N)
  A_bar <- Reduce(`+`, cf$A_folds) / length(cf$A_folds)
  A_std <- as.numeric(A_bar * sdx)

  expect_equal(chk$spectrum, eA$values / max(eA$values, 1e-12),
               tolerance = 1e-12)
  expect_equal(chk$loadings$se_std, se_std, tolerance = 1e-12)
  expect_equal(chk$loadings$loading_std, A_std, tolerance = 1e-12)
  expect_equal(chk$loadings$t, abs(A_std) / se_std, tolerance = 1e-12)
  expect_identical(chk$source, "fit")
  expect_output(print(chk), "design-rank check")
})

test_that(".scmix_information is seed-deterministic through the core", {
  i1 <- sconjoint:::.scmix_information(fit, n_bins = 10L, M = 100L, seed = 3L)
  i2 <- sconjoint:::.scmix_information(fit, n_bins = 10L, M = 100L, seed = 3L)
  expect_identical(i1$bin_of, i2$bin_of)
  expect_equal(i1$I_inv[[1L]], i2$I_inv[[1L]], tolerance = 1e-15)
  expect_equal(i1$I_AA[[1L]], i2$I_AA[[1L]], tolerance = 1e-15)
})

test_that(".scmix_prep returns the extended inference pieces", {
  pr <- sconjoint:::.scmix_prep(fit, n_bins = 10L, M = 100L, seed = 3L)
  expect_true(all(c("I_AAeff_inv", "sd_dxA", "eigA") %in% names(pr)))
  expect_equal(dim(pr$I_AAeff_inv), c(pr$pq, pr$pq))
  expect_length(pr$sd_dxA, pr$p)
  expect_type(pr$eigA$keep, "logical")
  expect_length(pr$eigA$keep, pr$pq)
  expect_equal(dim(pr$IF_A), c(pr$N, pr$pq))
})
