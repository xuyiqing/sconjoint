## Tests for scmix_signshare (estimand-memo P8): contrast sign shares
## with the completed loading correction and the three reporting gates.

skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())
skip_on_cran()

fixed <- .fit_mixed_fixture()
fit <- fixed$fit

test_that("coordinate contrasts reproduce scmix_polarization exactly (P8)", {
  pol <- suppressWarnings(
    scmix_polarization(fit, n_bins = 15L, M = 200L, seed = 2L))
  ss <- suppressWarnings(
    scmix_signshare(fit, diag(2L), t_min = 0,
                    n_bins = 15L, M = 200L, seed = 2L))
  expect_equal(unname(ss$estimate), unname(pol$estimate), tolerance = 1e-10)
  expect_equal(unname(ss$se), unname(pol$se), tolerance = 1e-10)
})

test_that("psi for a sum contrast matches a hand re-derivation (P8)", {
  d <- c(1, 1)
  ss <- suppressWarnings(
    scmix_signshare(fit, d, t_min = 0, n_bins = 10L, M = 200L, seed = 2L))

  cf <- sconjoint:::.scmix_canon(fit)
  pr <- sconjoint:::.scmix_prep(cf, n_bins = 10L, M = 200L, seed = 2L)
  q <- ncol(cf$A_folds[[1L]])
  sd_dx <- sconjoint:::.scmix_sd_dx(cf)
  sig_f <- vapply(cf$A_folds, function(A)
    sqrt(max(sum(crossprod(A, d)^2), 1e-12)), numeric(1L))
  sig_flr <- pmax(sig_f, 0.05 * sqrt(sum((d / sd_dx)^2)))
  fold_resp <- pr$sc$fold_resp
  s_i <- sig_flr[fold_resp]
  z <- as.numeric(pr$mu_resp %*% d) / s_i
  H <- pnorm(z)
  a_rows <- (dnorm(z) / s_i) %o% d
  dA_rows <- matrix(0, pr$N, pr$pq)
  for (i in seq_len(pr$N)) {
    v_f <- as.numeric(crossprod(cf$A_folds[[fold_resp[i]]], d))
    dA_rows[i, ] <- -dnorm(z[i]) * z[i] * as.numeric(outer(d, v_f)) /
      s_i[i]^2
  }
  man <- H + rowSums(a_rows * pr$C) -
    sconjoint:::.scmix_A_adjust(pr, a_rows, dA_rows = dA_rows)

  expect_equal(unname(ss$estimate), mean(man), tolerance = 1e-10)
  expect_equal(unname(ss$se), sd(man) / sqrt(length(man)), tolerance = 1e-10)
})

test_that("batch contrasts equal single calls and carry labels (P8)", {
  Dm <- rbind(a = c(1, 0), b = c(0, 1), s = c(1, 1))
  batch <- suppressWarnings(
    scmix_signshare(fit, Dm, t_min = 0, n_bins = 10L, M = 200L, seed = 2L))
  expect_named(batch$estimate, c("a", "b", "s"))
  for (j in 1:3) {
    single <- suppressWarnings(
      scmix_signshare(fit, Dm[j, ], t_min = 0, n_bins = 10L, M = 200L,
                      seed = 2L))
    expect_equal(unname(batch$estimate[j]), unname(single$estimate),
                 tolerance = 1e-10)
    expect_equal(unname(batch$se[j]), unname(single$se), tolerance = 1e-10)
  }
  lst <- suppressWarnings(
    scmix_signshare(fit, list(first = c(a1yes = 1)), t_min = 0,
                    n_bins = 10L, M = 200L, seed = 2L))
  expect_named(lst$estimate, "first")
})

test_that("contrast parsing expands names and rejects bad input (P8)", {
  pc <- sconjoint:::.scmix_parse_contrasts(fit, c(a1yes = 1))
  expect_equal(pc$D[1, ], c(1, 0))
  expect_equal(pc$labels, "a1yes")
  pc2 <- sconjoint:::.scmix_parse_contrasts(fit, c(a2yes = 2, a1yes = 1))
  expect_equal(pc2$D[1, ], c(1, 2))
  expect_match(pc2$labels, "2\\*a2yes")
  expect_error(sconjoint:::.scmix_parse_contrasts(fit, c(nope = 1)),
               "unknown contrast names")
  expect_error(sconjoint:::.scmix_parse_contrasts(fit, c(1, 2, 3)),
               "length")
  expect_error(sconjoint:::.scmix_parse_contrasts(fit, c(0, 0)),
               "all zeros")
  expect_error(sconjoint:::.scmix_parse_contrasts(fit,
                                                  matrix(1, 2, 5)),
               "columns")
})

test_that("the floor gate reports NA with a warning (P8)", {
  expect_warning(
    ss <- scmix_signshare(fit, c(1, 1), sd_floor = 10, t_min = 0,
                          n_bins = 10L, M = 200L, seed = 2L),
    "reported NA")
  expect_true(is.na(ss$estimate))
  expect_true(is.na(ss$se))
  expect_true(is.na(ss$ci_lower))
  expect_length(ss$extra$gated, 1L)
  expect_false(ss$extra$gates$reported)
  expect_true(ss$extra$gates$gate_floor)
})

test_that("the rank gate fires at an absurd t_min and matches a hand t (P8)", {
  expect_warning(
    ss <- scmix_signshare(fit, c(1, 1), t_min = 1e6,
                          n_bins = 10L, M = 200L, seed = 2L),
    "reported NA")
  expect_true(is.na(ss$estimate))
  expect_true(ss$extra$gates$gate_rank)
  expect_true(is.finite(ss$extra$gates$t_het))

  cf <- sconjoint:::.scmix_canon(fit)
  pr <- sconjoint:::.scmix_prep(cf, n_bins = 10L, M = 200L, seed = 2L)
  d <- c(1, 1)
  A_bar <- Reduce(`+`, cf$A_folds) / length(cf$A_folds)
  v_bar <- as.numeric(crossprod(A_bar, d))
  g_raw <- 2 * as.numeric(outer(d, v_bar))
  sig2 <- as.numeric(t(d) %*% tcrossprod(A_bar) %*% d)
  se2 <- as.numeric(t(g_raw) %*% pr$I_AAeff_inv %*% g_raw) / pr$N
  expect_equal(ss$extra$gates$t_het, sig2 / sqrt(se2), tolerance = 1e-10)
})

test_that("the projection gate mass is computed on synthetic eigen input (P8)", {
  ## 2 coords, q = 1: pq = 2. One kept direction e1, one truncated e2.
  eigA <- list(vectors = diag(2), values = c(1, 0),
               keep = c(TRUE, FALSE))
  g_all_out <- c(0, 1)   # all gradient mass in the truncated direction
  g_all_in <- c(1, 0)
  out1 <- sconjoint:::.sc_signshare_gates(
    sig2_bar = 1, sig_by_fold = 1, norm_d_std = 1, g_raw = g_all_out,
    sd_dx = c(1, 1), q = 1L, I_AAeff_inv = diag(2), eigA = eigA,
    N = 100L, sd_floor = 0.05, t_min = 2)
  expect_equal(out1$proj_mass, 1)
  expect_true(out1$proj)
  out2 <- sconjoint:::.sc_signshare_gates(
    sig2_bar = 1, sig_by_fold = 1, norm_d_std = 1, g_raw = g_all_in,
    sd_dx = c(1, 1), q = 1L, I_AAeff_inv = diag(2), eigA = eigA,
    N = 100L, sd_floor = 0.05, t_min = 2)
  expect_equal(out2$proj_mass, 0)
  expect_false(out2$proj)
})

test_that("subgroup signshare re-averages the full-sample psi (P8)", {
  resp_f <- factor(fit$respondent_id, levels = unique(fit$respondent_id))
  first <- !duplicated(as.integer(resp_f))
  g <- factor(ifelse(fit$Z[first, "z1"] > 0, "hi", "lo"))

  full <- suppressWarnings(
    scmix_signshare(fit, c(1, 1), t_min = 0, n_bins = 10L, M = 200L,
                    seed = 2L))
  sub <- suppressWarnings(
    scmix_signshare(fit, c(1, 1), by = g, t_min = 0, n_bins = 10L,
                    M = 200L, seed = 2L))
  for (lev in levels(g)) {
    rows <- which(g == lev)
    lab_j <- grep(paste0("^", lev, ": "), names(sub$estimate))
    expect_equal(unname(sub$estimate[lab_j]),
                 mean(full$psi[rows, 1L]), tolerance = 1e-10)
    expect_equal(unname(sub$se[lab_j]),
                 sd(full$psi[rows, 1L]) / sqrt(length(rows)),
                 tolerance = 1e-10)
  }
})

test_that("sign shares are invariant to contrast rescaling (P8)", {
  d <- c(1, 1)
  s1 <- suppressWarnings(
    scmix_signshare(fit, d, n_bins = 10L, M = 200L, seed = 2L))
  s2 <- suppressWarnings(
    scmix_signshare(fit, 10 * d, n_bins = 10L, M = 200L, seed = 2L))
  expect_equal(unname(s1$estimate), unname(s2$estimate), tolerance = 1e-10)
  expect_equal(unname(s1$se), unname(s2$se), tolerance = 1e-10)
  expect_equal(s1$extra$gates$reported, s2$extra$gates$reported)
  expect_equal(s1$extra$gates$t_het, s2$extra$gates$t_het, tolerance = 1e-8)
})
