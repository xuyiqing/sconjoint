## Tests for the respondent-cluster (wild) bootstrap on the
## distributional / threshold quantities (sc_polarization,
## sc_fraction_preferring) and the underlying engine.

## ---- Engine-level tests (no torch needed) --------------------------------

test_that(".sc_collapse_beta_to_resp reduces to one row per respondent", {
  ## beta is constant within respondent; collapse must pick that value.
  resp <- c(1L, 1L, 2L, 2L, 2L, 3L)
  B <- rbind(c(0.5, -0.2),
             c(0.5, -0.2),   # resp 1, repeated
             c(1.0,  0.3),
             c(1.0,  0.3),
             c(1.0,  0.3),   # resp 2, repeated
             c(-0.4, 0.8))   # resp 3
  out <- sconjoint:::.sc_collapse_beta_to_resp(B, resp)
  expect_equal(nrow(out$B_resp), 3L)
  expect_equal(out$resp, c(1L, 2L, 3L))
  expect_equal(out$B_resp, rbind(c(0.5, -0.2), c(1.0, 0.3), c(-0.4, 0.8)))
})

test_that(".sc_resp_cluster_boot: wild SE on a proportion ~ analytic clustered SE", {
  ## One indicator per respondent (M independent Bernoulli draws). The
  ## respondent-cluster bootstrap of the mean should reproduce the
  ## sqrt(p(1-p)/M) standard error of a proportion.
  set.seed(101)
  M <- 400L
  ind <- matrix(stats::rbinom(M, 1L, 0.35), ncol = 1L)
  p_hat <- mean(ind)
  analytic <- sqrt(p_hat * (1 - p_hat) / M)

  bw <- sconjoint:::.sc_resp_cluster_boot(ind, fun = function(m) m,
                                          n_boot = 2000L, boot_type = "wild",
                                          seed = 1L)
  bc <- sconjoint:::.sc_resp_cluster_boot(ind, fun = function(m) m,
                                          n_boot = 2000L, boot_type = "cluster",
                                          seed = 1L)
  expect_equal(bw$est, p_hat, tolerance = 1e-12)
  expect_true(is.finite(bw$se))
  ## both schemes land within ~12% (relative) of the analytic clustered SE
  expect_equal(bw$se, analytic, tolerance = 0.12)
  expect_equal(bc$se, analytic, tolerance = 0.12)
  ## percentile CI brackets the point estimate
  expect_true(bw$ci_lo <= p_hat && p_hat <= bw$ci_hi)
})

test_that(".sc_resp_cluster_boot does not disturb the caller's RNG stream", {
  set.seed(42)
  ind <- matrix(stats::rbinom(50L, 1L, 0.5), ncol = 1L)
  before <- .Random.seed                 # capture AFTER building `ind`
  invisible(sconjoint:::.sc_resp_cluster_boot(ind, fun = function(m) m,
                                              n_boot = 50L, seed = 7L))
  expect_identical(.Random.seed, before)
})

test_that(".sc_resp_cluster_boot propagates a nonlinear functional", {
  ## fun returns the polarization index 1 - |fp - fn| from c(fp, fn).
  set.seed(7)
  M <- 200L
  pos <- matrix(stats::rbinom(M, 1L, 0.5), ncol = 1L)
  neg <- 1 - pos                       # complementary directions
  G <- cbind(pos, neg)
  fun <- function(m) c(m[1], m[2], 1 - abs(m[1] - m[2]))
  bt <- sconjoint:::.sc_resp_cluster_boot(G, fun = fun, n_boot = 500L,
                                          boot_type = "wild", seed = 3L)
  expect_length(bt$est, 3L)
  ## index is near 1 (evenly split) with a finite, positive SE
  expect_gt(bt$est[3], 0.8)
  expect_true(is.finite(bt$se[3]) && bt$se[3] > 0)
  expect_true(bt$ci_lo[3] <= bt$est[3] && bt$est[3] <= bt$ci_hi[3])
})

## ---- Integration tests on a real fit (torch) ------------------------------

test_that("sc_fraction_preferring wild bootstrap: finite SEs, CI covers estimate", {
  fit <- .get_toy_fit()
  q <- sc_fraction_preferring(fit, threshold = 0,
                              se_method = "wild_bootstrap",
                              n_boot = 200L, boot_seed = 1L)
  est <- q$estimate
  expect_true(all(is.finite(est$se_positive)))
  expect_true(all(is.finite(est$se_negative)))
  expect_true(all(est$se_positive >= 0))
  ## percentile CI brackets the point estimate, every dummy
  expect_true(all(est$ci_lo_positive <= est$frac_positive + 1e-9 &
                  est$frac_positive <= est$ci_hi_positive + 1e-9))
  expect_true(all(est$ci_lo_negative <= est$frac_negative + 1e-9 &
                  est$frac_negative <= est$ci_hi_negative + 1e-9))
  expect_identical(q$details$se_method, "wild_bootstrap")
  expect_identical(q$details$n_boot, 200L)
})

test_that("sc_fraction_preferring wild SE ~ analytic clustered SE", {
  fit <- .get_toy_fit()
  qc <- sc_fraction_preferring(fit, threshold = 0, se_method = "clustered")
  qw <- sc_fraction_preferring(fit, threshold = 0, se_method = "wild_bootstrap",
                               n_boot = 1000L, boot_seed = 1L)
  ## On the same fit the two SE rules estimate the same clustered-proportion
  ## variance; they should agree to a few SE-units of bootstrap noise.
  nz <- qc$estimate$se_positive > 0
  expect_equal(qw$estimate$se_positive[nz], qc$estimate$se_positive[nz],
               tolerance = 0.20)
})

test_that("sc_polarization wild bootstrap gives finite index SE and covering CI", {
  fit <- .get_toy_fit()
  q0 <- sc_polarization(fit)                      # default: NA SEs
  expect_true(all(is.na(q0$estimate$se)))
  expect_identical(q0$details$se_method, "none")

  q <- sc_polarization(fit, se_method = "wild_bootstrap",
                       n_boot = 200L, boot_seed = 1L)
  est <- q$estimate
  expect_true(all(is.finite(est$se)))
  expect_true(all(est$se >= 0))
  expect_true(all(est$ci_lo <= est$polarization_idx + 1e-9 &
                  est$polarization_idx <= est$ci_hi + 1e-9))
  ## SE columns for the two underlying fractions are also populated
  expect_true(all(is.finite(est$se_positive)))
  expect_identical(q$details$se_method, "wild_bootstrap")
})

test_that("scfit() warns once when downgrading stage2 for a non-DNN learner", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  skip_if_not_installed("glmnet")
  data(simdata, package = "sconjoint")
  ## Reset the once-per-session gate so the warning is observable here.
  sc_state <- get(".sc_state", envir = asNamespace("sconjoint"))
  sc_state$warned <- setdiff(sc_state$warned, "stage2_downgrade")
  expect_warning(
    scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
          respondent = "respondent", task = "task", profile = "profile",
          learner = "enet", K = 2L, seed = 1L),
    regexp = "stage2.*learner = \"dnn\"|empirical-Bayes"
  )
  ## Second fit in the same session must NOT warn again.
  expect_no_warning(
    scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
          respondent = "respondent", task = "task", profile = "profile",
          learner = "enet", K = 2L, seed = 2L)
  )
})
