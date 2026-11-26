# sc_posterior() on fold states: each respondent is summarized at the fit of the fold that held it out.

test_that("sc_posterior() on fold states uses each respondent's held-out fold", {
  st <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
  dat <- st$data$data
  pick <- dat$ids[c(1, 2, dat$n - 1L, dat$n)]
  cc <- rbind(first = as.numeric(seq_len(dat$p) == 1L))
  pp <- sc_posterior(st, respondents = pick, contrasts = cc, nodes = 1024)
  expect_s3_class(pp, "sc_posterior")
  expect_identical(pp$diagnostics$id, pick)
  expect_identical(unique(pp$beta$id), pick)
  fold_of <- integer(dat$n)
  for (f in st$folds) fold_of[f$te] <- f$k
  expect_identical(pp$diagnostics$fold, fold_of[match(pick, dat$ids)])
  for (r in pick) {
    f <- st$folds[[as.character(fold_of[match(r, dat$ids)])]]
    one <- sc_posterior(list(mu = f$mu, kappa = f$th$kappa, loading = f$th$loading, omega = f$th$omega),
                        dat, respondents = r, contrasts = cc, nodes = 1024)
    expect_equal(pp$contrasts[pp$contrasts$id == r, "mean"], one$contrasts$mean, tolerance = 0)
    expect_equal(pp$beta[pp$beta$id == r, "sd"], one$beta$sd, tolerance = 0)
  }
})

test_that("sc_posterior() on fold states rejects data with other respondents", {
  st <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
  other <- rum_subset(st$data$data, st$data$data$ids[1:5])
  expect_error(sc_posterior(st, data = other), "same respondents")
})
