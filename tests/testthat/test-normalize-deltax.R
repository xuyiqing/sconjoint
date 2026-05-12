## Tests for the v0.2.1 `normalize_deltaX = TRUE` option on scfit().

test_that("normalize_deltaX = FALSE preserves no-op semantics", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  d <- sw2022[sw2022$respondent %in% unique(sw2022$respondent)[1:30], ]

  set.seed(1); torch::torch_manual_seed(1)
  f_off <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                   resp_female + age + pid,
                 data = d, respondent = "respondent", task = "task", profile = "profile",
                 K = 2L, n_epochs = 30L, seed = 1, stage2 = "none",
                 normalize_deltaX = FALSE)
  expect_equal(unname(f_off$sd_dx), rep(1, length(f_off$theta)))
  expect_false(isTRUE(f_off$normalize_deltaX))
})

test_that("default normalize_deltaX is TRUE (matches explicit TRUE)", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  d <- sw2022[sw2022$respondent %in% unique(sw2022$respondent)[1:30], ]

  set.seed(1); torch::torch_manual_seed(1)
  f_def <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                   resp_female + age + pid,
                 data = d, respondent = "respondent", task = "task", profile = "profile",
                 K = 2L, n_epochs = 30L, seed = 1, stage2 = "none")
  set.seed(1); torch::torch_manual_seed(1)
  f_on  <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                   resp_female + age + pid,
                 data = d, respondent = "respondent", task = "task", profile = "profile",
                 K = 2L, n_epochs = 30L, seed = 1, stage2 = "none",
                 normalize_deltaX = TRUE)
  expect_identical(f_def$theta, f_on$theta)
  expect_identical(f_def$vcov, f_on$vcov)
  expect_identical(f_def$beta_hat, f_on$beta_hat)
  expect_true(isTRUE(f_def$normalize_deltaX))
})

test_that("normalize_deltaX = TRUE stores sd_dx and flips the flag", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(br2017, package = "sconjoint")
  some_resp <- unique(br2017$respondent)[1:100]
  d <- br2017[br2017$respondent %in% some_resp, ]

  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ rate_L10 + rate_10_35 + rate_35_85 + rate_85_175 +
                 rate_175_375 + rate_375P + revenue_score |
                 resp_age + resp_female + resp_pid7,
               data = d, respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 30L, seed = 1, stage2 = "none",
               normalize_deltaX = TRUE)
  expect_true(isTRUE(fit$normalize_deltaX))
  expect_length(fit$sd_dx, length(fit$theta))
  ## sd_dx must reflect the empirical column SDs of the user-facing deltaX
  expect_equal(unname(fit$sd_dx),
               unname(apply(fit$deltaX, 2L, stats::sd)),
               tolerance = 1e-10)
  ## SDs for the rate columns should be on the order of 10-25
  expect_true(all(fit$sd_dx[grepl("rate_", names(fit$sd_dx))] > 5))
})

test_that("normalize_deltaX = TRUE: deltaX %*% theta reproduces linear index on original scale", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(br2017, package = "sconjoint")
  some_resp <- unique(br2017$respondent)[1:80]
  d <- br2017[br2017$respondent %in% some_resp, ]
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ rate_L10 + rate_10_35 + rate_35_85 + rate_85_175 +
                 rate_175_375 + rate_375P + revenue_score |
                 resp_age + resp_female + resp_pid7,
               data = d, respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 30L, seed = 1, stage2 = "none",
               normalize_deltaX = TRUE)
  ## Slot consistency: deltaX is on the user's original units
  expect_true(max(abs(apply(fit$deltaX, 2, sd) - fit$sd_dx)) < 1e-10)
  ## The product deltaX %*% theta should be on the logit scale
  ## (same magnitude as the prediction); validates that we
  ## un-standardized theta correctly.
  lin <- as.numeric(fit$deltaX %*% fit$theta)
  expect_true(is.finite(mean(lin)))
  expect_true(stats::sd(lin) < 5)  # sanity bound
})

test_that("normalize_deltaX = TRUE tames MAP per-respondent betas on BR-style data", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(br2017, package = "sconjoint")
  some_resp <- unique(br2017$respondent)[1:200]
  d <- br2017[br2017$respondent %in% some_resp, ]
  set.seed(1); torch::torch_manual_seed(1)
  fit_norm <- scfit(choice ~ rate_L10 + rate_10_35 + rate_35_85 + rate_85_175 +
                      rate_175_375 + rate_375P + revenue_score |
                      resp_age + resp_female + resp_pid7,
                    data = d, respondent = "respondent", task = "task", profile = "profile",
                    K = 2L, n_epochs = 50L, seed = 1, stage2 = "map_c5",
                    normalize_deltaX = TRUE)
  ## With standardization, MAP per-respondent betas should live in
  ## a sane range (|beta| < ~3 on a typical attribute).  Without
  ## standardization, they can drift to >1000 on BR-style data.
  max_abs <- max(abs(fit_norm$beta_hat))
  expect_true(max_abs < 50,
              info = sprintf("max(|beta_hat|) = %g with normalize_deltaX = TRUE", max_abs))
})
