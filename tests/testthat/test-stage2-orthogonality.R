## Tests that the DML θ̂ and Vcov are bit-exactly invariant across
## Stage-2 choices on the same `seed`.  This is the orthogonality
## property in the paper: the DML correction uses only the Stage-1
## single-DNN beta, never the MAP / BLUP refinement, so changing the
## Stage-2 method must not move θ̂ or Vcov.

scfit_small <- function(stage2 = "none", seed = 1L) {
  ## Small fit on sw2022 used by all tests below; same fixture for fairness.
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022_demo$respondent)[1:40]
  d <- sw2022_demo[sw2022_demo$respondent %in% some_resp, ]
  set.seed(seed)
  torch::torch_manual_seed(seed)
  scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
          resp_female + age + pid,
        data = d,
        respondent = "respondent", task = "task", profile = "profile",
        K = 2L, n_epochs = 40L, seed = seed,
        stage2 = stage2)
}

test_that("theta and vcov are identical between stage2='none' and 'map_c5'", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_none <- scfit_small(stage2 = "none",   seed = 1L)
  fit_map  <- scfit_small(stage2 = "map_c5", seed = 1L)
  expect_identical(fit_none$theta, fit_map$theta)
  expect_identical(fit_none$vcov,  fit_map$vcov)
  expect_identical(fit_none$influence_raw, fit_map$influence_raw)
})

test_that("stage2='varref' also leaves theta and vcov untouched", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_none   <- scfit_small(stage2 = "none",   seed = 1L)
  fit_varref <- scfit_small(stage2 = "varref", seed = 1L)
  expect_identical(fit_none$theta, fit_varref$theta)
  expect_identical(fit_none$vcov,  fit_varref$vcov)
})

test_that("stage2='map_c5' produces visibly different beta_hat vs DNN-only", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_none <- scfit_small(stage2 = "none",   seed = 1L)
  fit_map  <- scfit_small(stage2 = "map_c5", seed = 1L)
  ## beta_hat differs between MAP and the single-DNN
  expect_false(isTRUE(all.equal(fit_none$beta_hat, fit_map$beta_hat,
                                tolerance = 1e-6)))
  ## but beta_hat_dnn on the MAP fit should equal beta_hat on the
  ## stage2='none' fit (same Stage 1)
  expect_identical(fit_none$beta_hat, fit_map$beta_hat_dnn)
})

test_that("sigma_prior and sigma_post_diag are populated for MAP fits", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_map <- scfit_small(stage2 = "map_c5", seed = 1L)
  expect_length(fit_map$sigma_prior, length(fit_map$theta))
  expect_true(all(fit_map$sigma_prior > 0))
  expect_length(fit_map$sigma_post_diag, length(fit_map$theta))
  expect_true(all(fit_map$sigma_post_diag > 0))
})

test_that("stage2='none' leaves sigma_prior / sigma_post_diag NULL", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_none <- scfit_small(stage2 = "none", seed = 1L)
  expect_null(fit_none$sigma_prior)
  expect_null(fit_none$sigma_post_diag)
  expect_identical(fit_none$stage2_method, "none")
})
