## Bit-exact determinism tests for Stage 2.
## Builds on the existing crossfit-determinism guarantees: the master
## seed pins Stage 1; stage2_seed pins Stage 2.  Parallel vs sequential
## must produce identical sc_fit objects (slot-by-slot).

scfit_for_determinism <- function(seed = 1L, stage2_seed = 12345L,
                                  stage2 = "map_c5",
                                  parallel = FALSE, n_cores = NULL) {
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:30]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(seed)
  torch::torch_manual_seed(seed)
  scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
          resp_female + age + pid,
        data = d,
        respondent = "respondent", task = "task", profile = "profile",
        K = 2L, n_epochs = 30L, seed = seed,
        stage2 = stage2, stage2_seed = stage2_seed,
        parallel = parallel, n_cores = n_cores)
}

test_that("stage2='map_c5' is deterministic on the same (seed, stage2_seed)", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  f1 <- scfit_for_determinism(seed = 1L, stage2_seed = 12345L)
  f2 <- scfit_for_determinism(seed = 1L, stage2_seed = 12345L)
  expect_identical(f1$beta_hat,        f2$beta_hat)
  expect_identical(f1$beta_hat_dnn,    f2$beta_hat_dnn)
  expect_identical(f1$beta_hat_dnn2,   f2$beta_hat_dnn2)
  expect_identical(f1$beta_hat_ens,    f2$beta_hat_ens)
  expect_identical(f1$beta_hat_resp,   f2$beta_hat_resp)
  expect_identical(f1$sigma_prior,     f2$sigma_prior)
  expect_identical(f1$sigma_post_diag, f2$sigma_post_diag)
  expect_identical(f1$theta,           f2$theta)
})

test_that("stage2_seed actually changes Stage-2 output", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  f1 <- scfit_for_determinism(seed = 1L, stage2_seed = 12345L)
  f2 <- scfit_for_determinism(seed = 1L, stage2_seed = 999L)
  ## Different stage2_seed -> different 2nd DNN -> different ensemble
  ## -> different MAP betas.  Stage 1 (theta) unaffected.
  expect_identical(f1$theta, f2$theta)
  expect_identical(f1$beta_hat_dnn, f2$beta_hat_dnn)
  expect_false(isTRUE(all.equal(f1$beta_hat, f2$beta_hat, tolerance = 1e-6)))
})

test_that("sequential and parallel runs produce identical Stage-2 output", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  skip_if_not_installed("future.apply")
  f_seq <- scfit_for_determinism(seed = 1L, stage2_seed = 12345L,
                                  parallel = FALSE)
  f_par <- scfit_for_determinism(seed = 1L, stage2_seed = 12345L,
                                  parallel = TRUE, n_cores = 2L)
  expect_identical(f_seq$beta_hat,      f_par$beta_hat)
  expect_identical(f_seq$beta_hat_dnn,  f_par$beta_hat_dnn)
  expect_identical(f_seq$beta_hat_dnn2, f_par$beta_hat_dnn2)
  expect_identical(f_seq$theta,         f_par$theta)
})
