## Tests that `scfit(..., varref_floor = ...)` plumbs through to the
## sigma_prior on stage2 = "varref" fits.  The plumbing matters because
## the production setting (memo 42, code/60_setup_ballard_rosa.R) uses
## floor = 1e-3, which is now the package default; the prior 0.01 floor
## over-shrank continuous-attribute betas and collapsed BR validation r
## from 0.39 to 0.13.

scfit_small_varref <- function(varref_floor = 1e-3, seed = 1L) {
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:40]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(seed)
  torch::torch_manual_seed(seed)
  scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
          resp_female + age + pid,
        data = d,
        respondent = "respondent", task = "task", profile = "profile",
        K = 2L, n_epochs = 40L, seed = seed,
        stage2 = "varref", varref_floor = varref_floor)
}

test_that("varref_floor defaults to 1e-3 (production setting)", {
  expect_equal(formals(scfit)$varref_floor, 1e-3)
})

test_that("varref_floor plumbs through scfit() to sigma_prior", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_low  <- scfit_small_varref(varref_floor = 1e-6, seed = 1L)
  fit_high <- scfit_small_varref(varref_floor = 0.5,  seed = 1L)
  ## High floor binds: every prior variance is exactly 0.5.
  expect_true(all(fit_high$sigma_prior == 0.5))
  ## Low floor doesn't bind on this fixture: at least one prior is below 0.5.
  expect_true(any(fit_low$sigma_prior < 0.5))
})

test_that("varref_floor does not move theta or vcov (orthogonality)", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  fit_a <- scfit_small_varref(varref_floor = 1e-6, seed = 1L)
  fit_b <- scfit_small_varref(varref_floor = 0.5,  seed = 1L)
  expect_identical(fit_a$theta, fit_b$theta)
  expect_identical(fit_a$vcov,  fit_b$vcov)
})
