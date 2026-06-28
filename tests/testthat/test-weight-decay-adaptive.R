## Tests the v13 NT-adaptive weight-decay rule (memo 42):
##   K_adaptive = 15 if NT/p < 300 else 25
##   weight_decay = K_adaptive / NT
## The rule itself is tested via the internal helper
## `.sc_resolve_weight_decay()`; scfit-level plumbing is verified by
## one small live fit.

test_that("scfit() defaults are paper-faithful", {
  expect_identical(formals(scfit)$weight_decay, "adaptive")
  expect_equal(formals(scfit)$n_epochs, 1000L)
})

test_that("'adaptive' fires K=15 when NT/p < 300", {
  ## Saha-Weeks regime: NT=3573, p=13, NT/p=275 -> K=15.
  expect_equal(sconjoint:::.sc_resolve_weight_decay("adaptive", NT = 3573, p = 13),
               15 / 3573)
  ## Boundary check just below 300.
  expect_equal(sconjoint:::.sc_resolve_weight_decay("adaptive", NT = 299, p = 1),
               15 / 299)
})

test_that("'adaptive' fires K=25 when NT/p >= 300", {
  ## Graham-Svolik regime: NT~20865, p=30, NT/p~695 -> K=25.
  expect_equal(sconjoint:::.sc_resolve_weight_decay("adaptive", NT = 20865, p = 30),
               25 / 20865)
  ## Ballard-Rosa regime: NT=16000, p=7, NT/p~2286 -> K=25.
  expect_equal(sconjoint:::.sc_resolve_weight_decay("adaptive", NT = 16000, p = 7),
               25 / 16000)
  ## Boundary at exactly 300.
  expect_equal(sconjoint:::.sc_resolve_weight_decay("adaptive", NT = 300, p = 1),
               25 / 300)
})

test_that("numeric weight_decay passes through unchanged", {
  expect_equal(sconjoint:::.sc_resolve_weight_decay(1e-4, NT = 1000, p = 5), 1e-4)
  expect_equal(sconjoint:::.sc_resolve_weight_decay(0,    NT = 1000, p = 5), 0)
})

test_that("invalid weight_decay errors clearly", {
  expect_error(sconjoint:::.sc_resolve_weight_decay("auto_v13", NT = 1000, p = 5),
               "adaptive")
  expect_error(sconjoint:::.sc_resolve_weight_decay(-1, NT = 1000, p = 5),
               "non-negative")
  expect_error(sconjoint:::.sc_resolve_weight_decay(NA_real_, NT = 1000, p = 5),
               "non-negative")
})

test_that("scfit() exposes the resolved weight_decay on the fit", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022_demo$respondent)[1:30]
  d <- sw2022_demo[sw2022_demo$respondent %in% some_resp, ]
  set.seed(1L); torch::torch_manual_seed(1L)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d, respondent = "respondent",
               task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1L,
               weight_decay = "adaptive", stage2 = "none")
  NT <- nrow(fit$deltaX)
  p  <- ncol(fit$deltaX)
  K_expected <- if (NT / p < 300) 15L else 25L
  expect_equal(fit$weight_decay_used, K_expected / NT)
  ## And a fixed numeric flows through:
  set.seed(1L); torch::torch_manual_seed(1L)
  fit2 <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                  resp_female + age + pid,
                data = d, respondent = "respondent",
                task = "task", profile = "profile",
                K = 2L, n_epochs = 20L, seed = 1L,
                weight_decay = 5e-4, stage2 = "none")
  expect_equal(fit2$weight_decay_used, 5e-4)
})
