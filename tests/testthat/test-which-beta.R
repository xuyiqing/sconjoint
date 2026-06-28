## Tests for the .sc_pick_beta() helper and the which_beta argument
## semantics on quantity functions.

test_that(".sc_pick_beta returns beta_hat_dnn when present and which_beta='dnn'", {
  fit <- list(beta_hat = matrix(1, 4, 2),
              beta_hat_dnn = matrix(2, 4, 2),
              stage2_method = "map_c5")
  class(fit) <- c("sc_fit", "list")
  expect_equal(.sc_pick_beta(fit, "hybrid"), matrix(1, 4, 2))
  expect_equal(.sc_pick_beta(fit, "dnn"),    matrix(2, 4, 2))
})

test_that(".sc_pick_beta falls back to dnn when stage2='none'", {
  fit <- list(beta_hat = matrix(1, 4, 2),
              beta_hat_dnn = matrix(2, 4, 2),
              stage2_method = "none")
  class(fit) <- c("sc_fit", "list")
  ## With stage2='none', "hybrid" should fall through to the dnn slot.
  expect_equal(.sc_pick_beta(fit, "hybrid"), matrix(2, 4, 2))
})

test_that(".sc_pick_beta works on v0.1 fits with no beta_hat_dnn slot", {
  fit <- list(beta_hat = matrix(3, 4, 2))
  class(fit) <- c("sc_fit", "list")
  expect_equal(.sc_pick_beta(fit, "hybrid"), matrix(3, 4, 2))
  expect_equal(.sc_pick_beta(fit, "dnn"),    matrix(3, 4, 2))
})

test_that("which_beta argument is exposed on all quantity functions (smoke)", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022_demo$respondent)[1:30]
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = sw2022_demo[sw2022$respondent %in% some_resp, ],
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 30L, seed = 1)

  ## Each call should run without error and return an sc_quantity object.
  expect_s3_class(sc_fraction_preferring(fit, which_beta = "dnn"), "sc_quantity")
  expect_s3_class(sc_fraction_preferring(fit, which_beta = "hybrid"), "sc_quantity")
  expect_s3_class(sc_heterogeneity_test(fit, which_beta = "dnn"),  "sc_quantity")
  expect_s3_class(sc_importance(fit, which_beta = "dnn"),          "sc_quantity")
  expect_s3_class(sc_polarization(fit, which_beta = "dnn"),        "sc_quantity")
  expect_s3_class(sc_optimal_profile(fit, which_beta = "dnn"),     "sc_quantity")
  expect_s3_class(sc_direction_intensity(fit, which_beta = "dnn"),
                  "sc_quantity_bivariate")
})
