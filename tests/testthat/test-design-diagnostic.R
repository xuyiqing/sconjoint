## Tests for sc_design_diagnostic().

test_that("sc_design_diagnostic errors when stage2 = 'none'", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  d <- sw2022_demo[sw2022_demo$respondent %in% unique(sw2022_demo$respondent)[1:30], ]
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d, respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 30L, seed = 1, stage2 = "none")
  expect_error(sc_design_diagnostic(fit), "requires an sc_fit produced with")
})

test_that("sc_design_diagnostic returns a structured result on sw2022", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022_demo$respondent)[1:60]
  d <- sw2022_demo[sw2022_demo$respondent %in% some_resp, ]
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d, respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 40L, seed = 1, stage2 = "map_c5")
  diag <- sc_design_diagnostic(fit)
  expect_s3_class(diag, "sc_quantity_design_diagnostic")
  expect_s3_class(diag, "sc_quantity")
  ## estimate is a list with $estimate (data.frame) and $summary
  expect_true(is.data.frame(diag$estimate$estimate))
  expect_true(all(c("dummy_name", "var_ens", "mean_post_var", "R2_Z")
                  %in% names(diag$estimate$estimate)))
  expect_equal(nrow(diag$estimate$estimate), length(fit$theta))
  ## R^2_Z bounded in [0, 1]
  r2 <- diag$estimate$estimate$R2_Z
  expect_true(all(r2 >= 0 & r2 <= 1, na.rm = TRUE))
  ## Summary fields
  s <- diag$estimate$summary
  expect_named(s$tier_passes,
               c("mean", "distributional", "individual", "ratio"),
               ignore.order = FALSE)
  expect_true(isTRUE(s$tier_passes[["mean"]]))
  expect_true(s$N_resp == length(some_resp))
})

test_that("sc_design_diagnostic prints a tier banner without error", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022_demo$respondent)[1:50]
  d <- sw2022_demo[sw2022_demo$respondent %in% some_resp, ]
  set.seed(2); torch::torch_manual_seed(2)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d, respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 30L, seed = 2, stage2 = "map_c5")
  diag <- sc_design_diagnostic(fit)
  expect_output(print(diag), "sc_design_diagnostic")
  expect_output(print(diag), "Recovery tiers")
})

test_that("custom tier_thresholds override the defaults", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022_demo$respondent)[1:60]
  d <- sw2022_demo[sw2022_demo$respondent %in% some_resp, ]
  set.seed(3); torch::torch_manual_seed(3)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d, respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 30L, seed = 3, stage2 = "map_c5")
  ## Trivially low thresholds: all tiers should now pass
  diag <- sc_design_diagnostic(
    fit,
    tier_thresholds = list(
      T_distributional = 0,  R2_distributional = 0,
      T_individual     = 0,  R2_individual    = 0,
      T_ratio          = 0,  R2_ratio         = 0,
      N_ratio          = 0
    )
  )
  expect_true(all(diag$estimate$summary$tier_passes))
})
