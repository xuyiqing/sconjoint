## Tests for sc_validate_amce(): pooled and subgroup comparison
## of DML theta vs pooled-homogeneous-logit coefficient.

test_that("sc_validate_amce returns a structured result on sw2022 subset", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:60]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 40L, seed = 1)
  v <- sc_validate_amce(fit)
  expect_s3_class(v, "sc_quantity_validate_amce")
  expect_s3_class(v, "sc_quantity")
  expect_true(is.data.frame(v$estimate$pooled))
  expect_true(all(c("attribute", "dml_theta", "dml_se",
                    "homog_logit_coef", "homog_logit_se",
                    "diff", "abs_diff") %in% names(v$estimate$pooled)))
  expect_equal(nrow(v$estimate$pooled), length(fit$theta))
  expect_true(is.numeric(v$estimate$pooled_correlation))
  ## Note: on this 60-respondent / K=2 / 40-epoch toy fit, the DML
  ## inverse-Lambda step is poorly conditioned and theta can blow up,
  ## so the pooled correlation is not a stable smoke-test target.
  ## A meaningful correlation threshold lives in the manuscript
  ## replication script that runs the full K=10/2000-epoch fit; the
  ## paper itself reports r approx 0.998 on BHH 2023.
  expect_null(v$estimate$subgroup)
})

test_that("sc_validate_amce DML column is exactly object$theta", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:60]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(2); torch::torch_manual_seed(2)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 40L, seed = 2)
  v <- sc_validate_amce(fit)
  expect_equal(unname(v$estimate$pooled$dml_theta), unname(as.numeric(fit$theta)))
})

test_that("sc_validate_amce print method runs without error", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:60]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(3); torch::torch_manual_seed(3)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 40L, seed = 3)
  v <- sc_validate_amce(fit)
  expect_output(print(v), "sc_validate_amce")
  expect_output(print(v), "Pooled correlation")
})

test_that("sc_validate_amce errors informatively if deltaX/y are missing", {
  fake_fit <- list(theta = c(a = 1), vcov = matrix(0.1, 1, 1),
                   attr_names = "a", respondent_id = 1:3,
                   deltaX = NULL, y = NULL)
  class(fake_fit) <- c("sc_fit", "list")
  expect_error(sc_validate_amce(fake_fit), "deltaX")
})
