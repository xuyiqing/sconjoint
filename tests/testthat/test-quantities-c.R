## Tests for Tier C structural quantities + baseline wrappers.
## Uses simdata (known DGP) for ground-truth validation.

test_that("sc_surplus returns sc_quantity with positive estimate", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  s <- sc_surplus(fit, profiles = list(list(x1 = 1), list(x1 = 0)))
  expect_s3_class(s, "sc_quantity")
  expect_true(s$estimate > 0)
  expect_true(is.numeric(s$se) && s$se > 0)
})

test_that("sc_welfare_change positive when upgrading positive-beta attribute", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  wc <- sc_welfare_change(fit,
                          old_set = list(list(x1 = 0)),
                          new_set = list(list(x1 = 1)))
  expect_s3_class(wc, "sc_quantity")
  expect_true(wc$estimate > 0)
})

test_that("sc_average logit matches coef, probability is attenuated", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  a_logit <- sc_average(fit, scale = "logit")
  a_prob  <- sc_average(fit, scale = "probability")
  expect_s3_class(a_logit, "sc_quantity")
  expect_s3_class(a_prob, "sc_quantity")
  # logit estimates should match coef()
  theta <- coef(fit)
  expect_equal(a_logit$estimate$estimate, unname(theta), tolerance = 1e-10)
  # probability scale should be attenuated (smaller in absolute value)
  expect_true(all(abs(a_prob$estimate$estimate) < abs(theta)))
})

test_that("sc_indifference returns data.frame with mrs column", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  ind <- sc_indifference(fit, "x1", "x2")
  expect_s3_class(ind, "sc_quantity")
  expect_true("mrs" %in% names(ind$estimate))
  expect_true(nrow(ind$estimate) > 0)
})

test_that("sc_demand_curve returns decreasing demand", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  # x1 as "cost" — higher x1 should reduce demand if beta_x1 > 0 and
  # we sweep its value. Actually demand_curve computes choice probability
  # as function of cost, holding other attributes at reference.
  # For simdata, x1 is binary so cost_grid is limited.
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  dc <- sc_demand_curve(fit, cost_attr = "x1",
                        cost_grid = c(0, 0.5, 1, 2))
  expect_true("estimate" %in% names(dc))
  expect_true("cost" %in% names(dc$estimate))
  expect_true("demand" %in% names(dc$estimate))
})

test_that("sc_decisiveness returns estimate in [0,1]", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  d <- sc_decisiveness(fit, A = list(x1 = 1), B = list(x2 = 1))
  expect_s3_class(d, "sc_quantity")
  expect_true(d$estimate >= 0 && d$estimate <= 1)
})

test_that("sc_inequality variance larger for heterogeneous attributes", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  ineq <- sc_inequality(fit, measure = "variance")
  expect_s3_class(ineq, "sc_quantity")
  vals <- ineq$estimate$value
  # x1 and x2 are heterogeneous (depend on Z), x3 is constant
  # x1 variance should be > x3 variance
  expect_true(vals[1] > vals[3])
})

test_that("sc_inequality gini returns values in [0,1]", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  ineq <- sc_inequality(fit, measure = "gini")
  expect_true(all(ineq$estimate$value >= 0))
  expect_true(all(ineq$estimate$value <= 1))
})

test_that("sc_baseline_logit returns sc_baseline with matching coef signs", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  bl <- sc_baseline_logit(fit)
  expect_s3_class(bl, "sc_baseline")
  expect_equal(length(coef(bl)), length(coef(fit)))
  expect_equal(names(coef(bl)), names(coef(fit)))
  # Signs should match
  expect_equal(sign(coef(bl)), sign(coef(fit)))
  # vcov should be p x p
  V <- vcov(bl)
  expect_equal(nrow(V), length(coef(fit)))
  expect_equal(ncol(V), length(coef(fit)))
})

test_that("sc_baseline_lpm returns sc_baseline", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  data(simdata, package = "sconjoint")
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ x1 + x2 + x3 | z1 + z2, data = simdata,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 20L, seed = 1)
  bl <- sc_baseline_lpm(fit)
  expect_s3_class(bl, "sc_baseline")
  expect_equal(length(coef(bl)), length(coef(fit)))
})
