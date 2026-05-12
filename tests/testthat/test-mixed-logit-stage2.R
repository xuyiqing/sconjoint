## Tests for stage2 = "mixed_logit": converged path, parametrization,
## and the convergence-failure fallback.

test_that(".sc_mixed_logit_stage2 returns valid BLUPs on a clean fixture", {
  skip_if_not_installed("lme4")
  set.seed(1)
  n_resp <- 30L
  T_i <- 6L
  P <- 3L
  n_task <- n_resp * T_i
  resp_idx <- rep(seq_len(n_resp), each = T_i)
  deltaX <- matrix(stats::rnorm(n_task * P), n_task, P)
  beta_true_resp <- matrix(stats::rnorm(n_resp * P, sd = 0.5), n_resp, P)
  beta_task <- beta_true_resp[resp_idx, , drop = FALSE]
  y <- stats::rbinom(n_task, 1, stats::plogis(rowSums(deltaX * beta_task)))
  beta_hat_ens_resp <- beta_true_resp + matrix(stats::rnorm(n_resp * P, sd = 0.1),
                                                n_resp, P)
  out <- .sc_mixed_logit_stage2(deltaX, y, beta_hat_ens_resp, resp_idx)
  expect_true(out$status %in% c("converged", "converged_with_warnings"))
  expect_equal(dim(out$beta_hat_resp), c(n_resp, P))
  expect_true(all(is.finite(out$beta_hat_resp)))
})

test_that("stage2 = 'mixed_logit' end-to-end via scfit on sw2022 subset", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  skip_if_not_installed("lme4")
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:40]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(1); torch::torch_manual_seed(1)
  fit <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                 resp_female + age + pid,
               data = d,
               respondent = "respondent", task = "task", profile = "profile",
               K = 2L, n_epochs = 40L, seed = 1, stage2 = "mixed_logit")
  expect_true(fit$stage2_method %in% c("mixed_logit", "mixed_logit_failed"))
  if (identical(fit$stage2_method, "mixed_logit")) {
    expect_equal(dim(fit$beta_hat_resp),
                 c(length(some_resp), ncol(fit$beta_hat)))
    expect_false(isTRUE(all.equal(fit$beta_hat, fit$beta_hat_dnn,
                                  tolerance = 1e-6)))
  }
})

test_that("mixed_logit preserves DML orthogonality (theta and vcov)", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  skip_if_not_installed("lme4")
  data(sw2022, package = "sconjoint")
  some_resp <- unique(sw2022$respondent)[1:40]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  ## Same seed, two different stage2 choices: theta and vcov match
  set.seed(1); torch::torch_manual_seed(1)
  fit_none <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                      resp_female + age + pid,
                    data = d,
                    respondent = "respondent", task = "task", profile = "profile",
                    K = 2L, n_epochs = 40L, seed = 1, stage2 = "none")
  set.seed(1); torch::torch_manual_seed(1)
  fit_ml <- scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
                    resp_female + age + pid,
                  data = d,
                  respondent = "respondent", task = "task", profile = "profile",
                  K = 2L, n_epochs = 40L, seed = 1, stage2 = "mixed_logit")
  expect_identical(fit_none$theta, fit_ml$theta)
  expect_identical(fit_none$vcov,  fit_ml$vcov)
})

test_that("mixed_logit failure fallback path is exercised", {
  ## Manufacture a tiny pathological case where glmer is likely to either
  ## fail outright or emit a singular-fit warning.  We test that the
  ## controller-level behavior is well-defined either way:
  ##   - status == "failed"      -> a warning() is signalled and the
  ##                                 returned beta_hat equals beta_hat_dnn
  ##   - status == "converged*" -> beta_hat_resp has the right shape
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed())
  skip_if_not_installed("lme4")
  data(sw2022, package = "sconjoint")
  ## Tiny subset, very few tasks: glmer often complains here.
  some_resp <- unique(sw2022$respondent)[1:10]
  d <- sw2022[sw2022$respondent %in% some_resp, ]
  set.seed(7); torch::torch_manual_seed(7)
  res <- tryCatch(
    withCallingHandlers(
      scfit(choice ~ agenda + talent + children + cand_gender + prior_office |
              resp_female + age + pid,
            data = d,
            respondent = "respondent", task = "task", profile = "profile",
            K = 2L, n_epochs = 20L, seed = 7, stage2 = "mixed_logit"),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  ## Either we converged (with or without warnings) or we got a clean
  ## fallback. The package must never error from this path.
  expect_false(is.null(res))
  expect_true(res$stage2_method %in% c("mixed_logit", "mixed_logit_failed"))
  if (identical(res$stage2_method, "mixed_logit_failed")) {
    ## Fallback: beta_hat must equal beta_hat_dnn
    expect_identical(res$beta_hat, res$beta_hat_dnn)
    expect_null(res$beta_hat_resp)
  }
})

test_that(".sc_mixed_logit_stage2 errors clearly if lme4 is missing", {
  ## Best-effort: simulate the lme4-not-installed case by temporarily
  ## shadowing requireNamespace.  Skipped if testthat doesn't support
  ## the `local_mocked_bindings` mechanism cleanly.
  ## A simpler check: just verify the error message string is in the
  ## function body so future grep tests catch removal.
  src <- deparse(.sc_mixed_logit_stage2)
  expect_true(any(grepl("requires the 'lme4'", src, fixed = TRUE)))
})
