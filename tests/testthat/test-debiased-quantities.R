## Debiased (orthogonal-score) inference engine for the additional QoI.
## The defining correctness property: with H = f_k the engine must reproduce
## the average-parameter estimate and clustered SE that scfit() already
## computes and stores (object$theta, object$vcov) -- exactly. The other
## estimands then differ only in their H / grad, so this anchors all of them.

.fit_toy <- function(seed = 1L) {
  toy <- .make_toy_long(M = 80L, T_i = 4L, p = 3L, p_Z = 2L, seed = 7L)
  scfit(y ~ a1 + a2 + a3 | z1 + z2, data = toy$data,
        respondent = "rid", task = "tid", profile = "pos",
        K = 4L, n_epochs = 60L, seed = seed)
}

test_that("debiased scalar with H = f_k reproduces stored theta and clustered SE", {
  fit <- .fit_toy()
  for (k in seq_along(coef(fit))) {
    d <- sconjoint:::.sc_debiased_scalar(fit, sconjoint:::.sc_dH_thetak(k))
    expect_equal(unname(d["estimate"]), unname(coef(fit)[k]), tolerance = 1e-8)
    expect_equal(unname(d["se"]),
                 unname(sqrt(diag(fit$vcov))[k]), tolerance = 1e-8)
  }
})

test_that("debiased importance shares sum to one with finite positive SEs", {
  fit <- .fit_toy()
  imp <- sconjoint:::.sc_debiased_importance(fit, fit$attr_map)
  expect_equal(sum(imp$share), 1, tolerance = 1e-8)
  expect_true(all(is.finite(imp$se)) && all(imp$se >= 0))
  expect_equal(nrow(imp), length(fit$attr_map))
})

test_that("debiased population MRS/WTP return finite estimate, SE, and Fieller", {
  fit <- .fit_toy()
  for (tr in c("mrs", "wtp")) {
    r <- sconjoint:::.sc_debiased_ratio(fit, 1L, 2L, transform = tr)
    expect_true(is.finite(r$estimate) && is.finite(r$se) && r$se >= 0)
    expect_true(r$fieller_type %in% c("bounded", "empty", "all_real", "exclusive"))
  }
})

test_that("debiased vote share for a contrast lies in (0, 1)", {
  fit <- .fit_toy()
  vs <- sconjoint:::.sc_debiased_scalar(
    fit, sconjoint:::.sc_dH_voteshare(c(1, 0, 0)))
  expect_true(vs["estimate"] > 0 && vs["estimate"] < 1)
  expect_true(is.finite(vs["se"]) && vs["se"] >= 0)
})

test_that("sc_mrs / sc_wtp population estimand returns the debiased ratio", {
  fit <- .fit_toy()
  m  <- sc_mrs(fit, "a1", "a2", estimand = "population")
  r  <- sconjoint:::.sc_debiased_ratio(fit, 1L, 2L, "mrs")
  expect_equal(unname(m$estimate), unname(r$estimate), tolerance = 1e-10)
  expect_equal(unname(m$se),       unname(r$se),       tolerance = 1e-10)
  expect_identical(m$details$estimand, "population")
  expect_true(m$details$fieller_type %in%
                c("bounded", "empty", "all_real", "exclusive"))

  w  <- sc_wtp(fit, "a1", "a2", estimand = "population")
  rw <- sconjoint:::.sc_debiased_ratio(fit, 1L, 2L, "wtp")
  expect_equal(unname(w$estimate), unname(rw$estimate), tolerance = 1e-10)
  expect_identical(w$details$estimand, "population")

  ## individual (default) path is unchanged
  expect_identical(sc_mrs(fit, "a1", "a2")$name, "mrs")
})

test_that("debiased inference errors clearly when the fit lacks the needed slots", {
  fit <- .fit_toy()
  fit$correction <- NULL
  expect_error(
    sconjoint:::.sc_debiased_scalar(fit, sconjoint:::.sc_dH_thetak(1L)),
    "object\\$correction")
})
