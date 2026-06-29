## sc_voteshare_contrast(): public raw-contrast counterpart of
## sc_counterfactual(); and as.data.frame.sc_quantity() (no-recursion coercion).

.fit_toy_vc <- function(seed = 1L) {
  toy <- .make_toy_long(M = 80L, T_i = 4L, p = 3L, p_Z = 2L, seed = 7L)
  scfit(y ~ a1 + a2 + a3 | z1 + z2, data = toy$data,
        respondent = "rid", task = "tid", profile = "pos",
        K = 4L, n_epochs = 60L, seed = seed)
}

test_that("sc_voteshare_contrast wraps the debiased orthogonal scalar exactly", {
  fit <- .fit_toy_vc()
  nm <- colnames(fit$beta_hat)
  cvec <- numeric(length(nm)); names(cvec) <- nm; cvec[1L] <- 1
  q <- sc_voteshare_contrast(fit, cvec)
  expect_s3_class(q, "sc_quantity")
  expect_true(is.finite(q$estimate) && is.finite(q$se) && q$se >= 0)
  d <- sconjoint:::.sc_debiased_scalar(fit, sconjoint:::.sc_dH_voteshare(cvec))
  expect_equal(q$estimate, unname(d["estimate"]), tolerance = 1e-10)
  expect_equal(q$se, unname(d["se"]), tolerance = 1e-10)
})

test_that("sc_voteshare_contrast accepts positional and named contrasts and validates", {
  fit <- .fit_toy_vc()
  nm <- colnames(fit$beta_hat)
  pos <- numeric(length(nm)); pos[2L] <- 1
  named <- stats::setNames(1, nm[2L])
  expect_equal(sc_voteshare_contrast(fit, pos)$estimate,
               sc_voteshare_contrast(fit, named)$estimate, tolerance = 1e-10)
  expect_error(sc_voteshare_contrast(fit, c(bogus = 1)), "not in the fit")
  expect_error(sc_voteshare_contrast(fit, c(1, 2)), "must have length")
})

test_that("as.data.frame.sc_quantity does not recurse and returns a tidy frame", {
  fit <- .fit_toy_vc()
  v <- numeric(ncol(fit$beta_hat)); v[1L] <- 1
  q <- sc_voteshare_contrast(fit, v)
  df <- as.data.frame(q)                       # would overflow the C stack pre-fix
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 1L)
  expect_true(all(c("estimate", "se", "ci_lo", "ci_hi") %in% names(df)))
  av <- sc_average(fit)                        # data.frame-valued estimate
  expect_identical(as.data.frame(av), av$estimate)
})
