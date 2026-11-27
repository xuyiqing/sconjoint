# Bansak-Hainmueller-Hangartner 2016 design law (R/rum-design-bhh.R).

test_that("BHH coding is profile 1 minus profile 2, first level as reference, full rank", {
  sp <- sconjoint:::.rum_design_spec_bhh()
  expect_equal(sp$p, 27L)
  expect_equal(length(sconjoint:::.RUM_BHH_FEATURES), sum(lengths(sconjoint:::.RUM_BHH_LEVELS) - 1L))
  a <- sconjoint:::.rum_draw_designs(sp, NULL, 400, seed = 11, raw = TRUE)
  expect_equal(dim(a), c(400L, 5L, 27L))
  expect_identical(dimnames(a)[[3]], sconjoint:::.RUM_BHH_FEATURES)
  D <- matrix(a, 2000, 27)
  expect_true(all(D %in% c(-1, 0, 1)))
  expect_equal(qr(cbind(1, D))$rank, 28L)
  # recode through the profile coder
  pf <- sconjoint:::.rum_design_profiles(sp, attr(a, "raw"))
  D2 <- sconjoint:::.rum_design_code_profiles(sp, pf$profA, pf$profB)
  o <- order(pf$profA$task, pf$profA$draw)            # D is draw-fastest within task
  expect_equal(unname(D2[o, ]), unname(D))
})

test_that("BHH draws are uniform, reproducible and shared across respondents", {
  sp <- sconjoint:::.rum_design_spec_bhh()
  r <- sconjoint:::.rum_design_draw_raw(sp, 4000, seed = 5)
  for (a in names(sconjoint:::.RUM_BHH_LEVELS)) {
    L <- length(sconjoint:::.RUM_BHH_LEVELS[[a]])
    f <- tabulate(r[[a]], L) / length(r[[a]])
    expect_lt(max(abs(f - 1 / L)), 0.01)
  }
  x <- sconjoint:::.rum_draw_designs(sp, NULL, 30, seed = 9)
  y <- sconjoint:::.rum_draw_designs(sp, NULL, 30, seed = 9)
  expect_identical(x, y)
  crn <- sconjoint:::.rum_draw_designs_crn(sp, NULL, M = 20, seed = 3, n = 4)
  expect_equal(crn$key, rep(1L, 4))
  expect_equal(dim(crn$designs$all), c(20L, 5L, 27L))
  expect_equal(dim(sconjoint:::.rum_draw_designs(sp, NULL, 6, seed = 1, n_task = 3)), c(6L, 3L, 27L))
})
