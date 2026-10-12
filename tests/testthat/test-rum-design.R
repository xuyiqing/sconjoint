# Design generators for the RUM conditional information (R/rum-design.R).

fx <- local({ e <- new.env(); sys.source(test_path("fixtures", "rum-design-observed.R"), envir = e); e$rum_design_fixture })

test_that("the coders reproduce stored observed d rows exactly", {
  g <- fx$gs
  sp <- .rum_design_spec("gs")
  expect_identical(sp$features, g$features)
  d <- .rum_design_code_profiles(sp, g$profA, g$profB, resp = g$pid_lean)
  expect_equal(max(abs(unname(d) - g$d)), 0)
  # one respondent row per task, or pid7 instead of pid_lean
  pid7 <- c(D = -1, R = 2, I = 0)[g$pid_lean]
  expect_equal(unname(.rum_design_code_profiles(sp, g$profA, g$profB, data.frame(pid7 = pid7))), g$d)

  b <- fx$br
  sp <- .rum_design_spec("br")
  expect_identical(sp$features, b$features)
  expect_equal(max(abs(unname(.rum_design_code_profiles(sp, b$profA, b$profB)) - b$d)), 0)

  s <- fx$sw
  sa <- .rum_design_spec("sw_additive"); sx <- .rum_design_spec("sw_explicit")
  expect_identical(sa$features, s$features_additive)
  expect_identical(sx$features, s$features_explicit)
  expect_equal(max(abs(unname(.rum_design_code_profiles(sa, s$profA, s$profB)) - s$d_additive)), 0)
  expect_equal(max(abs(unname(.rum_design_code_profiles(sx, s$profA, s$profB)) - s$d_explicit)), 0)
})

test_that("GS blocking constraints hold on every drawn sequence", {
  sp <- .rum_design_spec("gs")
  M <- 2000L
  r <- .rum_design_draw_raw(sp, M, seed = 11)
  L <- r$party[, , 1]; R <- r$party[, , 2]
  expect_true(all(rowSums(L == 1 & R == 1) == 3))          # (D,D)
  expect_true(all(rowSums(L == 2 & R == 2) == 3))          # (R,R)
  expect_true(all(rowSums(L == 1 & R == 2) %in% 3:4))      # (D,R)
  expect_true(all(rowSums(L != R) == 7))
  dl <- r$dem[, , 1]; dr <- r$dem[, , 2]
  neg <- dl > 7 | dr > 7
  expect_true(all(!(dl > 7 & dr > 7)))                     # at most one negative candidate
  expect_true(all(rowSums(!neg) == 4))                     # 4 generic-vs-generic
  for (k in 8:16) expect_true(all(rowSums(dl == k | dr == k) == 1))
  expect_true(all(dl[!neg] != dr[!neg]))                   # distinct generic levels
  women <- rowSums(r$female[, , 1]) + rowSums(r$female[, , 2])
  expect_true(all(women <= 12 & women >= 6))
  expect_true(all(apply(r$area, 1, function(a) all(tabulate(a, 4) <= 4))))
  expect_true(all(r$p1 %in% 1:4) && all(r$race %in% 1:4) && all(r$pro %in% 1:9))
})

test_that("seeds reproduce and the array, list and CRN paths agree", {
  for (st in c("gs", "br", "sw_additive", "sw_explicit")) {
    sp <- .rum_design_spec(st)
    a <- .rum_draw_designs(sp, resp = data.frame(pid7 = 2), M = 20, seed = 7)
    b <- .rum_draw_designs(sp, resp = data.frame(pid7 = 2), M = 20, seed = 7)
    expect_identical(a, b)
    expect_identical(dim(a), c(20L, sp$T, sp$p))
    expect_identical(dimnames(a)[[3]], sp$features)
    expect_false(identical(a, .rum_draw_designs(sp, resp = data.frame(pid7 = 2), M = 20, seed = 8)))
    l <- .rum_draw_designs(sp, resp = data.frame(pid7 = 2), M = 20, seed = 7, format = "list")
    expect_length(l, 20L)
    expect_equal(unname(l[[5]]), unname(a[5, , ]))
    crn <- .rum_draw_designs_crn(sp, resp = data.frame(pid7 = c(2, -3, 0)), M = 20, seed = 7)
    expect_identical(.rum_crn_designs(crn, 1), a)
    expect_identical(dim(.rum_draw_designs(sp, "R", M = 4, seed = 1, T = 2)), c(4L, 2L, sp$p))
  }
  # the global RNG stream is left untouched
  set.seed(1); u1 <- stats::runif(1)
  set.seed(1); invisible(.rum_draw_designs("gs", "D", M = 5, seed = 3)); u2 <- stats::runif(1)
  expect_identical(u1, u2)
})

test_that("pure independents' co-partisan column is identically 0", {
  sp <- .rum_design_spec("gs")
  d0 <- .rum_draw_designs(sp, resp = data.frame(pid7 = 0), M = 500, seed = 3)
  expect_true(all(d0[, , "diff_respParty"] == 0))
  dD <- .rum_draw_designs(sp, resp = data.frame(pid7 = -1), M = 500, seed = 3)
  dR <- .rum_draw_designs(sp, resp = data.frame(pid7 = 1), M = 500, seed = 3)
  # same raw draws: D and R contrasts are mirror images, the rest is identical
  expect_identical(dD[, , "diff_respParty"], -dR[, , "diff_respParty"])
  expect_identical(dD[, , -1], d0[, , -1])
  expect_true(all(rowSums(dD[, , "diff_respParty"] != 0) == 7))
  expect_error(.rum_draw_designs(sp, resp = NULL, M = 2), "party")
})

test_that("BR revenue draws follow the rule's probabilities", {
  sp <- .rum_design_spec("br")
  r <- .rum_design_draw_raw(sp, 3000, seed = 5)
  rates <- vapply(1:6, function(j) .RUM_BR_MENUS[[j]][as.vector(r[[paste0("r", j)]])], numeric(3000 * 16))
  P <- .rum_br_revenue_probs(rates)
  expect_equal(rowSums(P), rep(1, nrow(P)), tolerance = 1e-12)
  expected <- colSums(P); observed <- tabulate(as.vector(r$taxrev), 5)
  expect_lt(max(abs(observed - expected) / sqrt(expected)), 5)
  expect_gt(mean(.rum_br_revenue_code(.rum_br_revenue_ratio(rates)) == as.vector(r$taxrev)), 0.95)
})
