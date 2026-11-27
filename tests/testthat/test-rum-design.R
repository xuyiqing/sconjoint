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

# ---------------------------------------------------------------------------
# Sequence-level gates (agent G1, 2026-10-12). The per-task gates above cannot
# see dependence across one respondent's tasks; these compare whole sequences.
# ---------------------------------------------------------------------------

test_that("GS: the negative candidate's side is a balanced 8/8 block over scenarios 1-16", {
  sp <- .rum_design_spec("gs")
  M <- 20000L
  r <- .rum_design_draw_raw(sp, M, seed = 101)
  negL <- r$dem[, , 1] > 7
  negR <- r$dem[, , 2] > 7
  k <- rowSums(negL)
  # 9 negatives per sequence; a block of 8 left / 8 right: never 0 or 9 on one side
  expect_true(all(rowSums(negL | negR) == 9))
  expect_true(all(k >= 1 & k <= 8))
  # hypergeometric(16 sides, 8 left, 9 drawn): mean 4.5, variance 9 * 1/4 * 7/15 = 1.05 (coin flips: 2.25)
  expect_lt(abs(mean(k) - 4.5), 4 * sqrt(1.05 / M))
  expect_lt(abs(stats::var(k) - 1.05), 0.06)
  p <- stats::dhyper(1:8, 8, 8, 9)
  z <- (tabulate(k, 8) - M * p) / sqrt(M * p * (1 - p))
  expect_lt(max(abs(z)), 4.5)
  # each scenario still has the negative candidate on the left with probability 1/2
  pl <- colSums(negL) / colSums(negL | negR)
  expect_lt(max(abs(pl - 0.5)), 4.5 * sqrt(0.25 / (M * 9 / 13)))
  # truncation keeps the block's law for the first T scenarios
  r5 <- .rum_design_draw_raw(sp, 4000L, seed = 101, T = 5)
  expect_identical(dim(r5$dem), c(4000L, 5L, 2L))
})

test_that("generated sequences match the observed designs' sequence statistics", {
  fx <- local({ e <- new.env(); sys.source(test_path("fixtures", "rum-design-sequence-stats.R"), envir = e); e$rum_design_seq_fixture })
  M <- 20000L
  pair_moment <- function(D) { T <- ncol(D); s <- rowSums(D); (s^2 - rowSums(D^2)) / (T * (T - 1)) }
  sumcols <- function(d, cols) apply(d[, , cols, drop = FALSE], c(1, 2), sum)
  gen <- list()   # per-sequence generated values, keyed "study|group|cell"
  r <- .rum_design_draw_raw(.rum_design_spec("gs"), M, seed = 202)
  dl <- r$dem[, , 1]; dr <- r$dem[, , 2]; pl <- r$party[, , 1]; pr <- r$party[, , 2]
  cnt <- list("negative on side1" = rowSums(dl > 7),
              "undemocratic on side1" = rowSums(dl >= 8 & dl <= 14),
              "valence on side1" = rowSums(dl > 14),
              "negative is D" = rowSums((dl > 7 & pl == 1) | (dr > 7 & pr == 1)),
              "negative in same-party pair" = rowSums((dl > 7 | dr > 7) & pl == pr),
              "negative on side1 in opposite pair" = rowSums(dl > 7 & pl != pr),
              "negative on side1 in same pair" = rowSums(dl > 7 & pl == pr),
              "female on side1 and negative" = rowSums(r$female[, , 1] == 1 & dl > 7),
              "party D on side1" = rowSums(pl == 1))
  for (nm in names(cnt)) gen[[paste("gs|all", nm, sep = "|")]] <- cnt[[nm]]
  for (g in c("D", "R", "I")) {
    d <- .rum_design_code(.rum_design_spec("gs"), r, g); fn <- dimnames(d)[[3]]
    gen[[paste("gs", g, "undemocratic", sep = "|")]] <- pair_moment(sumcols(d, grep("^diff_dem_code_u_", fn)))
    gen[[paste("gs", g, "negative", sep = "|")]] <- pair_moment(sumcols(d, grep("^diff_dem_code_[uv]_", fn)))
    gen[[paste("gs", g, "policy1", sep = "|")]] <- pair_moment(d[, , "diff_p1_num"])
    gen[[paste("gs", g, "policy2", sep = "|")]] <- pair_moment(d[, , "diff_p2_num"])
  }
  d <- .rum_draw_designs("br", M = M, seed = 203)
  gen[["br|all|top"]] <- pair_moment(d[, , 11])
  gen[["br|all|bottom"]] <- pair_moment(d[, , 1])
  gen[["br|all|progressivity"]] <- pair_moment(apply(sweep(d[, , seq(1, 11, by = 2)], 3, c(-5, -3, -1, 1, 3, 5), "*"), c(1, 2), sum))
  d <- .rum_draw_designs("sw_additive", M = M, seed = 204)
  gen[["sw|all|gender"]] <- pair_moment(d[, , "cand_genderMale"])
  gen[["sw|all|ambition"]] <- pair_moment(d[, , "cand_runYes"])

  key <- paste(fx$study, fx$group, fx$cell, sep = "|")
  expect_setequal(key, names(gen))
  z <- vapply(seq_len(nrow(fx)), function(i) {
    v <- gen[[key[i]]]
    if (fx$stat[i] == "count_var") {           # observed variance vs the law's, sd of (C - mu)^2 from the draws
      w <- (v - mean(v))^2
      (fx$obs_var[i] - mean(w)) / sqrt(stats::var(w) / fx$n[i] + stats::var(w) / M)
    } else (fx$obs_mean[i] - mean(v)) / sqrt(stats::var(v) / fx$n[i] + stats::var(v) / M)
  }, 0)
  names(z) <- key
  # each cell's bound is its study's two-sided Bonferroni 5% bound over the whole sequence audit (G1_design_fix.md):
  # GS 4.29, BR 3.84, SW 3.64. BR's top-rate pair moment sits at z = 3.1 (open, see the report); the old GS
  # coin-flip side gives z = -15 on the first cell.
  bad <- abs(z) >= fx$bound
  expect_true(!any(bad), info = paste(sprintf("%s z = %.2f", key[bad], z[bad]), collapse = "; "))
})
