test_that("sc_importance matches reference impl at 1e-6 (uniform)", {
  fit <- .get_toy_fit()
  q <- sc_importance(fit, design = "uniform")
  ref <- .ref_importance(fit$beta_hat, fit$respondent_id, fit$attr_map,
                         design = "uniform")
  expect_equal(q$estimate$share, ref$share, tolerance = 1e-6)
  expect_equal(q$estimate$se, ref$se, tolerance = 1e-6)
  expect_equal(sum(q$estimate$share), 1, tolerance = 1e-6)
})

test_that("sc_importance default is design_variance (paper formula)", {
  fit <- .get_toy_fit()
  q_def <- sc_importance(fit)
  q_dv  <- sc_importance(fit, design = "design_variance")
  expect_identical(q_def$estimate$share, q_dv$estimate$share)
  expect_equal(sum(q_def$estimate$share), 1, tolerance = 1e-6)
  expect_true(all(q_def$estimate$share >= 0))
})

test_that("sc_importance design_variance matches Sum beta^2 * Var(dX) directly", {
  fit <- .get_toy_fit()
  q <- sc_importance(fit, design = "design_variance")
  ## Hand-compute the same thing from scratch.
  attr_map <- fit$attr_map
  B  <- fit$beta_hat
  dX <- fit$deltaX
  K  <- length(attr_map)
  V <- matrix(0, nrow(B), K)
  for (a in seq_len(K)) {
    cols <- attr_map[[a]]
    dvar <- apply(dX[, cols, drop = FALSE], 2L, stats::var)
    V[, a] <- as.numeric((B[, cols, drop = FALSE]^2) %*% dvar)
  }
  row_sum <- rowSums(V)
  share <- V / pmax(row_sum, .Machine$double.eps)
  ref_share <- colMeans(share)
  expect_equal(q$estimate$share, ref_share, tolerance = 1e-10)
})

test_that("sc_importance empirical branch returns valid shares", {
  fit <- .get_toy_fit()
  q_emp <- sc_importance(fit, design = "empirical")
  expect_equal(sum(q_emp$estimate$share), 1, tolerance = 1e-6)
  expect_true(all(q_emp$estimate$share >= 0))
  ## Empirical and uniform should differ in general
  q_unif <- sc_importance(fit, design = "uniform")
  expect_false(isTRUE(all.equal(q_emp$estimate$share, q_unif$estimate$share)))
})

test_that("sc_importance levels design falls back to uniform without levels", {
  fit <- .get_toy_fit()
  q_lv <- sc_importance(fit, design = "levels")
  q_un <- sc_importance(fit, design = "uniform")
  expect_equal(q_lv$estimate$share, q_un$estimate$share, tolerance = 1e-12)
})

test_that("sc_importance levels design weights continuous attrs by level-set variance", {
  fit <- .get_toy_fit()
  map <- fit$attr_map
  a1 <- names(map)[1]
  stopifnot(length(map[[a1]]) == 1L)
  lv <- list(c(0, 5, 15, 25)); names(lv) <- a1
  q <- sc_importance(fit, design = "levels", levels = lv)
  ## hand-compute: a1 via beta^2 * popvar(levels); others via uniform formula
  B <- fit$beta_hat
  K <- length(map)
  V <- matrix(0, nrow(B), K)
  for (a in seq_len(K)) {
    cols <- map[[a]]
    bsub <- B[, cols, drop = FALSE]
    if (names(map)[a] == a1) {
      l <- lv[[a1]]
      V[, a] <- as.numeric(bsub^2) * mean((l - mean(l))^2)
    } else {
      L_a <- length(cols) + 1L
      V[, a] <- rowSums(bsub^2) / L_a - (rowSums(bsub) / L_a)^2
    }
  }
  share <- V / rowSums(V)
  expect_equal(q$estimate$share, colMeans(share), tolerance = 1e-10)
  expect_equal(sum(q$estimate$share), 1, tolerance = 1e-6)
})

test_that("sc_importance levels design validates its levels argument", {
  fit <- .get_toy_fit()
  expect_error(sc_importance(fit, design = "levels", levels = list(c(1, 2))),
               "named list")
  expect_error(sc_importance(fit, design = "levels",
                             levels = list(nope = c(1, 2))),
               "unknown attribute")
  a1 <- names(fit$attr_map)[1]
  bad <- list("x"); names(bad) <- a1
  expect_error(sc_importance(fit, design = "levels", levels = bad), "numeric")
  expect_warning(sc_importance(fit, design = "uniform",
                               levels = setNames(list(c(0, 1)), a1)),
                 "ignored")
})
