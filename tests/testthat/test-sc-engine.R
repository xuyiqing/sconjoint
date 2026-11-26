# Paper-v4 public layer: the held-out one-step engine, the contest constructors, rank selection, and the
# compact save format, on small synthetic problems.

# Synthetic fold states: random data, fitted means, common parameters, scores from the evaluator, and
# random positive-definite held-out information.
sc_test_states <- function(n = 30L, p = 3L, q = 1L, K = 2L, seed = 11L) {
  withr::with_seed(seed, {
    T <- 4L
    X <- matrix(sample(c(-1, 0, 1), n * T * p, TRUE), n * T, p, dimnames = list(NULL, paste0("f", seq_len(p))))
    y <- stats::rbinom(n * T, 1, .5); id <- rep(sprintf("i%02d", seq_len(n)), each = T)
    Z <- matrix(stats::rnorm(n * 2), n, 2)
    x <- sc_data(y, X, id, Z = Z, groups = data.frame(g = stats::rbinom(n, 1, .5)), fold = rep(seq_len(K), length.out = n))
    d <- x$data; n_th <- .rum_th_len(p, q); D <- p + n_th
    quad <- rum_quadrature(n_fac = q, rule = "sobol", nodes = 32L, seed = 3L)
    if (q == 1L) quad$u <- as.vector(quad$u)
    folds <- lapply(seq_len(K), function(k) {
      te <- which(x$fold == k)
      mu <- matrix(stats::rnorm(n * p, 0, .5), n, p, dimnames = list(d$ids, d$feature_names))
      L <- matrix(stats::rnorm(p * q, 0, .4), p, q)
      th <- list(kappa = .1, loading = if (q == 1L) drop(L) else .rum_canonical_loading(L), omega = .3)
      sub <- rum_subset(d, d$ids[te])
      ev <- rum_dnn_gaussian_evaluate(sub, unname(mu[te, , drop = FALSE]), th$kappa, th$loading, th$omega, quad)
      I <- array(0, c(length(te), D, D))
      for (i in seq_along(te)) { A <- matrix(stats::rnorm(D * D), D); I[i, , ] <- crossprod(A) / D + diag(D) }
      rot <- if (q > 1L) .rum_rotation_dirs(th$loading) else NULL
      parts <- .rum_weight_parts(function(i) I[i, , ], length(te), p, n_th, 1e-6, rot)
      list(k = k, te = te, mu = mu, th = th, output_score = unname(ev$output_score),
           common_score = unname(ev$common_score), parts = parts, I = I, diag = data.frame(fold = k), fit_sha = "test")
    })
    st <- .sc_new_states(x, q, folds, score = list(points = 16L, seed = 3L),
                         info = list(method = "synthetic", M = NA, ny = NA, nodes = NA, seed = NA, partial = FALSE))
  })
  st
}

test_that("the held-out one-step equals a direct computation of the correction", {
  st <- sc_test_states(q = 1L)
  d <- st$data$data; n <- d$n; p <- d$p; n_th <- .rum_th_len(p, 1L)
  w <- as.numeric(st$data$groups$g); cvec <- c(1, -.5, 2)
  tg <- .rum_tgt_mean("m", cvec, w)
  r <- .sc_onestep(st$folds, d, list(tg))
  phi <- m <- numeric(n); wr <- w / mean(w)
  for (f in st$folds) {
    te <- f$te; ip <- seq_len(p); it <- p + seq_len(n_th)
    v <- (wr * matrix(cvec, n, p, byrow = TRUE))[te, , drop = FALSE]
    gt_bar <- numeric(n_th)                                   # a mean contrast has no theta derivative
    Ic <- matrix(0, n_th, n_th); acc <- numeric(n_th)
    for (i in seq_along(te)) {
      L <- f$I[i, , ]
      Ic <- Ic + (L[it, it] - t(L[ip, it]) %*% solve(L[ip, ip], L[ip, it])) / length(te)
      acc <- acc + drop(t(L[ip, it]) %*% solve(L[ip, ip], v[i, ])) / length(te)
    }
    g <- solve(Ic, gt_bar - acc)
    for (i in seq_along(te)) {
      L <- f$I[i, , ]
      a <- solve(L[ip, ip], v[i, ] - L[ip, it] %*% g)
      phi[te[i]] <- sum(a * f$output_score[i, ]) + sum(g * f$common_score[i, ])
    }
    m[te] <- drop(f$mu[te, , drop = FALSE] %*% cvec)
  }
  plug <- sum(w * m) / sum(w); est <- plug + mean(phi)
  expect_equal(unname(r$est), est, tolerance = 1e-12)
  expect_equal(unname(r$plug), plug, tolerance = 1e-12)
  expect_equal(unname(r$IFP[, 1]), phi + wr * (m - est), tolerance = 1e-12)
  e <- sc_estimate(st, sc_mean("m", cvec, w = "g"))
  expect_equal(e$estimate, est, tolerance = 1e-12)
  expect_equal(e$se, stats::sd(phi + wr * (m - est)) / sqrt(n), tolerance = 1e-12)
  expect_equal(c(e$lower, e$upper), est + c(-1.96, 1.96) * e$se, tolerance = 1e-12)
  expect_identical(e$interval, "wald")
  expect_identical(e$n, as.integer(sum(w > 0)))
})

test_that("sc_estimate handles several factors, shares, contests and derived targets", {
  st <- sc_test_states(q = 2L)
  e <- sc_estimate(st, list(sc_mean("a", "f1"), sc_mean("b", c(f2 = 1)), sc_share("s", "f1"),
                            sc_contest("c", c(1, 0, -1)), sc_ratio("r", sc_mean("a", "f1"), sc_mean("b", "f2")),
                            sc_difference("d", sc_mean("a", "f1"), sc_mean("b", "f2"))))
  expect_identical(e$name, c("a", "b", "s", "c", "r", "d"))
  expect_true(all(is.finite(e$estimate)))
  expect_identical(e$interval, c("wald", "wald", "log-odds", "log-odds", "wald", "wald"))
  expect_equal(e$estimate[5], -e$estimate[1] / e$estimate[2], tolerance = 1e-12)
  expect_equal(e$estimate[6], e$estimate[1] - e$estimate[2], tolerance = 1e-12)
  infl <- attr(e, "influence")
  expect_equal(e$se[6], stats::sd(infl[, "a"] - infl[, "b"]) / sqrt(nrow(infl)), tolerance = 1e-12)
  e1 <- sc_estimate(st, sc_share("s", "f1"), bounded = FALSE)
  expect_identical(e1$status, "one-step")
  expect_error(sc_estimate(st, list(sc_mean("a", "f1"), sc_mean("a", "f2"))), "two targets are named")
  expect_error(sc_estimate(st, sc_mean("a", "nope")), "unknown feature")
})

test_that("contests built in a loop keep their own weights (lazy coef regression)", {
  D <- rbind(c(1, 0), c(0, 1)); mu <- matrix(c(.2, -.4, .1, .3), 2, 2)
  th <- list(kappa = 0, loading = c(.3, .2), omega = .2)
  tg <- list()
  for (k in 1:3) { cf <- c(k, 1) / (k + 1); ww <- rep(k, 2); tg[[k]] <- .rum_tgt_choice_set_pref("c", D, cf, w = ww) }
  for (k in 1:3) {
    ref <- .rum_tgt_choice_set_pref("c", D, c(k, 1) / (k + 1), w = rep(k, 2))
    expect_equal(tg[[k]]$m(mu, th), ref$m(mu, th))
    expect_equal(tg[[k]]$w, rep(k, 2))
  }
  expect_false(isTRUE(all.equal(tg[[1]]$m(mu, th), tg[[3]]$m(mu, th))))
  tg2 <- list()
  for (k in 1:2) { cf <- c(k, 1) / (k + 1); tg2[[k]] <- .rum_tgt_choice_set("c", D, cf) }
  expect_equal(tg2[[1]]$m(mu, th), .rum_tgt_choice_set("c", D, c(.5, .5))$m(mu, th))
})

test_that("sc_select_rank applies the paper's rule and stops at the first failure", {
  set.seed(5); n <- 400
  h1 <- rnorm(n, -5)
  mk <- function(h, gain) h + gain + rnorm(n, 0, .1)
  h2 <- mk(h1, .05); h3 <- mk(h2, 0); h4 <- mk(h3, .05)       # z large, ~0, large
  tab <- sc_select_rank(list(`1` = h1, `2` = h2, `3` = h3, `4` = h4))
  expect_identical(tab$q[tab$selected], 2L)
  expect_true(tab$z[4] > 2)                                     # q = 4 would move, but the rule has stopped
  d <- h2 - h1
  expect_equal(tab$z[2], mean(d) / (sd(d) / sqrt(n)))
  expect_identical(sc_select_rank(list(`1` = h1, `2` = h1 - .1))$q[sc_select_rank(list(`1` = h1, `2` = h1 - .1))$selected], 1L)
  expect_identical(sc_select_rank(list(`2` = h2, `1` = h1))$q, c(1L, 2L))
  expect_error(sc_select_rank(list(h1, h2)), "name the score vectors")
})

test_that("sc_save and sc_load round-trip the fold states", {
  st <- sc_test_states(q = 2L)
  f <- tempfile(fileext = ".rds"); on.exit(unlink(f))
  sc_save(st, f)
  st2 <- sc_load(f)
  expect_s3_class(st2, "sc_states")
  for (k in names(st$folds)) for (v in c("te", "mu", "th", "output_score", "common_score", "parts"))
    expect_identical(unname(st2$folds[[k]][[v]]), unname(st$folds[[k]][[v]]))
  expect_identical(dimnames(st2$folds[[1]]$mu), dimnames(st$folds[[1]]$mu))
  expect_identical(st2$data$data$d, st$data$data$d)
  expect_identical(st2$data$data$y, st$data$data$y)
  expect_identical(st2$data$groups, st$data$groups)
  expect_identical(st2$data$fold, st$data$fold)
  tl <- list(sc_mean("a", "f1"), sc_share("s", "f2", w = "g"))
  expect_identical(sc_estimate(st2, tl)$estimate, sc_estimate(st, tl)$estimate)
  z <- readRDS(f); z$folds[[1]]$mu[1, 1] <- z$folds[[1]]$mu[1, 1] + 1; saveRDS(z, f)
  expect_error(sc_load(f), "checksum")
  z$format_version <- 99L; saveRDS(z, f)
  expect_error(sc_load(f, check = FALSE), "format version")
  expect_message(expect_null(sc_load(tempfile())), "no file")
})

test_that("sc_data checks its inputs", {
  X <- matrix(c(1, 0, -1, 1, 0, 1), 3, 2, dimnames = list(NULL, c("a", "b")))
  x <- sc_data(c(1, 0, 1), X, id = c("r1", "r1", "r2"), Z = matrix(1:2, 2, 1))
  expect_s3_class(x, "sc_data")
  expect_identical(x$data$ids, c("r1", "r2")); expect_identical(rownames(x$Z), c("r1", "r2"))
  expect_output(print(x), "2 respondents, 3 tasks")
  expect_error(sc_data(c(1, 0), X, id = 1:3), "one entry per task")
  expect_error(sc_data(c(1, 0, 1), X, id = c(1, 1, 2), fold = 1), "fold")
  expect_error(sc_data(c(1, 0, 1), X, id = c(1, 1, 2), design = "sw"))
})
