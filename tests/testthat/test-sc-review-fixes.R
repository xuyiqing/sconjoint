# Regression tests for the outside review of the sc_* layer (2026-11-25, codex-review-1, items 1-15).

# Toy BR data with its 16 feature columns reversed (same tasks, other column order).
sc_reversed_br <- function(n = 12L) {
  x <- .sc_toy_data(n = n); d <- x$data
  X <- d$d[, rev(seq_len(d$p))]; colnames(X) <- rev(d$feature_names)
  list(x = x, y = d$y, X = X, id = rep(d$ids, d$T))
}

test_that("1: built-in designs require the design's feature order", {
  r <- sc_reversed_br()
  expect_error(sc_data(r$y, r$X, r$id, Z = r$x$Z, groups = r$x$groups, fold = r$x$fold, design = "br"),
               "fixed order.*same features in another order")
  expect_error(sc_data(r$y, r$X[, -1], r$id, design = "br"), "the data have 15 features, the design 16")
  xr <- sc_data(r$y, r$X, r$id, Z = r$x$Z, groups = r$x$groups, fold = r$x$fold)   # no design: accepted
  expect_null(xr$design)
  # states and design-generated targets check again (the design can be set after sc_data())
  xr$design <- "br"
  fake <- structure(list(data = xr, q = 1L, folds = list()), class = "sc_fit")
  expect_error(sc_states(fake), "fixed order")
  ctx <- .sc_context(xr)
  expect_error(sc_ame_switch("a", "r1", 2, 1, design = "br")$build(ctx), "fixed order")
  expect_error(sc_br_plan_contest("p", c(0, 5, 15, 25, 35, 45), rep(15, 6))$build(ctx), "not by the data's features")
  prof <- list(A = data.frame(tax_rate_1_pct = 5, tax_rate_2_pct = 15, tax_rate_3_pct = 25, tax_rate_4_pct = 25,
                              tax_rate_5_pct = 35, tax_rate_6_pct = 45, taxrev = 3),
               B = data.frame(tax_rate_1_pct = 0, tax_rate_2_pct = 5, tax_rate_3_pct = 15, tax_rate_4_pct = 25,
                              tax_rate_5_pct = 35, tax_rate_6_pct = 55, taxrev = 2))
  expect_error(sc_br_endpoint_ame("e", 6, prof)$build(ctx), "not by the data's features")
  # the design's own order is accepted, and so are unnamed contest rows
  ok <- .sc_context(r$x)
  expect_length(sc_br_plan_contest("p", c(0, 5, 15, 25, 35, 45), rep(15, 6))$build(ok), 1L)
  expect_length(sc_contest("c", matrix(0.1, 1, 16))$build(ctx), 1L)
})

test_that("2: ratio and difference components with a reused name must have the same definition", {
  st <- sc_test_states()
  expect_error(sc_estimate(st, list(sc_mean("x", "f1"), sc_difference("d", sc_mean("x", "f2"), sc_mean("y", "f1"))),
                           bounded = FALSE), "component named 'x' that differs")
  expect_error(sc_estimate(st, sc_difference("d", sc_mean("x", "f1"), sc_mean("x", "f2")), bounded = FALSE),
               "component named 'x'")
  expect_error(sc_estimate(st, list(sc_mean("x", "f1", w = "g"), sc_ratio("r", sc_mean("x", "f1"), sc_mean("y", "f2"))),
                           bounded = FALSE), "component named 'x'")
  # the same definition under the same name is reused (as the paper's target lists do)
  e <- sc_estimate(st, list(sc_mean("x", "f2"), sc_mean("y", "f1"),
                            sc_difference("d", sc_mean("x", c(f2 = 1)), sc_mean("y", "f1"))), bounded = FALSE)
  expect_equal(e$estimate[3], e$estimate[1] - e$estimate[2], tolerance = 1e-14)
  e2 <- sc_estimate(st, sc_difference("d", sc_mean("x", "f2"), sc_mean("y", "f1")), bounded = FALSE)
  expect_identical(e2$estimate, e$estimate[3]); expect_identical(e2$se, e$se[3])
})

test_that("3: rank selection pairs scores by respondent and rejects unequal lengths", {
  expect_error(sc_select_rank(list("1" = c(-2, -1), "2" = c(-1.9, -.9, -1.8, -.8))), "different lengths")
  h1 <- c(a = -2, b = -1, c = -3, d = -2.5); h2 <- c(c = -2.9, a = -1.9, d = -2.2, b = -.95)
  tab <- sc_select_rank(list("1" = h1, "2" = h2))
  expect_equal(tab$diff_to_prev[2], mean(h2[names(h1)] - h1), tolerance = 1e-15)
  expect_identical(sc_select_rank(list("1" = h1, "2" = h2[names(h1)])), tab)
  expect_error(sc_select_rank(list("1" = h1, "2" = setNames(h2, c("a", "b", "c", "x")))), "same respondents")
  expect_error(sc_select_rank(list("1" = h1, "2" = unname(h2))), "or none")
  expect_error(sc_select_rank(list("1" = h1, "1" = h1)), "distinct number")
  # fits: scores aligned by respondent id even when the fits list respondents in different orders
  mk <- function(q, ids, h) {
    x <- list(data = list(n = length(ids), ids = ids))
    structure(list(data = x, q = q, folds = list(list(te = seq_along(ids), heldout = list(r = h))),
                   settings = list(heldout_rule = "r")), class = "sc_fit")
  }
  f1 <- mk(1L, names(h1), unname(h1)); f2 <- mk(2L, names(h2), unname(h2))
  expect_equal(sc_select_rank(list(f1, f2))$diff_to_prev[2], tab$diff_to_prev[2], tolerance = 1e-15)
  f3 <- mk(2L, c("a", "b", "c", "z"), unname(h2))
  expect_error(sc_select_rank(list(f1, f3)), "same respondents")
})

test_that("4: fitted-law summaries use the weights' magnitudes", {
  st <- sc_test_states(); n <- st$data$data$n
  l <- sc_fitted_law(st, "f1", w = seq_len(n)); s <- attr(l, "summary")
  ww <- match(l$id, st$data$data$ids); ww <- ww / sum(ww)
  expect_equal(l$weight, ww, tolerance = 1e-15)
  m <- sum(ww * l$mean)
  expect_equal(s[["mean"]], m, tolerance = 1e-14)
  expect_equal(s[["sd"]], sqrt(sum(ww * l$sd^2) + sum(ww * (l$mean - m)^2)), tolerance = 1e-14)
  expect_equal(s[["share_positive"]], sum(ww * stats::pnorm(l$mean / l$sd)), tolerance = 1e-14)
  expect_output(print(l), "\\(weighted\\)")
  # 0/1 weights and no weights: equal weights, plain means (unchanged summaries)
  l0 <- sc_fitted_law(st, "f1", w = "g"); s0 <- attr(l0, "summary")
  expect_identical(s0[["mean"]], mean(l0$mean))
  expect_identical(nrow(l0), sum(st$data$groups$g > 0))
  la <- sc_fitted_law(st, "f1")
  expect_identical(attr(la, "summary")[["mean"]], mean(la$mean)); expect_equal(sum(la$weight), 1)
  expect_output(print(la), "over 30 held-out respondents: mean")
})

test_that("6: fitted laws skip folds without subgroup members", {
  st <- sc_test_states()
  l <- sc_fitted_law(st, "f1", w = as.numeric(st$data$fold == 1))
  expect_identical(nrow(l), sum(st$data$fold == 1)); expect_true(all(l$fold == 1L))
  one <- which(st$data$fold == 2)[1]
  l1 <- sc_fitted_law(st, "f1", w = as.numeric(seq_len(st$data$data$n) == one))
  expect_identical(l1$id, st$data$data$ids[one]); expect_identical(l1$fold, 2L)
})

test_that("5: per-respondent histories follow their respondents across folds", {
  st <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint")); dat <- st$data$data
  ids <- dat$ids[c(st$folds[[2]]$te[1], st$folds[[1]]$te[1], st$folds[[2]]$te[2])]
  h <- c(0L, 3L, 5L)
  pp <- sc_posterior(st, respondents = ids, history = h, nodes = 256)
  expect_identical(pp$diagnostics$id, ids)
  for (k in seq_along(ids)) {
    one <- sc_posterior(st, respondents = ids[k], history = h[k], nodes = 256)
    expect_equal(pp$beta[pp$beta$id == ids[k], "mean"], one$beta$mean, tolerance = 0)
  }
  expect_error(sc_posterior(st, respondents = ids, history = c(1, 2)), "one per selected respondent")
})

test_that("7: zero and degenerate share contrasts give no NaN", {
  st <- sc_test_states()
  expect_error(sc_estimate(st, sc_share("zero", rep(0, 3)), bounded = FALSE), "is zero")
  expect_error(sc_estimate(st, sc_share("zero", c(f2 = 0)), bounded = FALSE), "is zero")
  s0 <- attr(sc_fitted_law(st, c(0, 0, 0)), "summary")
  expect_identical(unname(s0), c(0, 0, 0))
  # a contrast the factor does not load on: point mass at c' mu, share = 1(c' mu > 0)
  th <- list(kappa = 0, loading = c(1, 0, 0), omega = .3)
  mu <- matrix(c(.5, -.2, .3, .1, -.4, .2), 2, 3)
  tg <- .rum_tgt_share("s", c(0, 1, 0), NULL)
  expect_identical(tg$m(mu, th), as.numeric(mu[, 2] > 0))
  expect_true(all(is.finite(unlist(.rum_target_grad(tg, mu, th)))))
  # nondegenerate shares are unchanged
  th$loading <- c(.4, .3, -.2); cc <- c(1, -1, .5)
  expect_identical(.rum_tgt_share("s", cc)$m(mu, th), stats::pnorm(drop(mu %*% cc) / abs(sum(th$loading * cc))))
})

test_that("8: validation sets are matched by fold label or by position over sorted labels", {
  skip_on_cran()
  x <- .sc_toy_data(n = 24L); x$fold <- ifelse(x$fold == 1L, 10L, 20L); ids <- x$data$ids
  v10 <- ids[x$fold == 20L][1:3]; v20 <- ids[x$fold == 10L][1:3]
  fit <- function(x) sc_fit(x, q = 1L, hidden = 3L, seeds = 1L, control = list(max_epochs = 3L), nodes = 32L,
                            check_nodes = 64L, heldout_nodes = 64L, verbose = FALSE)
  x$validation <- list("20" = v20, "10" = v10)
  f <- fit(x)
  expect_setequal(f$folds[["10"]]$validation, v10); expect_setequal(f$folds[["20"]]$validation, v20)
  x$validation <- list(v10, v20)
  f <- fit(x)
  expect_setequal(f$folds[["10"]]$validation, v10); expect_setequal(f$folds[["20"]]$validation, v20)
  expect_identical(.sc_fold_validation(list(v10), c(10L, 20L), 20L), NULL)   # not given: drawn
  x$validation <- list("1" = v10, "2" = v20)
  expect_error(fit(x), "not distinct fold labels: 1, 2")
  x$validation <- list(v10, v20, v10)
  expect_error(fit(x), "3 elements but the data have 2 folds")
})

test_that("9: save/load keeps the mean basis, mean names and metadata", {
  st <- sc_test_states(); d <- st$data$data
  rec <- function(v) lapply(seq_len(d$n), function(i) v(d$offsets[i] + seq_len(d$T[i])))
  rd <- rum_data(rec(function(r) d$y[r]), rec(function(r) d$d[r, , drop = FALSE]),
                 mean_basis = matrix(c(1, 0, 0), 3, 1, dimnames = list(NULL, "first")), ids = d$ids,
                 feature_names = d$feature_names, metadata = list(orientation = "o", tag = "example"))
  s9 <- st; s9$data$data <- rd
  f <- tempfile(fileext = ".rds"); on.exit(unlink(f))
  sc_save(s9, f); back <- sc_load(f)$data$data
  expect_identical(back, rd)
  # default data round-trip exactly (up to the names of the task counts), without storing the identity basis
  d <- st$data$data; names(d$T) <- NULL
  sc_save(st, f); z <- readRDS(f)
  expect_null(z$data$mean_basis)
  expect_identical(sc_load(f)$data$data, d)
  # files written before these fields were stored still load, with the defaults
  z$data$mean_basis <- z$data$mean_names <- z$data$metadata <- NULL; saveRDS(z, f)
  expect_identical(sc_load(f)$data$data, d)
  # per-respondent bases are kept as a list
  rl <- rum_data(rec(function(r) d$y[r]), rec(function(r) d$d[r, , drop = FALSE]),
                 mean_basis = lapply(seq_len(d$n), function(i) matrix(c(1, 0, i), 3, 1)), ids = d$ids,
                 feature_names = d$feature_names, metadata = list(orientation = "o"))
  s9$data$data <- rl; sc_save(s9, f)
  expect_identical(sc_load(f)$data$data, rl)
})

test_that("10: sc_decisiveness() fixes best_worst when it is called", {
  st <- sc_test_states(); ctx <- .sc_context(st$data)
  bw <- FALSE; t1 <- sc_decisiveness(list(a = 1), pairs = list(a = c(2, 1)), best_worst = bw); bw <- TRUE
  expect_length(t1$build(ctx), 1L)
  t2 <- sc_decisiveness(list(a = 1), pairs = list(a = c(2, 1)), best_worst = bw)
  expect_length(t2$build(ctx), 2L)
})

test_that("11: partial fold states are refused for population quantities", {
  st <- sc_test_states()
  one <- st; one$folds <- one$folds[1]
  expect_error(sc_estimate(one, sc_mean("m", "f1")), "15 of 30 respondents are held out in no fold")
  expect_error(sc_fitted_law(one, "f1"), "held out in no fold")
  twice <- st; twice$folds[[2]]$te <- c(twice$folds[[2]]$te, twice$folds[[1]]$te[1])
  expect_error(sc_estimate(twice, sc_mean("m", "f1")), "1 in more than one")
  part <- st; part$info$partial <- TRUE
  expect_error(sc_estimate(part, sc_mean("m", "f1")), "for checks")
  expect_error(sc_fitted_law(part, "f1"), "for checks")
  expect_s3_class(sc_estimate(st, sc_mean("m", "f1")), "sc_estimates")
  # sc_states(respondents = ) still builds states for checks, which estimation then refuses
  skip_on_cran()
  toy <- .sc_toy_data(n = 16L)
  f <- sc_fit(toy, q = 1L, hidden = 3L, seeds = 1L, control = list(max_epochs = 3L), nodes = 32L,
              check_nodes = 64L, heldout_nodes = 64L, verbose = FALSE)
  s <- sc_states(f, M = 2L, ny = 1L, info_nodes = 32L, score_nodes = 32L, respondents = 2L)
  expect_identical(vapply(s$folds, function(x) length(x$te), 0L), c(`1` = 2L, `2` = 2L))
  expect_output(print(s), "for checks only")
  expect_error(sc_estimate(s, sc_mean("m", "tax6_linear")), "for checks")
  expect_s3_class(sc_posterior(s, respondents = s$folds[[1]]$te[1], nodes = 64), "sc_posterior")
  s1 <- sc_states(f, M = 2L, ny = 1L, info_nodes = 32L, score_nodes = 32L, folds = 1)
  expect_error(sc_fitted_law(s1, "tax6_linear"), "held out in no fold")
})

test_that("12: named posterior contrasts must use unique feature names", {
  st <- sc_test_states(); f <- st$folds[[1]]
  par <- list(mu = f$mu, kappa = .1, loading = f$th$loading, omega = .3)
  post <- function(cc) sc_posterior(par, st$data, respondents = 1:2, contrasts = cc, nodes = 64)$contrasts
  expect_error(post(c(typo = 1, f2 = 0)), "unknown: typo")
  expect_error(post(c(f1 = 1, f1 = 2)), "repeated: f1")
  expect_error(post(rbind(a = c(f1 = 1, zz = 1))), "unknown: zz")
  # valid names, unnamed vectors, and contrasts in columns give the same answer
  ref <- post(rbind(a = c(1, 0, -1)))
  expect_equal(post(rbind(a = c(f1 = 1, f3 = -1)))$mean, ref$mean, tolerance = 0)
  expect_equal(post(c(f3 = -1, f1 = 1))$mean, ref$mean, tolerance = 0)
  expect_equal(post(cbind(a = c(f1 = 1, f2 = 0, f3 = -1)))$mean, ref$mean, tolerance = 0)
  m3 <- matrix(c(1, 0, -1, 0, 1, 0, 1, 1, 1), 3, 3, dimnames = list(c("f1", "f2", "f3"), c("a", "b", "c")))
  expect_equal(post(m3)$mean[post(m3)$contrast == "a"], ref$mean, tolerance = 0)
  expect_equal(post(matrix(c(1, 0, -1), 3, 1))$mean, ref$mean, tolerance = 0)
  expect_equal(post(rbind(f1 = c(1, 0, -1)))$mean, ref$mean, tolerance = 0)   # a label that is a feature name
})

test_that("13: importance level weights need one nonnegative weight per menu level", {
  st <- sc_test_states(); ctx <- .sc_context(st$data)
  b <- function(...) sc_importance(list(a = 1, b = 2:3), ...)$build(ctx)
  expect_error(b(level_weights = list(a = 1, b = 1)), "'a' must be 2 finite nonnegative numbers")
  expect_error(b(level_weights = list(b = c(1, 1))), "'b' must be 3")
  expect_error(b(level_weights = list(a = c(1, -1))), "'a' must be 2")
  expect_error(b(level_weights = list(a = c(0, 0))), "positive sum")
  expect_error(b(level_weights = list(a = c(1, NA))), "'a' must be 2")
  expect_error(b(menus = list(a = matrix(c(0, Inf)))), "finite numbers")
  # valid weights: the same targets as equal weights when they are equal, finite shares otherwise
  e1 <- b(); e2 <- b(level_weights = list(a = c(2, 2), b = c(1, 1, 1)))
  mu <- st$folds[[1]]$mu; th <- st$folds[[1]]$th
  expect_equal(e2[[1]]$m(mu, th), e1[[1]]$m(mu, th), tolerance = 1e-14)
  e3 <- b(level_weights = list(a = c(1, 3)))
  expect_true(all(is.finite(e3[[1]]$m(mu, th))))
})
