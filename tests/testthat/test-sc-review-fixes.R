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
