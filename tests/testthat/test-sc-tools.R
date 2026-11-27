# sc_compare(), sc_spread(), parallel sc_estimate() and the compact print of estimate tables (0.4.3).

toy <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))

draw <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

test_that("sc_compare lines estimates up with the truth and reports coverage", {
  skip_if(is.null(toy))
  est <- sc_estimate(toy, list(sc_mean("top", "tax6_linear"), sc_mean("bottom", "tax1_linear"),
                               sc_importance(list(a = "tax1_linear", b = "tax6_linear"), names = c("ia", "ib"))))
  inside <- (est$lower[1] + est$upper[1]) / 2
  cmp <- sc_compare(est, c(top = inside, bottom = est$upper[2] + 1, ia = .5))
  expect_s3_class(cmp, "sc_compare")
  expect_equal(cmp$name, est$name)
  expect_equal(cmp$truth, c(inside, est$upper[2] + 1, .5, NA))
  expect_equal(cmp$error, est$estimate - cmp$truth)
  expect_equal(cmp$covered, c(TRUE, FALSE, NA, NA))           # importance: no interval; ib: no truth
  out <- capture.output(print(cmp))
  expect_true(any(grepl("1 of 2 intervals cover the truth; missed: bottom", out, fixed = TRUE)))
  # a truth data frame (as in simconjoint_truth$population)
  cmp2 <- sc_compare(est, data.frame(name = c("top", "bottom"), value = c(inside, 0)))
  expect_equal(cmp2$truth[1:2], c(inside, 0))
  # markdown
  md <- sc_compare(est, c(top = inside), format = "markdown", labels = c(top = "Top bracket"), digits = 2)
  expect_s3_class(md, "sc_table_text")
  expect_match(md, "| Target | Truth | Estimate | 95% lower | 95% upper | Covered |", fixed = TRUE)
  expect_match(md, "| Top bracket | ", fixed = TRUE)
  expect_match(md, "| yes |", fixed = TRUE)
  # plots take it as their reference
  pd <- draw(sc_plot_estimates(est[1:2, ], reference = cmp))
  expect_equal(pd$reference, cmp$truth[1:2])
  expect_error(sc_compare(est, c(nope = 1)), "no name in the reference")
  expect_error(sc_compare(est, c(top = 1, top = 2)), "twice")
  expect_error(sc_compare(est, 1:2), "named by target name")
  expect_error(sc_compare(est, data.frame(a = 1)), "name and value")
})

test_that("sc_compare compares two models with paired standard errors", {
  skip_if(is.null(toy))
  tg <- list(sc_mean("top", "tax6_linear"), sc_mean("bottom", "tax1_linear"), sc_share("share", "tax6_linear"),
             sc_importance(list(a = "tax1_linear", b = "tax6_linear"), names = c("ia", "ib")))
  est <- sc_estimate(toy, tg)
  I <- attr(est, "influence")
  expect_equal(rownames(I), toy$data$data$ids)
  # the same model: zero differences with zero paired SE
  same <- sc_compare(est, est)
  expect_equal(attr(same, "type"), "model")
  expect_equal(same$difference, rep(0, 5))
  expect_equal(same$se[1:3], rep(0, 3))
  expect_true(all(is.na(same$se[4:5])))                         # importance: no SE
  # a second "model" whose influence values are half and estimates shifted (rows in another order)
  other <- est; other$estimate <- other$estimate + .1
  o <- sample(nrow(I)); attr(other, "influence") <- (I / 2)[o, , drop = FALSE]
  cmp <- sc_compare(est, other)
  expect_true(all(cmp$paired[1:3]))
  expect_equal(cmp$difference[1:3], rep(-.1, 3))
  expect_equal(cmp$se[1:3], unname(apply(I[, 1:3] / 2, 2, stats::sd)) / sqrt(nrow(I)))
  expect_equal(cmp$lower, cmp$difference - 1.96 * cmp$se)
  out <- capture.output(print(cmp))
  expect_true(any(grepl("differences have a 95% interval that excludes zero", out, fixed = TRUE)))
  # subsetting rows keeps the influence values, so the pairing still works
  sub <- sc_compare(est[1:2, ], est[1:2, ])
  expect_true(all(sub$paired)); expect_equal(sub$se, c(0, 0))
  # without influence values: independent SEs, said in print
  bare <- est[1:2, ]; attr(bare, "influence") <- NULL
  ind <- sc_compare(bare, bare)
  expect_false(any(ind$paired))
  expect_equal(ind$se, sqrt(2) * est$se[1:2])
  expect_true(any(grepl("independent", capture.output(print(ind)))))
  # other respondents: independent
  attr(other, "influence") <- I; rownames(attr(other, "influence")) <- paste0("x", seq_len(nrow(I)))
  expect_false(any(sc_compare(est, other)$paired))
  # markdown and plots
  md <- sc_compare(est, est, format = "markdown")
  expect_match(md, "| Target | Estimate | Reference | Difference | 95% interval |", fixed = TRUE)
  pd <- draw(sc_plot_estimates(est[1:2, ], reference = sc_compare(est, other)))
  expect_equal(pd$reference, other$estimate[1:2])
  expect_error(sc_compare(est, est[est$name == "nope", ]), "no target of the reference")
})

test_that("sc_spread gives rotation-invariant conditional sds by fold and on average", {
  skip_if(is.null(toy))
  sp <- sc_spread(toy)
  fn <- toy$data$data$feature_names
  expect_equal(sp$feature, fn)
  k <- length(toy$folds)
  expect_equal(names(sp), c("feature", "sd", paste0("fold_", seq_len(k))))
  by_hand <- sapply(toy$folds, function(f) sqrt(rowSums(as.matrix(f$th$loading)^2)))
  expect_equal(unname(as.matrix(sp[, -(1:2)])), unname(by_hand))
  expect_equal(sp$sd, rowMeans(by_hand))
  # a rotation of the loading leaves the spread unchanged
  f1 <- toy$folds[[1]]; L <- as.matrix(f1$th$loading)
  if (ncol(L) > 1) {
    th <- .5; R <- diag(ncol(L)); R[1:2, 1:2] <- c(cos(th), sin(th), -sin(th), cos(th))
    expect_equal(sqrt(rowSums((L %*% R)^2)), sqrt(rowSums(L^2)))
  }
  expect_equal(sc_spread(toy, c("tax6_linear", "tax1_linear"))$feature, c("tax6_linear", "tax1_linear"))
  expect_error(sc_spread(toy, "nope"), "not coded features")
})

test_that("sc_estimate(cores = 2) gives the same table as one call", {
  skip_if(is.null(toy))
  skip_on_os("windows")
  tg <- list(sc_mean("top", "tax6_linear"), sc_share("top share", "tax6_linear"),
             sc_ratio("ratio", sc_mean("t6", "tax6_linear"), sc_mean("t1", "tax1_linear")),
             sc_importance(list(a = "tax1_linear", b = "tax6_linear"), names = c("ia", "ib")))
  one <- sc_estimate(toy, tg)
  par <- sc_estimate(toy, tg, cores = 2)
  expect_s3_class(par, "sc_estimates")
  expect_equal(as.data.frame(par)[, c("name", "estimate", "se", "lower", "upper", "n", "status")],
               as.data.frame(one)[, c("name", "estimate", "se", "lower", "upper", "n", "status")], tolerance = 1e-10)
  expect_equal(unname(attr(par, "influence")), unname(attr(one, "influence")[, colnames(attr(par, "influence"))]))
  expect_error(sc_estimate(toy, list(sc_mean("a", "tax6_linear"), sc_mean("a", "tax1_linear")), cores = 2), "named 'a'")
  expect_error(sc_estimate(toy, tg, cores = 0), "cores")
})

test_that("estimate tables print compactly, rounded to decimals", {
  skip_if(is.null(toy))
  est <- sc_estimate(toy, list(sc_mean("top", "tax6_linear"), sc_mean("bottom", "tax1_linear")))
  out <- capture.output(print(est))
  expect_match(out[1], "name +estimate +plug_in +se +lower +upper +n")
  expect_false(any(grepl("kind|interval|status|method", out[1])))
  expect_true(any(grepl(format(round(est$estimate[1], 3), nsmall = 3), out, fixed = TRUE)))
  all <- capture.output(print(est, all = TRUE))
  expect_match(all[1], "kind")
  expect_equal(est$estimate, as.data.frame(est)$estimate)          # printing does not round the table
})
