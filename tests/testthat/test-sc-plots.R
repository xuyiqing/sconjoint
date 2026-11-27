# Plotting and table functions on the toy fit: they draw without error or warning, return their
# plotting data, restore par(), and give clear argument errors.

toy <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))

# Draw on a null PDF device and return the function's value.
draw <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

if (!is.null(toy)) {
  fn <- toy$data$data$feature_names
  est <- sc_estimate(toy, list(sc_mean("top", "tax6_linear"), sc_mean("bottom", "tax1_linear"),
                               sc_share("top_share", "tax6_linear"), sc_share("bottom_share", "tax1_linear")),
                     bounded = FALSE)
  means <- est[1:2, ]
  shares <- est[3:4, ]
  shares$name <- means$name
  laws <- list(sc_fitted_law(toy, "tax6_linear"), sc_fitted_law(toy, "tax1_linear"))
  g <- list(low = fn[1:4], middle = fn[5:8], high = fn[9:12], revenue = fn[13:16])
  imp_all <- sc_estimate(toy, sc_importance(g, names = paste("All", names(g), sep = ".")))
  imp_dem <- sc_estimate(toy, sc_importance(g, w = function(x) x$party == "Dem", names = paste("Dem", names(g), sep = ".")))
  d <- lapply(1:3, function(s) stats::setNames(s * (fn == "tax6_linear"), fn))
  con_all <- sc_estimate(toy, lapply(1:3, function(s) sc_contest(paste("c", s), d[[s]])))
  con_dem <- sc_estimate(toy, lapply(1:3, function(s) sc_contest(paste("c", s), d[[s]], w = function(x) x$party == "Dem")))
}

test_that("sc_plot_estimates draws one or several tables and returns its data", {
  skip_if(is.null(toy))
  mar0 <- NULL
  pd <- draw({
    mar0 <- graphics::par("mar")
    out <- sc_plot_estimates(est[1:2, ], labels = c(top = "Top bracket", bottom = "Bottom bracket"))
    expect_equal(graphics::par("mar"), mar0)
    out
  })
  expect_s3_class(pd, "data.frame")
  expect_equal(pd$name, c("top", "bottom"))
  expect_equal(pd$label, c("Top bracket", "Bottom bracket"))
  expect_equal(pd$estimate, est$estimate[1:2])
  expect_true(all(pd$shown))
  expect_no_warning(pd2 <- draw(sc_plot_estimates(First = est, Second = est[4:1, ], scale = 100, hide_wide = TRUE,
                                                  reference = c(top = .1, top_share = .5), main = "Toy")))
  expect_equal(nrow(pd2), 8L)
  expect_equal(unique(pd2$group), c("First", "Second"))
  expect_equal(pd2$name[1:4], est$name)                   # order of the first table
  expect_true(all(pd2$y[1:4] > pd2$y[5:8]))               # first group above the second in each row
  expect_equal(pd2$estimate[1:4], 100 * est$estimate)
  wide <- (est$upper - est$lower) >= .95
  expect_equal(pd2$shown[1:4], !wide & is.finite(est$lower) & is.finite(est$upper))
  expect_equal(pd2$reference[1], 10)
  expect_no_warning(draw(sc_plot_estimates(est, order = c("bottom", "top"), reference = c(top = 1),
                                           reference_style = "circle", reference_label = "Truth")))
  expect_no_warning(draw(plot(est)))
  expect_error(sc_plot_estimates(est, est), "name every table")
  expect_error(sc_plot_estimates(est, labels = c("a", "b")), "labels has 2 entries")
  expect_error(sc_plot_estimates(data.frame(x = 1)), "not an estimate table")
  expect_error(sc_plot_estimates(est, order = "nope"), "no table has")
  expect_error(sc_plot_estimates(est, scale = -1), "scale")
  expect_error(sc_plot_estimates(est, reference = 1), "named numeric")
})

test_that("sc_plot_laws draws ridges and panels", {
  skip_if(is.null(toy))
  r <- NULL
  expect_no_warning(r <- draw(sc_plot_laws(laws, labels = c("Top", "Bottom"), main = "Laws")))
  expect_equal(nrow(r), 2L * 150L)
  expect_equal(unique(r$label), c("Top", "Bottom"))
  expect_true(all(is.na(r$truth)))
  truth <- list(function(x) stats::dnorm(x, -1, 1.5), NULL)
  expect_no_warning(p <- draw(sc_plot_laws(laws, means = means, layout = "panels", truth = truth, ncol = 2)))
  expect_equal(nrow(p), 300L)
  expect_true(all(is.finite(p$truth[p$law == 1])))
  expect_true(all(is.na(p$truth[p$law == 2])))
  # the density integrates to about the mass inside the panel
  bw <- diff(p$x[1:2])
  expect_lt(abs(sum(p$density[p$law == 1]) * bw - 1), .02)
  expect_no_warning(draw(sc_plot_laws(laws[[1]], truth = function(x) stats::dnorm(x), xlim = c(-6, 4))))
  expect_no_warning(draw(sc_plot_laws(laws, means = means)))     # means also mark ridges (0.4.3)
  expect_error(sc_plot_laws(laws, means = means[1, ], layout = "panels"), "one mean per law")
  expect_error(sc_plot_laws(list(data.frame(a = 1))), "not a table from sc_fitted_law")
  expect_error(sc_plot_laws(laws, truth = list(function(x) x)), "density functions")
})

test_that("sc_plot_preferences draws the four panels", {
  skip_if(is.null(toy))
  out <- NULL
  expect_no_warning(out <- draw(sc_plot_preferences(means, laws, shares, ames = shares, reference = c(top = .2),
                                                    labels = c("Top", "Bottom"))))
  expect_named(out, c("rows", "xlim", "ticks"))
  expect_equal(names(out$xlim), c("A", "B", "C", "D"))
  expect_equal(out$rows$mean, means$estimate)
  expect_equal(out$rows$share, 100 * shares$estimate)
  expect_equal(out$rows$reference, c(20, NA))
  expect_equal(out$ticks$C, c(0, 50, 100))
  # tables matched by position when the names differ, laws by name, limits overridden
  sh2 <- shares; sh2$name <- c("x", "y")
  o2 <- draw(sc_plot_preferences(means, stats::setNames(laws, means$name), sh2, xlim = list(A = c(-20, 20)),
                                 ticks = list(B = c(-5, 0, 5))))
  expect_equal(o2$xlim$A, c(-20, 20))
  expect_equal(o2$ticks$B, c(-5, 0, 5))
  expect_false("D" %in% names(o2$xlim))
  expect_error(sc_plot_preferences(means, laws[1], shares), "laws for 2 rows")
  expect_error(sc_plot_preferences(means, laws, shares, reference = c(top = 1)), "needs ames")
  expect_error(sc_plot_preferences(means, laws, est[2:4, ]), "lacks target names")
  expect_error(sc_plot_preferences(means, laws, shares, xlim = list(E = c(0, 1))), "named by panel")
  expect_error(sc_plot_preferences(means, laws, shares, widths = 1:2), "widths needs 4")
})

test_that("sc_plot_importance draws grouped bars", {
  skip_if(is.null(toy))
  expect_equal(sconjoint:::.scp_attribute(c("All.party", "All.econ")), c("party", "econ"))
  expect_equal(sconjoint:::.scp_attribute(c("All.party", "Dem.econ")), c("All.party", "Dem.econ"))
  expect_equal(sconjoint:::.scp_attribute(c("party", "econ")), c("party", "econ"))
  out <- NULL
  expect_no_warning(out <- draw(sc_plot_importance(All = imp_all, Democrats = imp_dem)))
  expect_equal(nrow(out), 8L)
  expect_equal(out$attribute[1:4], names(g))
  expect_equal(out$share[1:4], 100 * imp_all$estimate)
  expect_equal(out$label[1], "Low")
  comb <- list(brackets = c("low", "middle", "high"), revenue = "revenue")
  expect_no_warning(o2 <- draw(sc_plot_importance(All = imp_all, Democrats = imp_dem, combine = comb,
                                                  labels = c("Tax brackets", "Revenue"))))
  expect_equal(o2$share[o2$group == "All"], 100 * c(sum(imp_all$estimate[1:3]), imp_all$estimate[4]))
  expect_equal(sum(o2$share[o2$group == "Democrats"]), 100, tolerance = 1e-6)
  expect_warning(draw(sc_plot_importance(imp_all[1:2, ])), "sum to")
  expect_error(sc_plot_importance(imp_all, combine = list(a = "low")), "in no group")
  expect_error(sc_plot_importance(imp_all, combine = list(a = c("low", "nope"))), "lack")
  expect_error(sc_plot_importance(est), "not importance shares")
  expect_error(sc_plot_importance(imp_all, imp_dem), "name every table")
})

test_that("sc_plot_lines draws series along a setting", {
  skip_if(is.null(toy))
  out <- NULL
  expect_no_warning(out <- draw(sc_plot_lines(All = con_all, Democrats = con_dem, x = 1:3, scale = 100,
                                              xlab = "Top-rate increase", ylab = "Expected support (%)",
                                              main = "Toy", xlabels = c("+1", "+2", "+3"))))
  expect_equal(nrow(out), 6L)
  expect_equal(out$estimate[1:3], 100 * con_all$estimate)
  expect_equal(unique(out$series), c("All", "Democrats"))
  expect_no_warning(draw(sc_plot_lines(est[1:2, ], x = c(2, 1), xlab = "Setting", legend = FALSE)))
  expect_error(sc_plot_lines(con_all, x = 1:2, xlab = "a"), "3 rows for 2 values")
  expect_error(sc_plot_lines(con_all, x = 1:3), "xlab")
  expect_error(sc_plot_lines(con_all, con_dem, x = 1:3, xlab = "a"), "name every table")
})

test_that("sc_plot_posterior draws a scatter against truth or a caterpillar", {
  skip_if(is.null(toy))
  cm <- rbind(top = stats::setNames(as.numeric(fn == "tax6_linear"), fn),
              bottom = stats::setNames(as.numeric(fn == "tax1_linear"), fn))
  post <- sc_posterior(toy, contrasts = cm, nodes = 256)
  truth <- stats::setNames(seq_len(nrow(post$contrasts) / 2), unique(post$contrasts$id))
  out <- NULL
  expect_no_warning(out <- draw(sc_plot_posterior(post, truth = truth, which = "bottom")))
  expect_equal(nrow(out), length(truth))
  expect_equal(out$mean, post$contrasts$mean[post$contrasts$contrast == "bottom"])
  expect_equal(out$truth, unname(truth))
  expect_no_warning(cat_out <- draw(sc_plot_posterior(post)))
  expect_equal(cat_out$mean, sort(post$contrasts$mean[post$contrasts$contrast == "top"]))
  expect_equal(cat_out$rank, seq_along(truth))
  p2 <- sc_posterior(toy, respondents = 1:5, nodes = 256)
  expect_no_warning(b <- draw(sc_plot_posterior(p2, which = "tax2_linear")))
  expect_equal(nrow(b), 5L)
  expect_no_warning(draw(sc_plot_posterior(post$contrasts[post$contrasts$contrast == "top", ], truth = unname(truth))))
  expect_error(sc_plot_posterior(post, which = "nope"), "not among")
  expect_error(sc_plot_posterior(post, truth = 1:3), "name it by respondent ID")
  expect_error(sc_plot_posterior(list(a = 1)), "output of sc_posterior")
})

test_that("sc_table formats cells, markdown and LaTeX", {
  skip_if(is.null(toy))
  tb <- sc_table(est)
  expect_s3_class(tb, "data.frame")
  expect_named(tb, c("Target", "Estimate"))
  e <- est[1, ]
  expect_equal(tb$Estimate[1], sprintf("%.2f [%.2f, %.2f]", e$estimate, e$lower, e$upper))
  tse <- sc_table(est, style = "se", digits = 3, scale = 100)
  expect_equal(tse$Estimate[1], sprintf("%.3f (%.3f)", 100 * e$estimate, 100 * e$se))
  # importance: no interval, estimate alone; a target a column lacks gives an empty cell
  ti <- sc_table(Means = est[1:2, ], Importance = imp_all, labels = c(top = "Top_rate"), n = TRUE)
  expect_equal(ti$Importance[3], sprintf("%.2f", imp_all$estimate[1]))
  expect_equal(ti$Means[3], "")
  expect_equal(ti$Target[1], "Top_rate")
  expect_equal(ti$Target[nrow(ti)], "Respondents")
  expect_equal(ti$Means[nrow(ti)], "40")
  expect_equal(sc_table(imp_all, style = "se")$Estimate[1], sprintf("%.2f", imp_all$estimate[1]))
  md <- sc_table(Means = est[1:2, ], Importance = imp_all, format = "markdown", n = TRUE, caption = "Toy")
  expect_s3_class(md, "sc_table_text")
  ln <- strsplit(md, "\n")[[1]]
  expect_equal(sum(startsWith(ln, "|")), 2L + nrow(ti))    # header, separator, rows
  expect_equal(ln[2], "|:---|---:|---:|")
  expect_true("Table: Toy" %in% ln)
  expect_output(print(md), "| Target | Means | Importance |", fixed = TRUE)
  tx <- sc_table(est, format = "latex", labels = c(top = "Top_rate & 50%"), caption = "A_b")
  expect_match(tx, "\\toprule", fixed = TRUE)
  expect_match(tx, "Top\\_rate \\& 50\\%", fixed = TRUE)
  expect_match(tx, "\\caption{A\\_b}", fixed = TRUE)
  expect_match(tx, "bottom\\_share", fixed = TRUE)
  expect_false(grepl("[^\\\\]_", tx))
  expect_match(sc_table(est, format = "latex", labels = c(top = "Below $10k")), "Below \\$10k", fixed = TRUE)
  expect_error(sc_table(est, digits = -1), "digits")
  expect_error(sc_table(est, format = "html"))
  expect_error(sc_table(A = est, A = est), "column names must differ")
})

# Regression tests for the review of the plotting code (2026-11-26). They use small hand-made
# tables, so they need no fit.

# Draw with the display list on and return the graphics calls made on the last page: a list of
# (name, args), where name is the C routine (e.g. "C_segments") and args its arguments.
drawn <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  force(expr)
  lapply(grDevices::recordPlot()[[1L]], function(e) {
    f <- e[[2L]][[1L]]
    list(name = if (is.list(f) && !is.null(f$name)) f$name else "", args = as.list(e[[2L]])[-1L])
  })
}
calls_named <- function(calls, name) Filter(function(cl) identical(cl$name, name), calls)

test_that("sc_plot_preferences reorders unnamed laws with the rows", {
  m <- data.frame(name = c("a", "b"), estimate = c(-2, 2))
  s <- data.frame(name = c("a", "b"), estimate = c(.2, .8))
  laws <- lapply(c(-2, 2), function(mu) data.frame(mean = mu, sd = 1, weight = 1))
  seen <- numeric(0)
  ridge <- sconjoint:::.scp_ridge
  local_mocked_bindings(.scp_ridge = function(law, y, ...) {
    seen[y] <<- law$mean[1L]
    ridge(law, y, ...)
  })
  # order permutes the rows without dropping any: row b (on top) must get b's law
  out <- draw(sc_plot_preferences(m, laws, s, order = c("b", "a")))
  expect_equal(out$rows$name, c("b", "a"))
  expect_equal(seen[out$rows$y], out$rows$mean)
  # order drops a row: laws in the order of means are still matched to their rows
  m3 <- data.frame(name = c("a", "b", "c"), estimate = c(-2, 2, 4))
  laws3 <- lapply(m3$estimate, function(mu) data.frame(mean = mu, sd = 1, weight = 1))
  seen <- numeric(0)
  out3 <- draw(sc_plot_preferences(m3, laws3, m3, order = c("c", "a")))
  expect_equal(seen[out3$rows$y], out3$rows$mean)
  # named laws are matched by name, whatever their order
  seen <- numeric(0)
  out4 <- draw(sc_plot_preferences(m, stats::setNames(laws[2:1], c("b", "a")), s, order = c("b", "a")))
  expect_equal(seen[out4$rows$y], out4$rows$mean)
})

test_that("tables with the same name stay separate groups", {
  a <- data.frame(name = c("a", "b"), estimate = c(1, 2))
  b <- data.frame(name = c("a", "b"), estimate = c(3, 4))
  # sc_plot_lines: one line per table, each with its own values
  cl <- drawn(sc_plot_lines(Model = a, Model = b, x = 1:2, xlab = "Setting"))
  ys <- lapply(calls_named(cl, "C_plotXY"), function(z) z$args[[1L]]$y)
  expect_true(any(vapply(ys, identical, TRUE, c(1, 2))))
  expect_true(any(vapply(ys, identical, TRUE, c(3, 4))))
  pd <- draw(sc_plot_lines(Model = a, Model = b, x = 1:2, xlab = "Setting"))
  expect_equal(pd$estimate, c(1, 2, 3, 4))
  # sc_plot_estimates: each group draws only its own points
  ce <- drawn(sc_plot_estimates(Model = a, Model = b))
  xs <- lapply(calls_named(ce, "C_plotXY"), function(z) z$args[[1L]]$x)
  expect_true(any(vapply(xs, identical, TRUE, c(1, 2))))
  expect_true(any(vapply(xs, identical, TRUE, c(3, 4))))
  expect_false(any(vapply(xs, function(v) length(v) == 4L, TRUE)))   # no group draws all four
  pe <- draw(sc_plot_estimates(Model = a, Model = b))
  expect_equal(nrow(pe), 4L)
  expect_true(all(pe$y[1:2] > pe$y[3:4]))
})

test_that("sc_plot_lines sets probability limits and the reference line from scale", {
  e <- data.frame(name = c("a", "b"), kind = "choice", estimate = c(.4, .6))
  usr <- NULL
  cl <- drawn({
    sc_plot_lines(e, x = 1:2, xlab = "Setting", scale = 10)
    usr <- graphics::par("usr")
  })
  expect_equal(usr[3:4], c(0, 10))
  dashed <- Filter(function(z) identical(z$args[[7L]], 2), calls_named(cl, "C_abline"))   # lty = 2
  expect_length(dashed, 1L)
  expect_equal(dashed[[1L]]$args[[3L]], 5)            # h = 0.5 * scale
  usr1 <- NULL
  drawn({
    sc_plot_lines(e, x = 1:2, xlab = "Setting")
    usr1 <- graphics::par("usr")
  })
  expect_equal(usr1[3:4], c(0, 1))
})

test_that("a point-mass law (sd 0) is drawn as a bar at its value", {
  law <- data.frame(mean = 0, sd = 0, weight = 1)
  for (lay in c("panels", "ridges")) {
    p <- NULL
    expect_no_warning(p <- draw(sc_plot_laws(law, layout = lay)))
    bw <- diff(p$x[1:2])
    expect_true(all(is.finite(p$density)))
    expect_true(min(p$x) < -10 * bw && max(p$x) > 10 * bw)           # limits padded around 0
    expect_equal(sum(p$density) * bw, 1, tolerance = 1e-6)
    expect_lt(abs(sum(p$x * p$density) / sum(p$density)), bw)         # centred on 0
    expect_gt(sum(sort(p$density, decreasing = TRUE)[1:3]) / sum(p$density), .99)   # a narrow bar
  }
  # a mixture with a point mass and a normal component
  mix <- data.frame(mean = c(0, 2), sd = c(0, .5), weight = c(1, 1))
  for (lay in c("panels", "ridges")) {
    p <- NULL
    expect_no_warning(p <- draw(sc_plot_laws(list(mix, law), layout = lay)))
    expect_true(all(is.finite(p$density)))
    bw <- diff(p$x[1:2])
    expect_equal(sum(p$density[p$law == 1]) * bw, 1, tolerance = .01)
  }
  # Panel B of sc_plot_preferences
  o <- NULL
  expect_no_warning(o <- draw(sc_plot_preferences(data.frame(name = "a", estimate = 0), list(law),
                                                  data.frame(name = "a", estimate = .5))))
  expect_true(o$xlim$B[1L] < 0 && o$xlim$B[2L] > 0)
  expect_error(sc_plot_laws(data.frame(mean = 0, sd = -1, weight = 1)), "non-negative")
})

test_that("sc_plot_lines draws a lone interval as a vertical line", {
  e <- data.frame(name = letters[1:3], estimate = c(1, 2, 3), lower = c(NA, 1, NA), upper = c(NA, 3, NA))
  cl <- drawn(sc_plot_lines(e, x = 1:3, xlab = "Setting"))
  seg <- calls_named(cl, "C_segments")
  expect_length(seg, 1L)
  expect_equal(unname(unlist(seg[[1L]]$args[1:4])), c(2, 1, 2, 3))
  expect_length(calls_named(cl, "C_polygon"), 0L)
  # a single row
  c1 <- NULL
  expect_no_warning(c1 <- drawn(sc_plot_lines(e[2, ], x = 5, xlab = "Setting")))
  expect_equal(unname(unlist(calls_named(c1, "C_segments")[[1L]]$args[1:4])), c(5, 1, 5, 3))
  # two neighbouring intervals still make a band, the lone one a line
  e2 <- e; e2$lower <- c(0, 1, NA); e2$upper <- c(2, 3, NA)
  e2 <- rbind(e2, data.frame(name = "d", estimate = 4, lower = 3, upper = 5))
  c2 <- drawn(sc_plot_lines(e2, x = 1:4, xlab = "Setting"))
  expect_length(calls_named(c2, "C_polygon"), 1L)
  expect_length(calls_named(c2, "C_segments"), 1L)
})

test_that("the page-filling plots end with one figure per page and restore other settings", {
  law <- data.frame(mean = 0, sd = 1, weight = 1)
  tab <- data.frame(name = "a", estimate = 0, lower = -1, upper = 1)
  for (f in list(function() sc_plot_laws(law, layout = "panels"),
                 function() sc_plot_preferences(tab, list(law), tab))) {
    grDevices::pdf(NULL)
    graphics::layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE), widths = c(1, 3))
    graphics::par(mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0), las = 0)
    keep <- graphics::par("mar", "oma", "las", "cex")
    f()
    expect_equal(graphics::par("mfrow"), c(1L, 1L))
    expect_equal(graphics::par("mar", "oma", "las", "cex"), keep)
    graphics::plot(1)
    expect_equal(graphics::par("mfg"), c(1L, 1L, 1L, 1L))
    grDevices::dev.off()
  }
})

test_that("a panel's limits include the corrected mean and its interval", {
  law <- data.frame(mean = 0, sd = 1, weight = 1)
  m <- data.frame(name = "a", estimate = 5, lower = 4.5, upper = 5.5)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  d <- sc_plot_laws(law, means = m, layout = "panels")
  expect_gte(max(d$x), 5.5)
})

test_that("groups add grey labels, gaps between groups and grouped plotting data (0.4.3)", {
  skip_if(is.null(toy))
  # estimates: rows of a group sit one unit apart, groups 1.6 apart
  three <- sc_estimate(toy, list(sc_mean("t6", "tax6_linear"), sc_mean("t5", "tax5_linear"),
                                 sc_mean("t1", "tax1_linear")))
  pd <- draw(sc_plot_estimates(three, groups = c("Top", "Top", "Bottom")))
  expect_equal(pd$row_group, c("Top", "Top", "Bottom"))
  expect_equal(-diff(pd$y), c(1, 1.6))
  pd2 <- draw(sc_plot_estimates(three, groups = c(t1 = "Bottom", t5 = "Top", t6 = "Top"), order = c("t1", "t5", "t6")))
  expect_equal(pd2$row_group, c("Bottom", "Top", "Top"))
  expect_null(draw(sc_plot_estimates(three))$row_group)
  expect_error(sc_plot_estimates(three, groups = c("A", "B", "A")), "not adjacent")
  expect_error(sc_plot_estimates(three, groups = c("A", "B")), "labels has 2 entries")
  expect_error(sc_plot_estimates(three, groups = c(t1 = "A")), "names no group")
  expect_error(sc_plot_estimates(three, groups = list("A", "A", "B")), "character vector")
  # laws as grouped ridges with mean marks and a line outline
  l3 <- list(sc_fitted_law(toy, "tax6_linear"), sc_fitted_law(toy, "tax5_linear"), sc_fitted_law(toy, "tax1_linear"))
  expect_no_warning(r <- draw(sc_plot_laws(l3, labels = c("Top", "Upper", "Bottom"), means = three,
                                           groups = c("High", "High", "Low"), height = 1.3, bins = 300,
                                           outline = "line")))
  expect_equal(unique(r$group), c("High", "Low"))
  expect_equal(as.vector(table(r$law)), rep(300L, 3))
  expect_error(sc_plot_laws(l3, groups = c("a", "a", "b"), layout = "panels"), "ridges")
  expect_error(sc_plot_laws(l3, height = 0), "height")
  expect_error(sc_plot_laws(l3, bins = 5), "bins")
  expect_error(sc_plot_laws(l3, outline = "smooth"), "should be one of")
  # the ridges' densities do not depend on the drawing options
  r0 <- draw(sc_plot_laws(l3, bins = 300))
  expect_equal(r0$density, r$density)
  # preferences: grouped rows, same estimates as without groups
  pr <- draw(sc_plot_preferences(means, laws, shares, groups = c("Top", "Bottom")))
  p0 <- draw(sc_plot_preferences(means, laws, shares))
  expect_equal(pr$rows$group, c("Top", "Bottom"))
  expect_equal(pr$rows[setdiff(names(pr$rows), c("group", "y"))], p0$rows[setdiff(names(p0$rows), "y")])
  expect_equal(-diff(pr$rows$y), 1.6)
  expect_false("group" %in% names(p0$rows))
})

test_that("sc_plot_shares draws favoring and opposing bars that add to 100 (0.4.3)", {
  skip_if(is.null(toy))
  sh <- sc_estimate(toy, lapply(1:3, function(j) sc_share(paste("b", j), paste0("tax", j, "_linear"))), bounded = FALSE)
  expect_no_warning(pd <- draw(sc_plot_shares(sh, labels = c("One", "Two", "Three"), groups = c("A", "A", "B"),
                                              main = "Shares")))
  expect_equal(pd$favor, 100 * sh$estimate)
  expect_equal(pd$favor + pd$oppose, rep(100, 3))
  expect_equal(pd$group, c("A", "A", "B"))
  expect_equal(-diff(pd$y), c(1, 1.6))
  wide <- (sh$upper - sh$lower) >= .95
  expect_equal(pd$shown, !wide & is.finite(sh$lower) & is.finite(sh$upper))
  expect_false(any(draw(sc_plot_shares(sh, intervals = FALSE))$shown))
  expect_equal(draw(sc_plot_shares(sh, order = c("b 3", "b 1")))$name, c("b 3", "b 1"))
  expect_error(sc_plot_shares(means), "sc_share\\(\\) targets only")
  bad <- sh; bad$estimate[1] <- 1.2; bad$kind <- NA
  expect_error(sc_plot_shares(bad), "between 0 and 1")
  expect_error(sc_plot_shares(sh, colors = "red"), "two colors")
})
