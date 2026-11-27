# Paper-v4 public layer end to end on a small simulated BR-design data set: sc_fit(), sc_states(),
# sc_estimate(), sc_fitted_law(), sc_save()/sc_load(), print methods; and the BR helpers.

test_that("the public pipeline runs on toy data and round-trips through the compact file", {
  skip_on_cran()
  st <- .sc_toy_states(n = 30L, q = 2L)
  expect_s3_class(st, "sc_states")
  expect_output(print(st), "fold states: q = 2")
  e <- sc_estimate(st, list(sc_mean("top", "tax6_linear"), sc_share("top share", "tax6_linear"),
                            sc_br_plan_contest("prog vs flat", c(0, 5, 15, 25, 35, 45), rep(15, 6), w = function(g) g$party != "Ind"),
                            sc_importance(list(rates = 1:12, revenue = 13:16))))
  expect_true(all(is.finite(e$estimate)))
  expect_identical(e$interval, c("wald", "log-odds", "log-odds", "none", "none"))
  expect_equal(sum(e$estimate[e$kind == "importance"]), 1, tolerance = 1e-8)
  expect_output(print(e), "top share")
  l <- sc_fitted_law(st, "tax6_linear")
  expect_identical(nrow(l), 30L); expect_output(print(l), "30 held-out respondents")
  f <- tempfile(fileext = ".rds"); on.exit(unlink(f))
  sc_save(st, f); st2 <- sc_load(f)
  expect_identical(sc_estimate(st2, e_tl <- list(sc_mean("top", "tax6_linear")))$estimate,
                   sc_estimate(st, e_tl)$estimate)
  net <- st2$fits$folds[[1]]$network
  Zs <- .rum_preprocess_apply(st2$fits$folds[[1]]$preprocess, st2$data$Z, st2$data$data$ids)
  expect_equal(unname(rum_dnn_gaussian_forward(net, Zs)), unname(st$folds[[1]]$mu))
})

test_that("sc_fit records the fold fits and their held-out scores", {
  skip_on_cran()
  x <- .sc_toy_data(n = 24L)
  f <- sc_fit(x, q = 1L, folds = 1L, hidden = 3L, seeds = 1L, control = list(max_epochs = 5L),
              nodes = 64L, check_nodes = 128L, heldout_nodes = 128L, verbose = FALSE)
  expect_s3_class(f, "sc_fit")
  expect_identical(names(f$folds), "1")
  g <- f$folds[["1"]]
  expect_identical(g$te, which(x$fold == 1L))
  expect_length(g$heldout$sobol128, length(g$te))
  expect_true(all(g$validation %in% x$data$ids[g$tr]))
  expect_output(print(f), "q = 1 taste factor")
  expect_output(print(summary(f)), "fold")
  expect_error(sc_states(f), NA)
  x$design <- NULL; f$data <- x
  expect_error(sc_states(f), "design law")
})

test_that("BR helpers code plans as the design coder does", {
  set.seed(4)
  menus <- .RUM_BR_MENUS
  A <- as.data.frame(sapply(1:6, function(j) sample(menus[[j]], 5, TRUE))); names(A) <- .sc_br_rate_names
  B <- as.data.frame(sapply(1:6, function(j) sample(menus[[j]], 5, TRUE))); names(B) <- .sc_br_rate_names
  A$taxrev <- sample(1:5, 5, TRUE); B$taxrev <- sample(1:5, 5, TRUE)
  ref <- .rum_design_code_profiles("br", A, B)
  mine <- .sc_br_features(.sc_br_rates(A), A$taxrev) - .sc_br_features(.sc_br_rates(B), B$taxrev)
  expect_equal(unname(mine), unname(ref))
  rows <- .sc_br_plan_rows(c(0, 5, 15, 25, 35, 45), rep(15, 6))
  expect_equal(sum(rows$coef), 1)
  am <- .sc_br_ame_rows(6, list(A = A, B = B))
  expect_equal(sum(am$coef), 0, tolerance = 1e-12); expect_equal(sum(abs(am$coef)), 2, tolerance = 1e-12)
  c6 <- sc_br_revenue_contrast(6, profiles = list(A = A, B = B))
  expect_named(c6, .RUM_BR_RECIPE$features)
  expect_equal(unname(c6[1:12]), drop(.sc_br_features(c(15, 15, 15, 15, 15, 55), 3) - .sc_br_features(c(15, 15, 15, 15, 15, 5), 3))[1:12])
  expect_equal(sc_br_revenue_contrast(6, 55, 35, list(A = A, B = B), per_point = TRUE),
               sc_br_revenue_contrast(6, 55, 35, list(A = A, B = B)) / 20)
  expect_error(.sc_br_features(c(1, 5, 15, 25, 35, 45), 3), "not on its menu")
  expect_s3_class(sc_br_endpoint_ame("e", 1, list(A = A, B = B)), "sc_target")
})
