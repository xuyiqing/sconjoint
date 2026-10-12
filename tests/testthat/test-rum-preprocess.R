# Tests of fold-internal preprocessing and the fold trainer (R/rum-preprocess.R;
# Acharya memo of 2026-10-08, item 3). A tiny simulated problem: 60 respondents, 4 tasks,
# 2 features, 3 covariates (one with missing values), a one-layer network and two seeds.
# Single-threaded; nothing is written outside tempdir().

rpp <- function(nm) get(nm, envir = asNamespace("sconjoint"))
learn <- rpp(".rum_preprocess_learn"); apply_rule <- rpp(".rum_preprocess_apply")
fold_train <- rpp(".rum_dnn_fold_train"); fold_mu <- rpp(".rum_dnn_fold_mu")

toy_problem <- function(n = 60L, T = 4L, seed = 11L) {
  set.seed(seed)
  ids <- sprintf("r%03d", seq_len(n))
  Z <- cbind(age = round(rnorm(n, 45, 12)), income = sample(1:9, n, TRUE),
             female = rbinom(n, 1, .5))
  Z[sample(n, 6), "income"] <- NA
  rownames(Z) <- ids
  beta <- cbind(.8 + .02 * (Z[, "age"] - 45), -.5 + .6 * Z[, "female"])
  d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), 2 * T, TRUE), T, 2,
                                               dimnames = list(NULL, c("f1", "f2"))))
  y <- lapply(seq_len(n), function(i) as.numeric(runif(T) < plogis(drop(d[[i]] %*% beta[i, ]))))
  data <- rum_data(y, d, ids = ids, metadata = list(orientation = "y = 1 picks the first profile"))
  fold <- rep(1:3, length.out = n)
  list(data = data, Z = Z, fold = fold, ids = ids)
}
CTL <- list(max_epochs = 12L, validation_every = 2L, patience = 3L, train_order = 5L,
            check_orders = 7L, block_size = 16L, max_seconds = 600, start_seconds = 200,
            refit_seconds = 150, evaluation_reserve = 60)
run_fold <- function(P, k = 1L, Z = P$Z, data = P$data, trainers = c("avi", "avipol", "avipolb")) {
  tr <- P$ids[P$fold != k]
  val <- tr[seq(1, length(tr), by = 5)]
  fold_train(data, Z, tr, val, hidden = 4L, seeds = c(3L, 5L), control = CTL,
             trainers = trainers, quadrature = rum_quadrature(c(7L, 7L)))
}
# Replace the evaluation fold's covariates and choices with something else entirely.
perturb_eval <- function(P, k = 1L) {
  ev <- which(P$fold == k); Q <- P
  Q$Z[ev, "age"] <- Q$Z[ev, "age"] * 3 + 100
  Q$Z[ev, "income"] <- NA
  Q$Z[ev, "female"] <- 1 - Q$Z[ev, "female"]
  d <- P$data; y <- split(d$y, rep(seq_len(d$n), d$T)); dd <- lapply(seq_len(d$n), function(i)
    d$d[d$offsets[i] + seq_len(d$T[i]), , drop = FALSE])
  for (i in ev) { y[[i]] <- 1 - y[[i]]; dd[[i]] <- -dd[[i]] }
  Q$data <- rum_data(unname(y), dd, ids = d$ids, feature_names = d$feature_names,
                     metadata = d$metadata)
  Q
}

test_that("the learned rule uses only training rows (medians, centers, scales)", {
  P <- toy_problem(); tr <- P$ids[P$fold != 1L]
  r <- learn(P$Z, tr)
  X <- P$Z[tr, ]; med <- median(X[, "income"], na.rm = TRUE)
  expect_identical(unname(r$fill[["income"]]), med)
  X[is.na(X[, "income"]), "income"] <- med
  expect_equal(unname(r$center), unname(colMeans(X)), tolerance = 0)
  expect_equal(unname(r$scale), unname(sqrt(colMeans(sweep(X, 2, colMeans(X))^2))), tolerance = 0)
  Zs <- apply_rule(r, P$Z)
  expect_false(anyNA(Zs))
  expect_equal(unname(colMeans(Zs[tr, ])), c(0, 0, 0), tolerance = 1e-12)
  expect_equal(unname(sqrt(colMeans(Zs[tr, ]^2))), c(1, 1, 1), tolerance = 1e-12)
  # the evaluation respondents' imputed values are the training median, not their own
  ev_na <- intersect(P$ids[P$fold == 1L], rownames(P$Z)[is.na(P$Z[, "income"])])
  if (length(ev_na)) expect_equal(unname(Zs[ev_na, "income"]),
                                  rep((med - r$center[["income"]]) / r$scale[["income"]], length(ev_na)))
  expect_error(learn(P$Z, tr, impute = "none"), "Missing")
  Zc <- P$Z; Zc[tr, "female"] <- 1
  expect_error(learn(Zc, tr), "constant")
})

test_that("leak test: perturbing the evaluation fold leaves everything learned for it bit-identical", {
  P <- toy_problem(); Q <- perturb_eval(P, 1L)
  expect_false(identical(P$Z, Q$Z)); expect_false(identical(P$data$y, Q$data$y))
  a <- run_fold(P); b <- run_fold(Q)
  expect_identical(a$preprocess, b$preprocess)
  for (tr in c("avi", "avipol", "avipolb")) expect_identical(a$finals[[tr]], b$finals[[tr]])
  expect_identical(a$fit$selection, b$fit$selection)
  expect_identical(a$polish, b$polish)
  # and the fold's predictions for the training respondents are identical too
  trn <- P$ids[P$fold != 1L]
  expect_identical(fold_mu(a, P$Z, trn), fold_mu(b, Q$Z, trn))
  # the check has power: perturbing a TRAINING respondent changes the fit
  R <- P; i <- which(P$fold == 2L)[1]; R$Z[i, "age"] <- R$Z[i, "age"] + 30
  c2 <- run_fold(R, trainers = "avi")
  expect_false(identical(a$preprocess$center, c2$preprocess$center))
  expect_false(identical(a$finals$avi, c2$finals$avi))
})

test_that("the old full-sample standardization does leak (contrast with the fold rule)", {
  P <- toy_problem(); Q <- perturb_eval(P, 1L)
  full <- function(Z) { X <- Z; m <- apply(X, 2, median, na.rm = TRUE)
    na <- which(is.na(X), arr.ind = TRUE); X[na] <- m[na[, "col"]]
    sweep(sweep(X, 2, colMeans(X)), 2, sqrt(colMeans(sweep(X, 2, colMeans(X))^2)), "/") }
  trn <- P$ids[P$fold != 1L]
  expect_false(identical(full(P$Z)[trn, ], full(Q$Z)[trn, ]))
})

test_that("invariance: a covariate multiplied by 1e8 leaves the fold network's predictions unchanged", {
  P <- toy_problem(); Zb <- P$Z; Zb[, "age"] <- Zb[, "age"] * 1e8
  a <- run_fold(P); b <- run_fold(P, Z = Zb)
  for (tr in c("avi", "avipolb")) {
    ma <- fold_mu(a, P$Z, trainer = tr); mb <- fold_mu(b, Zb, trainer = tr)
    expect_lt(max(abs(ma - mb)), 1e-6)
  }
  expect_equal(a$finals$avipolb$kappa, b$finals$avipolb$kappa, tolerance = 1e-6)
  # shifting a covariate by a constant is also absorbed
  Zs <- P$Z; Zs[, "income"] <- Zs[, "income"] + 1e4
  s <- run_fold(P, Z = Zs, trainers = "avi")
  expect_lt(max(abs(fold_mu(a, P$Z, trainer = "avi") - fold_mu(s, Zs, trainer = "avi"))), 1e-6)
})

test_that("the polish raises the training likelihood and matches its definition", {
  P <- toy_problem(); a <- run_fold(P)
  for (tr in c("avipol", "avipolb")) {
    expect_gte(a$polish[[tr]]$loglik_after, a$polish[[tr]]$loglik_before - 1e-10)
    expect_identical(a$polish[[tr]]$convergence, 0L)
  }
  # avipol keeps the network; avipolb changes only its output bias
  expect_identical(a$finals$avipol$network, a$finals$avi$network)
  L <- length(a$finals$avi$network$biases)
  nb <- a$finals$avipolb$network; na <- a$finals$avi$network
  expect_identical(nb$weights, na$weights)
  expect_identical(nb$biases[-L], na$biases[-L])
  expect_false(identical(nb$biases[[L]], na$biases[[L]]))
  expect_error(fold_train(P$data, P$Z, P$ids[P$fold != 1L], P$ids[P$fold == 1L][1:3],
                          hidden = 4L, seeds = 3L, control = CTL), "training respondents")
})
