# sc_design(): the general design law (independent attributes, two profiles, fixed tasks). Coding,
# draws, the data check, and the conditional information against an independent brute-force computation
# that enumerates every task sequence of a tiny design and differentiates the log likelihood numerically.

tiny_design <- function(probs = list(b = c(0.5, 0.25, 0.25)))
  sc_design(list(a = c("a1", "a2"), b = c("b1", "b2", "b3")), tasks = 2, probs = probs)

# Every profile of a design, its probability and its treatment-dummy coding (written out by hand).
enum_profiles <- function(des) {
  prof <- expand.grid(des$attributes, stringsAsFactors = FALSE)
  pp <- Reduce(`*`, lapply(names(des$attributes), function(a) des$probs[[a]][match(prof[[a]], des$attributes[[a]])]))
  x <- do.call(cbind, lapply(names(des$attributes), function(a) {
    lv <- des$attributes[[a]]
    vapply(lv[-1L], function(l) as.numeric(prof[[a]] == l), numeric(nrow(prof)))
  }))
  list(prof = prof, prob = pp, x = matrix(x, nrow(prof)))
}

# Brute-force information of one respondent: sum over all task sequences D (with their design
# probabilities) and all choice sequences y of p(y | D) s s', s the gradient of log p(y | D) in
# (mu, kappa, loading, omega) by central differences; p by the given (u, v) rule. One factor.
brute_info <- function(des, mu, th, quad, h = 1e-5) {
  e <- enum_profiles(des); p <- ncol(e$x); T <- des$tasks
  pr <- expand.grid(i = seq_len(nrow(e$prof)), j = seq_len(nrow(e$prof)))
  dp <- e$x[pr$i, , drop = FALSE] - e$x[pr$j, , drop = FALSE]; wp <- e$prob[pr$i] * e$prob[pr$j]
  seqs <- as.matrix(expand.grid(rep(list(seq_len(nrow(pr))), T)))
  Y <- as.matrix(expand.grid(rep(list(c(0, 1)), T)))
  par0 <- c(mu, th$kappa, th$loading, th$omega); D <- length(par0); G <- length(quad$weight)
  logp <- function(par, Dm) {                         # log p(y | D) for every y (rows of Y)
    m <- par[seq_len(p)]; k <- par[p + 1L]; L <- par[p + 1L + seq_len(p)]; om <- par[2L * p + 2L]
    eta <- k + drop(Dm %*% m) + outer(drop(Dm %*% L), quad$u) + matrix(sqrt(om) * quad$v, T, G, byrow = TRUE)
    ll <- Y %*% stats::plogis(eta, log.p = TRUE) + (1 - Y) %*% stats::plogis(-eta, log.p = TRUE)
    log(drop(exp(ll) %*% quad$weight))
  }
  I <- matrix(0, D, D)
  for (s in seq_len(nrow(seqs))) {
    Dm <- dp[seqs[s, ], , drop = FALSE]
    S <- vapply(seq_len(D), function(j) {
      a <- par0; b <- par0; a[j] <- a[j] + h; b[j] <- b[j] - h
      (logp(a, Dm) - logp(b, Dm)) / (2 * h)
    }, numeric(nrow(Y)))
    py <- exp(logp(par0, Dm))
    I <- I + prod(wp[seqs[s, ]]) * crossprod(S * py, S)
  }
  I
}

test_that("sc_design codes treatment dummies, custom codings and level probabilities", {
  des <- tiny_design()
  expect_s3_class(des, "sc_design")
  expect_identical(des$features, c("a_a2", "b_b2", "b_b3"))
  expect_equal(des$probs$a, c(0.5, 0.5)); expect_equal(des$probs$b, c(0.5, 0.25, 0.25))
  A <- data.frame(a = c("a2", "a1", "a1"), b = c("b3", "b1", "b2"))
  B <- data.frame(a = c("a1", "a1", "a2"), b = c("b1", "b2", "b2"))
  X <- sc_code(des, A, B)
  expect_equal(unname(X), rbind(c(1, 0, 1), c(0, -1, 0), c(-1, 0, 0)))
  expect_identical(colnames(X), des$features)
  expect_equal(sc_code(des, B, A), -X)
  # factors and extra columns are fine; unknown levels are not
  expect_equal(sc_code(des, transform(A, a = factor(a), z = 1), B), X)
  expect_error(sc_code(des, transform(A, b = "b9"), B), "unknown level")
  # a custom coding: one linear score for b; names with spaces are cleaned
  d2 <- sc_design(list(a = c("no", "yes please"), b = c("b1", "b2", "b3")), tasks = 3,
                  coding = list(b = matrix(0:2, dimnames = list(NULL, "b_score"))))
  expect_identical(d2$features, c("a_yes_please", "b_score"))
  expect_equal(unname(sc_code(d2, data.frame(a = "yes please", b = "b3"), data.frame(a = "no", b = "b2"))), cbind(1, 1))
  expect_output(print(d2), "2 attributes")
  expect_error(sc_design(list(a = "x"), tasks = 2), "at least two")
  expect_error(sc_design(list(a = c("x", "y")), tasks = 2, probs = list(a = c(1, -1))), "nonnegative")
  expect_error(sc_design(list(a = c("x", "y")), tasks = 0), "positive")
  expect_error(sc_design(list(a = c("x", "y"), b = c("u", "v")), tasks = 2,
                         coding = list(b = matrix(0:1, dimnames = list(NULL, "a_y")))), "distinct")
})

test_that("the design's draws follow its level probabilities and its coder matches sc_code()", {
  des <- tiny_design(); spec <- .rum_spec_arg(des)
  expect_s3_class(spec, "rum_design_spec"); expect_identical(spec$T, 2L); expect_false(spec$uses_resp)
  raw <- .rum_design_draw_raw(spec, 20000L, seed = 1L)
  expect_identical(dim(raw$b), c(20000L, 2L, 2L))
  expect_equal(as.numeric(tabulate(raw$b, 3) / length(raw$b)), c(0.5, 0.25, 0.25), tolerance = 0.01)
  expect_equal(as.numeric(tabulate(raw$a, 2) / length(raw$a)), c(0.5, 0.5), tolerance = 0.01)
  # sides and tasks are independent: no correlation between the two profiles' levels
  expect_lt(abs(stats::cor(as.vector(raw$b[, , 1]), as.vector(raw$b[, , 2]))), 0.03)
  d <- .rum_draw_designs(des, M = 5L, seed = 2L, raw = TRUE); r <- attr(d, "raw")
  prof <- .rum_design_profiles(spec, r)
  X <- sc_code(des, prof$profA, prof$profB)
  expect_equal(unname(X), unname(matrix(aperm(d, c(2L, 1L, 3L)), 10L, 3L)))
  # the private seed leaves the caller's random numbers alone
  set.seed(5); a <- stats::runif(1); set.seed(5); invisible(.rum_design_draw_raw(spec, 10L, seed = 3L)); b <- stats::runif(1)
  expect_identical(a, b)
})

test_that("sc_data() takes an sc_design and checks the feature names against its coding", {
  des <- tiny_design()
  set.seed(3)
  A <- data.frame(a = sample(c("a1", "a2"), 20, TRUE), b = sample(c("b1", "b2", "b3"), 20, TRUE))
  B <- data.frame(a = sample(c("a1", "a2"), 20, TRUE), b = sample(c("b1", "b2", "b3"), 20, TRUE))
  X <- sc_code(des, A, B); y <- stats::rbinom(20, 1, 0.5); id <- rep(1:10, each = 2)
  x <- sc_data(y, X, id, Z = matrix(stats::rnorm(10), 10, 1), design = des)
  expect_identical(x$design, des)
  expect_output(print(x), "sc_design \\(2 attributes, 2 tasks\\)")
  expect_error(sc_data(y, X[, c(2, 1, 3)], id, design = des), "same features in another order")
  expect_error(sc_data(y, X[, 1:2], id, design = des), "the data have 2 features, the design 3")
  Xb <- X; colnames(Xb)[1] <- "a_other"
  expect_error(sc_data(y, Xb, id, design = des), "features not in the design: a_other")
  expect_error(sc_data(y, X, id, design = list(1)), "sc_design")
  expect_error(sc_data(y, X, id, design = "sw"))
})

test_that("the exact conditional information under an sc_design equals a brute-force enumeration", {
  des <- tiny_design(); spec <- .rum_spec_arg(des); p <- 3L
  e <- enum_profiles(des)
  # every task sequence, replicated in proportion to its probability (profile probabilities are
  # multiples of 1/8, so each sequence of two tasks is a multiple of 1/8^4)
  cnt <- round(e$prob * 8); stopifnot(all(abs(e$prob * 8 - cnt) < 1e-12))
  pr <- expand.grid(i = seq_len(nrow(e$prof)), j = seq_len(nrow(e$prof)))
  sq <- expand.grid(t1 = seq_len(nrow(pr)), t2 = seq_len(nrow(pr)))
  rep_n <- cnt[pr$i[sq$t1]] * cnt[pr$j[sq$t1]] * cnt[pr$i[sq$t2]] * cnt[pr$j[sq$t2]]
  rows <- rep(seq_len(nrow(sq)), rep_n); M <- length(rows); expect_identical(M, 4096L)
  raw <- lapply(names(des$attributes), function(a) {
    lvl <- match(e$prof[[a]], des$attributes[[a]])
    arr <- array(0L, c(M, 2L, 2L))
    arr[, 1L, 1L] <- lvl[pr$i[sq$t1[rows]]]; arr[, 1L, 2L] <- lvl[pr$j[sq$t1[rows]]]
    arr[, 2L, 1L] <- lvl[pr$i[sq$t2[rows]]]; arr[, 2L, 2L] <- lvl[pr$j[sq$t2[rows]]]
    arr
  })
  names(raw) <- names(des$attributes)
  crn <- list(designs = list(all = spec$code(raw)), key = c(1L, 1L), M = M, T = 2L)
  quad <- rum_quadrature(c(12L, 12L))
  mu <- rbind(c(0.4, -0.3, 0.8), c(-0.6, 0.2, 0.1))
  th <- list(kappa = 0.2, loading = c(0.5, -0.3, 0.4), omega = 0.35)
  dat <- rum_data(list(c(1, 0), c(0, 1)), list(matrix(0, 2, 3), matrix(0, 2, 3)),
                  metadata = list(orientation = "test"))
  R <- .rum_cond_info(dat, mu, th, crn, method = "exact", quad = quad)
  for (i in 1:2) {
    Ib <- brute_info(des, mu[i, ], th, quad)
    expect_equal(R$I[i, , ], Ib, tolerance = 1e-7)
  }
})

test_that("sc_states() information under an sc_design matches the brute-force information", {
  des <- tiny_design(); n <- 40L
  set.seed(11)
  draw <- function() data.frame(a = sample(des$attributes$a, 2 * n, TRUE),
                                b = sample(des$attributes$b, 2 * n, TRUE, prob = des$probs$b))
  X <- sc_code(des, draw(), draw()); id <- rep(seq_len(n), each = 2)
  Z <- cbind(z = stats::rnorm(n)); beta <- outer(Z[, 1], c(0.5, -0.3, 0.4)) + 0.3
  y <- stats::rbinom(2 * n, 1, stats::plogis(0.2 + rowSums(X * beta[id, ])))
  x <- sc_data(y, X, id, Z = Z, fold = rep(1:2, length.out = n), design = des)
  f <- sc_fit(x, q = 1, folds = 1, hidden = 2L, seeds = 1L, control = list(max_epochs = 5L, patience = 2L),
              nodes = 64L, check_nodes = 128L, heldout_nodes = 128L, verbose = FALSE)
  # a tiny fit puts omega and the loading at their bounds; the information is a function of the
  # state, so check it at an interior one
  f$folds[[1]][c("kappa", "loading", "omega")] <- list(0.2, c(0.5, -0.3, 0.4), 0.35)
  st <- sc_states(f, M = 3000L, ny = 8L, info_nodes = 512L, score_nodes = 256L, respondents = 2L)
  expect_identical(st$info$M, 3000L)
  fs <- st$folds[[1]]; quad <- rum_quadrature(c(20L, 20L))
  for (j in seq_along(fs$te)) {
    Ib <- brute_info(des, fs$mu[fs$te[j], ], fs$th, quad, h = 1e-6)
    I3 <- cbind(solve(fs$parts$Li[j, , ]), fs$parts$Lt[j, , ])     # the mu rows of the information
    # Monte Carlo over 3,000 design draws: about 2% here
    expect_lt(max(abs(I3 - Ib[1:3, ])) / max(abs(Ib[1:3, ])), 0.06)
  }
})

test_that("sc_ame_switch() and sc_importance() take the design; the AME matches exact enumeration", {
  des <- sc_design(list(a = c("a1", "a2"), b = c("b1", "b2", "b3")), tasks = 4)
  n <- 60L; set.seed(21)
  draw <- function() data.frame(a = sample(des$attributes$a, 4 * n, TRUE), b = sample(des$attributes$b, 4 * n, TRUE))
  X <- sc_code(des, draw(), draw()); id <- rep(seq_len(n), each = 4)
  Z <- cbind(z = stats::rnorm(n)); beta <- outer(Z[, 1], c(0.5, -0.3, 0.6)) + 0.4
  y <- stats::rbinom(4 * n, 1, stats::plogis(0.3 + rowSums(X * beta[id, ])))
  x <- sc_data(y, X, id, Z = Z, fold = rep(1:2, length.out = n), design = des)
  f <- sc_fit(x, q = 1, hidden = 2L, seeds = 1L, control = list(max_epochs = 5L, patience = 2L),
              nodes = 64L, check_nodes = 128L, heldout_nodes = 128L, verbose = FALSE)
  st <- sc_states(f, M = 10L, ny = 2L, info_nodes = 128L, score_nodes = 128L)
  est <- sc_estimate(st, list(sc_ame_switch("b3 vs b1", "b", level = "b3", ref = "b1", draws = 3000L),
                              sc_ame_switch("b3 vs b1 (positions)", "b", level = 3, ref = 1, draws = 3000L)))
  expect_equal(est$estimate[1], est$estimate[2])
  # the exact plug-in: every context of the design (the switched profile's other attribute, the
  # opponent, the side), each respondent at the fold that held it out
  e <- enum_profiles(des); gq <- .rum_normal_rule(40L)
  exact <- unlist(lapply(st$folds, function(fs) {
    th <- fs$th; vals <- vapply(fs$te, function(i) {
      m <- fs$mu[i, ]
      P <- function(lv) {
        foc <- e$prof; foc$b <- lv; xf <- sc_code(des, foc, e$prof[rep(1, nrow(foc)), ])
        ctx <- expand.grid(f = which(!duplicated(foc$a)), o = seq_len(nrow(e$prof)))
        D <- xf[ctx$f, , drop = FALSE] - e$x[ctx$o, , drop = FALSE]
        wk <- 0.5 * e$prob[ctx$o]
        s <- sqrt(drop(D %*% th$loading)^2 + th$omega); mm <- drop(D %*% m)
        pr <- vapply(seq_along(mm), function(k) sum(gq$weights *
          (0.5 * stats::plogis(th$kappa + mm[k] + s[k] * gq$nodes) +
           0.5 * stats::plogis(-th$kappa + mm[k] + s[k] * gq$nodes))), 0)
        sum(wk * pr)
      }
      P("b3") - P("b1")
    }, 0)
    vals
  }))
  expect_equal(est$plug_in[1], mean(exact), tolerance = 0.01)
  expect_error(sc_estimate(st, sc_ame_switch("bad", "c", level = 1, ref = 2)), "not an attribute")
  expect_error(sc_estimate(st, sc_ame_switch("bad", "b", level = "b9", ref = "b1")), "not a level")
  # importance from the design: the menus and weights the design implies
  tg <- sc_importance(des)
  ctx <- .sc_context(x); built <- tg$build(ctx)
  expect_identical(vapply(built, `[[`, "", "name"), c("a", "b"))
  e1 <- sc_estimate(st, sc_importance(des)); e2 <- sc_estimate(st, sc_importance(list(a = "a_a2", b = c("b_b2", "b_b3"))))
  expect_equal(e1$estimate, e2$estimate, tolerance = 1e-10)
  # save and load keep the design
  file <- tempfile(fileext = ".rds"); sc_save(st, file); st2 <- sc_load(file)
  expect_identical(st2$data$design, des)
})
