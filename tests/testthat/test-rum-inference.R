# Tests of the RUM inference engine (R/rum-inference.R, rum-inference-targets.R,
# rum-inference-model.R). The fixture fixtures/rum-inference-fixture.rds holds a small
# simulated data set (90 respondents, 3 features, 3 folds of untrained networks), a trimmed
# formula fit, target specifications, and the outputs of the reproduction scripts' engine
# (rum_reproduction_2026-09-24/scripts_C/rum_debias.R and the Graham-Svolik mirrors
# debias_dnn_if()/formula_ci_if()) on those inputs. It was written by A_make_fixture.R in the
# 2026-10-11 run folder (rum_inference_2026-10-11/scripts/). Single-threaded; nothing is
# written outside tempdir().

rinf <- function(nm) get(nm, envir = asNamespace("sconjoint"))
fx <- readRDS(test_path("fixtures", "rum-inference-fixture.rds"))
CTOR <- list(mean = rinf(".rum_tgt_mean"), share = rinf(".rum_tgt_share"),
             choice = rinf(".rum_tgt_choice"), choice_pref = rinf(".rum_tgt_choice_pref"),
             choice_set = rinf(".rum_tgt_choice_set"),
             choice_set_pref = rinf(".rum_tgt_choice_set_pref"),
             lincomb = rinf(".rum_tgt_lincomb"))
spec_targets <- function(specs) lapply(specs, function(s) {
  a <- s$args; if (s$f == "lincomb") a$parts <- spec_targets(a$parts)
  do.call(CTOR[[s$f]], c(list(name = s$name), a))
})
TG <- spec_targets(fx$specs)
BND <- TG[vapply(TG, function(t) t$kind %in% rinf(".rum_bounded_kinds"), TRUE)]
fold_final <- function(k) fx$fins[[k]]
maxdiff <- function(a, b) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  expect_identical(dim(a), dim(b))
  max(abs(a - b))
}
# numeric columns of the old table against the new; other columns identical
table_diff <- function(old, new) {
  expect_true(all(names(old) %in% names(new)))
  num <- names(old)[vapply(old, is.numeric, TRUE)]
  for (cc in setdiff(names(old), num)) expect_identical(new[[cc]], old[[cc]])
  max(vapply(num, function(cc) max(abs(old[[cc]] - new[[cc]])), 0))
}
TOL <- 1e-10

test_that("exact sequence scores equal the evaluator's scores and a numerical gradient", {
  set.seed(3L)
  p <- 3L; B <- 12L; T <- 4L
  d <- lapply(seq_len(B), function(i) matrix(sample(c(-1, 0, 1), T * p, replace = TRUE), T, p))
  y0 <- lapply(seq_len(B), function(i) rep(0, T))
  data <- rum_data(y0, d, metadata = list(orientation = "test"))
  par <- rum_parameters(data, kappa = .2, alpha = c(.5, -.3, .1), loading = c(.4, .2, -.3),
                        position_variance = .5)
  data <- rum_simulate(data, par, seed = 11L)
  MU <- matrix(rnorm(B * p), B, p, dimnames = list(data$ids, NULL))
  th <- list(kappa = .2, loading = c(.4, .2, -.3), omega = .5)
  quad <- rum_quadrature(c(11L, 9L))
  DL <- lapply(seq_len(T), function(t) data$d[data$offsets[-(B + 1L)] + t, , drop = FALSE])
  Y <- matrix(data$y, B, T, byrow = TRUE)
  S <- rinf(".rum_seq_scores")(MU, DL, Y, th, quad)
  ev <- rum_dnn_gaussian_evaluate(data, MU, th$kappa, th$loading, th$omega, quad)
  expect_lt(maxdiff(S, cbind(ev$output_score, ev$common_score)), 1e-12)
  # central differences of each respondent's sequence log likelihood
  ll <- function(mu, v) rum_dnn_gaussian_evaluate(data, mu, v[1], v[1 + seq_len(p)], v[p + 2],
                                                  quad)$loglik
  h <- 1e-5; v0 <- c(th$kappa, th$loading, th$omega); num <- matrix(0, B, 2L * p + 2L)
  for (j in seq_len(p)) {
    m1 <- MU; m2 <- MU; m1[, j] <- m1[, j] + h; m2[, j] <- m2[, j] - h
    num[, j] <- (ll(m1, v0) - ll(m2, v0)) / (2 * h)
  }
  for (j in seq_len(p + 2L)) {
    v1 <- v0; v2 <- v0; v1[j] <- v1[j] + h; v2[j] <- v2[j] - h
    num[, p + j] <- (ll(MU, v1) - ll(MU, v2)) / (2 * h)
  }
  expect_lt(maxdiff(S, num), 1e-7)
})

test_that("one-step (constant, linear, model basis) reproduces the reproduction engine", {
  o <- fx$out
  for (b in c("constant", "linear", "model")) {
    # pop_center = "plug_in": the reproduction's population-reference values (explicit opt-in)
    r <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, fold_final, TG, info_basis = b,
                              model_M = fx$model_M, pop_center = "plug_in")
    tb <- rinf(".rum_onestep_table")(r)
    old <- o[[paste0("debias_", b)]]
    expect_lt(table_diff(old$table, tb), TOL)
    expect_lt(table_diff(old$attrs$fold_diagnostics, r$diag), TOL)
    if (b == "model") {
      expect_lt(maxdiff(old$attrs$IFF, r$IFF), TOL)
      expect_lt(maxdiff(old$attrs$IFP, r$IFP), TOL)
      expect_lt(maxdiff(old$attrs$fold_loadings, r$fold_loadings), TOL)
    } else {                                  # the Graham-Svolik mirror's influence values
      oi <- o[[paste0("if_", b)]]
      expect_lt(maxdiff(oi$IFF, r$IFF), TOL)
      expect_lt(maxdiff(oi$IFP, r$IFP), TOL)
      expect_lt(maxdiff(oi$est, r$est), TOL)
      expect_lt(maxdiff(oi$plug, r$plug), TOL)
      expect_identical(oi$kind, r$kind)
      expect_equal(unname(oi$n_group), unname(r$n_group))
      expect_lt(table_diff(oi$diag, r$diag), TOL)
    }
  }
})

test_that("CV-TMLE and the bounded rule reproduce the reproduction engine", {
  o <- fx$out
  tm <- rinf(".rum_tmle_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND)
  expect_lt(table_diff(o$tmle_constant$table, tm), TOL)
  expect_lt(maxdiff(o$tmle_constant$attrs$IFF, attr(tm, "IFF")), TOL)
  expect_lt(maxdiff(o$tmle_constant$attrs$IFP, attr(tm, "IFP")), TOL)
  tmm <- rinf(".rum_tmle_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND, info_basis = "model",
                               model_M = fx$model_M)
  expect_lt(table_diff(o$tmle_model$table, tmm), TOL)
  expect_lt(maxdiff(o$tmle_model$attrs$IFP, attr(tmm, "IFP")), TOL)
  expect_identical(attr(tmm, "info_basis"), "model")
  bd <- rinf(".rum_bounded_dnn")(fx$data, fx$Z, fx$fold, fold_final, BND)
  expect_lt(table_diff(o$bounded$table, bd), TOL)
  expect_identical(rownames(bd), rownames(o$bounded$table))
  expect_true(all(bd$lower_population >= 0 & bd$upper_population <= 1))
})

test_that("formula influence functions reproduce the reproduction engine", {
  o <- fx$out
  fc <- rinf(".rum_formula_ci")(fx$fit, TG)
  expect_lt(table_diff(o$formula$table, fc), TOL)
  fi <- rinf(".rum_formula_if")(fx$fit, TG)
  expect_lt(maxdiff(o$formula_if$IFF, fi$IFF), TOL)
  expect_lt(maxdiff(o$formula_if$IFP, fi$IFP), TOL)
  expect_lt(maxdiff(o$formula_if$est, fi$est), TOL)
})

test_that("model information and the eigenvalue floor are unchanged", {
  o <- fx$out; p <- fx$data$p; q <- p + 2L
  mi <- rinf(".rum_model_info")(fx$data, fx$mu1, fx$th1, 12L, rinf(".rum_info_quadrature")(),
                                seed = 99L, halves = TRUE)
  expect_lt(maxdiff(o$model_info$I, mi$I), TOL)
  expect_lt(maxdiff(o$model_info$IA, mi$IA), TOL)
  expect_lt(maxdiff(o$model_info$IB, mi$IB), TOL)
  IA <- o$floor_input
  for (rel in c(1e-6, 1e-2)) {
    old <- if (rel == 1e-6) o$floor_parts else o$floor_parts_rel
    W <- rinf(".rum_weight_parts")(function(i) IA[i, , ], dim(IA)[1], p, q, rel)
    expect_identical(W$floored, old$floored)
    expect_lt(maxdiff(old$Li, W$Li), TOL)
    expect_lt(maxdiff(old$Lt, W$Lt), TOL)
    expect_lt(maxdiff(old$Ic, W$Ic), TOL)
  }
  # the rank-deficient and the indefinite matrix are floored, the others are not
  expect_identical(o$floor_parts$floored, 2L)
  # one shared matrix: the rank-deficient one is floored
  W2 <- rinf(".rum_weight_parts")(function(i) IA[2, , ], 1L, p, q, 1e-6)
  expect_identical(W2$floored, 1L)
  expect_true(W2$shared)
})

test_that("a heavy ridge makes the linear basis the constant basis", {
  one <- rinf(".rum_onestep")
  rc <- one(fx$data, fx$Z, fx$fold, fold_final, TG[1:3], info_basis = "constant")
  rl <- one(fx$data, fx$Z, fx$fold, fold_final, TG[1:3], info_basis = "linear", ridge_rel = 1e12)
  expect_lt(maxdiff(rc$est, rl$est), 1e-6)
  expect_lt(maxdiff(rc$IFF, rl$IFF), 1e-6)
})

test_that("the model information cache is optional, private to tempdir, and exact", {
  skip_if_not_installed("digest")
  dir <- withr::local_tempdir()
  one <- rinf(".rum_onestep")
  r1 <- one(fx$data, fx$Z, fx$fold, fold_final, TG[1:2], info_basis = "model", model_M = 16L,
            cache_net = "toy", cache_dir = dir)
  expect_false(any(r1$model_info$cache_hit))
  expect_length(list.files(dir, pattern = "^toy_fold[1-3]_M16[.]rds$"), 3L)
  r2 <- one(fx$data, fx$Z, fx$fold, fold_final, TG[1:2], info_basis = "model", model_M = 16L,
            cache_net = "toy", cache_dir = dir, cache_require = TRUE)
  expect_true(all(r2$model_info$cache_hit))
  expect_identical(r1$IFP, r2$IFP)
  expect_error(one(fx$data, fx$Z, fx$fold, fold_final, TG[1:2], info_basis = "model",
                   model_M = 17L, cache_net = "toy", cache_dir = dir, cache_require = TRUE),
               "cache required")
  r3 <- one(fx$data, fx$Z, fx$fold, fold_final, TG[1:2], info_basis = "model", model_M = 16L,
            cache_net = "toy2", cache_dir = dir, cache_write = FALSE)
  expect_identical(r3$IFP, r1$IFP)
  expect_length(list.files(dir, pattern = "^toy2"), 0L)
})

test_that("the formula state and influence values work with a single feature (p = 1)", {
  # Codex review of eae4719, finding 8: vapply() returns a vector when p = 1, and t() of it
  # was 1 x n, so naming its rows by respondent failed
  set.seed(8L); n <- 6L; T <- 5L
  d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), T, replace = TRUE), T, 1))
  dat <- rum_data(lapply(seq_len(n), function(i) rep(0, T)), d, feature_names = "f1",
                  metadata = list(orientation = "test"))
  par <- rum_parameters(dat, kappa = .1, alpha = .4, loading = .3, position_variance = .5)
  st <- rinf(".rum_state_formula")(dat, par)
  expect_identical(dim(st$mu), c(n, 1L))
  expect_identical(rownames(st$mu), dat$ids)
  expect_equal(unname(st$mu[, 1]), rep(.4, n))
  # a moderated mean (respondent-specific basis), p = 1
  z <- cbind(1, seq_len(n) / n)
  dat2 <- rum_data(lapply(seq_len(n), function(i) rep(0, T)), d, feature_names = "f1",
                   mean_basis = rum_mean_basis(z, "f1"), metadata = list(orientation = "test"))
  par2 <- rum_parameters(dat2, kappa = .1, alpha = c(.4, -.2), loading = .3, position_variance = .5)
  st2 <- rinf(".rum_state_formula")(dat2, par2)
  expect_equal(unname(st2$mu[, 1]), .4 - .2 * z[, 2])
  # the formula influence values run end to end on a p = 1 fit-like object
  fit <- list(data = dat2, par = par2, information = diag(5), score = matrix(rnorm(n * 5), n, 5))
  fi <- rinf(".rum_formula_if")(fit, list(rinf(".rum_tgt_mean")("m", 1),
                                          rinf(".rum_tgt_share")("s", 1, w = c(1, 1, 1, 0, 0, 0))))
  expect_identical(dim(fi$IFP), c(n, 2L))
  expect_true(all(is.finite(fi$IFP)))
})

test_that("subgroup population-reference values are the ratio delta method at the one-step estimate", {
  # Codex review of eae4719, finding 6. est = plug + mean(phi) is nu1 / nu2 for the
  # cross-fitted moments q1 = w m + Wbar phi, q2 = w; the delta method at nu-hat (Appendix D,
  # eq. moment_delta_variance) gives phi + wr (m - est). The reproduction used
  # (phi - mean phi) + wr (m - plug), which differs by (1 - wr) mean(phi).
  for (b in c("constant", "linear")) {
    r <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, fold_final, TG, info_basis = b)
    old <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, fold_final, TG, info_basis = b,
                                pop_center = "plug_in")
    n <- fx$data$n
    for (t in seq_along(TG)) {
      w <- if (is.null(TG[[t]]$w)) rep(1, n) else as.numeric(TG[[t]]$w); wr <- w / mean(w)
      phi <- r$phi[, t]; m <- r$m[, t]; est <- unname(r$est[t])
      # the paper's construction, written out from the two moments
      q1 <- w * m + mean(w) * phi; q2 <- w
      nu1 <- mean(q1); nu2 <- mean(q2)
      expect_equal(est, nu1 / nu2, tolerance = 1e-12)
      if_delta <- ((q1 - nu1) - (nu1 / nu2) * (q2 - nu2)) / nu2
      expect_lt(max(abs(r$IFP[, t] - if_delta)), 1e-12)
      expect_lt(abs(sum(r$IFP[, t])), 1e-10)
      # the legacy values differ by exactly (1 - wr) mean(phi); not at all for wr = 1
      expect_lt(max(abs(r$IFP[, t] - old$IFP[, t] - (1 - wr) * mean(phi))), 1e-12)
      expect_identical(r$IFF[, t], old$IFF[, t]); expect_identical(r$est[t], old$est[t])
    }
    sub <- vapply(TG, function(t) !is.null(t$w), TRUE)
    expect_true(any(sub))
    expect_lt(max(abs(r$IFP[, !sub] - old$IFP[, !sub])), 1e-12)
    expect_gt(max(abs(r$IFP[, sub] - old$IFP[, sub])), 1e-8)
  }
  expect_identical(r$pop_center, "estimate")
})
