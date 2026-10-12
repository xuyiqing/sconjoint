# Tests of the conditional (per-respondent) information (R/rum-information-conditional.R):
# exact enumeration, the Louis identity and the simulated-choice estimator on tiny designs,
# and the info_basis = "conditional" path of the one-step on the inference fixture.
# Single-threaded, small quadratures; nothing is written outside tempdir().

rinf <- function(nm) get(nm, envir = asNamespace("sconjoint"))
small_quad <- function() rum_quadrature(c(9L, 7L))
tiny <- function(seed = 1L, p = 2L, T = 3L, n = 3L, M = 4L) {
  set.seed(seed)
  list(p = p, T = T, n = n, M = M, mu = matrix(stats::rnorm(n * p), n, p),
       th = list(kappa = 0.2, loading = stats::rnorm(p, 0, 0.5), omega = 0.4),
       designs = array(sample(c(-1, 0, 1), M * T * p, replace = TRUE), c(M, T, p)))
}
tiny_data <- function(tn) {
  d <- lapply(seq_len(tn$n), function(i) tn$designs[1, , ])
  rum_data(lapply(seq_len(tn$n), function(i) rep(0, tn$T)), d,
           metadata = list(orientation = "test"))
}
tiny_crn <- function(tn) list(designs = list(all = tn$designs), key = rep(1L, tn$n), M = tn$M, T = tn$T)
exact_I_one <- function(Dm, mu_i, th, quad) {   # sum_y p(y) s s' and the pieces, via .rum_seq_scores
  T <- nrow(Dm); p <- ncol(Dm); Y <- rinf(".rum_cond_Y")(T); R <- nrow(Y)
  sc <- function(mu_i, th) rinf(".rum_seq_scores")(matrix(mu_i, R, p, byrow = TRUE),
          lapply(seq_len(T), function(t) matrix(Dm[t, ], R, p, byrow = TRUE)), Y, th, quad)
  a <- th$kappa + drop(Dm %*% mu_i); b <- drop(Dm %*% th$loading)
  e <- rinf(".rum_cond_eta")(a, b, sqrt(th$omega), quad)
  py <- exp(rinf(".rum_cond_post")(Y, e, log(quad$weight))$logp)
  S <- sc(mu_i, th)
  list(I = crossprod(S * py, S), S = S, py = py, sc = sc)
}

test_that("exact: c(y) mapped by A_D equals .rum_seq_scores, and sum_y p s = 0", {
  tn <- tiny(); quad <- small_quad(); Dm <- tn$designs[1, , ]
  ref <- exact_I_one(Dm, tn$mu[1, ], tn$th, quad)
  Y <- rinf(".rum_cond_Y")(tn$T); s <- sqrt(tn$th$omega)
  a <- tn$th$kappa + drop(Dm %*% tn$mu[1, ]); b <- drop(Dm %*% tn$th$loading)
  e <- rinf(".rum_cond_eta")(a, b, s, quad)
  cc <- rinf(".rum_cond_c")(Y, rinf(".rum_cond_post")(Y, e, log(quad$weight))$Pi, e, quad)
  A <- rinf(".rum_cond_A")(Dm, s)
  expect_lt(max(abs(cc %*% t(A) - ref$S)), 1e-12)
  expect_lt(abs(sum(ref$py) - 1), 1e-12)
  expect_lt(max(abs(colSums(ref$S * ref$py))), 1e-12)
  K <- rinf(".rum_cond_K")(a, b, s, quad, "exact", Y)
  expect_lt(max(K$check), 1e-12)
  expect_lt(max(abs(A %*% K$K %*% t(A) - ref$I)), 1e-12)
})

test_that("exact: information identity E[s s'] = E[-d s / d theta]", {
  tn <- tiny(seed = 2L); quad <- small_quad(); Dm <- tn$designs[2, , ]; p <- tn$p
  ref <- exact_I_one(Dm, tn$mu[2, ], tn$th, quad)
  h <- 1e-5; D <- 2L * p + 2L; H <- matrix(0, D, D)
  v0 <- c(tn$mu[2, ], tn$th$kappa, tn$th$loading, tn$th$omega)
  unp <- function(v) list(mu = v[seq_len(p)], th = list(kappa = v[p + 1L], loading = v[p + 1L + seq_len(p)],
                                                       omega = v[D]))
  for (j in seq_len(D)) {
    v1 <- v0; v2 <- v0; v1[j] <- v1[j] + h; v2[j] <- v2[j] - h
    a1 <- unp(v1); a2 <- unp(v2)
    H[, j] <- -colSums(ref$py * (ref$sc(a1$mu, a1$th) - ref$sc(a2$mu, a2$th))) / (2 * h)
  }
  expect_lt(max(abs((H + t(H)) / 2 - ref$I)) / max(abs(ref$I)), 1e-6)
})

test_that("Louis and simulated choices agree with exact enumeration within Monte Carlo error", {
  tn <- tiny(seed = 3L); quad <- small_quad(); data <- tiny_data(tn); crn <- tiny_crn(tn)
  ci <- rinf(".rum_cond_info")
  ex <- ci(data, tn$mu, tn$th, crn, "exact", quad)$I
  rel <- function(X) max(abs(X - ex)) / max(abs(ex))
  lo <- ci(data, tn$mu, tn$th, crn, "louis", quad, ny = 4000L, seed = 5L)$I
  ln <- ci(data, tn$mu, tn$th, crn, "louis", quad, ny = 4000L, seed = 5L, louis_rb = FALSE)$I
  expect_lt(rel(lo), 0.03); expect_lt(rel(ln), 0.03)
  # simulated choices: the error shrinks as the draws grow
  e_small <- mean(vapply(1:4, function(s) rel(ci(data, tn$mu, tn$th, crn, "mc", quad, ny = 50L, seed = s)$I), 0))
  e_big <- rel(ci(data, tn$mu, tn$th, crn, "mc", quad, ny = 5000L, seed = 9L)$I)
  expect_lt(e_big, 0.03); expect_lt(e_big, e_small)
})

test_that("conditional information is symmetric PSD; halves average to the whole", {
  tn <- tiny(seed = 4L, M = 5L); quad <- small_quad(); data <- tiny_data(tn); crn <- tiny_crn(tn)
  for (m in c("exact", "louis", "mc")) {
    r <- rinf(".rum_cond_info")(data, tn$mu, tn$th, crn, m, quad, ny = 50L, halves = TRUE)
    expect_identical(dim(r$I), c(tn$n, 2L * tn$p + 2L, 2L * tn$p + 2L))
    for (i in seq_len(tn$n)) {
      Ii <- r$I[i, , ]
      expect_lt(max(abs(Ii - t(Ii))), 1e-12)
      if (m != "louis") expect_gt(min(eigen(Ii, symmetric = TRUE, only.values = TRUE)$values),
                                  -1e-10 * max(abs(Ii)))
    }
    expect_lt(max(abs((3 * r$IA + 2 * r$IB) / 5 - r$I)), 1e-12)
  }
})

test_that("a respondent who answered fewer tasks uses the first T_i tasks of each draw", {
  tn <- tiny(seed = 5L, n = 2L, T = 3L); quad <- small_quad(); crn <- tiny_crn(tn)
  d <- list(tn$designs[1, , ], tn$designs[1, 1:2, , drop = FALSE][1, , ])
  data <- rum_data(list(rep(0, 3), rep(0, 2)), d, metadata = list(orientation = "test"))
  r <- rinf(".rum_cond_info")(data, tn$mu, tn$th, crn, "exact", quad)
  ref <- Reduce(`+`, lapply(seq_len(tn$M), function(m)
    exact_I_one(matrix(tn$designs[m, 1:2, ], 2), tn$mu[2, ], tn$th, quad)$I)) / tn$M
  expect_lt(max(abs(r$I[2, , ] - ref)), 1e-12)
})

test_that("info_basis = 'conditional' runs through the one-step and the CV-TMLE; floors are reported", {
  fx <- readRDS(test_path("fixtures", "rum-inference-fixture.rds"))
  set.seed(7L); M <- 6L; T <- max(fx$data$T); p <- fx$data$p
  crn <- list(designs = list(all = array(sample(c(-1, 0, 1), M * T * p, replace = TRUE), c(M, T, p))),
              key = rep(1L, fx$data$n), M = M, T = T)
  tg <- list(rinf(".rum_tgt_mean")("m1", replace(numeric(p), 1, 1)))
  cache <- file.path(tempdir(), "rum-cond-cache")
  cond <- rinf(".rum_cond_options")(NULL, method = "exact", crn = crn, quad = small_quad(),
                                    cache_dir = cache, cache_net = "fx")
  r <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, function(k) fx$fins[[k]], tg,
                            info_basis = "conditional", cond = cond)
  expect_true(is.finite(r$est[1]) && all(is.finite(r$IFF)))
  expect_identical(r$info_basis, "conditional")
  expect_true(all(!r$model_info$cache_hit))
  r2 <- rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, function(k) fx$fins[[k]], tg,
                             info_basis = "conditional", cond = cond)
  expect_true(all(r2$model_info$cache_hit))
  expect_lt(abs(r2$est[1] - r$est[1]), 1e-12)
  tm <- rinf(".rum_tmle_dnn")(fx$data, fx$Z, fx$fold, function(k) fx$fins[[k]], tg,
                              info_basis = "conditional", cond = cond)
  expect_true(is.finite(tm$estimate[1]))
  # a coordinate that is zero in every design gives an exactly null direction, reported
  crn0 <- crn; crn0$designs$all[, , 2] <- 0
  I0 <- rinf(".rum_cond_info")(fx$data, matrix(0, fx$data$n, p), fx$fins[[1]][c("kappa", "loading", "omega")],
                               crn0, "exact", small_quad())$I
  fr <- rinf(".rum_cond_floor_report")(I0[1:3, , ], 1e-6, paste0("c", 1:(2 * p + 2)))
  expect_identical(nrow(fr), 6L)                     # mu_2 and loading_2, three respondents
  expect_true(all(grepl("c2|c6", fr$direction)))
  expect_error(rinf(".rum_onestep")(fx$data, fx$Z, fx$fold, function(k) fx$fins[[k]], tg,
                                    info_basis = "conditional"), "cond")
})

test_that("null_zero: no weight on exactly null coordinates; identical to the floor otherwise", {
  tn <- tiny(seed = 6L, n = 3L, M = 6L, p = 3L); quad <- small_quad(); data <- tiny_data(tn)
  I <- rinf(".rum_cond_info")(data, tn$mu, tn$th, tiny_crn(tn), "exact", quad)$I
  p <- tn$p; q <- p + 2L
  W1 <- rinf(".rum_weight_parts")(function(i) I[i, , ], tn$n, p, q, 1e-6)
  W2 <- rinf(".rum_cond_weight_parts")(I, p, q, 1e-6)
  expect_lt(max(abs(W1$Li - W2$Li)), 1e-10); expect_lt(max(abs(W1$Ic - W2$Ic)), 1e-10)
  crn0 <- tiny_crn(tn); z <- crn0$designs$all; z[, , 2] <- 0       # respondents 2 and 3 never see coordinate 2
  crn0$designs <- list(a = crn0$designs$all, z = z); crn0$key <- c(1L, 2L, 2L)
  I0 <- rinf(".rum_cond_info")(data, tn$mu, tn$th, crn0, "exact", quad)$I
  W0 <- rinf(".rum_cond_weight_parts")(I0, p, q, 1e-6)
  expect_identical(W0$null, 2L)
  expect_true(all(W0$Li[2:3, 2, ] == 0) && all(W0$Li[2:3, , 2] == 0) && all(W0$Li[1, 2, ] != 0))
  v <- matrix(1, tn$n, p)
  a <- rinf(".rum_correction")(W0, v, rep(0.1, q), seq_len(tn$n))$a
  expect_true(all(a[2:3, 2] == 0) && a[1, 2] != 0 && all(is.finite(a)))
})

test_that("own draws: with every respondent given the shared draws, exact equals the common path", {
  tn <- tiny(seed = 8L, n = 3L, M = 5L); quad <- small_quad(); data <- tiny_data(tn); crn <- tiny_crn(tn)
  ci <- rinf(".rum_cond_info")
  a <- ci(data, tn$mu, tn$th, crn, "exact", quad, halves = TRUE)
  own <- list(own = TRUE, M = tn$M, T = tn$T, designs_of = function(i) tn$designs)
  b <- ci(data, tn$mu, tn$th, own, "exact", quad, halves = TRUE)
  expect_lt(max(abs(a$I - b$I)), 1e-12); expect_lt(max(abs(a$IA - b$IA)), 1e-12)
  l <- ci(data, tn$mu, tn$th, own, "louis", quad, ny = 3000L, seed = 2L)
  expect_lt(max(abs(l$I - a$I)) / max(abs(a$I)), 0.05)
})
