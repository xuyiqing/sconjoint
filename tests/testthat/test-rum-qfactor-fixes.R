# Fix round on the Codex review of the Stage 2 q-factor build (829393d, report/codex_review_s2.md,
# 2026-11-14). One block per finding; each block fails on 829393d and passes after its fix.
# Small simulated problems; single-threaded; nothing written outside tempdir().

rq <- function(nm) get(nm, envir = asNamespace("sconjoint"))

fx_problem <- function(n = 30L, p = 3L, n_fac = 2L, seed = 5L, lsd = .7, omega = .4) {
  set.seed(seed)
  ids <- sprintf("r%02d", seq_len(n))
  Tn <- sample(3:5, n, TRUE)
  d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), p * Tn[i], TRUE), Tn[i], p,
                                               dimnames = list(NULL, paste0("f", seq_len(p)))))
  mu <- matrix(rnorm(n * p, 0, .6), n, p, dimnames = list(ids, paste0("f", seq_len(p))))
  L <- matrix(rnorm(p * n_fac, 0, lsd), p, n_fac)
  y <- lapply(seq_len(n), function(i) {
    u <- rnorm(n_fac); eta <- .2 + d[[i]] %*% (mu[i, ] + L %*% u) + sqrt(omega) * rnorm(1)
    as.numeric(runif(Tn[i]) < plogis(drop(eta)))
  })
  list(data = rum_data(y, d, ids = ids, metadata = list(orientation = "y = 1 picks the first profile")),
       mu = mu, kappa = .2, L = L, omega = omega)
}

# ---- finding 3: the trainer's default starts at p = 1, q > 1 ---------------------------------
test_that("finding 3: default starts work with one feature and two factors", {
  set.seed(31)
  P <- fx_problem(n = 30L, p = 1L, n_fac = 2L, seed = 31L)
  Z <- matrix(rnorm(30 * 2), 30, 2, dimnames = list(P$data$ids, c("z1", "z2")))
  ctl <- list(max_epochs = 2L, validation_every = 1L, patience = 2L, block_size = 16L,
              max_seconds = 600, start_seconds = 200, refit_seconds = 150, evaluation_reserve = 60)
  quad <- list(train = rum_quadrature(c(5L, 5L, 5L), n_fac = 2L, rule = "product"),
               check = list(rum_quadrature(c(7L, 7L, 7L), n_fac = 2L, rule = "product")))
  fit <- rum_dnn_gaussian_train(P$data, Z, P$data$ids[1:6], hidden = 2L, seeds = c(42L, 7L),
                                control = ctl, n_fac = 2L, quadrature = quad)
  expect_true(fit$completed)
  pat <- function(i) .1 * switch(i, sin(1), cos(1), -1)
  for (s in 1:2) {
    idx <- (s - 1L) %% 3L + 1L
    expect_identical(fit$starts[[s]]$initial_state$loading,
                     matrix(c(pat(idx), pat((idx + 2L - 2L) %% 3L + 1L)), 1L, 2L))
  }
  expect_identical(dim(fit$final$loading), c(1L, 2L))
})

# ---- finding 8: .rum_cond_options() at one factor returns the f20956e object ---------------
test_that("finding 8: the conditional options keep the one-factor defaults (method exact, M 200)", {
  co <- rq(".rum_cond_options")
  # the f20956e constructor's output, written out
  old <- function(spec, resp = NULL, method = "exact", M = NULL, ny = 8L, louis_rb = TRUE,
                  seed = 20261011L, quad = rq(".rum_info_quadrature")(), cache_dir = NULL,
                  cache_net = NULL, cache_write = TRUE, cache_require = FALSE, crn = NULL,
                  null_zero = FALSE, draws = "common")
    list(spec = spec, resp = resp, method = method, crn = crn, null_zero = null_zero, draws = draws,
         M = if (is.null(M)) (if (method == "exact") 200L else 400L) else as.integer(M),
         ny = as.integer(ny), louis_rb = louis_rb, seed = as.integer(seed), quad = quad,
         cache_dir = cache_dir, cache_net = cache_net, cache_write = cache_write,
         cache_require = cache_require)
  expect_identical(co("br"), old("br"))
  expect_identical(co("br")$method, "exact"); expect_identical(co("br")$M, 200L)
  expect_identical(co("gs", resp = data.frame(pid7 = 1:3), M = 50L, draws = "own"),
                   old("gs", resp = data.frame(pid7 = 1:3), M = 50L, draws = "own"))
  expect_identical(co("sw_additive", method = "louis"), old("sw_additive", method = "louis"))
  expect_identical(co("br", method = "mc", ny = 4L, null_zero = TRUE), old("br", method = "mc", ny = 4L, null_zero = TRUE))
  expect_identical(co("br", n_fac = 1L), old("br"))
  # automatic selection only when asked for: method "auto", or n_fac > 1 with the default method
  expect_identical(co("br", n_fac = 2L)$method, "auto"); expect_true(is.na(co("br", n_fac = 2L)$M))
  expect_identical(co("br", method = "exact", n_fac = 2L)$M, 200L)
  expect_identical(co("br", method = "auto")$method, "auto")
  expect_null(co("br", n_fac = 3L)$n_fac)
})
