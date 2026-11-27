# Ported from conjointRUM 0.1.3 tests/data-fit.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

library(sconjoint)

checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
rejects <- function(expr) check(inherits(tryCatch(force(expr), error = identity), "error"))
near <- function(a, b, tolerance = 1e-11) check(max(abs(a - b)) < tolerance)

d <- list(matrix(c(1, 0, -1, 1, 0, -1), 3, 2),
          matrix(c(1, -1, 0, 1), 2, 2), matrix(c(1, 1), 1, 2))
y <- list(c(1, 0, 1), c(0, 1), 0)
z <- cbind(intercept = 1, z = c(-1, 0, 1))
bs <- rum_mean_basis(z, c("first", "second"))
meta <- list(orientation = "y=1 selects synthetic displayed-left profile", source = "synthetic")
dat <- rum_data(y, d, bs, c("c", "a", "b"), c("first", "second"), metadata = meta)
check(identical(dat$T, c(3L, 2L, 1L)))
check(identical(dat$offsets, c(0L, 3L, 5L, 6L)))
check(identical(dat$ids, c("c", "a", "b")))
check(identical(dat$y, c(1, 0, 1, 0, 1, 0)))
near(dat$d, do.call(rbind, d))
near(dat$a, do.call(rbind, lapply(1:3, function(i) d[[i]] %*% bs[[i]])))
near(bs[[1]], cbind(diag(2), -diag(2)))
near(bs[[3]], cbind(diag(2), diag(2)))
sub <- rum_subset(dat, c("b", "c"))
check(identical(sub$T, c(1L, 3L)))
check(identical(sub$ids, c("b", "c")))
check(identical(sub$y, c(0, 1, 0, 1)))
check(identical(sub$metadata, meta))

rejects(rum_data(y, d))
rejects(rum_data(y, d, metadata = list(orientation = "")))
rejects(rum_data(y, d, metadata = list(orientation = c("left", "right"))))
rejects(rum_data(y, d, ids = c("x", "x", "z"), metadata = meta))
rejects(rum_data(y, d, ids = c("x", NA, "z"), metadata = meta))
rejects(rum_data(y[-1], d, metadata = meta))
rejects(rum_data(list(c(1, NA, 0), y[[2]], y[[3]]), d, metadata = meta))
rejects(rum_data(list(c(1, 2, 0), y[[2]], y[[3]]), d, metadata = meta))
rejects(rum_data(list(c(1, 0), y[[2]], y[[3]]), d, metadata = meta))
bad <- d; bad[[1]][1] <- Inf
rejects(rum_data(y, bad, metadata = meta))
bad <- d; bad[[2]] <- matrix(1, 2, 3)
rejects(rum_data(y, bad, metadata = meta))
rejects(rum_data(y, d, matrix(1, 3, 2), metadata = meta))
rejects(rum_data(y, d, bs[-1], metadata = meta))
rejects(rum_data(y, d, feature_names = c("x", "x"), metadata = meta))
rejects(rum_subset(dat, c(1, 1)))
rejects(rum_subset(dat, "unknown"))
rejects(rum_scale_features(dat, c(1, 0)))

par <- rum_parameters(dat, .2, c(-.2, .3, .4, -.1), c(.8, -.5), .4)
scales <- c(2, .3)
scaled <- rum_scale_features(dat, scales)
ps <- rum_scale_parameters(dat, par, scales)
check(identical(scaled$a, dat$a))
near(rum_scale_parameters(dat, ps, scales, inverse = TRUE), par)
near(ps[2:5], par[2:5])
near(ps[6:7], par[6:7] * scales)
for (i in seq_len(dat$n)) {
  ix <- dat$offsets[i] + seq_len(dat$T[i])
  near(scaled$d[ix, , drop = FALSE] %*% scaled$basis[[i]], dat$a[ix, , drop = FALSE])
}
near(drop(dat$a %*% par[2:5] + dat$d %*% par[6:7] * .37),
     drop(scaled$a %*% ps[2:5] + scaled$d %*% ps[6:7] * .37))
q <- rum_quadrature(c(9L, 11L))
near(rum_loglik(dat, par, q)$loglik, rum_loglik(scaled, ps, q)$loglik)

# An independent literal reconstruction checks shared latent draws and private RNG.
set.seed(2026091390)
before <- .Random.seed
sim <- rum_simulate(dat, par, seed = 2026091391)
check(identical(before, .Random.seed))
set.seed(2026091391)
u <- rnorm(dat$n); v <- rnorm(dat$n)
eta <- par[1] + drop(dat$a %*% par[2:5]) +
  drop(dat$d %*% par[6:7]) * rep(u, dat$T) + sqrt(par[8]) * rep(v, dat$T)
expected_y <- rbinom(length(eta), 1, plogis(eta))
check(identical(sim$y, expected_y))
check(identical(sim$T, dat$T) && identical(sim$d, dat$d) && identical(sim$a, dat$a))
check(identical(rum_simulate(dat, par, seed = 2026091391)$y, sim$y))

# Cheap rejection checks reach fit validation without fitting an application.
rejects(rum_fit(dat, control = list(not_a_control = 1)))
rejects(rum_fit(dat, control = list(max_seconds = -1)))
rejects(rum_fit(dat, orders = c(15, 9)))
alias_basis <- matrix(c(1, 0, 1, 0), 2, 2)
aliased <- rum_data(y, d, alias_basis, metadata = meta)
rejects(rum_fit(aliased))

# Literal boundary parameter records must not acquire regular full-model intervals,
# even when supplied an optimistic regular_candidate flag and nonsingular fake I.
for (case in c("zero_policy", "zero_position")) {
  pb <- par
  if (case == "zero_policy") pb[6:7] <- 0 else pb[8] <- 0
  ev <- rum_loglik(dat, pb, q)
  if (case == "zero_policy") near(ev$score[, 6:7], matrix(0, dat$n, 2))
  check(all(is.finite(ev$loglik)) && all(is.finite(ev$score)))
  ff <- list(data = dat, par = pb, score = ev$score, information = diag(length(pb)),
             regular_candidate = TRUE)
  tt <- rum_targets(ff, list(first = c(1, 0)), reference = "fixed")
  check(all(tt$table$status == "full_model_boundary"))
  check(all(is.na(tt$table$lower)) && all(is.na(tt$table$upper)))
}
cat(sprintf("data-fit: %d checks passed\n", checks))
