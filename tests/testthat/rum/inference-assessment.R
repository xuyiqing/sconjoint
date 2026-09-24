# Ported from conjointRUM 0.1.3 tests/inference-assessment.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Formula and finite-difference tests. These algebra fixtures are not MLE
# coverage experiments; complete-estimator calibration is a separate task.
library(sconjoint)
if (identical(Sys.getenv("RUM_TEST_SOURCE"), "1")) {
  .rum_with_seed <- getFromNamespace(".rum_with_seed", "sconjoint")
  .rum_check_data <- getFromNamespace(".rum_check_data", "sconjoint")
  .rum_check_par <- getFromNamespace(".rum_check_par", "sconjoint")
  rum_moments_cpp <- getFromNamespace("rum_moments_cpp", "sconjoint")
  source("R/rum-targets.R")
  source("R/rum-assessment.R")
}
moments_native <- getFromNamespace("rum_moments_cpp", "sconjoint")
checks <- list()
check <- function(name, actual, expected, tolerance = 1e-9) {
  shape <- identical(dim(actual), dim(expected)) && length(actual) == length(expected)
  error <- if (shape && all(is.finite(actual)) && all(is.finite(expected)))
    max(c(0, abs(actual - expected))) else Inf
  checks[[length(checks) + 1L]] <<- data.frame(check = name, error = error,
    tolerance = tolerance, passed = error <= tolerance, stringsAsFactors = FALSE)
}
check_true <- function(name, x) check(name, as.numeric(isTRUE(x)), 1, 0)
check_error <- function(name, expr) check_true(name, tryCatch({ force(expr); FALSE }, error = function(e) TRUE))
cov_rows <- function(x) {
  a <- sweep(x, 2L, colMeans(x), "-")
  crossprod(a) / nrow(a)
}

n <- 12L; p <- 2L; r <- 4L
z <- seq(-1, 1, length.out = n)
basis <- lapply(z, function(v) matrix(c(1, v, 0, 0, 0, 0, 1, v), 2, 4, byrow = TRUE))
design <- lapply(seq_len(n), function(i) matrix(c(
  sin(i), cos(i * .7), -.6 + .1 * i, sin(i * 1.3), cos(i * .4), -.8 + .09 * i), 3, 2, byrow = TRUE))
ys <- lapply(seq_len(n), function(i) as.numeric(c(i %% 2, (i %/% 2) %% 2, (i %/% 3) %% 2)))
data <- rum_data(ys, design, basis, metadata = list(orientation = "y=1 denotes physical left"))
par <- rum_parameters(data, .15, c(.4, .2, -.1, .3), c(.8, -.4), .3)
quad <- rum_quadrature(c(15L, 15L))
h <- length(par)
# A full positive definite information fixture deliberately includes a
# position/mean nuisance correlation, for testing the complete matrix solve.
v <- seq(.1, .8, length.out = h)
info <- diag(h) + tcrossprod(v)
score <- matrix(sin(seq_len(n * h) * .37), n, h)
score <- sweep(score, 2L, colMeans(score), "-")
fit <- list(data = data, par = par, quadrature = quad, score = score,
            information = info, regular_candidate = TRUE)
contrasts <- list(first = c(1, 0), second = c(0, 1), varying = cbind(1, z))
targets <- rum_targets(fit, contrasts)
ix <- function(type, c1, c2 = NA_character_) which(targets$table$type == type &
  targets$table$contrast1 == c1 & if (is.na(c2)) is.na(targets$table$contrast2) else targets$table$contrast2 == c2)
row_value <- function(type, c1, c2 = NA_character_) targets$table$estimate[ix(type, c1, c2)]
m1 <- .4 + .2 * z; m2 <- -.1 + .3 * z
check("fixed mean", row_value("mean", "first"), mean(m1))
check("HZ uses denominator N", row_value("HZ", "first"), mean((m1 - mean(m1))^2))
check("fixed HR", row_value("HR", "first"), .8^2)
check("KZ centered means", row_value("KZ", "first", "second"), mean((m1 - mean(m1)) * (m2 - mean(m2))))
check("KR signed cross-loading", row_value("KR", "first", "second"), -.32)
check("varying HR is mean squared loading", row_value("HR", "varying"), mean((.8 - .4 * z)^2))
check("composition centered", colMeans(targets$composition), rep(0, ncol(targets$composition)))
check("composition HZ", targets$composition[, ix("HZ", "first")], (m1 - mean(m1))^2 - mean((m1 - mean(m1))^2))
check("varying HR composition", targets$composition[, ix("HR", "varying")], (.8 - .4 * z)^2 - mean((.8 - .4 * z)^2))
check("fixed HR no composition", targets$composition[, ix("HR", "first")], rep(0, n))
fixed <- rum_targets(fit, contrasts, reference = "fixed")
check("fixed reference removes composition", fixed$composition, matrix(0, n, nrow(fixed$table)))
check("reference changes no point targets", fixed$table$estimate, targets$table$estimate)

fd <- matrix(0, nrow(targets$table), h)
for (j in seq_len(h)) {
  eps <- 1e-6
  a <- b <- fit; a$par[j] <- a$par[j] + eps; b$par[j] <- b$par[j] - eps
  fd[, j] <- (rum_targets(a, contrasts, infer = FALSE)$table$estimate -
                rum_targets(b, contrasts, infer = FALSE)$table$estimate) / (2 * eps)
}
check("all primary analytic gradients vs FD", targets$gradient, fd, 2e-9)
manual_if <- targets$composition + score %*% solve(info, t(targets$gradient))
check("target influence full nuisance information", targets$influence, manual_if, 1e-12)
check("target covariance from complete influence", targets$covariance, cov_rows(manual_if) / n, 1e-12)
reduced <- targets$composition + score[, -h, drop = FALSE] %*%
  solve(info[-h, -h], t(targets$gradient[, -h, drop = FALSE]))
check_true("position nuisance cannot silently be dropped", max(abs(manual_if - reduced)) > 1e-5)

zero <- fit; zero$par[1L + r + 2L] <- 0
zz <- rum_targets(zero, list(orthogonal = c(0, 1)))
rr <- which(zz$table$type == "HR"); ll <- which(zz$table$type == "loading")
check("derived zero HR point", zz$table$estimate[rr], 0)
check("derived zero HR interval starts at zero", zz$table$lower[rr], 0)
check_true("derived zero has positive projected upper", zz$table$upper[rr] > 0 &&
             zz$table$interval_method[rr] == "squared_loading_projection")
check("square projection matches signed interval", zz$table$upper[rr], max(c(zz$table$lower[ll], zz$table$upper[ll])^2))
boundary <- fit; boundary$par[h] <- 0
bb <- rum_targets(boundary, contrasts)
check_true("omega boundary withholds", all(is.na(bb$table$lower)) && all(bb$table$status == "full_model_boundary"))
boundary$par[h] <- .3; boundary$par[1L + r + seq_len(p)] <- 0
check_true("entire loading boundary withholds", all(is.na(rum_targets(boundary, contrasts)$table$lower)))
singular <- fit; singular$information[h, ] <- singular$information[, h] <- 0
check_true("singular information is not ridged", all(is.na(rum_targets(singular, contrasts)$table$lower)))
bad <- fit; bad$regular_candidate <- FALSE
check_true("numerical noncandidate withholds", all(is.na(rum_targets(bad, contrasts)$table$lower)))
deg <- rum_targets(fit, list(zero_contrast = c(0, 0)))
check_true("degenerate target has no SE floor", all(deg$table$se == 0) && all(is.na(deg$table$lower)))
invalid <- fit; invalid$par[h] <- -.1
check_error("invalid negative variance rejected for point targets", rum_targets(invalid, contrasts, infer = FALSE))

J <- 5L
mw <- matrix(0, length(data$y), J); pw <- matrix(0, 3L * n, J); cw <- matrix(0, 4L * n, J)
for (i in seq_len(n)) {
  tx <- (i - 1L) * 3L + seq_len(3L); px <- tx; cx <- (i - 1L) * 4L + seq_len(4L)
  mw[tx, 1L] <- 1 / 3; pw[px, 2L] <- 1 / 3
  cw[cx[c(1L, 4L)], 3L] <- 1
  mw[tx, 4L] <- c(.5, 1, .5); pw[px[c(1L, 3L)], 4L] <- -1
}
bank <- rum_moment_bank(data, mw, pw, cw, c("mean", "pairs", "extreme", "switch", "identity"))
summaries <- function(y) c(mean(y), (y[1] * y[2] + y[1] * y[3] + y[2] * y[3]) / 3,
                          as.numeric(sum(y) %in% c(0, 3)), mean(diff(y) != 0), 0)
observed <- t(vapply(ys, summaries, numeric(J)))
check("bank observed independent direct summaries", bank$observed, observed)
check_true("constant identity recognized response-free", identical(unname(bank$structural_identity), c(FALSE, FALSE, FALSE, FALSE, TRUE)))
known <- rum_assess(data, par, bank, quad, nsim = 63L, seed = 7L)
expected <- matrix(0, n, J)
patterns <- as.matrix(expand.grid(rep(list(c(0, 1)), 3L)))
for (i in seq_len(n)) {
  ix <- data$offsets[i] + seq_len(3L)
  eta0 <- par[1] + as.numeric(data$a[ix, , drop = FALSE] %*% par[1L + seq_len(r)])
  eta <- outer(as.numeric(data$d[ix, , drop = FALSE] %*% par[1L + r + seq_len(p)]), quad$u)
  eta <- sweep(eta, 1L, eta0, "+")
  eta <- sweep(eta, 2L, sqrt(par[h]) * quad$v, "+")
  prob <- stats::plogis(eta)
  for (k in seq_len(nrow(patterns))) {
    y <- patterns[k, ]; mass <- sum(quad$weight * apply(prob^y * (1 - prob)^(1 - y), 2L, prod))
    expected[i, ] <- expected[i, ] + mass * summaries(y)
  }
}
check("native expectations vs independent enumeration", known$expected, expected, 2e-13)
check("known parameter uses raw residual covariance", known$covariance, cov_rows(observed - expected), 2e-13)
check("known parameter has no fit correction", known$influence, observed - expected, 2e-13)
check_true("singular joint covariance permits Gaussian maximum", is.finite(known$p_value) &&
             min(eigen(known$covariance[1:4, 1:4], symmetric = TRUE)$values) < 1e-12)
check_true("sample oracle residuals need not vanish", max(abs(colMeans(known$residual))) > .005)
same <- rum_assess(data, bank = bank, estimated = TRUE, fit = fit, nsim = 63L, seed = 7L)
manual_adjusted <- known$residual - score %*% solve(info, t(same$derivative))
check("same-fit r minus A inverse I score", same$influence, manual_adjusted, 1e-12)
check("same-fit centered IF covariance", same$covariance, cov_rows(manual_adjusted), 1e-12)
check_true("known and fitted covariance genuinely differ", max(abs(known$covariance - same$covariance)) > 1e-5)
check_true("structural zero coordinate is excluded not floored", same$table$status[5L] == "structural_identity_not_tested")

model_eval <- function(x) moments_native(data$d, data$a, data$offsets, x,
  quad$u, quad$v, quad$weight, mw, pw, cw)$expected
fd_m <- matrix(0, J, h)
for (j in seq_len(h)) {
  a <- b <- par; eps <- 2e-5; a[j] <- a[j] + eps; b[j] <- b[j] - eps
  fd_m[, j] <- colMeans(model_eval(a) - model_eval(b)) / (2 * eps)
}
check("assessment moment derivative vs independent FD", same$derivative, fd_m, 2e-9)
check_true("boundary fit assessment withheld", all(is.na(rum_assess(data, bank = bank, estimated = TRUE,
  fit = boundary, nsim = 7L)$table$lower)))
check_true("known boundary can still assess observable moments", is.finite(rum_assess(data, boundary$par, bank,
  quad, nsim = 7L)$p_value))
changed <- data; changed$y <- 1 - data$y
changed_known <- rum_assess(changed, par, bank, quad, nsim = 7L)
check("observed bank refreshed for new outcomes", changed_known$observed,
      t(vapply(ys, function(y) summaries(1 - y), numeric(J))))
check_error("same-fit mismatch rejected", rum_assess(changed, bank = bank, estimated = TRUE, fit = fit))
check_error("bank feature mismatch rejected", { changed$d[1, 1] <- changed$d[1, 1] + .1; rum_assess(changed, par, bank, quad) })

set.seed(442L); previous_rng <- .Random.seed
invisible(rum_assess(data, par, bank, quad, nsim = 9L, seed = 22L))
check_true("calibration restores user RNG", identical(previous_rng, .Random.seed))
hold <- rum_holdout_assess(data, par, bank, quad, method = "hoeffding",
                            residual_bounds = cbind(rep(-1, J), rep(1, J)), tolerances = rep(5, J))
hw <- 2 * sqrt(log(2 * J / .05) / (2 * n))
check("Hoeffding width formula", hold$table$upper - hold$table$residual, rep(hw, J))
check_true("declared tolerances assessed", hold$all_within_tolerance)
check_error("Hoeffding cannot infer population bound from sample", rum_holdout_assess(data, par, bank, quad, method = "hoeffding"))
quad_b <- rum_quadrature(c(3L, 15L), policy_law = "three_point")
cmp <- rum_score_compare(data, par, par, quad, quadrature_b = quad_b)
check("paired full sequence score differences", cmp$differences,
      rum_loglik(data, par, quad)$loglik - rum_loglik(data, par, quad_b)$loglik)
check_true("identical predictors no fake standard error", is.na(rum_score_compare(data, par, par, quad)$lower))

result <- do.call(rbind, checks)
print(result, row.names = FALSE)
if (!all(result$passed)) stop("Inference/assessment checks failed")
cat(sprintf("All %d inference/assessment checks passed.\n", nrow(result)))
