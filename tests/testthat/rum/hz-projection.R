# Ported from conjointRUM 0.1.3 tests/hz-projection.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Independent algebra tests of fixed-reference HZ confidence-set projection.
# These are regular-covariance fixtures, not fitted models or coverage evidence.
# Source the current target code when available; keep installed native code.
argv <- commandArgs(FALSE)
arg <- grep("^--file=", argv, value = TRUE)
script <- if (length(arg)) sub("^--file=", "", arg[1L]) else ""
roots <- c(if (nzchar(script)) dirname(dirname(normalizePath(script))),
           getwd(), dirname(getwd()))
found <- vapply(roots, function(x) file.exists(file.path(x, "R", "rum-targets.R")), logical(1))
root <- if (any(found)) roots[which(found)[1L]] else NA_character_
if (!is.na(root) && dir.exists(file.path(root, ".local-library")))
  .libPaths(c(file.path(root, ".local-library"), .libPaths()))
if (!requireNamespace("sconjoint", quietly = TRUE)) stop("Install sconjoint before this test")
ns <- asNamespace("sconjoint")
api <- new.env(parent = ns)
if (!is.na(root)) sys.source(file.path(root, "R", "rum-targets.R"), envir = api)
targets <- get("rum_targets", envir = api, inherits = TRUE)
make_data <- get("rum_data", envir = ns)
make_basis <- get("rum_mean_basis", envir = ns)
make_par <- get("rum_parameters", envir = ns)

checks <- list()
check <- function(name, actual, expected, tol = 2e-11) {
  shape <- identical(dim(actual), dim(expected)) && length(actual) == length(expected)
  error <- if (shape && all(is.finite(actual)) && all(is.finite(expected)))
    max(c(0, abs(actual - expected))) else Inf
  checks[[length(checks) + 1L]] <<- data.frame(check = name,
    max_abs_error = error, tolerance = tol, passed = error <= tol,
    stringsAsFactors = FALSE)
}
check_true <- function(name, x) check(name, as.numeric(isTRUE(x)), 1, 0)
check_error <- function(name, expr) check_true(name,
  tryCatch({force(expr); FALSE}, error = function(e) TRUE))
hz <- function(x) x$table[x$table$type == "HZ", , drop = FALSE]
image_square <- function(interval) c(
  if (interval[1] <= 0 && interval[2] >= 0) 0 else min(interval^2), max(interval^2))
cov_n <- function(x) {
  centered <- sweep(as.matrix(x), 2L, colMeans(as.matrix(x)), "-")
  crossprod(centered) / nrow(centered)
}
project <- function(fit, contrast = c(1, -.4), ...) targets(fit,
  list(policy = contrast), reference = "fixed",
  hz_interval = "fixed_rank_one_projection", ...)

n <- 80L; p <- 2L; r <- 4L; h <- 2L + p + r
z <- seq(-1.4, 2.2, length.out = n)
design <- lapply(seq_len(n), function(i)
  matrix(c(.8, -.4, -.7, .5, .2, .9) + .01 * sin(i), 3L, p))
basis <- make_basis(cbind(intercept = 1, income = z), c("a", "b"))
data <- make_data(rep(list(c(0, 1, 0)), n), design, basis,
  metadata = list(orientation = "synthetic y=1 denotes first profile"))
par <- make_par(data, .13, c(.4, -.2, .6, -.25), c(.7, -.35), .4)
# Complete nuisance covariance with deliberately correlated mean/position blocks.
v <- seq(.2, 1.5, length.out = h)
info <- diag(seq(.8, 1.4, length.out = h)) + 3 * tcrossprod(v)
set.seed(2026091511)
u <- qr.Q(qr(cbind(1, matrix(rnorm(n*h), n, h))))[, -1L, drop = FALSE]
score <- sqrt(n) * u %*% chol(info)
fit <- list(data = data, par = par, score = score,
            information = info, regular_candidate = TRUE)
check("fixture score covariance equals full information", cov_n(score), info)
contrast <- c(1, -.4)
sdz <- sqrt(mean((z - mean(z))^2))
ge <- numeric(h); ge[4:5] <- sdz * contrast
e <- sdz * sum(contrast * par[4:5])
ei <- drop(score %*% solve(info, ge))
ese <- sqrt(mean((ei - mean(ei))^2) / n)
critical <- qnorm(.975)
expected_interval <- image_square(e + c(-1, 1) * critical * ese)
out <- project(fit); row <- hz(out)
check("rank-one point uses denominator N", row$estimate, e^2)
check("rank-one factor equals known Q", tcrossprod(out$hz_projection[[1]]$coefficient),
      tcrossprod(ge[2:5]))
check("exact whole-interval square map", c(row$lower, row$upper), expected_interval)
check("signed factor SE uses full covariance", out$hz_projection[[1]]$linear_se, ese)
check("full-information analytic SE", ese, sqrt(drop(crossprod(ge, solve(info, ge))) / n))
check_true("projection method explicitly recorded", row$interval_method == "squared_mean_projection_fixed_reference")
check("fixed reference has no composition term", out$composition, matrix(0, n, nrow(out$table)))
mean_only <- sqrt(drop(crossprod(ge[2:5], solve(info[2:5, 2:5], ge[2:5]))) / n)
check_true("fixture detects mean-only covariance substitution", abs(mean_only - ese) > 1e-4)
no_position <- sqrt(drop(crossprod(ge[-h], solve(info[-h, -h], ge[-h]))) / n)
check_true("fixture detects dropping position nuisance", abs(no_position - ese) > 1e-4)

# Finite differences of the underlying scalar supply an independent width check.
linear_value <- function(theta) sdz * sum(contrast * theta[4:5])
fd <- vapply(seq_len(h), function(j) {
  plus <- minus <- par; plus[j] <- plus[j] + 1e-6; minus[j] <- minus[j] - 1e-6
  (linear_value(plus) - linear_value(minus)) / 2e-6
}, numeric(1))
fd_se <- sqrt(drop(crossprod(fd, solve(info, fd))) / n)
check("scalar derivative finite difference", fd, ge, 2e-10)
check("interval width from FD scalar covariance", row$upper - row$lower,
      diff(image_square(e + c(-1, 1) * critical * fd_se)), 2e-10)
quadratic_fd <- vapply(seq_len(h), function(j) {
  plus <- minus <- fit; plus$par[j] <- plus$par[j] + 1e-6; minus$par[j] <- minus$par[j] - 1e-6
  (hz(project(plus, infer = FALSE))$estimate - hz(project(minus, infer = FALSE))$estimate) / 2e-6
}, numeric(1))
check("HZ gradient finite difference", out$gradient[row$target, ], quadratic_fd, 2e-10)

for (slope in c(-.7, -.01, 0, .01, .7)) {
  changed <- fit; changed$par[4:5] <- c(slope, 0)
  ans <- project(changed); rr <- hz(ans)
  expected <- image_square(sdz * slope + c(-1, 1) * critical * ese)
  check(paste("entire square image at slope", slope), c(rr$lower, rr$upper), expected)
  check_true(paste("projection used without significance switching", slope),
    rr$interval_method == "squared_mean_projection_fixed_reference")
  if (slope == 0) {
    check("zero scalar HZ estimate", rr$estimate, 0)
    check("zero scalar HZ first-order SE", rr$se, 0)
    check_true("zero scalar keeps positive projected upper", rr$lower == 0 && rr$upper > 0)
  }
}

# A general basis transformation retains the parameter, score and information geometry.
k <- matrix(c(1.3, .1, 0, .2, -.2, .9, .1, 0,
              .1, 0, 1.4, -.1, 0, .2, .1, .8), r, r)
transform <- diag(h); transform[2:5, 2:5] <- k
converted <- fit
converted$data$basis <- lapply(basis, function(b) b %*% solve(k))
converted$par <- drop(transform %*% par)
converted$score <- score %*% solve(transform)
converted$information <- t(solve(transform)) %*% info %*% solve(transform)
converted_out <- project(converted)
check("general mean-coordinate invariance of HZ", hz(converted_out)$estimate, row$estimate)
check("general mean-coordinate invariance of interval", unlist(hz(converted_out)[c("lower", "upper")], use.names = FALSE),
      c(row$lower, row$upper))
flip <- diag(h); flip[6:7, 6:7] <- -diag(p)
signed <- fit; signed$par <- drop(flip %*% par); signed$score <- score %*% flip
signed$information <- flip %*% info %*% flip
signed_out <- project(signed)
check("latent sign normalization leaves HZ interval unchanged", unlist(hz(signed_out)[c("lower", "upper")], use.names = FALSE),
      c(row$lower, row$upper))
opposite <- project(fit, -contrast)
check("contrast sign leaves HZ interval unchanged", unlist(hz(opposite)[c("lower", "upper")], use.names = FALSE),
      c(row$lower, row$upper))

# Rank depends on the declared reference design, not estimated mean values.
constant <- fit; constant$data$basis <- rep(list(basis[[1L]]), n)
co <- project(constant)
check("rank-zero reference gives known zero", c(hz(co)$estimate, hz(co)$lower, hz(co)$upper), c(0, 0, 0))
check_true("rank-zero identity marked", hz(co)$interval_method == "fixed_reference_structural_zero" &&
  co$hz_projection[[1L]]$rank == 0L)
rank2 <- fit
rank2$data$basis <- lapply(seq_len(n), function(i) matrix(c(1, 0, z[i], z[i]^2, 0, 1, 0, 0), p, r, byrow = TRUE))
rank2$par[4:5] <- 0
check_error("higher rank refused even at fitted zero HZ", project(rank2, c(1, 0)))
tiny_rank2 <- rank2
tiny_rank2$data$basis <- lapply(seq_len(n), function(i) matrix(c(1, 0, z[i], 1e-8*z[i]^2, 0, 1, 0, 0), p, r, byrow = TRUE))
check_error("small genuine second direction not truncated", project(tiny_rank2, c(1, 0)))
check_error("population reference refused", targets(fit, contrast, reference = "population",
  hz_interval = "fixed_rank_one_projection"))
check_error("default population cannot silently become fixed", targets(fit, contrast,
  hz_interval = "fixed_rank_one_projection"))

# Fixed respondent-dependent contrasts are also allowed if their known Q has rank one.
generic <- fit; generic$data$basis <- rep(list(cbind(diag(p), matrix(0, p, p))), n)
generic$par[2:3] <- c(.43, -.17)
varying <- cbind(1 + .3*z, -.4 + .6*z)
go <- project(generic, varying)
gg <- numeric(h); gg[2:3] <- sdz*c(.3, .6)
eg <- sum(gg * generic$par); seg <- sqrt(drop(crossprod(gg, solve(info, gg))) / n)
check("generic rank-one varying contrast point", hz(go)$estimate, eg^2)
check("generic rank-one varying contrast interval", unlist(hz(go)[c("lower", "upper")], use.names = FALSE),
      image_square(eg + c(-1, 1)*critical*seg))

boundary <- fit; boundary$par[h] <- 0
check_true("full position boundary remains withheld", all(is.na(hz(project(boundary))$lower)))
boundary <- fit; boundary$par[6:7] <- 0
check_true("entire latent-factor boundary remains withheld", all(is.na(hz(project(boundary))$lower)))
singular <- fit; singular$information[h, ] <- singular$information[, h] <- 0
check_true("singular information is not repaired", all(is.na(hz(project(singular))$lower)))
uncertain <- fit; uncertain$regular_candidate <- FALSE
check_true("numerical noncandidate remains withheld", all(is.na(hz(project(uncertain))$lower)))
plain <- targets(fit, list(policy = contrast), reference = "fixed")
check_true("default remains separately declared Wald method", hz(plain)$interval_method == "marginal_regular_wald")
check("projection changes no point estimates", out$table$estimate, plain$table$estimate)
others <- out$table$type != "HZ"
check("projection changes no other target intervals", as.matrix(out$table[others, c("lower", "upper")]),
      as.matrix(plain$table[others, c("lower", "upper")]))
check_true("scope does not claim simultaneous coverage", grepl("not simultaneous", out$interval_scope, fixed = TRUE))

results <- do.call(rbind, checks)
print(results, row.names = FALSE)
stopifnot(all(results$passed))
cat(nrow(results), "HZ projection checks passed.\n")
