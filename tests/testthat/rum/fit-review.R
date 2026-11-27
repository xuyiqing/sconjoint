# Ported from conjointRUM 0.1.3 tests/fit-review.R (Avidit Acharya). Changes: the package name
# (conjointRUM -> sconjoint) and the R file names (R/<name>.R -> R/rum-<name>.R).
# Run by tests/testthat/test-rum-conjointrum-scripts.R.

# Bounded instrumentation of the fitting wrapper; no coverage or application run.
argv <- commandArgs(FALSE)
script_arg <- grep("^--file=", argv, value = TRUE)
script <- if (length(script_arg)) sub("^--file=", "", script_arg[1L]) else ""
roots <- c(if (nzchar(script)) dirname(dirname(normalizePath(script))), getwd(), dirname(getwd()))
have_source <- vapply(roots, function(x) file.exists(file.path(x, "R", "rum-fit.R")), logical(1))
source_root <- if (any(have_source)) roots[which(have_source)[1L]] else NA_character_
if (!is.na(source_root) && dir.exists(file.path(source_root, ".local-library")))
  .libPaths(c(file.path(source_root, ".local-library"), .libPaths()))
if (!requireNamespace("sconjoint", quietly = TRUE)) stop("Install sconjoint before this test")

review_checks <- list()
review_check <- function(name, ok, detail = "") {
  review_checks[[length(review_checks) + 1L]] <<- data.frame(check = name,
    passed = isTRUE(ok), detail = as.character(detail), stringsAsFactors = FALSE)
}
fresh_environment <- function() {
  e <- new.env(parent = asNamespace("sconjoint"))
  if (!is.na(source_root)) {
    for (file in c("rum-data.R", "rum-quadrature.R", "rum-fit.R"))
      sys.source(file.path(source_root, "R", file), envir = e)
  } else {
    # Installed-package checks still get private closures so instrumentation
    # cannot mutate the package namespace or other tests.
    for (name in c("rum_fit", "rum_loglik", "rum_information", ".rum_information",
                   "rum_data", "rum_parameters", "rum_simulate",
                   "rum_scale_features", "rum_scale_parameters", "rum_quadrature")) {
      f <- getFromNamespace(name, "sconjoint")
      environment(f) <- e
      assign(name, f, envir = e)
    }
  }
  raw <- e$rum_loglik
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  e$rum_loglik <- function(...) {
    calls$n <- calls$n + 1L
    raw(...)
  }
  list(env = e, calls = calls, raw = raw)
}

setup <- fresh_environment(); api <- setup$env
n <- 24L
designs <- lapply(seq_len(n), function(i)
  matrix(c(-1.1, .35, .95, -.55) + .04 * sin(i + seq_len(4L)), ncol = 1L))
blank <- api$rum_data(rep(list(rep(0, 4L)), n), designs,
  metadata = list(orientation = "y=1 is physical left"))
truth <- api$rum_parameters(blank, .1, .3, .65, .35)
data <- api$rum_simulate(blank, truth, seed = 314159L)
initial <- api$rum_parameters(data, .15, .2, -.7, .3)
controls <- list(maxit = 15L, max_evaluations = 3000L, max_seconds = 20)
run_fit <- function(cap) {
  item <- fresh_environment()
  con <- controls; con$max_evaluations <- as.integer(cap)
  fit <- item$env$rum_fit(data, starts = list(initial), orders = c(3L, 5L), control = con)
  list(fit = fit, calls = item$calls$n, env = item$env, raw = item$raw)
}

for (cap in c(1L, 2L, 9L)) {
  out <- run_fit(cap); fit <- out$fit
  review_check(paste("actual likelihood calls equal recorded at cap", cap),
    out$calls == fit$evaluations, paste(out$calls, fit$evaluations))
  review_check(paste("no likelihood call exceeds evaluation cap", cap),
    out$calls <= cap, out$calls)
  review_check(paste("finite best retained when optimizer budget stops", cap),
    all(is.finite(fit$par)) && all(is.finite(fit$loglik)))
  review_check(paste("budget stop cannot yield regular candidacy", cap),
    !fit$regular_candidate && !fit$status$complete &&
      "incomplete_computation_schedule" %in% fit$numerical_issues)
  review_check(paste("budget stop retains candidate errors", cap),
    any(vapply(fit$candidates, function(x) !is.null(x$error), logical(1))))
  verify <- out$raw(data, fit$par, fit$quadrature)
  review_check(paste("sign-transported scores equal direct evaluation", cap),
    max(abs(verify$score - fit$score)) < 2e-11)
  review_check(paste("sign-transported likelihood equals direct evaluation", cap),
    max(abs(verify$loglik - fit$loglik)) < 2e-11)
}

baseline <- run_fit(3000L); fit <- baseline$fit
review_check("completed-wrapper call accounting includes refinement and Hessian",
  baseline$calls == fit$evaluations, paste(baseline$calls, fit$evaluations))
review_check("bounded fixture completes its numerical schedule", fit$status$complete,
  paste(fit$numerical_issues, collapse = ";"))
review_check("final Hessian is available before targeted budget stop",
  all(is.finite(fit$hessian_information)))
review_check("candidate initial values and objectives retained",
  all(vapply(fit$candidates, function(x) length(x$initial_par) == length(initial) &&
    is.finite(x$initial_objective), logical(1))))
review_check("candidate retained value never below its evaluated start",
  all(vapply(fit$candidates, function(x)
    is.finite(x$objective) && x$objective >= x$initial_objective - 2e-11, logical(1))))

if (isTRUE(fit$status$complete) && all(is.finite(fit$hessian_information))) {
  # The deterministic optimization path is identical until the final Hessian
  # call; reducing the cap by one must withhold that incomplete diagnostic.
  stopped <- run_fit(fit$evaluations - 1L)
  review_check("Hessian exhaustion obeys and reports the exact cap",
    stopped$calls == stopped$fit$evaluations && stopped$calls == fit$evaluations - 1L)
  review_check("Hessian exhaustion is explicitly qualified",
    !stopped$fit$regular_candidate && !stopped$fit$status$complete &&
      "hessian_computation_failed" %in% stopped$fit$numerical_issues &&
      "incomplete_computation_schedule" %in% stopped$fit$numerical_issues)
  review_check("Hessian exhaustion preserves the same fitted point",
    max(abs(stopped$fit$par - fit$par)) < 1e-12 &&
      max(abs(stopped$fit$loglik - fit$loglik)) < 1e-12)
} else {
  review_check("Hessian exhaustion regression could be exercised", FALSE)
}

# Check coordinate invariance through the actual data wrappers, not a manually
# repacked native design. Alpha's task design must remain unchanged.
scales <- 2.3
scaled_data <- api$rum_scale_features(data, scales)
scaled_par <- api$rum_scale_parameters(data, initial, scales)
base_eval <- setup$raw(data, initial, api$rum_quadrature(7L))
scaled_eval <- setup$raw(scaled_data, scaled_par, api$rum_quadrature(7L))
review_check("feature scaling preserves the finite mean task design",
  identical(data$a, scaled_data$a))
review_check("feature scaling preserves likelihood",
  max(abs(base_eval$loglik - scaled_eval$loglik)) < 2e-12)
loading_j <- 1L + data$r + seq_len(data$p)
review_check("feature scaling transports loading score",
  max(abs(base_eval$score[, loading_j] -
    scales * scaled_eval$score[, loading_j])) < 2e-12)
review_check("feature parameter scaling is invertible",
  max(abs(api$rum_scale_parameters(data, scaled_par, scales, inverse = TRUE) - initial)) < 1e-14)

for (order in c(2L, 15L, 80L)) {
  q <- api$rum_quadrature(order)
  moment_error <- max(abs(c(sum(q$weight), sum(q$weight * q$u), sum(q$weight * q$v),
    sum(q$weight * q$u^2), sum(q$weight * q$v^2)) - c(1, 0, 0, 1, 1)))
  review_check(paste("normal tensor normalization at order", order),
    all(q$weight > 0) && moment_error < 1e-10, format(moment_error))
}

# Directly verify the near-boundary Hessian path calls only valid variances.
boundary <- initial; boundary[length(boundary)] <- 0
info <- api$rum_information(data, boundary, api$rum_quadrature(7L), step = 1e-4)
review_check("omega-zero Hessian uses valid right-side evaluations",
  all(is.finite(info$hessian)) && max(abs(info$hessian - t(info$hessian))) < 1e-14)

review_results <- do.call(rbind, review_checks)
print(review_results, row.names = FALSE)
if (!all(review_results$passed)) stop("Fit-wrapper review checks failed")
cat(sprintf("Fit-wrapper review: %d/%d checks passed; baseline %d likelihood calls.\n",
  nrow(review_results), nrow(review_results), fit$evaluations))
