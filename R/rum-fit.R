# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/fit.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

.rum_sign_align <- function(data, par, anchor = NULL) {
  j <- 1L + data$r + seq_len(data$p)
  if (is.null(anchor)) anchor <- which.max(abs(par[j]))
  if (par[j[anchor]] < 0) par[j] <- -par[j]
  par
}

.rum_score_distance <- function(score, information, n) {
  # Numerical stationarity measured in standard-error units, conditional on I.
  tryCatch(sqrt(max(0, n * drop(crossprod(score, solve(information, score))))),
           error = function(e) Inf)
}

#' @export
rum_fit <- function(data, starts = NULL, orders = c(9L, 15L, 21L),
                    control = list(), policy_law = "normal", policy_rank = 1L) {
  .rum_check_data(data)
  defaults <- list(maxit = 300L, factr = 1e5, pgtol = 1e-8,
    max_seconds = 120, max_evaluations = 8000L,
    coefficient_bound = 12, loading_bound = 6, position_bound = 16,
    score_tolerance = 2e-5, score_se_tolerance = .01,
    integration_se_tolerance = .03, boundary_tolerance = 1e-8,
    keep_starts = 2L, hessian_step = 1e-4, hessian_curvature_tolerance = 1e-6)
  unknown <- setdiff(names(control), names(defaults))
  if (length(unknown)) stop("Unknown fit control: ", paste(unknown, collapse = ", "))
  control <- utils::modifyList(defaults, control)
  for (nm in names(defaults)) if (!is.numeric(control[[nm]]) || length(control[[nm]]) != 1L ||
      !is.finite(control[[nm]]) || control[[nm]] <= 0)
    stop("Fit controls must be finite positive scalars: ", nm)
  for (nm in c("maxit", "max_evaluations", "keep_starts"))
    if (control[[nm]] != as.integer(control[[nm]])) stop("Integer control required: ", nm)
  if (length(policy_rank) != 1L || is.na(policy_rank) || !policy_rank %in% c(0L, 1L))
    stop("This milestone implements policy rank zero or one.")
  if (!is.numeric(orders) || !length(orders) || any(!is.finite(orders)) ||
      any(orders != as.integer(orders)) || any(orders < 2L | orders > 80L) ||
      is.unsorted(orders, strictly = TRUE))
    stop("orders must be a strictly increasing sequence of quadrature orders from 2 to 80.")
  policy_law <- match.arg(policy_law, c("normal", "three_point"))
  h <- 2L + data$r + data$p
  alpha_j <- 1L + seq_len(data$r)
  loading_j <- 1L + data$r + seq_len(data$p)
  lower <- c(rep(-control$coefficient_bound, 1L + data$r),
             rep(-control$loading_bound, data$p), 0)
  upper <- c(rep(control$coefficient_bound, 1L + data$r),
             rep(control$loading_bound, data$p), control$position_bound)
  names(lower) <- names(upper) <- names(rum_parameters(data))
  started <- proc.time()[[3L]]
  evaluations <- 0L
  budget_hit <- FALSE
  check_budget <- function() {
    if (proc.time()[[3L]] - started > control$max_seconds ||
        evaluations >= control$max_evaluations) {
      budget_hit <<- TRUE
      stop("Declared fitting budget exhausted.")
    }
  }
  evaluate_full <- function(par, quadrature) {
    check_budget()
    evaluations <<- evaluations + 1L
    rum_loglik(data, par, quadrature)
  }
  mean_design <- cbind(1, data$a)
  column_norm <- sqrt(colMeans(mean_design^2))
  if (any(column_norm == 0) || qr(sweep(mean_design, 2L, column_norm, "/"))$rank < 1L + data$r)
    stop("Mean/position-constant design is numerically rank deficient; inspect aliases and basis scaling.")
  glm_start <- suppressWarnings(tryCatch(stats::glm.fit(cbind(1, data$a), data$y,
    family = stats::binomial(), control = stats::glm.control(maxit = 50L))$coefficients,
    error = function(e) rep(0, 1L + data$r)))
  glm_start[!is.finite(glm_start)] <- 0
  glm_start <- pmax(lower[seq_len(1L + data$r)] * .8,
                    pmin(upper[seq_len(1L + data$r)] * .8, glm_start))
  base <- rum_parameters(data, glm_start[1], glm_start[-1],
                         rep(0, data$p), .3)
  if (is.null(starts)) {
    directions <- list(rep(1, data$p), rep(c(1, -1), length.out = data$p),
                       c(1, rep(0, data$p - 1L)))
    starts <- lapply(seq_along(directions), function(k) {
      x <- base
      x[loading_j] <- (.55 + .15 * (k - 1L)) * directions[[k]] /
        sqrt(sum(directions[[k]]^2))
      x[h] <- c(.3, .7, .15)[k]
      x
    })
  } else {
    if (is.numeric(starts) && !is.matrix(starts)) starts <- list(starts)
    if (is.matrix(starts)) starts <- lapply(seq_len(nrow(starts)), function(i) starts[i, ])
    if (!is.list(starts) || !length(starts)) stop("starts must contain parameter vectors.")
    for (par in starts) .rum_check_par(data, par)
  }
  starts <- lapply(starts, function(x) {
    if (any(x < lower | x > upper)) stop("Starting value lies outside declared parameter bounds.")
    names(x) <- names(base)
    if (policy_rank == 0L) x[loading_j] <- 0
    x
  })
  candidates <- list()
  refinements <- list()
  stage_results <- list()
  previous <- NULL
  best <- NULL
  run_candidate <- function(initial, mode, quadrature, stage, label) {
    fixed <- if (mode == "position_only") loading_j else
      if (mode == "homogeneous") c(loading_j, h) else
      if (mode == "zero_position") h else integer()
    if (policy_rank == 0L) fixed <- union(fixed, loading_j)
    initial[fixed] <- 0
    free <- setdiff(seq_len(h), fixed)
    cache <- new.env(parent = emptyenv())
    cache$par <- NULL
    cache$best <- NULL
    initial_objective <- NA_real_
    evaluate <- function(x) {
      full <- initial; full[free] <- x
      if (identical(full, cache$par)) return(cache$value)
      val <- evaluate_full(full, quadrature)
      cache$par <- full; cache$value <- val
      if (is.null(cache$best) || val$objective > cache$best$value$objective)
        cache$best <- list(par = full, value = val)
      val
    }
    t0 <- proc.time()[[3L]]
    result <- tryCatch({
      # Preserve the likelihood already attainable at every declared/warm start.
      initial_objective <- evaluate(initial[free])$objective
      opt <- stats::optim(initial[free],
        fn = function(x) -evaluate(x)$objective,
        gr = function(x) -evaluate(x)$gradient[free],
        method = "L-BFGS-B", lower = lower[free], upper = upper[free],
        control = list(maxit = as.integer(control$maxit), factr = control$factr,
                       pgtol = control$pgtol))
      par <- initial; par[free] <- opt$par
      ev <- evaluate(opt$par)
      if (cache$best$value$objective > ev$objective + 1e-12) {
        par <- cache$best$par; ev <- cache$best$value
        opt$convergence <- 995L
        opt$message <- "An evaluated point has higher likelihood than the optimizer return."
      }
      list(par = par, objective = ev$objective, convergence = opt$convergence,
        message = if (is.null(opt$message)) "" else opt$message,
        loglik = ev$loglik, score = ev$score, gradient = ev$gradient,
        error = NULL)
    }, error = function(e) {
      if (is.null(cache$best)) return(list(par = initial, objective = NA_real_,
        convergence = 999L, message = conditionMessage(e), error = conditionMessage(e)))
      z <- cache$best$value
      list(par = cache$best$par, objective = z$objective, convergence = 999L,
        message = conditionMessage(e), error = conditionMessage(e),
        loglik = z$loglik, score = z$score, gradient = z$gradient)
    })
    # Symmetric latent laws permit exact chart transport without another integral.
    aligned <- .rum_sign_align(data, result$par)
    if (!identical(aligned, result$par) && is.finite(result$objective)) {
      result$score[, loading_j] <- -result$score[, loading_j, drop = FALSE]
      result$gradient[loading_j] <- -result$gradient[loading_j]
    }
    result$par <- aligned
    result$stage <- stage; result$order <- quadrature$order
    result$mode <- mode; result$start <- label
    result$initial_par <- initial
    result$initial_objective <- initial_objective
    result$elapsed_seconds <- proc.time()[[3L]] - t0
    result
  }
  for (stage in seq_along(orders)) {
    if (budget_hit) break
    quadrature <- rum_quadrature(rep(orders[stage], 2L), policy_law)
    jobs <- list()
    for (j in seq_along(starts))
      jobs[[length(jobs) + 1L]] <- list(par = starts[[j]],
        mode = if (policy_rank == 0L) "position_only" else "full", label = paste0("declared", j))
    # Boundaries are actual optimizations, not inaccessible log-variance limits.
    jobs[[length(jobs) + 1L]] <- list(par = base, mode = "position_only", label = "zero_policy")
    jobs[[length(jobs) + 1L]] <- list(par = base, mode = "homogeneous", label = "zero_both")
    if (policy_rank == 1L) for (j in seq_len(min(2L, length(starts))))
      jobs[[length(jobs) + 1L]] <- list(par = starts[[j]], mode = "zero_position", label = paste0("zero_position", j))
    if (!is.null(previous)) {
      eligible <- which(vapply(previous, function(z) is.finite(z$objective), logical(1)))
      if (length(eligible)) {
        eligible <- eligible[order(vapply(previous[eligible], `[[`, numeric(1), "objective"), decreasing = TRUE)]
        for (j in head(eligible, as.integer(control$keep_starts)))
          jobs <- c(list(list(par = previous[[j]]$par,
            mode = if (policy_rank == 0L) "position_only" else "full", label = paste0("warm", j))), jobs)
      }
    }
    this <- list()
    for (job in jobs) {
      if (budget_hit) break
      result <- run_candidate(job$par, job$mode, quadrature, stage, job$label)
      this[[length(this) + 1L]] <- result
      candidates[[length(candidates) + 1L]] <- result
    }
    eligible <- which(vapply(this, function(z) is.finite(z$objective), logical(1)))
    if (!length(eligible)) break
    # Keep the highest found value even if its numerical status is unresolved.
    # Discarding it in favor of a worse converged fit would hide a real problem.
    best_now <- this[[eligible[which.max(vapply(this[eligible], `[[`, numeric(1), "objective"))]]]
    if (!is.null(best)) {
      old_rule <- rum_quadrature(rep(orders[stage - 1L], 2L), policy_law)
      coarse <- tryCatch(evaluate_full(best_now$par, old_rule),
        error = function(e) list(objective = NA_real_, gradient = rep(NA_real_, h)))
      fine <- best_now
      opg <- crossprod(fine$score) / data$n
      anchor <- which.max(abs(best_now$par[loading_j]))
      oldpar <- .rum_sign_align(data, best$par, anchor)
      refinements[[length(refinements) + 1L]] <- list(
        from_order = orders[stage - 1L], to_order = orders[stage],
        objective_difference_at_fine_fit = fine$objective - coarse$objective,
        score_difference = fine$gradient - coarse$gradient,
        score_difference_se_units = .rum_score_distance(fine$gradient - coarse$gradient, opg, data$n),
        parameter_change = best_now$par - oldpar,
        policy_covariance_change = outer(best_now$par[loading_j], best_now$par[loading_j]) -
          outer(oldpar[loading_j], oldpar[loading_j]))
    }
    best <- best_now
    stage_results[[length(stage_results) + 1L]] <- best
    previous <- this
  }
  if (is.null(best)) stop(structure(list(
    message = "No finite fit was returned; inspect starts, data, and declared computation budget.",
    call = match.call(), candidates = candidates, evaluations = evaluations,
    elapsed_seconds = proc.time()[[3L]] - started),
    class = c("rum_fit_error", "error", "condition")))
  quadrature <- rum_quadrature(best$order, policy_law)
  par <- best$par
  information <- crossprod(best$score)/data$n
  gradient <- colMeans(best$score)
  free <- if (policy_rank == 0L) setdiff(seq_len(h), loading_j) else seq_len(h)
  projected <- gradient
  at_lower <- par <= lower + control$boundary_tolerance
  at_upper <- par >= upper - control$boundary_tolerance
  projected[at_lower] <- pmax(gradient[at_lower], 0)
  projected[at_upper] <- pmin(gradient[at_upper], 0)
  kkt <- max(abs(projected[free]))
  positive_information <- !inherits(try(chol(information), silent = TRUE), "try-error")
  stationary_se <- if (positive_information) .rum_score_distance(gradient, information, data$n) else Inf
  zero_policy <- sqrt(sum(par[loading_j]^2)) <= control$boundary_tolerance
  zero_position <- par[h] <= control$boundary_tolerance
  artificial_bound <- any(at_upper) || any(at_lower[-h])
  full_schedule <- length(stage_results) == length(orders) && !budget_hit
  refinement_ok <- length(refinements) > 0L && is.finite(tail(refinements, 1L)[[1L]]$score_difference_se_units) &&
    tail(refinements, 1L)[[1L]]$score_difference_se_units <= control$integration_se_tolerance
  issues <- character()
  if (budget_hit || !full_schedule) issues <- c(issues, "incomplete_computation_schedule")
  if (best$convergence != 0L) issues <- c(issues, "optimizer_not_converged")
  if (kkt > control$score_tolerance) issues <- c(issues, "projected_score_not_small")
  if (artificial_bound) issues <- c(issues, "artificial_parameter_bound")
  if (!positive_information) issues <- c(issues, "information_not_numerically_positive_definite")
  if (!refinement_ok) issues <- c(issues, "integration_refinement_unresolved")
  if (!zero_policy && !zero_position && stationary_se > control$score_se_tolerance)
    issues <- c(issues, "score_error_large_relative_to_sampling_scale")
  if (zero_policy) issues <- c(issues, "zero_policy_factor_chart")
  if (zero_position) issues <- c(issues, "zero_position_variance_boundary")
  # Hessian is a diagnostic, not substituted for a missing correct-model argument.
  info_detail <- tryCatch(.rum_information(data, par, quadrature, control$hessian_step,
      evaluate_full, best),
                         error = function(e) list(hessian = matrix(NA_real_, h, h), error = conditionMessage(e)))
  if (any(!is.finite(info_detail$hessian))) issues <- c(issues, "hessian_computation_failed")
  if (all(is.finite(info_detail$hessian)) && !zero_policy && !zero_position &&
      min(info_detail$hessian_eigenvalues) < -control$hessian_curvature_tolerance *
        max(1, max(abs(info_detail$hessian_eigenvalues))))
    issues <- c(issues, "interior_curvature_inconsistent_with_local_maximum")
  if (budget_hit) {
    full_schedule <- FALSE
    issues <- c(issues, "incomplete_computation_schedule")
  }
  regular_candidate <- !length(issues) && policy_rank == 1L
  out <- list(data = data, par = par, quadrature = quadrature,
    loglik = best$loglik, score = best$score, information = information,
    hessian_information = info_detail$hessian, information_diagnostics = info_detail,
    candidates = candidates, refinement = refinements, stage_results = stage_results,
    regular_candidate = regular_candidate, numerical_issues = unique(issues),
    status = list(code = if (regular_candidate) "regular_interior_candidate" else "qualified_fit",
      complete = full_schedule, optimizer_convergence = best$convergence,
      projected_score_max = kkt, score_error_se_units = stationary_se,
      boundary_policy = zero_policy, boundary_position = zero_position,
      artificial_bound = artificial_bound, integration_refined = refinement_ok,
      regularity_is_assumed_not_verified = TRUE),
    control = control, bounds = list(lower = lower, upper = upper), policy_rank = policy_rank,
    evaluations = evaluations, elapsed_seconds = proc.time()[[3L]] - started,
    call = match.call(), interpretation = paste(
      "Numerical candidacy is not proof of a global optimum, identification, true interiority,",
      "correct specification, or interval coverage. Inference is conditional on those assumptions."))
  class(out) <- "rum_fit"
  out
}

#' @export
print.rum_fit <- function(x, ...) {
  cat("One-policy-factor RUM fit:", x$data$n, "respondents; mean log likelihood",
      format(mean(x$loglik), digits = 6), "\n")
  cat("Status:", x$status$code, "| quadrature:", paste(x$quadrature$order, collapse = " x "), "\n")
  if (length(x$numerical_issues)) cat("Qualifications:", paste(x$numerical_issues, collapse = "; "), "\n")
  cat("Runtime:", round(x$elapsed_seconds, 2), "seconds in one process.\n")
  cat(x$interpretation, "\n")
  invisible(x)
}
