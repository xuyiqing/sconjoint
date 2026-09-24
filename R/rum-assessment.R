# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/assessment.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

.rum_bank_design <- function(data) list(ids = data$ids, offsets = data$offsets,
                                       d = data$d, a = data$a, basis = data$basis)

.rum_bank_validate <- function(data, bank) {
  if (!inherits(bank, "rum_moment_bank") || !identical(bank$design, .rum_bank_design(data)))
    stop("bank must match the respondent IDs, complete design and mean basis")
}

.rum_bank_observed <- function(data, bank) {
  ans <- matrix(0, data$n, bank$J, dimnames = list(data$ids, bank$names))
  pair_at <- count_at <- 0L
  for (i in seq_len(data$n)) {
    ti <- data$T[i]; ix <- data$offsets[i] + seq_len(ti); yi <- data$y[ix]
    ans[i, ] <- as.numeric(crossprod(yi, bank$marginal_weights[ix, , drop = FALSE]))
    if (ti > 1L) {
      pairs <- numeric(ti * (ti - 1L) / 2L); j <- 0L
      for (t in seq_len(ti - 1L)) for (s in (t + 1L):ti) {
        j <- j + 1L; pairs[j] <- yi[t] * yi[s]
      }
      px <- pair_at + seq_along(pairs)
      ans[i, ] <- ans[i, ] + as.numeric(crossprod(pairs, bank$pair_weights[px, , drop = FALSE]))
      pair_at <- pair_at + length(pairs)
    }
    ans[i, ] <- ans[i, ] + bank$count_weights[count_at + sum(yi) + 1L, ]
    count_at <- count_at + ti + 1L
  }
  ans
}

# Store a fixed bank of bounded respondent moments
#
# Weights must be specified from design/covariates without outcome selection.
# Pair rows use t first, then s > t within each respondent. Count rows use
# k=0,...,T. Stored design bounds describe these records, not unknown future
# designs, and do not supply a superpopulation Hoeffding bound.
#' @export
rum_moment_bank <- function(data, marginal_weights = NULL, pair_weights = NULL,
                            count_weights = NULL, names = NULL) {
  .rum_check_data(data)
  weights <- list(marginal_weights, pair_weights, count_weights)
  supplied <- which(!vapply(weights, is.null, logical(1)))
  if (!length(supplied)) stop("at least one weight matrix is required")
  dims <- vapply(weights[supplied], function(x) {
    if (!is.matrix(x) || !is.numeric(x)) stop("weights must be numeric matrices")
    ncol(x)
  }, integer(1))
  if (length(unique(dims)) != 1L || dims[1L] < 1L) stop("weight matrices must share a positive column count")
  J <- dims[1L]
  if (is.null(names)) names <- paste0("moment", seq_len(J))
  if (length(names) != J || anyNA(names) || any(!nzchar(names)) || anyDuplicated(names))
    stop("moment names must be unique and nonempty")
  sizes <- c(length(data$y), sum(data$T * (data$T - 1L) / 2L), sum(data$T + 1L))
  for (k in seq_len(3L)) {
    if (is.null(weights[[k]])) weights[[k]] <- matrix(0, sizes[k], J)
    if (nrow(weights[[k]]) != sizes[k] || any(!is.finite(weights[[k]])))
      stop("weight rows must match packed tasks, pairs and counts and be finite")
    storage.mode(weights[[k]]) <- "double"
    colnames(weights[[k]]) <- names
  }
  lower <- upper <- matrix(0, data$n, J, dimnames = list(data$ids, names))
  pa <- ca <- 0L
  for (i in seq_len(data$n)) {
    ti <- data$T[i]; ix <- data$offsets[i] + seq_len(ti)
    np <- ti * (ti - 1L) / 2L
    linear <- rbind(weights[[1L]][ix, , drop = FALSE],
                    weights[[2L]][pa + seq_len(np), , drop = FALSE])
    cw <- weights[[3L]][ca + seq_len(ti + 1L), , drop = FALSE]
    lower[i, ] <- colSums(pmin(linear, 0)) + apply(cw, 2L, min)
    upper[i, ] <- colSums(pmax(linear, 0)) + apply(cw, 2L, max)
    pa <- pa + np; ca <- ca + ti + 1L
  }
  bank <- structure(list(marginal_weights = weights[[1L]], pair_weights = weights[[2L]],
                         count_weights = weights[[3L]], names = names, J = J,
                         design = .rum_bank_design(data), design_lower = lower,
                         design_upper = upper,
                         structural_identity = colSums(lower != upper) == 0L,
                         scope = "Fixed supplied weights; no check of outcome-independent selection or bounds outside this design bank"),
                    class = "rum_moment_bank")
  bank$observed <- .rum_bank_observed(data, bank)
  bank
}

.rum_moment_components <- function(data, par, bank, quadrature) {
  .rum_check_data(data); .rum_check_par(data, par)
  .rum_bank_validate(data, bank)
  z <- rum_moments_cpp(data$d, data$a, data$offsets, as.numeric(par),
                       quadrature$u, quadrature$v, quadrature$weight,
                       bank$marginal_weights, bank$pair_weights, bank$count_weights)
  h <- 2L + data$r + data$p
  if (!identical(dim(z$expected), c(data$n, bank$J)) ||
      !identical(dim(z$derivative), c(bank$J, h)) ||
      any(!is.finite(z$expected)) || any(!is.finite(z$derivative)))
    stop("native moment outputs have invalid dimensions or nonfinite values")
  colnames(z$expected) <- bank$names
  dimnames(z$derivative) <- list(bank$names, names(par))
  z$observed <- .rum_bank_observed(data, bank)
  z$residual <- z$observed - z$expected
  z
}

.rum_check_inference_options <- function(alpha, nsim, seed) {
  if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly between zero and one")
  if (length(nsim) != 1L || !is.numeric(nsim) || !is.finite(nsim) || nsim < 1 || nsim != as.integer(nsim))
    stop("nsim must be a positive integer")
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) || seed != as.integer(seed))
    stop("seed must be one integer")
}

.rum_gaussian_moment_test <- function(residual, influence, excluded, alpha, nsim, seed) {
  n <- nrow(residual); nm <- colnames(residual); J <- ncol(residual)
  covariance <- .rum_cov_rows(influence)
  se <- sqrt(diag(covariance) / n)
  valid <- !excluded & is.finite(se) & se > 0
  invalid <- !excluded & !valid
  tab <- data.frame(moment = nm, residual = colMeans(residual), se = se,
                    statistic = NA_real_, lower = NA_real_, upper = NA_real_,
                    status = "degenerate_coordinate_withheld", stringsAsFactors = FALSE)
  tab$status[excluded] <- "structural_identity_not_tested"
  result <- list(table = tab, covariance = covariance, critical_value = NA_real_,
                 p_value = NA_real_, subset_p_value = NA_real_, statistic = NA_real_,
                 tested_coordinates = nm[valid], withheld_coordinates = nm[invalid],
                 status = "no_nondegenerate_test_coordinates", nsim = nsim, seed = seed)
  if (!any(valid)) return(result)
  centered <- sweep(influence[, valid, drop = FALSE], 2L,
                    colMeans(influence[, valid, drop = FALSE]), "-")
  scale <- sqrt(diag(covariance)[valid])
  maxima <- .rum_with_seed(seed, {
    ans <- numeric(nsim)
    for (start in seq.int(1L, nsim, by = 100L)) {
      ix <- start:min(start + 99L, nsim)
      g <- matrix(stats::rnorm(n * length(ix)), n, length(ix))
      draw <- sweep(crossprod(g, centered) / sqrt(n), 2L, scale, "/")
      ans[ix] <- apply(abs(draw), 1L, max)
    }
    ans
  })
  crit <- as.numeric(stats::quantile(maxima, 1 - alpha, type = 1L, names = FALSE))
  stat <- result$table$residual[valid] / se[valid]
  obs <- max(abs(stat))
  result$table$statistic[valid] <- stat
  result$table$lower[valid] <- result$table$residual[valid] - crit * se[valid]
  result$table$upper[valid] <- result$table$residual[valid] + crit * se[valid]
  result$table$status[valid] <- "gaussian_calibration_conditional_on_positive_population_variance"
  result$critical_value <- crit; result$statistic <- obs
  result$subset_p_value <- (1 + sum(maxima >= obs)) / (nsim + 1)
  result$status <- if (any(invalid)) "incomplete_family_degenerate_coordinates" else "conditional_gaussian_approximation"
  if (!any(invalid)) result$p_value <- result$subset_p_value
  result
}

# Conditional respondent moment assessment
#
# Known-parameter mode uses raw residual covariance C. Same-fit mode uses
# r-A I^{-1}S and requires an interior numerical candidate from the same data.
# Gaussian p-values and bands are model-null approximations, not robust
# discrepancy intervals, exact Monte Carlo tests or coverage certificates.
#' @export
rum_assess <- function(data, par = NULL, bank, quadrature = NULL, estimated = FALSE,
                       fit = NULL, alpha = .05, nsim = 1999L, seed = 1L) {
  .rum_check_inference_options(alpha, nsim, seed)
  if (estimated) {
    if (is.null(fit)) stop("estimated=TRUE requires a same-sample fit")
    if (!identical(.rum_bank_design(data), .rum_bank_design(fit$data)) || !identical(data$y, fit$data$y))
      stop("same-fit assessment requires exactly the fitted respondents and outcomes")
    if (is.null(par)) par <- fit$par
    if (is.null(quadrature)) quadrature <- fit$quadrature
    if (!identical(as.numeric(par), as.numeric(fit$par)) ||
        !identical(quadrature$u, fit$quadrature$u) ||
        !identical(quadrature$v, fit$quadrature$v) ||
        !identical(quadrature$weight, fit$quadrature$weight))
      stop("same-fit assessment must use the fitted parameters and quadrature")
  }
  if (is.null(par) || is.null(quadrature)) stop("par and quadrature are required")
  z <- .rum_moment_components(data, par, bank, quadrature)
  context <- if (estimated) .rum_inference_context(fit) else list(ok = TRUE, reason = "known_parameter")
  influence <- z$residual
  identity <- NULL
  if (estimated && context$ok) {
    influence <- z$residual - fit$score %*% context$inverse %*% t(z$derivative)
    identity <- .rum_cov_rows(z$residual) - z$derivative %*% context$inverse %*% t(z$derivative)
  }
  if (context$ok) {
    test <- .rum_gaussian_moment_test(z$residual, influence, bank$structural_identity,
                                      alpha, nsim, seed)
  } else {
    influence <- NULL
    test <- list(table = data.frame(moment = bank$names, residual = colMeans(z$residual),
                                    se = NA_real_, statistic = NA_real_, lower = NA_real_,
                                    upper = NA_real_, status = context$reason),
                 covariance = NULL, p_value = NA_real_, critical_value = NA_real_,
                 status = context$reason, tested_coordinates = character(),
                 withheld_coordinates = bank$names, nsim = nsim, seed = seed)
  }
  structure(c(list(mode = if (estimated) "same_fit_composite_null" else "known_parameter_null",
                   expected = z$expected, observed = z$observed, residual = z$residual,
                   derivative = z$derivative, influence = influence,
                   raw_residual_covariance = .rum_cov_rows(z$residual),
                   covariance_identity_plugin = identity,
                   identity_scope = "C-A I^-1 A' plug-in diagnostic; finite-sample empirical cross-moments need not equal model A",
                   alpha = alpha, inference_context = context,
                   interval_scope = "Null-calibrated compatibility bands, not misspecification-robust discrepancy confidence intervals",
                   assumptions = "Fixed bank and correct conditional RUM; same-fit mode additionally requires regular full-model MLE, empirical L2 influence consistency and negligible numerical error"), test),
            class = "rum_assessment")
}

# Assess moments of a predictor on an independent holdout
#
# Independence of training and iid test sampling are user-maintained
# conditions, not established from these arguments. Hoeffding bounds require
# a declared J-by-2 residual range valid beyond the observed sample.
#' @export
rum_holdout_assess <- function(data, par, bank, quadrature, alpha = .05,
                               nsim = 1999L, seed = 1L,
                               method = c("gaussian", "hoeffding"),
                               tolerances = NULL, residual_bounds = NULL) {
  method <- match.arg(method)
  .rum_check_inference_options(alpha, nsim, seed)
  z <- .rum_moment_components(data, par, bank, quadrature)
  if (method == "gaussian") {
    out <- .rum_gaussian_moment_test(z$residual, z$residual, rep(FALSE, bank$J), alpha, nsim, seed)
  } else {
    bd <- as.matrix(residual_bounds)
    if (!is.numeric(bd) || !identical(dim(bd), c(bank$J, 2L)) || any(!is.finite(bd)) || any(bd[, 1L] > bd[, 2L]))
      stop("Hoeffding requires declared finite J-by-2 residual_bounds")
    if (any(sweep(z$residual, 2L, bd[, 1L], "-") < 0) ||
        any(sweep(z$residual, 2L, bd[, 2L], "-") > 0)) stop("observed residuals violate declared bounds")
    delta <- colMeans(z$residual)
    width <- (bd[, 2L] - bd[, 1L]) * sqrt(log(2 * bank$J / alpha) / (2 * data$n))
    out <- list(table = data.frame(moment = bank$names, residual = delta,
                                   se = NA_real_, statistic = NA_real_, lower = delta - width,
                                   upper = delta + width, status = "conditional_hoeffding_bound"),
                 p_value = NA_real_, covariance = .rum_cov_rows(z$residual),
                 status = "conditional_bounded_holdout_intervals", residual_bounds = bd)
  }
  if (!is.null(tolerances)) {
    if (!is.numeric(tolerances) || length(tolerances) != bank$J || any(!is.finite(tolerances)) || any(tolerances < 0))
      stop("tolerances must be J prespecified nonnegative numbers")
    out$table$tolerance <- tolerances
    ok <- is.finite(out$table$lower) & is.finite(out$table$upper)
    out$table$within_tolerance <- NA
    out$table$within_tolerance[ok] <- out$table$lower[ok] >= -tolerances[ok] & out$table$upper[ok] <= tolerances[ok]
    out$all_within_tolerance <- if (all(ok)) all(out$table$within_tolerance) else NA
  }
  structure(c(list(mode = "independent_fixed_predictor", method = method, expected = z$expected,
                   observed = z$observed, residual = z$residual, alpha = alpha,
                   scope = "Conditional on independent training: predictor moment discrepancy, not composite-model truth or latent-target bias",
                   assumptions = "Independent training; iid test respondents; fixed moment bank; accurate predictor evaluation; declared population residual bounds for Hoeffding"), out),
            class = "rum_holdout_assessment")
}

# Paired respondent log-score comparison on an independent test sample
#' @export
rum_score_compare <- function(data, par_a, par_b, quadrature, alpha = .05,
                              quadrature_b = quadrature) {
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) stop("invalid alpha")
  a <- rum_loglik(data, par_a, quadrature)$loglik
  b <- rum_loglik(data, par_b, quadrature_b)$loglik
  delta <- a - b; estimate <- mean(delta)
  se <- if (length(delta) > 1L) sqrt(stats::var(delta) / length(delta)) else NA_real_
  regular <- is.finite(se) && se > 0
  list(estimate = estimate, se = se,
       lower = if (regular) estimate - stats::qnorm(1 - alpha / 2) * se else NA_real_,
       upper = if (regular) estimate + stats::qnorm(1 - alpha / 2) * se else NA_real_,
       differences = delta, log_score_a = a, log_score_b = b,
       status = if (regular) "conditional_holdout_clt" else "degenerate_score_difference_withheld",
       scope = "Paired full-respondent log score A minus B, conditional on independent training; no per-task normalization",
       assumptions = "Independent fixed predictors and iid heldout respondents with finite nonzero difference variance; not valid iid errors for overlapping-training cross-validation")
}
