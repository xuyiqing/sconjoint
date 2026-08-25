## Quantities of interest for the model in paperps.tex.
##
## These routines deliberately operate on respondent-level fitted conditional
## means and a population covariance.  They never aggregate respondent
## posterior means or modes.  They prefer an explicitly supplied or stored
## full-sample fit. Cross-fitted fold averages are not silently substituted for
## the paper's structural plug-in estimator.

.pps_first <- function(x, paths) {
  for (path in paths) {
    value <- x
    ok <- TRUE
    for (nm in strsplit(path, "\\$", fixed = FALSE)[[1L]]) {
      if (!is.list(value) || is.null(value[[nm]])) {
        ok <- FALSE
        break
      }
      value <- value[[nm]]
    }
    if (ok) return(list(value = value, source = path))
  }
  NULL
}

.pps_respondent_index <- function(fit) {
  rid <- fit$respondent_id
  if (is.null(rid)) {
    stop("The fit has no `respondent_id`; supply a fit with respondent-level ",
         "identifiers.", call. = FALSE)
  }
  key <- as.character(rid)
  ids <- unique(key)
  list(task_id = key, ids = ids, index = match(key, ids),
       first = match(ids, key), N = length(ids))
}

.pps_to_respondent <- function(value, fit, what) {
  idx <- .pps_respondent_index(fit)
  value <- as.matrix(value)
  if (!is.numeric(value) || any(!is.finite(value))) {
    stop("`", what, "` must be a finite numeric matrix.", call. = FALSE)
  }
  if (nrow(value) == length(idx$task_id)) {
    first_value <- value[idx$first[idx$index], , drop = FALSE]
    if (any(abs(value - first_value) > 1e-10)) {
      stop("`", what, "` must be constant within respondent when supplied ",
           "on task rows.", call. = FALSE)
    }
    value <- value[idx$first, , drop = FALSE]
  } else if (nrow(value) != idx$N) {
    stop("`", what, "` must have one row per respondent or one row per task.",
         call. = FALSE)
  }
  rownames(value) <- idx$ids
  value
}

.pps_extract_mu <- function(fit, mu = NULL) {
  if (!is.null(mu)) {
    value <- mu
    source <- "argument:mu"
    full <- TRUE
  } else {
    hit <- .pps_first(fit, c(
      "full_fit$mu_resp", "full_fit$mu_hat", "full_fit$mu",
      "mu_full", "mu_hat_full", "full_mu"
    ))
    if (is.null(hit)) {
      stop("No full-sample fitted conditional means were found. Supply `mu` ",
           "explicitly; out-of-fold-only means are not a structural plug-in.",
           call. = FALSE)
    }
    value <- hit$value
    source <- paste0("fit$", hit$source)
    full <- TRUE
  }

  if (is.function(value)) {
    z <- fit$Z
    if (is.null(z)) {
      stop("A fitted mean function was found, but the fit has no `Z` at ",
           "which to evaluate it.", call. = FALSE)
    }
    z <- .pps_to_respondent(z, fit, "Z")
    value <- value(z)
  }
  value <- .pps_to_respondent(value, fit, "mu")
  if (!is.null(fit$attr_names)) {
    target <- as.character(fit$attr_names)
    if (ncol(value) != length(target) || anyDuplicated(target)) {
      stop("Fitted coefficient names are incompatible with `mu`.", call. = FALSE)
    }
    if (!is.null(colnames(value))) {
      if (anyDuplicated(colnames(value)) || !setequal(colnames(value), target)) {
        stop("Named `mu` columns must match the fitted coefficient names.",
             call. = FALSE)
      }
      value <- value[, target, drop = FALSE]
    } else colnames(value) <- target
  }
  list(value = value, source = source, full_fit = full)
}

.pps_validate_sigma <- function(Sigma, p, coefficient_names = NULL) {
  Sigma <- as.matrix(Sigma)
  if (!is.numeric(Sigma) || !identical(dim(Sigma), c(p, p)) ||
      any(!is.finite(Sigma))) {
    stop("`Sigma` must be a finite p by p numeric matrix.", call. = FALSE)
  }
  if (max(abs(Sigma - t(Sigma))) > 1e-8) {
    stop("`Sigma` must be symmetric.", call. = FALSE)
  }
  if (!is.null(coefficient_names)) {
    rn <- rownames(Sigma); cn <- colnames(Sigma)
    if (xor(is.null(rn), is.null(cn))) {
      stop("`Sigma` must have both row and column names or neither.",
           call. = FALSE)
    }
    if (!is.null(rn)) {
      if (anyDuplicated(rn) || anyDuplicated(cn) ||
          !setequal(rn, coefficient_names) ||
          !setequal(cn, coefficient_names)) {
        stop("Named `Sigma` rows and columns must match the fitted ",
             "coefficient names.", call. = FALSE)
      }
      Sigma <- Sigma[coefficient_names, coefficient_names, drop = FALSE]
    } else dimnames(Sigma) <- list(coefficient_names, coefficient_names)
  }
  Sigma <- (Sigma + t(Sigma)) / 2
  ev <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < -1e-8 * max(1, max(abs(ev)))) {
    stop("`Sigma` must be positive semidefinite.", call. = FALSE)
  }
  ## Remove harmless numerical negativity without changing eigenvectors.
  if (min(ev) < 0) {
    ee <- eigen(Sigma, symmetric = TRUE)
    Sigma <- ee$vectors %*% diag(pmax(ee$values, 0), p) %*% t(ee$vectors)
  }
  Sigma
}

.pps_extract_sigma <- function(fit, p, Sigma = NULL, coefficient_names = NULL) {
  if (!is.null(Sigma)) {
    value <- Sigma
    source <- "argument:Sigma"
    full <- TRUE
  } else {
    hit <- .pps_first(fit, c("full_fit$Sigma", "Sigma_full", "Sigma_hat",
                             "Sigma", "full_fit$A", "A_full", "A"))
    if (!is.null(hit)) {
      value <- hit$value
      if (grepl("(^|\\$)A$|A_full$", hit$source)) value <- tcrossprod(value)
      source <- paste0("fit$", hit$source)
      full <- TRUE
    } else {
      stop("No full-sample residual covariance was found. Supply `Sigma` ",
           "explicitly; a fold-average covariance is not the structural ",
           "plug-in estimator.",
           call. = FALSE)
    }
  }
  list(value = .pps_validate_sigma(value, p, coefficient_names), source = source,
       full_fit = full)
}

.pps_extract_kappa <- function(fit, kappa = NULL) {
  if (!is.null(kappa)) {
    value <- kappa
    source <- "argument:kappa"
    full <- TRUE
  } else {
    hit <- .pps_first(fit, c("full_fit$kappa", "kappa_full", "kappa_hat",
                             "kappa"))
    if (is.null(hit)) {
      stop("No position/alternative intercept was found. Supply `kappa` ",
           "explicitly; it is not safe to assume it is zero.", call. = FALSE)
    }
    value <- hit$value
    source <- paste0("fit$", hit$source)
    full <- TRUE
  }
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop("`kappa` must be one finite number.", call. = FALSE)
  }
  list(value = as.numeric(value), source = source, full_fit = full)
}

.pps_contrast <- function(x, p, names = NULL, label = "contrast") {
  if (!is.numeric(x) || any(!is.finite(x))) {
    stop("`", label, "` must be finite and numeric.", call. = FALSE)
  }
  if (!is.null(names(x))) {
    if (is.null(names)) {
      stop("Named contrasts require coefficient names in the fit.",
           call. = FALSE)
    }
    bad <- setdiff(names(x), names)
    if (length(bad)) {
      stop("Unknown coefficient name(s) in `", label, "`: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
    out <- stats::setNames(numeric(p), names)
    out[names(x)] <- x
    return(unname(out))
  }
  if (length(x) != p) {
    stop("`", label, "` must have length ", p, ".", call. = FALSE)
  }
  as.numeric(x)
}

.pps_gh1 <- function(n_nodes = 31L) {
  if (!is.numeric(n_nodes) || length(n_nodes) != 1L || is.na(n_nodes) ||
      !is.finite(n_nodes) || n_nodes < 3L || n_nodes != as.integer(n_nodes)) {
    stop("`n_nodes` must be an integer at least 3.", call. = FALSE)
  }
  n <- as.integer(n_nodes)
  J <- matrix(0, n, n)
  off <- sqrt(seq_len(n - 1L))
  J[cbind(seq_len(n - 1L), 2L:n)] <- off
  J[cbind(2L:n, seq_len(n - 1L))] <- off
  ee <- eigen(J, symmetric = TRUE)
  ord <- order(ee$values)
  list(x = ee$values[ord], w = ee$vectors[1L, ord]^2)
}

.pps_gate <- function(value, margin, label, absolute = FALSE) {
  if (is.null(margin)) {
    return(list(pass = NA, reported = FALSE,
                reason = paste0(label, " margin was not prespecified"),
                value = value, margin = NA_real_))
  }
  if (!is.numeric(margin) || length(margin) != 1L || !is.finite(margin) ||
      margin <= 0) {
    stop("A reporting margin must be one finite positive number.",
         call. = FALSE)
  }
  checked <- if (absolute) abs(value) else value
  pass <- is.finite(checked) && checked >= margin
  list(pass = pass, reported = pass,
       reason = if (pass) "passed" else paste0(label, " is below its margin"),
       value = value, margin = margin)
}

.pps_quantity <- function(quantity, estimate, details = list(), gate = NULL,
                          sources = list()) {
  out <- list(quantity = quantity, estimate = estimate, details = details,
              gate = gate, sources = sources,
              respondent_weighting = "equal weight per respondent",
              posterior_summaries_used = FALSE)
  class(out) <- c("scmix_paper_quantity", "list")
  out
}

#' Respondent-weighted average and subgroup preferences
#'
#' Computes the paper's plug-in quantities from fitted conditional means.
#' Respondent posterior summaries are never used. When `subgroup` is supplied,
#' it must be prespecified and have one value per respondent or task.
#'
#' @param fit A mixed-logit fit containing respondent ids and fitted means.
#' @param subgroup Optional logical, character, or factor subgroup variable.
#' @return An internal `scmix_paper_quantity` object.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_theta <- function(fit, subgroup = NULL, mu = NULL) {
  mm <- .pps_extract_mu(fit, mu)
  theta <- colMeans(mm$value)
  details <- list(theta = theta)
  if (!is.null(subgroup)) {
    idx <- .pps_respondent_index(fit)
    if (length(subgroup) == length(idx$task_id)) {
      sg <- as.character(subgroup)
      reference <- sg[idx$first[idx$index]]
      if (anyNA(sg) || any(sg != reference)) {
        stop("Task-row `subgroup` values must be nonmissing and constant ",
             "within respondent.", call. = FALSE)
      }
      subgroup <- subgroup[idx$first]
    }
    if (length(subgroup) != idx$N || anyNA(subgroup)) {
      stop("`subgroup` must be nonmissing and have one value per respondent ",
           "or task.", call. = FALSE)
    }
    g <- factor(subgroup)
    subgroup_mean <- t(vapply(levels(g), function(lev) {
      colMeans(mm$value[g == lev, , drop = FALSE])
    }, numeric(ncol(mm$value))))
    rownames(subgroup_mean) <- levels(g)
    colnames(subgroup_mean) <- colnames(mm$value)
    details$subgroup <- subgroup_mean
    details$subgroup_n <- stats::setNames(as.integer(table(g)), levels(g))
  }
  .pps_quantity("average preferences", theta, details,
                sources = list(mu = mm$source, full_fit = mm$full_fit))
}

#' Population or position-neutral choice probability
#'
#' Integrates the logit choice probability over the fitted normal preference
#' distribution and then averages equally over respondents.
#'
#' @param fit A mixed-logit fit.
#' @param contrast Coefficient contrast `d`.
#' @param position_neutral If true, averages the two display positions.
#' @param kappa,mu,Sigma Optional explicit structural estimates.
#' @param n_nodes One-dimensional normal quadrature resolution.
#' @param on_support `TRUE`, `FALSE`, or `NA` after the design audit.
#' @return An internal `scmix_paper_quantity` object.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_choice <- function(fit, contrast, position_neutral = FALSE,
                               kappa = NULL, mu = NULL, Sigma = NULL,
                               n_nodes = 31L, on_support = NA) {
  mm <- .pps_extract_mu(fit, mu)
  p <- ncol(mm$value)
  ss <- .pps_extract_sigma(fit, p, Sigma, colnames(mm$value))
  kk <- .pps_extract_kappa(fit, kappa)
  d <- .pps_contrast(contrast, p, colnames(mm$value))
  eta <- as.numeric(mm$value %*% d)
  variance <- max(as.numeric(crossprod(d, ss$value %*% d)), 0)
  if (variance == 0) {
    p1 <- stats::plogis(kk$value + eta)
    p0 <- stats::plogis(-kk$value + eta)
  } else {
    gh <- .pps_gh1(n_nodes)
    sd_d <- sqrt(variance)
    p1 <- vapply(eta, function(m)
      sum(gh$w * stats::plogis(kk$value + m + sd_d * gh$x)), numeric(1L))
    p0 <- vapply(eta, function(m)
      sum(gh$w * stats::plogis(-kk$value + m + sd_d * gh$x)), numeric(1L))
  }
  estimate <- if (isTRUE(position_neutral)) mean((p1 + p0) / 2) else mean(p1)
  support_status <- if (isTRUE(on_support)) "on randomized support" else
    if (identical(on_support, FALSE)) "structural extrapolation" else
      "support not audited"
  .pps_quantity(
    if (isTRUE(position_neutral)) "position-neutral choice probability" else
      "population choice probability",
    estimate,
    details = list(contrast = d, directional_variance = variance,
                   conditional_probability = if (position_neutral)
                     (p1 + p0) / 2 else p1,
                   support = support_status),
    sources = list(mu = mm$source, Sigma = ss$source, kappa = kk$source,
                   full_fit = mm$full_fit && ss$full_fit && kk$full_fit)
  )
}

#' Marginal rate of substitution evaluated at average preferences
#'
#' Computes `-tau(c_a) / tau(c_b)` and applies a prespecified substantive
#' denominator margin. The estimate is retained for audit even when the
#' reporting gate fails.
#'
#' @param fit A mixed-logit fit.
#' @param numerator,denominator Contrasts `c_a` and `c_b`.
#' @param denominator_margin Prespecified lower bound on the absolute
#'   denominator. `NULL` leaves the reporting gate unconfigured.
#' @return An internal `scmix_paper_quantity` object.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_mrs <- function(fit, numerator, denominator,
                            denominator_margin = NULL, mu = NULL) {
  mm <- .pps_extract_mu(fit, mu)
  p <- ncol(mm$value)
  ca <- .pps_contrast(numerator, p, colnames(mm$value), "numerator")
  cb <- .pps_contrast(denominator, p, colnames(mm$value), "denominator")
  theta <- colMeans(mm$value)
  tau_a <- sum(ca * theta)
  tau_b <- sum(cb * theta)
  estimate <- if (tau_b != 0) -tau_a / tau_b else NA_real_
  gate <- .pps_gate(tau_b, denominator_margin, "MRS denominator", absolute = TRUE)
  .pps_quantity("MRS at average preferences", estimate,
                details = list(theta = theta, numerator = tau_a,
                               denominator = tau_b, c_a = ca, c_b = cb),
                gate = gate,
                sources = list(mu = mm$source, full_fit = mm$full_fit))
}

#' Preference-direction share under the fitted normal distribution
#'
#' At positive directional residual variance the conditional share is a normal
#' CDF. At zero variance, `ties = "exclude"` implements the strict sign-share
#' inequality and `ties = "include"` implements the compensating-share
#' convention.
#'
#' @param fit A mixed-logit fit.
#' @param contrast Direction `c`.
#' @param ties Whether exact zero is excluded or included.
#' @param variance_margin Prespecified lower bound on directional residual
#'   variance for regular inference. `NULL` leaves the gate unconfigured.
#' @param ci Optional two-element confidence interval for the majority gate.
#' @return An internal `scmix_paper_quantity` object.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_signshare <- function(fit, contrast, ties = c("exclude", "include"),
                                  variance_margin = NULL, ci = NULL,
                                  mu = NULL, Sigma = NULL) {
  ties <- match.arg(ties)
  mm <- .pps_extract_mu(fit, mu)
  p <- ncol(mm$value)
  ss <- .pps_extract_sigma(fit, p, Sigma, colnames(mm$value))
  d <- .pps_contrast(contrast, p, colnames(mm$value))
  directional_mean <- as.numeric(mm$value %*% d)
  variance <- max(as.numeric(crossprod(d, ss$value %*% d)), 0)
  conditional <- if (variance > 0) {
    stats::pnorm(directional_mean / sqrt(variance))
  } else if (identical(ties, "include")) {
    as.numeric(directional_mean >= 0)
  } else {
    as.numeric(directional_mean > 0)
  }
  gate <- .pps_gate(variance, variance_margin,
                    "directional residual variance", absolute = FALSE)
  majority <- "no regular majority claim"
  if (!is.null(ci)) {
    if (!is.numeric(ci) || length(ci) != 2L || any(!is.finite(ci)) || ci[1L] > ci[2L]) {
      stop("`ci` must be a finite ordered two-element interval.", call. = FALSE)
    }
    if (ci[1L] > 0.5) majority <- "above one-half"
    if (ci[2L] < 0.5) majority <- "below one-half"
  }
  .pps_quantity("preference-direction share", mean(conditional),
                details = list(contrast = d, conditional_share = conditional,
                               directional_variance = variance,
                               ties = ties, majority_claim = majority, ci = ci),
                gate = gate,
                sources = list(mu = mm$source, Sigma = ss$source,
                               full_fit = mm$full_fit && ss$full_fit))
}

#' Compensating-benefit share
#'
#' Computes `Pr{(c_p + a c_b)' beta >= 0}` and therefore counts exact ties as
#' compensated when directional residual variance is zero.
#'
#' @param fit A mixed-logit fit.
#' @param penalty,benefit Contrasts `c_p` and `c_b`.
#' @param amount Benefit amount `a`.
#' @return An internal `scmix_paper_quantity` object.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_compensating <- function(fit, penalty, benefit, amount,
                                     variance_margin = NULL, ci = NULL,
                                     mu = NULL, Sigma = NULL) {
  mm <- .pps_extract_mu(fit, mu)
  p <- ncol(mm$value)
  ss <- .pps_extract_sigma(fit, p, Sigma, colnames(mm$value))
  cp <- .pps_contrast(penalty, p, colnames(mm$value), "penalty")
  cb <- .pps_contrast(benefit, p, colnames(mm$value), "benefit")
  if (!is.numeric(amount) || length(amount) != 1L || !is.finite(amount)) {
    stop("`amount` must be one finite number.", call. = FALSE)
  }
  out <- scmix_paper_signshare(fit, cp + amount * cb, ties = "include",
                               variance_margin = variance_margin, ci = ci,
                               mu = mm$value, Sigma = ss$value)
  out$quantity <- "compensating-benefit share"
  out$details$penalty <- cp
  out$details$benefit <- cb
  out$details$amount <- amount
  out$sources <- list(mu = mm$source, Sigma = ss$source,
                      full_fit = mm$full_fit && ss$full_fit)
  out
}

#' Preference-heterogeneity decomposition
#'
#' Computes the respondent-weighted between-covariate component, residual
#' component, total component, and optional directional variance share.
#'
#' @param fit A mixed-logit fit.
#' @param direction Optional contrast `c`.
#' @param total_margin Prespecified lower bound on total directional
#'   heterogeneity before reporting its explained share.
#' @return An internal `scmix_paper_quantity` object.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_heterogeneity <- function(fit, direction = NULL,
                                      total_margin = NULL,
                                      mu = NULL, Sigma = NULL) {
  mm <- .pps_extract_mu(fit, mu)
  p <- ncol(mm$value)
  ss <- .pps_extract_sigma(fit, p, Sigma, colnames(mm$value))
  theta <- colMeans(mm$value)
  centered <- sweep(mm$value, 2L, theta, `-`)
  Omega_Z <- crossprod(centered) / nrow(centered)
  Omega_R <- ss$value
  Omega_T <- Omega_Z + Omega_R
  details <- list(theta = theta, Omega_Z = Omega_Z, Omega_R = Omega_R,
                  Omega_T = Omega_T)
  estimate <- Omega_T
  gate <- NULL
  if (!is.null(direction)) {
    d <- .pps_contrast(direction, p, colnames(mm$value), "direction")
    H_Z <- as.numeric(crossprod(d, Omega_Z %*% d))
    H_R <- as.numeric(crossprod(d, Omega_R %*% d))
    H_T <- H_Z + H_R
    share_Z <- if (H_T > 0) H_Z / H_T else NA_real_
    gate <- .pps_gate(H_T, total_margin, "total directional heterogeneity")
    estimate <- c(H_Z = H_Z, H_R = H_R, H_T = H_T,
                  share_Z = share_Z)
    details$direction <- d
  }
  .pps_quantity("preference heterogeneity", estimate, details, gate,
                sources = list(mu = mm$source, Sigma = ss$source,
                               full_fit = mm$full_fit && ss$full_fit))
}

#' Collect the paper's plug-in quantities
#'
#' Each optional specification is a named argument list passed to its
#' corresponding `scmix_paper_*` routine. This is a convenience collector, not
#' an inference routine; regular intervals require the cross-fitted one-step
#' procedure described in the paper.
#'
#' @param fit A mixed-logit fit.
#' @param subgroup Optional subgroup vector for average preferences.
#' @param choice,mrs,sign,compensating,heterogeneity Optional named lists of
#'   arguments for the corresponding quantity routine.
#' @return A named list of `scmix_paper_quantity` objects.
#' @rdname scmix_paper_quantities
#' @export
scmix_paper_quantities <- function(fit, subgroup = NULL, choice = NULL,
                                   mrs = NULL, sign = NULL,
                                   compensating = NULL,
                                   heterogeneity = NULL) {
  out <- list(theta = scmix_paper_theta(fit, subgroup = subgroup))
  add <- function(fun, spec) {
    if (is.null(spec)) return(NULL)
    if (!is.list(spec)) stop("A quantity specification must be a named list.",
                             call. = FALSE)
    do.call(fun, c(list(fit = fit), spec))
  }
  if (!is.null(choice)) out$choice <- add(scmix_paper_choice, choice)
  if (!is.null(mrs)) out$mrs <- add(scmix_paper_mrs, mrs)
  if (!is.null(sign)) out$sign <- add(scmix_paper_signshare, sign)
  if (!is.null(compensating)) {
    out$compensating <- add(scmix_paper_compensating, compensating)
  }
  if (!is.null(heterogeneity)) {
    out$heterogeneity <- add(scmix_paper_heterogeneity, heterogeneity)
  }
  attr(out, "inference_note") <- paste(
    "These are respondent-weighted plug-ins.",
    "Regular inference requires the paper's cross-fitted one-step/Riesz procedure."
  )
  class(out) <- c("scmix_paper_quantities", "list")
  out
}

#' @export
print.scmix_paper_quantity <- function(x, ...) {
  cat("paperps structural plug-in quantity:", x$quantity, "\n")
  print(x$estimate)
  if (!is.null(x$gate)) {
    cat("  reporting gate:", x$gate$reason, "\n")
  }
  cat("  posterior respondent summaries used: no\n")
  invisible(x)
}

#' @export
as.data.frame.scmix_paper_quantity <- function(x, ...) {
  value <- unlist(x$estimate, recursive = TRUE, use.names = TRUE)
  if (!is.numeric(value)) {
    stop("This quantity does not flatten to a numeric estimate table.",
         call. = FALSE)
  }
  if (is.null(names(value))) names(value) <- paste0("value_", seq_along(value))
  data.frame(component = names(value), estimate = as.numeric(value),
             stringsAsFactors = FALSE)
}

#' @export
print.scmix_paper_quantities <- function(x, ...) {
  cat("paperps respondent-weighted structural plug-ins\n")
  for (nm in names(x)) {
    cat("\n", nm, ":\n", sep = "")
    print(x[[nm]])
  }
  invisible(x)
}
