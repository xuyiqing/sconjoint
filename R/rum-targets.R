# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/targets.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

.rum_cov_rows <- function(x) {
  x <- as.matrix(x)
  z <- sweep(x, 2L, colMeans(x), "-")
  crossprod(z) / nrow(x)
}

.rum_inference_context <- function(fit) {
  d <- fit$data
  h <- 2L + d$r + d$p
  par <- fit$par
  loading <- par[1L + d$r + seq_len(d$p)]
  out <- list(ok = FALSE, reason = "fit_not_regular_candidate", inverse = NULL,
              eigenvalues = NULL, reciprocal_condition = NA_real_)
  if (length(par) != h || any(!is.finite(par))) {
    out$reason <- "invalid_parameter"; return(out)
  }
  if (par[h] <= 0 || all(loading == 0)) {
    out$reason <- "full_model_boundary"; return(out)
  }
  if (!isTRUE(fit$regular_candidate)) return(out)
  info <- as.matrix(fit$information)
  score <- as.matrix(fit$score)
  if (!identical(dim(info), c(h, h)) || any(!is.finite(info)) ||
      !identical(dim(score), c(d$n, h)) || any(!is.finite(score))) {
    out$reason <- "invalid_information_or_score"; return(out)
  }
  info <- (info + t(info)) / 2
  out$eigenvalues <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
  out$reciprocal_condition <- rcond(info)
  ch <- tryCatch(chol(info), error = function(e) NULL)
  if (is.null(ch)) {
    out$reason <- "information_not_positive_definite"; return(out)
  }
  out$inverse <- chol2inv(ch)
  if (any(!is.finite(out$inverse))) {
    out$reason <- "information_solve_nonfinite"; return(out)
  }
  out$ok <- TRUE
  out$reason <- "regular_candidate_not_coverage_certificate"
  out
}

.rum_contrasts <- function(contrasts, data) {
  if (is.numeric(contrasts) || is.matrix(contrasts)) contrasts <- list(contrast = contrasts)
  if (!is.list(contrasts) || !length(contrasts)) stop("contrasts must be a nonempty list")
  nm <- names(contrasts)
  if (is.null(nm)) nm <- paste0("contrast", seq_along(contrasts))
  if (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) stop("contrast names must be unique and nonempty")
  ans <- lapply(contrasts, function(x) {
    if (is.null(dim(x))) {
      if (!is.numeric(x) || length(x) != data$p) stop("a fixed contrast must have length p")
      x <- matrix(rep(x, each = data$n), data$n, data$p)
    }
    if (!is.numeric(x) || !identical(dim(x), c(data$n, data$p)) || any(!is.finite(x)))
      stop("each contrast must be finite with dimensions N by p")
    x
  })
  names(ans) <- nm
  ans
}

# Primary latent utility targets for a fixed finite-dimensional RUM
#
# Population standardization includes the direct respondent-composition term.
# Fixed standardization uses this data object's covariate bank as a declared
# reference, omitting its population sampling term. Neither mode verifies its
# sampling, model, regularity or numerical assumptions. Fixed-loading variance
# intervals map the entire signed-loading interval through squaring.
#' @export
rum_targets <- function(fit, contrasts, reference = c("population", "fixed"),
                        alpha = .05, infer = TRUE,
                        hz_interval = c("wald", "fixed_rank_one_projection")) {
  reference <- match.arg(reference)
  hz_interval <- match.arg(hz_interval)
  if (hz_interval == "fixed_rank_one_projection" && reference != "fixed")
    stop("Rank-one HZ projection requires a genuinely fixed reference distribution.")
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("alpha must lie strictly between zero and one")
  data <- fit$data
  if (is.null(data) || is.null(fit$par)) stop("fit must contain data and par")
  .rum_check_data(data)
  n <- data$n; p <- data$p; r <- data$r; h <- 2L + r + p
  par <- fit$par
  .rum_check_par(data, par)
  ai <- 1L + seq_len(r); li <- 1L + r + seq_len(p)
  lam <- par[li]; alp <- par[ai]
  cc <- .rum_contrasts(contrasts, data)
  mu <- do.call(rbind, lapply(data$basis, function(b) as.numeric(b %*% alp)))
  if (!identical(dim(mu), c(n, p))) stop("invalid mean basis")
  comp <- grad <- rows <- list()
  fixed <- vapply(cc, function(x) all(x == matrix(rep(x[1L, ], each = n), n, p)), logical(1))
  mm <- lapply(cc, function(x) rowSums(x * mu))
  ee <- lapply(cc, function(x) as.numeric(x %*% lam))
  qq <- lapply(cc, function(x) do.call(rbind, lapply(seq_len(n), function(i)
    as.numeric(crossprod(data$basis[[i]], x[i, ])))))
  hz_factors <- if (hz_interval == "fixed_rank_one_projection")
    lapply(qq, .rum_fixed_rank_one_factor) else NULL
  add <- function(type, j, k = NA_integer_, value, g, b) {
    idx <- length(rows) + 1L
    nm1 <- names(cc)[j]; nm2 <- if (is.na(k)) NA_character_ else names(cc)[k]
    id <- paste(type, nm1, if (is.na(k)) "" else nm2, sep = ":")
    id <- sub(":$", "", id)
    rows[[idx]] <<- data.frame(target = id, type = type, contrast1 = nm1,
                              contrast2 = nm2, estimate = value, stringsAsFactors = FALSE)
    grad[[idx]] <<- g
    comp[[idx]] <<- if (reference == "population") b else rep(0, n)
  }
  means <- vapply(mm, mean, numeric(1))
  for (j in seq_along(cc)) {
    g <- numeric(h); g[ai] <- colMeans(qq[[j]])
    add("mean", j, value = means[j], g = g, b = mm[[j]] - means[j])
    mc <- mm[[j]] - means[j]
    gz <- numeric(h); gz[ai] <- 2 * colMeans(qq[[j]] * mc)
    gr <- numeric(h); gr[li] <- 2 * colMeans(cc[[j]] * ee[[j]])
    hz <- mean(mc^2); hr <- mean(ee[[j]]^2)
    bz <- mc^2 - hz; br <- ee[[j]]^2 - hr
    add("HZ", j, value = hz, g = gz, b = bz)
    add("HR", j, value = hr, g = gr, b = br)
    add("HT", j, value = hz + hr, g = gz + gr, b = bz + br)
    if (fixed[j]) {
      gl <- numeric(h); gl[li] <- cc[[j]][1L, ]
      add("loading", j, value = ee[[j]][1L], g = gl, b = rep(0, n))
    }
  }
  if (length(cc) > 1L) for (j in seq_len(length(cc) - 1L)) for (k in (j + 1L):length(cc)) {
    mj <- mm[[j]] - means[j]; mk <- mm[[k]] - means[k]
    gz <- numeric(h); gz[ai] <- colMeans(qq[[j]] * mk + qq[[k]] * mj)
    gr <- numeric(h); gr[li] <- colMeans(cc[[j]] * ee[[k]] + cc[[k]] * ee[[j]])
    kz <- mean(mj * mk); kr <- mean(ee[[j]] * ee[[k]])
    bz <- mj * mk - kz; br <- ee[[j]] * ee[[k]] - kr
    add("KZ", j, k, kz, gz, bz)
    add("KR", j, k, kr, gr, br)
    add("KT", j, k, kz + kr, gz + gr, bz + br)
  }
  tab <- do.call(rbind, rows); rownames(tab) <- NULL
  gradient <- do.call(rbind, grad); composition <- do.call(cbind, comp)
  dimnames(gradient) <- list(tab$target, names(par))
  colnames(composition) <- tab$target
  tab$se <- tab$lower <- tab$upper <- NA_real_
  tab$interval_method <- "withheld"
  tab$status <- if (infer) "fit_not_regular_candidate" else "inference_not_requested"
  context <- if (infer) .rum_inference_context(fit) else list(ok = FALSE, reason = "inference_not_requested")
  influence <- covariance <- NULL
  if (context$ok) {
    influence <- composition + fit$score %*% context$inverse %*% t(gradient)
    colnames(influence) <- tab$target
    covariance <- .rum_cov_rows(influence) / n
    se <- sqrt(diag(covariance)); tab$se <- se
    good <- is.finite(se) & se > 0
    critical <- stats::qnorm(1 - alpha / 2)
    tab$lower[good] <- tab$estimate[good] - critical * se[good]
    tab$upper[good] <- tab$estimate[good] + critical * se[good]
    tab$interval_method[good] <- "marginal_regular_wald"
    tab$status[good] <- "regular_candidate_not_coverage_certificate"
    tab$status[!good] <- "degenerate_target_inference_withheld"
    if (!is.null(hz_factors)) for (j in seq_along(cc)) {
      iz <- which(tab$type == "HZ" & tab$contrast1 == names(cc)[j])
      factor <- hz_factors[[j]]
      if (factor$rank == 0L) {
        tab$estimate[iz] <- tab$se[iz] <- 0
        tab$lower[iz] <- tab$upper[iz] <- 0
        gradient[iz, ] <- 0; composition[, iz] <- 0
        influence[, iz] <- 0; covariance[iz, ] <- covariance[, iz] <- 0
        tab$interval_method[iz] <- "fixed_reference_structural_zero"
        tab$status[iz] <- "known_zero_for_every_parameter_in_declared_basis"
      } else {
        ge <- numeric(h); ge[ai] <- factor$coefficient
        e <- sum(factor$coefficient * alp)
        ei <- drop(fit$score %*% context$inverse %*% ge)
        ese <- sqrt(.rum_cov_rows(matrix(ei, ncol = 1L))[1L, 1L] / n)
        if (is.finite(ese) && ese > 0) {
          ends <- e + c(-1, 1) * critical * ese
          tab$lower[iz] <- if (ends[1L] <= 0 && ends[2L] >= 0) 0 else min(ends^2)
          tab$upper[iz] <- max(ends^2)
          tab$interval_method[iz] <- "squared_mean_projection_fixed_reference"
          tab$status[iz] <- "regular_mean_projection_not_coverage_certificate"
          hz_factors[[j]]$linear_estimate <- e
          hz_factors[[j]]$linear_se <- ese
          hz_factors[[j]]$linear_interval <- ends
        } else {
          tab$lower[iz] <- tab$upper[iz] <- NA_real_
          tab$interval_method[iz] <- "withheld"
          tab$status[iz] <- "degenerate_mean_projection_withheld"
        }
      }
    }
    # Always use loading projection for fixed-contrast residual variances;
    # there is no preliminary significance or distance-to-boundary screen.
    for (j in which(fixed)) {
      il <- which(tab$type == "loading" & tab$contrast1 == names(cc)[j])
      iv <- which(tab$type == "HR" & tab$contrast1 == names(cc)[j])
      if (length(il) && good[il]) {
        ends <- c(tab$lower[il], tab$upper[il])
        tab$lower[iv] <- if (ends[1L] <= 0 && ends[2L] >= 0) 0 else min(ends^2)
        tab$upper[iv] <- max(ends^2)
        tab$interval_method[iv] <- "squared_loading_projection"
        tab$status[iv] <- "regular_loading_projection_not_coverage_certificate"
      }
    }
    # A Bonferroni joint loading rectangle gives a conservative covariance
    # image, including when the covariance's own first derivative vanishes.
    for (i in which(tab$type == "KR")) {
      j <- match(tab$contrast1[i], names(cc)); k <- match(tab$contrast2[i], names(cc))
      if (!fixed[j] || !fixed[k]) next
      ix <- c(which(tab$type == "loading" & tab$contrast1 == names(cc)[j]),
              which(tab$type == "loading" & tab$contrast1 == names(cc)[k]))
      if (length(ix) == 2L && all(good[ix])) {
        z <- stats::qnorm(1 - alpha / 4)
        a <- tab$estimate[ix[1L]] + c(-1, 1) * z * se[ix[1L]]
        b <- tab$estimate[ix[2L]] + c(-1, 1) * z * se[ix[2L]]
        values <- as.vector(outer(a, b))
        tab$lower[i] <- min(values); tab$upper[i] <- max(values)
        tab$interval_method[i] <- "joint_loading_rectangle_projection"
        tab$status[i] <- "regular_loading_projection_not_coverage_certificate"
      }
    }
  } else tab$status <- context$reason
  structure(list(table = tab, gradient = gradient, composition = composition,
                 influence = influence, covariance = covariance,
                 reference = reference, alpha = alpha, contrasts = cc,
                 hz_interval = hz_interval, hz_projection = hz_factors,
                 fixed_contrasts = fixed, information_diagnostics = context,
                 interval_scope = "Per-target intervals; not simultaneous across the reported target family",
                 assumptions = "Correct fixed model, valid sampling, regular full parameter chart, negligible numerical errors and empirical L2 influence consistency are required"),
            class = "rum_targets")
}

# Factor the known reference-design quadratic form, never an estimated
# information matrix. Refuse higher rank instead of discarding directions.
.rum_fixed_rank_one_factor <- function(q) {
  n <- nrow(q); r <- ncol(q)
  if (all(q == matrix(rep(q[1L, ], each = n), n, r)))
    return(list(rank = 0L, coefficient = rep(0, r), relative_residual = 0))
  centered <- sweep(q, 2L, colMeans(q), "-") / sqrt(n)
  ss <- svd(centered, nu = 0L, nv = 1L)
  coefficient <- ss$d[1L] * ss$v[, 1L]
  axis <- ss$v[, 1L]
  remainder <- centered - tcrossprod(drop(centered %*% axis), axis)
  error <- sqrt(sum(remainder^2) / sum(centered^2))
  if (!is.finite(error) || error > 64 * .Machine$double.eps)
    stop("The fixed-reference HZ form is not numerically rank one; no directions were dropped.")
  list(rank = 1L, coefficient = coefficient, relative_residual = error,
       scope = "Known fixed reference-design form; not a statistical rank test")
}
