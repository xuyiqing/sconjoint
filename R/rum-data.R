# Ported from conjointRUM 0.1.3 (Avidit Acharya), R/data.R, unchanged except that
# roxygen-style comments are plain comments (the help pages are the hand-written Rd
# files in man/) and each export carries a roxygen @export tag.

# Complete respondent records; numerical feature coding is supplied by an adapter.
#' @export
rum_data <- function(y, d, mean_basis = NULL, ids = NULL,
                     feature_names = NULL, mean_names = NULL, metadata = list()) {
  if (!is.list(y) || !is.list(d) || !length(y) || length(y) != length(d))
    stop("y and d must be nonempty lists with one entry per respondent.")
  n <- length(y)
  if (!all(vapply(d, is.matrix, logical(1)))) stop("Each d must be a matrix.")
  p <- ncol(d[[1]])
  if (!p || !all(vapply(d, function(x) is.numeric(x) && ncol(x) == p &&
                        nrow(x) > 0L && all(is.finite(x)), logical(1))))
    stop("Task matrices must have the same positive feature count and finite values.")
  tasks <- vapply(d, nrow, integer(1))
  if (!all(vapply(seq_len(n), function(i) is.numeric(y[[i]]) &&
    length(y[[i]]) == tasks[i] && all(is.finite(y[[i]])) &&
    all(y[[i]] %in% c(0, 1)), logical(1))))
    stop("Each y must contain one numeric binary response per task; no missing tasks are dropped.")
  if (is.null(ids)) ids <- as.character(seq_len(n))
  ids <- as.character(ids)
  if (length(ids) != n || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))
    stop("Respondent IDs must be unique, nonmissing, nonempty, and match the records.")
  if (!is.list(metadata) || is.null(metadata$orientation) ||
      length(metadata$orientation) != 1L || !is.character(metadata$orientation) ||
      is.na(metadata$orientation) || !nzchar(metadata$orientation))
    stop("Supply metadata$orientation describing exactly what y=1 means.")
  if (is.null(mean_basis)) mean_basis <- diag(p)
  if (is.matrix(mean_basis)) mean_basis <- rep(list(mean_basis), n)
  if (!is.list(mean_basis) || length(mean_basis) != n ||
      !all(vapply(mean_basis, is.matrix, logical(1))))
    stop("mean_basis must be a common p by r matrix or one such matrix per respondent.")
  r <- ncol(mean_basis[[1]])
  if (!r || !all(vapply(mean_basis, function(b) is.numeric(b) &&
      identical(dim(b), c(p, r)) && all(is.finite(b)), logical(1))))
    stop("All mean bases must be finite p by r matrices with common dimensions.")
  if (is.null(feature_names)) feature_names <- colnames(d[[1]])
  if (is.null(feature_names)) feature_names <- paste0("feature", seq_len(p))
  if (is.null(mean_names)) mean_names <- colnames(mean_basis[[1]])
  if (is.null(mean_names)) mean_names <- paste0("mean", seq_len(r))
  check_names <- function(x, k) is.character(x) && length(x) == k &&
    !anyNA(x) && all(nzchar(x)) && !anyDuplicated(x)
  if (!check_names(feature_names, p) || !check_names(mean_names, r))
    stop("Feature and mean names must be unique nonempty strings of the right length.")
  packed_d <- do.call(rbind, d)
  packed_a <- do.call(rbind, lapply(seq_len(n), function(i) d[[i]] %*% mean_basis[[i]]))
  colnames(packed_d) <- feature_names
  colnames(packed_a) <- mean_names
  structure(list(y = as.numeric(unlist(y, use.names = FALSE)),
    d = unname(packed_d), a = unname(packed_a),
    offsets = as.integer(c(0, cumsum(tasks))), basis = mean_basis,
    n = n, p = p, r = r, T = tasks, ids = ids,
    feature_names = feature_names, mean_names = mean_names, metadata = metadata),
    class = "rum_data")
}

#' @export
rum_mean_basis <- function(z, feature_names) {
  z <- as.matrix(z)
  if (!is.numeric(z) || !nrow(z) || !ncol(z) || any(!is.finite(z)))
    stop("z must be a finite numeric moderator-basis matrix; include an intercept explicitly.")
  if (!is.character(feature_names) || !length(feature_names) ||
      anyNA(feature_names) || anyDuplicated(feature_names) || any(!nzchar(feature_names)))
    stop("Supply distinct feature names.")
  p <- length(feature_names)
  zn <- colnames(z)
  if (is.null(zn)) zn <- paste0("basis", seq_len(ncol(z)))
  mn <- unlist(lapply(zn, function(nm) paste(feature_names, nm, sep = ":")))
  lapply(seq_len(nrow(z)), function(i) {
    b <- kronecker(matrix(z[i, ], nrow = 1L), diag(p))
    dimnames(b) <- list(feature_names, mn)
    b
  })
}

.rum_check_data <- function(data) {
  if (!inherits(data, "rum_data")) stop("Expected a rum_data object.")
  invisible(data)
}

#' @export
rum_parameters <- function(data, kappa = 0, alpha = rep(0, data$r),
                           loading = rep(0, data$p), position_variance = .25) {
  .rum_check_data(data)
  if (length(kappa) != 1L || length(alpha) != data$r ||
      length(loading) != data$p || length(position_variance) != 1L)
    stop("Parameter dimensions do not match the declared mean and features.")
  par <- c(kappa, alpha, loading, position_variance)
  if (!is.numeric(par) || any(!is.finite(par)) || position_variance < 0)
    stop("Parameters must be finite and position variance nonnegative.")
  names(par) <- c("kappa", paste0("alpha:", data$mean_names),
                 paste0("loading:", data$feature_names), "omega")
  par
}

.rum_check_par <- function(data, par) {
  if (!is.numeric(par) || length(par) != 2L + data$r + data$p ||
      any(!is.finite(par)) || tail(par, 1) < 0)
    stop("Invalid parameter vector: expected kappa, alpha, loading, nonnegative omega.")
  invisible(par)
}

#' @export
rum_subset <- function(data, respondents) {
  .rum_check_data(data)
  if (is.character(respondents)) respondents <- match(respondents, data$ids)
  if (!is.numeric(respondents) || !length(respondents) || anyNA(respondents) ||
      any(respondents != as.integer(respondents)) ||
      any(respondents < 1L | respondents > data$n) || anyDuplicated(respondents))
    stop("Select unique existing respondents by index or ID.")
  rows <- lapply(respondents, function(i) seq.int(data$offsets[i] + 1L, data$offsets[i + 1L]))
  rum_data(lapply(rows, function(j) data$y[j]),
    lapply(rows, function(j) data$d[j, , drop = FALSE]), data$basis[respondents],
    data$ids[respondents], data$feature_names, data$mean_names, data$metadata)
}

#' @export
rum_scale_features <- function(data, scales) {
  .rum_check_data(data)
  if (!is.numeric(scales) || length(scales) != data$p ||
      any(!is.finite(scales)) || any(scales <= 0)) stop("Provide p positive feature scales.")
  result <- data
  result$d <- sweep(data$d, 2L, scales, "/")
  result$basis <- lapply(data$basis, function(b) sweep(b, 1L, scales, "*"))
  # d*' B* = d' B, so the alpha design is exactly unchanged.
  result$metadata$feature_scales <- scales
  result
}

#' @export
rum_scale_parameters <- function(data, par, scales, inverse = FALSE) {
  .rum_check_data(data); .rum_check_par(data, par)
  if (!is.numeric(scales) || length(scales) != data$p ||
      any(!is.finite(scales)) || any(scales <= 0)) stop("Provide p positive feature scales.")
  j <- 1L + data$r + seq_len(data$p)
  par[j] <- if (inverse) par[j] / scales else par[j] * scales
  par
}

.rum_with_seed <- function(seed, expr) {
  if (is.null(seed)) return(force(expr))
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) prior <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had) assign(".Random.seed", prior, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  force(expr)
}

#' @export
rum_simulate <- function(data, par, seed = NULL, policy_law = "normal") {
  .rum_check_data(data); .rum_check_par(data, par)
  policy_law <- match.arg(policy_law, c("normal", "three_point"))
  .rum_with_seed(seed, {
    u <- if (policy_law == "normal") stats::rnorm(data$n) else
      sample(c(-sqrt(3), 0, sqrt(3)), data$n, replace = TRUE, prob = c(1, 4, 1)/6)
    v <- stats::rnorm(data$n)
    loading <- par[1L + data$r + seq_len(data$p)]
    eta <- par[1] + drop(data$a %*% par[1L + seq_len(data$r)]) +
      drop(data$d %*% loading) * rep(u, data$T) + sqrt(tail(par, 1)) * rep(v, data$T)
    result <- data
    result$y <- stats::rbinom(length(eta), 1L, stats::plogis(eta))
    result$metadata$simulation <- list(seed = seed, policy_law = policy_law,
                                      truth = par, shared_effects = TRUE)
    result
  })
}

#' @export
print.rum_data <- function(x, ...) {
  cat("Repeated-choice data:", x$n, "respondents,", length(x$y), "tasks,",
      x$p, "utility features,", x$r, "mean coefficients.\n")
  cat("Outcome orientation:", x$metadata$orientation, "\n")
  invisible(x)
}
