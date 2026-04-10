## Preference clustering.
##
## `sc_clusters()` partitions respondents into `k` clusters by running
## k-means on the full per-row `beta_hat(Z_i)` matrix.  This is a
## descriptive tool for identifying latent preference types -- users
## typically follow up with plotting the within-cluster centroids
## (`sc_plot(fit, type = "cluster_profiles")`, pending in M6).  The
## return object carries the length-N cluster assignment, the k-by-p
## matrix of cluster centers, the within-cluster sum of squares, and
## the per-cluster sizes.  The procedure is deterministic whenever
## `seed` is supplied (plus an explicit `nstart` to protect against
## k-means local optima).

#' Preference clustering via k-means on `beta_hat(Z)`
#'
#' Run k-means clustering on the per-row `beta_hat` matrix of an
#' `sc_fit` and return the partition along with descriptive
#' statistics.  The algorithm is deterministic given `seed`; when
#' `seed = NULL` the assignment may vary across runs due to k-means
#' random initialization.
#'
#' Columns of `beta_hat` are optionally standardized (`scale = TRUE`,
#' the default) so that dummies with wider β ranges do not dominate
#' the Euclidean distance.  This matches the prototype's clustering
#' recipe in `07b_structural_quantities.R` §7.
#'
#' @param object An `sc_fit`.
#' @param k Integer number of clusters.  Default 3.
#' @param method Clustering method, currently only `"kmeans"` is
#'   supported.  `"hclust"` is accepted as a synonym for average-
#'   linkage hierarchical clustering on the same matrix and is
#'   provided for reproducibility of prototype analyses.
#' @param scale Logical, scale columns of `beta_hat` before
#'   clustering (default `TRUE`).
#' @param nstart Integer number of random k-means restarts (default
#'   25), passed through to `stats::kmeans`.
#' @param seed Optional integer.  When supplied, the R RNG state is
#'   set to `seed` before running k-means and restored on exit, so
#'   the partition is reproducible.
#' @param ... Unused; reserved for future extensions.
#' @return An `sc_quantity` whose `estimate` is a named list with
#'   fields `cluster_assignment` (length-N integer), `centers`
#'   (k-by-p numeric), `within_ss` (length-k numeric), `sizes`
#'   (length-k integer), and `total_within_ss` (scalar).
#' @export
sc_clusters <- function(object,
                        k = 3L,
                        method = c("kmeans", "hclust"),
                        scale = TRUE,
                        nstart = 25L,
                        seed = NULL,
                        ...) {
  stopifnot(inherits(object, "sc_fit"))
  method <- match.arg(method)
  if (!is.numeric(k) || length(k) != 1L || k < 2L) {
    stop("sc_clusters(): `k` must be an integer >= 2.")
  }
  k <- as.integer(k)
  B <- object$beta_hat
  N <- nrow(B)
  if (k > N) {
    stop("sc_clusters(): `k` exceeds number of rows in beta_hat.")
  }
  ## Standardize column-wise if requested.
  if (isTRUE(scale)) {
    col_sd <- apply(B, 2L, stats::sd)
    col_sd[col_sd == 0] <- 1
    col_mean <- colMeans(B)
    X <- sweep(sweep(B, 2L, col_mean, "-"), 2L, col_sd, "/")
  } else {
    X <- B
  }

  if (!is.null(seed)) {
    old_rng <- if (exists(".Random.seed", envir = .GlobalEnv))
                 .GlobalEnv$.Random.seed else NULL
    on.exit({
      if (is.null(old_rng)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_rng, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  if (method == "kmeans") {
    km <- stats::kmeans(X, centers = k, nstart = as.integer(nstart),
                        iter.max = 50L)
    assignment <- as.integer(km$cluster)
    ## Centers on the original (unscaled) beta scale for interpretability.
    centers <- matrix(NA_real_, k, ncol(B),
                      dimnames = list(NULL, colnames(B)))
    sizes <- integer(k)
    within_ss <- numeric(k)
    for (c in seq_len(k)) {
      rows <- which(assignment == c)
      sizes[c] <- length(rows)
      if (length(rows) > 0L) {
        centers[c, ] <- colMeans(B[rows, , drop = FALSE])
      }
    }
    within_ss <- as.numeric(km$withinss)
    total_within <- sum(within_ss)
  } else {
    ## Hierarchical: cut at k.
    d <- stats::dist(X)
    hc <- stats::hclust(d, method = "average")
    assignment <- as.integer(stats::cutree(hc, k = k))
    centers <- matrix(NA_real_, k, ncol(B),
                      dimnames = list(NULL, colnames(B)))
    sizes <- integer(k)
    within_ss <- numeric(k)
    for (c in seq_len(k)) {
      rows <- which(assignment == c)
      sizes[c] <- length(rows)
      if (length(rows) > 0L) {
        centers[c, ] <- colMeans(B[rows, , drop = FALSE])
        diffs <- sweep(B[rows, , drop = FALSE], 2L, centers[c, ], "-")
        within_ss[c] <- sum(diffs^2)
      }
    }
    total_within <- sum(within_ss)
  }

  est <- list(
    cluster_assignment = assignment,
    centers = centers,
    sizes = sizes,
    within_ss = within_ss,
    total_within_ss = total_within
  )
  .sc_quantity(
    name = "clusters",
    estimate = est,
    se = NA_real_,
    details = list(k = k, method = method, scale = scale,
                   nstart = as.integer(nstart), seed = seed,
                   n = N),
    call = match.call()
  )
}
