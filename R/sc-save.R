# sc_save() / sc_load(): the compact saved fit (paper-v4 spec section 1.3).
#
# What the estimator reads, per fold: k, te, mu (all n), th, the held-out rows' output and common scores,
# and the weight parts (Li, Lt, Ic, B, Ic_red, shared, floored, null). Added for predictions, posteriors
# and provenance: the fold's network weights and preprocessing rule, its held-out log scores and
# validation respondents, the fold diagnostics and fit_sha (the package revision that trained the fit).
# The data are stored as packed tasks (y, d, task counts, ids, feature names, orientation) with the
# covariates Z, the grouping variables, the folds and the design name; the internal data object is
# rebuilt at load time. The score rule of the bounded updates is rebuilt from (q, points, seed).
# A checksum of the stored numbers is verified at load.

.sc_format <- "sconjoint compact fit"
.sc_format_version <- 1L

.sc_fold_core <- function(f) f[c("k", "te", "mu", "th", "output_score", "common_score", "parts")]

#' Save and load compact fold states
#'
#' `sc_save()` writes fold states (from [sc_states()]) in a compact versioned
#' format: what [sc_estimate()] needs for every fold, plus each fold's network
#' and covariate preprocessing (for predictions), held-out scores (for
#' [sc_select_rank()]), validation respondents, respondent ids, grouping
#' variables and the provenance of the fits. `sc_load()` reads it back and
#' checks it.
#'
#' @param object Fold states from [sc_states()] or [sc_load()].
#' @param file Path of the `.rds` file.
#' @param compress Compression passed to [saveRDS()] (default `"xz"`).
#' @return `sc_save()` returns the file path invisibly. `sc_load()` returns an
#'   object of class `sc_states`, or `NULL` with a message when `file` is
#'   empty or does not exist.
#' @examples
#' \donttest{
#' toy <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(toy)) {
#'   f <- tempfile(fileext = ".rds")
#'   sc_save(toy, f)
#'   identical(sc_estimate(sc_load(f), sc_mean("x1", "x1"))$estimate,
#'             sc_estimate(toy, sc_mean("x1", "x1"))$estimate)
#' }
#' }
#' @export
sc_save <- function(object, file, compress = "xz") {
  object <- .sc_states_check(object)
  x <- object$data; d <- x$data
  data <- list(y = d$y, d = unname(d$d), T = as.integer(d$T), ids = d$ids, feature_names = d$feature_names,
               orientation = d$metadata$orientation, Z = x$Z, groups = x$groups, fold = x$fold,
               validation = x$validation, design = x$design)
  fits <- object$fits$folds
  folds <- lapply(names(object$folds), function(k) {
    f <- object$folds[[k]]; core <- .sc_fold_core(f)
    core$mu <- unname(core$mu)
    ft <- if (!is.null(fits)) fits[[k]] else NULL
    c(core, list(diag = f$diag, fit_sha = f$fit_sha,
                 network = if (is.null(ft$network)) NULL else list(weights = ft$network$weights, biases = ft$network$biases),
                 preprocess = ft$preprocess, heldout = ft$heldout, validation = ft$validation))
  })
  names(folds) <- names(object$folds)
  out <- list(format = .sc_format, format_version = .sc_format_version,
              package_version = as.character(utils::packageVersion("sconjoint")), created = format(Sys.time()),
              q = object$q, score = object$score, info = object$info, data = data, folds = folds,
              settings = object$fits$settings,
              fit_sha = vapply(folds, function(f) if (is.null(f$fit_sha)) NA_character_ else as.character(f$fit_sha), ""),
              checksum = digest::digest(lapply(folds, .sc_fold_core), algo = "xxhash64"))
  saveRDS(out, file, compress = compress)
  invisible(file)
}

#' @rdname sc_save
#' @param check Verify the stored checksum (default `TRUE`).
#' @export
sc_load <- function(file, check = TRUE) {
  if (!length(file) || !nzchar(file) || !file.exists(file)) {
    message("sc_load(): no file at '", file, "'")
    return(NULL)
  }
  z <- readRDS(file)
  if (!is.list(z) || !identical(z$format, .sc_format)) stop("not a sconjoint compact fit: ", file, call. = FALSE)
  if (z$format_version > .sc_format_version)
    stop(sprintf("the file has format version %d; this sconjoint reads up to %d", z$format_version, .sc_format_version),
         call. = FALSE)
  if (check && !identical(digest::digest(lapply(z$folds, .sc_fold_core), algo = "xxhash64"), z$checksum))
    stop("the stored fold states do not match their checksum: ", file, call. = FALSE)
  dd <- z$data
  off <- c(0L, cumsum(dd$T))
  rows <- lapply(seq_along(dd$ids), function(i) seq.int(off[i] + 1L, off[i + 1L]))
  rd <- rum_data(lapply(rows, function(r) dd$y[r]), lapply(rows, function(r) dd$d[r, , drop = FALSE]),
                 ids = dd$ids, feature_names = dd$feature_names, metadata = list(orientation = dd$orientation))
  x <- sc_data(data = rd, Z = dd$Z, groups = dd$groups, fold = dd$fold, validation = dd$validation, design = dd$design)
  hidden <- z$settings$hidden
  folds <- lapply(z$folds, function(f) {
    dimnames(f$mu) <- list(dd$ids, dd$feature_names)
    f[c("k", "te", "mu", "th", "output_score", "common_score", "parts", "diag", "fit_sha")]
  })
  fits <- lapply(z$folds, function(f) {
    net <- if (is.null(f$network) || is.null(f$preprocess) || is.null(hidden)) NULL else
      .sc_network(f$network, length(f$preprocess$columns), length(dd$feature_names), hidden)
    list(k = f$k, network = net, preprocess = f$preprocess, heldout = f$heldout, validation = f$validation)
  })
  .sc_new_states(x, z$q, folds, z$score, z$info, fits = list(folds = fits, settings = z$settings))
}
