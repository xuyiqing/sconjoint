# sc_data(): the public data object (paper-v4). It wraps the internal rum_data (one record per
# respondent: choices y and coded task matrices d) with what the paper's pipeline also needs: the
# respondent covariates Z that feed the network, grouping variables used for subgroup targets, the
# cross-fitting folds and the early-stopping (validation) respondents of every fold, and the name of the
# experiment's design law ("gs", "br") that the conditional information draws from.

#' Prepare conjoint data for the structural random-utility model
#'
#' Collects everything the estimation pipeline needs into one object: the
#' recorded choices and coded profile differences of every task, the
#' respondent characteristics that predict mean tastes, grouping variables for
#' subgroup quantities, the cross-fitting folds, and the design of the
#' experiment.
#'
#' Each task is a choice between two profiles. Row `t` of `X` is the coded
#' features of the first profile minus those of the second, and `y[t] = 1`
#' when the first profile was chosen. The tasks of one respondent share an
#' `id`; they are kept in the order given. Respondents are ordered by first
#' appearance in `id`.
#'
#' @param y Numeric 0/1 vector, one entry per task.
#' @param X Numeric matrix, one row per task and one column per coded
#'   feature (first profile minus second). Column names become feature names.
#' @param id Respondent identifier of every task.
#' @param Z Optional numeric matrix (or data frame) of respondent
#'   characteristics, one row per respondent. Rows are matched by row name when
#'   the row names are the respondent ids, otherwise taken in respondent order.
#' @param groups Optional data frame of respondent-level grouping variables
#'   (for example `pid7`, `party`, `ideology`, `PL`), one row per respondent in
#'   respondent order. Targets can name these columns as weights.
#' @param fold Optional integer vector of cross-fitting folds, one per
#'   respondent. [sc_fit()] draws folds when it is `NULL`.
#' @param validation Optional list, one element per fold, of the respondent ids
#'   used for early stopping when that fold is held out. Elements are matched
#'   to folds by name (the fold label) when named, otherwise by position over
#'   the sorted fold labels.
#' @param design Name of the experiment's randomization law: `"gs"` (Graham
#'   and Svolik 2020), `"br"` (Ballard-Rosa, Martin and Scheve 2017) or `NULL`.
#'   [sc_states()] needs it.
#' @param orientation One sentence saying what `y = 1` means.
#' @param data Alternatively, an internal `rum_data` object; `y`, `X` and `id`
#'   are then ignored.
#' @param mean_basis,metadata Passed to the internal data constructor
#'   (advanced use).
#' @return An object of class `sc_data`.
#' @examples
#' set.seed(1)
#' X <- matrix(sample(c(-1, 0, 1), 60, TRUE), 30, 2,
#'             dimnames = list(NULL, c("price", "quality")))
#' y <- rbinom(30, 1, 0.5)
#' x <- sc_data(y, X, id = rep(1:10, each = 3), Z = matrix(rnorm(10), 10, 1))
#' x
#' @export
sc_data <- function(y, X, id, Z = NULL, groups = NULL, fold = NULL, validation = NULL, design = NULL,
                    orientation = "y = 1 selects the first profile; X = first minus second profile",
                    data = NULL, mean_basis = NULL, metadata = list()) {
  if (is.null(data)) {
    X <- as.matrix(X)
    if (!is.numeric(X)) stop("X must be numeric", call. = FALSE)
    if (length(y) != nrow(X) || length(id) != nrow(X)) stop("y, X and id must have one entry per task", call. = FALSE)
    id <- as.character(id); ids <- unique(id)
    f <- factor(id, levels = ids); rows <- split(seq_along(id), f)
    if (is.null(metadata$orientation)) metadata$orientation <- orientation
    data <- rum_data(lapply(rows, function(r) as.numeric(y[r])),
                     lapply(rows, function(r) X[r, , drop = FALSE]), mean_basis = mean_basis,
                     ids = ids, feature_names = colnames(X), metadata = metadata)
  } else if (!inherits(data, "rum_data")) stop("data must be a rum_data object", call. = FALSE)
  n <- data$n
  if (!is.null(Z)) {
    Z <- as.matrix(Z)
    if (!is.numeric(Z)) stop("Z must be numeric", call. = FALSE)
    if (!is.null(rownames(Z)) && all(data$ids %in% rownames(Z))) Z <- Z[data$ids, , drop = FALSE]
    else if (nrow(Z) == n) rownames(Z) <- data$ids
    else stop("Z must have one row per respondent", call. = FALSE)
    if (is.null(colnames(Z))) colnames(Z) <- paste0("z", seq_len(ncol(Z)))
  }
  if (!is.null(groups)) {
    groups <- as.data.frame(groups, stringsAsFactors = FALSE)
    if (nrow(groups) != n) stop("groups must have one row per respondent", call. = FALSE)
    rownames(groups) <- NULL
  }
  if (!is.null(fold)) {
    fold <- as.integer(fold)
    if (length(fold) != n || anyNA(fold) || any(fold < 1L)) stop("fold must give a positive fold for every respondent", call. = FALSE)
  }
  if (!is.null(validation)) {
    if (!is.list(validation)) stop("validation must be a list of respondent ids, one element per fold", call. = FALSE)
    validation <- lapply(validation, as.character)
  }
  if (!is.null(design)) {
    design <- match.arg(design, c("gs", "br"))
    .sc_check_design_features(data$feature_names, design)
  }
  structure(list(data = data, Z = Z, groups = groups, fold = fold, validation = validation,
                 design = design), class = "sc_data")
}

#' @export
print.sc_data <- function(x, ...) {
  d <- x$data
  cat(sprintf("sconjoint data: %d respondents, %d tasks, %d coded features\n", d$n, length(d$y), d$p))
  cat("  features:", paste(utils::head(d$feature_names, 6L), collapse = ", "), if (d$p > 6L) ", ..." else "", "\n")
  cat("  covariates:", if (is.null(x$Z)) "none" else sprintf("%d columns", ncol(x$Z)), "\n")
  if (!is.null(x$groups)) cat("  groups:", paste(names(x$groups), collapse = ", "), "\n")
  if (!is.null(x$fold)) cat("  folds:", paste(sprintf("%d (%d)", sort(unique(x$fold)), tabulate(x$fold)[sort(unique(x$fold))]), collapse = ", "), "\n")
  cat("  design:", if (is.null(x$design)) "not given" else x$design, "\n")
  invisible(x)
}

# A built-in design law codes its features in a fixed order (.rum_design_spec(design)$features). The
# information and the design-generated targets use that order, so the data must have the same features
# in the same order; they are not reordered.
.sc_check_design_features <- function(feature_names, design) {
  want <- .rum_design_spec(design)$features
  if (identical(as.character(feature_names), want)) return(invisible(TRUE))
  detail <- if (length(feature_names) != length(want)) sprintf("the data have %d features, the design %d", length(feature_names), length(want))
    else if (setequal(feature_names, want)) "the data have the same features in another order"
    else paste0("features not in the design: ", paste(utils::head(setdiff(feature_names, want), 5L), collapse = ", "))
  stop(sprintf("the '%s' design codes its %d features in a fixed order (%s, ...), and the data must match it: %s. ",
               design, length(want), paste(utils::head(want, 3L), collapse = ", "), detail),
       "Reorder the columns of X to the design's order (.rum_design_spec(design)$features) or drop design.", call. = FALSE)
}

.sc_check_data <- function(x) {
  if (!inherits(x, "sc_data")) stop("expected an sc_data object (see sc_data())", call. = FALSE)
  x
}
