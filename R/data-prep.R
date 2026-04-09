## Internal data-preparation helpers for sconjoint (M2).
##
## None of the functions in this file are exported: they are building
## blocks that `scfit()` will wire together in M3.  Each helper is kept
## small and pure (no global state, no side effects) so that it can be
## unit tested in isolation.
##
## The supported design is the two-profile forced-choice conjoint:
## one respondent answers several tasks, and each task contains
## exactly two profiles.  A single row in the user's data frame
## corresponds to one (respondent, task, profile) tuple.

#' Parse an `scfit()` formula
#'
#' Splits a two-sided formula of the form
#' `choice ~ a1 + a2 + ... | z1 + z2 + ...` into its three components:
#' the response, the attribute variables, and the respondent-level
#' moderators (`Z`).  A one-sided right-hand side without a `|`
#' separator is treated as "no `Z` variables".
#'
#' @param formula A formula object.
#' @return A list with `response` (character), `attr_vars` (character
#'   vector), and `z_vars` (character vector, possibly empty).
#' @keywords internal
#' @noRd
.sc_parse_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop(".sc_parse_formula(): `formula` must be a formula object.")
  }
  if (length(formula) != 3L) {
    stop(".sc_parse_formula(): formula must be two-sided, e.g. `y ~ x1 + x2 | z1`.")
  }

  response <- all.vars(formula[[2L]])
  if (length(response) != 1L) {
    stop(".sc_parse_formula(): left-hand side must name a single response variable.")
  }

  rhs <- formula[[3L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("|"))) {
    attr_expr <- rhs[[2L]]
    z_expr    <- rhs[[3L]]
    attr_vars <- all.vars(attr_expr)
    z_vars    <- all.vars(z_expr)
  } else {
    attr_vars <- all.vars(rhs)
    z_vars    <- character(0)
  }

  if (length(attr_vars) == 0L) {
    stop(".sc_parse_formula(): no attribute variables found on the right-hand side.")
  }

  list(response = response, attr_vars = attr_vars, z_vars = z_vars)
}

#' Verify the structure of a long-format conjoint data frame
#'
#' Checks that (a) the respondent, task, and profile columns exist,
#' (b) each (respondent, task) has exactly two profiles, and (c) the
#' profile identifiers inside a task are distinct.  Returns the input
#' invisibly and sorted by (respondent, task, profile) so that
#' downstream helpers can rely on a canonical row order.
#'
#' @param data A data frame.
#' @param respondent Name of the respondent-id column.
#' @param task Name of the task-id column.
#' @param profile Name of the within-task profile-id column.
#' @return The input data frame, row-sorted by (respondent, task, profile).
#' @keywords internal
#' @noRd
.sc_to_long <- function(data, respondent, task, profile) {
  if (!is.data.frame(data)) {
    stop(".sc_to_long(): `data` must be a data frame.")
  }
  for (nm in c(respondent, task, profile)) {
    if (!nm %in% names(data)) {
      stop(sprintf(".sc_to_long(): column '%s' not found in `data`.", nm))
    }
  }

  rid <- data[[respondent]]
  tid <- data[[task]]
  pid <- data[[profile]]

  ord <- order(rid, tid, pid)
  data <- data[ord, , drop = FALSE]
  rid <- data[[respondent]]
  tid <- data[[task]]
  pid <- data[[profile]]

  key <- paste(rid, tid, sep = "\r")
  tab <- table(key)
  if (!all(tab == 2L)) {
    stop(sprintf(
      ".sc_to_long(): every (respondent, task) pair must contain exactly 2 profiles; found counts %s.",
      paste(sort(unique(as.integer(tab))), collapse = ", ")
    ))
  }

  split_pid <- split(pid, key)
  if (any(vapply(split_pid, function(x) length(unique(x)) != 2L, logical(1L)))) {
    stop(".sc_to_long(): the two profile identifiers within a task must be distinct.")
  }

  attr(data, "sc_key") <- list(
    respondent = respondent,
    task       = task,
    profile    = profile
  )
  data
}

#' Dummy-encode conjoint attributes and respondent moderators
#'
#' Every attribute is coerced to a factor (if not already) and
#' expanded via treatment contrasts, dropping the first level of each
#' attribute as the reference category.  Numeric attributes are passed
#' through unchanged.  The same logic is applied to `z_vars`.
#'
#' Reference levels may be supplied via `ref_levels`, a named list
#' mapping variable name to the level that should be the reference.
#'
#' @param data A data frame, typically the output of `.sc_to_long()`.
#' @param attr_vars Character vector of attribute variable names.
#' @param z_vars Character vector of respondent-moderator names (may be empty).
#' @param ref_levels Optional named list of reference levels.
#' @return A list with
#'   * `X` — a numeric matrix with one row per profile and `p` columns
#'     of attribute dummies,
#'   * `Z` — a numeric matrix with one row per profile and `p_z`
#'     columns (0 columns if `z_vars` is empty),
#'   * `x_names`, `z_names` — column names of the two matrices.
#' @keywords internal
#' @noRd
.sc_encode <- function(data, attr_vars, z_vars = character(0), ref_levels = NULL) {
  if (!is.data.frame(data)) {
    stop(".sc_encode(): `data` must be a data frame.")
  }
  missing_vars <- setdiff(c(attr_vars, z_vars), names(data))
  if (length(missing_vars) > 0L) {
    stop(sprintf(".sc_encode(): columns not found: %s",
                 paste(missing_vars, collapse = ", ")))
  }

  encode_one <- function(vars) {
    if (length(vars) == 0L) {
      return(list(mat = matrix(0, nrow = nrow(data), ncol = 0L),
                  nms = character(0),
                  levels_map = list(),
                  attr_map = list()))
    }
    parts <- list()
    levels_map <- list()
    attr_map <- list()
    col_offset <- 0L
    for (v in vars) {
      col <- data[[v]]
      if (is.numeric(col)) {
        m <- matrix(as.numeric(col), ncol = 1L)
        colnames(m) <- v
        levels_map[[v]] <- NULL
        attr_map[[v]] <- col_offset + 1L
        col_offset <- col_offset + 1L
        parts[[length(parts) + 1L]] <- m
        next
      }
      col <- as.factor(col)
      if (!is.null(ref_levels) && !is.null(ref_levels[[v]])) {
        col <- stats::relevel(col, ref = ref_levels[[v]])
      }
      lev <- levels(col)
      if (length(lev) < 2L) {
        stop(sprintf(".sc_encode(): variable '%s' has fewer than 2 levels.", v))
      }
      m <- stats::model.matrix(~ col)[, -1L, drop = FALSE]
      colnames(m) <- paste0(v, lev[-1L])
      levels_map[[v]] <- lev
      n_dum <- ncol(m)
      attr_map[[v]] <- col_offset + seq_len(n_dum)
      col_offset <- col_offset + n_dum
      parts[[length(parts) + 1L]] <- m
    }
    mat <- do.call(cbind, parts)
    list(mat = mat, nms = colnames(mat),
         levels_map = levels_map, attr_map = attr_map)
  }

  X <- encode_one(attr_vars)
  Z <- encode_one(z_vars)

  list(
    X             = X$mat,
    Z             = Z$mat,
    x_names       = X$nms,
    z_names       = Z$nms,
    factor_levels = X$levels_map,
    attr_map      = X$attr_map
  )
}

#' Build the per-task Delta X matrix
#'
#' Given an encoded X matrix whose rows are ordered by
#' (respondent, task, profile) and contain exactly two profiles per
#' task, compute `Delta X = X[profile == first] - X[profile == second]`
#' for every task.  The returned matrix has one row per task.
#'
#' The corresponding Z matrix (respondent-level moderators) is
#' subsetted to the first profile of each task, since Z is constant
#' within a task.
#'
#' @param X Encoded attribute matrix (rows = profiles).
#' @param Z Encoded moderator matrix (rows = profiles, possibly 0 columns).
#' @param task_id Vector giving the task id of each row in `X`.
#' @param profile_id Vector giving the within-task profile id of each row.
#' @param respondent_id Vector giving the respondent id of each row.
#' @return A list with `deltaX`, `Z_task`, and `respondent_task` — all
#'   with one row (or element) per task.
#' @keywords internal
#' @noRd
.sc_build_deltax <- function(X, Z, task_id, profile_id, respondent_id) {
  n <- nrow(X)
  if (length(task_id) != n || length(profile_id) != n || length(respondent_id) != n) {
    stop(".sc_build_deltax(): `task_id`, `profile_id`, `respondent_id` must all match nrow(X).")
  }

  key <- paste(respondent_id, task_id, sep = "\r")
  ord <- order(key, profile_id)
  X   <- X[ord, , drop = FALSE]
  Z   <- Z[ord, , drop = FALSE]
  key <- key[ord]
  respondent_id <- respondent_id[ord]
  task_id       <- task_id[ord]

  if (length(key) %% 2L != 0L) {
    stop(".sc_build_deltax(): expected an even number of profile rows (2 per task).")
  }
  idx1 <- seq(1L, length(key), by = 2L)
  idx2 <- seq(2L, length(key), by = 2L)
  if (!all(key[idx1] == key[idx2])) {
    stop(".sc_build_deltax(): internal error, (respondent, task) keys misaligned.")
  }

  deltaX <- X[idx1, , drop = FALSE] - X[idx2, , drop = FALSE]
  Z_task <- Z[idx1, , drop = FALSE]

  list(
    deltaX          = deltaX,
    Z_task          = Z_task,
    respondent_task = respondent_id[idx1],
    task_id         = task_id[idx1]
  )
}
