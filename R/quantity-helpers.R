## Internal helpers shared by the Tier A structural quantity functions.
##
## These bridge the abstract mathematical notation of spec.md §3.3 /
## §6A (e.g. "per-row B matrix", "resp cluster vector", "attribute
## block partition") to the concrete M3 `sc_fit` object returned by
## `scfit()`.  Everything in this file is internal.

#' Partition dummy columns into attribute blocks
#'
#' Returns a named list mapping each original attribute variable name
#' (as it appeared on the LHS of `|` in the `scfit()` formula) to the
#' integer column indices of `object$beta_hat` that belong to that
#' attribute.  Numeric attributes map to a single column; factor
#' attributes map to `(n_levels - 1)` columns (the reference level is
#' dropped in `.sc_encode()`).
#'
#' M3's `scfit()` stores this partition on `object$attr_map` directly.
#' For sc_fit objects produced by earlier builds (or by test helpers
#' that assemble an `sc_fit` manually) this helper falls back to
#' parsing `object$attr_names` against `object$attr_vars`.
#'
#' @param object An `sc_fit`.
#' @return A named list `attr_map` where each element is an integer
#'   vector of column indices into `object$beta_hat`, plus attribute
#'   `reference_levels` storing the reference level per factor
#'   attribute (NULL for numeric).
#' @keywords internal
#' @noRd
.sc_attr_map <- function(object) {
  if (!is.null(object$attr_map) && is.list(object$attr_map)) {
    map <- object$attr_map
  } else {
    ## Fallback: recover from attr_names + attr_vars by longest-prefix
    ## match (attr_vars sorted by length, longest first).
    av <- object$attr_vars
    if (is.null(av)) {
      stop(".sc_attr_map(): object has neither `attr_map` nor `attr_vars`.")
    }
    ord_av <- av[order(-nchar(av))]
    dn <- object$attr_names
    assigned <- integer(length(dn))
    map <- vector("list", length(av))
    names(map) <- av
    for (k in seq_along(dn)) {
      for (v in ord_av) {
        if (identical(dn[k], v) ||
            (nchar(dn[k]) > nchar(v) &&
             substr(dn[k], 1L, nchar(v)) == v)) {
          map[[v]] <- c(map[[v]], k)
          break
        }
      }
    }
  }
  ref_levels <- list()
  fl <- object$factor_levels
  if (!is.null(fl)) {
    for (v in names(fl)) {
      lv <- fl[[v]]
      if (!is.null(lv)) {
        ref_levels[[v]] <- lv[1L]
      }
    }
  }
  attr(map, "reference_levels") <- ref_levels
  map
}

#' Resolve a human-readable dummy reference to a column index
#'
#' Accepts three forms:
#'   1. `"attr:level"` — factor attribute, non-reference level;
#'   2. bare dummy name matching `object$attr_names` exactly
#'      (covers numeric attributes and `paste0(attr, level)` legacy);
#'   3. an integer column index (coerced and bounds-checked).
#'
#' Returns a length-1 integer column index into `object$beta_hat`.
#' @keywords internal
#' @noRd
.sc_parse_dummy_name <- function(object, x) {
  if (is.numeric(x) && length(x) == 1L) {
    idx <- as.integer(x)
    if (is.na(idx) || idx < 1L || idx > length(object$attr_names)) {
      stop(sprintf(".sc_parse_dummy_name(): index %s out of range.", x))
    }
    return(idx)
  }
  if (!is.character(x) || length(x) != 1L) {
    stop(".sc_parse_dummy_name(): `x` must be a length-1 character or integer.")
  }
  an <- object$attr_names
  hit <- which(an == x)
  if (length(hit) == 1L) return(hit)
  if (grepl(":", x, fixed = TRUE)) {
    parts <- strsplit(x, ":", fixed = TRUE)[[1L]]
    if (length(parts) != 2L) {
      stop(sprintf(".sc_parse_dummy_name(): '%s' must be 'attribute:level'.", x))
    }
    cand <- paste0(parts[1L], parts[2L])
    hit2 <- which(an == cand)
    if (length(hit2) == 1L) return(hit2)
  }
  stop(sprintf(".sc_parse_dummy_name(): cannot resolve '%s' against attr_names.", x))
}

#' Convert a human-readable profile list to a dummy vector
#'
#' Port of `07b_structural_quantities.R` lines 153-175.  Given a named
#' list like `list(gender = "female", talent = "hard_working")`, build
#' a length-`p` numeric dummy vector in the encoded space.  Attributes
#' not mentioned default to their reference level (all dummies for
#' that block = 0).  Unknown attribute names or unknown levels error.
#'
#' @param object An `sc_fit`.
#' @param profile A named list.
#' @return A length-`p` numeric vector (names = `object$attr_names`).
#' @keywords internal
#' @noRd
.sc_profile_to_dummies <- function(object, profile) {
  if (!is.list(profile) || is.null(names(profile))) {
    stop(".sc_profile_to_dummies(): `profile` must be a named list.")
  }
  p <- length(object$attr_names)
  out <- numeric(p)
  names(out) <- object$attr_names
  map <- .sc_attr_map(object)
  fl  <- object$factor_levels
  for (nm in names(profile)) {
    if (!nm %in% names(map)) {
      stop(sprintf(".sc_profile_to_dummies(): unknown attribute '%s'.", nm))
    }
    idx <- map[[nm]]
    val <- profile[[nm]]
    lv  <- if (!is.null(fl)) fl[[nm]] else NULL
    if (is.null(lv)) {
      ## Numeric attribute: write the scalar into its single column.
      if (!is.numeric(val) || length(val) != 1L) {
        stop(sprintf(".sc_profile_to_dummies(): '%s' is numeric; need length-1 numeric value.", nm))
      }
      out[idx] <- val
    } else {
      ## Factor attribute: val is a level name; write 1 in the matching
      ## non-reference dummy, leave others at 0.
      if (!is.character(val) || length(val) != 1L) {
        stop(sprintf(".sc_profile_to_dummies(): '%s' is a factor; need length-1 character level name.", nm))
      }
      if (!val %in% lv) {
        stop(sprintf(".sc_profile_to_dummies(): unknown level '%s' for attribute '%s'. Valid: %s",
                     val, nm, paste(lv, collapse = ", ")))
      }
      if (identical(val, lv[1L])) {
        ## reference level: all dummies 0 (already)
      } else {
        non_ref <- lv[-1L]
        pos <- which(non_ref == val)
        out[idx[pos]] <- 1
      }
    }
  }
  out
}

#' Respondent-clustered empirical SD of a per-row quantity
#'
#' Let `q_i` be a numeric per-row quantity for `i in S`.  Compute the
#' respondent-clustered SE of the sample mean `q_bar = mean(q_i)`:
#' \deqn{\hat{\mathrm{se}} = \sqrt{\frac{M_S}{M_S - 1} \cdot
#'   \frac{\sum_m c_m^2}{|S|^2}}}
#' where `c_m = sum_{i in S, resp_i == m}(q_i - q_bar)`.  Matches the
#' formula in spec §3.3 A1(c) and is shared by A2, A4, A6, A7, A8.
#'
#' @param q Numeric vector (already subsetted to S).
#' @param resp Integer/character cluster vector (same length as `q`).
#' @return Scalar numeric (NA if `M_S < 2`).
#' @keywords internal
#' @noRd
.sc_cluster_se <- function(q, resp) {
  n <- length(q)
  if (n == 0L) return(NA_real_)
  q_bar <- mean(q)
  dev <- q - q_bar
  resp_f <- as.factor(resp)
  M_S <- nlevels(resp_f)
  if (M_S < 2L) return(NA_real_)
  c_m <- as.numeric(tapply(dev, resp_f, sum))
  c_m[is.na(c_m)] <- 0
  sqrt((M_S / (M_S - 1)) * sum(c_m^2) / n^2)
}

#' Normal-approximation confidence bounds
#' @keywords internal
#' @noRd
.sc_ci_normal <- function(est, se, level = 0.95) {
  if (is.na(se)) return(c(NA_real_, NA_real_))
  q <- stats::qnorm(0.5 + level / 2)
  c(est - q * se, est + q * se)
}

#' Resolve a `subgroup` argument to a row index vector
#'
#' Accepts `NULL` (all rows), a logical vector of length `N`, an
#' integer index, or a character naming a Z column (factor-coerced,
#' first level = "out", others = "in") — simple enough for M4.
#' @keywords internal
#' @noRd
.sc_resolve_subgroup <- function(object, subgroup) {
  N <- nrow(object$beta_hat)
  if (is.null(subgroup)) return(seq_len(N))
  if (is.logical(subgroup)) {
    if (length(subgroup) != N) {
      stop(".sc_resolve_subgroup(): logical `subgroup` must have length N.")
    }
    return(which(subgroup))
  }
  if (is.numeric(subgroup)) {
    idx <- as.integer(subgroup)
    if (any(idx < 1L | idx > N)) {
      stop(".sc_resolve_subgroup(): numeric subgroup out of range.")
    }
    return(idx)
  }
  if (is.character(subgroup) && length(subgroup) == 1L) {
    zn <- object$z_names
    if (!subgroup %in% zn) {
      stop(sprintf(".sc_resolve_subgroup(): '%s' is not a Z column.", subgroup))
    }
    col <- object$Z[, subgroup]
    return(which(col > stats::median(col)))
  }
  stop(".sc_resolve_subgroup(): unsupported subgroup type.")
}
