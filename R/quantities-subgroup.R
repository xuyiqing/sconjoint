## Subgroup-conditional preference estimates.
##
## `sc_subgroup()` returns theta-hat and respondent-clustered SEs on
## one or more user-specified subgroups of respondents.  The point
## estimate is a direct re-average of the per-row beta_hat matrix over
## the rows selected by `subgroup`; the SE is the respondent-clustered
## empirical standard error of the per-row quantities inside that
## subset (same formula as `.sc_cluster_se()`).  No spec §6A table
## entry applies (Tier B); the reference implementation in
## `helper-reference-impl.R` (`.ref_subgroup()`) is derived directly
## from spec §3.3 B1.

#' Subgroup-conditional preference estimates
#'
#' Re-average `beta_hat(Z_i)` over a user-specified subset of
#' respondents and return point estimates with respondent-clustered
#' standard errors.  The subgroup argument accepts several forms:
#' \itemize{
#'   \item a logical vector of length `N = nrow(object$beta_hat)`
#'     (one flag per row of the DML output);
#'   \item an integer vector of row indices;
#'   \item a length-1 character naming a Z column (rows with
#'     `Z[, col] > median(col)` are selected -- same rule as the
#'     Tier A `subgroup = "z_col"` helper);
#'   \item a function of a single argument `Z` (the N-by-p_Z matrix
#'     on the `sc_fit`) that returns a logical vector of length `N`;
#'   \item a named list of any of the above, in which case the
#'     function is applied to each element and the return value is a
#'     named list of `sc_quantity` objects.
#' }
#'
#' @param object An `sc_fit` produced by `scfit()`.
#' @param subgroup Subgroup selector (see Details).
#' @param ... Unused; present for future extensions.
#' @return An `sc_quantity` with `estimate` a data.frame containing
#'   one row per dummy (`dummy_name`, `theta`, `se`, `ci_lo`, `ci_hi`),
#'   or a named list of `sc_quantity` objects if `subgroup` was a
#'   named list.
#' @examples
#' \donttest{
#' if (requireNamespace("torch", quietly = TRUE) &&
#'     torch::torch_is_installed()) {
#'   ## See vignette("sconjoint") for a full worked example.
#' }
#' }
#' @export
sc_subgroup <- function(object, subgroup, ...) {
  stopifnot(inherits(object, "sc_fit"))
  ## Named list of selectors -> recurse.
  if (is.list(subgroup) && !is.data.frame(subgroup) && !is.null(names(subgroup))) {
    out <- lapply(subgroup, function(sel) sc_subgroup(object, sel))
    return(out)
  }
  S <- .sc_resolve_subgroup_ext(object, subgroup)
  if (length(S) == 0L) {
    stop("sc_subgroup(): selected subgroup is empty.")
  }
  B <- object$beta_hat
  Bs <- B[S, , drop = FALSE]
  resp_s <- object$respondent_id[S]
  p <- ncol(B)
  theta <- colMeans(Bs)
  se <- numeric(p)
  for (j in seq_len(p)) {
    se[j] <- .sc_cluster_se(Bs[, j], resp_s)
  }
  ci_q <- stats::qnorm(0.975)
  df <- data.frame(
    dummy_name = object$attr_names,
    theta      = unname(theta),
    se         = se,
    ci_lo      = unname(theta) - ci_q * se,
    ci_hi      = unname(theta) + ci_q * se,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  .sc_quantity(
    name = "subgroup",
    estimate = df,
    se = NA_real_,
    details = list(subgroup_size = length(S),
                   n_respondents = length(unique(resp_s))),
    call = match.call()
  )
}

#' Resolve a subgroup argument with function and column-name support
#'
#' Extension of `.sc_resolve_subgroup()` that additionally accepts
#' functions of `Z`.  A function `f(Z)` must return a logical vector
#' of length `N = nrow(object$beta_hat)`.
#' @keywords internal
#' @noRd
.sc_resolve_subgroup_ext <- function(object, subgroup) {
  if (is.function(subgroup)) {
    N <- nrow(object$beta_hat)
    sel <- subgroup(object$Z)
    if (!is.logical(sel) || length(sel) != N) {
      stop(".sc_resolve_subgroup_ext(): function must return a logical vector of length N.")
    }
    return(which(sel))
  }
  .sc_resolve_subgroup(object, subgroup)
}
