# sc_posterior() methods for fitted objects. Each respondent's posterior is computed at the fit of the
# fold in which that respondent was held out, so no respondent's own choices shaped the parameters the
# posterior conditions on (the cross-fitting rule the paper uses for every respondent-level output).

#' @rdname sc_posterior
#' @details For fold states ([sc_states()], [sc_load()]) and cross-fitted fits
#'   ([sc_fit()]), `data` defaults to the data the object was fitted on, and each
#'   respondent is summarized at the fit of the fold that held that respondent
#'   out. The result gains a `fold` column in `diagnostics`.
#' @export
sc_posterior.sc_states <- function(object, data = NULL, respondents = NULL, contrasts = NULL,
                                   newtasks = NULL, history = NULL,
                                   method = c("adaptive", "quadrature"), nodes = 4096,
                                   seed = 20261114, ...) {
  parts <- lapply(object$folds, function(f)
    list(k = f$k, te = f$te, mu = f$mu, kappa = f$th$kappa, loading = f$th$loading, omega = f$th$omega))
  .sc_posterior_crossfit(parts, object$data$data, data, respondents = respondents,
    contrasts = contrasts, newtasks = newtasks, history = history, method = method,
    nodes = nodes, seed = seed, ...)
}

#' @rdname sc_posterior
#' @export
sc_posterior.sc_fit <- function(object, data = NULL, respondents = NULL, contrasts = NULL,
                                newtasks = NULL, history = NULL,
                                method = c("adaptive", "quadrature"), nodes = 4096,
                                seed = 20261114, ...) {
  parts <- lapply(object$folds, function(f)
    list(k = f$k, te = f$te, mu = f$mu_all, kappa = f$kappa, loading = f$loading, omega = f$omega))
  .sc_posterior_crossfit(parts, object$data$data, data, respondents = respondents,
    contrasts = contrasts, newtasks = newtasks, history = history, method = method,
    nodes = nodes, seed = seed, ...)
}

.sc_posterior_crossfit <- function(parts, fitted_data, data, respondents, newtasks, ...) {
  if (is.null(data)) data <- fitted_data
  if (!identical(data$ids, fitted_data$ids))
    stop("data must have the same respondents, in the same order, as the fitted object.")
  sel <- .sc_post_respondents(data, respondents)
  fold_of <- rep(NA_integer_, data$n)
  for (j in seq_along(parts)) fold_of[parts[[j]]$te] <- j
  if (anyNA(fold_of[sel])) stop("Some selected respondents are held out in no fold.")
  if (is.list(newtasks) && !is.data.frame(newtasks) && is.null(names(newtasks))) {
    if (length(newtasks) != length(sel))
      stop("A newtasks list needs one matrix per selected respondent (named by ID or in order).")
    names(newtasks) <- data$ids[sel]
  }
  runs <- list()
  for (j in sort(unique(fold_of[sel]))) {
    s <- sel[fold_of[sel] == j]
    nt <- if (is.list(newtasks) && !is.data.frame(newtasks)) newtasks[data$ids[s]] else newtasks
    pr <- parts[[j]]
    r <- .sc_posterior_run(pr$mu, pr$kappa, pr$loading, pr$omega, data, respondents = s,
                           newtasks = nt, ...)
    r$diagnostics$fold <- pr$k
    runs[[length(runs) + 1L]] <- r
  }
  ord <- data$ids[sel]
  bind <- function(name) {
    x <- lapply(runs, `[[`, name)
    x <- x[!vapply(x, is.null, logical(1))]
    if (!length(x)) return(NULL)
    r <- do.call(rbind, x)
    r <- r[order(match(r$id, ord), seq_len(nrow(r))), , drop = FALSE]
    rownames(r) <- NULL
    r
  }
  out <- runs[[1L]]
  for (nm in c("beta", "contrasts", "position", "predictions", "diagnostics")) out[[nm]] <- bind(nm)
  out$settings$crossfit <- TRUE
  out
}
