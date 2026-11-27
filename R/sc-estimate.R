# sc_estimate() and sc_fitted_law(): population quantities from cross-fitted fold states (paper-v4).

# Flatten a target specification into a list of sc_target objects.
.sc_flatten_targets <- function(targets) {
  if (inherits(targets, "sc_target")) return(list(targets))
  if (!is.list(targets)) stop("targets must be a target or a list of targets", call. = FALSE)
  out <- list()
  for (t in targets) out <- c(out, .sc_flatten_targets(t))
  out
}

# population = TRUE (sc_estimate(), sc_fitted_law()): the states must hold every respondent's held-out
# fold, each respondent in exactly one fold. States built with sc_states(respondents = ) are for checks.
.sc_states_check <- function(object, population = FALSE) {
  if (!inherits(object, "sc_states")) stop("expected fold states from sc_states() or sc_load()", call. = FALSE)
  if (!population) return(object)
  if (isTRUE(object$info$partial))
    stop("these fold states were built for the first held-out respondents only (sc_states(respondents = )): ",
         "they are for checks, not for population quantities. Build them without respondents.", call. = FALSE)
  n <- object$data$data$n
  cover <- tabulate(unlist(lapply(object$folds, `[[`, "te"), use.names = FALSE), nbins = n)
  if (any(cover != 1L))
    stop(sprintf(paste0("population quantities need every respondent held out in exactly one fold: %d of %d respondents ",
                        "are held out in no fold and %d in more than one. Fit and build states for all folds."),
                 sum(cover == 0L), n, sum(cover > 1L)), call. = FALSE)
  object
}
# The bounded updates' score rule (rebuilt from the stored settings; the paper's rule by default).
.sc_score_rule <- function(object) {
  QS <- rum_quadrature(n_fac = object$q, rule = "sobol", nodes = object$score$points, seed = object$score$seed)
  if (object$q == 1L) QS$u <- as.vector(QS$u)
  QS
}

#' Estimate population quantities from cross-fitted fold states
#'
#' Computes every target with the paper's estimator. For each fold, the
#' target is evaluated at that fold's fitted network for all respondents
#' (the plug-in) and corrected by a one-step term built from the held-out
#' respondents' scores and conditional information. Shares are then refined
#' by a bounded (targeted) update that keeps them in \[0, 1\]; importance
#' shares by one joint update that keeps them summing to one; ratios and
#' differences use the delta method. Standard errors are the standard
#' deviation of the influence values over respondents divided by the square
#' root of n.
#'
#' Intervals: Wald (estimate plus or minus z times SE) for means, AMEs, ratios
#' and differences; log-odds for probabilities and shares
#' (`plogis(qlogis(p) -/+ z SE / (p (1 - p)))`, missing when the estimate is 0
#' or 1); none for importance shares (point estimates only). A share whose
#' bounded update does not converge keeps its one-step estimate and SE, with
#' no interval; its `status` says why.
#'
#' @param object Fold states from [sc_states()] or [sc_load()], covering every
#'   respondent (all folds; not built with `respondents`).
#' @param targets A target or a list of targets (from [sc_mean()],
#'   [sc_share()], [sc_contest()], [sc_ame_switch()], [sc_ame_rows()],
#'   [sc_importance()], [sc_decisiveness()], [sc_ratio()], [sc_difference()]
#'   and the BR helpers).
#' @param bounded If `FALSE`, shares and importance shares are reported by
#'   their one-step estimates (faster).
#' @param z Critical value of the intervals (default 1.96).
#' @param joint_max_iter Iteration cap of the joint importance update.
#' @param cores Number of processes. With more than one, the targets (each
#'   element of `targets`: one target, or a set such as an importance set)
#'   are estimated in parallel forked processes and stacked, with the same
#'   results as one call; useful when there are many shares, whose bounded
#'   updates are slow. Forking is not available on Windows, where the targets
#'   run one after another.
#' @return A data frame with one row per target: `name`, `kind`, `estimate`,
#'   `plug_in` (the cross-fitted fitted value), `se`, `lower`, `upper`,
#'   `interval` (`"wald"`, `"log-odds"` or `"none"`), `n` (respondents with
#'   positive weight), `status` and `method`. The influence values are in the
#'   attribute `"influence"` (an n x targets matrix).
#' @examples
#' \donttest{
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) sc_estimate(fit, sc_mean("top rate", "tax6_linear"))
#' }
#' @export
sc_estimate <- function(object, targets, bounded = TRUE, z = 1.96, joint_max_iter = 50L, cores = 1L) {
  if (!is.numeric(cores) || length(cores) != 1L || !is.finite(cores) || cores < 1)
    stop("sc_estimate(): cores must be one whole number >= 1", call. = FALSE)
  cores <- as.integer(cores)
  if (cores > 1L) {
    tl <- .sc_flatten_targets(targets)
    if (length(tl) > 1L) return(.sc_estimate_parallel(object, tl, bounded, z, joint_max_iter, cores))
  }
  object <- .sc_states_check(object, population = TRUE)
  ctx <- .sc_context(object$data); data <- ctx$data; n <- data$n; FS <- object$folds
  tl <- .sc_flatten_targets(targets)
  # internal targets: one-step list (unique names), shares, joint sets, derived
  internal <- list(); rows <- list()
  add_internal <- function(tg) {
    if (!is.null(internal[[tg$name]])) stop("two targets are named '", tg$name, "'", call. = FALSE)
    internal[[tg$name]] <<- tg
  }
  build_one <- function(st) {
    b <- st$build(ctx)
    for (tg in b) add_internal(tg)
    vapply(b, `[[`, "", "name")
  }
  # A component of a ratio or difference reuses a target of the same name only when both define the
  # same mean contrast (same coefficients and weights); a different definition under that name stops.
  same_mean <- function(a, b) identical(a$kind, "mean") && identical(b$kind, "mean") &&
    identical(as.numeric(a$c), as.numeric(b$c)) && identical(a$w, b$w)
  for (st in tl) {
    if (identical(st$estimator, "derived")) {
      comp <- character(0)
      for (pt in st$parts) {
        nm <- pt$name
        if (is.null(internal[[nm]])) build_one(pt)
        else {
          b <- pt$build(ctx)
          if (length(b) != 1L || !same_mean(internal[[nm]], b[[1L]]))
            stop("'", st$name, "' uses a component named '", nm, "' that differs from another target of that name; ",
                 "give the two targets different names", call. = FALSE)
        }
        comp <- c(comp, nm)
      }
      rows[[length(rows) + 1L]] <- list(type = "derived", spec = st, names = st$name, parts = comp)
    } else {
      nm <- build_one(st)
      rows[[length(rows) + 1L]] <- list(type = st$estimator, spec = st, names = nm)
    }
  }
  joint_names <- unlist(lapply(rows, function(r) if (r$type == "joint" && bounded) r$names), use.names = FALSE)
  os_names <- setdiff(names(internal), joint_names)
  r1 <- if (length(os_names)) .sc_onestep(FS, data, unname(internal[os_names])) else NULL
  IFP <- if (is.null(r1)) matrix(0, n, 0) else r1$IFP
  se_of <- function(ifp) apply(ifp, 2L, stats::sd) / sqrt(n)
  out <- list(); infl <- list(); joints <- list()
  one_row <- function(name, kind, est, plug, se, nn, status, method, interval = .sc_interval_kind(kind)) {
    lo <- hi <- NA_real_
    if (interval == "wald" && is.finite(se)) { lo <- est - z * se; hi <- est + z * se }
    if (interval == "log-odds" && is.finite(se)) { ci <- .sc_logodds_ci(est, se, z); lo <- ci[1, 1]; hi <- ci[1, 2] }
    data.frame(name = name, kind = kind, estimate = est, plug_in = plug, se = se, lower = lo, upper = hi,
               interval = interval, n = as.integer(nn), status = status, method = method, stringsAsFactors = FALSE)
  }
  # shares: one bounded call
  share_names <- unlist(lapply(rows, function(r) if (r$type == "bounded" && bounded) r$names), use.names = FALSE)
  B <- NULL
  if (length(share_names)) {
    B <- .sc_bounded_path(FS, data, unname(internal[share_names]), .sc_score_rule(object))
    bIFP <- attr(B, "IFP")
  }
  for (r in rows) {
    if (r$type %in% c("onestep", "bounded") || (r$type == "joint" && !bounded)) {
      for (nm in r$names) {
        kind <- r1$kind[[nm]]; se <- se_of(r1$IFP[, nm, drop = FALSE])
        rw <- one_row(nm, kind, unname(r1$est[nm]), unname(r1$plug[nm]), unname(se), r1$n_group[[nm]],
                      "one-step", "held-out one-step")
        ifp <- r1$IFP[, nm]
        if (r$type == "bounded" && bounded) {
          h <- match(nm, B$target)
          rw$status <- B$status[h]
          if (identical(B$status[h], "converged")) {
            rw$estimate <- B$estimate[h]; rw$se <- B$se_population[h]
            ci <- .sc_logodds_ci(rw$estimate, rw$se, z); rw$lower <- ci[1, 1]; rw$upper <- ci[1, 2]
            rw$method <- "bounded update (held-out directions)"; ifp <- bIFP[, nm]
          } else { rw$lower <- rw$upper <- NA_real_ }
        }
        if (r$type == "joint") rw$interval <- "none"
        out[[length(out) + 1L]] <- rw; infl[[nm]] <- ifp
      }
    } else if (r$type == "joint") {
      J <- .sc_bounded_joint(FS, data, unname(internal[r$names]), .sc_score_rule(object), max_iter = joint_max_iter)
      for (nm in r$names) {
        h <- match(nm, J$table$target)
        rw <- one_row(nm, "importance", if (J$converged) J$table$estimate[h] else J$table$onestep[h],
                      J$table$plug_in[h], NA_real_, J$table$n_group[h], J$status,
                      if (J$converged) "joint simplex update (held-out directions)" else "one-step (joint update failed)",
                      interval = "none")
        out[[length(out) + 1L]] <- rw; infl[[nm]] <- J$IFP[, nm]
      }
      joints[[length(joints) + 1L]] <- J
    } else if (r$type == "derived") {
      st <- r$spec; ix <- r$parts
      zz <- vapply(ix, function(nm) unname(r1$est[nm]), 0)
      est <- st$fun(zz)
      influence <- drop(r1$IFP[, ix, drop = FALSE] %*% st$deriv(zz))
      s <- stats::sd(influence) / sqrt(n)
      wm <- .rum_target_weights(unname(internal[ix]), n); if (!is.matrix(wm)) wm <- matrix(wm, n)
      out[[length(out) + 1L]] <- one_row(st$name, "derived", est, NA_real_, s, sum(rowSums(wm > 0) > 0),
                                         "one-step", "delta method on one-step influence values", interval = "wald")
      infl[[st$name]] <- influence
    }
  }
  res <- do.call(rbind, out); rownames(res) <- NULL
  attr(res, "influence") <- do.call(cbind, infl)
  if (!is.null(B)) attr(res, "bounded") <- B
  if (length(joints)) attr(res, "joint") <- joints
  class(res) <- c("sc_estimates", "data.frame")
  res
}

# Each target (or target set) in its own forked process; rows, influence values and the update
# records stacked in the order of the targets.
.sc_estimate_parallel <- function(object, tl, bounded, z, joint_max_iter, cores) {
  run <- function(t) sc_estimate(object, list(t), bounded = bounded, z = z, joint_max_iter = joint_max_iter)
  parts <- if (.Platform$OS.type == "windows") lapply(tl, run) else
    parallel::mclapply(tl, function(t) tryCatch(run(t), error = function(e) e),
                       mc.cores = min(cores, length(tl)), mc.preschedule = FALSE)
  for (p in parts) {
    if (inherits(p, "error")) stop(conditionMessage(p), call. = FALSE)
    if (inherits(p, "try-error")) stop(as.character(p), call. = FALSE)
  }
  res <- do.call(rbind, lapply(parts, function(p) { y <- as.data.frame(p); attributes(y)[c("influence", "bounded", "joint")] <- NULL; y }))
  rownames(res) <- NULL
  if (anyDuplicated(res$name))
    stop("two targets are named '", res$name[duplicated(res$name)][1L], "'", call. = FALSE)
  attr(res, "influence") <- do.call(cbind, lapply(parts, attr, "influence"))
  B <- Filter(Negate(is.null), lapply(parts, attr, "bounded")); if (length(B)) attr(res, "bounded") <- B
  J <- unlist(lapply(parts, attr, "joint"), recursive = FALSE); if (length(J)) attr(res, "joint") <- J
  class(res) <- c("sc_estimates", "data.frame")
  res
}

#' Print an estimate table
#'
#' Shows each target's estimate, its fitted value before the correction
#' (`plug_in`), standard error, 95% interval and number of respondents,
#' rounded to `digits` decimal places, and lists targets whose update did not
#' converge. `all = TRUE` shows every column. The table itself keeps full
#' precision (use [as.data.frame()] for the numbers).
#'
#' @param x An estimate table from [sc_estimate()].
#' @param digits Decimal places.
#' @param all If `TRUE`, show every column.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.sc_estimates <- function(x, digits = 3, all = FALSE, ...) {
  y <- as.data.frame(x); attr(y, "influence") <- NULL; attr(y, "bounded") <- NULL; attr(y, "joint") <- NULL
  if (!isTRUE(all)) y <- y[, intersect(c("name", "estimate", "plug_in", "se", "lower", "upper", "n"), names(y)), drop = FALSE]
  num <- vapply(y, is.double, logical(1))
  y[num] <- lapply(y[num], round, digits)
  print(y, row.names = FALSE)
  odd <- !is.na(x$status) & !x$status %in% c("one-step", "converged")
  if (!isTRUE(all) && any(odd))
    cat(sprintf("\nNot converged (one-step estimate kept): %s\n",
                paste0(x$name[odd], " (", x$status[odd], ")", collapse = "; ")))
  invisible(x)
}

#' Fitted law of a taste contrast
#'
#' The model's distribution of a taste contrast `c' beta` across the
#' population. For each held-out respondent, the fold that did not train on
#' them gives a normal law with mean `c' mu(z_i)` and standard deviation
#' `||Lambda' c||`; the population law is the mixture of these normals (paper
#' eq. partworth_distribution).
#'
#' @param object Fold states from [sc_states()] or [sc_load()], covering every
#'   respondent (all folds; not built with `respondents`).
#' @inheritParams sc_mean
#' @return A data frame with one row per held-out respondent with positive
#'   weight (fold order): `id`, `fold`, `mean`, `sd` and `weight` (the
#'   respondent's weight, normalized to sum to one; equal without `w`). Its
#'   attribute `"summary"` gives the weighted mixture's mean, standard
#'   deviation and the share above zero.
#' @examples
#' \donttest{
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) head(sc_fitted_law(fit, "tax6_linear"))
#' }
#' @export
sc_fitted_law <- function(object, c, w = NULL) {
  object <- .sc_states_check(object, population = TRUE); ctx <- .sc_context(object$data)
  cc <- .sc_resolve_cvec(c, ctx); ww <- .sc_resolve_w(w, ctx)
  people <- if (is.null(ww)) NULL else which(ww > 0)
  out <- do.call(rbind, lapply(names(object$folds), function(k) {
    f <- object$folds[[k]]
    ii <- if (is.null(people)) f$te else intersect(f$te, people)
    if (!length(ii)) return(NULL)                     # no respondent of this fold has positive weight
    data.frame(id = ctx$data$ids[ii], fold = rep(as.integer(k), length(ii)), mean = drop(f$mu[ii, , drop = FALSE] %*% cc),
               sd = rep(sqrt(sum(drop(t(f$th$loading) %*% cc)^2)), length(ii)),
               weight = if (is.null(ww)) rep(1, length(ii)) else ww[ii], stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out$weight <- out$weight / sum(out$weight)
  attr(out, "summary") <- .sc_law_summary(out)
  class(out) <- c("sc_fitted_law", "data.frame")
  out
}

#' @export
print.sc_fitted_law <- function(x, ...) {
  if (!all(c("mean", "sd") %in% names(x))) { print(as.data.frame(x), ...); return(invisible(x)) }
  s <- .sc_law_summary(x)
  cat(sprintf("Fitted law of a taste contrast over %d held-out respondents%s: mean %.4f, sd %.4f, share above zero %.4f\n",
              nrow(x), if (.sc_law_equal_weights(x)) "" else " (weighted)", s[["mean"]], s[["sd"]], s[["share_positive"]]))
  invisible(x)
}
# Share of a normal law above zero; a degenerate law (sd 0) puts its mass at the mean.
.sc_share_positive <- function(mean, sd) ifelse(sd > 0, stats::pnorm(mean / sd), as.numeric(mean > 0))
.sc_law_equal_weights <- function(out) is.null(out$weight) || length(unique(out$weight)) <= 1L
# Mixture summaries with the respondents' weights (renormalized, so a subset of rows still works). With
# equal weights the plain means are used, as before weights were kept.
.sc_law_summary <- function(out) {
  if (.sc_law_equal_weights(out))
    return(c(mean = mean(out$mean), sd = sqrt(mean(out$sd^2) + mean((out$mean - mean(out$mean))^2)),
             share_positive = mean(.sc_share_positive(out$mean, out$sd))))
  w <- out$weight / sum(out$weight); m <- sum(w * out$mean)
  c(mean = m, sd = sqrt(sum(w * out$sd^2) + sum(w * (out$mean - m)^2)),
    share_positive = sum(w * .sc_share_positive(out$mean, out$sd)))
}
