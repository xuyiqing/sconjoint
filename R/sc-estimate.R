# sc_estimate() and sc_fitted_law(): population quantities from cross-fitted fold states (paper-v4).

# Flatten a target specification into a list of sc_target objects.
.sc_flatten_targets <- function(targets) {
  if (inherits(targets, "sc_target")) return(list(targets))
  if (!is.list(targets)) stop("targets must be a target or a list of targets", call. = FALSE)
  out <- list()
  for (t in targets) out <- c(out, .sc_flatten_targets(t))
  out
}

.sc_states_check <- function(object) {
  if (!inherits(object, "sc_states")) stop("expected fold states from sc_states() or sc_load()", call. = FALSE)
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
#' @param object Fold states from [sc_states()] or [sc_load()].
#' @param targets A target or a list of targets (from [sc_mean()],
#'   [sc_share()], [sc_contest()], [sc_ame_switch()], [sc_ame_rows()],
#'   [sc_importance()], [sc_decisiveness()], [sc_ratio()], [sc_difference()]
#'   and the BR helpers).
#' @param bounded If `FALSE`, shares and importance shares are reported by
#'   their one-step estimates (faster).
#' @param z Critical value of the intervals (default 1.96).
#' @param joint_max_iter Iteration cap of the joint importance update.
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
sc_estimate <- function(object, targets, bounded = TRUE, z = 1.96, joint_max_iter = 50L) {
  object <- .sc_states_check(object)
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
  for (st in tl) {
    if (identical(st$estimator, "derived")) {
      comp <- character(0)
      for (pt in st$parts) {
        nm <- pt$name
        if (is.null(internal[[nm]])) build_one(pt)
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

#' @export
print.sc_estimates <- function(x, digits = 4, ...) {
  y <- as.data.frame(x); attr(y, "influence") <- NULL; attr(y, "bounded") <- NULL; attr(y, "joint") <- NULL
  y$method <- NULL
  print(y, digits = digits, row.names = FALSE)
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
#' @param object Fold states from [sc_states()] or [sc_load()].
#' @inheritParams sc_mean
#' @return A data frame with one row per held-out respondent (fold order):
#'   `id`, `fold`, `mean`, `sd`. Its attribute `"summary"` gives the mixture's
#'   mean, standard deviation and the share above zero.
#' @examples
#' \donttest{
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) head(sc_fitted_law(fit, "tax6_linear"))
#' }
#' @export
sc_fitted_law <- function(object, c, w = NULL) {
  object <- .sc_states_check(object); ctx <- .sc_context(object$data)
  cc <- .sc_resolve_cvec(c, ctx); ww <- .sc_resolve_w(w, ctx)
  people <- if (is.null(ww)) NULL else which(ww > 0)
  out <- do.call(rbind, lapply(names(object$folds), function(k) {
    f <- object$folds[[k]]
    ii <- if (is.null(people)) f$te else intersect(f$te, people)
    data.frame(id = ctx$data$ids[ii], fold = as.integer(k), mean = drop(f$mu[ii, , drop = FALSE] %*% cc),
               sd = rep(sqrt(sum(drop(t(f$th$loading) %*% cc)^2)), length(ii)), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  attr(out, "summary") <- .sc_law_summary(out)
  class(out) <- c("sc_fitted_law", "data.frame")
  out
}

#' @export
print.sc_fitted_law <- function(x, ...) {
  if (!all(c("mean", "sd") %in% names(x))) { print(as.data.frame(x), ...); return(invisible(x)) }
  s <- .sc_law_summary(x)
  cat(sprintf("Fitted law of a taste contrast over %d held-out respondents: mean %.4f, sd %.4f, share above zero %.4f\n",
              nrow(x), s[["mean"]], s[["sd"]], s[["share_positive"]]))
  invisible(x)
}
.sc_law_summary <- function(out)
  c(mean = mean(out$mean), sd = sqrt(mean(out$sd^2) + mean((out$mean - mean(out$mean))^2)),
    share_positive = mean(stats::pnorm(out$mean / out$sd)))
