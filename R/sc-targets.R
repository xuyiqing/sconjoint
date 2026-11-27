# Public target constructors (paper-v4). A target is a recipe: it records what to estimate and builds the
# internal target (a list with name, kind, w, m(mu, th) and grad(mu, th); rum-inference-targets.R) when
# sc_estimate() knows the data. So contrasts can name features and weights can name grouping variables.
#
# Object: class "sc_target", list(name, kind, estimator, build = function(ctx) -> list of internal
# targets). estimator: "onestep" (held-out one-step), "bounded" (shares: bounded update), "joint"
# (importance sets: joint simplex update), "derived" (ratios and differences of one-step targets: delta
# method; field parts = the component targets, fun, deriv).
# ctx (.sc_context): data (rum_data), groups, design, n, p, feature_names.

.sc_context <- function(x) {
  x <- .sc_check_data(x)
  list(data = x$data, groups = x$groups, design = x$design, n = x$data$n, p = x$data$p,
       feature_names = x$data$feature_names)
}
.sc_target <- function(name, kind, estimator, build, ...)
  structure(list(name = name, kind = kind, estimator = estimator, build = build, ...), class = "sc_target")

#' @export
print.sc_target <- function(x, ...) {
  cat(sprintf("sconjoint target: %s (%s; %s)\n", paste(x$name, collapse = ", "), x$kind,
              switch(x$estimator, onestep = "one-step", bounded = "bounded update", joint = "joint simplex update",
                     derived = "delta method", x$estimator)))
  invisible(x)
}

# Respondent weights: NULL (everyone), a numeric or logical vector (one per respondent), the name of a
# 0/1 or logical column of the data's groups, or a function(groups) returning one of these.
.sc_resolve_w <- function(w, ctx) {
  if (is.null(w)) return(NULL)
  if (is.function(w)) {
    if (is.null(ctx$groups)) stop("a weight function needs groups in the data", call. = FALSE)
    w <- w(ctx$groups)
  } else if (is.character(w) && length(w) == 1L) {
    if (is.null(ctx$groups) || !w %in% names(ctx$groups)) stop("no grouping variable named '", w, "'", call. = FALSE)
    w <- ctx$groups[[w]]
  }
  if (length(w) != ctx$n) stop("weights must have one entry per respondent", call. = FALSE)
  w <- as.numeric(w)
  if (anyNA(w) || any(w < 0) || !any(w > 0)) stop("weights must be nonnegative, nonmissing and not all zero", call. = FALSE)
  w
}
# Contrast coefficients: feature names (the sum of their unit vectors), or a numeric vector with one
# entry per feature (a vector named by features is matched by name; missing features get 0).
.sc_resolve_cvec <- function(c, ctx) {
  p <- ctx$p; fn <- ctx$feature_names
  if (is.character(c)) {
    j <- match(c, fn)
    if (anyNA(j)) stop("unknown feature(s): ", paste(c[is.na(j)], collapse = ", "), call. = FALSE)
    out <- numeric(p); for (k in j) out <- out + as.numeric(seq_len(p) == k)
    return(out)
  }
  if (!is.numeric(c)) stop("a contrast must be numeric or feature names", call. = FALSE)
  nm <- names(c)
  if (!is.null(nm) && all(nzchar(nm)) && !identical(nm, fn)) {
    j <- match(nm, fn)
    if (anyNA(j)) stop("unknown feature(s): ", paste(nm[is.na(j)], collapse = ", "), call. = FALSE)
    out <- numeric(p); out[j] <- as.numeric(c); return(out)
  }
  if (length(c) != p) stop(sprintf("a contrast needs %d coefficients (one per feature)", p), call. = FALSE)
  unname(as.numeric(c))
}
# Column names of a contest matrix, when given, must be the data's features in order (no reordering).
.sc_check_columns <- function(cn, ctx, what) {
  if (is.null(cn) || identical(cn, ctx$feature_names)) return(invisible(TRUE))
  stop(sprintf("the columns of %s are named, but not by the data's features in order (%s, ...)", what,
               paste(utils::head(ctx$feature_names, 3L), collapse = ", ")), call. = FALSE)
}
# Feature columns of a group: indices or names.
.sc_resolve_cols <- function(g, ctx) {
  if (is.character(g)) {
    j <- match(g, ctx$feature_names)
    if (anyNA(j)) stop("unknown feature(s): ", paste(g[is.na(j)], collapse = ", "), call. = FALSE)
    return(j)
  }
  as.integer(g)
}

#' Mean taste contrast
#'
#' The population average of a contrast `c' beta` of tastes (paper eq.
#' contrast_estimand): how much more, on average, respondents value one
#' feature (or combination) than another, in logit units. With `w`, the
#' average over a subgroup.
#'
#' @param name Label of the target.
#' @param c The contrast: feature name(s) (their unit vectors are added) or a
#'   numeric vector with one coefficient per coded feature (named vectors are
#'   matched by name).
#' @param w Optional respondent weights: the name of a 0/1 grouping variable
#'   of the data, a vector with one weight per respondent, or a function of the
#'   data's `groups` returning such a vector.
#' @return A target for [sc_estimate()].
#' @examples
#' sc_mean("co-partisan", "diff_respParty", w = "PL")
#' @export
sc_mean <- function(name, c, w = NULL) {
  force(c); force(w)
  .sc_target(name, "mean", "onestep", function(ctx) list(.rum_tgt_mean(name, .sc_resolve_cvec(c, ctx), .sc_resolve_w(w, ctx))))
}

#' Share of respondents who prefer one side of a contrast
#'
#' The fraction of the population whose taste contrast `c' beta` is positive
#' (paper eq. sign_share), averaged over each respondent's fitted taste law.
#' Estimated with the bounded (targeted) update, so it stays in \[0, 1\], with
#' a log-odds interval. With `c` the sum of a violation and the co-partisan
#' feature this is the compensating share of the paper.
#'
#' @inheritParams sc_mean
#' @return A target for [sc_estimate()].
#' @examples
#' sc_share("prefers co-partisan", "diff_respParty", w = "PL")
#' @export
sc_share <- function(name, c, w = NULL) {
  force(c); force(w)
  .sc_target(name, "share", "bounded", function(ctx) {
    cc <- .sc_resolve_cvec(c, ctx)
    if (!all(is.finite(cc))) stop("the contrast of '", name, "' must be finite", call. = FALSE)
    if (all(cc == 0)) stop("the contrast of '", name, "' is zero: its share above zero is 0 for everyone, ",
                           "so there is nothing to estimate", call. = FALSE)
    list(.rum_tgt_share(name, cc, .sc_resolve_w(w, ctx)))
  })
}

#' Expected support in a hypothetical contest
#'
#' The expected support for profile A against profile B: the population
#' average of each respondent's probability of choosing A, with position
#' effects set to zero (paper eq. substantive_choice_estimand). It is not the
#' probability that A wins a finite election. In symbols,
#' `E[G(d' beta)]` with `d` the coded difference A minus B and `G` the
#' logistic function. Several rows with weights `coef` average over contests
#' (for example over the revenue categories a plan can display).
#'
#' @inheritParams sc_mean
#' @param D The contest(s): a numeric vector (one contest) or a matrix with one
#'   row per contest, columns = coded features; or feature names for one contest.
#' @param coef Weights of the rows: finite, nonnegative and summing to one
#'   (default equal).
#' @param order Nodes of the Gauss-Hermite rule in each of the two dimensions
#'   (default 41, the paper's rule).
#' @return A target for [sc_estimate()].
#' @examples
#' sc_contest("co-partisan vs other", "diff_respParty", w = "PL")
#' @export
sc_contest <- function(name, D, coef = NULL, w = NULL, order = 41L) {
  force(D); force(coef); force(w); force(order)
  .sc_target(name, "choice", "onestep", function(ctx) {
    DD <- if (is.matrix(D)) {
      if (ncol(D) != ctx$p) stop(sprintf("contest rows need %d columns", ctx$p), call. = FALSE)
      .sc_check_columns(colnames(D), ctx, "contest")
      unname(D)
    } else rbind(.sc_resolve_cvec(D, ctx))
    cf <- if (is.null(coef)) rep(1 / nrow(DD), nrow(DD)) else as.numeric(coef)
    if (length(cf) != nrow(DD)) stop("coef needs one weight per contest row", call. = FALSE)
    # a probability: a mixture of contests (finite nonnegative weights summing to one)
    if (!all(is.finite(cf)) || any(cf < 0) || abs(sum(cf) - 1) > 1e-8)
      stop("coef must be finite nonnegative weights summing to one (a mixture of contests); sum is ",
           format(sum(cf), digits = 6), call. = FALSE)
    if (!all(is.finite(DD))) stop("contest rows must be finite numbers", call. = FALSE)
    quad <- if (order == 41L) .rum_law_quadrature() else rum_quadrature(c(order, order))
    list(.rum_tgt_choice_set_pref(name, DD, cf, w = .sc_resolve_w(w, ctx), quad = quad))
  })
}

#' Average marginal effect of switching one attribute
#'
#' The change in the probability that a profile is chosen when one attribute
#' of that profile switches from a reference level to another level, with the
#' rest of the contest held fixed (paper eq. standardized_ame). Contexts are
#' draws from the experiment's design law, both screen positions, with
#' position effects kept. Needs a design: one from [sc_design()] or a built-in
#' design (`"gs"` or `"br"`).
#'
#' @inheritParams sc_mean
#' @param attr The design attribute: for an [sc_design()], one of its
#'   attributes; for `"gs"`: `"party"`, `"p1"`, `"p2"`, `"dem"`, `"female"`,
#'   `"race"`, `"pro"`; for `"br"`: `"r1"` ... `"r6"`, `"taxrev"`.
#' @param level,ref The level switched to and the reference level: for an
#'   [sc_design()], level names (or their positions); otherwise raw values
#'   (level indices), GS democracy codes by name, or functions of the
#'   respondent's design key (GS: `"D"`, `"R"`, `"I"`) returning a raw value or
#'   `NA` (switch undefined: weight 0).
#' @param design Design law; default the data's.
#' @param draws,seed Number of drawn respondents' task sets used as contexts
#'   and their seed (common random numbers: the same contexts for every fold).
#' @param scale Multiplies the effect (for example `1/3` for a per-unit change
#'   over a three-unit switch).
#' @param position Keep position effects (default `TRUE`).
#' @param context `"switch"` (default) or `"conditional"`.
#' @param order Nodes of the Gauss-Hermite rule (default 21, the paper's).
#' @return A target for [sc_estimate()].
#' @examples
#' sc_ame_switch("economic policy", "p1", level = 4, ref = 1, design = "gs", scale = 1/3)
#' @export
sc_ame_switch <- function(name, attr, level, ref, design = NULL, draws = 40L, seed = 20261012L, w = NULL,
                          scale = 1, position = TRUE, context = c("switch", "conditional"), order = 21L) {
  context <- match.arg(context)
  force(attr); force(level); force(ref); force(design); force(draws); force(seed); force(w); force(scale)
  force(position); force(order)
  .sc_target(name, "ame", "onestep", function(ctx) {
    des <- if (is.null(design)) ctx$design else design
    if (is.null(des)) stop("sc_ame_switch() needs a design law: give design (sc_design(), 'gs' or 'br'), or set it in sc_data()", call. = FALSE)
    des <- .sc_design_arg(des)
    spec <- .rum_spec_arg(des)
    .sc_check_design_features(ctx$feature_names, des)
    keys <- NULL
    if (spec$uses_resp) {
      if (is.null(ctx$groups$pid7)) stop("the GS design codes the co-partisan feature by party: the data's groups need pid7", call. = FALSE)
      keys <- .rum_gs_key(ctx$groups$pid7)
    }
    lv <- level; rf <- ref
    if (inherits(des, "sc_design")) {
      if (!attr %in% names(des$attributes))
        stop(sprintf("'%s' is not an attribute of the design (%s)", attr, paste(names(des$attributes), collapse = ", ")), call. = FALSE)
      lv <- .sc_design_level(des, attr, lv); rf <- .sc_design_level(des, attr, rf)
    }
    if (identical(des, "gs") && identical(attr, "dem")) {
      if (is.character(lv)) lv <- match(sub("^diff_dem_code_", "", lv), .RUM_GS_DEM)
      if (is.character(rf)) rf <- match(sub("^diff_dem_code_", "", rf), .RUM_GS_DEM)
    }
    raw <- .rum_design_draw_raw(des, draws, seed = seed)
    list(.rum_tgt_switch_ame(name, des, raw, attr, lv, rf, keys = keys, w = .sc_resolve_w(w, ctx),
                             position = position, context = context, scale = scale, order = order))
  })
}

# A level of an sc_design attribute, by name or position, as its index.
.sc_design_level <- function(des, attr, x) {
  lv <- des$attributes[[attr]]
  j <- if (is.character(x)) match(x, lv) else if (is.numeric(x) && length(x) == 1L && x %in% seq_along(lv)) as.integer(x) else NA_integer_
  if (length(j) != 1L || is.na(j))
    stop(sprintf("'%s' is not a level of '%s' (%s)", paste(x, collapse = ", "), attr, paste(lv, collapse = ", ")), call. = FALSE)
  j
}

# Internal target over contest rows evaluated in chunks of rows (sums of the chunks' values and gradients
# in order; br_ame_gradient of the rebuild). Rows: D (R x p), coef, side.
.sc_tgt_rows_chunked <- function(name, rows, w, position, order, chunk, kind = "ame") {
  force(rows); force(w); force(position); force(order); force(chunk)
  if (is.null(chunk) || nrow(rows$D) <= chunk)
    return(.rum_tgt_choice_rows(name, rows[c("D", "coef", "side")], w = w, position = position, order = order, kind = kind))
  chunks <- split(seq_len(nrow(rows$D)), ceiling(seq_len(nrow(rows$D)) / chunk))
  cache <- new.env(parent = emptyenv())
  vg <- function(mu, th) {
    if (!is.null(cache$key) && identical(cache$key, list(mu, th))) return(cache$val)
    n <- nrow(mu); p <- ncol(mu); q <- if (is.matrix(th$loading)) ncol(th$loading) else 1L
    res <- list(m = numeric(n), gm = matrix(0, n, p), gt = matrix(0, n, p * q + 2L))
    for (ii in chunks) {
      block <- list(D = rows$D[ii, , drop = FALSE], coef = rows$coef[ii], side = rows$side[ii])
      z <- .rum_tgt_choice_rows("block", block, position = position, order = order)$grad(mu, th)
      res$m <- res$m + z$m; res$gm <- res$gm + z$gm; res$gt <- res$gt + z$gt
    }
    cache$key <- list(mu, th); cache$val <- res
    res
  }
  list(name = name, kind = kind, w = w, m = function(mu, th) vg(mu, th)$m, grad = vg)
}

#' Average marginal effect over given contest rows
#'
#' A choice-probability target over explicit contest rows: for row `r` the
#' coded difference `D[r, ]` between a focal profile and its opponent, the
#' focal profile's screen side (`+1` first, `-1` second) and a coefficient.
#' The target is `sum_r coef[r] P(focal profile r is chosen)`, averaged over
#' respondents. With coefficients `+1/R` at one level and `-1/R` at the
#' reference this is an average marginal effect over a reference set of
#' contexts.
#'
#' @inheritParams sc_mean
#' @param D Matrix of coded differences (focal minus opponent), one row per contest.
#' @param coef Coefficient of each row.
#' @param side Screen side of the focal profile in each row (`+1` or `-1`).
#' @param position Keep position effects (default `TRUE`).
#' @param order Nodes of the Gauss-Hermite rule (default 31).
#' @param chunk Evaluate the rows in blocks of this many (memory; `NULL`: all at once).
#' @return A target for [sc_estimate()].
#' @examples
#' D <- rbind(c(1, 0), c(-1, 0))
#' sc_ame_rows("toy", D, coef = c(.5, -.5), side = c(1, -1))
#' @export
sc_ame_rows <- function(name, D, coef, side, w = NULL, position = TRUE, order = 31L, chunk = NULL) {
  D <- as.matrix(D); force(coef); force(side); force(w); force(position); force(order); force(chunk)
  if (length(coef) != nrow(D) || length(side) != nrow(D)) stop("coef and side need one entry per row of D", call. = FALSE)
  cols <- colnames(D)
  rows <- list(D = unname(D), coef = as.numeric(coef), side = as.numeric(side))
  .sc_target(name, "ame", "onestep", function(ctx) {
    if (ncol(rows$D) != ctx$p) stop(sprintf("D needs %d columns", ctx$p), call. = FALSE)
    .sc_check_columns(cols, ctx, "D")
    list(.sc_tgt_rows_chunked(name, rows, .sc_resolve_w(w, ctx), position, order, chunk))
  })
}

#' Ratio of two mean contrasts
#'
#' `sign * num / den` for two mean targets, with a delta-method standard error
#' from their joint influence values and a Wald interval. With the default
#' `sign = -1` and `num` the mean utility of a violation, `den` that of the
#' co-partisan feature, this is the relative utility cost of the paper (eq.
#' relative_utility_cost).
#'
#' @param name Label of the target.
#' @param num,den Two targets made by [sc_mean()].
#' @param sign Multiplies the ratio (default `-1`).
#' @return A target for [sc_estimate()].
#' @examples
#' sc_ratio("price of a violation",
#'          sc_mean("journalists", "diff_dem_code_u_journalists", w = "PL"),
#'          sc_mean("co-partisan", "diff_respParty", w = "PL"))
#' @export
sc_ratio <- function(name, num, den, sign = -1) {
  stopifnot(inherits(num, "sc_target"), inherits(den, "sc_target"), num$kind == "mean", den$kind == "mean")
  force(sign)
  .sc_target(name, "derived", "derived", NULL, parts = list(num, den),
             fun = function(z) sign * z[1] / z[2], deriv = function(z) c(sign / z[2], -sign * z[1] / z[2]^2))
}

#' Difference of two mean contrasts
#'
#' `a - b` for two mean targets (for example the same contrast in two
#' subgroups), with a delta-method standard error from their joint influence
#' values and a Wald interval.
#'
#' @param name Label of the target.
#' @param a,b Two targets made by [sc_mean()].
#' @return A target for [sc_estimate()].
#' @examples
#' sc_difference("Dem minus Rep",
#'               sc_mean("Dem", "diff_respParty", w = function(g) g$party == "Dem"),
#'               sc_mean("Rep", "diff_respParty", w = function(g) g$party == "Rep"))
#' @export
sc_difference <- function(name, a, b) {
  stopifnot(inherits(a, "sc_target"), inherits(b, "sc_target"), a$kind == "mean", b$kind == "mean")
  .sc_target(name, "derived", "derived", NULL, parts = list(a, b),
             fun = function(z) z[1] - z[2], deriv = function(z) c(1, -1))
}

#' Attribute importance shares
#'
#' For each respondent, the share of the variance of utility across levels
#' that each attribute contributes, averaged over the respondent's taste law
#' and then over respondents (paper eq. attribute_importance). An attribute's
#' variance is taken over a menu of its levels: by default the reference level
#' plus one level per coded dummy, all equally likely. A scale coded by one
#' column (such as a four-point policy position) takes `menus = matrix(1:4)`.
#' The shares of all groups sum to one and are estimated together by the joint
#' simplex update; the package reports point estimates (no intervals).
#'
#' @param groups Named list: the coded feature columns (names or indices) of
#'   each attribute. Every attribute that enters utility should be listed.
#'   Or a design from [sc_design()]: one group per attribute, its menu the
#'   attribute's coding of every level and its level weights the design's
#'   level probabilities (`menus` and `level_weights` are then ignored).
#' @param menus Optional named list of menus, one matrix per group (levels x
#'   the group's columns). Default: a zero row plus the identity.
#' @param level_weights Optional named list of level probabilities per group:
#'   one finite nonnegative weight per row of the group's menu, with a positive
#'   sum (normalized; default equal).
#' @param w Optional respondent weights (see [sc_mean()]).
#' @param names Labels of the shares (default the group names).
#' @param control Integration settings (`tol`, `n_start`, `n_max`, `n_rep`,
#'   `seed`); defaults as in the paper.
#' @return A target set for [sc_estimate()].
#' @examples
#' sc_importance(list(party = "diff_respParty", econ = "diff_p1_num"),
#'               menus = list(econ = matrix(1:4)))
#' @export
sc_importance <- function(groups, menus = NULL, level_weights = NULL, w = NULL, names = NULL, control = list()) {
  if (inherits(groups, "sc_design")) {
    des <- groups
    groups <- lapply(des$coding, colnames); menus <- lapply(des$coding, unname); level_weights <- des$probs
  }
  if (!is.list(groups) || is.null(base::names(groups))) stop("groups must be a named list", call. = FALSE)
  if (is.null(names)) names <- base::names(groups)
  force(menus); force(level_weights); force(w)
  ctl <- do.call(.rum_imp_control, control)
  .sc_target(names, "importance", "joint", function(ctx) {
    cols <- lapply(groups, .sc_resolve_cols, ctx = ctx)
    QL <- lapply(base::names(cols), function(g) {
      A <- if (!is.null(menus[[g]])) as.matrix(menus[[g]]) else rbind(0, diag(length(cols[[g]])))
      if (ncol(A) != length(cols[[g]])) stop("the menu of '", g, "' needs one column per coded feature", call. = FALSE)
      if (!is.numeric(A) || !all(is.finite(A))) stop("the menu of '", g, "' must be finite numbers", call. = FALSE)
      pi <- if (!is.null(level_weights[[g]])) level_weights[[g]] else rep(1, nrow(A))
      if (!is.numeric(pi) || length(pi) != nrow(A) || !all(is.finite(pi)) || any(pi < 0) || !(sum(pi) > 0))
        stop(sprintf(paste0("the level weights of '%s' must be %d finite nonnegative numbers (one per level of its menu) ",
                            "with a positive sum"), g, nrow(A)), call. = FALSE)
      .sc_Qlev(ctx$p, cols[[g]], A, pi)
    })
    .sc_imp_targets(QL, names, ctx$p, .sc_resolve_w(w, ctx), ctl)
  })
}

#' Decisiveness of named attribute swaps
#'
#' The probability that swapping one attribute of a profile between two named
#' levels flips the respondent's choice, with the same task shock and the
#' other attributes as recorded, averaged over the respondent's recorded tasks
#' and taste law and then over respondents (paper eq. swap_decisiveness).
#' Position terms are set to zero by default.
#'
#' @param groups Named list: the coded feature columns (names or indices) of
#'   each attribute.
#' @param pairs Named list per group: a two-column matrix (or length-2 vector)
#'   of the level indices (rows of the menu) compared; one target per row.
#' @param menus Optional list of menus per group (levels x columns); default a
#'   zero row plus the identity.
#' @param allowed Optional per-group restriction of the level pairs (see Details
#'   of the internal function; a logical vector over levels).
#' @param best_worst Also add the best-versus-worst swap of every group.
#' @param side `"zero"` (default: no position terms) or `"fitted"`.
#' @param names Labels of the targets.
#' @param w Optional respondent weights (see [sc_mean()]).
#' @param control Integration settings (list, see the internal `.rum_dec_control()`).
#' @return A target set for [sc_estimate()].
#' @examples
#' sc_decisiveness(list(party = "diff_respParty"), pairs = list(party = c(2, 1)),
#'                 names = "party: co-partisan vs not", w = "PL")
#' @export
sc_decisiveness <- function(groups, pairs = NULL, menus = NULL, allowed = NULL, best_worst = FALSE,
                            side = c("zero", "fitted"), names = NULL, w = NULL, control = list()) {
  side <- match.arg(side); force(groups); force(pairs); force(menus); force(allowed); force(best_worst); force(w)
  ctl <- do.call(.rum_dec_control, control)
  .sc_target(if (is.null(names)) "decisiveness" else names, "decisiveness", "onestep", function(ctx) {
    cols <- lapply(groups, .sc_resolve_cols, ctx = ctx)
    .rum_tgt_decisiveness_set(ctx$data, groups = cols, menus = menus, pairs = pairs, allowed = allowed,
                              side = side, best_worst = best_worst, names = names, w = .sc_resolve_w(w, ctx),
                              control = ctl)
  })
}

#' Expected support for one tax plan against another (Ballard-Rosa et al. 2017)
#'
#' A contest between two complete tax plans in the Ballard-Rosa, Martin and
#' Scheve (2017) application. Each plan's displayed revenue category follows
#' the experiment's revenue rule, so the contest averages over the pairs of
#' categories the two plans can show, weighted by their probabilities, before
#' averaging the expected support.
#'
#' @inheritParams sc_mean
#' @param plan,opponent Six tax rates each (percent; bracket 1 = lowest income),
#'   on the brackets' menus.
#' @return A target for [sc_estimate()].
#' @examples
#' sc_br_plan_contest("progressive vs flat", c(0, 5, 15, 25, 35, 45), rep(15, 6))
#' @export
sc_br_plan_contest <- function(name, plan, opponent, w = NULL) {
  rows <- .sc_br_plan_rows(plan, opponent)
  stopifnot(abs(sum(rows$coef) - 1) < 1e-8)
  sc_contest(name, rows$D, rows$coef, w = w)
}

#' Revenue-following endpoint AME of one tax bracket (Ballard-Rosa et al. 2017)
#'
#' The change in the probability that a plan is chosen when one bracket's rate
#' moves from the bottom to the top of its menu, with the displayed revenue
#' category redrawn from the revenue rule and the opponent as recorded. The
#' reference contexts are both sides of every recorded task, and position
#' effects are kept.
#'
#' @inheritParams sc_mean
#' @param bracket Bracket (1 = lowest income, 6 = highest).
#' @param profiles The recorded design: `list(A = , B = )`, two data frames
#'   with the six rate columns and the displayed category `taxrev`.
#' @param order Nodes of the Gauss-Hermite rule (default 61, the paper's).
#' @param chunk Rows per block (default 512).
#' @param probability_tol Revenue categories with probability at or below it
#'   are dropped.
#' @return A target for [sc_estimate()].
#' @examples
#' A <- data.frame(tax_rate_1_pct = 5, tax_rate_2_pct = 15, tax_rate_3_pct = 25,
#'                 tax_rate_4_pct = 25, tax_rate_5_pct = 35, tax_rate_6_pct = 45, taxrev = 3)
#' B <- data.frame(tax_rate_1_pct = 0, tax_rate_2_pct = 5, tax_rate_3_pct = 15,
#'                 tax_rate_4_pct = 25, tax_rate_5_pct = 35, tax_rate_6_pct = 55, taxrev = 2)
#' sc_br_endpoint_ame("top bracket", 6, list(A = A, B = B))
#' @export
sc_br_endpoint_ame <- function(name, bracket, profiles, w = NULL, order = 61L, chunk = 512L,
                               probability_tol = 1e-14) {
  rows <- .sc_br_ame_rows(bracket, profiles, probability_tol)
  if (rows$omitted > 1e-12) warning(sprintf("dropped revenue categories carry probability %.2g", rows$omitted), call. = FALSE)
  sc_ame_rows(name, rows$D, rows$coef, rows$side, w = w, position = TRUE, order = order, chunk = chunk)
}
