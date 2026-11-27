# Figures and tables from the package's estimate tables (sc_estimate()), fitted laws
# (sc_fitted_law()) and posterior summaries (sc_posterior()). Base graphics only. The look is
# the paper's (a port of its build_figures.R, as in the book's _figs-helpers.R): house blue,
# light grid lines, a dashed line at zero, and a hollow point when an interval is missing.

.scp_blue <- "#004a8c"
.scp_green <- "#27805A"
.scp_red <- "#BD4C39"
.scp_dark <- "#333333"
.scp_grid <- "#E8E8E8"
.scp_zero <- "#888888"
.scp_muted <- "#6E7378"   # group labels
.scp_palette <- c("#004a8c", "#27805A", "#BD4C39", "#333333", "#777777", "#8C6D31")

# ---------------------------------------------------------------------------------------------
# Small helpers

# The estimate tables passed in `...`: a list of data frames with name and estimate (lower,
# upper, se, n and kind are filled with NA when absent). A single plain list of tables is
# unpacked.
.scp_tables <- function(tabs, fun) {
  if (length(tabs) == 1L && is.list(tabs[[1L]]) && !is.data.frame(tabs[[1L]])) tabs <- tabs[[1L]]
  if (!length(tabs))
    stop(fun, "() needs at least one estimate table from sc_estimate()", call. = FALSE)
  nm <- names(tabs)
  if (is.null(nm)) nm <- rep("", length(tabs))
  nm[is.na(nm)] <- ""
  for (k in seq_along(tabs)) {
    x <- tabs[[k]]
    if (!is.data.frame(x) || !all(c("name", "estimate") %in% names(x)))
      stop(sprintf("%s(): table %d is not an estimate table (a data frame from sc_estimate() with columns name and estimate)",
                   fun, k), call. = FALSE)
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x$name <- as.character(x$name)
    if (anyDuplicated(x$name))
      stop(sprintf("%s(): table %d has repeated target names (%s)", fun, k,
                   paste(unique(x$name[duplicated(x$name)]), collapse = ", ")), call. = FALSE)
    for (col in c("lower", "upper", "se")) if (is.null(x[[col]])) x[[col]] <- NA_real_
    if (is.null(x$n)) x$n <- NA_integer_
    if (is.null(x$kind)) x$kind <- NA_character_
    tabs[[k]] <- x
  }
  names(tabs) <- nm
  tabs
}

# Rows shown: `order` when given (every name must be in some table), else the first table's
# names followed by names that only later tables have.
.scp_rows <- function(tabs, order = NULL, fun) {
  all_names <- unique(unlist(lapply(tabs, `[[`, "name"), use.names = FALSE))
  if (is.null(order)) return(all_names)
  order <- as.character(order)
  bad <- setdiff(order, all_names)
  if (length(bad)) stop(sprintf("%s(): order names targets that no table has: %s", fun, paste(bad, collapse = ", ")),
                        call. = FALSE)
  order
}

# Labels for `keys`: NULL gives the keys; a named vector is matched by name (keys it lacks keep
# their own name); an unnamed vector must have one label per key, in order.
.scp_labels <- function(labels, keys, fun) {
  if (is.null(labels)) return(keys)
  if (!is.character(labels) && !is.expression(labels)) labels <- as.character(labels)
  if (!is.null(names(labels)) && any(nzchar(names(labels)))) {
    out <- labels[match(keys, names(labels))]
    out[is.na(out)] <- keys[is.na(out)]
    return(unname(out))
  }
  if (length(labels) != length(keys))
    stop(sprintf("%s(): labels has %d entries for %d rows; give one label per row or name the labels by target",
                 fun, length(labels), length(keys)), call. = FALSE)
  unname(labels)
}

# Colors for k groups.
.scp_colors <- function(colors, k, fun) {
  if (is.null(colors)) {
    if (k > length(.scp_palette))
      stop(sprintf("%s(): %d groups need %d colors; pass colors", fun, k, k), call. = FALSE)
    return(.scp_palette[seq_len(k)])
  }
  if (length(colors) < k)
    stop(sprintf("%s(): colors has %d entries for %d groups", fun, length(colors), k), call. = FALSE)
  unname(colors[seq_len(k)])
}

.scp_check_scale <- function(scale, fun) {
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale) || scale <= 0)
    stop(sprintf("%s(): scale must be one positive number (100 for percent)", fun), call. = FALSE)
  scale
}

.scp_check_lim <- function(lim, what, fun) {
  if (is.null(lim)) return(NULL)
  if (!is.numeric(lim) || length(lim) != 2L || !all(is.finite(lim)) || lim[1L] >= lim[2L])
    stop(sprintf("%s(): %s must be two increasing finite numbers", fun, what), call. = FALSE)
  lim
}

# Vertical offsets of s marks within a row (first at the top).
.scp_offsets <- function(s) if (s <= 1L) 0 else seq(min(.3, .09 * (s - 1)), -min(.3, .09 * (s - 1)), length.out = s)

# Limits padded on both sides, always including zero when asked.
.scp_pad <- function(x, zero = TRUE, pad = .05) {
  x <- x[is.finite(x)]
  if (zero) x <- c(0, x)
  if (!length(x)) return(c(-1, 1))
  r <- range(x)
  if (diff(r) == 0) r <- r + c(-1, 1) * max(abs(r[1L]), 1) * .1
  r + c(-1, 1) * pad * diff(r)
}

# Tick marks inside the limits.
.scp_ticks <- function(lim, n = 5) {
  inside <- function(tk) tk[tk >= lim[1L] - 1e-9 * diff(lim) & tk <= lim[2L] + 1e-9 * diff(lim)]
  tk <- inside(pretty(lim, n = n))
  if (length(tk) < 2L) tk <- inside(pretty(lim, n = n + 2L))
  tk
}

# Automatic limits and ticks: a nice tick just outside the data limits (within 12% of the range)
# pulls the limit out to it, plus a small pad, so the outermost numbers are labelled.
.scp_axis <- function(lim, n = 5, pad = .02) {
  tk <- pretty(lim, n = n); w <- diff(lim)
  lo <- max(tk[tk <= lim[1L]]); hi <- min(tk[tk >= lim[2L]])
  if (lim[1L] - lo > 1e-9 * w && lim[1L] - lo <= .12 * w) lim[1L] <- lo - pad * w
  if (hi - lim[2L] > 1e-9 * w && hi - lim[2L] <= .12 * w) lim[2L] <- hi + pad * w
  list(lim = lim, ticks = .scp_ticks(lim, n))
}

# Default axis label from the target kinds.
.scp_default_label <- function(kinds, scale) {
  kinds <- unique(kinds[!is.na(kinds)])
  pct <- isTRUE(all.equal(scale, 100))
  unit <- if (pct) " (%)" else if (scale == 1) "" else sprintf(" (x %g)", scale)
  if (!length(kinds)) return(paste0("Estimate", unit))
  if (all(kinds == "mean")) return(if (scale == 1) "Mean utility (logit units)" else paste0("Mean utility", unit))
  if (all(kinds == "ame")) return(paste0("Choice effect", if (pct) " (pp)" else unit))
  if (all(kinds == "share")) return(paste0("Share favoring", unit))
  if (all(kinds == "choice")) return(paste0("Choice probability", unit))
  if (all(kinds == "importance")) return(paste0("Importance share", unit))
  if (all(kinds == "decisiveness")) return(paste0("Share of choices reversed", unit))
  if (all(kinds %in% c("share", "choice", "importance", "decisiveness"))) return(paste0("Share or probability", unit))
  paste0("Estimate", unit)
}

# Graphics settings of the paper's figures; returns the old values of everything the plot
# functions change, for on.exit().
.scp_par_keep <- function() graphics::par(c("las", "tcl", "mgp", "xaxs", "yaxs", "mar", "xpd", "cex.axis"))
.scp_paper_par <- function() graphics::par(las = 1, tcl = -.2, mgp = c(1.7, .45, 0), xaxs = "i", yaxs = "i")

# For the functions that use the whole page (their own layout() or mfrow): every setting but
# the layout. A layout cannot be read back from par(), so it is not restored; on exit the device
# is left with one figure per page and the other settings as they were.
.scp_page_keep <- function() {
  op <- graphics::par(no.readonly = TRUE)
  op[setdiff(names(op), c("mfrow", "mfcol", "mfg", "fig", "fin", "pin", "plt"))]
}
.scp_page_restore <- function(op) {
  graphics::par(mfrow = c(1L, 1L))
  graphics::par(op)
}

# Margins in lines, with the left margin in inches (to fit row labels).
.scp_set_margins <- function(bottom, left_in, top, right) {
  graphics::par(mar = c(bottom, 1, top, right))
  m <- graphics::par("mai"); m[2L] <- left_in
  graphics::par(mai = m)
}

# A legend centred under the figure region (in the bottom margin of the current plot).
.scp_bottom_legend <- function(legend, ...) {
  tw <- graphics::strwidth(legend, units = "user", cex = .85) + graphics::strwidth("mm", units = "user", cex = .85)
  graphics::legend(x = graphics::grconvertX(.5, "nfc", "user"), y = graphics::grconvertY(0, "nfc", "user"),
                   legend = legend, xjust = .5, yjust = 0, horiz = TRUE, bty = "n", cex = .85, xpd = NA,
                   text.width = tw, ...)
}

# Points and intervals (the book's ci()): segments where the interval is shown, solid points
# there and hollow points elsewhere.
.scp_ci <- function(est, lower, upper, y, shown, color, cex = .8) {
  if (any(shown)) graphics::segments(lower[shown], y[shown], upper[shown], y[shown], col = color, lwd = 1.3)
  ok <- is.finite(est)
  graphics::points(est[ok], y[ok], pch = ifelse(shown[ok], 16, 1), col = color, cex = cex)
}

# Which intervals to draw: finite, and with hide_wide narrower than 0.95 on the 0-1 scale.
.scp_shown <- function(lower, upper, hide_wide) {
  ok <- is.finite(lower) & is.finite(upper)
  if (hide_wide) ok <- ok & (upper - lower) < .95
  ok
}

# Named numeric reference values aligned to `rows` (NA where absent).
.scp_reference <- function(reference, rows, fun) {
  if (is.null(reference)) return(NULL)
  if (inherits(reference, "sc_compare")) reference <- stats::setNames(reference$truth, reference$name)
  if (!is.numeric(reference) || is.null(names(reference)))
    stop(sprintf("%s(): reference must be a named numeric vector (names = target names)", fun), call. = FALSE)
  out <- unname(reference[match(rows, names(reference))])
  if (all(is.na(out)))
    stop(sprintf("%s(): no name of reference matches a target shown", fun), call. = FALSE)
  out
}

# Rows of an estimate table for `rows`: by name when the table has every name, otherwise by
# position when it has one row per shown row.
.scp_match <- function(tab, rows, what, fun, n_all = NULL) {
  if (all(rows %in% tab$name)) return(tab[match(rows, tab$name), , drop = FALSE])
  if (nrow(tab) == length(rows)) return(tab)
  stop(sprintf(paste0("%s(): %s has %d rows and lacks target names %s; give it the targets' names or one row per ",
                      "row of the figure, in order"),
               fun, what, nrow(tab), paste(utils::head(setdiff(rows, tab$name), 3L), collapse = ", ")), call. = FALSE)
}

# Fitted laws: a data frame from sc_fitted_law() or a list of them.
.scp_laws <- function(laws, fun) {
  if (is.data.frame(laws)) laws <- list(laws)
  if (!is.list(laws) || !length(laws))
    stop(sprintf("%s(): laws must be a table from sc_fitted_law() or a list of them", fun), call. = FALSE)
  for (k in seq_along(laws)) {
    l <- laws[[k]]
    if (!is.data.frame(l) || !all(c("mean", "sd") %in% names(l)) || !nrow(l))
      stop(sprintf("%s(): law %d is not a table from sc_fitted_law() (columns mean, sd and weight)", fun, k),
           call. = FALSE)
    if (is.null(l$weight)) l$weight <- rep(1, nrow(l))
    if (!is.numeric(l$mean) || !is.numeric(l$sd) || !all(is.finite(l$mean)) || !all(is.finite(l$sd) & l$sd >= 0))
      stop(sprintf("%s(): law %d needs finite means and finite, non-negative sds", fun, k), call. = FALSE)
    if (!is.numeric(l$weight) || !all(is.finite(l$weight) & l$weight >= 0) || sum(l$weight) <= 0)
      stop(sprintf("%s(): law %d needs finite, non-negative weights with a positive sum", fun, k), call. = FALSE)
    laws[[k]] <- as.data.frame(l)[, c("mean", "sd", "weight")]
  }
  laws
}

# Probability mass of a fitted law (a mixture of normals) in the bins between `edges`. A point
# mass (a component with sd 0, as for a zero contrast) is spread as a normal with a quarter of
# the bin width, so it shows as a bar about one bin wide centred on its value (two half bins when
# the value is on a bin edge).
.scp_law_mass <- function(law, edges) {
  w <- law$weight / sum(law$weight)
  sd <- law$sd; sd[sd == 0] <- (edges[2L] - edges[1L]) / 4
  cdf <- vapply(edges, function(x) sum(w * stats::pnorm(x, law$mean, sd)), numeric(1))
  diff(cdf)
}

# Horizontal range for laws: zero, and each law's mean plus or minus three standard deviations
# (of the mixture with `mixture`, else of each component). A point mass (a component with sd 0)
# keeps a margin of 5% of that range on each side (1 when the range is a single point), so it
# never sits on an edge.
.scp_laws_range <- function(laws, mixture = TRUE) {
  ends <- unlist(lapply(laws, function(l) {
    if (!mixture) return(c(l$mean - 3 * l$sd, l$mean + 3 * l$sd))
    w <- l$weight / sum(l$weight); m <- sum(w * l$mean)
    s <- sqrt(sum(w * l$sd^2) + sum(w * (l$mean - m)^2)); c(m - 3 * s, m + 3 * s)
  }))
  r <- range(0, ends)
  pts <- unlist(lapply(laws, function(l) l$mean[l$sd == 0]))
  if (length(pts)) {
    r <- range(r, pts)
    h <- if (diff(r) > 0) .05 * diff(r) else max(1, .1 * abs(r[1L]))
    r <- range(r, pts - h, pts + h)
  }
  r
}

# Mass of a true density function in the same bins (midpoint rule on 4 points per bin).
.scp_truth_mass <- function(f, edges) {
  vapply(seq_len(length(edges) - 1L), function(b) {
    x <- seq(edges[b], edges[b + 1L], length.out = 9L)[c(2L, 4L, 6L, 8L)]
    mean(f(x)) * (edges[b + 1L] - edges[b])
  }, numeric(1))
}

.scp_step_x <- function(edges) as.vector(rbind(utils::head(edges, -1L), utils::tail(edges, -1L)))

# An empty panel of the paper (light grid, dashed zero, bold title over two lines).
.scp_panel <- function(limits, rows, title, ticks, margins = c(2.8, .4, 4, .6)) {
  graphics::par(mar = margins)
  graphics::plot(NA, xlim = limits, ylim = c(.5, rows + .5), axes = FALSE, xlab = "", ylab = "")
  graphics::abline(v = ticks, col = .scp_grid, lwd = .6)
  if (limits[1L] <= 0 && limits[2L] >= 0) graphics::abline(v = 0, col = .scp_zero, lty = 2, lwd = .7)
  graphics::axis(1, at = ticks, cex.axis = .9)
  graphics::mtext(title, 3, .7, cex = .92, font = 2)
}

# `color` mixed with white: an opaque tint that looks like `color` at opacity `alpha` on white.
.scp_tint <- function(color, alpha) {
  rgb <- grDevices::col2rgb(color) / 255
  grDevices::rgb(t(alpha * rgb + (1 - alpha) * 1))
}

# A fitted law as a ridge centred on row y: the outline of its mass in `bins` bins, as steps or
# (`line`) as a line through the bin midpoints, its baseline at y - 0.3 and its peak `height`
# rows above that. The fill is an opaque tint, so with a height above about 1 the lower ridges
# (drawn later) cover the tails of the ridges above them. `trim` draws the ridge and its
# baseline only between the outermost bins holding at least 1% of the peak bin's mass. `mark`
# adds a vertical line from the baseline up to the outline at that value (an estimated mean).
.scp_ridge <- function(law, y, limits, height = .63, color = .scp_blue, truth = NULL, bins = 150L,
                       fill_alpha = .20, mark = NULL, trim = FALSE, line = FALSE) {
  edges <- seq(limits[1L], limits[2L], length.out = bins + 1L); bw <- edges[2L] - edges[1L]
  h <- .scp_law_mass(law, edges)
  tm <- if (is.function(truth)) .scp_truth_mass(truth, edges) else NULL
  top <- max(h, tm, na.rm = TRUE)
  s <- if (is.finite(top) && top > 0) height / top else 0   # no mass inside the limits: a flat ridge
  keep <- seq_len(bins)
  if (trim && s > 0) {
    big <- which(pmax(h, if (is.null(tm)) 0 else tm) >= .01 * top)
    keep <- seq(min(big), max(big))
  }
  lo <- edges[min(keep)]; hi <- edges[max(keep) + 1L]
  ek <- edges[c(keep, max(keep) + 1L)]
  if (line) {
    mids <- (utils::head(ek, -1L) + utils::tail(ek, -1L)) / 2
    x <- c(lo, mids, hi); yh <- c(0, s * h[keep], 0)
    yt <- if (is.null(tm)) NULL else c(0, s * tm[keep], 0)
  } else {
    x <- .scp_step_x(ek); yh <- rep(s * h[keep], each = 2L)
    yt <- if (is.null(tm)) NULL else rep(s * tm[keep], each = 2L)
  }
  # Fill without a border, then one outline and one baseline, so the baseline has the same weight
  # along its whole length (a polygon border doubles it only where the mass is exactly zero).
  graphics::polygon(c(lo, x, hi), c(y - .3, y - .3 + yh, y - .3), col = .scp_tint(color, fill_alpha), border = NA)
  graphics::lines(x, y - .3 + yh, col = color, lwd = .7)
  graphics::segments(lo, y - .3, hi, y - .3, col = color, lwd = .7)
  if (!is.null(yt)) graphics::lines(x, y - .3 + yt, col = "black", lwd = 1)
  if (!is.null(mark) && is.finite(mark) && mark >= lo && mark <= hi) {
    top_at <- stats::approx(x, yh, xout = mark, ties = max)$y
    graphics::segments(mark, y - .3, mark, y - .3 + top_at, col = color, lwd = 1.6, lend = 1)
  }
  data.frame(x = (utils::head(edges, -1L) + utils::tail(edges, -1L)) / 2,
             density = h / bw, truth = if (is.null(tm)) NA_real_ else tm / bw)
}

# Groups of rows. `groups` gives each row's group like `labels` gives its label: one entry per
# row in order, or named by row key. Rows of a group must be adjacent. Returns NULL without
# groups, else the group of each row and the runs (label, first and last row).
.scp_groups <- function(groups, keys, fun) {
  if (is.null(groups)) return(NULL)
  if (is.list(groups)) {
    stop(sprintf("%s(): groups must be a character vector (one group per row, or named by row)", fun), call. = FALSE)
  }
  if (!is.null(names(groups)) && any(nzchar(names(groups))) && !all(keys %in% names(groups)))
    stop(sprintf("%s(): groups names no group for %s; name every row or give one group per row in order", fun,
                 paste(utils::head(setdiff(keys, names(groups)), 3L), collapse = ", ")), call. = FALSE)
  g <- as.character(.scp_labels(groups, keys, fun))
  if (anyNA(g) || any(!nzchar(g))) stop(sprintf("%s(): every row needs a non-empty group", fun), call. = FALSE)
  r <- rle(g)
  if (anyDuplicated(r$values))
    stop(sprintf("%s(): the rows of group \"%s\" are not adjacent; order the rows so each group's rows follow one another",
                 fun, r$values[duplicated(r$values)][1L]), call. = FALSE)
  last <- cumsum(r$lengths)
  list(group = g, runs = data.frame(label = r$values, first = last - r$lengths + 1L, last = last,
                                    stringsAsFactors = FALSE))
}

# Vertical positions of nr rows, first at the top: one unit apart, with an extra `gap` between
# groups. Returns y and the top of the highest row (y[1] + 0.5).
.scp_row_y <- function(nr, grp = NULL, gap = .6) {
  if (is.null(grp)) return(list(y = rev(seq_len(nr)), top = nr + .5))
  run <- rep(seq_len(nrow(grp$runs)), grp$runs$last - grp$runs$first + 1L)
  t <- (seq_len(nr) - 1L) + gap * (run - 1L)
  y <- max(t) + 1 - t
  list(y = y, top = y[1L] + .5)
}

# Group labels in the left margin, left-aligned `inset` inches from the figure's left edge and
# centred on each group's rows, in grey.
.scp_group_labels <- function(grp, y, inset = .08, cex = .88) {
  x <- graphics::grconvertX(inset / graphics::par("fin")[1L], "nfc", "user")
  for (i in seq_len(nrow(grp$runs))) {
    yy <- mean(y[c(grp$runs$first[i], grp$runs$last[i])])
    graphics::text(x, yy, grp$runs$label[i], adj = c(0, .5), xpd = NA, cex = cex, col = .scp_muted)
  }
}

# ---------------------------------------------------------------------------------------------
# Estimates

#' Plot estimates with their intervals
#'
#' A dot-and-interval plot of estimate tables from [sc_estimate()]: one row per
#' target, a point for the estimate and a line for its 95% interval. A point
#' is hollow when its interval is missing (for example importance shares, or a
#' share whose estimate is 0 or 1) or hidden by `hide_wide`. Several tables
#' passed as named arguments are drawn as groups, offset within each row in
#' different colors, with one legend at the bottom (the argument names are the
#' legend labels).
#'
#' @param ... One or more estimate tables from [sc_estimate()] (or one list of
#'   them). Several tables must be named; the names label the legend. Groups
#'   are kept apart by their position, so two tables may share a name (each
#'   keeps its own color and legend entry).
#' @param labels Row labels: a character vector named by target name, or one
#'   label per row in order. Default the target names.
#' @param order Optional character vector of target names: the rows shown, in
#'   order (first at the top). Default the first table's names, then names
#'   only later tables have.
#' @param scale Multiplier of estimates and intervals (100 for percent).
#' @param xlab Label of the horizontal axis. Default from the target kinds
#'   (for example "Mean utility (logit units)" or "Share favoring (%)").
#' @param xlim Optional limits of the horizontal axis, on the scaled values.
#' @param colors Colors of the groups (default house blue, green, red, ...).
#' @param reference Optional named numeric vector (names = target names) of
#'   reference values on the unscaled scale, for example true values in a
#'   simulation or regression AMCEs; multiplied by `scale`. A table from
#'   [sc_compare()] gives its true values.
#' @param reference_label Legend label of the reference values (default
#'   "Reference").
#' @param reference_style `"diamond"` (a grey diamond just below the row's
#'   estimates) or `"circle"` (an open black circle over the row).
#' @param hide_wide If `TRUE`, an interval spanning at least 0.95 on the
#'   unscaled 0-1 scale is not drawn and its point is hollow (for shares near
#'   0 or 1, whose intervals say little).
#' @param legend If `TRUE`, a legend at the bottom when there are several
#'   groups, a reference or hollow points.
#' @param main Optional title.
#' @param groups Optional groups of rows (for example attributes), named in
#'   grey to the left of the row labels with a small gap between groups: one
#'   group per row in order, or a character vector named by target name. The
#'   rows of a group must be adjacent.
#' @return Invisibly, the plotting data: a data frame with one row per group
#'   and target that the group's table has (`group`, `name`, `label`, `y`,
#'   scaled `estimate`, `lower`,
#'   `upper`, `shown` (whether the interval is drawn) and `reference`), and
#'   `row_group` when `groups` is given.
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   est <- sc_estimate(fit, list(sc_mean("Top bracket", "tax6_linear"),
#'                                sc_mean("Bottom bracket", "tax1_linear")))
#'   sc_plot_estimates(est, reference = c("Top bracket" = 5, "Bottom bracket" = 0),
#'                     reference_label = "Benchmark")
#' }
#' @export
sc_plot_estimates <- function(..., labels = NULL, order = NULL, scale = 1, xlab, xlim = NULL, colors = NULL,
                              reference = NULL, reference_label = NULL, reference_style = c("diamond", "circle"),
                              hide_wide = FALSE, legend = TRUE, main = NULL, groups = NULL) {
  fun <- "sc_plot_estimates"
  tabs <- .scp_tables(list(...), fun); k <- length(tabs); gn <- names(tabs)
  if (k > 1L && any(!nzchar(gn)))
    stop("sc_plot_estimates(): name every table when passing several, e.g. sc_plot_estimates(Model = a, Truth = b)",
         call. = FALSE)
  if (k == 1L && !nzchar(gn)) gn <- "Estimate and 95% interval"
  scale <- .scp_check_scale(scale, fun); xlim <- .scp_check_lim(xlim, "xlim", fun)
  reference_style <- match.arg(reference_style)
  rows <- .scp_rows(tabs, order, fun); labs <- .scp_labels(labels, rows, fun)
  colors <- .scp_colors(colors, k, fun)
  ref <- .scp_reference(reference, rows, fun)
  if (is.null(reference_label)) reference_label <- "Reference"
  if (missing(xlab)) xlab <- .scp_default_label(unlist(lapply(tabs, function(x) x$kind[x$name %in% rows])), scale)
  nr <- length(rows)
  grp <- .scp_groups(groups, rows, fun); ry <- .scp_row_y(nr, grp); yy <- ry$y
  diamond <- !is.null(ref) && reference_style == "diamond"
  off <- .scp_offsets(k + diamond)
  # one data frame per group, kept apart by position (group names are only labels and may repeat)
  pdl <- lapply(seq_len(k), function(g) {
    x <- tabs[[g]]; i <- match(rows, x$name)
    z <- data.frame(group = gn[g], name = rows, label = if (is.character(labs)) labs else rows, y = yy + off[g],
                    estimate = scale * x$estimate[i], lower = scale * x$lower[i], upper = scale * x$upper[i],
                    shown = .scp_shown(x$lower[i], x$upper[i], hide_wide),
                    reference = if (is.null(ref)) NA_real_ else scale * ref, stringsAsFactors = FALSE)
    if (!is.null(grp)) z$row_group <- grp$group
    z[!is.na(z$estimate), , drop = FALSE]
  })
  pd <- do.call(rbind, pdl)
  if (is.null(xlim)) {
    ax <- .scp_axis(.scp_pad(c(pd$estimate, pd$lower[pd$shown], pd$upper[pd$shown], scale * ref), pad = .04))
    xlim <- ax$lim; ticks <- ax$ticks
  } else ticks <- .scp_ticks(xlim)
  hollow <- any(!pd$shown & is.finite(pd$estimate))
  show_leg <- isTRUE(legend) && (k > 1L || !is.null(ref) || hollow)
  op <- .scp_par_keep(); on.exit(graphics::par(op))
  .scp_paper_par()
  lab_cex <- .9
  left <- max(graphics::strwidth(labs, "inches", cex = lab_cex)) + .2
  if (!is.null(grp)) left <- left + max(graphics::strwidth(grp$runs$label, "inches", cex = .88)) + .3
  .scp_set_margins(3 + 1.8 * show_leg, left, if (is.null(main)) .8 else 2.4, .8)
  graphics::plot(NA, xlim = xlim, ylim = c(.5, ry$top), axes = FALSE, xlab = xlab, ylab = "")
  graphics::abline(v = ticks, col = .scp_grid, lwd = .6)
  if (xlim[1L] <= 0 && xlim[2L] >= 0) graphics::abline(v = 0, col = .scp_zero, lty = 2, lwd = .7)
  graphics::axis(1, at = ticks, cex.axis = .9)
  graphics::axis(2, at = yy, labels = labs, tick = FALSE, cex.axis = lab_cex)
  if (!is.null(grp)) .scp_group_labels(grp, yy)
  if (!is.null(main)) graphics::mtext(main, 3, .7, cex = .95, font = 2)
  for (g in seq_len(k)) {
    z <- pdl[[g]]
    .scp_ci(z$estimate, z$lower, z$upper, z$y, z$shown, colors[g])
  }
  if (!is.null(ref)) {
    if (diamond) graphics::points(scale * ref, yy + off[k + 1L], pch = 18, col = "#555555", cex = .85)
    else graphics::points(scale * ref, yy, pch = 1, col = "black", cex = 1.2)
  }
  if (show_leg) {
    lg <- gn; lc <- colors; lp <- rep(16, k); ll <- rep(1, k); pc <- rep(.8, k)
    if (!is.null(ref)) {
      lg <- c(lg, reference_label); lc <- c(lc, if (diamond) "#555555" else "black")
      lp <- c(lp, if (diamond) 18 else 1); ll <- c(ll, NA); pc <- c(pc, if (diamond) .85 else 1.2)
    }
    if (hollow) { lg <- c(lg, "No interval shown"); lc <- c(lc, "#555555"); lp <- c(lp, 1); ll <- c(ll, NA); pc <- c(pc, .8) }
    .scp_bottom_legend(legend = lg, col = lc, pch = lp, lty = ll, lwd = 1.3, pt.cex = pc)
  }
  rownames(pd) <- NULL
  invisible(pd)
}

#' @rdname sc_plot_estimates
#' @param x An estimate table from [sc_estimate()].
#' @export
plot.sc_estimates <- function(x, ...) sc_plot_estimates(x, ...)

# ---------------------------------------------------------------------------------------------
# Shares favoring and opposing

#' Plot preference shares as favoring and opposing bars
#'
#' One row per target of an estimate table of [sc_share()] targets: a blue bar
#' to the right of the centre line for the percent of respondents whose taste
#' for the contrast is positive (favoring), and a red bar to the left for the
#' rest (opposing). The 95% interval of the share is drawn as a dark line at
#' the end of the blue bar; with `hide_wide`, an interval spanning at least 95
#' points is left out. Both bars come from the same estimate, so the red bar
#' is 100 minus the blue one; a respondent with a zero taste (possible only
#' for a contrast whose law has a point mass at zero) counts as opposing.
#'
#' @param shares Estimate table of preference shares ([sc_share()] targets).
#' @param labels Row labels: a character vector named by target name, or one
#'   label per row in order. Default the target names.
#' @param order Optional character vector of target names: the rows shown, in
#'   order (first at the top).
#' @param groups Optional groups of rows, named in grey at the left with a
#'   small gap between groups: one group per row in order, or a character
#'   vector named by target name. The rows of a group must be adjacent.
#' @param intervals If `TRUE`, draw each share's 95% interval.
#' @param hide_wide If `TRUE`, leave out intervals spanning at least 95
#'   points.
#' @param colors Colors of the favoring and opposing bars.
#' @param xlab Label of the horizontal axis.
#' @param legend If `TRUE`, a legend at the bottom.
#' @param main Optional title.
#' @param reference Optional named numeric vector (names = target names) of
#'   reference shares between 0 and 1, for example true values in a
#'   simulation, drawn as open black circles; or a table from [sc_compare()].
#' @param reference_label Legend label of the reference values (default
#'   "Reference").
#' @return Invisibly, the plotting data: a data frame with one row per target
#'   (`name`, `label`, `y`, `favor` and `oppose` in percent, the interval
#'   `lower` and `upper` in percent, `shown`, `reference` in percent, and
#'   `group` when `groups` is given).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   f <- paste0("tax", 1:6, "_linear")
#'   sh <- sc_estimate(fit, lapply(1:6, function(j) sc_share(paste("Bracket", j), f[j])))
#'   sc_plot_shares(sh, groups = rep(c("Lower", "Upper"), each = 3))
#' }
#' @export
sc_plot_shares <- function(shares, labels = NULL, order = NULL, groups = NULL, intervals = TRUE, hide_wide = TRUE,
                           colors = c(.scp_blue, .scp_red), xlab = "Respondents (%)", legend = TRUE, main = NULL,
                           reference = NULL, reference_label = NULL) {
  fun <- "sc_plot_shares"
  sh <- .scp_tables(list(shares), fun)[[1L]]
  kinds <- sh$kind[!is.na(sh$kind)]
  if (length(kinds) && !all(kinds == "share"))
    stop(sprintf("sc_plot_shares(): shares must hold sc_share() targets only; found kind %s",
                 paste(unique(kinds[kinds != "share"]), collapse = ", ")), call. = FALSE)
  if (any(is.finite(sh$estimate) & (sh$estimate < 0 | sh$estimate > 1)))
    stop(paste0("sc_plot_shares(): share estimates must lie between 0 and 1; estimate the shares with ",
                "sc_estimate(bounded = TRUE), the default, whose update keeps them there"), call. = FALSE)
  if (length(colors) != 2L) stop("sc_plot_shares(): colors needs two colors (favoring, opposing)", call. = FALSE)
  rows <- .scp_rows(list(sh), order, fun); nr <- length(rows)
  labs <- .scp_labels(labels, rows, fun)
  grp <- .scp_groups(groups, rows, fun); ry <- .scp_row_y(nr, grp); yy <- ry$y
  x <- sh[match(rows, sh$name), , drop = FALSE]
  favor <- 100 * x$estimate
  shown <- isTRUE(intervals) & .scp_shown(x$lower, x$upper, hide_wide)
  ref <- .scp_reference(reference, rows, fun)
  if (!is.null(ref) && any(is.finite(ref) & (ref < 0 | ref > 1)))
    stop("sc_plot_shares(): reference shares must lie between 0 and 1", call. = FALSE)
  if (is.null(reference_label)) reference_label <- "Reference"
  pd <- data.frame(name = rows, label = if (is.character(labs)) labs else rows, y = yy,
                   favor = favor, oppose = 100 - favor, lower = 100 * x$lower, upper = 100 * x$upper,
                   shown = shown, reference = if (is.null(ref)) NA_real_ else 100 * ref, stringsAsFactors = FALSE)
  if (!is.null(grp)) pd$group <- grp$group
  op <- .scp_par_keep(); on.exit(graphics::par(op))
  .scp_paper_par()
  left <- max(graphics::strwidth(labs, "inches", cex = .9)) + .2
  if (!is.null(grp)) left <- left + max(graphics::strwidth(grp$runs$label, "inches", cex = .88)) + .3
  show_leg <- isTRUE(legend)
  .scp_set_margins(3 + 1.8 * show_leg, left, if (is.null(main)) .8 else 2.4, .8)
  ticks <- c(-100, -50, 0, 50, 100)
  graphics::plot(NA, xlim = c(-104, 104), ylim = c(.5, ry$top), axes = FALSE, xlab = xlab, ylab = "")
  graphics::abline(v = ticks, col = .scp_grid, lwd = .6)
  graphics::axis(1, at = ticks, labels = abs(ticks), cex.axis = .9)
  graphics::axis(2, at = yy, labels = labs, tick = FALSE, cex.axis = .9)
  if (!is.null(grp)) .scp_group_labels(grp, yy)
  if (!is.null(main)) graphics::mtext(main, 3, .7, cex = .95, font = 2)
  ok <- is.finite(favor)
  graphics::rect(0, yy[ok] - .3, favor[ok], yy[ok] + .3, col = .scp_tint(colors[1L], .75), border = NA)
  graphics::rect(-(100 - favor[ok]), yy[ok] - .3, 0, yy[ok] + .3, col = .scp_tint(colors[2L], .75), border = NA)
  graphics::abline(v = 0, col = .scp_dark, lwd = .8)
  if (any(shown)) graphics::segments(pd$lower[shown], yy[shown], pd$upper[shown], yy[shown], col = .scp_dark, lwd = 1.3)
  if (!is.null(ref)) graphics::points(100 * ref, yy, pch = 21, bg = "white", col = "black", cex = 1.2)
  if (show_leg) {
    lg <- c("Favoring", "Opposing"); fl <- c(.scp_tint(colors[1L], .75), .scp_tint(colors[2L], .75))
    bd <- c(NA, NA); lc <- c(NA, NA); lt <- c(NA, NA); lp <- c(NA, NA)
    if (any(shown)) { lg <- c(lg, "95% interval of the share"); fl <- c(fl, NA); bd <- c(bd, NA); lc <- c(lc, .scp_dark); lt <- c(lt, 1); lp <- c(lp, NA) }
    if (!is.null(ref)) { lg <- c(lg, reference_label); fl <- c(fl, NA); bd <- c(bd, NA); lc <- c(lc, "black"); lt <- c(lt, NA); lp <- c(lp, 1) }
    .scp_bottom_legend(legend = lg, fill = fl, border = bd, col = lc, lty = lt, lwd = 1.3, pch = lp, pt.cex = 1.2)
  }
  invisible(pd)
}

# ---------------------------------------------------------------------------------------------
# Fitted laws

#' Plot fitted distributions of tastes
#'
#' Draws one or more fitted laws from [sc_fitted_law()]: the population
#' distribution of a taste contrast, a mixture of each held-out respondent's
#' normal law. `"ridges"` stacks the laws in one panel on one horizontal
#' axis, one row each, as the outline of their mass in `bins` bins, scaled to
#' the same height and drawn only where a bin holds at least 1% of the peak
#' bin's mass (the paper's Panel B draws the whole baseline); `groups` names groups of
#' rows (for example attributes) in grey to the left of the labels, with a gap
#' and alternating fills between groups, and a `height` above about 1 lets
#' each ridge rise over the row above it. `"panels"` gives each law its own
#' panel over its means plus or minus three standard deviations (and zero),
#' with its density on the vertical axis. `means` adds the estimated mean
#' taste: in `"ridges"` a vertical line from the baseline to the outline, in
#' `"panels"` a red line with its 95% interval as a red bar.
#'
#' The curves are fitted values before the score correction, so a corrected
#' mean need not sit at the curve's centre.
#'
#' A law or mixture component with standard deviation 0 is a point mass (for
#' example the law of a zero contrast). It is drawn as a narrow bar, about one
#' bin wide, at its value, and the automatic limits leave a margin around it.
#'
#' `"panels"` uses the whole device page, like [pairs()]: it sets its own
#' grid of panels, so it cannot be placed in one panel of your own
#' [layout()] or `par(mfrow = )`. Afterwards the device has one figure per
#' page (an earlier layout is not restored) and the other graphics settings
#' are as they were. `"ridges"` draws one panel and can go in a layout.
#'
#' @param laws A table from [sc_fitted_law()], or a list of them.
#' @param labels Labels of the laws: one per law in order, or named by the
#'   names of `laws`. Default the names of `laws`, else "Law 1", ...
#' @param means Optional estimate table from [sc_estimate()] with one mean per
#'   law, in the order of `laws`.
#' @param layout `"ridges"` or `"panels"`.
#' @param truth Optional true densities, for simulations: a function (one law)
#'   or a list of functions (or `NULL`s), one per law, each taking a vector of
#'   values and returning the density there. Drawn as black lines.
#' @param xlim Optional common horizontal limits.
#' @param xlab Label of the horizontal axis.
#' @param ncol Number of panel columns (`"panels"`).
#' @param main Optional title (`"ridges"`).
#' @param groups Optional groups of laws (`"ridges"` only): one group per law
#'   in order, or a character vector named by the names of `laws`. The laws of
#'   a group must be adjacent.
#' @param height Height of each ridge's peak, in rows (`"ridges"`). The
#'   default 0.63 keeps rows apart; about 1.3 gives overlapping ridges.
#' @param bins Number of bins of the outline (at least 20).
#' @param outline `"step"` draws each bin's mass as a step (a point mass shows
#'   as a narrow bar); `"line"` joins the bin midpoints, which reads more
#'   smoothly with many bins (`"ridges"`).
#' @return Invisibly, the plotting data: a data frame with one row per law and
#'   bin (`law`, `label`, bin midpoint `x`, fitted `density`, true density
#'   `truth`, and `group` when `groups` is given).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   laws <- list(sc_fitted_law(fit, "tax1_linear"), sc_fitted_law(fit, "tax6_linear"))
#'   sc_plot_laws(laws, labels = c("Bottom bracket", "Top bracket"))
#'   sc_plot_laws(laws, labels = c("Bottom bracket", "Top bracket"), layout = "panels", ncol = 2)
#'   means <- sc_estimate(fit, list(sc_mean("bottom", "tax1_linear"), sc_mean("top", "tax6_linear")))
#'   sc_plot_laws(laws, labels = c("Bottom bracket", "Top bracket"), means = means,
#'                groups = c("Tax rates", "Tax rates"), height = 1.3)
#' }
#' @export
sc_plot_laws <- function(laws, labels = NULL, means = NULL, layout = c("ridges", "panels"), truth = NULL,
                         xlim = NULL, xlab = "Utility contrast (logit units)", ncol = 3, main = NULL,
                         groups = NULL, height = .63, bins = 150, outline = c("step", "line")) {
  fun <- "sc_plot_laws"
  layout <- match.arg(layout); outline <- match.arg(outline)
  if (!is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0)
    stop("sc_plot_laws(): height must be one positive number (rows)", call. = FALSE)
  if (!is.numeric(bins) || length(bins) != 1L || !is.finite(bins) || bins < 20)
    stop("sc_plot_laws(): bins must be one number of at least 20", call. = FALSE)
  bins <- as.integer(round(bins))
  if (!is.null(groups) && layout != "ridges")
    stop("sc_plot_laws(): groups are drawn with layout = \"ridges\"", call. = FALSE)
  keys <- if (is.data.frame(laws)) "Law 1" else names(laws)
  laws <- .scp_laws(laws, fun); k <- length(laws)
  if (is.null(keys) || any(!nzchar(keys))) keys <- paste("Law", seq_len(k))
  labs <- .scp_labels(labels, keys, fun)
  xlim <- .scp_check_lim(xlim, "xlim", fun)
  if (is.function(truth)) truth <- list(truth)
  if (!is.null(truth)) {
    if (!is.list(truth) || length(truth) != k || !all(vapply(truth, function(f) is.null(f) || is.function(f), TRUE)))
      stop(sprintf("sc_plot_laws(): truth must be a list of %d density functions (or NULLs), one per law", k), call. = FALSE)
  } else truth <- vector("list", k)
  if (!is.null(means)) {
    means <- .scp_tables(list(means), fun)[[1L]]
    if (nrow(means) != k)
      stop(sprintf("sc_plot_laws(): means has %d rows for %d laws; give one mean per law, in order", nrow(means), k),
           call. = FALSE)
  }
  if (!is.numeric(ncol) || length(ncol) != 1L || ncol < 1) stop("sc_plot_laws(): ncol must be a positive number", call. = FALSE)
  has_truth <- any(!vapply(truth, is.null, TRUE))
  if (layout == "ridges") {
    grp <- .scp_groups(groups, keys, fun)
    if (is.null(xlim)) xlim <- range(pretty(.scp_laws_range(laws), n = 4))
    ticks <- .scp_ticks(xlim)
    ry <- .scp_row_y(k, grp); yy <- ry$y
    top <- max(ry$top, yy[1L] - .3 + height + .12)
    # fills alternate between two tints from group to group
    run <- if (is.null(grp)) rep(1L, k) else rep(seq_len(nrow(grp$runs)), grp$runs$last - grp$runs$first + 1L)
    alpha <- ifelse(run %% 2L == 1L, .18, .32)
    has_leg <- has_truth || !is.null(means)
    op <- .scp_par_keep(); on.exit(graphics::par(op))
    .scp_paper_par()
    left <- max(graphics::strwidth(labs, "inches", cex = .9)) + .2
    if (!is.null(grp)) left <- left + max(graphics::strwidth(grp$runs$label, "inches", cex = .88)) + .3
    .scp_set_margins(3 + 1.8 * has_leg, left, if (is.null(main)) .8 else 2.4, .8)
    graphics::plot(NA, xlim = xlim, ylim = c(.5, top), axes = FALSE, xlab = xlab, ylab = "")
    graphics::abline(v = ticks, col = .scp_grid, lwd = .6)
    graphics::axis(1, at = ticks, cex.axis = .9)
    graphics::axis(2, at = yy, labels = labs, tick = FALSE, cex.axis = .9)
    if (!is.null(grp)) .scp_group_labels(grp, yy)
    if (!is.null(main)) graphics::mtext(main, 3, .7, cex = .95, font = 2)
    out <- lapply(seq_len(k), function(j) {
      d <- .scp_ridge(laws[[j]], yy[j], xlim, height = height, truth = truth[[j]], bins = bins,
                      fill_alpha = alpha[j], mark = if (is.null(means)) NULL else means$estimate[j],
                      trim = TRUE, line = outline == "line")
      d <- cbind(law = j, label = if (is.character(labs)) labs[j] else keys[j], d, stringsAsFactors = FALSE)
      if (!is.null(grp)) d$group <- grp$group[j]
      d
    })
    # the zero line goes over the ridges, which cover each other's tails
    if (xlim[1L] <= 0 && xlim[2L] >= 0) graphics::abline(v = 0, col = .scp_zero, lty = 2, lwd = .7)
    if (has_leg) {
      lg <- "Fitted distribution"; fl <- .scp_tint(.scp_blue, .2); bd <- .scp_blue; lc <- NA; lt <- NA; lw <- NA
      if (has_truth) { lg <- c(lg, "True distribution"); fl <- c(fl, NA); bd <- c(bd, NA); lc <- c(lc, "black"); lt <- c(lt, 1); lw <- c(lw, 1) }
      if (!is.null(means)) { lg <- c(lg, "Estimated mean"); fl <- c(fl, NA); bd <- c(bd, NA); lc <- c(lc, .scp_blue); lt <- c(lt, 1); lw <- c(lw, 1.6) }
      .scp_bottom_legend(legend = lg, fill = fl, border = bd, col = lc, lty = lt, lwd = lw)
    }
    return(invisible(do.call(rbind, out)))
  }
  # panels
  nc <- min(as.integer(ncol), k); nrw <- ceiling(k / nc)
  op <- .scp_page_keep(); on.exit(.scp_page_restore(op))
  has_leg <- has_truth || !is.null(means)
  graphics::par(mfrow = c(nrw, nc), mar = c(1.9, 2.9, 1.9, .5), oma = c(1.2 + 1.6 * has_leg, .2, .2, .2),
                las = 1, tcl = -.2, mgp = c(1.9, .45, 0), xaxs = "i", yaxs = "i")
  out <- vector("list", k)
  for (j in seq_len(k)) {
    l <- laws[[j]]
    lim <- if (is.null(xlim)) .scp_laws_range(list(l), mixture = FALSE) else xlim
    if (is.null(xlim) && !is.null(means)) {   # keep the corrected mean and its interval in view
      mv <- unlist(means[j, intersect(c("estimate", "lower", "upper"), names(means))])
      mv <- mv[is.finite(mv)]
      if (length(mv)) {
        pad <- .05 * diff(range(lim, mv))
        lim <- range(lim, min(mv) - pad, max(mv) + pad)
      }
    }
    edges <- seq(lim[1L], lim[2L], length.out = bins + 1L); bw <- diff(edges)[1L]
    dens <- .scp_law_mass(l, edges) / bw
    td <- if (is.function(truth[[j]])) .scp_truth_mass(truth[[j]], edges) / bw else NULL
    top <- 1.12 * max(dens, td, na.rm = TRUE)
    if (!is.finite(top) || top <= 0) top <- 1   # no mass inside the limits
    graphics::plot(NA, xlim = lim, ylim = c(0, top), axes = FALSE, xlab = "", ylab = "Density", cex.lab = .8)
    yt <- .scp_ticks(c(0, top), n = 3)
    graphics::abline(h = yt, col = .scp_grid, lwd = .6)
    graphics::abline(v = 0, col = .scp_zero, lty = 2, lwd = .7)
    graphics::axis(1, at = .scp_ticks(lim, n = 4), cex.axis = .8)
    graphics::axis(2, at = yt, cex.axis = .75)
    graphics::title(labs[j], line = .6, cex.main = .92, font.main = 2)
    graphics::polygon(c(lim[1L], .scp_step_x(edges), lim[2L]), c(0, rep(dens, each = 2L), 0),
                      col = grDevices::adjustcolor(.scp_blue, .22), border = .scp_blue, lwd = .8)
    if (!is.null(td)) graphics::lines(.scp_step_x(edges), rep(td, each = 2L), col = "black", lwd = 1.1)
    if (!is.null(means)) {
      m <- means[j, ]
      graphics::abline(v = m$estimate, col = .scp_red, lwd = 1)
      if (is.finite(m$lower) && is.finite(m$upper))
        graphics::segments(m$lower, .95 * max(dens), m$upper, .95 * max(dens), col = .scp_red, lwd = 1.4)
    }
    out[[j]] <- data.frame(law = j, label = if (is.character(labs)) labs[j] else keys[j],
                           x = (utils::head(edges, -1L) + utils::tail(edges, -1L)) / 2, density = dens,
                           truth = if (is.null(td)) NA_real_ else td, stringsAsFactors = FALSE)
  }
  graphics::mtext(xlab, 1, outer = TRUE, line = .1, cex = .9)
  if (has_leg) {
    graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    graphics::plot.new()
    lg <- "Fitted distribution"; fl <- grDevices::adjustcolor(.scp_blue, .22); bd <- .scp_blue; lc <- NA; lt <- NA
    if (has_truth) { lg <- c(lg, "True distribution"); fl <- c(fl, NA); bd <- c(bd, NA); lc <- c(lc, "black"); lt <- c(lt, 1) }
    if (!is.null(means)) {
      lg <- c(lg, "Estimated mean and 95% interval"); fl <- c(fl, NA); bd <- c(bd, NA); lc <- c(lc, .scp_red); lt <- c(lt, 1)
    }
    .scp_bottom_legend(legend = lg, fill = fl, border = bd, col = lc, lty = lt, lwd = 1.2)
  }
  invisible(do.call(rbind, out))
}

# ---------------------------------------------------------------------------------------------
# The paper's summary figure

#' The paper's summary figure of preferences and choice effects
#'
#' Four panels side by side, one row per feature: (A) the mean taste with its
#' 95% interval, (B) the fitted distribution of the taste as a ridge, (C) the
#' share of respondents with a positive taste, in percent, and (D) optionally
#' the average marginal effect on choices, in percentage points, with optional
#' reference values (for example regression AMCEs) as grey diamonds just below.
#' In Panel C a point is hollow, and its interval not drawn, when the interval
#' spans at least 95 percentage points (`hide_wide`).
#'
#' Rows follow `order` or the names of `means`. The other tables are matched to
#' the rows by target name when they have every row's name, otherwise by
#' position (one row per row of the figure). `laws` is matched by name when
#' named by target. Otherwise it is matched by position: one law per row of
#' `means`, in the order of `means` (reordered with the rows when `order`
#' permutes or drops them), or, when `order` shows fewer rows than `means` has,
#' one law per row of the figure, in the figure's order. A law with standard
#' deviation 0 (a point mass) is drawn in Panel B as a narrow bar at its value.
#'
#' The function draws on the current device and does not set its size; the
#' book's version is 9.2 by 5.5 inches at 11 points. It uses the whole device
#' page, like [pairs()]: it sets its own [layout()] of panels, so it cannot be
#' placed in one panel of your own `layout()` or `par(mfrow = )`. Afterwards
#' the device has one figure per page (an earlier layout is not restored) and
#' the other graphics settings are as they were.
#'
#' @param means Estimate table of mean tastes ([sc_mean()] targets).
#' @param laws Fitted laws ([sc_fitted_law()]), a list, one per feature.
#' @param shares Estimate table of preference shares ([sc_share()] targets).
#' @param ames Optional estimate table of choice effects ([sc_ame_switch()]
#'   targets); adds Panel D.
#' @param labels Row labels: named by target name, or one per row in order.
#' @param reference Optional named numeric (names = target names of `means`)
#'   of reference choice effects on the probability scale, drawn in Panel D
#'   as grey diamonds.
#' @param order Optional target names of `means`: the rows shown, in order.
#' @param xlim Optional list of horizontal limits by panel (`A`, `B`, `C`,
#'   `D`); missing panels get automatic limits.
#' @param ticks Optional list of tick positions by panel, like `xlim`.
#' @param titles Optional panel titles by panel, like `xlim`.
#' @param hide_wide Hide intervals spanning at least 95 points in Panel C.
#' @param widths Optional relative widths of the label column and the panels.
#' @param groups Optional groups of rows (for example attributes), named in
#'   grey at the left of the label column, with a small gap and alternating
#'   ridge fills between groups: one group per row in order, or a character
#'   vector named by target name. The rows of a group must be adjacent.
#' @return Invisibly, a list: `rows` (a data frame with each row's name,
#'   label, position, group when `groups` is given, and the scaled estimates
#'   and intervals drawn), `xlim` and `ticks` (lists by panel).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   f <- c("tax1_linear", "tax6_linear")
#'   means <- sc_estimate(fit, lapply(f, function(j) sc_mean(j, j)))
#'   shares <- sc_estimate(fit, lapply(f, function(j) sc_share(j, j)), bounded = FALSE)
#'   laws <- lapply(f, function(j) sc_fitted_law(fit, j))
#'   sc_plot_preferences(means, laws, shares, labels = c("Bottom bracket", "Top bracket"))
#' }
#' @export
sc_plot_preferences <- function(means, laws, shares, ames = NULL, labels = NULL, reference = NULL, order = NULL,
                                xlim = NULL, ticks = NULL, titles = NULL, hide_wide = TRUE, widths = NULL,
                                groups = NULL) {
  fun <- "sc_plot_preferences"
  means <- .scp_tables(list(means), fun)[[1L]]
  shares <- .scp_tables(list(shares), fun)[[1L]]
  if (!is.null(ames)) ames <- .scp_tables(list(ames), fun)[[1L]]
  rows <- .scp_rows(list(means), order, fun); nr <- length(rows)
  labs <- .scp_labels(labels, rows, fun)
  grp <- .scp_groups(groups, rows, fun); ry <- .scp_row_y(nr, grp); nrt <- ry$top - .5
  # laws, matched to rows
  lnames <- if (is.data.frame(laws)) NULL else names(laws)
  laws <- .scp_laws(laws, fun)
  if (!is.null(lnames) && all(rows %in% lnames)) laws <- laws[match(rows, lnames)]
  else if (length(laws) == nrow(means)) laws <- laws[match(rows, means$name)]   # in the order of means
  else if (length(laws) != nr)
    stop(sprintf("sc_plot_preferences(): %d laws for %d rows; give one law per row (or per row of means), or name them by target",
                 length(laws), nr), call. = FALSE)
  m <- means[match(rows, means$name), , drop = FALSE]
  s <- .scp_match(shares, rows, "shares", fun)
  a <- if (is.null(ames)) NULL else .scp_match(ames, rows, "ames", fun)
  ref <- if (is.null(reference)) NULL else .scp_reference(reference, rows, fun)
  if (!is.null(ref) && is.null(a)) stop("sc_plot_preferences(): reference values go in Panel D, which needs ames", call. = FALSE)
  for (nm in c("xlim", "ticks", "titles")) {
    v <- get(nm)
    if (!is.null(v) && (!is.list(v) && !is.character(v) || is.null(names(v)) || !all(names(v) %in% c("A", "B", "C", "D"))))
      stop(sprintf("sc_plot_preferences(): %s must be a list named by panel (A, B, C, D)", nm), call. = FALSE)
  }
  panels <- c("A", "B", "C", if (!is.null(a)) "D")
  ttl <- c(A = "A. Mean\nutility", B = "B. Fitted\npart-worths", C = "C. Share\nfavoring (%)", D = "D. Choice\neffect (pp)")
  if (!is.null(titles)) ttl[names(titles)] <- unlist(titles)
  lim <- list(); tk <- list()
  auto <- list(
    A = function() .scp_pad(c(m$estimate, m$lower, m$upper)),
    B = function() range(pretty(.scp_laws_range(laws), n = 2)),
    C = function() c(0, 100),
    D = function() .scp_pad(100 * c(a$estimate, a$lower, a$upper, ref)))
  for (p in panels) {
    given <- !is.null(xlim[[p]])
    lim[[p]] <- if (given) .scp_check_lim(xlim[[p]], paste0("xlim$", p), fun) else auto[[p]]()
    if (!given && p %in% c("A", "D")) lim[[p]] <- .scp_axis(lim[[p]], n = 3)$lim
    tk[[p]] <- if (!is.null(ticks[[p]])) ticks[[p]]
               else if (p == "C" && !given) c(0, 50, 100)
               else if (p == "B" && !given) sort(unique(c(lim[[p]], if (lim[[p]][1L] < 0 && lim[[p]][2L] > 0) 0)))
               else { t3 <- .scp_ticks(lim[[p]], n = 3); if (lim[[p]][1L] < 0 && lim[[p]][2L] > 0) sort(unique(c(t3, 0))) else t3 }
    # Automatic limits: pull the end ticks in from the panel edges, so the end labels of
    # neighbouring panels do not run into each other.
    if (!given) lim[[p]] <- lim[[p]] + c(-1, 1) * .07 * diff(lim[[p]])
  }
  rel <- if (!is.null(widths)) widths else c(2.3, 1.45, 1.65, 1.6, if (!is.null(a)) 1.75)
  if (!is.numeric(rel) || length(rel) != length(panels) + 1L || !all(is.finite(rel) & rel > 0))
    stop(sprintf("sc_plot_preferences(): widths needs %d positive numbers (labels and %d panels)",
                 length(panels) + 1L, length(panels)), call. = FALSE)
  op <- .scp_page_keep(); on.exit(.scp_page_restore(op))
  .scp_paper_par()
  need <- max(graphics::strwidth(labs, "inches", cex = .93)) + .25
  if (!is.null(grp)) need <- need + max(graphics::strwidth(grp$runs$label, "inches", cex = .88)) + .25
  have <- rel[1L] / sum(rel) * graphics::par("din")[1L]
  if (is.null(widths) && need > have) {
    lw <- graphics::lcm(2.54 * need); graphics::layout(matrix(seq_len(length(panels) + 1L), 1), widths = c(lw, rel[-1L]))
  } else graphics::layout(matrix(seq_len(length(panels) + 1L), 1), widths = rel)
  yy <- ry$y
  # labels
  graphics::par(mar = c(2.8, 0, 4, 0))
  graphics::plot(NA, xlim = c(0, 1), ylim = c(.5, nrt + .5), axes = FALSE, xlab = "", ylab = "")
  graphics::text(.97, yy, labs, adj = 1, cex = .93)
  if (!is.null(grp)) {   # left-aligned in a column just left of the longest row label
    per_in <- diff(graphics::par("usr")[1:2]) / graphics::par("pin")[1L]
    xg <- .97 - max(graphics::strwidth(labs, "user", cex = .93)) - .25 * per_in -
      max(graphics::strwidth(grp$runs$label, "user", cex = .88))
    .scp_group_labels(grp, yy, inset = max(.02, xg) * graphics::par("fin")[1L])
  }
  # A
  .scp_panel(lim$A, nrt, ttl[["A"]], tk$A)
  shownA <- .scp_shown(m$lower, m$upper, FALSE)
  .scp_ci(m$estimate, m$lower, m$upper, yy, shownA, .scp_blue)
  # B
  .scp_panel(lim$B, nrt, ttl[["B"]], tk$B)
  run <- if (is.null(grp)) rep(1L, nr) else rep(seq_len(nrow(grp$runs)), grp$runs$last - grp$runs$first + 1L)
  for (j in seq_len(nr)) .scp_ridge(laws[[j]], yy[j], lim$B, fill_alpha = if (run[j] %% 2L == 1L) .20 else .32)
  if (lim$B[1L] <= 0 && lim$B[2L] >= 0) graphics::abline(v = 0, col = .scp_zero, lty = 2, lwd = .7)   # over the fills
  # C
  .scp_panel(lim$C, nrt, ttl[["C"]], tk$C)
  shownC <- .scp_shown(s$lower, s$upper, hide_wide)
  .scp_ci(100 * s$estimate, 100 * s$lower, 100 * s$upper, yy, shownC, .scp_blue)
  out <- data.frame(name = rows, label = if (is.character(labs)) labs else rows, y = yy,
                    mean = m$estimate, mean_lower = m$lower, mean_upper = m$upper,
                    share = 100 * s$estimate, share_lower = 100 * s$lower, share_upper = 100 * s$upper,
                    share_shown = shownC, stringsAsFactors = FALSE)
  if (!is.null(grp)) out <- cbind(out[1:3], group = grp$group, out[-(1:3)], stringsAsFactors = FALSE)
  # D
  if (!is.null(a)) {
    .scp_panel(lim$D, nrt, ttl[["D"]], tk$D)
    shownD <- .scp_shown(a$lower, a$upper, FALSE)
    ya <- if (is.null(ref)) yy else yy + .09
    .scp_ci(100 * a$estimate, 100 * a$lower, 100 * a$upper, ya, shownD, .scp_blue)
    if (!is.null(ref)) graphics::points(100 * ref, yy - .13, pch = 18, col = "#555555", cex = .85)
    out$ame <- 100 * a$estimate; out$ame_lower <- 100 * a$lower; out$ame_upper <- 100 * a$upper
    out$reference <- if (is.null(ref)) NA_real_ else 100 * ref
  }
  invisible(list(rows = out, xlim = lim, ticks = tk))
}

# ---------------------------------------------------------------------------------------------
# Importance

#' Plot attribute importance shares
#'
#' Horizontal bars of importance shares (percent, the value printed at each
#' bar's end), one row per attribute and one color per group. Each group is an
#' estimate table from [sc_estimate()] with [sc_importance()] targets, passed
#' as a named argument (the names label the legend).
#'
#' The attribute of a row is read from its target name: when every name in a
#' table starts with the same prefix followed by a dot (as with
#' `sc_importance(names = paste("All", attrs, sep = "."))`), that prefix and the
#' dot are removed ("All.party" becomes "party"); otherwise the name is used as
#' it is. Rows of different tables are matched by attribute.
#'
#' Each group's shares should sum to one; the function warns when they do not
#' (for example when a table holds only some attributes).
#'
#' @param ... One or more estimate tables of importance shares (or one list of
#'   them). Several tables must be named.
#' @param combine Optional named list that adds attributes into display groups,
#'   e.g. `list(policy = c("econ", "social"), party = "party")`. Every attribute
#'   must belong to one group.
#' @param labels Row labels: named by attribute (or by `combine` group), or one
#'   per row in order. Default the attribute names with a capital first letter.
#' @param colors Colors of the groups (default house blue, green, red, ...).
#' @param xlab Label of the horizontal axis.
#' @param legend If `TRUE` and there are several groups, a legend at the bottom.
#' @return Invisibly, the plotting data: a data frame with one row per group
#'   and attribute (`group`, `attribute`, `label`, `y`, `share` in percent).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   fn <- fit$data$data$feature_names
#'   g <- list(low = fn[1:4], middle = fn[5:8], high = fn[9:12], revenue = fn[13:16])
#'   all <- sc_estimate(fit, sc_importance(g, names = paste("All", names(g), sep = ".")))
#'   dem <- sc_estimate(fit, sc_importance(g, w = function(x) x$party == "Dem",
#'                                         names = paste("Dem", names(g), sep = ".")))
#'   sc_plot_importance(All = all, Democrats = dem,
#'                      combine = list(brackets = c("low", "middle", "high"), revenue = "revenue"))
#' }
#' @export
sc_plot_importance <- function(..., combine = NULL, labels = NULL, colors = NULL,
                               xlab = "Mean importance share (%)", legend = TRUE) {
  fun <- "sc_plot_importance"
  tabs <- .scp_tables(list(...), fun); k <- length(tabs); gn <- names(tabs)
  if (k > 1L && any(!nzchar(gn)))
    stop("sc_plot_importance(): name every table when passing several, e.g. sc_plot_importance(All = a, Dem = b)",
         call. = FALSE)
  if (k == 1L && !nzchar(gn)) gn <- "Importance share"
  for (g in seq_len(k)) {
    kd <- tabs[[g]]$kind
    if (!all(is.na(kd) | kd == "importance"))
      stop(sprintf("sc_plot_importance(): table %d has targets that are not importance shares (%s)", g,
                   paste(unique(tabs[[g]]$name[!is.na(kd) & kd != "importance"]), collapse = ", ")), call. = FALSE)
    tabs[[g]]$attribute <- .scp_attribute(tabs[[g]]$name)
    tot <- sum(tabs[[g]]$estimate)
    if (abs(tot - 1) > 1e-6)
      warning(sprintf("sc_plot_importance(): the shares of %s sum to %.4f, not 1; is an attribute missing?",
                      if (k == 1L) "the table" else paste0("'", gn[g], "'"), tot), call. = FALSE)
  }
  attrs <- unique(unlist(lapply(tabs, `[[`, "attribute"), use.names = FALSE))
  if (!is.null(combine)) {
    if (!is.list(combine) || is.null(names(combine)) || any(!nzchar(names(combine))))
      stop("sc_plot_importance(): combine must be a named list of attribute names", call. = FALSE)
    used <- unlist(combine, use.names = FALSE)
    bad <- setdiff(used, attrs)
    if (length(bad)) stop(sprintf("sc_plot_importance(): combine names attributes the tables lack: %s (attributes: %s)",
                                  paste(bad, collapse = ", "), paste(attrs, collapse = ", ")), call. = FALSE)
    miss <- setdiff(attrs, used)
    if (length(miss)) stop(sprintf("sc_plot_importance(): attributes in no group of combine: %s", paste(miss, collapse = ", ")),
                           call. = FALSE)
    if (anyDuplicated(used)) stop("sc_plot_importance(): an attribute is in two groups of combine", call. = FALSE)
    keys <- names(combine)
    val <- sapply(tabs, function(x) vapply(combine, function(a) sum(x$estimate[x$attribute %in% a]), 0))
  } else {
    keys <- attrs
    val <- sapply(tabs, function(x) x$estimate[match(attrs, x$attribute)])
  }
  val <- matrix(val, length(keys), k)
  labs <- if (is.null(labels)) paste0(toupper(substr(keys, 1L, 1L)), substring(keys, 2L)) else .scp_labels(labels, keys, fun)
  colors <- .scp_colors(colors, k, fun)
  nr <- length(keys); yy <- rev(seq_len(nr))
  off <- if (k == 1L) 0 else seq(min(.3, .1 * (k - 1)), -min(.3, .1 * (k - 1)), length.out = k)
  half <- if (k == 1L) .15 else min(.075, .9 * abs(off[1L] - off[2L]) / 2)
  pv <- 100 * val
  hi <- max(pv, 0, na.rm = TRUE); lo <- min(pv, 0, na.rm = TRUE)
  xl <- c(if (lo < 0) 10 * floor((lo - 5) / 10) else 0, 10 * ceiling((hi + 5) / 10))
  ticks <- seq(xl[1L], xl[2L], by = if (diff(xl) > 60) 20 else 10)
  show_leg <- isTRUE(legend) && k > 1L
  op <- .scp_par_keep(); on.exit(graphics::par(op))
  .scp_paper_par()
  left <- max(graphics::strwidth(labs, "inches", cex = .95)) + .25
  .scp_set_margins(3 + 1.8 * show_leg, left, .7, 1.1)
  graphics::plot(NA, xlim = xl, ylim = c(.5, nr + .5), axes = FALSE, xlab = xlab, ylab = "")
  graphics::abline(v = ticks, col = .scp_grid, lwd = .6)
  graphics::axis(1, at = ticks)
  graphics::axis(2, at = yy, labels = labs, tick = FALSE, cex.axis = .95)
  out <- list()
  for (g in seq_len(k)) {
    y <- yy + off[g]; s <- pv[, g]; ok <- is.finite(s)
    graphics::rect(0, y[ok] - half, s[ok], y[ok] + half, col = colors[g], border = NA)
    graphics::text(s[ok], y[ok], sprintf("%.1f", s[ok]), pos = ifelse(s[ok] < 0, 2, 4), offset = .55,
                   col = colors[g], cex = .78)
    out[[g]] <- data.frame(group = gn[g], attribute = keys, label = if (is.character(labs)) labs else keys,
                           y = y, share = s, stringsAsFactors = FALSE)
  }
  if (lo < 0) graphics::abline(v = 0, col = .scp_zero, lty = 2, lwd = .7)
  if (show_leg) .scp_bottom_legend(legend = gn, fill = colors, border = NA)
  invisible(do.call(rbind, out))
}

# Attribute of each importance target: strip a prefix shared by every name and followed by a dot.
.scp_attribute <- function(x) {
  if (!length(x) || !all(grepl(".", x, fixed = TRUE))) return(x)
  pre <- sub("\\..*$", "", x)
  if (length(unique(pre)) == 1L) sub("^[^.]*\\.", "", x) else x
}

# ---------------------------------------------------------------------------------------------
# Estimates along an ordered setting

#' Plot estimates along an ordered setting
#'
#' Estimates against an ordered setting (for example a policy position or an
#' income bracket): one line with points per table and a shaded band for the
#' 95% intervals, one legend at the bottom. Each table holds one row per
#' setting, in the order of `x`. Points are hollow where an interval is
#' missing. The band covers each run of two or more neighbouring settings with
#' intervals; an interval whose neighbours have none (or the only setting) is
#' drawn as a vertical line. One call draws one panel, so a figure with several
#' panels loops over `par(mfrow = )` (pass `legend = FALSE` to all but one
#' panel).
#'
#' @param ... One or more estimate tables from [sc_estimate()] (or one list of
#'   them), each with one row per value of `x`. Several tables must be named.
#'   Series are kept apart by their position, so two tables may share a name
#'   (each keeps its own line, color and legend entry).
#' @param x Numeric settings, one per row of each table.
#' @param xlab Label of the horizontal axis (required: say what `x` is).
#' @param ylab Label of the vertical axis. Default from the target kinds.
#' @param scale Multiplier of estimates and intervals (100 for percent).
#' @param colors Colors of the series (default house blue, red, green, ...).
#' @param ylim Optional vertical limits, on the scaled values. Default 0 to
#'   `scale` for probabilities and shares, else from the data.
#' @param main Optional panel title.
#' @param xlabels Optional tick labels at `x`.
#' @param hline Optional reference value on the unscaled scale, drawn as a
#'   dashed line at `scale * hline`; default 0.5 for probabilities and shares,
#'   else 0.
#' @param legend If `TRUE` and there are several series, a legend at the
#'   bottom of the panel.
#' @return Invisibly, the plotting data: a data frame with one row per series
#'   and setting (`series`, `x`, scaled `estimate`, `lower`, `upper`).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   fn <- fit$data$data$feature_names
#'   d <- lapply(1:3, function(s) setNames(s * (fn == "tax6_linear"), fn))
#'   all <- sc_estimate(fit, lapply(1:3, function(s) sc_contest(paste("All", s), d[[s]])))
#'   dem <- sc_estimate(fit, lapply(1:3, function(s) sc_contest(paste("Dem", s), d[[s]],
#'                                                              w = function(x) x$party == "Dem")))
#'   sc_plot_lines(All = all, Democrats = dem, x = 1:3, scale = 100,
#'                 xlab = "Top-rate increase (points)", ylab = "Expected vote share (%)")
#' }
#' @export
sc_plot_lines <- function(..., x, xlab, ylab, scale = 1, colors = NULL, ylim = NULL, main = NULL,
                          xlabels = NULL, hline = NULL, legend = TRUE) {
  fun <- "sc_plot_lines"
  tabs <- .scp_tables(list(...), fun); k <- length(tabs); gn <- names(tabs)
  if (k > 1L && any(!nzchar(gn)))
    stop("sc_plot_lines(): name every table when passing several, e.g. sc_plot_lines(Clean = a, Violation = b, x = 1:4)",
         call. = FALSE)
  if (k == 1L && !nzchar(gn)) gn <- "Estimate"
  if (missing(x) || !is.numeric(x) || !all(is.finite(x)))
    stop("sc_plot_lines(): x must be finite numbers, one per row of each table", call. = FALSE)
  if (missing(xlab) || !length(xlab)) stop("sc_plot_lines(): give xlab, saying what x measures", call. = FALSE)
  for (g in seq_len(k)) if (nrow(tabs[[g]]) != length(x))
    stop(sprintf("sc_plot_lines(): table %d has %d rows for %d values of x", g, nrow(tabs[[g]]), length(x)), call. = FALSE)
  if (!is.null(xlabels) && length(xlabels) != length(x))
    stop("sc_plot_lines(): xlabels needs one label per value of x", call. = FALSE)
  scale <- .scp_check_scale(scale, fun); ylim <- .scp_check_lim(ylim, "ylim", fun)
  kinds <- unlist(lapply(tabs, `[[`, "kind"))
  if (missing(ylab)) ylab <- .scp_default_label(kinds, scale)
  prob <- length(kinds[!is.na(kinds)]) && all(kinds[!is.na(kinds)] %in% c("choice", "share", "decisiveness"))
  if (is.null(hline)) hline <- if (prob) .5 else 0
  if (is.null(colors) && k > length(.scp_palette))
    stop(sprintf("sc_plot_lines(): %d series need %d colors; pass colors", k, k), call. = FALSE)
  colors <- if (is.null(colors)) .scp_palette[c(1L, 3L, 2L, 4L, 5L, 6L)][seq_len(k)] else .scp_colors(colors, k, fun)
  # one data frame per series, kept apart by position (series names are only labels and may repeat)
  pdl <- lapply(seq_len(k), function(g) {
    z <- tabs[[g]]
    data.frame(series = gn[g], x = x, estimate = scale * z$estimate, lower = scale * z$lower, upper = scale * z$upper,
               stringsAsFactors = FALSE)
  })
  pd <- do.call(rbind, pdl)
  if (is.null(ylim)) ylim <- if (prob) c(0, scale)
                             else .scp_axis(.scp_pad(c(pd$estimate, pd$lower, pd$upper, scale * hline), zero = FALSE), n = 4)$lim
  yt <- .scp_ticks(ylim, n = 4)
  show_leg <- isTRUE(legend) && k > 1L
  op <- .scp_par_keep(); on.exit(graphics::par(op))
  graphics::par(las = 1, tcl = -.2, mgp = c(2.2, .45, 0), xaxs = "r", yaxs = "i",
                mar = c(3.2 + 1.6 * show_leg, 3.6, if (is.null(main)) 1 else 2.6, .8))
  graphics::plot(NA, xlim = range(x), ylim = ylim, xlab = xlab, ylab = ylab, axes = FALSE)
  graphics::abline(h = yt, col = .scp_grid, lwd = .6)
  if (is.finite(hline) && scale * hline >= ylim[1L] && scale * hline <= ylim[2L])
    graphics::abline(h = scale * hline, lty = 2, col = "#777777", lwd = .8)
  ux <- sort(unique(x))
  if (!is.null(xlabels)) graphics::axis(1, at = x, labels = xlabels, cex.axis = .9)
  else graphics::axis(1, at = if (length(ux) <= 12L) ux else pretty(x), cex.axis = .9)
  graphics::axis(2, at = yt, cex.axis = .9)
  if (!is.null(main)) graphics::title(main, line = .8, cex.main = 1)
  ox <- order(x)
  for (g in seq_len(k)) {
    z <- pdl[[g]][ox, , drop = FALSE]
    ok <- is.finite(z$lower) & is.finite(z$upper)
    run <- cumsum(c(TRUE, diff(ok) != 0))
    for (r in unique(run[ok])) {
      i <- which(run == r)
      # a band over two or more neighbouring intervals; a lone interval (a band would have no
      # width) as a vertical line
      if (length(unique(z$x[i])) > 1L)
        graphics::polygon(c(z$x[i], rev(z$x[i])), c(z$lower[i], rev(z$upper[i])),
                          col = grDevices::adjustcolor(colors[g], .12), border = NA)
      else graphics::segments(z$x[i], z$lower[i], z$x[i], z$upper[i], col = colors[g], lwd = 1.2)
    }
    graphics::lines(z$x, z$estimate, col = colors[g], lwd = 1.5, lty = g)
    graphics::points(z$x, z$estimate, col = colors[g], pch = ifelse(ok, 16, 1), cex = .7)
  }
  if (show_leg) .scp_bottom_legend(legend = gn, col = colors, lty = seq_len(k), lwd = 1.5, pch = 16, pt.cex = .7)
  rownames(pd) <- NULL
  invisible(pd)
}

# ---------------------------------------------------------------------------------------------
# Posterior summaries

#' Plot respondents' posterior summaries
#'
#' For output of [sc_posterior()]. With `truth` (for simulations), a scatter
#' of each respondent's posterior mean against the true value, with a dashed
#' 45-degree line and the correlation in the top-left corner. Without
#' `truth`, a caterpillar plot: respondents sorted by posterior mean, each
#' with its 95% posterior interval.
#'
#' @param post Output of [sc_posterior()], or its `contrasts` or `beta` table.
#' @param truth Optional true values, one per respondent: named by respondent
#'   ID, or in the order of the posterior's rows.
#' @param which The contrast (or, without contrasts, the feature) to plot.
#'   Default the first.
#' @param xlab,ylab Axis labels (defaults describe the plot).
#' @param main Optional title.
#' @param color Color of the points.
#' @return Invisibly, the plotting data: a data frame with one row per
#'   respondent (`id`, `mean`, `lower`, `upper`, and `truth` or the sorted
#'   position `rank`).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   post <- sc_posterior(fit, contrasts = c(tax6_linear = 1), nodes = 256)
#'   sc_plot_posterior(post, ylab = "Posterior taste for a higher top rate")
#' }
#' @export
sc_plot_posterior <- function(post, truth = NULL, which = NULL, xlab = NULL, ylab = NULL, main = NULL,
                              color = .scp_blue) {
  fun <- "sc_plot_posterior"
  if (inherits(post, "sc_posterior")) {
    tab <- if (!is.null(post$contrasts)) post$contrasts else post$beta
  } else if (is.data.frame(post) && all(c("id", "mean") %in% names(post))) {
    tab <- post
  } else stop("sc_plot_posterior(): post must be output of sc_posterior() or its contrasts or beta table", call. = FALSE)
  tab <- as.data.frame(tab)
  key <- if ("contrast" %in% names(tab)) "contrast" else if ("feature" %in% names(tab)) "feature" else NULL
  if (!is.null(key)) {
    avail <- unique(tab[[key]])
    if (is.null(which)) which <- avail[1L]
    if (!which %in% avail)
      stop(sprintf("sc_plot_posterior(): which = '%s' is not among the %ss: %s", which, key, paste(avail, collapse = ", ")),
           call. = FALSE)
    tab <- tab[tab[[key]] == which, , drop = FALSE]
  }
  if (is.null(tab$lower) || is.null(tab$upper)) {
    s <- if (is.null(tab$sd)) NA_real_ else tab$sd
    tab$lower <- tab$mean - 1.96 * s; tab$upper <- tab$mean + 1.96 * s
  }
  what <- if (is.null(which)) "the taste" else which
  if (!is.null(truth)) {
    if (!is.numeric(truth)) stop("sc_plot_posterior(): truth must be numeric", call. = FALSE)
    tv <- if (!is.null(names(truth))) unname(truth[match(as.character(tab$id), names(truth))])
          else if (length(truth) == nrow(tab)) truth
          else stop(sprintf("sc_plot_posterior(): truth has %d values for %d respondents; name it by respondent ID",
                            length(truth), nrow(tab)), call. = FALSE)
    if (all(is.na(tv))) stop("sc_plot_posterior(): no name of truth matches a respondent ID", call. = FALSE)
  }
  op <- .scp_par_keep(); on.exit(graphics::par(op))
  graphics::par(las = 1, tcl = -.2, mgp = c(2.2, .45, 0), mar = c(3.4, 3.6, if (is.null(main)) 1 else 2.4, .8))
  if (!is.null(truth)) {
    lim <- range(pretty(range(tv, tab$mean, na.rm = TRUE)))
    graphics::plot(tv, tab$mean, pch = 1, cex = .5, col = grDevices::adjustcolor(color, .6), xlim = lim, ylim = lim,
                   xlab = if (is.null(xlab)) paste("True value of", what) else xlab,
                   ylab = if (is.null(ylab)) "Posterior mean" else ylab, axes = FALSE)
    graphics::axis(1, cex.axis = .9); graphics::axis(2, cex.axis = .9); graphics::box(col = "#888888")
    graphics::abline(0, 1, lty = 2, col = "grey40")
    r <- stats::cor(tv, tab$mean, use = "complete.obs")
    graphics::legend("topleft", bty = "n", legend = sprintf("Corr = %.2f", r))
    if (!is.null(main)) graphics::title(main, line = .8, cex.main = 1, font.main = 1)
    return(invisible(data.frame(id = tab$id, mean = tab$mean, lower = tab$lower, upper = tab$upper, truth = tv,
                                stringsAsFactors = FALSE)))
  }
  o <- order(tab$mean); z <- tab[o, , drop = FALSE]; n <- nrow(z); rk <- seq_len(n)
  ok <- is.finite(z$lower) & is.finite(z$upper)
  ylim <- .scp_axis(.scp_pad(c(z$mean, z$lower[ok], z$upper[ok]), pad = .04))$lim
  graphics::plot(NA, xlim = c(.5, n + .5), ylim = ylim, axes = FALSE,
                 xlab = if (is.null(xlab)) "Respondents, sorted by posterior mean" else xlab,
                 ylab = if (is.null(ylab)) paste("Posterior mean of", what) else ylab)
  graphics::abline(h = .scp_ticks(ylim), col = .scp_grid, lwd = .6)
  if (ylim[1L] <= 0 && ylim[2L] >= 0) graphics::abline(h = 0, col = .scp_zero, lty = 2, lwd = .7)
  graphics::axis(1, at = unique(round(pretty(c(1, n)))), cex.axis = .9); graphics::axis(2, at = .scp_ticks(ylim), cex.axis = .9)
  a <- if (n > 200) .25 else .6
  graphics::segments(rk[ok], z$lower[ok], rk[ok], z$upper[ok], col = grDevices::adjustcolor(color, a), lwd = if (n > 200) .5 else 1)
  graphics::points(rk, z$mean, pch = ifelse(ok, 16, 1), cex = if (n > 200) .25 else .55, col = color)
  if (!is.null(main)) graphics::title(main, line = .8, cex.main = 1, font.main = 1)
  invisible(data.frame(id = z$id, mean = z$mean, lower = z$lower, upper = z$upper, rank = rk, stringsAsFactors = FALSE))
}

# ---------------------------------------------------------------------------------------------
# Tables

#' A publication table of estimates
#'
#' One row per target and one column per estimate table. A cell holds the
#' estimate and its 95% interval, "0.81 \[0.70, 0.92\]" (`style = "ci"`), or the
#' estimate and its standard error, "0.81 (0.06)" (`style = "se"`). A target
#' with no interval (or no standard error), such as an importance share, shows
#' the estimate alone (a row whose `interval` is `"none"` shows no standard
#' error either); a target a table lacks gives an empty cell.
#'
#' `format = "markdown"` returns a pipe table as one string, which prints as a
#' table in a knitr or Quarto chunk with the option `results: asis`
#' (`#| results: asis`); `"latex"` returns a booktabs `tabular` (wrapped in a
#' `table` with the caption when one is given), with `%`, `&`, `_`, `#` and `$`
#' escaped in labels and headers. Both print as plain text.
#'
#' @param ... One or more estimate tables from [sc_estimate()] (or one list of
#'   them). Names become the column headers; a single unnamed table gets one
#'   column, "Estimate", and several unnamed tables "(1)", "(2)", ....
#' @param digits Decimal places.
#' @param scale Multiplier of estimates, intervals and standard errors (100
#'   for percent).
#' @param style `"ci"` or `"se"`.
#' @param labels Row labels: named by target name, or one per row in order.
#' @param order Optional target names: the rows shown, in order.
#' @param format `"data.frame"`, `"markdown"` or `"latex"`.
#' @param caption Optional caption (markdown and LaTeX).
#' @param n If `TRUE`, a last row "Respondents" gives the number of
#'   respondents counted in each column (a range when targets differ).
#' @return For `"data.frame"`, a data frame with a `Target` column and one
#'   character column per table. Otherwise a character string of class
#'   `sc_table_text` (printed with `cat()`).
#' @examples
#' fit <- sc_load(system.file("extdata", "toy_fit.rds", package = "sconjoint"))
#' if (!is.null(fit)) {
#'   est <- sc_estimate(fit, list(sc_mean("Top bracket", "tax6_linear"),
#'                                sc_share("Share favoring", "tax6_linear")), bounded = FALSE)
#'   sc_table(est)
#'   sc_table(Model = est, format = "markdown", n = TRUE)
#'   sc_table(est, style = "se", format = "latex", caption = "Estimates")
#' }
#' @export
sc_table <- function(..., digits = 2, scale = 1, style = c("ci", "se"), labels = NULL, order = NULL,
                     format = c("data.frame", "markdown", "latex"), caption = NULL, n = FALSE) {
  fun <- "sc_table"
  style <- match.arg(style); format <- match.arg(format)
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0 || digits != round(digits))
    stop("sc_table(): digits must be one whole number >= 0", call. = FALSE)
  scale <- .scp_check_scale(scale, fun)
  tabs <- .scp_tables(list(...), fun); k <- length(tabs); gn <- names(tabs)
  if (k == 1L && !nzchar(gn)) gn <- "Estimate"
  if (k > 1L && any(!nzchar(gn))) gn[!nzchar(gn)] <- sprintf("(%d)", which(!nzchar(gn)))
  if (anyDuplicated(gn)) stop("sc_table(): column names must differ", call. = FALSE)
  rows <- .scp_rows(tabs, order, fun); labs <- as.character(.scp_labels(labels, rows, fun))
  f <- function(v) {
    v <- round(v * scale, digits); v[!is.na(v) & v == 0] <- 0
    ifelse(is.na(v), NA_character_, formatC(v, format = "f", digits = digits))
  }
  cells <- sapply(tabs, function(x) {
    i <- match(rows, x$name)
    e <- f(x$estimate[i])
    if (!is.null(x$interval)) x$se[!is.na(x$interval) & x$interval == "none"] <- NA_real_
    extra <- if (style == "ci") {
      ifelse(is.finite(x$lower[i]) & is.finite(x$upper[i]), paste0(" [", f(x$lower[i]), ", ", f(x$upper[i]), "]"), "")
    } else ifelse(is.finite(x$se[i]), paste0(" (", f(x$se[i]), ")"), "")
    ifelse(is.na(e), "", paste0(e, extra))
  })
  cells <- matrix(cells, length(rows), k)
  out <- data.frame(Target = labs, cells, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- c("Target", gn)
  if (isTRUE(n)) {
    nn <- vapply(tabs, function(x) {
      v <- x$n[match(rows, x$name)]; v <- v[!is.na(v)]
      if (!length(v)) return("")
      r <- format(range(v), big.mark = ",", trim = TRUE)
      if (r[1L] == r[2L]) r[1L] else paste0(r[1L], "-", r[2L])
    }, "")
    out <- rbind(out, stats::setNames(as.data.frame(as.list(c("Respondents", nn)), stringsAsFactors = FALSE), names(out)))
  }
  rownames(out) <- NULL
  if (format == "data.frame") return(out)
  if (format == "markdown") {
    esc <- function(s) gsub("|", "\\|", s, fixed = TRUE)
    m <- as.matrix(out); m[] <- esc(m)
    line <- function(v) paste0("| ", paste(v, collapse = " | "), " |")
    txt <- c(line(esc(names(out))), paste0("|", paste(c(":---", rep("---:", k)), collapse = "|"), "|"),
             apply(m, 1L, line))
    if (!is.null(caption)) txt <- c(txt, "", paste0("Table: ", caption))
    return(structure(paste(txt, collapse = "\n"), class = "sc_table_text"))
  }
  esc <- function(s) gsub("([%&_#$])", "\\\\\\1", s)
  neg <- function(s) gsub("(^|[ \\[(])-([0-9])", "\\1$-$\\2", s)
  m <- as.matrix(out); m[, 1L] <- esc(m[, 1L]); m[, -1L] <- neg(m[, -1L])
  if (isTRUE(n)) m[nrow(m), -1L] <- gsub("-", "--", as.matrix(out)[nrow(m), -1L], fixed = TRUE)
  body <- apply(m, 1L, function(v) paste0(paste(v, collapse = " & "), " \\\\"))
  if (isTRUE(n)) body <- c(body[-length(body)], "\\midrule", body[length(body)])
  tabular <- c(paste0("\\begin{tabular}{l", strrep("c", k), "}"), "\\toprule",
               paste0(paste(esc(names(out)), collapse = " & "), " \\\\"), "\\midrule", body, "\\bottomrule",
               "\\end{tabular}")
  if (!is.null(caption))
    tabular <- c("\\begin{table}[!htbp]", "\\centering", paste0("\\caption{", esc(caption), "}"), tabular, "\\end{table}")
  structure(paste(tabular, collapse = "\n"), class = "sc_table_text")
}

#' @export
print.sc_table_text <- function(x, ...) {
  cat(unclass(x), "\n", sep = "")
  invisible(x)
}
