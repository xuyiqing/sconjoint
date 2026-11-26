# Helpers for the paper's figures in the application chapters. The drawing code is a port of the
# paper's build_figures.R; it reads the data frames that sc_estimate() and sc_fitted_law() return.

# Colours: the house blue, and the paper's green and red for second and third groups.
blue <- "#004a8c"; green <- "#27805A"; red <- "#BD4C39"
cols <- c(blue, green, red)

# Estimate a list of targets one at a time in parallel processes and stack the rows. Each element
# of `targets` is one target or one target set (an importance set is updated jointly). Forked
# processes are not available on Windows, where this runs one target after another.
estimate_each <- function(object, targets, cores = min(8L, parallel::detectCores())) {
  if (.Platform$OS.type == "windows") cores <- 1L
  out <- parallel::mclapply(targets, function(t) sc_estimate(object, t),
                            mc.cores = cores, mc.preschedule = FALSE)
  bad <- vapply(out, inherits, logical(1), "try-error")
  if (any(bad)) stop(out[[which(bad)[1]]], call. = FALSE)
  do.call(rbind, out)
}

# Rows of an estimate table in a given order of names.
pick <- function(est, names) {
  i <- match(names, est$name)
  stopifnot(!anyNA(i))
  est[i, , drop = FALSE]
}

# Graphics settings of the paper's figures.
paper_par <- function() par(las = 1, tcl = -.2, mgp = c(1.7, .45, 0), xaxs = "i", yaxs = "i")

# An empty panel with light grid lines, a dashed line at zero and a bold title.
panel <- function(limits, rows, title, ticks, xlabel = "", margins = c(2.8, .4, 4, .6)) {
  par(mar = margins)
  plot(NA, xlim = limits, ylim = c(.5, rows + .5), axes = FALSE, xlab = xlabel, ylab = "")
  abline(v = ticks, col = "#E8E8E8", lwd = .6)
  abline(v = 0, col = "#888888", lty = 2, lwd = .7)
  axis(1, at = ticks, cex.axis = .9)
  mtext(title, 3, .7, cex = .92, font = 2)
}

# A column of row labels, first label at the top.
row_labels <- function(labs, margins = c(2.8, 0, 4, 0)) {
  par(mar = margins)
  plot(NA, xlim = c(0, 1), ylim = c(.5, length(labs) + .5), axes = FALSE, xlab = "", ylab = "")
  text(.97, rev(seq_along(labs)), labs, adj = 1, cex = .93)
}

# Points and 95% intervals from an estimate table. A point is hollow when its interval is missing
# or, with hide_wide = TRUE, when the interval spans at least 95 percentage points.
ci <- function(est, y, color = blue, scale = 1, hide_wide = FALSE, pch = 16) {
  good <- is.finite(est$lower) & is.finite(est$upper)
  if (hide_wide) good <- good & est$upper - est$lower < .95
  segments(scale * est$lower[good], y[good], scale * est$upper[good], y[good], col = color, lwd = 1.3)
  points(scale * est$estimate, y, pch = ifelse(good, pch, 1), col = color, cex = .8)
}

# Probability mass of a fitted law (a mixture of normals) in the bins between `edges`.
law_mass <- function(law, edges) {
  cdf <- vapply(edges, function(x) sum(law$weight * pnorm(x, law$mean, law$sd)) / sum(law$weight),
                numeric(1))
  diff(cdf)
}

# A fitted law drawn as a ridge (a step outline of its mass in 150 bins) centred on row y.
ridge <- function(law, y, limits, height = .63, color = blue) {
  edges <- seq(limits[1], limits[2], length.out = 151)
  h <- law_mass(law, edges); h <- height * h / max(h)
  x <- as.vector(rbind(head(edges, -1), tail(edges, -1)))
  polygon(c(limits[1], x, limits[2]), c(y - .3, y - .3 + rep(h, each = 2), y - .3),
          col = adjustcolor(color, .20), border = color, lwd = .7)
}

# One fitted law in its own panel, over its mean plus or minus three standard deviations (and
# zero). With an estimate row `mean`, a red line marks the score-corrected mean and a red bar its
# 95% interval.
small_density <- function(law, title, mean = NULL) {
  lim <- range(0, law$mean - 3 * law$sd, law$mean + 3 * law$sd)
  edges <- seq(lim[1], lim[2], length.out = 151)
  mass <- law_mass(law, edges)
  plot(NA, xlim = lim, ylim = c(0, 1.12 * max(mass)), xlab = "", ylab = "", yaxt = "n", bty = "n",
       main = title, cex.main = .95)
  abline(v = 0, col = "#888888", lty = 2)
  polygon(c(lim[1], as.vector(rbind(head(edges, -1), tail(edges, -1))), lim[2]),
          c(0, rep(mass, each = 2), 0), col = adjustcolor(blue, .22), border = blue)
  if (!is.null(mean)) {
    abline(v = mean$estimate, col = red, lwd = 1)
    segments(mean$lower, .95 * max(mass), mean$upper, .95 * max(mass), col = red, lwd = 1.4)
  }
}
