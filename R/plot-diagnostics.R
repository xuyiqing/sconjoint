## Diagnostic and summary plots for sc_fit objects.
##
## Four exported ggplot-returning functions that produce publication-
## quality summaries of conjoint estimation results.

## Silence R CMD check NOTE for ggplot2 non-standard evaluation
utils::globalVariables(c("ci_lo", "ci_hi", "dummy_name", "var_beta",
                         "significant", "neg_log_p", "direction", "group"))

# ============================================================================
# Internal helpers
# ============================================================================

#' Apply common theme overrides to a ggplot object
#' @keywords internal
#' @noRd
.sc_plot_theme <- function(p, title = NULL, xlab = NULL, ylab = NULL,
                           theme.bw = FALSE, gridOff = FALSE,
                           cex.main = NULL, cex.axis = NULL, cex.lab = NULL,
                           legendOff = FALSE, legend.pos = NULL,
                           xlim = NULL) {
  base <- 12
  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
  if (!is.null(xlab))  p <- p + ggplot2::xlab(xlab)
  if (!is.null(ylab))  p <- p + ggplot2::ylab(ylab)
  if (!is.null(xlim))  p <- p + ggplot2::xlim(xlim)

  if (isTRUE(theme.bw)) {
    p <- p + ggplot2::theme_bw(base_size = base)
  }

  theme_args <- list()
  if (isTRUE(gridOff)) {
    theme_args$panel.grid.major <- ggplot2::element_blank()
    theme_args$panel.grid.minor <- ggplot2::element_blank()
  }
  if (isTRUE(legendOff)) {
    theme_args$legend.position <- "none"
  } else if (!is.null(legend.pos)) {
    theme_args$legend.position <- legend.pos
  }
  if (!is.null(cex.main)) {
    theme_args$plot.title <- ggplot2::element_text(size = 14 * cex.main)
  }
  if (!is.null(cex.axis)) {
    theme_args$axis.text <- ggplot2::element_text(size = 11 * cex.axis)
  }
  if (!is.null(cex.lab)) {
    theme_args$axis.title <- ggplot2::element_text(size = 12 * cex.lab)
  }
  if (length(theme_args) > 0) {
    p <- p + do.call(ggplot2::theme, theme_args)
  }
  p
}

#' Rename factor levels in dummy_name using a named character vector
#' @keywords internal
#' @noRd
.sc_apply_labels <- function(df, labels_map) {
  if (is.null(labels_map)) return(df)
  lvls <- levels(df$dummy_name)
  new_lvls <- ifelse(lvls %in% names(labels_map), labels_map[lvls], lvls)
  levels(df$dummy_name) <- new_lvls
  df
}

# ============================================================================
# 1. AMCE coefficient plot
# ============================================================================

#' AMCE coefficient plot
#'
#' Horizontal point-range plot of the DML population-average estimates
#' \eqn{\hat\theta_k} with confidence intervals, one row per
#' attribute dummy.
#'
#' @param object An \code{sc_fit} object.
#' @param dummies Optional character vector of dummy names to include.
#'   If non-\code{NULL}, only these dummies are shown in the specified order.
#' @param labels Optional named character vector to rename dummies for display
#'   (e.g., \code{c(agendaprogressive = "Progressive Agenda")}).
#' @param level Confidence level for intervals (default 0.95).
#' @param color Point/line color (default \code{"#E41A1C"}).
#' @param size Size of the point-range line (default 0.4).
#' @param fatten Fatten multiplier for the point (default 2).
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param xlab X-axis label.  \code{NULL} for a sensible default.
#' @param ylab Y-axis label.  \code{NULL} keeps no label.
#' @param xlim Numeric vector of length 2 for x-axis limits.
#' @param theme.bw If \code{TRUE}, use \code{theme_bw} instead of
#'   \code{theme_minimal}.
#' @param gridOff If \code{TRUE}, remove grid lines.
#' @param cex.main Scaling factor for the plot title.
#' @param cex.axis Scaling factor for axis tick labels.
#' @param cex.lab Scaling factor for axis titles.
#' @param legendOff If \code{TRUE}, hide the legend.
#' @param legend.pos Legend position (e.g., \code{"bottom"}, \code{"right"}).
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_amce <- function(object, dummies = NULL, labels = NULL,
                      level = 0.95, color = "#E41A1C",
                      size = 0.4, fatten = 2,
                      title = NULL, xlab = NULL, ylab = NULL,
                      xlim = NULL, theme.bw = FALSE, gridOff = FALSE,
                      cex.main = NULL, cex.axis = NULL, cex.lab = NULL,
                      legendOff = FALSE, legend.pos = NULL, ...) {
  stopifnot(inherits(object, "sc_fit"))
  theta <- stats::coef(object)
  se <- sqrt(diag(stats::vcov(object)))
  q <- stats::qnorm(1 - (1 - level) / 2)
  df <- data.frame(
    dummy_name = factor(names(theta), levels = rev(names(theta))),
    estimate   = as.numeric(theta),
    ci_lo      = as.numeric(theta - q * se),
    ci_hi      = as.numeric(theta + q * se),
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  if (!is.null(dummies)) {
    df <- df[df$dummy_name %in% dummies, , drop = FALSE]
    df$dummy_name <- factor(df$dummy_name, levels = rev(dummies))
  }
  df <- .sc_apply_labels(df, labels)
  default_title <- "Population-average AMCE"
  default_xlab <- expression(hat(theta)[k] ~ "(logit scale)")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate,
                                         y = .data$dummy_name)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "gray50") +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
      color = color, size = size, fatten = fatten
    ) +
    ggplot2::labs(
      x = if (is.null(xlab)) default_xlab else xlab,
      y = ylab,
      title = if (is.null(title)) default_title else title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  .sc_plot_theme(p, title = NULL, xlab = NULL, ylab = NULL,
                 theme.bw = theme.bw, gridOff = gridOff,
                 cex.main = cex.main, cex.axis = cex.axis,
                 cex.lab = cex.lab, legendOff = legendOff,
                 legend.pos = legend.pos, xlim = xlim)
}


# ============================================================================
# 2. Fraction favor / oppose
# ============================================================================

#' Fraction favor / oppose diverging bar chart
#'
#' Diverging horizontal bar chart showing the fraction of respondents
#' whose \eqn{\hat\beta_k(\mathbf Z_i)} is positive (favor) vs.\
#' negative (oppose) for each attribute dummy.
#'
#' @param object An \code{sc_fit} object.
#' @param dummies Optional character vector of dummy names to include.
#' @param labels Optional named character vector to rename dummies for display.
#' @param threshold Threshold for positive/negative classification
#'   (default 0).
#' @param colors Named character vector of length 2 for Favor and Oppose
#'   bars.  Defaults to \code{c(Favor = "#4393C3", Oppose = "#D6604D")}.
#' @param alpha Bar transparency (default 0.85).
#' @param bar.width Bar width (default 0.65).
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param xlab X-axis label.  \code{NULL} for a sensible default.
#' @param ylab Y-axis label.  \code{NULL} keeps no label.
#' @param xlim Numeric vector of length 2 for x-axis limits.
#' @param theme.bw If \code{TRUE}, use \code{theme_bw}.
#' @param gridOff If \code{TRUE}, remove grid lines.
#' @param cex.main Scaling factor for the plot title.
#' @param cex.axis Scaling factor for axis tick labels.
#' @param cex.lab Scaling factor for axis titles.
#' @param legendOff If \code{TRUE}, hide the legend.
#' @param legend.pos Legend position.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_fraction <- function(object, dummies = NULL, labels = NULL,
                          threshold = 0,
                          colors = c(Favor = "#4393C3", Oppose = "#D6604D"),
                          alpha = 0.85, bar.width = 0.65,
                          title = NULL, xlab = NULL, ylab = NULL,
                          xlim = NULL, theme.bw = FALSE, gridOff = FALSE,
                          cex.main = NULL, cex.axis = NULL, cex.lab = NULL,
                          legendOff = FALSE, legend.pos = NULL, ...) {
  stopifnot(inherits(object, "sc_fit"))
  frac <- sc_fraction_preferring(object, threshold = threshold)
  df <- frac$estimate
  if (!is.null(dummies)) {
    df <- df[df$dummy_name %in% dummies, , drop = FALSE]
    lvls <- rev(dummies)
  } else {
    lvls <- rev(df$dummy_name)
  }
  long <- rbind(
    data.frame(
      dummy_name = factor(df$dummy_name, levels = lvls),
      value      = df$frac_positive,
      direction  = "Favor",
      stringsAsFactors = FALSE
    ),
    data.frame(
      dummy_name = factor(df$dummy_name, levels = lvls),
      value      = -df$frac_negative,
      direction  = "Oppose",
      stringsAsFactors = FALSE
    )
  )
  long <- .sc_apply_labels(long, labels)
  default_title <- "Fraction favoring / opposing each level"
  default_xlab <- "Fraction of respondents"
  p <- ggplot2::ggplot(long,
                  ggplot2::aes(x = .data$value, y = dummy_name,
                               fill = direction)) +
    ggplot2::geom_col(width = bar.width, alpha = alpha) +
    ggplot2::geom_vline(xintercept = 0, color = "gray40", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(abs(round(x * 100)), "%"),
      breaks = seq(-1, 1, 0.25)
    ) +
    ggplot2::labs(x = if (is.null(xlab)) default_xlab else xlab,
                  y = ylab, fill = NULL,
                  title = if (is.null(title)) default_title else title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  .sc_plot_theme(p, title = NULL, xlab = NULL, ylab = NULL,
                 theme.bw = theme.bw, gridOff = gridOff,
                 cex.main = cex.main, cex.axis = cex.axis,
                 cex.lab = cex.lab, legendOff = legendOff,
                 legend.pos = legend.pos, xlim = xlim)
}


# ============================================================================
# 3. Heterogeneity bar chart
# ============================================================================

#' Preference heterogeneity bar chart
#'
#' Horizontal bar chart of \eqn{\mathrm{Var}(\hat\beta_k(\mathbf Z_i))}
#' per attribute dummy.  Bars are colored red when the heterogeneity
#' test rejects at the specified significance level.
#'
#' @param object An \code{sc_fit} object.
#' @param dummies Optional character vector of dummy names to include.
#' @param labels Optional named character vector to rename dummies for display.
#' @param alpha Significance level for coloring (default 0.05).
#' @param adjust P-value adjustment method passed to
#'   \code{\link{sc_heterogeneity_test}} (default \code{"bh"}).
#' @param gradient Named character vector of length 2 with \code{low} and
#'   \code{high} colors for the fill gradient (defaults to blue scale).
#' @param sig.color Color for the significance marker (default \code{"#B2182B"}).
#' @param sig.shape Shape for the significance marker (default 18).
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param xlab X-axis label.  \code{NULL} for a sensible default.
#' @param ylab Y-axis label.  \code{NULL} keeps no label.
#' @param xlim Numeric vector of length 2 for x-axis limits.
#' @param theme.bw If \code{TRUE}, use \code{theme_bw}.
#' @param gridOff If \code{TRUE}, remove grid lines.
#' @param cex.main Scaling factor for the plot title.
#' @param cex.axis Scaling factor for axis tick labels.
#' @param cex.lab Scaling factor for axis titles.
#' @param legendOff If \code{TRUE}, hide the legend.
#' @param legend.pos Legend position.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_hetero <- function(object, dummies = NULL, labels = NULL,
                        alpha = 0.05, adjust = "bh",
                        gradient = c(low = "#D1E5F0", high = "#2166AC"),
                        sig.color = "#B2182B", sig.shape = 18,
                        title = NULL, xlab = NULL, ylab = NULL,
                        xlim = NULL, theme.bw = FALSE, gridOff = FALSE,
                        cex.main = NULL, cex.axis = NULL, cex.lab = NULL,
                        legendOff = FALSE, legend.pos = NULL, ...) {
  stopifnot(inherits(object, "sc_fit"))
  het <- sc_heterogeneity_test(object, adjust = adjust)
  df <- het$estimate
  df$significant <- df$p_adjusted < alpha
  df$neg_log_p <- pmin(-log10(df$p_adjusted + 1e-300), 20)
  if (!is.null(dummies)) {
    df <- df[df$dummy_name %in% dummies, , drop = FALSE]
    df$dummy_name <- factor(df$dummy_name, levels = rev(dummies))
  } else {
    df$dummy_name <- factor(df$dummy_name, levels = rev(df$dummy_name))
  }
  df <- .sc_apply_labels(df, labels)
  default_title <- "Preference heterogeneity by attribute"
  default_xlab <- expression(Var(hat(beta)[k](Z)))
  p <- ggplot2::ggplot(df,
                  ggplot2::aes(x = var_beta, y = dummy_name)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$neg_log_p),
                      width = 0.65, alpha = 0.85) +
    ggplot2::geom_point(
      data = df[df$significant, , drop = FALSE],
      ggplot2::aes(x = var_beta, y = dummy_name),
      shape = sig.shape, size = 2.5, color = sig.color
    ) +
    ggplot2::scale_fill_gradient(
      low = gradient[["low"]], high = gradient[["high"]],
      name = expression(-log[10](p))
    ) +
    ggplot2::labs(
      x = if (is.null(xlab)) default_xlab else xlab,
      y = ylab,
      title = if (is.null(title)) default_title else title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.8, "cm")
    )
  .sc_plot_theme(p, title = NULL, xlab = NULL, ylab = NULL,
                 theme.bw = theme.bw, gridOff = gridOff,
                 cex.main = cex.main, cex.axis = cex.axis,
                 cex.lab = cex.lab, legendOff = legendOff,
                 legend.pos = legend.pos, xlim = xlim)
}


# ============================================================================
# 4. Subgroup AMCE comparison
# ============================================================================

#' Subgroup AMCE comparison plot
#'
#' Dodged horizontal point-range plot comparing subgroup-specific
#' \eqn{\hat\theta_k} across named groups.
#'
#' @param object An \code{sc_fit} object.
#' @param subgroup A **named list** of logical selectors, one per
#'   subgroup.  Example:
#'   \code{list(Female = Z[,"female"] > 0, Male = Z[,"female"] <= 0)}.
#' @param dummies Optional character vector of dummy names to include.
#'   If \code{NULL} (default), all dummies are plotted.
#' @param labels Optional named character vector to rename dummies for display.
#' @param colors Named character vector of colors, one per group name.
#'   If \code{NULL}, a default palette is used.
#' @param level Confidence level (default 0.95).
#' @param dodge.width Width for position dodging (default 0.6).
#' @param size Size of the point-range line (default 0.35).
#' @param fatten Fatten multiplier for the point (default 2).
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param xlab X-axis label.  \code{NULL} for a sensible default.
#' @param ylab Y-axis label.  \code{NULL} keeps no label.
#' @param xlim Numeric vector of length 2 for x-axis limits.
#' @param theme.bw If \code{TRUE}, use \code{theme_bw}.
#' @param gridOff If \code{TRUE}, remove grid lines.
#' @param cex.main Scaling factor for the plot title.
#' @param cex.axis Scaling factor for axis tick labels.
#' @param cex.lab Scaling factor for axis titles.
#' @param legendOff If \code{TRUE}, hide the legend.
#' @param legend.pos Legend position.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_subgroup <- function(object, subgroup, dummies = NULL, labels = NULL,
                          colors = NULL, level = 0.95,
                          dodge.width = 0.6, size = 0.35, fatten = 2,
                          title = NULL, xlab = NULL, ylab = NULL,
                          xlim = NULL, theme.bw = FALSE, gridOff = FALSE,
                          cex.main = NULL, cex.axis = NULL, cex.lab = NULL,
                          legendOff = FALSE, legend.pos = NULL, ...) {
  stopifnot(inherits(object, "sc_fit"))
  stopifnot(is.list(subgroup) && !is.null(names(subgroup)))
  sub_res <- sc_subgroup(object, subgroup)
  q <- stats::qnorm(1 - (1 - level) / 2)
  df <- do.call(rbind, lapply(names(sub_res), function(g) {
    e <- sub_res[[g]]$estimate
    data.frame(
      group      = g,
      dummy_name = e$dummy_name,
      theta      = e$theta,
      se         = e$se,
      ci_lo      = e$theta - q * e$se,
      ci_hi      = e$theta + q * e$se,
      stringsAsFactors = FALSE,
      row.names  = NULL
    )
  }))
  if (!is.null(dummies)) {
    df <- df[df$dummy_name %in% dummies, , drop = FALSE]
    df$dummy_name <- factor(df$dummy_name, levels = rev(dummies))
  } else {
    df$dummy_name <- factor(df$dummy_name,
                            levels = rev(unique(df$dummy_name)))
  }
  df <- .sc_apply_labels(df, labels)
  df$group <- factor(df$group, levels = names(subgroup))
  if (is.null(colors)) {
    ng <- length(subgroup)
    if (ng == 2L) {
      colors <- stats::setNames(c("#2166AC", "#B2182B"), names(subgroup))
    } else if (ng == 3L) {
      colors <- stats::setNames(c("#2166AC", "gray50", "#B2182B"),
                                names(subgroup))
    } else {
      pal <- if (requireNamespace("viridisLite", quietly = TRUE)) {
        viridisLite::viridis(ng)
      } else {
        grDevices::hcl.colors(ng, palette = "viridis")
      }
      colors <- stats::setNames(pal, names(subgroup))
    }
  }
  default_title <- "Subgroup AMCE comparison"
  default_xlab <- expression(hat(theta)[k] ~ "(logit scale)")
  p <- ggplot2::ggplot(df,
                  ggplot2::aes(x = .data$theta, y = dummy_name,
                               color = group)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "gray50") +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
      position = ggplot2::position_dodge(width = dodge.width),
      size = size, fatten = fatten
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      x = if (is.null(xlab)) default_xlab else xlab,
      y = ylab, color = NULL,
      title = if (is.null(title)) default_title else title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  .sc_plot_theme(p, title = NULL, xlab = NULL, ylab = NULL,
                 theme.bw = theme.bw, gridOff = gridOff,
                 cex.main = cex.main, cex.axis = cex.axis,
                 cex.lab = cex.lab, legendOff = legendOff,
                 legend.pos = legend.pos, xlim = xlim)
}
