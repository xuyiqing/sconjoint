## Internal plot helpers for the `sc_fit` visualization methods.
##
## These return `ggplot` objects.  They are intentionally small
## wrappers — the user-facing entry point is `plot.sc_fit()` below.

## Silence R CMD check NOTE for ggridges computed aesthetic
utils::globalVariables("x")

#' Ridgeline plot of per-respondent beta(Z) distributions
#'
#' @param beta_hat N x p matrix of held-out beta(Z) rows.
#' @param attr_names Character p-vector of dummy names.
#' @param dummies Optional character vector of dummy names to include.
#' @param labels Optional named character vector to rename dummies for display.
#' @param title Plot title. \code{NULL} for a sensible default.
#' @param xlab X-axis label. \code{NULL} for a sensible default.
#' @param ylab Y-axis label. \code{NULL} keeps no label.
#' @param theme.bw If \code{TRUE}, use \code{theme_bw}.
#' @param gridOff If \code{TRUE}, remove grid lines.
#' @param cex.main Scaling factor for the plot title.
#' @param cex.axis Scaling factor for axis tick labels.
#' @param cex.lab Scaling factor for axis titles.
#' @param legendOff If \code{TRUE}, hide the legend.
#' @param legend.pos Legend position.
#' @param xlim Numeric vector of length 2 for x-axis limits.
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
.sc_plot_ridgelines <- function(beta_hat, attr_names, dummies = NULL,
                                labels = NULL, title = NULL, xlab = NULL,
                                ylab = NULL, theme.bw = FALSE,
                                gridOff = FALSE, cex.main = NULL,
                                cex.axis = NULL, cex.lab = NULL,
                                legendOff = NULL, legend.pos = NULL,
                                xlim = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(".sc_plot_ridgelines(): ggplot2 is required.")
  }
  ## Subset columns if dummies is specified
  if (!is.null(dummies)) {
    idx <- match(dummies, attr_names)
    idx <- idx[!is.na(idx)]
    beta_hat <- beta_hat[, idx, drop = FALSE]
    attr_names <- attr_names[idx]
  }
  p_cols <- ncol(beta_hat)
  long <- data.frame(
    dummy = factor(rep(attr_names, each = nrow(beta_hat)),
                   levels = rev(attr_names)),
    value = as.numeric(beta_hat),
    stringsAsFactors = FALSE
  )
  ## Apply labels
  if (!is.null(labels)) {
    lvls <- levels(long$dummy)
    new_lvls <- ifelse(lvls %in% names(labels), labels[lvls], lvls)
    levels(long$dummy) <- new_lvls
  }
  default_title <- "Per-respondent preference distributions"
  default_xlab <- expression(hat(beta)(Z))
  if (requireNamespace("ggridges", quietly = TRUE)) {
    pl <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$dummy,
                                       fill = ggplot2::after_stat(x))) +
      ggridges::geom_density_ridges_gradient(
        scale = 2.0, rel_min_height = 0.01,
        quantile_lines = TRUE, quantiles = 2
      ) +
      ggplot2::scale_fill_viridis_c(option = "C") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "gray50") +
      ggplot2::labs(x = if (is.null(xlab)) default_xlab else xlab,
                    y = ylab,
                    title = if (is.null(title)) default_title else title) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  } else {
    ## Fallback: boxplot if ggridges is unavailable.
    pl <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$dummy)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "gray50") +
      ggplot2::labs(x = if (is.null(xlab)) default_xlab else xlab,
                    y = ylab,
                    title = if (is.null(title)) default_title else title) +
      ggplot2::theme_minimal(base_size = 12)
  }
  ## Use the shared theme helper from plot-diagnostics.R
  if (exists(".sc_plot_theme", mode = "function")) {
    pl <- .sc_plot_theme(pl, title = NULL, xlab = NULL, ylab = NULL,
                         theme.bw = theme.bw, gridOff = gridOff,
                         cex.main = cex.main, cex.axis = cex.axis,
                         cex.lab = cex.lab,
                         legendOff = if (is.null(legendOff)) FALSE else legendOff,
                         legend.pos = legend.pos, xlim = xlim)
  }
  pl
}

#' Multi-line plot of per-fold training loss traces
#' @param loss_traces List of numeric vectors (one per fold).
#' @param title Plot title. \code{NULL} for a sensible default.
#' @param xlab X-axis label. \code{NULL} for a sensible default.
#' @param ylab Y-axis label. \code{NULL} for a sensible default.
#' @param theme.bw If \code{TRUE}, use \code{theme_bw}.
#' @param gridOff If \code{TRUE}, remove grid lines.
#' @param cex.main Scaling factor for the plot title.
#' @param cex.axis Scaling factor for axis tick labels.
#' @param cex.lab Scaling factor for axis titles.
#' @param legendOff If \code{TRUE}, hide the legend.
#' @param legend.pos Legend position.
#' @param xlim Numeric vector of length 2 for x-axis limits.
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
.sc_plot_loss_traces <- function(loss_traces, title = NULL, xlab = NULL,
                                 ylab = NULL, theme.bw = FALSE,
                                 gridOff = FALSE, cex.main = NULL,
                                 cex.axis = NULL, cex.lab = NULL,
                                 legendOff = FALSE, legend.pos = NULL,
                                 xlim = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(".sc_plot_loss_traces(): ggplot2 is required.")
  }
  dfs <- lapply(seq_along(loss_traces), function(k) {
    v <- loss_traces[[k]]
    data.frame(fold = factor(k), epoch = seq_along(v), loss = v)
  })
  long <- do.call(rbind, dfs)
  n_folds <- length(loss_traces)
  fold_cols <- if (requireNamespace("viridisLite", quietly = TRUE)) {
    viridisLite::viridis(n_folds)
  } else {
    grDevices::hcl.colors(n_folds, palette = "viridis")
  }
  default_title <- "Per-fold training loss trace"
  default_xlab <- "epoch"
  default_ylab <- "training loss"
  pl <- ggplot2::ggplot(long, ggplot2::aes(x = .data$epoch, y = .data$loss,
                                     colour = .data$fold,
                                     group = .data$fold)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = fold_cols) +
    ggplot2::labs(x = if (is.null(xlab)) default_xlab else xlab,
                  y = if (is.null(ylab)) default_ylab else ylab,
                  title = if (is.null(title)) default_title else title) +
    ggplot2::theme_minimal(base_size = 12)
  if (exists(".sc_plot_theme", mode = "function")) {
    pl <- .sc_plot_theme(pl, title = NULL, xlab = NULL, ylab = NULL,
                         theme.bw = theme.bw, gridOff = gridOff,
                         cex.main = cex.main, cex.axis = cex.axis,
                         cex.lab = cex.lab, legendOff = legendOff,
                         legend.pos = legend.pos, xlim = xlim)
  }
  pl
}
