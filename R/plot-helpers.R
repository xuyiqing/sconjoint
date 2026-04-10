## Internal plot helpers for the `sc_fit` visualization methods.
##
## These return `ggplot` objects.  They are intentionally small
## wrappers — the user-facing entry point is `plot.sc_fit()` below.

#' Ridgeline plot of per-respondent beta(Z) distributions
#'
#' @param beta_hat N x p matrix of held-out beta(Z) rows.
#' @param attr_names Character p-vector of dummy names.
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
.sc_plot_ridgelines <- function(beta_hat, attr_names) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(".sc_plot_ridgelines(): ggplot2 is required.")
  }
  p <- ncol(beta_hat)
  long <- data.frame(
    dummy = factor(rep(attr_names, each = nrow(beta_hat)), levels = rev(attr_names)),
    value = as.numeric(beta_hat),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("ggridges", quietly = TRUE)) {
    ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$dummy,
                                       fill = ggplot2::after_stat(x))) +
      ggridges::geom_density_ridges_gradient(
        scale = 2.0, rel_min_height = 0.01,
        quantile_lines = TRUE, quantiles = 2
      ) +
      ggplot2::scale_fill_viridis_c(option = "C") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "gray50") +
      ggplot2::labs(x = expression(hat(beta)(Z)), y = NULL,
                    title = "Per-respondent preference distributions") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  } else {
    ## Fallback: boxplot if ggridges is unavailable.
    ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$dummy)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "gray50") +
      ggplot2::labs(x = expression(hat(beta)(Z)), y = NULL) +
      ggplot2::theme_minimal(base_size = 12)
  }
}

#' Multi-line plot of per-fold training loss traces
#' @param loss_traces List of numeric vectors (one per fold).
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
.sc_plot_loss_traces <- function(loss_traces) {
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
  ggplot2::ggplot(long, ggplot2::aes(x = .data$epoch, y = .data$loss,
                                     colour = .data$fold, group = .data$fold)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = fold_cols) +
    ggplot2::labs(x = "epoch", y = "training loss",
                  title = "Per-fold training loss trace") +
    ggplot2::theme_minimal(base_size = 12)
}
