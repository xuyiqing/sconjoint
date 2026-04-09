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
    ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$dummy)) +
      ggridges::geom_density_ridges(alpha = 0.7, scale = 1.1) +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, alpha = 0.6) +
      ggplot2::labs(x = expression(hat(beta)(Z)), y = NULL,
                    title = "Per-respondent preference distributions") +
      ggplot2::theme_minimal()
  } else {
    ## Fallback: boxplot if ggridges is unavailable.
    ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$dummy)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::labs(x = expression(hat(beta)(Z)), y = NULL) +
      ggplot2::theme_minimal()
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
  ggplot2::ggplot(long, ggplot2::aes(x = .data$epoch, y = .data$loss,
                                     colour = .data$fold, group = .data$fold)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "epoch", y = "training loss",
                  title = "Per-fold training loss trace") +
    ggplot2::theme_minimal()
}
