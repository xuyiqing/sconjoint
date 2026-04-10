## Diagnostic and summary plots for sc_fit objects.
##
## Four exported ggplot-returning functions that produce publication-
## quality summaries of conjoint estimation results.

## Silence R CMD check NOTE for ggplot2 non-standard evaluation
utils::globalVariables(c("ci_lo", "ci_hi", "dummy_name", "var_beta",
                         "significant", "direction", "group"))

# ============================================================================
# 1. AMCE coefficient plot
# ============================================================================

#' AMCE coefficient plot
#'
#' Horizontal point-range plot of the DML population-average estimates
#' \eqn{\hat\theta_k} with 95\% confidence intervals, one row per
#' attribute dummy.
#'
#' @param object An \code{sc_fit} object.
#' @param level Confidence level for intervals (default 0.95).
#' @param color Point/line color (default \code{"#E41A1C"}).
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_amce <- function(object, level = 0.95, color = "#E41A1C",
                      title = NULL, ...) {
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
  if (is.null(title)) title <- "Population-average AMCE"
  ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$dummy_name)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
      color = color, size = 0.4, fatten = 2
    ) +
    ggplot2::labs(
      x = expression(hat(theta)[k] ~ "(logit scale)"),
      y = NULL,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
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
#' @param threshold Threshold for positive/negative classification
#'   (default 0).
#' @param colors Named character vector of length 2 for Favor and Oppose
#'   bars.  Defaults to \code{c(Favor = "#377EB8", Oppose = "#E41A1C")}.
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_fraction <- function(object, threshold = 0,
                          colors = c(Favor = "#377EB8", Oppose = "#E41A1C"),
                          title = NULL, ...) {
  stopifnot(inherits(object, "sc_fit"))
  frac <- sc_fraction_preferring(object, threshold = threshold)
  df <- frac$estimate
  long <- rbind(
    data.frame(
      dummy_name = factor(df$dummy_name, levels = rev(df$dummy_name)),
      value      = df$frac_positive,
      direction  = "Favor",
      stringsAsFactors = FALSE
    ),
    data.frame(
      dummy_name = factor(df$dummy_name, levels = rev(df$dummy_name)),
      value      = -df$frac_negative,
      direction  = "Oppose",
      stringsAsFactors = FALSE
    )
  )
  if (is.null(title)) title <- "Fraction favoring / opposing each level"
  ggplot2::ggplot(long,
                  ggplot2::aes(x = .data$value, y = dummy_name,
                               fill = direction)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = "gray30", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(abs(round(x * 100)), "%"),
      breaks = seq(-1, 1, 0.25)
    ) +
    ggplot2::labs(x = "Fraction of respondents", y = NULL, fill = NULL,
                  title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
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
#' @param alpha Significance level for coloring (default 0.05).
#' @param adjust P-value adjustment method passed to
#'   \code{\link{sc_heterogeneity_test}} (default \code{"bh"}).
#' @param colors Named character vector of length 2.  Defaults to
#'   \code{c("TRUE" = "#E41A1C", "FALSE" = "gray70")}.
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_heterogeneity <- function(object, alpha = 0.05, adjust = "bh",
                               colors = c("TRUE" = "#E41A1C",
                                          "FALSE" = "gray70"),
                               title = NULL, ...) {
  stopifnot(inherits(object, "sc_fit"))
  het <- sc_heterogeneity_test(object, adjust = adjust)
  df <- het$estimate
  df$significant <- as.character(df$p_adjusted < alpha)
  df$dummy_name <- factor(df$dummy_name, levels = rev(df$dummy_name))
  if (is.null(title)) title <- "Preference heterogeneity by attribute"
  ggplot2::ggplot(df,
                  ggplot2::aes(x = var_beta, y = dummy_name,
                               fill = significant)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = colors,
      labels = c("TRUE" = paste0("p < ", alpha), "FALSE" = "n.s.")
    ) +
    ggplot2::labs(
      x = expression(Var(hat(beta)[k](Z))),
      y = NULL, fill = NULL,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
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
#' @param colors Named character vector of colors, one per group name.
#'   If \code{NULL}, a default palette is used.
#' @param level Confidence level (default 0.95).
#' @param title Plot title.  \code{NULL} for a sensible default.
#' @param ... Unused.
#' @return A \code{ggplot} object.
#' @export
plot_subgroup <- function(object, subgroup, dummies = NULL,
                          colors = NULL, level = 0.95,
                          title = NULL, ...) {
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
  }
  df$dummy_name <- factor(df$dummy_name,
                          levels = rev(unique(df$dummy_name)))
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
  if (is.null(title)) title <- "Subgroup AMCE comparison"
  ggplot2::ggplot(df,
                  ggplot2::aes(x = .data$theta, y = dummy_name,
                               color = group)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
      position = ggplot2::position_dodge(width = 0.6),
      size = 0.35, fatten = 2
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      x = expression(hat(theta)[k] ~ "(logit scale)"),
      y = NULL, color = NULL,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
