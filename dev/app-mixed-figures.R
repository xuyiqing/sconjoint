# BEFORE/AFTER overlay figures for the two-stage vs mixed-logit
# application comparison.  Reads the .rds saved by
# dev/app-mixed-comparison.R; never refits (plots load cache).
suppressMessages({library(ggplot2)})

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/sconjoint/mixedlogit_prototype")
FIG_DIR <- file.path(OUT_DIR, "figs")
dir.create(FIG_DIR, showWarnings = FALSE)

ACCENT <- "#a8003b"   # cardinal red (house accent)
GRAY <- "grey35"

theta_overlay <- function(res, title) {
  nm <- res$attr_names
  df <- rbind(
    data.frame(attr = nm, est = unname(res$before$theta),
               se = unname(res$before$se), which = "Two-stage (projection)"),
    data.frame(attr = nm, est = res$after$theta,
               se = res$after$se, which = "Mixed logit (latent mean)")
  )
  df$attr <- factor(df$attr, levels = rev(nm))
  ggplot(df, aes(x = est, y = attr, color = which, shape = which)) +
    geom_vline(xintercept = 0, linewidth = 0.3, color = "grey70") +
    geom_pointrange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se),
                    orientation = "y", linewidth = 0.5, size = 0.45,
                    fill = "white", stroke = 0.9,
                    position = position_dodge(width = 0.55)) +
    scale_color_manual(values = c("Two-stage (projection)" = GRAY,
                                  "Mixed logit (latent mean)" = ACCENT)) +
    scale_shape_manual(values = c("Two-stage (projection)" = 21,
                                  "Mixed logit (latent mean)" = 19)) +
    labs(x = expression(theta[k] ~ "(logit scale)"), y = NULL,
         color = NULL, shape = NULL, title = title,
         subtitle = "Population-average preferences, 95% CIs") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
}

sign_overlay <- function(res, title) {
  nm <- res$attr_names
  ## Display truncation: pi is a probability; the additive correction can
  ## push a near-boundary estimate slightly outside [0,1]. The figure
  ## shows those at the boundary with a distinct marker; the raw values
  ## stay in the .rds.
  pi_raw <- res$after$pi
  pi_disp <- pmin(pmax(pi_raw, 0), 1)
  truncated <- !is.na(pi_raw) & (pi_raw > 1 | pi_raw < 0)
  floored <- is.na(pi_raw)
  df <- rbind(
    data.frame(attr = nm, est = unname(res$before$sign_share), se = NA,
               trunc = FALSE, which = "Two-stage MAP sign fraction"),
    data.frame(attr = nm, est = pi_disp, se = res$after$pi_se,
               trunc = truncated, which = "Mixed logit pi (debiased)")
  )
  df <- df[!is.na(df$est), , drop = FALSE]
  df$attr <- factor(df$attr, levels = rev(nm))
  n_tr <- sum(truncated)
  cap_parts <- c(
    if (n_tr > 0)
      sprintf("%d share%s outside [0, 1] drawn at the boundary (diamonds).",
              n_tr, if (n_tr > 1) "s" else ""),
    if (any(floored))
      sprintf("%d floored coordinates (residual SD below the floor) are reported as NA and shown for the two-stage fraction only.",
              sum(floored)))
  cap <- if (length(cap_parts)) paste(cap_parts, collapse = "\n") else NULL
  ggplot(df, aes(x = est, y = attr, color = which, shape = which)) +
    geom_vline(xintercept = 0.5, linewidth = 0.3, color = "grey70") +
    geom_pointrange(aes(xmin = ifelse(is.na(se), est, pmax(est - 1.96 * se, 0)),
                        xmax = ifelse(is.na(se), est, pmin(est + 1.96 * se, 1))),
                    orientation = "y", linewidth = 0.5, size = 0.45,
                    fill = "white", stroke = 0.9,
                    position = position_dodge(width = 0.55)) +
    geom_point(data = subset(df, trunc), shape = 18, size = 3.2,
               show.legend = FALSE) +
    scale_color_manual(values = c("Two-stage MAP sign fraction" = GRAY,
                                  "Mixed logit pi (debiased)" = ACCENT)) +
    scale_shape_manual(values = c("Two-stage MAP sign fraction" = 21,
                                  "Mixed logit pi (debiased)" = 19)) +
    coord_cartesian(xlim = c(0, 1), clip = "on") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = expression(Pr(beta[ik] > 0)), y = NULL, color = NULL,
         shape = NULL, title = title,
         subtitle = "Share of respondents favoring the attribute",
         caption = cap) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(size = 8, color = "grey30"))
}

resid_overlay <- function(res, title) {
  nm <- res$attr_names
  sp <- res$before$sigma_prior
  df <- rbind(
    data.frame(attr = nm, est = sqrt(unname(sp)),
               which = "Two-stage prior SD (heuristic)"),
    data.frame(attr = nm, est = res$after$sd_resid,
               which = "Mixed logit residual SD (estimated)")
  )
  df$attr <- factor(df$attr, levels = rev(nm))
  ggplot(df, aes(x = est, y = attr, color = which, shape = which)) +
    geom_point(size = 2.4, fill = "white", stroke = 0.9) +
    scale_color_manual(values = c("Two-stage prior SD (heuristic)" = GRAY,
                                  "Mixed logit residual SD (estimated)" = ACCENT)) +
    scale_shape_manual(values = c("Two-stage prior SD (heuristic)" = 21,
                                  "Mixed logit residual SD (estimated)" = 19)) +
    labs(x = "Residual (within-Z) SD of the coefficient", y = NULL,
         color = NULL, shape = NULL, title = title,
         subtitle = "Calibrated heuristic vs likelihood-estimated heterogeneity") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
}

for (app in c("br2017", "sw2022")) {
  f <- file.path(OUT_DIR, paste0("app_", app, ".rds"))
  if (!file.exists(f)) { cat("missing:", f, "\n"); next }
  res <- readRDS(f)
  lab <- c(br2017 = "Tax conjoint (Ballard-Rosa et al. 2017, T = 8)",
           sw2022 = "Candidate conjoint (Saha-Weeks 2022, T = 3)")[app]
  ggsave(file.path(FIG_DIR, paste0(app, "_theta.png")),
         theta_overlay(res, lab), width = 7.5,
         height = 1.4 + 0.34 * length(res$attr_names), dpi = 200)
  ggsave(file.path(FIG_DIR, paste0(app, "_signshare.png")),
         sign_overlay(res, lab), width = 7.5,
         height = 1.4 + 0.34 * length(res$attr_names), dpi = 200)
  ggsave(file.path(FIG_DIR, paste0(app, "_residsd.png")),
         resid_overlay(res, lab), width = 7.5,
         height = 1.4 + 0.34 * length(res$attr_names), dpi = 200)
  cat("figures written for", app, "\n")
}
cat("DONE figures\n")
