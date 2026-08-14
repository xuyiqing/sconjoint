# v4 evidence figures beyond the BEFORE/AFTER overlays (E2).
# Reads v4_quantities_<app>.rds slot caches; never refits.
#   gs2020_ridgeline_modelimplied.png  model-implied population densities
#                                      + clearly-labeled descriptive
#                                      posterior-mean panel
#   br2017_rate_schedule_posterior.png descriptive posterior-mean rate
#                                      schedules by party
#   sw2022_partisan_gender_v4.png      party gender means (survives) +
#                                      design-check display (replaces the
#                                      MAP density panel)
suppressMessages({
  library(ggplot2)
  ## the S3 methods on scmix_posterior (as.data.frame) live in the package
  devtools::load_all("~/GitHub/sconjoint", quiet = TRUE)
})

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
FIG_DIR <- file.path(OUT_DIR, "figs")
MEMO_FIG <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/doc/2608_issues/figs_mixedlogit")
dir.create(FIG_DIR, showWarnings = FALSE)

ACCENT <- "#a8003b"
GRAY <- "grey35"
party_colors <- c(Democrat = "#1f78b4", Independent = "grey50",
                  Republican = "#e31a1c")

qload <- function(app) readRDS(file.path(OUT_DIR,
                                         sprintf("v4_quantities_%s.rds", app)))
save_fig <- function(name, plot, width, height) {
  ggsave(file.path(FIG_DIR, name), plot, width = width, height = height,
         dpi = 200)
  file.copy(file.path(FIG_DIR, name), file.path(MEMO_FIG, name),
            overwrite = TRUE)
  cat("written:", name, "\n")
}

## --- gs2020 ridgeline: model-implied population densities ------------------
gs <- qload("gs2020")
di <- gs$density_inputs
show_coords <- c("diff_respParty", "diff_dem_code_v_tax",
                 "diff_dem_code_v_affair", "diff_dem_code_u_journalists",
                 "diff_dem_code_u_limitVote", "diff_dem_code_u_court",
                 "diff_dem_code_u_gerry10", "diff_dem_code_u_gerry2")
coord_labels <- c(diff_respParty = "Co-partisan",
                  diff_dem_code_v_tax = "Tax evasion",
                  diff_dem_code_v_affair = "Extramarital affair",
                  diff_dem_code_u_journalists = "Prosecute journalists",
                  diff_dem_code_u_limitVote = "Close polling stations",
                  diff_dem_code_u_court = "Ignore courts",
                  diff_dem_code_u_gerry10 = "Gerrymander (10 seats)",
                  diff_dem_code_u_gerry2 = "Gerrymander (2 seats)")

grid_x <- seq(-3.2, 3.0, length.out = 501)
dens_rows <- list()
for (k in show_coords) {
  j <- match(k, di$attr_names)
  mu_k <- di$mu_resp[, j]
  s_k <- max(di$sigma_k[j], 0.02)   # display floor only, noted in caption
  d <- rowMeans(vapply(seq_along(mu_k), function(i)
    stats::dnorm(grid_x, mu_k[i], s_k), numeric(length(grid_x))))
  dens_rows[[k]] <- data.frame(coord = coord_labels[[k]], x = grid_x,
                               density = d)
}
dens_df <- do.call(rbind, dens_rows)
dens_df$coord <- factor(dens_df$coord, levels = rev(unname(coord_labels)))

post <- as.data.frame(gs$posterior)
post_rows <- list()
for (k in show_coords) {
  post_rows[[k]] <- data.frame(coord = coord_labels[[k]],
                               x = post[[paste0("mean_", k)]])
}
post_df <- do.call(rbind, post_rows)
post_df$coord <- factor(post_df$coord, levels = rev(unname(coord_labels)))

p_model <- ggplot(dens_df, aes(x = x, y = density)) +
  geom_area(fill = ACCENT, alpha = 0.25, color = ACCENT, linewidth = 0.4) +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey60") +
  facet_grid(rows = vars(coord), switch = "y", scales = "free_y") +
  labs(x = expression(beta[k] ~ "(logit scale)"), y = NULL,
       title = "A. Model-implied population densities",
       subtitle = "Mixture over respondents of N(mu_k(Z), sigma_k^2) from (mu, A)") +
  theme_minimal(base_size = 10) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
        axis.text.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

p_post <- ggplot(post_df, aes(x = x)) +
  geom_histogram(bins = 60, fill = GRAY, color = NA, alpha = 0.8) +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey60") +
  facet_grid(rows = vars(coord), scales = "free_y") +
  labs(x = expression(E * "[" * beta[ik] * " | data] (posterior mean)"),
       y = NULL,
       title = "B. Respondent posterior means (descriptive)",
       subtitle = "Shrinkage summaries; dispersion understates the population's") +
  theme_minimal(base_size = 10) +
  theme(strip.text.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

cap <- sprintf(paste0(
  "Model-implied densities use the fitted residual scale per coordinate; ",
  "coordinates with residual SD below 0.02 are drawn at that display ",
  "width. Fitted sigma_k is %.2f for the co-partisan coordinate and ",
  "below 0.05 elsewhere (zero-floor ratio %.2f); panel B is descriptive ",
  "and is never aggregated into population shares."),
  di$sigma_k[match("diff_respParty", di$attr_names)],
  gs$zero_floor$ratio)
library(patchwork)
save_fig("gs2020_ridgeline_modelimplied.png",
         (p_model | p_post) +
           patchwork::plot_annotation(
             caption = paste(strwrap(cap, 110), collapse = "\n"),
             theme = theme(plot.caption = element_text(size = 8,
                                                       color = "grey30"))),
         width = 10, height = 8)

## --- br2017 rate schedules from posterior means (descriptive) --------------
br <- qload("br2017")
RATE_COLS <- c("rate_L10", "rate_10_35", "rate_35_85", "rate_85_175",
               "rate_175_375", "rate_375P")
bracket_labels <- c("<$10k", "$10-35k", "$35-85k", "$85-175k",
                    "$175-375k", ">$375k")
pb <- as.data.frame(br$posterior)
party <- br$party
sched_rows <- list()
for (b in seq_along(RATE_COLS)) {
  v <- pb[[paste0("mean_", RATE_COLS[b])]]
  for (p in base::levels(party)) {
    x <- v[party == p]
    sched_rows[[length(sched_rows) + 1L]] <- data.frame(
      bracket = bracket_labels[b], bracket_idx = b, party = p,
      mid = stats::median(x),
      lo = stats::quantile(x, 0.1), hi = stats::quantile(x, 0.9))
  }
}
sched <- do.call(rbind, sched_rows)
p_sched <- ggplot(sched, aes(x = bracket_idx, y = mid, color = party,
                             fill = party)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:6, labels = bracket_labels) +
  scale_color_manual(values = party_colors) +
  scale_fill_manual(values = party_colors) +
  labs(x = "Income bracket",
       y = "Posterior-mean rate coefficient (per point)",
       color = NULL, fill = NULL,
       title = "Individual tax-rate schedules (descriptive posterior summaries)",
       subtitle = "Median and 10-90% band of respondent posterior means by party",
       caption = "Shrinkage summaries from the fitted model; population claims use (mu, A) directly.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, color = "grey30"))
save_fig("br2017_rate_schedule_posterior.png", p_sched, 7.5, 5)

## --- sw2022 partisan gender v4 ---------------------------------------------
sw <- qload("sw2022")
lv <- c("Democrat", "Independent", "Republican")
gm <- data.frame(
  party = factor(lv, levels = lv),
  est = unname(sw$theta_by_party$estimate[paste(lv, "cand_genderMale",
                                                sep = ": ")]),
  se = unname(sw$theta_by_party$se[paste(lv, "cand_genderMale",
                                         sep = ": ")]))
p_means <- ggplot(gm, aes(x = party, y = est, color = party)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  linewidth = 0.7, size = 0.6) +
  scale_color_manual(values = party_colors, guide = "none") +
  labs(x = NULL, y = expression("Mean " * beta["Male"] * " (logit scale)"),
       title = "A. Party-level gender means",
       subtitle = "Subgroup means with the loading correction (material at T = 3)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

dc <- sw$design_check
ld <- dc$loadings[seq_along(sw$theta$estimate), ]
ld$coord <- factor(ld$coord, levels = rev(ld$coord))
ld$identified <- ld$t >= dc$t_min
p_dc <- ggplot(ld, aes(x = t, y = coord, fill = identified)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = dc$t_min, linetype = 2, color = "grey40") +
  scale_fill_manual(values = c(`TRUE` = ACCENT, `FALSE` = "grey75"),
                    labels = c(`TRUE` = sprintf("t >= %g", dc$t_min),
                               `FALSE` = sprintf("t < %g", dc$t_min)),
                    name = NULL) +
  labs(x = "Loading t-ratio at this design", y = NULL,
       title = "B. What the T = 3 design identifies",
       subtitle = "Only the agenda coordinates support distributional claims",
       caption = sprintf(
         "Within-party gender majority shares are reported NA under this gate; zero-floor ratio %.2f (threshold 2).",
         sw$zero_floor$ratio)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, color = "grey30"))
save_fig("sw2022_partisan_gender_v4.png",
         patchwork::wrap_plots(p_means, p_dc, widths = c(1, 1.3)),
         width = 10, height = 5.5)

cat("DONE v4 figures\n")
