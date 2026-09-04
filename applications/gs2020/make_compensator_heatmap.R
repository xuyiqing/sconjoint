#!/usr/bin/env Rscript
## Compensating-benefit heatmap, rebuilt against a named calibration.
##
## Two panels on one scale: the calibration of record, and the rebuilt
## procedure-matched bar. Every cell shows the same plug-in share, because
## the fit did not change; what changes is which cells the release gate
## certifies. Withheld cells are drawn hatched, so the exhibit says what it
## can and cannot support rather than printing a bare number.
options(stringsAsFactors = FALSE, warn = 1)
suppressPackageStartupMessages({library(ggplot2); library(dplyr)})
root <- path.expand("~/GitHub/sconjoint")
P <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected/estimands")
OUT <- file.path(root, "applications/gs2020/results/mixed_logit/v21_corrected/figures")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

load_arm <- function(suffix, label) {
  d <- read.csv(file.path(P, paste0("compensator_columns", suffix, ".csv")))
  d$arm <- label
  d$released <- d$release_kind == "point"
  d
}
rec <- load_arm("_record", "Calibration of record")
mat <- load_arm("_matchedD", "Procedure-matched bar (R = 30, five folds)")
d <- bind_rows(rec, mat)

SPEC <- c(none = "None", party = "Co-party", pol = "Full policy",
          gov = "Gov. feature", any = "Any")
d$spec_lab <- factor(SPEC[d$spec], levels = unname(SPEC))
d$action_lab <- factor(sub("^diff_dem_code_u_", "", d$action))
d$group <- factor(d$group,
  levels = c("Overall", "Liberal (1-3)", "Moderate (4)", "Conservative (5-7)"))
d$arm <- factor(d$arm, levels = c("Calibration of record",
                                  "Procedure-matched bar (R = 30, five folds)"))

n_rel <- d %>% group_by(arm) %>% summarise(n = sum(released), .groups = "drop")
lab <- setNames(sprintf("%s\n%d of %d cells certified", n_rel$arm, n_rel$n,
                        nrow(d) / 2), n_rel$arm)

p <- ggplot(d, aes(x = spec_lab, y = action_lab, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  ## Withheld cells get a diagonal slash rather than a printed value.
  geom_segment(data = subset(d, !released),
               aes(x = as.numeric(spec_lab) - 0.5, xend = as.numeric(spec_lab) + 0.5,
                   y = as.numeric(action_lab) - 0.5, yend = as.numeric(action_lab) + 0.5),
               inherit.aes = FALSE, color = "grey35", linewidth = 0.35, alpha = 0.85) +
  geom_text(data = subset(d, released), aes(label = sprintf("%.2f", share)),
            size = 2.5, color = "black") +
  facet_grid(group ~ arm, labeller = labeller(arm = lab)) +
  scale_fill_gradient(low = "#F7F9FB", high = "#2C6FA6", name = "Plug-in share",
                      limits = c(0, 1)) +
  labs(x = NULL, y = NULL,
       title = "Compensating-benefit shares: what the release gate certifies",
       subtitle = paste("Shading is the plug-in share and is identical across panels ---",
                        "the fit did not change. Hatched cells are withheld.")) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 7.5),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom", legend.key.height = unit(3, "mm"))

ggsave(file.path(OUT, "fig_compensator_heatmap_rebuilt.pdf"), p, width = 8.4, height = 9.2)
ggsave(file.path(OUT, "fig_compensator_heatmap_rebuilt.png"), p, width = 8.4, height = 9.2, dpi = 200)
cat("released by arm:\n"); print(as.data.frame(n_rel))
cat("written:", file.path(OUT, "fig_compensator_heatmap_rebuilt.pdf"), "\n")
