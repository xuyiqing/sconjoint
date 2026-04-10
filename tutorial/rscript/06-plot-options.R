Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(2024)
torch::torch_manual_seed(2024)
library(sconjoint)
library(ggplot2)

data(sw2022, package = "sconjoint")
fit <- scfit(
  choice ~ agenda + talent + children + cand_gender + prior_office |
           resp_female + age + pid,
  data       = sw2022,
  respondent = "respondent",
  task       = "task",
  profile    = "profile",
  K          = 3L,
  n_epochs   = 60L,
  seed       = 2024
)

plot_amce(fit, dummies = c("cand_genderfemale", "agendaprogressive",
                            "prior_officeus_house"))

plot_fraction(fit, dummies = c("cand_genderfemale", "agendaprogressive"))

my_labels <- c(
  agendaprogressive  = "Progressive agenda",
  agendaconservative = "Conservative agenda",
  cand_genderfemale  = "Female candidate",
  prior_officeus_house = "U.S. House experience"
)

plot_amce(fit, labels = my_labels)

plot_amce(fit,
          dummies = c("cand_genderfemale", "agendaprogressive"),
          labels = c(cand_genderfemale = "Female candidate",
                     agendaprogressive = "Progressive agenda"),
          title = "Key attributes")

plot_amce(fit, color = "#2166AC", title = "AMCE (blue)")

plot_fraction(fit,
              colors = c(Favor = "#1B9E77", Oppose = "#D95F02"),
              title = "Custom fraction colors")

plot_hetero(fit,
            gradient = c(low = "#FEE0D2", high = "#DE2D26"),
            sig.color = "black",
            title = "Red gradient with black markers")

fem <- fit$Z[, "resp_female"] > 0.5
plot_subgroup(fit,
              subgroup = list(Female = fem, Male = !fem),
              colors = c(Female = "#7570B3", Male = "#1B9E77"),
              title = "Custom subgroup colors")

plot_amce(fit,
          title = "Saha-Weeks candidate choice",
          xlab  = "Logit-scale AMCE",
          ylab  = "Attribute level")

plot_amce(fit, theme.bw = TRUE)

plot_fraction(fit, gridOff = TRUE)

plot_amce(fit, cex.main = 1.4, cex.axis = 0.9, cex.lab = 1.1)

fem <- fit$Z[, "resp_female"] > 0.5
plot_subgroup(fit,
              subgroup = list(Female = fem, Male = !fem),
              legend.pos = "right")

plot_fraction(fit, legendOff = TRUE)

plot_amce(fit, xlim = c(-1.5, 1.5))

plot(fit, "beta_ridgelines",
     dummies = c("agendaprogressive", "cand_genderfemale",
                  "prior_officeus_house"),
     labels = c(agendaprogressive = "Progressive",
                cand_genderfemale = "Female",
                prior_officeus_house = "U.S. House"),
     title = "Selected attributes")

plot(fit, "loss_trace", title = "Training convergence", theme.bw = TRUE)

plot_amce(fit) +
  labs(subtitle = "Saha-Weeks (2022) candidate choice") +
  theme(plot.subtitle = element_text(color = "gray40", size = 10))
