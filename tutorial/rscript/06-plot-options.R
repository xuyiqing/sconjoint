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

plot_amce(fit, dummies = c("cand_genderFemale",
                            "agendaComplete Overhaul",
                            "prior_officeYes"))

plot_fraction(fit, dummies = c("cand_genderFemale",
                                "agendaComplete Overhaul"))

my_labels <- c(
  "agendaModerate Changes"   = "Moderate Changes",
  "agendaComplete Overhaul"  = "Complete Overhaul",
  "cand_genderFemale"        = "Female candidate",
  "prior_officeYes"          = "Prior office",
  "talentHard-Working"       = "Hard-Working",
  "talentCollaborative"      = "Collaborative"
)

plot_amce(fit, labels = my_labels)

plot_amce(fit,
          dummies = c("cand_genderFemale", "agendaComplete Overhaul"),
          labels = c("cand_genderFemale" = "Female candidate",
                     "agendaComplete Overhaul" = "Complete Overhaul"),
          title = "Key attributes")

sw_groups <- c(
  "agendaModerate Changes"  = "Agenda",
  "agendaComplete Overhaul" = "Agenda",
  "talentCollaborative"     = "Talent",
  "talentDetermined to Succeed" = "Talent",
  "talentEmpathetic"        = "Talent",
  "talentGood Communicator" = "Talent",
  "talentHard-Working"      = "Talent",
  "talentTough Negotiator"  = "Talent",
  "children1 child"         = "Children",
  "children2 children"      = "Children",
  "children3 children"      = "Children",
  "cand_genderFemale"       = "Gender",
  "prior_officeYes"         = "Prior Office"
)

plot_amce(fit, groups = sw_groups)

plot_fraction(fit, groups = sw_groups)

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

plot_importance(fit)

plot(fit, "beta_ridgelines",
     dummies = c("agendaComplete Overhaul", "cand_genderFemale",
                  "prior_officeYes"),
     labels = c("agendaComplete Overhaul" = "Complete Overhaul",
                "cand_genderFemale" = "Female",
                "prior_officeYes" = "Prior Office"),
     title = "Selected attributes")

plot(fit, "loss_trace", title = "Training convergence", theme.bw = TRUE)

plot_amce(fit) +
  labs(subtitle = "Saha-Weeks (2022) candidate choice") +
  theme(plot.subtitle = element_text(color = "gray40", size = 10))

library(gridExtra)
p1 <- plot_amce(fit, groups = sw_groups,
                title = expression(bold("A.") ~ "Average preferences"))
p2 <- plot_fraction(fit, groups = sw_groups,
                    title = expression(bold("B.") ~ "Fraction favor/oppose"))
grid.arrange(p1, p2, ncol = 2, widths = c(1, 1.1))
