Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(2024)
torch::torch_manual_seed(2024)
library(sconjoint)
library(ggplot2)

data(sw2022, package = "sconjoint")
dim(sw2022)
head(sw2022, 4)

str(sw2022)
table(table(sw2022$respondent))

fit_sw <- scfit(
  choice ~ agenda + talent + children + cand_gender + prior_office |
           resp_female + age + pid,
  data        = sw2022,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 2024
)
summary(fit_sw)

coef(fit_sw)

beta <- predict(fit_sw)
dim(beta)
head(beta, 3)

sc_importance(fit_sw)

sc_direction_intensity(fit_sw)

frac <- sc_fraction_preferring(fit_sw, threshold = 0)
frac$estimate[, c("dummy_name", "frac_positive", "frac_negative")]

sc_mrs(fit_sw, numerator = 1L, denominator = 2L)

sc_counterfactual(
  fit_sw,
  A = list(agenda = "progressive", talent = "experienced",
           children = "has_children", cand_gender = "female",
           prior_office = "us_house"),
  B = list(agenda = "conservative", talent = "average",
           children = "no_children", cand_gender = "male",
           prior_office = "none")
)

sc_optimal_profile(fit_sw)

sc_heterogeneity_test(fit_sw, adjust = "bh")

fem <- fit_sw$Z[, "resp_female"] > 0.5
sub_fem  <- sc_subgroup(fit_sw, fem)
sub_male <- sc_subgroup(fit_sw, !fem)
rbind(
  female = sub_fem$estimate[sub_fem$estimate$dummy_name == "cand_genderfemale",
                            c("theta", "se")],
  male   = sub_male$estimate[sub_male$estimate$dummy_name == "cand_genderfemale",
                            c("theta", "se")]
)

plot_amce(fit_sw)

plot_fraction(fit_sw)

plot_hetero(fit_sw)

plot_subgroup(
  fit_sw,
  subgroup = list(Female = fit_sw$Z[, "resp_female"] > 0.5,
                  Male   = fit_sw$Z[, "resp_female"] <= 0.5),
  title = "Subgroup AMCE: female vs male respondents"
)

plot(fit_sw, "beta_ridgelines")

plot(fit_sw, "loss_trace")
