Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(2024)
torch::torch_manual_seed(2024)
library(sconjoint)
library(ggplot2)

data(bs2013, package = "sconjoint")
dim(bs2013)
head(bs2013, 4)

str(bs2013)

fit_bs <- scfit(
  choice ~ distribution + enforcement + monitoring + participation +
           sanctions + cost_usd |
           resp_female + age + resp_ideo,
  data        = bs2013,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 2024
)
summary(fit_bs)

sc_importance(fit_bs)

sc_direction_intensity(fit_bs)

## WTP for selected treaty features
sc_wtp(fit_bs, attr = "distribution:ability_to_pay", cost_attr = "cost_usd")
sc_wtp(fit_bs, attr = "enforcement:trade_sanctions", cost_attr = "cost_usd")
sc_wtp(fit_bs, attr = "monitoring:international",    cost_attr = "cost_usd")

sc_compensating(fit_bs,
                benefit = "distribution:ability_to_pay",
                cost    = "cost_usd")
sc_compensating(fit_bs,
                benefit = "enforcement:trade_sanctions",
                cost    = "cost_usd")

frac_bs <- sc_fraction_preferring(fit_bs, threshold = 0)
frac_bs$estimate[, c("dummy_name", "frac_positive", "frac_negative")]

plot_amce(fit_bs)

plot_fraction(fit_bs)

plot_hetero(fit_bs)

ideo_col <- fit_bs$Z[, "resp_ideo"]
ideo_cuts <- quantile(ideo_col, probs = c(1/3, 2/3))
plot_subgroup(
  fit_bs,
  subgroup = list(Left   = ideo_col <= ideo_cuts[1],
                  Center = ideo_col > ideo_cuts[1] & ideo_col <= ideo_cuts[2],
                  Right  = ideo_col > ideo_cuts[2]),
  title = "Subgroup AMCE by ideology tercile"
)

plot(fit_bs, "beta_ridgelines")

plot(fit_bs, "loss_trace")
