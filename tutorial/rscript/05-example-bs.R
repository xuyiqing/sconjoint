Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(2024)
torch::torch_manual_seed(2024)
library(sconjoint)
library(ggplot2)

## Groups and labels for plot annotations --------------------------------
bs_groups <- c(
  "cost_usd"                             = "Cost",
  "distributionProp. current emissions"   = "Distribution",
  "distributionProp. hist. emissions"     = "Distribution",
  "distributionRich pay more, shared"     = "Distribution",
  "participation80 countries"             = "Participation",
  "participation160 countries"            = "Participation",
  "emissions60% reduction"               = "Emissions",
  "emissions80% reduction"               = "Emissions",
  "sanctions$6/mo sanctions"             = "Sanctions",
  "sanctions$17/mo sanctions"            = "Sanctions",
  "sanctions$23/mo sanctions"            = "Sanctions",
  "monitoringIndependent commission"      = "Monitoring",
  "monitoringUnited Nations"              = "Monitoring",
  "monitoringGreenpeace"                  = "Monitoring"
)

bs_labels <- c(
  "cost_usd"                             = "Cost (USD)",
  "distributionProp. current emissions"   = "Prop. current emissions",
  "distributionProp. hist. emissions"     = "Prop. hist. emissions",
  "distributionRich pay more, shared"     = "Rich pay more, shared",
  "participation80 countries"             = "80 countries",
  "participation160 countries"            = "160 countries",
  "emissions60% reduction"               = "60% reduction",
  "emissions80% reduction"               = "80% reduction",
  "sanctions$6/mo sanctions"             = "$6/mo sanctions",
  "sanctions$17/mo sanctions"            = "$17/mo sanctions",
  "sanctions$23/mo sanctions"            = "$23/mo sanctions",
  "monitoringIndependent commission"      = "Independent commission",
  "monitoringUnited Nations"              = "United Nations",
  "monitoringGreenpeace"                  = "Greenpeace"
)

data(bs2013, package = "sconjoint")
dim(bs2013)
head(bs2013, 4)

str(bs2013)

fit_bs <- scfit(
  choice ~ cost_usd + distribution + participation + emissions +
           sanctions + monitoring |
           resp_female + resp_age + resp_ideo,
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

plot_importance(fit_bs)

sc_direction_intensity(fit_bs)

## WTP for selected treaty features
sc_wtp(fit_bs, attr = "distributionRich pay more, shared", cost_attr = "cost_usd")
sc_wtp(fit_bs, attr = "emissions80% reduction",            cost_attr = "cost_usd")
sc_wtp(fit_bs, attr = "monitoringUnited Nations",          cost_attr = "cost_usd")

sc_compensating(fit_bs,
                benefit = "distributionRich pay more, shared",
                cost    = "cost_usd")
sc_compensating(fit_bs,
                benefit = "emissions80% reduction",
                cost    = "cost_usd")

frac_bs <- sc_fraction_preferring(fit_bs, threshold = 0)
frac_bs$estimate[, c("dummy_name", "frac_positive", "frac_negative")]

plot_amce(fit_bs, groups = bs_groups, labels = bs_labels)

plot_fraction(fit_bs, groups = bs_groups, labels = bs_labels)

plot_hetero(fit_bs, groups = bs_groups, labels = bs_labels)

ideo_col <- fit_bs$Z[, "resp_ideo"]
ideo_cuts <- quantile(ideo_col, probs = c(1/3, 2/3))
plot_subgroup(
  fit_bs,
  subgroup = list(Left   = ideo_col <= ideo_cuts[1],
                  Center = ideo_col > ideo_cuts[1] & ideo_col <= ideo_cuts[2],
                  Right  = ideo_col > ideo_cuts[2]),
  groups = bs_groups, labels = bs_labels,
  title = "Subgroup AMCE by ideology tercile"
)

plot(fit_bs, "beta_ridgelines")

plot(fit_bs, "loss_trace")
