Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(2024)
torch::torch_manual_seed(2024)
library(sconjoint)
library(ggplot2)

data(gs2020, package = "sconjoint")
dim(gs2020)
head(gs2020, 4)
str(gs2020)

fit_gs <- scfit(
  choice ~ federal + immigration + tax + abortion + undem + cand_party |
           resp_pid + resp_ideo + age,
  data        = gs2020,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 2024
)
summary(fit_gs)

het_gs <- sc_heterogeneity_test(fit_gs, adjust = "bh")
het_gs$estimate

sc_direction_intensity(fit_gs)

pid_col <- fit_gs$Z[, "resp_pid"]
sub <- sc_subgroup(fit_gs, list(
  dem = pid_col < -0.5,
  ind = abs(pid_col) <= 0.5,
  rep = pid_col >  0.5
))
do.call(rbind, lapply(names(sub), function(g) {
  e <- sub[[g]]$estimate
  rows <- grepl("^undem", e$dummy_name)
  data.frame(group = g, e[rows, c("dummy_name", "theta", "se")],
             row.names = NULL)
}))

cl_gs <- sc_clusters(fit_gs, k = 3L, seed = 2024)
cl_gs$estimate$sizes
round(cl_gs$estimate$centers, 2)

plot_amce(fit_gs)

plot_fraction(fit_gs)

pid_col <- fit_gs$Z[, "resp_pid"]
plot_subgroup(
  fit_gs,
  subgroup = list(Democrat    = pid_col < -0.5,
                  Independent = abs(pid_col) <= 0.5,
                  Republican  = pid_col > 0.5),
  dummies = grep("^undem", fit_gs$attr_names, value = TRUE),
  title = "Subgroup AMCE: undemocratic behavior"
)

plot(fit_gs, "beta_ridgelines")

plot(fit_gs, "loss_trace")
