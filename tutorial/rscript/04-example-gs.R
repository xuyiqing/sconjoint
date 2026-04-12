Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(2024)
torch::torch_manual_seed(2024)
library(sconjoint)
library(ggplot2)

## Groups and labels for plot annotations --------------------------------
gs_groups <- c(
  "cand_partyRepublican"      = "Party",
  "dem_codeg_committee"       = "Good Governance",
  "dem_codeg_officestructure" = "Good Governance",
  "dem_codeg_procedure"       = "Good Governance",
  "dem_codeg_progEval"        = "Good Governance",
  "dem_codeg_record"          = "Good Governance",
  "dem_codeg_schedule"        = "Good Governance",
  "dem_codeu_banProtest"      = "Undemocratic",
  "dem_codeu_court"           = "Undemocratic",
  "dem_codeu_execRule"        = "Undemocratic",
  "dem_codeu_gerry2"          = "Undemocratic",
  "dem_codeu_gerry10"         = "Undemocratic",
  "dem_codeu_journalists"     = "Undemocratic",
  "dem_codeu_limitVote"       = "Undemocratic",
  "dem_codev_affair"          = "Valence",
  "dem_codev_tax"             = "Valence",
  "cand_sexFemale"            = "Sex",
  "cand_raceBlack"            = "Race",
  "cand_raceHispanic"         = "Race",
  "cand_raceAsian"            = "Race",
  "policy_prox"               = "Policy"
)

gs_labels <- c(
  "cand_partyRepublican"      = "Republican",
  "dem_codeg_committee"       = "Committee oversight",
  "dem_codeg_officestructure" = "Office structure",
  "dem_codeg_procedure"       = "Procedure",
  "dem_codeg_progEval"        = "Program evaluation",
  "dem_codeg_record"          = "Record-keeping",
  "dem_codeg_schedule"        = "Schedule",
  "dem_codeu_banProtest"      = "Ban protest",
  "dem_codeu_court"           = "Ignore court",
  "dem_codeu_execRule"        = "Executive rule",
  "dem_codeu_gerry2"          = "Gerrymander (2%)",
  "dem_codeu_gerry10"         = "Gerrymander (10%)",
  "dem_codeu_journalists"     = "Restrict journalists",
  "dem_codeu_limitVote"       = "Limit voting",
  "dem_codev_affair"          = "Affair",
  "dem_codev_tax"             = "Tax evasion",
  "cand_sexFemale"            = "Female",
  "cand_raceBlack"            = "Black",
  "cand_raceHispanic"         = "Hispanic",
  "cand_raceAsian"            = "Asian",
  "policy_prox"               = "Policy proximity"
)

data(gs2020, package = "sconjoint")
dim(gs2020)
head(gs2020, 4)
str(gs2020)

fit_gs <- scfit(
  choice ~ cand_party + dem_code + cand_sex + cand_race + policy_prox |
           resp_ideo + resp_pid + resp_age,
  data        = gs2020,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 2024
)
summary(fit_gs)

sc_importance(fit_gs)

plot_importance(fit_gs)

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
  rows <- grepl("^dem_codeu_", e$dummy_name)
  data.frame(group = g, e[rows, c("dummy_name", "theta", "se")],
             row.names = NULL)
}))

cl_gs <- sc_clusters(fit_gs, k = 3L, seed = 2024)
cl_gs$estimate$sizes
round(cl_gs$estimate$centers, 2)

plot_amce(fit_gs, groups = gs_groups, labels = gs_labels)

plot_fraction(fit_gs, groups = gs_groups, labels = gs_labels)

pid_col <- fit_gs$Z[, "resp_pid"]
plot_subgroup(
  fit_gs,
  subgroup = list(Democrat    = pid_col < -0.5,
                  Independent = abs(pid_col) <= 0.5,
                  Republican  = pid_col > 0.5),
  dummies = grep("^dem_codeu_", fit_gs$attr_names, value = TRUE),
  groups = gs_groups, labels = gs_labels,
  title = "Subgroup AMCE: undemocratic behavior"
)

plot(fit_gs, "beta_ridgelines")

plot(fit_gs, "loss_trace")
