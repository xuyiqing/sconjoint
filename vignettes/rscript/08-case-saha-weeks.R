Sys.setenv(OMP_NUM_THREADS = "8")
options(Ncpus = 8L, mc.cores = 8L)
set.seed(20260409)
if (requireNamespace("torch", quietly = TRUE)) {
  torch::torch_manual_seed(20260409)
}
library(sconjoint)
data(sw2022, package = "sconjoint")
head(sw2022, 4)
dim(sw2022)
table(table(sw2022$respondent))  # tasks per respondent (x2 profiles)
fit_sw <- scfit(
  choice ~ agenda + talent + children + cand_gender + prior_office |
           resp_female + age + pid,
  data        = sw2022,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 20260409
)
summary(fit_sw)
imp_sw <- sc_importance(fit_sw)
imp_sw
di_sw <- sc_direction_intensity(fit_sw)
di_sw
frac_sw <- sc_fraction_preferring(fit_sw, threshold = 0)
frac_sw$estimate[, c("dummy_name", "frac_positive", "frac_negative")]
N <- nrow(fit_sw$beta_hat)
fem <- fit_sw$Z[, "resp_female"] > 0.5
sub_fem <- sc_subgroup(fit_sw, fem)
sub_male <- sc_subgroup(fit_sw, !fem)
rbind(
  female = sub_fem$estimate[sub_fem$estimate$dummy_name == "cand_genderfemale",
                            c("theta", "se")],
  male   = sub_male$estimate[sub_male$estimate$dummy_name == "cand_genderfemale",
                            c("theta", "se")]
)
