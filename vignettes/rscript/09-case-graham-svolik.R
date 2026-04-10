Sys.setenv(OMP_NUM_THREADS = "8")
options(Ncpus = 8L, mc.cores = 8L)
set.seed(20260409)
if (requireNamespace("torch", quietly = TRUE)) {
  torch::torch_manual_seed(20260409)
}
library(sconjoint)
data(gs2020, package = "sconjoint")
head(gs2020, 4)
dim(gs2020)
fit_gs <- scfit(
  choice ~ federal + immigration + tax + abortion + undem + cand_party |
           resp_pid + resp_ideo + age,
  data        = gs2020,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 20260409
)
summary(fit_gs)
het_gs <- sc_heterogeneity_test(fit_gs, adjust = "bh")
het_gs$estimate
di_gs <- sc_direction_intensity(fit_gs)
di_gs
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
cl_gs <- sc_clusters(fit_gs, k = 3L, seed = 20260409)
cl_gs$estimate$sizes
round(cl_gs$estimate$centers, 2)
