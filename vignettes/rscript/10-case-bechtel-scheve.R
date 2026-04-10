Sys.setenv(OMP_NUM_THREADS = "8")
options(Ncpus = 8L, mc.cores = 8L)
set.seed(20260409)
if (requireNamespace("torch", quietly = TRUE)) {
  torch::torch_manual_seed(20260409)
}
library(sconjoint)
data(bs2013, package = "sconjoint")
head(bs2013, 4)
dim(bs2013)
range(bs2013$cost_usd)
fit_bs <- scfit(
  choice ~ distribution + enforcement + monitoring +
           participation + sanctions + cost_usd |
           resp_female + age + resp_ideo,
  data        = bs2013,
  respondent  = "respondent",
  task        = "task",
  profile     = "profile",
  K           = 3L,
  n_epochs    = 60L,
  seed        = 20260409
)
summary(fit_bs)
imp_bs <- sc_importance(fit_bs)
imp_bs
wtp_part <- sc_wtp(fit_bs,
                   attr = "participation90pct",
                   cost_attr = "cost_usd")
wtp_mon  <- sc_wtp(fit_bs,
                   attr = "monitoringinternational",
                   cost_attr = "cost_usd")
rbind(
  participation_90 = c(wtp_part$estimate, wtp_part$se),
  monitoring_intl  = c(wtp_mon$estimate,  wtp_mon$se)
)
comp <- sc_compensating(fit_bs,
                        benefit = "participation90pct",
                        cost    = "cost_usd")
comp
comp$details$frac_compensated
