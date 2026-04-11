Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(42)
torch::torch_manual_seed(42)
library(sconjoint)
library(ggplot2)

data(simdata, package = "sconjoint")
dim(simdata)
head(simdata, 6)

## True beta matrix stored as attribute
beta_true <- attr(simdata, "beta_true")
dim(beta_true)

fit_sim <- scfit(
  choice ~ x1 + x2 + x3 | z1 + z2,
  data       = simdata,
  respondent = "respondent",
  task       = "task",
  profile    = "profile",
  K          = 5L,
  n_epochs   = 200L,
  seed       = 42
)
summary(fit_sim)

plot(fit_sim, "loss_trace")

theta_hat <- coef(fit_sim)
theta_true <- c(x1 = 0.5, x2 = -0.8, x3 = 0.3)
data.frame(
  attribute  = names(theta_hat),
  true       = theta_true,
  estimated  = round(unname(theta_hat), 3),
  row.names  = NULL
)

plot_amce(fit_sim)

beta_hat_all <- predict(fit_sim)

## De-duplicate to respondent level
first_row <- !duplicated(fit_sim$respondent_id)
beta_hat <- beta_hat_all[first_row, ]

## Correlation between true and estimated beta
data.frame(
  attribute   = c("x1", "x2", "x3"),
  correlation = round(c(
    cor(beta_true[, 1], beta_hat[, 1]),
    cor(beta_true[, 2], beta_hat[, 2]),
    cor(beta_true[, 3], beta_hat[, 3])
  ), 3),
  row.names = NULL
)

df_compare <- data.frame(
  true      = beta_true[, 1],
  estimated = beta_hat[, 1]
)

ggplot(df_compare, aes(x = true, y = estimated)) +
  geom_point(alpha = 0.3, color = "#2166AC", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "#E41A1C", linewidth = 0.8) +
  labs(x = expression("True " * beta[1](Z)),
       y = expression("Estimated " * hat(beta)[1](Z)),
       title = "Individual-level recovery: attribute x1") +
  theme_minimal(base_size = 12)

plot(fit_sim, "beta_ridgelines")

sc_direction_intensity(fit_sim)

frac_sim <- sc_fraction_preferring(fit_sim)

true_frac <- c(
  x1 = mean(beta_true[, 1] > 0),
  x2 = mean(beta_true[, 2] > 0),
  x3 = mean(beta_true[, 3] > 0)
)

data.frame(
  attribute     = names(true_frac),
  true_fraction = true_frac,
  estimated     = round(frac_sim$estimate$frac_positive, 3),
  row.names     = NULL
)

plot_fraction(fit_sim)

het_sim <- sc_heterogeneity_test(fit_sim, adjust = "bh")
het_sim$estimate[, c("dummy_name", "var_beta", "p_adjusted", "sig")]

plot_hetero(fit_sim)

z1_col <- fit_sim$Z[, "z1"]
sub_sim <- sc_subgroup(fit_sim, list(
  "z1 > 0" = z1_col > 0,
  "z1 < 0" = z1_col < 0
))

z1_resp <- fit_sim$Z[first_row, "z1"]
true_high <- mean(beta_true[z1_resp > 0, 1])
true_low  <- mean(beta_true[z1_resp < 0, 1])

data.frame(
  group     = c("z1 > 0", "z1 < 0"),
  true_b1   = round(c(true_high, true_low), 3),
  est_b1    = round(c(
    sub_sim[["z1 > 0"]]$estimate$theta[1],
    sub_sim[["z1 < 0"]]$estimate$theta[1]
  ), 3),
  row.names = NULL
)

plot_subgroup(
  fit_sim,
  subgroup = list("z1 > 0" = z1_col > 0, "z1 < 0" = z1_col < 0),
  title = "Subgroup AMCE: high-z1 vs low-z1 respondents"
)

bl <- sc_baseline_logit(fit_sim)
summary(bl)

theta_struct <- coef(fit_sim)
theta_logit  <- coef(bl)
df_comp <- data.frame(
  dummy = rep(names(theta_struct), 2),
  estimate = c(theta_struct, theta_logit),
  model = rep(c("Structural (DML)", "Logit"), each = length(theta_struct))
)
ggplot(df_comp, aes(x = estimate, y = dummy, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(0.4), size = 3) +
  scale_color_manual(values = c("Structural (DML)" = "#E41A1C",
                                "Logit" = "#2166AC")) +
  labs(x = expression(hat(theta)), y = NULL, color = NULL,
       title = "Structural vs baseline logit") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

sc_average(fit_sim, scale = "probability")

sc_surplus(fit_sim, profiles = list(
  list(x1 = 1, x2 = 0, x3 = 1),
  list(x1 = 0, x2 = 1, x3 = 0)
))

sc_welfare_change(fit_sim,
  old_set = list(list(x1 = 0)),
  new_set = list(list(x1 = 1))
)

sc_decisiveness(fit_sim,
  A = list(x1 = 1, x3 = 1),
  B = list(x2 = 1)
)

sc_inequality(fit_sim, measure = "variance")
