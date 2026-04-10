Sys.setenv(TORCH_SERIALIZE_VERSION = "2")
options(width = 78)
set.seed(42)
torch::torch_manual_seed(42)
library(sconjoint)
library(ggplot2)

M <- 100L
TT <- 4L
set.seed(42)

## Respondent-level moderators
z1 <- rnorm(M)
z2 <- rnorm(M)

## True beta(Z) for each respondent (M x 3)
beta_true <- cbind(
  x1 = 0.5 + 0.3 * z1,
  x2 = -0.8 + 0.2 * z2,
  x3 = rep(0.3, M)
)

## Build long-format data frame
rows <- vector("list", M * TT * 2L)
idx <- 0L
for (i in seq_len(M)) {
  for (tt in seq_len(TT)) {
    xA <- rbinom(3, 1, 0.5)
    xB <- rbinom(3, 1, 0.5)
    deltaX <- xA - xB
    V <- sum(deltaX * beta_true[i, ])
    choiceA <- rbinom(1, 1, plogis(V))
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      respondent = i, task = tt, profile = 1L, choice = choiceA,
      x1 = xA[1], x2 = xA[2], x3 = xA[3], z1 = z1[i], z2 = z2[i]
    )
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      respondent = i, task = tt, profile = 2L, choice = 1L - choiceA,
      x1 = xB[1], x2 = xB[2], x3 = xB[3], z1 = z1[i], z2 = z2[i]
    )
  }
}
sim_data <- do.call(rbind, rows)
dim(sim_data)
head(sim_data, 6)

fit_sim <- scfit(
  choice ~ x1 + x2 + x3 | z1 + z2,
  data       = sim_data,
  respondent = "respondent",
  task       = "task",
  profile    = "profile",
  K          = 3L,
  n_epochs   = 100L,
  seed       = 42
)
summary(fit_sim)

theta_hat <- coef(fit_sim)
theta_true <- c(x1 = 0.5, x2 = -0.8, x3 = 0.3)
data.frame(
  attribute  = names(theta_hat),
  true       = theta_true,
  estimated  = round(unname(theta_hat), 3),
  row.names  = NULL
)

beta_hat_all <- predict(fit_sim)

## De-duplicate to respondent level
first_row <- !duplicated(fit_sim$respondent_id)
beta_hat <- beta_hat_all[first_row, ]

## Correlation between true and estimated beta
data.frame(
  attribute   = c("x1", "x2"),
  correlation = round(c(
    cor(beta_true[, 1], beta_hat[, 1]),
    cor(beta_true[, 2], beta_hat[, 2])
  ), 3),
  row.names = NULL
)

df_compare <- data.frame(
  true      = beta_true[, 1],
  estimated = beta_hat[, 1]
)

ggplot(df_compare, aes(x = true, y = estimated)) +
  geom_point(alpha = 0.5, color = "#2166AC", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "#E41A1C", linewidth = 0.8) +
  labs(x = expression("True " * beta[1](Z)),
       y = expression("Estimated " * beta[1](Z)),
       title = "Individual-level recovery: attribute x1") +
  theme_minimal(base_size = 12)

sc_direction_intensity(fit_sim)

frac_sim <- sc_fraction_preferring(fit_sim)

## Compare to DGP truth
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

het_sim <- sc_heterogeneity_test(fit_sim, adjust = "bh")
het_sim$estimate[, c("dummy_name", "var_beta", "p_adjusted", "sig")]

z1_col <- fit_sim$Z[, "z1"]
sub_sim <- sc_subgroup(fit_sim, list(
  "z1 > 0" = z1_col > 0,
  "z1 < 0" = z1_col < 0
))

## Compare to true subgroup means
true_high <- mean(beta_true[z1 > 0, 1])
true_low  <- mean(beta_true[z1 < 0, 1])

data.frame(
  group     = c("z1 > 0", "z1 < 0"),
  true_b1   = round(c(true_high, true_low), 3),
  est_b1    = round(c(
    sub_sim[["z1 > 0"]]$estimate$theta[1],
    sub_sim[["z1 < 0"]]$estimate$theta[1]
  ), 3),
  row.names = NULL
)

plot_amce(fit_sim)

plot_fraction(fit_sim)

plot_hetero(fit_sim)

z1_col <- fit_sim$Z[, "z1"]
plot_subgroup(
  fit_sim,
  subgroup = list("z1 > 0" = z1_col > 0, "z1 < 0" = z1_col < 0),
  title = "Subgroup AMCE: high-z1 vs low-z1 respondents"
)

plot(fit_sim, "beta_ridgelines")

plot(fit_sim, "loss_trace")
