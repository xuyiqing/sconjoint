# Summarize dev/sim-old-vs-new.R into the memo's trade-off table + figure.
suppressMessages(library(ggplot2))
d <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
x <- readRDS(file.path(d, "oldvsnew_results.rds"))
res <- Filter(Negate(is.null), x$results)
cv <- x$cv

arm_summary <- function(arm) {
  rr <- Filter(function(r) r$arm == arm, res)
  R <- length(rr)
  tr <- x$truths[[arm]]
  g <- function(f) t(vapply(rr, f, numeric(4)))
  gm <- function(f) vapply(rr, f, numeric(1))
  th_m <- g(function(r) r$theta_mix); th_m_se <- g(function(r) r$theta_mix_se)
  th_o <- g(function(r) r$theta_old); th_o_se <- g(function(r) r$theta_old_se)
  th_p <- g(function(r) r$theta_pooled)
  pi_m <- g(function(r) r$pi_mix); pi_m_se <- g(function(r) r$pi_mix_se)
  pi_o <- g(function(r) r$pi_old_map)
  V_m <- gm(function(r) r$V_mix); V_m_se <- gm(function(r) r$V_mix_se)
  V_o <- gm(function(r) r$V_old_map)
  cover <- function(est, se, truth) mean(colMeans(
    abs(sweep(est, 2, truth)) <= 1.96 * se, na.rm = TRUE), na.rm = TRUE)
  ratio <- function(est) {
    big <- abs(tr$theta) >= 0.3
    mean(colMeans(sweep(est, 2, tr$theta, `/`))[big])
  }
  list(
    arm = arm, R = R,
    theta = c(mix_ratio = ratio(th_m), old_ratio = ratio(th_o),
              pooled_ratio = ratio(th_p),
              mix_rmse = sqrt(mean(sweep(th_m, 2, tr$theta)^2)),
              old_rmse = sqrt(mean(sweep(th_o, 2, tr$theta)^2)),
              mix_cover = cover(th_m, th_m_se, tr$theta),
              old_cover = cover(th_o, th_o_se, tr$theta)),
    pi = c(mix_bias = mean(abs(colMeans(sweep(pi_m, 2, tr$pi), na.rm = TRUE)),
                           na.rm = TRUE),
           old_bias = mean(abs(colMeans(sweep(pi_o, 2, tr$pi)))),
           mix_cover = cover(pi_m, pi_m_se, tr$pi),
           mix_na_rate = mean(is.na(pi_m))),
    V = c(mix_bias = mean(V_m) - tr$V, old_bias = mean(V_o) - tr$V,
          mix_cover = mean(abs(V_m - tr$V) <= 1.96 * V_m_se),
          mix_rmse = sqrt(mean((V_m - tr$V)^2)),
          old_rmse = sqrt(mean((V_o - tr$V)^2))),
    sd_hat = colMeans(g(function(r) r$sd_hat)),
    A_true = x$ARMS[[arm]]$A
  )
}

out <- lapply(names(x$ARMS), arm_summary)
names(out) <- names(x$ARMS)
for (a in out) {
  cat(sprintf("\n==== %s (R = %d) ====\n", a$arm, a$R))
  cat("theta:", paste(names(a$theta), round(a$theta, 3), sep = "=",
                      collapse = "  "), "\n")
  cat("pi:   ", paste(names(a$pi), round(a$pi, 3), sep = "=",
                      collapse = "  "), "\n")
  cat("V(c): ", paste(names(a$V), round(a$V, 4), sep = "=",
                      collapse = "  "), "\n")
  cat("loading SDs:", round(a$sd_hat, 2), "| true:", a$A_true, "\n")
}
saveRDS(out, file.path(d, "oldvsnew_summary.rds"))

## skew-arm scoping check: does the V truth move under skewness at all?
set.seed(2)
arm <- x$ARMS$skew_strong
nb <- 4e5
zb <- matrix(runif(nb * 12, -1, 1), ncol = 12)
mu_rich <- function(z) cbind(
  0.8 + 0.4 * z[, 1] - 0.3 * z[, 2] + 0.3 * z[, 3] * z[, 4],
  -0.9 + 0.5 * z[, 5] + 0.25 * z[, 6]^2 - 0.2,
  0.5 - 0.3 * z[, 7] + 0.2 * z[, 8],
  -0.4 + 0.4 * z[, 9] - 0.25 * z[, 10] * z[, 11])
mub <- mu_rich(zb)
u_skew <- { xs <- rlnorm(nb, 0, 0.8); (xs - mean(xs)) / sd(xs) }
u_gs <- rnorm(nb)
A <- matrix(arm$A, 4, 1)
V_skew <- mean(plogis((mub + u_skew %*% t(A)) %*% cv))
V_gaussequiv <- mean(plogis((mub + u_gs %*% t(A)) %*% cv))
cat(sprintf("\nskew arm V truth %.4f vs Gaussian-equivalent %.4f (gap %.4f)\n",
            V_skew, V_gaussequiv, V_skew - V_gaussequiv))

## figure: theta ratio by arm and estimator
df <- do.call(rbind, lapply(out, function(a) data.frame(
  arm = a$arm,
  estimator = c("Mixed logit", "Two-stage", "Pooled logit"),
  ratio = c(a$theta["mix_ratio"], a$theta["old_ratio"],
            a$theta["pooled_ratio"]))))
df$arm <- factor(df$arm, levels = rev(names(x$ARMS)))
gg <- ggplot(df, aes(x = ratio, y = arm, color = estimator,
                     shape = estimator)) +
  geom_vline(xintercept = 1, linewidth = 0.3, color = "grey60") +
  geom_point(size = 3, stroke = 0.9, fill = "white",
             position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Mixed logit" = "#a8003b",
                                "Two-stage" = "grey35",
                                "Pooled logit" = "grey65")) +
  scale_shape_manual(values = c("Mixed logit" = 19, "Two-stage" = 21,
                                "Pooled logit" = 24)) +
  labs(x = "Mean estimate / truth on large coefficients (1 = unbiased)",
       y = NULL, color = NULL, shape = NULL,
       title = "Latent-scale accuracy by regime",
       subtitle = sprintf("R = %d replicates per arm; N = %d", out[[1]]$R, x$N)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
ggsave(file.path(d, "figs", "oldvsnew_theta_ratio.png"), gg,
       width = 7.5, height = 3.6, dpi = 200)
cat("figure written\nDONE analyze\n")
