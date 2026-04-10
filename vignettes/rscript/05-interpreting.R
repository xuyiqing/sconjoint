library(sconjoint)
cat("interpreting\n")
set.seed(1)
n_resp <- 120L
n_tasks <- 3L
rid <- rep(seq_len(n_resp), each = n_tasks * 2L)
tid <- rep(rep(seq_len(n_tasks), each = 2L), n_resp)
pos <- rep(c(1L, 2L), n_resp * n_tasks)
x1 <- rbinom(length(rid), 1L, 0.5)
x2 <- rbinom(length(rid), 1L, 0.5)
z1 <- rep(rnorm(n_resp), each = n_tasks * 2L)
## True utility: preferences rotate with z1.
u <- (0.6 + 0.5 * z1) * x1 + (-0.3 + 0.4 * z1) * x2
toy <- data.frame(rid, tid, pos, x1, x2, z1)
toy$y <- 0L
for (i in seq_len(n_resp)) for (t in seq_len(n_tasks)) {
  idx <- which(toy$rid == i & toy$tid == t)
  pick <- which.max(u[idx] + -log(-log(runif(length(idx)))))
  toy$y[idx[pick]] <- 1L
}
fit_toy <- scfit(y ~ x1 + x2 | z1,
                 data = toy, respondent = "rid",
                 task = "tid", profile = "pos",
                 K = 2L, n_epochs = 40L, seed = 1L)
## Compare to a homogeneous baseline.
glm_base <- glm(y ~ x1 + x2, data = toy, family = binomial())
coef(glm_base)
## Structural subgroup estimates along z1:
hi <- fit_toy$Z[, "z1"] >  0
sc_subgroup(fit_toy, list(low = !hi, high = hi))
