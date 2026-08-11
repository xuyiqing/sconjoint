## Shared fixture for the mixed-logit (scmix) test files: N x T conjoint in
## long format simulated from the maintained model beta_i = mu(Z_i) + A u_i.
## Moved from test-mixed-likelihood.R so test-mixed-quantities.R can reuse it.
.mk_mixed_fixture <- function(N = 250L, T_i = 6L, seed = 5L) {
  withr::local_preserve_seed()
  set.seed(seed)
  z <- matrix(stats::runif(N * 2, -1, 1), N, 2)
  mu_fun <- function(z) cbind(0.7 + 0.4 * z[, 1], -0.8 + 0.5 * z[, 2])
  A_true <- matrix(c(0.8, 0.5), 2, 1)
  beta <- mu_fun(z) + stats::rnorm(N) %*% t(A_true)
  a1 <- sample(c("no", "yes"), 2 * N * T_i, TRUE)
  a2 <- sample(c("no", "yes"), 2 * N * T_i, TRUE)
  odd <- seq(1L, 2 * N * T_i, by = 2L)
  dxm <- cbind((a1[odd] == "yes") - (a1[odd + 1L] == "yes"),
               (a2[odd] == "yes") - (a2[odd + 1L] == "yes"))
  rid <- rep(seq_len(N), each = T_i)
  pr <- stats::plogis(rowSums(dxm * beta[rid, , drop = FALSE]))
  yA <- stats::rbinom(N * T_i, 1, pr)
  list(
    data = data.frame(
      resp_id = rep(rid, each = 2L),
      task_id = rep(rep(seq_len(T_i), N), each = 2L),
      profile_id = rep(1:2, N * T_i),
      a1 = a1, a2 = a2,
      z1 = rep(z[rid, 1], each = 2L), z2 = rep(z[rid, 2], each = 2L),
      choice = as.vector(rbind(yA, 1L - yA))
    ),
    mu_fun = mu_fun, A_true = A_true, z = z
  )
}

.fit_mixed_fixture <- function() {
  fx <- .mk_mixed_fixture()
  fit <- scmix(choice ~ a1 + a2 | z1 + z2, fx$data,
               respondent = "resp_id", task = "task_id",
               profile = "profile_id",
               q = 1L, K = 2L, n_epochs = 150L, seed = 11L)
  list(fit = fit, fx = fx)
}
