# Face-validity simulation for the integrated-likelihood mixed-logit
# estimator (scmix) -- feat/mixed-logit overnight prototype.
#
# Two arms, R replicates each:
#   A. well-specified: u_i ~ N(0,1)               (the maintained model)
#   B. misspecified:   u_i ~ centered-scaled Exp  (skewness 2; the known
#      soft spot -- the Gaussian mixing density sits inside the likelihood)
#
# Per replicate we record, for scmix / pooled logit / plug-in mean(mu_hat):
# theta estimates + SEs + CI coverage, polarization pi, counterfactual
# V(c), and the recovered loading scale.  R is small by design: this is
# face validity; the full battery is delegated to co-authors.
#
# Estimation only -- results saved as .rds; figures are drawn elsewhere.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

N <- 1500L; T_i <- 8L; R <- 40L
A_true <- matrix(c(0.9, 0.6, 0.0, 0.5), 4, 1)
mu_fun <- function(z) cbind(0.8 + 0.5 * z[, 1],
                            -0.9 + 0.4 * z[, 2]^2 - 0.2,
                            0.5 - 0.3 * z[, 1] * z[, 2],
                            -0.4 + 0.6 * z[, 1])
lev3 <- c("L1", "L2", "L3")
cv <- c(1, -1, 0, 1)  # contrast for V(c)

## population truths (theta identical across arms since E[u] = 0)
set.seed(1)
zbig <- matrix(runif(4e5, -1, 1), ncol = 2)
mubig <- mu_fun(zbig)
theta_true <- colMeans(mubig)
draw_u <- list(
  gauss = function(n) rnorm(n),
  skew  = function(n) (rexp(n) - 1)          # mean 0, var 1, skewness 2
)
truths <- lapply(draw_u, function(f) {
  ub <- f(nrow(mubig))
  bb <- mubig + ub %*% t(A_true)
  list(pi = colMeans(bb > 0), V = mean(plogis(bb %*% cv)))
})

one_rep <- function(r, arm) {
  set.seed(1000L * (arm == "skew") + r)
  z <- matrix(runif(N * 2, -1, 1), N, 2)
  beta <- mu_fun(z) + draw_u[[arm]](N) %*% t(A_true)
  n <- N * T_i
  a1 <- sample(c("no", "yes"), 2 * n, TRUE)
  a2 <- sample(c("no", "yes"), 2 * n, TRUE)
  a3 <- sample(lev3, 2 * n, TRUE)
  odd <- seq(1L, 2 * n, by = 2L)
  dxm <- cbind((a1[odd] == "yes") - (a1[odd + 1L] == "yes"),
               (a2[odd] == "yes") - (a2[odd + 1L] == "yes"),
               (a3[odd] == "L2") - (a3[odd + 1L] == "L2"),
               (a3[odd] == "L3") - (a3[odd + 1L] == "L3"))
  rid <- rep(seq_len(N), each = T_i)
  pr <- plogis(rowSums(dxm * beta[rid, , drop = FALSE]))
  yA <- rbinom(n, 1, pr)
  dat <- data.frame(
    resp_id = rep(rid, each = 2L),
    task_id = rep(rep(seq_len(T_i), N), each = 2L),
    profile_id = rep(1:2, n),
    a1 = a1, a2 = a2, a3 = a3,
    z1 = rep(z[rid, 1], each = 2L), z2 = rep(z[rid, 2], each = 2L),
    choice = as.vector(rbind(yA, 1L - yA)))

  fit <- scmix(choice ~ a1 + a2 + a3 | z1 + z2, dat,
               respondent = "resp_id", task = "task_id",
               profile = "profile_id",
               q = 1L, K = 3L, n_epochs = 400L, seed = r)
  th <- scmix_theta(fit, n_bins = 40L, seed = r)
  pol <- suppressWarnings(scmix_polarization(fit, n_bins = 40L, seed = r))
  vc <- scmix_counterfactual(fit, contrast = cv, n_bins = 40L, seed = r)
  pooled <- coef(glm(fit$y ~ 0 + fit$deltaX, family = binomial()))
  first <- !duplicated(fit$respondent_id)
  sd_hat <- sqrt(diag(Reduce(`+`, lapply(fit$A_folds, tcrossprod)) /
                        length(fit$A_folds)))
  list(arm = arm, r = r,
       theta = unname(th$estimate), theta_se = unname(th$se),
       theta_plugin = colMeans(fit$mu_hat[first, , drop = FALSE]),
       theta_pooled = unname(pooled),
       pi = unname(pol$estimate), pi_se = unname(pol$se),
       V = unname(vc$estimate), V_se = unname(vc$se),
       sd_hat = sd_hat)
}

res <- list()
t0 <- Sys.time()
for (arm in c("gauss", "skew")) {
  for (r in seq_len(R)) {
    res[[paste(arm, r)]] <- one_rep(r, arm)
    if (r %% 5 == 0) {
      cat(sprintf("[%s] %s rep %d/%d  elapsed %.1f min\n",
                  format(Sys.time(), "%H:%M:%S"), arm, r, R,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
      saveRDS(list(results = res, theta_true = theta_true, truths = truths,
                   A_true = A_true, cv = cv, N = N, T_i = T_i),
              file.path(OUT_DIR, "facevalidity_results.rds"))
    }
  }
}
saveRDS(list(results = res, theta_true = theta_true, truths = truths,
             A_true = A_true, cv = cv, N = N, T_i = T_i),
        file.path(OUT_DIR, "facevalidity_results.rds"))
cat("DONE facevalidity\n")
