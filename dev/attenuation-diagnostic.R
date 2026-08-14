# Why is there no visible de-attenuation of scmix theta against the
# two-stage estimates on br2017/sw2022, even though the fitted residual
# heterogeneity implies wedge factors of 0.83-0.88?
#
# Diagnostic A: fit a plain pooled conditional logit (no Z at all) -- the
#   textbook projection benchmark. If theta_pooled is visibly attenuated
#   relative to BOTH the two-stage and the mixed estimates, the two-stage
#   DNN with rich, nearly respondent-unique Z already sits close to the
#   latent mean in these data (it partially absorbs heterogeneity through
#   mu(Z)), so the wedge shows against the no-Z projection, not against
#   the two-stage.
#
# Diagnostic B: model-implied wedge. Simulate choices from the fitted
#   scmix model on the observed design (respondents' own deltaX rows,
#   their mu_hat, the fold-averaged loading), fit the pooled logit to the
#   simulated data, and compare against mean(mu_hat). This is the wedge
#   the fitted model PREDICTS for this design. Compare with the observed
#   pooled-vs-mixed gap from Diagnostic A.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))
OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")

diag_app <- function(name) {
  cat(sprintf("\n===== %s =====\n", name))
  mx <- readRDS(file.path(OUT_DIR, paste0("scmix_fit_", name, ".rds")))
  res <- readRDS(file.path(OUT_DIR, paste0("app_", name, ".rds")))

  ## A: plain pooled conditional logit on the real data
  pool <- coef(glm(mx$y ~ 0 + mx$deltaX, family = binomial()))
  names(pool) <- mx$attr_names

  first <- !duplicated(mx$respondent_id)
  mu_resp <- mx$mu_hat[first, , drop = FALSE]
  theta_mix <- res$after$theta
  theta_two <- unname(res$before$theta)

  cmp <- data.frame(
    pooled_noZ = round(unname(pool), 4),
    two_stage = round(theta_two, 4),
    mixed = round(theta_mix, 4),
    row.names = mx$attr_names)
  print(cmp)
  big <- abs(theta_mix) > 0.05
  cat("mean |pooled|/|mixed| on coords with |theta_mix|>0.05:",
      round(mean(abs(pool[big]) / abs(theta_mix[big])), 3), "\n")
  cat("mean |two-stage|/|mixed| on the same coords:",
      round(mean(abs(theta_two[big]) / abs(theta_mix[big])), 3), "\n")

  ## B: model-implied wedge on this design
  Sig <- Reduce(`+`, lapply(mx$A_folds, tcrossprod)) / length(mx$A_folds)
  eS <- eigen(Sig, symmetric = TRUE)
  A_sim <- eS$vectors[, seq_len(mx$q), drop = FALSE] %*%
    diag(sqrt(pmax(eS$values[seq_len(mx$q)], 0)), mx$q)

  set.seed(42)
  REP <- 25L                      # replicate the whole design REP times
  ridx <- as.integer(factor(mx$respondent_id, levels = unique(mx$respondent_id)))
  n <- nrow(mx$deltaX)
  theta_sim_pool <- 0
  for (r in seq_len(REP)) {
    u <- matrix(rnorm(nrow(mu_resp) * mx$q), ncol = mx$q)
    beta <- mu_resp + u %*% t(A_sim)
    pr <- plogis(rowSums(mx$deltaX * beta[ridx, , drop = FALSE]))
    ysim <- as.numeric(runif(n) < pr)
    theta_sim_pool <- theta_sim_pool +
      coef(glm(ysim ~ 0 + mx$deltaX, family = binomial())) / REP
  }
  latent_mean <- colMeans(mu_resp)
  pred <- data.frame(
    latent_mean = round(latent_mean, 4),
    predicted_pooled = round(unname(theta_sim_pool), 4),
    predicted_ratio = round(unname(theta_sim_pool) / latent_mean, 3),
    observed_pooled = round(unname(pool), 4),
    observed_ratio = round(unname(pool) / theta_mix, 3),
    row.names = mx$attr_names)
  print(pred)
  saveRDS(list(cmp = cmp, pred = pred), file.path(OUT_DIR, paste0("atten_diag_", name, ".rds")))
}

diag_app("br2017")
diag_app("sw2022")
cat("\nDONE attenuation-diagnostic\n")
