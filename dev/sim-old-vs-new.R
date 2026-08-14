# Old (two-stage scfit) versus new (integrated mixed logit scmix), head to
# head on the same replicates. Four arms isolate the trade-off:
#
#   richZ_gauss   T=8, Gaussian residual factor, 12 informative moderators.
#                 Rich Z lets the two-stage mean network absorb part of the
#                 residual heterogeneity (the attenuation-question mechanism);
#                 both estimators should do well on theta.
#   coarseZ_gauss T=8, Gaussian, 2 coarse categorical moderators (9 Z cells).
#                 The two-stage first stage cannot escape the projection here:
#                 this is where the old approach's latent-scale attenuation
#                 should appear and the mixed logit should remove it.
#   skew_strong   T=8, rich Z, strongly skewed residual factor with larger
#                 loadings, and a contrast whose integrated share truth moves
#                 materially under skewness. The mixed logit's Gaussian
#                 assumption is wrong here; the old approach's probability-
#                 scale quantities are supposed to be the robust ones.
#   T3_smallT     T=3, Gaussian, rich Z. The weak-identification regime for
#                 the loading matrix (manufactured-heterogeneity floor).
#
# Per replicate both estimators run on identical data; recorded per arm:
# theta (scmix corrected + SE; scfit DML + SE; pooled logit), sign shares
# (scmix pi with NA policy; scfit MAP sign fractions), integrated share
# (scmix V(c) + SE; scfit MAP plug-in share), plus truths integrated over
# the TRUE residual law. Estimation only; summaries via
# dev/sim-old-vs-new-analyze.R.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/ConjointStructural/mixedlogit_prototype")
R_REPS <- 20L
N <- 1200L
cv <- c(1, -1, 0, 1)     # contrast for the integrated share

## ---- residual laws ----
draw_u <- list(
  gauss = function(n) rnorm(n),
  skew  = function(n) { x <- rlnorm(n, 0, 0.8); (x - mean(x)) / sd(x) }
)

## ---- mean structures ----
mu_rich <- function(z) cbind(
  0.8 + 0.4 * z[, 1] - 0.3 * z[, 2] + 0.3 * z[, 3] * z[, 4],
  -0.9 + 0.5 * z[, 5] + 0.25 * z[, 6]^2 - 0.2,
  0.5 - 0.3 * z[, 7] + 0.2 * z[, 8],
  -0.4 + 0.4 * z[, 9] - 0.25 * z[, 10] * z[, 11])
mu_coarse <- function(g1, g2) cbind(
  0.8 + 0.4 * (g1 == 2) - 0.5 * (g1 == 3),
  -0.9 + 0.5 * (g2 == 2) + 0.3 * (g2 == 3),
  0.5 - 0.4 * (g1 == 3),
  -0.4 + 0.35 * (g2 == 3))

ARMS <- list(
  richZ_gauss  = list(T_i = 8L, u = "gauss", zform = "rich",
                      A = c(0.9, 0.6, 0, 0.5)),
  coarseZ_gauss = list(T_i = 8L, u = "gauss", zform = "coarse",
                       A = c(0.9, 0.6, 0, 0.5)),
  skew_strong  = list(T_i = 8L, u = "skew", zform = "rich",
                      A = c(1.3, 0.9, 0, 0.7)),
  T3_smallT    = list(T_i = 3L, u = "gauss", zform = "rich",
                      A = c(0.9, 0.6, 0, 0.5))
)

## truths per arm (theta identical across u-laws with E[u] = 0; pi and V not)
set.seed(1)
truths <- lapply(ARMS, function(arm) {
  nbig <- 4e5
  if (arm$zform == "rich") {
    zb <- matrix(runif(nbig * 12, -1, 1), ncol = 12)
    mub <- mu_rich(zb)
  } else {
    g1 <- sample(1:3, nbig, TRUE); g2 <- sample(1:3, nbig, TRUE)
    mub <- mu_coarse(g1, g2)
  }
  ub <- draw_u[[arm$u]](nbig)
  bb <- mub + ub %*% t(matrix(arm$A, 4, 1))
  list(theta = colMeans(mub), pi = colMeans(bb > 0),
       V = mean(plogis(bb %*% cv)))
})
cat("V truths per arm:", sapply(truths, function(t) round(t$V, 4)), "\n")
cat("V truth gauss-vs-skew check (arms 1 vs 3 use different A too):",
    round(truths$richZ_gauss$V - truths$skew_strong$V, 4), "\n")

one_rep <- function(arm_name, r) {
  arm <- ARMS[[arm_name]]
  set.seed(20000 + 999 * match(arm_name, names(ARMS)) + r)
  T_i <- arm$T_i
  if (arm$zform == "rich") {
    z <- matrix(runif(N * 12, -1, 1), N, 12)
    mu_i <- mu_rich(z)
    zdf_cols <- as.data.frame(z); names(zdf_cols) <- paste0("z", 1:12)
    zvars <- paste0("z", 1:12)
  } else {
    g1 <- sample(1:3, N, TRUE); g2 <- sample(1:3, N, TRUE)
    mu_i <- mu_coarse(g1, g2)
    zdf_cols <- data.frame(g1_2 = as.numeric(g1 == 2), g1_3 = as.numeric(g1 == 3),
                           g2_2 = as.numeric(g2 == 2), g2_3 = as.numeric(g2 == 3))
    zvars <- names(zdf_cols)
  }
  beta <- mu_i + draw_u[[arm$u]](N) %*% t(matrix(arm$A, 4, 1))
  n <- N * T_i
  attrs <- replicate(4, sample(c("no", "yes"), 2 * n, TRUE), simplify = FALSE)
  odd <- seq(1L, 2 * n, by = 2L)
  dxm <- sapply(attrs, function(a) (a[odd] == "yes") - (a[odd + 1L] == "yes"))
  rid <- rep(seq_len(N), each = T_i)
  yA <- rbinom(n, 1, plogis(rowSums(dxm * beta[rid, , drop = FALSE])))
  dat <- data.frame(
    respondent = rep(rid, each = 2L),
    task = rep(rep(seq_len(T_i), N), each = 2L),
    profile = rep(1:2, n),
    x1 = attrs[[1]], x2 = attrs[[2]], x3 = attrs[[3]], x4 = attrs[[4]],
    zdf_cols[rep(rid, each = 2L), , drop = FALSE],
    choice = as.vector(rbind(yA, 1L - yA)))
  fml <- stats::as.formula(paste("choice ~ x1 + x2 + x3 + x4 |",
                                 paste(zvars, collapse = " + ")))

  ## OLD: two-stage production pipeline
  old <- scfit(fml, dat, respondent = "respondent", task = "task",
               profile = "profile", K = 5L, n_epochs = 500L, seed = r,
               parallel = TRUE, n_cores = 5L)
  first_o <- !duplicated(old$respondent_id)
  old_beta <- old$beta_hat[first_o, , drop = FALSE]

  ## NEW: integrated mixed logit
  mx <- scmix(fml, dat, respondent = "respondent", task = "task",
              profile = "profile", q = 1L, K = 5L, n_epochs = 500L, seed = r)
  th <- scmix_theta(mx, seed = r)
  pol <- suppressWarnings(scmix_polarization(mx, seed = r))
  vc <- scmix_counterfactual(mx, contrast = cv, seed = r)
  pooled <- coef(glm(mx$y ~ 0 + mx$deltaX, family = binomial()))
  sd_hat <- sqrt(diag(Reduce(`+`, lapply(mx$A_folds, tcrossprod)) /
                        length(mx$A_folds)))

  list(arm = arm_name, r = r,
       theta_mix = unname(th$estimate), theta_mix_se = unname(th$se),
       theta_old = unname(old$theta),
       theta_old_se = sqrt(pmax(diag(old$vcov), 0)),
       theta_pooled = unname(pooled),
       pi_mix = unname(pol$estimate), pi_mix_se = unname(pol$se),
       pi_old_map = colMeans(old_beta > 0),
       V_mix = unname(vc$estimate), V_mix_se = unname(vc$se),
       V_old_map = mean(plogis(old_beta %*% cv)),
       sd_hat = sd_hat)
}

res <- list()
t0 <- Sys.time()
for (arm_name in names(ARMS)) {
  for (r in seq_len(R_REPS)) {
    res[[paste(arm_name, r)]] <- tryCatch(
      one_rep(arm_name, r),
      error = function(e) {
        cat(sprintf("[%s] %s rep %d ERROR: %s\n",
                    format(Sys.time(), "%H:%M:%S"), arm_name, r,
                    conditionMessage(e)))
        NULL
      })
    if (r %% 2 == 0) {
      cat(sprintf("[%s] %s rep %d/%d  elapsed %.1f min\n",
                  format(Sys.time(), "%H:%M:%S"), arm_name, r, R_REPS,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
      saveRDS(list(results = res, truths = truths, ARMS = ARMS, cv = cv, N = N),
              file.path(OUT_DIR, "oldvsnew_results.rds"))
    }
  }
}
saveRDS(list(results = res, truths = truths, ARMS = ARMS, cv = cv, N = N),
        file.path(OUT_DIR, "oldvsnew_results.rds"))
cat("DONE old-vs-new\n")
