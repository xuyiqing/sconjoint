# T1(c): does dropping the loading-influence correction for theta change the
# point estimates, SEs, and coverage? Two arms from dev/sim-old-vs-new.R:
# richZ_gauss (T=8, where the 2026-08-10 probe found the term minor) and
# T3_smallT (T=3, where it reached 0.44 SE with a positive mean). Same seeds
# and data generation as sim-old-vs-new.R so the corrected column must
# reproduce the stored theta_mix values replicate by replicate; scfit is
# skipped (not needed for this comparison). Both theta variants come from the
# SAME scmix fit: corrected = scmix_theta's psi; uncorrected = mu_resp + C
# with no .scmix_A_adjust term.
suppressMessages(devtools::load_all("~/GitHub/sconjoint", quiet = TRUE))

OUT_DIR <- path.expand("~/Dropbox/Research_Hub/Projects/sconjoint/mixedlogit_prototype")
R_REPS <- 20L
N <- 1200L

draw_u <- list(gauss = function(n) rnorm(n))

mu_rich <- function(z) cbind(
  0.8 + 0.4 * z[, 1] - 0.3 * z[, 2] + 0.3 * z[, 3] * z[, 4],
  -0.9 + 0.5 * z[, 5] + 0.25 * z[, 6]^2 - 0.2,
  0.5 - 0.3 * z[, 7] + 0.2 * z[, 8],
  -0.4 + 0.4 * z[, 9] - 0.25 * z[, 10] * z[, 11])

## arm_seed_index preserves the sim-old-vs-new.R seed formula, where the arms
## sat at positions 1 (richZ_gauss) and 4 (T3_smallT) of the ARMS list
ARMS <- list(
  richZ_gauss = list(T_i = 8L, u = "gauss", A = c(0.9, 0.6, 0, 0.5),
                     arm_seed_index = 1L),
  T3_smallT   = list(T_i = 3L, u = "gauss", A = c(0.9, 0.6, 0, 0.5),
                     arm_seed_index = 4L)
)

set.seed(1)
zb <- matrix(runif(4e5 * 12, -1, 1), ncol = 12)
theta_true <- colMeans(mu_rich(zb))
cat("theta truth:", round(theta_true, 4), "\n")

one_rep <- function(arm_name, r) {
  arm <- ARMS[[arm_name]]
  set.seed(20000 + 999 * arm$arm_seed_index + r)
  T_i <- arm$T_i
  z <- matrix(runif(N * 12, -1, 1), N, 12)
  mu_i <- mu_rich(z)
  zdf_cols <- as.data.frame(z); names(zdf_cols) <- paste0("z", 1:12)
  zvars <- paste0("z", 1:12)
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

  mx <- scmix(fml, dat, respondent = "respondent", task = "task",
              profile = "profile", q = 1L, K = 5L, n_epochs = 500L, seed = r)

  ## both variants from the same fit and the same prep
  pr <- sconjoint:::.scmix_prep(mx, seed = r)
  psi_no <- pr$mu_resp + pr$C
  psi_co <- psi_no
  for (k in seq_len(pr$p)) {
    e_k <- matrix(0, pr$N, pr$p); e_k[, k] <- 1
    psi_co[, k] <- psi_co[, k] - sconjoint:::.scmix_A_adjust(pr, e_k)
  }
  est_se <- function(psi) list(est = colMeans(psi),
                               se = sqrt(pmax(apply(psi, 2L, var), 0) / nrow(psi)))
  co <- est_se(psi_co); no <- est_se(psi_no)

  list(arm = arm_name, r = r,
       theta_corr = unname(co$est), theta_corr_se = unname(co$se),
       theta_nocorr = unname(no$est), theta_nocorr_se = unname(no$se))
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
      saveRDS(list(results = res, theta_true = theta_true, ARMS = ARMS, N = N),
              file.path(OUT_DIR, "t1_correction_results.rds"))
    }
  }
}
saveRDS(list(results = res, theta_true = theta_true, ARMS = ARMS, N = N),
        file.path(OUT_DIR, "t1_correction_results.rds"))
cat("DONE t1-correction\n")
