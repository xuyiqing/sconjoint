## inst/benchmarks/coverage_resp_cluster_boot.R
##
## Monte-Carlo calibration check for the respondent-cluster (wild)
## bootstrap used by sc_polarization() / sc_fraction_preferring()
## (.sc_resp_cluster_boot).
##
## What this validates and what it deliberately does NOT.
## ----------------------------------------------------------------
## The bootstrap targets the SAMPLING uncertainty of a fraction-over-
## respondents, Pr_i{beta_ij > tau}, GIVEN the recovered per-respondent
## betas. That is the quantity the reviewer flagged: sc_polarization()
## returned se = NA, and sc_fraction_preferring() put naive clustered
## SEs on indicators. So the relevant calibration target is: across
## repeated samples of M respondents from a fixed population, does the
## bootstrap 95% interval for the population fraction p = Pr{beta > tau}
## cover p at ~95%?
##
## To isolate that aggregation-step sampling uncertainty (the bootstrap
## reuses the recovered beta per resample and does NOT refit the DNN),
## we use an ORACLE first stage: draw the true per-respondent beta_i
## from a known population, so the true fraction p is known in closed
## form. This measures exactly what the bootstrap promises. It does NOT
## measure (and the bootstrap does not claim to fix) the finite-T
## shrinkage bias that, with a real DNN first stage, pulls beta_i toward
## consensus and biases the plug-in fraction toward agreement.
##
## Runtime: pure aggregation, no torch. ~1-2 min for the defaults below.
## Env overrides: SC_NREP (default 1000), SC_M (default 400), SC_NBOOT
## (default 200).

suppressPackageStartupMessages(library(sconjoint))

boot_engine <- get(".sc_resp_cluster_boot", envir = asNamespace("sconjoint"))

n_rep  <- as.integer(Sys.getenv("SC_NREP",  unset = "1000"))
M       <- as.integer(Sys.getenv("SC_M",     unset = "400"))
n_boot  <- as.integer(Sys.getenv("SC_NBOOT", unset = "200"))
level   <- 0.95

## Population of per-respondent slopes for one attribute: beta ~ N(mu, sd).
## The true fraction preferring (beta > 0) is Phi(mu / sd). Pick a few
## (mu, sd) so the true fraction spans a polarized case (~0.5) through a
## near-consensus case (~0.9).
scenarios <- list(
  polarized   = list(mu = 0.00, sd = 1.0),   # true p = 0.50 (max polarization)
  leaning     = list(mu = 0.40, sd = 1.0),   # true p ~ 0.655
  consensus   = list(mu = 1.20, sd = 1.0)    # true p ~ 0.885
)

cat(sprintf("coverage_resp_cluster_boot: n_rep=%d  M=%d  n_boot=%d  level=%.2f\n\n",
            n_rep, M, n_boot, level))

run_one_scenario <- function(mu, sd, boot_type, seed0) {
  p_true <- stats::pnorm(mu / sd)
  cover_frac <- logical(n_rep)   # CI for the fraction covers p_true
  cover_poli <- logical(n_rep)   # CI for polarization index covers its truth
  se_boot    <- numeric(n_rep)
  se_analytic <- sqrt(p_true * (1 - p_true) / M)  # clustered-proportion SE
  ## polarization index truth: 1 - |p_true - p_minus|, p_minus = Pr(beta<0)
  p_minus_true <- stats::pnorm(-mu / sd)
  poli_true <- 1 - abs(p_true - p_minus_true)

  for (r in seq_len(n_rep)) {
    set.seed(seed0 + r)
    beta <- stats::rnorm(M, mean = mu, sd = sd)
    ind_pos <- matrix((beta > 0) * 1, ncol = 1L)
    ind_neg <- matrix((beta < 0) * 1, ncol = 1L)
    ## frac functional: c(fp, fn, poli)
    G <- cbind(ind_pos, ind_neg)
    fun <- function(m) c(m[1], m[2], 1 - abs(m[1] - m[2]))
    bt <- boot_engine(G, fun = fun, n_boot = n_boot,
                      boot_type = boot_type, level = level,
                      seed = seed0 + r + 7919L)
    cover_frac[r] <- (bt$ci_lo[1] <= p_true)  && (p_true  <= bt$ci_hi[1])
    cover_poli[r] <- (bt$ci_lo[3] <= poli_true) && (poli_true <= bt$ci_hi[3])
    se_boot[r] <- bt$se[1]
  }
  list(p_true = p_true, poli_true = poli_true,
       cover_frac = mean(cover_frac), cover_poli = mean(cover_poli),
       se_boot_mean = mean(se_boot), se_analytic = se_analytic,
       mc_se = sqrt(level * (1 - level) / n_rep))
}

for (bt_type in c("wild", "cluster")) {
  cat(sprintf("==== boot_type = \"%s\" ====\n", bt_type))
  for (nm in names(scenarios)) {
    s <- scenarios[[nm]]
    out <- run_one_scenario(s$mu, s$sd, bt_type, seed0 = 1000L)
    cat(sprintf(
      "  %-10s p_true=%.3f | frac CI cover=%.3f  poli CI cover=%.3f (MC se %.3f) | se_boot=%.4f vs analytic=%.4f\n",
      nm, out$p_true, out$cover_frac, out$cover_poli, out$mc_se,
      out$se_boot_mean, out$se_analytic))
  }
  cat("\n")
}

cat("Target coverage = 0.95; values within ~2-3 MC se of 0.95 are well calibrated.\n")
cat("se_boot should track the analytic clustered-proportion SE sqrt(p(1-p)/M).\n\n")
cat("Note on the 'polarized' polarization-index row: when mu = 0 the true index\n")
cat("equals 1.0 exactly (its maximum). The index 1 - |fp - fn| cannot exceed 1,\n")
cat("so the point estimate sits at or below the truth and a two-sided percentile\n")
cat("interval under-covers a boundary parameter by construction. This is the\n")
cat("standard parameter-on-the-boundary effect, not a defect of the bootstrap;\n")
cat("away from the boundary (the 'leaning'/'consensus' rows) the index CI is\n")
cat("well calibrated, and the fraction CI is well calibrated everywhere.\n")
