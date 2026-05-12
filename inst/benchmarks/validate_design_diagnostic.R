## inst/benchmarks/validate_design_diagnostic.R
##
## Build-time validation of sc_design_diagnostic()'s R^2_Z estimator
## against a controlled simulation grid where the true R^2_Z is known
## by construction.
##
## DGP (self-contained; matches paper §6 setup, linear class):
##
##   Z_i        ~ N(0, I_{P_Z})              (respondent covariates)
##   gamma_j    ~ N(0, c^2 I_{P_Z})          (Z-loading for coef j)
##   f_j(Z_i)   = gamma_j' Z_i               (centered + rescaled so
##                                            Var(f_j(Z)) = tau_z^2)
##   eta_ij     ~ N(0, sigma_eta^2)          (residual heterogeneity)
##   beta_ij    = mu_j + f_j(Z_i) + eta_ij
##   DeltaX_t   ~ Unif({-1, 0, 1})           (centered factor dummies)
##   Y_t        ~ Bernoulli(sigma(DeltaX_t' beta_{r(t)}))
##
## With this construction:
##
##   True R^2_{Z,k} = Var(f_k(Z))/(Var(f_k(Z)) + Var(eta_k))
##                  = tau_z^2 / (tau_z^2 + sigma_eta^2)
##
## We sweep tau_z (equivalently, the nominal R^2_Z), fit `scfit()`
## with `stage2 = "map_c5"`, call `sc_design_diagnostic()`, and record
## the package's R^2_hat_Z (per-coefficient + mean) against the truth.
##
## Outputs a CSV at inst/benchmarks/results_design_diagnostic.csv
## and prints summary correlation + bias statistics.

suppressPackageStartupMessages({
  library(sconjoint)
})

## ---- DGP --------------------------------------------------------

#' Generate one synthetic conjoint dataset with controlled R^2_Z
#'
#' @return list with long-form `data` (cols: respondent, task, profile,
#'   y, x1..xp, z1..zP_Z), the true beta matrix, true R^2_Z per coef.
sim_one <- function(N, T_tasks, p, P_Z, R2_Z_target,
                    sigma_eta = 0.5, mu_scale = 0.3, seed) {
  set.seed(seed)

  ## 1) Z_i ~ N(0, I)
  Z_resp <- matrix(rnorm(N * P_Z), nrow = N, ncol = P_Z)
  Z_resp <- scale(Z_resp, center = TRUE, scale = FALSE)

  ## 2) gamma_j ~ N(0, c^2 I) -- raw loadings
  gamma_raw <- matrix(rnorm(P_Z * p), nrow = P_Z, ncol = p)

  ## 3) f_j(Z) = gamma_j' Z, then center & rescale to Var = tau_z^2
  tau_z <- sqrt(R2_Z_target / (1 - R2_Z_target)) * sigma_eta
  f_Z <- Z_resp %*% gamma_raw
  for (j in seq_len(p)) {
    f_Z[, j] <- f_Z[, j] - mean(f_Z[, j])
    sd_f <- sd(f_Z[, j])
    if (sd_f > 1e-10) {
      f_Z[, j] <- f_Z[, j] * (tau_z / sd_f)
    }
  }

  ## 4) beta_ij = mu_j + f_j(Z) + eta_ij
  mu <- rnorm(p, sd = mu_scale)
  eta <- matrix(rnorm(N * p, sd = sigma_eta), nrow = N, ncol = p)
  beta_true <- sweep(f_Z + eta, 2, mu, "+")

  ## 5) DeltaX_t -- per-task profile pair from {-1, 0, 1}.
  ##    Encode as two profile rows with X_A in {0, 1} and X_B = 0 or 1
  ##    such that X_A - X_B reproduces a Unif{-1,0,1} difference.
  NT <- N * T_tasks
  X_A <- matrix(rbinom(NT * p, 1, 0.5), nrow = NT, ncol = p)
  X_B <- matrix(rbinom(NT * p, 1, 0.5), nrow = NT, ncol = p)
  DeltaX <- X_A - X_B

  resp_idx  <- rep(seq_len(N), each = T_tasks)
  task_idx  <- rep(rep(seq_len(T_tasks), each = 1L), times = N)
  beta_task <- beta_true[resp_idx, , drop = FALSE]
  logit_idx <- rowSums(DeltaX * beta_task)
  prob_A    <- 1 / (1 + exp(-logit_idx))
  Y_task    <- rbinom(NT, size = 1L, prob = prob_A)  ## 1 = A chosen

  ## 6) Build long-form data frame: 2 rows per task (profile A + B).
  data_long <- data.frame(
    respondent = rep(resp_idx, each = 2L),
    task       = rep(task_idx, each = 2L),
    profile    = rep(c("A", "B"), times = NT)
  )
  ## Outcome: A chosen -> profile A row gets y=1, B row y=0; vice versa.
  data_long$y <- as.integer(
    (data_long$profile == "A" & rep(Y_task == 1L, each = 2L)) |
    (data_long$profile == "B" & rep(Y_task == 0L, each = 2L))
  )

  ## Attribute columns
  X_long <- matrix(0, nrow = NT * 2L, ncol = p)
  X_long[seq(1, NT * 2L, by = 2L), ] <- X_A
  X_long[seq(2, NT * 2L, by = 2L), ] <- X_B
  colnames(X_long) <- paste0("x", seq_len(p))
  data_long <- cbind(data_long, X_long)

  ## Z columns (constant within respondent, so replicated 2T times)
  Z_long <- Z_resp[data_long$respondent, , drop = FALSE]
  colnames(Z_long) <- paste0("z", seq_len(P_Z))
  data_long <- cbind(data_long, Z_long)

  ## True per-coef R^2_Z is the same target by construction; record
  ## the empirical realization for transparency.
  emp_var_f   <- apply(f_Z, 2, var)
  emp_var_eta <- apply(eta, 2, var)
  R2_Z_true   <- emp_var_f / (emp_var_f + emp_var_eta)

  list(
    data       = data_long,
    beta_true  = beta_true,
    f_Z        = f_Z,
    eta        = eta,
    R2_Z_true  = R2_Z_true,
    R2_Z_mean_true = mean(R2_Z_true),
    R2_Z_target = R2_Z_target
  )
}

## ---- Validation harness ----------------------------------------

run_validation <- function(grid, K = 5L, n_epochs = 200L,
                           seed_base = 20260512L, verbose = TRUE) {
  results <- list()
  for (i in seq_len(nrow(grid))) {
    row <- grid[i, ]
    if (verbose) {
      cat(sprintf("[cell %d/%d] R2_Z=%.2f N=%d T=%d p=%d rep=%d\n",
                  i, nrow(grid), row$R2_Z_target, row$N, row$T_tasks,
                  row$p, row$rep))
    }
    sim <- sim_one(N = row$N, T_tasks = row$T_tasks, p = row$p,
                   P_Z = row$P_Z, R2_Z_target = row$R2_Z_target,
                   sigma_eta = 0.5, mu_scale = 0.3,
                   seed = seed_base + i)
    attr_vars <- paste0("x", seq_len(row$p))
    z_vars    <- paste0("z", seq_len(row$P_Z))
    form <- stats::as.formula(paste0(
      "y ~ ", paste(attr_vars, collapse = " + "),
      " | ", paste(z_vars, collapse = " + ")
    ))
    t0  <- Sys.time()
    fit <- tryCatch(
      sconjoint::scfit(form, data = sim$data,
                       respondent = "respondent",
                       task = "task", profile = "profile",
                       K = K, n_epochs = n_epochs,
                       stage2 = "map_c5", seed = 1L,
                       verbose = FALSE),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      results[[i]] <- cbind(row, data.frame(
        R2_Z_true_emp = sim$R2_Z_mean_true,
        R2_Z_hat_mean = NA_real_, R2_Z_hat_min = NA_real_,
        R2_Z_hat_max = NA_real_, secs = NA_real_,
        error = conditionMessage(fit)
      ))
      next
    }
    diag <- sconjoint::sc_design_diagnostic(fit)
    R2_hat <- diag$estimate$estimate$R2_Z
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    results[[i]] <- cbind(row, data.frame(
      R2_Z_true_emp = sim$R2_Z_mean_true,
      R2_Z_hat_mean = mean(R2_hat),
      R2_Z_hat_min  = min(R2_hat),
      R2_Z_hat_max  = max(R2_hat),
      secs          = secs,
      error         = NA_character_
    ))
    if (verbose) {
      cat(sprintf("    true=%.3f  hat_mean=%.3f  (%.1fs)\n",
                  sim$R2_Z_mean_true, mean(R2_hat), secs))
    }
  }
  do.call(rbind, results)
}

## ---- Grid (small but spans R^2_Z axis) -------------------------

grid <- expand.grid(
  R2_Z_target = c(0.10, 0.35, 0.75),
  N           = 500L,
  T_tasks     = c(5L, 10L),
  p           = 8L,
  P_Z         = 4L,
  rep         = 1:3,
  KEEP.OUT.ATTRS = FALSE
)

cat("Validation grid: ", nrow(grid), " cells\n", sep = "")
res <- run_validation(grid, K = 5L, n_epochs = 200L, verbose = TRUE)

out_path <- file.path("inst", "benchmarks",
                      "results_design_diagnostic.csv")
utils::write.csv(res, out_path, row.names = FALSE)
cat("\nWrote ", out_path, "\n", sep = "")

## ---- Summary --------------------------------------------------

cat("\n=== Per-cell results ===\n")
print(res[, c("R2_Z_target", "N", "T_tasks", "p", "rep",
              "R2_Z_true_emp", "R2_Z_hat_mean", "secs")])

ok <- is.na(res$error)
if (sum(ok) >= 3) {
  rho <- cor(res$R2_Z_true_emp[ok], res$R2_Z_hat_mean[ok])
  bias <- mean(res$R2_Z_hat_mean[ok] - res$R2_Z_true_emp[ok])
  abs_bias <- mean(abs(res$R2_Z_hat_mean[ok] - res$R2_Z_true_emp[ok]))
  cat(sprintf("\nPearson rho(R2_hat, R2_true) = %.3f\n", rho))
  cat(sprintf("Mean bias  (R2_hat - R2_true) = %+.3f\n", bias))
  cat(sprintf("Mean |bias|                   = %.3f\n", abs_bias))

  ## Conclusion banner
  cat("\n=== Validation verdict ===\n")
  if (rho >= 0.7) {
    cat("PASS: rho >= 0.7. Estimator tracks the truth; can drop experimental flag.\n")
  } else {
    cat("KEEP-EXPERIMENTAL: rho < 0.7. Keep `experimental = TRUE`.\n")
  }
} else {
  cat("\nNot enough successful cells to compute correlation.\n")
}
