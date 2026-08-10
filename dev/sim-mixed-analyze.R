# Summarize the face-validity simulation into the memo's tables.
# Reads facevalidity_results.rds; prints (and saves) per-arm summaries.
res_path <- path.expand(
  "~/Dropbox/Research_Hub/Projects/sconjoint/mixedlogit_prototype/facevalidity_results.rds")
x <- readRDS(res_path)
res <- x$results
theta_true <- x$theta_true
big <- which(abs(theta_true) >= 0.3)   # large coefficients

arm_summary <- function(arm) {
  rr <- Filter(function(r) r$arm == arm, res)
  R <- length(rr)
  if (R == 0) return(NULL)
  g <- function(f) t(vapply(rr, f, numeric(length(theta_true))))
  th <- g(function(r) r$theta); se <- g(function(r) r$theta_se)
  plug <- g(function(r) r$theta_plugin); pool <- g(function(r) r$theta_pooled)
  pi_m <- g(function(r) r$pi); pi_se <- g(function(r) r$pi_se)
  sdh <- g(function(r) r$sd_hat)
  Vv <- vapply(rr, function(r) r$V, numeric(1))
  Vse <- vapply(rr, function(r) r$V_se, numeric(1))
  pit <- x$truths[[arm]]$pi; Vt <- x$truths[[arm]]$V

  cover <- function(est, se, truth)
    colMeans(abs(sweep(est, 2, truth)) <= 1.96 * se)
  ratio <- function(est) colMeans(sweep(est, 2, theta_true, `/`))[big]

  list(
    R = R,
    theta_ratio = c(scmix = mean(ratio(th)), plugin = mean(ratio(plug)),
                    pooled = mean(ratio(pool))),
    theta_bias = colMeans(sweep(th, 2, theta_true)),
    theta_rmse = sqrt(colMeans(sweep(th, 2, theta_true)^2)),
    theta_cover = cover(th, se, theta_true),
    theta_cover_mean = mean(cover(th, se, theta_true)[big]),
    pi_bias = colMeans(sweep(pi_m, 2, pit)),
    pi_cover = cover(pi_m, pi_se, pit),
    V_bias = mean(Vv) - Vt,
    V_cover = mean(abs(Vv - Vt) <= 1.96 * Vse),
    sd_hat_mean = colMeans(sdh),
    mc_se_theta = apply(th, 2, sd) / sqrt(R)
  )
}

out <- lapply(c("gauss", "skew"), arm_summary)
names(out) <- c("gauss", "skew")
for (arm in names(out)) {
  s <- out[[arm]]
  if (is.null(s)) next
  cat(sprintf("\n==== arm: %s (R = %d) ====\n", arm, s$R))
  cat("theta attenuation ratio, large coords (1 = unbiased):\n")
  print(round(s$theta_ratio, 3))
  cat("theta coverage per coord:", round(s$theta_cover, 2), "\n")
  cat("theta coverage mean (large):", round(s$theta_cover_mean, 3), "\n")
  cat("theta bias:", round(s$theta_bias, 3), " | MC SE:",
      round(s$mc_se_theta, 3), "\n")
  cat("pi bias:", round(s$pi_bias, 3), " | pi coverage:",
      round(s$pi_cover, 2), "\n")
  cat(sprintf("V(c): bias %.3f, coverage %.2f\n", s$V_bias, s$V_cover))
  cat("recovered loading SDs (true 0.9/0.6/0/0.5):",
      round(s$sd_hat_mean, 3), "\n")
}
saveRDS(out, sub("results", "summary", res_path))
cat("\nDONE sim analysis\n")
