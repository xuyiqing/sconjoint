# Ported from conjointRUM 0.1.3 inst/examples/synthetic.R (Avidit Acharya); only the
# package name changes. Run with: Rscript -e 'source(system.file("examples", "rum-synthetic.R", package = "sconjoint"))'

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           VECLIB_MAXIMUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(sconjoint)

set.seed(904)
n <- 160L
d <- lapply(seq_len(n), function(i) matrix(sample(c(-1, 0, 1), 16, TRUE), 8, 2))
B <- rum_mean_basis(cbind(intercept = 1, z = rep(c(-1, 1), n / 2)),
                    c("first", "second"))
design <- rum_data(rep(list(rep(0, 8)), n), d, B,
  feature_names = c("first", "second"),
  metadata = list(orientation = "Synthetic y=1 means physical left; d=left minus right"))
truth <- rum_parameters(design, .15, c(.4, -.2, .2, .15), c(.8, -.45), .45)
data <- rum_simulate(design, truth, seed = 113)
fit <- rum_fit(data)
print(fit)
print(rum_targets(fit, list(first = c(1, 0), second = c(0, 1)),
                  reference = "fixed")$table)

# Three predeclared bounded moments per respondent, with no outcome selection.
mw <- matrix(0, length(data$y), 3); mw[, 1] <- 1 / 8
pw <- matrix(0, sum(data$T * (data$T - 1) / 2), 3); pw[, 2] <- 1 / choose(8, 2)
cw <- matrix(0, sum(data$T + 1), 3)
cw[rep(seq(0, by = 9, length.out = n), each = 2) + rep(c(1, 9), n), 3] <- 1
bank <- rum_moment_bank(design, mw, pw, cw,
                        c("mean_choice", "mean_pair_product", "extreme_count"))
known <- rum_assess(data, truth, bank, rum_quadrature(31), nsim = 999)
fitted <- rum_assess(data, bank = bank, estimated = TRUE, fit = fit, nsim = 999)
cat("\nKnown-parameter moment assessment:\n"); print(known$table)
cat("Family p-value:", known$p_value, "\n")
cat("\nSame-sample fitted moment assessment:\n"); print(fitted$table)
cat("Family p-value:", fitted$p_value, "\n")
cat("This example is an implementation demonstration, not a coverage study.\n")
