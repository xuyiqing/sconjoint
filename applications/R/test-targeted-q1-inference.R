#!/usr/bin/env Rscript
## Fast tests for the targeted application runner helpers.

options(stringsAsFactors = FALSE, warn = 1)
root <- normalizePath(Sys.getenv(
  "SCONJOINT_ROOT", unset = path.expand("~/GitHub/sconjoint")),
  mustWork = TRUE)
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
source(file.path(root, "applications/R/targeted_q1_inference.R"))

passed <- 0L
check <- function(label, condition) {
  if (!isTRUE(condition)) stop("FAIL  ", label, call. = FALSE)
  passed <<- passed + 1L
  cat("PASS  ", label, "\n", sep = "")
}

attr_names <- c("penalty", "benefit", "other")
mu <- rbind(c(-0.5, 0.8, 0), c(-0.2, 0.1, 0), c(0.3, -0.1, 0),
            c(-0.7, 1.2, 0))
Sigma <- matrix(c(0.4, 0.1, 0, 0.1, 0.3, 0, 0, 0, 0.2), 3, 3)
rid <- paste0("r", seq_len(nrow(mu)))
cp <- tq1_unit_contrast(attr_names, "penalty")
cb <- tq1_unit_contrast(attr_names, "benefit")
comp <- scmix_inference_target(type = "compensating", penalty = cp,
  benefit = cb, amount = 1, variance_floor = 1e-6)
sign <- scmix_inference_target(type = "sign", contrast = cp + cb,
                              variance_floor = 1e-6)
args <- list(mu = mu, kappa = 0.2, Sigma = Sigma, Z = matrix(0, 4, 1),
             respondent_id = rid, fold = 1L, attr_names = attr_names)
a <- do.call(comp, args); b <- do.call(sign, args)
check("compensating target equals sign target on c_p + c_b",
      max(abs(a$value - b$value)) < 1e-14 &&
        max(abs(a$d_mu - b$d_mu)) < 1e-14 &&
        max(abs(a$d_Sigma - b$d_Sigma)) < 1e-14)

g <- stats::setNames(c(1, 0, 1, 0), rid)
wrapped <- tq1_subgroup_target(comp, g)
w <- do.call(wrapped, args)
check("subgroup wrapper returns E(gf) and E(g)",
      abs(mean(w$value[, 1L]) / mean(w$value[, 2L]) -
            mean(a$value[g == 1, 1L])) < 1e-14)
check("subgroup wrapper zeros excluded-row derivatives",
      max(abs(w$d_mu[g == 0, 1L, ])) == 0 &&
        max(abs(w$d_Sigma[g == 0, 1L, , ])) == 0)

oppose <- scmix_inference_target(type = "sign", contrast = -cp,
                                 variance_floor = 1e-6)
op <- do.call(oppose, args)$value[, 1L]
favor <- do.call(scmix_inference_target(type = "sign", contrast = cp,
  variance_floor = 1e-6), args)$value[, 1L]
check("reversed action direction reports opposition", max(abs(op + favor - 1)) < 1e-14)

fake_cov <- diag(c(0.01, 0.02, 0.03))
dimnames(fake_cov) <- list(c("h", "h2", "hr"), c("h", "h2", "hr"))
fake <- structure(list(
  estimate = c(h = 0.2, h2 = 0.3, hr = 0.4),
  diagnostic_covariance = fake_cov,
  status = "conditional_unverified", inference_available = FALSE,
  analysis_signature = "synthetic"), class = c("scmix_dml", "list"))
tr <- scmix_delta_transform(fake, type = "directional_heterogeneity",
  primitives = c("h", "h2", "hr"), total_margin = 1e-4)
check("directional decomposition obeys H_T = H_Z + H_R",
      abs(tr$estimate[["H_T"]] - tr$estimate[["H_Z"]] -
            tr$estimate[["H_R"]]) < 1e-14)
check("unverified primitives cannot produce formal intervals",
      !isTRUE(tr$inference_available) && all(is.na(tr$ci_lower)))

fit_path <- file.path(root,
  "applications/gs2020/results/mixed_logit/v21_corrected/fit_primary_assembled.rds")
if (file.exists(fit_path)) {
  fit <- readRDS(fit_path)
  basis <- tq1_rebuild_inference_basis(fit)
  check("portable GS networks reproduce saved predictions",
        max(basis$prediction_max_abs_deviation) < basis$prediction_tolerance)
  check("GS basis dimensions reproduce the dated tangent construction",
        identical(basis$basis_dimension, c(5L, 5L, 9L, 5L, 5L)) &&
          identical(basis$tangent_dimension,
                    c(181L, 181L, 301L, 181L, 181L)))
}

cat(passed, "tests passed\n")
