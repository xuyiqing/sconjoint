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
  plugin_estimate = c(h = 0.1, h2 = 0.15, hr = 0.2),
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
tr_margin_fail <- scmix_delta_transform(
  fake, type = "directional_heterogeneity",
  primitives = c("h", "h2", "hr"), total_margin = 1)
tr_summary <- tq1_transform_summary(
  tr_margin_fail, "heterogeneity", "synthetic")
check("heterogeneity total margin gates only the reported share",
      all(tr_summary$quantity_gate_pass[tr_summary$component != "share_Z"]) &&
        !tr_summary$quantity_gate_pass[tr_summary$component == "share_Z"])
plug_h <- tq1_plugin_transform(fake, "directional_heterogeneity",
                               c("h", "h2", "hr"))
check("plug-in heterogeneity transform uses plug-in primitives",
      abs(plug_h[["H_Z"]] - 0.14) < 1e-14 &&
        abs(plug_h[["H_T"]] - 0.34) < 1e-14 &&
        abs(plug_h[["share_Z"]] - 0.14 / 0.34) < 1e-14)

single <- structure(list(
  estimate = c(x = 0.25), plugin_estimate = c(x = 0.20),
  one_step_adjustment = c(x = 0.05), diagnostic_se = c(x = 0.10),
  diagnostic_covariance = matrix(0.01, 1L, 1L,
                                 dimnames = list("x", "x")),
  se = c(x = NA_real_), ci_lower = c(x = NA_real_),
  ci_upper = c(x = NA_real_), status = "conditional_unverified",
  target_inference_available = c(x = FALSE), reason = "synthetic",
  fold_details = rep(list(list(
    riesz_equation_relative_residual = 0.02,
    ridge_relative_sensitivity = 0.03,
    information_structural_min = 0.04, selected_nu = 0)), 2L)),
  class = c("scmix_dml", "list"))
single_summary <- tq1_family_summary(single, "single")
check("one-target family summary preserves fold by target shape",
      nrow(single_summary) == 1L && single_summary$target == "x" &&
        single_summary$fold1_riesz == 0.02 &&
        single_summary$fold2_ridge == 0.03)

ratio <- structure(list(plugin_estimate = c(num = 0.12, den = 0.30)),
                   class = c("scmix_dml", "list"))
check("plug-in subgroup transform is a ratio of plug-in primitives",
      abs(tq1_plugin_transform(ratio, "subgroup_ratio",
                               c("num", "den")) - 0.4) < 1e-14)

bound <- tq1_bind_rows(list(data.frame(a = 1), data.frame(b = 2)))
check("row binder retains the union of diagnostic columns",
      identical(names(bound), c("a", "b")) && nrow(bound) == 2L)

c_raw <- c(low = -1 / 7, high = 1 / 7)
c_norm <- tq1_normalize_contrast(c_raw)
check("contrast-ray normalization preserves orientation and scale",
      max(abs(c_norm$analysis)) == 1 &&
        identical(base::sign(c_norm$analysis), base::sign(c_raw)) &&
        abs(c_norm$raw_scale - 1 / 7) < 1e-15)

c_ray <- c(penalty = -1 / 7, benefit = 1 / 7, other = 0)
c_ray_norm <- tq1_normalize_contrast(c_ray)
raw_target <- scmix_inference_target(
  type = "sign", contrast = c_ray, variance_floor = 1e-12)
normalized_target <- scmix_inference_target(
  type = "sign", contrast = c_ray_norm$analysis, variance_floor = 1e-12)
raw_eval <- do.call(raw_target, args)
normalized_eval <- do.call(normalized_target, args)
check("sign target and derivatives are invariant along a contrast ray",
      max(abs(raw_eval$value - normalized_eval$value)) < 1e-13 &&
        max(abs(raw_eval$d_mu - normalized_eval$d_mu)) < 1e-13 &&
        max(abs(raw_eval$d_Sigma - normalized_eval$d_Sigma)) < 1e-12)

H_analysis <- c(H_Z = 0.4, H_R = 0.6, H_T = 1, share_Z = 0.4)
H_raw <- H_analysis
H_raw[c("H_Z", "H_R", "H_T")] <-
  H_raw[c("H_Z", "H_R", "H_T")] * c_ray_norm$raw_scale^2
check("variance rescaling leaves the heterogeneity share unchanged",
      H_raw[["share_Z"]] == H_analysis[["share_Z"]] &&
        abs(H_raw[["H_T"]] / c_ray_norm$raw_scale^2 - 1) < 1e-15)

scale_fixture <- data.frame(
  component = c("H_Z", "H_R", "H_T", "share_Z"),
  plugin = c(0.4, 0.6, 1, 0.4),
  one_step = c(0.3, 0.7, 1, 0.3),
  diagnostic_se = c(0.04, 0.05, 0.06, 0.07))
scale_multiplier <- ifelse(scale_fixture$component == "share_Z", 1,
                           c_ray_norm$raw_scale^2)
scaled_fixture <- tq1_rescale_summary_rows(scale_fixture, scale_multiplier)
check("directional rescaling returns dimensional estimates and SEs to raw units",
      abs(scaled_fixture$plugin[scale_fixture$component == "H_T"] -
            c_ray_norm$raw_scale^2) < 1e-15 &&
        abs(scaled_fixture$diagnostic_se[
          scale_fixture$component == "H_T"] -
            0.06 * c_ray_norm$raw_scale^2) < 1e-15 &&
        scaled_fixture$plugin[scale_fixture$component == "share_Z"] == 0.4)
space_ok <- function(v) all(v[c("H_Z", "H_R", "H_T")] >= 0) &&
  v[["H_T"]] > 0 && v[["share_Z"]] >= 0 && v[["share_Z"]] <= 1
raw_values <- stats::setNames(scaled_fixture$one_step,
                              scaled_fixture$component)
analysis_values <- stats::setNames(scale_fixture$one_step,
                                   scale_fixture$component)
check("positive ray rescaling preserves heterogeneity parameter-space flags",
      identical(space_ok(raw_values), space_ok(analysis_values)))

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
