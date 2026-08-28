## Shared diagnostic-inference driver for gs2020 and br2017 (step 04).
##
## The caller (a thin per-app wrapper) must define, before sourcing this file:
##   app_name, cfg, app_root, root, cli (profile=, force=)
## Mirrors the sw2022 04_inference.R: scmix_dml() in DIAGNOSTIC mode (the
## configs set enable_conditional_formal_inference = FALSE), deciding whether
## the Riesz-equation and ridge-sensitivity gates pass their prespecified
## tolerances. Reads the fitted-sieve basis extracted by step 03.

fit_dir <- file.path(cfg$output_root, cli$profile)
fit_path <- file.path(fit_dir, "fit_primary_assembled.rds")
if (!file.exists(fit_path)) {
  stop("Assembled primary fit not found: ", fit_path,
       ". Run 03_fit_models.R --stage=primary first.")
}
out_rds <- file.path(fit_dir, "inference_diagnostic.rds")
out_csv <- file.path(fit_dir, "inference_summary.csv")
if (file.exists(out_rds) && !cli$force) {
  stop("Output exists (use --force=true to overwrite): ", out_rds)
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The local analysis library must include pkgload.")
}
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))

fit <- readRDS(fit_path)
prepared <- readRDS(cfg$input$prepared)

basis_path <- file.path(fit_dir, "inference_basis.rds")
if (!file.exists(basis_path)) {
  stop("inference_basis.rds not found in ", fit_dir,
       ". Rerun 03_fit_models.R --stage=primary in a fresh session.")
}
basis_obj <- readRDS(basis_path)
if (!identical(basis_obj$analysis_signature, fit$analysis_signature)) {
  stop("inference_basis.rds does not match the assembled fit; rerun 03 with ",
       "--force=true.")
}
mu_basis <- basis_obj$mu_basis
respondent_id <- as.character(fit$respondent_id)
resp <- unique(respondent_id)
if (!identical(basis_obj$respondents, resp)) {
  stop("inference_basis.rds respondent order does not match the fit.")
}

inf_cfg <- cfg$inference
attr_names <- colnames(as.matrix(fit$deltaX))
headline_contrast <- as.numeric(attr_names == cfg$qoi$headline_coordinate)
names(headline_contrast) <- attr_names
if (sum(headline_contrast) != 1) {
  stop("Headline coordinate not found in the likelihood basis.")
}

targets <- list()
if (length(cfg$qoi$subgroups)) {
  meta <- prepared$respondent_meta
  meta_ids <- as.character(meta$respondent_id)
  sub_contrast <- as.numeric(attr_names == cfg$qoi$subgroup_contrast)
  names(sub_contrast) <- attr_names
  for (nm in names(cfg$qoi$subgroups)) {
    spec <- cfg$qoi$subgroups[[nm]]
    vals <- meta[[spec$meta_column]][match(resp, meta_ids)]
    if (anyNA(vals)) stop("Missing ", spec$meta_column, " for some respondents.")
    g <- switch(spec$op,
                "<=" = as.numeric(vals <= spec$value),
                ">=" = as.numeric(vals >= spec$value),
                "==" = as.numeric(vals == spec$value),
                stop("Unsupported subgroup operator: ", spec$op))
    names(g) <- resp
    targets[[paste0("subgroup_", nm)]] <- scmix_inference_target(
      type = "subgroup_tau_primitives",
      contrast = sub_contrast, subgroup = g,
      label = paste0("subgroup_", nm))
  }
}

dml_args <- list(
  fit = fit,
  targets = "theta",
  plugin_targets = if (length(targets)) targets else NULL,
  mu_basis = mu_basis,
  riesz_validation_fraction = inf_cfg$riesz_validation_fraction,
  riesz_equation_tolerance = inf_cfg$riesz_equation_tolerance,
  ridge_sensitivity_tolerance = inf_cfg$ridge_sensitivity_tolerance,
  active_eigenvalue_min = inf_cfg$active_eigenvalue_min,
  information_eigenvalue_min = inf_cfg$information_eigenvalue_min,
  rank_tolerance = inf_cfg$rank_tolerance,
  multiplier_draws = inf_cfg$multiplier_draws,
  multiplier = inf_cfg$multiplier,
  level = inf_cfg$level,
  seed = cfg$optimizer$seed
)
main <- do.call(scmix_dml, dml_args)

sign_out <- tryCatch({
  sign_args <- dml_args
  sign_args$targets <- character()
  sign_args$plugin_targets <- list(
    headline_sign = scmix_inference_target(
      type = "sign", contrast = headline_contrast,
      variance_floor = inf_cfg$variance_floor,
      label = "headline_sign"))
  do.call(scmix_dml, sign_args)
}, error = function(e) list(status = "construction_failed",
                            reason = conditionMessage(e)))

riesz_max <- main$riesz_equation_max_relative_residual
ridge_max <- max(unlist(lapply(main$fold_details,
                               `[[`, "ridge_relative_sensitivity")))

result <- list(
  application = app_name,
  profile = cli$profile,
  analysis_signature = fit$analysis_signature,
  main = main,
  sign = sign_out,
  riesz_equation_max_relative_residual = riesz_max,
  ridge_relative_sensitivity_max = ridge_max,
  riesz_gate_pass = is.finite(riesz_max) &&
    riesz_max <= inf_cfg$riesz_equation_tolerance,
  ridge_gate_pass = is.finite(ridge_max) &&
    ridge_max <= inf_cfg$ridge_sensitivity_tolerance,
  config_version = cfg$version,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
saveRDS(result, out_rds, version = 3)

labels <- names(main$estimate)
summary_df <- data.frame(
  label = labels,
  estimate = as.numeric(main$estimate[labels]),
  plugin = as.numeric(main$plugin_estimate[labels]),
  se = as.numeric(main$se[labels]),
  diagnostic_se = as.numeric(main$diagnostic_se[labels]),
  ci_lower = as.numeric(main$ci_lower[labels]),
  ci_upper = as.numeric(main$ci_upper[labels])
)
summary_df$status <- main$status
summary_df$riesz_max <- riesz_max
summary_df$ridge_max <- ridge_max
utils::write.csv(summary_df, out_csv, row.names = FALSE)

message(sprintf(
  "04 inference [%s %s]: status=%s riesz_max=%.4g (tol %.3g, pass=%s) ridge_max=%.4g (tol %.3g, pass=%s) sign=%s",
  app_name, cli$profile, main$status,
  riesz_max, inf_cfg$riesz_equation_tolerance, result$riesz_gate_pass,
  ridge_max, inf_cfg$ridge_sensitivity_tolerance, result$ridge_gate_pass,
  if (is.list(sign_out) && !is.null(sign_out$status)) sign_out$status else class(sign_out)[1]))
