## Saha--Weeks version 2.1 post-pilot final-analysis specification.
##
## This specification was frozen after the outcome-informed v2 pilot failed
## its exact selected-minus-constant held-out score gate by 6.63e-06. It does
## not retroactively change that pilot. The -0.001 guardrail below is a
## descriptive materiality/noninferiority rule, not a formal statistical test.

sw_v21_application_root <- getOption("sconjoint.sw_application_root", NULL)
if (is.null(sw_v21_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "sw2022")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.sw_application_root' before sourcing v2.1 config.")
  }
  sw_v21_application_root <- candidate
}
sw_v21_application_root <- normalizePath(sw_v21_application_root,
                                           mustWork = TRUE)

.sw_v21_final_grid <- function(nodes) {
  out <- list(list(
    name = "constant", mean_family = "constant", hidden = integer(),
    weight_decay = 0, integration = "gh", n_nodes = as.integer(nodes)))
  for (family in c("linear", "relu4", "relu8")) {
    for (wd in c(1e-3, 1e-2, 1e-1)) {
      is_linear <- identical(family, "linear")
      out[[length(out) + 1L]] <- list(
        name = paste0(family, "_wd", format(wd, scientific = TRUE)),
        mean_family = if (is_linear) "linear" else "relu",
        hidden = if (is_linear) integer() else
          if (identical(family, "relu4")) 4L else 8L,
        weight_decay = wd, integration = "gh",
        n_nodes = as.integer(nodes))
    }
  }
  out
}

sw_v21_config <- list(
  version = "sw2022-paperps-2026-08-24-v2.1-postpilot-final",
  predecessor = "sw2022-paperps-2026-08-24-v2-penalty-diagnostic",
  specification_frozen_after_failed_pilot = TRUE,
  outcome_blind = FALSE,
  formal_inference_available = FALSE,
  production_result = FALSE,
  primary_artifacts_overwritten = FALSE,
  provenance = paste(
    "Post-pilot version created after the outcome-informed v2 pilot failed",
    "the exact selected-minus-constant held-out gate by 6.63e-06. It retains",
    "the already-declared broader grid and computation schedule unchanged."
  ),
  input = list(
    prepared = file.path(sw_v21_application_root, "results",
                         "prep_analysis_data.rds"),
    primary_Z = "Z_primary",
    v1_nested = file.path(sw_v21_application_root, "results", "mixed_logit",
                          "production", "fit_primary_nested.rds"),
    v1_party_diagnostic = file.path(
      sw_v21_application_root, "results", "party_gender_mean_sensitivity",
      "production", "party_gender_mean_sensitivity.rds"),
    failed_v2_pilot_manifest = file.path(
      sw_v21_application_root, "results", "mixed_logit_v2", "pilot",
      "manifest.rds")
  ),
  output_root = file.path(sw_v21_application_root, "results",
                          "mixed_logit_v2_1_postpilot_final"),
  authorization_file = file.path(
    sw_v21_application_root, "results", "mixed_logit_v2_1_postpilot_final",
    "FINAL_ANALYSIS_AUTHORIZATION.rds"),
  model = list(
    q = 1L, integration = "gauss-hermite", n_nodes = 31L,
    mean = paste(
      "mu(z)=clip{alpha+g_omega(z_dagger)-g_omega(0)}; alpha is compact",
      "and unpenalized, and only moderator-deviation parameters are penalized."
    ),
    exact_constant_nested = TRUE
  ),
  bounds = list(mu = 10, alpha = 5, kappa = 10, loading = 10,
                deviation_parameter = 20),
  optimizer = list(
    device = "cpu", seed = 20260824L + 910000L,
    early_stop = FALSE, n_epochs = 1400L, n_starts = 3L,
    learning_rate = 0.005, opt_tol = 1e-4, grad_tol = 1e-2,
    nested_objective_tol = 1e-6, selection_tie_tol = 1e-8),
  folds = list(outer_K = 5L, inner_K = 3L,
               reuse_exact_v1_outer_folds = TRUE),
  grid = .sw_v21_final_grid(31L),
  postpilot_guardrail = list(
    estimand = paste(
      "mean respondent-level held-out complete-sequence log-score difference",
      "for selected procedure minus independently refitted exact constant"
    ),
    noninferiority_margin = -0.001,
    introduced_after_failed_pilot = TRUE,
    descriptive_only = TRUE,
    formal_test = FALSE,
    paired_respondent_standard_error_reported_separately = TRUE,
    fallback_if_missed = "exact constant q=1 mixed logit"
  )
)

rm(.sw_v21_final_grid)
