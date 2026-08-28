## Version-2 Saha--Weeks diagnostic specification.
##
## This configuration was created after the version-1 party-by-candidate-
## gender assessment exposed shrinkage of the common mean and the absence of
## an exact pooled candidate. It is outcome-informed and therefore supports
## descriptive model development only. It never overwrites version-1 output.

sw_v2_application_root <- getOption("sconjoint.sw_application_root", NULL)
if (is.null(sw_v2_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "sw2022")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.sw_application_root' before sourcing v2 config.")
  }
  sw_v2_application_root <- candidate
}
sw_v2_application_root <- normalizePath(sw_v2_application_root,
                                         mustWork = TRUE)

.sw_v2_grid <- function(nodes, penalties, include_legacy = FALSE) {
  out <- list(
    list(name = "constant", mean_family = "constant",
         hidden = integer(), weight_decay = 0,
         integration = "gh", n_nodes = as.integer(nodes)),
    list(name = paste0("linear_wd", gsub("\\.", "p", penalties$linear)),
         mean_family = "linear", hidden = integer(),
         weight_decay = penalties$linear,
         integration = "gh", n_nodes = as.integer(nodes)),
    list(name = paste0("h4_wd", gsub("\\.", "p", penalties$relu)),
         mean_family = "relu", hidden = 4L,
         weight_decay = penalties$relu,
         integration = "gh", n_nodes = as.integer(nodes)),
    list(name = paste0("h8_wd", gsub("\\.", "p", penalties$relu)),
         mean_family = "relu", hidden = 8L,
         weight_decay = penalties$relu,
         integration = "gh", n_nodes = as.integer(nodes))
  )
  if (isTRUE(include_legacy)) {
    out[[length(out) + 1L]] <- list(
      name = "legacy_h8_wd0p1", mean_family = "legacy", hidden = 8L,
      weight_decay = 0.1, integration = "gh", n_nodes = as.integer(nodes))
  }
  out
}

.sw_v2_production_grid <- function(nodes) {
  out <- list(list(
    name = "constant", mean_family = "constant", hidden = integer(),
    weight_decay = 0, integration = "gh", n_nodes = as.integer(nodes)))
  for (family in c("linear", "relu4", "relu8")) {
    for (wd in c(1e-3, 1e-2, 1e-1)) {
      is_linear <- identical(family, "linear")
      hidden <- if (is_linear) integer() else
        if (identical(family, "relu4")) 4L else 8L
      out[[length(out) + 1L]] <- list(
        name = paste0(family, "_wd", format(wd, scientific = TRUE)),
        mean_family = if (is_linear) "linear" else "relu",
        hidden = hidden, weight_decay = wd,
        integration = "gh", n_nodes = as.integer(nodes))
    }
  }
  out
}

sw_v2_config <- list(
  version = "sw2022-paperps-2026-08-24-v2-penalty-diagnostic",
  supersedes_for_development = "sw2022-paperps-2026-08-24-v1",
  primary_artifacts_overwritten = FALSE,
  outcome_blind = FALSE,
  formal_inference_available = FALSE,
  provenance = paste(
    "Created after the version-1 party-gender diagnostic showed that the",
    "selected all-parameter penalty shrank the common mean and that an exact",
    "unpenalized pooled q=1 benchmark was absent from the eligible grid."
  ),
  input = list(
    prepared = file.path(sw_v2_application_root, "results",
                         "prep_analysis_data.rds"),
    primary_Z = "Z_primary",
    v1_nested = file.path(sw_v2_application_root, "results", "mixed_logit",
                          "production", "fit_primary_nested.rds"),
    v1_party_diagnostic = file.path(
      sw_v2_application_root, "results", "party_gender_mean_sensitivity",
      "production", "party_gender_mean_sensitivity.rds")
  ),
  output_root = file.path(sw_v2_application_root, "results",
                          "mixed_logit_v2"),
  model = list(
    q = 1L, integration = "gauss-hermite",
    mean = paste(
      "mu(z)=clip{alpha+g_omega(z_dagger)-g_omega(0)}; alpha is compact",
      "and unpenalized, and only moderator-deviation parameters are penalized."
    ),
    alpha_reference = paste(
      "The origin of the training-fold centered/scaled moderator coding;",
      "alpha is a computational reference level, not a separate estimand."
    ),
    candidate_families = c("constant", "linear", "relu width 4",
                           "relu width 8"),
    exact_pooled_nesting_required = TRUE
  ),
  bounds = list(mu = 10, alpha = 5, kappa = 10, loading = 10,
                deviation_parameter = 20),
  optimizer = list(device = "cpu", seed = 20260824L + 910000L,
                   early_stop = FALSE),
  profiles = list(
    smoke = list(
      label = "v2 interface and nesting smoke test; never substantive",
      outer_K = 2L, inner_K = 2L, n_epochs = 20L, n_starts = 2L,
      learning_rate = 0.01, n_nodes = 5L,
      grid = .sw_v2_grid(5L, list(linear = 0.1, relu = 0.1)),
      opt_tol = 1e6, grad_tol = 1e6, nested_objective_tol = 1e-5,
      diagnostic_only = TRUE
    ),
    pilot = list(
      label = paste(
        "frozen-v1-outer-fold v2 diagnostic; outcome-informed and not the",
        "paper production result"
      ),
      outer_K = 5L, inner_K = 2L, n_epochs = 400L, n_starts = 2L,
      learning_rate = 0.005, n_nodes = 31L,
      grid = .sw_v2_grid(31L, list(linear = 0.01, relu = 0.1)),
      opt_tol = 5e-4, grad_tol = 2e-2, nested_objective_tol = 1e-6,
      diagnostic_only = TRUE
    ),
    production = list(
      label = paste(
        "declared future v2 production specification; do not run until the",
        "smoke/pilot audit is approved"
      ),
      outer_K = 5L, inner_K = 3L, n_epochs = 1400L, n_starts = 3L,
      learning_rate = 0.005, n_nodes = 31L,
      grid = .sw_v2_production_grid(31L),
      opt_tol = 1e-4, grad_tol = 1e-2, nested_objective_tol = 1e-6,
      diagnostic_only = TRUE,
      execution_authorization_file = file.path(
        sw_v2_application_root, "results", "mixed_logit_v2",
        "PRODUCTION_AUTHORIZATION.rds")
    )
  ),
  gates = list(
    all_inner_optimization = TRUE,
    exact_pooled_nested_objective = TRUE,
    selected_refit_optimization = TRUE,
    alpha_bound_inactive = TRUE,
    other_compact_bounds_inactive = TRUE,
    heldout_score_at_least_pooled = TRUE,
    formal_inference = FALSE
  )
)

rm(.sw_v2_grid, .sw_v2_production_grid)
