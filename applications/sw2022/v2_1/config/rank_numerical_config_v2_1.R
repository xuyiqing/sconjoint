## Saha--Weeks v2.1 rank sensitivity and numerical-integration diagnostics.
##
## This is a downstream, outcome-informed diagnostic specification. It does
## not alter the v2.1 reported-primary rule, select a rank, or authorize formal
## inference. Every positive-rank node ladder reruns the same frozen
## within-rank learner-selection procedure and optimization controls.

sw_v21_rank_application_root <- getOption(
  "sconjoint.sw_application_root", NULL)
if (is.null(sw_v21_rank_application_root)) {
  candidate <- file.path(normalizePath(".", mustWork = TRUE),
                         "applications", "sw2022")
  if (!dir.exists(candidate)) {
    stop("Set option 'sconjoint.sw_application_root' before sourcing the v2.1 rank config.")
  }
  sw_v21_rank_application_root <- candidate
}
sw_v21_rank_application_root <- normalizePath(
  sw_v21_rank_application_root, mustWork = TRUE)

sw_v21_rank_config <- list(
  version = "sw2022-paperps-2026-08-24-v2.1-rank-numerical-v1",
  parent_version = "sw2022-paperps-2026-08-24-v2.1-postpilot-final",
  outcome_blind = FALSE,
  formal_inference_available = FALSE,
  rank_selected = FALSE,
  primary_rank_changed = FALSE,
  output_root = file.path(
    sw_v21_rank_application_root, "results",
    "mixed_logit_v2_1_rank_numerical"),
  authorization_file = file.path(
    sw_v21_rank_application_root, "results",
    "mixed_logit_v2_1_rank_numerical",
    "RANK_NUMERICAL_AUTHORIZATION.rds"),
  ranks = c(0L, 1L, 2L),
  base_nodes = c(`0` = 31L, `1` = 31L, `2` = 15L),
  refinement_nodes = list(
    `1` = c(15L, 31L, 45L),
    `2` = c(9L, 15L, 21L)),
  rotation_angles = c(0, pi / 8, pi / 4, 3 * pi / 8),
  panels = list(
    selected_procedure = list(
      grid = "complete frozen ten-candidate v2.1 grid",
      always_run = TRUE),
    exact_constant = list(
      grid = "frozen exact-constant candidate only",
      run_if_reported_primary_fallback = TRUE)),
  tolerances = list(
    mean_sequence_log_score = 0.002,
    respondent_score_l2 = 0.005,
    qoi_max_absolute = 0.005,
    rotation_mean_sequence_log_score = 0.002,
    rotation_respondent_score_l2 = 0.005,
    rotation_covariance_max_absolute_error = 1e-10),
  interpretation = paste(
    "q=0,1,2 are fixed-rank sensitivity specifications evaluated on the",
    "exact same held-out respondent folds. Rank is never selected. For q=1",
    "and q=2, every node resolution reruns the complete frozen within-rank",
    "learner-selection and optimization procedure; q=2 additionally checks",
    "finite-product-GH sensitivity to orthogonal loading rotations. If the",
    "v2.1 guardrail selected the exact-constant fallback, the full selected-",
    "procedure panel remains diagnostic and a separate exact-constant panel",
    "is also run. These outcome-informed diagnostics provide no formal",
    "inference, even when every empirical stability gate passes."
  )
)
