## Additional, application-specific controls for honest held-out rank
## assessment.  This file is deliberately separate from analysis_config.R so
## the primary estimator's frozen configuration is not changed while its
## production checkpoints are being computed.

sw_rank_assessment_config <- list(
  version = "sw2022-common-outer-rank-assessment-2026-08-24-v1",
  ranks = c(0L, 1L, 2L),
  q2_refinement_nodes = list(
    smoke = c(3L, 5L, 7L),
    pilot = c(5L, 9L, 13L),
    production = c(9L, 15L, 21L)
  ),
  q2_rotation_angles = c(0, pi / 8, pi / 4, 3 * pi / 8),
  tolerances = list(
    mean_sequence_log_score = 0.002,
    respondent_score_l2 = 0.005,
    qoi_max_absolute = 0.005,
    rotation_mean_sequence_log_score = 0.002,
    rotation_respondent_score_l2 = 0.005,
    rotation_covariance_max_absolute_error = 1e-10
  ),
  interpretation = paste(
    "Ranks are never selected by this runner. q=0,1,2 use the same outer",
    "respondent folds, and the q=2 node ladder holds the base-selected learner",
    "specification fixed within each full or outer training sample. The final",
    "candidate grid was adapted after a same-sample pilot, so these are",
    "cross-fitted diagnostics rather than a clean outcome-blind evaluation."
  )
)
