## Application-only configuration for the Saha--Weeks sensitivity runner.
##
## This file was created before the production sensitivity results were run.
## The original analysis did not preregister quantitative materiality margins
## for these newly added checks.  NA therefore means "no approved threshold";
## the runner must not turn a finite difference into a pass/fail conclusion.

sw_sensitivity_config <- list(
  schema_version = "sw2022-paperps-sensitivity-v1",
  created_before_production_run = TRUE,
  primary_model_unchanged = TRUE,
  primary_rank = 1L,
  architecture_policy = paste(
    "Reuse the architecture selected within each primary outer-training",
    "sample. This keeps held-out respondents out of outcome-dependent tuning;",
    "the alternative moderator/feature set is a sensitivity, not a retuned primary model."
  ),
  postconjoint = list(
    variables = c("ideo_conservative", "vote_trump", "vote_clinton",
                  "gender_att"),
    imputation = "training-respondent median, frozen before held-out evaluation",
    interpretation = paste(
      "Post-conjoint variables are excluded from the primary specification",
      "and enter only this timing-sensitive perturbation."
    )
  ),
  male_run = list(
    feature = "I(candidate is Male) * I(candidate previously ran)",
    contrast_orientation = "feature(candidate A) - feature(candidate B)"
  ),
  completion = list(
    comparison = paste(
      "Primary 1,191 complete-case respondents versus every respondent with",
      "two or three valid observed tasks, using an intercept-only moderator",
      "specification in both samples."
    )
  ),
  materiality_tolerances = c(
    z19_choice_probability = NA_real_,
    male_run_choice_probability = NA_real_,
    completion_choice_probability = NA_real_,
    position_swap = NA_real_
  ),
  fail_closed_note = paste(
    "No quantitative materiality margin was preregistered for the rebuilt",
    "application. Results are descriptive sensitivities; missing alternative",
    "models remain not_run and no maintained assumption is marked verified."
  )
)
