# Decisions requiring author review

The following judgments are fixed in this draft but should be affirmatively
accepted or revised under a new protocol ID before implementation.

1. **Outcome-informed status.** The protocol is prospective only for the fresh
   rerun. Earlier Ballard--Rosa results informed its construction.
2. **Response-free moderator repair.** The primary source hash is retained,
   but the runner will reconstruct `work_vs_luck`, treat raw codes 8 and 9 as
   missing, and record a new derived-input hash. The restricted model uses
   `raw == 1` versus `raw in {2,3}`.
3. **Restricted mean.** The five moderators are party identification, income,
   inequality aversion, work-versus-luck beliefs, and perceived economic harm
   from taxes. Each may affect all seven coefficients, producing 35 slopes.
   The restriction is a sensitivity and can never become primary based on
   this rerun.
4. **Tuning budget.** The flexible grid has ten candidates, five outer folds,
   two inner folds, three starts, and 6,000 Adam epochs before L-BFGS. The
   q=0 and q=2 runs repeat the same flexible grid.
5. **Target scope.** The registry retains party-specific policy probabilities,
   twelve progressivity-direction shares, twelve design-anchored
   compensating-benefit shares, progressivity and coordinate heterogeneity,
   and one bottom-versus-top MRS. Coordinate heterogeneity and the
   log-midpoint direction are appendix targets, but still have prespecified
   families rather than being selected after results.
6. **Compensating benefits.** The benefit amounts 2, 2, and 4 come from the
   revenue-score differences in the three frozen policy schedules. These
   targets were not in the earlier corrected application and are explicitly
   outcome-informed additions fixed before the fresh rerun.
7. **Multiplicity families.** Progressivity heterogeneity is separated from
   coordinate-level heterogeneity. This protects a substantively central
   family from an unrelated weak coordinate but cannot be changed after the
   rerun.
8. **AME benchmark.** The common design contrast is 15 percent versus 5
   percent in each randomized bracket. Revenue receives no design AMCE because
   it is derived from the randomized rates. The absolute materiality tolerance
   is 0.03 on the probability scale.
9. **Assessment tolerances.** Sequence noninferiority is 0.08 nats per complete
   eight-task sequence. Calibration gaps use 0.05. Bounded target sensitivity
   uses 0.03, except missingness uses 0.02. Unbounded targets use one-half of
   the primary regular standard error.
10. **Structural sensitivities.** The battery includes t5, both orientations
    of a standardized skewed chi-square distribution, a fixed symmetric
    bimodal mixture, party covariance scales, lagged response dependence,
    task order, position interactions, knowledge-specific logit scale,
    missingness, and survey weighting. This is complete but computationally
    expensive.
11. **Formal release scope.** Only the flexible q=1 primary specification is
    eligible for formal family release. Other fits are named sensitivities.
12. **Identification documentation.** The fielding data do not establish the
    protocol probabilities or prove that implausible plans were never
    rejected. Without the original assignment instrument or generator, a
    clean theorem-backed identification claim remains unavailable.
