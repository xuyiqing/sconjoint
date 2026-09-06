\name{scmix_v22_computation_contract}
\alias{scmix_v22_multistart_stability}
\alias{scmix_v22_integration_compare}
\alias{scmix_v22_integration_refinement}
\title{Protocol-v2.2 Multistart and Integration Evidence}
\usage{
scmix_v22_multistart_stability(
  evaluations,
  registered_targets,
  registered_starts,
  near_optimum_objective_cutoff = NULL,
  multistart_target_se_cutoff = NULL,
  protocol_mode = c("calibration", "production"),
  protocol_id = "paperps-inference-2026-09-05-v2.2"
)

scmix_v22_integration_compare(
  level_a,
  fixed_b,
  repolished_b,
  target_primitives,
  calibration = list(),
  protocol_mode = c("calibration", "production"),
  protocol_id = "paperps-inference-2026-09-05-v2.2"
)

scmix_v22_integration_refinement(
  level_a,
  level_b_rule,
  fixed_parameter_evaluator,
  repolisher,
  repolished_evaluator,
  ...
)
}
\arguments{
  \item{evaluations}{A signed long-form table containing one row for every
  fit-scope, polished start, and registered target.}
  \item{registered_targets}{The frozen target IDs.}
  \item{registered_starts}{A start-ID vector, or a named list by fit scope.}
  \item{near_optimum_objective_cutoff}{The Step-3 relative-objective cutoff,
  or \code{NULL} while calibration is pending.}
  \item{multistart_target_se_cutoff}{The Step-3 target-stability cutoff, or
  \code{NULL} while calibration is pending.}
  \item{protocol_mode}{Whether the artifact is for calibration or production.}
  \item{protocol_id}{The frozen protocol identifier.}
  \item{level_a}{A signed production-rule evaluation artifact.}
  \item{fixed_b}{A signed higher-resolution evaluation at the exact level-A
  parameter states.}
  \item{repolished_b}{A signed higher-resolution evaluation after polishing
  from the exact level-A states.}
  \item{target_primitives}{A named list mapping targets to influence
  primitives.}
  \item{calibration}{Named Step-3 integration cutoffs.}
  \item{level_b_rule}{The signed higher-resolution integration rule.}
  \item{fixed_parameter_evaluator}{Callback that evaluates level B at fixed
  level-A parameters.}
  \item{repolisher}{Callback that polishes under level B from level A.}
  \item{repolished_evaluator}{Callback that recomputes the required objects
  at the repolished level-B state.}
  \item{...}{Arguments passed to \code{scmix_v22_integration_compare}.}
}
\value{
For multistart stability, a structured F008 artifact with objective gaps,
target changes, and target-scoped gates. For integration refinement, a
structured two-branch artifact with fixed-parameter, repolished, and worse-of
metrics plus I001--I005 gates.
}
\description{
These functions implement the pure validation and aggregation layer of the
frozen protocol-v2.2 multistart and numerical-integration contracts. They do
not refit application models or choose calibration thresholds.
}
