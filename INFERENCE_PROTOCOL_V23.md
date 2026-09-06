# Frozen inference protocol v2.3

**Protocol ID:** `paperps-inference-2026-09-06-v2.3`

**Frozen on:** 2026-09-06

**Status:** protocol structure frozen; simulation-calibrated constants pending

**Machine-readable companion:** `inst/protocol/inference-v2.3.yaml`

## 1. Scope of this revision

Protocol v2.3 is a narrow prospective correction to v2.2. The immutable v2.2
documents remain at `INFERENCE_PROTOCOL_V22.md` and
`inst/protocol/inference-v2.2.yaml`. Their checksum remains historical audit
evidence. Except for the changes stated here, v2.3 incorporates all v2.2
definitions, ordering rules, estimators, evidence requirements, failure
semantics, and reporting statuses.

The correction is made before any Step 3 simulation result or application
rerun is used. It resolves an ambiguity in G006 without constraining separately
calibrated constants to be equal. It also limits production calibration to the
paper's primary rank-one model. No production calibration values or approval
manifest exist at the time this protocol is frozen.

## 2. Rank scope

Production calibration has `Rank-Scope: 1`. A calibration artifact authorizes
ordinary v2.3 inference only when the signed analysis contract fixes (q=1).
Rank-zero and rank-two fits remain diagnostic sensitivity analyses. Their
predictive and specification-assessment results may be reported with that
label, but the rank-one calibration does not authorize their inferential
release. Rank-two integration sentinels are numerical stress checks and do not
authorize rank-two inference.

## 3. Revised G006 compound statistic

G006 remains a hard target-level regularity gate. Its v2.3 statistic is a
normalized compound diagnostic. For each applicable component (r), let
(D_r) be its frozen empirical diagnostic and let (c_r>0) be its own Step 3
calibrated cutoff. Define

\[
 G_{006}=\min_{r\in\mathcal R(\theta)}\frac{D_r}{c_r}.
\]

The gate passes if and only if (G_{006}\geq 1). Its comparator is
`greater_than_or_equal` and its units are `dimensionless`. A missing,
nonfinite, zero, or negative applicable cutoff leaves the gate `not_run` and
fails closed. Component cutoffs are independent. None is an alias for another.

The applicable components are as follows.

1. A bounded or positive scalar uses its standard-error distance to the chart
   boundary. Its cutoff is `interior_distance_cutoff`.
2. A subgroup share with primitive coordinates (0<a<b<1) uses
   \(\min\{a,1-b,b-a\}\). Its cutoff is `subgroup_interior_margin`.
3. Directional heterogeneity with primitives \((H_Z,H_R)\) uses three
   components. The primitive-interior diagnostic is
   \(\min(H_Z,H_R)\), with cutoff
   `directional_heterogeneity_regularity_margin`. The standardized residual
   variance diagnostic is (H_R/\widehat{\mathrm{se}}(H_R)), with cutoff
   `directional_variance_standardized_cutoff`. The standardized total is
   \((H_Z+H_R)/\widehat{\mathrm{se}}(H_Z+H_R)\), with cutoff
   `total_heterogeneity_standardized_cutoff`. The standard error of the total
   is computed from the respondent-level joint influence contributions.
4. A mean-tradeoff or MRS ratio uses two denominator components. The raw
   scientific denominator diagnostic is \(|b|\), with cutoff
   `mean_tradeoff_denominator_margin`. The standardized diagnostic is
   \(|b|/\widehat{\mathrm{se}}(b)\), with cutoff
   `denominator_standardized_cutoff`.
5. G006 is not applicable to an unrestricted scalar without a boundary or
   denominator condition.

These component definitions replace the v2.2 G006 row and related statements
only. Known-support intersection, primitive-set mapping, and confidence-set
boundary labels remain unchanged.

## 4. Other calibration bindings

G004 must use `active_eigenvalue_standardized_cutoff` exactly. G005 must use
`generalized_information_margin` exactly. An external gate artifact cannot
substitute a different threshold. The remaining constants retain the v2.2
metrics and consumers listed below.

- `chart_discrepancy_cutoff` and `nonlinear_remainder_cutoff` enter C003.
- `kkt_cutoff` enters F005.
- The five `integration_*_cutoff` constants enter I001 through I005.
- `near_optimum_objective_cutoff` and `multistart_target_se_cutoff` enter F008.
- `nested_fit_relative_cutoff` enters F007.
- `loading_probability_invariance_cutoff` enters L002.
- `derivative_cutoff` enters R001.
- `null_space_materiality_cutoff` and `numerical_rank_tolerance` enter the
  certified R002 calculation.
- `defect_cutoff` enters R003 and `ridge_cutoff` enters R004.

Together with the seven independent G006 constants and the exact G004 and G005
bindings, this accounts for all 26 production constants.

## 5. Production calibration files

The future package values file is
`inst/protocol/inference-v2.3-calibration-values.dcf`. The future approval
manifest is `inst/protocol/inference-v2.3-calibration.dcf`. Both must state the
v2.3 Protocol ID, `Status: frozen_for_production`, and `Rank-Scope: 1`.

The values file must contain exactly the 26 constants. The approval manifest
must repeat those exact values and bind the values-file SHA-256 digest and the
derived calibration hash. Missing, extra, altered, or nonfinite values fail
closed. All seven G006 component cutoffs must be strictly positive.

Neither production file may be created unless the prospective Step 3 study
passes all frozen validation criteria. Application outcomes and application
passage rates cannot determine the cutoffs.

## 6. Step 3 operating characteristics

The Step 3 study retains all nine v2.2 regimes. It must report unconditional
coverage, coverage conditional on release, false-certification rate, interval
length, empty-set rate, uninformative-set rate, and gate-passage frequency.
It must use response-free design templates and separate calibration and
validation draws. A pilot or incomplete study cannot produce the production
files described above.

No v2.1 or v2.2 application artifact is grandfathered into v2.3.
