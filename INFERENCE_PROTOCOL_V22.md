# Frozen inference protocol v2.2

**Protocol ID:** `paperps-inference-2026-09-05-v2.2`  
**Frozen on:** 2026-09-05  
**Status:** protocol structure frozen; simulation-calibrated constants pending  
**Machine-readable companion:** `inst/protocol/inference-v2.2.yaml`

## 1. Purpose and status

This document freezes the revised inference and reporting protocol before any
application is rerun under it. It is the implementation contract for the next
code revision. It does not certify the current code or any existing application
artifact.

The protocol has two parts. The estimator, diagnostic definitions, gate order,
resampling rule, multiplicity rule, and reporting statuses are frozen here.
Numerical cutoffs marked `CALIBRATE_IN_STEP_3` will be chosen in the simulation
stage using coverage, false-certification, and numerical-error criteria. They
must not be chosen to make Ballard--Rosa, Graham--Svolik, Saha--Weeks, or any
other application pass. No application may receive a v2.2 inferential status
until those constants and the code implementing this document are frozen and
fingerprinted together.

The words **must**, **must not**, **required**, and **withheld** are normative.
An amendment requires a new protocol ID, a dated explanation, and a rerun of
every affected application. Silent changes to a threshold, target family,
ridge grid, fold, seed, or reporting rule are prohibited.

This revision leaves the paper's substantive model and identification theory
unchanged. The flexible rank-one model remains primary. A theory-guided
restricted rank-one mean, rank zero, and rank two are sensitivity
specifications. Outcome-informed specifications must be labeled as such and
cannot be converted into confirmatory specifications on the same data.

## 2. Three distinct forms of eligibility

Eligibility belongs to an exact fit and an exact target. It does not belong to
an application in the abstract. Every reported result receives three statuses.

1. **Fit eligibility** records whether the exact structural fit satisfies the
   production computation and nuisance-verification requirements.
2. **Target eligibility** records whether the exact estimand satisfies its
   domain, local-information, inverse-problem, and linearization requirements.
3. **Interpretation eligibility** records what the randomized design and the
   specification assessments support.

A later check cannot cure an earlier failure. Predictive fit and a local-rank
certificate cannot override a failed target-specific inverse problem. A
range-respecting point or confidence set cannot cure weak identification,
boundary nonregularity, an inadmissible fit, or a failed Riesz approximation.

## 3. Objects frozen before estimation

Each application must have a signed registry containing the following objects
before a v2.2 fit begins.

- The primary and sensitivity specifications, including rank, mean family,
  moderator set, covariance restrictions, and residual distribution.
- The respondent sample, completion rule, missing-data rule, task order,
  contrast coding, display orientation, and respondent weights.
- Every target, its orientation and units, any subgroup, reference rule or
  frozen integration draw, and whether it is confirmatory, sensitivity, or
  outcome-informed.
- A signed registry ID for every unique scalar primitive and an explicit
  target-to-primitive map. Identical primitives share an ID only when their
  estimand, orientation, units, reference rule, and influence definition are
  identical.
- The substantive family containing each target. Families cannot be reduced
  after a target fails.
- Whole-respondent outer and inner folds, preprocessing rules, all random
  seeds, integration rules, optimization starts, learner grids, and the Riesz
  ridge grid.
- The code commit, data and design hashes, protocol ID, and calibration-file
  hash.

All folds contain whole respondents. A respondent's tasks may never be split
between training and evaluation samples. Every outcome-dependent nuisance
choice and preprocessing operation must occur inside the relevant training
sample. The same outer folds must be used for paired specification comparisons.

## 4. The four estimates retained for each target

The output must distinguish four quantities.

1. `full_sample_plugin` is the structural plug-in from the full-sample fit. It
   is used for structural description and figures.
2. `crossfit_plugin` is the fold-weighted plug-in used by the inferential
   procedure.
3. `raw_one_step` is the unrestricted cross-fitted one-step estimate. Its
   influence values are the basis for uncertainty calculations.
4. `reported_estimate` is the range-respecting, chart-stabilized one-step
   estimate defined below.

None may silently replace another. The raw estimate, correction, and all
diagnostics remain in the released artifact even when only the
range-respecting estimate is displayed in a table.

Let

\[
 \widehat\theta_{\mathrm{raw}}
 =\widehat\theta_{\mathrm{cf}}+\widehat\Delta
\]

denote the cross-fitted plug-in plus the fold-weighted Riesz score correction.
For a scalar target whose regular parameter space has a smooth interior chart
\(g\), define

\[
 \widehat\theta_{\mathrm{rr}}
 =g^{-1}\!\left\{
 g(\widehat\theta_{\mathrm{cf}})
 +g'(\widehat\theta_{\mathrm{cf}})\widehat\Delta
 \right\}.
\]

This is the reported estimate. For an unrestricted scalar, \(g\) is the
identity. For \((L,U)\), \(g(t)=\log\{(t-L)/(U-t)\}\). For \((L,\infty)\),
\(g(t)=\log(t-L)\). The analogous upper-bounded chart is used when needed.
Thus probabilities and shares use the logit chart and strictly positive
quantities use the log chart.

Suppose the truth lies in an interior neighborhood reached with probability
tending to one, \(g\) and \(g^{-1}\) are twice continuously differentiable
there with locally bounded first and second derivatives, and
\(\widehat\Delta=O_p(N^{-1/2})\). A Taylor expansion then gives

\[
 \widehat\theta_{\mathrm{rr}}
 =\widehat\theta_{\mathrm{raw}}+O_p(N^{-1}).
\]

It therefore has the same first-order influence function as the raw one-step
estimate under the paper's existing regularity conditions. This modest
interior result must be added to the theory before the estimator is described
as the paper's estimator. It is not a TMLE and it is not claimed to be valid
at a boundary. The same conclusion holds for the vector update below when the
chart, its inverse, and the target transformation satisfy the corresponding
multivariate conditions.

### 4.1 Composite targets

Nonlinear targets are built from the smallest nonredundant primitive vector.
The chart update is applied to that vector, after which the defining nonlinear
map is evaluated. The code must not apply a separate scalar correction to a
ratio when its primitive vector is available.

If \(G\) is the declared interior chart for a primitive vector \(\psi\), the
update is

\[
 \widehat\psi_{\mathrm{rr}}
 =G^{-1}\!\left\{G(\widehat\psi_{\mathrm{cf}})
 +DG(\widehat\psi_{\mathrm{cf}})
 (\widehat\psi_{\mathrm{raw}}-\widehat\psi_{\mathrm{cf}})\right\}.
\]

The reported composite point is \(h(\widehat\psi_{\mathrm{rr}})\). Its raw
linearized counterpart is
\(h(\widehat\psi_{\mathrm{cf}})+Dh(\widehat\psi_{\mathrm{cf}})
(\widehat\psi_{\mathrm{raw}}-\widehat\psi_{\mathrm{cf}})\).

- A probability or preference share is updated in its scalar logit chart.
- For a subgroup share with numerator \(a\) and denominator \(b\), use the
  coordinates \(\{\operatorname{logit}(a/b),\operatorname{logit}(b)\}\) on
  the interior \(0<a<b<1\).
- For directional heterogeneity, first construct the joint one-step influence
  contributions for the minimal vector \((H_Z,H_R)\), where
  \(H_Z=\operatorname{Var}\{d^\top\mu(Z)\}\) and
  \(H_R=d^\top\Sigma d\). The supporting moments used to calculate \(H_Z\)
  remain in the audit artifact. Use the coordinates
  \((\log H_Z,\log H_R)\). The reported heterogeneity share is
  \(H_Z/(H_Z+H_R)\).
- A mean-tradeoff or MRS ratio is based on joint numerator and denominator inference.
  Its point may be unrestricted, but the denominator margin remains a hard
  regularity condition.

If the cross-fitted plug-in is not in the relative interior of its declared
space, or if a required denominator or variance margin fails, the chart update
cannot support regular inference. The full-sample plug-in may still be shown as
a descriptive structural calculation when it is well defined.

## 5. Confidence sets and known parameter spaces

The chart-stabilized estimate and the raw one-step estimate have the same
first-order influence function under the interior equivalence conditions in
Section 4. Pointwise and simultaneous influence-function sets are therefore
centered on the reported chart-stabilized estimate and use the centered raw
one-step influence contributions. This rule keeps the displayed point inside
its ordinary interval while leaving the estimated first-order variance
unchanged. It cannot be used when the interior equivalence gate fails.

After constructing the influence-function set, intersect it with the known
target space.

For a scalar probability,

\[
 C_{\theta}^{\mathrm{reported}}
 =C_{\theta}^{\mathrm{IF}}\cap[0,1].
\]

If the true parameter belongs to the known space, intersection cannot reduce
the coverage event. This observation only preserves the validity of an
otherwise valid confidence set. It does not make an invalid linearization,
Riesz solve, or boundary approximation valid.

For a primitive vector, form a joint max-\(t\) rectangle from common respondent
multipliers, intersect it with the admissible primitive set, and map the
intersection through the target function. Endpoints are the infimum and
supremum of the target over that set. Numerical optimization used for this map
must be deterministic and verified against analytic solutions where available.

For directional heterogeneity the known support is

\[
 \mathcal K_H=\{(H_Z,H_R):H_Z\geq0,\ H_R\geq0\}.
\]

The confidence set for the share is the image of the intersected primitive
set, excluding the origin, under \(H_Z/(H_Z+H_R)\). The calibrated positive-
total-heterogeneity margin is a separate regularity gate. It is not inserted
into the known support unless the application explicitly maintains that lower
bound as a population assumption.

If the intersected region contains the origin and also contains admissible
positive-total-heterogeneity points, take the closure of the mapped set. The
reported share interval is then the fixed, uninformative interval \([0,1]\),
with release class `boundary_limited`, and no directional or majority claim is
allowed. Withhold only if the registered target is undefined or the
intersection contains no positive-total-heterogeneity point. A confidence
region that permits an MRS denominator of zero causes the ratio set to be
withheld.

An empty intersection is a hard failure. A nonempty set that touches a known
boundary receives the label `boundary_limited`. It is not described as
boundary-robust inference. The confidence set, not a clipped bootstrap point
distribution, determines the interval.

## 6. Linearization-stability diagnostics

The raw estimate leaving its natural range is a warning, not an automatic hard
failure. It must trigger the following prespecified diagnostics.

For every scalar target record

\[
 D_{\mathrm{corr}}
 =\frac{|\widehat\theta_{\mathrm{raw}}
 -\widehat\theta_{\mathrm{cf}}|}{\widehat{\mathrm{se}}},
 \qquad
 D_{\mathrm{lin}}
 =\frac{|\widehat\theta_{\mathrm{rr}}
 -\widehat\theta_{\mathrm{raw}}|}{\widehat{\mathrm{se}}}.
\]

For a nonlinear transform \(h(\psi)\), also record

\[
 D_{\mathrm{nonlin}}
 =\frac{\left|
 h(\widehat\psi_{\mathrm{rr}})
 -h(\widehat\psi_{\mathrm{cf}})
 -Dh(\widehat\psi_{\mathrm{cf}})
   (\widehat\psi_{\mathrm{raw}}-\widehat\psi_{\mathrm{cf}})
 \right|}{\widehat{\mathrm{se}}_h}.
\]

For a primitive vector let \(S=\operatorname{diag}(\widehat{\mathrm{se}}_1,
\ldots,\widehat{\mathrm{se}}_d)\). Freeze the remaining diagnostics as

\[
 D_{\mathrm{domain}}
 =\inf_{u\in\mathcal K}\|S^{-1}
 (\widehat\psi_{\mathrm{raw}}-u)\|_\infty,
 \qquad
 D_{\mathrm{boundary}}
 =\inf_{u\in\partial\mathcal K}\|S^{-1}
 (\widehat\psi_{\mathrm{cf}}-u)\|_\infty.
\]

Use the declared primitive coordinates and their marginal standard errors.
For a scalar these reduce to distance divided by its standard error. Also
record whether the pointwise and simultaneous confidence sets intersect the
boundary. No coordinate with a zero or nonfinite standard error may enter this
calculation. Such a coordinate is a hard target failure.

After Step 3, failure of the calibrated interior-distance or linearization
threshold is a hard target-level failure. Mere range violation by the raw
one-step point is not. The formulas and scaling above are frozen and cannot be
changed during an application rerun.

## 7. Gate taxonomy and order

Every gate row must contain `gate_id`, `category`, `scope`, `severity`,
`state`, `statistic`, `threshold`, `comparator`, `units`, `provenance`, and a
human-readable message. States are `pass`, `warn`, `fail`, `not_run`, and
`not_applicable`. The machine-readable registry gives an applicability
predicate for every conditional gate. A false predicate produces
`not_applicable` and requires a recorded reason. It is not a pass. Missing
evidence for an applicable required hard gate is a failure, not a pass.

| Order | Category | Required checks | Consequence |
|---:|---|---|---|
| 1 | Protocol integrity | Signed target registry, family membership, folds, seeds, hashes, protocol and calibration IDs | Hard fit or target failure |
| 2 | Design and data | Randomization reconstruction, support, exposure, position, response sequences, completion and missingness | Interpretation supported, qualified, or unsupported |
| 3 | Fit computation | Double precision, finite objective, identical objective across stages, full-batch L-BFGS polish, blockwise scale-free projected KKT, inactive artificial bounds, nested-fit and multi-start checks | Hard fit failure |
| 4 | Numerical integration | Common frozen draws, higher-resolution likelihood, score, covariance, and target stability | Hard fit or affected-target failure |
| 5 | Loading contract | Explicit raw and optimization-scale loadings, round-trip test, fitted-probability invariance to equivalent preprocessing scales | Hard fit failure |
| 6 | Nuisance verification | Whole-respondent cross-fitting, training-only preprocessing/tuning, fitted- or growing-sieve record, artifact signatures and implementation provenance | Hard fit failure |
| 7 | Identification and regularity | Fixed rank, exact fold and full-fit rank, active eigenvalue margin, fitted-point local-injectivity certificate with named scope, target information and domain margins | Hard fit or target failure |
| 8 | Target inverse problem | Analytic or refined derivatives, honest held-out Riesz defect, vanishing-ridge provenance and neighborhood stability | Hard target failure |
| 9 | Constraint and linearization | Interior plug-in, nonempty constrained confidence set, calibrated boundary and linearization diagnostics | Hard target failure or boundary label |
| 10 | Resampling and multiplicity | Positive finite SE, one multiplier per respondent, common family draws, frozen family size | Hard target or family failure |
| 11 | Specification assessment | Sequence scores and calibration, within-respondent checks, AME--AMCE benchmark, shape, scale, dependence, position, completion and missingness sensitivity | Interpretation supported, qualified, or unsupported |

### 7.1 Scale-invariant computation gate

The frozen objective \(Q\) is the mean negative complete-sequence log
likelihood per respondent plus the declared mean-deviation penalty, expressed
in nats. Its zero is the negative log likelihood of probability one with zero
penalty. It is evaluated with the same fixed numerical-integration rule at
every optimizer stage. Arbitrary sum-versus-mean or sign changes are
prohibited.

Write each optimization block as \(\theta_b=S_bu_b\), where \(S_b\) is a
positive diagonal scale matrix declared before that fit from training-only
design preprocessing and optimizer scales. Let \(C_b^u\) be its feasible set
in \(u\) coordinates, \(g_b^u=\nabla_{u_b}Q\), and
\(F=\max(1,|Q|)\). Define

\[
 r_b=\frac{1}{\max(1,\sqrt{d_b})}
 \left\|u_b-\Pi_{C_b^u}
 \{u_b-g_b^u/F\}\right\|_2.
\]

For a scalar box constraint this equals the corresponding scaled coordinate
projection. For loadings, first quotient out rotational directions and use the
horizontal structural tangent block. Do not apply coordinatewise projection to
raw factor rotations. Report \(r_b\) for the position effect, unpenalized mean
reference, centered mean deviation, horizontal loading directions, and the
joint structural vector. Because \(S_b\) changes with an equivalent diagonal
reparameterization, this residual is invariant to the recorded parameter
units. The fixed mean-per-respondent objective removes task-count and
sum-versus-mean dependence.

The same declared objective must be used in stochastic training and polishing.
Every candidate that enters an outer-fold score comparison, and every selected
full or fold fit, must receive a double-precision full-batch L-BFGS polish.
Store the objective and residuals before and after polishing, iteration and
line-search status, data type, and a non-worsening check. Activity at an
artificial bound is a hard failure even if a constrained KKT condition is
satisfied.

Numerical rule \(a\) is the production integration rule. Rule \(b\) is the
signed higher-resolution rule and nests the nodes or draws of \(a\) whenever
the integration method permits. Refinement has two required stages. First,
hold the level-\(a\) parameters fixed and reevaluate the likelihood, full-batch
score, covariance, targets, Riesz solutions, and influence values under
\(b\). Second, start from the polished level-\(a\) parameters, polish the same
objective under \(b\), and recompute those objects. Nuisance model selection
and folds remain fixed. Gate on the worse discrepancy from the fixed-fit and
repolished comparisons.

For either comparison define

\[
 D_{\mathrm{int},\theta}=|\widehat\theta_b-\widehat\theta_a|/
 \widehat{\mathrm{se}}_a,
 \qquad
 D_{\mathrm{int},\varphi}=
 \{N^{-1}\sum_i(\widehat\varphi_{i,b}-
 \widehat\varphi_{i,a})^2\}^{1/2}/
 \widehat\sigma_{\varphi,a}.
\]

Also define
\(D_{\mathrm{int},Q}=|Q_b-Q_a|/\max(1,|Q_a|)\),
\(D_{\mathrm{int},g}=\|g_b^u-g_a^u\|_2/
\max(1,\|g_a^u\|_2)\), and
\(D_{\mathrm{int},\Sigma}=\|\Sigma_b-\Sigma_a\|_F/
\max(1,\|\Sigma_a\|_F)\). The target and influence gates use the maximum
across registered targets and unique primitive columns. The objective, score,
and covariance gates use the maximum across full and outer-fold fits. For
multiple starts, let \(Q_*\) be the lowest polished objective and define
\(D_{Q,s}=(Q_s-Q_*)/\max(1,|Q_*|)\). Among starts satisfying the calibrated
near-optimum objective rule, define
\(D_{\mathrm{start}}=\max_s
|\widehat\theta_s-\widehat\theta_*|/\widehat{\mathrm{se}}_*\).
The nested-fit violation is
\(D_{\mathrm{nest}}=\max(0,Q_{\mathrm{full}}-
Q_{\mathrm{restricted}})/\max(1,|Q_{\mathrm{restricted}}|)\), using the
same objective and a restriction exactly embedded in the full family.

The KKT, near-optimum, multi-start, nested-fit, and numerical-integration
cutoffs are `CALIBRATE_IN_STEP_3`. Their definitions and maximum aggregation
rules are frozen here.

### 7.2 Structural regularity

The local-injectivity certificate is recomputed for every final specification
at its fitted point and names the structural block and contrast set it covers.
The signed target registry maps every target to the parameter blocks on which
it depends. A failed scoped certificate withholds only targets that depend on
that block. It becomes a fit-level failure only when the certificate covers the
full identified structural parameter or every registered target depends on the
failed block. Full local rank permits the affected analysis to continue. It
does not establish global injectivity, precise finite-sample inference, or
regularity of a particular nonlinear target.

Known support and empirical separation are distinct. Nonnegative variances,
the probability simplex, and the positive-total-heterogeneity domain define
the sets used for confidence-set intersection. Fixed population interior
conditions in the theorem remain maintained assumptions. They are not created
by a diagnostic threshold.

The empirical release diagnostics are standardized. For a positive scalar
\(v\), use \(v/\widehat{\mathrm{se}}(v)\). For a signed denominator \(b\),
use \(|b|/\widehat{\mathrm{se}}(b)\). For a scalar in \((L,U)\), use the
minimum of \((v-L)/\widehat{\mathrm{se}}(v)\) and
\((U-v)/\widehat{\mathrm{se}}(v)\). Apply the positive-scalar rule to active
covariance eigenvalues, directional variances, and total heterogeneity, using
their respondent-level delta or bootstrap standard errors. If such a standard
error cannot be computed, the gate fails. Raw-unit estimates and the
generalized-information eigenvalues in the declared structural norm remain
supporting diagnostics. They are not mixed with the standardized boundary
gate. All standardized cutoffs are `CALIBRATE_IN_STEP_3`. A boundary variance
component remains outside ordinary v2.2 inference.

The high-level nuisance-rate, product-rate, and likelihood-approximation
conditions remain maintained theoretical conditions. One dataset cannot verify
them. The gate checks whether the declared sieve, nesting, preprocessing,
signatures, approximation study, and written rate argument are present and
internally consistent. It must never report that the rates themselves were
empirically proved.

### 7.3 Honest target-specific Riesz gate

Each scalar primitive receives its own Riesz solve and ridge selection. The
primitive components needed for a nonlinear target retain identical folds and
respondent ordering so their joint covariance is preserved. A single ridge may
not be selected by summing validation risk over unrelated targets.

Derivatives are expressed in structural-orthonormal coordinates. With machine
precision \(\epsilon\), use centered differences at normalized-coordinate
steps \(h_j=\epsilon^{1/3}\max(1,|u_j|)\) and \(h_j/2\). For a numerical
derivative, define
\(D_{\mathrm{deriv}}=\|g_h-g_{h/2}\|_2/
\max(\|g_{h/2}\|_2,10^{-15})\). For an analytic derivative, replace the
numerator by the maximum of \(\|g_{\mathrm{analytic}}-g_{h/2}\|_2\) and
\(\|g_h-g_{h/2}\|_2\), with the analytic norm in the denominator. Runtime
checks may use a deterministic signed subset of coordinates when the full
audit is prohibitive. The subset rule and derivative cutoff are frozen before
the fit. The cutoff is `CALIBRATE_IN_STEP_3`.

Let \(N\) be the number of sampled respondents and \(n_k\) the number in outer
training sample \(k\). Within that sample use the signed respondent-level inner
folds. For inner fold \(v\), all outcome-dependent nuisance fitting and
preprocessing that enters its score or target derivative is repeated using
only the inner-training respondents. Estimate
\(\widehat\alpha_{k,-v}(\nu)\) there. On the \(n_{kv}\) inner-validation
respondents define

\[
 I_{kv}=n_{kv}^{-1}\sum_{i\in V_{kv}}s_is_i^\top,
 \qquad
 g_{kv}=n_{kv}^{-1}\sum_{i\in V_{kv}}\dot\Psi_i,
 \qquad
 r_{kv}(\nu)=I_{kv}\widehat\alpha_{k,-v}(\nu)-g_{kv}.
\]

The validation Riesz risk is
\(\widehat\alpha^\top I_{kv}\widehat\alpha
-2\widehat\alpha^\top g_{kv}\). Average it over inner folds with weights
\(n_{kv}/n_k\). Select the smallest risk. Values within
\(10^{-12}\max(1,|\text{minimum risk}|)\) are numerical ties, resolved in
favor of the largest ridge. These inner-fold quantities tune ridge and remain
tuning diagnostics. They do not certify the selected solve.

After selection, refit the Riesz coefficient on all outer-training
respondents. On the untouched outer evaluation fold \(E_k\), with
\(m_k=|E_k|\), compute

\[
 I_k^{E}=m_k^{-1}\sum_{i\in E_k}s_is_i^\top,
 \qquad
 g_{kj}^{E}=m_k^{-1}\sum_{i\in E_k}\dot\Psi_{ij},
 \qquad
 r_{kj}^{E}=I_k^{E}\widehat\alpha_{kj}-g_{kj}^{E}.
\]

Use an eigen-decomposition in the declared structural norm. Let
\((I_k^{E})^{+}\) use only eigenvalues above the recorded numerical-rank
tolerance and let \(P_k^{E}=I_k^{E}(I_k^{E})^{+}\). Record the certification
range defect

\[
 D_{\mathrm{null},k}
 =\max_j\frac{\|(I-P_k^{E})g_{kj}^{E}\|_2}
 {\max(\|g_{kj}^{E}\|_2,10^{-15})}.
\]

A material null-space component is a hard failure and may not be hidden by a
Moore--Penrose inverse. Conditional on passing that check, define

\[
 D_{R,kj}=\sqrt{N/n_k}\,
 \frac{\{(r_{kj}^{E})^\top (I_k^{E})^{+}r_{kj}^{E}\}^{1/2}}
 {\widehat\sigma_{\varphi,j}}.
\]

This is the worst representer defect for a one-information-standard-error
local nuisance perturbation, measured relative to the target's influence
standard deviation. Here \(j\) indexes a unique scalar primitive and
\(\widehat\sigma_{\varphi,j}\) is the standard deviation of its final pooled
cross-fitted respondent influence contributions over all \(N\) respondents.
Gate on the maximum over outer folds and primitive components. The
numerical-rank and null-defect cutoffs are
`CALIBRATE_IN_STEP_3`. A singular direction not removed by the declared
tangent reduction is a failure. The present in-sample relative equation
residual is retained only as a legacy diagnostic.

The frozen ridge grid is

\[
 \{0,n_k^{-1},n_k^{-3/4},n_k^{-1/2}\}.
\]

The stability neighborhood contains zero and the immediately adjacent grid
values around the selected ridge. Recompute the chart-stabilized target and
its confidence endpoints throughout this neighborhood and set

\[
 D_\nu=\max_{\nu\in\mathcal N(\widehat\nu)}
 \frac{\max\{|\widehat\theta_\nu-\widehat\theta_{\widehat\nu}|,
 |L_\nu-L_{\widehat\nu}|,|U_\nu-U_{\widehat\nu}|\}}
 {\widehat{\mathrm{se}}_{\widehat\nu}}.
\]

For a composite target, vary one primitive component's ridge at a time while
holding the other components at their selected values. Cartesian combinations
are recorded only as an optional stress diagnostic. Gate on the maximum over
folds, primitive components, and one-at-a-time perturbations. Unlike the old
selected-versus-zero calculation, this diagnostic is not identically zero
when zero ridge is selected. The cutoffs for \(D_R\),
\(D_{\mathrm{null}}\), and \(D_\nu\) are
`CALIBRATE_IN_STEP_3`. The old 0.05 relative-residual and 0.10 relative-
correction rules remain in output for historical comparison only. They are
not v2.2 certification gates because their metrics differ from those above.

## 8. Respondent-level resampling and multiplicity

The primary uncertainty calculation is a respondent-level multiplier
bootstrap applied to the cross-fitted influence matrix.

- Use 9,999 standard Normal multiplier draws for final results.
- Draw one multiplier per respondent. Apply it to the respondent's complete
  response sequence and to every unique scalar primitive needed by the family.
- Generate the multiplier matrix once from the signed analysis seed and reuse
  it across separately tuned target objects. Results must be invariant to
  target order and job partition.
- Deduplicate primitives with the same signed registry ID. Stack the unique
  primitive influence columns for the prespecified family and use their maximum
  absolute studentized statistic. Map the resulting simultaneous primitive
  rectangle into each registered target set.
- A familywise majority or directional claim uses the simultaneous interval,
  not a pointwise interval.

If every registered primitive has a usable influence contribution, the
common-draw max-\(t\) construction is primary. A failed primitive may not be
dropped and the maximum recomputed on a favorable subset. If a primitive has
no usable influence contribution, targets whose own required primitives remain
eligible use a fixed-family Bonferroni fallback based on the original number of
unique registered primitive coordinates. The family does not receive a
complete simultaneous-inference label.

A full respondent pairs bootstrap is optional diagnostic evidence. If used,
it resamples respondents with all their tasks and repeats preprocessing,
tuning, structural fitting, Riesz estimation, target construction, and all
gates. Report convergence and gate-passage rates. A design-conditional
parametric bootstrap belongs to Step 3 simulation validation. Neither method
overrides a failed Riesz, ridge, computation, identification, or boundary
gate. Tasks are never independently resampled. Ordinary bootstrap inference
is not a cure for a variance component on the boundary.

## 9. Required output statuses

Do not overload one status string. Every result stores the following fields.

- `calculation_status`: `not_computed` or `computed`.
- `fit_status`: `eligible`, `diagnostic_only`, or `withheld`.
- `target_status`: `conditional_available`, `diagnostic_only`, or `withheld`.
- `release_class`: `regular_interior`, `boundary_limited`,
  `constraint_mapped`, or `none`.
- `interval_method`: `regular_if`, `support_intersection`,
  `primitive_set_mapping`, `fixed_family_bonferroni`, or `none`.
- `multiplicity_status`: `simultaneous_complete`, `bonferroni_incomplete`,
  `pointwise_only`, or `none`.
- `interpretation_status`: `supported`, `qualified`, or `unsupported`.
- `hard_failure_codes`, `warning_codes`, and `first_failed_gate`.

The phrase **conditional available** remains literal. It is conditional on the
maintained structural model, the documented high-level nuisance-rate and
likelihood-approximation assumptions, and the successful empirical and
numerical gates. The data do not prove those high-level assumptions.

An out-of-range raw one-step point can coexist with a nonempty support-
intersected confidence set. It is then recorded as a range warning. It receives
formal status only if all other hard gates, including the calibrated
linearization check, pass. A majority claim is allowed only when the relevant
simultaneous or fixed-family interval lies wholly on one side of one-half.

## 10. Specification assessment required for interpretation

Every application must include the following common battery.

- Held-out complete-sequence log scores and calibration for the flexible
  rank-one model, the restricted rank-one mean, and fresh rank-zero and rank-two
  sensitivities fitted through the same pipeline.
- Within-respondent response-count, transition, and repeated-contrast checks.
- Position and task-order checks.
- Structural AME versus design AMCE comparisons on supported contrasts.
- Residual-shape, serial-dependence, covariance, and scale sensitivities.
- Completion, exclusion, and missingness sensitivities.
- Target-specific information, Riesz, ridge, domain, and linearization
  diagnostics.

These checks discipline interpretation. They do not prove the model correct.
Off-support quantities remain structural extrapolations. A complete-case
analysis targets the completer population unless stronger completion
assumptions are defended. A favorable global score or calibration plot does
not override a failed target gate.

## 11. Scope exclusions

The following are not part of v2.2 ordinary inference and require new theory
or a separately labeled analysis.

- Uniformly valid inference when a loading, directional variance, total
  heterogeneity, or denominator equals or approaches its boundary.
- Weak-identification-robust likelihood-ratio or test-inversion procedures.
- A TMLE or likelihood-targeting fluctuation beyond the chart-stabilized
  first-order update defined here.
- Post-selection inference after choosing rank or the mean specification using
  the same outcomes.
- Treating a pairs or parametric bootstrap as a replacement for a failed
  inverse-problem or boundary gate.
- Treating a zero-heterogeneity detection threshold as an upper confidence
  limit for true dispersion.
- General confidence-cone inference for an unrestricted covariance matrix.
- Claiming that simulation-calibrated diagnostic cutoffs supply a theorem-
  level finite-sample guarantee.

The exported legacy `scmix_signshare()` routine uses a different three-gate
scheme. It is not paper-aligned v2.2 inference and must be marked legacy and
diagnostic until it is reconciled or retired.

## 12. Step 3 calibration contract

Step 3 may assign numbers, but it may not change the formulas, gate order, or
failure semantics in this document. It must calibrate at least the following.

- The blockwise projected-KKT cutoff.
- Target-scaled integration and multi-start tolerances.
- Active-eigenvalue, information, directional-variance, denominator,
  subgroup, total-heterogeneity, and boundary-distance margins.
- The honest Riesz defect cutoff.
- The ridge-neighborhood sensitivity cutoff.
- The chart/nonlinear linearization cutoff.

Simulation regimes must include well-informed interior targets, probabilities
near a boundary, nearly zero directional variation, weak inverse problems,
optimization and integration error, residual-shape misspecification, serial
dependence, and informative completion. Evaluation reports unconditional and
pass-conditional coverage, false certification, interval length, empty or
uninformative constrained sets, and gate frequency. Threshold selection is
based on those operating characteristics, not on the desired narrative for an
application.

## 13. Current implementation crosswalk

This is a prospective protocol. The current code is not yet v2.2 compliant.

| Current behavior | Required v2.2 behavior |
|---|---|
| Float32 Torch optimization with Adam | Double precision plus full-batch L-BFGS polish |
| Absolute maximum gradients | Scale-invariant blockwise projected KKT residuals |
| Ambiguous loading aliases | One raw/optimization-scale accessor and probability-invariance tests |
| In-sample Riesz equation residual | Honest respondent-held-out, information-scaled defect |
| Selected ridge compared only with zero | Full declared stability neighborhood, including a nonzero neighbor when zero is selected |
| One ridge selected from summed risk across target columns | Separate solve per scalar primitive while preserving joint respondent covariance |
| Separate target calls are not restacked | Common reusable multiplier matrix and family aggregation |
| Additive, possibly out-of-range one-step point | Retain raw point and report chart-stabilized point |
| Unbounded Wald and multiplier intervals | Support intersection and primitive-set mapping |
| No substantive linearization gate | Standardized correction, boundary, and nonlinear discrepancy diagnostics |
| Missing legacy eligibility fields can pass | Explicit protocol-versioned evidence required; missing means fail |
| Two incompatible sign-share gate systems | Legacy routine excluded from paper-aligned inference |

Step 2 must implement this contract and its tests. Existing v2.1 fits and
application results remain useful diagnostics, but none is grandfathered into
v2.2 certification.
