# Lean v2.4 validation scaffold

Protocol v2.4 replaces threshold calibration with prospective validation of
fixed rules. It has no calibration stream. Its two integrity replications per
cell are excluded from evidence. Its 100 evidence replications per cell remain
locked in this commit.

The September 7 pre-outcome amendment corrects one target-registry label. For
directional heterogeneity, the inferential primitives are `H_Z` and `H_R`.
The directional mean and second moment are retained as supporting audit
columns used to construct `H_Z`; they are not extra familywise multiplier
coordinates. This matches the paper and the existing minimal-primitive code.
No pilot or evidence outcome existed when the label and hashes were corrected.

A final pre-outcome amendment on September 7 freezes the complete evaluator
and its implementation-manifest contract. It explicitly binds the fixed-ridge
and dimension-adjusted SVD rules, the two GH31 branches, exact-state zero-Adam
polishing, target mappings, and the pilot information firewall. Toy tests also
revealed that this R/Torch build could retain unsafe backing storage when a
tensor was made from a temporary indexed R slice. Fit tensors now own cloned
storage. This is a general correctness repair, not an application-specific
adjustment. No v2.4 outcome existed when it was made.

The selected full and outer-fold states carry the KKT, loading, and nesting
gates. Near-optimum states carry polish, precision, finiteness, and target-
stability checks. Discarded far starts remain descriptive. All three GH31
repolishes must satisfy the selected-state numerical checks. Directional-share
evaluation uses each estimand's declared tie rule at zero variance with no
positive floor. Preference direction excludes exact ties and compensating
benefit includes them. An auxiliary directional-variance primitive prevents a
regular release unless it is at least 1.96 standard errors from zero. This is
a pre-outcome clarification of the estimands already defined in the paper.

The three 400-respondent templates are deterministic response-free subsets of
the immutable v2.3 design wrappers. The v2.4 manifest pins each source wrapper,
selected respondent vector, and derived response-free payload. No observed
choice is read or stored.

Each future replication resamples 400 complete respondent records with
replacement from one frozen template. It preserves each selected respondent's
whole task sequence and assigns new replication-specific IDs before drawing
the latent utility shock and choices. Truth is defined under the original
400-respondent empirical distribution. This validates respondent-level
superpopulation variation rather than conditioning on a single fixed moderator
composition.

The DGP uses the same uncentered respondent-weighted RMS transformation as the
estimator. Regular coefficient directions are standardized by their design
eigenvalues. The weak direction is deliberately not standardized. Its score
RMS therefore remains the square root of the smallest design eigenvalue.
The six-cell response-free truth table is also hash-frozen. It contains twelve
target variants and the active rank-one covariance eigenvalue, which equals
`trace(Sigma)` for rank one. Probability targets use deterministic GH257
integration.

The protocol fixes a deterministic family-by-cell approval matrix. All cells
must complete at least 95 of 100 evidence replications. A regular family needs
at least 75 releases and at least 90 percent conditional simultaneous coverage
for its fixed 95 percent familywise set. A two-sided 90 percent Wilson interval
and binomial Monte Carlo standard error are reported descriptively. An exact
boundary family must have an observed invalid regular-release rate no larger
than 0.05 and a one-sided exact 95 percent Clopper-Pearson upper limit no
larger than 0.10.
Incomplete families and Bonferroni fallbacks do not count as releases. The
matrix records all 36 family-cell pairs. Thirty determine approval and six
unaffected special-cell pairs are descriptive controls.

The simultaneous critical value is the type-1 empirical 0.95 quantile of the
maximum absolute studentized statistic over all unique family primitives from
9,999 common standard-normal respondent multipliers. Influence columns are
centered before the multiplier statistic is formed.

The twelve excluded pilots may reveal only integrity, elapsed time, and
resource use. At most six workers are allowed. The evidence study proceeds
only if its projected wall time is no more than eight hours. Otherwise it stops
and any redesign receives a new protocol ID. This firewall is a procedural,
hash-audited rule rather than encryption. The public pilot interface exposes
only the prespecified metadata. A signed private ledger records each sealed
artifact's path and file hash. Interrupted pilot work may be reused only after
exact read-back validation, and elapsed wall time always begins at the original
wave start. All 600 evidence attempts are required, with no early success or
futility stop.

The response-free design, truths, target definitions, seeds, decision rules,
and one end-to-end evaluator are frozen. Its complete implementation manifest,
settings, and interface must be bound by SHA-256 in
`inst/protocol/inference-v2.4-evaluator-approval.dcf` before any outcome is
generated. At the instant of the final pre-outcome amendment that approval was
absent and `scmix_v24_generate_study_outcome()` failed closed. The live state
machine records subsequent pilot, evidence, and family-approval state.

A successful study would validate the implementation of these fixed rules on
the three finite empirical templates. It would not certify an application.
Each application must still pass its own design, fit, numerical,
approximation, and target-specific information gates.
