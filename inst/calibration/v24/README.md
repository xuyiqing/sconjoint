# Lean v2.4 validation scaffold

Protocol v2.4 replaces threshold calibration with prospective validation of
fixed rules. It has no calibration stream. Its two integrity replications per
cell are excluded from evidence. Its 100 evidence replications per cell remain
locked in this commit.

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
and any redesign receives a new protocol ID.

The response-free design, truths, target definitions, seeds, and decision rules
are frozen. The estimator implementation is not yet frozen. Before any
design-matched outcome can be generated, one concrete end-to-end evaluator and
all of its settings must be implemented, integrity-tested, and bound by SHA-256 in
`inst/protocol/inference-v2.4-evaluator-approval.dcf`. That file is absent.
`scmix_v24_generate_study_outcome()` therefore fails closed.

A successful study would validate the implementation of these fixed rules on
the three finite empirical templates. It would not certify an application.
Each application must still pass its own design, fit, numerical,
approximation, and target-specific information gates.
