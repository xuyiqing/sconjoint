# Ballard--Rosa v2.7 post-fit assessment

This is a separate post-fit assessment protocol. It does not amend or write to
the frozen v2.7 fitting protocol or its checkpoints. The only permitted inputs
are the completed, hash-verified q=0,1,2 assembled checkpoints. The only
permitted output is
`runs/br2017/v27/assessment-2026-09-07-v1/assessment.rds` and its SHA-256
sidecar.

The assessment records full-sample linear and unrestricted-quadratic design
ranks. It computes fitted-point covariance-tangent local-rank certificates for
q=1 and q=2 in raw coefficient units. Every numerical rank uses
`max(nrow, ncol) * .Machine$double.eps * leading singular value`. These
certificates concern local injectivity at the fitted loading. They do not prove
global injectivity, optimizer convergence, target-specific information, or
precise finite-sample inference.

The assessment also summarizes the already-created held-out predictions for
each rank, retained-sample response counts, transitions, switch patterns, and
task-order calibration. It carries forward the existing respondent-sequence
log scores and cross-checks the stored marginal calibration gap. Transition
and switch-pattern checks are descriptive.
The assembled checkpoints do not retain joint posterior-predictive response
paths. The assessment cannot diagnose model-based transition residuals or
selection into the retained analytic sample. Structural AME versus design AMCE is explicitly unavailable because
the assembled checkpoints do not contain a prespecified paired target map.
Constructing one would be a new target stage and is outside this protocol.

The covariance-tangent singular spectrum is a local information diagnostic.
Target-specific information remains unavailable because the assembled
checkpoints do not contain prespecified target derivatives or score callbacks.

After the protocol and implementation have been committed, run from the
package root:

```text
applications/bin/Rscript45 applications/br2017/R/08_assess_fast_v27.R status
applications/bin/Rscript45 applications/br2017/R/08_assess_fast_v27.R assess
```

The runner never prints substantive target estimates.
