# Graham–Svolik corrected targeted q = 1 results

Run date: 2026-09-03. Code commit: `d11046e7e7009d83d694e8203dfe739bd1e7fea5`.
Primary output: `runs/gs2020/v21_corrected/targeted_q1_estimand/`.

## What changed

The application now uses raw-scale loading vectors throughout the reported
quantities. This removes the earlier application-layer double scaling. The new
runner also estimates each direction and each overall or subgroup compensating
share separately. Each directional heterogeneity decomposition keeps only its
three jointly required primitives. Riesz ridge selection is therefore
estimand-specific rather than pooled over 10, 49, or 30 outputs. The output now
reports plug-in transforms for subgroup shares and heterogeneity decompositions,
retains all target-level Riesz and ridge diagnostics, and labels diagnostic
ranges as something other than confidence intervals. It wrote 48 restartable
estimand checkpoints. The run requested 1,999 multiplier draws, but none were
executed because every target was withheld before the multiplier step.

## What worked

The saved q = 1 fit and restored network states agree to
`2.154e-7`, below the `5e-6` tolerance. The loading has rank one in every fold.
The minimum active eigenvalue is 0.792, and the minimum structural-information
eigenvalue is 0.00793. Both exceed their declared floors. All 10 sign directions
clear the `1e-6` variance floor, although restricting voting is borderline at
`1.069e-6`. Ideology-group probabilities are 0.310, 0.331, and 0.358, well above
the 0.10 ratio margin. The corrected plug-in results are substantively coherent
and the broad patterns reproduce the earlier descriptive findings.

The corrected plug-in share preferring a co-partisan is 0.832. Plug-in shares
opposing the seven undemocratic actions range from 0.976 to 0.997. Opposition to
the affair and tax valence violations is essentially universal in the fitted
model. The overall plug-in compensating shares for one unit of co-partisanship
are as follows.

| Undemocratic action | Overall | Liberal | Moderate | Conservative |
|---|---:|---:|---:|---:|
| Ban protests | 0.619 | 0.642 | 0.609 | 0.610 |
| Ignore courts | 0.513 | 0.441 | 0.504 | 0.583 |
| Rule by executive order | 0.602 | 0.555 | 0.597 | 0.648 |
| Gerrymander by 2 seats | 0.624 | 0.610 | 0.618 | 0.641 |
| Gerrymander by 10 seats | 0.539 | 0.528 | 0.526 | 0.561 |
| Punish journalists | 0.452 | 0.362 | 0.433 | 0.546 |
| Restrict voting | 0.505 | 0.452 | 0.509 | 0.548 |

The plug-in share of total directional heterogeneity explained by observed
covariates is 0.044 for co-partisanship. It is 0.456, 0.761, 0.835, 0.972,
0.628, 0.701, and 0.961 for the seven actions in the table order, and 0.923 and
0.905 for the affair and tax directions. Thus fitted co-party heterogeneity is
almost entirely residual, while much of the fitted heterogeneity in reactions
to democratic and valence violations is associated with observed covariates.

## What did not work

Estimand-specific tuning did not rescue inference. No direction, none of the 28
overall or subgroup compensating shares, and none of the 10 heterogeneity
decompositions clears both numerical gates. Directional Riesz residuals range
from 0.090 to 0.360 and ridge sensitivities from 0.168 to 0.524, against limits
of 0.05 and 0.10. The corresponding ranges are 0.151–0.196 and 0.307–0.408 for
compensating shares, and 0.109–0.436 and 0.168–0.583 for heterogeneity.

Eight of 10 one-step direction estimates fall outside `[0,1]`. Only two of 10
one-step heterogeneity decompositions stay in the parameter space. Several have
negative residual variance or an explained share above one, and the tax share
is numerically singular. The fit is also marked diagnostic-only and ineligible
for ordinary inference. Consequently none of the displayed diagnostic ranges
is a confidence interval. The plug-in estimates may support transparent,
model-based descriptive discussion, but this run does not support reliable
uncertainty statements or strong inferential claims about heterogeneity.

Relative to the earlier application, correcting the scale changes the co-party
share from about 0.746 to 0.832. It preserves the earlier ideology ordering for
the clearest compensators but compresses some gaps. For example, the submitted
liberal/conservative values of 0.38/0.66 for punishing journalists become
0.362/0.546, 0.47/0.72 for ignoring courts becomes 0.441/0.583, and 0.46/0.65
for restricting voting becomes 0.452/0.548. The earlier heterogeneity pattern
is broadly reproduced by the corrected cross-fitted plug-ins, but not by the
one-step estimates. Graham–Svolik should therefore be described as a mixed
application or stress test unless a new, prospectively specified analysis
clears the inference gates.
