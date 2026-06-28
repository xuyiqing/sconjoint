# sconjoint

**Structural Deep-Learning Estimation for Conjoint Experiments**

`sconjoint` implements the structural deep-learning estimator of
[Acharya, Hainmueller, and Xu (2026)](https://arxiv.org/abs/2604.10845) for
forced-choice conjoint experiments. The estimator embeds a deep neural
network inside a random utility logit so that each respondent's
preference vector varies flexibly with observed covariates.
Double/debiased machine-learning (DML) inference delivers
respondent-clustered standard errors on all quantities.

## Installation

```r
install.packages("torch")
torch::install_torch()

# install.packages("remotes")
remotes::install_github("xuyiqing/sconjoint")
```

## Quick example

```r
library(sconjoint)
data(sw2022)

# Candidate attributes (left of |) and respondent moderators Z (right of |).
# sw2022 ships pre-coded dummy columns; dotted names are backquoted.
attrs <- c("cand_genderMale", "cand_runYes",
           "cand_talentCollaborative", "cand_talentDetermined.to.Succeed",
           "cand_talentEmpathetic", "cand_talentGood.Communicator",
           "cand_talentHard.Working", "cand_talentTough.Negotiator",
           "cand_agendaModerate.Changes", "cand_agendaComplete.Overhaul",
           "cand_child1.child", "cand_child2.children", "cand_child3.children")
Zvars <- c("gender_num", "age", "income", "educ_Middle", "educ_High",
           "party_Republican", "party_Independent", "region_NORTHEAST",
           "region_SOUTH", "region_WEST", "employ_parttime", "employ_homemaker",
           "employ_not_working", "employ_retired", "employ_student",
           "ideo_conservative", "vote_trump", "vote_clinton", "gender_att")

bt   <- function(x) paste0("`", x, "`")
form <- as.formula(paste("choice ~", paste(bt(attrs), collapse = " + "),
                         "|",        paste(bt(Zvars), collapse = " + ")))

fit <- scfit(
  form,
  data = sw2022, respondent = "respondent",
  task = "task", profile = "profile",
  K = 5L, n_epochs = 200L, seed = 2024
)

summary(fit)
plot_amce(fit)
plot_fraction(fit)
plot(fit, "beta_ridgelines")
```

## Features

**Estimator**: `scfit()` --- DNN-based structural conjoint with DML
inference, respondent-clustered cross-fitting, bit-exact determinism
across core counts.

**Structural quantities** (21 functions):

| Tier | Functions |
|------|-----------|
| Core | `sc_mrs`, `sc_counterfactual`, `sc_wtp`, `sc_importance`, `sc_polarization`, `sc_fraction_preferring`, `sc_optimal_profile`, `sc_direction_intensity`, `sc_heterogeneity_test` |
| Advanced | `sc_subgroup`, `sc_compensating`, `sc_clusters` |
| Welfare | `sc_surplus`, `sc_welfare_change`, `sc_average`, `sc_demand_curve` |
| Diagnostics | `sc_indifference`, `sc_decisiveness`, `sc_inequality` |

**Baselines**: `sc_baseline_logit`, `sc_baseline_lpm` for
side-by-side comparison with the structural model.

**Plots** (7 functions):

| Function | Purpose |
|----------|---------|
| `plot_amce()` | AMCE coefficient plot with CI |
| `plot_fraction()` | Fraction favor/oppose diverging bar |
| `plot_hetero()` | Preference heterogeneity bar chart |
| `plot_subgroup()` | Subgroup AMCE comparison |
| `plot_importance()` | Attribute importance ridgelines |
| `plot(fit, "beta_ridgelines")` | Per-respondent preference distributions |
| `plot(fit, "loss_trace")` | Training convergence |

All plot functions accept `dummies`, `labels`, `groups` for
customization. See the Plot Options chapter in the tutorial.

**Bundled datasets**: `sw2022` (Saha & Weeks 2022), `gs2020`
(Graham & Svolik 2020), `br2017` (Ballard-Rosa, Martin & Scheve 2017),
`bs2013` (Bechtel & Scheve 2013), `simdata` (known-DGP sanity check).

## Documentation

The primary documentation is a [Quarto book](tutorial/) with
worked examples:

1. **Get Started** --- installation
2. **Simulated Example** --- full workflow with ground-truth verification
3. **Example: Candidate Choice** --- Saha & Weeks (2022)
4. **Example: Democratic Norms** --- Graham & Svolik (2020)
5. **Example: Tax Preferences** --- Ballard-Rosa, Martin & Scheve (2017)
6. **Example: Climate Treaties** --- Bechtel & Scheve (2013)
7. **Plot Options** --- parameter-first reference for customization

## Reference

Acharya, Avidit, Jens Hainmueller, and Yiqing Xu. 2026.
"[A Structural Deep-Learning Estimator for Conjoint Experiments](https://arxiv.org/abs/2604.10845)."
Working paper.

## License

MIT.
