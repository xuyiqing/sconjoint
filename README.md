
<!-- README.md is generated from README.Rmd. Please edit README.Rmd. -->

# sconjoint

`sconjoint` fits the structural random-utility model of Acharya,
Hainmueller and Xu (2026) to forced-choice conjoint experiments. It
estimates what respondents prefer, how many of them prefer it, and how
strongly.

In the democracy experiment of Graham and Svolik (2020), for example, a
co-partisan candidate gains about 14 percentage points of support on
average. The model adds that an estimated 82.5% of partisans and leaners
prefer a co-partisan, other attributes held fixed, and that for many of
them the party advantage outweighs the penalty for a candidate who
prosecutes journalists.

## The model

Each respondent has tastes $\beta_i$, one weight per coded feature, that
stay the same across their tasks. A neural network maps the respondent’s
characteristics $Z_i$ to mean tastes, and $q$ taste factors let tastes
vary among respondents with the same characteristics:

$$\beta_i = \mu(Z_i) + \Lambda u_i, \quad u_i \sim N(0, I_q), \qquad
\Pr(Y_{it} = 1) = G(\kappa + a_i + d_{it}^\top \beta_i),$$

where $d_{it}$ is the coded difference between the two profiles, $G$ the
logistic function, and $a_i$ a persistent pull toward the first profile.
Estimation is cross-fitted over respondents, with one-step corrected
standard errors for population quantities.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("xuyiqing/sconjoint")
```

## Workflow

``` r
library(sconjoint)
x <- sc_example_data("gs")                  # the paper's inputs, from the bundled gs2020 data
fits <- lapply(1:4, function(q) sc_fit(x, q = q))
sc_select_rank(fits)                         # held-out fit chooses q = 3
st <- sc_states(fits[[3]])                   # what the estimator needs from each fold
sc_save(st, "gs_q3.rds")

st <- sc_load("gs_q3.rds")
sc_estimate(st, list(
  sc_mean("co-partisan.mean", "diff_respParty", w = "PL"),
  sc_share("co-partisan.share", "diff_respParty", w = "PL"),
  sc_contest("co-partisan vs other", "diff_respParty", w = "PL")))
```

Fitting at the paper’s settings takes hours. The paper’s saved fold
states for both applications, `gs_q3.rds` and `br_q4.rds`, are a
separate download from the release
[`fits-2026-11`](https://github.com/xuyiqing/sconjoint/releases/tag/fits-2026-11),
so the estimates take about a minute instead of hours:

``` r
url <- "https://github.com/xuyiqing/sconjoint/releases/download/fits-2026-11/gs_q3.rds"
download.file(url, "gs_q3.rds", mode = "wb")
st <- sc_load("gs_q3.rds")                   # checks the file's checksum
```

The installation chapter of the book gives each file’s checksum and how
it was made.

## Documentation

The package book walks through a simulated example, both applications of
the paper, every quantity, inference and respondent posteriors:
<https://yiqingxu.org/packages/sconjoint/>.

## Citation

Acharya, Avidit, Jens Hainmueller, and Yiqing Xu. 2026. “Learning
Preferences from Conjoint Data: A Structural Deep Learning Approach.”
Working paper, Stanford University.

The bundled data come from Graham and Svolik (2020, *American Political
Science Review*) and Ballard-Rosa, Martin and Scheve (2017, *Journal of
Politics*).

## License

MIT.
