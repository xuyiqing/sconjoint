# Builds toy_fit.rds, the small saved fit used by the help-page examples: 40 simulated respondents under
# the Ballard-Rosa et al. (2017) design law (8 tasks each), one taste factor, two folds, a tiny network
# and small integration rules. Not the paper's fit (see the package book for those).
# Run from the package root: Rscript inst/extdata/make_toy_fit.R
pkgload::load_all(".", quiet = TRUE)
st <- sconjoint:::.sc_toy_states(n = 40L, q = 1L, K = 2L, seed = 1L)
sc_save(st, "inst/extdata/toy_fit.rds")
