#' sconjoint: Structural Deep-Learning Estimation for Conjoint Experiments
#'
#' Implements a respondent-sequence low-rank normal mixed-logit model for
#' forced-choice conjoint experiments. A deep neural-network sieve represents
#' the covariate-dependent conditional mean of preferences, and a jointly
#' estimated low-rank covariance represents persistent residual heterogeneity.
#'
#' The paper-aligned workflow separates the full-sample structural fit from
#' respondent-level cross-fitted one-step inference for supported regular
#' rowwise primitives and named smooth transformations, and from specification
#' assessment. With bounded task counts,
#' respondent posterior summaries are predictions rather than consistently
#' recovered realized preferences. The package does not automatically verify
#' normality, rank, neural-network rates, optimization error, or numerical
#' integration accuracy.
#'
#' The `rum_*` functions are the repeated-choice random utility model ported
#' unchanged from `conjointRUM` 0.1.3 (Avidit Acharya): a one-factor mixed logit
#' whose side preference varies across respondents. It has a formula version of
#' the preference mean, fit by maximum likelihood with regular-inference targets
#' ([rum_fit()], [rum_targets()]), and an experimental neural-network version
#' without network inference ([rum_dnn_gaussian_train()]).
#'
#' @keywords internal
#' @useDynLib sconjoint, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom utils head tail
"_PACKAGE"
