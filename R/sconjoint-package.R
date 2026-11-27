#' sconjoint: Structural Random-Utility Estimation for Conjoint Experiments
#'
#' Fits the structural random-utility model of Acharya, Hainmueller and Xu
#' to forced-choice conjoint data. Each respondent's tastes for the
#' attributes are persistent across tasks and follow a normal law whose mean
#' depends on respondent characteristics through a neural network and whose
#' covariance has q taste factors. Task-level shocks are logistic, and each
#' respondent has a persistent preference for one side of the screen.
#'
#' Estimation is cross-fitted over respondents. Population quantities come
#' with one-step corrected standard errors. Start with the package book at
#' <https://yiqingxu.org/packages/sconjoint/>.
#'
#' @keywords internal
#' @useDynLib sconjoint, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom utils head tail
"_PACKAGE"
