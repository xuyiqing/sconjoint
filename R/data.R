#' Saha-Weeks (2022) candidate-choice conjoint (synthetic)
#'
#' A small long-format forced-choice candidate-conjoint dataset whose
#' design mirrors Saha and Weeks (2022).  Respondents see three
#' forced-choice tasks, each showing a pair of hypothetical political
#' candidates varying on five attributes.  The goal of the original
#' study was to understand voter perceptions of women candidates'
#' ambition.
#'
#' **Note (synthetic).** This dataset is a seed-fixed synthetic
#' fixture built by `data-raw/build_sw2022.R` with design constants
#' drawn from the published paper.  It is intended for package
#' examples and the Quarto book case-study chapter -- it is NOT the
#' original SSI sample and should not be used for substantive
#' replication.  The `synthetic` attribute is set to `TRUE` on the
#' loaded object.
#'
#' @format A data frame with 1,200 rows (200 respondents x 3 tasks x 2
#'   profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (integer 1..200).}
#'   \item{task}{Task id within respondent (1..3).}
#'   \item{profile}{Profile id within task (1..2).}
#'   \item{choice}{Binary outcome (1 = this profile was chosen in the
#'     task, 0 = not chosen).}
#'   \item{agenda}{Factor: candidate's policy agenda
#'     (`status_quo`, `progressive`, `conservative`).}
#'   \item{talent}{Factor: candidate's talent description
#'     (`average`, `hard_working`, `experienced`).}
#'   \item{children}{Factor: candidate has children
#'     (`no_children`, `has_children`).}
#'   \item{cand_gender}{Factor: candidate gender (`male`, `female`).}
#'   \item{prior_office}{Factor: candidate's prior elected office
#'     (`none`, `state_leg`, `us_house`).}
#'   \item{resp_female}{Integer 0/1: respondent gender (1 = female).}
#'   \item{age}{Numeric: respondent age, standardized.}
#'   \item{pid}{Integer in -1/0/+1: party identification
#'     (-1 = Democrat, 0 = Independent, +1 = Republican).}
#' }
#' @source Saha, Sparsha and Jessica L. P. Weeks. 2022. "Ambitious
#'   Women: Gender and Voter Perceptions of Candidate Ambition."
#'   *Political Behavior* 44(2):779-805.
#' @examples
#' data(sw2022)
#' head(sw2022)
#' \donttest{
#' if (requireNamespace("torch", quietly = TRUE) &&
#'     torch::torch_is_installed()) {
#'   fit <- scfit(
#'     choice ~ agenda + talent + children + cand_gender + prior_office |
#'              resp_female + age + pid,
#'     data = sw2022, respondent = "respondent",
#'     task = "task", profile = "profile",
#'     K = 2L, n_epochs = 50L, seed = 1L
#'   )
#'   sc_importance(fit)
#' }
#' }
"sw2022"

#' Graham-Svolik (2020) candidate-choice conjoint on democratic norms (synthetic)
#'
#' A small long-format forced-choice candidate-conjoint dataset whose
#' design mirrors Graham and Svolik (2020).  Respondents see eight
#' tasks, each showing a pair of hypothetical candidates described by
#' five policy attributes and one candidate-party attribute, as well
#' as an "undemocratic behavior" attribute.  The original study
#' measures the extent to which U.S. voters tolerate democratic
#' backsliding by co-partisan candidates.
#'
#' **Note (synthetic).** This dataset is a seed-fixed synthetic
#' fixture built by `data-raw/build_gs2020.R`.  See the note on
#' `sw2022` for context.  The `synthetic` attribute is set to `TRUE`.
#'
#' @format A data frame with 3,200 rows (200 respondents x 8 tasks x 2
#'   profiles) and the following columns:
#' \describe{
#'   \item{respondent, task, profile, choice}{Same role as in `sw2022`.}
#'   \item{federal}{Factor: federal program spending direction.}
#'   \item{immigration}{Factor: immigration policy direction.}
#'   \item{tax}{Factor: tax policy.}
#'   \item{abortion}{Factor: abortion policy stance.}
#'   \item{undem}{Factor: undemocratic behavior level -- `none`,
#'     `ignore_court`, `close_press`, `ban_opposition_rally`.}
#'   \item{cand_party}{Factor: candidate's party (`Dem`, `Rep`, `Ind`).}
#'   \item{resp_pid}{Integer -1/0/+1: respondent partisanship.}
#'   \item{resp_ideo}{Numeric: respondent ideology, standardized.}
#'   \item{age}{Numeric: respondent age, standardized.}
#' }
#' @source Graham, Matthew H. and Milan W. Svolik. 2020. "Democracy
#'   in America?  Partisanship, Polarization, and the Robustness of
#'   Support for Democracy in the United States." *American Political
#'   Science Review* 114(2):392-409.
#' @examples
#' data(gs2020)
#' head(gs2020)
"gs2020"

#' Bechtel-Scheve (2013) climate-treaty conjoint (synthetic)
#'
#' A small long-format forced-choice climate-treaty conjoint dataset
#' whose design mirrors Bechtel and Scheve (2013).  Respondents see
#' six tasks, each showing a pair of hypothetical climate agreements
#' varying on distribution, enforcement, monitoring, participation,
#' sanctions, and a numeric cost attribute (USD per household per
#' month).  The numeric cost attribute enables dollar-scale
#' willingness-to-pay (WTP) analysis via `sc_wtp()`, which is the
#' headline quantity of the companion Quarto case-study chapter.
#'
#' **Note (synthetic).** This dataset is a seed-fixed synthetic
#' fixture built by `data-raw/build_bs2013.R`.  See the note on
#' `sw2022` for context.  The `synthetic` attribute is set to `TRUE`.
#'
#' @format A data frame with 2,400 rows (200 respondents x 6 tasks x 2
#'   profiles) and the following columns:
#' \describe{
#'   \item{respondent, task, profile, choice}{Same role as in `sw2022`.}
#'   \item{distribution}{Factor: burden-sharing principle.}
#'   \item{enforcement}{Factor: enforcement mechanism.}
#'   \item{monitoring}{Factor: monitoring level.}
#'   \item{participation}{Factor: participation threshold.}
#'   \item{sanctions}{Factor: sanctions for non-compliance.}
#'   \item{cost_usd}{Numeric: estimated monthly cost per household
#'     in USD.  Supports dollar-scale WTP via `sc_wtp(fit,
#'     cost = "cost_usd", ...)`.}
#'   \item{resp_female}{Integer 0/1: respondent gender.}
#'   \item{age}{Numeric: respondent age, standardized.}
#'   \item{resp_ideo}{Numeric: respondent ideology, standardized.}
#' }
#' @source Bechtel, Michael M. and Kenneth F. Scheve. 2013. "Mass
#'   Support for Global Climate Agreements Depends on Institutional
#'   Design." *Proceedings of the National Academy of Sciences*
#'   110(34):13763-13768.
#' @examples
#' data(bs2013)
#' head(bs2013)
"bs2013"

#' Simulated conjoint with known ground truth
#'
#' A synthetic forced-choice conjoint dataset with known true
#' preference parameters, designed for validating the estimator.
#' The data-generating process uses three binary attributes and
#' two continuous respondent moderators:
#'
#' \deqn{\beta_1(Z_i) = 0.5 + 0.3 z_{1i}}
#' \deqn{\beta_2(Z_i) = -0.8 + 0.2 z_{2i}}
#' \deqn{\beta_3(Z_i) = 0.3}
#'
#' Attributes \eqn{x_1} and \eqn{x_2} are heterogeneous (preference
#' depends on respondent covariates); \eqn{x_3} is homogeneous.
#'
#' The true per-respondent \eqn{\beta} matrix is stored as
#' \code{attr(simdata, "beta_true")} and the DGP specification as
#' \code{attr(simdata, "dgp")}.
#'
#' @format A data frame with 6,000 rows (500 respondents x 6 tasks x 2
#'   profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (integer 1..500).}
#'   \item{task}{Task id within respondent (1..6).}
#'   \item{profile}{Profile id within task (1..2).}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{x1}{Binary attribute 1.}
#'   \item{x2}{Binary attribute 2.}
#'   \item{x3}{Binary attribute 3.}
#'   \item{z1}{Continuous respondent moderator 1 (standard normal).}
#'   \item{z2}{Continuous respondent moderator 2 (standard normal).}
#' }
#' @examples
#' data(simdata)
#' dim(simdata)
#' head(simdata)
#' ## True beta matrix:
#' beta_true <- attr(simdata, "beta_true")
#' dim(beta_true)
"simdata"
