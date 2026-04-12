#' Saha-Weeks (2022) candidate-choice conjoint
#'
#' A long-format forced-choice candidate-conjoint dataset from the
#' replication materials of Saha and Weeks (2022).  Respondents see
#' three forced-choice tasks, each showing a pair of hypothetical
#' political candidates varying on five attributes.  The goal of the
#' original study was to understand voter perceptions of women
#' candidates' ambition.
#'
#' The data is cleaned from the original SSI survey: respondents with
#' fewer than 3 elections or invalid demographics are dropped.
#'
#' @format A data frame with 7,146 rows (1,191 respondents x 3 tasks x
#'   2 profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (character).}
#'   \item{task}{Task id within respondent (integer).}
#'   \item{profile}{Profile id within task (1 or 2).}
#'   \item{choice}{Binary outcome (1 = this profile was chosen in the
#'     task, 0 = not chosen).}
#'   \item{agenda}{Factor: candidate's policy agenda
#'     (ref: `Very Few Changes`, `Moderate Changes`, `Complete Overhaul`).}
#'   \item{talent}{Factor: candidate's talent description
#'     (ref: `Assertive`, `Collaborative`, `Determined to Succeed`,
#'     `Empathetic`, `Good Communicator`, `Hard-Working`,
#'     `Tough Negotiator`).}
#'   \item{children}{Factor: candidate has children
#'     (ref: `No children`, `1 child`, `2 children`, `3 children`).}
#'   \item{cand_gender}{Factor: candidate gender (ref: `Male`, `Female`).}
#'   \item{prior_office}{Factor: candidate's prior elected office
#'     (ref: `No`, `Yes`).}
#'   \item{resp_female}{Numeric 0/1: respondent gender (1 = female).}
#'   \item{age}{Numeric: respondent age in years.}
#'   \item{pid}{Factor: party identification
#'     (`Democrat`, `Republican (GOP)`, `Independent`).}
#' }
#' @source Saha, Sparsha and Jessica L. P. Weeks. 2022. "Ambitious
#'   Women: Gender and Voter Perceptions of Candidate Ambition."
#'   *Political Behavior* 44(2):779-805.
#'   Data from published replication materials (SSI survey).
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

#' Graham-Svolik (2020) candidate-choice conjoint on democratic norms
#'
#' A long-format forced-choice candidate-conjoint dataset from the
#' replication materials of Graham and Svolik (2020).  Respondents see
#' up to 13 matchups, each showing a pair of hypothetical candidates
#' described by party, democracy behavior, policy positions, sex, race,
#' and profession.  The original study measures the extent to which U.S.
#' voters tolerate democratic backsliding by co-partisan candidates.
#'
#' The data is cleaned from the original experiment file: respondents
#' with incomplete matchups or missing moderator variables are dropped.
#'
#' @format A data frame with 41,314 rows (1,605 respondents, ~20,657
#'   matchups x 2 profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (character).}
#'   \item{task}{Matchup number within respondent (integer).}
#'   \item{profile}{Profile id within matchup (1 or 2).}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{copartisan}{Factor: co-partisan indicator (Not or Co-partisan).}
#'   \item{p1}{Numeric 1-4: economic policy position (1 = most liberal,
#'     4 = most conservative).}
#'   \item{p2}{Numeric 1-4: social policy position (1 = most liberal,
#'     4 = most conservative).}
#'   \item{dem_code}{Factor: democracy behavior code -- 7 good-governance
#'     codes (`g_*`), 7 undemocratic codes (`u_*`), 2 valence codes (`v_*`).
#'     Reference: `g_boardElect`.}
#'   \item{cand_sex}{Factor: candidate sex (ref: `Male`, `Female`).}
#'   \item{cand_race}{Factor: candidate race (ref: `White`, `Black`,
#'     `Hispanic`, `Asian`).}
#'   \item{cand_pro}{Factor: candidate profession (9 levels; ref:
#'     `Business_executive`).}
#'   \item{resp_ideo}{Numeric 1-7: respondent ideology (1 = extremely liberal,
#'     7 = extremely conservative).}
#'   \item{resp_pid}{Numeric: respondent party ID (-3 to 3).}
#'   \item{resp_trump}{Numeric 1-4: Trump approval (1 = strongly disapprove,
#'     4 = strongly approve).}
#'   \item{resp_age}{Numeric: respondent age in years.}
#'   \item{resp_female}{Numeric 0/1: respondent gender (1 = female).}
#'   \item{resp_race_black}{Numeric 0/1: respondent is Black.}
#'   \item{resp_race_asian}{Numeric 0/1: respondent is Asian.}
#'   \item{resp_race_other}{Numeric 0/1: respondent is other race.}
#'   \item{resp_educ}{Numeric 1-6: respondent education (1 = no HS,
#'     6 = graduate degree).}
#'   \item{resp_income}{Numeric 1-24: household income bracket (ordinal).}
#'   \item{resp_auth}{Numeric 0-4: authoritarianism score.}
#'   \item{resp_knowledge}{Numeric 0-8: political knowledge score.}
#' }
#' @source Graham, Matthew H. and Milan W. Svolik. 2020. "Democracy
#'   in America?  Partisanship, Polarization, and the Robustness of
#'   Support for Democracy in the United States." *American Political
#'   Science Review* 114(2):392-409.
#'   Data from published replication materials.
#' @examples
#' data(gs2020)
#' head(gs2020)
"gs2020"

#' Bechtel-Scheve (2013) climate-treaty conjoint
#'
#' A long-format forced-choice climate-treaty conjoint dataset from
#' the replication materials of Bechtel and Scheve (2013), US
#' subsample.  Respondents see four tasks, each showing a pair of
#' hypothetical climate agreements varying on cost, distribution,
#' participation, emissions, sanctions, and monitoring.  The numeric
#' cost attribute enables dollar-scale willingness-to-pay (WTP)
#' analysis via `sc_wtp()`.
#'
#' The data is filtered to US respondents (country == 4) with exactly
#' 8 rows (4 tasks x 2 profiles).
#'
#' @format A data frame with 20,000 rows (2,500 respondents x 4 tasks x
#'   2 profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (character).}
#'   \item{task}{Task id within respondent (1..4).}
#'   \item{profile}{Profile id within task (1 or 2).}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{cost_usd}{Numeric: monthly cost per household in USD
#'     (28, 56, 84, 113, or 141).  Supports dollar-scale WTP via
#'     `sc_wtp(fit, cost = "cost_usd", ...)`.}
#'   \item{distribution}{Factor: burden-sharing principle
#'     (ref: `Only rich pay`).}
#'   \item{participation}{Factor: number of participating countries
#'     (ref: `20 countries`).}
#'   \item{emissions}{Factor: emissions reduction target
#'     (ref: `40% reduction`).}
#'   \item{sanctions}{Factor: sanctions for non-compliance
#'     (ref: `No sanctions`).}
#'   \item{monitoring}{Factor: monitoring body
#'     (ref: `Your government`).}
#'   \item{resp_female}{Numeric 0/1: respondent gender (1 = female).}
#'   \item{resp_age}{Numeric: respondent age in years.}
#'   \item{resp_ideo}{Numeric 0-10: respondent ideology.}
#' }
#' @source Bechtel, Michael M. and Kenneth F. Scheve. 2013. "Mass
#'   Support for Global Climate Agreements Depends on Institutional
#'   Design." *Proceedings of the National Academy of Sciences*
#'   110(34):13763-13768.
#'   Data from published replication materials.
#' @examples
#' data(bs2013)
#' head(bs2013)
"bs2013"

#' Ballard-Rosa, Martin & Scheve (2017) tax-plan conjoint
#'
#' A long-format forced-choice tax-plan conjoint dataset from the
#' replication materials of Ballard-Rosa, Martin, and Scheve (2017).
#' Respondents see eight tasks comparing two tax-plan proposals.
#' Each plan specifies marginal tax rates for six income brackets
#' plus a revenue impact indicator. All attributes are numeric
#' (continuous), unlike the other bundled datasets which use factors.
#'
#' The sample is restricted to respondents who saw the revenue
#' column (saw_revenue == 1).
#'
#' @format A data frame with 32,000 rows (2,000 respondents x 8 tasks x
#'   2 profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (character).}
#'   \item{task}{Task id within respondent (1..8).}
#'   \item{profile}{Profile id within task (1 or 2).}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{rate_L10}{Marginal tax rate (percent) for under-$10k bracket.}
#'   \item{rate_10_35}{Rate for $10-35k bracket.}
#'   \item{rate_35_85}{Rate for $35-85k bracket.}
#'   \item{rate_85_175}{Rate for $85-175k bracket.}
#'   \item{rate_175_375}{Rate for $175-375k bracket.}
#'   \item{rate_375P}{Rate for over-$375k bracket.}
#'   \item{revenue_score}{Revenue impact: -2 (much less) to +2 (much more).}
#'   \item{resp_age}{Respondent age in years.}
#'   \item{resp_female}{Respondent gender (0/1, 1 = female).}
#'   \item{resp_pid7}{Respondent party ID (1=Strong Dem to 7=Strong Rep).}
#'   \item{resp_educ}{Respondent education (1-6 ordinal).}
#'   \item{resp_race_white}{Respondent is white (0/1).}
#'   \item{resp_income}{Household income (1-13 ordinal).}
#'   \item{resp_ineq_averse}{Inequality averse (0/1).}
#'   \item{resp_work_vs_luck}{Work-vs-luck belief (ordinal, higher = hard work).}
#'   \item{resp_taxes_harm}{Taxes-harm-economy belief (ordinal, higher = more harmful).}
#'   \item{resp_hardwork}{Hard work leads to success (0/1).}
#'   \item{resp_high_econ_know}{High economic knowledge (0/1).}
#'   \item{resp_employed_ft}{Employed full-time (0/1).}
#' }
#' @source Ballard-Rosa, Cameron, Lucy Martin, and Kenneth Scheve. 2017.
#'   "The Structure of American Income Tax Policy Preferences."
#'   *Journal of Politics* 79(1):1-16.
#'   Data from published replication materials.
#' @examples
#' data(br2017)
#' head(br2017)
"br2017"

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
#' @format A data frame with 12,000 rows (1,000 respondents x 6 tasks x 2
#'   profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (integer 1..1000).}
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
